{
 * Copyright (c) 2011, Tristan Linnell <tris@canthack.org>
 *
 * Permission to use, copy, modify, and/or distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
}

// hgdc-x Cross Platform hgd client written in Lazarus/Freepascal
// HGDClient.pas - HGD Client class
// Uses the synapse networking library

unit HGDClient;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BlckSock, StdCtrls, FileUtil, ssl_openssl, LCLProc;

const
  HGD_PROTO: string = '7|0'; //Todo this is a hack. Deal with proper proto
  //The hgd protocol is telnet based, use CRLF as LineEnding
  ProtoLineEnding = CRLF;

type
  THGDCState = (hsDisconnected, hsConnected, hsAuthenticated);

  TTrackInfo = record
    Number: integer;
    Filename: string;
    Artist: string;
    Title: string;
    Album: string;
    User: string;
  end;

  TPlaylist = array of TTrackInfo;

  { THGDClient }

  THGDClient = class
    private
      FUsername: string;
      FPassword: string;

      FHostAddress: string;
      FHostPort: string;

      FSSL: boolean; //SSL is set
      FEncrypted: boolean; //connection has been successfully encrypted

      Socket: TTCPBlockSocket;

      FState: THGDCState;
      FStatusMessage: string;
      FDebugMemo: TMemo;

      function Connect: boolean;
      procedure Disconnect;
      function GetProto: string;
      procedure Log(Message: string);
      procedure ParseHGDPacket(Packet: string; List: TStringList);
      function ProcessReply(Reply: string; var Msg: string): boolean;
      function ReceiveStringAndDeBork: string;
      procedure SetHostAddress(const AValue: string);
      procedure SetHostPort(const AValue: string);
      procedure SetPassword(const AValue: string);
      procedure SetUsername(const AValue: string);
      function SendUser(Username, Password: string): boolean;

    public
      Timeout: integer;

      constructor Create(HostAddress, HostPort, UserName, Password: string;
        SSL: boolean); overload;

      constructor Create(HostAddress, HostPort, UserName, Password: string;
        SSL: boolean; DebugMemo: TMemo); overload;

      destructor Destroy(); override;

      procedure ApplyChanges();
      function GetPlaylist(var PList: TPlaylist): boolean;
      function QueueSong(Filename: string): boolean;
      function VoteOff(id: integer): boolean;

      property State: THGDCState read FState;
      property StatusMessage: string read FStatusMessage;
      property UserName: string read FUsername write SetUsername;
      property Password: string write SetPassword;
      property HostAddress: string read FHostAddress write SetHostAddress;
      property HostPort: string read FHostPort write SetHostPort;
      property SSL: boolean read FSSL write FSSL;
      property Encrypted: boolean read FEncrypted;
  end;

implementation

{ THGDClient }

constructor THGDClient.Create(HostAddress, HostPort, UserName,
  Password: string; SSL: boolean);
begin
  FState := hsDisconnected;
  FStatusMessage := 'Disconnected';

  //2 second socket timeout
  Timeout := 2000;
  Socket := TTCPBlockSocket.Create();

  FUsername := UserName;
  FPassword := Password;
  FHostAddress := HostAddress;
  FHostPort := HostPort;
  FSSL := SSL;
  FEncrypted := False;

  ApplyChanges();
end;

constructor THGDClient.Create(HostAddress, HostPort, UserName,
  Password: string; SSL: boolean; DebugMemo: TMemo);
begin
  FDebugMemo := DebugMemo;
  Create(HostAddress, HostPort, UserName, Password, SSL);
end;

destructor THGDClient.Destroy();
begin
  Disconnect();
  Socket.Free();
end;

procedure THGDClient.Disconnect();
begin
  if FState >= hsConnected then
  begin
    Socket.SendString('bye' + ProtoLineEnding);
    Socket.CloseSocket();
    FState := hsDisconnected;
    FStatusMessage := 'Not connected';
  end;
end;

function THGDClient.Connect(): boolean;
var
  Reply, Msg: string;
begin
  Disconnect();

  Result := False;
  FState := hsDisconnected;

  Log('Connecting to hgd server (' + FHostAddress + ':' + FHostPort + ')...');
  FStatusMessage := 'Connecting...';

  Socket.Connect(FHostAddress, FHostPort);
  Reply := ReceiveStringAndDeBork();
  Log('connect reply: ' + Reply);

  Result := ProcessReply(Reply, Msg);

  if Result then
  begin
    if FSSL then
    begin
      Log('Checking if server supprts encryption...');
      Socket.SendString('encrypt?' + ProtoLineEnding);
      Reply := ReceiveStringAndDeBork();
      Log('encrypt? reply: ' + Reply);

      if ProcessReply(Reply, Msg) and (Msg <> 'nocrypto') then
      begin

        Log('Encrypting Socket...');
        Socket.SendString('encrypt' + ProtoLineEnding);

        Socket.SSLDoConnect();

        if Socket.LastError <>  0 then //check for success start of SSL
        begin
          FEncrypted := False;
          Result := False;
          FStatusMessage := 'Error setting SSL';
        end;

        Reply := ReceiveStringAndDeBork();

        Log('encrypt reply: ' + Reply);
        if ProcessReply(Reply, Msg) then
        begin
          FEncrypted := True;
        end
        else
          FEncrypted := False;
      end
      else
      begin
        Socket.SSLDoShutdown();
        FEncrypted := False;
      end;
    end
    else
    begin
      FEncrypted := False;
    end;

    if GetProto() <> HGD_PROTO then
    begin
      FState := hsDisconnected;
      Result := False;
      FStatusMessage := 'Error: Protocol of server does not match client';
    end
    else
    begin
      FState := hsConnected;
      FStatusMessage := 'Connected (';
      if FEncrypted then
        FStatusMessage := FStatusMessage + 'SSL)'
      else
        FStatusMessage := FStatusMessage + 'no SSL)'
    end;
  end
  else
  begin
    FState := hsDisconnected;
    FStatusMessage := 'Not connected';
  end;
end;

function THGDClient.GetProto: string;
var
  Reply: ansistring;
begin
  Result := '';
  Log('Getting Proto...');
  Socket.SendString('proto' + ProtoLineEnding);
  Reply := ReceiveStringAndDeBork();
  Log('proto reply: ' + Reply);
  if not ProcessReply(Reply, Result) then
    Result := '';
end;

function THGDClient.GetPlaylist(var PList: TPlaylist): boolean;
var
  Reply: string;
  Msg: string;
  i: integer;
  PLStringList: TStringList;
  D: Integer;
begin
  Result := False;
  log('Length of playlist: ' + IntToStr(Length(PList)));
  SetLength(PList, 0);

  //only do this if at least connected...
  if FState >= hsConnected then
  begin
    Log('Getting Playlist...');
    Socket.SendString('ls' + ProtoLineEnding);
    Reply := ReceiveStringAndDeBork();
    Log('ls reply: ' + Reply);
    if ProcessReply(Reply, Msg) then
    begin
      //Playlist came back OK. Parse it up, d00d...
      Log('Number of playlist items: ' + Msg);

      PLStringList := TStringList.Create();

      for i := 1 to StrToIntDef(Msg, 0) do
      begin
        Reply := ReceiveStringAndDeBork();
        Log('Playlist item ' + IntToStr(i) + ': ' + Reply);

        SetLength(PList, Length(PList) + 1);

        PLStringList.Clear();
        ParseHGDPacket(Reply, PLStringList);

        PList[Length(PList) - 1].Number := StrToIntDef(PLStringList.Strings[0],
          0);

        PList[Length(PList) - 1].Filename := PLStringList.Strings[1];
        PList[Length(PList) - 1].Artist := PLStringList.Strings[2];
        PList[Length(PList) - 1].Title := PLStringList.Strings[3];
        PList[Length(PList) - 1].User := PLStringList.Strings[4];
        PList[Length(PList) - 1].Album := PLStringList.Strings[5];
      end;

      PLStringList.Free();
      Result := True;
    end;
  end;
end;

function THGDClient.QueueSong(Filename: string): boolean;
var
  Reply, Msg: string;
  Fin: File;
  DataArray: array of byte;
  FileSizeValue: Int64;
begin
  //todo send track piece by piece, not all at same time
  Result := False;
  FileSizeValue := FileSize(Filename);
  Log('Queueing track ' + ExtractFilename(Filename) + '...');
  Socket.SendString('q|' + ExtractFilename(Filename) + '|' +
    IntToStr(FileSizeValue) + ProtoLineEnding);

  Reply := ReceiveStringAndDeBork();
  Log('q reply: ' + Reply);

  Result := ProcessReply(Reply, Msg);

  if Result then
  begin
    Log('q successful, sending data...');

    SetLength(DataArray, FileSizeValue);
    AssignFile(Fin, Filename);

    try
      FileMode := fmOpenRead;
      Reset(Fin, 1);
      BlockRead(Fin, DataArray[0], FileSizeValue);
    finally
      CloseFile(fin);
    end;

    Socket.SendBuffer(@DataArray[0], FileSizeValue);

    SetLength(DataArray, 0);

    Reply := ReceiveStringAndDeBork();
    Log('q reply: ' + Reply);

    Result := ProcessReply(Reply, Msg);
    if Result then FStatusMessage := 'Queued';
  end
  else
  begin
    Log('q Failed');
    FStatusMessage := 'Error queueing track ' + Msg;
  end;
end;

function THGDClient.VoteOff(id: integer): boolean;
var
  Reply, Msg: string;
begin
  Result := False;
  Log('Crapping on song id ' + IntToStr(id) + '...');
  Socket.SendString('vo|' + intToStr(id) + ProtoLineEnding);
  Reply := ReceiveStringAndDeBork();
  Log('vo reply: ' + Reply);

  Result := ProcessReply(Reply, Msg);

  if Result then
  begin
    Log('Vote off Successful');
    FStatusMessage := 'Vote off succeeded';
  end
  else
  begin
    Log('Vote off Failed');
    FStatusMessage := 'Error voting track off ' + Msg;
  end;
end;

procedure THGDClient.ParseHGDPacket(Packet: string; List: TStringList);
begin
  List.StrictDelimiter := True;
  List.Delimiter := '|';
  List.DelimitedText := Packet;
end;

function THGDClient.ProcessReply(Reply: string; var Msg: string): boolean;
begin
  Msg := Copy(Reply, Pos('|', Reply) + 1, MaxInt);

  if Pos('ok', Reply) > 0 then
  begin
    Result := True;
    Log('"ok" found in reply');
  end
  else if Pos('err', Reply) > 0 then
  begin
    Result := False;
    Log('Error Occurred: ' + Msg);
  end
  else
  begin
    Result := False;
    Log('"ok" or "err" not found in packet.');
  end;
end;

procedure THGDClient.ApplyChanges;
begin
  if Connect() then
    SendUser(FUsername, FPassword);
end;

function THGDClient.SendUser(Username, Password: string): boolean;
var
  Reply, Msg: string;
begin
  Result := False;
  Log('Sending username...');
  Socket.SendString('user|' + Username + '|' + Password + ProtoLineEnding);
  Reply := ReceiveStringAndDeBork();
  Log('user reply: ' + Reply);

  Result := ProcessReply(Reply, Msg);

  if Result then
  begin
    FState := hsAuthenticated;
    Log('SendUser Successful');
    FStatusMessage := 'User authenticated';
  end
  else
  begin
    Log('SendUser Failed');
    FState := hsConnected;
    FStatusMessage := 'Error logging in: ' + Msg;
  end;
end;

procedure THGDClient.Log(Message: string);
begin
  FDebugMemo.Lines.Add(Message);
end;

procedure THGDClient.SetHostAddress(const AValue: string);
begin
  if FHostAddress=AValue then Exit;
  FHostAddress := AValue;
end;

procedure THGDClient.SetHostPort(const AValue: string);
begin
  if FHostPort = AValue then Exit;
  FHostPort := AValue;
end;

procedure THGDClient.SetPassword(const AValue: string);
begin
  if FPassword = AValue then Exit;
  FPassword := AValue;
end;

procedure THGDClient.SetUsername(const AValue: string);
begin
  if FUsername = AValue then Exit;
  FUsername := AValue;
end;

function THGDClient.ReceiveStringAndDeBork: string;
var
  s: string;
  i: integer;
begin
  //Strip nulls out of string, in case of SSL padding
  Result := '';
  s := Socket.RecvString(Timeout);

  //Debug::
  Log('RECEIVED LENGTH: ' + IntToStr(Length(s)));

  for i := 1 to Length(s) do
    if s[i] <> #0 then
      Result := Result + s[i];
end;

end.
