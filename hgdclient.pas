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
  HGD_PROTO_MAJOR: integer = 8;
  HGD_PROTO_MINOR: integer = 2;

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
    Genre: string;
    Duration: integer;
    Bitrate: integer;
    SampleRate: integer;
    Channels: integer;
    Year: integer;
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
      function CheckProto: boolean;
      procedure Log(Message: string);
      procedure ParseHGDPacket(Packet: string; List: TStringList);
      function ProcessReply(Reply: string; var Msg: string): boolean;
      function ReceiveString: string;
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

  if Assigned(Socket) then
    FreeAndNil(Socket);
end;

function THGDClient.Connect(): boolean;
var
  Reply, Msg: string;
begin
  Disconnect();

  if not Assigned(Socket) then
    Socket := TTCPBlockSocket.Create();

  Result := False;
  FState := hsDisconnected;

  Log('Connecting to hgd server (' + FHostAddress + ':' + FHostPort + ')...');
  FStatusMessage := 'Connecting...';

  Socket.Connect(FHostAddress, FHostPort);
  Reply := ReceiveString();
  Log('connect reply: ' + Reply);

  Result := ProcessReply(Reply, Msg);

  if Result then
  begin
    if FSSL then
    begin
      Log('Checking if server supprts encryption...');
      Socket.SendString('encrypt?' + ProtoLineEnding);
      Reply := ReceiveString();
      Log('encrypt? reply: ' + Reply);

      if ProcessReply(Reply, Msg) and (Msg <> 'nocrypto') then
      begin
        if Msg = 'tlsv1' then
          Socket.SSL.SSLType:= LT_TLSv1
        else
          Socket.SSL.SSLType := LT_all;
        //Todo, maybe add other encryption types here

        Log('Encrypting Socket...');
        Socket.SendString('encrypt' + ProtoLineEnding);
        Socket.SSLDoConnect();

        if Socket.LastError <> 0 then //check for success start of SSL
        begin
          FStatusMessage := 'Error setting SSL ' + IntToStr(Socket.LastError) + ' ' + Socket.LastErrorDesc;
          FEncrypted := False;
          Socket.SSLDoShutdown();
          Exit(False);
        end;

        Reply := ReceiveString();

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

    if CheckProto() then
    begin
      FState := hsConnected;
      FStatusMessage := 'Connected (';
      if FEncrypted then
        FStatusMessage := FStatusMessage + 'SSL)'
      else
        FStatusMessage := FStatusMessage + 'no SSL)'
    end
    else
    begin
      FState := hsDisconnected;
      Result := False;
      FStatusMessage := 'Error: Protocol of server does not match client';
    end;
  end
  else
  begin
    FState := hsDisconnected;
    FStatusMessage := 'Not connected';
  end;
end;

function THGDClient.CheckProto(): boolean;
var
  Reply: string;
  Msg: string;
  Minor, Major: integer;
begin
  Result := False;
  Log('Getting Proto...');
  Socket.SendString('proto' + ProtoLineEnding);
  Reply := ReceiveString();
  Log('proto reply: ' + Reply);
  if ProcessReply(Reply, Msg) then
  begin
    Major := StrToIntDef(Copy(Msg, 1, Pos('|', Msg) - 1), - 1);
    Minor := StrToIntDef(Copy(Msg, Pos('|', Msg) + 1, Length(Msg)), - 1);

    if (HGD_PROTO_MAJOR = Major) and
      (HGD_PROTO_MINOR <= Minor) then
        Result := True;
  end;
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
  SetLength(PList, 0);

  //only do this if at least connected...
  if FState >= hsConnected then
  begin
    Log('Getting Playlist...');
    Socket.SendString('ls' + ProtoLineEnding);
    Reply := ReceiveString();
    Log('ls reply: ' + Reply);
    if ProcessReply(Reply, Msg) then
    begin
      //Playlist came back OK. Parse it up, d00d...
      Log('Number of playlist items: ' + Msg);

      PLStringList := TStringList.Create();

      for i := 1 to StrToIntDef(Msg, 0) do
      begin
        Reply := ReceiveString();
        Log('Playlist item ' + IntToStr(i) + ': ' + Reply);

        SetLength(PList, Length(PList) + 1);

        PLStringList.Clear();
        ParseHGDPacket(Reply, PLStringList);

        if PLStringList.Count >= 11 then
        begin
          PList[Length(PList) - 1].Number := StrToIntDef(PLStringList.Strings[0],
            0);

          PList[Length(PList) - 1].Filename := PLStringList.Strings[1];
          PList[Length(PList) - 1].Artist := PLStringList.Strings[2];
          PList[Length(PList) - 1].Title := PLStringList.Strings[3];
          PList[Length(PList) - 1].User := PLStringList.Strings[4];
          PList[Length(PList) - 1].Album := PLStringList.Strings[5];
          PList[Length(PList) - 1].Genre := PLStringList.Strings[6];
          PList[Length(PList) - 1].Duration := StrToIntDef(PLStringList.Strings[7], 0);
          PList[Length(PList) - 1].Bitrate := StrToIntDef(PLStringList.Strings[8], 0);
          PList[Length(PList) - 1].SampleRate := StrToIntDef(PLStringList.Strings[9], 0);
          PList[Length(PList) - 1].Channels := StrToIntDef(PLStringList.Strings[10], 0);
          PList[Length(PList) - 1].Year := StrToIntDef(PLStringList.Strings[11], 0);
        end;
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

  Reply := ReceiveString();
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

    Reply := ReceiveString();
    Log('q data send reply: ' + Reply);

    Result := ProcessReply(Reply, Msg);
    if Result then
      FStatusMessage := 'Queued';
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
  Reply := ReceiveString();
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

    //check if the server is booting us
    if Pos('Catch you later d00d!', Reply) > 0 then
    begin
      Log('We got booted, Catch you later d00d!');
      try
        FreeAndNil(Socket);
      except
        //Keep SSL errors from appearing
      end;

      FState := hsDisconnected;
      FStatusMessage := 'Not connected';
    end;
  end
  else
  begin
    Result := False;
    Log('"ok" or "err" not found in packet. Reading all remaining bytes...');
    //Something has gone wrong, so read all remaining bytes in the packet.
    Socket.RecvPacket(Timeout);
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
  Reply := ReceiveString();
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

function THGDClient.ReceiveString: string;
var
  s: string;
  i: integer;
begin
  Result := '';
  s := Socket.RecvString(Timeout);

  for i := 1 to Length(s) do
    if s[i] <> #0 then
    begin
      Result := Result + s[i];
    end;
end;

end.
