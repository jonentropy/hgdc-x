{
 * Copyright (c) 2012, Tristan Linnell <tris@canthack.org>
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
  {$IFDEF UNIX}
  Resolver,
  {$ENDIF UNIX}
  Classes, SysUtils, BlckSock, FileUtil, LCLProc, HGDConsts,
  ssl_openssl;

const
  HGD_PROTO_MAJOR: integer = 17;
  HGD_PROTO_MINOR: integer = 0;
  HGD_NUM_TRACK_FIELDS: integer = 14;
  HGD_MAX_LINE: integer = 512;
  BLOCK_SIZE: int64 = 512 * 1024;

  //The hgd protocol is telnet based, use CRLF as LineEnding
  ProtoLineEnding = CRLF;

type
  THGDCState = (hsDisconnected, hsConnected, hsAuthenticated, hsAdmin);

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
    VoteCount: integer;
    Voted: boolean;
    Playing: boolean;
  end;

  TPlaylist = array of TTrackInfo;

  TProgressCallBack = procedure(Percentage: integer) of Object;

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
      FDebug: boolean;

      function Connect: boolean;
      procedure Disconnect;
      function CheckProto: boolean;
      function IsAdmin: boolean;
      procedure Log(Message: string);
      procedure ParseHGDPacket(Packet: string; List: TStringList);
      function ProcessReply(Reply: string; var Msg: string): boolean;
      function ReceiveString: string;
      procedure SendString(St: string);
      procedure SetHostAddress(const AValue: string);
      procedure SetHostPort(const AValue: string);
      procedure SetPassword(const AValue: string);
      procedure SetUsername(const AValue: string);
      function SendUser(Username, Password: string): boolean;

    public
      Timeout: integer;
      ProgressCallBack: TProgressCallback;

      constructor Create(HostAddress, HostPort, UserName, Password: string;
        SSL: boolean; Debug: boolean); overload;

      constructor Create(Debug: boolean); overload;

      destructor Destroy(); override;

      procedure ApplyChanges();
      function GetNowPlaying(out Song: TTrackInfo): boolean;
      function GetPlaylist(var PList: TPlaylist): boolean;
      function QueueSong(Filename: string): boolean;
      function VoteOff(id: integer): boolean;
      function SkipTrack: boolean;
      function Pause: boolean;
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
  Password: string; SSL: boolean; Debug: boolean);
begin
  Create(Debug);

  FUsername := UserName;
  FPassword := Password;
  FHostAddress := HostAddress;
  FHostPort := HostPort;
  FSSL := SSL;

  ApplyChanges();
end;

constructor THGDClient.Create(Debug: boolean);
begin
  inherited Create();
  FState := hsDisconnected;
  FStatusMessage := 'Not connected.';
  FDebug := Debug;

  //10 second socket timeout
  Timeout := 10000;
  Socket := TTCPBlockSocket.Create();
  FEncrypted := False;
end;

destructor THGDClient.Destroy();
begin
  Disconnect();

  inherited Destroy();
end;

procedure THGDClient.Disconnect();
begin
  if FState >= hsConnected then
  begin
    Log('Disconnecting… bye! o/');
    SendString('bye');
    Log(ReceiveString());
    Socket.CloseSocket();
  end;

  FState := hsDisconnected;
  FEncrypted := False;

  if Assigned(Socket) then
    FreeAndNil(Socket);

end;

function THGDClient.Connect(): boolean;
var
  Reply, Msg: string;
  {$IFDEF UNIX}
    IP: string;
  {$ENDIF UNIX}
begin
  Disconnect();

  if not Assigned(Socket) then
    Socket := TTCPBlockSocket.Create();

  Result := False;
  FState := hsDisconnected;

  Log('Connecting to hgd server (' + FHostAddress + ':' + FHostPort + ')…');
  FStatusMessage := 'Connecting…';

  {$IFDEF UNIX}
  Log('Resolving ' + FHostAddress);
  if NameLookup(FHostAddress, IP) then
  begin
    Log('Resolved ' + FHostAddress + ' as ' + IP);
    Socket.Connect(IP, FHostPort)
  end
  else
  {$ENDIF UNIX}
    Socket.Connect(FHostAddress, FHostPort);

  Reply := ReceiveString();
  Log('connect reply: ' + Reply);

  Msg := '';
  Result := ProcessReply(Reply, Msg);

  if Result then
  begin
    if FSSL then
    begin
      Log('Checking if server supprts encryption…');
      SendString('encrypt?');
      Reply := ReceiveString();
      Log('encrypt? reply: ' + Reply);

      if ProcessReply(Reply, Msg) and (Msg <> 'nocrypto') then
      begin
        if Msg = 'tlsv1' then
          Socket.SSL.SSLType:= LT_TLSv1
        else
          Socket.SSL.SSLType := LT_all;
        //Todo: maybe add other encryption types here

        Log('Encrypting Socket…');
        SendString('encrypt');
        Socket.SSLDoConnect();

        if Socket.LastError <> 0 then //check for successful start of SSL
        begin
          FStatusMessage := 'Error setting SSL ' + IntToStr(Socket.LastError) +
            ' ' + Socket.LastErrorDesc;

          FEncrypted := False;
          Socket.SSLDoShutdown();
          Exit(False);
        end
        else
          FEncrypted := True;

        Reply := ReceiveString();

        Log('encrypt reply: ' + Reply);
        if not ProcessReply(Reply, Msg) then
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
      FStatusMessage := 'Connected to the HGD server';
      if FEncrypted then
        FStatusMessage := FStatusMessage + ' (Encrypted).'
      else
        FStatusMessage := FStatusMessage + ' (Not Encrypted).'
    end
    else
    begin
      FState := hsDisconnected;
      Result := False;
      FStatusMessage := 'Error: Protocol of server does not match client.';
    end;
  end
  else
  begin
    FState := hsDisconnected;
    FStatusMessage := 'Could not connect to the HGD server.';
  end;
end;

function THGDClient.CheckProto(): boolean;
var
  Reply: string;
  Msg: string;
  Minor, Major: integer;
begin
  Result := False;
  Log('Getting Proto…');
  SendString('proto');
  Reply := ReceiveString();
  Log('proto reply: ' + Reply);

  Msg := '';
  if ProcessReply(Reply, Msg) then
  begin
    Major := StrToIntDef(Copy(Msg, 1, Pos('|', Msg) - 1), - 1);
    Minor := StrToIntDef(Copy(Msg, Pos('|', Msg) + 1, Length(Msg)), - 1);

    if (HGD_PROTO_MAJOR = Major) and
      (HGD_PROTO_MINOR <= Minor) then
        Result := True;
  end;
end;

function THGDClient.GetNowPlaying(out Song: TTrackInfo): boolean;
var
  Reply: string;
  Msg: string;
  PLStringList: TStringList;
begin
  Result := False;

  //only do this if at least connected…
  if FState >= hsConnected then
  begin
    Log('Getting Now Playing…');
    SendString('np');
    Reply := ReceiveString();
    Log('np reply: ' + Reply);

    Msg := '';
    if ProcessReply(Reply, Msg) then
    begin
      PLStringList := TStringList.Create();

      try
        ParseHGDPacket(Reply, PLStringList);

        if  PLStringList.Count >= 2 then
          Song.Playing := PLStringList.Strings[1] = '1';

                                             //+2 due to difference between ls
                                             //and np reply formats
        if (PLStringList.Count >= HGD_NUM_TRACK_FIELDS + 2) then
        begin
          Song.Number := StrToIntDef(PLStringList.Strings[2], 0);
          Song.Filename := PLStringList.Strings[3];
          Song.Artist := PLStringList.Strings[4];
          Song.Title := PLStringList.Strings[5];
          Song.User := PLStringList.Strings[6];
          Song.Album := PLStringList.Strings[7];
          Song.Genre := PLStringList.Strings[8];
          Song.Duration := StrToIntDef(PLStringList.Strings[9], 0);
          Song.Bitrate := StrToIntDef(PLStringList.Strings[10], 0);
          Song.SampleRate := StrToIntDef(PLStringList.Strings[11], 0);
          Song.Channels := StrToIntDef(PLStringList.Strings[12], 0);
          Song.Year := StrToIntDef(PLStringList.Strings[13], 0);
          Song.VoteCount := StrToIntDef(PLStringList.Strings[14], -1);
          Song.Voted := (PLStringList.Strings[15] = '1');
        end;
      finally
        PLStringList.Free();
      end;

      Result := Song.Playing;
    end;
  end;
end;

function THGDClient.GetPlaylist(var PList: TPlaylist): boolean;
var
  Reply: string;
  Msg: string;
  i: integer;
  PLStringList: TStringList;
begin
  Result := False;
  SetLength(PList, 0);

  //only do this if at least connected…
  if FState >= hsConnected then
  begin
    Log('Getting Playlist…');
    SendString('ls');
    Reply := ReceiveString();
    Log('ls reply: ' + Reply);

    Msg := '';
    if ProcessReply(Reply, Msg) then
    begin
      //Playlist came back OK. Parse it up, d00d…
      Log('Number of playlist items: ' + Msg);

      PLStringList := TStringList.Create();

      try
        for i := 1 to StrToIntDef(Msg, 0) do
        begin
          Reply := ReceiveString();
          Log('Playlist item ' + IntToStr(i) + ': ' + Reply);

          PLStringList.Clear();
          ParseHGDPacket(Reply, PLStringList);

          if PLStringList.Count >= HGD_NUM_TRACK_FIELDS then
          begin
            SetLength(PList, Length(PList) + 1);
            PList[Length(PList) - 1].Number :=
              StrToIntDef(PLStringList.Strings[0], 0);

            PList[Length(PList) - 1].Filename := PLStringList.Strings[1];
            PList[Length(PList) - 1].Artist := PLStringList.Strings[2];
            PList[Length(PList) - 1].Title := PLStringList.Strings[3];
            PList[Length(PList) - 1].User := PLStringList.Strings[4];
            PList[Length(PList) - 1].Album := PLStringList.Strings[5];
            PList[Length(PList) - 1].Genre := PLStringList.Strings[6];
            PList[Length(PList) - 1].Duration :=
              StrToIntDef(PLStringList.Strings[7], 0);

            PList[Length(PList) - 1].Bitrate :=
              StrToIntDef(PLStringList.Strings[8], 0);

            PList[Length(PList) - 1].SampleRate :=
              StrToIntDef(PLStringList.Strings[9], 0);

            PList[Length(PList) - 1].Channels :=
              StrToIntDef(PLStringList.Strings[10], 0);

            PList[Length(PList) - 1].Year :=
              StrToIntDef(PLStringList.Strings[11], 0);

            PList[Length(PList) - 1].VoteCount :=
              StrToIntDef(PLStringList.Strings[12], -1);

            PList[Length(PList) - 1].Voted := (PLStringList.Strings[13] = '1');
            PList[Length(PList) - 1].Playing := False;
          end;
        end;
      finally
        PLStringList.Free();
      end;

      Result := True;
    end;
  end;
end;

function THGDClient.QueueSong(Filename: string): boolean;
var
  Reply, Msg: string;
  Fin: File;
  DataArray: array of byte;
  FileSizeValue: int64;
  SizeToSend: int64;
  i: integer;
  Percentage: integer;
begin
  Result := False;

  //only do this if at least authenticated…
  if FState >= hsAuthenticated then
  begin
    FileSizeValue := FileSize(Filename);
    Log('Queueing song ' + ExtractFilename(Filename) + '…');
    SendString('q|' + ExtractFilename(Filename) + '|' +
      IntToStr(FileSizeValue));

    Reply := ReceiveString();
    Log('q reply: ' + Reply);

    Msg := '';
    Result := ProcessReply(Reply, Msg);

    if Result then
    begin
      Log('q successful, sending data…');

      SetLength(DataArray, BLOCK_SIZE);
      AssignFile(Fin, Filename);

      try
        FileMode := fmOpenRead;
        Reset(Fin, 1);

        for i := 0 to (FileSizeValue div BLOCK_SIZE) do
        begin
          if (FileSizeValue - i * BLOCK_SIZE) > BLOCK_SIZE then
            SizeToSend := BLOCK_SIZE
          else
            SizeToSend := FileSizeValue - i * BLOCK_SIZE;

          BlockRead(Fin, DataArray[0], SizeToSend);
          Socket.SendBuffer(@DataArray[0], SizeToSend);

          Percentage := Round((i * BLOCK_SIZE / FileSizeValue) * 100);
          FStatusMessage := 'Uploaded ' +  FloatToStr(Percentage) + '%';
          Log('Uploaded ' +  FloatToStr(Percentage) + '%');

          if Assigned(ProgressCallBack) then ProgressCallBack(Percentage);
        end;
      finally
        CloseFile(fin);
      end;

      SetLength(DataArray, 0);

      Reply := ReceiveString();
      Log('q data send reply: ' + Reply);

      Result := ProcessReply(Reply, Msg);
      if Result then
        FStatusMessage := 'Queued.';
    end
    else
    begin
      Log('q Failed');
      FStatusMessage := 'Error queueing song. ' + GetHGDCErrorMessage(Msg);
    end;
  end;
end;

function THGDClient.VoteOff(id: integer): boolean;
var
  Reply, Msg: string;
begin
  Result := False;

  //only do this if at least authenticated…
  if (FState >= hsAuthenticated) then
  begin
    Log('Crapping on song id ' + IntToStr(id) + '…');
    SendString('vo|' + IntToStr(id));
    Reply := ReceiveString();
    Log('vo reply: ' + Reply);

    Msg := '';
    Result := ProcessReply(Reply, Msg);

    if Result then
    begin
      Log('Vote off Successful');
      FStatusMessage := 'Vote off succeeded.';
    end
    else
    begin
      Log('Vote off Failed');
      FStatusMessage := 'Error voting off song. ' + GetHGDCErrorMessage(Msg);
    end;
  end;
end;

function THGDClient.SkipTrack: boolean;
var
  Reply, Msg: string;
begin
  Result := False;

  //only do this if at least authenticated and user is admin…
  if (FState >= hsAdmin) then
  begin
    Log('Skipping song…');
    SendString('skip');
    Reply := ReceiveString();
    Log('skip reply: ' + Reply);

    Msg := '';
    Result := ProcessReply(Reply, Msg);

    if Result then
    begin
      Log('Skip Successful');
      FStatusMessage := 'Skipped';
    end
    else
    begin
      Log('Skip Failed');
      FStatusMessage := 'Error skipping song. ' + GetHGDCErrorMessage(Msg);
    end;
  end
  else
    Log('User is not admin, not skipping.');
end;

function THGDClient.Pause: boolean;
var
  Reply, Msg: string;
begin
  Result := False;

  //only do this if at least authenticated and user is admin…
  if (FState >= hsAdmin) then
  begin
    Log('Pausing…');
    SendString('pause');
    Reply := ReceiveString();
    Log('pause reply: ' + Reply);

    Msg := '';
    Result := ProcessReply(Reply, Msg);

    if Result then
    begin
      Log('Pause Successful');
    end
    else
    begin
      Log('Pause Failed');
      FStatusMessage := 'Error pausing. ' + GetHGDCErrorMessage(Msg);
    end;
  end
  else
    Log('User is not admin, not pausing.');
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
    Log('Error Occurred: ' + GetHGDCErrorMessage(Msg));
  end
  else
  begin
    Result := False;
    Log('"ok" or "err" not found in packet. Resetting…');
    //Something has gone wrong, so reset
    Disconnect();
    FStatusMessage := 'Connection lost.';
  end;

  //check if the server is booting us
  if (Msg = HGD_RESP_E_SHTDWN) or (Msg = HGD_RESP_E_KICK) then
  begin
    try
      FreeAndNil(Socket);
    except
      //Keep SSL errors from appearing
    end;

    FState := hsDisconnected;
    FStatusMessage := 'Server closed connection. ' + GetHGDCErrorMessage(Msg);
    Log(FStatusMessage);
  end;
end;

procedure THGDClient.ApplyChanges;
begin
  if Connect() and (FUsername <> '') and (FPassword <> '') then
    if SendUser(FUsername, FPassword) then
      if IsAdmin() then
        FState := hsAdmin;
end;

function THGDClient.SendUser(Username, Password: string): boolean;
var
  Reply, Msg: string;
begin
  Result := False;
  //only do this if at least connected…
  if FState >= hsConnected then
  begin
    Log('Sending username…');
    SendString('user|' + Username + '|' + Password);
    Reply := ReceiveString();
    Log('user reply: ' + Reply);

    Msg := '';
    Result := ProcessReply(Reply, Msg);

    if Result then
    begin
      FState := hsAuthenticated;
      Log('SendUser Successful');
      FStatusMessage := 'User authenticated.';
    end
    else
    begin
      Log('SendUser Failed');
      FState := hsConnected;
      FStatusMessage := 'Error logging in. ' + GetHGDCErrorMessage(Msg);
    end;
  end;
end;

function THGDClient.IsAdmin: boolean;
var
  Reply, Msg: string;
  Packets: TStringList;
begin
  Result := False;
  Log('Checking admin status…');
  SendString('id');
  Reply := ReceiveString();
  Log('id reply: ' + Reply);

  Msg := '';
  Result := ProcessReply(Reply, Msg);

  Packets := TStringList.Create();
  Packets.Clear();
  ParseHGDPacket(Reply, Packets);

  if Result and ((StrToIntDef(Packets[2], 0) and $01) = $01) then
  begin
    Result := True;
    Log('User is admin.');
  end
  else
  begin
    Result := False;
    Log('User is not admin.');
  end;

  Packets.Free();
end;

procedure THGDClient.Log(Message: string);
begin
  if FDebug then
    DebugLn(Self.ClassName + #9 + Message);
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

procedure THGDClient.SendString(St: string);
begin
  Socket.SendString(St + ProtoLineEnding);
end;

function THGDClient.ReceiveString: string;
var
  s: string;
  i: integer;
  p: pointer;
begin
  if FEncrypted then
  begin
    p := GetMem(HGD_MAX_LINE);
    Socket.RecvBufferEx(p, HGD_MAX_LINE, Timeout);

    s := '';
    for i := 0 to HGD_MAX_LINE - 1 do
    begin
      s := s + Char((p + i * SizeOf(Char))^);
      if Copy(s, Length(s) - 3, 2) = ProtoLineEnding then
      begin
        s := Copy(s, 1, Length(s) - 4);
        Break;
      end;
    end;

    FreeMem(p);
    Result := s;
  end
  else
    Result := Socket.RecvString(Timeout);
end;

end.
