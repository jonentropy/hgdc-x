//
// Copyright 2011 Tristan Linnell
//    tris@canthack.org
//
// HGDClient.pas - HGD Client in freepascal/Lazarus
//      Uses the synapse networking library

unit HGDClient;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BlckSock, StdCtrls, FileUtil, ssl_openssl, dialogs;

const
  HGD_PROTO: string = '3';
  //protocol is telnet based, use Windows LineEnding
  ProtoLineEnding = #13#10;

type

  THGDCState = (hsNone, hsError, hsConnected, hsUserSet);

  TTrackInfo = record
    Number: integer;
    Filename: string;
    Artist: string;
    Title: string;
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

      FSSL: boolean; //SSl is set
      FEncrypted: boolean; //connection has been successfully encrypted

      Socket: TTCPBlockSocket;

      FState: THGDCState;
      FErrorMsg: string;
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

      constructor Create(HostAddress, HostPort, UserName, Password: string; SSL: boolean); overload;
      constructor Create(HostAddress, HostPort, UserName, Password: string; SSL: boolean; DebugMemo: TMemo); overload;
      destructor Destroy(); override;

      procedure ApplyChanges();
      function GetPlaylist(var PList: TPlaylist): boolean;
      function QueueSong(Filename: string): boolean;
      function VoteOff(): boolean;

      property State: THGDCState read FState;
      property ErrMsg: string read FErrorMsg;

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
  FState := hsNone;
  FErrorMsg := '';

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
  Socket.Free;
end;

procedure THGDClient.Disconnect();
begin
  Socket.SendString('bye');
  Socket.CloseSocket;
end;

function THGDClient.Connect(): boolean;
var
  Reply, Msg: string;
begin
  Disconnect();

  Result := False;
  FState := hsNone;

  Log('Connecting...');

  Socket.Connect(FHostAddress, FHostPort);
  Reply := ReceiveStringAndDeBork();
  Log(Reply);

  Result := ProcessReply(Reply, Msg);

  if Result then
  begin

    if FSSL then
    begin
      Log('Checking if server supoprts encryption...');
      Socket.SendString('encrypt?' + ProtoLineEnding);
      Reply := ReceiveStringAndDeBork();
      Log('Encrypt? Reply: ' + Reply);

      if ProcessReply(Reply, Msg) then
      begin

        Log('Encrypting Socket...');
        Socket.SendString('encrypt' + ProtoLineEnding);
        Socket.SSLDoConnect;

        if Socket.LastError <>  0 then //check for success start of SSL
        begin
          FEncrypted := False;
          FState := hsError;
          Result := False;
          FErrorMsg := 'Error setting SSL';
        end;

        Reply := ReceiveStringAndDeBork();

        Log('Encrypt Reply: ' + Reply);
        if ProcessReply(Reply, Msg) then
        begin
          FEncrypted := True;
        end
        else
          FEncrypted := False;
      end
      else
      begin
        Socket.SSLDoShutdown;
        FEncrypted := False;
      end;
    end
    else
    begin
      FEncrypted := False;
    end;

    if GetProto() <> HGD_PROTO then
    begin
      FState := hsError;
      Result := False;
      FErrorMsg := 'Protocol Version Mismatch';
    end;
  end
  else
    FState := hsConnected;
end;

function THGDClient.GetProto: string;
var
  Reply: ansistring;
begin
  Result := '';
  Log('Getting Proto...');
  Socket.SendString('proto' + ProtoLineEnding);
  Reply := ReceiveStringAndDeBork();
  Log('GetProto Reply: ' + Reply);
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
  log(inttostr(Length(PList)));
  SetLength(PList, 0);

  //only do this if at least Error...
  if FState > hsNone then
  begin
    Log('Getting Playlist...');
    Socket.SendString('ls' + ProtoLineEnding);
    Reply := ReceiveStringAndDeBork();
    Log('GetPlaylist Reply: ' + Reply);
    if ProcessReply(Reply, Msg) then
    begin
      //Playlist came back OK. Parse it up, d00d...
      Log('Number of playlist items: ' + Msg);

      PLStringList := TStringList.Create;

      for i := 1 to StrToIntDef(Msg, 0) do
      begin
        Reply := ReceiveStringAndDeBork();
        Log('Playlist item ' + IntToStr(i) + ': ' + Reply);

        SetLength(PList, Length(PList) + 1);

        PLStringList.Clear;
        ParseHGDPacket(Reply, PLStringList);

        PList[Length(PList) - 1].Number := StrToIntDef(PLStringList.Strings[0], 0);
        PList[Length(PList) - 1].Filename := PLStringList.Strings[1];
        PList[Length(PList) - 1].Artist := PLStringList.Strings[2];
        PList[Length(PList) - 1].Title := PLStringList.Strings[3];
        PList[Length(PList) - 1].User := PLStringList.Strings[4];
      end;

      PLStringList.Free;
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
  Result := False;
  FileSizeValue := FileSize(Filename);
  Log('Queueing track ' + ExtractFilename(Filename) + '...');
  Socket.SendString('q|' + ExtractFilename(Filename) + '|' + IntToStr(FileSizeValue) + ProtoLineEnding);
  Reply := ReceiveStringAndDeBork();
  Log(Reply);

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
    Log(Reply);

    Result := ProcessReply(Reply, Msg);
  end
  else
  begin
    Log('q Failed');
    FErrorMsg := 'Error queueing track ' + Msg;
  end;
end;

function THGDClient.VoteOff(): boolean;
var
  Reply, Msg: string;
begin
  Result := False;
  Log('Crapping on song...');
  Socket.SendString('vo' + ProtoLineEnding);
  Reply := ReceiveStringAndDeBork();
  Log(Reply);

  Result := ProcessReply(Reply, Msg);

  if Result then
  begin
    Log('Vote off Successful');
  end
  else
  begin
    Log('Vote off Failed');
    FErrorMsg := 'Vote off failed ' + Msg;
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
    Log('OK');
  end
  else if Pos('err', Reply) > 0 then
  begin
    Result := False;
    FState := hsError;
    Log('Error Occurred: ' + Msg);
  end
  else
  begin
    Result := False;
    Log('Not OK or Err');
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
  Log(Reply);

  Result := ProcessReply(Reply, Msg);

  if Result then
  begin
    FState := hsUserSet;
    Log('SendUser Successful');
  end
  else
  begin
    Log('SendUser Failed');
    FErrorMsg := 'Error Logging In: ' + Msg;
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
begin
  Result := Copy(Socket.RecvString(Timeout), 1, MaxInt);
end;

end.

