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
  Classes, SysUtils, BlckSock, StdCtrls;

const
  HGD_PROTO = '3';
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
      procedure SetHostAddress(const AValue: string);
      procedure SetHostPort(const AValue: string);
      procedure SetPassword(const AValue: string);
      procedure SetUsername(const AValue: string);

      function SendUser(Username, Password: string): boolean;

    public
      Timeout: integer;

      constructor Create(HostAddress, HostPort, UserName, Password: string; SSL: boolean); overload;
      constructor Create(HostAddress, HostPort, UserName, Password: string; SSL: boolean; DebugMemo: TMemo); overload;
      destructor Destroy; override;

      procedure ApplyChanges;
      function GetPlaylist(var PList: TPlaylist): boolean;

      property State: THGDCState read FState;
      property ErrMsg: string read FErrorMsg;

      property UserName: string read FUsername write SetUsername;
      property Password: string write SetPassword;
      property HostAddress: string read FHostAddress write SetHostAddress;
      property HostPort: string read FHostPort write SetHostPort;
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

  ApplyChanges();
end;

constructor THGDClient.Create(HostAddress, HostPort, UserName,
  Password: string; SSL: boolean; DebugMemo: TMemo);
begin
  FDebugMemo := DebugMemo;
  Create(HostAddress, HostPort, UserName, Password, SSL);
end;

destructor THGDClient.Destroy;
begin
  Disconnect();
  Socket.Free;
end;

procedure THGDClient.Disconnect;
begin
  Socket.SendString('bye');
  Socket.CloseSocket;
end;

function THGDClient.Connect: boolean;
var
  Reply, Msg: string;
begin
  Disconnect();

  Result := False;
  FState := hsNone;

  Log('Connecting...');

  Socket.Connect(FHostAddress, FHostPort);
  Reply := Socket.RecvString(Timeout);
  Log(Reply);

  Result := ProcessReply(Reply, Msg);

  if Result then
  begin
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
  Reply: string;
begin
  Result := '';
  Log('Getting Proto...');
  Socket.SendString('proto' + ProtoLineEnding);
  Reply := Socket.RecvString(Timeout);
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

  //only do this if at least connected...
  if FState > hsConnected then
  begin
    Log('Getting Playlist...');
    Socket.SendString('ls' + ProtoLineEnding);
    Reply := Socket.RecvString(Timeout);
    Log('GetPlaylist Reply: ' + Reply);
    if ProcessReply(Reply, Msg) then
    begin
      //Playlist came back OK. Parse it up, d00d...
      Log('Number of playlist items: ' + Msg);

      PLStringList := TStringList.Create;

      for i := 1 to StrToIntDef(Msg, 0) do
      begin
        Reply := Socket.RecvString(Timeout);
        Log('Playlist item ' + IntToStr(i) + ': ' + Reply);

        SetLength(PList, Length(PList) + 1);

        PLStringList.Clear;
        ParseHGDPacket(Reply, PLStringList);

        //todo parse...
        PList[Length(PList) - 1].Number := StrToIntDef(PLStringList.Strings[0], 0);
        PList[Length(PList) - 1].Filename := PLStringList.Strings[1];
        PList[Length(PList) - 1].Artist := PLStringList.Strings[2];
        PList[Length(PList) - 1].Title := PLStringList.Strings[3];
        PList[Length(PList) - 1].User := PLStringList.Strings[4];
      end;

      PLStringList.Free;

      //ToDo: Parse Playlist here...
      Result := True;
    end;
  end;
end;

procedure THGDClient.ParseHGDPacket(Packet: string; List: TStringList);
begin
  List.Delimiter := '|';
  List.DelimitedText := Packet;
end;

function THGDClient.ProcessReply(Reply: string; var Msg: string): boolean;
begin
  //todo make this parse more than one string from the reply? Maybe?
  //Or leave this as a general check for "OK" then re-parse the message?
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
    Log('Error Occurred: ' + Msg +', Disconnecting');
    Disconnect();
  end
  else
  begin
    //could be blank reply...
    Result := False;
    FState := hsError;
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
  Reply := Socket.RecvString(Timeout);
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
    //todo, do a better error handling scheme.
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

end.

