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
  //protocol is telnet based, override lineending
  LineEnding = #13#10;
type

  THGDCState = (hsNone, hsError, hsConnected, hsUserSet);

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
  Socket.SendString('proto' + LineEnding);
  Reply := Socket.RecvString(Timeout);
  Log('GetProto Reply: ' + Reply);
  if not ProcessReply(Reply, Result) then
    Result := '';
end;

function THGDClient.ProcessReply(Reply: string; var Msg: string): boolean;
begin
  //todo make this parse more than one string from the reply
  Msg := Copy(Reply, Pos('|', Reply) + 1, MaxInt);

  if Pos('ok', Reply) > 0 then
  begin
    Result := True;
    Log('OK');
  end
  else
  begin
    Result := False;
    FState := hsError;
    Disconnect();

    if Pos('err', Reply) > 0 then
    begin
      Log('Error Occurred: ' + Msg);
    end;
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
  Socket.SendString('user|' + Username + '|' + Password + LineEnding);
  Reply := Socket.RecvString(Timeout);
  Log(Reply);

  Result := ProcessReply(Reply, Msg);

  if Result then
  begin
    FState := hsUserSet;
    Log('User Logged In');
  end
  else
  begin
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

end.

