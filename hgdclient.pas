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

      FDebugMemo: TMemo;

      function Connect: boolean;
      procedure Log(Message: string);
      procedure SetHostAddress(const AValue: string);
      procedure SetHostPort(const AValue: string);
      procedure SetPassword(const AValue: string);
      procedure SetUsername(const AValue: string);

      function SendUser(Username, Password: string): boolean;

    public
      Timeout: integer;

      constructor Create(HostAddress, HostPort, UserName, Password: string; SSL: boolean); overload;
      constructor Create(HostAddress, HostPort, UserName, Password: string; SSL: boolean; DebugMemo: TMemo); overload;
      destructor Destroy;

      procedure ApplyChanges;
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
  Socket.CloseSocket;
  Socket.Free;
end;

function THGDClient.Connect: boolean;
var
  Reply: string;
begin
  Result := False;
  FState := hsNone;
  Socket.CloseSocket;

  Log('Connecting...');

  Socket.Connect(FHostAddress, FHostPort);
  Reply := Socket.RecvString(Timeout);
  Log(Reply);

  if Pos('ok', Reply) > 0 then
  begin
    Result := True;
    FState := hsConnected;
    Log('Connected');
  end
  else
  begin
    FState := hsNone;
    Socket.CloseSocket;
    if Pos('err', Reply) > 0 then
      Log(Copy(Reply, Pos('|', Reply) + 1, MaxInt));
  end;

  //Socket.LastError;
end;

procedure THGDClient.ApplyChanges;
begin
  if Connect() then
    SendUser(FUsername, FPassword);
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

function THGDClient.SendUser(Username, Password: string): boolean;
var
  Reply: string;
begin
  Result := False;
  Socket.SendString('user|' + Username + '|' + Password + LineEnding);
  Reply := Socket.RecvString(Timeout);
  Log(Reply);

  if Pos('ok', Reply) > 0 then
  begin
    Result := True;
    FState := hsUserSet;
    Log('User Logged In');
  end
  else
  begin
    FState := hsNone;
    Socket.CloseSocket;
    if Pos('err', Reply) > 0 then
      Log(Copy(Reply, Pos('|', Reply) + 1, MaxInt));
  end;
end;

procedure THGDClient.Log(Message: string);
begin
  FDebugMemo.Lines.Add(Message);
end;

end.

