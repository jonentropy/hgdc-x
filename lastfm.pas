//
// Copyright 2011 Tristan Linnell
//    tris@canthack.org
//
// lastfm.pas - Last.fm scrobbling client

unit LastFM;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  API_ROOT_URL = 'http://ws.audioscrobbler.com/2.0/';
  API_KEY = 'KEY_HERE';
  API_SECRET = 'SECRET HERE';
type

  { TLastFM }

  TLastFM = class(TObject)
    private
      FUserName: string;
      FSessionKey: string;

      function ReadSettings: boolean;
      procedure SaveSettings;

    public
      constructor Create; overload;
      constructor Create(user: string); overload;

      destructor Destroy;

  end;

implementation

{ TLastFM }

function TLastFM.ReadSettings: boolean;
begin

end;

procedure TLastFM.SaveSettings;
begin

end;

constructor TLastFM.Create;
begin

end;

constructor TLastFM.Create(user: string);
begin

end;

destructor TLastFM.Destroy;
begin

end;

end.

