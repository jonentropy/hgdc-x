/*
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
 */

// hgdc-x Cross Platform hgd client written in Lazarus/Freepascal
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

