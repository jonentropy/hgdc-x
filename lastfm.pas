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
// lastfm.pas - Last.fm web service client for scrobbling and album
//              art fetching.

unit LastFM;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls, HTTPSend, SynaCode, XMLRead, DOM;

const
  API_ROOT_URL = 'http://ws.audioscrobbler.com/2.0/';
  API_KEY = 'd73bc1bcce5c08bd73c5f941480845e8';
  API_SECRET = 'SECRET_HERE';
type

  TLastFMAlbumSize = (szSmall, szMedium, szLarge, szExtraLarge, szMega);

  { TLastFM }

  TLastFM = class(TObject)
    private
      FUserName: string;
      FSessionKey: string;

      function ReadSettings: boolean;
      procedure SaveSettings;

    public
      function GetAlbumArt(Artist, Album: string; Size: TLastFMAlbumSize;
        var CoverImage: TImage): boolean;

      constructor Create; overload;
      constructor Create(User: string); overload;

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

function TLastFM.GetAlbumArt(Artist, Album: string; Size: TLastFMAlbumSize;
  var CoverImage: TImage): boolean;
var
  Connection: THTTPSend;
  RequestURL: string;
  XMLResponse: TXMLDocument;
  Node: TDOMNode;
  Found: boolean;
  CoverURL: string;
  ImStr: string;
begin
  Result := False;
  Connection := THTTPSend.Create();
  XMLResponse := TXMLDocument.Create();

  CoverURL := '';
  try
    Connection.Timeout := 1000;
    RequestURL := API_ROOT_URL + '?method=album.getinfo&api_key=' + API_KEY +
      '&artist=' + Artist + '&album=' + Album + '&autocorrect=1';

    RequestURL := EncodeURL(RequestURL);

    if Connection.HTTPMethod('GET', RequestURL) then
    begin
      try
        ReadXMLFile(XMLResponse, Connection.Document);
        Node := XMLResponse.DocumentElement.FindNode('album');
      except
        Node := nil;
      end;

      if Assigned(Node) then
      begin
        Node := Node.FindNode('image');
        Found := False;

        case Size of
        szSmall: ImStr := 'small';
        szLarge: ImStr := 'large';
        szExtraLarge: ImStr := 'extralarge';
        szMega: ImStr := 'mega';
        else
          ImStr := 'medium';
        end;

        while ((not Found) and (Node.Attributes.Item[0].NodeValue <> ImStr)) do
        begin
          if Node.NextSibling.NodeName = 'image' then
            Node := Node.NextSibling
          else
            Found := True;
        end;
        if Node.FirstChild <> nil then
          CoverURL := Node.FirstChild.NodeValue;
      end;

      if CoverURL <> '' then
      begin
        //Get the cover
        CoverURL := EncodeURL(CoverURL);
        if Connection.HTTPMethod('GET', CoverURL) then
        begin
          CoverImage.Picture.LoadFromStream(Connection.Document);
          Result := True;
        end;
      end;
    end;
  finally
    XMLResponse.Free();
    Connection.Free();
  end;

end;

constructor TLastFM.Create;
begin

end;

constructor TLastFM.Create(User: string);
begin
  FUserName := User;
  Create();
end;

destructor TLastFM.Destroy;
begin

end;

end.

