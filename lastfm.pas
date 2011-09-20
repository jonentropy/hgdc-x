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
// lastfm.pas - Last.fm scrobbling client

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

  { TLastFM }

  TLastFM = class(TObject)
    private
      FUserName: string;
      FSessionKey: string;

      function ReadSettings: boolean;
      procedure SaveSettings;

    public
      function GetAlbumArt(Artist, Album: string; var CoverImage: TImage): boolean;
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

function TLastFM.GetAlbumArt(Artist, Album: string; var CoverImage: TImage): boolean;
var
  Connection: THTTPSend;
  RequestURL: string;
  XMLResponse: TXMLDocument;
  Node: TDOMNode;
  Found: boolean;
  CoverURL: string;
begin
  Connection := THTTPSend.Create();
  XMLResponse := TXMLDocument.Create();

  CoverURL := '';
  try
    Connection.Timeout := 1000;
    RequestURL := API_ROOT_URL + '?method=album.getinfo&api_key=' + API_KEY + '&artist=' + Artist + '&album=' + Album;

    //Todo this didn't work with EncodeURLElement, try something else later
    RequestURL := StringReplace(RequestURL, ' ', '%20', [rfReplaceAll]);

    Connection.HTTPMethod('GET', RequestURL);
    ReadXMLFile(XMLResponse, Connection.Document);
    Node := XMLResponse.DocumentElement.FindNode('album');

    if Assigned(Node) then
    begin
      Node := Node.FindNode('image');
      Found := False;

      while ((not Found) and (Node.Attributes.Item[0].NodeValue <> 'small')) do
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
      //CoverURL := EncodeURLElement(CoverURL); //not working
      Connection.HTTPMethod('GET', CoverURL);
      CoverImage.Picture.LoadFromStream(Connection.Document);
    end;

  finally
    XMLResponse.Free;
    Connection.Free;
  end;

end;

constructor TLastFM.Create;
begin

end;

constructor TLastFM.Create(user: string);
begin
  Create;
end;

destructor TLastFM.Destroy;
begin

end;

end.

