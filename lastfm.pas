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
// lastfm.pas - Last.fm web service client for scrobbling and album
//              art fetching.

unit LastFM;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls, HTTPSend, SynaCode, XMLRead, DOM, FileUtil,
  LCLProc;

const
  API_ROOT_URL = 'http://ws.audioscrobbler.com/2.0/';
  API_KEY = 'd73bc1bcce5c08bd73c5f941480845e8';
  API_SECRET = 'SECRET_HERE';
type

  TLastFMSong = record
    Artist: string;
    Album: string;
    Duration: integer;
    TimeListened: TDateTime;
  end;

  TLastFMAlbumSize = (szSmall, szMedium, szLarge, szExtraLarge, szMega);

  { TLastFM }

  TLastFM = class(TObject)
    private
      FUserName: string;
      FCacheDirectory: string;
      FDebug: boolean;
      FScrobbleTimer: TTimer;
      FNowPlayingSong: TLastFMSong;
      FScrobbleList: TList;

      procedure Log(Message: String);
      function ReplaceIllegalFilenameChars(Input: string): string;
      procedure ScrobbleTimer(Sender: TObject);
      procedure SetPlayingSong(Song: TLastFMSong);

    public
      function GetAlbumArt(Artist, Album: string; Size: TLastFMAlbumSize;
        var CoverImage: TImage): boolean;

      constructor Create; overload;
      constructor Create(User: string); overload;
      constructor Create(User: string; CacheDirectory: string;
        Debug: boolean); overload;

      destructor Destroy; override;
  end;

implementation

{ TLastFM }

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
  CacheName: string;
begin
  Result := False;

  CacheName := FCacheDirectory + ReplaceIllegalFilenameChars(Artist +
    '-' + Album) + '.png';

  if (FCacheDirectory <> '') and
    (FileExistsUTF8(CacheName)) then
  begin
    Log('Album art is cached (' + CacheName + ')');
    CoverImage.Picture.LoadFromFile(CacheName);
    Exit(True);
  end;

  Log('Attempting to get artwork from Last.fm');
  Connection := THTTPSend.Create();
  XMLResponse := TXMLDocument.Create();

  CoverURL := '';
  try
    Connection.Timeout := 1000;
    RequestURL := API_ROOT_URL + '?method=album.getinfo&api_key=' + API_KEY +
      '&artist=' + EncodeURLElement(Artist) + '&album=' +
      EncodeURLElement(Album) + '&autocorrect=1';

    if Connection.HTTPMethod('GET', RequestURL) then
    begin
      try
        Log('Got XML response for album ' + Album + ' by ' + Artist);
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

        while (not Found) and (Node.Attributes.Item[0].NodeValue <> ImStr) do
        begin
          if Node.NextSibling.NodeName = 'image' then
            Node := Node.NextSibling
          else
            Found := True;
        end;
        if Node.FirstChild <> nil then
          CoverURL := Node.FirstChild.NodeValue;
      end
      else
        Log('Last.fm didn''t respond with album art.');

      if CoverURL <> '' then
      begin
        Log('Fetching the album cover image');
        //Get the cover
        CoverURL := EncodeURL(CoverURL);
        if Connection.HTTPMethod('GET', CoverURL) then
        begin
          Log('Album art downloaded, caching...');

          try
            CoverImage.Picture.LoadFromStream(Connection.Document);

            //Cache album art...
            if (FCacheDirectory <> '') and
              DirectoryIsWritable(FCacheDirectory) then
            begin
              CoverImage.Picture.SaveToFile(CacheName, 'png');
              Result := True;
            end
            else
              Log('There was a problem caching the album art.');

          except
            Log('Exception when loading image from stream.');
          end;
        end;
      end
      else
        log('No URL found for album art.');
    end;
  finally
    XMLResponse.Free();
    Connection.Free();
  end;
end;

constructor TLastFM.Create;
begin
  inherited Create();

  FScrobbleList := TList.Create;

  FScrobbleTimer := TTimer.Create(nil);
  FScrobbleTimer.Interval := 2000;
  FScrobbleTimer.Enabled := False;
  FScrobbleTimer.OnTimer := @ScrobbleTimer;
end;

constructor TLastFM.Create(User: string);
begin
  FUserName := User;
  Create();
end;

constructor TLastFM.Create(User: string; CacheDirectory: string;
    Debug: boolean);
begin
  if ForceDirectoriesUTF8(CacheDirectory) then
    FCacheDirectory := CacheDirectory
  else
    FCacheDirectory := '';

  FDebug := Debug;
  Create(User);
end;

procedure TLastFM.SetPlayingSong(Song: TLastFMSong);
begin
  FNowPlayingSong := Song;
end;

procedure TLastFM.ScrobbleTimer(Sender: TObject);
begin
  //if song has been set
  //  set now playing

  //inc time

  //if time > duration/2 then add to scrobble list

  //for all in scrobble list,
    //try and scrobble
    //if succesul remve from list

  //
end;

destructor TLastFM.Destroy;
begin
  //save scobble list...
  FScrobbleList.Free();
  FScrobbleTimer.Free();
  inherited Destroy();
end;

procedure TLastFM.Log(Message: string);
begin
  if FDebug then
    DebugLn(Self.ClassName + #9#9 + Message);
end;

function TLastFM.ReplaceIllegalFilenameChars(Input: string): string;
begin
  Result := StringReplace(Input, '/', '-', [rfReplaceAll]);
  Result := StringReplace(Result, '\', '-', [rfReplaceAll]);
  Result := StringReplace(Result, ':', '-', [rfReplaceAll]);
  Result := StringReplace(Result, '*', '-', [rfReplaceAll]);
  Result := StringReplace(Result, '<', '-', [rfReplaceAll]);
  Result := StringReplace(Result, '>', '-', [rfReplaceAll]);
  Result := StringReplace(Result, '?', '-', [rfReplaceAll]);
  Result := StringReplace(Result, '"', '-', [rfReplaceAll]);
  Result := StringReplace(Result, '|', '-', [rfReplaceAll]);
end;

end.

