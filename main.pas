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
// main.pas - HGD Client GUI

unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, XMLPropStorage, Buttons, Grids, ComCtrls, HGDClient,
  LastFM, Login, About, LCLProc;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    ApplicationProperties1: TApplicationProperties;
    Bevel1: TBevel;
    btnPause: TSpeedButton;
    btnSkip: TSpeedButton;
    gbNowPlaying: TGroupBox;
    imInsecure: TImage;
    imSecure: TImage;
    imVoteOff: TImage;
    imLogin: TImage;
    imNowPlaying: TImage;
    imAbout: TImage;
    lblStatus: TLabel;
    lblSampleRate: TLabel;
    lblGenre: TLabel;
    lblDuration: TLabel;
    lblBitrate: TLabel;
    lblNoAlbumArt: TLabel;
    lblYear: TLabel;
    lblTitle: TLabel;
    lblArtist: TLabel;
    lblAlbum: TLabel;
    lblNoPlaylist: TLabel;
    OpenDialog1: TOpenDialog;
    pbarUpload: TProgressBar;
    sgPlaylist: TStringGrid;
    btnQueue: TSpeedButton;
    btnCrapSong: TSpeedButton;
    tmrPlaylist: TTimer;
    tmrState: TTimer;
    XMLPropStorage1: TXMLPropStorage;
    procedure btnCrapSongClick(Sender: TObject);
    procedure ApplyChanges;
    procedure btnPauseClick(Sender: TObject);
    procedure btnSkipClick(Sender: TObject);
    procedure btnQueueClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const Filenames: array of String);
    procedure FormShow(Sender: TObject);
    procedure imAboutClick(Sender: TObject);
    procedure imLoginClick(Sender: TObject);
    procedure tmrPlaylistTimer(Sender: TObject);
    procedure tmrStateTimer(Sender: TObject);
  private
    { private declarations }
    FClient: THGDClient;
    FLastFM: TLastFM;
    FCurrentlyDisplayedArtwork: string;
    FArtworkAttempts: integer;
    FDebug: boolean;
    frmLogin: TFrmLogin;
    procedure DisableAllGUI;
    procedure EnableAllGUI;
    procedure Log(Message: string);
    procedure ProgressCallback(Percentage: integer);
    function QueueSong(Filename: string): boolean;
    procedure ShowStatus(Msg: string; Error: boolean);
    function UpdateState: boolean;

  public
    { public declarations }
  end;

var
  frmMain: TfrmMain;

const
  MAX_ARTWORK_ATTEMPTS = 3;
  VERSION = '0.5.3';

implementation

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.ApplyChanges;
begin
  tmrPlaylist.Enabled := False;
  tmrState.Enabled := False;

  ShowStatus('Applying...', False);
  frmLogin.XMLPropStorage1.Save();

  FClient.HostAddress := frmLogin.edtHost.Text;
  FClient.HostPort := frmLogin.edtPort.Text;
  FClient.UserName := frmLogin.edtUser.Text;
  FClient.Password := frmLogin.edtPwd.Text;
  FClient.SSL := frmLogin.chkSSL.Checked;

  FClient.ApplyChanges();

  tmrState.Enabled := True;
  tmrPlaylist.Enabled := True;
  tmrPlayListTimer(Self);
end;

procedure TfrmMain.btnPauseClick(Sender: TObject);
begin
  if sgPlaylist.RowCount > 1 then
    FClient.Pause();
end;

procedure TfrmMain.btnSkipClick(Sender: TObject);
begin
  if sgPlaylist.RowCount > 1 then
    FClient.SkipTrack();
end;

procedure TfrmMain.btnCrapSongClick(Sender: TObject);
begin
  if sgPlaylist.RowCount > 1 then
    if FClient.VoteOff(StrToIntDef(sgPlaylist.Cells[0,1], -1)) then
      imVoteOff.Visible := True;
end;

procedure TfrmMain.btnQueueClick(Sender: TObject);
var
  i: integer;
begin
  DisableAllGUI();

  if OpenDialog1.Execute() then
  begin
    btnQueue.Visible := False;
    btnPause.Visible := False;
    btnSkip.Visible := False;
    Screen.Cursor := crHourglass;

    for i := 0 to OpenDialog1.Files.Count - 1 do
    begin
      if not QueueSong(OpenDialog1.Files[i]) then
        Break;
    end;

    Screen.Cursor := crDefault;
    btnQueue.Visible := True;
    btnPause.Visible := FClient.State >= hsAdmin;
    btnSkip.Visible := FClient.State >= hsAdmin;
  end;

  EnableAllGUI();
end;

procedure TfrmMain.DisableAllGUI;
begin
  tmrPlayList.Enabled := False;
  tmrState.Enabled := False;
  btnQueue.Enabled := False;
  btnCrapSong.Enabled := False;
  btnSkip.Enabled := False;
  btnPause.Enabled := False;
  imLogin.Enabled := False;
  frmLogin.btnHGDLogin.Enabled := False;
end;

procedure TfrmMain.EnableAllGUI;
begin
  btnQueue.Enabled := True;
  btnCrapSong.Enabled := True;
  btnSkip.Enabled := True;
  btnPause.Enabled := True;
  imLogin.Enabled := True;
  frmLogin.btnHGDLogin.Enabled := True;
  tmrPlayList.Enabled := True;
  tmrPlayListTimer(Self);
  tmrState.Enabled := True;
  tmrStateTimer(Self);
  Self.SetFocus;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  Self.Caption := Self.Caption + ' ' +  VERSION;
  FArtworkAttempts := 0;
  FCurrentlyDisplayedArtwork := '';
  FDebug := Application.HasOption('d', 'debug');
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  FLastFM.Free();
  FClient.Free();
  frmLogin.Free();
end;

function TfrmMain.QueueSong(Filename: string): boolean;
begin
  Result := False;
  if FClient.State < hsAuthenticated then
    Exit;

  pbarUpload.Position := pbarUpload.Min;
  pbarUpload.Visible := True;
  btnQueue.Enabled := False;

  FClient.QueueSong(Filename);

  btnQueue.Enabled := True;
  pbarUpload.Visible := False;

  Result := True;
end;

procedure TfrmMain.FormDropFiles(Sender: TObject;
  const Filenames: array of String);
var
  i: integer;
begin
  Log(IntToStr(Length(Filenames)) + ' files dropped');
  if FClient.State >= hsAuthenticated then
  begin
    DisableAllGUI();
    Screen.Cursor := crHourglass;

    for i := Low(Filenames) to High(Filenames) do
    begin
      if not QueueSong(Filenames[i]) then
        Break;
    end;

    Screen.Cursor := crDefault;
    EnableAllGUI();
  end;
end;

procedure TfrmMain.FormShow(Sender: TObject);
begin
  Log('Creating login GUI...');
  frmLogin := TFrmLogin.Create(Self);

  {$IFDEF WINDOWS}
  if ForceDirectoriesUTF8(GetAppConfigDirUTF8(False)) then
  begin
    frmLogin.XMLPropStorage1.FileName := GetAppConfigDirUTF8(False) +
      'settings.xml';

    XMLPropStorage1.FileName := GetAppConfigDirUTF8(False) + 'settings.xml';

    XMLPropStorage1.Restore;
    frmLogin.XMLPropStorage1.Restore;
  end
  else
  begin
    XMLPropStorage1.Active := False;
    frmLogin.XMLPropStorage1.Active := False;
  end;
  {$ENDIF WINDOWS}

  Log('Creating HGD client...');
  FClient := THGDClient.Create(frmLogin.edtHost.Text, frmLogin.edtPort.Text,
    frmLogin.edtUser.Text, frmLogin.edtPwd.Text, frmLogin.chkSSL.Checked,
    FDebug);

  FClient.ProgressCallBack := @ProgressCallback;

  Log('Creating LastFM webservices client...');
  FLastFM := TLastFM.Create(frmLogin.edtLastFMUser.Text,
    GetAppConfigDirUTF8(False), FDebug);

  UpdateState();
  tmrPlaylistTimer(Self);
end;

procedure TfrmMain.imAboutClick(Sender: TObject);
begin
  frmAbout.Show();
end;

procedure TfrmMain.imLoginClick(Sender: TObject);
begin
  if mrOK = frmLogin.ShowModal() then
    ApplyChanges();
end;

procedure TfrmMain.ProgressCallback(Percentage: integer);
begin
  pBarUpload.Position := Percentage;
  ShowStatus(FClient.StatusMessage, False);
  Application.ProcessMessages();
end;

procedure TfrmMain.tmrPlaylistTimer(Sender: TObject);
var
  PL: TPlaylist;
  i: integer;
  NowPlayingSong: TTrackInfo;
begin
  tmrPlaylist.Enabled := False;
  PL := nil;

  if Assigned(FClient) and (FClient.State >= hsConnected) then
  begin
    //Display now playing info
    if FClient.GetNowPlaying(NowPlayingSong) then
    begin
      //A song is currently playing
      lblNoPlaylist.Visible := False;
      btnSkip.Enabled := FClient.State >= hsAuthenticated;
      btnPause.Enabled := FClient.State >= hsAuthenticated;
      btnCrapSong.Enabled := FClient.State >= hsAuthenticated;

      imVoteOff.Visible := NowPlayingSong.Voted;

      if NowPlayingSong.Title <> '' then
        lblTitle.Caption := NowPlayingSong.Title
      else
        lblTitle.Caption := NowPlayingSong.Filename;

      lblArtist.Caption := NowPlayingSong.Artist;
      lblAlbum.Caption := NowPlayingSong.Album;
      lblGenre.Caption := NowPlayingSong.Genre;

      if NowPlayingSong.Year > 0 then
        lblYear.Caption := IntToStr(NowPlayingSong.Year)
      else
        lblYear.Caption := '';

      if (NowPlayingSong.Artist <> '') and (NowPlayingSong.Album <> '') then
      begin
        if ((NowPlayingSong.Artist + ':' + NowPlayingSong.Album) <>
          FCurrentlyDisplayedArtwork) then
        begin
          //Playing track has changed, get artwork
          imNowPlaying.Visible := True;
          Bevel1.Visible := True;

          Log('Attempt ' + IntToStr(FArtworkAttempts + 1) +
            ' at fetching album art.');

          if FLastFM.GetAlbumArt(NowPlayingSong.Artist, NowPlayingSong.Album,
            szMedium, imNowPlaying) then
          begin
            FCurrentlyDisplayedArtwork := NowPlayingSong.Artist + ':' +
              NowPlayingSong.Album;

            FArtworkAttempts := 0;
          end
          else
          begin
            //Couldn't get artwork, so hide it
            Inc(FArtworkAttempts);
            imNowPlaying.Visible := False;
            lblNoAlbumArt.Visible := True;
          end;

          if (FArtworkAttempts = MAX_ARTWORK_ATTEMPTS) then
          begin
            Log('Too many artwork attempts, not trying again.');
            FCurrentlyDisplayedArtwork := NowPlayingSong.Artist + ':' +
              NowPlayingSong.Album;

            FArtworkAttempts := 0;
          end;
        end;
      end
      else
      begin
        //No album information to get art with
        Log('No album information to get art with.');
        imNowPlaying.Visible := False;
        lblNoAlbumArt.Visible := True;
      end;
    end
    else
    begin
      //Nothing playing
      Log('Nothing is playing.');
      lblTitle.Caption := '';
      lblArtist.Caption := '';
      lblAlbum.Caption := '';
      lblGenre.Caption := '';
      lblBitrate.Caption := '';
      lblYear.Caption := '';
      lblSampleRate.Caption := '';
      lblDuration.Caption := '';
      lblNoPlaylist.Visible := True;
      btnSkip.Enabled := False;
      btnPause.Enabled := False;
      btnCrapSong.Enabled := False;
      FCurrentlyDisplayedArtwork := '';
      imNowPlaying.Picture.Clear;
      imNowPlaying.Visible := False;
      Bevel1.Visible := False;
      lblNoAlbumArt.Visible := False;
      imVoteOff.Visible := False;
    end;

    //Now get the rest of the playlist for the string grid...
    sgPlaylist.RowCount := sgPlaylist.FixedRows;
    FClient.GetPlaylist(PL);

    if Length(PL) > 0 then
    begin
      //There are some items in the playlist

      for i := 0 to Length(PL) - 1 do
      begin
        sgPlaylist.RowCount := sgPlaylist.RowCount + 1;
        sgPlaylist.Cells[0, sgPlaylist.RowCount -1] := IntToStr(PL[i].Number);

        if PL[i].Title <> '' then
          sgPlaylist.Cells[1, sgPlaylist.RowCount -1] := PL[i].Title
        else
          sgPlaylist.Cells[1, sgPlaylist.RowCount -1] := PL[i].Filename;

        sgPlaylist.Cells[2, sgPlaylist.RowCount -1] := PL[i].Artist;
        sgPlaylist.Cells[3, sgPlaylist.RowCount -1] := PL[i].Album;
        sgPlaylist.Cells[4, sgPlaylist.RowCount -1] := PL[i].User;

      end;
    end;
  end
  else
    sgPlaylist.RowCount := 1;

  tmrPlaylist.Enabled := True;
end;

procedure TfrmMain.ShowStatus(Msg: string; Error: boolean);
begin
  if Error then
  begin
    lblStatus.Font.Color := clRed;
  end
  else
  begin
    lblStatus.Font.Color := clBlue;
  end;

  lblStatus.Caption := Msg;
end;

//todo make fn to pop up login window?

procedure TfrmMain.tmrStateTimer(Sender: TObject);
begin
  tmrState.Enabled := False;
  if UpdateState() then
    tmrState.Enabled := True;
end;

function TfrmMain.UpdateState: boolean;
var
  ErrorState: boolean;
begin
  Result := True;
  if Assigned(FClient) then
  begin

    ErrorState := Pos('error', LowerCase(FClient.StatusMessage)) > 0;
    ShowStatus(FClient.StatusMessage, ErrorState);

    imSecure.Visible := FClient.Encrypted;
    imInsecure.Visible := not FClient.Encrypted;

    if (not FClient.Encrypted) and (frmLogin.chkSSL.Checked) then
      frmLogin.chkSSL.Font.Style:= [fsStrikeOut]
    else
      frmLogin.chkSSL.Font.Style:= [];

    btnQueue.Enabled := FClient.State >= hsAuthenticated;
    btnSkip.Visible := FClient.State >= hsAdmin;
    btnPause.Visible := FClient.State >= hsAdmin;

    if (FClient.State < hsAuthenticated) and (not frmLogin.Visible) then
    begin
      if mrCancel = frmLogin.ShowModal() then
        Result := False
      else
        ApplyChanges();
    end;
  end;
end;

procedure TfrmMain.Log(Message: string);
begin
  if FDebug then
    DebugLn(Self.ClassName + #9 + Message);
end;

end.
