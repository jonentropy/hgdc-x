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
  LastFM, Login, About, LCLProc, Menus;

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
    imNowPlaying: TImage;
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
    mitmPause: TMenuItem;
    mitmVoteOff: TMenuItem;
    mitmSkip: TMenuItem;
    mitmAdminDivider: TMenuItem;
    mitmQueue: TMenuItem;
    mitmSong: TMenuItem;
    mitmAbout: TMenuItem;
    mitmHelp: TMenuItem;
    mitmQuit: TMenuItem;
    mitmFile: TMenuItem;
    mitmLogin: TMenuItem;
    mitmHGD: TMenuItem;
    mitmAboutMac: TMenuItem;
    mitmApple: TMenuItem;
    mnuMain: TMainMenu;
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
    procedure mitmAboutClick(Sender: TObject);
    procedure mitmAboutMacClick(Sender: TObject);
    procedure mitmLoginClick(Sender: TObject);
    procedure mitmPauseClick(Sender: TObject);
    procedure mitmQueueClick(Sender: TObject);
    procedure mitmQuitClick(Sender: TObject);
    procedure mitmSkipClick(Sender: TObject);
    procedure mitmVoteOffClick(Sender: TObject);
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
    procedure SetGUIForOS;
    procedure DisableAllGUI;
    procedure EnableAllGUI;
    procedure Log(Message: string);
    procedure ProgressCallback(Percentage: integer);
    function QueueSong(Filename: string): boolean;
    procedure ShowStatus(Msg: string; Error: boolean);
    function UpdateState: boolean;
    procedure QueueSongs;
    procedure VoteOff;
    procedure Skip;
    procedure Pause;

  public
    { public declarations }
  end;

var
  frmMain: TfrmMain;

const
  MAX_ARTWORK_ATTEMPTS = 3;
  VERSION = '0.5.4';

implementation

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.ApplyChanges;
begin
  tmrPlaylist.Enabled := False;
  tmrState.Enabled := False;

  ShowStatus('Applying…', False);
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
  Pause();
end;

procedure TfrmMain.Pause;
begin
  if sgPlaylist.RowCount > 1 then
    FClient.Pause();
end;

procedure TfrmMain.btnSkipClick(Sender: TObject);
begin
  Skip();
end;

procedure TfrmMain.Skip;
begin
  if sgPlaylist.RowCount > 1 then
    FClient.SkipTrack();
end;

procedure TfrmMain.btnCrapSongClick(Sender: TObject);
begin
  VoteOff();
end;

procedure TfrmMain.VoteOff;
begin
  if sgPlaylist.RowCount > 1 then
    if FClient.VoteOff(StrToIntDef(sgPlaylist.Cells[0,1], -1)) then
      imVoteOff.Visible := True;
end;

procedure TfrmMain.btnQueueClick(Sender: TObject);
begin
  QueueSongs();
end;

procedure TfrmMain.QueueSongs;
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

  Sleep(800);
  EnableAllGUI();
end;

procedure TfrmMain.DisableAllGUI;
begin
  tmrPlaylist.Enabled := False;
  tmrState.Enabled := False;
  btnQueue.Enabled := False;
  btnCrapSong.Enabled := False;
  btnSkip.Enabled := False;
  btnPause.Enabled := False;
  mitmLogin.Enabled := False;
  frmLogin.btnHGDLogin.Enabled := False;
  mitmPause.Enabled := False;
  mitmSkip.Enabled := False;
  mitmQueue.Enabled := False;
  mitmVoteOff.Enabled := False;
  mitmLogin.Enabled := False;
end;

procedure TfrmMain.EnableAllGUI;
begin
  btnQueue.Enabled := True;
  btnCrapSong.Enabled := True;
  btnSkip.Enabled := True;
  btnPause.Enabled := True;
  mitmLogin.Enabled := True;
  frmLogin.btnHGDLogin.Enabled := True;
  tmrState.Enabled := True;
  mitmPause.Enabled := True;
  mitmSkip.Enabled := True;
  mitmQueue.Enabled := True;
  mitmVoteOff.Enabled := True;
  mitmLogin.Enabled := True;
  tmrStateTimer(Self);
  tmrPlaylist.Enabled := True;
  tmrPlaylistTimer(Self);
  Self.SetFocus;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  FDebug := Application.HasOption('d', 'debug');
  SetGUIForOS();
  Self.Caption := Self.Caption + ' ' +  VERSION;
  FArtworkAttempts := 0;
  FCurrentlyDisplayedArtwork := '';
end;

procedure TfrmMain.SetGUIForOS;
begin
  //Sets up platform specifics in the user interface, e.g. Mac about menus etc.

  {$IFDEF DARWIN}
  //Running on Mac OS X, so remove About and File and Quit.
  //In the future if other menus are added to these menus, remove the free
  //of the top level menu (File).

  mitmAbout.Free;
  mitmQuit.Free;
  mitmFile.Free;
  {$ELSE DARWIN}
  //Not Mac OS X, so Apple menu not needed
  mitmApple.Free;
  {$ENDIF DARWIN}
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
  Log('Creating login GUI…');
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

  Log('Creating HGD client…');
  FClient := THGDClient.Create(FDebug);
  FClient.ProgressCallBack := @ProgressCallback;

  Log('Creating LastFM webservices client…');
  FLastFM := TLastFM.Create(frmLogin.edtLastFMUser.Text,
    GetAppConfigDirUTF8(False), FDebug);

  UpdateState();
  tmrPlaylistTimer(Self);
end;

procedure TfrmMain.mitmAboutClick(Sender: TObject);
begin
  frmAbout.ShowModal();
end;

procedure TfrmMain.mitmAboutMacClick(Sender: TObject);
begin
  frmAbout.Show();
end;

procedure TfrmMain.mitmLoginClick(Sender: TObject);
begin
  tmrPlaylist.Enabled := False;

  if mrOK = frmLogin.ShowModal() then
    ApplyChanges();

  tmrPlaylist.Enabled := True;
end;

procedure TfrmMain.mitmPauseClick(Sender: TObject);
begin
  Pause();
end;

procedure TfrmMain.mitmQueueClick(Sender: TObject);
begin
  QueueSongs();
end;

procedure TfrmMain.mitmQuitClick(Sender: TObject);
begin
  Close();
end;

procedure TfrmMain.mitmSkipClick(Sender: TObject);
begin
  Skip();
end;

procedure TfrmMain.mitmVoteOffClick(Sender: TObject);
begin
  VoteOff();
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
  ListRow: integer;
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
      btnSkip.Enabled := FClient.State >= hsAdmin;
      btnPause.Enabled := FClient.State >= hsAdmin;
      btnCrapSong.Enabled := FClient.State >= hsAuthenticated;
      mitmVoteOff.Enabled := FClient.State >= hsAuthenticated;
      mitmPause.Enabled := FClient.State >= hsAdmin;
      mitmSkip.Enabled := FClient.State >= hsAdmin;

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
            szLarge, imNowPlaying) then
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
            FCurrentlyDisplayedArtwork := '';
          end;

          if (FArtworkAttempts = MAX_ARTWORK_ATTEMPTS) then
          begin
            Log('Too many artwork attempts, not trying again.');

            //Ensure artwork wont fetch again…
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
      mitmSkip.Enabled := False;
      btnPause.Enabled := False;
      mitmPause.Enabled := False;
      btnCrapSong.Enabled := False;
      mitmVoteOff.Enabled := False;
      FCurrentlyDisplayedArtwork := '';
      imNowPlaying.Picture.Clear;
      imNowPlaying.Visible := False;
      Bevel1.Visible := False;
      lblNoAlbumArt.Visible := False;
      imVoteOff.Visible := False;
    end;

    //Now get the rest of the playlist for the string grid…
    ListRow := sgPlaylist.Row;
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

      sgPlayList.Row := ListRow;
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

    btnCrapSong.Enabled := btnCrapSong.Enabled and
      (FClient.State >= hsAuthenticated);

    mitmVoteOff.Enabled := mitmVoteOff.Enabled and
      (FClient.State >= hsAuthenticated);

    btnQueue.Enabled := FClient.State >= hsAuthenticated;
    mitmQueue.Enabled := FClient.State >= hsAuthenticated;
    btnSkip.Visible := FClient.State >= hsAdmin;
    mitmSkip.Visible := FClient.State >= hsAdmin;
    btnPause.Visible := FClient.State >= hsAdmin;
    mitmPause.Visible := FClient.State >= hsAdmin;
    mitmAdminDivider.Visible := FClient.State >= hsAdmin;

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