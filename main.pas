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
// main.pas - HGD Client GUI

unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, XMLPropStorage, Buttons, Grids, ComCtrls, HGDClient, DebugLog,
  LastFM, Settings, About;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    ApplicationProperties1: TApplicationProperties;
    Bevel1: TBevel;
    btnSkip: TBitBtn;
    btnPause: TBitBtn;
    btnSubmit: TBitBtn;
    btnHGDApply: TButton;
    btnCrapSong: TBitBtn;
    chkSSL: TCheckBox;
    edtHost: TEdit;
    edtPort: TEdit;
    edtUser: TEdit;
    edtPwd: TEdit;
    gbHGDServer: TGroupBox;
    gbNowPlaying: TGroupBox;
    imServer: TImage;
    imPort: TImage;
    imUserNormal: TImage;
    imPassword: TImage;
    imUserAdmin: TImage;
    imVoteOff: TImage;
    imSettings: TImage;
    imNowPlaying: TImage;
    imInsecure: TImage;
    imDebug: TImage;
    imSecure: TImage;
    imAbout: TImage;
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
    lblStatus: TLabel;
    lblHost: TLabel;
    lblPort: TLabel;
    lblUser: TLabel;
    lblPassword: TLabel;
    OpenDialog1: TOpenDialog;
    pbarUpload: TProgressBar;
    sgPlaylist: TStringGrid;
    tmrPlaylist: TTimer;
    tmrState: TTimer;
    XMLPropStorage1: TXMLPropStorage;
    procedure btnCrapSongClick(Sender: TObject);
    procedure btnHGDApplyClick(Sender: TObject);
    procedure btnPauseClick(Sender: TObject);
    procedure btnSkipClick(Sender: TObject);
    procedure btnSubmitClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    procedure FormShow(Sender: TObject);
    procedure imAboutClick(Sender: TObject);
    procedure imDebugClick(Sender: TObject);
    procedure imSettingsClick(Sender: TObject);
    procedure sgPlaylistDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure tmrPlaylistTimer(Sender: TObject);
    procedure tmrStateTimer(Sender: TObject);
  private
    { private declarations }
    FClient: THGDClient;
    FLastFM: TLastFM;
    FCurrentlyDisplayedArtwork: string;
    FArtworkAttempts: integer;
    procedure DisableAllGUI;
    procedure EnableAllGUI;
    procedure Log(Message: string);
    procedure ProgressCallback(Percentage: integer);
    procedure ShowNowPlaying(b: boolean);
    procedure ShowStatus(Msg: string; Error: boolean);

  public
    { public declarations }
  end;

var
  frmMain: TfrmMain;

const
  MAX_ARTWORK_ATTEMPTS = 3;
  VERSION = '0.5.0';

implementation

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.btnHGDApplyClick(Sender: TObject);
begin
  tmrPlaylist.Enabled := False;
  tmrState.Enabled := False;

  ShowStatus('Applying...', False);
  XMLPropStorage1.Save();

  FClient.HostAddress := edtHost.Text;
  FClient.HostPort := edtPort.Text;
  FClient.UserName := edtUser.Text;
  FClient.Password := edtPwd.Text;
  FClient.SSL := chkSSL.Checked;

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

procedure TfrmMain.btnSubmitClick(Sender: TObject);
var
  i: integer;
begin
  DisableAllGUI();

  if OpenDialog1.Execute() then
  begin
    Screen.Cursor := crHourglass;

    for i := 0 to OpenDialog1.Files.Count - 1 do
    begin
      if FClient.State < hsAuthenticated then
        Break;

      pbarUpload.Position := pbarUpload.Min;
      pbarUpload.Visible := True;
      FClient.QueueSong(OpenDialog1.Files[i]);
      pbarUpload.Visible := False;
    end;

    Screen.Cursor := crDefault;
  end;

  EnableAllGUI();
end;

procedure TfrmMain.DisableAllGUI;
begin
  tmrPlayList.Enabled := False;
  tmrState.Enabled := False;
  btnSubmit.Enabled := False;
  btnCrapSong.Enabled := False;
  btnSkip.Enabled := False;
  btnPause.Enabled := False;
  btnHGDApply.Enabled := False;
end;

procedure TfrmMain.EnableAllGUI;
begin
  btnSubmit.Enabled := True;
  btnCrapSong.Enabled := True;
  btnSkip.Enabled := True;
  btnPause.Enabled := True;
  btnHGDApply.Enabled := True;
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
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  FLastFM.Free();
  FClient.Free();
end;

procedure TfrmMain.FormDropFiles(Sender: TObject;
  const FileNames: array of String);
var
  i: integer;
begin
  if FClient.State >= hsAuthenticated then
  begin
    DisableAllGUI();
    Screen.Cursor := crHourglass;

    for i := Low(Filenames) to High(Filenames) do
    begin
      if FClient.State < hsAuthenticated then
        Break;

      pbarUpload.Position := pbarUpload.Min;
      pbarUpload.Visible := True;
      FClient.QueueSong(Filenames[i]);
      pbarUpload.Visible := False;
    end;

    Screen.Cursor := crDefault;
    EnableAllGUI();
  end;
end;

procedure TfrmMain.FormShow(Sender: TObject);
begin
  FClient := THGDClient.Create(edtHost.Text, edtPort.Text, edtUser.Text,
    edtPwd.Text, chkSSL.Checked, frmDebug.Memo1);

  FClient.ProgressCallBack := @ProgressCallback;

  FLastFM := TLastFM.Create(frmSettings.edtLastFMUser.Text, GetAppConfigDirUTF8(False));
  tmrPlaylistTimer(Self);
  edtPwd.SetFocus;
end;

procedure TfrmMain.imAboutClick(Sender: TObject);
begin
  frmAbout.Show();
end;

procedure TfrmMain.imDebugClick(Sender: TObject);
begin
  if not frmDebug.Visible then
    frmDebug.Left := Self.Left + Self.Width + 6;

  frmDebug.Visible := not frmDebug.Visible;
end;

procedure TfrmMain.imSettingsClick(Sender: TObject);
begin
  frmSettings.ShowModal();
end;

procedure TfrmMain.sgPlaylistDrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
var
  S: string;
begin
  if (aRow = 1) and (aCol > 0) then
  begin
    TStringGrid(Sender).Canvas.Brush.Color :=  clHighLight;
    TStringGrid(Sender).Canvas.Font.Color :=  clHighLightText;
    TStringGrid(Sender).Canvas.FillRect(aRect);

    S := TStringGrid(Sender).Cells[aCol, aRow];
    TStringGrid(Sender).Canvas.TextOut(aRect.Left + 2, aRect.Top + 2, S);
  end;
end;

procedure TfrmMain.ProgressCallback(Percentage: integer);
begin
  pBarUpload.Position := Percentage;
  Application.ProcessMessages;
end;

procedure TfrmMain.tmrPlaylistTimer(Sender: TObject);
var
  PL: TPlaylist;
  i: integer;
begin
  PL := nil;

  if Assigned(FClient) and (FClient.State >= hsConnected) then
  begin
    FClient.GetPlaylist(PL);

    if Length(PL) > 0 then
    begin
      //There are some items in the playlist
      btnSkip.Enabled := FClient.State >= hsAuthenticated;
      btnPause.Enabled := FClient.State >= hsAuthenticated;
      btnCrapSong.Enabled := FClient.State >= hsAuthenticated;

      sgPlaylist.RowCount := 1;

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
        lblNoPlaylist.Visible := False;

        //Display now playing info
        if (i = 0) then
        begin
          imVoteOff.Visible := PL[i].Voted;

          if PL[i].Title <> '' then
            lblTitle.Caption := PL[i].Title
          else
            lblTitle.Caption := PL[i].Filename;

          lblArtist.Caption := PL[i].Artist;
          lblAlbum.Caption := PL[i].Album;
          lblGenre.Caption := PL[i].Genre;

          if PL[i].Year > 0 then
            lblYear.Caption := IntToStr(PL[i].Year)
          else
            lblYear.Caption := '';

          if (PL[i].Artist <> '') and (PL[i].Album <> '') then
          begin
            if ((PL[i].Artist + ':' + PL[i].Album) <>
              FCurrentlyDisplayedArtwork) then
            begin
              //Playing track has changed, get artwork
              imNowPlaying.Visible := True;
              Bevel1.Visible := True;

              if FLastFM.GetAlbumArt(PL[i].Artist, PL[i].Album, szMedium,
                  imNowPlaying) then
              begin
                FCurrentlyDisplayedArtwork := PL[i].Artist + ':' + PL[i].Album;
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
                FCurrentlyDisplayedArtwork := PL[i].Artist + ':' + PL[i].Album;
                FArtworkAttempts := 0;
              end;
            end;
          end
          else
          begin
            //No album information to get art with
            imNowPlaying.Visible := False;
            lblNoAlbumArt.Visible := True;
          end;
        end;
      end;
    end
    else
    begin
      //Nothing playing
      sgPlaylist.RowCount := 1;
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
  end
  else
    sgPlaylist.RowCount := 1;
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

procedure TfrmMain.tmrStateTimer(Sender: TObject);
begin
  if Assigned(FClient) then
  begin
    ShowStatus(FClient.StatusMessage, Pos('error',
      LowerCase(FCLient.StatusMessage)) > 0);

    imSecure.Visible := FClient.Encrypted;
    imInsecure.Visible := not FClient.Encrypted;

    if (not FClient.Encrypted) and (chkSSL.Checked) then
      chkSSL.Font.Style:= [fsStrikeOut]
    else
      chkSSL.Font.Style:= [];

    btnSubmit.Enabled := FClient.State >= hsAuthenticated;
    ShowNowPlaying(FClient.State >= hsConnected);

    imUserAdmin.Visible :=  FClient.State >= hsAdmin;
    imUserNormal.Visible :=   FClient.State < hsAdmin;
    btnSkip.Visible := FClient.State >= hsAdmin;
    btnPause.Visible := FClient.State >= hsAdmin;
  end;
end;

procedure TfrmMain.ShowNowPlaying(b: boolean);
begin
  gbNowPlaying.Visible := b;
end;

procedure TfrmMain.Log(Message: string);
begin
  frmDebug.Memo1.Lines.Add(Message);
end;

end.
