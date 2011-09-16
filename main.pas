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
  ExtCtrls, XMLPropStorage, Buttons, Grids, ComCtrls, HGDClient, DebugLog, LastFM;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    btnSubmit: TBitBtn;
    btnLastFMApply: TBitBtn;
    btnHGDApply: TButton;
    btnCrapSong: TBitBtn;
    chkScrobbling: TCheckBox;
    chkScrobbling1: TCheckBox;
    chkSSL: TCheckBox;
    edtHost: TEdit;
    edtPort: TEdit;
    edtUser: TEdit;
    edtPwd: TEdit;
    gbHGDServer: TGroupBox;
    GroupBox1: TGroupBox;
    imInsecure: TImage;
    imLastFM: TImage;
    edtLastFMUser: TLabeledEdit;
    imDebug: TImage;
    imSecure: TImage;
    lblNowPlaying: TLabel;
    lblLastFM: TLabel;
    lblError: TLabel;
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
    procedure btnSubmitClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure imDebugClick(Sender: TObject);
    procedure sgPlaylistDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure tmrPlaylistTimer(Sender: TObject);
    procedure tmrStateTimer(Sender: TObject);
  private
    { private declarations }
    FClient: THGDClient;
    procedure ShowStatus(Msg: string; Error: boolean);

  public
    { public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.btnHGDApplyClick(Sender: TObject);
begin
  tmrPlaylist.Enabled := False;
  ShowStatus('Applying...', False);

  FClient.HostAddress := edtHost.Text;
  FClient.HostPort := edtPort.Text;
  FClient.UserName := edtUser.Text;
  FClient.Password := edtPwd.Text;
  FClient.SSL := chkSSL.Checked;

  FClient.ApplyChanges();
  tmrPlaylist.Enabled := True;
  tmrPlayListTimer(Self);
end;

procedure TfrmMain.btnCrapSongClick(Sender: TObject);
begin
  FClient.VoteOff();
end;

procedure TfrmMain.btnSubmitClick(Sender: TObject);
begin
  TBitBtn(Sender).Enabled := False;

  if OpenDialog1.Execute() then
  begin
    tmrPlayList.Enabled := False;
    Screen.Cursor := crHourglass;
    Application.ProcessMessages;
    FClient.QueueSong(OpenDialog1.FileName);
    tmrPlayList.Enabled := True;
    tmrPlayListTimer(Self);
    Screen.Cursor := crDefault;
  end;

  TBitBtn(Sender).Enabled := True;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin

end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  FClient.Free();
end;

procedure TfrmMain.FormShow(Sender: TObject);
begin
  FClient := THGDClient.Create(edtHost.Text, edtPort.Text, edtUser.Text, edtPwd.Text, chkSSL.Checked, frmDebug.Memo1);
end;

procedure TfrmMain.imDebugClick(Sender: TObject);
begin
  frmDebug.Show();
end;

procedure TfrmMain.sgPlaylistDrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
var
  S: string;
begin
  if (aRow = 1) and (aCol > 0) then
  begin
    TStringGrid(Sender).Canvas.Brush.Color :=  clHighLight;
    TStringGrid(Sender).Canvas.FillRect(aRect);

    S := TStringGrid(Sender).Cells[aCol, aRow];
    TStringGrid(Sender).Canvas.TextOut(aRect.Left + 2, aRect.Top + 2, S);
  end;
end;

procedure TfrmMain.tmrPlaylistTimer(Sender: TObject);
var
  PL: TPlaylist;
  i: integer;

begin
  sgPlaylist.RowCount := 1;
  FClient.GetPlaylist(PL);

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

procedure TfrmMain.ShowStatus(Msg: string; Error: boolean);
begin
  if Error then
  begin
    lblError.Font.Color := clRed;
  end
  else
  begin
    lblError.Font.Color := clBlue;
  end;

  lblError.Caption := Msg;
end;

procedure TfrmMain.tmrStateTimer(Sender: TObject);
begin
  if Assigned(FClient) then
  begin
    if FClient.State = hsError then
    begin
      ShowStatus(FClient.ErrMsg, True);
    end
    else if FClient.State = hsUserSet then
    begin
      ShowStatus('Ready', False);
    end;

    imSecure.Visible := FClient.Encrypted;
    imInsecure.Visible := not FClient.Encrypted;
  end;
end;

end.
