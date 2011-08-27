//
// Copyright 2011 Tristan Linnell
//    tris@canthack.org
//
// main.pas - HGD Client GUI

unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, XMLPropStorage, Buttons, Grids, ComCtrls, HGDClient, DebugLog;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    btnSubmit: TBitBtn;
    btnLastFMApply: TBitBtn;
    btnHGDApply: TButton;
    chkScrobbling: TCheckBox;
    chkScrobbling1: TCheckBox;
    chkSSL: TCheckBox;
    edtHost: TEdit;
    edtPort: TEdit;
    edtUser: TEdit;
    edtPwd: TEdit;
    gbHGDServer: TGroupBox;
    GroupBox1: TGroupBox;
    imSecure: TImage;
    Image2: TImage;
    edtLastFMUser: TLabeledEdit;
    imDebug: TImage;
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

  FClient.ApplyChanges();
  tmrPlaylist.Enabled := True;
end;

procedure TfrmMain.btnSubmitClick(Sender: TObject);
begin
  TBitBtn(Sender).Enabled := False;

  if OpenDialog1.Execute() then
  begin
    pbarUpload.Visible := True;
    FClient.QueueSong(OpenDialog1.FileName);
  end;

  pbarUpload.Visible := False;
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
    sgPlaylist.Cells[3, sgPlaylist.RowCount -1] := PL[i].User;
  end;
end;

procedure TfrmMain.ShowStatus(Msg: string; Error: boolean);
begin
  if Error then
    lblError.Font.Color := clRed
  else
    lblError.Font.Color := clBlue;

  lblError.Caption := Msg;
end;

procedure TfrmMain.tmrStateTimer(Sender: TObject);
begin
  if FClient.State = hsError then
  begin
    ShowStatus(FClient.ErrMsg, True);
  end
  else if FClient.State = hsUserSet then
  begin
    ShowStatus('Ready', False);
  end;
end;

end.
