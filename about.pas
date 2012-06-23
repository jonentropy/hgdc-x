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
// about.pas - About box

unit About;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Buttons, LCLIntf, LCLType;

type

  { TfrmAbout }

  TfrmAbout = class(TForm)
    btnClose: TBitBtn;
    imCatchThatMouse: TImage;
    imCow: TImage;
    imSynapseCon: TImage;
    imHgdcX: TImage;
    imSynapseDis: TImage;
    lblCow: TLabel;
    lblSynapse: TLabel;
    lblhgdcx: TLabel;
    ScrollBox1: TScrollBox;
    stAbout: TStaticText;
    procedure btnCloseClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure imCatchThatMouseMouseEnter(Sender: TObject);
    procedure imCatchThatMouseMouseLeave(Sender: TObject);
    procedure imHgdcXClick(Sender: TObject);
    procedure lblCowClick(Sender: TObject);
    procedure lblCowMouseEnter(Sender: TObject);
    procedure lblCowMouseLeave(Sender: TObject);
    procedure lblSynapseClick(Sender: TObject);
    procedure lblhgdcxClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  frmAbout: TfrmAbout;

implementation

{$R *.lfm}

{ TfrmAbout }

procedure TfrmAbout.btnCloseClick(Sender: TObject);
begin
  Close();
end;

procedure TfrmAbout.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    Close();
end;

procedure TfrmAbout.imCatchThatMouseMouseEnter(Sender: TObject);
begin
  imSynapseCon.Visible := True;
  imSynapseDis.Visible := False;
end;

procedure TfrmAbout.imCatchThatMouseMouseLeave(Sender: TObject);
begin
  imSynapseCon.Visible := False;
  imSynapseDis.Visible := True;
end;

procedure TfrmAbout.imHgdcXClick(Sender: TObject);
begin
  Screen.Cursor := crHourglass;
  OpenURL('http://theartofindigo.com/');
  Sleep(300);
  Screen.Cursor := crDefault;
end;

procedure TfrmAbout.lblCowClick(Sender: TObject);
begin
  Screen.Cursor := crHourglass;
  OpenURL('http://www.fatcow.com/free-icons');
  Sleep(300);
  Screen.Cursor := crDefault;
end;

procedure TfrmAbout.lblCowMouseEnter(Sender: TObject);
begin
  TLabel(Sender).Font.Style := [fsUnderline];
  TLabel(Sender).Font.Color := clBlue;
  Screen.Cursor := crHandPoint;
end;

procedure TfrmAbout.lblCowMouseLeave(Sender: TObject);
begin
  TLabel(Sender).Font.Style := [];
  TLabel(Sender).Font.Color := clDefault;
  Screen.Cursor := crDefault;
end;

procedure TfrmAbout.lblSynapseClick(Sender: TObject);
begin
  Screen.Cursor := crHourglass;
  OpenURL('http://www.ararat.cz/synapse/doku.php/start');
  Sleep(300);
  Screen.Cursor := crDefault;
end;

procedure TfrmAbout.lblhgdcxClick(Sender: TObject);
begin
  Screen.Cursor := crHourglass;
  OpenURL('http://hgdcx.canthack.org');
  Sleep(300);
  Screen.Cursor := crDefault;
end;

end.