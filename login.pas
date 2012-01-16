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
// login.pas - GUI for logging ino the hgd server

unit Login;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Buttons, LCLType, XMLPropStorage;

type

  { TfrmLogin }

  TfrmLogin = class(TForm)
    btnHGDLogin: TBitBtn;
    btnLastFMApply: TBitBtn;
    chkScrobbling: TCheckBox;
    chkScrobbling1: TCheckBox;
    chkSSL: TCheckBox;
    edtHost: TEdit;
    edtLastFMUser: TLabeledEdit;
    edtPort: TEdit;
    edtPwd: TEdit;
    edtUser: TEdit;
    gbHGDServer: TGroupBox;
    gbLastFM: TGroupBox;
    imLastFM: TImage;
    imPassword: TImage;
    imPort: TImage;
    imServer: TImage;
    imUser: TImage;
    lblHost: TLabel;
    lblLastFM: TLabel;
    lblPassword: TLabel;
    lblPort: TLabel;
    lblUser: TLabel;
    XMLPropStorage1: TXMLPropStorage;
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  frmLogin: TfrmLogin;

implementation

{$R *.lfm}

{ TfrmLogin }

procedure TfrmLogin.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    Close();
end;

procedure TfrmLogin.FormShow(Sender: TObject);
begin
  if edtUser.Text = '' then
  begin
    edtUser.SetFocus();
  end
  else
  begin
    edtPwd.SetFocus();
    edtPwd.SelectAll();
  end;
end;

end.

