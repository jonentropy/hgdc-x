//
// Copyright 2011 Tristan Linnell
//    tris@canthack.org
//
// main.pas - HGD Client GUI

unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  HGDClient;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    btnApply: TButton;
    chkSSL: TCheckBox;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    gbHGDServer: TGroupBox;
    Label1: TLabel;
    lblHost: TLabel;
    lblPort: TLabel;
    lblUser: TLabel;
    lblPassword: TLabel;
    Memo1: TMemo;
    procedure btnApplyClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
    FClient: THGDClient;

  public
    { public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.btnApplyClick(Sender: TObject);
begin
  FClient.HostAddress := Edit1.Text;
  FClient.HostPort := Edit2.Text;
  FClient.UserName := Edit3.Text;
  FClient.Password := Edit4.Text;

  FClient.ApplyChanges();
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
  FClient := THGDClient.Create(Edit1.Text, Edit2.Text, Edit3.Text, Edit4.Text, chkSSL.Checked, Memo1);
end;

end.
