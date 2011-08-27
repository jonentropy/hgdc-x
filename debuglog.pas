unit DebugLog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons;

type

  { TfrmDebug }

  TfrmDebug = class(TForm)
    BitBtn1: TBitBtn;
    btmClose: TBitBtn;
    Memo1: TMemo;
    procedure BitBtn1Click(Sender: TObject);
    procedure btmCloseClick(Sender: TObject);
    procedure Memo1Change(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  frmDebug: TfrmDebug;

implementation

{$R *.lfm}

{ TfrmDebug }

procedure TfrmDebug.Memo1Change(Sender: TObject);
begin
  while Memo1.Lines.Count > 1000 do
    Memo1.Lines.Delete(0);
end;

procedure TfrmDebug.BitBtn1Click(Sender: TObject);
begin
  Memo1.Clear();
end;

procedure TfrmDebug.btmCloseClick(Sender: TObject);
begin
  Close();
end;

end.

