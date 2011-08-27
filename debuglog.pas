unit DebugLog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TfrmDebug }

  TfrmDebug = class(TForm)
    Memo1: TMemo;
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

end.

