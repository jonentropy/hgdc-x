unit DebugLog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TfrmDebug }

  TfrmDebug = class(TForm)
    Memo1: TMemo;
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  frmDebug: TfrmDebug;

implementation

{$R *.lfm}

end.

