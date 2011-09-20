unit settings; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Buttons;

type

  { TfrmSettings }

  TfrmSettings = class(TForm)
    btnLastFMApply: TBitBtn;
    chkScrobbling: TCheckBox;
    chkScrobbling1: TCheckBox;
    edtLastFMUser: TLabeledEdit;
    GroupBox1: TGroupBox;
    imLastFM: TImage;
    lblLastFM: TLabel;
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  frmSettings: TfrmSettings;

implementation

{$R *.lfm}

end.

