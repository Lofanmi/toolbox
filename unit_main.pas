unit unit_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls,
  PopupNotifier, ExtCtrls, ActnList, MaskEdit, Grids, ColorBox, ValEdit, Types;

type

  { TFormMain }

  TFormMain = class(TForm)
    ButtonHTMLDecode: TButton;
    ButtonHTMLEncode: TButton;
    ButtonURLDecode: TButton;
    ButtonBase64Encode: TButton;
    ButtonBase64Decode: TButton;
    ButtonBase64EncodeURL: TButton;
    ButtonBase64DecodeURL: TButton;
    ButtonUnicodeDecode: TButton;
    ButtonURLEncode: TButton;
    ButtonUnicodeEncode: TButton;
    CheckBoxHashLowerUpperCase: TCheckBox;
    EditHashOutputFNV64: TEdit;
    EditHashOutputFNV128: TEdit;
    EditHashOutputSHA512: TEdit;
    EditHashOutputSHA256: TEdit;
    EditHashOutputSHA1: TEdit;
    EditHashOutputCRC32: TEdit;
    EditHashOutputMD5: TEdit;
    EditHashOutputFNV32: TEdit;
    LabelCoderOutput: TLabel;
    LabelFNV64: TLabel;
    LabelFNV128: TLabel;
    LabelHashInput: TLabel;
    LabelCoderInput: TLabel;
    LabelHashOutput: TLabel;
    LabelSHA512: TLabel;
    LabelSHA256: TLabel;
    LabelSHA1: TLabel;
    LabelCRC32: TLabel;
    LabelMD5: TLabel;
    LabelFNV32: TLabel;
    MemoCoderOutput: TMemo;
    MemoHashInput: TMemo;
    MemoCoderInput: TMemo;
    PageControlStringHelpers: TPageControl;
    PageControlMain: TPageControl;
    PanelCoderInput: TPanel;
    PanelCoderOutput: TPanel;
    PanelHashOptions: TPanel;
    PanelHashLabelFNV64: TPanel;
    PanelHashLabelFNV128: TPanel;
    PanelHashLabelSHA512: TPanel;
    PanelHash: TPanel;
    PanelHashInput: TPanel;
    PanelHashLabelSHA256: TPanel;
    PanelHashLabelSHA1: TPanel;
    PanelHashLabelCRC32: TPanel;
    PanelHashLabelMD5: TPanel;
    PanelHashLabelFNV32: TPanel;
    PanelCoderButtonGroup: TPanel;
    PanelHashOutput: TPanel;
    PanelHashOutputFNV64: TPanel;
    PanelHashOutputFNV128: TPanel;
    PanelHashOutputSHA512: TPanel;
    PanelHashOutputSHA256: TPanel;
    PanelHashOutputSHA1: TPanel;
    PanelHashOutputCRC32: TPanel;
    PanelHashOutputMD5: TPanel;
    PanelHashOutputFNV32: TPanel;
    StatusBarName: TStatusBar;
    TabHash: TTabSheet;
    TabCoder: TTabSheet;
    TabJSON: TTabSheet;
    TabBaseConv: TTabSheet;
    TabStringHelpers: TTabSheet;
    TabTimeHelpers: TTabSheet;
    TabShortcuts: TTabSheet;
    TabSpider: TTabSheet;
    TabBatches: TTabSheet;
    procedure ButtonBase64DecodeClick(Sender: TObject);
    procedure ButtonBase64DecodeURLClick(Sender: TObject);
    procedure ButtonBase64EncodeClick(Sender: TObject);
    procedure ButtonBase64EncodeURLClick(Sender: TObject);
    procedure ButtonHTMLDecodeClick(Sender: TObject);
    procedure ButtonHTMLEncodeClick(Sender: TObject);
    procedure ButtonUnicodeDecodeClick(Sender: TObject);
    procedure ButtonUnicodeEncodeClick(Sender: TObject);
    procedure ButtonURLDecodeClick(Sender: TObject);
    procedure ButtonURLEncodeClick(Sender: TObject);
    procedure CheckBoxHashLowerUpperCaseChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure MemoHashInputChange(Sender: TObject);
  private

  public

  end;

var
  FormMain: TFormMain;

implementation

{$R *.lfm}

{ TFormMain }

procedure TFormMain.MemoHashInputChange(Sender: TObject);
begin

end;

procedure TFormMain.CheckBoxHashLowerUpperCaseChange(Sender: TObject);
begin

end;

procedure TFormMain.FormCreate(Sender: TObject);
begin

end;

procedure TFormMain.ButtonBase64EncodeClick(Sender: TObject);
begin

end;

procedure TFormMain.ButtonBase64EncodeURLClick(Sender: TObject);
begin

end;

procedure TFormMain.ButtonHTMLDecodeClick(Sender: TObject);
begin

end;

procedure TFormMain.ButtonHTMLEncodeClick(Sender: TObject);
begin

end;

procedure TFormMain.ButtonUnicodeDecodeClick(Sender: TObject);
begin

end;

procedure TFormMain.ButtonUnicodeEncodeClick(Sender: TObject);
begin

end;

procedure TFormMain.ButtonURLDecodeClick(Sender: TObject);
begin

end;

procedure TFormMain.ButtonURLEncodeClick(Sender: TObject);
begin

end;

procedure TFormMain.ButtonBase64DecodeClick(Sender: TObject);
begin

end;

procedure TFormMain.ButtonBase64DecodeURLClick(Sender: TObject);
begin

end;

end.

