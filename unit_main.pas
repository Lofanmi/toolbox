unit unit_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls,
  PopupNotifier, ExtCtrls, ActnList, MaskEdit, Grids, ColorBox, ValEdit, Types;

type

  { TFormMain }

  TFormMain = class(TForm)
    ButtonJSONFormat: TButton;
    ButtonHTMLDecode: TButton;
    ButtonHTMLEncode: TButton;
    ButtonJSONCompact: TButton;
    ButtonURLDecode: TButton;
    ButtonBase64Encode: TButton;
    ButtonBase64Decode: TButton;
    ButtonURLBase64Encode: TButton;
    ButtonURLBase64Decode: TButton;
    ButtonUnicodeDecode: TButton;
    ButtonURLEncode: TButton;
    ButtonUnicodeEncode: TButton;
    ButtonJSONClear: TButton;
    CheckBoxHashLowerUpperCase: TCheckBox;
    EditHashOutputFNV64: TEdit;
    EditHashOutputFNV128: TEdit;
    EditHashOutputSHA512: TEdit;
    EditHashOutputSHA256: TEdit;
    EditHashOutputSHA1: TEdit;
    EditHashOutputCRC32: TEdit;
    EditHashOutputMD5: TEdit;
    EditHashOutputFNV32: TEdit;
    LabelJSONText: TLabel;
    LabelCoderOutput: TLabel;
    LabelJSONView: TLabel;
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
    MemoJSONText: TMemo;
    MemoCoderOutput: TMemo;
    MemoHashInput: TMemo;
    MemoCoderInput: TMemo;
    PageControlStringHelpers: TPageControl;
    PageControlMain: TPageControl;
    PanelCoderButtonGroup1: TPanel;
    PanelCoderInput: TPanel;
    PanelJSONText: TPanel;
    PanelCoderOutput: TPanel;
    PanelCoderOutput1: TPanel;
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
    TreeViewJSONView: TTreeView;
    procedure ButtonBase64DecodeClick(Sender: TObject);
    procedure ButtonJSONClearClick(Sender: TObject);
    procedure ButtonJSONCompactClick(Sender: TObject);
    procedure ButtonJSONFormatClick(Sender: TObject);
    procedure ButtonURLBase64DecodeClick(Sender: TObject);
    procedure ButtonBase64EncodeClick(Sender: TObject);
    procedure ButtonURLBase64EncodeClick(Sender: TObject);
    procedure ButtonHTMLDecodeClick(Sender: TObject);
    procedure ButtonHTMLEncodeClick(Sender: TObject);
    procedure ButtonUnicodeDecodeClick(Sender: TObject);
    procedure ButtonUnicodeEncodeClick(Sender: TObject);
    procedure ButtonURLDecodeClick(Sender: TObject);
    procedure ButtonURLEncodeClick(Sender: TObject);
    procedure CheckBoxHashLowerUpperCaseChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure MemoHashInputChange(Sender: TObject);
    procedure MemoJSONTextChange(Sender: TObject);
    procedure PanelCoderInputClick(Sender: TObject);
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

procedure TFormMain.MemoJSONTextChange(Sender: TObject);
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

procedure TFormMain.ButtonURLBase64EncodeClick(Sender: TObject);
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

procedure TFormMain.ButtonJSONClearClick(Sender: TObject);
begin

end;

procedure TFormMain.ButtonJSONFormatClick(Sender: TObject);
begin

end;

procedure TFormMain.ButtonJSONCompactClick(Sender: TObject);
begin

end;

procedure TFormMain.ButtonURLBase64DecodeClick(Sender: TObject);
begin

end;

end.

