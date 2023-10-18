// ---------------------------------------------------------------------
// File:   copyTestPrintPreview.pas
// Author: (c) 2023 by Jens Kallup - paule32
//         all rights reserved.
//
// only free for education, and non-profit !
// ---------------------------------------------------------------------
unit Unit4;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, JvExControls, JvPrvwDoc,
  Vcl.Menus, JvMenus;

type
  TPrintPreviewerFrame = class(TFrame)
    PrintPreview1: TJvPreviewControl;
    JvPopupMenu3: TJvPopupMenu;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    Print2: TMenuItem;
    procedure MenuItem4Click(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

implementation

{$R *.dfm}

uses Unit2;

procedure TPrintPreviewerFrame.MenuItem4Click(Sender: TObject);
begin
  Hide;
  Form2.FEditorFrame.RichEdit1.Show;
  Form2.FEditorFrame.RichEdit1.Enabled := true;
  Form2.FEditorFrame.RichEdit1.SetFocus;
end;

end.
