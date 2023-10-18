// ---------------------------------------------------------------------
// File:   copyTestContentEditor.pas
// Author: (c) 2023 by Jens Kallup - paule32
//         all rights reserved.
//
// only free for education, and non-profit !
// ---------------------------------------------------------------------
unit Unit5;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  JvExStdCtrls, JvRichEdit, Vcl.Menus, JvMenus, Vcl.ExtCtrls;

type
  TFrame5 = class(TFrame)
    ScrollBox1: TScrollBox;
    RichEdit1: TJvRichEdit;
    JvPopupMenu4: TJvPopupMenu;
    Undo1: TMenuItem;
    Undo2: TMenuItem;
    Cut1: TMenuItem;
    Copy1: TMenuItem;
    Delete1: TMenuItem;
    Paste1: TMenuItem;
    Paste2: TMenuItem;
    SelectAll1: TMenuItem;
    Splitter1: TSplitter;
    Memo1: TMemo;
    procedure RichEdit1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure RichEdit1KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure RichEdit1KeyPress(Sender: TObject; var Key: Char);
    procedure RichEdit1KeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure SelectAll1Click(Sender: TObject);
    procedure Undo1Click(Sender: TObject);
    procedure Cut1Click(Sender: TObject);
    procedure Paste1Click(Sender: TObject);
    procedure Delete1Click(Sender: TObject);
    procedure Copy1Click(Sender: TObject);
  private
  public
    FTabIndentSpace: Integer;
    FSelectedAll: Boolean;
    FIndex: Integer;
  end;

implementation

{$R *.dfm}

uses Unit2;

// RichEdit1: copy text to clipboard
procedure TFrame5.Copy1Click(Sender: TObject);
begin
  RichEdit1.CopyToClipboard;
end;

// RichEdit1: copy text to clipboard, and cut from editor
procedure TFrame5.Cut1Click(Sender: TObject);
begin
  RichEdit1.CutToClipboard;
end;

// delete a char or selected text from context text editor
procedure TFrame5.Delete1Click(Sender: TObject);
begin
  if (RichEdit1.SelStart > -1) and (RichEdit1.SelLength > 0) then
  RichEdit1.SelText := '' else
  if (RichEdit1.SelStart - 1) >= 0 then
  begin
    RichEdit1.SelStart  := RichEdit1.SelStart - 1;
    RichEdit1.SelLength := 1;
    RichEdit1.SelText   := '';
  end;
end;

// RichEdit1: if content in clipboard, then paste it to the editor
procedure TFrame5.Paste1Click(Sender: TObject);
begin
  if RichEdit1.CanPaste then
  RichEdit1.PasteFromClipboard;
end;

// currently listen for VK_TAB key in content editor
procedure TFrame5.RichEdit1KeyDown(
  Sender: TObject; var
  Key   : Word;
  Shift : TShiftState);
  var
  SelStart    : Integer;
  SelEnd      : Integer;
  SelectedText: String;
  IndentText  : String;
  strList     : TStringList;
  strLength   : Integer;
  c, i, p     : Integer;
  s1, s2, s3  : String;
  textOK      : Boolean;
begin
  // tabulator key pressed
  if key = VK_TAB then
  begin
    // check, if selected text available
    if RichEdit1.SelLength > 0 then
    begin
      // save the current selection
      SelStart := RichEdit1.SelStart;
      SelEnd   := RichEdit1.SelLength;

      // get the selected get
      SelectedText := RichEdit1.SelText;

      // create a copy of selected text
      strList := TStringList.Create;
      try
        strList.Text := SelectedText;
        SelectedText := strList.Text;

        IndentText := '';

        // ctrl + tab => reverse tab:
        if ssShift in Shift then
        begin
          // check for underflow
          if (SelStart - FTabIndentSpace) < FTabIndentSpace then
          raise Exception.Create('Text Length Underflow.');

          // the first line
          RichEdit1.SelStart  := SelStart - 4;
          RichEdit1.SelLength := 4;
          RichEdit1.SelText   := '';

          // length of first string - we use it later
          p := Length(RichEdit1.Lines.Strings[0]);

          // iterate through all rest lines
          for i := 1 to RichEdit1.Lines.Count - 1 do
          begin
            s1 := RichEdit1.Lines.Strings[i];
            textOK := false;

            // loop tab char number, look, if the n-chars
            // before are whitespace #32, else: nok
            for c := 1 to FTabIndentSpace do
            begin
              if s1[c] = #32 then
              begin
                textOK := true;
                continue;
              end else
              begin
                textOK := false;
                break;
              end;
            end;

            // there are n-chars at left border, then indent back
            if textOK then
            begin
              p := p + Length(s1) - FTabIndentSpace;

              RichEdit1.SelStart := p;
              RichEdit1.SelText := '';
            end;
          end;

          exit;
        end else
        begin
          // add tab space to each text line
          for c := 0 to strList.Count - 1 do
          begin
            IndentText := IndentText
            + StringOfChar(' ', FTabIndentSpace)
            + strList.Strings[c] + #10;
          end;
        end;

        // set the new indented text
        RichEdit1.SelText   := IndentText;

        // set new selection
        RichEdit1.SelStart  := SelStart + FTabIndentSpace;
        RichEdit1.SelLength := Length(IndentText);
      finally
        strList.Clear;
        strList.Free;
      end;
    end;
  end;
end;

// this is an other magic voodoo programming event handler -> why
// must the developer provide a handler, that blocks keyboard input ?
procedure TFrame5.RichEdit1KeyPress(
  Sender: TObject; var
  Key: Char);
begin
  // tell the os, we catch the key observer
  if key = #9 then key := #0;
end;

// i don't know why delphi need 3 event handlers for doing some
// things in the same scope ?? magic voodoo programming ?
procedure TFrame5.RichEdit1KeyUp(
  Sender: TObject; var
  Key   : Word;
  Shift : TShiftState);
begin
  // call undo text
  if (key = Ord('Z')) and (Shift = [ssCtrl]) then
  begin
    RichEdit1.Undo;
  end else

  // open file for content
  if (key = Ord('O')) and (Shift = [ssCtrl]) then
  begin
    Form2.ToolButton16Click(Sender);
  end;
end;

// when the user click into a position, check the text format,
// and update the ui:
procedure TFrame5.RichEdit1MouseDown(
  Sender: TObject;
  Button: TMouseButton;
  Shift : TShiftState;
  X, Y  : Integer);
  var
  APoint: TPoint;
begin
  if Button = mbRight then
  begin
    FIndex := RichEdit1.SelStart;
    exit;
  end;

  RichEdit1.SelLength := 1;

  // font style: bold
  if fsBold in RichEdit1.SelAttributes.Style then
  Form2.ToolButton1.Down := true else
  Form2.ToolButton1.Down := false;

  // font style: italic
  if fsItalic in RichEdit1.SelAttributes.Style then
  Form2.ToolButton2.Down := true else
  Form2.ToolButton2.Down := false;

  // font style: underline
  if fsUnderline in RichEdit1.SelAttributes.Style then
  Form2.ToolButton3.Down := true else
  Form2.ToolButton3.Down := false;

  // font style: strike-out
  if fsStrikeOut in RichEdit1.SelAttributes.Style then
  Form2.ToolButton4.Down := true else
  Form2.ToolButton4.Down := false;

  // font style: name
  Form2.JvFontComboBox2.Text := RichEdit1.SelAttributes.Name;

  // font style: size
  Form2.ComboBox1.Text := IntToStr(RichEdit1.SelAttributes.Height - 8);

  // font style: foreground color/ background color
  Form2.JvOfficeColorButton2.SelectedColor := RichEdit1.SelAttributes.Color;
  Form2.JvOfficeColorButton1.SelectedColor := RichEdit1.SelAttributes.BackColor;
end;

// RichEdit: select all text
procedure TFrame5.SelectAll1Click(Sender: TObject);
begin
  if not FSelectedAll then
  begin
    RichEdit1.SelectAll;
    FSelectedAll := true;
  end else
  begin
    RichEdit1.SelStart  := FIndex;
    RichEdit1.SelLength := 0;
    FSelectedAll := false;
  end;
end;

// undo last RichEdit editor operation
procedure TFrame5.Undo1Click(Sender: TObject);
var
  key  : Word;
  Shift: TShiftState;
begin
  key := Ord('Z');
  shift := [ssCtrl];
  RichEdit1KeyUp(Sender,key,shift);
end;

end.
