// ---------------------------------------------------------------------
// File:   copyTest.pas
// Author: (c) 2023 by Jens Kallup - paule32
//         all rights reserved.
//
// only free for education, and non-profit !
// ---------------------------------------------------------------------
unit Unit2;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes, System.ImageList,
  System.Generics.Collections,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.ComCtrls, Vcl.ToolWin, Vcl.ImgList, Vcl.Menus,  Vcl.ExtCtrls,
  TLHelp32, Clipbrd,
  JvFullColorSpaces, JvFullColorCtrls, JvExStdCtrls, JvCombobox, JvMenus,
  JvColorCombo, JvExComCtrls, JvToolBar, JvExExtCtrls, JvExtComponent,
  JvOfficeColorButton, JvGradientCaption, JvRichEdit;

type
  HWNDArray = array of THandle;

  PEnumData = ^TEnumData;
  TEnumData = record
    ProcessID: DWORD;
    Handles: HWNDArray;
    WindowHandle: HWND;
    WindowText: string;
  end;

type
  // the original menu items are very tiny on big displays
  // so, i decide to create a sub class, to change this...
  TMyMenuPainter = class(TJvXPMenuItemPainter)
  protected
    // set the width, height
    procedure Measure(
      Item      : TMenuItem;
      var Width : Integer;
      var Height: Integer);

    // paint the item's
    procedure Paint(
      AItem : TMenuItem;
      ARect : TRect;
      AState: TMenuOwnerDrawState);
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy;
  end;

type
  // this is the main application form
  TForm2 = class(TForm)
    StatusBar1: TStatusBar;
    ImageList1: TImageList;
    PopupMenu1: TPopupMenu;
    Consolas1: TMenuItem;
    Consolas2: TMenuItem;
    imeNewRoman1: TMenuItem;
    imeNewRoman2: TMenuItem;
    JvToolBar1: TJvToolBar;
    ToolButton5: TToolButton;
    JvPopupMenu1: TJvPopupMenu;
    Open1: TMenuItem;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    Panel4: TPanel;
    N1: TMenuItem;
    Exit1: TMenuItem;
    ScrollBox1: TScrollBox;
    Splitter1: TSplitter;
    JvToolBar2: TJvToolBar;
    ToolButton9: TToolButton;
    ToolButton10: TToolButton;
    Panel5: TPanel;
    TreeView1: TTreeView;
    Panel7: TPanel;
    Panel6: TPanel;
    Button1: TButton;
    Button2: TButton;
    Panel8: TPanel;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    Panel2: TPanel;
    JvFontComboBox2: TJvFontComboBox;
    Panel1: TPanel;
    Label2: TLabel;
    Label3: TLabel;
    JvOfficeColorButton2: TJvOfficeColorButton;
    JvOfficeColorButton1: TJvOfficeColorButton;
    JvToolBar3: TJvToolBar;
    ToolButton11: TToolButton;
    ToolButton12: TToolButton;
    ToolButton13: TToolButton;
    ToolButton14: TToolButton;
    ImageList2: TImageList;
    ToolButton15: TToolButton;
    ToolButton16: TToolButton;
    Panel3: TPanel;
    ComboBox1: TComboBox;
    Panel9: TPanel;
    Label1: TLabel;
    Label4: TLabel;
    JvOfficeColorButton3: TJvOfficeColorButton;
    JvOfficeColorButton4: TJvOfficeColorButton;
    ToolButton17: TToolButton;
    PageScroller1: TPageScroller;
    JvGradientCaption1: TJvGradientCaption;
    Splitter2: TSplitter;
    Panel10: TPanel;
    RichEdit1: TJvRichEdit;
    procedure Button1Click(Sender: TObject);
    procedure ToolButton5Click(Sender: TObject);
    procedure JvPopupMenu1MeasureItem(Sender: TMenu; Item: TMenuItem; var Width,
      Height: Integer);
    procedure Exit1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure JvFontComboBox2Change(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure ToolButton1Click(Sender: TObject);
    procedure RichEdit1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Label2Click(Sender: TObject);
    procedure Label3Click(Sender: TObject);
    procedure JvOfficeColorButton2ColorChange(Sender: TObject);
    procedure JvOfficeColorButton1ColorChange(Sender: TObject);
    procedure Label4Click(Sender: TObject);
    procedure Label1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    FCounter: Integer;
    FMenuPainter: TMyMenuPainter;
  public
    function ArrayToString(const a: array of Char): string;

    function EnumWindowsCallback(hwnd: HWND; lParam: LPARAM): BOOL; stdcall;
    function FindChildWindowHandles(processID: DWORD): HWNDArray;

    function GetWindowCaption(hWnd: HWND): string;
    function GetForegroundWindowFromPoint(x, y: Integer): HWND;
    function GetRichViewText(hWnd: HWND): string;
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

// the constructor for class TMyMenuPainter:
constructor TMyMenuPainter.Create(AOwner: TComponent);
begin
  inherited Create(Aowner);
end;

// the destructor for TMyMenuPainter:
destructor TMyMenuPainter.Destroy;
begin
  inherited Destroy;
end;

// set the width, and height of the menu items
procedure TMyMenuPainter.Measure(
  Item      : TMenuItem;
  var Width : Integer;
  var Height: Integer);
begin
  Width := 120;
  Height := 32;
end;

// override member, to flag the items measure
procedure TMyMenuPainter.Paint(
  AItem : TMenuItem;
  ARect : TRect;
  AState: TMenuOwnerDrawState);
begin
  ARect.width := 120;
  ARect.Height := 21;
  inherited Paint(AItem,ARect,AState);
end;

// convert a dynamic "Array of Char" to "String"
function TForm2.ArrayToString(const a: array of Char): string;
begin
  if Length(a)>0 then
  SetString(Result, PChar(@a[0]), Length(a)) else
  Result := '';
end;

// simply, copy the text from an edit component to a text buffer
function TForm2.GetRichViewText(hWnd: HWND): string;
var
  textLength: Integer;
  text: String;
begin
  // create buffer for the text:
  textLength := SendMessage(hWnd, WM_GETTEXTLENGTH, 0, 0);
  SetLength(text, textLength + 1);

  // get text from window hwnd:
  SendMessage(hWnd, WM_GETTEXT, textLength + 1, LPARAM(PChar(text)));
  Result := String(text);
end;

// get the caption of the window, and return
// it as string:
function TForm2.GetWindowCaption(hWnd: HWND): string;
var
  len: Integer;
begin
  len := GetWindowTextLength(hWnd);
  if len > 0 then
  begin
    SetLength(Result, len);
    GetWindowText(hWnd, PChar(Result), len + 1);
  end else
  Result := '';
end;

procedure TForm2.JvFontComboBox2Change(Sender: TObject);
begin
  RichEdit1.SelAttributes.Height := System.SysUtils.StrToInt(ComboBox1.Text) + 8;
  RichEdit1.SelAttributes.Name   := JvFontComboBox2.Text;
end;

// text format: background color
procedure TForm2.JvOfficeColorButton1ColorChange(Sender: TObject);
begin
  RichEdit1.SelAttributes.BackColor := JvOfficeColorButton1.SelectedColor;
end;

// text format: forground color
procedure TForm2.JvOfficeColorButton2ColorChange(Sender: TObject);
begin
  RichEdit1.SelAttributes.Color := JvOfficeColorButton2.SelectedColor;
end;

procedure TForm2.JvPopupMenu1MeasureItem(Sender: TMenu; Item: TMenuItem;
  var Width, Height: Integer);
begin
  Width := 150;
  Height := 41;
end;

// open color box, when the user clicj forground color lable
procedure TForm2.Label1Click(Sender: TObject);
begin
  SendMessage(JvOfficeColorButton3.Handle, WM_LBUTTONDOWN,
  2, JvOfficeColorButton3.Width - 2);
  Sleep(50);
  SendMessage(JvOfficeColorButton3.Handle, WM_LBUTTONUP,
  2, JvOfficeColorButton3.Width - 2);
end;

procedure TForm2.Label2Click(Sender: TObject);
begin
  SendMessage(JvOfficeColorButton2.Handle, WM_LBUTTONDOWN,
  2, JvOfficeColorButton2.Width - 2);
  Sleep(50);
  SendMessage(JvOfficeColorButton2.Handle, WM_LBUTTONUP,
  2, JvOfficeColorButton2.Width - 2);
end;

// open color box, when the user clicj background color lable
procedure TForm2.Label3Click(Sender: TObject);
begin
  SendMessage(JvOfficeColorButton1.Handle, WM_LBUTTONDOWN,
  2, JvOfficeColorButton1.Width - 2);
  Sleep(50);
  SendMessage(JvOfficeColorButton1.Handle, WM_LBUTTONUP,
  2, JvOfficeColorButton1.Width - 2);
end;

procedure TForm2.Label4Click(Sender: TObject);
begin
  SendMessage(JvOfficeColorButton4.Handle, WM_LBUTTONDOWN,
  2, JvOfficeColorButton4.Width - 2);
  Sleep(50);
  SendMessage(JvOfficeColorButton4.Handle, WM_LBUTTONUP,
  2, JvOfficeColorButton4.Width - 2);
end;

// when the user click into a position, check the text format,
// and update the ui:
procedure TForm2.RichEdit1MouseDown(
  Sender: TObject;
  Button: TMouseButton;
  Shift : TShiftState;
  X, Y  : Integer);
begin
  RichEdit1.SelLength := 1;

  // font style: bold
  if fsBold in RichEdit1.SelAttributes.Style then
  ToolButton1.Down := true else
  ToolButton1.Down := false;

  // font style: italic
  if fsItalic in RichEdit1.SelAttributes.Style then
  ToolButton2.Down := true else
  ToolButton2.Down := false;

  // font style: underline
  if fsUnderline in RichEdit1.SelAttributes.Style then
  ToolButton3.Down := true else
  ToolButton3.Down := false;

  // font style: strike-out
  if fsStrikeOut in RichEdit1.SelAttributes.Style then
  ToolButton4.Down := true else
  ToolButton4.Down := false;

  // font style: name
  JvFontComboBox2.Text := RichEdit1.SelAttributes.Name;

  // font style: size
  ComboBox1.Text := IntToStr(RichEdit1.SelAttributes.Height - 8);

  // font style: foreground color/ background color
  JvOfficeColorButton2.SelectedColor := RichEdit1.SelAttributes.Color;
  JvOfficeColorButton1.SelectedColor := RichEdit1.SelAttributes.BackColor;
end;

// toolbutton: bold
procedure TForm2.ToolButton1Click(Sender: TObject);
begin
  if fsBold in RichEdit1.SelAttributes.Style then
  RichEdit1.SelAttributes.Style  := RichEdit1.SelAttributes.Style - [fsBold] else
  RichEdit1.SelAttributes.Style  := RichEdit1.SelAttributes.Style + [fsBold];

  ToolButton1.Down := not ToolButton1.Down;
end;

procedure TForm2.ToolButton5Click(Sender: TObject);
begin
  JvPopupMenu1.Popup(Mouse.CursorPos.X,Mouse.CursorPos.Y);
end;

// get the window from x,y coordinate
function TForm2.GetForegroundWindowFromPoint(x, y: Integer): HWND;
var
  point: TPoint;
begin
  // set relative coordinates
  point.x := x;
  point.y := y;

  // try, to get the "point"ed window hwnd, based on the
  // foreground window
  Result := WindowFromPoint(point);
end;

// window callback, that iterate through the processes windows,
// and fill TEnumData with Informations:
function TForm2.EnumWindowsCallback(hwnd: HWND; lParam: LPARAM): BOOL; stdcall;
var
  data: PEnumData;
  windowProcessID: DWORD;
begin
  data := PEnumData(lParam);
  GetWindowThreadProcessId(hwnd, @windowProcessID);
  if windowProcessId = data.ProcessID then
  begin
    SetLength(data.Handles, Length(data.Handles) + 1);
    data.Handles[ High(data.Handles) ] := hwnd;
  end;
  Result := True;
end;

// find the window hwnd's, starting at processID:
function TForm2.FindChildWindowHandles(processID: DWORD): HWNDArray;
var
  data: TEnumData;
begin
  inc(FCounter);

  data.ProcessID := processID;
  SetLength(data.Handles, 0);

  EnumWindows(@TForm2.EnumWindowsCallback, LPARAM(@data));
  Result := data.Handles;
end;

// this procedure does initialize some stuff, used later on form...
procedure TForm2.FormCreate(Sender: TObject);
begin
  FMenuPainter := TMyMenuPainter.Create(nil);
  JvPopupMenu1.ItemPainter := FMenuPainter;

  JvGradientCaption1.Active := true;
end;

procedure TForm2.FormDestroy(Sender: TObject);
begin
  FMenuPainter.Free;
  FMenuPainter := nil;
end;

procedure TForm2.FormShow(Sender: TObject);
begin
  ShowWindow(Handle,SW_MINIMIZE);
  ShowWindow(Handle,SW_MAXIMIZE);
end;

procedure TForm2.Button1Click(Sender: TObject);
var
  hSnap    : THandle;
  hProc    : THandle;
  procWin  : THandle;
  priClass : DWORD;
  procEntry: TProcessEntry32;
  childWindowHandles: HWNDArray;
  i : Integer;
  s1, s2, s3: string;
  found: Boolean;
  Input: TInput;
  InputList: TList<TInput>;
begin
  RichEdit1.Lines.Clear;
  FCounter := -1;

  // make a snapshot of the current system
  found := false;
  hSnap := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  if hSnap = INVALID_HANDLE_VALUE then
  begin
    ShowMessage('Error: CreateToolhelp32Snapshot');
    exit;
  end;

  // clean init the ProcEntry structure:
  ProcEntry.dwSize := sizeOf(ProcessEntry32);

  // if the processID > 0 ...
  if (Process32First(hSnap, ProcEntry)) then
  begin
    // ... then get the running processes
    while Process32Next(hSnap, ProcEntry) do
    begin
      // if HelpNDoc.exe (hnd8.exe) is found, then
      // internal switch to the process
      s1 := ProcEntry.szExeFile;
      if ExtractFileName(s1) = 'hnd8.exe' then
      begin
        priClass := 0;
        hProc := OpenProcess(
        PROCESS_ALL_ACCESS, FALSE,
        ProcEntry.th32ProcessID);

        // get some internal informations about the process:
        priClass := GetPriorityClass( hProc );
        if priClass < 1 then
        ShowMessage('Error: GetPriorityClass');
        CloseHandle(hProc);

        // fill the sub-hwnd container:
        childWindowHandles := FindChildWindowHandles(ProcEntry.th32ProcessID);

        // now, we can iterate the sub-windows:
        for i := 0 to High(childWindowHandles) do
        begin
          s2 := GetWindowCaption(childWindowHandles[i]);
          if System.SysUtils.AnsiPos('- HelpNDoc Personal',s2) = 0 then
          begin
            if System.SysUtils.AnsiPos('- HelpNDoc',s2) > 0 then
            begin
              procWin := childWindowHandles[i];
              s3 := Copy(s2,1,Pos(' -',s2)-1);
              found := true;
              //ShowMessage('>' + s3 + '<');
              break;
            end;
          end;
        end;
      end;
    end;
    if found = true then
    begin
      Winapi.Windows.ShowWindow(procWin,SW_RESTORE);
      Winapi.Windows.SetForegroundWindow(procWin);

      // select all: ctrl+A ...
      InputList := TList<TInput>.Create;
      try
        Input := Default(TInput);
        Input.Itype := INPUT_KEYBOARD;
        Input.ki.wScan := 0;
        Input.ki.time  := 0;
        Input.ki.dwExtraInfo := 0;

        // 1. press ctrl key
        Input.ki.dwFlags := 0;  // 0 for key-press
        Input.ki.wVk     := VK_CONTROL;
        InputList.Add(Input);

        // 2. press "a" key
        Input.ki.dwFlags := 0;  // 0 for key-press
        Input.ki.wVk     := Ord('A');
        InputList.Add(Input);

        // 3. release "a" key
        Input.ki.dwFlags := KEYEVENTF_KEYUP;
        Input.ki.wVk     := Ord('A');
        InputList.Add(Input);

        // 4. release ctrl key
        Input.ki.dwFlags := KEYEVENTF_KEYUP;
        Input.ki.wVk     := VK_CONTROL;
        InputList.Add(Input);

        SendInput(InputList.Count, InputList.List[0], sizeof(TInput));

        //s := GetRichViewText(w2);
        //Memo1.Lines.Add(s);
      finally
        InputList.Free;
      end;

      // copy selected text: ctrl+c
      InputList := TList<TInput>.Create;
      try
        Input := Default(TInput);
        Input.Itype := INPUT_KEYBOARD;
        Input.ki.wScan := 0;
        Input.ki.time  := 0;
        Input.ki.dwExtraInfo := 0;

        // 1. press ctrl key
        Input.ki.dwFlags := 0;  // 0 for key-press
        Input.ki.wVk     := VK_CONTROL;
        InputList.Add(Input);

        // 2. press "c" key
        Input.ki.dwFlags := 0;  // 0 for key-press
        Input.ki.wVk     := Ord('C');
        InputList.Add(Input);

        // 3. release "c" key
        Input.ki.dwFlags := KEYEVENTF_KEYUP;
        Input.ki.wVk     := Ord('C');
        InputList.Add(Input);

        // 4. release ctrl key
        Input.ki.dwFlags := KEYEVENTF_KEYUP;
        Input.ki.wVk     := VK_CONTROL;
        InputList.Add(Input);

        // copy text to clipboard, sleep could be adjusted for big text:
        SendInput(InputList.Count, InputList.List[0], sizeof(TInput));
        Sleep(250);

        // this does not work:
        RichEdit1.Perform(WM_SETTEXT, 0, PWChar(Clipboard.AsText));
      finally
        InputList.Free;
      end;

      // at end, switch back to applicatiin
      Winapi.Windows.SetForegroundWindow(self.Handle);
      RichEdit1.PasteFromClipboard;
    end;
  end;
  CloseHandle(hSnap);
end;

procedure TForm2.Button2Click(Sender: TObject);
var
  s: String;
begin
  s := ''
  + '{\rtf1\ansi'
  + '{\trowd'
  + '\clbrdrl\brdrs\brdrw200\clbrdrt\brdrs\brdrw200\clbrdrb\brdrs\brdrw200\clbrdrr\brdrs\brdrw200\cellx4000'
  + 'cell 1\intbl\cell'
  + 'cell 2\intbl\cell'
  + 'cell 3\intbl\cell'
  + '\row'
  + '}}';
(*
  '{\rtf1\ansi\deff0'#13#10+
  '\trowd'#13#10+
  '\cellx2000'#13#10+
  '\cellx2000'#13#10+
  '\cellx3000'#13#10+
  'cell 1\intbl\cell'#13#10+
  'cell 2\intbl\cell'#13#10+
  'cell 3\intbl\cell'#13#10+
  '\row'#13#10+
  '}' ;
*)
  try
    RichEdit1.PlainText := false;
    RichEdit1.Lines.Clear;
    RichEdit1.Lines.Add(s);
  except
    on E: Exception do
    begin
      if E.ClassName = 'EOutOfResources' then
      begin
      end;
    end;
  end;
end;

procedure TForm2.ComboBox1Change(Sender: TObject);
begin
  RichEdit1.SelAttributes.Height := System.SysUtils.StrToInt(ComboBox1.Text) + 8;
  RichEdit1.SelAttributes.Name   := JvFontComboBox2.Text;
end;

procedure TForm2.Exit1Click(Sender: TObject);
begin
  Close;
end;

end.
