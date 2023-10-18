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
  System.StrUtils, System.Generics.Collections,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.ComCtrls, Vcl.ToolWin, Vcl.ImgList, Vcl.Menus,  Vcl.ExtCtrls,
  TLHelp32, Clipbrd, IniFiles,
  Unit4, Unit5,
  JvFullColorSpaces, JvFullColorCtrls, JvExStdCtrls, JvCombobox, JvMenus,
  JvColorCombo, JvExComCtrls, JvToolBar, JvExExtCtrls, JvExtComponent,
  JvOfficeColorButton, JvGradientCaption, JvRichEdit, JvExControls, JvxSlider,
  JvPrvwDoc, JvComponentBase, JvPrvwRender, madExceptVcl, JvArrowButton,
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.UI.Intf,
  FireDAC.Phys.Intf, FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async,
  FireDAC.Phys, FireDAC.Phys.SQLite, FireDAC.Phys.SQLiteDef,
  FireDAC.Stan.ExprFuncs, FireDAC.Phys.SQLiteWrapper.Stat, FireDAC.VCLUI.Wait,
  Data.DB, FireDAC.Comp.Client, FireDAC.Stan.Param, FireDAC.DatS,
  FireDAC.DApt.Intf, FireDAC.DApt, FireDAC.Comp.DataSet;

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
      Item  : TMenuItem;
      var W : Integer;
      var H : Integer); override;

    // paint the item's
    procedure Paint(
      AItem : TMenuItem;
      ARect : TRect;
      AState: TMenuOwnerDrawState); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
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
    JvPopupMenu2: TJvPopupMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    Panel11: TPanel;
    Panel12: TPanel;
    Splitter3: TSplitter;
    TreeView2: TTreeView;
    TreeView3: TTreeView;
    Panel13: TPanel;
    JvxSlider1: TJvxSlider;
    Panel14: TPanel;
    New1: TMenuItem;
    SaveAs1: TMenuItem;
    Save1: TMenuItem;
    N2: TMenuItem;
    Print1: TMenuItem;
    PrinterSetup1: TMenuItem;
    Project1: TMenuItem;
    Project2: TMenuItem;
    AssistentTemplate1: TMenuItem;
    JvPreviewRenderJvRichEdit1: TJvPreviewRenderJvRichEdit;
    OpenDialog1: TOpenDialog;
    MadExceptionHandler1: TMadExceptionHandler;
    Button1: TButton;
    Button2: TButton;
    Label5: TLabel;
    ImageList3: TImageList;
    ToolBar2: TToolBar;
    ToolButton18: TToolButton;
    ToolButton19: TToolButton;
    ToolButton20: TToolButton;
    ToolButton21: TToolButton;
    ToolButton22: TToolButton;
    ToolButton23: TToolButton;
    JvArrowButton1: TJvArrowButton;
    JvPopupMenu3: TJvPopupMenu;
    DeleteFilter1: TMenuItem;
    LocalConnection: TFDConnection;
    Button3: TButton;
    FDQuery1: TFDQuery;
    procedure Button1Click(Sender: TObject);
    procedure ToolButton5Click(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure JvFontComboBox2Change(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure ToolButton1Click(Sender: TObject);
    procedure Label2Click(Sender: TObject);
    procedure Label3Click(Sender: TObject);
    procedure JvOfficeColorButton2ColorChange(Sender: TObject);
    procedure JvOfficeColorButton1ColorChange(Sender: TObject);
    procedure Label4Click(Sender: TObject);
    procedure Label1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Open1MeasureItem(Sender: TObject; ACanvas: TCanvas; var W,
      H: Integer);
    procedure JvPopupMenu1DrawItem(Sender: TMenu; Item: TMenuItem; Rect: TRect;
      State: TMenuOwnerDrawState);
    procedure PrinterSetup1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure JvOfficeColorButton4ColorChange(Sender: TObject);
    procedure ToolButton14Click(Sender: TObject);
    procedure ToolButton15Click(Sender: TObject);
    procedure ToolButton16Click(Sender: TObject);
    procedure ToolButton11Click(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
    procedure Open1Click(Sender: TObject);
    procedure ToolButton2Click(Sender: TObject);
    procedure ToolButton3Click(Sender: TObject);
    procedure ToolButton4Click(Sender: TObject);
    procedure JvxSlider1Changed(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    FCounter: Integer;             // common used counter
    FMenuPainter: TMyMenuPainter;  // customized menu item painter
    FSelectedAll: Boolean;         // flag for text "select all"
    FIndex: Integer;               // text selection position
    FTabIndentSpace: Integer;      // number of text tab ident chat's
    FIniFileName: String;          // name of .ini file
    FDatabaseName: String;         // name of used databade
    FPrintPreview: TPrintPreviewerFrame;
  public
    FEditorZoom: Integer;          // content editor zoom
    FEditorFrame: TFrame5;

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

uses Unit3;

// the constructor for class TMyMenuPainter:
constructor TMyMenuPainter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

// the destructor for TMyMenuPainter:
destructor TMyMenuPainter.Destroy;
begin
  inherited Destroy;
end;

// set the width, and height of the menu items
procedure TMyMenuPainter.Measure(
  Item  : TMenuItem;
  var W : Integer;
  var H : Integer);
begin
  W := 220;
  H := 42;
  inherited Measure(Item,W,H);
end;

// override member, to flag the items measure
procedure TMyMenuPainter.Paint(
  AItem : TMenuItem;
  ARect : TRect;
  AState: TMenuOwnerDrawState);
begin
  inherited;
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
  FEditorFrame.RichEdit1.SelAttributes.Height := System.SysUtils.StrToInt(ComboBox1.Text) + 8;
  FEditorFrame.RichEdit1.SelAttributes.Name   := JvFontComboBox2.Text;
end;

// text format: background color
procedure TForm2.JvOfficeColorButton1ColorChange(Sender: TObject);
begin
  FEditorFrame.RichEdit1.SelAttributes.BackColor := JvOfficeColorButton1.SelectedColor;
end;

// text format: forground color
procedure TForm2.JvOfficeColorButton2ColorChange(Sender: TObject);
begin
  FEditorFrame.RichEdit1.SelAttributes.Color := JvOfficeColorButton2.SelectedColor;
end;

procedure TForm2.JvOfficeColorButton4ColorChange(Sender: TObject);
begin
  FEditorFrame.RichEdit1.Color := JvOfficeColorButton4.SelectedColor;
end;

procedure TForm2.JvPopupMenu1DrawItem(Sender: TMenu; Item: TMenuItem;
  Rect: TRect; State: TMenuOwnerDrawState);
begin
  Rect.Width := Rect.Width + 100;
  Rect.Height := Rect.Height + 20;
end;

// zoom slider for content editor
procedure TForm2.JvxSlider1Changed(Sender: TObject);
begin
  FEditorFrame.RichEdit1.Zoom := 6 * JvxSlider1.Value;
  Panel14.Caption := 'Zoom:  ' + IntToStr(6 * JvxSlider1.Value) + '%';
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

// show a about box - code of conduct
procedure TForm2.MenuItem3Click(Sender: TObject);
begin
  Application.CreateForm(TAboutForm, AboutForm);

  AboutForm.ShowModal;
  AboutForm.Free;
  AboutForm := nil;
end;

procedure TForm2.Open1Click(Sender: TObject);
begin
  ToolButton16Click(Sender);
end;

procedure TForm2.Open1MeasureItem(Sender: TObject; ACanvas: TCanvas; var W,
  H: Integer);
begin
  W := 120;
  H := 32;
end;

// RichEdit1: if content in clipboard, then paste it to the editor
procedure TForm2.PrinterSetup1Click(Sender: TObject);
begin
//  RichEdit1.Hide;
//  RichEdit1.Enabled := false;

  if FPrintPreview <> nil then
  FPrintPreview.Free;

  FPrintPreview := TPrintPreviewerFrame.Create(Panel6);
  FPrintPreview.Parent := Panel6;
  FPrintPreview.JvPopupMenu3.ItemPainter := FMenuPainter;
  FPrintPreview.Align  := alClient;
  FPrintPreview.Show;
end;

// open existing file
procedure TForm2.ToolButton11Click(Sender: TObject);
begin
  if (FEditorFrame.RichEdit1.SelStart   > 0) and
     (FEditorFrame.RichEdit1.SelLength  > 0) then
     (FEditorFrame.RichEdit1.CutToClipboard) ;
end;

procedure TForm2.ToolButton14Click(Sender: TObject);
begin
  if FEditorFrame.RichEdit1.CanRedo then
     FEditorFrame.RichEdit1.Redo
end;

// restore old status
procedure TForm2.ToolButton15Click(Sender: TObject);
begin
  FEditorFrame.RichEdit1.Undo;
end;

procedure TForm2.ToolButton16Click(Sender: TObject);
begin
  // set the directory near to application
  OpenDialog1.InitialDir :=
  ExtractFilePath(Application.ExeName);

  // open the dialog, if something nok, then error
  if not OpenDialog1.Execute then
  begin
    ShowMessage('something went wrong');
    exit;
  end;

  // try to guess the file content
  if ExtractFileExt(OpenDialog1.FileName) <> '.rtf' then
  begin
    ShowMessage('Error: File seems not be a RTF file.');
    exit;
  end;
end;

// toolbutton: bold
procedure TForm2.ToolButton1Click(Sender: TObject);
begin
  if fsBold in FEditorFrame.RichEdit1.SelAttributes.Style then
  FEditorFrame.RichEdit1.SelAttributes.Style  := FEditorFrame.RichEdit1.SelAttributes.Style - [fsBold] else
  FEditorFrame.RichEdit1.SelAttributes.Style  := FEditorFrame.RichEdit1.SelAttributes.Style + [fsBold];

  ToolButton1.Down := not ToolButton1.Down;
end;

// toolbutton: italic
procedure TForm2.ToolButton2Click(Sender: TObject);
begin
  if fsItalic in FEditorFrame.RichEdit1.SelAttributes.Style then
  FEditorFrame.RichEdit1.SelAttributes.Style  := FEditorFrame.RichEdit1.SelAttributes.Style - [fsItalic] else
  FEditorFrame.RichEdit1.SelAttributes.Style  := FEditorFrame.RichEdit1.SelAttributes.Style + [fsItalic];

  ToolButton2.Down := not ToolButton2.Down;
end;

// toolbutton: underline
procedure TForm2.ToolButton3Click(Sender: TObject);
begin
  if fsUnderline in FEditorFrame.RichEdit1.SelAttributes.Style then
  FEditorFrame.RichEdit1.SelAttributes.Style  := FEditorFrame.RichEdit1.SelAttributes.Style - [fsUnderline] else
  FEditorFrame.RichEdit1.SelAttributes.Style  := FEditorFrame.RichEdit1.SelAttributes.Style + [fsUnderline];

  ToolButton3.Down := not ToolButton3.Down;
end;

// toolbutton: strike-out
procedure TForm2.ToolButton4Click(Sender: TObject);
begin
  if fsStrikeout in FEditorFrame.RichEdit1.SelAttributes.Style then
  FEditorFrame.RichEdit1.SelAttributes.Style  := FEditorFrame.RichEdit1.SelAttributes.Style - [fsStrikeout] else
  FEditorFrame.RichEdit1.SelAttributes.Style  := FEditorFrame.RichEdit1.SelAttributes.Style + [fsStrikeout];

  ToolButton4.Down := not ToolButton4.Down;
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
var
  ini   : TIniFile;
  s1,s2 : String;
begin
  // open .ini file, and set the database name
  try
    FIniFileName := ChangeFileExt(Application.ExeName, '.ini');

    ini := TIniFile.Create(FIniFileName);
    try
      s1 := ExtractFilePath(Application.ExeName);
      s2 := ExtractFileName(Application.ExeName);

      s2 := ChangeFileExt(s2, '.s3db');
      s1 := s1 + s2 ;

      FDatabaseName := ini.ReadString('common','database', s1);
    finally
      ini.Free;
    end;
  except
    on E: Exception do
    begin
      ShowMessage('Error:'
      + #13#10
      + 'Exception: ' + E.Message);
    end;
  end;

  FMenuPainter := TMyMenuPainter.Create(Form2);
  FMenuPainter.SelectionFrameBrush.Color := clYellow;

  JvPopupMenu1.ItemPainter := FMenuPainter;
  JvPopupMenu2.ItemPainter := FMenuPainter;
  JvPopupMenu3.ItemPainter := FMenuPainter;

  JvGradientCaption1.Active := true;

  FSelectedAll := false;
  FTabIndentSpace := 4;

  FEditorZoom := 25;

  JvxSlider1.MaxValue := 100;
  JvxSlider1.Value    := 25;

  // create frame for content editor
  FEditorFrame := TFrame5.Create(Panel6);
  FEditorFrame.Parent  := Panel6;
  FEditorFrame.Align   := alClient;
  FEditorFrame.Visible := true;
  FEditorFrame.JvPopupMenu4.ItemPainter := FMenuPainter;

  // set database stuff
  LocalConnection.Params.Values['Database'] := FDatabaseName;
  LocalConnection.Open;

  // when the application is starting the first time,
  // then prepare tables ...
  FDQuery1.Close;
  FDQUERy1.SQL.Clear;
  FDQuery1.SQL.Add(''
  + 'CREATE TABLE IF NOT EXISTS test ('
  + 'id   INTEGER PRIMARY KEY,'
  + 'col1 TEXT'
  + ');');

  FDQuery1.ExecSQL;
end;

// destroy event handler is fired when the application is terminated.
// try to free prior allocated memory ...
procedure TForm2.FormDestroy(Sender: TObject);
var
  ini: TIniFile;
begin
  // close database table
  FDQuery1.SQL.Clear;
  FDQuery1.Close;
  FDQuery1.Free;

  // then, we close database
  LocalConnection.Close;

  // write data to storage
  ini := TIniFile.Create(FIniFilename);
  try
    ini.WriteString('common','database',FDatabaseName);
  finally
    ini.Free;
  end;

  FMenuPainter.Free;
  FMenuPainter := nil;

  FEditorFrame.Free;
  FEditorFrame := nil;
end;

// event handler that is fired when the form become visible true
// the default focus goes to the content editor
procedure TForm2.FormShow(Sender: TObject);
begin
  ShowWindow(Handle,SW_MAXIMIZE);
  FEditorFrame.RichEdit1.SetFocus;
end;

// get content of "open" hnd8.exe process content editor
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
  FEditorFrame.RichEdit1.Lines.Clear;
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
        FEditorFrame.RichEdit1.Perform(WM_SETTEXT, 0, PWChar(Clipboard.AsText));
      finally
        InputList.Free;
      end;

      // at end, switch back to applicatiin
      Winapi.Windows.SetForegroundWindow(self.Handle);
      FEditorFrame.RichEdit1.PasteFromClipboard;
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

  try
    FEditorFrame.RichEdit1.PlainText := false;
    FEditorFrame.RichEdit1.Lines.Clear;
    FEditorFrame.RichEdit1.Lines.Add(s);
  except
    on E: Exception do
    begin
      if E.ClassName = 'EOutOfResources' then
      begin
      end;
    end;
  end;
end;

// active LocalConnection
procedure TForm2.Button3Click(Sender: TObject);
begin
  LocalConnection.Open;
end;

procedure TForm2.ComboBox1Change(Sender: TObject);
begin
  FEditorFrame.RichEdit1.SelAttributes.Height := System.SysUtils.StrToInt(ComboBox1.Text) + 8;
  FEditorFrame.RichEdit1.SelAttributes.Name   := JvFontComboBox2.Text;
end;

// Menu: File->Exit  close, and exit application
procedure TForm2.Exit1Click(Sender: TObject);
begin
  Application.Terminate;
end;

end.
