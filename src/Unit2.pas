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
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, TLHelp32, Clipbrd, System.Generics.Collections,
  Vcl.ComCtrls, Vcl.ToolWin, System.ImageList, Vcl.ImgList, Vcl.Menus,
  JvFullColorSpaces, JvFullColorCtrls, JvExStdCtrls, JvCombobox, JvColorCombo,
  Vcl.ExtCtrls;

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
  TForm2 = class(TForm)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    ScrollBox1: TScrollBox;
    StatusBar1: TStatusBar;
    PageControl2: TPageControl;
    TabSheet3: TTabSheet;
    ScrollBox2: TScrollBox;
    RichEdit1: TRichEdit;
    Button2: TButton;
    Button1: TButton;
    Label1: TLabel;
    TabSheet4: TTabSheet;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ImageList1: TImageList;
    PopupMenu1: TPopupMenu;
    Consolas1: TMenuItem;
    Consolas2: TMenuItem;
    imeNewRoman1: TMenuItem;
    imeNewRoman2: TMenuItem;
    Panel1: TPanel;
    Panel2: TPanel;
    JvFontComboBox2: TJvFontComboBox;
    JvColorComboBox1: TJvColorComboBox;
    Panel3: TPanel;
    ComboBox1: TComboBox;
    procedure Button1Click(Sender: TObject);
  private
    FCounter: Integer;
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

        (*
        // only for information debug:
        // [..
        s1 := ''
        + #10 + 'Process Name   : = '   + ProcEntry.szExeFile
        + #10 + 'Process ID     : = 0x' + IntToHex(ProcEntry.th32ProcessID)
        + #10 + 'Thread Count   : = '   + IntToStr(ProcEntry.cntThreads);

        if priClass > 0 then
        s1 := s1
        + #10 + 'Priority class : = '   + IntToStr(priClass);
        ShowMessage(s1);
        // ..]
        *)

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

end.
