object Frame5: TFrame5
  Left = 0
  Top = 0
  Width = 640
  Height = 480
  TabOrder = 0
  object ScrollBox1: TScrollBox
    Left = 0
    Top = 0
    Width = 640
    Height = 480
    Align = alClient
    TabOrder = 0
    object Splitter1: TSplitter
      Left = 0
      Top = 382
      Width = 636
      Height = 3
      Cursor = crVSplit
      Align = alTop
      ExplicitTop = 281
      ExplicitWidth = 195
    end
    object Splitter2: TSplitter
      Left = 0
      Top = 385
      Width = 636
      Height = 3
      Cursor = crVSplit
      Align = alTop
      ExplicitWidth = 91
    end
    object RichEdit1: TJvRichEdit
      Left = 0
      Top = 0
      Width = 636
      Height = 265
      Align = alTop
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -14
      Font.Name = 'Segoe UI'
      Font.Style = []
      HideSelection = False
      HideScrollBars = False
      ParentFont = False
      PopupMenu = JvPopupMenu4
      SelText = ''
      StreamFormat = sfRichText
      TabOrder = 0
      WantTabs = True
      WordWrap = False
      OnKeyDown = RichEdit1KeyDown
      OnKeyPress = RichEdit1KeyPress
      OnKeyUp = RichEdit1KeyUp
      OnMouseDown = RichEdit1MouseDown
    end
    object Memo1: TMemo
      Left = 0
      Top = 265
      Width = 636
      Height = 117
      Align = alTop
      Lines.Strings = (
        'unit test;'
        ''
        'var'
        '  i;'
        'begin'
        '  i := 12;'
        '  write i;'
        'end test;')
      TabOrder = 1
      OnKeyDown = Memo1KeyDown
      ExplicitLeft = -3
    end
    object Memo2: TMemo
      Left = 0
      Top = 388
      Width = 636
      Height = 88
      Align = alClient
      PopupMenu = JvPopupMenu1
      ScrollBars = ssBoth
      TabOrder = 2
    end
  end
  object JvPopupMenu4: TJvPopupMenu
    Style = msItemPainter
    ImageMargin.Left = 0
    ImageMargin.Top = 0
    ImageMargin.Right = 0
    ImageMargin.Bottom = 0
    ImageSize.Height = 32
    ImageSize.Width = 32
    Left = 341
    Top = 197
    object Undo1: TMenuItem
      Caption = 'Undo'
      ShortCut = 16474
      OnClick = Undo1Click
    end
    object Undo2: TMenuItem
      Caption = '-'
    end
    object Cut1: TMenuItem
      Caption = 'Cut'
      OnClick = Cut1Click
    end
    object Copy1: TMenuItem
      Caption = 'Copy'
      ShortCut = 16451
      OnClick = Copy1Click
    end
    object Delete1: TMenuItem
      Caption = 'Delete'
      ShortCut = 46
      OnClick = Delete1Click
    end
    object Paste1: TMenuItem
      Caption = 'Paste'
      ShortCut = 16470
      OnClick = Paste1Click
    end
    object Paste2: TMenuItem
      Caption = '-'
    end
    object SelectAll1: TMenuItem
      Caption = 'Select All'
      ShortCut = 16449
      OnClick = SelectAll1Click
    end
  end
  object JvInterpreterProgram1: TJvInterpreterProgram
    Left = 184
    Top = 96
  end
  object JvPopupMenu1: TJvPopupMenu
    Style = msItemPainter
    ImageMargin.Left = 0
    ImageMargin.Top = 0
    ImageMargin.Right = 0
    ImageMargin.Bottom = 0
    ImageSize.Height = 32
    ImageSize.Width = 32
    Left = 229
    Top = 309
    object Run1: TMenuItem
      Caption = 'Run'
      ShortCut = 113
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object MenuItem1: TMenuItem
      Caption = 'Undo'
      ShortCut = 16474
      OnClick = MenuItem1Click
    end
    object MenuItem2: TMenuItem
      Caption = '-'
    end
    object MenuItem3: TMenuItem
      Caption = 'Cut'
      OnClick = MenuItem3Click
    end
    object MenuItem4: TMenuItem
      Caption = 'Copy'
      ShortCut = 16451
      OnClick = MenuItem4Click
    end
    object MenuItem5: TMenuItem
      Caption = 'Delete'
      ShortCut = 46
      OnClick = MenuItem5Click
    end
    object MenuItem6: TMenuItem
      Caption = 'Paste'
      ShortCut = 16470
      OnClick = MenuItem6Click
    end
    object MenuItem7: TMenuItem
      Caption = '-'
    end
    object MenuItem8: TMenuItem
      Caption = 'Select All'
      ShortCut = 16449
      OnClick = MenuItem8Click
    end
  end
end
