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
    ExplicitLeft = 64
    ExplicitTop = 312
    ExplicitWidth = 185
    ExplicitHeight = 41
    object Splitter1: TSplitter
      Left = 0
      Top = 321
      Width = 636
      Height = 3
      Cursor = crVSplit
      Align = alTop
      ExplicitTop = 281
      ExplicitWidth = 195
    end
    object RichEdit1: TJvRichEdit
      Left = 0
      Top = 0
      Width = 636
      Height = 321
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
      Top = 324
      Width = 636
      Height = 117
      Align = alTop
      Lines.Strings = (
        'Memo1')
      TabOrder = 1
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
end
