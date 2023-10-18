object Form2: TForm2
  Left = 0
  Top = 0
  Caption = 'HelpNDoc Cheater Tooll (c) 2023 by Jens Kallup - paule32'
  ClientHeight = 650
  ClientWidth = 1121
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -16
  Font.Name = 'Consolas'
  Font.Style = []
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  TextHeight = 19
  object StatusBar1: TStatusBar
    Left = 0
    Top = 631
    Width = 1121
    Height = 19
    Panels = <
      item
        Bevel = pbRaised
        Text = 'Ready:'
        Width = 100
      end
      item
        Width = 50
      end>
  end
  object JvToolBar1: TJvToolBar
    Left = 0
    Top = 0
    Width = 1121
    Height = 28
    ButtonHeight = 25
    ButtonWidth = 66
    Caption = 'JvToolBar1'
    DrawingStyle = dsGradient
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    List = True
    ParentFont = False
    AllowTextButtons = True
    TabOrder = 1
    Transparent = False
    object ToolButton5: TToolButton
      Left = 0
      Top = 0
      Caption = 'File'
      DropdownMenu = JvPopupMenu1
      ImageIndex = 0
      PopupMenu = JvPopupMenu1
      Style = tbsTextButton
      OnClick = ToolButton5Click
    end
    object ToolButton6: TToolButton
      Left = 43
      Top = 0
      Caption = 'Edit'
      ImageIndex = 1
      Style = tbsTextButton
    end
    object ToolButton7: TToolButton
      Left = 89
      Top = 0
      Caption = 'Project'
      ImageIndex = 2
      Style = tbsTextButton
    end
    object ToolButton8: TToolButton
      Left = 159
      Top = 0
      Caption = 'Help'
      DropdownMenu = JvPopupMenu2
      ImageIndex = 3
      Style = tbsTextButton
    end
  end
  object Panel4: TPanel
    Left = 0
    Top = 28
    Width = 1121
    Height = 47
    Align = alTop
    TabOrder = 2
    object JvToolBar3: TJvToolBar
      Left = 1
      Top = 1
      Width = 1119
      Height = 45
      ButtonHeight = 54
      ButtonWidth = 149
      Caption = 'JvToolBar1'
      DrawingStyle = dsGradient
      Images = ImageList2
      List = True
      ShowCaptions = True
      AllowTextButtons = True
      TabOrder = 0
      Transparent = False
      object ToolButton16: TToolButton
        Left = 0
        Top = 0
        AutoSize = True
        Caption = 'Open'
        ImageIndex = 5
        Style = tbsTextButton
        OnClick = ToolButton16Click
      end
      object ToolButton13: TToolButton
        Left = 99
        Top = 0
        Caption = 'Save'
        ImageIndex = 2
        Style = tbsTextButton
      end
      object ToolButton14: TToolButton
        Left = 198
        Top = 0
        Caption = 'Redo'
        ImageIndex = 3
        Style = tbsTextButton
        OnClick = ToolButton14Click
      end
      object ToolButton15: TToolButton
        Left = 297
        Top = 0
        Caption = 'Undo'
        ImageIndex = 4
        Style = tbsTextButton
        OnClick = ToolButton15Click
      end
      object ToolButton12: TToolButton
        Left = 396
        Top = 0
        Caption = 'Copy+Paste'
        ImageIndex = 1
        Style = tbsTextButton
      end
      object ToolButton17: TToolButton
        Left = 549
        Top = 0
        Width = 56
        Caption = 'ToolButton17'
        ImageIndex = 1
        Style = tbsSeparator
      end
      object ToolButton11: TToolButton
        Left = 605
        Top = 0
        Caption = 'Cut'
        ImageIndex = 0
        Style = tbsTextButton
        OnClick = ToolButton11Click
      end
      object PageScroller1: TPageScroller
        Left = 695
        Top = 0
        Width = 243
        Height = 54
        TabOrder = 0
      end
    end
  end
  object ScrollBox1: TScrollBox
    AlignWithMargins = True
    Left = 3
    Top = 132
    Width = 1115
    Height = 496
    Align = alClient
    TabOrder = 3
    object Splitter1: TSplitter
      Left = 239
      Top = 25
      Height = 467
      ExplicitLeft = 216
      ExplicitTop = 352
      ExplicitHeight = 100
    end
    object Splitter2: TSplitter
      Left = 793
      Top = 25
      Height = 467
      ExplicitLeft = 840
      ExplicitTop = 248
      ExplicitHeight = 100
    end
    object JvToolBar2: TJvToolBar
      Left = 0
      Top = 0
      Width = 1111
      Height = 25
      ButtonHeight = 25
      ButtonWidth = 237
      Caption = 'JvToolBar1'
      DrawingStyle = dsGradient
      List = True
      AllowTextButtons = True
      TabOrder = 0
      Transparent = False
      object ToolButton9: TToolButton
        Left = 0
        Top = 0
        Caption = 'Topics:                  '
        ImageIndex = 0
        PopupMenu = JvPopupMenu1
        Style = tbsTextButton
        OnClick = ToolButton5Click
      end
      object ToolButton10: TToolButton
        Left = 241
        Top = 0
        Caption = 'Editor:'
        ImageIndex = 1
        Style = tbsTextButton
      end
    end
    object Panel5: TPanel
      Left = 0
      Top = 25
      Width = 239
      Height = 467
      Align = alLeft
      Caption = 'Panel5'
      TabOrder = 1
      object Panel7: TPanel
        Left = 1
        Top = 1
        Width = 237
        Height = 40
        Align = alTop
        TabOrder = 1
        object ToolBar2: TToolBar
          Left = 1
          Top = 1
          Width = 235
          Height = 40
          ButtonHeight = 33
          ButtonWidth = 36
          Caption = 'ToolBar2'
          Images = ImageList3
          TabOrder = 0
          object ToolButton18: TToolButton
            Left = 0
            Top = 0
            AutoSize = True
            Caption = 'ToolButton18'
            ImageIndex = 0
          end
          object ToolButton19: TToolButton
            Left = 31
            Top = 0
            Caption = 'ToolButton19'
            ImageIndex = 3
          end
          object ToolButton20: TToolButton
            Left = 67
            Top = 0
            Caption = 'ToolButton20'
            ImageIndex = 1
          end
          object ToolButton21: TToolButton
            Left = 103
            Top = 0
            Caption = 'ToolButton21'
            ImageIndex = 2
          end
          object ToolButton22: TToolButton
            Left = 139
            Top = 0
            Caption = 'ToolButton22'
            ImageIndex = 4
          end
          object ToolButton23: TToolButton
            Left = 175
            Top = 0
            Width = 11
            Caption = 'ToolButton23'
            ImageIndex = 5
            Style = tbsSeparator
          end
          object JvArrowButton1: TJvArrowButton
            Left = 186
            Top = 0
            Width = 41
            Height = 33
            ArrowWidth = 21
            FillFont.Charset = DEFAULT_CHARSET
            FillFont.Color = clWindowText
            FillFont.Height = -14
            FillFont.Name = 'Segoe UI'
            FillFont.Style = []
            Glyph.Data = {
              F6060000424DF606000000000000360000002800000018000000180000000100
              180000000000C0060000B0110000B01100000000000000000000FFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF505050474747E0E0E0FFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF303030
              0000000A0A0A888888FDFDFDFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFF303030000000000000000000444444E0E0E0FFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFF3030300000000000000000000000003B3B3BFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF3030300000000000000000
              00000000303030FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF303030
              000000000000000000000000303030FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFF303030000000000000000000000000303030FFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFF303030000000000000000000000000303030FF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF3030300000000000000000
              00000000303030FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF303030
              000000000000000000000000303030FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFF303030000000000000000000000000303030FFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFC8C8C8BBBBBBBBBBBBBBBBBBBBBBBBC8C8C8FF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFCFCFCAEAEAEAAAAAAAAAAAAAAAAAAAAAA
              AAAAAAAAAAAAAAAEAEAEFCFCFCFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFDFDFD5E5E5E000000000000
              000000000000000000000000000000000000575757FCFCFCFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFEFE67676700
              0000000000000000000000000000000000000000000000000000000000606060
              FDFDFDFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFE
              FE69696900000000000000000000000000000000000000000000000000000000
              0000000000000000626262FDFDFDFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFEFEFE6A6A6A0000000000000000000000000000000000000000000000
              00000000000000000000000000000000000000636363FDFDFDFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFEFEFE6C6C6C000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000006565
              65FEFEFEFFFFFFFFFFFFFFFFFFFFFFFF75757500000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000006E6E6EFEFEFEFFFFFFFFFFFFA1A1A13333333333333333
              3333333333333333333333333333333333333333333333333333333333333333
              3333333333333333333333333333333333333333A1A1A1FFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFCCCCCC333333333333333333333333333333333333333333333333333333
              3333333333333333333333333333333333333333333333333333333333333333
              33333333333333CCCCCCBFBFBF00000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              000000000000000000000000000000BFBFBFD9D9D90B0B0B0000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              00000000000000000000000000000000000000000B0B0BDADADA}
          end
        end
      end
      object Button1: TButton
        Left = 27
        Top = 278
        Width = 167
        Height = 41
        Caption = 'Get Editor Text'
        Enabled = False
        TabOrder = 2
        Visible = False
        OnClick = Button1Click
      end
      object Button2: TButton
        Left = 27
        Top = 325
        Width = 167
        Height = 42
        Caption = 'Set Editor Text'
        Enabled = False
        TabOrder = 3
        Visible = False
        OnClick = Button2Click
      end
      object TreeView1: TTreeView
        Left = 1
        Top = 41
        Width = 237
        Height = 360
        Align = alTop
        Indent = 19
        RowSelect = True
        TabOrder = 0
        Items.NodeData = {
          0301000000380000000500000005000000FFFFFFFFFFFFFFFF00000000000000
          0002000000010D500072006F006A0065006300740020004E0061006D0065003A
          002C0000000600000006000000FFFFFFFFFFFFFFFF0000000000000000000000
          00010754006F00700069006300200031002E0000000000000000000000FFFFFF
          FFFFFFFFFF000000000000000000000000010841007000700065006E00640069
          007800}
      end
      object Button3: TButton
        Left = 27
        Top = 407
        Width = 75
        Height = 50
        Caption = 'Button3'
        TabOrder = 4
        OnClick = Button3Click
      end
    end
    object Panel6: TPanel
      Left = 242
      Top = 25
      Width = 551
      Height = 467
      Align = alLeft
      Anchors = [akLeft, akTop, akRight, akBottom]
      Caption = 'Panel6'
      TabOrder = 2
      object Panel13: TPanel
        Left = 1
        Top = 425
        Width = 549
        Height = 41
        Align = alBottom
        TabOrder = 0
        object Label5: TLabel
          Left = 336
          Top = 16
          Width = 54
          Height = 19
          Caption = 'Label5'
        end
        object JvxSlider1: TJvxSlider
          Left = 127
          Top = 1
          Width = 150
          Height = 40
          TabOrder = 0
          Value = 25
          OnChanged = JvxSlider1Changed
        end
        object Panel14: TPanel
          Left = 1
          Top = 1
          Width = 120
          Height = 39
          Align = alLeft
          Caption = 'Zoom: 100%'
          TabOrder = 1
        end
      end
    end
    object Panel10: TPanel
      Left = 796
      Top = 25
      Width = 315
      Height = 467
      Align = alClient
      Anchors = [akTop, akRight, akBottom]
      Caption = 'Panel10'
      TabOrder = 3
      object Splitter3: TSplitter
        Left = 1
        Top = 273
        Width = 313
        Height = 3
        Cursor = crVSplit
        Align = alTop
        ExplicitWidth = 193
      end
      object Panel11: TPanel
        Left = 1
        Top = 1
        Width = 313
        Height = 272
        Align = alTop
        Caption = 'Panel11'
        TabOrder = 0
        object TreeView2: TTreeView
          Left = 1
          Top = 1
          Width = 311
          Height = 270
          Align = alClient
          Indent = 19
          TabOrder = 0
        end
      end
      object Panel12: TPanel
        Left = 1
        Top = 276
        Width = 313
        Height = 190
        Align = alClient
        Caption = 'Panel12'
        TabOrder = 1
        object TreeView3: TTreeView
          Left = 1
          Top = 1
          Width = 311
          Height = 188
          Align = alClient
          Indent = 19
          TabOrder = 0
        end
      end
    end
  end
  object Panel8: TPanel
    Left = 0
    Top = 75
    Width = 1121
    Height = 54
    Align = alTop
    AutoSize = True
    TabOrder = 4
    object ToolBar1: TToolBar
      Left = 1
      Top = 1
      Width = 1119
      Height = 52
      BorderWidth = 2
      ButtonHeight = 45
      ButtonWidth = 65
      Caption = 'ToolBar1'
      Color = clGray
      Images = ImageList1
      ParentColor = False
      TabOrder = 0
      Transparent = False
      StyleName = 'Windows'
      Wrapable = False
      object ToolButton1: TToolButton
        AlignWithMargins = True
        Left = 0
        Top = 0
        Caption = 'B'
        ImageIndex = 0
        Style = tbsTextButton
        OnClick = ToolButton1Click
      end
      object ToolButton2: TToolButton
        Left = 65
        Top = 0
        Caption = 'ToolButton2'
        ImageIndex = 1
        OnClick = ToolButton2Click
      end
      object ToolButton3: TToolButton
        Left = 130
        Top = 0
        Caption = 'ToolButton3'
        ImageIndex = 2
        OnClick = ToolButton3Click
      end
      object ToolButton4: TToolButton
        Left = 195
        Top = 0
        Caption = 'ToolButton4'
        ImageIndex = 3
        OnClick = ToolButton4Click
      end
      object Panel2: TPanel
        Left = 260
        Top = 0
        Width = 197
        Height = 45
        Align = alTop
        ParentBackground = False
        TabOrder = 1
        object JvFontComboBox2: TJvFontComboBox
          Left = 6
          Top = 9
          Width = 177
          Height = 26
          DroppedDownWidth = 177
          MaxMRUCount = 0
          FontName = 'Consolas'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -16
          Font.Name = 'Consolas'
          Font.Style = []
          ItemIndex = 43
          ParentFont = False
          Sorted = False
          TabOrder = 0
          OnChange = JvFontComboBox2Change
        end
      end
      object Panel1: TPanel
        Left = 457
        Top = 0
        Width = 160
        Height = 45
        ParentBackground = False
        TabOrder = 0
        object Label2: TLabel
          Left = 47
          Top = 1
          Width = 18
          Height = 19
          Caption = 'FG'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -16
          Font.Name = 'Consolas'
          Font.Style = [fsBold]
          ParentFont = False
          OnClick = Label2Click
        end
        object Label3: TLabel
          Left = 47
          Top = 20
          Width = 18
          Height = 19
          Caption = 'BG'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -16
          Font.Name = 'Consolas'
          Font.Style = [fsBold]
          ParentFont = False
          OnClick = Label3Click
        end
        object JvOfficeColorButton2: TJvOfficeColorButton
          Left = 6
          Top = 1
          Width = 35
          Height = 22
          ColorDialogOptions = [cdFullOpen]
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -16
          Font.Name = 'Consolas'
          Font.Style = []
          ParentFont = False
          TabOrder = 0
          SelectedColor = clDefault
          HotTrackFont.Charset = DEFAULT_CHARSET
          HotTrackFont.Color = clWindowText
          HotTrackFont.Height = -14
          HotTrackFont.Name = 'Segoe UI'
          HotTrackFont.Style = []
          Properties.ShowUserColors = True
          Properties.HoldCustomColor = True
          Properties.NoneColorCaption = 'No Color'
          Properties.DefaultColorCaption = 'Automatic'
          Properties.CustomColorCaption = 'Other Colors...'
          Properties.NoneColorHint = 'No Color'
          Properties.DefaultColorHint = 'Automatic'
          Properties.CustomColorHint = 'Other Colors...'
          Properties.NoneColorFont.Charset = DEFAULT_CHARSET
          Properties.NoneColorFont.Color = clWindowText
          Properties.NoneColorFont.Height = -14
          Properties.NoneColorFont.Name = 'Segoe UI'
          Properties.NoneColorFont.Style = []
          Properties.DefaultColorFont.Charset = DEFAULT_CHARSET
          Properties.DefaultColorFont.Color = clWindowText
          Properties.DefaultColorFont.Height = -14
          Properties.DefaultColorFont.Name = 'Segoe UI'
          Properties.DefaultColorFont.Style = []
          Properties.CustomColorFont.Charset = DEFAULT_CHARSET
          Properties.CustomColorFont.Color = clWindowText
          Properties.CustomColorFont.Height = -14
          Properties.CustomColorFont.Name = 'Segoe UI'
          Properties.CustomColorFont.Style = []
          Properties.ColorSize = 28
          Properties.SelectIfPopup = True
          Properties.FloatWindowCaption = 'Color Window'
          Properties.DragBarHint = 'Drag to float'
          OnColorChange = JvOfficeColorButton2ColorChange
        end
        object JvOfficeColorButton1: TJvOfficeColorButton
          Left = 6
          Top = 19
          Width = 35
          Height = 22
          ColorDialogOptions = [cdFullOpen]
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -16
          Font.Name = 'Consolas'
          Font.Style = []
          ParentFont = False
          TabOrder = 1
          SelectedColor = clDefault
          HotTrackFont.Charset = DEFAULT_CHARSET
          HotTrackFont.Color = clWindowText
          HotTrackFont.Height = -14
          HotTrackFont.Name = 'Segoe UI'
          HotTrackFont.Style = []
          Properties.ShowUserColors = True
          Properties.NoneColorCaption = 'No Color'
          Properties.DefaultColorCaption = 'Automatic'
          Properties.CustomColorCaption = 'Other Colors...'
          Properties.NoneColorHint = 'No Color'
          Properties.DefaultColorHint = 'Automatic'
          Properties.CustomColorHint = 'Other Colors...'
          Properties.NoneColorFont.Charset = DEFAULT_CHARSET
          Properties.NoneColorFont.Color = clWindowText
          Properties.NoneColorFont.Height = -14
          Properties.NoneColorFont.Name = 'Segoe UI'
          Properties.NoneColorFont.Style = []
          Properties.DefaultColorFont.Charset = DEFAULT_CHARSET
          Properties.DefaultColorFont.Color = clWindowText
          Properties.DefaultColorFont.Height = -14
          Properties.DefaultColorFont.Name = 'Segoe UI'
          Properties.DefaultColorFont.Style = []
          Properties.CustomColorFont.Charset = DEFAULT_CHARSET
          Properties.CustomColorFont.Color = clWindowText
          Properties.CustomColorFont.Height = -14
          Properties.CustomColorFont.Name = 'Segoe UI'
          Properties.CustomColorFont.Style = []
          Properties.ColorSize = 28
          Properties.FloatWindowCaption = 'Color Window'
          Properties.DragBarHint = 'Drag to float'
          OnColorChange = JvOfficeColorButton1ColorChange
        end
        object Panel3: TPanel
          Left = 79
          Top = 1
          Width = 72
          Height = 47
          Color = 15790320
          ParentBackground = False
          TabOrder = 2
          object ComboBox1: TComboBox
            Left = 6
            Top = 9
            Width = 54
            Height = 27
            TabOrder = 0
            Text = '10'
            OnChange = ComboBox1Change
            Items.Strings = (
              '8'
              '9'
              '10'
              '11'
              '12'
              '14'
              '16'
              '18'
              '20'
              '21'
              '22'
              '24'
              '26'
              '28'
              '30'
              '32'
              '34'
              '36'
              '38'
              '40'
              '44'
              '48'
              '50'
              '60'
              '70'
              '80'
              '100')
          end
        end
      end
      object Panel9: TPanel
        Left = 617
        Top = 0
        Width = 134
        Height = 45
        BevelOuter = bvNone
        Color = 15790320
        ParentBackground = False
        TabOrder = 2
        object Label1: TLabel
          Left = 38
          Top = 20
          Width = 54
          Height = 19
          Caption = 'Border'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -16
          Font.Name = 'Consolas'
          Font.Style = [fsBold]
          ParentFont = False
          OnClick = Label1Click
        end
        object Label4: TLabel
          Left = 38
          Top = 1
          Width = 90
          Height = 19
          Caption = 'Page Color'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -16
          Font.Name = 'Consolas'
          Font.Style = [fsBold]
          ParentFont = False
          OnClick = Label4Click
        end
        object JvOfficeColorButton3: TJvOfficeColorButton
          AlignWithMargins = True
          Left = -3
          Top = 15
          Width = 35
          Height = 27
          ColorDialogOptions = [cdFullOpen]
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -16
          Font.Name = 'Consolas'
          Font.Style = []
          ParentFont = False
          TabOrder = 0
          SelectedColor = clDefault
          HotTrackFont.Charset = DEFAULT_CHARSET
          HotTrackFont.Color = clWindowText
          HotTrackFont.Height = -14
          HotTrackFont.Name = 'Segoe UI'
          HotTrackFont.Style = []
          Properties.ShowUserColors = True
          Properties.NoneColorCaption = 'No Color'
          Properties.DefaultColorCaption = 'Automatic'
          Properties.CustomColorCaption = 'Other Colors...'
          Properties.NoneColorHint = 'No Color'
          Properties.DefaultColorHint = 'Automatic'
          Properties.CustomColorHint = 'Other Colors...'
          Properties.NoneColorFont.Charset = DEFAULT_CHARSET
          Properties.NoneColorFont.Color = clWindowText
          Properties.NoneColorFont.Height = -14
          Properties.NoneColorFont.Name = 'Segoe UI'
          Properties.NoneColorFont.Style = []
          Properties.DefaultColorFont.Charset = DEFAULT_CHARSET
          Properties.DefaultColorFont.Color = clWindowText
          Properties.DefaultColorFont.Height = -14
          Properties.DefaultColorFont.Name = 'Segoe UI'
          Properties.DefaultColorFont.Style = []
          Properties.CustomColorFont.Charset = DEFAULT_CHARSET
          Properties.CustomColorFont.Color = clWindowText
          Properties.CustomColorFont.Height = -14
          Properties.CustomColorFont.Name = 'Segoe UI'
          Properties.CustomColorFont.Style = []
          Properties.ColorSize = 28
          Properties.FloatWindowCaption = 'Color Window'
          Properties.DragBarHint = 'Drag to float'
          OnColorChange = JvOfficeColorButton1ColorChange
        end
        object JvOfficeColorButton4: TJvOfficeColorButton
          Left = -3
          Top = 1
          Width = 35
          Height = 22
          ColorDialogOptions = [cdFullOpen]
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -16
          Font.Name = 'Consolas'
          Font.Style = []
          ParentFont = False
          TabOrder = 1
          SelectedColor = clDefault
          HotTrackFont.Charset = DEFAULT_CHARSET
          HotTrackFont.Color = clWindowText
          HotTrackFont.Height = -14
          HotTrackFont.Name = 'Segoe UI'
          HotTrackFont.Style = []
          Properties.ShowUserColors = True
          Properties.HoldCustomColor = True
          Properties.NoneColorCaption = 'No Color'
          Properties.DefaultColorCaption = 'Automatic'
          Properties.CustomColorCaption = 'Other Colors...'
          Properties.NoneColorHint = 'No Color'
          Properties.DefaultColorHint = 'Automatic'
          Properties.CustomColorHint = 'Other Colors...'
          Properties.NoneColorFont.Charset = DEFAULT_CHARSET
          Properties.NoneColorFont.Color = clWindowText
          Properties.NoneColorFont.Height = -14
          Properties.NoneColorFont.Name = 'Segoe UI'
          Properties.NoneColorFont.Style = []
          Properties.DefaultColorFont.Charset = DEFAULT_CHARSET
          Properties.DefaultColorFont.Color = clWindowText
          Properties.DefaultColorFont.Height = -14
          Properties.DefaultColorFont.Name = 'Segoe UI'
          Properties.DefaultColorFont.Style = []
          Properties.CustomColorFont.Charset = DEFAULT_CHARSET
          Properties.CustomColorFont.Color = clWindowText
          Properties.CustomColorFont.Height = -14
          Properties.CustomColorFont.Name = 'Segoe UI'
          Properties.CustomColorFont.Style = []
          Properties.ColorSize = 28
          Properties.SelectIfPopup = True
          Properties.FloatWindowCaption = 'Color Window'
          Properties.DragBarHint = 'Drag to float'
          OnColorChange = JvOfficeColorButton4ColorChange
        end
      end
    end
  end
  object ImageList1: TImageList
    DrawingStyle = dsTransparent
    Height = 24
    Width = 24
    Left = 171
    Top = 207
    Bitmap = {
      494C010104000800040018001800FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000600000003000000001002000000000000048
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000001111110011111100111111001111
      1100111111001111110011111100111111001111110011111100111111001111
      1100111111001D1D1D002C2C2C005555550087878700D6D6D600000000000000
      0000000000000000000000000000000000000000000000000000FCFCFC003636
      3600000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000050505009595950000000000000000000000
      00000000000000000000000000000000000000000000B0B0B000454545004444
      4400444444004444440044444400444444004444440044444400444444004444
      4400444444004444440044444400444444004444440044444400444444004444
      44004444440045454500B1B1B100000000000000000000000000000000000000
      000000000000F8F8F800AEAEAE005E5E5E003E3E3E00252525000C0C0C001010
      100031313100525252007A7A7A00E1E1E1000000000000000000000000000000
      000000000000000000000000000000000000838383005E5E5E00070707000000
      0000000000000000000000000000000000000000000030303000969696009292
      92006666660012121200000000000000000000000000000000003C3C3C00CCCC
      CC00000000000000000000000000000000000000000000000000F9F9F9002222
      2200000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008383830000000000000000000000
      0000000000000000000000000000000000000000000056565600000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000056565600000000000000000000000000000000000000
      0000969696000E0E0E0000000000000000000000000000000000000000000000
      00000000000000000000000000000505050065656500E4E4E400000000000000
      0000000000000000000000000000000000000000000000000000A8A8A8000000
      00000000000000000000000000000000000000000000E1E1E100000000000000
      000000000000F1F1F10054545400000000000000000000000000000000000303
      030096969600000000000000000000000000000000000000000000000000FBFB
      FB00CCCCCC008C8C8C0015151500000000000000000000000000000000000000
      00000202020066666600B8B8B800E2E2E2000000000000000000000000000000
      000000000000000000000000000000000000000000005F5F5F00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000060606000000000000000000000000000000000000000
      0000676767000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000002A2A2A00F3F3F3000000
      0000000000000000000000000000000000000000000000000000ECECEC000000
      0000000000000000000000000000000000000808080000000000000000000000
      00000000000000000000F9F9F900343434000000000000000000000000000000
      000001010100B8B8B80000000000000000000000000000000000000000000000
      00000000000000000000E0E0E000101010000000000000000000000000000000
      00006A6A6A000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000E8E8E800AAAAAA00AAAA
      AA00AAAAAA00AAAAAA00AAAAAA00AAAAAA00AAAAAA00AAAAAA00AAAAAA00AAAA
      AA00AAAAAA00AAAAAA00AAAAAA00AAAAAA00AAAAAA00AAAAAA00AAAAAA00AAAA
      AA00AAAAAA00AAAAAA00E8E8E800000000000000000000000000000000000000
      0000676767000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000565656000000
      0000000000000000000000000000000000000000000000000000FAFAFA000000
      0000000000000000000000000000000000000606060000000000000000000000
      0000000000000000000000000000ACACAC000000000000000000000000000000
      0000000000002D2D2D0000000000000000000000000000000000000000000000
      00000000000000000000000000008E8E8E000000000000000000000000000000
      0000898989000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00006767670000000000000000002323230076767600A1A1A100C6C6C600D8D8
      D8009F9F9F005E5E5E000101010000000000000000000000000000000000DDDD
      DD00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000606060000000000000000000000
      0000000000000000000000000000EFEFEF000000000000000000000000000000
      00000000000000000000DFDFDF00000000000000000000000000000000000000
      0000000000000000000000000000D9D9D9000000000000000000000000000000
      0000565656000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000DCDCDC00949494007474
      74007474740095959500DDDDDD00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00007878780066666600C4C4C400000000000000000000000000000000000000
      000000000000000000007373730000000000000000000000000000000000AAAA
      AA00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000606060000000000000000000000
      0000000000000000000000000000000000000505050000000000000000000000
      00000000000000000000C3C3C300000000000000000000000000000000000000
      0000000000000000000000000000000000002020200000000000000000000000
      000014141400FEFEFE0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000DADADA004B4B4B0000000000000000000000
      00000000000000000000000000004D4D4D00DBDBDB0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000B0B0B000000000000000000000000000000000007979
      7900000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000606060000000000000000000000
      0000000000000000000000000000EAEAEA000000000000000000000000000000
      00000000000000000000D2D2D200000000000000000000000000000000000000
      0000000000000000000000000000000000006868680000000000000000000000
      000000000000CFCFCF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000BDBDBD00101010000000000000000000000000000000
      00000000000000000000000000000000000010101000BEBEBE00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000071717100000000000000000000000000000000007F7F
      7F00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000606060000000000000000000000
      0000000000000000000000000000AAAAAA000000000000000000000000000000
      00000000000016161600FCFCFC00000000000000000000000000000000000000
      000000000000000000000000000000000000AFAFAF0000000000000000000000
      0000000000008B8B8B0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000D8D8D8000F0F0F00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000010101000D9D9D9000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000929292000202020000000000000000000000000000000000A2A2
      A200000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000606060000000000000000000000
      00000000000000000000FBFBFB00373737000000000000000000000000000000
      0000000000009090900000000000000000000000000000000000000000000000
      000000000000000000000000000000000000F2F2F20005050500000000000000
      0000000000004848480000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000047474700000000000000000000000000030303006B6B6B00B8B8
      B800B7B7B7006A6A6A0003030300000000000000000000000000494949000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000BBBB
      BB003535350000000000000000000000000000000000000000000F0F0F00EEEE
      EE00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000606060000000000000000000000
      000000000000F8F8F80064646400000000000000000000000000000000000000
      000067676700FEFEFE0000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000003E3E3E00000000000000
      0000000000000B0B0B00FAFAFA00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000D5D5D50000000000000000000000000004040400BDBDBD00000000000000
      00000000000000000000BCBCBC0004040400000000000000000000000000D7D7
      D7000000000000000000000000000000000000000000BCBCBC00B7B7B700B7B7
      B700B7B7B700B7B7B700B7B7B700B7B7B700B7B7B700ACACAC00414141000000
      000000000000000000000000000000000000000000000000000051515100B7B7
      B700B7B7B700B7B7B700BCBCBC00000000000000000000000000000000000000
      00000000000000000000000000000000000005050500D1D1D100C6C6C600B0B0
      B0007B7B7B00232323000000000000000000000000000000000030303000ABAB
      AB00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000086868600000000000000
      00000000000000000000C2C2C200000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00008C8C8C000000000000000000000000007272720000000000000000000000
      0000000000000000000000000000707070000000000000000000000000008E8E
      8E0000000000000000000000000000000000000000000F0F0F00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000F0F0F00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000151515008B8B8B00DBDBDB00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000CDCDCD00000000000000
      000000000000000000007E7E7E00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00006C6C6C00000000000000000000000000C1C1C10000000000000000000000
      0000000000000000000000000000C0C0C0000000000000000000000000006D6D
      6D0000000000000000000000000000000000000000000F0F0F00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000F0F0F00000000000000000000000000000000000000
      00000000000000000000000000000000000005050500BCBCBC00ACACAC008B8B
      8B00454545000101010000000000000000001B1B1B0072727200DADADA000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FEFEFE00161616000000
      0000000000000000000038383800000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00005F5F5F00000000000000000000000000D0D0D00000000000000000000000
      0000000000000000000000000000D0D0D0000000000000000000000000005F5F
      5F000000000000000000000000000000000000000000BCBCBC00B7B7B700B7B7
      B700B7B7B7009C9C9C0001010100000000000000000000000000000000000000
      0000000000001111110081818100B7B7B700B7B7B700B7B7B700B7B7B700B7B7
      B700B7B7B700B7B7B700BCBCBC00000000000000000000000000000000000000
      0000000000000000000000000000000000000606060000000000000000000000
      000000000000B9B9B9000B0B0B00000000000000000000000000060606008C8C
      8C00000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000005B5B5B000000
      0000000000000000000003030300EEEEEE000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00005F5F5F00000000000000000000000000D0D0D00000000000000000000000
      0000000000000000000000000000D0D0D0000000000000000000000000005F5F
      5F00000000000000000000000000000000000000000000000000000000000000
      0000000000005A5A5A0000000000000000000000000000000000000000000A0A
      0A0079797900EFEFEF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000606060000000000000000000000
      000000000000000000009D9D9D00000000000000000000000000000000000000
      0000A1A1A1000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000A1A1A1000000
      0000000000000000000000000000A9A9A9000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00005F5F5F00000000000000000000000000D0D0D00000000000000000000000
      0000000000000000000000000000D0D0D0000000000000000000000000005F5F
      5F00000000000000000000000000000000000000000000000000000000000000
      00000000000014141400000000000000000000000000000000004C4C4C00E7E7
      E700000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000606060000000000000000000000
      00000000000000000000F8F8F8000D0D0D000000000000000000000000000000
      00001E1E1E00FEFEFE0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000E6E6E6000000
      0000000000000000000000000000626262000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00005F5F5F00000000000000000000000000D0D0D00000000000000000000000
      0000000000000000000000000000D0D0D0000000000000000000000000005F5F
      5F00000000000000000000000000000000000000000000000000000000000000
      0000D9D9D900000000000000000000000000000000002F2F2F00FBFBFB000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000606060000000000000000000000
      00000000000000000000000000002E2E2E000000000000000000000000000000
      000000000000E0E0E00000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000002D2D
      2D000000000000000000000000001B1B1B000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00005F5F5F00000000000000000000000000D0D0D00000000000000000000000
      0000000000000000000000000000D0D0D0000000000000000000000000005F5F
      5F00000000000000000000000000000000000000000000000000000000000000
      0000B5B5B5000000000000000000000000000000000069696900000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000606060000000000000000000000
      00000000000000000000000000002D2D2D000000000000000000000000000000
      000000000000E8E8E80000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000006363
      630000000000000000000000000000000000D4D4D40000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00005F5F5F00000000000000000000000000D0D0D00000000000000000000000
      0000000000000000000000000000D0D0D0000000000000000000000000005F5F
      5F00000000000000000000000000000000000000000000000000000000000000
      0000E0E0E0000000000000000000000000000000000018181800F9F9F9000000
      000000000000000000000000000000000000F2F2F200A6A6A600FCFCFC000000
      0000000000000000000000000000000000000000000000000000F9F9F9000000
      0000000000000000000000000000000000000606060000000000000000000000
      00000000000000000000F8F8F8000D0D0D000000000000000000000000000000
      0000191919000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008F8F
      8F00000000000000000000000000000000008C8C8C0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00005F5F5F00000000000000000000000000D0D0D00000000000000000000000
      0000000000000000000000000000D0D0D0000000000000000000000000005F5F
      5F00000000000000000000000000000000000000000000000000000000000000
      0000000000001111110000000000000000000000000000000000424242009999
      9900D3D3D300BBBBBB0099999900535353000808080000000000ADADAD000000
      0000000000000000000000000000000000000000000000000000EBEBEB000000
      0000000000000000000000000000000000000606060000000000000000000000
      000000000000000000009D9D9D00000000000000000000000000000000000000
      0000888888000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FEFEFE005353
      5300000000000000000000000000000000000E0E0E00C2C2C200000000000000
      0000000000000000000000000000000000000000000000000000D3D3D300AAAA
      AA003F3F3F000000000000000000000000008B8B8B00AFAFAF00F4F4F4000000
      000000000000F4F4F400AEAEAE008B8B8B000000000000000000000000003F3F
      3F00AAAAAA00D3D3D30000000000000000000000000000000000000000000000
      0000000000007878780000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000434343000000
      0000000000000000000000000000000000000000000000000000A3A3A3000000
      0000000000000000000000000000000000000606060000000000000000000000
      000000000000B5B5B5000B0B0B00000000000000000000000000000000003D3D
      3D00F6F6F6000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000D3D3D3008F8F8F003D3D3D000000
      000000000000000000000000000000000000000000000A0A0A0068686800AAAA
      AA00F1F1F10000000000000000000000000000000000000000001F1F1F000000
      0000000000000000000000000000000000000000000000000000909090000000
      0000000000009090900000000000000000000000000000000000000000000000
      0000000000001F1F1F0000000000000000000000000000000000000000000000
      000000000000FBFBFB003B3B3B00000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000001010100D8D8
      D800000000000000000000000000000000007777770056565600040404000000
      00000000000000000000000000000000000004040400A1A1A100959595007878
      780039393900000000000000000000000000000000000000000056565600EFEF
      EF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000A6A6A6000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000018181800F9F9F90000000000000000000000000000000000151515000000
      0000000000000000000000000000000000000000000000000000878787000000
      0000000000008787870000000000000000000000000000000000000000000000
      0000000000001515150000000000000000000000000000000000000000000000
      00000000000000000000E9E9E900606060000202020000000000000000000000
      00000000000000000000000000000000000000000000060606004E4E4E00CFCF
      CF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000C0C0C0018181800242424003E3E3E006D6D6D00C5C5C500000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000959595000505050000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000036363600FCFCFC0000000000000000000000000000000000808080004444
      440044444400444444004444440044444400444444004A4A4A00CFCFCF000000
      000000000000CECECE0049494900444444004444440044444400444444004444
      4400444444008181810000000000000000000000000000000000000000000000
      000000000000000000000000000000000000D4D4D4006A6A6A00454545002323
      2300080808001E1E1E003E3E3E005E5E5E009A9A9A00EEEEEE00000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000060000000300000000100010000000000400200000000000000000000
      000000000000000000000000FFFFFF0000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000003FC0007F800001F800FF00000FC0
      007F800001F0003FC03807E000FF800001F0001FC07C03FC07FF800001F0001F
      C07E03FE07FFFFFFFFF0000FE07E01FE07FFFF81FFF1FC0FE07F01FF03FFFE00
      7FFFFC0FE07E01FF03FFFC003FFFFC0FE07E01FF03FFF8001FFFF80FE07C03FF
      03FFF8001FFFE00FE07803FF81FFF03C0F800001E0000FFF81FFF07E0F800001
      E0003FFF81FFF07E0F800001E0001FFF81FFF07E0F800001E0780FFFC0FFF07E
      0FF803FFE07C07FFC0FFF07E0FF80FFFE07C03FFC0FFF07E0FF01FFFE07E03FF
      E0FFF07E0FF03FFFE07E03FFE07FF07E0FF01F1FC07C07FFE07FF07E0FF8001F
      C07C07FFC03FC01803F8001FC07807FF0007C01803F8000F00000FFE0003C018
      03FC000F00003FFE0003C01803FF003F00000000000000000000000000000000
      000000000000}
  end
  object PopupMenu1: TPopupMenu
    Left = 313
    Top = 249
    object Consolas1: TMenuItem
      Caption = 'Consolas'
    end
    object Consolas2: TMenuItem
      Caption = 'Arial'
    end
    object imeNewRoman1: TMenuItem
      Caption = 'Time New Roman'
    end
    object imeNewRoman2: TMenuItem
      Caption = 'WebDings'
    end
  end
  object JvPopupMenu1: TJvPopupMenu
    Style = msItemPainter
    ImageMargin.Left = 0
    ImageMargin.Top = 0
    ImageMargin.Right = 0
    ImageMargin.Bottom = 0
    ImageSize.Height = 32
    ImageSize.Width = 32
    OnDrawItem = JvPopupMenu1DrawItem
    Left = 109
    Top = 288
    object New1: TMenuItem
      Caption = 'New ...'
      object Project1: TMenuItem
        Caption = 'Project'
      end
      object Project2: TMenuItem
        Caption = '-'
      end
      object AssistentTemplate1: TMenuItem
        Caption = 'Assistent Template'
      end
    end
    object Open1: TMenuItem
      Caption = 'Open'
      OnClick = Open1Click
      OnMeasureItem = Open1MeasureItem
    end
    object SaveAs1: TMenuItem
      Caption = 'Save As ...'
    end
    object Save1: TMenuItem
      Caption = 'Save'
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object Print1: TMenuItem
      Caption = 'Print'
    end
    object PrinterSetup1: TMenuItem
      Caption = 'Printer Setup'
      OnClick = PrinterSetup1Click
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object Exit1: TMenuItem
      Caption = 'Exit'
      ShortCut = 32883
      OnClick = Exit1Click
    end
  end
  object ImageList2: TImageList
    Height = 48
    Width = 48
    Left = 155
    Top = 373
    Bitmap = {
      494C010106000800040030003000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000C00000006000000001002000000000000020
      0100000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000500000007000000070000000700000007000000070000
      0007000000070000000700000007000000070000000700000007000000070000
      0007000000070000000700000007000000070000000700000007000000050000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000003B000000980000
      00D9000000FD000000FF000000FF000000FF000000FF000000FF000000FF0000
      00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
      00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
      00FD000000DA000000980000003B000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000004000000CA000000FF0000
      00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
      00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
      00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
      00FF000000FF000000FF000000CA000000040000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000100000027000000FF000000FF0000
      00FF000000F5000000DE000000D8000000D9000000D9000000D9000000D90000
      00D9000000D9000000D9000000D9000000D9000000D9000000D9000000D90000
      00D9000000D9000000D9000000D9000000D9000000D9000000D8000000DE0000
      00F5000000FF000000FF000000FF000000270000000100000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000001F0000001F000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000700000031000000070000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000060000008A000000FF000000FB0000
      00B60000004A0000002B0000002A0000002A0000002A0000002A0000002A0000
      002A0000002A0000002A0000002A0000002A0000002A0000002A0000002A0000
      002A0000002A0000002A0000002A0000002A0000002A0000002A0000002B0000
      004A000000B5000000FB000000FF0000008A0000000600000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000003F000000FB000000FB000000530000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000BB000000FF000000CB0000
      0015000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000013000000DD000000FF000000C10000
      002D000000030000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00030000002D000000C1000000FF000000DC0000001300000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000007B000000FF000000FF000000F40000002F00000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000FE000000FF000000FF0000
      00D50000001A0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000100000039000000F0000000F10000007E0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000007E000000F1000000F00000003900000001000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000071000000FF000000FF000000FF000000E00000001D000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000FF000000FF000000FF0000
      00FF000000DB0000001F00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000010000004F000000F2000000E6000000600000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000060000000E6000000F20000004F00000001000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000066000000FF000000FF000000FF000000FF000000DB0000001A0000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000FF000000FF000000FF0000
      00FF000000FF000000E100000025000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000010000004F000000F2000000E6000000600000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000060000000E6000000F20000004F00000001000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000005A000000FF000000FF000000FF000000FF000000FF000000DC0000
      0029000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000FF000000FF000000FF0000
      00F7000000FF000000FF000000E70000002C0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000010000004F000000F2000000E6000000600000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000060000000E6000000F20000004F00000001000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000004C000000FF000000FF000000EA000000FA000000FF000000FF0000
      00F0000000580000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000FF000000FF000000FF0000
      0043000000F4000000FF000000FF000000EC0000003300000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000010000004F000000F2000000E6000000600000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000060000000E6000000F20000004F00000001000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000029000000FF000000FF000000DE0000004C000000F2000000FF0000
      00FF000000FF000000BA00000033000000000000000000000000000000000000
      000000000000000000000000000000000000000000FF000000FF000000FF0000
      00000000003B000000F0000000FF000000FF000000F00000003B000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000010000004F000000F2000000E6000000600000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000060000000E6000000F20000004F00000001000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000005000000FA000000FF000000FD000000090000002D000000DF0000
      00FF000000FF000000FF000000FF000000C50000006600000023000000000000
      000000000000000000000000000000000000000000FF000000FF000000FF0000
      00000000000000000033000000EC000000FF000000FF000000F4000000430000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000010000004F000000F2000000E6000000600000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000060000000E6000000F20000004F00000001000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000D8000000FF000000FF0000002F000000000000000D0000
      008E000000FB000000FF000000FF000000FF000000FF000000FF000000E10000
      00B4000000920000006F0000005D00000055000000FF000000FF000000FF0000
      000000000000000000000000002C000000E7000000FF000000FF000000F70000
      004C000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000010000004F000000F2000000E6000000600000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000060000000E6000000F20000004F00000001000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000B0000000FF000000FF0000006100000000000000000000
      00000000002D000000A0000000F8000000FF000000FF000000FF000000FF0000
      00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
      000000000000000000000000000000000025000000E1000000FF000000FF0000
      00FA000000560000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000010000004F000000F2000000E6000000600000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000060000000E6000000F20000004F00000001000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000075000000FF000000FF000000A400000000000000000000
      00000000000000000000000000150000006E000000B2000000F1000000FF0000
      00FF000000FF000000FF000000FF000000FF000000FF000000FF000000F00000
      0000000000000000000000000000000000000000001F000000DB000000FF0000
      00FF000000FC0000006000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000010000004F000000F2000000E6000000600000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000060000000E6000000F20000004F00000001000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000032000000FF000000FF000000ED00000006000000000000
      00000000000000000000000000000000000000000000000000040000002E0000
      00510000007400000093000000A3000000AA000000B8000000B8000000500000
      000000000000000000000000000000000000000000000000001A000000D50000
      00FF000000FF000000FE0000006A000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000010000004F000000F2000000E6000000600000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000060000000E6000000F20000004F00000001000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000001000000DE000000FF000000FF00000050000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000150000
      00CE000000FF000000FF000000FE000000760000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000010000004F000000F2000000E6000000600000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000060000000E6000000F20000004F00000001000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000084000000FF000000FF000000C0000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0010000000C6000000FF000000FF000000FF0000008100000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000010000004F000000F2000000E6000000600000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000060000000E6000000F20000004F00000001000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000001D000000F9000000FF000000FF000000400000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000C000000BE000000FF000000FF000000FF0000008C000000010000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000010000004F000000F2000000E6000000600000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000060000000E6000000F20000004F00000001000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000009D000000FF000000FF000000D40000
      0006000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000009000000B5000000FF000000FF000000FF000000970000
      0002000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000010000004F000000F2000000E6000000600000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000060000000E6000000F20000004F00000001000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000001A000000F1000000FF000000FF0000
      0091000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000006000000AC000000FF000000FF000000FF0000
      005D000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000010000004F000000F2000000E6000000600000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000060000000E6000000F20000004F00000001000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000006B000000FF000000FF0000
      00FF0000006A0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000006000000AC000000FF000000FF000000FF0000
      005D000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000010000004F000000F2000000E6000000600000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000060000000E6000000F20000004F00000001000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000001000000AA000000FF0000
      00FF000000FE0000007700000001000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000009000000B5000000FF000000FF000000FF000000970000
      0002000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000010000004F000000F2000000E6000000600000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000060000000E6000000F20000004F00000001000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000C000000C40000
      00FF000000FF000000FF000000B00000001C0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000C000000BE000000FF000000FF000000FF0000008C000000010000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000010000004F000000F2000000E6000000600000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0007000000070000000700000007000000070000000700000007000000070000
      00070000000500000065000000E7000000F20000004F00000001000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000000D0000
      00BF000000FF000000FF000000FF000000F5000000880000001F000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0010000000C6000000FF000000FF000000FF0000008100000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000010000004F000000F2000000E6000000600000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000001900000082000000D5000000FE0000
      00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
      00FF000000FF000000FF000000FF000000F10000003C00000001000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00050000008C000000FE000000FF000000FF000000FF000000FD000000BD0000
      0077000000380000001000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000150000
      00CE000000FF000000FF000000FE000000760000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000010000004F000000F2000000E6000000600000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000A00000092000000F6000000FF000000FF0000
      00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
      00FF000000FF000000FF000000FF000000DA0000001700000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000003D000000CB000000FF000000FF000000FF000000FF0000
      00FF000000FF000000FF000000EA000000D6000000CC000000BA000000500000
      000000000000000000000000000000000000000000000000001A000000D50000
      00FF000000FF000000FE0000006A000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000010000004F000000F2000000E6000000600000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000004F000000E8000000FF000000FF000000F80000
      00D8000000D9000000D9000000D9000000D9000000D9000000D8000000DD0000
      00F3000000FF000000FF000000FF000000770000000500000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000010000004E000000B5000000FD000000FF0000
      00FF000000FF000000FF000000FF000000FF000000FF000000FF000000F00000
      0000000000000000000000000000000000000000001F000000DB000000FF0000
      00FF000000FC0000006000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000010000004F000000F2000000E6000000600000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000009B000000F8000000FF000000E80000006C0000
      002A0000002A0000002A0000002A0000002A0000002A000000310000007F0000
      00F3000000FF000000FF000000FD000000190000000100000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000180000005C0000
      00A0000000CF000000F5000000FF000000FF000000FF000000FF000000FF0000
      000000000000000000000000000000000025000000E1000000FF000000FF0000
      00FA000000560000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000010000004F000000F2000000E6000000600000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000C0000000FD000000FE000000CD0000000A0000
      0000000000000000000000000000000000000000000000000051000000DE0000
      00FE000000FF000000FC0000009A000000010000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000010000001B00000028000000FF000000FF000000FF0000
      000000000000000000000000002C000000E7000000FF000000FF000000F70000
      004C000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000010000004F000000F2000000E6000000600000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000C6000000FE000000FE000000C7000000000000
      00000000000000000000000000000000000000000008000000E1000000FE0000
      00FF000000FA0000009C00000020000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000FF000000FF000000FF0000
      00000000000000000032000000EB000000FF000000FF000000F5000000460000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000010000004F000000F2000000E6000000600000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000C7000000FE000000FE000000C7000000000000
      000000000000000000000000000700000051000000E1000000FF000000F60000
      00A0000000230000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000FF000000FF000000FF0000
      000000000035000000ED000000FF000000FF000000F300000041000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000010000004F000000F2000000E6000000600000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000C7000000FE000000FE000000C7000000000000
      0000000000000000000600000055000000DF000000FE000000F6000000A00000
      0024000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000FF000000FF000000FF0000
      003F000000F2000000FF000000FF000000EE0000003700000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000010000004F000000F2000000E6000000600000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000C7000000FE000000FE000000C7000000000000
      00000000000500000059000000DB000000FE000000FF000000A0000000240000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000FF000000FF000000FF0000
      00F7000000FF000000FF000000E70000002C0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000010000004F000000F2000000E6000000600000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000C7000000FE000000FE000000C7000000000000
      00060000005B000000D9000000FE000000FF000000FA00000023000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000FF000000FF000000FF0000
      00FF000000FF000000E200000026000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000010000004F000000F2000000E6000000600000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000C7000000FE000000FE000000C7000000000000
      005A000000D9000000FF000000FF000000FC0000009C00000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000FF000000FF000000FF0000
      00FF000000DB0000001F00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000010000004F000000F2000000E6000000600000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000C7000000FE000000FE000000C7000000060000
      00DB000000FF000000FF000000FD0000009A0000002000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000FE000000FF000000FF0000
      00D50000001A0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000100000039000000F0000000F10000007E0000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000C7000000FE000000FE000000F3000000E00000
      00FF000000FA0000009C00000020000000010000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000BB000000FF000000CB0000
      0015000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000013000000DD000000FF000000C10000
      002D000000030000000000000000000000000000000000000000000000000000
      00000000000000000000000000C7000000FE000000FF000000FF000000FE0000
      00F80000009E0000002100000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000700000031000000070000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000060000008A000000FF000000FB0000
      00B60000004A0000002B0000002A0000002A0000002A0000002A0000002A0000
      002A0000002A00000029000000D0000000FE000000FF000000FF000000FF0000
      009F000000220000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000100000027000000FF000000FF0000
      00FF000000F5000000DE000000D8000000D9000000D9000000D9000000D90000
      00D9000000D9000000D8000000F7000000FF000000FF000000FF000000F60000
      0023000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000004000000CA000000FF0000
      00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
      00FF000000FF000000FF000000FF000000FF000000FF000000F70000009F0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000003B000000980000
      00DA000000FD000000FF000000FF000000FF000000FF000000FF000000FF0000
      00FF000000FF000000FF000000FF000000FF000000D800000087000000220000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000500000007000000070000000700000007000000070000
      0007000000070000000700000007000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000030000004100000070000000870000
      0088000000880000008800000088000000880000008800000088000000880000
      0088000000880000008800000088000000880000008800000088000000880000
      007F0000005F0000001900000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000027000000C9000000FF000000FF000000FF0000
      00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
      00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
      00FF000000FF000000FA0000008A000000040000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000028000000F4000000FF000000FF000000FF000000FF0000
      00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
      00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
      00FF000000FF000000FF000000FF0000009C0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000002000000C4000000FF000000FF000000F200000096000000780000
      0077000000770000007700000077000000770000007700000077000000770000
      0077000000770000007700000077000000770000007700000077000000770000
      0080000000C2000000FF000000FF000000FE0000004600000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000003D000000FF000000FF000000F40000002800000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000009B000000FF000000FF000000BB00000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000070000000FF000000FF000000980000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000001A000000FF000000FF000000EF00000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000700000031000000070000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000001F0000001F0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000900000039000000230000
      0002000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000007F000000FF000000FF000000800000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000FF000000FF000000FF00000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000002C0000
      006B000000840000008800000088000000880000008800000088000000880000
      0088000000880000008800000088000000880000008800000088000000880000
      0088000000880000008800000088000000840000006B0000002C000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000015000000CB000000FF000000BB0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000053000000FB000000FB0000
      003F000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000032000000AC000000F5000000FF000000FF0000
      00E8000000910000001800000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000080000000FF000000FF000000800000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000FF000000FF000000FF00000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000E0000009B000000FF0000
      00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
      00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
      00FF000000FF000000FF000000FF000000FF000000FF000000FE0000009B0000
      000E000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      001A000000D5000000FF000000FF000000FE0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000031000000F4000000FF000000FF0000
      007B000000000000000000000000000000000000000000000000000000000000
      00000000008F000000FF000000FF000000FF000000FF000000FF000000FF0000
      00FF000000FF000000FF000000FD000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000080000000FF000000FF000000800000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000FF000000FF000000FF00000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000003000000C4000000FF000000FF0000
      00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
      00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
      00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
      00C2000000020000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000001F0000
      00DB000000FF000000FF000000FF000000FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000001E000000E2000000FF000000FF000000FF0000
      0071000000000000000000000000000000000000000000000000000000000000
      004D000000FF000000FF000000FF000000FF000000B3000000460000006C0000
      00D1000000FF000000FF000000FF000000180000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000080000000FF000000FF000000800000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000FF000000FF000000FF00000088000000880000
      0088000000880000008800000070000000410000000300000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000070000000FF000000FF000000FE0000
      00AB0000007B0000007700000077000000770000007700000077000000770000
      0077000000770000007700000077000000770000007700000077000000770000
      00770000007700000077000000770000007B000000AC000000FE000000FF0000
      00FF0000006D0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000021000000DE0000
      00FF000000FF000000FF000000FF000000FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000017000000D7000000FF000000FF000000FF000000FF0000
      0064000000000000000000000000000000000000000000000000000000000000
      00D3000000FF000000FF000000FD0000005E0000000000000000000000000000
      00040000009B000000FF000000FF000000910000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000080000000FF000000FF000000800000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000FF000000FF000000FF000000FF000000FF0000
      00FF000000FF000000FF000000FF000000FF000000D400000035000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000D3000000FF000000FF000000710000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000072000000FF0000
      00FF000000D10000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000002C000000E7000000FF0000
      00FF000000F7000000FF000000FF000000FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000002B000000DD000000FF000000FF000000FF000000FF000000FF0000
      0059000000000000000000000000000000000000000000000000000000000000
      00FF000000FF000000FF00000092000000000000000000000000000000000000
      000000000004000000D1000000FF000000E80000000200000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000080000000FF000000FF000000800000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000FF000000FF000000FF000000FF000000FF0000
      00FF000000FF000000FF000000FF000000FF000000FF000000F5000000270000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000F8000000FF000000FF000000090000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000A000000FF0000
      00FF000000F80000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000033000000EC000000FF000000FF0000
      00F400000043000000FF000000FF000000FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      005B000000F1000000FF000000FF000000F9000000EA000000FF000000FF0000
      004A000000000000000000000000000000000000000000000000000000000000
      00FF000000FF000000FF0000002A000000000000000000000000000000000000
      0000000000000000006C000000FF000000FF0000002300000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000080000000FF000000FF000000800000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000FF000000FF000000FF00000077000000770000
      0077000000770000007700000096000000EF000000FF000000FF000000C30000
      0002000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000FF000000FF000000FF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000FF0000
      00FF000000FF0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000003B000000F0000000FF000000FF000000F00000
      003B00000000000000FF000000FF000000FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000034000000BB0000
      00FF000000FF000000FF000000F10000004A000000E0000000FF000000FF0000
      0027000000000000000000000000000000000000000000000000000000000000
      00FF000000FF000000FF00000006000000000000000000000000000000000000
      00000000000000000047000000FF000000FF0000003900000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000001000000190000003600000040000000310000000D000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000080000000FF000000FF000000800000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000FF000000FF000000FF00000000000000000000
      000000000000000000000000000000000027000000F4000000FF000000FF0000
      003B000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000FF000000FF000000FF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000FF0000
      00FF000000FF0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000043000000F4000000FF000000FF000000EC000000330000
      000000000000000000FF000000FF000000FF0000000000000000000000000000
      000000000000000000000000002300000067000000C7000000FF000000FF0000
      00FF000000FF000000DC0000002B0000000B000000FE000000FF000000F90000
      0004000000000000000000000000000000000000000000000000000000000000
      00FF000000FF000000FF00000070000000000000000000000000000000000000
      000000000000000000E3000000FF000000FC0000002900000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000030000004C0000009E000000FF000000FF0000
      00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
      00EF000000630000000000000000000000000000000000000000000000000000
      000000000080000000FF000000FF000000800000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000FF000000FF000000FF00000000000000000000
      00000000000000000000000000000000000000000099000000FF000000FF0000
      006F000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000FF000000FF000000FF000000000000
      00000000000000000000000000000000000000000086000000F7000000910000
      0002000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000FF0000
      00FF000000FF0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000004C000000F7000000FF000000FF000000E70000002C000000000000
      000000000000000000FF000000FF000000FF000000550000005D000000700000
      0092000000B4000000E1000000FF000000FF000000FF000000FF000000FF0000
      00FB0000008B0000000B0000000000000031000000FF000000FF000000D60000
      0000000000000000000000000000000000000000000000000000000000000000
      00EA000000FF000000FF000000EF0000002C0000000000000000000000000000
      000000000074000000FF000000FF000000FF000000F8000000890000000E0000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000001500000079000000E0000000FF000000FF000000FF000000FF0000
      00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
      00FF000000E50000000000000000000000000000000000000000000000000000
      000000000080000000FF000000FF000000800000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000FF000000FF000000FF00000000000000000000
      00000000000000000000000000000000000000000080000000FF000000FF0000
      007F000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000F9000000FF000000F9000000000000
      000000000000000000000000000000000000000000F4000000FF000000FF0000
      009D000000020000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000FF0000
      00FF000000FF0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0056000000FA000000FF000000FF000000E10000002500000000000000000000
      000000000000000000FF000000FF000000FF000000FF000000FF000000FF0000
      00FF000000FF000000FF000000FF000000FF000000FF000000F70000009E0000
      002B00000000000000000000000000000063000000FF000000FF000000AD0000
      0000000000000000000000000000000000000000000000000000000000000000
      0072000000FF000000FF000000FF000000F000000071000000050000002A0000
      0093000000FE000000FF000000FF000000FF000000FF000000FF000000E90000
      0068000000040000000000000000000000000000000000000000000000000000
      0096000000F7000000FF000000FF000000FF000000FF000000FF000000FE0000
      00D0000000B9000000F9000000FF000000FF000000FF000000FF000000F30000
      007D0000000B0000000000000000000000000000000000000000000000000000
      000000000080000000FF000000FF000000800000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000FF000000FF000000FF00000000000000000000
      00000000000000000000000000000000000000000080000000FF000000FF0000
      0080000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000086000000F700000086000000000000
      00000000000000000000000000000000000000000092000000FF000000FF0000
      00FF0000009D0000000200000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000FF0000
      00FF000000FF0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000600000
      00FC000000FF000000FF000000DB0000001F0000000000000000000000000000
      000000000000000000F0000000FF000000FF000000FF000000FF000000FF0000
      00FF000000FF000000FF000000F0000000B10000006D00000014000000000000
      0000000000000000000000000000000000A7000000FF000000FF000000720000
      0000000000000000000000000000000000000000000000000000000000000000
      0005000000C0000000FF000000FF000000FF000000FF000000FF000000FF0000
      00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
      00FF000000D4000000000000000000000000000000000000002D000000A40000
      00FF000000FF000000FF000000FF000000FF000000FA0000006A0000002B0000
      0089000000F7000000FF000000FF000000FF000000FB00000098000000180000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000080000000FF000000FF000000800000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000FF000000FF000000FF00000000000000000000
      00000000000000000000000000000000000000000080000000FF000000FF0000
      0080000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000100000097000000FF0000
      00FF000000FF0000009E00000002000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000FF0000
      00FF000000FF0000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000006A000000FE0000
      00FF000000FF000000D50000001A000000000000000000000000000000000000
      00000000000000000050000000B8000000B8000000AA000000A3000000930000
      0073000000510000002D00000003000000000000000000000000000000000000
      0000000000000000000000000008000000EF000000FF000000FF0000002E0000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000012000000C0000000FF000000FF000000FF000000FF000000FF0000
      00FF000000FF000000FF000000F4000000FF000000FF000000FF000000FF0000
      00FF000000FF000000B70000002A0000000000000023000000DA000000FF0000
      00FF000000FF000000FF000000FF000000A0000000170000006F000000EC0000
      00FF000000FF000000FF000000FF000000B20000002900000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000080000000FF000000FF000000800000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000FF000000FF000000FF00000000000000000000
      00000000000000000000000000000000000000000080000000FF000000FF0000
      0080000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000001000000970000
      00FF000000FF000000FF0000009E000000020000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000FF0000
      00FF000000FF0000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000076000000FE000000FF0000
      00FF000000CE0000001500000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000054000000FF000000FF000000DA000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000600000073000000EA000000FF000000FF000000FF0000
      00FF000000D30000004D00000001000000D7000000FF000000FF000000FF0000
      00FF000000FF000000FF000000FB000000960000001500000006000000750000
      00FF000000FF000000FF000000FF000000F90000009A000000FF000000FF0000
      00FF000000FF000000FF000000CB000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000080000000FF000000FF000000800000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000FF000000FF000000FF00000000000000000000
      00000000000000000000000000000000000000000080000000FF000000FF0000
      0080000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000010000
      0097000000FF000000FF000000FF0000009E0000000200000000000000000000
      0000000000000000000000000000000000000000000000000000000000FF0000
      00FF000000FF0000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000081000000FF000000FF000000FF0000
      00C6000000100000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000C4000000FF000000FF00000080000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000059000000FF0000
      00FF000000FF000000FF000000FF000000FF000000FF000000FF000000F70000
      000100000031000000C1000000FF000000FF000000FF000000FF000000EE0000
      0074000000070000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000080000000FF000000FF000000800000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000160000003F00000044000000440000004400000044000000440000
      00440000004400000044000000FF000000FF000000FF00000000000000000000
      00000000000000000000000000000000000000000080000000FF000000FF0000
      0080000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000100000097000000FF000000FF000000FF0000009E00000002000000000000
      0000000000000000000000000000000000000000000000000000000000FF0000
      00FF000000FF0000000000000000000000000000000000000000000000000000
      000000000000000000010000008C000000FF000000FF000000FF000000BE0000
      000C000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000043000000FF000000FF000000F80000001A000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000B00000
      00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
      00CA000000360000000000000055000000DF000000FF0000008F000000120000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000080000000FF000000FF000000800000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000001F0000
      00BA000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
      00FF000000FF000000FF000000FF000000FF000000FF00000000000000000000
      00000000000000000000000000000000000000000080000000FF000000FF0000
      0080000000000000000000000000000000000000000000000000000000000000
      00240000007E0000008800000088000000880000008800000088000000880000
      0088000000880000008800000088000000880000008800000088000000880000
      0088000000880000008800000088000000880000008800000088000000880000
      00880000008A000000F0000000FF000000FF000000FF0000009D000000020000
      0000000000000000000000000000000000000000000000000000000000FF0000
      00FF000000FF0000000000000000000000000000000000000000000000000000
      00000000000200000097000000FF000000FF000000FF000000B5000000090000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000006000000D6000000FF000000FF0000009900000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000560000
      00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
      00FF000000FE000000A40000001D000000080000006A00000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000080000000FF000000FF000000800000000000000000000000000000
      000000000000000000000000000000000000000000000000000C000000DC0000
      00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
      00FF000000FF000000FF000000FF000000FF000000FE00000000000000000000
      00000000000000000000000000000000000000000080000000FF000000FF0000
      0080000000000000000000000000000000000000000000000000000000000000
      00DD000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
      00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
      00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
      00FF000000FF000000FF000000FF000000FF000000FF000000FF0000005D0000
      0000000000000000000000000000000000000000000000000000000000FF0000
      00FF000000FF0000000000000000000000000000000000000000000000000000
      00000000005D000000FF000000FF000000FF000000AC00000006000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000094000000FF000000FF000000F00000001800000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000001A0000
      00FF000000FF000000FF000000C80000002000000030000000E7000000FF0000
      00FF000000FF000000FF000000F6000000830000000C00000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000080000000FF000000FF000000800000000000000000000000000000
      0000000000000000000000000000000000000000000000000073000000FF0000
      00FF000000F9000000C0000000BB000000BB000000BB000000BB000000BB0000
      00BB000000C0000000F2000000FF000000FF000000D000000000000000000000
      00000000000000000000000000000000000000000080000000FF000000FF0000
      0080000000000000000000000000000000000000000000000000000000000000
      00DD000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
      00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
      00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
      00FF000000FF000000FF000000FF000000FF000000FF000000FF0000005D0000
      0000000000000000000000000000000000000000000000000000000000FF0000
      00FF000000FF0000000000000000000000000000000000000000000000000000
      00000000005D000000FF000000FF000000FF000000AC00000006000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      006D000000FF000000FF000000FF0000006A0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00E7000000FF000000FF00000050000000000000000000000051000000FF0000
      00FF000000FF000000FF000000FF000000FF000000E600000002000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000080000000FF000000FF000000800000000000000000000000000000
      00000000000000000000000000000000000000000000000000AD000000FF0000
      00FF000000610000000000000000000000000000000000000000000000000000
      0011000000CD000000FF000000FF000000FB0000003400000000000000000000
      00000000000000000000000000000000000000000080000000FF000000FF0000
      0080000000000000000000000000000000000000000000000000000000000000
      0024000000750000007700000077000000770000007700000077000000770000
      0077000000770000007700000077000000770000007700000077000000770000
      0077000000770000007700000077000000770000007700000077000000770000
      007700000079000000EA000000FF000000FF000000FF0000009D000000020000
      0000000000000000000000000000000000000000000000000000000000FF0000
      00FF000000FF0000000000000000000000000000000000000000000000000000
      00000000000200000097000000FF000000FF000000FF000000B5000000090000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000001000000790000
      00FE000000FF000000FF000000A7000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00A3000000FF000000FF0000009F00000000000000000000007A000000FF0000
      00FF000000D2000000FF000000FF000000FF000000FF000000CF000000420000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000080000000FF000000FF000000800000000000000000000000000000
      00000000000000000000000000000000000000000000000000BF000000FF0000
      00FF0000004000000000000000000000000000000000000000000000000F0000
      00C9000000FF000000FF000000FD0000005D0000000000000000000000000000
      00000000000000000000000000000000000000000080000000FF000000FF0000
      0080000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000100000097000000FF000000FF000000FF0000009E00000002000000000000
      0000000000000000000000000000000000000000000000000000000000FF0000
      00FF000000FF0000000000000000000000000000000000000000000000000000
      000000000000000000010000008C000000FF000000FF000000FF000000BE0000
      000C000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000001D000000B2000000FF0000
      00FF000000FF000000C20000000B000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000500000070000000E9000000FF000000FF000000FF0000
      00FF000000D10000004A00000002000000AA000000FB000000FF000000800000
      0000000000BE000000FF000000FF000000FF000000FF000000FF000000FF0000
      005E00000000000000000000000B00000080000000F4000000FF000000FF0000
      00FF000000FA0000009100000012000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000080000000FF000000FF000000800000000000000000000000000000
      00000000000000000000000000000000000000000000000000BF000000FF0000
      00FF00000040000000000000000000000000000000000000000E000000C80000
      00FF000000FF000000FE00000066000000000000000000000000000000000000
      00000000000000000000000000000000000000000080000000FF000000FF0000
      0080000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000010000
      0097000000FF000000FF000000FF0000009E0000000200000000000000000000
      0000000000000000000000000000000000000000000000000000000000FF0000
      00FF000000FF0000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000081000000FF000000FF000000FF0000
      00C6000000100000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000002000000089000000F5000000FF000000FF0000
      00FF000000BE0000000C00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000011000000BE000000FF000000FF000000FF000000FF000000FF0000
      00FF000000FF000000FF000000F1000000FF000000FF000000FF000000F70000
      002600000004000000FF000000FF000000FF000000FF000000FF000000FF0000
      003D000000010000000000000000000000000000001B000000FD000000FF0000
      00FF000000FF000000FF000000ED000000050000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000080000000FF000000FF000000800000000000000000000000000000
      00000000000000000000000000000000000000000000000000BF000000FF0000
      00FF000000400000000000000000000000000000000B000000C1000000FF0000
      00FF000000FE0000006800000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000080000000FF000000FF0000
      0080000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000001000000970000
      00FF000000FF000000FF0000009E000000020000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000FF0000
      00FF000000FF0000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000076000000FE000000FF0000
      00FF000000CE0000001500000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000100000
      003900000078000000BE000000FD000000FF000000FF000000FF000000FE0000
      0089000000040000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0005000000BD000000FF000000FF000000FF000000FF000000FF000000FF0000
      00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
      00E0000000260000008F000000F9000000FF000000FF000000FF000000FF0000
      00FF000000E0000000850000002D000000000000000000000033000000C10000
      00FF000000FF000000FF000000FF000000DA0000005000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000080000000FF000000FF000000800000000000000000000000000000
      00000000000000000000000000000000000000000000000000BF000000FF0000
      00FF0000004000000000000000000000000A000000BF000000FF000000FF0000
      00FE0000006A0000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000080000000FF000000FF0000
      0080000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000100000097000000FF0000
      00FF000000FF0000009E00000002000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000FF0000
      00FF000000FF0000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000006A000000FE0000
      00FF000000FF000000D50000001A000000000000000000000000000000000000
      00000000000000000050000000BA000000CC000000D6000000EA000000FF0000
      00FF000000FF000000FF000000FF000000FF000000FF000000C90000003B0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0071000000FF000000FF000000FF000000F000000073000000060000002C0000
      009E000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
      00AD0000001D000000000000001100000080000000EC000000FF000000FF0000
      00FF000000FF000000FF000000FF000000E1000000990000000D000000000000
      0052000000DB000000FF000000FF000000FF000000FF000000BF000000310000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000080000000FF000000FF000000800000000000000000000000000000
      00000000000000000000000000000000000000000000000000BF000000FF0000
      00FF00000040000000000000000A000000BD000000FF000000FF000000FE0000
      006C0000000000000000000000050000002E0000004400000044000000440000
      004400000044000000440000004400000044000000A2000000FF000000FF0000
      0080000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000086000000F700000086000000000000
      00000000000000000000000000000000000000000092000000FF000000FF0000
      00FF0000009D0000000200000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000FF0000
      00FF000000FF0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000600000
      00FC000000FF000000FF000000DB0000001F0000000000000000000000000000
      000000000000000000F0000000FF000000FF000000FF000000FF000000FF0000
      00FF000000FF000000FF000000FD000000B40000004C00000001000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00E9000000FF000000FF000000F00000002D0000000000000000000000000000
      00000000008A000000FF000000FF000000FF000000FF000000DB000000480000
      0000000000000000000000000000000000000000000B00000066000000C20000
      00FF000000FF000000FF000000FF000000FF000000FF000000FA000000C20000
      00830000004C0000007F000000EF000000FF000000FF000000FF000000FD0000
      009E0000001A0000000000000000000000000000000000000000000000000000
      00000000007F000000FF000000FF000000800000000000000000000000000000
      00000000000000000000000000000000000000000000000000BF000000FF0000
      00FF0000004000000009000000BB000000FF000000FF000000FF000000700000
      00000000000100000069000000F0000000FF000000FF000000FF000000FF0000
      00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
      009F000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000F9000000FF000000F9000000000000
      000000000000000000000000000000000000000000F4000000FF000000FF0000
      009D000000020000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000FF0000
      00FF000000FF0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0056000000FA000000FF000000FF000000E10000002500000000000000000000
      000000000000000000FF000000FF000000FF000000FF000000FF000000F50000
      00CF0000009F0000005C00000018000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00FF000000FF000000FF00000071000000000000000000000000000000000000
      000000000000000000DC000000FF000000FF0000008400000008000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0079000000D4000000FF000000FF000000FF000000FF000000FF000000FF0000
      00FF000000FF000000FC000000EF000000FF000000FF000000FF000000FF0000
      00FF000000F40000000000000000000000000000000000000000000000000000
      000000000070000000FF000000FF000000990000000000000000000000000000
      00000000000000000000000000000000000000000000000000BF000000FF0000
      00FF00000046000000B8000000FF000000FF000000FF00000074000000000000
      000000000068000000FF000000FF000000FF000000FF000000FF000000FF0000
      00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
      00AF000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000FF000000FF000000FF000000000000
      00000000000000000000000000000000000000000086000000F7000000910000
      0002000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000FF0000
      00FF000000FF0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000004C000000F7000000FF000000FF000000E70000002C000000000000
      000000000000000000FF000000FF000000FF000000280000001B000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00FF000000FF000000FF00000006000000000000000000000000000000000000
      00000000000000000047000000FF000000FF0000003900000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000E00000099000000D60000
      00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
      00FF000000840000000000000000000000000000000000000000000000000000
      00000000003C000000FF000000FF000000F20000002900000000000000000000
      00000000000000000000000000000000000000000000000000BF000000FF0000
      00FF000000C8000000FF000000FF000000FF0000007800000000000000000000
      0004000000EF000000FF000000FF000000DC000000BB000000BB000000BB0000
      00BB000000BB000000BB000000BB000000D6000000FF000000FF000000FF0000
      0046000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000FF000000FF000000FF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000FF0000
      00FF000000FF0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000040000000F2000000FF000000FF000000EE000000370000
      000000000000000000FF000000FF000000FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00FF000000FF000000FF0000002A000000000000000000000000000000000000
      0000000000000000006B000000FF000000FF0000002300000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00160000005100000083000000A9000000DD000000DD000000CE000000950000
      0047000000000000000000000000000000000000000000000000000000000000
      000000000002000000D0000000FF000000FF000000F200000099000000880000
      00880000008800000088000000880000008800000088000000E1000000FF0000
      00FF000000FF000000FF000000FF0000007D0000000000000000000000000000
      002E000000FF000000FF000000DC000000050000000000000000000000000000
      0000000000000000000000000060000000FD000000FF000000FF000000B00000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000FF000000FF000000FF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000FF0000
      00FF000000FF0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000003B000000F0000000FF000000FF000000F00000
      003B00000000000000FF000000FF000000FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00FF000000FF000000FF00000091000000000000000000000000000000000000
      000000000003000000D0000000FF000000E90000000200000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000030000000F2000000FF000000FF000000FF000000FF0000
      00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
      00FF000000FF000000FF00000081000000000000000000000000000000000000
      0040000000FF000000FF000000BF000000000000000000000000000000000000
      00000000000000000056000000FC000000FF000000FF000000CE000000120000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000F8000000FF000000FF000000090000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000A000000FF0000
      00FF000000F80000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000032000000EB000000FF000000FF0000
      00F500000045000000FF000000FF000000FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00D4000000FF000000FF000000FD0000005D0000000000000000000000000000
      000300000099000000FF000000FF000000920000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000030000000D0000000FF000000FF000000FF0000
      00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
      00FF000000F90000007300000000000000000000000000000000000000000000
      0040000000FF000000FF000000BF000000000000000000000000000000000000
      000000000055000000FB000000FF000000FF000000CF00000012000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000D4000000FF000000FF000000700000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000071000000FF0000
      00FF000000D20000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000002C000000E7000000FF0000
      00FF000000F7000000FF000000FF000000FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      004E000000FF000000FF000000FF000000FF000000B0000000460000006A0000
      00D0000000FF000000FF000000FF000000190000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000020000003B0000006F000000770000
      0077000000770000007700000077000000FF000000FF000000FF000000B20000
      008C000000220000000000000000000000000000000000000000000000000000
      0040000000FF000000FF000000BF000000000000000000000000000000000000
      0053000000FB000000FF000000FF000000D10000001300000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000070000000FF000000FF000000FE0000
      00A9000000880000008800000088000000880000008800000088000000880000
      0088000000880000008800000088000000880000008800000088000000880000
      008800000088000000880000008800000088000000A9000000FE000000FF0000
      00FF0000006E0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000021000000DE0000
      00FF000000FF000000FF000000FF000000FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000090000000FF000000FF000000FF000000FF000000FF000000FF0000
      00FF000000FF000000FF000000FD000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000FF000000FF000000FF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0040000000FF000000FF000000BF000000000000000000000000000000510000
      00FA000000FF000000FF000000D7000000170000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000002000000C4000000FF000000FF0000
      00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
      00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
      00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
      00C3000000020000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000001F0000
      00DB000000FF000000FF000000FF000000FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000034000000AE000000F6000000FF000000FF0000
      00EA000000930000001A00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000FF000000FF000000FF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0040000000FF000000FF000000BF000000000000000000000049000000F80000
      00FF000000FF000000D800000018000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000F0000009E000000FF0000
      00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
      00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
      00FF000000FF000000FF000000FF000000FF000000FF000000FF0000009C0000
      000E000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      001A000000D5000000FF000000FF000000FE0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000B00000039000000250000
      0003000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000FF000000FF000000FF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0040000000FF000000FF000000BF000000000000004B000000F9000000FF0000
      00FF000000D70000001800000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000002E0000
      006C000000770000007700000077000000770000007700000077000000770000
      0077000000770000007700000077000000770000007700000077000000770000
      0077000000770000007700000077000000770000006C0000002D000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000015000000CB000000FF000000BB0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000EF000000FF000000FF000000190000
      0000000000000000000000000000000000000000000000000000000000000000
      0040000000FF000000FF000000BF00000048000000F8000000FF000000FF0000
      00DA0000001A0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000700000031000000070000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000BB000000FF000000FF0000009B0000
      0001000000000000000000000000000000000000000000000000000000000000
      0040000000FF000000FF000000D0000000F7000000FF000000FF000000DC0000
      001C000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000052000000FF000000FF000000FF0000
      00C7000000890000008800000088000000880000008800000088000000880000
      00A6000000FF000000FF000000FC000000FF000000FF000000DE0000001E0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000001000000A2000000FF000000FF0000
      00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
      00FF000000FF000000FF000000FF000000FF000000E000000020000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000300000084000000F80000
      00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
      00FF000000FF000000FF000000FF000000C90000002100000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000150000
      005C000000770000007700000077000000770000007700000077000000770000
      007700000099000000AF00000053000000040000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      28000000C0000000600000000100010000000000000900000000000000000000
      000000000000000000000000FFFFFF00FFFFFFFFFFFFFFFFFFFFFFFF00000000
      0000000000000000FFFFFFFFFFFFFFFFFFFFFFFF000000000000000000000000
      FFFFFFFFFFFFFFF800001FFF000000000000000000000000FFFFFFFFFFFFFF80
      000001FF000000000000000000000000FFFFFFFFFFFFFF00000000FF00000000
      0000000000000000FFFFFFFFFFFFFE000000007F000000000000000000000000
      F9FFFF1FFFFFFE000000007F000000000000000000000000F0FFFF0FFFFFFE07
      FFFFE07F000000000000000000000000F07FFF07FFFFFC1FFFFFF83F00000000
      0000000000000000F03FFF03FFFFFC1FFFFFF83F000000000000000000000000
      F01FFF01FFFFFC1FFFFFF83F000000000000000000000000F00FFF00FFFFFC1F
      FFFFF83F000000000000000000000000F007FF007FFFFC1FFFFFF83F00000000
      0000000000000000F001FF103FFFFC1FFFFFF83F000000000000000000000000
      F0003F181FFFFC1FFFFFF83F000000000000000000000000F840001C0FFFFC1F
      FFFFF83F000000000000000000000000F870001E07FFFC1FFFFFF83F00000000
      0000000000000000F87C001F03FFFC1FFFFFF83F000000000000000000000000
      F83F801F81FFFC1FFFFFF83F000000000000000000000000F83FFFFFC0FFFC1F
      FFFFF83F000000000000000000000000FC3FFFFFE07FFC1FFFFFF83F00000000
      0000000000000000FC1FFFFFF01FFC1FFFFFF83F000000000000000000000000
      FE0FFFFFF80FFC1FFFFFF83F000000000000000000000000FE0FFFFFFC0FFC1F
      FFFFF83F000000000000000000000000FF07FFFFFC0FFC1FFFFFF83F00000000
      0000000000000000FF01FFFFF80FFC1FFFFFF83F000000000000000000000000
      FF80FFFFF01FFC1FFFE0003F000000000000000000000000FFC03FFFE07FFC1F
      FE00003F000000000000000000000000FFE003FFC0FFFC1FFC00007F00000000
      0000000000000000FFF8001F81FFFC1FFC00007F000000000000000000000000
      FFFC001F03FFFC1FFC00007F000000000000000000000000FFFF801E07FFFC1F
      FC1F80FF000000000000000000000000FFFFF81C0FFFFC1FFC3F01FF00000000
      0000000000000000FFFFFF181FFFFC1FFC3C07FF000000000000000000000000
      FFFFFF103FFFFC1FFC380FFF000000000000000000000000FFFFFF007FFFFC1F
      FC301FFF000000000000000000000000FFFFFF00FFFFFC1FFC203FFF00000000
      0000000000000000FFFFFF01FFFFFC1FFC207FFF000000000000000000000000
      FFFFFF03FFFFFC1FFC007FFF000000000000000000000000FFFFFF07FFFFFC1F
      FC00FFFF000000000000000000000000FFFFFF0FFFFFFE07FC03FFFF00000000
      0000000000000000FFFFFF1FFFFFFE000007FFFF000000000000000000000000
      FFFFFFFFFFFFFE00000FFFFF000000000000000000000000FFFFFFFFFFFFFF00
      001FFFFF000000000000000000000000FFFFFFFFFFFFFF80001FFFFF00000000
      0000000000000000FFFFFFFFFFFFFFF801FFFFFF000000000000000000000000
      FFFFFFFFFFFFFFFFFFFFFFFF000000000000000000000000FFFFFFFFFFFFFFFF
      FFFFFFFF000000000000000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFE000003FFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFC000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF800
      0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00000007FFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFF07FFFF87FFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFF0FFFFF87FFFFFFFFFFFFFFFFFFFF8FFFF9FFF0FFFFFFFFFF0FF
      FFFC7FFFFFFFC000003FFFFFF0FFFF0FFC03FFFFFFFFF0FFFFFC7FFFFFFF0000
      000FFFFFE0FFFE0FF001FFFFFFFFF0FFFFFC7FFFFFFE00000007FFFFC0FFFC0F
      E000FFFFFFFFF0FFFFFC007FFFFE00000007FFFF80FFF80FE0E0FFFFFFFFF0FF
      FFFC003FFFFE1FFFFF87FFFF00FFF00FE1F07FFFFFFFF0FFFFFC001FFFFE1FFF
      FF87FFFE00FFE00FE1F87FFFFFFFF0FFFFFC000FFFFE3FFFFFC7FFFC08FF800F
      E1F87FFFF03FF0FFFFFC7E0FFFFE3FFFFFC7FFF818FC000FE1F87FFC0007F0FF
      FFFC7F0FFFFE3F0FFFC7FFF03800021FE0F01FF00007F0FFFFFC7F0FFFFE3F07
      FFC7FFE078000E1FE00007E00007F0FFFFFC7F0FFFFE3F03FFC7FFC0F8003E1F
      E0000780001FF0FFFFFC7F0FFFFFFF01FFC7FF81F801FC1FF0000100007FF0FF
      FFFC7F0FFFFFFF80FFC7FF03FFFFFC3FF800000001FFF0FFFFFC7F0FFFFFFFC0
      7FC7FE07FFFFFC3FFFFF800007FFF0FFF0007F0FFFFFFFE03FC7F80FFFFFF83F
      FFFFC0041FFFF0FFC0007F0FE00000001FC7F01FFFFFF07FFFFFC0007FFFF0FF
      80007F0FE00000001FC7F03FFFFFF07FFFFFC0007FFFF0FF80007F0FE0000000
      1FC7F03FFFFFE0FFFFFFE1803FFFF0FF87E07F0FE00000001FC7F01FFFFF81FF
      FFFFE1801FFFF0FF87C0FF0FFFFFFFE03FC7F80FFFFF01FFF800100C01FFF0FF
      8781FF0FFFFFFFC07FC7FE07FFFC03FFF000000700FFF0FF8703FF0FFFFFFF80
      FFC7FF03FFC007FFE0000001807FF0FF8607FF0FFFFFFF01FFC7FF81F8001FFF
      E0000400201FF0FF840C000FFFFE3F03FFC7FFC0F8003FFFE0F01F000007F0FF
      8010000FFFFE3F07FFC7FFE07801FFFFE1F83FE00007F0FF8030000FFFFE3F0F
      FFC7FFF0383FFFFFE1F87FFF0007F07F8060000FFFFE3FFFFFC7FFF818FFFFFF
      E1F87FFFE00FF00000E0FC1FFFFE3FFFFFC7FFFC08FFFFFFE1F07FFFFFFFF800
      01E1F81FFFFE1FFFFF87FFFE00FFFFFFE0E0FFFFFFFFFC0003E1F03FFFFE1FFF
      FF87FFFF00FFFFFFE000FFFFFFFFFE0007E1E07FFFFE00000007FFFF80FFFFFF
      F001FFFFFFFFFFFE3FE1C0FFFFFE00000007FFFFC0FFFFFFFC03FFFFFFFFFFFE
      3FE181FFFFFF0000000FFFFFE0FFFFFFFF0FFFFFFFFFFFFE3FE103FFFFFFC000
      003FFFFFF0FFFFFFFFFFFFFFFFFFFFFE1FE007FFFFFFFFFFFFFFFFFFF8FFFFFF
      FFFFFFFFFFFFFFFE0FE00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE
      00001FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE00003FFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00007FFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFC000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00000000000000000000000000000000
      000000000000}
  end
  object JvGradientCaption1: TJvGradientCaption
    Active = False
    Captions = <
      item
        Caption = 'juhu'
      end>
    DefaultFont = False
    FormCaption = 'HelpNDoc Cheater Tooll (c) 2023 by Jens Kallup - paule32'
    Font.Charset = ANSI_CHARSET
    Font.Color = clCaptionText
    Font.Height = -18
    Font.Name = 'Arial Narrow'
    Font.Style = [fsBold]
    StartColor = clRed
    EndColor = clYellow
    Left = 304
    Top = 399
  end
  object JvPopupMenu2: TJvPopupMenu
    Style = msItemPainter
    ImageMargin.Left = 0
    ImageMargin.Top = 0
    ImageMargin.Right = 0
    ImageMargin.Bottom = 0
    ImageSize.Height = 32
    ImageSize.Width = 32
    OnDrawItem = JvPopupMenu1DrawItem
    Left = 212
    Top = 342
    object MenuItem1: TMenuItem
      Caption = 'Reference'
      OnMeasureItem = Open1MeasureItem
    end
    object MenuItem2: TMenuItem
      Caption = '-'
    end
    object MenuItem3: TMenuItem
      Caption = 'About ...'
      OnClick = MenuItem3Click
    end
  end
  object JvPreviewRenderJvRichEdit1: TJvPreviewRenderJvRichEdit
    Left = 621
    Top = 357
  end
  object OpenDialog1: TOpenDialog
    Left = 405
    Top = 381
  end
  object MadExceptionHandler1: TMadExceptionHandler
    Left = 362
    Top = 206
  end
  object ImageList3: TImageList
    Height = 24
    Width = 24
    Left = 608
    Top = 242
    Bitmap = {
      494C010107001800040018001800FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000600000003000000001002000000000000048
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000008080000080800000808000008080000080
      8000008080000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000080808000C0C0
      C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0
      C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0
      C000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000808000000000000000000000FFFF000080800000808000008080000080
      8000000000000000000000000000008080000080800000808000000000000000
      0000000000000000000000000000000000000000000080808000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C0C0C0000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000080800000FFFF00000000000080800000FFFF0000FFFF0000FFFF0000FF
      FF0000FFFF000080800000808000000000000080800000808000000000000000
      0000000000000000000000000000000000000000000080808000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C0C0C0000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000808000008080000000000000FFFF0000808000008080000000
      0000000000000000000000808000008080000000000000000000000000000000
      0000000000000000000000000000000000000000000080808000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000080800000FFFF0000000000008080000000000000000000000000000000
      0000C0C0C0000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000808000008080000080
      8000008080000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000080808000000000000000
      0000000000000000000000000000000000000000000000000000008080000080
      800000FFFF00000000000080800000FFFF000000000000000000000000000000
      0000C0C0C0000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000080
      8000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000080808000000000000000
      00000000000000000000000000000000000000000000000000000080800000FF
      FF0000FFFF00808080000000000000FFFF000080800000000000000000000000
      0000C0C0C0000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000B9B9
      B900B9B9B9000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000080800000808000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000080808000000000000000
      0000000000000000000000000000000000000000000000000000008080000080
      800000FFFF00000000000080800000FFFF000000000000000000000000000000
      0000C0C0C0000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000BABABA000808
      080006060600B4B4B40000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000008080000080800000FFFF000080800000000000000000000000
      0000000000000000000000808000008080000000000000000000000000000000
      0000000000000000000000000000000000000000000080808000000000000000
      0000000000000000000000000000000000000000000000000000000000000080
      80000080800000FFFF0000000000008080000000000000000000000000000000
      0000C0C0C0000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000C3C3C3000B0B0B000000
      00000000000009090900BDBDBD00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000080800000FFFF0000FFFF00000000000000000080008000808000008080
      0000808000008000800000000000000000000080800000000000000000000000
      0000000000000000000000000000000000000000000080808000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000080800000808000008080000000000000000000000000000000
      0000C0C0C0000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000C5C5C5000C0C0C00000000000000
      000000000000000000000A0A0A00C0C0C0000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000FFFF0000808000808080008080800080808000800080008000
      8000800080008080000080800000808000000080800000808000000000000000
      0000000000000000000000000000000000000000000080808000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C0C0C0000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000C8C8C8000E0E0E0000000000000000000000
      00000000000000000000000000000B0B0B00C2C2C20000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000008080800000800000FF00FF0080808000FFFF0000FFFF
      0000FFFF00008000800080008000808000008000800000000000008080000000
      0000000000000000000000000000000000000000000080808000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C0C0C0000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000CFCFCF00121212000000000000000000000000000000
      0000000000000000000000000000000000000F0F0F00CACACA00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008080000080808000FF00FF00008000000080000080808000FFFF
      0000FF00FF00FFFF000080800000800080008000800080808000000000000000
      0000000000000000000000000000000000000000000080808000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C0C0C0000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000D1D1D10013131300000000000000000000000000000000000000
      0000000000000000000000000000000000000000000010101000CDCDCD000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008080000000800000008000000080000000800000FFFF0000FF00
      FF00FFFF00000080000000800000808000008080800080808000000000000000
      0000000000000000000000000000000000000000000080808000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C0C0C0000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000D8D8D8001818180000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000015151500D4D4
      D400000000000000000000000000000000000000000000000000000000000000
      0000FF00FF00FFFF0000FF00FF0080808000FFFF0000FFFF0000FFFF0000FFFF
      0000FFFF00000080000000800000C0C0C0008000800080800000800080000080
      8000000000000000000000000000000000000000000080808000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C0C0C0000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000DADA
      DA001A1A1A000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000001616
      1600D6D6D6000000000000000000000000000000000000000000000000000000
      000080800000FF00FF00FFFF000080808000FFFF0000FF00FF00FFFF0000FFFF
      0000FFFF0000FF00FF0000800000FF00FF008080000080008000808000000080
      8000000000000000000000000000000000000000000080808000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C0C0C0000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000B0B0
      B000666666006666660066666600666666006666660066666600666666006666
      6600666666006666660066666600666666006666660066666600666666006666
      6600B0B0B0000000000000000000000000000000000000000000000000000000
      000080800000FF00FF0000FF0000FFFF0000FF00FF000000000000000000FFFF
      0000FFFF000000800000FF00FF0000800000C0C0C00080008000808000000080
      8000000000000000000000000000000000000000000080808000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C0C0C0000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FF00FF0000FF0000FF00FF00C0C0C000FFFF00000000000000000000FFFF
      0000000000000080000000800000808080008080000080800000800080000080
      8000008080000000000000000000000000000000000080808000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C0C0C0000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00008080000000FF000000FF0000FFFF000000000000FFFF0000808080000000
      000080808000FFFF000000FF000000FF000080808000808000008000800000FF
      FF00008080000000000000000000000000000000000080808000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C0C0C0000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008080000000FF0000FFFF0000FFFF000000000000000000008080
      80000000000000FF000000FF000000FF0000C0C0C000808000000000000000FF
      FF00008080000000000000000000000000000000000080808000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C0C0C0000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000080800000FFFF000080808000FF00FF00FFFF0000FFFF
      0000FFFF0000FF00FF00FF00FF0000FF0000808080000000000000FFFF000080
      8000000000000000000000000000000000000000000080808000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C0C0C0000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000FF00FF00FFFF0000FFFF0000FFFF0000FFFF
      0000FFFF0000FFFF000000FF000080808000000000000080800000FFFF000000
      0000000000000000000000000000000000000000000080808000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C0C0C0000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000008080000080800000FFFF0000FF00FF00FF00
      FF00FF00FF00FFFF00008080000080800000008080000000000000FFFF000000
      0000000000000000000000000000000000000000000080808000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C0C0C0000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000808000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000546EE700546EE700546EE700546E
      E700546EE700A9B6F30000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000A9B6F300546E
      E700546EE700546EE700546EE700546EE7000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000546EE700546EE700546EE700546E
      E700546EE700546EE700AAB7F300000000000000000000000000000000000000
      00000000000000000000000000000000000000000000AFBBF400546EE700546E
      E700546EE700546EE700546EE700546EE7000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000546EE700546EE700546EE700546E
      E700546EE700546EE700546EE700A9B6F3000000000000000000000000000000
      000000000000000000000000000000000000A9B6F300546EE700546EE700546E
      E700546EE700546EE700546EE700546EE7000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000546EE700546EE700546EE700546E
      E700546EE700546EE700546EE700546EE700AAB7F30000000000000000000000
      0000000000000000000000000000AFBBF400546EE700546EE700546EE700546E
      E700546EE700546EE700546EE700546EE7000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000DADADA00B0B0B00000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000B0B0B000D6D6D60000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000546EE700546EE700546EE700546E
      E700546EE700546EE700546EE700546EE700546EE700AAB7F300000000000000
      00000000000000000000AFBBF400546EE700546EE700546EE700546EE700546E
      E700546EE700546EE700546EE700546EE7000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000D8D8D8001A1A1A006666660000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000006666660016161600D4D4D400000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000A9B6F300546EE700546EE700546E
      E700546EE700546EE700546EE700546EE700546EE700546EE700AAB7F3000000
      000000000000AFBBF400546EE700546EE700546EE700546EE700546EE700546E
      E700546EE700546EE700546EE700A4B2F2000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000D1D1D10018181800000000006666660000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000666666000000000015151500CDCDCD000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000A9B6F300546EE700546E
      E700546EE700546EE700546EE700546EE700546EE700546EE700546EE700AAB7
      F300AFBBF400546EE700546EE700546EE700546EE700546EE700546EE700546E
      E700546EE700546EE700A4B2F200000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000CFCFCF001313130000000000000000006666660000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000066666600000000000000000010101000CACA
      CA00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000A9B6F300546E
      E700546EE700546EE700546EE700546EE700546EE700546EE700546EE700546E
      E700546EE700546EE700546EE700546EE700546EE700546EE700546EE700546E
      E700546EE700A4B2F20000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000C8C8
      C800121212000000000000000000000000006666660000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000666666000000000000000000000000000F0F
      0F00C2C2C2000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000B0B0
      B000666666006666660066666600666666006666660066666600666666006666
      6600666666006666660066666600666666006666660066666600666666006666
      6600B0B0B000000000000000000000000000000000000000000000000000A9B6
      F300546EE700546EE700546EE700546EE700546EE700546EE700546EE700546E
      E700546EE700546EE700546EE700546EE700546EE700546EE700546EE700546E
      E700A4B2F2000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C5C5C5000E0E
      0E00000000000000000000000000000000006666660000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000666666000000000000000000000000000000
      00000B0B0B00C0C0C00000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000DADA
      DA001A1A1A000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000001616
      1600D6D6D6000000000000000000000000000000000000000000000000000000
      0000A9B6F300546EE700546EE700546EE700546EE700546EE700546EE700546E
      E700546EE700546EE700546EE700546EE700546EE700546EE700546EE700A4B2
      F200000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000C3C3C3000C0C0C000000
      0000000000000000000000000000000000006666660000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000666666000000000000000000000000000000
      0000000000000A0A0A00BDBDBD00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000D8D8D8001818180000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000015151500D4D4
      D400000000000000000000000000000000000000000000000000000000000000
      000000000000A9B6F300546EE700546EE700546EE700546EE700546EE700546E
      E700546EE700546EE700546EE700546EE700546EE700546EE700A4B2F2000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000BABABA000B0B0B00000000000000
      0000000000000000000000000000000000006666660000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000666666000000000000000000000000000000
      0000000000000000000009090900B4B4B4000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000D1D1D10013131300000000000000000000000000000000000000
      0000000000000000000000000000000000000000000010101000CDCDCD000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000A9B6F300546EE700546EE700546EE700546EE700546E
      E700546EE700546EE700546EE700546EE700546EE700A4B2F200000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000B9B9B9000808080000000000000000000000
      0000000000000000000000000000000000006666660000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000666666000000000000000000000000000000
      000000000000000000000000000006060600B9B9B90000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000CFCFCF00121212000000000000000000000000000000
      0000000000000000000000000000000000000F0F0F00CACACA00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000AFBBF400546EE700546EE700546EE700546EE700546E
      E700546EE700546EE700546EE700546EE700546EE700AAB7F300000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000B9B9B9000606060000000000000000000000
      0000000000000000000000000000000000006666660000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000666666000000000000000000000000000000
      000000000000000000000000000008080800B9B9B90000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000C8C8C8000E0E0E0000000000000000000000
      00000000000000000000000000000B0B0B00C2C2C20000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000AFBBF400546EE700546EE700546EE700546EE700546EE700546E
      E700546EE700546EE700546EE700546EE700546EE700546EE700AAB7F3000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000B4B4B40009090900000000000000
      0000000000000000000000000000000000006666660000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000666666000000000000000000000000000000
      000000000000000000000B0B0B00BABABA000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000C5C5C5000C0C0C00000000000000
      000000000000000000000A0A0A00C0C0C0000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000AFBBF400546EE700546EE700546EE700546EE700546EE700546EE700546E
      E700546EE700546EE700546EE700546EE700546EE700546EE700546EE700AAB7
      F300000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000BDBDBD000A0A0A000000
      0000000000000000000000000000000000006666660000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000666666000000000000000000000000000000
      0000000000000C0C0C00C3C3C300000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000C3C3C3000B0B0B000000
      00000000000009090900BDBDBD00000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000AFBB
      F400546EE700546EE700546EE700546EE700546EE700546EE700546EE700546E
      E700546EE700546EE700546EE700546EE700546EE700546EE700546EE700546E
      E700AAB7F3000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C0C0C0000B0B
      0B00000000000000000000000000000000006666660000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000666666000000000000000000000000000000
      00000E0E0E00C5C5C50000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000BABABA000808
      080006060600B4B4B40000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000AFBBF400546E
      E700546EE700546EE700546EE700546EE700546EE700546EE700546EE700546E
      E700546EE700546EE700546EE700546EE700546EE700546EE700546EE700546E
      E700546EE700AAB7F30000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000C2C2
      C2000F0F0F000000000000000000000000006666660000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000666666000000000000000000000000001212
      1200C8C8C8000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000B9B9
      B900B9B9B9000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000A9B6F300546EE700546E
      E700546EE700546EE700546EE700546EE700546EE700546EE700546EE700A9B6
      F300A9B6F300546EE700546EE700546EE700546EE700546EE700546EE700546E
      E700546EE700546EE700A9B6F300000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000CACACA001010100000000000000000006666660000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000066666600000000000000000013131300CFCF
      CF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000AFBBF400546EE700546EE700546E
      E700546EE700546EE700546EE700546EE700546EE700546EE700A4B2F2000000
      000000000000A9B6F300546EE700546EE700546EE700546EE700546EE700546E
      E700546EE700546EE700546EE700AAB7F3000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000CDCDCD0015151500000000006666660000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000666666000000000018181800D1D1D1000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000546EE700546EE700546EE700546E
      E700546EE700546EE700546EE700546EE700546EE700A4B2F200000000000000
      00000000000000000000A9B6F300546EE700546EE700546EE700546EE700546E
      E700546EE700546EE700546EE700546EE7000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000D4D4D400161616006666660000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000666666001A1A1A00D8D8D800000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000546EE700546EE700546EE700546E
      E700546EE700546EE700546EE700546EE700A4B2F20000000000000000000000
      0000000000000000000000000000A9B6F300546EE700546EE700546EE700546E
      E700546EE700546EE700546EE700546EE7000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000D6D6D600B0B0B00000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000B0B0B000DADADA0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000546EE700546EE700546EE700546E
      E700546EE700546EE700546EE700A9B6F3000000000000000000000000000000
      000000000000000000000000000000000000A9B6F300546EE700546EE700546E
      E700546EE700546EE700546EE700546EE7000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000546EE700546EE700546EE700546E
      E700546EE700546EE700A4B2F200000000000000000000000000000000000000
      00000000000000000000000000000000000000000000A9B6F300546EE700546E
      E700546EE700546EE700546EE700546EE7000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000546EE700546EE700546EE700546E
      E700546EE700A9B6F30000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000A9B6F300546E
      E700546EE700546EE700546EE700546EE7000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000060000000300000000100010000000000400200000000000000000000
      000000000000000000000000FFFFFF00FFFFFFFE00FFE0000F000000FFFFFFF8
      003FC00007000000FFFFFFF6001FBFFFE3000000FFFFFFF2001FB0F8E3000000
      FFFFFFF9003FBFE063000000FFFFFFFF83FFBFC023000000FFFFFFFFC7FFBFC0
      23000000FFE7FFFF03FFB00023000000FFC3FFF8007FB00063000000FF81FFF0
      003FBFF8E3000000FF00FFF8001FB00063000000FE007FF8001FB00063000000
      FC003FF8000FBFFFE3000000F8001FF8000FBFFFE3000000F0000FF00007BFFF
      E3000000E00007F00007BFFFE3000000E00007F06007BFFFE3000000FFFFFFF0
      6807BFFFE3000000FFFFFFF09007B0FA63000000FFFFFFF86807BFFFE3000000
      FFFFFFFC000FBFFFE3000000FFFFFFFE001FAAF563000000FFFFFFFE005FAAF5
      63000000FFFFFFFFFFBFE8543F00000003FFC0FFFFFFFFFFFFFFFFFF01FF80FF
      FFFFFFFFFFFFFFFF00FF00FFFFFFFFFFFFFFFFFF007E00FFFE7FFE7FFFFFFFFF
      003C00FFFC7FFE3FFFFFFFFF001800FFF87FFE1FFFFFFFFF800001FFF07FFE0F
      FFFFFFFFC00003FFE07FFE07FFE00007E00007FFC07FFE03FFE00007F0000FFF
      807FFE01FFF0000FF8001FFF007FFE00FFF8001FFC003FFE007FFE007FFC003F
      FC003FFE007FFE007FFE007FF8001FFF007FFE00FFFF00FFF0000FFF807FFE01
      FFFF81FFE00007FFC07FFE03FFFFC3FFC00003FFE07FFE07FFFFE7FF800001FF
      F07FFE0FFFFFFFFF001800FFF87FFE1FFFFFFFFF003C00FFFC7FFE3FFFFFFFFF
      007E00FFFE7FFE7FFFFFFFFF00FF00FFFFFFFFFFFFFFFFFF01FF80FFFFFFFFFF
      FFFFFFFF03FFC0FFFFFFFFFFFFFFFFFF00000000000000000000000000000000
      000000000000}
  end
  object JvPopupMenu3: TJvPopupMenu
    Style = msItemPainter
    ImageMargin.Left = 0
    ImageMargin.Top = 0
    ImageMargin.Right = 0
    ImageMargin.Bottom = 0
    ImageSize.Height = 32
    ImageSize.Width = 32
    OnDrawItem = JvPopupMenu1DrawItem
    Left = 298
    Top = 177
    object DeleteFilter1: TMenuItem
      Caption = 'Delete Filter'
    end
  end
  object LocalConnection: TFDConnection
    ConnectionName = 'LOCALCONNECTION'
    Params.Strings = (
      'DriverID=SQLite'
      'SharedCache=False'
      'Synchronous=Full'
      'LockingMode=Normal'
      'Password=TOPSECRET')
    UpdateOptions.AssignedValues = [uvLockMode]
    UpdateOptions.LockMode = lmPessimistic
    Left = 561
    Top = 192
  end
end
