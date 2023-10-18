object PrintPreviewerFrame: TPrintPreviewerFrame
  Left = 0
  Top = 0
  Width = 640
  Height = 480
  TabOrder = 0
  object PrintPreview1: TJvPreviewControl
    Left = 0
    Top = 0
    Width = 640
    Height = 441
    TopRow = 0
    SelectedPage = 1
    DeviceInfo.LogPixelsX = 300
    DeviceInfo.LogPixelsY = 300
    DeviceInfo.PhysicalWidth = 2480
    DeviceInfo.PhysicalHeight = 3507
    DeviceInfo.PageWidth = 2400
    DeviceInfo.PageHeight = 3281
    DeviceInfo.OffsetLeft = 40
    DeviceInfo.OffsetTop = 40
    DeviceInfo.OffsetRight = 40
    DeviceInfo.OffsetBottom = 40
    Options.Rows = 1
    Options.Scale = 29
    Align = alTop
    ParentColor = False
    PopupMenu = JvPopupMenu3
    TabOrder = 0
  end
  object JvPopupMenu3: TJvPopupMenu
    Style = msItemPainter
    ImageMargin.Left = 0
    ImageMargin.Top = 0
    ImageMargin.Right = 0
    ImageMargin.Bottom = 0
    ImageSize.Height = 32
    ImageSize.Width = 32
    Left = 65
    Top = 424
    object MenuItem4: TMenuItem
      Caption = 'Switch'
      OnClick = MenuItem4Click
    end
    object MenuItem5: TMenuItem
      Caption = '-'
    end
    object MenuItem6: TMenuItem
      Caption = 'Settings'
    end
    object Print2: TMenuItem
      Caption = 'Print'
    end
  end
end
