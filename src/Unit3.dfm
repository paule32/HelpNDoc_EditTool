object AboutForm: TAboutForm
  Left = 0
  Top = 0
  Caption = 'About ...'
  ClientHeight = 193
  ClientWidth = 318
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -16
  Font.Name = 'Arial Narrow'
  Font.Style = [fsBold]
  TextHeight = 20
  object Label1: TLabel
    Left = 20
    Top = 24
    Width = 275
    Height = 21
    Caption = 'non-Profit HelpTool Version  1.0'
    Font.Charset = ANSI_CHARSET
    Font.Color = clRed
    Font.Height = -18
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label2: TLabel
    Left = 20
    Top = 59
    Width = 173
    Height = 21
    Caption = 'Author: Jens Kallup'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -18
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label3: TLabel
    Left = 20
    Top = 86
    Width = 250
    Height = 21
    Caption = '(c) 2023 All Rights Reserved.'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -18
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Button1: TButton
    Left = 56
    Top = 128
    Width = 185
    Height = 49
    Caption = 'Close'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -22
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 0
    OnClick = Button1Click
  end
end
