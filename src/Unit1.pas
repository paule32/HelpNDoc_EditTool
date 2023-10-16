// ---------------------------------------------------------------------
// File:   copyTestRTF.pas
// Author: (c) 2023 by Jens Kallup - paule32
//         all rights reserved.
//
// only free for education, and non-profit !
// ---------------------------------------------------------------------
unit Unit1;

interface

uses
  SysUtils;

type
  TMyRTFClass = class
  public
    function AddBullet(textRow: String; indent: Integer): String;
    function AddNewLine(text: String): String;
    function AddHeader(text: String; fontName: String; fontSize: Integer): String;
  end;

implementation

function TMyRTFClass.AddBullet(textRow: String; indent: Integer): String;
begin
  Result := '\li{'
  + IntToStr(indent)
  + '}{{\pntext\f1\''B7}}{{\*\pn\pnlvlblt{{\pntxtb\''B7}}}}{'
  + textRow
  + '}';
end;

function TMyRTFClass.AddNewLine(text: String): String;
begin
  Result := '{'
  + text
  + '}\par\pard ';
end;


function TMyRTFClass.AddHeader(text: String; fontName: String; fontSize: Integer): String;
begin
  Result := ''
  + '{{\rtf1\ansi\ansicpg1252\deff0\nouicompat\deflang1031{{\fonttbl'
  + '{{\f0\fnil\fcharset0 {'
  + fontName
  + '};}}{{\f1\fnil\fcharset2 Symbol;}}}}'
  + '{{\colortbl '
  + ';\red0\green77\blue187'
  + ';\red128\green0\blue0'
  + ';\red0\green0\blue0'
  + ';\red155\green0\blue211'
  + ';\red81\green163\blue69'
  + ';\red51\green51\blue51'
  + ';}}'
  + '\viewkind4\uc1\pard\sl0\slmult1\f0\fs{'
  + IntToStr(fontSize)
  + ' * 2}\lang7 {'
  + text
  + '}}}';
end;

end.
