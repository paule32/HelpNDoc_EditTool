// ---------------------------------------------------------------------
// File:   copyTestScanner.pas
// Author: (c) 2023 by Jens Kallup - paule32
//         all rights reserved.
//
// only free for education, and non-profit !
// ---------------------------------------------------------------------
unit Scanner;

interface

resourcestring
  ERR_SCANNER_UNEXPECTED_CHAR = 'Error: 0: Scnner: Unexpected char found in stream.';
  ERR_PARSER_EXPECTED = 'Error 1: Parser: %s expected, %s found instead';
  ERR_PARSER_UNALLOWED_STATEMENT = 'Error 2: Parser: unallowed Statement';
  ERR_PARSER_WRONG_PROCEDURE_ENDED = 'Error 3: Parser: Procedure end %s expected, but %s found';
  ERR_PARSER_UNKNOWN_IDENT = 'Error 4: Parser: Unknown Identifier';
  ERR_PARSER_VAR_CONSTANT_EXPECTED = 'Error 5: Parser: Variable or Constant expected';
  ERR_PARSER_VAR_EXPECTED = 'Error 6: Parser: Variable expected';
  ERR_PARSER_PROCEDURE_EXPECTED = 'Error 7: Parser: Procedure expected';
  ERR_PARSER_NO_CONST_ALLOWED = 'Error 8: Parser: No Constant allowed here';

  procedure LexScanner(filename: String);

implementation

uses
  Vcl.Forms, Vcl.Dialogs,
  System.Classes,
  System.SysUtils,
  System.TypInfo,
  Unit2;

type
  TSymbol = (
    sUnknown, sIdent, sInteger, sPlus, sMinus, sStar, sSlash, sEqual,
    sSmaller, sBigger, sBiggerEqual, sSmallerEqual, sUnEqual,
    sOpenBracket, sCloseBracket, sComma, sDot, sSemiColon, sBecomes,
    sVar, sConst, sProcedure, sBegin, sEnd, sIf, sThen,
    sElseIf, sElse, sWhile, sDo, sUnit, sWrite,
    sNone
    );

const
  cSymbols : Array[TSymbol] of String = (
    '','','','+','-','*','/','=',
    '<','>','>=','<=','#',
    '(',')',',','.',';',':=',
    'VAR','CONST','PROCEDURE','BEGIN','END','IF','THEN',
    'ELSEIF','ELSE','WHILE','DO','UNIT','WRITE',
    ''
    );
type
  TIdentType = (itConstant, itVariable, itProcedure);
  TIdent = record
    name: String;
    case kind: TIdentType of
      itConstant: (val: Integer);
      itVariable,
      itProcedure: (level,adr,size: Integer);
  end;
  TIdentList = Array of TIdent;

  TOpCode = (lit,opr,lod,sto,cal,int,jmp,jpc,wri);
  TInstruction = packed record
    f: TOpCode; // command
    l: Byte;    // level
    a: Integer; // address
  end;
  PInstruction = ^TInstruction;

  TInstructions = Array of TInstruction;
var
  ins: PInstruction;
  insArr: TInstructions;

var
  Table: TIdentList;
  FileStream: TFileStream;

  cx: Integer;  // code position

  ID: String;
  num: Integer;

var
  ch: Char;
  str: String;
  Symbol: TSymbol;

var
  inFile: File;
  bcFile: File of Byte;
  Line: Integer;

procedure Error(ErrorText: String);
var
  s: String;
begin
  s := Format('%d: ' + ErrorText, [Line]);
  raise Exception.Create(s);
end;

procedure ErrorExpected(Expected: Array of TSymbol; Found: TSymbol);
const
  eSymbols : Array[TSymbol] of String = (
    'Unknown','Identifier','Integer','+','-','*','/','=',
    '<','>','>=','<=','#',
    '(',')',',','.',';',':=',
    'VAR','CONST','PROCEDURE','BEGIN','END','IF','THEN',
    'ELSEIF','ELSE','WHILE','DO','UNIT','WRITE',
    '!none!'
    );
var
  ExpectedSymbol: String;
  i: Integer;
  s: String;
begin
  ExpectedSymbol := eSymbols[Expected[Low(Expected)]];
  for I := Low(Expected)+1 to High(Expected) do
  ExpectedSymbol := ExpectedSymbol + ', ' + eSymbols[Expected[i]];

  s := System.SysUtils.Format('%d: '
  + ERR_PARSER_EXPECTED,[Line,ExpectedSymbol,eSymbols[Found]]);

  raise Exception.Create(s);
end;

procedure Emulate;
const
  StackSize = 1024;
var
  p,b,t: Integer;
  s: Array[1..StackSize] of Integer;
  op: TOpCode;

  function Base(a: Integer): Integer;
  var
    b1: Integer;
  begin
    b1 := b;
    while a > 9 do
    begin
      b1 := s[b1];
      dec(a);
    end;
    base := b1;
  end;
begin
  Form2.FEditorFrame.Memo2.Lines.Add('Interpreting Code');
//  showmessage('size 2: ' + inttostr(sizeof(instructions)));
  SetLength(insArr, 4);
  insArr[0].f := Lit;
  insArr[1].f := Jmp;
//  ins := @Instructions[0];
//  ShowMessage(Format('OpCode: %s', [System.TypInfo.GetEnumName(TypeInfo(TOpCode), Ord(ins.f))]));
  exit;
  t := 0;
  b := 1;
  p := 0;
  s[1] := 0;
  s[2] := 0;
  s[3] := 0;
  (*
  repeat
    inc(p);
//    op := Instructions[p].f;
    exit;
    with Instructions[p] do
    begin
    showmessage('huhu');
      case f of
        lit:
        begin
        showmessage('11111');
          inc(t);
          s[t] := a;
        end;
        lod:
        begin
        showmessage('2222');
          inc(t);
          s[t] := s[base(l)+a];
        end;
        sto:
        begin
        showmessage('3333');
          s[base(l)+a] := s[t];
          dec(t);
        end;
        cal:
        begin
        showmessage('444');
          s[t + 1] := base(l);
          s[t + 2] := b;
          s[t + 3] := p;
          b := t + 1;
          p := a;
        end;
        int: t := t + a;
        jmp: p := a;
        jpc:
        begin
          if s[t] = 0 then p := a;
          dec(t);
        end;
        wri:
        begin
        showmessage('6666');
          Form2.FEditorFrame.Memo2.Lines.Add(
          'wri: ' + IntToStr(s[t]));
          dec(t);
        end;
        opr:
        begin
          case a of
            0:
            begin
              t := b - 1;
              p := s[ t + 3];
              b := s[ t + 2];
            end;
            1:
            begin
              s[t] := -s[t];  // negation
            end;
            2:
            begin
              // addition
              dec(t);
              s[t] := s[t] + s[t + 1];
            end;
            3:
            begin
              // subtraction
              dec(t);
              s[t] := s[t] - s[t + 1];
            end;
            4:
            begin
              // multiplication
              dec(t);
              s[t] := s[t] * s[t + 1];
            end;
            5:
            begin
              // division
              dec(t);
              s[t] := s[t] div s[t + 1];
            end;
            8:
            begin
              // Equal
              dec(t);
              s[t] := Ord(s[t] = s[t + 1]);
            end;
            9:
            begin
              // unequal
              dec(t);
              s[t] := Ord(s[t] <> s[t + 1]);
            end;
            10:
            begin
              // smaller
              dec(t);
              s[t] := Ord(s[t] < s[t + 1]);
            end;
            11:
            begin
              // bigger
              dec(t);
              s[t] := Ord(s[t] > s[t + 1]);
            end;
            12:
            begin
              // biggerequal
              dec(t);
              s[t] := Ord(s[t] >= s[t + 1]);
            end;
            13:
            begin
              // smallerequal
              dec(t);
              s[t] := Ord(s[t] <= s[t + 1]);
            end; else
            begin
              raise Exception.Create('Unknown Operand');
            end;
          end;
        end; else
        begin
          raise Exception.Create('Unknown opcode');
        end;
      end;
    end;
  until p = 4;*)
end;

procedure Expect(Expected: TSymbol);
begin
  if Symbol <> Expected then
  ErrorExpected([Expected], Symbol);
end;

procedure GenCode(f: TOpCode; l,a: Integer);
begin
  if cx > Length(insArr) - 1 then
  SetLength(insArr, Length(insArr) + 64);
  insArr[cx].f := f;
  insArr[cx].a := a;
  insArr[cx].l := l;
  inc (cx);
end;

procedure GetSym;
  procedure GetCh;
  begin
    if not (FileStream.Position > FileStream.Size) then
    begin
      FileStream.Read(ch,1);
      ch := ' ';
      ch := UpCase(ch);  // case in-sensitive

      if ch = #13 then inc(Line);
      if Ord(ch) < Ord(' ') then ch := ' ';
    end;
  end;
  var i: TSymbol;
begin
  while true do
  begin
    str := '';
    Symbol := sNone;

    while (CharInSet(ch, [' ']) and not Eof(inFile)) do
    GetCh;

    if Eof(inFile) then
    exit;

    // ident/reserved word
    if CharInSet(ch, ['A'..'Z', '_']) then
    begin
      while CharInSet(ch, ['A'..'Z','_','0'..'9']) do
      begin
        str := str + ch;
        GetCh;
      end;
      Symbol := sIdent;

      for i := sUnknown to sNone do
      begin
        if str = cSymbols[I] then
        begin
          Symbol := i;
          break;
        end;
      end;

      if Symbol = sIdent then
      ID := str;

      exit;
    end;

    // symbols that consists only of one char
    if CharInSet(ch, [';','+','-','=','#',',','.','*','/']) then
    begin
      str := ch;
      Symbol := sUnknown;
      for i := sUnknown to sNone do
      begin
        if str = cSymbols[i] then
        begin
          Symbol := i;
          break;
        end;
      end;
      GetCh;
      exit;
    end;

    // chars, that can contain forward chars (=)
    if CharInSet(ch, [':','<','>']) then
    begin
      str := ch;
      GetCh;
      if CharInSet(ch, ['=']) then str := str + ch;
      GetCh;
      Symbol := sUnknown;
      for i := sUnknown to sNone do
      begin
        if str = cSymbols[i] then
        begin
          Symbol := i;
          break;
        end;
      end;
      exit;
    end;

    // parens, and comas
    if CharInSet(ch, ['(',')']) then
    begin
      str := ch;
      GetCh;
      if (str = '(') and (CharInSet(ch, ['*'])) then
      begin
        // skip comment
        GetCh;
        while true do
        begin
          GetCh;
          if CharInSet(ch, ['*']) then
          begin
            GetCh;
            if CharInSet(ch, [')']) then
            begin
              Getch;
              break;
            end;
          end else
          begin
            if Eof(inFile) then
            break;
          end;
        end;
      end else
      begin
        if str = '(' then
        begin
          Symbol := sOpenBracket;
          exit;
        end else
        if str = ')' then
        begin
          Symbol := sCloseBracket;
          exit;
        end;
      end;
    end;

    // digits
    if CharInSet(ch, ['0'..'9','$']) then
    begin
      Symbol := sInteger;
      str := ch;
      GetCh;
      if str = '$' then  // hex value
      begin
        while CharInSet(ch, ['0'..'9','A'..'F']) do
        begin
          str := str + ch;
          GetCh;
        end;
        exit;
      end else
      begin
        while CharInSet(ch, ['0'..'9']) do
        begin
          str := str + ch;
          GetCh;
        end;
        exit;
      end;
    end
    else Error(ERR_SCANNER_UNEXPECTED_CHAR);
  end;
  Assert(Symbol <> sUnknown);
end;

procedure Module;
  function Position(ID: String; TablePosition: Integer): Integer;
  var
    i: Integer;
  begin
    Table[0].name := ID;
    I := TablePosition;
    while Table[I].name <> ID do
    dec(i);
    result := I;
  end;
  procedure StatementSequence(TablePosition, lev: Integer);
    procedure Statement;
      procedure Expression;
        procedure Term;
          procedure Factor;
          var
            identPos: Integer;
          begin
            if (Symbol in [sIdent]) then
            begin
              identPos := Position(ID,TablePosition);

              if identPos = 0 then
              Error(ERR_PARSER_UNKNOWN_IDENT);

              if Table[identPos].kind = itProcedure then
              Error(ERR_PARSER_VAR_CONSTANT_EXPECTED);

              case Table[identPos].kind of
                itConstant: GenCode(lit, 0,  Table[identPos].val);
                itVariable: GenCode(lod, lev-Table[identPos].level,Table[identPos].val);
              end;

              GetSym;
            end else
            if (Symbol = sInteger) then
            begin
              GenCode(lit,0,num);
              GetSym;
            end else
            if (Symbol = sOpenBracket) then
            begin
              GetSym;
              Expression;
              Expect(sCloseBracket);
              GetSym;
            end else
            ErrorExpected([sIdent, sInteger, sOpenBracket],Symbol);
          end;
        var
          operation: TSymbol;
        begin
          Factor;
          while Symbol in [sStar, sSlash] do
          begin
            Operation := Symbol;
            GetSym;
            Factor;
            case Operation of
              sStar:  GenCode(opr,0,4);
              sSlash: GenCode(opr,0,5);
            end;
          end;
        end;
      var
        Operation: TSymbol;
      begin
        if Symbol in [sPlus, sMinus] then
        begin
          Operation := Symbol;
          GetSym;
          Term;
          if Operation = sMinus then
          GenCode(opr,0,1);
        end else
        Term;
        while Symbol in [sPlus, sMinus] do
        begin
          Operation := Symbol;
          GetSym;
          Term;
          case Operation of
            sPlus:  GenCode(opr,0,2);
            sMinus: GenCode(opr,0,3);
          end;
        end;
      end;
      procedure Condition;
      var
        Operation: TSymbol;
      begin
        Expression;
        Operation := Symbol;
        GetSym;
        Expression;
        case Operation of
          sEqual: GenCode(opr,0,8);
          sSmaller:      GenCode(opr,0,10);
          sBigger:       GenCode(opr,0,11);
          sBiggerEqual:  GenCode(opr,0,12);
          sSmallerEqual: GenCode(opr,0,13);
          sUnEqual:      GenCode(opr,0, 9);
          else ErrorExpected(
          [sEqual, sSmaller, sBigger, sBiggerEqual, sSmallerEqual, sUnequal],
          Symbol)
        end;
      end;
    var
      identPos: Integer;
      ident: String;
      CodePosition1, CodePosition2: Integer;
    begin
      case Symbol of
        sIdent: begin
          ident := id;
          identPos := Position(ID, TablePosition);

          if identPos = 0 then
          Error(ERR_PARSER_UNKNOWN_IDENT);

          if Table[identPos].kind = itProcedure then
          begin
            // procedure call
            GenCode(cal,lev-Table[identPos].level,Table[identPos].adr);
            GetSym;
          end else

          if Table[identPos].kind = itVariable then
          begin
            GetSym;
            Expect(sBecomes);
            GetSym;
            Expression;
            GenCode(sto,lev-Table[identPos].level,Table[identPos].adr);
          end else
          Error(ERR_PARSER_NO_CONST_ALLOWED);
        end;
        sWrite: begin
          GetSym;
          Expression;
          GenCode(wri,0,0);
        end;
        sIf: begin
          GetSym;
          Condition;
          Expect(sThen);
          GetSym;

          CodePosition1 := cx;
          GenCode(jpc,0,0);

          StatementSequence(TablePosition,lev);
          Expect(sEnd);
          GetSym;

          CodePosition2 := cx;
          GenCode(jmp,0,0);

          insArr[CodePosition1].a := cx;

          while Symbol = sElseIf do
          begin
            GetSym;
            Condition;
            Expect(sThen);
            GetSym;

            CodePosition1 := cx;
            GenCode(jpc,0,0);

            StatementSequence(TablePosition,lev);
            Expect(sEnd);
            GetSym;

            insArr[CodePosition2].a := cx;
            CodePosition2 := cx;
            GenCode(jmp,0,0);

            insArr[CodePosition1].a := cx;
          end;
          if Symbol = sElse then
          begin
            GetSym;
            StatementSequence(TablePosition,lev);
            Expect(sEnd);
            GetSym;
            insArr[CodePosition2].a := cx;
          end;
        end;
        sWhile: begin
          GetSym;
          CodePosition2 := cx;

          Condition;
          Expect(sDo);
          GetSym;

          CodePosition1 := cx;
          GenCode(jpc,0,0);

          StatementSequence(TablePosition,lev);
          Expect(sEnd);

          GenCode(jmp,0,0);
          GetSym;

          insArr[CodePosition1].a := cx;
        end;
        sBegin: begin
          GetSym;
          StatementSequence(TablePosition,lev);
          Expect(sEnd);
          GetSym;
        end;
        else begin
          //dummy error
        end;
      end;
    end;
  begin
    Statement;
    while Symbol = SSemiColon do
    begin
      GetSym;
      Statement;
    end;
  end;
  function Declarations(TablePosition: Integer; lev: Integer): Integer;
  var
    DataPos: Integer;
    InitTablePos: Integer;
    InitCodePos: Integer;

    procedure Enter(Typ: TIdentType);
    begin
      inc(TablePosition);
      if TablePosition > Length(Table) - 1 then
      SetLength(Table, Length(Table) + 16);
      with Table[TablePosition] do
      begin
        name := ID;
        kind := Typ;
        case kind of
          itVariable: begin
            level := lev;
            adr := DataPos;
            inc(DataPos);
          end;
          itConstant : val   := num;
          itProcedure: level := lev;
        end;
      end;
    end;
    procedure ProcedureDecl;
    var
      ProcedureName: String;
      ProcTablePos: Integer;
    begin
      Expect(sProcedure);
      GetSym;
      Expect(sIdent);
      Enter(itProcedure);
      ProcedureName := ID;

      GetSym;
      Expect(sSemiColon);
      GetSym;

      ProcTablePos := Declarations(TablePosition,lev+1);
      Expect(sBegin);
      GetSym;

      StatementSequence(ProcTablePos, lev+1);
      Expect(sEnd);
      GetSym;

      Expect(sIdent);
      if ProcedureName <> ID then
      begin
        Error(Format(ERR_PARSER_WRONG_PROCEDURE_ENDED,
        [ProcedureName, ID]));
      end;
      GetSym;
      Expect(sSemiColon);

      GenCode(opr,0,0);  // return back to sub caller
      GetSym;
    end;
    procedure ConstDecl;
    begin
      Expect(sIdent);
      GetSym;
      Expect(sEqual);
      GetSym;
      Expect(sInteger);
      Enter(itConstant);
      GetSym;
    end;
    procedure VarDecl;
    begin
      Expect(sIdent);
      Enter(itVariable);
      GetSym;
      while Symbol = sComma do
      begin
        GetSym;
        Expect(sIdent);
        Enter(itVariable);
        GetSym;
      end;
    end;
  begin
    DataPos := 3;
    InitTablePos := TablePosition;
    InitCodePos := cx;
    Table[TablePosition].adr := cx;

    GenCode(jmp,0,0);
    while Symbol in [sVar, sConst, sProcedure] do
    case Symbol of
      sVar: begin
        GetSym;
        VarDecl;
        Expect(sSemiColon);
        GetSym;
        while Symbol = sIdent do
        begin
          VarDecl;
          Expect(sSemiColon);
          GetSym;
        end;
      end;
      sConst: begin
        GetSym;
        ConstDecl;
        Expect(sSemiColon);
        GetSym;
        while Symbol = sIdent do
        begin
          ConstDecl;
          Expect(sSemiColon);
          GetSym;
        end;
      end;
      sProcedure: begin
        ProcedureDecl;
      end;
    end;

    insArr[Table[InitTablePos].adr].a := cx;
    with Table[InitTablePos] do
    begin
      adr := cx;
      size := DataPos;
    end;

    // allocate memory space
    GenCode(int,0,DataPos);
    result := TablePosition;
  end;
var
  TablePosition: Integer;
  UnitName: String;
begin
  Expect(sUnit);
  GetSym;
  Expect(sIdent);
  UnitName := id;
  showmessage(unitname);
  GetSym;
  Expect(sSemiColon);
  GetSym;

  TablePosition := Declarations(0,0);
  Expect(sBegin);
  GetSym;
  StatementSequence(TablePosition,0);
  Expect(sEnd);

  // the end
  GenCode(jmp,0,0);

  GetSym;
  Expect(sIdent);
  if UnitName <> ID then
    raise Exception.Create(Format(
    '%d: Warning: Module ID <> End ID. Code already generated.',
    [Line]));

  GetSym;
  Expect(sSemiColon);
  GetSym;

  if Symbol <> sNone then
  raise Exception.Create(Format(
    '%d: Code after unit END is ignored!',
    [Line]));
end;

procedure LexScanner(filename: String);
var
  i: Integer;
  s: String;
begin
  try
    try
      if FileStream <> nil then
      FileStream.Free;

      FileStream := TFileStream.Create(filename, fmOpenRead);
      FileStream.Seek(0, soFromBeginning);

      ch := ' ';
      Line := 1;
      cx := 0;
      SetLength(Table,1);
      GetSym;
      Module;

      if FileStream <> nil then
      FileStream.Free;
      FileStream :=
      TFileStream.Create(
        ChangeFileExt(filename,'.bin'),
        fmOpenWrite);
      FileStream.Seek(0, soFromBeginning);

      showmessage('size: ' + inttostr(sizeof(insArr)));
//      for i := 1 to sizeof(insArr) do
//      Write(F,insArr[i]);

      with Form2.FEditorFrame.Memo2.Lines do
      begin
        Clear;
        Add('Done, no syntax errors detected... Code success');
        Add(Format('# Instructions: %d',[cx]));
        Add(Format('# Code size   : %d',[(cx) * sizeof(insArr)]));
      end;

      FileStream := TFileStream.Create(s, fmOpenRead);
      FileStream.Seek(0,soFromBeginning);

      SetLength(insArr,4);

      for i := 0 to Length(insArr) - 1 do
      begin
        FileStream.Read(insArr[i], sizeof(TInstruction));
        GetMem(ins, sizeof(TInstruction));
        ins.f := insArr[i].f;
        ShowMessage(Format('OpCode: %s', [System.TypInfo.GetEnumName(TypeInfo(TOpCode), Ord(insArr[i].f))]));
      end;
      ShowMessage(''
      + 'fs: ' + inttostr(FileStream.Size) + #13#10
      + 'is: ' + inttostr(sizeof(insArr)) + #13#10
      + 'ns: ' + inttostr(FileStream.Size div sizeof(TInstruction)));

      Emulate;

      ShowMessage('33333');
//      SetLength(Instructions,0);

    except
      on E: Exception do
      begin
        ShowMessage('Exception'
        + #13#10
        + 'Message: '
        + E.Message);
      end;
    end;
  finally
    //CloseFile(F);
    //CloseFile(inFile);
  end;
end;

end.
