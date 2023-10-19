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

var
  inFile: File;
  Line: Integer;

  procedure LexScanner(filename: String);
  procedure LexParser;

implementation

uses Vcl.Dialogs, System.SysUtils, Unit2;

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
  Instruction = packed record
    f: TOpCode; // command
    l: Byte;    // level
    a: Integer; // address
  end;

  TInstructions = Array of Instruction;

var
  Table: TIdentList;
  Code: TInstructions;

  cx: Integer;  // code position

  ID: String;
  num: Integer;

var
  ch: Char;
  str: String;
  Symbol: TSymbol;

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

procedure Expect(Expected: TSymbol);
begin
  if Symbol <> Expected then
  ErrorExpected([Expected], Symbol);
end;

procedure GenCode(f: TOpCode; l,a: Integer);
begin
  if cx <> Length(code) - 1 then
  SetLength(code, Length(code) + 64);
  Code[cx].f := f;
  Code[cx].a := a;
  Code[cx].l := l;
  inc (cx);
end;

procedure GetSym;
  procedure GetCh;
  begin
    if not Eof(inFile) then
    BlockRead(inFile, ch, 1) else
    ch := ' ';
    ch := UpCase(ch);  // case in-sensitive

    if ch = #13 then inc(Line);
    if Ord(ch) < Ord(' ') then ch := ' ';
  end;
  var i: TSymbol;
begin
  while true do
  begin
    str := '';
    Symbol := sNone;

    while (ch = ' ') and not Eof(inFile) do
    GetCh;

    if Eof(inFile) then
    exit;

    case ch of
      // ident/reserved word
      'A'..'Z', '_':
      begin
        while ch in ['A'..'Z','_','0'..'9'] do
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
      ';','+','-','=','#',',','.','*','/':
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
      ':','<','>':
      begin
        str := ch;
        GetCh;
        if ch = '=' then str := str + ch;
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
      '(',')':
      begin
        str := ch;
        GetCh;
        if (str = '(') and (ch = '*') then
        begin
          // skip comment
          GetCh;
          while true do
          begin
            GetCh;
            if ch = '*' then
            begin
              GetCh;
              if ch = ')' then
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
      '0'..'9','$':
      begin
        Symbol := sInteger;
        str := ch;
        GetCh;
        if str = '$' then  // hex value
        begin
          while ch in ['0'..'9','A'..'F'] do
          begin
            str := str + ch;
            GetCh;
          end;
          exit;
        end else
        begin
          while ch in ['0'..'9'] do
          begin
            str := str + ch;
            GetCh;
          end;
          exit;
        end;
      end;
      else Error(ERR_SCANNER_UNEXPECTED_CHAR);
    end;
    Assert(Symbol <> sUnknown);
  end;
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

          Code[CodePosition1].a := cx;

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

            Code[CodePosition2].a := cx;
            CodePosition2 := cx;
            GenCode(jmp,0,0);

            Code[CodePosition1].a := cx;
          end;
          if Symbol = sElse then
          begin
            GetSym;
            StatementSequence(TablePosition,lev);
            Expect(sEnd);
            GetSym;
            Code[CodePosition2].a := cx;
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

          Code[CodePosition1].a := cx;
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
        Error(Format(ERR_PARSER_WRONG_PROCEDURE_ENDED,
        [ProcedureName, ID]));
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
    begin
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
    end;

    Code[Table[InitTablePos].adr].a := cx;
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
  showmessage('semi');
  TablePosition := Declarations(0,0);
  Expect(sBegin);
  GetSym;
  StatementSequence(TablePosition,0);
  Expect(sEnd);

  // the end
  GenCode(jmp,0,0);

  GetSym;
  Expect(sIdent);
  GetSym;

  if UnitName <> ID then
    raise Exception.Create(Format(
    '%d: Warning: Module ID <> End ID',
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
  F: File of Instruction;
  i: Integer;
begin
  try
    try
      AssignFile(inFile, filename);
      Reset(inFile,1);
      ch := ' ';
      Line := 1;
      cx := 0;
      SetLength(Table,1);
      GetSym;
      Module;
            showmessage(id);

      AssignFile(F,ChangeFileExt(filename,'.bin'));
      ReWrite(F);

      i := 0;
      while i < cx do
      begin
        Write(F,Code[i]);
        inc(i);
      end;

    except
      on E: Exception do
      begin
        ShowMessage('Exception'
        + #13#10
        + 'Message: '
        + E.Message);
        exit;
      end;
    end;
  finally
    CloseFile(F);
    CloseFile(inFile);
  end;

  with Form2.FEditorFrame.Memo2.Lines do
  begin
    Clear;
    Add('Done, no syntax errors detected... Code success');
    Add(Format('# Instructions: %d',[cx]));
    Add(Format('# Code size   : %d',[(cx) * sizeof(Instruction)]));
  end;
end;

procedure LexParser;
begin

end;

end.
