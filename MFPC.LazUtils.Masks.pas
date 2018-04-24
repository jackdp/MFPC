{
 *****************************************************************************
  This file is part of LazUtils.

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}

{
  jpmod: Removed dependency on "Classes" unit
  2018.02.11
  =========================================================
  FPC 3.1.1 rev. 37820 (trunk)
  =========================================================
  Original file: lazarus\components\lazutils\masks.pas

  Removed routines & classes:
    TParseStringList (c)
    TMaskList (c)
    MatchesMaskList (f)
    MatchesWindowsMaskList (f)

}
unit MFPC.LazUtils.Masks;

{$mode objfpc}{$H+}

interface

uses
  // For Smart Linking: Do not use the LCL!
  //Classes,
  SysUtils,
  //Contnrs,
  MFPC.LazUtilsStrConsts, MFPC.LazUtils.LazUTF8;

type
  TMaskCharType = (mcChar, mcCharSet, mcAnyChar, mcAnyText);

  TCharSet = set of Char;
  PCharSet = ^TCharSet;

  TUtf8Char = String[7];

  TMaskChar = record
    case CharType: TMaskCharType of
      mcChar: (CharValue: TUtf8Char);
      mcCharSet: (Negative: Boolean; SetValue: PCharSet);
      mcAnyChar, mcAnyText: ();
  end;

  TMaskString = record
    MinLength: Integer;
    MaxLength: Integer;
    Chars: Array of TMaskChar;
  end;

  { TMask }

  TMask = class
  private
    FMask: TMaskString;
    fCaseSensitive: Boolean;
    fInitialMask: String;
    procedure InitMaskString(const AValue: String; const CaseSensitive: Boolean);
    procedure ClearMaskString;
  public
    constructor Create(const AValue: String; const CaseSensitive: Boolean = False);
    destructor Destroy; override;

    function Matches(const AFileName: String): Boolean;
    function MatchesWindowsMask(const AFileName: String): Boolean;
  end;


function MatchesMask(const FileName, Mask: String; const CaseSensitive: Boolean = False): Boolean;
function MatchesWindowsMask(const FileName, Mask: String; const CaseSensitive: Boolean = False): Boolean;

implementation

//Utf8 helper functions

function GetCodePoint(const S: String; const Index: PtrInt): TUTF8Char;
//equivalent for Result := S[Index], but for Utf8 encoded strings
var
  p: PChar;
  PLen: PtrInt;
  Res: AnsiString; //intermediate needed for PChar -> String -> ShortString assignement
begin
  Result := '';
  p := UTF8CodepointStart(PChar(S), Length(S), Index - 1); //zero-based call
  //determine the length in bytes of this UTF-8 character
  PLen := UTF8CodepointSize(p);
  Res := p;
  //Set correct length for Result (otherwise it returns all chars up to the end of the original string)
  SetLength(Res,PLen);
  Result := Res;
end;


function MatchesMask(const FileName, Mask: String; const CaseSensitive: Boolean): Boolean;
var
  AMask: TMask;
begin
  AMask := TMask.Create(Mask, CaseSensitive);
  try
    Result := AMask.Matches(FileName);
  finally
    AMask.Free;
  end;
end;

function MatchesWindowsMask(const FileName, Mask: String; const CaseSensitive: Boolean): Boolean;
var
  AMask: TMask;
begin
  AMask := TMask.Create(Mask, CaseSensitive);
  try
    Result := AMask.MatchesWindowsMask(FileName);
  finally
    AMask.Free;
  end;
end;

{$region '                           TMask                                   '}

procedure TMask.InitMaskString(const AValue: String; const CaseSensitive: Boolean);
var
  I: Integer;
  SkipAnyText: Boolean;

  procedure CharSetError;
  begin
    raise EConvertError.CreateFmt(lrsInvalidCharSet, [AValue]);
  end;

  procedure AddAnyText;
  begin
    if SkipAnyText then
    begin
      Inc(I);
      Exit;
    end;

    SetLength(FMask.Chars, Length(FMask.Chars) + 1);
    FMask.Chars[High(FMask.Chars)].CharType := mcAnyText;

    FMask.MaxLength := MaxInt;
    SkipAnyText := True;
    Inc(I);
  end;

  procedure AddAnyChar;
  begin
    SkipAnyText := False;

    SetLength(FMask.Chars, Length(FMask.Chars) + 1);
    FMask.Chars[High(FMask.Chars)].CharType := mcAnyChar;

    Inc(FMask.MinLength);
    if FMask.MaxLength < MaxInt then Inc(FMask.MaxLength);

    Inc(I);
  end;

  procedure AddCharSet;
  var
    CharSet: TCharSet;
    Valid: Boolean;
    C, Last: Char;
    CP: TUtf8Char;
  begin
    SkipAnyText := False;

    SetLength(FMask.Chars, Length(FMask.Chars) + 1);
    FMask.Chars[High(FMask.Chars)].CharType := mcCharSet;

    Inc(I);
    if (I <= Utf8Length(AValue)) and (GetCodePoint(AValue,I) = '!') then
    begin
      FMask.Chars[High(FMask.Chars)].Negative := True;
      Inc(I);
    end
    else FMask.Chars[High(FMask.Chars)].Negative := False;

    Last := '-';
    CharSet := [];
    Valid := False;
    while I <= Utf8Length(AValue) do
    begin
      CP := GetCodePoint(AValue,I);
      //CharSets can only contain 1-byte codepoints
      if (Length(CP) <> 1) then CharSetError;
      case CP of
        '-':
          begin
            if Last = '-' then CharSetError;
            Inc(I);
            if (I > Utf8Length(AValue)) then CharSetError;
            CP := GetCodePoint(AValue, I);
            if (Length(CP) <> 1) then CharSetError;
            if fCaseSensitive then
            begin
              //writeln('MaskUtf8: Set:  ' + Last + '-' + (CP[1]));
              for C := Last to CP[1] do
                Include(CharSet, C)
            end
            else
            begin
              //writeln('MaskUtf8: Set:  ' + Last + '-' + UpCase(CP[1]));
              for C := Last to LowerCase(CP[1]) do
                Include(CharSet, C)
            end;
            Inc(I);
          end;
        ']':
          begin
            Valid := True;
            Break;
          end;
        else
        begin
          if fCaseSensitive then
            Last := CP[1]
          else
            Last := LowerCase(CP[1]);
          Include(CharSet, Last);
          Inc(I);
        end;
      end;
    end;
    if (not Valid) or (CharSet = []) then CharSetError;

    New(FMask.Chars[High(FMask.Chars)].SetValue);
    FMask.Chars[High(FMask.Chars)].SetValue^ := CharSet;

    Inc(FMask.MinLength);
    if FMask.MaxLength < MaxInt then Inc(FMask.MaxLength);

    Inc(I);
  end;

  procedure AddChar;
  begin
    SkipAnyText := False;

    SetLength(FMask.Chars, Length(FMask.Chars) + 1);
    with FMask.Chars[High(FMask.Chars)] do
    begin
      CharType := mcChar;
      if fCaseSensitive then
        CharValue := GetCodePoint(AValue,I)
      else
        CharValue := Utf8LowerCase(GetCodePoint(AValue,I));
    end;

    Inc(FMask.MinLength);
    if FMask.MaxLength < MaxInt then Inc(FMask.MaxLength);

    Inc(I);
  end;

begin
  fCaseSensitive:=CaseSensitive;
  SetLength(FMask.Chars, 0);
  FMask.MinLength := 0;
  FMask.MaxLength := 0;
  SkipAnyText := False;

  I := 1;
  while I <= Utf8Length(AValue) do
  begin
    case GetCodePoint(AValue,I) of
      '*': AddAnyText;
      '?': AddAnyChar;
      '[': AddCharSet;
      else AddChar;
    end;
  end;
end;

procedure TMask.ClearMaskString;
var
  I: Integer;
begin
  for I := 0 to High(FMask.Chars) do
    if FMask.Chars[I].CharType = mcCharSet then
      Dispose(FMask.Chars[I].SetValue);
end;

constructor TMask.Create(const AValue: String; const CaseSensitive: Boolean);

begin
  fInitialMask := AValue;
  fCaseSensitive := CaseSensitive;
  InitMaskString(AValue, CaseSensitive);
end;

destructor TMask.Destroy;
begin
  ClearMaskString;
  inherited Destroy;
end;

function TMask.Matches(const AFileName: String): Boolean;
var
  L: Integer;
  S: String;

  function MatchToEnd(MaskIndex, CharIndex: Integer): Boolean;
  var
    I, J: Integer;
    CP: TUtf8Char;
  begin
    Result := False;

    for I := MaskIndex to High(FMask.Chars) do
    begin
      case FMask.Chars[I].CharType of
        mcChar:
          begin
            if CharIndex > L then Exit;
            //DebugLn('Match ' + S[CharIndex] + '<?>' + FMask.Chars[I].CharValue);
            if GetCodePoint(S,CharIndex) <> FMask.Chars[I].CharValue then Exit;
            Inc(CharIndex);
          end;
        mcCharSet:
          begin
            if CharIndex > L then Exit;
            CP := GetCodePoint(S, CharIndex);
            //There are only 1-byte codepoints in charsets
            if (Length(CP) <> 1) then Exit;
            if FMask.Chars[I].Negative xor
               (Cp[1] in FMask.Chars[I].SetValue^) then Inc(CharIndex)
            else Exit;
          end;
        mcAnyChar:
          begin
            //CP := GetCodePoint(S, CharIndex);
            //writeln('mcAnyChar: CP = ',UTF8ToConsole(CP),' CharIndex = ',CharIndex);
            if CharIndex > L then Exit;
            Inc(CharIndex);
          end;
        mcAnyText:
          begin
            if I = High(FMask.Chars) then
            begin
              Result := True;
              Exit;
            end;

            for J := CharIndex to L do
              if MatchToEnd(I + 1, J) then
              begin
                Result := True;
                Exit;
              end;
          end;
      end;
    end;

    Result := CharIndex > L;
  end;

begin
  Result := False;
  L := Utf8Length(AFileName);
  if L = 0 then
  begin
    if FMask.MinLength = 0 then Result := True;
    Exit;
  end;

  if (L < FMask.MinLength) or (L > FMask.MaxLength) then Exit;
  if fCaseSensitive then
    S := AFileName
  else
  begin
    S := Utf8LowerCase(AFileName);
    L := Utf8Length(S);
  end;
  Result := MatchToEnd(0, 1);
end;

function TMask.MatchesWindowsMask(const AFileName: String): Boolean;
var
  NewMaskValue, Ext: String;
begin
  // treat initial mask differently for special cases:
  // foo*.* -> foo*
  // foo*. -> match foo*, but muts not have an extension
  // *. -> any file without extension ( .foo is a filename without extension according to Windows)
  // foo. matches only foo but not foo.txt
  // foo.* -> match either foo or foo.*

  if (Utf8Length(fInitialMask) > 2) and (Utf8RightStr(fInitialMask,3) = '*.*') then
  // foo*.*
  begin
    NewMaskValue := Utf8Copy(fInitialMask,1,Utf8Length(fInitialMask)-2);
    ClearMaskString;
    InitMaskString(NewMaskValue, fCaseSensitive);
    Result := Matches(AFileName);
    //Restore initial state of FMask
    ClearMaskString;
    InitMaskString(fInitialMask, fCaseSensitive);
  end
  //else if (Length(fInitialMask) > 1) and (RightStr(fInitialMask,2) = '*.') then
  else if (Utf8Length(fInitialMask) > 1) and (GetCodePoint(fInitialMask,Utf8Length(fInitialMask)) = '.') then

  //foo*. or *. or foo.
  begin
    //if AFileName has an extension then Result is False, otherwise see if it matches foo*/foo
    //a filename like .foo under Windows is considered to be a file without an extension
    Ext := ExtractFileExt(AFileName);
    if (Ext = '') or (Ext = AFileName) then
    begin
      NewMaskValue := Utf8Copy(fInitialMask,1,Utf8Length(fInitialMask)-1);
      ClearMaskString;
      InitMaskString(NewMaskValue, fCaseSensitive);
      Result := Matches(AFileName);
      //Restore initial state of FMask
      ClearMaskString;
      InitMaskString(fInitialMask, fCaseSensitive);
    end
    else
    begin
      Result := False;
    end;
  end
  else if (Utf8Length(fInitialMask) > 2) and (Utf8RightStr(fInitialMask,2) = '.*') then
  //foo.*  (but not '.*')
  begin
    //First see if we have 'foo'
    if fCaseSensitive then
      Result := (AFileName = Utf8Copy(fInitialMask,1,Utf8Length(fInitialMask)-2))
    else
      Result := (Utf8CompareText(AFileName,Utf8Copy(fInitialMask,1,Utf8Length(fInitialMask)-2)) = 0);
    if not Result then Result := Matches(AFileName);
  end
  else
  //all other cases just call Matches()
  begin
    Result := Matches(AFileName);
  end;
end;
{$endregion TMask}


end.
