{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 1999-2000 Erik WachtMeester.

    File which provides TIniFile and friends.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{* Original disclaimer:
 * FCL inifiles.pp rewrite by Erik Wachtmeester (erikw@hotelconcepts.com)
 *
 * Proposed replacement for inifiles.pp v 1.8
 *
 * This version is Borland Delphi 5 compatible, implementing the classes
 * TCustomIniFile, TIniFile and TMemIniFile, with all the public
 * properties and methods that Delphi 5 implements.
 *
 * (inifiles.pp v 1.8 only implements TIniFile with some properties and
 *  methods missing, and some functionality added)
 *
 * In order to stay compatible with v 1.8, I added:
 * - TIniFile can be created and loaded from, and saved to a stream.
 * - ReadSectionRaw method (although it doesn't add empty lines to the
 *   TStrings recipient like v 1.8, since empty lines aren't stored in
 *   the SectionList object structure)
 * - ReadInteger supports '0x' type hex formats
 * - Comment support (this isn't standard in ini files)
 * - EscapeLineFeeds creation parameter
 *
 * Since the SectionList object structure is very different from the
 * way Delphi 5 accesses ini files (Delphi mostly uses Windows calls
 * like GetPrivateProfileString, etc.) it's completely platform
 * independant, and probably faster.
 * The only drawback is memory consumption: all sections, keys and
 * values are kept in memory. But same goes for inifiles.pp v 1.8
 * (the FFileBuffer member) and for Delphi's TMemIniFile.
 * Anyway, Windows restricts ini files to 64K max, so this shouldn't be
 * too much of a problem.
 *
 *}

{
  jpmod: Excerpt from the "Classes" unit.
  2018.02.11   
  
  TStringList replaced with TJPStrList
  
  =================================================
  FPC 3.1.1 rev. 37820 (trunk)
  =================================================
  Original files:
    fpcsrc\rtl\objpas\classes\classesh.inc
}

unit MFPC.IniFiles;
{$mode objfpc}
{$H+}

{$WARN 5024 off : Parameter "$1" not used}
{$WARN 5057 off : Local variable "$1" does not seem to be initialized}
{$WARN 4055 off : Conversion between ordinals and pointers is not portable}
{$WARN 4056 off : Conversion between ordinals and pointers is not portable}
{$WARN 5066 off : Symbol "$1" is deprecated: "$2"}
interface

//uses classes, sysutils, contnrs;
uses
  SysUtils,
  MFPC.Contnrs,
  MFPC.Classes.SHARED, MFPC.Classes.Lists, MFPC.Classes.Streams, JPL.StrList;



type

  { TM_StringHash }

  TM_StringHash  = class
  private
    FAddReplacesExisting: Boolean;
    FHashList : TM_FPDataHashTable;
  public
    constructor Create(ACapacity : Cardinal = 256);
    destructor Destroy;override;
    procedure Add(const Key: string; Value: Integer);
    procedure Clear;
    function Modify(const Key: string; Value: Integer): Boolean;
    procedure Remove(const Key: string);
    function ValueOf(const Key: string): Integer;
    Property AddReplacesExisting : Boolean Read FAddReplacesExisting Write FAddReplacesExisting;
  end;

  { THashedStringList }

  //THashedStringList = class(TStringList)
  //private
  //  FValueHash: TFPHashList;
  //  FNameHash: TFPHashList;
  //  FValueHashValid: Boolean;
  //  FNameHashValid: Boolean;
  //  procedure UpdateValueHash;
  //  procedure UpdateNameHash;
  //protected
  //  procedure Changed; override;
  //public
  //  destructor Destroy; override;
  //  function IndexOf(const S: String): Integer; override;
  //  function IndexOfName(const Name: String): Integer; override;
  //end;

  TM_IniFileKey = class
  Private
    FIdent: string;
    FValue: string;
  public
    constructor Create(const AIdent, AValue: string);
    property Ident: string read FIdent write FIdent;
    property Value: string read FValue write FValue;
  end;

  TM_IniFileKeyList = class(TM_List)
  private
    function GetItem(Index: integer): TM_IniFileKey;
    function KeyByName(const AName: string; CaseSensitive : Boolean): TM_IniFileKey;
  public
    destructor Destroy; override;
    procedure Clear; override;
    property Items[Index: integer]: TM_IniFileKey read GetItem; default;
  end;

  TM_IniFileSection = class
  private
    FName: string;
    FKeyList: TM_IniFileKeyList;
  public
    Function Empty : Boolean;
    constructor Create(const AName: string);
    destructor Destroy; override;
    property Name: string read FName;
    property KeyList: TM_IniFileKeyList read FKeyList;
  end;

  TM_IniFileSectionList = class(TM_List)
  private
    function GetItem(Index: integer): TM_IniFileSection;
    function SectionByName(const AName: string; CaseSensitive : Boolean): TM_IniFileSection;
  public
    destructor Destroy; override;
    procedure Clear;override;
    property Items[Index: integer]: TM_IniFileSection read GetItem; default;
  end;

  TIniFileOption = (ifoStripComments,    // Strip comments when reading file
                    ifoStripInvalid,     // Strip invalid lines when reading file.
                    ifoEscapeLineFeeds, // Escape linefeeds when reading file.
                    ifoCaseSensitive,   // Use Case sensitive section/key names
                    ifoStripQuotes,     // Strip quotes when reading string values.
                    ifoFormatSettingsActive, // Use format settings when writing date/float etc.
                    ifoWriteStringBoolean // Write booleans as string
                    );
  TIniFileOptions = Set of TIniFileOption;

  TSectionValuesOption = (svoIncludeComments,svoIncludeInvalid, svoIncludeQuotes);
  TSectionValuesOptions = set of TSectionValuesOption;

  { TM_CustomIniFile }

  TM_CustomIniFile = class
  Private
    FBoolFalseStrings: TStringArray;
    FBoolTrueStrings: TStringArray;
    FFileName: string;
    FOptions: TIniFileOptions;
    FSectionList: TM_IniFileSectionList;
    function GetOption(AIndex: TIniFileOption): Boolean;
    procedure SetOption(AIndex: TIniFileOption; AValue: Boolean);
    procedure SetOptions(AValue: TIniFileOptions);
  public
    FormatSettings: TFormatSettings;
    constructor Create(const AFileName: string; AOptions : TIniFileOptions = []); virtual;
    constructor Create(const AFileName: string; AEscapeLineFeeds : Boolean); virtual;
    destructor Destroy; override;
    Procedure SetBoolStringValues(ABoolValue : Boolean; Values : Array of string);
    function SectionExists(const Section: string): Boolean; virtual;
    function ReadString(const Section, Ident, Default: string): string; virtual; abstract;
    procedure WriteString(const Section, Ident, Value: String); virtual; abstract;
    function ReadInteger(const Section, Ident: string; Default: Longint): Longint; virtual;
    procedure WriteInteger(const Section, Ident: string; Value: Longint); virtual;
    function ReadInt64(const Section, Ident: string; Default: Int64): Int64; virtual;
    procedure WriteInt64(const Section, Ident: string; Value: Int64); virtual;
    function ReadBool(const Section, Ident: string; Default: Boolean): Boolean; virtual;
    procedure WriteBool(const Section, Ident: string; Value: Boolean); virtual;
    function ReadDate(const Section, Ident: string; Default: TDateTime): TDateTime; virtual;
    function ReadDateTime(const Section, Ident: string; Default: TDateTime): TDateTime; virtual;
    function ReadFloat(const Section, Ident: string; Default: Double): Double; virtual;
    function ReadTime(const Section, Ident: string; Default: TDateTime): TDateTime; virtual;
    function ReadBinaryStream(const Section, Name: string; Value: TM_Stream): Integer; virtual;
    procedure WriteDate(const Section, Ident: string; Value: TDateTime); virtual;
    procedure WriteDateTime(const Section, Ident: string; Value: TDateTime); virtual;
    procedure WriteFloat(const Section, Ident: string; Value: Double); virtual;
    procedure WriteTime(const Section, Ident: string; Value: TDateTime); virtual;
    procedure WriteBinaryStream(const Section, Name: string; Value: TM_Stream); virtual;
    procedure ReadSection(const Section: string; Strings: TJPStrList); virtual; abstract;
    procedure ReadSections(Strings: TJPStrList); virtual; abstract;
    procedure ReadSectionValues(const Section: string; Strings: TJPStrList; Options : TSectionValuesOptions); virtual;    overload;
    procedure ReadSectionValues(const Section: string; Strings: TJPStrList); virtual;overload;
    procedure EraseSection(const Section: string); virtual; abstract;
    procedure DeleteKey(const Section, Ident: String); virtual; abstract;
    procedure UpdateFile; virtual; abstract;
    function ValueExists(const Section, Ident: string): Boolean; virtual;
    property FileName: string read FFileName;
    Property Options : TIniFileOptions Read FOptions Write SetOptions;
    property EscapeLineFeeds: boolean index ifoEscapeLineFeeds Read GetOption ;deprecated 'Use options instead';
    Property CaseSensitive : Boolean index ifoCaseSensitive Read GetOption Write SetOption; deprecated  'Use options instead';
    Property StripQuotes : Boolean index ifoStripQuotes Read GetOption Write SetOption; deprecated 'Use options instead';
    Property FormatSettingsActive : Boolean index ifoFormatSettingsActive Read GetOption Write SetOption;deprecated  'Use options instead';
    Property BoolTrueStrings : TStringArray Read FBoolTrueStrings Write FBoolTrueStrings;
    Property BoolFalseStrings : TStringArray Read FBoolFalseStrings Write FBoolFalseStrings;
  end;

  { TM_IniFile }

  TM_IniFile = class(TM_CustomIniFile)
  Private
    FStream: TM_Stream;
    FCacheUpdates: Boolean;
    FDirty : Boolean;
    FBOM : String;
    procedure FillSectionList(AStrings: TJPStrList);
    Procedure DeleteSection(ASection : TM_IniFileSection);
    Procedure MaybeDeleteSection(ASection : TM_IniFileSection);
    procedure SetCacheUpdates(const AValue: Boolean);
  protected
    procedure MaybeUpdateFile;
    property Dirty : Boolean Read FDirty;
  public
    constructor Create(const AFileName: string; AOptions : TIniFileoptions = []); overload; override;
    //constructor Create(AStream: TM_Stream; AOptions : TIniFileoptions = []); overload;
    //constructor Create(AStream: TM_Stream; AEscapeLineFeeds : Boolean); overload; deprecated 'Use Options argument instead';
    destructor Destroy; override;
    function ReadString(const Section, Ident, Default: string): string; override;
    procedure WriteString(const Section, Ident, Value: String); override;
    procedure ReadSection(const Section: string; Strings: TJPStrList); override;
    procedure ReadSectionRaw(const Section: string; Strings: TJPStrList);
    procedure ReadSections(Strings: TJPStrList); override;
    procedure ReadSectionValues(const Section: string; Strings: TJPStrList; AOptions : TSectionValuesOptions = [svoIncludeInvalid]); overload; override;
    procedure EraseSection(const Section: string); override;
    procedure DeleteKey(const Section, Ident: String); override;
    procedure UpdateFile; override;
    property Stream: TM_Stream read FStream;
    property CacheUpdates : Boolean read FCacheUpdates write SetCacheUpdates;
  end;

  TM_MemIniFile = class(TM_IniFile)
  public
    constructor Create(const AFileName: string; AEscapeLineFeeds : Boolean = False); overload; override;
    procedure Clear;
    procedure GetStrings(List: TJPStrList);
    procedure Rename(const AFileName: string; Reload: Boolean);
    procedure SetStrings(List: TJPStrList);
  end;


implementation

Resourcestring
  SErrCouldNotCreatePath = 'Could not create directory "%s"';

const
   Brackets  : array[0..1] of Char = ('[', ']');
   Separator : Char = '=';
   Comment   : Char = ';';
   LF_Escape : Char = '\';

function CharToBool(AChar: char): boolean;
begin
  Result := (Achar = '1');
end;

function BoolToChar(ABool: boolean): char;
begin
  if ABool then
    Result := '1'
  else
    Result := '0';
end;

function IsComment(const AString: string): boolean;
begin
  Result := False;
  if AString > '' then
    Result := (Copy(AString, 1, 1) = Comment);
end;

{ TM_StringHash }

constructor TM_StringHash.Create(ACapacity : Cardinal = 256);
begin
  FHashList := TM_FPDataHashTable.Create;
end;

destructor TM_StringHash.Destroy;
begin
  FreeAndNil(FHashList);
  inherited;
end;

procedure TM_StringHash.Add(const Key: string; Value: Integer);
begin
  if Not (AddReplacesExisting and Modify(Key,Value)) then
    FHashList.Add(Key, Pointer(Value));
end;

procedure TM_StringHash.Clear;
begin
  FHashList.Clear;
end;

function TM_StringHash.Modify(const Key: string; Value: Integer): Boolean;
Var
  AIndex : Integer;
  Node : TM_HTDataNode;

begin
  Node := TM_HTDataNode(FHashList.Find(Key));
  Result:=Assigned(Node);
  if Result Then
    Node.Data:=Pointer(Value);
end;

procedure TM_StringHash.Remove(const Key: string);

begin
  FHashList.Delete(Key);
end;

function TM_StringHash.ValueOf(const Key: string): Integer;
Var
  N : TM_HTDataNode;
begin
  N:=TM_HTDataNode(FHashList.Find(Key));
  If Assigned(N) then
    Result:=PTrInt(N.Data)
  else
    Result:=-1;
end;

{ THashedStringList }

//destructor THashedStringList.Destroy;
//begin
//  FreeAndNil(FValueHash);
//  FreeAndNil(FNameHash);
//  inherited Destroy;
//end;
//
//function THashedStringList.IndexOf(const S: String): Integer;
//var
//  I: Integer;
//begin
//  if not FValueHashValid then
//    UpdateValueHash;
//
//  if CaseSensitive then
//    I := FValueHash.FindIndexOf(S)
//  else
//    I := FValueHash.FindIndexOf(AnsiUpperCase(S));
//  if I >= 0 then
//    Result := Integer(FValueHash[I])-1
//  else
//    Result := -1;
//end;
//
//function THashedStringList.IndexOfName(const Name: String): Integer;
//var
//  I: Integer;
//begin
//  if not FNameHashValid then
//    UpdateNameHash;
//
//  if CaseSensitive then
//    I := FNameHash.FindIndexOf(Name)
//  else
//    I := FNameHash.FindIndexOf(AnsiUpperCase(Name));
//  if I >= 0 then
//    Result := Integer(FNameHash[I])-1
//  else
//    Result := -1;
//end;
//
//procedure THashedStringList.Changed;
//begin
//  FValueHashValid := False;
//  FNameHashValid := False;
//  inherited Changed;
//end;
//
//procedure THashedStringList.UpdateValueHash;
//var
//  I: Integer;
//begin
//  if not Assigned(FValueHash) then
//    FValueHash := TFPHashList.Create
//  else
//    FValueHash.Clear;
//  for I := 0 to Count - 1 do
//    if CaseSensitive then
//      FValueHash.Add(Strings[I], Pointer(I+1))
//    else
//      FValueHash.Add(AnsiUpperCase(Strings[I]), Pointer(I+1));
//  FValueHashValid := True;
//end;
//
//procedure THashedStringList.UpdateNameHash;
//var
//  I: Integer;
//begin
//  if not Assigned(FNameHash) then
//    FNameHash := TFPHashList.Create
//  else
//    FNameHash.Clear;
//  for I := 0 to Count - 1 do
//    if CaseSensitive then
//      FNameHash.Add(Names[I], Pointer(I+1))
//    else
//      FNameHash.Add(AnsiUpperCase(Names[I]), Pointer(I+1));
//  FNameHashValid := True;
//end;

{ TM_IniFileKey }

constructor TM_IniFileKey.Create(const AIdent, AValue: string);
begin
  FIdent := AIdent;
  FValue := AValue;
end;

{ TM_IniFileKeyList }

function TM_IniFileKeyList.GetItem(Index: integer): TM_IniFileKey;
begin
  Result := nil;
  if (Index >= 0) and (Index < Count) then
    Result := TM_IniFileKey(inherited Items[Index]);
end;

function TM_IniFileKeyList.KeyByName(const AName: string; CaseSensitive : Boolean): TM_IniFileKey;
var
  i: integer;
begin
  Result := nil;
  if (AName > '') and not IsComment(AName) then
    If CaseSensitive then
      begin
      for i := 0 to Count-1 do
        if Items[i].Ident=AName then
          begin
          Result := Items[i];
          Break;
          end;
      end
    else
      for i := 0 to Count-1 do
        if CompareText(Items[i].Ident, AName) = 0 then begin
          Result := Items[i];
          Break;
        end;
end;

destructor TM_IniFileKeyList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TM_IniFileKeyList.Clear;
var
  i: integer;
begin
  for i := Count-1 downto 0 do
    Items[i].Free;
  inherited Clear;
end;

Function TM_IniFileSection.Empty : Boolean;

Var
  I : Integer;

begin
  Result:=True;
  I:=0;
  While Result and (I<KeyList.Count)  do
    begin
    result:=IsComment(KeyList[i].Ident);
    Inc(i);
    end;
end;


{ TM_IniFileSection }

constructor TM_IniFileSection.Create(const AName: string);
begin
  FName := AName;
  FKeyList := TM_IniFileKeyList.Create;
end;

destructor TM_IniFileSection.Destroy;
begin
  FKeyList.Free;
end;

{ TM_IniFileSectionList }

function TM_IniFileSectionList.GetItem(Index: integer): TM_IniFileSection;
begin
  Result := nil;
  if (Index >= 0) and (Index < Count) then
    Result := TM_IniFileSection(inherited Items[Index]);
end;

function TM_IniFileSectionList.SectionByName(const AName: string; CaseSensitive : Boolean): TM_IniFileSection;
var
  i: integer;
begin
  Result := nil;
  if (AName > '') and not IsComment(AName) then
    If CaseSensitive then
      begin
      for i:=0 to Count-1 do
        if (Items[i].Name=AName) then
          begin
          Result := Items[i];
          Break;
          end;
      end
    else
      for i := 0 to Count-1 do
        if CompareText(Items[i].Name, AName) = 0 then
          begin
          Result := Items[i];
          Break;
          end;
end;

destructor TM_IniFileSectionList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TM_IniFileSectionList.Clear;
var
  i: integer;
begin
  for i := Count-1 downto 0 do
    Items[i].Free;
  inherited Clear;
end;

{ TM_CustomIniFile }


function TM_CustomIniFile.GetOption(AIndex: TIniFileOption): Boolean;
begin
  Result:=AIndex in FOptions;
end;


procedure TM_CustomIniFile.SetOption(AIndex: TIniFileOption; AValue: Boolean);
begin
  if AIndex in [ifoStripComments,ifoStripInvalid] then
    Raise Exception.Create('Flags ifoStripComments or ifoStripInvalid must be set/unset in the constructor');
  if AValue then
    Include(FOptions,AIndex)
  else
    Exclude(FOptions,AIndex)
end;

procedure TM_CustomIniFile.SetOptions(AValue: TIniFileOptions);

Const
  CreateOnlyOptions = [ifoStripComments,ifoStripInvalid];
begin
  if FOptions=AValue then Exit;
  if (Foptions*CreateOnlyOptions)<>(AValue*CreateOnlyOptions) then
    Raise Exception.Create('Can only change StripComments or StripInvalid in constructor');
  FOptions:=AValue;
end;

constructor TM_CustomIniFile.Create(const AFileName: string; AOptions : TIniFileOptions = []);
begin
  FFileName := AFileName;
  FSectionList := TM_IniFileSectionList.Create;
  FOptions:=AOptions;
  FormatSettings := DefaultFormatSettings;
  with FormatSettings do begin
    DecimalSeparator := '.';
    ThousandSeparator := ',';
    ListSeparator := ';';
    DateSeparator := '/';
    TimeSeparator := ':';
    ShortDateFormat := 'yyyy/mm/dd';
    ShortTimeFormat := 'hh:nn';
    LongTimeFormat := 'hh:nn:ss';
  end;
end;

constructor TM_CustomIniFile.Create(const AFileName: string;
  AEscapeLineFeeds: Boolean);
begin
  if AEscapeLineFeeds then
    Create(AFileName,[ifoEscapeLineFeeds])
  else
    Create(AFileName,[])
end;

destructor TM_CustomIniFile.Destroy;
begin
  FSectionList.Free;
  inherited Destroy;
end;

procedure TM_CustomIniFile.SetBoolStringValues(ABoolValue: Boolean;
  Values: array of string);

Var
  A : TstringArray;
  I : Integer;

begin
  SetLength(A,Length(Values));
  For I:=0 to length(Values)-1 do
    A[i]:=Values[i];
  If AboolValue then
    FBoolTrueStrings:=A
  else
    FBoolFalseStrings:=A;
end;

function TM_CustomIniFile.SectionExists(const Section: string): Boolean;

Var
  S : TM_IniFileSection;

begin
  S:=FSectionList.SectionByName(Section,CaseSensitive);
  Result:=Assigned(S) and Not S.Empty;
end;

function TM_CustomIniFile.ReadInteger(const Section, Ident: string; Default: Longint): Longint;
begin
  // StrToInfDef() supports hex numbers prefixed with '0x' via val()
  Result := StrToIntDef(ReadString(Section, Ident, ''), Default);
end;

procedure TM_CustomIniFile.WriteInteger(const Section, Ident: string; Value: Longint);
begin
  WriteString(Section, Ident, IntToStr(Value));
end;

function TM_CustomIniFile.ReadInt64(const Section, Ident: string; Default: Int64
  ): Int64;
begin
  Result := StrToInt64Def(ReadString(Section, Ident, ''), Default);
end;

procedure TM_CustomIniFile.WriteInt64(const Section, Ident: string; Value: Int64);
begin
  WriteString(Section, Ident, IntToStr(Value));
end;

function IndexOfString(A : TStringArray; S : String) : integer;

begin
  Result:=Length(A)-1;
  While (Result>=0) and (CompareText(A[Result],S)<>0) do
    Dec(Result);
end;

function TM_CustomIniFile.ReadBool(const Section, Ident: string; Default: Boolean): Boolean;

var
  s: string;
begin
  Result := Default;
  s:=ReadString(Section, Ident, '');
  if s > '' then
    if (Length(FBoolTrueStrings)>0) or (Length(FBoolFalseStrings)>0) then
      begin
      if IndexOfString(FBoolTrueStrings,S)>=0 then
        Result:=True
      else if IndexOfString(FBoolFalseStrings,S)>=0 then
        Result:=False
      end
    else
      Result := CharToBool(s[1]);
end;

procedure TM_CustomIniFile.WriteBool(const Section, Ident: string; Value: Boolean);

Var
  S : String;

begin
  if (ifoWriteStringBoolean in options) then
    begin
    if Value then
      begin
      if Length(BoolTrueStrings)>0 then
        S:=BoolTrueStrings[0]
      else
        S:='true';
      end
    else
      begin
      if Length(BoolFalseStrings)>0 then
        S:=BoolFalseStrings[0]
      else
        S:='false';
      end;
    end
  else
    S:=BoolToChar(Value);
  WriteString(Section, Ident, S);
end;

function TM_CustomIniFile.ReadDate(const Section, Ident: string; Default: TDateTime): TDateTime;

begin
  if FormatSettingsActive then begin
    if not TryStrToDate(ReadString(Section, Ident, ''), Result, FormatSettings) then
      Result := Default;
  end else
    Result := StrToDateDef(ReadString(Section, Ident, ''),Default);
end;

function TM_CustomIniFile.ReadDateTime(const Section, Ident: string; Default: TDateTime): TDateTime;

begin
  if FormatSettingsActive then begin
    if not TryStrToDateTime(ReadString(Section, Ident, ''), Result, FormatSettings) then
      Result := Default;
  end else
    Result := StrToDateTimeDef(ReadString(Section, Ident, ''),Default);
end;

function TM_CustomIniFile.ReadFloat(const Section, Ident: string; Default: Double): Double;

begin
  if FormatSettingsActive then
    Result:=StrToFloatDef(ReadString(Section, Ident, ''),Default, FormatSettings)
  else
    Result:=StrToFloatDef(ReadString(Section, Ident, ''),Default);
end;

function TM_CustomIniFile.ReadTime(const Section, Ident: string; Default: TDateTime): TDateTime;

begin
  if FormatSettingsActive then
    Result := StrToTimeDef(ReadString(Section, Ident, ''),Default, FormatSettings.TimeSeparator)
  else
    Result := StrToTimeDef(ReadString(Section, Ident, ''),Default);
end;

procedure TM_CustomIniFile.WriteDate(const Section, Ident: string; Value: TDateTime);
begin
  if FormatSettingsActive then
    WriteString(Section, Ident, DateToStr(Value, FormatSettings))
  else
    WriteString(Section, Ident, DateToStr(Value));
end;

procedure TM_CustomIniFile.WriteDateTime(const Section, Ident: string; Value: TDateTime);
begin
  if FormatSettingsActive then
    WriteString(Section, Ident, DateTimeToStr(Value, FormatSettings))
  else
    WriteString(Section, Ident, DateTimeToStr(Value));
end;

procedure TM_CustomIniFile.WriteFloat(const Section, Ident: string; Value: Double);
begin
  if FormatSettingsActive then
    WriteString(Section, Ident, FloatToStr(Value, FormatSettings))
  else
    WriteString(Section, Ident, FloatToStr(Value));
end;

procedure TM_CustomIniFile.WriteTime(const Section, Ident: string; Value: TDateTime);
begin
  if FormatSettingsActive then
    WriteString(Section, Ident, TimeToStr(Value, FormatSettings))
  else
    WriteString(Section, Ident, TimeToStr(Value));
end;

function TM_CustomIniFile.ValueExists(const Section, Ident: string): Boolean;
var
  oSection: TM_IniFileSection;
begin
  Result := False;
  oSection := FSectionList.SectionByName(Section,CaseSensitive);
  if oSection <> nil then
    Result := (oSection.KeyList.KeyByName(Ident,CaseSensitive) <> nil);
end;

function TM_CustomIniFile.ReadBinaryStream(const Section, Name: string; Value: TM_Stream): Integer;

Var
  M : TM_MemoryStream;
  S : String;
  PB,PR : PByte;
  PC : PChar;
  H : String[3];
  i,l2,code : Integer;


begin
  S:=ReadString(Section,Name,'');
  Setlength(H,3);
  H[1]:='$';
  Result:=Length(S) div 2;
  If Result>0 then
    begin
    GetMem(PR,Result);
    Try
      PC:=PChar(S);
      PB:=PR;
      For I:=1 to Result do
        begin
        H[2]:=PC[0];
        H[3]:=PC[1];
        Val(H,PB^,code);
        Inc(PC,2);
        Inc(PB);
        end;
      Value.WriteBuffer(PR^,Result);
    finally
      FreeMem(PR);
    end;
    end;
end;

procedure TM_CustomIniFile.WriteBinaryStream(const Section, Name: string;
  Value: TM_Stream);


Var
  M : TM_MemoryStream;
  S : String;
  PB : PByte;
  PC : PChar;
  H : String[2];
  i : Integer;

begin
  M:=TM_MemoryStream.Create;
  Try
    M.CopyFrom(Value,0);
    SetLength(S,M.Size*2);
    If (length(S)>0) then
      begin
      PB:=M.Memory;
      PC:=PChar(S);
      For I:=1 to Length(S) div 2 do
        begin
        H:=HexStr(PB^,2);
        PC[0]:=H[1];
        PC[1]:=H[2];
        Inc(PC,2);
        Inc(PB);
        end;
      end;
    WriteString(Section,Name,S);
  Finally
    M.Free;
  end;
end;

procedure TM_CustomIniFile.ReadSectionValues(const Section: string; Strings: TJPStrList; Options: TSectionValuesOptions);

type
  TOldSectionValues = Procedure (const Section: string; Strings: TJPStrList) of object;

var
  CurrSV,
  TCustomSV: TOldSectionValues;
  CurrClass   : TClass;

begin
  if (Options<>[]) then
    Raise Exception.Create('Options not supported, options must be empty');
  // Redirect calls to old implementation, if it is overridden.
  CurrSV:=nil;
  CurrClass:=Classtype;
  while (CurrClass<>nil) and (CurrClass<>TM_CustomIniFile) do
   CurrClass:=CurrClass.Classparent;
  if CurrClass<>nil then
    begin
    CurrSV:=@Self.ReadSectionValues;
    TCustomSV:=@TM_CustomIniFile(@CurrClass).ReadSectionValues;
    if TMethod(TCustomSV).Code=TMethod(CurrSV).Code then
      CurrSV:=nil;
   end;
  if Assigned(CurrSV) then
    ReadSectionValues(Section,Strings)
  else
    Raise Exception.Create('ReadSectionValues not overridden');
end;

procedure TM_CustomIniFile.ReadSectionValues(const Section: string;
  Strings: TJPStrList);
begin
  ReadSectionValues(Section,Strings,[svoIncludeInvalid]);
end;

{ TM_IniFile }


constructor TM_IniFile.Create(const AFileName: string; AOptions : TIniFileOptions = []);
var
  slLines: TJPStrList;
begin
  FBOM := '';
  If Not (self is TM_MemIniFile) then
    StripQuotes:=True;
  inherited Create(AFileName,AOptions);
  FStream := nil;
  slLines := TJPStrList.Create;
  try
    if FileExists(FFileName) then
      begin
      // read the ini file values
      slLines.LoadFromFile(FFileName);
      FillSectionList(slLines);
      end
  finally
    slLines.Free;
  end;
end;

//constructor TM_IniFile.Create(AStream: TM_Stream; AEscapeLineFeeds : Boolean);
//
//begin
//  if AEscapeLineFeeds then
//    Create(AStream,[ifoEscapeLineFeeds])
//  else
//    Create(AStream,[]);
//end;

//constructor TM_IniFile.Create(AStream: TM_Stream; AOptions : TIniFileOptions = []);
//
//var
//  slLines: TJPStrList;
//
//begin
//  FBOM := '';
//  inherited Create('',AOptions);
//  FStream := AStream;
//  slLines := TJPStrList.Create;
//  try
//    // read the ini file values
//    slLines.LoadFromStream(FStream);
//    FillSectionList(slLines);
//  finally
//    slLines.Free;
//  end;
//end;

destructor TM_IniFile.destroy;
begin
  If FDirty and FCacheUpdates then
    try
      UpdateFile;
    except
      // Eat exception. Compatible to D7 behaviour, see comments to bug 19046
    end;
  inherited destroy;
end;

procedure TM_IniFile.FillSectionList(AStrings: TJPStrList);
const
  Utf8Bom    = #$EF#$BB#$BF;        { Die einzelnen BOM Typen }

var
  i,j: integer;
  sLine, sIdent, sValue: string;
  oSection: TM_IniFileSection;

  procedure RemoveBackslashes;
  var
    i,l: integer;
    s: string;
    bAppendNextLine, bAppended: boolean;
  begin
    AStrings.BeginUpdate;
    try
      For I:=AStrings.Count-2 downto 0 do
        begin
        S:=AStrings[i];
        L:=Length(S);
        If (I<AStrings.Count-1) and (L>0) and (S[L]=LF_Escape) then
          begin
          S:=Copy(S,1,L-1)+AStrings[I+1];
          AStrings.Delete(I+1);
          AStrings[i]:=S;
          end;
        end;
    finally
      AStrings.EndUpdate;
    end;
  end;

Var
  addKey : Boolean;

begin
  oSection := nil;
  FSectionList.Clear;
  if EscapeLineFeeds then
    RemoveBackslashes;
  if (AStrings.Count > 0) and (copy(AStrings.Items[0],1,Length(Utf8Bom)) = Utf8Bom) then
  begin
    FBOM := Utf8Bom;
    AStrings.Items[0] := copy(AStrings.Items[0],Length(Utf8Bom)+1,Length(AStrings.Items[0]));
  end;
  for i := 0 to AStrings.Count-1 do begin
    sLine := Trim(AStrings[i]);
    if sLine > '' then
      begin
      if IsComment(sLine) and (oSection = nil) then
        begin
        // comment at the beginning of the ini file
        if Not (ifoStripComments in Options) then
          begin
          oSection := TM_IniFileSection.Create(sLine);
          FSectionList.Add(oSection);
          end;
        continue;
        end;
      if (Copy(sLine, 1, 1) = Brackets[0]) and (Copy(sLine, length(sLine), 1) = Brackets[1]) then
        begin
        // regular section
        oSection := TM_IniFileSection.Create(Copy(sLine, 2, Length(sLine) - 2));
        FSectionList.Add(oSection);
        end
      else if oSection <> nil then
        begin
        if IsComment(sLine) then
          begin
          AddKey:=Not (ifoStripComments in Options);
          // comment within a section
          sIdent := sLine;
          sValue := '';
          end
        else
          begin
          // regular key
          j:=Pos(Separator, sLine);
          if j=0 then
           begin
           AddKey:=Not (ifoStripInvalid in Options);
           sIdent:='';
           sValue:=sLine
           end
          else
           begin
           AddKey:=True;
           sIdent:=Trim(Copy(sLine, 1,  j - 1));
           sValue:=Trim(Copy(sLine, j + 1, Length(sLine) - j));
           end;
        end;
        if AddKey then
          oSection.KeyList.Add(TM_IniFileKey.Create(sIdent, sValue));
        end;
      end;
  end;
end;

function TM_IniFile.ReadString(const Section, Ident, Default: string): string;
var
  oSection: TM_IniFileSection;
  oKey: TM_IniFileKey;
  J: integer;
begin
  Result := Default;
  oSection := FSectionList.SectionByName(Section,CaseSensitive);
  if oSection <> nil then begin
    oKey := oSection.KeyList.KeyByName(Ident,CaseSensitive);
    if oKey <> nil then
      If StripQuotes then
      begin
        J:=Length(oKey.Value);
        // Joost, 2-jan-2007: The check (J>1) is there for the case that
        // the value consist of a single double-quote character. (see
        // mantis bug 6555)
        If (J>1) and ((oKey.Value[1] in ['"','''']) and (oKey.Value[J]=oKey.Value[1])) then
           Result:=Copy(oKey.Value,2,J-2)
        else
           Result:=oKey.Value;
      end
      else Result:=oKey.Value;
    end;
  end;

procedure TM_IniFile.SetCacheUpdates(const AValue: Boolean);
begin
  if FCacheUpdates and not AValue and FDirty then
    UpdateFile;
  FCacheUpdates := AValue;
end;

procedure TM_IniFile.WriteString(const Section, Ident, Value: String);
var
  oSection: TM_IniFileSection;
  oKey: TM_IniFileKey;
begin
  if (Section > '') and (Ident > '') then
    begin
    // update or add key
    oSection := FSectionList.SectionByName(Section,CaseSensitive);
    if (oSection = nil) then
      begin
      oSection := TM_IniFileSection.Create(Section);
      FSectionList.Add(oSection);
      end;
    with oSection.KeyList do
      begin
      oKey := KeyByName(Ident,CaseSensitive);
      if oKey <> nil then
        oKey.Value := Value
      else
        oSection.KeyList.Add(TM_IniFileKey.Create(Ident, Value));
      end;
    end;
  MaybeUpdateFile;
end;

procedure TM_IniFile.ReadSection(const Section: string; Strings: TJPStrList);
var
  oSection: TM_IniFileSection;
  i: integer;
begin
  Strings.BeginUpdate;
  try
    Strings.ClearAll;
    oSection := FSectionList.SectionByName(Section,CaseSensitive);
    if oSection <> nil then with oSection.KeyList do
      for i := 0 to Count-1 do
        if not IsComment(Items[i].Ident) then
          Strings.Add(Items[i].Ident);
  finally
    Strings.EndUpdate;
  end;
end;

procedure TM_IniFile.ReadSectionRaw(const Section: string; Strings: TJPStrList);
var
  oSection: TM_IniFileSection;
  i: integer;
begin
  Strings.BeginUpdate;
  try
    Strings.ClearAll;
    oSection := FSectionList.SectionByName(Section,CaseSensitive);
    if oSection <> nil then with oSection.KeyList do
      for i := 0 to Count-1 do
        if not IsComment(Items[i].Ident) then
         begin
           if Items[i].Ident<>'' then
            Strings.Add(Items[i].Ident + Separator +Items[i].Value)
           else
            Strings.Add(Items[i].Value);
         end;
  finally
    Strings.EndUpdate;
  end;
end;

procedure TM_IniFile.ReadSections(Strings: TJPStrList);
var
  i: integer;
begin
  Strings.BeginUpdate;
  try
    Strings.ClearAll;
    for i := 0 to FSectionList.Count-1 do
      if not IsComment(FSectionList[i].Name) then
        Strings.Add(FSectionList[i].Name);
  finally
    Strings.EndUpdate;
  end;
end;

procedure TM_IniFile.ReadSectionValues(const Section: string; Strings: TJPStrList; AOptions : TSectionValuesOptions = [svoIncludeInvalid]);
var
  oSection: TM_IniFileSection;
  s: string;
  i,J: integer;
  KeyIsComment,IncludeComments,IncludeInvalid,DoStripQuotes : boolean;
  K : TM_IniFileKey;

begin
  IncludeComments:=(svoIncludeComments in AOptions) Or (ifoStripComments in Options);
  IncludeInvalid:=(svoIncludeInvalid in AOptions) Or (ifoStripInvalid in Options);
  DoStripQuotes:=StripQuotes and Not (svoIncludeQuotes in AOptions);
  Strings.BeginUpdate;
  try
    Strings.ClearAll;
    oSection := FSectionList.SectionByName(Section,CaseSensitive);
    if oSection = nil then
      Exit;
    for i := 0 to oSection.KeyList.Count-1 do
      begin
      K:=oSection.KeyList.Items[i];
      if IncludeInvalid or (K.Ident<>'') then
        begin
        s := K.Value;
        KeyIsComment:=IsComment(K.Ident);
        if IncludeComments Or Not KeyIsComment then
          begin
          If DoStripQuotes then
            begin
            J:=Length(s);
            // Joost, 2-jan-2007: The check (J>1) is there for the case that
            // the value consist of a single double-quote character. (see
            // mantis bug 6555)
            If (J>1) and ((s[1] in ['"','''']) and (s[J]=s[1])) then
               s:=Copy(s,2,J-2);
            end;
          if KeyIsComment then
            S:=K.Ident
          else if k.ident<>'' then
            s:=K.Ident+Separator+s;
          Strings.Add(s);
          end;
        end;
      end;
  finally
    Strings.EndUpdate;
  end;
end;

procedure TM_IniFile.DeleteSection(ASection : TM_IniFileSection);

begin
  FSectionList.Delete(FSectionList.IndexOf(ASection));
  ASection.Free;
end;

Procedure TM_IniFile.MaybeDeleteSection(ASection : TM_IniFileSection);

begin
  If Asection.Empty then
    DeleteSection(ASection);
end;

procedure TM_IniFile.EraseSection(const Section: string);
var
  oSection: TM_IniFileSection;
begin
  oSection := FSectionList.SectionByName(Section,CaseSensitive);
  if oSection <> nil then begin
    { It is needed so UpdateFile doesn't find a defunct section }
    { and cause the program to crash }
    DeleteSection(OSection);
    MaybeUpdateFile;
  end;
end;

procedure TM_IniFile.DeleteKey(const Section, Ident: String);
var
 oSection: TM_IniFileSection;
 oKey: TM_IniFileKey;
begin
  oSection := FSectionList.SectionByName(Section,CaseSensitive);
  if oSection <> nil then
    begin
    oKey := oSection.KeyList.KeyByName(Ident,CaseSensitive);
    if oKey <> nil then
      begin
      oSection.KeyList.Delete(oSection.KeyList.IndexOf(oKey));
      oKey.Free;
      MaybeUpdateFile;
      end;
    end;
end;

procedure TM_IniFile.UpdateFile;
var
  slLines: TJPStrList;
  i, j: integer;
  D: String;

begin
  slLines := TJPStrList.Create;
  try
    for i := 0 to FSectionList.Count - 1 do
      with FSectionList[i] do
      begin
        if IsComment(Name) then
          // comment
            slLines.Add(Name)
        else
          // regular section
            slLines.Add(Brackets[0] + Name + Brackets[1]);
        for j := 0 to KeyList.Count - 1 do
          if IsComment(KeyList[j].Ident) then
            // comment
              slLines.Add(KeyList[j].Ident)
          else
            // regular key
              slLines.Add(KeyList[j].Ident + Separator + KeyList[j].Value);
        if (i < FSectionList.Count - 1) and not IsComment(Name) then slLines.Add('');
      end;

    if slLines.Count > 0 then slLines.Items[0] := FBOM + slLines.Items[0];
    if FFileName > '' then
    begin
      D := ExtractFilePath(FFileName);
      If D <> '' Then
        if not ForceDirectories(D) then Raise EInoutError.CreateFmt(SErrCouldNotCreatePath, [D]);
      slLines.SaveToFile(FFileName);
    end;
    //else if FStream <> nil then
    //begin
    //  FStream.Size := 0;
    //  slLines.SaveToStream(FStream);
    //end;

    FillSectionList(slLines);
    FDirty := false;
  finally
    slLines.Free;
  end;
end;

procedure TM_IniFile.MaybeUpdateFile;
begin
  If FCacheUpdates then
    FDirty:=True
  else
    UpdateFile;
end;

{ TM_MemIniFile }

constructor TM_MemIniFile.Create(const AFileName: string; AEscapeLineFeeds : Boolean = False);

begin
  Inherited;
  FCacheUpdates:=True;
end;

procedure TM_MemIniFile.Clear;
begin
  FSectionList.Clear;
end;

procedure TM_MemIniFile.GetStrings(List: TJPStrList);
var
  i, j: integer;
  oSection: TM_IniFileSection;
begin
  List.BeginUpdate;
  try
    for i := 0 to FSectionList.Count-1 do begin
      oSection := FSectionList[i];
      with oSection do begin
        if IsComment(Name) then
          List.Add(Name)
        else
          List.Add(Brackets[0] + Name + Brackets[1]);
        for j := 0 to KeyList.Count-1 do begin
          if IsComment(KeyList[j].Ident) then
            List.Add(KeyList[j].Ident)
          else
            List.Add(KeyList[j].Ident + Separator + KeyList[j].Value);
        end;
      end;
      if i < FSectionList.Count-1 then
        List.Add('');
    end;
  finally
    List.EndUpdate;
  end;
end;

procedure TM_MemIniFile.Rename(const AFileName: string; Reload: Boolean);
var
  slLines: TJPStrList;
begin
  FFileName := AFileName;
  FStream := nil;
  if Reload then begin
    slLines := TJPStrList.Create;
    try
      slLines.LoadFromFile(FFileName);
      FillSectionList(slLines);
    finally
      slLines.Free;
    end;
  end;
end;

procedure TM_MemIniFile.SetStrings(List: TJPStrList);
begin
  FillSectionList(List);
end;


end.
