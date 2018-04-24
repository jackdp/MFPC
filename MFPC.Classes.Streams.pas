{
  This file is part of the Free Component Library (FCL)
  Copyright (c) 1999-2000 by the Free Pascal development team

  See the file COPYING.FPC, included in this distribution,
  for details about the copyright.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{
  jpmod: Excerpt from the "Classes" unit.
  2018.02.11
  =================================================
  FPC 3.1.1 rev. 37820 (trunk)
  =================================================
  Original files:
    fpcsrc\rtl\objpas\classes\classesh.inc
    fpcsrc\rtl\objpas\classes\streams.inc
}

unit MFPC.Classes.Streams;

{$mode objfpc}{$H+}
{$WARN 5024 off : Parameter "$1" not used}
{$WARN 5057 off : Local variable "$1" does not seem to be initialized}
{$WARN 4055 off : Conversion between ordinals and pointers is not portable}
interface

uses
  SysUtils,
  MFPC.Classes.SHARED,
  //sysconst,
  RtlConsts;

  //Classes


type

  TM_Stream = class(TObject)
  private
  protected
    procedure InvalidSeek; virtual;
    procedure Discard(const Count: Int64);
    procedure DiscardLarge(Count: Int64; const MaxBufferSize: Longint);
    procedure FakeSeekForward(Offset: Int64; const Origin: TSeekOrigin; const Pos: Int64);
    function GetPosition: Int64; virtual;
    procedure SetPosition(const Pos: Int64); virtual;
    function GetSize: Int64; virtual;
    procedure SetSize64(const NewSize: Int64); virtual;
    procedure SetSize(NewSize: Longint); virtual; overload;
    procedure SetSize(const NewSize: Int64); virtual; overload;
    procedure ReadNotImplemented;
    procedure WriteNotImplemented;
  public
    function Read(var Buffer; Count: Longint): Longint; virtual;
    function Write(const Buffer; Count: Longint): Longint; virtual;
    function Seek(Offset: Longint; Origin: Word): Longint; virtual; overload;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; virtual; overload;
    procedure ReadBuffer(var Buffer; Count: Longint);
    procedure WriteBuffer(const Buffer; Count: Longint);
    function CopyFrom(Source: TM_Stream; Count: Int64): Int64;
    //function ReadComponent(Instance: TComponent): TComponent;
    //function ReadComponentRes(Instance: TComponent): TComponent;
    //procedure WriteComponent(Instance: TComponent);
    //procedure WriteComponentRes(const ResName: string; Instance: TComponent);
    //procedure WriteDescendent(Instance, Ancestor: TComponent);
    //procedure WriteDescendentRes(const ResName: string; Instance, Ancestor: TComponent);
    //procedure WriteResourceHeader(const ResName: string; {!!!:out} var FixupInfo: Longint);
    //procedure FixupResourceHeader(FixupInfo: Longint);
    //procedure ReadResHeader;
    function ReadByte: Byte;
    function ReadWord: Word;
    function ReadDWord: Cardinal;
    function ReadQWord: QWord;
    function ReadAnsiString: String;
    procedure WriteByte(b: Byte);
    procedure WriteWord(w: Word);
    procedure WriteDWord(d: Cardinal);
    procedure WriteQWord(q: QWord);
    Procedure WriteAnsiString(const S: String);
    property Position: Int64 read GetPosition write SetPosition;
    property Size: Int64 read GetSize write SetSize64;
  end;


  TM_HandleStream = class(TM_Stream)
  private
    FHandle: THandle;
  protected
    procedure SetSize(NewSize: Longint); override;
    procedure SetSize(const NewSize: Int64); override;
  public
    constructor Create(AHandle: THandle);
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    property Handle: THandle read FHandle;
  end;

  TM_FileStream = class(TM_HandleStream)
  Private
    FFileName : String;
  public
    constructor Create(const AFileName: string; Mode: Word);
    constructor Create(const AFileName: string; Mode: Word; Rights: Cardinal);
    destructor Destroy; override;
    property FileName : String Read FFilename;
  end;

  TM_CustomMemoryStream = class(TM_Stream)
  private
    FMemory: Pointer;
    FSize, FPosition: PtrInt;
  protected
    Function GetSize : Int64; Override;
    function GetPosition: Int64; Override;
    procedure SetPointer(Ptr: Pointer; ASize: PtrInt);
  public
    function Read(var Buffer; Count: LongInt): LongInt; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    procedure SaveToStream(Stream: TM_Stream);
    procedure SaveToFile(const FileName: string);
    property Memory: Pointer read FMemory;
  end;

  TM_MemoryStream = class(TM_CustomMemoryStream)
  private
    FCapacity: PtrInt;
    procedure SetCapacity(NewCapacity: PtrInt);
  protected
    function Realloc(var NewCapacity: PtrInt): Pointer; virtual;
    property Capacity: PtrInt read FCapacity write SetCapacity;
  public
    destructor Destroy; override;
    procedure Clear;
    procedure LoadFromStream(Stream: TM_Stream);
    procedure LoadFromFile(const FileName: string);
    procedure SetSize({$ifdef CPU64}const NewSize: Int64{$else}NewSize: LongInt{$endif}); override;
    function Write(const Buffer; Count: LongInt): LongInt; override;
  end;

  TM_BytesStream = class(TM_MemoryStream)
  private
    FBytes: TBytes;
  protected
    function Realloc(var NewCapacity: PtrInt): Pointer; override;
  public
    constructor Create(const ABytes: TBytes); virtual; overload;
    property Bytes: TBytes read FBytes;
  end;

  //{$IFDEF MFPC_USE_StringStream}
  TM_StringStream = class(TM_BytesStream)
  private
    FEncoding: TEncoding;
    FOwnsEncoding : Boolean;
    function GetDataString: string;
    function GetUnicodeDataString: String;
  protected
  public
    constructor Create(const ABytes: TBytes); override; overload;
    constructor Create(const AString: string = ''); overload;
    constructor CreateRaw(const AString: RawByteString); overload;
    constructor Create(const AString: string; AEncoding: TEncoding; AOwnsEncoding: Boolean = True); overload;
    constructor Create(const AString: string; ACodePage: Integer); overload;
    // UnicodeString versions
    constructor Create(const AString: UnicodeString); overload;
    constructor Create(const AString: UnicodeString; AEncoding: TEncoding; AOwnsEncoding: Boolean = True); overload;
    constructor Create(const AString: UnicodeString; ACodePage: Integer); overload;
    function ReadUnicodeString(Count: Longint): UnicodeString;
    procedure WriteUnicodeString(const AString: UnicodeString);
    function ReadAnsiString(Count: Longint): AnsiString;
    procedure WriteAnsiString(const AString: AnsiString);
    function ReadString(Count: Longint): string;
    procedure WriteString(const AString: string);
    property DataString: string read GetDataString;
    Property UnicodeDataString : String Read GetUnicodeDataString;
    Property OwnsEncoding : Boolean Read FOwnsEncoding;
    Property Encoding : TEncoding Read FEncoding;
  end;
  //{$ENDIF}


  {$ifdef FPC_OS_UNICODE}
  TM_ResourceStream = class(TM_CustomMemoryStream)
  private
    Res: TFPResourceHandle;
    Handle: TFPResourceHGLOBAL;
    procedure Initialize(Instance: TFPResourceHMODULE; Name, ResType: PWideChar; NameIsID: Boolean);
  public
    constructor Create(Instance: TFPResourceHMODULE; const ResName: WideString; ResType: PWideChar);
    constructor CreateFromID(Instance: TFPResourceHMODULE; ResID: Integer; ResType: PWideChar);
    destructor Destroy; override;
  end;
  {$else}
  TM_ResourceStream = class(TM_CustomMemoryStream)
  private
    Res: TFPResourceHandle;
    Handle: TFPResourceHGLOBAL;
    procedure Initialize(Instance: TFPResourceHMODULE; Name, ResType: PChar; NameIsID: Boolean);
  public
    constructor Create(Instance: TFPResourceHMODULE; const ResName: string; ResType: PChar);
    constructor CreateFromID(Instance: TFPResourceHMODULE; ResID: Integer; ResType: PChar);
    destructor Destroy; override;
  end;
  {$endif FPC_OS_UNICODE}


implementation

{$region '                           TM_Stream                               '}
procedure TM_Stream.ReadNotImplemented;
begin
  raise EStreamError.CreateFmt(SStreamNoReading, [ClassName])at get_caller_addr(get_frame), get_caller_frame(get_frame);
  //raise EStreamError.Create('TStream - ReadNotImplemented');
end;

procedure TM_Stream.WriteNotImplemented;
begin
  raise EStreamError.CreateFmt(SStreamNoWriting, [ClassName])at get_caller_addr(get_frame), get_caller_frame(get_frame);
  //raise EStreamError.Create('TStream - WriteNotImplemented');
end;

function TM_Stream.Read(var Buffer; Count: Longint): Longint;
begin
  ReadNotImplemented;
  Result := 0;
end;

function TM_Stream.Write(const Buffer; Count: Longint): Longint;
begin
  WriteNotImplemented;
  Result := 0;
end;

function TM_Stream.GetPosition: Int64;
begin
  Result := Seek(0, soCurrent);
end;

procedure TM_Stream.SetPosition(const Pos: Int64);
begin
  Seek(Pos, soBeginning);
end;

procedure TM_Stream.SetSize64(const NewSize: Int64);
begin
  // Required because can't use overloaded functions in properties
  SetSize(NewSize);
end;

function TM_Stream.GetSize: Int64;
var
  p: Int64;
begin
  p := Seek(0, soCurrent);
  GetSize := Seek(0, soEnd);
  Seek(p, soBeginning);
end;

procedure TM_Stream.SetSize(NewSize: Longint);
begin
  // We do nothing. Pipe streams don't support this
  // As wel as possible read-ony streams !!
end;

procedure TM_Stream.SetSize(const NewSize: Int64);
begin
  // Backwards compatibility that calls the longint SetSize
  if (NewSize < Low(Longint)) or (NewSize > High(Longint)) then raise ERangeError.Create(SRangeError);
  SetSize(Longint(NewSize));
end;

function TM_Stream.Seek(Offset: Longint; Origin: Word): Longint;
type
  TSeek64 = function(const Offset: Int64; Origin: TSeekOrigin): Int64 of object;
var
  CurrSeek, TStreamSeek: TSeek64;
  CurrClass: TClass;
begin
  // Redirect calls to 64bit Seek, but we can't call the 64bit Seek
  // from TM_Stream, because then we end up in an infinite loop
  CurrSeek := nil;
  CurrClass := Classtype;
  while (CurrClass <> nil) and (CurrClass <> TM_Stream) do CurrClass := CurrClass.Classparent;
  if CurrClass <> nil then
  begin
    CurrSeek := @Self.Seek;
    TStreamSeek := @TM_Stream(@CurrClass).Seek;
    if TMethod(TStreamSeek).Code = TMethod(CurrSeek).Code then CurrSeek := nil;
  end;
  if CurrSeek <> nil then Result := Seek(Int64(Offset), TSeekOrigin(Origin))
  else raise EStreamError.CreateFmt(SSeekNotImplemented, [ClassName]);
end;

procedure TM_Stream.Discard(const Count: Int64);
const
  CSmallSize = 255;
  CLargeMaxBuffer = 32 * 1024; // 32 KiB
var
  Buffer: array [1 .. CSmallSize] of Byte;
begin
  if Count = 0 then Exit;
  if Count <= SizeOf(Buffer) then ReadBuffer(Buffer, Count)
  else DiscardLarge(Count, CLargeMaxBuffer);
end;

procedure TM_Stream.DiscardLarge(Count: Int64; const MaxBufferSize: Longint);
var
  Buffer: array of Byte;
begin
  if Count = 0 then Exit;
  if Count > MaxBufferSize then SetLength(Buffer, MaxBufferSize)
  else SetLength(Buffer, Count);
  while (Count >= Length(Buffer)) do
  begin
    ReadBuffer(Buffer[0], Length(Buffer));
    Dec(Count, Length(Buffer));
  end;
  if Count > 0 then ReadBuffer(Buffer[0], Count);
end;

procedure TM_Stream.InvalidSeek;
begin
  raise EStreamError.CreateFmt(SStreamInvalidSeek, [ClassName])at get_caller_addr(get_frame), get_caller_frame(get_frame);
end;

procedure TM_Stream.FakeSeekForward(Offset: Int64; const Origin: TSeekOrigin; const Pos: Int64);
begin
  if Origin = soBeginning then Dec(Offset, Pos);
  if (Offset < 0) or (Origin = soEnd) then InvalidSeek;
  if Offset > 0 then Discard(Offset);
end;

function TM_Stream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  // Backwards compatibility that calls the longint Seek
  if (Offset < Low(Longint)) or (Offset > High(Longint)) then raise ERangeError.Create(SRangeError);
  Result := Seek(Longint(Offset), ord(Origin));
end;

procedure TM_Stream.ReadBuffer(var Buffer; Count: Longint);
Var
  r, t: Longint;
begin
  t := 0;
  repeat
    r := Read(PByte(@Buffer)[t], Count - t);
    inc(t, r);
  until (t = Count) or (r <= 0);
  if (t < Count) then Raise EReadError.Create(SReadError);
end;

procedure TM_Stream.WriteBuffer(const Buffer; Count: Longint);
var
  r, t: Longint;
begin
  t := 0;
  Repeat
    r := Write(PByte(@Buffer)[t], Count - t);
    inc(t, r);
  Until (t = Count) or (r <= 0);
  if (t < Count) then Raise EWriteError.Create(SWriteError);
end;

function TM_Stream.CopyFrom(Source: TM_Stream; Count: Int64): Int64;
var
  Buffer: Pointer;
  BufferSize, i: Longint;
const
  MaxSize = $20000;
begin

  Result := 0;
  if Count = 0 then Source.Position := 0; // This WILL fail for non-seekable streams...
  BufferSize := MaxSize;
  if (Count > 0) and (Count < BufferSize) then BufferSize := Count; // do not allocate more than needed

  GetMem(Buffer, BufferSize);
  try
    if Count = 0 then
      repeat
        i := Source.Read(Buffer^, BufferSize);
        if i > 0 then WriteBuffer(Buffer^, i);
        inc(Result, i);
      until i < BufferSize
    else
      while Count > 0 do
      begin
        if Count > BufferSize then i := BufferSize
        else i := Count;
        Source.ReadBuffer(Buffer^, i);
        WriteBuffer(Buffer^, i);
        Dec(Count, i);
        inc(Result, i);
      end;
  finally
    FreeMem(Buffer);
  end;

end;

function TM_Stream.ReadByte: Byte;
var
  b: Byte;
begin
  ReadBuffer(b, 1);
  ReadByte := b;
end;

function TM_Stream.ReadWord: Word;
var
  w: Word;
begin
  ReadBuffer(w, 2);
  ReadWord := w;
end;

function TM_Stream.ReadDWord: Cardinal;
var
  d: Cardinal;
begin
  ReadBuffer(d, 4);
  ReadDWord := d;
end;

function TM_Stream.ReadQWord: QWord;
var
  q: QWord;
begin
  ReadBuffer(q, 8);
  ReadQWord := q;
end;

Function TM_Stream.ReadAnsiString: String;
Var
  TheSize: Longint;
  p: PByte;
begin
  ReadBuffer(TheSize, SizeOf(TheSize));
  SetLength(Result, TheSize);
    // Illegal typecast if no AnsiStrings defined.
  if TheSize > 0 then
  begin
    ReadBuffer(Pointer(Result)^, TheSize);
    p := Pointer(Result) + TheSize;
    p^ := 0;
  end;
end;

Procedure TM_Stream.WriteAnsiString(const S: String);
Var
  L: Longint;
begin
  L := Length(S);
  WriteBuffer(L, SizeOf(L));
  WriteBuffer(Pointer(S)^, L);
end;

procedure TM_Stream.WriteByte(b: Byte);
begin
  WriteBuffer(b, 1);
end;

procedure TM_Stream.WriteWord(w: Word);
begin
  WriteBuffer(w, 2);
end;

procedure TM_Stream.WriteDWord(d: Cardinal);
begin
  WriteBuffer(d, 4);
end;

procedure TM_Stream.WriteQWord(q: QWord);
begin
  WriteBuffer(q, 8);
end;
{$endregion TM_Stream}


{$region '                          TM_HandleStream                            '}
Constructor TM_HandleStream.Create(AHandle: THandle);
begin
  Inherited Create;
  FHandle := AHandle;
end;

function TM_HandleStream.Read(var Buffer; Count: Longint): Longint;
begin
  Result := FileRead(FHandle, Buffer, Count);
  If Result = -1 then Result := 0;
end;

function TM_HandleStream.Write(const Buffer; Count: Longint): Longint;
begin
  Result := FileWrite(FHandle, Buffer, Count);
  If Result = -1 then Result := 0;
end;

Procedure TM_HandleStream.SetSize(NewSize: Longint);
begin
  SetSize(Int64(NewSize));
end;

Procedure TM_HandleStream.SetSize(const NewSize: Int64);
begin
  FileTruncate(FHandle, NewSize);
end;

function TM_HandleStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  Result := FileSeek(FHandle, Offset, ord(Origin));
end;
{$endregion TM_HandleStream}


{$region '                          TM_FileStream                       '}
constructor TM_FileStream.Create(const AFileName: string; Mode: Word);
begin
  Create(AFileName, Mode, 438);
end;

constructor TM_FileStream.Create(const AFileName: string; Mode: Word; Rights: Cardinal);
begin
  FFileName := AFileName;
  If (Mode and fmCreate) > 0 then FHandle := FileCreate(AFileName, Mode, Rights)
  else FHandle := FileOpen(AFileName, Mode);

  If (THandle(FHandle) = feInvalidHandle) then
    If Mode = fmCreate then raise EFCreateError.createfmt(SFCreateError, [AFileName])
    else raise EFOpenError.createfmt(SFOpenError, [AFileName]);
end;

destructor TM_FileStream.Destroy;
begin
  FileClose(FHandle);
end;
{$endregion TM_FileStream}


{$region '                       TM_CustomMemoryStream                       '}
procedure TM_CustomMemoryStream.SetPointer(Ptr: Pointer; ASize: PtrInt);
begin
  FMemory := Ptr;
  FSize := ASize;
end;

function TM_CustomMemoryStream.GetSize: Int64;
begin
  Result := FSize;
end;

function TM_CustomMemoryStream.GetPosition: Int64;
begin
  Result := FPosition;
end;

function TM_CustomMemoryStream.Read(var Buffer; Count: LongInt): LongInt;
begin
  Result := 0;
  If (FSize > 0) and (FPosition < FSize) and (FPosition >= 0) then
  begin
    Result := Count;
    If (Result > (FSize - FPosition)) then Result := (FSize - FPosition);
    Move((FMemory + FPosition)^, Buffer, Result);
    FPosition := FPosition + Result;
  end;
end;

function TM_CustomMemoryStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  Case Word(Origin) of
    soFromBeginning: FPosition := Offset;
    soFromEnd: FPosition := FSize + Offset;
    soFromCurrent: FPosition := FPosition + Offset;
  end;
  Result := FPosition;
{$IFDEF DEBUG}
  if Result < 0 then raise Exception.Create('TCustomMemoryStream');
{$ENDIF}
end;

procedure TM_CustomMemoryStream.SaveToStream(Stream: TM_Stream);
begin
  if FSize > 0 then Stream.WriteBuffer(FMemory^, FSize);
end;

procedure TM_CustomMemoryStream.SaveToFile(const FileName: string);
Var
  S: TM_FileStream;
begin
  S := TM_FileStream.Create(FileName, fmCreate);
  Try
    SaveToStream(S);
  finally
    S.free;
  end;
end;
{$endregion TM_CustomMemoryStream}


{$region '                         TM_MemoryStream                            '}
const
  TMSGrow = 4096; { Use 4k blocks. }

procedure TM_MemoryStream.SetCapacity(NewCapacity: PtrInt);
begin
  SetPointer(Realloc(NewCapacity), Fsize);
  FCapacity := NewCapacity;
end;

function TM_MemoryStream.Realloc(var NewCapacity: PtrInt): Pointer;
Var
  GC: PtrInt;
begin
  If NewCapacity < 0 Then NewCapacity := 0
  else
  begin
    GC := FCapacity + (FCapacity div 4);
    if (NewCapacity > FCapacity) and (NewCapacity < GC) then NewCapacity := GC; // if growing, grow at least a quarter
    NewCapacity := (NewCapacity + (TMSGrow - 1)) and not(TMSGrow - 1); // round off to block size.
  end;

  // Only now check !
  If NewCapacity = FCapacity then Result := FMemory
  else
  begin
    Result := Reallocmem(FMemory, NewCapacity);
    If (Result = Nil) and (NewCapacity > 0) then Raise EStreamError.Create(SMemoryStreamError);
  end;
end;

destructor TM_MemoryStream.Destroy;
begin
  Clear;
  Inherited Destroy;
end;

procedure TM_MemoryStream.Clear;
begin
  Fsize := 0;
  FPosition := 0;
  SetCapacity(0);
end;

procedure TM_MemoryStream.LoadFromStream(Stream: TM_Stream);
begin
  Stream.Position := 0;
  SetSize(Stream.Size);
  If Fsize > 0 then Stream.ReadBuffer(FMemory^, Fsize);
end;

procedure TM_MemoryStream.LoadFromFile(const FileName: string);
Var
  S: TM_FileStream;
begin
  S := TM_FileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  Try
    LoadFromStream(S);
  finally
    S.free;
  end;
end;

procedure TM_MemoryStream.SetSize({$IFDEF CPU64}const NewSize: Int64{$ELSE}NewSize: LongInt{$ENDIF});
begin
  SetCapacity(NewSize);
  Fsize := NewSize;
  IF FPosition > Fsize then FPosition := Fsize;
end;

function TM_MemoryStream.Write(const Buffer; Count: LongInt): LongInt;
Var
  NewPos: PtrInt;
begin
  If (Count = 0) or (FPosition < 0) then exit(0);
  NewPos := FPosition + Count;
  If NewPos > Fsize then
  begin
    IF NewPos > FCapacity then SetCapacity(NewPos);
    Fsize := NewPos;
  end;
  System.Move(Buffer, (FMemory + FPosition)^, Count);
  FPosition := NewPos;
  Result := Count;
end;
{$endregion TM_MemoryStream}


{$region '                        TM_BytesStream                         '}
constructor TM_BytesStream.Create(const ABytes: TBytes);
begin
  inherited Create;
  FBytes := ABytes;
  SetPointer(Pointer(FBytes), Length(FBytes));
  FCapacity := Length(FBytes);
end;

function TM_BytesStream.Realloc(var NewCapacity: PtrInt): Pointer;
begin
  // adapt TMemoryStream code to use with dynamic array
  if NewCapacity < 0 Then NewCapacity := 0
  else
  begin
    if (NewCapacity > Capacity) and (NewCapacity < (5 * Capacity) div 4) then NewCapacity := (5 * Capacity) div 4;
    NewCapacity := (NewCapacity + (TMSGrow - 1)) and not(TMSGrow - 1);
  end;
  if NewCapacity = Capacity then Result := Pointer(FBytes)
  else
  begin
    SetLength(FBytes, NewCapacity);
    Result := Pointer(FBytes);
    if (Result = nil) and (NewCapacity > 0) then raise EStreamError.Create(SMemoryStreamError);
  end;
end;
{$endregion TM_BytesStream}


{$region '                        TM_StringStream                        '}
//{$IFDEF MFPC_USE_StringStream}
function TM_StringStream.GetDataString: string;
begin
  Result := FEncoding.GetAnsiString(Bytes, 0, Size);
end;

function TM_StringStream.GetUnicodeDataString: String;
begin
  Result := FEncoding{%H-}.GetString(Bytes, 0, Size);
end;

constructor TM_StringStream.Create(const AString: string = '');

begin
  Create(AString, TEncoding.Default, False);
end;

constructor TM_StringStream.Create(const ABytes: TBytes);
begin
  inherited Create(ABytes);
  FEncoding := TEncoding.Default;
  FOwnsEncoding := False;
end;

constructor TM_StringStream.CreateRaw(const AString: RawByteString);
var
  CP: TSystemCodePage;
begin
  CP := StringCodePage(AString);
  if (CP = CP_ACP) or (CP = TEncoding.Default.CodePage) then
  begin
    FEncoding := TEncoding.Default;
    FOwnsEncoding := False;
  end
  else
  begin
    FEncoding := TEncoding.GetEncoding(CP);
    FOwnsEncoding := True;
  end;
  inherited Create(BytesOf(AString));
end;

constructor TM_StringStream.Create(const AString: string; AEncoding: TEncoding; AOwnsEncoding: Boolean);
begin
  FOwnsEncoding := AOwnsEncoding and not TEncoding.IsStandardEncoding(AEncoding);
  FEncoding := AEncoding;
  Inherited Create(AEncoding.GetAnsiBytes(AString));
end;

constructor TM_StringStream.Create(const AString: string; ACodePage: Integer);
begin
  Create(AString, TEncoding.GetEncoding(ACodePage), True);
end;

constructor TM_StringStream.Create(const AString: UnicodeString);
begin
  Create(AString, TEncoding.Unicode, False);
end;

constructor TM_StringStream.Create(const AString: UnicodeString; AEncoding: TEncoding; AOwnsEncoding: Boolean);
begin
  FOwnsEncoding := AOwnsEncoding and not TEncoding.IsStandardEncoding(AEncoding);
  FEncoding := AEncoding;
  Inherited Create(AEncoding.GetBytes(AString));
end;

constructor TM_StringStream.Create(const AString: UnicodeString; ACodePage: Integer);
begin
  Create(AString, TEncoding.GetEncoding(ACodePage), True);
end;

function TM_StringStream.ReadString(Count: Longint): string;
begin
  Result := ReadAnsiString(Count);
end;

function TM_StringStream.ReadUnicodeString(Count: Longint): UnicodeString;
Var
  NewLen{, SLen}: Longint;
begin
  NewLen := Size - FPosition;
  If NewLen > Count then NewLen := Count;
  Result := FEncoding.GetString(FBytes, FPosition, NewLen);
end;

procedure TM_StringStream.WriteString(const AString: string);
begin
  WriteAnsiString(AString);
end;

procedure TM_StringStream.WriteUnicodeString(const AString: UnicodeString);
Var
  B: TBytes;
begin
  B := FEncoding.GetBytes(AString);
  if Length(B) > 0 then WriteBuffer(B[0], Length(Bytes));
end;

function TM_StringStream.ReadAnsiString(Count: Longint): AnsiString;
Var
  NewLen: Longint;
begin
  NewLen := Size - FPosition;
  If NewLen > Count then NewLen := Count;
  Result := FEncoding.GetAnsiString(FBytes, FPosition, NewLen);
end;

procedure TM_StringStream.WriteAnsiString(const AString: AnsiString);
Var
  B: TBytes;
begin
  B := FEncoding.GetAnsiBytes(AString);
  if Length(B) > 0 then WriteBuffer(B[0], Length(B));
end;
//{$ENDIF}
{$endregion TM_StringStream}


{$region '                           TM_ResourceStream                                 '}

{$IFDEF FPC_OS_UNICODE}

procedure TM_ResourceStream.Initialize(Instance: TFPResourceHMODULE; Name, ResType: PWideChar; NameIsID: Boolean);
begin
  Res := FindResource(Instance, Name, ResType);
  if Res = 0 then
    if NameIsID then raise EResNotFound.CreateFmt(SResNotFound, [IntToStr(PtrInt(Name))])
    else raise EResNotFound.CreateFmt(SResNotFound, [Name]);
  Handle := LoadResource(Instance, Res);
  if Handle = 0 then
    if NameIsID then raise EResNotFound.CreateFmt(SResNotFound, [IntToStr(PtrInt(Name))])
    else raise EResNotFound.CreateFmt(SResNotFound, [Name]);
  SetPointer(LockResource(Handle), SizeOfResource(Instance, Res));
end;

constructor TM_ResourceStream.Create(Instance: TFPResourceHMODULE; const ResName: WideString; ResType: PWideChar);
begin
  inherited Create;
  Initialize(Instance, PWideChar(ResName), ResType, False);
end;

constructor TM_ResourceStream.CreateFromID(Instance: TFPResourceHMODULE; ResID: Integer; ResType: PWideChar);
begin
  inherited Create;
  Initialize(Instance, PWideChar(ResID), ResType, True);
end;
{$ELSE FPC_OS_UNICODE}

procedure TM_ResourceStream.Initialize(Instance: TFPResourceHMODULE; Name, ResType: PChar; NameIsID: Boolean);
begin
  Res := FindResource(Instance, Name, ResType);
  if Res = 0 then
    if NameIsID then raise EResNotFound.CreateFmt(SResNotFound, [IntToStr({%H-}PtrInt(Name))])
    else raise EResNotFound.CreateFmt(SResNotFound, [Name]);
  Handle := LoadResource(Instance, Res);
  if Handle = 0 then
    if NameIsID then raise EResNotFound.CreateFmt(SResNotFound, [IntToStr(PtrInt(Name))])
    else raise EResNotFound.CreateFmt(SResNotFound, [Name]);
  SetPointer(LockResource(Handle), SizeOfResource(Instance, Res));
end;

constructor TM_ResourceStream.Create(Instance: TFPResourceHMODULE; const ResName: string; ResType: PChar);
begin
  inherited Create;
  Initialize(Instance, PChar(ResName), ResType, False);
end;

constructor TM_ResourceStream.CreateFromID(Instance: TFPResourceHMODULE; ResID: Integer; ResType: PChar);
begin
  inherited Create;
  Initialize(Instance, PChar(PtrInt(ResID)), ResType, True);
end;
{$ENDIF FPC_OS_UNICODE}

destructor TM_ResourceStream.Destroy;
begin
  UnlockResource(Handle);
  FreeResource(Handle);
  inherited Destroy;
end;
{$endregion TM_ResourceStream}


end.
