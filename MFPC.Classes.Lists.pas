{
    This file is part of the Free Pascal Run Time Library (rtl)
    Copyright (c) 1999-2005 by the Free Pascal development team

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
    fpcsrc\rtl\objpas\classes\lists.inc
}

unit MFPC.Classes.Lists;

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

{$IFNDEF FPC_TESTGENERICS}
  TListAssignOp = (laCopy, laAnd, laOr, laXor, laSrcUnique, laDestUnique);
  TM_FPList = class;

  TM_FPListEnumerator = class
  private
    FList: TM_FPList;
    FPosition: Integer;
  public
    constructor Create(AList: TM_FPList);
    function GetCurrent: Pointer;
    function MoveNext: Boolean;
    property Current: Pointer read GetCurrent;
  end;

  TM_FPList = class(TObject)
  private
    FList: PPointerList;
    FCount: Integer;
    FCapacity: Integer;
    procedure CopyMove(AList: TM_FPList);
    procedure MergeMove(AList: TM_FPList);
    procedure DoCopy(ListA, ListB: TM_FPList);
    procedure DoSrcUnique(ListA, ListB: TM_FPList);
    procedure DoAnd(ListA, ListB: TM_FPList);
    procedure DoDestUnique(ListA, ListB: TM_FPList);
    procedure DoOr(ListA, ListB: TM_FPList);
    procedure DoXOr(ListA, ListB: TM_FPList);
  protected
    function Get(Index: Integer): Pointer; {$IFDEF CLASSESINLINE} inline; {$ENDIF CLASSESINLINE}
    procedure Put(Index: Integer; Item: Pointer); {$IFDEF CLASSESINLINE} inline; {$ENDIF CLASSESINLINE}
    procedure SetCapacity(NewCapacity: Integer);
    procedure SetCount(NewCount: Integer);
    Procedure RaiseIndexError(Index: Integer); deprecated;
    Procedure CheckIndex(AIndex: Integer); {$IFDEF CLASSESINLINE} inline; {$ENDIF}
  public
    Type
      TDirection = (FromBeginning, FromEnd);
    destructor Destroy; override;
    Procedure AddList(AList: TM_FPList);
    function Add(Item: Pointer): Integer; {$IFDEF CLASSESINLINE} inline; {$ENDIF CLASSESINLINE}
    procedure Clear;
    procedure Delete(Index: Integer); {$IFDEF CLASSESINLINE} inline; {$ENDIF CLASSESINLINE}
    class procedure Error(const Msg: string; Data: PtrInt);
    procedure Exchange(Index1, Index2: Integer);
    function Expand: TM_FPList; {$IFDEF CLASSESINLINE} inline; {$ENDIF CLASSESINLINE}
    function Extract(Item: Pointer): Pointer;
    function First: Pointer;
    function GetEnumerator: TM_FPListEnumerator;
    function IndexOf(Item: Pointer): Integer;
    function IndexOfItem(Item: Pointer; Direction: TDirection): Integer;
    procedure Insert(Index: Integer; Item: Pointer); {$IFDEF CLASSESINLINE} inline; {$ENDIF CLASSESINLINE}
    function Last: Pointer;
    procedure Move(CurIndex, NewIndex: Integer);
    procedure Assign(ListA: TM_FPList; AOperator: TListAssignOp = laCopy; ListB: TM_FPList = nil);
    function Remove(Item: Pointer): Integer;
    procedure Pack;
    procedure Sort(Compare: TListSortCompare);
    procedure ForEachCall(proc2call: TListCallback; arg: Pointer);
    procedure ForEachCall(proc2call: TListStaticCallback; arg: Pointer);
    property Capacity: Integer read FCapacity write SetCapacity;
    property Count: Integer read FCount write SetCount;
    property Items[Index: Integer]: Pointer read Get write Put; default;
    property List: PPointerList read FList;
  end;

{$ELSE}

  TM_FPPtrList = specialize TFPGList<Pointer>;

  TM_FPList = class(TFPPtrList)
  public
    procedure Assign(Source: TFPList);
    procedure Sort(Compare: TListSortCompare);
    procedure ForEachCall(proc2call: TListCallback; arg: Pointer);
    procedure ForEachCall(proc2call: TListStaticCallback; arg: Pointer);
  end;

{$ENDIF}
{ TM_List class}

  TListNotification = (lnAdded, lnExtracted, lnDeleted);
  TM_List = class;

  TM_ListEnumerator = class
  private
    FList: TM_List;
    FPosition: Integer;
  public
    constructor Create(AList: TM_List);
    function GetCurrent: Pointer;
    function MoveNext: Boolean;
    property Current: Pointer read GetCurrent;
  end;

  TM_List = class(TObject, IFPObserved)
  private
    FList: TM_FPList;
    FObservers: TM_FPList;
    procedure CopyMove(AList: TM_List);
    procedure MergeMove(AList: TM_List);
    procedure DoCopy(ListA, ListB: TM_List);
    procedure DoSrcUnique(ListA, ListB: TM_List);
    procedure DoAnd(ListA, ListB: TM_List);
    procedure DoDestUnique(ListA, ListB: TM_List);
    procedure DoOr(ListA, ListB: TM_List);
    procedure DoXOr(ListA, ListB: TM_List);
  protected
    function Get(Index: Integer): Pointer;
    procedure Grow; virtual;
    procedure Put(Index: Integer; Item: Pointer);
    procedure Notify(Ptr: Pointer; Action: TListNotification); virtual;
    procedure SetCapacity(NewCapacity: Integer);
    function GetCapacity: Integer;
    procedure SetCount(NewCount: Integer);
    function GetCount: Integer;
    function GetList: PPointerList;
  public
    constructor Create;
    destructor Destroy; override;
    Procedure FPOAttachObserver(AObserver: TObject);
    Procedure FPODetachObserver(AObserver: TObject);
    Procedure FPONotifyObservers(ASender: TObject; AOperation: TFPObservedOperation; Data: Pointer);
    Procedure AddList(AList: TM_List);
    function Add(Item: Pointer): Integer;
    procedure Clear; virtual;
    procedure Delete(Index: Integer);
    class procedure Error(const Msg: string; Data: PtrInt); virtual;
    procedure Exchange(Index1, Index2: Integer);
    function Expand: TM_List;
    function Extract(Item: Pointer): Pointer;
    function First: Pointer;
    function GetEnumerator: TM_ListEnumerator;
    function IndexOf(Item: Pointer): Integer;
    procedure Insert(Index: Integer; Item: Pointer);
    function Last: Pointer;
    procedure Move(CurIndex, NewIndex: Integer);
    procedure Assign(ListA: TM_List; AOperator: TListAssignOp = laCopy; ListB: TM_List = nil);
    function Remove(Item: Pointer): Integer;
    procedure Pack;
    procedure Sort(Compare: TListSortCompare);
    property Capacity: Integer read GetCapacity write SetCapacity;
    property Count: Integer read GetCount write SetCount;
    property Items[Index: Integer]: Pointer read Get write Put; default;
    property List: PPointerList read GetList;
  end;


  TM_ThreadList = class
  private
    FList: TM_List;
    FDuplicates: TDuplicates;
    FLock: TRTLCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(Item: Pointer);
    procedure Clear;
    function  LockList: TM_List;
    procedure Remove(Item: Pointer);
    procedure UnlockList;
    property Duplicates: TDuplicates read FDuplicates write FDuplicates;
  end;

implementation

{$IF defined(VER2_0) or not defined(FPC_TESTGENERICS)}


{$REGION '                      TM_FPListEnumerator                           '}

constructor TM_FPListEnumerator.Create(AList: TM_FPList);
begin
  inherited Create;
  FList := AList;
  FPosition := -1;
end;

function TM_FPListEnumerator.GetCurrent: Pointer;
begin
  Result := FList[FPosition];
end;

function TM_FPListEnumerator.MoveNext: Boolean;
begin
  Inc(FPosition);
  Result := FPosition < FList.Count;
end;
{$ENDREGION TM_FPListEnumerator}


{$REGION '                           TM_FPList                              '}

Const
  // Ratio of Pointer and Word Size.
  WordRatio = SizeOf(Pointer) Div SizeOf(Word);

procedure TM_FPList.RaiseIndexError(Index: Integer);
begin
  // We should be able to remove this, but unfortunately it is marked protected.
  Error(SListIndexError, Index);
end;

Procedure TM_FPList.CheckIndex(AIndex: Integer);

begin
  If (AIndex < 0) or (AIndex >= FCount) then Error(SListIndexError, AIndex); // Don't use RaiseIndexError, exception address will be better if we use error.
end;

function TM_FPList.Get(Index: Integer): Pointer;
begin
  CheckIndex(Index);
  Result := FList^[Index];
end;

procedure TM_FPList.Put(Index: Integer; Item: Pointer);
begin
  CheckIndex(Index);
  FList^[Index] := Item;
end;

function TM_FPList.Extract(Item: Pointer): Pointer;
var
  i: Integer;
begin
  i := IndexOf(Item);
  if i >= 0 then
  begin
    Result := Item;
    Delete(i);
  end
  else Result := nil;
end;

procedure TM_FPList.SetCapacity(NewCapacity: Integer);
begin
  If (NewCapacity < FCount) or (NewCapacity > MaxListSize) then Error(SListCapacityError, NewCapacity);
  if NewCapacity = FCapacity then exit;
  ReallocMem(FList, SizeOf(Pointer) * NewCapacity);
  FCapacity := NewCapacity;
end;

procedure TM_FPList.SetCount(NewCount: Integer);
begin
  if (NewCount < 0) or (NewCount > MaxListSize) then Error(SListCountError, NewCount);
  If NewCount > FCount then
  begin
    If NewCount > FCapacity then SetCapacity(NewCount);
    If FCount < NewCount then FillWord(FList^[FCount], (NewCount - FCount) * WordRatio, 0);
  end;
  FCount := NewCount;
end;

destructor TM_FPList.Destroy;
begin
  Self.Clear;
  inherited Destroy;
end;

Procedure TM_FPList.AddList(AList: TM_FPList);
Var
  i: Integer;
begin
  If (Capacity < Count + AList.Count) then Capacity := Count + AList.Count;
  For i := 0 to AList.Count - 1 do Add(AList[i]);
end;

function TM_FPList.Add(Item: Pointer): Integer;
begin
  if FCount = FCapacity then Self.Expand;
  FList^[FCount] := Item;
  Result := FCount;
  FCount := FCount + 1;
end;

procedure TM_FPList.Clear;
begin
  if Assigned(FList) then
  begin
    SetCount(0);
    SetCapacity(0);
    FList := nil;
  end;
end;

procedure TM_FPList.Delete(Index: Integer);
begin
  CheckIndex(Index);
  FCount := FCount - 1;
  System.Move(FList^[Index + 1], FList^[Index], (FCount - Index) * SizeOf(Pointer));
  // Shrink the list if appropriate:
  // If capacity>256 and the list is less than a quarter filled,  shrink to 1/2 the size.
  // Shr is used because it is faster than div.
  if (FCapacity > 256) and (FCount < FCapacity shr 2) then
  begin
    FCapacity := FCapacity shr 1;
    ReallocMem(FList, SizeOf(Pointer) * FCapacity);
  end;
end;

class procedure TM_FPList.Error(const Msg: string; Data: PtrInt);
begin
  Raise EListError.CreateFmt(Msg, [Data])at get_caller_addr(get_frame), get_caller_frame(get_frame);
end;

procedure TM_FPList.Exchange(Index1, Index2: Integer);
var
  Temp: Pointer;
begin
  CheckIndex(Index1);
  CheckIndex(Index2);
  Temp := FList^[Index1];
  FList^[Index1] := FList^[Index2];
  FList^[Index2] := Temp;
end;

function TM_FPList.Expand: TM_FPList;
var
  IncSize: Longint;
begin
  if FCount < FCapacity then exit(Self);
  {
    For really big lists, (128Mb elements), increase with fixed amount: 16Mb elements (=1/8th of 128Mb).
    For big lists (8mb elements), increase with 1/8th of the size
    For moderate lists (128 or more, increase with 1/4th the size
    For smaller sizes, increase with 16 or 4
  }
  if FCapacity > 128 * 1024 * 1024 then IncSize := 16 * 1024 * 1024
  else if FCapacity > 8 * 1024 * 1024 then IncSize := FCapacity shr 3
  else if FCapacity > 128 then IncSize := FCapacity shr 2
  else if FCapacity > 8 then IncSize := 16
  else IncSize := 4;
  SetCapacity(FCapacity + IncSize);
  Result := Self;
end;

function TM_FPList.First: Pointer;
begin
  If FCount = 0 then Result := Nil
  else Result := Items[0];
end;

function TM_FPList.GetEnumerator: TM_FPListEnumerator;
begin
  Result := TM_FPListEnumerator.Create(Self);
end;

function TM_FPList.IndexOf(Item: Pointer): Integer;

Var
  C: Integer;

begin
  Result := 0;
  C := Count;
  while (Result < C) and (FList^[Result] <> Item) do Inc(Result);
  If Result >= C then Result := -1;
end;

function TM_FPList.IndexOfItem(Item: Pointer; Direction: TDirection): Integer;

begin
  if Direction = FromBeginning then Result := IndexOf(Item)
  else
  begin
    Result := Count - 1;
    while (Result >= 0) and (FList^[Result] <> Item) do Result := Result - 1;
  end;
end;

procedure TM_FPList.Insert(Index: Integer; Item: Pointer);
begin
  if (Index < 0) or (Index > FCount) then Error(SListIndexError, Index);
  iF FCount = FCapacity then Self.Expand;
  if Index < FCount then System.Move(FList^[Index], FList^[Index + 1], (FCount - Index) * SizeOf(Pointer));
  FList^[Index] := Item;
  FCount := FCount + 1;
end;

function TM_FPList.Last: Pointer;
begin
{ Wouldn't it be better to return nil if the count is zero ?}
  If FCount = 0 then Result := nil
  else Result := Items[FCount - 1];
end;

procedure TM_FPList.Move(CurIndex, NewIndex: Integer);
var
  Temp: Pointer;
begin
  CheckIndex(CurIndex);
  CheckIndex(NewIndex);
  Temp := FList^[CurIndex];
  if NewIndex > CurIndex then System.Move(FList^[CurIndex + 1], FList^[CurIndex], (NewIndex - CurIndex) * SizeOf(Pointer))
  else System.Move(FList^[NewIndex], FList^[NewIndex + 1], (CurIndex - NewIndex) * SizeOf(Pointer));
  FList^[NewIndex] := Temp;
end;

function TM_FPList.Remove(Item: Pointer): Integer;
begin
  Result := IndexOf(Item);
  If Result <> -1 then Self.Delete(Result);
end;

procedure TM_FPList.Pack;
var
  NewCount, i: Integer;
  pdest, psrc: PPointer;
begin
  NewCount := 0;
  psrc := @FList^[0];
  pdest := psrc;
  For i := 0 To FCount - 1 Do
  begin
    if Assigned(psrc^) then
    begin
      pdest^ := psrc^;
      Inc(pdest);
      Inc(NewCount);
    end;
    Inc(psrc);
  end;
  FCount := NewCount;
end;

// Needed by Sort method.

Procedure QuickSort(FList: PPointerList; L, R: Longint; Compare: TListSortCompare);
var
  i, J: Longint;
  P, Q: Pointer;
begin
  repeat
    i := L;
    J := R;
    P := FList^[(L + R) div 2];
    repeat
      while Compare(P, FList^[i]) > 0 do i := i + 1;
      while Compare(P, FList^[J]) < 0 do J := J - 1;
      If i <= J then
      begin
        Q := FList^[i];
        FList^[i] := FList^[J];
        FList^[J] := Q;
        i := i + 1;
        J := J - 1;
      end;
    until i > J;
   // sort the smaller range recursively
   // sort the bigger range via the loop
   // Reasons: memory usage is O(log(n)) instead of O(n) and loop is faster than recursion
    if J - L < R - i then
    begin
      if L < J then QuickSort(FList, L, J, Compare);
      L := i;
    end
    else
    begin
      if i < R then QuickSort(FList, i, R, Compare);
      R := J;
    end;
  until L >= R;
end;

procedure TM_FPList.Sort(Compare: TListSortCompare);
begin
  if Not Assigned(FList) or (FCount < 2) then exit;
  QuickSort(FList, 0, FCount - 1, Compare);
end;

procedure TM_FPList.ForEachCall(proc2call: TListCallback; arg: Pointer);
var
  i: Integer;
  P: Pointer;
begin
  For i := 0 To Count - 1 Do
  begin
    P := FList^[i];
    if Assigned(P) then proc2call(P, arg);
  end;
end;

procedure TM_FPList.ForEachCall(proc2call: TListStaticCallback; arg: Pointer);
var
  i: Integer;
  P: Pointer;
begin
  For i := 0 To Count - 1 Do
  begin
    P := FList^[i];
    if Assigned(P) then proc2call(P, arg);
  end;
end;

procedure TM_FPList.CopyMove(AList: TM_FPList);
var
  R: Integer;
begin
  Clear;
  for R := 0 to AList.Count - 1 do Add(AList[R]);
end;

procedure TM_FPList.MergeMove(AList: TM_FPList);
var
  R: Integer;
begin
  For R := 0 to AList.Count - 1 do
    if Self.IndexOf(AList[R]) < 0 then Self.Add(AList[R]);
end;

procedure TM_FPList.DoCopy(ListA, ListB: TM_FPList);
begin
  if Assigned(ListB) then CopyMove(ListB)
  else CopyMove(ListA);
end;

procedure TM_FPList.DoDestUnique(ListA, ListB: TM_FPList);
  procedure MoveElements(src, dest: TM_FPList);
  var
    R: Integer;
  begin
    Self.Clear;
    for R := 0 to src.Count - 1 do
      if dest.IndexOf(src[R]) < 0 then Self.Add(src[R]);
  end;

var
  dest: TM_FPList;
begin
  if Assigned(ListB) then MoveElements(ListB, ListA)
  else
    try
      dest := TM_FPList.Create;
      dest.CopyMove(Self);
      MoveElements(ListA, dest)
    finally
      dest.Free;
    end;
end;

procedure TM_FPList.DoAnd(ListA, ListB: TM_FPList);
var
  R: Integer;
begin
  if Assigned(ListB) then
  begin
    Self.Clear;
    for R := 0 to ListA.Count - 1 do
      if ListB.IndexOf(ListA[R]) >= 0 then Self.Add(ListA[R]);
  end
  else
  begin
    for R := Self.Count - 1 downto 0 do
      if ListA.IndexOf(Self[R]) < 0 then Self.Delete(R);
  end;
end;

procedure TM_FPList.DoSrcUnique(ListA, ListB: TM_FPList);
var
  R: Integer;
begin
  if Assigned(ListB) then
  begin
    Self.Clear;
    for R := 0 to ListA.Count - 1 do
      if ListB.IndexOf(ListA[R]) < 0 then Self.Add(ListA[R]);
  end
  else
  begin
    for R := Self.Count - 1 downto 0 do
      if ListA.IndexOf(Self[R]) >= 0 then Self.Delete(R);
  end;
end;

procedure TM_FPList.DoOr(ListA, ListB: TM_FPList);
begin
  if Assigned(ListB) then
  begin
    CopyMove(ListA);
    MergeMove(ListB);
  end
  else MergeMove(ListA);
end;

procedure TM_FPList.DoXOr(ListA, ListB: TM_FPList);
var
  R: Integer;
  L: TM_FPList;
begin
  if Assigned(ListB) then
  begin
    Self.Clear;
    for R := 0 to ListA.Count - 1 do
      if ListB.IndexOf(ListA[R]) < 0 then Self.Add(ListA[R]);
    for R := 0 to ListB.Count - 1 do
      if ListA.IndexOf(ListB[R]) < 0 then Self.Add(ListB[R]);
  end
  else
    try
      L := TM_FPList.Create;
      L.CopyMove(Self);
      for R := Self.Count - 1 downto 0 do
        if ListA.IndexOf(Self[R]) >= 0 then Self.Delete(R);
      for R := 0 to ListA.Count - 1 do
        if L.IndexOf(ListA[R]) < 0 then Self.Add(ListA[R]);
    finally
      L.Free;
    end;
end;

procedure TM_FPList.Assign(ListA: TM_FPList; AOperator: TListAssignOp = laCopy; ListB: TM_FPList = nil);
begin
  case AOperator of
    laCopy: DoCopy(ListA, ListB); // replace dest with src
    laSrcUnique: DoSrcUnique(ListA, ListB); // replace dest with src that are not in dest
    laAnd: DoAnd(ListA, ListB); // remove from dest that are not in src
    laDestUnique: DoDestUnique(ListA, ListB); // remove from dest that are in src
    laOr: DoOr(ListA, ListB); // add to dest from src and not in dest
    laXor: DoXOr(ListA, ListB); // add to dest from src and not in dest, remove from dest that are in src
  end;
end;

{$ELSE}
{ generics based implementation of TFPList follows }

procedure TFPList.Assign(Source: TFPList);
begin
  inherited Assign(Source);
end;

type
  TFPPtrListSortCompare = function(const Item1, Item2: Pointer): Integer;

procedure TFPList.Sort(Compare: TListSortCompare);
begin
  inherited Sort(TFPPtrListSortCompare(Compare));
end;

procedure TFPList.ForEachCall(proc2call: TListCallback; arg: Pointer);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do proc2call(InternalItems[i], arg);
end;

procedure TFPList.ForEachCall(proc2call: TListStaticCallback; arg: Pointer);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do proc2call(InternalItems[i], arg);
end;

{$ENDIF}
{$ENDREGION TM_FPList}


{$region '                        TM_ListEnumerator                         '}
constructor TM_ListEnumerator.Create(AList: TM_List);
begin
  inherited Create;
  FList := AList;
  FPosition := -1;
end;

function TM_ListEnumerator.GetCurrent: Pointer;
begin
  Result := FList[FPosition];
end;

function TM_ListEnumerator.MoveNext: Boolean;
begin
  Inc(FPosition);
  Result := FPosition < FList.Count;
end;
{$endregion TM_ListEnumerator}


{$REGION '                           TM_List                                 '}

{  TM_List = class(TObject)
  private
    FList: TM_FPList;
}

function TM_List.Get(Index: Integer): Pointer;
begin
  Result := FList.Get(Index);
end;

procedure TM_List.Grow;
begin
  // Only for compatibility with Delphi. Not needed.
end;

procedure TM_List.Put(Index: Integer; Item: Pointer);
var
  P: Pointer;
begin
  P := Get(Index);
  FList.Put(Index, Item);
  if Assigned(P) then Notify(P, lnDeleted);
  if Assigned(Item) then Notify(Item, lnAdded);
end;

function TM_List.Extract(Item: Pointer): Pointer;
var
  C: Integer;
begin
  C := FList.Count;
  Result := FList.Extract(Item);
  if C <> FList.Count then Notify(Result, lnExtracted);
end;

procedure TM_List.Notify(Ptr: Pointer; Action: TListNotification);
begin
  if Assigned(FObservers) then
    Case Action of
      lnAdded: FPONotifyObservers(Self, ooAddItem, Ptr);
      lnExtracted: FPONotifyObservers(Self, ooDeleteItem, Ptr);
      lnDeleted: FPONotifyObservers(Self, ooDeleteItem, Ptr);
    end;
end;

function TM_List.GetCapacity: Integer;
begin
  Result := FList.Capacity;
end;

procedure TM_List.SetCapacity(NewCapacity: Integer);
begin
  FList.SetCapacity(NewCapacity);
end;

function TM_List.GetCount: Integer;
begin
  Result := FList.Count;
end;

procedure TM_List.SetCount(NewCount: Integer);
begin
  if NewCount < FList.Count then
    while FList.Count > NewCount do Delete(FList.Count - 1)
  else FList.SetCount(NewCount);
end;

constructor TM_List.Create;
begin
  inherited Create;
  FList := TM_FPList.Create;
end;

destructor TM_List.Destroy;
begin
  if Assigned(FList) then Clear;
  If Assigned(FObservers) then
  begin
    FPONotifyObservers(Self, ooFree, Nil);
    FreeAndNil(FObservers);
  end;
  FreeAndNil(FList);
  inherited Destroy;
end;

procedure TM_List.FPOAttachObserver(AObserver: TObject);

Var
  i: IFPObserver;

begin
  If Not AObserver.GetInterface(SGUIDObserver, i) then Raise EObserver.CreateFmt(SErrNotObserver, [AObserver.ClassName]);
  If not Assigned(FObservers) then FObservers := TM_FPList.Create;
  FObservers.Add(i);
end;

procedure TM_List.FPODetachObserver(AObserver: TObject);
Var
  i: IFPObserver;

begin
  If Not AObserver.GetInterface(SGUIDObserver, i) then Raise EObserver.CreateFmt(SErrNotObserver, [AObserver.ClassName]);
  If Assigned(FObservers) then
  begin
    FObservers.Remove(i);
    If (FObservers.Count = 0) then FreeAndNil(FObservers);
  end;
end;

procedure TM_List.FPONotifyObservers(ASender: TObject; AOperation: TFPObservedOperation; Data: Pointer);

Var
  i: Integer;
  Obs: IFPObserver;

begin
  If Assigned(FObservers) then
    For i := FObservers.Count - 1 downto 0 do
    begin
      Obs := IFPObserver(FObservers[i]);
      Obs.FPOObservedChanged(ASender, AOperation, Data);
    end;
end;

function TM_List.Add(Item: Pointer): Integer;
begin
  Result := FList.Add(Item);
  if Item <> nil then Notify(Item, lnAdded);
end;

Procedure TM_List.AddList(AList: TM_List);
var
  i: Integer;
begin
  { this only does FList.AddList(AList.FList), avoiding notifications }
  FList.AddList(AList.FList);

  { make lnAdded notifications }
  for i := 0 to AList.Count - 1 do
    if AList[i] <> nil then Notify(AList[i], lnAdded);
end;

procedure TM_List.Clear;

begin
  While (FList.Count > 0) do Delete(Count - 1);
end;

procedure TM_List.Delete(Index: Integer);

var
  P: Pointer;

begin
  P := FList.Get(Index);
  FList.Delete(Index);
  if Assigned(P) then Notify(P, lnDeleted);
end;

class procedure TM_List.Error(const Msg: string; Data: PtrInt);
begin
  Raise EListError.CreateFmt(Msg, [Data])at get_caller_addr(get_frame), get_caller_frame(get_frame);
end;

procedure TM_List.Exchange(Index1, Index2: Integer);
begin
  FList.Exchange(Index1, Index2);
  FPONotifyObservers(Self, ooChange, Nil);
end;

function TM_List.Expand: TM_List;
begin
  FList.Expand;
  Result := Self;
end;

function TM_List.First: Pointer;
begin
  Result := FList.First;
end;

function TM_List.GetEnumerator: TM_ListEnumerator;
begin
  Result := TM_ListEnumerator.Create(Self);
end;

function TM_List.IndexOf(Item: Pointer): Integer;
begin
  Result := FList.IndexOf(Item);
end;

procedure TM_List.Insert(Index: Integer; Item: Pointer);
begin
  FList.Insert(Index, Item);
  if Item <> nil then Notify(Item, lnAdded);
end;

function TM_List.Last: Pointer;
begin
  Result := FList.Last;
end;

procedure TM_List.Move(CurIndex, NewIndex: Integer);
begin
  FList.Move(CurIndex, NewIndex);
end;

function TM_List.Remove(Item: Pointer): Integer;
begin
  Result := IndexOf(Item);
  if Result <> -1 then Self.Delete(Result);
end;

procedure TM_List.Pack;
begin
  FList.Pack;
end;

procedure TM_List.Sort(Compare: TListSortCompare);
begin
  FList.Sort(Compare);
end;

procedure TM_List.CopyMove(AList: TM_List);
var
  R: Integer;
begin
  Clear;
  for R := 0 to AList.Count - 1 do Add(AList[R]);
end;

procedure TM_List.MergeMove(AList: TM_List);
var
  R: Integer;
begin
  For R := 0 to AList.Count - 1 do
    if Self.IndexOf(AList[R]) < 0 then Self.Add(AList[R]);
end;

procedure TM_List.DoCopy(ListA, ListB: TM_List);
begin
  if Assigned(ListB) then CopyMove(ListB)
  else CopyMove(ListA);
end;

procedure TM_List.DoDestUnique(ListA, ListB: TM_List);
  procedure MoveElements(src, dest: TM_List);
  var
    R: Integer;
  begin
    Self.Clear;
    for R := 0 to src.Count - 1 do
      if dest.IndexOf(src[R]) < 0 then Self.Add(src[R]);
  end;

var
  dest: TM_List;
begin
  if Assigned(ListB) then MoveElements(ListB, ListA)
  else
    try
      dest := TM_List.Create;
      dest.CopyMove(Self);
      MoveElements(ListA, dest)
    finally
      dest.Free;
    end;
end;

procedure TM_List.DoAnd(ListA, ListB: TM_List);
var
  R: Integer;
begin
  if Assigned(ListB) then
  begin
    Self.Clear;
    for R := 0 to ListA.Count - 1 do
      if ListB.IndexOf(ListA[R]) >= 0 then Self.Add(ListA[R]);
  end
  else
  begin
    for R := Self.Count - 1 downto 0 do
      if ListA.IndexOf(Self[R]) < 0 then Self.Delete(R);
  end;
end;

procedure TM_List.DoSrcUnique(ListA, ListB: TM_List);
var
  R: Integer;
begin
  if Assigned(ListB) then
  begin
    Self.Clear;
    for R := 0 to ListA.Count - 1 do
      if ListB.IndexOf(ListA[R]) < 0 then Self.Add(ListA[R]);
  end
  else
  begin
    for R := Self.Count - 1 downto 0 do
      if ListA.IndexOf(Self[R]) >= 0 then Self.Delete(R);
  end;
end;

procedure TM_List.DoOr(ListA, ListB: TM_List);
begin
  if Assigned(ListB) then
  begin
    CopyMove(ListA);
    MergeMove(ListB);
  end
  else MergeMove(ListA);
end;

procedure TM_List.DoXOr(ListA, ListB: TM_List);
var
  R: Integer;
  L: TM_List;
begin
  if Assigned(ListB) then
  begin
    Self.Clear;
    for R := 0 to ListA.Count - 1 do
      if ListB.IndexOf(ListA[R]) < 0 then Self.Add(ListA[R]);
    for R := 0 to ListB.Count - 1 do
      if ListA.IndexOf(ListB[R]) < 0 then Self.Add(ListB[R]);
  end
  else
    try
      L := TM_List.Create;
      L.CopyMove(Self);
      for R := Self.Count - 1 downto 0 do
        if ListA.IndexOf(Self[R]) >= 0 then Self.Delete(R);
      for R := 0 to ListA.Count - 1 do
        if L.IndexOf(ListA[R]) < 0 then Self.Add(ListA[R]);
    finally
      L.Free;
    end;
end;

procedure TM_List.Assign(ListA: TM_List; AOperator: TListAssignOp = laCopy; ListB: TM_List = nil);
begin
  case AOperator of
    laCopy: DoCopy(ListA, ListB); // replace dest with src
    laSrcUnique: DoSrcUnique(ListA, ListB); // replace dest with src that are not in dest
    laAnd: DoAnd(ListA, ListB); // remove from dest that are not in src
    laDestUnique: DoDestUnique(ListA, ListB); // remove from dest that are in src
    laOr: DoOr(ListA, ListB); // add to dest from src and not in dest
    laXor: DoXOr(ListA, ListB); // add to dest from src and not in dest, remove from dest that are in src
  end;
end;

function TM_List.GetList: PPointerList;
begin
  Result := PPointerList(FList.List);
end;
{$ENDREGION TM_List}


{$region '                         TM_ThreadList                            '}
constructor TM_ThreadList.Create;
begin
  inherited Create;
  FDuplicates := dupIgnore;
{$IFDEF FPC_HAS_FEATURE_THREADING}
  InitCriticalSection(FLock);
{$ENDIF}
  FList := TM_List.Create;
end;

destructor TM_ThreadList.Destroy;
begin
  LockList;
  try
    FList.Free;
    inherited Destroy;
  finally
    UnlockList;
{$IFDEF FPC_HAS_FEATURE_THREADING}
    DoneCriticalSection(FLock);
{$ENDIF}
  end;
end;

procedure TM_ThreadList.Add(Item: Pointer);
begin
  LockList;
  try
    if (Duplicates = dupAccept) or
        // make sure it's not already in the list
      (FList.IndexOf(Item) = -1) then FList.Add(Item)
    else if (Duplicates = dupError) then FList.Error(SDuplicateItem, PtrUInt(Item));
  finally
    UnlockList;
  end;
end;

procedure TM_ThreadList.Clear;
begin
  LockList;
  try
    FList.Clear;
  finally
    UnlockList;
  end;
end;

function TM_ThreadList.LockList: TM_List;
begin
  Result := FList;
{$IFDEF FPC_HAS_FEATURE_THREADING}
  System.EnterCriticalSection(FLock);
{$ENDIF}
end;

procedure TM_ThreadList.Remove(Item: Pointer);
begin
  LockList;
  try
    FList.Remove(Item);
  finally
    UnlockList;
  end;
end;

procedure TM_ThreadList.UnlockList;
begin
{$IFDEF FPC_HAS_FEATURE_THREADING}
  System.LeaveCriticalSection(FLock);
{$ENDIF}
end;

{$endregion TM_ThreadList}

end.
