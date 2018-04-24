{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2002 by Florian Klaempfl

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$ifdef fpc}
{$mode objfpc}
{$endif}
{$H+}
{$ifdef CLASSESINLINE}{$inline on}{$endif}

unit MFPC.Contnrs;
{$WARN 4055 off : Conversion between ordinals and pointers is not portable}
{$WARN 4044 off : Comparison might be always false due to range of constant and expression}
{$WARN 4082 off : Converting pointers to signed integers may result in wrong comparison results and range errors, use an unsigned type instead.}
interface

uses
  SysUtils, MFPC.Classes.SHARED, MFPC.Classes.Lists;


Type
  TM_ObjectListCallback = Procedure(data:TObject;arg:pointer) of object;
  TM_ObjectListStaticCallback = Procedure(data:TObject;arg:pointer);

  TM_FPObjectList = class(TObject)
  private
    FFreeObjects : Boolean;
    FList: TM_FPList;
    Function GetCount: integer;
    Procedure SetCount(const AValue: integer);
  protected
    Function GetItem(Index: Integer): TObject; {$ifdef CLASSESINLINE}inline;{$endif}
    Procedure SetItem(Index: Integer; AObject: TObject); {$ifdef CLASSESINLINE}inline;{$endif}
    Procedure SetCapacity(NewCapacity: Integer);
    Function GetCapacity: integer;
  public
    constructor Create;
    constructor Create(FreeObjects : Boolean);
    destructor Destroy; override;
    Procedure Clear;
    Function Add(AObject: TObject): Integer; {$ifdef CLASSESINLINE}inline;{$endif}
    Procedure Delete(Index: Integer); {$ifdef CLASSESINLINE}inline;{$endif}
    Procedure Exchange(Index1, Index2: Integer);
    Function Expand: TM_FPObjectList;
    Function Extract(Item: TObject): TObject;
    Function Remove(AObject: TObject): Integer;
    Function IndexOf(AObject: TObject): Integer;
    Function FindInstanceOf(AClass: TClass; AExact: Boolean; AStartAt: Integer): Integer;
    Procedure Insert(Index: Integer; AObject: TObject); {$ifdef CLASSESINLINE}inline;{$endif}
    Function First: TObject;
    Function Last: TObject;
    Procedure Move(CurIndex, NewIndex: Integer);
    Procedure Assign(Obj:TM_FPObjectList);
    Procedure Pack;
    Procedure Sort(Compare: TListSortCompare);
    Procedure ForEachCall(proc2call:TM_ObjectListCallback;arg:pointer);
    Procedure ForEachCall(proc2call:TM_ObjectListStaticCallback;arg:pointer);
    property Capacity: Integer read GetCapacity write SetCapacity;
    property Count: Integer read GetCount write SetCount;
    property OwnsObjects: Boolean read FFreeObjects write FFreeObjects;
    property Items[Index: Integer]: TObject read GetItem write SetItem; default;
    property List: TM_FPList read FList;
  end;


  { TM_ObjectList }

  TM_ObjectList = class(TM_List)
  private
    FFreeObjects : Boolean;
  Protected
    Procedure Notify(Ptr: Pointer; Action: TListNotification); override;
    Function GetItem(Index: Integer): TObject;
    Procedure SetItem(Index: Integer; AObject: TObject);
  public
    constructor Create;
    constructor Create(FreeObjects : boolean);
    Function Add(AObject: TObject): Integer;
    Function Extract(Item: TObject): TObject;
    Function Remove(AObject: TObject): Integer;
    Function IndexOf(AObject: TObject): Integer;
    Function FindInstanceOf(AClass: TClass; AExact: Boolean; AStartAt: Integer): Integer;
    Procedure Insert(Index: Integer; AObject: TObject);
    Function First: TObject;
    Function Last: TObject;
    property OwnsObjects: Boolean read FFreeObjects write FFreeObjects;
    property Items[Index: Integer]: TObject read GetItem write SetItem; default;
  end;

  //TComponentList = class(TObjectList)
  //Private
  //  FNotifier : TComponent;
  //Protected
  //  Procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  //  Function GetItems(Index: Integer): TComponent;
  //  Procedure SetItems(Index: Integer; AComponent: TComponent);
  //  Procedure HandleFreeNotify(Sender: TObject; AComponent: TComponent);
  //public
  //  destructor Destroy; override;
  //  Function Add(AComponent: TComponent): Integer;
  //  Function Extract(Item: TComponent): TComponent;
  //  Function Remove(AComponent: TComponent): Integer;
  //  Function IndexOf(AComponent: TComponent): Integer;
  //  Function First: TComponent;
  //  Function Last: TComponent;
  //  Procedure Insert(Index: Integer; AComponent: TComponent);
  //  property Items[Index: Integer]: TComponent read GetItems write SetItems; default;
  //end;

  TM_ClassList = class(TM_List)
  protected
    Function GetItems(Index: Integer): TClass;
    Procedure SetItems(Index: Integer; AClass: TClass);
  public
    Function Add(AClass: TClass): Integer;
    Function Extract(Item: TClass): TClass;
    Function Remove(AClass: TClass): Integer;
    Function IndexOf(AClass: TClass): Integer;
    Function First: TClass;
    Function Last: TClass;
    Procedure Insert(Index: Integer; AClass: TClass);
    property Items[Index: Integer]: TClass read GetItems write SetItems; default;
  end;

  TM_OrderedList = class(TObject)
  private
    FList: TM_List;
  protected
    Procedure PushItem(AItem: Pointer); virtual; abstract;
    Function PopItem: Pointer; virtual;
    Function PeekItem: Pointer; virtual;
    property List: TM_List read FList;
  public
    constructor Create;
    destructor Destroy; override;
    Function Count: Integer;
    Function AtLeast(ACount: Integer): Boolean;
    Function Push(AItem: Pointer): Pointer;
    Function Pop: Pointer;
    Function Peek: Pointer;
  end;

{ TM_Stack class }

  TM_Stack = class(TM_OrderedList)
  protected
    Procedure PushItem(AItem: Pointer); override;
  end;

{ TM_ObjectStack class }

  TM_ObjectStack = class(TM_Stack)
  public
    Function Push(AObject: TObject): TObject;
    Function Pop: TObject;
    Function Peek: TObject;
  end;

{ TM_Queue class }

  TM_Queue = class(TM_OrderedList)
  protected
    Procedure PushItem(AItem: Pointer); override;
  end;

{ TM_ObjectQueue class }

  TM_ObjectQueue = class(TM_Queue)
  public
    Function Push(AObject: TObject): TObject;
    Function Pop: TObject;
    Function Peek: TObject;
  end;

{ ---------------------------------------------------------------------
    TFPList with Hash support
  ---------------------------------------------------------------------}

type
  TM_HashItem=record
    HashValue : LongWord;
    StrIndex  : Integer;
    NextIndex : Integer;
    Data      : Pointer;
  end;
  PM_HashItem=^TM_HashItem;

const
{$ifdef CPU16}
  MaxHashListSize = maxsmallint div 16;
  MaxHashStrSize  = maxsmallint;
  MaxHashTableSize = maxsmallint div 4;
{$else CPU16}
  MaxHashListSize = Maxint div 16;
  MaxHashStrSize  = Maxint;
  MaxHashTableSize = Maxint div 4;
{$endif CPU16}
  MaxItemsPerHash = 3;

type
  PM_HashItemList = ^TM_HashItemList;
  TM_HashItemList = array[0..MaxHashListSize - 1] of TM_HashItem;
  PM_HashTable = ^TM_HashTable;
  TM_HashTable = array[0..MaxHashTableSize - 1] of Integer;

  TM_FPHashList = class(TObject)
  private
    { ItemList }
    FHashList     : PM_HashItemList;
    FCount,
    FCapacity : Integer;
    { Hash }
    FHashTable    : PM_HashTable;
    FHashCapacity : Integer;
    { Strings }
    FStrs     : PChar;
    FStrCount,
    FStrCapacity : Integer;
    Function InternalFind(AHash:LongWord;const AName:shortstring;out PrevIndex:Integer):Integer;
  protected
    Function Get(Index: Integer): Pointer; {$ifdef CCLASSESINLINE}inline;{$endif}
    Procedure Put(Index: Integer; Item: Pointer); {$ifdef CCLASSESINLINE}inline;{$endif}
    Procedure SetCapacity(NewCapacity: Integer);
    Procedure SetCount(NewCount: Integer);
    Procedure RaiseIndexError(Index : Integer);
    Function  AddStr(const s:shortstring): Integer;
    Procedure AddToHashTable(Index: Integer);
    Procedure StrExpand(MinIncSize:Integer);
    Procedure SetStrCapacity(NewCapacity: Integer);
    Procedure SetHashCapacity(NewCapacity: Integer);
    Procedure ReHash;
  public
    constructor Create;
    destructor Destroy; override;
    Function Add(const AName:shortstring;Item: Pointer): Integer;
    Procedure Clear;
    Function NameOfIndex(Index: Integer): ShortString; {$ifdef CCLASSESINLINE}inline;{$endif}
    Function HashOfIndex(Index: Integer): LongWord; {$ifdef CCLASSESINLINE}inline;{$endif}
    Function GetNextCollision(Index: Integer): Integer;
    Procedure Delete(Index: Integer);
    class Procedure Error(const Msg: string; Data: PtrInt);
    Function Expand: TM_FPHashList;
    Function Extract(item: Pointer): Pointer;
    Function IndexOf(Item: Pointer): Integer;
    Function Find(const AName:shortstring): Pointer;
    Function FindIndexOf(const AName:shortstring): Integer;
    Function FindWithHash(const AName:shortstring;AHash:LongWord): Pointer;
    Function Rename(const AOldName,ANewName:shortstring): Integer;
    Function Remove(Item: Pointer): Integer;
    Procedure Pack;
    Procedure ShowStatistics;
    Procedure ForEachCall(proc2call:TListCallback;arg:pointer);
    Procedure ForEachCall(proc2call:TListStaticCallback;arg:pointer);
    property Capacity: Integer read FCapacity write SetCapacity;
    property Count: Integer read FCount write SetCount;
    property Items[Index: Integer]: Pointer read Get write Put; default;
    property List: PM_HashItemList read FHashList;
    property Strs: PChar read FStrs;
  end;


{*******************************************************
        TFPHashObjectList (From fcl/inc/contnrs.pp)
********************************************************}

  TM_FPHashObjectList = class;

  { TM_FPHashObject }

  TM_FPHashObject = class
  private
    FOwner     : TM_FPHashObjectList;
    FCachedStr : pshortstring;
    FStrIndex  : Integer;
    Procedure InternalChangeOwner(HashObjectList:TM_FPHashObjectList;const s:shortstring);
  protected
    Function GetName:shortstring;virtual;
    Function GetHash:Longword;virtual;
  public
    constructor CreateNotOwned;
    constructor Create(HashObjectList:TM_FPHashObjectList;const s:shortstring);
    Procedure ChangeOwner(HashObjectList:TM_FPHashObjectList); {$ifdef CCLASSESINLINE}inline;{$endif}
    Procedure ChangeOwnerAndName(HashObjectList:TM_FPHashObjectList;const s:shortstring); {$ifdef CCLASSESINLINE}inline;{$endif}
    Procedure Rename(const ANewName:shortstring);
    property Name:shortstring read GetName;
    property Hash:Longword read GetHash;
  end;

  TM_FPHashObjectList = class(TObject)
  private
    FFreeObjects : Boolean;
    FHashList: TM_FPHashList;
    Function GetCount: integer; {$ifdef CCLASSESINLINE}inline;{$endif}
    Procedure SetCount(const AValue: integer); {$ifdef CCLASSESINLINE}inline;{$endif}
  protected
    Function GetItem(Index: Integer): TObject; {$ifdef CCLASSESINLINE}inline;{$endif}
    Procedure SetItem(Index: Integer; AObject: TObject); {$ifdef CCLASSESINLINE}inline;{$endif}
    Procedure SetCapacity(NewCapacity: Integer); {$ifdef CCLASSESINLINE}inline;{$endif}
    Function GetCapacity: integer; {$ifdef CCLASSESINLINE}inline;{$endif}
  public
    constructor Create(FreeObjects : boolean = True);
    destructor Destroy; override;
    Procedure Clear;
    Function Add(const AName:shortstring;AObject: TObject): Integer; {$ifdef CCLASSESINLINE}inline;{$endif}
    Function NameOfIndex(Index: Integer): ShortString; {$ifdef CCLASSESINLINE}inline;{$endif}
    Function HashOfIndex(Index: Integer): LongWord; {$ifdef CCLASSESINLINE}inline;{$endif}
    Function GetNextCollision(Index: Integer): Integer; {$ifdef CCLASSESINLINE}inline;{$endif}
    Procedure Delete(Index: Integer);
    Function Expand: TM_FPHashObjectList; {$ifdef CCLASSESINLINE}inline;{$endif}
    Function Extract(Item: TObject): TObject; {$ifdef CCLASSESINLINE}inline;{$endif}
    Function Remove(AObject: TObject): Integer;
    Function IndexOf(AObject: TObject): Integer; {$ifdef CCLASSESINLINE}inline;{$endif}
    Function Find(const s:shortstring): TObject; {$ifdef CCLASSESINLINE}inline;{$endif}
    Function FindIndexOf(const s:shortstring): Integer; {$ifdef CCLASSESINLINE}inline;{$endif}
    Function FindWithHash(const AName:shortstring;AHash:LongWord): Pointer;
    Function Rename(const AOldName,ANewName:shortstring): Integer; {$ifdef CCLASSESINLINE}inline;{$endif}
    Function FindInstanceOf(AClass: TClass; AExact: Boolean; AStartAt: Integer): Integer;
    Procedure Pack; {$ifdef CCLASSESINLINE}inline;{$endif}
    Procedure ShowStatistics; {$ifdef CCLASSESINLINE}inline;{$endif}
    Procedure ForEachCall(proc2call:TM_ObjectListCallback;arg:pointer); {$ifdef CCLASSESINLINE}inline;{$endif}
    Procedure ForEachCall(proc2call:TM_ObjectListStaticCallback;arg:pointer); {$ifdef CCLASSESINLINE}inline;{$endif}
    property Capacity: Integer read GetCapacity write SetCapacity;
    property Count: Integer read GetCount write SetCount;
    property OwnsObjects: Boolean read FFreeObjects write FFreeObjects;
    property Items[Index: Integer]: TObject read GetItem write SetItem; default;
    property List: TM_FPHashList read FHashList;
  end;

{ ---------------------------------------------------------------------
    Hash support, implemented by Dean Zobec
  ---------------------------------------------------------------------}


  { Must return a Longword value in the range 0..TableSize,
   usually via a mod operator;  }
  TM_HashFunction = Function(const S: string; const TableSize: Longword): Longword;


  { THTNode }

  TM_HTCustomNode = class(TObject)
  private
    FKey: string;
  public
    constructor CreateWith(const AString: String);
    Function HasKey(const AKey: string): boolean;
    property Key: string read FKey;
  end;
  TM_HTCustomNodeClass = Class of TM_HTCustomNode;


  { TM_FPCustomHashTable }

  TM_FPCustomHashTable = class(TObject)
  private
    FHashTable: TM_FPObjectList;
    FHashFunction: TM_HashFunction;
    FCount: Longword;
    Function GetDensity: Longword;
    Function GetNumberOfCollisions: Longword;
    Procedure SetHashTableSize(const Value: Longword);
    Procedure InitializeHashTable;
    Function GetVoidSlots: Longword;
    Function GetLoadFactor: double;
    Function GetAVGChainLen: double;
    Function GetMaxChainLength: Longword;
  protected
    FHashTableSize: Longword;
    Function Chain(const index: Longword):TM_FPObjectList;
    Function CreateNewNode(const aKey : string) : TM_HTCustomNode; virtual; abstract;
    Procedure AddNode(ANode : TM_HTCustomNode); virtual; abstract;
    Function ChainLength(const ChainIndex: Longword): Longword; virtual;
    Function FindOrCreateNew(const aKey: string): TM_HTCustomNode; virtual;
    Procedure SetHashFunction(AHashFunction: TM_HashFunction); virtual;
    Function FindChainForAdd(Const aKey : String) : TM_FPObjectList;
  public
    constructor Create;
    constructor CreateWith(AHashTableSize: Longword; aHashFunc: TM_HashFunction);
    destructor Destroy; override;
    Procedure ChangeTableSize(const ANewSize: Longword); virtual;
    Procedure Clear; virtual;
    Procedure Delete(const aKey: string); virtual;
    Function Find(const aKey: string): TM_HTCustomNode;
    Function IsEmpty: boolean;
    property HashFunction: TM_HashFunction read FHashFunction write SetHashFunction;
    property Count: Longword read FCount;
    property HashTableSize: Longword read FHashTableSize write SetHashTableSize;
    property HashTable: TM_FPObjectList read FHashTable;
    property VoidSlots: Longword read GetVoidSlots;
    property LoadFactor: double read GetLoadFactor;
    property AVGChainLen: double read GetAVGChainLen;
    property MaxChainLength: Longword read GetMaxChainLength;
    property NumberOfCollisions: Longword read GetNumberOfCollisions;
    property Density: Longword read GetDensity;
  end;

  { TM_FPDataHashTable : Hash table with simple data pointers }

  TM_HTDataNode = Class(TM_HTCustomNode)
  Private
    FData: pointer;
  public
    property Data: pointer read FData write FData;
  end;
  // For compatibility
  THTNode = TM_HTDataNode;

  TM_DataIteratorMethod = Procedure(Item: Pointer; const Key: string; var Continue: Boolean) of object;
  TM_DataIteratorCallBack = Procedure(Item: Pointer; const Key: string; var Continue: Boolean);

  // For compatibility
  TIteratorMethod = TM_DataIteratorMethod;

  TM_FPDataHashTable = Class(TM_FPCustomHashTable)
  Private
    FIteratorCallBack: TM_DataIteratorCallBack;
    Procedure CallbackIterator(Item: Pointer; const Key: string; var Continue: Boolean);
  Protected
    Function CreateNewNode(const aKey : String) : TM_HTCustomNode; override;
    Procedure AddNode(ANode : TM_HTCustomNode); override;
    Procedure SetData(const index: string; const AValue: Pointer); virtual;
    Function GetData(const index: string):Pointer; virtual;
    Function ForEachCall(aMethod: TM_DataIteratorMethod): TM_HTDataNode; virtual;
  Public
    Function Iterate(aMethod: TM_DataIteratorMethod): Pointer; virtual;
    Function Iterate(aMethod: TM_DataIteratorCallBack): Pointer; virtual;
    Procedure Add(const aKey: string; AItem: pointer); virtual;
    property Items[const index: string]: Pointer read GetData write SetData; default;
  end;

  { TM_TFPStringHashTable : Hash table with simple strings as data }
  TM_HTStringNode = Class(TM_HTCustomNode)
  Private
    FData : String;
  public
    property Data: String read FData write FData;
  end;
  
  TM_StringIteratorMethod = Procedure(Item: String; const Key: string; var Continue: Boolean) of object;
  TM_StringIteratorCallback = Procedure(Item: String; const Key: string; var Continue: Boolean);

  TM_TFPStringHashTable = Class(TM_FPCustomHashTable)
  Private
    FIteratorCallBack: TM_StringIteratorCallback;
    Procedure CallbackIterator(Item: String; const Key: string; var Continue: Boolean);
  Protected
    Function CreateNewNode(const aKey : String) : TM_HTCustomNode; override;
    Procedure AddNode(ANode : TM_HTCustomNode); override;
    Procedure SetData(const Index, AValue: string); virtual;
    Function GetData(const index: string): String; virtual;
    Function ForEachCall(aMethod: TM_StringIteratorMethod): TM_HTStringNode; virtual;
  Public
    Function Iterate(aMethod: TM_StringIteratorMethod): String; virtual;
    Function Iterate(aMethod: TM_StringIteratorCallback): String; virtual;
    Procedure Add(const aKey,aItem: string); virtual;
    property Items[const index: string]: String read GetData write SetData; default;
  end;

  { TFPStringHashTable : Hash table with simple strings as data }


  TM_HTObjectNode = Class(TM_HTCustomNode)
  Private
    FData : TObject;
  public
    property Data: TObject read FData write FData;
  end;

  TM_HTOwnedObjectNode = Class(TM_HTObjectNode)
  public
    destructor Destroy; override;
  end;

  TM_ObjectIteratorMethod = Procedure(Item: TObject; const Key: string; var Continue: Boolean) of object;
  TM_ObjectIteratorCallback = Procedure(Item: TObject; const Key: string; var Continue: Boolean);

  TM_FPObjectHashTable = Class(TM_FPCustomHashTable)
  Private
    FOwnsObjects : Boolean;
    FIteratorCallBack: TM_ObjectIteratorCallback;
    procedure CallbackIterator(Item: TObject; const Key: string; var Continue: Boolean);
  Protected
    Function CreateNewNode(const aKey : String) : TM_HTCustomNode; override;
    Procedure AddNode(ANode : TM_HTCustomNode); override;
    Procedure SetData(const Index: string; AObject : TObject); virtual;
    Function GetData(const index: string): TObject; virtual;
    Function ForEachCall(aMethod: TM_ObjectIteratorMethod): TM_HTObjectNode; virtual;
  Public
    constructor Create(AOwnsObjects : Boolean = True);
    constructor CreateWith(AHashTableSize: Longword; aHashFunc: TM_HashFunction; AOwnsObjects : Boolean = True);
    Function Iterate(aMethod: TM_ObjectIteratorMethod): TObject; virtual;
    Function Iterate(aMethod: TM_ObjectIteratorCallback): TObject; virtual;
    Procedure Add(const aKey: string; AItem : TObject); virtual;
    property Items[const index: string]: TObject read GetData write SetData; default;
    Property OwnsObjects : Boolean Read FOwnsObjects;
  end;

  EDuplicate = class(Exception);
  EKeyNotFound = class(Exception);

  Function RSHash(const S: string; const TableSize: Longword): Longword;

{ ---------------------------------------------------------------------
    Bucket lists as in Delphi
  ---------------------------------------------------------------------}


Type
  TM_BucketItem = record
    Item, Data: Pointer;
  end;
  TM_BucketItemArray = array of TM_BucketItem;

  TM_Bucket = record
    Count : Integer;
    Items : TM_BucketItemArray;
  end;
  PM_Bucket = ^TM_Bucket;
  TM_BucketArray = array of TM_Bucket;

  TM_BucketProc = Procedure(AInfo, AItem, AData: Pointer; out AContinue: Boolean);
  TM_BucketProcObject = Procedure(AItem, AData: Pointer; out AContinue: Boolean) of Object;

{ ---------------------------------------------------------------------
  TCustomBucketList
  ---------------------------------------------------------------------}

  { TM_CustomBucketList }

  TM_CustomBucketList = class(TObject)
  private
    FBuckets: TM_BucketArray;
    Function GetBucketCount: Integer;
    Function GetData(AItem: Pointer): Pointer;
    Procedure SetData(AItem: Pointer; const AData: Pointer);
    Procedure SetBucketCount(const Value: Integer);
  protected
    Procedure GetBucketItem(AItem: Pointer; out ABucket, AIndex: Integer);
    Function AddItem(ABucket: Integer; AItem, AData: Pointer): Pointer; virtual;
    Function BucketFor(AItem: Pointer): Integer; virtual; abstract;
    Function DeleteItem(ABucket: Integer; AIndex: Integer): Pointer; virtual;
    Procedure Error(Msg : String; Args : Array of Const);
    Function FindItem(AItem: Pointer; out ABucket, AIndex: Integer): Boolean; virtual;
    property Buckets: TM_BucketArray read FBuckets;
    property BucketCount: Integer read GetBucketCount write SetBucketCount;
  public
    destructor Destroy; override;
    Procedure Clear;
    Function Add(AItem, AData: Pointer): Pointer;
    Procedure Assign(AList: TM_CustomBucketList);
    Function Exists(AItem: Pointer): Boolean;
    Function Find(AItem: Pointer; out AData: Pointer): Boolean;
    Function ForEach(AProc: TM_BucketProc; AInfo: Pointer = nil): Boolean;
    Function ForEach(AProc: TM_BucketProcObject): Boolean;
    Function Remove(AItem: Pointer): Pointer;
    property Data[AItem: Pointer]: Pointer read GetData write SetData; default;
  end;

{ ---------------------------------------------------------------------
  TBucketList
  ---------------------------------------------------------------------}


  TM_BucketListSizes = (bl2, bl4, bl8, bl16, bl32, bl64, bl128, bl256);

  { TM_BucketList }

  TM_BucketList = class(TM_CustomBucketList)
  private
    FBucketMask: Byte;
  protected
    Function BucketFor(AItem: Pointer): Integer; override;
  public
    constructor Create(ABuckets: TM_BucketListSizes = bl16);
  end;

{ ---------------------------------------------------------------------
  TObjectBucketList
  ---------------------------------------------------------------------}

  { TM_ObjectBucketList }

  TM_ObjectBucketList = class(TM_BucketList)
  protected
    Function GetData(AItem: TObject): TObject;
    Procedure SetData(AItem: TObject; const AData: TObject);
  public
    Function Add(AItem, AData: TObject): TObject;
    Function Remove(AItem: TObject): TObject;
    property Data[AItem: TObject]: TObject read GetData write SetData; default;
  end;


implementation

uses
  RtlConsts;

ResourceString
  DuplicateMsg   = 'An item with key %0:s already exists';
  {%H-}KeyNotFoundMsg = 'Method: %0:s key [''%1:s''] not found in container';
  NotEmptyMsg    = 'Hash table not empty.';
  SErrNoSuchItem = 'No item in list for %p';
  SDuplicateItem = 'Item already exists in list: %p';

const
  NPRIMES = 28;

  PRIMELIST: array[0 .. NPRIMES-1] of Longword =
  ( 53,         97,         193,       389,       769,
    1543,       3079,       6151,      12289,     24593,
    49157,      98317,      196613,    393241,    786433,
    1572869,    3145739,    6291469,   12582917,  25165843,
    50331653,   100663319,  201326611, 402653189, 805306457,
    1610612741, 3221225473, 4294967291 );

constructor TM_FPObjectList.Create(FreeObjects : boolean);
begin
  Create;
  FFreeObjects:=Freeobjects;
end;

destructor TM_FPObjectList.Destroy;
begin
  if (FList <> nil) then
  begin
    Clear;
    FList.Destroy;
  end;
  inherited Destroy;
end;

Procedure TM_FPObjectList.Clear;
var
  i: integer;
begin
  if FFreeObjects then
    for i:=FList.Count-1 downto 0  do
      TObject(FList[i]).Free;
  FList.Clear;
end;

constructor TM_FPObjectList.Create;
begin
  inherited Create;
  FList:=TM_FPList.Create;
  FFreeObjects:=True;
end;

Function TM_FPObjectList.GetCount: integer;
begin
  Result:=FList.Count;
end;

Procedure TM_FPObjectList.SetCount(const AValue: integer);
begin
  if FList.Count <> AValue then
    FList.Count:=AValue;
end;

Function TM_FPObjectList.GetItem(Index: Integer): TObject; {$ifdef CLASSESINLINE}inline;{$endif}
begin
  Result:=TObject(FList[Index]);
end;

Procedure TM_FPObjectList.SetItem(Index: Integer; AObject: TObject); {$ifdef CLASSESINLINE}inline;{$endif}
begin
  if OwnsObjects then
    TObject(FList[Index]).Free;
  FList[index]:=AObject;
end;

Procedure TM_FPObjectList.SetCapacity(NewCapacity: Integer);
begin
  FList.Capacity:=NewCapacity;
end;

Function TM_FPObjectList.GetCapacity: integer;
begin
  Result:=FList.Capacity;
end;

Function TM_FPObjectList.Add(AObject: TObject): Integer; {$ifdef CLASSESINLINE}inline;{$endif}
begin
  Result:=FList.Add(AObject);
end;

Procedure TM_FPObjectList.Delete(Index: Integer); {$ifdef CLASSESINLINE}inline;{$endif}
begin
  if OwnsObjects then
    TObject(FList[Index]).Free;
  FList.Delete(Index);
end;

Procedure TM_FPObjectList.Exchange(Index1, Index2: Integer);
begin
  FList.Exchange(Index1, Index2);
end;

Function TM_FPObjectList.Expand: TM_FPObjectList;
begin
  FList.Expand;
  Result:=Self;
end;

Function TM_FPObjectList.Extract(Item: TObject): TObject;
begin
  Result:=TObject(FList.Extract(Item));
end;

Function TM_FPObjectList.Remove(AObject: TObject): Integer;
begin
  Result:=IndexOf(AObject);
  if (Result <> -1) then
    begin
    if OwnsObjects then
      TObject(FList[Result]).Free;
    FList.Delete(Result);
    end;
end;

Function TM_FPObjectList.IndexOf(AObject: TObject): Integer;
begin
  Result:=FList.IndexOf(Pointer(AObject));
end;

Function TM_FPObjectList.FindInstanceOf(AClass: TClass; AExact: Boolean; AStartAt : Integer): Integer;
var
  I : Integer;
begin
  I:=AStartAt;
  Result:=-1;
  if AExact then
    while (I<Count) and (Result=-1) do
      if Items[i].ClassType=AClass then
        Result:=I
      else
        Inc(I)
  else
    while (I<Count) and (Result=-1) do
      if Items[i].InheritsFrom(AClass) then
        Result:=I
      else
        Inc(I);
end;

Procedure TM_FPObjectList.Insert(Index: Integer; AObject: TObject); {$ifdef CLASSESINLINE}inline;{$endif}
begin
  FList.Insert(Index, Pointer(AObject));
end;

Procedure TM_FPObjectList.Move(CurIndex, NewIndex: Integer);
begin
  FList.Move(CurIndex, NewIndex);
end;

Procedure TM_FPObjectList.Assign(Obj: TM_FPObjectList);
var
  i: Integer;
begin
  Clear;
  for i:=0 to Obj.Count - 1 do
    Add(Obj[i]);
end;

Procedure TM_FPObjectList.Pack;
begin
  FList.Pack;
end;

Procedure TM_FPObjectList.Sort(Compare: TListSortCompare);
begin
  FList.Sort(Compare);
end;

Function TM_FPObjectList.First: TObject;
begin
  Result:=TObject(FList.First);
end;

Function TM_FPObjectList.Last: TObject;
begin
  Result:=TObject(FList.Last);
end;

Procedure TM_FPObjectList.ForEachCall(proc2call:TM_ObjectListCallback;arg:pointer);
begin
  FList.ForEachCall(TListCallBack(proc2call),arg);
end;

Procedure TM_FPObjectList.ForEachCall(proc2call:TM_ObjectListStaticCallback;arg:pointer);
begin
  FList.ForEachCall(TListStaticCallBack(proc2call),arg);
end;


{ TM_ObjectList }

constructor TM_ObjectList.Create(FreeObjects: boolean);
begin
  inherited Create;
  FFreeObjects:=FreeObjects;
end;

constructor TM_ObjectList.Create;
begin
  inherited Create;
  FFreeObjects:=True;
end;

Procedure TM_ObjectList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  if FFreeObjects then
    if (Action=lnDeleted) then
      TObject(Ptr).Free;
  inherited Notify(Ptr,Action);
end;


Function TM_ObjectList.GetItem(Index: Integer): TObject;
begin
  Result:=TObject(inherited Get(Index));
end;


Procedure TM_ObjectList.SetItem(Index: Integer; AObject: TObject);
begin
  // Put will take care of deleting old one in Notify.
  Put(Index,Pointer(AObject));
end;


Function TM_ObjectList.Add(AObject: TObject): Integer;
begin
  Result:=inherited Add(Pointer(AObject));
end;


Function TM_ObjectList.Extract(Item: TObject): TObject;
begin
  Result:=TObject(inherited Extract(Pointer(Item)));
end;


Function TM_ObjectList.Remove(AObject: TObject): Integer;
begin
  Result:=inherited Remove(Pointer(AObject));
end;


Function TM_ObjectList.IndexOf(AObject: TObject): Integer;
begin
  Result:=inherited IndexOf(Pointer(AObject));
end;


Function TM_ObjectList.FindInstanceOf(AClass: TClass; AExact: Boolean;
  AStartAt: Integer): Integer;
var
  I : Integer;
begin
  I:=AStartAt;
  Result:=-1;
  if AExact then
    while (I<Count) and (Result=-1) do
      if Items[i].ClassType=AClass then
        Result:=I
      else
        Inc(I)
  else
    while (I<Count) and (Result=-1) do
      if Items[i].InheritsFrom(AClass) then
        Result:=I
      else
        Inc(I);
end;


Procedure TM_ObjectList.Insert(Index: Integer; AObject: TObject);
begin
  Inherited Insert(Index,Pointer(AObject));
end;


Function TM_ObjectList.First: TObject;
begin
  Result:=TObject(inherited First);
end;


Function TM_ObjectList.Last: TObject;
begin
  Result:=TObject(inherited Last);
end;

{ TListComponent }

//type
//  TlistComponent = class(TComponent)
//  private
//    Flist : TComponentList;
//  public
//    Procedure Notification(AComponent: TComponent; Operation: TOperation); override;
//  end;
//
//Procedure TlistComponent.Notification(AComponent: TComponent;
//  Operation: TOperation);
//begin
//  if (Operation=opRemove) then
//    Flist.HandleFreeNotify(Self,AComponent);
//  inherited;
//end;
//
//{ TComponentList }
//
//Function TComponentList.Add(AComponent: TComponent): Integer;
//begin
//  Result:=inherited Add(AComponent);
//end;
//
//destructor TComponentList.Destroy;
//begin
//  inherited;
//  FreeAndNil(FNotifier);
//end;
//
//Function TComponentList.Extract(Item: TComponent): TComponent;
//begin
//  Result:=TComponent(inherited Extract(Item));
//end;
//
//Function TComponentList.First: TComponent;
//begin
//  Result:=TComponent(inherited First);
//end;
//
//Function TComponentList.GetItems(Index: Integer): TComponent;
//begin
//  Result:=TComponent(inherited Items[Index]);
//end;
//
//Procedure TComponentList.HandleFreeNotify(Sender: TObject;
//  AComponent: TComponent);
//begin
//  Extract(AComponent);
//end;
//
//Function TComponentList.IndexOf(AComponent: TComponent): Integer;
//begin
//  Result:=inherited IndexOf(AComponent);
//end;
//
//Procedure TComponentList.Insert(Index: Integer; AComponent: TComponent);
//begin
//  inherited Insert(Index,AComponent)
//end;
//
//Function TComponentList.Last: TComponent;
//begin
//  Result:=TComponent(inherited Last);
//end;
//
//Procedure TComponentList.Notify(Ptr: Pointer; Action: TListNotification);
//begin
//  if FNotifier=nil then
//    begin
//    FNotifier:=TlistComponent.Create(nil);
//    TlistComponent(FNotifier).FList:=Self;
//    end;
//  if Assigned(Ptr) then
//    with TComponent(Ptr) do
//      case Action of
//        lnAdded : FreeNotification(FNotifier);
//        lnExtracted, lnDeleted: RemoveFreeNotification(FNotifier);
//      end;
//  inherited Notify(Ptr, Action);
//end;
//
//Function TComponentList.Remove(AComponent: TComponent): Integer;
//begin
//  Result:=inherited Remove(AComponent);
//end;
//
//Procedure TComponentList.SetItems(Index: Integer; AComponent: TComponent);
//begin
//  Put(Index,AComponent);
//end;

{ TM_ClassList }

Function TM_ClassList.Add(AClass: TClass): Integer;
begin
  Result:=inherited Add(Pointer(AClass));
end;

Function TM_ClassList.Extract(Item: TClass): TClass;
begin
  Result:=TClass(inherited Extract(Pointer(Item)));
end;

Function TM_ClassList.First: TClass;
begin
  Result:=TClass(inherited First);
end;

Function TM_ClassList.GetItems(Index: Integer): TClass;
begin
  Result:=TClass(inherited Items[Index]);
end;

Function TM_ClassList.IndexOf(AClass: TClass): Integer;
begin
  Result:=inherited IndexOf(Pointer(AClass));
end;

Procedure TM_ClassList.Insert(Index: Integer; AClass: TClass);
begin
  inherited Insert(Index,Pointer(AClass));
end;

Function TM_ClassList.Last: TClass;
begin
  Result:=TClass(inherited Last);
end;

Function TM_ClassList.Remove(AClass: TClass): Integer;
begin
  Result:=inherited Remove(Pointer(AClass));
end;

Procedure TM_ClassList.SetItems(Index: Integer; AClass: TClass);
begin
  Put(Index,Pointer(AClass));
end;

{ TM_OrderedList }

Function TM_OrderedList.AtLeast(ACount: Integer): Boolean;
begin
  Result:=(FList.Count>=Acount)
end;

Function TM_OrderedList.Count: Integer;
begin
  Result:=FList.Count;
end;

constructor TM_OrderedList.Create;
begin
  FList:=TM_list.Create;
end;

destructor TM_OrderedList.Destroy;
begin
  FList.Free;
end;

Function TM_OrderedList.Peek: Pointer;
begin
  if AtLeast(1) then
    Result:=PeekItem
  else
    Result:=nil;
end;

Function TM_OrderedList.PeekItem: Pointer;
begin
  with Flist do
    Result:=Items[Count-1]
end;

Function TM_OrderedList.Pop: Pointer;
begin
  If Atleast(1) then
    Result:=PopItem
  else
    Result:=nil;
end;

Function TM_OrderedList.PopItem: Pointer;
begin
  with FList do
    if Count>0 then
      begin
      Result:=Items[Count-1];
      Delete(Count-1);
      end
    else
      Result:=nil;
end;

Function TM_OrderedList.Push(AItem: Pointer): Pointer;
begin
  PushItem(AItem);
  Result:=AItem;
end;

{ TM_Stack }

Procedure TM_Stack.PushItem(AItem: Pointer);
begin
  FList.Add(AItem);
end;

{ TM_ObjectStack }

Function TM_ObjectStack.Peek: TObject;
begin
  Result:=TObject(inherited Peek);
end;

Function TM_ObjectStack.Pop: TObject;
begin
  Result:=TObject(Inherited Pop);
end;

Function TM_ObjectStack.Push(AObject: TObject): TObject;
begin
  Result:=TObject(inherited Push(Pointer(AObject)));
end;

{ TM_Queue }

Procedure TM_Queue.PushItem(AItem: Pointer);
begin
  with FList Do
    Insert(0,AItem);
end;

{ TM_ObjectQueue }

Function TM_ObjectQueue.Peek: TObject;
begin
  Result:=TObject(inherited Peek);
end;

Function TM_ObjectQueue.Pop: TObject;
begin
  Result:=TObject(inherited Pop);
end;

Function TM_ObjectQueue.Push(AObject: TObject): TObject;
begin
  Result:=TObject(inherited Push(Pointer(AObject)));
end;


{*****************************************************************************
                            TM_FPHashList
*****************************************************************************}

    Function FPHash(const s:shortstring):LongWord;
    var
      p,pmax : PChar;
    begin
{$push}
{$Q-}
      Result:=0;
      p:=@s[1];
      pmax:=@s[length(s)+1];
      while (p<pmax) do
        begin
          Result:=LongWord(LongInt(Result shl 5) - LongInt(Result)) xor LongWord(P^);
          Inc(p);
        end;
{$pop}
    end;

    Function FPHash(P: PChar; Len: Integer): LongWord;
    var
      pmax : PChar;
    begin
{$push}
{$Q-}
      Result:=0;
      pmax:=p+len;
      while (p<pmax) do
        begin
          Result:=LongWord(LongInt(Result shl 5) - LongInt(Result)) xor LongWord(P^);
          Inc(p);
        end;
{$pop}
    end;


Procedure TM_FPHashList.RaiseIndexError(Index : Integer);
begin
  Error(SListIndexError, Index);
end;


Function TM_FPHashList.Get(Index: Integer): Pointer;
begin
  If (Index < 0) or (Index >= FCount) then
    RaiseIndexError(Index);
  Result:=FHashList^[Index].Data;
end;


Procedure TM_FPHashList.Put(Index: Integer; Item: Pointer);
begin
  if (Index < 0) or (Index >= FCount) then
    RaiseIndexError(Index);
  FHashList^[Index].Data:=Item;
end;


Function TM_FPHashList.NameOfIndex(Index: Integer): shortstring;
begin
  if (Index < 0) or (Index >= FCount) then
    RaiseIndexError(Index);
  with FHashList^[Index] do
    begin
    if StrIndex>=0 then
      Result:=PShortString(@FStrs[StrIndex])^
    else
      Result:='';
    end;
end;


Function TM_FPHashList.HashOfIndex(Index: Integer): LongWord;
begin
  If (Index < 0) or (Index >= FCount) then
    RaiseIndexError(Index);
  Result:=FHashList^[Index].HashValue;
end;


Function TM_FPHashList.GetNextCollision(Index: Integer): Integer;
begin
  Result:=-1;
  if ((Index > -1) and (Index < FCount)) then
    Result:=FHashList^[Index].NextIndex;
end;


Function TM_FPHashList.Extract(item: Pointer): Pointer;
var
  i : Integer;
begin
  Result:=nil;
  i:=IndexOf(item);
  if i >= 0 then
    begin
    Result:=item;
    Delete(i);
    end;
end;


Procedure TM_FPHashList.SetCapacity(NewCapacity: Integer);
begin
  if (NewCapacity < FCount) or (NewCapacity > MaxHashListSize) then
     Error (SListCapacityError, NewCapacity);
  if NewCapacity = FCapacity then
    Exit;
  ReallocMem(FHashList, NewCapacity*SizeOf(TM_HashItem));
  FCapacity:=NewCapacity;
  { Maybe expand hash also }
  if FCapacity>FHashCapacity*MaxItemsPerHash then
    SetHashCapacity(FCapacity div MaxItemsPerHash);
end;


Procedure TM_FPHashList.SetCount(NewCount: Integer);
begin
  if (NewCount < 0) or (NewCount > MaxHashListSize)then
    Error(SListCountError, NewCount);
  if NewCount > FCount then
    begin
    if NewCount > FCapacity then
      SetCapacity(NewCount);
    if FCount < NewCount then
      FillChar(FHashList^[FCount], (NewCount-FCount) div SizeOf(TM_HashItem), 0);
    end;
  FCount:=NewCount;
end;


Procedure TM_FPHashList.SetStrCapacity(NewCapacity: Integer);
begin
  if (NewCapacity < FStrCount) or (NewCapacity {%H-}> MaxHashStrSize) then
    Error(SListCapacityError, NewCapacity);
  if NewCapacity = FStrCapacity then
    Exit;
  ReallocMem(FStrs, NewCapacity);
  FStrCapacity:=NewCapacity;
end;


Procedure TM_FPHashList.SetHashCapacity(NewCapacity: Integer);
begin
  if (NewCapacity < 1) then
    Error(SListCapacityError, NewCapacity);
  if FHashCapacity=NewCapacity then
    Exit;
  FHashCapacity:=NewCapacity;
  ReallocMem(FHashTable, FHashCapacity*SizeOf(Integer));
  ReHash;
end;


Procedure TM_FPHashList.ReHash;
var
  i : Integer;
begin
  FillDword(FHashTable^,FHashCapacity,LongWord(-1));
  for i:=0 to FCount-1 do
    AddToHashTable(i);
end;


constructor TM_FPHashList.Create;
begin
  SetHashCapacity(1);
end;


destructor TM_FPHashList.Destroy;
begin
  Clear;
  if Assigned(FHashTable) then
    FreeMem(FHashTable);
  inherited Destroy;
end;


Function TM_FPHashList.AddStr(const s:shortstring): Integer;
var
  Len : Integer;
begin
  len:=Length(s)+1;
  if FStrCount+Len >= FStrCapacity then
    StrExpand(Len);
  System.Move(s[0],FStrs[FStrCount],Len);
  Result:=FStrCount;
  Inc(FStrCount,Len);
end;


Procedure TM_FPHashList.AddToHashTable(Index: Integer);
var
  HashIndex : Integer;
begin
  with FHashList^[Index] do
    begin
    if not Assigned(Data) then
      Exit;
    HashIndex:=HashValue mod LongWord(FHashCapacity);
    NextIndex:=FHashTable^[HashIndex];
    FHashTable^[HashIndex]:=Index;
    end;
end;


Function TM_FPHashList.Add(const AName:shortstring;Item: Pointer): Integer;
begin
  if FCount = FCapacity then
    Expand;
  with FHashList^[FCount] do
    begin
    HashValue:=FPHash(AName);
    Data:=Item;
    StrIndex:=AddStr(AName);
    end;
  AddToHashTable(FCount);
  Result:=FCount;
  Inc(FCount);
end;

Procedure TM_FPHashList.Clear;
begin
  if Assigned(FHashList) then
    begin
    FCount:=0;
    SetCapacity(0);
    FHashList:=nil;
    end;
  SetHashCapacity(1);
  FHashTable^[0]:=(-1); // sethashcapacity does not always call rehash
  if Assigned(FStrs) then
    begin
    FStrCount:=0;
    SetStrCapacity(0);
    FStrs:=nil;
    end;
end;

Procedure TM_FPHashList.Delete(Index: Integer);
begin
  if (Index<0) or (Index>=FCount) then
    Error(SListIndexError, Index);
  { Remove from HashList }
  Dec(FCount);
  System.Move(FHashList^[Index+1], FHashList^[Index], (FCount - Index) * SizeOf(TM_HashItem));
  { All indexes are updated, we need to build the hashtable again }
  ReHash;
  { Shrink the list if appropriate }
  if (FCapacity > 256) and (FCount < FCapacity shr 2) then
    begin
    FCapacity:=FCapacity shr 1;
    ReAllocMem(FHashList, SizeOf(TM_HashItem) * FCapacity);
    end;
end;

Function TM_FPHashList.Remove(Item: Pointer): Integer;
begin
  Result:=IndexOf(Item);
  If Result <> -1 then
    Self.Delete(Result);
end;

class Procedure TM_FPHashList.Error(const Msg: string; Data: PtrInt);
begin
  raise EListError.CreateFmt(Msg,[Data]) at get_caller_addr(get_frame), get_caller_frame(get_frame);
end;

Function TM_FPHashList.Expand: TM_FPHashList;
var
  IncSize : Longint;
begin
  Result:=Self;
  if FCount < FCapacity then
    Exit;
  IncSize:=SizeOf(PtrInt)*2;
  if FCapacity > 127 then
    Inc(IncSize, FCapacity shr 2)
  else if FCapacity > SizeOf(PtrInt)*3 then
    Inc(IncSize, FCapacity shr 1)
  else if FCapacity >= SizeOf(PtrInt) then
    Inc(IncSize,sizeof(PtrInt));
  SetCapacity(FCapacity + IncSize);
end;

Procedure TM_FPHashList.StrExpand(MinIncSize:Integer);
var
  IncSize : Longint;
begin
  if FStrCount+MinIncSize < FStrCapacity then
    Exit;
  IncSize:=64;
  if FStrCapacity > 255 then
    Inc(IncSize, FStrCapacity shr 2);
  SetStrCapacity(FStrCapacity + IncSize + MinIncSize);
end;

Function TM_FPHashList.IndexOf(Item: Pointer): Integer;
var
  psrc  : PM_HashItem;
  Index : integer;
begin
  Result:=-1;
  psrc:=@FHashList^[0];
  for Index:=0 to FCount-1 do
    begin
    if psrc^.Data=Item then
      begin
      Result:=Index;
      Exit;
      end;
    Inc(psrc);
    end;
end;

Function TM_FPHashList.InternalFind(AHash:LongWord;const AName:shortstring;out PrevIndex:Integer):Integer;
var
  HashIndex : Integer;
  Len,
  LastChar  : Char;
begin
  HashIndex:=AHash mod LongWord(FHashCapacity);
  Result:=FHashTable^[HashIndex];
  Len:=Char(Length(AName));
  LastChar:=AName[Byte(Len)];
  PrevIndex:=-1;
  while Result<>-1 do
    with FHashList^[Result] do
      begin
      if Assigned(Data) and
         (HashValue=AHash) and
         (Len=FStrs[StrIndex]) and
         (LastChar=FStrs[StrIndex+Byte(Len)]) and
         (AName=PShortString(@FStrs[StrIndex])^) then
        Exit;
      PrevIndex:=Result;
      Result:=NextIndex;
      end;
end;


Function TM_FPHashList.Find(const AName:shortstring): Pointer;
var
  Index,
  PrevIndex : Integer;
begin
  Result:=nil;
  Index:=InternalFind(FPHash(AName),AName,PrevIndex);
  if Index=-1 then
    Exit;
  Result:=FHashList^[Index].Data;
end;


Function TM_FPHashList.FindIndexOf(const AName:shortstring): Integer;
var
  PrevIndex : Integer;
begin
  Result:=InternalFind(FPHash(AName),AName,PrevIndex);
end;


Function TM_FPHashList.FindWithHash(const AName:shortstring;AHash:LongWord): Pointer;
var
  Index,
  PrevIndex : Integer;
begin
  Result:=nil;
  Index:=InternalFind(AHash,AName,PrevIndex);
  if Index=-1 then
    Exit;
  Result:=FHashList^[Index].Data;
end;


Function TM_FPHashList.Rename(const AOldName,ANewName:shortstring): Integer;
var
  PrevIndex,
  Index : Integer;
  OldHash : LongWord;
begin
  Result:=-1;
  OldHash:=FPHash(AOldName);
  Index:=InternalFind(OldHash,AOldName,PrevIndex);
  if Index=-1 then
    Exit;
  { Remove from current Hash }
  if PrevIndex<>-1 then
    FHashList^[PrevIndex].NextIndex:=FHashList^[Index].NextIndex
  else
    FHashTable^[OldHash mod LongWord(FHashCapacity)]:=FHashList^[Index].NextIndex;
  { Set new name and hash }
  with FHashList^[Index] do
    begin
    HashValue:=FPHash(ANewName);
    StrIndex:=AddStr(ANewName);
    end;
  { Insert back in Hash }
  AddToHashTable(Index);
  { Return Index }
  Result:=Index;
end;

Procedure TM_FPHashList.Pack;
var
  NewCount,
  i : integer;
  pdest,
  psrc : PM_HashItem;
  FOldStr : Pchar;
begin
  NewCount:=0;
  psrc:=@FHashList^[0];
  FOldStr:=FStrs;
  try
    FStrs:=nil;
    FStrCount:=0;
    FStrCapacity:=0;
    pdest:=psrc;
    for I:=0 to FCount-1 do
      begin
      if Assigned(psrc^.Data) then
        begin
        pdest^:=psrc^;
        pdest^.StrIndex:=AddStr(PShortString(@FOldStr[PDest^.StrIndex])^);
        Inc(pdest);
        Inc(NewCount);
        end;
      Inc(psrc);
      end;
  finally
    FreeMem(FoldStr);
  end;
  FCount:=NewCount;
  { We need to ReHash to update the IndexNext }
  ReHash;
  { Release over-capacity }
  SetCapacity(FCount);
  SetStrCapacity(FStrCount);
end;


Procedure TM_FPHashList.ShowStatistics;
var
  HashMean,
  HashStdDev : Double;
  Index,
  i,j : Integer;
begin
  { Calculate Mean and StdDev }
  HashMean:=0;
  HashStdDev:=0;
  for i:=0 to FHashCapacity-1 do
    begin
    j:=0;
    Index:=FHashTable^[i];
    while (Index<>-1) do
      begin
      Inc(j);
      Index:=FHashList^[Index].NextIndex;
      end;
    HashMean:=HashMean+j;
    HashStdDev:=HashStdDev+Sqr(j);
    end;
  HashMean:=HashMean/FHashCapacity;
  HashStdDev:=(HashStdDev-FHashCapacity*Sqr(HashMean));
  if FHashCapacity>1 then
    HashStdDev:=Sqrt(HashStdDev/(FHashCapacity-1))
  else
    HashStdDev:=0;
  { Print info to stdout }
  Writeln('HashSize   : ',FHashCapacity);
  Writeln('HashMean   : ',HashMean:1:4);
  Writeln('HashStdDev : ',HashStdDev:1:4);
  Writeln('ListSize   : ',FCount,'/',FCapacity);
  Writeln('StringSize : ',FStrCount,'/',FStrCapacity);
end;


Procedure TM_FPHashList.ForEachCall(proc2call:TListCallback;arg:pointer);
var
  i : integer;
  p : pointer;
begin
  for i:=0 to Count-1 Do
    begin
    p:=FHashList^[i].Data;
    if Assigned(p) then
      proc2call(p,arg);
    end;
end;


Procedure TM_FPHashList.ForEachCall(proc2call:TListStaticCallback;arg:pointer);
var
  i : integer;
  p : pointer;
begin
  for i:=0 to Count-1 Do
    begin
    p:=FHashList^[i].Data;
    if Assigned(p) then
      proc2call(p,arg);
    end;
end;


{*****************************************************************************
                               TM_FPHashObject
*****************************************************************************}

Procedure TM_FPHashObject.InternalChangeOwner(HashObjectList:TM_FPHashObjectList;const s:shortstring);
var
  Index : integer;
begin
  FOwner:=HashObjectList;
  Index:=HashObjectList.Add(s,Self);
  FStrIndex:=HashObjectList.List.List^[Index].StrIndex;
  FCachedStr:=PShortString(@FOwner.List.Strs[FStrIndex]);
end;


constructor TM_FPHashObject.CreateNotOwned;
begin
  FStrIndex:=-1;
end;


constructor TM_FPHashObject.Create(HashObjectList:TM_FPHashObjectList;const s:shortstring);
begin
  InternalChangeOwner(HashObjectList,s);
end;


Procedure TM_FPHashObject.ChangeOwner(HashObjectList:TM_FPHashObjectList);
begin
  InternalChangeOwner(HashObjectList,PShortString(@FOwner.List.Strs[FStrIndex])^);
end;


Procedure TM_FPHashObject.ChangeOwnerAndName(HashObjectList:TM_FPHashObjectList;const s:shortstring);
begin
  InternalChangeOwner(HashObjectList,s);
end;


Procedure TM_FPHashObject.Rename(const ANewName:shortstring);
var
  Index : integer;
begin
  Index:=FOwner.Rename(PShortString(@FOwner.List.Strs[FStrIndex])^,ANewName);
  if Index<>-1 then
    begin
    FStrIndex:=FOwner.List.List^[Index].StrIndex;
    FCachedStr:=PShortString(@FOwner.List.Strs[FStrIndex]);
    end;
end;


Function TM_FPHashObject.GetName:shortstring;
begin
  if FOwner<>nil then
    begin
    FCachedStr:=PShortString(@FOwner.List.Strs[FStrIndex]);
    Result:=FCachedStr^;
    end
  else
    Result:='';
end;


Function TM_FPHashObject.GetHash:Longword;
begin
  if FOwner<>nil then
    Result:=FPHash(PShortString(@FOwner.List.Strs[FStrIndex])^)
  else
    Result:=$ffffffff;
end;


{*****************************************************************************
            TM_FPHashObjectList (Copied from rtl/objpas/classes/lists.inc)
*****************************************************************************}

constructor TM_FPHashObjectList.Create(FreeObjects : boolean = True);
begin
  inherited Create;
  FHashList:=TM_FPHashList.Create;
  FFreeObjects:=Freeobjects;
end;

destructor TM_FPHashObjectList.Destroy;
begin
  if (FHashList <> nil) then
    begin
    Clear;
    FHashList.Destroy;
    end;
  inherited Destroy;
end;

Procedure TM_FPHashObjectList.Clear;
var
  i: integer;
begin
  if FFreeObjects then
    for i:=0 to FHashList.Count - 1 do
      TObject(FHashList[i]).Free;
  FHashList.Clear;
end;

Function TM_FPHashObjectList.GetCount: integer;
begin
  Result:=FHashList.Count;
end;

Procedure TM_FPHashObjectList.SetCount(const AValue: integer);
begin
  if FHashList.Count <> AValue then
    FHashList.Count:=AValue;
end;

Function TM_FPHashObjectList.GetItem(Index: Integer): TObject;
begin
  Result:=TObject(FHashList[Index]);
end;

Procedure TM_FPHashObjectList.SetItem(Index: Integer; AObject: TObject);
begin
  if OwnsObjects then
    TObject(FHashList[Index]).Free;
  FHashList[Index]:=AObject;
end;

Procedure TM_FPHashObjectList.SetCapacity(NewCapacity: Integer);
begin
  FHashList.Capacity:=NewCapacity;
end;

Function TM_FPHashObjectList.GetCapacity: integer;
begin
  Result:=FHashList.Capacity;
end;

Function TM_FPHashObjectList.Add(const AName:shortstring;AObject: TObject): Integer;
begin
  Result:=FHashList.Add(AName,AObject);
end;

Function TM_FPHashObjectList.NameOfIndex(Index: Integer): shortstring;
begin
  Result:=FHashList.NameOfIndex(Index);
end;

Function TM_FPHashObjectList.HashOfIndex(Index: Integer): LongWord;
begin
  Result:=FHashList.HashOfIndex(Index);
end;

Function TM_FPHashObjectList.GetNextCollision(Index: Integer): Integer;
begin
  Result:=FHashList.GetNextCollision(Index);
end;

Procedure TM_FPHashObjectList.Delete(Index: Integer);
begin
  if OwnsObjects then
    TObject(FHashList[Index]).Free;
  FHashList.Delete(Index);
end;

Function TM_FPHashObjectList.Expand: TM_FPHashObjectList;
begin
  FHashList.Expand;
  Result:=Self;
end;

Function TM_FPHashObjectList.Extract(Item: TObject): TObject;
begin
  Result:=TObject(FHashList.Extract(Item));
end;

Function TM_FPHashObjectList.Remove(AObject: TObject): Integer;
begin
  Result:=IndexOf(AObject);
  if (Result <> -1) then
    begin
    if OwnsObjects then
      TObject(FHashList[Result]).Free;
    FHashList.Delete(Result);
    end;
end;

Function TM_FPHashObjectList.IndexOf(AObject: TObject): Integer;
begin
  Result:=FHashList.IndexOf(Pointer(AObject));
end;


Function TM_FPHashObjectList.Find(const s:shortstring): TObject;
begin
  Result:=TObject(FHashList.Find(s));
end;


Function TM_FPHashObjectList.FindIndexOf(const s:shortstring): Integer;
begin
  Result:=FHashList.FindIndexOf(s);
end;


Function TM_FPHashObjectList.FindWithHash(const AName:shortstring;AHash:LongWord): Pointer;
begin
  Result:=TObject(FHashList.FindWithHash(AName,AHash));
end;


Function TM_FPHashObjectList.Rename(const AOldName,ANewName:shortstring): Integer;
begin
  Result:=FHashList.Rename(AOldName,ANewName);
end;


Function TM_FPHashObjectList.FindInstanceOf(AClass: TClass; AExact: Boolean; AStartAt : Integer): Integer;
var
  I : Integer;
begin
  I:=AStartAt;
  Result:=-1;
  if AExact then
    while (I<Count) and (Result=-1) do
      if Items[i].ClassType=AClass then
        Result:=I
      else
        Inc(I)
  else
    while (I<Count) and (Result=-1) do
      if Items[i].InheritsFrom(AClass) then
        Result:=I
      else
        Inc(I);
end;


Procedure TM_FPHashObjectList.Pack;
begin
  FHashList.Pack;
end;


Procedure TM_FPHashObjectList.ShowStatistics;
begin
  FHashList.ShowStatistics;
end;


Procedure TM_FPHashObjectList.ForEachCall(proc2call:TM_ObjectListCallback;arg:pointer);
begin
  FHashList.ForEachCall(TListCallBack(proc2call),arg);
end;


Procedure TM_FPHashObjectList.ForEachCall(proc2call:TM_ObjectListStaticCallback;arg:pointer);
begin
  FHashList.ForEachCall(TListStaticCallBack(proc2call),arg);
end;


{ ---------------------------------------------------------------------
    Hash support, by Dean Zobec
  ---------------------------------------------------------------------}

{ Default hash Function }

Function RSHash(const S: string; const TableSize: Longword): Longword;
const
  b = 378551;
var
  a: Longword;
  i: Longword;
begin
  a:=63689;
  Result:=0;
  if length(s)>0 then
    for i:=1 to Length(S) do
      begin
      Result:=Result * a + Ord(S[i]);
      a:=a * b;
      end;
  Result:=(Result and $7FFFFFFF) mod TableSize;
end;

{ THTNode }

constructor TM_HTCustomNode.CreateWith(const AString: string);
begin
  inherited Create;
  FKey:=AString;
end;

Function TM_HTCustomNode.HasKey(const AKey: string): boolean;
begin
  Result:=(AKey=FKey);
end;

{ TM_FPCustomHashTable }

constructor TM_FPCustomHashTable.Create;
begin
  CreateWith(196613,@RSHash);
end;

constructor TM_FPCustomHashTable.CreateWith(AHashTableSize: Longword;
  aHashFunc: TM_HashFunction);
begin
  inherited Create;
  FHashTable:=TM_FPObjectList.Create(True);
  HashTableSize:=AHashTableSize;
  FHashFunction:=aHashFunc;
end;

destructor TM_FPCustomHashTable.Destroy;
begin
  FHashTable.Free;
  inherited Destroy;
end;

Function TM_FPCustomHashTable.GetDensity: Longword;
begin
  Result:=FHashTableSize - VoidSlots
end;

Function TM_FPCustomHashTable.GetNumberOfCollisions: Longword;
begin
  Result:=FCount -(FHashTableSize - VoidSlots)
end;

Procedure TM_FPCustomHashTable.SetHashTableSize(const Value: Longword);
var
  i: Longword;
  newSize: Longword;
begin
  if Value <> FHashTableSize then
    begin
    i:=0;
    while (PRIMELIST[i] < Value) and (i < 27) do
     Inc(i);
    newSize:=PRIMELIST[i];
    if Count = 0 then
      begin
      FHashTableSize:=newSize;
      InitializeHashTable;
      end
    else
      ChangeTableSize(newSize);
    end;
end;

Procedure TM_FPCustomHashTable.InitializeHashTable;
var
  i: LongWord;
begin
  if FHashTableSize>0 Then
    for i:=0 to FHashTableSize-1 do
      FHashTable.Add(nil);
  FCount:=0;
end;

Procedure TM_FPCustomHashTable.ChangeTableSize(const ANewSize: Longword);
var
  SavedTable: TM_FPObjectList;
  SavedTableSize: Longword;
  i, j: Longword;
  temp: TM_HTCustomNode;
begin
  SavedTable:=FHashTable;
  SavedTableSize:=FHashTableSize;
  FHashTableSize:=ANewSize;
  FHashTable:=TM_FPObjectList.Create(True);
  InitializeHashTable;
  if SavedTableSize>0 Then
    for i:=0 to SavedTableSize-1 do
      if Assigned(SavedTable[i]) then
        for j:=0 to TM_FPObjectList(SavedTable[i]).Count -1 do
          begin
          temp:=TM_HTCustomNode(TM_FPObjectList(SavedTable[i])[j]);
          AddNode(temp);
          end;
  SavedTable.Free;
end;

Procedure TM_FPCustomHashTable.SetHashFunction(AHashFunction: TM_HashFunction);
begin
  if IsEmpty then
    FHashFunction:=AHashFunction
  else
    raise Exception.Create(NotEmptyMsg);
end;

Function TM_FPCustomHashTable.Find(const aKey: string): TM_HTCustomNode;
var
  hashCode: Longword;
  chn: TM_FPObjectList;
  i: Longword;
begin
  hashCode:=FHashFunction(aKey, FHashTableSize);
  chn:=Chain(hashCode);
  if Assigned(chn) then
    if chn.count>0 then
      for i:=0 to chn.Count - 1 do
        if TM_HTCustomNode(chn[i]).Key=aKey then
          Exit(TM_HTCustomNode(chn[i]));
  Result:=nil;
end;

Function TM_FPCustomHashTable.FindChainForAdd(Const aKey : String) : TM_FPObjectList;
var
  hashCode: Longword;
  i: Longword;
begin
  hashCode:=FHashFunction(aKey, FHashTableSize);
  Result:=Chain(hashCode);
  if Assigned(Result)  then
    begin
    if Result.count>0 then
      for i:=0 to Result.Count - 1 do
        if (TM_HTCustomNode(Result[i]).Key=aKey) then
          raise EDuplicate.CreateFmt(DuplicateMsg, [aKey]);
    end
  else
    begin
    FHashTable[hashcode]:=TM_FPObjectList.Create(True);
    Result:=Chain(hashCode);
    end;
  Inc(FCount);
end;


Procedure TM_FPCustomHashTable.Delete(const aKey: string);
var
  hashCode: Longword;
  chn: TM_FPObjectList;
  i: Longword;
begin
  hashCode:=FHashFunction(aKey, FHashTableSize);
  chn:=Chain(hashCode);
  if Assigned(chn) then
    if chn.count>0 then
      for i:=0 to chn.Count - 1 do
        if TM_HTCustomNode(chn[i]).Key=aKey then
          begin
          chn.Delete(i);
          dec(FCount);
          Exit;
          end;
end;

Function TM_FPCustomHashTable.IsEmpty: boolean;
begin
  Result:=(FCount = 0);
end;

Function TM_FPCustomHashTable.Chain(const index: Longword): TM_FPObjectList;
begin
  Result:=TM_FPObjectList(FHashTable[index]);
end;

Function TM_FPCustomHashTable.GetVoidSlots: Longword;
var
  i: Longword;
  num: Longword;
begin
  num:=0;
  if FHashTableSize>0 then
    for i:= 0 to FHashTableSize-1 do
      if not Assigned(Chain(i)) then
        Inc(num);
  Result:=num;
end;

Function TM_FPCustomHashTable.GetLoadFactor: double;
begin
  Result:=Count / FHashTableSize;
end;

Function TM_FPCustomHashTable.GetAVGChainLen: double;
begin
  Result:=Count / (FHashTableSize - VoidSlots);
end;

Function TM_FPCustomHashTable.GetMaxChainLength: Longword;
var
  i: Longword;
begin
  Result:=0;
  if FHashTableSize>0 Then
   for i:=0 to FHashTableSize-1 do
      if ChainLength(i) > Result then
        Result:=ChainLength(i);
end;

Function TM_FPCustomHashTable.FindOrCreateNew(const aKey: string): TM_HTCustomNode;
var
  hashCode: Longword;
  chn: TM_FPObjectList;
  i: Longword;
begin
  hashCode:=FHashFunction(aKey, FHashTableSize);
  chn:=Chain(hashCode);
  if Assigned(chn)  then
    begin
    if chn.count>0 then
      for i:=0 to chn.Count - 1 do
        if (TM_HTCustomNode(chn[i]).Key=aKey) then
          Exit(THTNode(chn[i]));
    end
  else
    begin
    FHashTable[hashcode]:=TM_FPObjectList.Create(true);
    chn:=Chain(hashcode);
    end;
  Inc(FCount);
  Result:=CreateNewNode(aKey);
  chn.Add(Result);
end;

Function TM_FPCustomHashTable.ChainLength(const ChainIndex: Longword): Longword;
begin
  if Assigned(Chain(ChainIndex)) then
    Result:=Chain(ChainIndex).Count
  else
    Result:=0;
end;

Procedure TM_FPCustomHashTable.Clear;
var
  i: Longword;
begin
  if FHashTableSize>0 then
    for i:=0 to FHashTableSize - 1 do
      if Assigned(Chain(i)) then
        Chain(i).Clear;
  FCount:=0;
end;



{ TM_FPDataHashTable }

Procedure TM_FPDataHashTable.Add(const aKey: string; aItem: pointer);
var
  chn: TM_FPObjectList;
  NewNode: TM_HTDataNode;
begin
  chn:=FindChainForAdd(akey);
  NewNode:=TM_HTDataNode(CreateNewNode(aKey));
  NewNode.Data:=aItem;
  chn.Add(NewNode);
end;

Function TM_FPDataHashTable.GetData(const Index: string): Pointer;
var
  node: TM_HTDataNode;
begin
  node:=TM_HTDataNode(Find(Index));
  if Assigned(node) then
    Result:=node.Data
  else
    Result:=nil;
end;

Procedure TM_FPDataHashTable.SetData(const index: string; const AValue: Pointer);
begin
  TM_HTDataNode(FindOrCreateNew(index)).Data:=AValue;
end;

Function TM_FPDataHashTable.CreateNewNode(const aKey : string) : TM_HTCustomNode;

begin
  Result:=TM_HTDataNode.CreateWith(aKey);
end;

Function TM_FPDataHashTable.Iterate(aMethod: TM_DataIteratorMethod): Pointer;
var
  N : TM_HTDataNode;
begin
  N:=ForEachCall(AMethod);
  if Assigned(N) then
    Result:=N.Data
  else
    Result:=nil;
end;

Procedure TM_FPDataHashTable.CallbackIterator(Item: Pointer; const Key: string; var Continue: Boolean);
begin
  FIteratorCallBack(Item, Key, Continue);
end;

Function TM_FPDataHashTable.Iterate(aMethod: TM_DataIteratorCallBack): Pointer;
begin
  FIteratorCallBack := aMethod;
  Result := Iterate(@CallbackIterator);
end;

Function TM_FPDataHashTable.ForEachCall(aMethod: TM_DataIteratorMethod): TM_HTDataNode;
var
  i, j: Longword;
  continue: Boolean;
begin
  Result:=nil;
  continue:=true;
  if FHashTableSize>0 then
    for i:=0 to FHashTableSize-1 do
      if Assigned(Chain(i)) then
        if chain(i).count>0 then
          for j:=0 to Chain(i).Count-1 do
            begin
            aMethod(TM_HTDataNode(Chain(i)[j]).Data, TM_HTDataNode(Chain(i)[j]).Key, continue);
            if not continue then
              begin
              Result:=TM_HTDataNode(Chain(i)[j]);
              Exit;
              end;
           end;
end;

Procedure TM_FPDataHashTable.AddNode(ANode : TM_HTCustomNode);
begin
  with TM_HTDataNode(ANode) do
    Add(Key,Data);
end;

{ TM_TFPStringHashTable }

Procedure TM_TFPStringHashTable.AddNode(ANode : TM_HTCustomNode);
begin
  with TM_HTStringNode(ANode) do
    Add(Key,Data);
end;

Function TM_TFPStringHashTable.GetData(const Index: string): String;
var
  node: TM_HTStringNode;
begin
  node:=TM_HTStringNode(Find(Index));
  if Assigned(node) then
    Result:=node.Data
  else
    Result:='';
end;

Procedure TM_TFPStringHashTable.SetData(const index, AValue: string);
begin
  TM_HTStringNode(FindOrCreateNew(index)).Data:=AValue;
end;

Procedure TM_TFPStringHashTable.Add(const aKey, aItem: string);
var
  chn: TM_FPObjectList;
  NewNode: TM_HTStringNode;
begin
  chn:=FindChainForAdd(akey);
  NewNode:=TM_HTStringNode(CreateNewNode(aKey));
  NewNode.Data:=aItem;
  chn.Add(NewNode);
end;

Function TM_TFPStringHashTable.CreateNewNode(const aKey : string) : TM_HTCustomNode;
begin
  Result:=TM_HTStringNode.CreateWith(aKey);
end;

Function TM_TFPStringHashTable.Iterate(aMethod: TM_StringIteratorMethod): String;
var
  N : TM_HTStringNode;
begin
  N:=ForEachCall(AMethod);
  if Assigned(N) then
    Result:=N.Data
  else
    Result:='';
end;

Procedure TM_TFPStringHashTable.CallbackIterator(Item: String; const Key: string; var Continue: Boolean);
begin
  FIteratorCallBack(Item, Key, Continue);
end;

Function TM_TFPStringHashTable.Iterate(aMethod: TM_StringIteratorCallback): String;
begin
  FIteratorCallBack := aMethod;
  Result := Iterate(@CallbackIterator);
end;

Function TM_TFPStringHashTable.ForEachCall(aMethod: TM_StringIteratorMethod): TM_HTStringNode;
var
  i, j: Longword;
  continue: boolean;
begin
  Result:=nil;
  continue:=True;
  if FHashTableSize>0 then
    for i:=0 to FHashTableSize-1 do
      if Assigned(Chain(i)) then
        if chain(i).Count>0 then
          for j:=0 to Chain(i).Count-1 do
            begin
            aMethod(TM_HTStringNode(Chain(i)[j]).Data, TM_HTStringNode(Chain(i)[j]).Key, continue);
            if not continue then
              begin
              Result:=TM_HTStringNode(Chain(i)[j]);
              Exit;
              end;
            end;
end;

{ TM_FPObjectHashTable }

Procedure TM_FPObjectHashTable.AddNode(ANode : TM_HTCustomNode);
begin
  With TM_HTObjectNode(ANode) do
    Add(Key,Data);
end;

Function TM_FPObjectHashTable.GetData(const Index: string): TObject;
var
  node: TM_HTObjectNode;
begin
  node:=TM_HTObjectNode(Find(Index));
  if Assigned(node) then
    Result:=node.Data
  else
    Result:=nil;
end;

Procedure TM_FPObjectHashTable.SetData(const index : string; AObject : TObject);
begin
  TM_HTObjectNode(FindOrCreateNew(index)).Data:=AObject;
end;

Procedure TM_FPObjectHashTable.Add(const aKey: string; AItem : TObject);
var
  chn: TM_FPObjectList;
  NewNode: TM_HTObjectNode;
begin
  chn:=FindChainForAdd(akey);
  NewNode:=TM_HTObjectNode(CreateNewNode(aKey));
  NewNode.Data:=aItem;
  chn.Add(NewNode);
end;

Function TM_FPObjectHashTable.CreateNewNode(const aKey : string) : TM_HTCustomNode;
begin
  if OwnsObjects then
    Result:=TM_HTOwnedObjectNode.CreateWith(aKey)
  else
    Result:=TM_HTObjectNode.CreateWith(aKey);
end;


Function TM_FPObjectHashTable.Iterate(aMethod: TM_ObjectIteratorMethod): TObject;
var
  N : TM_HTObjectNode;
begin
  N:=ForEachCall(AMethod);
  if Assigned(N) then
    Result:=N.Data
  else
    Result:=nil;
end;

Procedure TM_FPObjectHashTable.CallbackIterator(Item: TObject; const Key: string; var Continue: Boolean);
begin
  FIteratorCallBack(Item, Key, Continue);
end;

Function TM_FPObjectHashTable.Iterate(aMethod: TM_ObjectIteratorCallback): TObject;
begin
  FIteratorCallBack := aMethod;
  Result := Iterate(@CallbackIterator);
end;

Function TM_FPObjectHashTable.ForEachCall(aMethod: TM_ObjectIteratorMethod): TM_HTObjectNode;
var
  i, j: Longword;
  continue: boolean;
begin
  Result:=nil;
  continue:=true;
  if FHashTableSize>0 then
    for i:=0 to FHashTableSize-1 do
      if Assigned(Chain(i)) then
        if Chain(i).Count>0 then
          for j:=0 to Chain(i).Count-1 do
           begin
           aMethod(TM_HTObjectNode(Chain(i)[j]).Data, TM_HTObjectNode(Chain(i)[j]).Key, continue);
           if not continue then
             begin
             Result:=TM_HTObjectNode(Chain(i)[j]);
             Exit;
             end;
           end;
end;

constructor TM_FPObjectHashTable.Create(AOwnsObjects : Boolean = True);
begin
  inherited Create;
  FOwnsObjects:=AOwnsObjects;
end;

constructor TM_FPObjectHashTable.CreateWith(AHashTableSize: Longword; aHashFunc: TM_HashFunction; AOwnsObjects : Boolean = True);
begin
  inherited CreateWith(AHashTableSize,AHashFunc);
  FOwnsObjects:=AOwnsObjects;
end;

destructor TM_HTOwnedObjectNode.Destroy;
begin
  FreeAndNil(FData);
  inherited;
end;

{ TM_CustomBucketList }

Function TM_CustomBucketList.GetData(AItem: Pointer): Pointer;
var
  B,I : Integer;
begin
  GetBucketItem(AItem,B,I);
  Result:=FBuckets[B].Items[I].Data;
end;

Function TM_CustomBucketList.GetBucketCount: Integer;
begin
  Result:=Length(FBuckets);
end;

Procedure TM_CustomBucketList.SetData(AItem: Pointer; const AData: Pointer);
var
  B,I : Integer;
begin
  GetBucketItem(AItem,B,I);
  FBuckets[B].Items[I].Data:=AData;
end;

Procedure TM_CustomBucketList.SetBucketCount(const Value: Integer);
begin
  if (Value<>GetBucketCount) then
    SetLength(FBuckets,Value);
end;

Procedure TM_CustomBucketList.GetBucketItem(AItem: Pointer; out ABucket,
  AIndex: Integer);
begin
  if not FindItem(AItem,ABucket,AIndex) then
    Error(SErrNoSuchItem,[AItem]);
end;

Function TM_CustomBucketList.AddItem(ABucket: Integer; AItem, AData: Pointer
  ): Pointer;
var
  B : PM_Bucket;
  L : Integer;
begin
  B:=@FBuckets[ABucket];
  L:=Length(B^.Items);
  if (B^.Count=L) then
    begin
    if L<8 then
      L:=8
    else
      L:=L+L div 2;
    SetLength(B^.Items,L);
    end;
  with B^ do
    begin
    Items[Count].Item:=AItem;
    Items[Count].Data:=AData;
    Result:=AData;
    Inc(Count);
    end;
end;

Function TM_CustomBucketList.DeleteItem(ABucket: Integer; AIndex: Integer): Pointer;
var
  B : PM_Bucket;
  L : Integer;
begin
  B:=@FBuckets[ABucket];
  Result:=B^.Items[AIndex].Data;
  if B^.Count=1 then
    SetLength(B^.Items,0)
  else
    begin
    L:=(B^.Count-AIndex-1);// No point in moving if last one...
    if L>0 then
      Move(B^.Items[AIndex+1],B^.Items[AIndex],L*SizeOf(TM_BucketItem));
    end;
  Dec(B^.Count);
end;

Procedure TM_CustomBucketList.Error(Msg: String; Args: array of const);
begin
  raise ElistError.CreateFmt(Msg,Args);
end;

Function TM_CustomBucketList.FindItem(AItem: Pointer; out ABucket, AIndex: Integer
  ): Boolean;
var
  I : Integer;
  B : TM_Bucket;
begin
  ABucket:=BucketFor(AItem);
  B:=FBuckets[ABucket];
  I:=B.Count-1;
  while (I>=0) and (B.Items[I].Item<>AItem) do
    Dec(I);
  Result:=I>=0;
  if Result then
    AIndex:=I;
end;

destructor TM_CustomBucketList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

Procedure TM_CustomBucketList.Clear;
var
  B : TM_Bucket;
  I,J : Integer;
begin
  for I:=0 to Length(FBuckets)-1 do
    begin
    B:=FBuckets[I];
    for J:=B.Count-1 downto 0 do
      DeleteItem(I,J);
    end;
  SetLength(FBuckets,0);
end;

Function TM_CustomBucketList.Add(AItem, AData: Pointer): Pointer;
var
  B,I : Integer;
begin
  if FindItem(AItem,B,I) then
    Error(SDuplicateItem,[AItem]);
  Result:=AddItem(B,AItem,AData);
end;

Procedure TM_CustomBucketList.Assign(AList: TM_CustomBucketList);
var
  I,J : Integer;
begin
  Clear;
  SetLength(FBuckets,Length(Alist.FBuckets));
  for I:=0 to BucketCount-1 do
    begin
    SetLength(FBuckets[i].Items,Length(AList.Fbuckets[I].Items));
    for J:=0 to AList.Fbuckets[I].Count-1 do
      with AList.Fbuckets[I].Items[J] do
        AddItem(I,Item,Data);
    end;
end;

Function TM_CustomBucketList.Exists(AItem: Pointer): Boolean;
var
  B,I : Integer;
begin
  Result:=FindItem(AItem,B,I);
end;

Function TM_CustomBucketList.Find(AItem: Pointer; out AData: Pointer): Boolean;
var
  B,I : integer;
begin
  Result:=FindItem(AItem,B,I);
  if Result then
    AData:=FBuckets[B].Items[I].Data;
end;

Function TM_CustomBucketList.ForEach(AProc: TM_BucketProc; AInfo: Pointer
  ): Boolean;
var
  I,J,S : Integer;
  Bu : TM_Bucket;
begin
  I:=0;
  Result:=True;
  S:=GetBucketCount;
  while Result and (I<S) do
    begin
    J:=0;
    Bu:=FBuckets[I];
    while Result and (J<Bu.Count) do
      begin
      with Bu.Items[J] do
        AProc(AInfo,Item,Data,Result);
      Inc(J);
      end;
    Inc(I);
    end;
end;

Function TM_CustomBucketList.ForEach(AProc: TM_BucketProcObject): Boolean;
var
  I,J,S : Integer;
  Bu : TM_Bucket;
begin
  I:=0;
  Result:=True;
  S:=GetBucketCount;
  while Result and (I<S) do
    begin
    J:=0;
    Bu:=FBuckets[I];
    while Result and (J<Bu.Count) do
      begin
      with Bu.Items[J] do
        AProc(Item,Data,Result);
      Inc(J);
      end;
    Inc(I);
    end;
end;

Function TM_CustomBucketList.Remove(AItem: Pointer): Pointer;
var
  B,I : integer;
begin
  if FindItem(AItem,B,I) then
    begin
    Result:=FBuckets[B].Items[I].Data;
    DeleteItem(B,I);
    end
  else
    Result:=nil;
end;

{ TM_BucketList }

Function TM_BucketList.BucketFor(AItem: Pointer): Integer;
begin
  // Pointers on average have a granularity of 4
  Result:=({%H-}PtrInt(AItem) shr 2) and FBucketMask;
end;

constructor TM_BucketList.Create(ABuckets: TM_BucketListSizes);
var
  L : Integer;
begin
  inherited Create;
  L:=1 shl (Ord(Abuckets)+1);
  SetBucketCount(L);
  FBucketMask:=L-1;
end;

{ TM_ObjectBucketList }

Function TM_ObjectBucketList.GetData(AItem: TObject): TObject;
begin
  Result:=TObject(inherited GetData(AItem));
end;

Procedure TM_ObjectBucketList.SetData(AItem: TObject; const AData: TObject);
begin
  inherited SetData(Pointer(AItem),Pointer(AData));
end;

Function TM_ObjectBucketList.Add(AItem, AData: TObject): TObject;
begin
  Result:=TObject(inherited Add(Pointer(AItem),Pointer(AData)));
end;

Function TM_ObjectBucketList.Remove(AItem: TObject): TObject;
begin
  Result:=TObject(inherited Remove(Pointer(AItem)));
end;

end.
