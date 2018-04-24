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
}

unit MFPC.Classes.SHARED;

{$mode objfpc}{$H+}
{$WARN 5024 off : Parameter "$1" not used}
{$WARN 5057 off : Local variable "$1" does not seem to be initialized}
{$WARN 4055 off : Conversion between ordinals and pointers is not portable}
interface

uses
  SysUtils, Types;
  //sysconst,
  //RtlConsts;

  //Classes


{$ifdef CLASSESINLINE}{$inline on}{$endif}


type
   { extra types to compile with FPC }
   HRSRC =  TFPResourceHandle deprecated;
   TComponentName = string;
   THandle = System.THandle;

   TPoint=Types.TPoint;
   TRect=Types.TRect;
   TSmallPoint=Types.TSmallPoint;

{$ifndef FPC_HAS_FEATURE_DYNLIBS}
   HMODULE = ptrint;
{$else}
   HModule = System.HModule;
{$endif}

const

{ Maximum TList size }

{$ifdef cpu16}
  MaxListSize = {Maxint div 16}1024;
{$else cpu16}
  MaxListSize = Maxint div 16;
{$endif cpu16}

{ values for TShortCut }

  scShift = $2000;
  scCtrl = $4000;
  scAlt = $8000;
  scNone = 0;

{ TStream seek origins }
const
  soFromBeginning = 0;
  soFromCurrent = 1;
  soFromEnd = 2;

type
  TSeekOrigin = (soBeginning, soCurrent, soEnd);
  TDuplicates = Types.TDuplicates;

// For Delphi and backwards compatibility.
const
  dupIgnore = Types.dupIgnore;
  dupAccept = Types.dupAccept;
  dupError  = Types.dupError;

{ TFileStream create mode }
const
  fmCreate        = $FF00;
  fmOpenRead      = 0;
  fmOpenWrite     = 1;
  fmOpenReadWrite = 2;

{ TParser special tokens }

  toEOF     = Char(0);
  toSymbol  = Char(1);
  toString  = Char(2);
  toInteger = Char(3);
  toFloat   = Char(4);
  toWString = Char(5);

Const
  FilerSignature : Array[1..4] of char = 'TPF0';

type
{ Text alignment types }
  TAlignment = (taLeftJustify, taRightJustify, taCenter);

  TLeftRight = taLeftJustify..taRightJustify;

  TBiDiMode = (bdLeftToRight,bdRightToLeft,bdRightToLeftNoAlign,bdRightToLeftReadingOnly);


{ Types used by standard events }
  TShiftStateEnum = (ssShift, ssAlt, ssCtrl,
    ssLeft, ssRight, ssMiddle, ssDouble,
    // Extra additions
    ssMeta, ssSuper, ssHyper, ssAltGr, ssCaps, ssNum,
    ssScroll,ssTriple,ssQuad,ssExtra1,ssExtra2,
    ssScrollH);

{$packset 1}
  TShiftState = set of TShiftStateEnum;
{$packset default}

  THelpContext = -MaxLongint..MaxLongint;
  THelpType = (htKeyword, htContext);

  TShortCut = Low(Word)..High(Word);

{ Standard events }

  TNotifyEvent = procedure(Sender: TObject) of object;
  THelpEvent = function (Command: Word; Data: Longint;
    var CallHelp: Boolean): Boolean of object;
  TGetStrProc = procedure(const S: string) of object;

{ Exception classes }

  EStreamError = class(Exception);
  EFCreateError = class(EStreamError);
  EFOpenError = class(EStreamError);
  EFilerError = class(EStreamError);
  EReadError = class(EFilerError);
  EWriteError = class(EFilerError);
  EClassNotFound = class(EFilerError);
  EMethodNotFound = class(EFilerError);
  EInvalidImage = class(EFilerError);
  EResNotFound = class(Exception);
{$ifdef FPC_TESTGENERICS}
  EListError = fgl.EListError;
{$else}
  EListError = class(Exception);
{$endif}
  EBitsError = class(Exception);
  EStringListError = class(Exception);
  EComponentError = class(Exception);
  EParserError = class(Exception);
  EOutOfResources = class(EOutOfMemory);
  EInvalidOperation = class(Exception);
  TExceptionClass = Class of Exception;

{ ---------------------------------------------------------------------
  Free Pascal Observer support
  ---------------------------------------------------------------------}


Const
  SGUIDObserved = '{663C603C-3F3C-4CC5-823C-AC8079F979E5}';
  SGUIDObserver = '{BC7376EA-199C-4C2A-8684-F4805F0691CA}';

Type
  // Notification operations :
  // Observer has changed, is freed, item added to/deleted from list, custom event.
  TFPObservedOperation = (ooChange,ooFree,ooAddItem,ooDeleteItem,ooCustom);
{$INTERFACES CORBA}

  { IFPObserved }

  IFPObserved = Interface [SGUIDObserved]
    // attach a new observer
    Procedure FPOAttachObserver(AObserver : TObject);
    // Detach an observer
    Procedure FPODetachObserver(AObserver : TObject);
    // Notify all observers of a change.
    Procedure FPONotifyObservers(ASender : TObject; AOperation : TFPObservedOperation; Data : Pointer);
  end;

  { IFPObserver }

  IFPObserver = Interface  [SGUIDObserver]
    // Called by observed when observers are notified.
    Procedure FPOObservedChanged(ASender : TObject; Operation : TFPObservedOperation; Data : Pointer);
  end;
{$INTERFACES COM}

  EObserver = Class(Exception);


{ TFPList class }

  PPointerList = ^TPointerList;
  TPointerList = array[0..MaxListSize - 1] of Pointer;
  TListSortCompare = function (Item1, Item2: Pointer): Integer;
  TListCallback = Types.TListCallback;
  TListStaticCallback = Types.TListStaticCallback;




implementation

end.
