program console_normal;


{$mode objfpc}{$H+}

{$IFDEF MSWINDOWS}{$APPTYPE CONSOLE}{$ENDIF}

uses
  SysUtils, LazUTF8, Classes;

type
  PData = ^TData;
  TData = record
    No: integer;
    Str: string;
  end;

var
  List: TList;
  Data: PData;
  i: integer;

begin

  List := TList.Create;
  try

    // Add 10 items to the list
    for i := 1 to 10 do
    begin
      New(Data);
      Data^.No := i;
      Data^.Str := 'Str ' + IntToStr(i);
      List.Add(Data);
    end;

    // Display all data
    for i := 0 to List.Count - 1 do
    begin
      Data := List[i];
      if not Assigned(Data) then Continue;
      Writeln('Item no: ', Data^.No, '   Value: ', Data^.Str);
    end;

    // Free data
    for i := 0 to List.Count - 1 do
    begin
      Data := List.Items[i];
      if Assigned(Data) then Dispose(Data);
    end;

  finally
    List.Free;
  end;

end.

