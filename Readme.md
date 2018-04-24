# MFPC - Mini FPC

Several slightly truncated and modified [Free Pascal](https://www.freepascal.org/) units.

Modifications were made to remove unnecessary classes for console applications (eg. TComponent, TReader, TWriter).
TStringList replaced by TJPStrList from [JPLib/Containers](https://github.com/jackdp/JPLib/tree/master/Containers).

# Example

Lazarus console application:

```delphi
program console_mfpc;

//IMPORTANT: Add "$(LazarusDir)\components\lazutils" to project include files (-Fi)
{$mode objfpc}{$H+}
{$IFDEF MSWINDOWS}{$APPTYPE CONSOLE}{$ENDIF}

uses
  SysUtils, MFPC.LazUtils.LazUTF8, MFPC.Classes.Lists;

type
  PData = ^TData;
  TData = record
    No: integer;
    Str: string;
  end;

var
  List: TM_List;
  Data: PData;
  i: integer;

begin

  List := TM_List.Create;
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
```


The size of the compiled executable (Lazarus 1.9.0 + FPC 3.1.1):

Target | FPC without mod. | Using MFPC | Difference |
----|:----:|:----:|:----:|
Win 32-bit | 207.00 KB | 130.50 KB | -76.50 KB |
Win 64-bit | 279.50 KB | 145.50 KB | -134.00 KB |
Lin 32-bit | 192.86 KB | 103.63 KB | -89.23 KB |
Lin 64-bit | 258.08 KB | 134.88 KB | -123.20 KB |


