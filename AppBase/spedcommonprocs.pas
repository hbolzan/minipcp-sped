unit SpedCommonProcs;

{$mode objfpc}{$H+}

interface

uses
  Forms, Controls, Classes, SysUtils;

function PrimeiroDiaDoMes(Data: TDate): TDate;
function UltimoDiaDoMes(Data: TDate): TDate;
function VincularUmFormAUmWinControl(UmForm: TForm; UmWinControl: TWinControl): TForm;

implementation

function PrimeiroDiaDoMes(Data: TDate): TDate;
var
  y, m, d: Word;
begin
  DecodeDate(Data, y, m, d);
  Result := EncodeDate(y, m, 1);
end;

function MaiorDia(Mes, Ano: Integer): Integer;
var
  d: TDateTime;
  y, m, dd: Word;
begin
  Inc(Mes);
  if Mes > 12 then begin
    Mes := 1;
    Inc(Ano);
  end;
  d := EncodeDate(Ano, Mes, 1)-1;
  DecodeDate(d, y, m, dd);
  Result := dd;
end;

function UltimoDiaDoMes(Data: TDate): TDate;
var
  y, m, d: Word;
begin
  DecodeDate(Data, y, m, d);
  Result := EncodeDate(y, m, MaiorDia(m, y));
end;

function VincularUmFormAUmWinControl(UmForm: TForm; UmWinControl: TWinControl): TForm;
begin
  UmForm.Parent := UmWinControl;
  UmForm.Position := poDesigned;
  UmForm.WindowState := wsFullScreen;
  UmForm.Align := alClient;
  UmForm.BorderStyle := bsNone;
  UmForm.Visible := True;
  Result := UmForm;
end;

end.

