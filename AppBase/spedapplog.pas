unit SpedAppLog;

{$mode objfpc}{$H+}

interface

uses
  MyStr,
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls;

type

  { TFormAppLog }

  TFormAppLog = class(TForm)
    MemoLog: TMemo;
    Panel1: TPanel;
  private
    { private declarations }
  public
    { public declarations }
    class function Instancia: TFormAppLog;
    procedure OpenSession(SessionName: String);
    procedure CloseSession(SessionName: String);
    procedure LogNameValue(KeyName, Value: String);
    procedure LogOneLine(Line: String);
    procedure LogStrings(Strings: TStringList);
  end;

implementation

{$R *.lfm}

{ TFormAppLog }

var
  FormAppLog: TFormAppLog;

class function TFormAppLog.Instancia: TFormAppLog;
begin
  if not Assigned(FormAppLog) then begin
    FormAppLog := TFormAppLog.Create(Application);
  end;
  Result := FormAppLog;
end;

procedure TFormAppLog.OpenSession(SessionName: String);
begin
  MemoLog.Lines.Append(PadC(' =' + SessionName + ' - START =', 80, '='));
end;

procedure TFormAppLog.CloseSession(SessionName: String);
begin
  MemoLog.Lines.Append(PadC('= ' + SessionName + ' - END =', 80, '='));
end;

procedure TFormAppLog.LogNameValue(KeyName, Value: String);
begin
  MemoLog.Lines.Append(KeyName + ': ' + Value);
end;

procedure TFormAppLog.LogOneLine(Line: String);
begin
  MemoLog.Lines.Append(Line);
end;

procedure TFormAppLog.LogStrings(Strings: TStringList);
begin
  MemoLog.Lines.AddStrings(Strings);
end;

initialization
  FormAppLog := Nil;

end.

