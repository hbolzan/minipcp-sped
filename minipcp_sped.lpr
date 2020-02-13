program minipcp_sped;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, datetimectrls, MyStr, zcomponent, SpedPrincipal, SpedDMPrincipal,
  SpedCommonTypes, SpedAppLog, SpedCommonProcs
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TFormSpedPrincipal, FormSpedPrincipal);
  Application.Run;
end.

