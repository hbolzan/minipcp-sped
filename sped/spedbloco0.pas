unit SpedBloco0;

{$mode objfpc}{$H+}

interface

uses
  SpedCommonTypes, SpedAppLog,
  ACBrSpedFiscal, ACBrEFDBlocos, ACBrEFDBloco_0,
  ZDataset,
  ComCtrls, Classes, SysUtils;

procedure AdicionarRegistrosBloco0Basicos(ACBrSPEDFiscal: TACBrSPEDFiscal; QueryParams: TZReadOnlyQuery);
procedure AdicionarRegistros0190(ACBrSPEDFiscal: TACBrSPEDFiscal; Unidades: TListaUnidades);

implementation

procedure AdicionarRegistro0190(Registro0190: TRegistro0190; Unidade: TUnidade);
begin
  Registro0190.UNID := Unidade.Codigo;
  Registro0190.DESCR := Unidade.Descricao;
end;

procedure AdicionarRegistro0000(Registro0000: TRegistro0000; QueryParams: TZReadOnlyQuery);
begin
  Registro0000.COD_VER := vlVersao103;
  Registro0000.COD_FIN := raOriginal;
  Registro0000.NOME := QueryParams.FieldByName('razaosocial').AsString;
  Registro0000.CNPJ := QueryParams.FieldByName('cnpj').AsString;
  Registro0000.UF := QueryParams.FieldByName('uf').AsString;
  Registro0000.IE := QueryParams.FieldByName('inscricao_estadual').AsString;
  Registro0000.COD_MUN := QueryParams.FieldByName('ibge_municipio').AsInteger;
  Registro0000.IND_PERFIL := pfPerfilA;
  Registro0000.IND_ATIV := atOutros;
end;

procedure AdicionarRegistrosBloco0Basicos(ACBrSPEDFiscal: TACBrSPEDFiscal;
  QueryParams: TZReadOnlyQuery);
begin
  ACBrSPEDFiscal.Bloco_0.Registro0001New.IND_MOV := imComDados;
  AdicionarRegistro0000(ACBrSPEDFiscal.Bloco_0.Registro0000New, QueryParams);
end;

procedure AdicionarRegistros0190(ACBrSPEDFiscal: TACBrSPEDFiscal; Unidades: TListaUnidades);
var
  Index: Integer;
begin
  TFormAppLog.Instancia.OpenSession('UNIDADES');
  for Index := 0 to Length(Unidades)-1 do begin
    TFormAppLog.Instancia.LogNameValue(Unidades[Index].Codigo, Unidades[Index].Descricao);
    AdicionarRegistro0190(ACBrSPEDFiscal.Bloco_0.Registro0190New, Unidades[Index]);
  end;
  TFormAppLog.Instancia.CloseSession('UNIDADES');
end;


end.

