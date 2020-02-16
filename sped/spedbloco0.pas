unit SpedBloco0;

{$mode objfpc}{$H+}

interface

uses
  MyStr,
  SpedCommonTypes, SpedAppLog,
  ACBrSpedFiscal, ACBrEFDBlocos, ACBrEFDBloco_0,
  ZDataset,
  Classes, SysUtils;

procedure AdicionarRegistrosBloco0Basicos(ACBrSPEDFiscal: TACBrSPEDFiscal; QueryParams: TZReadOnlyQuery);
procedure AdicionarRegistros0150(ACBrSPEDFiscal: TACBrSPEDFiscal; Participantes: TListaParticipantes);
procedure AdicionarRegistros0190(ACBrSPEDFiscal: TACBrSPEDFiscal; Unidades: TListaUnidades);
procedure AdicionarRegistros0200(ACBrSPEDFiscal: TACBrSPEDFiscal; Produtos: TListaProdutos);

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

procedure AdicionarRegistro0005(Registro0005: TRegistro0005; QueryParams: TZReadOnlyQuery);
begin
  Registro0005.FANTASIA := QueryParams.FieldByName('nome_fantasia').AsString;
  Registro0005.CEP := QueryParams.FieldByName('cep').AsString;
  Registro0005.ENDERECO := QueryParams.FieldByName('tipo_de_logradouro').AsString + ' ' + QueryParams.FieldByName('nome_do_logradouro').AsString;
  Registro0005.NUM := QueryParams.FieldByName('numero').AsString;
  Registro0005.COMPL := QueryParams.FieldByName('complemento').AsString;
  Registro0005.BAIRRO := QueryParams.FieldByName('bairro').AsString;
  Registro0005.FONE := QueryParams.FieldByName('telefone').AsString;
  //Registro0005.EMAIL := QueryParams.FieldByName('').AsString;
end;

procedure AdicionarRegistrosBloco0Basicos(ACBrSPEDFiscal: TACBrSPEDFiscal;
  QueryParams: TZReadOnlyQuery);
begin
  ACBrSPEDFiscal.Bloco_0.Registro0001New.IND_MOV := imComDados;
  AdicionarRegistro0000(ACBrSPEDFiscal.Bloco_0.Registro0000New, QueryParams);
  AdicionarRegistro0005(ACBrSPEDFiscal.Bloco_0.Registro0005New, QueryParams);
end;

procedure AdicionarRegistro0150(Registro0150: TRegistro0150; Participante: TParticipante);
begin
  Registro0150.COD_PART := Participante.Codigo;
  Registro0150.NOME := Participante.Nome;
  Registro0150.COD_PAIS := IntToStr(Participante.CodigoPais);
  Registro0150.CNPJ := Participante.CNPJ;
  Registro0150.CPF := Participante.CPF;
  Registro0150.IE := Participante.InscricaoEstadual;
  Registro0150.COD_MUN := Participante.CodigoMunicipio;
  Registro0150.SUFRAMA := Participante.CodigoSuframa;
  Registro0150.ENDERECO := Participante.Endereco;
  Registro0150.NUM := Participante.Numero;
  Registro0150.COMPL := Participante.Complemento;
  Registro0150.BAIRRO := Participante.Bairro;
end;

procedure AdicionarRegistros0150(ACBrSPEDFiscal: TACBrSPEDFiscal; Participantes: TListaParticipantes);
var
  Index: Integer;
begin
  TFormAppLog.Instancia.OpenSession('PARTICIPANTES');
  for Index := 0 to Length(Participantes)-1 do begin
    TFormAppLog.Instancia.LogNameValue(Participantes[Index].CNPJ, Participantes[Index].Nome);
    AdicionarRegistro0150(ACBrSPEDFiscal.Bloco_0.Registro0150New, Participantes[Index]);
  end;
  TFormAppLog.Instancia.CloseSession('PARTICIPANTES');
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

procedure AdicionarRegistro0200(Registro0200: TRegistro0200; Produto: TProduto);
begin
  Registro0200.COD_ITEM := Produto.Codigo;
  Registro0200.DESCR_ITEM := Produto.Descricao;
  Registro0200.COD_BARRA := Produto.CodigoDeBarras;
  Registro0200.UNID_INV := Produto.Unidade;
  Registro0200.TIPO_ITEM := TACBrTipoItem(Produto.TipoDeItem);
  Registro0200.COD_NCM := Produto.NCM;
  Registro0200.ALIQ_ICMS := Produto.AliquotaICMS;
  Registro0200.CEST := Produto.CodigoCEST;
end;

procedure AdicionarRegistros0200(ACBrSPEDFiscal: TACBrSPEDFiscal; Produtos: TListaProdutos);
var
  Index: Integer;
begin
  for Index := 0 to Length(Produtos)-1 do begin
    AdicionarRegistro0200(ACBrSPEDFiscal.Bloco_0.Registro0200New, Produtos[Index]);
  end;
end;

end.

