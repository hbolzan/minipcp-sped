unit SpedPrincipal;

{$mode objfpc}{$H+}

interface

uses
  MyStr,
  SpedCommonTypes, SpedCommonProcs,
  SpedQueries, SpedBlocoK, SpedBloco0,
  SpedDMPrincipal, SpedAppLog,
  Classes, SysUtils, FileUtil, ZDataset,
  SynEdit, DateTimePicker, ACBrSpedFiscal, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls;

type

  { TFormSpedPrincipal }

  TFormSpedPrincipal = class(TForm)
    BotaoGerarSPED: TButton;
    BotaoConectar: TButton;
    CampoCfgPath: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    PainelPrincipal: TPanel;
    PainelRodape: TPanel;
    PainelMainView: TPanel;
    PainelLog: TPanel;
    PeriodoFim: TDateTimePicker;
    PeriodoIni: TDateTimePicker;
    ProgressBarPosEstoque: TProgressBar;
    ProgressBarSPED: TProgressBar;
    SaveDialog: TSaveDialog;
    procedure FormCreate(Sender: TObject);
    procedure BotaoConectarClick(Sender: TObject);
    procedure BotaoGerarSPEDClick(Sender: TObject);
    procedure ACBrSPEDFiscalError(const MsnError: AnsiString);
  private
    { private declarations }
    procedure IniciarAmbiente;
    function GerarSPED(ACBrSPEDFiscal: TACBrSPEDFiscal; QueryParams: TZReadOnlyQuery;
      Posicao: TPosicaoDeEstoqueArray; DataIni, DataFim: TDateTime): TACBrSPEDFiscal;
    procedure ConectarDB;
    procedure IniciarProgressBar(AProgressBar: TProgressBar);
    procedure GravarTXT(ACBrSPEDFiscal: TACBrSPEDFiscal; DataIni, DataFim: TDateTime);
    function DefaultTxtFileName(DataIni, DataFim: TDateTime): String;
    function IniciarComponenteSped(DataIni, DataFim: TDateTime): TACBrSPEDFiscal;
    function PosicaoDeEstoqueFinal(ProgressBar: TProgressBar; DataFim: TDate): TPosicaoDeEstoqueArray;
    function PosicaoDeEstoqueArrayAppend(AnArray: TPosicaoDeEstoqueArray; Posicao: TPosicaoDeEstoque): TPosicaoDeEstoqueArray;
    function PosicaoDeEstoqueFinalPorProduto(QueryEstoqueInicial: TZReadOnlyQuery): TPosicaoDeEstoque;
    function ProximaPosicaoDeEstoque(Posicao: TPosicaoDeEstoque; Movimentacao: TZReadOnlyQuery): TPosicaoDeEstoque;
    function AjustarPosicaoDeEstoque(PosicaoAtual, NovaPosicao: TPosicaoDeEstoque): TPosicaoDeEstoque;
    function PosicaoDeEstoque(QueryEstoque: TZReadOnlyQuery): TPosicaoDeEstoque;
    function ListaDeUnidades: TListaUnidades;
    function ListaDeProdutos: TListaProdutos;
    function ListaDeParticipantes: TListaParticipantes;
  public
    { public declarations }
  end;

var
  FormSpedPrincipal: TFormSpedPrincipal;

implementation

{$R *.lfm}


{ TFormSpedPrincipal }

procedure TFormSpedPrincipal.FormCreate(Sender: TObject);
var
  RefDate: TDate;
begin
  RefDate := IncMonth(Date, -1);
  PeriodoIni.Date := PrimeiroDiaDoMes(RefDate);
  PeriodoFim.Date := UltimoDiaDoMes(RefDate);
  VincularUmFormAUmWinControl(TFormAppLog.Instancia, PainelLog);
end;

procedure TFormSpedPrincipal.BotaoConectarClick(Sender: TObject);
begin
  ConectarDB;
end;

procedure TFormSpedPrincipal.BotaoGerarSPEDClick(Sender: TObject);
var
  Posicao: TPosicaoDeEstoqueArray;
  ACBrSPEDFiscal: TACBrSPEDFiscal;
  QueryParams: TZReadOnlyQuery;
begin
  IniciarAmbiente;
  Posicao := PosicaoDeEstoqueFinal(ProgressBarPosEstoque, PeriodoFim.Date);
  QueryParams := NfeParametros;
  ACBrSPEDFiscal := GerarSPED(IniciarComponenteSped(PeriodoIni.Date, PeriodoFim.Date), QueryParams, Posicao, PeriodoIni.Date, PeriodoFim.Date);
  try
    GravarTXT(ACBrSPEDFiscal, PeriodoIni.Date, PeriodoFim.Date);
  finally
    ACBrSPEDFiscal.Free;
    QueryParams.Free;
  end;
end;

procedure TFormSpedPrincipal.ACBrSPEDFiscalError(const MsnError: AnsiString);
begin
  TFormAppLog.Instancia.LogOneLine(MsnError);
end;

procedure TFormSpedPrincipal.IniciarAmbiente;
begin
  IniciarProgressBar(ProgressBarPosEstoque);
  IniciarProgressBar(ProgressBarSPED);
  ConectarDB;
end;

function TFormSpedPrincipal.GerarSPED(ACBrSPEDFiscal: TACBrSPEDFiscal; QueryParams: TZReadOnlyQuery;
    Posicao: TPosicaoDeEstoqueArray; DataIni, DataFim: TDateTime): TACBrSPEDFiscal;
begin
  Result := ACBrSPEDFiscal;
  TFormAppLog.Instancia.LogOneLine('*** Adicionando registros básicos ***');
  AdicionarRegistrosBloco0Basicos(ACBrSPEDFiscal, QueryParams);
  TFormAppLog.Instancia.LogOneLine('*** Adicionando Registros 0150 ***');
  AdicionarRegistros0150(Result, ListaDeParticipantes);
  TFormAppLog.Instancia.LogOneLine('*** Adicionando Registros 0190 ***');
  AdicionarRegistros0190(Result, ListaDeUnidades);
  TFormAppLog.Instancia.LogOneLine('*** Adicionando Registros 0200 ***');
  AdicionarRegistros0200(ACBrSPEDFiscal, ListaDeProdutos);
  TFormAppLog.Instancia.LogOneLine('*** Adicionando Registros Bloco K ***');
  AdicionarRegistrosBlocoK(Result, ProgressBarSPED, Posicao, DataIni, DataFim);
end;

procedure TFormSpedPrincipal.ConectarDB;
begin
  TDMPrincipal.ConectarDB(TFormAppLog.Instancia, CampoCfgPath.Text);
end;

procedure TFormSpedPrincipal.IniciarProgressBar(AProgressBar: TProgressBar);
begin
  AProgressBar.Position := 0;
  AProgressBar.Step := 1;
end;

procedure TFormSpedPrincipal.GravarTXT(ACBrSPEDFiscal: TACBrSPEDFiscal; DataIni, DataFim: TDateTime);
begin
  SaveDialog.FileName := DefaultTxtFileName(DataIni, DataFim);
  if SaveDialog.Execute then begin
    TFormAppLog.Instancia.LogNameValue('InitialDir', SaveDialog.InitialDir);
    TFormAppLog.Instancia.LogNameValue('FileName', SaveDialog.FileName);
    ACBrSPEDFiscal.Path := ExtractFilePath(SaveDialog.FileName);
    ACBrSPEDFiscal.Arquivo := ExtractFileName(SaveDialog.FileName);
    ACBrSPEDFiscal.SaveFileTXT;
  end;
end;

function TFormSpedPrincipal.DefaultTxtFileName(DataIni, DataFim: TDateTime): String;
begin
  Result := 'sped_estoque_' + FormatDateTime('yyyymmdd', DataIni) + '__' + FormatDateTime('yyyymmdd', DataFim) + '.txt';
end;

function TFormSpedPrincipal.IniciarComponenteSped(DataIni, DataFim: TDateTime): TACBrSPEDFiscal;
begin
  Result := TACBrSPEDFiscal.Create(nil);
  Result.DT_INI := DataIni;
  Result.DT_FIN := DataFim;
  Result.OnError := @ACBrSPEDFiscalError;
end;

function TFormSpedPrincipal.PosicaoDeEstoqueFinal(ProgressBar: TProgressBar; DataFim: TDate): TPosicaoDeEstoqueArray;
var
  QueryEstoqueInicial: TZReadOnlyQuery;
begin
  Result := TPosicaoDeEstoqueArray.Create;
  QueryEstoqueInicial := PosicaoDeEstoqueInicial(DataFim);
  ProgressBar.Max := QueryEstoqueInicial.RecordCount;
  try
    while not QueryEstoqueInicial.Eof do begin
      ProgressBar.StepIt;
      Result := PosicaoDeEstoqueArrayAppend(Result, PosicaoDeEstoqueFinalPorProduto(QueryEstoqueInicial));
      QueryEstoqueInicial.Next;
    end;
  finally
    QueryEstoqueInicial.Free;
  end;
end;

function TFormSpedPrincipal.PosicaoDeEstoqueArrayAppend(AnArray: TPosicaoDeEstoqueArray;
  Posicao: TPosicaoDeEstoque): TPosicaoDeEstoqueArray;
var
  NewIndex: Integer;
begin
  NewIndex := Length(AnArray);
  SetLength(AnArray, NewIndex + 1);
  AnArray[NewIndex] := Posicao;
  Result := AnArray;
end;

function TFormSpedPrincipal.PosicaoDeEstoqueFinalPorProduto(QueryEstoqueInicial: TZReadOnlyQuery): TPosicaoDeEstoque;
var
  PosicaoInicial: TPosicaoDeEstoque;
  Movimentacao: TZReadOnlyQuery;
begin
  PosicaoInicial := PosicaoDeEstoque(QueryEstoqueInicial);
  Movimentacao := MovimentacaoPorProduto(PosicaoInicial);
  try
    Result := ProximaPosicaoDeEstoque(PosicaoInicial, Movimentacao)
  finally
    Movimentacao.Free;
  end;
end;

function TFormSpedPrincipal.ProximaPosicaoDeEstoque(Posicao: TPosicaoDeEstoque; Movimentacao: TZReadOnlyQuery): TPosicaoDeEstoque;
var
  NovaPosicao: TPosicaoDeEstoque;
begin
  if Movimentacao.IsEmpty then begin
    Result := Posicao;
  end else begin
    NovaPosicao := AjustarPosicaoDeEstoque(Posicao, PosicaoDeEstoque(Movimentacao));
    Movimentacao.Next;
    if not Movimentacao.EOF then begin
      Result := ProximaPosicaoDeEstoque(NovaPosicao, Movimentacao);
    end else begin
      Result := NovaPosicao;
    end;
  end;
end;

function TFormSpedPrincipal.AjustarPosicaoDeEstoque(PosicaoAtual, NovaPosicao: TPosicaoDeEstoque): TPosicaoDeEstoque;
begin
  Result := PosicaoAtual;
  Result.Data := NovaPosicao.Data;
  Result.Quantidade := PosicaoAtual.Quantidade + NovaPosicao.Quantidade;
end;

function TFormSpedPrincipal.PosicaoDeEstoque(QueryEstoque: TZReadOnlyQuery): TPosicaoDeEstoque;
begin
  Result.Data := QueryEstoque.FieldByName('data').AsDateTime;
  Result.TipoDeEstoque := QueryEstoque.FieldByName('tipo_de_estoque').AsInteger;
  Result.TipoDeEscrituracao := QueryEstoque.FieldByName('tipo_de_escrituracao').AsInteger;
  Result.ParticipanteId := QueryEstoque.FieldByName('participante').AsInteger;
  Result.Tipo := QueryEstoque.FieldByName('tipo').AsInteger;
  Result.Codigo := QueryEstoque.FieldByName('codigo').AsString;
  Result.Quantidade := QueryEstoque.FieldByName('quantidade').AsFloat;
end;

function TFormSpedPrincipal.ListaDeUnidades: TListaUnidades;
var
  QueryUnidades: TZReadOnlyQuery;
  Index: Integer;
begin
  Result := TListaUnidades.Create;
  QueryUnidades := GetUnidades;
  try
    while not QueryUnidades.EOF do begin
      Index := Length(Result);
      SetLength(Result, Index + 1);
      Result[Index].Codigo := QueryUnidades.FieldByName('id').AsString;
      Result[Index].Descricao := QueryUnidades.FieldByName('descricao').AsString;
      QueryUnidades.Next;
    end;
  finally
    QueryUnidades.Free;
  end;
end;

function TFormSpedPrincipal.ListaDeProdutos: TListaProdutos;
var
  Index: Integer;
  QueryProdutos: TZReadOnlyQuery;

  function FromQueryRow(Query: TZReadOnlyQuery): TProduto;
  begin
    Result.Codigo := Query.FieldByName('codigo').AsString;
    Result.Descricao := Query.FieldByName('descricao').AsString;
    Result.CodigoDeBarras := Query.FieldByName('codigoean').AsString;
    Result.Unidade := Query.FieldByName('unidade').AsString;
    Result.NCM := Query.FieldByName('ncm').AsString;
    Result.TipoDeItem := Query.FieldByName('tipo_item_bloco_k').AsInteger;
    Result.AliquotaICMS := Query.FieldByName('aliq_icms').AsInteger;
    Result.CodigoCEST := IIfStr(Query.FieldByName('cest').IsNull, '', Query.FieldByName('cest').AsString);

    //Result.CodigoAnterior := Query.FieldByName('').AsString;
    //Result.ExIPI := Query.FieldByName('').AsString;
    //Result.CodigoLST := Query.FieldByName('').AsString;
    //Result.Genero :=
  end;

begin
  Result := TListaProdutos.Create;
  QueryProdutos := GetProdutos;
  try
    while not QueryProdutos.EOF do begin
      Index := Length(Result);
      SetLength(Result, Index + 1);
      Result[Index] := FromQueryRow(QueryProdutos);
      QueryProdutos.Next;
    end;
  finally
    QueryProdutos.Free;
  end;
end;

function TFormSpedPrincipal.ListaDeParticipantes: TListaParticipantes;
var
  Index: Integer;
  QueryParticipantes: TZReadOnlyQuery;

  function FromQueryRow(Query: TZReadOnlyQuery): TParticipante;
  begin
    Result.Codigo := Query.FieldByName('id').AsString;
    Result.Nome := Query.FieldByName('razaosocial').AsString;
    Result.CNPJ := Query.FieldByName('cnpj').AsString;
    Result.CPF := Query.FieldByName('cpf').AsString;
    Result.InscricaoEstadual := Query.FieldByName('inscricao_estadual').AsString;
    Result.CodigoSuframa := Query.FieldByName('codigo_suframa').AsString;
    Result.Endereco := Query.FieldByName('endereco').AsString;
    Result.Numero := Query.FieldByName('numero').AsString;
    Result.Complemento := Query.FieldByName('complemento').AsString;
    Result.Bairro := Query.FieldByName('bairro').AsString;
    Result.CodigoPais := Query.FieldByName('pais').AsInteger;
    Result.CodigoMunicipio := Query.FieldByName('ibge_municipio').AsInteger;
  end;

begin
  Result := TListaParticipantes.Create;
  QueryParticipantes := GetParticipantes;
  try
    while not QueryParticipantes.EOF do begin
      Index := Length(Result);
      SetLength(Result, Index + 1);
      Result[Index] := FromQueryRow(QueryParticipantes);
      QueryParticipantes.Next;
    end;
  finally
    QueryParticipantes.Free;
  end;
end;

end.

