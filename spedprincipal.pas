unit SpedPrincipal;

{$mode objfpc}{$H+}

interface

uses
  SpedCommonTypes, SpedCommonProcs,
  SpedDMPrincipal, SpedAppLog,
  ACBrEFDBlocos, ACBrEFDBloco_K,
  Classes, SysUtils, FileUtil, ZDataset,
  SynEdit, DateTimePicker, ACBrSpedFiscal, Forms, Controls, Graphics, Dialogs,
  StdCtrls, CustomDrawnControls, ExtCtrls, ComCtrls;

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
  private
    { private declarations }
    procedure IniciarAmbiente;
    function GerarSPED(ACBrSPEDFiscal: TACBrSPEDFiscal; Posicao: TPosicaoDeEstoqueArray;
      DataIni, DataFim: TDateTime): TACBrSPEDFiscal;
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
    function PosicaoDeEstoqueInicial(DataFinal: TDate): TZReadOnlyQuery;
    function MovimentacaoPorProduto(Posicao: TPosicaoDeEstoque): TZReadOnlyQuery;
    function PosicaoDeEstoqueInicialSQL(DataFim: TDate): String;
    function MovimentacaoPorProdutoSQL(TipoDeEstoque, Tipo: Integer; Codigo: String; Data: TDate): String;

    procedure AdicionarRegistrosBlocoK(ACBrSPEDFiscal: TACBrSPEDFiscal; ProgressBar: TProgressBar;
        EstoqueFinal: TPosicaoDeEstoqueArray; DataFim: TDate);
    function TipoDeEstoqueToIndEst(TipoDeEstoque: Integer): TACBrIndEstoque;
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
begin
  IniciarAmbiente;
  Posicao := PosicaoDeEstoqueFinal(ProgressBarPosEstoque, PeriodoFim.Date);
  ACBrSPEDFiscal := GerarSPED(IniciarComponenteSped(PeriodoIni.Date, PeriodoFim.Date), Posicao, PeriodoIni.Date, PeriodoFim.Date);
  try
    GravarTXT(ACBrSPEDFiscal, PeriodoIni.Date, PeriodoFim.Date);
  finally
    ACBrSPEDFiscal.Free;
  end;
end;

procedure TFormSpedPrincipal.IniciarAmbiente;
begin
  IniciarProgressBar(ProgressBarPosEstoque);
  IniciarProgressBar(ProgressBarSPED);
  ConectarDB;
end;

function TFormSpedPrincipal.GerarSPED(ACBrSPEDFiscal: TACBrSPEDFiscal; Posicao: TPosicaoDeEstoqueArray;
    DataIni, DataFim: TDateTime): TACBrSPEDFiscal;
begin
  Result := ACBrSPEDFiscal;
  AdicionarRegistrosBlocoK(Result, ProgressBarSPED, Posicao, DataFim);
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
  Result.Tipo := QueryEstoque.FieldByName('tipo').AsInteger;
  Result.Codigo := QueryEstoque.FieldByName('codigo').AsString;
  Result.Quantidade := QueryEstoque.FieldByName('quantidade').AsFloat;
end;

function TFormSpedPrincipal.PosicaoDeEstoqueInicial(DataFinal: TDate): TZReadOnlyQuery;
begin
  Result := TDMPrincipal.Instancia.GetReadOnlyQuery(PosicaoDeEstoqueInicialSQL(DataFinal));
  Result.Open;
end;

function TFormSpedPrincipal.MovimentacaoPorProduto(Posicao: TPosicaoDeEstoque): TZReadOnlyQuery;
begin
  Result := TDMPrincipal.Instancia.GetReadOnlyQuery(
    MovimentacaoPorProdutoSQL(
      Posicao.TipoDeEstoque,
      Posicao.Tipo,
      Posicao.Codigo,
      Posicao.Data
    )
  );
  Result.Open;
end;

function TFormSpedPrincipal.PosicaoDeEstoqueInicialSQL(DataFim: TDate): String;
begin
  Result := StringReplace(
    'select data, tipo_de_estoque, cast(0 as integer) as tipo_de_escrituracao, tipo, codigo, sum(quantidade) as quantidade from (' +
    'select data, tipo_de_estoque, tipo, codigo, quantidade, ' +
    'row_number() over (partition by tipo_de_estoque, tipo, codigo order by data desc) as rn ' +
    'from estoques.escrituracao where tipo_de_escrituracao = 0 and data <= <periodo_fim>) q1 ' +
    'where rn = 1 group by data, tipo_de_estoque, tipo, codigo ' +
    'order by tipo, codigo, tipo_de_estoque',
    '<periodo_fim>',
    QuotedStr(FormatDateTime('yyyy-mm-dd', DataFim)),
    [rfReplaceAll, rfIgnoreCase]
  );
end;

function TFormSpedPrincipal.MovimentacaoPorProdutoSQL(TipoDeEstoque, Tipo: Integer; Codigo: String; Data: TDate): String;
var
  Sql: String;
begin
  Sql := 'select data, tipo_de_estoque, tipo_de_escrituracao, tipo, codigo, ' +
    'cast(case when tipo_de_escrituracao = 1 then quantidade else -quantidade end as float) as quantidade ' +
    'from estoques.escrituracao ' +
    'where tipo_de_escrituracao > 0 and tipo_de_estoque = <tipo_de_estoque> and  tipo = <tipo> and ' +
    'codigo = <codigo> and data > <data> ' +
    'order by data, id';
  Result := StringReplace(
    StringReplace(
      StringReplace(
        StringReplace(Sql, '<tipo_de_estoque>', IntToStr(TipoDeEstoque), [rfReplaceAll, rfIgnoreCase]),
        '<tipo>', IntToStr(Tipo), [rfReplaceAll, rfIgnoreCase]
      ),
      '<codigo>', QuotedStr(Codigo), [rfReplaceAll, rfIgnoreCase]
    ),
    '<data>', QuotedStr(FormatDateTime('yyyy-mm-dd', Data)), [rfReplaceAll, rfIgnoreCase]
  );
end;

procedure TFormSpedPrincipal.AdicionarRegistrosBlocoK(ACBrSPEDFiscal: TACBrSPEDFiscal; ProgressBar: TProgressBar;
  EstoqueFinal: TPosicaoDeEstoqueArray; DataFim: TDate);

  procedure PreencherRegistroK100(RegistroK100: TRegistroK100; DataIni, DataFim: TDateTime);
  begin
    RegistroK100.DT_INI := DataIni;
    RegistroK100.DT_FIN := DataFim;
  end;

  procedure PreencherRegistroK200(RegistroK200: TRegistroK200; Estoque: TPosicaoDeEstoque; DataFim: TDate);
  begin
    RegistroK200.DT_EST := DataFim;
    RegistroK200.COD_ITEM := Estoque.Codigo;
    RegistroK200.QTD := Estoque.Quantidade;
    RegistroK200.IND_EST := TipoDeEstoqueToIndEst(Estoque.TipoDeEstoque);

    (* ATENÇÃO PARA REGISTROS DE ESTOQUE DE TERCEIROS
     *
     * RegistroK200.COD_PART := ;
     *
    **)
  end;

var
  Index: Integer;

begin
  ProgressBar.Max :=  Length(EstoqueFinal);
  ACBrSPEDFiscal.Bloco_K.RegistroK001New.IND_MOV := imComDados;
  PreencherRegistroK100(ACBrSPEDFiscal.Bloco_K.RegistroK100New, PeriodoIni.Date, PeriodoFim.Date);
  for Index := 0 to Length(EstoqueFinal) - 1 do begin
    ProgressBar.StepIt;
    if (EstoqueFinal[Index].TipoDeEstoque = 1) or (EstoqueFinal[Index].TipoDeEstoque = 4) then begin
      PreencherRegistroK200(ACBrSPEDFiscal.Bloco_K.RegistroK200New, EstoqueFinal[Index], DataFim);
    end;
  end;
end;

function TFormSpedPrincipal.TipoDeEstoqueToIndEst(TipoDeEstoque: Integer): TACBrIndEstoque;
begin
  case TipoDeEstoque of
    1, 4: Result := estPropInformantePoder;
    2: Result := estPropTerceirosInformante;
    3: Result := estPropInformanteTerceiros;
  end;
end;

end.

