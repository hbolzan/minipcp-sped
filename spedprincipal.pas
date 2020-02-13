unit SpedPrincipal;

{$mode objfpc}{$H+}

interface

uses
  SpedCommonTypes, SpedCommonProcs,
  SpedQueries, SpedBlocoK,
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

end.

