unit SpedBlocoK;

{$mode objfpc}{$H+}

interface

uses
  SpedCommonTypes,
  ACBrSpedFiscal, ACBrEFDBlocos, ACBrEFDBloco_K,
  ComCtrls, Classes, SysUtils;

procedure AdicionarRegistrosBlocoK(ACBrSPEDFiscal: TACBrSPEDFiscal; ProgressBar: TProgressBar;
    EstoqueFinal: TPosicaoDeEstoqueArray; DataIni, DataFim: TDate);

implementation

function TipoDeEstoqueToIndEst(TipoDeEstoque: Integer): TACBrIndEstoque;
begin
  case TipoDeEstoque of
    1, 4: Result := estPropInformantePoder;
    2: Result := estPropTerceirosInformante;
    3: Result := estPropInformanteTerceiros;
  end;
end;

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
  case RegistroK200.IND_EST of
    estPropInformanteTerceiros,
    estPropTerceirosInformante: RegistroK200.COD_PART := IntToStr(Estoque.ParticipanteId);
  end;
end;

procedure AdicionarRegistrosBlocoK(ACBrSPEDFiscal: TACBrSPEDFiscal;
  ProgressBar: TProgressBar; EstoqueFinal: TPosicaoDeEstoqueArray;
  DataIni, DataFim: TDate);
var
  Index: Integer;
begin
  ProgressBar.Max :=  Length(EstoqueFinal);
  ACBrSPEDFiscal.Bloco_K.RegistroK001New.IND_MOV := imComDados;
  PreencherRegistroK100(ACBrSPEDFiscal.Bloco_K.RegistroK100New, DataIni, DataFim);
  for Index := 0 to Length(EstoqueFinal) - 1 do begin
    ProgressBar.StepIt;
    PreencherRegistroK200(ACBrSPEDFiscal.Bloco_K.RegistroK200New, EstoqueFinal[Index], DataFim);
  end;
end;

end.

