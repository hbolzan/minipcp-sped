unit SpedCommonTypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TConnectionParams = record
    CNPJ,
    Host,
    DbName,
    User,
    Password: String;
    Port: Integer;
  end;

  TPosicaoDeEstoque = record
    Data: TDate;
    Tipo: Integer;
    Codigo: String;
    ParticipanteId,
    TipoDeEstoque,
    TipoDeEscrituracao: Integer;
    Quantidade: Real;
  end;
  TPosicaoDeEstoqueArray = array of TPosicaoDeEstoque;

  TUnidade = record
    Codigo,
    Descricao: String;
  end;
  TListaUnidades = array of TUnidade;

implementation

end.

