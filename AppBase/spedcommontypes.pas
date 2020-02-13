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
    TipoDeEstoque,
    TipoDeEscrituracao: Integer;
    Quantidade: Real;
  end;
  TPosicaoDeEstoqueArray = array of TPosicaoDeEstoque;

implementation

end.

