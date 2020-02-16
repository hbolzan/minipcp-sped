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

  TProduto = record
    Codigo,
    Descricao,
    CodigoDeBarras,
    CodigoAnterior,
    Unidade,
    NCM,
    ExIPI,
    CodigoLST,
    CodigoCEST: String;
    TipoDeItem,
    Genero,
    AliquotaICMS: Integer;
  end;
  TListaProdutos = array of TProduto;

  TParticipante = record
    Codigo,
    Nome,
    CNPJ,
    CPF,
    InscricaoEstadual,
    CodigoSuframa,
    Endereco,
    Numero,
    Complemento,
    Bairro: String;

    CodigoPais,
    CodigoMunicipio: Integer;
  end;
  TListaParticipantes = array of TParticipante;

  // códigos de tipo de item
  (* 00 – Mercadoria para Revenda;
  01 – Matéria-prima;
  02 – Embalagem;
  03 – Produto em Processo;
  04 – Produto Acabado;
  05 – Subproduto;
  06 – Produto Intermediário;
  07 – Material de Uso e Consumo;
  08 – Ativo Imobilizado;
  09 – Serviços;
  10 – Outros insumos;
  99 - Outras
  *)

implementation

end.

