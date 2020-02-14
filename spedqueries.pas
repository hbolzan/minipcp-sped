unit SpedQueries;

{$mode objfpc}{$H+}

interface

uses
  SpedDMPrincipal, SpedCommonTypes,
  ZDataset,
  Classes, SysUtils;

function PosicaoDeEstoqueInicial(DataFinal: TDate): TZReadOnlyQuery;
function MovimentacaoPorProduto(Posicao: TPosicaoDeEstoque): TZReadOnlyQuery;
function GetUnidades: TZReadOnlyQuery;
function NfeParametros: TZReadOnlyQuery;

implementation

function PosicaoDeEstoqueInicialSQL(DataFim: TDate): String;
begin
  Result := StringReplace(
    'select data, tipo_de_estoque, cast(0 as integer) as tipo_de_escrituracao, participante, ' +
    'tipo, codigo, sum(quantidade) as quantidade from (' +
    'select data, tipo_de_estoque, participante, tipo, codigo, quantidade, ' +
    'row_number() over (partition by tipo_de_estoque, tipo, codigo order by data desc) as rn ' +
    'from estoques.escrituracao where tipo_de_escrituracao = 0 and data <= <periodo_fim>) q1 ' +
    'where rn = 1 group by data, tipo_de_estoque, participante, tipo, codigo ' +
    'order by tipo, codigo, tipo_de_estoque, participante',
    '<periodo_fim>',
    QuotedStr(FormatDateTime('yyyy-mm-dd', DataFim)),
    [rfReplaceAll, rfIgnoreCase]
  );
end;

function PosicaoDeEstoqueInicial(DataFinal: TDate): TZReadOnlyQuery;
begin
  Result := TDMPrincipal.Instancia.GetReadOnlyQuery(PosicaoDeEstoqueInicialSQL(DataFinal));
  Result.Open;
end;

function MovimentacaoPorProdutoSQL(TipoDeEstoque, Tipo: Integer; Codigo: String; Data: TDate): String;
var
  Sql: String;
begin
  Sql := 'select data, tipo_de_estoque, tipo_de_escrituracao, participante, tipo, codigo, ' +
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

function MovimentacaoPorProduto(Posicao: TPosicaoDeEstoque): TZReadOnlyQuery;
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

function UnidadesSQL: String;
begin
  Result := 'select distinct und.id, und.descricao from estoques.escrituracao esc ' +
    'join produtos prd on prd.tipo = esc.tipo and prd.codigo = esc.codigo ' +
    'join unidades und on und.id = prd.unidade order by und.id';
end;

function GetUnidades: TZReadOnlyQuery;
begin
  Result := TDMPrincipal.Instancia.GetReadOnlyQuery(UnidadesSQL);
  Result.Open;
end;

function NfeParametros: TZReadOnlyQuery;
begin
  Result := TDMPrincipal.Instancia.GetReadOnlyQuery('select * from nfe.nfe_parametros');
  Result.Open;
end;

end.

