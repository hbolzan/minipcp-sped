unit SpedQueries;

{$mode objfpc}{$H+}

interface

uses
  SpedDMPrincipal, SpedCommonTypes, SpedAppLog,
  ZDataset,
  Classes, SysUtils;

function NfeParametros: TZReadOnlyQuery;
function PosicaoDeEstoqueInicial(DataFinal: TDate): TZReadOnlyQuery;
function MovimentacaoPorProduto(Posicao: TPosicaoDeEstoque): TZReadOnlyQuery;
function GetUnidades: TZReadOnlyQuery;
function GetProdutos: TZReadOnlyQuery;
function GetParticipantes: TZReadOnlyQuery;

implementation

function NfeParametros: TZReadOnlyQuery;
begin
  Result := TDMPrincipal.Instancia.GetReadOnlyQuery('select * from nfe.nfe_parametros');
  Result.Open;
end;

function ViewEscrituracao: String;
begin
  Result := 'select data, ' +
    'case when tipo_de_estoque = 4 then 1 else tipo_de_estoque end as tipo_de_estoque, ' +
    'tipo_de_escrituracao, tipo, codigo, sum(quantidade) as quantidade, participante ' +
    'from estoques.escrituracao ' +
    'group by data, case when tipo_de_estoque = 4 then 1 else tipo_de_estoque end, ' +
    'tipo_de_escrituracao, tipo, codigo, participante';
end;

function PosicaoDeEstoqueInicialSQL(DataFim: TDate): String;
begin
  Result := StringReplace(
    StringReplace(
      'select data, tipo_de_estoque, cast(0 as integer) as tipo_de_escrituracao, participante, ' +
      'tipo, codigo, sum(quantidade) as quantidade from (' +
      'select data, tipo_de_estoque, participante, tipo, codigo, quantidade, ' +
      'row_number() over (partition by tipo_de_estoque, tipo, codigo order by data desc) as rn ' +
      'from (<view_escrituracao>) q1 where tipo_de_escrituracao = 0 and data <= <periodo_fim>) q1 ' +
      'where rn = 1 group by data, tipo_de_estoque, participante, tipo, codigo ' +
      'order by tipo, codigo, tipo_de_estoque, participante',
      '<periodo_fim>',
      QuotedStr(FormatDateTime('yyyy-mm-dd', DataFim)),
      [rfReplaceAll, rfIgnoreCase]
    ),
    '<view_escrituracao>',
    ViewEscrituracao,
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
    'from (<view_escrituracao>) q1 ' +
    'where tipo_de_escrituracao > 0 and tipo_de_estoque = <tipo_de_estoque> and  tipo = <tipo> and ' +
    'codigo = <codigo> and data > <data> ' +
    'order by data';
  Result := StringReplace(
    StringReplace(
      StringReplace(
        StringReplace(
          StringReplace(Sql, '<tipo_de_estoque>', IntToStr(TipoDeEstoque), [rfReplaceAll, rfIgnoreCase]),
          '<tipo>', IntToStr(Tipo), [rfReplaceAll, rfIgnoreCase]
        ),
        '<codigo>', QuotedStr(Codigo), [rfReplaceAll, rfIgnoreCase]
      ),
      '<data>', QuotedStr(FormatDateTime('yyyy-mm-dd', Data)), [rfReplaceAll, rfIgnoreCase]
    ),
    '<view_escrituracao>',
    ViewEscrituracao,
    [rfReplaceAll, rfIgnoreCase]
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

function ProdutosSQL: String;
begin
  Result := 'select distinct prd.codigo, prd.descricao, prd.codigoean, prd.unidade, ' +
    'tip.tipo_item_bloco_k, replace(ncm, ''.'', '''') as ncm, 18 as aliq_icms, cast(null as varchar(10)) as cest ' +
    'from estoques.escrituracao esc join prd_tipos tip on tip.codigo = esc.tipo ' +
    'join produtos prd on prd.tipo = esc.tipo and prd.codigo = esc.codigo ' +
    'order by prd.codigo';
  TFormAppLog.Instancia.LogOneLine(Result);
end;

function GetProdutos: TZReadOnlyQuery;
begin
  Result := TDMPrincipal.Instancia.GetReadOnlyQuery(ProdutosSQL);
  Result.Open;
end;

function ParticipantesSQL: String;
begin
  Result := 'select distinct ' +
    'par.id, par.razaosocial, par.cnpj, par.cpf, par.inscricao_estadual, par.codigo_suframa, ' +
    'cast(par.tipo_de_logradouro || '' '' || par.nome_do_logradouro as varchar(60)) as endereco, ' +
    'par.numero, par.complemento, par.bairro, par.pais, par.ibge_municipio ' +
    'from ' +
    '  estoques.escrituracao esc ' +
    'join ' +
    'view_nfe_participantes par ' +
    'on par.id = esc.participante';
end;

function GetParticipantes: TZReadOnlyQuery;
begin
  Result := TDMPrincipal.Instancia.GetReadOnlyQuery(ParticipantesSQL);
  Result.Open;
end;

end.

