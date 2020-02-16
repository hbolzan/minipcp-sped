unit SpedDMPrincipal;

{$mode objfpc}{$H+}

interface

uses
  MyStr,
  SpedCommonTypes, SpedAppLog,
  IniFiles,
  Forms, Classes, SysUtils, FileUtil, ZConnection, ZDataset;

type


  { TDMPrincipal }
  TDMPrincipal = class(TDataModule)
    ZConnection: TZConnection;
    ZReadOnlyQuery1: TZReadOnlyQuery;
  private
    { private declarations }
    function LogConnectionParams(Logger: TFormAppLog; Params: TConnectionParams): TConnectionParams;
    function ConnectionParams(ConfigFilePath: String): TConnectionParams;
    function SetDBConnection(Logger: TFormAppLog; Params: TConnectionParams): Boolean;
    procedure DoConectarDB(Logger: TFormAppLog; ConfigFilePath: String);
  public
    { public declarations }
    class function Instancia: TDMPrincipal;
    class procedure ConectarDB(Logger: TFormAppLog; ConfigFilePath: String);
    function GetReadOnlyQuery(Sql: String): TZReadOnlyQuery;
  end;

implementation

{$R *.lfm}

{ TDMPrincipal }

var
  DMPrincipal: TDMPrincipal;

class function TDMPrincipal.Instancia: TDMPrincipal;
begin
  if not Assigned(DMPrincipal) then begin
    DMPrincipal := TDMPrincipal.Create(Application);
  end;
  Result := DMPrincipal;
end;

class procedure TDMPrincipal.ConectarDB(Logger: TFormAppLog; ConfigFilePath: String);
var
  DM: TDMPrincipal;
begin
  DM := Self.Instancia;
  DM.DoConectarDB(Logger, ConfigFilePath);
end;

function TDMPrincipal.LogConnectionParams(Logger: TFormAppLog; Params: TConnectionParams): TConnectionParams;
begin
  Logger.OpenSession('CONNECTION PARAMS [' + Params.CNPJ + ']');
  Logger.LogNameValue('Host', Params.Host);
  Logger.LogNameValue('Port', IntToStr(Params.Port));
  Logger.LogNameValue('DbName', Params.DbName);
  Logger.LogNameValue('User', Params.User);
  Logger.LogNameValue('Password', Params.Password);
  Logger.CloseSession('CONNECTION PARAMS [' + Params.CNPJ + ']');
  Result := Params;
end;

function TDMPrincipal.ConnectionParams(ConfigFilePath: String
  ): TConnectionParams;

var
  CNPJ: String;
  Config: TIniFile;
  Sections: TStringList;

  function Params(Host, DbName, User, Password, SenhaDeRegistro: String; Port: Integer): TConnectionParams;
  begin
    Result.CNPJ := CNPJ;
    Result.Host := Host;
    Result.Port := IIf(Port = 0, 5432, Port);
    Result.DbName := DbName;
    Result.User := IIfStr(User = '', 'postgres', User);
    Result.Password := IIfStr(Password <> '', Password, 'MiniPCP' + SenhaDeRegistro);
  end;

begin
  Config := TIniFile.Create(ConfigFilePath);
  Sections := TStringList.Create;
  try
    Config.ReadSections(Sections);
    CNPJ := Sections[0];
    Result := Params(
      Config.ReadString(CNPJ, 'DbHost', ''),
      Config.ReadString(CNPJ, 'DbName', ''),
      Config.ReadString(CNPJ, 'DbUser', ''),
      Config.ReadString(CNPJ, 'DbPassword', ''),
      Config.ReadString(CNPJ, 'Senha', ''),
      InteiroDe(Config.ReadString(CNPJ, 'Port', ''))
    );

  finally
    Sections.Free;
    Config.Free;
  end;
end;

function TDMPrincipal.SetDBConnection(Logger: TFormAppLog; Params: TConnectionParams): Boolean;
begin
  ZConnection.Connected := False;
  ZConnection.HostName := Params.Host;
  ZConnection.Port := Params.Port;
  ZConnection.Database := Params.DbName;
  ZConnection.User := Params.User;
  ZConnection.Password := Params.Password;

  try
    ZConnection.Connect;
    Result := True;
  except
    on E: Exception do begin
      Logger.LogOneLine(E.Message);
      Result := False;
    end;
  end;
end;

function TDMPrincipal.GetReadOnlyQuery(Sql: String): TZReadOnlyQuery;
begin
  Result := TZReadOnlyQuery.Create(Nil);
  Result.Connection := ZConnection;
  Result.SQL.Text := Sql;
end;

procedure TDMPrincipal.DoConectarDB(Logger: TFormAppLog; ConfigFilePath: String);
begin
  if SetDBConnection(Logger, LogConnectionParams(Logger, DMPrincipal.ConnectionParams(ConfigFilePath))) then begin
    Logger.LogOneLine('DB CONECTADO COM SUCESSO');
  end else begin
    Logger.LogOneLine('ERRO AO TENTAR CONEXÃO COM O BANCO DE DADOS');
  end;
end;

initialization
  DMPrincipal := Nil;

end.

