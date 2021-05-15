unit uIniManager;

interface

uses
  System.SysUtils
  , System.IniFiles
  , PK.AutoRun
  ;

type
  TURLInfo = record
  private var
    FURL: String;
    FTitle: String;
  public
    class function CreateEmpty: TURLInfo; static;
    constructor Create(const AURL, ATitle: String); overload;
    constructor Create(const ACombinedStr: String); overload;
    function ToString: String;
    property URL: String read FURL;
    property Title: String read FTitle;
  end;

  TIniManager = class
  public type
    TActionType = (atCopy, atOpen);
  private class var
    FCurrent: TIniManager;
  private
    class constructor CreateClass;
    class destructor DestroyClass;
  private var
    FIni: TIniFile;
    FDebug: Boolean;
    FAutoRun: TAutoRun;
    FMaxCount: Integer;
    FActionType: TActionType;
    FItems: TArray<TURLInfo>;
  private
    function GetCount: Integer;
    function GetItems(const iIndex: Integer): TURLInfo;
    procedure SetItems(const iIndex: Integer; const iValue: TURLInfo);
    function GetAutoRun: Boolean;
    procedure SetAutoRun(const Value: Boolean);
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
    procedure Load;
    procedure Save;
    procedure Clear;
    property Debug: Boolean read FDebug;
    property MaxCount: Integer read FMaxCount write FMaxCount;
    property ActionType: TActionType read FActionType write FActionType;
    property AutoRun: Boolean read GetAutoRun write SetAutoRun;
    property Count: Integer read GetCount;
    property Items[const iIndex: Integer]: TURLInfo
      read GetItems write SetItems; default;
  public
    class property Current: TIniManager read FCurrent;
  end;

implementation

uses
  System.IOUtils
  , uConsts
  ;

{ TURLInfo }

constructor TURLInfo.Create(const AURL, ATitle: String);
begin
  FURL := AURL;
  FTItle := ATitle;
end;

constructor TURLInfo.Create(const ACombinedStr: String);
begin
  var Index := ACombinedStr.IndexOf(TIniConsts.URL_SEP_CHAR);
  if Index < 0 then
    Exit;

  FURL := ACombinedStr.Substring(0, Index);
  FTitle := ACombinedStr.Substring(Index + TIniConsts.URL_SEP_CHAR_LEN);
end;

class function TURLInfo.CreateEmpty: TURLInfo;
begin
  Result.FTitle := '';
  Result.FURL := '';
end;

function TURLInfo.ToString: String;
begin
  Result := FURL + TIniConsts.URL_SEP_CHAR + FTitle;
end;

{ TIniManager }

procedure TIniManager.Clear;
begin
  SetLength(FItems, 0);
end;

constructor TIniManager.Create;
begin
  inherited;

  var AppName := ChangeFileExt(ExtractFileName(ParamStr(0)), '');;
  var FileName :=
    String.Join(
      PathDelim,
      [TPath.GetHomePath, TIniConsts.COMPANY_NAME, AppName]
    );

  if (not FileName.EndsWith(PathDelim)) then
    FileName := FileName + PathDelim;

  if (not TDirectory.Exists(FileName)) then
    TDirectory.CreateDirectory(FileName);

  if (TDirectory.Exists(FileName)) then
    FileName := FileName + AppName + TIniConsts.EXT;

  FIni := TIniFile.Create(FileName);
  FAutoRun := TAutoRun.Create(TIniConsts.REG_KEY);

  Load;
end;

class constructor TIniManager.CreateClass;
begin
  FCurrent := TIniManager.Create;
end;

destructor TIniManager.Destroy;
begin
  Save;
  FAutoRun.Free;
  FIni.Free;

  inherited;
end;

class destructor TIniManager.DestroyClass;
begin
  FCurrent.Free;
end;

function TIniManager.GetAutoRun: Boolean;
begin
  Result := FAutoRun.Registered;
end;

function TIniManager.GetCount: Integer;
begin
  Result := Length(FItems);
end;

function TIniManager.GetItems(const iIndex: Integer): TURLInfo;
begin
  Result := TURLInfo.CreateEmpty;
  if (iIndex > -1) and (iIndex < GetCount) then
    Result := FItems[iIndex];
end;

procedure TIniManager.Load;
begin
  FDebug := FIni.ReadBool(TIniConsts.SEC_SYSTEM, TIniConsts.KEY_DEBUG, False);

  FMaxCount :=
    FIni.ReadInteger(
      TIniConsts.SEC_SYSTEM,
      TIniConsts.KEY_COUNT_MAX,
      TIniConsts.DEF_MAX_COUNT);

  FActionType :=
    TActionType(
      FIni.ReadInteger(
        TIniConsts.SEC_SYSTEM,
        TIniConsts.KEY_ACTION_TYPE,
        TIniConsts.DEF_ACTION_TYPE)
    );

  var C := FIni.ReadInteger(TIniConsts.SEC_URLLIST, TIniConsts.KEY_COUNT,0);

  var Index := 0;
  for var i := 0 to C - 1 do
  begin
    var CombinedText :=
      FIni.ReadString(
        TIniConsts.SEC_URLLIST,
        Format(TIniConsts.KEY_ITEM, [i]),
        '');

    if (CombinedText.IsEmpty) or (CombinedText = TIniConsts.URL_SEP_CHAR) then
      Continue;

    Items[Index] := TURLInfo.Create(CombinedText);
    Inc(Index);
  end;
end;

procedure TIniManager.Save;
begin
  FIni.WriteInteger(
    TIniConsts.SEC_SYSTEM,
    TIniConsts.KEY_COUNT_MAX,
    FMaxCount);

  FIni.WriteInteger(
    TIniConsts.SEC_SYSTEM,
    TIniConsts.KEY_ACTION_TYPE,
    Ord(FActionType));

  FIni.EraseSection(TIniConsts.SEC_URLLIST);

  var C := GetCount;

  FIni.WriteInteger(TIniConsts.SEC_URLLIST, TIniConsts.KEY_COUNT, C);

  var Index := 0;
  for var i := 0 to C - 1 do
  begin
    var Item := FItems[i];
    if Item.FURL.IsEmpty then
      Continue;

    FIni.WriteString(
      TIniConsts.SEC_URLLIST,
      Format(TIniConsts.KEY_ITEM, [Index]),
      Item.ToString);

    Inc(Index);
  end;
end;

procedure TIniManager.SetAutoRun(const Value: Boolean);
begin
  if FAutoRun.Registered <> Value then
  begin
    if Value then
      FAutoRun.Register
    else
      FAutoRun.Unregister;
  end;
end;

procedure TIniManager.SetItems(const iIndex: Integer; const iValue: TURLInfo);
begin
  var Index := iIndex;
  var C := GetCount;
  if (Index < 0) or (Index >= C) then
  begin
    Index := C;
    SetLength(FItems, C + 1);
  end;

  FItems[Index] := iValue;
end;

end.
