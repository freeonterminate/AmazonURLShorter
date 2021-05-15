unit uConsts;

interface

uses
  uIniManager;

type
  TConsts = record
  public const
    HTTP_TIMEOUT = 10 * 1000;
  end;

  TIniConsts = record
  public const
    COMPANY_NAME = 'pik';

    EXT = '.ini';
    SEC_SYSTEM = 'system';
    KEY_DEBUG = 'debug';
    KEY_ACTION_TYPE = 'action_type';
    SEC_URLLIST = 'urllist';
    KEY_COUNT = 'count';
    KEY_COUNT_MAX = 'count_max';
    KEY_ITEM = 'item%d';
    DEF_ACTION_TYPE = Ord(TIniManager.TActionType.atCopy);
    DEF_MAX_COUNT = 12;

    URL_SEP_CHAR = '***';
    URL_SEP_CHAR_LEN = Length(URL_SEP_CHAR);

    REG_KEY = 'AmzURLShorter';
  end;

  TMenuConsts = record
  public const
    NO_ITEM = '(NO URL)';
    SEP = '-';
    EXIT = 'Exit';
    URL_FORMAT = '%s | %s';

    ELLIPSIS = '...';

    TRAY_ICON_NAME = 'Tray';
    TRAY_APP_NAME = 'Amz URL Shorter';
  end;

  TLogConsts = record
  public const
    DATEFORMAT = 'yyyy/mm/dd hh:nn:ss';
    MAX_COUNT = 128;
  end;

  TMainFormConsts = record
  public const
    WIDTH = 370;
    HEIGHT = 144;
    VERSION_TEXT = 'version ';
  end;

  TAmazonConsts = record
  public const
    TITLE_S = '<title>';
    TITLE_E = '</title>';
    TITLE_S_LEN = Length(TITLE_S);
    TITLE_MAX_LEN = 32;

    STARTS_WITH: array [0.. 2] of String = (
      'Amazon |',
      'Amazon.co.jp:',
      'Amazon.co.jp：'
    );
  end;

  TMessageConsts = record
  private const
    // English
    EN_HISTORY_CLEAR =
      'Do you want to delete the URL history?' + sLineBreak +
      'This operation is irreversible.';
    EN_HISTORY_CLEARED = 'URL history removed.';
    EN_ARE_YOU_SURE = 'Are you sure?';
    // Japanese
    JA_HISTORY_CLEAR =
      'URL 履歴を消去しますか？' + sLineBreak +
      'この操作は取り消せません';
    JA_HISTORY_CLEARED = 'URL 履歴を消去しました';
    JA_ARE_YOU_SURE = '本当に実行しますか？';
  private class var
    FHistoryClear: String;
    FHistoryCleared: String;
    FAreYouSure: String;
  private
    class procedure Init; static;
  public
    class property HistoryClear: String read FHistoryClear;
    class property HistoryCleared: String read FHistoryCleared;
    class property AreYouSure: String read FAreYouSure;
  end;

implementation

uses
  System.SysUtils
  , FMX.Platform
  ;

{ TMessageConsts }

class procedure TMessageConsts.Init;
const
  LANG_JA = 'ja';
begin
  var LangId := '';

  var Svc: IFMXLocaleService;
  if
    TPlatformServices.Current.SupportsPlatformService(IFMXLocaleService, Svc)
  then
    LangId := Svc.GetCurrentLangID.ToLower;

  if LangId = LANG_JA then
  begin
    FHistoryClear := JA_HISTORY_CLEAR;
    FHistoryCleared := JA_HISTORY_CLEARED;
    FAreYouSure := JA_ARE_YOU_SURE;
  end
  else
  begin
    FHistoryClear := EN_HISTORY_CLEAR;
    FHistoryCleared := EN_HISTORY_CLEARED;
    FAreYouSure := EN_ARE_YOU_SURE;
  end;
end;

initialization
  TMessageConsts.Init;

end.
