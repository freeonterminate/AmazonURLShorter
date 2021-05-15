unit uMain;

interface

uses
  System.SysUtils
  , System.Types
  , System.Classes
  , System.ImageList
  , System.UITypes
  , FMX.Controls
  , FMX.Controls.Presentation
  , FMX.Forms
  , FMX.ImgList
  , FMX.Layouts
  , FMX.Menus
  , FMX.Objects
  , FMX.StdCtrls
  , FMX.Types
  , PK.Clipboard.Watcher
  , PK.TrayIcon
  , PK.Utils.Form
  , uIniManager
  ;

type
  TfmmMain = class(TForm)
    layoutBase: TLayout;
    imgIcon: TImage;
    layoutTextBase: TLayout;
    lblTitle: TLabel;
    lblCopyright: TLabel;
    lblVersion: TLabel;
    styleDark: TStyleBook;
    timerHide: TTimer;
    imglstIcon: TImageList;
    popupMenu: TPopupMenu;
    menuCustomize: TMenuItem;
    menuExit: TMenuItem;
    menuInfo: TMenuItem;
    menuNoItem: TMenuItem;
    menuSep1: TMenuItem;
    menuSep2: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure menuExitClick(Sender: TObject);
    procedure timerHideTimer(Sender: TObject);
    procedure menuCustomizeClick(Sender: TObject);
    procedure menuInfoClick(Sender: TObject);
  private var
    FTrayIcon: TTrayIcon;
    FFormUtils: TFormUtils;
    FWatcher: TClipboardWatcher;
    FURLMenuStartIndex: Integer;
    FUpdateTrayMenuCount: Integer;
  private
    procedure CreateItem(const AURL, ATitle: String);
    procedure SetItemTitle(const AItem: TMenuItem; const AURL, ATitle: String);
    procedure ClipboardWatcherChangeHandler(const AText: String);
    procedure BeginUpdateTrayMenu;
    procedure EndUpdateTrayMenu;
    procedure UpdateTrayMenu;
    procedure RebuildURLMenu;
    procedure UpdateIniManagerURL;
    procedure MenuURLItemClickHandler(Sender: TObject);
  public
    function CanShow: Boolean; override;
  public
  end;

var
  fmmMain: TfmmMain;

implementation

uses
  System.Generics.Collections
  , System.Net.HttpClient
  , System.NetEncoding
  , FMX.Graphics
  , FMX.Helpers.Win
  , PK.Net.Amazon
  , PK.Utils.Application
  , PK.Utils.Browser
  , PK.Utils.ImageListHelper
  , PK.Utils.KeyCodeHelper
  , uConsts
  , uCustomize
  , uInfoWindow
  , uLangModule
  ;

{$R *.fmx}

procedure TfmmMain.BeginUpdateTrayMenu;
begin
  Inc(FUpdateTrayMenuCount);
end;

function TfmmMain.CanShow: Boolean;
begin
  Result := timerHide.Enabled;
end;

procedure TfmmMain.ClipboardWatcherChangeHandler(const AText: String);
begin
  AddLog('Change clipboard');

  if TAmazonUtils.IsAmazonURL(AText) then
  begin
    AddLog('Amazon URL: ' + AText);
    var URL := TAmazonUtils.ToShort(AText);
    AddLog('Short: "' + URL + '"');

    if (URL <> AText) and (not URL.IsEmpty) then
    begin
      FWatcher.SetTextToClip(URL);
      AddLog('Set clipboard: "' + URL + '"');

      var H := popupMenu.ItemsCount - 1;
      for var i := 0 to H do
      begin
        var Item := popupMenu.Items[i];
        if Item.TagString = URL then
        begin
          Item.Index := 0;
          UpdateTrayMenu;
          AddLog('Already exist: ' + Item.Text);
          Exit;
        end;
      end;

      CreateItem(URL, '');
    end;
  end;
end;

procedure TfmmMain.CreateItem(const AURL, ATitle: String);
begin
  if AURL.IsEmpty then
    Exit;

  var Item := TMenuItem.Create(Self);

  popupMenu.InsertObject(0, Item);
  Item.OnClick := MenuURLItemClickHandler;

  if (menuNoItem <> nil) and (menuNoItem.Parent <> nil) then
    menuNoItem.Parent := nil;

  if
    popupMenu.ItemsCount >= (FURLMenuStartIndex + TIniManager.Current.MaxCount)
  then
    popupMenu.Items[popupMenu.ItemsCount - FURLMenuStartIndex].Free;

  if ATitle.IsEmpty then
  begin
    AddLog('Load title: "' + AURL + '"');

    Item.Text := AURL;
    Item.TagString := AURL;

    UpdateTrayMenu;

    TThread.CreateAnonymousThread(
      procedure
      begin
        // title タグの中身を取り出す
        var Http := THttpClient.Create;
        try
          Http.ConnectionTimeout := TConsts.HTTP_TIMEOUT;
          Http.SendTimeout := TConsts.HTTP_TIMEOUT;
          Http.ResponseTimeout := TConsts.HTTP_TIMEOUT;

          var Source := Http.Get(AURL).ContentAsString;

          var SIndex := Source.IndexOf(TAmazonConsts.TITLE_S);
          var EIndex := Source.IndexOf(TAmazonConsts.TITLE_E);
          if EIndex <= SIndex then
            Exit;

          var Title :=
            Source.SubString(
              SIndex +
              TAmazonConsts.TITLE_S_LEN,
              EIndex - SIndex
            );

          var S := 0;

          for var Str in TAmazonConsts.STARTS_WITH do
            if Title.StartsWith(Str) then
            begin
              S := Length(Str);
              Break;
            end;

          var E := '';
          if Title.Length > TAmazonConsts.TITLE_MAX_LEN then
            E := TMenuConsts.ELLIPSIS;

          Title := Title.Substring(S, TAmazonConsts.TITLE_MAX_LEN).Trim + E;

          TThread.Queue(
            nil,
            procedure
            begin
              SetItemTitle(Item, AURL, Title);
              AddLog('Title loaded: "' + Title + '"');
            end
          );
        finally
          Http.Free;
        end;
      end
    ).Start;
  end
  else
    SetItemTitle(Item, AURL, ATitle);
end;

procedure TfmmMain.EndUpdateTrayMenu;
begin
  Dec(FUpdateTrayMenuCount);
  if FUpdateTrayMenuCount = 0 then
    UpdateTrayMenu;
end;

procedure TfmmMain.FormCreate(Sender: TObject);
begin
  BeginUpdate;
  try
    Width := TMainFormConsts.WIDTH;
    Height := TMainFormConsts.HEIGHT;
    TFormUtils.SetMainScreenCenter(Self);

    lblVersion.Text := TMainFormConsts.VERSION_TEXT + Application.Version;
  finally
    EndUpdate;
  end;

  TmoduleLang.SetLangToMenu(popupMenu);
  FURLMenuStartIndex := popupMenu.ItemsCount;

  FFormUtils := TFormUtils.Create;
  FFormUtils.HideTaskbar;

  FTrayIcon := TTrayIcon.Create;
  FTrayIcon.SetLButtonAsRButton(True);

  var Bmp := TBitmap.Create;
  try
    if imglstIcon.GetBitmap(TMenuConsts.TRAY_ICON_NAME, Bmp) then
    begin
      FTrayIcon.RegisterIcon(TMenuConsts.TRAY_ICON_NAME, Bmp);
      FTrayIcon.ChangeIcon(
        TMenuConsts.TRAY_ICON_NAME,
        TMenuConsts.TRAY_APP_NAME);
    end;
  finally
    Bmp.Free;
  end;

  FTrayIcon.Apply;

  FWatcher := TClipboardWatcher.Create;
  FWatcher.OnChange := ClipboardWatcherChangeHandler;

  menuInfo.Visible := TIniManager.Current.Debug;

  RebuildURLMenu;
end;

procedure TfmmMain.FormDestroy(Sender: TObject);
begin
  UpdateIniManagerURL;

  FTrayIcon.Free;
  FFormUtils.Free;
  FWatcher.Free;
end;

procedure TfmmMain.menuCustomizeClick(Sender: TObject);
begin
  if TfrmCustomize.IsShown then
    TfrmCustomize.BringToFrontSelf
  else
  begin
    UpdateIniManagerURL;
    TIniManager.Current.Save;

    var C := TIniManager.Current.Count;

    TfrmCustomize.ShowCustomize(styleDark);

    if TIniManager.Current.Count <> C then
      RebuildURLMenu;
  end;
end;

procedure TfmmMain.menuExitClick(Sender: TObject);
begin
  Close;
end;

procedure TfmmMain.menuInfoClick(Sender: TObject);
begin
  if TIniManager.Current.Debug then
    TfrmInfoWindow.ShowSelf(styleDark, nil);
end;

procedure TfmmMain.MenuURLItemClickHandler(Sender: TObject);
var
  Item: TMenuItem absolute Sender;
begin
  if not (Sender is TMenuItem) then
    Exit;

  var URL := Item.TagString;

  var IsRev := False;
  case TIniManager.Current.ActionType of
    TIniManager.TActionType.atOpen:
      IsRev := True;
  end;

  if TKeyCodeHelper.IsControlPressed xor IsRev then
    TBrowserUtils.Open(URL)
  else
    FWatcher.SetTextToClip(URL);
end;

procedure TfmmMain.RebuildURLMenu;
begin
  BeginUpdateTrayMenu;
  try
    var Menus := TList<TMenuItem>.Create;
    try
      for var i := 0 to popupMenu.ItemsCount - 1 do
      begin
        var Item := popupMenu.Items[i];
        if Item.Name = '' then
          Menus.Add(Item);
      end;

      for var Item in Menus do
        Item.Free;
    finally
      Menus.Free;
    end;

    var C := TIniManager.Current.Count;
    if C = 0 then
    begin
      if menuNoItem.Parent = nil then
      begin
        popupMenu.InsertObject(0, menuNoItem);
        menuNoItem.Enabled := False;
      end;
    end
    else
      for var i := C - 1 downto 0 do
      begin
        var Info := TIniManager.Current.Items[i];
        CreateItem(Info.URL, Info.Title);
      end;
  finally
    EndUpdateTrayMenu;
  end;
end;

procedure TfmmMain.SetItemTitle(
  const AItem: TMenuItem;
  const AURL, ATitle: String);
begin
  var Title := TNetEncoding.HTML.Decode(ATitle.Trim);

  AItem.Text := Format(TMenuConsts.URL_FORMAT, [Title, AURL]);
  AItem.TagString := AURL;
  AItem.Hint := Title;
  UpdateTrayMenu;
end;

procedure TfmmMain.timerHideTimer(Sender: TObject);
begin
  timerHide.Enabled := False;
  Visible := False;
end;

procedure TfmmMain.UpdateIniManagerURL;
begin
  TIniManager.Current.Clear;

  var H := popupMenu.ItemsCount - 1;
  for var i := 0 to H do
  begin
    var Item := popupMenu.Items[i];
    if Item.TagString.IsEmpty then
      Break;

    TIniManager.Current.Items[i] :=
      TURLInfo.Create(Item.TagString, Item.Hint);
  end;
end;

procedure TfmmMain.UpdateTrayMenu;
begin
  if FUpdateTrayMenuCount = 0 then
    FTrayIcon.AssignPopupMenu(popupMenu);
end;

end.
