(*
 * TrayIcon / StatusBar Icon Utility
 *
 * PLATFORMS
 *   Windows / macOS
 *
 * LICENSE
 *   Copyright (c) 2018 HOSOKAWA Jun
 *   Released under the MIT license
 *   http://opensource.org/licenses/mit-license.php
 *
 * HOW TO USE
 *   uses PK.TrayIcon;
 *
 *   type
 *     TForm1 = class(TForm)
 *       procedure FormCreate(Sender: TObject);
 *     private
 *       FTray: TTrayIcon;
 *     end;
 *
 *   procedure TForm1.FormCreate(Sender: TObject);
 *   begin
 *     FTray := TTrayIcon.Create;
 *
 *     // Right Click Menu
 *     // No.2 Param is UserData. Event Handlers Sender is No.2 param.
 *     FTray.AddMenu('Foo', nil, FooClick);
 *
 *     FTray.RegisterIcon('Bar', BarBmp); // BarBmp is TBitmap Instance
 *     FTray.RegisterOnClick(TrayClick);  // TrayIcon Clicked Event (Win Only)
 *
 *     FTray.Apply;
 *   end;
 *
 * 2018/04/17 Version 1.0.0
 * 2020/11/06 Version 1.1.0  Support Native PopupMenu / Eliminate VCL Components
 * Programmed by HOSOKAWA Jun (twitter: @pik)
 *)

unit PK.TrayIcon.Win;

{$IFNDEF MSWINDOWS}
{$WARNINGS OFF 1011}
interface
implementation
end.
{$ENDIF}

interface

implementation

uses
  Winapi.Windows
  , Winapi.Messages
  , Winapi.ShellAPI
  , System.Classes
  , System.Generics.Collections
  , System.SysUtils
  , System.UITypes
  , FMX.Forms
  , FMX.Graphics
  , FMX.Helpers.Win
  , FMX.Menus
  , FMX.Platform
  , FMX.Platform.Win
  , FMX.Surfaces
  , FMX.Types
  , PK.TrayIcon.Default
  ;

type
  TTrayIconWin = class(TInterfacedObject, ITrayIcon)
  private type
    TIconDic = TDictionary<String, HICON>;
    TMenuRec = record
      FID: Integer;
      FText: String;
      FData: Pointer;
      FHandler: TNotifyEvent;
    end;
    TMenuItemArray = TArray<TMenuRec>;
  private const
    IM_NOTIFY = WM_USER + 100;
  private var
    FHandle: HWND;
    FIcon: HICON;
    FHint: String;
    FGUID: TGUID;
    FLButtonAsRButton: Boolean;
    FVisible: Boolean;
    FMenu: HMENU;
    FMenuItems: TMenuItemArray;
    FIcons: TIconDic;
    FTaskbarRestart: NativeUInt;
    FPopuping: Boolean;
    FOnClick: TMouseEvent;
    FOnBeginPopup: TNotifyEvent;
    FOnEndPopup: TNotifyEvent;
  private
    function FindMenu(const iName: String): TMenuRec;
    function CreateIcon(const iIcon: TBitmap): HICON;
    procedure ClearIcon;
    procedure CreateMaskBitmap(const iMask, iSource: TBitmap);
    procedure InitNID(var ioNID: TNotifyIconData);
  protected
    procedure WndProc(var Msg: TMessage); virtual;
    procedure TaskTrayMessage(var ioMsg: TMessage); virtual;
    procedure SetVisible(const Value: Boolean); virtual;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
    procedure ApplyGUID(const iGUID: TGUID);
    procedure Apply;
    procedure AddMenu(
      const iText: String;
      const iData: Pointer;
      const iEvent: TNotifyEvent);
    procedure EnableMenu(const iText: String; const iEnabled: Boolean);
    procedure ClearMenus;
    procedure RegisterOnClick(const iEvent: TMouseEvent);
    procedure RegisterIcon(
      const iName: String;
      const iIcon: FMX.Graphics.TBitmap);
    procedure ChangeIcon(const iName, iHint: String);
    procedure SetLButtonAsRButton(const iEnabled: Boolean);
    property Visible: Boolean read FVisible write SetVisible;
    property Popuping: Boolean read FPopuping;
    property OnBeginPopup: TNotifyEvent read FOnBeginPopup write FOnBeginPopup;
    property OnEndPopup: TNotifyEvent read FOnEndPopup write FOnEndPopup;
  end;

  TTrayIconFactoryWin = class(TTrayIconFactory)
  public
    function CreateTrayIcon: ITrayIcon; override;
  end;

procedure RegisterTrayIconWin;
begin
  TPlatformServices.Current.AddPlatformService(
    ITrayIconFactory,
    TTrayIconFactoryWin.Create);
end;

{ TTrayIconWin }

procedure TTrayIconWin.AddMenu(
  const iText: String;
  const iData: Pointer;
  const iEvent: TNotifyEvent);
begin
  var Len := Length(FMenuItems);
  SetLength(FMenuItems, Len + 1);

  with FMenuItems[Len] do
  begin
    FID := Len + 1;
    FText := iText;
    FData := iData;
    FHandler := iEvent;

    if FText = '-' then
      AppendMenu(FMenu, MF_SEPARATOR, FID, nil)
    else
      AppendMenu(FMenu, MF_STRING or MF_ENABLED, FID, PChar(FText));
  end;
end;

procedure TTrayIconWin.Apply;
begin
  ApplyGUID(TGUID.Empty);
end;

procedure TTrayIconWin.ApplyGUID(const iGUID: TGUID);
begin
  FGUID := iGUID;
  SetVisible(True);
end;

procedure TTrayIconWin.ChangeIcon(const iName, iHint: String);
var
  Icon: HICON;
  NID: TNotifyICONData;
begin
  if FIcons.TryGetValue(iName, Icon) then
    FIcon := Icon;

  FHint := iHint;

  if FVisible then
  begin
    InitNID(NID);
    Shell_NotifyICON(NIM_MODIFY, @NID);
  end
  else
    SetVisible(True);
end;

procedure TTrayIconWin.ClearIcon;
begin
  SetVisible(False);

  for var Pair in FIcons do
    DestroyIcon(Pair.Value);

  FIcons.Clear;
end;

procedure TTrayIconWin.ClearMenus;
begin
  for var i := Low(FMenuItems) to High(FMenuItems) do
    DeleteMenu(FMenu, FMenuItems[i].FID, MF_BYCOMMAND);

  SetLength(FMenuItems, 0);
end;

constructor TTrayIconWin.Create;
begin
  inherited Create;

  FIcons := TIconDic.Create;
  FMenu := CreatePopupMenu;
  FHint := Application.Title;
  FHandle := AllocateHWnd(WndProc);
end;

function TTrayIconWin.CreateIcon(const iIcon: TBitmap): HICON;

  function ToHBitmap(const iBmp: TBitmap): HBITMAP;
  var
    Info: PBitmapInfo;
  begin
    var W: NativeUInt := iBmp.Width;
    var H: NativeUInt := iBmp.Height;

    GetMem(Info, SizeOf(TBitmapInfoHeader));
    try
      with Info^, bmiHeader do
      begin
        ZeroMemory(@bmiHeader, SizeOf(bmiHeader));

        biSize := SizeOf(bmiHeader);
        biWidth := iBmp.Width;
        biHeight := -iBmp.Height; // top down Bitmap
        biPlanes := 1;
        biBitCount := 32;
        biCompression := BI_RGB;
      end;

      var DIBW: NativeUInt := (W shl 2) and $fffffffc; // DIB Word 境界に合せる
      var Pixels := PByteArray(AllocMem(DIBW * H));
      try
        var Data: TBitmapData;
        iBmp.Map(TMapAccess.Read, Data);
        try
          for var Y := 0 to H - 1 do
            Move(Data.GetScanline(Y)^, Pixels[Y * DIBW], W * 4);
        finally
          iBmp.Unmap(Data);
        end;

        var DC := GetDC(0); // 0 = ScreenDC
        try
          Result :=
            CreateDIBitmap(
              DC,
              Info^.bmiHeader,
              CBM_INIT,
              Pixels,
              Info^,
              DIB_RGB_COLORS);
        finally
          ReleaseDC(0, DC);
        end;
      finally
        FreeMem(Pixels);
      end;
    finally
      FreeMem(Info);
    end;
  end;

begin
  var Mask := TBitmap.Create;
  try
    CreateMaskBitmap(Mask, iIcon);

    var IconInfo: TIconInfo;
    ZeroMemory(@IconInfo, SizeOf(IconInfo));

    try
      IconInfo.fIcon := True;
      IconInfo.hbmMask := ToHBitmap(Mask);
      IconInfo.hbmColor := ToHBitmap(iIcon);

      Result := CreateIconIndirect(IconInfo);
    finally
      DeleteObject(IconInfo.hbmMask);
      DeleteObject(IconInfo.hbmColor);
    end;
  finally
    Mask.DisposeOf;
  end;
end;

procedure TTrayIconWin.CreateMaskBitmap(const iMask, iSource: TBitmap);
const
  BW: array [Boolean] of UInt32 = ($00000000, $ffffffff);
var
  M, S: TBitmapData;
begin
  var W := iSource.Width;
  var H := iSource.Height;

  iMask.SetSize(W, H);

  iMask.Map(TMapAccess.Write, M);
  try
    iSource.Map(TMapAccess.Read, S);
    try
      for var Y := 0 to H - 1 do
        for var X := 0 to W - 1 do
          M.SetPixel(X, Y, BW[TAlphaColorRec(S.GetPixel(X, Y)).A = 0])
    finally
      iSource.Unmap(S);
    end;
  finally
    iMask.Unmap(M);
  end;
end;

destructor TTrayIconWin.Destroy;
begin
  ClearIcon;

  DestroyMenu(FMenu);
  FIcons.Free;

  inherited;
end;

procedure TTrayIconWin.EnableMenu(const iText: String; const iEnabled: Boolean);
const
  ENABLED: array [Boolean] of UInt32 = (MF_GRAYED, MF_ENABLED);
begin
  var Item := FindMenu(iText);
  if Item.FID > -1 then
    EnableMenuItem(
      FMenu,
      Item.FID,
      MF_STRING or MF_BYCOMMAND or ENABLED[iEnabled]
    );
end;

function TTrayIconWin.FindMenu(const iName: String): TMenuRec;
begin
  Result.FID := -1;

  for var Item in FMenuItems do
    if Item.FText = iName then
    begin
      Result := Item;
      Break;
    end
end;

procedure TTrayIconWin.InitNID(var ioNID: TNotifyIconData);
begin
  ZeroMemory(@ioNID, SizeOf(ioNID));
  with ioNID do
  begin
    cbSize := SizeOf;
    hIcon := FIcon;
    uFlags := NIF_ICON;
    uID := FHandle;
    Wnd := FHandle;

    if not FGUID.IsEmpty then
    begin
      guidItem := FGUID;
      uFlags := uFlags or NIF_GUID;
    end;

    if FHint <> '' then
    begin
      StrPLCopy(szTip, FHint, 63);
      uFlags := uFlags or NIF_TIP;
    end;
  end;
end;

procedure TTrayIconWin.RegisterIcon(
  const iName: String;
  const iIcon: FMX.Graphics.TBitmap);
begin
  if iIcon <> nil then
  begin
    var HI := CreateIcon(iIcon);
    if FIcon = 0 then
      FIcon := HI;

    FIcons.Add(iName, HI);
  end;
end;

procedure TTrayIconWin.RegisterOnClick(const iEvent: TMouseEvent);
begin
  FOnClick := iEvent;
end;

procedure TTrayIconWin.SetLButtonAsRButton(const iEnabled: Boolean);
begin
  FLButtonAsRButton := iEnabled;
end;

procedure TTrayIconWin.SetVisible(const Value: Boolean);
var
  NID: TNotifyICONData;
begin
  if FVisible = Value then
    Exit;

  FVisible := Value;

  try
    InitNID(NID);
    with NID do
    begin
      uCallbackMessage := IM_NOTIFY;
      uTimeout := 10000;
      uVersion := NOTIFYICON_VERSION_4;
      uFlags := uFlags or NIF_MESSAGE or NIM_SETVERSION;
    end;

    if (FVisible) then
      Shell_NotifyICON(NIM_ADD, @NID)
    else
      Shell_NotifyICON(NIM_DELETE, @NID);
  except
  end;
end;

procedure TTrayIconWin.TaskTrayMessage(var ioMsg: TMessage);
var
  tmpPos: TPoint;

  procedure CallOnClick;
  begin
    if Assigned(FOnClick) then
    begin
      FOnClick(
        Self,
        TMouseButton.mbLeft,
        MouseToShiftState(ioMsg.WParam),
        tmpPos.X,
        tmpPos.Y);
    end;
  end;

  procedure Popup;
  begin
    if Length(FMenuItems) > 0 then
    begin
      FPopuping := True;
      try
        SetForegroundWindow(FHandle);

        if (Assigned(FOnBeginPopup)) then
          FOnBeginPopup(Self);

        var Cmd :=
          NativeInt(
            TrackPopupMenu(
              FMenu,
              TPM_LEFTBUTTON or
                TPM_RIGHTBUTTON or
                TPM_RETURNCMD or
                TPM_BOTTOMALIGN or
                TPM_NOANIMATION,
              tmpPos.X,
              tmpPos.Y,
              0,
              FHandle,
              nil
            )
          );

        if (Assigned(FOnEndPopup)) then
          FOnEndPopup(Self);

        for var Item in FMenuItems do
          if (Item.FID = Cmd) and Assigned(Item.FHandler) then
          begin
            Item.FHandler(Item.FData);
            Break;
          end;
      finally
        FPopuping := False;
      end;
    end;
  end;

begin
  case ioMsg.lParam of
    WM_LBUTTONUP:
    begin
      GetCursorPos(tmpPos);
      CallOnClick;
      if FLButtonAsRButton then
        Popup;
    end;

    WM_RBUTTONUP:
    begin
      GetCursorPos(tmpPos);
      CallOnClick;
      Popup;
    end;
  end;
end;

procedure TTrayIconWin.WndProc(var Msg: TMessage);

  procedure Default;
  begin
    Msg.Result := DefWindowProc(FHandle, Msg.Msg, Msg.wParam, Msg.lParam);
  end;

begin
  case Msg.Msg of
    IM_NOTIFY:
      TaskTrayMessage(Msg);

    WM_CREATE:
    begin
      FTaskbarRestart := RegisterWindowMessage('TaskbarCreated');
    end;

    WM_ENDSESSION:
      SetVisible(False);

    WM_SYSCOMMAND:
      if (Msg.WParam = SC_CLOSE) then
        Msg.Result := 0
      else
        Default;
  else
    if Msg.Msg = FTaskbarRestart then
    begin
      if FVisible then
      begin
        FVisible := False;
        SetVisible(True);
      end;
    end
    else
      Default;
  end;
end;

{ TTrayIconFactoryWin }

function TTrayIconFactoryWin.CreateTrayIcon: ITrayIcon;
begin
  Result := TTrayIconWin.Create;
end;

initialization
  RegisterTrayIconWin;

end.
