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
 *     FTray.AddMenu('Foo', FooClick);    // Right Click Menu
 *     FTray.RegisterIcon('Bar', BarBmp); // BarBmp is TBitmap Instance
 *     FTray.RegisterOnClick(TrayClick);  // TrayIcon Clicked Event (Win Only)
 *     FTray.Apply('{FED0B67E-7C6E-41C3-A257-E4EB588E3E94}');
 *   end;
 *
 * 2018/04/17 Version 1.0.0
 * Programmed by HOSOKAWA Jun (twitter: @pik)
 *)

unit PK.TrayIcon;

interface

uses
  System.SysUtils
  , System.Classes
  , FMX.Graphics
  , FMX.Menus
  , FMX.Types
  , PK.TrayIcon.Default
  ;

type
  TTrayIcon = class
  private var
    FTrayIcon: ITrayIcon;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
    procedure Apply(const iGUID: TGUID); overload;
    procedure Apply(const iGUIDString: String); overload;
    procedure Apply; overload;
    procedure AssignPopupMenu(const iPopupMenu: TPopupMenu);
    procedure AddMenu(
      const iText: String;
      const iData: Pointer;
      const iEvent: TNotifyEvent);
    procedure EnableMenu(const iText: String; const iEnabled: Boolean);
    procedure ClearMenus;
    procedure RegisterOnClick(const iEvent: TMouseEvent);
    procedure RegisterIcon(const iName: String; const iIcon: TBitmap);
    procedure ChangeIcon(const iName, iHint: String);
    procedure SetLButtonAsRButton(const iEnabled: Boolean);
  end;

implementation

uses
  FMX.Platform
  {$IFDEF MSWINDOWS}
  , PK.TrayIcon.Win
  {$ENDIF}
  {$IFDEF OSX}
  , PK.TrayIcon.Mac
  {$ENDIF}
  ;

{ TTrayIcon }

procedure TTrayIcon.AddMenu(
  const iText: String;
  const iData: Pointer;
  const iEvent: TNotifyEvent);
begin
  if FTrayIcon <> nil then
    FTrayIcon.AddMenu(iText, iData, iEvent);
end;

procedure TTrayIcon.Apply(const iGUID: TGUID);
begin
  if FTrayIcon <> nil then
    FTrayIcon.ApplyGUID(iGUID);
end;

procedure TTrayIcon.Apply;
begin
  if FTrayIcon <> nil then
    FTrayIcon.Apply;
end;

procedure TTrayIcon.Apply(const iGUIDString: String);
begin
  Apply(TGUID.Create(iGUIDString));
end;

procedure TTrayIcon.AssignPopupMenu(const iPopupMenu: TPopupMenu);
begin
  ClearMenus;

  for var i := 0 to iPopupMenu.ItemsCount - 1 do
  begin
    var Item := iPopupMenu.Items[i];
    if not Item.Visible then
      Continue;

    AddMenu(Item.Text, Item, Item.OnClick);
    if not Item.Enabled then
      EnableMenu(Item.Text, False);
  end;
end;

procedure TTrayIcon.ChangeIcon(const iName, iHint: String);
begin
  if FTrayIcon <> nil then
    FTrayIcon.ChangeIcon(iName, iHint);
end;

procedure TTrayIcon.ClearMenus;
begin
  if FTrayIcon <> nil then
    FTrayIcon.ClearMenus;
end;

constructor TTrayIcon.Create;
var
  TrayIconFactory: ITrayIconFactory;
begin
  inherited Create;

  if
    (
      TPlatformServices.Current.SupportsPlatformService(
        ITrayIconFactory,
        IInterface(TrayIconFactory)
      )
    )
  then
    FTrayIcon := TrayIconFactory.CreateTrayIcon;
end;

destructor TTrayIcon.Destroy;
begin
  FTrayIcon := nil;

  inherited;
end;

procedure TTrayIcon.EnableMenu(const iText: String; const iEnabled: Boolean);
begin
  if FTrayIcon <> nil then
    FTrayIcon.EnableMenu(iText, iEnabled);
end;

procedure TTrayIcon.RegisterIcon(const iName: String; const iIcon: TBitmap);
begin
  if FTrayIcon <> nil then
    FTrayIcon.RegisterIcon(iName, iIcon);
end;

procedure TTrayIcon.RegisterOnClick(const iEvent: TMouseEvent);
begin
  if FTrayIcon <> nil then
    FTrayIcon.RegisterOnClick(iEvent);
end;

procedure TTrayIcon.SetLButtonAsRButton(const iEnabled: Boolean);
begin
  if FTrayIcon <> nil then
    FTrayIcon.SetLButtonAsRButton(iEnabled);
end;

end.
