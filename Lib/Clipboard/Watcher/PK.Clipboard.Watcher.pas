(*
 * Clipboard Watcher
 *
 * PLATFORMS
 *   Windows / macOS / Android
 *
 * LICENSE
 *   Copyright (c) 2021 HOSOKAWA Jun
 *   Released under the MIT license
 *   http://opensource.org/licenses/mit-license.php
 *
 * HISTORY
 *   2005/11/22  Ver 1.0.0  Release
 *   2021/04/30  Ver 2.0.0  FMX support
 *
 * Programmed by HOSOKAWA Jun (twitter: @pik)
 *)

unit PK.Clipboard.Watcher;

interface

uses
  PK.Clipboard.Watcher.Types;

type
  TClipboardWatcher = class
  private var
    FWatcher: IClipboardWatcher;
    FOnChange: TClipboardWatcherChangeEvent;
  private
    procedure ChangeHandler(const iText: String);
    procedure SetOnChange(const Value: TClipboardWatcherChangeEvent);
  public
    constructor Create; reintroduce;
    procedure SetTextToClip(const iText: String);
  public
    property OnChange: TClipboardWatcherChangeEvent
      read FOnChange write SetOnChange;
  end;

implementation

uses
  {$IFDEF MSWINDOWS}
  PK.Clipboard.Watcher.Win
  {$ENDIF}
  {$IFDEF OSX}
  PK.Clipboard.Watcher.Mac
  {$ENDIF}
  {$IFDEF ANDROID}
  PK.Clipboard.Watcher.Android
  {$ENDIF}
  , FMX.Platform
  ;

{ TClipboardWatcher }

procedure TClipboardWatcher.ChangeHandler(const iText: String);
begin
  if Assigned(FOnChange) then
    FOnChange(iText);
end;

constructor TClipboardWatcher.Create;
begin
  inherited;

  var Factory: IClipboardWatcherFactory := nil;

  if
    TPlatformServices.Current.SupportsPlatformService(
      IClipboardWatcherFactory,
      Factory)
  then
  begin
    FWatcher := Factory.CreateClipboardWatcher;

    if FWatcher <> nil then
      FWatcher.SetChangeHandler(ChangeHandler)
  end;
end;

procedure TClipboardWatcher.SetOnChange(
  const Value: TClipboardWatcherChangeEvent);
begin
  FOnChange := Value;
end;

procedure TClipboardWatcher.SetTextToClip(const iText: String);
begin
  if FWatcher <> nil then
    FWatcher.SetTextToClip(iText);
end;

end.
