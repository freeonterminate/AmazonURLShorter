(*
 * PopupForm Fixer
 *   Always stay on top
 *
 * PLATFORMS
 *   Windows / macOS
 *
 * LICENSE
 *   Copyright (c) 2021 HOSOKAWA Jun
 *   Released under the MIT license
 *   http://opensource.org/licenses/mit-license.php
 *
 * 2021/03/28 Ver 1.0.0
 *
 * Programmed by HOSOKAWA Jun (twitter: @pik)
 *)

unit PK.Fix.PopupForm;

{$IFNDEF MSWINDOWS}
{$WARNINGS OFF}
interface
implementation
end.
{$ENDIF}

interface

implementation

uses
  Winapi.Windows
  , Winapi.Messages
  , System.Classes
  , System.Generics.Collections
  , System.Rtti
  , FMX.Types
  , FMX.Forms
  , FMX.Platform.Win
  ;

var
  GHook: HHOOK;
  GSubclassed: TDictionary<hWnd, Pointer>;

function WndProc(
  hWnd: HWND;
  uMsg: DWORD;
  wParam: WPARAM;
  lParam: LPARAM): LRESULT; stdcall;
var
  OrgWndProc: Pointer;

  procedure CallOrg;
  begin
    Result := CallWindowProc(OrgWndProc, hWnd, uMsg, wParam, lParam);
  end;

begin
  OrgWndProc := nil;

  if (not GSubclassed.TryGetValue(hWnd, OrgWndProc)) then
    Exit(DefWindowProc(hWnd, uMsg, wParam, lParam));

  case uMsg of
    WM_WINDOWPOSCHANGING:
    begin
      with PWindowPos(lParam)^ do
      begin
        hwndInsertAfter := HWND_TOPMOST;
        CallOrg;
      end;
    end;
  else
    CallOrg;
  end;
end;

function HookProc(
  Code: Integer;
  wParam: WPARAM;
  lParam: LPARAM): LRESULT; stdcall;
var
  OrgWndProc: Pointer;
begin
  if (Code > -1) then
    with PCWPSTRUCT(lParam)^ do
      case message of
        WM_ACTIVATE:
        begin
          if not GSubclassed.ContainsKey(hWnd) then
          begin
            var Exists := False;
            for var i := 0 to Screen.PopupFormCount - 1 do
            begin
              var Wnd := FormToHWND(Screen.PopupForms[i]);
              if Wnd = hWnd then
              begin
                Exists := True;
                Break;
              end;
            end;

            if Exists then
            begin
              OrgWndProc :=
                Pointer(SetWindowLong(hWnd, GWL_WNDPROC, Integer(@WndProc)));

              if (OrgWndProc <> nil) then
                GSubclassed.Add(hWnd, OrgWndProc);
            end;
          end;
        end;

        WM_CLOSE:
        begin
          if GSubclassed.ContainsKey(hWnd) then
          begin
            OrgWndProc := GSubclassed[hWNd];
            SetWindowLong(hWnd, GWL_WNDPROC, Integer(@OrgWndProc));
            GSubclassed.Remove(hWnd);
          end;
        end;
      end;

  Result := CallNextHookEx(GHook, Code, wParam, lParam);
end;

initialization
begin
  GSubclassed := TDictionary<hWnd, Pointer>.Create;
  GHook := SetWindowsHookEx(WH_CALLWNDPROC, @HookProc, 0, GetCurrentThreadID);
end;

finalization
begin
  UnhookWindowsHookEx(GHook);
  GSubclassed.DisposeOf;
end;

end.
