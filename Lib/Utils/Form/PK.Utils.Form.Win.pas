(*
 * Form Utils
 *
 * PLATFORMS
 *   Windows
 *
 * LICENSE
 *   Copyright (c) 2018 HOSOKAWA Jun
 *   Released under the MIT license
 *   http://opensource.org/licenses/mit-license.php
 *
 * 2018/04/08 Version 1.0.0
 * 2018/05/13 Version 1.0.1  Rename Show -> BringToFront
 * 2018/05/21 Version 1.0.2  BringToFront overloaded (Only Windows)
 * Programmed by HOSOKAWA Jun (twitter: @pik)
 *)

unit PK.Utils.Form.Win;

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
  , System.Classes
  , System.SysUtils
  , System.Generics.Collections
  , FMX.Forms
  , FMX.Platform
  , FMX.Platform.Win
  , PK.Utils.Form
  ;

type
  TFormUtilsWin = class(TInterfacedObject, IFormUtils)
  private class var
    FFormUtilsList: TList<TFormUtilsWin>;
  private var
    FRegsitedForm: TCommonCustomForm;
    FRegistedProc: TFormStateNotifyProc;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
    function FindWindow(const iCaption: String): NativeInt;
    procedure HideTaskbar;
    procedure ShowTaskbar;
    procedure HideMainForm;
    procedure BringToFront(const iForm: TCommonCustomForm); overload;
    procedure BringToFront(const iWnd: NativeUInt); overload;
    procedure RegistMinimizeProc(
      const iForm: TCommonCustomForm;
      const iProc: TFormStateNotifyProc);
    function GetAutoRestore: Boolean;
  end;

  TFormUtilsFactoryWin = class(TFormUtilsFactory)
  public
    function CreateFormUtils: IFormUtils; override;
  end;

procedure RegisterFormUtilsWin;
begin
  TPlatformServices.Current.AddPlatformService(
    IFormUtilsFactory,
    TFormUtilsFactoryWin.Create);
end;

{ TFormUtilsWin }

//{$DEFINE NEED_ICONINC} // 本来はアイコン化しないと完全に最前面には出ない
procedure TFormUtilsWin.BringToFront(const iWnd: NativeUInt);
var
  ActiveThreadID: DWORD;
  TargetID: DWORD;
  {$IFDEF NEED_ICONIC}
  SelfIconic: Boolean;
  {$ENDIF}
begin
  {$IFDEF NEED_ICONIC}
  SelfIconic := not IsIconic(iWnd);
  if SelfIconic then
    SendMessage(iWnd, WM_SYSCOMMAND, SC_MINIMIZE, 0);
  {$ENDIF}

  ActiveThreadID := GetWindowThreadProcessId(GetForegroundWindow, nil);
  TargetID := GetWindowThreadProcessId(iWnd, nil);

  AttachThreadInput(TargetID, ActiveThreadID, True);
  try
    {$IFDEF NEED_ICONIC}
    if SelfIconic then
      OpenIcon(iWnd);
    {$ENDIF}

    SetWindowPos(
      iWnd,
      HWND_TOP,
      0, 0,
      0, 0,
      SWP_NOSIZE or SWP_NOMOVE or SWP_SHOWWINDOW);
  finally
    AttachThreadInput(TargetID, ActiveThreadID, False);
  end;
end;

procedure TFormUtilsWin.BringToFront(const iForm: TCommonCustomForm);
begin
  BringToFront(FormToHWND(iForm));
end;

constructor TFormUtilsWin.Create;
begin
  inherited Create;

  if FFormUtilsList = nil then
    FFormUtilsList := TList<TFormUtilsWin>.Create;

  FFormUtilsList.Add(Self);
end;

destructor TFormUtilsWin.Destroy;
begin
  FFormUtilsList.Remove(Self);
  if FFormUtilsList.Count < 1 then
  begin
    FFormUtilsList.DisposeOf;
    FFormUtilsList := nil;
  end;

  inherited;
end;

function TFormUtilsWin.FindWindow(const iCaption: String): NativeInt;
begin
  Result := Winapi.Windows.FindWindow(nil, PChar(iCaption));
end;

function TFormUtilsWin.GetAutoRestore: Boolean;
begin
  Result := True;
end;

procedure TFormUtilsWin.HideMainForm;
begin
  ShowWindow(FormToHWND(FRegsitedForm), SW_HIDE);
end;

procedure TFormUtilsWin.HideTaskbar;
begin
  ShowWindow(ApplicationHWND, SW_HIDE);
  HideMainForm;
end;

procedure TFormUtilsWin.RegistMinimizeProc(
  const iForm: TCommonCustomForm;
  const iProc: TFormStateNotifyProc);
begin
  FRegsitedForm := iForm;
  FRegistedProc := iProc;
end;

procedure TFormUtilsWin.ShowTaskbar;
begin
  ShowWindow(ApplicationHWND, SW_SHOW);
  ShowWindow(FormToHWND(FRegsitedForm), SW_SHOW);
end;

{ TFormUtilsFactoryWin }

function TFormUtilsFactoryWin.CreateFormUtils: IFormUtils;
begin
  Result := TFormUtilsWin.Create;
end;

var
  GHookHandle: THandle;

function WndProc(Code: integer; WParam, LParam: LongInt): LRESULT; stdcall;
var
  Msg: TCWPRetStruct;
  Utils: TFormUtilsWin;
begin;
  if
    (Code >= HC_ACTION) and
    (LParam > 0) and
    (TFormUtilsWin.FFormUtilsList <> nil)
  then
  begin
    Msg := PCWPRetStruct(LParam)^;

    case Msg.message of
      WM_SIZE:
      begin
        case Msg.WParam of
          SIZE_MINIMIZED:
          begin
            for Utils in TFormUtilsWin.FFormUtilsList do
              if
                (FormToHWND(Utils.FRegsitedForm) = Msg.hwnd) and
                (Assigned(Utils.FRegistedProc))
              then
                Utils.FRegistedProc(TFormState.Minimize);
          end;

          SIZE_RESTORED:
          begin
            for Utils in TFormUtilsWin.FFormUtilsList do
              if
                (FormToHWND(Utils.FRegsitedForm) = Msg.hwnd) and
                (Assigned(Utils.FRegistedProc))
              then
                Utils.FRegistedProc(TFormState.Normal);
          end;
        end;
      end;
    end;
  end;

  Result := CallNextHookEx(GHookHandle, Code, WParam, LParam)
end;

initialization
  RegisterFormUtilsWin;
  GHookHandle :=
    SetWindowsHookEx(WH_CALLWNDPROCRET, @WndProc, 0, GetCurrentThreadId);

finalization
  UnhookWindowsHookEx(GHookHandle);

end.
