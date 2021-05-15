(*
 * Form Utils
 *
 * PLATFORMS
 *   Windows / macOS
 *
 * LICENSE
 *   Copyright (c) 2018, 2020 HOSOKAWA Jun
 *   Released under the MIT license
 *   http://opensource.org/licenses/mit-license.php
 *
 * 2018/04/08 Ver 1.0.0
 * 2018/05/13 Ver 1.0.1  CHG: Rename Show -> BringToFront
 * 2018/05/21 Ver 1.0.2  NEW: BringToFront overloaded version (Only Windows)
 * 2020/08/14 Ver 1.0.3  NEW: ShowTaskbarAndMainForm / HideTaskbarAndMainForm
 * 2020/08/18 Ver 1.0.4  NEW: HideMainForm
 *
 * Programmed by HOSOKAWA Jun (twitter: @pik)
 *)

unit PK.Utils.Form;

interface

uses
  FMX.Forms;

type
  TFormState = (Normal, Minimize);
  TFormStateNotifyProc = reference to procedure(const iState: TFormState);

  IFormUtils = interface
    ['{C7D3C846-1FA8-47E7-B0D4-F94D0022A85A}']
    // 同じキャプションを持ったウィンドウを探す
    function FindWindow(const iCaption: String): NativeInt;

    // Taskbar / Dock からアプリケーションを消す
    procedure HideTaskbar;

    // HideTasdkbar で消したアプリケーションを表示する
    procedure ShowTaskbar;

    // Taskbar / Dock から MainForm を消す
    procedure HideMainForm;

    // Form を表示し、最前面にする
    procedure BringToFront(const iForm: TCommonCustomForm); overload;

    // 指定された HWND を持つフォームを表示し最前面にする (Windows のみ)
    procedure BringToFront(const iWnd: NativeUInt); overload;

    // iForm が最小化されるときに呼ばれる処理を登録する
    procedure RegistMinimizeProc(
      const iForm: TCommonCustomForm;
      const iProc: TFormStateNotifyProc);

    // Taskbar / Dock のアイコンクリックで自動的に BringToFront が
    // 呼ばれるかどうか。
    // 基本的には Dock 用（Windows 版は常に True を返す）で、
    // HideMainForm してしまった MainForm の復帰用機能
    // このフラグが False の場合
    // applicationShouldHandleReopen:hasVisibleWindows:
    // を自分で実装して、BringToFront を呼ぶ必要がある
    function GetAutoRestore: Boolean;
    property AutoRestore: Boolean read GetAutoRestore;
  end;

  IFormUtilsFactory = interface
    ['{17320B02-74E5-4C1B-A64F-122FCD2EE896}']
    function CreateFormUtils: IFormUtils;
  end;

  TFormUtilsFactory = class(TInterfacedObject, IFormUtilsFactory)
  public
    function CreateFormUtils: IFormUtils; virtual; abstract;
  end;

  TFormUtils = class
  private var
    FFormUtils: IFormUtils;
  private
    function GetAutoRestore: Boolean;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
    function FindWindow(const iCaption: String): NativeInt;
    procedure HideTaskbar;
    procedure ShowTaskbar;
    procedure HideMainForm;
    procedure HideTaskbarAndMainForm;
    procedure ShowTaskbarAndMainForm;
    procedure BringToFront; overload;
    procedure BringToFront(const iForm: TCommonCustomForm); overload;
    procedure BringToFront(const iWnd: NativeUInt); overload;
    procedure RegisterMinimizeProc(
      const iForm: TCommonCustomForm;
      const iProc: TFormStateNotifyProc);
    property AutoRestore: Boolean read GetAutoRestore;
  public
    class procedure SetMainScreenCenter(const iForm: TCommonCustomForm);
  end;

implementation

uses
  FMX.Platform
  {$IFDEF MSWINDOWS}
  , PK.Utils.Form.Win
  {$ENDIF}
  {$IFDEF OSX}
  , PK.Utils.Form.Mac
  {$ENDIF}
  ;

{ TFormUtils }

procedure TFormUtils.BringToFront;
begin
  if FFormUtils <> nil then
    FFormUtils.BringToFront(Application.MainForm);
end;

procedure TFormUtils.BringToFront(const iWnd: NativeUInt);
begin
  if FFormUtils <> nil then
    FFormUtils.BringToFront(iWnd);
end;

procedure TFormUtils.BringToFront(const iForm: TCommonCustomForm);
begin
  if FFormUtils <> nil then
    FFormUtils.BringToFront(iForm);
end;

constructor TFormUtils.Create;
var
  FormUtilsFactory: IFormUtilsFactory;
begin
  inherited Create;

  if
    TPlatformServices.Current.SupportsPlatformService(
      IFormUtilsFactory,
      IInterface(FormUtilsFactory)
    )
  then
    FFormUtils := FormUtilsFactory.CreateFormUtils;
end;

destructor TFormUtils.Destroy;
begin
  FFormUtils := nil;

  inherited;
end;

function TFormUtils.FindWindow(const iCaption: String): NativeInt;
begin
  if FFormUtils = nil then
    Result := 0
  else
    Result := FFormUtils.FindWindow(iCaption);
end;

function TFormUtils.GetAutoRestore: Boolean;
begin
  Result := False;
  if FFormUtils <> nil then
    Result := FFormUtils.GetAutoRestore;
end;

procedure TFormUtils.HideMainForm;
begin
  if FFormUtils <> nil then
    FFormUtils.HideMainForm;
end;

procedure TFormUtils.HideTaskbar;
begin
  if FFormUtils <> nil then
    FFormUtils.HideTaskbar;
end;

procedure TFormUtils.HideTaskbarAndMainForm;
begin
  HideMainForm;
  HideTaskbar;
end;

procedure TFormUtils.RegisterMinimizeProc(
  const iForm: TCommonCustomForm;
  const iProc: TFormStateNotifyProc);
begin
  if FFormUtils <> nil then
    FFormUtils.RegistMinimizeProc(iForm, iProc);
end;

class procedure TFormUtils.SetMainScreenCenter(const iForm: TCommonCustomForm);
begin
  for var i := 0 to Screen.DisplayCount - 1 do
  begin
    var D := Screen.Displays[i];
    if D.Primary then
    begin
      var R := D.WorkArea;
      var X := R.Left + (R.Width - iForm.Width) div 2;
      var Y := R.Top + (R.Height - iForm.Height) div 2;

      iForm.SetBounds(X, Y, iForm.Width, iForm.Height);

      Break;
    end;
  end;
end;

procedure TFormUtils.ShowTaskbar;
begin
  if FFormUtils <> nil then
    FFormUtils.ShowTaskbar;
end;

procedure TFormUtils.ShowTaskbarAndMainForm;
begin
  if FFormUtils = nil then
    Exit;

  ShowTaskbar;
  BringToFront;
end;

end.
