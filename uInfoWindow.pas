unit uInfoWindow;

interface

uses
  System.Classes
  , System.SysUtils
  , System.Types
  , System.UITypes
  , FMX.Controls
  , FMX.Controls.Presentation
  , FMX.Dialogs
  , FMX.Forms
  , FMX.Layouts
  , FMX.Memo.Types
  , FMX.Memo
  , FMX.ScrollBox
  , FMX.StdCtrls
  , FMX.Types
  ;

type
  TfrmInfoWindow = class(TForm)
    memoLog: TMemo;
    layoutButtonBase: TLayout;
    btnClear: TButton;
    btnSave: TButton;
    btnClose: TButton;
    dlgSave: TSaveDialog;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnClearClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnCloseApplyStyleLookup(Sender: TObject);
  private
    procedure ScrollToLast;
  private class var
    FCloseProc: TProc;
    FInstance: TfrmInfoWindow;
  private
    class constructor CreateClass;
    class destructor DestroyClass;
  public
    class procedure ShowSelf(
      const iStyleBook: TStyleBook;
      const iProc: TProc);
  end;

procedure AddLog(const iMsg: String);

implementation

{$R *.fmx}

uses
  System.DateUtils
  , FMX.Effects
  , FMX.Platform.Win
  , PK.Utils.Dialogs
  , PK.Utils.Form
  , uConsts
  , uIniManager
  ;

procedure AddLog(const iMsg: String);
begin
  if not TIniManager.Current.Debug then
    Exit;

  var TimeStr := '';
  DateTimeToString(TimeStr, TLogConsts.DATEFORMAT, Now);

  TfrmInfoWindow.FInstance.memoLog.Lines.Add(TimeStr + ' ' + iMsg);

  while TfrmInfoWindow.FInstance.memoLog.Lines.Count > TLogConsts.MAX_COUNT do
    TfrmInfoWindow.FInstance.memoLog.Lines.Delete(0);

  TfrmInfoWindow.FInstance.ScrollToLast;
end;

{ TfrmInfoWindow }

procedure TfrmInfoWindow.btnClearClick(Sender: TObject);
begin
  QueryYesNoDialog(
    TMessageConsts.AreYouSure,
    procedure (const iOK: Boolean)
    begin
      if iOK then
        memoLog.Lines.Clear;
    end
  );
end;

procedure TfrmInfoWindow.btnCloseApplyStyleLookup(Sender: TObject);
begin
  btnClose.SetFocus;
end;

procedure TfrmInfoWindow.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmInfoWindow.btnSaveClick(Sender: TObject);
begin
  if dlgSave.Execute then
    memoLog.Lines.SaveToFile(dlgSave.FileName, TEncoding.UTF8);
end;

class constructor TfrmInfoWindow.CreateClass;
begin
  FInstance := TfrmInfoWindow.Create(nil);
end;

class destructor TfrmInfoWindow.DestroyClass;
begin
  FInstance.Free;
end;

procedure TfrmInfoWindow.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := TCloseAction. caHide;

  if Assigned(FCloseProc) then
    FCloseProc;
end;

procedure TfrmInfoWindow.ScrollToLast;
begin
  memoLog.ScrollBy(0, MaxInt);
end;

class procedure TfrmInfoWindow.ShowSelf(
  const iStyleBook: TStyleBook;
  const iProc: TProc);
begin
  FCloseProc := iProc;

  if FInstance.Visible then
  begin
    var Wnd := FormToHWND(FInstance);
    var U := TFormUtils.Create;
    try
      U.BringToFront(Wnd);
    finally
      U.Free;
    end;

    FInstance.ScrollToLast;
  end
  else
  begin
    FInstance.StyleBook := iStyleBook;
    TFormUtils.SetMainScreenCenter(FInstance);
    FInstance.Show;
    FInstance.ScrollToLast;
  end;
end;

end.
