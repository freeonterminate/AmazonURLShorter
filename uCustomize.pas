unit uCustomize;

interface

uses
  System.SysUtils
  , System.Types
  , System.UITypes
  , System.Classes
  , FMX.Types
  , FMX.Controls
  , FMX.Forms
  , FMX.Edit
  , FMX.EditBox
  , FMX.SpinBox
  , FMX.StdCtrls
  , FMX.Controls.Presentation
  , FMX.Layouts
  ;

type
  TfrmCustomize = class(TForm)
    layoutRoot: TLayout;
    layoutBase: TLayout;
    lblPreferenceTitle: TLabel;
    layoutButtonBase: TLayout;
    btnOK: TButton;
    btnCancel: TButton;
    grpbxMenu: TGroupBox;
    grpbxClickType: TGroupBox;
    rdoActionTypeCopy: TRadioButton;
    rdoActionTypeOpen: TRadioButton;
    lblHistoryCount: TLabel;
    spinHistoryCount: TSpinBox;
    layoutHistoryCountBase: TLayout;
    layoutHistoryClearButtonBase: TLayout;
    btnHistoryClear: TButton;
    layoutAutoRunBase: TLayout;
    chbxAutoRun: TCheckBox;
    grpbxSystem: TGroupBox;
    procedure btnHistoryClearClick(Sender: TObject);
  private class var
    FCurrent: TfrmCustomize;
    FIsShown: Boolean;
  public
    class function ShowCustomize(const iStyleBook: TStyleBook): Boolean;
    class procedure BringToFrontSelf;
    class property IsShown: Boolean read FIsShown;
  end;

implementation

uses
  PK.Utils.Dialogs
  , PK.Utils.Form
  , uConsts
  , uIniManager
  ;

{$R *.fmx}

{ TfrmCustomize }

class procedure TfrmCustomize.BringToFrontSelf;
begin
  if FCurrent <> nil then
  begin
    var U := TFormUtils.Create;
    try
      U.BringToFront(FCurrent);
    finally
      U.Free;
    end;
  end;
end;

procedure TfrmCustomize.btnHistoryClearClick(Sender: TObject);
begin
  QueryYesNoDialog(
    TMessageConsts.HistoryClear,
    procedure (const iOK: Boolean)
    begin
      if iOK then
      begin
        TIniManager.Current.Clear;
        ShowInfoDialog(TMessageConsts.HistoryCleared);
      end;
    end
  );
end;

class function TfrmCustomize.ShowCustomize(
  const iStyleBook: TStyleBook): Boolean;
begin
  FCurrent := TfrmCustomize.Create(nil);
  try
    FCurrent.StyleBook := iStyleBook;
    TFormUtils.SetMainScreenCenter(FCurrent);

    // 設定読み込み
    FCurrent.spinHistoryCount.Value := TIniManager.Current.MaxCount;

    case TIniManager.Current.ActionType of
      TIniManager.TActionType.atCopy:
        FCurrent.rdoActionTypeCopy.IsChecked := True;

      TIniManager.TActionType.atOpen:
        FCurrent.rdoActionTypeOpen.IsChecked := True;
    end;

    FCurrent.chbxAutoRun.IsChecked := TIniManager.Current.AutoRun;

    // 表示
    FIsShown := True;
    try
      Result := FCurrent.ShowModal = mrOK;
    finally
      FIsShown := False;
    end;

    // 設定書き込み
    if Result then
    begin
      TIniManager.Current.MaxCount := Trunc(FCurrent.spinHistoryCount.Value);

      if FCurrent.rdoActionTypeCopy.IsChecked then
        TIniManager.Current.ActionType := TIniManager.TActionType.atCopy;

      if FCurrent.rdoActionTypeOpen.IsChecked then
        TIniManager.Current.ActionType := TIniManager.TActionType.atOpen;

      TIniManager.Current.AutoRun := FCurrent.chbxAutoRun.IsChecked;

      TIniManager.Current.Save;
    end;
  finally
    FreeAndNil(FCurrent);
  end;
end;

end.
