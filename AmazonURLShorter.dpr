program AmazonURLShorter;

uses
  System.StartUpCopy,
  FMX.Forms,
  uConsts in 'uConsts.pas',
  uCustomize in 'uCustomize.pas' {frmCustomize},
  uMain in 'uMain.pas' {fmmMain},
  uInfoWindow in 'uInfoWindow.pas' {frmInfoWindow},
  uIniManager in 'uIniManager.pas',
  uLangModule in 'uLangModule.pas' {moduleLang: TDataModule},
  PK.AutoRun in 'Lib\AutoRun\PK.AutoRun.Pas',
  PK.Clipboard.Watcher in 'Lib\Clipboard\Watcher\PK.Clipboard.Watcher.pas',
  PK.Net.Amazon in 'Lib\Net\PK.Net.Amazon.pas',
  PK.TrayIcon in 'Lib\TrayIcon\PK.TrayIcon.pas',
  PK.Utils.Application in 'Lib\Utils\PK.Utils.Application.pas',
  PK.Utils.Browser in 'Lib\Utils\PK.Utils.Browser.pas',
  PK.Utils.DateTime in 'Lib\Utils\PK.Utils.DateTime.pas',
  PK.Utils.Dialogs in 'Lib\Utils\PK.Utils.Dialogs.pas',
  PK.Utils.Form in 'Lib\Utils\Form\PK.Utils.Form.pas',
  PK.Utils.Form.Win in 'Lib\Utils\Form\PK.Utils.Form.Win.pas',
  PK.Utils.ImageListHelper in 'Lib\Utils\PK.Utils.ImageListHelper.pas',
  PK.Utils.KeyCodeHelper in 'Lib\Utils\PK.Utils.KeyCodeHelper.pas',
  PK.Utils.ProhibitMultiExec in 'Lib\Utils\PK.Utils.ProhibitMultiExec.pas';

{$R *.res}

begin
  {$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := True;
  {$ENDIF}

  if RegisterInstance('{0B8DA50F-79E5-44F2-8760-6C894C2F2122}', False) then
    try
      Application.Initialize;
      Application.CreateForm(TfmmMain, fmmMain);
      Application.Run;
    finally
      UnregisterInstance;
    end;
end.
