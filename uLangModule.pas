unit uLangModule;

interface

uses
  System.SysUtils
  , System.Classes
  , FMX.Menus
  , FMX.Types;

type
  TmoduleLang = class(TDataModule)
    langJa: TLang;
  private class var
    FInstance: TmoduleLang;
  private
    class constructor CreateClass;
    class destructor DestroyClass;
  public
    class procedure SetLangToMenu(const iMenu: TPopupMenu);
  end;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}

{$R *.dfm}

{ TmoduleLang }

class constructor TmoduleLang.CreateClass;
begin
  FInstance := TmoduleLang.Create(nil);
end;

class destructor TmoduleLang.DestroyClass;
begin
  FInstance.Free;
end;

class procedure TmoduleLang.SetLangToMenu(const iMenu: TPopupMenu);

  procedure TranslateMenuText(const iMenuItem: TMenuItem);
  begin
    var Str := Translate(iMenuItem.Text);
    //var Str := FInstance.langJa.LangStr[iMenuItem.Text];
    if (Str.Length > 0) then
      iMenuItem.Text := Str;

    for var i := 0 to iMenuItem.ItemsCount - 1 do
      TranslateMenuText(iMenuItem.Items[i]);
  end;

begin
  for var i := 0 to iMenu.ItemsCount - 1 do
    TranslateMenuText(iMenu.Items[i]);
end;

end.
