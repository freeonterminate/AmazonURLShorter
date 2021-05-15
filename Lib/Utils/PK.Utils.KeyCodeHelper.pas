unit PK.Utils.KeyCodeHelper;

interface

type
  TKeyCodeHelper = record
  public
    class function IsControlV(const iKey: Word): Boolean; static;
    class function IsControlPressed: Boolean; static;
    class function IsShiftPressed: Boolean; static;
  end;

implementation

{$IFDEF MSWINDOWS}
uses
  Winapi.Windows;
{$ENDIF}

{ TKeyCodeHelper }

class function TKeyCodeHelper.IsControlPressed: Boolean;
begin
  {$IFDEF MSWINDOWS}
    Result := GetKeyState(VK_CONTROL) < 0;
  {$ELSE}
    Result := False;
  {$ENDIF}
end;

class function TKeyCodeHelper.IsControlV(const iKey: Word): Boolean;
begin
  {$IFDEF MSWINDOWS}
    Result := (iKey = 86); // Ctrl + V
  {$ELSE}
    {$IFDEF OSX}
      Result := (iKey = 86); // Cmd + V
    {$ELSE}
      Result := False
    {$ENDIF}
  {$ENDIF}
end;

class function TKeyCodeHelper.IsShiftPressed: Boolean;
begin
  {$IFDEF MSWINDOWS}
    Result := GetKeyState(VK_SHIFT) < 0;
  {$ELSE}
    Result := False;
  {$ENDIF}
end;

end.
