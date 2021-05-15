(*
 * Application Utils
 *
 * PLATFORMS
 *   Windows / macOS
 *
 * LICENSE
 *   Copyright (c) 2018 HOSOKAWA Jun
 *   Released under the MIT license
 *   http://opensource.org/licenses/mit-license.php
 *
 * 2018/04/08 Version 1.0.0
 * Programmed by HOSOKAWA Jun (twitter: @pik)
 *)

unit PK.Utils.Application;

interface

uses
  FMX.Forms;

type
  TApplicationHelper = class helper for TApplication
  private
    function GetVersion: String;
  public
    function Path: String; // Path Only
    function ExeName: String; // Full Path with FileName
    property Version: String read GetVersion;
  end;


implementation

uses
  System.Classes
  , System.SysUtils
  , System.IOUtils
  {$IFDEF MSWINDOWS}
  {$ENDIF}
  {$IFDEF OSX}
  , Macapi.CoreFoundation
  {$ENDIF}
  ;

{ TApplicationHelper }

function TApplicationHelper.Path: String;
var
  SL: TStringList;
  Str: String;
  i: Integer;
  Count: Integer;
begin
  Result := '';

  SL := TStringList.Create;
  try
    SL.Text := ParamStr(0).Replace(TPath.DirectorySeparatorChar, sLineBreak);

    Count := SL.Count - 1;
    for i := 0 to Count do
    begin
      Str := SL[i];

      {$IFDEF MSWINDOWS}
      if i = Count then
        Break;
      {$ENDIF}

      {$IFDEF OSX}
      if Str.EndsWith('.app') then
        Break;
      {$ENDIF}

      Result := Result + Str + TPath.DirectorySeparatorChar;
    end;
  finally
    SL.DisposeOf;
  end;
end;

function TApplicationHelper.ExeName: String;
begin
  Result := ParamStr(0);
end;

function TApplicationHelper.GetVersion: String;
{$IFDEF MSWINDOWS}
var
  Major, Minor, Build: Cardinal;
{$ENDIF}
{$IFDEF OSX}
var
  CFStr: CFStringRef;
  Range: CFRange;
{$ENDIF}
begin
  Result := 'unknown';

{$IFDEF MSWINDOWS}
  if GetProductVersion(ExeName, Major, Minor, Build) then
    Result := Format('%d.%d.%d', [Major, Minor, Build]);
{$ENDIF}
{$IFDEF OSX}
  CFStr :=
    CFBundleGetValueForInfoDictionaryKey(
      CFBundleGetMainBundle,
      kCFBundleVersionKey
    );

  Range.location := 0;
  Range.length := CFStringGetLength(CFStr);
  SetLength(Result, Range.length);

  CFStringGetCharacters(CFStr, Range, PChar(Result));

  Result := Result.Trim;
{$ENDIF}
end;

end.
