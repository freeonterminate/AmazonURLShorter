(*
 * DateTime Utils
 *
 * PLATFORMS
 *   All
 *
 * LICENSE
 *   Copyright (c) 2018 HOSOKAWA Jun
 *   Released under the MIT license
 *   http://opensource.org/licenses/mit-license.php
 *
 * 2002/02/07 Version 1.0.0
 * Programmed by HOSOKAWA Jun (twitter: @pik)
 *)

 unit PK.Utils.DateTime;

interface

function Date2String(
  iDate: TDateTime;
  iFormat: String = 'yyyy/mm/dd hh:nn:ss'): String;
function String2Date(
  iStr: String;
  iFormat: String = 'yyyy/mm/dd hh:nn:ss'): TDateTime;
function TryStrToDateTimeEx(
  const iStr: String;
  out oDateTime: TDateTime): Boolean;

implementation

uses
  System.SysUtils;

function Date2String(
  iDate: TDateTime;
  iFormat: String = 'yyyy/mm/dd hh:nn:ss'): String;
begin
  Result := FormatDateTime(iFormat, iDate);
end;

function String2Date(
  iStr: String;
  iFormat: String = 'yyyy/mm/dd hh:nn:ss'): TDateTime;
var
  KeepShortDateFormat: String;
begin
  Result := 0;

  if (iStr = '') then
    Exit;

  KeepShortDateFormat := FormatSettings.ShortDateFormat;

  FormatSettings.ShortDateFormat := iFormat;

  if (not TryStrToDateTime(iStr, Result)) then
    Result := 0;

  FormatSettings.ShortDateFormat := KeepShortDateFormat;
end;

function TryStrToDateTimeEx(
  const iStr: String;
  out oDateTime: TDateTime): Boolean;
label
  Last;
const
  DateSeps: array [0.. 1] of Char = ('/', '-');
  TimeSeps: array [0.. 1] of Char = (':', '.');
var
  FS: TFormatSettings;
  i, j: Integer;
begin
  FS := TFormatSettings.Create;

  for i := Low(DateSeps) to High(DateSeps) do
  begin
    FS.DateSeparator := DateSeps[i];

    for j := Low(TimeSeps) to High(TimeSeps) do
    begin
      FS.TimeSeparator := TimeSeps[j];

      Result := TryStrToDateTime(iStr, oDateTime, FS);
      if (Result) then
        goto Last;
    end;
  end;

Last:
end;

end.
