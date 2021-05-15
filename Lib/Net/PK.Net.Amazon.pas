(*
 * Amazon Utils
 *
 * PLATFORMS
 *   Windows / macOS / iOS / Android / Linux
 *
 * LICENSE
 *   Copyright (c) 2021 HOSOKAWA Jun
 *   Released under the MIT license
 *   http://opensource.org/licenses/mit-license.php
 *
 * HISTORY
 *   2021/01/31 Ver 1.0.0  Release
 *
 * Programmed by HOSOKAWA Jun (twitter: @pik)
 *)

unit PK.Net.Amazon;

interface

type
  TAmazonUtils = record
  public const
    AMAZON_HOST = 'amazon.co.jp';
    AMAZON_URL = 'https://' + AMAZON_HOST;
    AMAZON_DP = '/dp/';
    AMAZON_PATHS: array [0.. 2] of String = (
      AMAZON_DP,
      '/gp/product/',
      '/exec/obidos/ASIN/'
    );
  public
    class function IsAmazonURL(const AURL: String): Boolean; static;
    class function ToShort(const AURL: String): String; static;
  end;

implementation

uses
  System.SysUtils;

{ TAmazonUtils }

class function TAmazonUtils.IsAmazonURL(const AURL: String): Boolean;
begin
  Result := AURL.IndexOf(AMAZON_HOST) > -1;
end;

class function TAmazonUtils.ToShort(const AURL: String): String;
var
  URL: String;

  function CutNonCore(const ACore: String): Boolean;
  begin
    Result := False;

    var Index := URL.IndexOf(ACore);
    if Index > 0 then
    begin
      var Len := URL.Length;
      for var i := Index + ACore.Length to Len do
        if CharInSet(URL.Chars[i], ['?', '/']) then
        begin
          URL := URL.Substring(Index, i - Index);
          Result := True;
          Break;
        end;
    end;
  end;

begin
  Result := AURL;

  if not IsAmazonURL(AURL) then
    Exit;

  // /dp/XXXXXXXXXX のように最後に / が無いパターン用に / を追加
  URL := AURL + '/';

  for var Path in AMAZON_PATHS do
    if CutNonCore(Path) then
    begin
      Result := URL.Replace(Path, AMAZON_DP);
      Break;
    end;

  if Result <> AURL then
    Result := AMAZON_URL + Result;
end;

end.
