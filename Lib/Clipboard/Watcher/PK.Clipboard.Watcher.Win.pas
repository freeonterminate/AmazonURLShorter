(*
 * Clipboard Watcher Windows
 *)

unit PK.Clipboard.Watcher.Win;

interface

implementation

uses
  Winapi.Windows
  , Winapi.Messages
  , System.Classes
  , System.SysUtils
  , FMX.Platform
  , PK.Clipboard.Watcher.Types
  ;

type
  TClipboardWatcherWin = class(TInterfacedObject, IClipboardWatcher)
  private var
    FHandle: HWnd;
    FNextViewer: HWnd;
    FEnabled: Boolean;
    FChangeHandler: TClipboardWatcherChangeEvent;
  private
    { IClipboardWatcher }
    procedure SetChangeHandler(const iHandler: TClipboardWatcherChangeEvent);
    procedure SetTextToClip(const iText: String);
  private
    procedure WMDrawClipboard(var ioMsg: TWMDrawClipboard);
      message WM_DRAWCLIPBOARD;
    procedure WMChangeCBChain(var ioMsg: TWMChangeCBChain);
      message WM_CHANGECBCHAIN;
  protected
    procedure WndProc(var ioMsg: TMessage); virtual;
    procedure CallNextViewer(const iMsg: TMessage);
  public
    // Constructor & Destructor
    constructor Create; reintroduce; virtual;
    destructor Destroy; override;
  public
    // Properties
    property Handle: HWnd read FHandle;
    property Enabled: Boolean read FEnabled write FEnabled;
  end;

  TClipboardWatcherFactoryWin = class(TClipboardWatcherFactory)
  public
    function CreateClipboardWatcher: IClipboardWatcher; override;
  end;

procedure RegisterFormUtilsWin;
begin
  TPlatformServices.Current.AddPlatformService(
    IClipboardWatcherFactory,
    TClipboardWatcherFactoryWin.Create);
end;

{ TClipboardWatcherFactoryWin }

function TClipboardWatcherFactoryWin.CreateClipboardWatcher: IClipboardWatcher;
begin
  Result := TClipboardWatcherWin.Create;
end;

{TClipboardWatcherWin}

constructor TClipboardWatcherWin.Create;
begin
  inherited;

  FHandle := AllocateHWnd(WndProc);
  FNextViewer := SetClipboardViewer(FHandle);
  FEnabled := True;
end;

destructor TClipboardWatcherWin.Destroy;
begin
  ChangeClipboardChain(FHandle, FNextViewer);
  DeallocateHWnd(FHandle);

  inherited;
end;

procedure TClipboardWatcherWin.SetChangeHandler(
  const iHandler: TClipboardWatcherChangeEvent);
begin
  FChangeHandler := iHandler;
end;

procedure TClipboardWatcherWin.SetTextToClip(const iText: String);
begin
  var Keep := FEnabled;

  FEnabled := False;
  try
    if OpenClipboard(FHandle) then
    begin
      try
        EmptyClipboard;

        var Data :=
          GlobalAlloc(GMEM_MOVEABLE, Length(iText) * SizeOf(Char) + 1);
        if Data <> 0 then
        begin
          try
            var DataPtr := GlobalLock(Data);
            try
              Move(PChar(iText)^, DataPtr^, GlobalSize(Data));
              SetClipboardData(CF_UNICODETEXT, Data);
            finally
              GlobalUnlock(Data);
            end;
          except
            GlobalFree(Data);
          end;
        end;
      finally
        CloseClipboard;
      end;
    end;
  finally
    FEnabled := Keep;
  end;
end;

procedure TClipboardWatcherWin.WndProc(var ioMsg: TMessage);
begin
  try
    Dispatch(ioMsg);
  except
    DefWindowProc(FHandle, ioMsg.Msg, ioMsg.WParam, ioMsg.LParam);
  end;
end;

procedure TClipboardWatcherWin.CallNextViewer(const iMsg: TMessage);
begin
  if FNextViewer <> 0 then
    SendMessage(FNextViewer, iMsg.Msg, iMsg.WParam, iMsg.LParam);
end;

procedure TClipboardWatcherWin.WMChangeCBChain(var ioMsg: TWMChangeCBChain);
begin
  if (FNextViewer = ioMsg.Remove) then
    FNextViewer := ioMsg.Next
  else
    CallNextViewer(TMessage(ioMsg));

  ioMsg.Result := 0;
end;

procedure TClipboardWatcherWin.WMDrawClipboard(var ioMsg: TWMDrawClipboard);
type
  TPackedDWordArray = packed array of DWORD;
begin
  if FEnabled then
  begin
    var List: TPackedDWordArray := [CF_UNICODETEXT, CF_TEXT];
    var Format := GetPriorityClipboardFormat(List[0], Length(List));

    if Assigned(FChangeHandler) and (Format in [CF_UNICODETEXT, CF_TEXT]) then
    begin
      var Text := '';

      if OpenClipboard(FHandle) then
      begin
        try
          var Data := GetClipboardData(Format);
          if Data <> 0 then
          begin
            var TextData := GlobalLock(Data);
            try
              if Format = CF_UNICODETEXT then
                Text := PChar(TextData)
              else
                Text := String(PAnsiChar(TextData));
            finally
              GlobalUnlock(Data);
            end;
          end;
        finally
          CloseClipboard;
        end;
      end;

      if not Text.IsEmpty then
        FChangeHandler(Text);
    end;
  end;

  CallNextViewer(TMessage(ioMsg));
  ioMsg.Result := 0;
end;

initialization
  RegisterFormUtilsWin;

end.
