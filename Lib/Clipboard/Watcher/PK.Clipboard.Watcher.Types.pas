(*
 * Clipboard Watcher Typess
 *)

unit PK.Clipboard.Watcher.Types;

interface

type
  TClipboardWatcherChangeEvent = procedure (const iText: String) of object;

  IClipboardWatcher = interface
  ['{DB861148-A06A-4018-8756-3E91CC5FDC24}']
    procedure SetChangeHandler(const iEvent: TClipboardWatcherChangeEvent);
    procedure SetTextToClip(const iText: String);
  end;

  IClipboardWatcherFactory = interface
    ['{B64BCB79-A270-4974-B7DE-ADC2B04D5713}']
    function CreateClipboardWatcher: IClipboardWatcher;
  end;

  TClipboardWatcherFactory = class(TInterfacedObject, IClipboardWatcherFactory)
  public
    function CreateClipboardWatcher: IClipboardWatcher; virtual; abstract;
  end;

implementation

end.
