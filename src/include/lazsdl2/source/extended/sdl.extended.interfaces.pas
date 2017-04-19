unit SDL.Extended.Interfaces;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SDL.Api.Types, SDL.Extended.Types;

type

  { IInternalProviderContainer }

  IInternalProviderContainer = interface
    ['{294B085A-66E1-44F8-8715-D22DB19D48FA}']
    function GetInternalProvider: TComponent;
  end;

  ILogger = interface
    ['{8EB480BC-8F2B-4DF9-BC60-CF4E49FEC2F9}']
    procedure Log(Level: TLogLevel; Msg: String);
  end;

  { IControl }

  IControl = interface
    ['{BF9BC628-9A8A-4E3A-96D1-2E228FB5FF0F}']
    function GetSharedContext(out Context: SDL_GLContext): Boolean; (** Get shared opengl context for resource loading *)
    function PutSharedContext(const Context: SDL_GLContext): Boolean; (** Put shared opengl context back for other to use *)
    function GetSDLWindow: PSDL_Window; (** Get window associated with control *)
    function GetLogger: ILogger; (** Logger for debug and information purpose *)
    function GetProvider: TComponent; (** SDL library provider *)
  end;

  { IWorker }

  IWorker = interface
    ['{14B5FAB3-9A49-46BA-83AF-6968ABDEAE51}']
    function Execute(const Control: IControl; var Data: Pointer): Boolean;
  end;

implementation

end.

