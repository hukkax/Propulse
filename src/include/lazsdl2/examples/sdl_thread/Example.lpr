program example;

uses
{$IFDEF UNIX}
  CThreads,
{$ENDIF}
  interfaces,
  SDL.Extended.Application, mainfm, sysutils
  { add your units here };

type

  { TMySDLApplication }

  TMySDLApplication = class(TSDL2Application)
  public
    procedure Initialize; override;
  end;

{ TMySDLApplication }

procedure TMySDLApplication.Initialize;
begin
  inherited Initialize;
  Paths.SDL2 := ''; //Change this to load SDL2 library from diferent place
  Paths.SDL_Image := ''; //Change this to load SDL_Image library from diferent place
end;

begin
  DeleteFile('leaks.trc');
  SetHeapTraceOutput('leaks.trc');

  TSDL2Application.Init(TMySDLApplication);
  try
    Application.Title:='SDL2 Application';
    Application.Initialize;
    Application.CreateWnd;
    Application.Run;
  finally
    Application.Free;
  end;
end.
