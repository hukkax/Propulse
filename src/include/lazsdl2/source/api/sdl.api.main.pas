unit SDL.Api.Main;

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF WINDOWS}
  SDL.Api.Types,
  {$ENDIF}
  Classes, SysUtils, DLibrary;

type

  { TSDLMainApi }

  TSDLMainApi = class(TSubLibrary)
  private type
    TSDL_SetMainReady = procedure() cdecl;
  {$IFDEF WINDOWS}
    TSDL_main = function(argc: SDL_SInt32; argv: PSDL_String): SDL_SInt32 cdecl;
    TSDL_RegisterApp = function(name: SDL_String; style: SDL_UInt32; hInst: SDL_Pointer): SDL_SInt32 cdecl;
    TSDL_UnregisterApp = procedure() cdecl;
    TSDL_WinRTRunApp = function(mainFunction: TSDL_main; xamlBackgroundPanel: SDL_Pointer; reserved: SDL_Pointer): SDL_SInt32 cdecl;
  {$ENDIF}
  public
    (**
     *  This is called by the real SDL main function to let the rest of the
     *  library know that initialization was done properly.
     *
     *  Calling this yourself without knowing what you're doing can cause
     *  crashes and hard to diagnose problems with your application.
     *)
    SDL_SetMainReady: TSDL_SetMainReady;
  {$IFDEF WINDOWS}
    (**
     *  The prototype for the application's main() function
     *)
    SDL_main: TSDL_main;
    (**
     *  This can be called to set the application class at startup
     *)
    SDL_RegisterApp: TSDL_RegisterApp;
    SDL_UnregisterApp: TSDL_UnregisterApp;
    (**
     *  \brief Initializes and launches an SDL/WinRT application.
     *
     *  \param mainFunction The SDL app's C-style main().
     *  \param reserved Reserved for future use; should be NULL
     *  \return 0 on success, -1 on failure.  On failure, use SDL_GetError to retrieve more
     *      information on the failure.
     *)
    SDL_WinRTRunApp: TSDL_WinRTRunApp;
  {$ENDIF}
  protected
    procedure GetRequiredMethods(const List: TMethodList); override;
  end;

implementation

{ TSDLMainApi }

procedure TSDLMainApi.GetRequiredMethods(const List: TMethodList);
begin
  inherited GetRequiredMethods(List);

  List.Add('SDL_SetMainReady', @SDL_SetMainReady);
  {$IFDEF WINDOWS}
{
  // these would fail on my system but they don't seem to be required to run --hukka
  List.Add('SDL_main', @SDL_main);
  List.Add('SDL_WinRTRunApp', @SDL_WinRTRunApp);
}
  List.Add('SDL_RegisterApp', @SDL_RegisterApp);
  List.Add('SDL_UnregisterApp', @SDL_UnregisterApp);
  {$ENDIF}
end;

end.

