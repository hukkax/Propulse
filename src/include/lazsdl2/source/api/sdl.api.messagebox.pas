unit SDL.Api.MessageBox;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SDL.Api.Types, DLibrary{, Graphics};

type

  { TSDLMessageBoxApi }

  TSDLMessageBoxApi = class(TSubLibrary)
  private type
    TSDL_ShowMessageBox = function(messageboxdata: PSDL_MessageBoxData; out buttonid: SDL_SInt32): SDL_SInt32 cdecl;
    TSDL_ShowSimpleMessageBox = function(flags: SDL_MessageBoxFlags; title: SDL_String; message: SDL_String; window: PSDL_Window): SDL_SInt32 cdecl;
  public
    (**
     *  \brief Create a modal message box.
     *
     *  \param messageboxdata The SDL_MessageBoxData structure with title, text, etc.
     *  \param buttonid The pointer to which user id of hit button should be copied.
     *
     *  \return -1 on error, otherwise 0 and buttonid contains user id of button
     *          hit or -1 if dialog was closed.
     *
     *  \note This function should be called on the thread that created the parent
     *        window, or on the main thread if the messagebox has no parent.  It will
     *        block execution of that thread until the user clicks a button or
     *        closes the messagebox.
     *)
    SDL_ShowMessageBox: tSDL_ShowMessageBox;
    (**
     *  \brief Create a simple modal message box
     *
     *  \param flags    ::SDL_MessageBoxFlags
     *  \param title    UTF-8 title text
     *  \param message  UTF-8 message text
     *  \param window   The parent window, or NULL for no parent
     *
     *  \return 0 on success, -1 on error
     *
     *  \sa SDL_ShowMessageBox
     *)
    SDL_ShowSimpleMessageBox: TSDL_ShowSimpleMessageBox;
  protected
    procedure GetRequiredMethods(const List: TMethodList); override;
  public
    //function SDL_RGBToSDLColor(const Color: TColor): SDL_MessageBoxColor; inline;
  end;

implementation

{ TSDLMessageBoxApi }

procedure TSDLMessageBoxApi.GetRequiredMethods(const List: TMethodList);
begin
  inherited GetRequiredMethods(List);

  List.Add('SDL_ShowMessageBox', @SDL_ShowMessageBox);
  List.Add('SDL_ShowSimpleMessageBox', @SDL_ShowSimpleMessageBox);
end;

{function TSDLMessageBoxApi.SDL_RGBToSDLColor(const Color: TColor
  ): SDL_MessageBoxColor;
begin
  RedGreenBlue(Color, Result.r, Result.g, Result.b)
end;  }

end.

