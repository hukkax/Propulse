unit SDL.Api.Clipboard;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SDL.Api.Types, DLibrary;

type

  { TSDLClipboardApi }

  TSDLClipboardApi = class(TSubLibrary)
  private type
    TSDL_SetClipboardText = function(const text: SDL_String): SDL_ResultCode cdecl;
    TSDL_GetClipboardText = function(): SDL_String cdecl;
    TSDL_HasClipboardText = function(): SDL_Bool cdecl;
  public
    (**
     * \brief Put UTF-8 text into the clipboard
     *
     * \sa SDL_GetClipboardText()
     *)
    SDL_SetClipboardText: TSDL_SetClipboardText;
    (**
     * \brief Get UTF-8 text from the clipboard, which must be freed with SDL_free()
     *
     * \sa SDL_SetClipboardText()
     *)
    SDL_GetClipboardText: TSDL_GetClipboardText;
    (**
     * \brief Returns a flag indicating whether the clipboard exists and contains a text string that is non-empty
     *
     * \sa SDL_GetClipboardText()
     *)
    SDL_HasClipboardText: TSDL_HasClipboardText;
  protected
    procedure GetRequiredMethods(const List: TMethodList); override;
  end;

implementation

{ TSDLClipboardApi }

procedure TSDLClipboardApi.GetRequiredMethods(const List: TMethodList);
begin
  inherited GetRequiredMethods(List);

  List.Add('SDL_SetClipboardText', @SDL_SetClipboardText);
  List.Add('SDL_GetClipboardText', @SDL_GetClipboardText);
  List.Add('SDL_HasClipboardText', @SDL_HasClipboardText);
end;

end.

