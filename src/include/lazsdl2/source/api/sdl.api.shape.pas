unit SDL.Api.Shape;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SDL.Api.Types, DLibrary;

type

  { TSDLShapeApi }

  TSDLShapeApi = class(TSubLibrary)
  private type
    TSDL_CreateShapedWindow = function(title: SDL_String; x: SDL_UInt32; y: SDL_UInt32; w: SDL_UInt32; h: SDL_UInt32; flags: SDL_UInt32): PSDL_Window cdecl;
    TSDL_IsShapedWindow = function(window: PSDL_Window): SDL_Bool cdecl;
    TSDL_SetWindowShape = function(window: PSDL_Window; shape: PSDL_Surface; shape_mode: PSDL_WindowShapeMode): SDL_SInt32 cdecl;
    TSDL_GetShapedWindowMode = function(window: PSDL_Window; shape_mode: PSDL_WindowShapeMode): SDL_SInt32 cdecl;
  public
    (**
     *  \brief Create a window that can be shaped with the specified position, dimensions, and flags.
     *
     *  \param title The title of the window, in UTF-8 encoding.
     *  \param x     The x position of the window, ::SDL_WINDOWPOS_CENTERED, or
     *               ::SDL_WINDOWPOS_UNDEFINED.
     *  \param y     The y position of the window, ::SDL_WINDOWPOS_CENTERED, or
     *               ::SDL_WINDOWPOS_UNDEFINED.
     *  \param w     The width of the window.
     *  \param h     The height of the window.
     *  \param flags The flags for the window, a mask of SDL_WINDOW_BORDERLESS with any of the following:
     *               ::SDL_WINDOW_OPENGL,     ::SDL_WINDOW_INPUT_GRABBED,
     *               ::SDL_WINDOW_HIDDEN,     ::SDL_WINDOW_RESIZABLE,
     *               ::SDL_WINDOW_MAXIMIZED,  ::SDL_WINDOW_MINIMIZED,
     *       ::SDL_WINDOW_BORDERLESS is always set, and ::SDL_WINDOW_FULLSCREEN is always unset.
     *
     *  \return The window created, or NULL if window creation failed.
     *
     *  \sa SDL_DestroyWindow()
     *)
    SDL_CreateShapedWindow: TSDL_CreateShapedWindow;
    (**
     * \brief Return whether the given window is a shaped window.
     *
     * \param window The window to query for being shaped.
     *
     * \return SDL_TRUE if the window is a window that can be shaped, SDL_FALSE if the window is unshaped or NULL.
     * \sa SDL_CreateShapedWindow
     *)
    SDL_IsShapedWindow: TSDL_IsShapedWindow;
    (**
     * \brief Set the shape and parameters of a shaped window.
     *
     * \param window The shaped window whose parameters should be set.
     * \param shape A surface encoding the desired shape for the window.
     * \param shape_mode The parameters to set for the shaped window.
     *
     * \return 0 on success, SDL_INVALID_SHAPE_ARGUMENT on invalid an invalid shape argument, or SDL_NONSHAPEABLE_WINDOW
     *           if the SDL_Window* given does not reference a valid shaped window.
     *
     * \sa SDL_WindowShapeMode
     * \sa SDL_GetShapedWindowMode.
     *)
    SDL_SetWindowShape: TSDL_SetWindowShape;
    (**
     * \brief Get the shape parameters of a shaped window.
     *
     * \param window The shaped window whose parameters should be retrieved.
     * \param shape_mode An empty shape-mode structure to fill, or NULL to check whether the window has a shape.
     *
     * \return 0 if the window has a shape and, provided shape_mode was not NULL, shape_mode has been filled with the mode
     *           data, SDL_NONSHAPEABLE_WINDOW if the SDL_Window given is not a shaped window, or SDL_WINDOW_LACKS_SHAPE if
     *           the SDL_Window* given is a shapeable window currently lacking a shape.
     *
     * \sa SDL_WindowShapeMode
     * \sa SDL_SetWindowShape
     *)
    SDL_GetShapedWindowMode: TSDL_GetShapedWindowMode;
  protected
    procedure DoInit; override;
    procedure GetRequiredMethods(const List: TMethodList); override;
  end;

implementation

{ TSDLShapeApi }

procedure TSDLShapeApi.DoInit;
begin

end;

procedure TSDLShapeApi.GetRequiredMethods(const List: TMethodList);
begin
  inherited GetRequiredMethods(List);

  List.Add('SDL_CreateShapedWindow', @SDL_CreateShapedWindow);
  List.Add('SDL_IsShapedWindow', @SDL_IsShapedWindow);
  List.Add('SDL_SetWindowShape', @SDL_SetWindowShape);
  List.Add('SDL_GetShapedWindowMode', @SDL_GetShapedWindowMode);
end;

end.

