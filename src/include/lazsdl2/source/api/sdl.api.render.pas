(**
 *  \file SDL_render.h
 *
 *  Header file for SDL 2D rendering functions.
 *
 *  This API supports the following features:
 *      * single pixel points
 *      * single pixel lines
 *      * filled rectangles
 *      * texture images
 *
 *  The primitives may be drawn in opaque, blended, or additive modes.
 *
 *  The texture images may be drawn in opaque, blended, or additive modes.
 *  They can have an additional color tint or alpha modulation applied to
 *  them, and may also be stretched with linear interpolation.
 *
 *  This API is designed to accelerate simple 2D operations. You may
 *  want more functionality such as polygons and particle effects and
 *  in that case you should use SDL's OpenGL/Direct3D support or one
 *  of the many good 3D engines.
 *
 *  These functions must be called from the main thread.
 *  See this bug for details: http://bugzilla.libsdl.org/show_bug.cgi?id=1995
 *)
unit SDL.Api.Render;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SDL.Api.Types, DLibrary;

type

  { TSDLRenderApi }

  TSDLRenderApi = class(TSubLibrary)
  private type
    TSDL_GetNumRenderDrivers = function: SDL_SInt32 cdecl;
    TSDL_GetRenderDriverInfo = function(index: SDL_SInt32; info: PSDL_RendererInfo): SDL_SInt32 cdecl;
    TSDL_CreateWindowAndRenderer = function(width: SDL_SInt32; height: SDL_SInt32; window_flags: SDL_WindowFlags; out window: PSDL_Window; out renderer: PSDL_Renderer): SDL_SInt32 cdecl;
    TSDL_CreateRenderer = function(window: PSDL_Window; index: SDL_SInt32; flags: SDL_RendererFlags): PSDL_Renderer cdecl;
    TSDL_CreateSoftwareRenderer = function(surface: PSDL_Surface): PSDL_Renderer cdecl;
    TSDL_GetRenderer = function(window: PSDL_Window): PSDL_Renderer cdecl;
    TSDL_GetRendererInfo = function(renderer: PSDL_Renderer; info: PSDL_RendererInfo): SDL_SInt32 cdecl;
    TSDL_GetRendererOutputSize = function(renderer: PSDL_Renderer; out w: SDL_SInt32; h: SDL_SInt32): SDL_SInt32 cdecl;
    TSDL_CreateTexture = function(renderer: PSDL_Renderer; format: SDL_UInt32; access: SDL_SInt32; w: SDL_SInt32; h: SDL_SInt32): PSDL_Texture cdecl;
    TSDL_CreateTextureFromSurface = function(renderer: PSDL_Renderer; surface: PSDL_Surface): PSDL_Texture cdecl;
    TSDL_QueryTexture = function(texture: PSDL_Texture; out format: SDL_UInt32; out access: SDL_SInt32; out w: SDL_SInt32; out h: SDL_SInt32): SDL_SInt32 cdecl;
    TSDL_SetTextureColorMod = function(texture: PSDL_Texture; r: SDL_UInt8; g: SDL_UInt8; b: SDL_UInt8): SDL_SInt32 cdecl;
    TSDL_GetTextureColorMod = function(texture: PSDL_Texture; out r: SDL_UInt8; out g: SDL_UInt8; out b: SDL_UInt8): SDL_SInt32 cdecl;
    TSDL_SetTextureAlphaMod = function(texture: PSDL_Texture; alpha: SDL_UInt8): SDL_SInt32 cdecl;
    TSDL_GetTextureAlphaMod = function(texture: PSDL_Texture; out alpha: SDL_UInt8): SDL_SInt32 cdecl;
    TSDL_SetTextureBlendMode = function(texture: PSDL_Texture; blendMode: SDL_BlendMode): SDL_SInt32 cdecl;
    TSDL_GetTextureBlendMode = function(texture: PSDL_Texture; out blendMode: SDL_BlendMode): SDL_SInt32 cdecl;
    TSDL_UpdateTexture = function(texture: PSDL_Texture; rect: PSDL_Rect; pixels: SDL_Data; pitch: SDL_SInt32): SDL_SInt32 cdecl;
    TSDL_UpdateYUVTexture = function(texture: PSDL_Texture; rect: PSDL_Rect; Yplane: PSDL_UInt8; Ypitch: SDL_SInt32; Uplane: PSDL_UInt8; UPitch: SDL_SInt32; Vplane: PSDL_UInt8; VPitch: SDL_SInt32): SDL_SInt32; cdecl;
    TSDL_LockTexture = function(texture: PSDL_Texture; rect: PSDL_Rect; pixels: PPointer; pitch: PSDL_SInt32): SDL_SInt32 cdecl;
    TSDL_UnlockTexture = procedure(texture: PSDL_Texture) cdecl;
    TSDL_RenderTargetSupported = function(renderer: PSDL_Renderer): SDL_Bool cdecl;
    TSDL_SetRenderTarget = function(renderer: PSDL_Renderer; texture: PSDL_Texture): SDL_SInt32 cdecl;
    TSDL_GetRenderTarget = function(renderer: PSDL_Renderer): PSDL_Texture cdecl;
    TSDL_RenderSetLogicalSize = function(renderer: PSDL_Renderer; w: SDL_SInt32; h: SDL_SInt32): SDL_SInt32 cdecl;
    TSDL_RenderGetLogicalSize = procedure(renderer: PSDL_Renderer; out w: SDL_SInt32; out h: SDL_SInt32) cdecl;
    TSDL_RenderSetViewport = function(renderer: PSDL_Renderer; const rect: PSDL_Rect): SDL_SInt32 cdecl;
    TSDL_RenderGetViewport = procedure(renderer: PSDL_Renderer; rect: PSDL_Rect) cdecl;
    TSDL_RenderSetClipRect = function(renderer: PSDL_Renderer; rect: PSDL_Rect): SDL_SInt32 cdecl;
    TSDL_RenderGetClipRect = procedure(renderer: PSDL_Renderer; rect: PSDL_Rect) cdecl;
    {$IFNDEF DISABLE_SDL2_2_0_4}
    TSDL_RenderIsClipEnabled = function(renderer: PSDL_Renderer): SDL_Bool cdecl;
    {$ENDIF}
    TSDL_RenderSetScale = function(renderer: PSDL_Renderer; scaleX: SDL_Float; scaleY: SDL_Float): SDL_SInt32 cdecl;
    {$IFNDEF DISABLE_SDL2_2_0_5}
    TSDL_RenderSetIntegerScale = function(renderer: PSDL_Renderer; enable: SDL_Bool): SDL_SInt32 cdecl;
    {$ENDIF}
    TSDL_RenderGetScale = procedure(renderer: PSDL_Renderer; out scaleX: SDL_Float; out scaleY: SDL_Float) cdecl;
    TSDL_SetRenderDrawColor = function(renderer: PSDL_Renderer; r: SDL_UInt8; g: SDL_UInt8; b: SDL_UInt8; a: SDL_UInt8): SDL_SInt32 cdecl;
    TSDL_GetRenderDrawColor = function(renderer: PSDL_Renderer; out r: SDL_UInt8; out g: SDL_UInt8; out b: SDL_UInt8; out a: SDL_UInt8): SDL_SInt32 cdecl;
    TSDL_SetRenderDrawBlendMode = function(renderer: PSDL_Renderer; blendMode: SDL_BlendMode): SDL_SInt32 cdecl;
    TSDL_GetRenderDrawBlendMode = function(renderer: PSDL_Renderer; out blendMode: SDL_BlendMode): SDL_SInt32 cdecl;
    TSDL_RenderClear = function(renderer: PSDL_Renderer): SDL_SInt32 cdecl;
    TSDL_RenderDrawPoint = function(renderer: PSDL_Renderer; x: SDL_SInt32; y: SDL_SInt32): SDL_SInt32 cdecl;
    TSDL_RenderDrawPoints = function(renderer: PSDL_Renderer; points: PSDL_Point; count: SDL_SInt32): SDL_SInt32 cdecl;
    TSDL_RenderDrawLine = function(renderer: PSDL_Renderer; x1: SDL_SInt32; y1: SDL_SInt32; x2: SDL_SInt32; y2: SDL_SInt32): SDL_SInt32 cdecl;
    TSDL_RenderDrawLines = function(renderer: PSDL_Renderer; points: PSDL_Point; count: SDL_SInt32): SDL_SInt32 cdecl;
    TSDL_RenderDrawRect = function(renderer: PSDL_Renderer; rect: PSDL_Rect): SDL_SInt32 cdecl;
    TSDL_RenderDrawRects = function(renderer: PSDL_Renderer; rects: PSDL_Rect; count: SDL_SInt32): SDL_SInt32 cdecl;
    TSDL_RenderFillRect = function(renderer: PSDL_Renderer; rect: PSDL_Rect): SDL_SInt32 cdecl;
    TSDL_RenderFillRects = function(renderer: PSDL_Renderer; rects: PSDL_Rect; count: SDL_SInt32): SDL_SInt32 cdecl;
    TSDL_RenderCopy = function(renderer: PSDL_Renderer; texture: PSDL_Texture; srcrect: PSDL_Rect; dstrect: PSDL_Rect): SDL_SInt32 cdecl;
    TSDL_RenderCopyEx = function(renderer: PSDL_Renderer; texture: PSDL_Texture; const srcrect: PSDL_Rect; dstrect: PSDL_Rect; angle: SDL_Double; center: PSDL_Point; flip: SDL_RendererFlip): SDL_SInt32 cdecl;
    TSDL_RenderReadPixels = function(renderer: PSDL_Renderer; rect: PSDL_Rect; format: SDL_UInt32; pixels: SDL_Pointer; pitch: SDL_SInt32): SDL_SInt32 cdecl;
    TSDL_RenderPresent = procedure(renderer: PSDL_Renderer) cdecl;
    TSDL_DestroyTexture = procedure(texture: PSDL_Texture) cdecl;
    TSDL_DestroyRenderer = procedure(renderer: PSDL_Renderer) cdecl;
    TSDL_GL_BindTexture = function(texture: PSDL_Texture; texw: PSDL_Float; texh: PSDL_Float): SDL_ResultCode cdecl;
    TSDL_GL_BindTextureExt = function(texture: PSDL_Texture; out texw: SDL_Float; out texh: SDL_Float): SDL_ResultCode cdecl;
    TSDL_GL_UnbindTexture = function(texture: PSDL_Texture): SDL_ResultCode cdecl;
  public
    (**
     *  \brief Get the number of 2D rendering drivers available for the current
     *         display.
     *
     *  A render driver is a set of code that handles rendering and texture
     *  management on a particular display.  Normally there is only one, but
     *  some drivers may have several available with different capabilities.
     *
     *  \sa SDL_GetRenderDriverInfo()
     *  \sa SDL_CreateRenderer()
     *)
    SDL_GetNumRenderDrivers: TSDL_GetNumRenderDrivers;
    (**
     *  \brief Get information about a specific 2D rendering driver for the current
     *         display.
     *
     *  \param index The index of the driver to query information about.
     *  \param info  A pointer to an SDL_RendererInfo struct to be filled with
     *               information on the rendering driver.
     *
     *  \return 0 on success, -1 if the index was out of range.
     *
     *  \sa SDL_CreateRenderer()
     *)
    SDL_GetRenderDriverInfo: TSDL_GetRenderDriverInfo;
    (**
     *  \brief Create a window and default renderer
     *
     *  \param width    The width of the window
     *  \param height   The height of the window
     *  \param window_flags The flags used to create the window
     *  \param window   A pointer filled with the window, or NULL on error
     *  \param renderer A pointer filled with the renderer, or NULL on error
     *
     *  \return 0 on success, or -1 on error
     *)
    SDL_CreateWindowAndRenderer: TSDL_CreateWindowAndRenderer;
    (**
     *  \brief Create a 2D rendering context for a window.
     *
     *  \param window The window where rendering is displayed.
     *  \param index    The index of the rendering driver to initialize, or -1 to
     *                  initialize the first one supporting the requested flags.
     *  \param flags    ::SDL_RendererFlags.
     *
     *  \return A valid rendering context or NULL if there was an error.
     *
     *  \sa SDL_CreateSoftwareRenderer()
     *  \sa SDL_GetRendererInfo()
     *  \sa SDL_DestroyRenderer()
     *)
    SDL_CreateRenderer: TSDL_CreateRenderer;
    (**
     *  \brief Create a 2D software rendering context for a surface.
     *
     *  \param surface The surface where rendering is done.
     *
     *  \return A valid rendering context or NULL if there was an error.
     *
     *  \sa SDL_CreateRenderer()
     *  \sa SDL_DestroyRenderer()
     *)
    SDL_CreateSoftwareRenderer: TSDL_CreateSoftwareRenderer;
    (**
     *  \brief Get the renderer associated with a window.
     *)
    SDL_GetRenderer: TSDL_GetRenderer;
    (**
     *  \brief Get information about a rendering context.
     *)
    SDL_GetRendererInfo: TSDL_GetRendererInfo;
    (**
     *  \brief Get the output size in pixels of a rendering context.
     *)
    SDL_GetRendererOutputSize: TSDL_GetRendererOutputSize;
    (**
     *  \brief Create a texture for a rendering context.
     *
     *  \param renderer The renderer.
     *  \param format The format of the texture.
     *  \param access One of the enumerated values in ::SDL_TextureAccess.
     *  \param w      The width of the texture in pixels.
     *  \param h      The height of the texture in pixels.
     *
     *  \return The created texture is returned, or NULL if no rendering context was
     *          active,  the format was unsupported, or the width or height were out
     *          of range.
     *
     *  \sa SDL_QueryTexture()
     *  \sa SDL_UpdateTexture()
     *  \sa SDL_DestroyTexture()
     *)
    SDL_CreateTexture: TSDL_CreateTexture;
    (**
     *  \brief Create a texture from an existing surface.
     *
     *  \param renderer The renderer.
     *  \param surface The surface containing pixel data used to fill the texture.
     *
     *  \return The created texture is returned, or NULL on error.
     *
     *  \note The surface is not modified or freed by this function.
     *
     *  \sa SDL_QueryTexture()
     *  \sa SDL_DestroyTexture()
     *)
    SDL_CreateTextureFromSurface: TSDL_CreateTextureFromSurface;
    (**
     *  \brief Query the attributes of a texture
     *
     *  \param texture A texture to be queried.
     *  \param format  A pointer filled in with the raw format of the texture.  The
     *                 actual format may differ, but pixel transfers will use this
     *                 format.
     *  \param access  A pointer filled in with the actual access to the texture.
     *  \param w       A pointer filled in with the width of the texture in pixels.
     *  \param h       A pointer filled in with the height of the texture in pixels.
     *
     *  \return 0 on success, or -1 if the texture is not valid.
     *)
    SDL_QueryTexture: TSDL_QueryTexture;
    (**
     *  \brief Set an additional color value used in render copy operations.
     *
     *  \param texture The texture to update.
     *  \param r       The red color value multiplied into copy operations.
     *  \param g       The green color value multiplied into copy operations.
     *  \param b       The blue color value multiplied into copy operations.
     *
     *  \return 0 on success, or -1 if the texture is not valid or color modulation
     *          is not supported.
     *
     *  \sa SDL_GetTextureColorMod()
     *)
    SDL_SetTextureColorMod: TSDL_SetTextureColorMod;
    (**
     *  \brief Get the additional color value used in render copy operations.
     *
     *  \param texture The texture to query.
     *  \param r         A pointer filled in with the current red color value.
     *  \param g         A pointer filled in with the current green color value.
     *  \param b         A pointer filled in with the current blue color value.
     *
     *  \return 0 on success, or -1 if the texture is not valid.
     *
     *  \sa SDL_SetTextureColorMod()
     *)
    SDL_GetTextureColorMod: TSDL_GetTextureColorMod;
    (**
     *  \brief Set an additional alpha value used in render copy operations.
     *
     *  \param texture The texture to update.
     *  \param alpha     The alpha value multiplied into copy operations.
     *
     *  \return 0 on success, or -1 if the texture is not valid or alpha modulation
     *          is not supported.
     *
     *  \sa SDL_GetTextureAlphaMod()
     *)
    SDL_SetTextureAlphaMod: TSDL_SetTextureAlphaMod;
    (**
     *  \brief Get the additional alpha value used in render copy operations.
     *
     *  \param texture The texture to query.
     *  \param alpha     A pointer filled in with the current alpha value.
     *
     *  \return 0 on success, or -1 if the texture is not valid.
     *
     *  \sa SDL_SetTextureAlphaMod()
     *)
    SDL_GetTextureAlphaMod: TSDL_GetTextureAlphaMod;
    (**
     *  \brief Set the blend mode used for texture copy operations.
     *
     *  \param texture The texture to update.
     *  \param blendMode ::SDL_BlendMode to use for texture blending.
     *
     *  \return 0 on success, or -1 if the texture is not valid or the blend mode is
     *          not supported.
     *
     *  \note If the blend mode is not supported, the closest supported mode is
     *        chosen.
     *
     *  \sa SDL_GetTextureBlendMode()
     *)
    SDL_SetTextureBlendMode: TSDL_SetTextureBlendMode;
    (**
     *  \brief Get the blend mode used for texture copy operations.
     *
     *  \param texture   The texture to query.
     *  \param blendMode A pointer filled in with the current blend mode.
     *
     *  \return 0 on success, or -1 if the texture is not valid.
     *
     *  \sa SDL_SetTextureBlendMode()
     *)
    SDL_GetTextureBlendMode: TSDL_GetTextureBlendMode;
    (**
     *  \brief Update the given texture rectangle with new pixel data.
     *
     *  \param texture   The texture to update
     *  \param rect      A pointer to the rectangle of pixels to update, or NULL to
     *                   update the entire texture.
     *  \param pixels    The raw pixel data.
     *  \param pitch     The number of bytes in a row of pixel data, including padding between lines.
     *
     *  \return 0 on success, or -1 if the texture is not valid.
     *
     *  \note This is a fairly slow function.
     *)
    SDL_UpdateTexture: TSDL_UpdateTexture;
    (**
     *  \brief Update a rectangle within a planar YV12 or IYUV texture with new pixel data.
     *
     *  \param texture   The texture to update
     *  \param rect      A pointer to the rectangle of pixels to update, or NULL to
     *                   update the entire texture.
     *  \param Yplane    The raw pixel data for the Y plane.
     *  \param Ypitch    The number of bytes between rows of pixel data for the Y plane.
     *  \param Uplane    The raw pixel data for the U plane.
     *  \param Upitch    The number of bytes between rows of pixel data for the U plane.
     *  \param Vplane    The raw pixel data for the V plane.
     *  \param Vpitch    The number of bytes between rows of pixel data for the V plane.
     *
     *  \return 0 on success, or -1 if the texture is not valid.
     *
     *  \note You can use SDL_UpdateTexture() as long as your pixel data is
     *        a contiguous block of Y and U/V planes in the proper order, but
     *        this function is available if your pixel data is not contiguous.
     *)
    SDL_UpdateYUVTexture: TSDL_UpdateYUVTexture;
    (**
     *  \brief Lock a portion of the texture for write-only pixel access.
     *
     *  \param texture   The texture to lock for access, which was created with
     *                   ::SDL_TEXTUREACCESS_STREAMING.
     *  \param rect      A pointer to the rectangle to lock for access. If the rect
     *                   is NULL, the entire texture will be locked.
     *  \param pixels    This is filled in with a pointer to the locked pixels,
     *                   appropriately offset by the locked area.
     *  \param pitch     This is filled in with the pitch of the locked pixels.
     *
     *  \return 0 on success, or -1 if the texture is not valid or was not created with ::SDL_TEXTUREACCESS_STREAMING.
     *
     *  \sa SDL_UnlockTexture()
     *)
    SDL_LockTexture: TSDL_LockTexture;
    (**
     *  \brief Unlock a texture, uploading the changes to video memory, if needed.
     *
     *  \sa SDL_LockTexture()
     *)
    SDL_UnlockTexture: TSDL_UnlockTexture;
    (**
     * \brief Determines whether a window supports the use of render targets
     *
     * \param renderer The renderer that will be checked
     *
     * \return SDL_TRUE if supported, SDL_FALSE if not.
     *)
     SDL_RenderTargetSupported: TSDL_RenderTargetSupported;
     (**
      * \brief Set a texture as the current rendering target.
      *
      * \param renderer The renderer.
      * \param texture The targeted texture, which must be created with the SDL_TEXTUREACCESS_TARGET flag, or NULL for the default render target
      *
      * \return 0 on success, or -1 on error
      *
      *  \sa SDL_GetRenderTarget()
      *)
     SDL_SetRenderTarget: TSDL_SetRenderTarget;
     (**
      * \brief Get the current render target or NULL for the default render target.
      *
      * \return The current render target
      *
      *  \sa SDL_SetRenderTarget()
      *)
     SDL_GetRenderTarget: TSDL_GetRenderTarget;
     (**
      *  \brief Set device independent resolution for rendering
      *
      *  \param renderer The renderer for which resolution should be set.
      *  \param w      The width of the logical resolution
      *  \param h      The height of the logical resolution
      *
      *  This function uses the viewport and scaling functionality to allow a fixed logical
      *  resolution for rendering, regardless of the actual output resolution.  If the actual
      *  output resolution doesn't have the same aspect ratio the output rendering will be
      *  centered within the output display.
      *
      *  If the output display is a window, mouse events in the window will be filtered
      *  and scaled so they seem to arrive within the logical resolution.
      *
      *  \note If this function results in scaling or subpixel drawing by the
      *        rendering backend, it will be handled using the appropriate
      *        quality hints.
      *
      *  \sa SDL_RenderGetLogicalSize()
      *  \sa SDL_RenderSetScale()
      *  \sa SDL_RenderSetViewport()
      *)
     SDL_RenderSetLogicalSize: TSDL_RenderSetLogicalSize;
     (**
      *  \brief Get device independent resolution for rendering
      *
      *  \param renderer The renderer from which resolution should be queried.
      *  \param w      A pointer filled with the width of the logical resolution
      *  \param h      A pointer filled with the height of the logical resolution
      *
      *  \sa SDL_RenderSetLogicalSize()
      *)
     SDL_RenderGetLogicalSize: TSDL_RenderGetLogicalSize;
     (**
      *  \brief Set the drawing area for rendering on the current target.
      *
      *  \param renderer The renderer for which the drawing area should be set.
      *  \param rect The rectangle representing the drawing area, or NULL to set the viewport to the entire target.
      *
      *  The x,y of the viewport rect represents the origin for rendering.
      *
      *  \return 0 on success, or -1 on error
      *
      *  \note If the window associated with the renderer is resized, the viewport is automatically reset.
      *
      *  \sa SDL_RenderGetViewport()
      *  \sa SDL_RenderSetLogicalSize()
      *)
     SDL_RenderSetViewport: TSDL_RenderSetViewport;
     (**
      *  \brief Get the drawing area for the current target.
      *
      *  \sa SDL_RenderSetViewport()
      *)
     SDL_RenderGetViewport: TSDL_RenderGetViewport;
     (**
      *  \brief Set the clip rectangle for the current target.
      *
      *  \param renderer The renderer for which clip rectangle should be set.
      *  \param rect   A pointer to the rectangle to set as the clip rectangle, or
      *                NULL to disable clipping.
      *
      *  \return 0 on success, or -1 on error
      *
      *  \sa SDL_RenderGetClipRect()
      *)
     SDL_RenderSetClipRect: TSDL_RenderSetClipRect;
     (**
      *  \brief Get the clip rectangle for the current target.
      *
      *  \param renderer The renderer from which clip rectangle should be queried.
      *  \param rect   A pointer filled in with the current clip rectangle, or
      *                an empty rectangle if clipping is disabled.
      *
      *  \sa SDL_RenderSetClipRect()
      *)
    SDL_RenderGetClipRect: TSDL_RenderGetClipRect;
    {$IFNDEF DISABLE_SDL2_2_0_4}
    (**
     *  \brief Get whether clipping is enabled on the given renderer.
     *
     *  \param renderer The renderer from which clip state should be queried.
     *
     *  \sa SDL_RenderGetClipRect()
     *)
    SDL_RenderIsClipEnabled: TSDL_RenderIsClipEnabled;
    {$ENDIF}
     (**
      *  \brief Set the drawing scale for rendering on the current target.
      *
      *  \param renderer The renderer for which the drawing scale should be set.
      *  \param scaleX The horizontal scaling factor
      *  \param scaleY The vertical scaling factor
      *
      *  The drawing coordinates are scaled by the x/y scaling factors
      *  before they are used by the renderer.  This allows resolution
      *  independent drawing with a single coordinate system.
      *
      *  \note If this results in scaling or subpixel drawing by the
      *        rendering backend, it will be handled using the appropriate
      *        quality hints.  For best results use integer scaling factors.
      *
      *  \sa SDL_RenderGetScale()
      *  \sa SDL_RenderSetLogicalSize()
      *)
     SDL_RenderSetScale: TSDL_RenderSetScale;
     (**
      *  \brief Get the drawing scale for the current target.
      *
      *  \param renderer The renderer from which drawing scale should be queried.
      *  \param scaleX A pointer filled in with the horizontal scaling factor
      *  \param scaleY A pointer filled in with the vertical scaling factor
      *
      *  \sa SDL_RenderSetScale()
      *)
     SDL_RenderGetScale: TSDL_RenderGetScale;
     (**
      *  \brief Set the color used for drawing operations (Rect, Line and Clear).
      *
      *  \param renderer The renderer for which drawing color should be set.
      *  \param r The red value used to draw on the rendering target.
      *  \param g The green value used to draw on the rendering target.
      *  \param b The blue value used to draw on the rendering target.
      *  \param a The alpha value used to draw on the rendering target, usually
      *           ::SDL_ALPHA_OPAQUE (255).
      *
      *  \return 0 on success, or -1 on error
      *)
	 {$IFNDEF DISABLE_SDL2_2_0_5}
	 (**
	  *  \brief Set whether to force integer scales for resolution-independent rendering.
	  *
	  *  \param renderer The renderer from which clip state should be queried.
	  *  \param enable   Enable or disable integer scaling.
	  *
	  *  \sa SDL_RenderSetIntegerScale()
	  *)
	 SDL_RenderSetIntegerScale: TSDL_RenderSetIntegerScale;
	 {$ENDIF}
     SDL_SetRenderDrawColor: TSDL_SetRenderDrawColor;
     (**
      *  \brief Get the color used for drawing operations (Rect, Line and Clear).
      *
      *  \param renderer The renderer from which drawing color should be queried.
      *  \param r A pointer to the red value used to draw on the rendering target.
      *  \param g A pointer to the green value used to draw on the rendering target.
      *  \param b A pointer to the blue value used to draw on the rendering target.
      *  \param a A pointer to the alpha value used to draw on the rendering target,
      *           usually ::SDL_ALPHA_OPAQUE (255).
      *
      *  \return 0 on success, or -1 on error
      *)
     SDL_GetRenderDrawColor: TSDL_GetRenderDrawColor;
     (**
      *  \brief Set the blend mode used for drawing operations (Fill and Line).
      *
      *  \param renderer The renderer for which blend mode should be set.
      *  \param blendMode ::SDL_BlendMode to use for blending.
      *
      *  \return 0 on success, or -1 on error
      *
      *  \note If the blend mode is not supported, the closest supported mode is
      *        chosen.
      *
      *  \sa SDL_GetRenderDrawBlendMode()
      *)
     SDL_SetRenderDrawBlendMode: TSDL_SetRenderDrawBlendMode;
     (**
      *  \brief Get the blend mode used for drawing operations.
      *
      *  \param renderer The renderer from which blend mode should be queried.
      *  \param blendMode A pointer filled in with the current blend mode.
      *
      *  \return 0 on success, or -1 on error
      *
      *  \sa SDL_SetRenderDrawBlendMode()
      *)
     SDL_GetRenderDrawBlendMode: TSDL_GetRenderDrawBlendMode;
     (**
      *  \brief Clear the current rendering target with the drawing color
      *
      *  This function clears the entire rendering target, ignoring the viewport.
      *
      *  \return 0 on success, or -1 on error
      *)
     SDL_RenderClear: TSDL_RenderClear;
     (**
      *  \brief Draw a point on the current rendering target.
      *
      *  \param renderer The renderer which should draw a point.
      *  \param x The x coordinate of the point.
      *  \param y The y coordinate of the point.
      *
      *  \return 0 on success, or -1 on error
      *)
     SDL_RenderDrawPoint: TSDL_RenderDrawPoint;
     (**
      *  \brief Draw multiple points on the current rendering target.
      *
      *  \param renderer The renderer which should draw multiple points.
      *  \param points The points to draw
      *  \param count The number of points to draw
      *
      *  \return 0 on success, or -1 on error
      *)
     SDL_RenderDrawPoints: TSDL_RenderDrawPoints;
     (**
      *  \brief Draw a line on the current rendering target.
      *
      *  \param renderer The renderer which should draw a line.
      *  \param x1 The x coordinate of the start point.
      *  \param y1 The y coordinate of the start point.
      *  \param x2 The x coordinate of the end point.
      *  \param y2 The y coordinate of the end point.
      *
      *  \return 0 on success, or -1 on error
      *)
     SDL_RenderDrawLine: TSDL_RenderDrawLine;
     (**
      *  \brief Draw a series of connected lines on the current rendering target.
      *
      *  \param renderer The renderer which should draw multiple lines.
      *  \param points The points along the lines
      *  \param count The number of points, drawing count-1 lines
      *
      *  \return 0 on success, or -1 on error
      *)
     SDL_RenderDrawLines: TSDL_RenderDrawLines;
     (**
      *  \brief Draw a rectangle on the current rendering target.
      *
      *  \param renderer The renderer which should draw a rectangle.
      *  \param rect A pointer to the destination rectangle, or NULL to outline the entire rendering target.
      *
      *  \return 0 on success, or -1 on error
      *)
     SDL_RenderDrawRect: TSDL_RenderDrawRect;
     (**
      *  \brief Draw some number of rectangles on the current rendering target.
      *
      *  \param renderer The renderer which should draw multiple rectangles.
      *  \param rects A pointer to an array of destination rectangles.
      *  \param count The number of rectangles.
      *
      *  \return 0 on success, or -1 on error
      *)
     SDL_RenderDrawRects: TSDL_RenderDrawRects;
     (**
      *  \brief Fill a rectangle on the current rendering target with the drawing color.
      *
      *  \param renderer The renderer which should fill a rectangle.
      *  \param rect A pointer to the destination rectangle, or NULL for the entire
      *              rendering target.
      *
      *  \return 0 on success, or -1 on error
      *)
     SDL_RenderFillRect: TSDL_RenderFillRect;
     (**
      *  \brief Fill some number of rectangles on the current rendering target with the drawing color.
      *
      *  \param renderer The renderer which should fill multiple rectangles.
      *  \param rects A pointer to an array of destination rectangles.
      *  \param count The number of rectangles.
      *
      *  \return 0 on success, or -1 on error
      *)
     SDL_RenderFillRects: TSDL_RenderFillRects;
     (**
      *  \brief Copy a portion of the texture to the current rendering target.
      *
      *  \param renderer The renderer which should copy parts of a texture.
      *  \param texture The source texture.
      *  \param srcrect   A pointer to the source rectangle, or NULL for the entire
      *                   texture.
      *  \param dstrect   A pointer to the destination rectangle, or NULL for the
      *                   entire rendering target.
      *
      *  \return 0 on success, or -1 on error
      *)
     SDL_RenderCopy: TSDL_RenderCopy;
     (**
      *  \brief Copy a portion of the source texture to the current rendering target, rotating it by angle around the given center
      *
      *  \param renderer The renderer which should copy parts of a texture.
      *  \param texture The source texture.
      *  \param srcrect   A pointer to the source rectangle, or NULL for the entire
      *                   texture.
      *  \param dstrect   A pointer to the destination rectangle, or NULL for the
      *                   entire rendering target.
      *  \param angle    An angle in degrees that indicates the rotation that will be applied to dstrect
      *  \param center   A pointer to a point indicating the point around which dstrect will be rotated (if NULL, rotation will be done around dstrect.w/2, dstrect.h/2).
      *  \param flip     An SDL_RendererFlip value stating which flipping actions should be performed on the texture
      *
      *  \return 0 on success, or -1 on error
      *)
     SDL_RenderCopyEx: TSDL_RenderCopyEx;
     (**
      *  \brief Read pixels from the current rendering target.
      *
      *  \param renderer The renderer from which pixels should be read.
      *  \param rect   A pointer to the rectangle to read, or NULL for the entire
      *                render target.
      *  \param format The desired format of the pixel data, or 0 to use the format
      *                of the rendering target
      *  \param pixels A pointer to be filled in with the pixel data
      *  \param pitch  The pitch of the pixels parameter.
      *
      *  \return 0 on success, or -1 if pixel reading is not supported.
      *
      *  \warning This is a very slow operation, and should not be used frequently.
      *)
     SDL_RenderReadPixels: TSDL_RenderReadPixels;
     (**
      *  \brief Update the screen with rendering performed.
      *)
     SDL_RenderPresent: TSDL_RenderPresent;
     (**
      *  \brief Destroy the specified texture.
      *
      *  \sa SDL_CreateTexture()
      *  \sa SDL_CreateTextureFromSurface()
      *)
     SDL_DestroyTexture: TSDL_DestroyTexture;
     (**
      *  \brief Destroy the rendering context for a window and free associated
      *         textures.
      *
      *  \sa SDL_CreateRenderer()
      *)
     SDL_DestroyRenderer: TSDL_DestroyRenderer;
     (**
      *  \brief Bind the texture to the current OpenGL/ES/ES2 context for use with
      *         OpenGL instructions.
      *
      *  \param texture  The SDL texture to bind
      *  \param texw     A pointer to a float that will be filled with the texture width
      *  \param texh     A pointer to a float that will be filled with the texture height
      *
      *  \return 0 on success, or -1 if the operation is not supported
      *)
     (* @{ *)
     SDL_GL_BindTexture: TSDL_GL_BindTexture;
     SDL_GL_BindTextureExt: TSDL_GL_BindTextureExt;
     (* @} *)
     (**
      *  \brief Unbind a texture from the current OpenGL/ES/ES2 context.
      *
      *  \param texture  The SDL texture to unbind
      *
      *  \return 0 on success, or -1 if the operation is not supported
      *)
     SDL_GL_UnbindTexture: TSDL_GL_UnbindTexture;
  protected
    procedure DoInit; override;
    procedure GetRequiredMethods(const List: TMethodList); override;
  end;

implementation

{ TSDLRenderApi }

procedure TSDLRenderApi.DoInit;
begin

end;

procedure TSDLRenderApi.GetRequiredMethods(const List: TMethodList);
begin
  inherited GetRequiredMethods(List);

  List.Add('SDL_GetNumRenderDrivers', @SDL_GetNumRenderDrivers);
  List.Add('SDL_GetRenderDriverInfo', @SDL_GetRenderDriverInfo);
  List.Add('SDL_CreateWindowAndRenderer', @SDL_CreateWindowAndRenderer);
  List.Add('SDL_CreateRenderer', @SDL_CreateRenderer);
  List.Add('SDL_CreateSoftwareRenderer', @SDL_CreateSoftwareRenderer);
  List.Add('SDL_GetRenderer', @SDL_GetRenderer);
  List.Add('SDL_GetRendererInfo', @SDL_GetRendererInfo);
  List.Add('SDL_GetRendererOutputSize', @SDL_GetRendererOutputSize);
  List.Add('SDL_CreateTexture', @SDL_CreateTexture);
  List.Add('SDL_CreateTextureFromSurface', @SDL_CreateTextureFromSurface);
  List.Add('SDL_QueryTexture', @SDL_QueryTexture);
  List.Add('SDL_SetTextureColorMod', @SDL_SetTextureColorMod);
  List.Add('SDL_GetTextureColorMod', @SDL_GetTextureColorMod);
  List.Add('SDL_SetTextureAlphaMod', @SDL_SetTextureAlphaMod);
  List.Add('SDL_GetTextureAlphaMod', @SDL_GetTextureAlphaMod);
  List.Add('SDL_SetTextureBlendMode', @SDL_SetTextureBlendMode);
  List.Add('SDL_GetTextureBlendMode', @SDL_GetTextureBlendMode);
  List.Add('SDL_UpdateTexture', @SDL_UpdateTexture);
  List.Add('SDL_UpdateYUVTexture', @SDL_UpdateYUVTexture);
  List.Add('SDL_LockTexture', @SDL_LockTexture);
  List.Add('SDL_UnlockTexture', @SDL_UnlockTexture);
  List.Add('SDL_RenderTargetSupported', @SDL_RenderTargetSupported);
  List.Add('SDL_SetRenderTarget', @SDL_SetRenderTarget);
  List.Add('SDL_GetRenderTarget', @SDL_GetRenderTarget);
  List.Add('SDL_RenderSetLogicalSize', @SDL_RenderSetLogicalSize);
  List.Add('SDL_RenderGetLogicalSize', @SDL_RenderGetLogicalSize);
  List.Add('SDL_RenderSetViewport', @SDL_RenderSetViewport);
  List.Add('SDL_RenderGetViewport', @SDL_RenderGetViewport);
  List.Add('SDL_RenderSetClipRect', @SDL_RenderSetClipRect);
  List.Add('SDL_RenderGetClipRect', @SDL_RenderGetClipRect);
  {$IFNDEF DISABLE_SDL2_2_0_4}
  List.Add('SDL_RenderIsClipEnabled', @SDL_RenderIsClipEnabled, False);
  {$ENDIF}
  {$IFNDEF DISABLE_SDL2_2_0_5}
  List.Add('SDL_RenderSetIntegerScale', @SDL_RenderSetIntegerScale, False);
  {$ENDIF}
  List.Add('SDL_RenderSetScale', @SDL_RenderSetScale);
  List.Add('SDL_RenderGetScale', @SDL_RenderGetScale);
  List.Add('SDL_SetRenderDrawColor', @SDL_SetRenderDrawColor);
  List.Add('SDL_GetRenderDrawColor', @SDL_GetRenderDrawColor);
  List.Add('SDL_SetRenderDrawBlendMode', @SDL_SetRenderDrawBlendMode);
  List.Add('SDL_GetRenderDrawBlendMode', @SDL_GetRenderDrawBlendMode);
  List.Add('SDL_RenderClear', @SDL_RenderClear);
  List.Add('SDL_RenderDrawPoint', @SDL_RenderDrawPoint);
  List.Add('SDL_RenderDrawPoints', @SDL_RenderDrawPoints);
  List.Add('SDL_RenderDrawLine', @SDL_RenderDrawLine);
  List.Add('SDL_RenderDrawLines', @SDL_RenderDrawLines);
  List.Add('SDL_RenderDrawRect', @SDL_RenderDrawRect);
  List.Add('SDL_RenderDrawRects', @SDL_RenderDrawRects);
  List.Add('SDL_RenderFillRect', @SDL_RenderFillRect);
  List.Add('SDL_RenderFillRects', @SDL_RenderFillRects);
  List.Add('SDL_RenderCopy', @SDL_RenderCopy);
  List.Add('SDL_RenderCopyEx', @SDL_RenderCopyEx);
  List.Add('SDL_RenderReadPixels', @SDL_RenderReadPixels);
  List.Add('SDL_RenderPresent', @SDL_RenderPresent);
  List.Add('SDL_DestroyTexture', @SDL_DestroyTexture);
  List.Add('SDL_DestroyRenderer', @SDL_DestroyRenderer);
  List.Add('SDL_GL_BindTexture', @SDL_GL_BindTexture);
  List.Add('SDL_GL_BindTexture', @SDL_GL_BindTextureExt);
  List.Add('SDL_GL_UnbindTexture', @SDL_GL_UnbindTexture);
end;

end.

