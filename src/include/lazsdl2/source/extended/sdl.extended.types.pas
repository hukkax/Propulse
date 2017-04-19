unit SDL.Extended.Types;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  Classes, SDL.Api.OpenGL.Types, SDL.Extended.Context;

type

  TLogLevel = (
    logUnknown,
    logDefault,
    logVerbose,
    logDebug,
    logInfo,
    logWarning,
    logError,
    logFatal,
    logSilent
  );

  { Vertex3f }

  Vertex3f = record
  public
    x: GLfloat;
    y: GLfloat;
    z: GLfloat;

    procedure Make(out V: Vertex3f; aX, aY, aZ: GLfloat); inline;
    procedure Make(out V: Vertex3f; aX, aY: GLfloat); inline;
  end;

  { Vertex2f }

  Vertex2f = record
  public
    x: GLfloat;
    y: GLfloat;

    function Make(aX, aY: GLfloat): Vertex2f; inline;
    procedure Make(out V: Vertex2f; aX, aY: GLfloat); inline;
  end;

  TOpenGLVertex3f = array of Vertex3f;
  TOpenGLVertex2f = array of Vertex2f;

  TSDLVertex = array of GLfloat;
  TSDLCords = array of array of GLfloat;
  TSDLTextures = array of PSDLTextureContext;

  { IRenderer }

  IRenderer = interface
    ['{16E8EAA1-3241-4CA7-B4E1-C47769BCB715}']
    function LoadShaderProgramm(VertexSource: TStream; FragmentSource: TStream; out ShaderProgramm: TOpenGLShaderContext): Boolean;
    function DrawTexture2f(const Vertex, TextCoords: TOpenGLVertex2f; const Texture: GL_Texture; Matrix: POpenGLMatrix): Boolean;
    function DrawTransparentTexture2f(const Vertex, TextCoords: TOpenGLVertex2f; const Texture: GL_Texture; Matrix: POpenGLMatrix): Boolean;
    (**
      *
      * \brief Draw rectangle
      *
      * \param Vertex Contains all rectangle for corrdinates. All vertex coordinates must divide with 4
      * \param Color Color witch use to draw rectangle or colors withc will be used for every vertex seperatly
      * \param Matrix Object position in world
      *
      * \note Vertex count and Colors count must be equal if using multi color mode
      *
     **)
    function DrawRectangle2f(const Vertex: TOpenGLVertex2f; const Color: GL_Color; Matrix: POpenGLMatrix): Boolean;
    function DrawRectangle2f(const Vertex: TOpenGLVertex2f; const Color: GL_Colors; Matrix: POpenGLMatrix): Boolean;
    function DrawRectangle3f(const Vertex: TOpenGLVertex3f; const Color: GL_Color; Matrix: POpenGLMatrix): Boolean;
    function DrawRectangle3f(const Vertex: TOpenGLVertex3f; const Color: GL_Colors; Matrix: POpenGLMatrix): Boolean;
    function DrawTransparentRectangle2f(const Vertex: TOpenGLVertex2f; const Color: GL_Color; Matrix: POpenGLMatrix): Boolean;
    function DrawTransparentRectangle2f(const Vertex: TOpenGLVertex2f; const Color: GL_Colors; Matrix: POpenGLMatrix): Boolean;
    function DrawTransparentRectangle3f(const Vertex: TOpenGLVertex3f; const Color: GL_Color; Matrix: POpenGLMatrix): Boolean;
    function DrawTransparentRectangle3f(const Vertex: TOpenGLVertex3f; const Color: GL_Colors; Matrix: POpenGLMatrix): Boolean;
    (**
      *
      * \brief Draws objects
      *
      * \param mode Type of object what to draw
      * \param Vertex All poligon vertex x, y or z corrdinates
      * \param Color Color witch use to draw object or colors withc will be used for every vertex seperatly
      * \param Matrix Object position in world
      *
      * \sa DrawRectangle
      *
      * \note Vertex count and Colors count must be equal if using multi color mode
      *
      * \return True if all parameters was valid false if one or more parameters was invalid
      *
    **)
    (* @{ *)
    function DrawArrays3f(const mode: GL_DrawMode; const Vertex: TOpenGLVertex3f; const Color: GL_Color; Matrix: POpenGLMatrix): Boolean;
    function DrawArrays2f(const mode: GL_DrawMode; const Vertex: TOpenGLVertex2f; const Color: GL_Color; Matrix: POpenGLMatrix): Boolean;
    function DrawArrays3f(const mode: GL_DrawMode; const Vertex: TOpenGLVertex3f; const Color: GL_Colors; Matrix: POpenGLMatrix): Boolean;
    function DrawArrays2f(const mode: GL_DrawMode; const Vertex: TOpenGLVertex2f; const Color: GL_Colors; Matrix: POpenGLMatrix): Boolean;
    function DrawTransparentArrays3f(const mode: GL_DrawMode; const Vertex: TOpenGLVertex3f; const Color: GL_Color; Matrix: POpenGLMatrix): Boolean;
    function DrawTransparentArrays2f(const mode: GL_DrawMode; const Vertex: TOpenGLVertex2f; const Color: GL_Color; Matrix: POpenGLMatrix): Boolean;
    function DrawTransparentArrays3f(const mode: GL_DrawMode; const Vertex: TOpenGLVertex3f; const Color: GL_Colors; Matrix: POpenGLMatrix): Boolean;
    function DrawTransparentArrays2f(const mode: GL_DrawMode; const Vertex: TOpenGLVertex2f; const Color: GL_Colors; Matrix: POpenGLMatrix): Boolean;
    (* @} *)
    (**
      *
      * \brief Creates simple rectangle vector
      *
      * \param X Rectangle x coordinate
      * \param Y Rectangle y coordinate
      * \param W Rectangle width
      * \param H Rectangle height
      * \param Vertex Output variable for new vertex vector
      *
     **)
    (* @{ *)
    procedure MakeVertex3f(X, Y, W, H: GLfloat; out Vertex: TOpenGLVertex3f);
    procedure MakeVertex2f(X, Y, W, H: GLfloat; out Vertex: TOpenGLVertex2f);
    (* @} *)
    procedure ApplyCamera(const Matrix: POpenGLMatrix);
  end;

  TSDLOnRenderEvent = procedure(const Renderer: IRenderer) of object;

implementation

{ Vertex2f }

function Vertex2f.Make(aX, aY: GLfloat): Vertex2f;
begin
  Make(Result, aX, aY)
end;

procedure Vertex2f.Make(out V: Vertex2f; aX, aY: GLfloat);
begin
  V.x := aX;
  V.y := aY
end;

{ Vertex3f }

procedure Vertex3f.Make(out V: Vertex3f; aX, aY, aZ: GLfloat);
begin
  V.x := aX;
  V.y := aY;
  V.z := aZ
end;

procedure Vertex3f.Make(out V: Vertex3f; aX, aY: GLfloat);
begin
  Make(V, aX, aY, 0)
end;

end.

