unit MainWnd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SDL.Extended.Window, SDL.Extended.Renderer,
  SDL.Extended.Camera, SDL.Extended.Types, SDL.Api.OpenGL.Types;

type

  { TSDLWindow1 }

  TSDLWindow1 = class(TSDLWindow)
    SDLCamera1: TSDLCamera;
    SDLRenderer1: TSDLRenderer;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleInitOpenGL(const Sender: TObject);
    procedure DataModuleMinimize(Sender: TObject);
    procedure DataModuleRender(const Sender: TObject);
    procedure DataModuleRestore(Sender: TObject);
    procedure SDLRenderer1Render(const Renderer: IRenderer);
  private
    FInitialized: Boolean;
    Running: Boolean;
    Colors1: GL_Colors;
    Colors2: GL_Colors;
    Colors3: GL_Colors;
    Matrix: TOpenGLMatrix;
    Vertex1a: TOpenGLVertex2f;
    Vertex1b: TOpenGLVertex2f;
    Vertex1c: TOpenGLVertex3f;
    Vertex1d: TOpenGLVertex3f;
    Vertex1e: TOpenGLVertex3f;
    Vertex1f: TOpenGLVertex3f;

    Vertex2a: TOpenGLVertex2f;
    Vertex2b: TOpenGLVertex2f;
    Vertex2c: TOpenGLVertex2f;
    Vertex2d: TOpenGLVertex2f;
    Vertex2e: TOpenGLVertex3f;
    Vertex2f: TOpenGLVertex3f;
    Vertex2g: TOpenGLVertex3f;
  public
    { public declarations }
  end;

var
  SDLWindow1: TSDLWindow1;

implementation

uses
  SDL.Extended.Application, Graphics;

{$R *.lfm}

{ TSDLWindow1 }

procedure TSDLWindow1.DataModuleCreate(Sender: TObject);
begin
  FInitialized := False;
  Running := True;
end;

procedure TSDLWindow1.DataModuleInitOpenGL(const Sender: TObject);
begin
  OpenGL.glViewport(0, 0, Round(Size.Width), Round(Size.Height));
  OpenGL.glBlendFunc (GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
end;

procedure TSDLWindow1.DataModuleMinimize(Sender: TObject);
begin
  Running := False;
end;

procedure TSDLWindow1.DataModuleRender(const Sender: TObject);
begin

end;

procedure TSDLWindow1.DataModuleRestore(Sender: TObject);
begin
  Running := True;
end;

procedure TSDLWindow1.SDLRenderer1Render(const Renderer: IRenderer);
begin
  if not FInitialized then
  begin
    FInitialized := True;
    Renderer.ApplyCamera(@Matrix);

    Renderer.MakeVertex2f(-5.0, 0, 1, 1, Vertex1a);
    Renderer.MakeVertex2f(-3.5, 0, 1, 1, Vertex1b);
    Renderer.MakeVertex3f(-2.0, 0, 1, 1, Vertex1c);
    Renderer.MakeVertex3f(-0.5, 0, 1, 1, Vertex1d);
    Renderer.MakeVertex3f( 1.0, 0, 1, 1, Vertex1e);
    Renderer.MakeVertex3f( 1.5, 0, 1, 1, Vertex1f);

    Renderer.MakeVertex2f(-5.0, 2, 1, 1, Vertex2a);
    Renderer.MakeVertex2f(-3.5, 2, 1, 1, Vertex2b);
    Renderer.MakeVertex2f(-2.0, 2, 1, 1, Vertex2c);
    Renderer.MakeVertex2f(-0.5, 2, 1, 1, Vertex2d);
    Renderer.MakeVertex3f( 1.0, 2, 1, 1, Vertex2e);
    Renderer.MakeVertex3f( 2.5, 2, 1, 1, Vertex2g);
    Renderer.MakeVertex3f( 4.0, 2, 1, 1, Vertex2f);

    SetLength(Colors1, 4);
    Colors1[0] := GL_Color.Make(1.0, 0, 0, 1);
    Colors1[1] := GL_Color.Make(0.0, 1, 0, 1);
    Colors1[2] := GL_Color.Make(0.0, 0, 1, 1);
    Colors1[3] := GL_Color.Make(0.0, 1, 1, 1);

    SetLength(Colors2, 4);
    Colors2[0] := GL_Color.Make(255, 242, 0, 255);
    Colors2[1] := GL_Color.Make(255, 0, 0, 255);
    Colors2[2] := GL_Color.Make(255, 242, 0, 255);
    Colors2[3] := GL_Color.Make(255, 0, 0, 255);

    SetLength(Colors3, 4);
    Colors3[0] := GL_Color.Make(clBlue, 255);
    Colors3[1] := GL_Color.Make(clBlue, 255);
    Colors3[2] := GL_Color.Make(clGreen, 255);
    Colors3[3] := GL_Color.Make(clGreen, 255);
  end;

  if not Running then
    Exit;
       ApplicationName;
  Renderer.DrawArrays2f(GL_POINTS, Vertex2a, GL_Color.Make(255, 0, 0, 255), @Matrix);
  Renderer.DrawArrays2f(GL_LINES, Vertex2b, GL_Color.Make(0, 96, 0, 255), @Matrix);
  Renderer.DrawArrays2f(GL_LINE_STRIP, Vertex2c, GL_Color.Make(0, 168, 0, 255), @Matrix);
  Renderer.DrawArrays2f(GL_LINE_LOOP, Vertex2d, Colors1, @Matrix);
  Renderer.DrawArrays3f(GL_TRIANGLES, Vertex2e, GL_Color.Make(0, 0, 96, 255), @Matrix);
  Renderer.DrawArrays3f(GL_TRIANGLE_STRIP, Vertex2f, GL_Color.Make(0, 0, 168, 255), @Matrix);
  Renderer.DrawArrays3f(GL_TRIANGLE_FAN, Vertex2g, Colors3, @Matrix);

  Renderer.DrawRectangle2f(Vertex1a, GL_Color.Make(255, 242, 0, 255), @Matrix);
  Renderer.DrawRectangle2f(Vertex1b, Colors1, @Matrix);
  Renderer.DrawRectangle3f(Vertex1c, GL_Color.Make(128, 128, 255, 255), @Matrix);
  Renderer.DrawRectangle3f(Vertex1d, Colors2, @Matrix);
  OpenGL.glEnable(GL_BLEND);
  OpenGL.glEnable(GL_ALPHA_TEST);
  Renderer.DrawRectangle3f(Vertex1e, GL_Color.Make(clRed, 255), @Matrix);
  Renderer.DrawRectangle3f(Vertex1f, GL_Color.Make(clMoneyGreen, 128), @Matrix);
  OpenGL.glDisable(GL_BLEND);
  OpenGL.glDisable(GL_ALPHA_TEST);
end;


initialization
  Application.RegisterWindow(TSDLWindow1, True);
end.

