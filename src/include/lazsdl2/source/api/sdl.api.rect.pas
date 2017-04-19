unit SDL.Api.Rect;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SDL.Api.Types, DLibrary;

type

  { TSDLRectApi }

  TSDLRectApi = class(TSubLibrary)
  private type
    TSDL_HasIntersection = function(const A: PSDL_Rect; const B: PSDL_Rect): SDL_Bool cdecl;
    TSDL_IntersectRect = function(const A: PSDL_Rect; const B: PSDL_Rect; result: PSDL_Rect): SDL_Bool cdecl;
    TSDL_UnionRect = procedure(const A: PSDL_Rect; const B: PSDL_Rect; result: PSDL_Rect) cdecl;
    TSDL_EnclosePoints = function(const points: PSDL_Point; count: SDL_SInt32; const clip: PSDL_Rect; result: PSDL_Rect): SDL_Bool cdecl;
    TSDL_IntersectRectAndLine = function(const rect: PSDL_Rect; X1: PSDL_SInt32; Y1: PSDL_SInt32; X2: PSDL_SInt32; Y2: PSDL_SInt32): SDL_Bool cdecl;
  public
    (**
     *  \brief Determine whether two rectangles intersect.
     *
     *  \return SDL_TRUE if there is an intersection, SDL_FALSE otherwise.
     *)
    SDL_HasIntersection: TSDL_HasIntersection;
    (**
     *  \brief Calculate the intersection of two rectangles.
     *
     *  \return SDL_TRUE if there is an intersection, SDL_FALSE otherwise.
     *)
    SDL_IntersectRect: TSDL_IntersectRect;
    (**
     *  \brief Calculate the union of two rectangles.
     *)
    SDL_UnionRect: TSDL_UnionRect;
    (**
     *  \brief Calculate a minimal rectangle enclosing a set of points
     *
     *  \return SDL_TRUE if any points were within the clipping rect
     *)
    SDL_EnclosePoints: TSDL_EnclosePoints;
    (**
     *  \brief Calculate the intersection of a rectangle and line segment.
     *
     *  \return SDL_TRUE if there is an intersection, SDL_FALSE otherwise.
     *)
    SDL_IntersectRectAndLine: TSDL_IntersectRectAndLine;
  protected
    procedure DoInit; override;
    procedure GetRequiredMethods(const List: TMethodList); override;
  public
    (**
     *  \brief Returns true if the rectangle has no area.
     *)
    function SDL_RectEmpty(X: SDL_Rect): SDL_Bool;
    (**
     *  \brief Returns true if the two rectangles are equal.
     *)
    function SDL_RectEquals(A: SDL_Rect; B: SDL_Rect): SDL_Bool;
    (**
     *  \brief Returns true if point resides inside a rectangle.
     *)
    function SDL_PointInRect(P: PSDL_Point; R: PSDL_Rect): SDL_Bool;
  end;

implementation

{ TSDLRectApi }

procedure TSDLRectApi.DoInit;
begin

end;

procedure TSDLRectApi.GetRequiredMethods(const List: TMethodList);
begin
  inherited GetRequiredMethods(List);

  List.Add('SDL_HasIntersection', @SDL_HasIntersection);
  List.Add('SDL_IntersectRect', @SDL_IntersectRect);
  List.Add('SDL_UnionRect', @SDL_UnionRect);
  List.Add('SDL_EnclosePoints', @SDL_EnclosePoints);
  List.Add('SDL_IntersectRectAndLine', @SDL_IntersectRectAndLine);
end;

function TSDLRectApi.SDL_RectEmpty(X: SDL_Rect): SDL_Bool;
begin
  Result := SDL_Bool((X.w <= 0) or (X.h <= 0))
end;

function TSDLRectApi.SDL_RectEquals(A: SDL_Rect; B: SDL_Rect): SDL_Bool;
begin
  Result := SDL_Bool((A.x = B.x) and (A.y = B.y) and (A.w = B.w) and (A.h = B.h))
end;

function TSDLRectApi.SDL_PointInRect(P: PSDL_Point; R: PSDL_Rect): SDL_Bool;
begin
  if (P^.x >= R^.x) and (P^.x <= R^.x + R^.h) and
     (P^.y >= R^.y) and (P^.y <= R^.y + R^.w) then
    Result := SDL_TRUE
  else
    Result := SDL_FALSE
end;

end.

