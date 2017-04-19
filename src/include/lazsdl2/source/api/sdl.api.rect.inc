
type

  PSDL_Point = ^SDL_Point;
  PSDL_Rect = ^SDL_Rect;

  (**
   *  \brief  The structure that defines a point
   *
   *  \sa SDL_EnclosePoints
   *  \sa SDL_PointInRect
   *)
  SDL_Point = record
    x: SDL_SInt32;
    y: SDL_SInt32;
  end;

  (**
   *  \brief A rectangle, with the origin at the upper left.
   *
   *  \sa SDL_RectEmpty
   *  \sa SDL_RectEquals
   *  \sa SDL_HasIntersection
   *  \sa SDL_IntersectRect
   *  \sa SDL_UnionRect
   *  \sa SDL_EnclosePoints
   *)
   SDL_Rect = record
     x: SDL_SInt32;
     y: SDL_SInt32;
     w: SDL_SInt32;
     h: SDL_SInt32;
   end;

