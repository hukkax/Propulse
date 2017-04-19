
type

  PSDL_WindowShapeMode = ^SDL_WindowShapeMode;

  (** \brief An enum denoting the specific type of contents present in an SDL_WindowShapeParams union. *)
  WindowShapeMode = (
    ShapeModeDefault, (** The default mode, a binarized alpha cutoff of 1. *)
    ShapeModeBinarizeAlpha, (** A binarized alpha cutoff with a given integer value. *)
    ShapeModeReverseBinarizeAlpha, (** A binarized alpha cutoff with a given integer value, but with the opposite comparison. *)
    ShapeModeColorKey (** A color key is applied. *)
  );

  (** \brief A union containing parameters for shaped windows. *)
  SDL_WindowShapeParams = record
    case Integer of
      0: (binarizationCutoff: SDL_UInt8;); (** a cutoff alpha value for binarization of the window shape's alpha channel. *)
      1: (colorKey: SDL_Color;);
  end;

  (** \brief A struct that tags the SDL_WindowShapeParams union with an enum describing the type of its contents. *)
  SDL_WindowShapeMode = record
    mode: WindowShapeMode; (** The mode of these window-shape parameters. *)
    parameters: SDL_WindowShapeParams; (*** Window-shape parameters. *)
  end;
