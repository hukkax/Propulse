
type

  PSDL_BlendMode = ^SDL_BlendMode;

  SDL_BlendMode = (
    SDL_BLENDMODE_NONE = $00000000,     (** no blending
                                              dstRGBA = srcRGBA *)
    SDL_BLENDMODE_BLEND = $00000001,    (** alpha blending
                                              dstRGB = (srcRGB * srcA) + (dstRGB * (1-srcA))
                                              dstA = srcA + (dstA * (1-srcA)) *)
    SDL_BLENDMODE_ADD = $00000002,      (** additive blending
                                              dstRGB = (srcRGB * srcA) + dstRGB
                                              dstA = dstA *)
    SDL_BLENDMODE_MOD = $00000004       (** color modulate
                                              dstRGB = srcRGB * dstRGB
                                              dstA = dstA *)
  );

