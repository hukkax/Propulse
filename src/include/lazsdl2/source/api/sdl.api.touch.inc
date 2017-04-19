
type

  PSDL_TouchID  = ^SDL_TouchID;
  PSDL_Finger = ^SDL_Finger;
  PSDL_FingerID = ^SDL_FingerID;

  SDL_TouchID  = SDL_SInt64;
  SDL_FingerID = SDL_SInt64;

  SDL_Finger = record
    id: SDL_FingerID;
    x: SDL_Float;
    y: SDL_Float;
    pressure: SDL_Float;
  end;
