
type

  (**
   *  \brief The basic state for the system's power supply.
   *)
  SDL_PowerState = (
    SDL_POWERSTATE_UNKNOWN,      (** cannot determine power status *)
    SDL_POWERSTATE_ON_BATTERY,   (** Not plugged in, running on the battery *)
    SDL_POWERSTATE_NO_BATTERY,   (** Plugged in, no battery available *)
    SDL_POWERSTATE_CHARGING,     (** Plugged in, charging battery *)
    SDL_POWERSTATE_CHARGED       (** Plugged in, battery charged *)
  );
