

type

  (**
   *  \brief The predefined log categories
   *
   *  By default the application category is enabled at the INFO level,
   *  the assert category is enabled at the WARN level, test is enabled
   *  at the VERBOSE level and all other categories are enabled at the
   *  CRITICAL level.
   *)
  SDL_LogCategory = (
      SDL_LOG_CATEGORY_APPLICATION,
      SDL_LOG_CATEGORY_ERROR,
      SDL_LOG_CATEGORY_ASSERT,
      SDL_LOG_CATEGORY_SYSTEM,
      SDL_LOG_CATEGORY_AUDIO,
      SDL_LOG_CATEGORY_VIDEO,
      SDL_LOG_CATEGORY_RENDER,
      SDL_LOG_CATEGORY_INPUT,
      SDL_LOG_CATEGORY_TEST,

      (* Reserved for future SDL library use *)
      SDL_LOG_CATEGORY_RESERVED1,
      SDL_LOG_CATEGORY_RESERVED2,
      SDL_LOG_CATEGORY_RESERVED3,
      SDL_LOG_CATEGORY_RESERVED4,
      SDL_LOG_CATEGORY_RESERVED5,
      SDL_LOG_CATEGORY_RESERVED6,
      SDL_LOG_CATEGORY_RESERVED7,
      SDL_LOG_CATEGORY_RESERVED8,
      SDL_LOG_CATEGORY_RESERVED9,
      SDL_LOG_CATEGORY_RESERVED10,

      (** Beyond this point is reserved for application use, e.g.
         enum {
             MYAPP_CATEGORY_AWESOME1 = SDL_LOG_CATEGORY_CUSTOM,
             MYAPP_CATEGORY_AWESOME2,
             MYAPP_CATEGORY_AWESOME3,
             ...
         };
       *)
      SDL_LOG_CATEGORY_CUSTOM
  );

  (**
   *  \brief The predefined log priorities
   *)
  SDL_LogPriority = (
      SDL_LOG_PRIORITY_VERBOSE = 1,
      SDL_LOG_PRIORITY_DEBUG,
      SDL_LOG_PRIORITY_INFO,
      SDL_LOG_PRIORITY_WARN,
      SDL_LOG_PRIORITY_ERROR,
      SDL_LOG_PRIORITY_CRITICAL,
      SDL_NUM_LOG_PRIORITIES
  );

