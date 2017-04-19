
type

  IMG_InitFlags = (
      IMG_INIT_JPG = $00000001,
      IMG_INIT_PNG = $00000002,
      IMG_INIT_TIF = $00000004,
      IMG_INIT_WEBP = $00000008,
      IMG_INIT_EVERYTHING =
          Integer(IMG_INIT_JPG) or
          Integer(IMG_INIT_PNG) or
          Integer(IMG_INIT_TIF) or
          Integer(IMG_INIT_WEBP)
  );

