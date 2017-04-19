unit SDL.Extended.OpenGLExtensions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, SDL.Extended.LibObject,
  SDL.Extended.LibraryProvider;

type

  TSDLExtensionOption = (extOptional, extRequired, extIgnore);
  TSDLExtensionLoadEvent = procedure(const Sender: TObject; const Extension: String; Option: TSDLExtensionOption) of object;

  { TSDLCustomOpenGLExtensions }

  TSDLCustomOpenGLExtensions = class(TSDLWindowedObject)
  private type

    TSDLLoadMethod = function: Boolean;

    { TSDLOpenGLExtension }

    TSDLOpenGLExtension = class(TPersistent)
    strict private
      FOwner: TSDLCustomOpenGLExtensions;
      function GetProvider: TSDLCustomLibraryProvider;
    protected
      function DefaultOption: String; virtual; abstract;
      procedure Validate(ExtName: String; Method: TSDLLoadMethod; Option: TSDLExtensionOption);
    public
      constructor Create(Owner: TSDLCustomOpenGLExtensions); reintroduce;

      procedure Load; virtual; abstract;

      property Provider: TSDLCustomLibraryProvider read GetProvider;
    end;

    { TSDLOpenGLVersions }

    TSDLOpenGLVersions = class(TSDLOpenGLExtension)
    private
      FGL_version_1_2: TSDLExtensionOption;
      FGL_version_1_3: TSDLExtensionOption;
      FGL_version_1_4: TSDLExtensionOption;
      FGL_version_1_5: TSDLExtensionOption;
      FGL_version_2_0: TSDLExtensionOption;
      FGL_version_2_1: TSDLExtensionOption;
      FGL_version_3_0: TSDLExtensionOption;
      FGL_version_3_1: TSDLExtensionOption;
      FGL_version_3_2: TSDLExtensionOption;
      FGL_version_3_3: TSDLExtensionOption;
      FGL_version_4_0: TSDLExtensionOption;
      procedure SetGL_version_1_2(AValue: TSDLExtensionOption);
      procedure SetGL_version_1_3(AValue: TSDLExtensionOption);
      procedure SetGL_version_1_4(AValue: TSDLExtensionOption);
      procedure SetGL_version_1_5(AValue: TSDLExtensionOption);
      procedure SetGL_version_2_0(AValue: TSDLExtensionOption);
      procedure SetGL_version_2_1(AValue: TSDLExtensionOption);
      procedure SetGL_version_3_0(AValue: TSDLExtensionOption);
      procedure SetGL_version_3_1(AValue: TSDLExtensionOption);
      procedure SetGL_version_3_2(AValue: TSDLExtensionOption);
      procedure SetGL_version_3_3(AValue: TSDLExtensionOption);
      procedure SetGL_version_4_0(AValue: TSDLExtensionOption);
    protected
      function DefaultOption: String; override;
    public
      procedure Load; override;
    published
      property GL_version_1_2: TSDLExtensionOption read FGL_version_1_2 write SetGL_version_1_2;
      property GL_version_1_3: TSDLExtensionOption read FGL_version_1_3 write SetGL_version_1_3;
      property GL_version_1_4: TSDLExtensionOption read FGL_version_1_4 write SetGL_version_1_4;
      property GL_version_1_5: TSDLExtensionOption read FGL_version_1_5 write SetGL_version_1_5;
      property GL_version_2_0: TSDLExtensionOption read FGL_version_2_0 write SetGL_version_2_0;
      property GL_version_2_1: TSDLExtensionOption read FGL_version_2_1 write SetGL_version_2_1;
      property GL_version_3_0: TSDLExtensionOption read FGL_version_3_0 write SetGL_version_3_0;
      property GL_version_3_1: TSDLExtensionOption read FGL_version_3_1 write SetGL_version_3_1;
      property GL_version_3_2: TSDLExtensionOption read FGL_version_3_2 write SetGL_version_3_2;
      property GL_version_3_3: TSDLExtensionOption read FGL_version_3_3 write SetGL_version_3_3;
      property GL_version_4_0: TSDLExtensionOption read FGL_version_4_0 write SetGL_version_4_0;
    end;

    { TSDLOpenGLExtensionsWindows }

    TSDLOpenGLExtensionsWindows = class(TSDLOpenGLExtension)
    private
      FWGL_ARB_buffer_region: TSDLExtensionOption;
      FWGL_ARB_extensions_string: TSDLExtensionOption;
      FWGL_ARB_make_current_read: TSDLExtensionOption;
      FWGL_ARB_pbuffer: TSDLExtensionOption;
      FWGL_ARB_pixel_format: TSDLExtensionOption;
      FWGL_ARB_render_texture: TSDLExtensionOption;
      FWGL_ATI_pixel_format_float: TSDLExtensionOption;
      FWGL_EXT_extensions_string: TSDLExtensionOption;
      FWGL_EXT_make_current_read: TSDLExtensionOption;
      FWGL_EXT_pbuffer: TSDLExtensionOption;
      FWGL_EXT_pixel_format: TSDLExtensionOption;
      FWGL_EXT_swap_control: TSDLExtensionOption;
      FWGL_I3D_digital_video_control: TSDLExtensionOption;
      FWGL_I3D_gamma: TSDLExtensionOption;
      FWGL_I3D_genlock: TSDLExtensionOption;
      FWGL_I3D_image_buffer: TSDLExtensionOption;
      FWGL_I3D_swap_frame_lock: TSDLExtensionOption;
      FWGL_I3D_swap_frame_usage: TSDLExtensionOption;
      FWGL_NV_render_texture_rectangle: TSDLExtensionOption;
      procedure SetWGL_ARB_buffer_region(AValue: TSDLExtensionOption);
      procedure SetWGL_ARB_extensions_string(AValue: TSDLExtensionOption);
      procedure SetWGL_ARB_make_current_read(AValue: TSDLExtensionOption);
      procedure SetWGL_ARB_pbuffer(AValue: TSDLExtensionOption);
      procedure SetWGL_ARB_pixel_format(AValue: TSDLExtensionOption);
      procedure SetWGL_ARB_render_texture(AValue: TSDLExtensionOption);
      procedure SetWGL_ATI_pixel_format_float(AValue: TSDLExtensionOption);
      procedure SetWGL_EXT_extensions_string(AValue: TSDLExtensionOption);
      procedure SetWGL_EXT_make_current_read(AValue: TSDLExtensionOption);
      procedure SetWGL_EXT_pbuffer(AValue: TSDLExtensionOption);
      procedure SetWGL_EXT_pixel_format(AValue: TSDLExtensionOption);
      procedure SetWGL_EXT_swap_control(AValue: TSDLExtensionOption);
      procedure SetWGL_I3D_digital_video_control(AValue: TSDLExtensionOption);
      procedure SetWGL_I3D_gamma(AValue: TSDLExtensionOption);
      procedure SetWGL_I3D_genlock(AValue: TSDLExtensionOption);
      procedure SetWGL_I3D_image_buffer(AValue: TSDLExtensionOption);
      procedure SetWGL_I3D_swap_frame_lock(AValue: TSDLExtensionOption);
      procedure SetWGL_I3D_swap_frame_usage(AValue: TSDLExtensionOption);
      procedure SetWGL_NV_render_texture_rectangle(AValue: TSDLExtensionOption);
    protected
      function DefaultOption: String; override;
    public
      procedure Load; override;
    published
      property WGL_ARB_buffer_region: TSDLExtensionOption read FWGL_ARB_buffer_region write SetWGL_ARB_buffer_region;
      property WGL_ARB_extensions_string: TSDLExtensionOption read FWGL_ARB_extensions_string write SetWGL_ARB_extensions_string;
      property WGL_ARB_make_current_read: TSDLExtensionOption read FWGL_ARB_make_current_read write SetWGL_ARB_make_current_read;
      property WGL_ARB_pbuffer: TSDLExtensionOption read FWGL_ARB_pbuffer write SetWGL_ARB_pbuffer;
      property WGL_ARB_pixel_format: TSDLExtensionOption read FWGL_ARB_pixel_format write SetWGL_ARB_pixel_format;
      property WGL_ARB_render_texture: TSDLExtensionOption read FWGL_ARB_render_texture write SetWGL_ARB_render_texture;
      property WGL_ATI_pixel_format_float: TSDLExtensionOption read FWGL_ATI_pixel_format_float write SetWGL_ATI_pixel_format_float;
      property WGL_EXT_extensions_string: TSDLExtensionOption read FWGL_EXT_extensions_string write SetWGL_EXT_extensions_string;
      property WGL_EXT_make_current_read: TSDLExtensionOption read FWGL_EXT_make_current_read write SetWGL_EXT_make_current_read;
      property WGL_EXT_pbuffer: TSDLExtensionOption read FWGL_EXT_pbuffer write SetWGL_EXT_pbuffer;
      property WGL_EXT_pixel_format: TSDLExtensionOption read FWGL_EXT_pixel_format write SetWGL_EXT_pixel_format;
      property WGL_EXT_swap_control: TSDLExtensionOption read FWGL_EXT_swap_control write SetWGL_EXT_swap_control;
      property WGL_I3D_digital_video_control: TSDLExtensionOption read FWGL_I3D_digital_video_control write SetWGL_I3D_digital_video_control;
      property WGL_I3D_gamma: TSDLExtensionOption read FWGL_I3D_gamma write SetWGL_I3D_gamma;
      property WGL_I3D_genlock: TSDLExtensionOption read FWGL_I3D_genlock write SetWGL_I3D_genlock;
      property WGL_I3D_image_buffer: TSDLExtensionOption read FWGL_I3D_image_buffer write SetWGL_I3D_image_buffer;
      property WGL_I3D_swap_frame_lock: TSDLExtensionOption read FWGL_I3D_swap_frame_lock write SetWGL_I3D_swap_frame_lock;
      property WGL_I3D_swap_frame_usage: TSDLExtensionOption read FWGL_I3D_swap_frame_usage write SetWGL_I3D_swap_frame_usage;
      property WGL_NV_render_texture_rectangle: TSDLExtensionOption read FWGL_NV_render_texture_rectangle write SetWGL_NV_render_texture_rectangle;
    end;

    { TSDLOpenGLExtensionsExt }

    TSDLOpenGLExtensionsExt = class(TSDLOpenGLExtension)
    private
      FGL_EXT_422_pixels: TSDLExtensionOption;
      FGL_EXT_abgr: TSDLExtensionOption;
      FGL_EXT_bgra: TSDLExtensionOption;
      FGL_EXT_blend_color: TSDLExtensionOption;
      FGL_EXT_blend_equation_separate: TSDLExtensionOption;
      FGL_EXT_blend_func_separate: TSDLExtensionOption;
      FGL_EXT_blend_logic_op: TSDLExtensionOption;
      FGL_EXT_blend_minmax: TSDLExtensionOption;
      FGL_EXT_blend_subtract: TSDLExtensionOption;
      FGL_EXT_clip_volume_hint: TSDLExtensionOption;
      FGL_EXT_color_subtable: TSDLExtensionOption;
      FGL_EXT_compiled_vertex_array: TSDLExtensionOption;
      FGL_EXT_convolution: TSDLExtensionOption;
      FGL_EXT_depth_bounds_test: TSDLExtensionOption;
      FGL_EXT_fog_coord: TSDLExtensionOption;
      FGL_EXT_framebuffer_object: TSDLExtensionOption;
      FGL_EXT_histogram: TSDLExtensionOption;
      FGL_EXT_multi_draw_arrays: TSDLExtensionOption;
      FGL_EXT_packed_pixels: TSDLExtensionOption;
      FGL_EXT_paletted_texture: TSDLExtensionOption;
      FGL_EXT_pixel_buffer_object: TSDLExtensionOption;
      FGL_EXT_point_parameters: TSDLExtensionOption;
      FGL_EXT_polygon_offset: TSDLExtensionOption;
      FGL_EXT_secondary_color: TSDLExtensionOption;
      FGL_EXT_separate_specular_color: TSDLExtensionOption;
      FGL_EXT_shadow_funcs: TSDLExtensionOption;
      FGL_EXT_shared_texture_palette: TSDLExtensionOption;
      FGL_EXT_stencil_two_side: TSDLExtensionOption;
      FGL_EXT_stencil_wrap: TSDLExtensionOption;
      FGL_EXT_subtexture: TSDLExtensionOption;
      FGL_EXT_texture3D: TSDLExtensionOption;
      FGL_EXT_texture_compression_dxt1: TSDLExtensionOption;
      FGL_EXT_texture_compression_s3tc: TSDLExtensionOption;
      FGL_EXT_texture_env_add: TSDLExtensionOption;
      FGL_EXT_texture_env_combine: TSDLExtensionOption;
      FGL_EXT_texture_env_dot3: TSDLExtensionOption;
      FGL_EXT_texture_filter_anisotropic: TSDLExtensionOption;
      FGL_EXT_texture_lod_bias: TSDLExtensionOption;
      FGL_EXT_texture_mirror_clamp: TSDLExtensionOption;
      FGL_EXT_texture_object: TSDLExtensionOption;
      FGL_EXT_texture_rectangle: TSDLExtensionOption;
      FGL_EXT_vertex_array: TSDLExtensionOption;
      FGL_EXT_vertex_shader: TSDLExtensionOption;
      FGL_EXT_vertex_weighting: TSDLExtensionOption;
      procedure SetGL_EXT_422_pixels(AValue: TSDLExtensionOption);
      procedure SetGL_EXT_abgr(AValue: TSDLExtensionOption);
      procedure SetGL_EXT_bgra(AValue: TSDLExtensionOption);
      procedure SetGL_EXT_blend_color(AValue: TSDLExtensionOption);
      procedure SetGL_EXT_blend_equation_separate(AValue: TSDLExtensionOption);
      procedure SetGL_EXT_blend_func_separate(AValue: TSDLExtensionOption);
      procedure SetGL_EXT_blend_logic_op(AValue: TSDLExtensionOption);
      procedure SetGL_EXT_blend_minmax(AValue: TSDLExtensionOption);
      procedure SetGL_EXT_blend_subtract(AValue: TSDLExtensionOption);
      procedure SetGL_EXT_clip_volume_hint(AValue: TSDLExtensionOption);
      procedure SetGL_EXT_color_subtable(AValue: TSDLExtensionOption);
      procedure SetGL_EXT_compiled_vertex_array(AValue: TSDLExtensionOption);
      procedure SetGL_EXT_convolution(AValue: TSDLExtensionOption);
      procedure SetGL_EXT_depth_bounds_test(AValue: TSDLExtensionOption);
      procedure SetGL_EXT_fog_coord(AValue: TSDLExtensionOption);
      procedure SetGL_EXT_framebuffer_object(AValue: TSDLExtensionOption);
      procedure SetGL_EXT_histogram(AValue: TSDLExtensionOption);
      procedure SetGL_EXT_multi_draw_arrays(AValue: TSDLExtensionOption);
      procedure SetGL_EXT_packed_pixels(AValue: TSDLExtensionOption);
      procedure SetGL_EXT_paletted_texture(AValue: TSDLExtensionOption);
      procedure SetGL_EXT_pixel_buffer_object(AValue: TSDLExtensionOption);
      procedure SetGL_EXT_point_parameters(AValue: TSDLExtensionOption);
      procedure SetGL_EXT_polygon_offset(AValue: TSDLExtensionOption);
      procedure SetGL_EXT_secondary_color(AValue: TSDLExtensionOption);
      procedure SetGL_EXT_separate_specular_color(AValue: TSDLExtensionOption);
      procedure SetGL_EXT_shadow_funcs(AValue: TSDLExtensionOption);
      procedure SetGL_EXT_shared_texture_palette(AValue: TSDLExtensionOption);
      procedure SetGL_EXT_stencil_two_side(AValue: TSDLExtensionOption);
      procedure SetGL_EXT_stencil_wrap(AValue: TSDLExtensionOption);
      procedure SetGL_EXT_subtexture(AValue: TSDLExtensionOption);
      procedure SetGL_EXT_texture3D(AValue: TSDLExtensionOption);
      procedure SetGL_EXT_texture_compression_dxt1(AValue: TSDLExtensionOption);
      procedure SetGL_EXT_texture_compression_s3tc(AValue: TSDLExtensionOption);
      procedure SetGL_EXT_texture_env_add(AValue: TSDLExtensionOption);
      procedure SetGL_EXT_texture_env_combine(AValue: TSDLExtensionOption);
      procedure SetGL_EXT_texture_env_dot3(AValue: TSDLExtensionOption);
      procedure SetGL_EXT_texture_filter_anisotropic(AValue: TSDLExtensionOption);
      procedure SetGL_EXT_texture_lod_bias(AValue: TSDLExtensionOption);
      procedure SetGL_EXT_texture_mirror_clamp(AValue: TSDLExtensionOption);
      procedure SetGL_EXT_texture_object(AValue: TSDLExtensionOption);
      procedure SetGL_EXT_texture_rectangle(AValue: TSDLExtensionOption);
      procedure SetGL_EXT_vertex_array(AValue: TSDLExtensionOption);
      procedure SetGL_EXT_vertex_shader(AValue: TSDLExtensionOption);
      procedure SetGL_EXT_vertex_weighting(AValue: TSDLExtensionOption);
    protected
      function DefaultOption: String; override;
    public
      procedure AfterConstruction; override;
      procedure Load; override;
    published
      property GL_EXT_422_pixels: TSDLExtensionOption read FGL_EXT_422_pixels write SetGL_EXT_422_pixels;
      property GL_EXT_abgr: TSDLExtensionOption read FGL_EXT_abgr write SetGL_EXT_abgr;
      property GL_EXT_bgra: TSDLExtensionOption read FGL_EXT_bgra write SetGL_EXT_bgra;
      property GL_EXT_blend_color: TSDLExtensionOption read FGL_EXT_blend_color write SetGL_EXT_blend_color;
      property GL_EXT_blend_equation_separate: TSDLExtensionOption read FGL_EXT_blend_equation_separate write SetGL_EXT_blend_equation_separate;
      property GL_EXT_blend_func_separate: TSDLExtensionOption read FGL_EXT_blend_func_separate write SetGL_EXT_blend_func_separate;
      property GL_EXT_blend_logic_op: TSDLExtensionOption read FGL_EXT_blend_logic_op write SetGL_EXT_blend_logic_op;
      property GL_EXT_blend_minmax: TSDLExtensionOption read FGL_EXT_blend_minmax write SetGL_EXT_blend_minmax;
      property GL_EXT_blend_subtract: TSDLExtensionOption read FGL_EXT_blend_subtract write SetGL_EXT_blend_subtract;
      property GL_EXT_clip_volume_hint: TSDLExtensionOption read FGL_EXT_clip_volume_hint write SetGL_EXT_clip_volume_hint;
      property GL_EXT_color_subtable: TSDLExtensionOption read FGL_EXT_color_subtable write SetGL_EXT_color_subtable;
      property GL_EXT_compiled_vertex_array: TSDLExtensionOption read FGL_EXT_compiled_vertex_array write SetGL_EXT_compiled_vertex_array;
      property GL_EXT_convolution: TSDLExtensionOption read FGL_EXT_convolution write SetGL_EXT_convolution;
      property GL_EXT_depth_bounds_test: TSDLExtensionOption read FGL_EXT_depth_bounds_test write SetGL_EXT_depth_bounds_test;
      property GL_EXT_fog_coord: TSDLExtensionOption read FGL_EXT_fog_coord write SetGL_EXT_fog_coord;
      property GL_EXT_framebuffer_object: TSDLExtensionOption read FGL_EXT_framebuffer_object write SetGL_EXT_framebuffer_object;
      property GL_EXT_histogram: TSDLExtensionOption read FGL_EXT_histogram write SetGL_EXT_histogram;
      property GL_EXT_multi_draw_arrays: TSDLExtensionOption read FGL_EXT_multi_draw_arrays write SetGL_EXT_multi_draw_arrays;
      property GL_EXT_packed_pixels: TSDLExtensionOption read FGL_EXT_packed_pixels write SetGL_EXT_packed_pixels;
      property GL_EXT_paletted_texture: TSDLExtensionOption read FGL_EXT_paletted_texture write SetGL_EXT_paletted_texture;
      property GL_EXT_pixel_buffer_object: TSDLExtensionOption read FGL_EXT_pixel_buffer_object write SetGL_EXT_pixel_buffer_object;
      property GL_EXT_point_parameters: TSDLExtensionOption read FGL_EXT_point_parameters write SetGL_EXT_point_parameters;
      property GL_EXT_polygon_offset: TSDLExtensionOption read FGL_EXT_polygon_offset write SetGL_EXT_polygon_offset;
      property GL_EXT_secondary_color: TSDLExtensionOption read FGL_EXT_secondary_color write SetGL_EXT_secondary_color;
      property GL_EXT_separate_specular_color: TSDLExtensionOption read FGL_EXT_separate_specular_color write SetGL_EXT_separate_specular_color;
      property GL_EXT_shadow_funcs: TSDLExtensionOption read FGL_EXT_shadow_funcs write SetGL_EXT_shadow_funcs;
      property GL_EXT_shared_texture_palette: TSDLExtensionOption read FGL_EXT_shared_texture_palette write SetGL_EXT_shared_texture_palette;
      property GL_EXT_stencil_two_side: TSDLExtensionOption read FGL_EXT_stencil_two_side write SetGL_EXT_stencil_two_side;
      property GL_EXT_stencil_wrap: TSDLExtensionOption read FGL_EXT_stencil_wrap write SetGL_EXT_stencil_wrap;
      property GL_EXT_subtexture: TSDLExtensionOption read FGL_EXT_subtexture write SetGL_EXT_subtexture;
      property GL_EXT_texture3D: TSDLExtensionOption read FGL_EXT_texture3D write SetGL_EXT_texture3D;
      property GL_EXT_texture_compression_dxt1: TSDLExtensionOption read FGL_EXT_texture_compression_dxt1 write SetGL_EXT_texture_compression_dxt1;
      property GL_EXT_texture_compression_s3tc: TSDLExtensionOption read FGL_EXT_texture_compression_s3tc write SetGL_EXT_texture_compression_s3tc;
      property GL_EXT_texture_env_add: TSDLExtensionOption read FGL_EXT_texture_env_add write SetGL_EXT_texture_env_add;
      property GL_EXT_texture_env_combine: TSDLExtensionOption read FGL_EXT_texture_env_combine write SetGL_EXT_texture_env_combine;
      property GL_EXT_texture_env_dot3: TSDLExtensionOption read FGL_EXT_texture_env_dot3 write SetGL_EXT_texture_env_dot3;
      property GL_EXT_texture_filter_anisotropic: TSDLExtensionOption read FGL_EXT_texture_filter_anisotropic write SetGL_EXT_texture_filter_anisotropic;
      property GL_EXT_texture_lod_bias: TSDLExtensionOption read FGL_EXT_texture_lod_bias write SetGL_EXT_texture_lod_bias;
      property GL_EXT_texture_mirror_clamp: TSDLExtensionOption read FGL_EXT_texture_mirror_clamp write SetGL_EXT_texture_mirror_clamp;
      property GL_EXT_texture_object: TSDLExtensionOption read FGL_EXT_texture_object write SetGL_EXT_texture_object;
      property GL_EXT_texture_rectangle: TSDLExtensionOption read FGL_EXT_texture_rectangle write SetGL_EXT_texture_rectangle;
      property GL_EXT_vertex_array: TSDLExtensionOption read FGL_EXT_vertex_array write SetGL_EXT_vertex_array;
      property GL_EXT_vertex_shader: TSDLExtensionOption read FGL_EXT_vertex_shader write SetGL_EXT_vertex_shader;
      property GL_EXT_vertex_weighting: TSDLExtensionOption read FGL_EXT_vertex_weighting write SetGL_EXT_vertex_weighting;
    end;

    { TSDLOpenGLExtensionsARB }

    TSDLOpenGLExtensionsARB = class(TSDLOpenGLExtension)
    private
      FGL_ARB_color_buffer_float: TSDLExtensionOption;
      FGL_ARB_depth_texture: TSDLExtensionOption;
      FGL_ARB_draw_buffers: TSDLExtensionOption;
      FGL_ARB_fragment_program: TSDLExtensionOption;
      FGL_ARB_fragment_program_shadow: TSDLExtensionOption;
      FGL_ARB_fragment_shader: TSDLExtensionOption;
      FGL_ARB_half_float_pixel: TSDLExtensionOption;
      FGL_ARB_imaging: TSDLExtensionOption;
      FGL_ARB_matrix_palette: TSDLExtensionOption;
      FGL_ARB_multisample: TSDLExtensionOption;
      FGL_ARB_multitexture: TSDLExtensionOption;
      FGL_ARB_occlusion_query: TSDLExtensionOption;
      FGL_ARB_pixel_buffer_object: TSDLExtensionOption;
      FGL_ARB_point_parameters: TSDLExtensionOption;
      FGL_ARB_point_sprite: TSDLExtensionOption;
      FGL_ARB_shader_objects: TSDLExtensionOption;
      FGL_ARB_shading_language_100: TSDLExtensionOption;
      FGL_ARB_shadow: TSDLExtensionOption;
      FGL_ARB_shadow_ambient: TSDLExtensionOption;
      FGL_ARB_texture_border_clamp: TSDLExtensionOption;
      FGL_ARB_texture_compression: TSDLExtensionOption;
      FGL_ARB_texture_cube_map: TSDLExtensionOption;
      FGL_ARB_texture_env_add: TSDLExtensionOption;
      FGL_ARB_texture_env_combine: TSDLExtensionOption;
      FGL_ARB_texture_env_crossbar: TSDLExtensionOption;
      FGL_ARB_texture_env_dot3: TSDLExtensionOption;
      FGL_ARB_texture_float: TSDLExtensionOption;
      FGL_ARB_texture_mirrored_repeat: TSDLExtensionOption;
      FGL_ARB_texture_non_power_of_two: TSDLExtensionOption;
      FGL_ARB_texture_rectangle: TSDLExtensionOption;
      FGL_ARB_transpose_matrix: TSDLExtensionOption;
      FGL_ARB_vertex_blend: TSDLExtensionOption;
      FGL_ARB_vertex_buffer_object: TSDLExtensionOption;
      FGL_ARB_vertex_program: TSDLExtensionOption;
      FGL_ARB_vertex_shader: TSDLExtensionOption;
      FGL_ARB_window_pos: TSDLExtensionOption;
      procedure SetGL_ARB_color_buffer_float(AValue: TSDLExtensionOption);
      procedure SetGL_ARB_depth_texture(AValue: TSDLExtensionOption);
      procedure SetGL_ARB_draw_buffers(AValue: TSDLExtensionOption);
      procedure SetGL_ARB_fragment_program(AValue: TSDLExtensionOption);
      procedure SetGL_ARB_fragment_program_shadow(AValue: TSDLExtensionOption);
      procedure SetGL_ARB_fragment_shader(AValue: TSDLExtensionOption);
      procedure SetGL_ARB_half_float_pixel(AValue: TSDLExtensionOption);
      procedure SetGL_ARB_imaging(AValue: TSDLExtensionOption);
      procedure SetGL_ARB_matrix_palette(AValue: TSDLExtensionOption);
      procedure SetGL_ARB_multisample(AValue: TSDLExtensionOption);
      procedure SetGL_ARB_multitexture(AValue: TSDLExtensionOption);
      procedure SetGL_ARB_occlusion_query(AValue: TSDLExtensionOption);
      procedure SetGL_ARB_pixel_buffer_object(AValue: TSDLExtensionOption);
      procedure SetGL_ARB_point_parameters(AValue: TSDLExtensionOption);
      procedure SetGL_ARB_point_sprite(AValue: TSDLExtensionOption);
      procedure SetGL_ARB_shader_objects(AValue: TSDLExtensionOption);
      procedure SetGL_ARB_shading_language_100(AValue: TSDLExtensionOption);
      procedure SetGL_ARB_shadow(AValue: TSDLExtensionOption);
      procedure SetGL_ARB_shadow_ambient(AValue: TSDLExtensionOption);
      procedure SetGL_ARB_texture_border_clamp(AValue: TSDLExtensionOption);
      procedure SetGL_ARB_texture_compression(AValue: TSDLExtensionOption);
      procedure SetGL_ARB_texture_cube_map(AValue: TSDLExtensionOption);
      procedure SetGL_ARB_texture_env_add(AValue: TSDLExtensionOption);
      procedure SetGL_ARB_texture_env_combine(AValue: TSDLExtensionOption);
      procedure SetGL_ARB_texture_env_crossbar(AValue: TSDLExtensionOption);
      procedure SetGL_ARB_texture_env_dot3(AValue: TSDLExtensionOption);
      procedure SetGL_ARB_texture_float(AValue: TSDLExtensionOption);
      procedure SetGL_ARB_texture_mirrored_repeat(AValue: TSDLExtensionOption);
      procedure SetGL_ARB_texture_non_power_of_two(AValue: TSDLExtensionOption);
      procedure SetGL_ARB_texture_rectangle(AValue: TSDLExtensionOption);
      procedure SetGL_ARB_transpose_matrix(AValue: TSDLExtensionOption);
      procedure SetGL_ARB_vertex_blend(AValue: TSDLExtensionOption);
      procedure SetGL_ARB_vertex_buffer_object(AValue: TSDLExtensionOption);
      procedure SetGL_ARB_vertex_program(AValue: TSDLExtensionOption);
      procedure SetGL_ARB_vertex_shader(AValue: TSDLExtensionOption);
      procedure SetGL_ARB_window_pos(AValue: TSDLExtensionOption);
    protected
      function DefaultOption: String; override;
    public
      procedure Load; override;
    published
      property GL_ARB_color_buffer_float: TSDLExtensionOption read FGL_ARB_color_buffer_float write SetGL_ARB_color_buffer_float;
      property GL_ARB_depth_texture: TSDLExtensionOption read FGL_ARB_depth_texture write SetGL_ARB_depth_texture;
      property GL_ARB_draw_buffers: TSDLExtensionOption read FGL_ARB_draw_buffers write SetGL_ARB_draw_buffers;
      property GL_ARB_fragment_program: TSDLExtensionOption read FGL_ARB_fragment_program write SetGL_ARB_fragment_program;
      property GL_ARB_fragment_shader: TSDLExtensionOption read FGL_ARB_fragment_shader write SetGL_ARB_fragment_shader;
      property GL_ARB_half_float_pixel: TSDLExtensionOption read FGL_ARB_half_float_pixel write SetGL_ARB_half_float_pixel;
      property GL_ARB_imaging: TSDLExtensionOption read FGL_ARB_imaging write SetGL_ARB_imaging;
      property GL_ARB_matrix_palette: TSDLExtensionOption read FGL_ARB_matrix_palette write SetGL_ARB_matrix_palette;
      property GL_ARB_multisample: TSDLExtensionOption read FGL_ARB_multisample write SetGL_ARB_multisample;
      property GL_ARB_multitexture: TSDLExtensionOption read FGL_ARB_multitexture write SetGL_ARB_multitexture;
      property GL_ARB_occlusion_query: TSDLExtensionOption read FGL_ARB_occlusion_query write SetGL_ARB_occlusion_query;
      property GL_ARB_pixel_buffer_object: TSDLExtensionOption read FGL_ARB_pixel_buffer_object write SetGL_ARB_pixel_buffer_object;
      property GL_ARB_point_parameters: TSDLExtensionOption read FGL_ARB_point_parameters write SetGL_ARB_point_parameters;
      property GL_ARB_point_sprite: TSDLExtensionOption read FGL_ARB_point_sprite write SetGL_ARB_point_sprite;      property GL_ARB_fragment_program_shadow: TSDLExtensionOption read FGL_ARB_fragment_program_shadow write SetGL_ARB_fragment_program_shadow;
      property GL_ARB_shader_objects: TSDLExtensionOption read FGL_ARB_shader_objects write SetGL_ARB_shader_objects;
      property GL_ARB_shading_language_100: TSDLExtensionOption read FGL_ARB_shading_language_100 write SetGL_ARB_shading_language_100;
      property GL_ARB_shadow: TSDLExtensionOption read FGL_ARB_shadow write SetGL_ARB_shadow;
      property GL_ARB_shadow_ambient: TSDLExtensionOption read FGL_ARB_shadow_ambient write SetGL_ARB_shadow_ambient;
      property GL_ARB_texture_border_clamp: TSDLExtensionOption read FGL_ARB_texture_border_clamp write SetGL_ARB_texture_border_clamp;
      property GL_ARB_texture_compression: TSDLExtensionOption read FGL_ARB_texture_compression write SetGL_ARB_texture_compression;
      property GL_ARB_texture_cube_map: TSDLExtensionOption read FGL_ARB_texture_cube_map write SetGL_ARB_texture_cube_map;
      property GL_ARB_texture_env_add: TSDLExtensionOption read FGL_ARB_texture_env_add write SetGL_ARB_texture_env_add;
      property GL_ARB_texture_env_combine: TSDLExtensionOption read FGL_ARB_texture_env_combine write SetGL_ARB_texture_env_combine;
      property GL_ARB_texture_env_crossbar: TSDLExtensionOption read FGL_ARB_texture_env_crossbar write SetGL_ARB_texture_env_crossbar;
      property GL_ARB_texture_env_dot3: TSDLExtensionOption read FGL_ARB_texture_env_dot3 write SetGL_ARB_texture_env_dot3;
      property GL_ARB_texture_float: TSDLExtensionOption read FGL_ARB_texture_float write SetGL_ARB_texture_float;
      property GL_ARB_texture_mirrored_repeat: TSDLExtensionOption read FGL_ARB_texture_mirrored_repeat write SetGL_ARB_texture_mirrored_repeat;
      property GL_ARB_texture_non_power_of_two: TSDLExtensionOption read FGL_ARB_texture_non_power_of_two write SetGL_ARB_texture_non_power_of_two;
      property GL_ARB_texture_rectangle: TSDLExtensionOption read FGL_ARB_texture_rectangle write SetGL_ARB_texture_rectangle;
      property GL_ARB_transpose_matrix: TSDLExtensionOption read FGL_ARB_transpose_matrix write SetGL_ARB_transpose_matrix;
      property GL_ARB_vertex_blend: TSDLExtensionOption read FGL_ARB_vertex_blend write SetGL_ARB_vertex_blend;
      property GL_ARB_vertex_buffer_object: TSDLExtensionOption read FGL_ARB_vertex_buffer_object write SetGL_ARB_vertex_buffer_object;
      property GL_ARB_vertex_program: TSDLExtensionOption read FGL_ARB_vertex_program write SetGL_ARB_vertex_program;
      property GL_ARB_vertex_shader: TSDLExtensionOption read FGL_ARB_vertex_shader write SetGL_ARB_vertex_shader;
      property GL_ARB_window_pos: TSDLExtensionOption read FGL_ARB_window_pos write SetGL_ARB_window_pos;
    end;

    { TSDLOpenGLExtensionsHP }

    TSDLOpenGLExtensionsHP = class(TSDLOpenGLExtension)
    private
      FGL_HP_occlusion_test: TSDLExtensionOption;
      procedure SetGL_HP_occlusion_test(AValue: TSDLExtensionOption);
    protected
      function DefaultOption: String; override;
    public
      procedure Load; override;
    published
      property GL_HP_occlusion_test: TSDLExtensionOption read FGL_HP_occlusion_test write SetGL_HP_occlusion_test;
    end;

    { TSDLOpenGLExtensionsNvidia }

    TSDLOpenGLExtensionsNvidia = class(TSDLOpenGLExtension)
    private
      FGL_NV_blend_square: TSDLExtensionOption;
      FGL_NV_copy_depth_to_color: TSDLExtensionOption;
      FGL_NV_depth_clamp: TSDLExtensionOption;
      FGL_NV_element_array: TSDLExtensionOption;
      FGL_NV_evaluators: TSDLExtensionOption;
      FGL_NV_fence: TSDLExtensionOption;
      FGL_NV_float_buffer: TSDLExtensionOption;
      FGL_NV_fog_distance: TSDLExtensionOption;
      FGL_NV_fragment_program: TSDLExtensionOption;
      FGL_NV_fragment_program2: TSDLExtensionOption;
      FGL_NV_fragment_program_option: TSDLExtensionOption;
      FGL_NV_half_float: TSDLExtensionOption;
      FGL_NV_light_max_exponent: TSDLExtensionOption;
      FGL_NV_multisample_filter_hint: TSDLExtensionOption;
      FGL_NV_occlusion_query: TSDLExtensionOption;
      FGL_NV_packed_depth_stencil: TSDLExtensionOption;
      FGL_NV_pixel_data_range: TSDLExtensionOption;
      FGL_NV_point_sprite: TSDLExtensionOption;
      FGL_NV_primitive_restart: TSDLExtensionOption;
      FGL_NV_register_combiners: TSDLExtensionOption;
      FGL_NV_register_combiners2: TSDLExtensionOption;
      FGL_NV_texgen_emboss: TSDLExtensionOption;
      FGL_NV_texgen_reflection: TSDLExtensionOption;
      FGL_NV_texture_compression_vtc: TSDLExtensionOption;
      FGL_NV_texture_env_combine4: TSDLExtensionOption;
      FGL_NV_texture_expand_normal: TSDLExtensionOption;
      FGL_NV_texture_rectangle: TSDLExtensionOption;
      FGL_NV_texture_shader: TSDLExtensionOption;
      FGL_NV_texture_shader2: TSDLExtensionOption;
      FGL_NV_texture_shader3: TSDLExtensionOption;
      FGL_NV_vertex_array_range: TSDLExtensionOption;
      FGL_NV_vertex_array_range2: TSDLExtensionOption;
      FGL_NV_vertex_program: TSDLExtensionOption;
      FGL_NV_vertex_program1_1: TSDLExtensionOption;
      FGL_NV_vertex_program2: TSDLExtensionOption;
      FGL_NV_vertex_program2_option: TSDLExtensionOption;
      FGL_NV_vertex_program3: TSDLExtensionOption;
      procedure SetGL_NV_blend_square(AValue: TSDLExtensionOption);
      procedure SetGL_NV_copy_depth_to_color(AValue: TSDLExtensionOption);
      procedure SetGL_NV_depth_clamp(AValue: TSDLExtensionOption);
      procedure SetGL_NV_element_array(AValue: TSDLExtensionOption);
      procedure SetGL_NV_evaluators(AValue: TSDLExtensionOption);
      procedure SetGL_NV_fence(AValue: TSDLExtensionOption);
      procedure SetGL_NV_float_buffer(AValue: TSDLExtensionOption);
      procedure SetGL_NV_fog_distance(AValue: TSDLExtensionOption);
      procedure SetGL_NV_fragment_program(AValue: TSDLExtensionOption);
      procedure SetGL_NV_fragment_program2(AValue: TSDLExtensionOption);
      procedure SetGL_NV_fragment_program_option(AValue: TSDLExtensionOption);
      procedure SetGL_NV_half_float(AValue: TSDLExtensionOption);
      procedure SetGL_NV_light_max_exponent(AValue: TSDLExtensionOption);
      procedure SetGL_NV_multisample_filter_hint(AValue: TSDLExtensionOption);
      procedure SetGL_NV_occlusion_query(AValue: TSDLExtensionOption);
      procedure SetGL_NV_packed_depth_stencil(AValue: TSDLExtensionOption);
      procedure SetGL_NV_pixel_data_range(AValue: TSDLExtensionOption);
      procedure SetGL_NV_point_sprite(AValue: TSDLExtensionOption);
      procedure SetGL_NV_primitive_restart(AValue: TSDLExtensionOption);
      procedure SetGL_NV_register_combiners(AValue: TSDLExtensionOption);
      procedure SetGL_NV_register_combiners2(AValue: TSDLExtensionOption);
      procedure SetGL_NV_texgen_emboss(AValue: TSDLExtensionOption);
      procedure SetGL_NV_texgen_reflection(AValue: TSDLExtensionOption);
      procedure SetGL_NV_texture_compression_vtc(AValue: TSDLExtensionOption);
      procedure SetGL_NV_texture_env_combine4(AValue: TSDLExtensionOption);
      procedure SetGL_NV_texture_expand_normal(AValue: TSDLExtensionOption);
      procedure SetGL_NV_texture_rectangle(AValue: TSDLExtensionOption);
      procedure SetGL_NV_texture_shader(AValue: TSDLExtensionOption);
      procedure SetGL_NV_texture_shader2(AValue: TSDLExtensionOption);
      procedure SetGL_NV_texture_shader3(AValue: TSDLExtensionOption);
      procedure SetGL_NV_vertex_array_range(AValue: TSDLExtensionOption);
      procedure SetGL_NV_vertex_array_range2(AValue: TSDLExtensionOption);
      procedure SetGL_NV_vertex_program(AValue: TSDLExtensionOption);
      procedure SetGL_NV_vertex_program1_1(AValue: TSDLExtensionOption);
      procedure SetGL_NV_vertex_program2(AValue: TSDLExtensionOption);
      procedure SetGL_NV_vertex_program2_option(AValue: TSDLExtensionOption);
      procedure SetGL_NV_vertex_program3(AValue: TSDLExtensionOption);
    protected
      function DefaultOption: String; override;
    public
      procedure Load; override;
    published
      property GL_NV_blend_square: TSDLExtensionOption read FGL_NV_blend_square write SetGL_NV_blend_square;
      property GL_NV_copy_depth_to_color: TSDLExtensionOption read FGL_NV_copy_depth_to_color write SetGL_NV_copy_depth_to_color;
      property GL_NV_depth_clamp: TSDLExtensionOption read FGL_NV_depth_clamp write SetGL_NV_depth_clamp;
      property GL_NV_element_array: TSDLExtensionOption read FGL_NV_element_array write SetGL_NV_element_array;
      property GL_NV_evaluators: TSDLExtensionOption read FGL_NV_evaluators write SetGL_NV_evaluators;
      property GL_NV_fence: TSDLExtensionOption read FGL_NV_fence write SetGL_NV_fence;
      property GL_NV_float_buffer: TSDLExtensionOption read FGL_NV_float_buffer write SetGL_NV_float_buffer;
      property GL_NV_fog_distance: TSDLExtensionOption read FGL_NV_fog_distance write SetGL_NV_fog_distance;
      property GL_NV_fragment_program: TSDLExtensionOption read FGL_NV_fragment_program write SetGL_NV_fragment_program;
      property GL_NV_fragment_program2: TSDLExtensionOption read FGL_NV_fragment_program2 write SetGL_NV_fragment_program2;
      property GL_NV_fragment_program_option: TSDLExtensionOption read FGL_NV_fragment_program_option write SetGL_NV_fragment_program_option;
      property GL_NV_half_float: TSDLExtensionOption read FGL_NV_half_float write SetGL_NV_half_float;
      property GL_NV_light_max_exponent: TSDLExtensionOption read FGL_NV_light_max_exponent write SetGL_NV_light_max_exponent;
      property GL_NV_multisample_filter_hint: TSDLExtensionOption read FGL_NV_multisample_filter_hint write SetGL_NV_multisample_filter_hint;
      property GL_NV_occlusion_query: TSDLExtensionOption read FGL_NV_occlusion_query write SetGL_NV_occlusion_query;
      property GL_NV_packed_depth_stencil: TSDLExtensionOption read FGL_NV_packed_depth_stencil write SetGL_NV_packed_depth_stencil;
      property GL_NV_pixel_data_range: TSDLExtensionOption read FGL_NV_pixel_data_range write SetGL_NV_pixel_data_range;
      property GL_NV_point_sprite: TSDLExtensionOption read FGL_NV_point_sprite write SetGL_NV_point_sprite;
      property GL_NV_primitive_restart: TSDLExtensionOption read FGL_NV_primitive_restart write SetGL_NV_primitive_restart;
      property GL_NV_register_combiners: TSDLExtensionOption read FGL_NV_register_combiners write SetGL_NV_register_combiners;
      property GL_NV_register_combiners2: TSDLExtensionOption read FGL_NV_register_combiners2 write SetGL_NV_register_combiners2;
      property GL_NV_texgen_emboss: TSDLExtensionOption read FGL_NV_texgen_emboss write SetGL_NV_texgen_emboss;
      property GL_NV_texgen_reflection: TSDLExtensionOption read FGL_NV_texgen_reflection write SetGL_NV_texgen_reflection;
      property GL_NV_texture_compression_vtc: TSDLExtensionOption read FGL_NV_texture_compression_vtc write SetGL_NV_texture_compression_vtc;
      property GL_NV_texture_env_combine4: TSDLExtensionOption read FGL_NV_texture_env_combine4 write SetGL_NV_texture_env_combine4;
      property GL_NV_texture_expand_normal: TSDLExtensionOption read FGL_NV_texture_expand_normal write SetGL_NV_texture_expand_normal;
      property GL_NV_texture_rectangle: TSDLExtensionOption read FGL_NV_texture_rectangle write SetGL_NV_texture_rectangle;
      property GL_NV_texture_shader: TSDLExtensionOption read FGL_NV_texture_shader write SetGL_NV_texture_shader;
      property GL_NV_texture_shader2: TSDLExtensionOption read FGL_NV_texture_shader2 write SetGL_NV_texture_shader2;
      property GL_NV_texture_shader3: TSDLExtensionOption read FGL_NV_texture_shader3 write SetGL_NV_texture_shader3;
      property GL_NV_vertex_array_range: TSDLExtensionOption read FGL_NV_vertex_array_range write SetGL_NV_vertex_array_range;
      property GL_NV_vertex_array_range2: TSDLExtensionOption read FGL_NV_vertex_array_range2 write SetGL_NV_vertex_array_range2;
      property GL_NV_vertex_program: TSDLExtensionOption read FGL_NV_vertex_program write SetGL_NV_vertex_program;
      property GL_NV_vertex_program1_1: TSDLExtensionOption read FGL_NV_vertex_program1_1 write SetGL_NV_vertex_program1_1;
      property GL_NV_vertex_program2: TSDLExtensionOption read FGL_NV_vertex_program2 write SetGL_NV_vertex_program2;
      property GL_NV_vertex_program2_option: TSDLExtensionOption read FGL_NV_vertex_program2_option write SetGL_NV_vertex_program2_option;
      property GL_NV_vertex_program3: TSDLExtensionOption read FGL_NV_vertex_program3 write SetGL_NV_vertex_program3;
    end;

    { TSDLOpenGLExtensionsATI }

    TSDLOpenGLExtensionsATI = class(TSDLOpenGLExtension)
    private
      FGL_ATI_draw_buffers: TSDLExtensionOption;
      FGL_ATI_element_array: TSDLExtensionOption;
      FGL_ATI_envmap_bumpmap: TSDLExtensionOption;
      FGL_ATI_fragment_shader: TSDLExtensionOption;
      FGL_ATI_map_object_buffer: TSDLExtensionOption;
      FGL_ATI_pn_triangles: TSDLExtensionOption;
      FGL_ATI_separate_stencil: TSDLExtensionOption;
      FGL_ATI_texture_env_combine3: TSDLExtensionOption;
      FGL_ATI_texture_float: TSDLExtensionOption;
      FGL_ATI_texture_mirror_once: TSDLExtensionOption;
      FGL_ATI_text_fragment_shader: TSDLExtensionOption;
      FGL_ATI_vertex_array_object: TSDLExtensionOption;
      FGL_ATI_vertex_attrib_array_object: TSDLExtensionOption;
      FGL_ATI_vertex_streams: TSDLExtensionOption;
      procedure SetGL_ATI_draw_buffers(AValue: TSDLExtensionOption);
      procedure SetGL_ATI_element_array(AValue: TSDLExtensionOption);
      procedure SetGL_ATI_envmap_bumpmap(AValue: TSDLExtensionOption);
      procedure SetGL_ATI_fragment_shader(AValue: TSDLExtensionOption);
      procedure SetGL_ATI_map_object_buffer(AValue: TSDLExtensionOption);
      procedure SetGL_ATI_pn_triangles(AValue: TSDLExtensionOption);
      procedure SetGL_ATI_separate_stencil(AValue: TSDLExtensionOption);
      procedure SetGL_ATI_texture_env_combine3(AValue: TSDLExtensionOption);
      procedure SetGL_ATI_texture_float(AValue: TSDLExtensionOption);
      procedure SetGL_ATI_texture_mirror_once(AValue: TSDLExtensionOption);
      procedure SetGL_ATI_text_fragment_shader(AValue: TSDLExtensionOption);
      procedure SetGL_ATI_vertex_array_object(AValue: TSDLExtensionOption);
      procedure SetGL_ATI_vertex_attrib_array_object(AValue: TSDLExtensionOption);
      procedure SetGL_ATI_vertex_streams(AValue: TSDLExtensionOption);
    protected
      function DefaultOption: String; override;
    public
      procedure Load; override;
    published
      property GL_ATI_draw_buffers: TSDLExtensionOption read FGL_ATI_draw_buffers write SetGL_ATI_draw_buffers;
      property GL_ATI_element_array: TSDLExtensionOption read FGL_ATI_element_array write SetGL_ATI_element_array;
      property GL_ATI_envmap_bumpmap: TSDLExtensionOption read FGL_ATI_envmap_bumpmap write SetGL_ATI_envmap_bumpmap;
      property GL_ATI_fragment_shader: TSDLExtensionOption read FGL_ATI_fragment_shader write SetGL_ATI_fragment_shader;
      property GL_ATI_map_object_buffer: TSDLExtensionOption read FGL_ATI_map_object_buffer write SetGL_ATI_map_object_buffer;
      property GL_ATI_pn_triangles: TSDLExtensionOption read FGL_ATI_pn_triangles write SetGL_ATI_pn_triangles;
      property GL_ATI_separate_stencil: TSDLExtensionOption read FGL_ATI_separate_stencil write SetGL_ATI_separate_stencil;
      property GL_ATI_texture_env_combine3: TSDLExtensionOption read FGL_ATI_texture_env_combine3 write SetGL_ATI_texture_env_combine3;
      property GL_ATI_texture_float: TSDLExtensionOption read FGL_ATI_texture_float write SetGL_ATI_texture_float;
      property GL_ATI_texture_mirror_once: TSDLExtensionOption read FGL_ATI_texture_mirror_once write SetGL_ATI_texture_mirror_once;
      property GL_ATI_text_fragment_shader: TSDLExtensionOption read FGL_ATI_text_fragment_shader write SetGL_ATI_text_fragment_shader;
      property GL_ATI_vertex_array_object: TSDLExtensionOption read FGL_ATI_vertex_array_object write SetGL_ATI_vertex_array_object;
      property GL_ATI_vertex_attrib_array_object: TSDLExtensionOption read FGL_ATI_vertex_attrib_array_object write SetGL_ATI_vertex_attrib_array_object;
      property GL_ATI_vertex_streams: TSDLExtensionOption read FGL_ATI_vertex_streams write SetGL_ATI_vertex_streams;
    end;

    { TSDLOpenGLExtensionsIBM }

    TSDLOpenGLExtensionsIBM = class(TSDLOpenGLExtension)
    private
      FGL_IBM_cull_vertex: TSDLExtensionOption;
      FGL_IBM_multimode_draw_arrays: TSDLExtensionOption;
      FGL_IBM_raster_pos_clip: TSDLExtensionOption;
      FGL_IBM_texture_mirrored_repeat: TSDLExtensionOption;
      FGL_IBM_vertex_array_lists: TSDLExtensionOption;
      procedure SetGL_IBM_cull_vertex(AValue: TSDLExtensionOption);
      procedure SetGL_IBM_multimode_draw_arrays(AValue: TSDLExtensionOption);
      procedure SetGL_IBM_raster_pos_clip(AValue: TSDLExtensionOption);
      procedure SetGL_IBM_texture_mirrored_repeat(AValue: TSDLExtensionOption);
      procedure SetGL_IBM_vertex_array_lists(AValue: TSDLExtensionOption);
    protected
      function DefaultOption: String; override;
    public
      procedure Load; override;
    published
      property GL_IBM_cull_vertex: TSDLExtensionOption read FGL_IBM_cull_vertex write SetGL_IBM_cull_vertex;
      property GL_IBM_multimode_draw_arrays: TSDLExtensionOption read FGL_IBM_multimode_draw_arrays write SetGL_IBM_multimode_draw_arrays;
      property GL_IBM_raster_pos_clip: TSDLExtensionOption read FGL_IBM_raster_pos_clip write SetGL_IBM_raster_pos_clip;
      property GL_IBM_texture_mirrored_repeat: TSDLExtensionOption read FGL_IBM_texture_mirrored_repeat write SetGL_IBM_texture_mirrored_repeat;
      property GL_IBM_vertex_array_lists: TSDLExtensionOption read FGL_IBM_vertex_array_lists write SetGL_IBM_vertex_array_lists;
    end;

    { TSDLOpenGLExtensionsMesa }

    TSDLOpenGLExtensionsMesa = class(TSDLOpenGLExtension)
    private
      FGL_MESA_pack_invert: TSDLExtensionOption;
      FGL_MESA_resize_buffers: TSDLExtensionOption;
      FGL_MESA_window_pos: TSDLExtensionOption;
      FGL_MESA_ycbcr_texture: TSDLExtensionOption;
      procedure SetGL_MESA_pack_invert(AValue: TSDLExtensionOption);
      procedure SetGL_MESA_resize_buffers(AValue: TSDLExtensionOption);
      procedure SetGL_MESA_window_pos(AValue: TSDLExtensionOption);
      procedure SetGL_MESA_ycbcr_texture(AValue: TSDLExtensionOption);
    protected
      function DefaultOption: String; override;
    public
      procedure Load; override;
    published
      property GL_MESA_pack_invert: TSDLExtensionOption read FGL_MESA_pack_invert write SetGL_MESA_pack_invert;
      property GL_MESA_resize_buffers: TSDLExtensionOption read FGL_MESA_resize_buffers write SetGL_MESA_resize_buffers;
      property GL_MESA_window_pos: TSDLExtensionOption read FGL_MESA_window_pos write SetGL_MESA_window_pos;
      property GL_MESA_ycbcr_texture: TSDLExtensionOption read FGL_MESA_ycbcr_texture write SetGL_MESA_ycbcr_texture;
    end;

    { TSDLOpenGLExtensions3DFx }

    TSDLOpenGLExtensions3DFx = class(TSDLOpenGLExtension)
    private
      FGL_3DFX_texture_compression_FXT1: TSDLExtensionOption;
      procedure SetGL_3DFX_texture_compression_FXT1(AValue: TSDLExtensionOption);
    protected
      function DefaultOption: String; override;
    public
      procedure Load; override;
    published
      property GL_3DFX_texture_compression_FXT1: TSDLExtensionOption read FGL_3DFX_texture_compression_FXT1 write SetGL_3DFX_texture_compression_FXT1;
    end;

    { TSDLOpenGLExtensionsOML }

    TSDLOpenGLExtensionsOML = class(TSDLOpenGLExtension)
    private
      FGL_OML_interlace: TSDLExtensionOption;
      FGL_OML_resample: TSDLExtensionOption;
      FGL_OML_subsample: TSDLExtensionOption;
      procedure SetGL_OML_interlace(AValue: TSDLExtensionOption);
      procedure SetGL_OML_resample(AValue: TSDLExtensionOption);
      procedure SetGL_OML_subsample(AValue: TSDLExtensionOption);
    protected
      function DefaultOption: String; override;
    public
      procedure Load; override;
    published
      property GL_OML_interlace: TSDLExtensionOption read FGL_OML_interlace write SetGL_OML_interlace;
      property GL_OML_resample: TSDLExtensionOption read FGL_OML_resample write SetGL_OML_resample;
      property GL_OML_subsample: TSDLExtensionOption read FGL_OML_subsample write SetGL_OML_subsample;
    end;

    { TSDLOpenGLExtensionsSGI }

    TSDLOpenGLExtensionsSGI = class(TSDLOpenGLExtension)
    private
      FGL_SGI_color_matrix: TSDLExtensionOption;
      FGL_SGI_color_table: TSDLExtensionOption;
      FGL_SGI_texture_color_table: TSDLExtensionOption;
      procedure SetGL_SGI_color_matrix(AValue: TSDLExtensionOption);
      procedure SetGL_SGI_color_table(AValue: TSDLExtensionOption);
      procedure SetGL_SGI_texture_color_table(AValue: TSDLExtensionOption);
    protected
      function DefaultOption: String; override;
    public
      procedure Load; override;
    published
      property GL_SGI_color_matrix: TSDLExtensionOption read FGL_SGI_color_matrix write SetGL_SGI_color_matrix;
      property GL_SGI_color_table: TSDLExtensionOption read FGL_SGI_color_table write SetGL_SGI_color_table;
      property GL_SGI_texture_color_table: TSDLExtensionOption read FGL_SGI_texture_color_table write SetGL_SGI_texture_color_table;
    end;

    { TSDLOpenGLExtensionsSun }

    TSDLOpenGLExtensionsSun = class(TSDLOpenGLExtension)
    private
      FGL_SUN_vertex: TSDLExtensionOption;
      procedure SetGL_SUN_vertex(AValue: TSDLExtensionOption);
    protected
      function DefaultOption: String; override;
    public
      procedure Load; override;
    published
      property GL_SUN_vertex: TSDLExtensionOption read FGL_SUN_vertex write SetGL_SUN_vertex;
    end;

    { TSDLOpenGLExtensionsSGIS }

    TSDLOpenGLExtensionsSGIS = class(TSDLOpenGLExtension)
    private
      FGL_SGIS_depth_texture: TSDLExtensionOption;
      FGL_SGIS_generate_mipmap: TSDLExtensionOption;
      FGL_SGIS_multisample: TSDLExtensionOption;
      FGL_SGIS_pixel_texture: TSDLExtensionOption;
      FGL_SGIS_texture_border_clamp: TSDLExtensionOption;
      FGL_SGIS_texture_color_mask: TSDLExtensionOption;
      FGL_SGIS_texture_edge_clamp: TSDLExtensionOption;
      FGL_SGIS_texture_lod: TSDLExtensionOption;
      procedure SetGL_SGIS_depth_texture(AValue: TSDLExtensionOption);
      procedure SetGL_SGIS_generate_mipmap(AValue: TSDLExtensionOption);
      procedure SetGL_SGIS_multisample(AValue: TSDLExtensionOption);
      procedure SetGL_SGIS_pixel_texture(AValue: TSDLExtensionOption);
      procedure SetGL_SGIS_texture_border_clamp(AValue: TSDLExtensionOption);
      procedure SetGL_SGIS_texture_color_mask(AValue: TSDLExtensionOption);
      procedure SetGL_SGIS_texture_edge_clamp(AValue: TSDLExtensionOption);
      procedure SetGL_SGIS_texture_lod(AValue: TSDLExtensionOption);
    protected
      function DefaultOption: String; override;
    public
      procedure Load; override;
    published
      property GL_SGIS_depth_texture: TSDLExtensionOption read FGL_SGIS_depth_texture write SetGL_SGIS_depth_texture;
      property GL_SGIS_generate_mipmap: TSDLExtensionOption read FGL_SGIS_generate_mipmap write SetGL_SGIS_generate_mipmap;
      property GL_SGIS_multisample: TSDLExtensionOption read FGL_SGIS_multisample write SetGL_SGIS_multisample;
      property GL_SGIS_pixel_texture: TSDLExtensionOption read FGL_SGIS_pixel_texture write SetGL_SGIS_pixel_texture;
      property GL_SGIS_texture_border_clamp: TSDLExtensionOption read FGL_SGIS_texture_border_clamp write SetGL_SGIS_texture_border_clamp;
      property GL_SGIS_texture_color_mask: TSDLExtensionOption read FGL_SGIS_texture_color_mask write SetGL_SGIS_texture_color_mask;
      property GL_SGIS_texture_edge_clamp: TSDLExtensionOption read FGL_SGIS_texture_edge_clamp write SetGL_SGIS_texture_edge_clamp;
      property GL_SGIS_texture_lod: TSDLExtensionOption read FGL_SGIS_texture_lod write SetGL_SGIS_texture_lod;
    end;

    { TSDLOpenGLExtensionsSGISX }

    TSDLOpenGLExtensionsSGISX = class(TSDLOpenGLExtension)
    private
      FGL_SGIX_fog_offset: TSDLExtensionOption;
      FGL_SGIX_interlace: TSDLExtensionOption;
      FGL_SGIX_shadow_ambient: TSDLExtensionOption;
      procedure SetGL_SGIX_fog_offset(AValue: TSDLExtensionOption);
      procedure SetGL_SGIX_interlace(AValue: TSDLExtensionOption);
      procedure SetGL_SGIX_shadow_ambient(AValue: TSDLExtensionOption);
    protected
      function DefaultOption: String; override;
    public
      procedure Load; override;
    published
      property GL_SGIX_fog_offset: TSDLExtensionOption read FGL_SGIX_fog_offset write SetGL_SGIX_fog_offset;
      property GL_SGIX_interlace: TSDLExtensionOption read FGL_SGIX_interlace write SetGL_SGIX_interlace;
      property GL_SGIX_shadow_ambient: TSDLExtensionOption read FGL_SGIX_shadow_ambient write SetGL_SGIX_shadow_ambient;
    end;

    { TSDLOpenGLExtensionsApple }

    TSDLOpenGLExtensionsApple = class(TSDLOpenGLExtension)
    private
      FGL_APPLE_client_storage: TSDLExtensionOption;
      FGL_APPLE_element_array: TSDLExtensionOption;
      FGL_APPLE_fence: TSDLExtensionOption;
      FGL_APPLE_vertex_array_object: TSDLExtensionOption;
      FGL_APPLE_vertex_array_range: TSDLExtensionOption;
      procedure SetGL_APPLE_client_storage(AValue: TSDLExtensionOption);
      procedure SetGL_APPLE_element_array(AValue: TSDLExtensionOption);
      procedure SetGL_APPLE_fence(AValue: TSDLExtensionOption);
      procedure SetGL_APPLE_vertex_array_object(AValue: TSDLExtensionOption);
      procedure SetGL_APPLE_vertex_array_range(AValue: TSDLExtensionOption);
    protected
      function DefaultOption: String; override;
    public
      procedure Load; override;
    published
      property GL_APPLE_client_storage: TSDLExtensionOption read FGL_APPLE_client_storage write SetGL_APPLE_client_storage;
      property GL_APPLE_element_array: TSDLExtensionOption read FGL_APPLE_element_array write SetGL_APPLE_element_array;
      property GL_APPLE_fence: TSDLExtensionOption read FGL_APPLE_fence write SetGL_APPLE_fence;
      property GL_APPLE_vertex_array_object: TSDLExtensionOption read FGL_APPLE_vertex_array_object write SetGL_APPLE_vertex_array_object;
      property GL_APPLE_vertex_array_range: TSDLExtensionOption read FGL_APPLE_vertex_array_range write SetGL_APPLE_vertex_array_range;
    end;

    { TSDLOpenGLExtensionsS3Tc }

    TSDLOpenGLExtensionsS3Tc = class(TSDLOpenGLExtension)
    private
      FGL_S3_s3tc: TSDLExtensionOption;
      procedure SetGL_S3_s3tc(AValue: TSDLExtensionOption);
    protected
      function DefaultOption: String; override;
    public
      procedure Load; override;
    published
      property GL_S3_s3tc: TSDLExtensionOption read FGL_S3_s3tc write SetGL_S3_s3tc;
    end;

  private
    FActive: Boolean;
    FApple: TSDLOpenGLExtensionsApple;
    FARB: TSDLOpenGLExtensionsARB;
    FATI: TSDLOpenGLExtensionsATI;
    FDFx: TSDLOpenGLExtensions3DFx;
    FExt: TSDLOpenGLExtensionsExt;
    FHP: TSDLOpenGLExtensionsHP;
    FIBM: TSDLOpenGLExtensionsIBM;
    FMesa: TSDLOpenGLExtensionsMesa;
    FNvidia: TSDLOpenGLExtensionsNvidia;
    FOML: TSDLOpenGLExtensionsOML;
    FOnNotLoaded: TSDLExtensionLoadEvent;
    FS3Tc: TSDLOpenGLExtensionsS3Tc;
    FSGI: TSDLOpenGLExtensionsSGI;
    FSGIS: TSDLOpenGLExtensionsSGIS;
    FSGISX: TSDLOpenGLExtensionsSGISX;
    FSun: TSDLOpenGLExtensionsSun;
    FVersions: TSDLOpenGLVersions;
    FWindows: TSDLOpenGLExtensionsWindows;
    procedure SetActive(AValue: Boolean);
    procedure SetApple(AValue: TSDLOpenGLExtensionsApple);
    procedure SetARB(AValue: TSDLOpenGLExtensionsARB);
    procedure SetATI(AValue: TSDLOpenGLExtensionsATI);
    procedure SetDFx(AValue: TSDLOpenGLExtensions3DFx);
    procedure SetExt(AValue: TSDLOpenGLExtensionsExt);
    procedure SetHP(AValue: TSDLOpenGLExtensionsHP);
    procedure SetIBM(AValue: TSDLOpenGLExtensionsIBM);
    procedure SetMesa(AValue: TSDLOpenGLExtensionsMesa);
    procedure SetNvidia(AValue: TSDLOpenGLExtensionsNvidia);
    procedure SetOML(AValue: TSDLOpenGLExtensionsOML);
    procedure SetOnNotLoaded(AValue: TSDLExtensionLoadEvent);
    procedure SetS3Tc(AValue: TSDLOpenGLExtensionsS3Tc);
    procedure SetSGI(AValue: TSDLOpenGLExtensionsSGI);
    procedure SetSGIS(AValue: TSDLOpenGLExtensionsSGIS);
    procedure SetSGISX(AValue: TSDLOpenGLExtensionsSGISX);
    procedure SetSun(AValue: TSDLOpenGLExtensionsSun);
    procedure SetVersions(AValue: TSDLOpenGLVersions);
    procedure SetWindows(AValue: TSDLOpenGLExtensionsWindows);
  protected
    procedure LoadExtensions;
    procedure DoReload; override;
    procedure FPOObservedChanged(ASender: TObject; Operation: TFPObservedOperation; Data: Pointer); override;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    procedure Init;

    property Active: Boolean read FActive write SetActive; (** Activate extensions. Loading will fire only when OpenGL context is created *)
    property Apple: TSDLOpenGLExtensionsApple read FApple write SetApple;
    property ARB: TSDLOpenGLExtensionsARB read FARB write SetARB;
    property ATI: TSDLOpenGLExtensionsATI read FATI write SetATI;
    property DFx: TSDLOpenGLExtensions3DFx read FDFx write SetDFx;
    property Ext: TSDLOpenGLExtensionsExt read FExt write SetExt;
    property HP: TSDLOpenGLExtensionsHP read FHP write SetHP;
    property IBM: TSDLOpenGLExtensionsIBM read FIBM write SetIBM;
    property Mesa: TSDLOpenGLExtensionsMesa read FMesa write SetMesa;
    property Nvidia: TSDLOpenGLExtensionsNvidia read FNvidia write SetNvidia;
    property OML: TSDLOpenGLExtensionsOML read FOML write SetOML;
    property S3Tc: TSDLOpenGLExtensionsS3Tc read FS3Tc write SetS3Tc;
    property SGI: TSDLOpenGLExtensionsSGI read FSGI write SetSGI;
    property SGIS: TSDLOpenGLExtensionsSGIS read FSGIS write SetSGIS;
    property SGISX: TSDLOpenGLExtensionsSGISX read FSGISX write SetSGISX;
    property Sun: TSDLOpenGLExtensionsSun read FSun write SetSun;
    property Versions: TSDLOpenGLVersions read FVersions write SetVersions;
    property Windows: TSDLOpenGLExtensionsWindows read FWindows write SetWindows;

    property OnNotLoaded: TSDLExtensionLoadEvent read FOnNotLoaded write SetOnNotLoaded; (** Fires whenever extension is not loaded *)
  end;

  TSDLOpenGLExtensions = class(TSDLCustomOpenGLExtensions)
  published
    property Active;
    property Apple;
    property ARB;
    property ATI;
    property DFx;
    property Ext;
    property HP;
    property IBM;
    property Mesa;
    property Nvidia;
    property OML;
    property Provider;
    property S3Tc;
    property SGI;
    property SGIS;
    property SGISX;
    property Sun;
    property Versions;
    property Windows;

    property OnNotLoaded;
  end;

implementation

uses
  typinfo, LMessages;

{ TSDLCustomOpenGLExtensions.TSDLOpenGLExtension }

function TSDLCustomOpenGLExtensions.TSDLOpenGLExtension.GetProvider: TSDLCustomLibraryProvider;
begin
  Result := FOwner.Provider;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtension.Validate(
  ExtName: String; Method: TSDLLoadMethod; Option: TSDLExtensionOption);
begin
  if (Option <> extIgnore) then
  begin
    if not Method() then
    begin
      if Assigned(FOwner.FOnNotLoaded) then
        FOwner.FOnNotLoaded(FOwner, ExtName, Option);
    end;
  end;
end;

constructor TSDLCustomOpenGLExtensions.TSDLOpenGLExtension.Create(
  Owner: TSDLCustomOpenGLExtensions);
var
  List: PPropList;
  i: Integer;
begin
  FOwner := Owner;

  for i := 0 to GetPropList(Self, List) - 1 do
  begin
    if List^[i]^.PropType^.Name = 'TSDLExtensionOption' then
      SetEnumProp(Self, List^[i], DefaultOption);
  end;
  Freemem(List);
end;

{ TSDLCustomOpenGLExtensions }

procedure TSDLCustomOpenGLExtensions.SetActive(AValue: Boolean);
begin
  if FActive=AValue then Exit;
  FActive:=AValue;

  LoadExtensions;
end;

procedure TSDLCustomOpenGLExtensions.SetApple(AValue: TSDLOpenGLExtensionsApple
  );
begin
  if FApple=AValue then Exit;
  FApple:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.SetARB(AValue: TSDLOpenGLExtensionsARB);
begin
  if FARB=AValue then Exit;
  FARB:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.SetATI(AValue: TSDLOpenGLExtensionsATI);
begin
  if FATI=AValue then Exit;
  FATI:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.SetDFx(AValue: TSDLOpenGLExtensions3DFx);
begin
  if FDFx=AValue then Exit;
  FDFx:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.SetExt(AValue: TSDLOpenGLExtensionsExt);
begin
  if FExt=AValue then Exit;
  FExt:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.SetHP(AValue: TSDLOpenGLExtensionsHP);
begin
  if FHP=AValue then Exit;
  FHP:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.SetIBM(AValue: TSDLOpenGLExtensionsIBM);
begin
  if FIBM=AValue then Exit;
  FIBM:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.SetMesa(AValue: TSDLOpenGLExtensionsMesa);
begin
  if FMesa=AValue then Exit;
  FMesa:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.SetNvidia(
  AValue: TSDLOpenGLExtensionsNvidia);
begin
  if FNvidia=AValue then Exit;
  FNvidia:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.SetOML(AValue: TSDLOpenGLExtensionsOML);
begin
  if FOML=AValue then Exit;
  FOML:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.SetOnNotLoaded(
  AValue: TSDLExtensionLoadEvent);
begin
  FOnNotLoaded:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.SetS3Tc(AValue: TSDLOpenGLExtensionsS3Tc);
begin
  if FS3Tc=AValue then Exit;
  FS3Tc:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.SetSGI(AValue: TSDLOpenGLExtensionsSGI);
begin
  if FSGI=AValue then Exit;
  FSGI:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.SetSGIS(AValue: TSDLOpenGLExtensionsSGIS);
begin
  if FSGIS=AValue then Exit;
  FSGIS:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.SetSGISX(AValue: TSDLOpenGLExtensionsSGISX
  );
begin
  if FSGISX=AValue then Exit;
  FSGISX:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.SetSun(AValue: TSDLOpenGLExtensionsSun);
begin
  if FSun=AValue then Exit;
  FSun:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.SetVersions(AValue: TSDLOpenGLVersions);
begin
  if FVersions=AValue then Exit;
  FVersions:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.SetWindows(
  AValue: TSDLOpenGLExtensionsWindows);
begin
  if FWindows=AValue then Exit;
  FWindows:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.LoadExtensions;
var
  List: PPropList;
  Obj: TObject;
  i: Integer;
begin
  if FActive and not (csDesigning in ComponentState) and Assigned(Provider) and
     Provider.Active and Assigned(Window) and Assigned(Window.Context) then
  begin
    for i := 0 to GetPropList(Self, List) - 1 do
    begin
      if List^[i]^.PropType^.Kind = tkClass then
      begin
        Obj := GetObjectProp(Self, List^[i]);
        if Obj is TSDLOpenGLExtension then
          TSDLOpenGLExtension(Obj).Load;
      end;
    end;
    Freemem(List);
  end;
end;

procedure TSDLCustomOpenGLExtensions.DoReload;
begin
  LoadExtensions
end;

procedure TSDLCustomOpenGLExtensions.FPOObservedChanged(ASender: TObject;
  Operation: TFPObservedOperation; Data: Pointer);
begin
  if (ASender = Window) and ({%H-}PtrUInt(Data) = PtrUInt(LM_CREATE)) then
    LoadExtensions
  else
    inherited FPOObservedChanged(ASender, Operation, Data);
end;

procedure TSDLCustomOpenGLExtensions.AfterConstruction;
begin
  FApple := TSDLOpenGLExtensionsApple.Create(Self);
  FARB := TSDLOpenGLExtensionsARB.Create(Self);
  FATI := TSDLOpenGLExtensionsATI.Create(Self);
  FDFx := TSDLOpenGLExtensions3DFx.Create(Self);
  FExt := TSDLOpenGLExtensionsExt.Create(Self);
  FHP := TSDLOpenGLExtensionsHP.Create(Self);
  FIBM := TSDLOpenGLExtensionsIBM.Create(Self);
  FMesa := TSDLOpenGLExtensionsMesa.Create(Self);
  FNvidia := TSDLOpenGLExtensionsNvidia.Create(Self);
  FOML := TSDLOpenGLExtensionsOML.Create(Self);
  FS3Tc := TSDLOpenGLExtensionsS3Tc.Create(Self);
  FSGI := TSDLOpenGLExtensionsSGI.Create(Self);
  FSGIS := TSDLOpenGLExtensionsSGIS.Create(Self);
  FSGISX := TSDLOpenGLExtensionsSGISX.Create(Self);
  FSun := TSDLOpenGLExtensionsSun.Create(Self);
  FVersions := TSDLOpenGLVersions.Create(Self);
  FWindows := TSDLOpenGLExtensionsWindows.Create(Self);

  inherited AfterConstruction;
end;

procedure TSDLCustomOpenGLExtensions.BeforeDestruction;
begin
  FApple.Free;
  FARB.Free;
  FATI.Free;
  FDFx.Free;
  FExt.Free;
  FHP.Free;
  FIBM.Free;
  FMesa.Free;
  FNvidia.Free;
  FOML.Free;
  FS3Tc.Free;
  FSGI.Free;
  FSGIS.Free;
  FSGISX.Free;
  FSun.Free;
  FVersions.Free;
  FWindows.Free;

  inherited BeforeDestruction;
end;

procedure TSDLCustomOpenGLExtensions.Init;
begin
  if Active then
    LoadExtensions;
end;

{ TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsS3Tc }

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsS3Tc.SetGL_S3_s3tc(
  AValue: TSDLExtensionOption);
begin
  if FGL_S3_s3tc=AValue then Exit;
  FGL_S3_s3tc:=AValue;
end;

function TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsS3Tc.DefaultOption: String;
begin
  Result := 'extIgnore'
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsS3Tc.Load;
begin
  //Validate('GL_S3_s3tc', @Load_GL_S3_s3tc, GL_S3_s3tc);
end;

{ TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsApple }

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsApple.SetGL_APPLE_client_storage
  (AValue: TSDLExtensionOption);
begin
  if FGL_APPLE_client_storage=AValue then Exit;
  FGL_APPLE_client_storage:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsApple.SetGL_APPLE_element_array
  (AValue: TSDLExtensionOption);
begin
  if FGL_APPLE_element_array=AValue then Exit;
  FGL_APPLE_element_array:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsApple.SetGL_APPLE_fence
  (AValue: TSDLExtensionOption);
begin
  if FGL_APPLE_fence=AValue then Exit;
  FGL_APPLE_fence:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsApple.SetGL_APPLE_vertex_array_object
  (AValue: TSDLExtensionOption);
begin
  if FGL_APPLE_vertex_array_object=AValue then Exit;
  FGL_APPLE_vertex_array_object:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsApple.SetGL_APPLE_vertex_array_range
  (AValue: TSDLExtensionOption);
begin
  if FGL_APPLE_vertex_array_range=AValue then Exit;
  FGL_APPLE_vertex_array_range:=AValue;
end;

function TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsApple.DefaultOption: String;
begin
  Result := 'extIgnore'
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsApple.Load;
begin
  //Validate('GL_APPLE_client_storage', @Load_GL_APPLE_client_storage, GL_APPLE_client_storage);
  //Validate('GL_APPLE_element_array', @Load_GL_APPLE_element_array, GL_APPLE_element_array);
  //Validate('GL_APPLE_fence', @Load_GL_APPLE_fence, GL_APPLE_fence);
  //Validate('GL_APPLE_vertex_array_object', @Load_GL_APPLE_vertex_array_object, GL_APPLE_vertex_array_object);
  //Validate('GL_APPLE_vertex_array_range', @Load_GL_APPLE_vertex_array_range, GL_APPLE_vertex_array_range);
end;

{ TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsSGISX }

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsSGISX.SetGL_SGIX_fog_offset
  (AValue: TSDLExtensionOption);
begin
  if FGL_SGIX_fog_offset=AValue then Exit;
  FGL_SGIX_fog_offset:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsSGISX.SetGL_SGIX_interlace
  (AValue: TSDLExtensionOption);
begin
  if FGL_SGIX_interlace=AValue then Exit;
  FGL_SGIX_interlace:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsSGISX.SetGL_SGIX_shadow_ambient
  (AValue: TSDLExtensionOption);
begin
  if FGL_SGIX_shadow_ambient=AValue then Exit;
  FGL_SGIX_shadow_ambient:=AValue;
end;

function TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsSGISX.DefaultOption: String;
begin
  Result := 'extIgnore'
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsSGISX.Load;
begin
  //Validate('GL_SGIX_fog_offset', @Load_GL_SGIX_fog_offset, GL_SGIX_fog_offset);
  //Validate('GL_SGIX_interlace', @Load_GL_SGIX_interlace, GL_SGIX_interlace);
  //Validate('GL_SGIX_shadow_ambient', @Load_GL_SGIX_shadow_ambient, GL_SGIX_shadow_ambient);
end;

{ TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsSGIS }

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsSGIS.SetGL_SGIS_depth_texture
  (AValue: TSDLExtensionOption);
begin
  if FGL_SGIS_depth_texture=AValue then Exit;
  FGL_SGIS_depth_texture:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsSGIS.SetGL_SGIS_generate_mipmap
  (AValue: TSDLExtensionOption);
begin
  if FGL_SGIS_generate_mipmap=AValue then Exit;
  FGL_SGIS_generate_mipmap:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsSGIS.SetGL_SGIS_multisample
  (AValue: TSDLExtensionOption);
begin
  if FGL_SGIS_multisample=AValue then Exit;
  FGL_SGIS_multisample:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsSGIS.SetGL_SGIS_pixel_texture
  (AValue: TSDLExtensionOption);
begin
  if FGL_SGIS_pixel_texture=AValue then Exit;
  FGL_SGIS_pixel_texture:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsSGIS.SetGL_SGIS_texture_border_clamp
  (AValue: TSDLExtensionOption);
begin
  if FGL_SGIS_texture_border_clamp=AValue then Exit;
  FGL_SGIS_texture_border_clamp:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsSGIS.SetGL_SGIS_texture_color_mask
  (AValue: TSDLExtensionOption);
begin
  if FGL_SGIS_texture_color_mask=AValue then Exit;
  FGL_SGIS_texture_color_mask:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsSGIS.SetGL_SGIS_texture_edge_clamp
  (AValue: TSDLExtensionOption);
begin
  if FGL_SGIS_texture_edge_clamp=AValue then Exit;
  FGL_SGIS_texture_edge_clamp:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsSGIS.SetGL_SGIS_texture_lod
  (AValue: TSDLExtensionOption);
begin
  if FGL_SGIS_texture_lod=AValue then Exit;
  FGL_SGIS_texture_lod:=AValue;
end;

function TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsSGIS.DefaultOption: String;
begin
  Result := 'extIgnore'
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsSGIS.Load;
begin
  //Validate('GL_SGIS_depth_texture', @Load_GL_SGIS_depth_texture, GL_SGIS_depth_texture);
  //Validate('GL_SGIS_generate_mipmap', @Load_GL_SGIS_generate_mipmap, GL_SGIS_generate_mipmap);
  //Validate('GL_SGIS_multisample', @Load_GL_SGIS_multisample, GL_SGIS_multisample);
  //Validate('GL_SGIS_pixel_texture', @Load_GL_SGIS_pixel_texture, GL_SGIS_pixel_texture);
  //Validate('GL_SGIS_texture_border_clamp', @Load_GL_SGIS_texture_border_clamp, GL_SGIS_texture_border_clamp);
  //Validate('GL_SGIS_texture_color_mask', @Load_GL_SGIS_texture_color_mask, GL_SGIS_texture_color_mask);
  //Validate('GL_SGIS_texture_edge_clamp', @Load_GL_SGIS_texture_edge_clamp, GL_SGIS_texture_edge_clamp);
  //Validate('GL_SGIS_texture_lod', @Load_GL_SGIS_texture_lod, GL_SGIS_texture_lod);
end;

{ TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsSun }

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsSun.SetGL_SUN_vertex(
  AValue: TSDLExtensionOption);
begin
  if FGL_SUN_vertex=AValue then Exit;
  FGL_SUN_vertex:=AValue;
end;

function TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsSun.DefaultOption: String;
begin
  Result := 'extIgnore'
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsSun.Load;
begin
  //Validate('GL_SUN_vertex', @Load_GL_SUN_vertex, GL_SUN_vertex);
end;

{ TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsSGI }

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsSGI.SetGL_SGI_color_matrix
  (AValue: TSDLExtensionOption);
begin
  if FGL_SGI_color_matrix=AValue then Exit;
  FGL_SGI_color_matrix:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsSGI.SetGL_SGI_color_table
  (AValue: TSDLExtensionOption);
begin
  if FGL_SGI_color_table=AValue then Exit;
  FGL_SGI_color_table:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsSGI.SetGL_SGI_texture_color_table
  (AValue: TSDLExtensionOption);
begin
  if FGL_SGI_texture_color_table=AValue then Exit;
  FGL_SGI_texture_color_table:=AValue;
end;

function TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsSGI.DefaultOption: String;
begin
  Result := 'extIgnore'
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsSGI.Load;
begin
  //Validate('GL_SGI_color_matrix', @Load_GL_SGI_color_matrix, GL_SGI_color_matrix);
  //Validate('GL_SGI_color_table', @Load_GL_SGI_color_table, GL_SGI_color_table);
  //Validate('GL_SGI_texture_color_table', @Load_GL_SGI_texture_color_table, GL_SGI_texture_color_table);
end;

{ TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsOML }

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsOML.SetGL_OML_interlace
  (AValue: TSDLExtensionOption);
begin
  if FGL_OML_interlace=AValue then Exit;
  FGL_OML_interlace:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsOML.SetGL_OML_resample(
  AValue: TSDLExtensionOption);
begin
  if FGL_OML_resample=AValue then Exit;
  FGL_OML_resample:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsOML.SetGL_OML_subsample
  (AValue: TSDLExtensionOption);
begin
  if FGL_OML_subsample=AValue then Exit;
  FGL_OML_subsample:=AValue;
end;

function TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsOML.DefaultOption: String;
begin
  Result := 'extIgnore'
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsOML.Load;
begin
  //Validate('GL_OML_interlace', @Load_GL_OML_interlace, GL_OML_interlace);
  //Validate('GL_OML_resample', @Load_GL_OML_resample, GL_OML_resample);
  //Validate('GL_OML_subsample', @Load_GL_OML_subsample, GL_OML_subsample);
end;

{ TSDLCustomOpenGLExtensions.TSDLOpenGLExtensions3DFx }

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensions3DFx.SetGL_3DFX_texture_compression_FXT1
  (AValue: TSDLExtensionOption);
begin
  if FGL_3DFX_texture_compression_FXT1=AValue then Exit;
  FGL_3DFX_texture_compression_FXT1:=AValue;
end;

function TSDLCustomOpenGLExtensions.TSDLOpenGLExtensions3DFx.DefaultOption: String;
begin
  Result := 'extIgnore'
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensions3DFx.Load;
begin
  //Validate('GL_3DFX_texture_compression_FXT1', @Load_GL_3DFX_texture_compression_FXT1, GL_3DFX_texture_compression_FXT1);
end;

{ TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsMesa }

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsMesa.SetGL_MESA_pack_invert
  (AValue: TSDLExtensionOption);
begin
  if FGL_MESA_pack_invert=AValue then Exit;
  FGL_MESA_pack_invert:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsMesa.SetGL_MESA_resize_buffers
  (AValue: TSDLExtensionOption);
begin
  if FGL_MESA_resize_buffers=AValue then Exit;
  FGL_MESA_resize_buffers:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsMesa.SetGL_MESA_window_pos
  (AValue: TSDLExtensionOption);
begin
  if FGL_MESA_window_pos=AValue then Exit;
  FGL_MESA_window_pos:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsMesa.SetGL_MESA_ycbcr_texture
  (AValue: TSDLExtensionOption);
begin
  if FGL_MESA_ycbcr_texture=AValue then Exit;
  FGL_MESA_ycbcr_texture:=AValue;
end;

function TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsMesa.DefaultOption: String;
begin
  Result := 'extIgnore'
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsMesa.Load;
begin
  //Validate('GL_MESA_pack_invert', @Load_GL_MESA_pack_invert, GL_MESA_pack_invert);
  //Validate('GL_MESA_resize_buffers', @Load_GL_MESA_resize_buffers, GL_MESA_resize_buffers);
  //Validate('GL_MESA_window_pos', @Load_GL_MESA_window_pos, GL_MESA_window_pos);
  //Validate('GL_MESA_ycbcr_texture', @Load_GL_MESA_ycbcr_texture, GL_MESA_ycbcr_texture);
end;

{ TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsIBM }

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsIBM.SetGL_IBM_cull_vertex
  (AValue: TSDLExtensionOption);
begin
  if FGL_IBM_cull_vertex=AValue then Exit;
  FGL_IBM_cull_vertex:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsIBM.SetGL_IBM_multimode_draw_arrays
  (AValue: TSDLExtensionOption);
begin
  if FGL_IBM_multimode_draw_arrays=AValue then Exit;
  FGL_IBM_multimode_draw_arrays:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsIBM.SetGL_IBM_raster_pos_clip
  (AValue: TSDLExtensionOption);
begin
  if FGL_IBM_raster_pos_clip=AValue then Exit;
  FGL_IBM_raster_pos_clip:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsIBM.SetGL_IBM_texture_mirrored_repeat
  (AValue: TSDLExtensionOption);
begin
  if FGL_IBM_texture_mirrored_repeat=AValue then Exit;
  FGL_IBM_texture_mirrored_repeat:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsIBM.SetGL_IBM_vertex_array_lists
  (AValue: TSDLExtensionOption);
begin
  if FGL_IBM_vertex_array_lists=AValue then Exit;
  FGL_IBM_vertex_array_lists:=AValue;
end;

function TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsIBM.DefaultOption: String;
begin
  Result := 'extIgnore'
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsIBM.Load;
begin
  //Validate('GL_IBM_cull_vertex', @Load_GL_IBM_cull_vertex, GL_IBM_cull_vertex);
  //Validate('GL_IBM_multimode_draw_arrays', @Load_GL_IBM_multimode_draw_arrays, GL_IBM_multimode_draw_arrays);
  //Validate('GL_IBM_raster_pos_clip', @Load_GL_IBM_raster_pos_clip, GL_IBM_raster_pos_clip);
  //Validate('GL_IBM_texture_mirrored_repeat', @Load_GL_IBM_texture_mirrored_repeat, GL_IBM_texture_mirrored_repeat);
  //Validate('GL_IBM_vertex_array_lists', @Load_GL_IBM_vertex_array_lists, GL_IBM_vertex_array_lists);
end;

{ TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsATI }

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsATI.SetGL_ATI_draw_buffers
  (AValue: TSDLExtensionOption);
begin
  if FGL_ATI_draw_buffers=AValue then Exit;
  FGL_ATI_draw_buffers:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsATI.SetGL_ATI_element_array
  (AValue: TSDLExtensionOption);
begin
  if FGL_ATI_element_array=AValue then Exit;
  FGL_ATI_element_array:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsATI.SetGL_ATI_envmap_bumpmap
  (AValue: TSDLExtensionOption);
begin
  if FGL_ATI_envmap_bumpmap=AValue then Exit;
  FGL_ATI_envmap_bumpmap:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsATI.SetGL_ATI_fragment_shader
  (AValue: TSDLExtensionOption);
begin
  if FGL_ATI_fragment_shader=AValue then Exit;
  FGL_ATI_fragment_shader:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsATI.SetGL_ATI_map_object_buffer
  (AValue: TSDLExtensionOption);
begin
  if FGL_ATI_map_object_buffer=AValue then Exit;
  FGL_ATI_map_object_buffer:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsATI.SetGL_ATI_pn_triangles
  (AValue: TSDLExtensionOption);
begin
  if FGL_ATI_pn_triangles=AValue then Exit;
  FGL_ATI_pn_triangles:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsATI.SetGL_ATI_separate_stencil
  (AValue: TSDLExtensionOption);
begin
  if FGL_ATI_separate_stencil=AValue then Exit;
  FGL_ATI_separate_stencil:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsATI.SetGL_ATI_texture_env_combine3
  (AValue: TSDLExtensionOption);
begin
  if FGL_ATI_texture_env_combine3=AValue then Exit;
  FGL_ATI_texture_env_combine3:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsATI.SetGL_ATI_texture_float
  (AValue: TSDLExtensionOption);
begin
  if FGL_ATI_texture_float=AValue then Exit;
  FGL_ATI_texture_float:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsATI.SetGL_ATI_texture_mirror_once
  (AValue: TSDLExtensionOption);
begin
  if FGL_ATI_texture_mirror_once=AValue then Exit;
  FGL_ATI_texture_mirror_once:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsATI.SetGL_ATI_text_fragment_shader
  (AValue: TSDLExtensionOption);
begin
  if FGL_ATI_text_fragment_shader=AValue then Exit;
  FGL_ATI_text_fragment_shader:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsATI.SetGL_ATI_vertex_array_object
  (AValue: TSDLExtensionOption);
begin
  if FGL_ATI_vertex_array_object=AValue then Exit;
  FGL_ATI_vertex_array_object:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsATI.SetGL_ATI_vertex_attrib_array_object
  (AValue: TSDLExtensionOption);
begin
  if FGL_ATI_vertex_attrib_array_object=AValue then Exit;
  FGL_ATI_vertex_attrib_array_object:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsATI.SetGL_ATI_vertex_streams
  (AValue: TSDLExtensionOption);
begin
  if FGL_ATI_vertex_streams=AValue then Exit;
  FGL_ATI_vertex_streams:=AValue;
end;

function TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsATI.DefaultOption: String;
begin
  Result := 'extIgnore'
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsATI.Load;
begin
  //Validate('GL_ATI_draw_buffers', @Load_GL_ATI_draw_buffers, GL_ATI_draw_buffers);
  //Validate('GL_ATI_element_array', @Load_GL_ATI_element_array, GL_ATI_element_array);
  //Validate('GL_ATI_envmap_bumpmap', @Load_GL_ATI_envmap_bumpmap, GL_ATI_envmap_bumpmap);
  //Validate('GL_ATI_fragment_shader', @Load_GL_ATI_fragment_shader, GL_ATI_fragment_shader);
  //Validate('GL_ATI_map_object_buffer', @Load_GL_ATI_map_object_buffer, GL_ATI_map_object_buffer);
  //Validate('GL_ATI_pn_triangles', @Load_GL_ATI_pn_triangles, GL_ATI_pn_triangles);
  //Validate('GL_ATI_separate_stencil', @Load_GL_ATI_separate_stencil, GL_ATI_separate_stencil);
  //Validate('GL_ATI_texture_env_combine3', @Load_GL_ATI_texture_env_combine3, GL_ATI_texture_env_combine3);
  //Validate('GL_ATI_texture_float', @Load_GL_ATI_texture_float, GL_ATI_texture_float);
  //Validate('GL_ATI_texture_mirror_once', @Load_GL_ATI_texture_mirror_once, GL_ATI_texture_mirror_once);
  //Validate('GL_ATI_text_fragment_shader', @Load_GL_ATI_text_fragment_shader, GL_ATI_text_fragment_shader);
  //Validate('GL_ATI_vertex_array_object', @Load_GL_ATI_vertex_array_object, GL_ATI_vertex_array_object);
  //Validate('GL_ATI_vertex_attrib_array_object', @Load_GL_ATI_vertex_attrib_array_object, GL_ATI_vertex_attrib_array_object);
  //Validate('GL_ATI_vertex_streams', @Load_GL_ATI_vertex_streams, GL_ATI_vertex_streams);
end;

{ TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsNvidia }

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsNvidia.SetGL_NV_blend_square
  (AValue: TSDLExtensionOption);
begin
  if FGL_NV_blend_square=AValue then Exit;
  FGL_NV_blend_square:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsNvidia.SetGL_NV_copy_depth_to_color
  (AValue: TSDLExtensionOption);
begin
  if FGL_NV_copy_depth_to_color=AValue then Exit;
  FGL_NV_copy_depth_to_color:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsNvidia.SetGL_NV_depth_clamp
  (AValue: TSDLExtensionOption);
begin
  if FGL_NV_depth_clamp=AValue then Exit;
  FGL_NV_depth_clamp:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsNvidia.SetGL_NV_element_array
  (AValue: TSDLExtensionOption);
begin
  if FGL_NV_element_array=AValue then Exit;
  FGL_NV_element_array:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsNvidia.SetGL_NV_evaluators
  (AValue: TSDLExtensionOption);
begin
  if FGL_NV_evaluators=AValue then Exit;
  FGL_NV_evaluators:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsNvidia.SetGL_NV_fence(
  AValue: TSDLExtensionOption);
begin
  if FGL_NV_fence=AValue then Exit;
  FGL_NV_fence:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsNvidia.SetGL_NV_float_buffer
  (AValue: TSDLExtensionOption);
begin
  if FGL_NV_float_buffer=AValue then Exit;
  FGL_NV_float_buffer:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsNvidia.SetGL_NV_fog_distance
  (AValue: TSDLExtensionOption);
begin
  if FGL_NV_fog_distance=AValue then Exit;
  FGL_NV_fog_distance:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsNvidia.SetGL_NV_fragment_program
  (AValue: TSDLExtensionOption);
begin
  if FGL_NV_fragment_program=AValue then Exit;
  FGL_NV_fragment_program:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsNvidia.SetGL_NV_fragment_program2
  (AValue: TSDLExtensionOption);
begin
  if FGL_NV_fragment_program2=AValue then Exit;
  FGL_NV_fragment_program2:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsNvidia.SetGL_NV_fragment_program_option
  (AValue: TSDLExtensionOption);
begin
  if FGL_NV_fragment_program_option=AValue then Exit;
  FGL_NV_fragment_program_option:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsNvidia.SetGL_NV_half_float
  (AValue: TSDLExtensionOption);
begin
  if FGL_NV_half_float=AValue then Exit;
  FGL_NV_half_float:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsNvidia.SetGL_NV_light_max_exponent
  (AValue: TSDLExtensionOption);
begin
  if FGL_NV_light_max_exponent=AValue then Exit;
  FGL_NV_light_max_exponent:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsNvidia.SetGL_NV_multisample_filter_hint
  (AValue: TSDLExtensionOption);
begin
  if FGL_NV_multisample_filter_hint=AValue then Exit;
  FGL_NV_multisample_filter_hint:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsNvidia.SetGL_NV_occlusion_query
  (AValue: TSDLExtensionOption);
begin
  if FGL_NV_occlusion_query=AValue then Exit;
  FGL_NV_occlusion_query:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsNvidia.SetGL_NV_packed_depth_stencil
  (AValue: TSDLExtensionOption);
begin
  if FGL_NV_packed_depth_stencil=AValue then Exit;
  FGL_NV_packed_depth_stencil:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsNvidia.SetGL_NV_pixel_data_range
  (AValue: TSDLExtensionOption);
begin
  if FGL_NV_pixel_data_range=AValue then Exit;
  FGL_NV_pixel_data_range:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsNvidia.SetGL_NV_point_sprite
  (AValue: TSDLExtensionOption);
begin
  if FGL_NV_point_sprite=AValue then Exit;
  FGL_NV_point_sprite:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsNvidia.SetGL_NV_primitive_restart
  (AValue: TSDLExtensionOption);
begin
  if FGL_NV_primitive_restart=AValue then Exit;
  FGL_NV_primitive_restart:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsNvidia.SetGL_NV_register_combiners
  (AValue: TSDLExtensionOption);
begin
  if FGL_NV_register_combiners=AValue then Exit;
  FGL_NV_register_combiners:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsNvidia.SetGL_NV_register_combiners2
  (AValue: TSDLExtensionOption);
begin
  if FGL_NV_register_combiners2=AValue then Exit;
  FGL_NV_register_combiners2:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsNvidia.SetGL_NV_texgen_emboss
  (AValue: TSDLExtensionOption);
begin
  if FGL_NV_texgen_emboss=AValue then Exit;
  FGL_NV_texgen_emboss:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsNvidia.SetGL_NV_texgen_reflection
  (AValue: TSDLExtensionOption);
begin
  if FGL_NV_texgen_reflection=AValue then Exit;
  FGL_NV_texgen_reflection:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsNvidia.SetGL_NV_texture_compression_vtc
  (AValue: TSDLExtensionOption);
begin
  if FGL_NV_texture_compression_vtc=AValue then Exit;
  FGL_NV_texture_compression_vtc:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsNvidia.SetGL_NV_texture_env_combine4
  (AValue: TSDLExtensionOption);
begin
  if FGL_NV_texture_env_combine4=AValue then Exit;
  FGL_NV_texture_env_combine4:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsNvidia.SetGL_NV_texture_expand_normal
  (AValue: TSDLExtensionOption);
begin
  if FGL_NV_texture_expand_normal=AValue then Exit;
  FGL_NV_texture_expand_normal:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsNvidia.SetGL_NV_texture_rectangle
  (AValue: TSDLExtensionOption);
begin
  if FGL_NV_texture_rectangle=AValue then Exit;
  FGL_NV_texture_rectangle:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsNvidia.SetGL_NV_texture_shader
  (AValue: TSDLExtensionOption);
begin
  if FGL_NV_texture_shader=AValue then Exit;
  FGL_NV_texture_shader:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsNvidia.SetGL_NV_texture_shader2
  (AValue: TSDLExtensionOption);
begin
  if FGL_NV_texture_shader2=AValue then Exit;
  FGL_NV_texture_shader2:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsNvidia.SetGL_NV_texture_shader3
  (AValue: TSDLExtensionOption);
begin
  if FGL_NV_texture_shader3=AValue then Exit;
  FGL_NV_texture_shader3:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsNvidia.SetGL_NV_vertex_array_range
  (AValue: TSDLExtensionOption);
begin
  if FGL_NV_vertex_array_range=AValue then Exit;
  FGL_NV_vertex_array_range:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsNvidia.SetGL_NV_vertex_array_range2
  (AValue: TSDLExtensionOption);
begin
  if FGL_NV_vertex_array_range2=AValue then Exit;
  FGL_NV_vertex_array_range2:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsNvidia.SetGL_NV_vertex_program
  (AValue: TSDLExtensionOption);
begin
  if FGL_NV_vertex_program=AValue then Exit;
  FGL_NV_vertex_program:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsNvidia.SetGL_NV_vertex_program1_1
  (AValue: TSDLExtensionOption);
begin
  if FGL_NV_vertex_program1_1=AValue then Exit;
  FGL_NV_vertex_program1_1:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsNvidia.SetGL_NV_vertex_program2
  (AValue: TSDLExtensionOption);
begin
  if FGL_NV_vertex_program2=AValue then Exit;
  FGL_NV_vertex_program2:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsNvidia.SetGL_NV_vertex_program2_option
  (AValue: TSDLExtensionOption);
begin
  if FGL_NV_vertex_program2_option=AValue then Exit;
  FGL_NV_vertex_program2_option:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsNvidia.SetGL_NV_vertex_program3
  (AValue: TSDLExtensionOption);
begin
  if FGL_NV_vertex_program3=AValue then Exit;
  FGL_NV_vertex_program3:=AValue;
end;

function TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsNvidia.DefaultOption: String;
begin
  Result := 'extIgnore'
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsNvidia.Load;
begin
  //Validate('GL_NV_blend_square', @Load_GL_NV_blend_square, GL_NV_blend_square);
  //Validate('GL_NV_copy_depth_to_color', @Load_GL_NV_copy_depth_to_color, GL_NV_copy_depth_to_color);
  //Validate('GL_NV_depth_clamp', @Load_GL_NV_depth_clamp, GL_NV_depth_clamp);
  //Validate('GL_NV_element_array', @Load_GL_NV_element_array, GL_NV_element_array);
  //Validate('GL_NV_evaluators', @Load_GL_NV_evaluators, GL_NV_evaluators);
  //Validate('GL_NV_fence', @Load_GL_NV_fence, GL_NV_fence);
  //Validate('GL_NV_float_buffer', @Load_GL_NV_float_buffer, GL_NV_float_buffer);
  //Validate('GL_NV_fog_distance', @Load_GL_NV_fog_distance, GL_NV_fog_distance);
  //Validate('GL_NV_fragment_program', @Load_GL_NV_fragment_program, GL_NV_fragment_program);
  //Validate('GL_NV_fragment_program2', @Load_GL_NV_fragment_program2, GL_NV_fragment_program2);
  //Validate('GL_NV_fragment_program_option', @Load_GL_NV_fragment_program_option, GL_NV_fragment_program_option);
  //Validate('GL_NV_half_float', @Load_GL_NV_half_float, GL_NV_half_float);
  //Validate('GL_NV_light_max_exponent', @Load_GL_NV_light_max_exponent, GL_NV_light_max_exponent);
  //Validate('GL_NV_multisample_filter_hint', @Load_GL_NV_multisample_filter_hint, GL_NV_multisample_filter_hint);
  //Validate('GL_NV_occlusion_query', @Load_GL_NV_occlusion_query, GL_NV_occlusion_query);
  //Validate('GL_NV_packed_depth_stencil', @Load_GL_NV_packed_depth_stencil, GL_NV_packed_depth_stencil);
  //Validate('GL_NV_pixel_data_range', @Load_GL_NV_pixel_data_range, GL_NV_pixel_data_range);
  //Validate('GL_NV_point_sprite', @Load_GL_NV_point_sprite, GL_NV_point_sprite);
  //Validate('GL_NV_primitive_restart', @Load_GL_NV_primitive_restart, GL_NV_primitive_restart);
  //Validate('GL_NV_register_combiners', @Load_GL_NV_register_combiners, GL_NV_register_combiners);
  //Validate('GL_NV_register_combiners2', @Load_GL_NV_register_combiners2, GL_NV_register_combiners2);
  //Validate('GL_NV_texgen_emboss', @Load_GL_NV_texgen_emboss, GL_NV_texgen_emboss);
  //Validate('GL_NV_texgen_reflection', @Load_GL_NV_texgen_reflection, GL_NV_texgen_reflection);
  //Validate('GL_NV_texture_compression_vtc', @Load_GL_NV_texture_compression_vtc, GL_NV_texture_compression_vtc);
  //Validate('GL_NV_texture_env_combine4', @Load_GL_NV_texture_env_combine4, GL_NV_texture_env_combine4);
  //Validate('GL_NV_texture_expand_normal', @Load_GL_NV_texture_expand_normal, GL_NV_texture_expand_normal);
  //Validate('GL_NV_texture_rectangle', @Load_GL_NV_texture_rectangle, GL_NV_texture_rectangle);
  //Validate('GL_NV_texture_shader', @Load_GL_NV_texture_shader, GL_NV_texture_shader);
  //Validate('GL_NV_texture_shader2', @Load_GL_NV_texture_shader2, GL_NV_texture_shader2);
  //Validate('GL_NV_texture_shader3', @Load_GL_NV_texture_shader3, GL_NV_texture_shader3);
  //Validate('GL_NV_vertex_array_range', @Load_GL_NV_vertex_array_range, GL_NV_vertex_array_range);
  //Validate('GL_NV_vertex_array_range2', @Load_GL_NV_vertex_array_range2, GL_NV_vertex_array_range2);
  //Validate('GL_NV_vertex_program', @Load_GL_NV_vertex_program, GL_NV_vertex_program);
  //Validate('GL_NV_vertex_program1_1', @Load_GL_NV_vertex_program1_1, GL_NV_vertex_program1_1);
  //Validate('GL_NV_vertex_program2', @Load_GL_NV_vertex_program2, GL_NV_vertex_program2);
  //Validate('GL_NV_vertex_program2_option', @Load_GL_NV_vertex_program2_option, GL_NV_vertex_program2_option);
  //Validate('GL_NV_vertex_program3', @Load_GL_NV_vertex_program3, GL_NV_vertex_program3);
end;

{ TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsHP }

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsHP.SetGL_HP_occlusion_test
  (AValue: TSDLExtensionOption);
begin
  if FGL_HP_occlusion_test=AValue then Exit;
  FGL_HP_occlusion_test:=AValue;
end;

function TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsHP.DefaultOption: String;
begin
  Result := 'extIgnore'
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsHP.Load;
begin
  //Validate('GL_HP_occlusion_test', @Load_GL_HP_occlusion_test, GL_HP_occlusion_test);
end;

{ TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsARB }

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsARB.SetGL_ARB_color_buffer_float
  (AValue: TSDLExtensionOption);
begin
  if FGL_ARB_color_buffer_float=AValue then Exit;
  FGL_ARB_color_buffer_float:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsARB.SetGL_ARB_depth_texture
  (AValue: TSDLExtensionOption);
begin
  if FGL_ARB_depth_texture=AValue then Exit;
  FGL_ARB_depth_texture:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsARB.SetGL_ARB_draw_buffers
  (AValue: TSDLExtensionOption);
begin
  if FGL_ARB_draw_buffers=AValue then Exit;
  FGL_ARB_draw_buffers:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsARB.SetGL_ARB_fragment_program
  (AValue: TSDLExtensionOption);
begin
  if FGL_ARB_fragment_program=AValue then Exit;
  FGL_ARB_fragment_program:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsARB.SetGL_ARB_fragment_program_shadow
  (AValue: TSDLExtensionOption);
begin
  if FGL_ARB_fragment_program_shadow=AValue then Exit;
  FGL_ARB_fragment_program_shadow:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsARB.SetGL_ARB_fragment_shader
  (AValue: TSDLExtensionOption);
begin
  if FGL_ARB_fragment_shader=AValue then Exit;
  FGL_ARB_fragment_shader:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsARB.SetGL_ARB_half_float_pixel
  (AValue: TSDLExtensionOption);
begin
  if FGL_ARB_half_float_pixel=AValue then Exit;
  FGL_ARB_half_float_pixel:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsARB.SetGL_ARB_imaging(
  AValue: TSDLExtensionOption);
begin
  if FGL_ARB_imaging=AValue then Exit;
  FGL_ARB_imaging:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsARB.SetGL_ARB_matrix_palette
  (AValue: TSDLExtensionOption);
begin
  if FGL_ARB_matrix_palette=AValue then Exit;
  FGL_ARB_matrix_palette:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsARB.SetGL_ARB_multisample
  (AValue: TSDLExtensionOption);
begin
  if FGL_ARB_multisample=AValue then Exit;
  FGL_ARB_multisample:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsARB.SetGL_ARB_multitexture
  (AValue: TSDLExtensionOption);
begin
  if FGL_ARB_multitexture=AValue then Exit;
  FGL_ARB_multitexture:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsARB.SetGL_ARB_occlusion_query
  (AValue: TSDLExtensionOption);
begin
  if FGL_ARB_occlusion_query=AValue then Exit;
  FGL_ARB_occlusion_query:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsARB.SetGL_ARB_pixel_buffer_object
  (AValue: TSDLExtensionOption);
begin
  if FGL_ARB_pixel_buffer_object=AValue then Exit;
  FGL_ARB_pixel_buffer_object:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsARB.SetGL_ARB_point_parameters
  (AValue: TSDLExtensionOption);
begin
  if FGL_ARB_point_parameters=AValue then Exit;
  FGL_ARB_point_parameters:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsARB.SetGL_ARB_point_sprite
  (AValue: TSDLExtensionOption);
begin
  if FGL_ARB_point_sprite=AValue then Exit;
  FGL_ARB_point_sprite:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsARB.SetGL_ARB_shader_objects
  (AValue: TSDLExtensionOption);
begin
  if FGL_ARB_shader_objects=AValue then Exit;
  FGL_ARB_shader_objects:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsARB.SetGL_ARB_shading_language_100
  (AValue: TSDLExtensionOption);
begin
  if FGL_ARB_shading_language_100=AValue then Exit;
  FGL_ARB_shading_language_100:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsARB.SetGL_ARB_shadow(
  AValue: TSDLExtensionOption);
begin
  if FGL_ARB_shadow=AValue then Exit;
  FGL_ARB_shadow:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsARB.SetGL_ARB_shadow_ambient
  (AValue: TSDLExtensionOption);
begin
  if FGL_ARB_shadow_ambient=AValue then Exit;
  FGL_ARB_shadow_ambient:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsARB.SetGL_ARB_texture_border_clamp
  (AValue: TSDLExtensionOption);
begin
  if FGL_ARB_texture_border_clamp=AValue then Exit;
  FGL_ARB_texture_border_clamp:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsARB.SetGL_ARB_texture_compression
  (AValue: TSDLExtensionOption);
begin
  if FGL_ARB_texture_compression=AValue then Exit;
  FGL_ARB_texture_compression:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsARB.SetGL_ARB_texture_cube_map
  (AValue: TSDLExtensionOption);
begin
  if FGL_ARB_texture_cube_map=AValue then Exit;
  FGL_ARB_texture_cube_map:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsARB.SetGL_ARB_texture_env_add
  (AValue: TSDLExtensionOption);
begin
  if FGL_ARB_texture_env_add=AValue then Exit;
  FGL_ARB_texture_env_add:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsARB.SetGL_ARB_texture_env_combine
  (AValue: TSDLExtensionOption);
begin
  if FGL_ARB_texture_env_combine=AValue then Exit;
  FGL_ARB_texture_env_combine:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsARB.SetGL_ARB_texture_env_crossbar
  (AValue: TSDLExtensionOption);
begin
  if FGL_ARB_texture_env_crossbar=AValue then Exit;
  FGL_ARB_texture_env_crossbar:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsARB.SetGL_ARB_texture_env_dot3
  (AValue: TSDLExtensionOption);
begin
  if FGL_ARB_texture_env_dot3=AValue then Exit;
  FGL_ARB_texture_env_dot3:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsARB.SetGL_ARB_texture_float
  (AValue: TSDLExtensionOption);
begin
  if FGL_ARB_texture_float=AValue then Exit;
  FGL_ARB_texture_float:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsARB.SetGL_ARB_texture_mirrored_repeat
  (AValue: TSDLExtensionOption);
begin
  if FGL_ARB_texture_mirrored_repeat=AValue then Exit;
  FGL_ARB_texture_mirrored_repeat:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsARB.SetGL_ARB_texture_non_power_of_two
  (AValue: TSDLExtensionOption);
begin
  if FGL_ARB_texture_non_power_of_two=AValue then Exit;
  FGL_ARB_texture_non_power_of_two:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsARB.SetGL_ARB_texture_rectangle
  (AValue: TSDLExtensionOption);
begin
  if FGL_ARB_texture_rectangle=AValue then Exit;
  FGL_ARB_texture_rectangle:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsARB.SetGL_ARB_transpose_matrix
  (AValue: TSDLExtensionOption);
begin
  if FGL_ARB_transpose_matrix=AValue then Exit;
  FGL_ARB_transpose_matrix:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsARB.SetGL_ARB_vertex_blend
  (AValue: TSDLExtensionOption);
begin
  if FGL_ARB_vertex_blend=AValue then Exit;
  FGL_ARB_vertex_blend:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsARB.SetGL_ARB_vertex_buffer_object
  (AValue: TSDLExtensionOption);
begin
  if FGL_ARB_vertex_buffer_object=AValue then Exit;
  FGL_ARB_vertex_buffer_object:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsARB.SetGL_ARB_vertex_program
  (AValue: TSDLExtensionOption);
begin
  if FGL_ARB_vertex_program=AValue then Exit;
  FGL_ARB_vertex_program:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsARB.SetGL_ARB_vertex_shader
  (AValue: TSDLExtensionOption);
begin
  if FGL_ARB_vertex_shader=AValue then Exit;
  FGL_ARB_vertex_shader:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsARB.SetGL_ARB_window_pos
  (AValue: TSDLExtensionOption);
begin
  if FGL_ARB_window_pos=AValue then Exit;
  FGL_ARB_window_pos:=AValue;
end;

function TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsARB.DefaultOption: String;
begin
  Result := 'extOptional'
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsARB.Load;
begin
  //Validate('GL_ARB_color_buffer_float', @Load_GL_ARB_color_buffer_float, GL_ARB_color_buffer_float);
  //Validate('GL_ARB_depth_texture', @Load_GL_ARB_depth_texture, GL_ARB_depth_texture);
  //Validate('GL_ARB_draw_buffers', @Load_GL_ARB_draw_buffers, GL_ARB_draw_buffers);
  //Validate('GL_ARB_fragment_program', @Load_GL_ARB_fragment_program, GL_ARB_fragment_program);
  //Validate('GL_ARB_fragment_shader', @Load_GL_ARB_fragment_shader, GL_ARB_fragment_shader);
  //Validate('GL_ARB_half_float_pixel', @Load_GL_ARB_half_float_pixel, GL_ARB_half_float_pixel);
  //Validate('GL_ARB_imaging', @Load_GL_ARB_imaging, GL_ARB_imaging);
  //Validate('GL_ARB_matrix_palette', @Load_GL_ARB_matrix_palette, GL_ARB_matrix_palette);
  //Validate('GL_ARB_multisample', @Load_GL_ARB_multisample, GL_ARB_multisample);
  //Validate('GL_ARB_multitexture', @Load_GL_ARB_multitexture, GL_ARB_multitexture);
  //Validate('GL_ARB_occlusion_query', @Load_GL_ARB_occlusion_query, GL_ARB_occlusion_query);
  //Validate('GL_ARB_pixel_buffer_object', @Load_GL_ARB_pixel_buffer_object, GL_ARB_pixel_buffer_object);
  //Validate('GL_ARB_point_parameters', @Load_GL_ARB_point_parameters, GL_ARB_point_parameters);
  //Validate('GL_ARB_point_sprite', @Load_GL_ARB_point_sprite, GL_ARB_point_sprite);
  //Validate('GL_ARB_fragment_program_shadow', @Load_GL_ARB_fragment_program_shadow, GL_ARB_fragment_program_shadow);
  //Validate('GL_ARB_shader_objects', @Load_GL_ARB_shader_objects, GL_ARB_shader_objects);
  //Validate('GL_ARB_shading_language_100', @Load_GL_ARB_shading_language_100, GL_ARB_shading_language_100);
  //Validate('GL_ARB_shadow', @Load_GL_ARB_shadow, GL_ARB_shadow);
  //Validate('GL_ARB_shadow_ambient', @Load_GL_ARB_shadow_ambient, GL_ARB_shadow_ambient);
  //Validate('GL_ARB_texture_border_clamp', @Load_GL_ARB_texture_border_clamp, GL_ARB_texture_border_clamp);
  //Validate('GL_ARB_texture_compression', @Load_GL_ARB_texture_compression, GL_ARB_texture_compression);
  //Validate('GL_ARB_texture_cube_map', @Load_GL_ARB_texture_cube_map, GL_ARB_texture_cube_map);
  //Validate('GL_ARB_texture_env_add', @Load_GL_ARB_texture_env_add, GL_ARB_texture_env_add);
  //Validate('GL_ARB_texture_env_combine', @Load_GL_ARB_texture_env_combine, GL_ARB_texture_env_combine);
  //Validate('GL_ARB_texture_env_crossbar', @Load_GL_ARB_texture_env_crossbar, GL_ARB_texture_env_crossbar);
  //Validate('GL_ARB_texture_env_dot3', @Load_GL_ARB_texture_env_dot3, GL_ARB_texture_env_dot3);
  //Validate('GL_ARB_texture_float', @Load_GL_ARB_texture_float, GL_ARB_texture_float);
  //Validate('GL_ARB_texture_mirrored_repeat', @Load_GL_ARB_texture_mirrored_repeat, GL_ARB_texture_mirrored_repeat);
  //Validate('GL_ARB_texture_non_power_of_two', @Load_GL_ARB_texture_non_power_of_two, GL_ARB_texture_non_power_of_two);
  //Validate('GL_ARB_texture_rectangle', @Load_GL_ARB_texture_rectangle, GL_ARB_texture_rectangle);
  //Validate('GL_ARB_transpose_matrix', @Load_GL_ARB_transpose_matrix, GL_ARB_transpose_matrix);
  //Validate('GL_ARB_vertex_blend', @Load_GL_ARB_vertex_blend, GL_ARB_vertex_blend);
  //Validate('GL_ARB_vertex_buffer_object', @Load_GL_ARB_vertex_buffer_object, GL_ARB_vertex_buffer_object);
  //Validate('GL_ARB_vertex_program', @Load_GL_ARB_vertex_program, GL_ARB_vertex_program);
  //Validate('GL_ARB_vertex_shader', @Load_GL_ARB_vertex_shader, GL_ARB_vertex_shader);
  //Validate('GL_ARB_window_pos', @Load_GL_ARB_window_pos, GL_ARB_window_pos);
end;

{ TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsExt }

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsExt.SetGL_EXT_422_pixels
  (AValue: TSDLExtensionOption);
begin
  if FGL_EXT_422_pixels=AValue then Exit;
  FGL_EXT_422_pixels:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsExt.SetGL_EXT_abgr(
  AValue: TSDLExtensionOption);
begin
  if FGL_EXT_abgr=AValue then Exit;
  FGL_EXT_abgr:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsExt.SetGL_EXT_bgra(
  AValue: TSDLExtensionOption);
begin
  if FGL_EXT_bgra=AValue then Exit;
  FGL_EXT_bgra:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsExt.SetGL_EXT_blend_color
  (AValue: TSDLExtensionOption);
begin
  if FGL_EXT_blend_color=AValue then Exit;
  FGL_EXT_blend_color:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsExt.SetGL_EXT_blend_equation_separate
  (AValue: TSDLExtensionOption);
begin
  if FGL_EXT_blend_equation_separate=AValue then Exit;
  FGL_EXT_blend_equation_separate:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsExt.SetGL_EXT_blend_func_separate
  (AValue: TSDLExtensionOption);
begin
  if FGL_EXT_blend_func_separate=AValue then Exit;
  FGL_EXT_blend_func_separate:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsExt.SetGL_EXT_blend_logic_op
  (AValue: TSDLExtensionOption);
begin
  if FGL_EXT_blend_logic_op=AValue then Exit;
  FGL_EXT_blend_logic_op:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsExt.SetGL_EXT_blend_minmax
  (AValue: TSDLExtensionOption);
begin
  if FGL_EXT_blend_minmax=AValue then Exit;
  FGL_EXT_blend_minmax:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsExt.SetGL_EXT_blend_subtract
  (AValue: TSDLExtensionOption);
begin
  if FGL_EXT_blend_subtract=AValue then Exit;
  FGL_EXT_blend_subtract:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsExt.SetGL_EXT_clip_volume_hint
  (AValue: TSDLExtensionOption);
begin
  if FGL_EXT_clip_volume_hint=AValue then Exit;
  FGL_EXT_clip_volume_hint:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsExt.SetGL_EXT_color_subtable
  (AValue: TSDLExtensionOption);
begin
  if FGL_EXT_color_subtable=AValue then Exit;
  FGL_EXT_color_subtable:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsExt.SetGL_EXT_compiled_vertex_array
  (AValue: TSDLExtensionOption);
begin
  if FGL_EXT_compiled_vertex_array=AValue then Exit;
  FGL_EXT_compiled_vertex_array:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsExt.SetGL_EXT_convolution
  (AValue: TSDLExtensionOption);
begin
  if FGL_EXT_convolution=AValue then Exit;
  FGL_EXT_convolution:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsExt.SetGL_EXT_depth_bounds_test
  (AValue: TSDLExtensionOption);
begin
  if FGL_EXT_depth_bounds_test=AValue then Exit;
  FGL_EXT_depth_bounds_test:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsExt.SetGL_EXT_fog_coord
  (AValue: TSDLExtensionOption);
begin
  if FGL_EXT_fog_coord=AValue then Exit;
  FGL_EXT_fog_coord:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsExt.SetGL_EXT_framebuffer_object
  (AValue: TSDLExtensionOption);
begin
  if FGL_EXT_framebuffer_object=AValue then Exit;
  FGL_EXT_framebuffer_object:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsExt.SetGL_EXT_histogram
  (AValue: TSDLExtensionOption);
begin
  if FGL_EXT_histogram=AValue then Exit;
  FGL_EXT_histogram:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsExt.SetGL_EXT_multi_draw_arrays
  (AValue: TSDLExtensionOption);
begin
  if FGL_EXT_multi_draw_arrays=AValue then Exit;
  FGL_EXT_multi_draw_arrays:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsExt.SetGL_EXT_packed_pixels
  (AValue: TSDLExtensionOption);
begin
  if FGL_EXT_packed_pixels=AValue then Exit;
  FGL_EXT_packed_pixels:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsExt.SetGL_EXT_paletted_texture
  (AValue: TSDLExtensionOption);
begin
  if FGL_EXT_paletted_texture=AValue then Exit;
  FGL_EXT_paletted_texture:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsExt.SetGL_EXT_pixel_buffer_object
  (AValue: TSDLExtensionOption);
begin
  if FGL_EXT_pixel_buffer_object=AValue then Exit;
  FGL_EXT_pixel_buffer_object:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsExt.SetGL_EXT_point_parameters
  (AValue: TSDLExtensionOption);
begin
  if FGL_EXT_point_parameters=AValue then Exit;
  FGL_EXT_point_parameters:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsExt.SetGL_EXT_polygon_offset
  (AValue: TSDLExtensionOption);
begin
  if FGL_EXT_polygon_offset=AValue then Exit;
  FGL_EXT_polygon_offset:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsExt.SetGL_EXT_secondary_color
  (AValue: TSDLExtensionOption);
begin
  if FGL_EXT_secondary_color=AValue then Exit;
  FGL_EXT_secondary_color:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsExt.SetGL_EXT_separate_specular_color
  (AValue: TSDLExtensionOption);
begin
  if FGL_EXT_separate_specular_color=AValue then Exit;
  FGL_EXT_separate_specular_color:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsExt.SetGL_EXT_shadow_funcs
  (AValue: TSDLExtensionOption);
begin
  if FGL_EXT_shadow_funcs=AValue then Exit;
  FGL_EXT_shadow_funcs:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsExt.SetGL_EXT_shared_texture_palette
  (AValue: TSDLExtensionOption);
begin
  if FGL_EXT_shared_texture_palette=AValue then Exit;
  FGL_EXT_shared_texture_palette:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsExt.SetGL_EXT_stencil_two_side
  (AValue: TSDLExtensionOption);
begin
  if FGL_EXT_stencil_two_side=AValue then Exit;
  FGL_EXT_stencil_two_side:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsExt.SetGL_EXT_stencil_wrap
  (AValue: TSDLExtensionOption);
begin
  if FGL_EXT_stencil_wrap=AValue then Exit;
  FGL_EXT_stencil_wrap:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsExt.SetGL_EXT_subtexture
  (AValue: TSDLExtensionOption);
begin
  if FGL_EXT_subtexture=AValue then Exit;
  FGL_EXT_subtexture:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsExt.SetGL_EXT_texture3D
  (AValue: TSDLExtensionOption);
begin
  if FGL_EXT_texture3D=AValue then Exit;
  FGL_EXT_texture3D:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsExt.SetGL_EXT_texture_compression_dxt1
  (AValue: TSDLExtensionOption);
begin
  if FGL_EXT_texture_compression_dxt1=AValue then Exit;
  FGL_EXT_texture_compression_dxt1:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsExt.SetGL_EXT_texture_compression_s3tc
  (AValue: TSDLExtensionOption);
begin
  if FGL_EXT_texture_compression_s3tc=AValue then Exit;
  FGL_EXT_texture_compression_s3tc:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsExt.SetGL_EXT_texture_env_add
  (AValue: TSDLExtensionOption);
begin
  if FGL_EXT_texture_env_add=AValue then Exit;
  FGL_EXT_texture_env_add:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsExt.SetGL_EXT_texture_env_combine
  (AValue: TSDLExtensionOption);
begin
  if FGL_EXT_texture_env_combine=AValue then Exit;
  FGL_EXT_texture_env_combine:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsExt.SetGL_EXT_texture_env_dot3
  (AValue: TSDLExtensionOption);
begin
  if FGL_EXT_texture_env_dot3=AValue then Exit;
  FGL_EXT_texture_env_dot3:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsExt.SetGL_EXT_texture_filter_anisotropic
  (AValue: TSDLExtensionOption);
begin
  if FGL_EXT_texture_filter_anisotropic=AValue then Exit;
  FGL_EXT_texture_filter_anisotropic:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsExt.SetGL_EXT_texture_lod_bias
  (AValue: TSDLExtensionOption);
begin
  if FGL_EXT_texture_lod_bias=AValue then Exit;
  FGL_EXT_texture_lod_bias:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsExt.SetGL_EXT_texture_mirror_clamp
  (AValue: TSDLExtensionOption);
begin
  if FGL_EXT_texture_mirror_clamp=AValue then Exit;
  FGL_EXT_texture_mirror_clamp:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsExt.SetGL_EXT_texture_object
  (AValue: TSDLExtensionOption);
begin
  if FGL_EXT_texture_object=AValue then Exit;
  FGL_EXT_texture_object:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsExt.SetGL_EXT_texture_rectangle
  (AValue: TSDLExtensionOption);
begin
  if FGL_EXT_texture_rectangle=AValue then Exit;
  FGL_EXT_texture_rectangle:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsExt.SetGL_EXT_vertex_array
  (AValue: TSDLExtensionOption);
begin
  if FGL_EXT_vertex_array=AValue then Exit;
  FGL_EXT_vertex_array:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsExt.SetGL_EXT_vertex_shader
  (AValue: TSDLExtensionOption);
begin
  if FGL_EXT_vertex_shader=AValue then Exit;
  FGL_EXT_vertex_shader:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsExt.SetGL_EXT_vertex_weighting
  (AValue: TSDLExtensionOption);
begin
  if FGL_EXT_vertex_weighting=AValue then Exit;
  FGL_EXT_vertex_weighting:=AValue;
end;

function TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsExt.DefaultOption: String;
begin
  Result := 'extIgnore'
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsExt.AfterConstruction;
begin
  GL_EXT_texture_compression_s3tc := extOptional;
  inherited AfterConstruction;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsExt.Load;
begin
  //Validate('GL_EXT_422_pixels', @Load_GL_EXT_422_pixels, GL_EXT_422_pixels);
  //Validate('GL_EXT_abgr', @Load_GL_EXT_abgr, GL_EXT_abgr);
  //Validate('GL_EXT_bgra', @Load_GL_EXT_bgra, GL_EXT_bgra);
  //Validate('GL_EXT_blend_color', @Load_GL_EXT_blend_color, GL_EXT_blend_color);
  //Validate('GL_EXT_blend_equation_separate', @Load_GL_EXT_blend_equation_separate, GL_EXT_blend_equation_separate);
  //Validate('GL_EXT_blend_func_separate', @Load_GL_EXT_blend_func_separate, GL_EXT_blend_func_separate);
  //Validate('GL_EXT_blend_logic_op', @Load_GL_EXT_blend_logic_op, GL_EXT_blend_logic_op);
  //Validate('GL_EXT_blend_minmax', @Load_GL_EXT_blend_minmax, GL_EXT_blend_minmax);
  //Validate('GL_EXT_blend_subtract', @Load_GL_EXT_blend_subtract, GL_EXT_blend_subtract);
  //Validate('GL_EXT_clip_volume_hint', @Load_GL_EXT_clip_volume_hint, GL_EXT_clip_volume_hint);
  //Validate('GL_EXT_color_subtable', @Load_GL_EXT_color_subtable, GL_EXT_color_subtable);
  //Validate('GL_EXT_compiled_vertex_array', @Load_GL_EXT_compiled_vertex_array, GL_EXT_compiled_vertex_array);
  //Validate('GL_EXT_convolution', @Load_GL_EXT_convolution, GL_EXT_convolution);
  //Validate('GL_EXT_depth_bounds_test', @Load_GL_EXT_depth_bounds_test, GL_EXT_depth_bounds_test);
  //Validate('GL_EXT_fog_coord', @Load_GL_EXT_fog_coord, GL_EXT_fog_coord);
  //Validate('GL_EXT_framebuffer_object', @Load_GL_EXT_framebuffer_object, GL_EXT_framebuffer_object);
  //Validate('GL_EXT_histogram', @Load_GL_EXT_histogram, GL_EXT_histogram);
  //Validate('GL_EXT_multi_draw_arrays', @Load_GL_EXT_multi_draw_arrays, GL_EXT_multi_draw_arrays);
  //Validate('GL_EXT_packed_pixels', @Load_GL_EXT_packed_pixels, GL_EXT_packed_pixels);
  //Validate('GL_EXT_paletted_texture', @Load_GL_EXT_paletted_texture, GL_EXT_paletted_texture);
  //Validate('GL_EXT_pixel_buffer_object', @Load_GL_EXT_pixel_buffer_object, GL_EXT_pixel_buffer_object);
  //Validate('GL_EXT_point_parameters', @Load_GL_EXT_point_parameters, GL_EXT_point_parameters);
  //Validate('GL_EXT_polygon_offset', @Load_GL_EXT_polygon_offset, GL_EXT_polygon_offset);
  //Validate('GL_EXT_secondary_color', @Load_GL_EXT_secondary_color, GL_EXT_secondary_color);
  //Validate('GL_EXT_separate_specular_color', @Load_GL_EXT_separate_specular_color, GL_EXT_separate_specular_color);
  //Validate('GL_EXT_shadow_funcs', @Load_GL_EXT_shadow_funcs, GL_EXT_shadow_funcs);
  //Validate('GL_EXT_shared_texture_palette', @Load_GL_EXT_shared_texture_palette, GL_EXT_shared_texture_palette);
  //Validate('GL_EXT_stencil_two_side', @Load_GL_EXT_stencil_two_side, GL_EXT_stencil_two_side);
  //Validate('GL_EXT_stencil_wrap', @Load_GL_EXT_stencil_wrap, GL_EXT_stencil_wrap);
  //Validate('GL_EXT_subtexture', @Load_GL_EXT_subtexture, GL_EXT_subtexture);
  //Validate('GL_EXT_texture3D', @Load_GL_EXT_texture3D, GL_EXT_texture3D);
  //Validate('GL_EXT_texture_compression_dxt1', @Load_GL_EXT_texture_compression_dxt1, GL_EXT_texture_compression_dxt1);
  //Validate('GL_EXT_texture_compression_s3tc', @Load_GL_EXT_texture_compression_s3tc, GL_EXT_texture_compression_s3tc);
  //Validate('GL_EXT_texture_env_add', @Load_GL_EXT_texture_env_add, GL_EXT_texture_env_add);
  //Validate('GL_EXT_texture_env_combine', @Load_GL_EXT_texture_env_combine, GL_EXT_texture_env_combine);
  //Validate('GL_EXT_texture_env_dot3', @Load_GL_EXT_texture_env_dot3, GL_EXT_texture_env_dot3);
  //Validate('GL_EXT_texture_filter_anisotropic', @Load_GL_EXT_texture_filter_anisotropic, GL_EXT_texture_filter_anisotropic);
  //Validate('GL_EXT_texture_lod_bias', @Load_GL_EXT_texture_lod_bias, GL_EXT_texture_lod_bias);
  //Validate('GL_EXT_texture_mirror_clamp', @Load_GL_EXT_texture_mirror_clamp, GL_EXT_texture_mirror_clamp);
  //Validate('GL_EXT_texture_object', @Load_GL_EXT_texture_object, GL_EXT_texture_object);
  //Validate('GL_EXT_texture_rectangle', @Load_GL_EXT_texture_rectangle, GL_EXT_texture_rectangle);
  //Validate('GL_EXT_vertex_array', @Load_GL_EXT_vertex_array, GL_EXT_vertex_array);
  //Validate('GL_EXT_vertex_shader', @Load_GL_EXT_vertex_shader, GL_EXT_vertex_shader);
  //Validate('GL_EXT_vertex_weighting', @Load_GL_EXT_vertex_weighting, GL_EXT_vertex_weighting);
end;

{ TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsWindows }

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsWindows.SetWGL_ARB_buffer_region
  (AValue: TSDLExtensionOption);
begin
  if FWGL_ARB_buffer_region=AValue then Exit;
  FWGL_ARB_buffer_region:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsWindows.SetWGL_ARB_extensions_string
  (AValue: TSDLExtensionOption);
begin
  if FWGL_ARB_extensions_string=AValue then Exit;
  FWGL_ARB_extensions_string:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsWindows.SetWGL_ARB_make_current_read
  (AValue: TSDLExtensionOption);
begin
  if FWGL_ARB_make_current_read=AValue then Exit;
  FWGL_ARB_make_current_read:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsWindows.SetWGL_ARB_pbuffer
  (AValue: TSDLExtensionOption);
begin
  if FWGL_ARB_pbuffer=AValue then Exit;
  FWGL_ARB_pbuffer:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsWindows.SetWGL_ARB_pixel_format
  (AValue: TSDLExtensionOption);
begin
  if FWGL_ARB_pixel_format=AValue then Exit;
  FWGL_ARB_pixel_format:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsWindows.SetWGL_ARB_render_texture
  (AValue: TSDLExtensionOption);
begin
  if FWGL_ARB_render_texture=AValue then Exit;
  FWGL_ARB_render_texture:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsWindows.SetWGL_ATI_pixel_format_float
  (AValue: TSDLExtensionOption);
begin
  if FWGL_ATI_pixel_format_float=AValue then Exit;
  FWGL_ATI_pixel_format_float:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsWindows.SetWGL_EXT_extensions_string
  (AValue: TSDLExtensionOption);
begin
  if FWGL_EXT_extensions_string=AValue then Exit;
  FWGL_EXT_extensions_string:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsWindows.SetWGL_EXT_make_current_read
  (AValue: TSDLExtensionOption);
begin
  if FWGL_EXT_make_current_read=AValue then Exit;
  FWGL_EXT_make_current_read:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsWindows.SetWGL_EXT_pbuffer
  (AValue: TSDLExtensionOption);
begin
  if FWGL_EXT_pbuffer=AValue then Exit;
  FWGL_EXT_pbuffer:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsWindows.SetWGL_EXT_pixel_format
  (AValue: TSDLExtensionOption);
begin
  if FWGL_EXT_pixel_format=AValue then Exit;
  FWGL_EXT_pixel_format:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsWindows.SetWGL_EXT_swap_control
  (AValue: TSDLExtensionOption);
begin
  if FWGL_EXT_swap_control=AValue then Exit;
  FWGL_EXT_swap_control:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsWindows.SetWGL_I3D_digital_video_control
  (AValue: TSDLExtensionOption);
begin
  if FWGL_I3D_digital_video_control=AValue then Exit;
  FWGL_I3D_digital_video_control:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsWindows.SetWGL_I3D_gamma
  (AValue: TSDLExtensionOption);
begin
  if FWGL_I3D_gamma=AValue then Exit;
  FWGL_I3D_gamma:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsWindows.SetWGL_I3D_genlock
  (AValue: TSDLExtensionOption);
begin
  if FWGL_I3D_genlock=AValue then Exit;
  FWGL_I3D_genlock:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsWindows.SetWGL_I3D_image_buffer
  (AValue: TSDLExtensionOption);
begin
  if FWGL_I3D_image_buffer=AValue then Exit;
  FWGL_I3D_image_buffer:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsWindows.SetWGL_I3D_swap_frame_lock
  (AValue: TSDLExtensionOption);
begin
  if FWGL_I3D_swap_frame_lock=AValue then Exit;
  FWGL_I3D_swap_frame_lock:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsWindows.SetWGL_I3D_swap_frame_usage
  (AValue: TSDLExtensionOption);
begin
  if FWGL_I3D_swap_frame_usage=AValue then Exit;
  FWGL_I3D_swap_frame_usage:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsWindows.SetWGL_NV_render_texture_rectangle
  (AValue: TSDLExtensionOption);
begin
  if FWGL_NV_render_texture_rectangle=AValue then Exit;
  FWGL_NV_render_texture_rectangle:=AValue;
end;

function TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsWindows.DefaultOption: String;
begin
  Result := 'extIgnore'
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLExtensionsWindows.Load;
begin
  {$IFDEF WINDOWS}
  {Validate('WGL_ARB_buffer_region', @Load_WGL_ARB_buffer_region, WGL_ARB_buffer_region);
  Validate('WGL_ARB_extensions_string', @Load_WGL_ARB_extensions_string, WGL_ARB_extensions_string);
  Validate('WGL_ARB_make_current_read', @Load_WGL_ARB_make_current_read, WGL_ARB_make_current_read);
  Validate('WGL_ARB_pbuffer', @Load_WGL_ARB_pbuffer, WGL_ARB_pbuffer);
  Validate('WGL_ARB_pixel_format', @Load_WGL_ARB_pixel_format, WGL_ARB_pixel_format);
  Validate('WGL_ARB_render_texture', @Load_WGL_ARB_render_texture, WGL_ARB_render_texture);
  Validate('WGL_ATI_pixel_format_float', @Load_WGL_ATI_pixel_format_float, WGL_ATI_pixel_format_float);
  Validate('WGL_EXT_extensions_string', @Load_WGL_EXT_extensions_string, WGL_EXT_extensions_string);
  Validate('WGL_EXT_make_current_read', @Load_WGL_EXT_make_current_read, WGL_EXT_make_current_read);
  Validate('WGL_EXT_pbuffer', @Load_WGL_EXT_pbuffer, WGL_EXT_pbuffer);
  Validate('WGL_EXT_pixel_format', @Load_WGL_EXT_pixel_format, WGL_EXT_pixel_format);
  Validate('WGL_EXT_swap_control', @Load_WGL_EXT_swap_control, WGL_EXT_swap_control);
  Validate('WGL_I3D_digital_video_control', @Load_WGL_I3D_digital_video_control, WGL_I3D_digital_video_control);
  Validate('WGL_I3D_gamma', @Load_WGL_I3D_gamma, WGL_I3D_gamma);
  Validate('WGL_I3D_genlock', @Load_WGL_I3D_genlock, WGL_I3D_genlock);
  Validate('WGL_I3D_image_buffer', @Load_WGL_I3D_image_buffer, WGL_I3D_image_buffer);
  Validate('WGL_I3D_swap_frame_lock', @Load_WGL_I3D_swap_frame_lock, WGL_I3D_swap_frame_lock);
  Validate('WGL_I3D_swap_frame_usage', @Load_WGL_I3D_swap_frame_usage, WGL_I3D_swap_frame_usage);
  Validate('WGL_NV_render_texture_rectangle', @Load_WGL_NV_render_texture_rectangle, WGL_NV_render_texture_rectangle);}
  {$ENDIF}
end;

{ TSDLCustomOpenGLExtensions.TSDLOpenGLVersions }

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLVersions.SetGL_version_1_2(
  AValue: TSDLExtensionOption);
begin
  if FGL_version_1_2=AValue then Exit;
  FGL_version_1_2:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLVersions.SetGL_version_1_3(
  AValue: TSDLExtensionOption);
begin
  if FGL_version_1_3=AValue then Exit;
  FGL_version_1_3:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLVersions.SetGL_version_1_4(
  AValue: TSDLExtensionOption);
begin
  if FGL_version_1_4=AValue then Exit;
  FGL_version_1_4:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLVersions.SetGL_version_1_5(
  AValue: TSDLExtensionOption);
begin
  if FGL_version_1_5=AValue then Exit;
  FGL_version_1_5:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLVersions.SetGL_version_2_0(
  AValue: TSDLExtensionOption);
begin
  if FGL_version_2_0=AValue then Exit;
  FGL_version_2_0:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLVersions.SetGL_version_2_1(
  AValue: TSDLExtensionOption);
begin
  if FGL_version_2_1=AValue then Exit;
  FGL_version_2_1:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLVersions.SetGL_version_3_0(
  AValue: TSDLExtensionOption);
begin
  if FGL_version_3_0=AValue then Exit;
  FGL_version_3_0:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLVersions.SetGL_version_3_1(
  AValue: TSDLExtensionOption);
begin
  if FGL_version_3_1=AValue then Exit;
  FGL_version_3_1:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLVersions.SetGL_version_3_2(
  AValue: TSDLExtensionOption);
begin
  if FGL_version_3_2=AValue then Exit;
  FGL_version_3_2:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLVersions.SetGL_version_3_3(
  AValue: TSDLExtensionOption);
begin
  if FGL_VERSION_3_3=AValue then Exit;
  FGL_VERSION_3_3:=AValue;
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLVersions.SetGL_version_4_0(
  AValue: TSDLExtensionOption);
begin
  if FGL_VERSION_4_0=AValue then Exit;
  FGL_VERSION_4_0:=AValue;
end;

function TSDLCustomOpenGLExtensions.TSDLOpenGLVersions.DefaultOption: String;
begin
  Result := 'extOptional'
end;

procedure TSDLCustomOpenGLExtensions.TSDLOpenGLVersions.Load;
begin
  //Validate('GL_version_1_2', @Load_GL_version_1_2, GL_version_1_2);
  //Validate('GL_version_1_3', @Load_GL_version_1_3, GL_version_1_3);
  //Validate('GL_version_1_4', @Load_GL_version_1_4, GL_version_1_4);
  //Validate('GL_version_1_5', @Load_GL_version_1_5, GL_version_1_5);
  //Validate('GL_version_2_0', @Load_GL_version_2_0, GL_version_2_0);
  //Validate('GL_version_2_1', @Load_GL_version_2_1, GL_version_2_1);
  //Validate('GL_version_3_0', @Load_GL_version_3_0, GL_version_3_0);
  //Validate('GL_version_3_1', @Load_GL_version_3_1, GL_version_3_1);
  //Validate('GL_version_3_2', @Load_GL_version_3_2, GL_version_3_2);
  //Validate('GL_VERSION_3_3', @Load_GL_VERSION_3_3, GL_VERSION_3_3);
  //Validate('GL_VERSION_4_0', @Load_GL_VERSION_4_0, GL_VERSION_4_0);
end;

end.
