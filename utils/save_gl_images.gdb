# Usage instructions:
#  1. Call `./result/bin/[module]_rr_replay -x ./utils/save_gl_images.gdb <rr_trace>` from Simula root
#  2. Call `importImageBinary /@ FileNames["*.bin", "/path/to/Simula/gl.bin"]` after importing `./utils/Helpers.wls` from Wolfram

set breakpoint pending on
set pagination off
# set logging file ./save_gl_image.log
set logging on
set logging overwrite on

# dir ./submodules/godot
# dir ./submodules/wlroots-dev

source ./utils/save_gl_image.py

# Clear gl files
shell rm -r ./gl.bin
shell mkdir -p gl.bin

# Simula breakpoints:

# fmt @ gles3_renderer.cpp:317
break WlrGLES3Renderer::texture_from_pixels
  commands 1
  save_gl_image
  continue
end

# fmt @ gles3_renderer.cpp:165
break WlrGLES3Texture::wlr_texture_write_pixels 
  commands 2
  save_gl_image
  continue
end

# rootston breakpoints:

# fmt @ texture.c:152
break wlr_gles2_texture_from_pixels
  commands 3
  save_gl_image
  continue
end

# fmt @ texture.c:61
break gles2_texture_write_pixels
  commands 4
  save_gl_image
  continue
end
