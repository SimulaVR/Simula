import gdb

class ImageCommand(gdb.Command):
    def __init__(self):
        # This registers our class as "simple_command"
        super(ImageCommand, self).__init__("save_gl_image", gdb.COMMAND_DATA)

    def invoke(self, arg, from_tty):
        stride = str(gdb.selected_frame().read_var("stride"))
        width = str(gdb.selected_frame().read_var("width"))
        height = str(gdb.selected_frame().read_var("height"))
        src_x = str(gdb.selected_frame().read_var("src_x"))
        src_y = str(gdb.selected_frame().read_var("src_y"))
        dst_x = str(gdb.selected_frame().read_var("dst_x"))
        dst_y = str(gdb.selected_frame().read_var("dst_y"))
        data = str(gdb.selected_frame().read_var("data"))

        function_name = str(gdb.selected_frame().name())
        file_name = "gl.bin/" + function_name + "." + stride + "." + width + "." + height + "." + src_x + "." + src_y + "." + dst_x + "." + dst_y + "." + data + ".bin"
        gdb.execute("dump binary memory " + file_name + " data (data + stride * height)")


# This registers our class to the gdb runtime at "source" time.
ImageCommand()