# Hack to fix error:
#    > libEGL warning: DRI2: failed to open swrast (search paths /run/opengl-driver/lib/dri)

if [ ! -d /run/opengl-driver ]; then
    sudo mkdir -p /run
    sudo ln -s /nix/store/*-mesa-noglu-*-drivers /run/opengl-driver
fi

# TODO: Embed this into shell.nix or stack post-build hook
# TODO: In case of more than one `/nix/store/*-mesa-noglu-drivers*`, symlink the correct one.
