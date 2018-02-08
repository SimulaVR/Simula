# Hack to fix error:
#    > libEGL warning: DRI2: failed to open swrast (search paths /run/opengl-driver/lib/dri)

# Fix was found here https://github.com/NixOS/nixpkgs/issues/9415#issuecomment-336494579

if [ ! -d /run/opengl-driver ]; then
    sudo nix-build -A 'mesa_noglu.drivers' $HOME/.nix-defexpr/channels/nixpkgs -o /run/opengl && \
        sudo mv /run/opengl-drivers /run/opengl-driver
fi

# TODO: Embed this into shell.nix or stack post-build hook
