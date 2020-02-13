RUNNING_NIXOS=$(nixos-version)
NVIDIA_VERSION=$(nvidia-smi | grep -ho "Driver Version: [0-9]*.[0-9]*" |  awk '{print $NF}')

if [ ! -z $RUNNING_NIXOS]; then
   echo "nixos"
elif [ -z $NVIDIA_VERSION ]; then
   echo "intel"
else
   echo "nvidia" "$NVIDIA_VERSION" $(nix-prefetch-url http://download.nvidia.com/XFree86/Linux-x86_64/${NVIDIA_VERSION}/NVIDIA-Linux-x86_64-${NVIDIA_VERSION}.run | tail -1)
fi
