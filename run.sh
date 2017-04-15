if [[ $# -lt 1 ]]; then
  echo "Usage: $0 <app name>"
  echo "Example: $0 simple-compositor"
  exit 1
fi

ROOTDIR=$(dirname $0)
LD_LIBRARY_PATH=$ROOTDIR/simula_cpp/lib:$ROOTDIR/simula_cpp/dependencies/build/qt5/lib/ $ROOTDIR/.stack-work/install/x86_64-linux/lts-8.9/8.0.2/bin/$1
