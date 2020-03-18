RUNNING_NIXOS=$(nixos-version > /dev/null 2>&1)
NVIDIA_VERSION=$(nvidia-smi | grep -ho "Driver Version: [0-9]*.[0-9]*" |  awk '{print $NF}')

getNvidiaHash() {
    case $1 in
        430.26)
          echo 1rnfxl4dxa3jjidfdvfjmg1a8nc787ss15cakrp2wwrn8jlr9av6
          ;;
        430.34)
          echo 0c3x25gilibbgazvp20d5sfmmgcf0gfqf024nzzqryxg4m05h39b
          ;;
        430.40)
          echo 1myzhy1mf27dcx0admm3pbbkfdd9p66lw0cq2mz1nwds92gqj07p
          ;;
        430.50)
          echo 1i9x9lr6izfq6csgnh8dfg3sa7s3has20hdpi7wlbla7msa36s0c
          ;;
        430.64)
          echo 1k5s05a7yvrms97nr3gd8cbvladn788qamsmwr6jsmdzv6yh5gvk
          ;;
        435.17)
          echo 19p9v5km1kfw45ghqmzgawa2fdq559jj6h1dkbnkbbzhp2syq757
          ;;
        435.21)
          echo 0v3pq677ab01qdmwl5dawk8hn39qlwj05p8s9qzh9irmrlnc1izs
          ;;
        440.26)
          echo 0ay3c4vhl8cqhl57vjar4p6v1nkh5zpvya41ag2sibj30spyg62z
          ;;
        440.31)
          echo 03w5v3079c35sz3nkdk28yc76jb5hv8dy99jjy7pkywvbhw2ynfd
          ;;
        440.36)
          echo 0nbdldwizb802w4x0rqnyb1p7iqz5nqiahqr534n5ihz21a6422h
          ;;
        440.44)
          echo   057wq9p2vl87gy61f079b6d7clw2vhw3kq7rj411brhrnvr7shmd
          ;;
        440.59)
          echo 162gq6w44l8sgnn4qnl2rdlx8c008p04zv4c3i1ps20p21n1mjv1
          ;;
        440.64)
          echo 0xbm1dh95kz8h4d62pql2wmvw2gbgc7iif2bkixbnqijl4dryg71
          ;;
        *)
          echo $(nix-prefetch-url http://download.nvidia.com/XFree86/Linux-x86_64/${NVIDIA_VERSION}/NVIDIA-Linux-x86_64-${NVIDIA_VERSION}.run | tail -1)
          ;;
    esac
}

if [ ! -z "$RUNNING_NIXOS" ]; then
   echo "nixos"
elif [ -z $NVIDIA_VERSION ]; then
   echo "intel"
else
   echo "nvidia" "$NVIDIA_VERSION" $(getNvidiaHash "$NVIDIA_VERSION")
fi
