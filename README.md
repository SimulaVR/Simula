# SimulaHS
Step 1: Build simula_cpp with ./build.sh qtwayland libmotorcar-compositor
Step 2: Build SimulaHS with LD_LIBRARY_PATH=$PWD/simula_cpp/lib stack build
Step 3: Run the simple compositor with ./run-simple-compositor.sh
