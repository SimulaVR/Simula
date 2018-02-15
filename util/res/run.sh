#!/bin/bash
#
# This is a script which runs programs in the Steam runtime

# The top level of the runtime tree
TOP=$(cd "${0%/*}" && echo ${PWD})

# Make sure we have something to run
if [ "$1" = "" ]; then
    echo "Usage: $0 program [args]"
    exit 1
fi

# Note that we put the Steam runtime first
# If ldd on a program shows any library in the system path, then that program
# may not run in the Steam runtime.
export STEAM_RUNTIME="${TOP}"

host_library_paths=

if [ "${STEAM_RUNTIME_PREFER_HOST_LIBRARIES-}" != "0" ]; then
    while read -r line; do
        # If line starts with a leading / and contains :, it's a new path prefix
        if [[ "$line" =~ ^/.*: ]]
        then
            library_path_prefix=`echo $line | cut -d: -f1`

            host_library_paths=$host_library_paths$library_path_prefix:
        fi
    done <<< "$(/sbin/ldconfig -XNv 2> /dev/null)"

    host_library_paths="$STEAM_RUNTIME/pinned_libs_32:$STEAM_RUNTIME/pinned_libs_64:$host_library_paths"
fi

steam_runtime_library_paths="$host_library_paths$STEAM_RUNTIME/i386/lib/i386-linux-gnu:$STEAM_RUNTIME/i386/lib:$STEAM_RUNTIME/i386/usr/lib/i386-linux-gnu:$STEAM_RUNTIME/i386/usr/lib:$STEAM_RUNTIME/amd64/lib/x86_64-linux-gnu:$STEAM_RUNTIME/amd64/lib:$STEAM_RUNTIME/amd64/usr/lib/x86_64-linux-gnu:$STEAM_RUNTIME/amd64/usr/lib"

if [ "$1" = "--print-steam-runtime-library-paths" ]; then
    echo "$steam_runtime_library_paths"
    exit 0
fi

export LD_LIBRARY_PATH="$steam_runtime_library_paths:${LD_LIBRARY_PATH-}"

exec "$@"

# vi: ts=4 sw=4 expandtab
