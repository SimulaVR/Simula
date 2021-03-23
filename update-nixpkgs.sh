if (( $# != 1 )); then
    echo "Usage: ./update-nixpkgs.sh <channel name>"
    echo "Examples:"
    echo "  ./update-nixpkgs.sh nixos-19.09"
    echo "  ./update-nixpkgs.sh nixpkgs-unstable"
    exit
fi
CHANNEL=$1
echo "Updating pinned nixpkgs to most recent ${CHANNEL}"

SHA256=$(sha256sum ./nixpkgs-version.json)
nix-prefetch-git https://github.com/KaneTW/nixpkgs refs/heads/$CHANNEL \
    2>/dev/null > ./nixpkgs-version.json
NEWSHA256=$(sha256sum ./nixpkgs-version.json)

if [ "$SHA256" = "$NEWSHA256" ]
then
    echo "Done, but the contents of nixpkgs-version.json is the same."
else
    echo "Done. File nixpkgs-version.json was updated."
fi
