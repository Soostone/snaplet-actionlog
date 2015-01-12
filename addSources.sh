set -e
cabal sandbox init

cabal sandbox add-source $SOOSTONE_CODE_DIR/snap-extras
cabal sandbox add-source $SOOSTONE_CODE_DIR/restful-snap
cabal sandbox add-source $SOOSTONE_CODE_DIR/snaplet-persistent
