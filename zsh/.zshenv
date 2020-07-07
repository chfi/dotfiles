# setting user env variables


# export PKG_CONFIG_PATH=/usr/local/lib/pkgconfig:/usr/local/lib:"$NIX_LINK/lib/pkgconfig"

export PATH="$HOME/.local/bin:$PATH"
export PATH="$HOME/.npm/bin:$PATH"
export PATH="$HOME/.cargo/bin:$PATH"

# export PATH="/home/christian/.config/guix/current/bin${PATH:+:}$PATH"
GUIX_PROFILE="$HOME/.guix-profile"
. "$GUIX_PROFILE/etc/profile"

NIX_PROFILE="$HOME/.nix-profile"
. "$NIX_PROFILE/etc/profile.d/nix.sh"

# guix paths
export GUIX_LOCPATH="$HOME/.guix-profile/lib/locale"
export SSL_CERT_DIR="$HOME/.guix-profile/etc/ssl/certs"
export SSL_CERT_FILE="$HOME/.guix-profile/etc/ssl/certs/ca-certificates.crt"
# export GIT_SSL_CAINFO="$SSL_CERT_FILE"

# Use emacs for editing everything, with emacs daemon
export EDITOR="emacsclient -t" # open in terminal
export VISUAL="emacsclient -c -a emacs" # open a new emacs instance if there isn't one

if [ -d "$HOME/adb-fastboot/platform-tools" ] ; then
 export PATH="$HOME/adb-fastboot/platform-tools:$PATH"
fi

# OS specific zshenvs
case "$(uname)" in
  ("Darwin")
    export PATH="/usr/local/bin:$PATH"
    export PATH="/usr/local/sbin:$PATH"
    source ~/.zshenv.osx
    ;;
  # ("Linux")
  #   source ~/.zshenv.linux
  #   ;;
esac

case "$(hostname)" in
  ("jupiter-nix")
    source ~/.zshenv.nixos
    ;;
esac
