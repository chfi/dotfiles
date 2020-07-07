# executed when zsh is run as a login shell

if [ -d "$HOME/adb-fastboot/platform-tools" ] ; then
 export PATH="$HOME/adb-fastboot/platform-tools:$PATH"
fi
