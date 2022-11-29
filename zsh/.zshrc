# executed when zsh is started as an interactive shell

# use antigen
# source ~/.nix-profile/share/antigen/antigen.zsh
# source /usr/share/zsh-antigen/antigen.zsh
source ~/.local/antigen-zsh/antigen.zsh

antigen use oh-my-zsh

antigen bundle git
antigen bundle history
# antigen bundle git@github.com:spwhitt/nix-zsh-completions.git
antigen bundle vi-mode

antigen theme gallois

antigen apply


emacs-command-line () {
    local VISUAL="emacsclient -t"
    edit-command-line
}

zle -N emacs-command-line
bindkey -M vicmd v emacs-command-line

bindkey "\eOH" beginning-of-line
bindkey "\eOF" end-of-line

# alias l='ls --color=yes -lah'
alias l='exa -l'
alias la='exa -la'
alias s='du -hd1'

alias imv='imv-wayland'

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/home/christian/miniconda3/bin/conda' 'shell.zsh' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/home/christian/miniconda3/etc/profile.d/conda.sh" ]; then
        . "/home/christian/miniconda3/etc/profile.d/conda.sh"
    else
        export PATH="/home/christian/miniconda3/bin:$PATH"
    fi
fi
unset __conda_setup
# <<< conda initialize <<<


export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
