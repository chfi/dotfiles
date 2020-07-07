# executed when zsh is started as an interactive shell

# use antigen
source ~/.nix-profile/share/antigen/antigen.zsh

antigen use oh-my-zsh

antigen bundle git
antigen bundle history
antigen bundle git@github.com:spwhitt/nix-zsh-completions.git
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
alias s='du -hd1'
