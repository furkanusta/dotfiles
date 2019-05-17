# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

# System wide functions and aliases
# Environment stuff goes in /etc/profile
export HISTCONTROL=ignoredups:erasedups
export HISTSIZE=100000                   # big big history
export HISTFILESIZE=100000               # big big history
shopt -s histappend
#export PROMPT_COMMAND="${PROMPT_COMMAND:+$PROMPT_COMMAND$'\n'}history -a; history -c; history -r"

if [ "$TERM" = dumb ] && [ "$INSIDE_EMACS" ]; then
    alias ls='TERM=dumb-emacs-ansi COLORTERM=1 ls --color=auto'
fi

export LD_LIBRARY_PATH="$LD_LIBRARY_PATH:/usr/local/lib:/usr/local/lib64/"
export ALTERNATE_EDITOR=""
export EDITOR='emacsclient -nw'

alias pip3-install="pip3 install --user"
alias gem-install="gem install --user"

alias ec="emacsclient -t"
alias mp3-dl="youtube-dl --extract-audio --audio-format=mp3"
alias clang++="clang++ -std=c++17 -stdlib=libc++"
#alias trash=trash-put
export FZF_DEFAULT_COMMAND='fd --type f'
export FZF_DEFAULT_OPTS="--preview 'head {}'"


fzf() {
  local out file key
  IFS=$'\n' out=$(fzf-tmux --query="$1" --exit-0 --expect=ctrl-e)
  key=$(head -1 <<< "$out")
  file=$(head -2 <<< "$out" | tail -1)
  if [ -n "$file" ]; then
    [ "$key" = ctrl-e ] && emacsclient -nw "$file"
  fi
  echo "$file";
}
