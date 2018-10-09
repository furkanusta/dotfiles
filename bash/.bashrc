# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

# System wide functions and aliases
# Environment stuff goes in /etc/profile
shopt -s histappend


if [ "$TERM" = dumb ] && [ "$INSIDE_EMACS" ]; then
    alias ls='TERM=dumb-emacs-ansi COLORTERM=1 ls --color=auto'
fi

export PATH="$PATH:$HOME/.npm-packages/bin/:$HOME/.cargo/bin/"

export ALTERNATE_EDITOR=""
export EDITOR='emacsclient -nw'

alias pip3-install="pip3 install --user"
alias gem-install="gem install --user"

export HISTFILESIZE=
export HISTSIZE=

alias mp3-dl="youtube-dl --extract-audio --audio-format=mp3"
alias clang++="clang++ -std=c++17 -stdlib=libc++"
alias trash=trash-put
export FZF_DEFAULT_COMMAND='fd --type f'
export FZF_DEFAULT_OPTS="--preview 'head {}'"

alias share="gotty tmux new -A"

alias bc="bc -l -q ~/Documents/functions.bc"

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

# keycode 108 = Return
