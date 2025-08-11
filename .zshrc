export PROMPT=" %F{blue}%~%f ---> %F{red}thyruh%f: "

# Aliases
alias C++='cd ~/personal/Architect/C++ && vim .'
alias py='cd ~/personal/Architect/Python && vim .'
alias hsk='cd ~/personal/Architect/Haskell && vim .'
alias vg='cd ~/personal/Architect/Go && vim .'
alias cls='clear && cd'
alias ..='cd ..'
alias ...='cd ../..'
alias 2.='cd ../../..'
alias 3.='cd ../../../..'
alias arc='cd ~/personal/Architect && vim .'
alias sn='sudo shutdown now'
alias sr='sudo reboot'
alias inst='python3 ~/scripts/mc/instanceCFG.py'
alias :q='vim .'
alias nautilus='nautilus . &'
alias dwbackup='mv ~/Downloads/* ~/dnBackup/ && echo "Done"'
alias g++='g++ -std=c++23'
alias gcc='gcc -std=c23'

# Add paths to $PATH
export GOROOT=/usr/local/go
export GOPATH=$HOME/go
export PATH=$PATH:$GOROOT/bin:$GOPATH/bin
export PATH="/usr/local/bin:$PATH"
export PATH="$PATH:/usr/local/go/bin"
export PATH="$PATH:$HOME/go/bin"
export PATH="/usr/local/go/bin:$PATH"
export PATH="~/ded"
export PATH="/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/home/thyruh/.local/bin:~/ded/ded:/home/thyruh/.local/opt/go/bin"

# Run startup commands
tmux a
tmux
clear

# Generated for envman. Do not edit.
[ -s "$HOME/.config/envman/load.sh" ] && source "$HOME/.config/envman/load.sh"
export PATH=$PATH:$HOME/.local/opt/go/bin

# Set keyboard layout options 
#setxkbmap -option caps:swapescape

set -o vi  # Vim mode
setxkbmap -option caps:escape
#setxkbmap -layout us,ru -variant dvorak-qwerty, -option grp:alt_shift_toggle
setxkbmap -layout us,ru -option grp:alt_shift_toggle


