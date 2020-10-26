
function my_key_bindings
    fish_vi_key_bindings
    bind -M insert -m default kj force-repaint
end

source ~/.fish_aliases

# Shut off fish greeting because it's annoying
set fish_greeting

set NVM_DIR $HOME/.nvm
set PATH /Users/mbligh/.nvm/versions/node/v10.13.0/bin $PATH

# Fixes all the cli utils (like ssh) not showing up in emacs
set PATH /usr/bin $PATH
set PATH /usr/local/opt/ruby/bin $PATH
set PATH /usr/local/bin $PATH
set PATH /usr/local/sbin $PATH


set -x -g ANT_OPTS '-Dspring.profiles.active=default,local'
set -x -g SPRING_PROFILES_ACTIVE 'default,local,dev'

set -x EDITOR "open -a Emacs"
set -x VISUAL $EDITOR

set -g fish_key_bindings my_key_bindings
set -g nvm_dir

set -g theme_color_scheme dracula

# Required config for vterm in Emacs
function vterm_printf;
    if [ -n "$TMUX" ]
        # tell tmux to pass the escape sequences through
        # (Source: http://permalink.gmane.org/gmane.comp.terminal-emulators.tmux.user/1324)
        printf "\ePtmux;\e\e]%s\007\e\\" "$argv"
    else if string match -q -- "screen*" "$TERM"
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$argv"
    else
        printf "\e]%s\e\\" "$argv"
    end
end

# Fixes IDEA terminal
/usr/local/bin/starship init fish | source

# For Jenv
export PATH="$HOME/.jenv/bin:$PATH"
