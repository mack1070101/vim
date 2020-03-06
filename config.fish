eval (starship init fish)

# this function may be required
# function my_key_bindings
#     fish_vi_key_bindings
#     bind -M insert -m default kj force-repaint
# end

source ~/.fish_aliases

set NVM_DIR $HOME/.nvm
set PATH /Users/mbligh/.nvm/versions/node/v10.13.0/bin $PATH

set -x -g ANT_OPTS '-Dspring.profiles.active=default,local'
set -x -g SPRING_PROFILES_ACTIVE 'default,local'

set -x EDITOR "open -a Emacs"
set -x VISUAL $EDITOR

#set -g fish_key_bindings my_key_bindings
set -g nvm_dir

set -g theme_color_scheme dracula
