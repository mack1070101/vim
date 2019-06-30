function my_key_bindings
    fish_vi_key_bindings
    bind -M insert -m default kj force-repaint
end

function fish_title
  true
end

source ~/.fish_aliases

set NVM_DIR $HOME/.nvm

set -x -g ANT_OPTS '-Dspring.profiles.active=default,local'
set -x -g SPRING_PROFILES_ACTIVE 'default,local'

set -g fish_key_bindings my_key_bindings
set -g nvm_dir

set -g theme_nerd_fonts yes
set -g theme_color_scheme dracula
