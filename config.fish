function fish_prompt
        echo -n "mackenzie:"
        set_color $fish_color_cwd
        echo -n (prompt_pwd)
        set_color normal
        echo -n "> "
    end

function eclimd
        cd ~/eclipse
        ./eclimd
    end

function eclipse
        cd ~/eclipse
        ./eclipse
    end 

function pcm
        pcmanfm
    end

function my_key_bindings
    fish_vi_key_bindings
    bind -M insert -m default kj force-repaint
end

function lisp
    sbcl --script $argv
end

function driveSync
    cd ~/Drive
    grive
end

set -g fish_key_bindings my_key_bindings

set PATH $HOME/.rbenv/bin $PATH
set PATH $HOME/.rbenv/shims $PATH
rbenv rehash >/dev/null ^&1
