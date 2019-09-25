;; -*- mode: emacs-lisp; lexical-binding: t -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Layer configuration:
This function should only modify configuration layer settings."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs

   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused

   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t

   ;; If non-nil layers with lazy install support are lazy installed.
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()

   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(
     ;; Genral Utilities
     ibuffer
     treemacs
     themes-megapack
     spotify
     ;; Markup and text processing
     markdown
     (org :variables org-enable-github-support t)
     yaml
     html
     json
     ;; Utilities for writing code & prose
     ivy
     auto-completion
     spell-checking
     syntax-checking
     lsp
     git
     ;; Programming language layers
     shell-scripts
     (shell :variables
            shell-default-height 30
            shell-default-shell 'ansi-term
            shell-enable-smart-eshell t
            shell-default-position 'bottom)

     python
     emacs-lisp
     clojure
     kotlin
     restclient)

   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages '(parinfer oauth2 forge)
   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()

   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '()

   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and deletes any unused
   ;; packages as well as their unused dependencies. `used-but-keep-unused'
   ;; installs only the used packages but won't delete unused ones. `all'
   ;; installs *all* packages supported by Spacemacs and never uninstalls them.
   ;; (default is `used-only')
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization:
This function is called at the very beginning of Spacemacs startup,
before layer configuration.
It should only modify the values of Spacemacs settings."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.

  ;; Fix missing ELPA Bug from startup
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
  (setq-default

   ;; If non-nil then enable support for the portable dumper. You'll need
   ;; to compile Emacs 27 from source following the instructions in file
   ;; EXPERIMENTAL.org at to root of the git repository.
   ;; (default nil)
   dotspacemacs-enable-emacs-pdumper nil

   ;; Name of executable file pointing to emacs 27+. This executable must be
   ;; in your PATH.
   ;; (default "emacs")
   dotspacemacs-emacs-pdumper-executable-file "emacs"

   ;; Name of the Spacemacs dump file. This is the file will be created by the
   ;; portable dumper in the cache directory under dumps sub-directory.
   ;; To load it when starting Emacs add the parameter `--dump-file'
   ;; when invoking Emacs 27.1 executable on the command line, for instance:
   ;;   ./emacs --dump-file=~/.emacs.d/.cache/dumps/spacemacs.pdmp
   ;; (default spacemacs.pdmp)
   dotspacemacs-emacs-dumper-dump-file "spacemacs.pdmp"

   ;; If non-nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t

   ;; Maximum allowed time in seconds to contact an ELPA repository.
   ;; (default 5)
   dotspacemacs-elpa-timeout 5

   ;; Set `gc-cons-threshold' and `gc-cons-percentage' when startup finishes.
   ;; This is an advanced option and should not be changed unless you suspect
   ;; performance issues due to garbage collection operations.
   ;; (default '(100000000 0.1))
   dotspacemacs-gc-cons '(100000000 0.1)

   ;; If non-nil then Spacelpa repository is the primary source to install
   ;; a locked version of packages. If nil then Spacemacs will install the
   ;; latest version of packages from MELPA. (default nil)
   dotspacemacs-use-spacelpa nil

   ;; If non-nil then verify the signature for downloaded Spacelpa archives.
   ;; (default nil)
   dotspacemacs-verify-spacelpa-archives nil

   ;; If non-nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil

   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'. (default 'emacs-version)
   dotspacemacs-elpa-subdirectory 'emacs-version

   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'hybrid

   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'random

   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'.
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((todos . 5)
                                (agenda . 5)
                                (recents . 5)
                                (projects . 2))

   ;; True if the home buffer should respond to resize events. (default t)
   dotspacemacs-startup-buffer-responsive t

   ;; Default major mode for a new empty buffer. Possible values are mode
   ;; names such as `text-mode'; and `nil' to use Fundamental mode.
   ;; (default `text-mode')
   dotspacemacs-new-empty-buffer-major-mode 'text-mode

   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'org-mode

   ;; Initial message in the scratch buffer, such as "Welcome to Spacemacs!"
   ;; (default nil)
   dotspacemacs-initial-scratch-message nil

   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press `SPC T n' to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(dracula
                         spacemacs-dark
                         spacemacs-light)

   ;; Set the theme for the Spaceline. Supported themes are `spacemacs',
   ;; `all-the-icons', `custom', `doom', `vim-powerline' and `vanilla'. The
   ;; first three are spaceline themes. `doom' is the doom-emacs mode-line.
   ;; `vanilla' is default Emacs mode-line. `custom' is a user defined themes,
   ;; refer to the DOCUMENTATION.org for more info on how to create your own
   ;; spaceline theme. Value can be a symbol or list with additional properties.
   ;; (default '(spacemacs :separator wave :separator-scale 1.5))
   dotspacemacs-mode-line-theme '(spacemacs :separator rounded :separator-scale 1.5)

   ;; If non-nil the cursor color matches the state color in GUI Emacs.
   ;; (default t)
   dotspacemacs-colorize-cursor-according-to-state t

   ;; Default font or prioritized list of fonts.
   dotspacemacs-default-font '("Menlo for Powerline"
                               :size 16.0
                               :weight normal
                               :width normal)

   ;; The leader key (default "SPC")
   dotspacemacs-leader-key "SPC"

   ;; The key used for Emacs commands `M-x' (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"

   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"

   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"

   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","

   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m")
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"

   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs `C-i', `TAB' and `C-m', `RET'.
   ;; Setting it to a non-nil value, allows for separate commands under `C-i'
   ;; and TAB or `C-m' and `RET'.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil

   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"

   ;; If non-nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil

   ;; If non-nil then the last auto saved layouts are resumed automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil

   ;; If non-nil, auto-generate layout name when creating new layouts. Only has
   ;; effect when using the "jump to layout by number" commands. (default nil)
   dotspacemacs-auto-generate-layout-names nil

   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1

   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache

   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5

   ;; If non-nil, the paste transient-state is enabled. While enabled, after you
   ;; paste something, pressing `C-j' and `C-k' several times cycles through the
   ;; elements in the `kill-ring'. (default nil)
   dotspacemacs-enable-paste-transient-state nil

   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.2

   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom

   ;; Control where `switch-to-buffer' displays the buffer. If nil,
   ;; `switch-to-buffer' displays the buffer in the current window even if
   ;; another same-purpose window is available. If non-nil, `switch-to-buffer'
   ;; displays the buffer in a same-purpose window even if the buffer can be
   ;; displayed in the current window. (default nil)
   dotspacemacs-switch-to-buffer-prefers-purpose nil

   ;; If non-nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t

   ;; If non-nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil

   ;; If non-nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil

   ;; If non-nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup 't

   ;; If non-nil the frame is undecorated when Emacs starts up. Combine this
   ;; variable with `dotspacemacs-maximized-at-startup' in OSX to obtain
   ;; borderless fullscreen. (default nil)
   dotspacemacs-undecorated-at-startup nil

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90

   ;; If non-nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t

   ;; If non-nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t

   ;; If non-nil unicode symbols are displayed in the mode line.
   ;; If you use Emacs as a daemon and wants unicode characters only in GUI set
   ;; the value to quoted `display-graphic-p'. (default t)
   dotspacemacs-mode-line-unicode-symbols t

   ;; If non-nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t

   ;; Control line numbers activation.
   ;; If set to `t', `relative' or `visual' then line numbers are enabled in all
   ;; `prog-mode' and `text-mode' derivatives. If set to `relative', line
   ;; numbers are relative. If set to `visual', line numbers are also relative,
   ;; but lines are only visual lines are counted. For example, folded lines
   ;; will not be counted and wrapped lines are counted as multiple lines.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :visual nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; When used in a plist, `visual' takes precedence over `relative'.
   ;; (default nil)
   dotspacemacs-line-numbers 'visual

   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil

   ;; If non-nil `smartparens-strict-mode' will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil

   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc...
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil

   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all

   ;; If non-nil, start an Emacs server if one is not already running.
   ;; (default nil)
   dotspacemacs-enable-server nil

   ;; Set the emacs server socket location.
   ;; If nil, uses whatever the Emacs default is, otherwise a directory path
   ;; like \"~/.emacs.d/server\". It has no effect if
   ;; `dotspacemacs-enable-server' is nil.
   ;; (default nil)
   dotspacemacs-server-socket-dir nil

   ;; If non-nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil

   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `rg', `ag', `pt', `ack' and `grep'.
   ;; (default '("rg" "ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")

   ;; Format specification for setting the frame title.
   ;; %a - the `abbreviated-file-name', or `buffer-name'
   ;; %t - `projectile-project-name'
   ;; %I - `invocation-name'
   ;; %S - `system-name'
   ;; %U - contents of $USER
   ;; %b - buffer name
   ;; %f - visited file name
   ;; %F - frame name
   ;; %s - process status
   ;; %p - percent of buffer above top of window, or Top, Bot or All
   ;; %P - percent of buffer above bottom of window, perhaps plus Top, or Bot or All
   ;; %m - mode name
   ;; %n - Narrow if appropriate
   ;; %z - mnemonics of buffer, terminal, and keyboard coding systems
   ;; %Z - like %z, but including the end-of-line format
   ;; (default "%I@%S")
   dotspacemacs-frame-title-format "%I@%S"

   ;; Format specification for setting the icon title format
   ;; (default nil - same as frame-title-format)
   dotspacemacs-icon-title-format nil

   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed' to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup 'trailing

   ;; Either nil or a number of seconds. If non-nil zone out after the specified
   ;; number of seconds. (default nil)
   dotspacemacs-zone-out-when-idle nil

   ;; Run `spacemacs/prettify-org-buffer' when
   ;; visiting README.org files of Spacemacs.
   ;; (default nil)
   dotspacemacs-pretty-docs nil))

(defun dotspacemacs/user-env ()
  "Environment variables setup.
This function defines the environment variables for your Emacs session. By
default it calls `spacemacs/load-spacemacs-env' which loads the environment
variables declared in `~/.spacemacs.env' or `~/.spacemacs.d/.spacemacs.env'.
See the header of this file for more information."
  (spacemacs/load-spacemacs-env))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init', before layer configuration
executes.
 This function is mostly useful for variables that need to be set
before packages are loaded. If you are unsure, you should try in setting them in
`dotspacemacs/user-config' first."
  ;; Configure Clojure fancy symbols
  (setq clojure-enable-fancify-symbols t))

(defun dotspacemacs/user-load ()
  "Library to load while dumping.
This function is called only while dumping Spacemacs configuration. You can
`require' or `load' the libraries of your choice that will be included in the
dump.")

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration.
This is the place where most of your configurations should be done. Unless it is
explicitly specified that a variable should be set before a package is loaded,
you should place your code here.
TODO break nested defuns out"

  ;; GENERAL UTILITIES
  (setq company-idle-delay 0)
  (setq fringe-mode 'no-fringes)

  ;; Config terminal
  (add-hook 'term-mode-hook 'toggle-truncate-lines)
  (add-hook 'kill-emacs-hook 'mb/kill-emacs-hook)

  ;; Spaceline config
  (setq spaceline-purpose-p nil)
  (setq spaceline-minor-modes-p nil)
  (setq spaceline-org-clock-p t)

  ;; Custom layouts
  (spacemacs|define-custom-layout "@DUNLOP"
    :binding "d"
    :body
    (progn
      (magit-status "~/code/dunlop/")))

  (spacemacs|define-custom-layout "@CODE"
    :binding "c"
    :body
    (progn
      (dired-at-point "~/code/")))

  ;; Rebind avy goto char to match Intellij
  (global-set-key "j" (quote avy-goto-char))

  ;; ORG MODE STUFF
  ;; Org text display config
  (add-hook 'text-mode-hook #'visual-line-mode)
  (add-hook 'org-mode-hook 'auto-fill-mode) ;; Wrap long lines
  (setq org-pretty-entities 't)

  ;; Org key bindings
  (spacemacs/set-leader-keys-for-major-mode 'org-mode "I" 'org-clock-in)
  (spacemacs/set-leader-keys-for-major-mode 'org-mode "O" 'org-clock-out)
  (spacemacs/set-leader-keys-for-major-mode 'org-mode "sp" 'mb/org-narrow-to-parent)
  ;; Toggle TODO states in normal mode with the "t" key
  (evil-define-key 'normal org-mode-map "t" 'org-todo)
  ;; Fix missing <s TAB shortcut
  (require 'org-tempo)
  (setenv "PATH" (concat "/Library/TeX/texbin" (getenv "PATH")))
  (setq exec-path (append '("/Library/TeX/texbin") exec-path))

  ;; Sets custom TODO states
  (setq org-todo-keywords '((sequence "TODO"
                                      "IN-PROGRESS" "|"
                                      "DONE"
                                      "WILL-NOT-IMPLEMENT")))

  ;; Org agenda config
  (setq org-agenda-start-with-follow-mode 't)
  (setq org-agenda-files (list "~/Org/Turo/TuroWorkLog.org"
                               "~/Org/Personal/PersonalTODO.org"
                               "~/Org/Turo/TuroVisa.org"
                               "~/Org/Personal/Wedding.org"))

  ;; Org capture and reflile config
  (setq org-refile-targets '((org-agenda-files :maxlevel . 3)))
  (setq org-capture-templates
        '(("t" "TODO" entry (file+headline "~/Org/Inbox.org" "Tasks")
           "* TODO  %?\n\t%U\n  %i\n" :prepend t)
          ("T" "TODO Ticket" entry (file+headline "~/Org/Inbox.org" "Tasks")
           "* TODO  %?\n\t%U\n** Checklist:[0/1]\n\t- [ ] Self review  %i\n" :prepend t :jump-to-captured t)
          ("s" "Sprint Check In" entry (file+olp"~/Org/Turo/TuroWorkLog.org" "Meetings" "Sprint Meetings")
           "* %t Sprint Meeting%?\n** iOS:\n\n** Android:\n\n** Web:\n\n** Backend:\n\n** Product & Design: %i\n"
           :clock-in t :jump-to-captured t)
          ("S" "Sprint Planning" entry (file+olp"~/Org/Turo/TuroWorkLog.org" "Meetings" "Sprint Meetings")
           "* %t Sprint Planning%?\n** Previous Sprint\n*** iOS:\n\n*** Android:\n\n*** Web:\n\n*** Backend:\n** Next Sprint: %i\n"
           :clock-in t :jump-to-captured t)))

  ;; Org babel/programming config
  (with-eval-after-load
      (org-babel-do-load-languages 'org-babel-load-languages '((java . t)
                                                               (shell . t)
                                                               (restclient . t))))

  ;; Clojure in orgmode stuff
  (require 'org)
  (require 'ob-clojure)
  (setq org-babel-clojure-backend 'cider)
  (require 'cider)

  ;; MAGIT STUFF
  ;; magit performance tweaks
  (setq magit-refresh-status-buffer nil)
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  (setq magit-status-buffer-switch-function 'switch-to-buffer)
  (setq vc-handled-backends nil) ;Turn off emacs native version control because I only use magit

  ;; Add commands to magit menus
  (transient-append-suffix 'magit-commit "c" '("p" "pull-request" mb/generate-turo-pr-message))
  (transient-append-suffix 'magit-branch "l" '("-" "Checkout last branch" mb/checkout-last-branch))
  (transient-append-suffix 'magit-branch "-" '("M" "Checkout master" mb/checkout-master))
  (transient-insert-suffix 'magit-pull "-r" '("-f" "Overwrite local branch" "--force"))

  ;; Add commit message generation
  (add-hook 'git-commit-setup-hook 'mb/generate-git-commit-msg)

  ;; CLOJURE STUFF
  ;; Set configs for parinfer
  (setq parinfer-extensions
        '(pretty-parens  ; different paren styles for different modes.
          evil           ; If you use Evil.
          smart-tab))      ; C-b & C-f jump positions and smart shift with tab & S-tab.
  (add-hook 'clojure-mode-hook #'parinfer-mode)

  ;; Lisp config
  (add-hook 'emacs-lisp-mode-hook #'parinfer-mode)
  (add-hook 'common-lisp-mode-hook #'parinfer-mode)
  (add-hook 'scheme-mode-hook #'parinfer-mode)
  (add-hook 'lisp-mode-hook #'parinfer-mode)

  ;; PYTHON STUFF
  (add-hook 'python-mode-hook 'anaconda-mode)

  ;; IBUFFER Stuff
  ;; Rebind to projectile-ibuffer for workspace isolation
  (global-set-key "p" (quote projectile-ibuffer))
  ;; Use human readable Size column instead of original one
  (define-ibuffer-column size-h
    (:name "Size")
    (cond
     ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
     ((> (buffer-size) 100000) (format "%7.0fk" (/ (buffer-size) 1000.0)))
     ((> (buffer-size) 1000) (format "%7.1fk" (/ (buffer-size) 1000.0)))
     (t (format "%8d" (buffer-size)))))
  ;; Modify the default ibuffer-formats
  (setq ibuffer-formats
        '((mark modified read-only " "
                (name 18 18 :left :elide)
                " "
                (size-h 9 -1 :right)
                " "
                (mode 16 16 :left :elide)
                " "
                filename-and-process))))

;; This is mostly a copy and pasted version of `org-babel-execute-src-block` but
;; it extracts the language parameter from being defined within the function
;; to a mandatory argument that needs to be passed in.
(defun org-babel-execute-src-block-with-lang (lang &optional arg info params)
  "Execute the current source code block by specifying the))))
language the block should be executed with.
Insert the results of execution into the buffer.  Source code
execution and the collection and formatting of results can be
controlled through a variety of header arguments.

With prefix argument ARG, force re-execution even if an existing
result cached in the buffer would otherwise have been returned.

Optionally supply a value for INFO in the form returned by
`org-babel-get-src-block-info'.

Optionally supply a value for PARAMS which will be merged with
the header arguments specified at the front of the source code
block."
  (let* ((org-babel-current-src-block-location
          (or org-babel-current-src-block-location
              (nth 5 info)
              (org-babel-where-is-src-block-head)))
         (info (if info (copy-tree info) (org-babel-get-src-block-info))))
    ;; Merge PARAMS with INFO before considering source block
    ;; evaluation since both could disagree.
    (cl-callf org-babel-merge-params (nth 2 info) params)
    (when (org-babel-check-evaluate info)
      (cl-callf org-babel-process-params (nth 2 info))
      (let* ((params (nth 2 info))
             (cache (let ((c (cdr (assq :cache params))))
                      (and (not arg) c (string= "yes" c))))
             (new-hash (and cache (org-babel-sha1-hash info :eval)))
             (old-hash (and cache (org-babel-current-result-hash)))
             (current-cache (and new-hash (equal new-hash old-hash))))
        (cond
         (current-cache
          (save-excursion    ;Return cached result.
            (goto-char (org-babel-where-is-src-block-result nil info))
            (forward-line)
            (skip-chars-forward " \t")
            (let ((result (org-babel-read-result)))
              (message (replace-regexp-in-string "%" "%%" (format "%S" result)))
              result)))
         ((org-babel-confirm-evaluate info)
          (let* ((result-params (cdr (assq :result-params params)))
                 ;; Expand noweb references in BODY and remove any
                 ;; coderef.
                 (body
                  (let ((coderef (nth 6 info))
                        (expand
                         (if (org-babel-noweb-p params :eval)
                             (org-babel-expand-noweb-references info)
                           (nth 1 info))))
                    (if (not coderef) expand
                      (replace-regexp-in-string
                       (org-src-coderef-regexp coderef) "" expand nil nil 1))))
                 (dir (cdr (assq :dir params)))
                 (default-directory
                   (or (and dir (file-name-as-directory (expand-file-name dir)))
                       default-directory))
                 (cmd (intern (concat "org-babel-execute:" lang)))
                 result)
            (unless (fboundp cmd)
              (error "No org-babel-execute function for %s!" lang))
            (message "executing %s code block%s..."
                     (capitalize lang)
                     (let ((name (nth 4 info)))
                       (if name (format " (%s)" name) "")))
            (if (member "none" result-params)
                (progn (funcall cmd body params)
                       (message "result silenced"))
              (setq result
                    (let ((r (funcall cmd body params)))
                      (if (and (eq (cdr (assq :result-type params)) 'value)
                               (or (member "vector" result-params)
                                   (member "table" result-params))
                               (not (listp r)))
                          (list (list r))
                        r)))
              (let ((file (cdr (assq :file params))))
                ;; If non-empty result and :file then write to :file.
                (when file
                  ;; If `:results' are special types like `link' or
                  ;; `graphics', don't write result to `:file'.  Only
                  ;; insert a link to `:file'.
                  (when (and result
                             (not (or (member "link" result-params)
                                      (member "graphics" result-params))))
                    (with-temp-file file
                      (insert (org-babel-format-result
                               result
                               (cdr (assq :sep params))))))
                  (setq result file))
                ;; Possibly perform post process provided its
                ;; appropriate.  Dynamically bind "*this*" to the
                ;; actual results of the block.
                (let ((post (cdr (assq :post params))))
                  (when post
                    (let ((*this* (if (not file) result
                                    (org-babel-result-to-file
                                     file
                                     (let ((desc (assq :file-desc params)))
                                       (and desc (or (cdr desc) result)))))))
                      (setq result (org-babel-ref-resolve post))
                      (when file
                        (setq result-params (remove "file" result-params))))))
                (org-babel-insert-result
                 result result-params info new-hash lang)))
            (run-hooks 'org-babel-after-execute-hook)
            result)))))))

;; generated-curl-command is used to communicate state across several function calls
(setq generated-curl-command nil)

(defvar org-babel-default-header-args:restclient-curl
  `((:results . "raw"))
  "Default arguments for evaluating a restclient block.")

;; Lambda function reified to a named function, stolen from restclient
(defun gen-restclient-curl-command (method url headers entitty)
  (let ((header-args
         (apply 'append
                (mapcar (lambda (header)
                          (list "-H" (format "%s: %s" (car header) (cdr header))))
                        headers))))
    (setq generated-curl-command
          (concat
           "#+BEGIN_SRC sh\n"
           "curl "
           (mapconcat 'shell-quote-argument
                      (append '("-i")
                              header-args
                              (list (concat "-X" method))
                              (list url)
                              (when (> (string-width entitty) 0)
                                (list "-d" entitty)))
                      " ")
           "\n#+END_SRC"))))

(defun org-babel-execute:restclient-curl (body params)
  "Execute a block of Restclient code to generate a curl command with org-babel. This function is called by `org-babel-execute-src-block'"
  (message "executing Restclient source code block")
  (with-temp-buffer
    (let ((results-buffer (current-buffer))
          (restclient-same-buffer-response t)
          (restclient-same-buffer-response-name (buffer-name))
          (display-buffer-alist
           (cons
            '("\\*temp\\*" display-buffer-no-window (allow-no-window . t))
            display-buffer-alist)))

      (insert (buffer-name))
      (with-temp-buffer
        (dolist (p params)
          (let ((key (car p))
                (value (cdr p)))
            (when (eql key :var)
              (insert (format ":%s = %s\n" (car value) (cdr value))))))
        (insert body)
        (goto-char (point-min))
        (delete-trailing-whitespace)
        (goto-char (point-min))
        (restclient-http-parse-current-and-do 'gen-restclient-curl-command))
      generated-curl-command)))

;; Make it easy to interactively generate curl commands
(defun restclient-gen-curl-command ()
  (interactive)
  (org-babel-execute-src-block-with-lang "restclient-curl"))

(defun mb/copy-region-to-clipboard()
  "Used to copy a region to clipboard when in emacs terminal"
  (interactive)
  (shell-command-on-region (region-beginning) (region-end) "pbcopy"))

;; Git functions
;; For building custom commit messages
(defun mb/get-staged-git-files()
  "Gets a list of staged gits files"
  (split-string (shell-command-to-string "git diff --cached --name-only") "\n"))
(defun mb/insert-file-name(file-name)
  "Inserts a file name into the current buffer. May be fully or partially qualified"
  (let* ((file-extension (file-name-extension file-name))
         (file-name-without-base (file-name-base file-name)))
    ;; Do not perform inserts for files with no name
    (if (not (string= "" file-name-without-base))
        ;; Check whitelist of files who's extensions to include
        (cond ((and file-extension (member file-extension '("yaml"
                                                            "fish"
                                                            "properties"
                                                            "md"
                                                            "org"
                                                            "gradle"
                                                            "sql"
                                                            "jsp"
                                                            "xml"
                                                            "yml")))
               (insert (concat
                        (file-name-directory file-name)
                        file-name-without-base
                        "." file-extension
                        ":\n-\n\n")))
              (t (insert (concat file-name-without-base ":\n-\n\n")))))))

(defun mb/generate-git-commit-msg()
  "Builds a commit message consiting of the branch name and staged files"
  (insert (concat (car (split-string (magit-get-current-branch) "_")) ":\n\n"))
  (dolist (file (mb/get-staged-git-files)) (mb/insert-file-name file))
  (evil-goto-first-line))

(defun mb/get-branch-data()
  "Gets the first word of a branch name when qualified by underscores"
  (split-string (car (split-string (magit-get-current-branch) "_")) "/"))

(defun mb/generate-turo-pr-message()
  "Generates a pull request message for work"
  (interactive)
  (split-window-below-and-focus)
  (spacemacs/new-empty-buffer)
  (markdown-mode)
  (let* ((issue-data (mb/get-branch-data))
         (issue-name (nth 1 issue-data))
         (issue-type (nth 0 issue-data)))
    (if issue-name (insert (concat "# [" issue-name "](https://team-turo.atlassian.net/browse/" issue-name ")\n")))
    (cond ((string= "b" issue-type) (insert "## Problem:\n\n## Solution:\n\n"))
          ((string= "c" issue-type) (insert "## Background:\n\n## Required Changes:\n\n"))
          ((string= "f" issue-type) (insert "## Background:\n\n##Solution:\n\n## Acceptance Criteria:\n\n"))
          (t (insert "## Acceptance Criteria:\n")))
    (evil-goto-line 3)))

(defun mb/checkout-last-branch()
  "Added to magit transient; replicates git checkout -"
  (interactive)
  (magit-branch-checkout "-"))

(defun mb/checkout-master()
  "git checkout master"
  (interactive)
  (magit-branch-checkout "master"))

(defun mb/kill-emacs-hook()
  "Performs cleanup tasks when quitting emacs"
  (mb/auto-commit-repo "~/dotfiles")
  (mb/auto-commit-repo "~/Org"))

(defun mb/auto-commit-repo(repo-path)
  (dired-at-point repo-path)
  (magit-call-git "add" "-A")
  (magit-call-git "commit" "-m" (mb/format-auto-commit-msg))
  (magit-call-git "push"))

(defun mb/format-auto-commit-msg()
 (concat "Updates: " (format-time-string "%m-%d-%Y")))

(defun mb/org-narrow-to-parent ()
  "Narrow buffer to the current subtree."
  (interactive)
  (widen)
  (org-up-element)
  (save-excursion
    (save-match-data
      (org-with-limited-levels
       (narrow-to-region
        (progn
          (org-back-to-heading t) (point))
        (progn (org-end-of-subtree t t)
               (when (and (org-at-heading-p) (not (eobp))) (backward-char 1))
               (point)))))))

(defun mb/org-babel-restclient-wrap-result ()
  "Wrap the contents of the buffer in an `org-mode' src block."
  (let ((mode-name "json"))
    (insert (format "#+BEGIN_SRC %s\n" mode-name))
    (goto-char (point-max))
    (insert "#+END_SRC\n")
    (buffer-string)))

(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(custom-safe-themes
   '("bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "274fa62b00d732d093fc3f120aca1b31a6bb484492f31081c1814a858e25c72e" default))
 '(evil-want-Y-yank-to-eol nil)
 '(org-agenda-files
   '("~/Org/Personal/PersonalTODO.org" "~/Org/Turo/TuroWorkLog.org" "~/Org/Turo/TuroVisa.org" "~/Org/Personal/Wedding.org"))
 '(package-selected-packages
   '(helm-gtags ggtags flycheck-bashate counsel-gtags howdoyou hnreader promise json-navigator hierarchy closql emacsql-sqlite emacsql ox-gfm slack emojify circe websocket parinfer clojure-snippets clj-refactor inflections edn paredit peg cider-eval-sexp-fu cider sesman queue clojure-mode oauth insert-shebang fish-mode company-shell transient oauth2 spaceline-all-the-icons doom-modeline eldoc-eval shrink-path all-the-icons memoize ibuffer-projectile disaster company-c-headers cmake-mode clang-format csv-mode wgrep smex ivy-hydra flyspell-correct-ivy counsel-projectile counsel swiper ivy web-beautify livid-mode skewer-mode simple-httpd json-mode json-snatcher json-reformat js2-refactor multiple-cursors js2-mode js-doc company-tern tern coffee-mode web-mode tagedit slim-mode scss-mode sass-mode pug-mode helm-css-scss haml-mode emmet-mode company-web web-completion-data mmm-mode markdown-toc markdown-mode gh-md restclient-helm ob-restclient ob-http company-restclient restclient know-your-http-well yaml-mode sql-indent yapfify pyvenv pytest pyenv-mode py-isort pip-requirements live-py-mode hy-mode helm-pydoc cython-mode company-anaconda anaconda-mode pythonic xterm-color shell-pop pandoc-mode ox-pandoc ht multi-term eshell-z eshell-prompt-extras esh-help ox-epub flycheck-pos-tip pos-tip flycheck typit mmt sudoku pacmacs dash-functional 2048-game zenburn-theme zen-and-art-theme white-sand-theme underwater-theme ujelly-theme twilight-theme twilight-bright-theme twilight-anti-bright-theme toxi-theme tao-theme tangotango-theme tango-plus-theme tango-2-theme sunny-day-theme sublime-themes subatomic256-theme subatomic-theme spacegray-theme soothe-theme solarized-theme soft-stone-theme soft-morning-theme soft-charcoal-theme smyx-theme seti-theme reverse-theme rebecca-theme railscasts-theme purple-haze-theme professional-theme planet-theme phoenix-dark-pink-theme phoenix-dark-mono-theme organic-green-theme omtose-phellack-theme oldlace-theme occidental-theme obsidian-theme noctilux-theme naquadah-theme mustang-theme monokai-theme monochrome-theme molokai-theme moe-theme minimal-theme material-theme majapahit-theme madhat2r-theme lush-theme light-soap-theme jbeans-theme jazz-theme ir-black-theme inkpot-theme heroku-theme hemisu-theme hc-zenburn-theme gruvbox-theme gruber-darker-theme grandshell-theme gotham-theme gandalf-theme flatui-theme flatland-theme farmhouse-theme exotica-theme espresso-theme dracula-theme django-theme darktooth-theme autothemer darkokai-theme darkmine-theme darkburn-theme dakrone-theme cyberpunk-theme color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized clues-theme cherry-blossom-theme busybee-theme bubbleberry-theme birds-of-paradise-plus-theme badwolf-theme apropospriate-theme anti-zenburn-theme ample-zen-theme ample-theme alect-themes afternoon-theme helm-company helm-c-yasnippet fuzzy company-statistics company auto-yasnippet yasnippet ac-ispell auto-complete flyspell-correct-helm flyspell-correct auto-dictionary org-projectile org-category-capture org-present org-pomodoro alert log4e gntp org-mime org-download htmlize gnuplot smeargle orgit magit-gitflow helm-gitignore gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link evil-magit magit magit-popup git-commit ghub treepy graphql with-editor ws-butler winum which-key volatile-highlights vi-tilde-fringe uuidgen use-package toc-org spaceline powerline restart-emacs request rainbow-delimiters popwin persp-mode pcre2el paradox spinner org-plus-contrib org-bullets open-junk-file neotree move-text macrostep lorem-ipsum linum-relative link-hint indent-guide hydra hungry-delete hl-todo highlight-parentheses highlight-numbers parent-mode highlight-indentation helm-themes helm-swoop helm-projectile helm-mode-manager helm-make projectile pkg-info epl helm-flx helm-descbinds helm-ag google-translate golden-ratio flx-ido flx fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state smartparens evil-indent-plus evil-iedit-state iedit evil-exchange evil-escape evil-ediff evil-args evil-anzu anzu evil goto-chg undo-tree eval-sexp-fu highlight elisp-slime-nav dumb-jump f dash s diminish define-word column-enforce-mode clean-aindent-mode bind-map bind-key auto-highlight-symbol auto-compile packed aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line helm avy helm-core popup async))
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(custom-safe-themes
   '("bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "274fa62b00d732d093fc3f120aca1b31a6bb484492f31081c1814a858e25c72e" default))
 '(evil-want-Y-yank-to-eol nil)
 '(package-selected-packages
   '(flyspell-popup helm-gtags ggtags flycheck-bashate counsel-gtags howdoyou hnreader promise json-navigator hierarchy closql emacsql-sqlite emacsql ox-gfm slack emojify circe websocket parinfer clojure-snippets clj-refactor inflections edn paredit peg cider-eval-sexp-fu cider sesman queue clojure-mode oauth insert-shebang fish-mode company-shell transient oauth2 spaceline-all-the-icons doom-modeline eldoc-eval shrink-path all-the-icons memoize ibuffer-projectile disaster company-c-headers cmake-mode clang-format csv-mode wgrep smex ivy-hydra flyspell-correct-ivy counsel-projectile counsel swiper ivy web-beautify livid-mode skewer-mode simple-httpd json-mode json-snatcher json-reformat js2-refactor multiple-cursors js2-mode js-doc company-tern tern coffee-mode web-mode tagedit slim-mode scss-mode sass-mode pug-mode helm-css-scss haml-mode emmet-mode company-web web-completion-data mmm-mode markdown-toc markdown-mode gh-md restclient-helm ob-restclient ob-http company-restclient restclient know-your-http-well yaml-mode sql-indent yapfify pyvenv pytest pyenv-mode py-isort pip-requirements live-py-mode hy-mode helm-pydoc cython-mode company-anaconda anaconda-mode pythonic xterm-color shell-pop pandoc-mode ox-pandoc ht multi-term eshell-z eshell-prompt-extras esh-help ox-epub flycheck-pos-tip pos-tip flycheck typit mmt sudoku pacmacs dash-functional 2048-game zenburn-theme zen-and-art-theme white-sand-theme underwater-theme ujelly-theme twilight-theme twilight-bright-theme twilight-anti-bright-theme toxi-theme tao-theme tangotango-theme tango-plus-theme tango-2-theme sunny-day-theme sublime-themes subatomic256-theme subatomic-theme spacegray-theme soothe-theme solarized-theme soft-stone-theme soft-morning-theme soft-charcoal-theme smyx-theme seti-theme reverse-theme rebecca-theme railscasts-theme purple-haze-theme professional-theme planet-theme phoenix-dark-pink-theme phoenix-dark-mono-theme organic-green-theme omtose-phellack-theme oldlace-theme occidental-theme obsidian-theme noctilux-theme naquadah-theme mustang-theme monokai-theme monochrome-theme molokai-theme moe-theme minimal-theme material-theme majapahit-theme madhat2r-theme lush-theme light-soap-theme jbeans-theme jazz-theme ir-black-theme inkpot-theme heroku-theme hemisu-theme hc-zenburn-theme gruvbox-theme gruber-darker-theme grandshell-theme gotham-theme gandalf-theme flatui-theme flatland-theme farmhouse-theme exotica-theme espresso-theme dracula-theme django-theme darktooth-theme autothemer darkokai-theme darkmine-theme darkburn-theme dakrone-theme cyberpunk-theme color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized clues-theme cherry-blossom-theme busybee-theme bubbleberry-theme birds-of-paradise-plus-theme badwolf-theme apropospriate-theme anti-zenburn-theme ample-zen-theme ample-theme alect-themes afternoon-theme helm-company helm-c-yasnippet fuzzy company-statistics company auto-yasnippet yasnippet ac-ispell auto-complete flyspell-correct-helm flyspell-correct auto-dictionary org-projectile org-category-capture org-present org-pomodoro alert log4e gntp org-mime org-download htmlize gnuplot smeargle orgit magit-gitflow helm-gitignore gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link evil-magit magit magit-popup git-commit ghub treepy graphql with-editor ws-butler winum which-key volatile-highlights vi-tilde-fringe uuidgen use-package toc-org spaceline powerline restart-emacs request rainbow-delimiters popwin persp-mode pcre2el paradox spinner org-plus-contrib org-bullets open-junk-file neotree move-text macrostep lorem-ipsum linum-relative link-hint indent-guide hydra hungry-delete hl-todo highlight-parentheses highlight-numbers parent-mode highlight-indentation helm-themes helm-swoop helm-projectile helm-mode-manager helm-make projectile pkg-info epl helm-flx helm-descbinds helm-ag google-translate golden-ratio flx-ido flx fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state smartparens evil-indent-plus evil-iedit-state iedit evil-exchange evil-escape evil-ediff evil-args evil-anzu anzu evil goto-chg undo-tree eval-sexp-fu highlight elisp-slime-nav dumb-jump f dash s diminish define-word column-enforce-mode clean-aindent-mode bind-map bind-key auto-highlight-symbol auto-compile packed aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line helm avy helm-core popup async))
 '(tool-bar-mode nil))
(custom-set-faces)
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(custom-safe-themes
   '("bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "274fa62b00d732d093fc3f120aca1b31a6bb484492f31081c1814a858e25c72e" default))
 '(evil-want-Y-yank-to-eol nil)
 '(package-selected-packages
   '(helm-gtags ggtags flycheck-bashate counsel-gtags howdoyou hnreader promise json-navigator hierarchy closql emacsql-sqlite emacsql ox-gfm slack emojify circe websocket parinfer clojure-snippets clj-refactor inflections edn paredit peg cider-eval-sexp-fu cider sesman queue clojure-mode oauth insert-shebang fish-mode company-shell transient oauth2 spaceline-all-the-icons doom-modeline eldoc-eval shrink-path all-the-icons memoize ibuffer-projectile disaster company-c-headers cmake-mode clang-format csv-mode wgrep smex ivy-hydra flyspell-correct-ivy counsel-projectile counsel swiper ivy web-beautify livid-mode skewer-mode simple-httpd json-mode json-snatcher json-reformat js2-refactor multiple-cursors js2-mode js-doc company-tern tern coffee-mode web-mode tagedit slim-mode scss-mode sass-mode pug-mode helm-css-scss haml-mode emmet-mode company-web web-completion-data mmm-mode markdown-toc markdown-mode gh-md restclient-helm ob-restclient ob-http company-restclient restclient know-your-http-well yaml-mode sql-indent yapfify pyvenv pytest pyenv-mode py-isort pip-requirements live-py-mode hy-mode helm-pydoc cython-mode company-anaconda anaconda-mode pythonic xterm-color shell-pop pandoc-mode ox-pandoc ht multi-term eshell-z eshell-prompt-extras esh-help ox-epub flycheck-pos-tip pos-tip flycheck typit mmt sudoku pacmacs dash-functional 2048-game zenburn-theme zen-and-art-theme white-sand-theme underwater-theme ujelly-theme twilight-theme twilight-bright-theme twilight-anti-bright-theme toxi-theme tao-theme tangotango-theme tango-plus-theme tango-2-theme sunny-day-theme sublime-themes subatomic256-theme subatomic-theme spacegray-theme soothe-theme solarized-theme soft-stone-theme soft-morning-theme soft-charcoal-theme smyx-theme seti-theme reverse-theme rebecca-theme railscasts-theme purple-haze-theme professional-theme planet-theme phoenix-dark-pink-theme phoenix-dark-mono-theme organic-green-theme omtose-phellack-theme oldlace-theme occidental-theme obsidian-theme noctilux-theme naquadah-theme mustang-theme monokai-theme monochrome-theme molokai-theme moe-theme minimal-theme material-theme majapahit-theme madhat2r-theme lush-theme light-soap-theme jbeans-theme jazz-theme ir-black-theme inkpot-theme heroku-theme hemisu-theme hc-zenburn-theme gruvbox-theme gruber-darker-theme grandshell-theme gotham-theme gandalf-theme flatui-theme flatland-theme farmhouse-theme exotica-theme espresso-theme dracula-theme django-theme darktooth-theme autothemer darkokai-theme darkmine-theme darkburn-theme dakrone-theme cyberpunk-theme color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized clues-theme cherry-blossom-theme busybee-theme bubbleberry-theme birds-of-paradise-plus-theme badwolf-theme apropospriate-theme anti-zenburn-theme ample-zen-theme ample-theme alect-themes afternoon-theme helm-company helm-c-yasnippet fuzzy company-statistics company auto-yasnippet yasnippet ac-ispell auto-complete flyspell-correct-helm flyspell-correct auto-dictionary org-projectile org-category-capture org-present org-pomodoro alert log4e gntp org-mime org-download htmlize gnuplot smeargle orgit magit-gitflow helm-gitignore gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link evil-magit magit magit-popup git-commit ghub treepy graphql with-editor ws-butler winum which-key volatile-highlights vi-tilde-fringe uuidgen use-package toc-org spaceline powerline restart-emacs request rainbow-delimiters popwin persp-mode pcre2el paradox spinner org-plus-contrib org-bullets open-junk-file neotree move-text macrostep lorem-ipsum linum-relative link-hint indent-guide hydra hungry-delete hl-todo highlight-parentheses highlight-numbers parent-mode highlight-indentation helm-themes helm-swoop helm-projectile helm-mode-manager helm-make projectile pkg-info epl helm-flx helm-descbinds helm-ag google-translate golden-ratio flx-ido flx fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state smartparens evil-indent-plus evil-iedit-state iedit evil-exchange evil-escape evil-ediff evil-args evil-anzu anzu evil goto-chg undo-tree eval-sexp-fu highlight elisp-slime-nav dumb-jump f dash s diminish define-word column-enforce-mode clean-aindent-mode bind-map bind-key auto-highlight-symbol auto-compile packed aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line helm avy helm-core popup async))
 '(tool-bar-mode nil))
(custom-set-faces)
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
