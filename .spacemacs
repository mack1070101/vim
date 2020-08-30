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
     ;; General Utilities
     ibuffer
     themes-megapack
     ;; Markup and text processing
     markdown
     (org :variables org-enable-github-support t)
     yaml
     html
     json
     csv
     ;; Utilities for writing code & prose
     ivy
     (auto-completion :variables
                      auto-completion-enable-sort-by-usage t)
     spell-checking
     syntax-checking
     git
     ;; Programming language layers
     python
     rust
     (terraform :variables terraform-auto-format-on-save t)
     shell-scripts
     (shell :variables
            shell-default-height 30
            shell-default-shell 'vterm
            shell-enable-smart-eshell t
            shell-default-position 'bottom)
     emacs-lisp
     java
     kotlin
     (clojure :variables clojure-enable-linters 'clj-kondo)
     restclient
     (sql :variables sql-capitalize-keywords t))

   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages '(parinfer-rust-mode
                                      emojify
                                      json
                                      ox-hugo
                                      solaire-mode
                                      exec-path-from-shell)
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
   dotspacemacs-startup-banner nil

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
   dotspacemacs-themes '(doom-dracula
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
   dotspacemacs-default-font '("Menlo"
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
   dotspacemacs-auto-generate-layout-names 't

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
   dotspacemacs-loading-progress-bar nil

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
   dotspacemacs-undecorated-at-startup 't

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 80

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
   dotspacemacs-smooth-scrolling nil

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
   dotspacemacs-enable-server t

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
  (setq comp-deferred-compilation t)

  ;; Magit performance tweaks
  (setq magit-refresh-status-buffer nil)
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  (setq magit-status-buffer-switch-function 'switch-to-buffer)
                                        ;Turn off emacs native version control because I only use magit
  (setq vc-handled-backends nil)

  (eval-after-load 'org
    (lambda()
      ;; Clojure in orgmode stuff
      (require 'org)
      (require 'ob-clojure)
      (require 'org-tempo)
      (require 'cider)
      (require 'flycheck)
      (setq org-babel-clojure-backend 'cider)

      ;; General config
      (setq org-startup-indented t)
      ;; Disable "ask to execute code block"  because it's annoying
      (setq org-confirm-babel-evaluate nil)))

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
you should place your code here."

  ;; GENERAL CONFIGURATION
  ;; Config auto complete
  (setq company-idle-delay 0.2)
  (global-company-mode)
  ;; Execute cleanup functions when Emacs is closed
  (add-hook 'kill-emacs-hook 'mb/kill-emacs-hook)
  ;; Fix bug where Dired gives a warning on first launch on macos
  (setq dired-use-ls-dired nil)
  ;; Fix bug where vterm always launches with zsh instead of fish
  (exec-path-from-shell-initialize)
  (solaire-global-mode +1)

 ;; WINDOW CONFIGURATION
  ;; Automatic buffer resizing based on which split has focus
  ;; Bias towards splitting horizontally on narrow screens customized to 15 inch MBP
  (setq split-width-threshold 168)
  (setq fringe-mode 'no-fringes)

  ;; Spaceline config
  (setq spaceline-purpose-p nil)
  (setq spaceline-minor-modes-p nil)
  (setq spaceline-org-clock-p t)

  ;; TERMINAL CONFIGURATION
  (add-hook 'term-mode-hook 'toggle-truncate-lines)
  ;; Disable evil in vterm so fish can handle vi emulation
  (evil-set-initial-state 'vterm-mode 'emacs)
  ;; Make links in terminals clickable
  (add-hook 'shell-mode-hook 'goto-address-mode)
  (add-hook 'vterm-mode-hook 'goto-address-mode)

  ;; NAVIGATION CONFIGURATION
  (spacemacs|define-custom-layout "@DUNLOP"
    :binding "d"
    :body
    (progn
      (dired-at-point "~/code/dunlop/")
      (magit-status "~/code/dunlop/")))

  (spacemacs|define-custom-layout "@CODE"
    :binding "c"
    :body
    (progn
      (dired-at-point "~/code/")))

  ;; Rebind avy goto char to match Intellij
  (global-set-key "j" (quote avy-goto-char))

  ;; Dired - Readable file sizes
  (setq dired-listing-switches "-alh")

  ;; IBUFFER CONFIGURATION
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
                filename-and-process)))

  ;; ORG MODE CONFIGURATION
  ;; Wrap long lines in org-mode
  (add-hook 'org-mode-hook 'auto-fill-mode)
  ;; Force headings to be the same Size. Not sure if I'm crazy...
  (add-hook 'org-load-hook #'mb/org-mode-hook)
  ;; Ensure buffers are saved automatically to prevent sync errors
  (add-hook 'auto-save-hook 'org-save-all-org-buffers)
  ;; Save file (if it exists) when cycling TODO states
  (advice-add 'org-todo           :after 'mb/save-buffer-if-file)
  (advice-add 'org-deadline       :after 'mb/save-buffer-if-file)
  (advice-add 'org-schedule       :after 'mb/save-buffer-if-file)
  (advice-add 'org-store-log-note :after 'mb/save-buffer-if-file)
  ;; Refile notes to top
  (setq org-reverse-note-order t)
  ;; Fix double splits when executing restclient org-babel blocks in spacemacs
  (add-hook 'org-babel-after-execute-hook 'mb/org-babel-after-execute-hook)
  ;; Size images displayed in org buffers to be more reasonable by default
  (setq org-image-actual-width 600)
  ;; Org key bindings
  (spacemacs/set-leader-keys-for-major-mode 'org-mode "I" 'org-clock-in)
  (spacemacs/set-leader-keys-for-major-mode 'org-mode "O" 'org-clock-out)
  (spacemacs/set-leader-keys-for-major-mode 'org-mode "sp" 'mb/org-narrow-to-parent)
  ;; Toggle TODO states in normal mode with the "t" key
  (evil-define-key 'normal org-mode-map "t" 'org-todo)
  ;; Fix latex stuff
  (add-hook 'text-scale-mode-hook 'mb/update-org-latex-fragment-scale)
  (setenv "PATH" (concat "/Library/TeX/texbin" (getenv "PATH")))
  (setq exec-path (append '("/Library/TeX/texbin") exec-path))
  ;; Sets custom TODO states
  (setq org-todo-keywords '((sequence "TODO"
                                      "IN-PROGRESS"
                                      "|"
                                      "DONE"
                                      "WILL-NOT-IMPLEMENT")))
  (setq org-log-state-notes-into-drawer t)
  ;; Tweak priorities to A B C D, from A B C to make "A" super important,
  ;; "B" important, "C" normal, and "D" not important
  (setq org-lowest-priority 68)
  (setq org-default-priority 67)
                                        ;Force completing subtask TODOs, and checkbox items before a TODO can be completed
  (setq org-enforce-todo-dependencies 't)
  (setq org-enforce-todo-checkbox-dependencies 't)

  ;; ORG-AGENDA CONFIGURATION
  (setq org-agenda-start-with-follow-mode 't)
  (setq org-agenda-files (list "~/Org/Inbox.org"
                               "~/Org/Turo.org"
                               "~/Org/Personal.org"
                               "~/Org/TuroVisa.org"
                               "~/Org/Wedding.org"))
  ;; Build custom agenda views
  (setq mb/turo-sprint-name "sierra_nevada")
  (setq org-agenda-custom-commands '(("n" "Agenda and all TODOs"
                                      ((agenda "")
                                       (todo "")))
                                     ("d" "Today and all TODOs"
                                      ((agenda "" ((org-agenda-span 'day)))
                                       (todo "")))
                                     ("w" "Work TODOs"
                                      ((agenda "" ((org-agenda-span 'day)))
                                       (tags-todo (concat "turo+" mb/turo-sprint-name "-recurring"))
                                       (tags-todo (concat "turo-" mb/turo-sprint-name "-recurring"))
                                       (tags-todo "turo+recurring")))
                                     ("p" "Personal TODOs"
                                      ((agenda "" ((org-agenda-span 'day)))
                                       (tags-todo "personal-recurring-outdoor-programming")
                                       (tags-todo "wedding")
                                       (tags-todo "outdoor")
                                       (tags-todo "personal+programming")
                                       (tags-todo "personal+recurring+tasks")
                                       (tags-todo "personal+recurring+chores")
                                       (tags-todo "personal+recurring-people-chores-tasks")
                                       (tags-todo "personal+recurring+people")))))

  ;; org-capture templates and reflile config
  (setq org-refile-targets '((org-agenda-files :maxlevel . 3)))
  (setq org-capture-templates
        `(("n" "Note" entry (file+headline "~/Org/Inbox.org" "Notes")
           "* %?\n%U\n  %i\n" :prepend t)
          ("t" "TODO" entry (file+headline "~/Org/Inbox.org" "Tasks")
           "* TODO %? \n%U\n  %i\n" :prepend t)
          ("s" "TODO - SCHEDULED" entry (file+headline "~/Org/Inbox.org" "Tasks")
           "\n\n** TODO %?\nSCHEDULED: <%(org-read-date nil nil nil)>"
           :prepend t)
          ("T" "TODO Ticket" entry (file+headline "~/Org/Turo.org" "Tickets")
           ,(concat "* TODO %? :" mb/turo-sprint-name ":"
                    "\n%U\n** Checklist:[0/1]\n- [ ] Self review  %i\n")
           :prepend t
           :jump-to-captured t)
          ("m" "Generic Meeting" entry (file+headline "~/Org/Inbox.org" "Meetings")
           "* %t %?"
           :jump-to-captured t)
          ("w" "Work Meeting" entry (file+olp"~/Org/Turo.org" "Meetings")
           "* %t %?"
           :jump-to-captured t)))

  ;; Org babel/programming config
  (setq org-modules '(ol-bbdb ol-bibtex ol-docview ol-eww ol-gnus ol-info ol-irc ol-mhe ol-rmail ol-w3m org-checklist))
  (with-eval-after-load
      (org-babel-do-load-languages 'org-babel-load-languages '((sql . t)
                                                               (clojure . t)
                                                               (restclient . t)
                                                               (java . t)
                                                               (shell . t))))
  (use-package ox-hugo :ensure t :after ox)

  ;; MAGIT CONFIGURATION
  ;; temp install of fotingo emacs
  (package-install-file "~/code/fotingo-emacs")
  ;; Add commands to magit menus
  (transient-append-suffix 'magit-branch "l" '("-" "Checkout last branch" mb/checkout-last-branch))
  (transient-append-suffix 'magit-branch "-" '("M" "Checkout master" mb/checkout-master))
  (transient-insert-suffix 'magit-pull "-r" '("-f" "Overwrite local branch" "--force"))
  (transient-append-suffix 'magit-dispatch "F" '("o" "Fotingo" fotingo-dispatch))
  ;; Add commit message generation
  (add-hook 'git-commit-setup-hook 'mb/generate-git-commit-msg)
  (setq magit-list-refs-sortby "-committerdate")

  ;; CLOJURE STUFF
  ;; Set configs for parinfer
;;  (setq parinfer-extensions
;;        '(pretty-parens  ; different paren styles for different modes.
;;          evil           ; If you use Evil.
;;          smart-tab))      ; C-b & C-f jump positions and smart shift with tab & S-tab.
  ;; Add linting for clojure; fixes not being able to run flycheck in buffers without a file
  (flycheck-define-checker clojure-joker-mb
    "A Clojure syntax checker using Joker.
  See URL `https://github.com/candid82/joker'."
    :command ("joker" "--lint" "-")
    :standard-input t
    :error-patterns
    ((error line-start "<stdin>:" line ":" column ": " (0+ not-newline) (or "error: " "Exception: ") (message) line-end)
     (warning line-start "<stdin>:" line ":" column ": " (0+ not-newline) "warning: " (message) line-end))
    :modes (clojure-mode clojurec-mode))
  (add-to-list 'flycheck-checkers 'clojure-joker-mb)

  ;; Lisp programming configuration
  (add-hook 'emacs-lisp-mode-hook 'parinfer-rust-mode)
  (add-hook 'clojure-mode-hook 'parinfer-rust-mode)

  ;; SQL programming configuration
  (add-hook 'sql-mode-hook 'flycheck-mode))

;; Magit helper functions
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
                                                            "tf"
                                                            "yml")))
               (insert (concat
                        (file-name-directory file-name)
                        file-name-without-base
                        "." file-extension
                        ":\n-\n\n")))
              (t (insert (concat file-name-without-base ":\n-\n\n")))))))

(defun mb/generate-git-commit-msg()
  "Builds a commit message consiting of the branch name and staged files"
  (let* ((replaced-string (replace-regexp-in-string "DUNLOP" "D" (magit-get-current-branch)))
         (max (string-match "_" replaced-string))
         (almost-done (substring replaced-string 0 max)))
    (insert (concat almost-done ":\n\n")))

  (dolist (file
           ((lambda () (split-string
                        (shell-command-to-string "git diff --cached --name-only")
                        "\n"))))
    (mb/insert-file-name file))
  (evil-goto-first-line))

(defun mb/checkout-last-branch()
  "Added to magit transient; replicates git checkout -"
  (interactive)
  (magit-branch-checkout "-"))

(defun mb/checkout-master()
  "git checkout master"
  (interactive)
  (magit-branch-checkout "master"))

(defun mb/auto-commit-repo(repo-path)
  (dired-at-point repo-path)
  (let* ((changed-files (string-match "" (shell-command-to-string "git ls-files --modified"))))
    ;; Only run the process if the directory has changed
    (magit-call-git "add" "-A")
    (magit-call-git "commit" "-m" (mb/format-auto-commit-msg))
    (magit-call-git "push")))

(defun mb/format-auto-commit-msg()
  (concat "Updates: "
          (format-time-string "%Y-%m-%dT%T")
          ((lambda (x)
             (concat (substring x 0 3)
                     ":"
                     (substring x 3 5)))
           (format-time-string "%z"))))

;; General Emacs functions
(defun mb/kill-emacs-hook()
  "Performs cleanup tasks when quitting emacs"
  ;; Clock out when shutting down to prevent dangling clocks
  (org-clock-out nil t)
  (org-save-all-org-buffers)
  ;; Keep my dotfiles and notes always backed up
  (mb/auto-commit-repo "~/dotfiles")
  (mb/auto-commit-repo "~/Org"))

(defun mb/keep-duplicate-lines ()
  "Utility function for keeping lines that have duplicates"
  (interactive)
  (let (lines dups)
    (save-excursion
      (goto-char (point-max))
      (when (/= (char-after (1- (point-max))) ?\n)
        (newline))
      (goto-char (point-min))
      (while (not (eobp))
        (forward-line 1)
        (push (buffer-substring-no-properties (line-beginning-position)
                                              (line-end-position))
              lines))
      (dolist (line lines)
        (when (and (> (cl-count line lines :test 'equal) 1)
                   (not (string= "" line)))
          (push (regexp-quote line) dups)))
      (goto-char (point-min))
      (keep-lines (mapconcat #'identity
                             dups
                             "\\|")))))

(defun mb/save-buffer-if-file (&rest _rest)
  "Save the buffer if it has an associated file"
  (if (buffer-file-name)
      (save-buffer)))

;; ORG-MODE helper functions
(defun mb/org-babel-after-execute-hook()
  "Bug fix for error with restclient"
  (if (string= (car (org-babel-get-src-block-info)) "restclient")
      (delete-window)))

(defun mb/org-mode-hook ()
  "Keep headings all the same size"
  (set-face-attribute 'org-level-1 nil :height 1.0))

(defun mb/org-babel-run-block ()
  "Run a code block by name"
  (interactive)
  (save-excursion
    (goto-char
     (org-babel-find-named-block
      (completing-read "#+NAME: "
                       (org-babel-src-block-names))))
    (org-babel-execute-src-block-maybe)))

(defun mb/update-org-latex-fragment-scale ()
  (interactive)
  (let ((text-scale-factor (expt text-scale-mode-step text-scale-mode-amount)))
    (plist-put org-format-latex-options :scale (* 2.3 text-scale-factor))))

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

;; I don't use custom for anything. Everything should be defined in code
(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization.")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(treemacs flyspell-correct flycheck magit git-commit with-editor transient counsel ace-window popup markdown-mode rust-mode projectile package-lint ivy org-plus-contrib hydra evil goto-chg fotingo-emacs ox-hugo zenburn-theme zen-and-art-theme yasnippet-snippets yapfify yaml-mode xterm-color ws-butler writeroom-mode winum white-sand-theme which-key web-mode web-beautify vterm volatile-highlights vi-tilde-fringe uuidgen use-package underwater-theme ujelly-theme twilight-theme twilight-bright-theme twilight-anti-bright-theme treemacs-projectile treemacs-persp treemacs-magit treemacs-icons-dired treemacs-evil toxi-theme toml-mode toc-org terminal-here tao-theme tangotango-theme tango-plus-theme tango-2-theme tagedit symon symbol-overlay sunny-day-theme sublime-themes subatomic256-theme subatomic-theme string-inflection sqlup-mode sql-indent spaceline-all-the-icons spacegray-theme soothe-theme solarized-theme soft-stone-theme soft-morning-theme soft-charcoal-theme smyx-theme smex smeargle slim-mode shell-pop seti-theme scss-mode sass-mode rg reverse-theme restart-emacs rebecca-theme rainbow-delimiters railscasts-theme racer pytest pyenv-mode py-isort purple-haze-theme pug-mode professional-theme prettier-js popwin planet-theme pippel pipenv pip-requirements phoenix-dark-pink-theme phoenix-dark-mono-theme pcre2el password-generator parinfer paradox ox-gfm overseer orgit organic-green-theme org-projectile org-present org-pomodoro org-mime org-gcal org-download org-cliplink org-bullets org-brain open-junk-file omtose-phellack-theme oldlace-theme occidental-theme obsidian-theme ob-restclient ob-http noctilux-theme naquadah-theme nameless mvn mustang-theme multi-term move-text monokai-theme monochrome-theme molokai-theme moe-theme modus-vivendi-theme modus-operandi-theme mmm-mode minimal-theme meghanada maven-test-mode material-theme markdown-toc majapahit-theme magit-svn magit-section magit-gitflow madhat2r-theme macrostep lush-theme lorem-ipsum live-py-mode link-hint light-soap-theme kotlin-mode kaolin-themes json-navigator json-mode jbeans-theme jazz-theme ivy-yasnippet ivy-xref ivy-purpose ivy-hydra ir-black-theme insert-shebang inkpot-theme indent-guide importmagic impatient-mode ibuffer-projectile hybrid-mode hungry-delete hl-todo highlight-parentheses highlight-numbers highlight-indentation heroku-theme hemisu-theme helm-make hc-zenburn-theme gruvbox-theme gruber-darker-theme groovy-mode groovy-imports grandshell-theme gradle-mode gotham-theme google-translate golden-ratio gnuplot gitignore-templates gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link gh-md gandalf-theme fuzzy font-lock+ flyspell-correct-ivy flycheck-rust flycheck-pos-tip flycheck-package flycheck-kotlin flycheck-elsa flycheck-clj-kondo flycheck-bashate flx-ido flatui-theme flatland-theme fish-mode farmhouse-theme fancy-battery eziam-theme eyebrowse expand-region exotica-theme evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-textobj-line evil-surround evil-org evil-numbers evil-nerd-commenter evil-matchit evil-magit evil-lisp-state evil-lion evil-indent-plus evil-iedit-state evil-goggles evil-exchange evil-escape evil-ediff evil-cleverparens evil-args evil-anzu espresso-theme eshell-z eshell-prompt-extras esh-help emr emojify emmet-mode elisp-slime-nav editorconfig dumb-jump dracula-theme dotenv-mode doom-themes django-theme diminish devdocs define-word darktooth-theme darkokai-theme darkmine-theme darkburn-theme dakrone-theme cython-mode cyberpunk-theme csv-mode counsel-projectile counsel-css company-web company-terraform company-statistics company-shell company-restclient company-anaconda column-enforce-mode color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized clues-theme clojure-snippets clean-aindent-mode cider-eval-sexp-fu cider chocolate-theme cherry-blossom-theme centered-cursor-mode cargo busybee-theme bubbleberry-theme blacken birds-of-paradise-plus-theme badwolf-theme auto-yasnippet auto-highlight-symbol auto-dictionary auto-compile apropospriate-theme anti-zenburn-theme ample-zen-theme ample-theme alect-themes aggressive-indent afternoon-theme ace-link ac-ispell)))
(custom-set-faces)
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
