;;; Package --- eos
;;; Commentary: ... Present day, present time ....
;;; Code:

;;; -*- lexical-binding: t -*-

(when (version< emacs-version "26.3")
  (error "This requires Emacs 26.3 and above!"))

(require 'org nil t)

;; load languages (reference)
;; (org-babel-do-load-languages 'org-babel-load-languages
;;                              '((emacs-lisp . t)))))

(defun eos/gc/defer-gc-collection ()
  "Set 'gc-cons-threshold most-positive-fixnum."
  (setq gc-cons-threshold most-positive-fixnum))

(defun eos/gc/restore-gc-collection ()
  "Defer garbage collection."
  (run-at-time
   1 nil (lambda () (setq gc-cons-threshold 16777216))))

;; threshold inital value
(setq gc-cons-threshold most-positive-fixnum ; 2^61 bytes
      gc-cons-percentage 0.5)

;; hooks
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold 16777216 ; 16mb
                  gc-cons-percentage 0.1)))

;; defer garbage collection
(add-hook 'minibuffer-setup-hook 'eos/gc/defer-gc-collection)

;; reset threshold to inital value
(add-hook 'minibuffer-exit-hook 'eos/gc/restore-gc-collection)

(defvar eos-file-name-handler-alist
  file-name-handler-alist
  "Save file-name-handler-alist")

;; clean file-name-handler-alist
(setq file-name-handler-alist nil)

;; hooks
;; restore file-name-handler-alist
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist eos-file-name-handler-alist)))

;; y or n
(defalias 'yes-or-no-p 'y-or-n-p)

(defvar eos-tags-map
  (make-sparse-keymap)
  "Keymap for tags (navigation) keybinds.")

(defvar eos-pm-map
  (make-sparse-keymap)
  "Keymap for project manager keybinds.")

(defvar eos-sc-map
  (make-sparse-keymap)
  "Keymap for syntax check keybinds.")

(defvar eos-complete-map
  (make-sparse-keymap)
  "Keymap for (complete) keybinds.")

(defvar eos-window-map
  (make-sparse-keymap)
  "Keymap for window related keybinds.")

(defvar eos-docs-map
  (make-sparse-keymap)
  "Keymap for documentation keybinds.")

(defvar eos-rtags-map
  (make-sparse-keymap)
  "Keymap for rtag minor mode keybinds.")

(dolist (prefix-map '(eos-tags-map
                      eos-pm-map
                      eos-sc-map
                      eos-docs-map
                      eos-window-map
                      eos-complete-map
                      eos-rtags-map))
  (define-prefix-command prefix-map))

(defun eos/funcall (func &optional args)
  "Call FUNC if it's bounded."
  (when (fboundp func)
    (funcall func args)))

(defun eos/build ()
  "If the current buffer is '~/emacs.d/init.org' the code-blocks are
tangled, and the tangled file is compiled."
  (interactive)
  (when (equal (buffer-name) "init.org")
    (progn
      ;; Avoid running hooks when tangling.
      (let ((prog-mode-hook nil))
        (org-babel-tangle)
        (byte-compile-file (concat user-emacs-directory "init.el"))))))

(defun eos/load-file (file)
  "Load FILE if exists."
  (if (file-exists-p file)
      (load (expand-file-name file) t nil nil)))

(defun eos/toggle-debug-on-error ()
  "Toggle `debug-on-error`."
  (interactive)
  (customize-set-variable 'debug-on-error (not debug-on-error))
  (message "Debug-on-error: %s"
           (if debug-on-error "enabled" "disabled")))

(defun eos/buffer-too-big-p ()
  "Return t if buffer-size if to big."
  (interactive)
  (or (> (buffer-size) (* 5000 80))
      (> (line-number-at-pos (point-max)) 5000)))

(defun eos/mkdir (dir)
  "Create DIR in the file system."
  ;; (interactive)
  (when (and (not (file-exists-p dir))
             (make-directory dir :parents))))

(defun eos/compile (dir command)
  "Compile COMMAND at specific DIR.
Just a `compile` function wrapper."
  (interactive)
  (if (file-exists-p dir)
      (let ((default-directory dir))
        (compile command))))

(defun eos/move/beginning-of-line (arg)
  "Move point back to indentation(ARG) start, or line(ARG) start."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; line movement
(global-set-key (kbd "C-a") 'eos/move/beginning-of-line)
(global-set-key (kbd "C-e") 'move-end-of-line)

;; word movement
;; (global-set-key (kbd "C-<left>") 'backward-word)
;; (global-set-key (kbd "C-<right>") 'forward-whitespace)

;; scroll movement
(global-set-key (kbd "C-M-v") 'scroll-other-window)
(global-set-key (kbd "C-M-y") 'scroll-other-window-down)

(defun eos/edit/move-lines (n)
  "Move N lines, up if N is positive, else down."
  (let* (text-start
         text-end
         (region-start (point))
         (region-end region-start)
         swap-point-mark
         delete-latest-newline)

    ;; STEP 1: identifying the text to cut.
    (when (region-active-p)
      (if (> (point) (mark))
          (setq region-start (mark))
        (exchange-point-and-mark)
        (setq swap-point-mark t
              region-end (point))))

    ;; text-end and region-end
    (end-of-line)

    (if (< (point) (point-max))
        (forward-char 1)
      (setq delete-latest-newline t)
      (insert-char ?\n))
    (setq text-end (point)
          region-end (- region-end text-end))

    ;; text-start and region-start
    (goto-char region-start)
    (beginning-of-line)
    (setq text-start (point)
          region-start (- region-start text-end))

    ;; STEP 2: cut and paste.
    (let ((text (delete-and-extract-region text-start text-end)))
      (forward-line n)
      ;; If the current-column != 0, I have moved the region at the bottom of a
      ;; buffer doesn't have the trailing newline.
      (when (not (= (current-column) 0))
        (insert-char ?\n)
        (setq delete-latest-newline t))
      (insert text))

    ;; STEP 3: Restoring.
    (forward-char region-end)

    (when delete-latest-newline
      (save-excursion
        (goto-char (point-max))
        (delete-char -1)))

    (when (region-active-p)
      (setq deactivate-mark nil)
      (set-mark (+ (point) (- region-start region-end)))
      (if swap-point-mark
          (exchange-point-and-mark)))))

(defun eos/edit/move-lines-up (n)
  "Move N lines up."
  (interactive "p")
  (if (eq n nil)
      (setq n 1))
  (eos/edit/move-lines (- n)))

(defun eos/edit/move-lines-down (n)
  "Move N lines down."
  (interactive "p")
  (if (eq n nil)
      (setq n 1))
  (eos/edit/move-lines n))

(defun eos/edit/move-words-left (n)
  "Move word N times to the left."
  (interactive "p")
  (if (eq n nil)
      (setq n 1))
  (transpose-words (- n)))

(defun eos/edit/indent-buffer ()
  "Indent the currently visited buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

(defun eos/edit/indent-region-or-buffer ()
  "Indent a region if selected, otherwise the whole buffer."
  (interactive)
  (save-excursion
    (if (region-active-p)
        (progn
          (indent-region (region-beginning) (region-end))
          (message "Indented selected region."))
      (progn
        (eos/edit/indent-buffer)
        (message "Indented buffer.")))))

(defun eos/edit/duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.
  If there's no region, the current line will be duplicated.
  However, if there's a region, all lines that region covers will be duplicated."
  (interactive "p")
  (let (beg end (origin (point)))
    (if (and mark-active (> (point) (mark)))
        (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if mark-active
        (exchange-point-and-mark))
    (setq end (line-end-position))
    (let ((region (buffer-substring-no-properties beg end))
          (i arg))
      (while (> i 0)
        (goto-char end)
        (newline)
        (insert region)
        (setq end (point))
        (setq i (1- i)))
      (goto-char (+ origin (* (length region) arg) arg)))))

;; edit
(global-set-key (kbd "M-i") 'eos/edit/indent-region-or-buffer)
(global-set-key (kbd "M-j") 'eos/edit/duplicate-current-line-or-region)

(global-set-key (kbd "M-p") 'eos/edit/move-lines-up)
(global-set-key (kbd "M-n") 'eos/edit/move-lines-down)

(defun eos/kill-buffer (buffer-name)
  "Kill BUFFER-NAME if exists."
  (when (get-buffer buffer-name)
    (kill-buffer buffer-name)))

(defun eos/kill/current-buffer ()
  "Kill the current buffer without prompting."
  (interactive)
  (kill-buffer (current-buffer)))

(defun eos/run/proc (name)
  "Run a process defined by NAME."
  (if (executable-find name)
      (start-process name nil name)
    nil))

;;; Get symbol at point, maybe
(defun eos/get-selected-text-or-symbol-at-point ()
  "Get the text in region or symbol at point.

If region is active, return the text in that region.  Else if the
point is on a symbol, return that symbol name.  Else return nil."
  (cond ((use-region-p)
         (buffer-substring-no-properties (region-beginning) (region-end)))
        ((symbol-at-point)
         (substring-no-properties (thing-at-point 'symbol)))
        (t
         nil)))

(defun eos/set-frame-font (font)
  "Set the default font to FONT."
  (cond ((find-font (font-spec :name font))
         (set-frame-font font nil t))))

;; non-nil means to make the cursor very visible
(customize-set-variable 'visible-cursor nil)

;; scroll options
;; number of lines of margin at the top and bottom of a window
(customize-set-variable 'scroll-margin 0)

;; scroll up to this many lines, to bring point back on screen
(customize-set-variable 'scroll-conservatively 100)

;; t means point keeps its screen position
(customize-set-variable 'scroll-preserve-screen-position t)

;; non-nil means mouse commands use dialog boxes to ask questions
(customize-set-variable 'use-dialog-box nil)

;; set window margins
;; width in columns of left marginal area for display of a buffer
(customize-set-variable 'left-margin-width 1)

;; width in columns of right marginal area for display of a buffer.
(customize-set-variable 'right-margin-width 1)

;; binds (global)
(global-set-key (kbd "s-l") 'shrink-window-horizontally)
(global-set-key (kbd "s-h") 'enlarge-window-horizontally)
(global-set-key (kbd "s-j") 'shrink-window)
(global-set-key (kbd "s-k") 'enlarge-window)

;; next and previous buffer (on current window)
(define-key ctl-x-map (kbd "C-,") 'previous-buffer)
(define-key ctl-x-map (kbd "C-.") 'next-buffer)

;; binds (eos-window prefix map)
(define-key eos-window-map (kbd "1") 'maximize-window)
(define-key eos-window-map (kbd "q") 'minimize-window)
(define-key eos-window-map (kbd "w") 'balance-windows)

;; binds ctl-x-map (C-x w)
(define-key ctl-x-map (kbd "w") 'eos-window-map)

;; kill buffer and window
(define-key ctl-x-map (kbd "C-k") 'kill-buffer-and-window)

(add-to-list 'display-buffer-alist
             '(("\\*Choices\\*"
                (display-buffer-below-selected display-buffer-at-bottom)
                (inhibit-same-window . t)
                (window-height . fit-window-to-buffer))))

;; custom
;; non-nil inhibits the startup screen.
(customize-set-variable 'inhibit-startup-screen t)

;; non-nil inhibits the startup screen
(customize-set-variable 'inhibit-startup-message t)

;; non-nil inhibits the initial startup echo area message
(customize-set-variable 'inhibit-startup-echo-area-message t)

;; custom
;; non-nil means do not display continuation lines.
(customize-set-variable 'truncate-lines nil)

;; sentences should be separated by a single space,
;; so treat two sentences as two when filling
(customize-set-variable 'sentence-end-double-space nil)

;; default indent
;; distance between tab stops (for display of tab characters), in columns.
(customize-set-variable 'tab-width 4)

;; indentation can insert tabs if this is non-nil.
(customize-set-variable 'indent-tabs-mode nil)

;; kill process not confirmation required
;; list of functions called with no args to query before killing a buffer.
;; The buffer being killed will be current while the functions are running.
(customize-set-variable
 'kill-buffer-query-functions
 (remq 'process-kill-buffer-query-function kill-buffer-query-functions))

;; non-nil means load prefers the newest version of a file.
(customize-set-variable 'load-prefer-newer t)

;; hooks
;; (add-hook 'buffer-list-update-hook
;;           (lambda ()
;;             (if (eos/buffer-too-big-p)
;;                 (eos/funcall 'display-line-numbers 0))))

;; non-nil means to allow minibuffer commands while in the minibuffer
(customize-set-variable 'enable-recursive-minibuffers t)

;; coding system to use with system messages
(customize-set-variable 'locale-coding-system 'utf-8)

;; coding system to be used for encoding the buffer contents on saving
(customize-set-variable 'buffer-file-coding-system 'utf-8)

;; add coding-system at the front of the priority list for automatic detection
(prefer-coding-system 'utf-8)

;; set coding system (UFT8)
(set-language-environment "UTF-8")
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)

(when (require 'simple nil t)
  (progn
    ;; custom
    ;; don't omit information when lists nest too deep.
    (customize-set-variable 'eval-expression-print-level nil)

    ;; binds
    (define-key ctl-x-map (kbd "C-g") 'keyboard-quit)

    ;; enable
    ;; column number display in the mode line
    (eos/funcall 'column-number-mode 1)

    ;; buffer size display in the mode line
    (eos/funcall 'size-indication-mode 1)))

(require 'prog-mode nil t)

(when (require 'server nil t)
  (progn
    ;; hooks
    ;; enable emacs server after startup
    (add-hook 'emacs-startup-hook
              (lambda ()
                (eos/funcall 'server-start)))))

(when (require 'help nil t)
  (progn
    ;; custom
    ;; always select the help window
    (customize-set-variable 'help-window-select t)))

;; binds
(when (boundp 'help-map)
  (progn
    ;; clean, quality of life
    (define-key help-map (kbd "<help>") nil)
    (define-key help-map (kbd "<f1>") nil)
    (define-key help-map (kbd "C-n") nil)
    (define-key help-map (kbd "C-h") nil)
    (define-key help-map (kbd "C-;") nil)
    (define-key help-map (kbd "K") nil)
    (define-key help-map (kbd "RET") nil)))

(require 'help-mode nil t)

;; binds
(when (boundp 'help-mode-map)
  (progn
    (define-key help-mode-map (kbd "C-j") 'push-button)))

(when (require 'info nil t)
  (progn
    ;; custom
    ;; non-nil means don’t record intermediate Info nodes to the history
    (customize-set-variable 'info-history-skip-intermediate-nodes nil)

    ;; 0 means do not display breadcrumbs
    ;; (customize-set-variable 'info-breadcrumbs-depth 0)
    ))

(when (require 'fringe nil t)
  (progn
    ;; disable
    (add-hook 'after-init-hook
              (lambda ()
                ;; set the default appearance of fringes on the selected frame
                ;; 1 ->  ("no-fringes" . 0)
                (set-fringe-style 1)))))

(when (require 'files nil t)
  (progn
    ;; custom
    ;; control use of version numbers for backup files.
    (customize-set-variable 'version-control t)

    ;; non-nil means always use copying to create backup files
    (customize-set-variable 'backup-by-copying t)

    ;; number of newest versions to keep when a new numbered backup is made
    (customize-set-variable 'kept-new-versions 6)

    ;; number of oldest versions to keep when a new numbered backup is made
    (customize-set-variable 'kept-old-versions 2)

    ;; if t, delete excess backup versions silently
    (customize-set-variable 'delete-old-versions t)

    ;; non-nil means make a backup of a file the first time it is saved
    (customize-set-variable 'make-backup-files nil)

    ;; non-nil says by default do auto-saving of every file-visiting buffer
    (customize-set-variable 'auto-save-default nil)

    ;; most *NIX tools work best when files are terminated
    ;; with a newline
    (customize-set-variable 'require-final-newline t)

    ;; backup directory list
    ;; alist of filename patterns and backup directory names
    (customize-set-variable 'backup-directory-alist '(("" . "~/.emacs.d/backup")))))

;; create cache directory
(eos/mkdir (concat user-emacs-directory "cache"))

(when (require 'recentf nil t)
  (progn
    ;; custom
    ;; file to save the recent list into.
    (customize-set-variable
     'recentf-save-file (concat user-emacs-directory "cache/recentf"))))

(when (require 'bookmark nil t)
  (progn
    ;; custom
    ;; file in which to save bookmarks by default.
    (customize-set-variable
     'bookmark-default-file (concat user-emacs-directory "cache/bookmarks"))))

(when (require 'savehist nil t)
  (progn
    ;; file name where minibuffer history is saved to and loaded from.
    (customize-set-variable
     'savehist-file (concat user-emacs-directory "cache/history"))

    ;; if non-nil, save all recorded minibuffer histories.
    (customize-set-variable 'savehist-save-minibuffer-history t)

    ;; enable savehist mode
    (eos/funcall 'savehist-mode 1)))

(when (require 'frame nil t)
  (progn
    ;; custom
    ;; with some window managers you may have to set this to non-nil
    ;; in order to set the size of a frame in pixels, to maximize
    ;; frames or to make them fullscreen.
    (customize-set-variable 'frame-resize-pixelwise t)

    ;; normalize before maximize
    (customize-set-variable 'x-frame-normalize-before-maximize t)

    ;; set frame title format
    (customize-set-variable 'frame-title-format
                            '((:eval (if (buffer-file-name)
                                         (abbreviate-file-name (buffer-file-name))
                                       "%b"))))

    ;; alist of parameters for the initial X window frame
    (add-to-list 'initial-frame-alist '(fullscreen . fullheight))

    ;; (vertical-scroll-bars)
    ;; (bottom-divider-width . 0)
    ;; (right-divider-width . 6)))

    ;; alist of default values for frame creation
    (add-to-list 'default-frame-alist '(internal-border-width . 2))

    ;; set frame font
    (eos/set-frame-font "Hermit Light:pixelsize=18")

    ;; hooks
    ;; enable window divider
    (add-hook 'after-init-hook
              (lambda()
                (eos/funcall 'window-divider-mode)))

    ;; disable blink cursor
    (add-hook 'emacs-startup-hook
              (lambda()
                (eos/funcall 'blink-cursor-mode 0)))))

;; binds
(global-set-key (kbd "C-x C-o") 'other-frame)

;; set font by face attribute (reference)
;; (set-face-attribute 'default nil :height)

(when (require 'windmove nil t)
  (progn
    ;; enable
    ;; window move default keybinds (shift-up/down etc..)
    (eos/funcall 'windmove-default-keybindings)))

(when (require 'kmacro nil t)
  (progn
    ;; binds
    (define-key ctl-x-map (kbd "m") 'kmacro-keymap)))

(require 'paren nil t)

;; enable
;; visualization of matching parens
(eos/funcall 'show-paren-mode 1)

(when (require 'hideshow nil t)
  (progn
    ;; hooks
    (add-hook 'prog-mode-hook 'hs-minor-mode)

    ;; binds
    (define-key ctl-x-map (kbd "[") 'hs-toggle-hiding)))

(when (require 'elec-pair nil t)
  (progn
    ;; custom
    ;; alist of pairs that should be used regardless of major mode.
    (customize-set-variable 'electric-pair-pairs
                            '((?\{ . ?\})
                              (?\( . ?\))
                              (?\[ . ?\])
                              (?\" . ?\")))

    ;; enable
    (eos/funcall 'electric-pair-mode 1)))

(when (require 'newcomment nil t)
  (progn
    ;; binds
    (global-set-key (kbd "M-c") 'comment-or-uncomment-region)))

(when (require 'time nil t)
  (progn
    ;; custom
    ;; seconds between updates of time in the mode line.
    (customize-set-variable 'display-time-interval 15)

    ;; non-nil indicates time should be displayed as hh:mm, 0 <= hh <= 23
    (customize-set-variable 'display-time-24hr-format t)

    ;; set format time string
    (customize-set-variable 'display-time-format "%H:%M")

    ;; load-average values below this value won’t be shown in the mode line.
    (customize-set-variable 'display-time-load-average-threshold 1.0)

    ;; enable display time
    (eos/funcall 'display-time-mode 1)))

(when (require 'tool-bar nil t)
  (progn
    ;; disable
    (eos/funcall 'tool-bar-mode 0)))

(when (require 'tooltip nil t)
  (progn
    ;; disable tooltip
    (eos/funcall 'tooltip-mode 0)))

(when (require 'menu-bar nil t)
  (progn
    ;; disable menu-bar
    (eos/funcall 'menu-bar-mode 0)))

(when (require 'scroll-bar nil t)
  (progn
    ;; disable scroll bar
    (eos/funcall 'scroll-bar-mode 0)))

(when (require 'hl-line nil t)
  (progn
    ;; enable highlight line
    (eos/funcall 'global-hl-line-mode 1)))

(when (require 'linum nil t)
  (progn
    ;; custom
    ;; format used to display line numbers.
    (customize-set-variable 'linum-format " %2d ")))

(when (require 'display-line-numbers nil t)
  (progn
    ;; hooks
    (add-hook 'prog-mode-hook 'display-line-numbers-mode)))

;; enable display line numbers mode
;; (eos/funcall 'global-display-line-numbers-mode 1)))

(when (require 'delsel nil t)
  (progn
    ;; delete selection-mode
    (eos/funcall 'delete-selection-mode 1)))

(when (require 'whitespace nil t)
  (progn
    ;; hooks
    ;; clean whitespace and newlines before buffer save
    (add-hook 'before-save-hook 'whitespace-cleanup)

    ;; binds
    (define-key ctl-x-map (kbd ".") 'whitespace-mode)))

(when (require 'subword nil t)
  (progn
    ;; enable
    (eos/funcall 'global-subword-mode 1)))

(when (require 'face-remap nil t)
  (progn
    ;; binds
    ;; text scale adjust
    (define-key ctl-x-map (kbd "=") 'text-scale-adjust)))

(when (require 'completion nil t)
  (progn
    ;; custom
    ;; how far to search in the buffer when looking for completions. Hide
    ;; in number of characters.  If nil, search the whole buffer.
    (customize-set-variable 'completion-search-distance 0)

    ;; if non-nil, the next completion prompt does a cdabbrev search.
    (customize-set-variable 'completion-cdabbrev-prompt-flag t)

    ;; non-nil means show help message in *Completions* buffer.
    (customize-set-variable 'completion-show-help t)

    ;; non-nil means separator characters mark previous word as used
    (customize-set-variable 'completion-on-separator-character t)

    ;; the filename to save completions to.
    (customize-set-variable
     'save-completions-file-name
     (expand-file-name "cache/completitions" user-emacs-directory))

    ;; non-nil means save most-used completions when exiting emacs
    (customize-set-variable 'save-completions-flag t)

    ;; Discard a completion if unused for this many hours. Hide
    ;; (1 day = 24, 1 week = 168).  If this is 0, non-permanent completions
    ;; will not be saved unless these are used.  Default is two weeks.
    (customize-set-variable 'save-completions-retention-time 0)

    ;; enable
    ;; dynamic completion on
    (eos/funcall 'dynamic-completion-mode 0)))

;; add display-buffer-alist
(add-to-list 'display-buffer-alist
             '("\\*Completions\\*" display-buffer-below-selected))

(defun eos/complete-or-indent ()
  (interactive)
  (if (looking-at "\\_>")
      (when (fboundp 'complete)
        (complete nil)))
  (indent-according-to-mode))

(defun eos/complete-at-point-or-indent ()
  "This smart tab is minibuffer compliant: it acts as usual in
the minibuffer. Else, if mark is active, indents region. Else if
point is at the end of a symbol, expands it. Else indents the
current line."
  (interactive)
  (if (minibufferp)
      (unless (minibuffer-complete)
        (complete-symbol nil))
    (if mark-active
        (indent-region (region-beginning)
                       (region-end))
      (if (looking-at "\\_>")
          (complete-symbol nil)
        (indent-according-to-mode)))))

;; binds (testing)
(global-set-key (kbd "M-RET") 'eos/complete-or-indent)

(when (require 'dabbrev nil t)
  (progn
    ;; custom
    ;; non-nil means case sensitive search.
    (customize-set-variable 'dabbrev-upcase-means-case-search t)

    ;; whether dabbrev treats expansions as the same if they differ in case
    ;; a value of nil means treat them as different.
    (customize-set-variable 'dabbrev-case-distinction t)))

(when (require 'custom nil t)
  (progn
    ;; custom
    ;; file used for storing customization information.
    ;; The default is nil, which means to use your init file
    ;; as specified by ‘user-init-file’.  If the value is not nil,
    ;; it should be an absolute file name.
    (customize-set-variable
     'custom-file (concat (expand-file-name user-emacs-directory) "custom.el"))))

;; load custom-file
(eos/load-file custom-file)

(require 'forms nil t)

;; avoid warnings when byte-compile
(eval-when-compile
  ;; eval require when compile
  (require 'cask "~/.cask/cask.el" t)

  ;; enable
  (if (fboundp 'cask-initialize)
      (cask-initialize)))

;; load cask
(require 'cask "~/.cask/cask.el" t)

;; initialize cask
(eos/funcall 'cask-initialize)

(when (require 'exwm nil t)
  (progn
    (require 'exwm-config nil t)

    ;; set exwm workspaces number
    (customize-set-variable 'exwm-workspace-number 0)

    ;; show workspaces in all buffers
    (customize-set-variable 'exwm-workspace-show-all-buffers t)

    ;; non-nil to allow switching to buffers on other workspaces
    (customize-set-variable 'exwm-layout-show-all-buffers t)

    ;; non-nil to force managing all X windows in tiling layout.
    (customize-set-variable 'exwm-manage-force-tiling t)

    ;; exwn global keybindings
    (customize-set-variable 'exwm-input-global-keys
                            `(([?\s-r] . exwm-reset)
                              ([?\s-q] . exwm-input-toggle-keyboard)
                              ;; ([?\s-w] . exwm-workspace-switch)
                              ;; ([?\s-k] . exwm-workspace-delete)
                              ;; ([?\s-a] . exwm-workspace-swap)

                              ;; create and switch to workspaces
                              ,@(mapcar (lambda (i)
                                          `(,(kbd (format "s-%d" i)) .
                                            (lambda ()
                                              (interactive)
                                              (exwm-workspace-switch-create ,i))))
                                        (number-sequence 0))))

    ;; The following example demonstrates how to use simulation keys to mimic
    ;; the behavior of Emacs.  The value of `exwm-input-simulation-keys` is a
    ;; list of cons cells (SRC . DEST), where SRC is the key sequence you press
    ;; and DEST is what EXWM actually sends to application.  Note that both SRC
    ;; and DEST should be key sequences (vector or string).
    (customize-set-variable 'exwm-input-simulation-keys
                            '(
                              ;; movement
                              ([?\C-b] . [left])
                              ([?\M-b] . [C-left])
                              ([?\C-f] . [right])
                              ([?\M-f] . [C-right])
                              ([?\C-p] . [up])
                              ([?\C-n] . [down])
                              ([?\C-a] . [home])
                              ([?\C-e] . [end])
                              ([?\M-v] . [prior])
                              ([?\C-v] . [next])
                              ([?\C-d] . [delete])
                              ([?\C-k] . [S-end delete])

                              ;; firefox temporary
                              ([?\C-o] . [C-prior]) ; change tab mapping
                              ([?\C-k] . [C-w]) ; close tab mapping
                              ([?\C-j] . [return]) ; close tab mapping

                              ;; cut/paste.
                              ([?\C-w] . [?\C-x])
                              ([?\M-w] . [?\C-c])
                              ([?\C-y] . [?\C-v])

                              ;; Escape (cancel)
                              ([?\C-g] . [escape])

                              ;; search
                              ([?\C-s] . [?\C-f])))))

;; this little bit will make sure that XF86 keys work in exwm buffers as well
(if (boundp 'exwm-input-prefix-keys)
    (progn
      (dolist (key '(XF86AudioLowerVolume
                     XF86AudioRaiseVolume
                     XF86PowerOff
                     XF86AudioMute
                     XF86AudioPlay
                     XF86AudioStop
                     XF86AudioPrev
                     XF86AudioNext
                     XF86ScreenSaver
                     XF68Back
                     XF86Forward
                     Scroll_Lock
                     print))
        (cl-pushnew key exwm-input-prefix-keys))))

;; enable exwm
(eos/funcall 'exwm-enable)

;; All buffers created in EXWM mode are named "*EXWM*". You may want to
;; change it in `exwm-update-class-hook' and `exwm-update-title-hook', which
;; are run when a new X window class name or title is available.  Here's
;; some advice on this topic:
;; + Always use `exwm-workspace-rename-buffer` to avoid naming conflict.
;; + For applications with multiple windows (e.g. GIMP), the class names of
;; all windows are probably the same.  Using window titles for them makes
;; more sense.
(require 'exwm-core nil t)
(require 'exwm-workspace nil t)

;; hooks
;; update the buffer name by X11 window title
(add-hook 'exwm-update-title-hook
          (lambda ()
            (exwm-workspace-rename-buffer
             (concat "[" exwm-class-name "] " exwm-title))))

(when (require 'exwm-randr nil t)
  (progn
    ;; custom
    ;; monitors: check the xrandr(1) output and use the same name/order
    ;; TODO: create a func that retrieves these values from xrandr
    ;; (customize-set-variable
    ;;  'exwm-randr-workspace-monitor-plist '(0 "eDP-1"
    ;;                                        1 "HDMI-1"))

    (customize-set-variable 'exwm-workspace-number
                            (if (boundp 'exwm-randr-workspace-monitor-plist)
                                (progn
                                  (/ (safe-length exwm-randr-workspace-monitor-plist) 2))
                              1))))
;; enable
;; (exwm-randr-enable)

(defvar eos/helm-source-exwm-buffers
  nil
  "Helm exwm buffers source.")

(when (require 'helm-exwm nil t)
  (progn
    ;; exwm buffers list
    (setq eos/helm-source-exwm-buffers
          (if (fboundp 'helm-exwm-build-source)
              (helm-exwm-build-source)))))

(require 'async nil t)
(require 'async-bytecomp nil t)

;; to run command without displaying the output in a window
(add-to-list 'display-buffer-alist
             '("\\*Async Shell Command\\*" display-buffer-no-window))

(when (require 'buffer-move nil t)
  (progn
    ;; bind
    (global-set-key (kbd "<C-S-up>") 'buf-move-up)
    (global-set-key (kbd "<C-S-down>") 'buf-move-down)
    (global-set-key (kbd "<C-S-left>") 'buf-move-left)
    (global-set-key (kbd "<C-S-right>") 'buf-move-right)))

(when (require 'helm nil t)
  (progn
    ;; require
    (require 'helm-config nil t)

    ;; custom
    ;; idle time before updating, specified in seconds (variable defined as float)
    (customize-set-variable 'helm-input-idle-delay 0.01)

    ;; the default side to display `helm-buffer'
    (customize-set-variable 'helm-split-window-default "below")

    ;; set autoresize max and mim height
    (customize-set-variable 'helm-autoresize-max-height 40)
    (customize-set-variable 'helm-autoresize-min-height 40)

    ;; enable fuzzing matching
    (customize-set-variable 'helm-M-x-fuzzy-match t)
    (customize-set-variable 'helm-imenu-fuzzy-match t)
    (customize-set-variable 'helm-recentf-fuzzy-match t)
    (customize-set-variable 'helm-apropos-fuzzy-match t)
    (customize-set-variable 'helm-lisp-fuzzy-completion t)
    (customize-set-variable 'helm-buffers-fuzzy-matching t)

    ;; helm-M-x save command in extended-command-history even when it fail
    (customize-set-variable 'helm-M-x-always-save-history t)

    ;; always show details in buffer list when non-nil
    (customize-set-variable 'helm-buffer-details-flag t)

    ;; forces split inside selected window when non-nil
    (customize-set-variable 'helm-split-window-inside-p t)

    ;; cycle to the beginning or end of the list after reaching the bottom or top
    (customize-set-variable 'helm-move-to-line-cycle-in-source t)

    ;; scroll amount when scrolling other window in a helm session
    (customize-set-variable 'helm-scroll-amount 8)

    ;; send current input in header-line when non-nil
    (customize-set-variable 'helm-echo-input-in-header-line t)

    ;; display header-line when non nil
    (customize-set-variable 'helm-display-header-line nil)

    ;; specify the space before prompt in header-line
    (customize-set-variable 'helm-header-line-space-before-prompt 'left-margin)

    ;; search for library in 'require' and 'declare-function' sexp
    (customize-set-variable 'helm-ff-search-library-in-sexp t)

    ;; use `recentf-list' instead of `file-name-history' in `helm-find-files'
    (customize-set-variable 'helm-ff-file-name-history-use-recentf t)

    ;; this enable support for completing-read-multiple
    ;; and completion-at-point when non--nil
    (customize-set-variable 'helm-mode-handle-completion-in-region t)

    ;; if non-nil, prevent escaping from minibuffer with other-window
    ;; during the helm sessions
    (customize-set-variable 'helm-prevent-escaping-from-minibuffer nil)

    ;; use the same state of window split, vertical or horizontal
    (customize-set-variable 'helm-split-last-window-split-state t)

    ;; helm left marginal area for display of a buffer
    (customize-set-variable 'helm-left-margin-width 1)

    ;; left-margin-width value for `helm-mini' and `helm-buffers-list'
    (customize-set-variable 'helm-buffers-left-margin-width 1)

    ;; binds (C-x)
    ;; (define-key ctl-x-map (kbd "b") 'helm-buffers-list)
    (define-key ctl-x-map (kbd "C-b") 'helm-mini)
    (define-key ctl-x-map (kbd "C-f") 'helm-find-files)
    (define-key ctl-x-map (kbd "c") 'helm-command-prefix)

    ;; binds (C-h) help
    (define-key help-map (kbd "a") 'helm-apropos)

    ;; binds (global)
    (global-set-key (kbd "M-x") 'helm-M-x)
    (global-set-key (kbd "M-y") 'helm-show-kill-ring)
    (global-set-key (kbd "M-m") 'helm-mark-ring)))

;; enable
(eos/funcall 'helm-mode 1)
(eos/funcall 'helm-autoresize-mode 1)

;; binds
(when (boundp 'helm-map)
  (progn
    (define-key helm-map (kbd "TAB") 'helm-execute-persistent-action)
    (define-key helm-map (kbd "C-j") 'helm-maybe-exit-minibuffer)
    (define-key helm-map (kbd "C-z") 'helm-select-action)))

(when (require 'helm-lib nil t)
  (progn
    ;; custom
    ;; display help window in full frame when non nil
    (customize-set-variable 'helm-help-full-frame t)))

;; for some silency (byte-compile)
(defvar helm-mini-default-sources nil "")

(when (require 'helm-source nil t)
  (progn
    ;; files buffers list
    (defvar eos/helm-source-file-buffers
      (if (fboundp 'helm-make-source)
          (helm-make-source "File Buffers" 'helm-source-in-buffer
            :data 'helm-buffer-list
            :candidate-transformer (lambda (buffers)
                                     (cl-loop for buf in buffers
                                              when (with-current-buffer
                                                       buf buffer-file-name)
                                              collect buf))
            :action 'helm-type-buffer-actions))
      "Helm file buffers source.")

    ;; non files buffers list
    (defvar eos/helm-source-nonfile-buffers
      (if (fboundp 'helm-make-source)
          (helm-make-source "Non-file Buffers" 'helm-source-in-buffer
            :data 'helm-buffer-list
            :candidate-transformer (lambda (buffers)
                                     (cl-loop for buf in buffers
                                              unless (with-current-buffer
                                                         buf buffer-file-name)
                                              collect buf))
            :filtered-candidate-transformer 'helm-skip-boring-buffers
            :action 'helm-type-buffer-actions))
      "Helm nonfile buffers source.")

    ;; setq helm-mini default sources
    (setq helm-mini-default-sources
          '(eos/helm-source-file-buffers
            eos/helm-source-exwm-buffers
            helm-source-buffers-list
            helm-source-recentf
            ;; eos/helm-source-nonfile-buffers
            helm-source-buffer-not-found))))

;; add eos-theme-dir to theme load path
(add-to-list 'custom-theme-load-path
             (concat user-emacs-directory "themes"))

;; load theme
(load-theme 'mesk-term t)

(when (require 'all-the-icons nil t)
  (progn
    ;; custom
    ;; whether or not to include a foreground colour when formatting the icon
    (customize-set-variable 'all-the-icons-color-icons nil)

    ;; the default adjustment to be made to the `raise' display property of an icon
    (customize-set-variable 'all-the-icons-default-adjust -0.0)

    ;; the base Scale Factor for the `height' face property of an icon
    (customize-set-variable 'all-the-icons-scale-factor 1.0)))

(when (require 'epa nil t)
  (progn
    ;; custom
    ;; if non-nil, cache passphrase for symmetric encryption.
    (customize-set-variable
     'epa-file-cache-passphrase-for-symmetric-encryption t)

    ;; if t, always asks user to select recipients.
    (customize-set-variable 'epa-file-select-keys nil)

    ;; the gpg executable.
    (customize-set-variable 'epg-gpg-program "gpg")

    ;; the pinentry mode.
    ;; In epa commands, a particularly useful mode is ‘loopback’, which
    ;; redirects all Pinentry queries to the caller, so Emacs can query
    ;; passphrase through the minibuffer, instead of external Pinentry
    ;; program.
    (customize-set-variable 'epa-pinentry-mode 'loopback)))

(when (require 'auth-source nil t)
  (progn

    ;; Note: If the auth-sources variable contains ~/.auth.gpg before
    ;; ~/.auth, the auth-source library will try to read the GnuPG
    ;; encrypted .gpg file first, before the unencrypted file.

    ;; list of authentication sources
    (customize-set-variable
     'auth-sources '("~/.auth/auth.gpg" "~/.auth/netrc"))))

(require 'password-store nil t)

(defun eos/lookup-password (host user port)
  "Lookup password on auth-source default file."
  (let ((auth (auth-source-search :host host :user user :port port)))
    (if auth
        (let ((secretf (plist-get (car auth) :secret)))
          (if secretf
              (funcall secretf)
            (error "Auth entry for %s@%s:%s has no secret!"
                   user host port)))
      (error "No auth entry found for %s@%s:%s" user host port))))

(when (require 'helm-info nil t)
  (progn
    ;; binds
    (if (boundp 'helm-map)
        (progn
          (define-key help-map (kbd "C-i") 'helm-info)))))

(when (require 'helm-descbinds nil t)
  (progn
    ;; helm-descbinds, window splitting style (2: vertical)
    (customize-set-variable 'helm-descbinds-window-style 2)))

;; binds help-map (C-h)
(if (boundp 'help-map)
    (progn
      (define-key help-map (kbd "C-b") 'helm-descbinds)))

(when (require 'iedit nil t)
  (progn
    ;; if no-nil, the key is inserted into global-map,
    ;; isearch-mode-map, esc-map and help-map.
    (customize-set-variable 'iedit-toggle-key-default nil)))

;; binds
(when (boundp 'iedit-mode-keymap)
  (define-key iedit-mode-keymap (kbd "TAB") 'eos/complete-or-indent))

(when (require 'undo-tree nil t)
  (progn
    ;; define alias for redo
    (defalias 'redo 'undo-tree-redo)

    ;; binds
    (define-key ctl-x-map (kbd "u") 'undo-tree-visualize)))

;; enable
(eos/funcall 'global-undo-tree-mode 1)

(require 'editorconfig nil t)

;; enable
(eos/funcall 'editorconfig-mode)

(require 'ibuffer nil t)

(when (require 'dashboard nil t)
  (progn
    ;; items
    (customize-set-variable 'dashboard-items
                            '((recents . 4)
                              (projects . 4)
                              (agenda . 4)
                              (bookmarks . 4)))

    ;; banners directory
    (customize-set-variable 'dashboard-banners-directory
                            (concat user-emacs-directory "banner/"))

    ;; banner
    (customize-set-variable 'dashboard-startup-banner 1)

    ;; page separator
    (customize-set-variable 'dashboard-page-separator "

 ")

    ;; footer icon
    (customize-set-variable 'dashboard-footer-icon
                            #(" " 0 1 (face dashboard-footer)))

    ;; a footer with some short message
    (customize-set-variable 'dashboard-footer
                            "Litany Against Fear

  I must not fear.
  Fear is the mind-killer.
  Fear is the little-death that brings total obliteration.
  I will face my fear.
  I will permit it to pass over me and through me.
  And when it has gone past I will turn the inner eye to see its path.
  Where the fear has gone there will be nothing.
  Only I will remain.
  ")

    ;; a list of messages, one of which dashboard chooses to display
    (customize-set-variable 'dashboard-footer-messages nil)

    ;; set initial buffer choice (emacsclient fix)
    (customize-set-variable 'initial-buffer-choice
                            (lambda ()
                              (let ((initial-buffer (get-buffer "*dashboard*")))
                                (unless initial-buffer
                                  (setq initial-buffer (get-buffer "*scratch*")))
                                initial-buffer)))

    ;; init dashboard after emacs initialize
    (add-hook 'after-init-hook 'dashboard-setup-startup-hook)))

(when (require 'artist nil t)
  (progn
    ;; custom
    ;; whether or not to incrementally update display when flood-filling
    (customize-set-variable 'artist-flood-fill-show-incrementally nil)

    ;; whether or not to remove white-space at end of lines
    (customize-set-variable 'artist-trim-line-endings nil)))

(require 'locate nil t)

(when (require 'helm-locate nil t)
  (progn
    ;; custom
    ;; disable fuzzy matching in `helm-locate'.
    (customize-set-variable 'helm-locate-fuzzy-match nil)

    ;; a list of arguments for locate program
    ;; berkeley-unix: "locate %s %s" (not working with fuzzing match?)
    (customize-set-variable 'helm-locate-command "locate %s %s")))

(when (require 'helm-swoop nil t)
  (progn
    ;; custom
    ;; if nil, you can slightly boost invoke speed in exchange for text color
    (customize-set-variable 'helm-swoop-speed-or-color nil)

    ;; split window when having multiple windows open
    (customize-set-variable 'helm-swoop-split-with-multiple-windows t)

    ;; if t, use fuzzy matching functions as well as exact matches
    (customize-set-variable 'helm-swoop-use-fuzzy-match t)

    ;; return to the opposite side of line.
    (customize-set-variable 'helm-swoop-move-to-line-cycle t)

    ;; use face to line numbers on helm-swoop buffer
    (customize-set-variable 'helm-swoop-use-line-number-face nil)

    ;; bind global
    (global-set-key (kbd "C-s") 'helm-swoop)))

;; binds
(when (boundp 'helm-swoop-map)
  (progn
    (define-key helm-swoop-map (kbd "C-s")
      'helm-multi-swoop-all-from-helm-swoop)

    (define-key helm-swoop-map (kbd "C-c s c")
      'helm-multi-swoop-current-mode-from-helm-swoop)))

;; load helm-imenu
(when (require 'helm-imenu nil t)
  (progn
    ;; add hooks
    (add-hook 'prog-mode
              (lambda ()
                ;; bind to C-x `
                (define-key ctl-x-map (kbd "`") 'helm-imenu-in-all-buffers)))))

;; binds (local map)
(when (boundp 'helm-imenu-map)
  (progn
    (define-key helm-imenu-map (kbd "C-M-i") 'helm-next-source)))

(when (require 'dired nil t)
  (progn
    ;; enable dired-find-alternate-file
    (put 'dired-find-alternate-file 'disabled nil)))

(when (require 'dired-async nil t)
  (progn
    ;; enable dired-aysnc-mode
    (eos/funcall 'dired-async-mode 1)))

;; binds
(if (boundp 'dired-mode-map)
    (progn
      (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
      (define-key dired-mode-map (kbd "C-j") 'dired-find-alternate-file)))

(require 'elfeed nil t)

(when (require 'moody nil t)
  (progn
    ;; remove underline
    (customize-set-variable 'x-underline-at-descent-line t)

    ;; change line height
    (customize-set-variable 'moody-mode-line-height 32)

    ;; mode-line format
    (customize-set-variable 'mode-line-format
                            '("%e  "
                              ;; "%*%& %l:%c | %I "
                              ;; mode-line-mule-info
                              "%*%& "
                              ;; mode-line-misc-info
                              ;; mode-line-percent-position
                              "(%l:%c) / %I  "
                              mode-line-misc-info
                              ""
                              moody-mode-line-buffer-identification
                              " %m "
                              (vc-mode moody-vc-mode)
                              " "
                              ))))

(when (require 'erc nil t)
  (progn
    ;; the string to append to the nick if it is already in use.
    (customize-set-variable 'erc-nick-uniquifier "_")

    ;; non-nil means rename buffers with network name, if available.
    (customize-set-variable 'erc-rename-buffers t)

    ;; prompt for channel key when using erc-join-channel interactively.
    (customize-set-variable 'erc-prompt-for-channel-key t)

    ;; asks before using the default password,
    ;; or whether to enter a new one.
    (customize-set-variable 'erc-prompt-for-password t)

    ;; if nil, ERC will call system-name to get this information.
    (customize-set-variable 'erc-system-name "eos")

    ;;   if non-nil, then all incoming CTCP requests will be shown.
    (customize-set-variable 'erc-paranoid t)

    ;; disable replies to CTCP requests that require a reply.
    (customize-set-variable 'erc-disable-ctcp-replies t)

    ;; be paranoid, don’t give away your machine name.
    (customize-set-variable 'erc-anonymous-login t)

    ;; show the channel key in the header line.
    (customize-set-variable 'erc-show-channel-key-p t)

    ;; kill all query (also channel) buffers of this server on QUIT.
    (customize-set-variable 'erc-kill-queries-on-quit t)

    ;; functions
    (defun eos/irc-tls ()
      "A `erc-tls function interface."
      (interactive)
      (let ((server "irc.freenode.net")
            (nick "esac-io"))
        (erc-tls :server server :port 6697 :nick nick
                 :password (eos/lookup-password server nick 6697))))))

;; binds
(when (boundp 'erc-mode-map)
  (progn
    ;; use eos/complete
    (define-key erc-mode-map (kbd "TAB") 'eos/complete)))

(when (require 'shell nil t)
  (progn
    ;; hook
    (add-hook 'shell-mode-hook
              (lambda()
                ;; do not display continuation lines.
                (toggle-truncate-lines)

                ;; disable line numbers
                (display-line-numbers-mode 0)))))

(require 'eshell nil t)

;; binds
(define-key ctl-x-map (kbd "&") 'eshell)

(when (require 'term nil t)
  (progn
    ;; custom
    ;; if non-nil, is file name to use for explicitly requested inferior shell. (reference)
    (customize-set-variable 'explicit-shell-file-name
                            (if (eq system-type "gnu/linux")
                                "/usr/bin/fish"
                              "/usr/local/bin/fish"))

    ;; if non-nil, add a ‘/’ to completed directories
    (customize-set-variable 'term-completion-addsuffix t)

    ;; regexp to recognize prompts in the inferior process
    ;; (customize-set-variable 'term-prompt-regexp "^\\(>\\|\\(->\\)+\\) *")
    ;; (customize-set-variable 'term-prompt-regexp ".*:.*>.*? ")

    ;; if non-nil, automatically list possibilities on partial completion.
    (customize-set-variable 'term-completion-autolist t)

    ;; if true, buffer name equals process name
    (customize-set-variable 'term-ansi-buffer-base-name t)

    ;; functions
    (defun eos/term-send-kill-line ()
      "Kill line in multi-term mode with the possibility to paste it like in a normal shell."
      (interactive)
      (when (fboundp 'term-send-raw-string)
        (progn
          (kill-line)
          (term-send-raw-string "\C-k"))))

    ;; binds (with hook)
    (add-hook 'term-mode-hook
              (lambda ()
                (when (and (boundp 'term-raw-map)
                           (boundp 'term-mode-map))
                  (progn
                    ;; term-raw-map
                    (define-key term-raw-map (kbd "s-q") 'term-line-mode)

                    ;; term-mode-map
                    (define-key term-mode-map (kbd "s-q") 'term-char-mode)))))

    ;; hook
    (add-hook 'term-mode-hook
              (lambda()
                ;; do not display continuation lines.
                (setq truncate-lines nil)

                ;; disable line numbers mode
                (display-line-numbers-mode 0)))))

(when (require 'multi-term nil t)
  (progn
    ;; custom
    (customize-set-variable 'multi-term-program "/usr/local/bin/fish")

    ;; focus terminal window after you open dedicated window
    (customize-set-variable 'multi-term-dedicated-select-after-open-p t)

    ;; the buffer name of term buffer.
    (customize-set-variable 'multi-term-buffer-name "Term")

    ;; binds (C-x) prefix
    (define-key ctl-x-map (kbd "<C-return>") 'multi-term)
    (define-key ctl-x-map (kbd "C-x") 'multi-term-dedicated-toggle)))

(defun eos/launch/st ()
  "Launch st terminal."
  (interactive)
  (eos/run/proc "st"))

(when (require 'shr nil t)
  (progn
    (customize-set-variable 'shr-width 80)
    (customize-set-variable 'shr-use-fonts nil)
    (customize-set-variable 'shr-use-colors nil)
    (customize-set-variable 'shr-inhibit-images t)
    (customize-set-variable 'shr-blocked-images t)
    (customize-set-variable 'shr-color-visible-distance-min 10)
    (customize-set-variable 'shr-color-visible-luminance-min 80)))

(when (require 'eww nil t)
  (progn
    ;; define google search url
    (defvar eos/eww-google-search-url "https://www.google.com/search?q="
      "URL for Google searches.")

    ;; custom search prefix
    (customize-set-variable 'eww-search-prefix eos/eww-google-search-url)
    ;; (customize-set-variable eww-search-prefix "https://duckduckgo.com/html/?q=")

    ;; custom download directory
    (customize-set-variable 'eww-download-directory "~/down")

    ;; custom checkbox symbols
    (customize-set-variable 'eww-form-checkbox-symbol "[ ]")
    (customize-set-variable 'eww-form-checkbox-selected-symbol "[X]")
    ;; (customize-set-variable eww-form-checkbox-symbol "☐") ; Unicode hex 2610
    ;; (customize-set-variable eww-form-checkbox-selected-symbol "☑") ; Unicode hex 2611

    ;; Re-write of the `eww-search-words' definition.
    (defun eos/eww-search-words ()
      "Search the web for the text between BEG and END.
      If region is active (and not whitespace), search the web for
      the text in that region.
      Else if the region is not active, and the point is on a symbol,
      search the web for that symbol.
      Else prompt the user for a search string.
      See the `eww-search-prefix' variable for the search engine used."
      (interactive)
      (let ((search-string (eos/get-selected-text-or-symbol-at-point)))
        (when (and (stringp search-string)
                   (string-match-p "\\`[[:blank:]]*\\'" search-string))
          (customize-set-variable search-string nil))
        (if (stringp search-string)
            (eww search-string)
          (call-interactively #'eww))))
    ))

;; binds
(when (boundp 'eww-mode-map)
  (progn
    (define-key eww-mode-map (kbd "C-j") 'eww-follow-link)))

(when (require 'browse-url nil t)
  (progn
    ;; custom

    ;; the name of the browser program used by ‘browse-url-generic’.
    (customize-set-variable 'browse-url-generic-program "eww")

    ;; function to display the current buffer in a WWW browser: eww
    (customize-set-variable 'browse-url-browser-function 'eww-browse-url)))

(require 'helm-ag nil t)

(when (require 'ispell nil t)
  (progn
    ;; custom
    ;; program invoked by M-x ispell-word and M-x ispell-region commands.
    (customize-set-variable 'ispell-program-name "aspell")))

;; add display-buffer-alist
;; (add-to-list 'display-buffer-alist
;;              '("\\*Choices\\*" display-buffer-below-selected))

(when (require 'flyspell nil t)
  (progn
    ;; custom
    ;; string that is the name of the default dictionary
    (customize-set-variable 'flyspell-default-dictionary "english")

    ;; hooks
    (add-hook 'text-mode-hook 'flyspell-mode)
    (add-hook 'prog-mode-hook 'flyspell-prog-mode)))

(when (require 'flycheck nil t)
  (progn
    ;; binds
    (define-key eos-sc-map (kbd "C-g") 'keyboard-quit)
    (define-key eos-sc-map (kbd "m") 'flycheck-mode)
    (define-key eos-sc-map (kbd "M") 'flycheck-manual)
    (define-key eos-sc-map (kbd "o") 'flycheck-list-errors)
    (define-key eos-sc-map (kbd "b") 'flycheck-buffer)

    (define-key eos-sc-map
      (kbd "v") 'flycheck-verify-setup)

    (define-key eos-sc-map
      (kbd "c") 'flycheck-select-checker)

    (define-key eos-sc-map
      (kbd "d") 'flycheck-disable-checker)

    (define-key eos-sc-map
      (kbd "?") 'flycheck-describe-checker)

    ;; init flycheck mode after some programming mode
    ;; is activated (c-mode, elisp-mode, etc).
    (add-hook 'prog-mode-hook 'flycheck-mode)))

(when (require 'helm-flycheck nil t)
  (progn
    ;; binds
    (define-key eos-sc-map (kbd "e") 'helm-flycheck)
    (define-key ctl-x-map (kbd ";") 'helm-flycheck)))

;; auxiliary function
(defun eos/flycheck/set-checker (checker)
  "Set flycheck CHECKER variable."
  (make-local-variable 'flycheck-checker)
  (customize-set-variable 'flycheck-checker checker))

;; bind eos-sc-map prefix to C-x e
(define-key ctl-x-map (kbd "e") 'eos-sc-map)

;; function (reference)
;; (defun eos/ispell/switch-dictionary ()
;;   "Switch dictionaries."
;;   (interactive)
;;   (let* ((dic ispell-current-dictionary)
;;          (change (if (string= dic "english") "brasileiro" "english")))
;;     (ispell-change-dictionary change)
;;     (message "Dictionary switched from %s to %s" dic change)))))

(when (require 'verb nil t)
  (progn
    ;; hooks
    (add-hook 'org-ctrl-c-ctrl-c-hook
              (lambda ()
                (when (boundp 'verb-mode)
                  (if verb-mode
                      (eos/funcall 'verb-send-request-on-point 'this-window)))))
    ))

(when (require 'diff nil t)
  (progn
    ;; custom
    ;; a string or list of strings specifying switches to be passed to diff
    (customize-set-variable 'diff-switches "-u")))

(when (require 'ediff nil t)
  (progn
    ;; custom
    ;; options to pass to `ediff-custom-diff-program'.
    (customize-set-variable 'ediff-custom-diff-options "-U3")

    ;; the function used to split the main window between buffer-A and buffer-B
    (customize-set-variable 'ediff-split-window-function 'split-window-horizontally)

    ;; function called to set up windows
    (customize-set-variable 'ediff-window-setup-function 'ediff-setup-windows-plain)

    ;; hooks
    (add-hook 'ediff-startup-hook 'ediff-toggle-wide-display)
    (add-hook 'ediff-cleanup-hook 'ediff-toggle-wide-display)
    (add-hook 'ediff-suspend-hook 'ediff-toggle-wide-display)))

(when (require 'helm-external nil t)
  (progn
    ;; bind (C-x) prefix map
    (define-key ctl-x-map (kbd "C-l") 'helm-run-external-command)))

(when (require 'comint nil t)
  (progn
    ;; custom
    ;; if non-nil, assume that the subprocess echoes any input.
    (customize-set-variable 'comint-process-echoes t)

    ;; if non-nil, use comint-prompt-regexp to recognize prompts.
    (customize-set-variable 'comint-use-prompt-regexp t)

    ;; regexp to recognize prompts in the inferior process.
    ;; (customize-set-variable 'comint-prompt-regexp ".*:.*>.*? ")

    ;; value to use for TERM when the system uses terminfo.
    (customize-set-variable 'comint-terminfo-terminal "eterm-color")))

(when (require 'ielm nil t)
  (progn
    ;; custom
    ;; if non-nil, after entering the first line of
    ;; an incomplete sexp, a newline will be inserted after the prompt.
    (customize-set-variable 'ielm-dynamic-multiline-inputs t)

    ;; if non-nil, IELM will beep on error
    (customize-set-variable 'ielm-noisy nil)

    ;; prompt used in IELM
    (customize-set-variable 'ielm-prompt "elisp > ")

    ;; if non-nil, the IELM prompt is read only
    (customize-set-variable 'ielm-prompt-read-only nil)))

(when (require 'sql nil t)
  (progn
    ;; custom
    ;; select the SQL database product used
    (customize-set-variable 'sql-product "sqlite")))

;; start compton after emacs initialize
(add-hook 'after-init-hook
          (lambda ()
            (eos/run/proc "compton")))

(defun eos/transset-set (opacity)
  "Set transparency on frame window specify by OPACITY."
  (interactive "nOpacity: ")
  (let ((opacity (or opacity 1.0)))
    (if (executable-find "transset")
        (async-shell-command (format "transset -a %.1f" opacity))
      (message "transset not found"))))

;; hooks
(add-hook 'after-make-frame-functions
          (lambda (frame)
            (interactive)
            (eos/transset-set 0.9)))

;; init after exwm
(add-hook 'exwm-init-hook
          (lambda ()
            (interactive)
            (eos/transset-set 0.9)))

(if (fboundp 'helm-calcul-expression)
    (define-key ctl-x-map (kbd "C-/") 'helm-calcul-expression))

(when (require 'tramp nil t)
  (progn
    ;; custom
    ;; set tramp default method
    (customize-set-variable 'tramp-default-method "ssh")

    ;; if non-nil, chunksize for sending input to local process.
    ;; (customize-set-variable 'tramp-chunksize 512)

    ;; a value of t would require an immediate reread during filename completion,
    ;; nil means to use always cached values for the directory contents.
    (customize-set-variable 'tramp-completion-reread-directory-timeout nil)

    ;; set tramp verbose level
    (customize-set-variable 'tramp-verbose 4)

    ;; file which keeps connection history for tramp connections.
    (customize-set-variable
     'tramp-persistency-file-name
     (concat (expand-file-name user-emacs-directory) "cache/tramp"))

    ;; connection timeout in seconds
    (customize-set-variable 'tramp-connection-timeout 60)))

(require 'helm-tramp nil t)

(define-key ctl-x-map (kbd "<end>")
  (lambda ()
    (interactive)
    (eos/run/proc "slock")))

(global-set-key (kbd "<print>")
                (lambda ()
                  (interactive)
                  (eos/run/proc "scrot")))

;; control functions: volume
;; (defun eos/toggle-audio ()
;;   "Toggle audio (mute or unmute)."
;;   (interactive)
;;   (async-shell-command "amixer -D default set Master"))

(defun eos/raise-volume ()
  "Raise the volume (factor +5)."
  (interactive)
  (async-shell-command "amixer -D default set Master 5+ unmute"))

(defun eos/lower-volume ()
  "Lower the volume (factor -5)."
  (interactive)
  (async-shell-command "amixer -D default set Master 5- unmute"))

;; bind
;; (define-key ctl-x-map (kbd "C-0") 'eos/toggle-audio)
(global-set-key (kbd "s--") 'eos/lower-volume)
(global-set-key (kbd "s-=") 'eos/raise-volume)

(require 'helm-youtube nil t)

;; binds
(when (boundp 'helm-command-map)
  (progn
    (define-key helm-command-map (kbd "m") 'helm-youtube)))

(when (require 'emms nil t)
  (progn
    ;; the 'emms-setup' feature is provided by the file 'emms-setup.el'
    (require 'emms-setup nil t)

    ;; custom
    ;; list of players that emms can use (only mpv)
    (customize-set-variable 'emms-player-list '(emms-player-mpv))

    ;; the default directory to look for media files.
    (customize-set-variable
     'emms-source-file-default-directory (expand-file-name "~/media"))

    ;; hooks
    ;; disable emms mode line
    (add-hook 'emms-playlist-mode-hook
              (lambda ()
                (when (and (boundp 'emms-mode-line-active-p)
                           (fboundp 'emms-mode-line-disable))
                  (progn
                    (if emms-mode-line-active-p
                        (emms-mode-line-disable))))))))

;; if emms is available, enable it
(when (and (fboundp 'emms-all)
           (fboundp 'emms-default-players))
  (progn
    (funcall 'emms-all)
    (funcall 'emms-default-players)))

;; custom
;; when non-nil, fontify code in code blocks
(customize-set-variable 'org-src-fontify-natively t)

;; if non-nil, the effect of TAB in a code block is as if it were
;; issued in the language major mode buffer
(customize-set-variable 'org-src-tab-acts-natively t)

;; indentation for the content of a source code block.
(customize-set-variable 'org-edit-src-content-indentation 0)

;; confirm before evaluation
(customize-set-variable 'org-confirm-babel-evaluate nil)

;; how the source code edit buffer should be displayed
(customize-set-variable 'org-src-window-setup 'current-window)

;; non-nil means C-a and C-e behave specially in headlines and items
(customize-set-variable 'org-special-ctrl-a/e t)

(add-hook 'org-mode-hook
          (lambda ()
            ;; do not truncate lines
            (setq truncate-lines nil)

            ;; set company backends
            (eos/company/set-backends
             '((company-ispell
                company-yasnippet
                company-dabbrev
                company-dabbrev-code)
               (company-files)))))

;; binds
(define-key org-mode-map (kbd "C-M-i") 'eos/company-or-indent)

(require 'tex-mode nil t)

(when (require 'text-mode nil t)
  (progn
    ;; binds
    (define-key text-mode-map (kbd "C-c C-g") 'keyboard-quit)
    (define-key text-mode-map (kbd "TAB") 'eos/company-or-indent)
    (define-key text-mode-map (kbd "C-M-i") 'eos/helm-company)

    (define-key text-mode-map (kbd "C-c C-k") 'with-editor-cancel)
    (define-key text-mode-map (kbd "C-c C-c") 'with-editor-finish)

    ;; text mode hook
    (add-hook 'text-mode-hook
              (lambda ()
                ;; turn on auto fill mode
                (turn-on-auto-fill)

                ;; set company backends
                (eos/company/set-backends
                 '((company-ispell
                    company-dabbrev)
                   (company-files)))))))

(when (require 'markdown-mode nil t)
  (progn
    ;; custom
    (customize-set-variable 'markdown-command "multimarkdown")))

;; binds
(when (boundp 'markdown-mode-map)
  (progn
    (define-key markdown-mode-map (kbd "TAB") 'eos/complete-or-indent)))

(require 'notifications nil t)

(require 'eldoc nil t)

(when (require 'man nil t)
  (progn
    ;; hooks
    (add-hook 'Man-mode-hook
              (lambda ()
                ;; don't truncate lines
                (setq truncate-lines nil)))))

;; binds
(when (boundp 'Man-mode-map)
  (progn
    (define-key Man-mode-map (kbd "C-j") 'push-button)))

(when (require 'helm-man nil t)
  (progn
    ;; bind
    (define-key eos-docs-map (kbd "m") 'helm-man-woman)))

(when (require 'dash-docs nil t)
  (progn
    ;; custom (fix async?)
    ;; (customize-set-variable
    ;;  'dash-docs-use-workaround-for-emacs-bug t)

    ;; bind
    (define-key eos-docs-map (kbd "u") 'dash-docs-update-docset)))

(when (require 'helm-dash nil t)
  (progn
    ;; disable helm dash debug
    (customize-set-variable 'helm-dash-enable-debugging nil)

    ;; set browser function
    (customize-set-variable 'helm-dash-browser-func 'eww)

    ;; binds
    (define-key eos-docs-map (kbd "l") 'helm-dash)
    (define-key eos-docs-map (kbd "p") 'helm-dash-at-point)
    (define-key eos-docs-map (kbd "i") 'helm-dash-install-docset)
    (define-key eos-docs-map (kbd "a") 'helm-dash-activate-docset)))

;; activate docset
(defun eos/dash/activate-docset (docset)
  "Activate a DOCSET, if available."
  (when (fboundp 'helm-dash-activate-docset)
    (funcall 'helm-dash-activate-docset docset)))

(when (require 'rfc-mode nil t)
  (progn
    ;; custom
    ;; the directory where RFC documents are stored
    (customize-set-variable
     'rfc-mode-directory (concat (expand-file-name user-emacs-directory) "rfc/"))))

;; bind documentation related functions on eos-docs-map
(define-key eos-docs-map (kbd "C-g") 'keyboard-quit)

;; bind eos-docs-map under ctl-x-map
(define-key ctl-x-map (kbd "l") 'eos-docs-map)

(when (require 'company nil t)
  (progn
    ;; set echo delay
    (customize-set-variable 'company-echo-delay .01)

    ;; idle delay in seconds until completion starts automatically
    (customize-set-variable 'company-idle-delay nil)

    ;; maximum number of candidates in the tooltip
    (customize-set-variable 'company-tooltip-limit 10)

    ;; set minimum prefix length
    (customize-set-variable 'company-minimum-length 2)

    ;; if enabled, selecting item before first or after last wraps around
    (customize-set-variable 'company-selection-wrap-around t)

    ;; sort by frequency
    (customize-set-variable 'company-transformers
                            '(company-sort-by-occurrence))

    ;; whether to downcase the returned candidates.
    (customize-set-variable 'company-dabbrev-downcase nil)

    ;; if enabled, disallow non-matching input
    (customize-set-variable 'company-require-match nil)

    ;; When non-nil, align annotations to the right tooltip border
    (customize-set-variable 'company-tooltip-align-annotations nil)

    ;; show candidates number
    ;; to select completions use: M-1, M-2, etc..
    (customize-set-variable 'company-show-numbers t)

    ;; binds (prefix eos-complete map)
    (define-key eos-complete-map (kbd "M-`") 'company-ispell)
    (define-key eos-complete-map (kbd "1") 'company-yasnippet)
    (define-key eos-complete-map (kbd "g") 'company-gtags)
    (define-key eos-complete-map (kbd "f") 'company-files)))

;; enable globally
(eos/funcall 'global-company-mode 1)

;; binds
(when (boundp 'company-active-map)
  (progn
    (define-key company-active-map (kbd "TAB") 'company-complete-common)
    (define-key company-active-map (kbd "C-j") 'company-complete-selection)
    (define-key company-active-map (kbd "C-n") 'company-select-next)
    (define-key company-active-map (kbd "C-p") 'company-select-previous)))

(when (require 'company-statistics nil t)
  (progn
    ;; custom
    ;; set company-statistics cache location
    (customize-set-variable
     'company-statistics-file
     (concat user-emacs-directory "cache/company-statistics-cache.el"))))

(when (require 'yasnippet nil t)
  (progn
    ;; binds
    (define-key eos-complete-map (kbd "y") 'yas-expand)
    (define-key eos-complete-map (kbd "i") 'yas-insert-snippet)
    (define-key eos-complete-map (kbd "v") 'yas-visit-snippet-file)))

;; binds
(when (boundp 'yas-keymap)
  (progn
    (define-key yas-keymap (kbd "<tab>") nil)
    (define-key yas-keymap (kbd "ESC") 'yas-next-field)))

;; enable
(eos/funcall 'yas-global-mode 1)

(when (require 'helm-company nil t)
  (progn
    ;; custom
    ;; enable fuzzy matching for helm company
    (customize-set-variable 'helm-company-fuzzy-match t)))

(when (boundp 'helm-company-map)
  (define-key helm-company-map (kbd "SPC") 'helm-keyboard-quit)
  (define-key helm-company-map (kbd "TAB") 'helm-maybe-exit-minibuffer)
  (define-key helm-company-map (kbd "C-j") 'helm-maybe-exit-minibuffer))

;; set company backends
(defun eos/company/set-backends (backends)
  "Set company BACKENDS."
  (make-local-variable 'company-backends)
  (when (boundp 'company-backends)
    (setq company-backends backends)))

;; helm-company or indent
(defun eos/helm-company ()
  "Helm company wrapper."
  (interactive)
  (when (fboundp 'helm-company)
    (helm-company)))

;; company or indent
(defun eos/company-or-indent ()
  "Complete or indent (TAB)."
  (interactive)
  (if (looking-at "\\_>")
      (progn
        (when (fboundp 'helm-company)
          (helm-company)))
    (indent-according-to-mode)))

;; exit, keyboard quit
(define-key eos-complete-map (kbd "C-g") 'keyboard-quit)

;; testing
(define-key eos-complete-map (kbd "M-/") 'hippie-expand)
;; (define-key eos-complete-map (kbd "<tab>") 'eos/complete-or-indent)

;; binds (global)
(global-set-key (kbd "<M-tab>") 'eos/helm-company)
(global-set-key (kbd "TAB") 'eos/company-or-indent)

;; binds eos-complete-map prefix M-] map
(define-key ctl-x-map (kbd "TAB") 'eos-complete-map)

(when (require 'helm-gtags nil t)
  (progn
    ;; custom
    (customize-set-variable 'helm-gtags-ignore-case t)
    (customize-set-variable 'helm-gtags-auto-update t)
    (customize-set-variable 'helm-gtags-pulse-at-cursor t)
    (customize-set-variable 'helm-gtags-use-input-at-cursor t)
    (customize-set-variable 'helm-gtags-suggested-key-mapping t)

    ;; binds
    (define-key eos-tags-map (kbd "t") 'helm-gtags-dwim)
    (define-key eos-tags-map (kbd "s") 'helm-gtags-select)
    (define-key eos-tags-map (kbd "f") 'helm-gtags-find-tag)
    (define-key eos-tags-map (kbd "+") 'helm-gtags-show-stack)
    (define-key eos-tags-map (kbd "a") 'helm-gtags-parse-file)
    (define-key eos-tags-map (kbd "c") 'helm-gtags-create-tags)
    (define-key eos-tags-map (kbd "u") 'helm-gtags-update-tags)
    (define-key eos-tags-map (kbd "p") 'helm-gtags-find-pattern)
    (define-key eos-tags-map (kbd "r") 'helm-gtags-find-rtag)
    (define-key eos-tags-map (kbd "o") 'helm-gtags-find-tag-other-window)))

;; enable helm-gtags
(eos/funcall 'helm-gtags-mode 1)

;; exit, keyboard quit
(define-key eos-tags-map (kbd "C-g") 'keyboard-quit)

;; ctl-x-map bind (C-x t)
(define-key ctl-x-map (kbd "t") 'eos-tags-map)

(require 'gud nil t)

(when (require 'rmsbolt nil t)
  (progn
    ;; custom
    ;; which output assembly format to use.
    (customize-set-variable 'rmsbolt-asm-format "att")

    ;;    whether we should disassemble an output binary
    (customize-set-variable 'rmsbolt-disassemble t)

    ;; rmsbolt mode lighter
    (customize-set-variable 'rmsbolt-mode-lighter "RMS")))

(when (require 'cmake-ide nil t)
  (progn
    ;; hooks
    (add-hook 'c-mode-hook 'cmake-ide-setup)
    (add-hook 'c++-mode-hook 'cmake-ide-setup)))

(require 'compile nil t)

;; don't truncate lines
(add-hook 'compilation-mode-hook
          (lambda ()
            (setq truncate-lines nil)))

;; fix compilation buffer colors
(add-hook 'compilation-filter-hook
          (lambda ()
            (when (eq major-mode 'compilation-mode)
              (ansi-color-apply-on-region
               compilation-filter-start (point-max)))))

(add-to-list 'load-path
             (concat user-emacs-directory "elpa/helm-compile"))

(require 'helm-compile nil t)

(when (require 'magit nil t)
  (progn
    ;; binds
    (define-key ctl-x-map (kbd "j") 'magit-status)))

(require 'ede nil t)

(when (require 'projectile nil t)
  (progn
    ;; custom
    ;; enable cache and choose indexing method
    (customize-set-variable 'projectile-enable-caching t)
    (customize-set-variable 'projectile-indexing-method 'hybrid)
    (customize-set-variable 'projectile-completion-system 'helm)

    ;; set bookmarks file localtion (cache)
    (customize-set-variable 'projectile-known-projects-file
                            (concat user-emacs-directory "cache/projectile-bookmarks.eld"))

    (customize-set-variable 'projectile-cache-file
                            (concat user-emacs-directory "cache/projectile.cache"))

    ;; binds
    (define-key eos-pm-map (kbd "g") 'projectile-grep)
    (define-key eos-pm-map (kbd "t") 'projectile-find-tag)
    (define-key eos-pm-map (kbd "x") 'projectile-compile-project)
    (define-key eos-pm-map (kbd "!") 'projectile-run-eshell)
    (define-key eos-pm-map (kbd "o") 'projectile-switch-project)
    (define-key eos-pm-map (kbd "r") 'projectile-replace-regexp)
    (define-key eos-pm-map (kbd "R") 'projectile-replace)
    (define-key eos-pm-map (kbd "s") 'projectile-save-project-buffers)
    (define-key eos-pm-map (kbd "d") 'projectile-discover-projects-in-directory)
    (define-key eos-pm-map (kbd "c") 'projectile-cleanup-known-projects)
    (define-key eos-pm-map (kbd "C") 'projectile-invalidate-cache)
    (define-key eos-pm-map (kbd "U") 'projectile-purge-dir-from-cache)
    (define-key eos-pm-map (kbd "u") 'projectile-purge-file-from-cache)
    (define-key eos-pm-map (kbd ".") 'projectile-edit-dir-locals)
    (define-key eos-pm-map (kbd "k") 'projectile-kill-buffers)
    (define-key eos-pm-map (kbd "D") 'projectile-remove-known-project)))

;; enable
(eos/funcall 'projectile-mode)

(when (require 'helm-projectile nil t)
  (progn
    ;; binds
    (define-key eos-pm-map (kbd "p") 'helm-projectile-ag)
    (define-key eos-pm-map (kbd "n") 'helm-projectile-recentf)
    (define-key eos-pm-map (kbd "/") 'helm-projectile-find-dir)
    (define-key eos-pm-map (kbd "f") 'helm-projectile-find-file)
    (define-key eos-pm-map (kbd "b") 'helm-projectile-browse-dirty-projects)
    (define-key eos-pm-map (kbd "a")
      'helm-projectile-find-file-in-known-projects)

    ;; helm-swoop
    (define-key eos-pm-map (kbd "S") 'helm-multi-swoop-projectile)

    ;; dwin
    (define-key eos-pm-map (kbd "w") 'helm-projectile-find-file-dwim)

    ;; hooks
    (add-hook 'projectile-mode-hook 'helm-projectile-on)))

;; exit, keyboard quit
(define-key eos-pm-map (kbd "C-g") 'keyboard-quit)

;; set ctl-x-map prefix (C-x p)
(define-key ctl-x-map (kbd "p") 'eos-pm-map)

;; c/c++ garage
(defun eos/cc/set-company-backends ()
  "Set C/C++ common company backends."
  (eos/company/set-backends
   '((company-c-headers)
     (company-irony
      company-yasnippet
      company-dabbrev-code)
     (company-files))))

(when (require 'cc-mode nil t)
  (progn
    ;; hooks
    (add-hook 'c-mode-hook
              (lambda ()
                ;; set cc common company backends
                (eos/cc/set-company-backends)

                ;; set dash docset
                (eos/dash/activate-docset '"C")

                ;; set flycheck checker
                (eos/flycheck/set-checker 'c/c++-clang)

                ;; load rtags
                (eos/cc/load-rtags)))

    (add-hook 'c++-mode-hook
              (lambda ()
                ;; set cc common backends (company and flycheck)
                (eos/cc/set-company-backends)

                ;; set flycheck checker
                (eos/flycheck/set-checker 'c++-cppcheck)

                ;; set dash docset
                (eos/dash/activate-docset '"C++")

                ;; load rtags
                (eos/cc/load-rtags)))))

;; binds
(when (boundp 'c-mode-map)
  (progn
    ;; set rtags prefix map in c-mode map (C-c r)
    (define-key c-mode-map (kbd "C-c r") 'eos-rtags-map)

    ;; complete or indent
    (define-key c-mode-map (kbd "TAB") 'eos/complete-or-indent)
    (define-key c-mode-map (kbd "C-M-i") 'eos/company-or-indent)))

(defun eos/cc/load-rtags ()
  "Load rtags manually."
  (eos/load-file (concat user-emacs-directory "rtags/src/rtags.el"))

  ;; load helm-rtags
  (eos/load-file (concat user-emacs-directory "rtags/src/helm-rtags.el"))

  ;; set rtags binary path
  (customize-set-variable
   'rtags-path
   (concat user-emacs-directory "rtags/build/bin/"))

  ;; set helm as the frontend
  (customize-set-variable 'rtags-display-result-backend 'helm)
  (customize-set-variable 'rtags-completing-read-behavior 'helm))

(when (require 'irony nil t)
  (progn
    ;; irony hooks
    (add-hook 'irony-mode-hook 'electric-pair-mode)

    ;; mode hooks (init)
    (add-hook 'c-mode-hook 'irony-mode)
    (add-hook 'c++-mode-hook 'irony-mode)
    (add-hook 'objc-mode-hook 'irony-mode)))

(when (require 'irony-cdb nil t)
  (progn
    ;; hooks
    (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)))

(when (require 'company-irony nil t)
  (progn
    ;; hooks
    (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)))

(require 'company-c-headers nil t)

;; eos rtags prefix map (todo revise)
(define-key eos-rtags-map (kbd "C-g") 'keyboard-quit)
(define-key eos-rtags-map (kbd "l") 'rtags-taglist)
(define-key eos-rtags-map (kbd "I") 'rtags-install)
(define-key eos-rtags-map (kbd "y") 'rtags-symbol-type)
(define-key eos-rtags-map (kbd "l") 'rtags-symbol-info)
(define-key eos-rtags-map (kbd "n") 'rtags-rename-symbol)
(define-key eos-rtags-map (kbd "m") 'rtags-asm-file)
(define-key eos-rtags-map (kbd "h") 'rtags-find-file-history)
(define-key eos-rtags-map (kbd "x") 'rtags-fixit)
(define-key eos-rtags-map (kbd "d") 'rtags-diagnostics)
(define-key eos-rtags-map (kbd "c") 'rtags-compile-file)
(define-key eos-rtags-map (kbd "-") 'rtags-compilation-flags)
(define-key eos-rtags-map (kbd ".") 'rtags-find-functions-called-by-this-function)

;; (define-key eos-rtags-map (kbd "r") 'rtags-find-references-at-point)
;; (define-key eos-rtags-map (kbd "p") 'rtags-find-all-references-at-point)
;; (define-key eos-rtags-map (kbd "i") 'rtags-imenu)
;; (define-key eos-rtags-map (kbd "t") 'rtags-find-symbol-at-point)
;; (define-key eos-rtags-map (kbd "s") 'rtags-find-symbol)

(when (require 'lisp-mode nil t)
  (progn
    ;; custom
    ;; number of columns to indent the second line of a (def...) form
    (customize-set-variable 'lisp-body-indent 2)))

(when (require 'elisp-mode nil t)
  (progn
    ;; hooks
    ;; enable minor modes
    (add-hook 'emacs-lisp-mode-hook
              (lambda()
                (eos/funcall 'eldoc-mode 1)))

    (add-hook 'lisp-interaction-mode-hook
              (lambda()
                (eos/funcall 'eldoc-mode 1)))

    ;; set backends
    (add-hook 'emacs-lisp-mode-hook
              (lambda ()
                ;; set company backends
                (eos/company/set-backends
                 '((company-elisp
                    company-yasnippet
                    company-dabbrev-code)
                   (company-files)))

                ;; set flycheck checker
                (eos/flycheck/set-checker 'emacs-lisp)

                ;; activate dash docset (emacs)
                (eos/dash/activate-docset "Emacs Lisp")))))

;; binds
(when (boundp 'emacs-lisp-mode-map)
  (progn
    ;; eval keybinds
    (define-key emacs-lisp-mode-map (kbd "C-c C-f") 'eval-defun)
    (define-key emacs-lisp-mode-map (kbd "C-c C-r") 'eval-region)
    (define-key emacs-lisp-mode-map (kbd "C-c C-c") 'eval-buffer)

    ;; complete keybind
    (define-key emacs-lisp-mode-map (kbd "<tab>") 'eos/company-or-indent)

    ;; ubind, qualaty of life
    (define-key emacs-lisp-mode-map (kbd "DEL") 'nil)
    (define-key emacs-lisp-mode-map (kbd "ESC") 'nil)
    (define-key emacs-lisp-mode-map (kbd "C-x") 'nil)
    (define-key emacs-lisp-mode-map (kbd "C-M-x") 'nil)
    (define-key emacs-lisp-mode-map (kbd "C-M-q") 'nil)))

(require 'company-elisp nil t)

(when (require 'sh-script nil t)
  (progn
    ;; hooks
    (add-hook 'sh-mode-hook
              (lambda ()
                ;; set company backends
                (eos/company/set-backends
                 '((company-shell
                    company-shell-env
                    company-yasnippet
                    company-dabbrev-code)
                   (company-files)))

                ;; set flycheck backends
                (eos/flycheck/set-checker 'sh-shellcheck)))))

(require 'company-shell nil t)

(when (require 'fish-mode nil t)
  (progn
    ;; hooks
    (add-hook 'fish-mode-hook
              (lambda ()
                ;; set company backends
                (eos/company/set-backends
                 '((company-fish-shell
                    company-yasnippet
                    company-shell
                    company-shell-env
                    company-dabbrev-code)
                   (company-files)))))))

(when (require 'lua-mode nil t)
  (progn
    ;; custom
    ;; non-nil means display lua-process-buffer after sending a command.
    (customize-set-variable 'lua-process-buffer t)

    ;; default application to run in Lua process
    (customize-set-variable 'lua-default-application "lua")

    ;; command switches for lua-default-application
    (customize-set-variable 'lua-default-command-switches "-i")

    ;; amount by which Lua subexpressions are indented
    (customize-set-variable 'lua-indent-level 4)

    ;; if non-nil, contents of multiline string will be indented
    (customize-set-variable 'lua-indent-string-contents t)

    ;; jump to innermost traceback location in *lua* buffer
    ;; when this variable is non-nil and a traceback occurs
    ;; when running Lua code in a process, jump immediately
    ;; to the source code of the innermost traceback location
    (customize-set-variable 'lua-jump-on-traceback t)

    ;; hooks
    (add-hook 'lua-mode-hook
              (lambda ()
                ;; set company backends
                (eos/company/set-backends
                 '((company-yasnippet
                    company-keywords
                    company-gtags
                    company-dabbrev
                    company-dabbrev-code
                    company-keywords)
                   (company-files)))

                ;; set flycheck checker
                (eos/flycheck/set-checker 'lua)

                ;; activate dash docset
                (eos/dash/activate-docset "Lua")))

    ;; add auto-mode
    (add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
    (add-to-list 'interpreter-mode-alist '("lua" . lua-mode))))

(require 'tcl nil t)

(require 'cperl-mode nil t)

(when (require 'python nil t)
  (progn
    ;; custom
    ;; default Python interpreter for shell
    (customize-set-variable 'python-shell-interpreter "python2.7")

    ;; non-nil means template skeletons will be automagically inserted
    (customize-set-variable 'python-skeleton-autoinsert t)

    ;; hooks
    ;; enable modes
    (add-hook 'python-mode-hook
              (lambda()
                ;; enable eldoc mode
                (eos/funcall 'eldoc-mode 1)))

    ;; set backends
    (add-hook 'python-mode-hook
              (lambda ()
                ;; set company backends
                (eos/company/set-backends
                 '((company-yasnippet
                    company-keywords
                    company-dabbrev-code)
                   (company-files)))

                ;; set flycheck checker
                (eos/flycheck/set-checker 'python-pycompile)

                ;; set dash docsets
                (eos/dash/activate-docset '"Python 3")))))

(when (require 'go-mode nil t)
  (progn
    ;; add (*.go . go-mode) to auto-mode-alist
    ;; init go-mode when a file with the extersion .go is opened
    (add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))

    ;; hooks
    (add-hook 'go-mode-hook
              (lambda ()
                ;; set company backends
                (eos/company/set-backends
                 '((company-yasnippet
                    company-keywords
                    company-dabbrev-code)
                   (company-files)))

                ;; set flycheck checker (go lint)
                (eos/flycheck/set-checker 'go-golint)

                ;; set dash docsets
                (eos/dash/activate-docset '"Go")))))

(require 'ess-r-mode nil t)

(require 'julia-mode nil t)

(require 'ess-julia nil t)

(require 'vhdl-mode nil t)

(require 'verilog nil t)

(require 'cmake-mode nil t)

(add-to-list 'load-path
             (concat user-emacs-directory "elpa/mql-mode"))

(when (require 'mql-mode nil t)
  (progn
    ;; hooks
    (add-hook 'mql-mode-hook
              (lambda ()
                ;; set company backends
                (eos/company/set-backends
                 '((company-gtags
                    company-yasnippet
                    company-dabbrev
                    company-dabbrev-code)
                   (company-files)))

                ;; select flycheck checker (use gcc)
                (eos/flycheck/set-checker 'c/c++-gcc)

                ;; activate mql5 docset
                (eos/dash/activate-docset '"mql5")))))

(when (require 'highlight-doxygen nil t)
  (progn
    ;; add doxygen
    (add-hook 'prog-mode-hook 'highlight-doxygen-global-mode)))

(when (require 'web-mode nil t)
  (progn
    ;; add files extensions to web-mode
    (add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))))

(when (boundp 'web-mode-engines-alist)
  (progn
    (add-to-list 'web-mode-engines-alist '(("php" . "\\.phtml\\'")))))

;; clean esc map
(define-key esc-map (kbd "ESC") nil)
(define-key esc-map (kbd "<f10>") nil)

;; unbind
;; (define-key ctl-x-map (kbd "C-SPC") nil)
;; (define-key ctl-x-map (kbd "C-=") nil)
;; (define-key ctl-x-map (kbd "C-0") nil)
;; (define-key ctl-x-map (kbd "C-z") nil)
;; (define-key ctl-x-map (kbd "C--") nil)
;; (define-key ctl-x-map (kbd "ESC") nil)
;; (define-key ctl-x-map (kbd ".") nil)
;; (define-key ctl-x-map (kbd "C-l") nil)
(define-key ctl-x-map (kbd "C-d") nil)
;; (define-key ctl-x-map (kbd "C-x") nil)
(define-key ctl-x-map (kbd "C-j") nil)
(define-key ctl-x-map (kbd "C-<left>") nil)
(define-key ctl-x-map (kbd "C-<right>") nil)
(define-key ctl-x-map (kbd "C-<up>") nil)
(define-key ctl-x-map (kbd "C-<down>") nil)
(define-key ctl-x-map (kbd "<right>") nil)
(define-key ctl-x-map (kbd "<left>") nil)
(define-key ctl-x-map (kbd "C-+") nil)
(define-key ctl-x-map (kbd "C-a") nil)
(define-key ctl-x-map (kbd "C-r") nil)
(define-key ctl-x-map (kbd "C-n") nil)
(define-key ctl-x-map (kbd "C-p") nil)
;; (define-key ctl-x-map (kbd "C-o") nil)
(define-key ctl-x-map (kbd "C-h") nil)
(define-key ctl-x-map (kbd "C-u") nil)
(define-key ctl-x-map (kbd "C-\@") nil)
(define-key ctl-x-map (kbd "M-:") nil)
(define-key ctl-x-map (kbd "`") nil)
(define-key ctl-x-map (kbd "]") nil)
;; (define-key ctl-x-map (kbd "[") nil)
(define-key ctl-x-map (kbd ")") nil)
(define-key ctl-x-map (kbd "(") nil)
(define-key ctl-x-map (kbd "<") nil)
(define-key ctl-x-map (kbd ">") nil)
(define-key ctl-x-map (kbd "\@") nil)
(define-key ctl-x-map (kbd "-") nil)
(define-key ctl-x-map (kbd ";") nil)
(define-key ctl-x-map (kbd "#") nil)
(define-key ctl-x-map (kbd "*") nil)
(define-key ctl-x-map (kbd "'") nil)
(define-key ctl-x-map (kbd "$") nil)
(define-key ctl-x-map (kbd "{") nil)
(define-key ctl-x-map (kbd "}") nil)
(define-key ctl-x-map (kbd "^") nil)
(define-key ctl-x-map (kbd "n") nil)
(define-key ctl-x-map (kbd "f") nil)
(define-key ctl-x-map (kbd "a") nil)
(define-key ctl-x-map (kbd "h") nil)
(define-key ctl-x-map (kbd "v") nil)
(define-key ctl-x-map (kbd "x") nil)
(define-key ctl-x-map (kbd "X") nil)

;; clean minor-mode-map-alist
(setq minor-mode-map-alist nil)

;; unset
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-@"))
(global-unset-key (kbd "C-\\"))
(global-unset-key (kbd "C-_"))
(global-unset-key (kbd "M-l"))
(global-unset-key (kbd "M-h"))
(global-unset-key (kbd "M-\\"))
(global-unset-key (kbd "M-$"))
(global-unset-key (kbd "M-("))
(global-unset-key (kbd "M-)"))
(global-unset-key (kbd "M-r"))
(global-unset-key (kbd "M-{"))
(global-unset-key (kbd "M-}"))
(global-unset-key (kbd "S-SPC"))
(global-unset-key (kbd "<backtap>"))
(global-unset-key (kbd "M-="))
(global-unset-key (kbd "M-@"))
(global-unset-key (kbd "M-~"))

;; (global-unset-key (kbd "M-z"))
;; (global-unset-key (kbd "M-SPC"))
;; (global-unset-key (kbd "M-m"))
;; (global-unset-key (kbd "M-k"))
;; (global-unset-key (kbd "M-t"))
;; (global-unset-key (kbd "M-q"))

(global-unset-key (kbd "C-M-h"))
(global-unset-key (kbd "C-M-j"))
(global-unset-key (kbd "C-M-."))
(global-unset-key (kbd "C-M-l"))
(global-unset-key (kbd "C-M-/"))
(global-unset-key (kbd "C-M-;"))
(global-unset-key (kbd "C-M-@"))
(global-unset-key (kbd "C-M-\\"))
(global-unset-key (kbd "C-M-a"))
(global-unset-key (kbd "C-M-r"))
(global-unset-key (kbd "C-M-s"))
(global-unset-key (kbd "C-M-%"))
(global-unset-key (kbd "C-M-u"))
(global-unset-key (kbd "C-M-d"))
(global-unset-key (kbd "C-M-SPC"))
(global-unset-key (kbd "C-M-S-v"))

(global-unset-key (kbd "<C-M-end>"))
(global-unset-key (kbd "<C-M-home>"))
(global-unset-key (kbd "<C-S-backspace>"))
(global-unset-key (kbd "<C-backspace>"))
(global-unset-key (kbd "<C-delete>"))
(global-unset-key (kbd "<C-down>"))
(global-unset-key (kbd "<C-next>"))
(global-unset-key (kbd "<C-end>"))
(global-unset-key (kbd "<C-f10>"))
(global-unset-key (kbd "<M-f10>"))

(global-unset-key (kbd "<bottom-divider>"))
(global-unset-key (kbd "<bottom-edge>"))
(global-unset-key (kbd "<bottom-left-corner>"))
(global-unset-key (kbd "<bottom-right-corner>"))

(global-unset-key (kbd "<horizontal-scroll-bar>"))
(global-unset-key (kbd "<vertical-scroll-bar>"))

(global-unset-key (kbd "<left-edge>"))
(global-unset-key (kbd "<right-edge>"))

(global-unset-key (kbd "<undo>"))
(global-unset-key (kbd "<find>"))
(global-unset-key (kbd "<help>"))
(global-unset-key (kbd "<open>"))
(global-unset-key (kbd "<again>"))
(global-unset-key (kbd "<menu>"))
(global-unset-key (kbd "<header-line>"))
(global-unset-key (kbd "<mode-line>"))

(global-unset-key (kbd "<XF86Back>"))
(global-unset-key (kbd "<XF86Forward>"))
(global-unset-key (kbd "<XF86WakeUp>"))

(global-unset-key (kbd "<top-edge>"))
(global-unset-key (kbd "<top-left-corner>"))
(global-unset-key (kbd "<top-right-corner>"))

(global-unset-key (kbd "<mouse-1>"))
(global-unset-key (kbd "<mouse-2>"))
(global-unset-key (kbd "<mouse-3>"))
(global-unset-key (kbd "<mouse-4>"))
(global-unset-key (kbd "<mouse-5>"))
(global-unset-key (kbd "<mouse-6>"))
(global-unset-key (kbd "<mouse-7>"))

(global-unset-key (kbd "<right-divider>"))
(global-unset-key (kbd "<vertical-line>"))

(global-unset-key (kbd "<f10>"))
(global-unset-key (kbd "<f11>"))
(global-unset-key (kbd "<f16>"))
(global-unset-key (kbd "<f18>"))
(global-unset-key (kbd "<f20>"))

(global-unset-key (kbd "<drag-mouse-1>"))
(global-unset-key (kbd "<C-mouse-4>"))
(global-unset-key (kbd "<C-mouse-5>"))
(global-unset-key (kbd "<C-mouse-6>"))
(global-unset-key (kbd "<C-mouse-7>"))
(global-unset-key (kbd "<M-mouse-1>"))
(global-unset-key (kbd "<M-mouse-2>"))
(global-unset-key (kbd "<M-mouse-3>"))
(global-unset-key (kbd "<S-mouse-3>"))
(global-unset-key (kbd "<S-mouse-4>"))
(global-unset-key (kbd "<S-mouse-5>"))
(global-unset-key (kbd "<S-mouse-6>"))
(global-unset-key (kbd "<S-mouse-7>"))
(global-unset-key (kbd "<C-down-mouse-1>"))
(global-unset-key (kbd "<C-down-mouse-2>"))
(global-unset-key (kbd "<M-down-mouse-1>"))
(global-unset-key (kbd "<M-drag-mouse-1>"))
(global-unset-key (kbd "<S-down-mouse-1>"))

(require 'eos-adapt
         (expand-file-name "eos-adapt.el" user-emacs-directory) t)
