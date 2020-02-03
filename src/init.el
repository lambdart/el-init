;;; Package --- eos
;;; Commentary:
;;; Code:

;;; -*- lexical-binding: t -*-

(when (version< emacs-version "26.3")
  (error "This requires Emacs 26.3 and above!"))

(when (require 'org nil t)
  (progn
    ;; customize
    (customize-set-variable 'org-src-fontify-natively t)
    (customize-set-variable 'org-src-tab-acts-natively t)
    (customize-set-variable 'org-edit-src-content-indentation 0)
    (customize-set-variable 'org-export-with-smart-quotes t)
    (customize-set-variable 'org-confirm-babel-evaluate nil)
    (customize-set-variable 'org-src-window-setup 'current-window)
    (customize-set-variable 'org-special-ctrl-a/e t)

    ;; load languages
    (org-babel-do-load-languages 'org-babel-load-languages
                                 '((emacs-lisp . t)))))

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

(defun eos/minibuffer/setup-hook ()
  "Set 'gc-cons-threshold most."
  (setq gc-cons-threshold most-positive-fixnum))

(defun eos/minibuffer/exit-hook ()
  "Set 'gc-cons-threshold to 800000 (magic number)."
  (setq gc-cons-threshold 800000))

;; threshold inital value
(let ((gc-cons-threshold most-positive-fixnum)))

;; set threshold to 800000)
(add-hook 'minibuffer-setup-hook 'eos/minibuffer/setup-hook)

;; reset threshold to inital value
(add-hook 'minibuffer-exit-hook 'eos/minibuffer/exit-hook)

;; start emacs server
(server-start)

;; Load private.el after emacs initialize.
(add-hook 'after-init-hook
          (lambda ()
            (interactive)
            (let ((private-file (expand-file-name "~/.private/private.el")))
              (if (file-exists-p private-file)
                  (progn (load-file private-file))))))

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
(global-set-key (kbd "C-<left>") 'backward-word)
(global-set-key (kbd "C-<right>") 'forward-whitespace)

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

(global-set-key (kbd "M-<up>") 'eos/edit/move-lines-up)
(global-set-key (kbd "M-<down>") 'eos/edit/move-lines-down)
;; edit (terminal quick fix)
(global-set-key (kbd "ESC <up>") 'eos/edit/move-lines-up)
(global-set-key (kbd "ESC <down>") 'eos/edit/move-lines-down)

(defun eos/kill-buffer (buffer-name)
  "Kill BUFFER-NAME if exists."
  (when (get-buffer buffer-name)
    (kill-buffer buffer-name)))

(defun eos/kill/current-buffer ()
  "Kill the current buffer without prompting."
  (interactive)
  (kill-buffer (current-buffer)))

(defun eos/run/async-proc (name)
  "Run a asynchronous process defined by NAME."
  (interactive)
  (start-process name nil name))

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

;; always select the help window
(customize-set-variable 'help-window-select t)

(defun eos/set-frame-font (font)
  "Set the default font to FONT."
  (cond ((find-font (font-spec :name font))
         (set-frame-font font nil t))))

;; hermit light
(defvar eos-font "Hermit Light:pixelsize=18"
  "Default eos font.")

;; set frame font
(eos/set-frame-font eos-font)

;; set font by face attribute (reference)
;; (set-face-attribute 'default nil :height)

;; disable fringe
(add-hook 'after-init-hook
          (lambda ()
            (set-fringe-style 1)))

;; autosave/backups options
(customize-set-variable 'version-control t)
(customize-set-variable 'kept-new-versions 6)
(customize-set-variable 'backup-by-copying t)
(customize-set-variable 'kept-old-versions 2)
(customize-set-variable 'delete-old-versions t)
(customize-set-variable 'make-backup-files nil)
(customize-set-variable 'auto-save-default nil)

;; set backup directory list
(customize-set-variable
 'backup-directory-alist
 '(("" . (concat user-emacs-directory "backup"))))

;; set autosave locations and format
(customize-set-variable
 'auto-save-list-file-prefix
 (concat user-emacs-directory "backup/.saves-"))

;; create cache directory
(mkdir (concat user-emacs-directory "cache") t)

;; recentf location
(customize-set-variable
 'recentf-save-file
 (concat user-emacs-directory "cache/recentf"))

;; bookmark file location
(customize-set-variable
 'bookmark-default-file
 (concat user-emacs-directory "cache/bookmarks"))

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

;; fullscreen
(add-to-list 'initial-frame-alist '(fullscreen . fullheight))

;; internal border
(add-to-list 'default-frame-alist '(internal-border-width . 2))

;; bind
;; (global-set-key (kbd "M-z") 'other-frame)

;; scroll options
(customize-set-variable 'scroll-margin 0)
(customize-set-variable 'scroll-conservatively 100)
(customize-set-variable 'scroll-preserve-screen-position 1)

;; never show dialogs box
(customize-set-variable 'use-dialog-box nil)

;; window move default keybinds (shift-up/down etc..)
(add-hook 'after-init-hook 'windmove-default-keybindings)

;; set window margins
(customize-set-variable 'left-margin-width 1)
(customize-set-variable 'right-margin-width 1)

;; set window divider
(add-hook 'after-init-hook 'window-divider-mode)

;; global bind
(global-set-key (kbd "<s-left>") 'shrink-window-horizontally)
(global-set-key (kbd "<s-right>") 'enlarge-window-horizontally)
(global-set-key (kbd "<s-down>") 'shrink-window)
(global-set-key (kbd "<s-up>") 'enlarge-window)

;; eos-window-map bind
;; (define-key eos-window-map (kbd "j") 'windmove-up)
;; (define-key eos-window-map (kbd "k") 'windmove-down)
;; (define-key eos-window-map (kbd "h") 'windmove-left)
;; (define-key eos-window-map (kbd "l") 'windmove-right)

(define-key eos-window-map (kbd "1") 'maximize-window)
(define-key eos-window-map (kbd "q") 'minimize-window)
(define-key eos-window-map (kbd "w") 'balance-windows)

;; ctl-x-map bind (C-x w)
(define-key ctl-x-map (kbd "w") 'eos-window-map)

;; linum format
(customize-set-variable 'linum-format " %2d ")

;; truncate lines
(customize-set-variable 'truncate-lines nil)

;; Most *NIX tools work best when files are terminated
;; with a newline.
(customize-set-variable 'require-final-newline t)

;; sentences should be separated by a single space,
;; so treat two sentences as two when filling.
(customize-set-variable 'sentence-end-double-space nil)

;; default indent
(customize-set-variable 'tab-width 4)
(customize-set-variable 'indent-tabs-mode nil)

;; kill process not confirmation required
(customize-set-variable
 'kill-buffer-query-functions
 (remq 'process-kill-buffer-query-function kill-buffer-query-functions))

;; delete selection-mode
(eos/funcall 'delete-selection-mode 1)

;; clean whitespace and newlines before buffer save
(add-hook 'before-save-hook 'whitespace-cleanup)

;; prefer newer
(customize-set-variable 'load-prefer-newer t)

;; binds
(define-key ctl-x-map (kbd "C-,") 'previous-buffer)
(define-key ctl-x-map (kbd "C-.") 'next-buffer)

(customize-set-variable 'enable-recursive-minibuffers t)

;; bind kmacro-keymap to C-x m
(define-key ctl-x-map (kbd "m") 'kmacro-keymap)

(global-set-key (kbd "M-c") 'comment-or-uncomment-region)

;; conding-system (utf8)
(customize-set-variable 'locale-coding-system 'utf-8)
(customize-set-variable 'buffer-file-coding-system 'utf-8)

(prefer-coding-system 'utf-8)
(set-language-environment "UTF-8")
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)

(customize-set-variable 'visible-cursor nil)

(when (require 'time nil t)
  (progn
    ;; customize
    ;; seconds between updates of time in the mode line.
    (customize-set-variable 'display-time-interval 15)

    ;; non-nil indicates time should be displayed as hh:mm, 0 <= hh <= 23
    (customize-set-variable 'display-time-24hr-format t)

    ;; set format time string
    (customize-set-variable 'display-time-format "%H:%M")

    ;; load-average values below this value won’t be shown in the mode line.
    (customize-set-variable 'display-time-load-average-threshold 1.0)

    ;; enable
    ;; initialize display time mode
    (display-time-mode 1)))

;; always refresh the modeline (reference)
;; (add-hook 'buffer-list-update-hook 'func)

(customize-set-variable 'eval-expression-print-level nil)

;; file used for storing customization information.
;; The default is nil, which means to use your init file
;; as specified by ‘user-init-file’.  If the value is not nil,
;; it should be an absolute file name.
(setq custom-file (concat (expand-file-name user-emacs-directory) "custom.el"))

;; load custom file
(eos/load-file custom-file)

;; clean startup message/area
(customize-set-variable 'inhibit-startup-screen t)
(customize-set-variable 'inhibit-startup-message nil)
(customize-set-variable 'inhibit-startup-echo-area-message nil)
(customize-set-variable 'inhibit-buffer-choice nil)

(customize-set-variable 'electric-pair-pairs
                        '((?\{ . ?\})
                          (?\( . ?\))
                          (?\[ . ?\])
                          (?\" . ?\")))

;; disabled modes list
(dolist (mode
         '(tool-bar-mode
           tooltip-mode
           menu-bar-mode
           scroll-bar-mode
           blink-cursor-mode))
  (eos/funcall mode 0))

;; enabled modes list
(dolist (mode
         '(show-paren-mode
           column-number-mode
           size-indication-mode
           electric-pair-mode
           global-subword-mode
           global-display-line-numbers-mode
           global-hl-line-mode))
  (eos/funcall mode 1))

;; exit/quit
(define-key ctl-x-map (kbd "C-g") 'keyboard-quit)

;; text scale adjust
(define-key ctl-x-map (kbd "=") 'text-scale-adjust)

;; whitespace-mode
(define-key ctl-x-map (kbd ".") 'whitespace-mode)

;; kill buffer and window
(define-key ctl-x-map (kbd "C-k") 'kill-buffer-and-window)

;; avoid warnings when byte-compile
(eval-when-compile
  (require 'cask "~/.cask/cask.el")
  (cask-initialize))

;; load cask
(require 'cask "~/.cask/cask.el")

;; initialize cask
(cask-initialize)

(require 'async nil t)
(require 'async-bytecomp nil t)

;; to run command without displaying the output in a window
(add-to-list 'display-buffer-alist
             '("\\*Async Shell Command\\*" display-buffer-no-window))

(when (require 'exwm nil t)
  (progn
    (require 'exwm-config nil t)

    ;; set exwm workspaces
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
                              ([?\s-w] . exwm-workspace-switch)
                              ([?\s-q] . exwm-input-toggle-keyboard)
                              ([?\s-z] . multi-term-dedicated-toggle)

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
    ;; set exwm workspaces
    (customize-set-variable 'exwm-workspace-number 2)

    ;; customize monitors
    (customize-set-variable
     'exwm-randr-workspace-monitor-plist '(0 "HDMI-1"))

    (customize-set-variable
     'exwm-randr-workspace-monitor-plist '(1 "DP-1"))

    (add-hook 'exwm-randr-screen-change-hook
              (lambda ()
                (start-process-shell-command
                 "xrandr" nil "xrandr --output HDMI-1 --left-of DP-1 --auto")))

    ;; enable exwm randr
    ;; (exwm-randr-enable)
    ))

(defvar eos/helm-source-exwm-buffers
  nil
  "Helm exwm buffers source.")

(when (require 'helm-exwm nil t)
  (progn
    ;; exwm buffers list
    (setq eos/helm-source-exwm-buffers
          (if (fboundp 'helm-exwm-build-source)
              (helm-exwm-build-source)
            nil))))

(when (require 'helm nil t)
  (progn
    ;; default input idle delay
    (customize-set-variable 'helm-idle-delay 0.01)
    (customize-set-variable 'helm-input-idle-delay 0.01)

    ;; set autoresize max and mim height
    (customize-set-variable 'helm-autoresize-max-height 35)
    (customize-set-variable 'helm-autoresize-min-height 25)

    ;; enable fuzzing matching
    (customize-set-variable 'helm-M-x-fuzzy-match t)
    (customize-set-variable 'helm-imenu-fuzzy-match t)
    (customize-set-variable 'helm-locate-fuzzy-match t)
    (customize-set-variable 'helm-recentf-fuzzy-match t)
    (customize-set-variable 'helm-apropos-fuzzy-match t)
    (customize-set-variable 'helm-lisp-fuzzy-completion t)
    (customize-set-variable 'helm-buffers-fuzzy-matching t)

    ;; save console history
    (customize-set-variable 'helm-M-x-always-save-history t)

    ;; clean details flag
    (customize-set-variable 'helm-buffer-details-flag t)

    ;; split window in side
    (customize-set-variable 'helm-split-window-in-side-p t)

    ;; move in cycles
    (customize-set-variable 'helm-move-to-line-cycle-in-source t)

    ;; set scroll reaching
    (customize-set-variable 'helm-scroll-amount 8)

    ;; show input header
    (customize-set-variable 'helm-echo-input-in-header-line t)

    ;; search for library in 'require' and 'declare-function' sexp.
    (customize-set-variable 'helm-ff-search-library-in-sexp t)

    ;; use 'recentf-list' instead of 'file-name-history' in 'helm-find-files'.
    (customize-set-variable 'helm-ff-file-name-history-use-recentf t)

    ;; handle completion in region
    (customize-set-variable 'helm-mode-handle-completion-in-region t)

    ;; don't display header line
    (customize-set-variable 'helm-display-header-line nil)

    ;; bind (C-x)
    ;; (define-key ctl-x-map (kbd "b") 'helm-buffers-list)
    (define-key ctl-x-map (kbd "C-b") 'helm-mini)
    (define-key ctl-x-map (kbd "C-f") 'helm-find-files)

    ;; bind global map
    (global-set-key (kbd "M-x") 'helm-M-x)
    (global-set-key (kbd "M-y") 'helm-show-kill-ring)
    (global-set-key (kbd "M-m") 'helm-mark-ring)

    ;; init helm mode
    (add-hook 'after-init-hook 'helm-mode)
    (add-hook 'after-init-hook 'helm-autoresize-mode)))

;; bind
(when (boundp 'helm-map)
  (progn
    (define-key helm-map (kbd "TAB") 'helm-execute-persistent-action)
    (define-key helm-map (kbd "C-j") 'helm-maybe-exit-minibuffer)
    (define-key helm-map (kbd "C-z") 'helm-select-action)))

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
            eos/helm-source-nonfile-buffers
            helm-source-recentf
            helm-source-buffers-list
            helm-source-buffer-not-found))))

(when (require 'epa nil t)
  (progn
    ;; customize
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

(require 'notifications nil t)

(when (require 'helm-info nil t)
  (progn
    ;; bind
    (define-key help-map (kbd "C-i") 'helm-info)))

(when (require 'helm-descbinds nil t)
  (progn
    ;; helm-descbinds, window splitting style (2: vertical)
    (customize-set-variable 'helm-descbinds-window-style 2)))

;; bind
(define-key help-map (kbd "b") 'helm-descbinds)

;; unbind (clean, quality of life)
(define-key help-map (kbd "<help>") nil)
(define-key help-map (kbd "<f1>") nil)
(define-key help-map (kbd "C-n") nil)
(define-key help-map (kbd "C-h") nil)
(define-key help-map (kbd "C-;") nil)
(define-key help-map (kbd "K") nil)
(define-key help-map (kbd "RET") nil)

(when (require 'iedit nil t)
  (progn
    ;; if no-nil, the key is inserted into global-map,
    ;; isearch-mode-map, esc-map and help-map.
    (customize-set-variable 'iedit-toggle-key-default nil)))

;; bind
(when (boundp 'iedit-mode-keymap)
  (define-key iedit-mode-keymap (kbd "TAB") 'eos/complete-or-indent))

(when (require 'undo-tree nil t)
  (progn
    ;; define alias for redo
    (defalias 'redo 'undo-tree-redo)

    ;; binds
    (define-key ctl-x-map (kbd "u") 'undo-tree-visualize)

    ;; init after emacs initialize
    (add-hook 'after-init-hook 'global-undo-tree-mode)))

(when (require 'editorconfig nil t)
  (progn
    (add-hook 'after-init-hook 'editorconfig-mode)))

(when (require 'helm-swoop nil t)
  (progn
    ;; customize
    (customize-set-variable 'helm-swoop-speed-or-color nil)
    (customize-set-variable 'helm-swoop-split-with-multiple-windows t)
    (customize-set-variable 'helm-swoop-use-fuzzy-match t)
    (customize-set-variable 'helm-swoop-move-to-line-cycle t)
    (customize-set-variable 'helm-swoop-use-line-number-face t)

    ;; bind global
    (global-set-key (kbd "C-s") 'helm-swoop)))

;; bind helm-swoop-map
(when (boundp 'helm-swoop-map)
  (progn
    (define-key helm-swoop-map (kbd "C-s")
      'helm-multi-swoop-all-from-helm-swoop)

    (define-key helm-swoop-map (kbd "C-c s c")
      'helm-multi-swoop-current-mode-from-helm-swoop)))

(require 'helm-locate nil t)

;; load helm-imenu
(when (require 'helm-imenu nil t)
  (progn
    ;; bind
    (define-key ctl-x-map (kbd "TAB") 'helm-imenu-in-all-buffers)))

;; bind global
;; (global-set-key (kbd "C-M-i") 'helm-imenu-in-all-buffers)))

;; bind
(when (boundp 'helm-imenu-map)
  (progn
    (define-key helm-imenu-map (kbd "C-M-i") 'helm-next-source)))

(when (require 'dired nil t)
  (progn
    ;; enable find-alternate-file
    (put 'dired-find-alternate-file 'disabled nil)))

(when (require 'dired-async nil t)
  (progn
    ;; enable dired-aysnc-mode
    (eos/funcall 'dired-async-mode 1)))

;; binds
(if (boundp 'dired-mode-map)
    (progn
      ;;     (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
      (define-key dired-mode-map (kbd "C-j") 'dired-find-alternate-file)))

(when (require 'dired-sidebar nil t)
  (progn
    ;; customize
    ;; close sidebar when dired-sidebar-find-file it's called
    (customize-set-variable
     'dired-sidebar-close-sidebar-on-file-open t)

    ;; when finding file to point at for
    ;; dired-sidebar-follow-file-at-point-on-toggle-open, use file at point
    ;; in magit buffer.
    (customize-set-variable
     'dired-sidebar-use-magit-integration t)

    ;; refresh on projectile switch
    (customize-set-variable
     'dired-sidebar-refresh-on-projectile-switch t)

    ;; only show one buffer instance for dired-sidebar for each frame
    (customize-set-variable 'dired-sidebar-one-instance-p t)

    ;; refresh sidebar to match current file.
    (customize-set-variable 'dired-sidebar-should-follow-file t)

    ;; bind
    ;; assign C-x C-d to sidebar file browser
    (define-key ctl-x-map (kbd "C-d") 'dired-sidebar-toggle-sidebar)))

(require 'elfeed nil t)

(when (require 'moody nil t)
  (progn
    ;; remove underline
    (customize-set-variable 'x-underline-at-descent-line t)

    ;; change line height
    (customize-set-variable 'moody-mode-line-height 32)

    ;; mode-line format
    (customize-set-variable 'mode-line-format
                            '(" "
                              mode-line-misc-info
                              mode-line-mule-info
                              "%*%& %l:%c | %I "
                              moody-mode-line-buffer-identification
                              " %m "
                              (moody-vc-mode vc-mode)))))

(when (require 'erc nil t)
  (progn
    ;; nickname to use if one is not provided
    (customize-set-variable 'erc-nick "esac-io")

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

    ;; kill all query (also channel) buffers of this server on QUIT
    (customize-set-variable 'erc-kill-queries-on-quit t)))

;; binds
(when (boundp 'erc-mode-map)
  (progn
    ;; use eos/complete
    (define-key erc-mode-map (kbd "TAB") 'eos/complete)))

(when (require 'which-key nil t)
  (progn
    ;; customize
    ;; (customize-set-variable 'which-key-paging-key nil)
    (customize-set-variable 'which-key-idle-delay 0.8)
    (customize-set-variable 'which-key-idle-secondary-delay 0.4)
    (customize-set-variable 'which-key-separator " - ")
    (customize-set-variable 'which-key-use-C-h-commands t)
    (customize-set-variable 'which-key-add-column-padding 2)
    (customize-set-variable 'which-key-side-window-location 'bottom)
    (customize-set-variable 'which-key-sort-order
                            'which-key-key-order-alpha)

    ;; if non-nil allow which-key to use a less intensive method of Hide
    ;; fitting the popup window to the buffer.
    (customize-set-variable 'which-key-allow-imprecise-window-fit t)

    ;; bind
    (define-key ctl-x-map (kbd "x") 'which-key-show-major-mode)

    ;; init which-key-mode after emacs initialize
    (add-hook 'after-init-hook 'which-key-mode)))

(when (boundp 'which-key-replacement-alist)
  (progn

    ;; customize key replacements
    (add-to-list 'which-key-replacement-alist
                 '(("\\(.+\\)" .
                    "\\(\\(helm-\\)\\|.?\\(projectile\\|rtags\\|gtags\\|flycheck\\|company\\|dash\\|yas\\)[\-]\\)")
                   . (nil . "")))

    (add-to-list 'which-key-replacement-alist
                 '((nil . "helm-dash") . (nil . "search")))

    (add-to-list 'which-key-replacement-alist
                 '((nil . "helm-dash-at-point") . (nil . "search-at-point")))

    (add-to-list 'which-key-replacement-alist
                 '((nil . "helm-flycheck") . (nil . "list-erros")))

    (add-to-list 'which-key-replacement-alist
                 '((nil . "flycheck-list-errors") . (nil . "list-erros-other-window")))

    ;; (add-to-list 'which-key-replacement-alist
    ;;              '(("<left>" . nil) . ("left" . nil)))

    ;; (add-to-list 'which-key-replacement-alist
    ;;              '(("<right>" . nil) . ("right" . nil)))

    (add-to-list 'which-key-replacement-alist
                 '((nil . "eos-rtags-map") . (nil . "rtags")))

    (add-to-list 'which-key-replacement-alist
                 '((nil . "eos-tags-map") . (nil . "gtags")))

    (add-to-list 'which-key-replacement-alist
                 '((nil . "eos-pm-map") . (nil . "projectile")))

    (add-to-list 'which-key-replacement-alist
                 '((nil . "eos-window-map") . (nil . "window")))

    (add-to-list 'which-key-replacement-alist
                 '((nil . "eos-docs-map") . (nil . "dash")))

    (add-to-list 'which-key-replacement-alist
                 '((nil . "eos-sc-map") . (nil . "flycheck")))

    (add-to-list 'which-key-replacement-alist
                 '((nil . "eos-complete-map") . (nil . "complete")))))

(when (fboundp 'which-key-add-key-based-replacements)
  (which-key-add-key-based-replacements
    "C-x @"   "event"
    "C-x RET" "set"
    "C-x r"   "regs"
    "C-x @"   "event"
    "C-x 4"   "other"
    "C-x 5"   "frame"
    "C-x 6"   "2c"
    "C-x <end>" "eos/lock-screen"
    "C-x ESC"   "rept"
    "C-x 8"   "iso"
    "C-x m"   "kmacro"
    "C-h 4"   "other"))

(when (require 'buffer-move nil t)
  (progn
    ;; bind
    (global-set-key (kbd "C-x <C-up>") 'buf-move-up)
    (global-set-key (kbd "C-x <C-down>") 'buf-move-down)
    (global-set-key (kbd "C-x <C-left>") 'buf-move-left)
    (global-set-key (kbd "C-x <C-right>") 'buf-move-right)))

(when (require 'ibuffer nil t)
  (progn
    ;; customize

    ;; hook
    (add-hook 'ibuffer-mode-hook
              (lambda ()
                (interactive)
                ;; sort by filename/process
                (when (fboundp 'ibuffer-do-sort-by-filename/process)
                  (ibuffer-do-sort-by-filename/process))))
    ))

;; bind
;; (define-key ctl-x-map (kbd "b") 'ibuffer)))

(when (require 'savehist nil t)
  (progn
    ;; file name where minibuffer history is saved to and loaded from.
    (customize-set-variable
     'savehist-file (concat user-emacs-directory "cache/history"))

    ;; if non-nil, save all recorded minibuffer histories.
    (customize-set-variable 'savehist-save-minibuffer-history t)

    ;; enable savehist mode
    (eos/funcall 'savehist-mode 1)))

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

;; bind
(define-key ctl-x-map (kbd "&") 'eshell)

(when (require 'term nil t)
  (progn
    ;; customize
    ;; if non-nil, is file name to use for explicitly requested inferior shell.
    (customize-set-variable 'explicit-shell-file-name "/usr/local/bin/fish")

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

    ;; bind (with hook)
    (add-hook 'term-mode-hook
              (lambda ()
                (when (and (boundp 'term-raw-map)
                           (boundp 'term-mode-map))
                  (progn
                    ;; term-raw-map
                    (define-key term-raw-map (kbd "M-SPC") 'term-line-mode)

                    ;; term-mode-map
                    (define-key term-mode-map (kbd "M-SPC") 'term-char-mode)))))

    ;; hook
    (add-hook 'term-mode-hook
              (lambda()
                ;; do not display continuation lines.
                (toggle-truncate-lines)

                ;; disable line numbers mode
                (display-line-numbers-mode 0)))))

(when (require 'multi-term nil t)
  (progn
    ;; customize
    (customize-set-variable 'multi-term-program "/usr/local/bin/fish")

    ;; focus terminal window after you open dedicated window
    (customize-set-variable 'multi-term-dedicated-select-after-open-p t)

    ;; the buffer name of term buffer.
    (customize-set-variable 'multi-term-buffer-name "Term")

    ;; bind (C-x) prefix
    (define-key ctl-x-map (kbd "<C-return>") 'multi-term)

    ;; bind global
    (global-set-key (kbd "C-z") 'multi-term-dedicated-toggle)))

(defun eos/launch/st ()
  "Launch urxvt"
  (interactive)
  (eos/run/async-proc "st"))

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

    ;; customize search prefix
    (customize-set-variable 'eww-search-prefix eos/eww-google-search-url)
    ;; (customize-set-variable eww-search-prefix "https://duckduckgo.com/html/?q=")

    ;; customize download directory
    (customize-set-variable 'eww-download-directory "~/down")

    ;; customize checkbox symbols
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

(when (require 'browse-url nil t)
  (progn
    ;; customize:

    ;; the name of the browser program used by ‘browse-url-generic’.
    (customize-set-variable 'browse-url-generic-program "eww")

    ;; function to display the current buffer in a WWW browser: eww
    (customize-set-variable 'browse-url-browser-function 'eww-browse-url)))

(require 'helm-ag nil t)

(when (require 'ispell nil t)
  (progn
    ;; customize
    ;; program invoked by M-x ispell-word and M-x ispell-region commands.
    (customize-set-variable 'ispell-program-name "aspell")))

;; function (reference)
;; (defun eos/ispell/switch-dictionary ()
;;   "Switch dictionaries."
;;   (interactive)
;;   (let* ((dic ispell-current-dictionary)
;;          (change (if (string= dic "english") "brasileiro" "english")))
;;     (ispell-change-dictionary change)
;;     (message "Dictionary switched from %s to %s" dic change)))))

(when (require 'flyspell nil t)
  (progn
    ;; string that is the name of the default dictionary
    (customize-set-variable 'flyspell-default-dictionary "english")

    ;; add hooks
    (add-hook 'text-mode-hook 'flyspell-mode)
    (add-hook 'prog-mode-hook 'flyspell-prog-mode)))

(when (require 'flycheck nil t)
  (progn
    ;; bind
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

(when (require 'dmenu nil t)
  (progn
    ;; set dmenu-itens cache location
    (customize-set-variable
     'dmenu-save-file
     (concat user-emacs-directory "cache/dmenu-items"))

    ;; bind
    (define-key ctl-x-map (kbd "C-x") 'dmenu)))

(when (require 'comint nil t)
  (progn
    ;; customize
    ;; if non-nil, assume that the subprocess echoes any input.
    (customize-set-variable 'comint-process-echoes t)

    ;; if non-nil, use comint-prompt-regexp to recognize prompts.
    (customize-set-variable 'comint-use-prompt-regexp t)

    ;; regexp to recognize prompts in the inferior process.
    ;; (customize-set-variable 'comint-prompt-regexp ".*:.*>.*? ")

    ;; value to use for TERM when the system uses terminfo.
    (customize-set-variable 'comint-terminfo-terminal "eterm-color")))

(defun eos/transset-set (opacity)
  "Set transparency on frame window specify by OPACITY."
  (interactive "nOpacity: ")
  (let ((opacity (or opacity 1.0)))
    (async-shell-command (format "transset -a %.1f" opacity))))

;; init after exwm
(add-hook 'exwm-init-hook
          (lambda ()
            (interactive)
            (eos/transset-set 0.9)))

;; start compton after emacs initialize
(add-hook 'after-init-hook
          (lambda ()
            (eos/run/async-proc "compton")))

(when (require 'tramp nil t)
  (progn
    ;; customize
    ;; set tramp default method
    (customize-set-variable 'tramp-default-method "ssh")

    ;; if non-nil, chunksize for sending input to local process.
    ;; (customize-set-variable 'tramp-chunksize 512)

    ;; set tramp verbose level
    (customize-set-variable 'tramp-verbose 2)

    ;; connection timeout 30 seconds
    (customize-set-variable 'tramp-connection-timeout 30)))

(define-key ctl-x-map (kbd "<end>")
  (lambda ()
    (interactive)
    (eos/run/async-proc "slock")))

(global-set-key (kbd "<print>")
                (lambda ()
                  (interactive)
                  (eos/run/async-proc "scrot")))

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
(define-key ctl-x-map (kbd "C--") 'eos/lower-volume)
(define-key ctl-x-map (kbd "C-=") 'eos/raise-volume)

(add-hook 'org-mode-hook
          (lambda ()
            ;; do not truncate lines
            (setq truncate-lines nil)

            ;; set company backends
            (eos/company/set-backends
             '((company-capf
                company-keywords
                company-yasnippet
                company-ispell
                company-dabbrev
                company-dabbrev-code)
               (company-files)))))

(when (require 'text-mode nil t)
  (progn
    ;; bind
    (define-key text-mode-map (kbd "C-c C-g") 'keyboard-quit)
    (define-key text-mode-map (kbd "TAB") 'eos/complete-or-indent)
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
                    company-keywords
                    company-capf
                    company-dabbrev-code
                    company-dabbrev)
                   (company-files)))))
    ))

(when (require 'markdown-mode nil t)
  (progn
    ;; customize
    (customize-set-variable 'markdown-command "multimarkdown")))

;; bind
(when (boundp 'markdown-mode-map)
  (progn
    (define-key markdown-mode-map (kbd "TAB") 'eos/complete-or-indent)))

;; add eos-theme-dir to theme load path
(add-to-list 'custom-theme-load-path
             (concat user-emacs-directory "themes"))

;; load theme
(load-theme 'mesk-term t)

(when (require 'dashboard nil t)
  (progn
    ;; items
    (customize-set-variable 'dashboard-items
                            '((recents . 5)
                              (projects . 5)
                              (agenda . 5)
                              (bookmarks . 5)))

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

    ;; footer
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

    ;; set initial buffer choice (emacsclient fix)
    (customize-set-variable 'initial-buffer-choice
                            (lambda ()
                              (let ((initial-buffer (get-buffer "*dashboard*")))
                                (unless initial-buffer
                                  (setq initial-buffer (get-buffer "*scratch*")))
                                initial-buffer)))

    ;; init dashboard after emacs initialize
    (add-hook 'after-init-hook 'dashboard-setup-startup-hook)))

(require 'man nil t)

(add-hook 'Man-mode-hook
          (lambda ()
            ;; don't truncate lines
            (setq truncate-lines nil)))

(when (require 'helm-man nil t)
  (progn
    ;; bind
    (define-key help-map (kbd "y") 'helm-man-woman)))

(when (require 'dash-docs nil t)
  (progn
    ;; customize (fix async?)
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
    ;; customize
    ;; the directory where RFC documents are stored
    (customize-set-variable
     'rfc-mode-directory (concat (expand-file-name user-emacs-directory) "rfc/"))))

;; bind documentation related functions on eos-docs-map
(define-key eos-docs-map (kbd "C-g") 'keyboard-quit)

;; bind eos-docs-map under ctl-x-map
(define-key ctl-x-map (kbd "l") 'eos-docs-map)

(add-to-list 'display-buffer-alist
             '("\\*Completions\\*" display-buffer-below-selected))

(when (require 'company nil t)
  (progn
    ;; set echo delay
    (customize-set-variable 'company-echo-delay 0.2)

    ;; disable idle delay
    (customize-set-variable 'company-idle-delay nil)

    ;; set tooltip limit
    (customize-set-variable 'company-tooltip-limit 20)

    ;; set prefix length
    (customize-set-variable 'company-minimum-length 2)

    ;; cycle selection
    (customize-set-variable 'company-selection-wrap-around t)

    ;; sort by frequency
    (customize-set-variable 'company-transformers
                            '(company-sort-by-occurrence))

    ;; enable dabbrev downcase (most common)
    (customize-set-variable 'company-dabbrev-downcase t)

    ;; align annotations true
    (customize-set-variable 'company-tooltip-align-annotations nil)

    ;; show candidates number
    ;; to select completions use: M-1, M-2, etc..
    (customize-set-variable 'company-show-numbers t)

    ;; bind
    (define-key eos-complete-map (kbd "M-`") 'company-ispell)
    (define-key eos-complete-map (kbd "2") 'company-dabbrev)
    (define-key eos-complete-map (kbd "3") 'company-dabbrev-code)
    (define-key eos-complete-map (kbd "4") 'company-gtags)
    (define-key eos-complete-map (kbd "5") 'company-files)
    (define-key eos-complete-map (kbd "6") 'company-capf)
    (define-key eos-complete-map (kbd "1") 'company-yasnippet)

    ;; init after emacs initialize
    (add-hook 'after-init-hook 'global-company-mode)))

;; bind
(when (boundp 'company-active-map)
  (progn
    (define-key company-active-map (kbd "C-j") 'company-complete-selection)
    (define-key company-active-map (kbd "C-n") 'company-select-next)
    (define-key company-active-map (kbd "C-p") 'company-select-previous)))

(when (require 'company-statistics nil t)
  (progn

    ;; set company-statistics cache location
    (customize-set-variable
     'company-statistics-file
     (concat user-emacs-directory "cache/company-statistics-cache.el"))

    ;; init after company mode
    (add-hook 'company-mode-hook 'company-statistics-mode)))

(when (require 'yasnippet nil t)
  (progn
    ;; bind
    (define-key eos-complete-map (kbd "q") 'yas-expand)
    (define-key eos-complete-map (kbd "i") 'yas-insert-snippet)
    (define-key eos-complete-map (kbd "v") 'yas-visit-snippet-file)

    ;; initialize after emacs starts
    (add-hook 'after-init-hook 'yas-global-mode)))

(require 'helm-company nil t)

(when (boundp 'helm-company-map)
  (define-key helm-company-map (kbd "SPC") 'helm-keyboard-quit)
  (define-key helm-company-map (kbd "C-j") 'helm-maybe-exit-minibuffer))

;; set company backends
(defun eos/company/set-backends (backends)
  "Set company BACKENDS."
  (make-local-variable 'company-backends)
  (customize-set-variable 'company-backends backends))

;; calls helm-company if its bounded
(defun eos/complete ()
  "Helm company complete wrapper."
  (interactive)
  (when (fboundp 'helm-company)
    (helm-company)))

;; complete or indent
(defun eos/complete-or-indent ()
  "Complete or indent (TAB)."
  (interactive)
  (if (looking-at "\\_>")
      (progn
        (when (fboundp 'helm-company)
          (helm-company)))
    (indent-according-to-mode)))

;; exit, keyboard quit
(define-key eos-complete-map (kbd "C-g") 'keyboard-quit)

;; set eos-complete-map M-` keybind
(global-set-key (kbd "TAB") 'eos/complete-or-indent)
(global-set-key (kbd "M-`") 'eos-complete-map)

(when (require 'helm-gtags nil t)
  (progn
    ;; customize
    (customize-set-variable 'helm-gtags-ignore-case t)
    (customize-set-variable 'helm-gtags-auto-update t)
    (customize-set-variable 'helm-gtags-pulse-at-cursor t)
    (customize-set-variable 'helm-gtags-use-input-at-cursor t)
    (customize-set-variable 'helm-gtags-suggested-key-mapping t)

    ;; bind
    (define-key eos-tags-map (kbd "t") 'helm-gtags-dwim)
    (define-key eos-tags-map (kbd "s") 'helm-gtags-select)
    (define-key eos-tags-map (kbd "f") 'helm-gtags-find-tag)
    (define-key eos-tags-map (kbd "+") 'helm-gtags-show-stack)
    (define-key eos-tags-map (kbd "a") 'helm-gtags-parse-file)
    (define-key eos-tags-map (kbd "c") 'helm-gtags-create-tags)
    (define-key eos-tags-map (kbd "u") 'helm-gtags-update-tags)
    (define-key eos-tags-map (kbd "p") 'helm-gtags-find-pattern)
    (define-key eos-tags-map (kbd "r") 'helm-gtags-find-rtag)
    (define-key eos-tags-map (kbd "o") 'helm-gtags-find-tag-other-window)

    ;; enable helm-gtags mode after some programming mode startup
    (add-hook 'porg-mode-hook 'helm-gtags-mode)))

;; exit, keyboard quit
(define-key eos-tags-map (kbd "C-g") 'keyboard-quit)

;; ctl-x-map bind (C-x t)
(define-key ctl-x-map (kbd "t") 'eos-tags-map)

(require 'gud nil t)

(when (require 'cmake-ide nil t)
  (progn
    ;; setup
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
    ;; bind
    (define-key ctl-x-map (kbd "j") 'magit-status)))

(when (require 'projectile nil t)
  (progn
    ;; customize
    ;; enable cache and choose indexing method
    (customize-set-variable 'projectile-enable-caching t)
    (customize-set-variable 'projectile-indexing-method 'hybrid)
    (customize-set-variable 'projectile-completion-system 'helm)

    ;; set bookmarks file localtion (cache)
    (customize-set-variable 'projectile-known-projects-file
                            (concat user-emacs-directory "cache/projectile-bookmarks.eld"))

    (customize-set-variable 'projectile-cache-file
                            (concat user-emacs-directory "cache/projectile.cache"))

    ;; bind
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
    (define-key eos-pm-map (kbd "D") 'projectile-remove-known-project)

    ;; add hook (init projectile)
    (add-hook 'after-init-hook 'projectile-mode)))

(when (require 'helm-projectile nil t)
  (progn
    ;; bind
    (define-key eos-pm-map (kbd "p") 'helm-projectile-ag)
    (define-key eos-pm-map (kbd "n") 'helm-projectile-recentf)
    (define-key eos-pm-map (kbd "/") 'helm-projectile-find-dir)
    (define-key eos-pm-map (kbd "f") 'helm-projectile-find-file)
    (define-key eos-pm-map (kbd "b") 'helm-projectile-browse-dirty-projects)
    (define-key eos-pm-map (kbd "a")
      'helm-projectile-find-file-in-known-projects)

    ;; dwin
    (define-key eos-pm-map (kbd "w") 'helm-projectile-find-file-dwim)

    ;; helm-swoop
    ;; (define-key eos-pm-map (kbd "S") 'helm-multi-swoop-projectile)

    ;; enable helm-projectile after emacs start
    (add-hook 'after-init-hook 'helm-projectile-on)))

;; exit, keyboard quit
(define-key eos-pm-map (kbd "C-g") 'keyboard-quit)

;; set ctl-x-map prefix (C-x p)
(define-key ctl-x-map (kbd "p") 'eos-pm-map)

(require 'cc-mode nil t)

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
    ;; hooks:
    (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)))

(require 'company-c-headers nil t)

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

;; eos rtags prefix map
(define-key eos-rtags-map (kbd "C-g") 'keyboard-quit)
(define-key eos-rtags-map (kbd "l") 'rtags-taglist)
(define-key eos-rtags-map (kbd "I") 'rtags-install)
(define-key eos-rtags-map (kbd "i") 'rtags-imenu)
(define-key eos-rtags-map (kbd "t") 'rtags-find-symbol-at-point)
(define-key eos-rtags-map (kbd "s") 'rtags-find-symbol)
(define-key eos-rtags-map (kbd "y") 'rtags-symbol-type)
(define-key eos-rtags-map (kbd "l") 'rtags-symbol-info)
(define-key eos-rtags-map (kbd "n") 'rtags-rename-symbol)
(define-key eos-rtags-map (kbd "m") 'rtags-asm-file)
(define-key eos-rtags-map (kbd "h") 'rtags-find-file-history)
(define-key eos-rtags-map (kbd "x") 'rtags-fixit)
(define-key eos-rtags-map (kbd "d") 'rtags-diagnostics)
(define-key eos-rtags-map (kbd "c") 'rtags-compile-file)
(define-key eos-rtags-map (kbd "-") 'rtags-compilation-flags)
(define-key eos-rtags-map (kbd "r") 'rtags-find-references-at-point)
(define-key eos-rtags-map (kbd "p") 'rtags-find-all-references-at-point)
(define-key eos-rtags-map (kbd ".")
  'rtags-find-functions-called-by-this-function)

;; ser rtags prefix map in c-mode map (C-c r)
(define-key c-mode-map (kbd "C-c r") 'eos-rtags-map)

;; complete or indent
(define-key c-mode-map (kbd "TAB") 'eos/complete-or-indent)

;; c/c++ garage
(defun eos/cc/set-company-backends ()
  "Set C/C++ company backends."
  (eos/company/set-backends
   '((company-c-headers)
     (company-irony
      company-yasnippet
      company-capf
      company-keywords
      company-dabbrev
      company-dabbrev-code)
     (company-files))))

(add-hook 'c-mode-hook
          (lambda ()
            ;; set cc common company backends
            (eos/cc/set-company-backends)

            ;; set dash docset
            (eos/dash/activate-docset '"C")

            ;; load rtags
            (eos/cc/load-rtags)))

(add-hook 'c++-mode-hook
          (lambda ()
            ;; set cc common backends (company and flycheck)
            (eos/cc/set-company-backends)

            ;; set dash docset
            (eos/dash/activate-docset '"C++")

            ;; load rtags
            (eos/cc/load-rtags)))

(require 'lisp-mode nil t)

(customize-set-variable 'lisp-body-indent 2)

(require 'company-elisp nil t)

(require 'elisp-mode nil t)

;; bind
(define-key emacs-lisp-mode-map (kbd "C-c C-f") 'eval-defun)
(define-key emacs-lisp-mode-map (kbd "C-c C-r") 'eval-region)
(define-key emacs-lisp-mode-map (kbd "C-c C-c") 'eval-buffer)
(define-key emacs-lisp-mode-map (kbd "TAB") 'eos/complete-or-indent)

;; unbind
(define-key emacs-lisp-mode-map (kbd "DEL") 'nil)
(define-key emacs-lisp-mode-map (kbd "ESC") 'nil)
(define-key emacs-lisp-mode-map (kbd "C-x") 'nil)
(define-key emacs-lisp-mode-map (kbd "C-M-x") 'nil)
(define-key emacs-lisp-mode-map (kbd "C-M-q") 'nil)

;; add-hook
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            ;; set company backends
            (eos/company/set-backends
             '((company-elisp
                company-capf
                company-yasnippet
                company-keywords
                company-dabbrev
                company-dabbrev-code)
               (company-files)))
            ))

(require 'sh-script nil t)

(require 'company-shell nil t)

;; add backends selection on sh-mode-hook space.
;; this function will be called after sh-mode initialize
(add-hook 'sh-mode-hook
          (lambda ()
            ;; set company backends
            (eos/company/set-backends
             '((company-shell
                company-shell-env
                company-yasnippet
                company-keywords
                company-capf
                company-dabbrev-code
                company-dabbrev)
               (company-files)))))

(require 'fish-mode nil t)

;; set the proper company backends
(add-hook 'fish-mode-hook
          (lambda ()
            (eos/company/set-backends
             '((company-fish-shell
                company-shell
                company-shell-env
                company-yasnippet
                company-keywords
                company-capf
                company-dabbrev
                company-dabbrev-code)
               (company-files)))))

(require 'cperl-mode nil t)

(require 'python nil t)

(when (require 'go-mode nil t)
  (progn
    ;; add (*.go . go-mode) to auto-mode-alist
    ;; init go-mode when a file with the extersion .go is opened
    (add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))))

;; select the proper backends and docset
(add-hook 'go-mode-hook
          (lambda ()
            ;; set company backends
            (eos/company/set-backends
             '((company-yasnippet
                company-keywords
                company-capf
                company-dabbrev
                company-dabbrev-code)
               (company-files)))

            ;; set dash docsets
            (eos/dash/activate-docset '"Go")))

(require 'ess-r-mode nil t)

(require 'verilog nil t)

(when (require 'highlight-doxygen nil t)
  (progn
    ;; add doxygen
    (add-hook 'prog-mode-hook 'highlight-doxygen-global-mode)))

(when (require 'web-mode nil t)
  (progn
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
;; (define-key ctl-x-map (kbd "C-<left>") nil)
;; (define-key ctl-x-map (kbd "C-<right>") nil)
;; (define-key ctl-x-map (kbd "C-=") nil)
;; (define-key ctl-x-map (kbd "C-0") nil)
;; (define-key ctl-x-map (kbd "C-z") nil)
;; (define-key ctl-x-map (kbd "C--") nil)
;; (define-key ctl-x-map (kbd "C-d") nil)
;; (define-key ctl-x-map (kbd "ESC") nil)
(define-key ctl-x-map (kbd "]") nil)
(define-key ctl-x-map (kbd "[") nil)
(define-key ctl-x-map (kbd "C-+") nil)
(define-key ctl-x-map (kbd "C-a") nil)
(define-key ctl-x-map (kbd "C-l") nil)
(define-key ctl-x-map (kbd "C-r") nil)
(define-key ctl-x-map (kbd "C-n") nil)
(define-key ctl-x-map (kbd "C-p") nil)
(define-key ctl-x-map (kbd "C-o") nil)
(define-key ctl-x-map (kbd "C-h") nil)
(define-key ctl-x-map (kbd "C-u") nil)
(define-key ctl-x-map (kbd "C-\@") nil)
(define-key ctl-x-map (kbd "M-:") nil)
(define-key ctl-x-map (kbd "`") nil)
(define-key ctl-x-map (kbd ")") nil)
(define-key ctl-x-map (kbd "(") nil)
(define-key ctl-x-map (kbd "<") nil)
(define-key ctl-x-map (kbd ">") nil)
(define-key ctl-x-map (kbd "\@") nil)
(define-key ctl-x-map (kbd "-") nil)
(define-key ctl-x-map (kbd ".") nil)
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
(define-key ctl-x-map (kbd "X") nil)

;; clean minor-mode-map-alist
(setq minor-mode-map-alist nil)

;; unset
;; (global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-@"))
(global-unset-key (kbd "C-\\"))
(global-unset-key (kbd "M-l"))
(global-unset-key (kbd "M-h"))
(global-unset-key (kbd "M-\\"))
;; (global-unset-key (kbd "M-z"))
;; (global-unset-key (kbd "M-SPC"))
(global-unset-key (kbd "M-$"))
(global-unset-key (kbd "M-("))
(global-unset-key (kbd "M-)"))
;; (global-unset-key (kbd "M-m"))
(global-unset-key (kbd "M-r"))
(global-unset-key (kbd "M-{"))
(global-unset-key (kbd "M-}"))
(global-unset-key (kbd "S-SPC"))
(global-unset-key (kbd "<backtap>"))
(global-unset-key (kbd "M-="))
(global-unset-key (kbd "M-@"))
(global-unset-key (kbd "M-~"))

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

(require 'eos-adapt (expand-file-name "eos-adapt.el" user-emacs-directory) t)

(provide 'eos)
;; eos.el ends here
