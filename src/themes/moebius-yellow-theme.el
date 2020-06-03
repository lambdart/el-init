;; moebius-theme.el --- Moebius emacs theme
;;
;;; Copyright (C) 2020 esac
;;
;; Author: esac <esac-io@tutanota.com>
;; URL: https://gitlab.com/esac-io/moebius-theme
;; Package-Requires: ((emacs "26.1"))
;; Keywords: faces, theme, accessibility
;; License: MIT
;;
;;; Commentary:
;;
;; This theme is a working in progress, and was designed
;; for people who likes transparent frames (windows),
;; smooth colors, elegance and simplicity.
;;
;; PS: if you use EXWM X11 windows manager and really misses
;; the transparent windows (like on i3) maybe this theme can help.
;;
;; The name is a humble homage to a great artist called
;; Jean "moebius-yellow" Giraud, thank you for you priceless
;; work, may you rest in peace...
;;
;;; Code:
;;
(require 'color nil t)

;; defines moebius-yellow theme
(deftheme moebius-yellow
  "Simple emacs minimal dark theme.")

;; Define colours pallets.
(let ((class '((class color) (min-colors 89)))
       ;; all colors has to have a tuple (background . foreground)
       ;; special colors
       (bg-default   "#ffffe0") (fg-default   "#696969")
       (bg-dim       "#eeeed1") (fg-dim       "#969696")
       (bg-bright    "#cdcdb4") (fg-bright    "#ededed")
       (bg-shadow    "#cdc9a5") (fg-shadow    "#f5f5f5")

       ;; basic colors
       (bg-black     "#8b8b83") (fg-black     "#323232")
       (bg-white     "#bfbfbf") (fg-white     "#d3d3d3")
       (bg-red       "#8b6969") (fg-red       "#a85454")
       (bg-gray      "#696969") (fg-gray      "#a9a9a9")
       (bg-orange    "#3f321f") (fg-orange    "#a88654")
       (bg-yellow    "#343922") (fg-yellow    "#8d995c")
       (bg-turquoise "#1f3f2c") (fg-turquoise "#54a875")
       (bg-green     "#548b54") (fg-green     "#65a854")
       (bg-cyan      "#1f3f3f") (fg-cyan      "#54a8a8")
       (bg-blue      "#005f87") (fg-blue      "#5476a8")
       (bg-purple    "#d9d9d9") (fg-purple    "#7d71a8")
       (bg-magenta   "#8787af") (fg-magenta   "#9754a8")
       (bg-pink      "#3f1f32") (fg-pink      "#cd6889"))

  ;; set faces
  (custom-theme-set-faces
    'moebius-yellow
    ;; custom-set-faces was added by Custom.
    ;; If you edit it by hand, you could mess it up, so be careful.
    ;; Your init file should contain only one such instance.
    ;; If there is more than one, they won't work right.

    ;; Default
    `(default ((t (:background ,bg-default :foreground ,fg-default))))
    `(c-nonbreakable-space-face ((t (:background ,bg-magenta :foreground ,fg-white :weight normal))) t)
    `(cursor ((t (:background ,fg-dim))))
    `(error ((t (:foreground ,fg-red))))
    `(italic ((t (:slant italic))))
    `(match ((t (:background ,bg-dim :foreground ,fg-blue))))
    ;; `(match ((t (:foreground ,fg-blue))))
    `(nobreak-hyphen ((t (:foreground ,fg-red))))
    `(minibuffer-prompt ((t (:foreground ,fg-blue))))
    `(warning ((t (:foreground ,fg-pink :weight unspecified))))
    `(region ((t (:background ,bg-blue :foreground ,bg-white))))
    `(shadow ((t (:foreground ,fg-dim))))
    `(success ((t (:foreground ,fg-green :weight bold))))
    `(trailing-whitespace ((t (:background ,bg-red :foreground ,fg-default))))
    `(info-node ((t (:foreground ,fg-orange :slant italic :weight bold))))
    `(lazy-highlight ((t (:background ,bg-blue :foreground ,bg-white))))
    `(line-number ((t (:inherit (shadow black) :back))))
    `(line-number-current-line ((t (:inherit line-number))))
    `(secondary-selection ((t (:background ,bg-dim :foreground ,fg-default))))
    `(escape-glyph ((t (:foreground ,fg-red))))

    ;; Font
    `(font-lock-builtin-face ((t (:foreground ,fg-blue))))
    `(font-lock-comment-delimiter-face ((t (:foreground ,fg-dim :inherit unspecified))))
    `(font-lock-comment-face ((t (:foreground ,fg-dim))))
    `(font-lock-constant-face ((t (:foreground ,fg-red))))
    `(font-lock-constant-face ((t (:foreground ,fg-red))))
    `(font-lock-doc-face ((t (:foreground ,fg-green :inherit unspecified))))
    `(font-lock-function-name-face ((t (:foreground ,fg-blue))))
    `(font-lock-keyword-face ((t (:foreground ,fg-orange))))
    `(font-lock-negation-char-face ((t (:foreground ,fg-red))))
    `(font-lock-regexp-grouping-backslash ((t (:foreground ,fg-red))))
    `(font-lock-regexp-grouping-construct ((t (:foreground ,fg-pink))))
    `(font-lock-string-face ((t (:foreground ,fg-pink))))
    `(font-lock-type-face ((t (:foreground ,fg-purple))))
    `(font-lock-variable-name-face ((t (:foreground ,fg-yellow))))
    `(font-lock-warning-face ((t (:foreground ,fg-orange :inherit unspecified :weight bold))))

    ;; Fringe
    `(fringe ((t nil)))
    ;;`(fringe ((t (:background unspecified :inherit default))))

    ;; Mode-line
    `(mode-line ((t (:background ,bg-dim :foreground ,bg-black
                      :box (:color ,bg-dim :line-width 2)))))
    `(mode-line-inactive ((t (:background ,bg-dim :foreground ,fg-gray
                               :box (:color ,bg-dim :line-width 2)))))
    `(mode-line-buffer-id ((t (:foreground ,fg-cyan :weight normal))))

    ;; Highlight
    `(highlight ((t (:background ,bg-dim))))
    `(highlight-numbers-number ((t (:foreground ,fg-cyan :inherit unspecified))))
    `(highlight-80+ ((t (:underline (:color ,fg-red :style wave) :background unspecified))))

    ;; Highlight line
    `(hl-line ((t (:background ,bg-dim))))

    ;; Highlight indent
    `(hl-indent-block-face-1 ((t (:background ,bg-red))))
    `(hl-indent-block-face-2 ((t (:background ,bg-pink))))
    `(hl-indent-block-face-3 ((t (:background ,bg-orange))))
    `(hl-indent-block-face-4 ((t (:background ,bg-yellow))))
    `(hl-indent-block-face-5 ((t (:background ,bg-green))))
    `(hl-indent-block-face-6 ((t (:background ,bg-turquoise))))
    `(hl-indent-face ((t (:inherit unspecified :background ,bg-dim))))

    ;; Holiday
    `(holiday ((t (:background ,fg-red :foreground ,bg-white))))

    ;; Homoglyph
    `(homoglyph ((t (:foreground ,fg-red))))

    ;; Header Line
    `(header-line ((t (:background ,bg-dim :foreground ,fg-gray))))

    ;; Popup
    `(popup-isearch-match ((t (:inherit black :background ,bg-blue))))

    ;; Pulse
    `(pulse-highlight-face ((t (:background ,bg-yellow :foreground ,bg-bright))))
    `(pulse-highlight-start-face ((t (:background ,bg-yellow :foreground ,bg-bright))))

    ;; Elpa
    `(epa-string ((t (:foreground ,fg-blue))))

    ;; Eww
    `(eww-valid-certificate ((t (:foreground ,fg-green :weight bold))))

    ;; Links
    `(link ((t (:foreground ,fg-orange :underline t))))
    `(link-visited ((t (:foreground ,fg-magenta :underline t))))

    ;; Completions
    `(completions-common-part ((t (:foreground ,fg-blue))))

    ;; Icomplete
    `(icomplete-first-match ((t (:foreground ,fg-red :weight normal))))

    ;; Isearch
    `(isearch ((t (:background ,bg-blue :foreground ,fg-white))))
    `(isearch-fail ((t (:background ,bg-default :foreground ,fg-red))))

    ;; Compilation
    `(compilation-info ((t (:foreground ,fg-green :weight normal))))
    `(compilation-mode-line-exit ((t (:inherit compilation-info :foreground ,fg-green :weight bold))))

    ;; CSS
    `(css-property ((t (:foreground ,fg-orange))))
    `(css-proprietary-property ((t (:foreground ,fg-red :inherit unspecified :slant italic))))
    `(css-selector ((t (:foreground ,fg-blue))))

    ;; Custom
    `(custom-button ((t (:background ,bg-white :foreground ,fg-black
                          :box (:line-width 1 :color ,bg-white)
                          :weight ultra-light :height 1.1 :width ultra-condensed))))
    `(custom-button-pressed-unraised ((t (:inherit custom-button-unraised :foreground ,fg-red))))
    `(custom-changed ((t (:background ,bg-blue :foreground ,fg-black))))
    `(custom-comment-tag ((t (:foreground ,fg-blue))))
    `(custom-group-tag ((t (:inherit variable-pitch :foreground ,fg-blue :weight bold :height 1.2))))
    `(custom-invalid ((t (:background ,bg-red :foreground ,fg-black))))
    `(custom-modified ((t (:background ,bg-blue :foreground ,fg-white))))
    `(custom-set ((t (:background ,bg-white :foreground ,fg-blue))))
    `(custom-themed ((t (:background ,bg-blue :foreground ,fg-white))))
    `(custom-variable-tag ((t (:foreground ,fg-blue :weight bold))))

    ;; Company
    `(company-preview ((t (:background nil :foreground ,fg-blue))))
    `(company-preview-common ((t (:inherit nil :background ,bg-dim
                                   :foreground ,fg-blue :weight normal))))
    `(company-echo-common ((t (:background ,bg-shadow :foreground ,fg-blue))))
    `(company-scrollbar-bg ((t (:background ,bg-bright))))
    `(company-scrollbar-fg ((t (:background ,bg-dim))))
    `(company-tooltip ((t (:background ,bg-dim :foreground ,fg-default))))
    `(company-tooltip-common ((t (:foreground ,fg-blue))))
    `(company-tooltip-selection ((t (:background ,bg-bright))))
    `(company-tooltip-annotation ((t (:foreground ,fg-blue))))
    `(company-tooltip-search ((t (:background ,bg-yellow :inherit unspecified))))
    `(company-tooltip-search-selection ((t (:background ,bg-yellow :inherit unspecified))))
    `(company-template-field ((t (:background ,bg-dim :foreground ,fg-yellow))))

    ;; Dashboard
    `(dashboard-footer ((t (:foreground ,fg-red))))
    `(dashboard-heading ((t (:foreground ,fg-blue))))
    `(dashboard-text-banner ((t (:foreground ,fg-dim))))

    ;; Diff
    `(diff-header ((t (:background ,bg-shadow))))
    `(diff-added ((t (:background ,bg-green :foreground ,fg-black))))
    `(diff-removed ((t (:background ,bg-red :foreground ,fg-black))))
    `(diff-changed ((t (:background ,bg-red :foreground ,fg-black))))
    `(diff-function ((t (:inherit unspecified :foreground ,fg-orange))))
    `(diff-file-header ((t (:foreground ,fg-default :background unspecified))))
    `(diff-refine-changed ((t (:background ,bg-yellow :foreground ,fg-white))))
    `(diff-hl-change ((t (:foreground ,bg-yellow :background unspecified :inherit diff-changed))))
    `(diff-hl-delete ((t (:foreground ,bg-red :inherit diff-removed))))
    `(diff-hl-insert ((t (:foreground ,bg-green :inherit diff-added))))
    `(diff-hunk-header ((t (:inherit unspecified :weight bold :foreground ,fg-yellow :underline t))))
    `(diff-indicator-added ((t (:foreground ,fg-green :weight bold :inherit unspecified))))
    `(diff-indicator-changed ((t (:foreground ,fg-yellow :weight bold :inherit unspecified ))))
    `(diff-indicator-removed ((t (:foreground ,fg-red :weight bold :inherit unspecified))))
    `(diff-refine-added ((t (:foreground ,fg-green :background unspecified :inherit unspecified))))
    `(diff-refine-removed ((t (:foreground ,fg-red :background unspecified :inherit unspecified))))

    ;; Dired
    `(dired-flagged ((t (:inherit region))))
    `(dired-header ((t (:foreground ,fg-turquoise :weight bold :inherit unspecified))))
    `(dired-mark ((t (:foreground ,fg-pink :inherit unspecified))))
    `(dired-marked ((t (:inherit nil :background ,bg-magenta :foreground ,fg-bright))))

    ;; Dictionary
    ;; dictionary-button-face                    white
    ;; dictionary-reference-face                 yellow
    ;; dictionary-word-definition-face           gray (another font) (italic)
    ;; dictionary-word-entry-face                gray (italic)
    `(diary ((t (:inherit nil :background nil :foreground ,fg-red))))
    `(dictionary-reference-face ((t (:inherit nil :background nil :foreground ,fg-orange))))

    ;; Ediff
    `(ediff-current-diff-A ((t (:inherit diff-removed))))
    `(ediff-current-diff-B ((t (:inherit diff-added))))
    `(ediff-current-diff-C ((t (:inherit diff-changed))))
    `(ediff-fine-diff-A ((t (:inherit diff-refine-removed))))
    `(ediff-fine-diff-B ((t (:inherit diff-refine-added))))
    `(ediff-fine-diff-C ((t (:inherit diff-refine-change))))

    ;; Ert
    `(ert-test-result-expected ((t (:background unspecified :foreground ,fg-green))))
    `(ert-test-result-unexpected ((t (:background unspecified :foreground ,fg-red))))

    ;; Eshell
    `(eshell-fringe-status-failure ((t (:foreground ,fg-red))))
    `(eshell-fringe-status-success ((t (:foreground ,fg-green))))
    `(eshell-ls-archive ((t (:foreground ,fg-pink :weight unspecified))))
    `(eshell-ls-backup ((t (:foreground ,fg-orange))))
    `(eshell-ls-clutter ((t (:foreground ,fg-dim :weight unspecified))))
    `(eshell-ls-directory ((t (:foreground ,fg-blue :weight unspecified))))
    `(eshell-ls-executable ((t (:foreground ,fg-red :weight unspecified))))
    `(eshell-ls-missing ((t (:foreground ,fg-red :weight bold))))
    `(eshell-ls-product ((t (:foreground ,fg-purple))))
    `(eshell-ls-readonly ((t (:foreground ,fg-magenta))))
    `(eshell-ls-special ((t (:foreground ,fg-turquoise))))
    `(eshell-ls-symlink ((t (:foreground ,fg-cyan :weight unspecified))))
    `(eshell-ls-unreadable ((t (:foreground ,fg-red))))
    `(eshell-prompt ((t (:foreground ,fg-blue :weight unspecified))))

    ;; Flymake
    `(flymake-errline ((t (:background unspecified :underline (:color ,fg-red :style wave)))))
    `(flymake-infoline ((t (:background unspecified :underline (:color ,fg-blue :style wave)))))
    `(flymake-warnline ((t (:background unspecified :underline (:color ,fg-orange :style wave)))))

    ;; Flyspell
    `(flyspell-duplicate ((t (:inherit unspecified :underline (:color ,fg-orange :style wave)))))
    `(flyspell-incorrect ((t (:inherit unspecified :underline (:color ,fg-red :style wave)))))

    ;; Flycheck
    `(flycheck-warning ((t (:underline (:color ,fg-red :style wave)))))

    ;; Gnus
    `(gnus-button ((t (:weight bold))))
    `(gnus-cite-1 ((t (:foreground ,fg-red))))
    `(gnus-cite-10 ((t (:foreground ,fg-pink))))
    `(gnus-cite-11 ((t (:foreground ,fg-bright))))
    `(gnus-cite-2 ((t (:foreground ,fg-orange))))
    `(gnus-cite-3 ((t (:foreground ,fg-yellow))))
    `(gnus-cite-4 ((t (:foreground ,fg-green))))
    `(gnus-cite-5 ((t (:foreground ,fg-turquoise))))
    `(gnus-cite-6 ((t (:foreground ,fg-cyan))))
    `(gnus-cite-7 ((t (:foreground ,fg-blue))))
    `(gnus-cite-8 ((t (:foreground ,fg-purple))))
    `(gnus-cite-9 ((t (:foreground ,fg-magenta))))
    `(gnus-group-mail-3 ((t (:foreground ,fg-cyan :weight bold))))
    `(gnus-group-mail-3-empty ((t (:foreground ,fg-cyan))))
    `(gnus-group-news-3 ((t (:foreground ,fg-red :weight bold))))
    `(gnus-group-news-3-empty ((t (:foreground ,fg-red))))
    `(gnus-header-content ((t (:foreground ,fg-dim :slant italic))))
    `(gnus-header-from ((t (:weight bold))))
    `(gnus-header-name ((t (:foreground ,fg-blue :weight bold))))
    `(gnus-header-newsgroups ((t (:foreground ,fg-bright :weight bold))))
    `(gnus-header-subject ((t (:foreground ,fg-yellow))))
    `(gnus-signature ((t (:foreground ,fg-dim :slant italic))))
    `(gnus-splash ((t (:foreground ,fg-default))))
    `(gnus-summary-cancelled ((t (:foreground ,fg-dim :background unspecified :strike-through t))))
    `(gnus-summary-high-ancient ((t (:inherit gnus-summary-normal-ancient :weight bold))))
    `(gnus-summary-high-read ((t (:inherit gnus-summary-normal-read :weight bold))))
    `(gnus-summary-high-ticked ((t (:inherit gnus-summary-normal-ticked :weight bold))))
    `(gnus-summary-high-unread ((t (:foreground ,fg-pink))))
    `(gnus-summary-low-ancient ((t (:inherit gnus-summary-normal-ancient :slant italic))))
    `(gnus-summary-low-read ((t (:inherit gnus-summary-normal-read :slant italic))))
    `(gnus-summary-low-ticked ((t (:inherit gnus-summary-normal-ticked :slant italic))))
    `(gnus-summary-low-unread ((t (:inherit gnus-summary-normal-unread :slant italic))))
    `(gnus-summary-normal-ancient ((t (:foreground ,fg-cyan))))
    `(gnus-summary-normal-read ((t (:foreground ,fg-dim))))
    `(gnus-summary-normal-ticked ((t (:foreground ,fg-turquoise))))
    `(gnus-summary-normal-unread ((t (:foreground ,fg-default))))
    `(gnus-group-mail-1 ((t (:foreground ,fg-red :weight bold))))
    `(gnus-group-mail-1-empty ((t (:foreground ,fg-purple))))
    `(gnus-group-mail-2 ((t (:foreground ,fg-magenta :weight bold))))
    `(gnus-group-mail-2-empty ((t (:foreground ,fg-magenta))))
    `(gnus-group-mail-low ((t (:foreground ,fg-magenta :weight bold))))
    `(gnus-group-mail-low-empty ((t (:foreground ,fg-magenta))))
    `(gnus-summary-selected ((t (:background ,bg-blue :foreground ,fg-white))))

    ;; Go-mode
    `(go-coverage-10 ((t (:foreground ,fg-green))))
    `(go-coverage-7 ((t (:foreground ,fg-green))))
    `(go-coverage-8 ((t (:foreground ,fg-green))))
    `(go-coverage-9 ((t (:foreground ,fg-green))))
    `(go-coverage-covered ((t (:foreground ,fg-green))))

    ;; Identica
    `(identica-stripe-face ((t (:background ,bg-bright))))
    `(identica-uri-face ((t (:foreground ,fg-orange :underline t))))
    `(identica-username-face ((t (:foreground ,fg-blue :weight bold :underline unspecified))))

    ;; Ibuffer
    `(ibuffer-locked-buffer ((t (:foreground ,bg-red))))

    ;; Ido
    `(ido-only-match ((t (:foreground ,fg-green))))
    `(ido-indicator ((t (:background ,bg-bright :foreground ,fg-white :width condensed))))

    ;; Iedit
    `(iedit-occurrence ((t (:background ,bg-blue :foreground ,fg-white))))

    ;; Man
    `(Man-overstrike ((t (:foreground ,fg-blue :weight bold))))
    `(Man-underline ((t (:foreground ,fg-red))))

    ;; Message
    `(message-header-cc ((t (:foreground ,fg-dim))))
    `(message-header-name ((t (:inherit gnus-header-name))))
    `(message-header-newsgroups ((t (:foreground ,fg-dim :weight bold))))
    `(message-header-other ((t (:inherit gnus-header-content))))
    `(message-header-subject ((t (:inherit gnus-header-subject))))
    `(message-header-to ((t (:foreground ,fg-dim :weight bold))))
    `(message-header-xheader ((t (:foreground ,fg-dim :slant italic))))
    `(message-mml ((t (:foreground ,fg-green))))
    `(message-separator ((t (:foreground ,fg-blue))))

    ;; Magit
    `(magit-branch-current ((t (:inherit nil :background ,bg-blue :foreground ,fg-bright))))
    `(magit-branch-local ((t (:background ,bg-magenta :foreground ,fg-bright))))
    `(magit-branch-remote ((t (:background ,fg-green :foreground ,fg-bright))))
    `(magit-log-author ((t (:foreground ,fg-red :slant normal :weight normal))))
    `(magit-signature-expired ((t (:foreground ,fg-yellow))))
    `(magit-signature-revoked ((t (:foreground ,fg-magenta))))
    `(magit-bisect-bad ((t (:foreground ,fg-red))))
    `(magit-bisect-good ((t (:foreground ,fg-green))))
    `(magit-bisect-skip ((t (:foreground ,fg-dim))))
    `(magit-blame-date ((t (:foreground ,fg-pink :inherit magit-blame-heading))))
    `(magit-blame-hash ((t (:foreground ,fg-magenta :inherit magit-blame-heading))))
    `(magit-blame-header ((t (:foreground ,fg-green :background ,bg-dim
                               :weight bold :inherit unspecified))))
    `(magit-blame-heading ((t (:foreground ,fg-default :background ,bg-bright))))
    `(magit-blame-name ((t (:foreground ,fg-turquoise :inherit magit-blame-heading))))
    `(magit-blame-summary ((t (:foreground ,fg-green :inherit magit-blame-heading))))
    `(magit-branch ((t (:foreground ,fg-pink :weight bold :inherit unspecified))))
    `(magit-diff-added ((t (:foreground unspecified :background unspecified :inherit diff-added))))
    `(magit-diff-added-highlight ((t (:foreground unspecified :background unspecified
                                       :inherit magit-diff-added))))

    `(magit-diff-context ((t (:foreground unspecified :inherit shadow))))

    `(magit-diff-whitespace-warning ((t (:background ,bg-dim :foreground ,fg-red unspecified :inherit shadow))))

    `(magit-diff-context-highlight ((t (:foreground unspecified :background ,bg-dim
                                         :inherit magit-diff-context))))
    `(magit-diff-file-heading ((t (:foreground unspecified :underline unspecified
                                    :inherit diff-file-header))))
    `(magit-diff-removed ((t (:foreground unspecified :background unspecified
                               :inherit diff-removed))))
    `(magit-diff-removed-highlight ((t (:foreground unspecified :background unspecified
                                         :inherit magit-diff-removed))))
    `(magit-diffstat-removed ((t (:foreground ,fg-red))))
    `(magit-item-highlight ((t (:inherit unspecified))))

    ;; Magit logs
    `(magit-log-head-label-default ((t (:foreground ,fg-default :background ,bg-cyan
                                         :box (:color ,bg-cyan :line-width 2 :style nil)))))
    `(magit-log-head-label-head ((t (:foreground ,fg-default :background ,bg-blue
                                      :box (:color ,bg-blue :line-width 2 :style nil)))))
    `(magit-log-head-label-local ((t (:foreground ,bg-white :background ,bg-magenta
                                       :box (:color ,bg-magenta :line-width 2 :style nil)))))
    `(magit-log-head-label-remote ((t (:foreground ,fg-white :background ,bg-green
                                        :box (:color ,bg-green :line-width 2 :style nil)))))
    `(magit-log-head-label-tags ((t (:foreground ,fg-default :background ,bg-orange
                                      :box (:color ,bg-orange :line-width 2 :style nil)))))
    `(magit-log-sha1 ((t (:foreground ,fg-default :background ,bg-blue
                           :box (:color ,bg-blue :line-width 2 :style nil)))))

    `(magit-reflog-merge ((t (:foreground ,fg-green :inherit unspecified))))
    `(magit-reflog-amend ((t (:foreground ,fg-pink :inherit unspecified))))
    `(magit-reflog-reset ((t (:foreground ,fg-red :inherit unspecified))))
    `(magit-reflog-rebase ((t (:foreground ,fg-pink :inherit unspecified))))
    `(magit-reflog-commit ((t (:foreground ,fg-green :inherit unspecified))))
    `(magit-reflog-checkout ((t (:foreground ,fg-green :inherit unspecified))))
    `(magit-reflog-cherry-pick ((t (:foreground ,fg-green :inherit unspecified))))

    `(magit-reflog-other ((t (:foreground ,fg-cyan :inherit unspecified))))
    `(magit-reflog-remote ((t (:foreground ,fg-cyan :inherit unspecified))))

    `(magit-process-ng ((t (:foreground ,fg-red :inherit unspecified))))
    `(magit-process-ok ((t (:foreground ,fg-green :inherit unspecified))))
    `(magit-section-heading ((t (:foreground ,fg-turquoise :weight unspecified))))
    `(magit-section-highlight ((t (:background unspecified))))
    `(magit-section-title ((t (:foreground ,fg-white :inherit unspecified))))

    ;; Transient
    `(transient-disabled-suffix ((t (:background ,bg-red :foreground ,fg-gray :weight bold))))
    `(transient-enabled-suffix ((t (:background ,bg-green :foreground ,bg-bright :weight bold))))
    `(transient-separator ((t (:background ,bg-dim :foreground ,fg-white))))

    ;; Tooltip
    `(tooltip ((t (:background ,bg-default :foreground ,fg-default))))

    ;; Org-agenda
    `(org-agenda-restriction-lock ((t (:background ,bg-white :foreground ,fg-gray))))
    `(org-agenda-calendar-sexp ((t (:foreground ,fg-yellow))))
    `(org-agenda-current-time ((t (:foreground ,fg-orange :weight bold))))
    `(org-agenda-date ((t (:foreground ,bg-cyan))))
    `(org-agenda-date-today ((t (:foreground ,fg-cyan :slant italic))))
    `(org-agenda-date-weekend ((t (:foreground ,fg-cyan))))
    `(org-agenda-done ((t (:foreground ,fg-orange))))
    `(org-agenda-structure ((t (:foreground ,fg-blue))))

    ;; Org-block
    `(org-block ((t (:background ,bg-shadow))))
    `(org-block-begin-line ((t (:background ,bg-purple :foreground ,fg-default))))
    `(org-block-end-line ((t (:background ,bg-purple :foreground ,fg-default))))

    ;; Org-column
    `(org-column ((t (:background ,bg-dim :strike-through nil :underline nil
                       :slant normal :weight normal))))
    `(org-column-title ((t (:background ,bg-dim :underline t :weight bold))))

    ;; Org-document
    `(org-document-info ((t (:foreground ,fg-blue))))
    `(org-document-title ((t (:foreground ,fg-green))))

    ;; Org
    `(org-todo ((t (:foreground ,fg-red :weight bold))))
    `(org-done ((t (:foreground ,fg-green :weight bold))))
    `(org-drawer ((t (:foreground ,fg-blue))))
    `(org-footnote ((t (:foreground ,fg-magenta :underline t))))
    `(org-formula ((t (:foreground ,fg-red))))
    `(org-level-1 ((t (:foreground ,fg-green))))
    `(org-sexp-date ((t (:foreground ,fg-magenta))))
    `(org-table ((t (:foreground ,fg-blue))))
    `(org-upcoming-deadline ((t (:foreground ,fg-red))))
    `(org-checkbox-statistics-done ((t (:foreground ,bg-cyan))))
    `(org-checkbox-statistics-todo ((t (:foreground ,fg-cyan))))
    `(org-date ((t (:foreground ,fg-pink :underline unspecified))))
    `(org-date-selected ((t (:background ,bg-dim :foreground ,fg-pink :underline unspecified))))
    `(org-hide ((t (:foreground ,fg-dim))))
    `(org-headline-done ((t (:foreground ,fg-dim))))
    `(org-level-1 ((t (:foreground ,fg-green))))
    `(org-level-2 ((t (:foreground ,fg-cyan))))
    `(org-level-3 ((t (:foreground ,fg-red))))
    `(org-level-4 ((t (:foreground ,fg-blue))))
    `(org-level-5 ((t (:foreground ,fg-yellow))))
    `(org-level-6 ((t (:foreground ,fg-purple))))
    `(org-level-7 ((t (:foreground ,fg-turquoise))))
    `(org-level-8 ((t (:foreground ,fg-orange))))
    `(org-scheduled ((t (:foreground ,fg-dim))))
    `(org-scheduled-previously ((t (:weight bold))))
    `(org-scheduled-today ((t (:foreground ,fg-default))))
    `(org-time-grid ((t (:foreground ,fg-orange))))
    `(org-mode-line-clock-overrun ((t (:foreground ,fg-orange))))

    ;; Outline
    `(outline-1 ((t (:inherit org-level-1))))
    `(outline-2 ((t (:inherit org-level-2))))
    `(outline-3 ((t (:inherit org-level-3))))
    `(outline-4 ((t (:inherit org-level-4))))
    `(outline-5 ((t (:inherit org-level-5))))
    `(outline-6 ((t (:inherit org-level-6))))
    `(outline-7 ((t (:inherit org-level-7))))
    `(outline-8 ((t (:inherit org-level-8))))

    ;; Package
    `(package-status-avail-obso ((t (:foreground ,bg-green))))
    `(package-status-available ((t (:foreground ,fg-green))))
    `(package-status-dependency ((t (:foreground ,fg-dim))))
    `(package-status-installed ((t (:foreground ,fg-default))))

    ;; Pretty-print ^L highlight
    `(pp^L-highlight ((t (:box unspecified :foreground ,fg-bright))))

    ;; Rainbow delimiters
    `(rainbow-delimiters-depth-1-face ((t (:foreground ,fg-red))))
    `(rainbow-delimiters-depth-2-face ((t (:foreground ,fg-orange))))
    `(rainbow-delimiters-depth-3-face ((t (:foreground ,fg-yellow))))
    `(rainbow-delimiters-depth-4-face ((t (:foreground ,fg-green))))
    `(rainbow-delimiters-depth-4-face ((t (:foreground ,fg-turquoise))))
    `(rainbow-delimiters-depth-6-face ((t (:foreground ,fg-cyan))))
    `(rainbow-delimiters-depth-7-face ((t (:foreground ,fg-blue))))
    `(rainbow-delimiters-depth-8-face ((t (:foreground ,fg-purple))))
    `(rainbow-delimiters-depth-9-face ((t (:foreground ,fg-magenta))))
    `(rainbow-delimiters-unmatched-face ((t (:background ,bg-red :foreground unspecified))))

    ;; Rebase mode
    `(rebase-mode-description-face ((t (:foreground ,fg-bright))))

    ;; RST -  re-structured-text
    `(rst-level-1 ((t (:background unspecified))))
    `(rst-level-2 ((t (:background unspecified))))
    `(rst-level-3 ((t (:background unspecified))))
    `(rst-level-4 ((t (:background unspecified))))
    `(rst-level-5 ((t (:background unspecified))))
    `(rst-level-6 ((t (:background unspecified))))

    ;; Sh
    `(sh-escaped-newline ((t (:foreground ,fg-purple :inherit unspecified :weight bold))))
    `(sh-heredoc ((t (:foreground ,fg-green))))
    `(sh-quoted-exec ((t (:foreground ,fg-pink))))

    ;; Show paren
    `(show-paren-match ((t (:inverse-video t))))
    `(show-paren-mismatch ((t (:background unspecified :foreground ,fg-red))))

    ;; Slime
    `(slime-repl-input-face ((t (:foreground ,fg-bright))))
    `(slime-repl-inputed-output-face ((t (:foreground ,fg-bright))))
    `(slime-repl-output-face ((t (:foreground ,fg-default))))
    `(slime-repl-prompt-face ((t (:foreground ,fg-blue))))

    ;; Smerge
    `(smerge-base ((t (:background ,bg-green))))
    `(smerge-markers ((t (:background ,bg-bright))))
    `(smerge-mine ((t (:background ,bg-blue))))
    `(smerge-other ((t (:background ,bg-red))))
    `(smerge-refined-added ((t (:inherit diff-refine-added))))
    `(smerge-refined-change ((t (:inherit diff-refine-change))))
    `(smerge-refined-removed ((t (:inherit diff-refine-removed))))
    `(smerge-lower ((t (:background ,bg-green :foreground ,bg-bright))))

    ;; Term
    `(term-bold ((t (:foreground ,fg-default))))
    `(term-color-black ((t (:background ,bg-default :foreground ,fg-bright))))
    `(term-color-blue ((t (:background ,bg-blue :foreground ,fg-blue))))
    `(term-color-cyan ((t (:background ,bg-cyan :foreground ,fg-cyan))))
    `(term-color-green ((t (:background ,bg-green :foreground ,fg-green))))
    `(term-color-magenta ((t (:background ,bg-magenta :foreground ,fg-magenta))))
    `(term-color-red ((t (:background ,bg-red :foreground ,fg-red))))
    `(term-color-white ((t (:background ,bg-bright :foreground ,fg-default))))
    `(term-color-yellow ((t (:background ,bg-yellow :foreground ,fg-yellow))))

    ;; Texinfo
    `(texinfo-heading ((t (:foreground ,fg-pink :inherit unspecified :height 1.3))))

    ;; Tty
    `(tty-menu-selected-face ((t (:background ,bg-red :foreground ,bg-white))))

    ;; Term
    `(term-color-black ((t (:background ,bg-default :foreground ,fg-default))))
    `(term-color-blue ((t (:background ,bg-blue :foreground ,fg-blue))))
    `(term-color-cyan ((t (:background ,bg-cyan :foreground ,fg-cyan))))
    `(term-color-green ((t (:background ,bg-green :foreground ,fg-green))))
    `(term-color-magenta ((t (:background ,bg-magenta :foreground ,fg-magenta))))
    `(term-color-red ((t (:background ,bg-red :foreground ,fg-red))))
    `(term-color-white ((t (:background ,bg-bright :foreground ,fg-default))))
    `(term-color-yellow ((t (:background ,bg-yellow :foreground ,fg-yellow))))

    ;; Which function
    `(which-func ((t (:foreground ,fg-blue))))

    ;; Whitespace mode
    `(whitespace-empty ((t (:background ,bg-cyan :foreground ,fg-default))))
    `(whitespace-hspace ((t (:background ,bg-bright :foreground ,fg-bright))))
    `(whitespace-indentation ((t (:background ,bg-yellow :foreground unspecified))))
    `(whitespace-newline ((t (:foreground ,fg-pink))))
    `(whitespace-space ((t (:background unspecified :foreground ,fg-pink))))
    `(whitespace-space-after-tab ((t (:background ,bg-orange :foreground unspecified))))
    `(whitespace-space-before-tab ((t (:background ,bg-orange :foreground unspecified))))
    `(whitespace-tab ((t (:background unspecified :underline ,bg-bright))))
    `(whitespace-trailing ((t (:background ,bg-orange :foreground unspecified))))
    `(whitespace-big-indent ((t (:background ,bg-red :foreground ,fg-gray))))
    `(whitespace-line ((t (:background ,bg-red :foreground ,fg-black))))

    ;; yas
    ;; yas--field-debug-face
    `(yas-field-highlight-face ((t (:background ,bg-black :foreground ,fg-blue))))

    ;; Widget
    `(widget-button ((t (:inherit button))))
    `(widget-button-pressed ((t (:inherit widget-button :weight bold))))
    `(widget-documentation ((t (:inherit font-lock-doc-face))))
    `(widget-field ((t (:background ,bg-blue :foreground ,fg-white :box (:color ,bg-blue :line-width 2)))))

    ;; Rpm-spec-mode
    `(rpm-spec-tag-face ((t (:foreground ,fg-blue))))
    `(rpm-spec-obsolete-tag-face ((t (:background ,bg-red))))
    `(rpm-spec-macro-face ((t (:foreground ,fg-yellow))))
    `(rpm-spec-var-face ((t (:foreground ,fg-cyan))))
    `(rpm-spec-doc-face ((t (:foreground ,fg-magenta))))
    `(rpm-spec-dir-face ((t (:foreground ,fg-turquoise))))
    `(rpm-spec-package-face ((t (:foreground ,fg-red))))
    `(rpm-spec-ghost-face ((t (:foreground ,fg-red))))
    `(rpm-spec-stion-face ((t (:foreground ,fg-yellow :underline t))))

    ;; Window dividers
    `(window-divider ((t (:foreground ,bg-dim))))
    `(window-divider-first-pixel ((t (:inherit window-divider))))
    `(window-divider-last-pixel ((t (:inherit window-divider))))

    ;; Vertical-border
    `(vertical-border ((t nil)))))

;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
    (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'moebius-yellow)
;; moebius-theme.el ends here
