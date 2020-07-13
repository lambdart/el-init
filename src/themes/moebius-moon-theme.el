;; moebius-theme.el --- An Emacs dark theme -*- lexical-binding: t; -*-
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
;; Jean "moebius" Giraud, thank you for you priceless
;; work, may you rest in peace...
;;
;;; Code:
;;
(require 'color)

;; defines moebius theme
(deftheme moebius
  "Simple and minimal Emacs dark theme.")

;; Define colours pallets.
(let ((class '((class color) (min-colors 89)))
      ;; all colors has to have a tuple (background . foreground)
      ;; special colors
      (bg-default "#000000") (fg-default "#eee5de")
      (bg-bright  "#cdc5bf") (fg-bright  "#fffff0")
      (bg-dim     "#262626") (fg-dim     "#969696")
      (bg-shadow  "#333333") (fg-shadow  "#f5f5f5")

      ;; basic colors
      (bg-black     "#1f1f1f") (fg-black     "#666666")
      (bg-white     "#ffefdb") (fg-white     "#ffefdb")
      (bg-red       "#8b4c39") (fg-red       "#cd6889")
      (bg-gray      "#6c7b8b") (fg-gray      "#b9d3ee")
      (bg-orange    "#8b5a2b") (fg-orange    "#eec591")
      (bg-yellow    "#8b864e") (fg-yellow    "#eec591")
      (bg-green     "#548b54") (fg-green     "#43cd80")
      (bg-blue      "#4a708b") (fg-blue      "#6ca6cd")
      (bg-purple    "#5d478b") (fg-purple    "#ab82ff")
      (bg-turquoise "#1f3f2c") (fg-turquoise "#54a875")
      (bg-cyan      "#1f3f3f") (fg-cyan      "#54a8a8")
      (bg-magenta   "#8787af") (fg-magenta   "#9754a8")
      (bg-pink      "#8b5f65") (fg-pink      "#cd8c95"))

  ;; set faces
  (custom-theme-set-faces
   'moebius
   ;; custom-set-faces was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.

   ;; Default
   `(default ((,class (:background ,bg-default :foreground ,fg-default))))
   `(c-nonbreakable-space-face ((,class (:background ,bg-magenta :foreground ,fg-white :weight normal))))
   `(cursor ((,class (:background ,fg-dim))))
   `(error ((,class (:foreground ,fg-red))))
   `(italic ((,class (:slant italic))))
   `(match ((,class (:background ,bg-dim :foreground ,fg-blue))))
   ;; `(match ((,class (:foreground ,fg-blue))))
   `(nobreak-hyphen ((,class (:foreground ,fg-red))))
   `(minibuffer-prompt ((,class (:foreground ,fg-blue))))
   `(warning ((,class (:foreground ,fg-pink :weight unspecified))))
   `(region ((,class (:background ,bg-blue :foreground ,bg-white))))
   `(shadow ((,class (:foreground ,fg-dim))))
   `(success ((,class (:foreground ,fg-green :weight bold))))
   `(trailing-whitespace ((,class (:background ,fg-red))))
   `(info-node ((,class (:foreground ,fg-orange :slant italic :weight bold))))
   `(lazy-highlight ((,class (:background ,bg-blue :foreground ,bg-white))))
   `(line-number ((,class (:inherit (shadow black) :back))))
   `(line-number-current-line ((,class (:inherit line-number))))
   `(secondary-selection ((,class (:background ,bg-yellow :foreground ,fg-black))))
   `(escape-glyph ((,class (:foreground ,fg-red))))

   ;; Font
   `(font-lock-builtin-face ((,class (:foreground ,fg-blue))))
   `(font-lock-comment-delimiter-face ((,class (:foreground ,fg-dim :inherit unspecified))))
   `(font-lock-comment-face ((,class (:foreground ,fg-dim))))
   `(font-lock-constant-face ((,class (:foreground ,fg-red))))
   `(font-lock-constant-face ((,class (:foreground ,fg-red))))
   `(font-lock-doc-face ((,class (:foreground ,fg-green :inherit unspecified))))
   `(font-lock-function-name-face ((,class (:foreground ,fg-blue))))
   `(font-lock-keyword-face ((,class (:foreground ,fg-orange))))
   `(font-lock-negation-char-face ((,class (:foreground ,fg-red))))
   `(font-lock-regexp-grouping-backslash ((,class (:foreground ,fg-red))))
   `(font-lock-regexp-grouping-construct ((,class (:foreground ,fg-pink))))
   `(font-lock-string-face ((,class (:foreground ,fg-pink))))
   `(font-lock-type-face ((,class (:foreground ,fg-purple))))
   `(font-lock-variable-name-face ((,class (:foreground ,fg-yellow))))
   `(font-lock-warning-face ((,class (:foreground ,fg-orange :inherit unspecified :weight bold))))

   ;; Fringe
   `(fringe ((,class nil)))
   ;;`(fringe ((,class (:background unspecified :inherit default))))

   ;; Mode-line
   `(mode-line ((,class (:background ,bg-dim :foreground ,fg-white
                                     :box (:color ,bg-dim :line-width 2 :style nil)))))

   `(mode-line-inactive ((,class (:background ,bg-dim :foreground ,bg-gray
                                              :box (:line-width 2 :color ,bg-dim :style nil)))))

   `(mode-line-buffer-id ((,class (:foreground ,fg-blue :weight normal))))

   ;; Highlight
   `(highlight ((,class (:background ,bg-gray))))
   `(highlight-numbers-number ((,class (:foreground ,fg-cyan :inherit unspecified))))
   `(highlight-80+ ((,class (:underline (:color ,fg-red :style wave) :background unspecified))))

   ;; Highlight line
   `(hl-line ((,class (:background ,bg-dim))))

   ;; Highlight indent
   `(hl-indent-block-face-1 ((,class (:background ,bg-red))))
   `(hl-indent-block-face-2 ((,class (:background ,bg-pink))))
   `(hl-indent-block-face-3 ((,class (:background ,bg-orange))))
   `(hl-indent-block-face-4 ((,class (:background ,bg-yellow))))
   `(hl-indent-block-face-5 ((,class (:background ,bg-green))))
   `(hl-indent-block-face-6 ((,class (:background ,bg-turquoise))))
   `(hl-indent-face ((,class (:inherit unspecified :background ,bg-dim))))

   ;; Holiday
   `(holiday ((,class (:background ,fg-red))))

   ;; Homoglyph
   `(homoglyph ((,class (:foreground ,fg-red))))

   ;; Header Line
   `(header-line ((,class (:background ,bg-dim :foreground ,fg-gray))))

   ;; Popup
   `(popup-isearch-match ((,class (:inherit black :background ,bg-blue))))

   ;; Pulse
   `(pulse-highlight-face ((,class (:background ,bg-yellow :foreground ,bg-bright))))
   `(pulse-highlight-start-face ((,class (:background ,bg-yellow :foreground ,bg-bright))))

   ;; Elpa
   `(epa-string ((,class (:foreground ,fg-blue))))

   ;; Eww
   `(eww-valid-certificate ((,class (:foreground ,fg-green :weight bold))))

   ;; Links
   `(link ((,class (:foreground ,fg-orange :underline t))))
   `(link-visited ((,class (:foreground ,fg-magenta :underline t))))

   ;; Completions
   `(completions-common-part ((,class (:foreground ,fg-blue))))

   ;; Icomplete
   `(icomplete-first-match ((,class (:foreground ,fg-red :weight normal))))

   ;; Isearch
   `(isearch ((,class (:background ,bg-blue :foreground ,fg-white))))
   `(isearch-fail ((,class (:background ,bg-default :foreground ,fg-red))))

   ;; Compilation
   `(compilation-info ((,class (:foreground ,fg-green :weight normal))))
   `(compilation-mode-line-exit ((,class (:inherit compilation-info :foreground ,fg-green :weight bold))))

   ;; CSS
   `(css-property ((,class (:foreground ,fg-orange))))
   `(css-proprietary-property ((,class (:foreground ,fg-red :inherit unspecified :slant italic))))
   `(css-selector ((,class (:foreground ,fg-blue))))

   ;; Custom
   `(custom-button ((,class (:background ,bg-white :foreground ,bg-black
                                         :box (:line-width 1 :color ,bg-white)
                                         :weight ultra-light :height 1.1 :width ultra-condensed))))
   `(custom-button-pressed-unraised ((,class (:inherit custom-button-unraised :foreground ,fg-red))))
   `(custom-changed ((,class (:background ,bg-blue :foreground ,fg-white))))
   `(custom-comment-tag ((,class (:foreground ,fg-blue))))
   `(custom-group-tag ((,class (:inherit variable-pitch :foreground ,fg-blue :weight bold :height 1.2))))
   `(custom-invalid ((,class (:background ,bg-red :foreground ,fg-black))))
   `(custom-modified ((,class (:background ,bg-blue :foreground ,fg-white))))
   `(custom-set ((,class (:background ,bg-white :foreground ,fg-blue))))
   `(custom-themed ((,class (:background ,bg-blue :foreground ,fg-white))))
   `(custom-variable-tag ((,class (:foreground ,fg-blue :weight bold))))

   ;; Company
   `(company-preview ((,class (:background nil :foreground ,fg-blue))))
   `(company-preview-common ((,class (:inherit nil :background ,bg-dim
                                               :foreground ,fg-blue :weight normal))))
   `(company-echo-common ((,class (:background ,bg-dim :foreground ,fg-blue))))
   `(company-scrollbar-bg ((nil (:background ,bg-black))))
   `(company-scrollbar-fg ((nil (:background ,bg-dim))))
   `(company-tooltip ((,class (:background ,bg-black :foreground ,fg-default))))
   `(company-tooltip-common ((,class (:foreground ,fg-blue))))
   `(company-tooltip-selection ((,class (:background ,bg-dim))))
   `(company-tooltip-annotation ((,class (:foreground ,fg-blue))))
   `(company-tooltip-search ((,class (:background ,bg-yellow :inherit unspecified))))
   `(company-tooltip-search-selection ((,class (:background ,bg-yellow :inherit unspecified))))
   `(company-template-field ((,class (:background ,bg-dim :foreground ,fg-yellow))))

   ;; Dashboard
   `(dashboard-footer ((,class (:foreground ,fg-black))))
   `(dashboard-heading ((,class (:foreground ,fg-blue))))
   `(dashboard-text-banner ((,class (:foreground ,fg-black))))

   ;; Diff
   `(diff-added ((,class (:background ,bg-green :foreground ,fg-bright))))
   `(diff-refine-changed ((,class (:background ,bg-yellow :foreground ,fg-bright))))
   `(diff-removed ((,class (:background ,bg-red :foreground ,fg-bright))))
   `(diff-changed ((,class (:background ,bg-orange))))
   `(diff-file-header ((,class (:foreground ,bg-white :background unspecified))))
   `(diff-function ((,class (:inherit unspecified :foreground ,fg-orange))))
   `(diff-header ((,class (:background ,bg-bright))))
   `(diff-hl-change ((,class (:foreground ,bg-yellow :background unspecified :inherit diff-changed))))
   `(diff-hl-delete ((,class (:foreground ,bg-red :inherit diff-removed))))
   `(diff-hl-insert ((,class (:foreground ,bg-green :inherit diff-added))))
   `(diff-hunk-header ((,class (:inherit unspecified :weight bold :foreground ,fg-yellow :underline t))))
   `(diff-indicator-added ((,class (:foreground ,fg-green :weight bold :inherit unspecified))))
   `(diff-indicator-changed ((,class (:foreground ,fg-yellow :weight bold :inherit unspecified ))))
   `(diff-indicator-removed ((,class (:foreground ,fg-red :weight bold :inherit unspecified))))
   `(diff-refine-added ((,class (:foreground ,fg-green :background unspecified :inherit unspecified))))
   `(diff-refine-removed ((,class (:foreground ,fg-red :background unspecified :inherit unspecified))))

   ;; Dired
   `(dired-flagged ((,class (:inherit region))))
   `(dired-header ((,class (:foreground ,fg-turquoise :weight bold :inherit unspecified))))
   `(dired-mark ((,class (:foreground ,fg-pink :inherit unspecified))))
   `(dired-marked ((,class (:inherit nil :background ,bg-magenta :foreground ,fg-bright))))

   ;; Dictionary
   ;; diary                                     yellow
   ;; dictionary-button-face                    white
   ;; dictionary-reference-face                 yellow
   ;; `(dictionary-reference-face ((,class (:inherit nil :background nil :foreground ,fg-orange)))
   ;; dictionary-word-definition-face           gray (another font) (italic)
   ;; dictionary-word-entry-face                gray (italic)

   ;; Ediff
   `(ediff-current-diff-A ((,class (:inherit diff-removed))))
   `(ediff-current-diff-B ((,class (:inherit diff-added))))
   `(ediff-current-diff-C ((,class (:inherit diff-changed))))
   `(ediff-fine-diff-A ((,class (:inherit diff-refine-removed))))
   `(ediff-fine-diff-B ((,class (:inherit diff-refine-added))))
   `(ediff-fine-diff-C ((,class (:inherit diff-refine-change))))

   ;; Ert
   `(ert-test-result-expected ((,class (:background unspecified :foreground ,fg-green))))
   `(ert-test-result-unexpected ((,class (:background unspecified :foreground ,fg-red))))

   ;; Eshell
   `(eshell-fringe-status-failure ((,class (:foreground ,fg-red))))
   `(eshell-fringe-status-success ((,class (:foreground ,fg-green))))
   `(eshell-ls-archive ((,class (:foreground ,fg-pink :weight unspecified))))
   `(eshell-ls-backup ((,class (:foreground ,fg-orange))))
   `(eshell-ls-clutter ((,class (:foreground ,fg-dim :weight unspecified))))
   `(eshell-ls-directory ((,class (:foreground ,fg-blue :weight unspecified))))
   `(eshell-ls-executable ((,class (:foreground ,fg-red :weight unspecified))))
   `(eshell-ls-missing ((,class (:foreground ,fg-red :weight bold))))
   `(eshell-ls-product ((,class (:foreground ,fg-purple))))
   `(eshell-ls-readonly ((,class (:foreground ,fg-magenta))))
   `(eshell-ls-special ((,class (:foreground ,fg-turquoise))))
   `(eshell-ls-symlink ((,class (:foreground ,fg-cyan :weight unspecified))))
   `(eshell-ls-unreadable ((,class (:foreground ,fg-red))))
   `(eshell-prompt ((,class (:foreground ,fg-blue :weight unspecified))))

   ;; Flymake
   `(flymake-errline ((,class (:background unspecified :underline (:color ,fg-red :style wave)))))
   `(flymake-infoline ((,class (:background unspecified :underline (:color ,fg-blue :style wave)))))
   `(flymake-warnline ((,class (:background unspecified :underline (:color ,fg-orange :style wave)))))

   ;; Flyspell
   `(flyspell-duplicate ((,class (:inherit unspecified :underline (:color ,fg-orange :style wave)))))
   `(flyspell-incorrect ((,class (:inherit unspecified :underline (:color ,fg-red :style wave)))))

   ;; Flycheck
   `(flycheck-warning ((,class (:underline (:color ,fg-red :style wave)))))

   ;; Gnus
   `(gnus-button ((,class (:weight bold))))
   `(gnus-cite-1 ((,class (:foreground ,fg-red))))
   `(gnus-cite-10 ((,class (:foreground ,fg-pink))))
   `(gnus-cite-11 ((,class (:foreground ,fg-bright))))
   `(gnus-cite-2 ((,class (:foreground ,fg-orange))))
   `(gnus-cite-3 ((,class (:foreground ,fg-yellow))))
   `(gnus-cite-4 ((,class (:foreground ,fg-green))))
   `(gnus-cite-5 ((,class (:foreground ,fg-turquoise))))
   `(gnus-cite-6 ((,class (:foreground ,fg-cyan))))
   `(gnus-cite-7 ((,class (:foreground ,fg-blue))))
   `(gnus-cite-8 ((,class (:foreground ,fg-purple))))
   `(gnus-cite-9 ((,class (:foreground ,fg-magenta))))
   `(gnus-group-mail-3 ((,class (:foreground ,fg-cyan :weight bold))))
   `(gnus-group-mail-3-empty ((,class (:foreground ,fg-cyan))))
   `(gnus-group-news-3 ((,class (:foreground ,fg-red :weight bold))))
   `(gnus-group-news-3-empty ((,class (:foreground ,fg-red))))
   `(gnus-header-content ((,class (:foreground ,fg-dim :slant italic))))
   `(gnus-header-from ((,class (:weight bold))))
   `(gnus-header-name ((,class (:foreground ,fg-blue :weight bold))))
   `(gnus-header-newsgroups ((,class (:foreground ,fg-bright :weight bold))))
   `(gnus-header-subject ((,class (:foreground ,fg-yellow))))
   `(gnus-signature ((,class (:foreground ,fg-dim :slant italic))))
   `(gnus-splash ((,class (:foreground ,fg-default))))
   `(gnus-summary-cancelled ((,class (:foreground ,fg-dim :background unspecified :strike-through t))))
   `(gnus-summary-high-ancient ((,class (:inherit gnus-summary-normal-ancient :weight bold))))
   `(gnus-summary-high-read ((,class (:inherit gnus-summary-normal-read :weight bold))))
   `(gnus-summary-high-ticked ((,class (:inherit gnus-summary-normal-ticked :weight bold))))
   `(gnus-summary-high-unread ((,class (:foreground ,fg-pink))))
   `(gnus-summary-low-ancient ((,class (:inherit gnus-summary-normal-ancient :slant italic))))
   `(gnus-summary-low-read ((,class (:inherit gnus-summary-normal-read :slant italic))))
   `(gnus-summary-low-ticked ((,class (:inherit gnus-summary-normal-ticked :slant italic))))
   `(gnus-summary-low-unread ((,class (:inherit gnus-summary-normal-unread :slant italic))))
   `(gnus-summary-normal-ancient ((,class (:foreground ,fg-cyan))))
   `(gnus-summary-normal-read ((,class (:foreground ,fg-dim))))
   `(gnus-summary-normal-ticked ((,class (:foreground ,fg-turquoise))))
   `(gnus-summary-normal-unread ((,class (:foreground ,fg-default))))
   `(gnus-group-mail-1 ((,class (:foreground ,fg-red :weight bold))))
   `(gnus-group-mail-1-empty ((,class (:foreground ,fg-purple))))
   `(gnus-group-mail-2 ((,class (:foreground ,fg-magenta :weight bold))))
   `(gnus-group-mail-2-empty ((,class (:foreground ,fg-magenta))))
   `(gnus-group-mail-low ((,class (:foreground ,fg-magenta :weight bold))))
   `(gnus-group-mail-low-empty ((,class (:foreground ,fg-magenta))))
   `(gnus-summary-selected ((,class (:background ,bg-blue :foreground ,fg-white))))

   ;; Go-mode
   `(go-coverage-10 ((,class (:foreground ,fg-green))))
   `(go-coverage-7 ((,class (:foreground ,fg-green))))
   `(go-coverage-8 ((,class (:foreground ,fg-green))))
   `(go-coverage-9 ((,class (:foreground ,fg-green))))
   `(go-coverage-covered ((,class (:foreground ,fg-green))))

   ;; Identica
   `(identica-stripe-face ((,class (:background ,bg-bright))))
   `(identica-uri-face ((,class (:foreground ,fg-orange :underline t))))
   `(identica-username-face ((,class (:foreground ,fg-blue :weight bold :underline unspecified))))

   ;; Ibuffer
   `(ibuffer-locked-buffer ((,class (:foreground ,bg-red))))

   ;; Ido
   `(ido-only-match ((,class (:foreground ,fg-green))))
   `(ido-indicator ((,class (:background ,bg-bright :foreground ,fg-white :width condensed))))

   ;; Iedit
   `(iedit-occurrence ((,class (:background ,bg-yellow :foreground ,fg-default))))

   ;; Man
   `(Man-overstrike ((,class (:foreground ,fg-blue :weight bold))))
   `(Man-underline ((,class (:foreground ,fg-red))))

   ;; Message
   `(message-header-cc ((,class (:foreground ,fg-dim))))
   `(message-header-name ((,class (:inherit gnus-header-name))))
   `(message-header-newsgroups ((,class (:foreground ,fg-dim :weight bold))))
   `(message-header-other ((,class (:inherit gnus-header-content))))
   `(message-header-subject ((,class (:inherit gnus-header-subject))))
   `(message-header-to ((,class (:foreground ,fg-dim :weight bold))))
   `(message-header-xheader ((,class (:foreground ,fg-dim :slant italic))))
   `(message-mml ((,class (:foreground ,fg-green))))
   `(message-separator ((,class (:foreground ,fg-blue))))

   ;; Magit
   `(magit-branch-current ((,class (:inherit nil :background ,bg-blue :foreground ,bg-white))))
   `(magit-branch-local ((,class (:background ,bg-magenta :foreground ,bg-white))))
   `(magit-branch-remote ((,class (:background ,bg-green :foreground ,fg-white))))
   `(magit-log-author ((,class (:foreground ,fg-red :slant normal :weight normal))))
   `(magit-signature-expired ((,class (:foreground ,fg-yellow))))
   `(magit-signature-revoked ((,class (:foreground ,fg-magenta))))
   `(magit-bisect-bad ((,class (:foreground ,fg-red))))
   `(magit-bisect-good ((,class (:foreground ,fg-green))))
   `(magit-bisect-skip ((,class (:foreground ,fg-dim))))
   `(magit-blame-date ((,class (:foreground ,fg-pink :inherit magit-blame-heading))))
   `(magit-blame-hash ((,class (:foreground ,fg-magenta :inherit magit-blame-heading))))
   `(magit-blame-header ((,class (:foreground ,fg-green :background ,bg-dim
                                              :weight bold :inherit unspecified))))
   `(magit-blame-heading ((,class (:foreground ,fg-default :background ,bg-bright))))
   `(magit-blame-name ((,class (:foreground ,fg-turquoise :inherit magit-blame-heading))))
   `(magit-blame-summary ((,class (:foreground ,fg-green :inherit magit-blame-heading))))
   `(magit-branch ((,class (:foreground ,fg-pink :weight bold :inherit unspecified))))
   `(magit-diff-added ((,class (:foreground unspecified :background unspecified :inherit diff-added))))
   `(magit-diff-added-highlight ((,class (:foreground unspecified :background unspecified
                                                      :inherit magit-diff-added))))
   `(magit-diff-context ((,class (:foreground unspecified :inherit shadow))))
   `(magit-diff-context-highlight ((,class (:foreground unspecified :background ,bg-dim
                                                        :inherit magit-diff-context))))
   `(magit-diff-file-heading ((,class (:foreground unspecified :underline unspecified
                                                   :inherit diff-file-header))))
   `(magit-diff-removed ((,class (:foreground unspecified :background unspecified
                                              :inherit diff-removed))))
   `(magit-diff-removed-highlight ((,class (:foreground unspecified :background unspecified
                                                        :inherit magit-diff-removed))))
   `(magit-diffstat-removed ((,class (:foreground ,fg-red))))
   `(magit-item-highlight ((,class (:inherit unspecified))))

   ;; Magit logs
   `(magit-log-head-label-default ((,class (:foreground ,fg-default :background ,bg-cyan
                                                        :box (:color ,bg-cyan :line-width 2 :style nil)))))
   `(magit-log-head-label-head ((,class (:foreground ,fg-default :background ,bg-blue
                                                     :box (:color ,bg-blue :line-width 2 :style nil)))))
   `(magit-log-head-label-local ((,class (:foreground ,bg-white :background ,bg-magenta
                                                      :box (:color ,bg-magenta :line-width 2 :style nil)))))
   `(magit-log-head-label-remote ((,class (:foreground ,fg-white :background ,bg-green
                                                       :box (:color ,bg-green :line-width 2 :style nil)))))
   `(magit-log-head-label-tags ((,class (:foreground ,fg-default :background ,bg-orange
                                                     :box (:color ,bg-orange :line-width 2 :style nil)))))
   `(magit-log-sha1 ((,class (:foreground ,fg-default :background ,bg-blue
                                          :box (:color ,bg-blue :line-width 2 :style nil)))))

   `(magit-process-ng ((,class (:foreground ,fg-red :inherit unspecified))))
   `(magit-process-ok ((,class (:foreground ,fg-green :inherit unspecified))))
   `(magit-section-heading ((,class (:foreground ,fg-turquoise :weight unspecified))))
   `(magit-section-highlight ((,class (:background unspecified))))
   `(magit-section-title ((,class (:foreground ,fg-white :inherit unspecified))))

   ;; Transient
   `(transient-disabled-suffix ((,class (:background ,bg-red :foreground ,fg-gray :weight bold))))
   `(transient-enabled-suffix ((,class (:background ,bg-green :foreground ,bg-bright :weight bold))))
   `(transient-separator ((,class (:background ,bg-dim :foreground ,fg-white))))

   ;; Tooltip
   `(tooltip ((,class (:foreground ,fg-default :background ,bg-gray))))

   ;; Org-agenda
   `(org-agenda-restriction-lock ((,class (:background ,bg-white :foreground ,fg-gray))))
   `(org-agenda-calendar-sexp ((,class (:foreground ,fg-yellow))))
   `(org-agenda-current-time ((,class (:foreground ,fg-orange :weight bold))))
   `(org-agenda-date ((,class (:foreground ,bg-cyan))))
   `(org-agenda-date-today ((,class (:foreground ,fg-cyan :slant italic))))
   `(org-agenda-date-weekend ((,class (:foreground ,fg-cyan))))
   `(org-agenda-done ((,class (:foreground ,fg-orange))))
   `(org-agenda-structure ((,class (:foreground ,fg-blue))))

   ;; Org-block
   `(org-block ((,class (:background ,bg-shadow :extend t))))
   `(org-block-begin-line ((,class (:background ,bg-gray :foreground ,fg-bright :extend t))))
   `(org-block-end-line ((,class (:background ,bg-gray :foreground ,fg-bright :extend t))))

   ;; Org-column
   `(org-column ((,class (:background ,bg-gray :strike-through nil :underline nil :slant normal :weight normal))))
   `(org-column-title ((,class (:background ,bg-gray :underline t :weight bold))))

   ;; Org-document
   `(org-document-info ((,class (:foreground ,fg-blue))))
   `(org-document-title ((,class (:foreground ,fg-green))))

   ;; Org
   `(org-done ((,class (:foreground ,fg-green :weight bold))))
   `(org-drawer ((,class (:foreground ,fg-blue))))
   `(org-footnote ((,class (:foreground ,fg-magenta :underline t))))
   `(org-formula ((,class (:foreground ,fg-red))))
   `(org-level-1 ((,class (:foreground ,fg-green))))
   `(org-sexp-date ((,class (:foreground ,fg-magenta))))
   `(org-table ((,class (:foreground ,fg-blue))))
   `(org-upcoming-deadline ((,class (:foreground ,fg-red))))
   `(org-checkbox-statistics-done ((,class (:foreground ,bg-cyan))))
   `(org-checkbox-statistics-todo ((,class (:foreground ,fg-cyan))))
   `(org-date ((,class (:foreground ,fg-pink :underline unspecified))))
   `(org-headline-done ((,class (:foreground ,fg-dim))))
   `(org-level-1 ((,class (:foreground ,fg-green))))
   `(org-level-2 ((,class (:foreground ,fg-cyan))))
   `(org-level-3 ((,class (:foreground ,fg-red))))
   `(org-level-4 ((,class (:foreground ,fg-blue))))
   `(org-level-5 ((,class (:foreground ,fg-yellow))))
   `(org-level-6 ((,class (:foreground ,fg-purple))))
   `(org-level-7 ((,class (:foreground ,fg-turquoise))))
   `(org-level-8 ((,class (:foreground ,fg-orange))))
   `(org-scheduled ((,class (:foreground ,fg-dim))))
   `(org-scheduled-previously ((,class (:weight bold))))
   `(org-scheduled-today ((,class (:foreground ,fg-default))))
   `(org-time-grid ((,class (:foreground ,fg-orange))))

   ;; Outline
   `(outline-1 ((,class (:inherit org-level-1))))
   `(outline-2 ((,class (:inherit org-level-2))))
   `(outline-3 ((,class (:inherit org-level-3))))
   `(outline-4 ((,class (:inherit org-level-4))))
   `(outline-5 ((,class (:inherit org-level-5))))
   `(outline-6 ((,class (:inherit org-level-6))))
   `(outline-7 ((,class (:inherit org-level-7))))
   `(outline-8 ((,class (:inherit org-level-8))))

   ;; Package
   `(package-status-avail-obso ((,class (:foreground ,bg-green))))
   `(package-status-available ((,class (:foreground ,fg-green))))
   `(package-status-dependency ((,class (:foreground ,fg-dim))))
   `(package-status-installed ((,class (:foreground ,fg-default))))

   ;; Pretty-print ^L highlight
   `(pp^L-highlight ((,class (:box unspecified :foreground ,fg-bright))))

   ;; Rainbow delimiters
   `(rainbow-delimiters-depth-1-face ((,class (:foreground ,fg-red))))
   `(rainbow-delimiters-depth-2-face ((,class (:foreground ,fg-orange))))
   `(rainbow-delimiters-depth-3-face ((,class (:foreground ,fg-yellow))))
   `(rainbow-delimiters-depth-4-face ((,class (:foreground ,fg-green))))
   `(rainbow-delimiters-depth-4-face ((,class (:foreground ,fg-turquoise))))
   `(rainbow-delimiters-depth-6-face ((,class (:foreground ,fg-cyan))))
   `(rainbow-delimiters-depth-7-face ((,class (:foreground ,fg-blue))))
   `(rainbow-delimiters-depth-8-face ((,class (:foreground ,fg-purple))))
   `(rainbow-delimiters-depth-9-face ((,class (:foreground ,fg-magenta))))
   `(rainbow-delimiters-unmatched-face ((,class (:background ,bg-red :foreground unspecified))))

   ;; Rebase mode
   `(rebase-mode-description-face ((,class (:foreground ,fg-bright))))

   ;; RST -  re-structured-text
   `(rst-level-1 ((,class (:background unspecified))))
   `(rst-level-2 ((,class (:background unspecified))))
   `(rst-level-3 ((,class (:background unspecified))))
   `(rst-level-4 ((,class (:background unspecified))))
   `(rst-level-5 ((,class (:background unspecified))))
   `(rst-level-6 ((,class (:background unspecified))))

   ;; Sh
   `(sh-escaped-newline ((,class (:foreground ,fg-purple :inherit unspecified :weight bold))))
   `(sh-heredoc ((,class (:foreground ,fg-green))))
   `(sh-quoted-exec ((,class (:foreground ,fg-pink))))

   ;; Show paren
   `(show-paren-match ((,class (:inverse-video t))))
   `(show-paren-mismatch ((,class (:background unspecified :foreground ,fg-red))))

   ;; Slime
   `(slime-repl-input-face ((,class (:foreground ,fg-bright))))
   `(slime-repl-inputed-output-face ((,class (:foreground ,fg-bright))))
   `(slime-repl-output-face ((,class (:foreground ,fg-default))))
   `(slime-repl-prompt-face ((,class (:foreground ,fg-blue))))

   ;; Smerge
   `(smerge-base ((,class (:background ,bg-green))))
   `(smerge-markers ((,class (:background ,bg-bright))))
   `(smerge-mine ((,class (:background ,bg-blue))))
   `(smerge-other ((,class (:background ,bg-red))))
   `(smerge-refined-added ((,class (:inherit diff-refine-added))))
   `(smerge-refined-change ((,class (:inherit diff-refine-change))))
   `(smerge-refined-removed ((,class (:inherit diff-refine-removed))))
   `(smerge-lower ((,class (:background ,bg-green :foreground ,bg-bright))))

   ;; Term
   `(term-bold ((,class (:foreground ,fg-default))))
   `(term-color-black ((,class (:background ,bg-default :foreground ,fg-bright))))
   `(term-color-blue ((,class (:background ,bg-blue :foreground ,fg-blue))))
   `(term-color-cyan ((,class (:background ,bg-cyan :foreground ,fg-cyan))))
   `(term-color-green ((,class (:background ,bg-green :foreground ,fg-green))))
   `(term-color-magenta ((,class (:background ,bg-magenta :foreground ,fg-magenta))))
   `(term-color-red ((,class (:background ,bg-red :foreground ,fg-red))))
   `(term-color-white ((,class (:background ,bg-bright :foreground ,fg-default))))
   `(term-color-yellow ((,class (:background ,bg-yellow :foreground ,fg-yellow))))

   ;; Texinfo
   `(texinfo-heading ((,class (:foreground ,fg-pink :inherit unspecified :height 1.3))))

   ;; Tty
   `(tty-menu-selected-face ((,class (:background ,bg-red :foreground ,bg-white))))

   ;; Term
   `(term-color-black ((,class (:background ,bg-default :foreground ,fg-bright))))
   `(term-color-blue ((,class (:background ,bg-blue :foreground ,fg-blue))))
   `(term-color-cyan ((,class (:background ,bg-cyan :foreground ,fg-cyan))))
   `(term-color-green ((,class (:background ,bg-green :foreground ,fg-green))))
   `(term-color-magenta ((,class (:background ,bg-magenta :foreground ,fg-magenta))))
   `(term-color-red ((,class (:background ,bg-red :foreground ,fg-red))))
   `(term-color-white ((,class (:background ,bg-bright :foreground ,fg-default))))
   `(term-color-yellow ((,class (:background ,bg-yellow :foreground ,fg-yellow))))

   ;; Which function
   `(which-func ((,class (:foreground ,fg-blue))))

   ;; Whitespace mode
   `(whitespace-empty ((,class (:background ,bg-cyan :foreground ,fg-default))))
   `(whitespace-hspace ((,class (:background ,bg-bright :foreground ,fg-bright))))
   `(whitespace-indentation ((,class (:background ,bg-yellow :foreground unspecified))))
   `(whitespace-newline ((,class (:foreground ,fg-pink))))
   `(whitespace-space ((,class (:background unspecified :foreground ,fg-pink))))
   `(whitespace-space-after-tab ((,class (:background ,bg-orange :foreground unspecified))))
   `(whitespace-space-before-tab ((,class (:background ,bg-orange :foreground unspecified))))
   `(whitespace-tab ((,class (:background unspecified :underline ,bg-bright))))
   `(whitespace-trailing ((,class (:background ,bg-orange :foreground unspecified))))
   `(whitespace-big-indent ((,class (:background ,bg-red :foreground ,fg-gray))))
   `(whitespace-line ((,class (:background ,fg-red :foreground ,fg-bright))))

   ;; yas
   ;; yas--field-debug-face
   `(yas-field-highlight-face ((,class (:background ,bg-black :foreground ,fg-blue))))

   ;; Widget
   `(widget-button ((,class (:inherit button))))
   `(widget-button-pressed ((,class (:inherit widget-button :weight bold))))
   `(widget-documentation ((,class (:inherit font-lock-doc-face))))
   `(widget-field ((,class (:background ,bg-blue :box (:color ,bg-blue :line-width 2)))))

   ;; Rpm-spec-mode
   `(rpm-spec-tag-face ((,class (:foreground ,fg-blue))))
   `(rpm-spec-obsolete-tag-face ((,class (:background ,bg-red))))
   `(rpm-spec-macro-face ((,class (:foreground ,fg-yellow))))
   `(rpm-spec-var-face ((,class (:foreground ,fg-cyan))))
   `(rpm-spec-doc-face ((,class (:foreground ,fg-magenta))))
   `(rpm-spec-dir-face ((,class (:foreground ,fg-turquoise))))
   `(rpm-spec-package-face ((,class (:foreground ,fg-red))))
   `(rpm-spec-ghost-face ((,class (:foreground ,fg-red))))
   `(rpm-spec-section-face ((,class (:foreground ,fg-yellow :underline t))))

   ;; Window dividers
   `(window-divider ((,class (:foreground ,bg-dim))))
   `(window-divider-first-pixel ((,class (:inherit window-divider))))
   `(window-divider-last-pixel ((,class (:inherit window-divider))))

   ;; Vertical-border
   `(vertical-border ((,class nil)))))

;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'moebius)
;;; moebius-theme.el ends here
