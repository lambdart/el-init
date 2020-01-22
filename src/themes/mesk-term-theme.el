;;; mesk-theme.el --- Mesk official theme
;;
;; Copyright (C) 2019 k-bps
;;
;; Author: k-bps <k-bps@tutanota.com>
;; Keywords: faces
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>;;
;;; Commentary:
;; Emacs theme compatible with terminal emulators (urxvt and st).
;;
;;; Code:
;;
(require 'color nil t)

;; declare theme (mesk theme) to be a custom theme
(deftheme mesk-term
  "Mesk theme (dark)")

;; your colors (bt=bright)
(let
  (
    (black    "#000000")
    (red      "#af5f87")
    (green    "#65a854")
    (yellow   "#8d995c")
    (blue     "#5476a8")
    (magenta  "#9754a8")
    (cyan     "#54a8a8")
    (white    "#bfbfbf")

    (bgdim       "#222222") (fgdim       "#969696")
    (bgorange    "#3f321f") (fgorange    "#a88654")
    (bgdefault   "#000000") (fgdefault   "#ffffff")
    (bgbright    "#3d3d3d") (fgbright    "#ededed")
    (bgred       "#3f1a1a") (fgred       "#a85454")
    (bgyellow    "#343922") (fgyellow    "#8d995c")
    (bggreen     "#263f1f") (fggreen     "#65a854")
    (bgturquoise "#1f3f2c") (fgturquoise "#54a875")
    (bgcyan      "#1f3f3f") (fgcyan      "#54a8a8")
    (bgblue      "#005f87") (fgblue      "#5476a8")
    (bgpurple    "#2f2a3f") (fgpurple    "#7d71a8")
    (bgmagenta   "#381f3f") (fgmagenta   "#9754a8")
    (bgpink      "#3f1f32") (fgpink      "#a85487")
    (bggray      "#101010") (fggray      "#1d1d1d"))

  (custom-theme-set-faces
    'mesk-term
    ;; custom-set-faces was added by Custom.
    ;; If you edit it by hand, you could mess it up, so be careful.
    ;; Your init file should contain only one such instance.
    ;; If there is more than one, they won't work right.

    ;; Default
    `(default ((t (:background ,black :foreground ,white))))
    `(c-nonbreakable-space-face ((t (:background "#8787af" :foreground ,white :weight normal))) t)
    `(cursor ((t (:background ,fgdim))))
    `(error ((t (:foreground "#a85454"))))
    `(font-lock-builtin-face ((t (:foreground ,fgblue))))
    `(font-lock-comment-delimiter-face ((t (:foreground ,fgdim :inherit unspecified))))
    `(font-lock-comment-face ((t (:foreground ,fgdim))))
    `(font-lock-constant-face ((t (:foreground ,red))))
    `(font-lock-constant-face ((t (:foreground ,fgred))))
    `(font-lock-doc-face ((t (:foreground ,green :inherit unspecified))))
    `(font-lock-function-name-face ((t (:foreground ,fgblue))))
    `(font-lock-keyword-face ((t (:foreground ,fgorange))))
    `(font-lock-negation-char-face ((t (:foreground ,fgred))))
    `(font-lock-regexp-grouping-backslash ((t (:foreground ,fgred))))
    `(font-lock-regexp-grouping-construct ((t (:foreground ,fgpink))))
    `(font-lock-string-face ((t (:foreground ,red))))
    `(font-lock-type-face ((t (:foreground ,fgpurple))))
    `(font-lock-variable-name-face ((t (:foreground ,fgyellow))))
    `(font-lock-warning-face ((t (:foreground ,fgorange :inherit unspecified :weight bold))))
    `(fringe ((t (:background unspecified :inherit default))))
    `(italic ((t (:slant italic))))
    `(match ((t (:background ,yellow :foreground "#262626"))))
    `(nobreak-hyphen ((t (:foreground ,red))))
    `(minibuffer-prompt ((t (:foreground ,fgblue))))
    `(warning ((t (:foreground ,fgorange :weight unspecified))))
    `(region ((t (:background ,blue :foreground "#eeeeee"))))
    `(shadow ((t (:foreground ,fgdim))))
    `(success ((t (:foreground ,fggreen :weight bold))))
    `(trailing-whitespace ((t (:background ,fgred))))
    `(info-node ((t (:foreground "#878700" :slant italic :weight bold))))
    `(isearch ((t (:background "#262626" :foreground "#5f87af"))))
    `(isearch-fail ((t (:background "#af5f5f" :foreground "#262626"))))
    `(lazy-highlight ((t (:background "cyan" :foreground "#262626"))))
    `(line-number ((t (:inherit (shadow black)))))
    `(line-number-current-line ((t (:inherit line-number))))
    `(link-visited ((t (:foreground ,magenta :underline t))))

    ;;; Mode Line
    `(mode-line ((t (:background "#222222" :foreground ,white :box (:color "#222222" :line-width 2 :style nil)))))
    `(mode-line-buffer-id ((t (:foreground ,blue :weight semi-bold))))
    `(mode-line-inactive ((t (:weight normal :background ,bggray :foreground ,white))))

    ;;; Highlight numbers
    `(highlight ((t (:background "#1c1c1c"))))
    `(highlight-numbers-number ((t (:foreground ,fgcyan :inherit unspecified))))

    ;;; Highlight 80+
    `(highlight-80+ ((t (:underline (:color ,fgred :style wave) :background unspecified))))

    ;;; Highlight indent
    `(hl-indent-block-face-1 ((t (:background ,bgred))))
    `(hl-indent-block-face-2 ((t (:background ,bgpink))))
    `(hl-indent-block-face-3 ((t (:background ,bgorange))))
    `(hl-indent-block-face-4 ((t (:background ,bgyellow))))
    `(hl-indent-block-face-5 ((t (:background ,bggreen))))
    `(hl-indent-block-face-6 ((t (:background ,bgturquoise))))
    `(hl-indent-face ((t (:inherit unspecified :background ,bgdim))))

    `(hl-line ((t (:background "#262626"))))
    `(holiday ((t (:background "#af5f5f"))))
    `(homoglyph ((t (:foreground ,red))))
    `(header-line ((t (:background "#222222" :foreground ,white))))

    `(package-status-avail-obso ((t (:foreground ,green))))
    `(popup-isearch-match ((t (:inherit black :background ,blue))))
    `(pulse-highlight-face ((t (:background "#FFFFAA" :foreground "#444444"))))
    `(pulse-highlight-start-face ((t (:background "#FFFFAA" :foreground "#444444"))))
    `(secondary-selection ((t (:background ,yellow :foreground ,black))))
    `(smerge-lower ((t (:background "#ddffdd" :foreground "#444444"))))
    `(epa-string ((t (:foreground ,blue))))
    `(escape-glyph ((t (:foreground ,red))))
    `(eww-valid-certificate ((t (:foreground ,green :weight bold))))

    ;; Links
    `(link ((t (:foreground ,fgorange :underline t))))
    `(link-visited ((t (:foreground ,fgmagenta :underline t))))

    ;; Company
    `(company-preview ((t (:background unspecified :foreground ,fgdim))))
    `(company-preview-common ((t (:inherit nil :background "#262626" :foreground ,blue :weight normal))))
    `(company-echo-common ((t (:background ,red :foreground "#ffd700"))))
    `(company-scrollbar-bg ((nil (:background ,bggray))))
    `(company-scrollbar-fg ((nil (:background ,bgbright))))
    `(company-tooltip ((t (:foreground ,fgdefault :background ,bggray))))
    `(company-tooltip-common ((t (:foreground ,blue))))
    `(company-tooltip-selection ((t (:background "#262626"))))
    `(company-tooltip-annotation ((t (:foreground ,fgblue))))
    `(company-tooltip-search ((t (:background ,bgyellow :inherit unspecified))))
    `(company-tooltip-search-selection ((t (:background ,bgyellow :inherit unspecified))))
    `(company-template-field ((t (:background "#262626" :foreground  ,yellow))))

    ;; Compilation
    `(compilation-mode-line-exit ((t (:inherit compilation-info :foreground ,green :weight bold))))

    ;; CSS
    `(css-property ((t (:foreground ,fgorange))))
    `(css-proprietary-property ((t (:foreground ,fgred :inherit unspecified :slant italic))))
    `(css-selector ((t (:foreground ,fgblue))))

    ;; Custom
    `(custom-button-pressed-unraised ((t (:inherit custom-button-unraised :foreground ,red))))
    `(custom-changed ((t (:background ,blue :foreground ,white))))
    `(custom-comment-tag ((t (:foreground ,blue))))
    `(custom-group-tag ((t (:inherit variable-pitch :foreground ,blue :weight bold :height 1.2))))
    `(custom-invalid ((t (:background "#ffd7d7" :foreground ,black))))
    `(custom-modified ((t (:background ,blue :foreground ,white))))
    `(custom-set ((t (:background ,white :foreground ,blue))))
    `(custom-themed ((t (:background ,blue :foreground ,white))))
    `(custom-variable-tag ((t (:foreground ,blue :weight bold))))

    ;; Dashboard
    `(dashboard-footer ((t (:foreground "#af5f5f"))))
    `(dashboard-heading ((t (:foreground "#5f87af"))))
    `(dashboard-text-banner ((t (:foreground "#8a8a8a"))))

    ;; Diff
    `(diff-added ((t (:background ,green :foreground "#ffffff"))))
    `(diff-refine-changed ((t (:background ,yellow :foreground ,black))))
    `(diff-removed ((t (:background "#af5f5f" :foreground "#ffffff"))))
    `(diff-changed ((t (:background ,bgorange))))
    `(diff-file-header ((t (:foreground "#eeeeee" :background unspecified))))
    `(diff-function ((t (:inherit unspecified :foreground ,fgorange))))
    `(diff-header ((t (:background ,bgbright))))
    `(diff-hl-change ((t (:foreground ,bgyellow :background unspecified :inherit diff-changed))))
    `(diff-hl-delete ((t (:foreground ,bgred :inherit diff-removed))))
    `(diff-hl-insert ((t (:foreground ,bggreen :inherit diff-added))))
    `(diff-hunk-header ((t (:inherit unspecified :weight bold :foreground ,fgyellow :underline t))))
    `(diff-indicator-added ((t (:foreground ,fggreen :weight bold :inherit unspecified))))
    `(diff-indicator-changed ((t (:foreground ,fgyellow :weight bold :inherit unspecified ))))
    `(diff-indicator-removed ((t (:foreground ,fgred :weight bold :inherit unspecified))))
    `(diff-refine-added ((t (:foreground ,fggreen :background unspecified :inherit unspecified))))
    `(diff-refine-removed ((t (:foreground ,fgred :background unspecified :inherit unspecified))))

    ;; Dired
    `(dired-flagged ((t (:inherit region))))
    `(dired-header ((t (:foreground ,fgturquoise :weight bold :inherit unspecified))))
    `(dired-mark ((t (:foreground ,fgpink :inherit unspecified))))
    `(dired-marked ((t (:inherit nil :background ,magenta))))

    ;; Ediff
    `(ediff-current-diff-A ((t (:inherit diff-removed))))
    `(ediff-current-diff-B ((t (:inherit diff-added))))
    `(ediff-current-diff-C ((t (:inherit diff-changed))))
    `(ediff-fine-diff-A ((t (:inherit diff-refine-removed))))
    `(ediff-fine-diff-B ((t (:inherit diff-refine-added))))
    `(ediff-fine-diff-C ((t (:inherit diff-refine-change))))

    ;; ERC
    `(erc-button ((t (:weight unspecified :inherit button))))
    `(erc-current-nick-face ((t (:foreground ,fgred :weight bold))))
    `(erc-input-face ((t (:inherit shadow))))
    `(erc-my-nick-face ((t (:inherit erc-current-nick-face))))
    `(erc-notice-face ((t (:foreground ,fgblue :weight normal))))
    `(erc-prompt-face ((t (:foreground ,fgbright :weight bold))))
    `(erc-timestamp-face ((t (:foreground ,fgdim :weight normal))))

    ;; ERT
    `(ert-test-result-expected ((t (:background unspecified :foreground ,fggreen))))
    `(ert-test-result-unexpected ((t (:background unspecified :foreground ,fgred))))

    ;; Eshell
    `(eshell-fringe-status-failure ((t (:foreground ,fgred))))
    `(eshell-fringe-status-success ((t (:foreground ,fggreen))))
    `(eshell-ls-archive ((t (:foreground ,fgpink :weight unspecified))))
    `(eshell-ls-backup ((t (:foreground ,fgorange))))
    `(eshell-ls-clutter ((t (:foreground ,fgdim :weight unspecified))))
    `(eshell-ls-directory ((t (:foreground ,fgblue :weight unspecified))))
    `(eshell-ls-executable ((t (:foreground ,red :weight unspecified))))
    `(eshell-ls-missing ((t (:foreground ,fgred :weight bold))))
    `(eshell-ls-product ((t (:foreground ,fgpurple))))
    `(eshell-ls-readonly ((t (:foreground ,fgmagenta))))
    `(eshell-ls-special ((t (:foreground ,fgturquoise))))
    `(eshell-ls-symlink ((t (:foreground ,fgcyan :weight unspecified))))
    `(eshell-ls-unreadable ((t (:foreground ,fgred))))
    `(eshell-prompt ((t (:foreground ,fgbright :weight unspecified))))

    ;; Flymake
    `(flymake-errline ((t (:background unspecified :underline (:color ,fgred :style wave)))))
    `(flymake-infoline ((t (:background unspecified :underline (:color ,fgblue :style wave)))))
    `(flymake-warnline ((t (:background unspecified :underline (:color ,fgorange :style wave)))))

    ;; Flyspell
    `(flyspell-duplicate ((t (:inherit unspecified :underline (:color ,fgorange :style wave)))))
    `(flyspell-incorrect ((t (:inherit unspecified :underline (:color ,fgred :style wave)))))

    ;; GNUS
    `(gnus-button ((t (:weight bold))))
    `(gnus-cite-1 ((t (:foreground ,fgred))))
    `(gnus-cite-10 ((t (:foreground ,fgpink))))
    `(gnus-cite-11 ((t (:foreground ,fgbright))))
    `(gnus-cite-2 ((t (:foreground ,fgorange))))
    `(gnus-cite-3 ((t (:foreground ,fgyellow))))
    `(gnus-cite-4 ((t (:foreground ,fggreen))))
    `(gnus-cite-5 ((t (:foreground ,fgturquoise))))
    `(gnus-cite-6 ((t (:foreground ,fgcyan))))
    `(gnus-cite-7 ((t (:foreground ,fgblue))))
    `(gnus-cite-8 ((t (:foreground ,fgpurple))))
    `(gnus-cite-9 ((t (:foreground ,fgmagenta))))
    `(gnus-group-mail-3 ((t (:foreground ,fgcyan :weight bold))))
    `(gnus-group-mail-3-empty ((t (:foreground ,fgcyan))))
    `(gnus-group-news-3 ((t (:foreground ,fgred :weight bold))))
    `(gnus-group-news-3-empty ((t (:foreground ,fgred))))
    `(gnus-header-content ((t (:foreground ,fgdim :slant italic))))
    `(gnus-header-from ((t (:weight bold))))
    `(gnus-header-name ((t (:foreground ,fgblue :weight bold))))
    `(gnus-header-newsgroups ((t (:foreground ,fgbright :weight bold))))
    `(gnus-header-subject ((t (:foreground ,fgyellow))))
    `(gnus-signature ((t (:foreground ,fgdim :slant italic))))
    `(gnus-splash ((t (:foreground ,fgdefault))))
    `(gnus-summary-cancelled ((t (:foreground ,fgdim :background unspecified :strike-through t))))
    `(gnus-summary-high-ancient ((t (:inherit gnus-summary-normal-ancient :weight bold))))
    `(gnus-summary-high-read ((t (:inherit gnus-summary-normal-read :weight bold))))
    `(gnus-summary-high-ticked ((t (:inherit gnus-summary-normal-ticked :weight bold))))
    `(gnus-summary-high-unread ((t (:foreground ,fgpink))))
    `(gnus-summary-low-ancient ((t (:inherit gnus-summary-normal-ancient :slant italic))))
    `(gnus-summary-low-read ((t (:inherit gnus-summary-normal-read :slant italic))))
    `(gnus-summary-low-ticked ((t (:inherit gnus-summary-normal-ticked :slant italic))))
    `(gnus-summary-low-unread ((t (:inherit gnus-summary-normal-unread :slant italic))))
    `(gnus-summary-normal-ancient ((t (:foreground ,fgcyan))))
    `(gnus-summary-normal-read ((t (:foreground ,fgdim))))
    `(gnus-summary-normal-ticked ((t (:foreground ,fgturquoise))))
    `(gnus-summary-normal-unread ((t (:foreground ,fgdefault))))
    `(gnus-group-mail-1 ((t (:foreground "brightred" :weight bold))))
    `(gnus-group-mail-1-empty ((t (:foreground " #af87ff"))))
    `(gnus-group-mail-2 ((t (:foreground ,magenta :weight bold))))
    `(gnus-group-mail-2-empty ((t (:foreground ,magenta))))
    `(gnus-group-mail-low ((t (:foreground "#af5faf" :weight bold))))
    `(gnus-group-mail-low-empty ((t (:foreground "#af5faf"))))
    `(gnus-summary-selected ((t (:background ,blue :foreground ,white))))

    ;; Go
    `(go-coverage-10 ((t (:foreground ,green))))
    `(go-coverage-7 ((t (:foreground ,green))))
    `(go-coverage-8 ((t (:foreground ,green))))
    `(go-coverage-9 ((t (:foreground ,green))))
    `(go-coverage-covered ((t (:foreground ,green))))

    ;;; Hydra
    `(hydra-face-amaranth ((t (:foreground ,fgorange :weight bold))))
    `(hydra-face-blue ((t (:foreground ,fgblue :weight bold))))
    `(hydra-face-pink ((t (:foreground ,fgpink :weight bold))))
    `(hydra-face-red ((t (:foreground ,fgred :weight bold))))
    `(hydra-face-teal ((t (:foreground ,fgcyan :weight bold))))

    ;; Helm
    `(helm-M-x-key ((t (:foreground "#af5f5f" :underline nil :weight semi-bold))))
    `(helm-action ((t nil)))
    `(helm-visible-mark ((t (:background "#d1f5ea" :foreground ,black))))
    `(helm-selection ((t (:distant-foreground unspecified :background ,bgdim))))
    `(helm-source-header ((t (:foreground "#af5f5f" :weight semi-bold :height 1.0))))
    `(helm-candidate-number ((t (:background ,yellow :foreground ,black))))
    `(helm-match ((t (:foreground ,blue :weight semi-bold))))
    `(helm-moccur-buffer ((t (:foreground "DarkTurquoise" :underline nil))))
    `(helm-history-remote ((t (:foreground "#af8700"))))
    `(helm-bookmark-addressbook ((t (:foreground ,red))))
    `(helm-bookmark-file ((t (:foreground "#005f87"))))
    `(helm-bookmark-info ((t (:foreground ,green))))
    `(helm-buffer-archive ((t (:foreground  ,yellow))))
    `(helm-buffer-process ((t (:foreground ,green))))
    `(helm-buffer-directory ((t (:background unspecified :foreground ,fgblue))))
    `(helm-buffer-file ((t (:inherit default))))
    `(helm-buffer-not-saved ((t (:foreground ,fgcyan))))
    `(helm-buffer-process ((t (:foreground ,fggreen))))
    `(helm-buffer-size ((t (:foreground ,fgorange))))
    `(helm-ff-dotted-directory ((t (:foreground unspecified :background unspecified :inherit helm-ff-directory))))
    `(helm-ff-directory ((t (:foreground ,fgblue :background unspecified))))
    `(helm-ff-dotted-symlink-directory ((t (:background "#1c1c1c"))))
    `(helm-ff-socket ((t (:foreground "#af5f87"))))
    `(helm-ff-suid ((t (:background ,red :foreground "#ffffaf"))))
    `(helm-ff-executable ((t (:foreground ,red))))
    `(helm-ff-file ((t (:inherit default))))
    `(helm-ff-invalid-symlink ((t (:foreground ,fgred :background unspecified))))
    `(helm-ff-symlink ((t (:foreground ,cyan))))
    `(helm-grep-file ((t (:foreground ,magenta :underline t))))
    `(helm-grep-lineno ((t (:foreground  ,yellow))))
    `(helm-grep-match ((t (:foreground ,red))))
    `(helm-lisp-show-completion ((t (:background "#262626" :foreground "#5fafff"))))
    `(helm-rtags-token-face ((t (:inherit font-lock-warning-face :background "#1c1c1c"))))
    `(helm-visible-mark ((t (:background "#d1f5ea" :foreground ,black))))
    `(helm-selection ((t (:distant-foreground unspecified :background ,bgdim))))
    `(helm-source-header ((t (:foreground "#af5f5f" :weight semi-bold :height 1.0))))
    `(helm-swoop-target-line-face ((t nil)))
    `(helm-swoop-target-word-face ((t (:background "#121212" :foreground ,blue))))

    ;;; Hydra
    `(hydra-face-amaranth ((t (:foreground ,fgorange :weight bold))))
    `(hydra-face-blue ((t (:foreground ,fgblue :weight bold))))
    `(hydra-face-pink ((t (:foreground ,fgpink :weight bold))))
    `(hydra-face-red ((t (:foreground ,fgred :weight bold))))
    `(hydra-face-teal ((t (:foreground ,fgcyan :weight bold))))

    ;;; Identica
    `(identica-stripe-face ((t (:background ,bgbright))))
    `(identica-uri-face ((t (:foreground ,fgorange :underline t))))
    `(identica-username-face ((t (:foreground ,fgblue :weight bold :underline unspecified))))

    ;;; Ibuffer
    `(ibuffer-locked-buffer ((t (:foreground "#875f00"))))

    ;;; Ido
    `(ido-only-match ((t (:foreground ,green))))
    `(ido-indicator ((t (:background "#5f5f5f" :foreground ,white :width condensed))))

    ;;; Iedit
    `(iedit-occurrence ((t (:background ,yellow :foreground ,black))))

    ;;; Man
    `(Man-overstrike ((t (:foreground ,blue :weight bold))))
    `(Man-underline ((t (:foreground ,red))))

    ;;; Message
    `(message-header-cc ((t (:foreground ,fgdim))))
    `(message-header-name ((t (:inherit gnus-header-name))))
    `(message-header-newsgroups ((t (:foreground ,fgdim :weight bold))))
    `(message-header-other ((t (:inherit gnus-header-content))))
    `(message-header-subject ((t (:inherit gnus-header-subject))))
    `(message-header-to ((t (:foreground ,fgdim :weight bold))))
    `(message-header-xheader ((t (:foreground ,fgdim :slant italic))))
    `(message-mml ((t (:foreground ,fggreen))))
    `(message-separator ((t (:foreground ,fgblue))))

    ;; Magit
    `(magit-branch-current ((t (:inherit nil :background ,blue :foreground "#eeeeee"))))
    `(magit-branch-local ((t (:background ,magenta :foreground "#eeeeee"))))
    `(magit-branch-remote ((t (:background ,green :foreground "#ffffff"))))
    `(magit-log-author ((t (:foreground ,red :slant normal :weight normal))))
    `(magit-signature-expired ((t (:foreground ,yellow))))
    `(magit-signature-revoked ((t (:foreground ,magenta))))
    `(magit-bisect-bad ((t (:foreground ,fgred))))
    `(magit-bisect-good ((t (:foreground ,fggreen))))
    `(magit-bisect-skip ((t (:foreground ,fgdim))))
    `(magit-blame-date ((t (:foreground ,fgpink :inherit magit-blame-heading))))
    `(magit-blame-hash ((t (:foreground ,fgmagenta :inherit magit-blame-heading))))
    `(magit-blame-header ((t (:foreground ,fggreen :background ,bgdim :weight bold :inherit unspecified))))
    `(magit-blame-heading ((t (:foreground ,fgdefault :background ,bgbright))))
    `(magit-blame-name ((t (:foreground ,fgturquoise :inherit magit-blame-heading))))
    `(magit-blame-summary ((t (:foreground ,fggreen :inherit magit-blame-heading))))
    `(magit-branch ((t (:foreground ,fgpink :weight bold :inherit unspecified))))
    `(magit-diff-added ((t (:foreground unspecified :background unspecified :inherit diff-added))))
    `(magit-diff-added-highlight ((t (:foreground unspecified :background unspecified :inherit magit-diff-added))))
    `(magit-diff-context ((t (:foreground unspecified :inherit shadow))))
    `(magit-diff-context-highlight ((t (:foreground unspecified :background ,bgdim :inherit magit-diff-context))))
    `(magit-diff-file-heading ((t (:foreground unspecified :underline unspecified :inherit diff-file-header))))
    `(magit-diff-removed ((t (:foreground unspecified :background unspecified :inherit diff-removed))))
    `(magit-diff-removed-highlight ((t (:foreground unspecified :background unspecified :inherit magit-diff-removed))))
    `(magit-diffstat-removed ((t (:foreground ,red))))
    `(magit-item-highlight ((t (:inherit unspecified))))
    `(magit-log-head-label-default ((t (:foreground ,fgdefault :background ,bgcyan :box (:color ,bgcyan :line-width 2 :style nil)))))
    `(magit-log-head-label-head ((t (:foreground ,fgdefault :background ,bgblue :box (:color ,bgblue :line-width 2 :style nil)))))
    `(magit-log-head-label-local ((t (:foreground ,fgdefault :background ,bgmagenta :box (:color ,bgmagenta :line-width 2 :style nil)))))
    `(magit-log-head-label-remote ((t (:foreground ,fgdefault :background ,bggreen :box (:color ,bggreen :line-width 2 :style nil)))))
    `(magit-log-head-label-tags ((t (:foreground ,fgdefault :background ,bgorange :box (:color ,bgorange :line-width 2 :style nil)))))
    `(magit-log-sha1 ((t (:foreground ,fgdefault :background ,bgblue :box (:color ,bgblue :line-width 2 :style nil)))))
    `(magit-process-ng ((t (:foreground ,fgred :inherit unspecified))))
    `(magit-process-ok ((t (:foreground ,fggreen :inherit unspecified))))
    `(magit-section-heading ((t (:foreground ,fgturquoise :weight unspecified))))
    `(magit-section-highlight ((t (:background unspecified))))
    `(magit-section-title ((t (:foreground ,white :inherit unspecified))))

    ;; Transient
    `(transient-disabled-suffix ((t (:background ,red :foreground "#1c1c1c" :weight bold))))
    `(transient-enabled-suffix ((t (:background ,green :foreground "#444444" :weight bold))))
    `(transient-separator ((t (:background "#262626" :foreground ,white))))

    ;; Org
    `(org-agenda-restriction-lock ((t (:background "#eeeeee" :foreground "#1c1c1c"))))
    `(org-agenda-calendar-sexp ((t (:foreground ,fgyellow))))
    `(org-agenda-current-time ((t (:foreground ,fgorange :weight bold))))
    `(org-agenda-date ((t (:foreground ,bgcyan))))
    `(org-agenda-date-today ((t (:foreground ,fgcyan :slant italic))))
    `(org-agenda-date-weekend ((t (:foreground ,fgcyan))))
    `(org-agenda-done ((t (:foreground ,fgorange))))
    `(org-agenda-structure ((t (:foreground ,fgblue))))

    `(org-block ((t (:inherit shadow :background "#121212"))))
    `(org-block-begin-line ((t (:background "#444444" :foreground "#bfbfbf" :underline nil))))
    `(org-block-end-line ((t (:background "#444444" :foreground ,white))))
    `(org-column ((t (:background "#1c1c1c" :strike-through nil :underline nil :slant normal :weight normal))))
    `(org-column-title ((t (:background "#1c1c1c" :underline t :weight bold))))
    `(org-document-info ((t (:foreground ,blue))))
    `(org-document-title ((t (:foreground ,green))))
    `(org-done ((t (:foreground ,green :weight bold))))
    `(org-drawer ((t (:foreground ,blue))))
    `(org-footnote ((t (:foreground ,magenta :underline t))))
    `(org-formula ((t (:foreground ,red))))
    `(org-level-1 ((t (:foreground ,green))))
    `(org-sexp-date ((t (:foreground ,magenta))))
    `(org-table ((t (:foreground ,blue))))
    `(org-upcoming-deadline ((t (:foreground ,red))))
    `(org-checkbox-statistics-done ((t (:foreground ,bgcyan))))
    `(org-checkbox-statistics-todo ((t (:foreground ,fgcyan))))
    `(org-date ((t (:foreground ,fgpink :underline unspecified))))
    `(org-headline-done ((t (:foreground ,fgdim))))
    `(org-level-1 ((t (:foreground ,green))))
    `(org-level-2 ((t (:foreground ,cyan))))
    `(org-level-3 ((t (:foreground ,red))))
    `(org-level-4 ((t (:foreground ,blue))))
    `(org-level-5 ((t (:foreground ,yellow))))
    `(org-level-6 ((t (:foreground ,fgpurple))))
    `(org-level-7 ((t (:foreground ,fgturquoise))))
    `(org-level-8 ((t (:foreground ,fgorange))))
    `(org-scheduled ((t (:foreground ,fgdim))))
    `(org-scheduled-previously ((t (:weight bold))))
    `(org-scheduled-today ((t (:foreground ,fgdefault))))
    `(org-time-grid ((t (:foreground ,fgorange))))

    ;;; Outline
    `(outline-1 ((t (:inherit org-level-1))))
    `(outline-2 ((t (:inherit org-level-2))))
    `(outline-3 ((t (:inherit org-level-3))))
    `(outline-4 ((t (:inherit org-level-4))))
    `(outline-5 ((t (:inherit org-level-5))))
    `(outline-6 ((t (:inherit org-level-6))))
    `(outline-7 ((t (:inherit org-level-7))))
    `(outline-8 ((t (:inherit org-level-8))))

    ;;; Package
    `(package-status-avail-obso ((t (:foreground ,bggreen))))
    `(package-status-available ((t (:foreground ,fggreen))))
    `(package-status-dependency ((t (:foreground ,fgdim))))
    `(package-status-installed ((t (:foreground ,fgdefault))))

    ;;; Pretty-print ^L highlight
    `(pp^L-highlight ((t (:box unspecified :foreground ,fgbright))))

    ;;; Rainbow delimiters
    `(rainbow-delimiters-depth-1-face ((t (:foreground ,fgred))))
    `(rainbow-delimiters-depth-2-face ((t (:foreground ,fgorange))))
    `(rainbow-delimiters-depth-3-face ((t (:foreground ,fgyellow))))
    `(rainbow-delimiters-depth-4-face ((t (:foreground ,fggreen))))
    `(rainbow-delimiters-depth-4-face ((t (:foreground ,fgturquoise))))
    `(rainbow-delimiters-depth-6-face ((t (:foreground ,fgcyan))))
    `(rainbow-delimiters-depth-7-face ((t (:foreground ,fgblue))))
    `(rainbow-delimiters-depth-8-face ((t (:foreground ,fgpurple))))
    `(rainbow-delimiters-depth-9-face ((t (:foreground ,fgmagenta))))
    `(rainbow-delimiters-unmatched-face ((t (:background ,bgred :foreground unspecified))))

    ;;; Rebase mode
    `(rebase-mode-description-face ((t (:foreground ,fgbright))))

    ;;; reStructuredText
    `(rst-level-1 ((t (:background unspecified))))
    `(rst-level-2 ((t (:background unspecified))))
    `(rst-level-3 ((t (:background unspecified))))
    `(rst-level-4 ((t (:background unspecified))))
    `(rst-level-5 ((t (:background unspecified))))
    `(rst-level-6 ((t (:background unspecified))))

    ;;; Sh
    `(sh-escaped-newline ((t (:foreground ,fgpurple :inherit unspecified :weight bold))))
    `(sh-heredoc ((t (:foreground ,fggreen))))
    `(sh-quoted-exec ((t (:foreground ,fgpink))))

    ;;; Show paren
    `(show-paren-match ((t (:inverse-video t))))
    `(show-paren-mismatch ((t (:background unspecified :foreground ,fgred))))

    ;;; Slime
    `(slime-repl-input-face ((t (:foreground ,fgbright))))
    `(slime-repl-inputed-output-face ((t (:foreground ,fgbright))))
    `(slime-repl-output-face ((t (:foreground ,fgdefault))))
    `(slime-repl-prompt-face ((t (:foreground ,fgblue))))

    ;;; Smerge
    `(smerge-base ((t (:background ,bggreen))))
    `(smerge-markers ((t (:background ,bgbright))))
    `(smerge-mine ((t (:background ,bgblue))))
    `(smerge-other ((t (:background ,bgred))))
    `(smerge-refined-added ((t (:inherit diff-refine-added))))
    `(smerge-refined-change ((t (:inherit diff-refine-change))))
    `(smerge-refined-removed ((t (:inherit diff-refine-removed))))

    ;;; Term
    `(term-color-black ((t (:background ,bgdefault :foreground ,fgbright))))
    `(term-color-blue ((t (:background ,bgblue :foreground ,fgblue))))
    `(term-color-cyan ((t (:background ,bgcyan :foreground ,fgcyan))))
    `(term-color-green ((t (:background ,bggreen :foreground ,fggreen))))
    `(term-color-magenta ((t (:background ,bgmagenta :foreground ,fgmagenta))))
    `(term-color-red ((t (:background ,bgred :foreground ,fgred))))
    `(term-color-white ((t (:background ,bgbright :foreground ,fgdefault))))
    `(term-color-yellow ((t (:background ,bgyellow :foreground ,fgyellow))))

    ;;; Texinfo
    `(texinfo-heading ((t (:foreground ,fgpink :inherit unspecified :height 1.3))))

    ;;; Tty
    `(tty-menu-selected-face ((t (:background ,red :foreground "#eeeeee"))))

    ;;; Which-key
    `(which-key-key-face ((t (:foreground "#af5f5f"))))
    `(which-key-special-key-face ((t (:inherit which-key-key-face :inverse-video t :weight normal))))

    ;; Term
    `(term-color-black ((t (:background ,bgdefault :foreground ,fgbright))))
    `(term-color-blue ((t (:background ,bgblue :foreground ,fgblue))))
    `(term-color-cyan ((t (:background ,bgcyan :foreground ,fgcyan))))
    `(term-color-green ((t (:background ,bggreen :foreground ,fggreen))))
    `(term-color-magenta ((t (:background ,bgmagenta :foreground ,fgmagenta))))
    `(term-color-red ((t (:background ,bgred :foreground ,fgred))))
    `(term-color-white ((t (:background ,bgbright :foreground ,fgdefault))))
    `(term-color-yellow ((t (:background ,bgyellow :foreground ,fgyellow))))

    ;; Texinfo
    `(texinfo-heading ((t (:foreground ,fgpink :inherit unspecified :height 1.3))))

    ;; Which function
    `(which-func ((t (:foreground ,fgblue))))

    ;; Whitespace mode
    `(whitespace-empty ((t (:background ,bgcyan :foreground ,fgdefault))))
    `(whitespace-hspace ((t (:background ,bgbright :foreground ,fgbright))))
    `(whitespace-indentation ((t (:background ,bgyellow :foreground unspecified))))
    `(whitespace-newline ((t (:foreground ,fgpink))))
    `(whitespace-space ((t (:background unspecified :foreground ,fgpink))))
    `(whitespace-space-after-tab ((t (:background ,bgorange :foreground unspecified))))
    `(whitespace-space-before-tab ((t (:background ,bgorange :foreground unspecified))))
    `(whitespace-tab ((t (:background unspecified :underline ,bgbright))))
    `(whitespace-trailing ((t (:background ,bgorange :foreground unspecified))))
    `(whitespace-big-indent ((t (:background ,red :foreground "#1c1c1c"))))
    `(whitespace-line ((t (:background "#af5f87"))))

    ;;; Widget
    `(widget-button ((t (:inherit button))))
    `(widget-button-pressed ((t (:inherit widget-button :weight bold))))
    `(widget-documentation ((t (:inherit font-lock-doc-face))))
    `(widget-field ((t (:background ,bgblue :box (:color ,bgblue :line-width 2)))))

    ;;; rpm-spec-mode
    `(rpm-spec-tag-face ((t (:foreground ,fgblue))))
    `(rpm-spec-obsolete-tag-face ((t (:background ,bgred))))
    `(rpm-spec-macro-face ((t (:foreground ,fgyellow))))
    `(rpm-spec-var-face ((t (:foreground ,fgcyan))))
    `(rpm-spec-doc-face ((t (:foreground ,fgmagenta))))
    `(rpm-spec-dir-face ((t (:foreground ,fgturquoise))))
    `(rpm-spec-package-face ((t (:foreground ,fgred))))
    `(rpm-spec-ghost-face ((t (:foreground ,fgred))))
    `(rpm-spec-section-face ((t (:foreground ,fgyellow :underline t))))

    ;;; Window dividers
    `(window-divider ((t (:foreground "#262626"))))
    `(window-divider-first-pixel ((t (:foreground unspecified :inherit window-divider))))
    `(window-divider-last-pixel ((t (:foreground unspecified :inherit window-divider))))

    ;; vertical-border
    ;; `(vertical-border ((((type x tty)) (:foreground "#262626"))))
    `(vertical-border ((t nil)))
    ))

;;;###autoload
(and load-file-name
  (boundp 'custom-theme-load-path)
  (add-to-list 'custom-theme-load-path
    (file-name-as-directory
      (file-name-directory load-file-name))))

(provide-theme 'mesk-term)
;;; mesk-term-theme.el ends here
