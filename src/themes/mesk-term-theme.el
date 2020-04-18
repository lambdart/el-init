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

    (bg-dim       "#222222") (fg-dim       "#969696")
    (bg-orange    "#3f321f") (fg-orange    "#a88654")
    (bg-default   "#000000") (fg-default   "#bfbfbf")
    (bg-bright    "#3d3d3d") (fg-bright    "#ededed")
    (bg-red       "#3f1a1a") (fg-red       "#a85454")
    (bg-yellow    "#343922") (fg-yellow    "#8d995c")
    (bg-green     "#263f1f") (fg-green     "#65a854")
    (bg-turquoise "#1f3f2c") (fg-turquoise "#54a875")
    (bg-cyan      "#1f3f3f") (fg-cyan      "#54a8a8")
    (bg-blue      "#005f87") (fg-blue      "#5476a8")
    (bg-purple    "#2f2a3f") (fg-purple    "#7d71a8")
    (bg-magenta   "#381f3f") (fg-magenta   "#9754a8")
    (bg-pink      "#3f1f32") (fg-pink      "#a85487")
    (bg-gray      "#101010") (fg-gray      "#1d1d1d"))

  (custom-theme-set-faces
    'mesk-term
    ;; custom-set-faces was added by Custom.
    ;; If you edit it by hand, you could mess it up, so be careful.
    ;; Your init file should contain only one such instance.
    ;; If there is more than one, they won't work right.

    ;; Default
    `(default ((t (:background ,black :foreground ,white))))
    `(c-nonbreakable-space-face ((t (:background "#8787af" :foreground ,white :weight normal))) t)
    `(cursor ((t (:background ,fg-dim))))
    `(error ((t (:foreground "#a85454"))))
    `(font-lock-builtin-face ((t (:foreground ,fg-blue))))
    `(font-lock-comment-delimiter-face ((t (:foreground ,fg-dim :inherit unspecified))))
    `(font-lock-comment-face ((t (:foreground ,fg-dim))))
    `(font-lock-constant-face ((t (:foreground ,red))))
    `(font-lock-constant-face ((t (:foreground ,fg-red))))
    `(font-lock-doc-face ((t (:foreground ,green :inherit unspecified))))
    `(font-lock-function-name-face ((t (:foreground ,fg-blue))))
    `(font-lock-keyword-face ((t (:foreground ,fg-orange))))
    `(font-lock-negation-char-face ((t (:foreground ,fg-red))))
    `(font-lock-regexp-grouping-backslash ((t (:foreground ,fg-red))))
    `(font-lock-regexp-grouping-construct ((t (:foreground ,fg-pink))))
    `(font-lock-string-face ((t (:foreground ,red))))
    `(font-lock-type-face ((t (:foreground ,fg-purple))))
    `(font-lock-variable-name-face ((t (:foreground ,fg-yellow))))
    `(font-lock-warning-face ((t (:foreground ,fg-orange :inherit unspecified :weight bold))))
    ;; `(fringe ((t (:background unspecified :inherit default))))
    `(fringe ((t nil)))
    `(italic ((t (:slant italic))))
    `(match ((t (:background ,yellow :foreground "#262626"))))
    `(nobreak-hyphen ((t (:foreground ,red))))
    `(minibuffer-prompt ((t (:foreground ,fg-blue))))
    `(warning ((t (:foreground ,fg-orange :weight unspecified))))
    `(region ((t (:background ,blue :foreground "#eeeeee"))))
    `(shadow ((t (:foreground ,fg-dim))))
    `(success ((t (:foreground ,fg-green :weight bold))))
    `(trailing-whitespace ((t (:background ,fg-red))))
    `(info-node ((t (:foreground "#878700" :slant italic :weight bold))))
    `(isearch ((t (:background "#262626" :foreground "#5f87af"))))
    `(isearch-fail ((t (:background "#af5f5f" :foreground "#262626"))))
    `(lazy-highlight ((t (:background "cyan" :foreground "#262626"))))
    `(line-number ((t (:inherit (shadow black)))))
    `(line-number-current-line ((t (:inherit line-number))))
    `(link-visited ((t (:foreground ,magenta :underline t))))

    ;;; Mode Line
    ;; `(mode-line ((t nil)))))
    ;; `(mode-line-emphasis ((t nil)))
    ;; `(mode-line-inactive ((t nil)))
    `(mode-line ((t (:background "#262626" :foreground "#bfbfbf" :box (:color "#262626" :line-width 2 :style nil)))))
    `(mode-line-inactive ((t (:background "#262626" :foreground "dim gray" :box (:line-width 2 :color "gray15" :style nil)))))
    `(mode-line-buffer-id ((t (:foreground ,blue :weight normal))))

    ;; `(mode-line ((t (:background "#262626" :foreground "#bfbfbf"))))
    ;; `(mode-line-inactive ((t (:weight 1 :background ,bg-gray :foreground ,white))))
    ;; `(mode-line-inactive ((t (:weight 1 :background ,bg-gray :foreground ,white))))

    ;;; Highlight numbers
    `(highlight ((t (:background "#1c1c1c"))))
    `(highlight-numbers-number ((t (:foreground ,fg-cyan :inherit unspecified))))

    ;;; Highlight 80+
    `(highlight-80+ ((t (:underline (:color ,fg-red :style wave) :background unspecified))))

    ;;; Highlight indent
    `(hl-indent-block-face-1 ((t (:background ,bg-red))))
    `(hl-indent-block-face-2 ((t (:background ,bg-pink))))
    `(hl-indent-block-face-3 ((t (:background ,bg-orange))))
    `(hl-indent-block-face-4 ((t (:background ,bg-yellow))))
    `(hl-indent-block-face-5 ((t (:background ,bg-green))))
    `(hl-indent-block-face-6 ((t (:background ,bg-turquoise))))
    `(hl-indent-face ((t (:inherit unspecified :background ,bg-dim))))

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
    `(link ((t (:foreground ,fg-orange :underline t))))
    `(link-visited ((t (:foreground ,fg-magenta :underline t))))

    ;; Company
    `(company-preview ((t (:background unspecified :foreground ,fg-dim))))
    `(company-preview-common ((t (:inherit nil :background "#262626" :foreground ,blue :weight normal))))
    `(company-echo-common ((t (:background ,red :foreground "#ffd700"))))
    `(company-scrollbar-bg- ((nil (:background ,bg-gray))))
    `(company-scrollbar-fg- ((nil (:background ,bg-bright))))
    `(company-tooltip ((t (:foreground ,fg-default :background ,bg-gray))))
    `(company-tooltip-common ((t (:foreground ,blue))))
    `(company-tooltip-selection ((t (:background "#262626"))))
    `(company-tooltip-annotation ((t (:foreground ,fg-blue))))
    `(company-tooltip-search ((t (:background ,bg-yellow :inherit unspecified))))
    `(company-tooltip-search-selection ((t (:background ,bg-yellow :inherit unspecified))))
    `(company-template-field ((t (:background "#262626" :foreground  ,yellow))))

    ;; Compilation
    `(compilation-mode-line-exit ((t (:inherit compilation-info :foreground ,green :weight bold))))

    ;; CSS
    `(css-property ((t (:foreground ,fg-orange))))
    `(css-proprietary-property ((t (:foreground ,fg-red :inherit unspecified :slant italic))))
    `(css-selector ((t (:foreground ,fg-blue))))

    ;; Custom
    `(custom-button ((t (:background "dark gray" :foreground "gray1" :box (:line-width 1 :color "dark gray") :weight ultra-light :height 1.0 :width ultra-condensed))))
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
    `(diff-changed ((t (:background ,bg-orange))))
    `(diff-file-header ((t (:foreground "#eeeeee" :background unspecified))))
    `(diff-function ((t (:inherit unspecified :foreground ,fg-orange))))
    `(diff-header ((t (:background ,bg-bright))))
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
    `(erc-current-nick-face ((t (:foreground ,fg-red :weight bold))))
    `(erc-input-face ((t (:inherit shadow))))
    `(erc-my-nick-face ((t (:inherit erc-current-nick-face))))
    `(erc-notice-face ((t (:foreground ,fg-blue :weight normal))))
    `(erc-prompt-face ((t (:foreground ,fg-bright :weight bold))))
    `(erc-timestamp-face ((t (:foreground ,fg-dim :weight normal))))

    ;; ERT
    `(ert-test-result-expected ((t (:background unspecified :foreground ,fg-green))))
    `(ert-test-result-unexpected ((t (:background unspecified :foreground ,fg-red))))

    ;; Eshell
    `(eshell-fringe-status-failure ((t (:foreground ,fg-red))))
    `(eshell-fringe-status-success ((t (:foreground ,fg-green))))
    `(eshell-ls-archive ((t (:foreground ,fg-pink :weight unspecified))))
    `(eshell-ls-backup ((t (:foreground ,fg-orange))))
    `(eshell-ls-clutter ((t (:foreground ,fg-dim :weight unspecified))))
    `(eshell-ls-directory ((t (:foreground ,fg-blue :weight unspecified))))
    `(eshell-ls-executable ((t (:foreground ,red :weight unspecified))))
    `(eshell-ls-missing ((t (:foreground ,fg-red :weight bold))))
    `(eshell-ls-product ((t (:foreground ,fg-purple))))
    `(eshell-ls-readonly ((t (:foreground ,fg-magenta))))
    `(eshell-ls-special ((t (:foreground ,fg-turquoise))))
    `(eshell-ls-symlink ((t (:foreground ,fg-cyan :weight unspecified))))
    `(eshell-ls-unreadable ((t (:foreground ,fg-red))))
    `(eshell-prompt ((t (:foreground ,fg-bright :weight unspecified))))

    ;; Flymake
    `(flymake-errline ((t (:background unspecified :underline (:color ,fg-red :style wave)))))
    `(flymake-infoline ((t (:background unspecified :underline (:color ,fg-blue :style wave)))))
    `(flymake-warnline ((t (:background unspecified :underline (:color ,fg-orange :style wave)))))

    ;; Flyspell
    `(flyspell-duplicate ((t (:inherit unspecified :underline (:color ,fg-orange :style wave)))))
    `(flyspell-incorrect ((t (:inherit unspecified :underline (:color ,fg-red :style wave)))))

    ;; Flycheck
    `(flycheck-warning ((t (:underline (:color "IndianRed2" :style wave)))))

    ;; GNUS
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
    `(hydra-face-amaranth ((t (:foreground ,fg-orange :weight bold))))
    `(hydra-face-blue ((t (:foreground ,fg-blue :weight bold))))
    `(hydra-face-pink ((t (:foreground ,fg-pink :weight bold))))
    `(hydra-face-red ((t (:foreground ,fg-red :weight bold))))
    `(hydra-face-teal ((t (:foreground ,fg-cyan :weight bold))))

    ;; Helm
    `(helm-M-x-key ((t (:foreground "#af5f5f" :underline nil :weight semi-bold))))
    `(helm-action ((t nil)))
    `(helm-visible-mark ((t (:background "#d1f5ea" :foreground ,black))))
    `(helm-selection ((t (:distant-foreground unspecified :background ,bg-dim))))
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
    `(helm-buffer-directory ((t (:background unspecified :foreground ,fg-blue))))
    `(helm-buffer-file ((t (:inherit default))))
    `(helm-buffer-not-saved ((t (:foreground ,fg-cyan))))
    `(helm-buffer-process ((t (:foreground ,fg-green))))
    `(helm-buffer-size ((t (:foreground ,fg-orange))))
    `(helm-ff-dotted-directory ((t (:foreground unspecified :background unspecified :inherit helm-ff-directory))))
    `(helm-ff-directory ((t (:foreground ,fg-blue :background unspecified))))
    `(helm-ff-dotted-symlink-directory ((t (:background "#1c1c1c"))))
    `(helm-ff-socket ((t (:foreground "#af5f87"))))
    `(helm-ff-suid ((t (:background ,red :foreground "#ffffaf"))))
    `(helm-ff-executable ((t (:foreground ,red))))
    `(helm-ff-file ((t (:inherit default))))
    `(helm-ff-invalid-symlink ((t (:foreground ,fg-red :background unspecified))))
    `(helm-ff-symlink ((t (:foreground ,cyan))))
    `(helm-grep-file ((t (:foreground ,magenta :underline t))))
    `(helm-grep-lineno ((t (:foreground  ,yellow))))
    `(helm-grep-match ((t (:foreground ,red))))
    `(helm-lisp-show-completion ((t (:background "#262626" :foreground "#5fafff"))))
    `(helm-rtags-token-face ((t (:inherit font-lock-warning-face :background "#1c1c1c"))))
    `(helm-visible-mark ((t (:background "#d1f5ea" :foreground ,black))))
    `(helm-selection ((t (:distant-foreground unspecified :background ,bg-dim))))
    `(helm-source-header ((t (:foreground "#af5f5f" :weight semi-bold :height 1.0))))
    `(helm-swoop-target-line-face ((t nil)))
    `(helm-swoop-target-word-face ((t (:background "#121212" :foreground ,blue))))

    ;;; Hydra
    `(hydra-face-amaranth ((t (:foreground ,fg-orange :weight bold))))
    `(hydra-face-blue ((t (:foreground ,fg-blue :weight bold))))
    `(hydra-face-pink ((t (:foreground ,fg-pink :weight bold))))
    `(hydra-face-red ((t (:foreground ,fg-red :weight bold))))
    `(hydra-face-teal ((t (:foreground ,fg-cyan :weight bold))))

    ;;; Identica
    `(identica-stripe-face ((t (:background ,bg-bright))))
    `(identica-uri-face ((t (:foreground ,fg-orange :underline t))))
    `(identica-username-face ((t (:foreground ,fg-blue :weight bold :underline unspecified))))

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
    `(magit-branch-current ((t (:inherit nil :background ,blue :foreground "#eeeeee"))))
    `(magit-branch-local ((t (:background ,magenta :foreground "#eeeeee"))))
    `(magit-branch-remote ((t (:background ,green :foreground "#ffffff"))))
    `(magit-log-author ((t (:foreground ,red :slant normal :weight normal))))
    `(magit-signature-expired ((t (:foreground ,yellow))))
    `(magit-signature-revoked ((t (:foreground ,magenta))))
    `(magit-bisect-bad ((t (:foreground ,fg-red))))
    `(magit-bisect-good ((t (:foreground ,fg-green))))
    `(magit-bisect-skip ((t (:foreground ,fg-dim))))
    `(magit-blame-date ((t (:foreground ,fg-pink :inherit magit-blame-heading))))
    `(magit-blame-hash ((t (:foreground ,fg-magenta :inherit magit-blame-heading))))
    `(magit-blame-header ((t (:foreground ,fg-green :background ,bg-dim :weight bold :inherit unspecified))))
    `(magit-blame-heading ((t (:foreground ,fg-default :background ,bg-bright))))
    `(magit-blame-name ((t (:foreground ,fg-turquoise :inherit magit-blame-heading))))
    `(magit-blame-summary ((t (:foreground ,fg-green :inherit magit-blame-heading))))
    `(magit-branch ((t (:foreground ,fg-pink :weight bold :inherit unspecified))))
    `(magit-diff-added ((t (:foreground unspecified :background unspecified :inherit diff-added))))
    `(magit-diff-added-highlight ((t (:foreground unspecified :background unspecified :inherit magit-diff-added))))
    `(magit-diff-context ((t (:foreground unspecified :inherit shadow))))
    `(magit-diff-context-highlight ((t (:foreground unspecified :background ,bg-dim :inherit magit-diff-context))))
    `(magit-diff-file-heading ((t (:foreground unspecified :underline unspecified :inherit diff-file-header))))
    `(magit-diff-removed ((t (:foreground unspecified :background unspecified :inherit diff-removed))))
    `(magit-diff-removed-highlight ((t (:foreground unspecified :background unspecified :inherit magit-diff-removed))))
    `(magit-diffstat-removed ((t (:foreground ,red))))
    `(magit-item-highlight ((t (:inherit unspecified))))
    `(magit-log-head-label-default ((t (:foreground ,fg-default :background ,bg-cyan :box (:color ,bg-cyan :line-width 2 :style nil)))))
    `(magit-log-head-label-head ((t (:foreground ,fg-default :background ,bg-blue :box (:color ,bg-blue :line-width 2 :style nil)))))
    `(magit-log-head-label-local ((t (:foreground ,fg-default :background ,bg-magenta :box (:color ,bg-magenta :line-width 2 :style nil)))))
    `(magit-log-head-label-remote ((t (:foreground ,fg-default :background ,bg-green :box (:color ,bg-green :line-width 2 :style nil)))))
    `(magit-log-head-label-tags ((t (:foreground ,fg-default :background ,bg-orange :box (:color ,bg-orange :line-width 2 :style nil)))))
    `(magit-log-sha1 ((t (:foreground ,fg-default :background ,bg-blue :box (:color ,bg-blue :line-width 2 :style nil)))))
    `(magit-process-ng ((t (:foreground ,fg-red :inherit unspecified))))
    `(magit-process-ok ((t (:foreground ,fg-green :inherit unspecified))))
    `(magit-section-heading ((t (:foreground ,fg-turquoise :weight unspecified))))
    `(magit-section-highlight ((t (:background unspecified))))
    `(magit-section-title ((t (:foreground ,white :inherit unspecified))))

    ;; Transient
    `(transient-disabled-suffix ((t (:background ,red :foreground "#1c1c1c" :weight bold))))
    `(transient-enabled-suffix ((t (:background ,green :foreground "#444444" :weight bold))))
    `(transient-separator ((t (:background "#262626" :foreground ,white))))

    ;; Org
    `(org-agenda-restriction-lock ((t (:background "#eeeeee" :foreground "#1c1c1c"))))
    `(org-agenda-calendar-sexp ((t (:foreground ,fg-yellow))))
    `(org-agenda-current-time ((t (:foreground ,fg-orange :weight bold))))
    `(org-agenda-date ((t (:foreground ,bg-cyan))))
    `(org-agenda-date-today ((t (:foreground ,fg-cyan :slant italic))))
    `(org-agenda-date-weekend ((t (:foreground ,fg-cyan))))
    `(org-agenda-done ((t (:foreground ,fg-orange))))
    `(org-agenda-structure ((t (:foreground ,fg-blue))))

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
    `(org-checkbox-statistics-done ((t (:foreground ,bg-cyan))))
    `(org-checkbox-statistics-todo ((t (:foreground ,fg-cyan))))
    `(org-date ((t (:foreground ,fg-pink :underline unspecified))))
    `(org-headline-done ((t (:foreground ,fg-dim))))
    `(org-level-1 ((t (:foreground ,green))))
    `(org-level-2 ((t (:foreground ,cyan))))
    `(org-level-3 ((t (:foreground ,red))))
    `(org-level-4 ((t (:foreground ,blue))))
    `(org-level-5 ((t (:foreground ,yellow))))
    `(org-level-6 ((t (:foreground ,fg-purple))))
    `(org-level-7 ((t (:foreground ,fg-turquoise))))
    `(org-level-8 ((t (:foreground ,fg-orange))))
    `(org-scheduled ((t (:foreground ,fg-dim))))
    `(org-scheduled-previously ((t (:weight bold))))
    `(org-scheduled-today ((t (:foreground ,fg-default))))
    `(org-time-grid ((t (:foreground ,fg-orange))))

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
    `(package-status-avail-obso ((t (:foreground ,bg-green))))
    `(package-status-available ((t (:foreground ,fg-green))))
    `(package-status-dependency ((t (:foreground ,fg-dim))))
    `(package-status-installed ((t (:foreground ,fg-default))))

    ;;; Pretty-print ^L highlight
    `(pp^L-highlight ((t (:box unspecified :foreground ,fg-bright))))

    ;;; Rainbow delimiters
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

    ;;; Rebase mode
    `(rebase-mode-description-face ((t (:foreground ,fg-bright))))

    ;;; reStructuredText
    `(rst-level-1 ((t (:background unspecified))))
    `(rst-level-2 ((t (:background unspecified))))
    `(rst-level-3 ((t (:background unspecified))))
    `(rst-level-4 ((t (:background unspecified))))
    `(rst-level-5 ((t (:background unspecified))))
    `(rst-level-6 ((t (:background unspecified))))

    ;;; Sh
    `(sh-escaped-newline ((t (:foreground ,fg-purple :inherit unspecified :weight bold))))
    `(sh-heredoc ((t (:foreground ,fg-green))))
    `(sh-quoted-exec ((t (:foreground ,fg-pink))))

    ;;; Show paren
    `(show-paren-match ((t (:inverse-video t))))
    `(show-paren-mismatch ((t (:background unspecified :foreground ,fg-red))))

    ;;; Slime
    `(slime-repl-input-face ((t (:foreground ,fg-bright))))
    `(slime-repl-inputed-output-face ((t (:foreground ,fg-bright))))
    `(slime-repl-output-face ((t (:foreground ,fg-default))))
    `(slime-repl-prompt-face ((t (:foreground ,fg-blue))))

    ;;; Smerge
    `(smerge-base ((t (:background ,bg-green))))
    `(smerge-markers ((t (:background ,bg-bright))))
    `(smerge-mine ((t (:background ,bg-blue))))
    `(smerge-other ((t (:background ,bg-red))))
    `(smerge-refined-added ((t (:inherit diff-refine-added))))
    `(smerge-refined-change ((t (:inherit diff-refine-change))))
    `(smerge-refined-removed ((t (:inherit diff-refine-removed))))

    ;;; Term
    `(term-color-black ((t (:background ,bg-default :foreground ,fg-bright))))
    `(term-color-blue ((t (:background ,bg-blue :foreground ,fg-blue))))
    `(term-color-cyan ((t (:background ,bg-cyan :foreground ,fg-cyan))))
    `(term-color-green ((t (:background ,bg-green :foreground ,fg-green))))
    `(term-color-magenta ((t (:background ,bg-magenta :foreground ,fg-magenta))))
    `(term-color-red ((t (:background ,bg-red :foreground ,fg-red))))
    `(term-color-white ((t (:background ,bg-bright :foreground ,fg-default))))
    `(term-color-yellow ((t (:background ,bg-yellow :foreground ,fg-yellow))))

    ;;; Texinfo
    `(texinfo-heading ((t (:foreground ,fg-pink :inherit unspecified :height 1.3))))

    ;;; Tty
    `(tty-menu-selected-face ((t (:background ,red :foreground "#eeeeee"))))

    ;;; Which-key
    `(which-key-key-face ((t (:foreground "#af5f5f"))))
    `(which-key-special-key-face ((t (:inherit which-key-key-face :inverse-video t :weight normal))))

    ;; Term
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
    `(whitespace-big-indent ((t (:background ,red :foreground "#1c1c1c"))))
    `(whitespace-line ((t (:background "#af5f87"))))

    ;;; Widget
    `(widget-button ((t (:inherit button))))
    `(widget-button-pressed ((t (:inherit widget-button :weight bold))))
    `(widget-documentation ((t (:inherit font-lock-doc-face))))
    `(widget-field ((t (:background ,bg-blue :box (:color ,bg-blue :line-width 2)))))

    ;;; rpm-spec-mode
    `(rpm-spec-tag-face ((t (:foreground ,fg-blue))))
    `(rpm-spec-obsolete-tag-face ((t (:background ,bg-red))))
    `(rpm-spec-macro-face ((t (:foreground ,fg-yellow))))
    `(rpm-spec-var-face ((t (:foreground ,fg-cyan))))
    `(rpm-spec-doc-face ((t (:foreground ,fg-magenta))))
    `(rpm-spec-dir-face ((t (:foreground ,fg-turquoise))))
    `(rpm-spec-package-face ((t (:foreground ,fg-red))))
    `(rpm-spec-ghost-face ((t (:foreground ,fg-red))))
    `(rpm-spec-section-face ((t (:foreground ,fg-yellow :underline t))))

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
