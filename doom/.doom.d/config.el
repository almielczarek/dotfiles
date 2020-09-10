;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Alexander Mielczarek"
      user-mail-address "almielczarek@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "Source Code Pro" :size 12))

(custom-set-faces! '(org-headline-done :foreground "#606F73"))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-henna)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.

(add-to-list 'auto-mode-alist '("\\.leex\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))

(defun web-hook ()
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))

(add-hook 'web-mode-hook 'web-hook)

(after! org
  (add-to-list 'org-modules 'org-habit)
  (add-to-list 'org-modules 'org-drill)
  (setq org-agenda-files '("~/org/work.org" "~/org/personal.org"))
  (setq org-tags-column -80)
  (setq org-log-done 'time)
  (setq org-startup-folded t)
  (setq org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "FOLLOWUP(f)" "WAITING(w)" "INACTIVE(i)" "STARTED(s)" "DELEGATED(D@)" "REPEATING(r)" "|" "CANCELLED(c)" "DONE(d)")))
  (org-babel-do-load-languages 'org-babel-load-languages '((ledger . t)))
  (setq org-capture-templates
        '(("P" "Templates for projects")
          ("Pt" "Project-local todo" entry  ; {project-root}/todo.org
           (file+headline +org-capture-project-todo-file "Inbox")
           "* TODO %?\n%i\n%a" :prepend t)
          ("Pn" "Project-local notes" entry  ; {project-root}/notes.org
           (file+headline +org-capture-project-notes-file "Inbox")
           "* %U %?\n%i\n%a" :prepend t)
          ("Pc" "Project-local changelog" entry  ; {project-root}/changelog.org
           (file+headline +org-capture-project-changelog-file "Unreleased")
           "* %U %?\n%i\n%a" :prepend t)
          ("b" "Personal bookmark" entry (file "~/org/personal.org")
           "* TODO %?")
          ("t" "Work TODO" entry (file+headline "~/org/work.org" "Tasks")
           "* TODO %?")
          ("T" "Work TODO (clocked)" entry (file+headline "~/org/work.org" "Tasks")
           "* DONE %?" :clock-in t :clock-resume t)
          ("p" "Phone call" entry (file+headline "~/org/work.org" "Tasks")
           "* TODO %? :PHONE:" :clock-in t :clock-resume t))))

(defun counsel-shell (command)
  (interactive
   (list
    (read-shell-command "$ ")))
  (start-process-shell-command command nil command))

(map!
 "M-B" '+ivy/switch-buffer
 "C-c c" 'org-capture
 "s-C" 'kill-buffer-and-window
 "s-<return>" '+vterm/here

 :leader
 "f$" (lambda () (interactive (async-shell-command "miniserve -u .")))
 "pe" 'projectile-run-vterm
 "bb" 'switch-to-buffer
 "." (lambda () (interactive) (dired "."))
 "oc" 'calendar
 "o~" (lambda () (interactive (dired "~")))
 "op" (lambda () (interactive (dired "~/projects")))
 "oD" (lambda () (interactive (dired "~/rockwood/development")))
 "od" (lambda () (interactive (dired "~/.dotfiles")))
 "ow" (lambda () (interactive) (find-file "~/org/work.org")))

(map! :map org-mode-map :n "t" 'org-todo)
(map! :map org-agenda-mode-map "C-l" 'org-agenda-log-mode)

(after! persp-mode
  (setq persp-emacsclient-init-frame-behaviour-override "main"))

(add-hook 'elixir-mode-hook
          (lambda () (add-hook 'before-save-hook 'elixir-format nil t)))

(defvar my-org-capture-before-config nil
  "Window configuration before `org-capture'.")

(defadvice org-capture (before save-config activate)
  "Save the window configuration before `org-capture'."
  (setq my-org-capture-before-config (current-window-configuration)))

(add-hook 'org-capture-mode-hook 'delete-other-windows)

(defun my-org-capture-cleanup ()
  "Clean up the frame created while capturing via org-protocol."
  ;; In case we run capture from emacs itself and not an external app,
  ;; we want to restore the old window config
  (when my-org-capture-before-config
    (set-window-configuration my-org-capture-before-config))
  (-when-let ((&alist 'name name) (frame-parameters))
    (when (equal name "org-protocol-capture")
      (delete-frame))))

(add-hook 'org-capture-after-finalize-hook 'my-org-capture-cleanup)

(require 'exwm)
(require 'exwm-config)
(exwm-config-default)

(require 'ivy-posframe)
;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-top-center)))
(setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-center)))
(setq ivy-posframe-parameters '((parent-frame nil)))
(ivy-posframe-mode 1)

(setq exwm-input-global-keys
      `(([?\s-r] . exwm-input-toggle-keyboard) ([?\M-x] . counsel-M-x) ([?\s-p] . counsel-shell) ([?\s-h] . evil-window-left) ([?\s-k] . evil-window-up) ([?\s-j] . evil-window-down) ([?\s-l] . evil-window-right)))

(push ?\C-g exwm-input-prefix-keys)
(push ?\M-1 exwm-input-prefix-keys)
(push ?\M-2 exwm-input-prefix-keys)
(push ?\M-3 exwm-input-prefix-keys)
(push ?\M-4 exwm-input-prefix-keys)
(push ?\M-5 exwm-input-prefix-keys)
(push ?\M-6 exwm-input-prefix-keys)
(push ?\M-7 exwm-input-prefix-keys)
(push ?\M-8 exwm-input-prefix-keys)
(push ?\M-9 exwm-input-prefix-keys)
(push ?\M-B exwm-input-prefix-keys)

(exwm-input-set-key (kbd "M-y") #'my/exwm-counsel-yank-pop)
(exwm-input-set-key (kbd "M-SPC") doom-leader-map)

(setq display-time-day-and-date t)
(display-time)
(global-auto-revert-mode)

(setq initial-buffer-choice "~/org/work.org")

(defun my/exwm-counsel-yank-pop ()
  "Same as `counsel-yank-pop' and paste into exwm buffer."
  (interactive)
  (let ((inhibit-read-only t)
        ;; Make sure we send selected yank-pop candidate to
        ;; clipboard:
        (yank-pop-change-selection t))
    (call-interactively #'counsel-yank-pop))
  (when (derived-mode-p 'exwm-mode)
    ;; https://github.com/ch11ng/exwm/issues/413#issuecomment-386858496
    (exwm-input--set-focus (exwm--buffer->id (window-buffer (selected-window))))
    (exwm-input--fake-key ?\C-v)))


(let ((device-specific-config "~/.doom.d/device.el"))
  (when (file-exists-p device-specific-config)
    (load-file device-specific-config)))
