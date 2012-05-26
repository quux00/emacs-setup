;; ---------------------------------------------------------- ;;
;; ------------------- General Settings --------------------- ;;
;; ---------------------------------------------------------- ;;

(transient-mark-mode)          ; region between mark and point is highlighted only when 'active'
(global-linum-mode)            ; puts line numbers on left side of screen
(column-number-mode)           ; puts column numbers on the taskbar
(fset 'yes-or-no-p 'y-or-n-p)  ; changes 'yes' 'no' prompts to 'y' and 'n'(
(delete-selection-mode)        ; the active region can be replaced just be starting to type
(tool-bar-mode -1)             ; turn off the toolbar
(mouse-avoidance-mode 'exile)  ; if cursor nears mouse, make the cursor move away automatically
(global-auto-revert-mode 1)    ; auto refresh buffers
(setq global-auto-revert-non-file-buffers t)  ; Also auto refresh dired

;; remove if this becomes a problem
(add-hook 'before-exit-hook 'delete-trailing-whitespace)

;; default size of frame
(add-to-list 'default-frame-alist '(height . 40))
(add-to-list 'default-frame-alist '(width . 118))

(setq scroll-step 1
      mouse-yank-at-point 't       ; mouse will paste at point, not where you click
      require-final-newline t)

;; hide or get rid of annoying backups and auto-saves
(setq backup-by-copying t             ; don't clobber symlinks
      backup-directory-alist
      '(("." . "~/.emacs.d/saves"))   ; don't litter my fs tree
      ;; backup-inhibited t           ; uncomment this one?
      auto-save-default nil
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)              ; use versioned backups


;; UTF-8 - probably not necessary anymore, but just in case
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Add parts of each file's directory to the buffer name if not unique
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)


;; ---------------------------------------------------------- ;;
;; ----- Experimental => try out and document or delete ----- ;;
;; ---------------------------------------------------------- ;;
; I don't know what this does - research or remove!!
(autoload 'filladapt-mode "filladapt" nil t)
; Show keystrokes in progress - not sure what this means
(setq echo-keystrokes 0.1)


;; ----------------------------------------------------------- ;;
;; ----- start the emacs server - so can use emacsclient ----- ;;
;; ----------------------------------------------------------- ;;
(require 'server)
(unless (server-running-p)
  (server-start))


;; ---------------------------------------------------- ;;
;; ---------- Kill/Delete Whole Line Section ---------- ;;
;; ---------------------------------------------------- ;;

(defun delete-whole-line ()
  "Delete a line without retaining it in kill-ring"
  (interactive)
  (let ((kill-whole-line t))
    (beginning-of-line)
    (kill-line)                         ; kill the whole line
    (setq kill-ring (cdr kill-ring))))  ; and then erase it from the kill-ring

(defun copy-whole-line ()
  "Do a kill-line but copy rather than kill"
  (interactive)
  (let ((kill-whole-line t))
    (beginning-of-line)
    (kill-line)
    (yank)))

;; TODO: need to decide between these two ...
(global-set-key "\C-cd"    'delete-whole-line)
(global-set-key "\C-c\C-d" 'delete-whole-line)
(setq kill-whole-line t)       ; make the C-k kill line also remove the newline char(s)
                                        ;=> Note: this "kill-whole-line" does not refer to the function
                                        ;=> normally assigned to C-S-backspace, but rather to what C-k
                                        ;=> points to
(global-set-key "\C-c\C-k" 'kill-whole-line)  ; by default it is C-S-backspace
                                        ; this will delete the entire line no matter where the
                                        ; cursor is and it will put the line in the kill-ring
(global-set-key "\C-c\C-j" 'copy-whole-line)
;; ---------- END Kill/Delete Whole Line Section ---------- ;;



;; -------------------------------------------------------- ;;
;; ---------------- Tab and Spaces Settings --------------- ;;
;; -------------------------------------------------------- ;;
(setq-default indent-tabs-mode nil)     ; insert spaces instead of tab chars
(setq indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default c-basic-offset 2)         ; make programming-langs use an indent of 2 spaces
(setq c-basic-indent 2)                 ; this sets the basic indent for c-based major modes
(setq js-indent-level 2)                ; works for the basic javascript mode (not js2-mode)
(setq indent-line-function 'insert-tab)
(setq tab-stop-list '(2 4 6 8 10 12 14 16 18 20 22 24 26 28 30))


;; ------------------------------------------------------- ;;
;; ------------- Make emacs use the clipboard ------------ ;;
;; ------------------------------------------------------- ;;
(setq x-select-enable-clipboard t)
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)


;; ------------------------------------------------------- ;;
;; -------- Save point position between sessions --------- ;;
;; ------------------------------------------------------- ;;
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file "~/.emacs.d/.places")


;; --------------------------------------------------- ;;
;; -------------- Adjust 'writing modes' ------------- ;;
;; --------------------------------------------------- ;;
;; Adapted from: https://github.com/maryrosecook/emacs/blob/master/init.el

;; narrower window, better line wrapping for prose
(defun write-words ()
  (interactive)
  (set-frame-width nil 90)
  (global-visual-line-mode t)
  (setq mode-line-format nil)
  (show-paren-mode nil))

;; widescreen, no line-wrap
(defun write-code ()
  (interactive)
  (set-frame-width nil 126)
  (global-visual-line-mode 0)
  (show-paren-mode)
  (setq mode-line-format
        (list "-"
              'mode-line-mule-info
              'mode-line-modified
              'mode-line-frame-identification
              'mode-line-buffer-identification
              " "
              'mode-line-position
              '(vc-mode vc-mode)
              " "
              'mode-line-modes
              '(which-func-mode ("" which-func-format))
              '(global-mode-string (global-mode-string))
              )))


;; --------------------------------------------------------------- ;;
;; -------------- TODO: JUST ADDED - need to test !!! ------------- ;;
;; ---------------------------------------------------------------- ;;
;; Original idea from
;; http://www.opensubscriber.com/message/emacs-devel@gnu.org/10971693.html
;; from: http://www.emacswiki.org/emacs/CommentingCode
(defun comment-dwim-line (&optional arg)
  "Replacement for the comment-dwim command.
   If no region is selected and current line is not blank and we are not at the end of the line,
   then comment current line.
   Replaces default behaviour of comment-dwim, when it inserts comment at the end of the line."
  (interactive "*P")
  (comment-normalize-vars)
  (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    (comment-dwim arg)))
(global-set-key "\M-;" 'comment-dwim-line)

;; ------------------------------------------------------ ;;
;; -------------- My preferred key bindings ------------- ;;
;; ------------------------------------------------------ ;;
(global-set-key [(meta g)]          'goto-line)
(global-set-key [(control shift l)] 'goto-line)
(global-set-key "\C-c\C-c" 'comment-region)
(global-set-key "\C-c\C-u" 'uncomment-region)
;(global-set-key "\M-;"     'comment-or-uncomment-region)
(global-set-key "\C-c;"    'comment-indent)
(global-set-key "\C-x\C-d" 'electric-buffer-list) ; one of my favorite things in emacs ...
(global-set-key "\C-c\C-k" 'kill-whole-line)      ; delete whole line (including newline) from anywhere
(global-set-key "\C-x\C-k" 'kill-region)          ; normally mapped to \C-w
(global-set-key "\C-w"     'backward-kill-word)   ; make emacs more like bash shell
(global-set-key "\C-z"     'undo)                 ; too ingrained from years of Windoze ...
(global-set-key "\C-x\C-z" 'shell)
(global-set-key [(control \;)] 'dabbrev-expand)   ; I find the M-/ binding awkward
(global-set-key (kbd "RET") 'newline-and-indent)  ; indent previous line after
(global-set-key (read-kbd-macro "M-s") 'query-replace)
(global-set-key (read-kbd-macro "C-x w") 'write-words)
(global-set-key (read-kbd-macro "C-x c") 'write-code)
(global-set-key "\C-x\C-v" 'scroll-up)            ; to align with my Eclipse settings
(global-set-key "\C-c o" 'occur)                   ; occur takes regex to show all occurances in file

;; Use control-arrow keys for window resizing
;; <<MP note: turned off since these don't seem to work>>
;; (global-set-key (kbd "<C-f11>") 'enlarge-window-horizontally)
;; (global-set-key (kbd "<C-f12>") 'shrink-window-horizontally)


;; ----------------------------------------------------- ;;
;; ------------- Load Files && Load-Path --------------- ;;
;; ----------------------------------------------------- ;;
(add-to-list 'load-path "~/.emacs.d/site-lisp")
(load-file "~/.emacs.d/macros.el")   ; load my macros


;; ----------------------------------------------------- ;;
;; ------------- Mine goes to 11: chords! -------------- ;;
;; ----------------------------------------------------- ;;
(require 'key-chord)
(key-chord-mode 1)

;; General purpose chords
(key-chord-define-global "jk"    'dabbrev-expand)
(key-chord-define-global "90"    "()")
(key-chord-define-global ",,"    'indent-for-comment)
(key-chord-define-global "a\;"   "@")
(key-chord-define-global "s\;"   "$")
(key-chord-define-global "df"    "\C-b")
(key-chord-define-global "cl"    "console.log();\C-b\C-b")
(key-chord-define-global "fj"    "\C-f")      ;; ahead one space
(key-chord-define-global "<>"    "<>\C-b")

;; chords for Clojure coding
(key-chord-define-global "d\;"   "#{}\C-b")  ;; \C-b is "backspace">?<>

;; chords for JavaScript coding
(key-chord-define-global "jl"    'jslambda)  ;; jslambda is a macro I defined
(key-chord-define-global "fn"    'jsfunc)         
(key-chord-define-global "jq"    "$('')\C-b\C-b") ;; for jquery

;; --------------------------------------------------------- ;;
;; ------------------ Color Theme Support ------------------ ;;
;; --------------------------------------------------------- ;;

(add-to-list 'load-path "/usr/share/emacs/site-lisp/emacs-goodies-el/color-theme.el")
(require 'color-theme)

;; (add-to-list 'load-path "~/.emacs.d/site-lisp/color-themes")
;; (load-file "~/.emacs.d/site-lisp/color-themes/zenburn-theme.el")
;; (require 'color-theme-zenburn)
;; (color-theme-zenburn)
  
;; uncomment this if you want color-theme to autoload
;; (eval-after-load "color-theme"
;;   '(progn
;;      (color-theme-initialize)
;;      (color-theme-calm-forest)))

;; my own theme loading functions
;; these will open a new frame and put the theme in just that frame
(defun theme-char ()
  (interactive)
  (select-frame (make-frame))
  (color-theme-initialize)
  (set-variable 'color-theme-is-global nil)
  (color-theme-charcoal-black))

(defun theme-calm ()
  (interactive)
  (select-frame (make-frame))
  (color-theme-initialize)
  (set-variable 'color-theme-is-global nil)
  (color-theme-calm-forest))

(defun theme-law ()
  (interactive)
  (select-frame (make-frame))
  (color-theme-initialize)
  (set-variable 'color-theme-is-global nil)
  (color-theme-lawrence))

;; don't use - doesn't work => error reported to author 15-Jan-2012
(defun theme-zen ()
  (add-to-list 'load-path "~/.emacs.d/site-lisp/color-themes")
  (interactive)
  (color-theme-initialize)
  (select-frame (make-frame))
  (require 'color-theme-zenburn)  
  (color-theme-initialize)
  (set-variable 'color-theme-is-global nil)
  ;; custom theme from: https://github.com/bbatsov/zenburn-emacs
  (color-theme-charcoal-black)
  (color-theme-zenburn))

;; not interactive - intended to be called by interactive methods
;; thsub and thsubx
(defun theme-subdued (new-frame)
  (add-to-list 'load-path "~/.emacs.d/site-lisp/color-themes")
  ;; (interactive)
  (color-theme-initialize)
  (if new-frame
      (select-frame (make-frame)))
  (require 'color-theme-subdued)  
  (color-theme-initialize)
  (set-variable 'color-theme-is-global nil)
  ;; custom theme from: http://jblevins.org/projects/emacs-color-themes/
  (color-theme-charcoal-black)
  (color-theme-subdued))

;; load the subdued color theme in the current frame
(defun thsub ()
  (interactive)
  (theme-subdued nil))

;; load the subdued color theme in (and only in) a new frame
(defun thsubx ()
  (interactive)
  (theme-subdued t))


;; --------------------------------------------------------- ;;
;; ---------- Programming/Markup Language Support ---------- ;;
;; --------------------------------------------------------- ;;

;; Steve Yegge's JavaScript major mode: http://code.google.com/p/js2-mode
(autoload 'js2-mode "js2" nil t)
;; (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; Scala mode
(add-to-list 'load-path "~/.emacs.d/site-lisp/scala-mode")
(require 'scala-mode-auto)

;; yaml support
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-hook 'yaml-mode-hook
          '(lambda ()
             (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

;; Clojure mode
;; git clone git://github.com/technomancy/clojure-mode.git
(add-to-list 'load-path "~/.emacs.d/site-lisp/clojure-mode")
(require 'clojure-mode)
;;(autoload 'clojure-mode "clojure-mode" "A major mode for Clojure" t)
(add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))

;; Haml mode
(require 'haml-mode)
(add-hook 'haml-mode-hook
          '(lambda ()
             (setq indent-tabs-mode nil)
             (define-key haml-mode-map "\C-m" 'newline-and-indent)))

;; PHP support, for those sad moments I have to use it ...
;; workaround from usual (require 'php-mode), since there is a bug in emacs23 around this
;; From: http://beyondteck.blogspot.com/2010/05/making-php-mode-on-emacs-23-work.html
(autoload 'php-mode "php-mode" "Major mode for editing php code." t)
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.inc$" . php-mode))

;; CoffeeScript support
;; git clone git://github.com/defunkt/coffee-mode.git
(add-to-list 'load-path "~/.emacs.d/site-lisp/coffee-mode")
(require 'coffee-mode)
(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
(add-to-list 'auto-mode-alist '("Cakefile" . coffee-mode))

;; Markdown mode
(autoload 'markdown-mode "markdown-mode.el"
  "Major mode for editing Markdown files" t)
(setq auto-mode-alist
      (cons '("\\.md" . markdown-mode) auto-mode-alist))

;; Ruby support
(add-to-list 'auto-mode-alist '("[Rr]akefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rjs$" . ruby-mode))
(add-hook 'ruby-mode-hook (lambda () (local-set-key "\r" 'newline-and-indent)))

;; yasnippet
;; git clone git://github.com/capitaomorte/yasnippet.git
(add-to-list 'load-path "~/.emacs.d/site-lisp/yasnippet")
(require 'yasnippet)
(yas/global-mode 1)

;; cucumber mode and support
;; git clone git://github.com/michaelklishin/cucumber.el.git
(add-to-list 'load-path "~/.emacs.d/site-lisp/cucumber.el")
(require 'feature-mode)
(require 'cucumber-mode)
(add-to-list 'auto-mode-alist '("\.feature$" . feature-mode))

;; load bundle snippets for cucumber
(yas/load-directory "~/.emacs.d/site-lisp/cucumber.el/snippets/feature-mode")

;; mumamo (multiple major modes) support -> mostly good for html and php, so disabled by default
;; (load "~/.emacs.d/site-lisp/nxhtml/autostart.el")


;; --------------------------------------------------- ;;
;; --------------- Dirtree and Friends --------------- ;;
;; --------------------------------------------------- ;;

(require 'tree-mode)
(require 'windata)
(require 'dirtree)
;(autoload 'dirtree "dirtree" "Add directory to tree view" t)
(global-set-key "\C-o" 'dirtree-show)



;; ----------------- ruby-debug support ------------------- ;;
;;(require 'rdebug)   ; ~TODO: need to figure this out - not working


;; Highlight FIXME:, TODO: and DEBUG: keywords in c-mode-common and ruby-mode
;;note that these keywords must be followed by a colon ...
(add-hook 'c-mode-common-hook
          (lambda ()
            (font-lock-add-keywords
             nil '(("\\<\\(FIXME:\\|TODO:\\|DEBUG:\\):" 1 font-lock-warning-face t)))))

(add-hook 'ruby-mode-hook
          (lambda ()
            (font-lock-add-keywords
             nil '(("\\<\\(FIXME:\\|TODO:\\|DEBUG:\\|lambda\\)" 1 font-lock-warning-face t)))))

(add-hook 'clojure-mode-hook
          (lambda ()
            (font-lock-add-keywords
             nil '(("\\<\\(FIXME:\\|TODO:\\|DEBUG:\\)" 1 font-lock-warning-face t)))))

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (font-lock-add-keywords
             nil '(("\\<\\(FIXME:\\|TODO:\\|DEBUG:\\)" 1 font-lock-warning-face t)))))

(add-hook 'scala-mode-hook
          (lambda ()
            (font-lock-add-keywords
             nil '(("\\<\\(FIXME:\\|TODO:\\|DEBUG:\\|lambda\\)" 1 font-lock-warning-face t)))))

(add-hook 'js-mode-hook
          (lambda ()
            (font-lock-add-keywords
             nil '(("\\<\\(\\$\\.\\|FIXME:\\|TODO:\\|DEBUG:\\|lambda\\)" 1 font-lock-warning-face t)))))

;; (add-hook 'ruby-mode-hook
;;           (lambda ()
;;             (font-lock-add-keywords
;;              nil '(("\\<\\(lambda\\)" 1 font-lock-warning-face t)))))



;; ---------------------------------------------------------- ;;
;; ----------------- Enable paredit mode -------------------- ;;
;; ---------------------------------------------------------- ;;
(autoload 'enable-paredit-mode "paredit"
  "Turn on pseudo-structural editing of Lisp code."
  t)

;; always enable paredit for clojure
(add-hook 'clojure-mode-hook    'enable-paredit-mode)
(add-hook 'lisp-mode-hook       'enable-paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)

;; paredit in slime
(add-hook 'slime-repl-mode-hook (lambda () (paredit-mode +1)))
	
;; (eval-after-load "slime" 
;;   '(progn (slime-setup '(slime-repl))	
;; 	(defun paredit-mode-enable () (paredit-mode 1))	
;; 	(add-hook 'slime-mode-hook 'paredit-mode-enable)	
;; 	(add-hook 'slime-repl-mode-hook 'paredit-mode-enable)
;; 	(setq slime-protocol-version 'ignore)))


;; ---------------------------------------------------------- ;;
;; ------------------------ SLIME --------------------------- ;;
;; ---------------------------------------------------------- ;;
;; (eval-after-load "slime" 
;;   '(progn (slime-setup '(slime-repl))))

;; (add-to-list 'load-path "~/.emacs.d/site-lisp/slime")
;; (require 'slime)
;; (slime-setup)


;; ---------------------------------------------------------- ;;
;; ------------------ org-mode settings --------------------- ;;
;; ---------------------------------------------------------- ;;
(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(setq org-startup-folded nil )
(setq word-wrap t)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ck" 'org-agenda)  ; note this is C-ca in the org-mode doc
(setq org-todo-keywords
      '((sequence "TODO" "IN-PROGRESS" "QUESTION(q)" "DONE(d)" "DEFERRED" "DATEDONE")))
(setq org-todo-keyword-faces
           '(("DONE"     . (:foreground "#94ff94"))
             ("QUESTION" . (:foreground "#ff65ff"))
             ("DEFERRED" . (:foreground "#ffc358"))
             ))
(setq org-log-done t)
(org-remember-insinuate)  ; use remember.el in org-mode
(setq org-default-notes-file (concat org-directory "/remember.org"))
(define-key global-map "\C-cr" 'org-remember)  ; note this is C-cc in the org-mode doc

;; ---------------------------------------------------------- ;;
;; --------------- Printing and PDF support ----------------- ;;
;; ---------------------------------------------------------- ;;
;; Use the Printing Package
(require 'printing)
(pr-update-menus)

;; set the PDF printer as default
;;(setq printer-name "PDF_file_generator")
;;(setq printer-name t)
(setq ps-printer-name "PDF_file_generator")
(setq ps-printer-name t)

(defun print-to-pdf ()
  (interactive)
  (ps-spool-buffer-with-faces)
  (switch-to-buffer "*PostScript*")
  (write-file "~/tmp.ps")
  (kill-buffer "tmp.ps")
  (setq cmd (concat "ps2pdf14 /tmp/tmp.ps " (buffer-name) ".pdf"))
  (shell-command cmd)
  (shell-command "rm /tmp/tmp.ps")
  (message (concat "Saved to:  " (buffer-name) ".pdf"))
  )


;; --------------------------------------------------------- ;;
;; ------------- Newline conversion functions -------------- ;;
;; --------------------------------------------------------- ;;
;; Convert CR-LF to LF
;;  => TODO: not sure I need these, see note near end of this file
(defun dos-to-unix (file-path)
  (interactive "fFile name: ")
  (save-excursion
    (let (dos-buffer)
      (setq dos-buffer (find-file file-path))
      (set-buffer-file-coding-system 'unix)
      (save-buffer)
      (kill-buffer dos-buffer))))

;; Convert LF to CR-LF
(defun unix-to-dos (file-path)
  (interactive "fFile name: ")
  (save-excursion
    (let (unix-buffer)
      (setq unix-buffer (find-file file-path))
      (set-buffer-file-coding-system 'dos)
      (save-buffer)
      (kill-buffer unix-buffer))))


;; ----------------------------------------------------- ;;
;; ---------------- Shell support / use ---------------- ;;
;; ----------------------------------------------------- ;;
;; load my aliases
(add-to-list 'auto-mode-alist '("\\.bash_aliases$" . sh-mode))

;; Make colours in Emacs' shell look normal
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; Don't auto-truncate lines in shell mode
(add-hook 'shell-mode-hook '(lambda () (toggle-truncate-lines 1)))

;; start with shell closed - find I don't use it much
;; (shell)


;; ------------------------------------------------------ ;;
;; ----------------- Things Yet to Try ------------------ ;;
;; ------------------------------------------------------ ;;
;; ===> ido:  http://www.masteringemacs.org/articles/2010/10/10/introduction-to-ido-mode/
;; ;; enable ido mode
;; (require 'ido)
;; (ido-mode t)
;; (setq ido-enable-flex-matching t)


;; --------------------------------------------- ;;
;; ------------ Reminders and Notes ------------ ;;
;; --------------------------------------------- ;;
;; Replace spaces with tabs using M-x tabify.
;; Replace tabs with spaces using M-x untabify.
;; change 'file coding system' with 'set-buffer-file-coding-system RET unix or dos
;; or C-x RET f unix or dos
;; Use C-x <left> and C-x <right> to rapidly switch between buffers



;; ---------------------------------------------------- ;;
;; ---------- Colors, Faces and Auto-settings --------- ;;
;; ---------------------------------------------------- ;;
(set-background-color "black")
(set-foreground-color "yellow")
(set-cursor-color "white")

;;------ Automatic settings: Do Not Edit These ------- ;;
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(inhibit-startup-screen t)
 '(show-paren-mode t)
 '(tab-stop-list (quote (2 4 6 8 10 12 14 16 18 20 22 24 26 28 30)))
 '(tool-bar-mode nil))

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "black" :foreground "yellow" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight bold :height 98 :width normal :foundry "unknown" :family "DejaVu Sans Mono"))))
 '(font-lock-warning-face ((((class color) (background dark)) (:foreground "SkyBlue" :background "black")))))

;;'(show-paren-mode t nil (paren))
;;note: Cornflower Blue is also good font color

(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
