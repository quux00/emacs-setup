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
(add-hook 'before-save-hook 'delete-trailing-whitespace)

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

(autoload 'filladapt-mode "filladapt" nil t)  ; I don't know what this does - research or remove!!


;; ----------------------------------------------------------- ;;
;; ----- start the emacs server - so can use emacsclient ----- ;;
;; ----------------------------------------------------------- ;;
(server-start)


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

;; --------------------------------------------------- ;;
;; -------------- Adjust 'writing modes' ------------- ;;
;; --------------------------------------------------- ;;
;; From: https://github.com/maryrosecook/emacs/blob/master/init.el

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
  (set-frame-width nil 124)
  (set-frame-height nil 95)
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


;; ------------------------------------------------------ ;;
;; -------------- My preferred key bindings ------------- ;;
;; ------------------------------------------------------ ;;
(global-set-key [(meta g)]          'goto-line)
(global-set-key [(control shift l)] 'goto-line) 
(global-set-key "\C-c\C-c" 'comment-region)
(global-set-key "\M-;"     'comment-or-uncomment-region)
(global-set-key "\C-c;"    'comment-indent) 
(global-set-key "\C-c\C-u" 'uncomment-region)
(global-set-key "\C-x\C-d" 'electric-buffer-list)
(global-set-key "\C-c\C-k" 'kill-whole-line)    ; delete whole line (including newline) from anywhere
(global-set-key "\C-x\C-k" 'kill-region)        ; normally mapped to \C-w
(global-set-key "\C-w"     'backward-kill-word) ; make emacs more like bash shell
(global-set-key "\C-z"     'undo)
(global-set-key "\C-x\C-z" 'undo)
(global-set-key [(control \;)] 'dabbrev-expand) ; I find the M-/ binding awkward

;; Use control-arrow keys for window resizing
;; <<MP note: these below don't work>>
;; (global-set-key (kbd "<C-f11>") 'enlarge-window-horizontally)
;; (global-set-key (kbd "<C-f12>") 'shrink-window-horizontally)


;; ----------------------------------------------------- ;;
;; ---------------- Primary Load-Path ------------------ ;;
;; ----------------------------------------------------- ;;
(add-to-list 'load-path "~/.emacs.d/site-lisp")



;; --------------------------------------------------------- ;;
;; ---------- Programming/Markup Language Support ---------- ;;
;; --------------------------------------------------------- ;;

;; Steve Yegge's JavaScript major mode: http://code.google.com/p/js2-mode
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

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
(autoload 'clojure-mode "clojure-mode" "A major mode for Clojure" t)
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
;; from: https://github.com/defunkt/coffee-mode
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
(add-to-list 'auto-mode-alist '("\\.ru$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rjs$" . ruby-mode))
(add-hook 'ruby-mode-hook (lambda () (local-set-key "\r" 'newline-and-indent)))


;; ----------------- ruby-debug support ------------------- ;;
;;(require 'rdebug)   ; ~TODO: need to figure this out - not working


;; Highlight FIXME:, TODO: and DEBUG: keywords in c-mode-common and ruby-mode
;;note that these keywords must be followed by a colon ...
(add-hook 'c-mode-common-hook
          (lambda ()
            (font-lock-add-keywords 
             nil '(("\\<\\(FIXME\\|TODO\\|DEBUG\\):" 1 font-lock-warning-face t)))))

(add-hook 'ruby-mode-hook
          (lambda ()
            (font-lock-add-keywords 
             nil '(("\\<\\(FIXME:\\|TODO:\\|DEBUG:\\|lambda\\)" 1 font-lock-warning-face t)))))

(add-hook 'clojure-mode-hook
          (lambda ()
            (font-lock-add-keywords 
             nil '(("\\<\\(FIXME:\\|TODO:\\|DEBUG:\\|lambda\\)" 1 font-lock-warning-face t)))))

(add-hook 'scala-mode-hook
          (lambda ()
            (font-lock-add-keywords 
             nil '(("\\<\\(FIXME:\\|TODO:\\|DEBUG:\\|lambda\\)" 1 font-lock-warning-face t)))))

(add-hook 'javascript-mode-hook
          (lambda ()
            (font-lock-add-keywords 
             nil '(("\\<\\(FIXME:\\|TODO:\\|DEBUG:\\|lambda\\)" 1 font-lock-warning-face t)))))

;; (add-hook 'ruby-mode-hook
;;           (lambda ()
;;             (font-lock-add-keywords 
;;              nil '(("\\<\\(lambda\\)" 1 font-lock-warning-face t)))))




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
      '((sequence "TODO" "IN-PROGRESS" "|" "DONE" "DEFERRED" "NOTE")))
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
;; start with shell open
(shell)


;; ----------------------------------------------------- ;;
;; ------------------- emacs macros -------------------- ;;
;; ----------------------------------------------------- ;;

;; Note: how to create, name and save an emacs macro
;;   Create by: C-x ( <do macro steps here> C-x )
;;   Name it with: C-x C-k n (kmacro-name-last-macro)
;;   Open file you want to save it in and type: M-x insert-kbd-macro <RET> macroname <RET>

;; ---- general macros ---- ;
;; this will uncommont the current line you are on and only the current line
;; do not set mark and point for this, just invoke for a single give line
(fset 'uc
      [?\C-a ?\C-  down ?\C-u ?\C-c ?\C-c up])

;; "highlight comments" for C-based comment languages
;; produces: /* ---[  ]--- */
(fset 'hic
      [tab ?/ ?* ?  ?- ?- ?- ?\[ ?  ?  ?\] ?- ?- ?- ?  ?* ?/ left left left left left left left left])

;; "highlight comments" for Ruby-like comment languages
;; produces: # ---[  ]--- #
(fset 'ric
      [tab ?# ?  ?- ?- ?- ?\[ ?  ?  ?\] ?- ?- ?- ?  ?# left left left left left left left])


;; ---- xml macros ---- ;
(fset 'xcom
      [tab ?< ?! ?- ?- ?  ?  ?- ?- ?> left left left left])

;; ---- Makefile macros ---- ;
(fset 'mktmpl
      [?C ?C ?= ?t backspace ?g ?c ?c return ?C ?F ?L ?A ?G ?S ?= ?- ?W ?a ?l ?l ?  ?- ?p ?e ?d ?a ?n ?t ?i ?c ?  ?- ?s ?t ?d ?= ?c ?9 ?9 return return return ?h ?e ?l ?p ?: return tab ?@ ?e ?c ?h ?o ?  ?\" ?T ?a ?r ?g ?e ?t ?s ?\" return tab ?@ ?e ?g ?r ?e ?p ?  ?\" ?^ ?\( ?\\ ?- ?| ?\\ ?w ?| ?_ ?\) ?+ ?: ?\" ?  ?\[ ?M ?m ?\} ?a backspace backspace ?\] ?a ?k ?e ?f ?i ?l ?e return return ?c ?l ?e ?a ?n ?: return tab ?@ ?r ?m ?  ?- ?f ?  ?* ?. ?o return tab ?@ ?f ?i ?l ?e ?  ?* ?  ?| ?  ?g ?r ?e ?p ?  ?E ?L ?F ?  ?| ?  ?c ?u ?t ?  ?\" ?- ?d ?: ?\" ?  ?- ?f ?1 ?  ?| ?  ?x ?a ?r ?g ?s ?  ?r ?m ?  ?2 ?> ?/ ?d ?e ?v ?/ ?n ?u ?l ?l ?\; ?  ?l ?s ?  ?> ?/ ?d ?e ?v ?/ ?n ?u ?l ?l ?\; return up up up up up up up up up return])


;; ---- ruby macros --- ;
(fset 'rdd
      [tab ?# ?D ?E ?B ?U ?G ?\C-j ?p ?u ?t ?s ?\C-j ?# ?E ?N ?D ?\S-  ?D ?E ?B ?U ?G up ? ])

(fset 'rmain
      [?# ?! ?/ ?u ?s ?r ?/ ?b ?i ?n ?/ ?e ?n ?v ?  ?r ?u ?b ?y return return ?r ?e ?q ?u ?i ?r ?e ?  ?\' ?r ?u ?b ?y ?g ?e ?m ?s ?\' return ?r ?e ?q ?u ?i ?r ?e ?  ?\' ?o ?p ?t ?p ?a ?r ?s ?e ?\' return return ?d ?e ?f ?  ?m ?a ?i ?n return return ?e ?n ?d return return ?d ?e ?f ?  ?f return return ?e ?n ?d return return ?i ?f ?  ?$ ?0 ?  ?= ?= ?  ?_ ?_ ?F ?I ?L ?E ?_ ?_ return return ?e ?n ?d up ?  ?  ?m ?a ?i ?n up up up up up up up up tab])

(fset 'rcom
      [tab ?# ?  ?D ?e ?s ?c return tab ?# return tab ?# return ?# left tab right up up ?  down ?  down ?  ?@ ?r ?e ?t ?u ?r ?n ?  ?\[ ?A ?r ?r ?a ?y ?< ?S ?t ?r ?i ?n ?g ?> ?\] ?  ?r ?e ?t ?u ?r ?n ?s ?  ?x ?x ?x up ?@ ?p ?a ?r ?a ?m ?  ?\[ ?N ?u ?m ?b ?e ?r ?\] ?  ?n ?u ?m ?  ?s ?i ?z ?e up up left left left left])

(fset 'private
      [tab ?# ?# ?# ?# ?# ?# ?# ?# ?# ?# ?# ?# ?# ?# ?# ?# ?# ?# ?# ?# ?# ?# ?# ?# ?# ?# ?# ?# ?# ?# ?# ?# ?# return tab ?p ?r ?i ?v ?a ?t ?e ?  ?# ?# ?# ?# ?# ?# ?# ?# ?# ?# ?# ?# ?# ?# ?# ?# ?# ?# ?# ?# ?# ?# ?# ?# ?# return tab ?# ?# ?# ?# ?# ?# ?# ?# ?# ?# ?# ?# ?# ?# ?# ?# ?# ?# ?# ?# ?# ?# ?# ?# ?# ?# ?# ?# ?# ?# ?# ?# ?# ?\C-j])

;; describe for rspec
(fset 'rde
      [tab ?d ?e ?s ?c ?r ?i ?b ?e ?  ?\" ?\" ?  ?d ?o return tab ?i ?t ?  ?\" ?s ?h ?o ?u ?l ?d ?  ?\" ?  ?d ?o return tab ?e ?n ?d tab return tab ?e ?n ?d tab up up up right right right right right right right])

;; template for OptionParser command line option parsing
(fset 'ropts
      (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([tab 111 112 116 112 97 114 115 101 32 61 32 79 112 116 105 111 110 97 backspace 80 97 114 115 101 114 46 110 101 119 32 32 100 111 32 124 124 left 111 112 116 115 right return return 101 110 100 tab up tab 111 112 116 115 46 98 97 110 110 101 114 32 61 32 34 85 115 97 103 101 58 32 109 46 114 98 32 91 79 80 84 73 79 78 83 93 32 102 105 108 101 92 110 34 return return tab 35 32 100 101 102 105 110 101 32 116 104 101 32 111 112 116 105 111 110 115 return tab 111 112 116 105 111 110 115 91 58 118 101 114 98 111 115 101 93 32 61 32 102 97 108 115 101 return tab 111 112 116 105 backspace 115 46 111 110 57 39 118 backspace backspace backspace 40 39 118 39 left left 45 right right 44 32 32 backspace 39 45 45 118 101 114 98 111 115 101 39 44 32 39 79 117 116 112 117 116 32 109 111 114 101 32 105 110 102 111 109 114 97 116 105 111 110 backspace backspace backspace backspace backspace backspace backspace 114 109 97 116 105 111 110 39 41 32 100 111 return tab 111 112 116 105 111 110 115 91 58 118 101 114 98 111 115 101 93 32 61 32 116 114 117 101 return tab 101 110 100 tab return return tab 111 112 116 105 111 110 115 91 58 118 97 108 93 32 61 32 110 105 108 return tab 111 112 116 115 46 111 110 40 39 45 111 39 44 32 39 45 45 111 115 32 79 83 39 44 32 39 83 112 101 99 105 102 121 32 88 88 88 39 41 32 100 111 32 124 120 124 return return 101 110 100 tab up tab 111 112 116 105 111 110 115 91 58 118 97 108 93 32 61 32 120 down return return tab 35 32 104 101 108 112 32 115 99 114 101 101 110 return tab 111 112 116 115 46 111 110 40 39 45 104 39 44 32 39 45 45 104 101 108 112 39 44 32 39 68 105 115 112 108 97 121 32 116 104 105 115 32 115 99 114 101 101 110 39 41 32 100 111 return tab 112 117 116 115 32 111 112 116 115 return tab 101 120 105 116 return tab 101 110 100 tab down 32 32 35 32 101 110 100 32 79 112 116 105 111 110 80 97 114 115 101 114 46 110 101 119 32 98 108 111 99 107 return return tab 35 32 112 97 114 115 101 32 65 82 71 86 32 backspace 44 32 114 101 109 111 118 105 110 103 32 97 110 121 111 112 116 105 111 backspace backspace backspace backspace backspace 32 111 112 116 105 111 110 115 32 97 110 100 32 116 104 101 105 114 32 112 97 114 97 109 115 return tab 111 112 116 112 97 114 115 101 46 112 97 114 115 101 33] 0 "%d")) arg)))

(fset 'rtempfile
      (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([tab 114 101 113 117 105 114 101 32 39 116 101 109 112 102 105 108 101 39 return tab 116 109 112 102 105 108 101 32 61 32 84 101 109 112 102 105 108 101 46 110 101 119 40 39 109 121 116 109 112 39 41 return tab 116 109 112 102 105 108 101 46 112 97 116 104 return tab 116 109 112 102 105 108 101 32 60 60 32 34 116 101 120 116 34 return tab 116 109 112 102 105 108 101 46 102 108 105 115 104 backspace backspace backspace 117 115 104 return tab 116 109 112 102 105 108 101 46 99 108 111 115 101 return up up up up up up down right right right right right right right right right right right right right right right right right right right right right right right right right right right right right right] 0 "%d")) arg)))


(fset 'pry
      [tab ?# ?D ?E ?B ?U ?G return tab ?b ?i ?n ?d ?i ?n ?g ?. ?p ?r ?y return tab ?# ?E ?N ?D ?\S-  ?D ?E ?B ?U ?G])


;; ---- java macros --- ;
;; adds a space and then puts balancing braces
(fset 'p
      [?  ?\{ return return ?\} up tab])

(fset 'jtry
      [tab ?t ?r ?y ?  ?\{ return return ?\} ?  ?c ?a ?t ?c ?h ?  ?\( ?E ?x ?c ?e ?p ?t ?i ?o ?n ?  ?e ?\) ?  ?\{ return return ?\} ?  ?f ?i ?n ?a ?l ?l ?y ?  ?\{ return return ?\} up up up up up tab])

(fset 'sop
      (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([tab 83 121 115 116 101 109 46 111 117 116 46 112 114 105 110 116 108 110 40 41 59 left left] 0 "%d")) arg)))

(fset 'sep
      [tab ?S ?y ?s ?t ?e ?m ?. ?e ?r ?r ?. ?p ?r ?i ?n ?t ?l ?n ?\( ?\" ?E ?R ?R ?O ?R ?: ?  ?\" ?  ?+ ?\S-  ?e ?. ?t ?o ?S ?t ?r ?i ?n ?g ?\( ?\) ?\) ?\;])

(fset 'jdd
      (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([tab 47 47 32 68 69 66 85 71 return return tab 47 47 32 69 78 68 33554464 68 69 66 85 71 up tab 83 121 115 116 101 109 46 111 117 116 46 112 114 105 110 116 108 110 40 41 59 left left] 0 "%d")) arg)))

(fset 'jdoc
      [?/ ?* ?* return ?  ?* return ?  ?* return ?  ?* return ?  ?* ?/ up ?  up ?  up ? ])

(fset 'jmain
      [?i ?m ?p ?o ?r ?t ?  ?j ?a ?v ?a ?. ?u ?t ?i ?l ?. ?* ?\; return return ?p ?u ?b ?l ?i ?c ?  ?c ?l ?a ?s ?s ?  ?X ?  ?\{ return return return ?\} up ?  ?p ?u ?b ?l ?i ?c ?  ?s ?t ?a ?t ?i ?c ?  ?v ?o ?i ?d ?  ?m ?a ?i ?n ?\( ?S ?t ?r ?i ?n ?g ?\[ ?\] ?  ?a ?r ?g ?s ?\) ?  ?\{ return return ?\} up up up return return up ?p ?u ?b ?l ?i ?c ?  ?X ?\( ?\) ?  ?\{ return return ?\} up up right right right right right right right right right up up right right])

(fset 'jtry
      [tab ?t ?r ?y ?  ?\{ return return return ?\} ?  ?c ?a ?t ?c ?h ?  ?\( ?E ?x ?c ?e ?p ?t ?i ?o ?n ?  ?e ?\) ?  ?\{ return return ?\} ?  ?f ?i ?n ?a ?l ?l ?y ?  ?\{ return return ?\} up up up up up up right up tab])

;; --- Java neo4j macros --- ;;
(fset 'txneo
      [tab ?T ?r ?a ?n ?s ?a ?c ?t ?i ?o ?n ?  ?t ?x ?  ?= ?  ?d ?b ?. ?b ?e ?g ?i ?n ?g backspace ?T ?x ?\( ?\) ?\; ?\C-j ?t ?r ?y ?  ?\{ ?\C-j ?\C-j ?\C-j ?t ?x ?. ?s ?u ?c ?c ?e ?s ?s ?\( ?\) ?\; return tab ?\} ?  ?c ?a ?t ?c ?h ?  ?\( ?E ?x ?c ?e ?p ?t ?i ?o ?n ?  ?e ?\) ?  ?\{ ?\C-j ?e ?. ?p ?r ?i ?n ?t ?S ?t ?a ?c ?k ?T ?r ?a ?c ?e ?\( ?\) ?\; ?\C-j ?t ?h ?r ?o ?w ?  ?n ?e ?w ?  ?R ?u ?n ?t ?i ?m ?e ?E ?x ?c ?e ?p ?t ?i ?o ?n ?\( ?e ?\) ?\; return ?\} ?  ?f ?i ?n ?a ?l ?l ?y ?  ?\{ return return ?\} up tab ?t ?x ?. ?f ?i ?n ?i ?s ?h ?\( ?\) ?\; up up up up up up up tab])



;; --- javascript macros --- ;
(fset 'jsdd
      [tab ?/ ?/ ?D ?E ?B ?U ?G ?\C-j ?a ?l ?e ?r ?t ?\( ?\" ?\" ?\) ?\; ?\C-j ?/ ?/ ?E ?N ?D ?\S-  ?D ?E ?B ?U ?G up left left left])

(fset 'jss-for
      [tab ?f ?o ?r ?  ?\( ?v ?a ?r ?  ?i ?= ?0 ?\; ?  ?i ?  ?< ?  ?a ?r ?y ?. ?l ?e ?n ?g ?t ?h ?\; ?  ?i ?+ ?+ ?\) ?  ?\{ return return ?\} up up right right right right right right right right right right right right right right right right right right right right])


;; --- C macros --- ;
(fset 'cmain
      [?# ?i ?n ?c ?l ?u ?d ?e ?  ?< ?s ?t ?d ?i ?o ?. ?h ?> return return ?i ?n ?t ?  ?m ?a ?i ?n ?\( ?\) ?  ?\{ return return return return return ?\} up tab ?r ?e ?t ?u ?r ?n ?  ?0 ?\; up up tab])

(fset 'cmain2
      [?# ?i ?n ?c ?l ?u ?d ?e ?  ?< ?s ?t ?d ?i ?o ?. ?h ?> return ?# ?i ?n ?c ?l ?u ?d ?e ?  ?< ?s ?t ?d ?b ?o ?o ?l ?. ?h ?> return ?# ?i ?n ?c ?l ?u ?d ?e ?  ?< ?s ?t ?d ?l ?i ?b ?. ?h ?> return return ?i ?n ?t ?  ?m ?a ?i ?n ?\( ?\) ?  ?\{ return return return ?\} up tab ?r ?e ?t ?u ?r ?n ?  ?E ?X ?I ?T ?_ ?S ?U ?C ?C ?E ?S ?S ?\; up return return up tab])

(fset 'cmain3
      [?# ?i ?n ?c ?l ?u ?d ?e ?  ?< ?s ?t ?d ?i ?o ?. ?h ?> return ?# ?i ?n ?c ?l ?u ?d ?e ?  ?< ?s ?t ?d ?b ?o ?o ?l ?. ?h ?> return ?# ?i ?n ?c ?l ?u ?d ?e ?  ?< ?s ?t ?d ?l ?i ?b ?. ?h ?> return ?# ?i ?n ?c ?l ?u ?d ?e ?  ?< ?s ?t ?r ?i ?n ?g ?. ?h ?> return return return ?/ ?* ?* return ?  ?* return ?  ?* return ?  ?* return ?  ?* ?/ return return ?i ?n ?t ?  ?m ?a ?i ?n ?\( ?i ?n ?t ?  ?a ?r ?g ?c ?, ?  ?c ?h ?a ?r ?  ?* ?a ?r ?g ?v ?\[ ?\] ?\) ?  ?\{ return return return return return return ?\} up ?  ?  ?r ?e ?t ?u ?r ?n ?  ?E ?X ?I ?T ?_ ?S ?U ?C ?C ?E ?S ?S ?\; up up up ?  ? ])


(fset 'cfor
      [?f ?o ?r ?  ?\( ?i ?  ?= ?  ?0 ?\; ?  ?i ?  ?< ?  ?S ?I ?Z ?E ?\; ?  ?i ?+ ?+ ?\) ?  ?\{ return return ?\} up tab])

(fset 'cdd
      [tab ?/ ?* ?\S-  ?D ?E ?B ?U ?G ?  ?* ?/ return return tab ?/ ?* ?\S-  ?E ?N ?D ?\S-  ?D ?E ?B ?U ?G ?  ?* ?/ up tab ?p ?r ?i ?n ?t ?f ?\( ?\" ?\\ ?n ?\" ?\) ?\; left left left left left])

(fset 'ccom
      [tab ?/ ?* ?  ?  ?* ?/ left left left])

;; --- CoffeeScript macros --- ;;
(fset 'cfdd
      [?# ?  ?D ?E ?B ?U ?G return ?E ?N ?D ?\S-  ?D ?E ?B ?U ?G up return backspace backspace ?c ?o ?n ?s ?o ?l ?e ?. ?l ?o ?g ? ])

(fset 'cfcom
      [tab ?# return return return ?@ ?p ?a ?r ?a ?m ?  ?\[ ?T ?y ?p ?e ?\] ?  ?t return ?@ ?r ?e ?t ?u ?r ?n ?  ?\[ ?T ?y ?p ?e ?\] ?  ?t up up up down ?  up ?  up ? ])


;; --- macro aliases --- ;;
(defalias 'jcom 'jdoc)


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
 ;; '(standard-indent 2)
 '(tab-stop-list (quote (2 4 6 8 10 12 14 16 18 20 22 24 26 28 30))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "black" :foreground "yellow" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight bold :width normal))))
 ;;'(default ((t (:inherit nil :stipple nil :background "black" :foreground "yellow" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight bold :height 113 :width normal :foundry "monotype" :family "Courier New"))))
 '(font-lock-warning-face ((((class color) (background dark)) (:foreground "SkyBlue" :background "black")))))

;;'(show-paren-mode t nil (paren))
;;note: Cornflower Blue is also good font color

(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
