;; Syntax highlight of syntactic g-choreographies in Emacs
;; Author: Emilio Tuosto
;; 17/7/2016
;;
;; Taken from http://xahlee.info/emacs/emacs/elisp_syntax_coloring.html
;; with hints from https://www.emacswiki.org/emacs/ModeTutorial
;;
;; There is yet some mess with the prettyfication part (see
;; experimental-sgc-mode.el).
;;

(defconst sgc-mode-hook nil "Hook name of the SGC mode.")

(autoload 'sgc-mode-hook "sgc-mode" "Emacs mode for g-choreographies." t)
(add-hook 'sgc-mode-hook 'prettify-symbols-mode)

(setq-default display-line-numbers t)

(add-to-list 'auto-mode-alist '("\\.sgg\\'" . sgc-mode))
(add-to-list 'auto-mode-alist '("\\.gc\\'" . sgc-mode))

;; Playing with Emacs faces
;; see http://makble.com/how-to-define-emacs-custom-faces-for-text-highlighting
;;
(defun sgc-colors ()
	;; setting the "color scheme"
	(set-face-background 'default "gray10")
	(set-face-foreground 'default "white")
	(set-face-foreground 'font-lock-comment-face "GhostWhite")
	(set-face-background 'show-paren-match "DarkSlateGrey")
	(set-face-foreground 'show-paren-match "orange1")
	;
	(defconst sgc-kwd 'sgc-kwd "Face name for keywords of the SGC mode.")
	(defface sgc-kwd '((t :inverse-video nil)) "Face for SGC keywords.")
	(set-face-attribute 'sgc-kwd nil :weight 'semi-bold :foreground "LightSteelBlue")
	;
	(defconst sgc-sym 'sgc-sym "Face name to use for symbols in the SGC mode.")
	(defface sgc-sym '((t :inverse-video nil)) "Face for SGC symbols.")
	(set-face-attribute 'sgc-sym nil :foreground "LemonChiffon")
	;
	(defconst sgc-brk 'sgc-brk "Face name to use for delimiters in the SGC mode.")
	(defface sgc-brk '((t :inverse-video nil)) "Face for SGC delimiters.")
	(set-face-attribute 'sgc-brk nil :foreground "white")
	;
	(defconst sgc-ptp 'sgc-ptp "Face name to use for participants' identifiers in the SGC mode.")
	(defface sgc-ptp '((t :inverse-video nil)) "Face for SGC participants.")
	(set-face-attribute 'sgc-ptp nil :foreground "aquamarine")
	;
	(defconst sgc-msg 'sgc-msg "Face name to use for messages in the SGC mode.")
	(defface sgc-msg '((t :inverse-video nil)) "Face for SGC messages.")
	(set-face-attribute 'sgc-msg nil :foreground "khaki1" :slant 'italic)
	)

;; SGC syntax highlighting
(setq sgc-highlights
	'(
		("\\(\\.\\..*\\)\\|\\[\\[\\(.\\|\n\\)*\\]\\]+" . font-lock-comment-face)
		("[ \n\t[]+?\\(do\\|in\\|let\\|repeat\\|branch\\|sel\\|unless\\|with\\|be\\)[ \n\t]+?" . sgc-kwd)
		("^\\(do\\|in\\|let\\|repeat\\|branch\\|sel\\|unless\\|with\\|be\\)$" . sgc-kwd)
		("^\\(do\\|in\\|let\\|repeat\\|branch\\|sel\\|unless\\|with\\|be\\)[ \t]+?" . sgc-kwd)
		("[ \t]+\\(=>\\|==\\|\->\\|\|\->\\|\\*\\|\\+\\|\|\\|;\\|:\\|@\\|§\\|\(o\)\\|&\\)" . sgc-sym)
		("\{\\|\}\(\\|\)" . sgc-brk)
		("[A-Z][a-zA-Z0-9]*" . sgc-ptp)
		("[a-z][a-zA-Z0-9]*" . sgc-msg)
		)
	)

(defvar sgc-mode-map
	;; to be extended/finished
  (let ((sgc-map (make-sparse-keymap)))
    (define-key sgc-map "\C-j" 'newline-and-indent)
    (define-key sgc-map (kbd "RET") 'newline-and-indent)
    (define-key sgc-map (kbd "<C-return>") 'reindent-then-newline-and-indent)
    sgc-map)
  "Keymap for SGC major mode")

(autoload 'sgc-mode "sgc-mode" "SGC mode." t)
(add-to-list 'auto-mode-alist '("\\.sgc" . sgc-mode))
(add-to-list 'auto-mode-alist '("\\.gc" . sgc-mode))

;; Prettification
(defun to-unicode ()
  "Mapping to symbols for pretty printing"
	;; https://en.wikipedia.org/wiki/List_of_Unicode_characters
	(setq prettify-symbols-alist
				'(
					("|"   . 9125)    ; |
					("->"  . 10230)   ; ⟶
					("=>"  . 8658)    ; ⇒
					("<=>" . 8660)    ; ⇔
					("<="  . 8804)    ; ≤
					(">="  . 8805)    ; ≥
					("map" . 8614)    ; ↦ 
					("|->" . 8796)    ; ≜
					("==" . 8796)    ; ≜
					("[["  . 10214)
					("]]"  . 10215)
					("alpha" . 945)
					("beta" . 946)
					("gamma" . 947)
					("delta" . 948)
					("epsilon" . 949)
					("zeta" . 950)
					("eta" . 951)
					("teta" . 952)
					("iota" . 953)
					("kappa" . 954)
					("lambda" . 955)
					("mu" . 956)
					("nu" . 957)
					("xi" . 958)
					("omicron" . 959)
					("pi" . 960)
					("rho" . 961)
					("sigmaa" . 962)
					("sigma" . 963)
					("tau" . 964)
					("upsilon" . 965)
					("phi" . 966)
					("chi" . 967)
					("psi" . 968)
					("omega" . 969)
					("partial" . 8706)
					("theta" . 8708)
					("kappax" . 8709)
					("Delta" . 8710)
					("Nabla" . 8711)
					("epsilonx" . 8712)
					;; ("{"   . 9128)    ; ⎨
					;; ("}"   . 9132)    ; ⎬
					("(o)" . 9673)    ; ⓞ
					)
				)
	;;	(add-hook 'sgc-mode-hook 'prettify-symbols-mode)
	(prettify-symbols-mode)
	)

;; Indentation
;; rules
;;   rule 1. if at the beginning of buffer, indentation col = 0
;;   rule 2. if the current line starts with '}' decrease indentation
;;   rule 3. if the line before ends with '{' or 'let' increse indentation
;;   rule 4. in any other case, the indentation is as on the previous line
;;
(setq indent-tabs-mode t)
;;;; (setq tab-width 10)                                               ;;;; ?????????
(global-set-key (kbd "TAB") 'self-insert-command)
(defvar sgc-indent-offset 2 "Indentation offset for `sgc-mode'.")

(defun mydiff (x y)
	(setq tmp (- x y))
	(if (< tmp 0) 0 tmp)
	)

(defun get-previous-indent()
	(progn
		(save-excursion
			(forward-line -1)
			(current-indentation)
			)
		)
	)

(defun sgc-indent-line ()
  "Indent current line for `sgc-mode'."
  (interactive)
  (beginning-of-line)
	(let (cur-indent)
		(progn
			(if (bobp)  ; Check for rule 1
					(setq cur-indent 0)
				(progn
					;; Check for rule 2
					(if (looking-at ".*\\(\}[ \t;]*\\|in[ \t\{\(]*\\|&\\)$")
							(setq cur-indent (mydiff (get-previous-indent) sgc-indent-offset))
						;; otherwise, check for rule 3
						(progn
							(save-excursion
								(forward-line -1)
								(if (looking-at ".*\\(\{\\|in\\|let\\|=\\|\\[\\)$")
										(setq cur-indent (+ (current-indentation) sgc-indent-offset))
									;; otherwise, apply rule 4
									(setq cur-indent (current-indentation))
									)
								)
							)
						)
					(if cur-indent
							(indent-line-to cur-indent)
						(indent-line-to 0)
						)
					)
				)
			)
		)
	)

;; end Indentation

;;; (define-derived-mode sgc-mode fundamental-mode "eM's sgc mode" "Emacs mode for g-choreographies."
(defun sgc-mode ()
  (interactive)
  (kill-all-local-variables)
  (setq font-lock-defaults '(sgc-highlights))
	(sgc-colors)
	;; setting comment syntax
	(set (make-local-variable 'comment-start) ".. ")
	(set (make-local-variable 'comment-end) " ..")
	(set (make-local-variable 'comment-start-skip) ".+")
	;; customising indentation
  (set (make-local-variable 'indent-line-function) 'sgc-indent-line)
  (use-local-map sgc-mode-map)
  (setq mode-name "sgc-mode")
  (font-lock-fontify-buffer)
	;; setting parenthesis highlighting
	(electric-pair-mode 1)
	(electric-indent-mode 1)
	(set-face-attribute 'show-paren-match nil :weight 'extra-bold)
	(setq show-paren-style 'expression)
	(to-unicode)
	)

(provide 'sgc-mode)

;; 2*16**3 + 5*16**2 + 8*16 + 14
