;;; nyaatouch --- an ergonomic dvorak modal scheme for emacs -*- lexical-binding: t; -*-

;; Author: esrh
;; Keywords: modal-editing
;; Package-Requires: ((avy "0.5.0") (swiper "0.13.0")(smartparens "1.11.0") (meow "1.4.2") (emacs "27.1"))
;; Version: 0.0.1
;;
;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Code:
(require 'meow)
(require 'smartparens)
(require 'avy)
(require 'swiper)
(require 'cl-lib)

(define-key key-translation-map [?\C-x] [?\C-u])
(define-key key-translation-map [?\C-u] [?\C-x])

(defun nt-negative-find ()
  (interactive)
  (let ((current-prefix-arg -1))
    (call-interactively 'meow-find)))

(put 'upcase-region 'disabled nil)

(defun uppercasep (c) (and (= ?w (char-syntax c)) (= c (upcase c))))

(defun downcase-char ()
  (interactive)
  (save-excursion
    (let ((ch (thing-at-point 'char t)))
      (delete-char 1)
      (insert (downcase ch)))))

(defun nt-toggle-case-dwiam ()
  "toggle cases, do what i actually mean:

If no region is active, toggle between upcase and downcase on the
current character. If a region is active, then if there exists at
least one upcase char in the region, then downcase the whole
region. Otherwise, upcase the whole region."
  (interactive)
  (if (region-active-p)
      (let ((region (buffer-substring-no-properties
                     (region-beginning) (region-end))))
        (message "%s" region)
        (if (cl-remove-if-not #'uppercasep (string-to-list region))
            (downcase-region (region-beginning) (region-end))
          (upcase-region (region-beginning) (region-end))))
    (if (uppercasep (string-to-char (thing-at-point 'char t)))
        (downcase-char)
      (upcase-char 1))))

(defun replace-bounds (strt end content)
  (delete-region strt end)
  (insert (number-to-string content)))

(defun nt-add-number (arg)
  (interactive "P")
  (let* ((num (thing-at-point 'number t))
         (bounds (bounds-of-thing-at-point 'word))
         (strt (car bounds))
         (end (cdr bounds)))
    (message "%s" arg)
    (if arg
        (replace-bounds strt end (+ num arg))
      (replace-bounds strt end (+ num 1)))))

(defun nt-subtract-one ()
  (interactive)
  (let ((current-prefix-arg -1))
    (call-interactively 'add-number)))

(defun nt-goto-top ()
  (interactive)
  (let ((res (sp-up-sexp)))
    (while res
      (setq res (sp-up-sexp)))))

(defun nt-insert-at-cursor ()
  (interactive)
  (if meow--temp-normal
      (progn
        (message "Quit temporary normal mode")
        (meow--switch-state 'motion))
    (meow--cancel-selection)
    (meow--switch-state 'insert)))


(defun nt-duplicate ()
  "Duplicate region if active. Otherwise duplicate char at point"
  (interactive)
  (if (region-active-p)
      (progn
        (meow-save)
        (meow-yank))
    (progn
      (meow-save-char)
      (meow-yank))))

(defun nt-duplicate-and-comment ()
  "Duplicate region and then comment region"
  (interactive)
  (if (region-active-p)
      (progn
        (meow-save)
        (meow-yank)
        (comment-or-uncomment-region
         (region-beginning)
         (region-end)))))

(setq meow-cheatsheet-layout meow-cheatsheet-layout-dvorak)
(meow-motion-overwrite-define-key
 '("h" . meow-next)
 '("t" . meow-prev)
 '("<escape>" . ignore))

;; expansion
(meow-normal-define-key
 '("0" . meow-expand-0)
 '("9" . meow-expand-9)
 '("8" . meow-expand-8)
 '("7" . meow-expand-7)
 '("6" . meow-expand-6)
 '("5" . meow-expand-5)
 '("4" . meow-expand-4)
 '("3" . meow-expand-3)
 '("2" . meow-expand-2)
 '("1" . meow-expand-1)
 '(";" . meow-reverse))

;; movement
(meow-normal-define-key
 '("'" . meow-find)
 '("\"" . nt-negative-find)
 '("<" . meow-beginning-of-thing)
 '(">" . meow-end-of-thing)
 '("k" . meow-back-word)
 '("K" . meow-back-symbol)
 '("x" . meow-next-word)
 '("X" . meow-next-symbol)
 '("_" . meow-goto-line)
 '("t" . meow-prev)
 '("T" . meow-prev-expand)
 '("h" . meow-next)
 '("H" . meow-next-expand)
 '("c" . meow-right)
 '("C" . meow-right-expand)
 '("f" . meow-left)
 '("F" . meow-left-expand))

(defun nt-inner-str () (interactive) (meow-inner-of-thing 'string))
(defun nt-bounds-str () (interactive) (meow-bounds-of-thing 'string))
(defun nt-paragraph () (interactive) (meow-inner-of-thing 'string))

;; selection
(meow-normal-define-key
 '("," . meow-inner-of-thing)
 '("." . meow-bounds-of-thing)
 '("a" . meow-line)
 '("o" . nt-inner-str)
 '("O" . nt-bounds-str)
 '("e" . meow-mark-word)
 '("E" . meow-mark-symbol)
 '("i" . meow-block)
 '("I" . meow-block-expand)
 '("p" . meow-join))

;; insertion actions
(meow-normal-define-key
 '("u" . nt-insert-at-cursor)
 '("U" . meow-insert))

;; selection actions
(meow-normal-define-key
 '("D" . meow-yank)
 '("d" . meow-save)
 '("n" . meow-change)
 '("N" . meow-replace)
 '("s" . meow-kill)
 '("g" . meow-open-below)
 '("G" . meow-open-above)
 '("l" . meow-undo)
 '("L" . meow-undo-in-selection))

;; other actions
(meow-normal-define-key
 '("w" . meow-cancel-selection)
 '("j" . meow-grab)
 '("v" . repeat)
 '("q" . meow-quit)
 '("b" . meow-paren-mode)
 '("z" . meow-pop-selection)
 '("<escape>" . ignore))

;; extras
(meow-normal-define-key
 '("*" . nt-toggle-case-dwiam)
 '("+" . nt-add-number)
 '("_" . nt-subtract-one)
 '("/" . nt-duplicate)
 '("?" . nt-duplicate-and-comment)
 '("m" . avy-goto-word-1)
 '("-" . swiper))

(define-key meow-beacon-state-keymap [remap nt-insert-at-cursor] 'meow-beacon-insert)


(setq meow-paren-keymap (make-keymap))

(meow-define-state paren
  "paren state"
  :lighter " [P]"
  :keymap meow-paren-keymap)

(setq meow-cursor-type-paren 'hollow)

(defun nt-wrap-string () (interactive) (sp-wrap-with-pair "\""))
(defun nt-back-transpose () (interactive) (sp-transpose-sexp -1))

(meow-define-keys 'paren
  '("<escape>" . meow-normal-mode)
  '("f" . sp-backward-sexp)
  '("r" . sp-forward-sexp)
  '("h" . sp-down-sexp)
  '("t" . sp-up-sexp)
  '("o s" . sp-wrap-square)
  '("o r" . sp-wrap-round)
  '("o c" . sp-wrap-curly)
  '("o g" . nt-wrap-string)
  '("O" . sp-unwrap-sexp)
  '("b" . sp-slurp-hybrid-sexp)
  '("x" . sp-forward-barf-sexp)
  '("k" . sp-backward-barf-sexp)
  '("j" . sp-backward-slurp-sexp)
  '("s" . sp-raise-sexp)
  '("n" . sp-absorb-sexp)
  '("," . sp-split-sexp)
  '("e" . sp-end-of-sexp)
  '("a" . sp-beginning-of-sexp)
  '("G" . sp-goto-top)
  '("y" . sp-transpose-sexp)
  '("Y" . nt-back-transpose)
  '("l" . meow-undo))

(setq meow-keypad-start-keys
      '((?c . ?c)
        (?x . ?x)
        (?o . ?h)))

(meow-leader-define-key
 '("a" . "M-x")
 '("e" . "C-x b")
 '("u" . "C-x C-f")

 '("h" . "C-x o")
 '("t" . "C-x 0")
 '("T" . "C-x 1")
 '("s" . "C-x C-k")
 '("S" . "C-x k")
 '("n" . "C-x 3")
 '("N" . "C-x 2")

 '("1" . meow-digit-argument)
 '("2" . meow-digit-argument)
 '("3" . meow-digit-argument)
 '("4" . meow-digit-argument)
 '("5" . meow-digit-argument)
 '("6" . meow-digit-argument)
 '("7" . meow-digit-argument)
 '("8" . meow-digit-argument)
 '("9" . meow-digit-argument)
 '("0" . meow-digit-argument)
 '("/" . meow-keypad-describe-key)
 '("?" . meow-cheatsheet))

(setq latex-thing-regexp
      '(regexp "\\\\begin{.*?}\\(.*?\\)\n\\|\\$"
               "\\\\end{.*?}\n\\|\\$"))

(meow-thing-register 'latex
		             latex-thing-regexp
                     latex-thing-regexp)

(add-to-list 'meow-char-thing-table
	         (cons ?x 'latex))

(setq meow-use-clipboard t)

(defun meow-clipboard-toggle ()
  (interactive)
  (if meow-use-clipboard
      (progn
        (setq meow-use-clipboard nil)
        (message "Meow clipboard usage disabled"))
    (progn
      (setq meow-use-clipboard t)
      (message "Meow clipboard usage enabled"))))

(meow-leader-define-key '("l" . meow-clipboard-toggle))

(setq meow-expand-exclude-mode-list '())

(setq avy-keys '(?a ?o ?e ?u ?i ?d ?h ?t ?n ?s))

(setq meow-command-to-short-name-list
      (append meow-command-to-short-name-list
              '((nt-add-number . "+num")
                (nt-bounds-str . "[str]")
                (nt-inner-str . "←str→")
                (nt-insert-at-cursor . "ins.")
                (nt-negative-find . "←find")
                (nt-paragraph . "[para]")
                (nt-subtract-one . "-1")
                (nt-toggle-case-dwiam . "case"))))

;;;###autoload
(defun turn-on-nyaatouch ()
  (interactive)
  (meow-global-mode 1))

;;;###autoload
(defun turn-off-nyaatouch ()
  (interactive)
  (meow-global-mode -1))

(provide 'nyaatouch)
;;; nyaatouch.el ends here