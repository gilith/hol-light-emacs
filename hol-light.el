;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SIMPLE ELISP MACROS TO SUPPORT HOL LIGHT PROOF IN EMACS
;; Copyright (c) 2015 Joe Leslie-Hurd, distributed under the MIT license
;;
;; For documentation and updates see https://github.com/gilith/hol-light-emacs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Parsing large tactic proofs may reach the default elisp recursion
;; depths (500 and 1300), so we increase them.

(setq max-lisp-eval-depth 10000)
(setq max-specpdl-size 25000)

;; This prevents line truncation in my preferred Emacs layout of a
;; single horizontally-split window with the proof script on the left
;; and the HOL Light process on the right.

(setq truncate-partial-width-windows nil)

;; Example key bindings.

(global-set-key [?\C-c ?\C-l] 'shell-recenter)
(global-set-key [?\C-c ?\C-r] 'shell-send-region)
(global-set-key [?\C-c ?\C-c] 'shell-send-paragraph)
(global-set-key [?\C-c ?\C-w] 'shell-send-word)
(global-set-key [?\C-c ?\C-g] 'shell-set-goal)
(global-set-key [?\C-c ?\C-e] 'shell-expand-tactic)
(global-set-key [?\C-c ?\C-b] 'shell-undo-tactic)
(global-set-key [?\C-c ?\C-p] 'shell-goto-proof-point)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun string-trim-start (s)
  "Remove whitespace at the start of S."
  (if (string-match "\\`[ \t\n\r]+" s)
      (replace-match "" t t s)
    s))

(defun string-trim-end (s)
  "Remove whitespace at the end of S."
  (if (string-match "[ \t\n\r]+\\'" s)
      (replace-match "" t t s)
    s))

(defun string-trim (s)
  "Remove whitespace at the start and end of S."
  (string-trim-start (string-trim-end s)))

(defun string-chomp-start (arg s)
  "Remove ARG from start of string S, if it is a prefix."
  (let ((sl (length s))
        (argl (length arg)))
    (if (and (<= argl sl) (string-equal (substring s 0 argl) arg))
        (substring s argl sl)
      s)))

(defun string-chomp-end (arg s)
  "Remove ARG from end of string S, if it is a suffix."
  (let ((sl (length s))
        (argl (length arg)))
    (if (and (<= argl sl) (string-equal (substring s (- sl argl) sl) arg))
        (substring s 0 (- sl argl))
      s)))

(defun region-string-raw ()
  (string-trim (buffer-substring (region-beginning) (region-end))))

(defun get-paragraph ()
  (save-excursion
    (forward-paragraph 1)
    (let ((end (point)))
      (backward-paragraph)
      (string-trim (buffer-substring (point) end)))))

(defun get-word ()
  (save-excursion
    (skip-chars-backward "[:alnum:]_")
    (let ((start (point)))
      (skip-chars-forward "[:alnum:]_")
      (string-trim (buffer-substring start (point))))))

(defun shell-recenter-raw ()
  (let (proc pbuf pwin)
    (setq proc (get-process "shell"))
    (setq pbuf (process-buffer proc))
    (display-buffer pbuf t)
    (setq pwin (get-buffer-window pbuf t))
    (save-selected-window
      (with-current-buffer pbuf
        (select-window pwin)
        (goto-char (process-mark proc))
        (recenter -1)))))

(defun shell-send-string-raw (cmd)
  (let (proc pbuf)
    (setq proc (get-process "shell"))
    (setq pbuf (process-buffer proc))
    (with-current-buffer pbuf
      (goto-char (process-mark proc))
      (insert cmd)
      (move-marker (process-mark proc) (point))
      (process-send-string proc cmd))
    (shell-recenter-raw)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parsing (prefixes of) HOL Light tactic proofs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-proof-point-regexp (reg re start)
  (let ((i (string-match re reg start)))
    (if i
      (let ((j (match-end 0)))
        (list (substring reg i j) i j))
      i)))

(defun parse-proof-point-string (reg str start)
  (let ((i (string-match str reg start)))
    (if i (match-end 0) (error "parse-proof-point: no \"%s\" found" str))))

(defun parse-proof-point-quote (reg start)
  (parse-proof-point-string reg "`" start))

(defun parse-proof-point-goal (reg start)
  (let ((i (parse-proof-point-quote reg start)))
    (let ((j (parse-proof-point-quote reg i)))
      (list (substring reg (- i 1) j) j))))

(defun parse-proof-point-tactic-clean (s)
  (let ((x (string-trim-end
             (string-chomp-end "THEN"
               (string-chomp-end "THENL"
                 (string-trim s))))))
    (if (string-equal x "") "" (concat "e(" x ");;\n"))))

(defun parse-proof-point-tactic-suffix (reg start current)
  (let ((s-i-j (parse-proof-point-regexp reg "[][`;]" current)))
    (if s-i-j
      (let ((s (car s-i-j))
            (i (car (cdr s-i-j)))
            (j (car (cdr (cdr s-i-j)))))
        (cond
          ((string-equal s "`")
           (let ((k (parse-proof-point-quote reg j)))
             (parse-proof-point-tactic-suffix reg start k)))
          ((string-equal s "[")
           (let ((c-ts (parse-proof-point-tactic-list reg j)))
             (if (car c-ts)
               (parse-proof-point-tactic-suffix reg start (cadr c-ts))
               (let ((x (parse-proof-point-tactic-clean
                          (substring reg start i))))
                 (cons nil (cons x (cdr c-ts)))))))
          (t (list t s i j))))
      (list nil (parse-proof-point-tactic-clean (substring reg start))))))

(defun parse-proof-point-tactic-list (reg start)
  (let ((c-t (parse-proof-point-tactic-suffix reg start start)))
    (if (car c-t)
      (let ((s (car (cdr c-t)))
            (i (car (cdr (cdr c-t))))
            (j (car (cdr (cdr (cdr c-t))))))
        (if (string-equal s "]")
           (list t j)
           (let ((c-ts (parse-proof-point-tactic-list reg j)))
             (if (car c-ts)
               c-ts
               (let ((x (parse-proof-point-tactic-clean
                          (substring reg start i))))
                 (cons nil (cons x (cdr c-ts))))))))
      c-t)))

(defun parse-proof-point-tactic (reg start)
  (let ((c-t (parse-proof-point-tactic-suffix reg start start)))
    (if (car c-t)
      (error "parse-proof-point: whole tactic found")
      (cdr c-t))))

(defun parse-proof-point (reg)
  (let ((goal-i (parse-proof-point-goal reg 0)))
    (let ((goal (concat "g" (car goal-i) ";;\n")))
      (let ((i (parse-proof-point-string reg "," (cadr goal-i))))
        (let ((tactics (parse-proof-point-tactic reg i)))
          (apply #'concat (cons goal tactics)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interacting with the HOL Light process
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun shell-recenter ()
  (interactive ())
  (shell-recenter-raw))

(defun shell-send-region ()
  (interactive ())
  (let ((reg (string-chomp-end ";;"
                (region-string-raw))))
    (let ((cmd (concat reg ";;\n")))
      (shell-send-string-raw cmd))))

(defun shell-send-paragraph ()
  (interactive ())
  (shell-send-string-raw (concat (get-paragraph) "\n")))

(defun shell-send-word ()
  (interactive ())
  (shell-send-string-raw (concat (get-word) ";;\n")))

(defun shell-set-goal ()
  (interactive ())
  (let ((reg (string-chomp-start "("
               (string-chomp-end ","
                 (region-string-raw)))))
    (let ((cmd (concat "g" reg ";;\n")))
      (shell-send-string-raw cmd))))

(defun shell-expand-tactic ()
  (interactive ())
  (let ((reg (string-chomp-start "["
               (string-chomp-end "THEN"
                 (string-chomp-end "THENL"
                   (string-chomp-end ";"
                     (string-chomp-end ");;"
                       (region-string-raw))))))))
    (let ((cmd (concat "e(" reg ");;\n")))
      (shell-send-string-raw cmd))))

(defun shell-undo-tactic ()
  (interactive ())
  (shell-send-string-raw "b();;\n"))

(defun shell-goto-proof-point ()
  (interactive ())
  (let ((reg (save-excursion
               (let ((end (point)))
                 (backward-paragraph)
                 (string-trim (buffer-substring (point) end))))))
    (let ((cmd (parse-proof-point reg)))
      (shell-send-string-raw cmd))))
