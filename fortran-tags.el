;; Copyright (C) 2015 Raul Laasner
;; This file is distributed under the terms of the GNU General Public
;; License, see 'LICENSE' in the root directory of the present
;; distribution or http://gnu.org/copyleft/gpl.txt .

;; Quick explanation:
;;  fortran-read-tags    works like visit-tags-table
;;  fortran-find-tag     works like find-tag
;;  fortran-pop-tag-mark works like pop-tag-mark
;;  fortran-goto-next    works like find-tag for multiple matches
;;  fortran-find-proc-calls finds all the places where the procedure
;;                          has been used
;; See README for detailed information.

;; Global variables:
;;   fortran-buffers:
;;     Each invocation of goto-new-position pushes the current buffer
;;     into fortran-buffers from where it is popped by
;;     fortran-pop-tag-mark.
;;   fortran-positions:
;;     Similar to fortran-buffers except contains the saved positions.
;;   alt-positions:
;;     Holds the positions of alternative definitions if any were
;;     found by fortran-find-tag or contains the matches found by
;;     fortran-find-proc-calls. This is a list where each item is a
;;     3-item list (filepath, line_nr, position).
;;   alt-positions-counter:
;;     Remembers how many times alt-positions has been accessed. Each
;;     call to fortran-goto-next moves on to the next element of
;;     alt-positions.
;;   fortran-tags-path:
;;     Absolute path to the tags file.
;;   fortran-tags-version-ok:
;;     True if the current version and the version found on the first
;;     line of the tags file agree.
;;   cur-scope:
;;     The scope at the position of the cursor as determined by
;;     fortran-find-scope.

(setq VERSION "1.2.1")

(defun fortran-read-tags ()
  "Interactively return the path of the fortran tags file."
  (interactive)
  (read-file-name "Read tags file (default FORTAGS): " default-directory
		  (expand-file-name "FORTAGS" default-directory) t))

(defun check-fortran-tags-version ()
  "Return true if the first line of the tags file matches the
current version of Fortran-tags."
  (string= (concat VERSION "\n")
	   (shell-command-to-string
	    (concat "head -n 1 " fortran-tags-path))))

(defun fortran-word-at-point (&optional lowercase)
  "Return the Fortran word at point (in lowercase if requested)."
  (let (p1 p2)
    (save-excursion
      (progn
	(skip-chars-backward "_A-Za-z0-9")
	(setq p1 (point))
	(skip-chars-forward "_A-Za-z0-9")
	(setq p2 (point))))
    (if (= p1 p2)
	(read-string "Enter name: ")
      (if lowercase
	  (downcase (buffer-substring-no-properties p1 p2))
	(buffer-substring-no-properties p1 p2)))))
  
(defun goto-new-position (file line char)
  "Go to the new position determined by FILE LINE CHAR and save
the current buffer and position."
  (if (not (boundp 'fortran-buffers)) (setq fortran-buffers (list)))
  (if (not (boundp 'fortran-positions)) (setq fortran-positions (list)))
  (push (current-buffer) fortran-buffers)
  (push (point) fortran-positions)
  (find-file file)
  (goto-line (string-to-number line))
  (forward-char (string-to-number char)))

(defconst f90-scope-keyword-re
  (regexp-opt '("program" "module" "subroutine" "function" "submodule" "associate" "block") 'paren)
  "Regexp used to locate the start/end of a scope.")

(defun f90-beginning-of-scope ()
  "Move point to the beginning of the current scope.
Return (TYPE NAME), or nil if not found."
  (let ((count 1) (case-fold-search t) matching-beg)
    (while (and (> count 0)
                (re-search-backward f90-scope-keyword-re nil 'move))
      (beginning-of-line)
      (skip-chars-forward " \t0-9")
      (cond ((setq matching-beg (f90-looking-at-program-block-start))
             (setq count (1- count)))
	    ((f90-looking-at-associate)
	     (setq matching-beg (list "end" "fortags_associate_construct"))
	     (setq count (1- count)))
	    ((f90-looking-at-critical) ; Critical or block
	     (setq matching-beg (list "end" "fortags_block_construct"))
	     (setq count (1- count)))
            ((f90-looking-at-program-block-end)
             (setq count (1+ count)))))
    (beginning-of-line)
    (if (zerop count) matching-beg)))

(defun fortran-find-scope ()
  "Set 'cur-scope' to the current scope."
  (setq cur-scope ":")
  (let ((tmp ""))
    (save-excursion
      (while tmp
	(setq tmp (f90-beginning-of-scope))
	(if tmp
	    (setq cur-scope (concat ":" (downcase (nth 1 tmp)) cur-scope))))))
  (if (or (not (save-excursion (nth 1 (f90-beginning-of-scope))))
	  (string= "" (shell-command-to-string
		       (concat "LC_ALL=C fgrep -m 1 \" " cur-scope " \" "
			       fortran-tags-path))))
      ;; i) If (nth 1 (f90-beginning-of-scope)) returns zero, we have
      ;; zero scope. In this case it is necessary to check whether we
      ;; are inside the program construct where the beginning
      ;; 'program' keyword is missing; ii) If the scope is not found
      ;; in the tags file, then it was incorrectly found. If so,
      ;; perform a more advanced search using fortran-tags.py -s.
      (let ((tmp-buffer "fortran-tags-tmp-buffer"))
	(generate-new-buffer tmp-buffer)
	(if (save-excursion (nth 1 (f90-beginning-of-scope)))
	    ;; In case of ii) above, process the input file up to the
	    ;; current point. The objective is to determine the
	    ;; current scope.
	    (call-process-region (point-min) (point)
				 "fortran-tags.py" nil tmp-buffer nil "-s")
	  ;; In case of i), process the input file from the current
	  ;; point onwards. The objective is to find out whether there
	  ;; is an 'end program' somewhere. If so, cur-scope will be
	  ;; returned as '\n' (fortran-tags.py starts with ':' and
	  ;; only returns '\n' is there is an excess 'end' somewhere).
	  (call-process-region (point) (point-max)
			       "fortran-tags.py" nil tmp-buffer nil "-s"))
	(with-current-buffer tmp-buffer
	  (setq cur-scope (buffer-string)))
	(kill-buffer tmp-buffer))))
  
(defun fortran-find-tag (&optional force-global)
  "Find the definition of the word under the cursor. If found,
move to the new position. If force-global is true, the search is
performed by only scanning through variables with the module
scope."
  (interactive)
  ;; Find the tags file
  (if (and (boundp 'fortran-tags-path)
	   (not (file-exists-p fortran-tags-path)))
      (makunbound 'fortran-tags-path)) ; FORTAGS not present at the old location
  (if (not (boundp 'fortran-tags-path))
      (setq fortran-tags-path (fortran-read-tags)))
  (if (not (boundp 'fortran-tags-version-ok))
      (setq fortran-tags-version-ok (check-fortran-tags-version)))
  (if (and (not fortran-tags-version-ok)
	   (not (check-fortran-tags-version))) ; Recheck if changed
      (let ((tags-path fortran-tags-path))
	(makunbound 'fortran-tags-path)
	(if (not (file-exists-p tags-path))
	    (error (concat tags-path " does not exist."))
	  (error "Incorrect format (regenerate using the current version of Fortran-tags)."))))
  ;; Find the definition
  (let ((WORD (fortran-word-at-point t)) scope scopes match i j)
    (setq alt-positions (list))
    (fortran-find-scope)
    (if (string= cur-scope "\n")
	;; We are a program construct that didn't start with the
	;; 'program' keyword. Set cur-scope to :fortags_program_scope:
	;; manually.
	(setq cur-scope ":fortags_program_scope:"))
    (setq scopes (split-string cur-scope ":"))
    (catch 'found
      (if (not force-global)
	  (progn
	    (setq i (1- (length scopes)))
	    ;; If the current scope is :a:b:c:, but contains no match,
	    ;; the search is expanded to the scopes :a:b:, :a:, and :
	    ;; if necessary.
	    (while (> i 0)
	      (setq scope ":")
	      (setq j (1- i))
	      (while (> j 0)
		;; This is where the original scope is widened,
		;; e.g. :a:b:c: becomes :a:b:, which can become :a:
		;; and so on.
		(setq scope (concat ":" (nth j scopes) scope))
		(setq j (1- j)))
	      (setq match (shell-command-to-string
			   (concat "LC_ALL=C fgrep -m 1 \" "
				   WORD " " scope " \" " fortran-tags-path)))
	      (if (not (string= "" match))
		  (let ((words (split-string match)))
		    (goto-new-position
		     (nth 0 words) (nth 4 words) (nth 5 words))
		    (throw 'found nil)))
	      (setq i (1- i)))))
      ;; If no match was found, go through all the definitions with the
      ;; module scope.
      (setq match (split-string
		   (shell-command-to-string
		    (concat "LC_ALL=C fgrep \" 0 " WORD
				       " :\" " fortran-tags-path)) "\n"))
      (if (not (string= "" (nth 0 match)))
	  (progn
	    (dolist (line (delete "" match))
	      (let ((words (split-string line)))
		(setq alt-positions
		      (append alt-positions (list (list
						   (nth 0 words)
						   (nth 4 words)
						   (nth 5 words)))))))
	    (setq alt-positions-counter 0)
	    (fortran-goto-next)
	    (message (concat (number-to-string
			      (length alt-positions)) " found"))
	    (throw 'found nil)))
      (message "Definition not found"))))

(defun fortran-pop-tag-mark ()
  "Return to the previous location."
  (interactive)
  (if fortran-buffers (progn
			(switch-to-buffer (pop fortran-buffers))
			(goto-char (pop fortran-positions)))))

(defun fortran-goto-next ()
  "If alt-positions is not empty, go to the next position,
else force fortran-find-tag to search only through module level
variables."
  (interactive)
  (if (not (boundp 'alt-positions)) (setq alt-positions (list)))
  (if (not alt-positions) (fortran-find-tag t)
    (let ((x (nth alt-positions-counter alt-positions)))
      (goto-new-position (nth 0 x) (nth 1 x) (nth 2 x))
      (setq alt-positions-counter (1+ alt-positions-counter))
      (if (= alt-positions-counter (length alt-positions))
	  (setq alt-positions-counter 0)))))

(defun fortran-find-proc-calls (&optional fast-search)
  "Find all calls to the procedure under the cursor. If found,
alt-positions is populated with the corresponding positions that
can be cycled through with fortran-goto-next. Default is to use a
general regex, while fast-search determines a specialized regex
for subroutines or functions."
  (interactive)
  (if (not (boundp 'fortran-tags-path))
      (setq fortran-tags-path (fortran-read-tags)))
  (let ((WORD (fortran-word-at-point))
	(match "")
	(src-file-paths
	 (shell-command-to-string
	  (concat
	   "head -n 2 " fortran-tags-path " | tail -n 1 | xargs echo -n"))))
    (if (not (string= "" WORD))
	(cond
	 ((string= "subroutine" fast-search)
	  (setq match
		(shell-command-to-string
		 (concat "LC_ALL=C egrep -Hn \"^ *call " WORD
			 " *([(&]|$)\" " src-file-paths
			 " | cut -f1,2 -d:"))))
	 ((string= "function" fast-search)
	  (setq match
		 (shell-command-to-string
		  (concat "LC_ALL=C egrep -Hn \"[=+/*(&\-] *" WORD
			  " *[(&]\" " src-file-paths " | cut -f1,2 -d:"))))
	 ((string= "type-bound" fast-search)
	  (setq match
		 (shell-command-to-string
		  (concat "LC_ALL=C egrep -Hn \"%" WORD
			  " *[(&]\" " src-file-paths " | cut -f1,2 -d:"))))
	 (t
	  (setq match
		(concat
		 (shell-command-to-string
		  (concat "LC_ALL=C egrep -Hni \"(^|[;&]) *call +" WORD
			  " *([(&;\!]|$)\" " src-file-paths " | cut -f1,2 -d:"))
		 (shell-command-to-string
		  (concat "LC_ALL=C egrep -Hni \"([=+/*(%%&\-]|^) *" WORD
			  " *[(&]\" " src-file-paths " | cut -f1,2 -d:")))))))
    (if (not (string= "" match))
	(let ((matches (delete "" (split-string match "\n"))) (files "")
	      (num-files ""))
	  (setq alt-positions (list))
	  (setq alt-positions-counter 0)
	  (setq num-files (number-to-string (length matches)))
	  (dolist (line matches)
	    (let ((pieces (split-string line ":")))
	      (setq alt-positions
		    (append alt-positions
			    (list (list (nth 0 pieces) (nth 1 pieces) "0"))))))
	  (let ((i 0) tmp)
	    (while (< i (length matches))
	      (setq tmp (nth 0 (split-string (nth i matches) ":")))
	      (setcar (nthcdr i matches) (concat (file-name-base tmp) "."
						 (file-name-extension tmp)))
	      (setq i (1+ i))))
	  (setq matches (delete-dups matches))
	  (dolist (x matches)
	    (if (not (string= "" files))
		(setq files (concat files ", " x))
	      (setq files x))
	    (message x))
	  (message (concat num-files " found (" files ")")))
      (message "Not found"))))

(defun fortran-find-proc-calls-sub()
  "Search only for subroutine calls."
  (interactive)
  (fortran-find-proc-calls "subroutine"))

(defun fortran-find-proc-calls-func()
  "Search only for function calls (excluding type-bound
procedures)."
  (interactive)
  (fortran-find-proc-calls "function"))

(defun fortran-find-proc-calls-type()
  "Search only for calls to a type-bound procedure."
  (interactive)
  (fortran-find-proc-calls "type-bound"))

(define-minor-mode fortran-tags-mode
  "Minor mode providing functions for Fortran source code indexing."
  :lighter " FT"
  :keymap (let ((map (make-sparse-keymap)))
	    (define-key map (kbd "M-.") 'fortran-find-tag)
	    (define-key map (kbd "M-*") 'fortran-pop-tag-mark)
	    (define-key map (kbd "M-n") 'fortran-goto-next)
	    (define-key map (kbd "M-s g") 'fortran-find-proc-calls)
	    (define-key map (kbd "M-s s") 'fortran-find-proc-calls-sub)
	    (define-key map (kbd "M-s f") 'fortran-find-proc-calls-func)
	    (define-key map (kbd "M-s t") 'fortran-find-proc-calls-type)
	    map))

(add-hook 'f90-mode-hook 'fortran-tags-mode)

(provide 'fortran-tags)
