;;; pygen.el --- Python code generation using Elpy and Python-mode.

;; Copyright (C) 2016 Jack Crawley

;; Author: Jack Crawley <http://www.github.com/jackcrawley>
;; Keywords: python, code generation
;; Version: 0.1
;; Package-Requires: ((elpy "1.12.0") (python-mode "))
;; URL: 
;; TODO: Input URL

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; Pygen is a package that allows the user to automatically generate
;; Python code.  This allows programmers to code first, and worry
;; about the functions they are calling later.

;; A description is available here, but the readme has more
;; information. You can view it at
;; http://www.github.com/jackcrawley/pygen


;; It provides the following code generation commands.  For animated
;; examples of each command in action, see the GitHub page.

;; Generating Classes & Functions
;; ------------------------------

;; `pygen-generate-class' - Generate a python class from the reference
;; under point.

;; `pygen-generate-function' - Generate a python function from the
;; reference under point.

;; `pygen-generate-static-function' - Generate a static python
;; function from the reference under point.

;; Generating Variables
;; --------------------

;; `pygen-extract-variable' - Extract the current region into a
;; variable.

;; `pygen-make-keyword-argument' - Adds a keyword argument to the
;; current function.

;; `pygen-make-sequence-argument' - Adds a sequence argument to the
;; current function.

;; Automatic Decorators
;; --------------------

;; `pygen-add-decorator-to-function' - Adds a decorator in front of
;; the current function.

;; `pygen-make-static' - Turns the current function into a static
;; function.  WARNING: Not currently functional.

;; Modifying the "self" keyword:
;; -----------------------------

;; `pygen-selfify-symbol' - Puts the word 'self.' in front of the
;; current symbol.

;; `pygen-toggle-selfify-symbol' - Toggles the word 'self' in front of
;; the current symbol.

;; `pygen-unselfify-symbol' - Removes the word 'self.' from the
;; current symbol (if it exists).

;; Dynamic Boilerplate Code Generation
;; -----------------------------------

;; `pygen-insert-super' - Inserts a proper call to the current method
;; in the superclass.


;; Pygen leverages `elpy' and `python-mode'.  That's the package
;; called `python-mode', not just the mode.  As of Emacs 24.5.1,
;; `python-mode' is not the default Python mode but a separate
;; package.  The default package is called `python'.

;; Pygen won't work with a Python setup unless `python-mode' is
;; installed.  However, it can work with a setup that doesn't include
;; Elpy.  You just need to tell it how to navigate to function and
;; class definitions.  By default, this is handled by the command
;; `elpy-goto-definition' to navigate to definitions and the command
;; `pop-tag-mark'.  If you use a different system to navigate through
;; python code, you can set the variables
;; `pygen-navigate-to-definition-command' and `pygen-go-back-command'
;; to different functions.

;;; Code:


(require 'elpy)
(require 'python-mode)


(defgroup pygen nil
  "Tools for automatically generating Python code."
  :group 'python-mode
  :prefix "pygen-")


(defconst pygen-version "0.1")


(defcustom pygen-mode-hook nil
  "Hook run when `pygen-mode' is enabled."
  :type 'hook
  :group 'pygen)


(defcustom pygen-navigate-to-definition-command 'elpy-goto-definition
  "Command to use to navigate to function and class definitions."
  :type 'symbol
  :group 'pygen)


;; Commented out. Not a necessary command at the moment.
;; (defcustom pygen-go-back-command 'pop-tag-mark
;;   "Command to navigate back to previous positions.

;; After `pygen-navigate-to-definition-command' is called, this
;; command is called to navigate back to the previous position."
;;   :type 'symbol
;;   :group 'pygen)


(defun pygen-verify-environment ()
  "Verify that pygen has the required environments installed."
  ;; TODO: Verify all python-mode functions are available.
  ;; `python-mode' does not need to be active, the functions just need
  ;; to be usable.
  (when (eq pygen-navigate-to-definition-command 'elpy-goto-definition)
	;; TODO: Verify Elpy is installed
	)
  )


(defun pygen-verify-expression (&optional bounds)
  "Verify the point is on an expression."
  ;; Get parameters not provided
  (when (not bounds)
	(setq bounds (pygen-get-expression-bounds)))
  ;; TODO: verify the current expression
  t)


(defun pygen-get-expression-bounds ()
  "Get the bounds of the function under point."
  ;; TODO: Expand this function so it can select any expression style.
  (save-excursion
	(when (looking-back "[^A-Za-z0-9._*]")
		(right-char))
	(py-partial-expression)
	(cons (region-beginning) (region-end))))


(defun pygen-has-parent (&optional bounds verified)
  "Check whether this expression has a parent."
  ;; Get input parameters if not provided
  (when (not bounds)
	(setq bounds (pygen-get-expression-bounds)))
  (when (not verified)
	(setq verified (pygen-verify-expression)))
  ;; Now check whether this expression has a parent.
  (save-excursion
	(goto-char (car bounds))
	(and
	 ;; There is enough room for a parent
	 (>= (point) 2)
	 ;; The preceding 2 characters match the regular expression for a parent
	 (string-match "[A-Za-z0-9_]\\." (buffer-substring (- (point) 2) (point))))))


(defun pygen-goto-expression-parent (&optional bounds verified)
  "Go to the parent of this expression."
  ;; Get input parameters if not provided
  (when (not bounds)
	(setq bounds (pygen-get-expression-bounds)))
  (when (not verified)
	(setq verified (pygen-verify-expression)))
  ;; Now go to the parent of this expression
	(let ((left-bound (car bounds)))
	  (goto-char left-bound)
	  ;; If preceding character is a dot
	  (if (pygen-has-parent)
		  (progn
			;; Navigate to parent
			;; TODO: Move back to function after this
			(left-char 2)
			(let (error-variable)
			  (condition-case error-variable
				  (funcall pygen-navigate-to-definition-command)
				(error
				 (message "Pygen could not navigate to the parent of this expression.")
				 (if (not (string-match "No definition found" (error-message-string error-variable)))
					 (progn
					   (message (concat "Miscellaneous error. Perhaps try "
										"calling the same command again. "
										"Original error below."))
					   error-variable)
				   (error "The parent of this expression could not be found."))))))
		;; Throw an error if we arent looking at something with a parent
		(error "Expression does not have a parent. Cannot navigate to parent expression."))))


(defun pygen-get-expression-name (&optional bounds verified)
  "Get the name of the function within `BOUNDS'."
  ;; Get input parameters if not provided
  (when (not bounds)
	(setq bounds (pygen-get-expression-bounds)))
  (when (not verified)
	(setq verified (pygen-verify-expression)))
  ;; Now get the expression name
  (let ((full-expression (buffer-substring-no-properties (car bounds) (cdr bounds))))
	(with-temp-buffer
	  (insert full-expression)
	  (goto-char 1)
	  ;; If the expression has arguments (within the partial expression)
	  (if (re-search-forward "[^A-Za-z0-9_]" nil t)
		  (buffer-substring-no-properties 1 (1- (point)))
		full-expression))))


(defun pygen-get-expression-arguments-string (&optional bounds verified)
  "Get the arguments for the current expression as a string.

Does not include parentheses.  Only gets the argument inside the
parentheses."
  ;; Get input parameters if not provided
  (when (not bounds)
	(setq bounds (pygen-get-expression-bounds)))
  (when (not verified)
	(setq verified (pygen-verify-expression)))
  ;; Now get the expression arguments as a string
  (let ((full-expression (buffer-substring-no-properties (car bounds) (cdr bounds))))
	(with-temp-buffer
	  (insert full-expression)
	  (goto-char 1)
	  ;; If the expression has arguments (within the partial expression)
	  (if (re-search-forward "(" nil t)
		  (buffer-substring-no-properties (point)
										  (1- (point-max)))
		nil))))


(defun pygen-parse-single-argument (argument)
  "Parse a single `ARGUMENT' from raw form into a meaningful keyword.

If no keyword is defined, a dummy argument will be returned
instead of the form `\"_\"'.

If the `argument' is malformed, this function will do its best to
extract some kind of meaningful argument."
  (let (keyword
		keyword-start)
	(with-temp-buffer
	  (insert " ")
	  (insert argument)
	  (insert " ")
	  (goto-char 1)
	  (if (re-search-forward "[A-Za-z0-9_*]" nil t)
		  (progn
			(setq keyword-start (1- (point)))
			(re-search-forward "[^A-Za-z0-9_*]" nil t)
			(setq keyword
				  (buffer-substring-no-properties keyword-start (1- (point))))
			(when (string= keyword "")
			  (setq keyword "_"))
			keyword)
		nil))))


(defun pygen-get-expression-arguments (&optional bounds verified)
  "Get the list of arguments for the current expression."
  ;; Get input parameters if not provided
  (when (not bounds)
	(setq bounds (pygen-get-expression-bounds)))
  (when (not verified)
	(setq verified (pygen-verify-expression)))
  ;; Now get expression arguments
  (save-excursion
	(let ((arguments-string (pygen-get-expression-arguments-string bounds verified))
		  (arguments '()))
	  (if arguments-string
		  (with-temp-buffer
			(insert arguments-string)
			(goto-char 1)
			;; Clean up nested s-expressions (dicts, strings, nested
			;; function calls, etc.). Each nested s-exp should represent
			;; 1 argument and have its own commas removed.
			(save-excursion
			  (while (re-search-forward "[([{\"]" nil t)
				(left-char 1)
				(let ((current-start (point)))
				  (forward-sexp)
				  (delete-region current-start (point))
				  (insert ""))))
			;; Now repeatedly try to find arguments.
			;; First search for comma separated arguments.
			(let ((previous-position (point)))
			  (while (re-search-forward "[^ \n\t\\\\].*," nil t)
				(let (current-argument)
				  (setq current-argument (buffer-substring-no-properties previous-position (1- (point))))
				  (setq current-argument (pygen-parse-single-argument current-argument))
				  (when current-argument
					(push current-argument arguments)
					(setq previous-position (point))))))
			;; Now search the last, non-comma separated argument.
			(let* ((unparsed-argument (buffer-substring-no-properties (point) (point-max)))
				   (last-argument (pygen-parse-single-argument unparsed-argument)))
			  (when last-argument
				(push last-argument arguments)))
			(when arguments
			  (reverse arguments)))
		nil))))


(defun pygen-chomp-whitespace (str)
  "Chomp leading and tailing whitespace from STR."
  (replace-regexp-in-string (rx (or (: bos (* (any " \t\n")))
									(: (* (any " \t\n")) eos)))
							""
							str))


(defun pygen-extract-function-arguments-from-string (arguments-string)
  "Extract a list of arguments from the arguments section of a
function definition.

WARNING: Does not work with star arguments."
  (let ((arguments '())
		(individual-arguments-strings '()))
	(with-temp-buffer
	  (insert arguments-string)
	  (message "")
	  (goto-char 1)
	  (let ((last-position (point)))
		(while (search-forward "," nil t)
		  (when (not (in-string-p))
			(push (buffer-substring-no-properties last-position (- (point) 1)) individual-arguments-strings)
			(setq last-position (point))))
		(when (re-search-forward "[A-Za-z0-9_*]" nil t)
		  (setq last-position (- (point) 1))
		  (push (buffer-substring-no-properties last-position (point-max)) individual-arguments-strings))))
	(while individual-arguments-strings
	  (with-temp-buffer
		(let (start-position
			  end-position)
		  (insert " ")
		  (insert (pop individual-arguments-strings))
		  (insert " ")
		  (goto-char 1)
		  ;; First get argument, then get default value.
		  (let (argument)
			(if (re-search-forward "[A-Za-z0-9_*]" nil t)
				(progn
				  (left-char)
				  (setq start-position (point))
				  (if (re-search-forward "[^A-Za-z0-9_*]" nil t)
					  (setq argument
							(buffer-substring-no-properties start-position (- (point) 1)))
					(error "Error: malformed function definition. Comma was found with no argument before it.")))
			  (error "Error: malformed function definition. Comma was found with no argument before it."))
			(left-char)
			(let ((value (if (search-forward "=" nil t)
							 (progn
							   (pygen-chomp-whitespace (buffer-substring-no-properties (point) (point-max))))
						   nil)))
			  (push (cons argument value) arguments))))))
	arguments))


(defun pygen-get-def-arguments (&optional in-function)
  "Get a list of arguments in the current defun.
Returns a list of (`argument' . `default-value') pairs.
Arguments with no default value have a default value of nil.

`IN-FUNCTION' specifies whether a check has already been
performed to see whether the point is in a function.  This check
can take time, so it's optimal to only do it once."
  (save-excursion
	(let (arguments-string)
	  (when (not in-function)
		(when (not (pygen-def-at-point))
		  (error "Error: not currently in a def.")))
	  (py-beginning-of-def-or-class)
	  (search-forward "(")
	  (left-char)
	  (let ((start-position (1+ (point))))
		;; TODO: Replace this with a more durable navigation function
		;; `forward-sexp' isn't going to work if the definition of an
		;; s-expression changes.
		(forward-sexp)
		(let* ((end-position (1- (point)))
			   (arguments-string
				(buffer-substring-no-properties start-position end-position)))
		  (pygen-extract-function-arguments-from-string arguments-string))))))


(defun pygen-def-at-point ()
  "Check whether the point is currently in a def."
  (let ((start-point (point))
		(region-was-active (region-active-p))
		error-marking-def)
	(condition-case nil
		(py-mark-def)
	  (wrong-type-argument (goto-char start-point)
			 (setq error-marking-def t)))
	(if (and (<= (region-beginning) start-point)
			 (>= (region-end) start-point)
			 (not (= start-point (mark)))
			 (not error-marking-def))
		(progn
		  (pop-mark)
		  (goto-char start-point)
		  (when region-was-active
			(activate-mark))
		  t)
	  (pop-mark)
	  (goto-char start-point)
	  (when region-was-active
		(activate-mark))
	  nil)))


(defun pygen-class-at-point ()
  "Check whether the point is currently in a class."
  (let ((start-point (point))
		(region-was-active (region-active-p))
		error-marking-class)
	(condition-case nil
		(py-mark-class)
	  (wrong-type-argument (goto-char start-point)
						   (setq error-marking-class t)))
	(if (and (<= (region-beginning) start-point)
			 (>= (region-end) start-point)
			 (not (= start-point (mark)))
			 (not error-marking-class))
		(progn
		  (pop-mark)
		  (goto-char start-point)
		  (when region-was-active
			(activate-mark))
		  t)
	  (pop-mark)
	  (goto-char start-point)
	  (when region-was-active
		(activate-mark))
	  nil)))


(defun pygen-argument-already-exists (argument &optional in-function)
  "Whether an `ARGUMENT' already exists in the current function.

Returns t if the argument already exists in the current
function's definition.

`IN-FUNCTION' specifies whether a check has already been
performed to see whether the point is in a function.  This check
can take time, so it's optimal to only do it once.

`ARGUMENT' is the argument to check for."
  (when (not in-function)
	(setq in-function (pygen-def-at-point)))
  (let ((argument-exists nil)
		(arguments-list (pygen-get-def-arguments)))
	(mapc (lambda (existing-argument)
			 (when (string= (downcase argument) (downcase (car existing-argument)))
			   (setq argument-exists t)))
		  arguments-list)
	argument-exists))


(defun pygen-add-keyword-argument-internal (argument)
  "Add a keyword argument to the current function's definition."
  (when (not (pygen-def-at-point))
	(error "Error: not currently in a def."))
  (when (pygen-argument-already-exists argument t)
	(error "Error: the argument `%s' already already exists in the function definition."
		   argument))
  
  (py-beginning-of-def-or-class)
  (search-forward "(")
  (let ((start-position (point))
		end-position)
	(left-char)
	(forward-sexp)
	(setq end-position (- (point) 1))
	;; "self" is a special keyword that should always be inserted at
	;; the start of the definition.
	(if (string= (downcase argument) "self")
		(progn
		  (goto-char start-position)
		  (if (re-search-forward "[A-Za-z0-9_*]" end-position t)
			  (progn
				(left-char)
				(insert "self, "))
			(insert "self")))
	  ;; Note that in Python, a star indicates a termination of the
	  ;; positional argument rule. We can use this.
	  
	  ;; Check if any star arguments exist
	  (goto-char end-position)
	  (let ((star-args-present nil)
			first-star-argument) 
		(while (search-backward "*" start-position t)
		  (when (not (in-string-p))
			(setq star-args-present t)
			(setq first-star-argument (point))))
		;; If another argument already exists, argument must be inserted
		;; with a comma. Otherwise, just insert it.
		(if star-args-present
			(progn
			  (goto-char first-star-argument)
			  (if (search-backward "," start-position t)
				  (progn
					(right-char)
					(insert (concat " " argument "=,"))
					(left-char))
				(insert (concat argument "=, "))
				(left-char 2)))
		  (goto-char end-position)
		  (if (re-search-backward "[^ \t\n\\\\]" start-position t)
			  (progn
				(right-char)
				(insert (concat ", " argument "=")))
			(insert argument "=")))))))


(defun pygen-add-sequence-argument-internal (argument)
  "Add a sequence argument to the current functions definition." 
  (when (not (pygen-def-at-point))
	(error "Error: not currently in a def."))
  (when (pygen-argument-already-exists argument)
	(error "Error: the argument `%s' already already exists in the function definition."
		   argument))
  (py-beginning-of-def-or-class)
  (search-forward "(")
  (let ((start-position (point))
		end-position)
	(left-char)
	(forward-sexp)
	(setq end-position (- (point) 1))
	
	;; "self" is a special keyword that should always be inserted at
	;; the start of the definition.
	(if (string= (downcase argument) "self")
		(progn
		  (goto-char start-position)
		  (if (re-search-forward "[A-Za-z0-9_*]" end-position t)
			  (progn
				(left-char)
				(insert "self, "))
			(insert "self")))  
	  (goto-char start-position)
	  ;; If keyword arguments exist, place after sequence arguments, but
	  ;; before the first keyword argument.
	  (if (search-forward "=" end-position t)
		  (progn
			;; Keyword argument exists, so:
			;; We will be just after the first equals sign, so search
			;; backwards for the keyword being assigned to with that
			;; equals sign. Then, jump to the beginning of that keyword.
			(if (and (re-search-backward "[A-Za-z0-9_*]" start-position t)
					 (re-search-backward "[^A-Za-z0-9_*]" (- start-position 1) t))
				(progn
				  (right-char)
				  (insert (concat argument ", ")))
			  ;; If these searches yield strange results, it means the
			  ;; function is malformed. Inform the user, but try to
			  ;; insert the argument anyway.
			  (message "Warning: malformed function. Attempting to add argument as best can be done.")
			  (insert argument)))
		;; Keyword arguments don't exist, so check for star arguments
		;; next.
		(goto-char end-position)
		(let ((star-args-present nil)
			  first-star-argument)
		  (while (search-backward "*" start-position t)
			(when (not (in-string-p))
			  (setq star-args-present t)
			  (setq first-star-argument (point))))
		  (if star-args-present
			  (progn
				(goto-char first-star-argument)
				(insert (concat argument ", ")))
			(goto-char start-position)
			;; If other arguments exist, a comma must be added.
			(if (re-search-forward "[A-Za-z0-9_*]" end-position t)
				(progn
				  ;; TODO: If star args, put before them.
				  (goto-char end-position)
				  (insert (concat ", " argument)))
			  (insert argument))))))))


(defun pygen-expression-exists (&optional bounds verified)
  "Check if the current expression already exists."
  ;; Get input parameters if not provided
  (when (not bounds)
	(setq bounds (pygen-get-expression-bounds)))
  (when (not verified)
	(setq verified (pygen-verify-expression)))
  ;; Now check if the function exists
  ;; TODO: Optimise this. Find a faster method of checking if it exists.
  ;;       Perhaps if flycheck is open, use that?
  (save-excursion
	(let ((expression-exists nil))
	  (goto-char (car bounds))
	  (let ((start-position (point))
			(start-buffer (current-buffer)))
		(condition-case error-variable
			(progn 
			  (funcall pygen-navigate-to-definition-command)
			  (setq expression-exists t)
			  (condition-case nil
				  (tags-loop-continue)
				(error
				 (message "Warning: could not return to previous position. Manually reverting.")
				 (switch-to-buffer start-buffer)
				 (goto-char start-position))))
		  (error
		   (message "Pygen could not navigate to the parent of this expression.")
		   (if (not (string-match "No definition found" (error-message-string error-variable)))
			   (progn
				 (message (concat "Miscellaneous error. Perhaps try "
								  "calling the same command again. "
								  "Original error:"))
				 error-variable)
			 nil))))
	  expression-exists)))


(defun pygen-is-parent-self (&optional bounds verified has-parent)
  "Check if the immediate parent of this expression is 'self'."
  ;; Get input parameters if not provided
  (when (not bounds)
	(setq bounds (pygen-get-expression-bounds)))
  (when (not verified)
	(setq verified (pygen-verify-expression)))
  (when (not has-parent)
	(setq has-parent (pygen-has-parent)))
  ;; Now check if the parent is self
  (save-excursion
	(if has-parent
		(progn
		  (goto-char (car bounds))
		  (left-char)
		  (let ((end-position (point)))
			(py-backward-partial-expression)
			(if (string= (buffer-substring-no-properties (point) end-position)
						 "self")
				t
			  nil)))
	  nil)))


(defun pygen-create-new-function-in-module (arguments &optional function-name decorators)
  "Create a new function in the current module.

Creates the function at the top level of the module, immediately
after all imports.

`decorators' is an optional list of decorators to place before
  the function, as strings."
  ;; First get the cursor in position, then insert the function.
  
  ;; If def exists, place before def
  ;; If class exists, place before class
  ;; Otherwise, navigate after imports and place in front of the first statement.
  (goto-char (point-min))
  
  (let* (;; TODO: Replace this a method for finding the first class with
		 ;; supplying an indent parameter to the `py-forward-class' method.
		 (first-class-position (save-excursion
								 (py-down-class)
								 (py-up-class)
								 (if (<= (point) (point-min))
									 nil
								   (point))))
		 (first-def-position (save-excursion
							   (py-down-def)
							   (py-up-class)
							   (if (<= (point) (point-min))
								   nil
								 (point))))
		 (after-imports-position
		  (save-excursion
			(while (re-search-forward
					"import *[A-Za-z_][A-Za-z_0-9].*\\|^from +[A-Za-z_][A-Za-z_0-9.]+ +import .*" nil t)
			  ;; HACK: The above import searching regexp is dumb.  It stops
			  ;;       once it enters a bracketed import.  This is a way of
			  ;;       getting it to carry on, however it looks fragile to
			  ;;       me.
			  (when (looking-back "(")
				(search-forward ")")))
			(point)))
		 (first-expression-position (save-excursion
									  (goto-char after-imports-position)
									  (py-forward-expression)
									  (py-beginning-of-expression)
									  (if (<= (point) after-imports-position)
										  nil
										(point)))))
	
	(cond ((and first-class-position first-def-position)
		   ;; If class and definition both exist, move to the earlier
		   ;; of the two.
		   (if (< first-class-position first-def-position)
			   (goto-char first-class-position)
			 (goto-char first-def-position)))
		  (first-class-position
		   (goto-char first-class-position))
		  (first-def-position
		   (goto-char first-def-position))
		  (first-expression-position
		   (goto-char first-expression-position))
		  (t
		   (goto-char (point-max)))))
  ;; Now the cursor is in position, the function can be created.
  (let (start-position
		end-position)
	(when (not function-name)
	  (setq function-name (read-string "Enter function name: ")))
	(beginning-of-line)
	(setq start-position (point))
	(insert "\n\n\n")
	(goto-char start-position)
	(while decorators
	  (insert (pop decorators))
	  (insert "\n"))
	(insert (concat "def " function-name "("))
	(while arguments
	  (let ((argument (pop arguments)))
		(if arguments
			(insert (concat argument ", "))
		  (insert argument))))
	(insert "):")
	(py-newline-and-indent)
	(setq end-position (point))
	;; (insert "pass")
	(goto-char end-position)))


(defun pygen-create-new-class-in-module (arguments &optional class-name)
  "Create a new function in the current module.

Creates the function at the top level of the module, immediately
after all imports."
  ;; First get the cursor in position, then insert the function.
  
  ;; If def exists, place before def
  ;; If class exists, place before class
  ;; Otherwise, navigate after imports and place in front of the first statement.
  (let (first-class-position
		first-def-position
		first-expression-position
		after-imports-position
		insertion-position)
	(goto-char (point-min))
	(save-excursion
	  (while (re-search-forward
			  "import *[A-Za-z_][A-Za-z_0-9].*\\|^from +[A-Za-z_][A-Za-z_0-9.]+ +import .*" nil t)
		;; HACK: The above import searching regexp is dumb.  It stops
		;;       once it enters a bracketed import.  This is a way of
		;;       getting it to carry on, however it looks fragile to
		;;       me.
		(when (looking-back "(")
		  (search-forward ")")))
	  (setq after-imports-position (point)))
	;; TODO: Replace this a method for finding the first class with
	;; supplying an indent parameter to the `py-forward-class' method.
	(save-excursion
	  (py-down-class)
	  (py-up-class)
	  (if (eq (point) (point-min))
		  (setq first-class-position nil)
		(setq first-class-position (point))))
	(save-excursion
	  (py-down-class)
	  (py-up-class)
	  (if (eq (point) (point-min))
		  (setq first-def-position nil)
		(setq first-def-position (point))))
	(save-excursion
	  (goto-char after-imports-position)
	  (py-forward-expression)
	  (py-beginning-of-expression)
	  (if (<= (point) after-imports-position)
		  (setq first-expression-position nil)
		(setq first-expression-position (point))))
	(cond ((and first-class-position first-def-position)
		   ;; If class and definition both exist, move to the earlier
		   ;; of the two.
		   (if (< first-class-position first-def-position)
			   (goto-char first-class-position)
			 (goto-char first-def-position)))
		  (first-class-position
		   (goto-char first-class-position))
		  (first-def-position
		   (goto-char first-def-position))
		  (first-expression-position
		   (goto-char first-expression-position))
		  (t
		   (goto-char (point-max)))))
  ;; Now the cursor is in position, the function can be created.
  (let (start-position
		end-position)
	(when (not class-name)
	  (setq class-name (read-string "Enter class name: ")))
	(beginning-of-line)
	(setq start-position (point))
	(insert "\n\n\n")
	(goto-char start-position)
	(insert (concat "class " class-name "("))
	(push-mark nil t)
	(insert "):")
	(py-newline-and-indent)
	(insert (concat "def __init__(self"))
	(if arguments
		(insert ", "))
	(while arguments
	  (let ((argument (pop arguments)))
		(if arguments
			(insert (concat argument ", "))
		  (insert argument))))
	(insert "):")
	(py-newline-and-indent)
	(setq end-position (point))
	;; (insert "pass")
	(goto-char end-position)))


(defun pygen-create-new-function-in-class (arguments &optional function-name decorators)
  "Create a new function in the current class.

`ARGUMENTS' - the arguments the function should take, as a list
of (NAME . DEFAULT-VALUE) pairs.

`FUNCTION-NAME' - the name of the function.  If this is not
provided, the user will be prompted for a name.

`DECORATORS' - a list of decorator strings to add before the
function.  Leave this as nil if no decorators should be added."
  (let (start-position
		indentation-start
		indentation-end
		indentation-string
		end-position
		(static-function nil))
	(push-mark nil t)
	(setq start-position (point))
	(py-beginning-of-class)
	(setq indentation-end (point))
	(beginning-of-line)
	(setq indentation-start (point))
	(setq indentation-string
		  (buffer-substring-no-properties indentation-start indentation-end))
	(goto-char indentation-end)
	(py-end-of-class)
	(insert "\n\n")

	;; Now insert decorators
	(while decorators
	  (let ((current-decorator (pop decorators)))
		(insert indentation-string)
		(insert current-decorator)
		(py-shift-indent-right)
		(when (string= current-decorator "@staticmethod")
		  (setq static-function t))
		(insert "\n")))
	;; Insert function itself
	(insert indentation-string)
	(when (not function-name)
	  (setq function-name (read-string "Enter function name: ")))
	(insert (concat "def " function-name "("))
	(when (not static-function)
	  (insert "self")
	  (when arguments
		(insert ", ")))
	(while arguments
	  (let ((argument (pop arguments)))
		(if arguments
			(insert (concat argument ", "))
		  (insert argument))))
	(insert "):")
	(py-shift-indent-right)
	(py-newline-and-indent)
	(setq end-position (point))
	;; (insert "pass")
	(goto-char end-position)))


(defun pygen-create-new-class-in-class (arguments &optional class-name)
  "Create a new function in the current class.

`ARGUMENTS' should be a list of argument names.  
`CLASS-NAME' is an optional name.  If it isn't provided, the user
will be prompted for a name."
  (let (start-position
		indentation-start
		indentation-end
		indentation-string
		end-position)
	(push-mark nil t)
	(setq start-position (point))
	(py-beginning-of-class)
	(setq indentation-end (point))
	(beginning-of-line)
	(setq indentation-start (point))
	(setq indentation-string
		  (buffer-substring-no-properties indentation-start indentation-end))
	(goto-char indentation-end)
	(py-end-of-class)
	(insert "\n\n")
	(insert indentation-string)
	(when (not class-name)
	  (setq class-name (read-string "Enter class name: ")))
	(insert (concat "class " class-name "():"))
	(py-shift-class-right)
	(py-newline-and-indent)
	(insert "def __init__(")
	(insert "self")
	(when arguments
	  (insert ", "))
	(while arguments
	  (let ((argument (pop arguments)))
		(if arguments
			(insert (concat argument ", "))
		  (insert argument))))
	(insert "):")
	(py-newline-and-indent)
	(setq end-position (point))
	;; (insert "pass")
	(goto-char end-position)))


(defun pygen-extract-variable-internal (start-position end-position)
  "Extract the code within a given region into a variable."
  (let ((variable-name (read-string "Variable name: "))
		 variable-value
		 new-position
		 statement-start
		 indentation-string)
	 (when (or (string= variable-name "")
			   (not variable-name))
	   (error "Error: variable name cannot be empty."))
	 (setq variable-value
		   (buffer-substring-no-properties start-position end-position))
	 (goto-char end-position)
	 (save-excursion
	   (delete-region start-position end-position)
	   (insert variable-name)
	   (setq new-position (point))
	   (py-beginning-of-statement)
	   (setq statement-start (point))
	   (beginning-of-line)
	   (setq indentation-string
			 (buffer-substring-no-properties (point) statement-start))
	   (insert "\n")
	   (forward-line -1)
	   (insert indentation-string)
	   (insert (concat variable-name " = " variable-value))
	   (goto-char new-position))
	 (right-char (length variable-name))
	 (message (concat "Variable `" variable-name "' generated."))))


(defun pygen-extract-variable-from-region ()
  "Extracts a variable from the current region."
  ;; TODO: verify that the current region is valid
  (let ((start-position (region-beginning))
		(end-position (region-end)))
	(pygen-extract-variable-internal start-position end-position)))


(defun pygen-bounds-of-thing-at-point ()
  "Get bounds of the Python thing at point.

Only works for things that are symbols."
  ;; TODO: get the bounds of the current thing at point
  (error "Function `pygen-bounds-of-thing-at-point' is not yet implemented."))


(defun pygen-add-decorator-to-function (&optional decorator)
  "Add a `DECORATOR' in front of the current function.

If function is called interactively, prompts for a decorator. 

If no decorator has been provided, prompts for a decorator."
  (interactive)
  (when (not (pygen-def-at-point))
	(error "Error: not currently in a function"))
  (when (not decorator)
	(setq decorator (read-string "Enter decorator: " "@"))
	(when (string= decorator "")
	  (error "Error: decorator cannot be empty.")))
  (save-excursion
	(let (def-start
		   indentation-string)
	  (py-beginning-of-def-or-class)
	  (setq def-start (point))
	  (beginning-of-line)
	  (setq indentation-string
			(buffer-substring-no-properties (point) def-start))
	  (insert "\n")
	  (forward-line -1)
	  (insert indentation-string)
	  (insert decorator))))


(defun pygen-remove-argument (argument)
  "Remove an `ARGUMENT' from the current function's definition.

This function can only be called from within a function
definition.  It will throw an error otherwise."
  ;; TODO: This check is performed twice if interactive frontend is called.
  ;; TODO: Allow this command to work with classes as well as functions
  (when (not (pygen-def-at-point))
	(error "Error: no def found at point."))
  (when (pygen-argument-already-exists argument)
	(py-beginning-of-def-or-class)
	;; TODO: Ensure this (and other instances of the same search)
	;; don't go past the end of the function.
	(search-forward "(")
	(while (re-search-forward "[A-Za-z0-9_*]")
	)
	)
  (error "Error: method not yet implemented."))


(defun pygen-generate-function ()
  "Generate a python function from the reference under point."
  (interactive)
  (let ((bounds (pygen-get-expression-bounds))
		function-name
		arguments
		has-parent
		is-method
		parent-is-self)
	;; Ensure it's a valid function signature
	(pygen-verify-expression bounds)
	(when (pygen-expression-exists)
	  (user-error "Error: this name already exists. Navigate to its definition to find out where."))
	;; Get function parts (name, args, is-method)
	(setq function-name (pygen-get-expression-name bounds t))
	(setq arguments (pygen-get-expression-arguments bounds t))
	(setq has-parent (pygen-has-parent bounds t))
	(when has-parent
	  (setq parent-is-self (pygen-is-parent-self bounds t has-parent)))

	(message "Generating function, please wait...")
	(if has-parent
		;; If the immediate parent is the "self" keyword
		(if parent-is-self
			(progn
			  (pygen-create-new-function-in-class arguments function-name))
		  (pygen-goto-expression-parent)
		  ;; Is the parent a class or a module? Each requires
		  ;; different handling.
		  (message "Still generating function, please wait...")
		  (redisplay)
		  (if (pygen-class-at-point)
			  (pygen-create-new-function-in-class arguments function-name)
			(pygen-create-new-function-in-module arguments function-name)))
	  ;; Otherwise create in current module.
	  (pygen-create-new-function-in-module arguments function-name))
	(message "Function generated.")))


(defun pygen-generate-static-function ()
  "Generate a static python function from the reference under point.

Must be currently inside a class to do this."
  (interactive)
  ;; FIXME: Generates static functions for the current class as
  ;;        module functions.
  ;; FIXME: Inserts the empty string as the name if invoked when
  ;;        point is at the start of a symbol.
  ;; TODO: extract this code into its own class.
  (let ((bounds (pygen-get-expression-bounds))
		function-name
		arguments
		has-parent
		is-method
		parent-is-self)
	;; Ensure it's a valid function signature
	(pygen-verify-expression bounds)
	(when (pygen-expression-exists)
	  (user-error "Error: this name already exists. Navigate to its definition to find out where."))
	;; Get function parts (name, args, has-parent)
	(setq has-parent (pygen-has-parent bounds t))
	;; (when (not has-parent)
	;;   (error "Error: cannot understand where to create this function. Does the expression specify a parent?"))
	(setq function-name (pygen-get-expression-name bounds t))
	(setq arguments (pygen-get-expression-arguments bounds t))
	
	(when has-parent
	  (setq parent-is-self (pygen-is-parent-self bounds t has-parent)))

	(message "Generating function, please wait...")
	(if has-parent
		;; If the immediate parent is the "self" keyword
		(if parent-is-self
			(progn
			  (pygen-create-new-function-in-class arguments function-name '("@staticmethod")))
		  (pygen-goto-expression-parent)
		  ;; Is the parent a class or a module? Each requires
		  ;; different handling.
		  (message "Still generating function, please wait...")
		  (redisplay)
		  (if (pygen-class-at-point)
			  (pygen-create-new-function-in-class arguments function-name '("@staticmethod"))
			(pygen-create-new-function-in-module arguments function-name)))
	  ;; Otherwise create in current module.
	  (pygen-create-new-function-in-module arguments function-name))
	(message "Function generated.")))


(defun pygen-generate-class ()
  "Generat a python class from the reference under point."
  (interactive)
  ;; TODO: Generate a python class from a reference
  (let ((bounds (pygen-get-expression-bounds)))
	(pygen-verify-expression bounds)
	;; Ensure it's a valid function signature
	(when (pygen-expression-exists)
	  (user-error "Error: this name already exists. Navigate to its definition to find out where."))
	(let* ((class-name (pygen-get-expression-name bounds t))
		   (arguments (pygen-get-expression-arguments bounds t)) 
		   (has-parent (pygen-has-parent bounds t))
		   (parent-is-self (if has-parent
							   (setq parent-is-self (pygen-is-parent-self bounds t has-parent))
							 nil)))
	  (message "Generating class, please wait...")
	  (if has-parent
		  ;; If the immediate parent is the "self" keyword
		  (if parent-is-self
			  (progn
				(pygen-create-new-class-in-class arguments class-name))
			(pygen-goto-expression-parent)
			(message "Still generating class, please wait...")
			(redisplay)
			;; Is the parent a class or a module? Each requires
			;; different handling.
			(if (pygen-class-at-point)
				(pygen-create-new-class-in-class arguments class-name)
			  (pygen-create-new-class-in-module arguments class-name)))
		;; Otherwise create in current module.
		(pygen-create-new-class-in-module arguments class-name))
	  (message "Class generated."))))


(defun pygen-extract-variable ()
  "Generate a python variable from a reference."
  (interactive)
  
  (if (region-active-p)
	  (progn 
		(pygen-extract-variable-from-region))
	(error "Error: region must be active to extract it into a variable.")
	;; Not implemented yet.  Control will not flow to here.
	(let* ((boundaries (pygen-bounds-of-thing-at-point))
		   (start-position (car boundaries))
		   (end-position (cdr boundaries)))
	  ;; TODO: Implement dynamic extraction of variables.
	  (pygen-extract-variable-internal start-position end-position))))


(defun pygen-make-keyword-argument ()
  "Add the variable under point as a keyword argument.

The variable under point will be added as a sequence argument to
the definition of the current function.  By sequence argument, I
mean the argument has a default value.  This command can only be
called from within a function.

This function is intelligent.  It will attempt to insert the
argument after existing keyword arguments end before star
arguments, to have minimal impact on existing calls to this
function."
  (interactive)
  (let ((variable-name (thing-at-point 'symbol)))
	(when (not variable-name)
	  (user-error "No variable under point."))
	(pygen-add-keyword-argument-internal variable-name))
  (message "Keyword argument created. Input default value."))


(defun pygen-make-sequence-argument ()
  "Add the variable under point as a sequence argument.

The variable under point will be added as a sequence argument to
the definition of the current function.  By sequence argument, I
mean an argument that has no default value.  This command can
only be called from within a function.

This function is intelligent.  It will attempt to insert the
argument after existing arguments but before keyword arguments,
to have minimal impact on existing calls to this function."
  (interactive)
  (let ((variable-name (thing-at-point 'symbol)))
	(when (not variable-name)
	  (user-error "No variable under point."))
	(save-excursion
	  (pygen-add-sequence-argument-internal variable-name)))
  (message "Sequence argument created."))


(defun pygen-insert-super ()
  "Insert a proper call to the superclass.

This is basically just a wrapper for `py-insert-super'"
  (interactive)
  (py-insert-super))


(defun pygen-make-static ()
  "Turn the current function into a static function."
  (interactive)
  (pygen-add-decorator-to-function "@staticmethod")
  (when (pygen-argument-already-exists "self")
	(pygen-remove-argument "self")))


(defun pygen-selfify-symbol ()
  "Puts the word 'self.' in front of the current symbol."
  (interactive)
  (save-excursion
	(when (not (symbol-at-point))
	  (error "No symbol could be found at point."))
	(when (not (pygen-class-at-point))
	  (error "Not currently in a class."))
	(when (string= (symbol-at-point) "self")
	  (error "'self' keyword already present."))
	(goto-char (car (pygen-get-expression-bounds)))
	(when (looking-back "self\.[A-Za-z0-9._*]*")
	  (error "Keyword 'self.' already attached to this symbol."))
	(re-search-backward "[^A-Za-z0-9._*]")
	(right-char)
	(insert "self.")
	t))


(defun pygen-unselfify-symbol ()
  "Remove the word 'self.' from the current symbol (if it exists)."
  (interactive)
  (save-excursion
	(when (not (symbol-at-point))
	  (error "No symbol could be found at point."))
	(if (string= (symbol-at-point) "self")
	  (let ((bounds (bounds-of-thing-at-point 'symbol)))
		(goto-char (car bounds))
		(delete-region (car bounds) (cdr bounds))
		(when (looking-at "\.")
		  (delete-char 1)))
	  (goto-char (car (pygen-get-expression-bounds)))
	  (if (looking-back "self\.[A-Za-z0-9._*]*")
		  (progn
			(search-backward "self.")
			(delete-char (length "self.")))
		(error "The keyword 'self.' could not be found")
		nil))))


(defun pygen-toggle-selfify-symbol ()
  "Toggle the word 'self' in front of the current symbol."
  (interactive)
  (save-excursion
	(when (not (symbol-at-point))
	  (error "No symbol could be found at point."))
	(goto-char (car (pygen-get-expression-bounds)))
	(if (or (looking-back "self\.[A-Za-z0-9._*]*")
			(string= (symbol-at-point) "self"))
		(pygen-unselfify-symbol)
	  (pygen-selfify-symbol))))


(defvar pygen-mode-map (make-sparse-keymap)
  "Keymap for pygen.")
(set-keymap-parent pygen-mode-map python-mode-map)

(define-key pygen-mode-map (kbd "C-c C-= @") 'pygen-add-decorator-to-function)
(define-key pygen-mode-map (kbd "C-c C-= v") 'pygen-extract-variable)
(define-key pygen-mode-map (kbd "C-c C-= c") 'pygen-generate-class)
(define-key pygen-mode-map (kbd "C-c C-= f") 'pygen-generate-function)
(define-key pygen-mode-map (kbd "C-c C-= s") 'pygen-generate-static-function)
(define-key pygen-mode-map (kbd "C-c C-= u") 'pygen-insert-super)
(define-key pygen-mode-map (kbd "C-c C-= k") 'pygen-make-keyword-argument)
(define-key pygen-mode-map (kbd "C-c C-= a") 'pygen-make-sequence-argument)
(define-key pygen-mode-map (kbd "C-c C-= m") 'pygen-make-static)
;; (define-key pygen-mode-map (kbd "") 'pygen-selfify-symbol)
;; (define-key pygen-mode-map (kbd "") 'pygen-unselfify-symbol)
(define-key pygen-mode-map (kbd "C-c C-= .") 'pygen-toggle-selfify-symbol)


(define-minor-mode pygen-minor-mode
  "Minor mode that allows python code-generation commands to be used.

These commands are outlined in the main file, as well as in the
GitHub repo for this project."
  :group 'pygen
  :keymap pygen-mode-map
  :lighter " pygen"
  (when pygen-minor-mode
	(pygen-verify-environment)
	(run-hooks pygen-mode-hook)))


;; TODO: generating constants?

;; TODO: pygen-make-star-argument

;; TODO: Make all these commands fully compatible with star arguments
;; (*args, **kwargs).

;; TODO: Auto-generation of star args from arguments named "args" and
;; "kwargs".
;; https://pythontips.com/2013/08/04/args-and-kwargs-in-python-explained/
;;
;; Once that is done, the package should be ready for
;; deployment. Possibly add in some default keybindings?

;; TODO: Possibly have a verification function when pygen commands are
;; called to ensure the required packages are installed?

;; TODO: Add Hydra

;; TODO: Implement suggestions from the Melpa guys

;; TODO: Change keybindings ("C-=" is not available on some terminals)

;; FIXME: Can't generate function if it looks like the following:
;;        variable = Class(function_name)


(provide 'pygen)
;;; pygen.el ends here
