;;; package-stack --- Package Manager with Elegant Dependence Managing.

;; Copyright (C) 2017- Junyoung Clare Jang

;; Author: Junyoung Clare Jang <jjc9310@gmail.com>
;; Maintainer: Junyoung Clare Jang <jjc9310@gmail.com>
;; Created: 31 Jan 2017
;; Version: 0.1
;; Package-Requires: (dash)
;; Keywords: dotemacs startup speed config package
;; URL: https://github.com/ailrun/package-stack


;;; Commentary:

;; The `package-stack' defines macro for stacking
;; isolated package configurations while keep their
;; dependence in mind.  By using this package, you can
;; configure your ".emacs" file more neat way.
;;
;; Of course, you can extend `package-stack' for your
;; specific usage!
;;
;; For more detail, please see README.org.

;;; Code:

(require 'dash)

(defgroup package-stack nil
  "Settings for `package-stack'."
  :group 'emacs)

(defconst package-stack/github-url "https://github.com/ailrun/package-stack")

(defconst package-stack/no-log 0)
(defconst package-stack/load-log 1)
(defconst package-stack/verbose-log 2)
(defconst package-stack/debug-log 3)

(defcustom package-stack/log-level package-stack/debug-log
  "Setting for log level of package-stack.

Lowest log level is ‘No Log’, and in ascending order,
next levels are ‘Load Log’, ‘Macro Log’, and ‘Debug Log’."
  :type '(choice (const :tag "No Log"
                        package-stack/no-log)
                 (const :tag "Load Log"
                        package-stack/load-log)
                 (const :tag "Verbose Log"
                        package-stack/verbose-log)
                 (const :tag "Debug Log"
                        package-stack/debug-log))
  :group 'package-stack)

(defcustom package-stack/keywords
  '(:ignored
    :now
    :on-start
    :first
    :modes
    :interpreters
    :commands
    :before-deps
    :deps
    :before-load
    :after-load
    :last)
  "List of valid keywords in package-stack, in the order they are processed in.

However, in ‘package-stack’ with ‘:ignored’ keyword, any invalid keywords are
just ignored with ‘package-stack’ itself."
  :type '(repeat symbol)
  :group 'package-stack)

(defmacro package-stack/log-caller-name ()
  "Get the name of log caller at expansion time."
  (symbol-name
   (cadr
    (cadr
     (--first (ignore-errors
                (equal (car it) 'defalias))
              (reverse (let ((frames-args)
                             (frame)
                             (index 10))
                         (while (setq frame (backtrace-frame index))
                           (when (car frame)
                             (push (nth 2 frame) frames-args))
                           (setq index (1+ index)))
                         frames-args)))))))

(defmacro package-stack/log (level
                             msg-format &rest args)
  "If LEVEL is less than ‘package-stack/log-level’, write log.

Write log using MSG-FORMAT and ARGS when LEVEL is
less than ‘package-stack/log-level’."
  (declare (indent 1))
  `(unless (= package-stack/no-log ,level)
     (when (<= ,level package-stack/log-level)
       (message "%s"
                (concat
                 (when (<= package-stack/load-log ,level)
                   (format "[%s] "
                           (format-time-string "%Y-%m-%d, %T")))
                 (when (<= package-stack/debug-log ,level)
                   (format "%s --- " (package-stack/log-caller-name)))
                 "\""
                 (format ,msg-format ,@args)
                 "\"")))))

(defun package-stack/break (pred ls)
  "In point of predicate PRED, split list LS into unsatisfied prefix and rest."
  (let ((res nil)
        (cont t))
    (while (and ls cont)
      (if (funcall pred (car ls))
          (setq cont nil)
        (setq res (cons (car ls) res))
        (setq ls (cdr ls))))
    (cons (nreverse res) ls)))

(defun package-stack/settings-to-keypl (name-sym settings)
  "NAME-SYM SETTINGS."
  (declare (indent 1))
  (if settings
      (let* ((keyword (car settings))
             (pair (package-stack/break #'keywordp (cdr settings)))
             (keyargs (car pair))
             (tail (cdr pair)))
        (cons (cons keyword keyargs)
              (package-stack/settings-to-keypl name-sym tail)))
    nil))

(defun package-stack/validate-keypl (name-sym keypl)
  "NAME-SYM KEYPL."
  (declare (indent 1))
  (package-stack/log package-stack/debug-log
    "")
  (when keypl
    (let* ((keyword (caar keypl))
           (args (cdar keypl))
           (validator (intern (concat "package-stack/validate-"
                                      (symbol-name keyword)))))
      (cond ((eq keyword :ignored)
             (error (concat
                     "Unchecked :ignored was found in %s.  "
                     "Please report this via %s")
                    (symbol-name name-sym)
                    package-stack/github-url))
            ((functionp validator)
             (funcall validator name-sym args))
            (t
             (error "No validator for %s was found"
                    keyword))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commons for keywords
;;

(defun package-stack/validate-no-args (_ args)
  "Common validator for no ARGS keywords."
  (declare (indent 1))
  (package-stack/log package-stack/debug-log
    "ARGS=%S" args)
  (null args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; :now keyword
;;

(defalias 'package-stack/validate-:now 'package-stack/validate-no-args)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; :on-start keyword
;;

(defalias 'package-stack/validate-:on-start 'package-stack/validate-no-args)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; :first keyword
;;

(defun package-stack/validate-:first (_ args)
  "Validator for ARGS of ‘:first’ keyword.

‘:first’ keyword requires S-Exps or a S-Exp."
  (declare (indent 1))
  (package-stack/log package-stack/debug-log
    "ARGS=%S" args)
  (listp (car args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; :modes keyword
;;

(defun package-stack/validate-:mode (_ args)
  "Validator for ARGS of ‘:mode’ keyword.

‘:mode’ keyword requires a list of string/symbol pair"
  (declare (indent 1))
  (and (listp args)
       (--all? (and (consp it)
                    (stringp (car it))
                    (symbolp (cdr it)))
               args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; The package-stack macro
;;

;; TEST: (package-stack 'this :first ((cons 1 2) (cons 2 3)))

(defmacro package-stack (package-name &rest settings)
  "Define an isolated package configuration for PACKAGE-NAME using SETTINGS.

Usage: (package-stack package-name [:keyword [options]])

Keywords in order they resolved.

:ignored            Ignore this configuration when this keyword is given.
                    `package-stack' works as if this configuration
                    doesn't exists, even if there are wrong configurations.
:now                Load immediatly when this keyword is given.
                    With this keyword, ‘:dep’ keyword is ignored.
:on-start           Load just after stack is configured when this keyword
                    is given.
:first CODE         CODE to run before everything except
                    ‘:ignored’, ‘:now’ and ‘:on-start’.
:modes FORM         FORM to be added to ‘auto-mode-alist’.
:interpreters FORM  FORM to be added to ‘interpreter-mode-alist’.
:commands COMMANDS  COMMANDS for autoloading.
:before-deps CODE   CODE to run before ‘:deps’ keyword is resolved.
:deps DEPS          Declare dependence of PACKAGE-NAME.
:before-load CODE   CODE to run before PACKAGE-NAME has been loaded.
:after-load CODE    CODE to run after PACKAGE-NAME has been loaded.
:last CODE          CODE to run after everything."
  (declare (indent 1))
  (unless (member :ignored settings)
    (let* ((name-sym (if (stringp package-name)
                         (intern package-name)
                       package-name))
           (keypl (package-stack/settings-to-keypl name-sym settings))
           (valid (package-stack/validate-keypl name-sym keypl)))
      valid)))


(provide 'package-stack)


;;; package-stack.el ends here
