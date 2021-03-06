;;; package-stack.el --- Configuration Manager with Elegant Dependence Managing

;; Copyright (C) 2017- Junyoung Clare Jang

;; Author: Junyoung Clare Jang <jjc9310@gmail.com>
;; Maintainer: Junyoung Clare Jang <jjc9310@gmail.com>
;; Created: 31 Jan 2017
;; Version: 0.0.1
;; Package-Requires: (dash)
;; Keywords: dotemacs startup speed config package
;; URL: https://github.com/ailrun/package-stack

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with package-stack; see the file COPYING.  If not, see
;; <http://www.gnu.org/licenses/>.

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
(require 'package-stack-log)

(defconst package-stack/github-url "https://github.com/ailrun/package-stack")

(defgroup package-stack nil
  "Settings for `package-stack'."
  :group 'emacs)

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



(defun package-stack/settings-to-keyal (name-sym settings)
  "NAME-SYM SETTINGS."
  (declare (indent 1))
  (--reduce-from
   (if (keywordp it)
       (progn
         (when (assq it acc)
           (error "%s has duplicated keyword %s"
                  it name-sym))
         (cons (cons it nil) acc))
     (-let ((((keyword . keyargs) . tail) acc))
       (cons (cons keyword (-snoc keyargs it)) tail)))
    nil
    settings))

(defun package-stack/standardize-keyal (name-sym keyal)
  "Standardizer of NAME-SYM for all KEYAL."
  (declare (indent 1))
  (package-stack/-debug-log
   "")

  (--map
   (-let* (((keyword . args) it)
           (validator (intern (concat "package-stack/validate-"
                                      (symbol-name keyword))))
           (standardizer (intern (concat "package-stack/standardize-"
                                         (symbol-name keyword))))

           (validity
            (progn
              (cond ((eq keyword :ignored)
                     (error (concat
                             "%s has unchecked :ignored"
                             ".  Please report this via %s")
                            name-sym
                            package-stack/github-url))
                    ((functionp validator)
                     (package-stack/-debug-log
                      "Common validator of %s=%s" name-sym validator)
                     (funcall validator name-sym args))
                    (t
                     (error "No validator for %s was found"
                            keyword)))))

           (standard-args
            (progn
              (cond ((null validity)
                     (error "%s has keyword %s with invalid args %s"
                            name-sym
                            keyword
                            args))
                    ((functionp standardizer)
                     (package-stack/-debug-log
                      "Common standardizer of %s=%s" name-sym standardizer)
                     (funcall standardizer name-sym args))
                    (t
                     (error "No standardizer for %s was found"
                            keyword))))))

     (package-stack/-verbose-log
      "Standard Args of %s=%s" keyword standard-args)
     (cons keyword standard-args))
   keyal))

(defun package-stack/sort-keyal (name-sym keyal)
  "Sorting function for settings of NAME-SYM.

This function sorts keywords in KEYAL using ‘package-stack/keywords’."
  (declare (indent 1))
  (package-stack/-debug-log
   "")
  (if (--all? (member (car it) package-stack/keywords) keyal)
      (let ((sorted-keyal
             (--sort (< (-elem-index it package-stack/keywords)
                        (-elem-index other package-stack/keywords))
                     keyal)))
        (package-stack/-debug-log
         "SORTED-KEYAL=%S" sorted-keyal))
    (error "%s has a keyword not in package-stack/keywords" name-sym)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commons for keywords
;;

(defun package-stack/validate-no-args (name-sym args)
  "Common validator of NAME-SYM for no ARGS keywords.

Valid no ARGS keywords require empty ARGS.

1. :keyword"
  (declare (indent 1))
  (package-stack/-debug-log
   "%s has ARGS=%S" name-sym args)
  (null args))

(defun package-stack/standardize-no-args (name-sym args)
  "Common standardizer of NAME-SYM for no ARGS keywords.

Standard no ARGS keywords use nothing.

1. :keyword"
  (declare (indent 1))
  (package-stack/-debug-log
   "%s has ARGS=%S" name-sym args)
  nil)

(defun package-stack/validate-sexp-args (name-sym args)
  "Common validator of NAME-SYM for S-Exp ARGS keywords.

Valid S-Exp ARGS keywords require S-Exps or a list of S-Exp.

1. :keyword S-EXP ...
2. :keyword (S-EXP ...)"
  (declare (indent 1))
  (package-stack/-debug-log
   "%s has ARGS=%S" name-sym args)
  (listp (car args)))

(defun package-stack/standardize-sexp-args (name-sym args)
  "Common standardizer of NAME-SYM for S-Exp ARGS keywords.

Standard S-Exp ARGS keywords use S-Exps.

1. :keyword S-EXP ..."
  (declare (indent 1))
  (package-stack/-debug-log
   "%s has ARGS=%S" name-sym args)
  (if (cdr args)
      args
    (car args)))

(defun package-stack/validate-cons-ss-args (name-sym args)
  "Common validator of NAME-SYM for string/symbol pair ARGS keywords.

Valid cons of string/symbol ARGS keywords require cons cells of
string and symbol or a list of cons cells of string and symbol.

1. :keyword ((STRING . SYMBOL) ...)"
  (declare (indent 1))
  (package-stack/-debug-log
   "%s has ARGS=%S" name-sym args)
  (and (listp args)
       (--all? (or
                (and (listp it)
                     (string-or-null-p (car it))
                     (symbolp (cdr it)))
                (and (listp (car it))
                     (string-or-null-p (caar it))
                     (symbolp (cdar it))))
               args)))

(defun package-stack/standardize-cons-ss-args (name-sym args)
  "Common standardizer of NAME-SYM for string/symbol pair ARGS keywords.

Standard cons of string/symbol keywords use a list of
cons cells of string and symbol.

1. :keyword ((STRING . SYMBOL) ...)"
  (declare (indent 1))
  (package-stack/-debug-log
   "%s has ARGS=%S" name-sym args)
  args)

(defun package-stack/validate-s-or-s-args (name-sym args)
  "Common validator of NAME-SYM for string-or-symbol ARGS keywords.

Valid string-or-symbol ARGS keywords require a list of, strings or symbols.

1. :keyword (SYMBOL-OR-STRING ...)"
  (declare (indent 1))
  (package-stack/-debug-log
   "%s has ARGS=%S" name-sym args)
  (and (listp (car args))
       (--all? (or (stringp it)
                   (symbolp it))
               (car args))))

(defun package-stack/standardize-s-or-s-args (name-sym args)
  "Common validator of NAME-SYM for string-or-symbol ARGS keywords.

Standard string-or-symbol ARGS keywords use a list of symbols.

1. :keyword (SYMBOL ...)"
  (declare (indent 1))
  (package-stack/-debug-log
   "%s has ARGS=%S" name-sym args)
  (--map (if (stringp it)
             (intern it)
           it)
         args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; :now keyword
;;

(defalias 'package-stack/validate-:now
  'package-stack/validate-no-args)

(defalias 'package-stack/standardize-:now
  'package-stack/standardize-no-args)

;; "Resolver of NAME-SYM with STD-KEYAL for ‘:now’ keyword."
(defun package-stack/resolve-:now (name-sym std-keyal))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; :on-start keyword
;;

(defalias 'package-stack/validate-:on-start
  'package-stack/validate-no-args)

(defalias 'package-stack/standardize-:on-start
  'package-stack/standardize-no-args)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; :first keyword
;;

(defalias 'package-stack/validate-:first
  'package-stack/validate-sexp-args)

(defalias 'package-stack/standardize-:first
  'package-stack/validate-sexp-args)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; :modes keyword
;;

(defalias 'package-stack/validate-:modes
  'package-stack/validate-cons-ss-args)

(defalias 'package-stack/standardize-:modes
  'package-stack/validate-cons-ss-args)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; :interpreters keyword
;;

(defalias 'package-stack/validate-:interpreters
  'package-stack/validate-cons-ss-args)

(defalias 'package-stack/standardize-:interpreters
  'package-stack/validate-cons-ss-args)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; :commands keyword
;;

(defalias 'package-stack/validate-:commands
  'package-stack/validate-s-or-s-args)

(defalias 'package-stack/standardize-:commands
  'package-stack/standardize-s-or-s-args)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; :before-deps keyword
;;

(defalias 'package-stack/validate-:before-deps
  'package-stack/validate-sexp-args)

(defalias 'package-stack/standardize-:before-deps
  'package-stack/validate-sexp-args)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; :deps keyword
;;

(defalias 'package-stack/validate-:deps
  'package-stack/validate-s-or-s-args)

(defalias 'package-stack/standardize-:deps
  'package-stack/standardize-s-or-s-args)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; :before-load keyword
;;

(defalias 'package-stack/validate-:before-load
  'package-stack/validate-sexp-args)

(defalias 'package-stack/standardize-:before-load
  'package-stack/validate-sexp-args)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; :after-load keyword
;;

(defalias 'package-stack/validate-:after-load
  'package-stack/validate-sexp-args)

(defalias 'package-stack/standardize-:after-load
  'package-stack/validate-sexp-args)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; :last keyword
;;

(defalias 'package-stack/validate-:last
  'package-stack/validate-sexp-args)

(defalias 'package-stack/standardize-:last
  'package-stack/validate-sexp-args)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; package-stack/keystack
;;

(defvar package-stack/keystack [])

(defun package-stack/keystack-insert-package (name-sym std-keyal))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; The package-stack macro
;;

(defmacro package-stack (package-name &rest settings)
  "Define an isolated package configuration for PACKAGE-NAME using SETTINGS.

Usage: (package-stack package-name [:keyword [options]])

Keywords in order they resolved.

:ignored            Ignore this configuration when this keyword is given.
                    `package-stack' works as if this configuration
                    doesn't exists, even if there are wrong configurations.
:now                Load immediatly when this keyword is given.
                    With this keyword, ‘:deps’ keyword is ignored.
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
           (keyal
            (package-stack/settings-to-keyal name-sym settings)))
      (setq keyal (package-stack/standardize-keyal name-sym keyal))
      (setq keyal (package-stack/sort-keyal name-sym keyal))
      (if (assq :now keyal)
          (package-stack/resolve-:now name-sym keyal)
        (package-stack/keystack-insert-package name-sym keyal))
      t)))

(provide 'package-stack)


;;; package-stack.el ends here
