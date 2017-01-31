;;; package-stack --- Package Manager with Elegant Dependence Managing.

;; Copyright (C) 2017- Junyoung Clare Jang

;; Author: Junyoung Clare Jang <jjc9310@gmail.com>
;; Maintainer: Junyoung Clare Jang <jjc9310@gmail.com>
;; Created: 31 Jan 2017
;; Version: 0.1
;; Package-Requires: ()
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

(defgroup package-stack nil
  "Settings for `package-stack'."
  :group 'startup)

(defcustom package-stack/log-level 0
  "Setting for log level of package-stack.

Lowest log level is ‘No Log’, and in ascending order,
next levels are ‘Load Log’, ‘Macro Log’, and ‘Debug Log’."
  :type '(choice (const :tag "No Log" 0)
                 (const :tag "Load Log" 1)
                 (const :tag "Macro Log" 2)
                 (const :tag "Debug Log" 3))
  :group 'package-stack)

(defmacro package-stack (package-name &rest args)
  "Define an isolated package configuration for PACKAGE-NAME using ARGS.

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
:mode FORM          FORM to be added to ‘auto-mode-alist’.
:interpreter FORM   FORM to be added to ‘interpreter-mode-alist’.
:commands COMMANDS  COMMANDS for autoloading.
:before-dep CODE    CODE to run before ‘:dep’ keyword is resolved.
:dep DEPS           Declare dependence of PACKAGE-NAME.
:before-load CODE   CODE to run before PACKAGE-NAME has been loaded.
:after-load CODE    CODE to run after PACKAGE-NAME has been loaded.
:last CODE          CODE to run after everything."
  (declare (indent 1))
  (unless (member :ignored args)
    (print 5)))


(provide 'package-stack)


;;; package-stack.el ends here
