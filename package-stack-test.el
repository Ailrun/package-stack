;;; package-stack-test --- Test for package-stack

;; Copyright (C) 2017- Junyoung Clare Jang

;; Author: Junyoung Clare Jang <jjc9310@gmail.com>
;; Maintainer: Junyoung Clare Jang <jjc9310@gmail.com>
;; Created: 31 Jan 2017
;; Version: 0.0.1
;; URL: https://github.com/ailrun/package-stack


;;; Commentary:

;;; Code:

(require 'package-stack)


(ert-deftest package-stack/:ignored ()
  "Test ‘:ignored’ keyword.

With ‘:ignored’ keyword, package-stack settings must be ignored that
without any error, even when the setting is invalid one."
  (should
   (null
    (macroexpand-all
     '(package-stack test-package
        :ignored))))
  (should
   (null
    (macroexpand-all
     '(package-stack test-package
        :ignored
        :no-such-keyword))))
  (should
   (null
    (macroexpand-all
     '(package-stack test-package
        :ignored
        :now nil))))
  (should
   (null
    (macroexpand-all
     '(package-stack test-package
        :ignored
        :now this-is-illegal)))))

(ert-deftest package-stack/:now--valid-args ()
  "Test valid arguments for ‘:now’ keyword.

For ‘:now’, no argument is only valid one."
  (should
   (macroexpand-all
    '(package-stack test-package
       :now))))

(ert-deftest package-stack/:now--invalid-args ()
  "Test invalid arguments for ‘:now’ keyword.

For ‘:now’, any arguments are invalid."
  (should-error
   (macroexpand-all
    '(package-stack test-package
       :now nil)))
  (should-error
   (macroexpand-all
    '(package-stack test-package
       :now t)))
  (should-error
   (macroexpand-all
    '(package-stack test-package
       :now (t))))
  (should-error
   (macroexpand-all
    '(package-stack test-package
       :now '(t)))))

(ert-deftest package-stack-:on-start--valid-args ()
  "Test valid arguments for ‘:on-start’ keyword.

For ‘:on-start’, no argument is only valid one."
  (should
   (macroexpand-all
    '(package-stack test-package
       :on-start))))

(ert-deftest package-stack/:on-start--invalid-args ()
  "Test invalid arguments for ‘:on-start’ keyword.

For ‘:on-start’, any arguments are invalid."
  (should-error
   (macroexpand-all
    '(package-stack test-package
       :on-start nil)))
  (should-error
   (macroexpand-all
    '(package-stack test-package
       :on-start t)))
  (should-error
   (macroexpand-all
    '(package-stack test-package
       :on-start (t))))
  (should-error
   (macroexpand-all
    '(package-stack test-package
       :on-start '(t)))))

(ert-deftest package-stack/:first--valid-args ()
  "Test valid arguments for ‘:first’ keyword.

For ‘:first’, S-Exps or a list of S-Exps arguments are valid."
  (should
   (macroexpand-all
    '(package-stack test-package
       :first nil)))
  (should
   (macroexpand-all
    '(package-stack test-package
       :first ())))
  (should
   (macroexpand-all
    '(package-stack test-package
       :first (print 1))))
  (should
   (macroexpand-all
    '(package-stack test-package
       :first ((print 1)))))
  (should
   (macroexpand-all
    '(package-stack test-package
       :first (print 1) (print 2))))
  (should
   (macroexpand-all
    '(package-stack test-package
       :first ((print 1) (print 2))))))

(ert-deftest package-stack/:first--invalid-args ()
  "Test invalid arguments for ‘:first’ keyword.

For ‘:first’, any arguments those are not S-Exps or a list of S-Exps
are invalid."
  (should-error
   (macroexpand-all
    '(package-stack test-package
       :first t)))
  (should-error
   (macroexpand-all
    '(package-stack test-package
       :first 1)))
  (should-error
   (macroexpand-all
    '(package-stack test-package
       :first cons 1 nil)))
  (should-error
   (macroexpand-all
    '(package-stack test-package
       :first [cons 1 nil]))))

(ert-deftest package-stack/:modes--valid-args ()
  "Test valid arguments for ‘:modes’ keyword.

For ‘:modes’, cons cells of string and symbol or a list of
cons cells of string and symbol are valid."
  (should
   (macroexpand-all
    '(package-stack test-package
       :modes
       nil)))
  (should
   (macroexpand-all
    '(package-stack test-package
       :modes
       ("\\.test\\'" . test-mode))))
  (should
   (macroexpand-all
    '(package-stack test-package
       :modes
       (("\\.test\\'" . test-mode)))))
  (should
   (macroexpand-all
    '(package-stack test-package
       :modes
       ("\\.test\\'" . test-mode)
       ("\\.ntest\\'" . not-test-mode))))
  (should
   (macroexpand-all
    '(package-stack test-package
       :modes
       (("\\.test\\'" . test-mode)
        ("\\.ntest\\'" . not-test-mode))))))

(ert-deftest package-stack/:modes--invalid-args ()
  "Test invalid arguments for ‘:modes’ keyword.

For ‘:modes’, any arguments those are not cons cells of string or
a list of cons of string and symbol are invalid."
  (should-error
   (macroexpand-all
    '(package-stack test-package
       :modes
       t)))
  (should-error
   (macroexpand-all
    '(package-stack test-package
       :modes
       (cons 1 nil)))))

(ert-deftest package-stack/:interpreters--valid-args ()
  "Test valid arguments for ‘:interpreters’ keyword.

For ‘:interpreters’, cons cells of string and symbol or a list of
cons cells of string and symbol are valid."
  (should
   (macroexpand-all
    '(package-stack test-package
       :interpreters
       nil)))
  (should
   (macroexpand-all
    '(package-stack test-package
       :interpreters
       ("\\.test\\'" . test-mode))))
  (should
   (macroexpand-all
    '(package-stack test-package
       :interpreters
       (("\\.test\\'" . test-mode)))))
  (should
   (macroexpand-all
    '(package-stack test-package
       :interpreters
       ("\\.test\\'" . test-mode)
       ("\\.ntest\\'" . not-test-mode))))
  (should
   (macroexpand-all
    '(package-stack test-package
       :interpreters
       (("\\.test\\'" . test-mode)
        ("\\.ntest\\'" . not-test-mode))))))

(ert-deftest package-stack/:interpreters--invalid-args ()
  "Test invalid arguments for ‘:interpreters’ keyword.

For ‘:interpreters’, any arguments those are not cons cells of string or
a list of cons of string and symbol are invalid."
  (should-error
   (macroexpand-all
    '(package-stack test-package
       :interpreters
       t)))
  (should-error
   (macroexpand-all
    '(package-stack test-package
       :interpreters
       (cons 1 nil)))))

(ert-deftest package-stack/:commands--valid-args ()
  "Test valid arguments for ‘:commands’ keyword.

For ‘:commands’, a list of, strings or symbols is valid."
  (should
   (macroexpand-all
    '(package-stack test-package
       :commands
       nil)))
  (should
   (macroexpand-all
    '(package-stack test-package
       :commands
       ("test-command"))))
  (should
   (macroexpand-all
    '(package-stack test-package
       :commands
       (test-command))))
  (should
   (macroexpand-all
    '(package-stack test-package
       :commands
       ("test-command1" "test-command2"))))
  (should
   (macroexpand-all
    '(package-stack test-package
       :commands
       ("test-command1" test-command2))))
  (should
   (macroexpand-all
    '(package-stack test-package
       :commands
       (test-command1 "test-command2"))))
  (should
   (macroexpand-all
    '(package-stack test-package
       :commands
       (test-command1 test-command2)))))

(ert-deftest package-stack/:commands--invalid-args ()
  "Test invalid arguments for ‘:commands’ keyword.

For ‘:commands’, any arguments those are not a list of,
strings or symbols are invalid."
  (should-error
   (macroexpand-all
    '(package-stack test-package
       :commands
       t)))
  (should-error
   (macroexpand-all
    '(package-stack test-package
       :commands
       (cons 1 nil))))
  (should-error
   (macroexpand-all
    '(package-stack test-package
       :commands
       (cons 1 0))))
  (should-error
   (macroexpand-all
    '(package-stack test-package
       :commands
       [test-command]))))

(provide 'package-stack-test)


;;; package-stack-test.el ends here
