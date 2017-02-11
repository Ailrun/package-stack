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

;; TEST:
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
cons cells of string and symbol is valid."
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
a list of cons of string and symbol is invalid."
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
cons cells of string and symbol is valid."
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
a list of cons of string and symbol is invalid."
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

(provide 'package-stack-test)


;;; package-stack-test.el ends here
