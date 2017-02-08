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
(package-stack this
  :now
  :first (cons 1 2) (cons 2 3)
  :deps (hi bi))

(provide 'package-stack-test)


;;; package-stack-test.el ends here
