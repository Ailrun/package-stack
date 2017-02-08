;;; package-stack-log --- Log functions for package-stack

;;; Commentary:

;;; Code:

(require 'dash)

(defconst package-stack/-no-log 0)
(defconst package-stack/-load-log 1)
(defconst package-stack/-verbose-log 2)
(defconst package-stack/-debug-log 3)

(defcustom package-stack/log-level package-stack/-debug-log
  "Setting for log level of package-stack.

Lowest log level is ‘package-stack/-no-log’, and in ascending order,
next levels are ‘package-stack/-load-log’, ‘package-stack/-verbose-log’,
and ‘package-stack/-debug-log’."
  :type '(choice (const :tag "no-log"
                        package-stack/-no-log)
                 (const :tag "load-log"
                        package-stack/-load-log)
                 (const :tag "verbose-log"
                        package-stack/-verbose-log)
                 (const :tag "debug-log"
                        package-stack/-debug-log))
  :group 'package-stack)

(defconst package-stack/-messages-buffer "*Messages*")
(defconst package-stack/-package-stack-buffer "*PackageStack*")

(defcustom package-stack/log-buffer package-stack/-messages-buffer
  "Setting for log buffer of package-stack.

If this has ‘package-stack/-messages-buffer’,
 then logs will written in `*Messages*' buffer.
If this has ‘package-stack/-package-stack-buffer’,
 then logs will written in `*PackageStack*' buffer."
  :type '(choice (const :tag "Messages"
                        package-stack/-messages-buffer)
                 (const :tag "PackageStack"
                        package-stack/-package-stack-buffer))
  :group 'package-stack)



(defmacro package-stack/--log (log-msg)
  "Find a function to pass LOG-MSG depending on customization."
  `(let (buf
         (msg ,log-msg))
     (cond
      ((equal package-stack/log-buffer
              package-stack/-messages-buffer)
       (message "%s" msg)
       nil)
      ((equal package-stack/log-buffer
              package-stack/-package-stack-buffer)
       (with-current-buffer
           (get-buffer-create package-stack/log-buffer)
         (goto-char (point-max))
         (setq buffer-read-only nil)
         (insert (format "%s" msg) "\n")
         (setq buffer-read-only t)))
      (t
       (error "Error: Malformed package-stack/log-buffer")))))

(defmacro package-stack/-log-caller-name ()
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

(defmacro package-stack/-log (log-level
                              log-format &rest args)
  "If LOG-LEVEL is less than option ‘package-stack/log-level’, write log.

Write log using LOG-FORMAT and ARGS when LEVEL is
less than option ‘package-stack/log-level’."
  (declare (indent 1))
  `(let ((level ,log-level)
         (caller (package-stack/-log-caller-name)))
     (unless (= package-stack/-no-log level)
       (when (<= level package-stack/log-level)
         (package-stack/--log
          (concat
           (when (<= package-stack/-load-log package-stack/log-level)
             (format "[%s] "
                     (format-time-string "%Y-%m-%d, %T.%3N")))
           (when (<= package-stack/-debug-log package-stack/log-level)
             (format "%-35s --- "
                     (if caller
                         caller
                       "Not a Function")))
           "\""
           (format ,log-format ,@args)
           "\""))))))

(defmacro package-stack/-load-log (log-format &rest args)
  "Debug level log writing function.

Pass ‘package-stack/-load-level’, LOG-FORMAT and ARGS to function ‘package-stack/-log’."
  `(package-stack/-log package-stack/-load-log ,log-format ,@args))

(defmacro package-stack/-verbose-log (log-format &rest args)
  "Debug level log writing function.

Pass ‘package-stack/-verbose-level’, LOG-FORMAT and ARGS to function ‘package-stack/-log’."
  `(package-stack/-log package-stack/-verbose-log ,log-format ,@args))

(defmacro package-stack/-debug-log (log-format &rest args)
  "Debug level log writing function.

Pass ‘package-stack/-debug-level’, LOG-FORMAT and ARGS to function ‘package-stack/-log’."
  `(package-stack/-log package-stack/-debug-log ,log-format ,@args))


(provide 'package-stack-log)

;;; package-stack-log.el ends here
