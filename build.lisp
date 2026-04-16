;;;; build.lisp - Build standalone playlisp binary
;;;;
;;;; Produces a single binary that supports both GUI and TUI modes:
;;;;   playlisp -G [playlist.m3u]   → McCLIM graphical interface
;;;;   playlisp -T [playlist.m3u]   → McCLIM terminal interface (charmed)
;;;;   playlisp [playlist.m3u]      → defaults to GUI

(require :asdf)

;;; Suppress Quicklisp output during build
(let ((*standard-output* *error-output*))
  ;; Load both interfaces
  (ql:quickload :playlisp/gui)
  (handler-case
      (ql:quickload :playlisp/mcclim)
    (error (e)
      (format *error-output* "~&Warning: TUI backend not available: ~A~%" e)
      (format *error-output* "         Binary will only support -G (GUI) mode.~%"))))

(ensure-directories-exist #P"bin/")

(defun playlisp-main ()
  "Top-level entry point for the standalone binary.
   Dispatches to GUI or TUI based on command-line flags."
  (let* ((args (uiop:command-line-arguments))
         (mode (cond
                 ((member "-T" args :test #'string-equal) :tui)
                 ((member "-G" args :test #'string-equal) :gui)
                 (t :gui)))
         (file-args (remove-if (lambda (a) (member a '("-T" "-G") :test #'string-equal))
                               args))
         (filepath (first file-args)))
    (case mode
      (:gui
       (playlisp-gui:run :filepath filepath :new-process nil))
      (:tui
       (if (find-package :playlisp/src/mcclim-app)
           (funcall (find-symbol "RUN" :playlisp/src/mcclim-app) filepath)
           (progn
             (format *error-output* "Error: TUI backend not available in this build.~%")
             (uiop:quit 1)))))))

#+sbcl
(sb-ext:save-lisp-and-die "bin/playlisp"
                           :toplevel #'playlisp-main
                           :executable t
                           :compression t)

#+ccl
(ccl:save-application "bin/playlisp"
                      :toplevel-function #'playlisp-main
                      :prepend-kernel t)

#-(or sbcl ccl)
(error "Unsupported implementation for binary build. Use SBCL or CCL.")
