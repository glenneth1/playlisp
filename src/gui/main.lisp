;;;; main.lisp - Entry points for playlisp GUI

(in-package #:playlisp-gui)

(defun run (&key filepath (width 1000) (height 700) (new-process t))
  "Launch the playlisp McCLIM GUI application.
   FILEPATH - optional path to an M3U playlist to load on startup.
   WIDTH and HEIGHT set the initial window size in pixels.
   NEW-PROCESS controls whether to run in a separate thread."
  (%limit-font-path)
  (let ((frame (clim:make-application-frame
                'playlisp-app
                :width width
                :height height)))
    (when filepath
      (setf (frame-filepath frame) (namestring (truename filepath))))
    (if new-process
        (clim:run-frame-top-level frame :new-process t)
        (progn
          (clim:run-frame-top-level frame :new-process nil)
          (uiop:quit)))))

(defun main ()
  "Top-level entry point for standalone binary."
  (let ((args (uiop:command-line-arguments)))
    (run :filepath (first args) :new-process nil)))
