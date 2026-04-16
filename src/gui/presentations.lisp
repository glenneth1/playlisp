;;;; presentations.lisp - CLIM presentation types for playlisp GUI

(in-package #:playlisp-gui)

;;; ---------------------------------------------------------------------------
;;; Presentation type: playlisp-track
;;; ---------------------------------------------------------------------------

(clim:define-presentation-type playlisp-track ()
  :description "a playlist track")

(clim:define-presentation-method clim:present
    (object (type playlisp-track) stream (view clim:textual-view) &key)
  (let ((name (or (title object)
                  (file-namestring (track-path object))
                  "Unknown")))
    (format stream "~A" name)))

(clim:define-presentation-method clim:describe-presentation-type
    ((type playlisp-track) stream plural-count)
  (declare (ignore plural-count))
  (format stream "playlist track"))

;;; ---------------------------------------------------------------------------
;;; Presentation type: track-index (for positional operations)
;;; ---------------------------------------------------------------------------

(clim:define-presentation-type track-index ()
  :description "a track position index")

;;; ---------------------------------------------------------------------------
;;; Presentation type: browser-entry (for file browser)
;;; ---------------------------------------------------------------------------

(clim:define-presentation-type browser-entry ()
  :description "a file browser entry")
