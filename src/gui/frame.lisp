;;;; frame.lisp - McCLIM application frame for playlisp GUI

(in-package #:playlisp-gui)

;;; ---------------------------------------------------------------------------
;;; Font path restriction
;;; ---------------------------------------------------------------------------
;;; McCLIM's CLX-TTF port loads every system TrueType font at startup,
;;; which can consume multiple gigabytes. We restrict it to only the
;;; bundled DejaVu fonts that McCLIM actually uses.

(defun %limit-font-path ()
  "Prevent McCLIM from scanning all system TrueType fonts at startup."
  (let ((sym (find-symbol "*TRUETYPE-FONT-PATH*" "MCCLIM-TRUETYPE")))
    (when (and sym (boundp sym))
      (setf (symbol-value sym) "")
      t)))

;;; ---------------------------------------------------------------------------
;;; Application frame
;;; ---------------------------------------------------------------------------

(clim:define-application-frame playlisp-app ()
  ((playlist :initform nil :accessor frame-playlist)
   (filepath :initform nil :accessor frame-filepath)
   (selected-index :initform 0 :accessor frame-selected-index)
   (message :initform nil :accessor frame-message)
   (edit-mode-p :initform nil :accessor frame-edit-mode-p)
   (browse-lines :initform nil :accessor frame-browse-lines)
   (preview-process :initform nil :accessor frame-preview-process)
   (preview-label :initform nil :accessor frame-preview-label))
  (:panes
   (tracklist :application
              :display-function 'display-tracklist
              :scroll-bars :vertical
              :incremental-redisplay nil
              :text-style (clim:make-text-style :fix :roman :small)
              :end-of-line-action :allow
              :end-of-page-action :allow)
   (details :application
            :display-function 'display-details
            :scroll-bars t
            :incremental-redisplay nil
            :text-style (clim:make-text-style :fix :roman :small)
            :end-of-line-action :allow
            :end-of-page-action :allow)
   (status :application
           :display-function 'display-status
           :scroll-bars nil
           :max-height 22
           :min-height 22
           :text-style (clim:make-text-style :fix :roman :small))
   (interactor :interactor
               :text-style (clim:make-text-style :fix :roman :small)
               :max-height 48
               :min-height 48))
  (:layouts
   (default
     (clim:vertically ()
       status
       (clim:horizontally ()
         (2/5 (clim:scrolling (:scroll-bars :vertical) details))
         (3/5 (clim:scrolling (:scroll-bars :vertical) tracklist)))
       interactor)))
  (:command-table (playlisp-commands))
  (:menu-bar t))

;;; ---------------------------------------------------------------------------
;;; Frame helpers
;;; ---------------------------------------------------------------------------

(defun frame-tracks (frame)
  "Return the list of tracks from the frame's playlist."
  (when (frame-playlist frame)
    (playlist-elements (frame-playlist frame))))

(defun frame-track-count (frame)
  "Return the number of tracks in the playlist."
  (length (frame-tracks frame)))

(defun frame-selected-track (frame)
  "Return the currently selected track."
  (let ((tracks (frame-tracks frame))
        (idx (frame-selected-index frame)))
    (when (and tracks (< idx (length tracks)))
      (nth idx tracks))))

;;; ---------------------------------------------------------------------------
;;; Initialization
;;; ---------------------------------------------------------------------------

(defmethod clim:run-frame-top-level :before ((frame playlisp-app) &key)
  "Load the initial playlist if a filepath was stored."
  (let ((path (frame-filepath frame)))
    (when (and path (probe-file path))
      (handler-case
          (let ((playlist (parse-m3u-file path)))
            (setf (frame-playlist frame) playlist
                  (frame-selected-index frame) 0))
        (error (e)
          (setf (frame-message frame)
                (format nil "Error loading: ~A" e)))))))
