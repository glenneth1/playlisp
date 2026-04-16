;;;; display.lisp - Display functions for playlisp GUI panes

(in-package #:playlisp-gui)

;;; ---------------------------------------------------------------------------
;;; Colors
;;; ---------------------------------------------------------------------------

(defparameter *selected-bg* (clim:make-rgb-color 0.15 0.25 0.45)
  "Background color for the selected track row.")

(defparameter *playing-fg* (clim:make-rgb-color 0.3 0.85 0.65)
  "Foreground color for the currently playing indicator.")

(defparameter *dim-fg* (clim:make-rgb-color 0.5 0.5 0.6)
  "Foreground color for secondary text (duration, artist).")

(defparameter *header-fg* (clim:make-rgb-color 0.7 0.8 0.95)
  "Foreground color for status bar text.")

(defparameter *detail-label-fg* (clim:make-rgb-color 0.6 0.7 0.8)
  "Foreground color for detail pane labels.")

(defparameter *index-fg* (clim:make-rgb-color 0.45 0.45 0.55)
  "Foreground color for track index numbers.")

(defparameter *edit-mode-fg* (clim:make-rgb-color 0.95 0.75 0.2)
  "Foreground color for edit mode indicator.")

;;; ---------------------------------------------------------------------------
;;; Audio file detection
;;; ---------------------------------------------------------------------------

(defparameter *audio-extensions*
  '("mp3" "flac" "ogg" "opus" "m4a" "wav" "aac" "wma")
  "Recognized audio file extensions.")

(defun audio-file-p (path)
  "Return T if PATH has a recognized audio file extension."
  (let ((ext (pathname-type path)))
    (and ext (member ext *audio-extensions* :test #'string-equal))))

;;; ---------------------------------------------------------------------------
;;; Helpers
;;; ---------------------------------------------------------------------------

(defun format-duration (seconds)
  "Format duration in seconds as MM:SS or HH:MM:SS."
  (if (or (null seconds) (< seconds 0))
      "--:--"
      (let* ((s (round seconds))
             (h (floor s 3600))
             (m (floor (mod s 3600) 60))
             (sec (mod s 60)))
        (if (> h 0)
            (format nil "~D:~2,'0D:~2,'0D" h m sec)
            (format nil "~D:~2,'0D" m sec)))))

(defun total-duration (tracks)
  "Sum the runtime of all TRACKS, skipping nil runtimes."
  (loop for trk in tracks
        when (runtime trk)
          sum (runtime trk)))

(defun truncate-string (str max-len)
  "Truncate STR to MAX-LEN characters, appending ... if needed."
  (if (<= (length str) max-len)
      str
      (concatenate 'string (subseq str 0 (max 0 (- max-len 3))) "...")))

;;; ---------------------------------------------------------------------------
;;; Track list display
;;; ---------------------------------------------------------------------------

(defun display-tracklist (frame pane)
  "Display the scrollable playlist as CLIM presentations."
  (let* ((playlist (frame-playlist frame))
         (tracks (when playlist (playlist-elements playlist)))
         (selected-idx (frame-selected-index frame))
         (edit-p (frame-edit-mode-p frame)))
    (cond
      ((null playlist)
       (clim:with-text-face (pane :italic)
         (clim:with-drawing-options (pane :ink *dim-fg*)
           (format pane "~%  No playlist loaded.~%")
           (format pane "  Use  Open  (Ctrl+O) to load an M3U file.~%"))))
      ((null tracks)
       (clim:with-text-face (pane :italic)
         (clim:with-drawing-options (pane :ink *dim-fg*)
           (format pane "~%  Playlist is empty.~%"))))
      (t
       (when edit-p
         (clim:with-drawing-options (pane :ink *edit-mode-fg*)
           (clim:with-text-face (pane :bold)
             (format pane " [EDIT MODE]  "))
           (clim:with-drawing-options (pane :ink *dim-fg*)
             (format pane "Up/Down=select  Ctrl+Up/Down=move  Del=remove  Esc=exit~%"))))
       (loop for trk in tracks
             for idx from 0
             for selected-p = (= idx selected-idx)
             do (display-track-row pane trk idx selected-p edit-p))))))

(defun display-track-row (pane track index selected-p edit-p)
  "Display a single track row as a CLIM presentation."
  (let ((name (or (title track)
                  (file-namestring (track-path track))
                  "Unknown"))
        (the-artist (or (artist track) ""))
        (dur (format-duration (runtime track))))
    ;; Wrap entire row in a presentation for click-to-select
    (clim:with-output-as-presentation (pane track 'playlisp-track
                                            :single-box t)
      ;; Track number
      (clim:with-drawing-options (pane :ink *index-fg*)
        (format pane " ~3D  " (1+ index)))
      ;; Selection indicator + title
      (if selected-p
          (progn
            (clim:with-drawing-options (pane :ink *playing-fg*)
              (format pane "▶ "))
            (clim:with-text-face (pane :bold)
              (format pane "~A" name)))
          (progn
            (format pane "  ")
            (format pane "~A" name)))
      ;; Artist
      (when (plusp (length the-artist))
        (clim:with-drawing-options (pane :ink *dim-fg*)
          (format pane "  ~A" the-artist)))
      ;; Duration
      (clim:with-drawing-options (pane :ink *dim-fg*)
        (format pane "  [~A]" dur))
      ;; Edit mode marker
      (when (and edit-p selected-p)
        (clim:with-drawing-options (pane :ink *edit-mode-fg*)
          (format pane "  ◆"))))
    (terpri pane)))

;;; ---------------------------------------------------------------------------
;;; Detail pane display
;;; ---------------------------------------------------------------------------

(defun display-details (frame pane)
  "Display details for the currently selected track."
  (let ((track (frame-selected-track frame)))
    (if (null track)
        (clim:with-text-face (pane :italic)
          (clim:with-drawing-options (pane :ink *dim-fg*)
            (format pane "~%  Select a track to view details.~%")))
        (display-track-detail pane track))))

(defun display-track-detail (pane track)
  "Render full details for a single track."
  (let ((name (or (title track) "Unknown"))
        (the-artist (or (artist track) ""))
        (dur (format-duration (runtime track)))
        (path (track-path track)))
    (terpri pane)
    (clim:with-text-face (pane :bold)
      (clim:with-text-size (pane :large)
        (format pane "  ~A~%" name)))
    (terpri pane)
    (when (plusp (length the-artist))
      (%detail-field pane "Artist" the-artist))
    (%detail-field pane "Duration" dur)
    (when path
      (terpri pane)
      (clim:with-drawing-options (pane :ink *detail-label-fg*)
        (format pane "  Path~%"))
      (format pane "  ~A~%" (namestring path)))))

(defun %detail-field (pane label value)
  "Render a single label: value line in the detail pane."
  (when value
    (clim:with-drawing-options (pane :ink *detail-label-fg*)
      (format pane "  ~12A" label))
    (format pane " ~A~%" value)))

;;; ---------------------------------------------------------------------------
;;; Status bar display
;;; ---------------------------------------------------------------------------

(defun display-status (frame pane)
  "Display the status/header bar."
  (let* ((playlist (frame-playlist frame))
         (filepath (frame-filepath frame))
         (tracks (when playlist (playlist-elements playlist)))
         (count (length tracks))
         (dur (when tracks (total-duration tracks)))
         (msg (frame-message frame)))
    (clim:with-text-face (pane :bold)
      (clim:with-drawing-options (pane :ink *header-fg*)
        (format pane " ♫ Playlisp")))
    (when filepath
      (clim:with-drawing-options (pane :ink *dim-fg*)
        (format pane "  |  "))
      (format pane "~A" (file-namestring filepath)))
    (when (plusp count)
      (clim:with-drawing-options (pane :ink *dim-fg*)
        (format pane "  |  ~D track~:P  |  ~A" count (format-duration dur))))
    (when msg
      (clim:with-drawing-options (pane :ink *dim-fg*)
        (format pane "  |  "))
      (format pane "~A" msg))))
