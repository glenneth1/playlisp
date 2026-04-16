;;;; commands.lisp - McCLIM command definitions for playlisp GUI

(in-package #:playlisp-gui)

;;; ---------------------------------------------------------------------------
;;; Command tables — menus are populated at end of file
;;; ---------------------------------------------------------------------------

(clim:define-command-table file-menu)
(clim:define-command-table edit-menu)
(clim:define-command-table playlisp-commands
  :menu (("File" :menu file-menu)
         ("Edit" :menu edit-menu)))

;;; ---------------------------------------------------------------------------
;;; File commands (all in playlisp-commands for command-enabled lookup)
;;; ---------------------------------------------------------------------------

(clim:define-command (com-new :command-table playlisp-commands
                              :name "New Playlist"
                              :keystroke (#\n :control))
    ()
  "Create a new empty playlist."
  (let ((frame clim:*application-frame*))
    (setf (frame-playlist frame) (make-playlist "Untitled")
          (frame-filepath frame) nil
          (frame-selected-index frame) 0
          (frame-message frame) "New playlist created")))

(clim:define-command (com-open :command-table playlisp-commands
                               :name "Open"
                               :keystroke (#\o :control))
    ()
  "Open an M3U playlist file."
  (let ((frame clim:*application-frame*))
    (handler-case
        (let* ((path (clim:accept 'pathname
                                  :prompt "M3U file"
                                  :stream (clim:frame-standard-input frame)))
               (playlist (parse-m3u-file path)))
          (setf (frame-playlist frame) playlist
                (frame-filepath frame) (namestring (truename path))
                (frame-selected-index frame) 0
                (frame-message frame) (format nil "Loaded ~A" (file-namestring path)))
          (clim:redisplay-frame-panes frame :force-p t))
      (error (e)
        (setf (frame-message frame)
              (format nil "Error loading: ~A" e))))))

(clim:define-command (com-save :command-table playlisp-commands
                               :name "Save"
                               :keystroke (#\s :control))
    ()
  "Save the current playlist to its file."
  (let* ((frame clim:*application-frame*)
         (playlist (frame-playlist frame))
         (filepath (frame-filepath frame)))
    (cond
      ((null playlist)
       (setf (frame-message frame) "No playlist to save"))
      ((null filepath)
       (setf (frame-message frame) "No file path -- use Save As"))
      (t
       (handler-case
           (progn
             (write-m3u-file playlist filepath)
             (setf (frame-message frame) (format nil "Saved ~A" (file-namestring filepath))))
         (error (e)
           (setf (frame-message frame) (format nil "Save error: ~A" e))))))))

(clim:define-command (com-save-as :command-table playlisp-commands
                                  :name "Save As")
    ()
  "Save the current playlist to a new file."
  (let* ((frame clim:*application-frame*)
         (playlist (frame-playlist frame)))
    (if (null playlist)
        (setf (frame-message frame) "No playlist to save")
        (handler-case
            (let* ((path (clim:accept 'pathname
                                      :prompt "Save to"
                                      :stream (clim:frame-standard-input frame)))
                   (resolved (namestring (merge-pathnames path))))
              (write-m3u-file playlist resolved)
              (setf (frame-filepath frame) resolved
                    (frame-message frame) (format nil "Saved ~A" (file-namestring resolved))))
          (error (e)
            (setf (frame-message frame) (format nil "Save error: ~A" e)))))))

(clim:define-command (com-add-tracks :command-table playlisp-commands
                                     :name "Add Tracks"
                                     :keystroke (#\a :control))
    ()
  "Add audio files to the playlist. Accepts a single file or a directory."
  (let* ((frame clim:*application-frame*)
         (playlist (frame-playlist frame)))
    (unless playlist
      (setf playlist (make-playlist "Untitled")
            (frame-playlist frame) playlist))
    (handler-case
        (let* ((path (clim:accept 'pathname
                                  :prompt "Audio file or directory"
                                  :stream (clim:frame-standard-input frame)))
               (added 0))
          (if (uiop:directory-pathname-p path)
              ;; Add all audio files in directory
              (let ((files (sort (uiop:directory-files path)
                                 #'string< :key #'namestring)))
                (dolist (f files)
                  (when (audio-file-p f)
                    (let ((track (make-track-from-file (namestring f))))
                      (add-playlist-element playlist track)
                      (incf added)))))
              ;; Add single file
              (when (probe-file path)
                (let ((track (make-track-from-file (namestring path))))
                  (add-playlist-element playlist track)
                  (incf added))))
          (setf (frame-message frame)
                (format nil "Added ~D track~:P" added)))
      (error (e)
        (setf (frame-message frame)
              (format nil "Error adding tracks: ~A" e))))))

(clim:define-command (com-browse-add :command-table playlisp-commands
                                     :name "Browse & Add"
                                     :keystroke (#\b :control))
    ()
  "Browse the filesystem and add audio files to the playlist."
  (let* ((frame clim:*application-frame*)
         (stream (clim:frame-standard-input frame))
         (dir (or (when (frame-filepath frame)
                    (make-pathname :directory (pathname-directory
                                              (pathname (frame-filepath frame)))))
                  (merge-pathnames "Music/" (user-homedir-pathname))))
         (playlist (frame-playlist frame)))
    (unless playlist
      (setf playlist (make-playlist "Untitled")
            (frame-playlist frame) playlist))
    (loop
      (format stream "~&~%Directory: ~A~%" (namestring dir))
      (format stream "---~%")
      ;; List parent
      (format stream "  [..] Parent directory~%")
      ;; List subdirectories
      (let ((subdirs (sort (uiop:subdirectories dir)
                           #'string< :key #'namestring))
            (files (sort (remove-if-not #'audio-file-p (uiop:directory-files dir))
                         #'string< :key #'namestring))
            (choices nil)
            (idx 0))
        (dolist (d subdirs)
          (let ((name (first (last (pathname-directory d)))))
            (push (cons idx (cons :dir d)) choices)
            (format stream "  [~D] ~A/~%" idx name)
            (incf idx)))
        ;; List audio files
        (dolist (f files)
          (push (cons idx (cons :file f)) choices)
          (format stream "  [~D] ~A~%" idx (file-namestring f))
          (incf idx))
        (format stream "---~%")
        (format stream "  [a] Add ALL audio files from this directory~%")
        (format stream "  [q] Done browsing~%")
        (let ((input (clim:accept 'string
                                  :prompt "Choice"
                                  :stream stream)))
          (cond
            ((string-equal input "q") (return))
            ((string-equal input "..")
             (let ((parent (uiop:pathname-parent-directory-pathname dir)))
               (when parent (setf dir parent))))
            ((string-equal input "a")
             (let ((added 0))
               (dolist (f files)
                 (let ((track (make-track-from-file (namestring f))))
                   (add-playlist-element playlist track)
                   (incf added)))
               (setf (frame-message frame)
                     (format nil "Added ~D track~:P from ~A"
                             added (first (last (pathname-directory dir)))))))
            (t
             (let* ((num (ignore-errors (parse-integer input)))
                    (entry (when num (cdr (assoc num choices)))))
               (cond
                 ((null entry)
                  (format stream "  Invalid choice.~%"))
                 ((eq (car entry) :dir)
                  (setf dir (cdr entry)))
                 ((eq (car entry) :file)
                  (let ((track (make-track-from-file (namestring (cdr entry)))))
                    (add-playlist-element playlist track)
                    (setf (frame-message frame)
                          (format nil "Added: ~A" (file-namestring (cdr entry)))))))))))))))

(clim:define-command (com-quit :command-table playlisp-commands
                               :name "Quit"
                               :keystroke (#\q :control))
    ()
  "Quit playlisp."
  (clim:frame-exit clim:*application-frame*))

;;; ---------------------------------------------------------------------------
;;; Edit commands
;;; ---------------------------------------------------------------------------

(clim:define-command (com-toggle-edit :command-table playlisp-commands
                                      :name "Edit Mode"
                                      :keystroke (#\e :control))
    ()
  "Toggle edit mode for reordering/deleting tracks."
  (let ((frame clim:*application-frame*))
    (setf (frame-edit-mode-p frame) (not (frame-edit-mode-p frame)))
    (setf (frame-message frame)
          (if (frame-edit-mode-p frame)
              "Edit mode ON"
              "Edit mode OFF"))))

(clim:define-command (com-move-track-up :command-table playlisp-commands
                                        :name "Move Up"
                                        :keystroke (:up :control))
    ()
  "Move the selected track up in the playlist."
  (let* ((frame clim:*application-frame*)
         (playlist (frame-playlist frame))
         (idx (frame-selected-index frame)))
    (when (and playlist (plusp idx))
      (move-track-up playlist idx)
      (setf (frame-selected-index frame) (1- idx)))))

(clim:define-command (com-move-track-down :command-table playlisp-commands
                                          :name "Move Down"
                                          :keystroke (:down :control))
    ()
  "Move the selected track down in the playlist."
  (let* ((frame clim:*application-frame*)
         (playlist (frame-playlist frame))
         (idx (frame-selected-index frame))
         (count (frame-track-count frame)))
    (when (and playlist (< (1+ idx) count))
      (move-track-down playlist idx)
      (setf (frame-selected-index frame) (1+ idx)))))

(clim:define-command (com-delete-track :command-table playlisp-commands
                                       :name "Delete Track"
                                       :keystroke (:delete))
    ()
  "Delete the selected track from the playlist."
  (let* ((frame clim:*application-frame*)
         (playlist (frame-playlist frame))
         (idx (frame-selected-index frame))
         (count (frame-track-count frame)))
    (when (and playlist (plusp count))
      (let ((track (nth idx (playlist-elements playlist))))
        (delete-track track playlist)
        (setf (frame-message frame)
              (format nil "Removed track ~D" (1+ idx)))
        (when (>= idx (frame-track-count frame))
          (setf (frame-selected-index frame)
                (max 0 (1- (frame-track-count frame)))))))))

;;; ---------------------------------------------------------------------------
;;; Populate menus — commands are in playlisp-commands, menu items reference them
;;; ---------------------------------------------------------------------------

(clim:add-menu-item-to-command-table 'file-menu "New Playlist" :command '(com-new) :errorp nil)
(clim:add-menu-item-to-command-table 'file-menu "Open" :command '(com-open) :errorp nil)
(clim:add-menu-item-to-command-table 'file-menu "Save" :command '(com-save) :errorp nil)
(clim:add-menu-item-to-command-table 'file-menu "Save As" :command '(com-save-as) :errorp nil)
(clim:add-menu-item-to-command-table 'file-menu "Add Tracks" :command '(com-add-tracks) :errorp nil)
(clim:add-menu-item-to-command-table 'file-menu "Browse & Add" :command '(com-browse-add) :errorp nil)
(clim:add-menu-item-to-command-table 'file-menu "Quit" :command '(com-quit) :errorp nil)

(clim:add-menu-item-to-command-table 'edit-menu "Edit Mode" :command '(com-toggle-edit) :errorp nil)
(clim:add-menu-item-to-command-table 'edit-menu "Move Up" :command '(com-move-track-up) :errorp nil)
(clim:add-menu-item-to-command-table 'edit-menu "Move Down" :command '(com-move-track-down) :errorp nil)
(clim:add-menu-item-to-command-table 'edit-menu "Delete Track" :command '(com-delete-track) :errorp nil)

;;; ---------------------------------------------------------------------------
;;; Selection and navigation (not in menus, keyboard only)
;;; ---------------------------------------------------------------------------

(clim:define-command (com-select-track :command-table playlisp-commands
                                       :name "Select Track")
    ((track 'playlisp-track :gesture :select))
  "Select a track by clicking on it."
  (let* ((frame clim:*application-frame*)
         (tracks (when (frame-playlist frame)
                   (playlist-elements (frame-playlist frame))))
         (pos (position track tracks :test #'eq)))
    (when pos
      (setf (frame-selected-index frame) pos))))

(clim:define-command (com-next-track :command-table playlisp-commands
                                     :name "Next Track"
                                     :keystroke (:down))
    ()
  "Move selection to the next track."
  (let* ((frame clim:*application-frame*)
         (count (frame-track-count frame))
         (idx (frame-selected-index frame)))
    (when (< (1+ idx) count)
      (setf (frame-selected-index frame) (1+ idx)))))

(clim:define-command (com-prev-track :command-table playlisp-commands
                                     :name "Previous Track"
                                     :keystroke (:up))
    ()
  "Move selection to the previous track."
  (let* ((frame clim:*application-frame*)
         (idx (frame-selected-index frame)))
    (when (plusp idx)
      (setf (frame-selected-index frame) (1- idx)))))

(clim:define-command (com-first-track :command-table playlisp-commands
                                      :name "First Track"
                                      :keystroke (:home))
    ()
  "Jump to the first track."
  (setf (frame-selected-index clim:*application-frame*) 0))

(clim:define-command (com-last-track :command-table playlisp-commands
                                     :name "Last Track"
                                     :keystroke (:end))
    ()
  "Jump to the last track."
  (let* ((frame clim:*application-frame*)
         (count (frame-track-count frame)))
    (when (plusp count)
      (setf (frame-selected-index frame) (1- count)))))

;;; ---------------------------------------------------------------------------
;;; Drag-and-drop reordering
;;; ---------------------------------------------------------------------------

(defun drag-track-feedback (frame presentation stream x0 y0 x y state)
  "Visual feedback during track drag."
  (declare (ignore frame presentation x0 y0 state))
  (clim:with-output-recording-options (stream :record nil :draw t)
    (clim:draw-rectangle* stream (- x 2) (- y 2) (+ x 200) (+ y 16)
                          :ink clim:+flipping-ink+
                          :filled nil)))

(clim:define-command (com-reorder-track :command-table playlisp-commands
                                        :name nil)
    ((from-idx 'integer) (to-idx 'integer))
  "Reorder a track from one position to another."
  (let* ((frame clim:*application-frame*)
         (playlist (frame-playlist frame)))
    (when (and playlist (/= from-idx to-idx))
      (cond
        ((< from-idx to-idx)
         (loop for i from from-idx below to-idx
               do (move-track-down playlist i)))
        ((> from-idx to-idx)
         (loop for i from from-idx above to-idx
               do (move-track-up playlist i))))
      (setf (frame-selected-index frame) to-idx
            (frame-message frame)
            (format nil "Moved track ~D to position ~D"
                    (1+ from-idx) (1+ to-idx))))))

(clim:define-drag-and-drop-translator drag-reorder-track
    (playlisp-track clim:command playlisp-track playlisp-commands
     :feedback drag-track-feedback)
    (object destination-object)
  "Drag a track and drop it on another track to reorder."
  (let* ((frame clim:*application-frame*)
         (tracks (when (frame-playlist frame)
                   (playlist-elements (frame-playlist frame))))
         (from-pos (position object tracks :test #'eq))
         (to-pos (position destination-object tracks :test #'eq)))
    (when (and from-pos to-pos)
      `(com-reorder-track ,from-pos ,to-pos))))
