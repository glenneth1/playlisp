;;;; package.lisp - Package definition for playlisp GUI

(defpackage #:playlisp-gui
  (:use #:cl)
  (:import-from #:playlisp/parser
                #:playlist
                #:playlist-name
                #:playlist-phase
                #:playlist-duration
                #:playlist-curator
                #:playlist-description
                #:playlist-elements
                #:track
                #:track-path
                #:title
                #:artist
                #:runtime
                #:parse-m3u-file
                #:make-playlist
                #:make-track)
  (:import-from #:playlisp/m3u-operations
                #:move-track-up
                #:move-track-down
                #:delete-track
                #:add-playlist-element
                #:write-m3u-file
                #:get-audio-duration
                #:make-track-from-file)
  (:export #:run
           #:main))
