(asdf:defsystem :game-of-life
  :description "A basic Game of Life simulator"
  :author "Joram Schrijver <i@joram.io>"
  :depends-on (#:iterate #:cl-opengl #:lispbuilder-sdl)
  :components ((:file "engine")
               (:file "game")))
