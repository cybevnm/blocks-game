(asdf:defsystem "blocks-game"
  :serial t
  :depends-on (#:lispbuilder-sdl
               #:trivial-features
               #:osicat)
  :components ((:file "blocks-game")))
                
