(pushnew "/home/cybevnm/proj/lisp/lispbuilder/lispbuilder-sdl/" asdf:*central-registry* :test #'equal)
(pushnew "/home/cybevnm/proj/lisp/lispbuilder/lispbuilder-sdl-mixer/" asdf:*central-registry* :test #'equal)

(asdf:oos 'asdf:load-op 'lispbuilder-sdl)
(asdf:oos 'asdf:load-op 'lispbuilder-sdl-examples)

(load "packages.lisp")
(load "blocks.lisp")