;;;; pawn-dc.scm
;;;;
;;;; Copyright (c) Stewart J. Milberger, 2022
;;;; All rights reserved. No copying.
;;;;

;;; Setup for pawn dual contouring

(define save-cwd (path.cwd))
(path.cwd "../src/model")
(load "model.scm")
(path.cwd save-cwd)
(load "pawn.scm")

(define p (create-pawn-shape))
p
