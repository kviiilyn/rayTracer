#lang typed/racket

;; CMSC 15100 Winter 2022
;; Project 2
;; plane.rkt module
;; <YOUR NAME>
;;
;; This module contains the implementation of the plane object
;;

;; load custom definitions
;;
(require "../include/cs151-core.rkt")

;; load image library definitions
;;
(require "../include/cs151-image.rkt")

;; load testing infrastructure
;;
(require typed/test-engine/racket-tests)

;; project modules
;;
(require "util.rkt")
(require "math-util.rkt")
(require "material.rkt")
(require "hit.rkt")
(require "object.rkt")

;; CODE FOR make-plane GOES HERE
(: make-plane : Float3 Float3 Material -> Object)
;; (make-plane pt perp material) makes a plane object.  The plane
;; contains the point pt and its orientation is defined by the
;; vector perp, which is perpendicular to the plane.  The third
;; argument specifies the plane's surface material.
;; Note that the perpendicular vector does not have to be unit length.
(define (make-plane pt perp material)
  (local
    {(: hit-test : Ray Float -> Hit-List)
     (define (hit-test r f)
       (local
         {(define Q : Float3 pt)
          (define P : Float3 (Ray-origin r))
          (define N : Float3 (fl3-normalize perp))
          (define D : Float3 (Ray-dir r))
          (define DN : Float (fl3-dot D N))}
         (if
          (< (abs DN) 0.0001)
          miss
          (local
            {(define numerator : Float (fl3-dot (fl3- Q P) N))
             (define t : Float (/ numerator DN))}
            (if
             (> f t)
             (if
              (< DN 0)
              miss
              (list (Hit 'OUT +inf.0 fl3-zero fl3-zero flat-black)))
             (if
              (< numerator 0)
              (list (Hit 'IN t (fl3-scale+ P t D) N material)
                    (Hit 'OUT +inf.0 fl3-zero fl3-zero flat-black))
              (list (Hit 'OUT t (fl3-scale+ P t D) N material))))))))}
    (Object hit-test)))

;;;;;;;;;;
;; Exports
;;;;;;;;;;

(provide make-plane)
