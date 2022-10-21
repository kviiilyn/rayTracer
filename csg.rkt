#lang typed/racket

;; CMSC 15100 Winter 2022
;; Project 2
;; csg.rkt module
;; <YOUR NAME>
;;
;; This module implements the Camera abstraction
;;

;; load custom definitions
;;
(require "../include/cs151-core.rkt")

;; load image library definitions
;;
(require "../include/cs151-image.rkt")

;; project modules
;;
(require "math-util.rkt")
(require "hit.rkt")
(require "object.rkt")

(: union-hit-lists : Hit-List Hit-List -> Hit-List)
;; given two hit lists corresponding to two objects, compute the hit list for
;; the union of the two objects.  We discard hits that do not change the status
;; of the ray w.r.t. the combined object.  For example, if we are inside the second
;; object, then an entry hit for the first object can be discarded.  We determine
;; the status of the ray based on the parity of the next hit in the list for the
;; object.
(define (union-hit-lists hits1 hits2)
  (match* (hits1 hits2)
    [('() _) hits2]
    [(_ '()) hits1]
    [((cons h1 r1) (cons h2 r2))
     (if (hit< h1 h2)
         (local
           {(define hits : Hit-List (union-hit-lists r1 hits2))}
           (match* ((Hit-parity h1) (Hit-parity h2))
             [('IN 'IN) (cons h1 hits)]
             [('IN 'OUT) hits]
             [('OUT 'IN) (cons h1 hits)]
             [('OUT 'OUT) hits]))
         (local
           {(define hits : Hit-List (union-hit-lists hits1 r2))}
           (match* ((Hit-parity h1) (Hit-parity h2))
             [('IN 'IN) (cons h2 hits)]
             [('IN 'OUT) (cons h2 hits)]
             [('OUT 'IN) hits]
             [('OUT 'OUT) hits])))]))

(: object-union : Object Object -> Object)
;; return the union of two objects
(define (object-union obj1 obj2)
  (Object
   (lambda ([ray : Ray] [min-t : Float])
     (union-hit-lists (hit-test obj1 ray min-t) (hit-test obj2 ray min-t)))))

;; CODE FOR intersect-hit-lists GOES HERE
(: intersect-hit-lists : Hit-List Parity Hit-List Parity -> Hit-List)
;; given two hit lists corresponding to two objects and the most previous parities,
;; compute the hit list for the union of the two objects
(define (intersect-hit-lists hl1 p1 hl2 p2)
  (match* (hl1 hl2)
    [('() _)
     (if
      (symbol=? p1 'OUT)
      '()
      hl2)]
    [(_ '())
     (if
      (symbol=? p2 'OUT)
      '()
      hl1)]
    [((cons h1 r1) (cons h2 r2))
     (if (hit< h1 h2)
         (match* ((Hit-parity h1) (Hit-parity h2))
           [('IN 'IN) (intersect-hit-lists r1 'IN hl2 p2)]
           [('IN 'OUT) (cons h1 (intersect-hit-lists r1 'IN hl2 p2))]
           [('OUT 'IN) (intersect-hit-lists r1 'OUT hl2 p2)]
           [('OUT 'OUT) (cons h1 (intersect-hit-lists r1 'OUT hl2 p2))])
         (match* ((Hit-parity h2) (Hit-parity h1))
           [('IN 'IN) (intersect-hit-lists hl1 p1 r2 'IN)]
           [('IN 'OUT) (cons h2 (intersect-hit-lists hl1 p1 r2 'IN))]
           [('OUT 'IN) (intersect-hit-lists hl1 p1 r2 'OUT)]
           [('OUT 'OUT) (cons h2 (intersect-hit-lists hl1 p1 r2 'OUT))]))]))

;; CODE FOR object-intersect GOES HERE
(: object-intersect : Object Object -> Object)
(define (object-intersect obj1 obj2)
  (Object
   (lambda ([ray : Ray] [min-t : Float])
     (local
       {(define hl1 : Hit-List (hit-test obj1 ray min-t))
        (define p1 : Parity (match hl1 [(cons x xr) (Hit-parity (flip-parity x))]
                              ['() 'OUT]))
        (define hl2 : Hit-List (hit-test obj2 ray min-t))
        (define p2 : Parity (match hl2 [(cons x xr) (Hit-parity (flip-parity x))]
                              ['() 'OUT]))} 
       (intersect-hit-lists hl1 p1 hl2 p2)))))

;; CODE FOR subtract-hit-lists GOES HERE
(: subtract-hit-lists : Hit-List Parity Hit-List Parity -> Hit-List)
;; given two hit lists and their most previous parities corresponding to two objects,
;; compute the hit list for the first object subtracting the second
(define (subtract-hit-lists hl1 p1 hl2 p2)
  (match* (hl1 hl2)
    [('() '()) '()]
    [('() _)
     (if
      (symbol=? p1 'IN)
      (map flip-parity hl2)
      '())]
    [(_ '())
     (if
      (symbol=? p2 'OUT)
      hl1
      '())]
    [((cons h1 r1) (cons h2 r2))
     (if (hit< h1 h2)
         (match* ((Hit-parity h1) (Hit-parity h2))
           [('IN 'IN) (cons h1 (subtract-hit-lists r1 'IN hl2 p2))]
           [('IN 'OUT) (subtract-hit-lists r1 'IN hl2 p2)]
           [('OUT 'IN) (cons h1 (subtract-hit-lists r1 'OUT hl2 p2))]
           [('OUT 'OUT) (subtract-hit-lists r1 'OUT hl2 p2)])
         (match* ((Hit-parity h2) (Hit-parity h1))
           [('IN 'IN) (subtract-hit-lists hl1 p1 r2 'IN)]
           [('IN 'OUT) (cons (flip-parity h2) (subtract-hit-lists hl1 p1 r2 'IN))]
           [('OUT 'IN) (subtract-hit-lists hl1 p1 r2 'OUT)]
           [('OUT 'OUT) (cons (flip-parity h2) (subtract-hit-lists hl1 p1 r2 'OUT))]))]))

;; CODE FOR object-subtract GOES HERE
(: object-subtract : Object Object -> Object)
(define (object-subtract obj1 obj2)
  (Object
   (lambda ([ray : Ray] [min-t : Float])
     (local
       {(define hl1 : Hit-List (hit-test obj1 ray min-t))
        (define p1 : Parity (match hl1 [(cons x xr) (Hit-parity (flip-parity x))]
                              ['() 'OUT]))
        (define hl2 : Hit-List (hit-test obj2 ray min-t))
        (define p2 : Parity (match hl2 [(cons x xr) (Hit-parity (flip-parity x))]
                              ['() 'OUT]))} 
       (subtract-hit-lists hl1 p1 hl2 p2)))))

;;;;;;;;;;
;; Exports
;;;;;;;;;;

(provide object-union
         object-intersect
         object-subtract)
