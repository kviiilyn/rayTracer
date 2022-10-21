#lang typed/racket

;; CMSC 15100 Winter 2022
;; Project 2
;; trace.rkt module
;; Kaitlyn Li
;;
;; Ray casting and recursive ray tracing
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
(require "color.rkt")
(require "camera.rkt")
(require "material.rkt")
(require "object.rkt")
(require "hit.rkt")

;; A (Scene obj bg) packages up the description of a scene, where obj
;; is an object representing the geometric objects in the scene and
;; bg is a function that computes a background color based on a ray
(define-struct Scene
  ([world : Object]
   [background : (Ray -> RGB)]))

(: cast-ray-in-world : Scene -> Ray -> RGB)
;; ray caster for testing purposes
(define (cast-ray-in-world scene)
  (match scene
    [(Scene world ray->background)
     (lambda ([ray : Ray])
       (match (first-entry (hit-test world ray 0.001))
         [(Some hit)
          (match (get-reflection ray hit)
            ['None (ray->background ray)]
            [(Some (Reflect-Info rgb _)) rgb])]
         ['None (ray->background ray)]))]))

(: trace-ray-in-world : Scene Natural -> Ray -> RGB)
;; Given a world and a maximum tracing depth, this function returns
;; a function that will recursively trace a ray through the world
;; to compute a color
(define (trace-ray-in-world scene max-depth)
  (lambda ([r : Ray])
    (local
      {(define og-hl : Hit-List (hit-test (Scene-world scene) r 0.001))
       (define bg-fx : (Ray -> RGB) (Scene-background scene))}
      (match og-hl
        ['() (bg-fx r)]
        [(cons x xr)
         (local
           {(define first-hit : Hit x)
            (define first-emission : RGB (get-emission r x))
            (: triw-aux : Ray Hit-List Natural RGB -> RGB)
            (define (triw-aux r xs count acc)
              (if
               (= count 0)
               rgb-black
               (match xs
                 ['() (rgb* acc ((Scene-background scene) r))]
                 [(cons x xr)
                  (match (get-reflection r x)
                    ['None (get-emission r x)]
                    [(Some ri) (triw-aux (Reflect-Info-reflect-ray ri)
                                         (hit-test (Scene-world scene) (Reflect-Info-reflect-ray ri) 0.001)
                                         (- count 1)
                                         (rgb* acc (Reflect-Info-aten ri)))])])))}
           (rgb+ first-emission (triw-aux r og-hl max-depth (RGB 1.0 1.0 1.0))))]))))



;  (lambda ([r : Ray])
;    (local
;      {(: triw-aux : Ray Hit-List Natural RGB -> RGB)
;       (define (triw-aux r xs count acc)
;         (if
;          (= count 0)
;          acc
;          (match xs
;            ['() (rgb* acc ((Scene-background scene) r))]
;            [(cons x xr)
;             (match (get-reflection r x)
;               ['None (get-emission r x)]
;               [(Some ri) (triw-aux (Reflect-Info-reflect-ray ri)
;                                    (hit-test (Scene-world scene) (Reflect-Info-reflect-ray ri) 0.001)
;                                    (- count 1)
;                                    (rgb+ (get-emission r x) (rgb* acc (Reflect-Info-aten ri))))])])))}
;      (triw-aux r (hit-test (Scene-world scene) r 0.001) max-depth (RGB 1.0 1.0 1.0)))))


(: ray-tracer : Camera Scene Natural -> Image)
;; Given a camera, world object, and max depth, render a scene
;; using the given depth limit.
(define (ray-tracer cam scene n)
  (foreach-pixel
   cam
   (make-pixel-renderer
    (antialias-pixel->rgb
     cam
     (trace-ray-in-world scene n))
    gamma-rgb->color)))

;;;;;;;;;;
;; Exports
;;;;;;;;;;

(provide Scene)

(provide cast-ray-in-world
         trace-ray-in-world
         ray-tracer)
