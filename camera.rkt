#lang typed/racket

;; CMSC 15100 Winter 2022
;; Project 2
;; camera.rkt module
;; Kaitlyn Li
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
(require "color.rkt")

;; the representation of a Camera for the ray tracer.
(define-struct Camera
  [(wid : Natural)           ;; width of image
   (ht : Natural)            ;; height of image
   (n-samples : Natural)     ;; number of samples per pixel
   (origin : Float3)         ;; where the camera is located
   (ulc : Float3)            ;; upper-left-corner of image plane
   (h-vec : Float3)          ;; horizontal pixel-wide vector parallel to image
                             ;; pointing right
   (v-vec : Float3)])        ;; vertical pixel-wide vector parallel to image
                             ;; pointing down

(: simple-camera : Natural Natural Natural Float -> Camera)
;; make a camera that is equivalent to a Project 1 camera (for
;; testing purposes)
(define (simple-camera wid ht ns flen)
  (local
    {(define pw : Float (/ 2.0 (->fl wid)))}
    (Camera wid ht ns
            fl3-zero
            (Float3 -1.0
                    (/ (->fl ht) (->fl wid))
                    (- flen))
            (Float3 pw 0.0 0.0)
            (Float3 0.0 (- pw) 0.0))))

;; CODE FOR make-camera HERE
(: make-camera : Natural Natural Natural Float3 Float3 Float3 Float -> Camera)
;; make a camera.  The arguments are (in order):
;;   - width of image
;;   - height of image
;;   - number of samples per pixel
;;   - origin of camera in the world
;;   - point that the camera is looking at
;;   - up vector
;;   - horizontal field of view (in degrees)
(define (make-camera wid ht ns pos look-at up fov)
  (local
    {(define dcam : Float3 (fl3-normalize (fl3- look-at pos)))
     (define r : Float3 (fl3-normalize (fl3-cross dcam up)))
     (define u- : Float3 (fl3-normalize (fl3-cross r dcam)))
     (define pw : Float (/ 2 (->fl wid)))
     (define h : Float3 (fl3-scale pw r))
     (define v : Float3 (fl3-scale (* -1 pw) u-))
     (define theta : Float (/ fov 2))
     (define flen : Float (/ 1 (tan (degrees->radians theta))))
     (define center : Float3 (fl3+ pos (fl3-scale flen dcam)))
     (define ulc : Float3 (fl3- (fl3+ center (fl3-scale (/ (->fl ht) (->fl wid)) u-)) r))}
    (Camera wid ht ns pos ulc h v)))


;; A Pixel-Renderer is a function that takes the row and column of a pixel
;; and produces a Racket Image-Library Color
(define-type Pixel-Renderer (Natural Natural -> Color))

(: foreach-pixel : Camera Pixel-Renderer -> Image)
;; given a camera and a pixel renderer, generate an image.
;;
(define (foreach-pixel cam pixel-renderer)
  (match cam
    [(Camera wid ht _ _ _ _ _)
     (if (or (= wid 0) (= ht 0))
         empty-image
         (local
           {(: for-rows : Natural (Listof Color) -> (Listof Color))
            ;; iterate over the rows of the image from bottom to top
            (define (for-rows row pixels)
              (if (= 0 row)
                  pixels
                  (for-cols (- row 1) wid pixels)))
            (: for-cols :  Natural Natural (Listof Color) -> (Listof Color))
            ;; iterate over the columns of a row from right to left
            (define (for-cols row col pixels)
              (if (= 0 col)
                  (for-rows row pixels)
                  (for-cols
                   row
                   (- col 1)
                   (cons (pixel-renderer row (- col 1)) pixels))))}
           (color-list->bitmap
            (for-rows ht '())
            wid ht)))]))

(: make-pixel-renderer : (Natural Natural -> RGB) (RGB -> Color) -> Pixel-Renderer)
;; compose a function that maps pixel coordinates to RGB values with
;; an RGB to Image-Library Color converter
(define (make-pixel-renderer pixel->rgb rgb->color)
  (lambda ([row : Natural] [col : Natural]) (rgb->color (pixel->rgb row col))))

;; CODE FOR ray-for-pixel GOES HERE
(: ray-for-pixel : Camera -> Natural Natural -> Ray)
;; returns a function for generating a ray for a pixel
;; specified by its row and column
(define (ray-for-pixel cam)
  (local
    {(define ulc : Float3 (Camera-ulc cam))
     (define h-vec : Float3 (Camera-h-vec cam))
     (define v-vec : Float3 (Camera-v-vec cam))
     (define ulc-center : Float3 (fl3+ (fl3+ ulc (fl3-scale 0.5 h-vec)) (fl3-scale 0.5 v-vec)))
     (define cam-ori : Float3 (Camera-origin cam))}
    (lambda ([row : Natural] [col : Natural])
      (local
        {(define center : Float3 (fl3+ (fl3+ ulc (fl3-scale (->fl row) v-vec))
                                       (fl3-scale (->fl col) h-vec)))}
      (make-ray
       cam-ori
       (fl3- center cam-ori))))))

;; CODE FOR rays-for-pixel GOES HERE
(: rays-for-pixel : Camera -> (Natural Natural -> (Listof Ray)))
;; given a camera and the row and column of a pixel,
;; return a list of random rays through that pixel
;; number of rays = n-sample of camera
(define (rays-for-pixel cam)
  (local
    {(define ulc : Float3 (Camera-ulc cam))
     (define h-vec : Float3 (Camera-h-vec cam))
     (define v-vec : Float3 (Camera-v-vec cam))
     (define cam-ori : Float3 (Camera-origin cam))
     (define samples : Natural (Camera-n-samples cam))}
    (lambda [(r : Natural) (c : Natural)]
      (local
        {(: rays-aux : Natural -> (Listof Ray))
         (define (rays-aux n)
           (local
             {(define ran-point : Float3 (fl3+ ulc
                                               (fl3+
                                                (fl3-scale (+ r (random)) v-vec)
                                                (fl3-scale (+ c (random)) h-vec))))}
             (if
              (= n samples)
              '()
              (cons
               (make-ray
                cam-ori
                (fl3- ran-point cam-ori))
               (rays-aux (+ n 1))))))}
        (rays-aux 0)))))

;; CODE FOR pixel->rgb GOES HERE
(: pixel->rgb : Camera (Ray -> RGB) -> Natural Natural -> RGB)
;; given a camera and a ray-tracing function, return a function that
;; traces the ray for the given pixel
(define (pixel->rgb cam ray->rgb)
  (local {(define ray->pixel : (Natural Natural -> Ray) (ray-for-pixel cam))}
    (lambda [(row : Natural) (col : Natural)]
      (ray->rgb (ray->pixel row col)))))


;; CODE FOR antialias-pixel->rgb GOES HERE
(: antialias-pixel->rgb : Camera (Ray -> RGB) -> Natural Natural -> RGB)
;; given a camera and a ray-tracing function, return a function that
;; traces a list of rays for the given pixel and returns their average
(define (antialias-pixel->rgb cam r->rgb)
  (local
    {(define s-fac : Float (if (= (Camera-n-samples cam) 0)
                               1.0
                               (/ 1 (->fl (Camera-n-samples cam)))))}
    (lambda [(r : Natural) (c : Natural)]
      (local
         {(: rlist->rgb : (Listof Ray) RGB -> RGB)
         (define (rlist->rgb xs acc)
           (match xs
             ['() acc]
             [(cons x xr)
              (local
                {(define r-rgb : RGB (r->rgb x))}
                (rlist->rgb xr (RGB (+ (RGB-r acc) (* (RGB-r r-rgb) s-fac))
                                    (+ (RGB-g acc) (* (RGB-g r-rgb) s-fac))
                                    (+ (RGB-b acc) (* (RGB-b r-rgb) s-fac)))))]))}
      (rlist->rgb ((rays-for-pixel cam) r c) (RGB 0.0 0.0 0.0))))))


(: ray->rgb : Ray -> RGB)
;; a function for testing ray generation.  It maps a ray to a color in
;; the white-to-blue range based on the Y component of the ray's direction
;; vector.
(define (ray->rgb ray)
  (match ray
    [(Ray _ dir)
     (local
       {(define t : Float (* 0.5 (+ 1.0 (Float3-y dir))))}
       (rgb-lerp rgb-white t (RGB 0.5 0.7 1.0)))]))

;;;;;;;;;;
;; Exports
;;;;;;;;;;

(provide Camera)

(provide make-camera
         simple-camera
         foreach-pixel
         make-pixel-renderer
         pixel->rgb
         antialias-pixel->rgb
         ray->rgb)
