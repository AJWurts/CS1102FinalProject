;; Alexander Wurts
;; Program redraws the screen after every command
(require test-engine/racket-gui)
(require "world-cs1102.rkt")
;; a shape is (make-shape symbol posn color)
;; acceptable shape types are 'triangle, 'ellipse, and 'rectangle
(define-struct shape (type posn color width height ID))

;; a delta is (make-delta x y)
(define-struct delta (x y))

;; an animation is (make-animation number number list[cmd])
(define-struct animation (width height cmds))

;; a cmd is either
;; - (make-dd-cmd shape)
;; - (make-delete-shape-cmd shape)
;; - (make-move-shape-cmd shape)
;; - (make-jump-shape-cmd shape)
;; - (make-init-shapes-cmd list[shape])
;; - (define-struct cmds-until-xy-cond (shp1 minx miny maxx maxy cmd))
(define-struct add-shape-cmd (shape))
(define-struct delete-shape-cmd (shape))
(define-struct move-shape-cmd (shape delta))
(define-struct jump-shape-cmd (shape))
(define-struct init-shapes-cmd (los))
(define-struct until-xy-cond-cmd (shape minx miny maxx maxy cmds))

;;All examples written assuming that the shapes are placed in the correct location based on the original pictures, even though they are not
(define ANIMATION1
  (let [(circle (make-shape 'ellipse (make-posn 0 100) 'red 20 20 1))
        (wall1 (make-shape 'rectangle (make-posn 300 100) 'blue 300 100 2))
        (wall2 (make-shape 'rectangle (make-posn 100 300) 'green 100 300 3))]
  (make-animation 400 400
                  (list
                   (make-init-shapes-cmd (list circle wall1))
                   (make-until-xy-cond-cmd circle 95 'na 'na 'na
                                            (list
                                             (make-move-shape-cmd circle (make-delta 5 0))))
                   (make-delete-shape-cmd wall1)
                   (make-until-xy-cond-cmd circle 'na 'na 0 'na
                                            (list
                                             (make-move-shape-cmd circle (make-delta -3 1)))))
                   )))

(define ANIMATION2
  (let [(circle (make-shape 'ellipse (make-posn 0 100) 'red 20 20 1))
        (wall1 (make-shape 'rectangle (make-posn 300 100) 'blue 300 100 2))
        (wall2 (make-shape 'rectangle (make-posn 100 300) 'green 100 300 3))]
    (make-animation 400 400
                    (list
                     (make-init-shapes-cmd (list circle))
                     (make-until-xy-cond-cmd circle 0 0 0 0
                                         (list
                                          (make-jump-shape-cmd circle)))))))

;(define ANIMATION3
;  (let [(circle (make-ball (make-posn 100 100)))
;        (wall1 (make-wall (make-posn 300 100) 300 100))
;        (wall2 (make-wall (make-posn 100 300) 100 300))]
;    (make-animation 400 400
;                    (list
;                     (make-init-shapes-cmd (list circle wall1))
;                     (make-move-until-collide circle (make-delta 0 4))
;                     (make-move-until-collide circle (make-delta 4 0))
;                     (make-display-shape-cmd wall2)
;                     (make-move-until-collide circle (make-delta -2 -2 ))))))
                    


;; Interpreter

(define WIDTH 400)
(define HEIGHT 400)
(create-canvas WIDTH HEIGHT)

(define shapes empty)

;; A var is (make-var ID value)
(define-struct var (ID value))

;; init-vars: list[values] -> void
(define (init-vars a-list)
  (set! shapes (map (lambda (shpe) (make-var (shape-ID shpe) shpe)) a-list)))

;; lookup-shape-by-id: id -> value
(define (lookup-var-by-id id)
  (var-value (first (filter (lambda (item) (= (var-ID item) id)) shapes))))
 
;; lookup-shape: shape -> shape
 (define (lookup-shape a-shape)
   (var-value (first (filter (lambda (a-var) (= (var-ID a-var) (shape-ID a-shape))) shapes))))

;; delete-shape: shape -> void
(define (delete-shape a-shape)
  (set! shapes (filter (lambda (a-var) (not (= (var-ID a-var) (shape-ID a-shape))))
                       shapes)))
 

;; add-shape: shape -> void
(define (add-shape shape)
  (set! shapes (append shapes (list (make-var (shape-ID shape) shape)))))
  
;;run-animation: animation -> void
(define (run-animation anim)
  (run-cmds (animation-cmds anim)))

;;run-cmds: list[command] -> void
(define (run-cmds loc)
  (cond [(empty? loc) (void)]
        [(cons? loc)
         (begin
         (run-cmd (first loc))
         (show-all)
         (sleep/yield 0.25)
         (run-cmds (rest loc)))]))

;;run-cmd: cmd -> void
(define (run-cmd cmd)
  (cond [(add-shape-cmd? cmd) (add-shape (add-shape-cmd-shape cmd))]
        [(delete-shape-cmd? cmd) (delete-shape (delete-shape-cmd-shape cmd))]
        [(move-shape-cmd? cmd) (move-shape (move-shape-cmd-shape cmd) (move-shape-cmd-delta cmd))]
        [(jump-shape-cmd? cmd) (jump-shape (jump-shape-cmd-shape cmd))]
        [(init-shapes-cmd? cmd) (init-vars (init-shapes-cmd-los cmd))]
        [(until-xy-cond-cmd? cmd) (until-xy-cond (until-xy-cond-cmd-shape cmd) cmd)]))




;;move-shape: shape delta -> void
(define (move-shape a-shape a-delta)
  (edit-shape-posn (lookup-shape a-shape) (delta-x a-delta) (delta-y a-delta)))

;;jump-shape: shape -> void
(define (jump-shape a-shape)
  (edit-shape-posn a-shape (random WIDTH) (random HEIGHT)))

;;edit-shape-posn: shape number number -> void
(define (edit-shape-posn a-shape dx dy)
    (delete-shape a-shape)
    (add-shape (make-shape (shape-type a-shape)
                           (let ([x (posn-x (shape-posn a-shape))]
                                 [y (posn-y (shape-posn a-shape))])
                             (make-posn (+ x dx) (+ y dy)))
                           (shape-color a-shape)
                           (shape-width a-shape)
                           (shape-height a-shape)
                           (shape-ID a-shape))))

;;until-xy-cond: shape cmd -> void
(define (until-xy-cond a-shape cmd)
  (let* ([nshape (lookup-shape a-shape)]
        [shx (posn-x (shape-posn nshape))]
        [shy (posn-y (shape-posn nshape))]
        [minx (range-interp (until-xy-cond-cmd-minx cmd) 'low)]
        [miny (range-interp (until-xy-cond-cmd-miny cmd) 'low)]
        [maxx (range-interp (until-xy-cond-cmd-maxx cmd) 'high)]
        [maxy (range-interp (until-xy-cond-cmd-maxy cmd) 'high)])
 (cond [(or (and (and (>= shx minx) (<= shx maxx)) (and (>= shy miny) (<= shy maxy)))
            (or (or (< shx 0) (> shx WIDTH)) (or (< shy 0) (> shy HEIGHT))))
        (void)]
       [else
        (run-cmds (until-xy-cond-cmd-cmds cmd))
        (until-xy-cond a-shape cmd)])))

;;range-interp: value -> number
(define (range-interp val type)
  (cond [(symbol? val) (if (symbol=? type 'low)
                           -10000
                           10000)]
        [else val]))
        

;;show-all: -> void
(define (show-all)
  (update-frame (show-shapes (map (lambda (item) (cond [(shape? (var-value item))
                                                        (var-value item)]
                                                       [else empty])) shapes))))

;;show-shapes: list[shape] -> scene
(define (show-shapes los)
  (cond [(empty? los) (empty-scene WIDTH HEIGHT)]
        [(cons? los)
         (show-shape (first los) (show-shapes (rest los)))]))

;;show-shape: shape scene -> scene
(define (show-shape shape scene)
  (let ([nshape (lookup-shape shape)])
  (place-image (make-shape-img nshape)
               (posn-x (shape-posn nshape)) (posn-y (shape-posn nshape))
               scene)))

;;make-shape-img: shape -> image
(define (make-shape-img shape)
  (let [(wid (shape-width shape))
        (hei (shape-height shape))
        (col (shape-color shape))]
  (cond [(symbol=? 'triangle (shape-type shape))
         (isosceles-triangle (sqrt (+ (expt hei 2) (expt (* .5 wid) 2)))
                             (* 2 (tan (/ (* .5 (shape-width shape)) (shape-height))))
                             'solid col)]
        [(symbol=? 'ellipse (shape-type shape))
         (ellipse wid hei 'solid col)]
        [(symbol=? 'rectangle (shape-type shape))
         (rectangle wid hei 'solid col)])))

  