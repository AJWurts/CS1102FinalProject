;; Alexander Wurts
;; Program redraws the screen after every command
(require test-engine/racket-gui)
(require "world-cs1102.rkt")
(require test-engine/racket-tests)
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
(define-struct until-collision-cond-cmd (shp cmds))
(define-struct show-cmd ())

(define-syntax until
  (syntax-rules ()
    [(until-xy shp minx miny maxx maxy
               cmds ...)
     (make-until-xy-cond-cmd shp minx miny maxx maxy
                             (list cmds ...))]))

(define-syntax add
  (syntax-rules ()
    [(add-shape shape)
     (make-add-shape-cmd shape)]))

(define-syntax delete
  (syntax-rules ()
    [(delete-shape shape)
     (make-delete-shape-cmd shape)]))

(define-syntax move
  (syntax-rules (dx: dy:)
    [(move-shape shape dx: dx dy: dy)
     (make-move-shape-cmd shape (make-delta dx dy))]))

(define-syntax jump
  (syntax-rules ()
    [(jump-shape shape)
     (make-jump-shape-cmd shape)]))

(define-syntax init
  (syntax-rules ()
    [(init shape ...)
     (make-init-shapes-cmd (list shape ...))]))

(define-syntax show
  (syntax-rules ()
    [(show)
     (make-show-cmd)]))

(define-syntax collision
  (syntax-rules ()
    [(collision shape1 cmds ...)
     (make-until-collision-cond-cmd shape1 (list cmds ...))]))


(define-syntax animation
  (syntax-rules (: -> pos size)
    [(animation twid thei
                (shapes (sname ID -> type pos(xpos : ypos) size(swidth : sheight) col)
                        ...)
                (commands (cmdname rest ...) ...))
     (let [(sname (make-shape 'type (make-posn xpos ypos) 'col swidth sheight ID)) ...]
       (make-animation twid thei
                       (list
                        (cmdname rest ... ) ...)))]))



;;All examples written assuming that the shapes are placed in the correct location based on the original pictures, even though they are not
(define ANIMATION1
  (animation 400 400
             (shapes (circle 1 -> ellipse pos(50 : 100) size(20 : 20) red)
                     (wall 2 -> rectangle pos(300 : 100) size(300 : 100) blue))
             (commands
              (init circle wall)
              (show)
              (collision circle 
                     (move circle dx: 5 dy: 0)
                     (show))
              (show)
              (delete wall)
              (show)
              (collision circle
                     (move circle dx: -5 dy: 1)
                     (show)))))


(define ANIMATION2
  (animation 400 400
             (shapes (circle 1 -> ellipse pos(50 : 100) size(20 : 20) red))
             (commands
              (init circle)
              (show)
              (collision circle
                     (jump circle)
                     (show)))))


(define ANIMATION3
  (animation 400 400
             (shapes (circle 1 -> ellipse pos(20 : 20) size(10 : 10) orange)
                     (wall1 2 -> rectangle pos(320 : 200) size(20 : 250) red)
                     (wall2 3 -> rectangle pos(200 : 350) size(300 : 30) green))
             (commands
              (init circle wall2)
              (show)
              (collision circle
                         (move circle dx: 5 dy: 10)
                         (show))
              (add wall1)
              (show)
              (move circle dx: 0 dy: -12)
              (collision circle
                         (move circle dx: 4 dy: -5)
                         (show))
              (show)
              (move circle dx: -5 dy: 0)
              (collision circle
                         (move circle dx: -5 dy: -1)
                         (show))
              (show))))




; Interpreter
(define WIDTH 400)
(define HEIGHT 400)
(create-canvas WIDTH HEIGHT)

(define shapes empty)

;; A var is (make-var ID value)
(define-struct var (ID value))

;; init-vars: list[values] -> void
(define (init-vars a-list)
  (set! shapes (map (lambda (shpe) (make-var (shape-ID shpe) shpe)) a-list)))

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
  (run-cmds (animation-cmds anim))
  (set! shapes empty))

;;run-cmds: list[command] -> void
(define (run-cmds loc)
  (cond [(empty? loc) (void)]
        [(cons? loc)
         (begin
           (run-cmd (first loc))
           (sleep/yield 0.025)
           (run-cmds (rest loc)))]))

;;run-cmd: cmd -> void
(define (run-cmd cmd)
  (cond [(add-shape-cmd? cmd) (add-shape (add-shape-cmd-shape cmd))]
        [(delete-shape-cmd? cmd) (delete-shape (delete-shape-cmd-shape cmd))]
        [(move-shape-cmd? cmd) (move-shape (move-shape-cmd-shape cmd) (move-shape-cmd-delta cmd))]
        [(jump-shape-cmd? cmd) (jump-shape (jump-shape-cmd-shape cmd))]
        [(init-shapes-cmd? cmd) (init-vars (init-shapes-cmd-los cmd))]
        [(until-xy-cond-cmd? cmd) (until-xy-cond (until-xy-cond-cmd-shape cmd) cmd)]
        [(show-cmd? cmd) (show-all)]
        [(until-collision-cond-cmd? cmd) (until-collision cmd)]))


;;move-shape: shape delta -> void
(define (move-shape a-shape a-delta)
  (let ([nshape (lookup-shape a-shape)])
    (edit-shape-posn nshape
                     (+ (posn-x (shape-posn nshape)) (delta-x a-delta))
                     (+ (posn-y (shape-posn nshape)) (delta-y a-delta)))))

;;jump-shape: shape -> void
(define (jump-shape a-shape)
  (edit-shape-posn a-shape
                   (random WIDTH)
                   (random HEIGHT)))

;;edit-shape-posn: shape number number -> void
(define (edit-shape-posn a-shape nx ny)
  (delete-shape a-shape)
  (add-shape (make-shape (shape-type a-shape)
                         (make-posn nx ny)
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
               (or (< shx (/ (shape-width a-shape) 2))
                   (> shx (- WIDTH (/ (shape-width a-shape) 2)))
                   (< shy (/ (shape-height a-shape) 2))
                   (> shy (- HEIGHT (/ (shape-height a-shape) 2)))))
           (void)]
          [else
           (run-cmds (until-xy-cond-cmd-cmds cmd))
           (until-xy-cond a-shape cmd)])))

;;range-interp: value -> number
(check-expect (range-interp 50 'low) 50)
(check-expect (range-interp 'na 'low) -10000)
(check-expect (range-interp 'na 'high) 10000)
(define (range-interp val type)
  (cond [(symbol? val) (if (symbol=? type 'low)
                           -10000
                           10000)]
        [else val]))


;;until-collision: command -> void
(define (until-collision acmd)
  (cond [(any-collision? (until-collision-cond-cmd-shp acmd)
                        (map (lambda (var) (var-value var)) shapes))
         (void)]
        [else
         (run-cmds (until-collision-cond-cmd-cmds acmd))
         (until-collision acmd)]))

;;any-collision: shape list[shape]-> boolean
(define (any-collision? ashp los)
  (or (member true (map (lambda (shp) (cond [(= (shape-ID ashp) (shape-ID shp))
                                         empty]
                                        [else (collision?
                                                (lookup-shape ashp)
                                                (lookup-shape shp))])) los))
      (offscreen? (lookup-shape ashp))))
   
;;collision?: shape shape -> boolean
(check-expect (collision? (make-shape 'circle (make-posn -10 100) 'blue 10 10 1)
                          (make-shape 'circle (make-posn 100 100) 'blue 10 10 2))
              false)
;(check-expect (collision? (make-shape 'circle (make-posn 
(define (collision? shp1 shp2)
  (let* ([s1x (posn-x (shape-posn shp1))]
         [s1y (posn-y (shape-posn shp1))]
         [s2x (posn-x (shape-posn shp2))]
         [s2y (posn-y (shape-posn shp2))]
         [s1w (/ (shape-width shp1) 2)]
         [s1minx (- s1x s1w)]
         [s1maxx (+ s1x s1w)]
         [s1h (/ (shape-height shp1) 2)]
         [s1miny (- s1y s1h)]
         [s1maxy (+ s1y s1h)]
         [s2w (/ (shape-width shp2) 2)]
         [s2minx (- s2x s2w)]
         [s2maxx (+ s2x s2w)]
         [s2h (shape-height shp2)]
         [s2miny (- s2y s2h)]
         [s2maxy (+ s2y s2h)])
    (or (and (> s1maxx s2minx) (< s1maxx s2maxx) (> s1y s2miny) (< s1y s2maxy))
        (and (< s1minx s2maxx) (> s1minx s2minx) (> s1y s2miny) (< s1y s2maxy))
        (and (< s1x s2maxx) (> s1x s2minx) (> s1maxy s2miny) (< s1maxy s2maxy))
        (and (< s1x s2maxx) (> s1x s2minx) (< s1miny s2maxy) (> s1miny s2miny)))))


;;offscreen?
(define (offscreen? ashp)
  (let* ([s1x (posn-x (shape-posn ashp))]
         [s1y (posn-y (shape-posn ashp))]
         [s1w (/ (shape-width ashp) 2)]
         [s1h (/ (shape-height ashp) 2)])
  (or (<= s1x s1w)
      (>= s1x (- WIDTH s1w))
      (<= s1y s1h)
      (>= s1y (- HEIGHT s1h)))))

;;show-all: -> void
(define (show-all)
  (update-frame (show-shapes (map (lambda (item) (var-value item)) shapes))))

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

(test)