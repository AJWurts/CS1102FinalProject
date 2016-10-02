;; Alexander Wurts
;; Program redraws the screen after every command
(require "world-cs1102.rkt")
;; a shape is (make-shape symbol posn color)
;; acceptable shape types are 'triangle, 'ellipse, and 'rectangle
(define-struct shape (type posn color width height))

;; a delta is (make-delta x y)
(define-struct delta (x y))

;; an animation is (make-animation number number list[cmd])
(define-struct animation (width height cmds))

;; a cmd is either
;; - (make-display-shape-cmd shape)
;; - (make-delete-shape-cmd shape)
;; - (make-move-shape-cmd shape)
;; - (make-jump-shape-cmd shape)
;; - (make-init-shapes-cmd list[shape])
;; - (make-move-until-collide (shp1)) ;; a collision is either hitting the edge of the screen or another shape
;; - (repeat-until-off-screen (cmd)) ;; runs a given cmd until the object touches the side of the screen
(define-struct display-shape-cmd (shape))
(define-struct delete-shape-cmd (shape))
(define-struct move-shape-cmd (shape delta))
(define-struct jump-shape-cmd (shape))
(define-struct make-init-shapes-cmd (los))
(define-struct cmds-until-xy-cond (shp1 minx miny maxx maxy cmd))

;;All examples written assuming that the shapes are placed in the correct location based on the original pictures, even though they are not
(define ANIMATION1
  (let [(circle (make-shape 'ellipse (make-posn 100 100) 'red 5 5))
        (wall1 (make-shape 'rectangle (make-posn 300 100) 'blue 300 100))
        (wall2 (make-shape 'rectangle (make-posn 100 300) 'red 100 300))]
  (make-animation 400 400
                  (list
                   (make-init-shapes-cmd (list circle wall1))
                   (make-cmds-until-xy-cond circle 100 95 105 105
                                            (list
                                             (make-cmd-move-shape-cmd circle (make-delta 3 1))
                                             (make-display-shape-cmd circle)))
                   (make-delete-shape-cmd wall1)
                   (make-cmds-until-xy-cond circle 0 'na 0 'na
                                            (list
                                             (make-cmd-move-shape-cmd circle (make-delta -3 1))
                                             (make-display-shape-cmd circle))))
                   )))

(define ANIMATION2
  (let [(circle (make-ball (make-posn 100 100)))
        (wall1 (make-wall (make-posn 300 100) 300 100))
        (wall2 (make-wall (make-posn 100 300) 100 300))]
    (make-animation 400 400
                    (list
                     (make-init-shapes-cmd (list circle))
                     (make-repeat-until-offscreen
                      (make-jump-shape-cmd circle))))))

(define ANIMATION3
  (let [(circle (make-ball (make-posn 100 100)))
        (wall1 (make-wall (make-posn 300 100) 300 100))
        (wall2 (make-wall (make-posn 100 300) 100 300))]
    (make-animation 400 400
                    (list
                     (make-init-shapes-cmd (list ( circle wall1))
                     (make-move-until-collide circle (make-delta 0 4))
                     (make-move-until-collide circle (make-delta 4 0))
                     (make-display-shape-cmd wall2)
                     (make-move-until-collide circle (make-delta -2 -2 )))))))
                     


;; Interpreter

(define WIDTH 400)
(define HEIGHT 400)
(create-canvas WIDTH HEIGHT)

(define vars empty)

;; A var is (make-var ID value)
(define-struct var (ID value))

;; init-vars: list[values] -> void
(define (init-var a-list)
  (set! vars (id-vars a-list 1)))

;; id-vars: list[values] id -> list[vars]
(define (id-vars a-list id)
  (cond [(empty? a-list) empty]
        [(cons? a-list)
         (append (list (make-var id (first a-list)))
                 (id-vars (rest a-list) (+ 1 id)))]))
  
;; lookup-var-by-id: id -> value
(define (lookup-var-by-id id)
  (first (filter (lambda (item) (= (var-ID item) id)) vars)))
 

  
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
  (cond [(display-shape-cmd? cmd)]
        [(delete-shape-cmd? cmd)]
        [(move-shape-cmd? cmd)]
        [(jump-shape-cmd? cmd)]
        [(make-init-shapes-cmd? cmd)]
        [(cmds-until-xy-cond? cmd)]))

;;show-all: -> void
(define (show-all)
  (update-frame (show-shapes (filter (lambda (item) (shape? item)) vars))))

;;show-shapes: list[shape] -> scene
(define (show-shapes los)
  (cond [(empty? los) (empty-scene WIDTH HEIGHT)]
        [(cons? los)
         (show-shape (first los) (show-shapes (rest los)))]))

;;show-shape: shape scene -> scene
(define (show-shape shape scene)
  (place-image (make-shape-img shape)
               (posn-x (shape-posn shape)) (posn-y (shape-posn shape))
               scene))

;;make-shape-img: shape -> image
(define (make-shape-img shape)
  (shape-type shape) (shape-width width) (shape-height shape) (shape-color shape)
  (let [(wid (shape-width shape))
        (hei (shape-height shape))
        (col (shape-color col))]
  (cond [(symbol=? 'triangle)
         (isosceles-triangle (sqrt (+ (expt hei 2) (expt (* .5 wid) 2)))
                             (* 2 (tan (/ (* .5 (shape-width shape)) (shape-height))))
                             'solid col)]
        [(symbol=? 'ellipse)
         (ellipse wid hei 'solid col)]
        [(symbol=? 'rectangle)
         (rectangle wid hei 'solid col)])))
         
  