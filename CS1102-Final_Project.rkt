;; Alexander Wurts
(require "world-cs1102.rkt")
;; a mshape is (make-mshape symbol posn color)
(define-struct mshape (type posn color))

;; a wall is (make-wall posn number number)
(define-struct wall (posn width height color))

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
(define-struct move-until-collide (shp1 delta))
(define-struct repeat-until-offscreen (cmd))

;;All examples written assuming that the shapes are placed in the correct location based on the original pictures, even though they are not
(define ANIMATION1
  (let [(circle (make-ball 'circle (make-posn 100 100) 'red))
        (wall1 (make-wall (make-posn 300 100) 300 100 'blue))
        (wall2 (make-wall (make-posn 100 300) 100 300 'blue))]
 
  (make-animation 400 400
                  (list
                   (make-init-shapes-cmd (list circle wall1))
                   (make-move-until-collide circle (make-delta 4 1))
                   (make-delete-shape-cmd wall1)
                   (make-move-until-collide circle (make-delta -4 1))
                   ))))

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
                     (make-move-until-collide circle (make-delta -2 -2 ))))))
                     


;; Interpreter

(define vars empty)

;; A var is (make-var ID value)
(define-struct var (ID value))

;; init-vars: list[values] -> void
(define (init-var a-list)
  (set! vars (id-vars a-list 1))

;; id-vars: list[values] id -> list[vars]
(define (id-vars a-list id)
  (cond [(empty? a-list) empty]
        [(cons? a-list)
         (append (list (make-var id (first a-list)))
                 (id-vars (rest a-list) (+ 1 id)))]))

;; lookup-var-by-shape: shape -> value
(define (look-up-var-by-shape shape)
  (vars-value (first (filter (lambda ( ))))))
  
;; lookup-var-by-id: id -> value
(define (lookup-var-by-id id)
  (first (filter (lambda (item) (= (var-ID item) id)) vars)))
 

  
;;run-animation: animation -> void
(define (run-animation anim)
  (run-cmds (animation-cmds anim))) 
