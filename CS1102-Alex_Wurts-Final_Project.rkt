;; Alexander Wurts
#|
1. Run to program by typing in (run-animation #animation name here#).
The 4 animations already made are A1, A2, A3, A4.
2. Everything aspect of the program is functional. All features asked for by the project
assignment page work. Repeated code blocks work well. The macro language works as long as
the syntax is right as in any programming language.
3. The biggest change was consolidating all the shapes types into one shape structure with a type
to differentiate between the shapes. This made it easier to generalize the functions written to work on
all shapes used in the program, even if one was a circle and one was a rectangle.
I also added a lot more data to shape to include color width height and ID. The ID
is to track the shape once the interpretor has it to allow for the shape to be called by
name when being used in the language. As for functions I consolidated repeat-until-offscreen
and move-until-collide into one function to simplify things. The show-shape command was also
turned into show-all so that the user didn't need to repeat a lot of code. The amount a shape
moved was also moved into the move command instead of being a constant in the shape structure.
This made the code easier to understand, and made things in general simpler.
4. The implemetation of global variables is quite messy. I added a var structure that wasnt required and it
just complicated the get and sets. I also think I should have thought of a better way to implement (show)
so the user does not have to type it out so many times in a program. I initially had it just show after every command
but that does not allow shapes to move simultaneously. Other than those small things, I think overall everything is
satisfactory. The language with the macros turned out well, and is (hopefully) easy to understand and use. It
definitely makes writing the animations a lot faster. 
|#
(require test-engine/racket-gui)
(require "world-cs1102.rkt")
(require test-engine/racket-tests)


;; a shape is (make-shape symbol posn color)
;; acceptable shape types are 'triangle, 'ellipse, and 'rectangle
(define-struct shape (type posn color width height ID) (make-inspector))

;; a delta is (make-delta x y)
(define-struct delta (x y) (make-inspector))

;; an animation is (make-animation number number list[cmd])
(define-struct animation (width height cmds) (make-inspector))

;; a cmd is either
;; - (make-add-cmd shape)
;; - (make-delete-shape-cmd shape)
;; - (make-move-shape-cmd shape)
;; - (make-jump-shape-cmd shape)
;; - (make-init-shapes-cmd list[shape])
;; - (make-show-cmd)
;; - (make-until-collision-cond-cmd shape list[shape])
(define-struct add-shape-cmd (shape) (make-inspector))
(define-struct delete-shape-cmd (shape) (make-inspector))
(define-struct move-shape-cmd (shape delta) (make-inspector))
(define-struct jump-shape-cmd (shape) (make-inspector))
(define-struct init-shapes-cmd (los) (make-inspector))
(define-struct until-collision-cond-cmd (shp cmds) (make-inspector))
(define-struct show-cmd () (make-inspector))

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
(define A1
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


(define A2
  (animation 400 400
             (shapes (circle 1 -> ellipse pos(50 : 100) size(20 : 20) red))
             (commands
              (init circle)
              (show)
              (collision circle
                     (jump circle)
                     (show)))))


(define A3
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
              (show)
              (collision circle
                         (move circle dx: 4 dy: -5)
                         (show))
              (show)
              (move circle dx: -5 dy: 0)
              (show)
              (collision circle
                         (move circle dx: -5 dy: -1)
                         (show))
              (show))))

(define A4
  (animation 400 400
             (shapes (circle 1 -> triangle pos(100 : 350) size(30 : 30) blue)
                     (wall 2 -> rectangle pos(200 : 40) size(400 : 20) yellow))
             (commands
              (init circle wall)
              (show)
              (collision circle
                         (move circle dx: 0 dy: -9)
                         (show))
              (show)
              (move circle dx: 0 dy: 12)
              (collision circle
                         (move circle dx: 0 dy: 9)
                         (show)))))


; Interpreter
(define WIDTH 400)
(define HEIGHT 400)
(create-canvas WIDTH HEIGHT)

(define shapes empty)

;; A var is (make-var ID value)
(define-struct var (ID value))

;; init-vars: list[values] -> void
;; takes a list of values turns them into variables and adds them to the list of shapes
(define (init-vars a-list)
  (set! shapes (map (lambda (shpe) (make-var (shape-ID shpe) shpe)) a-list)))

;; lookup-shape: shape -> shape
;; takes a shape and returns the shape with the same ID saved in shapes
(define (lookup-shape a-shape)
  (var-value (first (filter (lambda (a-var) (= (var-ID a-var) (shape-ID a-shape))) shapes))))

;; delete-shape: shape -> void
;; takes a shape and deletes the same with the same ID as the one in shapes
(define (delete-shape a-shape)
  (set! shapes (filter (lambda (a-var) (not (= (var-ID a-var) (shape-ID a-shape))))
                       shapes)))


;; add-shape: shape -> void
;; takes a shape and adds it to the shapes variable
(define (add-shape shape)
  (set! shapes (append shapes (list (make-var (shape-ID shape) shape)))))

;;run-animation: animation -> void
;; runs an animation
(define (run-animation anim)
  (run-cmds (animation-cmds anim))
  (set! shapes empty))

;;run-cmds: list[command] -> void
;; runs a list of commands
(define (run-cmds loc)
  (cond [(empty? loc) (void)]
        [(cons? loc)
         (begin
           (run-cmd (first loc))
           (sleep/yield 0.025)
           (run-cmds (rest loc)))]))

;;run-cmd: cmd -> void
;; runs a command
(define (run-cmd cmd)
  (cond [(add-shape-cmd? cmd) (add-shape (add-shape-cmd-shape cmd))]
        [(delete-shape-cmd? cmd) (delete-shape (delete-shape-cmd-shape cmd))]
        [(move-shape-cmd? cmd) (move-shape (move-shape-cmd-shape cmd) (move-shape-cmd-delta cmd))]
        [(jump-shape-cmd? cmd) (jump-shape (jump-shape-cmd-shape cmd))]
        [(init-shapes-cmd? cmd) (init-vars (init-shapes-cmd-los cmd))]
        [(show-cmd? cmd) (show-all)]
        [(until-collision-cond-cmd? cmd) (until-collision cmd)]))


;;move-shape: shape delta -> void
;; moves a shape by adding the values saved in delta to its x and y posn
(define (move-shape a-shape a-delta)
  (let ([nshape (lookup-shape a-shape)])
    (delete-shape a-shape)
    (add-shape (edit-shape-posn nshape
                     (+ (posn-x (shape-posn nshape)) (delta-x a-delta))
                     (+ (posn-y (shape-posn nshape)) (delta-y a-delta))))))

;;jump-shape: shape -> void
;; moves a shape to a random location on the screen
(define (jump-shape a-shape)
  (delete-shape a-shape)
  (add-shape (edit-shape-posn a-shape
                   (random WIDTH)
                   (random HEIGHT))))

;;edit-shape-posn: shape number number -> shape
;; changes a shapes position to the two numbers given
(check-expect (edit-shape-posn (make-shape 'circle (make-posn 100 100) 'blue 10 10 1) 10 10)
              (make-shape 'circle (make-posn 10 10) 'blue 10 10 1))
(define (edit-shape-posn a-shape nx ny)
  (make-shape (shape-type a-shape)
              (make-posn nx ny)
              (shape-color a-shape)
              (shape-width a-shape)
              (shape-height a-shape)
              (shape-ID a-shape)))

;;until-collision: command -> void
;;takes a until-collision command and checks to runs the commands inside until the given
;;shape collides with something
(define (until-collision acmd)
  (cond [(any-collision? (lookup-shape (until-collision-cond-cmd-shp acmd))
                        (map (lambda (var) (var-value var)) shapes))
         (void)]
        [else
         (run-cmds (until-collision-cond-cmd-cmds acmd))
         (until-collision acmd)]))

;;any-collision?: shape list[shape]-> boolean
;;detects if the given shape has hit any other shape or the edge of the screen
(check-expect (first (any-collision? (make-shape 'circle (make-posn 110 100) 'blue 10 10 1)
                              (list (make-shape 'circle (make-posn 100 100) 'blue 10 10 2))))
              #t)
(check-expect (first (any-collision? (make-shape 'circle (make-posn 98 100) 'blue 10 10 1)
                              (list (make-shape 'circle (make-posn 100 100) 'blue 10 10 2))))
              #t)
(check-expect (first (any-collision? (make-shape 'circle (make-posn 100 102) 'blue 10 10 1)
                              (list (make-shape 'circle (make-posn 100 100) 'blue 10 10 2))))
              #t)
(check-expect (first (any-collision? (make-shape 'circle (make-posn 100 98) 'blue 10 10 1)
                              (list (make-shape 'circle (make-posn 100 100) 'blue 10 10 2))))
              #t)
(check-expect (first (any-collision? (make-shape 'circle (make-posn 100 100) 'blue 30 10 1)
                              (list (make-shape 'circle (make-posn 102 110) 'blue 10 10 2))))
              #t)
(check-expect (first (any-collision? (make-shape 'circle (make-posn 100 98) 'blue 30 10 1)
                              (list (make-shape 'circle (make-posn 100 90) 'blue 10 10 2))))
              #t)
(check-expect (any-collision? (make-shape 'circle (make-posn 100 100) 'blue 10 10 1)
                              (list (make-shape 'circle (make-posn 100 200) 'blue 10 10 1)))
              false)
(check-expect (any-collision? (make-shape 'circle (make-posn -10 100) 'blue 10 10 1)
                              (list (make-shape 'circle (make-posn 100 100) 'blue 10 10 1)))
              true)
(check-expect (any-collision? (make-shape 'circle (make-posn 100 100) 'blue 10 10 1)
                              (list (make-shape 'circle (make-posn 10 100) 'blue 10 10 1)))
              false)
(define (any-collision? ashp los)
  (or (offscreen? ashp)
      (member true (map (lambda (shp) (cond [(= (shape-ID ashp) (shape-ID shp))
                                             empty]
                                            [else (collision? ashp shp)])) los))))
;;collision?: shape shape -> boolean
;;detects whether two shapes have collided with eachother
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
    (or (and (>= s1maxx s2minx) (<= s1maxx s2maxx) (>= s1y s2miny) (<= s1y s2maxy))
        (and (<= s1minx s2maxx) (> s1minx s2minx) (>= s1y s2miny) (<= s1y s2maxy))
        (and (<= s1x s2maxx) (>= s1x s2minx) (>= s1maxy s2miny) (<= s1maxy s2maxy))
        (and (<= s1x s2maxx) (>= s1x s2minx) (<= s1miny s2maxy) (>= s1miny s2miny)))))


;;offscreen?: shape -> boolean
;;detects whether the given shape is offscreen or not
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
;;shows all shapes in the shapes variable
(define (show-all)
  (update-frame (show-shapes (map (lambda (item) (var-value item)) shapes))))

;;show-shapes: list[shape] -> scene
;;produces a scene with all shapes placed on it
(define (show-shapes los)
  (cond [(empty? los) (empty-scene WIDTH HEIGHT)]
        [(cons? los)
         (show-shape (first los) (show-shapes (rest los)))]))

;;show-shape: shape scene -> scene
;; takes a shape and another scene and returns a new scene with the shape on it
(define (show-shape shape scene)
  (let ([nshape (lookup-shape shape)])
    (place-image (make-shape-img nshape)
                 (posn-x (shape-posn nshape)) (posn-y (shape-posn nshape))
                 scene)))

;;make-shape-img: shape -> image
;;generates an image to correspond to the shape input
(define (make-shape-img shape)
  (let [(wid (shape-width shape))
        (hei (shape-height shape))
        (col (shape-color shape))]
    (cond [(symbol=? 'triangle (shape-type shape))
           (triangle wid 'solid col)]
          [(symbol=? 'ellipse (shape-type shape))
           (ellipse wid hei 'solid col)]
          [(symbol=? 'rectangle (shape-type shape))
           (rectangle wid hei 'solid col)])))


(test)