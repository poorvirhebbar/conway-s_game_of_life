#lang racket
(require racket/gui (prefix-in rnrs: rnrs/exceptions-6))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;try-catch syntax definition
(define-syntax try
  (syntax-rules (catch)
    ((_ body (catch catcher))
     (call-with-current-continuation
      (lambda (exit)
        (rnrs:with-exception-handler
         (lambda (condition)
           catcher
           (exit condition))
         (lambda () body)))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;parsing, importing, deparsing and exporting tools
(define (rle->grid rle)
  (define x 0)
  (reverse
   (map reverse
        (foldl
         (lambda (c l)
           (cond
             [(= 98 c) (cons (if
                              (> x 0)
                              (append (make-list (begin0 x (set! x 0)) 0) (car l))
                              (cons 0 (car l))) (cdr l))]
             [(= 111 c) (cons (if
                               (> x 0)
                               (append (make-list (begin0 x (set! x 0)) 1) (car l))
                               (cons 1 (car l))) (cdr l))]
             [(= 36 c) (if (> x 0)
                           (append (make-list (begin0 x (set! x 0)) '()) l)
                           (cons '() l))]
             [(and (> c 47) (< c 58)) (begin (set! x (+ (- c 48) (* 10 x))) l)]
             [else l]))
         '(())
         (map char->integer (string->list rle))))))
(define (get-rle path)
  (define in (open-input-file path))
  (define grid "")
  (define (reader)
    (define line (read-line in))
    (cond
      [(eq? eof line) (rle->grid grid)]
      [(eq? #\# (string-ref line 0)) (reader)]
      [(eq? #\x (string-ref line 0)) (reader)]
      [else (begin (set! grid (string-append grid line)) (reader))]))
  (set! grid (reader))
  (grid-expand grid (apply max (map length grid)) (length grid)))
(define (list-expand bait n)
  (lambda (lst) (if (> n (length lst)) (append lst (make-list (- n (length lst)) bait)) (take lst n))))
(define (grid-expand grid m n)
  (begin
    (set! grid (map (list-expand 0 m) grid))
    (set! grid ((list-expand (make-list m 0) n) grid))
    grid))
(define grid->rle ((lambda ()
                     (define (list->rle lst)
                       (define n 1)
                       (define l (car lst))
                       (foldl (lambda (str lit)
                                (if
                                 (eq? l lit)
                                 (begin
                                   (set! n (+ n 1))
                                   str)
                                 (begin0
                                   (string-append str (if (= 1 n) "" (number->string n)) (if (= 0 l) "b" "o"))
                                   (set! n 1)
                                   (set! l lit))
                                 ))
                              ""
                              (cdr lst)))
                     (lambda (grid)
                       (foldr string-append "" (map list->rle grid) (make-list (- (length grid) 1) "$"))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;grid characteristics and algorithms
(define (grid-god l c u)
  (lambda (g n)
    (cond [(> l n) 0]
          [(= c n) 1]
          [(< u n) 0]
          [else g])))
(define (grid-apply o . L)
  (apply map (lambda R (apply map (lambda C (apply o C)) R)) L))
(define conway-god (grid-god 2 3 3))
(define (grid-n grid)
  (append (cdr grid) (list (car grid))))
(define (grid-s grid)
  (cons (last grid) (take grid (- (length grid) 1))))
(define (grid-w grid)
  (map grid-n grid))
(define (grid-e grid)
  (map grid-s grid))
(define (grid-move grid x y)
  (set! grid (append (drop grid (- (length grid) y)) (take grid (- (length grid) y))))
  (map (lambda (row) (append (drop row (- (length row) x)) (take row (- (length row) x)))) grid))
(define grid-type 0)
(define (get-toroid-neighbours grid)
  (define g grid)
  (begin
    (set! g (grid-apply + g (grid-w g) (grid-e g)))
    (set! g (grid-apply + g (grid-n g) (grid-s g)))
    (grid-apply - g grid)))

(define (get-neighbours grid)
  (cond
    [(eq? grid-type 0) (get-toroid-neighbours grid)]
    [(eq? grid-type 1) (map cdr (cdr (get-toroid-neighbours (cons (make-list (+ 1 (length (car grid))) 0) (map (lambda (x) (cons 0 x)) grid)))))]
    [(eq? grid-type 2) grid]))
(define (grid-next grid)
  (map
   (lambda (g n) (map conway-god g n))
   grid
   (get-neighbours grid)))
(define (grid-jump grid x)
  (define grid-new grid)
  (void (map (lambda (x) (set! grid-new (grid-next grid-new))) (range x)))
  grid-new)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;welcome frame
(define welcome-frame
  (new frame%
       [label "Conway's Game of Life: Simulator"]
       [width 1024]
       [height 768]
       ))
(define welcome-wrap
  (new panel%
       [parent welcome-frame]
       [alignment '(center center)]))
(define welcome-background
  (new message%
       [parent welcome-wrap]
       [label (make-object bitmap% "icons/wallpaper.jpeg")]))
(define welcome-panel
  (new vertical-panel%
       [parent welcome-wrap]
       [alignment '(center center)]))
(define welcome-heading
  (new message%
       [parent welcome-panel]
       [label "Conway's Game of Life: Simulator"]
       [font
        (make-object font% 40 'swiss 'slant 'light)]))
(define new-game
  (new button%
       [parent welcome-panel]
       [label "new  game"]
       [callback (lambda (button event)
                   (send welcome-frame delete-child welcome-wrap)
                   (send welcome-frame add-child panels))]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;loading frame
(define load-panel
  (new vertical-panel%
       [parent welcome-wrap]
       [style '(deleted)]
       [alignment '(center center)]))
(define folders (map path->string (directory-list "hangar")))
(define files (map path->string (directory-list (string-append "hangar/" (list-ref folders 10)))))
(define load-folder
  (new choice%
       [parent load-panel]
       [label "Folder: "]
       [choices folders]
       [selection 10]
       [callback (lambda (choice event)
                   (send load-panel delete-child load-file)
                   (send load-panel delete-child load-button)
                   (set! files (map path->string (directory-list (string-append "hangar/" (list-ref folders (send load-folder get-selection))))))
                   (set! load-file
                         (new choice%
                              [parent load-panel]
                              [label "File: "]
                              [choices files]))
                   (send load-panel add-child load-button))]))
(define load-file
  (new choice%
       [parent load-panel]
       [label "File: "]
       [choices files]
       [selection 9]))
(define load-button
  (new button%
       [parent load-panel]
       [label "load game"]
       [callback (lambda (button event)
                   (send welcome-frame delete-child welcome-wrap)
                   (set! grid (get-rle (string-append
                                        "hangar/"
                                        (list-ref folders (send load-folder get-selection))
                                        "/"
                                        (list-ref files (send load-file get-selection)))))
                   (set! grid-size (cons (max (car grid-size) (length (car grid))) (max (cdr grid-size) (length grid))))
                   (set! grid
                         (grid-expand grid (car grid-size) (cdr grid-size)))
                   (send welcome-frame add-child panels))]))
(define load-game
  (new button%
       [parent welcome-panel]
       [label "load game"]
       [callback (lambda (choice event)
                   (send welcome-wrap delete-child welcome-panel)
                   (send welcome-wrap add-child load-panel)
                   (void))]))
(send welcome-frame show #t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;the simulator part
(define cell-size 3)
(define grid-size '(256 . 256))
(define panels
  (new horizontal-panel%
       [parent welcome-frame]
       [style '(deleted)]))
(define panel-left
  (new panel%
       [parent panels]))
(define panel-right
  (new vertical-panel%
       [parent panels]
       [min-width 256]))
(define play-state #f)
(define patch-state #f)
(define generation 0)
(define population 0)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define pause-bmp (make-object bitmap% "icons/pause.png"))
(define play-bmp (make-object bitmap% "icons/play.png"))
(define next-bmp (make-object bitmap% "icons/next.png"))
(define jump-bmp (make-object bitmap% "icons/jump.png"))
(define fast-bmp (make-object bitmap% "icons/fast.png"))
(define slow-bmp (make-object bitmap% "icons/slow.png"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define control-panel
  (new horizontal-panel%
       [parent panel-right]
       [alignment '(center bottom)]))
(define speed-slow
  (new button%
       [parent control-panel]
       [label slow-bmp]
       [callback (lambda (button choice)
                   (set! speed-time
                         (cond
                           [(= speed-time 500) 500]
                           [(= speed-time 100) 500]
                           [(= speed-time 20) 100]))
                   (when play-state (send timer stop) (send timer start speed-time)))]))
(define play-button
  (new button%
       [parent control-panel]
       [label play-bmp]
       [callback (lambda (button event) (set! play-state (not play-state))
                   (send play-button set-label
                         (if play-state
                             (begin
                               (send timer start speed-time)
                               pause-bmp)
                             (begin
                               (send timer stop)
                               play-bmp))))]))
(define next-button
  (new button%
       [parent control-panel]
       [label next-bmp]
       [callback (lambda (button event)
                   (set! generation (+ generation 1))
                   (canvas-swap (send canvas get-dc) (grid-next grid))
                   (send generation-gauge set-label (string-append "Generation: " (number->string generation)))
                   (send population-gauge set-label (string-append "Population: " (number->string population))))]))
(define jump-button
  (new button%
       [parent control-panel]
       [label jump-bmp]
       [callback (lambda (button event)
                   (try (begin
                          (canvas-swap (send canvas get-dc) (grid-jump grid (string->number (send jump-field get-value))))
                          (set! generation (+ generation (string->number (send jump-field get-value))))
                          (send generation-gauge set-label (string-append "Generation: " (number->string generation)))
                          (send population-gauge set-label (string-append "Population: " (number->string population))))
                        (catch (void)))
                   )]))
(define speed-fast
  (new button%
       [parent control-panel]
       [label fast-bmp]
       [callback (lambda (button choice)
                   (set! speed-time
                         (cond
                           [(= speed-time 500) 100]
                           [(= speed-time 100) 20]
                           [(= speed-time 20) 20]))
                   (when play-state (send timer stop) (send timer start speed-time)))]))
(define settings-panel
  (new vertical-panel%
       [parent panel-right]
       [alignment '(center top)]))
(define jump-field
  (new text-field%
       [parent settings-panel]
       [label "Jump by: "]
       [init-value "25"]))
(define grid-choice
  (new choice%
       [parent settings-panel]
       [label "Grid type: "]
       [choices (list "Closed" "Open, Bounded" "Open")]
       [callback (lambda (choice type) (set! grid-type (send choice get-selection)))]))
(define speed-time 100)
(define generation-gauge
  (new message%
       [parent settings-panel]
       [label "Generation: 0"]))
(define population-gauge
  (new message%
       [parent settings-panel]
       [label "Population: 0"]))
(define load-game-again
  (new button%
       [parent settings-panel]
       [label "load game"]
       [callback (lambda (choice event)
                   (send timer stop)
                   (send play-button set-label play-bmp)
                   (set! play-state #f)
                   (set! speed-time 100)
                   (set! grid-size '(256 . 256))
                   (send welcome-frame delete-child panels)
                   (send welcome-frame add-child welcome-wrap)
                   (try (begin
                          (send welcome-wrap delete-child welcome-panel)
                          (send welcome-wrap add-child load-panel))
                        (catch (void))))]))
(define size-choice
  (new choice%
       [parent settings-panel]
       [label "Size: "]
       [choices (list "3" "4" "6" "8" "12")]
       [callback
        (lambda (choice event)
          (begin
            (send timer stop)
            (send play-button set-label play-bmp)
            (set! cell-size (list-ref '(3 4 6 8 12) (send choice get-selection)))
            (send canvas refresh-now)
            (send canvas init-auto-scrollbars (* cell-size (car grid-size)) (* cell-size (cdr grid-size)) 0 0)))]))
(define end-game
  (new button%
       [parent settings-panel]
       [label "end game"]
       [callback (lambda (button event)
                   (send timer stop)
                   (send welcome-frame show #f)
                   (send credits-frame show #t))]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define live (make-object brush% "BLACK" 'solid))
(define dead (make-object brush% "WHITE" 'solid))
(define kill (make-object brush% "GREEN" 'solid))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (canvas-draw dc grid-new)
  (set! grid grid-new)
  (set! population (apply + (map (lambda (row) (apply + row)) grid)))
  (map (lambda (row)
         (map (lambda (col)
                (send dc set-brush (if (= 1 (list-ref (list-ref grid row) col)) live dead))
                (send dc draw-rectangle (* cell-size col) (* cell-size row) cell-size cell-size))
              (range (car grid-size))))
       (range (cdr grid-size))))
(define (canvas-swap dc grid-new)
  (map (lambda (row)
         (map (lambda (col)
                (cond
                  [(= (list-ref (list-ref grid row) col) (list-ref (list-ref grid-new row) col)) (void)]
                  [else (begin
                          (send dc set-brush
                                (if (= 1 (list-ref (list-ref grid-new row) col))
                                    (begin (set! population (+ population 1)) live)
                                    (begin (set! population (- population 1)) kill)))
                          (send dc draw-rectangle (* cell-size col) (* cell-size row) cell-size cell-size))]))
              (range (car grid-size))))
       (range (cdr grid-size)))
  (set! grid grid-new))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define grid (grid-expand '() (car grid-size) (cdr grid-size)))
(define grid-offset '(0 0))
(define drag-coords '())
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define timer #f)
(define canvas
  (new
   (class canvas%
     (define/override (on-event e)
       (define type (send e get-event-type))
       (define x (quotient (send e get-x) cell-size))
       (define y (quotient (send e get-y) cell-size))
       (when (eq? type 'left-down)
         (canvas-swap
          (send canvas get-dc)
          (list-set
           grid y
           (list-set
            (list-ref grid y) x
            (- 1 (list-ref (list-ref grid y) x)))))))
     (super-new))
   [parent panel-left]
   [min-width 768]
   [min-height 768]
   [style '(hscroll vscroll)]
   [paint-callback
    (lambda (canvas dc)
      (send dc set-pen (new pen% [color "black"] [width 0] [style 'solid]))
      (send dc set-brush dead)
      (canvas-draw dc grid)
      (send dc set-pen (new pen% [color "black"] [width 0] [style 'transparent]))
      (send population-gauge set-label (string-append "Population: " (number->string population)))
      (set! timer
            (new timer%
                 [notify-callback (lambda ()
                                    (if (send welcome-frame is-shown?)
                                        (begin
                                          (canvas-swap dc (grid-next grid))
                                          (set! generation (+ generation 1))
                                          (send generation-gauge set-label (string-append "Generation: " (number->string generation)))
                                          (send population-gauge set-label (string-append "Population: " (number->string population))))
                                        (send timer stop)))])))]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define thanku (grid-expand (rle->grid "11$b5obo3bo2b2o2b2o4bobo2bo$
3bo3bo3bobo2bobobo3bobobo$
3bo3b5ob4obo2bo2bob2o$
3bo3bo3bobo2bobo3bobobobo$
3bo3bo3bobo2bobo4b2obo2bo$
$
$
10bo3bo2b2o2bo3bo$
11bobo2bo2bobo3bo$
12bo3bo2bobo3bo$
12bo3bo2bobo3bo$
12bo4b2o3b3o$") 32 32))
(define nikhil-poorvi
  (grid-expand
   (rle->grid
    "10$2o4bob3obo2bobobob3obo$obo3bo2bo2bobo2bobo2bo2bo$o2bo2bo2bo2b2o3b3o2bo2bo$o3bobo2bo2bobo2bobo2bo2bo$o4b2ob3obo2bobobob3ob4o3$
3o3b2o3b2o2b3o2bo5bob3o$o2bobo2bobo2bobo2bobo5bo2bo$3o2bo2bobo2bob3o3bo3bo3bo$o4bo2bobo2bobobo4bobo4bo$o5b2o3b2o2bo2bo4bo4b3o$") 32 32))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define count 0)
(define credits-frame
  (new frame%
       [label "Thank you! Run me again!"]
       [width 256]
       [height 256]))
(define credits-canvas
  (new canvas%
       [parent credits-frame]
       [paint-callback
        (lambda (canvas dc)
          (set! cell-size 8)
          (set! grid-size '(32 . 32))
          (set! grid-type 0)
          (set! grid nikhil-poorvi)
          (canvas-draw dc grid)
          (set! timer
                (new timer%
                     [interval 200]
                     [notify-callback (lambda ()
                                        (set! count (+ count 1))
                                        (canvas-swap dc
                                                     (cond
                                                       [(< count 32)
                                                        (append (cdr (grid-next (cons (make-list 32 0) (take grid count))))
                                                                (drop grid count))]
                                                       [(< count 40) (grid-next grid)]
                                                       [(< count 45) (begin (canvas-draw dc thanku) thanku)]
                                                       [(< count 72) (append (cdr (grid-next (cons (make-list 32 0) (take grid (- count 40)))))
                                                                             (drop grid (- count 40)))]
                                                       [else
                                                        (begin0
                                                          grid
                                                          (send timer stop)
                                                          (send credits-frame show #f))])))])))]))
