;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname space-invaders-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders


;; Constants:

(define WIDTH  300)
(define HEIGHT 500)

(define INVADER-X-SPEED 1.5)  ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 1.5)
(define TANK-SPEED 2)
(define MISSILE-SPEED 10)

(define HIT-RANGE 10)

(define INVADE-RATE 100)

(define BACKGROUND (empty-scene WIDTH HEIGHT))

(define INVADER
  (overlay/xy (ellipse 10 15 "outline" "blue")              ;cockpit cover
              -5 6
              (ellipse 20 10 "solid"   "blue")))            ;saucer

(define TANK
  (overlay/xy (overlay (ellipse 28 8 "solid" "black")       ;tread center
                       (ellipse 30 10 "solid" "green"))     ;tread outline
              5 -14
              (above (rectangle 5 10 "solid" "black")       ;gun
                     (rectangle 20 10 "solid" "black"))))   ;main body

(define TANK-HEIGHT/2 (/ (image-height TANK) 2))

(define MISSILE (ellipse 5 15 "solid" "red"))



;; Data Definitions:

(define-struct game (invaders missiles tank))
;; Game is (make-game  (listof Invader) (listof Missile) Tank)
;; interp. the current state of a space invaders game
;;         with the current invaders, missiles and tank position

;; Game constants defined below Missile data definition

#;
(define (fn-for-game s)
  (... (fn-for-loinvader (game-invaders s))
       (fn-for-lom (game-missiles s))
       (fn-for-tank (game-tank s))))



(define-struct tank (x dir))
;; Tank is (make-tank Number Integer[-1, 1])
;; interp. the tank location is x, HEIGHT - TANK-HEIGHT/2 in screen coordinates
;;         the tank moves TANK-SPEED pixels per clock tick left if dir -1, right if dir 1

(define T0 (make-tank (/ WIDTH 2) 1))   ;center going right
(define T1 (make-tank 50 1))            ;going right
(define T2 (make-tank 50 -1))           ;going left

#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dir t)))



(define-struct invader (x y dx))
;; Invader is (make-invader Number Number Number)
;; interp. the invader is at (x, y) in screen coordinates
;;         the invader along x by dx pixels per clock tick

(define I1 (make-invader 150 100 12))           ;not landed, moving right
(define I2 (make-invader 150 HEIGHT -10))       ;exactly landed, moving left
(define I3 (make-invader 150 (+ HEIGHT 10) 10)) ;> landed, moving right


#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader)))


(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile 150 300))                       ;not hit U1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit U1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit U1

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))



(define G0 (make-game empty empty T0))
(define G1 (make-game empty empty T1))
(define G2 (make-game (list I1) (list M1) T1))
(define G3 (make-game (list I1 I2) (list M1 M2) T1))

;; =================
;; Functions:

;; Game -> Game
;; called to start the Space Invaders game; start with (main G0)
;; no tests for main function
(define (main g)
  (big-bang g
            (on-tick next-tick)       ; Game -> Game
            (to-draw render)          ; Game -> Image
            (on-key  handle-key)      ; Game KeyEvent -> Game
            (stop-when game-over?)))  ; Game -> boolean?

;; Game -> Game
;; - add new invaders
;; - move tank, missiles and invaders
;; - destroy collided and out of screen objects
(check-expect (next-tick G3) (add-objects (move-objects (destroy-objects G3))))

;(define (next-tick g) g) ; stub

(define (next-tick g)
  (add-objects (move-objects (destroy-objects g))))

;; Game -> Game
;; adds new invaders to the game
(check-expect (add-objects G3) (make-game (add-invaders (game-invaders G3))
                                          (game-missiles G3)
                                          (game-tank G3)))

;(define (add-objects g) g) ; stub

(define (add-objects g)
  (make-game (add-invaders (game-invaders g))
             (game-missiles g)
             (game-tank g)))

;; ListOfInvaders -> ListOfInvaders
;; randomly adds new invader to the list (on <3% calls due to high "next-tick" call frequency)
;; invader moving direction is randomly left or right

;(define (add-invaders loi) loi) ; stub

(define (add-invaders loi)
  (if (< (random INVADE-RATE) 3)
    (cons (make-invader (random WIDTH) 0 (* (if (= (random 2) 1) 1 -1) 1)) loi)
    loi))

;; Game -> Game
;; moves all game objects according to their speed, including wall handling
(check-expect (move-objects G3) (make-game
                                  (move-invaders (game-invaders G3))
                                  (move-missiles (game-missiles G3))
                                  (move-tank (game-tank G3))))

;(define (move-objects g) g) ; stub

(define (move-objects g)
  (make-game
    (move-invaders (game-invaders g))
    (move-missiles (game-missiles g))
    (move-tank (game-tank g))))

;; ListOfInvader -> ListOfInvader
;; updates all invader positions using INVADER-X-SPEED, INVADER-Y-SPEED and invader direction
(check-expect (move-invaders empty) empty)
(check-expect (move-invaders (list I1 I2)) (list (move-invader I1) (move-invader I2)))

;(define (move-invaders loi) loi) ; stub

(define (move-invaders loi)
  (cond [(empty? loi) empty]
        [else (cons (move-invader (first loi))
                   (move-invaders (rest loi)))]))

;; Invader -> Invader
;; updates invader position using INVADER-X-SPEED, INVADER-Y-SPEED and invader direction
(check-expect (move-invader I1)
                (make-invader
                  (+ (invader-x I1) (* (invader-dx I1) INVADER-X-SPEED))
                  (+ (invader-y I1) (* (invader-dx I1) INVADER-Y-SPEED))
                  (invader-dx I1)))
(check-expect (move-invader (make-invader 0 50 -10))
                (onscreen-invader (make-invader
                  (+ 0 (* -10 INVADER-X-SPEED))
                  (+ 50 (* (abs -10) INVADER-Y-SPEED))
                  -10)))
(check-expect (move-invader (make-invader WIDTH 70 12))
                (onscreen-invader (make-invader
                  (+ WIDTH (* 12 INVADER-X-SPEED))
                  (+ 70 (* (abs 12) INVADER-Y-SPEED))
                  12)))

;(define (move-invader i) i) ; stub

(define (move-invader i)
  (onscreen-invader (make-invader (+ (invader-x i) (* (invader-dx i) INVADER-X-SPEED))
                (+ (invader-y i) (* (abs (invader-dx i)) INVADER-Y-SPEED))
                (invader-dx i))))

;; Invader -> Invader
;; moves invader back to screen if it went out of its bounds
(check-expect (onscreen-invader (make-invader 0 50 -10))
                (make-invader
                  (* 10 INVADER-X-SPEED)
                  (+ 50 (* 10 INVADER-Y-SPEED))
                  10))
(check-expect (onscreen-invader (make-invader WIDTH 70 12))
                (make-invader
                  (+ WIDTH (* -12 INVADER-X-SPEED))
                  (+ 70 (* (abs -12) INVADER-Y-SPEED))
                  -12))

;(define (onscreen-invader i) i) ; stub

(define (onscreen-invader i)
  (cond [(<= (invader-x i) 0)
           (make-invader
             (* (- (invader-dx i)) INVADER-X-SPEED)
             (+ (invader-y i) (* (abs (invader-dx i)) INVADER-Y-SPEED))
             (- (invader-dx i)))]
        [(>= (invader-x i) WIDTH)
           (make-invader
             (+ WIDTH (* (- (invader-dx i)) INVADER-X-SPEED))
             (+ (invader-y i) (* (abs (invader-dx i)) INVADER-Y-SPEED))
             (- (invader-dx i)))]
        [else i]))

;; ListOfMissile -> ListOfMissile
;; updates all missile y positions using MISSILE-SPEED
(check-expect (move-missiles empty) empty)
(check-expect (move-missiles (list M1 M2)) (list
                                             (make-missile (missile-x M1) (- (missile-y M1) MISSILE-SPEED))
                                             (make-missile (missile-x M2) (- (missile-y M2) MISSILE-SPEED))))

;(define (move-missiles lom) lom) ; stub

(define (move-missiles lom)
  (cond [(empty? lom) empty]
        [else (cons (move-missile (first lom))
                   (move-missiles (rest lom)))]))

;; Missile -> Missile
;; updates missile y position using MISSILE-SPEED
(check-expect (move-missile M1) (make-missile (missile-x M1) (- (missile-y M1) MISSILE-SPEED)))

;(define (move-missile m) m) ; stub

(define (move-missile m)
  (make-missile (missile-x m) (- (missile-y m) MISSILE-SPEED)))

;; Tank -> Tank
;; updates tank x position using TANK-SPEED and moving direction
(check-expect (move-tank T1) (make-tank (+ (tank-x T1) TANK-SPEED) (tank-dir T1)))
(check-expect (move-tank T2) (make-tank (- (tank-x T2) TANK-SPEED) (tank-dir T2)))
(check-expect (move-tank (make-tank 0 -1)) (onscreen-tank (make-tank (- 0 TANK-SPEED) -1)))
(check-expect (move-tank (make-tank WIDTH 1)) (onscreen-tank (make-tank (+ WIDTH TANK-SPEED) 1)))

;(define (move-tank t) t) ; stub

(define (move-tank t)
  (onscreen-tank
    (if (= (tank-dir t) 1)
      (make-tank (+ (tank-x t) TANK-SPEED) (tank-dir t))
      (make-tank (- (tank-x t) TANK-SPEED) (tank-dir t)))))

;; Tank -> Tank
;; moves tank back to screen if it went out of its bounds
(check-expect (onscreen-tank (make-tank 0 -1)) (make-tank (+ 0 TANK-SPEED) 1))
(check-expect (onscreen-tank (make-tank -10 -1)) (make-tank (+ 0 TANK-SPEED) 1))
(check-expect (onscreen-tank (make-tank WIDTH 1)) (make-tank (- WIDTH TANK-SPEED) -1))
(check-expect (onscreen-tank (make-tank (+ WIDTH 5) 1)) (make-tank (- WIDTH TANK-SPEED) -1))

;(define (onscreen-tank t) t) ; stub

(define (onscreen-tank t)
  (cond [(<= (tank-x t) 0)
          (make-tank TANK-SPEED 1)]
        [(>= (tank-x t) WIDTH)
          (make-tank (- WIDTH TANK-SPEED) -1)]
        [else t]))

;; Game -> Game
;; removes invaders hit by missiles
(check-expect (destroy-objects G3)
              (make-game (destroy-invaders (game-invaders G3) (game-missiles G3))
                         (destroy-missiles (game-missiles G3) (game-invaders G3))
                         (game-tank G3)))

;(define (destroy-objects g) g) ; stub

(define (destroy-objects g)
  (make-game (destroy-invaders (game-invaders g) (game-missiles g))
             (destroy-missiles (game-missiles g) (game-invaders g))
             (game-tank g)))

;; ListOfInvaders ListOfMissiles -> ListOfInvaders
;; produces new list of invaders that were not destroyed by missiles
(check-expect (destroy-invaders empty empty) empty)
(check-expect (destroy-invaders (list I1 I2) (list M1)) (list I1 I2))
(check-expect (destroy-invaders (list I1 I2) (list M1 M2)) (list I2))
(check-expect (destroy-invaders (list I1 I2) (list M1 M3)) (list I2))

;(define (destroy-invaders loi lom) loi) ; stub

(define (destroy-invaders loi lom)
  (cond [(empty? loi) empty]
        [(destroy-invader? (first loi) lom)
            (destroy-invaders (rest loi) lom)]
        [else (cons (first loi)
                    (destroy-invaders (rest loi) lom))]))

;; Invader ListOfMissiles -> boolean?
;; produces #true if invader is hit by any missile from the list
(check-expect (destroy-invader? I1 empty) #false)
(check-expect (destroy-invader? I1 (list M1)) #false)
(check-expect (destroy-invader? I1 (list M1 M2)) #true)
(check-expect (destroy-invader? I1 (list M1 M3)) #true)

;(define (destroy-invader? i lom) #false) ; stub

(define (destroy-invader? i lom)
  (cond [(empty? lom) #false]
        [else (or (hit-invader? i (first lom))
                  (destroy-invader? i (rest lom)))]))

;; Invader Missile -> boolean?
;; produces #true if invader x,y position is in HIT-RANGE area of missile x,y position
(check-expect (hit-invader? I1 M1) #false)
(check-expect (hit-invader? I1 M2) #true)
(check-expect (hit-invader? I1 M3) #true)

;(define (hit-invader? i m) #false) ; stub

(define (hit-invader? i m)
  (and (<= (invader-x i) (+ (missile-x m) HIT-RANGE))
       (>= (invader-x i) (- (missile-x m) HIT-RANGE))
       (<= (invader-y i) (+ (missile-y m) HIT-RANGE))
       (>= (invader-y i) (- (missile-y m) HIT-RANGE))))

;; ListOfMissiles ListOfInvaders -> ListOfMissiles
;; produces new list of missiles that didn't hit invaders
(check-expect (destroy-missiles empty empty) empty)
(check-expect (destroy-missiles (list M1) (list I1 I2)) (list M1))
(check-expect (destroy-missiles (list M1 M2) (list I1 I2)) (list M1))
(check-expect (destroy-missiles (list M1 M3) (list I1 I2)) (list M1))

;(define (destroy-missiles lom loi) lom) ; stub

(define (destroy-missiles lom loi)
  (cond [(empty? lom) empty]
        [(destroy-missile? (first lom) loi)
            (destroy-missiles (rest lom) loi)]
        [else (cons (first lom)
                    (destroy-missiles (rest lom) loi))]))

;; Missile ListOfInvaders -> boolean?
;; produces #true if missile hit any invader from the list
(check-expect (destroy-missile? M1 empty) #false)
(check-expect (destroy-missile? M1 (list I1)) #false)
(check-expect (destroy-missile? M2 (list I1 I2)) #true)
(check-expect (destroy-missile? M3 (list I1 I2)) #true)

;(define (destroy-missile? m loi) #false) ; stub

(define (destroy-missile? m loi)
  (cond [(empty? loi) #false]
        [else (or (hit-invader? (first loi) m)
                  (destroy-missile? m (rest loi)))]))

;; ListOfMissile -> ListOfMissile
;; removes missiles from the list that are out of screen bounds
(check-expect (onscreen-missiles empty) empty)
(check-expect (onscreen-missiles (list M1 M2 M3)) (list M1 M2 M3))
(check-expect (onscreen-missiles (list M1 (make-missile 50 0) M3)) (list M1 (make-missile 50 0) M3))
(check-expect (onscreen-missiles (list M1 (make-missile 50 -1) M3)) (list M1 M3))

;(define (onscreen-missiles lom) lom) ; stub

(define (onscreen-missiles lom)
  (cond [(empty? lom) empty]
        [(< (missile-y (first lom)) 0)
           (onscreen-missiles (rest lom))]
        [else (cons (first lom)
                  (onscreen-missiles (rest lom)))]))

;; Game -> Image
;; draws game surface and all objects on it
(check-expect (render G0) (place-image TANK (tank-x T0) (- HEIGHT TANK-HEIGHT/2) BACKGROUND))
(check-expect (render G3) (place-image INVADER (invader-x I1) (invader-y I1)
                            (place-image INVADER (invader-x I2) (invader-y I2)
                              (place-image MISSILE (missile-x M1) (missile-y M1)
                                (place-image MISSILE (missile-x M2) (missile-y M2)
                                  (place-image TANK (tank-x T1) (- HEIGHT TANK-HEIGHT/2) BACKGROUND))))))

;(define (render g) BACKGROUND) ; stub

(define (render g)
  (render-invaders (game-invaders g)
    (render-missiles (game-missiles g)
      (render-tank (game-tank g) BACKGROUND))))

;; Tank Image -> Image
;; renders tank using its x,y coordinates on the image
(check-expect (render-tank T0 BACKGROUND) (place-image TANK (tank-x T0) (- HEIGHT TANK-HEIGHT/2) BACKGROUND))

;(define (render-tank t img) img) ; stub

(define (render-tank t img)
  (place-image TANK (tank-x t) (- HEIGHT TANK-HEIGHT/2) BACKGROUND))

;; ListOfMissile Image -> Image
;; renders missiles using their x,y coordinates on the image
(check-expect (render-missiles empty BACKGROUND) BACKGROUND)
(check-expect (render-missiles (list M1 M2) BACKGROUND)
                (place-image MISSILE (missile-x M1) (missile-y M1)
                  (place-image MISSILE (missile-x M2) (missile-y M2)
                    BACKGROUND)))

;(define (render-missiles lom img) img) ; stub

(define (render-missiles lom img)
  (cond [(empty? lom) img]
        [else (place-image MISSILE (missile-x (first lom)) (missile-y (first lom))
                   (render-missiles (rest lom) img))]))

;; ListOfInvaders Image -> Image
;; renders invaders using their x,y coordinates on the image
(check-expect (render-invaders empty BACKGROUND) BACKGROUND)
(check-expect (render-invaders (list I1 I2) BACKGROUND)
                (place-image INVADER (invader-x I1) (invader-y I1)
                  (place-image INVADER (invader-x I2) (invader-y I2)
                    BACKGROUND)))

;(define (render-invaders loi img) img) ; stub

(define (render-invaders loi img)
  (cond [(empty? loi) img]
        [else (place-image INVADER (invader-x (first loi)) (invader-y (first loi))
                   (render-invaders (rest loi) img))]))

;; Game KeyEvent -> Game
;; handles player actions
(check-expect (handle-key (make-game (list I1 I2) (list M1 M2) (make-tank 50 1)) "left")
                (make-game (list I1 I2) (list M1 M2) (make-tank 50 -1)))
(check-expect (handle-key (make-game (list I1 I2) (list M1 M2) (make-tank 30 -1)) "right")
                (make-game (list I1 I2) (list M1 M2) (make-tank 30 1)))
(check-expect (handle-key (make-game (list I1 I2) (list M1 M2) (make-tank 30 -1)) "a")
                (make-game (list I1 I2) (list M1 M2) (make-tank 30 -1)))
(check-expect (handle-key (make-game (list I1 I2) (list M1 M2) (make-tank 60 1)) " ")
                (make-game (list I1 I2) (list (make-missile 60 (- HEIGHT TANK-HEIGHT/2)) M1 M2) (make-tank 60 1)))

;(define (handle-key g ke) g) ; stub

(define (handle-key g ke)
  (cond [(key=? ke " ")
           (make-game
             (game-invaders g)
             (cons (make-missile (tank-x (game-tank g)) (- HEIGHT TANK-HEIGHT/2)) (game-missiles g))
             (game-tank g))]
        [(key=? ke "left")
           (make-game (game-invaders g) (game-missiles g) (make-tank (tank-x (game-tank g)) -1))]
        [(key=? ke "right")
           (make-game (game-invaders g) (game-missiles g) (make-tank (tank-x (game-tank g)) 1))]
        [else g]))

;; Game -> boolean?
;; returns #true when any game over condition is #true
(check-expect (game-over? G0) #false)
(check-expect (game-over? G2) #false)
(check-expect (game-over? G3) #true)

;(define (game-over? g) #false) ; stub

(define (game-over? g)
   (any-invader-passed? (game-invaders g)))  ; (or (condition1) (condition2) ...)

;; ListOfInvader -> boolean?
;; returns #true when any invader passed below the HEIGHT level
(check-expect (any-invader-passed? empty) #false)
(check-expect (any-invader-passed? (list I1)) #false)
(check-expect (any-invader-passed? (list I1 I2)) #true)
(check-expect (any-invader-passed? (list I1 I3)) #true)

;(define (any-invader-passed? loi) #false) ; stub

(define (any-invader-passed? loi)
  (cond [(empty? loi) #false]
        [else (or (>= (invader-y (first loi)) HEIGHT)
                  (any-invader-passed? (rest loi)))]))