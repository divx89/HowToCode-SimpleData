;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname space-invaders) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders

;; -------------------------------------------------------------------------------------------------------------
;;                                                   Constants
;; -------------------------------------------------------------------------------------------------------------

(define WIDTH  300)
(define HEIGHT 500) 
(define BACKGROUND (empty-scene WIDTH HEIGHT))

(define INVADER
  (overlay/xy (ellipse 10 15 "outline" "blue")              ;cockpit cover
              -5 6
              (ellipse 20 10 "solid"   "blue")))            ;saucer
(define INVADER-X-SPEED 1.5)                                ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 1.5)
(define INVADE-RATE 1000)                                   
(define INVADE-RATE-MULTIPLIER 0.02)                        

(define GAME-STOP-Y
  (- HEIGHT (/ (image-height INVADER) 2)))                  ;Stop game if the invader's Y is more than this value

(define TANK
  (overlay/xy (overlay (ellipse 28 8 "solid" "black")       ;tread center
                       (ellipse 30 10 "solid" "green"))     ;tread outline
              5 -14
              (above (rectangle 5 10 "solid" "black")       ;gun
                     (rectangle 20 10 "solid" "black"))))   ;main body

(define TANK-HEIGHT/2 (/ (image-height TANK) 2))            ;Distance above HEIGHT (ground) that Tank's Y should be
(define TANK-Y (- HEIGHT TANK-HEIGHT/2))                    ;Constant Y position of the Tank (mid point of the tank)
(define TANK-SPEED 3)                                       
(define TANK-LX (/ (image-width TANK) 2))                   ;The leftmost X that a tank can reach.
(define TANK-RX (- WIDTH (/ (image-width TANK) 2)))         ;The rightmost X that a tank can reach

(define MISSILE (ellipse 5 15 "solid" "red"))
(define MISSILE-SPEED 10)

(define HIT-RANGE 10)                                       



;; -------------------------------------------------------------------------------------------------------------
;;                                                  Data Definitions
;; -------------------------------------------------------------------------------------------------------------

;; **************************************************
(define-struct invader (x y dx))
;; invader is (make-invader Number[0,WIDTH] Number[0,HEIGHT] Number)
;; interp. The invader is at x,y, and moving on the X axis with velocity dx

(define I1 (make-invader (/ WIDTH 2) (/ HEIGHT  2)        INVADER-X-SPEED))         ;not landed, moving right
(define I2 (make-invader (/ WIDTH 2) GAME-STOP-Y       (- INVADER-X-SPEED)))        ;exactly landed, moving left
(define I3 (make-invader (/ WIDTH 2) (+ GAME-STOP-Y 1)    INVADER-X-SPEED))         ;> landed, moving right

#;
(define (fn-for-invader invader)
  (... (invader-x invader)
       (invader-y invader)
       (invader-dx invader)))
;; Template Rules Used:
;; Compound: 3 fields

;; ListOfInvaders is one of:
;; - empty
;; - (cons invader ListOfInvaders)
;; interp. A list of Invaders

(define LOI0 empty)
(define LOI1 (cons I1 LOI0))
(define LOI2 (cons I2 LOI1))
(define LOI3 (cons I3 LOI2))

#;
(define (fn-for-loi loi)
  (cond [(empty? loi) (...)]
        [else
         (... (fn-for-invader (first loi))
              (fn-for-loi (rest loi)))]))
;; Template Rules Used:
;; - One of: 2 cases:
;;   - atomic distinct: empty
;;   - compound data: (cons invader ListOfinvaders)
;;     - Reference: (first loi) is an invader
;;     - Self Reference: (rest loi) is ListOfinvaders
;; **************************************************

;; **************************************************
(define-struct missile (x y))
;; missile is (make-missile Natural[0,WIDTH] Natural[0,TANK-Y])
;; interp. A missile at position (x,y) on the screen

(define M1 (make-missile    (/ WIDTH 2) (/ HEIGHT 5)))                    ;not hit I1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) HIT-RANGE  )))  ;hit I1
(define M3 (make-missile (invader-x I1)  -1))                             ;> top of screen

#;
(define (fn-for-missile m)
  (... (missile-x m)
       (missile-y m)))
;; Template Rules Used:
;; - Compound: 2 fields

;; ListOfMissiles is one of:
;; - empty
;; - (cond missile ListOfMissiles)
;; interp. A list of Missiles

(define LOM0 empty)             ;No Missiles
(define LOM1 (cons M1 LOM0))    ;1 missile at half the width and 1/5th of height above ground
(define LOM2 (cons M2 LOM1))    ;2 missiles, 1 hitting an invader, and M1
(define LOM3 (cons M3 LOM2))    ;3 missiles, 1 over the top of the screen, M2 and M3

#;
(define (fn-for-lom lom)
  (cond [(empty? lom) (...)]
        [else
         (... (fn-for-missile (first lom))
              (fn-for-lom (rest lom)))]))
;; Template Rules Used:
;; - One of: 2 cases:
;;   - atomic distinct: empty
;;   - compound data: (cons missile ListOfMissiles)
;;     - Reference: (first lom) is a missile
;;     - Self Reference: (rest lom) is ListOfMissiles
;; **************************************************

;; **************************************************
(define-struct tank (x dir))
;; tank is (make-tank Natural[0,WIDTH] Natural{1,-1})
;; interp. The tank is at (x,TANK-Y) on the screen, and can move right or left
;;         depending on whether dir is +1 or -1 respectively

(define T0 (make-tank (/ WIDTH 2)  1))   ;center, going right
(define T1 (make-tank (/ WIDTH 3)  1))   ;going right
(define T2 (make-tank (/ WIDTH 3) -1))   ;going left
(define T3 (make-tank    TANK-LX   1))   ;At left edge, going right
(define T4 (make-tank    TANK-LX  -1))   ;At left edge, going left
(define T5 (make-tank    TANK-RX  -1))   ;At right edge, going left
(define T6 (make-tank    TANK-RX   1))   ;At right edge, going right

#;
(define (fn-for-tank t)
  (... (tank-x t)
       (tank-dir t)))
;; Template Rules Used:
;;  - Compound: 2 fields
;; **************************************************

;; **************************************************
(define-struct game (invaders missiles tank))
;; game is (make-game ListOfInvaders ListOfMissiles Tank)
;; interp. A game consists of a list of invaders, another list of missiles, and a tank

(define G0 (make-game LOI0 LOM0 T0))
(define G1 (make-game LOI0 LOM0 T1))
(define G2 (make-game LOI1 LOM1 T2))
(define G3 (make-game LOI2 LOM2 T1))
(define G4 (make-game LOI3 LOM2 T3))

#;
(define (fn-for-game g)
  (... (fn-for-loi (game-invaders g))
       (fn-for-lom (game-missiles g))
       (fn-for-tank (game-tank g))))
;; Template Rules Used:
;; - Compound: 3 Fields
;;   - Reference: (game-invaders s) is ListOfInvaders
;;   - Reference: (game-missiles s) is ListOfMissiles
;;   - Reference: (game-tank s) is a tank
;; **************************************************

;; -------------------------------------------------------------------------------------------------------------
;;                                               Function Definitions
;; -------------------------------------------------------------------------------------------------------------

;; Game -> Game
;; Start the world with a Game.
;; Start big-bang with (main (make-game empty empty T0))

(define (main g)
  (big-bang g                     ;Game
    (on-tick update-game)         ;Game -> Game
    (to-draw render-game)         ;Game -> Image
    (on-key move-and-fire)        ;Game KeyEvent -> Game
    (stop-when invader-landed?))) ;Game -> Boolean


;; ***********************************************************************************
;;                               "Update-Game" Starts
;; ***********************************************************************************

;; Game -> Game
;; Update the state of the missiles, invaders and Tank in the game
;; - Update Invaders: Add, Remove and/or move invader(s)
;; - Update Missiles: Move and/or remove missiles (when invader hit/missile moves out)
;; - Update Tank: Move tank left/right based on specified direction

(check-random (update-game (make-game LOI0 LOM0 T0))
              (make-game (update-invaders LOI0 LOM0)
                         (update-missiles LOM0 LOI0)
                         (update-tank T0)))
(check-random (update-game (make-game LOI3 LOM3 T1))
              (make-game (update-invaders LOI3 LOM3)
                         (update-missiles LOM3 LOI3)
                         (update-tank T1)))

;; (define (update-game g) g)
;; <Template taken from Game Data Definition>
(define (update-game g)
  (make-game (update-invaders (game-invaders g) (game-missiles g))
             (update-missiles (game-missiles g) (game-invaders g))
             (update-tank (game-tank g))))

;; ===================================================
;;              "Update-Invaders" Starts
;; ===================================================
;; ListOfInvaders ListOfMissiles -> ListOfInvaders
;; Modify existing invaders to move in X & Y direction with time,
;; Remove invaders which have been hit by a missile,
;; Add new invaders on random spots on X axis at random points in time

(check-random (update-invaders LOI1 LOM2) (add-invaders (move-invaders (remove-invaders LOI1 LOM2))))
(check-random (update-invaders LOI1 LOM1) (add-invaders (move-invaders (remove-invaders LOI1 LOM1))))

;; (define (update-invaders loi lom) loi) ;Stub
;; <Template from helper functions>
(define (update-invaders loi lom)
  (add-invaders
   (move-invaders 
    (remove-invaders loi lom))))
;; ------------------------------------

;; ------------------------------------
;; ListOfInvaders -> ListOfInvaders
;; Given a LOI, add an invader if a random test returns true

(check-random (add-invaders empty) (cond [(add-invader? INVADE-RATE)
                                          (cons (make-invader (random WIDTH) 0 INVADER-X-SPEED) empty)]
                                         [else empty]))
(check-random (add-invaders (cons (make-invader 50 200 (* -1 INVADER-X-SPEED))
                                  (cons (make-invader 100 210 INVADER-X-SPEED) empty)))
              (cond [(add-invader? INVADE-RATE)
                     (cons (make-invader (random WIDTH) 0 INVADER-X-SPEED)
                           (cons (make-invader 50 200 (* -1 INVADER-X-SPEED))
                                 (cons (make-invader 100 210 INVADER-X-SPEED) empty)))]
                    [else (cons (make-invader 50 200 (* -1 INVADER-X-SPEED))
                                (cons (make-invader 100 210 INVADER-X-SPEED) empty))]))
;; (define (add-invaders loi) loi) ;Stub
;; <Template taken from LOI data definition>

(define (add-invaders loi)
  (cond [(add-invader? INVADE-RATE)
         (cons (make-invader (random WIDTH) 0 INVADER-X-SPEED) loi)]
        [else loi]))

;; ------------

;; Natural -> Boolean
;; Generate True or False based on whether a random number created between 0 and a provided number
;; is less than a calculated percentage of the number

(check-random (add-invader? 1000) (<= (random 1000) (* 1000 INVADE-RATE-MULTIPLIER)))
(check-random (add-invader? 1000) (<= (random 1000) (* 1000 INVADE-RATE-MULTIPLIER)))

(define (add-invader? num)
  (<= (random num) (* num INVADE-RATE-MULTIPLIER)))
;; ------------------------------------

;; ------------------------------------
;; ListOfInvaders -> ListOfInvaders
;; Given a List of Invaders, modify them to move downwards with constant speed
;; and sideways with constant velocity

(check-expect (move-invaders LOI0) LOI0)
(check-expect (move-invaders (cons (make-invader 10 20 3) empty))
              (cons (make-invader (+ 10 3) (+ 20 INVADER-Y-SPEED) 3) empty))
(check-expect (move-invaders (cons (make-invader 297 200 3)
                                   (cons (make-invader 298 210 3)
                                         (cons (make-invader 3 250 -3)
                                               (cons (make-invader 2 280 -3) empty)))))
              (cons (make-invader 300 (+ 200 INVADER-Y-SPEED) 3)
                    (cons (make-invader 300 (+ 210 INVADER-Y-SPEED) -3)
                          (cons (make-invader 0 (+ 250 INVADER-Y-SPEED) -3)
                                (cons (make-invader 0 (+ 280 INVADER-Y-SPEED) 3) empty)))))

;; (define (move-invaders loi) loi) ;Stub
;; <Template taken from Data Definition for LOI>

(define (move-invaders loi)
  (cond [(empty? loi) empty]
        [else
         (cons (move-invader (first loi))
               (move-invaders (rest loi)))]))

;; ------------

;; invader -> invader
;; Given an invader, add X and Y speeds to change its position
;; and change direction if it hits a wall

(check-expect (move-invader (make-invader 297 200  3)) (make-invader 300 (+ 200 INVADER-Y-SPEED)  3))
(check-expect (move-invader (make-invader 298 210  3)) (make-invader 300 (+ 210 INVADER-Y-SPEED) -3))
(check-expect (move-invader (make-invader   3 280 -3)) (make-invader   0 (+ 280 INVADER-Y-SPEED) -3))
(check-expect (move-invader (make-invader   2 290 -3)) (make-invader   0 (+ 290 INVADER-Y-SPEED)  3))

;; (define (move-invader s) s) ;Stub
;; <Template taken from invader data definition>

(define (move-invader s)
  (cond [(< (change-position s "x") 0)
         (make-invader 0 (change-position s "y") (* -1 (invader-dx s)))]
        [(> (change-position s "x") WIDTH)
         (make-invader WIDTH (change-position s "y") (* -1 (invader-dx s)))]
        [else
         (make-invader (change-position s "x") (change-position s "y") (invader-dx s))]))

;; ------------

;; invader String -> Number
;; Given a invader and a string (either "x" or "y"), determine the new X or Y coordinate
;; by adding the dx or the Y direction's speed

(check-expect (change-position (make-invader 398 210  3) "x") (+ 398 3))
(check-expect (change-position (make-invader 398 210  3) "y") (+ 210 INVADER-Y-SPEED))

(define (change-position s xy)
  (cond [(string=? xy "x") (+ (invader-x s) (invader-dx s))]
        [else (+ (invader-y s) INVADER-Y-SPEED)]))
;; ------------------------------------

;; ------------------------------------
;; ListOfInvaders ListOfMissiles -> ListOfInvaders
;; Remove invader if it is hit by a missile

(check-expect (remove-invaders LOI0 LOM2) LOI0)
(check-expect (remove-invaders LOI1 LOM1) LOI1)
(check-expect (remove-invaders LOI1 LOM2) LOI0)

;; (define (remove-invaders loi) loi) ;Stub
;; <Template taken from LOI data definition, with added LOM parameter>

(define (remove-invaders loi lom)
  (cond [(empty? loi) empty]
        [else
         (if (invader-hit? (first loi) lom)
             (remove-invaders (rest loi) lom)
             (cons (first loi) (remove-invaders (rest loi) lom)))]))

;; ------------

;; Invader ListOfMissiles -> Boolean
;; Return True if the (X,Y) of a missile in the LOM is within
;; HIT-RANGE of the (X,Y) of the invader

(check-expect (invader-hit? I1 LOM2)  true)
(check-expect (invader-hit? I1 LOM1) false)

;; (define (hit? i lom) true) ;Stub
;; <Template taken from LOM data definition along with additional parameter>
(define (invader-hit? i lom)
  (cond [(empty? lom) false]
        [else
         (if (in-range? i (first lom))
             true
             (invader-hit? i (rest lom)))]))

;; ------------

;; Invader Missile -> Boolean
;; Return True if the (X,Y) of the missile is within
;; HIT-RANGE of the (X,Y) of the invader

(check-expect (in-range? I1 M2)  true)
(check-expect (in-range? I1 M1) false)

;; (define (in-range? i m) true) ;Stub
;; <Template taken from data definition of invader, along with parameter of missile>
(define (in-range? i m)
  (and (<= (abs (- (missile-x m) (invader-x i))) HIT-RANGE)
       (<= (abs (- (missile-y m) (invader-y i))) HIT-RANGE)))
;; ===================================================
;;              "Update-Invaders" Ends
;; ===================================================


;; ===================================================
;;              "Update-Missiles" Starts
;; ===================================================
;; ListOfMissiles ListOfInvaders -> ListOfMissiles
;; Modify the Y coordinate of a missile via reduction of MISSILE-SPEED

(check-expect (update-missiles empty empty) empty)
(check-expect (update-missiles  LOM2  LOI2) (move-missiles (remove-missiles LOM2 LOI2)))       

;; (define (update-missiles lom loi) lom) ;Stub
;; <Helper functions used instead of standard template>
(define (update-missiles lom loi)
  (move-missiles (remove-missiles lom loi)))
;; ------------------------------------

;; ------------------------------------
;; ListOfMissiles -> ListOfMissiles
;; Update the Y coordinate of each missile in the list of missiles

(check-expect (move-missiles LOM0) LOM0)
(check-expect (move-missiles LOM1) (cons (move-missile (first LOM1)) (rest LOM1)))

;; (define (move-missiles lom) lom) ;Stub
;; <Template taken from LOM data definition>
(define (move-missiles lom)
  (cond [(empty? lom) empty]
        [else
         (cons (move-missile (first lom))
               (move-missiles (rest lom)))]))

;; ------------

;; Missile -> Missile
;; Move a missile upwards with MISSILE-SPEED

(check-expect (move-missile (make-missile 100 50)) (make-missile 100 (- 50 MISSILE-SPEED)))

;; (define (move-missile m) m) ;Stub
;; <Template taken from Missile Data Definition>
(define (move-missile m)
  (make-missile (missile-x m)
                (- (missile-y m) MISSILE-SPEED)))

;; ------------------------------------

;; ------------------------------------
;; ListOfMissiles -> ListOfMissiles
;; From a list of missiles, remove those which have passed the top of the screen
;; or have hit an invader

(check-expect (remove-missiles empty LOI3) empty)
(check-expect (remove-missiles (cons (make-missile 80 10)
                                     (cons (make-missile 27 95) empty))
                               (cons (make-invader 30 100 6)
                                     (cons (make-invader 10 20 3) empty)))
              (cons (make-missile 80 10) empty))
(check-expect (remove-missiles (cons (make-missile 30 -2) empty) LOI3) empty)

;; (define (remove-missiles lom loi) lom) ;Stub
;; <Template taken from LOM and LOI data definitions>
(define (remove-missiles lom loi)
  (cond [(empty? lom) empty]
        [else
         (if (or (missile-hit? (first lom) loi)
                 (missile-passed-top? (first lom)))
             (remove-missiles (rest lom) loi)
             (cons (first lom) (remove-missiles (rest lom) loi)))]))

;; ------------

;; Missile ListOfInvaders -> Boolean
;; Return True if the (X,Y) of the missile is within
;; HIT-RANGE of the (X,Y) of any invader in the ListOfInvaders

(check-expect (missile-hit? (make-missile 27 95) (cons (make-invader 30 100 6)
                                                       (cons (make-invader 10 20 3) empty)))  true)
(check-expect (missile-hit? (make-missile 27 95) (cons (make-invader 5 5 6)
                                                       (cons (make-invader 10 20 3) empty)))  false)

;; (define (hit? i lom) true) ;Stub
;; <Template taken from LOM data definition along with additional parameter>
(define (missile-hit? m loi)
  (cond [(empty? loi) false]
        [else
         (if (in-range? (first loi) m)
             true
             (missile-hit? m (rest loi)))]))

;; ------------

;; Missile -> Boolean
;; Return true if the missile's Y is -ve, i.e. it has moved passed the screen-top

(check-expect (missile-passed-top? M1) false)
(check-expect (missile-passed-top? M3) true)

;; (define (missile-passed-top? m) false)
;; <Template taken from Missile Data Definition>
(define (missile-passed-top? m)
  (< (missile-y m) 0))
;; ===================================================
;;              "Update-Missiles" Ends
;; ===================================================


;; ===================================================
;;              "Update-Tank" Starts
;; ===================================================
;; Tank -> Tank
;; Move tank left or right based on value of (tank-dir). When at either edge, tank remains there.

(check-expect (update-tank T0) (make-tank (+ (tank-x T0) TANK-SPEED)  1))
(check-expect (update-tank T2) (make-tank (- (tank-x T2) TANK-SPEED) -1))
(check-expect (update-tank T4) T4)
(check-expect (update-tank T6) T6)

;; (define (update-tank t) t) ;Stub
;; <Template taken from Tank Data Definition>
(define (update-tank t)
  (make-tank (final-tank-x (+ (tank-x t)
                              (cond [(= (tank-dir t) -1) (- TANK-SPEED)]
                                    [else TANK-SPEED])))
             (tank-dir t)))

;; Number -> Number
;; Given an X position of a tank, return the left/right screen point
;; if it has touched the left/right edges of the screen; otherwise
;; return the current X

(check-expect (final-tank-x (tank-x T0)) (tank-x T0))
(check-expect (final-tank-x (- TANK-LX 2)) TANK-LX)
(check-expect (final-tank-x (+ TANK-RX 1)) TANK-RX)

;; (define (final-tank-x tx) tx)
(define (final-tank-x tx)
  (cond [(< tx TANK-LX) TANK-LX]
        [(> tx TANK-RX) TANK-RX]
        [else tx]))

;; ===================================================
;;              "Update-Tank" Ends
;; ===================================================
;; ***********************************************************************************
;;                               "Update Game" Ends
;; ***********************************************************************************


;; ***********************************************************************************
;;                               "Render Game" Starts
;; ***********************************************************************************

;; Game -> Image
;; Render images of Invaders, Missiles, and a tank on correct positions on the background

(check-expect (render-game (make-game empty empty T0)) (render-invaders empty (render-missiles empty (render-tank T0))))
(check-expect (render-game (make-game  LOI3  LOM3 T1)) (render-invaders  LOI3 (render-missiles  LOM3 (render-tank T1))))

;; (define (render-game g) (place-image empty-image 0 0 BACKGROUND)) ;Stub
;; <Template based on helper functions>
(define (render-game g)
  (render-invaders (game-invaders g)
                   (render-missiles (game-missiles g)
                                    (render-tank (game-tank g)))))
;; ===================================================
;;              "Render-Tank" Starts
;; ===================================================
;; Tank -> Image
;; Place a tank image on the correct X position over the background

(check-expect (render-tank T0) (place-image TANK (tank-x T0) TANK-Y BACKGROUND))

;; (define (render-tank t) (place-image empty-image 0 0 BACKGROUND)) ;Stub
;; <Template taken from Tank Data Definition>
(define (render-tank t)
  (place-image TANK
               (tank-x t)
               TANK-Y
               BACKGROUND))
;; ===================================================
;;              "Render-Tank" Ends
;; ===================================================

;; ===================================================
;;              "Render-Missiles" Starts
;; ===================================================
;; ListOfMissiles Image -> Image
;; Draw missiles at the locations specified by the elements of
;; the List of Missiles, atop the Image provided

(check-expect (render-missiles (cons (make-missile 30 40) empty) (render-tank T0)) (place-image MISSILE 30 40 (render-tank T0)))

;; (define (render-missiles lom img) (place-image MISSILE 0 0 img)) ;Stub
;; <Template taken from LOM data definitino, with additional parameter>
(define (render-missiles lom img)
  (cond [(empty? lom) (place-image empty-image 0 0 img)]
        [else
         (place-image MISSILE
                      (missile-x (first lom))
                      (missile-y (first lom))
                      (render-missiles (rest lom) img))]))
;; ===================================================
;;              "Render-Missiles" Ends
;; ===================================================

;; ===================================================
;;              "Render-Invaders" Starts
;; ===================================================
;; ListOfInvaders Image -> Image
;; Draw invaders at the locations specified by the elements of
;; the List of invaders, atop the Image provided

(check-expect (render-invaders empty empty-image) (place-image empty-image 0 0 empty-image))
(check-expect (render-invaders (cons (make-invader 297 200 3)
                                     (cons (make-invader 2 290 -3) empty)) BACKGROUND)
              (place-image INVADER 297 200
                           (place-image INVADER 2 290
                                        (place-image empty-image 0 0 BACKGROUND))))

;; (define (render-invaders loi) (place-image INVADER 0 0 BACKGROUND))
;; <Template taken from LOI data definition with additional parameter>
(define (render-invaders loi img)
  (cond [(empty? loi) (place-image empty-image 0 0 img)]
        [else
         (place-image INVADER
                      (invader-x (first loi))
                      (invader-y (first loi))
                      (render-invaders (rest loi) img))]))
;; ===================================================
;;              "Render-Invaders" Ends
;; ===================================================
;; ***********************************************************************************
;;                               "Render Game" Ends
;; ***********************************************************************************



;; ***********************************************************************************
;;                               "Move and Fire" Starts
;; ***********************************************************************************
;; Game KeyEvent -> Game
;; Change the movement direction of tank left or right based on the key pressed
;; create a new missile at the middle of the tank when space is pressed

(check-expect (move-and-fire G3       " ") (add-missile G3)) 
(check-expect (move-and-fire G3   "right") (change-tank-dir G3 "right"))
(check-expect (move-and-fire G3    "left") (change-tank-dir G3  "left"))
(check-expect (move-and-fire G3  "escape") G3)

;;(define (move-and-fire g ke) g)
;; <Template taken from KeyEvent>
(define (move-and-fire g ke)
  (cond [(key=? ke " ") (add-missile g)]
        [(or (key=? ke "right") (key=? ke "left")) (change-tank-dir g ke)]
        [else g]))

;; ------------

;; Game -> Game
;; Add a missile to a ListOfMissiles within a game, at the same X as the Tank.

(check-expect (add-missile G3) (make-game (game-invaders G3)
                                          (cons (make-missile (tank-x (game-tank G3)) TANK-Y)
                                                (game-missiles G3))
                                          (game-tank G3)))

;; (define (add-missile G3) G3) ;Stub
;; <Template taken from Game data definition>
(define (add-missile g)
  (make-game (game-invaders g)
             (cons (make-missile (tank-x (game-tank g)) TANK-Y)
                   (game-missiles g))
             (game-tank g)))

;; ------------

;; Game -> Game
;; Change the tank's direction if right or left key is pressed

(check-expect (change-tank-dir G2 "right") (make-game (game-invaders G2)
                                                      (game-missiles G2)
                                                      (make-tank (tank-x (game-tank G2)) 1)))
(check-expect (change-tank-dir G3 "left") (make-game (game-invaders G3)
                                                     (game-missiles G3)
                                                     (make-tank (tank-x (game-tank G3)) -1)))

;; (define (change-tank-dir g ke) g) ;Stub
;; <Template taken from KeyEvent and Game Data Definitions>
(define (change-tank-dir g ke)
  (make-game (game-invaders g)
             (game-missiles g)
             (make-tank (tank-x (game-tank g))
                        (cond [(key=? ke "right") 1]
                              [else -1]))))
 
;; ***********************************************************************************
;;                               "Move and Fire" Ends
;; ***********************************************************************************



;; ***********************************************************************************
;;                               "invader-landed?" Starts
;; ***********************************************************************************

;; Game -> Boolean
;; Return True is any invader in the list of invaders reaches the ground.

(check-expect (invader-landed? G0) false)    ;No invaders
(check-expect (invader-landed? G4) true)     ;Invader crossed bottom
(check-expect (invader-landed? G3) true)     ;Invader reached bottom
(check-expect (invader-landed? G2) false)    ;Invader not at bottom

;; (define (invader-landed? g) false) ;Stub
;; <Template taken from Game data structure>
(define (invader-landed? g)
  (cond [(empty? (game-invaders g)) false]
        [else
         (cond [(>= (invader-y (first (game-invaders g))) GAME-STOP-Y) true]
               [else (invader-landed? (make-game (rest (game-invaders g))
                                                 (game-missiles g)
                                                 (game-tank g)))])]))

;; ***********************************************************************************
;;                               "invader-landed?" Ends
;; ***********************************************************************************