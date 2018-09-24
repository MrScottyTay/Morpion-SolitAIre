(require '[clojure.pprint :as p])

;;;; An attempt to make an AI that will play Morpion Solitaire and aim for the highest score possible

;; Basic cell structure
(defn grid-cell [coordinates] 
    {   :coordinates coordinates
        :cross false
        :possible-connections {:n 0 :ne 0 :e 0 :se 0 :s 0 :sw 0 :w 0 :nw 0}
        :connections {:n false :ne false :e false :se false :s false :sw false :w false :nw false} })

;; Grid Helper functions
(defn coordinates-keyword [coordinates]
    (let [x (first coordinates), y (second coordinates)]
        (keyword (str x "x" y "y"))))

(defn get-cell [grid coordinates]
    (if (contains? grid (coordinates-keyword coordinates))
        ((coordinates-keyword coordinates) grid)
        (grid-cell coordinates)))

(defn replace-cell [grid coordinates new-cell]
    (assoc grid (coordinates-keyword coordinates) new-cell))

(declare add-possible-connections)
(declare get-possible-connection-coordinates)
(defn add-cross [grid coordinates]
    (add-possible-connections
        (replace-cell grid coordinates (assoc (get-cell grid coordinates) :cross true)) 
        (get-possible-connection-coordinates coordinates)))

(declare add-possible-connection)
(declare add-possible-connections-)
(defn add-possible-connections [grid coordinates-list]
    (add-possible-connections- grid (first coordinates-list) (rest coordinates-list)))
(defn add-possible-connections- [grid position coordinates-list]
    (if (empty? coordinates-list)
        (add-possible-connection grid (first position) (second position))
        (add-possible-connections- (add-possible-connection grid (first position) (second position)) (first coordinates-list) (rest coordinates-list))))
(defn add-possible-connection [grid coordinates direction]
    (let [cell (get-cell grid coordinates)]
        (replace-cell grid coordinates (assoc cell (keyword direction) (inc ((keyword direction) cell))))))

;; Get possible connection coordinates, along with the direction towards the origin
;; Example node in the returned list ([x y] origin-direction)
(declare get-possible-connection-coordinates-straight)
(declare get-possible-connection-coordinates-diagonal)
(defn get-possible-connection-coordinates [coordinates]
    (concat (get-possible-connection-coordinates-straight coordinates) (get-possible-connection-coordinates-diagonal coordinates)))

;; Gets the possible connection coordinates on straight axes
(declare get-possible-connection-coordinates-straight-)
(defn get-possible-connection-coordinates-straight [coordinates]
    (concat (get-possible-connection-coordinates-straight- coordinates "e" 'x -4 ()) (get-possible-connection-coordinates-straight- coordinates "s" 'y -4 ())))
(defn get-possible-connection-coordinates-straight- [coordinates cardinal plane position return-list]
    (cond
        (= position 5)
        return-list

        (= position 0)
        (if (= plane 'x)
            (get-possible-connection-coordinates-straight- coordinates "w" plane (inc position) return-list)
            (get-possible-connection-coordinates-straight- coordinates "n" plane (inc position) return-list))

        :else
        (if (= plane 'x)
            (get-possible-connection-coordinates-straight- coordinates cardinal plane (inc position)
                (conj return-list (list [ (+ (first coordinates) position) (second coordinates)] cardinal)))
            (get-possible-connection-coordinates-straight- coordinates cardinal plane (inc position)
                (conj return-list (list [ (first coordinates) (+ (second coordinates) position)] cardinal))) )))

;; Gets the possible connection coordinates on diagonal axes, 
(declare get-possible-connection-coordinates-diagonal-)
(defn get-possible-connection-coordinates-diagonal [coordinates]
    (concat (get-possible-connection-coordinates-diagonal- coordinates "se" 'top [-4 -4] ()) (get-possible-connection-coordinates-diagonal- coordinates "ne" 'bottom [-4 4] ())) )
(defn get-possible-connection-coordinates-diagonal- [coordinates cardinal origin position return-list]
    (cond
        (or (= position [5 5]) (= position [5 -5]))
        return-list

        (= position [0 0])
        (if (= origin 'top)
            (get-possible-connection-coordinates-diagonal- coordinates "nw" origin [(inc (first position)) (inc (second position))] return-list)
            (get-possible-connection-coordinates-diagonal- coordinates "sw" origin [(inc (first position)) (dec (second position))] return-list))

        :else
        (let [new-coordinate (list [(+ (first coordinates) (first position)) (+ (second coordinates) (second position))] cardinal)]
        (if (= origin 'top)
            (get-possible-connection-coordinates-diagonal- coordinates cardinal origin [(inc (first position)) (inc (second position))] (conj return-list new-coordinate))
            (get-possible-connection-coordinates-diagonal- coordinates cardinal origin [(inc (first position)) (dec (second position))] (conj return-list new-coordinate)) ))))

;; Creating start grid
(def start-pattern-template
    '[[3 0] [4 0] [5 0] [6 0] [3 1] [6 1] [3 2] [6 2] [0 3] [1 3] [2 3] [3 3] [6 3] [7 3] [8 3] [9 3] [0 4] [9 4]
      [0 5] [9 5] [0 6] [1 6] [2 6] [3 6] [6 6] [7 6] [8 6] [9 6] [3 7] [6 7] [2 8] [6 8] [3 9] [4 9] [5 9] [6 9]])

(declare create-grid-)
(defn create-grid []
    (create-grid- {} (first start-pattern-template) (rest start-pattern-template)))
(defn create-grid- [grid current-cross pattern-template]
    (if (empty? pattern-template)
        (add-cross grid current-cross)
        (create-grid- (add-cross grid current-cross) (rest pattern-template))))

(p/pprint (create-grid))