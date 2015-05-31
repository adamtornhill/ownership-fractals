(ns ownership-fractals.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [clojure.string :as str]))

;;; Algorithm:
;;; =========
;;; We get CSV ownership data that looks like this:
;;;    Entity, Owner, Parts Owned, Total Parts
;;;    a.c,    adam,     50,          122
;;;    a.c,     ada,     25,          122
;;;    b.c,     bab,     10,           12
;;;    ...
;;;
;;; We transform that into a dictionary like:
;;; {a.c [[10N 50/122]
;;;       [20N 25/122]
;;;       [40N 10/122]]
;;; ...
;;;
;;; Within our dictionary (ordered by entity), we have a vector
;;; specifying the color and fraction of each author.

(defn- read-csv-from
  [file]
  (with-open [in-file (io/reader file)]
    (doall
     (csv/read-csv in-file))))

(defn- as-ownership-fraction
  [row]
  (/ (bigint (get row 2))
     (bigint (get row 3))))

(defn- color-picker-from
  [author-colors]
  (let [colors (into {} author-colors)]
    (fn [author]
      (if-let [color (colors author)]
        (bigint color)
        0))))

(defn- update-author-ownership
  [pick-color-of known-ownership row]
  (let [author (get row 1)
        color (pick-color-of author)
        ownership (as-ownership-fraction row)]
    (conj known-ownership
          [color ownership])))

(defn- update-ownership
  [owners pick-color-of row]
  (let [entity (get row 0)
        updater (partial update-author-ownership pick-color-of)]
    (update-in owners
               [entity]
               (fnil updater [])
               row)))

(defn- rows->entity-owners
  ([rows author-colors]
     (rows->entity-owners
      rows
      (color-picker-from author-colors)
      {}))
  ([rows pick-color-of owners]
     (if (seq rows)
       (recur
        (rest rows)
        pick-color-of
        (update-ownership owners pick-color-of (first rows)))
       owners)))

(defn initialize-sketch
  [entities-ownership]
  ; Set color mode to HSB (HSV) instead of default RGB.
  (q/color-mode :hsb)
  (q/background 250)
  (q/text-font (q/create-font "Courier" 10 true))
  ; setup function returns initial state.
  entities-ownership)

(def ^:const fractal-size 80)
(def ^:const figure-area (* fractal-size fractal-size))
(def ^:const figures-per-row 10)
(def ^:const padding 50)

(defn- drawing-size
  [n-rows]
  (let [y-rows (+ (quot n-rows figures-per-row)
                  (if (= 0 (mod n-rows figures-per-row)) 0 1))]
    [(+ (* figures-per-row fractal-size) (* (inc figures-per-row) padding))
     (+ (* y-rows fractal-size) (* (inc y-rows) padding))]))

(defn- fractal-position-of-index
  [n]
  (let [xpos (mod n figures-per-row)
        ypos (quot n figures-per-row)]
    [(+ (* xpos fractal-size) (* (inc xpos) padding))
     (+ (* ypos fractal-size) (* (inc ypos) padding))]))

(defn- color-of
  [row]
  (get row 0))

(defn- ownership-of
  [row]
  (get row 1))

(defn- sort-by-ownership
  [rows]
  (sort-by ownership-of > rows))

(defn- side-length-from
  [ownership area side-left]
  (/ (* ownership area) side-left))

(defn- draw-rect
  [space-left x y]
  (q/rect (- fractal-size (first space-left))
          (- fractal-size (second space-left))
          x
          y))

(defn- draw-y-fractal
  [ownership space-left]
  (let [[x-left y-left] space-left
        x-width (side-length-from ownership figure-area y-left)]
    (draw-rect space-left x-width y-left)
    [(- x-left x-width) y-left]))

(defn- draw-x-fractal
  [ownership space-left]
  (let [[x-left y-left] space-left
        y-length (side-length-from ownership figure-area x-left)]
    (draw-rect space-left x-left y-length)
    [x-left (- y-left y-length)]))

(defn- pick-drawer-for
  [fractal-index]
  (if (even? fractal-index)
    draw-y-fractal
    draw-x-fractal))

(defn- draw-fractals
  [indexed-rows space-left]
  (when (seq indexed-rows)
    (let [[index row] (first indexed-rows)
          drawer (pick-drawer-for index)]
      (q/fill (color-of row) 255 255)
      (recur (rest indexed-rows)
             (drawer (ownership-of row) space-left)))))

(defn- strip-path-from
  [entity]
  (->
   (str/split entity #"/|\\")
   last))

(defn draw-fractal-figures
  [entities-ownership]
  (doseq [[n [entity colored-ownership]] (map-indexed vector entities-ownership)]
    (let [position (fractal-position-of-index n)
          rows (sort-by-ownership colored-ownership)
          indexed-rows (map-indexed vector rows)]
      (q/with-translation position
        (q/fill 0) ; fill sets the text color
        (q/text (strip-path-from entity) 0 (- 0 (/ padding 2)))
        (draw-fractals indexed-rows [fractal-size fractal-size]))))
  (q/no-loop)) ; run once

(defn visualize
  [ownership-file color-file]
  (let [ownership (read-csv-from ownership-file)
        colors (read-csv-from color-file)
        entities-ownership (rows->entity-owners ownership colors)]
    (q/defsketch ownership-fractals
      :title (str "Knowledge Ownership visualized as Fractal Figures")
      :size (drawing-size (count entities-ownership))
      :setup (partial initialize-sketch entities-ownership)
      :draw draw-fractal-figures
      ; The functional-mode middleware allows us to inject 
      ; state into our draw function:
      :middleware [m/fun-mode])))
