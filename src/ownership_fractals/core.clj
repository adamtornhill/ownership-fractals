(ns ownership-fractals.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [clojure.data.csv :as csv]
            [clojure.java.io :as io]))

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
        (rand-int 256)))))

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

(defn build-ownership-model
  [ownership-by-author author-colors]
  ; Set color mode to HSB (HSV) instead of default RGB.
  (q/color-mode :hsb)
  ; setup function returns initial state.
  (rows->entity-owners
   (read-csv-from ownership-by-author)
   (read-csv-from author-colors)))

(def ^:const fractal-size 150)
(def ^:const figure-area (* fractal-size fractal-size))

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

(defn draw-fractal-figures
  [entity-to-visualize entities-ownership]
  (q/background 250)
  (let [colored-ownership (entities-ownership entity-to-visualize)
        rows (sort-by-ownership colored-ownership)
        indexed-rows (map-indexed vector rows)]
    (q/with-translation [(- (/ (q/width) 2) (/ fractal-size 2))
                         (- (/ (q/height) 2) (/ fractal-size 2))]
      (draw-fractals indexed-rows [fractal-size fractal-size])))
  (q/no-loop)) ; run once

(defn visualize
  [ownership-file color-file entity-to-visualize]
  (q/defsketch ownership-fractals
    :title (str "Knowledge Ownership of '" entity-to-visualize "' as Fractal Figures")
    :size [500 500]
    :setup (partial build-ownership-model ownership-file color-file)
    :draw (partial draw-fractal-figures entity-to-visualize)
    ; The functional-mode middleware allows us to inject 
    ; state into our draw function:
    :middleware [m/fun-mode]))
