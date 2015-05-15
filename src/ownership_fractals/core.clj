(ns ownership-fractals.core
  (:require [quil.core :as q]
            [quil.middleware :as m]))

(def fake-colors
  {"adam" 40
   "ada" 110
   "bab" 130})

(def fake-ownership
  [["a.c" "adam" "60" "100"]
   ["a.c" "ada" "30" "100"]
   ["a.c" "bab" "10" "100"]])

(defn- as-ownership-fraction
  [row]
  (/ (bigint (get row 2))
     (bigint (get row 3))))

(defn- rows->colored-ownership
  [rows author-colors]
  (for [row rows
        :let [author (get row 1)
              color (author-colors author)
              ownership (as-ownership-fraction row)]]
    [author [color ownership]]))

(defn build-ownership-model
  [ownership-by-author author-colors]
  ; Set color mode to HSB (HSV) instead of default RGB.
  (q/color-mode :hsb)
  ; setup function returns initial state.
  (->>
   (rows->colored-ownership ownership-by-author author-colors)
   (into {})))

(def ^:const fractal-size 50)
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
  [colored-ownership]
  (q/background 240)
  (let [rows (sort-by-ownership (vals colored-ownership))
        indexed-rows (map-indexed vector rows)]
    (q/with-translation [(/ (q/width) 2)
                         (/ (q/height) 2)]
      (draw-fractals indexed-rows [fractal-size fractal-size])))
  (q/no-loop)) ; run once

(defn visualize
  [ownership]
  (q/defsketch ownership-fractals
    :title "Knowledge Ownership visualized by Fractal Figures"
    :size [500 500]
    :setup (partial build-ownership-model fake-ownership fake-colors)
    :draw draw-fractal-figures
    ; The functional-mode middleware allows us to inject 
    ; state into our draw function:
    :middleware [m/fun-mode]))
