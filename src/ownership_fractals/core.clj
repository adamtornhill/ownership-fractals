(ns ownership-fractals.core
  (:require [quil.core :as q]
            [quil.middleware :as m]))

(defn build-ownership-model
  [ownership-by-author]
  ; Set color mode to HSB (HSV) instead of default RGB.
  (q/color-mode :hsb)
  ; setup function returns initial state.
  {"adam" [40 0.60]
   "ada"  [120 0.30]
   "bab"  [130 0.10]})

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

(defn- draw-y-fractal
  [ownership space-left]
  (let [[x-left y-left] space-left
        x-width (side-length-from ownership figure-area y-left)]
    (q/rect (- fractal-size x-left) (- fractal-size y-left) x-width y-left)
    [(- x-left x-width) y-left]))

(defn- draw-x-fractal
  [ownership space-left]
  (let [[x-left y-left] space-left
        y-length (side-length-from ownership figure-area x-left)]
    (q/rect (- fractal-size x-left) (- fractal-size y-left) x-left y-length)
    [x-left (- y-left y-length)]))

(defn- pick-drawer
  [fractal-index]
  (if (even? fractal-index)
    draw-y-fractal
    draw-x-fractal))

(defn- draw-fractals
  [indexed-rows space-left]
  (when (seq indexed-rows)
    (let [[index row] (first indexed-rows)
          drawer (pick-drawer index)]
      (q/fill (color-of row) 255 255)
      (recur (rest indexed-rows)
             (drawer (ownership-of row) space-left)))))

(defn draw-fractal-figures
  [colored-ownership]
  ; Clear the sketch by filling it with light-grey color.
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
    :setup (partial build-ownership-model ownership)
    :draw draw-fractal-figures
    ; The functional-mode middleware allows us to inject 
    ; state into our draw function:
    :middleware [m/fun-mode]))
