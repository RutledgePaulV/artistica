(ns artistica.core
  (:require [clojure.java.io :as io])
  (:import (javax.imageio ImageIO)
           (java.io InputStream OutputStream)
           (java.awt.image BufferedImage)
           (java.awt Color)))


(defn dims [^BufferedImage img]
  [(.getWidth img) (.getHeight img)])

(defn tiles [nx ny width height]
  (let [x-step (quot width nx)
        y-step (quot height ny)]
    (for [x (range 0 width x-step)
          y (range 0 height y-step)]
      [x y (+ x x-step) (+ y y-step)])))

(defn distance [[x1 y1] [x2 y2]]
  (let [dx (- x2 x1) dy (- y2 y1)]
    (Math/sqrt (+ (* dx dx) (* dy dy)))))

(defn convex-hull [points]
  (let [cc         (sort-by identity points)
        x          #(first %)
        y          #(second %)
        ccw        #(- (* (- (x %2) (x %1)) (- (y %3) (y %1))) (* (- (y %2) (y %1)) (- (x %3) (x %1))))
        half-hull  #(loop [h [] c %]
                      (if (not (empty? c))
                        (if (and (> (count h) 1) (<= 0 (ccw (h (- (count h) 2)) (h (- (count h) 1)) (first c))))
                          (recur (vec (butlast h)) c)
                          (recur (conj h (first c)) (rest c)))
                        h))
        upper-hull (butlast (half-hull cc))
        lower-hull (butlast (half-hull (reverse cc)))]
    (vec (concat upper-hull lower-hull))))

(defn veronoi [w h seeds]
  (let [verbose
        (reduce (fn [agg [[k v] & _]]
                  (if (contains? agg k)
                    (update agg k conj v)
                    (assoc agg k [v]))) {}
                (for [x (range 0 w) y (range 0 h)]
                  (let [matching-seed
                        (->> (map #(hash-map :distance (distance [x y] %) :seed %) seeds)
                             (apply min-key :distance)
                             (:seed))]
                    {matching-seed [x y]})))]
    (into {} (for [[k v] verbose] [k (convex-hull v)]))))

(defn unique [f]
  (let [seen (atom #{})]
    (fn [& args]
      (->> (repeatedly #(apply f args))
           (drop-while #(contains? (first (swap-vals! seen conj %)) %))
           (first)))))

(defn get-color []
  (Color. ^int (rand-int 255)
          ^int (rand-int 255)
          ^int (rand-int 255)))

(defn draw-background [^BufferedImage image ^Color color]
  (let [g        (.getGraphics image)
        original (.getColor g)]
    (.setColor g color)
    (.drawRect g 0 0 (.getWidth image) (.getHeight image))
    (.setColor g original)))

(defn seeds
  ([w h] (repeatedly (fn [] [(rand-int w) (rand-int h)])))
  ([n w h] (take n (seeds w h))))

(defn colorized [w h seeds output]
  (let [result    (BufferedImage. w h BufferedImage/TYPE_3BYTE_BGR)
        regions   (veronoi w h seeds)
        graphics  (.getGraphics result)
        colorizer (unique get-color)]
    (draw-background result Color/WHITE)
    (doseq [[k vs] regions]
      (let [xs (int-array (map first vs))
            ys (int-array (map second vs))]
        (.setColor graphics (colorizer))
        (.fillPolygon graphics xs ys (count vs))))
    (with-open [out ^OutputStream output]
      (ImageIO/write result "png" out))))

(defn generate-puzzle [^BufferedImage img output]
  (let [[w h] (dims img)
        tiling         (tiles 10 5 w h)
        transpositions (zipmap tiling (shuffle tiling))
        result         (BufferedImage. w h (.getType img))
        tgt-g          (.getGraphics result)]
    (doseq [[[sx1 sy1 sx2 sy2] [tx1 ty1 tx2 ty2]] transpositions]
      (.drawImage tgt-g img tx1 ty1 tx2 ty2 sx1 sy1 sx2 sy2 Color/WHITE nil))
    (with-open [out ^OutputStream output]
      (ImageIO/write result "png" out))))

(defn ^InputStream resource [path]
  (io/input-stream (io/resource path)))

(defn load-image [path]
  (with-open [stream (resource path)]
    (ImageIO/read stream)))

