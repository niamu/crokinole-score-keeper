(ns crokinole-score-keeper.color)

(defn- pow [x n] (Math/pow x n))

(defn- color-diff
  "Calculate the distance between two colors"
  [[r1 g1 b1] [r2 g2 b2]]
  (Math/sqrt (+ (pow (- r1 r2) 2)
                (pow (- g1 g2) 2)
                (pow (- b1 b2) 2))))

(defn- avg
  [colors]
  (mapv #(Math/sqrt %)
        [(/ (reduce + (map (fn [[r _ _]] (pow r 2)) colors)) (count colors))
         (/ (reduce + (map (fn [[_ g _]] (pow g 2)) colors)) (count colors))
         (/ (reduce + (map (fn [[_ _ b]] (pow b 2)) colors)) (count colors))]))

(defn- k-means
  [coll centroids distance-fn avg-fn iterations]
  (let [result (reduce (fn [accl color]
                         (let [[centroid distance]
                               (->> centroids
                                    (map #(vector % (distance-fn % color)))
                                    (sort-by last)
                                    first)]
                           (update accl centroid conj color)))
                       (apply hash-map
                              (interleave centroids
                                          (repeat (count centroids) [])))
                       coll)]
    (if (> iterations 1)
      (k-means (apply concat (vals result))
               (map (fn [[centroid values]] (avg values)) result)
               distance-fn
               avg-fn
               (dec iterations))
      result)))

(defn cluster
  [colors]
  (let [sorted-colors (sort-by (partial color-diff [0 0 0]) colors)]
    (k-means colors
             [(first sorted-colors) (last sorted-colors)]
             color-diff
             avg
             1)))
