(ns crokinole-score-keeper.core
  (:require [clojure.pprint :as pprint]
            [clojure.set :refer [map-invert]]
            [crokinole-score-keeper.color :as color])
  (:import [org.opencv.core Core Mat Size]
           [org.opencv.imgproc Imgproc]
           [org.opencv.videoio VideoCapture]
           [org.opencv.imgcodecs Imgcodecs]))

(clojure.lang.RT/loadLibrary Core/NATIVE_LIBRARY_NAME)

(defonce frames (atom 0))

(defn- hypotenuse [x y] (Math/sqrt (+ (Math/pow x 2) (Math/pow y 2))))

(defn- point-discs
  [discs rings]
  (mapv (fn [{:keys [coords] :as disc}]
          (reduce (fn [accl [ring-label [ring-x ring-y ring-r]]]
                    (let [[x y r] coords
                          side-x (Math/abs (- ring-x x))
                          side-y (Math/abs (- ring-y y))]
                      (if (< (hypotenuse side-x side-y)
                             (- ring-r r))
                        (assoc accl :points ring-label)
                        accl)))
                  disc
                  rings))
        discs))

(defn- normalize-discs-color
  [discs colors-cluster]
  (let [colors-lookup (->> (reduce (fn [accl [centroid colors]]
                                     (conj accl (reduce #(conj %1 %2 centroid)
                                                        []
                                                        colors)))
                                   []
                                   colors-cluster)
                           (apply concat)
                           (apply hash-map))]
    (mapv (fn [[x y radius [r g b]]]
            {:coords [x y radius]
             :color (get colors-lookup [r g b] [r g b])})
          discs)))

(defn- detect-circles
  [frame]
  (with-local-vars [grayscale (Mat.)
                    blur (Mat.)
                    discs-buffer (Mat.)
                    ring15 (Mat.)]
    (let [_ (Imgproc/cvtColor frame @grayscale Imgproc/COLOR_BGR2GRAY)
          _ (Imgproc/GaussianBlur frame @blur (Size. 5 5) 0 0)
          _ (Imgproc/HoughCircles @grayscale @discs-buffer
                                  Imgproc/CV_HOUGH_GRADIENT
                                  1 20 50 30
                                  (/ (.width frame) 128)
                                  (/ (.width frame) 64))
          discs (->> (for [x (range (.cols @discs-buffer))]
                       (vec (.get @discs-buffer 0 x)))
                     (mapv (fn [[x y radius]]
                             (let [[b g r] (vec (.get @blur y x))]
                               [x y radius [r g b]]))))
          r20r (/ (reduce + (map (fn [disc] (nth disc 2)) discs))
                  (count discs))
          _ (Imgproc/HoughCircles @grayscale @ring15
                                  Imgproc/CV_HOUGH_GRADIENT
                                  1 100 15 50 (* r20r 6) (* r20r 8))
          ring15 (vec (.get @ring15 0 0))
          ring15-coords (-> ring15 drop-last vec)]
      {5 (conj ring15-coords (* (last ring15) 3))
       10 (conj ring15-coords (* (last ring15) 2))
       15 ring15
       20 (conj ring15-coords
                (/ (last ring15) (* (/ 4 5.5) 8)))
       :discs discs})))

(defn- process-frame
  [frame]
  (let [circles (detect-circles frame)
        colors-cluster (color/cluster (mapv last (:discs circles)))]
    (-> circles
        (update-in [:discs] normalize-discs-color colors-cluster)
        (update-in [:discs] point-discs (select-keys circles [5 10 15 20]))
        (assoc :players (apply hash-map (interleave [:player1 :player2]
                                                    (keys colors-cluster)))
               :dimensions [(.width frame) (.height frame)]))))

(defn analyze-image
  [filename]
  (reset! frames [])
  (process-frame (Imgcodecs/imread filename)))

(defn analyze-video
  [filename]
  (reset! frames [])
  (with-local-vars [buffer (Mat.)]
    (while (.read (VideoCapture. filename) @buffer)
      (swap! frames conj (process-frame @buffer))))
  @frames)

(defn -main
  [& args]
  (pprint/pprint (reduce (fn [accl filename]
                           (assoc accl filename
                                  (analyze-image filename)))
                         {}
                         ["resources/sample1.png"
                          "resources/sample2.png"
                          "resources/sample3.png"
                          "resources/sample3_nocenter.png"
                          "resources/crokinole.jpg"])))
