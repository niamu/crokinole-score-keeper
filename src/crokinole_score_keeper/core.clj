(ns crokinole-score-keeper.core
  (:require [clojure.pprint :as pprint]
            [clojure.set :refer [map-invert]]
            [crokinole-score-keeper.color :as color])
  (:import [org.opencv.core Core Mat Size]
           [org.opencv.imgproc Imgproc]
           [org.opencv.videoio VideoCapture VideoWriter Videoio]
           [org.opencv.imgcodecs Imgcodecs]))

(clojure.lang.RT/loadLibrary Core/NATIVE_LIBRARY_NAME)

(defonce frames (atom 0))

(defn detect-circles
  [frame]
  (let [grayscale (volatile! (Mat.))
        blur (volatile! (Mat.))
        discs (volatile! (Mat.))
        ring15 (volatile! (Mat.))
        _ (Imgproc/cvtColor frame @grayscale Imgproc/COLOR_BGR2GRAY)
        _ (Imgproc/GaussianBlur frame @blur (Size. 5 5) 0 0)
        _ (Imgproc/HoughCircles @grayscale @discs
                                Imgproc/CV_HOUGH_GRADIENT
                                1 20 50 30
                                (/ (.width frame) 128)
                                (/ (.width frame) 64))
        normalized-discs (->> (for [x (range (.cols @discs))]
                                (vec (.get @discs 0 x)))
                              (mapv (fn [[x y radius]]
                                      (let [[b g r] (vec (.get @blur y x))]
                                        [x y radius [r g b]]))))
        colors-lookup (->> (reduce (fn [accl [centroid colors]]
                                     (conj accl (reduce #(conj %1 %2 centroid)
                                                        []
                                                        colors)))
                                   []
                                   (color/cluster (mapv last normalized-discs)))
                           (apply concat)
                           (apply hash-map))
        r20r (/ (reduce + (map (fn [disc] (nth disc 2)) normalized-discs))
                (count normalized-discs))
        _ (Imgproc/HoughCircles @grayscale @ring15
                                Imgproc/CV_HOUGH_GRADIENT
                                1 100 15 50 (* r20r 6) (* r20r 8))
        ring15 (vec (.get @ring15 0 0))
        ring15-coords (-> ring15 drop-last vec)]
    {:dimensions [(.width frame) (.height frame)]
     :ring5 (conj ring15-coords (* (last ring15) 3))
     :ring10 (conj ring15-coords (* (last ring15) 2))
     :ring15 ring15
     :ring20 (conj ring15-coords
                   (/ (last ring15) (* (/ 4 5.5) 8)))
     :discs normalized-discs
     :colors-lookup colors-lookup}))

(defn analyze-image
  ([]
   (analyze-image "resources/crokinole.jpg"))
  ([filename]
   (detect-circles (Imgcodecs/imread filename))))

(defn analyze-video
  ([]
   (analyze-video "resources/crokinole.mp4"))
  ([filename]
   (reset! frames 0)
   (let [buffer (volatile! (Mat.))
         video-in (VideoCapture. filename)]
     (while (.read video-in @buffer)
       (swap! frames inc)
       (detect-circles @buffer))
     (.release video-in))))

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
