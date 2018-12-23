(ns crokinole-score-keeper.core
  (:require [clojure.pprint :as pprint])
  (:import [org.opencv.core Core Mat Size]
           [org.opencv.imgproc Imgproc]
           [org.opencv.videoio VideoCapture VideoWriter Videoio]
           [org.opencv.imgcodecs Imgcodecs]))

(clojure.lang.RT/loadLibrary Core/NATIVE_LIBRARY_NAME)

#_(VideoWriter. "resources/out.mp4"
                (VideoWriter/fourcc \H \2 \6 \4)
                (.get video-in Videoio/CAP_PROP_FPS)
                (Size. (.get video-in Videoio/CAP_PROP_FRAME_WIDTH)
                       (.get video-in Videoio/CAP_PROP_FRAME_HEIGHT))
                true)

(defn center-circle
  "Find the circle closest to the center of the width and height.
  The result should be the 20 point ring on the board."
  [width height circles]
  (some->> circles
           (sort-by (fn [[x y _]]
                      (let [rx (- x (/ width 2))
                            ry (- y (/ height 2))]
                        (+ (Math/abs rx) (Math/abs ry)))))
           first))

(defn detect-circles
  [frame]
  (let [grayscale (volatile! (Mat.))
        discs (volatile! (Mat.))
        ring5 (volatile! (Mat.))
        ring10 (volatile! (Mat.))
        ring15 (volatile! (Mat.))
        _ (Imgproc/cvtColor frame @grayscale Imgproc/COLOR_BGR2GRAY)
        _ (Imgproc/HoughCircles @grayscale @discs
                                Imgproc/CV_HOUGH_GRADIENT
                                1 20 50 30
                                (/ (.width frame) 128)
                                (/ (.width frame) 64))
        discs (for [x (range (.cols @discs))]
                (vec (.get @discs 0 x)))
        [r20x r20y r20r] (center-circle (.width frame) (.height frame) discs)
        _ (Imgproc/HoughCircles @grayscale @ring15
                                Imgproc/CV_HOUGH_GRADIENT
                                1 100 15 50 (* r20r 6) (* r20r 8))
        _ (Imgproc/HoughCircles @grayscale @ring10
                                Imgproc/CV_HOUGH_GRADIENT
                                1 100 15 50 (* r20r 12) (* r20r 14))
        _ (Imgproc/HoughCircles @grayscale @ring5
                                Imgproc/CV_HOUGH_GRADIENT
                                1 100 15 50 (* r20r 21) (* r20r 22))]
    {:ring5 (vec (.get @ring5 0 0))
     :ring10 (vec (.get @ring10 0 0))
     :ring15 (vec (.get @ring15 0 0))
     :ring20 [r20x r20y r20r]
     :discs (remove #(= % [r20x r20y r20r]) discs)}))

(defn analyze-image
  ([]
   (analyze-image "resources/crokinole.jpg"))
  ([filename]
   (detect-circles (Imgcodecs/imread filename))))

(defn analyze-video
  ([]
   (analyze-video "resources/crokinole.mp4"))
  ([filename]
   (let [buffer (volatile! (Mat.))
         video-in (VideoCapture. filename)]
     (while (.read video-in @buffer)
       (detect-circles @buffer)
       #_(.write video-out @buffer))
     (.release video-in))))

(defn -main
  [& args]
  (analyze-video))
