(ns crokinole-score-keeper.core
  (:import [org.opencv.core Core Mat Size]
           [org.opencv.videoio VideoCapture VideoWriter Videoio]))

(clojure.lang.RT/loadLibrary Core/NATIVE_LIBRARY_NAME)

(def video-in (VideoCapture. "resources/crok.mp4"))
(def video-out (VideoWriter. "resources/out.mp4"
                             (VideoWriter/fourcc \H \2 \6 \4)
                             (.get video-in Videoio/CAP_PROP_FPS)
                             (Size. (.get video-in Videoio/CAP_PROP_FRAME_WIDTH)
                                    (.get video-in Videoio/CAP_PROP_FRAME_HEIGHT))
                             true))

(defn -main
  [& args]
  (loop [i 0]
    (let [buffer (volatile! (Mat.))]
      (when (.read video-in @buffer)
        (.write video-out @buffer)
        (recur (inc i)))))
  (.release video-in)
  (.release video-out))
