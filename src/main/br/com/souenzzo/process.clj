(ns br.com.souenzzo.process
  (:require [clojure.core.async :as async]))


(defprotocol IProcess
  (pid [this])
  (alive? [this]))

(extend-protocol IProcess
  Process
  (pid [this] (.pid this))
  (alive? [this] (.isAlive this)))

(defn execute
  [{::keys [command
            timeout
            directory
            buffer-size
            on-stdout
            on-stderr]
    :or    {timeout     100
            buffer-size 64}}]
  (let [pb (ProcessBuilder. ^"[Ljava.lang.String;" (into-array command))
        pb (if directory
             (.directory pb directory)
             pb)
        p (.start pb)
        #_#_stdin (.getOutputStream p)]
    (when on-stdout
      (async/thread
        (let [stdout (.getInputStream p)
              stdout-buffer (byte-array buffer-size)]
          (loop []
            (let [len (.read stdout stdout-buffer 0 buffer-size)]
              (when (pos? len)
                (on-stdout (slurp stdout-buffer))
                (when (< len buffer-size)
                  (async/<!! (async/timeout timeout)))
                (recur)))))))
    (when on-stderr
      (async/thread
        (let [stderr (.getErrorStream p)
              stderr-buffer (byte-array buffer-size)]
          (loop []
            (let [len (.read stderr stderr-buffer 0 buffer-size)]
              (when (pos? len)
                (on-stderr (slurp stderr-buffer))
                (when (< len buffer-size)
                  (async/<!! (async/timeout timeout)))
                (recur)))))))
    p))