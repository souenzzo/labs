(ns user
  (:require [clojure.java.io :as io])
  (:import (org.apache.poi.xslf.usermodel XMLSlideShow XSLFSlide XSLFTextParagraph XSLFTextShape)))

(defn slide-show
  [x]
  (new XMLSlideShow (io/input-stream x)))

(defn shapes
  [^XMLSlideShow slide-show]
  (->> (.getSlides slide-show)
       (mapcat #(.getShapes ^XSLFSlide %))))

(defn texts
  [x]
  (->> x
       (shapes)
       (map #(.getText ^XSLFTextShape %))))
