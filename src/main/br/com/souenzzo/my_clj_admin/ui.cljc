(ns br.com.souenzzo.my-clj-admin.ui
  (:require ["react" :as r]
            [goog.dom :as gdom]
            ["react-dom" :as rd]))

(defn hello
  []
  (r/createElement "p" #js {} "ok"))

(defn ^:export main
  []
  (let [target (gdom/getElement "target")]
    (rd/render (hello)
               target)))

(defn after-load
  []
  (let [target (gdom/getElement "target")]
    (rd/render (hello)
               target)))
