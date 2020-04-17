(ns br.com.souenzzo.todo-app
  (:require [clojure.spec.alpha :as s]
            [next.jdbc.specs :as jdbc.specs]))

(s/def ::entity-db ::jdbc.specs/db-spec)



(defn select-in
  [x selection]
  (letfn [(impl [x selection]
            (cond
              (not (vector? selection)) (impl x [selection])
              (map? x) (reduce (fn [[acc] sel]
                                 (cond
                                   (map? sel) (reduced [(into {}
                                                              (for [[k v] sel
                                                                    :let [vv (impl acc v)]
                                                                    :when vv]
                                                                [k (first vv)]))])
                                   (contains? acc sel) [(get acc sel)]))
                               [x]
                               selection)
              (coll? x) [(mapv #(select-in % selection) x)]
              :else [x]))]
    (first (impl x selection))))
