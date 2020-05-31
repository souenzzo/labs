(ns user
  (:require [clojure.java.io :as io]
            [shadow.cljs.devtools.server :as shadow.server]
            [br.com.souenzzo.my-clj-admin :as my-clj-admin]
            [shadow.cljs.devtools.api :as shadow.api]))

(defn my-clj-admin
  []
  (shadow.server/start!)
  (shadow.api/watch :my-clj-admin)
  (my-clj-admin/-main))
