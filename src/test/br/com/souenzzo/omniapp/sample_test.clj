(ns br.com.souenzzo.omniapp.sample-test
  (:require [clojure.test :refer [deftest]]
            [midje.sweet :refer [fact => contains just]]
            [com.stuartsierra.component :as component]
            [br.com.souenzzo.omniapp.sample :as omniapp.sample]
            [br.com.souenzzo.omniapp.http :as omniapp.http]
            [br.com.souenzzo.omniapp.testing :as omnitest])
  (:import (clojure.lang Associative IPersistentCollection IMeta IPersistentMap)
           (java.io Writer)))

(deftest app
  (let [app (omniapp.sample/->app {})]
    (fact
      (omnitest/dispatch app
                         ::omniapp.sample/parser
                         [::omniapp.sample/hello])
      => {::omniapp.sample/hello "ok"})))


(defn my-comp
  [{::keys [id]
    :as    kv}]
  (reify
    component/Lifecycle
    (start [this]
      (prn [:start (name id) this])
      this)
    (stop [this]
      (prn [:stop (name id) this])
      this)
    Associative
    (containsKey [this key]
      (contains? kv key))
    (entryAt [this key]
      (get kv key))
    (assoc [this key val]
      (my-comp (assoc kv key val)))
    IPersistentCollection
    (equiv [this o]
      (= kv o))
    (count [this]
      (count kv))
    (cons [this o]
      (my-comp (cons kv o)))
    (empty [this]
      (my-comp {::id id}))
    Object
    (toString [this]
      (str kv))))

(deftest components
  (comment
    (let [system (-> (component/system-map
                       ::rest1 (my-comp {::id :rest1})
                       ::rest2 (my-comp {::id :rest2})
                       ::gql (my-comp {::id :gql})
                       ::db1 (my-comp {::id :db1})
                       ::db2 (my-comp {::id :db2}))
                     (component/system-using
                       {::rest1 {:db ::db1}
                        ::rest2 {:db ::db2}
                        ::db2   {:db ::db1}
                        ::gql   {:db1 ::db1
                                 :db2 ::db2}}))
          inits (for [[from component] system
                      :let [edges (component/dependencies component)]
                      [label to] edges
                      inits [[from {:label (pr-str from)}]
                             [to {:label (pr-str to)}]
                             [from
                              to
                              {:label (pr-str label)}]]]
                  inits)]
      (-> (apply uber/digraph inits)
          (doto (uber/viz-graph))
          (uber.alg/topsort ::rest2))))
  (let [system (-> (component/system-map
                     ::rest1 (my-comp {::id ::rest1})
                     ::rest2 (my-comp {::id ::rest2})
                     ::gql (my-comp {::id ::gql})
                     ::db1 (my-comp {::id ::db1})
                     ::db2 (my-comp {::id ::db2}))
                   (component/system-using
                     {::rest1 {:db ::db1}
                      ::rest2 {:db ::db2}
                      ::db2   {:db ::db1}
                      ::gql   {:db1 ::db1
                               :db2 ::db2}}))]
    (fact
      (component/start-system system [::rest1 ::db1])
      => {})))
