(ns br.com.souenzzo.omniapp.sample-test
  (:require [clojure.test :refer [deftest]]
            [midje.sweet :refer [fact => contains just]]
            [br.com.souenzzo.omniapp.sample :as omniapp.sample]
            [br.com.souenzzo.omniapp.http :as omniapp.http]
            [br.com.souenzzo.omniapp.testing :as omnitest]))

(deftest app
  (let [app (omniapp.sample/->app {})]
    (fact
      (omnitest/dispatch app
                         ::omniapp.sample/parser
                         [::omniapp.sample/hello])
      => {::omniapp.sample/hello "ok"})))
