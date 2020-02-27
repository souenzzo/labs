(ns br.com.souenzzo.summon-test.driver.pathom
  (:require [com.wsscode.pathom.core :as p]
            [br.com.souenzzo.summon :as summon]))

(def driver
  {::summon/output [::parser]
   ::summon/start  (fn [env]
                     {::parser (p/parser env)})
   ::summon/stop   (fn [env]
                     (dissoc env ::parser))})
