(ns br.com.souenzzo.summon-test.driver.datascript
  (:require [br.com.souenzzo.summon :as summon]
            [datascript.core :as ds]))

(def driver
  {::summon/input  [::schema]
   ::summon/output [::conn]
   ::summon/start  (fn [{::keys [schema]}]
                     {::conn (ds/create-conn schema)})
   ::summon/stop   (fn [env]
                     (dissoc env ::conn))})
