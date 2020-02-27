(ns br.com.souenzzo.summon-test.driver.pedestal
  (:require [io.pedestal.http :as http]
            [br.com.souenzzo.summon :as summon]))

(def start-server
  {::summon/output [::server]
   ::summon/start  (fn [env]
                     {::server (-> env
                                   http/create-server
                                   http/start)})
   ::summon/stop   (fn [{::keys [server]}]
                     (http/stop server))})

(def create-servlet
  {::summon/output [::server]
   ::summon/start  (fn [env]
                     {::server (-> env
                                   http/create-servlet)})})
