(ns br.com.souenzzo.summon-test
  (:require [clojure.test :refer [deftest]]
            [br.com.souenzzo.summon :as summon]
            [br.com.souenzzo.summon-test.driver.datascript :as driver.datascript]
            [br.com.souenzzo.summon-test.driver.pathom :as driver.pathom]
            [br.com.souenzzo.summon-test.driver.pedestal :as driver.pedestal]
            [midje.sweet :refer [fact => contains just]]))

(def system
  {::entity-schema   {}
   ::summon/drivers  {::driver.datascript/driver driver.datascript/driver
                      ::driver.pathom/driver     driver.pathom/driver
                      ::driver.pedestal/driver   driver.pedestal/start-server}
   ::summon/elements [{::summon/id       ::entity-conn-id
                       ::summon/driver   ::driver.datascript/driver
                       ::summon/requires {::driver.datascript/schema ::entity-schema}
                       ::summon/provides {::entity-conn ::driver.datascript/conn}}
                      {::summon/id       ::event-conn-id
                       ::summon/driver   ::driver.datascript/driver
                       ::summon/requires {::driver.datascript/schema ::event-schema}
                       ::summon/provides {::event-conn ::driver.datascript/conn}}
                      {::summon/id       ::parser
                       ::summon/driver   ::driver.pathom/driver
                       ::summon/requires {::event-conn  ::event-conn
                                          ::entity-conn ::entity-conn}
                       ::summon/provides {::parser ::driver.pathom/parser}}]
   ::event-schema    {}})

(deftest system-test
  (fact
    "Check if system is valid"
    (summon/valid? system)
    => true)
  (let [{::keys [parser] :as system} (summon/start system)]
    (fact
      "Check if parser is started"
      (fn? parser)
      => true)
    (fact
      "Check if application was stopped"
      (keys (summon/stop system))
      => [::entity-schema
          ::summon/drivers
          ::summon/elements
          ::event-schema])))
