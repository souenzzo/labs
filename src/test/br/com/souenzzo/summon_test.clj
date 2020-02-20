(ns br.com.souenzzo.summon-test
  (:require [clojure.test :refer [deftest]]
            [br.com.souenzzo.summon :as summon]
            [br.com.souenzzo.summon-test.driver.datascript :as driver.datascript]
            [br.com.souenzzo.summon-test.driver.pathom :as driver.pathom]
            [midje.sweet :refer [fact => contains just]]))


(deftest foo
  (let [elements [(assoc driver.datascript/driver
                    ::summon/id ::entity-conn-id
                    ::summon/requires {::driver.datascript/schema ::entity-schema}
                    ::summon/provides {::entity-conn ::driver.datascript/conn})
                  (assoc driver.datascript/driver
                    ::summon/id ::event-conn-id
                    ::summon/requires {::driver.datascript/schema ::event-schema}
                    ::summon/provides {::event-conn ::driver.datascript/conn})
                  (assoc driver.pathom/driver
                    ::summon/id ::parser
                    ::summon/requires {::event-conn  ::event-conn
                                       ::entity-conn ::entity-conn}
                    ::summon/provides {::parser ::driver.pathom/parser})]
        env {::entity-schema {}
             ::event-schema  {}}
        {::keys [parser]} (summon/start env elements)]
    (fact
      (fn? parser)
      => true)))

