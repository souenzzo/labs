(ns br.com.souenzzo.summon-test
  (:require [clojure.test :refer [deftest]]
            [br.com.souenzzo.summon :as summon]
            [br.com.souenzzo.summon-test.driver.datascript :as driver.datascript]
            [br.com.souenzzo.summon-test.driver.pathom :as driver.pathom]
            [br.com.souenzzo.summon-test.driver.pedestal :as driver.pedestal]
            [midje.sweet :refer [fact => contains just]]))

(def simple-system
  {::entity-schema   {}
   ::summon/drivers  {::driver.datascript/driver driver.datascript/driver
                      ::driver.pathom/driver     driver.pathom/driver
                      ::driver.pedestal/driver   driver.pedestal/start-server}
   ::summon/elements {::entity-conn-id {::summon/driver   ::driver.datascript/driver
                                        ::summon/requires {::driver.datascript/schema ::entity-schema}
                                        ::summon/provides {::entity-conn ::driver.datascript/conn}}
                      ::event-conn-id  {::summon/driver   ::driver.datascript/driver
                                        ::summon/requires {::driver.datascript/schema ::event-schema}
                                        ::summon/provides {::event-conn ::driver.datascript/conn}}
                      ::parser         {::summon/driver   ::driver.pathom/driver
                                        ::summon/requires {::event-conn ::event-conn}
                                        ::entity-conn     ::entity-conn
                                        ::summon/provides {::parser ::driver.pathom/parser}}}
   ::event-schema    {}})

(deftest simple-system-test
  (fact
    "Check if system is valid"
    (summon/valid? simple-system)
    => true)
  (fact
    "Check required globals"
    (summon/required-globals simple-system)
    => [::entity-schema
        ::event-schema])
  (fact
    "Check missing globals"
    (summon/explain-data (dissoc simple-system ::entity-schema))
    => [{::summon/issue ::summon/missing-global,
         ::summon/key   ::entity-schema}])
  (fact
    "Check missing drivers"
    (summon/explain-data (update simple-system ::summon/drivers dissoc ::driver.pathom/driver))
    => [{::summon/issue       ::summon/missing-driver,
         ::summon/driver      ::driver.pathom/driver
         ::summon/required-by ::parser}])
  (fact
    "Check missing globals"
    (summon/explain-data (-> simple-system
                             (update ::summon/elements dissoc ::event-conn-id)
                             (assoc ::event-conn {})))
    => [])
  (let [{::keys [parser] :as system} (summon/start simple-system)]
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
