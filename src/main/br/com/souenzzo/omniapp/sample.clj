(ns br.com.souenzzo.omniapp.sample
  (:require [br.com.souenzzo.omniapp :as omniapp]
            [br.com.souenzzo.omniapp.http :as omniapp.http]
            [br.com.souenzzo.omniapp.parser :as omniapp.parser]
            [br.com.souenzzo.omniapp.ds :as omniapp.ds]
            [datascript.core :as ds]))


(defn ->app
  [{::keys []}]
  (let [conn (ds/create-conn omniapp/schema)]
    (ds/transact!
      conn
      [{::omniapp/component ::parser
        ::omniapp/kind      omniapp.ds/kind}
       {::omniapp/component ::entity-db
        ::omniapp/kind      omniapp.parser/kind}
       {::omniapp/component ::http-api
        ::omniapp/kind      omniapp.http/kind}
       {::omniapp/component ::http-backend
        ::omniapp/kind      omniapp.http/kind}])
    conn))
