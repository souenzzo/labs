(ns br.com.souenzzo.todo-app.session
  (:require [next.jdbc :as j]
            [br.com.souenzzo.todo-app :as todo-app]
            [com.wsscode.pathom.connect :as pc]))

(defn tx-sql-schema
  []
  ["CREATE TABLE app_session ( id SERIAL, cookie TEXT )"])


(defn register
  []
  [(pc/resolver
     `pull
     {::pc/input  #{::id}
      ::pc/output [::cookie]}
     (fn [{::todo-app/keys [entity-db]} {::keys [id]}]
       (let [{:app_session/keys [cookie]} (first (j/execute! entity-db
                                                             ["SELECT cookie FROM app_session WHERE id = ?"
                                                              id]))]
         {::cookie cookie})))
   (pc/mutation
     `create-session
     {::pc/params [::cookie]}
     (fn [{::todo-app/keys [entity-db]} {::keys [cookie]}]
       (let [{:app_session/keys [id]} (first (j/execute! entity-db
                                                         ["INSERT INTO app_session (id, cookie)
                                                           VALUES (DEFAULT, ?)
                                                           RETURNING id"
                                                          cookie]))]
         {::id id})))])
