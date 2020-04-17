(ns br.com.souenzzo.todo-app.account
  (:require [next.jdbc :as j]
            [br.com.souenzzo.todo-app :as todo-app]
            [com.wsscode.pathom.connect :as pc]))

(defn tx-sql-schema
  []
  ["CREATE TABLE app_account ( id SERIAL, ident TEXT )"])


(defn register
  []
  [(pc/resolver
     `pull
     {::pc/input  #{::id}
      ::pc/output [::ident]}
     (fn [{::todo-app/keys [entity-db]} {::keys [id]}]
       (let [{:app_account/keys [ident]} (first (j/execute! entity-db
                                                            ["SELECT ident FROM app_account WHERE id = ?"
                                                             id]))]
         {::ident ident})))
   (pc/mutation
     `create-account
     {::pc/params [::ident]}
     (fn [{::todo-app/keys [entity-db]} {::keys [ident]}]
       (let [{:app_account/keys [id]} (first (j/execute! entity-db
                                                         ["INSERT INTO app_account (id, ident)
                                                           VALUES (DEFAULT, ?)
                                                           RETURNING id"
                                                          ident]))]
         {::id id})))])
