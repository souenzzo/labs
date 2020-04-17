(ns br.com.souenzzo.todo-app.todo
  (:require [clojure.spec.alpha :as s]
            [next.jdbc :as j]
            [br.com.souenzzo.todo-app :as todo-app]
            [com.wsscode.pathom.connect :as pc]))

(s/def ::id number?)
(s/def ::note string?)

(defn tx-sql-schema
  []
  ["CREATE TABLE app_todo ( id SERIAL, note TEXT )"])


(defn register
  []
  [(pc/resolver
     `pull
     {::pc/input  #{::id}
      ::pc/output [::note]}
     (fn [{::todo-app/keys [entity-db]} {::keys [id]}]
       (let [{:app_todo/keys [note]} (first (j/execute! entity-db
                                                        ["SELECT note FROM app_todo WHERE id = ?"
                                                         id]))]
         {::note note})))
   (pc/mutation
     `create-todo
     {::pc/params [::note]}
     (fn [{::todo-app/keys [entity-db]} {::keys [note]}]
       (let [{:app_todo/keys [id]} (first (j/execute! entity-db
                                                      ["INSERT INTO app_todo (id, note)
                                                                               VALUES (DEFAULT, ?)
                                                                               RETURNING id"
                                                       note]))]
         {::id id})))])
