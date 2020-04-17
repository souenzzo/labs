(ns br.com.souenzzo.todo-app.main
  (:require [br.com.souenzzo.todo-app.todo :as todo]
            [br.com.souenzzo.todo-app.account :as account]
            [br.com.souenzzo.todo-app.session :as session]))


(def routes
  [{::route-name ::create-todo
    ::path       "/todo"
    ::method     :post
    ::mutation   `todo/create-todo
    ::req->param {::todo/note [:json-params :note]}
    ::returning  {:id   ::todo/id
                  :note ::todo/note}}])

(defn register
  []
  (concat
    (todo/register)
    (account/register)
    (session/register)))

(defn -main
  [& _]
  (prn :ok))
