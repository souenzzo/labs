(ns br.com.souenzzo.todo-app.main
  (:require [br.com.souenzzo.todo-app.todo :as todo]))

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
    (todo/register)))

(defn -main
  [& _]
  (prn :ok))
