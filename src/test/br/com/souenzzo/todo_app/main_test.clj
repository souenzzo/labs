(ns br.com.souenzzo.todo-app.main-test
  (:require [clojure.test :refer [deftest]]
            [io.pedestal.http :as http]
            [br.com.souenzzo.todo-app.todo :as todo]
            [br.com.souenzzo.todo-app.session :as session]
            [br.com.souenzzo.todo-app.account :as account]
            [next.jdbc :as j]
            [io.pedestal.test :refer [response-for]]
            [br.com.souenzzo.todo-app.main :as todo-app.main]
            [br.com.souenzzo.todo-app :as todo-app]
            [br.com.souenzzo.eql-as.alpha :as eql-as]
            [midje.sweet :refer [fact =>]]
            [clojure.data.json :as json]
            [com.wsscode.pathom.connect :as pc]
            [com.wsscode.pathom.core :as p]
            [edn-query-language.core :as eql]
            [io.pedestal.http.body-params :as body-params]))

(defn try-json->edn
  [x]
  (try
    (json/read-str x :key-fn keyword)
    (catch Exception e
      x)))

(deftest selector
  (fact
    "get-in"
    (todo-app/select-in {:b 1}
                        :b)
    => 1)
  (fact
    "simple rename"
    (todo-app/select-in {:b 1} {:a :b})
    => {:a 1})
  (fact
    "missing rename"
    (todo-app/select-in {} {:a :b})
    => {})
  (fact
    "join"
    (todo-app/select-in {:b {:d 1}}
                        {:a [:b {:c :d}]})
    => {:a {:c 1}})
  (fact
    "double join"
    (todo-app/select-in {:b {:d 1}}
                        {:a [:b :d]})
    => {:a 1}))

(deftest app
  ;; $ sudo docker run --name my-postgres --rm -p 5432:5432 postgres:alpine
  (let [db-spec {:dbtype "postgres"
                 :dbname "app"
                 :user   "postgres"}
        parser (p/parser {::p/plugins [(pc/connect-plugin {::pc/register (concat (todo-app.main/register)
                                                                                 pc/connect-resolvers)})]
                          ::p/mutate  pc/mutate
                          ::p/env     {::p/reader               [p/map-reader
                                                                 pc/reader2
                                                                 pc/open-ident-reader
                                                                 p/env-placeholder-reader]
                                       ::todo-app/entity-db     db-spec
                                       ::p/placeholder-prefixes #{">"}}})
        service-fn (-> {::http/routes (set (for [{::todo-app.main/keys [route-name path method returning req->param mutation]} todo-app.main/routes
                                                 :let [query (eql-as/ident-query {::eql-as/as-map returning
                                                                                  ::eql-as/as-key :pathom/as})
                                                       op {:dispatch-key mutation
                                                           :key          mutation
                                                           :type         :call
                                                           :query        query
                                                           :children     (:children (eql/query->ast query))}]]
                                             [path method [(body-params/body-params)
                                                           (fn [req]
                                                             (let [tx (eql/ast->query {:type     :root,
                                                                                       :children [(assoc op
                                                                                                    :params (todo-app/select-in req req->param))]})
                                                                   result (parser req tx)]
                                                               {:body   (json/write-str (get result mutation))
                                                                :status 200}))]
                                              :route-name route-name]))}
                       http/default-interceptors
                       http/dev-interceptors
                       http/create-servlet
                       ::http/service-fn)]
    (with-open [conn (j/get-connection (dissoc db-spec :dbname))]
      (j/execute! conn ["DROP DATABASE IF EXISTS app"])
      (j/execute! conn ["CREATE DATABASE app"]))
    (with-open [conn (j/get-connection db-spec)]
      (j/execute! conn (todo/tx-sql-schema))
      (j/execute! conn (account/tx-sql-schema))
      (j/execute! conn (session/tx-sql-schema)))
    (fact
      (-> (response-for service-fn :post "/todo"
                        :headers {"Content-Type" "application/json"}
                        :body (json/write-str {:note "ok"}))
          :body
          try-json->edn)
      => {:id   1
          :note "ok"})))
