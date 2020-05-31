(ns br.com.souenzzo.my-clj-admin
  (:require [datascript.core :as ds]
            [hiccup2.core :as h]
            [io.pedestal.http :as http]
            [io.pedestal.http.route :as route]
            [br.com.souenzzo.process :as proc]
            [ring.util.mime-type :as mime]
            [com.wsscode.pathom.core :as p]
            [com.wsscode.pathom.connect :as pc]
            [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clojure.java.shell :as sh]
            [cognitect.transit :as transit]
            [clojure.string :as string]
            [io.pedestal.http.body-params :as body-params]
            [clojure.pprint :as pprint])
  (:import (java.io File)
           (java.net URLEncoder URLDecoder)
           (java.nio.charset StandardCharsets)
           (java.util Date)))

(defn ui-action-button
  [action kvs]
  [:form
   {:action (str "/" action)
    :method "POST"}
   (for [[k v] kvs]
     (if (coll? v)
       (for [v v]
         [:input {:hidden true
                  :value  (if (ident? v)
                            (str (symbol v))
                            (str v))
                  :name   (str (symbol k))}])
       [:input {:hidden true
                :value  (str v)
                :name   (str (symbol k))}]))
   [:input {:type :submit :value (name action)}]])

(set! *warn-on-reflection* true)
(def schema {::dir         {:db/unique :db.unique/identity}
             ::process-dir {:db/valueType :db.type/ref}
             ::msg-process {:db/valueType :db.type/ref}
             ::insert-on   {:db/valueType :db.type/ref}
             ::uuid        {:db/unique :db.unique/identity}
             ::pid         {:db/unique :db.unique/identity}})
(defonce state (ds/create-conn schema))

(def register
  [(pc/mutation `create-repl-config
                {::pc/sym `create-repl-config}
                (fn [_ {::keys [dir description profiles]}]
                  (let [profiles (map keyword (remove nil? (if (coll? profiles) profiles [profiles])))
                        config-file (io/file dir ".repl-configs")
                        current-config (if (.exists config-file)
                                         (edn/read-string (slurp config-file))
                                         [])
                        new-config (conj current-config
                                         {:description description
                                          :profiles    (apply sorted-set profiles)})]
                    (spit config-file (with-out-str (pprint/pprint new-config)))
                    {})))
   (pc/mutation `start-repl
                {::pc/sym `start-repl}
                (fn [{:keys [parser] :as env} {::keys [dir profiles]}]
                  (let [profiles (map keyword (remove nil? (if (coll? profiles) profiles [profiles])))
                        eid [::dir dir]
                        uuid (ds/squuid)
                        {::keys [path]} (-> (parser env `[{~eid [::path]}])
                                            (get eid))
                        p-state (promise)
                        command ["/usr/bin/java" "-cp" path "clojure.main"]
                        p (proc/execute {::proc/command   command
                                         ::proc/on-stdout (fn [msg]
                                                            (ds/transact! @p-state
                                                                          [{::msg         msg
                                                                            ::inst        (new Date)
                                                                            ::msg-process [::uuid uuid]}]))
                                         ::proc/on-stdin  (fn []
                                                            (let [conn @p-state
                                                                  {:db/keys [id]
                                                                   ::keys   [inserted-line]} (->> (ds/q '[:find [(pull ?e [*]) ...]
                                                                                                          :in $ ?uuid
                                                                                                          :where
                                                                                                          [?x ::uuid ?uuid]
                                                                                                          [?e ::insert-on ?x]
                                                                                                          (not [?e ::ok? true])]
                                                                                                        (ds/db conn) uuid)
                                                                                                  (sort-by ::inst)
                                                                                                  first)]
                                                              (when inserted-line
                                                                (ds/transact! conn [{:db/id id ::ok? true}])
                                                                inserted-line)))
                                         ::proc/directory (io/file dir)})]
                    (ds/transact! state [{:db/id "dir"
                                          ::dir  dir}
                                         {::pid         (proc/pid p)
                                          ::process-dir "dir"
                                          ::command     command
                                          ::profiles    profiles
                                          ::path        path
                                          ::uuid        uuid
                                          ::process     p}])
                    (deliver p-state state)
                    {})))
   (pc/mutation `stop
                {::pc/sym `stop}
                (fn [{:keys [parser] :as env} {::keys [pid]}]
                  (let [process (-> (ds/entity (ds/db state) [::pid (edn/read-string pid)])
                                    ::process)]
                    (proc/destroy process)
                    {})))
   (pc/mutation `insert-on-repl
                {::pc/sym `insert-on-repl}
                (fn [{:keys [parser] :as env} {::keys [pid inserted-line]}]
                  (ds/transact! state [{::inserted-line (str inserted-line "\n")
                                        ::inst          (new Date)
                                        ::insert-on     [::pid (edn/read-string pid)]}])
                  {}))
   (pc/mutation `delete-profile
                {::pc/sym `delete-profile}
                (fn [_ {::keys [dir description]}]
                  (let [config-file (io/file dir ".repl-configs")
                        current-config (if (.exists config-file)
                                         (edn/read-string (slurp config-file))
                                         [])
                        new-config (remove
                                     (comp #{description} :description)
                                     current-config)]
                    (spit config-file (with-out-str (pprint/pprint (vec new-config))))
                    {})))

   (pc/resolver `repl-configs
                {::pc/input  #{::dir}
                 ::pc/output [::repl-configs]}
                (fn [_ {::keys [dir]}]
                  (let [config-file (io/file dir ".repl-configs")
                        current-config (if (.exists config-file)
                                         (edn/read-string (slurp config-file))
                                         [])]
                    {::repl-configs (for [{:keys [description profiles]} current-config]
                                      {::description description
                                       ::profiles    profiles})})))
   (pc/resolver `deps
                {::pc/input  #{::dir}
                 ::pc/output [::deps]}
                (fn [_ {::keys [dir]}]
                  {::deps (edn/read-string (slurp (io/file dir "deps.edn")))}))
   (pc/resolver `projects
                {::pc/output [::projects]}
                (fn [_ _]
                  {::projects (for [^File file (.listFiles (io/file "/home/souenzzo/src"))]
                                {::name (.getName file)
                                 ::href (str "/project/" (URLEncoder/encode (.getCanonicalPath file)
                                                                            StandardCharsets/UTF_8))
                                 ::dir  (.getCanonicalPath file)})}))
   (pc/resolver `classpath
                {::pc/input  #{::dir}
                 ::pc/output [::path]}
                (fn [_ {::pc/keys [dir profiles]}]
                  (let [profiles (string/join ":" (map name profiles))
                        profiles (when-not (string/blank? profiles)
                                   (str "-A:" profiles))
                        [cp & opts] (reverse (string/split-lines (:out (binding [sh/*sh-dir* dir]
                                                                         (if profiles
                                                                           (sh/sh "clojure" "-Sverbose" "-Spath" profiles)
                                                                           (sh/sh "clojure" "-Sverbose" "-Spath"))))))
                        opts (into {}
                                   (comp
                                     (remove string/blank?)
                                     (map #(string/split % #"=" 2))
                                     (map (partial mapv string/trim)))
                                   opts)]
                    {::cp-file (get opts "cp_file")
                     ::path    cp})))

   (pc/resolver `profiles
                {::pc/input  #{::deps}
                 ::pc/output [::profiles]}
                (fn [_ {::keys [deps]}]
                  {::profiles (-> deps :aliases keys)}))])

(def parser
  (p/parser {::p/plugins [(pc/connect-plugin {::pc/register register})]
             ::p/mutate  pc/mutate
             ::p/env     {::p/reader               [p/map-reader
                                                    pc/reader3
                                                    pc/open-ident-reader
                                                    p/env-placeholder-reader]
                          ::p/placeholder-prefixes #{">"}}}))

(defn projects
  [req]
  (let [{::keys [projects]} (parser req [{::projects [::name ::dir ::href]}])]
    [:div
     (for [pid (ds/q '[:find [?pid ...]
                       :where
                       [?e ::pid ?pid]]
                     (ds/db state))]
       [:a {:href (str "/repl/" pid)}
        (str "pid: " pid)])
     (for [{::keys [name dir href]} projects]
       [:li [:a {:href href}
             name]
        [:code dir]])]))

(defn project
  [{:keys [path-params] :as req}]
  (let [dir (-> path-params
                :dir
                str
                (URLDecoder/decode StandardCharsets/UTF_8))
        {::keys [repl-configs profiles]} (-> (parser req [{[::dir dir] [{::repl-configs [::description
                                                                                         ::profiles]}
                                                                        ::profiles]}])
                                             (get [::dir dir]))]
    [:div
     [:div
      "REPL's"
      (for [{::keys [description profiles] :as repl-config} repl-configs]
        [:li
         {:style {:padding "1rem"}}
         [:div description]
         [:div profiles]
         (ui-action-button `delete-profile
                           {::description description
                            ::dir         dir})
         (ui-action-button `start-repl
                           {::profiles profiles
                            ::dir      dir})])]
     [:form
      {:style  {:display        "flex"
                :flex-direction "column"}
       :method "POST"
       :action (str "/" `create-repl-config)}
      "Create a new REPL config"

      [:label
       "Dir"
       [:input {:name   (str `dir)
                :hidden true
                :value  dir}]]
      [:label
       "REPL Name"
       [:input {:name  (str `description)
                :value "my new repl"}]]
      [:fieldset
       [:legend "profiles"]
       (for [profile profiles]
         [:label
          [:input {:name  (str `profiles)
                   :type  "checkbox"
                   :value (name profile)}]
          (pr-str profile)])]
      [:input {:type "submit"}]]]))

(defn repl
  [{:keys [path-params]}]
  (let [pid (-> path-params :pid edn/read-string)]
    [:div
     (ui-action-button `stop {::pid pid})
     [:pre (with-out-str (clojure.pprint/pprint (ds/pull (ds/db state)
                                                         '[*]
                                                         [::pid pid])))]
     [:div
      {:style {:background-color "lightgray"}}
      (for [{::keys [msg]} (sort-by ::inst (::_msg-process (ds/entity (ds/db state) [::pid pid])))]
        [:pre msg])]
     [:form
      {:action (str "/" `insert-on-repl)
       :method "POST"}
      [:input {:hidden true :value pid
               :name   (str `pid)}]
      [:input {:name (str `inserted-line)}]
      [:input {:type :submit :value ">"}]]]))

(def routes
  `{"/"             projects
    "/project/:dir" project
    "/repl/:pid"    repl})

(def html-page
  {:name  ::html-page
   :leave (fn [{:keys [response] :as ctx}]
            (assoc ctx
              :response {:body    (str (h/html
                                         {:mode :html}
                                         (h/raw "<!DOCTYPE html>")
                                         [:html
                                          [:head
                                           #_[:link {:rel "stylesheet" :href "https://unpkg.com/mvp.css"}]
                                           [:meta {:charset "UTF-8"}]
                                           [:title "my-clj-admin"]]
                                          [:body
                                           {:onload "br.com.souenzzo.my_clj_admin.ui.main()"}
                                           [:div {:id "target"}]
                                           [:script {:src "/ui.js"}]
                                           #_#_[:a {:href "/"} "home"]
                                               response]]))
                         :headers {"Content-Type" (mime/default-mime-types "html")}
                         :status  200}))})

(def handle-mutation
  {:name  ::handle-mutation
   :enter (fn [ctx]
            (let [sym (-> ctx :route :route-name symbol)
                  params (-> ctx :request :form-params)
                  tx `[(~sym ~params)]]
              (parser (:request ctx) tx)
              (assoc ctx :response {:status  303
                                    :headers {"Location" (-> ctx :request :headers (get "referer" "/"))}})))})

(defonce http-state (atom nil))


(defn -main
  [& _]
  (swap! http-state (fn [st]
                      (when st
                        (http/stop st))
                      (-> {::http/join?          false
                           ::http/routes         (fn []
                                                   (route/expand-routes
                                                     (into #{["/api" :post (fn [{:keys [body] :as env}]
                                                                             (let [tx (some-> body
                                                                                              io/input-stream
                                                                                              (transit/reader :json)
                                                                                              transit/read)
                                                                                   result (parser env tx)]
                                                                               {:body   (fn [w]
                                                                                          (-> w
                                                                                              io/output-stream
                                                                                              (transit/writer :json)
                                                                                              (transit/write result)))
                                                                                :status 200}))
                                                              :route-name ::api]}

                                                           cat
                                                           [(for [[path fn-name] routes]
                                                              `[~path :get [html-page ~fn-name]])
                                                            (for [mutation-sym (keys (::pc/index-mutations (pc/register {} register)))]
                                                              `[~(str "/" mutation-sym)
                                                                :post
                                                                [~(body-params/body-params)
                                                                 handle-mutation]
                                                                :route-name
                                                                ~(keyword mutation-sym)])])))
                           ::http/type           :jetty
                           ::http/secure-headers {:content-security-policy-settings ""}
                           ::http/file-path      "target/public"
                           ::http/port           1111}
                          http/default-interceptors
                          http/dev-interceptors
                          http/create-server
                          http/start))))

