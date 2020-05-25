(ns br.com.souenzzo.my-clj-admin
  (:require [datascript.core :as ds]
            [hiccup2.core :as h]
            [io.pedestal.http :as http]
            [io.pedestal.http.route :as route]
            [ring.util.mime-type :as mime]
            [com.wsscode.pathom.core :as p]
            [com.wsscode.pathom.connect :as pc]
            [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clojure.java.shell :as sh]
            [clojure.string :as string]
            [io.pedestal.http.body-params :as body-params]
            [clojure.pprint :as pprint])
  (:import (java.io File)
           (java.net URLEncoder URLDecoder)
           (java.nio.charset StandardCharsets)))

(set! *warn-on-reflection* true)
(def schema {::dir {:db/unique :db.unique/identity}})
(def state (ds/create-conn schema))

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
   (pc/resolver `repl-configs
                {::pc/input  #{::dir}
                 ::pc/output [::repl-configs]}
                (fn [_ {::keys [dir]}]
                  (let [config-file (io/file dir ".repl-configs")
                        current-config (if (.exists config-file)
                                         (edn/read-string (slurp config-file))
                                         [])]
                    {::repl-configs current-config})))
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
   (pc/resolver `command
                {::pc/input  #{::path
                               ::java-cmd}
                 ::pc/output [::command]}
                (fn [_ {::keys [java-cmd path]}]
                  (prn [:ok java-cmd path])
                  {::command [java-cmd
                              "-classpath"
                              path
                              "clojure.main"]}))
   (pc/resolver `classpath
                {::pc/input  #{::dir}
                 ::pc/output [::path]}
                (fn [_ {::pc/keys [dir]}]
                  (let [[cp & opts] (reverse (string/split-lines (:out (binding [sh/*sh-dir* dir]
                                                                         (sh/sh "clojure" "-Sverbose" "-Spath")))))
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
  (p/parser {::p/plugins [(pc/connect-plugin {::pc/register register})
                          p/elide-special-outputs-plugin]
             ::p/mutate  pc/mutate
             ::p/env     {::p/reader               [p/map-reader
                                                    pc/reader3
                                                    pc/open-ident-reader
                                                    p/env-placeholder-reader]
                          ::p/placeholder-prefixes #{">"}}}))

(defn index
  [req]
  [:div
   [:code (pr-str (parser (assoc req ::p/entity {::java-cmd "/usr/bin/java"})
                          [{[::dir "/home/souenzzo/src/eql-datomic"]
                            [::path
                             ::command]}]))]])


(defn projects
  [req]
  (let [{::keys [projects]} (parser req [{::projects [::name ::dir ::href]}])]
    [:div
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
        {::keys [repl-configs profiles]} (-> (parser req [{[::dir dir] [{::repl-configs [::description ]}
                                                                        ::profiles]}])
                                             (get [::dir dir]))]
    [:div
     [:div
      "REPL's"
      (for [{::keys [] :as repl-config} repl-configs]
        [:li (pr-str repl-config)])]
     [:form
      {:style  {:display        "flex"
                :flex-direction "column"}
       :method "POST"
       :action (str "/" `create-repl-config)}
      "Create a new REPL config"

      [:label
       "Dir"
       [:input {:name  (str `dir)
                :value dir}]]
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
                   :value (pr-str profile)}]
          (pr-str profile)])]
      [:input {:type "submit"}]]]))

(def routes
  `{"/"             projects
    "/project/:dir" project})

(def html-page
  {:name  ::html-page
   :leave (fn [{:keys [response] :as ctx}]
            (assoc ctx
              :response {:body    (str (h/html
                                         {:mode :html}
                                         (h/raw "<!DOCTYPE html>")
                                         [:html
                                          [:head
                                           [:title "my-clj-admin"]]
                                          [:body
                                           [:a {:href "/"} "home"]
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
                      (-> {::http/join?  false
                           ::http/routes (fn []
                                           (route/expand-routes
                                             (into #{}
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
                           ::http/type   :jetty
                           ::http/port   1111}
                          http/default-interceptors
                          http/dev-interceptors
                          http/create-server
                          http/start))))

