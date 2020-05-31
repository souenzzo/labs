(ns br.com.souenzzo.my-clj-admin.ui
  (:require #?@(:cljs    [[com.fulcrologic.fulcro.dom :as dom]
                          [goog.dom :as gdom]]
                :default [[com.fulcrologic.fulcro.dom-server :as dom]])
            [com.fulcrologic.fulcro.mutations :as m]
            [taoensso.timbre :as log]
            [com.fulcrologic.fulcro.data-fetch :as df]
            [com.fulcrologic.fulcro.routing.dynamic-routing :as dr]
            [com.fulcrologic.fulcro.application :as fa]
            [com.fulcrologic.fulcro.components :as comp :refer [defsc]]
            [clojure.edn :as edn]))



(defsc Settings [this props]
  {:ident               (fn [] [:component/id ::settings])
   :query               [:settings]
   :initial-state       {:settings "stuff"}
   :route-segment       ["settings"]
   :will-enter          (fn [app route-params]
                          (log/info "Will enter settings with route params " route-params)
                          (dr/route-immediate [:component/id ::settings]))
   :allow-route-change? (fn [this props]
                          #?(:cljs (js/console.log (comp/get-ident this) "props" props))
                          true)}
  (dom/div "Settings"))

(defsc Person [this {:ui/keys      [modified?]
                     :person/keys  [id name]
                     :address/keys [city state]
                     :as           props}]
  {:query               [:ui/modified? :person/id :person/name :address/city :address/state]
   :ident               :person/id
   :route-segment       ["person" :person/id]
   :route-cancelled     (fn [{:person/keys [id]}]
                          (log/info "Routing cancelled to user " id))
   :allow-route-change? (fn [this {:ui/keys [modified?]}]
                          (when modified?
                            #?(:cljs (js/alert "You cannot navigate until the user is not modified!")))
                          (not modified?))
   :will-enter          (fn [app {:person/keys [id] :as route-params}]
                          (log/info "Will enter user with route params " route-params)
                          ;; be sure to convert strings to int for this case
                          (let [id (if (string? id) (edn/read-string id) id)]
                            (dr/route-deferred [:person/id id]
                                               #(df/load app [:person/id id] Person
                                                         {:post-mutation        `dr/target-ready
                                                          :post-mutation-params {:target [:person/id id]}}))))}
  (dom/div
    (dom/h3 (str "Person " id))
    (dom/div (str name " from " city ", " state))
    (dom/div
      (dom/input {:type     "checkbox"
                  :onChange (fn []
                              (m/toggle! this :ui/modified?))
                  :checked  (boolean modified?)})
      "Modified (prevent routing)")))

(def ui-person (comp/factory Person {:keyfn :person/id}))

(defsc Main [this props]
  {:ident               (fn [] [:component/id ::main])
   :query               [:main]
   :initial-state       {:main "stuff"}
   :route-segment       ["main"]
   :will-enter          (fn [app route-params]
                          (log/info "Will enter main" route-params)
                          (dr/route-immediate [:component/id ::main]))
   :allow-route-change? (fn [this props]
                          (log/info (comp/get-ident this) "props" props)
                          true)}
  (dom/div "Main"))

(dr/defrouter TopRouter [this {:keys [current-state pending-path-segment]}]
  {:router-targets [Main Settings Person]}
  (case current-state
    :pending (dom/div "Loading...")
    :failed (dom/div "Loading seems to have failed. Try another route.")
    (dom/div "Unknown route")))

(def ui-top-router (comp/factory TopRouter))

(defsc Root [this {:root/keys [router]}]
  {:query         [{:root/router (comp/get-query TopRouter)}]
   :initial-state {:root/router {}}}
  (dom/div
    (dom/button {:onClick #(dr/change-route this ["main"])} "Go to main")
    (dom/button {:onClick #(dr/change-route this ["settings"])} "Go to settings")
    (dom/button {:onClick #(dr/change-route this ["person" "1"])} "Go to person 1")
    (dom/button {:onClick #(dr/change-route this ["person" "2"])} "Go to person 2")
    (ui-top-router router)))
(defonce state (atom nil))

(defn ^:export main
  []
  (let [node #?(:cljs (gdom/getElement "target")
                :default nil)
        app (fa/fulcro-app {:client-did-mount (fn [app]
                                                (dr/change-route app ["main"]))})]
    (fa/mount! app Root node)))


(defn after-load
  []
  (fa/force-root-render! @state))
