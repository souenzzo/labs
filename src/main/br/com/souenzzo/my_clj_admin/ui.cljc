(ns br.com.souenzzo.my-clj-admin.ui
  (:require #?@(:cljs    [[com.fulcrologic.fulcro.dom :as dom]
                          [goog.dom :as gdom]]
                :default [[com.fulcrologic.fulcro.dom-server :as dom]])
            [com.fulcrologic.fulcro.mutations :as fm]
            [com.fulcrologic.fulcro.application :as fa]
            [com.fulcrologic.fulcro.components :as comp]))

(fm/defmutation bump-number
  [_]
  (action [{:keys [state]}]
          (swap! state update :ui/number inc)))

(comp/defsc Root [this {:ui/keys [number]}]
  {:query         [:ui/number]
   :initial-state {:ui/number 0}}
  (dom/div
    (dom/h4 "This is an example.")
    (dom/button {:onClick #(comp/transact! this `[(bump-number {})])}
                "You've clicked this button " number " times.")))

(defonce state (atom nil))

(defn ^:export main
  []
  (let [node #?(:cljs (gdom/getElement "target")
                :default nil)
        app (fa/fulcro-app {})]
    (fa/mount! app Root node)))

(defn after-load
  []
  (fa/force-root-render! @state))
