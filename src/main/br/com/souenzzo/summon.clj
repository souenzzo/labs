(ns br.com.souenzzo.summon
  (:require [br.com.souenzzo.eql-as.alpha :as eql-as]
            [com.wsscode.pathom.core :as p]))

(defn elements->digraph
  [elements]
  (let [index-id->element (into {} (map (juxt ::id identity))
                                elements)
        index-element->requires (reduce (fn [idx {::keys [id requires]}]
                                          (assoc idx
                                            id (set (vals requires))))
                                        {}
                                        elements)
        index-require->element (reduce (fn [idx {::keys [id requires]}]
                                         (into idx
                                               (for [r (keys requires)]
                                                 [r id])))
                                       {}
                                       elements)
        index-element->provides (reduce (fn [idx {::keys [id provides]}]
                                          (assoc idx
                                            id (set (keys provides))))
                                        {}
                                        elements)
        index-provide->element (reduce (fn [idx {::keys [id provides]}]
                                         (into idx
                                               (for [p (keys provides)]
                                                 [p id])))
                                       {}
                                       elements)]
    (concat [[::env {:label "env"}]]
            (for [{::keys [id driver requires]} elements
                  x (concat [[id {:label (str (str "id: " (pr-str id))
                                              (when driver
                                                (str "\ndriver: " (pr-str driver))))}]]
                            (for [require (get index-element->requires id)
                                  :let [from (get index-provide->element require ::env)
                                        as (get (eql-as/reverse requires) require)
                                        from-attr (get (::provides (get index-id->element from)) require)]]
                              [from id {:label (str (str "require: " (pr-str require))
                                                    (str "\nas: " (pr-str as))
                                                    (str "\nfrom: " (pr-str from-attr)))}]))]
              x))))

(defn start-el
  [env {::keys [input output start id requires provides]}]
  (let [input-selection (eql-as/ident-query {::eql-as/as-map requires
                                             ::eql-as/as-key :pathom/as})
        output-selection (eql-as/ident-query {::eql-as/as-map provides
                                              ::eql-as/as-key :pathom/as})
        env-for-start (if requires
                        (p/map-select env input-selection)
                        env)
        env-from-start (start (merge env env-for-start))]
    (merge env
           (if provides
             (p/map-select env-from-start output-selection)
             env-from-start))))

(defn start
  [env elements]
  (reduce start-el
          env
          ;; TODO: Sort elements
          elements))

