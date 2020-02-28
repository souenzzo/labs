(ns br.com.souenzzo.summon
  (:require [br.com.souenzzo.eql-as.alpha :as eql-as]
            [com.wsscode.pathom.core :as p]
            [ubergraph.alg :as uber.alg]
            [ubergraph.core :as uber]
            [clojure.spec.alpha :as s]))

(s/def ::driver qualified-keyword?)
(s/def ::drivers (s/map-of ::driver ::driver-impl))

(s/def ::driver-impl (s/keys :req [::input
                                   ::output
                                   ::start]))
(s/def ::elements (s/map-of ::id (s/keys :req [::driver])))
(s/def ::id qualified-keyword?)
(s/def ::input (s/coll-of qualified-keyword?))
(s/def ::output (s/coll-of qualified-keyword?))
(s/def ::start fn?)

(defn elements->digraph
  [root elements]
  (let [elements (for [[id el] elements]
                   (assoc el ::id id))
        index-id->element (into {} (map (juxt ::id identity))
                                elements)
        index-element->requires (reduce (fn [idx {::keys [id requires]}]
                                          (assoc idx
                                            id (set (vals requires))))
                                        {}
                                        elements)
        #_#_index-require->element (reduce (fn [idx {::keys [id requires]}]
                                             (into idx
                                                   (for [r (keys requires)]
                                                     [r id])))
                                           {}
                                           elements)
        #_#_index-element->provides (reduce (fn [idx {::keys [id provides]}]
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
    (concat [[root {:label (name root)}]]
            (for [{::keys [id driver requires]} elements
                  x (concat [[id {:label (str (str "id: " (pr-str id))
                                              (when driver
                                                (str "\ndriver: " (pr-str driver))))}]]
                            (for [require (get index-element->requires id)
                                  :let [from (get index-provide->element require root)
                                        as (get (eql-as/reverse requires) require)
                                        from-attr (get (::provides (get index-id->element from)) require)]]
                              [from id {:id    require
                                        :label (str (str "require: " (pr-str require))
                                                    (str "\nas: " (pr-str as))
                                                    (str "\nfrom: " (if (= from root)
                                                                      (name root)
                                                                      (pr-str from-attr))))}]))]
              x))))

(defn graph
  [{::keys [elements]} root]
  (apply uber/digraph (elements->digraph root elements)))

(defn required-globals
  [system]
  (let [g (graph system ::system)]
    (mapv #(uber/attr g % :id) (uber/find-edges g {:src ::system}))))

(defn explain-data
  [{::keys [drivers elements]
    :as    system}]
  (let [elements (for [[id el] elements]
                   (assoc el ::id id))]
    (concat (for [k (required-globals system)
                  :when (not (contains? system k))]
              {::issue ::missing-global
               ::key   k})
            (for [{::keys [driver id]} elements
                  :when (not (contains? drivers driver))]
              {::issue       ::missing-driver
               ::required-by id
               ::driver      driver}))))

(defn valid?
  [system]
  (empty? (explain-data system)))

(defn elements-for-start
  [{::keys [elements]
    :as    system}]
  (let [elements (for [[id el] elements]
                   (assoc el ::id id))
        g (graph system ::system)
        idx (into {}
                  (map (juxt ::id identity))
                  elements)]
    (for [id (uber.alg/topsort g)
          :when (contains? idx id)]
      (get idx id))))

(defn start-el
  [{::keys [drivers]
    :as    env}
   {::keys [input output driver requires provides]}]
  (let [input-selection (eql-as/ident-query {::eql-as/as-map requires
                                             ::eql-as/as-key :pathom/as})
        output-selection (eql-as/ident-query {::eql-as/as-map provides
                                              ::eql-as/as-key :pathom/as})
        env-for-start (if requires
                        (p/map-select env input-selection)
                        env)
        {::keys [start]} (get drivers driver)
        env-from-start (start (merge env env-for-start))]
    (merge env
           (if provides
             (p/map-select env-from-start output-selection)
             env-from-start))))

(defn start
  [system]
  (reduce start-el
          system
          (elements-for-start system)))

(defn elements-for-stop
  [{::keys [elements]}]
  (for [[id el] elements]
    (assoc el ::id id)))

(defn stop-el
  [{::keys [drivers]
    :as    env}
   {::keys [input output driver requires provides]}]
  (let [input-selection (eql-as/as-query {::eql-as/as-map provides
                                          ::eql-as/as-key :pathom/as})
        output-selection (eql-as/as-query {::eql-as/as-map requires
                                           ::eql-as/as-key :pathom/as})
        env-for-stop (if requires
                       (p/map-select env input-selection)
                       env)
        {::keys [stop]} (get drivers driver)
        env-from-stop (stop (merge env env-for-stop))]
    (apply dissoc
           (merge env
                  (if requires
                    (p/map-select env-from-stop output-selection)
                    env-from-stop))
           (keys provides))))

(defn stop
  [system]
  (reduce stop-el
          system
          (elements-for-stop system)))