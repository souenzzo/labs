(ns br.com.souenzzo.todo-app
  (:require [clojure.spec.alpha :as s]))

(s/def ::entity-db :next.jdbc.specs/db-spec)
