(ns camp.core-test
  (:require [clojure.test :refer :all]
            [camp.core :refer :all])
  (:import [camp.core Position]))

(deftest add-retrieve-component
  "Creates and retrieves a component"
  (let [system (make-system)
        ent (make-entity)]
    (is (= ent (first
                (-> system
                   (add-component ent (->Position 0 0))
                   (entities-with Position)))))))
