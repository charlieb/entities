(ns entities.core)

;;---------------
(defn make-entity [] (java.util.UUID/randomUUID))
(defn make-system [] {:entities {}})

(defn add-component [system entity component]
  (let [ents (:entities system)
        ent (get ents entity)]
    (assoc system :entities
           (assoc ents entity
                  (assoc ent (class component) component)))))

(defn entities-with [system component-type]
  (keys (filter #(contains? (second %) component-type) (:entities system))))

(defn add-system-function [system fn]
  (assoc system :functions fn))

;;---------------
