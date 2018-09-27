(ns camp.core)

;;---------------
(defn make-entity [] (java.util.UUID/randomUUID))
(defn make-system [] {:entities {}})

(defn add-component [system entity component]
  (let [ents (:entities system)
        ent (get entity ents)]
    (assoc system :entities
           (assoc ents entity
                  (assoc ent (class component) component)))))

(defn entities-with [system component-type]
  (keys (filter #(contains? (second %) component-type) (:entities system))))

(defn add-system-function [system fn]
  (assoc system :functions fn))

;;---------------

(defrecord Position [x y])
(defrecord Circle [x y r])

;;---------------

(defn circle [system]
  (let [ents (entities-with system Circle)]

;;---------------
