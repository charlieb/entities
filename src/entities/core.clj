(ns entities.core)

;;---------------
(defn make-entity [] (java.util.UUID/randomUUID))
(defn make-system [] {:entities {} :ticks 0})

(defn add-component [system entity component]
  (let [ents (:entities system)
        ent (get ents entity)]
    (assoc system :entities
           (assoc ents entity
                  (assoc ent (class component) component)))))

(defn entities-with [system component-type]
  (keys (filter #(contains? (second %) component-type) (:entities system))))

(defn add-system-function [system fn tick-mod]
  (assoc system :functions
         (conj (:functions system) {:func fn :tick-mod tick-mod})))

(defn get-entity-components [system ent]
  (get-in system [:entities ent]))

(defn get-entity-component [system ent comp]
  (get-in system [:entities ent comp]))

(defn get-in-entity-component [system ent comp path]
  (get-in system (concat [:entities ent comp] path)))

(defn assoc-entity-component [system ent comp val]
  (assoc-in system [:entities ent comp] val))

(defn assoc-in-entity-component [system ent comp path val]
  (assoc-in system (concat [:entities ent comp] path) val))

(defn tick [system]
  (reduce (fn [sys func] (func sys))
          system
          (map :func (filter #(zero? (mod (:ticks system) (:tick-mod %)))
                             (:functions system)))))

(defn run []
  )

;;---------------
