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

(defn tick [system]
  (reduce (fn [sys func] (func sys))
          system
          (map :func (filter #(zero? (mod (:ticks system) (:tick-mod %)))
                             (:functions system)))))

(defn run []
  )

;;---------------
