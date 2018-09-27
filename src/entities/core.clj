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
(defrecord Circle [cx cy r th])

;;---------------

(defn circle-tick [system tdelta]
  (doseq [ent (entities-with system Circle)]
    (let [c (get-in system [:entities ent Circle])
          th (+ (:th c) tdelta)
          p (get-in system [:entities ent Position])
          x (+ (:cx c) (* (:r c) (Math/cos th)))
          y (+ (:cy c) (* (:r c) (Math/sin th)))
          ]
    (-> system
        (assoc-in [:entities ent Circle :th] th)
        (assoc-in [:entities ent Position :x] x)
        (assoc-in [:entities ent Position :y] y)))))
