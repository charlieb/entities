(ns entities.core-test
  (:require [clojure.test :refer :all]
            [entities.core :refer :all])
  )

;;---------------

(defrecord Position [x y])
(defrecord Velocity [vx vy])

(defn velocity-tick [system]
  (reduce (fn [sys ent]
            (let [v (get-in sys [:entities ent Velocity])
                  p (get-in sys [:entities ent Position])]
              (-> sys
                  (assoc-in [:entities ent Position :x] (+ (:vx v) (:x p)))
                  (assoc-in [:entities ent Position :y] (+ (:vy v) (:y p))))))
          system (entities-with system Velocity)))

(def velocity-test-ent make-entity)
(def velocity-test-system
  (-> (make-system)
      (add-component velocity-test-ent (->Position 0 0))
      (add-component velocity-test-ent (->Velocity 5 10))
      (add-system-function velocity-tick 1)))

(deftest apply-system-function
  "Velocity time tick"
  (is (= (->Position 5 10)
         (-> velocity-test-system
             (tick)
             (get-in [:entities velocity-test-ent Position])
             ))))

;;---------------

(deftest add-retrieve-component
  "Creates and retrieves a component"
  (let [system (make-system)
        ent (make-entity)]
    (is (= ent (first
                (-> system
                   (add-component ent (->Position 0 0))
                   (entities-with Position)))))))

(deftest add-retrieve-components
  "Creates and retrieves >1 component"
  (let [ent (make-entity)
        system (-> (make-system)
                   (add-component ent (->Position 0 0))
                   (add-component ent (->Velocity 0 0))
                   )
        ]
    (is (= ent
           (first (entities-with system Position))
           (first (entities-with system Velocity))))))
