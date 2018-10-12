(ns entities.core-test
  (:require [clojure.test :refer :all]
            [entities.core :refer :all])
  )

(defn prnt1 [x] (println x) x)
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

(def velocity-test-ent (make-entity))
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

;;----------------

(def world-x 1)
(def world-y 4)
(def world-data
  (letfn [(mk-row [] (vec (repeatedly world-y #(ref nil))))]
    (vec (repeatedly world-x mk-row))))
(defrecord World [data])
(def world (->World world-data))
(defn reset-world []
  (dosync
   (doseq [row (:data world)] (doseq [cell row] (ref-set cell nil)))))
(defn init-world [system]
  (dosync 
   (doseq [ent (entities-with system Position)]
     (let [p (get-entity-component system ent Position)]
       (ref-set (get-in-entity-component system ent World [:data (:x p) (:y p)]) true))))
  system)

;;---------------

(defn exclusive-velocity [system]
  (reduce (fn [sys ent]
            (let [v (get-entity-component sys ent Velocity)
                  p (get-entity-component sys ent Position)
                  w (get-entity-component sys ent World)
                  new-p (->Position (+ (:vx v) (:x p)) (+ (:vy v) (:y p)))]
              (dosync ;; dosync here in case the tx retries, we need to recheck the destination
               (if @(get-in-entity-component sys ent World [:data  (:x new-p) (:y new-p)])
                 sys ;; move failed
                 (do ;; update the world in-place and return the new system
                   (ref-set (get-in w [:data (:x p) (:y p)]) nil)
                   (ref-set (get-in w [:data (:x new-p) (:y new-p)]) true)
                   (assoc-entity-component sys ent Position new-p))))))
          system
          (entities-with system Velocity)))

(defn exclusive-velocity-system [ent1 ent2]
  (reset-world)
  (-> (make-system)
      (add-component ent1 world)
      (add-component ent2 world)
      (add-component ent1 (->Position 0 0))
      (add-component ent2 (->Position 0 1))
      (add-component ent1 (->Velocity 0 1))
      (add-system-function exclusive-velocity 1)))

(deftest shared-state-component-setup
  "Testing the world component setup"
  (let [ent1 (make-entity)
        ent2 (make-entity)
        sys (exclusive-velocity-system ent1 ent2)]
    (init-world sys) ;; Now the ents have position we set them in the world
    (is (= (get-entity-component sys ent1 World) (get-entity-component sys ent2 World)) "World consistent across ents before tick")
    (is (and @(get-in-entity-component sys ent1 World [:data 0 0])
             @(get-in-entity-component sys ent1 World [:data 0 1])
             (nil? @(get-in-entity-component sys ent1 World [:data 0 2]))
             (nil? @(get-in-entity-component sys ent1 World [:data 0 3])))
        "Init world OK")))

(deftest shared-state-component-tick-no-change
  "Testing the world tick"
  (let [ent1 (make-entity)
        ent2 (make-entity)
        sys (-> (exclusive-velocity-system ent1 ent2)
                (init-world) ;; Now the ents have position we set them in the world
                (tick))]
    (is (= (get-entity-component sys ent1 Position) (->Position 0 0)) "Move Failed OK entity")
    (is (= (get-entity-component sys ent1 World) (get-entity-component sys ent2 World)) "World consistent across ents after tick")
    (is @(get-in-entity-component sys ent1 World [:data 0 0]) "Move failed OK world")))

(deftest shared-state-component-tick-change
  "Testing the world tick with changes"
  (let [ent1 (make-entity)
        ent2 (make-entity)
        sys (-> (exclusive-velocity-system ent1 ent2)
                (assoc-in-entity-component ent2 Position [:y] 3)
                (init-world) ;; Now the ents have position we set them in the world
                (tick))]
    (is (= (get-entity-component sys ent1 Position) (->Position 0 1)) "Move Entity OK")
    (is (= (get-entity-component sys ent1 World) (get-entity-component sys ent2 World)) "World consistent across ents after tick")
    (is (and (nil? @(get-in-entity-component sys ent1 World [:data 0 0]))
             @(get-in-entity-component sys ent1 World [:data 0 1])
             @(get-in-entity-component sys ent2 World [:data 0 3])) "Move OK world")))
    

;;---------------
