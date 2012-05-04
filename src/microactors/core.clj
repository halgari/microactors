(ns microactors.core
    (:use [clojure.core.match :only [match]])
    (:import [java.util.concurrent Executors Executor]))

(set! *warn-on-reflection* true)

(comment
    
(defn example-beh [value]
    (fn [&args]
        (match [sender :read] (vec (send sender value))
               [sender :write value] (vec (become example-beh value)
                                          (send sender value)))))


)

(def ^Executor executor (Executors/newCachedThreadPool))


(defn mini-actor [queue beh]
    {:queue queue
     :beh beh})

(defn new-actor [beh]
    (atom (mini-actor (clojure.lang.PersistentQueue/EMPTY) beh)))


(defn merge-become [actor beh args]
    (swap! actor 
        (fn [old args]
            (mini-actor (:queue old) (apply beh args))) args))


(def run-microactor)

(defn exec-send [target args]
    (let [msg (peek (:queue @target))]
         (swap! target
                (fn [old] (assoc old :queue (conj (:queue old) args))))
         (when-not msg
             (.execute executor 
                       (fn [] (run-microactor target))))))

(defn run-microactor [actor]
    (when-let [msg (peek (:queue @actor))]
         (doseq [r ((:beh @actor) msg)]
             (match [r] 
                 [[:become beh & args]]
                     (merge-become actor beh args)
                 [[:send target & args]]
                     (exec-send target args)))
         (swap! actor
               (fn [old]
                   (assoc old :queue (pop (:queue old)))))
         (when (peek (:queue @actor))
             (.execute executor 
                       (fn [] (run-microactor actor))))))


(defn counterbeh [cnt]
    (fn [msg]
        (match msg
            [:inc] [[:become counterbeh (inc cnt)]]
            [:print] (println cnt))))

(defbeh counterbeh [cnt]
    ([:inc] (become counterbeh (inc cnt))
            (become counterbeh (inc cnt)))
    ([:print] (println cnt)))

(defmacro become [& args]
    `[:become ~@args])

(defn debug [x]
    (println x)
    x)

(defmacro defbeh
    [bname args & patterns]
    (let [pats (reduce concat (map (fn [x] `(~(first x) (vector ~@(rest x)))) patterns))]
    (debug `(defn ~bname ~args
        (fn [msg#]
            (clojure.core.match/match msg# ~@pats))))))


(comment
    
    (def a (new-actor (counterbeh 0)))
    (exec-send a ["beh"])
    
    (time (dotimes [x 10000000] (exec-send a [:inc])))
    (exec-send a [:print])
    
    
    )
              
                      



