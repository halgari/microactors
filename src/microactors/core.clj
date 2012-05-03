(ns microactors.core
    (:use [clojure.core.match :only [match]])
    (:import [java.util.concurrent Executors]))

(comment
    
(defn example-beh [value]
    (fn [&args]
        (match [sender :read] (vec (send sender value))
               [sender :write value] (vec (become example-beh value)
                                          (send sender value)))))


)

(def executor (Executors/newCachedThreadPool))


(defn mini-actor [queue beh]
    {:queue queue
     :beh beh})

(defn new-actor [beh]
    (atom (mini-actor (clojure.lang.PersistentQueue/EMPTY) beh)))


(defn merge-become [actor beh args]
    (mini-actor (:queue @actor) (apply beh args)))


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
             (match r 
                 [:become beh & args]
                     (merge-become actor beh args)
                    [:send target & args]
                     (exec-send target args)))
         (swap! actor
               (fn [old]
                   (assoc old :queue (pop (:queue old)))))
         (when (peek (:queue @actor))
             (.execute executor 
                       (fn [] (run-microactor actor))))))

              
                      



