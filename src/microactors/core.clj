(ns microactors.core
    (:use [clojure.core.match :only [match]])
    (:import [java.util.concurrent Executors Executor]
             [java.util.concurrent.atomic AtomicReference]))

(set! *warn-on-reflection* true)

;; ## Microactors in Clojure
;;
;; The actor model is a model of concurrent computation that defines many
;; small "actors". When invoked, each actor can:
;; 
;;  * send a finite number of messages to other actors
;;  * create a finite number of new actors
;;  * designate the behavior to be used for the next message it receives
;;
;; In the past this model of concurrent programming has been most sucsessfully
;; used by Erlang. However its model is only a subset of what the true actor
;; model describes. As such we are defining micro actors here. Micro actors a
;; in essence very small processess. 
;;
;; In a average operating system OS threads are approxementally 4MB in size,
;; Erlang processes are much smaller, around 350 bytes. Microactors aim 
;; to have an overhead no larger than 20 bytes.




;; Let's define a ThreadPool we'll use later in the dispatching of actors
(def ^Executor executor (Executors/newCachedThreadPool))


(defprotocol IMsgBox
    """Defines a message protocol for objects that can receive messages"""
    (post-msg [this msg]))

(defprotocol IMicroActor
    """Common functions defined by microactors"""
    (behavior [this] "Gets the current behavior")
    (top-message [this] "Gets the top most item in the message box")
    (set-behavior! [this beh] "Sets the current behavior"))

;;  This class hold all the data we need for a micro actor. Our actors
;;  consist purely of a queue and the curent behavior. We will then wrap these
;;  in an AtomicReference for thread safety. Assuming the pointer to queue, beh
;;  and the reference are all 4 bytes. We clock in at about 12 bytes as the size
;;  of a micro actor. We're also defining some helper functions to make this look
;;  like a map, without the memory overhead of defrecord.
(deftype MicroActor 

    [queue beh]

    clojure.lang.Associative
    (valAt 
        [this k]
         (.valAt this k nil))
    
    (valAt [this k d]
         (cond (= k :queue) queue
               (= k :beh) beh
               :default d))
    
    (containsKey [this k]
        (or (= k :queue) (= k beh)))
    
    (assoc [this k v]
        (cond (= k :queue) (MicroActor. v beh)
              (= k :beh) (MicroActor. queue v)))
    
    IMsgBox
    ([this msg] (assoc this :queue (conj queue msg))))

(defn micro-actor [queue beh]
    (MicroActor. queue beh))


(defn aref-swap! [^AtomicReference r f & args]
    """ Atomically updates the ref with the result of (apply f state args).
        f may be invoked several times. Returns the old value."""
    (loop []
          (let [oval (.get r)
                nval (apply f oval args)]
                (if (.compareAndSet r oval nval)
                    oval
                    (recur)))))

(defn- conj-swap! [r itm]
    (aref-swap! #(conj % itm)))

(defn- pop-swap! [r]
    (aref-swap! #(pop %)))


;; A type to hold our actor data
(deftype MicroActor [^AtomicReference queue ^{:volatile-mutable true} beh]
    IMsgBox
    (post-msg [this msg]
        (let [old (conj-swap! queue msg)]
             (when (= (count old) 0)
                   (.execute executor this))))
    
    IMicroActor
    (behavior [this] beh)
    (top-message [this] (first (.get queue)))
    (set-behavior! [this newbeh] (set! beh newbeh))
    
    java.lang.Runnable
    (run [this]
       (when-let [msg (peek (.get queue))]
         (doseq [r (beh msg)]
             (match [r] 
                 [[:become newbeh & args]]
                     (set-behavior! beh (apply newbeh args))
                 [[:post target & args]]
                     (post-msg target args)))
         (when (> (pop-swap! queue) 1)
               (.execute executor this))))) 



(defn aref [init]
    """Creates a new atomic reference. These are much smaller than atoms due to 
    the lack of validators, metadata, etc"""
    (ARef. (AtomicReference. init)))



;; Make AtomicReference pass IMsgQueue requests
(extend IMsgBox
    AtomicReference
    {:post-msg (fn post-msg [this msg]
                      (aref-swap! #(post-msg % msg)))})


(defn new-actor [beh]
    (aref (MicroActor. (clojure.lang.PersistentQueue/EMPTY) beh)))


(defn merge-become [actor beh args]
    (aref-swap! actor 
        (fn [old args]
            (assoc old :beh (apply beh args))) args))


(def run-microactor)

(defn exec-send [target args]
    (let [msg (peek (:queue (aref-get target)))]
         (aref-swap! target
                (fn [old] (assoc old :queue (conj (:queue old) args))))
         (when-not msg
             (.execute executor 
                       (fn [] (run-microactor target))))))

(defn run-microactor [actor]
    (when-let [msg (peek (:queue (aref-get actor)))]
         (doseq [r ((:beh (aref-get actor)) msg)]
             (match [r] 
                 [[:become beh & args]]
                     (merge-become actor beh args)
                 [[:post target & args]]
                     (exec-send target args)))
         (aref-swap! actor
               (fn [old]
                   (assoc old :queue (pop (:queue old)))))
         (when (peek (:queue (aref-get actor)))
             (.execute executor 
                       (fn [] (run-microactor actor))))))



(defmacro become [& args]
    `[:become ~@args])

(defmacro post [& args]
    `[:post ~@args])

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
    
    (defbeh counterbeh [cnt]
    ([:inc] (become counterbeh (inc cnt))
            (become counterbeh (inc cnt)))
    ([:print] (println cnt)))
    
    (def a (new-actor (counterbeh 0)))
    (exec-send a ["beh"])
    
    (time (dotimes [x 10000000] (exec-send a [:inc])))
    
        (time (do (dotimes [x 100] (exec-send a [:inc])) (exec-send a [:print])))
    (exec-send a [:print])
    
    (def v (vec (for [x (range 1000000)] (new-actor (counterbeh 0)))))
    
    )
              
                      



