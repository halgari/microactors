(ns microactors.core
    (:use [clojure.core.match :only [match]])
    (:import [java.util.concurrent Executors Executor]
             [java.util.concurrent.atomic AtomicReference]
             [clojure.lang PersistentQueue]))

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
    (post [this msg]))

(defprotocol IMicroActor
    """Common functions defined by microactors"""
    (behavior [this] "Gets the current behavior")
    (top-message [this] "Gets the top most item in the message box")
    (set-behavior! [this beh] "Sets the current behavior"))

(defn aref-swap! [^AtomicReference r f]
    """ Atomically updates the ref with the result of (f state).
        f may be invoked several times. Returns the old value."""
    (loop []
          (let [oval (.get r)
                nval (f oval)]
                (if (.compareAndSet r oval nval)
                    oval
                    (recur)))))

(defn- conj-swap! [r itm]
    (aref-swap! r #(let [old (if (= ::working %) PersistentQueue/EMPTY %)]
                             (conj old itm))))

(defn- pop-swap! [r]
    (aref-swap! r #(pop %)))

(defmacro dovec [[bind vec] & body]
    `(let [vec# ~vec]
        (dotimes [x# (count vec#)]
            (let [~bind (nth vec# x#)]
                 ~@body))))


;; A type to hold our actor data
(deftype MicroActor [^AtomicReference queue ^{:volatile-mutable true} beh]
    IMsgBox
    (post [this msg]
        (let [old (conj-swap! queue msg)]
             (when (and (not (= ::working old)) (= (count old) 0))
                   (.execute executor this))))
    
    IMicroActor
    (behavior [this] beh)
    (top-message [this] (first (.get queue)))
    (set-behavior! [this newbeh] (set! beh newbeh))
    
    java.lang.Runnable
    (run [this]
       (let [msgs (aref-swap! queue (fn [x] identity ::working))]
         (when (> (count msgs) 0)
               (doseq [msg msgs]
                 (beh this msg))
         (let [oval (.get queue)]
              (when-not (and (= oval ::working)
                             (.compareAndSet queue oval PersistentQueue/EMPTY))
                        (recur)))))))
                      
    ;;     (when (> (count (pop-swap! queue)) 1)
    ;;           (.execute executor this))))
    ;;)

(defn new-actor [beh]
    (MicroActor. (AtomicReference. (clojure.lang.PersistentQueue/EMPTY)) beh))

(defmacro create [[nm const] & body]
    `(let [~nm (new-actor ~const)]
          ~@body))

(defn debug [x]
    (println x)
    x)


(defmacro become [beh]
    `(set-behavior! ~'self ~beh))

(defmacro defbeh
    [bname args & patterns]
    (let [pats (reduce concat (map (fn [x] `(~(first x) (vector ~@(rest x)))) patterns))]
    `(defn ~bname ~args
        (fn [~'self msg#]
            (clojure.core.match/match msg# ~@pats)))))


(extend clojure.lang.IFn
    IMsgBox
    {:post (fn [this msg] (this msg))})


(defbeh gather-beh [ksel trigger cont reset mp]
    ([msg] (let [mp (assoc mp (ksel msg) msg)]
                (if (trigger mp)
                    (do (post cont mp)
                        (if reset
                            (become (gather-beh ksel trigger cont reset {}))
                            (become (gather-beh ksel trigger cont reset mp))))
                    (become (gather-beh ksel trigger cont reset mp))))))

(defn gather 
    "Creates an actor that gathers a set of results
    the ksel selects a key from the input messages. Every time a message
    is received the trigger fn is executed against it. When trigger returns
    true, the result is sent to cont. If reset is true, the contents of this
    actor will be reset every time trigger evalutates to true"
    [ksel trigger cont reset]
    (gather-beh ksel trigger cont reset {}))
(comment 
    
(defbeh scatter-beh [const-fn]
    ([coll] (create [newa (const-fn (first coll))]
                    (send newa (first coll)))
            (post self (next coll))))

(defn scatter
    "A form of pmap. Given a seq of items, creates a set of agents using the behaviors
    returned by apply const-fn to each item. Each actor is then sent the contents of the
    corrisponding seq. This is basically the opposite of gather"
    [const-fn coll]
    (let [newa (new-actor (scatter-beh const-fn))]
         (post! newa coll)))
)

(comment
    
    (defbeh counterbeh [cnt]
    ([:inc] (become counterbeh (inc cnt))
            (become counterbeh (inc cnt)))
    ([:print] (println cnt)))
    
    
    (def a (new-actor (counterbeh 0)))
    (post-msg a [:inc])
    (post-msg a [:print])
    (exec-send a ["beh"])
    
    (time (dotimes [x 10000000] (exec-send a [:inc])))
    
        (time (do (dotimes [x 100] (exec-send a [:inc])) (exec-send a [:print])))
    (post-msg a [:print])
    
    (def v (vec (for [x (range 1000000)] (new-actor (counterbeh 0)))))
    
    )
              
                      



