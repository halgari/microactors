(ns microactors.core)

(comment
    
(defn example-beh [value]
    (fn [&args]
        (match [sender :read] (vec (send sender value))
               [sender :write value] (vec (become example-beh value)
                                          (send sender value)))))
=> (de

)


(deftype mini-actor [queue beh pid]
    (queue [msg] (mini-actor. (cons msg queue) beh pid))
    (msg [] (peek queue)))

(defn run-actor-ctor [actor]
    (fn [] 
       (doseq [result (actor)]
          (cond (instance? result SendCommand)
