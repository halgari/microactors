(ns examples.futures)

;;
;; ## Futures
;; 
;; Here is an implementation of futures as specified by Dale Schumacher
;; on his blog at www.dalnefre.com/2012/03/futures-and-capabilities
;;



(defbeh future-beh []
 ([cust :write value] (become value-beh value)
                      (send cust self))

 ([cust :read] (become wait-beh cust)))


(defbeh value-beh [value]
 ([cust :read] (send cust value)))


(defbeh wait-beh [waiting]
 ([cust :write value] (become value_beh value)
                      (send cust self)
                      (send (create broadcast_beh value) waiting))


 ([cust :read] (become wait-beh (cons cust waiting))))

(defbeh broadcast-beh [value]
 ([first & rest] (send first value)
                 (send self rest))
 ([last] (send last value)))



