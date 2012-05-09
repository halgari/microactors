(ns examples.benchmarks.microactors
  (:use [microactors.core]))


(defbeh counterbeh [cnt cont]
    ([:inc] (become counterbeh (inc cnt) cont))
    ([:print] (post-msg cont cnt)))

(println "Creating 10mil agents")
(time (vec (doseq [x (range 10000000)]  (new-actor (counterbeh x nil)))))

(println "Sending 10mil messages to a single agent")
(time (let [f (promise)
            a (new-actor (counterbeh 0 f))]
           (dotimes [x 10000000]
                    (post-msg a :inc))
           (post-msg a :print)
            @f))


