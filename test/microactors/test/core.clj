(ns microactors.test.core
  (:use [microactors.core])
  (:use [clojure.test]))

(defbeh counterbeh [cont cur]
    ([:inc] (if (>= (inc cur) 3) 
             (post-msg cont cur)
             (become (counterbeh cont (inc cur))))))

(deftest replace-me 
  (let [prom (promise)
        actor (new-actor (counterbeh prom 0))]
       (post-msg actor [:inc])
       (post-msg actor [:inc])
       (post-msg actor [:inc])
       (is @prom 3)))       
  
