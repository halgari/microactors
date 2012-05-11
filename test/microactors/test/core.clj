(ns microactors.test.core
  (:use [microactors.core])
  (:use [clojure.test]))

(defbeh counterbeh [cont cur]
    ([:inc] (if (>= (inc cur) 3) 
             (post cont (inc cur))
             (become (counterbeh cont (inc cur))))))

(defbeh identitybeh [cont]
    ([msg] (post cont msg)))

(def prom (promise))
(def actor (new-actor (counterbeh prom 0)))
(post actor :inc)
(post actor :inc)
(post actor :inc)
@prom

(deftest basic-tests 
  (let [prom (promise)
        actor (new-actor (counterbeh prom 0))]
       (post actor :inc)
       (post actor :inc)
       (post actor :inc)
       (is (= @prom [3]))
       ))       
  
(comment
(deftest gather-tests-with-reset-true
  (let [prom (promise)
        gth (new-actor (gather :idx #(= (count %) 3) prom true))]
       (post gth [{:idx 1}])
       (post gth [{:idx 2}])
       (post gth [{:idx 3}])
       (is @prom {1 {:idx 1}
                  2 {:idx 2}
                  3 {:idx 3}})))

(deftest gather-tests-with-reset-false
  (let [prom (promise)
        gth (new-actor (gather :idx #(= (count %) 3) prom false))]
       (post gth [{:idx 1}])
       (post gth [{:idx 2}])
       (post gth [{:idx 3}])
       (is @prom {1 {:idx 1}
                  2 {:idx 2}
                  3 {:idx 3}})))

(deftest scatter-tests
    (let [coll (range 10)
          prom (promise)
          gth (new-actor (gather :idx #(debug (= (count %) (count coll))) prom false))
          b (beh tmp [itm]
              ([msg] (post gth {:idx itm})))]
         (scatter b coll)
         ;(is (vals @prom) coll)
    ))
          
       )
