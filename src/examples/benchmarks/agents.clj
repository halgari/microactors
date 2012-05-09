(ns examples.benchmarks.agents)



(defn -main[]
        
    (println "Creating 10mil agents")
    (time (vec (doseq [x (range 10000000)] (agent x))))
    
    (println "Sending 10mil messages to a single agent")
    (time (let [a (agent 0)]
               (dotimes [x 10000000]
                        (send a inc))
                        (await a)))
)
