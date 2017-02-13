(ns markov.markov)

(defmacro !
  "Shorthand for an anonymous function with no parameters."
  [& body]
  `(fn [] ~body))

(defn valid-chain?
  "Returns true if all transition probablities equal one."
  [transitions]
  (every? identity
    (map (comp (partial = 1)
               (partial reduce +)
               vals)
         (vals transitions))))

(defn iter
  "Returns the value of one iteration of the given chain, starting from 'state'."
  [state transitions]
  (let [r (rand)
        trans (transitions state)
        ks (vec (keys trans))
        vs (vals trans)]
    (ks (count (filter false?
                       (map #(> % r)
                            (reductions + vs)))))))

(defn eval-chain
  ""
  [state transitions n]
  (if (= n 0)
      state
      (recur (iter state transitions) transitions (dec n))))

(defn iter!
  "Performs one iteration of the given chain, starting from 'state'."
  [state transitions]
  (swap! state
         assoc
         :state
         (iter (@state :state)
               transitions)))

(defn start-chain!
  ""
  ([state transitions n]
    (repeatedly n (! :state
                     (iter! state transitions))))

  ([state transitions ms n]
    (repeatedly n (! do (Thread/sleep ms)
                        (println (:state (iter! state transitions)))))))

(defn chain-until
  ""
  [state transitions stop]
  (take-while #(= stop %)
              (lazy-seq (repeatedly (! :state
                                       (iter! state transitions))))))

(defn rand-ratio
  "Returns a random rational number from 0/x to x/x."
  [x]
  (/ (rand-int x)
     x))

(defn rand-rationals
  "Returns n random rational numbers from 0/1000 to 1000/1000 which add up to 1."
  [n]
  (let [ints (repeatedly n (! rand-int 1000))
        total (reduce + ints)]
    (map #(/ % total) ints)))

(defn rand-chain
  ""
  [nodes]
  (let [n (count nodes)]
    (zipmap nodes
            (repeatedly n (! zipmap
                             nodes (rand-rationals n))))))

(defn pairs
  "Takes a list and returns a list of pairs of adjacent elements."
  [data]
  (map vector
       data (rest data)))

(defn counts
  "Takes a list of pairs and returns a list of maps from pairs to frequencies of the pair."
  [pairs]
  (map #(apply hash-map %)
       (into '()
             (frequencies pairs))))

(defn int-transitions
  ""
  [counts]
  (apply (partial merge-with
                  merge)
         (map #(assoc-in
                {}
                (first (keys %))
                (first (vals %)))
              counts)))

(defn transitions
  ""
  [int-transitions]
  (zipmap (map first
               int-transitions)
          (map #(reduce-kv
                 (fn [m k v]
                     (assoc m k (/ v (reduce + (map second (second %))))))
                 {}
                 (second %))
               int-transitions)))

(defn make-chain
  "Takes a list and returns a hash of transitions"
  [data]
  (-> data
      pairs
      counts
      int-transitions
      transitions))

;; transformations:
;; ("z" "a" "c" "a" "c" "\n") : data
;; (["z" "a"] ["a" "c"] ["c" "a"] ["a" "c"] ["c" "\n"]) : pairs
;; {["z" "a"] 1 ["a" "c"] 2 ["c" "a"] 1 ["c" "\n"] 1} : frequencies?
;; ({["z" "a"] 1} {["a" "c"] 2} {["c" "a"] 1} {["c" "\n"] 1}) : counts
;; {"z" {"a" 1} "a" {"c" 2} "c" {"a" 1 "\n" 1}} : integer transitions
;; {"z" {"a" 1/1} "a" {"c" 2/2} "c" {"a" 1/2 "\n" 1/2}} : transitions
