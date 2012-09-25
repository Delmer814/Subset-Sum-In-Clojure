(ns subsetSum.core)

(defn index [i j]
  (mod (+ 1 (quot i (Math/pow 2 j))) 2))
                                                                        
(defn combF [i x]
  (map * (map #(index i %)  (range 0 (count x))) x))

(defn powerSet* [x]
  (let [n (count x)
        k (- (Math/pow 2 n) 1)]
    (loop [i 0 j k sol []]
      (if (< i k)
        (recur (+ i 1) n (conj sol (combF i x)))
        sol))))

(defn powerSet [x]
  (map #(reduce + %) (powerSet* x)))


(defn subsetSum* [i x]
  (filter #(= i (int %)) (powerSet x)))

(defn subsetSum [i x]
  (not (empty? (subsetSum* i x))))
                                                                        