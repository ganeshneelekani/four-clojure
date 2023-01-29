(ns four-clojure.hard.core)

;;Problem 89, Graph Tour
(defn vertices [edges]
  (-> edges
      flatten
      distinct))

(defn same-verices [edges]
  (let [vs     (vertices edges)
        result []
        same-vert (mapcat (fn [v]
                            (mapcat (fn [x]
                                      (if (= v x)
                                        (conj result [v x]))) vs))
                          vs)]

    (map (fn [e]
           (if (some #(= e %) edges)
             true
             false)) same-vert)))

(defn adjacent-vert [edges]
  (let [pair-edges (map (fn [[x y]]
                          [y x]) edges)]
    (map (fn [e]
           (if (some #(= e %) edges)
             true
             false)) pair-edges)))

(defn count-occurance [edges]
  (let [freq (merge-with + (frequencies (map second edges))
                         (frequencies (map first edges)))]
    (map (fn [x]
           (if (and (> (val x) 1) (odd? (val x)))
             true
             false)) freq)))

(defn -graph [edges]
  (cond
    (some true? (same-verices edges)) false
    (some true? (adjacent-vert edges)) false
    (some true? (count-occurance edges)) false
    :else true))

(= true (-graph [[:a :b]]))
(= false (-graph [[:a :a] [:b :b]]))
(= false (-graph [[:a :b] [:a :b] [:a :c] [:c :a]
                  [:a :d] [:b :d] [:c :d]]))
(= true (-graph [[1 2] [2 3] [3 4] [4 1]]))
(= true (-graph [[:a :b] [:a :c] [:c :b] [:a :e]
                 [:b :e] [:a :d] [:b :d] [:c :e]
                 [:d :e] [:c :f] [:d :f]]))
(= false (-graph [[1 2] [2 3] [2 4] [2 5]]))

;; Problem 73, Analyze a Tic-Tac-Toe Board

(defn analyze-tic-toc [board1]
  (let [board (vec (flatten board1))]
    (let [winning-position [[0 1 2] [3 4 5] [6 7 8]
                            [0 3 6] [1 4 7] [2 5 8]
                            [0 4 8] [2 4 6]]]
      (if winning-position
        (loop [position winning-position]
          (if-not (empty? position) 
            (let [[c1 c2 c3] (first position)] 
              (cond
                (= (str (get board c1) (get board c2) (get board c3)) ":x:x:x") :x
                (= (str (get board c1) (get board c2) (get board c3)) ":o:o:o") :o
                :else (recur (rest position))))
            nil))))))

(= nil (analyze-tic-toc [[:e :e :e]
            [:e :e :e]
            [:e :e :e]]))

(= :x (analyze-tic-toc [[:x :e :o]
           [:x :e :e]
           [:x :e :o]]))

(= :o (analyze-tic-toc [[:e :x :e]
           [:o :o :o]
           [:x :e :x]]))

(= nil (analyze-tic-toc [[:x :e :o]
            [:x :x :e]
            [:o :x :o]]))

(= :x (analyze-tic-toc [[:x :e :e]
           [:o :x :e]
           [:o :e :x]]))

(= :o (analyze-tic-toc [[:x :e :o]
           [:x :o :e]
           [:o :e :x]]))

(= nil (analyze-tic-toc [[:x :o :x]
            [:x :o :x]
            [:o :x :o]]))

;; Problem 92, Read Roman numerals
(def roman-to-int {"I" 1 "V" 5 "X" 10 "L" 50 "C" 100 "D" 500 "M" 1000})

 (defn -sum [input]
   (let [v (map #(get roman-to-int %) input)]
     (reduce (fn [f s]
               (if (>= f s)
                 (+  f s)
                 (- s f)))
             (first v)
             (next v))))

 (defn -partition-by
   [f coll]
   (lazy-seq (when-let [st (seq coll)]
               (let [s (map str st)
                     ft #(get roman-to-int %)
                     run (reduce (fn [acc x]
                                   (if (empty? acc)
                                     (conj acc x)
                                     (if (>= 0 (- (ft (last acc)) 
                                                  (ft x)))
                                       (conj acc x)
                                       (reduced acc))))
                                 []
                                 s)
                     result (-sum run)]
                 (cons result (-partition-by f (drop (count run) s))))))) 


(defn -roman-to-number [input]
  (reduce + (-partition-by identity input)))
 

(-partition-by identity "MMMCMXCIX")

(-partition-by identity "XLVIII")

(= 14 (-roman-to-number "XIV"))
(= 827 (-roman-to-number "DCCCXXVII"))
(= 3999 (-roman-to-number "MMMCMXCIX"))
(= 48 (-roman-to-number "XLVIII"))