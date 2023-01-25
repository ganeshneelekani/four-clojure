(ns four-clojure.easy.core
  (:require [clojure.walk :as walk]
             [clojure.string :as str]))

; Problem 19, Last Element
(= (last [1 2 3 4 5]) 5)

(= (last '(5 4 3)) 3)
(= (last ["b" "c" "d"]) "d")

; Problem 20, Penultimate Element

(= (last (butlast (list 1 2 3 4 5))) 4)
(= (last (butlast ["a" "b" "c"])) "b")
(= (last (butlast [[1 2] [3 4]])) [1 2])

; Problem 21, Nth Element

(= (nth '(4 5 6 7) 2) 6)
(= (nth [:a :b :c] 0) :a)
(= (nth [1 2 3 4] 1) 2)
(= (nth '([1 2] [3 4] [5 6]) 2) [5 6])

; Problem 22, Count a Sequence
(= (count '(1 2 3 3 1)) 5)
(= (count "Hello World") 11)
(= (count [[1 2] [3 4] [5 6]]) 3)
(= (count '(13)) 1)
(= (count '(:a :b :c)) 3)

; Problem 23, Reverse a Sequence
(= (reverse [1 2 3 4 5]) [5 4 3 2 1])
(= (reverse (sorted-set 5 7 2 7)) '(7 5 2))
(= (reverse [[1 2] [3 4] [5 6]]) [[5 6] [3 4] [1 2]])

; Problem 24, Sum It All Up
(= (reduce + [1 2 3]) 6)
(= (reduce + (list 0 -2 5 5)) 8)
(= (reduce + #{4 2 1}) 7)
(= (reduce + '(0 0 -1)) -1)
(= (reduce + '(1 10 3)) 14)

; Problem 26, Fibonacci Sequence
(defn -fib 
  ([]
   (-fib 1 1))
  ([a b]
   (cons a (lazy-seq (-fib b (+ a b))))))

(defn get-fib [n]
  (take n (-fib)))

(= (get-fib 3) '(1 1 2))
(= (get-fib 6) '(1 1 2 3 5 8))
(= (get-fib 8) '(1 1 2 3 5 8 13 21))

; Problem 27, Palindrome Detector
(defn -polindrome [coll]
  (let [r (fn []
            (reduce #(conj %1 %2) `() coll))]
    (= coll (if (string? coll)
              (apply str (r))
              (r)))))

(false? (-polindrome '(1 2 3 4 5)))
(true? (-polindrome "racecar"))
(true? (-polindrome [:foo :bar :foo]))
(true? (-polindrome '(1 1 3 3 1 1)))
(false? (-polindrome '(:a :b :c)))

; Problem 28, Flatten a Sequence 
; Special Restrictions : flatten
(defn -pre-walk [coll]
  (filter (complement sequential?) 
          (rest (tree-seq sequential? seq coll))))

(= (-pre-walk '(1 2 3 [4 [5 6]])) '(1 2 3 4 5 6))
(= (-pre-walk ["a" ["b"] "c"]) '("a" "b" "c"))
(= (-pre-walk '((((:a))))) '(:a))

; Problem 29, Get the Caps
(defn capital-letter [coll]
  (apply str (reduce (fn [acc x]
                       (if (Character/isUpperCase (first x))
                         (conj acc x)
                         acc))
                     []
                     (str/split coll #""))))

(= (capital-letter "HeLlO, WoRlD!") "HLOWRD")
(empty? (capital-letter "nothing"))
(= (capital-letter "$#A(*&987Zf") "AZ")

; Problem 30, Compress a Sequence
(defn -partition-by
  [f coll]
  (lazy-seq (when-let [s (seq coll)]
              (let [fst (first s)
                    fv (f fst)
                    run (cons fst (take-while #(= fv %) (next s)))]
                (cons run (-partition-by f (drop (count run) s)))))))

(defn -occurance [coll]
   (let [f (fn [](map (fn [x]
                      (first x)) (-partition-by identity coll)))]
     (if (string? coll)
       (apply str (f))
       (f))))

(= (apply str (-occurance "Leeeeeerrroyyy")) "Leroy")
(= (-occurance [1 1 2 3 3 2 2 3]) '(1 2 3 2 3))
(= (-occurance [[1 2] [1 2] [3 4] [1 2]]) '([1 2] [3 4] [1 2]))

;; Problem 31, Pack a Sequence
(= (-partition-by identity [1 1 2 1 1 1 3 3]) '((1 1) (2) (1 1 1) (3 3)))
(= (-partition-by identity [:a :a :b :b :c]) '((:a :a) (:b :b) (:c)))
(= (-partition-by identity [[1 2] [1 2] [3 4]]) '(([1 2] [1 2]) ([3 4])))


;; Problem 32, Duplicate a Sequence
(defn duplicate [coll]
  (mapcat (fn [x]
         (list x x)) coll))

(= (duplicate [1 2 3]) '(1 1 2 2 3 3))
(= (duplicate [:a :a :b :b]) '(:a :a :a :a :b :b :b :b))
(= (duplicate [[1 2] [3 4]]) '([1 2] [1 2] [3 4] [3 4]))
(= (duplicate [44 33]) [44 44 33 33])

;; Problem 33, Replicate a Sequence
(defn replicate-seq [coll n]
  (mapcat (fn [x]
            (repeat n x)) coll))

(= (replicate-seq [1 2 3] 2) '(1 1 2 2 3 3))
(= (replicate-seq [:a :b] 4) '(:a :a :a :a :b :b :b :b))
(= (replicate-seq [4 5 6] 1) '(4 5 6))
(= (replicate-seq [[1 2] [3 4]] 2) '([1 2] [1 2] [3 4] [3 4]))
(= (replicate-seq [44 33] 2) [44 44 33 33])

;; Problem 34, Implement range
;; Special Restrictions : range
(defn -range [in ln]
  (reduce #(cons %2 %1) `()  
          (loop [result []
                 x      in]
            (if (= x ln)
              result
              (recur (cons x result) (inc x))))))

(= (-range 1 4) '(1 2 3))
(= (-range -2 2) '(-2 -1 0 1))
(= (-range 5 8) '(5 6 7))

;; Problem 38, Maximum value
;; Special Restrictions : max,max-key

(defn -max [& coll]
  (reduce #(if (> %1 %2) %1 %2) coll))

(= (-max 1 8 3 4) 8)
(= (-max 30 20) 30)
(= (-max 45 67 11) 67)

;; Problem 39, Interleave Two Seqs
;; Special Restrictions : interleave

(defn -interleave [coll1 coll2]
  (lazy-seq (loop [result []
                   c1 coll1
                   c2 coll2]
              (if (or (empty? c1) (empty? c2))
                result
                (recur (conj result (first c1) (first c2)) 
                       (rest c1) 
                       (rest c2))))))

(= (-interleave [1 2 3] [:a :b :c]) '(1 :a 2 :b 3 :c))
(= (-interleave [1 2] [3 4 5 6]) '(1 3 2 4))
(= (-interleave [1 2 3 4] [5]) [1 5])
(= (-interleave [30 20] [25 15]) [30 25 20 15])

;; Problem 40, Interpose a Seq
;; Special Restrictions : interpose

(defn -interpose [i c]
  (drop-last (loop [result []
                    coll c]
               (if (empty? coll)
                 result
                 (recur (conj result (first coll) i) (rest coll))))))

(= (-interpose 0 [1 2 3]) [1 0 2 0 3])
(= (apply str (-interpose ", " ["one" "two" "three"])) "one, two, three")
(= (-interpose :z [:a :b :c :d]) [:a :z :b :z :c :z :d])

;; Problem 41, Drop Every Nth Item
(defn -partition-by-all [coll n]
         (when-not (empty? coll)
           (conj (take n coll) (-partition-by-all (drop n coll) n))))

(defn -drop-nth-element [coll n]
  (->> (map #(take (dec n) %) (-partition-by-all coll n))
      (apply concat) 
       (into [])))

(= (-drop-nth-element [1 2 3 4 5 6 7 8] 3) [1 2 4 5 7 8])
(= (-drop-nth-element [:a :b :c :d :e :f] 2) [:a :c :e])
(= (-drop-nth-element [1 2 3 4 5 6] 4) [1 2 3 5 6])

; Problem 42, Factorial Fun

(defn -fact [number]
  (if (= number 1)
    number
    (* number (-fact (- number 1)))))

(= (-fact 1) 1)
(= (-fact 3) 6)
(= (-fact 5) 120)
(= (-fact 8) 40320)
