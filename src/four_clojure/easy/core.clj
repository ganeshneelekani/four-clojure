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
           (cons (take n coll) (-partition-by-all (drop n coll) n))))

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

;;Problem 45, Intro to Iterate
(= '(1 4 7 10 13) (take 5 (iterate #(+ 3 %) 1)))

;; Problem 47, Contain Yourself

(contains? #{4 5 6} 4)
(contains? [1 1 1 1 1] 1)
(contains? {4 :a 2 :b} 4)
(not (contains? [1 2 4] 5))

;; Problem 48, Intro to some
(= 6 (some #{2 7 6} [5 6 7 8]))
(=  6 (some #(when (even? %) %) [5 6 7 8]))

;; Problem 49, Split a sequence
(defn -split-at
  [n coll]
  [(take n coll) (drop n coll)])

(= (-split-at 3 [1 2 3 4 5 6]) [[1 2 3] [4 5 6]])
(= (-split-at 1 [:a :b :c :d]) [[:a] [:b :c :d]])
(= (-split-at 2 [[1 2] [3 4] [5 6]]) [[[1 2] [3 4]] [[5 6]]])

;; Problem 51, Advanced Destructuring
(= [1 2 [3 4 5] [1 2 3 4 5]] (let [[a b & c :as d] (list 1 2 3 4 5)] [a b c d]))

;; Problem 61, Map Construction
;; Special Restrictions : zipmap

(defn -map-construction [arg1 arg2]
  (loop [result {}
         a arg1
         b arg2]
    (if (or (empty? a) (empty? b))
      result
      (recur (assoc result (first a) (first b)) (rest a) (rest b)))))

(= (-map-construction [:a :b :c] [1 2 3]) {:a 1, :b 2, :c 3})
(= (-map-construction [1 2 3 4] ["one" "two" "three"]) {1 "one", 2 "two", 3 "three"})
(= (-map-construction [:foo :bar] ["foo" "bar" "baz"]) {:foo "foo", :bar "bar"})

;; Problem 62, Re-implement Iteration
;; Special Restrictions : iterate

(defn -iterate [f x]
  (cons
   x
   (lazy-seq
    (-iterate f (f x)))))

(= (take 5 (-iterate #(* 2 %) 1)) [1 2 4 8 16])
(= (take 100 (-iterate inc 0)) (take 100 (range)))
(= (take 9 (-iterate #(inc (mod % 3)) 1)) (take 9 (cycle [1 2 3])))

;; Problem 63, Group a Sequence
;; Special Restrictions : group-by

(defn -group-by[f coll]
  (reduce (fn [acc v]
            (let [k (f v)]
              (assoc acc k (conj (get acc k []) v))))
          {}
          coll))

(= (-group-by #(> % 5) #{1 3 6 8}) {false [1 3], true [6 8]})
(= (-group-by #(apply / %) [[1 2] [2 4] [4 6] [3 6]])
   {1/2 [[1 2] [2 4] [3 6]], 2/3 [[4 6]]})
(= (-group-by count [[1] [1 2] [3] [1 2 3] [2 3]])
   {1 [[1] [3]], 2 [[1 2] [2 3]], 3 [[1 2 3]]})

; Problem 66, Greatest Common Divisor

(defn gcd [a b]
  (if (zero? b)
    a
    (recur b (mod a b))))

(= (gcd 2 4) 2)
(= (gcd 10 5) 5)
(= (gcd 5 7) 1)
(= (gcd 1023 858) 33)

;; Problem 67, Prime Numbers
(defn -prime-number [x]
  (take x (reduce
           (fn [primes number]
             (if (some zero? (map (partial mod number) primes))
               primes
               (conj primes number)))
           [2]
           (take 1000 (iterate inc 3)))))

(= (-prime-number 2) [2 3])
(= (-prime-number 5) [2 3 5 7 11])
(= (last (-prime-number 100)) 541)

;; Problem 81, Set Intersection
;; Special Restrictions : intersection


(defn -intersection [s1 s2 ]
  (set (reduce (fn [acc x]
                 (if (contains? s2 x)
                   (conj acc x)
                   acc)) [] s1)))

(= (-intersection #{0 1 2 3} #{2 3 4 5}) #{2 3})
(= (-intersection #{0 1 2} #{3 4 5}) #{})
(= (-intersection #{:a :b :c :d} #{:c :e :a :f :d}) #{:a :c :d})

;; Problem 83, A Half-Truth

(defn -half-truth [& x]
  (and (contains? (set x) true)
       (not (every? identity x))))

(= false (-half-truth false false))
(= true (-half-truth true false))
(= false (-half-truth true))
(= true (-half-truth false true false))
(= false (-half-truth true true true))
(= true (-half-truth true true true false))

;; Problem 88, Symmetric Difference

(defn -intersection [s1 s2 ]
  (let [rds (fn [z]
              (fn [acc x]
                (if-not (contains? z x)
                  (conj acc x)
                  acc)))]
    (set (concat (reduce (rds s2) [] s1)
                 (reduce (rds s1) [] s2)))))

(defn -intersection-1 [s1 s2]
  (set (concat (reduce disj s1 s2) (reduce disj s2 s1))))

(= (-intersection #{1 2 3 4 5 6} #{1 3 5 7}) #{2 4 6 7})
(= (-intersection #{:a :b :c} #{}) #{:a :b :c})
(= (-intersection-1 #{} #{4 5 6}) #{4 5 6})
(= (-intersection-1 #{[1 2] [2 3]} #{[2 3] [3 4]}) #{[1 2] [3 4]})

;; Problem 90, Cartesian Product

(defn cartesian-product [s1 s2]
  (set (for [a s1 b s2]
         (vector a b))))

(= (cartesian-product #{"ace" "king" "queen"} #{"♠" "♥" "♦" "♣"})
   #{["ace"   "♠"] ["ace"   "♥"] ["ace"   "♦"] ["ace"   "♣"]
     ["king"  "♠"] ["king"  "♥"] ["king"  "♦"] ["king"  "♣"]
     ["queen" "♠"] ["queen" "♥"] ["queen" "♦"] ["queen" "♣"]})

(= (cartesian-product #{1 2 3} #{4 5})
   #{[2 4] [1 4] [3 4] [1 5] [2 5] [3 5]})

(= 300 (count (cartesian-product (into #{} (range 10))
                  (into #{} (range 30)))))

;; Problem 95, To Tree, or not to Tree
(defn tree? [coll]
  (or (nil? coll)
      (and (sequential? coll)
           (= 3 (count coll))
           (tree? (nth coll 1))           
           (tree? (nth coll 2)))))

(= (tree? '(:a (:b nil nil) nil))
   true)
(= (tree? '(:a (:b nil nil)))
   false)
(= (tree? [1 nil [2 [3 nil nil] [4 nil nil]]])
   true)
(= (tree? [1 [2 nil nil] [3 nil nil] [4 nil nil]])
   false)
(= (tree? [1 [2 [3 [4 nil nil] nil] nil] nil])
   true)
(= (tree? [1 [2 [3 [4 false nil] nil] nil] nil])
   false)
(= (tree? '(:a nil ()))
   false)

;; Problem 96, Beauty is Symmetry
(defn symmetric? [tree]
  (let [left (first tree)
        right (last tree)]
    (if (and left right)
      (and (symmetric? left) (symmetric? right) (== (first left) (first right)))
      (not (or left right)))))

; Problem 96, Beauty is Symmetry
(defn sym-tree? [[n x y]] 
  (letfn [(r [x] (if (sequential? x) (t x) x))
          (t [[n x y]] [n (r y) (r x)])]
    (= x (t y))))

(= (sym-tree? '(:a (:b nil nil) (:b nil nil))) true)
(= (sym-tree? '(:a (:b nil nil) nil)) false)
(= (sym-tree? '(:a (:b nil nil) (:c nil nil))) false)
(= (sym-tree? [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
        [2 [3 nil [4 [6 nil nil] [5 nil nil]]] nil]])
   true)
(= (sym-tree? [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
        [2 [3 nil [4 [5 nil nil] [6 nil nil]]] nil]])
   false)
(= (sym-tree? [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
        [2 [3 nil [4 [6 nil nil] nil]] nil]])
   false)

;Problem 97, Pascal's Triangle

(defn pascal-triangle [x]
  (cond
    (= x 1) [1]    
    (= x 2) [1 1]    
    :else (loop [r 2 
                 y [1 1]]            
            (if (= r x)
              y
              (recur (inc r) (flatten [1 (map + y (rest y)) 1]))))))

(= (pascal-triangle 1) [1])
(= (map pascal-triangle (range 1 6))
   [[1]
    [1 1]
    [1 2 1]
    [1 3 3 1]
    [1 4 6 4 1]])
(= (pascal-triangle 11)
   [1 10 45 120 210 252 210 120 45 10 1])
(= (pascal-triangle 11)
   [1 10 45 120 210 252 210 120 45 10 1])

;; Problem 99, Product Digits
(defn product-digit [x y]
  (mapv #(Integer/parseInt %) (-> (* x y)
                              str
                              (clojure.string/split #""))))

(= (product-digit 1 1) [1])
(= (product-digit 99 9) [8 9 1])
(= (product-digit 999 99) [9 8 9 0 1])

;; Problem 100, Least Common Multiple
(defn lcm [& x]   
  (let [y (apply min x)]     
    (loop [z y]       
       (if (every? #(zero? (mod z %)) x)        
         z         
         (recur (+ z y))))))

(== (lcm 2 3) 6)
(== (lcm 5 3 7) 105)
(== (lcm 1/3 2/5) 2)
(== (lcm 3/4 1/6) 3/2)
(== (lcm 7 5/7 2 3/5) 210)


;; Problem 107, Simple closures
(defn my-closure
  [n]
  (fn my-pow
    [x]
    (reduce * (repeat n x))))

(= 256 ((my-closure 2) 16), ((my-closure 8) 2))
(= [1 8 27 64] (map (my-closure 3) [1 2 3 4]))
(= [1 2 4 8 16] (map #((my-closure %) 2) [0 1 2 3 4]))