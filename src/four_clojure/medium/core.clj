(ns four-clojure.medium.core
  (:require [clojure.walk :as walk]))

;; Problem 43, Reverse Interleave
(defn -partition-by-all [coll n]
  (when-not (empty? coll)
    (cons (take n coll) (-partition-by-all (drop n coll) n))))

(defn -reverse-interleave [coll n]
  (lazy-seq (let [pa (-partition-by-all coll n)]
              (loop [result []
                     c pa]
                (if (every? empty? c)
                  result
                  (recur (conj result (map #(first %) c)) (map #(rest %) c)))))))

(= (-reverse-interleave [1 2 3 4 5 6] 2) '((1 3 5) (2 4 6)))
(= (-reverse-interleave (range 9) 3) '((0 3 6) (1 4 7) (2 5 8)))
(= (-reverse-interleave (range 10) 5) '((0 5) (1 6) (2 7) (3 8) (4 9)))

;; Problem 44, Rotate Sequence

(defn rotate-right [n coll f ]
  (if (= n 0)
    (seq coll)
    (rotate-right (f n) (conj (into [] (rest coll)) (first coll)) f)))

(defn rotate-left [n coll f]
  (if (= n 1)
    (seq coll)
    (rotate-left (f n) (conj (into [] (rest coll)) (first coll)) f)))

(defn rotate-seq [n coll]
  (cond 
    (pos? n) (rotate-right n coll dec)
    :else (rotate-left n coll inc)))

(= (rotate-seq 2 [1 2 3 4 5]) '(3 4 5 1 2))
(= (rotate-seq -2 [1 2 3 4 5]) '(4 5 1 2 3))
(= (rotate-seq 6 [1 2 3 4 5]) '(2 3 4 5 1))
(= (rotate-seq 1 '(:a :b :c)) '(:b :c :a))
(= (rotate-seq -4 '(:a :b :c)) '(:c :a :b))

;; Problem 46, Flipping out
(defn flipping-out
  [f]
  #(f %2 %1))

(= 3 ((flipping-out nth) 2 [1 2 3 4 5]))
(= true ((flipping-out >) 7 8))
(= 4 ((flipping-out quot) 2 8))
(= [1 2 3] ((flipping-out take) [1 2 3 4 5] 3))


;; Problem 50, Split by Type
(defn split-by-type [coll]
  (vals (group-by type coll)))

(= (set (split-by-type [1 :a 2 :b 3 :c])) #{[1 2 3] [:a :b :c]})
(= (set (split-by-type [:a "foo"  "bar" :b])) #{[:a :b] ["foo" "bar"]})
(= (set (split-by-type [[1 2] :a [3 4] 5 6 :b])) #{[[1 2] [3 4]] [:a :b] [5 6]})

;; Problem 54, Partition a Sequence

(defn -partition-by-all [n coll]
  (when-not (empty? coll)
    (cons (take n coll) (-partition-by-all n (drop n coll)))))

(= (-partition-by-all 3 (range 9)) '((0 1 2) (3 4 5) (6 7 8)))
(= (-partition-by-all 2 (range 8)) '((0 1) (2 3) (4 5) (6 7)))
(= (-partition-by-all 3 (range 6)) '((0 1 2) (3 4 5)))

;; Problem 55, Count Occurences
;; Special Restrictions : frequencies
(defn -occurance [coll]
  (reduce (fn [acc v]
           (assoc acc v (if (get acc v)
                          (inc (get acc v))
                          1)))
          {}
          coll))

(= (-occurance [1 1 2 3 2 1 1]) {1 4, 2 2, 3 1})
(= (-occurance [:b :a :b :a :b]) {:a 2, :b 3})
(= (-occurance '([1 2] [1 3] [1 3])) {[1 2] 1, [1 3] 2})

;; Problem 56, Find Distinct Items
;; Special Restrictions : distinct

(defn -distinct [coll]
  (mapv first (vals (group-by identity coll))))

(= (-distinct [1 2 1 3 1 2 4]) [1 2 3 4])
(= (-distinct [:a :a :b :b :c :c]) [:a :b :c])
(= (-distinct '([2 4] [1 2] [1 3] [1 3])) '([2 4] [1 2] [1 3]))
(= (sort (-distinct (range 9))) (range 9))

;; Problem 59, Juxtaposition
(defn -juxt [ & f]
  (fn [& args]
    (map #(apply % args) f)))


(= [21 6 1] ((-juxt + max min) 2 3 5 1 6 4))
(= ["HELLO" 5] ((-juxt #(.toUpperCase %) count) "hello"))
(= [2 6 4] ((-juxt :a :c :b) {:a 2, :b 4, :c 6, :d 8 :e 10}))

;; Problem 60, Sequence Reductions
;; Special Restrictions : reductions
(defn seq-reduction
  ([f coll] (seq-reduction f (first coll) (rest coll)))
  ([f initial coll]
   (lazy-seq
    (if (seq coll)
      (cons initial
            (seq-reduction f (f initial (first coll)) (rest coll)))
      [initial]))))

(= (take 5 (seq-reduction + (range))) [0 1 3 6 10])
(= (seq-reduction conj [1] [2 3 4]) [[1] [1 2] [1 2 3] [1 2 3 4]])
(= (last (seq-reduction * 2 [3 4 5])) (reduce * 2 [3 4 5]) 120)

;; Problem 65, Black Box Testing

(defn black-box-testing [c]
  (let [e (empty c)]
    (condp = e
      {} :map
      #{} :set
      () (if (reversible? c) :vector :list))))

(= :map (black-box-testing {:a 1, :b 2}))
(= :list (black-box-testing (range (rand-int 20))))
(= :vector (black-box-testing [1 2 3 4 5 6]))
(= :set (black-box-testing #{10 (rand-int 5)}))
(= [:map :set :vector :list] (map black-box-testing [{} #{} [] ()]))

;; Problem 69, Merge with a Function
;; Do not use merge-with

(defn -merge-with [f & maps]
  (reduce (fn [m1 m2]
            (reduce (fn [m e]
                      (let [k (key e) v (val e)]
                        (if (contains? m k)
                          (assoc m k (f (get m k) v))
                          (assoc m k v)))) 
                    (or m1 {}) (seq m2))
            )
          {}
maps))

(= (-merge-with * {:a 2, :b 3, :c 4} {:a 2} {:b 2} {:c 5})
   {:a 4, :b 6, :c 20})
(= (-merge-with - {1 10, 2 20} {1 3, 2 10, 3 15})
   {1 7, 2 10, 3 15})
(= (-merge-with concat {:a [3], :b [6]} {:a [4 5], :c [8 9]} {:b [7]})
   {:a [3 4 5], :b [6 7], :c [8 9]})

; Problem 70, Word Sorting

(defn word-count [words] 
  (sort-by #(.toLowerCase %) (re-seq #"\w+" words)))

(= (word-count  "Have a nice day.")
   ["a" "day" "Have" "nice"])
(= (word-count  "Clojure is a fun language!")
   ["a" "Clojure" "fun" "is" "language"])
(= (word-count  "Fools fall for foolish follies.")
   ["fall" "follies" "foolish" "Fools" "for"])

;; 	Problem 74, Filter Perfect Squares
(defn filter-perfect-square [s]
  (let [int-arr (map read-string (clojure.string/split s #","))
        filtered-arr (filter #(some (fn [x] 
                                      (= (* x x) %)) (range %)) int-arr)]
    (clojure.string/join "," filtered-arr)))

;; Problem 76, Intro to Trampoline
(= [1 3 5 7 9 11]
   (letfn
    [(foo [x y] #(bar (conj x y) y))
     (bar [x y] (if (> (last x) 10)
                  x
                  #(foo x (+ 2 y))))]
     (trampoline foo [] 1)))

;; Problem 77, Anagram Finder

(defn anagram-finder [s]
  (->> (group-by set s)
       (vals)
       (map set)
       (filter #(> (count %) 1))
       (set)))

(= (anagram-finder ["meat" "mat" "team" "mate" "eat"])
   #{#{"meat" "team" "mate"}})
(= (anagram-finder ["veer" "lake" "item" "kale" "mite" "ever"])
   #{#{"veer" "ever"} #{"lake" "kale"} #{"mite" "item"}})

;; Problem 78, Reimplement Trampoline
;; Special Restrictions : trampoline

(defn -trampolione [f & args]
  (loop [f (apply f args)]
    (if (fn? f)
      (recur (f))
      f)))

(= (letfn [(triple [x] #(sub-two (* 3 x)))
           (sub-two [x] #(stop? (- x 2)))
           (stop? [x] (if (> x 50) x #(triple x)))]
     (-trampolione triple 2))
   82)

(= (letfn [(my-even? [x] (if (zero? x) true #(my-odd? (dec x))))
           (my-odd? [x] (if (zero? x) false #(my-even? (dec x))))]
     (map (partial -trampolione my-even?) (range 6)))
   [true false true false true false])

;; Problem 80, Perfect Numbers
(defn perfect-number? [x]
  (= x (apply + (filter #(= 0 (mod x %)) (range 1 x)))))

(= (perfect-number? 6) true)
(= (perfect-number? 7) false)
(= (perfect-number? 496) true)
(= (perfect-number? 500) false)
(= (perfect-number? 8128) true)

;; Problem 85, Power Set

(defn pow [s]
  (if (empty? s) #{#{}}
      (let [f (first s)
            r (pow (rest s))]
        (into r (map #(conj % f) r)))))

;; Problem 93, Partially Flatten a Sequence

(defn -flatten [input]
  (loop [result input]
    (if-not (coll? (first result)) 
      result 
      (recur (first result)))))

(defn -partially-flattern[input]
  (distinct (let [f (fn [x]
                      (mapv #(-flatten %) x))]
              (f (filter sequential?
                         (rest (tree-seq sequential? seq input)))))))

(= (-partially-flattern [["Do"] ["Nothing"]])
   [["Do"] ["Nothing"]])
(= (-partially-flattern [[[[:a :b]]] [[:c :d]] [:e :f]])
   [[:a :b] [:c :d] [:e :f]])
(= (-partially-flattern '((1 2) ((3 4) ((((5 6)))))))
   '((1 2) (3 4) (5 6)))

;; Problem 98, Equivalence Classes
(defn -group-by [f coll]
  (into #{} (vals (reduce (fn [acc v]
                            (let [k (f v)]
                              (assoc acc k (conj (get acc k #{}) v))))
                          {}
                          coll))))

(= (-group-by #(* % %) #{-2 -1 0 1 2})
   #{#{0} #{1 -1} #{2 -2}})
(= (-group-by #(rem % 3) #{0 1 2 3 4 5})
   #{#{0 3} #{1 4} #{2 5}})
(= (-group-by identity #{0 1 2 3 4})
   #{#{0} #{1} #{2} #{3} #{4}})
(= (-group-by (constantly true) #{0 1 2 3 4})
   #{#{0 1 2 3 4}})

;; Problem 102, intoCamelCase
(defn into-camel-case[s]
  (let [words (re-seq #"[A-Za-z]+" s)]
    (apply str (first words) (map (fn [x]
                                    (let [w (re-seq #"[A-Za-z]" x)
                                          first-c (.toUpperCase (first w))]
                                      (apply str (concat first-c (rest w))))) 
                                  (rest words)))))

(= (into-camel-case "something") "something")
(= (into-camel-case "multi-word-key") "multiWordKey")
(= (into-camel-case "leaveMeAlone") "leaveMeAlone")

;; Problem 103, Generating k-combinations

(defn combinations [k lst]
  (if (empty? lst)
    []
    (if (= k 1)
      (map #(vector %) lst)
      (mapcat (fn [x] (combinations (dec k) (remove x lst) )) lst))))


;; (= (combinations 3 #{0 1 2 3 4}) #{#{0 1 2} #{0 1 3} #{0 1 4} #{0 2 3} #{0 2 4}
;;                          #{0 3 4} #{1 2 3} #{1 2 4} #{1 3 4} #{2 3 4}})



(defn k-combination [n coll] 
  (for [x (conj [] (first coll))
        y (into [] (for [x (next coll)
                         y (next coll)
                         :when (< x y)]
                     [x y]))]
    (flatten [x y])))

(defn k-combination [n coll]
  (map #(conj % (first coll))
       (for [x     (next coll)
             y     (next coll) 
             :when (< x y )]
         #{x y})))

(defn kc [n coll]
  (apply concat (loop [result #{}
                       s coll]
                  (if (empty? s)
                    result
                    (recur (conj result (k-combination n s)) (rest s))))))

(defn kc [k xs]
  (cond
    (= 0 k) #{#{}}
    (empty? xs) #{}
    :else
    (let [y (first xs) ys (rest xs)]
      (set (concat (kc k ys) (map #(conj % y) (kc (dec k) ys)))))))


(= (kc 1 #{4 5 6}) #{#{4} #{5} #{6}})
(= (kc 10 #{4 5 6}) #{})
(= (kc 2 #{0 1 2}) #{#{0 1} #{0 2} #{1 2}})
(= (kc 3 #{0 1 2 3 4}) #{#{0 1 2} #{0 1 3} #{0 1 4} #{0 2 3} #{0 2 4}
                                     #{0 3 4} #{1 2 3} #{1 2 4} #{1 3 4} #{2 3 4}})
(= (kc 4 #{[1 2 3] :a "abc" "efg"}) #{#{[1 2 3] :a "abc" "efg"}})
(= (kc 2 #{[1 2 3] :a "abc" "efg"}) #{#{[1 2 3] :a} #{[1 2 3] "abc"} #{[1 2 3] "efg"}
                                                  #{:a "abc"} #{:a "efg"} #{"abc" "efg"}})

;; Problem 105, Identify keys and values

(defn identify-keys-vals [coll]
  (loop [result {}
         s coll]
    (if (empty? s)
      result
      (if-not (keyword? (first s))
        (let [last-key (first (last result))
              last-val (second (last result))]
          (recur (assoc result last-key (conj last-val (first s))) (next s))) 
        (recur (assoc result (first s) (if (keyword? (second s))
                                            []
                                         [(second s)])) (nnext s))))))

(defn identify-keys-vals
  [coll]
  (if (empty? coll)
    {}
    (apply merge
           (when-let [st (seq coll)]
             (let [ft (first st)
                   run (reduce (fn [acc x]
                                 (if (keyword? x)
                                   (reduced acc )
                                   (conj acc x)))
                               []
                               (rest st))
                   result (assoc {} ft run)]
               (cons result (identify-keys-vals (drop (inc (count run)) st))))))))


(= {} (identify-keys-vals  []))
(= {:a [1]} (identify-keys-vals [:a 1]))
(= {:a [1]:b [2]} (identify-keys-vals [:a 1, :b 2]))
(= {:a [1 2 3] :b []:c [4]} (identify-keys-vals [:a 1 2 3 :b :c 4]))

;; Problem 108, Lazy Searching
(defn lazy-sarching [& s]
  (let [firsts (map first s)]
    (if (apply = firsts)
      (first firsts)
      (recur (map (fn [a] (filter #(>= % (apply max firsts)) a)) s)))))

(= 3 (lazy-sarching [3 4 5]))
(= 4 (lazy-sarching [1 2 3 4 5 6 7] [0.5 3/2 4 19]))
(= 64 (lazy-sarching (map #(* % % %) (range))
          (filter #(zero? (bit-and % (dec %))) (range))
          (iterate inc 20)))
(= 7 (lazy-sarching (range) (range 0 100 7/6) [2 3 5 7 11 13]))

;; Problem 110, Sequence of pronunciations
(defn sequence-of-pronunciations [x]
  (let [y (mapcat #(vector (count %) (first %)) (partition-by identity x))]
      (cons y (lazy-seq (sequence-of-pronunciations y)))))

(= [[1 1] [2 1] [1 2 1 1]] (take 3 (sequence-of-pronunciations [1])))
(= [3 1 2 4] (first (sequence-of-pronunciations [1 1 1 4 4])))
(= [1 1 1 3 2 1 3 2 1 1] (nth (sequence-of-pronunciations [1]) 6))
(= 338 (count (nth (sequence-of-pronunciations [3 2]) 15)))

;; Problem 112, Sequs Horribilis
(defn sequs-horribilis
  [n c]
  (loop [res []
         elements c]
    (if (or (empty? elements) (> (reduce + (flatten res)) n))
      res
      (if-not (coll? (first elements))
        (if (> (+ (reduce + (flatten res)) (first elements)) n)
          res
          (recur (conj res (first elements)) (drop 1 elements)))
        (recur (conj res (sequs-horribilis (- n (reduce + (flatten res))) (first elements))) (drop 1 elements))))))

(=  (sequs-horribilis 10 [1 2 [3 [4 5] 6] 7])    '(1 2 (3 (4))))
(=  (sequs-horribilis 30 [1 2 [3 [4 [5 [6 [7 8]] 9]] 10] 11])    '(1 2 (3 (4 (5 (6 (7)))))))
(=  (sequs-horribilis 9 (range))    '(0 1 2 3))
(=  (sequs-horribilis 1 [[[[[1]]]]])    '(((((1))))))
(=  (sequs-horribilis 0 [1 2 [3 [4 5] 6] 7])    '())
(=  (sequs-horribilis 0 [0 0 [0 [0]]])    '(0 0 (0 (0))))
(=  (sequs-horribilis 1 [-10 [1 [2 3 [4 5 [6 7 [8]]]]]])
    '(-10 (1 (2 3 (4)))))

;; Problem 114, Global take-while
(defn take-n-while [n p coll]
  (let [counter (atom 0)]
    (lazy-seq
     (when-let [item (seq coll)]
       (when (and (< @counter n) (p (first item)))
         (swap! counter inc))
       (when (< @counter n)
         (cons (first item) (take-n-while n p (rest item))))))))

(= [2 3 5 7 11 13]
   (take-n-while 4 #(= 2 (mod % 3))
       [2 3 5 7 11 13 17 19 23]))
(= ["this" "is" "a" "sentence"]
   (take-n-while 3 #(some #{\i} %)
       ["this" "is" "a" "sentence" "i" "wrote"]))
(= ["this" "is"]
   (take-n-while 1 #{"a"}
       ["this" "is" "a" "sentence" "i" "wrote"]))

;; Problem 115, The Balance of N
(defn reverse-number[ number]
  (loop [result 0
         n number]
    (if (<= n 0)
      result
      (recur (+ (mod n 10) (*  result 10)) (quot n 10)))))

(defn balanced-number [number]
  (let [rev (reverse-number number)]
    (if (= rev number)
      true
      false)))

(= true (balanced-number 11))
(= true (balanced-number 121))
(= false (balanced-number 123))
(= true (balanced-number 0))
(= false (balanced-number 88099))
(= true (balanced-number 89098))
(= false (balanced-number 89089))

(= (take 20 (filter balanced-number (range)))
   [0 1 2 3 4 5 6 7 8 9 11 22 33 44 55 66 77 88 99 101])

;; Problem 116, Prime Sandwich

(defn bp [x]
  (letfn [(is-prime [x]
            (condp = x
              1 nil
              2 2
              (when (not-any? #(= 0 (mod x %)) (range 2 (inc (quot x 2))))
                x)))]
    (cond
      (<= x 2) false
      (not (is-prime x)) false
      :else (let [lp (some is-prime (range (dec x) 1 -1))
                  rp (some is-prime (drop (inc x) (range)))]
              (= (/ (+ lp rp) 2) x)))))

(= false (bp 4))
(= true (bp 563))
(= 1103 (nth (filter bp (range)) 15))

;; Problem 118, Re-implement Map
(defn -map [f xs]
  (lazy-seq
   (when-let [s (seq xs)]
     (cons (f (first s))
           (-map f (rest s))))))



(= [3 4 5 6 7]
   (-map inc [2 3 4 5 6]))
(= (repeat 10 nil)
   (-map (fn [_] nil) (range 10)))
(= [1000000 1000001]
   (->> (-map inc (range))
        (drop (dec 1000000))
        (take 2)))
(= [1000000 1000001]
   (->> (-map inc (range))
        (drop (dec 1000000))
        (take 2)))
