(ns four-clojure.medium.core)

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