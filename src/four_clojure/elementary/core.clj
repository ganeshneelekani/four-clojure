(ns four-clojure.elementary.core)

; Problem 1, Nothing but the Truth
(= true true)

; Problem 2, Simple Math
(= (- 10 (* 2 3)) 4)

; Problem 3, Strings
(= "HELLO WORLD" (.toUpperCase "hello world"))

; Problem 4, Lists
(= (list :a :b :c) '(:a :b :c))

; Problem 5, conj on lists
(= (list 1 2 3 4) (conj '(2 3 4) 1))

; Problem 6, Vectors
(= [:a :b :c] (list :a :b :c) (vec '(:a :b :c)) (vector :a :b :c))

; Problem 7, conj on vectors

(= [ 1 2 3 4] (conj [1 2 3] 4))

(= [ 1 2 3 4] (conj [1 2] 3 4))

; Problem 8, Sets
(=  #{:a :b :c :d} (set '(:a :a :b :c :c :c :c :d :d)))

(= #{:a :b :c :d} (clojure.set/union #{:a :b :c} #{:b :c :d}))

; Problem 9, conj on sets
(= #{1 2 3 4} (conj #{1 4 3} 2))

; Problem 10, Maps
(= 20 ((hash-map :a 10, :b 20, :c 30) :b))


(= 20 (:b {:a 10, :b 20, :c 30}))

;; Problem 11, conj on maps
(= {:a 1, :b 2, :c 3} (conj {:a 1} [:b 2] [:c 3]))

; Problem 12, Sequences
(= 3 (first '(3 2 1)))

(= 3 (second [2 3 4]))

(= 3 (last (list 1 2 3)))

;Problem 13, rest 
(= [20 30 40] (rest [10 20 30 40]))

; Problem 14, Functions
(= 8 ((fn add-five [x] (+ x 5)) 3))

(= 8 ((fn [x] (+ x 5)) 3))

(= 8 (#(+ % 5) 3))

(= 8 ((partial + 5) 3))

; Problem 15, Double Down

(= (+ 2 2) 4)
(= (+ 3 3) 6)
(= (+ 11 11) 22)
(= (+ 7 7) 14)

; Problem 16, Hello World
(= (#(str "Hello, " % "!") "Dave") "Hello, Dave!")
(= (#(str "Hello, " % "!") "Jenn") "Hello, Jenn!")
(= (#(str "Hello, " % "!") "Rhea") "Hello, Rhea!")

; Problem 17, map
(= (list 6 7 8) (map #(+ % 5) '(1 2 3)))

; Problem 18, filter
(= (list 6 7) (filter #(> % 5) '(3 4 5 6 7)))

;; Problem 35, Local bindings
(= 7 (let [x 5] (+ 2 x)))
(= 7 (let [x 3, y 10] (- y x)))
(= 7 (let [x 21] (let [y 3] (/ x y))))

;; Problem 36, Let it Be
(= 10 (let [x 5 y 5] (+ x y)))
(= 4 (let [y 2 z 2] (+ y z)))
(= 1 (let [z 1] z))

;; Problem 37, Regular Expressions
(= "ABC" (apply str (re-seq #"[A-Z]+" "bA1B3Ce ")))

;; Problem 52, Intro to Destructuring
(= [2 4] (let [[a b c d e f g] (range)] (into [] (take 2 (filter even? (range 1 10))))))

;; Problem 57, Simple Recursion
(= `(5 4 3 2 1) ((fn foo [x] (when (> x 0) (conj (foo (dec x)) x))) 5))