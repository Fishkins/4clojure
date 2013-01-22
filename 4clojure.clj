;; Problem 19

#(first (reverse %))

(fn [seqs] 
  (reduce (fn [fst snd] snd) nil seqs))

(fn [fullseq] 
  (loop [parseq
         fullseq]
    (let [more (rest parseq)]
      (if (empty? more)
        (first parseq)
        (recur more)))))

(defn clj19 [seqs]
  (if (= (count seqs) 1)
    (first seqs)
    (clj19 (rest seqs))))

;; Problem 20

#(first (rest (reverse %)))

;; Problem 21

#(first (drop %2 %1))

;; Problem 22

(fn [seq]
  (reduce (fn [x y] (inc x)) 0 seq))

(fn [seq]
  (reduce + 0 (map (fn [x] 1) seq)))

(fn [seq]
  (loop [parseq seq    
         total   0]
    (if (empty? parseq)
      total
      (recur (rest parseq) (inc total)))))

;; Problem 23

#(reduce + 0 %)

;; Problem 24

#(filter odd? %)

;; Problem 25

(fn [seq]
  (reduce #(cons %2 %1) () seq))

;; Problem 26

#(=
  (reverse %)
  (seq %))

;; Problem 27

(fn [n]
  (let [fibs (lazy-cat [0 1] (map + fibs (rest fibs)))]
  (take n fibs)))

;; Problem 28

;; O(n * lg(n))
(fn [& args]
  (first (sort > args)))

;; O(n)
(fn [& args]
  (reduce #(if (> %1 %2) %1 %2) args))

;; Problem 29

(fn [x]
  (apply str (filter #(Character/isUpperCase %) x)))

;; Problem 30

4

;; Problem 31

(fn [col]
  (reverse (reduce #(cons %2 (cons %2 %1)) [] col)))

;; Problem 32

(fn [beg end]
  (loop [n   (dec end)
         col []]
    (if (> beg n)
      col
      (recur (dec n) (cons n col)))))

;; Problem 33

#(reduce * (range 1 (inc %)))

;; Problem 30

4

;; Problem 31

(fn flatten' [col]
  (reduce #(concat %1 (if (sequential? %2) (flatten' %2) [%2])) []  col))
