;; Problem 19

#(first (reverse %))

(fn [seqs] 
  (reduce (fn [fst snd] snd) nil seqs))

(fn [fullseq] 
  (loop [parseq fullseq]
    (let [more (rest parseq)]
      (if (empty? more)
        (first parseq)
        (recur more)))))

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

(fn range' [beg end]
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

;; Intro to Iterate

'(1 4 7 10 13)

;; Compress a Sequence

(fn compress [col]
  (let [repeat-elem? #(= %2 (first %1))
        add-new #(if (repeat-elem? %1 %2) %1 (cons %2 %1))]
    (reverse (reduce add-new [] col))))
            
;; Interleave two Seqs

(fn interleave' [col1 col2]
  (mapcat #(list %1 %2) col1 col2))

;; Replicate a Seq

(fn replicate' [col n]
  (mapcat #(repeat n %1) col))

;; Interpose a Seq

(fn interpose' [item col]
  (rest (reverse (reduce #(cons %2 (cons item %1)) [] col))))

;; Destructring Intro

[c e]

;; Drop Nth

(fn drop-nth [col n]
  (loop [coll col
         i    1
         acc  []]
    (if (empty? coll)
      (reverse acc)
      (let [newacc (if (= 0 (rem i n)) acc (cons (first coll) acc))]
        (recur (rest coll) (inc i) newacc)))))
        
;; Split Col

#(list (take %1 %2) (drop %1 %2))

;; Advanced Destructring

(range 1 6)

;; Half-Truth

(fn half-truth [& bools]
  (boolean (and (some #(boolean %) bools)
                (some #(not %) bools))))

;; Map Construction

#(apply hash-map (interleave %1 %2))

;; GCD

(fn GCD [a b]
  (if (= (rem x y) 0)
    y
    (recur y (rem x y))))

;; Set Intersection

(fn intersection' [set1 set2]
  (set (filter #(contains? set2 %) set1)))

;; iterate

(fn iterate' [f n]
  (lazy-seq
   (cons n (iterate' f (f n)))))

;; Closures

(fn make-exp [exp]
  #(int (Math/pow % exp)))

;; Product Digits

(fn digit-product [a b]
  (map #(Integer/parseInt (str %)) (str (* a b))))

;; Cartesian

(fn cartesian [col1 col2]
  (into #{} (mapcat (fn [elem] (map #(vector elem %) col2)) col1)))

;; group-by

(fn group-by' [f coll]
  (let [keys       (set (map f coll))
        get-by-key (fn [elem]
                     (filter #(= (f %) elem) coll))
        add-vals   (fn [acc elem]
                     (cons elem (cons (get-by-key elem) acc)))]
    (apply hash-map (reduce add-vals () keys))))
  
;; Symmetric Difference

(fn sym-diff [set1 set2]
  (clojure.set/union
   (clojure.set/difference set1 set2)
   (clojure.set/difference set2 set1)))

;; Binary

(fn eval-binary [num]
  (let [powers (take (count num) (iterate #(* 2 %) 1))
        bins   (reverse (map #(Integer/parseInt (str %)) num))
        bin-pows (map * powers bins)]
    (reduce + 0 bin-pows)))
    
;; Looking Class

Class

;; dot product

(fn dot-product [v1 v2]
  (reduce + (map * v1 v2)))

;; Pascal's triangle

(fn pascals [n]
  (loop [curr-n   (dec n)
         curr-row [1]]
    (if (= curr-n 0)
      curr-row
      (let [extended-row (cons 0 curr-row)
            new-row      (map + extended-row (reverse extended-row))]
        (recur (dec curr-n) new-row)))))
