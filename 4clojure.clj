;; Problem 1
true

;; 2
4

;; 3
"HELLO WORLD" 

;; 4
:a :b :c

;; 5
'(1 2 3 4)

;; 6
:a :b :c

;; 7
[1 2 3 4]

;; 8
#{:a :b :c :d}

;; 9
2

;; 10
20

;; 11
[:b 2]

;; 12
3

;; 13
[20 30 40]

;; 14
8

;; 15
* 2

;; 16
#(str "Hello, " % "!")

;; 17
[6 7 8]

;; 18
'(6 7)

;; Problem 19
#(first (reverse %))

;; Problem 20
#(first (rest (reverse %)))

;; Problem 21
#(first (drop %2 %1))

;; Problem 22
(fn [seq]
  (reduce (fn [x y] (inc x)) 0 seq))

;; Problem 23
(fn [seq]
  (reduce #(cons %2 %1) () seq))

;; Problem 24
#(reduce + 0 %)

;; Problem 25
#(filter odd? %)

;; Problem 26
(fn [n]
  (let [fibs (map first (iterate (fn [[a b]] [b (+ a b)]) [1 1]))]
  (take n fibs)))

;; Problem 27
#(=
  (reverse %)
  (seq %))

;; Problem 28
(fn flatten' [col]
  (reduce #(concat %1 (if (sequential? %2) (flatten' %2) [%2])) []  col))

;; Problem 29
(fn [x]
  (apply str (filter #(Character/isUpperCase %) x)))

;; Problem 30
(fn compress [col]
  (let [repeat-elem? #(= %2 (first %1))
        add-new #(if (repeat-elem? %1 %2) %1 (cons %2 %1))]
    (reverse (reduce add-new [] col))))
            
;; Problem 31
(partial partition-by identity)

;; Problem 32
(fn [coll] (mapcat #(list %1 %1) coll))

;; Problem 33
(fn replicate' [col n]
  (mapcat #(repeat n %1) col))

;; Problem 34
(fn range' [beg end]
  (loop [n   (dec end)
         col []]
    (if (> beg n)
      col
      (recur (dec n) (cons n col)))))

;; Problem 35
7

;; Problem 36
[z 1 y 3 x 7]

;; Problem 37
"ABC"

;; Problem 38
(fn [& args]
  (first (sort > args)))

;; Problem 39
(fn interleave' [col1 col2]
  (mapcat #(list %1 %2) col1 col2))

;; Problem 40
(fn interpose' [item col]
  (rest (reverse (reduce #(cons %2 (cons item %1)) [] col))))

;; Problem 41
(fn drop-nth [col n]
  (loop [coll col
         i    1
         acc  []]
    (if (empty? coll)
      (reverse acc)
      (let [newacc (if (= 0 (rem i n)) acc (cons (first coll) acc))]
        (recur (rest coll) (inc i) newacc)))))
        
;; Problem 42
#(reduce * (range 1 (inc %)))

;; Problem 43
(fn rev-interleave [col num]
  (loop [old-col col
         new-col (take num (repeat nil))]
    (if (empty? old-col)
      (map reverse new-col)
      (let [rest-col (drop num old-col)
            mapped-col (map cons (take num old-col) new-col)]
        (recur rest-col mapped-col)))))

;; Problem 44
(fn rotate [n col]
  (let [offset (mod n (count col))
        beginning (drop offset col)
        ending (take offset col)]
    (concat beginning ending)))

;; Problem 45
'(1 4 7 10 13)

;; Problem 46
(fn flip [f]
  #(f %2 %1))

;; Problem 47
4

;; Problem 48
6

;; Problem 49
#(list (take %1 %2) (drop %1 %2))

;; Problem 50
(fn split-by-type [col]
 (for [cur-type (set (map type col))]
   (filter #(= cur-type (type %)) col)))
   
;; Problem 51
(range 1 6)

;; Problem 52
[c e]

;; Problem 53
(fn long-inc-sub-seq [[fst & rst]]
  (loop [[x & xs] rst
         prev     fst
         curr     [fst]
         longest  []]
    (if (nil? x)
      (if (> (count longest) 1)
        longest
        [])
      (let [curr     (if (= (inc prev) x)
                       (conj curr x)
                       [x])
            longest  (if (> (count curr) (count longest))
                       curr
                       longest)]
        (recur xs x curr longest)))))

;; Problem 54
(fn partition' [n col]
  (loop [parts    ()
         rest-col col]
    (if (< (count rest-col) n)
      (reverse parts)
      (recur (cons (take n rest-col) parts) (drop n rest-col)))))

;; Problem 55
(fn count-occurs [col]
  (apply hash-map
         (apply concat
                (for [occur (set col)]
                  (list occur (count (filter #(= occur %) col)))))))

;; Problem 56
(fn distinct' [col]
  (let [add-uniq (fn [acc elem]
                   (if (some #(= % elem) acc)
                     acc
                     (cons elem acc)))]
    (reverse (reduce add-uniq () col))))

;; Problem 57
[5 4 3 2 1]

;; Problem 58
(fn comp' [& fns]
  (fn [& args]
    (let [[fst-fn & rst-fns] (reverse fns)]
      (reduce #(%2 %1) (apply fst-fn args) rst-fns))))

;; Problem 59
(fn juxt' [& fns]
  (fn [& args]
    (map #(apply % args) fns)))

;; Problem 60
(fn reductions' [f & args]
  (let [[fst & rst] (if (> (count args) 1)
                      (apply cons args)
                      (first args))]
    ((fn reduct-helper [accum col]
       (lazy-seq
        (if (empty? col)
          (list accum)
          (let [[fst & rst] col]
            (cons accum (reduct-helper (f accum fst) rst))))))
     fst rst)))

;; 61
#(apply hash-map (interleave %1 %2))

;; 62
(fn iterate' [f n]
  (lazy-seq
   (cons n (iterate' f (f n)))))

;; 63
(fn group-by' [f coll]
  (let [keys       (set (map f coll))
        get-by-key (fn [elem]
                     (filter #(= (f %) elem) coll))
        add-vals   (fn [acc elem]
                     (cons elem (cons (get-by-key elem) acc)))]
    (apply hash-map (reduce add-vals () keys))))

;; 64
(partial + 0)

;; 65
(fn guess-type [mystery-col]
  (let [col (conj mystery-col [1 2])
        orig-count (count col)
        self-conj (conj col col)
        first-conj (conj col (first col))]
    (cond
     (= orig-count (count self-conj)) :map
     (= orig-count (count first-conj)) :set
     (= col (first self-conj)) :list
     :else :vector)))

;; 66
(fn GCD [x y]
  (if (= (rem x y) 0)
    y
    (recur y (rem x y))))

;; 67
(fn make-primes [n]
  (loop [curr   2
         primes []]
    (if (>= (count primes) n)
      primes
      (let [is-divisible   (fn [num] (= (rem curr num) 0))
            some-divisible (some is-divisible primes)
            new-primes     (if (not some-divisible) (conj primes curr) primes)]
        (recur (inc curr) new-primes)))))

;; 68
[7 6 5 4 3]

;; 69
(fn merge-with' [f & maps]
  (let [merger (fn [result latter]
                 (let [res-set (set (keys result))
                       lat-set (set (keys latter))
                       dupes   (clojure.set/intersection res-set lat-set)
                       combine #(list % (f (result %) (latter %)))
                       dupe-map (apply hash-map (mapcat combine dupes))]
                   (merge result latter dupe-map)))]
    (reduce merger maps)))

;; 70
(fn sort-words [sentence]
  (sort-by clojure.string/upper-case (clojure.string/split sentence #"(\p{Punct}| )")))

;; 71
last

;; 72
apply +

;; 73
(fn tic-tac-toe [[[a b c]
                  [d e f]
                  [g h i]]]
  "Boring, inflexible solution, but it works efficiently"
  (cond (= a b c :x) :x 
        (= d e f :x) :x 
        (= g h i :x) :x 
        (= a d g :x) :x 
        (= b e h :x) :x 
        (= c f i :x) :x 
        (= a e i :x) :x 
        (= c e g :x) :x 
        (= a b c :o) :o 
        (= d e f :o) :o 
        (= g h i :o) :o 
        (= a d g :o) :o 
        (= b e h :o) :o 
        (= c f i :o) :o 
        (= a e i :o) :o 
        (= c e g :o) :o 
        :else nil))

(fn tic-tac-toe [board]
  "Works for any board size"
  (let [winner      (fn [row]
                      "Returns winner of this row (:x, :o, or nil if none)"
                      (if (and (apply = row)
                               (not= (first row) :e))
                        (first row)
                        nil))
        find-winner (fn [rows]
                      (some #{:x :o} (map winner rows)))
        swapped     (vec (apply map vector board))
        diag        (fn [rows]
                      (map nth rows (range)))]
    (or (find-winner board)
        (find-winner swapped)
        (winner (diag board))
        (winner (diag (reverse board))))))

;; 74
(fn filt-squares [num-str]
  (let [nums (map #(read-string %) (clojure.string/split num-str #","))
        is-square? (fn [num]
                     (let [sqrt-num (Math/sqrt num)]
                       (= sqrt-num (Math/floor sqrt-num))))]
    (clojure.string/join "," (filter is-square? nums))))

;; 75
(fn num-coprimes [number]
  (if (= number 1)
    1
    (let [gcd (fn gcd [x y]
                (if (= (rem x y) 0)
                  y
                  (recur y (rem x y))))
          is-coprime? #(= 1 (gcd % number))
          coprimes (filter is-coprime? (range 1 number))]
      (count coprimes))))

;; 76
[1 3 5 7 9 11]

;; 77
(fn find-anagrams [col]
  (->>
   (group-by sort col)
   vals
   (map set)
   (filter #(> (count %) 1))
   set))

;; 78
(fn trampoline' [f & args]
  (loop [result (apply f args)]
    (if (ifn? result)
      (recur (result))
      result)))

;; 79
(fn triangle-path [triangle]
  "Standard dynamic programming sol'n"
  (loop [[layer & layers]  triangle
         opts              '([0])]
    (if (nil? layer)
      (apply min (map #(apply min %) opts))
      (let [choose-best (fn [value curr-opts]
                          (+ value (apply min curr-opts)))
            best-paths  (map choose-best layer opts)
            make-opt    (fn [opt1 opt2]
                          (remove nil? (list opt1 opt2)))
            new-opts    (map make-opt (cons nil best-paths) (concat best-paths '(nil)))]
        (recur layers new-opts)))))

;; 80
(fn is-perfect? [number]
  (let [factors (filter #(= 0 (rem number %)) (range 1 number))]
    (= number (reduce + factors))))

;; 81
(fn intersection' [set1 set2]
  (set (filter #(contains? set2 %) set1)))

;; 82
(fn chainable? [words]
  (let [levenshtein (memoize
                     (fn levenshtein [[fst1 & rst1 :as str1]
                                      [fst2 & rst2 :as str2]]
                       (cond (empty? str1) (count str2)
                             (empty? str2) (count str1)
                             :else
                             (let [cost (if (= fst1 fst2)
                                          0
                                          1)]
                               (min
                                (+ (levenshtein str1 rst2) 1)
                                (+ (levenshtein rst1 str2) 1)
                                (+ (levenshtein rst1 rst2) cost))))))

        sub-chainable? (fn sub-chainable? [fst rst]
                         (if (empty? rst)
                           true
                           (let [matches (filter #(= 1 (levenshtein fst %)) rst)]
                             (if (empty? matches)
                               false
                               (some true?
                                     (map
                                      #(sub-chainable? % (remove (partial = %) rst))
                                      matches))))))]
    
    (true?
     (some true? (map #(sub-chainable? % (remove (partial = %) words)) words)))))

;; 83
(fn half-truth [& bools]
  (boolean (and (some #(boolean %) bools)
                (some #(not %) bools))))

;; 84
(fn transitive-closure [bin-rel]
  (let [get-transitives (fn [[fst-val snd-val :as pair]]
                          "Returns the given pair, plus any pairs one step away"
                          (cons pair
                                (for [[fst snd] bin-rel
                                      :when (= snd-val fst)]
                                  [fst-val snd])))
        new-rel         (set (mapcat get-transitives bin-rel))]
    ;; Cycle till we stop finding new pairs
    (if (= (count bin-rel) (count new-rel))
      bin-rel
      (recur new-rel))))

;; 85
;; works, but super inefficient
(fn power-set [coll]
  (if (empty? coll)
    #{#{}}
    (conj
     (set (mapcat #(power-set (disj coll %)) coll))
     coll)))

;; Doesn't examine subsets multiple times, and has TCO
(fn power-set [coll]
  (loop [sets #{}
         new-sets #{coll}]
    (if (empty? new-sets)
      sets
      (let [get-subsets (fn [elem-set]
                          (set (map #(disj elem-set %) elem-set)))
            subsets     (map get-subsets new-sets)
            new-subsets (apply clojure.set/union subsets)]
        (recur (clojure.set/union sets new-sets) new-subsets)))))

;; 86
(fn is-happy? [number]
  (loop [digits (str number)
         prevs  #{}]
    (let [square #(* % %)
          add-square #(+ (square (read-string (str %2))) %1)
          sum-squares #(reduce add-square 0 (str %))
          dig-sum (sum-squares digits)]
      (cond
       (= dig-sum 1) true
       (contains? prevs dig-sum) false
       :else
       (recur dig-sum (conj prevs dig-sum))))))

;; 88
(fn sym-diff [set1 set2]
  (clojure.set/union
   (clojure.set/difference set1 set2)
   (clojure.set/difference set2 set1)))

;; 89
(fn traversable? [graph]
  (let [remove-elem      (fn [elem coll]
                           "This really seems like it should be in core. Found this on SO when I was looking for docs."
                           (let [[n m] (split-with (partial not= elem) coll)] (concat n (rest m))))
        
        sub-traversable? (fn sub-traversable? [node edges]
                           (if (empty? edges)
                             true
                             (let [any-traversable? (fn [[fst snd :as edge]]
                                                      "Examine any valid orientation of the edge"
                                                      (let [rst (remove-elem edge edges)]
                                                        (or
                                                         (and (= node fst) (sub-traversable? snd rst))
                                                         (and (= node snd) (sub-traversable? fst rst)))))]
                               (some true? (map any-traversable? edges)))))

        fst-traversable? (fn fst-traversable? [[fst snd :as edge]]
                           "Special fn to handle the fact that we can use either end of the first edge"
                           (let [edges (remove-elem edge graph)]
                             (or
                              (sub-traversable? fst edges)
                              (sub-traversable? snd edges))))]
    
    (true? (some true? (map fst-traversable? graph)))))

;; 90
(fn cartesian [col1 col2]
  (into #{} (mapcat (fn [elem] (map #(vector elem %) col2)) col1)))

;; 91
(fn connected-graph? [graph]
  "Might as well use the sol'n to the previous problem.
In a real situation, I wouldn't have to include this as part of a let."
  (let [bi-graph (clojure.set/union
                  graph
                  (map reverse graph))
        vertices (set (flatten (seq graph)))
        num-verts (count vertices)
        full-num-verts (* num-verts num-verts)
        transitive-closure (fn [bin-rel]
                             (let [get-transitives (fn [[fst-val snd-val :as pair]]
                                                     "Returns the given pair, plus any pairs one step away"
                                                     (cons pair
                                                           (for [[fst snd] bin-rel
                                                                 :when (= snd-val fst)]
                                                             [fst-val snd])))
                                   new-rel         (set (mapcat get-transitives bin-rel))]
                               ;; Cycle till we stop finding new pairs
                               (if (= (count bin-rel) (count new-rel))
                                 bin-rel
                                 (recur new-rel))))]
    (= (count (transitive-closure bi-graph)) full-num-verts)))

;; 92
(fn read-roman [numeral]
  (let [roman-map {\D 500, \L 50, \X 10, \M 1000, \V 5, \C 100, \I 1}]
    (loop [[digit & digits] (map roman-map numeral)
           value            0]
      (if (nil? digit)
        value
        (let [new-val (if (some #(> % digit) digits)
                        (- value digit)
                        (+ value digit))]
          (recur digits new-val))))))

;; 93
(fn part-flat [coll]
  (if (every? (complement sequential?) coll)
    (list coll)
    (mapcat part-flat coll)))

;; 94
(fn life [board]
  "Ugly, but it works. Should at least be rewritten using map-indexed."
  (let [count-live  (fn [coll]
                      (count (filter #(= \# %) coll)))
        get-neighbs (fn [coll n]
                      (let [elems (if (= n 0)
                                    2
                                    3)]
                        (take elems (drop (dec n) coll))))
        count-row   (fn [row x]
                      (count-live (get-neighbs row x)))
        count-rows  (fn [x y]
                      (apply + (map #(count-row % x) (get-neighbs board y))))
        lives?      (fn [x y]
                      (let [curr-gen    (->
                                         board
                                         (nth y)
                                         (nth x))
                            is-alive    (= \# curr-gen)
                            num-cells   (count-rows x y)
                            num-neighbs (if is-alive
                                          (dec num-cells)
                                          num-cells)]
                        (println num-cells num-neighbs is-alive)
                        (if is-alive
                          (or (= num-neighbs 2) (= num-neighbs 3))
                          (= num-neighbs 3))))]
    (for [y (range (count board))]
      (clojure.string/join
       (for [x (range (count (first board)))]
         (if (lives? x y)
           "#"
           " "))))))

(fn life [board]
  "This is a little cleaner. Still not sure if it's ideal.
It bothers me that we have to examine every element of the
board so many times."
  (let [count-live  (fn [coll]
                      (count (filter #(= \# %) coll)))
        get-neighbs (fn [coll n]
                      (let [elems (if (= n 0)
                                    2
                                    3)]
                        (take elems (drop (dec n) coll))))
        count-row   (fn [row x]
                      (count-live (get-neighbs row x)))
        count-rows  (fn [x y]
                      (apply + (map #(count-row % x) (get-neighbs board y))))
        lives?      (fn [is-alive num-cells]
                      "Will a cell live here next turn?
num-cells includes the current spot"
                      (if is-alive
                        (or (= num-cells 3) (= num-cells 4))
                        (= num-cells 3)))
        next-gen    (fn [x y cell]
                      "The cell in this spot next turn"
                      (let [is-alive    (= \# cell)
                            num-cells   (count-rows x y)]
                        (if (lives? is-alive num-cells)
                          "#"
                          " ")))
        next-row    (fn [y row]
                      "All the new cells for the given row."
                      (clojure.string/join
                       (map-indexed #(next-gen %1 y %2) row)))]
    (map-indexed next-row board)))

;; 95
(fn tree? [col]
  (cond (nil? col)
        true
        (not (or (vector? col) (seq? col)))
        false
        (= (count col) 3)
        (and (tree? (second col)) (tree? (nth col 2)))
        :else
        false))
     
;; 96
(fn sym-tree [tree]
  (let [rev-leaves (fn rev-leaves [branch]
                     (if (nil? branch)
                       branch
                       (let [fst (first branch)
                             snd (second branch)
                             thd (nth branch 2)]
                         (list fst (rev-leaves thd) (rev-leaves snd)))))]
    (= (second tree) (rev-leaves (nth tree 2)))))

;; 97
(fn pascals [n]
  (loop [curr-n   (dec n)
         curr-row [1]]
    (if (= curr-n 0)
      curr-row
      (let [extended-row (cons 0 curr-row)
            new-row      (map + extended-row (reverse extended-row))]
        (recur (dec curr-n) new-row)))))

;; 98
(fn equivalence [f D]
  (set (map (comp set second) (group-by f D))))

;; 99
(fn digit-product [a b]
  (map #(read-string (str %)) (str (* a b))))

;; 100
(fn LCM [& nums]
  (let [GCD (fn GCD [x y]
                (if (= (rem x y) 0)
                  y
                  (recur y (rem x y))))
        bin-LCM (fn [x y]
                  (/ (* x y) (GCD x y)))]
    (reduce bin-LCM nums)))

;; 101
(fn levens [this-str that-str]
  "We need to pass a memoized version of the function into
the function for the memoization to apply to each
recursive call. In practice, a defn would take care of this,
but it isn't allowed on 4clojure."
  (let [levenshtein 
        (fn levenshtein [mem-leven
                         [fst1 & rst1 :as str1]
                         [fst2 & rst2 :as str2]]
          (cond (empty? str1) (count str2)
                (empty? str2) (count str1)
                :else
                (let [cost (if (= fst1 fst2) 0 1)
                      levenshtein (fn [x y]
                                    (mem-leven mem-leven x y))]
                  (min
                   (+ (levenshtein str1 rst2) 1)
                   (+ (levenshtein rst1 str2) 1)
                   (+ (levenshtein rst1 rst2) cost)))))
        other-leven (memoize levenshtein)]
    (other-leven other-leven this-str that-str)))

;; 102
(fn to-camel-case [hyphen-string]
  (let [[fst & rst] (clojure.string/split hyphen-string #"-")
        cap-words (cons fst (map clojure.string/capitalize rst))]
    (clojure.string/join cap-words)))

;; 103
(fn k-combinations [k S]
  (if (= k (count S))
    #{S}
    (set (mapcat #(k-combinations k (disj S %)) S))))

;; 104
(fn int-to-roman [num]
  (let [roman-map {1000 "M", 500 "D", 100 "C", 50 "L", 10 "X", 5 "V", 1 "I"}]
    (loop [part-num num
           roman ""
           digits (sort > (map first roman-map))]
      (if (= part-num 0)
        roman
        (let [[fst-dig & rst-digs] digits]
          (if (>= part-num fst-dig)
            (recur (- part-num fst-dig)
                   (str roman (roman-map fst-dig)) 
                   digits)
            (let [is-subtractor (fn [digit]
                                  (and
                                   (> (/ fst-dig 2) digit)
                                   (<= (/ fst-dig 10) digit)
                                   (not (= 5 digit))))
                  subtractor (or
                              (first (filter is-subtractor digits))
                              0)
                  cutoff (- fst-dig subtractor)]
              (if (> cutoff part-num)
                (recur part-num roman rst-digs)
                (recur (+ part-num subtractor)
                       (str roman (roman-map subtractor))
                       digits)))))))))

;; 105
(fn key-values [coll]
  (loop [key-vals {}
         col coll]
    (if (empty? col)
      key-vals
      (let [[fst-key & more] col
            [fst-vals rst] (split-with (complement keyword?) more)]
        (recur (assoc key-vals fst-key fst-vals) rst)))))

;; 106
(fn shortest-path [start end]
  (let [apply-ops (fn [n]
                    (let [dub-add [(* 2 n) (+ 2 n)]]
                      (if (even? n)
                        (conj dub-add (/ n 2))
                        dub-add)))
        apply-all #(set (mapcat apply-ops %))
        winner?   #(= end %)]
    (loop [nums [start]
           cnt  1]
      (if (some winner? nums)
        cnt
        (recur (apply-all nums) (inc cnt))))))

;; 107
(fn make-exp [exp]
  #(int (Math/pow % exp)))

;; 108
(fn lazy-min [& colls]
  "Takes one elem from each coll at a time. Works, but too slow with exponential colletions"
  (loop [seen (repeat (count colls) #{})
         new colls]
    (let [new-seen (map conj seen (map first new))
          enough (apply clojure.set/intersection new-seen)]
      (if (not (empty? enough))
        (apply min enough)
        (recur new-seen (map rest new))))))

(fn lazy-min [& colls]
  "Searches based on max number to take up through.
Repeats some work, but doesn't examine too far in lists with huge values."
  (loop [max-num (apply max (map first colls))]
    (let [nums (map #(set (take-while (partial >= max-num) %)) colls)
          enough (apply clojure.set/intersection nums)]
      (if (not (empty? enough))
        (apply min enough)
        (recur (inc max-num))))))

;; 110
(fn seq-pronounces [nums]
  (let [group-to-prounce (fn [group]
                           (list (count group) (first group)))
        get-pronounce (fn [numbers]
                        (vec (mapcat
                              group-to-prounce
                              (partition-by identity numbers))))]
    (rest (iterate get-pronounce nums))))

;; 111
(fn is-answer? [word puzzle]
  (let [rows (map #(clojure.string/replace % #" " "") puzzle)
        lines-to-words (fn [lines]
                         (mapcat #(clojure.string/split % #"#") 
                                 lines))
        words (concat
               (lines-to-words rows)
               (lines-to-words (apply map str rows)))
        make-regex (fn [word]
                     (re-pattern (clojure.string/replace word #"_" ".")))
        regexs (map make-regex words)
        combine-matches (fn [acc regex]
                          (or
                           acc
                           (re-matches regex word)))]

    (not (not (reduce combine-matches false regexs)))))

;; 112
(fn sequs [n coll]
  (let [[fst & rst] coll
        count-n (fn [coll]
                  (reduce + (flatten coll)))]
    (if (not (sequential? fst))
      (if (or (nil? fst)
              (< n fst))
        ()
        (cons fst (sequs (- n fst) rst)))
      (let [sub-coll (sequs n fst)
            rem-n (- n (count-n sub-coll))]
        (if (>= rem-n 0)
          (cons sub-coll (sequs rem-n rst))
          sub-coll)))))

;; 113
(fn dance [& nums]
  (reify
    clojure.lang.Seqable
    (toString [_]
      (clojure.string/join ", " (sort nums)))
    (seq [_]
        (seq (distinct nums)))))

;; 114
(fn global-take-while [n p coll]
  (let [[fst & rst] coll
        n (if (p fst) (dec n) n)]
    (if (>= 0 n)
      ()
      (lazy-seq
       (cons fst (global-take-while n p rst))))))

;; 115
(fn is-balanced? [number]
  (let [dig-list (map #(read-string (str %)) (str number))
        half-dig-count (/ (count dig-list) 2)
        sum-half #(reduce + (take half-dig-count %))]
    (= (sum-half dig-list) (sum-half (reverse dig-list)))))

;; 116
(fn is-balanced? [num]
  "Using sieve code from clojure lazy-seq docs
http://clojuredocs.org/clojure_core/clojure.core/lazy-seq"
  (let [sieve (fn sieve [s]
                (cons (first s)
                      (lazy-seq (sieve (filter #(not= 0 (mod % (first s)))
                                               (rest s))))))
        primes (sieve (iterate inc 2))
        [lesses [bigger & mores]] (split-with #(>= num %) primes)
        [middle lesser & others] (reverse lesses)]
    (cond (not= middle num) false
          (or (nil? lesser) (nil? bigger)) false
          :else
          (= (/ (+ bigger lesser) 2) middle))))

;; 117
;; Seems like there should be a cleaner sol'n
(fn science [maze]
  (let [indexed-maze (vec
                      (map-indexed
                       (fn [y row]
                         (vec
                          (map-indexed
                           (fn [x elem]
                             [elem [y x]]) row)))
                       maze))
        update-maze (fn [rows]
                      (let [is-mouse? (fn [loc]
                                        (= \M (first (get-in rows loc))))
                            nearby? (fn [[x y]]
                                      (if (nil? x)
                                        false
                                        (some identity
                                              (map is-mouse?
                                                   (list
                                                    [(dec x) y]
                                                    [(inc x) y]
                                                    [x (inc y)]
                                                    [x (dec y)])))))
                            update-spot (fn [[symbol loc]]
                                          (let [new-sym (case symbol
                                                          \# \#
                                                          \M \M
                                                          (if (nearby? loc)
                                                            \M
                                                            symbol))]
                                            [new-sym loc]))]
                        (vec (map #(vec (map update-spot %)) rows))))
                      finished?  (fn [rows]
                                   (->>
                                    rows
                                    (apply concat)
                                    (map first)
                                    (some #{\C})
                                    not))
                      explore-maze (fn explore-maze [old-rows rows]
                                     (cond
                                      (finished? rows) true
                                      (= old-rows rows) false
                                      :else
                                      (explore-maze rows (update-maze rows))))]
        (explore-maze indexed-maze (update-maze indexed-maze))))

;; 118
(fn map' [f col]
  (lazy-seq
   (if (empty? col)
     col
     (cons (f (first col))
           (map' f (rest col))))))

;; 119
(fn win-tic-tac-toe [type board]
  "Needs to keep track of location"
  (let [indexed-board (map-indexed
                       (fn [y row]
                         (map-indexed
                          (fn [x elem]
                            [elem [y x]]) row))
                       board)

        winner        (fn [row]
                        "Returns list with location where type can win."
                        (let [piece-freqs (frequencies (map first row))]
                          ;; we need two pieces of type and one empty to win
                          (if (= [1 2]
                               (map #(piece-freqs %) [:e type]))
                            (map second (filter #(= :e (first %)) row))
                            ())))

        find-winners  (fn [rows]
                        (mapcat winner rows))
        swapped       (vec (apply map vector indexed-board))
        diag          (fn [rows]
                        (map nth rows (range)))]

    (set (concat (find-winners indexed-board)
                 (find-winners swapped)
                 (winner (diag indexed-board))
                 (winner (diag (reverse indexed-board)))))))

;; 120
(fn filter-sum-squares [col]
  (let [square #(* % %)
        add-square #(+ (square (read-string (str %2))) %1)
        sum-squares #(reduce add-square 0 (str %))
        greater-squares? #(< % (sum-squares %))]
    (count (filter greater-squares? col))))

;; 121
(fn compute [expr]
  (fn sub [name-vals]
    (let [sub-in-expr (fn [sub-expr]
                        (if (sequential? sub-expr)
                          ((compute sub-expr) name-vals)
                          (or (name-vals sub-expr) sub-expr)))]
      (eval (map sub-in-expr expr)))))

;; 122
(fn eval-binary [num]
  (let [powers (take (count num) (iterate #(* 2 %) 1))
        bins   (reverse (map #(read-string (str %)) num))
        bin-pows (map * powers bins)]
    (reduce + 0 bin-pows)))

;; 124
(fn reversi [board piece-type]
  (let [indexed-board
        (vec (map-indexed
              (fn [y row]
                (vec (map-indexed
                      (fn [x elem]
                        [elem [y x]]) row)))
              board))

        is-type
        (fn [elem]
          (= (first elem) piece-type))

        is-empty
        (fn [elem]
          (= (first elem) 'e))

        is-other
        (fn [elem]
          (= (first elem)
             (if (= piece-type 'w) 'b 'w)))

        moves
        (list
         [-1 -1]
         [1 1]
         [1 -1]
         [-1 1]
         [0 -1]
         [0 1]
         [1 0]
         [-1 0])

        make-move
        (partial map +)

        elems-of-type
        (mapcat #(filter is-type %) indexed-board)

        try-move
        (fn [move [val coord :as elem]]
          (if (empty? elem)
            ()
            (->>
             (make-move coord move)
             (get-in indexed-board))))

        repeat-move
        (fn [move elem]
          (->>
           elem
           (iterate (partial try-move move))
           rest
           (split-with is-other)))

        validate-move
        (fn [move elem]
          (let [[intermediates empties] (repeat-move move elem)]
            ;; A move is only valid if there are "other" pieces
            ;; between here and an empty square on the board
            (if (and (is-empty (first empties))
                     (is-other (first intermediates)))
              (let [destination (second (first empties))
                    flipped (set (map second intermediates))]
                [destination flipped])
              nil)))

        find-moves-from-spot
        (fn [elem]
          (map #(validate-move % elem) moves))

        all-valid-moves
        (->>
         (mapcat find-moves-from-spot elems-of-type)
         (remove nil?)
         (into {}))]

    all-valid-moves))

;; 125
(fn quine []
  "Boring way"
  (let [quo (char 34)
        spa (char 32)
        make-str (fn [st] (str quo st quo))
        join-em (fn [lins] (clojure.string/join spa lins))
        lines [
               "(fn quine []"
               "(let [quo (char 34)"
               "spa (char 32)"
               "make-str (fn [st] (str quo st quo))"
               "join-em (fn [lins] (clojure.string/join spa lins))"
               "lines ["
               "]]"
               "(apply str (map join-em (list (take 6 lines)"
               "(map make-str lines)"
               "(drop 6 lines))))))"
               ]]
    (apply str (map join-em (list (take 6 lines)
                                  (map make-str lines)
                                  (drop 6 lines))))))

;; 126
Class

;; 127
;; other files

;; 128
;; TODO

;; 130
(fn reparent [node tree]
  "It seems like it would be cleaner if I could combine merge-in
into move-up, but this works"
  (let [is-node? #(= node (first %))

        should-move? (fn should-move? [children]
                       (->>
                        children
                        (map is-node?)
                        (some true?)))

        move-up (fn move-up [[fst & rst]]
                  (let [new-top  (first (filter is-node? rst))
                        new-sub  (cons fst (remove is-node? rst))]
                    (conj (vec new-top) new-sub)))
        
        up-one (fn up-one [[fst & rst :as tree]]
                 (cond
                  (>= 1 (count tree)) tree
                  (should-move? rst) (move-up tree)
                  :else
                  (cons fst (map up-one rst))))

        merge-in (fn [part-tree]
                   (let [len (count part-tree)
                         [beg [pen lst]] (split-at (- len 2) part-tree)]
                   (concat
                    beg
                    (list (conj (vec pen) lst)))))]
                   
    (->>
     (iterate (comp merge-in up-one) (up-one tree))
     (drop-while #(not= (first %) node))
     first)))

;; 131
(fn common-set-sum [& sets]
  "Power counts is adapted from the earlier power sets exercise"
  (let [sum #(reduce + %)
        power-counts (fn [coll]
                       (loop [sets #{}
                              new-sets #{coll}]
                         (if (empty? new-sets)
                           (set (map sum (remove empty? sets)))
                           (let [get-subsets (fn [elem-set]
                                               (set (map #(disj elem-set %) elem-set)))
                                 subsets     (map get-subsets new-sets)
                                 new-subsets (apply clojure.set/union subsets)]
                             (recur (clojure.set/union sets new-sets) new-subsets)))))
        common-sums (apply clojure.set/intersection (map power-counts sets))]
    (not (empty? common-sums))))

;; 132
(fn insert-btwn [p v coll]
  "Boring lazy-seq creation"
  (if (empty? (rest coll))
    coll
    (lazy-seq
     (let [[fst & rst] coll]
       (if (p fst (first rst))
         (cons fst (cons v (insert-btwn p v rst)))
         (cons fst (insert-btwn p v rst)))))))

(fn insert-btwn [p v coll]
  "Almost much more elegant. The obvious implementation is one
element too short for non-infinite sequences. Needed a hacky fix..."
  (let [hacky-p #(and
                  (not (= :ending-val %2))
                  (p %1 %2))
        if-p-add-v #(if (hacky-p %1 %2)
                      (list %1 v)
                      (list %1))
        rst-colls (lazy-cat (rest coll) (list :ending-val))]
    (mapcat if-p-add-v coll rst-colls)))

;; 134
;; TODO

;; 135
(fn infix [& args]
  (if (= (count args) 1)
    (first args)
    (let [arg1     (first args)
          operator (second args)
          arg2     (nth args 2)
          others   (rest (rest (rest args)))]
      (recur (cons (operator arg1 arg2) others)))))

;; 137
(fn change-base [number base]
  (loop [digits ()
         num    number]
    (let [remainder (rem num base)
          times (/ (- num remainder) base)
          new-digits (conj digits remainder)]
      (if (= 0 times)
        new-digits
        (recur new-digits times)))))

;; 138
(defn square-square [start end]
  "Starts by making a string of the required nums/symbols, then process to find the locations to insert them on the board. This could be more efficient if I could generate each row in one swoop, but I think that would require some mathematical insight I haven't figured out."

  (let [square
        #(int (Math/pow %1 2))

        nums
        (->>
         start
         (iterate square)
         (take-while #(>= end %))
         (apply str))

        num-count
        (count nums)
        
        side-size
        (->>
         num-count
         Math/sqrt
         Math/ceil
         int)

        num-rows
        (dec (* 2 side-size))

        padded-count
        (square side-size)

        padded-nums
        (->>
         (repeat (- padded-count num-count) "*")
         (apply str)
         (str nums))

        mid-point
        (int (/ num-rows 2))

        ;; if sqrt n is odd, the first num starts in the very middle.
        ;; else, it starts in middle of row just above the middle row.
        starting-pos
        (if (odd? side-size)
          [mid-point mid-point]
          [(dec mid-point) mid-point])

        blank-board
        (vec (repeat num-rows (vec (repeat num-rows " "))))

        update-spot
        (fn [pos val] 
          (fn [board]
            (update-in board pos (fn [x] val))))

        move-stream
        (->>
         [[1 1]
          [1 -1]
          [-1 -1]
          [-1 1]]
         repeat
         (apply concat))

        ;; Once we've gone along half of the diamond, i.e. around 2 sides,
        ;; it's bigger and we have to go one move farther for next 2 sides.
        times-to-repeat-moves
        (mapcat #(list % %) (next (range)))
        
        all-moves
        (mapcat
         (fn [index mv] (repeat index mv))
         times-to-repeat-moves
         move-stream)

        make-move
        (partial map +)

        position-stream
        (reductions make-move starting-pos all-moves)

        all-updates
        (map update-spot position-stream padded-nums)]

    (->>
     all-updates
     (reduce #(%2 %1) blank-board)
     (map #(apply str %))
     vec)))

;; 140
;; TODO

;; 141
(fn winning-card [trump]
  (fn best-card [cards]
    (let [filter-suit (fn [suit]
                        (filter #(= suit (% :suit)) cards))
          get-best (fn [suit-cards]
                     "Expects all cards to be the same suit"
                     (apply max-key #(% :rank) suit-cards))
          trumps (filter-suit trump)]
      (if (not (empty? trumps))
        (get-best trumps)
        (get-best (filter-suit ((first cards) :suit)))))))

;; 143
(fn dot-product [v1 v2]
  (reduce + (map * v1 v2)))

;; 144
(fn oscilrate [n & funs]
  (let [num-funs (count funs)]
    ((fn helper [n i]
       (let [f (nth funs i)
             new-i (mod (inc i) num-funs)]
         (lazy-seq
          (cons n (helper (f n) new-i)))))
     n 0)))

;; 145
;; TODO

;; 146
(fn tree-tables [tree]
  (apply hash-map
        (apply concat
         (for [key1 (keys tree)
               key2 (keys (tree key1))]
           [[key1 key2] ((tree key1) key2)]))))

;; 147
(fn lazy-pascal [col]
  (let [next-pascal #(vec (map +' (cons 0 %) (concat % [0])))]
    (iterate next-pascal col)))

;; 148
(fn big-divide-slow [n a b]
  "too slow"
  (let [get-nums (fn [x] 
                   (take-while #(> n %) (iterate #(+ x %) 0)))
        as (set (get-nums a))
        bs (set (get-nums b))
        all (clojure.set/union as bs)
        num-as (dec (count as))
        sum-as (reduce + as)
        avg-as (/ sum-as num-as)]
    (reduce + all)))

(fn big-divide [n a b]
  "Just uses arithmetic. Always uses bigints,
which makes it a little slower than necessary for small inputs.
Still way better than any range/iterate based approach."
  (let [sum-num (fn [x]
                  (let [times (bigint (/ (dec n) x))]
                    (* times (/ (* (inc times) x) 2))))
        a-sum (sum-num a)
        b-sum (sum-num b)
        ab-sum (sum-num (* a b))]
    (- (+ a-sum b-sum) ab-sum)))
        
;; 150
;; palindromic numbers - I made a few different versions to try to optimize perf,
;; because things were timing out on the site even though locally they were really fast.
;; I found out there's some sort of java interop penalty on the site, but kept the different versions.
(fn make-palindromes [num]
  "Naive, slow version for comparison."
  (let [is-palindrome? (fn [x]
                        (= (seq (str x)) (reverse (str x))))
        greater-nums  (iterate inc num)]
    (filter is-palindrome? greater-nums)))
        
(fn make-palindromes [num]
  "This is fast on my machine, but not quite good enough for the text.
Need to figure out how smartly handle 9s."
  (let [toInt #(read-string (str %))
        is-palindrome? (fn [x]
                         (= (seq (str x)) (reverse (str x))))
        first-pal (fn [x]
                    (first (filter is-palindrome? (iterate inc x))))
        inc-nines (fn inc-nines [[x & xs]]
                    (cond (nil? x) (list 1)
                          (= 9 x) (cons 0 (inc-nines xs))
                          :else (cons (inc x) xs)))
        next-pal (fn [digs]
                   "Assumes a palindrome as input"
                   (let [cnt  (count digs)
                         middle (int (/ cnt 2))
                         mid-num (nth digs middle)]
                     (if (= 9 mid-num)
                       (if (every? #(= 9 %) digs)
                         (vec (concat [1] (repeat (dec cnt) 0) [1]))
                         (let [inced (inc-nines (drop middle digs))
                               rev-inced (reverse inced)
                               rev-inced (if (every? #(= 9 %) digs)
                                           (drop-last rev-inced)
                                           rev-inced)
                               new-digs (concat
                                         rev-inced
                                         (if (even? cnt)
                                           inced
                                           (rest inced)))]
                           (vec new-digs)))
                       (let [inced-mid (inc mid-num)
                             new-digs (if (even? cnt)
                                        (assoc digs middle inced-mid
                                               (dec middle) inced-mid)
                                        (assoc digs middle inced-mid))]
                         (vec new-digs)))))]
    (map #(read-string (apply str %)) (iterate next-pal (vec (map toInt (str (first-pal num))))))))


(fn make-palindromes [num]
  "This is fast on my machine, but not quite good enough for the text.
Need to figure out how smartly handle 9s."
  (let [is-palindrome? (fn [x]
                         (= (seq (str x)) (reverse (str x))))
        first-pal (fn [x]
                    (first (filter is-palindrome? (iterate inc x))))
        inc-nines (fn inc-nines [[x & xs]]
                    (cond (nil? x) (list 1)
                          (= 9 x) (cons 0 (inc-nines xs))
                          :else (cons (inc x) xs)))
        next-pal (fn [pal]
                   "Assumes a palindrome as input"
                   (let [toInt #(read-string (str %))
                         digs (vec (str pal))
                         cnt  (count digs)
                         middle (int (/ cnt 2))
                         mid-num (toInt (str (nth digs middle)))]
                     (if (= 9 mid-num)
                       (if (every? #(= "9" (str %)) digs)
                         (read-string
                          (apply str
                           (concat [1] (repeat (dec cnt) 0) [1])))
                         (let [inced (inc-nines (map toInt (drop middle digs)))
                               rev-inced (reverse inced)
                               rev-inced (if (every? #(= 9 %) digs)

                                           rev-inced)
                               new-digs (concat
                                         rev-inced
                                         (if (even? cnt)
                                           inced
                                           (rest inced)))
                               pal-str (apply str new-digs)]
                           (read-string pal-str)))
                       (let [inced-mid (str (inc mid-num))
                             new-digs (if (even? cnt)
                                        (assoc digs middle inced-mid
                                               (dec middle) inced-mid)
                                        (assoc digs middle inced-mid))
                             pal-str (apply str new-digs)]
                         (read-string pal-str)))))]
    (iterate next-pal (first-pal num))))


(fn make-palindromes [num]
  "This passes all test cases in under a second on my machine,
but times out on 4clojure.com. IDK why."
  (let [toInt #(read-string (str %))
        is-palindrome? (fn [x]
                         (= (seq (str x)) (reverse (str x))))
        first-pal (fn [x]
                    (first (filter is-palindrome? (iterate inc x))))
        inc-nines (fn inc-nines [[x & xs]]
                    (let [x (int x)]
                    (cond (nil? x) (list 1)
                          (= 9 x) (cons 0 (inc-nines xs))
                          :else (cons (inc x) xs))))
        next-pal (fn [digs]
                   "Assumes a palindrome as input"
                   (let [cnt  (int (count digs))
                         middle (int (/ cnt 2))
                         mid-num (int (nth digs middle))]
                       (if (every? #(= 9 %) digs)
                         (vec (concat [1] (repeat (dec cnt) 0) [1]))
                         (let [inced (inc-nines (drop middle digs))
                               rev-inced (reverse inced)
                               rev-inced (if (every? #(= 9 %) digs)
                                           (drop-last rev-inced)
                                           rev-inced)
                               new-digs (concat
                                         rev-inced
                                         (if (even? cnt)
                                           inced
                                           (rest inced)))]
                           (vec new-digs)))))]
    (map #(read-string (apply str %)) (iterate next-pal (vec (map toInt (str (first-pal num))))))))

;; 152
;; TODO

;; 153
(fn pair-disj-sets [set-set]
  (if (empty? set-set)
    true
    (let [fst-set (first set-set)
          rest-set (rest set-set)
          has-common #(> (count (clojure.set/intersection %1 %2)) 0)
          has-common-acc #(or %1 (has-common fst-set %2))
          any-eq-set (reduce has-common-acc false rest-set)]
      (if any-eq-set
        false
        (recur rest-set)))))

;; 156
;; TODO

;; 157
(fn index-seq [col]
  (map #(list %1 %2) col (range)))

;; 158
(fn decurry [fun]
  (fn apply-args [& args]
    (reduce #(%1 %2) fun args)))

;; 161
;; TODO

;; 162
;; TODO

;; 164
;; TODO

;; 166
;; TODO

;; 168
(fn infinite-matrix [f & args]
  (let [rang     (fn rang [x]
                   (lazy-seq
                    (cons x (rang (inc x)))))
        [m n s t] args
        m        (or m 0)
        n        (or n 0)
        make-row (fn make-row [i]
                   (map #(f i %) (rang n)))
        table    (map make-row (rang m))]
    (if (nil? s)
      table
      (take s (map (partial take t) table)))))

;; 171
(fn intervals [coll]
  (if (empty? coll)
    []
    (loop [sorted-coll (sort (set coll))
           ranges []
           beg-range (apply min coll)
           last-num (apply min coll)]
      (if (empty? sorted-coll)
        (conj ranges [beg-range last-num])
        (let [[fst & rst] sorted-coll]
          (if (>= (inc last-num) fst) (recur rst ranges beg-range fst)
              (recur rst (conj ranges [beg-range last-num]) fst fst)))))))

;; 173
a c

;; 177
;; TODO

;; 178
;; TODO
