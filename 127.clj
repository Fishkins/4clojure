;; Love Triangle
;; Start looking for squares = to max length and look for triangles, then work down.
(declare __)
(declare parse-board)
(declare biggest-triangle)
(declare triangle-of-size?)
(declare get-sub-board)
(declare triangle-size)
(declare bottom-left-triangle-size)
(declare top-center-triangle-size)
(declare transpose)
(defn __ [rocks]
  (let [board (parse-board rocks)]
    (biggest-triangle board)))

(defn parse-board [rocks]
    (let [to-bits
        (fn to-bits [dec-num]
          (loop [cur-num dec-num
                 bits    ()]
            (if (= 0 cur-num)
              bits
              (recur (int (/ cur-num 2)) (cons (rem cur-num 2) bits)))))

        uneven-rocks
        (map to-bits rocks)

        max-len
        (apply max (map count uneven-rocks))

        pad-bin
        (fn [bin]
          (->
           (- max-len (count bin))
           (repeat 0)
           (concat bin)))]

      (vec (map (comp vec pad-bin) uneven-rocks))))

(defn biggest-triangle [board]
  (loop [len (min (count (board 0)) (count board))]
    (let [size (triangle-of-size? board len)]
      (cond (> size 0) size
            (>= 2 len) nil
            :else (recur (dec len))))))

(defn triangle-of-size? [board side]
  (let [sizes (for [y-off (range (inc (- (count board) side)))
                    x-off (range (inc (- (count (board 0)) side)))]
                (triangle-size (get-sub-board board x-off y-off side)))]
    (if (empty? sizes)
      0
      (apply max sizes))))

(defn get-sub-board [board x-off y-off side]
  (vec (map #(vec (take side (drop x-off %)))
            (take side (drop y-off board)))))

(defn triangle-size [board-piece]
  (let [rotations [
                   identity
                   (comp vec reverse)
                   #(map (comp vec reverse) %)
                   (comp vec reverse #(map (comp vec reverse) %))
                   ]
        orientations (for [orientation rotations]
                       (vec (orientation board-piece)))]
    (apply max (map bottom-left-triangle-size orientations))))

(defn bottom-left-triangle-size [board-piece]
  (let [len (count board-piece)
        triangle-parts (for [x (range len)
                             y (range len)
                             :when (<= (+ x y) (dec len))]
                         (get-in board-piece [x y]))]
    (if (every? #(= 1 %) triangle-parts)
      (count triangle-parts)
      0)))

(defn top-center-triangle-size [board-piece]
  (let [height (count board-piece)
        width  (count (board-piece 0))
        subrow #(take (- width (* 2 %)) (drop % (board-piece %)))
        triangle-parts (mapcat subrow (range height))]
    
    (if (every? #(= 1 %) triangle-parts)
      (count triangle-parts)
      0)))

(defn transpose [board]
  (vec (apply map (comp vec list) board)))


;; Tests

(= 9 (__ [18 7 14 14 6 3]))
(= 4 (__ [7 3]))
(= nil (__ [21 10 21 10]))
(= nil (__ [0 31 0 31 0]))
(= 6 (__ [17 22 6 14 22]))
(= 3 (__ [3 3]))
(= 15 (__ [1 3 7 15 31]))
(= 10 (__ [15 15 15 15 15]))
