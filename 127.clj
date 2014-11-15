;; Love Triangle

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

(defn all-sub-boards [board]
  (let [height (count board)
        width  (count (board 0))]
    (for [
          y-side (range 2 (inc height))
          x-side (range 2 (inc width))
          y-off  (range (inc (- height y-side)))
          x-off  (range (inc (- width x-side)))
          ]
      (vec (map #(vec (take x-side (drop x-off %1)))
                (take y-side (drop y-off board)))))))

(defn biggest-triangle [rocks]
  (let [
        board (parse-board rocks)

        transpose
        #(vec (apply map (comp vec list) %))

        basic-rotations [
                         identity
                         (comp vec reverse)
                         #(map (comp vec reverse) %)
                         (comp vec reverse #(map (comp vec reverse) %))
                         ]

        rotations
        (concat basic-rotations (map #(comp transpose %) basic-rotations))

        orientations
        (for [rotation rotations]
          (vec (rotation board)))

        pieces
        (mapcat all-sub-boards orientations)

        corner-sizes
        (for [piece pieces
              ;; only consider squares
              :when (= (count piece) (count (piece 0)))
              ]
          (bottom-left-triangle-size piece))

        side-sizes
        (for [piece pieces
              ;; sides need the following ratio to form a triangle
              :when (= (- (* (count piece) 2) 1) (count (piece 0)))
              ]
          (top-center-triangle-size piece))
        
        sizes
        (concat side-sizes corner-sizes)

        max-size
        (apply max sizes)
        ]
    (if (= 0 max-size)
      nil
      max-size))
  )

;; Tests

(defn __ [rocks]
  (biggest-triangle rocks))

;; (= 9 (__ [18 7 14 14 6 3]))
;; (= 4 (__ [7 3]))
;; (= nil (__ [21 10 21 10]))
;; (= nil (__ [0 31 0 31 0]))
;; (= 6 (__ [17 22 6 14 22]))
;; (= 3 (__ [3 3]))
;; (= 15 (__ [1 3 7 15 31]))
;; (= 10 (__ [15 15 15 15 15]))
