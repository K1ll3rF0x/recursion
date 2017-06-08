(ns recursion
  (:require [clojure.set :as set]))

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (and (not (empty? coll))
       (empty? (rest coll))))

(defn my-last [coll]
  (if (singleton? coll)
    (first coll)
    (if (empty? coll)
      nil
      (my-last (rest coll)))))

(defn max-element [a-seq]
  (if (empty? a-seq)
    nil
    (if (singleton? a-seq)
      (first a-seq)
      (max (first a-seq)
           (max-element (next a-seq))))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
    (if (singleton? a-seq)
      (first a-seq)
      (seq-max (first a-seq)
               (longest-sequence (rest a-seq))))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (let [fst (first a-seq)
          rst (my-filter pred? (rest a-seq))]
      (if (pred? fst)
        (cons fst rst)
        rst))))

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq) false
    (not= (first a-seq) elem) (sequence-contains? elem (rest a-seq))
    :else true))

(defn my-take-while [pred? a-seq]
  (cond
    (empty? a-seq) a-seq
    (pred? (first a-seq)) (cons
                            (first a-seq)
                            (my-take-while pred? (rest a-seq)))
    :else '()))

(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq) a-seq
    (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
    :else (cons
            (first a-seq)
            (my-drop-while (fn [n] false) (rest a-seq)))))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (or (empty? a-seq) (empty? b-seq)) false
    (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
    :else false))

(defn my-map [f seq-1 seq-2]
  (cond
    (or (empty? seq-1) (empty? seq-2)) '()
    (and (empty? seq-1) (empty? seq-2)) '()
    :else (cons (f (first seq-1) (first seq-2))
                (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (if (<= n 1)
    n
    (+ (fib (- n 1)) (fib (- n 2)))))


(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    '()
    (cons what-to-repeat
          (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (<= up-to 0)
    '()
    (cons (- up-to 1)
          (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    '(())
    (cons (seq a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (reverse (map reverse (tails (reverse a-seq)))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    '(())
    (rest (map concat (tails a-seq) (inits a-seq)))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [fst (first a-seq)
          old-val (or (freqs fst) 0)
          freqs (assoc freqs fst (+ old-val 1))]
      (my-frequencies-helper freqs (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    '()
    (let [fst (first a-map)]
      (concat (repeat (second fst) (first fst))
              (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (if (or (<= n 0) (empty? coll))
    '()
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (reverse (my-take (- (count coll) n) (reverse coll))))

(defn halve [a-seq]
  (if (empty? a-seq)
    [() ()]
    (let [half-size (int (/ (count a-seq) 2))]
      [(my-take half-size a-seq) (my-drop half-size a-seq)])))

(defn seq-merge [a-seq b-seq]
  (cond
    (empty? a-seq) b-seq
    (empty? b-seq) a-seq
    (< (first a-seq) (first b-seq)) (cons (first a-seq)
                                          (seq-merge (rest a-seq) b-seq))
    :else (cons (first b-seq)
                (seq-merge a-seq (rest b-seq)))))

(defn merge-sort [a-seq]
  (if (or (empty? a-seq) (singleton? a-seq))
    a-seq
    (let [[x y] (halve a-seq)]
      (seq-merge (merge-sort x) (merge-sort y)))))

(defn split-into-monotonics [a-seq]
  (if (empty? a-seq)
    a-seq
    (let [monotonic? (fn [a-seq] (or (apply >= a-seq) (apply <= a-seq)))
          seq-inits (rest (inits a-seq))
          longest-monotonic (last (my-take-while monotonic? seq-inits))
          next-seq (drop (count longest-monotonic) a-seq)]
      (cons longest-monotonic (split-into-monotonics next-seq)))))

(defn permutations [a-set]
  (if (empty? a-set)
    '(())
    ; Note: Mapcat is short for (apply concat (map fn seq))
    (mapcat
      (fn [x]
        ; Create permutations for each rotation by repeating the first
        ; number and rotating the rest recursively
        (map cons (repeat (first x)) (permutations (rest x))))
      (rotations a-set))))

(defn powerset [a-set]
  [:-])

