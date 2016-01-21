(ns playground.core
  (:gen-class)
  (require [clojure.set :refer :all]))

(defn -main
  [this]
  (println "Hello, World!"))

(defn -toString
  [this]
  "Hello, toString")

(defn -hashCode
  [this] 1)

(defn x [key map]
  (and (contains? map key ) (nil? (key map))))


(defn m [x & xs]
  (reduce #(if (> % %2) % %2) x xs))

(defn longest-inc-subseq [coll]
  (reduce #(let [len-xs (count %)
                 len-x (count %2)]
             (if (and (< len-xs len-x) (< 1 len-x)) %2 %))
          []
          (reductions (fn [xs x]
                        (if (> x (last xs)) (conj xs x) [x])) (cons [(first coll)] (rest coll)))))
;; partition
(defn part [n coll]
  (filter #(= n (count %))
          (reductions (fn [xs s]
                        (if (= (count xs) n) [s] (conj xs s))) [(first coll)] (rest coll))))

;;Count Occurrences
(defn frequencies-1 [coll]
  (reduce (fn [map k]
            (if (not (contains? map k))
              (assoc map k 1)
              (assoc map k (inc (map k)))))
          {} coll))

;;; interleave
(defn interleave-1 [c1 c2]
  (let [s1 (seq c1) s2 (seq c2)]
    (when (and s1 s2)
      (cons (first s1) (cons (first s2) (interleave-1 (rest s1) (rest s2)))))))

;;; interpose
(defn interpose-1 [val coll]
  (reduce (fn [xs x]
            (conj xs val x)) [(first coll)] (rest coll)))

;;; Drop Every Nth Item
(defn drop-every-nth-item [coll nth]
  (filter identity
       (reduce-kv (fn [vs k v]
                    (if (= 0 (mod (inc k) nth))
                      (conj vs nil)
                      (conj vs v))) [] coll)))

;;; Factorial Function
(defn factorial [n]
  (apply * (map inc (range n))))

;;; Reverse interleave
(defn reverse-interleave [coll num]
  (map second (reduce-kv (fn [ret ind v]
                           (let [k (mod ind num)]
                             (assoc ret k (conj (get ret k []) v))))
                         {}
                         (into [] coll))))

;;; Range
(defn range-1 [s e]
  (when (< s e)
    (cons s (range-1 (inc s) e))))

;;; Rotate
(defn rotate [num coll]
  (let [ind (mod num (count coll))]
    (concat (drop ind coll) (take ind coll))))

;;; split-at
(defn split-at-1 [num coll]
  [(take num coll) (drop num coll)])


;;; split by type
(defn split-by-type [coll]
  (into #{} (map second (group-by type coll))))

;;; remove distinct items
(defn find-distinct-items [coll]
  (reduce (fn [xs x]
            (if ((set xs) x) xs
                (conj xs x))) [] coll))

;;; Function Composition
(defn func-comp [& funcs]
  (fn
    ([coll]
     (loop [rfuncs (reverse funcs) c1 coll]

       (if (seq rfuncs)
         (recur (rest rfuncs) ((first rfuncs) c1))
         c1)))
    ([x & args]
     (prn args)
     (let [v (apply (last funcs) x args)]
       (loop [rfuncs (rest (reverse funcs)) v1 v]
         (if (seq rfuncs)
           (recur (rest rfuncs) ((first rfuncs) v1))
           v1))))))

;;; Juxtaposition
(defn juxtaposition [& funcs]
  (fn [x & args]
    (map (fn [f]
           (apply f x args)) funcs)))

;;; Sequence reductions
(defn seq-reductions
  ([f coll]
   (seq-reductions f (first coll) (rest coll)))
  
  ([f init coll]
   (cons init
         (lazy-seq
          (when-let [s (seq coll)]
            (seq-reductions f (f init (first s)) (rest s)))))))

;;; zipmap
(defn zipmap-1 [c1 c2]
  (apply merge (map (fn [x x1]
                      {x x1}) c1 c2)))

;;; iterate
(defn iterate-1 [f init]
  (cons init
        (lazy-seq
         (iterate-1 f (f init)))))

;;; group-by
(defn group-by-1 [f coll]
  (reduce
   (fn [ret x]
     (let [k (f x)]
       (assoc ret k (conj (get ret k []) x))))
   {}
   coll))

;;; black box testing
(defn black-box-testing [s]
  (cond
    (= (conj s {}) s) :map
    (empty? s) (if (= (clojure.set/union s #{}) #{})
                 :set
                 (first (conj s :vector :list)))
    (= (clojure.set/union s s) s) :set
    (= (first (conj s s)) s) :list
    :else :vector))

;;; Flatten a Sequence
(defn flatten-1 [coll]
  (reduce (fn [xs x]
            (if (sequential? x)
              (apply conj xs (flatten-1 x))
              (conj xs x)))
          [] coll))


;;; Pack a Sequence
(defn pack-a-seq [coll]
  (partition-by identity coll))

;;; Duplicate a Sequence
(defn duplicate-a-seq [coll]
  (apply concat
         (map #(list % %) coll))) 

;;; Replicate a Sequence
(defn replicate-a-seq [coll n]
  (apply concat
         (map #(repeat n %) coll)))
 
;;; Word Sorting
(defn sort-word [s]
  (sort-by #(.toUpperCase %) (re-seq #"\w+" s)))

;;; dot product
(defn dot-product [c1 c2]
  (reduce + (map * c1 c2)))

;;; Oscilrate
(defn oscilrate [init & fs]
  (reductions (fn [v f]
                (f v))
              init (cycle fs)))

;;; Trees into tables
(defn trees-into-table [m]
  (apply merge
         (map
          (fn [[key values]]
            (reduce
             (fn [m1 [k v]]
               (merge m1 {[key k] v})) {} values)) m)))

;;; Pascal's Trapezoid
#_(defn pascal-trapezoid [coll]
  )


;;; Simple closures
(defn simple-closures [n]
  (fn [x]
    (reduce * (repeat n x))))

;;; Lazy Searching
(defn smallest-common-number [& seqs]
  (letfn [
          (max-first [seqs]
            (apply max (map first seqs)))
          
          (first-equals? [seqs]
            (apply = (map first seqs)))
          ;; filter collection with these elements less than v
          (reduce-seqs [v coll]
            (if (> v (first coll))
              (reduce-seqs v (rest coll))
              coll))]
    
    (let [curr (max-first seqs)]
      (if (first-equals? seqs)
        curr
        (apply smallest-common-number (map #(reduce-seqs curr %) seqs)))))) 

;;; Set Intersection
(defn intersection-1 [s1 s2]
  (reduce
   (fn [xs x]
     (if (contains? s1 x)
       (conj xs x)
       xs)) #{} s2))

;;; Half-truth
(defn half-truth [& coll]
  (not (or (every? false? coll) (every? true? coll))))


;;; Map Defaults
(defn map-defaults [v coll]
  (reduce #(assoc % %2 v) {} coll))

;;; reverse sequences
(defn reverse-seq [coll]
  (when-let [elem (first coll)]
    (concat (reverse-seq (rest coll)) [elem])))

;;; fabonacci seq
(defn fab [n]
  (take n
        (map first (iterate (fn [[a b]] [b (+ a b)])
                            [1 1]))))

;;; Symmetric Difference
(defn symmetric-difference [s1 s2]
  (let [i (clojure.set/intersection s1 s2) u (clojure.set/union s1 s2)]
    (into #{} (filter #(not (contains? i %)) u))))

;;; to be tree
(defn tree? [tr]
  (cond
    (nil? tr) true
    (and (sequential? tr) (= 3 (count tr))) (and (tree? (second tr)) (tree? (second (rest tr))))
    :else false))

;;; symmetric 
(defn symmetric-tree? [tr]
  ())

;;; read a binary number
(defn read-binary-number [s]
  (reduce-kv (fn [sum ind x]
               (+ sum (apply * x (repeat ind 2)))) 0 (into [] (reverse (map read-string (re-seq #"\d" s))))))
;;; Product Digits
(defn product-digits [n1 n2]
  (map #(- (int %) 48) (.toString (* n1 n2))))

;;; least common multiple
(defn least-common-multiple [& nums]
  (/ (apply * nums)
     (reduce #(if (= 0 %2)
                %
                (recur %2 (mod % %2))) nums)))

;;; prime numbers
(defn prime-numbers [n]
  (letfn
      [(prime? [n]
         (not-any? #(= 0 (mod n %)) (range 2 n)))]
    (take n
          (filter prime? (drop 2 (range))))))
