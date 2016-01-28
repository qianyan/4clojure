(ns playground.core
  (require [clojure.set :refer :all]))

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
  (letfn
      [(before-root-travel [tr]
         (if (not (coll? tr)) [tr]
             (concat
              (before-root-travel (second tr))
              [(first tr)]
              (before-root-travel (last tr)))))
       
       (after-root-travel [tr]
         (if (not (coll? tr)) [tr]
             (concat
              (after-root-travel (last tr))
              [(first tr)]
              (after-root-travel (second tr)))))]
    (or
     (= 1 (count tr))
     (= (before-root-travel (second tr)) (after-root-travel (last tr))))))

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
;;; greatest common divisor
(defn gcd [& nums]
  (reduce #(if (= 0 %2)
                %
                (recur %2 (mod % %2))) nums))

;;; prime numbers
(defn prime-numbers [n]
  (letfn
      [(prime? [n]
         (not-any? #(= 0 (mod n %)) (range 2 n)))]
    (take n
          (filter prime? (drop 2 (range))))))

;;; power set
(defn power-set [s]
  (let [first (first s) subset (rest s)]
    (if (empty? s) #{#{}}
        (clojure.set/union
         (into #{} (map #(clojure.set/union #{first} %) (power-set subset)))
         (power-set subset)))))


;;; happy numbers
(defn happy-number? [n]
  (letfn
      [(square [n]
         (*' n n))
       (discompose [d]
         (map #(Character/digit % 10) (str d)))]
    (loop [num n visited #{}]
      (cond
        (visited num) false
        (= 1 num) true
        :else (recur (apply + (map square (discompose num))) (conj visited num))))))

;;; comparison
(defn comparison [op x y]
  (cond
    (op x y) :lt
    (op y x) :gt
    :else :eq))

;;; indexing sequences
(defn index-seq [coll]
  (reduce (fn [xs x]
            (conj xs [x (count xs)])) [] coll))

;;; decurried
(defn decurried [f]
  (fn [& nums]
    (loop [f1 f nums1 nums]
      (if (empty? nums1)
        f1
        (recur (f1 (first nums1)) (rest nums1))))))

;;; map implementation
(defn re-map [f coll]
  (lazy-seq
   (when-let [s (seq coll)]
     (cons (f (first coll))
           (re-map f (rest coll))))))


;;; Sum of square of digits
(defn sum-of-square-of-digits [coll]
  (count
   (filter (fn [x]
             (< x (apply + (map #(let [d (Character/digit % 10)]
                                   (*' d d)) (str x)))))
           coll)))

;;; Cartesian Product
(defn cartesian-product [s1 s2]
  (into #{} (for [x1 s1 x2 s2]
              [x1 x2])))

;;; infix calculator
(defn infix-calculator [& operands]
  (let [pairs (partition 2 operands)
        numbers (conj (into [] (map first pairs)) (last operands))
        operations (into [] (map second pairs))]
    
    (loop [ops operations nums numbers]
      (if (empty? ops)
        (first nums)
        (recur (rest ops) (cons
                           ((first ops) (first nums) (second nums))
                           (nthrest nums 2)))))))

;;; perfect squares
(defn filter-perfect-squares [s]
  (let [perfect-squares-sets (into #{} (take 100 (map #(* % %) (range))))]
    (clojure.string/join "," (filter #(perfect-squares-sets %) (map read-string (re-seq #"\d+" s))))))

;;; perfect numbers
(defn perfect-numbers [n]
  (= n (apply + (filter #(= 0 (mod n %)) (range 1 n)))))

;;; Identify keys and values
(defn identify-keys-and-values [coll]
  (->>
   (reduce #(if (and (keyword? (last %))
                     (keyword? %2))
              (conj % nil %2)
              (conj % %2)) [] coll)
   (partition-by keyword?)
   (map #(cond
           (keyword? (first %)) (first %)
           (nil? (first %)) []
           :else (into [] %)))
   (apply hash-map)))

;;; Roman numberals
(defn read-roman-numberals [s]
  (let [map-of-roman-numberals {:I 1 :V 5 :X 10 :L 50
                                :C 100 :D 500 :M 1000}
        numberals-seq (map #(map-of-roman-numberals (keyword (str %))) s)]
    (second (reduce (fn [[pre result] x]
               [x (+ result
                     (if (< pre x)
                       (- x (* 2 pre))
                       x))])
                    (repeat 2 (first numberals-seq)) (rest numberals-seq)))))

;;; intoCamelCase
(defn into-camel-case [s]
  (let [xs (re-seq #"\w+" s)]
    (apply str (first xs) (map #(apply str (Character/toUpperCase (first %)) (rest %)) (rest xs)))))

;;; generating k-combinations
(defn generate-k-combinations [k coll]
  (letfn [(power-set [s]
            (let [first (first s) subset (rest s)]
              (if (empty? s) #{#{}}
                  (clojure.set/union
                   (into #{} (map #(clojure.set/union #{first} %) (power-set subset)))
                   (power-set subset)))))]
    (set (filter #(= k (count %)) (power-set coll)))))
 
;;; partially flatten a sequence
(defn partially-flatten-a-sequence [coll]
  (cond
    (nil? (seq coll)) coll
    (not (coll? (ffirst coll))) (cons (first coll) (partially-flatten-a-sequence (rest coll)))
    :else (concat (partially-flatten-a-sequence (first coll)) (partially-flatten-a-sequence (rest coll)))))

;;; Pascal's trangle
(defn pascal-trangle [n]
  (letfn [(factorial [n]
            (if (= n 0) 1
                (reduce * (range 1 (inc n)))))
          (binomial-coefficient [n k]
            (/ (factorial n)
               (* (factorial k) (factorial (- n k)))))]
    (map #(binomial-coefficient (dec n) %) (range n))))

;;; Pascal's Trapezoid
(defn pascal-trapezoid [coll]
  (iterate (fn [c]
             (reduce (fn [[xs p] x]
                       (let [result (conj xs (+' p x))]
                         (if (= (count result)
                                (count c))
                           (conj result x)
                           [result x]))) [[] 0] c)) coll))

;;; The Balance of N
(defn the-balance-of [n]
  (let [string-of-n (str n)
        reverse-string (reverse string-of-n)
        halve (int (/ (count string-of-n) 2))]
    (= (reduce + (map #(Character/digit % 10) (first (partition halve string-of-n))))
       (reduce + (map #(Character/digit % 10) (first (partition halve reverse-string)))))))

;;; Prime Sandwich
(defn prime-sandwich [n]
  (letfn
      [(prime []
         (->> Long/MAX_VALUE
              (range 2)
              ((fn step [coll]
                 (let [head (first coll)]
                   (lazy-seq (cons head (step (filter #(pos? (mod % head)) coll)))))))))
       
       (balanced-prime []
         (map second (filter #(= (second %) (/ (+ (first %) (last %)) 2))
                             (partition 3 1 (prime)))))]
    (->> (balanced-prime)
         (take-while #(<= % n))
         last
         (= n))))

;;; ANAGRAM Finder
(defn anagram-finder [coll]
  (let [angarams (map (fn [x]
                        [(sort x) x]) coll)
        values (vals (group-by #(first %) angarams))]
    (into #{} (filter #(< 1 (count %)) (map (fn [group]
                                              (into #{} (map second group))) values)))))

;;; digits and bases
(defn digits-and-bases [num base]
  (if (= 0 num) '(0)
      (loop [n num result '()]
        (if (not= 0 n)
          (recur (int (/ n base)) (cons (mod n base) result))
          result))))

;;; Merge with a Function
(defn merge-with-function [f & maps]
  (reduce (fn [ms m]
            (apply merge ms
                   (reduce (fn [es e]
                             (let [k (key e)
                                   v (val e)]
                               (if (contains? ms k)
                                 (assoc es k (f (get ms k) v))
                                 (assoc es k v)))) {} m))) {} maps))

;;; Analyze a Tic-Tac-Toe Board
(defn analyze-a-tic-tac-toe-board [board]
  (letfn [(win? [who]
            (some true?
                  (for [x (range 3)
                        y (range 3)
                        [dx dy] [[1 0] [0 1] [1 1] [1 -1]]]
                    (every? true? (for [i (range 3)]
                                    (= (get-in board [(+ (* dx i) x)
                                                      (+ (* dy i) y)])
                                       who))))))]
    (cond (win? :x) :x
          (win? :o) :o
          :else nil)))

;;; Euler's Totient Function
(defn euler-totient [num]
  (letfn [(gcd [x y]
            (loop [n x m y]
              (if (= 0 n)
                m
                (recur (mod m n) n))))]
    (count (filter (fn [[x y]]
                     (= 1 (gcd x y))) (map (fn [n]
                                             [n num]) (range num))))))
