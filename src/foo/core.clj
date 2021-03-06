(ns foo.core
  (:require [clojure.spec :as s]
            [clojure.spec.test :as st]
            [clojure.spec.gen :as gen]))

(defn ranged-rand
  [start end]
  (+ 0 #_start (long (rand (- end start)))))

(s/fdef ranged-rand
        :args (s/and (s/cat :start int? :end int?)
                     #(< (:start %) (:end %)))
        :ret int?
        :fn (s/and #(>= (:ret %) (-> % :args :start))
                   #(<= (:ret %) (-> % :args :end))))

(s/instrument #'ranged-rand)

(ranged-rand 8 5)

(use 'clojure.pprint)

(st/check-var #'ranged-rand)

;;
;; test equal on both end
;;  e.g -1 2 4 -5
;;    sums of left and right (of index) 1 are equal
;;    left: -1
;;    right: 4 + -5 = -1
;;

(defn equi [xs]
  (let [rs  (reduce +' xs)
        ans (loop [xs xs
                   i  0
                   rs rs
                   ls 0]
              (let [[a & xs] xs
                    rs (-' rs (or a 0))]
                (if (= rs ls)
                  i
                  (when (seq xs)
                    (recur xs (inc i) rs (+' ls a))))))]
    (or ans -1)))

(defn sum-eq? [xs i]
  (let [[ls [_ & rs]] (split-at i xs)]
    (= (reduce +' ls) (reduce +' rs))))

(s/fdef equi
        :args (s/cat :xs (s/coll-of int? []))
        :ret (s/or
              :no-soln  #(= % -1)
              :has-soln #(>= % 0))
        :fn #(let [[rt v] (:ret %)]
               (case rt
                 :no-soln  (not-any? (partial sum-eq? (-> % :args :xs)) (range 0 (-> % :args :xs count)))
                 :has-soln (let [xs (-> % :args :xs)
                                 ls (reduce +' (take v xs))
                                 rs (reduce +' (drop (inc v) xs))]
                             (= ls rs)))))

(st/check-var #'equi)

(s/def ::S (s/coll-of #{1 -1} []))

(s/def ::S (s/& (s/* #{-1 1}) #(>= (count %) 3)))
(s/def ::N (s/int-in 0 20001))
(s/def ::E (s/int-in -100 101))
(s/def ::A (s/coll-of ::E []))

(gen/sample (s/gen ::A))
(gen/sample (s/gen ::S))
(gen/sample (s/gen ::N))

(s/explain ::S [1 1 1 -1 -2])

;;
;;  find number of pairs (edges) of same number
;;  e.g. 3, 3, 3, 4, 5, 6, 5 => 4 pairs
;;    since (3,3,3) => 3 pairs
;;          (5,5) => 1 pairs
;;

(defn pairs
  ([xs]
   (pairs 1000000000 xs))
  ([max-res xs]
   (let [ans (transduce (comp (map val)
                              (map #(/ (* % (dec %)) 2)))
                        (fn
                          ([] 0)
                          ([s] s)
                          ([s e]
                           (if (>= s max-res)
                             (reduced max-res)
                             (+' s e))))
                        (frequencies xs))]
     (min ans max-res))))

(defn pairs
  ([xs]
   (pairs 1000000000 xs))
  ([max-res xs]
   (->> xs
        frequencies
        (map val)
        (map #(/ (* % (dec %)) 2))
        (reduce (fn [s e]
                  (if (>= s max-res)
                    (reduced max-res)
                    (+' s e))))
        (min max-res))))

(s/fdef pairs
        :args (s/cat :max-res (s/int-in 1 100)
                     :xs (s/& (s/coll-of int? []) #(> (count %) 0)))
        :ret int?
        :fn (s/and #(<= (:ret %) (-> % :args :max-res))
                   #(let [max-res (-> % :args :max-res)
                          cnt (->> % :args :xs frequencies (filter (fn [[_ v]] (> v 1))) count)]
                      (>= (:ret %) (min max-res cnt)))
                   #(<= (:ret %) (-> % :args :xs count))))

(st/check-var #'pairs)

;;
;; longest match of proper prefix and suffix
;;

(defn all-suffix [s]
  (if-let [s (seq s)]
    (cons (apply str s) (lazy-seq (all-suffix (rest s))))))

(defn all-prefix [s]
  (if-let [x (seq s)]
    (cons (apply str x) (lazy-seq (all-prefix (butlast x))))))

(defn suffix-array [s]
  (let [ss (all-suffix s)]
    (apply sorted-map-by compare (interleave ss (range)))))

(defn lcps [s]
  (reduce (fn [s pfx]
            (let [ss (subseq s #(>= 0 (compare %1 %2)) pfx)]
              (if (and (= 1 (count ss))
                       (= (ffirst ss) pfx))
                (reduced ss)
                (into (empty s) ss))))
          (suffix-array s)
          (all-prefix s)))

(lcps "abab")

(lcps "abcdxxxxabcdx")

(all-suffix "abab")
(all-prefix "abcdxxxxabcd")

(all-prefix "abc")


;;
;; stack machine
;;

(defn dup [s]
  (let [x (first s)]
    (cons x s)))

(defn op [op s]
  (let [x (first s)
        y (first (rest s))]
    (cons (op x y) (drop 2 s))))

(defn push [v s]
  (cons v s))

(reductions (fn [s f]
              (try (f s)
                   (catch Exception e
                     (reduced :err))))
            '()
            [(partial push 12)
             dup
             (partial push 3)
             (partial op +)
             (partial push 20)
             (partial op -)
             (partial op +)
             (partial op +)])
