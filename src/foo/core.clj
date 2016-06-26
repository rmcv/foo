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

(defn equi [xs]
  (let [rs  (reduce +' xs)
        ans (loop [[a & xs] xs
                   i        0
                   rs       rs
                   ls       0]
              (when a
                (if (not= rs ls)
                  (recur xs (inc i) (-' rs a) (+' ls a))
                  i)))]
    (or ans -1))
  #_(or (let [ys (map-indexed (fn [i e] [i  (sum-eq? xs e)]) (range 0 (count xs)))]
        (->> ys
             (filter (comp true? last))
             ffirst))
      -1)
  #_(rand-int (count xs)))

(defn sum-eq? [xs i]
  (let [[ls rs] (split-at i xs)]
    (= (reduce +' ls) (reduce +' rs))))

(s/fdef equi
        :args (s/cat :xs (s/coll-of int? []))
        :ret (s/or
              :no-soln #(= % -1)
              :has-soln #(>= % 0))
        :fn #(let [[rt v] (:ret %)]
               (case rt
                 :no-soln (not-any? (partial sum-eq? (-> % :args :xs)) (range 0 (-> % :args :xs count))) 
                 :has-soln (let [xs (-> % :args :xs)
                                 ls (reduce +' (take v xs))
                                 rs (reduce +' (drop v xs))]
                             (= ls rs))))
        #_:fn #_(s/or
             :no-soln (s/and
                       #(= -1 (:ret %))
                       #_(not-any? (partial sum-eq? (-> % :args :xs)) (range 0 (-> % :args :xs count))))
             :yes-soln (s/and
                        #(>= (:ret %) 0)
                        #(<= (:ret %) (-> :args :xs count))
                        #_(let [xs (-> % :args :xs)
                               ret (:ret %)
                               ls (reduce +' (take ret xs))
                               rs (reduce +' (drop ret xs))]
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

(s/fdef min-abs-sum
        :args (s/cat :A ::A)
        :ret int?
        :fn 
        )
