(ns mahjong.comb)

(def ^:const *ke-count* 3)
(def ^:const *shun-count* 3)
(def ^:const *gang-count* 4)

(defn step-increase? [v step]
  (cond (= (count v) 1) step
        (not= (- (second v) (first v)) step) false
        :else (recur (rest v) step)))

(defn ke? [v]
  (and (= (count v) *ke-count*) (step-increase? v 0)))

(defn shun? [v]
  (and (= (count v) *shun-count*) (step-increase? v 1)))

(defn gang? [v]
  (and (= (count v) *gang-count*) (step-increase? v 0)))
