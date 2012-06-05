(ns mahjong.comb
  (:use (mahjong tile)))

(def ^:const *ke-count* 3)
(def ^:const *shun-count* 3)
(def ^:const *gang-count* 4)

(defprotocol CommonComb
  (get-tile [this pos])
  (get-tile [this])
  (pub [this])
  (tile-num [this])
  (count [this]))

(defprotocol ShunComb
  "Protocol for Shun."
  (head-enum [this])
  (mid-enum [this])
  (tail-enum [this]))

(defrecord Ke [tile pub])

(defrecord Gang [tile pub]
  clojure.lang.IPersistentCollection
  (count [this] 4))

(defrecord Pair [tile]
  clojure.lang.IPersistentCollection
  (count [this] 2))

(extend-protocol clojure.lang.IPersistentSet
  Ke
  (count [this] 3))

(extend-protocol CommonComb
  Ke
  (get-tile [this] (:tile this))
  (pub [this]
    (:pub this))
  Gang
  (get-tile [this] (:tile this))
  (pub [this]
    (:pub this))
  Pair
  (get-tile [this] (:tile this))
  (pub [this]
    false))

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
