(ns mahjong.comb
  (:use (mahjong tile)))

(def ^:dynamic ^:const *ke-count* 3)
(def ^:dynamic ^:const *shun-count* 3)
(def ^:dynamic ^:const *gang-count* 4)

(defprotocol CommonComb
  (get-tile [this] [this pos])
  (pub [this])
  (tile-num [this])
  (tile-weight [this]))

(defprotocol ShunComb
  "Protocol for Shun."
  (head-enum [this])
  (mid-enum [this])
  (tail-enum [this]))

(defprotocol FreeComb
  "Protocol for free comb"
  (sort-tile [this])
  (add-tile [this enum cate])
  (remove-tile [this pos]))

(defrecord Ke [tile pub])

(defrecord Gang [tile pub])

(defrecord Pair [tile])

(defrecord Shun [tail mid head pub])

(defrecord FreeTiles [impl])

(extend-protocol CommonComb
  Ke
  (get-tile [this] (:tile this))
  (pub [this]
    (:pub this))
  (tile-num [this] 3)
  (tile-weight [this] 3)
  Gang
  (get-tile [this] (:tile this))
  (pub [this]
    (:pub this))
  (tile-num [this] 4)
  (tile-weight [this] 3)
  Pair
  (get-tile [this] (:tile this))
  (pub [this]
    false)
  (tile-num [this] 2)
  (tile-weight [this] 2))

(extend-type Shun
  CommonComb
  (get-tile [this pos]
    {:pre [(>= pos 0) (< pos 3)]}
    (cond (= 0 pos) (:tail this)
          (= 1 pos) (:mid this)
          :else (:head this)))
  (pub [this]
    (:pub this))
  (tile-num [this] 3)
  (tile-weight [this] 3)
  ShunComb
  (head-enum [this]
    (enum (:head this)))
  (mid-enum [this]
    (enum (:mid this)))
  (tail-enum [this]
    (enum (:tail this))))

(extend-type FreeTiles
  CommonComb
  (get-tile [this pos]
    {:pre [(>= pos 0) (< pos (tile-num this))]}
    ((:impl this) pos))
  (tile-num [this] (count (:impl this)))
  (tile-weight [this] (count (:impl this)))
  FreeComb
  (sort-tile [this]
    (let [tiles (vals (:impl this))
          sorted-tiles (sort-by tile-key tiles)]
      (assoc this :impl (apply sorted-map (interleave (range (count tiles)) sorted-tiles)))))
  (add-tile [this enum cate]
    (let [tiles (cons (make-tile enum cate) (vals (:impl this)))]
      (assoc this :impl (apply sorted-map (interleave (range (count tiles)) (sort-by tile-key tiles))))))
  (remove-tile [this pos]
    (assoc this :impl (dissoc (:impl this) pos))))

(defn make-ke [enum cate & {:keys [pub] :or {pub false}}]
  (->Ke (make-tile enum cate) pub))

(defn make-gang [enum cate & {:keys [pub] :or {pub false}}]
  (->Gang (make-tile enum cate) pub))

(defn make-pair [enum cate]
  (->Pair (make-tile enum cate)))

(declare step-increase?)
(defn make-shun [tail mid head cate & {:keys [pub] :or {pub false}}]
  {:pre [(step-increase? [tail mid head] 1)]}
  (apply ->Shun (lazy-cat (map #(make-tile % cate) [tail mid head]) (list pub))))

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
