(ns mahjong.comb
  (:use (mahjong tile)))

(def ^:dynamic ^:const *ke-count* 3)
(def ^:dynamic ^:const *shun-count* 3)
(def ^:dynamic ^:const *gang-count* 4)

(defprotocol CommonComb
  (get-tile [this] [this pos])
  (pub [this])
  (tile-num [this])
  (tile-weight [this])
  (char-codes [this]))

(defprotocol ShunComb
  "Protocol for Shun."
  (head-enum [this])
  (mid-enum [this])
  (tail-enum [this]))

(defprotocol FreeComb
  "Protocol for free comb"
  (sort-tile [this])
  (add-tile [this enum cate])
  (remove-tile [this pos])
  (find-tile [this enum cate])
  (tile-seq [this] [this cate]))

(defprotocol TileCaseComb
  "Protocol for tile case"
  (chi-seq [this])
  (pong-seq [this])
  (gang-seq [this])
  (pub-gang-seq [this])
  (free-tiles [this])
  (all-comb-seq [this]))

(defrecord Ke [tile pub])

(defrecord Gang [tile pub])

(defrecord Pair [tile])

(defrecord Shun [tail mid head pub])

(defrecord FreeTiles [impl])

(defrecord TileCase [chi pong pub-gang gang free-tiles])

(extend-protocol CommonComb
  Ke
  (get-tile [this] (:tile this))
  (pub [this]
    (:pub this))
  (tile-num [this] 3)
  (tile-weight [this] 3)
  (char-codes [this]
    (repeat 3 (char-code (get-tile this))))
  Gang
  (get-tile [this] (:tile this))
  (pub [this]
    (:pub this))
  (tile-num [this] 4)
  (tile-weight [this] 3)
  (char-codes [this]
    (if (pub this)
      (repeat 4 (char-code (get-tile this)))
      (cons (char-code (get-tile this)) (repeat 3 (back-char-code (get-tile this))))))
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
  (char-codes [this]
    (map #(char-code (get-tile this %)) [0 1 2]))
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
  (char-codes [this]
    (map #(char-code %) (tile-seq this)))
  FreeComb
  (sort-tile [this]
    (let [tiles (vals (:impl this))
          sorted-tiles (sort-by tile-key tiles)]
      (assoc this :impl (apply sorted-map (interleave (range (count tiles)) sorted-tiles)))))
  (add-tile [this enum cate]
    (let [tiles (cons (make-tile enum cate) (vals (:impl this)))]
      (assoc this :impl (apply sorted-map (interleave (range (count tiles)) (sort-by tile-key tiles))))))
  (remove-tile [this pos]
    (let [tiles (vals (dissoc (:impl this) pos))]
      (assoc this :impl (apply sorted-map (interleave (range (count tiles)) tiles)))))
  (tile-seq
    ([this]
       (vals (:impl this)))
    ([this c]
       (filter #(= c (cate %)) (vals (:impl this))))))

(extend-type TileCase
  CommonComb
  (get-tile [this pos]
    (get-tile (:free-tiles this) pos))
  (tile-num [this]
    (reduce + #(tile-num %) (all-comb-seq this)))
  (tile-weight [this] (count (:impl this))
    (reduce + #(tile-weight %) (all-comb-seq this)))
  TileCaseComb
  (chi-seq [this]
    (:chi this))
  (pong-seq [this]
    (:pong this))
  (gang-seq [this]
    (:gang this))
  (pub-gang-seq [this]
    (:pub-gang this))
  (free-tiles [this]
    (:free-tiles this))
  (all-comb-seq [this]
    (lazy-cat (chi-seq this) (pong-seq this) (pub-gang-seq this) (gang-seq this) (list (free-tiles this)))))

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

(defn make-free-tiles []
  (->FreeTiles {}))

(defn make-tile-case [chi pong pub-gang gang free-tiles]
  (->TileCase chi pong pub-gang gang free-tiles))

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
