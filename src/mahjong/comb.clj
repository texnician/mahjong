(ns mahjong.comb
  (:use (mahjong tile)))

(def ^:dynamic ^:const *pong-count* 3)
(def ^:dynamic ^:const *chow-count* 3)
(def ^:dynamic ^:const *kong-count* 4)

(defprotocol CommonComb
  (get-tile [this] [this pos])
  (pub [this])
  (tile-num [this])
  (tile-weight [this])
  (char-codes [this]))

(defprotocol ChowComb
  "Protocol for Chow."
  (head-enum [this])
  (mid-enum [this])
  (tail-enum [this]))

(defprotocol FreeComb
  "Protocol for free comb"
  (sort-tile [this])
  (add-tile [this enum cate])
  (remove-tile [this pos])
  (find-tile [this enum cate])
  (tile-seq [this] [this cate])
  (tile-seq-with-index [this] [this cate]))

(defprotocol TileCaseComb
  "Protocol for tile case"
  (chow-seq [this])
  (pong-seq [this])
  (kong-seq [this])
  (pub-kong-seq [this])
  (free-tiles [this])
  (all-comb-seq [this]))

(defrecord Pong [tile pub])

(defrecord Kong [tile pub])

(defrecord Pair [tile])

(defrecord Chow [tail mid head pub])

(defrecord FreeTiles [impl])

(defrecord TileCase [chow pong pub-kong kong free-tiles])

(extend-protocol CommonComb
  Pong
  (get-tile [this] (:tile this))
  (pub [this]
    (:pub this))
  (tile-num [this] 3)
  (tile-weight [this] 3)
  (char-codes [this]
    (repeat 3 (char-code (get-tile this))))
  Kong
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

(extend-type Chow
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
  ChowComb
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
       (filter #(= c (cate %)) (vals (:impl this)))))
  (tile-seq-with-index
    ([this]
       (seq (:impl this)))
    ([this c]
       (filter #(= c (cate (second %))) (seq (:impl this))))))

(extend-type TileCase
  CommonComb
  (get-tile [this pos]
    (get-tile (:free-tiles this) pos))
  (tile-num [this]
    (reduce + #(tile-num %) (all-comb-seq this)))
  (tile-weight [this] (count (:impl this))
    (reduce + #(tile-weight %) (all-comb-seq this)))
  TileCaseComb
  (chow-seq [this]
    (:chow this))
  (pong-seq [this]
    (:pong this))
  (kong-seq [this]
    (:kong this))
  (pub-kong-seq [this]
    (:pub-kong this))
  (free-tiles [this]
    (:free-tiles this))
  (all-comb-seq [this]
    (lazy-cat (chow-seq this) (pong-seq this) (pub-kong-seq this) (kong-seq this) (list (free-tiles this)))))

(defn make-pong [enum cate & {:keys [pub] :or {pub false}}]
  (->Pong (make-tile enum cate) pub))

(defn make-kong [enum cate & {:keys [pub] :or {pub false}}]
  (->Kong (make-tile enum cate) pub))

(defn make-pair [enum cate]
  (->Pair (make-tile enum cate)))

(declare step-increase?)
(defn make-chow [tail mid head cate & {:keys [pub] :or {pub false}}]
  {:pre [(step-increase? [tail mid head] 1)]}
  (apply ->Chow (lazy-cat (map #(make-tile % cate) [tail mid head]) (list pub))))

(defn make-free-tiles []
  (->FreeTiles {}))

(defn make-tile-case [chow pong pub-kong kong free-tiles]
  (->TileCase chow pong pub-kong kong free-tiles))

(defn step-increase? [v step]
  (cond (= (count v) 1) step
        (not= (- (second v) (first v)) step) false
        :else (recur (rest v) step)))

(defn pong? [v]
  (and (= (count v) *pong-count*) (step-increase? v 0)))

(defn chow? [v]
  (and (= (count v) *chow-count*) (step-increase? v 1)))

(defn kong? [v]
  (and (= (count v) *kong-count*) (step-increase? v 0)))

;;; pattern
;;; {:pair 1 :triplets 4}
(defn meld-normal [free-tiles pattern max-hole discard meld-index-list]
  (letfn [(pattern-matched? [p]
            (let [result (and (= (:pair p) 0) (= (:triplets p) 0))]
              result))
          (consume-pattern [p k]
            (assoc p k (dec (k p))))
          (valid-path? [x]
            (if (or (= x 'win) (= x 'ready))
              true
              (and x (:child x))))
          (tile-matched [tile e c]
            (and (= (enum (second tile)) e)
                 (= (cate (second tile)) c)))
          (match-pair [cur remains]
            (if-let [t (some #(if (tile-matched % (enum cur) (cate cur)) %) remains)]
              [[(first t)] []]
              [[]]))
          (match-chow [cur remains]
            (let [n (some #(if (tile-matched % (succ cur) (cate cur)) %) remains)
                  nn (some #(if (tile-matched % (+ (enum cur) 2) (cate cur)) %) remains)]
              (cond (and n nn) [[(first n) (first nn)] [(first n)] [(first nn)]]
                    n [[(first n)]]
                    nn [[(first nn)]]
                    :else nil)))
          (match-pong [cur remains]
            (let [cur-enum (enum cur)]
              (let [[n nn & _] (filter #(tile-matched % (enum cur) (cate cur)) remains)]
                (cond (and n nn) [[(first n) (first nn)] [(first n)]]
                      n [[(first n)]]
                      :else nil))))]
    (let [remain-tiles (filter (fn [x]
                                 (and (not= discard (first x))
                                      (not (some #(= % (first x)) (apply concat meld-index-list)))))
                               (tile-seq-with-index free-tiles))]
      (if (-> remain-tiles empty? not)
        (let [[cur-index cur-tile] (first remain-tiles)]
          (if (pattern-matched? pattern)
            (if (= 1 (count remain-tiles))
              (list {:node-type :discard
                     :tile cur-index
                     :child (meld-normal free-tiles pattern max-hole cur-index meld-index-list)})
              nil)
            (let [valid-path-list
                  (filter #(valid-path? %)
                          (concat (if (> (:pair pattern) 0)
                                    (let [pair-idx-list (match-pair cur-tile (rest remain-tiles))]
                                      (map (fn [x]
                                             (if (< (count x) 1)
                                               (if (> max-hole 0)
                                                 {:node-type :pair
                                                  :tile [cur-index]
                                                  :child (meld-normal free-tiles (consume-pattern pattern :pair)
                                                                      (dec max-hole) discard (cons [cur-index] meld-index-list))}
                                                 nil)
                                               {:node-type :pair
                                                :tile (cons cur-index x)
                                                :child (meld-normal free-tiles (consume-pattern pattern :pair)
                                                                    max-hole discard (cons (cons cur-index x) meld-index-list))}))
                                           pair-idx-list)))
                                  (if (and (> (:triplets pattern) 0) (suit? cur-tile))
                                    (let [chow-index-list (match-chow cur-tile (rest remain-tiles))]
                                      (map (fn [x]
                                             (if (< (count x) (dec *chow-count*))
                                               (if (> max-hole 0)
                                                 {:node-type :chow
                                                  :tile (cons cur-index x)
                                                  :child (meld-normal free-tiles (consume-pattern pattern :triplets)
                                                                      (dec max-hole) discard (cons (cons cur-index x) meld-index-list))}
                                                 nil)
                                               {:node-type :chow
                                                :tile (cons cur-index x)
                                                :child (meld-normal free-tiles (consume-pattern pattern :triplets)
                                                                    max-hole discard (cons (cons cur-index x) meld-index-list))}))
                                           chow-index-list)))
                                  (if (> (:triplets pattern) 0)
                                    (let [pong-index-list (match-pong cur-tile (rest remain-tiles))]
                                      (map (fn [x]
                                             (if (< (count x) (dec *pong-count*))
                                               (if (> max-hole 0)
                                                 {:node-type :pong
                                                  :tile (cons cur-index x)
                                                  :child (meld-normal free-tiles (consume-pattern pattern :triplets)
                                                                      (dec max-hole) discard (cons (cons cur-index x) meld-index-list))}
                                                 nil)
                                               {:node-type :pong
                                                :tile (cons cur-index x)
                                                :child (meld-normal free-tiles (consume-pattern pattern :triplets)
                                                                    max-hole discard (cons (cons cur-index x) meld-index-list))}))
                                           pong-index-list)))
                                  (if-not discard
                                    (list {:node-type :discard
                                             :tile cur-index
                                             :child (meld-normal free-tiles pattern max-hole cur-index meld-index-list)})
                                    nil)))]
              (if (empty? valid-path-list)
                nil
                valid-path-list))))
        (if (nil? discard) 'win 'ready)))))
