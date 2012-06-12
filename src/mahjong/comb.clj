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

(defn- pattern-matched? [p]
  (let [result (and (= (:pair p) 0) (= (:triplets p) 0))]
    result))

(defn- get-remain-tiles [free discard melded]
  (filter (fn [x]
            (and (not= discard (first x))
                 (not (some #(= % (first x)) (apply concat melded)))))
          (tile-seq-with-index free)))

(defn- consume-pattern [p k]
  (assoc p k (dec (k p))))

(defn- valid-path? [x]
  (if (or (= x 'win) (= x 'ready))
    true
    (and x (:child x))))

(defn- tile-matched [tile e c]
  (and (= (enum (second tile)) e)
       (= (cate (second tile)) c)))

(defn- match-pair [cur remains]
  (if-let [t (some #(if (tile-matched % (enum cur) (cate cur)) %) remains)]
    [[(first t)] []]
    [[]]))

(defn- match-chow [cur remains]
  (let [n (some #(if (tile-matched % (succ cur) (cate cur)) %) remains)
        nn (some #(if (tile-matched % (+ (enum cur) 2) (cate cur)) %) remains)]
    (cond (and n nn) [[(first n) (first nn)] [(first n)] [(first nn)]]
          n [[(first n)]]
          nn [[(first nn)]]
          :else nil)))

(defn- match-pong [cur remains]
  (let [cur-enum (enum cur)]
    (let [[n nn & _] (filter #(tile-matched % (enum cur) (cate cur)) remains)]
      (cond (and n nn) [[(first n) (first nn)] [(first n)]]
            n [[(first n)]]
            :else nil))))

;;; pattern
;;; {:pair 1 :triplets 4}
(defn meld-normal [free-tiles pattern max-hole discard meld-index-list]
  "Meld free tiles by normal pattern. return search tree on success, 
if can't winning or ready return nil.
matched pattern:

11 111 111 111 111
11 111 111 111 123
11 111 111 123 123
11 111 123 123 123
11 123 123 123 123

PATTERN is initially set to {:pair 1 :triplets 4}.
MAX-HOLE is allowed missing tile number, initially set to 1.
DISCARD is the tile index for discarded tile, initially set to nil
MELD-INDEX-LIST is melded tiles' indexs list, initially set to []"
  (let [remain-tiles (get-remain-tiles free-tiles discard meld-index-list)]
    (if-not (empty? remain-tiles)
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
      (if (nil? discard) 'win 'ready))))

(defn- consume-n-pattern [pattern key n]
  (loop [i 0, p pattern]
    (if-not (< i n)
      p
      (recur (inc i) (consume-pattern p key)))))


(defn parse-meld-normal-tree [tree]
  (if (symbol? tree)
    (list (list tree))
    (mapcat (fn [x]
              (map #(cons [(:node-type x) (:tile x)] %)
                   (parse-meld-normal-tree (:child x))))
          tree)))

(defn parse-knitted-tree [tree]
  (if (= tree 'knitted)
    '(nil)
    (mapcat (fn [x]
              (if (= (:node-type x) :normal)
                (parse-knitted-tree (:child x))
                (map #(cons (:tile x) %)
                     (parse-knitted-tree (:child x)))))
          tree)))

(defn meld-knitted [free-tiles pattern hole-num]
  (let [prev-cates {:wan (map #(first %) (filter #(< (second %) (:wan *category-order*)) *category-order*))
                    :tiao (map #(first %) (filter #(< (second %) (:tiao *category-order*)) *category-order*)) 
                    :bing (map #(first %) (filter #(< (second %) (:bing *category-order*)) *category-order*))}]
    (letfn [(cate-knitted-matched? [k cate]
              (cate k))
            (valid-knitted-path? [x]
              (if (= x 'knitted)
                true
                (and x (:child x))))
            (match-knitted [k cur remains]
              {:pre [(not (cate-knitted-matched? k (cate cur)))]}
              (let [cur-cate (cate cur)]
                (if (not-any? #(contains? % (enum cur)) (map #(% k) (prev-cates cur-cate)))
                  (let [n (some #(if (tile-matched % (+ 3 (enum cur)) cur-cate) %) remains)
                      nn (some #(if (tile-matched % (+ 6 (enum cur)) cur-cate) %) remains)]
                  (cond (and n nn) [[(first n) (first nn)] [(first n)] [(first nn)]]
                        n [[(first n)]]
                        nn [[(first nn)]]
                        :else nil))
                  nil)))
            (add-knitted [k cur]
              {:pre [(not (cate-knitted-matched? k (cate cur)))]}
              (let [cur-enum (enum cur)
                    cur-cate (cate cur)]
                (cond (contains? #{1 4 7} cur-enum) (assoc k cur-cate #{1 4 7})
                      (contains? #{2 5 8} cur-enum) (assoc k cur-cate #{2 5 8})
                      :else (assoc k cur-cate #{3 6 9}))))
            (knitted-matched? [k]
              (= 3 (count k)))
            (iter [max-hole knitted meld-index-list]
              (let [remain-tiles (get-remain-tiles free-tiles nil meld-index-list)]
                (if-not (empty? remain-tiles)
                  (let [[cur-index cur-tile] (first remain-tiles)
                        cur-cate (cate cur-tile)]
                    (if (every? #(cate-knitted-matched? knitted %) (prev-cates cur-cate))
                      (let [valid-path-list
                            (filter #(valid-knitted-path? %)
                                    (if (cate-knitted-matched? knitted cur-cate)
                                      (list {:node-type :normal
                                             :tile cur-index
                                             :child (iter max-hole knitted (cons [cur-index] meld-index-list))}) 
                                      (concat (list {:node-type :normal
                                                     :tile cur-index
                                                     :child (iter max-hole knitted (cons [cur-index] meld-index-list))})
                                              (let [knit-index-list (match-knitted knitted cur-tile (rest remain-tiles))]
                                                (map (fn [x]
                                                       (if (< (count x) (dec *chow-count*))
                                                         (if (> max-hole 0)
                                                           {:node-type :knitted
                                                            :tile (cons cur-index x)
                                                            :child (iter (dec max-hole) (add-knitted knitted cur-tile) (cons (cons cur-index x) meld-index-list))}
                                                           nil)
                                                         {:node-type :knitted
                                                          :tile (cons cur-index x)
                                                          :child (iter max-hole (add-knitted knitted cur-tile) (cons (cons cur-index x) meld-index-list))}))
                                                     knit-index-list)))))]
                        (if-not (empty? valid-path-list)
                          valid-path-list
                          nil))
                      nil))
                  (if (knitted-matched? knitted) 'knitted
                      nil))))]
      (let [knitted-tile-list (parse-knitted-tree (iter hole-num {} []))]
          (mapcat (fn [x]
                    (let [hole (- hole-num (- (* 3 *chow-count*) (-> x flatten count))) 
                          pt (consume-n-pattern pattern :triplets 3)]
                      (if-let [meld (parse-meld-normal-tree (meld-normal free-tiles pt hole nil x))]
                        (let [knitted-meld (map #(vector :chow %) x)]
                          (map #(concat knitted-meld %) meld)))))
                  knitted-tile-list)))))

(defn meld-seven-pair [])
(defn meld-13-orphans [])
(defn meld-honors-and-knitted [])

;; (parse-meld-normal-tree (let [x (free-tiles (mahjong.dl/build-tile-case-from-ast (mahjong.dl/parse-dl-string "2147t1258w369b111f")))]
;;                           (meld-normal x {:pair 1 :triplets 1} 1 nil '((1 2 3) (4 6 7) (8 9 10)))))

;; (parse-meld-normal-tree (let [x (free-tiles (mahjong.dl/build-tile-case-from-ast (mahjong.dl/parse-dl-string "1112345678999t1f")))]
;;                            (meld-normal x {:pair 1 :triplets 4} 1 nil [])))

;; (let [x (free-tiles (mahjong.dl/build-tile-case-from-ast (mahjong.dl/parse-dl-string "1258w2147t111b369b")))]
;;    (meld-knitted x {:pair 1 :triplets 4} 1))
