(ns mahjong.comb
  (:gen-class)
  (:use (mahjong tile))
  (:require (clojure set)))

(def ^:dynamic ^:const *pong-count* 3)
(def ^:dynamic ^:const *chow-count* 3)
(def ^:dynamic ^:const *kong-count* 4)

(defprotocol CommonComb
  (get-tile [this] [this pos])
  (pub [this])
  (tile-num [this])
  (tile-weight [this])
  (tile-seq [this] [this suit])
  (char-codes [this])
  (comb-suit [this]))

(defprotocol ChowComb
  "Protocol for Chow."
  (head-enum [this])
  (mid-enum [this])
  (tail-enum [this]))

(defprotocol FreeComb
  "Protocol for free comb"
  (sort-tile [this])
  (add-tile [this enum suit])
  (remove-tile [this pos])
  (find-tile [this enum suit])
  (tile-seq-with-index [this] [this suit]))

(defprotocol TileCaseComb
  "Protocol for tile case"
  (chow-seq [this] [this ready])
  (pong-seq [this] [this ready])
  (kong-seq [this] [this ready])
  (pair-seq [this] [this ready])
  (pub-kong-seq [this] [this ready])
  (orphans-seq [this] [this ready])
  (free-tiles [this] [this ready])
  (all-comb-seq [this] [this ready]))

(extend-protocol CommonComb
  nil
  (get-tile
    ([this] nil)
    ([this pos] nil))
  (pub [this] nil)
  (tile-num [this] nil)
  (tile-weight [this] nil)
  (tile-seq
    ([this] nil)
    ([this suit] nil) )
  (char-codes [this] nil)
  (comb-suit [this] nil))

(extend-protocol TileCaseComb
  nil
  (chow-seq
    ([this] nil)
    ([this ready] nil))
  (pong-seq ([this] nil) ([this ready] nil))
  (kong-seq ([this] nil) ([this ready] nil))
  (pair-seq ([this] nil) ([this ready] nil))
  (pub-kong-seq ([this] nil) ([this ready] nil))
  (orphans-seq ([this] nil) ([this ready] nil))
  (free-tiles ([this] nil) ([this ready] nil))
  (all-comb-seq ([this] nil) ([this ready] nil)))

(defrecord Pong [tile pub])

(defrecord Kong [tile pub])

(defrecord Pair [tile])

(defrecord Chow [tail mid head pub])

(defrecord FreeTiles [impl])

(defrecord TileCase [chow pong pub-kong kong free-tiles])

(defrecord Orphans [tile-list])

(extend-protocol CommonComb
  Pong
  (get-tile [this] (:tile this))
  (pub [this]
    (:pub this))
  (tile-num [this] 3)
  (tile-weight [this] 3)
  (char-codes [this]
    (repeat 3 (char-code (get-tile this))))
  (tile-seq [this]
    (repeat 3 (get-tile this)))
  (comb-suit [this]
    (suit (get-tile this)))
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
  (tile-seq [this]
    (repeat 4 (get-tile this)))
  (comb-suit [this]
    (suit (get-tile this)))
  Pair
  (get-tile [this] (:tile this))
  (pub [this]
    false)
  (tile-num [this] 2)
  (tile-weight [this] 2)
  (tile-seq [this] (repeat 2 (get-tile this)))
  (comb-suit [this]
    (suit (get-tile this)))
  Orphans
  (tile-num [this] (count (:tile-list this)))
  (tile-weight [this] (tile-num this))
  (tile-seq [this] (:tile-list this))
  (comb-suit [this] nil))


(extend-type Chow
  CommonComb
  (get-tile
    ([this pos]
      {:pre [(>= pos 0) (< pos 3)]}
      (cond (= 0 pos) (:tail this)
          (= 1 pos) (:mid this)
          :else (:head this)))
    ([this]
       (:tail this)))
  (pub [this]
    (:pub this))
  (tile-num [this] 3)
  (tile-weight [this] 3)
  (char-codes [this]
    (map #(char-code (get-tile this %)) [0 1 2]))
  (tile-seq [this] (map #(get-tile this %) [0 1 2]))
  (comb-suit [this]
    (suit (get-tile this 0)))
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
  (tile-seq
    ([this]
       (vals (:impl this)))
    ([this c]
       (filter #(= c (suit %)) (vals (:impl this)))))
  (comb-suit [this] nil)
  FreeComb
  (sort-tile [this]
    (let [tiles (vals (:impl this))
          sorted-tiles (sort-by tile-key tiles)]
      (assoc this :impl (apply sorted-map (interleave (range (count tiles)) sorted-tiles)))))
  (add-tile [this enum suit]
    (let [tiles (cons (make-tile enum suit) (vals (:impl this)))]
      (assoc this :impl (apply sorted-map (interleave (range (count tiles)) (sort-by tile-key tiles))))))
  (remove-tile [this pos]
    (let [tiles (vals (dissoc (:impl this) pos))]
      (assoc this :impl (apply sorted-map (interleave (range (count tiles)) tiles)))))
  (tile-seq-with-index
    ([this]
       (seq (:impl this)))
    ([this c]
       (filter #(= c (suit (second %))) (seq (:impl this))))))

(extend-type TileCase
  CommonComb
  (get-tile [this pos]
    (get-tile (:free-tiles this) pos))
  (tile-num [this]
    (reduce + (map #(tile-num %) (all-comb-seq this))))
  (tile-weight [this] (count (:impl this))
    (reduce + (map #(tile-weight %) (all-comb-seq this))))
  (tile-seq [this]
    (mapcat #(tile-seq %) (all-comb-seq this)))
  TileCaseComb
  (chow-seq [this] (:chow this))
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

(defn make-pong [enum suit & {:keys [pub] :or {pub false}}]
  (with-meta (->Pong (make-tile enum suit) pub)
    {:tag :pong}))

(defn make-kong [enum suit & {:keys [pub] :or {pub false}}]
  (with-meta (->Kong (make-tile enum suit) pub)
    {:tag :kong}))

(defn make-pair [enum suit]
  (with-meta (->Pair (make-tile enum suit))
    {:tag :pair}))

(defn make-orphans [& orphans]
  {:pre [(even? (count orphans))]}
  (with-meta (->Orphans (map (fn [x]
                               (let [[enum suit] x]
                                 (make-tile enum suit)))
                             (partition 2 orphans)))
    {:tag :orphans}))

(declare step-increase?)
(defn make-chow [tail mid head suit & {:keys [pub] :or {pub false}}]
  {:pre [(or (step-increase? [tail mid head] 1) (step-increase? [tail mid head] 3))]}
  (with-meta (apply ->Chow (lazy-cat (map #(make-tile % suit) [tail mid head]) (list pub)))
    {:tag :chow}))

(defn make-free-tiles []
  (with-meta (->FreeTiles {})
    {:tag :free-tiles}))

(defn make-tile-case [chow pong pub-kong kong free-tiles]
  (with-meta (->TileCase chow pong pub-kong kong free-tiles)
    {:tag :tile-case}))

(defn step-increase? [v step]
  (cond (empty? v) false
        (= (count v) 1) step
        (not= (- (second v) (first v)) step) false
        :else (recur (rest v) step)))

(defn pong? [v]
  (and (= (count v) *pong-count*) (step-increase? v 0)))

(defn chow? [v]
  (and (= (count v) *chow-count*) (step-increase? v 1)))

(defn kong? [v]
  (and (= (count v) *kong-count*) (step-increase? v 0)))

(defn group-combs-by [f coll]
  (group-by f coll))

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
       (= (suit (second tile)) c)))

(defn- match-pair [cur remains]
  (if-let [t (some #(if (tile-matched % (enum cur) (suit cur)) %) remains)]
    [[(first t)] []]
    [[]]))

(defn- match-chow [cur remains]
  (let [n (some #(if (tile-matched % (succ cur) (suit cur)) %) remains)
        nn (some #(if (tile-matched % (+ (enum cur) 2) (suit cur)) %) remains)]
    (cond (and n nn) [[(first n) (first nn)] [(first n)] [(first nn)]]
          n [[(first n)]]
          nn [[(first nn)]]
          :else nil)))

(defn- match-pong [cur remains]
  (let [cur-enum (enum cur)]
    (let [[n nn & _] (filter #(tile-matched % (enum cur) (suit cur)) remains)]
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
DISCARD is the tile index to discard.
initially set to nil. If tile number == 13, set to -1.  MELD-INDEX-LIST is
melded tiles' indexs list, initially set to []"
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
                (filter
                 #(valid-path? %)
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
                         (if (and (> (:triplets pattern) 0) (simple? cur-tile))
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

(defn parse-meld-seven-pairs-tree [tree]
  (if (symbol? tree)
    (list (list tree))
    (mapcat (fn [x]
              (map #(cons [(:node-type x) (:tile x)] %)
                   (parse-meld-seven-pairs-tree (:child x))))
            tree)))

(defn parse-meld-orphans-tree [tree]
  (if (symbol? tree)
    (list (list tree))
    (mapcat (fn [x]
              (map #(cons [(:node-type x) (:tile x)] %)
                   (parse-meld-orphans-tree (:child x))))
            tree)))

(defn meld-knitted [free-tiles pattern hole-num discard]
  "meld free tiles by knitted pattern: 

11 147 258 369 111
11 147 258 369 123

PATTERN is match pattern, initially set to {:pair 1 :triplets 4}
HOLE-NUM is max allowed missing tile number, initially set to 1.
DISCARD is the tile index to discard, initially set to nil, if time number = 13, set to -1"
  (let [prev-suits {:wan (map #(first %) (filter #(< (second %) (:wan *category-order*)) *category-order*))
                    :tiao (map #(first %) (filter #(< (second %) (:tiao *category-order*)) *category-order*)) 
                    :bing (map #(first %) (filter #(< (second %) (:bing *category-order*)) *category-order*))}]
    (letfn [(suit-knitted-matched? [k suit]
              (suit k))
            (valid-knitted-path? [x]
              (if (= x 'knitted)
                true
                (and x (:child x))))
            (match-knitted [k cur remains]
              {:pre [(not (suit-knitted-matched? k (suit cur)))]}
              (let [cur-suit (suit cur)]
                (if (not-any? #(contains? % (enum cur)) (map #(% k) (prev-suits cur-suit)))
                  (let [n (some #(if (tile-matched % (+ 3 (enum cur)) cur-suit) %) remains)
                      nn (some #(if (tile-matched % (+ 6 (enum cur)) cur-suit) %) remains)]
                  (cond (and n nn) [[(first n) (first nn)] [(first n)] [(first nn)]]
                        n [[(first n)]]
                        nn [[(first nn)]]
                        :else nil))
                  nil)))
            (add-knitted [k cur]
              {:pre [(not (suit-knitted-matched? k (suit cur)))]}
              (let [cur-enum (enum cur)
                    cur-suit (suit cur)]
                (cond (contains? #{1 4 7} cur-enum) (assoc k cur-suit #{1 4 7})
                      (contains? #{2 5 8} cur-enum) (assoc k cur-suit #{2 5 8})
                      :else (assoc k cur-suit #{3 6 9}))))
            (knitted-matched? [k]
              (= 3 (count k)))
            (iter [max-hole knitted meld-index-list]
              (let [remain-tiles (get-remain-tiles free-tiles nil meld-index-list)]
                (if-not (empty? remain-tiles)
                  (let [[cur-index cur-tile] (first remain-tiles)
                        cur-suit (suit cur-tile)]
                    (if (every? #(suit-knitted-matched? knitted %) (prev-suits cur-suit))
                      (let [valid-path-list
                            (filter #(valid-knitted-path? %)
                                    (if (suit-knitted-matched? knitted cur-suit)
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
                                                            :child (iter (dec max-hole)
                                                                         (add-knitted knitted cur-tile)
                                                                         (cons (cons cur-index x) meld-index-list))}
                                                           nil)
                                                         {:node-type :knitted
                                                          :tile (cons cur-index x)
                                                          :child (iter max-hole
                                                                       (add-knitted knitted cur-tile)
                                                                       (cons (cons cur-index x) meld-index-list))}))
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
                      (if-let [meld (parse-meld-normal-tree (meld-normal free-tiles pt hole discard x))]
                        (let [knitted-meld (map #(vector :knitted %) x)]
                          (map #(concat knitted-meld %) meld)))))
                  knitted-tile-list)))))

(defn meld-seven-pairs [free-tiles discard]
  "Meld tiles by seven pairs pattern

11 11 11 11 11 11 11

DISCARD is the tile index to discard, initially set to nil. If tile number = 13, set to -1"
  (letfn [(seven-pair-pattern-matched? [pt]
            (= 0 (:pair pt)))
          (iter [pattern max-hole discard meld-index-list]
            (let [remain-tiles (get-remain-tiles free-tiles discard meld-index-list)]
              (if-not (empty? remain-tiles)
                (let [[cur-index cur-tile] (first remain-tiles)]
                  (if (seven-pair-pattern-matched? pattern)
                    (if (= 1 (count remain-tiles))
                      (list {:node-type :discard
                             :tile cur-index
                             :child (iter pattern max-hole cur-index meld-index-list)})
                      (assert false))
                    (let [valid-path-list
                          (filter
                           #(valid-path? %)
                           (concat (if (> (:pair pattern) 0)
                                     (let [pair-idx-list (match-pair cur-tile (rest remain-tiles))]
                                       (map (fn [x]
                                              (if (< (count x) 1)
                                                (if (> max-hole 0)
                                                  {:node-type :pair
                                                   :tile (list cur-index)
                                                   :child (iter (consume-pattern pattern :pair)
                                                                (dec max-hole) discard
                                                                (cons (list cur-index) meld-index-list))}
                                                  nil)
                                                {:node-type :pair
                                                 :tile (cons cur-index x)
                                                 :child (iter (consume-pattern pattern :pair)
                                                              max-hole discard
                                                              (cons (cons cur-index x) meld-index-list))}))
                                            pair-idx-list))
                                     nil)
                                   (if-not discard
                                     (list {:node-type :discard
                                            :tile cur-index
                                            :child (iter pattern max-hole cur-index meld-index-list)})
                                     nil)))]
                      (if-not (empty? valid-path-list)
                        valid-path-list
                        nil))))
                (if discard 'ready 'win))))]
    (iter {:pair 7} 1 discard [])))

(defn meld-13-orphans [free-tiles discard]
  "Meld tiles by 13 orphans pattern

11 1 1 1 1 1 1 1 1 1 1 1 1

DISCARD is tile index to discard, initially set to nil. If tile number = 13, set to -1."
  {:pre [(>= (tile-num free-tiles) 13)]}
  (letfn [(orphans-pattern-matched? [pt]
            (every? #(= 0 %) (vals pt)))
          (miss-one-orphan? [pt]
            (and (= (:pair pt) 0)
                 (= (count (filter #(= 1 %) (vals (dissoc pt :pair)))) 1)))
          (pattern-key [tile]
            (keyword (tile-name tile)))
          (iter [pattern max-hole discard meld-index-list]
            (let [remain-tiles (get-remain-tiles free-tiles discard meld-index-list)]
              (if-not (empty? remain-tiles)
                (let [[cur-index cur-tile] (first remain-tiles)
                      pt-key (pattern-key cur-tile)]
                  (if (orphans-pattern-matched? pattern)
                    (if (= (count remain-tiles) 1)
                      (list {:node-type :discard
                             :tile cur-index
                             :child (iter pattern max-hole cur-index meld-index-list)})
                      (assert false))
                    (let [valid-path-list
                          (filter
                           #(valid-path? %)
                           (cond (not (terminal-or-honor? cur-tile))
                                 (if-not discard (list {:node-type :discard
                                                        :tile cur-index
                                                        :child (iter pattern max-hole cur-index meld-index-list)})
                                         nil)
                                 (> (pt-key pattern) 0)
                                 (concat (if (> (:pair pattern) 0)
                                           (let [pair-idx-list (match-pair cur-tile (rest remain-tiles))]
                                             (map (fn [x]
                                                    (if (< (count x) 1)
                                                      (if (> max-hole 0)
                                                        {:node-type :pair
                                                         :tile (list cur-index)
                                                         :child (iter (consume-pattern (consume-pattern pattern :pair) pt-key)
                                                                      (dec max-hole) discard (cons (list cur-index) meld-index-list))}
                                                        nil)
                                                      {:node-type :pair
                                                       :tile (cons cur-index x)
                                                       :child (iter (consume-pattern (consume-pattern pattern :pair) pt-key)
                                                                    max-hole discard (cons (cons cur-index x) meld-index-list))}))
                                                  pair-idx-list)))
                                         (list {:node-type :orphan
                                                :tile (list cur-index)
                                                :child (iter (consume-pattern pattern pt-key)
                                                             max-hole discard (cons (list cur-index) meld-index-list))})
                                         (if-not discard
                                           (list {:node-type :discard
                                                  :tile cur-index
                                                  :child (iter pattern max-hole cur-index meld-index-list)})))
                                 :else
                                 (if-not discard
                                   (list {:node-type :discard
                                          :tile cur-index
                                          :child (iter pattern max-hole cur-index meld-index-list)})
                                   nil)))]
                      (if-not (empty? valid-path-list) valid-path-list nil))))
                (cond (orphans-pattern-matched? pattern) (if discard 'ready 'win)
                      (miss-one-orphan? pattern) 'ready
                      :else nil))))]
    (iter {:pair 1 :1W 1 :9W 1 :1T 1 :9T 1 :1B 1 :9B 1 :Dong 1 :Xi 1 :Nan 1 :Bei 1 :Zhong 1 :Fa 1 :Bai 1}
          1 discard [])))

(defn meld-honors-and-knitted [free-tiles discard]
  "Meld tiles by honors and knitted pattern

1 1 1 1 1 1 1 1 1 1 1 1 1 1 

DISCARD is the tile index to discard, initially set to nil, if tile number = 13, set to -1"
  (letfn [(orphan-tile? [pattern tile]
            (if (simple? tile)
              (let [tmp (apply concat (map (fn [x]
                                             (x pattern))
                                           (disj #{:wan :tiao :bing} (suit tile))))]
                (if (not (contains? (:samples pattern) (enum tile)))
                  (if (and (not ((suit tile) pattern))
                           (not (some #(= % (enum tile)) (apply concat (map (fn [x]
                                                                (x pattern))
                                                              (disj #{:wan :tiao :bing} (suit tile)))))))
                    true
                    (contains? ((suit tile) pattern) (enum tile)))))              
              (not (contains? ((suit tile) pattern) (enum tile)))))
          (add-orphan-tile [pattern tile]
            {:pre [(orphan-tile? pattern tile)]}
            (if (simple? tile)
              (let [p (assoc pattern :samples (conj (:samples pattern) (enum tile)))]
                (cond (#{1 4 7} (enum tile)) (assoc p (suit tile) #{1 4 7})
                      (#{2 5 8} (enum tile)) (assoc p (suit tile) #{2 5 8})
                      :else (assoc p (suit tile) #{3 6 9})))
              (assoc pattern (suit tile) (conj ((suit tile) pattern) (enum tile)))))
          (iter [pattern max-hole discard meld-index-list]
            (let [remain-tiles (get-remain-tiles free-tiles discard meld-index-list)]
              (if-not (empty? remain-tiles)
                (let [[cur-index cur-tile] (first remain-tiles)]
                  (let [valid-path-list
                        (filter #(valid-path? %)
                                (concat (if (orphan-tile? pattern cur-tile)
                                          (list {:node-type :orphan
                                                 :tile (list cur-index)
                                                 :child (iter (add-orphan-tile pattern cur-tile)
                                                              max-hole discard
                                                              (cons (list cur-index) meld-index-list))}))
                                        (if-not discard
                                          (list {:node-type :discard
                                                 :tile cur-index
                                                 :child (iter pattern max-hole cur-index meld-index-list)}) 
                                          nil)))]
                    (if-not (empty? valid-path-list)
                      valid-path-list
                      nil)))
                (if discard 'ready 'win))))]
    (iter {:feng #{} :jian #{} :samples #{}} 1 discard [])))

(defn map-tile-index [case index-list]
  (map #(get-tile (free-tiles case) %) index-list))

(defn- parse-ready-tile [comb mapper tile-index-list]
  (cond (= comb :chow) (if (< (count tile-index-list) *chow-count*)
                         (let [[tile1 tile2] (mapper tile-index-list)]
                           (if (= 1 (- (enum tile2) (enum tile1)))
                             (let [l (pre tile1)
                                   r (succ tile2)]
                               (cond (and l r) [(make-tile l (suit-sym tile1))
                                                (make-tile r (suit-sym tile2))]
                                     l [(make-tile l (suit-sym tile1))]
                                     :else [(make-tile r (suit-sym tile2))]))
                              [(make-tile (succ tile1) (suit-sym tile1))])))
        (= comb :pong) (if (< (count tile-index-list) *pong-count*)
                         (let [[tile & _] (mapper tile-index-list)]
                           [(make-tile (enum tile) (suit-sym tile))]))
        (= comb :pair) (if (< (count tile-index-list) 2)
                         (let [[tile & _] (mapper tile-index-list)]
                           [(make-tile (enum tile) (suit-sym tile))]))
        (= comb :knitted) (if (< (count tile-index-list) *chow-count*)
                            (let [[tile1 tile2] (mapper tile-index-list)]
                              (if (= 3 (- (enum tile2) (enum tile1)))
                                (if (< (enum tile1) 4) [(make-tile (+ 6 (enum tile1)) (suit-sym tile1))]
                                    [(make-tile (- (enum tile2) 6) (suit-sym tile2))])
                                [(make-tile (+ 3 (enum tile1)) (suit-sym tile1))])))))

(defn- parse-normal-meld-path [case meld-path]
  "Parse normal pattern meld path ([:chow (1 2 3)] [:chow (4 6 7)] [:chow (11 12 13)] [:pair [0]] [:discard 5] [:pong (8 9 10)] ready)
-> {:result 'ready :meld {:chow [(1 2 3) (4 6 7) (11 12 13)]
                          :pong [(8 9 10)]
                          :discard 5}
    :read-for [tile1 tile2]})"
  (let [result (last meld-path)
        chow-list (filter #(= (first %) :chow) (drop-last meld-path))
        pong-list (filter #(= (first %) :pong) (drop-last meld-path))
        pair-list (filter #(= (first %) :pair) (drop-last meld-path))
        knitted-list (filter #(= (first %) :knitted) (drop-last meld-path))
        discard (filter #(= (first %) :discard) (drop-last meld-path))]
    (into {:result result
           :type :normal
           :meld {:chow (map #(second %) chow-list)
                  :pong (map #(second %) pong-list)
                  :pair (map #(second %) pair-list)
                  :knitted (map #(second %) knitted-list)}
           :ready-for (some #(parse-ready-tile (first %) (partial map-tile-index case) (second %))
                            (concat chow-list pong-list pair-list knitted-list))} discard)))

(defn- parse-seven-pairs-path [case meld-path]
  (let [result (last meld-path)
        pair-list (filter #(= (first %) :pair) (drop-last meld-path))
        discard (filter #(= (first %) :discard) (drop-last meld-path))]
    (into {:result result
           :type :seven-pairs
           :meld {:pair (map #(second %) pair-list)}
           :ready-for (some #(parse-ready-tile (first %) (partial map-tile-index case) (second %))
                            pair-list)} discard)))

(def ^:dynamic ^:const *13-orphans* {:wan #{1 9}
                                     :bing #{1 9}
                                     :tiao #{1 9}
                                     :feng #{1 2 3 4}
                                     :jian #{1 2 3}})

(defn- parse-13-orphans-ready-tile [mapper all-orphans]
  (let [all-tiles (mapper all-orphans)
        all-13-orphans-set (into #{} (mapcat (fn [x]
                                           (let [[c e] x]
                                             (map #(vector c %) e))) *13-orphans*))]
    (let [[c e] (first (clojure.set/difference all-13-orphans-set
                                               (into #{} (map #(vector (suit %) (enum %)) all-tiles))))]
      (if e
        (list (make-tile e (symbol (subs (name c) 0 1))))))))

(defn- parse-13-orphans-path [case meld-path]
  (let [result (last meld-path)
        pair-list (filter #(= (first %) :pair) (drop-last meld-path))
        discard (filter #(= (first %) :discard) (drop-last meld-path))
        orphan-list (filter #(= (first %) :orphan) (drop-last meld-path))
        all-orphans (cons (first (second (first pair-list))) (flatten (map #(second %) orphan-list)))]
    (into {:result result
           :type :13-orphans
           :meld {:pair (map #(second %) pair-list)
                  :orphans (map #(second %) orphan-list)}
           :ready-for (let [r (some #(parse-ready-tile (first %) (partial map-tile-index case) (second %))
                                           pair-list)]
                               (if r r
                                   (parse-13-orphans-ready-tile (partial map-tile-index case) all-orphans)))}
          discard)))

(defn- parse-honors-and-knitted-ready-tile [mapper all-orphans]
  (letfn [(knit-suit-map [wans tiaos bings]
            (let [suit-map (into {} (map (fn [x]
                                           (let [c (suit (first x))]
                                             (cond (some #(contains? #{1 4 7} (enum %)) x) [c #{1 4 7}]
                                                   (some #(contains? #{2 5 8} (enum %)) x) [c #{2 5 8}]
                                                   (some #(contains? #{3 6 9} (enum %)) x) [c #{3 6 9}])))
                                         [wans bings tiaos]))]
              (if (< (count suit-map) 3)
                (let [miss-suit (first (clojure.set/difference #{:wan :tiao :bing} (keys suit-map)))]
                  (assoc suit-map miss-suit (clojure.set/difference #{1 2 3 4 5 6 7 8 9}
                                                                    (apply clojure.set/union (vals suit-map)))))
                suit-map)))]
    (let [all-tiles (mapper all-orphans)
          tile-map (merge {:wan nil :tiao nil :bing nil :feng nil :jian nil} (group-by #(suit %) all-tiles))
          honors-and-knitted-map (merge {:feng #{1 2 3 4} :jian #{1 2 3}}
                                        (knit-suit-map (:wan tile-map) (:tiao tile-map) (:bing tile-map)))]
      (if (< (count all-tiles) 14)
        (let [ready-tiles (filter #(not-empty (second %))
                                (map (fn [x]
                                       (let [[c s] x]
                                         [c (clojure.set/difference s (set (map #(enum %) (c tile-map))))]))
                                     honors-and-knitted-map)) ]
        (mapcat (fn [x]
                  (let [[c tiles] x]
                    (map #(make-tile % (symbol (subs (name c) 0 1))) tiles)))
                ready-tiles))))))

(defn- parse-honors-and-knitted-path [case meld-path]
  (let [result (last meld-path)
        discard (filter #(= (first %) :discard) (drop-last meld-path))
        orphan-list (filter #(= (first %) :orphan) (drop-last meld-path))
        all-orphans (flatten (map #(second %) orphan-list))]
    (into {:result result
           :type :honors-and-knitted
           :meld {:orphans (map #(second %) orphan-list)}
           :ready-for (parse-honors-and-knitted-ready-tile (partial map-tile-index case) all-orphans)}
          discard)))

(defn parse-by-normal-pattern [case]
  "parse hands by normal pattern"
  (let [pattern {:pair 1 :triplets 4}
        tripplet-num (reduce + (map #(count %) [(chow-seq case) (pong-seq case) (kong-seq case) (pub-kong-seq case)]))
        discard (if (= 14 (tile-weight case)) nil -1)]
    (let [meld-path-list (parse-meld-normal-tree (meld-normal (free-tiles case) (consume-n-pattern pattern  :triplets tripplet-num) 1 discard []))]
      (if-not (empty? meld-path-list)
        (map #(parse-normal-meld-path case %) meld-path-list)
        (let [knitted-meld-list (meld-knitted (free-tiles case) (consume-n-pattern pattern :triplets tripplet-num) 1 discard)]
          (if-not (empty? knitted-meld-list)
            (map #(parse-normal-meld-path case %) knitted-meld-list)))))))

(defn parse-by-seven-pairs-pattern [case]
  {:pre [(>= (tile-num (free-tiles case)) 13)]}
  (let [discard (if (= 14 (tile-weight case)) nil -1)
        meld-path-list (parse-meld-seven-pairs-tree (meld-seven-pairs (free-tiles case) discard))]
    (if-not (empty? meld-path-list)
      (map #(parse-seven-pairs-path case %) meld-path-list))))

(defn parse-by-13-orphans-pattern [case]
  {:pre [(>= (tile-num (free-tiles case)) 13)] }
  (let [discard (if (= 14 (tile-weight case)) nil -1)
        meld-path-list (parse-meld-orphans-tree (meld-13-orphans (free-tiles case) discard))]
    (if-not (empty? meld-path-list)
      (map #(parse-13-orphans-path case %) meld-path-list))))

(defn parse-by-honors-and-knitted-pattern [case]
  {:pre [(>= (tile-num (free-tiles case)) 13)]}
  (let [discard (if (= 14 (tile-weight case)) nil -1)
        meld-path-list (parse-meld-orphans-tree (meld-honors-and-knitted (free-tiles case) discard))]
    (if-not (empty? meld-path-list)
      (map #(parse-honors-and-knitted-path case %) meld-path-list))))

(defn parse-hands-win [hands-case draw draw-type]
  "Parse hands case, return parse result on winning, else return nil")

(defprotocol ReadyHands
  (get-ready-tiles [this])
  (ready-type [this])
  (ready-tile? [this tile])
  (get-hands-case [this])
  (complete-what? [this tile]))

(defrecord NormalReadyHands [hands-case parse-result])

(extend-type NormalReadyHands
  CommonComb
  (tile-num [this]
    (tile-num (:hands-case this)))
  (tile-weight [this]
    (tile-weight (:hands-case this)))
  (tile-seq [this]
    (tile-seq (:hands-case this)))
  TileCaseComb
  (chow-seq [this ready]
    (concat (chow-seq (:hands-case this))
            (let [chow-index-list (concat (get-in this [:parse-result :meld :chow])
                                          (get-in this [:parse-result :meld :knitted]))]
              (map (fn [idx]
                     (let [tile-list (map-tile-index (:hands-case this) idx)
                           [t m h] (map #(enum %) (if (= (count idx) *chow-count*)
                                                    tile-list
                                                    (sort-by tile-key (cons ready tile-list))))
                           chow (make-chow t m h (suit-sym (first tile-list)))]
                       (if (= (count idx) *chow-count*)
                         chow
                         (assoc chow :hole ready))))
                   chow-index-list))))
  (pong-seq [this]
    (concat (pong-seq (:hands-case this))
            (let [pong-index-list (get-in this [:parse-result :meld :pong])]
              (map (fn [idx]
                     (let [tile-list (map-tile-index (:hands-case this) idx)
                           pong (make-pong (enum (first tile-list)) (suit-sym (first tile-list)))]
                       (if (= (count idx) *pong-count*)
                         pong
                         (assoc pong :hole (first tile-list)))))
                   pong-index-list))))
  (kong-seq [this]
    (kong-seq (:hands-case this)))
  (pub-kong-seq [this]
    (pub-kong-seq (:hands-case this)))
  (pair-seq [this]
    (let [pair-index-list (get-in this [:parse-result :meld :pair])]
      (map (fn [idx]
             (let [tile-list (map-tile-index (:hands-case this) idx)
                   pair (make-pair (enum (first tile-list)) (suit-sym (first tile-list)))]
               (if (= 2 (count idx))
                 pair
                 (assoc pair :hole (first tile-list)))))
           pair-index-list)))
  (free-tiles [this]
    (free-tiles (:hands-case)))
  (all-comb-seq [this ready]
    (lazy-cat (chow-seq this ready) (pong-seq this) (pub-kong-seq this) (kong-seq this)
              (pair-seq this)))
  ReadyHands
  (ready-type [this] :normal)
  (get-ready-tiles [this]
    (get-in this [:parse-result :ready-for]))
  (ready-tile? [this tile]
    (some #(and (= (enum tile) (enum %)) (= (suit tile) (suit %))) (get-ready-tiles this)))
  (get-hands-case [this]
    (:hands-case this))
  (complete-what? [this tile]
    (if (ready-tile? this tile)
      (let [meld-map (get-in this [:parse-result :meld])]
        (cond (some #(< (count %) 2) (:pair meld-map)) :pair
              (some #(< (count %) *chow-count*) (:chow meld-map)) :chow
              (some #(< (count %) *pong-count*) (:pong meld-map)) :pong
              :else :knitted)))))

(defrecord SevenPairsReadyHands [hands-case parse-result])
(extend-type SevenPairsReadyHands
  CommonComb
  (tile-num [this]
    (tile-num (:hands-case this)))
  (tile-weight [this]
    (tile-weight (:hands-case this)))
  (tile-seq [this]
    (tile-seq (:hands-case this)))
  TileCaseComb
  (chow-seq
    ([this ready] nil)
    ([this] nil))
  (pong-seq [this] nil)
  (kong-seq [this] nil)
  (pub-kong-seq [this] nil)
  (pair-seq [this]
    (let [pair-index-list (get-in this [:parse-result :meld :pair])]
      (map (fn [idx]
             (let [tile-list (map-tile-index (:hands-case this) idx)
                   pair (make-pair (enum (first tile-list)) (suit-sym (first tile-list)))]
               (if (= 2 (count idx))
                 pair
                 (assoc pair :hole (first tile-list)))))
           pair-index-list)))
  (free-tiles [this]
    (free-tiles (:hands-case)))
  (all-comb-seq [this ready]
    (pair-seq this))
  ReadyHands
  (ready-type [this] :seven-pairs)
  (get-ready-tiles [this]
    (get-in this [:parse-result :ready-for]))
  (ready-tile? [this tile]
    (some #(and (= (enum tile) (enum %)) (= (suit tile) (suit %))) (get-ready-tiles this)))
  (get-hands-case [this]
    (:hands-case this))
  (complete-what? [this tile] :pair))

(defrecord ThirteenOrphansReadyHands [hands-case parse-result])
(extend-type ThirteenOrphansReadyHands
  CommonComb
  (tile-num [this]
    (tile-num (:hands-case this)))
  (tile-weight [this]
    (tile-weight (:hands-case this)))
  (tile-seq [this]
    (tile-seq (:hands-case this)))
  TileCaseComb
  (chow-seq
    ([this ready] nil)
    ([this] nil))
  (pong-seq [this] nil)
  (kong-seq [this] nil)
  (pub-kong-seq [this] nil)
  (pair-seq [this]
    (let [pair-index-list (get-in this [:parse-result :meld :pair])]
      (map (fn [idx]
             (let [tile-list (map-tile-index (:hands-case this) idx)
                   pair (make-pair (enum (first tile-list)) (suit-sym (first tile-list)))]
               (if (= 2 (count idx))
                 pair
                 (assoc pair :hole (first tile-list)))))
           pair-index-list)))
  (orphans-seq [this ready]
    (let [pair-index-set (set (flatten (get-in this [:parse-result :meld :pair])))
          orphans-index-list (filter #(not (pair-index-set %))
                                     (flatten (get-in this [:parse-result :meld :orphans])))
          tile-list (if (< (count pair-index-set) 2)
                      (map-tile-index (:hands-case this) orphans-index-list)
                      (sort-by tile-key (cons ready (map-tile-index (:hands-case this) orphans-index-list))))]
      (let [orphan (apply make-orphans (mapcat (fn [x]
                                                 [(enum x) (suit-sym x)])
                                               tile-list))]
        (list (if (< (count pair-index-set) 2)
                orphan
                (assoc orphan :hole ready))))))
  (free-tiles [this]
    (free-tiles (:hands-case)))
  (all-comb-seq [this ready]
    (lazy-cat (orphans-seq this ready) (pair-seq this)))
  ReadyHands
  (ready-type [this] :13-orphans)
  (get-ready-tiles [this]
    (get-in this [:parse-result :ready-for]))
  (ready-tile? [this tile]
    (some #(and (= (enum tile) (enum %)) (= (suit tile) (suit %))) (get-ready-tiles this)))
  (get-hands-case [this]
    (:hands-case this))
  (complete-what? [this tile] :orphan))

(defrecord HonorsAndKnittedReadyHands [hands-case parse-result])
(extend-type HonorsAndKnittedReadyHands
  CommonComb
  (tile-num [this]
    (tile-num (:hands-case this)))
  (tile-weight [this]
    (tile-weight (:hands-case this)))
  (tile-seq [this]
    (tile-seq (:hands-case this)))
  TileCaseComb
  (chow-seq
    ([this ready] nil)
    ([this] nil))
  (pong-seq [this] nil)
  (kong-seq [this] nil)
  (pub-kong-seq [this] nil)
  (pair-seq [this] nil)
  (orphans-seq [this ready]
    (let [orphans-index-list (flatten (get-in this [:parse-result :meld :orphans]))
          tile-list (sort-by tile-key (cons ready (map-tile-index (:hands-case this) orphans-index-list)))]
      (list (assoc (apply make-orphans (mapcat (fn [x]
                                                 [(enum x) (suit-sym x)])
                                               tile-list)) :hole ready))))
  (free-tiles [this]
    (free-tiles (:hands-case)))
  (all-comb-seq [this ready]
    (orphans-seq this ready))
  ReadyHands
  (ready-type [this] :honors-and-knitted)
  (get-ready-tiles [this]
    (get-in this [:parse-result :ready-for]))
  (ready-tile? [this tile]
    (some #(and (= (enum tile) (enum %)) (= (suit tile) (suit %))) (get-ready-tiles this)))
  (get-hands-case [this]
    (:hands-case this))
  (complete-what? [this tile] :orphan))

(defn- encode-ready-hands [ready-hands ready]
  (apply str (cons (name (complete-what? ready-hands ready))
                   (sort (map (fn [comb]
                                (apply str (map #(tile-name %) (tile-seq comb))))
                              (all-comb-seq ready-hands ready))))))

(defn sort-ready-hands [ready-hands]
  (let [hands-list (apply concat (vals ready-hands))]
    (sort-by #(tile-key (first %))
             (mapcat (fn [x]
                       (let [readys (get-ready-tiles x)]
                         (map #(vector % x) readys)))
                     hands-list))))

(defn filter-duplicate-ready-hands [ready-hands]
  "Filter out duplicate ready hands, return [[ready-tile ReadyHands], ...]"
  (let [hands-list (apply concat (vals ready-hands))]
    (sort-by #(tile-key (first %))
             (vals (into {} (mapcat (fn [x]
                                      (let [readys (get-ready-tiles x)]
                                        (map #(vector (encode-ready-hands x %) [% x]) readys)))
                                    hands-list))))))

(defn make-ready-hands [hands-case parse-result result-type]
  (cond (= :normal result-type) (->NormalReadyHands hands-case parse-result)
        (= :seven-pairs result-type) (->SevenPairsReadyHands hands-case parse-result)
        (= :honors-and-knitted result-type) (->HonorsAndKnittedReadyHands hands-case parse-result)
        (= :13-orphans result-type) (->ThirteenOrphansReadyHands hands-case parse-result)))

(defn parse-hands-ready [hands-case]
  {:pre [(= 13 (tile-weight hands-case))]}
  "Parse hands case, return parse result on ready, else return nil"
  (let [free-num (tile-num (free-tiles hands-case))
        ready-map
        (into {} (filter
                  #(second %)
                  (list [:normal (parse-by-normal-pattern hands-case)]
                        (if (= 13 free-num)
                          (some #(if (second %) %) [[:seven-pairs (parse-by-seven-pairs-pattern hands-case)]
                                                    [:honors-and-knitted (parse-by-honors-and-knitted-pattern hands-case)] 
                                                    [:13-orphans (parse-by-13-orphans-pattern hands-case)]])))))]
    (if-not (empty? ready-map)
      (into {} (map (fn [x]
                      (let [[ready-type result-seq] x]
                        [ready-type (map #(make-ready-hands hands-case % ready-type)
                                         result-seq)]))
                    ready-map)))))

;; (parse-meld-normal-tree (let [x (free-tiles (mahjong.dl/build-tile-case-from-ast (mahjong.dl/parse-dl-string "2147t1258w369b111f")))]
;;                           (meld-normal x {:pair 1 :triplets 1} 1 nil '((1 2 3) (4 6 7) (8 9 10)))))

;; (parse-meld-normal-tree (let [x (free-tiles (mahjong.dl/build-tile-case-from-ast (mahjong.dl/parse-dl-string "1112345678999t")))]
;;                           (meld-normal x {:pair 1 :triplets 4} 1 nil [])))

;; (let [x (free-tiles (mahjong.dl/build-tile-case-from-ast (mahjong.dl/parse-dl-string "1258w147t111b369b")))]
;;       (meld-knitted x {:pair 1 :triplets 4} 1 -1))

;; (let [x (free-tiles (mahjong.dl/build-tile-case-from-ast (mahjong.dl/parse-dl-string "11223355778899w")))]
;;       (meld-seven-pairs x))

;; (let [x (free-tiles (mahjong.dl/build-tile-case-from-ast (mahjong.dl/parse-dl-string "179w19t19b1234f123j")))]
;;      (meld-13-orphans x))

;; (let [x (free-tiles (mahjong.dl/build-tile-case-from-ast (mahjong.dl/parse-dl-string "7w258t369b1234f123j")))]
;;      (meld-honors-and-knitted x))

; (tile-name #mahjong.tile.FengTile{:enum 3})

; (parse-by-normal-pattern (mahjong.dl/build-tile-case-from-ast (mahjong.dl/parse-dl-string "111t^234t^567t^999t^8t")))


;(parse-by-normal-pattern (mahjong.dl/build-tile-case-from-ast (mahjong.dl/parse-dl-string "58w122247t111b369b")))
;(parse-by-normal-pattern (mahjong.dl/build-tile-case-from-ast (mahjong.dl/parse-dl-string "111t^444f^78999w11b9w")))
; (parse-by-normal-pattern (mahjong.dl/build-tile-case-from-ast (mahjong.dl/parse-dl-string "111t^444f^78999w11b9w")))
;(parse-by-normal-pattern (mahjong.dl/build-tile-case-from-ast (mahjong.dl/parse-dl-string "11112345678999b")))

;(parse-by-seven-pairs-pattern (mahjong.dl/build-tile-case-from-ast (mahjong.dl/parse-dl-string "11w112244t11113b1f")))
;(parse-by-seven-pairs-pattern (mahjong.dl/build-tile-case-from-ast (mahjong.dl/parse-dl-string "11w112244t11113b")))

; (parse-by-13-orphans-pattern (mahjong.dl/build-tile-case-from-ast (mahjong.dl/parse-dl-string "119w19t19b1234f123j")))
;(parse-by-13-orphans-pattern (mahjong.dl/build-tile-case-from-ast (mahjong.dl/parse-dl-string "19w19t19b1234f123j")))

;(parse-by-honors-and-knitted-pattern (mahjong.dl/build-tile-case-from-ast (mahjong.dl/parse-dl-string "147w258t369b12344f")))
;(parse-by-honors-and-knitted-pattern (mahjong.dl/build-tile-case-from-ast (mahjong.dl/parse-dl-string "147w5t69b1234f123j")))

;(parse-hands-ready (mahjong.dl/build-tile-case-from-ast (mahjong.dl/parse-dl-string "1122335566788b")))
 
;(parse-hands-ready (mahjong.dl/build-tile-case-from-ast (mahjong.dl/parse-dl-string "119w19t19b123f123j")))
;(parse-hands-ready (mahjong.dl/build-tile-case-from-ast (mahjong.dl/parse-dl-string "777w^6666t-55567b77t")))
