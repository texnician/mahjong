(ns mahjong.tile)

(def ^:dynamic ^:const *tile-char-table*
  {:bing 0x1f019
   :tiao 0x1f010
   :wan 0x1f007
   :feng 0x1f000
   :jian 0x1f004})

(defprotocol CommonTiles
  "Protocol for common tiles"
  (enum [this])
  (suit [this])
  (tile-name [this])
  (pre [this])
  (succ [this])
  (char-code [this])
  (back-char-code [this]))

(extend-protocol CommonTiles
  nil
  (enum [this]
    nil)
  (suit [this]
    nil)
  (tile-name [this]
    "Invalid tile")
  (pre [this]
    nil)
  (succ [this]
    nil))

(defmacro def-basic-tile [suit-name min-enum max-enum]
  (let [record-name (symbol (str (clojure.string/capitalize suit-name) "Tile"))
        suit-key (keyword (clojure.string/lower-case suit-name))
        suit-sym (symbol (subs (str record-name) 0 1))]
    `(do
       (defrecord ~record-name [~'enum])
       (extend-protocol CommonTiles
         ~record-name
         (enum [this]
           (:enum this))
         (suit [this]
           ~suit-key)
         (tile-name [this]
           (str (:enum this) '~suit-sym)))
       (defn ~(symbol (format "make-%s-tile" suit-name)) [~'enum]
         {:pre [(>= ~'enum ~min-enum) (<= enum ~max-enum)]}
         (~(symbol (str "->" record-name)) ~'enum)))))

(defmacro extend-protocol-on-records [protocol records & body]
  `(do
     ~@(map (fn [record]
              `(extend ~record
                 ~protocol
                 (merge (get-in ~protocol [:impls ~record])
                        ~(into {} (map (fn [x]
                                         (let [[tag & rest] x]
                                           [(keyword tag) (cons 'fn rest)]))
                                       body))))) records)))

(defrecord BingTile [enum])

(extend-protocol CommonTiles
  BingTile
  (enum [this]
    (:enum this))
  (suit [this]
    :bing)
  (tile-name [this]
    (str (:enum this) 'B)))

(defn make-bing-tile [enum]
  {:pre [(>= enum 1) (<= enum 9)]}
  (->BingTile enum))

(defrecord TiaoTile [enum])

(extend-protocol CommonTiles
  TiaoTile
  (enum [this]
    (:enum this))
  (suit [this]
    :tiao)
  (tile-name [this]
    (str (:enum this) 'T)))

(defn make-tiao-tile [enum]
  {:pre [(>= enum 1) (<= enum 9)]}
  (->TiaoTile enum))

(defrecord WanTile [enum])

(extend-protocol CommonTiles
  WanTile
  (enum [this]
    (:enum this))
  (suit [this]
    :wan)
  (tile-name [this]
    (str (:enum this) 'W)))

(defn make-wan-tile [enum]
  {:pre [(>= enum 1) (<= enum 9)]}
  (->WanTile enum))

(extend-protocol-on-records
 CommonTiles [BingTile TiaoTile WanTile]
 (pre [this]
      (let [e (:enum this)]
        (if (= e 1)
        nil
        (- e 1))))
 (succ [this]
       (let [e (:enum this)]
         (if (= e 9)
           nil
           (+ e 1)))))

(defrecord FengTile [enum])

(extend-protocol CommonTiles
  FengTile
  (enum [this]
    (:enum this))
  (suit [this]
    :feng)
  (tile-name [this]
    (let [e (:enum this)]
      (cond (= 1 e) 'Dong
            (= 2 e) 'Nan
            (= 3 e) 'Xi
            (= 4 e) 'Bei
            :else 'Error)))
  (pre [this]
    (let [e (:enum this)]
      (if (= e 1)
        nil
        (- e 1))))
  (succ [this]
    (let [e (:enum this)]
      (if (= e 4)
        nil
        (+ e 1)))))

(defn make-feng-tile [enum]
  {:pre [(>= enum 1) (<= enum 4)]}
  (->FengTile enum))

(defrecord JianTile [enum])

(extend-protocol CommonTiles
  JianTile
  (enum [this]
    (:enum this))
  (suit [this]
    :jian)
  (tile-name [this]
    (let [e (:enum this)]
      (cond (= 1 e) 'Zhong
            (= 2 e) 'Fa
            (= 3 e) 'Bai
            :else 'Error)))
  (pre [this]
    (let [e (:enum this)]
      (if (= e 1)
        nil
        (- e 1))))
  (succ [this]
    (let [e (:enum this)]
      (if (= e 3)
        nil
        (+ e 1)))))

(defn make-jian-tile [enum]
  {:pre [(>= enum 1) (<= enum 3)]}
  (->JianTile enum))

(defrecord HuaTile [enum])

(extend-protocol-on-records
 CommonTiles [BingTile TiaoTile WanTile FengTile JianTile]
 (char-code [this]
            (let [e (:enum this)]
              (+ -1 e ((suit this) *tile-char-table*))))
 (back-char-code [this]
                 0x1f02b))

(defn make-tile [enum suit-sym]
  "Make a tile recored, suit-sym is a case insensitive category symbol(B, T, W, F, J)."
  (let [suit (symbol (clojure.string/upper-case suit-sym))]
    (cond (= suit 'B) (make-bing-tile enum)
          (= suit 'T) (make-tiao-tile enum)
          (= suit 'W) (make-wan-tile enum)
          (= suit 'F) (make-feng-tile enum)
          (= suit 'J) (make-jian-tile enum)
          :else (assert false (format "'%s' is not a valid tile category" suit)))))

(def ^:dynamic ^:const *category-order* {:bing 2 :tiao 1 :wan 0 :feng 3 :jian 4})

(defn compare-tile [a b]
  (let [c1 (suit a)
        c2 (suit b)]
    (if (not= c1 c2)
      (< (c1 *category-order*) (c2 *category-order*))
      (< (enum a) (enum b)))))

(defn tile-key [a]
  [((suit a) *category-order*) (enum a)])

(defn simple? [a]
  (#{:wan :bing :tiao} (suit a)))

(defn honor? [a]
  (not (simple? a)))

(defn terminal-or-honor? [tile]
  (cond ((suit tile) #{:wan :tiao :bing}) (contains? #{1 9} (enum tile))
        ((suit tile) #{:feng :jian}) true
        :else false))

(defn suit-sym [tile]
  (cond (simple? tile) (symbol (clojure.string/upper-case (subs (name (suit tile)) 0 1)))
        (= (suit tile) :feng) 'F
        (= (suit tile) :jian) 'J
        :else nil))

(defn green-color? [tile]
  (cond (= (suit tile) :tiao) (#{2 3 4 6 8} (enum tile))
        (= (suit tile) :jian) (= 2 (enum tile))
        :else false))

(defn symmetric-tile? [tile]
  (cond (= (suit tile) :tiao) (#{2 4 5 6 8 9} (enum tile))
        (= (suit tile) :bing) (#{1 2 3 4 5 8 9} (enum tile))
        (= (suit tile) :jian) (= 3 (enum tile))
        :else false))

(defn one-suit? [tile-list]
  (let [suits (distinct (map #(suit %) tile-list))]
    (if (= 1 (count suits)) (first suits))))