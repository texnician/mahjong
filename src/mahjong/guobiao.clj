(ns mahjong.guobiao
  (:use (mahjong tile comb))
  (:require (clojure set)))

;; http://en.wikipedia.org/wiki/Guobiao_Majiang
;; 2 Points
;;     2.3 48 points
;;         2.3.1 Four same sequences
;;         2.3.2 Four step triplets
;;     2.4 32 points
;;         2.4.1 Four step sequences
;;         2.4.2 Three quads
;;         2.4.3 All terminals or honors
;;     2.5 24 points
;;         2.5.1 Seven pairs
;;         2.5.2 Seven honors and knitted
;;         2.5.3 All even
;;         2.5.4 One suit only
;;         2.5.5 Three same sequences
;;         2.5.6 Three step triplets
;;         2.5.7 Large three only
;;         2.5.8 Medium three only
;;         2.5.9 Small three only
;;     2.6 16 points
;;         2.6.1 One suit through
;;         2.6.2 Three suits edge sequences plus center pair
;;         2.6.3 Three step sequences
;;         2.6.4 Number 5 in each set
;;         2.6.5 Three suits triplets
;;         2.6.6 Three closed triplets
;;     2.7 12 points
;;         2.7.1 Honors and knitted
;;         2.7.2 Knitted through
;;         2.7.3 More than five
;;         2.7.4 Less than five
;;         2.7.5 Three winds
;;     2.8 8 points
;;         2.8.1 Three suits through
;;         2.8.2 Symmetric tiles only
;;         2.8.3 Three suits sequences
;;         2.8.4 Three suits step triplets
;;         2.8.5 Avoid points
;;         2.8.6 Last drawn tile
;;         2.8.7 Last discarded tile
;;         2.8.8 Supplemental tile of melding quad
;;         2.8.9 Appended tile to melded triplet
;;     2.9 6 points
;;         2.9.1 All triplets
;;         2.9.2 One suit plus honors
;;         2.9.3 Three suits step sequences
;;         2.9.4 Five types
;;         2.9.5 Others' tiles in each set
;;         2.9.6 Two closed quad
;;         2.9.7 Two dragon triplets
;;     2.10 4 points
;;         2.10.1 Terminals or honors in each set
;;         2.10.2 Self tiles only
;;         2.10.3 Two open quads
;;         2.10.4 Last tile other than revealed
;;     2.11 2 points
;;         2.11.1 Dragon triplet
;;         2.11.2 Prevailing wind triplet
;;         2.11.3 Game wind triplet
;;         2.11.4 No melding
;;         2.11.5 Simple sequence hand
;;         2.11.6 Four tiles collection
;;         2.11.7 Two suits triplets
;;         2.11.8 Two closed triplets
;;         2.11.9 Closed quad
;;         2.11.10 All simples
;;     2.12 1 point
;;         2.12.1 Two same sequences
;;         2.12.2 Two suits sequences
;;         2.12.3 Chain six
;;         2.12.4 Edge sequences pair
;;         2.12.5 Terminal or non-special wind triplet
;;         2.12.6 Open quad
;;         2.12.7 Lack of one suit
;;         2.12.8 No honor
;;         2.12.9 One tile wait for a edge sequence
;;         2.12.10 One tile wait for a holed sequence
;;         2.12.11 One tile wait for a pair
;;         2.12.12 Completion by draw
;;         2.12.13 Flower tile

(def ^:dynamic *prevailing-wind* 1)
(def ^:dynamic *game-wind* 1)

(defmacro deffan [fan points meta-info arg-list & body]
  (let [key (keyword fan)
        exclude-keys (vec (map #(keyword %) (:exclude meta-info)))
        part-exclude-keys (vec (map #(keyword (first %)) (partition 2 (:part-exclude meta-info))))
        part-exclude-vals (vec (map #(second %) (partition 2 (:part-exclude meta-info))))
        pred (gensym "pred__")
        pred-ret (gensym "m__")]
    `(defn ~fan
       {:key ~key
        :points ~points
        :exclude ~exclude-keys
        :part-exclude ~(zipmap part-exclude-keys part-exclude-vals)}
       ~arg-list
       (letfn [(~pred []
                 ~@body)]
         (if-let [~pred-ret (~pred)]
           (if (> ~pred-ret 0)
             [~key (* ~pred-ret ~points)]))))))

;; 大四喜
(deffan big-four-winds 88
  {:exclude [three-winds-triplets all-triplets game-wind-triplet
             prevailing-wind-triplet terminal-or-non-special-wind-triplet
             little-four-winds]}
  [hands ready]
  (let [triplet-seq (concat (pong-seq hands) (kong-seq hands) (pub-kong-seq hands))]
    (if (and (every? #(= (suit %) :feng) (map #(get-tile %) triplet-seq))
             (= 4 (count (distinct (map #(enum (get-tile %)) triplet-seq)))))
      1 0)))

;; 大三元
(deffan big-three-dragons 88 {:exclude [little-three-dragons
                                        dragon-triplet
                                        two-dragon-triplets]}
  [hands ready]
  (let [triplet-seq (concat (pong-seq hands) (kong-seq hands) (pub-kong-seq hands))]
    (if (= 3 (count (distinct (map #(enum %) (filter #(= :jian (suit %))
                                                     (map #(get-tile %) triplet-seq))))))
      1 0)))

;; 绿一色
(deffan all-green 88 {}
  [hands ready]
  (if (every? #(green-color? %) (tile-seq hands))
    1 0))

;; 九莲宝灯
(deffan nine-gates 88 {:part-exclude [terminal-or-non-special-wind-triplet 1]}
  [hands ready]
  (let [case (get-hands-case hands)]
    (if (and (one-suit? (tile-seq hands))
             (= "1112345678999" (apply str (map #(enum %) (-> case free-tiles tile-seq)))))
      1 0)))

;; 四杠
(deffan four-quads 88 {:exclude [open-quad closed-quad two-open-quads three-quads
                                 all-triplets one-tile-wait-for-a-pair]}
  [hands ready]
  (if (= 4 (count (concat (kong-seq hands) (pub-kong-seq hands))))
    1 0))

;; 连七对
(deffan chained-seven-pairs 88 {:exclude [seven-pairs
                                          one-suit-only
                                          no-melding
                                          one-tile-wait-for-a-pair
                                          no-honor]}
  [hands ready]
  (if (and (= :seven-pairs (ready-type hands))
           (one-suit? (tile-seq hands))
           (every? #(= -1 %)
                   (map #(apply - %)
                        (partition 2 1 (map #(enum (get-tile %))
                                            (pair-seq hands))))))
    1 0))

;; 十三幺
(deffan thirteen-orphans 88 {:exclude [five-types
                                       no-melding
                                       one-tile-wait-for-a-pair]}
  [hands ready]
  (if (= :13-orphans (ready-type hands)) 1 0))

;; 清幺九
(deffan all-terminals 64 {:exclude [all-triplets
                                    terminals-or-honors-in-each-set
                                    terminal-or-non-special-wind-triplet
                                    no-honor]}
  [hands ready]
  (if (and (= :normal (ready-type hands))
           (every? #(and (simple? %) (#{1 9} (enum %))) (tile-seq hands)))
    1 0))

;; 小四喜
(deffan little-four-winds 64 {:exclude [three-winds-triplets]
                              :part-exclude [terminal-or-non-special-wind-triplet 3]}
  [hands ready]
  (if (and (= :normal (ready-type hands))
           (let [triplet-seq (concat (pong-seq hands)
                                     (kong-seq hands)
                                     (pub-kong-seq hands))
                 pair-tile (get-tile (first (pair-seq hands)))
                 feng-set (set (map #(enum (get-tile %))
                                      (filter #(= :feng (-> % get-tile suit)) triplet-seq)))]
             (and (= :feng (suit pair-tile))
                  (= 3 (count feng-set))
                  (= (enum pair-tile)
                     (first (clojure.set/difference #{1 2 3 4} feng-set))))))
    1 0))

;; 小三元
(deffan little-three-dragons 64 {:exclude [two-dragon-triplets
                                           dragon-triplet]}
  [hands ready]
  (let [triplet-seq (concat (pong-seq hands) (kong-seq hands) (pub-kong-seq hands))
        pair-tile (get-tile (first (pair-seq hands)))
        jian-set (set (map #(enum %) (filter #(= :jian (suit %))
                                             (map #(get-tile %) triplet-seq))))]
    (if (and (= :normal (ready-type hands))
             (= 2 (count jian-set))
             (= :jian (suit pair-tile))
             (= (enum pair-tile)
                (first (clojure.set/difference #{1 2 3} jian-set))))
      1 0)))

;; 字一色
(deffan all-honors 64 {:exclude [all-triplets
                                 terminal-or-non-special-wind-triplet
                                 terminals-or-honors-in-each-set]}
  [hands ready]
  (if (every? #(honor? %) (tile-seq hands))
    1 0))

;; 四暗刻
(deffan all-closed-triplets 64 {:exclude [all-triplets
                                          no-melding
                                          three-closed-triplets
                                          two-closed-triplets]}
  [hands ready]
  (let [triplet-seq (concat (pong-seq hands) (kong-seq hands))]
    (if (and (= 4 (count triplet-seq))
             (every? #(not (pub %)) triplet-seq))
      1 0)))

;;; 一色双龙会
(deffan twin-edge-sequences-plus-center-pair 64 {:exclude [one-suit-only
                                                           simple-sequence-hand
                                                           no-honor
                                                           edge-sequences-pair
                                                           two-same-sequences]}
  [hands ready]
  (let [chows (chow-seq hands ready)
        pair-tile (get-tile (first (pair-seq hands)))]
    (if (and (= :normal (ready-type hands))
             (= (enum pair-tile) 5)
             (one-suit? (tile-seq hands))
             (= (count chows) 4)
             (= "1177" (apply str (sort (map (fn [x]
                                               (enum (get-tile x 0)))
                                             chows)))))
      1 0)))

;; 一色四同顺
(deffan four-same-sequences 32 {:exclude [four-tiles-collection]}
  [hands ready]
  (let [chows (chow-seq hands ready)
        ]))

;;         2.3.2 Four step triplets

(def ^:dynamic *guobiao-fans*
  '[big-four-winds
    big-three-dragons
    all-green
    nine-gates
    four-quads
    chained-seven-pairs
    thirteen-orphans
    all-terminals
    little-four-winds
    little-three-dragons
    all-honors
    all-closed-triplets
    twin-edge-sequences-plus-center-pair])

(defn fan-meta [func]
  (meta (resolve func)))

(defn apply-fan [func & args]
  (apply (resolve func) args))

(defn calculate-fan [hands ready]
  (letfn
      [(sieve [cur-fan fans]
         (let [excludes (:exclude (fan-meta cur-fan))]
           (filter (fn [x]
                     (let [xfm (fan-meta x)]
                       (not (some #(= (:key xfm) %) excludes))))
                   fans)))
       (iter [fans]
         (if (empty? fans)
           {}
           (let [fm (fan-meta (first fans))
                 cur-fan (apply-fan (first fans) hands ready)]
             (if cur-fan
               (let [succ-fans (iter (sieve (first fans) (rest fans)))]
                 (into (let [part-excludes (:part-exclude fm)]
                         (loop [e part-excludes r succ-fans]
                           (if-not (empty? e)
                             (let [[k v] (first e)]
                               (cond (not (contains? r k)) (recur (rest e) r)
                                     (> (k r) v) (recur (rest e) (assoc r k (- (k r) v)))
                                     :else (recur (rest e) (dissoc r k))))
                             r)))
                       (list cur-fan)))
               (iter (rest fans))))))]
    (if-let [result (iter *guobiao-fans*)]
      (sort-by #(second %) (fn [a b]
                             (> a b)) result))))

(defn- test [instr]
  (let [case (mahjong.dl/build-tile-case-from-ast (mahjong.dl/parse-dl-string instr))
        results (filter-duplicate-ready-hands (parse-hands-ready case))]
    (map (fn [x]
           (let [[r h] x]
             [r (calculate-fan h r)]))
         results)))

;(test "2344466688t222j")
;(test "1111f^2222f-3333f-4444f-3j")
;(test "1122334455677b")
;(test "99w19b19t1234f123j")
;(test "111j222j333j56t33b")
;(test "1112345678999t")
;(test "111122334f1122j")
;(test "111122334f1122j")
;(test "11223355778899w")
