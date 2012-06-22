(ns mahjong.guobiao
  (:use (mahjong comb tile guobiao-util))
  (:require (clojure set)))

;; http://en.wikipedia.org/wiki/Guobiao_Majiang
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
(deffan nine-gates 88 {:exclude [one-suit-only
                                 no-melding
                                 no-honor]
                       :part-exclude [terminal-or-non-special-wind-triplet 1]}
  [hands ready]
  (let [case (get-hands-case hands)]
    (if (and (one-suit? (tile-seq hands))
             (= "1112345678999" (apply str (map #(enum %) (-> case free-tiles tile-seq)))))
      1 0)))

;; 四杠
(deffan four-quads 88 {:exclude [open-quad
                                 two-open-quads
                                 three-quads
                                 all-triplets
                                 one-tile-wait-for-a-pair]}
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
           (every? #(and (simple? %) (#{1 9} (enum %))) (cons ready (tile-seq hands))))
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
                                          two-closed-triplets
                                          two-closed-quads]}
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
(deffan four-same-sequences 48 {:exclude [four-tiles-collection
                                          three-same-sequences
                                          two-same-sequences]}
  [hands ready]
  (let [chows (chow-seq hands ready)]
    (if (and (= 4 (count chows))
             (one-suit? (mapcat #(tile-seq %) chows))
             (= 1 (count (distinct (map #(head-enum %) chows)))))
      1 0)))

;; 一色四节高
(deffan four-step-triplets 48 {:exclude [all-triplets
                                         three-step-triplets]}
  [hands ready]
  (let [pongs (concat (pong-seq hands) (kong-seq hands) (pub-kong-seq hands))]
    (if (and (= 4 (count pongs))
             (one-suit? (mapcat #(tile-seq %) pongs))
             (step-increase? (sort (map #(-> % get-tile enum) pongs)) 1))
      1 0)))

;; 一色四步高
(deffan four-step-sequences 32 {:exclude [three-step-sequences
                                          edge-sequences-pair
                                          chain-six]}
  [hands ready]
  (let [chows (chow-seq hands ready)]
    (if (and (= 4 (count chows))
             (one-suit? (mapcat #(tile-seq %) chows))
             (or (step-increase? (sort (map #(head-enum %) chows)) 1)
                 (step-increase? (sort (map #(head-enum %) chows)) 2)))
      1 0)))

;; 三杠
(deffan three-quads 32 {:exclude [open-quad
                                  two-open-quads]}
  [hands ready]
  (if (= 3 (count (concat (kong-seq hands) (pub-kong-seq hands))))
    1 0))

;; 混幺九
(deffan all-terminals-or-honors 32 {:exclude [all-triplets
                                              terminals-or-honors-in-each-set
                                              terminal-or-non-special-wind-triplet]}
  [hands ready]
  (if (and (= :normal (ready-type hands))
           (not (every? #(simple? %) (cons ready (tile-seq hands))))
           (every? (fn [x]
                     (cond (#{:feng :jian} (suit x)) true
                           :else (#{1 9} (enum x))))
                   (cons ready (tile-seq hands))))
    1 0))

;; 七对
(deffan seven-pairs 24 {:exclude [no-melding
                                  one-tile-wait-for-a-pair]}
  [hands ready]
  (if (= :seven-pairs (ready-type hands))
    1 0))

;; 七星不靠
(deffan seven-honors-and-knitted 24 {:exclude [no-melding
                                               five-types
                                               honors-and-knitted]}
  [hands ready]
  (if (and (= :honors-and-knitted (ready-type hands))
           (= 4 (count (filter #(= :feng (suit %)) (cons ready (tile-seq hands)))))
           (= 3 (count (filter #(= :jian (suit %)) (cons ready (tile-seq hands))))))
    1 0))

;; 全双刻
(deffan all-even 24 {:exclude [all-triplets
                               all-simples]}
  [hands ready]
  (let [pongs (concat (pong-seq hands) (kong-seq hands) (pub-kong-seq hands))]
    (if (and (= 4 (count pongs))
             (every? #(-> % enum even?) (cons ready (tile-seq hands))))
      1 0)))

;; 清一色
(deffan one-suit-only 24 {:exclude [no-honor]}
  [hands ready]
  (if (one-suit? (cons ready (tile-seq hands)))
    1 0))

;; 一色三同顺
(deffan three-same-sequences 24 {:exclude [two-same-sequences]}
  [hands ready]
  (let [mp (group-combs-by #(comb-suit %) (chow-seq hands ready))
        suit-chows (first (filter #(= 3 (count %)) (vals mp)))]
    (if (and (not (empty? suit-chows))
             (= 1 (count (distinct (map #(head-enum %) suit-chows)))))
      1 0)))

;; 一色三节高
(deffan three-step-triplets 24 {:exclude []}
  [hands ready]
  (let [mp (group-combs-by #(comb-suit %) (concat (pong-seq hands)
                                                  (kong-seq hands)
                                                  (pub-kong-seq hands)))
        suit-pongs (first (filter #(= 3 (count %)) (vals mp)))]
    (if (and (not (empty? suit-pongs))
             (simple? (get-tile (first suit-pongs)))
             (step-increase? (sort (map #(-> % get-tile enum) suit-pongs)) 1))
      1 0)))

;; 全大
(deffan large-three-only 24 {:exclude [no-honor
                                       more-than-five]}
  [hands ready]
  (if (and (every? #(simple? %) (cons ready (tile-seq hands)))
           (every? #(>= (enum %) 7) (cons ready (tile-seq hands))))
    1 0))

;; 全中
(deffan medium-three-only 24 {:exclude [all-simples
                                        no-honor]}
  [hands ready]
  (if (and (every? #(simple? %) (cons ready (tile-seq hands)))
           (every? #(and (>= (enum %) 4)
                         (<= (enum %) 6)) (cons ready (tile-seq hands))))
    1 0))

;; 全小
(deffan small-three-only 24 {:exclude [no-honor
                                       less-than-five]}
  [hands ready]
  (if (and (every? #(simple? %) (cons ready (tile-seq hands)))
           (every? #(<= (enum %) 3) (cons ready (tile-seq hands))))
    1 0))

;; 清龙
(deffan one-suit-through 16 {:exclude []}
  [hands ready]
  (let [mp (group-combs-by #(comb-suit %) (chow-seq hands ready))
        suit-chows (first (filter #(>= (count %) 3) (vals mp)))]
    (if (and (not (empty? suit-chows))
             (clojure.set/subset? #{1 4 7} (set (map #(tail-enum %) suit-chows))))
      (with-comb-consumed [:chow [(comb-suit (first suit-chows)) [1 4 7]]] 1) 0)))

;; 三色双龙会
(deffan three-suits-edge-sequences-plus-center-pair 16 {:exclude [simple-sequence-hand
                                                                  two-suits-sequences
                                                                  no-honor]}
  [hands ready]
  (let [mp (group-combs-by #(comb-suit %) (chow-seq hands ready))
        pair-tile (get-tile (first (pair-seq hands)))]
    (if (and (= :normal (ready-type hands))
             (= 2 (count mp))
             (every? (fn [x]
                       (clojure.set/subset? #{1 7} (set (map #(tail-enum %) x))))
                     (vals mp))
             (not (contains? mp (suit pair-tile)))
             (simple? pair-tile)
             (= 5 (enum pair-tile)))
      1 0)))

;; 一色三步高
(deffan three-step-sequences 16 {:exclude []}
  [hands ready]
  (let [mp (group-combs-by #(comb-suit %) (chow-seq hands ready))
        suit-chows (first (filter #(>= (count %) 3) (vals mp)))]
    (if (not (empty? suit-chows))
      (let [[s & _] (mapcat (fn [x]
                              (get-step-sub-sequence x 3 (distinct (map #(tail-enum %) suit-chows))))
                            '(1 2))]
        (if s
          (with-comb-consumed [:chow [(comb-suit (first suit-chows)) s]] 1))))))

;; 全带五
(deffan number-5-in-each-set 16 {:exclude [all-simples]}
  [hands ready]
  (let [all-combs (all-comb-seq hands ready)]
    (if (and (= :normal (ready-type hands))
             (every? (fn [comb]
                       (some #(= 5 (enum %)) (tile-seq comb)))
                     all-combs))
      1 0)))

;; 三同刻
(deffan three-suits-triplets 16 {:exclude [two-suits-triplets]}
  [hands ready]
  (let [pongs (concat (pong-seq hands) (kong-seq hands) (pub-kong-seq hands))
        mp (select-keys (group-combs-by #(comb-suit %) pongs) [:wan :tiao :bing])]
    (if (and (= (count mp) 3)
             (not (empty? (reduce clojure.set/intersection (map (fn [x]
                                                                  (set (map #(-> % get-tile enum) x)))
                                                                (vals mp))))))
      1 0)))

;; 三暗刻
(deffan three-closed-triplets 16 {:exclude [two-closed-triplets
                                            two-closed-quads]}
  [hands ready]
  (let [pongs (concat (pong-seq hands) (kong-seq hands) (pub-kong-seq hands))]
    (if (= 3 (count (filter #(not (pub %)) pongs)))
      1 0)))

;; 全不靠
(deffan honors-and-knitted 12 {:exclude [five-types
                                         no-melding]}
  [hands ready]
  (if (= :honors-and-knitted (ready-type hands))
    1 0))

;; 组合龙
(deffan knitted-through 12 {:exclude []}
  [hands ready]
  (if (cond (= :normal (ready-type hands)) (some #(= 3 (- (mid-enum %) (tail-enum %)))
                                                 (chow-seq hands ready))
            (= :honors-and-knitted (ready-type hands)) (= 9 (count (filter #(simple? %) (cons ready (tile-seq hands)))))
            :else nil)
    1 0))

;; 大于五
(deffan more-than-five 12 {:excludes [no-honor]}
  [hands ready]
  (if (every? #(and (simple? %) (> (enum %) 5)) (cons ready (tile-seq hands)))
    1 0))

;;; 小于五
(deffan less-than-five 12 {:exclude [no-honor]}
  [hands ready]
  (if (every? #(and (simple? %) (< (enum %) 5)) (cons ready (tile-seq hands)))
    1 0))

;; 三风刻
(deffan three-winds-triplets 12 {:exclude []}
  [hands ready]
  (if (= 3 (count (filter #(= :feng (comb-suit %)) (concat (pong-seq hands)
                                                           (kong-seq hands)
                                                           (pub-kong-seq hands)))))
    1 0))

;; 花龙
(deffan three-suits-through 8 {:exclude []}
  [hands ready]
  (let [chows (chow-seq hands ready)
        mp (group-combs-by #(comb-suit %) chows)]
    (if (= 3 (count mp))
      (let [chow-sets (sorted-chow-sets (vals mp))]
        (if (empty? (reduce clojure.set/difference #{1 4 7} chow-sets))
          (let [[a b & _] (filter #(= 1 (count (second %))) mp)
                a-enum (-> a second first tail-enum)
                a-suit (-> a first)
                b-suit (-> b first)
                b-enum (-> b second first tail-enum)
                c-enum (first (clojure.set/difference #{1 4 7} #{a-enum b-enum}))
                c-suit (first (clojure.set/difference #{:wan :tiao :bing} #{a-suit b-suit}))]
            (with-comb-consumed [:chow [a-suit [a-enum]
                                        b-suit [b-enum]
                                        c-suit [c-enum]]] 1))
          0))
      0)))

;; 推不倒
(deffan symmetric-tiles-only 8 {:exclude [lack-one-suit]}
  [hands ready]
  (if (every? #(symmetric-tile? %) (cons ready (tile-seq hands)))
    1 0))

;; 三色三同顺
(deffan three-suits-sequences 8 {:exclude []}
  [hands ready]
  (let [chows (chow-seq hands ready)
        mp (group-combs-by #(comb-suit %) chows)]
    (if (= 3 (count mp))
      (let [chow-sets (sorted-chow-sets (vals mp))
            e (first (reduce clojure.set/intersection chow-sets))]
        (if e
          (with-comb-consumed [:chow [:wan [e]
                                      :tiao [e]
                                      :bing [e]]] 1) 0))
      0)))

;; 三色三节高
(deffan three-suits-step-triplets 8 {:exclude []}
  [hands ready]
  (let [pongs (concat (pong-seq hands) (kong-seq hands) (pub-kong-seq hands))
        mp (select-keys (group-combs-by #(comb-suit %) pongs) [:wan :tiao :bing])]
    (if (and (= (count mp) 3)
             (get-step-sub-sequence 1 3 (distinct (sort (mapcat (fn [x]
                                                                  (map #(-> % get-tile enum) x))
                                                                (vals mp))))))
      1 0)))

;;; 双暗杠
(deffan two-closed-quads 8 {:exclude []}
  [hands ready]
  (if (= 2 (count (kong-seq hands)))
    1 0))

;; 碰碰胡
(deffan all-triplets 6 {:exclude []}
  [hands ready]
  (let [pongs (concat (pong-seq hands) (kong-seq hands) (pub-kong-seq hands))]
    (if (= 4 (count pongs)) 1)))

;; 混一色
(deffan one-suit-plus-honors 6 {:exclude []}
  [hands ready]
  (let [simples (filter #(simple? %) (cons ready (tile-seq hands)))]
    (if (and (one-suit? simples)
             (< (count simples) 14))
      1)))

;; 三色三步高
(deffan three-suits-step-sequences 6 {:exclude []}
  [hands ready]
  (let [chows (chow-seq hands ready)
        mp (group-combs-by #(comb-suit %) chows)]
    (if (= 3 (count mp))
      (let [two-chow (some (fn [x] (if (= 2 (count (second x))) x)) mp)]
        (let [chow-seq-list (if two-chow
                              (let [others (select-keys mp (disj #{:wan :tiao :bing} (first two-chow)))]
                                [(cons (first (second two-chow)) (map #(first %) (vals others)))
                                 (cons (second (second two-chow)) (map #(first %) (vals others)))])
                              [(map #(first %) (vals mp))])
              step-seq (some (fn [x]
                               (if (step-increase? (sort (map #(tail-enum %) x)) 1)
                                 x)) chow-seq-list)]
          (if step-seq
            (let [[a b c] step-seq]
              (with-comb-consumed [:chow [(comb-suit a) [(tail-enum a)]
                                          (comb-suit b) [(tail-enum b)]
                                          (comb-suit c) [(tail-enum c)]]] 1))))))))

;; 五门齐
(deffan five-types 6 {:exclude []}
  [hands ready]
  (if (= 5 (count (group-by #(suit %) (cons ready (tile-seq hands))))) 1))

;; 全求人
(deffan others-tiles-in-each-set 6 {:exclude [one-tile-wait-for-a-pair]}
  [hands ready]
  (let [combs (concat (chow-seq hands ready) (pong-seq hands) (pub-kong-seq hands))
        pair-tile (get-tile (first (pair-seq hands)))]
    (if (and pair-tile
             (every? #(pub %) combs)
             (= (enum ready) (enum pair-tile))
             (= (suit ready) (suit pair-tile)))
      1)))

;; 双箭刻
(deffan two-dragon-triplets 6 {:exclude []}
  [hands ready]
  (if (= 2 (count (filter #(= :jian (comb-suit %))
                          (concat (pong-seq hands)
                                  (kong-seq hands)
                                  (pub-kong-seq hands)))))
    1))

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
    twin-edge-sequences-plus-center-pair
    four-same-sequences
    four-step-triplets
    four-step-sequences
    three-quads
    all-terminals-or-honors
    seven-pairs
    seven-honors-and-knitted
    all-even
    one-suit-only
    three-same-sequences
    three-step-triplets
    large-three-only
    medium-three-only
    small-three-only
    one-suit-through
    three-suits-edge-sequences-plus-center-pair
    three-step-sequences
    number-5-in-each-set
    three-suits-triplets
    three-closed-triplets
    honors-and-knitted
    knitted-through
    more-than-five
    less-than-five
    three-winds-triplets
    three-suits-through
    symmetric-tiles-only
    three-suits-sequences
    three-suits-step-triplets
    two-closed-quads
    all-triplets
    one-suit-plus-honors
    three-suits-step-sequences
    five-types
    others-tiles-in-each-set
    two-dragon-triplets])

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
       (iter [fans ctx]
         (if (empty? fans)
           {}
           (let [fm (fan-meta (first fans))
                 [cur-fan rctx] (apply-fan (first fans) ctx hands ready)]
             (if cur-fan
               (let [succ-fans (iter (sieve (first fans) (rest fans)) rctx)]
                 (print (get-in rctx [:chow :consumed]) (avaliable-comb-seq rctx :chow) "\n")
                 (into (let [part-excludes (:part-exclude fm)]
                         (loop [e part-excludes r succ-fans]
                           (if-not (empty? e)
                             (let [[k v] (first e)]
                               (cond (not (contains? r k)) (recur (rest e) r)
                                     (> (k r) v) (recur (rest e) (assoc r k (- (k r) (* v (:points fm)))))
                                     :else (recur (rest e) (dissoc r k))))
                             r)))
                       (list cur-fan)))
               (iter (rest fans) ctx)))))]
    (if-let [result (iter *guobiao-fans* (make-fan-context hands ready))]
      (sort-by #(second %) (fn [a b]
                             (> a b)) result))))

(defn- test [instr]
  (let [case (mahjong.dl/build-tile-case-from-ast (mahjong.dl/parse-dl-string instr))
        results (filter-duplicate-ready-hands (parse-hands-ready case))]
    (map (fn [x]
           (let [[r h] x]
             [r (calculate-fan h r)]))
         results)))

;(test "1112345678999t")
;(test "2344466688t222j")
;(test "1111f^2222f-3333f-4444f-3j")
;(test "1122334455677b")
;(test "99w19b19t1234f123j")
;(test "111j222j333j56t33b")
;(test "1112345678999t")
;(test "111122334f1122j")
;(test "111122334f1122j")
;(test "1122335577889w")
;(test "23434234234b66w")
;(test "1112323434545w")
;(test "23345567789t55b")
;(test "2222w^3333w-4444w-2233t")
;(test "111f111w999t99b11j")
;(test "17w28b369t1234f12j")
; (test "222w^444t^666b^444b^7b")
; (test "2233445567788t")
;(test "23434234b234t66w")
; (test "23434234b234b66w")
;(test "789b789w789t89t99t")
;(test "1234567891277w")
;(test "123789w55b13789t")
;(test "12323434b234t22t")
;(test "12334567b345b22t")
;(test "12334567b345b22t")
;(test "456b456t456w46w55w")
;(test "111w^111b^111t99b99t")
;(test "147w28b369t123f12j")
;(test "123t^147w28b369t22b")
;(test "6688t6677w66889b")
;(test "444t^11122233t22w")
;;; (test "111f^222f^333f^99w99t")

;;; (test "123t456b789w123b8b")

;;; (test "234b^456t^888b^33j88t")
;;; (test "345t345b345w5644w")
;;; (test "222b333t444w44b22w")
;;; (test "2222w^456b^678w^888t^6w")
;(test "111j^222j12378w88t")