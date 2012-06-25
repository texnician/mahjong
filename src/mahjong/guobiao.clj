(ns mahjong.guobiao
  (:use (mahjong comb tile guobiao-util))
  (:require (clojure set)))

;; http://en.wikipedia.org/wiki/Guobiao_Majiang
;;     2.11 2 points

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
  (if (every? #(green-color? %) (cons ready (tile-seq hands)))
    1 0))

;; 九莲宝灯
(deffan nine-gates 88 {:exclude [one-suit-only
                                 no-melding
                                 no-honor
                                 terminal-or-non-special-wind-triplet]}
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
                                          two-closed-triplets]}
  [hands ready]
  (let [triplet-seq (concat (pong-seq hands) (kong-seq hands))]
    (if (and (= 4 (count triplet-seq))
             (every? #(not (pub %)) triplet-seq)
             (if (= (complete-what? hands ready) :pong)
               *self-draw*
               true))
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
             (-> pongs first get-tile simple?)
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
           (some #(simple? %) (cons ready (tile-seq hands)))
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
                                                                  edge-sequences-pair
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
(deffan three-closed-triplets 16 {:exclude [two-closed-triplets]}
  [hands ready]
  (let [pongs (concat (pong-seq hands) (kong-seq hands) (pub-kong-seq hands))]
    (if (and (= 3 (count (filter #(not (pub %)) pongs)))
             (if (= :pong (complete-what? hands ready))
               *self-draw*
               true)) 
      1 0)))

;; 全不靠
(deffan honors-and-knitted 12 {:exclude [five-types
                                         no-melding]}
  [hands ready]
  (if (= :honors-and-knitted (ready-type hands))
    1 0))

;; 组合龙
(deffan knitted-through 12 {:exclude [three-suits-step-sequences]}
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
(deffan two-closed-quads 8 {:exclude [closed-quad]}
  [hands ready]
  (if (= 2 (count (kong-seq hands)))
    1 0))

;;; 妙手回春
(deffan last-drawn-tile 8 {:exclude [completion-by-draw]}
  [hands ready]
  (if *last-drawn-tile* 1))

;;; 海底捞月
(deffan last-discarded-tile 8 {:exclude []}
  [hands ready]
  (if *last-discard-tile* 1))

;;; 杠上开花
(deffan supplemental-tile-of-melding-quad 8 {:exclude []}
  [hands ready]
  (if (and *supplemental-tile-of-melding-quad*
           (not (empty? (concat (pub-kong-seq hands) (kong-seq hands))) ) 0)
    1))

;;; 抢杠和
(deffan appended-tile-to-melded-triplet 8 {:exclude [last-tile-other-than-revealed]}
  [hands ready]
  (if (and *appended-tile-to-melded-triplet*
           (not-any? #(= (tile-name ready) (tile-name %)) (tile-seq hands)))
    1))

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
    (if (and (not *self-draw*)
             pair-tile
             (= 4 (count combs))
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

;; 全带幺
(deffan terminals-or-honors-in-each-set 4 {:exclude []}
  [hands ready]
  (let [pong-pairs (concat (pong-seq hands)
                           (kong-seq hands)
                           (pub-kong-seq hands)
                           (pair-seq hands))
        chows (chow-seq hands ready)]
    (if (and (= :normal (ready-type hands))
             (every? #(terminal-or-honor? %) (map #(get-tile %) pong-pairs))
             (if-not (empty? chows)
               (every? (fn [x]
                         (some #(terminal-or-honor? %) (tile-seq x)))
                       chows)
               true))
      1)))

;; 不求人
(deffan self-tiles-only 4 {:exclude [completion-by-draw no-melding]}
  [hands ready]
  (if (and *self-draw*
           (if (= :normal (ready-type hands))
             (let [combs (concat (pong-seq hands) (kong-seq hands) (chow-seq hands ready))]
               (and (= 4 (count combs))
                    (not-any? #(pub %) combs)))
             true))
    1))

;; 双明杠
(deffan two-open-quads 2 {:exclude [open-quad
                                    closed-quad]}
  [hands ready]
  (if (= 2 (count (concat (kong-seq hands) (pub-kong-seq hands))))
    (cond (= 2 (count (pub-kong-seq hands))) 2
          (= 1 (count (pub-kong-seq hands))) 3)))

;; 和绝张
(deffan last-tile-other-than-revealed 4 {:exclude []}
  [hands ready]
  (if (and *last-tile*
           (not-any? #(= (tile-name ready) (tile-name %)) (tile-seq (free-tiles (get-hands-case hands)))))
    1))

;; 箭刻
(deffan dragon-triplet 2 {:exclude []}
  [hands ready]
  (if (= 1 (count (filter #(= :jian (comb-suit %))
                          (concat (pong-seq hands)
                                  (kong-seq hands)
                                  (pub-kong-seq hands)))))
    1))

;; 圈风刻
(deffan prevailing-wind-triplet 2 {:exclude []}
  [hands ready]
  (let [wind-pongs (filter #(= :feng (comb-suit %)) (concat (pong-seq hands)
                                                            (kong-seq hands)
                                                            (pub-kong-seq hands)))]
    (if (some #(= (-> % get-tile enum) *prevailing-wind*) wind-pongs)
      1)))

;; 门风刻
(deffan game-wind-triplet 2 {:exclude []}
  [hands ready]
  (let [wind-pongs (filter #(= :feng (comb-suit %)) (concat (pong-seq hands)
                                                            (kong-seq hands)
                                                            (pub-kong-seq hands)))]
    (if (some #(= (-> % get-tile enum) *game-wind*) wind-pongs)
      1)))

;; 门清
(deffan no-melding 2 {:exclude []}
  [hands ready]
  (if (and (= :normal (ready-type hands))
           (let [combs (concat (pong-seq hands) (kong-seq hands) (chow-seq hands ready))]
             (and (= 4 (count combs))
                  (not-any? #(pub %) combs))))
    1))

;; 平胡
(deffan simple-sequence-hand 2 {:exclude [no-honor]}
  [hands ready]
  (if (and (= :normal (ready-type hands))
           (every? #(simple? %) (cons ready (tile-seq hands)))
           (= 4 (count (chow-seq hands ready))))
    1))

;; 四归一
(deffan four-tiles-collection 2 {:exclude []}
  [hands ready]
  (let [kong-tile-set (set (map #(-> % get-tile tile-name) (concat (kong-seq hands) (pub-kong-seq hands))))
        tiles (filter #(not (get kong-tile-set (tile-name %))) (cons ready (tile-seq hands)))]
    (count (filter #(= 4 (count %)) (vals (group-by #(tile-name %) tiles))))))

;; 双同刻
(deffan two-suits-triplets 2 {:exclude []}
  [hands ready]
  (let [pongs (filter #(#{:wan :tiao :bing} (comb-suit %))
                      (concat (pong-seq hands) (kong-seq hands) (pub-kong-seq hands)))]
    (count (filter #(= 2 (count %)) (vals (group-by #(-> % get-tile enum) pongs))))))

;; 双暗刻
(deffan two-closed-triplets 2 {:exclude []}
  [hands ready]
  (let [pongs (concat (pong-seq hands) (kong-seq hands))
        closed-pongs (filter #(not (pub %)) pongs)]
    (if (and (= 2 (count closed-pongs))
             (not-every? #(= :kong (:tag (meta %))) closed-pongs)
             (if (= :pong (complete-what? hands ready))
               *self-draw*
               true))
      1)))

;; 暗杠
(deffan closed-quad 2 {:exclude []}
  [hands ready]
  (if (and (= 1 (count (kong-seq hands))))
    1))

;; 断幺
(deffan all-simples 2 {:exclude [no-honor]}
  [hands ready]
  (if (not-any? #(terminal-or-honor? %) (cons ready (tile-seq hands)))
    1))

;; 一般高
(deffan two-same-sequences 1 {:exclude []}
  [hands ready]
  (match-comb-pair (= (comb-suit :a) (comb-suit :b))
                   (= (tail-enum :a) (tail-enum :b))
                   (= (head-enum :a) (head-enum :b))))

;; 喜相逢
(deffan two-suits-sequences 1 {:exclude []}
  [hands ready]
  (match-comb-pair (not (= (comb-suit :a) (comb-suit :b)))
                   (= (tail-enum :a) (tail-enum :b))
                   (= (head-enum :a) (head-enum :b))))

;; 连六
(deffan chain-six 1 {:exclude []}
  [hands ready]
  (match-comb-pair (= (comb-suit :a) (comb-suit :b))
                   (= 3 (Math/abs (- (tail-enum :a) (tail-enum :b))))
                   (= 3 (Math/abs (- (head-enum :a) (head-enum :b))))))


;; 老少副
(deffan edge-sequences-pair 1 {:exclude []}
  [hands ready]
  (match-comb-pair (= (comb-suit :a) (comb-suit :b))
                   (= 6 (Math/abs (- (tail-enum :a) (tail-enum :b))))
                   (= 6 (Math/abs (- (head-enum :a) (head-enum :b))))))

;; 幺九刻
(deffan terminal-or-non-special-wind-triplet 1 {:exclude []}
  [hands ready]
  (let [pongs (concat (pong-seq hands) (kong-seq hands) (pub-kong-seq hands))]
    (count (filter (fn [x]
                     (let [tile (get-tile x)]
                       (if (terminal-or-honor? tile)
                         (cond (= :jian (suit tile)) false
                               (= :feng (suit tile)) (not (or (= (enum tile) *prevailing-wind*)
                                                              (= (enum tile) *game-wind*)))
                               :else true))))
                   pongs))))

;; 明杠
(deffan open-quad 1 {:exclude []}
  [hands ready]
  (if (and (= 1 (count (pub-kong-seq hands)))
           (empty? (kong-seq hands))) 
    1))

;; 缺一门
(deffan lack-one-suit 1 {:exclude []}
  [hands ready]
  (let [mp (select-keys (group-by #(suit %) (cons ready (tile-seq hands))) [:wan :tiao :bing])]
    (if (= 2 (count mp)) 1)))

;; 无字
(deffan no-honor 1 {:exclude []}
  [hands ready]
  (if (empty? (filter #(honor? %) (cons ready (tile-seq hands))))
    1))

;; 边张
(deffan one-tile-wait-for-a-edge-sequence 1 {:exclude []}
  [hands ready]
  (if (= :normal (ready-type hands))
    (if (= 1 (count (distinct (mapcat #(get-in % [:parse-result :ready-for]) (:normal *parse-result*)))))
      (let [chow (some (fn [x]
                         (if (= 2 (count x))
                           x)) (get-in hands [:parse-result :meld :chow]))]
        (if-not (empty? chow)
          (let [[a b] (map #(enum %) (map-tile-index (:hands-case hands) chow))]
            (if (or (and (= 1 a) (= 2 b) (= 3 (enum ready)))
                    (and (= 7 (enum ready)) (= 8 a) (= 9 b)))
              1)))))))

;; 坎张
(deffan one-tile-wait-for-a-holed-sequence 1 {:exclude []}
  [hands ready]
  (if (= :normal (ready-type hands))
    (if (= 1 (count (distinct (mapcat #(get-in % [:parse-result :ready-for]) (:normal *parse-result*)))))
      (let [chow (some (fn [x]
                         (if (= 2 (count x))
                           x)) (get-in hands [:parse-result :meld :chow]))]
        (if-not (empty? chow)
          (let [[a b] (map #(enum %) (map-tile-index (:hands-case hands) chow))]
            (if (step-increase? [a (enum ready) b] 1)
              1)))))))

;; 单吊将
(deffan one-tile-wait-for-a-pair 1 {:exclude []}
  [hands ready]
  (if (= :normal (ready-type hands))
    (if (= 1 (count (distinct (mapcat #(get-in % [:parse-result :ready-for]) (:normal *parse-result*)))))
      (let [pair (some (fn [x]
                         (if (= 1 (count x))
                           x)) (get-in hands [:parse-result :meld :pair]))]
        (if-not (empty? pair)
          1)))))

;; 自摸
(deffan completion-by-draw 1 {:exclude []}
  [hands ready]
  (if *self-draw* 1))

;;; 无番胡
(deffan avoid-points 8 {:exclude []}
  [hands ready]
  1)


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
    last-drawn-tile
    last-discarded-tile
    supplemental-tile-of-melding-quad
    appended-tile-to-melded-triplet
    all-triplets
    one-suit-plus-honors
    three-suits-step-sequences
    five-types
    others-tiles-in-each-set
    two-dragon-triplets
    terminals-or-honors-in-each-set
    self-tiles-only
    two-open-quads
    last-tile-other-than-revealed
    dragon-triplet
    prevailing-wind-triplet
    game-wind-triplet
    no-melding
    simple-sequence-hand
    four-tiles-collection
    two-suits-triplets
    two-closed-triplets
    closed-quad
    all-simples
    two-same-sequences
    chain-six
    edge-sequences-pair
    two-suits-sequences
    terminal-or-non-special-wind-triplet
    open-quad
    lack-one-suit
    no-honor
    one-tile-wait-for-a-edge-sequence
    one-tile-wait-for-a-holed-sequence
    one-tile-wait-for-a-pair
    completion-by-draw
    avoid-points])

(defn fan-meta [func]
  (meta (ns-resolve 'mahjong.guobiao func)))

(defn apply-fan [func & args]
  (apply (ns-resolve 'mahjong.guobiao func) args))

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
                 (into (let [part-excludes (:part-exclude fm)]
                         (loop [e part-excludes r succ-fans]
                           (if-not (empty? e)
                             (let [[k v] (first e)]
                               (cond (not (contains? r k)) (recur (rest e) r)
                                     (> (k r) v) (recur (rest e) (assoc r k (- (k r) (* v (:points (fan-meta (symbol (name k))))))))
                                     :else (recur (rest e) (dissoc r k))))
                             r)))
                       (list cur-fan)))
               (iter (rest fans) ctx)))))]
    (if-let [result (iter *guobiao-fans* (make-fan-context hands ready))]
      (sort-by #(second %) (fn [a b]
                             (> a b)) result))))

(defn- test-fan [instr]
  (let [case (mahjong.dl/build-tile-case-from-ast (mahjong.dl/parse-dl-string instr))
        parse-result  (parse-hands-ready case)
        results (filter-duplicate-ready-hands parse-result)]
    (map (fn [x]
             (let [[r h] x]
               [r (binding [*parse-result* parse-result]
                    (calculate-fan h r))]))
           results)))

;(test-fan "1112345678999t")
;(test-fan "2344466688t222j")
;(test-fan "1111f^2222f-3333f-4444f-3j")
;(test-fan "1122334455677b")
;(test-fan "99w19b19t1234f123j")
;(test-fan "111j222j333j56t33b")
;(test-fan "1112345678999t")
;(test-fan "111122334f1122j")
;(test-fan "111122334f1122j")
;(test-fan "1122335577889w")
;(test-fan "23434234234b66w")
;(test-fan "1112323434545w")
;(test-fan "23345567789t55b")
;(test-fan "2222w^3333w-4444w-2233t")
;(test-fan "111f111w999t99b11j")
;(test-fan "17w28b369t1234f12j")
; (test-fan "222w^444t^666b^444b^7b")
; (test-fan "2233445567788t")
;(test-fan "23434234b234t66w")
; (test-fan "23434234b234b66w")
;(test-fan "789b789w789t89t99t")
;(test-fan "1234567891277w")
;(test-fan "123789w55b13789t")
;(test-fan "12323434b234t22t")
;(test-fan "12334567b345b22t")
;(test-fan "12334567b345b22t")
;(test-fan "456b456t456w46w55w")
;(test-fan "111w^111b^111t99b99t")
;(test-fan "147w28b369t123f12j")
;(test-fan "123t^147w28b369t22b")
;(test-fan "6688t6677w66889b")
;(test-fan "444t^11122233t22w")
;;; (test-fan "111f^222f^333f^99w99t")

;;; (test-fan "123t456b789w123b8b")

;;; (test-fan "234b^456t^888b^33j88t")
;;; (test-fan "345t345b345w5644w")
;;; (test-fan "222b333t444w44b22w")
;;; (test-fan "2222w^456b^678w^888t^6w")
;(test-fan "111j^222j12378w88t")
;(test-fan "789b789w789t89t99t")
;(test-fan "8888t-789b789w77w33b")
;(test-fan "444w^555w^555t88b44b")
;(test-fan "9999b-999t789w55w12w")
;(test-fan "234w^345b^456t77t44b")
;;; ;(test-fan "123w678t678t89t99b")
;;; (test-fan "123w456b789t789w1f")
;;; (test-fan "123456t234567w1j")
;(test-fan "234567w789t123t1j")
;(test-fan "123b^444t^789w^34b11j")
;; (test-fan "888w^5555w-222w666t8t")
;; (test-fan "3333b-5555w^1111t^77t78b")
;; (test-fan "55w12389t123789b")
;; (test-fan "333j^1111w^66w11j222j")
;; (test-fan "888w^5555w-222w666t8t")
;; (test-fan "444f^111f^456b^2223f")
;; (test-fan "789t^23345567t22j")
;; (test-fan "258w17t39b123f123j")
;; (test-fan "9999b^2222j^23488t11j")
;; (test-fan "333f^222j^999w444f1j")
;; (test-fan "2223456677889w")
;; (test-fan "999t^789w^77t78899b")
;; (test-fan "789b^77788w78889t")
;(test-fan "2344466688t222j")
;(test-fan "456b^456t^789t^1111b^2t")