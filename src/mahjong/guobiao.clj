(ns mahjong.guobiao
  (:use (mahjong tile comb))
  (:require (clojure set)))

;; http://en.wikipedia.org/wiki/Guobiao_Majiang
;; 2 Points
;;     2.1 88 points
;;         2.1.1 Big four winds
;;         2.1.2 Big three dragons
;;         2.1.3 All green
;;         2.1.4 Nine gates
;;         2.1.5 Four quads
;;         2.1.6 Chained seven pairs
;;         2.1.7 Thirteen orphans
;;     2.2 64 points
;;         2.2.1 All terminals
;;         2.2.2 Little four winds
;;         2.2.3 Little three dragons
;;         2.2.4 All honors
;;         2.2.5 All closed triplets
;;         2.2.6 Twin edge sequences plus center pair
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
  (let [triplet-seq (concat (pong-seq hands ready) (kong-seq hands ready) (pub-kong-seq hands ready))]
    (if (and (every? #(= (cate %) :feng) (map #(cate (get-tile %)) triplet-seq))
             (= 4 (count (distinct (map #(enum (get-tile %)) triplet-seq)))))
      1 0)))

;; 大三元
(deffan big-three-dragons 88 {:exclude [dragon-triplet two-dragon-triplets]}
  [hands ready]
  (let [triplet-seq (concat (pong-seq hands ready) (kong-seq hands ready) (pub-kong-seq hands ready))]
    (if (= 3 (distinct (map #(enum %) (filter #(= :jian (cate %))
                                              (map #(get-tile %) triplet-seq)))))
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
  (if (= 4 (count (concat (kong-seq hands ready) (pub-kong-seq hands ready))))
    1 0))

;; 连七对
(deffan chained-seven-pairs 88 {:exclude [seven-pairs
                                          one-suit-only
                                          no-melding
                                          one-tile-wait-for-a-pair]}
  [hands ready]
  (if (and (= :seven-pairs (ready-type hands))
           (one-suit? (tile-seq hands))
           (every? #(= -1 %)
                   (map #(apply - %)
                        (partition 2 1 (map #(enum (get-tile %))
                                            (pair-seq hands ready))))))
    1 0))

;; 十三幺
(deffan chained-seven-pairs 88 {:exclude [five-types
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
           (every? #(and (suit? %) (#{1 9} (enum %))) (tile-seq hands)))
    1 0))

;; 小四喜
(deffan little-four-winds 64 {:exclude [three-winds-triplets]
                              :part-exclude [terminal-or-non-special-wind-triplet 3]}
  [hands ready]
  (if (and (= :normal (ready-type hands))
           (let [triplet-seq (concat (pong-seq hands ready)
                                     (kong-seq hands ready)
                                     (pub-kong-seq hands ready))
                 pair-tile (get-tile (first (pair-seq hands ready)))
                 feng-set (set (map #(enum %)
                                      (filter #(= :feng (-> % get-tile cate)) triplet-seq)))]
             (and (= :feng (cate pair-tile))
                  (= 3 (count feng-set))
                  (= (enum pair-tile)
                     (first (clojure.set/difference #{1 2 3 4} feng-set))))))
    1 0))

;; 2.2.3 Little three dragons
;; 2.2.4 All honors
;; 2.2.5 All closed triplets
;; 2.2.6 Twin edge sequences plus center pair