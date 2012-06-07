(ns mahjong.dl
  (:import (org.antlr.runtime ANTLRStringStream
                              CommonTokenStream)
           (mahjong dlLexer dlParser))
  (:use (mahjong tile comb)))

;;; http://s123.codeinspot.com/q/396974

(defn parse-dl-stream [ss]
  (let [lexer (dlLexer. ss)
        tokens (CommonTokenStream. lexer)
        parser (dlParser. tokens)]
    (.getTree (.tile_seq parser))))

(defn parse-dl-string [s]
  (parse-dl-stream (ANTLRStringStream. s)))

(declare parse-chi-pong)
(declare parse-gang)
(declare parse-free-tiles)

(defn parse-tile-case [ast]
  (let [childs (.getChildren ast)]
    (map (fn [c]
           (let [txt (clojure.string/upper-case (.getText c))]
             (cond (= txt "^") (parse-chi-pong c)
                   (= txt "-") (parse-gang c)
                   (= txt "W") (parse-free-tiles c)
                   (= txt "B") (parse-free-tiles c)
                   (= txt "T") (parse-free-tiles c)
                   (= txt "F") (parse-free-tiles c)
                   (= txt "J") (parse-free-tiles c)
                   :else (assert false (format "%s is not a valid char" txt)))))
         childs)))

(defn parse-chi-pong [ast]
  "Parse chi pong [ast] 345t^ -> [:chi 'T [3 4 5]], 444b^ -> [:pong 'T 4], '7777w^ -> [:pub-gang 'W 7]'"
  (let [cate (first (.getChildren ast))
        cate-sym (-> cate .getText clojure.string/upper-case symbol)
        tile-enums (map #(-> % .getText java.lang.Integer/parseInt) (.getChildren cate))
        tile-num (.getChildCount cate)]
    (cond (ke? tile-enums) [:pong cate-sym (first tile-enums)]
          (shun? tile-enums) [:chi cate-sym [(first tile-enums) (second tile-enums) (nth tile-enums 2)]]
          (gang? tile-enums) [:pub-gang cate-sym (first tile-enums)]
          :else (assert false))))

(defn parse-free-tiles [ast]
  "Parse free tiles. ([ast]) 3456b -> [:free-tiles 'B [3 4 5 6]]"
  (let [cate-sym (-> ast .getText clojure.string/upper-case symbol)
        tile-enums (map #(-> % .getText java.lang.Integer/parseInt) (.getChildren ast))]
    [:free-tiles cate-sym tile-enums]))

(defn parse-gang [ast]
  "Parse gang. ([ast]) 1111f- -> [:gang 'F 1]"
  (let [cate (first (.getChildren ast)) 
        cate-sym (-> cate .getText clojure.string/upper-case symbol)
        tile-enums (map #(-> % .getText java.lang.Integer/parseInt) (.getChildren cate))]
    (if (gang? tile-enums)
      [:gang cate-sym (first tile-enums)]
      (assert false (format "%s is not valid gang" tile-enums)))))

(defn build-tile-case-from-ast [ast]
  (letfn
      [(build-comb-map [parsed]
         (into {} (map (fn [x]
                         (let [[comb-type comb-list] x]
                           [comb-type
                            (cond (= comb-type :chi)
                                  (map (fn [comb]
                                         (apply make-shun (lazy-cat (nth comb 2) (list (second comb) :pub true))))
                                       comb-list)
                                  (= comb-type :pong)
                                  (map #(make-ke (nth % 2) (second %) :pub true) comb-list)
                                  (= comb-type :pub-gang)
                                  (map #(make-gang (nth % 2) (second %) :pub true) comb-list)
                                  (= comb-type :gang)
                                  (map #(make-gang (nth % 2) (second %)) comb-list)
                                  (= comb-type :free-tiles)
                                  (let [free-tiles (make-free-tiles)]
                                    (let [tile-seq (partition 2 (apply concat (map (fn [x]
                                                                                     (let [[_ cate enums] x]
                                                                                       (interleave enums (repeat cate))))
                                                                                   comb-list)))]
                                      (loop [tiles tile-seq free-tiles (make-free-tiles)]
                                        (if (empty? tiles)
                                          free-tiles
                                          (let [[enum cate] (first tiles)]
                                            (recur (next tiles) (add-tile free-tiles enum cate))))))))]
                           ))
                       (group-by #(first %) parsed))))]
    (let [comb-map (build-comb-map (parse-tile-case ast))]
      (make-tile-case (:chi comb-map) (:pong comb-map) (:pub-gang comb-map) (:gang comb-map) (:free-tiles comb-map)))))