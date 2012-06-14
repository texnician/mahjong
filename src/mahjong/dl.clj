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

(declare parse-triples)
(declare parse-kong)
(declare parse-free-tiles)

(defn parse-tile-case [ast]
  (letfn [(parse [c]
            (let [txt (clojure.string/upper-case (.getText c))]
              (cond (= txt "^") (parse-triples c)
                    (= txt "-") (parse-kong c)
                    (= txt "W") (parse-free-tiles c)
                    (= txt "B") (parse-free-tiles c)
                    (= txt "T") (parse-free-tiles c)
                    (= txt "F") (parse-free-tiles c)
                    (= txt "J") (parse-free-tiles c)
                    :else (assert false (format "%s is not a valid char" txt)))))]
    (if (nil? (.getText ast))
      (let [childs (.getChildren ast)]
        (map parse childs))
      (list (parse ast)))))

(defn parse-triples [ast]
  "Parse chow pong [ast] 345t^ -> [:chow 'T [3 4 5]], 444b^ -> [:pong 'T 4], '7777w^ -> [:pub-kong 'W 7]'"
  (let [cate (first (.getChildren ast))
        cate-sym (-> cate .getText clojure.string/upper-case symbol)
        tile-enums (map #(-> % .getText java.lang.Integer/parseInt) (.getChildren cate))
        tile-num (.getChildCount cate)]
    (cond (pong? tile-enums) [:pong cate-sym (first tile-enums)]
          (chow? tile-enums) [:chow cate-sym [(first tile-enums) (second tile-enums) (nth tile-enums 2)]]
          (kong? tile-enums) [:pub-kong cate-sym (first tile-enums)]
          :else (assert false))))

(defn parse-free-tiles [ast]
  "Parse free tiles. ([ast]) 3456b -> [:free-tiles 'B [3 4 5 6]]"
  (let [cate-sym (-> ast .getText clojure.string/upper-case symbol)
        tile-enums (map #(-> % .getText java.lang.Integer/parseInt) (.getChildren ast))]
    [:free-tiles cate-sym tile-enums]))

(defn parse-kong [ast]
  "Parse kong. ([ast]) 1111f- -> [:kong 'F 1]"
  (let [cate (first (.getChildren ast)) 
        cate-sym (-> cate .getText clojure.string/upper-case symbol)
        tile-enums (map #(-> % .getText java.lang.Integer/parseInt) (.getChildren cate))]
    (if (kong? tile-enums)
      [:kong cate-sym (first tile-enums)]
      (assert false (format "%s is not valid kong" tile-enums)))))

(defn build-tile-case-from-ast [ast]
  (letfn
      [(build-comb-map [parsed]
         (into {} (map (fn [x]
                         (let [[comb-type comb-list] x]
                           [comb-type
                            (cond (= comb-type :chow)
                                  (map (fn [comb]
                                         (apply make-chow (lazy-cat (nth comb 2) (list (second comb) :pub true))))
                                       comb-list)
                                  (= comb-type :pong)
                                  (map #(make-pong (nth % 2) (second %) :pub true) comb-list)
                                  (= comb-type :pub-kong)
                                  (map #(make-kong (nth % 2) (second %) :pub true) comb-list)
                                  (= comb-type :kong)
                                  (map #(make-kong (nth % 2) (second %)) comb-list)
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
      (make-tile-case (:chow comb-map) (:pong comb-map) (:pub-kong comb-map) (:kong comb-map) (:free-tiles comb-map)))))
