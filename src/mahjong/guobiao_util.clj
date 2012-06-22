(ns mahjong.guobiao-util
  (:use (mahjong tile comb)))

(defprotocol FanContext
  (consume-comb [this t s e])
  (comb-consumed? [this t s e])
  (avaliable-comb-seq [this t]))

(defrecord NormalFanContext [chow])

(extend-protocol FanContext
  NormalFanContext
  (consume-comb [this t s e]
    (let [[idx consumed]  (some (fn [x]
                                  (let [[i c] x]
                                    (if (and (= s (comb-suit c))
                                             (= e (-> c get-tile enum)))
                                      x)))
                                (get-in this [t :combs]))]
      (if idx
        (assoc-in this [t :consumed] (cons idx (get-in this [t :consumed]))))))
  (comb-consumed? [this t s e]
    (let [combs  (filter (fn [x]
                           (let [[i c] x]
                             (if (and (= s (comb-suit c))
                                      (= e (-> c get-tile enum)))
                               x)))
                         (get-in this [t :combs]))]
      (every? (fn [x]
                (some #(= % (first x))) (get-in this [t :consumed])) combs)))
  (avaliable-comb-seq [this t]
    (map #(second %) (filter (fn [x]
                               (not (some #(= (first x) %) (get-in this [t :consumed]))))
                             (get-in this [t :combs])))))


(defn make-fan-context [hands ready]
  (cond (= :normal (ready-type hands)) (->NormalFanContext {:combs (map vector (iterate inc 0)
                                                                        (sort-by #(tile-key (get-tile % 0)) (chow-seq hands ready))) 
                                                            :consumed '()})))

(defn unfold-consume-args [args]
  "[:chow [:bing [1 4 7]
           :tiao [2]]
     :pong [:bing [2 5]]]
  -> ((:chow :bing 1) (:chow :bing 4) (:chow :bing 7) (:chow :tiao 2) (:pong :bing 2) (:pong :bing 5))
  "
  {:pre [(or (and (keyword? (first args)) (even? (count args))) true)]}
  (if-not (keyword? (first args))
    (partition 1 args)
    (mapcat (fn [x]
              (map #(cons (first x) %) (unfold-consume-args (second x))))
            (partition 2 args))))

(defmacro with-comb-consumed [consumed ret]
  (let [arg-seq (gensym)
        ctx (gensym)
        args (gensym)]
    `(vector ~ret
             (let [~arg-seq (unfold-consume-args ~consumed)]
               (reduce (fn [~ctx ~args]
                         (apply consume-comb (cons ~ctx ~args))) ~'&ctx ~arg-seq)))))

(defmacro deffan [fan points meta-info arg-list & body]
  (let [key (keyword fan)
        exclude-keys (vec (map #(keyword %) (:exclude meta-info)))
        part-exclude-keys (vec (map #(keyword (first %)) (partition 2 (:part-exclude meta-info))))
        part-exclude-vals (vec (map #(second %) (partition 2 (:part-exclude meta-info))))
        pred (gensym "pred__")
        pred-ret (gensym "m__")
        factor (gensym "f__")
        rctx (gensym "rctx__")]
    `(defn ~fan
       {:key ~key
        :points ~points
        :exclude ~exclude-keys
        :part-exclude ~(zipmap part-exclude-keys part-exclude-vals)}
       ~(vec (cons `~'&ctx arg-list))
       (letfn [(~pred []
                 ~@body)]
         (let [[~factor ~rctx] (let [~pred-ret (~pred)]
                                 (cond (nil? ~pred-ret) [0 ~'&ctx]
                                       (integer? ~pred-ret) [~pred-ret ~'&ctx]
                                       :else ~pred-ret))]
           (if (> ~factor 0)
             [[~key (* ~factor ~points)] ~rctx]
             [~'nil ~rctx]))))))

(defn get-step-sub-sequence [step n coll]
  "get step increase  sub sequence length n in coll, step is default 1"
  (if (>= (count coll) n)
    (filter #(step-increase? % step) (partition n 1 (sort coll)))))

(defn sorted-chow-sets [chow-set-list]
  "get chow set list, return sorted chow tail enum sets
  ([(1 2 3) (2 3 4)] [(5 6 7)] [(7 8 9)]) -> (#{7} #{5} #{1 2})"
  (sort-by #(count %) (map (fn [x]
                             (apply sorted-set (map #(tail-enum %) x))) chow-set-list)))

(def ^:dynamic *prevailing-wind* 1)
(def ^:dynamic *game-wind* 1)
(def ^:dynamic *self-draw* false)
(def ^:dynamic *last-tile* false)