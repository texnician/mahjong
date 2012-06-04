(ns mahjong.dl
  (:import (org.antlr.runtime ANTLRStringStream
                              CommonTokenStream)
           (mahjong dlLexer dlParser)))

;;; http://s123.codeinspot.com/q/396974

(defn parse-dl-stream [ss]
  (let [lexer (dlLexer. ss)
        tokens (CommonTokenStream. lexer)
        parser (dlParser. tokens)]
    (.getTree (.tile_seq parser))))

(defn parse-dl-string [s]
  (parse-dl-stream (ANTLRStringStream. s)))
