(ns mahjong.core
  (:import (org.antlr.runtime ANTLRStringStream
                              CommonTokenStream)
           (mahjong exprLexer exprParser)))

(defn parse-expr [s]
  (let [lexer (exprLexer. (ANTLRStringStream. s))
        tokens (CommonTokenStream. lexer)
        parser (exprParser. tokens)]
    (.getTree (.expr parser))))

(defn node-seq [x]
  (map #(.getText %)
       (tree-seq #(not (zero? (.getChildCount %)))
                 #(.getChildren %)
                 x)))

(defn AST [node]
  (if (zero? (.getChildCount node))
    (.getText node)
    (let [children (map AST (.getChildren node))
          txt (.getText node)]
      (if txt
        (cons txt children)
        children))))