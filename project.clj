(defproject mahjong "1.0.0-SNAPSHOT"
  :description "mahjong game"
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [org.antlr/antlr "3.4"]]
  :plugins [[lein-ring "0.4.5"]]
  :java-source-path "."
  :resources-path "resources/"
  :omit-source true
  :aot [mahjong.dl mahjong.tile mahjong.comb mahjong.guobiao mahjong.guobiao-util mahjong.gui.MainFrame]
  :main mahjong.gui.MainFrame)