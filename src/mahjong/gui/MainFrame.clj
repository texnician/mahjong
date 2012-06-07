(ns mahjong.gui.MainFrame
  (:gen-class
   :name mahjong.gui.MainFrame
   :extends javax.swing.JFrame
   :implements [clojure.lang.IMeta]
   :prefix df-
   :state state
   :init init
   :constructors {[String] [String]}
   :methods [[display [java.awt.Container] void]
             ^{:static true} [version [] String]])
  (:import (javax.swing JFrame JPanel JTextPane)
           (java.awt BorderLayout Container))
  (:use (mahjong tile comb)))

(compile 'mahjong.gui.MainFrame)

(defn df-init [title]
  [[title] (atom {::title title})])

(defn df-meta [this] @(.state this))
(defn version [] "1.0")

(meta (mahjong.gui.MainFrame. "3rd"))

(defn df-display [this pane]
  (doto this
    (-> .getContentPane .removeAll)
    (.setContentPane (doto (JPanel.)
                       (.add pane BorderLayout/CENTER)))
    (.pack)
    (.setVisible true)))

(def gui (mahjong.gui.MainFrame. "4th"))

;(.display gui (doto (javax.swing.JPanel.)
;                (.add (javax.swing.JLabel. "你好"))))

 
(defn display-tile-case [case]
  (let [char-seq (flatten (interpose 0x20 (map char-codes (all-comb-seq case))))]
    (.display gui (doto (javax.swing.JPanel.)
                    (.add (let [label (javax.swing.JLabel. (.toString (loop [seq char-seq bd (StringBuilder.)]
                                                                        (if (empty? seq)
                                                                          bd
                                                                          (recur (rest seq) (.appendCodePoint bd (first seq)))))))]
                            (.setFont label (java.awt.Font. "Symbola" java.awt.Font/PLAIN 36))
                            label))))))
