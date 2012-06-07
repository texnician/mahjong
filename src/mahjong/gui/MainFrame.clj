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
           (java.awt BorderLayout Container)))

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

(.display gui (doto (javax.swing.JPanel.)
                (.add (javax.swing.JLabel. "你好"))))

;; (.display gui (doto (javax.swing.JPanel.)
;;                 (.add (let [label (javax.swing.JLabel. (.toString (doto (StringBuilder.)
;;                                                                     (.appendCodePoint 0x1f000)
;;                                                                     (.appendCodePoint 0x1f001)
;;                                                                     (.appendCodePoint 0x1f002)
;;                                                                     (.appendCodePoint 0x1f003)
;;                                                                     (.appendCodePoint 0x1f004)
;;                                                                     (.appendCodePoint 0x1f005)
;;                                                                     (.appendCodePoint 0x1f006)
;;                                                                     (.appendCodePoint 0x1f007)
;;                                                                     (.appendCodePoint 0x1f008)
;;                                                                     (.appendCodePoint 0x1f009)
;;                                                                     (.appendCodePoint 0x1f00a)
;;                                                                     (.appendCodePoint 0x1f00b)
;;                                                                     (.appendCodePoint 0x1f00c)
;;                                                                     (.appendCodePoint 0x1f00d)
;;                                                                     (.appendCodePoint 0x1f00e)
;;                                                                     (.appendCodePoint 0x1f00f)
;;                                                                     (.appendCodePoint 0x1f010)
;;                                                                     (.appendCodePoint 0x1f011)
;;                                                                     (.appendCodePoint 0x1f012)
;;                                                                     (.appendCodePoint 0x1f013)
;;                                                                     (.appendCodePoint 0x1f014)
;;                                                                     (.appendCodePoint 0x1f015)
;;                                                                     (.appendCodePoint 0x1f016)
;;                                                                     (.appendCodePoint 0x1f017)
;;                                                                     (.appendCodePoint 0x1f018)
;;                                                                     (.appendCodePoint 0x1f019)
;;                                                                     (.appendCodePoint 0x1f01a)
;;                                                                     (.appendCodePoint 0x1f01b)
;;                                                                     (.appendCodePoint 0x1f01c)
;;                                                                     (.appendCodePoint 0x1f01d)
;;                                                                     (.appendCodePoint 0x1f01e)
;;                                                                     (.appendCodePoint 0x1f01f)
;;                                                                     (.appendCodePoint 0x1f020)
;;                                                                     (.appendCodePoint 0x1f021)
;;                                                                     (.appendCodePoint 0x1f022)
;;                                                                     (.appendCodePoint 0x1f023)
;;                                                                     (.appendCodePoint 0x1f024)
;;                                                                     (.appendCodePoint 0x1f025)
;;                                                                     (.appendCodePoint 0x1f026)
;;                                                                     (.appendCodePoint 0x1f027)
;;                                                                     (.appendCodePoint 0x1f028)
;;                                                                     (.appendCodePoint 0x1f029)
;;                                                                     (.appendCodePoint 0x1f02a)
;;                                                                     (.appendCodePoint 0x1f02b))))]
;;                         (.setFont label (java.awt.Font. "Symbola" java.awt.Font/PLAIN 36))
;;                         label))))
