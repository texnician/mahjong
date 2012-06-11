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
  (:import (javax.swing JFrame JPanel JTextPane Box BoxLayout JTextField
                        JSplitPane JLabel JButton JOptionPane ImageIcon BorderFactory
                        )
           (javax.swing.border BevelBorder)
           (javax.imageio ImageIO)
           (java.io File)
           (java.awt Image BorderLayout Container Component GridLayout FlowLayout Color Dimension, Graphics2D)
           (java.awt.image BufferedImage)
           (java.awt.event ActionListener))
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

(defn shelf [& components]
  (let [shelf (JPanel.)]
    (.setLayout shelf (FlowLayout. FlowLayout/CENTER -1 0))
    (println (count components))
    (doall (map #(.add shelf %) components))
    shelf))

(defn stack [& components]
  (let [stack (Box. BoxLayout/PAGE_AXIS)]
    (doseq [c components]
      (.setAlignmentX c Component/CENTER_ALIGNMENT)
      (.add stack c))
    stack))

(defn splitter [top bottom]
  (doto (JSplitPane.)
    (.setOrientation JSplitPane/VERTICAL_SPLIT)
    (.setLeftComponent top)
    (.setRightComponent bottom)))

(defn tile_set [& components]
  (let [tile_set (JPanel.)]
    (.setLayout tile_set (GridLayout. 1 (count components)))
    (doto (.getLayout tile_set)
      (.setHgap -1))
    (doseq [c components] (.add tile_set c))
    tile_set))
 
(defn display-tile-case [case]
  (let [char-seq (flatten (interpose 0x20 (map char-codes (all-comb-seq case))))]
    (.display gui (doto (javax.swing.JPanel.)
                    (.add (let [label (javax.swing.JLabel. (.toString (loop [seq char-seq bd (StringBuilder.)]
                                                                        (if (empty? seq)
                                                                          bd
                                                                          (recur (rest seq) (.appendCodePoint bd (first seq)))))))]
                            (.setFont label (java.awt.Font. "Symbola" java.awt.Font/PLAIN 36))
                            label))))))

;BufferedImage resizedImage = new BufferedImage(IMG_WIDTH, IMG_HEIGHT, type);
;	Graphics2D g = resizedImage.createGraphics();
;	g.drawImage(originalImage, 0, 0, IMG_WIDTH, IMG_HEIGHT, null);
;	g.dispose();

(defn resize-image [img, w, h]
  (.getSubimage img 0 0 w h))

(defn image-label [uri]
  (doto (javax.swing.JLabel. (ImageIcon. (resize-image (ImageIO/read (File. uri)) 40 78)))
    ;(.setBorder (BorderFactory/createBevelBorder BevelBorder/RAISED (Color. 0x4f4f4f) Color/GRAY))
    ))

(defn display-image []
  (.display gui
            (apply tile_set (map #(image-label %) ["images/small/t1ld.png"
                                                   "images/small/t1ld.png"
                                                   "images/small/t1ld.png"
                                                   "images/small/w9ld.png"
                                                   "images/small/w9ld.png"
                                                   "images/small/j1ld.png"
                                                   "images/small/back.png"
                                                   "images/small/back.png"
                                                   "images/small/back.png"
                                                   "images/small/j2ld.png"
                                                   "images/small/j2ld.png"
                                                   "images/small/j2ld.png"
                                                   "images/small/j3ld.png"
                                                   "images/small/j3ld.png"
                                                   "images/small/j3ld.png"]))
            ))

;(display-image)