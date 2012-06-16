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
                        JScrollPane)
           (javax.swing.border BevelBorder)
           (javax.imageio ImageIO)
           (java.io File)
           (java.awt Image BorderLayout Container Component GridLayout FlowLayout Color Dimension, Graphics2D)
           (java.awt.image BufferedImage)
           (java.awt.event ActionListener))
  (:use (mahjong tile comb dl)))

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

(def gui (mahjong.gui.MainFrame. "Mahjong"))

;(.display gui (doto (javax.swing.JPanel.)
;                (.add (javax.swing.JLabel. "你好"))))

(defn shelf [& components]
  (let [shelf (JPanel.)]
    (.setLayout shelf (FlowLayout. FlowLayout/CENTER 8 0))
    (doall (map #(.add shelf %) components))
    shelf))

(defn stack [& components]
  (let [stack (JPanel.)
        scroll (JScrollPane.)]
    (.setLayout stack (BoxLayout. stack BoxLayout/Y_AXIS))
    (doseq [c components]
      (.setAlignmentX c Component/CENTER_ALIGNMENT)
      (.add stack c))
    (doto scroll
      (.setMaximumSize (Dimension. 1024 480))
      (.setViewportView stack))))

(defn splitter [top bottom]
  (doto (JSplitPane.)
    (.setOrientation JSplitPane/VERTICAL_SPLIT)
    (.setLeftComponent top)
    (.setRightComponent bottom)))

(defn tile-group [& components]
  (let [tile-group (JPanel.)]
    (.setLayout tile-group (GridLayout. 1 (count components)))
    (doto (.getLayout tile-group)
      (.setHgap -1))
    (doseq [c components] (.add tile-group c))
    tile-group))

(defn tile-set [& components]
  (let [tile-set (JPanel.)]
    (.setLayout tile-set (GridLayout. 1 (count components)))
    (doto (.getLayout tile-set)
      (.setHgap 5))
    (doseq [c components] (.add tile-set c))
    tile-set))

(defn make-tile-component [tile & {:keys [back] :or {back false}}]
  (let [image-name (if-not back (format "%s%dld.png" (clojure.string/lower-case (name (cate-sym tile)))
                                        (enum tile))
                           "back.png")
        uri (format "images/small/%s" image-name)]
    (image-label uri)))

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

(defn make-comb-component [comb]
  (let [tiles (tile-seq comb)
        image-labels (if (and (= (type comb) mahjong.comb.Kong) (not (pub comb)))
                       (list (make-tile-component (first tiles) :back true)
                             (make-tile-component (first tiles))
                             (make-tile-component (first tiles) :back true)
                             (make-tile-component (first tiles) :back true))
                       (map make-tile-component tiles))]
    (apply tile-group image-labels)))

(defn display-image-tile-case [instr]
  (let [case (build-tile-case-from-ast (parse-dl-string instr))
        combs (all-comb-seq case)]
    (.display gui (apply shelf (map make-comb-component combs)))))

(defn display-hands-ready [instr]
  (let [case (build-tile-case-from-ast (parse-dl-string instr))
        results (sort-by #(tile-key (first %)) (into {} (let [r (parse-hands-ready case)]
                                                          (mapcat (fn [x]
                                                                    (let [readys (get-ready-tiles x)]
                                                                      (map #(vector % (all-comb-seq x %)) readys)))
                                                                  (:normal r)))))]
    (let [tp (apply shelf (map make-comb-component (all-comb-seq case)))
          bt (apply stack (map (fn [x]
                                 (let [[ready combs] x]
                                   (apply shelf (cons (make-tile-component ready)
                                                      (map make-comb-component combs)))))
                               results))]
      (.display gui (splitter tp bt)))))

;; (display-image-tile-case "1111t^4444f-78999w11b")
;; (display-hands-ready "111t^444f^78999w11b")
;; (display-hands-ready "1112345678999b")
;; (display-hands-ready "567w^123w^2344555w")
;; (display-hands-ready "567w^123b^2223444t")
;; (display-hands-ready "2344466688t222j")
;; (display-hands-ready "1111j-2222j-3333j-4444f-3f")
;; (display-image-tile-case "1258w147t111b369b")
