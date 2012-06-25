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
                        JScrollPane JTextField JSeparator SwingConstants
                        Box$Filler JRadioButton JCheckBox ButtonGroup
                        UIManager)
           (javax.swing.border BevelBorder)
           (javax.imageio ImageIO)
           (java.io File)
           (java.awt Image BorderLayout Container Component GridLayout FlowLayout Color Dimension, Graphics2D
                     Insets GridBagConstraints GridBagLayout)
           (java.awt.image BufferedImage)
           (java.awt.event ActionListener))
  (:use (mahjong tile comb dl locals))
  (:require (mahjong guobiao-util guobiao)))

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
    (.setLayout shelf (BoxLayout. shelf BoxLayout/X_AXIS))
    ;(.setLayout shelf (FlowLayout. FlowLayout/CENTER))
    (doall (map #(.add shelf %) components))
    shelf))

(defn tile-shelf [& components]
  (let [shelf (JPanel.)]
    (.setLayout shelf (BoxLayout. shelf BoxLayout/LINE_AXIS))
    (doseq [c components]
      (.add shelf c)
      (.add shelf (Box/createRigidArea (Dimension. 5 0))))
    shelf))

(defn stack [& components]
  (let [stack (JPanel.)]
    (.setLayout stack (BoxLayout. stack BoxLayout/Y_AXIS))
    (doseq [c components]
      (.setAlignmentX c Component/CENTER_ALIGNMENT)
      (.add stack c))
    stack))

(defn parse-result-panel [tile-num & components]
  (let [p (JPanel.)
        scroll (JScrollPane.)]
    (.setLayout p (BoxLayout. p BoxLayout/PAGE_AXIS))
    (doseq [c components]
      (.setAlignmentX c Component/CENTER_ALIGNMENT)
      (.add p c))
    (doto scroll
      (.setPreferredSize (Dimension. (* 50 tile-num) (min 600 (* 83 (count components)))))
      (.setViewportView p))
    scroll))

(defn splitter [top bottom]
  (doto (JSplitPane.)
    (.setOrientation JSplitPane/VERTICAL_SPLIT)
    (.setLeftComponent top)
    (.setRightComponent bottom)))

(defn hsplitter [left right]
  (doto (JSplitPane.)
    (.setOrientation JSplitPane/HORIZONTAL_SPLIT)
    (.setLeftComponent left)
    (.setRightComponent right)))



(defn button [text f]
  (doto (JButton. text)
    (.addActionListener
     (proxy [ActionListener] []
       (actionPerformed [_] (f))))))

(defn txt [cols t]
  (doto (JTextField.)
    (.setColumns cols)
    (.setText t)))

(defn tile-group [& components]
  (let [tile-group (JPanel.)]
    (.setLayout tile-group (BoxLayout. tile-group BoxLayout/X_AXIS))
    (doseq [c (butlast components)]
      (.add tile-group c)
      (.add tile-group (Box/createRigidArea (Dimension. -1 0))))
    (.add tile-group (last components))
    tile-group))

(declare image-label)
(defn make-tile-component [tile & {:keys [back lay-down hole] :or {back false lay-down true hole false}}]
  (let [image-name (if-not back (if lay-down
                                  (format "%s%dld.png" (clojure.string/lower-case (name (suit-sym tile)))
                                          (enum tile))
                                  (format "%s%d.png" (clojure.string/lower-case (name (suit-sym tile)))
                                          (enum tile))) 
                           "back.png")
        uri (format "images/small/%s" image-name)]
    (let [label (image-label uri)]
      (if hole
        (doto label
          (.setBorder (BorderFactory/createLineBorder Color/GREEN 3)))
        label))))

(defn display-tile-case [case]
  (let [char-seq (flatten (interpose 0x20 (map char-codes (all-comb-seq case))))]
    (.display gui (doto (javax.swing.JPanel.)
                    (.add (let [label (javax.swing.JLabel. (.toString (loop [seq char-seq bd (StringBuilder.)]
                                                                        (if (empty? seq)
                                                                          bd
                                                                          (recur (rest seq) (.appendCodePoint bd (first seq)))))))]
                            (.setFont label (java.awt.Font. "Symbola" java.awt.Font/PLAIN 36))
                            label))))))

(defn resize-image [img, w, h]
  (.getSubimage img 0 0 w h))

(defn image-icon [uri]
  (ImageIcon. (resize-image (ImageIO/read (File. uri)) 40 78)))

(defn image-label [uri]
  (doto (javax.swing.JLabel. (image-icon uri))))

(defn image-button [uri]
  (JButton. (image-icon uri)))

(defmulti make-comb-component
  "Make tile combination component."
  (fn [comb] (:tag (meta comb)))
  :default nil)

(defmethod make-comb-component :kong
  [comb]
  (let [tiles (tile-seq comb)]
    (apply tile-group (if (not (pub comb))
                        (list (make-tile-component (first tiles) :back true)
                              (make-tile-component (first tiles))
                              (make-tile-component (first tiles) :back true)
                              (make-tile-component (first tiles) :back true))
                        (map make-tile-component tiles)))))

(defmethod make-comb-component :free-tiles
  [comb]
  (apply tile-group (map #(make-tile-component % :lay-down false) (tile-seq comb))))

(defmethod make-comb-component :chow
  [comb]
  (let [hole (:hole comb)]
    (apply tile-group (map #(make-tile-component % :hole (and hole
                                                              (= (enum hole) (enum %))) )
                           (tile-seq comb)))))

(defmethod make-comb-component :pong
  [comb]
  (let [hole (:hole comb)]
    (apply tile-group (cons (make-tile-component (first (tile-seq comb)) :hole hole)
                            (map #(make-tile-component %) (rest (tile-seq comb)))))))

(defmethod make-comb-component :pair
  [comb]
  (let [hole (:hole comb)]
    (apply tile-group (list (make-tile-component (first (tile-seq comb)) :hole hole)
                            (make-tile-component (second (tile-seq comb)))))))

(defmethod make-comb-component :orphans
  [comb]
  (let [hole (:hole comb)]
    (apply tile-group (map #(make-tile-component % :hole (and hole
                                                              (= (enum hole) (enum %))
                                                              (= (suit hole) (suit %))))
                           (tile-seq comb)))))

(defmethod make-comb-component nil
  [comb]
  (apply tile-group (map make-tile-component (tile-seq comb))))

(defn display-image-tile-case [instr]
  (let [case (build-tile-case-from-ast (parse-dl-string instr))
        combs (all-comb-seq case)]
    (.display gui (apply shelf (map make-comb-component combs)))))

(defn- make-hands-ready-component [hands ready]
  (let [comb-seq (map make-comb-component (all-comb-seq hands ready))
        comb-num (count comb-seq)]
    comb-seq))

(defn- calc-fan [hands ready parse-result & {:keys [last-drawn-tile
                                                    last-discard-tile
                                                    supplemental-tile-of-melding-quad
                                                    appended-tile-to-melded-triplet
                                                    prevailing-wind
                                                    game-wind
                                                    self-draw
                                                    last-tile]
                                             :or {last-drawn-tile false
                                                  last-discard-tile false
                                                  supplemental-tile-of-melding-quad false
                                                  appended-tile-to-melded-triplet false
                                                  prevailing-wind 1
                                                  game-wind 1
                                                  self-draw false
                                                  last-tile false}}]

  (binding [mahjong.guobiao-util/*parse-result* parse-result
            mahjong.guobiao-util/*last-drawn-tile* last-drawn-tile
            mahjong.guobiao-util/*last-discard-tile* last-discard-tile
            mahjong.guobiao-util/*supplemental-tile-of-melding-quad* supplemental-tile-of-melding-quad
            mahjong.guobiao-util/*appended-tile-to-melded-triplet* appended-tile-to-melded-triplet
            mahjong.guobiao-util/*prevailing-wind* prevailing-wind
            mahjong.guobiao-util/*game-wind* game-wind
            mahjong.guobiao-util/*self-draw* self-draw
            mahjong.guobiao-util/*last-tile* last-tile]
    (mahjong.guobiao/calculate-fan hands ready)))

(defn point-board-pane []
  (let [pane (JPanel.)
        sum-pane (JPanel.)
        point-pane (JPanel.)]
    ;(.setLayout pane (GridLayout. (int (Math/ceil (/ (count points) 4))) 8))
    (letfn [(update [points]
              (.removeAll pane)
              (.removeAll sum-pane)
              (.removeAll point-pane)
              (.setLayout point-pane (GridLayout. (count points) 2))
              (doseq [[fan point] points]
                (.add point-pane (JLabel. (get-in *fan-name-table* [:zh_CN fan])))
                (.add point-pane (JLabel. (str point))))
              (.validate point-pane)
              
              (doto sum-pane
                (.setLayout (GridLayout. 1 2))
                (.add (JLabel. (get-in *ui-string* [:zh_CN :summury])))
                (.add (JLabel. (str (reduce + (map #(second %) points))
                                    (get-in *ui-string* [:zh_CN :fan])))))
              (.validate sum-pane)
              
              (doto pane
                (.setLayout (BorderLayout.))
                (.add point-pane BorderLayout/PAGE_START)
                (.add (JSeparator.) BorderLayout/CENTER)
                (.add sum-pane BorderLayout/SOUTH))
              (.validate pane)
              pane)
            (init [points]
              (update points))
            (dispath [f & args]
              (cond (= f 'update) (apply update args)
                    (= f 'init) (apply init args)))]
      dispath)))

(defn- wind-pane [pane-name]
  (let [pane (JPanel.)
        e-b (JRadioButton. (get-in *ui-string* [:zh_CN :east]))
        w-b (JRadioButton. (get-in *ui-string* [:zh_CN :west]))
        s-b (JRadioButton. (get-in *ui-string* [:zh_CN :south]))
        n-b (JRadioButton. (get-in *ui-string* [:zh_CN :north]))
        bt-group (ButtonGroup.)]
    (letfn [(get-env []
              (cond (.isSelected e-b) 1
                    (.isSelected s-b) 2
                    (.isSelected w-b) 3
                    (.isSelected n-b) 4))
            (init [action]
              (doto e-b
                (.setSelected true))
              (doto w-b
                (.setSelected false))
              (doto s-b
                (.setSelected false))
              (doto n-b
                (.setSelected false))
              (doto bt-group
                (.add e-b)
                (.add w-b)
                (.add s-b)
                (.add n-b))
              (doseq [c (list e-b w-b s-b n-b)]
                (.addActionListener c
                                    (proxy [ActionListener] []
                                      (actionPerformed [_] (action)))))
              (doto pane
                (.setLayout (GridLayout. 1 4))
                (.setBorder (BorderFactory/createTitledBorder (BorderFactory/createLineBorder Color/BLACK)
                                                              pane-name))
                (.add e-b)
                (.add s-b)
                (.add w-b)
                (.add n-b)))
            (dispath [f & args]
              (cond (= f 'get-env) (apply get-env args)
                    (= f 'init) (apply init args)
                    :else 'error))]
      dispath)))


(defn- win-tile-pane []
  (let [pane (JPanel.)
        last-draw (JRadioButton. (get-in *ui-string* [:zh_CN :last-draw]))
        last-discard (JRadioButton. (get-in *ui-string* [:zh_CN :last-discard]))
        supplemental-tile (JRadioButton. (get-in *ui-string* [:zh_CN :supplemental-tile]))
        rob-kong (JRadioButton. (get-in *ui-string* [:zh_CN :rob-kong]))
        self-draw (JRadioButton. (get-in *ui-string* [:zh_CN :self-draw]))
        win-by-discard (JRadioButton. (get-in *ui-string* [:zh_CN :win-by-discard]))
        last-tile (JCheckBox. (get-in *ui-string* [:zh_CN :last-tile]))
        bt-group (ButtonGroup.)]
    (letfn [(get-env []
              {:last-drawn-tile (if (.isSelected last-draw) true false)
               :last-discard-tile (if (.isSelected last-discard) true false)
               :supplemental-tile-of-melding-quad (if (.isSelected supplemental-tile) true false)
               :appended-tile-to-melded-triplet (if (.isSelected rob-kong) true false)
               :self-draw (if (.isSelected self-draw) true false)
               :last-tile (if (.isSelected last-tile) true false)})
            (init [action hands ready]
              (doto last-draw
                (.setSelected false))
              (doto last-discard
                (.setSelected false))
              (doto supplemental-tile
                (.setEnabled (not (empty? (concat (pub-kong-seq hands) (kong-seq hands)))))
                (.setSelected false))
              (doto rob-kong
                (.setEnabled (not-any? #(= (tile-name ready) (tile-name %)) (tile-seq hands)))
                (.setSelected false))
              (doto self-draw
                (.setSelected false))
              (doto win-by-discard
                (.setSelected true))
              (doto last-tile
                (.setEnabled (not-any? #(= (tile-name ready) (tile-name %)) (tile-seq (free-tiles (get-hands-case hands)))))
                (.setSelected false))
              (doto bt-group
                (.add last-draw)
                (.add last-discard)
                (.add supplemental-tile)
                (.add rob-kong)
                (.add self-draw)
                (.add win-by-discard))
              (doseq [c (list win-by-discard self-draw last-draw last-discard supplemental-tile rob-kong last-tile)]
                (.addActionListener c
                                    (proxy [ActionListener] []
                                      (actionPerformed [_] (action)))))
              (doto pane
                (.setLayout (GridLayout. 3 2))
                (.setBorder (BorderFactory/createLineBorder Color/BLACK))
                (.add win-by-discard)
                (.add self-draw)
                (.add last-draw)
                (.add last-discard)
                (.add supplemental-tile)
                (.add rob-kong)
                (.add last-tile)))
            (dispath [f & args]
              (cond (= f 'init) (apply init args)
                    (= f 'get-env) (apply get-env args)))]
      dispath)))

(defn env-pane [update hands ready]
  (let [pane (JPanel.)
        prevailing-wind-pane (wind-pane (get-in *ui-string* [:zh_CN :prevailing-wind]))
        game-wind-pane (wind-pane (get-in *ui-string* [:zh_CN :game-wind]))
        win-tile (win-tile-pane)]
    (letfn [(action []
              (update (merge {:prevailing-wind (prevailing-wind-pane 'get-env)
                              :game-wind (game-wind-pane 'get-env)}
                             (win-tile 'get-env))))]
      
      (doto pane
        (.setLayout (GridBagLayout.)))
      (let [c (GridBagConstraints.)]
        (set! (. c fill) GridBagConstraints/HORIZONTAL)
        (set! (. c gridx) 0)
        (set! (. c gridy) 0)
        (.add pane (prevailing-wind-pane 'init action) c))
      (let [c (GridBagConstraints.)]
        (set! (. c fill) GridBagConstraints/HORIZONTAL)
        (set! (. c gridx) 1)
        (set! (. c gridy) 0)
        (.add pane (game-wind-pane 'init action)  c))
      (let [c (GridBagConstraints.)]
        (set! (. c fill) GridBagConstraints/HORIZONTAL)
        (set! (. c gridwidth) 2)
        (set! (. c gridx) 0)
        (set! (. c gridy) 1)
        (.add pane (win-tile 'init action hands ready) c))
      pane)))

(defn fan-frame [hands ready parse-result]
  (let [tile-pane (JPanel.)
        sf (apply tile-shelf (list* (make-tile-component ready)
                                    (JSeparator. SwingConstants/VERTICAL)
                                    (make-hands-ready-component hands ready)))
        point-pane (point-board-pane)]
    (let [frame (mahjong.gui.MainFrame. "Fan")
          points (calc-fan hands ready parse-result)]
      (letfn [(update [env]
                (let [new-points (apply (partial calc-fan hands ready parse-result)
                                        (flatten (seq env)))]
                  (point-pane 'update new-points)
                  (.validate frame)
                  (.pack frame)))]
        (doto frame
          (.setResizable false)
          (.display (splitter (doto tile-pane
                                (.setAlignmentX Component/CENTER_ALIGNMENT)
                                (.add sf))
                              (doto (JPanel.)
                                (.setLayout (BorderLayout.))
                                (.setLayout (GridLayout. 0 2))
                                (.add (point-pane 'init points)  BorderLayout/LINE_START)
                                        ;(.add (JSeparator. SwingConstants/VERTICAL) BorderLayout/CENTER)
                                (.add (env-pane update hands ready) BorderLayout/LINE_END)))))))))

(defn tile-button [hands ready parse-result]
  (doto (image-button (format "images/small/%s%dld.png" (clojure.string/lower-case (name (suit-sym ready)))
                              (enum ready)))
    (.setMargin (Insets. -3 0 -5 0))
    (.setToolTipText (get-in *ui-string* [:zh_CN :calculate-fan]))
    (.addActionListener
     (proxy [ActionListener] []
       (actionPerformed [_] (fan-frame hands ready parse-result))))))

(defn display-hands-ready [instr]
  (let [case (build-tile-case-from-ast (parse-dl-string instr))
        parse-result (parse-hands-ready case)
        results (filter-duplicate-ready-hands parse-result)
        ;results (sort-ready-hands parse-result)
        tile-num (tile-num case)]
    (let [tp (doto (JPanel.)
               (.setAlignmentX Component/CENTER_ALIGNMENT)
               (.add (apply tile-shelf (map make-comb-component (all-comb-seq case)))))
          bt (apply parse-result-panel tile-num (map (fn [x]
                                                       (let [[ready hands] x]
                                                         (doto (apply tile-shelf
                                                                      (list* (tile-button hands ready parse-result)
                                                                             (JSeparator. SwingConstants/VERTICAL)
                                                                             (make-hands-ready-component hands ready)))
                                                           (.setBorder (BorderFactory/createEmptyBorder 2 2 5 2)))))
                                                     results))]
      (splitter tp bt))))

(defn alert
  ([msg] (alert nil msg))
  ([frame msg]
     (javax.swing.JOptionPane/showMessageDialog frame msg)))

(defn display-gui []
  (let [input (txt 20 "123456t12345w22j")
                                        ;(txt 20 "2344466688t222j")
        
        d (JPanel.)]
    (UIManager/setLookAndFeel (UIManager/getSystemLookAndFeelClassName))
    (.setResizable gui false)
    (.display gui
              (stack (shelf input (button (get-in *ui-string* [:zh_CN :go]) (fn []
                                                                              (doto d
                                                                                (.removeAll)
                                                                                (.add (display-hands-ready (.getText input)))
                                                                                (.validate))
                                                                              (.validate gui)
                                                                              (.pack gui))))
                     d))))

;(display-gui)
