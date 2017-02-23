(ns ^{:hoplon/page "index.html"} guardian.dashboard
  (:refer-clojure
    :exclude [-])
  (:require
    [adzerk.env                         :as e]
    [guardian.dashboard.visualizations  :as v]
    [guardian.dashboard.service         :as s]
    [cljs.pprint     :refer [pprint]]
    [javelin.core    :refer [defc defc= cell= cell cell-let with-let]]
    [hoplon.core     :refer [defelem if-tpl when-tpl for-tpl case-tpl]]
    [hoplon.ui       :refer [elem image window video s b]]
    [hoplon.ui.attrs :refer [- r d rgb hsl lgr]]))

;;; environment ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(enable-console-print!)

(e/def URL "ws://localhost:8000")

(def hist-max 180)

;;; utils ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn ->GB [bytes] (when bytes (str (.toFixed (/ bytes 1000000000) 2) "GB")))
(defn ->% [num]    (when num (str (.toFixed num) "%")))
(defn safe-name [keyword] (try (name keyword) (catch js/Error _)))

(defn rect    [e] (.getBoundingClientRect (.-currentTarget e)))
(defn mouse-x [e] (- (.-pageX e) (.-left (rect e))))
(defn mouse-y [e] (- (.-pageY e) (.-top  (rect e))))

;;; content ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def footer-menu-items
  [["facebook-icon.svg"  "https://www.facebook.com/xoticpc/"]
   ["instagram-icon.svg" "https://www.instagram.com/xoticpc/"]
   ["twitter-icon.svg"   "https://twitter.com/XoticPC"]
   ["youtube-icon.svg"   "https://www.youtube.com/channel/UCJ9O0vRPsMFk5UtIDimr6hQ"]])

;;; data-models ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defonce conn (atom nil))

(defc state {:path [:system :gpu] :index 0 :hist #queue[]})
(defc error nil)

;;; queries ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defc= hist  (-> state :hist))
(defc= data  (-> hist  last) #(swap! state update :hist (fn [h] (conj (if (> (count h) hist-max) (pop h) h) %))))
(defc= path  (-> state :path))
(defc= view  (-> path first))

(defc= hist-model (mapv #(get-in % [:devices (:index state 0)]) hist))
(defc= data-model (-> hist-model last))

#_(cell= (cljs.pprint/pprint (-> state :hist last)))

;;; commands ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn change-state! [& args]
  (swap! state assoc :path (vec args)))

(defn change-route! [[[view] _]]
  (change-state! view))

(defn initiate! [[path qmap] status _]
  (when-not @conn
    (-> (s/connect URL)
        (.then  #(reset! conn (s/bind-sensors! % data error 1000 120)))
        (.catch #(.log js/console "error: " %)))))

(defn set-keyboard-hue! [zone hue]
  (s/set-keyboard-zone! @conn zone [hue 1 0.5]))

;;; styles ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;-- breakpoints ---------------------------------------------------------------;

(def sm 760)
(def md 1240)
(def lg 1480)

(defn >sm [& bks] (apply b (r 1 1) sm bks))

;-- sizes ---------------------------------------------------------------------;

(def l    2)
(def g-sm 6)
(def g-lg 16)

;-- colors --------------------------------------------------------------------;

(def white   (rgb 0xFAFAFA))
(def red     (rgb 0xCC181E))
(def yellow  (rgb 0xFFD200))
(def grey-1  (rgb 0x777777))
(def grey-2  (rgb 0x555555))
(def grey-3  (rgb 0x414141))
(def grey-4  (rgb 0x333333))
(def grey-5  (rgb 0x202020))
(def grey-6  (rgb 0x161616))
(def black   (rgb 0x181818))

(defn temp->color [d1 d2]
  (fn [t]
    (let [h (v/scale-linear [d1 d2] [260 0])]
      (hsl (h t) (r 4 5) (r 9 20)))))

;-- typography ----------------------------------------------------------------;

(def font-1     {:f 21 :ff ["MagistralC Bold" :sans-serif] :fc white})
(def font-2     {:f 18 :ff ["MagistralC Bold" :sans-serif] :fc white})
(def font-3     {:f 16 :ff ["MagistralC Bold" :sans-serif] :fc white})
(def font-4     {:f 14 :ff ["MagistralC Bold" :sans-serif] :fc white})
(def font-label {:f 14 :ff ["Lato Semibold"   :sans-serif] :fc black})
(def font-body  {:f 12 :ff ["Lato Medium"     :sans-serif] :fc black})

;-- controls ------------------------------------------------------------------;

(defelem hue-slider [{:keys [sh sv s dir hue hue-changed] :as attrs} elems]
  (let [hue (cell= hue (or hue-changed identity))
        len (cell= (case dir 90 (or sh s) 180 (or sv s) 270 (or sh s) (or sv s)))
        pos (cell= (* (/ hue 360) len) #(reset! hue (int (* (/ % @len) 360))))
        col #(hsl % (r 1 1) (r 1 2))
        lgr (apply lgr dir (map col (range 0 360 10)))]
    (elem :pt pos :c lgr :click #(reset! pos (mouse-y %))
      (elem :s 20 :r 10 :c (cell= (col hue)) :b 2 :bc (white :a 0.8) :m :pointer)
      (dissoc attrs :dir :hue :hue-changed) elems)))

;;; views ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defelem panel [{:keys [items selected-index name-fn value-fn] :as attrs} elems]
  (let [selected-index (cell= (when (> (count items) 1) (or selected-index 0)))
        selected-item  (cell= (get items (or selected-index 0)))]
    (elem :c grey-6 :fc grey-1 (dissoc attrs :items :index)
      (elem :sh (r 1 1) :sv 64 :bb 2 :bc grey-6
        (for-tpl [[idx {:keys [name type] :as item}] (cell= (map-indexed vector items))]
          (let [selected (cell= (and (= idx selected-index)))]
            (elem :sh 64 :sv (r 1 1) :a :mid :bt 2 :m :pointer
              :c     (cell= (if selected grey-5 grey-6))
              :bc    (cell= (if selected red    grey-5))
              :click #(swap! state assoc (keyword (str "selected-" (safe-name @type) "-index")) @idx)
              (image :s 34 :a :mid :url (cell= (when type (str (safe-name type) "-icon.svg")))))))
        (elem font-4 :sh (cell= (- (r 1 1) (-> items count (* 64)))) :sv (r 1 1) :ph g-lg :av :mid
          (elem :sh (- (r 1 1) 100) :sv (r 1 1) :f (b 14 sm 12 md 16 lg 18) :l :text
            (cell= (name-fn selected-item)))
          (elem :sh 100 :ah :end :l :text :f (b 16 sm 14 md 18 lg 21) :fc (white :a 0.6)
            (cell= (value-fn selected-item)))))
      (elem :sh (r 1 1) :sv (- (r 1 1) 64)
        elems))))

(defelem title [{:keys [name] :as attrs} elems]
  (elem font-1 :sh (r 1 1) :g g-sm :av :end
      (elem font-1
        name)
      (elem font-4 :fc red
        elems)))

(defn system-view []
  (let [sv-sm     280
        mem-hist  (cell= (mapv :memory (:hist state)))
        cpus-hist (cell= (mapv #(get (:cpus %)           (:selected-cpu-index           state 0)) (:hist state)))
        gcs-hist  (cell= (mapv #(get (:graphics-cards %) (:selected-graphics-card-index state 0)) (:hist state)))
        hds-hist  (cell= (mapv #(get (:hard-drives    %) (:selected-hard-drive-index    state 0)) (:hist state)))
        gc        (cell= (get (:graphics-cards data) (:selected-graphics-card-index state 0)))
        cpu-color (temp->color 20 80)
        hdd-color (temp->color 20 50)]
    (list
      (panel :sh (r 1 1) :sv (b (* sv-sm 2) sm (r 1 3)) :gh 5 :c grey-6
        :name-fn        :name
        :value-fn       #(str (:value (:load %)) "% " (:value (:temp %)) "°")
        :items          (cell= (:cpus data))
        :selected-index (cell= (:selected-cpu-index state))
        (v/histogram font-4 :sh (>sm (r 3 4)) :sv (b (r 1 2) sm (r 1 1)) :c grey-5 :fc (white :a 0.6)
          :name "CPU Load & Temperature"
          :icon "cpu-icon.svg"
          :data (cell= (mapv #(hash-map :value (-> % :load :value) :color (-> % :temp :value cpu-color)) cpus-hist)))
        (v/cpu-capacity font-4 :sh (>sm (r 1 4)) :sv (b (r 1 2) sm (r 1 1)) :c grey-5 :bl (b 0 sm 2) :bt (b 2 sm 0) :bc grey-4
          :cfn  cpu-color
          :data (cell= (get (:cpus data) (:selected-cpu-index state 0)))))
      (panel :sh (r 1 1) :sv (b (* sv-sm 2) sm (r 1 3)) :gh 5 :c grey-6
        :name-fn        :name
        :value-fn       #(when-let [gpu (:gpu %)] % (str (-> gpu  :load :value) "% " (-> gpu :temp :value) "°"))
        :items          (cell= (:graphics-cards data))
        :selected-index (cell= (:selected-graphics-card-index state))
        (if-tpl (cell= (:integrated? gc))
          (elem font-4 :s (r 1 1) :a :mid :fc (white :a 0.6)
            "No sensor data available for integrated GPU.")
          (list
            (v/histogram font-4 :sh (>sm (r 3 4)) :sv (b (r 1 2) sm (r 1 1)) :c grey-5 :fc (white :a 0.6)
              :name "GPU Load"
              :icon "capacity-icon.svg"
              :data (cell= (mapv #(hash-map :value (-> % :gpu :load :value) :color (-> % :gpu :temp :value cpu-color)) gcs-hist)))
            (v/gpu-capacity font-4 :sh (>sm (r 1 4)) :sv (b (r 1 2) sm (r 1 1)) :c grey-5 :bl (b 0 sm 2) :bt (b 2 sm 0) :bc grey-4
              :cfn  cpu-color
              :data gc))))
      (panel :sh (>sm (r 1 2)) :sv (b sv-sm sm (r 1 3)) :c grey-6
        :name-fn        :name
        :value-fn       #(-> % :used :value (/ 1000000000) (.toFixed 2) (str "G"))
        :items (cell= [(:memory data)])
        (v/histogram font-4 :s (r 1 1) :c grey-5 :fc (white :a 0.6)
          :name "Memory Utilization"
          :icon "memory-icon.svg"
          :data (cell= (mapv #(hash-map :value (* (/ (-> % :used :value) (-> % :total :value)) 100) :color "grey") mem-hist))))
      (panel :sh (>sm (r 1 2)) :sv (b sv-sm sm (r 1 3)) :c grey-6
        :name-fn        #(apply str (:name %) " " (interpose " & " (mapv :name (:volumes %))))
        :value-fn       #(str (-> % :load :value int) "% " (-> % :temp :value) "°")
        :items          (cell= (:hard-drives data))
        :selected-index (cell= (:selected-hard-drive-index state))
        (v/histogram font-4 :s (r 1 1) :c grey-5 :fc (white :a 0.6)
          :name "Drive Utilization"
          :icon "capacity-icon.svg"
          :data (cell= (mapv #(hash-map :value (-> % :load :value) :color (-> % :temp :value hdd-color)) hds-hist)))))))

(defn keyboard-view []
  (elem :sh (r 1 1) :sv (b (- js/window.innerHeight 113 246 l) sm (r 1 1)) :p g-lg :c grey-6
    (title :name (cell= (-> data :keyboard :name))
      "Keyboard")
    (elem :s (r 1 1) :a :mid :p (b g-sm sm 50) :g (b g-sm sm 50)
      (for-tpl [{id :id z-name :name z-effect :effect [hue :as color] :color [beg-hue :as beg-color] :beg-color [end-hue :as end-color] :end-color :as zone} (cell= (:zones (:keyboard data)))]
        (let [zone (cell= zone #(s/set-keyboard-zone! @conn @id (:effect %) (:color %) (:beg-color %) (:end-color %)))]
          (elem :sh 140 :ah :mid :g 40
            (elem :sh (r 1 1) :b 2 :bc grey-2
              (for [[effect [e-name _]] s/effects]
                (elem font-4 :sh (r 1 1) :p 8 :m :pointer :c (cell= (if (= effect z-effect) grey-4 grey-5)) :fc (cell= (if (= effect z-effect) white grey-1)) :click #(swap! zone assoc :effect effect)
                  e-name)))
            (if-tpl (cell= (= z-effect :color))
              (hue-slider :sh 20 :sv 300 :r 10 :dir 180 :hue hue :hue-changed #(swap! zone assoc :color [% 1 0.5]))
              (list
                (hue-slider :sh 20 :sv 300 :r 10 :dir 180 :hue beg-hue :hue-changed #(swap! zone assoc :beg-color [% 1 0.5]))
                (hue-slider :sh 20 :sv 300 :r 10 :dir 180 :hue end-hue :hue-changed #(swap! zone assoc :end-color [% 1 0.5]))))
            (elem font-4 :sh (r 1 1) :ah :mid
              z-name)))))))

(defn fan-view []
  (elem :sh (r 1 1) :sv (b (- js/window.innerHeight 113 246 l) sm (r 1 1)) :p g-lg :c grey-6
    (title :name (cell= (:name data-model))
      "Fans")
    (elem :sh (r 1 1) :p 50 :g 50
      (for-tpl [{:keys [id name] [h s l] :color} (cell= (:zones data-model))]
       (elem :ah :mid :gv 20
         (hue-slider :sh 20 :sv 300 :r 10 :dir 180 :hue h :hue-changed #(set-keyboard-hue! @id %))
         (elem font-4 :sh (r 1 1) :ah :mid
           name))))))

(window
  :title        "Xotic"
  :route        (cell= [path])
  :initiated    initiate!
  :routechanged change-route!
  :scroll       (b true sm false) :c grey-4 :g 2
  (elem :sh (r 1 1) :ah :mid :c black
    (elem :sh (r 1 1) :ah (b :mid sm :beg) :av (b :beg sm :mid) :p g-lg :gv g-lg
      (image :sh 200 :url "xotic-pc-logo.svg" :m :pointer :click #(.open js/window "https://www.xoticpc.com"))
      (elem :sh (>sm (- (r 1 1) 200)) :ah (b :mid sm :end) :gh (* 2 g-lg)
        (for [[logo link] footer-menu-items]
          (image :m :pointer :url logo :click #(.open js/window link))))))
  (if-tpl (cell= (not data))
    (elem :s (r 1 1) :pb 200 :a :mid :c black
      (elem :s 100 :g 10 :ah :mid
        (image :url "loading-icon.png")
        (elem :sh (r 1 1) :ah :mid font-2 :fc (white :a 0.9)
          "connecting")))
    (let [sh-close 80 sh-open 240]
      (elem :sh (r 1 1) :sv (- (r 1 1) 80) :g l
        (elem :sh (>sm sh-close md sh-open) :sv (b nil sm (r 3 5)) :gv l
          (for-tpl [{label :label v :view} (cell= [{:view :system :label "System Monitor"} {:view :keyboard :label "Keyboard Settings"} #_{:view :fan :label "Fan Settings"}])]
            (let [selected (cell= (= view v))]
              (elem font-4 :sh (r 1 1) :s sh-close :ph g-lg :gh g-lg :ah (b :mid md :beg) :av :mid :c (cell= (if selected grey-4 grey-5)) :fc (cell= (if selected white grey-1)) :bl 2 :bc (cell= (if selected red grey-5)) :m :pointer :click #(change-state! @v :gpu)
                (image :s 34 :a :mid :url (cell= (when v (str (safe-name v) "-icon.svg"))))
                (when-tpl (b true sm false md true)
                  (elem :sh (b 120 sm (- (r 1 1) 34 g-lg))
                    label)))))
          (b nil sm (elem :sh (>sm sh-close md sh-open) :sv (r 2 1) :c grey-6)))
      (elem :sh (>sm (- (r 1 1) sh-close l) md (- (r 1 1) sh-open l)) :sv (b nil sm (r 1 1)) :g 2
        (case-tpl (cell= (-> path first))
          :system   (system-view)
          :fan      (fan-view)
          :keyboard (keyboard-view)))))))
