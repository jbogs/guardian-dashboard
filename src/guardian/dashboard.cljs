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

#_(cell= (prn :hist-model hist-model))
#_(cell= (prn :data-model data-model))

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

(defn temp->color [t]
  (let [h (- 240 (/ (* 11 t) 5))]
    (hsl h (r 1 1) (r 1 2))))

;-- typography ----------------------------------------------------------------;

(def font-1     {:f 21 :ff ["MagistralC Bold" :sans-serif] :fc white})
(def font-2     {:f 18 :ff ["MagistralC Bold" :sans-serif] :fc white})
(def font-3     {:f 16 :ff ["MagistralC Bold" :sans-serif] :fc white})
(def font-4     {:f 14 :ff ["MagistralC Bold" :sans-serif] :fc white})
(def font-label {:f 14 :ff ["Lato Semibold"   :sans-serif] :fc black})
(def font-body  {:f 12 :ff ["Lato Medium"     :sans-serif] :fc black})

;-- controls ------------------------------------------------------------------;

(defelem primary-button [attrs elems]
  (elem font-label :pv 6 :ph 12 :c red :r 6
    attrs elems))

(defelem secondary-button [attrs elems]
  (elem font-label :pv 4 :ph 10 :c grey-5 :r 6
    attrs elems))

(defelem hue-slider [{:keys [sh sv s dir hue hue-changed] :as attrs} elems]
  (let [hue (cell= hue (or hue-changed identity))
        len (cell= (case dir 90 (or sh s) 180 (or sv s) 270 (or sh s) (or sv s)))
        pos (cell= (* (/ hue 360) len) #(reset! hue (int (* (/ % @len) 360))))
        col #(hsl % (r 1 1) (r 1 2))
        lgr (apply lgr dir (map col (range 0 360 10)))]
    (elem :pt pos :c lgr :click #(reset! pos (mouse-y %))
      (elem :s 20 :r 10 :c (cell= (col hue)) :b 2 :bc white :m :pointer)
      (dissoc attrs :dir :hue :hue-changed) elems)))

;;; views ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defelem panel [{:keys [icon name] :as attrs} elems]
  (elem (dissoc attrs :icon :name) :fc grey-1
    (elem :sh (r 1 1) :c grey-4 :p g-lg :gh g-lg
      (image :sv 40 :av :mid :url icon)
      (elem :sv 40 :av :mid
        name))
    (elem :sh (r 1 1) :sv (- (r 1 1) 40 g-lg) :pv g-sm :ph g-lg :c grey-5
      elems)))

(defelem panel-table [{:keys [name] :as attrs} elems]
  (elem (dissoc attrs :name)
    (elem :sh (r 1 1) :p g-sm :ah :mid
      name)
    elems))

(defelem panel-row [{:keys [name] :as attrs} elems]
  (elem :fc grey-4 :pv g-sm (dissoc attrs :name)
    (elem :sh (r 1 2)
      name)
    (elem :sh (r 1 2) :ah :end
      elems)))

(defelem card [{:keys [name icon] :as attrs} elems]
  (elem (dissoc attrs :icon)
    (elem font-3 :sh (r 1 1) :p 8
      name)
    (image :p 28 :sh (r 1 1) :a :mid :c grey-5 :url icon)
    (elem font-3 :p 6 :sh (r 1 1) :sv (r 1 5) :a :mid :c grey-3 :fc (white :a 0.5)
      elems)))

(defelem title [{:keys [name] :as attrs} elems]
  (elem font-1 :sh (r 1 1) :g g-sm :av :end
      (elem font-1
        name)
      (elem font-4 :fc red
        elems)))

(defn mb-view []
  (list
    (title :name (cell= (:name data-model))
      "Motherboard")
    (elem :sh (r 1 1) :g g-lg
      (for-tpl [{:keys [name temp] :as zone} (cell= (:zones data-model))]
        (list
          (v/histogram font-4 :sh (>sm (- (r 1 1) 300 g-lg)) :sv 300 :c grey-4 :b 10 :bc grey-5 :fc (white :a 0.6)
            :name name
            :icon "temperature-icon.svg"
            :data (cell= (mapv #(hash-map :value (-> % :temp :value) :color (-> % :temp :value temp->color)) (mapv #(some (fn [v] (when (= v zone) v)) (:zones %)) hist-model))))
          (v/gauge font-4 :sh (>sm 300) :sv 300 :c grey-4 :b 10 :bc grey-5
            :cfn  temp->color
            :data temp))))))

(defn cpu-view []
  (list
    (title :name (cell= (:name data-model))
      "CPU")
    (v/histogram font-4 :sh (>sm (- (r 1 1) 300 g-lg)) :sv 300 :c grey-4 :b 10 :bc grey-5 :fc (white :a 0.6)
      :name "CPU Load"
      :icon "capacity-icon.svg"
      :data (cell= (mapv #(hash-map :value (-> % :load :value) :color (-> % :temp :value temp->color)) hist-model)))
    (v/cpu-capacity font-4 :sh (>sm 300) :sv 300 :c grey-4 :b 10 :bc grey-5
      :cfn  temp->color
      :data data-model)
    #_(elem :g g-lg :av :end ;; remove after merging opts with vflatten
      (for-tpl [{:keys [name value]} (cell= (:volts data-model))]
        (card :sh 100 :name name :icon "mb-icon.svg"
          (cell= (str value "V")))))))

(defn gpu-view []
  (list
    (title :name (cell= (:name data-model))
      "GPU")
    (v/histogram font-4 :sh (>sm (- (r 1 1) 300 g-lg)) :sv 300 :c grey-4 :b 10 :bc grey-5 :fc (white :a 0.6)
      :name "CPU Load"
      :icon "capacity-icon.svg"
      :data (cell= (mapv #(hash-map :value (-> % :load :value) :color (-> % :temp :value temp->color)) hist-model)))
    (elem :g g-lg :av :end ;; remove after merging opts with vflatten
      (for-tpl [{:keys [name value]} (cell= (:loads data-model))]
        (card :sh 100 :name name :icon "mb-icon.svg"
          (cell= (str value "%")))))))

(defn memory-view []
  (list
    (title :name (cell= (:name data-model))
      "Memory")
    (v/dist-chart font-4 :sh (r 1 1) :sv 100 :c grey-5 :fc (white :a 0.5)
      :domain (cell= [{:label (->GB (:used data-model)) :value (:used data-model)} {:label (->GB (:free data-model)) :value (:free data-model)}])
      :range  [{:color :green} {:color grey-4}])
    (v/histogram font-4 :sh (r 1 1) :sv 400 :c grey-5 :fc (white :a 0.5)
      :name "Memory"
      :icon "copaciy-icon.svg"
      :data (cell= (mapv #(hash-map :label "" :value (* (/ (:used %) (:free %)) 400)) hist-model)))))

(defn hdd-view []
  (list
    (title :name (cell= (:name data-model))
      "Hard Drive")
    (v/dist-chart font-4 :sh (r 1 1) :sv 100 :c grey-5 :fc (white :a 0.5)
      :domain (cell= [{:label (->% (:used data-model)) :value (:used data-model)} {:label (->% (:free data-model)) :value (:free data-model)}])
      :range  [{:color :green} {:color grey-4}])
    (elem :sh (r 1 1) :sv 300 :c grey-4 :b 10 :bc grey-5)
    (card :sh 100 :name (cell= (-> data-model :temp :name)) :icon "mb-icon.svg"
      (cell= (-> data-model :temp :value (str "° C"))))))

(defn system-view []
  (list
     (elem :sh (>sm (r 2 5)) :sv (r 2 3) :p g-lg :c grey-6
       (image :s 34 :a :mid :url "mb-icon.svg"))
     (elem :sh (>sm (r 3 5)) :sv (r 2 3) :g 2
       (elem :sh (r 2 3) :sv (b nil sm (r 1 2)) :p g-lg :c grey-6
         (image :s 34 :a :mid :url "cpu-icon.svg"))
       (elem :sh (r 1 3) :sv (b nil sm (r 1 2)) :p g-lg :c grey-6
         "test")
       (elem :sh (r 2 3) :sv (b nil sm (r 1 2)) :p g-lg :c grey-6
         (image :s 34 :a :mid :url "memory-icon.svg"))
       (elem :sh (r 1 3) :sv (b nil sm (r 1 2)) :p g-lg :c grey-6
         "test"))
    (elem :sh (>sm 80) :sv (b nil sm (r 1 3)) :gv l
      (for-tpl [[idx {:keys [name type]}] (cell= (map-indexed vector (:views data)))]
        (let [selected (cell= (= idx (:index state)))]
          (elem font-4 :sh (r 1 1) :s 80 :ph g-lg :gh g-lg :ah (b :mid md :beg) :av :mid :c (cell= (if selected grey-4 grey-5)) :fc (cell= (if selected white grey-1)) :bl 2 :bc (cell= (if selected red grey-5)) :m :pointer :click #(change-state! @type)
            (image :s 34 :a :mid :url (cell= (when type (str (safe-name type) "-icon.svg"))))
            (when-tpl (b true sm false)
              (elem :sh (b 300 sm (- (r 1 1) 34 g-lg))
                name)))))
      (b nil sm (elem :sh (>sm 80) :sv (r 2 1) :c grey-6)))
    (elem :sh (>sm (- (r 1 1) 80 l)) :sv (r 1 3) :p g-lg :g g-lg :c grey-6
      (case-tpl (cell= (-> path second))
        :gpu (gpu-view)
        :hdd (hdd-view)))))

(defn keyboard-view []
  (elem :s (r 1 1) :c grey-6
    (title :name (cell= (:name data-model))
      "Keyboard")
    (elem :sh (r 1 1) :p 50 :g 50
      (for-tpl [{id :id z-name :name z-effect :effect [hue :as color] :color [beg-hue :as beg-color] :beg-color [end-hue :as end-color] :end-color :as zone} (cell= (:zones (:keyboard data)))]
        (let [zone (cell= zone #(s/set-keyboard-zone! @conn @id (:effect %) (:color %) (:beg-color %) (:end-color %)))]
          (elem :sh 160 :ah :mid :g 20
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
  (list
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
  :scroll (b true sm false) :c grey-4 :g 2
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
          (for-tpl [{label :label v :view} (cell= [{:view :system :label "System Monitor"} {:view :keyboard :label "Keyboard Settings"} {:view :fan :label "Fan Settings"}])]
            (let [selected (cell= (= view v))]
              (elem font-4 :sh (r 1 1) :s sh-close :ph g-lg :gh g-lg :ah (b :mid md :beg) :av :mid :c (cell= (if selected grey-4 grey-5)) :fc (cell= (if selected white grey-1)) :bl 2 :bc (cell= (if selected red grey-5)) :m :pointer :click #(change-state! @v :gpu)
                (image :s 34 :a :mid :url (cell= (when v (str (safe-name v) "-icon.svg"))))
                (when-tpl (b true sm false md true)
                  (elem :sh (b 300 sm (- (r 1 1) 34 g-lg))
                    label)))))
          (b nil sm (elem :sh (>sm sh-close md sh-open) :sv (r 2 1) :c grey-6)))
      (elem :sh (>sm (- (r 1 1) sh-close l) md (- (r 1 1) sh-open l)) :sv (b nil sm (r 1 1)) :g 2
        (case-tpl (cell= (-> path first))
          :system   (system-view)
          :fan      (fan-view)
          :keyboard (keyboard-view)))))))
