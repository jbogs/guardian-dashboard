(ns ^{:hoplon/page "index.html"} guardian.dashboard
  (:refer-clojure
    :exclude [-])
  (:require
    [adzerk.env                         :as e]
    [guardian.dashboard.visualizations  :as v]
    [guardian.dashboard.transformations :as x]
    [cljs.pprint     :refer [pprint]]
    [javelin.core    :refer [defc defc= cell= cell cell-let with-let]]
    [hoplon.core     :refer [defelem for-tpl when-tpl case-tpl]]
    [hoplon.ui       :refer [elem image window video s b]]
    [hoplon.ui.attrs :refer [- r d rgb hsl lgr]]))

;;; environment ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(enable-console-print!)

(e/def URL "ws://localhost:8000")

(def hist-max 120)

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

(def machine
  {:name    "MSI Gaming Series G Laptop"
   :devices [{:name "Micro-Star International Co., Ltd. MS-16H8"
              :type :mb
              :zones [{:id "THRM" :name "CPU"          :temps #queue[{:time #inst"2017-01-26T00:00:00" :value 30}
                                                                     {:time #inst"2017-01-26T00:00:01" :value 29}
                                                                     {:time #inst"2017-01-26T00:00:02" :value 30}
                                                                     {:time #inst"2017-01-26T00:00:03" :value 36}]}
                      {:id "TZ00" :name "North Bridge" :temps #queue[{:time #inst"2017-01-26T00:00:00" :value 27.7}
                                                                     {:time #inst"2017-01-26T00:00:01" :value 27.8}]}
                      {:id "TZ01" :name "South Bridge" :temps #queue[{:time #inst"2017-01-26T00:00:00" :value 29.8}
                                                                     {:time #inst"2017-01-26T00:00:01" :value 29.8}]}]}]})

(defc state {:view :mb :index 0 :data machine})
(defc error nil)

;;; queries ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defc= data  (-> state :data) #(swap! state assoc :data %))
(defc= view  (-> state :view))
(defc= model (get-in data [:devices (:index state 0)]))

(cell= (prn :view view))
(cell= (prn :model model))

;;; service ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defonce conn (atom nil))

(defn connect [url state error]
  (let [conn (reset! conn (js/WebSocket. url))
        cljs #(js->clj % :keywordize-keys true)
        data #(-> % .-data js/JSON.parse cljs :data x/xform)]
    (-> (fn [resolve reject]
          (set! (.-onopen    conn) #(resolve conn))
          (set! (.-onerror   conn) #(reject (reset! error %)))
          (set! (.-onmessage conn) #(reset! state (data %))))
        (js/Promise.))))

(defn call [tag conn & args]
  (->> {:tag tag :data (apply hash-map args)} (clj->js) (.stringify js/JSON) (.send conn)))

(defn poll [tag conn data & [interval]]
  (.setInterval js/window call (or interval 1000) tag conn data))

(def subs-sensors       (partial poll "get_sensors"))
(def get-devices        (partial call "get_devices"))
(def set-plugin-effect  (partial call "set_plugin_effect"))
(def set-keyboard-zones (partial call "set_keyboard_zones"))

;;; commands ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn change-state! [view & [index]]
  (swap! state assoc :view view :index index))

(defn change-route! [[[view {:keys [index]}] _]]
  (change-state! view index))

(defn initiate! [[path qmap] status _]
  #_(-> (connect URL data error)
      (.then  #(subs-sensors %))
      (.catch #(.log js/console "error: " %))))

(defn set-keyboard-hue! [zone hue]
  (set-keyboard-zones @conn :name "static_color" :zone (str zone) :color (x/hsl->rgb [hue 1 0.5])))

;;; styles ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;-- breakpoints ---------------------------------------------------------------;

(def sm 760)
(def md 1240)
(def lg 1480)

(defn >sm [& bks] (apply b (r 1 1) sm bks))

;-- sizes ---------------------------------------------------------------------;

(def l    2)
(def g-sm 6)
(def g-md 10)
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
  (let [h (-> (+ 20 t) (* 240) (/ 60) (- 20))]
    (hsl h (r 7 10) (r 1 2))))

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

(defelem card [{:keys [name icon values] :as attrs} elems]
  (elem :p g-md :g g-md :r 3 :c grey-5 (dissoc attrs :name :icon)
    (elem :sh (>sm (- (r 1 1) 250 g-md)) :sv 250 :d :pile :r 3 :c grey-4
      (v/histogram :s (r 1 1) :data (cell= (clj->js values)) :xfn #(.-time %) :yfn #(.-value %))
      (elem :sh (r 1 1) :ph 10 :pv 2 :av :mid
        (image :s 14 :url icon)
        (elem font-3 :sh (- (r 1 1) 24 10) :p 8 :fc (white :a 0.5)
          name)))
    (elem font-3 :sh (>sm 250) :sv 250 :d :pile :f 32 :ft :800 :fw 3 :fc (white :a 0.5) :r 3 :c grey-4
      (v/gauge :s (r 1 1) :yfn #(.-value %) :data (cell= (clj->js (last values))))
      (elem :s (r 1 1) :a :mid
        (cell= (str (:value (last values)) "°"))))))

(defelem title [{:keys [name] :as attrs} elems]
  (elem font-1 :sh (r 1 1) :g g-sm :av :end
      (elem font-1
        name)
      (elem font-4 :fc red
        elems)))

(defn mb-view []
  (list
    (title :name (cell= (:name model))
      "Motherboard")
    (elem :sh (r 1 1) :g g-lg ;; remove after merging opts with vflatten
      (for-tpl [{:keys [id name temps]} (cell= (:zones model))]
        (card :sh (r 1 1) :icon "temperature-icon.svg" :name name :values temps)))
    #_(elem :sh (r 1 1) :g g-lg ;; remove after merging apts with vflatten
      (for-tpl [{:keys [name value]} (cell= (:fans model))]
        (card :sh (r 1 1) :name name :icon "rpms-icon.svg"
          (cell= (str value "RPM")))))))

(defn cpu-view []
  #_(list
    (title :name (cell= (:name data-model))
      "CPU")
    (v/hist-chart font-4 :sh (- (r 1 1) 300 g-lg) :sv 300 :c grey-4 :b 10 :bc grey-5 :fc (white :a 0.5)
      :domain (cell= (mapv #(hash-map :value (* (/ (-> % :load :value) 100) 300) :color (-> % :temp :value temp->color)) hist-model))
      :range  [{:color :blue}])
    (elem :s 300 :c grey-4 :b 10 :bc grey-5
       (for-tpl [{:keys [name temp threads]} (cell= (:cores data-model))]
         (elem :sh (cell= (r 1 (count (:cores data-model)))) :sv (r 1 1) :gh 8 :ah :mid :av :end
           (for-tpl [{:keys [name load]} threads]
             (elem :sh 4 :sv (cell= (+ (* load 3) 6)) :r 6 :c (cell= (temp->color temp))))))) ;; can't use ratio because of https://github.com/hoplon/ui/issues/25
    (elem :g g-lg :av :end ;; remove after merging opts with vflatten
      (for-tpl [volts (cell= (apply mapv vector (mapv :volts hist-model)))]
        (card :sh (r 1 1) :name (cell= (-> volts last :name)) :icon "voltage-icon.svg" :values (cell= (mapv :value volts))
          (cell= (str (-> volts last :value) "V")))))))

(defn gpu-view []
  #_(list
    (title :name (cell= (:name data-model))
      "GPU")
    (elem :g g-lg :av :end ;; remove after merging opts with vflatten
      (for-tpl [loads (cell= (apply mapv vector (mapv :loads hist-model)))]
        (card :sh (r 1 1) :name (cell= (-> loads last :name)) :icon "capacity-icon.svg" :values (cell= (mapv :value loads))
          (cell= (str (-> loads last :value) "%")))))))

(defn memory-view []
  #_(list
    (title :name (cell= (:name data-model))
      "Memory")
    (v/dist-chart font-4 :sh (r 1 1) :sv 100 :c grey-5 :fc (white :a 0.5)
      :domain (cell= [{:label (->GB (:used data-model)) :value (:used data-model)} {:label (->GB (:free data-model)) :value (:free data-model)}])
      :range  [{:color :green} {:color grey-4}])
    (v/hist-chart font-4 :sh (r 1 1) :sv 400 :c grey-5 :fc (white :a 0.5)
      :domain (cell= (mapv #(hash-map :label "" :value (* (/ (:used %) (:free %)) 400)) hist-model))
      :range  [{:color :green}])))

(defn hdd-view []
  #_(list
    (title :name (cell= (:name data-model))
      "Hard Drive")
    (v/dist-chart font-4 :sh (r 1 1) :sv 100 :c grey-5 :fc (white :a 0.5)
      :domain (cell= [{:label (->% (:used data-model)) :value (:used data-model)} {:label (->% (:free data-model)) :value (:free data-model)}])
      :range  [{:color :green} {:color grey-4}])
    (card :sh (r 1 1) :name (cell= (-> data-model :temp :name)) :icon "voltage-icon.svg" :values (cell= (mapv (comp :value :temp) hist-model))
      (cell= (-> data-model :temp :value (str "° C"))))))

(defn keyboard-view []
  #_(list
    (title :name (cell= (:name data-model))
      "Keyboard")
    (elem :sh (r 1 1) :p 50 :g 50
      (for-tpl [{:keys [id name] [h s l] :color} (cell= (:zones data-model))]
       (elem :ah :mid :gv 20
         (hue-slider :sh 20 :sv 300 :r 10 :dir 180 :hue h :hue-changed #(set-keyboard-hue! @id %))
         (elem font-4 :sh (r 1 1) :ah :mid
           name))))))

(window
  :title        "Xotic"
  :route        (cell= [[view]])
  :initiated    initiate!
  :routechanged change-route!
  :scroll (b true sm false) :c grey-4 :g l
  (elem :sh (r 1 1) :ah :mid :c black
    (elem :sh (r 1 1) :ah (b :mid sm :beg) :av (b :beg sm :mid) :p g-lg :gv g-lg
      (image :sh 200 :url "xotic-pc-logo.svg" :m :pointer :click #(.open js/window "https://www.xoticpc.com"))
      (elem :sh (>sm (- (r 1 1) 200)) :ah (b :mid sm :end) :gh (* 2 g-lg)
        (for [[logo link] footer-menu-items]
          (image :m :pointer :url logo :click #(.open js/window link))))))
  (elem :sh (r 1 1) :sv (- (r 1 1) 80) :g l
    (elem :sh (>sm 80 md 380) :sv (b nil sm (r 1 1)) :gv l
      (for-tpl [[idx {:keys [name type]}] (cell= (map-indexed vector (:devices data)))]
        (let [selected (cell= (= idx (:index state)))]
          (elem font-4 :sh (r 1 1) :s 80 :ph g-lg :gh g-lg :ah (b :mid md :beg) :av :mid :c (cell= (if selected grey-4 grey-5)) :fc (cell= (if selected white grey-1)) :bl 2 :bc (cell= (if selected red grey-5)) :m :pointer :click #(change-state! @type @idx)
            (image :s 34 :a :mid :url (cell= (when type (str (safe-name type) "-icon.svg"))))
            (when-tpl (b true sm false md true)
              (elem :sh (b 300 sm (- (r 1 1) 34 g-lg))
                name)))))
      (b nil sm (elem :sh (>sm 80 md 380) :sv (r 2 1) :c grey-6)))
    (elem :sh (>sm (- (r 1 1) 80 l) md (- (r 1 1) 380 l)) :sv (r 2 1) :p g-lg :g g-lg :c grey-6
      (case-tpl view
        :mb       (mb-view)
        :cpu      (cpu-view)
        :gpu      (gpu-view)
        :memory   (memory-view)
        :hdd      (hdd-view)
        :keyboard (keyboard-view)))))
