(ns ^{:hoplon/page "index.html"} guardian.dashboard
  (:refer-clojure
    :exclude [-])
  (:require
    [adzerk.env       :as env]
    [guardian.visuals :as viz]
    [clojure.set     :refer [rename-keys]]
    [javelin.core    :refer [defc defc= cell= cell cell-let with-let]]
    [hoplon.core     :refer [defelem for-tpl when-tpl case-tpl]]
    [hoplon.ui       :refer [elem image window video s b]]
    [hoplon.ui.attrs :refer [- c r d]]))

;;; environment ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(enable-console-print!)

(env/def URL "ws://localhost:8000")

;;; content ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def footer-menu-items
  [["facebook-icon.svg"  "https://facebook.com"]
   ["instagram-icon.svg" "https://www.instagram.com/xoticpc/"]
   ["twitter-icon.svg"   "https://twitter.com/XoticPC"]
   ["youtube-icon.svg"   "https://www.youtube.com/channel/UCJ9O0vRPsMFk5UtIDimr6hQ"]])

;;; models ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defc state {:view :mb :data {}})
(defc error nil)

;;; queries ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defc= data (-> state :data) #(swap! state assoc :data %))
(defc= view (-> state :view))

(cell= (prn :state state))
#_(cell= (prn :error error))

;;; service ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn connect [url state error]
  (let [conn (js/WebSocket. url)
        cljs #(js->clj % :keywordize-keys true)
        data #(-> % .-data js/JSON.parse cljs :data)]
    (-> (fn [resolve reject]
          (set! (.-onopen    conn) #(resolve conn))
          (set! (.-onerror   conn) #(reject (reset! error %)))
          (set! (.-onmessage conn) #(reset! state (data %))))
        (js/Promise.))))

(defn call [tag conn data]
  (->> {:tag tag :data data} (clj->js) (.stringify js/JSON) (.send conn)))

(defn poll [tag conn data & [interval]]
  (.setInterval js/window call (or interval 100) tag conn data))

(def sub-hardware-data (partial poll "get_monitor_data"))
(def get-smart-data    (partial call "get_smart_data"))
(def set-client-data   (partial call "set_client_data"))

;;; commands ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn change-view! [view]
  (swap! state assoc :view view))

(defn change-route! [[[view] _]]
  (change-view! view))

(defn initiate! [[path qmap] status _]
  (-> (connect URL data error)
      (.then  #(sub-hardware-data %))
      (.catch #(.log js/console "error: " %))))

;;; styles ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;-- breakpoints ---------------------------------------------------------------;

(def sm 760)
(def md 1240)
(def lg 1480)

(defn >sm [& bks] (apply b (r 1 1) sm bks))

;-- sizes ---------------------------------------------------------------------;

(def l    2)
(def g-sm 6)
(def g-lg 20)

;-- colors --------------------------------------------------------------------;

(def white (c 0xffffff))
(def bgrey (c 0x4d4d4d))
(def cgrey (c 0x161616))
(def black (c 0x111111))
(def red   (c 0xc32026))

;-- fonts ---------------------------------------------------------------------;

(def body-font   {:f 20 :ff ["Helvetica Neue" "Lucida Grande" :sans-serif] :fc bgrey})
(def button-font {:f 14 :ff ["Helvetica Neue" "Lucida Grande" :sans-serif] :fc bgrey})
(def title-font button-font)

;;; views ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn xform-cpu [{:keys [name temps loads] :as cpu}]
  (when cpu
  (let [cores   (mapv (fn [t] (rename-keys t {:value :temp})) (remove #(= (:name %) "Package") temps))
        threads (mapv (fn [l] (rename-keys l {:value :load})) (remove #(= (:name %) "UC"     ) loads))]
    {:name  name
     :temp  (some #(when (= "Package" (:name %)) %) temps)
     :load  (some #(when (= "UC"      (:name %)) %) loads)
     :cores (mapv #(assoc % :threads %2) cores (partition 2 threads))})))

(defc= cpu (-> data :cpus first xform-cpu))

(defelem panel [{:keys [icon name] :as attrs} elems]
  (elem (dissoc attrs :icon :name) :fc bgrey
    (elem :sh (r 1 1) :c black :p g-lg :gh g-lg
      (image :sv 40 :av :mid :url icon)
      (elem :sv 40 :av :mid
        name))
    (elem :sh (r 1 1) :sv (- (r 1 1) 40 g-lg) :pv g-sm :ph g-lg :c cgrey
      elems)))

(defelem panel-table [{:keys [name] :as attrs} elems]
  (elem (dissoc attrs :name)
    (elem :sh (r 1 1) :p g-sm :ah :mid
      name)
    elems))

(defelem panel-row [{:keys [name] :as attrs} elems]
  (elem :fc bgrey :pv g-sm (dissoc attrs :name)
    (elem :sh (r 1 2)
      name)
    (elem :sh (r 1 2) :ah :end
      elems)))

(defn mb-view []
  (elem title-font :sh (>sm (- (r 1 1) 60 2)) :sv (r 1 1) :p g-lg :g g-lg :c cgrey
    (panel :sh (>sm (r 1 2) md (r 1 4)) :sv (b (r 1 2) md (r 1 1)) :name "MOTHERBOARD" :icon "motherboard-icon.svg"
      (cell-let [{:keys [name temps fans]} (cell= (:mb data))]
        (panel-table :sh (r 1 1) :name name
          (for-tpl [{:keys [name value]} temps]
            (panel-row :sh (r 1 1) :name name
               (cell= (str value "° C"))))
          (for-tpl [{:keys [name value]} fans]
            (panel-row :sh (r 1 1) :name name
               (cell= (str value "RPM")))))))))

(defn cpus-view []
  (elem title-font :sh (>sm (- (r 1 1) 60 2)) :sv (r 1 1) :p g-lg :g g-lg :ah :mid :c cgrey
    (elem :sh (r 1 1)
      (cell= (:name cpu)))
    (panel :sh (>sm (r 1 2) md (r 1 4)) :sv (b (r 1 2) md (r 1 1)) :name "CPUS" :icon "processor-icon.svg"
      (for-tpl [{:keys [name temps loads]} (cell= (:cpus data))]
        (panel-table :sh (r 1 1) :name name
          (for-tpl [{:keys [name value]} temps]
            (panel-row :sh (r 1 1) :name name
              (cell= (str value "° C"))))
          (for-tpl [{:keys [name value]} loads]
            (panel-row :sh (r 1 1) :name name
              (cell= (str value "%")))))))
    (viz/line-chart :sh (r 1 1) :sv (- (r 1 1) 30)
      :labels (->> (range) (take 11) (mapv #(str (* 10 %) "%")))
      :series [[0 5 8 10 8 10 8 10]]
      :styles {:line "stroke: red"})
    #_(elem :sh 400 :sv (- (r 1 1) 30)
      (for-tpl [{:keys [name temp threads]} (cell= (:cores cpu))]
        (elem :sh (r 1 (count (:cores @cpu))) :sv (r 1 1) :gh 10 :ah :mid :av :end
          (for-tpl [{:keys [name load]} threads]
            (elem :sh 10 :sv (cell= (+ (* load 6) 20)) :r 6 :c (cell= (condp < temp 40 :blue 50 :yellow :red))))))))) ;; can't use ratio because of https://github.com/hoplon/ui/issues/25

(defn gpus-view []
  (elem title-font :sh (>sm (- (r 1 1) 60 2)) :sv (r 1 1) :p g-lg :g g-lg :c cgrey
    (panel :sh (>sm (r 1 2) md (r 1 4)) :sv (b (r 1 2) md (r 1 1)) :name "GPUS" :icon "video-card-icon.svg"
      (for-tpl [{:keys [name temps loads]} (cell= (:gpus data))]
        (panel-table :sh (r 1 1) :name name
          (for-tpl [{:keys [name value]} temps]
            (panel-row :sh (r 1 1) :name name
              (cell= (str value "° C"))))
          (for-tpl [{:keys [name value]} loads]
            (panel-row :sh (r 1 1) :name name
              (cell= (str value "%")))))))))

(defn memory-view []
  (elem title-font :sh (>sm (- (r 1 1) 60 2)) :sv (r 1 1) :p g-lg :g g-lg :c cgrey
    "memory view"))

(defn hdds-view []
  (elem title-font :sh (>sm (- (r 1 1) 60 2)) :sv (r 1 1) :p g-lg :g g-lg :c cgrey
    (panel :sh (>sm (r 1 2) md (r 1 4)) :sv (b (r 1 2) md (r 1 1)) :name "DRIVES" :icon "drive-icon.svg"
      (for-tpl [{:keys [name temps]} (cell= (:hdds data))]
        (panel-table :sh (r 1 1) :name name
          (for-tpl [{:keys [name value]} temps]
            (panel-row :sh (r 1 1) :name name
              (cell= (str value "%")))))))
    (elem :sh (r 1 2) :p g-lg :g g-lg :c black :av :mid
      (image :url "pc-icon.svg")
      "PC NAME")
    (elem :sh (r 1 2) :p g-lg :g g-lg :c black :av :mid
      (image :url "os-icon.svg")
      "OPERATING SYSTEM")
    (elem :sh (r 1 2) :p g-lg :g g-lg :c black :av :beg
      (image :s 40 :a :mid :url "processor-icon.svg")
      (elem :sv 40 :a :mid "PROCESSOR"))
    (elem :sh (r 1 2) :p g-lg :g g-lg :c black :av :beg
      (image :s 40 :a :mid :url "video-card-icon.svg")
      (elem :sv 40 :a :mid "VIDEO CARD"))
    (elem :sh (r 1 2) :p g-lg :g g-lg :c black :av :beg
      (image :s 40 :a :mid :url "motherboard-icon.svg")
      (elem :sv 40 :a :mid "MOTHERBOARD"))
    (elem :sh (r 1 2) :p g-lg :g g-lg :c black :av :beg
      (image :s 40 :a :mid :url "memory-icon.svg")
      (elem :sv 40 :a :mid "MEMORY"))
    (elem :sh (r 1 2) :p g-lg :g g-lg :c black :av :beg
      (image :s 40 :a :mid :url "drive-icon.svg")
      (elem :sv 40 :a :mid "HDD 1"))
    (elem :sh (r 1 2) :p g-lg :g g-lg :c black :av :beg
      (image :s 40 :a :mid :url "drive-icon.svg")
      (elem :sv 40 :a :mid "HDD 2"))))

(defn os-view []
  (elem title-font :sh (>sm (- (r 1 1) 60 2)) :sv (r 1 1) :p g-lg :g g-lg :c cgrey
    "os view"))

(window
  :title        "Xotic"
  :route        (cell= [[view]])
  :initiated    initiate!
  :routechanged change-route!
  :c bgrey :g l
  (elem :sh (r 1 1) :ah :mid :c black
    (elem :sh (r 1 1) :ah (b :mid sm :beg) :av (b :beg sm :mid) :p g-lg :gv g-lg
      (image :sh 200 :url "xotic-pc-logo.svg" :m :pointer :click #(.open js/window "https://www.xoticpc.com"))
      (elem :sh (>sm (- (r 1 1) 200)) :ah (b :mid sm :end) :gh (* 2 g-lg)
        (for [[logo link] footer-menu-items]
          (image :m :pointer :url logo :click #(.open js/window link))))))
  (elem :sh (r 1 1) :sv (- (r 1 1) 88) :g l
    (elem :sh (>sm 60) :sv (r 1 1) :ah :mid :gv l
      (image :s 60 :p 10 :a :mid :fc bgrey :c black :m :pointer :click #(change-view! :mb)     :url "motherboard-icon.svg")
      (image :s 60 :p 10 :a :mid :fc bgrey :c black :m :pointer :click #(change-view! :cpus)   :url "processor-icon.svg")
      (image :s 60 :p 10 :a :mid :fc bgrey :c black :m :pointer :click #(change-view! :gpus)   :url "video-card-icon.svg")
      (image :s 60 :p 10 :a :mid :fc bgrey :c black :m :pointer :click #(change-view! :memory) :url "memory-icon.svg")
      (image :s 60 :p 10 :a :mid :fc bgrey :c black :m :pointer :click #(change-view! :hdds)   :url "drive-icon.svg")
      (image :s 60 :p 10 :a :mid :fc bgrey :c black :m :pointer :click #(change-view! :os)     :url "os-icon.svg")
      (b nil sm (elem :sh 60 :sv (- (r 1 1) (* 60 4) (- (* 4 l) l)) :c black)))
    (case-tpl view
      :mb     (mb-view)
      :cpus   (cpus-view)
      :gpus   (gpus-view)
      :memory (memory-view)
      :hdds   (hdds-view)
      :os     (os-view))))
