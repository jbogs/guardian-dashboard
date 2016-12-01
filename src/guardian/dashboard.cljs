(ns+ guardian.dashboard
  (:page
    "index.html")
  (:refer-clojure
    :exclude [-])
  (:require
    [chart.core :as c]
    [javelin.core    :refer [defc defc= cell= cell cell-doseq with-let]]
    [hoplon.core     :refer [defelem for-tpl when-tpl case-tpl]]
    [hoplon.ui       :refer [elem image window video s b]]
    [hoplon.ui.attrs :refer [- c r d]]))

;;; environment ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(enable-console-print!)

(def url "ws://jbog.pagekite.me:8000")

;;; content ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def footer-menu-items
  [["facebook-icon.svg"  "https://facebook.com"]
   ["instagram-icon.svg" "https://www.instagram.com/xoticpc/"]
   ["twitter-icon.svg"   "https://twitter.com/XoticPC"]
   ["youtube-icon.svg"   "https://www.youtube.com/channel/UCJ9O0vRPsMFk5UtIDimr6hQ"]])

;;; models ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defc state {:view :health :data {}})
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
  (.setInterval js/window call (or interval 3000) tag conn data))

(def sub-hardware-data (partial poll "get_hardware_data"))
(def get-smart-data    (partial call "get_smart_data"))
(def set-client-data   (partial call "set_client_data"))

;;; commands ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn change-view! [view]
  (swap! state assoc :view view))

(defn change-route! [[[view] _]]
  (change-view! view))

(defn initiate! [[path qmap] status _]
  (-> (connect url data error)
      (.then  #(sub-hardware-data %))
      (.catch #(.log js/console "error: " %))))

;;; styles ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;-- breakpoints ---------------------------------------------------------------;

(def sm 760)
(def md 1240)
(def lg 1480)

(defn >sm [& bks] (apply b (r 1 1) sm bks))

;-- sizes ---------------------------------------------------------------------;

(def l 2)
(def g 20)

;-- colors --------------------------------------------------------------------;

(def white (c 0xffffff))
(def grey  (c 0x4d4d4d))
(def black (c 0x111111))
(def red   (c 0xc32026))

;-- fonts ---------------------------------------------------------------------;

(def body-font   {:f 20 :ff ["Helvetica Neue" "Lucida Grande" :sans-serif] :fc grey})
(def button-font {:f 14 :ff ["Helvetica Neue" "Lucida Grande" :sans-serif] :fc grey})
(def title-font button-font)

;;; views ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defelem panel [{:keys [icon title] :as attrs} elems]
  (elem :gv 1 (dissoc attrs :icon :title)
    (elem :sh (r 1 1) :c black :p g :gh g
      (image :s 40 :a :mid :url icon)
      (elem :sv 40 :a :mid :fc grey title))
    (elem :sh (r 1 1) :sv (- (r 1 1) 40 g) :c black
      elems)))

(defn lighting-view []
  (elem title-font :sh (>sm (- (r 1 1) 60 2)) :sv (r 1 1) :p g :g g :c black
    (elem "LIGHTING VIEW")))

(defn fans-view []
  (elem title-font :sh (>sm (- (r 1 1) 60 2)) :sv (r 1 1) :p g :g g :c black
    (elem "FANS VIEW")))

(defn health-view []
  (elem title-font :sh (>sm (- (r 1 1) 60 2)) :sv (r 1 1) :p g :g g :c black
    (elem "HEALTH VIEW")))

(defn info-view []
  (elem title-font :sh (>sm (- (r 1 1) 60 2)) :sv (r 1 1) :p g :g g :c black
    (elem :sh (r 1 2) :p g :g g :c black :av :mid
      (image :url "pc-icon.svg")
      "PC NAME")
    (elem :sh (r 1 2) :p g :g g :c black :av :mid
      (image :url "os-icon.svg")
      "OPERATING SYSTEM")
    (elem :sh (r 1 2) :p g :g g :c black :av :beg
      (image :s 40 :a :mid :url "processor-icon.svg")
      (elem :sv 40 :a :mid "PROCESSOR"))
    (elem :sh (r 1 2) :p g :g g :c black :av :beg
      (image :s 40 :a :mid :url "video-card-icon.svg")
      (elem :sv 40 :a :mid"VIDEO CARD"))
    (elem :sh (r 1 2) :p g :g g :c black :av :beg
      (image :s 40 :a :mid :url "motherboard-icon.svg")
      (elem :sv 40 :a :mid "MOTHERBOARD"))
    (elem :sh (r 1 2) :p g :g g :c black :av :beg
      (image :s 40 :a :mid :url "memory-icon.svg")
      (elem :sv 40 :a :mid "MEMORY"))
    (elem :sh (r 1 2) :p g :g g :c black :av :beg
      (image :s 40 :a :mid :url "drive-icon.svg")
      (elem :sv 40 :a :mid "HDD 1"))
    (elem :sh (r 1 2) :p g :g g :c black :av :beg
      (image :s 40 :a :mid :url "drive-icon.svg")
      (elem :sv 40 :a :mid "HDD 2"))))

(window
  :title        "Xotic"
  :route        (cell= [[view]])
  :initiated    initiate!
  :routechanged change-route!
  :scroll true :c grey :g l
  (elem :sh (r 1 1) :ah :mid :c black
    (elem :sh (r 1 1) :ah (b :mid sm :beg) :av (b :beg sm :mid) :p g :gv g
      (image :sh 200 :url "xotic-pc-logo.svg" :m :pointer :click #(.open js/window "https://www.xoticpc.com"))
      (elem :sh (>sm (- (r 1 1) 200)) :ah (b :mid sm :end) :gh (* 2 g)
        (for [[logo link] footer-menu-items]
          (image :m :pointer :url logo :click #(.open js/window link))))))
  (elem :sh (r 1 1) :sv (- (r 1 1) 86 300 (* l 2)) :gh l
    (elem :sh (>sm 60) :ah :mid :gv l
      (elem :s 60 :a :mid :fc grey :c black :m :pointer :click #(change-view! :health)   "S")
      (elem :s 60 :a :mid :fc grey :c black :m :pointer :click #(change-view! :lighting) "L")
      (elem :s 60 :a :mid :fc grey :c black :m :pointer :click #(change-view! :fans)     "F")
      (elem :s 60 :a :mid :fc grey :c black :m :pointer :click #(change-view! :info)     "I")
      (elem :sh 60 :sv (- (r 1 1) (* 60 4)) :c black))
    (case-tpl view
      :health   (health-view)
      :lighting (lighting-view)
      :fans     (fans-view)
      :info     (info-view)))
  (elem :sh (r 1 1) :sv 300 :gh l
    (panel :sh (r 1 4) :sv (r 1 1) :title "MBS" :icon "motherboard-icon.svg"
      (for-tpl [{:keys [name temps fans]} (cell= (:mbs data))]
        (elem :sh (r 1 1) :p g :g g
          (elem :sh (r 1 1) :fc grey
            name)
          (for-tpl [{:keys [name value]} temps]
            (elem :sh (r 1 1) :fc grey
              (cell= (str name " " value "° C"))))
          (for-tpl [{:keys [name value]} fans]
            (elem :sh (r 1 1) :fc grey
              (cell= (str name " " value "RPM")))))))
    (panel :sh (r 1 4) :sv (r 1 1) :title "CPUS" :icon "processor-icon.svg"
      (for-tpl [{:keys [name temps loads]} (cell= (:cpus data))]
        (elem :sh (r 1 1) :p g :g g
          (elem :sh (r 1 1) :fc grey
            name)
          (for-tpl [{:keys [name value]} temps]
            (elem :sh (r 1 1) :fc grey
              (cell= (str name " " value "° C"))))
          (for-tpl [{:keys [name value]} loads]
            (elem :sh (r 1 1) :fc grey
              (cell= (str name " " value "%")))))))
    (panel :sh (r 1 4) :sv (r 1 1) :title "GPUS" :icon "video-card-icon.svg"
      (for-tpl [{:keys [name temps loads]} (cell= (:gpus data))]
        (elem :sh (r 1 1) :p g :g g
          (elem :sh (r 1 1) :fc grey
            name)
          (for-tpl [{:keys [name value]} temps]
            (elem :sh (r 1 1) :fc grey
              (cell= (str name " " value "° C"))))
          (for-tpl [{:keys [name value]} loads]
            (elem :sh (r 1 1) :fc grey
              (cell= (str name " " value "%")))))))
    (panel :sh (r 1 4) :sv (r 1 1) :title "HDDS" :icon "drive-icon.svg"
      (for-tpl [{:keys [name temps]} (cell= (:hdds data))]
        (elem :sh (r 1 1) :p g :g g
          (elem :sh (r 1 1) :fc grey
            name)
          (for-tpl [{:keys [name value]} temps]
            (elem :sh (r 1 1) :fc grey
              (cell= (str name " " value "° C")))))))))
