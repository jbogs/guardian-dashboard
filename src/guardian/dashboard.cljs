(ns+ guardian.dashboard
  (:page
    "index.html")
  (:refer-clojure
    :exclude [-])
  (:require
    [adzerk.env :as env]
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
   ["xotic-pc-logo.svg"  "http://www.xoticpc.com/contact-us"]
   ["twitter-icon.svg"   "https://twitter.com/XoticPC"]
   ["youtube-icon.svg"   "https://www.youtube.com/channel/UCJ9O0vRPsMFk5UtIDimr6hQ"]])

;;; models ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defc state {:view :health :data {}})
(defc error nil)

;;; queries ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defc= data (-> state :data) #(swap! state assoc :data %))
(defc= view (-> state :view))

(cell= (prn :state state))
(cell= (prn :error error))
;;; service ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn connect [url state error]
  (let [conn (js/WebSocket. url)
        cljs #(js->clj % :keywordize-keys true)
        data #(-> % .-data js/JSON.parse cljs :data)]
    (-> (fn [resolve reject]
          (set! (.-onopen    conn) #(resolve conn))
          (set! (.-onerror   conn) #(do (reset! error %) (reject conn)))
          (set! (.-onmessage conn) #(reset! state (data %))))
        (js/Promise.))))

(defn send [tag conn data]
  (->> {:tag tag :data data} (clj->js) (.stringify js/JSON) (.send conn)))

(def get-hardware-data (partial send "get_hardware_data"))
(def get-smart-data    (partial send "get_smart_data"))

;;; commands ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn change-view! [view]
  (swap! state assoc :view view))

(defn change-route! [[[view] _]]
  (change-view! view))

(defn initiate! [[path qmap] status _]
  (-> (connect url data error)
      (.then  #(get-hardware-data %))
      (.catch #(.log js/console "error: " %))))

;;; styles ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;-- breakpoints ---------------------------------------------------------------;

(def sm 760)
(def md 1240)
(def lg 1480)

(defn >sm [& bks] (apply b (r 1 1) sm bks))

;-- sizes ---------------------------------------------------------------------;

(def g 20)

;-- colors --------------------------------------------------------------------;

(def white (c 0xffffff))
(def grey  (c 0x4d4d4d))
(def black (c 0x111111))
(def red   (c 0xc32026))

;-- fonts ---------------------------------------------------------------------;

(def body-font   {:f 20 :ff ["Helvetica Neue" "Lucida Grande" :sans-serif] :fc white})
(def button-font {:f 14 :ff ["Helvetica Neue" "Lucida Grande" :sans-serif] :fc white})
(def title-font button-font)

;;; views ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defelem panel [attrs elems]
  (elem :p 20 :c black attrs
    (elem :sh (r 1 1) :sv 60 :av :mid
      (image :s 40 :a :mid
        "icon")
      (elem :sh (- (r 1 1) 40) :fc white
        "title"))
    (elem :sh (r 1 1) :sv (- (r 1 1) (+ 60 20)) :c white
      elems)))

(defelem tab-button [{:keys [val] :as attrs} elems]
  (elem :ph 28 :ah :mid (dissoc attrs :val)
    (elem button-font :s (r 1 1) :a :mid :m :pointer :click #(change-view! val)
      :fc  (cell= (if (= view val) red  white))
      ;:url (cell= (if (= view val) "selected-buton.svg" "button.svg"))
      elems)))

(defn lighting-view []
  (elem title-font :sh (>sm 920 md 1240 lg 1400) :p g :g g
    (for-tpl [{:keys [name]} (cell= [{:name "Keyboard Lighting One"} {:name "Keyboard Lighting Two"} {:name "Keyboard Lighting Three"}])]
      (elem :sh (r 1 1) :p g :g g
        (elem :sh (r 1 1) :p g :c black
          name)
        (image :sh (r 1 3) :sv 400 :a :mid :url "fan-speed-bg.svg"
          "1365 RPM")
        (image :sh (r 1 3) :sv 400 :a :mid :url "zone-keyboard.svg"
          "1365 RPM")
        (elem :sh (r 1 3) :g g :sv 400 :ah :mid
          (elem :sh 100 :sv 40 :a :mid :r 6 :c black :b 2 :bc black :fc :green :m :pointer
            "On")
          (elem :sh (r 1 1) :sv 40 :a :mid :r 6 :c red :b 2 :bc black :m :pointer
            "Settings"))))))

(defn fans-view []
  (elem title-font :sh (>sm 920 md 1240 lg 1400) :p g :g g
    (for-tpl [{:keys [name]} (cell= [{:name "CPU Fan"} {:name "GPU Fan"}])]
      (elem :sh (r 1 1) :p g :g g
        (elem :sh (r 1 1) :p g :c black
          name)
        (image :sh (r 1 4) :sv 400 :a :mid :url "fan-speed-bg.svg"
          "1365 RPM")
        (elem :sh (r 3 4) :sv 400 :p g :ah :mid :c black
          "Motherboard Fan Graph")))))

(defn health-view []
  (elem title-font :sh (>sm 920 md 1240 lg 1400) :p g :g g
    (elem :sh (r 1 1)
      (for-tpl [{:keys [name load fan temp]} (cell= (cons (:cpu data) (:gpu_list data)))]
        (elem :sh (>sm (r 1 2)) :p g :g g
          (elem :sh (r 1 1) :p g :c black
            name)
          (image :sh (r 1 2) :a :mid :url "processor-temp-bg.svg"
            (cell= (str temp "° C")))
          (image :sh (r 1 2) :a :mid :url "processor-load-bg.svg"
            (cell= (str load "%")))
          (elem :sh (r 1 1) :sv 400 :p g :ah :mid :c black
            "CPU GRAPH"))))
    (elem :sh (r 1 1) :p g :g g
      (elem :sh (r 1 1) :p g :c black
        "Memory Info")
      (elem :sh (>sm (r 3 4)) :c black :b 2 :bc white
        (elem :sh (r 2 5) :sv (r 1 1) :c red :p g "9.08GB"))
      (elem :sh (>sm (r 1 4)) :c black :b 2 :bc white
        (elem :sh (r 14 50) :sv (r 1 1) :c red :p g "28%")))
    (elem :sh (>sm (r 2 3))
      (for-tpl [{:keys [name temp]} (cell= (:hdd_list data))]
        (elem :sh (>sm (r 1 2)) :p g :g g
          (elem :sh (r 1 1) :p g :c black
            name)
          (image :sh (r 1 2) :a :mid :url "processor-temp-bg.svg"
            (cell= (str temp "° C")))
          (image :sh (r 1 2) :a :mid :url "processor-load-bg.svg"
            "27%"))))
    (elem :sh (>sm (r 1 3)) :p g :g g
      (elem :sh (r 1 1) :p g :c black
        (cell= (-> data :mb :name)))
        (elem :sh (r 1 2) 
          (for-tpl [{:keys [rpm]} (cell= (-> data :mb :fan_list))]
            (image :sh (r 1 2) :a :mid :url "fan-speed-bg.svg"
              (cell= (str rpm " RPM")))))
        (elem :sh (r 1 2)
          (for-tpl [{:keys [temp]} (cell= (-> data :mb :temp_list))]
            (image :sh (r 1 2) :a :mid :url "fan-speed-bg.svg"
              (cell= (str temp "° C"))))))))

(defn info-view []
  (elem title-font :sh (>sm 920 md 1240 lg 1400) :p 42 :g 42
    (elem :sh (r 1 2) :p g :g g :c black :av :mid
      (image :url "pc-icon.svg")
      "PC NAME")
    (elem :sh (r 1 2) :p g :g g :c black :av :mid
      (image :url "os-icon.svg")
      "OPERATING SYSTEM")
    (elem :sh (r 1 2) :sv 400 :p g :g g :c black :av :beg
      (image :s 40 :a :mid :url "processor-icon.svg")
      (elem :sv 40 :a :mid "PROCESSOR"))
    (elem :sh (r 1 2) :sv 400 :p g :g g :c black :av :beg
      (image :s 40 :a :mid :url "video-card-icon.svg")
      (elem :sv 40 :a :mid"VIDEO CARD"))
    (elem :sh (r 1 2) :sv 400 :p g :g g :c black :av :beg
      (image :s 40 :a :mid :url "motherboard-icon.svg")
      (elem :sv 40 :a :mid "MOTHERBOARD"))
    (elem :sh (r 1 2) :sv 400 :p g :g g :c black :av :beg
      (image :s 40 :a :mid :url "memory-icon.svg")
      (elem :sv 40 :a :mid "MEMORY"))
    (elem :sh (r 1 2) :sv 400 :p g :g g :c black :av :beg
      (image :s 40 :a :mid :url "drive-icon.svg")
      (elem :sv 40 :a :mid "HDD 1"))
    (elem :sh (r 1 2) :sv 400 :p g :g g :c black :av :beg
      (image :s 40 :a :mid :url "drive-icon.svg")
      (elem :sv 40 :a :mid "HDD 2"))
    (elem :sh (r 1 1) :p g :c black :av :mid
      "Contact & Support")))

(window
  :title        "Xotic"
  :route        (cell= [[view]])
  :initiated    initiate!
  :routechanged change-route!
  :scroll true
  (elem :sh (r 1 1) :ah :mid :c black :bb 6 :bc red
    (elem :sh (r 1 1) :p g :ah (b :mid sm :beg) :av (b :beg sm :mid)
      (image :sh 200 :url "xotic-pc-logo.svg" :m :pointer :click #(.open js/window "https://www.xoticpc.com")))
    (elem :sh (>sm 920 md 1240 lg 1400) :p g :av (b :beg sm :end)
      (tab-button :sh (>sm (r 11 50)) :val :health   "SYSTEM HEALTH")
      (tab-button :sh (>sm (r 11 50)) :val :lighting "LIGHTING")
      (elem       :sh (>sm (r 6  50)))
      (tab-button :sh (>sm (r 11 50)) :val :fans     "FANS")
      (tab-button :sh (>sm (r 11 50)) :val :info     "INFO")))
  (elem :sh (r 1 1) :ah :mid :c grey
    (case-tpl view
      :health   (health-view)
      :lighting (lighting-view)
      :fans     (fans-view)
      :info     (info-view)))
  (elem :sh (r 1 1) :ah :mid :c black
    (elem :sh (>sm 920 md 1240 lg 1400) :p g :g g
      (for [[logo link] footer-menu-items :let [n (count footer-menu-items)]]
        (elem :sh (>sm (r 1 n)) :a :mid
          (image :m :pointer :url logo :click #(.open js/window link)))))))
