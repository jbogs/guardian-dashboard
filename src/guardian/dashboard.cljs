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

(env/def JBOGHOST "cov.us.to")

(def dev (= js/location.hostname "localhost"))
(def url (if dev (str "ws://" JBOGHOST ":8000") "ws://localhost:8000"))

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

;;; service ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn connect [url state error]
  (let [conn (js/WebSocket. url)
        cljs #(js->clj % :keywordize-keys true)
        data #(-> % .-data js/JSON.parse cljs)]
    (-> (fn [resolve reject]
          (set! (.-onopen    conn) #(resolve conn))
          (set! (.-onerror   conn) #(->> (.-error %) (reset! error) (reject)))
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

;;; from plotSVG ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defn by-id [id]
;;   (.getElementById js/document (name id)))

;; (defn val-id [id]
;;   (do! (by-id id) :value))

;; ;; colors
;; (def   c1      "#006666")
;; (def   c2      "#660066")

;; ;; returns a seq of random [x y] pairs, 0 <= y <= 9
;; (defn data! []
;;   (vec (for [x (range 0 11)] [x (rand-int 11)])))

;; ;; push a random value onto the end of a data series
;; (defn add! [data]
;;   (conj data [(-> data last first inc) (rand-int 11)]))

;; ;; two data series
;; (defc  data1   (data!))
;; (defc  data2   (data!))
;; ;; user configurations "knobs"
;; (defc  pwidth  400)
;; (defc  paused? false)

;; ;; "clipped" data (moving strip-chart)
;; (defc= series1 (take-last 10 data1))
;; (defc= series2 (take-last 10 data2))

;; ;; configure the plotting envelope (linear scale)
;; (defc= chart1
;;   (let [min-x (max (ffirst series1) (ffirst series2))
;;         max-x (min (first (last series1)) (first (last series2)))]
;;     (c/config
;;       :width pwidth :height   200
;;       :min-x  min-x :max-x  max-x
;;       :min-y      0 :max-y     10)))


;; ;; add data random data points every 1000ms
;; (with-init!
;;   (with-interval 1000
;;     (when-not @paused?
;;       (swap! data1 add!)
;;       (swap! data2 add!))))


;; (defelem chart-object [attribs elems]

;;       (c/container
;;         :chart    chart1
;;         :css      (cell= {:border      "1px solid black"
;;                           :margin-left (str "-" (- (/ (:width chart1) 2) 200) "px")})
;;         :click #(swap! paused? not)

;;         ;; draw shading first so it doesn't cover lines or points
;;         (c/polygon :chart chart1 :data series1 :css {:fill c1 :stroke "none" :fill-opacity 0.5})
;;         (c/polygon :chart chart1 :data series2 :css {:fill c2 :stroke "none" :fill-opacity 0.5})

;;         ;; draw lines
;;         (c/polyline :chart chart1 :data series1 :css {:fill "none" :stroke c1 :stroke-width 2})
;;         (c/polyline :chart chart1 :data series2 :css {:fill "none" :stroke c2 :stroke-width 2})

;;         ;; draw points last so they're not covered by lines or shading
;;         (c/points-circle :chart chart1 :data series1 :r 3 :css {:stroke c1 :fill c1})
;;         (c/points-rect :chart chart1 :data series2 :width 6 :height 6 :css {:stroke c2 :fill c2}))
;;   )


;;; styles ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;-- breakpoints ---------------------------------------------------------------;

(def sm 760)
(def md 1240)
(def lg 1480)

(defn >sm [& bks] (apply b (r 1 1) sm bks))

;-- sizes ---------------------------------------------------------------------;

(def g 20)

(def info-panel-item-height 40)  ;; in pixels
(def info-panel-heading-height 40) ;; in pixels
(def info-panel-width 33)        ;; in CHARACTERS
(def info-page-gutter 42)
(def info-page-padding 42)

;-- colors --------------------------------------------------------------------;

(def white (c 0xffffff))
(def grey  (c 0x4d4d4d))
(def black (c 0x111111))
(def red   (c 0xc32026))

;-- fonts ---------------------------------------------------------------------;

(def body-font   {:f 20 :ff ["Helvetica Neue" "Lucida Grande" :sans-serif] :fc white})
(def button-font {:f 14 :ff ["Helvetica Neue" "Lucida Grande" :sans-serif] :fc white})
(def title-font button-font)


(def info-icon-ph 10)
(def info-icon-pv 2)
(def ig 3)  ; info button gutter

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
  (elem title-font :sh (r 1 1)
    "lighting things"))

(defn fans-view []
  (elem title-font :sh (r 1 1)
    "fan things"))

(defn health-view []
  (elem title-font :sh (r 1 1) :p 20 :g 20
    (for-tpl [{:keys [name temp load cpu]} (cell= [{:name "processor one"} {:name "processor two"}])]
      (elem :sh (r 1 2) :p 20 :g 20
        (elem :sh (r 1 1) :p 20 :g 20 :c black
          name)
        (elem :sh (r 1 2) :a :mid
          "temp")
        (elem :sh (r 1 2) :a :mid
          "load")
        (elem :sh (r 1 1) :a :mid
          "graph")))))

#_(def info-proc
  "info page processor descriptions map"
  `(("PROCESSOR"             ~#(cell= (:name (:cpu jm))))
;     `(("PROCESSOR"             ~#(cell= (:presetOneEffect (first @data))))
    ("CODE NAME"              #(identity nil))
    ("SOCKET TYPE"            #(identity nil))
    ("STOCK FREQUENCY"        #(identity nil))))

#_(def info-vid
  "info page video card descriptions map"
  `(("VIDEO CARD"          ~#(cell= (:name (nth (:gpu_list jm) %))))
    ("MAX TDP"              #(identity nil))
    ("DEFAULT CLOCK"        #(identity nil))
    ("TURBO CLOCK"          #(identity nil))
    ("UNIFIED SHADERS"      #(identity nil))))

#_(def info-mb
  "info page motherboard descriptions map"
  `(("MOTHERBOARD"     ~#(cell= (:name (:mb jm))))
    ("MODEL"               #(identity nil))
    ("CHIPSET"             #(identity nil))
    ("SOUTHBRIDGE"         #(identity nil))
    ("BIOS VERSION"        #(identity nil))
    ("BIOS DATE"           #(identity nil))))

#_(def info-mem
  "info page memory description map"
  `(("MEMORY"             ~#(identity nil))
    ("MANUFACTURER"        #(identity nil))
    ("CAPACITY"            #(identity nil))
    ("DEFAULT FREQUENCY"   #(identity nil))
    ("TYPE"                #(identity nil))
    ("TIMINGS"             #(identity nil))))

#_(def info-drv-generic
  "info-page drive description map"
  ;; as the drive name is nested an extra level, there's no easy way to extract it with a function fragment
  ;; here so it will be done custom in the header rendering below
   `((":\\]DRIVE"        ~#(identity nil))
     ("TYPE"               #(identity nil))
     ("FREE"               #(identity nil))
     ("USED"               #(identity nil))
     ("POWER ON HOURS"     #(identity nil))))

(defn info-view []
  (elem title-font :sh (r 1 1) :p 42 :g 42
    (elem :sh (r 1 2) :sv 100 :c black :av :mid
      #_(image :url laptop-icon)
        "PC NAME")
    (elem :sh (r 1 2) :sv 100 :c black :av :mid
      #_(image :url display-icon)
      "OPERATING SYSTEM")
    (elem :sh (r 1 2) :sv 400 :c black :av :beg
      #_(image :url cpu-icon)
      "PROCESSOR")
    (elem :sh (r 1 2) :sv 400 :c black)
    (elem :sh (r 1 2) :sv 400 :c black)
    (elem :sh (r 1 2) :sv 400 :c black)))

(window
  :title        "Xotic"
  :route        (cell= [[view]])
  :initiated    initiate!
  :routechanged change-route!
  :c grey :scroll true
  (elem :sh (r 1 1) :c black :bb 6 :bc red
    (elem :sh (r 1 1) :p g :ah (b :mid sm :beg) :av (b :beg sm :mid)
      (image :sh 200 :url "xotic-pc-logo.svg"))
    (elem :sh (r 1 1) :p g :av (b :beg sm :end)
      (tab-button :sh (>sm (r 11 50)) :val :health   "SYSTEM HEALTH")
      (tab-button :sh (>sm (r 11 50)) :val :lighting "LIGHTING")
      (elem       :sh (>sm (r 6  50)))
      (tab-button :sh (>sm (r 11 50)) :val :fans     "FANS")
      (tab-button :sh (>sm (r 11 50)) :val :info     "INFO")))
  (case-tpl view
    :health   (health-view)
    :lighting (lighting-view)
    :fans     (fans-view)
    :info     (info-view))
  (elem :sh (r 1 1) :ah :mid :c black
    (elem :sh (>sm 920 md 1240 lg 1400) :p g :g g
      (for [[logo link] footer-menu-items :let [n (count footer-menu-items)]]
        (elem :sh (>sm (r 1 n)) :a :mid
          (image :m :pointer :url logo :click #(.open js/window link)))))))
