(ns+ guardian.dashboard
  (:page
    "index.html")
  (:refer-clojure
    :exclude [-])
  (:require
   [cljs.core       :refer [js->clj clj->js]]
                                        ;   [goog.string :refer [format]]
;   [planck.core :refer [eval]]
   [cuerdas.core :as str]
;    [goog.string :as gstring]
;    [goog.string.format]
    [chart.core      :as c]
    [castra.core     :refer [mkremote]]
    [javelin.core    :refer [defc defc= cell= cell cell-doseq]]
    [hoplon.core     :refer [defelem for-tpl when-tpl case-tpl]]
    [hoplon.ui       :refer [elem image window video s b]]
    [hoplon.ui.attrs :refer [- c r d]]))

;;; environment ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(enable-console-print!)

(def dev (= js/location.hostname "localhost"))
(def url (if dev "http://localhost:8000" "/service"))

;;; utils ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; models ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defc session {:state :info})
(defc loading nil)
(defc error   nil)

;;; queries ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defc= state (-> session :state))

;;; remotes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn remote [endpoint & [opts]]
  (let [opts* {:url url :on-error #(when dev (println (.-serverStack %)))}]
    (mkremote endpoint session error loading (merge opts* opts))))

(def initiate-session  (remote 'guardian.service.command/initiate-session))
(def terminate-session (remote 'guardian.service.command/termminate-session))

;;; commands ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn change-state! [state]
  (swap! session assoc :state state))

(defn change-route! [[[state] _]]
  (change-state! state))

(defn initiate! [[path qmap] status _]
  (prn "calling service")
  #_(initiate-session))

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




;;; jbog data import ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; string constant to dummy up data

;#_

(def sample-record "{\"mb\": {\"name\": \"Gigabyte Technology Co., Ltd. Q77M-D2H\",\"fan_list\": [1151.88],\"temp_list\": [27.8, 29.8, 42.0, 37.0, 37.0] }, \"cpu\": {   \"name\": \"Intel Core i7 3770\",   \"load\": 6.86,   \"temp\": 45.0 }, \"hdd_list\": [{   \"name\": \"INTEL SSDSC2BB240G4                     \",   \"temp\": 31.0 }, {   \"name\": \"WDC WD2000FYYZ-01UL1B0                  \",   \"temp\": 36.0 }], \"gpu_list\": [{   \"name\": \"Intel(R) HD Graphics 4000\",   \"fan\": 0.0,   \"load\": 0.0,   \"temp\": 0.0 }]}")


(defn jbog-map []
  "return map of jbog data"
  (-> (.parse js/JSON sample-record) (js->clj :keywordize-keys true)))

(defc jm (jbog-map))  ; latest record coming in from jbog server

;;  (println (:name (:mb (jbog-map))))

;; (.log js/console (.parse js/JSON sample-record))

;;; constants ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; info panel specifics

(def info-panel-item-height 40)  ;; in pixels
(def info-panel-heading-height 40) ;; in pixels
(def info-panel-width 35)        ;; in CHARACTERS
(def info-panel-heading-color :black)
(def info-panel-color :white)
(def info-page-gutter 42)
(def info-page-padding 42)

;;; xotic images

(def background-texture "background texture.svg")
(def black-bar "black.bar.svg")
(def black-button  "Black button.svg")
(def black-button-red-line "Button normal complet.svg")
(def blank-header "Blank Header.svg")
(def button-mouseover "Button mouseover complet.svg")
(def button-selected "Button Selected complet.svg")
(def circular-gage-bubbles  "Circular Gauge (Bubbles).svg")
(def circular-gage-fan-speed  "Circular Gauge (Fan Speed).svg")
(def circular-gage-lighting  "Circular Gauge (Lighting).svg")
(def circular-gage-radical "Circular Gauge (Radial).svg")
(def cpu-graph "CPU graph.svg")
(def gpu-graph "GPU graph.svg")
(def cpu-gpu-graph  "CPU-GPU Graph.svg")
(def cpu-icon "CPU icon.svg")
(def cpu-gpu-load-ring "CPU_GPU_LOAD_ring.svg")
(def cpu-load-bar "cpu-load-bar.svg")
(def cpu-memory-bar  "cpu-memory-bar.svg")
(def display-icon "Display icon.svg")
(def exotic-pc "Exotic PC.svg")
(def exotic-pc-big "Exotic PC big.svg")
(def facebook-icon  "Facebook icon.svg")
(def fan-speed-background "Fan Speed background.svg")
(def gpu-icon "GPU icon.svg")
(def harddrive-icon  "Harddrive icon.svg")
(def hdd-icon "HDD icon.svg")
(def page-header "Header.svg")   ; renamed to avoid collision with hoplon core
(def info-button "Info button.svg")
(def instagram-icon  "Instagram icon.svg")
(def keyboard-overlay  "Keyboard Overlay.svg")
(def laptop-icon "laptop icon.svg")
(def logo "logo.svg")
(def logo-with-black "Logo_with_black.svg")
(def long-title-background "long-title-background.svg")
(def mb-icon "MB icon.svg")
(def memory-icon  "Memory icon.svg")
(def menu-top "Menu_top.svg")
(def motherboard-fan-graph  "MB fan Graph.svg")
(def motherboard-icon  "Motherboard icon.svg")
(def operating-system-icon  "Operating System icon.svg")
(def pc-name-icon  "PC Name icon.svg")
(def picagram-icon "Picagram icon.svg")
(def preset-toggle-off  "Preset Toggle Off.svg")
(def preset-toggle-on  "Preset Toggle On.svg")
(def processor-icon  "Processor icon.svg")
(def ram-icon "RAM icon.svg")
(def red-box "Red box.svg")
(def temp-background "TEMP background.svg")
(def title-background "Title background.svg")
(def twitter-icon  "Twitter icon.svg")
(def video-card-icon  "Video Card icon.svg")
(def white-box "White box.svg")
(def xotic-pc-emblem-white  "XoticPC Emblem White.svg")
(def xoticpc-emblem-red  "XoticPC Emblem Red.svg")
(def xoticpc-logo  "XoticPC Logo.svg")
(def youtube-icon  "Youtube icon.svg")
(def zone-keyboard "Zone keyboard.svg")

;;; styles ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;-- breakpoints ---------------------------------------------------------------;

(def sm 760)
(def md 1240)
(def lg 1480)

(defn >sm [& bks] (apply b (r 1 1) sm bks))

;-- sizes ---------------------------------------------------------------------;

(def g 20)

;-- colors --------------------------------------------------------------------;

(def white (c 0xEEEEEE))
(def grey  (c 0x303030))
(def black (c 0x111111))

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
    (image button-font :s (r 1 1) :a :mid :m :pointer :click #(change-state! val)
      :fc  (cell= (if (= state val) grey white))
      :url (cell= (if (= state val) button-selected black-button-red-line))
      elems)))

;;; first page

(defelem processor-status [{:keys [val] :as attrs} elems]
  "display a CPU/GPU status collection"
   (println elems)
  (elem  :ph 10 :ah :mid (dissoc attrs :val)
          (image title-font  :url title-background
                 (image :g ig :ph info-icon-ph :pv info-icon-pv :a :mid  :url info-button)
                 (str "   " (first elems) "  - INFO"))
          (elem :ah :beg :pv 20 :g 10
                (image :url temp-background)
                (image :s (r 1 2.2) :url circular-gage-radical))
          (elem :ah :beg :pv 20 :g g
                (image :url cpu-graph))))

(defelem drive-info [{:keys [val] :as attrs} elems]
  "display the given disk drive's status collection"
  (println val)
  (elem :ah :mid  (dissoc attrs :val)
    (image title-font  :url title-background
      (image :g ig :ph info-icon-ph :pv info-icon-pv :a :mid :url info-button)
      (str "HDD   " val  "  - INFO"))
    (elem :ah :beg :pv 20 :g 10
      (image :url circular-gage-radical)
      (image :s (r 1 2.2) :url temp-background))
    (elem  :ah :beg :pv 20 :g g
      (image  :url red-box "USED: 150GB")
      (image  :url white-box "FREE: 850GB"))))
;         (elem :ah :beg :pv 20 :g g
;               (image :url cpu-graph))))

;;; whole mid section on page 1

(defn health-view []
  "display a CPU/GPU status collection"
  (elem title-font :s (r 1 1)  :p g :g g
    (processor-status :sh (r 1 2) :ah :beg "CPU")
    (processor-status :sh (r 1 2) :ah :end "GPU")
    (elem
      :s (r 1 1) :p g :g g
      :g g ; mid section
      (elem ; the single overbar midscreen, its info icon, and text
        (image  :s (r 1 .85)  :ah :beg
          :url long-title-background
          (elem :av :mid
            (image :g ig :ph info-icon-ph :pv info-icon-pv :a :mid :url info-button)
            "CPU - INFO"))
        (elem :g g ; containing memory and load bars
          (image :s (r 1 1.35) :url cpu-memory-bar)  ; memory
          (image :p 0 :g 0 :ah :beg :s (r 1 4.4) :url cpu-load-bar))
        (drive-info :sh (r 1 2) :ah :beg :val "1")
        (drive-info :sh (r 1 2) :ah :beg :val "2")))))
                                        ;        (image :sh (>sm 100) :url temp-background)
;        (image :sh (>sm 100) :url circular-gage-radical)))

;;; page 2

(defelem kb-lighting [{keys [val] :as attrs} elems]
  "display a keyboard lighting unit"
  (elem title-font :s (r 1 1) :p g :g g ; :ph 10 :ah :mid
        (dissoc attrs :val)
        (image title-font :url title-background
               (image :g ig :ph info-icon-ph :pv info-icon-pv :a :mid :url info-button)
               (str "KEYBOARD LIGHTING - INFO"))
        (image :url circular-gage-lighting)
        (image :url keyboard-overlay)))

(defn lighting-view []
  (elem title-font :sh (r 1 1)
    (kb-lighting)))

(defn fans-view []
  (elem title-font :sh (r 1 1)
    "fan things"))


;;; the following maps are for the desc and the function to extract the value
;;; used by the "info-panel" function below

(def info-proc
  "info page processor descriptions map"
  '(("PROCESSOR"            #(cell= (:name (:cpu jm))))
    ("CODE NAME"             #(identity nil))
    ("SOCKET TYPE"           #(identity nil))
    ("STOCK FREQUENCY"       #(identity nil))))

(def info-vid
  "info page video card descriptions map"
  '(("VIDEO CARD"          #(cell= (:name (:gpu_list jm))))
    ("MAX TDP"             #(identity nil))
    ("DEFAULT CLOCK"       #(identity nil))
    ("TURBO CLOCK"         #(identity nil))
    ("UNIFIED SHADERS"     #(identity nil))))

(def info-mb
  "info page motherboard descriptions map"
  '(("MOTHERBOARD"        #(cell= (:name (:mb jm))))
    ("MODEL"              #(identity nil))
    ("CHIPSET"            #(identity nil))
    ("SOUTHBRIDGE"        #(identity nil))
    ("BIOS VERSION"       #(identity nil))
    ("BIOS DATE"          #(identity nil))))

(def info-mem
  "info page memory description map"
  '(("MEMORY"             #(identity nil))
    ("MANUFACTURER"       #(identity nil))
    ("CAPACITY"           #(identity nil))
    ("DEFAULT FREQUENCY"  #(identity nil))
    ("TYPE"               #(identity nil))
    ("TIMINGS"            #(identity nil))))

(def info-drv-generic
  "info-page drive description map"
  ;; as the drive name is nested an extra level, there's no easy way to extract it with a function fragment
  ;; here so it will be done custom in the header rendering below
  '((":\\]DRIVE"         #(identity nil))
   ("TYPE"              #(identity nil))
   ("FREE"              #(identity nil))
   ("USED"              #(identity nil))
   ("POWER ON HOURS"    #(identity nil))))

(defn items-field [item1 item2  width]
  "place each item on the end of a width length field"
  (do (println (str item1 ", "  item2 ", " width))
  (str/format (str "%-" width "s%s") item1 item2)))

(defelem info-panel-item [name func data]
  "single element of info panel (not the heading)"
  (elem :sh (r 1 2) :sv info-panel-item-height :c info-panel-color :av :mid
        (items-field  name func info-panel-width)))

  ;; note: the desc field has to be a list or vector because the items have to go in the
  ;; panel in a certain order

(defelem info-panel [:keys [icon desc data drive]] ; [icon desc data & drive]
  "render an info view with icon, description map, data map and optional drive (0 or 1)"
  (elem :sh (r 1 2) :sv info-panel-heading-height
        :c info-panel-heading-color :av :mid
        (image :url icon)
        (items-field (str " " (["C" "D"] drive)  ":\\DRIVE") (:name (first (:hdd_list data )))
                     info-panel-width)))

(defelem info-header [{:keys [icon desc val]}]
  "render an info-header element with the icon to display,
the description, and the value/data"
  (elem :sh (r 1 2) :sv info-panel-heading-height
        (image :url icon)
        (items-field desc val info-panel-width)))


(def a `("motherboard" ~#(cell= (:name (:mb jm)))))

(defn info-view []
  (println ((#(first (rest a))))))

#_
(defn info-view []
           (println (js/eval (str #(first (rest (first info-mb)))))))

#_
(defn info-view []
  (elem title-font :sh (r 1 1)
        :p info-page-padding
        :g info-page-gutter
        (info-header :icon mb-icon :desc (ffirst info-mb)
                     :val (a))))


;                     :val (-> info-mb first rest))))
;  (elem title-font :sh (r 1 1) :p 42 :g 42
;        (elem :sh (r 1 2) :sv info-panel-heading-height
;              :c info-panel-heading-color ))
;  )



#_
(defn info-view []
  (elem title-font :sh (r 1 1) :p 42 :g 42
        (elem :sh (r 1 2) :sv 100 :c :black :av :mid
              (image :url laptop-icon)
              "PC NAME")
        (elem :sh (r 1 2) :sv 100 :c :black :av :mid
              (image :url display-icon)
         "OPERATING SYSTEM")
        (elem :sh (r 1 2) :sv 400 :c :black :av :beg
              (image :url cpu-icon)
              "PROCESSOR")
    (elem :sh (r 1 2) :sv 400 :c :black)
    (elem :sh (r 1 2) :sv 400 :c :black)
    (elem :sh (r 1 2) :sv 400 :c :black)))

(window
  :title        "Xotic"
  :route        (cell= [[state]])
  :initiate     initiate!
  :change-route change-route!
  :ah :mid :c grey :scroll true
  (image :sh (>sm 920 md 1240 lg 1400) :url background-texture
    (image :sh (r 1 1) :sv (b 800 sm :auto) :url page-header :av (b :beg sm :end) ;; note: padding has not yet been applied to media elements
      (tab-button :sh (>sm (r 11 50)) :val :health   "SYSTEM HEALTH")
      (tab-button :sh (>sm (r 11 50)) :val :lighting "LIGHTING")
      (elem       :sh (>sm (r 6  50)))
      (tab-button :sh (>sm (r 11 50)) :val :fans     "FANS")
      (tab-button :sh (>sm (r 11 50)) :val :info     "INFO"))
    (case-tpl state
      :health   (health-view)
      :lighting (lighting-view)
      :fans     (fans-view)
      :info     (info-view))))
