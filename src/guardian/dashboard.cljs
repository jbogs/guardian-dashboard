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
    [hoplon.ui.attrs :refer [- c r d]]))

;;; environment ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(enable-console-print!)

(e/def URL "ws://localhost:8000")

;;; utils ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn safe-name [keyword] (try (name keyword) (catch js/Error _)))

;;; content ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def footer-menu-items
  [["facebook-icon.svg"  "https://facebook.com"]
   ["instagram-icon.svg" "https://www.instagram.com/xoticpc/"]
   ["twitter-icon.svg"   "https://twitter.com/XoticPC"]
   ["youtube-icon.svg"   "https://www.youtube.com/channel/UCJ9O0vRPsMFk5UtIDimr6hQ"]])

;;; models ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defc state {:view :mb :index 0 :data {}})
(defc error nil)

;;; queries ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defc= data  (-> state :data) #(swap! state assoc :data %))
(defc= view  (-> state :view))
(defc= model (get-in state [:data :components (:index state)]))

#_(cell= (pprint state))
#_(cell= (pprint model))
#_(cell= (pprint error))

;;; service ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn connect [url state error]
  (let [conn (js/WebSocket. url)
        cljs #(js->clj % :keywordize-keys true)
        data #(-> % .-data js/JSON.parse cljs :data x/xform)]
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

(defn change-state! [view & [index]]
  (swap! state assoc :view view :index index))

(defn change-route! [[[view {:keys [index]}] _]]
  (change-state! view index))

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
(def g-lg 16)

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
  [ (panel :sh (>sm (r 1 2) md (r 1 4)) :sv (b (r 1 2) md (r 1 1)) :name "MOTHERBOARD" :icon "mb-icon.svg"
      (panel-table :sh (r 1 1)
        (for-tpl [{:keys [name value]} (cell= (:temps model))]
          (panel-row :sh (r 1 1) :name  name
            (cell= (str value "° C"))))
        (for-tpl [{:keys [name value]} (cell= (:fans model))]
          (panel-row :sh (r 1 1) :name name
            (cell= (str value "RPM"))))))])

(defn cpu-view []
  [ (elem :sh 300 :sv 300 #_(- (r 1 1) 30) :b 2 :bc bgrey
      (for-tpl [{:keys [name temp threads]} (cell= (:cores model))]
        (elem :sh (cell= (r 1 (count (:cores model)))) :sv (r 1 1) :gh 10 :ah :mid :av :end
          (for-tpl [{:keys [name load]} threads]
            (elem :sh 4 :sv (cell= (+ (* load 3) 10)) :r 6 :c (cell= (condp < temp 40 :blue 50 :yellow :red)))))))]);; can't use ratio because of https://github.com/hoplon/ui/issues/25

(defn gpu-view []
  (panel :sh (>sm (r 1 2) md (r 1 4)) :sv (b (r 1 2) md (r 1 1)) :name "GPUS" :icon "video-card-icon.svg"
    (cell-let [{:keys [name temps loads]} model]
      (panel-table :sh (r 1 1) :name name
        (for-tpl [{:keys [name value]} temps]
          (panel-row :sh (r 1 1) :name name
            (cell= (str value "° C"))))
        (for-tpl [{:keys [name value]} loads]
          (panel-row :sh (r 1 1) :name name
            (cell= (str value "%"))))))))

(defn memory-view []
  (elem :sh 400
    (elem :sh (r 1 1)
      (cell= (str (:free model) " / " (:total model))))))

(defn hdd-view []
  [ (panel :sh (>sm (r 1 2) md (r 1 4)) :sv (b (r 1 2) md (r 1 1)) :name "DRIVES" :icon "drive-icon.svg"
      (panel-table :sh (r 1 1)
        (for-tpl [{:keys [name value]} (cell= (:loads model))]
          (panel-row :sh (r 1 1) :name name
            (cell= (str value "%"))))
        (for-tpl [{:keys [name value]} (cell= (:temps model))]
          (panel-row :sh (r 1 1) :name name
            (cell= (str value "%"))))))])

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
    (elem :sh (>sm 80 md 380) :sv (r 1 1) :gv l
      (for-tpl [[idx {:keys [name type]}] (cell= (map-indexed vector (:components data)))]
        (elem :sh (r 1 1) :s 80 :ph g-lg :gh g-lg :ah (b :mid md :beg) :av :mid :c black :fc bgrey :bl 2 :bc (cell= (if (= idx (:index state)) red cgrey)) :m :pointer :click #(change-state! @type @idx)
          (image :s 34 :a :mid :url (cell= (when type (str (safe-name type) "-icon.svg"))))
          (when-tpl (b true sm false md true)
            (elem :sh (b 300 sm (- (r 1 1) 34 g-lg))
              name))))
      (b nil sm (elem :sh (>sm 80 md 380) :sv (- (r 1 1) (* 60 4) (- (* 4 l) l)) :c black)))
    (elem title-font :sh (>sm (- (r 1 1) 80 l) md (- (r 1 1) 380 l)) :sv (r 1 1) :p g-lg :g g-lg :c cgrey
      (case-tpl view
        :mb     (mb-view)
        :cpu    (cpu-view)
        :gpu    (gpu-view)
        :memory (memory-view)
        :hdd    (hdd-view)))))
