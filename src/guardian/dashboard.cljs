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

(defn ->GB [bytes] (when bytes (str (.toFixed (/ bytes 1000000000) 2) "GB")))
(defn ->% [num]    (when num (str (.toFixed num) "%")))
(defn safe-name [keyword] (try (name keyword) (catch js/Error _)))

;;; content ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def footer-menu-items
  [["facebook-icon.svg"  "https://facebook.com"]
   ["instagram-icon.svg" "https://www.instagram.com/xoticpc/"]
   ["twitter-icon.svg"   "https://twitter.com/XoticPC"]
   ["youtube-icon.svg"   "https://www.youtube.com/channel/UCJ9O0vRPsMFk5UtIDimr6hQ"]])

;;; models ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def resp {:mb     {:name   "Micro-Star International Co., Ltd. MS-16H8"
          :fans   []
          :temps  [{:name "THRM" :value 45}
                   {:name "TZ00" :value 27.8}
                   {:name "TZ01" :value 29.8}]
          :volts  []}
 :cpus   [{:name  "Intel Core i7 6700HQ"
           :loads [{:name "UC"     :value 2.34}
                   {:name "CPU #0" :value 15.62}
                   {:name "CPU #1" :value 0}
                   {:name "CPU #2" :value 1.56}
                   {:name "CPU #3" :value 0}
                   {:name "CPU #4" :value 0}
                   {:name "CPU #5" :value 0}
                   {:name "CPU #6" :value 0}
                   {:name "CPU #7" :value 1.54}]
           :temps [{:name "Core #0" :value 42}
                   {:name "Core #1" :value 41}
                   {:name "Core #2" :value 39}
                   {:name "Core #3" :value 40}
                   {:name "Package" :value 42}]
           :volts [{:name "VID"                 :value 1.08}
                   {:name "IA Offset"           :value 0}
                   {:name "GT Offset",          :value 0}
                   {:name "LLC/Ring Offset"     :value 0}
                   {:name "System Agent Offset" :value 0}]}]
 :hdds   [{:name  "HGST HTS721010A9E630"
           :loads [{:name "Space (e:)" :value 0.02}]
           :temps [{:name "Assembly"   :value 33}]}
          {:name  "SAMSUNG MZVPV128HDGM-00000"
           :loads [{:name "Space (c:)" :value 18.23}]
           :temps [{:name "Assembly"   :value 40}]}]
 :gpus   [{:name  "Intel(R) HD Graphics 530"
           :fans  []
           :loads []
           :temps []}
          {:name  "NVIDIA GeForce GTX 965M"
           :fans  []
           :loads [{:name "Memory"        :value 1.42}
                   {:name "GPU"           :value 0}
                   {:name "Frame Buffer"  :value 0}
                   {:name "Video Engine"  :value 0}
                   {:name "Bus Interface" :value 0}]
           :temps [{:name "TMPIN0" :value 49}]}]
 :memory {:free  6559485952
          :total 8496087040}})

(defc state {:view :mb :index 0 :data (x/xform resp)})
(defc error nil)

;;; queries ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defc= data  (-> state :data) #(swap! state assoc :data %))
(defc= view  (-> state :view))
(defc= model (get-in state [:data :components (:index state)]))

(cell= (pprint state))
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
  (.setInterval js/window call (or interval 1000) tag conn data))

(def sub-hardware-data (partial poll "get_monitor_data"))
(def get-smart-data    (partial call "get_smart_data"))
(def set-client-data   (partial call "set_client_data"))

;;; commands ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn change-state! [view & [index]]
  (swap! state assoc :view view :index index))

(defn change-route! [[[view {:keys [index]}] _]]
  (change-state! view index))

(defn initiate! [[path qmap] status _]
  #_(-> (connect URL data error)
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

(def white   (c 0xFAFAFA))
(def red     (c 0xCC181E))
(def yellow  (c 0xFFD200))
(def grey-1  (c 0x777777))
(def grey-2  (c 0x555555))
(def grey-3  (c 0x414141))
(def grey-4  (c 0x333333))
(def grey-5  (c 0x202020))
(def grey-6  (c 0x161616))
(def black   (c 0x181818))

;-- typography ----------------------------------------------------------------;

(def font-1     {:f 21 :ff ["MagistralC Bold" :sans-serif] :fc white})
(def font-2     {:f 18 :ff ["MagistralC Bold" :sans-serif] :fc white})
(def font-3     {:f 16 :ff ["MagistralC Bold" :sans-serif] :fc white})
(def font-4     {:f 14 :ff ["MagistralC Bold" :sans-serif] :fc white})
(def font-label {:f 14 :ff ["Lato Semibold"   :sans-serif] :fc black})
(def font-body  {:f 12 :ff ["Lato Medium"     :sans-serif] :fc black})

;-- controls - ----------------------------------------------------------------;

(defelem primary-button [attrs elems]
  (elem font-label :pv 6 :ph 12 :c red :r 6
    attrs elems))

(defelem secondary-button [attrs elems]
  (elem font-label :pv 4 :ph 10 :c grey-5 :r 6
    attrs elems))

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
    (title :name (cell= (:name model))
      "Motherboard")
    (elem :g g-lg ;; remove after merging opts with vflatten
      (for-tpl [{:keys [name value]} (cell= (:temps model))]
        (card :sh 100 :name name :icon "mb-icon.svg"
          (cell= (str value "° C")))))
    (elem :g g-lg ;; remove after merging apts with vflatten
      (for-tpl [{:keys [name value]} (cell= (:fans model))]
        (card :sh 100 :name name :icon "mb-icon.svg"
          (cell= (str value "RPM")))))))

(defn cpu-view []
  (list
    (title :name (cell= (:name model))
      "CPU")
    (elem :sh (- (r 1 1) 300 g-lg) :sv 300 :c grey-4 :b 10 :bc grey-5)
    (elem :s 300 :c grey-4 :b 10 :bc grey-5
       (for-tpl [{:keys [name temp threads]} (cell= (:cores model))]
         (elem :sh (cell= (r 1 (count (:cores model)))) :sv (r 1 1) :gh 8 :ah :mid :av :end
           (for-tpl [{:keys [name load]} threads]
             (elem :sh 4 :sv (cell= (+ (* load 3) 10)) :r 6 :c (cell= (condp < temp 40 :blue 50 :yellow :red))))))) ;; can't use ratio because of https://github.com/hoplon/ui/issues/25
    (elem :g g-lg :av :end ;; remove after merging opts with vflatten
      (for-tpl [{:keys [name value]} (cell= (:volts model))]
        (card :sh 100 :name name :icon "mb-icon.svg"
          (cell= (str value "V")))))))

(defn gpu-view []
  (list
    (title :name (cell= (:name model))
      "GPU")
    (elem :g g-lg :av :end ;; remove after merging opts with vflatten
      (for-tpl [{:keys [name value]} (cell= (:loads model))]
        (card :sh 100 :name name :icon "mb-icon.svg"
          (cell= (str value "%")))))))

(defn memory-view []
  (list
    (title :name (cell= (:name model))
      "Memory")
    (v/dist-chart font-4 :sh (r 1 1) :sv 100 :c grey-5 :fc (white :a 0.5)
      :domain (cell= [{:label (->GB (:used model)) :value (:used model)} {:label (->GB (:free model)) :value (:free model)}])
      :range  [{:color :green} {:color grey-4}])
    (elem :sh (r 1 1) :sv 300 :c grey-4 :b 10 :bc grey-5)))

(defn hdd-view []
  (list
    (title :name (cell= (:name model))
      "Hard Drive")
    (v/dist-chart font-4 :sh (r 1 1) :sv 100 :c grey-5 :fc (white :a 0.5)
      :domain (cell= [{:label (->% (:used model)) :value (:used model)} {:label (->% (:free model)) :value (:free model)}])
      :range  [{:color :green} {:color grey-4}])
    (elem :sh (r 1 1) :sv 300 :c grey-4 :b 10 :bc grey-5)
    (card :sh 100 :name (cell= (-> model :temp :name)) :icon "mb-icon.svg"
      (cell= (-> model :temp :value (str "° C"))))))

(window
  :title        "Xotic"
  :route        (cell= [[view]])
  :initiated    initiate!
  :routechanged change-route!
  :c grey-4 :g l
  (elem :sh (r 1 1) :ah :mid :c black
    (elem :sh (r 1 1) :ah (b :mid sm :beg) :av (b :beg sm :mid) :p g-lg :gv g-lg
      (image :sh 200 :url "xotic-pc-logo.svg" :m :pointer :click #(.open js/window "https://www.xoticpc.com"))
      (elem :sh (>sm (- (r 1 1) 200)) :ah (b :mid sm :end) :gh (* 2 g-lg)
        (for [[logo link] footer-menu-items]
          (image :m :pointer :url logo :click #(.open js/window link))))))
  (elem :sh (r 1 1) :sv (- (r 1 1) 80) :g l
    (elem :sh (>sm 80 md 380) :sv (r 1 1) :gv l
      (for-tpl [[idx {:keys [name type]}] (cell= (map-indexed vector (:components data)))]
        (let [selected (cell= (= idx (:index state)))]
        (elem font-4 :sh (r 1 1) :s 80 :ph g-lg :gh g-lg :ah (b :mid md :beg) :av :mid :c (cell= (if selected grey-4 grey-5)) :fc (cell= (if selected white grey-1)) :bl 2 :bc (cell= (if selected red grey-5)) :m :pointer :click #(change-state! @type @idx)
          (image :s 34 :a :mid :url (cell= (when type (str (safe-name type) "-icon.svg"))))
          (when-tpl (b true sm false md true)
            (elem :sh (b 300 sm (- (r 1 1) 34 g-lg))
              name)))))
      (b nil sm (elem :sh (>sm 80 md 380) :sv (- (r 1 1) (* 60 4) (- (* 4 l) l)) :c grey-6)))
    (elem :sh (>sm (- (r 1 1) 80 l) md (- (r 1 1) 380 l)) :sv (r 1 1) :p g-lg :g g-lg :c grey-6
      (case-tpl view
        :mb     (mb-view)
        :cpu    (cpu-view)
        :gpu    (gpu-view)
        :memory (memory-view)
        :hdd    (hdd-view)))))
