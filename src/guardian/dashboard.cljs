(ns ^{:hoplon/page "index.html"} guardian.dashboard
  (:refer-clojure
    :exclude [- name])
  (:require
    [adzerk.env                         :as e]
    [guardian.dashboard.visualizations  :as v]
    [guardian.dashboard.service         :as s]
    [cljs.pprint          :refer [pprint]]
    [javelin.core         :refer [cell= cell cell-let]]
    [hoplon.core          :refer [defelem if-tpl when-tpl for-tpl case-tpl]]
    [hoplon.ui            :refer [window elem image b]]
    [hoplon.ui.attrs      :refer [- r font rgb hsl lgr]]
    [hoplon.ui.utils      :refer [name]]
    [hoplon.ui.transforms :refer [linear]]))

;;; environment ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(enable-console-print!)

(e/def URL "ws://localhost:8000")

;;; utils ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn ->GB [bytes] (when bytes (str (.toFixed (/ bytes 1000000000) 2) "GB")))
(defn ->% [num]    (when num (str (.toFixed num) "%")))

(defn rect    [e] (.getBoundingClientRect (.-currentTarget e)))
(defn mouse-x [e] (- (.-pageX e) (.-left (rect e))))
(defn mouse-y [e] (- (.-pageY e) (.-top  (rect e))))

(defn path= [c path] (cell= (get-in c path) (partial swap! c assoc-in path)))

;;; content ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def footer-menu-items
  [["facebook-icon.svg"  "https://www.facebook.com/xoticpc/"]
   ["instagram-icon.svg" "https://www.instagram.com/xoticpc/"]
   ["twitter-icon.svg"   "https://twitter.com/XoticPC"]
   ["youtube-icon.svg"   "https://www.youtube.com/channel/UCJ9O0vRPsMFk5UtIDimr6hQ"]])

;;; models ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defonce conn (atom nil))

(defonce sess (cell {:state :system}))
(defonce hist (cell #queue[]))

;;; derivations ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def state (path= sess [:state]))
(def error (path= sess [:error]))
(def route (cell= [[state]] #(reset! state (ffirst %))))

(def data  (cell= (-> hist last) #(swap! hist (fn [h] (conj (if (> (count h) 180) (pop h) h) %)))))

;;; commands ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when-not @conn
  (-> (s/connect URL)
      (.then  #(reset! conn (s/bind-sensors! % data error 1000 120)))
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

(defn temp->color [& ds] #(hsl ((linear ds [260 0]) %) (r 4 5) (r 9 20)))

;-- typography ----------------------------------------------------------------;

(def magistralc-bold (font :system ["MagistralC Bold"] :opentype "magistralc-bold.otf"))
(def lato-semibold   (font :system ["Lato Semibold"]   :truetype "lato-semibold.ttf"))
(def lato-medium     (font :system ["Lato Medium"]     :truetype "lato-medium.ttf"))

(def font-1     {:t 21 :tf magistralc-bold :tc white})
(def font-2     {:t 18 :tf magistralc-bold :tc white})
(def font-3     {:t 16 :tf magistralc-bold :tc white})
(def font-4     {:t 14 :tf magistralc-bold :tc white})
(def font-5     {:t 12 :tf magistralc-bold :tc white})
(def font-label {:t 14 :tf lato-semibold   :tc black})
(def font-body  {:t 12 :tf lato-medium     :tc black})

;;; components ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defelem hslider [{:keys [sh sv s src] :as attrs} elems]
  (let [len (cell= (or sv s))
        pos (cell= (* (/ src 360) len) #(reset! src (int (* (/ % @len) 360))))
        col (cell= (hsl src (r 1 1) (r 1 2)))
        lgr (apply lgr 180 (map #(hsl % (r 1 1) (r 1 2)) (range 0 360 10)))]
    (elem :pt (cell= (- pos (* (/ pos len) sh))) :r (/ sh 2) :c lgr :click #(reset! pos (mouse-y %)) :m :pointer (dissoc attrs :dir :src)
      (elem :s sh :r (cell= (/ sh 2)) :c red :b 2 :bc (white :a 0.6) :m :pointer)
      elems)))

(defelem sslider [{:keys [sh sv s src] :as attrs} elems]
  (let [len (cell= (or sv s))
        pos (cell= (* (* src 100) len) #(reset! src (int (* (/ % @len) .01))))
        col (cell= (hsl src (r 1 1) (r 1 2)))
        lgr (apply lgr 0 (map #(hsl 0 (r % 100) (r 1 2)) (range 0 100)))]
    (elem :pt (cell= (- pos (* (/ pos len) sh))) :r (/ sh 2) :c lgr :click #(reset! pos (mouse-y %)) :m :pointer (dissoc attrs :dir :src)
      (elem :s sh :r (cell= (/ sh 2)) :c red :b 2 :bc (white :a 0.6) :m :pointer)
      elems)))

(defelem lslider [{:keys [sh sv s src] :as attrs} elems]
  (let [len (cell= (or sv s))
        pos (cell= (* (* src 100) len) #(reset! src (int (* (/ % @len) .01))))
        col (cell= (hsl src (r 1 1) (r 1 2)))
        lgr (apply lgr 180 (map #(hsl 0 (r 1 1) (r % 100)) (range 0 100)))]
    (elem :pt (cell= (- pos (* (/ pos len) sh))) :r (/ sh 2) :c lgr :click #(reset! pos (mouse-y %)) :m :pointer (dissoc attrs :dir :src)
      (elem :s sh :r (cell= (/ sh 2)) :c red :b 2 :bc (white :a 0.6) :m :pointer)
      elems)))

;;; views ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defelem panel [{:keys [items selected-index name-fn value-fn] :as attrs} elems]
  (let [selected-index (cell= (when (> (count items) 1) (or selected-index 0)))
        selected-item  (cell= (get items (or selected-index 0)))]
    (elem :c grey-6 :tc grey-1 (dissoc attrs :items :index)
      (elem :sh (r 1 1) :sv 64 :bb 2 :bc grey-6
        (for-tpl [[idx {:keys [_ type] :as item}] (cell= (map-indexed vector items))]
          (let [selected (cell= (and (= idx selected-index)))]
            (elem :sh 64 :sv (r 1 1) :a :mid :bt 2 :m :pointer
              :c     (cell= (if selected grey-5 grey-6))
              :bc    (cell= (if selected red    grey-5))
              :click #(swap! sess assoc (keyword (str "selected-" (name @type) "-index")) @idx)
              (image :s 34 :a :mid :src (cell= (when type (str (name type) "-icon.svg")))))))
        (elem font-4 :sh (cell= (- (r 1 1) (-> items count (* 64)))) :sv (r 1 1) :ph g-lg :av :mid
          (elem :sh (- (r 1 1) 100) :sv (r 1 1) :t (b 14 sm 12 md 16 lg 18) :ms :text
            (cell= (name-fn selected-item)))
          (elem :sh 100 :ah :end :ms :text :t (b 16 sm 14 md 18 lg 21) :tc (white :a 0.6)
            (cell= (value-fn selected-item)))))
      (elem :sh (r 1 1) :sv (- (r 1 1) 64)
        elems))))

(defn system-view []
  (let [sv-sm     280
        mem-hist  (cell= (mapv :memory hist))
        cpus-hist (cell= (mapv #(get (:cpus %)           (:selected-cpu-index           sess 0)) hist))
        gcs-hist  (cell= (mapv #(get (:graphics-cards %) (:selected-graphics-card-index sess 0)) hist))
        hds-hist  (cell= (mapv #(get (:hard-drives    %) (:selected-hard-drive-index    sess 0)) hist))
        gc        (cell= (get (:graphics-cards data) (:selected-graphics-card-index sess 0)))
        cpu-color (temp->color 20 80)
        hdd-color (temp->color 20 50)]
    (list
      (panel :sh (r 1 1) :sv (b (* sv-sm 2) sm (r 1 3)) :gh 5 :c grey-6
        :name-fn        :name
        :value-fn       #(str (:value (:load %)) "% " (:value (:temp %)) "°")
        :items          (cell= (:cpus data))
        :selected-index (cell= (:selected-cpu-index sess))
        (v/histogram font-4 :sh (>sm (r 3 4)) :sv (b (r 1 2) sm (r 1 1)) :c grey-5 :tc (white :a 0.6)
          :name "CPU Load & Temperature"
          :icon "cpu-icon.svg"
          :data (cell= (mapv #(hash-map :value (-> % :load :value) :color (-> % :temp :value cpu-color)) cpus-hist)))
        (v/cpu-capacity font-4 :sh (>sm (r 1 4)) :sv (b (r 1 2) sm (r 1 1)) :c grey-5 :bl (b 0 sm 2) :bt (b 2 sm 0) :bc grey-4
          :cfn  cpu-color
          :data (cell= (get (:cpus data) (:selected-cpu-index sess 0)))))
      (panel :sh (r 1 1) :sv (b (* sv-sm 2) sm (r 1 3)) :gh 5 :c grey-6
        :name-fn        :name
        :value-fn       #(when-let [gpu (:gpu %)] % (str (-> gpu  :load :value) "% " (-> gpu :temp :value) "°"))
        :items          (cell= (:graphics-cards data))
        :selected-index (cell= (:selected-graphics-card-index sess))
        (if-tpl (cell= (:integrated? gc))
          (elem font-2 :s (r 1 1) :a :mid :tc (white :a 0.6) :c grey-5
            "No sensor data available for integrated GPU.")
          (list
            (v/histogram font-4 :sh (>sm (r 3 4)) :sv (b (r 1 2) sm (r 1 1)) :c grey-5 :tc (white :a 0.6)
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
        (v/histogram font-4 :s (r 1 1) :c grey-5 :tc (white :a 0.6)
          :name "Memory Utilization"
          :icon "memory-icon.svg"
          :data (cell= (mapv #(hash-map :value (* (/ (-> % :used :value) (-> % :total :value)) 100) :color "grey") mem-hist))))
      (panel :sh (>sm (r 1 2)) :sv (b sv-sm sm (r 1 3)) :c grey-6
        :name-fn        #(apply str (:name %) " " (interpose " & " (mapv :name (:volumes %))))
        :value-fn       #(str (-> % :load :value int) "% " (-> % :temp :value) "°")
        :items          (cell= (:hard-drives data))
        :selected-index (cell= (:selected-hard-drive-index sess))
        (v/histogram font-4 :s (r 1 1) :c grey-5 :tc (white :a 0.6)
          :name "Drive Utilization"
          :icon "capacity-icon.svg"
          :data (cell= (mapv #(hash-map :value (-> % :load :value) :color (-> % :temp :value hdd-color)) hds-hist)))))))

(defn lighting-view []
  (let [lightable (cell nil)
        id        (cell= (:id     lightable))
        effect    (cell= (:effect lightable))
        [h s l]   @(cell= (:color  lightable))]
    (list
      (elem :sh (r 1 1) :sv (r 1 4)
        (elem font-2 :sh (r 1 1) :sv 64 :ph g-lg :av :mid :c black
          "Devices")
        (elem :sh (r 1 1) :sv (- (r 1 1) 64) :ph g-lg :gh g-lg :ah :mid :c grey-5 #_(lgr 180 grey-5 grey-5 black)
          (for-tpl [{[type :as id] :id name* :name effect :effect [h s l :as color] :color [hb sb lb :as beg-color] :beg-color [he se le :as end-color] :end-color :as lightable*} (cell= (:lights data))]
            (let [selected? (cell= (= lightable lightable*))]
              (elem font-4 :sh (cell= (r 1 (count (:lights data)))) :sv (r 1 1) :pv g-lg :gv g-lg :ah :mid :bb 2
                :bc (cell= (if selected? red grey-5)) :m :pointer
                :tc (cell= (if selected? white grey-1))
                :click #(reset! lightable @lightable*)
                (elem :sh (r 1 1) :sv (- (r 1 1) 16 34 (* g-lg 3)) :r 2 :a :mid :tx :capitalize
                  :c (cell= (if (= effect :color) (hsl h (r 1 1) (r 1 2) (if selected? (r 1 1) (r 2 3))) (lgr 180 (hsl hb (r 1 1) (r 1 2)) (hsl he (r 1 1) (r 1 2)))))
                  (cell= (name effect)))
                (elem :sh (r 1 1) :ah :mid
                  name*)
                (image :s 34 :src (cell= (when type (str (name type) "-icon.svg")))))))))
      (elem :sh (r 1 4) :sv (r 3 4)
        (elem font-2 :sh (r 1 1) :sv 64 :ph g-lg :av :mid :c black
          "Effects")
        (elem :sh (r 1 1) :sv (r 1 1) :c grey-5
           (for [[effect* [name* _ icon]] s/effects]
              (let [selected (cell= (= effect effect*))]
                (elem font-5 :sh (r 1 1) :p g-lg :g g-lg :av :mid :m :pointer
                   :c (cell= (when selected grey-4))
                   :click #(s/set-effect! @conn @id effect*)
                   ;:tc #_(cell= (if (= effect effect*) white grey-1)) :click #(s/set-effect! @conn @id %)
                  (image :s 26 :src (cell= (str icon "-icon.svg")))
                  (elem name*))))))
      (elem :sh (r 3 4) :sv (r 3 4) :c grey-5
        (elem font-2 :sh (r 1 1) :sv 64 :ph g-lg :av :mid :c black
          "Colors")
        (elem :s (r 1 1) :p g-lg :gh (* 3 g-lg) :a :mid
          (hslider :sh 24 :sv 400 :r 12 :src (cell= h #(s/set-color! @conn @id [% 1 0.5])))
          (sslider :sh 24 :sv 400 :r 12 :src (cell= s #(s/set-color! @conn @id [0 % 0.5])))
          (lslider :sh 24 :sv 400 :r 12 :src (cell= l #(s/set-color! @conn @id [0 1 % ]))))))))

(defn fan-view []
  (let [fan  (cell nil)
        id   (cell= (:id   fan))
        pwm  (cell= (:pwm  fan))
        tach (cell= (:tach fan))]
    (list
      (elem :sh (r 1 1) :sv (r 1 1)
        (elem font-2 :s (r 1 1) :sv 64 :ph g-lg :av :mid :c black
          "Fans")
        (elem :sh (r 1 1) :sv (- (r 1 1) 64) :ph g-lg :gh g-lg :ah :mid :c grey-5 #_(lgr 180 grey-5 grey-5 black)
          #_(for-tpl [{[type :as id] :id name* :name pwm :pwm tach :tach :as fan*} (cell= (:fans data))]
            (let [selected? (cell= (= fan fan*))]
              (elem font-4 :sh (cell= (r 1 (count (:lights data)))) :sv (r 1 1) :pv g-lg :gv g-lg :ah :mid :bb 2
                :bc (cell= (if selected? red grey-5)) :m :pointer
                :tc (cell= (if selected? white grey-1))
                :click #(reset! lightable @lightable*)
                (elem :sh (r 1 1) :sv (- (r 1 1) 16 34 (* g-lg 3)) :r 2 :a :mid :tx :capitalize
                  :c (cell= (if (= effect :color) (hsl h (r 1 1) (r 1 2) (if selected? (r 1 1) (r 2 3))) (lgr 180 (hsl hb (r 1 1) (r 1 2)) (hsl he (r 1 1) (r 1 2)))))
                  (cell= (name effect)))
                (elem :sh (r 1 1) :ah :mid
                  name*)
                (image :s 34 :src (cell= (when type (str (name type) "-icon.svg")))))))))
      #_(elem :sh (r 1 4) :sv (r 3 4)
        (elem font-2 :sh (r 1 1) :sv 64 :ph g-lg :av :mid :c black
          "Effects")
        (elem :sh (r 1 1) :sv (r 1 1) :c grey-5
           (for [[effect* [name* _ icon]] s/effects]
              (let [selected (cell= (= effect effect*))]
                (elem font-5 :sh (r 1 1) :p g-lg :g g-lg :av :mid :m :pointer
                   :c (cell= (when selected grey-4))
                   :click #(s/set-effect! @conn @id effect*)
                   ;:tc #_(cell= (if (= effect effect*) white grey-1)) :click #(s/set-effect! @conn @id %)
                  (image :s 26 :src (cell= (str icon "-icon.svg")))
                  (elem name*))))))
      #_(elem :sh (r 3 4) :sv (r 3 4) :c grey-5
        (elem font-2 :sh (r 1 1) :sv 64 :ph g-lg :av :mid :c black
          "Colors")
        (elem :s (r 1 1) :p g-lg :g g-lg :a :mid
          (hslider :sh 24 :sv 400 :r 12 :src (cell= [0 1 0.5] #(s/set-color! @conn @id [% 1 0.5])))
          (sslider :sh 24 :sv 400 :r 12 :src (cell= [0 1 0.5] #(s/set-color! @conn @id [% 1 0.5])))
          (lslider :sh 24 :sv 400 :r 12 :src (cell= [0 1 0.5] #(s/set-color! @conn @id [% 1 0.5]))))))))

(window :g 2 :c grey-4 :scroll (b true sm false) :src route :title "Xotic"
  (elem :sh (r 1 1) :ah :mid :c black
    (elem :sh (r 1 1) :ah (b :mid sm :beg) :av (b :beg sm :mid) :p g-lg :gv g-lg
      (image :sh 200 :src "xotic-pc-logo.svg" :m :pointer :click #(.open js/window "https://www.xoticpc.com"))
      (elem :sh (>sm (- (r 1 1) 200)) :ah (b :mid sm :end) :gh (* 2 g-lg)
        (for [[logo link] footer-menu-items]
          (image :m :pointer :src logo :click #(.open js/window link))))))
  (if-tpl (cell= (not data))
    (elem :s (r 1 1) :pb 200 :a :mid :c black
      (elem :s 100 :g 10 :ah :mid
        (image :src "loading-icon.png")
        (elem :sh (r 1 1) :ah :mid font-2 :tc (white :a 0.9)
          "connecting")))
    (let [sh-close 80 sh-open 240]
      (elem :sh (r 1 1) :sv (- (r 1 1) 80) :g l
        (elem :sh (>sm sh-close md sh-open) :sv (b nil sm (r 3 5)) :gv l
          (for-tpl [{label :label v :view} (cell= [{:view :system :label "System Monitor"} {:view :lighting :label "Lighting Settings"} {:view :fan :label "Fan Settings"}])]
            (let [selected (cell= (= state v))]
              (elem font-4 :sh (r 1 1) :s sh-close :ph g-lg :gh g-lg :ah (b :mid md :beg) :av :mid :c (cell= (if selected grey-4 grey-5)) :tc (cell= (if selected white grey-1)) :bl 2 :bc (cell= (if selected red grey-5)) :m :pointer :click #(reset! state @v)
                (image :s 34 :a :mid :src (cell= (when v (str (name v) "-icon.svg"))))
                (when-tpl (b true sm false md true)
                  (elem :sh (b 120 sm (- (r 1 1) 34 g-lg))
                    label)))))
          (b nil sm (elem :sh (>sm sh-close md sh-open) :sv (r 2 1) :c grey-6)))
        (elem :sh (>sm (- (r 1 1) sh-close l) md (- (r 1 1) sh-open l)) :sv (b nil sm (r 1 1)) :g l
          (case-tpl state
            :system   (system-view)
            :fan      (fan-view)
            :lighting (lighting-view)))))))
