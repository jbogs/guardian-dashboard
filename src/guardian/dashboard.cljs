(ns ^{:hoplon/page "index.html"} guardian.dashboard
  (:refer-clojure
    :exclude [- name])
  (:require
    [adzerk.env                         :as e]
    [guardian.dashboard.visualizations  :as v]
    [guardian.dashboard.service         :as s]
    [cljs.pprint             :refer [pprint]]
    [javelin.core            :refer [alts! lens? cell= cell cell-let dosync]]
    [hoplon.core             :refer [defelem if-tpl when-tpl for-tpl case-tpl]]
    [hoplon.ui               :refer [window elem image b t]]
    [hoplon.ui.attrs         :refer [- r font rgb hsl lgr sdw]]
    [hoplon.ui.utils         :refer [name clamp loc x y debounce]]
    [hoplon.ui.interpolators :refer [linear quadratic-out]]))

;;; environment ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(enable-console-print!)

(e/def URL "ws://localhost:8000")

;;; utils ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn ->GB [bytes] (when bytes (str (.toFixed (/ bytes 1000000000) 2) "GB")))
(defn ->%  [num]   (when num (str (.toFixed num) "%")))

(defn path= [c path] (cell= (get-in c path) (partial swap! c assoc-in path)))

(defn deb= [c f & [ms]]
  "debouncing transaction lens"
  (let [val (cell @c)
        set #(when (not= % @c) (f %))
        deb (debounce (or ms 1000) set)
        deb #(do (deb %) (reset! val %))
        src (alts! val c)]
    (cell= (first src) #(if (= % ::tx) (set val) (deb %)))))

(defn commit! [cell]
  (reset! cell ::tx))

(defn cache [c]
  (let [v (cell nil)
        a (alts! c v)]
    (cell= (first a) #(if (lens? c) (reset! c %) (reset! v %)))))

;;; content ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def main-menu-items
  [{:view :system :label "System Monitor"}
   {:view :lights :label "Light Settings"}
   {:view :fans   :label "Fan Settings"}])

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
(def g-md 8)
(def g-lg 16)
(def g-xl 32)

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

(def font-2 {:t 18 :tf magistralc-bold :tc white})
(def font-4 {:t 14 :tf magistralc-bold :tc white})
(def font-5 {:t 12 :tf magistralc-bold :tc white})

;;; components ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defelem slider [{:keys [sh sv s src] r* :r :as attrs}]
  ;- change sizes to body after api refactoring where border becomes stroke
  ;- substract border from sizes / add border to body bh bv b
  ;- support different types of transitions
  ;- consider passing the knob as a child, how to proxy values to knob
  ;- switch from grab to grabbing mouse cursors during drag
  ;- consider tool tip with values
  ;- consider passing curried interpolator function
  ;- handle overflowing margins without overflow: none due to perf problem. new
  ;  box model may fix.
  (let [src    (deb= src #(reset! src %) 500)
        w      (cell= (or sh s))
        h      (cell= (or sv s))
        kd     (cell= (min 32 w h))
        kr     (cell= (/ kd 2))
        dx->rx (cell= (linear [0       100] [0  (- w kd)]))
        dy->ry (cell= (linear [0       100] [(- h kd)  0]))
        rx->dx (cell= (linear [kr (- w kr)] [0       100]))
        ry->dy (cell= (linear [(- h kr) kr] [0       100]))
        pos    (cell= [(dx->rx (clamp (x src) 0 100)) (dy->ry (clamp (y src) 0 100))] #(reset! src [(clamp (@rx->dx (x %)) 0 100) (clamp (@ry->dy (y %)) 0 100)]))
        sdw    (sdw 2 2 (rgb 0 0 0 (r 1 14)) 2 0)]
    (elem :d (sdw :inset true) :r r* :m :pointer
      :pl   (t (cell= (x pos)) 500 quadratic-out)
      :pt   (t (cell= (y pos)) 500 quadratic-out)
      :overflowv :hidden
      :up   #(reset! pos (loc %))
      :move #(when (= (.-which %) 1) (reset! pos (loc %)))
      (dissoc attrs :src)
      (elem :s kd :r (cell= (or r* 16)) :c red :b 2 :bc (white :a 0.6) :d sdw :m :grab))))

(defelem hslider [{:keys [src] :as attrs}]
  (slider :src (cell= (vector src 0) #(reset! src (x %))) (dissoc attrs :src)))

(defelem vslider [{:keys [src] :as attrs}]
  (slider :src (cell= (vector 0 src) #(reset! src (y %))) (dissoc attrs :src)))

(defelem hswitch [{:keys [sh sv s src] r* :r :as attrs}]
  (let [src (deb= src #(reset! src %) 500)
        w   (cell= (or sh s))
        sw  (cell= (/ w 2))
        sdw (sdw 2 2 (rgb 0 0 0 (r 1 14)) 2 0)]
    (elem :d (sdw :inset true) :r 4 :m :pointer
      :pl   (t (cell= (if-not src 0 sw)) 500 quadratic-out)
      :down #(swap! src not)
      (dissoc attrs :src)
      (elem :sh sw :sv (r 1 1) :c red :r 4 :b 2 :bc (white :a 0.6) :d sdw))))

(defn h-grd [s l]
  (let [s (* (or s 1)   100)
        l (* (or l 0.5) 100)]
    (apply lgr 0 (map #(hsl % (r s 100) (r l 100)) (range 0 360 10)))))

(defn s-grd [h l]
  (let [h (or h 0)
        l (* (or l 0.5) 100)]
   (apply lgr 0 (map #(hsl h (r % 100) (r l 100)) (range 0 100)))))

(defn l-grd [h s uv?]
  (let [r? (and h s) ; reg not uv
        h (or h (if r? 0 290))
        s (* (or s 1) 100)]
   (apply lgr 0 (map #(hsl h (r s 100) (r % 100)) (range 0 (if uv? 100 50))))))

(defelem vslider-group [{:keys [label color src] :as attrs}]
  (elem :sh 28 :gv g-xl :ah :mid (dissoc attrs :c :label :src)
    label
   (vslider :sh 28 :sv (b :v 275 850 400) :r 14 :c (cell= (or color grey-6)) :src src)))

(defelem hsl-picker [{:keys [src] :as attrs}]
  (cell-let [[h s l] src]
    (elem (dissoc attrs :src)
      (when-tpl h (vslider-group :label "Hue" :color (cell= (h-grd s l)) :src (cell= (* (/ h 360) 100)            #(reset! src [(* (/ % 100) 360) @s        @l]))))
      (when-tpl s (vslider-group :label "Sat" :color (cell= (s-grd h l)) :src (cell= (* s 100)                    #(reset! src [@h                (/ % 100) @l]))))
      (when-tpl l (vslider-group :label "Lum" :color (cell= (l-grd h s)) :src (cell= (* l (if (and h s) 100 200)) #(reset! src [@h                @s        (if (and @h @s) (/ % 100) (/ % 200))])))))))

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

(defn lights-view []
  (let [light-id (cell nil)
        lights   (cell= (:lights data))
        light    (cell= (some #(when (= light-id (:id %)) %) lights))
        effects  (cell= (:effects data))
        effect   (cell= (some #(when (= (:effect light) (:id %)) %) effects))]
    (list
      (elem :sh (r 1 1) :sv (r 1 5)
        (elem font-2 :sh (r 1 1) :sv 64 :ph g-lg :av :mid :c black
          "Lights")
        (elem :sh (r 1 1) :sv (- (r 1 1) 64) :c grey-5 :gh l :gv (b l sm nil) :ah (b :beg sm :mid)
          (for-tpl [{id* :id name* :name type :type effect-id :effect [h s l :as color] :color [hb sb lb :as beg-color] :beg-color [he se le :as end-color] :end-color :as light*} lights]
            (let [selected? (cell= (= light light*))
                  alpha     (cell= (if selected? (r 1 1) (r 1 3)))
                  effect    (cell= (some #(when (= effect-id (:id %)) %) effects))]
              (elem font-4 :sh (b (r 1 3) sm (cell= (r 1 (count lights)))) :sv (r 1 1) :pv (b g-lg sm g-md) :gv (b g-lg sm g-md) :a :mid :m :pointer
                :tc    (cell= (if selected? white grey-1))
                :click #(reset! light-id (if (= @light-id @id*) nil @id*))
                :c     (cell= (case (first (:types effect))
                                 :color      (hsl (or h 290) (r (or s 1) 1) (r (or l 0.5) 1) alpha)
                                 :beg-color  (lgr 180 (hsl (or hb 290) (r (or sb 1) 1) (r (or lb 0.5) 1) alpha) (hsl (or he 290) (r (or se 1) 1) (r (or le 0.5) 1) alpha))
                                             grey-6))
                (elem :sh (r 1 1) :ah :mid
                  name*)
                (image :s 34 :src (cell= (when type (str (name type) "-icon.svg"))))
                (elem :sh (r 1 1) :ah :mid
                  (cell= (:name effect))))))))
      (elem :sh (r 1 1) :sv (r 4 5) :g l
        (list
          (elem :sh (>sm (r 1 4)) :sv (r 1 1)
            (elem font-2 :sh (r 1 1) :sv 64 :ph g-lg :av :mid :c black
              "Effects")
            (if-tpl light-id
              (elem :sh (r 1 1) :sv (- (r 1 1) 64) :c grey-5
                (for-tpl [{eid :id ename :name etype :type :as effect*} effects]
                  (let [selected (cell= (= effect effect*))]
                    (elem font-5 :sh (r 1 1) :p g-lg :g g-lg :av :mid :m :pointer
                       :c     (cell= (when selected grey-4))
                       :click #(s/set-light-effect! @conn @light-id @eid)
                      (image :s 26 :src (cell= (str eid "-icon.svg")))
                      (elem ename)))))
              (elem font-2 :sh (r 1 1) :sv (- (r 1 1) 64) :p g-lg :c grey-5 :a :mid :tc (white :a 0.9)
                "no lights selected")))
          (elem :sh (>sm (r 3 4)) :sv (r 1 1) :c grey-5
            (elem font-2 :sh (r 1 1) :sv 64 :ph g-lg :av :mid :c black
              "Colors")
            (if-tpl light-id
              (elem :s (r 1 1) 
                (if-tpl (cell= (not-empty (:types effect)))
                  (elem font-2 :s (r 1 1) :pv g-xl :gh (b (* 3 g-xl) 400 (* 2 g-xl) 500 (* 3 g-xl) sm g-xl 940 (* 2 g-xl) md (* 6 g-lg) lg (* 5 g-xl)) :gv g-xl :a :mid 
                    (for-tpl [type (cell= (:types effect))]
                      (elem
                        (cell= (prn :speed light))
                        (case-tpl type
                          :color     (hsl-picker :gh g-xl           :src (cell= (:color     light) #(s/set-light-color!     @conn @light-id %)))
                          :beg-color (hsl-picker :gh g-xl           :src (cell= (:beg-color light) #(s/set-light-beg-color! @conn @light-id %)))
                          :end-color (hsl-picker :gh g-xl           :src (cell= (:end-color light) #(s/set-light-end-color! @conn @light-id %)))
                          :speed     (vslider-group :label "Speed"  :src (cell= (:speed     light) #(s/set-light-speed!     @conn @light-id %)))
                          :scale     (vslider-group :label "Scale"  :src (cell= (:scale     light) #(s/set-light-scale!     @conn @light-id %)))
                          :drift     (vslider-group :label "Drift"  :src (cell= (:drift     light) #(s/set-light-drift!     @conn @light-id %)))
                          :random    (vslider-group :label "Random" :src (cell= (:random    light) #(s/set-light-random!    @conn @light-id %)))
                          :smooth    (vslider-group :label "Smooth" :src (cell= (:smooth    light) #(s/set-light-smooth!    @conn @light-id %)))))))
                  (elem font-2 :s (r 1 1) :c grey-5 :a :mid :tc (white :a 0.9)
                    "no effect enabled")))
              (elem font-2 :sh (r 1 1) :sv (- (r 1 1) 64) :p g-lg :c grey-5 :a :mid :tc (white :a 0.9)
                "no lights selected"))))))))

(defn fans-view []
  (let [fan-id  (cell nil)
        fans    (cell= (:fans data))
        fan     (cell= (some #(when (= fan-id (:id %)) %) fans))
        pwm     (cell= (:pwm    fan))
        tach    (cell= (:tach   fan))
        temp    (cell= (:temp fan))
        devices (cell= (apply concat ((juxt :cpus :graphics-cards :hard-drives) (update data :graphics-cards pop))))
        device  (cell= (:device fan))
        t->c    (temp->color 20 80)
        r->t    (linear [0 100] [20 80])
        t->r    (linear [20 80] [0 100])]
    (list
      (elem :sh (r 1 1) :sv (r 3 5)
        (elem font-2 :sh (r 1 1) :sv 64 :ph g-lg :av :mid :c black
          "Fans")
        (elem :sh (r 1 1) :sv (- (r 1 1) 64) :ph (b 0 sm (* g-lg 3)) :gh (b l sm (* g-lg 3)) :ah :mid :c grey-5
          (for-tpl [{id* :id name* :name type :type device :device tach :tach temp :temp :as fan*} fans]
            (let [selected? (cell= (= fan-id id*))
                  alpha   (cell= (if selected? (r 1 1) (r 1 3)))
                  tach 2000]
              (elem font-4 :sh (cell= (r 1 (count fans))) :sv (b 500 sm (r 1 1)) :ah :mid :av :beg
               ; :bc (cell= (if selected? red grey-5))
                :tc (cell= (if selected? white grey-1))
                :m  :pointer
                :click #(reset! fan-id (if (= @fan-id @id*) nil @id*))
                (elem :sh (r 1 1) :sv (cell= (r (- 4500 tach) 5008)) :pv g-lg :ah :mid
                  name*)
                (elem :sh (r 1 1) :sv (cell= (r (+ tach 500) 5000)) :pv g-lg :rt 2 :ah :mid :c (cell= (if device ((t->c temp) :a alpha) (red :a alpha))) :tx :capitalize
                  (cell= (str tach " RPM"))))))))
      (elem :sh (r 1 1) :sv (r 2 5) :g l
        (list
          (elem :sh (>sm 300) :sv (r 1 1)
            (elem font-2 :sh (r 1 1) :sv 64 :ph g-lg :av :mid :c black
              "Mode")
            (if-tpl fan-id
              (elem :sh (r 1 1) :sv (- (r 1 1) 64) :c grey-5
                (for-tpl [{:keys [id name type] :as fan*} (cell= (cons {:id nil :name "Manual" :type :manual} devices))]
                  (let [selected (cell= (= id device))]
                    (elem font-5 :sh (r 1 1) :p g-lg :g g-lg :av :mid :m :pointer
                       :c     (cell= (when selected grey-4))
                       :click #(s/set-fan-device! @conn @fan-id @id)
                      (image :s 26 :src (cell= (str (hoplon.ui.utils/name type) "-icon.svg")))
                      (elem name)))))
              (elem font-2 :sh (r 1 1) :sv (- (r 1 1) 64) :p g-lg :c grey-5 :a :mid :tc (white :a 0.9)
                "no fan selected")))
          (elem :sh (>sm (- (r 1 1) 300 l)) :sv (r 1 1) :c grey-5
            (elem font-2 :sh (r 1 1) :sv 64 :ph g-lg :av :mid :c black
              "Temperature & Speed")
            (if-tpl fan-id
              (elem :sh (r 1 1) :sv (- (r 1 1) 64) :p g-lg :a :mid
                (if-tpl device
                  (elem font-2 :s (r 1 1) :c grey-5 :a :mid :tc (white :a 0.9)
                    "auto")
                  (elem font-2 :s (r 1 1) :gh g-xl :a :mid :tc (white :a 0.9)
                    "0%"
                    (hslider :sh (b 150 332 190 400 240 426 300 sm 450 md 600) :sv 28 :r 14 :c black
                       :src (cell= pwm #(s/set-fan-pwm! @conn @fan-id (int %))))
                    "100%"))))
              (elem font-2 :sh (r 1 1) :sv (- (r 1 1) 64) :p g-lg :c grey-5 :a :mid :tc (white :a 0.9)
                "no fan selected")))))))

(window :g l :c grey-4 :scroll (b true sm false) :src route :title "Xotic"
  (elem :sh (r 1 1) :ah :mid :c black
    (elem :sh (r 1 1) :ah (b :mid sm :beg) :av (b :beg sm :mid) :p g-lg :gv g-lg
      (image :sh 200 :src "xotic-pc-logo.svg" :m :pointer :click #(.open js/window "https://www.xoticpc.com"))
      (elem :sh (>sm (- (r 1 1) 200)) :ah (b :mid sm :end) :gh (* 2 g-lg)
        (for [[logo link] footer-menu-items]
          (image :m :pointer :src logo :click #(.open js/window link))))))
  (if-tpl (cell= (not data))
    (elem :sh (r 1 1) :sv (- (r 1 1) (- 78 l)) :pb 100 :a :mid :c black
      (elem :s 100 :g 10 :ah :mid
        (image :src "loading-icon.png")
        (elem :sh (r 1 1) :ah :mid font-2 :tc (white :a 0.9)
          "connecting")))
    (let [sh-close 80 sh-open 240]
      (elem :sh (r 1 1) :sv (b nil sm (- (r 1 1) (- 78 l))) :g l
        (elem :sh (>sm sh-close md sh-open) :sv (b nil sm (r 1 1)) :gv l
          (for [{label :label v :view} main-menu-items]
            (let [selected (cell= (= state v))]
              (elem font-4 :sh (r 1 1) :s sh-close :ph g-lg :gh g-lg :ah (b :mid md :beg) :av :mid :c (cell= (if selected grey-4 grey-5)) :tc (cell= (if selected white grey-1)) :bl 2 :bc (cell= (if selected red grey-5)) :m :pointer :click #(reset! state v)
                (image :s 34 :a :mid :src (when v (str (name v) "-icon.svg")))
                (when-tpl (b true sm false md true)
                  (elem :sh (b 120 sm (- (r 1 1) 34 g-lg))
                    label)))))
          (b nil sm (elem :sh (>sm sh-close md sh-open) :sv (- (r 1 1) (- (* (count main-menu-items) (+ 80 l)) l)) :c grey-6)))
        (elem :sh (>sm (- (r 1 1) sh-close l) md (- (r 1 1) sh-open l)) :sv (b nil sm (r 101)) :g l
          (case-tpl state
            :system (system-view)
            :fans   (fans-view)
            :lights (lights-view)))))))
