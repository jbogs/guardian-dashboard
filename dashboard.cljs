(ns+ guardian.dashboard
     (:page "index.html")
     (:refer-clojure
      :exclude [-  next])
;      :rename [name cljname])   ;used to exclude name
     (:require
      [javelin.core    :refer [defc defc= cell=]]
      [hoplon.core     :refer [defelem for-tpl when-tpl if-tpl case-tpl do!
                               with-init! with-interval]]
      [hoplon.ui       :refer :all] ; [elem  image form window canvas ]]
      [hoplon.ui.attrs :refer [- c r d]]
      [chart.core :as c])
      )


;;; from plotSVG ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn by-id [id]
  (.getElementById js/document (name id)))

(defn val-id [id]
  (do! (by-id id) :value))

;; colors
(def   c1      "#006666")
(def   c2      "#660066")

;; returns a seq of random [x y] pairs, 0 <= y <= 9
(defn data! []
  (vec (for [x (range 0 11)] [x (rand-int 11)])))

;; push a random value onto the end of a data series
(defn add! [data]
  (conj data [(-> data last first inc) (rand-int 11)]))

;; two data series
(defc  data1   (data!))
(defc  data2   (data!))
;; user configurations "knobs"
(defc  pwidth  400)
(defc  paused? false)

;; "clipped" data (moving strip-chart)
(defc= series1 (take-last 10 data1))
(defc= series2 (take-last 10 data2))

;; configure the plotting envelope (linear scale)
(defc= chart1
  (let [min-x (max (ffirst series1) (ffirst series2))
        max-x (min (first (last series1)) (first (last series2)))]
    (c/config
      :width pwidth :height   200
      :min-x  min-x :max-x  max-x
      :min-y      0 :max-y     10)))


;; add data random data points every 1000ms
(with-init!
  (with-interval 1000
    (when-not @paused?
      (swap! data1 add!)
      (swap! data2 add!))))


(defelem chart-object [attribs elems]
  (svg
      (c/container
        :chart    chart1
        :css      (cell= {:border      "1px solid black"
                          :margin-left (str "-" (- (/ (:width chart1) 2) 200) "px")})
        :click #(swap! paused? not)

        ;; draw shading first so it doesn't cover lines or points
        (c/polygon :chart chart1 :data series1 :css {:fill c1 :stroke "none" :fill-opacity 0.5})
        (c/polygon :chart chart1 :data series2 :css {:fill c2 :stroke "none" :fill-opacity 0.5})

        ;; draw lines
        (c/polyline :chart chart1 :data series1 :css {:fill "none" :stroke c1 :stroke-width 2})
        (c/polyline :chart chart1 :data series2 :css {:fill "none" :stroke c2 :stroke-width 2})

        ;; draw points last so they're not covered by lines or shading
        (c/points-circle :chart chart1 :data series1 :r 3 :css {:stroke c1 :fill c1})
        (c/points-rect :chart chart1 :data series2 :width 6 :height 6 :css {:stroke c2 :fill c2}))
  attribs elems
  ))

;;; constants ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; xotic images

(def background-texture "background texture.svg")
(def black-bar "black.bar.svg")
(def black-button  "Black button.svg")
(def black-button-red-line "Button normal complet.svg")
(def button-mouseover "Button mouseover complet.svg")
(def button-selected "Button Selected complet.svg")
(def circular-gage-bubbles  "Circular Gauge (Bubbles).svg")
(def circular-gage-fan-speed  "Circular Gauge (Fan Speed).svg")
(def circular-gage-lighting  "Circular Gauge (Lighting).svg")
(def circular-gage-radical "Circular Gauge (Radial).svg")
(def cpu-gpu-graph  "CPU-GPU Graph.svg")
(def cpu-gpu-load-ring "CPU icon.svg")
(def cpu-icon "CPU_GPU_LOAD_ring.svg")
(def cpu-load-bar "cpu-load-bar.svg")

(def cpu-memory-bar  "cpu-memory-bar.svg")
;(def cpu-memory-bar "laptop icon.svg")

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
(def motherboard-fan-graph  "Motherboard Fan Graph.svg")
(def motherboard-icon  "Motherboard icon.svg")
(def operating-system-icon  "Operating System icon.svg")
(def pc-name-icon  "PC Name icon.svg")
(def picagram-icon "Picagram icon.svg")
(def preset-toggle-off  "Preset Toggle Off.svg")
(def preset-toggle-on  "Preset Toggle On.svg")
(def processor-icon  "Processor icon.svg")
(def ram-icon "RAM icon.svg")
(def temp-background "TEMP background.svg")
(def title-background "Title background.svg")
(def twitter-icon  "Twitter icon.svg")
(def video-card-icon  "Video Card icon.svg")
(def xotic-pc-emblem-white  "XoticPC Emblem White.svg")
(def xoticpc-emblem-red  "XoticPC Emblem Red.svg")
(def xoticpc-logo  "XoticPC Logo.svg")
(def youtube-icon  "Youtube icon.svg")
(def zone-keyboard "Zone keyboard.svg")

;;; state ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defc selection "SYSTEM HEALTH")

;;; service ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; query ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; command ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; view styles ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; breakpoints

; sizes

;; colors


; fonts
(def default-font-style ["Helvetica Neue" "Lucida Grande" :sans-serif])
(def default-font-size 10)
(def default-font-color 0xffffff)
;(defn set-default-font []   (list  :ff default-font-style
;                              :f default-font-size
;                              :fc (c default-font-color)))
; text
#_ (defelem html [attrs html]
  (set! (.-innerHTML (elem attrs)) html))

;;; view components ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn logo-over-name []
  "render xotic's logo on top of the name of the pc"
  (elem :ah :beg
        (image #_ (:ph 11) :url exotic-pc )
        "PC NAME INFO"))

(defelem tab-button [attrs elems]
  (image  :a :mid #_ (:b 1 :bc :white)
          :click #(reset! selection elems)
                                        ;               :onmouseout #(println "mouse went out")
          :url (cell= (if (= elems selection)
                        button-selected
                        black-button-red-line))
          attrs elems))

(defelem processor-status [{:keys [s sh sv a ah av p ph pv g gh gv] :as attr} attrs [type] elems]
  "display a CPU/GPU status collection"
;  (println attr)
  (elem   :s s :sh sh :sv sv :a a :ah ah :av av :p p :ph ph :pv pv :g g :gh gh :gv gv
          (image :url title-background
                 (image :url info-button) (str "   " (first attrs) "  - INFO"))
          (elem :ah :beg :pv 20 :g 10
                (image :url temp-background)
                (image :s (r 1 2.2) :url circular-gage-radical))
  ) 



;;; view views ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; application ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(enable-console-print!)


(window
  :title        "Xotic"
  ;:route        route
  ;:styles       [icon-url font-url]
  ;:initiated    (partial swap! db initiate)
                                        ;:routechanged (partial swap! db change-route)

 :ff default-font-style
 :f default-font-size
 :fc (c default-font-color)

 (image :c (c 0x303030) :url background-texture
        (image
         ;:ah :beg :av :beg

         :url page-header ; :ah :beg :av :beg
         (elem
              :sh (r 3 1)  :ah :beg :av :mid :ph 20
          "PC NAME - INFO")  
               :ah :mid :av :end
                  (elem ; all buttons
                   :ah :mid
                   :av :end
                   :gh 70
                   :pv 5
                   :gh 145
                   (elem :gh 20
                         (tab-button "SYSTEM HEALTH" nil)
                         (tab-button "LIGHTING" nil))
                   (elem :gh 20
                         (tab-button "FANS" nil)
                         (tab-button "INFO" nil))))

        (elem  :s (r 1 1)  :p (r 1 20)   ; contains everything below the buttons

               (processor-status
                :sh (r 1 2)
                :ah :beg
                "CPU" )
               (processor-status
                :sh (r 1 2)
                :ah :end
                "GPU")

               (elem :g 10 ; mid section
                     (elem ; the single overbar midscreen, its info icon, and text
                      (image  :sh (r 1 1) :sv (r 1 2)  :ah :beg
                              :url long-title-background
                              (elem :av :mid
                                    (image :av :mid :ph 5  :url info-button) "CPU - INFO")))
                     (elem :g 20; containing memory and load bars
                           (image :ah :beg :s (r 1 1.35) :url cpu-memory-bar)  ; memory
                           (image :ah :end :s (r 1 4.4) :url cpu-load-bar)))

               )))






  ;; (image :b 1 :bc :white  :url black-button-red-line :ah :mid :av :mid "SYSTEM HEALTH")
  ;; (image :b 1 :bc :white :url button-mouseover  :ah :mid :av :mid "LIGHTING")
                  ;; ;(image  :url info-button)
                  ;; (image :b 1 :bc :white  :url black-button-red-line :ah :mid :av :mid "FANS")
                  ;; (image :b 1 :bc :white :url black-button-red-line :ah :mid :av :mid "INFO")))))

#_                  (elem   ; left set of buttons
                                        ;                  :sh (r 1 2)
                  :ah :mid
                  :pv 5
                  :gh 1
                  :b 1 :bc :white
                                        ;:sh (r 1 2) :pv 6 :ph 5   :gh 5
                  (image :url black-button-red-line :ah :mid :av :mid "SYSTEM HEALTH")
                  (image :url button-mouseover  :ah :mid :av :mid "LIGHTING"))
#_                 (elem    ; right set of buttons
                  :pv 5
                  :b 1 :bc :white
                  (image :url black-button-red-line :ah :mid :av :mid "FANS")
                  (image :url black-button-red-line :ah :mid :av :mid "INFO"))

#_ (image :url "http://placekitten.com/300/300"
        (elem :sh (r 1 2) :ah :beg 
              (image :url "http://placekitten.com/100/100"
                     :s 100
                     :b 3 :bc :white))
        (elem :sh (r 1 2) :ah :mid
              (image :url "http://placekitten.com/100/100"
                     :s 100
                     :b 3 :bc :white)))



    ;(image :s 1000 :ah :beg :av :jst :url menu-top))
            ;(image :sh (r 1 2) :sv (r 1 2)  :url exotic-pc)
            ;(image :url logo)
            ;(image :s 100 :url exotic-pc))))

                                        ;:ff helvetica :fr :optimizeLegibility :fm :antialiased
; (elem :sh (r 1 3) :p 6 :av :mid :b 2 ; :bc stroke-grey
;        (image :s 50 :url header-logo)
      ;(image :s 50 :url "http://hoplon.github.io/assets/images/logos/hoplon-logo.png")
       ;(image :s 50 :url "http://sovietov.com/tmp/guardian/guardian_logo.png")
                                        ;(image :s 50 :url header-logo)

;      (elem :pl 6 :f 21 "XOTIC PC Logo"))

 ; (elem :url "http://sovietov.com/tmp/guardian/guardian_logo.png")





  ;(image :url keyboard-keys-pad))

  ;; (when-tpl (b menu-open lg true)
  ;;   (sidebar :sh 240 :sv (r 1 1) :xl (b menu-open lg false)))
  ;; (main :sh (b (r 1 1) lg (- (r 1 1) 240)) :sv (r 1 1)
  ;;   (case-tpl state
  ;;     :basic      (basic-view)
  ;;     :event      (event-view)
  ;;     :events     (events-view)
  ;;     :project    (project-view)
  ;;     :projects   (projects-view)
  ;;     :organizers (organizers-view)
  ;;     :photo      (photo-view)
  ;;     :photos     (photos-view)
  ;;     :notes      (notes-view)))
  ;; (overlay :xl 240 :xr 0 :sh (r 1 1) :sv (r 1 1) :v (b menu-open lg false)))
