(ns guardian.dashboard.service
  (:require
    [clojure.set    :refer [rename-keys]]
    [clojure.string :refer [replace]]
    [javelin.core   :refer [with-let cell cell=]]))

;;; config ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def effects*
  [{:id     "none" ;; unique id used to set the effect on the server
    :name   "Off"  ;; label viewed by the user
    :types  []     ;; determines the semantic
    :source :none} ;; determines the icon
   {:id     "static_color"
    :name   "Solid Color"
    :types   [:color]
    :source :color}
   {:id     "morph"
    :name   "Morph"
    :types   [:color]
    :source :color}
   {:id     "rainbow"
    :name   "Rainbow"
    :types   [:speed :smooth]
    :source :color}
   {:id     "roll"
    :name   "Roll"
    :types   [:beg-color :end-color :speed]
    :source :color}
   {:id     "random"
    :name   "Random"
    :types   [:speed :random]
    :source :color}
   {:id     "police"
    :name   "Police"
    :types   [:beg-color :end-color :scale :drift]
    :source :color}
   {:id     "cpu_load"
    :name   "CPU Load"
    :types  [:beg-color :end-color]
    :source :cpu}
   {:id     "cpu_temp"
    :name   "CPU Temp"
    :types  [:beg-color :end-color]
    :source :cpu}
   {:id     "gpu_load"
    :name   "GPU Load"
    :types  [:beg-color :end-color]
    :source :gpu}
   {:id     "gpu_temp"
    :name   "GPU Temp"
    :types  [:beg-color :end-color]
    :source :gpu}])

(def sensors
  {:cpu-power         "Package"
   :cpu-temp          "Package"
   :cpu-load          "UC"
   :gpu-processor     "GPU"
   :gpu-memory        "Memory"
   :gpu-frame-buffer  "Frame Buffer"
   :gpu-video-engine  "Video Engine"
   :gpu-bus-interface "Bus Interface"
   :hdd-temp          "Assembly"
   :zone-1            "THRM"
   :zone-2            "TZ00"
   :zone-3            "TZ01"})

;;; utils ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn colkey->type [x]
  (keyword (replace (apply str (drop-last (name x))) #"_" "-")))

(defn type->colkey [x]
  (keyword (replace (str (name x) "s") #"-" "_")))

(defn name->sensor [reading]
  (-> (rename-keys reading {:name :sensor})
      (update :value int)))

(defn get-sensor [coll sensor]
 (-> (some #(when (= (sensor sensors) (:name %)) %) coll)
     (name->sensor)))

;;; xforms ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn cpu [{:keys [name clocks loads temps volts watts]}]
  (let [load    (get-sensor loads :cpu-load)
        temp    (get-sensor temps :cpu-temp)
        loads*  (remove (partial = load) loads)
        temps*  (remove (partial = temp) temps)
        threads (mapv #(hash-map :name (:name %) :load (name->sensor %1)) loads*)
        cores   (mapv #(hash-map :name (:name %) :freq (name->sensor %1) :temp (name->sensor %2)) clocks temps*)]
    {:name  name
     :type  :cpu
     :temp  temp
     :load  load
     :cores (mapv #(assoc % :threads %2) cores (partition 2 threads))}))

(defn hard-drive [{:keys [name loads temps] :as hdd}]
  {:name    name
   :type    :hard-drive
   :temp    (hash-map :value (apply + (mapv :value temps)))
   :load    (hash-map :value (apply + (mapv :value loads)))
   :volumes (mapv #(hash-map :name (:name %1) :load (name->sensor %1) :temp (name->sensor %2)) loads temps)})

(defn graphics-card [{:keys [name loads temps]}]
  (let [card {:name name :type :graphics-card}]
    (if (seq loads)
      (assoc card
        :name          name
        :type          :graphics-card
        :integrated?   false
        :gpu           {:name "GPU"
                        :temp (get-sensor temps :gpu-processor)
                        :load (get-sensor loads :gpu-processor)}
        :memory        {:name "Memory"
                        :load (get-sensor loads :gpu-memory)}
        :frame-buffer  {:name "Frame Buffer"
                        :load (get-sensor loads :gpu-frame-buffer)}
        :video-engine  {:name "Video Engine"
                        :temp (get-sensor loads :gpu-video-engine)}
        :bus-interface {:name "Bus Interface"
                        :temp (get-sensor loads :gpu-bus-interface)})
      (assoc card :integrated? true))))

(defn keyboard [keyboard]
  (let [zone #(hash-map
                :id        (->> % :zone js/parseInt)
                :name      (->> % :zone (str "Zone "))
                :effect    (:name %)
                :color     (-> % :color)
                :beg-color (-> % :beg_color)
                :end-color (-> % :end_color))
        zones (->> (dissoc keyboard :all)
                   (sort-by first)
                   (mapv (comp zone second)))]
    {:name  "Keyboard"
     :type  :kb
     :zones zones}))

(defn light [{:keys [name type effect color beg_color end_color]}]
  {:id        [type name]
   :name      name
   :type      type
   :effect    (or effect "none")
   :color     color
   :beg-color beg_color
   :end-color end_color})

(defn fan [{:keys [name auto pwm tach temp]}]
  {:id   [:fan name]
   :name name
   :auto (if (= auto 0) false true)
   :pwm  pwm
   :tach tach
   :temp temp})

(defn memory [{:keys [name free total] :as memory}]
  {:name  name
   :type  :memory
   :used  {:value (- total free)}
   :total {:value total}})

(defn effect [{:keys [id name source types]}]
   {:id     id
    :name   name
    :source (keyword source)
    :types  (mapv keyword types)})

(defn motherboard [{{:keys [name temps]} :mb mem :memory kb :led_keyboard :keys [cpus gpus hdds fans strips uv_strips effects]}]
  {:name           name
   :type           :mb
   :zone-1         {:name "CPU Thermal Zone"
                    :desc "Located next to the CPU core on Intel boards"
                    :temp (get-sensor temps :zone-1)}
   :zone-2         {:name "North Bridge Thermal Zone"
                    :desc "Located next to the CPU socket on Intel boards"
                    :temp (get-sensor temps :zone-2)}
   :zone-3         {:name "South Bridge Thermal Zone"
                    :desc "Located next to the memory slots on Intel boards"
                    :temp (get-sensor temps :zone-3)}
   :gpu             {:name (-> gpus first :name)}
   :memory         (memory mem)
   :lights         (mapv light (concat fans strips uv_strips (:zones (keyboard kb))))
   :fans           (mapv fan fans)
   :cpus           (mapv cpu cpus)
   :graphics-cards (into [] (sort-by :integrated? (mapv graphics-card gpus)))
   :hard-drives    (into [] (sort-by (comp :name first :volumes) (mapv hard-drive hdds)))
   :effects        (mapv effect (or effects effects*))})

(defn device-data [data]
  data)

(defn data [data]
  (->> data
      (mapcat (fn [[k v]] [k (if (and (sequential? v) (not= k :effects)) (mapv #(assoc % :type (colkey->type k)) v) v)]))
      (motherboard)))

;;; api ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn connect [url]
  (let [conn (cell (js/WebSocket. url))]
    (cell= (set! (.-onclose conn) ~(fn [_] (reset! conn (js/WebSocket. url)))))
    (-> (fn [resolve reject]
          (set! (.-onopen  @conn) #(resolve conn))
          (set! (.-onerror @conn) #(reject %)))
        (js/Promise.))))

(defn- call [tag conn & kwargs]
  (let [data (apply hash-map kwargs)]
    (->> {:tag tag :data data} (clj->js) (.stringify js/JSON) (.send @conn))))

(defn bind-sensors! [conn state error & [poll-freq hist-max]]
  (let [cljs #(js->clj % :keywordize-keys true)
        parse #(-> % .-data js/JSON.parse cljs)]
    (with-let [_ conn]
      (cell= (set! (.-onmessage conn) ~(fn [e] (let [d (parse e)] (when (= (:tag d) "sensors") #_(prn :data (:data d)) (reset! state (data (:data d))))))))
      (cell= (set! (.-onerror   conn) ~(fn [e] (reset! error e))))
      (call "get_sensors" conn))))

(defn get-devices [conn]
  (device-data (call "get_devices" conn)))

(defn set-effect! [conn [type name :as id] effect]
  (call (type->colkey type) conn :name name :effect effect))

(defn set-color! [conn [type name :as id] color]
  (call (type->colkey type) conn :name name :color color))

(defn set-beg-color! [conn [type name :as id] color]
  (call (type->colkey type) conn :name name :beg_color color))

(defn set-end-color! [conn [type name :as id] color]
  (call (type->colkey type) conn :name name :end_color color))

(defn set-speed! [conn [type name :as id] percent]
  (call (type->colkey type) conn :name name :speed percent))

(defn set-scale! [conn [type name :as id] percent]
  (call (type->colkey type) conn :name name :scale percent))

(defn set-drift! [conn [type name :as id] percent]
  (call (type->colkey type) conn :name name :drift percent))

(defn set-random! [conn [type name :as id] percent]
  (call (type->colkey type) conn :name name :random percent))

(defn set-smooth! [conn [type name :as id] percent]
  (call (type->colkey type) conn :name name :smooth percent))

(defn set-fan-pwm! [conn [type :name :as id] pwm]
  (call (type->colkey type) conn :name name :pwm pwm))

(defn set-fan-temp! [conn [type :name :as id] temp]
  (call (type->colkey type) conn :name name :temp temp))

(defn set-fan-auto! [conn [type :name :as id] auto]
  (call (type->colkey type) conn :name name :auto (if auto 1 0)))
