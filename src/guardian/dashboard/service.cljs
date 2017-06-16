(ns guardian.dashboard.service
  (:require
    [clojure.set    :refer [rename-keys]]
    [clojure.string :refer [replace]]
    [javelin.core   :refer [with-let cell cell=]]))

;;; config ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def effects
  {:off      ["Off"             nil            "off"]
   :color    ["Solid Color"     "static_color" "color"]
   :cpu-load ["CPU Load"        "cpu_load"     "cpu"]
   :cpu-temp ["CPU Temperature" "cpu_temp"     "cpu"]
   :gpu-load ["GPU Load"        "gpu_load"     "gpu"]
   :gpu-temp ["GPU Temperature" "gpu_temp"     "gpu"]})

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
                :effect    (some (fn [[k [_ n]]] (when (= n (:name %)) k)) effects)
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
   :effect    (or (some (fn [[k [_ n]]] (when (= n effect) k)) effects) :off)
   :color     color
   :beg-color beg_color
   :end-color end_color})

(defn fan [{:keys [name auto pwm tach]}]
  {:id   [:fan name]
   :name name
   :auto auto
   :pwm  pwm
   :tach tach})

(defn memory [{:keys [name free total] :as memory}]
  {:name  name
   :type  :memory
   :used  {:value (- total free)}
   :total {:value total}})

(defn motherboard [{{:keys [name temps]} :mb mem :memory kb :led_keyboard :keys [cpus gpus hdds fans strips uv_strips]}]
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
   :hard-drives    (into [] (sort-by (comp :name first :volumes) (mapv hard-drive hdds)))})

(defn device-data [data]
  data)

(defn data [data]
  (->> data
      (mapcat (fn [[k v]] [k (if (sequential? v) (mapv #(assoc % :type (colkey->type k)) v) v)]))
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
      (cell= (set! (.-onmessage conn) ~(fn [e] (let [d (parse e)] (when (= (:tag d) "sensors") (reset! state (data (:data d))))))))
      (cell= (set! (.-onerror   conn) ~(fn [e] (reset! error e))))
      (call "get_sensors" conn))))

(defn get-devices [conn]
  (device-data (call "get_devices" conn)))

(defn set-effect! [conn [type name :as id] effect]
  (call (type->colkey type) conn :name name :effect (-> effect effects second)))

(defn set-color! [conn [type name :as id] color]
  (call (type->colkey type) conn :name name :color color))

(defn set-beg-color! [conn [type name :as id] color]
  (call (type->colkey type) conn :name name :beg_color color))

(defn set-end-color! [conn [type name :as id] color]
  (call (type->colkey type) conn :name name :end_color color))
