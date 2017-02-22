(ns guardian.dashboard.visualizations
  (:require
    [javelin.core    :refer [cell=]]
    [hoplon.core     :refer [defelem for-tpl]]
    [hoplon.svg      :refer [g line rect]]
    [hoplon.ui       :refer [elem image svg]]
    [hoplon.ui.attrs :refer [r rgb translate]]
    [hoplon.ui.elems :refer [in]]))

;;; utils ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defelem chart [{:keys [b bh bv p ph pv pl pr pt pb] :as attrs} elems]
  "construct a vector image suitable for data visualizations where the size is
   the sum of the fill and the padding.  this is necessary because svg strokes
   cannot be inset, and will be cropped by the edges of the container if there's
   not padding equal to at least half the width of the stroke.  this follows the
   convention described by https://bl.ocks.org/mbostock/3019563."
  (let [pl (or p ph pl 0)
        pr (or p ph pr 0)
        pt (or p pv pt 0)
        pb (or p pv pb 0)
        bh (or b bh)
        bv (or b bv)]
    (svg :view-box [0 0 (+ bh pl pr) (+ bv pt pb)] (dissoc attrs :b :bh :bv :p :ph :pv :pl :pr :pt :pb)
      (g :transform (translate pl pt)
         elems))))

(defelem path [{:keys [data stroke] :as attrs}]
  (chart :p stroke
    (path :d data (dissoc attrs stroke :data))))

(defelem grid [{:keys [bh bv xticks yticks]}]
  (list
    (for-tpl [y (drop 1 yticks)]
      (line :x1 0 :x2 bh :y1 y :y2 y :stroke "#161616" :stroke-width 1))
    (for-tpl [x (drop 1 xticks)]
      (line :x1 x :x2 x :y1 0 :y2 bv :stroke "#161616" :stroke-width 1))))

;;; charts ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defelem dist-chart [{:keys [domain range] :as attrs} _]
  (let [total (cell= (apply + (mapv :value domain)))]
    (elem :p 10 (dissoc attrs :domain :range)
      (for-tpl [[{:keys [label value]} {:keys [color]}] (cell= (mapv vector domain (cycle range)))]
        (elem :sh (cell= (r value total)) :sv (r 1 1) :a :mid :c color
          (cell= (when (> value 5) label)))))))

(defelem histogram [{:keys [name icon data] :as attrs}]
  (let [bh 360 bv 100 s 2 o (/ s 2)]
    (elem :d :pile (dissoc attrs :data)
      (chart :s (r 1 1) :bh bh :bv bv
        (for-tpl [[i {:keys [label value color]}] (cell= (map-indexed vector data))]
          (let [x (cell= (- bh (- (* (count data) s) (* i s) o)))]
            (line :x1 x :y1 (cell= (- bv value)) :x2 x :y2 bv :stroke color :stroke-width s :stroke-linecap "round")))
        (grid :bh bh :bv bv :xticks (range 0 bh (* 10 s)) :yticks (range 0 bv (* 5 s))))
      #_(elem :sh (r 1 1) :p 10 :av :mid
        (image :s 14 :url icon)
        (elem :sh (- (r 1 1) 24 10) :p 8
          name)))))

(defelem gauge [{:keys [data cfn] :as attrs}]
  (elem :d :pile (dissoc attrs :data :cfn)
    (elem :s (r 1 1) :a :mid :f 36 :fw 2 :ft :500 :fc (cell= (-> data :value cfn))
      (cell= (str (:value data) "°C")))))

(defelem cpu-capacity [{:keys [data cfn] :as attrs}]
  (let [b 300 s 4 p (/ b 11) h 70]
    (elem :d :pile (dissoc attrs :data :cfn)
      (elem :s (r 1 1) :pt 8
        (for-tpl [{{temp :value} :temp {freq :value} :freq :keys [name threads]} (cell= (:cores data))]
           (elem :sh (cell= (r 1 (count (:cores data)))) :sv (r 1 1) :g 2 :ah :mid
              (elem :sh (r 1 1) :ah :mid :f 12 :fc (cell= (cfn temp))
                 (cell= (str (/ freq 1000) "GHz")))
              (elem :sh (r 1 1) :ah :mid :f 12 :fc (cell= (cfn temp))
                 (cell= (str temp "°")))
              (elem :sh (r 1 1) :ah :mid :f 12 :fc (cell= (cfn temp))
                 (cell= (apply str (interpose " " (mapv #(-> % :load :value (str "%")) threads))))))))
      (chart :s (r 1 1) :b b
         (for-tpl [[i {{temp :value} :temp :keys [name threads]}] (cell= (map-indexed vector (:cores data)))]
            (let [w (cell= (- (/ b (count (:cores data))) (* 2 p)))]
              (g :transform (cell= (translate (+ p (* i (/ b (count (:cores data))))) 0))
                (for-tpl [[j {{load :value} :load name :name}] (cell= (map-indexed vector threads))]
                  (let [x (cell= (+ (* j (/ w (count threads))) (/ w 4)))]
                    (line :x1 x :y1 (cell= (- b (* (/ load 100) (- b h)))) :x2 x :y2 b :stroke (cell= (cfn temp)) :stroke-width s :stroke-linecap "round"))))))))))

(defelem gpu-capacity [{:keys [data cfn] :as attrs}]
  #_(prn :data @data)
  (elem :d :pile (dissoc attrs :data :cfn)
    (elem :s (r 1 1) :a :mid :f 36 :fw 2 :ft :500 :fc (cell= (-> data :gpu :temp :value cfn))
      (cell= (str (-> data :gpu :load :value) "%")))
    (elem :s (r 1 1)
       (for-tpl [{{temp :value} :temp {freq :value} :freq :keys [name threads]} (cell= (:cores data))]
         (elem :sh (cell= (r 1 (count (:cores data)))) :sv (r 1 1) :g 8 :ah :mid :av :end
           (elem :sh (r 1 1) :ah :mid :fc (rgb 0x414141)
             freq)
           (for-tpl [{{load :value} :load name :name} threads]
             (elem :sh 4 :sv (cell= (+ (* load 2) 6)) :r 6 :c (cell= (cfn temp)))))))))

#_(defelem gpu-capacity [{:keys [data cfn] :as attrs}]
  (prn :data @data)
  (elem :d :pile (dissoc attrs :data :cfn)
    (elem :s (r 1 1) :a :mid :f 36 :fw 2 :ft :500 :fc (cell= (-> data :gpu :temp :value cfn))
      (cell= (str (-> data :gpu :load :value) "%")))
    (elem :s (r 1 1)
       (for-tpl [{{temp :value} :temp {freq :value} :freq :keys [name threads]} (cell= (:cores data))]
         (elem :sh (cell= (r 1 (count (:cores data)))) :sv (r 1 1) :g 8 :ah :mid :av :end
           (elem :sh (r 1 1) :ah :mid :fc (rgb 0x414141)
             freq)
           (for-tpl [{{load :value} :load name :name} threads]
             (elem :sh 4 :sv (cell= (+ (* load 2) 6)) :r 6 :c (cell= (cfn temp)))))))))
