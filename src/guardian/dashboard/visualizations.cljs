(ns guardian.dashboard.visualizations
  (:require
    [javelin.core    :refer [cell=]]
    [hoplon.core     :refer [defelem for-tpl]]
    [hoplon.svg      :refer [g line rect]]
    [hoplon.ui       :refer [elem image svg]]
    [hoplon.ui.attrs :refer [r translate]]
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
    (for-tpl [y yticks]
      (line :x1 0 :x2 bh :y1 y :y2 y :stroke "#202020" :stroke-width 1))
    (for-tpl [x xticks]
      (line :x1 x :x2 x :y1 0 :y2 bv :stroke "#202020" :stroke-width 1))))

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
      (elem :sh (r 1 1) :p 10 :av :mid
        (image :s 14 :url icon)
        (elem :sh (- (r 1 1) 24 10) :p 8
          name)))))

(defelem gauge [{:keys [data cfn] :as attrs}]
  (elem :d :pile (dissoc attrs :data :cfn)
    (elem :s (r 1 1) :a :mid :f 36 :fw 2 :ft :500 :fc (cell= (-> data :value cfn))
      (cell= (str (:value data) "°C")))))

(defelem cpu-capacity [{:keys [data cfn] :as attrs}]
  (elem :d :pile (dissoc attrs :data :cfn)
    (elem :s (r 1 1) :a :mid :f 36 :fw 2 :ft :500 :fc (cell= (-> data :temp :value cfn))
      (cell= (str (-> data :load :value) "%")))
    (elem :s (r 1 1)
       (for-tpl [{:keys [name temp threads]} (cell= (:cores data))]
         (elem :sh (cell= (r 1 (count (:cores data)))) :sv (r 1 1) :gh 8 :ah :mid :av :end
           (for-tpl [{:keys [name load]} threads]
             (elem :sh 4 :sv (cell= (+ (* load 3) 6)) :r 6 :c (cell= (cfn temp))))))))) ;; can't use ratio because of https://github.com/hoplon/ui/issues/25

#_(elem :s 300 :c grey-4 :b 10 :bc grey-5
  (for-tpl [{:keys [name temp threads]} (cell= (:cores data-model))]
    (elem :sh (cell= (r 1 (count (:cores data-model)))) :sv (r 1 1) :gh 8 :ah :mid :av :end
      (for-tpl [{:keys [name load]} threads]
        (elem :sh 2 :sv (cell= (+ (* load 3) 10)) :r 6 :c (cell= (condp < temp 40 :blue 50 :yellow :red)))))))

#_(defelem line-chart [{:keys [labels series styles] :as attrs} _]
  (with-let [e (elem (dissoc attrs :labels :series))]
    (doto (js/Chartist.Line. (in e) (clj->js {:labels labels :series series :pointSmooth false :lineSmooth false}))
      (.on "draw" #(when-let [style (some (fn [[k v]] (prn :k k :v v :t (.-type %)) (= (.-type %) (name k)) v) styles)]
        (.attr (.-element %) #js{:style style}))))))

#_(v/line-chart :sh (r 1 1) :sv (- (r 1 1) 30)
      :labels (->> (range) (take 11) (mapv #(str (* 10 %) "%")))
      :series [[0 5 8 10 8 10 8 10]]
      :styles {:line "stroke: red"})
