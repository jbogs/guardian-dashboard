(ns guardian.visuals
  (:require
    [javelin.core    :refer [with-let]]
    [hoplon.core     :refer [defelem]]
    [hoplon.ui       :refer [elem]]
    [hoplon.ui.elems :refer [in]]
    [cljsjs.chartist]))

(defelem line-chart [{:keys [labels series styles] :as attrs} _]
  (with-let [e (elem (dissoc attrs :labels :series))]
    (doto (js/Chartist.Line. (in e) (clj->js {:labels labels :series series :pointSmooth false :lineSmooth false}))
          (.on "draw" #(when-let [style (some (fn [[k v]] (prn :k k :v v :t (.-type %)) (= (.-type %) (name k)) v) styles)]
                         (.attr (.-element %) #js{:style style}))))))
