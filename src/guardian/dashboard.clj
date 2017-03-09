(ns  guardian.dashboard
  (:require
    [hoplon.ui.attrs :refer [- r d rgb hsl lgr]]))

(defmacro dataurl [path & [ext]]
  (str "data://" (or ext "png") ";" (slurp path)))
