(set-env!
  :asset-paths  #{"rsc"}
  :source-paths #{"src"}
  :dependencies '[[org.clojure/clojure       "1.7.0"          :scope "test"]
                  [org.clojure/clojurescript "1.7.228"        :scope "test"]
                  [adzerk/boot-cljs          "1.7.228-1"      :scope "test"]
                  [adzerk/boot-reload        "0.4.12"         :scope "test"]
                  [hoplon/boot-hoplon        "0.2.4"          :scope "test"]
;                  [cljs/pprint "LATEST"]
                                        ;                  [funcool/cuerdas "2.0.0"] ; google string library
;                  [goog/string "LATEST"]
                  [tailrecursion/boot-static "0.0.1-SNAPSHOT" :scope "test"]
                #_[tailrecursion/boot-bucket "0.1.0-SNAPSHOT" :scope "test"]
                  [hoplon/ui                 "0.1.0-SNAPSHOT"]
                  [hoplon/castra             "3.0.0-alpha4"]])
(require
  '[adzerk.boot-cljs          :refer [cljs]]
  '[adzerk.boot-reload        :refer [reload]]
  '[hoplon.boot-hoplon        :refer [hoplon]]
 #_[tailrecursion.boot-bucket :refer [spew]]
  '[tailrecursion.boot-static :refer [serve]])

(def buckets
  {:production "guardian-production-application"
   :staging    "guardian-staging-application"})

(deftask develop
  [o optimizations OPM kw "Optimizations to pass the cljs compiler."]
  (let [o (or optimizations :none)]
    (comp (watch) (speak) (hoplon) (reload) (cljs :optimizations o) (serve))))

(deftask build
  [o optimizations OPM kw "Optimizations to pass the cljs compiler."]
  (let [o (or optimizations :simple)] ; default to advanced when fixed
    (comp (speak) (hoplon) (cljs :optimizations o :compiler-options {:elide-asserts true}) (sift))))

(deftask deploy
  "Build the application with advanced optimizations then deploy it to s3."
  [e environment   ENV kw "The application environment to be utilized by the service."
   o optimizations OPM kw "Optimizations to pass the cljs copmiler."]
  (assert environment "Missing required environment argument.")
  (let [b (buckets environment)]
    (comp (build :optimizations optimizations) #_(spew :bucket b))))

(task-options!
  serve   {:port 7000}
  sift    {:include #{#"index.html.out/" #"guardian/"} :invert true}
  #_spew    {:access-key (System/getenv "<YOUR_AWS_ACCESS_KEY>")
           :secret-key (System/getenv "<YOUR_AWS_SECRET_KEY")})
