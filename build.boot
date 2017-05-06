(set-env!
  :asset-paths  #{"rsc"}
  :source-paths #{"src"}
  :dependencies '[[org.clojure/clojure       "1.8.0"          :scope "test"]
                  [org.clojure/clojurescript "1.9.494"        :scope "test"]
                  [adzerk/boot-cljs          "1.7.228-2"      :scope "test"]
                  [adzerk/boot-reload        "0.4.13"         :scope "test"]
                  [adzerk/env                "0.3.1"          :scope "test"]
                  [tailrecursion/boot-static "0.0.1-SNAPSHOT" :scope "test"]
                  [tailrecursion/boot-bucket "0.2.1-SNAPSHOT" :scope "test"]
                  [tailrecursion/boot-front  "0.1.0-SNAPSHOT" :scope "test"]
                  [cljsjs/d3                 "4.3.0-3"]
                  [hoplon/ui                 "0.2.1-SNAPSHOT"]])

(require
  '[clojure.string :as s]
  '[adzerk.boot-cljs          :refer [cljs]]
  '[adzerk.boot-reload        :refer [reload]]
  '[hoplon.boot-hoplon        :refer [hoplon]]
  '[tailrecursion.boot-bucket :refer [spew]]
  '[tailrecursion.boot-front  :refer [burst]]
  '[tailrecursion.boot-static :refer [serve]])

(def +version+ "0.9.3")

(def buckets
  {:guardian "guardiangui"
   :xotic    "xoticpcgui"})

(def distributions
  {:guardian "guardian"
   :xotic   "xotipcdistro"})

(def services
  {:local     "ws://localhost:8000"
   :laptop    "ws://jbog.pagekite.me:8000"
   :aws       "ws://ec2-54-193-74-95.us-west-1.compute.amazonaws.com:8000"
   :simulator "ws://simulator.pagekite.me:8000"})

;;; tasks ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftask develop
  [s service       SVC kw   "The guardian server the client should connect to."
   o optimizations OPM kw   "Optimizations to pass the cljs compiler."
   v no-validate       bool "Exclude validations from build"]
  (let [o (or optimizations :none)
        s (or service       :local)]
    (System/setProperty "URL" (services s))
    (comp (watch) (speak) (hoplon) (reload) (cljs :optimizations o :compiler-options {:language-in :ecmascript5-strict :elide-asserts no-validate}) (serve))))

(deftask build
  [s service       SVC kw "The guardian server the client should connect to."
   o optimizations OPM kw "Optimizations to pass the cljs compiler."]
  (let [o (or optimizations :simple) ;; default to simple until hsl->rgb lib issue resolved
        s (or service       :local)]
    (System/setProperty "URL" (services s))
    (comp (speak) (hoplon) (cljs :optimizations o :compiler-options {:language-in :ecmascript5-strict :elide-asserts true}) (sift))))

(deftask deploy
  "Build the application with advanced optimizations then deploy it to s3."
  [e environment   ENV kw "The aws environment to deploy to."
   s service       SVC kw "The service the client should connect to."
   o optimizations OPM kw "Optimizations to pass the cljs compiler."]
  (assert environment "Missing required environment argument.")
  (let [b (buckets       environment)
        d (distributions environment)]
    (comp (build :optimizations optimizations :service service) (spew :bucket b) (burst :distribution d))))

(deftask package
  "Build the application with advanced optimizations then dump it into the tgt folder."
  [s service       SVC kw `"The guardian server the client should connect to."
   o optimizations OPM kw "Optimizations to pass the cljs compiler."]
  (comp (build :optimizations optimizations :service service) (target :dir #{"tgt"})))

(deftask distribute
  "Build the application with advanced optimizations then zip it."
  [s service       SVC kw `"The guardian server the client should connect to."
   o optimizations OPM kw "Optimizations to pass the cljs compiler."]
  (comp (build :optimizations optimizations :service service)
        (zip :file (str "guardian-dashboard-" +version+  ".zip"))
        (sift :include #{#"guardian-dashboard-*."} :invert false)
        (target :dir #{"dst"})))

(task-options!
  serve {:port 7000}
  sift  {:include #{#"index.html.out/" #"guardian/"} :invert true}
  spew  {:access-key (System/getenv "ROOT_JBOG_AWS_ACCESS_KEY")
         :secret-key (System/getenv "ROOT_JBOG_AWS_SECRET_KEY")}
  burst {:access-key (System/getenv "ROOT_JBOG_AWS_ACCESS_KEY")
         :secret-key (System/getenv "ROOT_JBOG_AWS_SECRET_KEY")})
