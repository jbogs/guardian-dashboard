(set-env!
  :asset-paths  #{"rsc"}
  :source-paths #{"src"}
  :dependencies '[[org.clojure/clojure       "1.8.0"          :scope "test"]
                  [org.clojure/clojurescript "1.9.293"        :scope "test"]
                  [adzerk/boot-cljs          "1.7.228-2"      :scope "test"]
                  [adzerk/boot-reload        "0.4.13"         :scope "test"]
                  [adzerk/env                "0.3.1"          :scope "test"]
                  [tailrecursion/boot-static "0.0.1-SNAPSHOT" :scope "test"]
                  [tailrecursion/boot-bucket "0.1.0-SNAPSHOT" :scope "test"]
                  [cljsjs/d3                 "4.3.0-3"]
                  [hoplon/ui                 "0.1.0-SNAPSHOT"]])

(require
  '[clojure.string :as s]
  '[adzerk.boot-cljs          :refer [cljs]]
  '[adzerk.boot-reload        :refer [reload]]
  '[hoplon.boot-hoplon        :refer [hoplon]]
  '[tailrecursion.boot-bucket :refer [spew]]
  '[tailrecursion.boot-static :refer [serve]])

(def buckets
  {:laptop "xoticpcgui"})

(def services
  {:local     "ws://localhost:8000"
   :laptop    "ws://jbog.pagekite.me:8000"
   :aws       "ws://ec2-54-193-74-95.us-west-1.compute.amazonaws.com:8000"
   :simulator "ws://simulator.pagekite.me:8000"})

;;; tasks ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftask develop
  [e environment   ENV kw   "Environment of the server to connect to"
   o optimizations OPM kw   "Optimizations to pass the cljs compiler."
   v no-validate       bool "Exclude validations from build"]
  (let [o (or optimizations :none)
        e (or environment   :local)]
    (System/setProperty "URL" (services e))
    (comp (watch) (speak) (hoplon) (reload) (cljs :optimizations o :compiler-options {:language-in :ecmascript5-strict :elide-asserts no-validate}) (serve))))

(deftask build
  [e environment   ENV kw "The application environment to be utilized by the service."
   o optimizations OPM kw "Optimizations to pass the cljs compiler."]
  (let [o (or optimizations :advanced)
        e (or environment   :local)]
    (System/setProperty "URL" (services e))
    (comp (speak) (hoplon) (cljs :optimizations o :compiler-options {:language-in :ecmascript5-strict :elide-asserts true}) (sift))))

(deftask deploy
  "Build the application with advanced optimizations then deploy it to s3."
  [e environment   ENV kw "The application environment to be utilized by the service."
   o optimizations OPM kw "Optimizations to pass the cljs compiler."]
  (assert environment "Missing required environment argument.")
  (let [b (buckets environment)]
    (comp (build :optimizations optimizations :environment environment) (spew :bucket b))))

(deftask package
  "Build the application with advanced optimizations then dump it into the tgt folder."
  [e environment   ENV kw `"The application environment to be utilized by the service."
   o optimizations OPM kw "Optimizations to pass the cljs compiler."]
  (comp (build :optimizations optimizations :environment environment) (target :dir #{"tgt"})))

(task-options!
  serve {:port 7000}
  sift  {:include #{#"index.html.out/" #"guardian/"} :invert true}
  spew  {:access-key (System/getenv "ROOT_JBOG_AWS_ACCESS_KEY")
         :secret-key (System/getenv "ROOT_JBOG_AWS_SECRET_KEY")})
