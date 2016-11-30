(set-env!
  :asset-paths  #{"rsc"}
  :source-paths #{"src"}
  :dependencies '[[org.clojure/clojure       "1.8.0"          :scope "test"]
                  [org.clojure/clojurescript "1.8.51"         :scope "test"]
                  [adzerk/boot-cljs          "1.7.228-2"      :scope "test"]
                  [adzerk/boot-reload        "0.4.13"         :scope "test"]
                  [tailrecursion/boot-static "0.0.1-SNAPSHOT" :scope "test"]
                  [tailrecursion/boot-bucket "0.1.0-SNAPSHOT" :scope "test"]
                  [hoplon/ui                 "0.1.0-SNAPSHOT"]])

(require
  '[adzerk.boot-cljs          :refer [cljs]]
  '[adzerk.boot-reload        :refer [reload]]
  '[hoplon.boot-hoplon        :refer [hoplon]]
  '[tailrecursion.boot-bucket :refer [spew]]
  '[tailrecursion.boot-static :refer [serve]])

(def buckets
  {:xotic "xoticpcgui"})

(deftask build*
  [o optimizations OPM kw "Optimizations to pass the cljs compiler."]
  (let [o (or optimizations :advanced)]
    (comp (speak) (hoplon) (cljs :optimizations o :compiler-options {:language-in :ecmascript5-strict :elide-asserts true}) (sift))))

(deftask develop
  [e elide-asserts bool   "Exclude validations from build"
   o optimizations OPM kw "Optimizations to pass the cljs compiler."]
  (let [o (or optimizations :none)]
    (comp (watch) (speak) (hoplon) (reload) (cljs :optimizations o :compiler-options {:language-in :ecmascript5-strict :elide-asserts elide-asserts}) (serve))))

(deftask build
  "Build the application with advanced optimizations then dump it into the tgt folder."
  [o optimizations OPM kw "Optimizations to pass the cljs compiler."]
  (comp (build* :optimizations optimizations) (target :dir #{"tgt"})))

(deftask deploy
  "Build the application with advanced optimizations then deploy it to s3."
  [e environment   ENV kw "The application environment to be utilized by the service."
   o optimizations OPM kw "Optimizations to pass the cljs compiler."]
  (assert environment "Missing required environment argument.")
  (let [b (buckets environment)]
    (comp (build* :optimizations optimizations) (spew :bucket b))))

(task-options!
  serve {:port 7000}
  sift  {:include #{#"index.html.out/" #"guardian/"} :invert true}
  spew  {:access-key (System/getenv "ROOT_JBOG_AWS_ACCESS_KEY")
         :secret-key (System/getenv "ROOT_JBOG_AWS_SECRET_KEY")})
