(set-env!
  :asset-paths  #{"rsc"}
  :source-paths #{"src"}
  :dependencies '[[org.clojure/clojure       "1.8.0"          :scope "test"]
                  [org.clojure/clojurescript "1.8.51"        :scope "test"]
                  [adzerk/boot-cljs          "1.7.228-1"      :scope "test"]
                  [adzerk/boot-reload        "0.4.12"         :scope "test"]
                  [tailrecursion/boot-static "0.0.1-SNAPSHOT" :scope "test"]
                  [tailrecursion/boot-bucket "0.1.0-SNAPSHOT" :scope "test"]
                  [adzerk/env                "0.3.0"]
                  [hoplon/ui                 "0.1.0-SNAPSHOT"]])
(require
  '[adzerk.boot-cljs          :refer [cljs]]
  '[adzerk.boot-reload        :refer [reload]]
  '[hoplon.boot-hoplon        :refer [hoplon]]
  '[tailrecursion.boot-bucket :refer [spew]]
  '[tailrecursion.boot-static :refer [serve]])

(def buckets
  {:xotic "xoticpcgui"})

(deftask develop
  [o optimizations OPM kw "Optimizations to pass the cljs compiler."]
  (let [o (or optimizations :none)]
    (comp (watch) (speak) (hoplon) (reload) (cljs :optimizations o) (serve))))

(deftask build
  [o optimizations OPM kw "Optimizations to pass the cljs compiler."]
  (let [o (or optimizations :advanced)]
    (comp (speak) (hoplon) (cljs :optimizations o :compiler-options {:elide-asserts true :language-in :ecmascript5-strict}) (sift))))

(deftask deploy
  "Build the application with advanced optimizations then deploy it to s3."
  [e environment   ENV kw "The application environment to be utilized by the service."
   o optimizations OPM kw "Optimizations to pass the cljs copmiler."]
  (assert environment "Missing required environment argument.")
  (let [b (buckets environment)]
    (comp (build :optimizations optimizations) (spew :bucket b))))

(task-options!
  serve   {:port 7000}
  sift    {:include #{#"index.html.out/" #"guardian/"} :invert true}
  spew    {:access-key (System/getenv "ROOT_JBOG_AWS_ACCESS_KEY")
           :secret-key (System/getenv "ROOT_JBOG_AWS_SECRET_KEY")})

(deftask cordova
  [p platform OPT str  "target operating system"
   r run          bool "run after building"]
  (comp (target :dir ["bld/www"])
        (with-pre-wrap fileset
          (binding [*sh-dir* "bld"]
            (dosh "cordova" "requirements")
            (dosh "cordova" (if run "run" "build") platform))
          fileset)))

(deftask develop
  [o optimizations OPT kw  "Optimizations to pass the cljs compiler."
   p platform      OPT str "Platform to use during development"]
  (let [opt (or optimizations :none)
        run (if (and platform (not= platform "browser")) #(cordova :platform platform :run true) serve)]
    (comp (watch) (speak) (hoplon) (reload) (cljs :optimizations opt) (run))))

(deftask build
  [p platform OPT str "target operating system"]
  (comp (speak) (hoplon) (cljs :optimizations :whitespace) (cordova :platform platform)))

