(ns guardian.dashboard.service.data)

(def machine
  {:name    "MSI Gaming Series G Laptop"
   :devices [{:name "Micro-Star International Co., Ltd. MS-16H8"
              :type :mb
              :zones [{:name  "CPU"
                       :temps #queue[{:sensor "THRM" :value 30   :time #inst"2017-01-26T00:00:00"}
                                     {:sensor "THRM" :value 29   :time #inst"2017-01-26T00:00:01"}]}
                      {:name  "North Bridge"
                       :temps #queue[{:sensor "TZ00" :value 27.7 :time #inst"2017-01-26T00:00:00"}
                                     {:sensor "TZ00" :value 27.8 :time #inst"2017-01-26T00:00:01"}]}
                      {:name  "South Bridge"
                       :temps #queue[{:sensor "TZ01" :value 29.8 :time #inst"2017-01-26T00:00:00"}
                                     {:sensor "TZ01" :value 29.8 :time #inst"2017-01-26T00:00:01"}]}]}
             {:name "Intel Core i7 6700HQ"
              :type  :cpu
              :temps #queue[{:sensor "Package" :value 35   :time #inst"2017-01-26T00:00:00"}
                            {:sensor "Package" :value 35   :time #inst"2017-01-26T00:00:01"}]
              :loads #queue[{:sensor "UC"      :value 2.73 :time #inst"2017-01-26T00:00:00"}
                            {:sensor "UC"      :value 2.73 :time #inst"2017-01-26T00:00:00"}]
              :cores [{:name    "Core 1"
                       :freqs   #queue[{:sensor "Core #0" :value 3292 :time #inst"2017-01-26T00:00:00"}
                                       {:sensor "Core #0" :value 1596 :time #inst"2017-01-26T00:00:01"}]
                       :temps   #queue[{:sensor "Core #0" :value 33   :time #inst"2017-01-26T00:00:00"}
                                       {:sensor "Core #0" :value 33   :time #inst"2017-01-26T00:00:01"}]
                       :threads [{:name  "Thread 1"
                                  :loads #queue[{:sensor "CPU #0" :value 15.38 :time #inst"2017-01-26T00:00:00"}
                                                {:sensor "CPU #0" :value 15.38 :time #inst"2017-01-26T00:00:01"}]}
                                 {:name  "Thread 2"
                                  :loads #queue[{:sensor "CPU #1" :value 0     :time #inst"2017-01-26T00:00:00"}
                                                {:sensor "CPU #1" :value 0     :time #inst"2017-01-26T00:00:01"}]}]}
                      {:name    "Core 2"
                       :freqs   #queue[{:sensor "Core #1" :value 3292 :time #inst"2017-01-26T00:00:00"}
                                       {:sensor "Core #1" :value 1596 :time #inst"2017-01-26T00:00:01"}]
                       :temps   #queue[{:sensor "Core #1" :value 35   :time #inst"2017-01-26T00:00:00"}
                                       {:sensor "Core #1" :value 35   :time #inst"2017-01-26T00:00:01"}]
                       :threads [{:name  "Thread 3"
                                  :loads #queue[{:sensor "CPU #2" :value 0     :time #inst"2017-01-26T00:00:00"}
                                                {:sensor "CPU #2" :value 0     :time #inst"2017-01-26T00:00:01"}]}
                                 {:name  "Thread 4"
                                  :loads #queue[{:sensor "CPU #3" :value 0     :time #inst"2017-01-26T00:00:00"}
                                                {:sensor "CPU #3" :value 0     :time #inst"2017-01-26T00:00:01"}]}]}
                      {:name    "Core 3"
                       :freqs   #queue[{:sensor "Core #2" :value 3292 :time #inst"2017-01-26T00:00:00"}
                                       {:sensor "Core #2" :value 1596 :time #inst"2017-01-26T00:00:01"}]
                       :temps   #queue[{:sensor "Core #2" :value 33   :time #inst"2017-01-26T00:00:00"}
                                       {:sensor "Core #2" :value 33   :time #inst"2017-01-26T00:00:01"}]
                       :threads [{:name  "Thread 5"
                                  :loads #queue[{:sensor "CPU #4" :value 1.54  :time #inst"2017-01-26T00:00:00"}
                                                {:sensor "CPU #4" :value 1.54  :time #inst"2017-01-26T00:00:01"}]}
                                 {:name  "Thread 6"
                                  :loads #queue[{:sensor "CPU #5" :value 0     :time #inst"2017-01-26T00:00:00"}
                                                {:sensor "CPU #5" :value 0     :time #inst"2017-01-26T00:00:01"}]}]}
                      {:name    "Core 4"
                       :freqs   #queue[{:sensor "Core #3" :value 3292 :time #inst"2017-01-26T00:00:00"}
                                       {:sensor "Core #3" :value 1596 :time #inst"2017-01-26T00:00:01"}]
                       :temps   #queue[{:sensor "Core #3" :value 34   :time #inst"2017-01-26T00:00:00"}
                                       {:sensor "Core #3" :value 34   :time #inst"2017-01-26T00:00:01"}]
                       :threads [{:name  "Thread 7"
                                  :loads #queue[{:sensor "CPU #6" :value 0     :time #inst"2017-01-26T00:00:00"}
                                                {:sensor "CPU #6" :value 0     :time #inst"2017-01-26T00:00:01"}]}
                                 {:name  "Thread 8"
                                  :loads #queue[{:sensor "CPU #7" :value 0     :time #inst"2017-01-26T00:00:00"}
                                                {:sensor "CPU #7" :value 0     :time #inst"2017-01-26T00:00:01"}]}]}
              :pins [{:name  "VID"
                      :volts #queue[{:sensor "VID"                 :value 0.84 :time #inst"2017-01-26T00:00:00"}
                                    {:sensor "VID"                 :value 0.84 :time #inst"2017-01-26T00:00:01"}]}
                     {:name  "IA Offset"
                      :volts #queue[{:sensor "GT Offset"           :value 0.84 :time #inst"2017-01-26T00:00:00"}
                                    {:sensor "GT Offset"           :value 0.84 :time #inst"2017-01-26T00:00:01"}]}
                     {:name  "GT Offset"
                      :volts #queue[{:sensor "GT Offset"           :value 0.84 :time #inst"2017-01-26T00:00:00"}
                                    {:sensor "GT Offset"           :value 0.84 :time #inst"2017-01-26T00:00:01"}]}
                     {:name  "LLC/Ring Offset"
                      :volts #queue[{:sensor "LLC/Ring Offset"     :value 0.84 :time #inst"2017-01-26T00:00:00"}
                                    {:sensor "LLC/Ring Offset"     :value 0.84 :time #inst"2017-01-26T00:00:01"}]}
                     {:name  "System Agent Offset"
                      :volts #queue[{:sensor "System Agent Offset" :value 0.84 :time #inst"2017-01-26T00:00:00"}
                                    {:sensor "System Agent Offset" :value 0.84 :time #inst"2017-01-26T00:00:01"}]}]]}]})
