(ns ownership-fractals.input-cleaner
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(defn- read-csv-from
  [file]
  (with-open [in-file (io/reader file)]
    (doall
     (csv/read-csv in-file))))

(defn- cloc-report->file-set
  [report]
  (->>
   report
   (map second)
   (into #{})))

(defn- remove-dead-files
  [all-files existing]
  (remove #(existing (first %)) all-files))

(defn strip-dead-files-from
  [ownership-file cloc-report]
  (let [ownership (read-csv-from ownership-file)
        existing-files (cloc-report->file-set (read-csv-from cloc-report))
        striped-ownership (remove-dead-files ownership existing-files)]
    (csv/write-csv *out* striped-ownership)))

(defn strip-dead-files-to-outfile
  [ownership-file cloc-report outfile-name]
  (with-open [out-file (io/writer outfile-name)]
    (binding [*out* out-file]
      (strip-dead-files-from ownership-file cloc-report))))
   
