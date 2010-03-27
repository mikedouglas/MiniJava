(ns minijava.test-typechecker
  (:use clojure.test
        minijava.typechecker)
  (:import java.io.File))

(deftest test-that-samples-type
  (let [filter (proxy [java.io.FilenameFilter] []
                 (accept [_ name] (not (nil? (re-find #"java$" name)))))
        files (-> "resources/sample" File. (.listFiles filter))]
    (is (every? types? files) "Sample files pass typechecker")))
