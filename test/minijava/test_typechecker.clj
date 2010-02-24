(ns minijava.test-typechecker
  (:use clojure.test)
  (:require (minijava [typechecker :as tc]))
  (:import java.io.File))

(deftest test-that-samples-type
  (let [filter (proxy [java.io.FilenameFilter] []
                 (accept [_ name] (not (nil? (re-find #"java$" name)))))
        files (-> "resources/sample" File. (.listFiles filter))]
    (is (every? tc/types? files) "Sample files pass typechecker")))
