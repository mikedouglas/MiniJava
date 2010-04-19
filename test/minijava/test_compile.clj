(ns minijava.test-compile
  (:use clojure.test
        clojure.contrib.def
        (minijava compile)))

(deftest tests-files-convert-without-exception
  (let [filter (proxy [java.io.FilenameFilter] []
                 (accept [_ name] (not (nil? (re-find #"java$" name)))))
        files (-> "resources/sample" java.io.File. (.listFiles filter))]
    (doseq [f files]
      (compile-program f))))
