(ns minijava.main
  (:use clojure.contrib.io
     clojure.contrib.shell
     (minijava compile))
  (:gen-class))

(defn -main [file]
  (do (println "Compiling to _code.s")
      (with-out-writer (writer "_code.s")
        (print-program-text (compile-program (java.io.File. file))))
      (println "Running GCC...")
      (println (sh "gcc" "_code.s" "./resources/runtime/runtime.s"))))
