(ns minijava.test-compile
  (:use clojure.test
        clojure.contrib.def
        clojure.contrib.pprint
        (minijava compile)))

(comment
(deftest tests-files-convert-without-exception
  (let [filter (proxy [java.io.FilenameFilter] []
                 (accept [_ name] (not (nil? (re-find #"java$" name)))))
        files (-> "resources/sample" java.io.File. (.listFiles filter))]
    (doseq [f files]
      (pprint (compile-program f))))))

(deftest test-factorial
	(let [prog 

"class Factorial{
	public static void main(String[] a){
		System.out.println(new Fac().ComputeFac(10));
	}
}

class Fac {

	public int ComputeFac(int num){
		int num_aux ;
		if (num < 1)
			num_aux = 1 ;
		else 
			num_aux = num * (this.ComputeFac(num-1)) ;
		return num_aux ;
	}

}" ]

 (pprint (compile-program prog))

))
