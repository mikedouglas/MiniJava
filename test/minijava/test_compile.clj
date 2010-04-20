(ns minijava.test-compile
  (:use clojure.test
        clojure.contrib.def
        clojure.contrib.pprint
        (minijava typechecker compile)))

(deftest tests-files-convert-without-exception
  (let [filter (proxy [java.io.FilenameFilter] []
                 (accept [_ name] (not (nil? (re-find #"java$" name)))))
        files (-> "resources/sample" java.io.File. (.listFiles filter))]
    (doseq [f files]
    (println (compile-program f)))))


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

 (println (compile-program prog))

))


(deftest test-ls
	(let [prog 
"class LinearSearch{
	public static void main(String[] a){
		System.out.println(new LS().Start(10));
	}
}


// This class contains an array of integers and
// methods to initialize, print and search the array
// using Linear Search
class LS {
	int[] number ;
	int size ;

	// Invoke methods to initialize, print and search
	// for elements on the array
	public int Start(int sz){
		int aux01 ;
		int aux02 ;

		aux01 = this.Init(sz);
		aux02 = this.Print();
		System.out.println(9999);
		System.out.println(this.Search(8));
		System.out.println(this.Search(12)) ;
		System.out.println(this.Search(17)) ;
		System.out.println(this.Search(50)) ;
		return 55 ;
	}

	// Print array of integers
	public int Print(){
		int j ;

		j = 1 ;
		while (j < (size)) {
			System.out.println(number[j]);
			j = j + 1 ;
		}
		return 0 ;
	}

	// Search for a specific value (num) using
	// linear search
	public int Search(int num){
		int j ;
		boolean ls01 ;
		int ifound ;
		int aux01 ;
		int aux02 ;
		int nt ;

		j = 1 ;
		ls01 = false ;
		ifound = 0 ;

		//System.out.println(num);
		while (j < (size)) {
			aux01 = number[j] ;
			aux02 = num + 1 ;
			if (aux01 < num) nt = 0 ;
			else if (!(aux01 < aux02)) nt = 0 ;
			else {
				ls01 = true ;
				ifound = 1 ;
				j = size ;
			}
			j = j + 1 ;
		}

		return ifound ;
	}



	// initialize array of integers with some
	// some sequence
	public int Init(int sz){
		int j ;
		int k ;
		int aux01 ;
		int aux02 ;

		size = sz ;
		number = new int[sz] ;

		j = 1 ;
		k = size + 1 ;
		while (j < (size)) {
			aux01 = 2 * j ;
			aux02 = k - 3 ;
			number[j] = aux01 + aux02 ;
			j = j + 1 ;
			k = k - 1 ;
		}
		return 0 ;	
	}

}"]

 (println (compile-program prog))

))


