(ns minijava.test-alloc
  (:use (minijava alloc liveness gas temp)
        clojure.test))

(deftest tests-no-spilled-no-dead
  (let [val [{:start 0, :end 3}
             {:start 0, :end 4}
             {:start 1, :end 3}
             {:start 1, :end 7}
             {:start 2, :end 6}
             {:start 2, :end 5}]
        inreg (scan val)
        spilled (filter #(= (:reg %) :spilled) inreg)
        inreg (remove #(= (:reg %) :spilled) inreg)]
    (is (= (count val) (count inreg)) "Every interval is allocated to reg.")
    (is (distinct? (map :reg inreg)) "Interval is given a unique register.")
    (is (empty? spilled) "Nothing is spilled.")))

(defn find-dup-in [feat s]
  (loop [[f & rest] (sort-by :reg s)]
    (when (seq rest)
      (if (= (feat f) (feat (first rest)))
        [f (first rest)]
        (recur rest)))))

(deftest tests-dead-no-spilled
  (let [val [{:start 0, :end 1}
             {:start 0, :end 2}
             {:start 1, :end 3}
             {:start 2, :end 3}]
        inreg (scan val)
        spilled (filter #(= (:reg %) :spilled) inreg)
        inreg (remove #(= (:reg %) :spilled) inreg)
        doubled (find-dup-in :reg inreg)]
    (is (= (count val) (count inreg)) "Every interval is allocated to reg.")
    (is (not (nil? doubled)) "Registers are not unique.")
    (is (empty? spilled) "Nothing is spilled.")))

(deftest tests-spilled
  (let [val [{:start 0, :end 3}
             {:start 0, :end 2}
             {:start 0, :end 4}
             {:start 1, :end 5}
             {:start 1, :end 6}
             {:start 1, :end 4}
             {:start 2, :end 6}]
        inreg (scan val)
        spilled (filter #(= (:reg %) :spilled) inreg)
        inreg (remove #(= (:reg %) :spilled) inreg)
        longest-lived (first (reverse (sort-by :reg val)))]
    (is (not (= (count val) (count inreg))) "Not every interval in reg.")
    (is (not (empty? spilled)) "Something is spilled.")
    (is (= (dissoc (first spilled) :reg) longest-lived)
        "Longest-lived interval spilled.")))

(deftest pipeline-no-spilled
  (let [l1 (label)
        a (temp :EAX)
        b (temp :EBX)
        c (temp :ECX)
        val (vector
             (LABEL l1)
             (addl (CONST 3) c)
             (addl (CONST 5) b)
             (addl b c)
             (movl c a)
             (jmp l1))
        res (vector
             (LABEL l1)
             (addl (CONST 3) :ECX)
             (addl (CONST 5) :EBX)
             (addl :EBX :ECX)
             (movl :ECX :EAX)
             (jmp l1))]
    (is (= (fill val) res))))

(deftest pipeline-unique
  (let [l1 (label)
        a (temp)
        b (temp)
        c (temp)
        val (vector
             (LABEL l1)
             (addl (CONST 3) c)
             (addl (CONST 5) b)
             (addl b c)
             (movl c a)
             (jmp l1))]
    (is (distinct? (:dst (val 1)) (:dst (val 2)) (:dst (val 4))))))


