(ns minijava.temp)

(def *num* (atom 0))

(defn reset-num! []
  (reset! *num* 0))

;; TEMPS
(defrecord Temp [id]
  Object
  (toString [x] (str id)))

(defn temp
  ([] (Temp. (swap! *num* inc)))
  ([id] (Temp. id)))

;; LABELS
(defrecord Label [id isMethod]
  Object
  (toString [x] (str (if (number? id) (str "lbl_" id) id))))

(defn label
  ([] (Label. (swap! *num* inc) false))
  ([str] (Label. str false))
  ([str isMethod] (Label. str isMethod)))
