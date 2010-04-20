(ns minijava.temp)

(def *num* (atom 0))

(defn reset-num! []
  (reset! *num* 0))

;; TEMPS
(deftype Temp [id]
  Object
  (toString [] (str id)))

(defn temp
  ([] (Temp (swap! *num* inc)))
  ([id] (Temp id)))

(comment
;; LABELS
(deftype Label [id]
  Object
  (toString [] (str id)))
)
(defn digit? [x]
	(Character/isDigit (.charAt (str x) 0))
)

;; LABELS
(deftype Label [id]
  Object
  (toString [] 
(if (digit? id)
(str "lbl" id)
id
)))

(defn label
  ([] (Label (swap! *num* inc)))
  ([str] (Label str)))
