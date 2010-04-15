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

;; LABELS
(deftype Label [id])

(defn label
  ([] (Label (swap! *num* inc)))
  ([str] (Label str)))
