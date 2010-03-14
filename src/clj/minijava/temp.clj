(ns minijava.temp)

(def *num* (atom 0))

(defn reset-num! []
  (reset! *num* 0))

;; TEMPS
(defprotocol Colorable
  (setColor [this]))

(deftype Temp [id color]
  Colorable
  (setColor [] nil)) ;; TODO

(defn temp
  ([] (Temp (swap! *num* inc) nil))
  ([id] (Temp id nil)))

;; LABELS
(deftype Label [id])

(defn label
  ([] (Label (swap! *num* inc)))
  ([str] (Label str)))
