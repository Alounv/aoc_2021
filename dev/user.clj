(ns user
  (:require [nextjournal.clerk :as clerk]))

(defn start! []
  (clerk/serve! {:browse? true
                 :watch-paths ["notebooks"]}))

(comment
  (start!)
  (clerk/halt!))
