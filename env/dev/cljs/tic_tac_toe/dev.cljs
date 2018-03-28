(ns ^:figwheel-no-load tic-tac-toe.dev
  (:require
    [tic-tac-toe.core :as core]
    [devtools.core :as devtools]))


(enable-console-print!)

(devtools/install!)

(core/init!)
