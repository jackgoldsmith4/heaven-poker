(ns heaven-poker.view
  (:use [hiccup.page :only (html5 include-css include-js)]))

(defn home []
  (html5 {:lang "en"}
         [:head
          [:title "Test"]
          (include-css "https://dl.dropbox.com/s/sfjndyokk97x00x/hogstyle.css")
          [:body
           [:div {:class "container"}
            [:h1 {:class "info"} "Heaven Poker"]
            ]
           ]
          ]
         )
  )