(ns tree-editor.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [clojure.string :as string]
            [cljs.core.async :refer [put! chan <! >! close! timeout]]
            [cljs-http.client :as http]))

(enable-console-print!)

(def app-state
  (atom {:repos []
         :response "nothing here yet"
         :map {:input
               {:atts
                {:name "No Input Tree Loaded"}}
               :output
               {:atts
                {:name "No Output Tree Loaded"}}}}))

(def xdapi "http://localhost:5000/xd/")
(def echo-url (str xdapi "echo"))
(def repo-list-url (str xdapi "list"))
(defn map-url [path] (str xdapi "map/" path))

(defn GET [url]
  (let [c (chan)]
    (go (let [response (<! (http/get url {:with-credentials? false}))]
          (>! c response)))
    c))

(defn POST [url payload]
  (let [c (chan)]
    (go (let [response (<! (http/post url (conj {:with-credentials? false}
                                                {:json-params payload})))]
          (>! c response)))
    c))

(go
  (let [echo-resp (<! (GET echo-url))
        echo-post (<! (POST echo-url {:key "value"}))
        list-resp (<! (GET repo-list-url))
        map-resp (<! (GET (map-url "examples/master/attributed_xml/poCustWrite.xtl")))]
    (prn (:repos (:body list-resp)))
    (prn (:body map-resp))
    (prn (:body echo-post))
    (swap! app-state assoc :response (:body echo-resp))
    (swap! app-state assoc :repos (:repos (:body list-resp)))
    (swap! app-state assoc :map (:body map-resp))))

(defn handle-change [e owner {:keys [text]}]
  (om/set-state! owner :text (.. e -target -value)))

(defn find-exact-matches [xs x]
  (filter #(if (nil? x) true
             (> (.indexOf % x) -1)) xs))

(defn find-fuzzy-matches [coll query]
  (let [contains-all (fn [y xs]
                       (reduce #(and %1 %2) (for [x xs] (.contains y x))))
        words (if (nil? query) "" (string/split query #"\s+"))]
    (filter #(contains-all (string/lower-case %) words) coll)))

(defn search-view [app owner]
  (reify
    om/IInitState
    (init-state [_]
      {:text nil})
    om/IRenderState
    (render-state [_ state]
      (dom/div nil
               (apply dom/ul #js {:className "repo-list"}
                              (dom/input
                                #js {:type "text" :ref "repo-list" :value (:text state)
                                     :onChange (fn [event] (handle-change event owner state))})
                              (let [repo-list-result (:repos app)
                                    matching-repos (take 10 (find-fuzzy-matches repo-list-result (:text state)))]
                                (map #(dom/li nil %)
                                     (cond
                                       (> (count matching-repos) 0) matching-repos
                                       (= (count matching-repos) (count (:text state)) 0) (take 10 (:repos app))
                                       :else '("no matching repos")))))))))

(om/root 
  search-view
  app-state
  {:target (. js/document (getElementById "search-area"))})

(defn map-view [app owner]
  (reify
    om/IRender
    (render [_]
      (dom/p nil (:name (:atts (:input (:map app))))))))

(om/root map-view app-state
  {:target (. js/document (getElementById "map-workspace"))})

(om/root
  (fn [app owner]
    (om/component
      (dom/h2 nil "Tree Editor")))
  app-state
  {:target (. js/document (getElementById "header"))})
