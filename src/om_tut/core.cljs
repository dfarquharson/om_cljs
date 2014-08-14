(ns tree-editor.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [clojure.zip :as zip]
            [clojure.string :as string]
            [cljs.core.async :refer [put! chan <! >! close! timeout]]
            [cljs-http.client :as http]
            [clojure.browser.repl :as repl]))

(enable-console-print!)

(def app-state
  (atom {:repos ["loading repos..."]
         :response "nothing here yet"
         :map {:input
               {:atts
                {:name "Loading Input Tree..."}
                :children [{:atts
                            {:fullyQualifiedJavaName "No Input Tree Selected"}}]}
               :output
               {:atts
                {:name "Loading Output Tree..."}
                :children [{:atts
                            {:fullyQualifiedJavaName "No Output Tree Selected"}}]}}}))

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

(defn DELETE [url]
  (let [c (chan)]
    (go (let [response (<! (http/delete url {:with-credentials? false}))]
          (>! c response)))
    c))

; updating the global state with http requests
; definitely a code-smell. not sure how to avoid it yet.
(defn test-get-post-delete []
  (go
    (let [echo-get (<! (GET echo-url))
          echo-post (<! (POST echo-url {:key "value"}))
          echo-delete (<! (DELETE echo-url))]
      (prn (:body echo-get))
      (prn (:body echo-post))
      (prn (:body echo-delete))
      (swap! app-state assoc :response (:body echo-get)))))
(comment (test-get-post-delete))

(defn update-repo-list []
  (go
    (let [repo-list (<! (GET repo-list-url))]
      (prn (:repos (:body repo-list)))
      (swap! app-state assoc :repos (:repos (:body repo-list))))))
; uncomment for 'real' use
(comment (update-repo-list))

(defn load-map [path]
  (let [loading-map {:input {:children [{:atts {:fullyQualifiedJavaName "Loading input..."}}]}
                     :output {:children [{:atts {:fullyQualifiedJavaName "Loading output..."}}]}}]
    (swap! app-state assoc :map loading-map))
  (go
    (let [map-path (map-url path)
          map-resp (<! (GET map-path))]
      (prn (str map-path ", " (:status map-resp)))
      (swap! app-state assoc :map (:body map-resp)))))
; just for testing because I'm too lazy to click the button all the time
(load-map "examples/master/attributed_xml/poCustWrite.xtl")

(defn handle-change [e owner {:keys [text]}]
  (om/set-state! owner :text (.. e -target -value)))

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
                                     :onChange #(handle-change % owner state)})
                              (let [repo-list-result (:repos app)
                                    matching-repos (take 10 (find-fuzzy-matches repo-list-result (:text state)))]
                                (map #(dom/li nil %)
                                     (cond
                                       (> (count matching-repos) 0) matching-repos
                                       (= (count matching-repos) (count (:text state)) 0) (take 10 (:repos app))
                                       :else '("no matching repos")))))))))
(comment
(om/root search-view app-state
  {:target (. js/document (getElementById "search-area"))})
  )

(defn get-docdef-atts [xtl]
  (-> xtl :children first :atts))

(defn att-component [[k v] owner]
  (om/component
    (dom/li nil (str (name k) ": " v))))

(defn atts-component [atts owner]
  (om/component
    (apply dom/ul nil (om/build-all att-component (seq atts)))))

(defn node-component [node owner]
  (om/component
    (dom/ul nil
            (dom/li nil (str "tag: " (:name node)))
            (dom/li nil (str "text: " (:text node)))
            (dom/li nil (str "atts: "))
            (om/build atts-component (:atts node))
            (dom/li nil (str "children: " (-> node :children count)))
            (apply dom/ul nil
                   (om/build-all node-component (:children node))))))

(defn xtl-component [xtl owner]
  (om/component
    (dom/div nil
             (dom/h3 nil (str "xtl name: " (-> xtl get-docdef-atts :fullyQualifiedJavaName))))))

(defn map-view [app owner]
  (reify
    om/IInitState
    (init-state [_]
      {:text "examples/master/attributed_xml/poCustWrite.xtl"})
    om/IRenderState
    (render-state [_ state]
      (dom/div #js {:id "map-workspace"}
               (dom/h2 nil "Map Workspace")
               (dom/input
                 #js {:type "text" :ref "map-path" :value (:text state) :size "40"
                      :onChange #(handle-change % owner state)})
               (dom/button
                 #js {:onClick #(load-map (:text state))} "Load Map")
               (let [input (-> app :map :input)
                     output (-> app :map :output)
                     fqjn :fullyQualifiedJavaName]
                 (dom/ul nil
                         (om/build xtl-component input)
                         (om/build node-component (-> input))
                         (om/build xtl-component output)
                         (om/build node-component (-> output))))))))

(om/root map-view app-state
  {:target (. js/document (getElementById "map-workspace"))})

(comment
(om/root
  (fn [app owner]
    (om/component
      (dom/h2 nil "Tree Editor")))
  app-state
  {:target (. js/document (getElementById "header"))})
  )
