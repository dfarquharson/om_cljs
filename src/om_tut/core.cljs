(ns tree-editor.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [clojure.zip :as zip]
            [clojure.string :as string]
            [cljs.core.async :as async :refer [put! chan <! >! close! timeout]]
            [cljs-http.client :as http]
            [clojure.browser.repl :as repl]
            [goog.events :as events])
  (:import [goog.events EventType]))

(enable-console-print!)

(def app-state
  (atom {:repos ["loading repos..."]
         :response "nothing here yet"
         :windows [{:tiles [{:content "Tile 1"}
                            {:content "Tile 2"}
                            {:content "Tile 3"}
                            {:content "Tile 4"}]}
                    ;{:tiles [{:content "Tile 1"}
                             ;{:content "Tile 2"}
                             ;{:content "Tile 3"}]}
                    ;{:tiles [{}]}
                   ]
         :map {:input
               {:children [{:atts
                            {:fullyQualifiedJavaName "No Input Tree Selected"}}]}
               :output
               {:children [{:atts
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
;(test-get-post-delete)

(defn update-repo-list []
  (go
    (let [repo-list (<! (GET repo-list-url))]
      (prn (:repos (:body repo-list)))
      (swap! app-state assoc :repos (:repos (:body repo-list))))))
; uncomment for 'real' use
;(update-repo-list)

(defn load-map [path]
  (let [loading-map {:input {:children [{:atts {:fullyQualifiedJavaName "Loading input..."}}]}
                     :output {:children [{:atts {:fullyQualifiedJavaName "Loading output..."}}]}}]
    ;(-> app-state :windows first :tiles first :content prn)
    (-> loading-map :input prn)
    ;(om/transact! app-state 
    ;(swap! app-state assoc (-> :windows first :tiles first :content loading-map :input))
    ;(swap! app-state assoc (-> :windows first :tiles second :content loading-map :output)))
    (swap! app-state assoc :map loading-map))
  (go
    (let [map-path (map-url path)
          map-resp (<! (GET map-path))]
      (prn (str map-path ", " (:status map-resp)))
      (swap! app-state assoc :map (:body map-resp)))))
; just for testing because I'm too lazy to click the button all the time
;(load-map "examples/master/attributed_xml/poCustWrite.xtl")

(comment
(om/root
  (fn [app owner]
    (om/component
      (dom/h2 nil "Tree Editor")))
  app-state
  {:target (. js/document (getElementById "header"))})
  )

(defn handle-change [e owner {:keys [text]}]
  (om/set-state! owner :text (.. e -target -value)))
;; this way is better because it doesn't require extending strings ICloneable
;(defn handle-change [e data edit-key owner]
  ;(om/transact! data edit-key (fn [_] (.. e -target -value))))

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

; higher-order editable component and helpers
(comment
(extend-type string
  ICloneable
  (-clone [s] (js/String. s)))
(extend-type js/String
  ICloneable
  (-clone [s] (js/String. s))
  om/IValue
  (-value [s] (str s)))
  )

(defn display [show]
  (if show
    #js {}
    #js {:display "none"}))

(defn commit-change [text owner]
  (om/set-state! owner :editing false))

(defn editable [text owner]
  (reify
    om/IInitState
    (init-state [_]
      {:editing false})
    om/IRenderState
    (render-state [_ {:keys [editing]}]
      (dom/li #js {:onClick #(om/set-state! owner :editing true)}
              (dom/span #js {:style (display (not editing))} (om/value text))
              (dom/input
                #js {:style (display editing)
                     :value (om/value text)
                     :onChange #(handle-change % text owner)
                     :onKeyDown #(when (= (.-key  %) "Enter")
                                   (commit-change text owner))
                     :onBlur (fn [e] (commit-change text owner))})
              ))))
              ;(dom/button
                ;#js {:style (display (not editing))
                     ;:onClick #(om/set-state! owner :editing true)}
                ;"Edit")))))

; tree view from here down
(defn get-docdef-atts [xtl]
  (-> xtl :children first :atts))

(defn att-component [[k v] owner]
  (om/component
    (dom/li #js {:className "att"}
            (str (name k) ": " v))))

(defn atts-component [atts owner]
  (reify
    om/IInitState
    (init-state [_]
      {:hidden false})
    om/IRenderState
    (render-state [_ {:keys [hidden]}]
      (dom/div #js {:style  #js {:border "1px solid black"}
                    :className "atts"}
               (apply dom/ul nil (om/build-all att-component (seq atts)))))))
               ;(apply dom/ul nil (om/build-all editable (att-component (seq atts) atts)))))))
               ;(apply dom/ul nil (om/build-all editable (vals atts)))))))

(defn node-component [node owner]
  (reify
    om/IRender
    (render [_]
      (dom/div nil
               (dom/ul #js {:style #js {:border "1px solid black"}
                            :className "node"}
                       (dom/strong nil (-> node :atts :javaName str))
                       (dom/li nil (str "tag: " (:name node)))
                       (dom/li nil (str "text: " (om/value (:text node))))
                       (dom/li nil (str "atts: "))
                       (om/build atts-component (:atts node))
                       (dom/li nil (str "children: " (-> node :children count)))
                       (apply dom/ul nil
                              (om/build-all node-component (:children node))))))))

(defn xtl-component [xtl owner]
  (om/component
    (dom/div #js {:id (-> xtl get-docdef-atts :fullyQualifiedJavaName)
                  :className "xtl"
                  :style #js {:border "1px solid black"
                              :float "left"
                              :width "40%"
                              :overflow "hidden"
                              :margin "50px"
                              }}
             (dom/h3 nil (str "xtl name: " (-> xtl get-docdef-atts :fullyQualifiedJavaName)))
             (om/build node-component xtl))))

(defn mapping-area [app owner]
  (reify
    om/IRender
    (render [_]
      (dom/div #js {:id "map-area"}
               (let [input (-> app :map :input)
                     output (-> app :map :output)]
                 (dom/ul nil
                         (om/build xtl-component input)
                         (om/build xtl-component output)))))))

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
               (om/build mapping-area app)))))

;(om/root map-view app-state
  ;{:target (. js/document (getElementById "map-workspace"))})

;; input listener area keyboard/mouse

(defn listen [el type]
  (let [out (chan)]
    (events/listen el type #(put! out %))
    out))

(defn mouse-view [app owner]
  (reify
    om/IWillMount
    (will-mount [_]
      (let [mouse-chan
            (async/map
              (fn [e] [(.-clientX e) (.-clientY e)])
              [(listen js/window EventType/MOUSEMOVE)])]
        (go (while true
              (om/update! app :mouse (<! mouse-chan))))))
    om/IRender
    (render [_]
      (dom/p nil
             (when-let [pos (:mouse app)]
               (pr-str (:mouse app)))))))

(defn keyboard-view [app owner]
  (reify
    om/IWillMount
    (will-mount [_]
      (let [key-chan
            (async/map
              (fn [e] [(.-keycode e)])
              [(listen js/window EventType/KEYUP)])]
        (go (while true
              (om/update! app :key (<! key-chan))))))
    om/IRender
    (render [_]
      (dom/p nil
             (when-let [key (:key app)]
               (pr-str (:key app)))))))

(defn input-listener [app owner]
  (om/component
    (dom/div nil
             (om/build mouse-view app)
             (om/build keyboard-view app))))

(om/root input-listener {:mouse nil :key nil}
  {:target (. js/document (getElementById "input-area"))})

(defn tile-component [tile owner]
  (reify
    om/IRender
    (render [_]
      (prn tile)
      (dom/div #js {:className "tile"
                    :onmouseover "style.color='black'"
                    :onmouseout "style.color='white'"}
               (dom/p nil (:content tile))))))

(defn tile-group-component [window owner]
  (reify
    om/IRender
    (render [_]
      (dom/div nil
               (dom/h2 #js {:className "tile-group"} "Tile Group")
               (-> window :tiles prn)
               (apply dom/div nil
                      (om/build-all tile-component (:tiles window)))))))

(defn tabbed-tile-group-container [app owner]
  (reify
    om/IRender
    (render [_]
      (dom/div nil
               ;(dom/h2 nil "The Tabbed Holder of Tile Groups")
               (apply dom/div nil
                      (om/build-all tile-group-component (:windows app)))))))

(om/root tabbed-tile-group-container app-state
  {:target (. js/document (getElementById "tiling-area"))})

; should be focused with C-x C-m
; users can bind commands to the keyboard shortcuts of their choice from here
; for instance, should be able to use a color-picker to set background color
(defn mini-buffer [app owner]
  (reify
    om/IInitState
    (init-state [_]
      {:text nil})
    om/IRenderState
    (render-state [_ state]
      (dom/div
        #js {:style #js {:clear "both"
                         :width "100%"}}
        (dom/input {:id "mini-buffer" :type "text" :value (:text state)})
        (dom/label nil "<-- minibuffer")))))

;(om/root mini-buffer app-state
  ;{:target (. js/document (getElementById "mini-buffer"))})
