(ns drop.core
  (:require [om.core :as om :include-macros true]
            [cljs.reader :as reader]
            [sablono.core :as html :refer-macros [html]]))

(defn random-color
  []
  (apply str "#" (map #(.toString % 16) (take 3 (repeatedly #(rand-int 16))))))

(defn random-ball
  []
  {:p [(+ 50 (rand-int 500)) (+ 50 (rand-int 500))]
   :v [(- 500 (rand-int 1000)) (- 500 (rand-int 1000))]
   :color (random-color)
   :r (rand-int 30)
   :bouncy (+ 0.5 (/ (rand) 2)) }
  )

(defonce app-state (atom {:text "Bouncyness!"
                          :time 0
                          :gravity 980
                          :balls #_[{:v [500 0]
                                   :p [100 100]
                                   :r 10
                                   :color "#ff0000"
                                   :bouncy 0.63}
                                  {:v [-500 0]
                                   :p [200 95]
                                   :r 10
                                   :color "#00ff00"
                                   :bouncy 0.63}
                                  ]
                          (into [] (take 10 (repeatedly random-ball)))
                          #_[{:p [60 50]
                                   :v [600 0]
                                   :r 20
                                   :color "#00ff00"
                                   :bouncy 0.63}]
                          }))

(defn v+
  [[xa ya] [xb yb]]
  [(+ xa xb) (+ ya yb)])

(defn v*
  [[x y] s]
  [(* s x) (* s y)])

(defn v-
  ([[x y]]
   [(- x) (- y)])
  ([[xa ya] [xb yb]]
   [(- xa xb) (- ya yb)]))

(defn v-create
  [[xa ya] [xb yb]]
  [(- xb xa) (- yb ya)])

(def sqrt (.-sqrt js/Math))

(defn magnitude
  [[x y]]
  (sqrt (+ (* x x) (* y y))))

(defn normalize
  [[x y]]
  (let [m (magnitude [x y])]
    [(/ x m) (/ y m)]))

(defn dot
  [[x1 y1] [x2 y2]]
  (+ (* x1 x2) (* y1 y2)))

(defn dist
  [[x1 y1] [x2 y2]]
  (let [dx (- x2 x1)
        dy (- y2 y1)]
    (sqrt (+ (* dx dx) (* dy dy)))))

(defn project
  [a b]
  (let [bn (normalize b)]
    (v* bn (dot a bn))))

(defn component
  "component of a onto b (scalar)"
  [a b]
  (let [bn (normalize b)]
    (dot a bn)))

(comment
 (v+ [1 1] [1 1])
 (v* [2 2] 2)
 (v- [2 2])
 (v* v (* 0.010 980))
 (- 550 20 1)
 (sqrt 4)
 (dist [0 1] [1 1])
 (dist [0 0] [4 3])

 (dist [0 10] [0 20])

 (magnitude [3 4])

 (normalize [1 1])

 (project [1 1] [2 0])

 (project [0 2] [2 2])

 (project [4 0] [8 0])

 (normalize [8 0])

 (dot [4 0] [1 0])
 (dot [0 4] [4 0])
 (dot [0 4] [0 4])

 (v-create [0 4] [4 0])
 )

(defn accelerate-g
  [{:keys [v] :as o} gravity delta-t]
  (assoc o
         :v (v+ v [0 (* delta-t gravity)])))

(defn update-position
  [{:keys [v p] :as o} delta-t]
  (assoc o
         :p (v+ p (v* v delta-t))))

(defn collide-ground
  [{:keys [v p r bouncy] :as o} ground]
  (assoc o
         :v (if (and (> (v 1) 0) (>= (+ r (p 1)) ground))
              [(first v) (* bouncy (- (second v)))] 
              v)
         :p (if (>= (+ r (p 1)) ground)
              [(first p) (- ground r 1)]
              p)
         ))

(defn collide-left-wall
  [{:keys [v p r bouncy] :as o} wall]
  (assoc o
         :v (if (and (< (v 0) 0) (<= (- (p 0) r) wall))
              [(* bouncy (- (first v))) (second v)]
              v)))

(defn collide-right-wall
  [{:keys [v p r bouncy] :as o} wall]
  (assoc o
         :v (if (and (> (v 0) 0) (>= (+ r (p 0)) wall))
              [(* bouncy (- (first v))) (second v)]
              v)))

(defn collide-balls
  [{:keys [v p r] :as ball} other-balls]
  (reduce (fn [b ob]
            (let [d (dist (:p b) (:p ob))]
              (if (<= d (+ (:r b) (:r ob)))
                (let [vab (v-create (:p b) (:p ob))
                      vpb (component (:v b) vab)
                      vpo (component (:v ob) vab)
                      dt (/ (- (+ (:r b) (:r ob)) d) (- vpb vpo))
                      pb (v- (:p b) (v* (:v b) dt))
                      po (v- (:p ob) (v* (:v ob) dt))
                      vab (v-create pb po)
                      pab (project (:v b) vab)
                      pao (project (:v ob) vab)
                      pan (v- (:v b) pab)
                      ; pab' (+ pab (* (+ 1 (:bouncy b)) (- pao pab) (+ 1 (/ (:r b) (:r ob)))))
                      v' (v+ (v* (v- pab) (:bouncy b)) pan)
                      p' (v+ pb (v* v' dt))]
                  (assoc b
                         :v v'
                         :p p'))
                b))) ball other-balls)
  )

(defn step
  [delta-t {:keys [gravity time balls] :as sim}]
  (let [balls' (mapv (fn [b]
                     (-> b
                         (accelerate-g gravity delta-t)
                         (update-position delta-t)
                         (collide-ground 550)
                         (collide-left-wall 50)
                         (collide-right-wall 550))) balls)]
    (assoc sim
           :time (+ delta-t time)
           :balls (mapv (fn [b]
                          (-> b
                              (collide-balls (remove #(= b %) balls')))) balls')) 
    )
  )


(defn main []
  (om/root
    (fn [app owner]
      (reify
        om/IRender
        (render [_]
          (html/html
           [:div
            [:h1 (:text app)]
            [:div
             [:h2 (str "Time " (:time app))]
             [:table
              [:tr
               [:td "Bouncy"]
               [:td [:input {:type "number"
                             :min 0
                             :max 5
                             :step 0.01
                             :value (get-in app [:ball 0 :bouncy])
                             :on-change (fn [e]
                                          (om/update! app [:ball 0 :bouncy] (reader/read-string (.. e -target -value))))}]]]]
             [:button {:on-click (fn [_]
                                   (om/transact! app (partial step 0.001)))} "click"]
             [:button {:on-click (fn [_]
                                   (let [i (.setInterval js/window
                                                         (fn []
                                                           (om/update! app (step 0.01 @app)))
                                                         10)]
                                     (om/set-state! owner :interval i)))} "run"]
             [:button {:on-click (fn [_]
                                   (let [i (om/get-state owner :interval)]
                                     (.clearTimeout js/window i)
                                     (om/set-state! owner :interval nil)))} "stop"]
             [:button {:on-click (fn [_]
                                   (om/transact! app [:balls]
                                     #(mapv (fn [b]
                                              (assoc b
                                                     :p [50 50]
                                                     :v [0 0])) %)))} "reset"]
             [:button {:on-click (fn [_]
                                   (om/transact! app [:balls]
                                     #(conj % (random-ball))))} "add"]
             ]
            [:svg {:width 600 :height 600}
             (map (fn [b]
                    [:circle {:cx (-> b :p first)
                              :cy (-> b :p second)
                              :r (-> b :r)
                              :fill (-> b :color)
                              :stroke "black"}]) (:balls app))]]))))
    app-state
    {:target (. js/document (getElementById "app"))}))

(comment
 (reset! app-state {:text "Hello Devon.  You rock!"})

 (.now js/Date)

 (.abs js/Math -10)
 )
