(ns drop.core
  (:require [om.core :as om :include-macros true]
            [cljs.reader :as reader]
            [sablono.core :as html :refer-macros [html]]
            [cljs-webgl.context :as context]
            [cljs-webgl.shaders :as shaders]
            [cljs-webgl.constants.draw-mode :as draw-mode]
            [cljs-webgl.constants.data-type :as data-type]
            [cljs-webgl.constants.buffer-object :as buffer-object]
            [cljs-webgl.constants.shader :as shader]
            [cljs-webgl.buffers :as buffers]
            [cljs-webgl.constants.capability :as capability]
            [cljs-webgl.constants.blending-factor-dest :as blending-factor-dest]
            [cljs-webgl.typed-arrays :as ta]))

(defn str-color
  [c]
  (apply str "#" (map #(.toString (int (* 255 %)) 16) (take 3 c))))

(defn random-glcolor
  []
  [(rand) (rand) (rand) 1.0])

(defn random-ball
  []
  (let [c (random-glcolor)]
    {:p [(+ 50 (rand-int 500)) (+ 50 (rand-int 500))]
     :v [(- 500 (rand-int 1000)) (- 500 (rand-int 1000))]
     :color (str-color c)
     :c c
     :r (rand-int 30)
     :bouncy (+ 0.5 (/ (rand) 2)) })
  )

(defonce app-state (atom {:text "Bouncyness!"
                          :time 0
                          :frame 0
                          :gravity 980
                          :origin [100.0 100.0]
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
  [delta-t {:keys [gravity time frame balls] :as sim}]
  (let [balls' (mapv (fn [b]
                     (-> b
                         (accelerate-g gravity delta-t)
                         (update-position delta-t)
                         (collide-ground 550)
                         (collide-left-wall 50)
                         (collide-right-wall 550))) balls)]
    (assoc sim
           :time (+ delta-t time)
           :frame (inc frame)
           :balls (mapv (fn [b]
                          (-> b
                              (collide-balls (remove #(= b %) balls')))) balls')) 
    )
  )

(def a-vertex-shader-source
    "attribute vec3 a_vertex_position;
     attribute vec2 a_texCoord;
     uniform vec3 u_resolution;
     varying vec2 v_texCoord;
       void main() {
        vec3 vp = a_vertex_position;
        vec3 zeroToOne = vp / u_resolution;
        vec3 zeroToTwo = zeroToOne * 2.0;
        vec3 vertex_position = zeroToTwo - 1.0;

        gl_Position = vec4(vertex_position, 1);
        v_texCoord = a_texCoord;
       }")


(def vertex-shader-source
    "attribute vec3 a_vertex_position;
     attribute vec2 a_texCoord;
     uniform vec3 a_center;
     uniform vec2 u_origin;
     uniform vec3 u_resolution;
     varying vec2 v_texCoord;
       void main() {
        vec3 vp = a_vertex_position + a_center + vec3(u_origin, 0.0);
        vec3 zeroToOne = vp / u_resolution;
        vec3 zeroToTwo = zeroToOne * 2.0;
        vec3 vertex_position = zeroToTwo - 1.0;

        gl_Position = vec4(vertex_position, 1);
        v_texCoord = a_texCoord;
       }")

(def fragment-shader-source
    "uniform int frame;
       void main() {
         gl_FragColor.r = sin(float(frame) * 0.05) / 2.0 + 0.5;
         gl_FragColor.g = sin(float(frame) * 0.1) / 2.0 + 0.5;
         gl_FragColor.b = sin(float(frame) * 0.02) / 2.0 + 0.5;
         gl_FragColor.a = 1.0;
            }")

(def circle-fragment-source
  "
  precision mediump float;
  uniform float a_radius;
  uniform vec4 a_color;
  varying vec2 v_texCoord;
  void main () {
    float border = 0.01;
    vec3 border_color = vec3(0.0, 0.0, 0.0);
    vec2 circle_center = vec2(0.5, 0.5);

    float dist = distance(v_texCoord, circle_center);

    if (dist <= a_radius) {
      if (dist < (a_radius - border)) {
         gl_FragColor = a_color;
      } else {
         gl_FragColor = vec4(border_color, (a_radius - dist) * 100.0);
      }
    } else {
      discard;
    }
  }
  "
  )

(def grid-fragment-source
  "precision mediump float;
   uniform vec2 u_origin;
   uniform vec4 u_bg_color;
   uniform vec4 u_grid_color;
   uniform vec4 u_axis_color;
   varying vec2 v_texCoord;
   void main () {
     float range = 600.0;
     float grid_size = 50.0;
     vec2 wc = (v_texCoord * range) - u_origin;
     vec2 sc = mod(wc, grid_size) / grid_size;

     vec4 c = u_bg_color;
     if ((wc.x <= 1.0 && wc.x >= -1.0) ||
         (wc.y <= 1.0 && wc.y >= -1.0)) {
        gl_FragColor = u_axis_color;
     } else {
        if (sc.x <= 0.01 || sc.x >= 0.99) {
           c = u_grid_color;
        }
        if (sc.y <= 0.01 || sc.y >= 0.99) {
           c = u_grid_color;
        }
        gl_FragColor = c;
     }
   }
  "
  )

(defn draw-grid-fn
  [gl w h]
  (let [vertex-buffer  (buffers/create-buffer gl  (ta/float32  [w h 0.0
                                                                0.0 h 0.0
                                                                w 0.0 0.0
                                                                0.0 h 0.0
                                                                0.0 0.0 0.0
                                                                w 0.0 0.0])
                                              buffer-object/array-buffer
                                              buffer-object/static-draw)
        tex-buffer  (buffers/create-buffer gl  (ta/float32  [1.0 1.0
                                                             0.0 1.0
                                                             1.0 0.0
                                                             0.0 1.0
                                                             0.0 0.0
                                                             1.0 0.0])
                                           buffer-object/array-buffer
                                           buffer-object/static-draw)
        element-buffer  (buffers/create-buffer gl (ta/unsigned-int16  [0 1 2 3 4 5])
                                               buffer-object/element-array-buffer
                                               buffer-object/static-draw)]
    (fn  [origin]
      (let [shader  (shaders/create-program gl
                                            (shaders/create-shader gl shader/vertex-shader a-vertex-shader-source)
                                            (shaders/create-shader gl shader/fragment-shader grid-fragment-source))]
        (buffers/draw! gl
                       :shader shader
                       :draw-mode draw-mode/triangles
                       :count 6

                       :attributes
                       [{:buffer vertex-buffer
                         :location  (shaders/get-attrib-location gl shader "a_vertex_position")
                         :components-per-vertex 3
                         :type data-type/float}
                        {:buffer tex-buffer
                         :location  (shaders/get-attrib-location gl shader "a_texCoord")
                         :components-per-vertex 2
                         :type data-type/float}]

                       :uniforms
                       [{:name "u_resolution" :type :vec3 :values (ta/float32 [600.0 600.0 1.0])}
                        {:name "u_origin" :type :vec2 :values (ta/float32 origin)}
                        {:name "u_bg_color" :type :vec4 :values (ta/float32 [0.8 0.8 0.8 1.0])}
                        {:name "u_grid_color" :type :vec4 :values (ta/float32 [0.9 0.9 0.9 1.0])}
                        {:name "u_axis_color" :type :vec4 :values (ta/float32 [1.0 1.0 1.0 1.0])}]
                       :element-array
                       {:buffer element-buffer
                        :count 3
                        :type data-type/unsigned-short
                        :offset 0})))))

(defn init-gl  [canvas]
  (let
    [gl  (context/get-context  canvas)
     grid-fn (draw-grid-fn gl 600 600)
     vertex-buffer  (buffers/create-buffer gl  (ta/float32  [100.0 100.0 0.0
                                                             -100.0 100.0 0.0
                                                             100.0 -100.0 0.0
                                                             -100.0 100.0 0.0
                                                             -100.0 -100.0 0.0
                                                             100.0 -100.0 0.0])
                                           buffer-object/array-buffer
                                           buffer-object/static-draw)
     tex-buffer  (buffers/create-buffer gl  (ta/float32  [1.0 1.0
                                                          0.0 1.0
                                                          1.0 0.0
                                                          0.0 1.0
                                                          0.0 0.0
                                                          1.0 0.0])
                                           buffer-object/array-buffer
                                           buffer-object/static-draw)
     element-buffer  (buffers/create-buffer gl  (ta/unsigned-int16  [0 1 2 3 4 5])
                                            buffer-object/element-array-buffer
                                            buffer-object/static-draw)
     draw  (fn  [balls origin]
             (let [shader  (shaders/create-program gl
                                                    (shaders/create-shader gl shader/vertex-shader vertex-shader-source)
                                                    (shaders/create-shader gl shader/fragment-shader circle-fragment-source))]
               (-> gl
                 (buffers/clear-color-buffer 1 1 1 1))
               (grid-fn origin)
               (doseq [{:keys [p r c]} balls]
                 (buffers/draw! gl :shader shader
                                :draw-mode draw-mode/triangles
                                :count 6

                                :attributes
                                [{:buffer vertex-buffer
                                  :location  (shaders/get-attrib-location gl shader "a_vertex_position")
                                  :components-per-vertex 3
                                  :type data-type/float}
                                 {:buffer tex-buffer
                                  :location  (shaders/get-attrib-location gl shader "a_texCoord")
                                  :components-per-vertex 2
                                  :type data-type/float}]

                                :uniforms
                                [{:name "u_resolution" :type :vec3 :values (ta/float32 [600.0 600.0 1.0])}
                                 {:name "u_origin" :type :vec2 :values (ta/float32 origin)}
                                 {:name "a_center" :type :vec3 :values (ta/float32 [(p 0) (- 600 (p 1)) 0.0])}
                                 {:name "a_radius" :type :float :values (ta/float32 [(/ r 200.0)])}
                                 {:name "a_color" :type :vec4 :values (ta/float32 c)}]

                                :element-array
                                {:buffer element-buffer
                                 :count 3
                                 :type data-type/unsigned-short
                                 :offset 0}
                                :capabilities {capability/depth-test false capability/blend true}
                                :blend-function {blending-factor-dest/src-alpha 1}
                                )
                 )
               ))]
    draw))


(defn relative-coord
  [e]
  (let [t (.. e -target)
        r (.getBoundingClientRect t)]
    [(- (.. e -clientX) (.-left r))
     (- (.-height t) (- (.. e -clientY) (.-top r)))]))

(defn main []
  (om/root
    (fn [app owner]
      (reify
        om/IDidMount
        (did-mount [_]
          (let [d (init-gl (om/get-node owner "gl"))]
            (om/set-state! owner :gl-draw d)
            (d (:balls app) (:origin app))
            ))
        om/IRender
        (render [_]
          (html/html
           [:div
            [:h1 (:text app)]
            [:div
             [:h2 (str "Time " (:time app))]
             [:h3 (str "FPS " (/ (:frame app) (:time app)))]
             [:table
              [:tr
               [:td "Bouncy"]
               [:td [:input {:type "number"
                             :min 0
                             :max 5
                             :step 0.01
                             :value (get-in app [:ball 0 :bouncy])
                             :on-change (fn [e]
                                          (om/update! app [:ball 0 :bouncy] (reader/read-string (.. e -target -value))))}]]]
              [:tr
               [:td "Gravity"]
               [:td [:input {:type "number"
                             :min 0
                             :max 10000
                             :step 10
                             :value (:gravity app)
                             :on-change (fn [e]
                                          (om/update! app :gravity (reader/read-string  (.. e -target -value))))}]]]]
             [:button {:on-click (fn [_]
                                   (om/transact! app (partial step 0.001)))} "click"]
             [:button {:on-click (fn [_]
                                   (let [i (.setInterval js/window
                                                         (fn []
                                                           (let [app' (step 0.01 @app)]
                                                             (om/update! app app')
                                                             (if-let [d (om/get-state owner :gl-draw)]
                                                               (d (:balls app') (:origin app')))))
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
                                                     :p [200 50]
                                                     :v [0 0])) %)))} "reset"]
             [:button {:on-click (fn [_]
                                   (om/transact! app [:balls]
                                     #(conj % (random-ball))))} "add"]
             ]
            [:div
             [:canvas {:width 600
                       :height 600
                       :float "right"
                       :ref "gl"
                       :on-click (fn [e]
                                   (let [pt (relative-coord e)]
                                     (om/update! app :origin pt)
                                     (if-let [d (om/get-state owner :gl-draw)]
                                       (d (:balls @app) pt))))
                       :on-mouse-move (fn [e]
                                        (when (= 1 (bit-and (.-button e) 1))
                                          (let [pt (relative-coord e)]
                                            (om/update! app :origin pt))))}]
             [:svg {:width 600 :height 600}
              (map (fn [b]
                     [:circle {:cx (-> b :p first)
                               :cy (-> b :p second)
                               :r (-> b :r)
                               :fill (-> b :color)
                               :stroke "black"}]) (:balls app))]]]))))
    app-state
    {:target (. js/document (getElementById "app"))}))

(comment
 (reset! app-state {:text "Hello Devon.  You rock!"})

 (.now js/Date)

 (.abs js/Math -10)

 (aset js/document "title" "Hi Haters!")

 (. js/document -title)

 )
