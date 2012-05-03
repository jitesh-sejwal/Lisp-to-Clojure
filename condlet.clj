;=> (condlet [(= 12 12) [x 0 y 1]
;            (= 1 2) [x 1 y 2]
;            true [y 30 z 90]]
;           (list x y z))
;(0 1 nil)



(defn condlet-clauses [vars body-fn-name [condition binding]]
  `(~condition
    (let [~@(mapcat (fn [x] (list x nil)) vars)]
      (let
         ~binding
         (~body-fn-name ~@vars)))))

(defmacro condlet [clauses & body]
  (let [bents (partition 2 clauses)
        body-fn-name (gensym)
        vars (distinct
               (mapcat
                 (fn [[condition bindings]]
                   (map first (partition 2 bindings)))
                 bents))]
    `(letfn
      [(~body-fn-name [~@vars] ~@body)]
      (cond
        ~@(mapcat
            #(condlet-clauses vars body-fn-name %)
            bents)))))