(use 'clojure.contrib.def)
(declare article)
(declare noun)
(declare verb)
(declare noun-phrase)
(declare verb-phrase)

(declare one-of)

(defn sentence []
  (concat (noun-phrase) (verb-phrase)))
(defn noun-phrase []
  (concat (article) (noun)))
(defn verb-phrase []
  (concat (verb) (noun-phrase)))
(defn noun []
  (one-of '(man ball woman table)))
(defn article []
  (one-of '(a the)))
(defn verb []
  (one-of '(hit took saw fucked)))
(defn one-of [s]
     (list (nth s (rand-int (count s)))))
  
(defvar *simple-grammar* 
  {:sentence [[:noun-phrase :verb-phrase]]
   :noun-phrase [[:Article :Noun]]
   :verb-phrase [[:Verb :noun-phrase]]
   :Article ['the 'a]    
   :Noun ['man 'ball 'woman 'table]
   :Verb ['hit 'took 'saw 'liked]})

(def *grammar*  *simple-grammar*)

(defn rule-lhs [rule]
  (key rule))
(defn rule-rhs [rule]
  (val rule))
(defn rewrites [category]
  (get  *grammar* category))
(defn generate [phrase]
     (cond 
       (vector? phrase) (mapcat generate phrase)
       (rewrites phrase) (mapcat generate (one-of (rewrites phrase)))
       true (list phrase)))
(defvar *bigger-grammar*
  {:sentence [[:noun-phrase :verb-phrase]]
    :noun-phrase [[ :Article :Adj* :Noun :PP* :Name :Pronoun]]
    :verb-phrase  [[:Verb :noun-phrase :PP*]]
    :PP*  [ [] [[:PP :PP*]]]
    :Adj*  [ [] [[:Adj :Adj*]]]
    :PP  [[:Prep :noun-phrase]]
    :Prep ['to 'in 'by 'with 'on]
    :Adj ['big 'little 'blue 'green 'adiabatic]
    :Article [ 'the 'a]
    :Name ['Pat 'Kim 'Lee 'Terry 'Robin]
    :Noun ['man 'ball 'woman 'table]
    :Verb ['hit 'took 'saw 'liked]
    :Pronoun ['he 'she 'it 'these 'those 'that]})

(def *grammar* *bigger-grammar*)
    
(declare len=1)
(declare build-cases)
(declare build-code)

(defn compile-rule [rule]
  (let [rhs (rule-rhs rule)]
    `(defn ~(rule-lhs rule) []
       ~(cond 
          (every? symbol? rhs) `(one-of '~rhs)
          (len=1 rhs) (#'build-code (first rhs))
          true `(case (random ~(count rhs))
                  ~@(build-cases 0 rhs))))))

(defn build-cases [number choice]
  (if (and (seq? choice) (not (empty? choice)))
    (list (list number) (#'build-code (first choice))
          (#'build-cases (dec number) (rest choice)))))
(defn build-code [choice]
  (cond
    (nil? choice) nil
    (or (symbol? choice) (keyword choice)) (list choice)
    (len=1 choice) choice
    true `(concat ~@(map #'build-code choice))))
(defn len=1 [x]
  (and (vector? x) (= (count x) 1)))
  
          

