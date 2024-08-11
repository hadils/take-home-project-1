(ns take-home-project-1.core
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [clojure.walk :as walk])
  (:gen-class))

(defn variable? [s]
  (and (symbol? s) (str/starts-with? (str s) "!")))

(defn assignment? [s]
  (and (variable? s) (str/ends-with? (str s) "+")))

(defn get-variable-from-assignment
  [s]
  (subs (str s) 0 (- (count (str s)) 1)))

(defn invoke?
  [s]
  (and (list? s) (symbol? (first s)) (= (first  s) 'invoke>)))

(defn if?
  [s]
  (and (list? s) (symbol? (first s)) (= (first s) 'if>)))

(defn else?
  [s]
  (and (symbol? s) (= s 'else>)))

(defn pop?
  [s]
  (and (symbol? s) (= s '<pop>)))

(defn push-item! [stack v]
  (reset! stack (conj @stack v)))

(defn pop-item! [stack]
  (let [v (peek @stack)]
    (reset! stack (pop @stack))
    v))

(defn assign-top [stack]
  (peek @stack))

(defn process-arguments
  "Return a let statement with the list of variables, assigned to atoms."
  [args]
  (reduce (fn [accum arg]
            (if (variable? arg)
              (conj accum arg `(atom ~arg))
              accum))
          [] args))

(defn arg-symbols
  [args]
  (reduce (fn [accum arg]
            (if (variable? arg)
              (conj accum arg)
              accum))
          [] args))

(defn body-symbols
  [body]
  (walk/walk (fn [form]
               (when (assignment? form)
                 (symbol (get-variable-from-assignment form))))
             (fn [forms]
               (->> forms
                    flatten
                    (filter identity)
                    (into #{}))) body))

(defn declare-arg-locals
  [args]
  (reduce (fn [accum arg] (conj accum arg `(atom ~arg)))
          [] args))

(defn declare-body-locals
  [locals]
  (reduce (fn [accum local] (conj accum local `(atom nil)))
          [] locals))

(defn if-reducer
  [form]
  (let [{:keys [then else]} (reduce (fn [accum form]
                                      (if (else? form)
                                        (assoc accum :else-branch? true)
                                        (if (:else-branch? accum)
                                          (assoc accum :else (conj (:else accum) form))
                                          (assoc accum :then (conj (:then accum) form)))))
                                    {:else-branch? false
                                     :then []
                                     :else []} (rest form))]
    [then else]))

(defn process-body
  [stack-name body]
  (walk/walk (fn [form]
               (cond
                 (assignment? form) `(reset! ~(symbol (get-variable-from-assignment form)) (assign-top ~stack-name))
                 (variable? form) `(push-item! ~stack-name (deref ~form))
                 (pop? form) `(pop-item! ~stack-name)
                 (invoke? form)
                 (let [[_ func arity] form]
                   `(push-item! ~stack-name (~func ~@(repeat arity `(pop-item! ~stack-name)))))
                 (if? form)
                 (let [symbols (body-symbols (rest form))
                       [then else] (if-reducer form)]
                   `(let [~@(declare-body-locals symbols)]
                      (if (pop-item! ~stack-name)
                        (do ~@(process-body stack-name then))
                        (do ~@(process-body stack-name else)))))
                 :else
                 `(push-item! ~stack-name ~form))) identity body))

(defmacro defstackfn
  [form args & body]
  (let [stack-name (gensym 'stack)
        arg-symbols (arg-symbols args)
        body-symbols (body-symbols body)]
    `(defn ~form ~args
       (let [~stack-name (atom [])
             ~@(declare-arg-locals arg-symbols)
             ~@(declare-body-locals body-symbols)]
         ~@(process-body stack-name body)
         (pop-item! ~stack-name)))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(comment
  (defstackfn f [!a !b !c]
    !a
    !b
    (invoke> + 2)
    !v1+
    !c
    !c
    <pop>
    2
    (invoke> * 2)
    !v2+
    (invoke> = 2)
    (if> !v1 !v2 (invoke> - 2) else> "false!" (invoke> println 1) <pop> !v1 !v2 (invoke> * 2))))
