(ns take-home-project-1.core
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [clojure.walk :as walk])
  (:gen-class))

(defn constant? [s]
  (or (number? s) (string? s) (boolean? s) (keyword? s)
      (nil? s)))

(defn variable? [s]
  (and (symbol? s) (str/starts-with? (str s) "!") (not (str/ends-with? (str s) "+"))))

(defn assignment? [s]
  (and (str/starts-with? (str s) "!") (str/ends-with? (str s) "+")))

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

(defn arg-symbols
  "Given an argument list, return a list of symbols that are variables."
  [args]
  (reduce (fn [accum arg]
            (if (variable? arg)
              (conj accum arg)
              accum))
          [] args))

(defn body-symbols
  "Given a defstackfn body, return a set of symbols that are variables."
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
  "Takes a list of symbols and returns a list of locals declarations.
  This is spliced into a let binding. The locals are initialized to the argument values."
  [args]
  (reduce (fn [accum arg] (conj accum arg `(atom ~arg)))
          [] args))

(defn declare-body-locals
  "Takes a set of symbols and returns a list of locals declarations.
  This is spliced into a let binding. The locals are initialized to nil."
  [locals]
  (reduce (fn [accum local] (conj accum local `(atom nil)))
          [] locals))

(defn if-reducer
  "Takes an if form and returns a vector of two forms, the then and else branches."
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

(defn compile-body
  [stack-name var-table body]
  (let [form (first body)]
    (when form
      (cond
        (variable? form) (if (var-table form)
                           `((push-item! ~stack-name ~form)
                             ~@(compile-body stack-name var-table (rest body)))
                           (throw (IllegalArgumentException. (str "Variable " form " not found."))))
        (assignment? form) (let [var (symbol (get-variable-from-assignment form))]
                             `((let [~var (assign-top ~stack-name)]
                                 ~@(compile-body stack-name  (conj var-table var) (rest body)))))
        (pop? form) `((pop-item! ~stack-name)
                      ~@(compile-body stack-name var-table (rest body)))
        (invoke? form) (let [[_ func arity] form]
                         `((push-item! ~stack-name (~func ~@(repeat arity `(pop-item! ~stack-name))))
                           ~@(compile-body stack-name var-table (rest body))))
        (if? form) (let [[then else] (if-reducer form)]
                     (if (seq else)
                       `((if (pop-item! ~stack-name)
                           (do ~@(compile-body stack-name var-table then))
                           (do ~@(compile-body stack-name var-table else)))
                         ~@(compile-body stack-name var-table (rest body)))
                       (throw (IllegalArgumentException. "if> requires an else> branch"))))
        :else  `((push-item! ~stack-name ~form)
                 ~@(compile-body stack-name var-table (rest body)))))))

(defmacro defstackfn
  "Macro for the stack function DSL. Takes a form, an argument list, and a body."
  [form args & body]
  (let [stack-name (gensym 'stack)
        arg-vars (into #{} (arg-symbols args))]
    `(defn ~form ~args
       (try
         (let [~stack-name (atom [])]
           ~@(compile-body stack-name arg-vars body)
           (pop-item! ~stack-name))
         (catch IllegalStateException ~'_
           (throw (IllegalStateException. "Invalid stack operation")))))))
