(ns take-home-project-1.core-test
  (:require [clojure.test :refer :all]
            [take-home-project-1.core :refer :all]))

(deftest arg-symbols-test
  (testing "find symbols in a list of arguments"
    (is (= '[!a !b] (arg-symbols '[!a !b])))
    (is (= '[!a !b] (arg-symbols '[!a x !b y])))))

(deftest body-symbols-test
  (testing "find symbols in a body"
    (is (= #{'!a '!b} (body-symbols '(!a+ !b+ 1 2 <pop>))))))

(defstackfn assignment-test-fn []
  1 !b+)

(deftest assignment-test
  (testing "assignment does not pop the stack"
    (is (= 1 (assignment-test-fn)))))

(defstackfn example-test-fn [!a !b !c]
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
  (if>
   !v1 !v2
   (invoke> - 2)
   else> "false!"
   (invoke> println 1) <pop> !v1 !v2 (invoke> * 2)))

(deftest example-test
  (testing "example test"
    (is (= 24 (example-test-fn 1 2 4)))
    (is (= 0  (example-test-fn 0 0 0)))
    (is (= 16 (example-test-fn 0 2 4)))))

(defstackfn shadow-test-fn
  [!x]
  0 !y+
  !x
  (if> 1 !y+ else> 2 !y+)
  !y)

(deftest shadow-test
  (testing "a variable assignment in either branch of an if> masks the variable outside of the if>"
    (is (= 0 (shadow-test-fn true)))
    (is (= 0 (shadow-test-fn false)))))

(defstackfn invalid-stack-operation-test-fn
  []
  <pop>)

(deftest invalid-stack-operation-test
  (testing "invalid stack operation"
    (is (thrown-with-msg? Exception #"Invalid stack operation" (invalid-stack-operation-test-fn)))))
