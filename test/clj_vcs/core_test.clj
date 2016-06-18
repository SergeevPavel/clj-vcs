(ns clj-vcs.core-test
  (:require [clojure.test :refer :all]
            [clj-vcs.core :refer :all]))

(use '[clojure.pprint :only (pprint)])

(def commands [(fn [r] (commit r {:a 1 :b 2}))
               (fn [r] (commit r {:a 2 :c {:x [1 2 3]}}))
               (fn [r] (branch r "develop"))
               (fn [r] (commit r {:a 2 :c {:x [1 2 3]} :d 4}))
               (fn [r] (checkout r "develop"))
               (fn [r] (commit r {:a 2 :c {:x [1 2 3]} :e 5}))
               (fn [r] (checkout r "master"))
               ])

(def rs (reductions (fn [r c] (c r)) initial-repo-state commands))

(pprint rs)

(deftest get-ids-history-test
  (testing "Linear history ids"
    (is (= [0]) (get-ids-history (nth rs 0)))
    (is (= [1 0]) (get-ids-history (nth rs 1)))
    (is (= [2 1 0] (get-ids-history (nth rs 2))))))

(deftest history-test
  (testing "History of changes by path"
    (is (= [nil] (history (nth rs 0) [:a])))
    (is (= [1 nil] (history (nth rs 1) [:a])))
    (is (= [2 1 nil] (history (nth rs 2) [:a])))
    (is (= [3 nil nil] (history (nth rs 2) [:c :x 2])))))

(deftest multiple-branches-test
  (testing "History from different branches."
    (is (= [5 nil nil nil]   (history (nth rs 6) [:e])))
    (is (= [nil nil nil nil] (history (nth rs 7) [:e])))
    (is (= [4 nil nil nil]   (history (nth rs 4) [:d])))
    (is (= [nil nil nil]     (history (nth rs 5) [:d])))))
