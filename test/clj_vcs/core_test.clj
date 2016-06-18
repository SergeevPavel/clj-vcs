(ns clj-vcs.core-test
  (:require [clojure.test :refer :all]
            [clj-vcs.core :refer :all]))

(def r1 (commit initial-repo-state {:a 1 :b 2}))

(def r2 (commit r1 {:a 2 :c {:x [1 2 3]}}))

(deftest get-ids-history-test
  (testing "Linear history"
    (is (= [0]) (get-ids-history initial-repo-state))
    (is (= [1 0]) (get-ids-history r1))
    (is (= [2 1 0] (get-ids-history r2)))))

(deftest history-test
  (testing "History of changes by path"
    (is (= [nil] (history initial-repo-state [:a])))
    (is (= [1 nil] (history r1 [:a])))
    (is (= [2 1 nil] (history r2 [:a])))
    (is (= [3 nil nil] (history r2 [:c :x 2])))))
