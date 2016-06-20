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

;; (pprint rs)

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

(def repo {:index {
                   0 [{} []]
                   1 [{} [0]]
                   2 [{} [1]]
                   3 [{} [2]]
                   4 [{} [1]]
                   5 [{} [4]]
                   6 [{} [3 5]]
                   7 [{} [6]]
                   }
           :branches {"master" 0}
           :current "master"
           })

(def index (repo :index))

(deftest find-common-parent-test
  (testing "Find common parrent of two commits."
    (is (= 2 (find-common-parent index 7 2)))
    (is (= 3 (find-common-parent index 3 3)))
    (is (= 1 (find-common-parent index 5 3)))))

(def before {:index {
                     0 [{} []]
                     1 [{:a 1} [0]]
                     2 [{:a 1 :b 2} [1]]
                     3 [{:a 1 :b 3} [2]]
                     4 [{:a 1 :b {:x 42 :y 42}} [3]]
                     5 [{:a 2} [1]]
                     6 [{:a 2 :c 4} [5]]
                     }
             :branches {"master" 4
                        "develop" 6}
             :current "develop"})

(def after-merge {:index {
                          0 [{} []]
                          1 [{:a 1} [0]]
                          2 [{:a 1 :b 2} [1]]
                          3 [{:a 1 :b 3} [2]]
                          4 [{:a 1 :b {:x 42 :y 42}} [3]]
                          5 [{:a 2} [1]]
                          6 [{:a 2 :c 4} [5]]
                          7 [{:a 2 :b {:x 42 :y 42} :c 4} [4 6]]
                          }
                  :branches {"master" 7}
                  :current "master"})

(def after-rebase {:index
                   {0 [{} []],
                    1 [{:a 1} [0]],
                    2 [{:a 1, :b 2} [1]],
                    3 [{:a 1, :b 3} [2]],
                    4 [{:a 1, :b {:x 42, :y 42}} [3]],
                    5 [{:a 2} [1]],
                    6 [{:a 2, :c 4} [5]],
                    7 [{:a 2, :b {:x 42, :y 42}} [4]],
                    8 [{:a 2, :b {:x 42, :y 42}, :c 4} [7]]},
                   :branches {"master" 4, "develop" 8},
                   :current "develop"})

(deftest simple-merge-branches-test
  (testing "Simple merge branches test."
    (is (= after-merge (merge-branches before "master")))))


(deftest simple-rebase-branches-test
  (testing "Simple rebase branches test."
    (is (= after-rebase (rebase before "master")))))

(deftest diff-test
  (testing "Diff test."
    (let [x {:a [1 2 3]
             :b {:foo #{4 5 6}
                 :bar 42
                 :baz {1 2}
                 }
             }
          y {:a [2 3 4]
             :b {:foo #{5 6 7}
                 :baz {1 2}
                 }
             :c 42
             }
          d (diff-structures x y)]
      (is (= y (apply-diff x d))))))

(deftest three-way-merge-test
  (testing "Three way merge test."
    (let [p {:a [1 2]
             :b {:foo #{4 5}
                 :bar 42
                 :baz {:k 10 :l 15}
                 }
             :d 10
             }
          x {:a [1 2 3]
             :b {:foo #{5 6}
                 :bar 42
                 :baz {:k 11 :l 15}
                 }
             :d 10
             }
          y {:a [1 2 3 4]
             :b {:foo #{7 8}
                 :bar 43
                 :baz {:k 10 :l 16}
                 }
             }
          r {:a [1 2 3 4]
             :b {:foo #{5 6 7 8}
                 :bar 43
                 :baz {:k 11 :l 16}
                 }
             }]
      (is (= r (three-way-merge p x y))))))
