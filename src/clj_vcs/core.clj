(ns clj-vcs.core
  (:gen-class))

(require '[clojure.set :refer [union]])
(require '[clojure.data :refer [diff]])

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))


(def initial-repo-state {:index {
                                 0 [{} []]
                                 }
                         :branches {"master" 0}
                         :current "master"
                         })

(defn get-next-commit-id
  [repo]
  (let [ks (keys (:index repo))
        m (apply max ks)]
    (inc m)))

(defn get-current-id
  [repo]
  (let [current (:current repo)]
    ((:branches repo) current)))

(defn commit
  "Add new snapshot to current branch."
  [repo snapshot]
  (let [id (get-next-commit-id repo)
        parent-id (get-current-id repo)
        {i :index b :branches c :current} repo]
    {:index (assoc i id [snapshot [parent-id]])
     :branches (assoc b c id)
     :current c
     }))

(defn get-ids-history
  [index current-id]
  (loop [id current-id history []]
    (if (nil? id)
      history
      (recur (get-in index [id 1 0])
             (conj history id)))))

(defn history
  "Return history of changes."
  [repo path]
  (let [ids-history (get-ids-history (repo :index) (get-current-id repo))
        snapshots (map (fn [id] (get-in repo [:index id 0])) ids-history)]
    (map (fn [sn] (get-in sn path)) snapshots)))

(defn branch
  "Create new branch."
  [repo branch-name]
  (let [id (get-current-id repo)
        {i :index b :branches c :current} repo]
    {:index i
     :branches (assoc b branch-name id)
     :current c
     }))

(defn checkout
  "Switch current branch."
  [repo branch-name]
  (assoc repo :current branch-name))

;; При мерже с веткой созданной из уже замерженой,
;; на данный момент, выбирается не самый поздний родительский коммит.
; as far as I can see comment is outdated? right?
; no, example below
(defn find-common-parent
  [index h1 h2]
  (loop [ids1 (reverse (get-ids-history index h1))
         ids2 (reverse (get-ids-history index h2))
         prev nil]
    (if (or (not= (first ids1) (first ids2)) (empty? ids1) (empty? ids2))
      prev
      (recur (rest ids1) (rest ids2) (first ids1)))))

; expected 7, but 2 returned
(comment
  (find-common-parent {-1 [{} []]
                       0  [{} [-1]]
                       1  [{} [0]]
                       2  [{} [1]]
                       3  [{} [2]]
                       4  [{} [3 7]]
                       5  [{} [4]]
                       6  [{} [2]]
                       7  [{} [6]]
                       8  [{} [7]]
                       9  [{} [8]]}
                      5 9)
  )

(comment
  (find-common-parent {-1 [{} []]
                       0 [{} [-1]]
                       1 [{} [0]]
                       2 [{} [1]]
                       3 [{} [0]]
                       }
                      2 3)

  (get-ids-history {-1 [{} []]
                    0 [{} [-1]]
                    1 [{} [0]]
                    2 [{} [1]]
                    3 [{} [0]]
                    } 2)
  
  )

(declare three-way-merge)

(defn seqzip
  "returns a sequence of [[ value-left] [value-right]....]  padding with nulls."
  [& seqs]
  (loop [l [] s seqs]
    (if (some some? (map seq s))
      (recur (conj l (map first s)) (map rest s))
       l)))

(defn merge-values
  [pv av bv]
  (cond
    (= av bv)                av
    (= av pv)                bv
    (= bv pv)                av
    (every? map? [av bv pv]) (three-way-merge pv av bv)
    (every? sequential? [av bv pv]) (map (fn [[p a b]]
                                           (merge-values p a b))
                                         (seqzip pv av bv))
    (every? set? [av bv pv]) (union av bv)
    :else (throw (Exception. "Merge conflict"))))

(defn three-way-merge
  [p a b]
  (let [all-keys (into #{} (union (keys p) (keys a) (keys b)))]
    (reduce (fn [r k]
              (let [v (merge-values (p k) (a k) (b k))]
                (if (some? v)
                  (assoc r k v)
                  r)))
            {}
            all-keys)))

(defn merge-commits
  [index id1 id2]
  (let [pid (find-common-parent index id1 id2)
        psn (get-in index [pid 0])
        sn1 (get-in index [id1 0])
        sn2 (get-in index [id2 0])]
    [(three-way-merge psn sn1 sn2) [id1 id2]]))

(defn merge-branches 
  "Merge current branch to target."
  [repo target]
  (let [{i :index b :branches c :current} repo
        target-id (b target)
        current-id (b c)
        next-id (get-next-commit-id repo)]
    {
     :index (assoc i next-id (merge-commits i target-id current-id))
     :branches (dissoc (assoc b target next-id) c)
     :current target
     }))

;; FIXME fails on {:x {1 2}} {:x [1 2]}
(defn recursive-merge
  " Merge two structures recusively , taking non-nil values from sequences and maps and merging sets." 
  [part-state original-state]
  (cond
    (sequential? part-state) (map (fn [[l r]]
                                    (recursive-merge l r))
                                  (seqzip part-state original-state))
    (map? part-state) (merge-with recursive-merge part-state original-state)
    (set? part-state) (union part-state original-state)
    (nil? part-state) original-state
    :else part-state))

(defn diff-structures
  [before after]
  (let [[del add _] (diff before after)]
    [del add]))

(defn add-values
  [base changes]
  (recursive-merge base changes))

(defn remove-values
  [base changes]
  (let [[result _ _] (diff base changes)]
    result))

(defn apply-diff
  [base difference]
  (let [[del add] difference]
    (add-values (remove-values base del) add)))

(defn get-snapshots-between
  [index up-id dn-id]
  (loop [id dn-id snapshots []]
    (if (= up-id id)
      (conj snapshots (get-in index [id 0]))
      (recur (get-in index [id 1 0]) (conj snapshots (get-in index [id 0]))))))

(defn snapshot-difference
  [snapshots]
  (map diff-structures snapshots (rest snapshots)))

(defn create-rebased-snapshots
  [index from-id to-id]
  (let [pid (find-common-parent index from-id to-id)
        snapshots (reverse (get-snapshots-between index pid from-id))
        diffs (snapshot-difference snapshots)
        base (get-in index [to-id 0])]
    (rest (reductions apply-diff base diffs))))

(defn rebase
  "Reabase commits from current to target."
  [repo target]
  (let [{i :index b :branches c :current} repo
        current-id (b c)
        target-id (b target)
        snapshots (create-rebased-snapshots i current-id target-id)
        new-repo (assoc-in repo [:branches c] target-id)]
    (reduce commit new-repo snapshots)))

;; the main problem here is the diff structure. it should be recursive, implemented for all datasctrucures. remove-nil-values is non-sense and so is assic'ing nil in case of the difference. you should go deeper. Apart from this rebase code is quite elegant. That's why I want to to fix it.

;; rebase and merge should have a continuation state in case of the conflicts. user should be able to resolve them and call the continuation with the snapshot corrected.
;; conflict snapshot state should contain special conflict-nodes which contain two conflicting values so user could be able to choose one of them by replacing conflict-node with it.
;; However, this is not a requirement, I'm not expecting you to implement it but just to give an idea of how it might be implemented in a better way. Exceptions is not a best way to handle user-level errors. Check out continuation-based error handling in Lisps which serve well in this particular case.
