(ns clj-vcs.core
  (:gen-class))

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
(defn find-common-parent
  [index h1 h2]
  (loop [ids1 (reverse (get-ids-history index h1))
         ids2 (reverse (get-ids-history index h2))
         prev nil]
    (if (or (not= (first ids1) (first ids2)) (empty? ids1) (empty? ids2))
      prev
      (recur (rest ids1) (rest ids2) (first ids1)))))

(declare three-way-merge)

(defn merge-values
  [pv av bv]
  (cond
    (= av bv)                av
    (= av pv)                bv
    (= bv pv)                av
    (every? map? (av bv pv)) (three-way-merge pv av bv)
    :else (throw (Exception. "Merge conflict"))))

(defn remove-nil-values
  [m]
  (reduce (fn [r k] (cond
                      (map? (m k))  (assoc r k (remove-nil-values (m k)))
                      (some? (m k)) (assoc r k (m k))
                      :else         r)) {} (keys m)))

(defn three-way-merge
  [p a b]
  (let [all-keys (concat (keys p) (keys a) (keys b))]
    (reduce (fn [r k] (assoc r k (merge-values (p k) (a k) (b k)))) {} all-keys)))

(defn merge-commits
  [index id1 id2]
  (let [pid (find-common-parent index id1 id2)
        psn (get-in index [pid 0])
        sn1 (get-in index [id1 0])
        sn2 (get-in index [id2 0])]
    [(remove-nil-values (three-way-merge psn sn1 sn2)) [id1 id2]]))

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

(defn rebase
  "Reabase commits from current to target."
  [repo target]
  nil)
