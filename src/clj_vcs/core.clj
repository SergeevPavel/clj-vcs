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
  [repo]
  (let [{index :index} repo]
    (loop [id (get-current-id repo) history []]
      (if (nil? id)
        history
        (recur (get-in index [id 1 0])
               (conj history id))))))

(defn history
  "Return history of changes."
  [repo path]
  (let [ids-history (get-ids-history repo)
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

(defn merge-branches
  "Merge current branch to target."
  [repo target]
  nil)

(defn rebase
  "Reabase commits from current to target."
  [repo target]
  nil)
