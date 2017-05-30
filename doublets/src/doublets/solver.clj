(ns doublets.solver
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clj-fuzzy.metrics :refer [levenshtein]]))

(def words (-> "words.edn"
               (io/resource)
               (slurp)
               (read-string)))

(defn count-diff-letters
  [word1 word2]
  (reduce (fn [sum pair] (if (= (first pair) (second pair))
                           sum
                           (inc sum)))
          0
          (partition 2 (interleave word1 word2))))

(defn find-links
  [word invalid-links]
  (let [valid? #(and
                  (not (contains? invalid-links %))
                  (= (count %) (count word)))]
    (for [w (filter valid? words)
          :let [diff-letters (count-diff-letters word w)]
          :when (<= diff-letters 1)]
      w)))

(defn lexo-compare
  [target-word word1 word2]
  (compare
    (levenshtein target-word word1)
    (levenshtein target-word word2)))

(defn doublets
  [word1 word2]
  (let [cmp (partial lexo-compare word2)
        next-link (fn [curr-links curr]
                    (let [links (find-links curr (set curr-links))]
                      (first (sort cmp links))))
        reducer (fn [curr-links curr]
                  (if (some? curr)
                    (let [links (conj curr-links curr)]
                      (if (= curr word2)
                        links
                        (recur links (next-link links curr))))
                    []))]
    (reducer [] word1)))
