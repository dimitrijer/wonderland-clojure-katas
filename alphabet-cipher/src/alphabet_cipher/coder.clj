(ns alphabet-cipher.coder
  (require [clojure.set :refer [map-invert]]))

(def ^:private alphabet (map char (range (int \a) (inc (int \z)))))

(defn- rotate
  "Rotates input coll by removing element at the front and appending it at the
  back."
  ([row]
   (conj  (vec (rest row)) (first row)))
  ([row times]
   (nth (iterate rotate row) times)))

(defn- expand-str
  "Expands string s so that its length is len, by wrapping over to the start when
  when end of string is reached, or truncates it if len is less than length of s."
  [s len]
  (letfn [(char-at [s idx] (nth s (rem idx (count s))))]
    (apply str (map (partial char-at s) (range len)))))

(def ^:private matrix
  (let [len (count alphabet)]
    (into {}
          (map #(vector %1 (into {} (map vector alphabet (rotate alphabet %2))))
               alphabet
               (range len)))))

(def ^:private inverted-matrix
  (into {} (map #(vector % (map-invert (get matrix %))) alphabet)))

(defn- encrypt [s k]
  (let [columns->rows (map vector s (expand-str k (count s)))]
    (apply str (map #(get-in matrix %) columns->rows))))

(defn- decrypt [s k]
  (let [rows->columns (map vector (expand-str k (count s)) s)]
    (apply str (map #(get-in inverted-matrix  %) rows->columns))))

(defn repeating-substr?
  [s substr]
  (let [s-len (count s)
        substr-len (count substr)]
    (if (< s-len substr-len)
      ;; This means that original string may only be a part of repeating substr.
      (.contains substr s)
      (let [s1 (.substring s 0 substr-len)]
        (if (= s1 substr)
          (repeating-substr? (.substring s substr-len) substr)
          false)))))

(defn shortest-repeating-substr
  [s]
  (let [substrs (map #(.substring s 0 %) (range 1 (count s)))]
    (first (drop-while #(not (repeating-substr? s %)) substrs))))

(defn encode [keyword message]
  (encrypt message keyword))

(defn decode [keyword message]
  (decrypt message keyword))

(defn decipher [cipher message]
  (shortest-repeating-substr (decrypt cipher message)))

