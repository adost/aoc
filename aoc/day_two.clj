(ns aoc.day-two
  (:require
   [clojure.data.csv :as csv]
   [clojure.java.io :as io]
   [clojure.string :as str]))

(defn- is-password-valid
  [min max letter password]
  (let [letters (frequencies (str/split password #""))
        val (get letters letter)]
    (if (not val)
      false
      (and (<= min val) (>= max val)))))

(defn- is-password-valid-part2
  [pos1 pos2 letter password]
  (let [lower (subs password (- pos1 1) pos1)
        upper (subs password (- pos2 1) pos2)]
    (or (and (= lower letter)
             (not (= upper letter)))
        (and (= upper letter)
             (not (= lower letter))))))

(defn- count-valid-passwords
  [data amt fun]
  (if-let [item (first data)]
    (let [[min-max letter password] (str/split (first item) #"\s")
          [min max] (str/split min-max #"-")
          letter (str/replace letter #":" "")]
      (if (fun (Integer/parseInt min) (Integer/parseInt max) letter password)
        (count-valid-passwords (rest data) (inc amt) fun)
        (count-valid-passwords (rest data) amt fun)))
    amt))

(defn valid-passwords-count
  []
  (let [data (with-open [reader (io/reader "passwords.csv")]
               (doall
                (csv/read-csv reader)))]
    (count-valid-passwords data 0 is-password-valid)))

(defn valid-passwords-count-part2
  []
  (let [data (with-open [reader (io/reader "passwords.csv")]
               (doall
                (csv/read-csv reader)))]
    (count-valid-passwords data 0 is-password-valid-part2)))
