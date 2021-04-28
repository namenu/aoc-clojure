(ns _4_2020
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [clojure.set :as set]))

(def hgt-regex #"^((1[5-8][0-9]|19[0-3])(cm))|^((59|6[0-9]|7[0-6])(in))$")
(def pid-regex #"^\d{9}$")
(def ecl-regex #"^(amb|blu|brn|gry|grn|hzl|oth)$")
(def hcl-regex #"^#[0-9a-z]{6}$")

(s/def ::byr (s/and int? #(>= % 1920) #(>= 2002 %)))
(s/def ::iyr (s/and int? #(>= % 2010) #(>= 2020 %)))
(s/def ::eyr (s/and int? #(>= % 2020) #(>= 2030 %)))
(s/def ::hgt (s/and string? #(re-matches hgt-regex %)))
(s/def ::hcl (s/and string? #(re-matches hcl-regex %)))
(s/def ::ecl (s/and string? #(re-matches ecl-regex %)))
(s/def ::pid (s/and string? #(re-matches pid-regex %)))
(s/def ::cid (s/nilable (s/and string?)))
(s/def ::passport (s/keys :req [::byr ::iyr ::eyr ::hgt ::hcl ::ecl ::pid]
                          :opt-un [::cid]))

(s/def ::passport-incomplete (s/keys :req-un [::byr ::iyr ::eyr ::hgt ::hcl ::ecl ::pid]
                                     :opt-un [::cid]))

(def passport-keys #{:ecl :byr :iyr :hgt :pid :hcl :eyr})
(defn keywordify [input]
  (->> input
       (map (fn [v] (let [[k v] (str/split v #":")]
                      (cond (= "byr" k) {(keyword (str *ns*) k) (Integer/parseInt v)}
                            (= "iyr" k) {(keyword (str *ns*) k) (Integer/parseInt v)}
                            (= "eyr" k) {(keyword (str *ns*) k) (Integer/parseInt v)}
                            :else {(keyword (str *ns*) k) v}))))
       (into {})))

(defn mapify [input]
  (->> input
       (map (fn [v] (let [[k v] (str/split v #":")]
                     {(keyword k) v})))
       (into {})))

(defn parse-input-incomplete [input]
  (->> (str/split (slurp input) #"\n\n")
       (map #(str/split % #"\n| "))
       (map mapify)
       (map (fn [v] (->> (keys v)
                         (into #{})
                         (set/difference passport-keys))))))

(defn parse-input [keyword input]
  (->> (str/split (slurp input) #"\n\n")
       (map #(str/split % #"\n| "))
       (map keywordify)
       (map #(s/conform keyword %))))

;part-1
(->> "src/2020_4.txt"
     (parse-input-incomplete)
     (filter #(= (count %) 0))
     (count))

;part-2
(->> "src/2020_4.txt"
     (parse-input ::passport)
     (filter #(not= ::s/invalid %))
     (count))
