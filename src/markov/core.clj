(ns markov.core
  (:gen-class)
  (:require [markov.markov :as mkv]))

(def stock-market-chain
  {:bull {:bull 900/1000
          :bear 75/1000
          :stag 25/1000}
   :bear {:bull 15/100
          :bear 80/100
          :stag 5/100}
   :stag {:bull 1/4
          :bear 1/4
          :stag 1/2}})

(def market
  (atom {:state :stag}))

(def keywords
  (list :a :b :c :d :e :f :g :h :i :j :k :l :m
        :n :o :p :q :r :s :t :u :v :w :x :y :z))

(def letters
  (list \a \b \c \d \e \f \g \h \i \j \k \l \m
        \n \o \p \q \r \s \t \u \v \w \x \y \z "\n"))

(def sentences
  (clojure.string/split (slurp "resources/file.txt") #"[ ]"))

(defn test-sentences
  [n]
  (mkv/start-chain! (atom {:state (rand-nth sentences)}) (mkv/make-chain sentences) n))

(defn -main
  [& args]
  (print "1. ")(println (test-sentences 10))
  (print "2. ")(println (test-sentences 10))
  (print "3. ")(println (test-sentences 10)))
