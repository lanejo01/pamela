;; Copyright © 2016 Dynamic Object Language Labs Inc.
;;
;; This software is licensed under the terms of the
;; Apache License, Version 2.0 which can be found in
;; the file LICENSE at the root of this distribution.

;;; Acknowledgement and Disclaimer:
;;; This material is based upon work supported by the Army Contracting
;;; and DARPA under contract No. W911NF-15-C-0005.
;;; Any opinions, findings and conclusions or recommendations expressed
;;; in this material are those of the author(s) and do necessarily reflect the
;;; views of the Army Contracting Command and DARPA.

(ns pamela-grammar.parser
  "The PAMELA parser."
  (:require [clojure.java.io :refer :all]
            [clojure.string :as string]
            [clojure.pprint :as pp :refer :all]
            [instaparse.core :as insta]
            [instaparse.viz :as iv]
            [environ.core :refer [env]])
  )

(defn build-parser []
  (let [ebnf (slurp (resource "data/pamela.ebnf"))
        whitespace (insta/parser "whitespace = #'([,\\s]+|;.*\\n)+'")
        parser (insta/parser ebnf
                             :input-format :ebnf
                             :auto-whitespace whitespace)]
    parser))

(defn make-args [args]
  {:args (if args
           (into [] args)
           [])})

(defn make-cond-map [& cmap]
  ;(println "condition map" cmap)
  {:cond-map (into {} cmap)})

(defn make-method [name & rest-as-map]
  ;(println "make-method" name)
  (let [all (apply merge rest-as-map)
        ;_ (pp/pprint all)
        cond-map (:cond-map all)
        all (dissoc all :cond-map)
        all (merge all cond-map)
        all (merge all {:temporal-constraints []})
        ]
    {name all}))

(defn make-field [name & rest]
  ;(println "make-field" name rest)
  {name {:todo 'todo}})

(defn make-option [option]
  ;(println "make-option")
  ;(pprint option)
  {(first option) (apply merge (rest option))})

(defn make-pclass [name args & options]
  ;(println "\n--------make-pclass" name args "options:")
  ;(pprint options)
  ;(println)
  {name (merge args (apply merge options))}
  )

(defn make-body [& body]
  (if body
    {:body body}))

(defn check-transform [tree]
  ;(pprint tree)
  (let [
        transformed (insta/transform {
                                      :symbol     symbol
                                      :keyword    keyword
                                      :args       (fn [& rest]
                                                    (make-args rest))

                                      :cond-map   make-cond-map
                                      :defpmethod make-method
                                      :field      make-field
                                      :option     make-option
                                      :defpclass  make-pclass
                                      :fn         make-body
                                      :pamela     (fn [& rest]
                                                    (apply merge rest))
                                      } tree)
        ]
    (println "transformed: -------")
    (pp/pprint transformed)
    (println "-------")
    )
  )

(defn test-grammar [parser src results-dir]
  (let [testname (string/replace (.getName src) ".pamela" "")
        _ (println "Checking grammar for:" (.getName src))
        tree (insta/parses parser (slurp src))]
    (if (insta/failure? tree)
      (do
        (pp/pprint (insta/get-failure tree))
        false)
      (let [txt (as-file (str (.getPath results-dir) "/" testname ".txt"))
            png (as-file (str (.getPath results-dir) "/" testname ".png"))
            clj (as-file (str (.getPath results-dir) "/" testname ".clj"))]
        (if (not= 1 (count tree))
          (do
            (println "  Failure! ambiguous parse tree in: " (.getPath txt))
            (spit txt (with-out-str (pp/pprint tree)))
            (spit clj (with-out-str (pp/pprint tree)))
            false)
          (do
            (println "  Success! parse tree in: " (.getPath txt))
            (spit txt (with-out-str (pp/pprint (first tree))))
            (spit clj (with-out-str (pp/pprint (first tree))))

            (check-transform tree)
            (println "  parse diagram in: " (.getPath png))
            ;; (println "  type: " (iv/tree-type tree))
            ;; NOTE visuzlize does NOT recur correctly :(
            ;; must add fake-root ourselves
            ;; https://github.com/Engelberg/instaparse/issues/138
            (insta/visualize (iv/fake-root tree)
                             :output-file png :options {:dpi 72})
            true))))))

(defn pamela-filename? [filename]
  (string/ends-with? filename ".pamela"))

(defn test-grammar-all []
  (let [regression (as-file "../src/test/pamela/regression")
        srcs (and (.exists regression) (.listFiles regression))
        analysis (as-file (str (:user-dir env) "/target/analysis"))
        parser (build-parser)]
    (when srcs
      (if-not (.exists analysis)
        (.mkdirs analysis))
      (doseq [src (sort srcs)]
        (if (pamela-filename? src)
          (test-grammar parser src analysis))))))

(defn test-grammar-one [filename]
  (let [file (as-file filename)
        cwd (or (:pamela-cwd env) (:user-dir env))
        file (if (.exists file) file
                                (as-file (str cwd "/" filename)))]
    (if-not (.exists file)
      (println "ERROR: input file does not exist: " filename)
      (let [parser (build-parser)]
        (test-grammar parser file (.getParentFile file))))))

(defn exit
  "Exit PAMELA grammar with given status code (and optional messages)."
  {:added "0.2.0"}
  [status & msgs]
  (if msgs
    (binding [*out* *err*]
      (println (string/join \newline msgs))))
  (flush)                                                   ;; ensure all pending output has been flushed
  (shutdown-agents)
  (System/exit status))

(defn -main
  "PAMELA grammar entry point"
  {:added "0.2.0"}
  [& args]
  (try
    (case (count args)
      0 (test-grammar-all)
      1 (if-not (test-grammar-one (first args)) (exit 1 "Failure!"))
      (println "analyze: usage analyze [pamela-file]"))
    (catch Throwable e                                      ;; note AssertionError not derived from Exception
      (exit 1 "ERROR caught exception:" (.getMessage e))))
  (exit 0))
