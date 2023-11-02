(ns functional-programming.core-test
  (:require [clojure.test :refer :all]
            [functional-programming.core :refer :all]))

(deftest test-translate-codon-to-protein
  (is (= "Methionine" (translate_codon_to_protein "AUG")))
  (is (= "Phenylalanine" (translate_codon_to_protein "UUU")))
  (is (= "Phenylalanine" (translate_codon_to_protein "UUC")))
  (is (= "Leucine" (translate_codon_to_protein "UUA")))
  (is (= "Leucine" (translate_codon_to_protein "UUG")))
  (is (= "Serine" (translate_codon_to_protein "UCU")))
  (is (= "Serine" (translate_codon_to_protein "UCC")))
  (is (= "Serine" (translate_codon_to_protein "UCA")))
  (is (= "Serine" (translate_codon_to_protein "UCG")))
  (is (= "Tyrosine" (translate_codon_to_protein "UAU")))
  (is (= "Tyrosine" (translate_codon_to_protein "UAC")))
  (is (= "Cysteine" (translate_codon_to_protein "UGU")))
  (is (= "Cysteine" (translate_codon_to_protein "UGC")))
  (is (= "Tryptophan" (translate_codon_to_protein "UGG")))
  (is (= "STOP" (translate_codon_to_protein "UAA")))
  (is (= "STOP" (translate_codon_to_protein "UAG")))
  (is (= "STOP" (translate_codon_to_protein "UGA")))
  (is true (instance? String  (translate_codon_to_protein "AUG")))
  (is (= nil (translate_codon_to_protein 1))))
;do i want to test extra data types here?