(ns functional-programming.core-test
  (:require [clojure.test :refer :all]
            [functional-programming.core :refer :all])
  (:import (clojure.lang LazySeq)))

;(ACTUAL, EXPECTED)
;Task One Tests
(deftest test-trinary-to-decimal
  ;Tests can be validated using the trinary to decimal calculator here https://www.epochconverter.com/numbers/bin-oct-dec-hex
  (testing "Trinary Conversions tests")
    (is (= 0 (trinary-to-decimal "0")))
    (is (= 1 (trinary-to-decimal "1")))
    (is (= 3 (trinary-to-decimal "10")))
    (is (= 14 (trinary-to-decimal "112")))
    (is (= 563 (trinary-to-decimal "202212")))
    (is (= 32091 (trinary-to-decimal "1122000120"))
    (is (= true (int? (trinary-to-decimal "1122000120")))))

  (testing "Invalid Inputs")
  (is (= 0 (trinary-to-decimal "   ")))
  (is (= 0 (trinary-to-decimal "")))
  (is (= 0 (trinary-to-decimal "333")))
  (is (= 0 (trinary-to-decimal "4")))
  (is (= 0 (trinary-to-decimal "5")))
  (is (= 0 (trinary-to-decimal "6")))
  (is (= 0 (trinary-to-decimal "7")))
  (is (= 0 (trinary-to-decimal "8")))
  (is (= 0 (trinary-to-decimal "9")))
  (is (= 0 (trinary-to-decimal "11220080120")))
  (is (= 0 (trinary-to-decimal "abc")))
  (is (= 0 (trinary-to-decimal "v"))))

;Task Two Tests
(deftest test-translate-codon-to-protein
  (testing "Valid Conversions")
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

(deftest test_translate_rna_to_amino_acids
  (testing "Simple Conversions")
  (is (= (translate_rna_to_amino_acids "AUG") '("Methionine")))
  (is (= (translate_rna_to_amino_acids "UUU") '("Phenylalanine")))
  (is (= (translate_rna_to_amino_acids "UUC") '("Phenylalanine")))
  (is (= (translate_rna_to_amino_acids "UUA") '("Leucine")))
  (is (= (translate_rna_to_amino_acids "UUG") '("Leucine")))
  (is (= (translate_rna_to_amino_acids "UCU") '("Serine")))
  (is (= (translate_rna_to_amino_acids "UCC") '("Serine")))
  (is (= (translate_rna_to_amino_acids "UCA") '("Serine")))
  (is (= (translate_rna_to_amino_acids "UCG") '("Serine")))
  (is (= (translate_rna_to_amino_acids "UAU") '("Tyrosine")))
  (is (= (translate_rna_to_amino_acids "UAC") '("Tyrosine")))
  (is (= (translate_rna_to_amino_acids "UGU") '("Cysteine")))
  (is (= (translate_rna_to_amino_acids "UGC") '("Cysteine")))
  (is (= (translate_rna_to_amino_acids "UGG") '("Tryptophan")))
  (is (= (translate_rna_to_amino_acids "UAA") '()))
  (is (= (translate_rna_to_amino_acids "UAG") '()))
  (is (= (translate_rna_to_amino_acids "UGA") '()))
  (testing "Will Return A Lazy Sequence")
  (is true (instance? LazySeq (translate_rna_to_amino_acids "AUG")))
  (is true (instance? LazySeq (translate_rna_to_amino_acids "UAA"))))



;do i want to test extra data types here?