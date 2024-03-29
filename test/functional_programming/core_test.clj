(ns functional-programming.core-test
  (:require [clojure.test :refer :all]
            [functional-programming.core :refer :all])
  (:import (clojure.lang LazySeq)))

;(ACTUAL, EXPECTED)
;Task One Tests
(deftest test-convert-trinary-to-decimal-recursive
  ;Tests can be validated using the trinary to decimal calculator here https://www.epochconverter.com/numbers/bin-oct-dec-hex
  (testing "Trinary Conversions tests")
    (is (= 0 (convert-trinary-to-decimal-recursive "0")))
    (is (= 1 (convert-trinary-to-decimal-recursive "1")))
    (is (= 3 (convert-trinary-to-decimal-recursive "10")))
    (is (= 14 (convert-trinary-to-decimal-recursive "112")))
    (is (= 563 (convert-trinary-to-decimal-recursive "202212")))
    (is (= 32091 (convert-trinary-to-decimal-recursive "1122000120"))
    (is (= true (int? (convert-trinary-to-decimal-recursive "1122000120")))))

  (testing "Invalid Inputs")
  (is (= 0 (convert-trinary-to-decimal-recursive "   ")))
  (is (= 0 (convert-trinary-to-decimal-recursive "")))
  (is (= 0 (convert-trinary-to-decimal-recursive "333")))
  (is (= 0 (convert-trinary-to-decimal-recursive "4")))
  (is (= 0 (convert-trinary-to-decimal-recursive "5")))
  (is (= 0 (convert-trinary-to-decimal-recursive "6")))
  (is (= 0 (convert-trinary-to-decimal-recursive "7")))
  (is (= 0 (convert-trinary-to-decimal-recursive "8")))
  (is (= 0 (convert-trinary-to-decimal-recursive "9")))
  (is (= 0 (convert-trinary-to-decimal-recursive "11220080120")))
  (is (= 0 (convert-trinary-to-decimal-recursive "abc")))
  (is (= 0 (convert-trinary-to-decimal-recursive "v")))
  (is (= 0 (convert-trinary-to-decimal 1)))
  (is (= 0 (convert-trinary-to-decimal 1.0)))
  (is (= 0 (convert-trinary-to-decimal 1/3)))
  (is (= 0 (convert-trinary-to-decimal [1 2 3]))))

(deftest test-convert-trinary-to-decimal
  ;Tests can be validated using the trinary to decimal calculator here https://www.epochconverter.com/numbers/bin-oct-dec-hex
  (testing "Trinary Conversions tests")
  (is (= 0 (convert-trinary-to-decimal "0")))
  (is (= 1 (convert-trinary-to-decimal "1")))
  (is (= 3 (convert-trinary-to-decimal "10")))
  (is (= 14 (convert-trinary-to-decimal "112")))
  (is (= 563 (convert-trinary-to-decimal "202212")))
  (is (= 32091 (convert-trinary-to-decimal "1122000120"))
      (is (= true (int? (convert-trinary-to-decimal "1122000120")))))
  (testing "Invalid Inputs")
  ;invalid trinary inputs should always return 0
  (is (= 0 (convert-trinary-to-decimal "   ")))
  (is (= 0 (convert-trinary-to-decimal "")))
  (is (= 0 (convert-trinary-to-decimal "333")))
  (is (= 0 (convert-trinary-to-decimal "4")))
  (is (= 0 (convert-trinary-to-decimal "5")))
  (is (= 0 (convert-trinary-to-decimal "6")))
  (is (= 0 (convert-trinary-to-decimal "7")))
  (is (= 0 (convert-trinary-to-decimal "8")))
  (is (= 0 (convert-trinary-to-decimal "9")))
  (is (= 0 (convert-trinary-to-decimal "11220080120")))
  (is (= 0 (convert-trinary-to-decimal "abc")))
  (is (= 0 (convert-trinary-to-decimal "v")))
  (is (= 0 (convert-trinary-to-decimal 1)))
  (is (= 0 (convert-trinary-to-decimal 1.0)))
  (is (= 0 (convert-trinary-to-decimal 1/3)))
  (is (= 0 (convert-trinary-to-decimal [1 2 3])))
  )



;Task Two Tests
(deftest test-translate-codon-to-protein
  (testing "Valid Conversions")
  (is (= "Methionine" (convert-codon-to-protein "AUG")))
  (is (= "Phenylalanine" (convert-codon-to-protein "UUU")))
  (is (= "Phenylalanine" (convert-codon-to-protein "UUC")))
  (is (= "Leucine" (convert-codon-to-protein "UUA")))
  (is (= "Leucine" (convert-codon-to-protein "UUG")))
  (is (= "Serine" (convert-codon-to-protein "UCU")))
  (is (= "Serine" (convert-codon-to-protein "UCC")))
  (is (= "Serine" (convert-codon-to-protein "UCA")))
  (is (= "Serine" (convert-codon-to-protein "UCG")))
  (is (= "Tyrosine" (convert-codon-to-protein "UAU")))
  (is (= "Tyrosine" (convert-codon-to-protein "UAC")))
  (is (= "Cysteine" (convert-codon-to-protein "UGU")))
  (is (= "Cysteine" (convert-codon-to-protein "UGC")))
  (is (= "Tryptophan" (convert-codon-to-protein "UGG")))
  (is (= "STOP" (convert-codon-to-protein "UAA")))
  (is (= "STOP" (convert-codon-to-protein "UAG")))
  (is (= "STOP" (convert-codon-to-protein "UGA")))
  (is true (instance? String  (convert-codon-to-protein "AUG")))
  (is (= nil (convert-codon-to-protein 1))))

(deftest test_convert-rna-sequence-to-amino-acids
  (testing "Simple Conversions")
  ;Valid conversions should always return a lazy sequence of proteins
  (is (= '("Methionine") (convert-rna-sequence-to-amino-acids "AUG")))
  (is (= '("Phenylalanine") (convert-rna-sequence-to-amino-acids "UUU")))
  (is (= '("Phenylalanine") (convert-rna-sequence-to-amino-acids "UUC")))
  (is (= '("Leucine") (convert-rna-sequence-to-amino-acids "UUA")))
  (is (= '("Leucine") (convert-rna-sequence-to-amino-acids "UUG")))
  (is (= '("Serine") (convert-rna-sequence-to-amino-acids "UCU")))
  (is (= '("Serine") (convert-rna-sequence-to-amino-acids "UCC")))
  (is (= '("Serine") (convert-rna-sequence-to-amino-acids "UCA")))
  (is (= '("Serine") (convert-rna-sequence-to-amino-acids "UCG")))
  (is (= '("Tyrosine") (convert-rna-sequence-to-amino-acids "UAU")))
  (is (= '("Tyrosine")(convert-rna-sequence-to-amino-acids "UAC")))
  (is (= '("Cysteine") (convert-rna-sequence-to-amino-acids "UGU")))
  (is (= '("Cysteine") (convert-rna-sequence-to-amino-acids "UGC")))
  (is (= '("Tryptophan") (convert-rna-sequence-to-amino-acids "UGG")))
  (is (= '() (convert-rna-sequence-to-amino-acids "UAA")))
  (is (= '() (convert-rna-sequence-to-amino-acids "UAG")))
  (is (= '() (convert-rna-sequence-to-amino-acids "UGA")))
  (testing "Complex Proteins")
  (is (= '("Methionine" "Phenylalanine" "Tryptophan") (convert-rna-sequence-to-amino-acids "AUGUUUUGG")))
  (is (= '() (convert-rna-sequence-to-amino-acids "UAGUGG")))
  (is (= '("Tryptophan") (convert-rna-sequence-to-amino-acids "UGGUAG")))
  (is (= '("Methionine" "Phenylalanine") (convert-rna-sequence-to-amino-acids "AUGUUUUAA")))
  (is (= '("Tryptophan") (convert-rna-sequence-to-amino-acids "UGGUAGUGG")))
  (testing "Will Return A Lazy Sequence")
  (is true (instance? LazySeq (convert-rna-sequence-to-amino-acids "AUG")))
  (is true (instance? LazySeq (convert-rna-sequence-to-amino-acids "UAA")))
  (is true (instance? LazySeq (convert-rna-sequence-to-amino-acids "AUGUUUUGG")))
  (testing "Invalid Types")
  ;should always return a empty lazy seq
  (is (= '() (convert-rna-sequence-to-amino-acids 1)))
  (is (= '() (convert-rna-sequence-to-amino-acids 1.000)))
  (is (= '() (convert-rna-sequence-to-amino-acids 'a')))
  (is (= '() (convert-rna-sequence-to-amino-acids (vector [1 2 3 4]))))
  (is (= '() (convert-rna-sequence-to-amino-acids "hdbshdbdshdbshdsbhdsbsdhdsbhdsbh")))
  )

(deftest test_convert-rna-sequence-to-amino-acids
  (testing "Simple Conversions")
  ;Valid conversions should always return a lazy sequence of proteins
  (is (= '("Methionine") (convert-rna-sequence-to-amino-acids2 "AUG")))
  (is (= '("Phenylalanine") (convert-rna-sequence-to-amino-acids2 "UUU")))
  (is (= '("Phenylalanine") (convert-rna-sequence-to-amino-acids2 "UUC")))
  (is (= '("Leucine") (convert-rna-sequence-to-amino-acids2 "UUA")))
  (is (= '("Leucine") (convert-rna-sequence-to-amino-acids2 "UUG")))
  (is (= '("Serine") (convert-rna-sequence-to-amino-acids2 "UCU")))
  (is (= '("Serine") (convert-rna-sequence-to-amino-acids2 "UCC")))
  (is (= '("Serine") (convert-rna-sequence-to-amino-acids2 "UCA")))
  (is (= '("Serine") (convert-rna-sequence-to-amino-acids2 "UCG")))
  (is (= '("Tyrosine") (convert-rna-sequence-to-amino-acids2 "UAU")))
  (is (= '("Tyrosine")(convert-rna-sequence-to-amino-acids2 "UAC")))
  (is (= '("Cysteine") (convert-rna-sequence-to-amino-acids2 "UGU")))
  (is (= '("Cysteine") (convert-rna-sequence-to-amino-acids2 "UGC")))
  (is (= '("Tryptophan") (convert-rna-sequence-to-amino-acids2 "UGG")))
  (is (= '() (convert-rna-sequence-to-amino-acids2 "UAA")))
  (is (= '() (convert-rna-sequence-to-amino-acids2 "UAG")))
  (is (= '() (convert-rna-sequence-to-amino-acids2 "UGA")))
  (testing "Complex Proteins")
  (is (= '("Methionine" "Phenylalanine" "Tryptophan") (convert-rna-sequence-to-amino-acids2 "AUGUUUUGG")))
  (is (= '() (convert-rna-sequence-to-amino-acids2 "UAGUGG")))
  (is (= '("Tryptophan") (convert-rna-sequence-to-amino-acids2 "UGGUAG")))
  (is (= '("Methionine" "Phenylalanine") (convert-rna-sequence-to-amino-acids2 "AUGUUUUAA")))
  (is (= '("Tryptophan") (convert-rna-sequence-to-amino-acids2 "UGGUAGUGG")))
  (testing "Will Return A Lazy Sequence")
  (is true (instance? LazySeq (convert-rna-sequence-to-amino-acids2 "AUG")))
  (is true (instance? LazySeq (convert-rna-sequence-to-amino-acids2 "UAA")))
  (is true (instance? LazySeq (convert-rna-sequence-to-amino-acids2 "AUGUUUUGG")))
  (testing "Invalid Types")
  ;should always return a empty lazy seq
  (is (= '() (convert-rna-sequence-to-amino-acids2 1)))
  (is (= '() (convert-rna-sequence-to-amino-acids2 1.000)))
  (is (= '() (convert-rna-sequence-to-amino-acids2 'a')))
  (is (= '() (convert-rna-sequence-to-amino-acids2 (vector [1 2 3 4]))))
  (is (= '() (convert-rna-sequence-to-amino-acids2 "hdbshdbdshdbshdsbhdsbsdhdsbhdsbh")))
  )

(deftest test-read-input
  (testing "Valid JSON file"
    (is (not (nil? (read-input "testData.json")))))
  (testing "Invalid Json File"
    ;i couldnt find a way to do this but have specs in the main file to handle this
    )
  )

(deftest test-most-individual-meteor-falls-in-year
  (testing "Most individual falls using test data"
    (is (= (most-individual-meteor-falls-in-year "testData.json") ["1949" 4])))
  (testing "Most individual falls using real data"
    (is (= (most-individual-meteor-falls-in-year "nasa.json") ["1933" 16])))
  (testing "Function returns a vector using test data"
    (is (most-individual-meteor-falls-in-year "testData.json") vector?))
  (testing "Function returns a vector using real data"
    (is (most-individual-meteor-falls-in-year "nasa.json") vector?)))

(deftest test-heaviest-collective-fall
  (testing "Most heaviest collective falling using test data"
    (is (= (heaviest-collective-fall "testData.json") ["1981" 112000.0])))
  (testing "Most heaviest collective falling using real data"
    (is (= (heaviest-collective-fall "nasa.json") ["1947" 2.303023E7])))
  (testing "Function returns a vector using test data"
    (is (heaviest-collective-fall "testData.json") vector?))
  (testing "Function returns a vector using real data"
    (is (heaviest-collective-fall "nasa.json") vector?)))
(deftest test-years-between-first-and-last
  (testing "The amount of years between earilest recording and latest using test data"
    (is (= (years-between-first-and-last "testData.json") 257)))
  (testing "The amount of years between earilest recording and latest using real data"
    (is (= (years-between-first-and-last "nasa.json") 1152)))
  (testing "Function returns a int using test data"
    (is (years-between-first-and-last "testData.json") int?))
  (testing "Function returns a int using real data"
    (is (years-between-first-and-last "nasa.json") int?)))

(deftest test-closest-meteorite-fall-to-cantor
  (testing "Which metorite fell closest to cantor using the real data "
    (is (= (closest-meteorite-fall-to-cantor "nasa.json") ["Appley Bridge" 85.8277310701543])))
  (testing "Which metorite fell closest to cantor using the test data "
    (is (= (closest-meteorite-fall-to-cantor "testData.json") ["Fake-Collegiate-Destroyer" 1.788606846923771]))) ;Oh no!
  (testing "Function returns a vector using test data"
    (is (closest-meteorite-fall-to-cantor "testData.json") vector?))
  (testing "Function returns a vector using real data"
    (is (closest-meteorite-fall-to-cantor "nasa.json") vector?))
  )

(deftest test-closest-meteorite-fall-to-cantor
  (testing "What is the address of the metorite that fell closest to cantor using the real data "
    (is (= (closest-meteorite-fall-to-cantor-address "nasa.json") "77 Skull House Ln, Appley Bridge, Wigan WN6 9DJ, UK")))
  (testing "What is the address of the metorite that fell closest to cantor using the test data "
    (is (= (closest-meteorite-fall-to-cantor-address "testData.json") "College House, Broomhall, Sheffield S10 2BP, UK"))) ;Oh no!
  (testing "Function returns a vector using test data"
    (is (closest-meteorite-fall-to-cantor-address "testData.json") string?))
  (testing "Function returns a vector using real data"
    (is (closest-meteorite-fall-to-cantor-address "nasa.json") string?))
  )

(deftest test-most-collective-mass-in-decades-with-frequency
  (testing "Most heaviest collective falling in decade using test data"
    (is (= (most-collective-mass-in-decades-with-frequency "testData.json") {:average-mass 37333.333333333336
                                                                             :frequency    3
                                                                             :year         1980})))
  (testing "Most heaviest collective falling in decade using real data"
    (is (= (most-collective-mass-in-decades-with-frequency "nasa.json") {:average-mass 465829.28301886795
                                                                         :frequency    53
                                                                         :year         1940})))
  (testing "Function returns a vector using test data"
    (is (most-collective-mass-in-decades-with-frequency "testData.json") map?))
  (testing "Function returns a vector using real data"
    (is (most-collective-mass-in-decades-with-frequency "nasa.json") map?))
  )