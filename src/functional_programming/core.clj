(ns functional-programming.core
  (:gen-class)
  (:require [clojure.spec.alpha :as s]))

;Task One
;I have done two solutions to task one

;a reuseable function for both which checks if we pass in a string and then checks the string agaisnt our pattern
(defn is-string-matching-pattern [trinary-str]
  (if (string? trinary-str) ; Replace this with your custom predicate logic
    (re-matches #"^[0-2]+" trinary-str)
    false))

;Task one version 1
;For this i use the power of recursive. I first check the string using regex to ensure that we only have 0 1 and 2, returning 0 if not, this covers invalid types
;if the string matches then map the string to a lazy sequence of ints and reverse the collection
;we then call our recursive method making the use of Multi-arity function to split our logic into small chunks and not repeat unnessary code
;we pass in the reversed mapped lazy sequence and perform our logic on it in a recursive way.

(defn convert-trinary-to-decimal-recursive  ([trinary-str]
  (if (is-string-matching-pattern trinary-str ) ;Matches against string containing multiple characters but all must be 0, 1 or 2.
    (convert-trinary-to-decimal-recursive
      (reverse (map #(Character/getNumericValue %) trinary-str)) ;pass each character in and map each character to an int and reverse the lazy sequence.
      0 0) ;calls the recursive trinary method with a reversed lazy sequence of ints
    0)) ;returns 0 if invalid data is passed in

  ([trinary-lazy, index ,total]
   (if (= index (count trinary-lazy))  ;recursive condition, if we get past the last index return total
     total
     (recur trinary-lazy,                                   ;tail recursion to optimized stack frame so we can deal with bigger numbers if need be
            (inc index),                                    ;Increment this for the power and for recursive condition
            (int (+ total (* (nth trinary-lazy index) (Math/pow 3 index))))) ;provides calculation to the digit in index
     )))

;task one version 2
;As you can see much less code is required. some same logic is still implemented such as the pattern regex to check if the strings are matching the pattern of 0, 1 or 2
;we then use a thread last to make the code more readable, and will after the first mapping pass each value from the lazy sequence through a sequence of functions as the last argument.
;we first map the string to a lazy sequence of numbers and reverse the lazy sequence
;we then use a higher-order function map-indexed to apply our calculation to each element in the lazy sequence while also providing the index
;this way we can use clojure built in lanugage to abstract some of the code away, in the end we get a lazy sequence of results (2, 3, 9)
;to add these together we use the reduce
;we then use the higher order function reduce to add all the results together
(defn convert-trinary-to-decimal [trinary-str]
  (if (is-string-matching-pattern trinary-str) ;Matches against string containing multiple characters but all must be 0, 1 or 2.
  (->> trinary-str
       (map #(Character/getNumericValue %));code to convert string to lazy sequence of numbers code is found here https://stackoverflow.com/questions/34985202/character-to-integer-in-clojure
       (reverse)
       (map-indexed (fn [%1 %2]                             ;Returns a lazy sequence, we are doing very simular logic as above however this is doing the (inc index) for us and passing us each number in the string to apply the calculation
                      (* %2 (int (Math/pow 3 %1)))))              ;at this point each collum in the lazy seqeunce has had the logic applied and will look like (2, 3, 9)
       (reduce +))
  0)
  )

;times of each function
;this was quite surprising to me that convert-trinary-to-decimal-recursive would be faster even with more code
;but after some reflection i thought about it, and it makes complete sense as we are using less built in clojure code
;which will have complex implementations behind them.
(time (convert-trinary-to-decimal-recursive "1122000120")) ;2.93msec
(time (convert-trinary-to-decimal "1122000120")) ; 4.11msec


;;Task Two
;Here we have defined a map with only the scope of the function which will act as our main codon to protein conversion
;we use the get to return the value of the key and convert the codon string to a key using (keyword)
(defn convert-codon-to-protein[codon]
  (let [codon-to-protein-map {
                              :AUG "Methionine"
                              :UUU "Phenylalanine"
                              :UUC "Phenylalanine"
                              :UUA "Leucine"
                              :UUG "Leucine"
                              :UCU "Serine"
                              :UCC "Serine"
                              :UCA "Serine"
                              :UCG "Serine"
                              :UAU "Tyrosine"
                              :UAC "Tyrosine"
                              :UGU "Cysteine"
                              :UGC "Cysteine"
                              :UGG "Tryptophan"
                              :UAA "STOP"
                              :UAG "STOP"
                              :UGA "STOP" }]
    (get codon-to-protein-map (keyword codon)))) ;converts the string into a keyword which is used to get the value from the map

;This function checks if the protein passed in is equal to "STOP" OR Nil and returns a true or false value
(defn not-stop-not-nil? [protein]
  (if (and (not= "STOP" protein) (not= nil protein)) ; Replace this with your custom predicate logic
    true
    false))

;This function effectively splits the rna-sequence into a lazy sequence of characters, before using the map function
;to apply the function "apply str" to each col of the lazy sequence to turn the 3 characters into a codon
;we then use another map function to apply the convert-codon-to-protein to each codon in the lazy sequence
;finally we use the take-while to only return the lazy sequence which contains codons until the "Stop"
(defn convert-rna-sequence-to-amino-acids [rna-sequence]
  ;Equivalent to (map #(convert-codon-to-protein %) (map #(apply str %) (partition 3 rna)))
  (if (s/valid? string? rna-sequence)
    (let  [amino-acids ( ->> rna-sequence           ;threads the last arugmenet to each function
                      (partition 3) ;splits the rna string into a lazy sequence of characters eg (A U G) (A U G)
                      (map #(apply str %))  ;applies the fn "apply" onto the lazy sequence to create a new lazy sequence of strings ("AUG") ("AUG")
                      (map #(convert-codon-to-protein %)))] ;applies the fn convert-codon-to-protein which returns the proteins
      (take-while #(not-stop-not-nil? %) amino-acids) ;Although not a complete copy, i got this code from https://stackoverflow.com/questions/11866446/how-to-stop-iterating-a-sequence-when-a-condition-is-met and used the clojure cheatsheet to further understand how it works and apply it to this program
      ;(filter #(not-stop-not-nil? %) amino-acids) ;returns all that is specific so wouldnt work?
      )
    0
  )
)

;task 3

;Figure out how to read from a file and parse it

;1.	Which year saw the most individual meteor falls?

;2.	Which year saw the heaviest collective meteor fall?

;3.	How many years since the first recorded meteorite and the last

;4.

;5.
(defn -main []
  (let [trinary-number "112"
        decimal-equivalent (convert-trinary-to-decimal-recursive trinary-number)]
    (println (str "The decimal equivalent of " trinary-number " is " decimal-equivalent)))
  (println(convert-rna-sequence-to-amino-acids "AUGUUUUAA" ))
  )