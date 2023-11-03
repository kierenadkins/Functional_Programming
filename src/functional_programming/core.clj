(ns functional-programming.core
  (:gen-class))

;Task One
;To solve this problem i have used recurision, i first match the string using regex pattern matching.
;if false we simply return 0
;if true we call the function when we convert the trinary-str to a lazy sequence of ints and reverse the string
;we then apply the calculation recursively

(defn convert-trinary-to-decimal  ([trinary-str]
  (if (re-matches #"^[0-2]+" trinary-str) ;Matches against string containing multiple characters but all must be 0, 1 or 2.
    (convert-trinary-to-decimal
      (->> trinary-str
           (map #(Character/getNumericValue %));code to convert string to lazy sequence of numbers code is found here https://stackoverflow.com/questions/4836768/create-a-list-from-a-string-in-clojure
           (reverse))
      0 0) ;calls the recursive trinary method with a reversed vector of ints which match the string
    0))
  ([trinary-lazy, index ,total]
   (if (= index (count trinary-lazy))                         ;recursive condition, if we get past the last index return total
     total
     (recur trinary-lazy, (inc index), (int (+ total (* (nth trinary-lazy index) (Math/pow 3 index))))) ;provides calculation to the digit in index
     )))

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

;This function effectively splits the rna-sequence into a lazy sequence of characters, before using the map function
;to apply the function "apply str" to each col of the lazy sequence to turn the 3 characters into a codon
;we then use another map function to apply the convert-codon-to-protein to each codon in the lazy sequence
;finally we use the take-while to only return the lazy sequence which contains codons until the "Stop"
(defn convert-rna-sequence-to-amino-acids [rna-sequence]
  ;Equivalent to (map #(convert-codon-to-protein %) (map #(apply str %) (partition 3 rna)))
  (let  [amino-acids ( ->> rna-sequence           ;threads the last arugmenet to each function
                      (partition 3) ;splits the rna string into a lazy sequence of characters eg (A U G) (A U G)
                      (map #(apply str %))  ;applies the fn "apply" onto the lazy sequence to create a new lazy sequence of strings ("AUG") ("AUG")
                      (map #(convert-codon-to-protein %)) ;applies the fn convert-codon-to-protein which returns the proteins
                      (take-while #(not= "STOP" %))         ;Although not a complete copy, i got this code from https://stackoverflow.com/questions/11866446/how-to-stop-iterating-a-sequence-when-a-condition-is-met and used the clojure cheatsheet to further understand how it works and apply it to this program
                     )]
    amino-acids))

;task 3

;Figure out how to read from a file and parse it

;1.	Which year saw the most individual meteor falls?

;2.	Which year saw the heaviest collective meteor fall?

;3.	How many years since the first recorded meteorite and the last

;4.

;5.
(defn -main []
  (let [trinary-number "112"
        decimal-equivalent (convert-trinary-to-decimal trinary-number)]
    (println (str "The decimal equivalent of " trinary-number " is " decimal-equivalent)))
  (println(convert-rna-sequence-to-amino-acids "AUGUUUUGG" ))
  )