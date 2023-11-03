(ns functional-programming.core
  (:gen-class))

;Task One
(defn trinary-to-decimal  ([trinary-str]
  (if (re-matches #"^[0-2]+" trinary-str) ;Matches against string containing multiple characters but all must be 0, 1 or 2.
    (trinary-to-decimal
      (->> trinary-str
           (map #(Character/getNumericValue %));code to convert string to numbers is found here https://stackoverflow.com/questions/4836768/create-a-list-from-a-string-in-clojure
           (reverse))
      0 0) ;calls the recursive trinary method with a reversed vector of ints which match the string
    0))
  ([trinary-str, index ,total]
   (if (= index (count trinary-str))                         ;recursive condition, if we get past the last index return total
     total
     (recur trinary-str, (inc index), (int (+ total (* (nth trinary-str index) (Math/pow 3 index))))) ;provides calculation to the digit in index
     )))

;;Task Two

(defn translate_codon_to_protein[codon]
  (let [codon-to-protein-map {:AUG "Methionine" :UUU "Phenylalanine" :UUC "Phenylalanine" :UUA "Leucine" :UUG "Leucine" :UCU "Serine" :UCC "Serine" :UCA "Serine" :UCG "Serine" :UAU "Tyrosine" :UAC "Tyrosine" :UGU "Cysteine" :UGC "Cysteine" :UGG "Tryptophan" :UAA "STOP" :UAG "STOP" :UGA "STOP" }]
    (get codon-to-protein-map (keyword codon)))) ;converts the string into a keyword which is used to get the value from the map

(defn translate_rna_to_amino_acids [rna]
  ;Equivalent to (map #(translate_codon_to_protein %) (map #(apply str %) (partition 3 rna)))
  (let  [result ( ->> rna           ;threads the last arugmenet to each function
                      (partition 3) ;splits the rna string into a lazy sequence of characters eg (A U G) (A U G)
                      (map #(apply str %))  ;applies the fn "apply" onto the lazy sequence to create a new lazy sequence of strings ("AUG") ("AUG")
                      (map #(translate_codon_to_protein %)) ;applies the fn translate_codon_to_protein which returns the proteins
                      (take-while #(not= % "STOP"))         ;Although not a complete copy, i got this code from https://stackoverflow.com/questions/11866446/how-to-stop-iterating-a-sequence-when-a-condition-is-met and used the clojure cheatsheet to further understand how it works and apply it to this program
                      )]
    result
  )
  )
;Is there a better way instead of an if statement?
;is there a way i can do a similar thing within the thread?

(defn -main []
  (let [trinary-number "112"
        decimal-equivalent (trinary-to-decimal trinary-number)]
    (println (str "The decimal equivalent of " trinary-number " is " decimal-equivalent)))
  (println(translate_rna_to_amino_acids "AUGUUUUGG" ))
  )