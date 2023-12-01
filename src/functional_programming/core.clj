(ns functional-programming.core
  (:gen-class)
  (:require [clojure.spec.test.alpha :as stest])
  (:require [clojure.spec.alpha :as s])
  (:require [clojure.data.json :as json]))

;For the first two tasks i have done two different approaches, this was mainly for me to get a understanding of the content and to talk about
;in task 2, i also wanted to see which were more effective

;Task One
;I have done two solutions to task one

;a reusable function for both which checks if we pass in a string and then checks the string agaisnt our pattern [retired]
(s/def ::trinary-matches-pattern (s/and #(string? %) #(re-matches #"^[0-2]+" %)))

;Task one version 1
;For this i use the power of recursive. I first check the string using regex to ensure that we only have 0 1 and 2, returning 0 if not, this covers invalid types
;if the string matches then map the string to a lazy sequence of ints and reverse the collection
;we then call our recursive method making the use of Multi-arity function to split our logic into small chunks and not repeat unnessary code
;we pass in the reversed mapped lazy sequence and perform our logic on it in a recursive way.

(defn convert-trinary-to-decimal-recursive  ([trinary-str]
  (if (s/valid? ::trinary-matches-pattern trinary-str) ;Matches against string containing multiple characters but all must be 0, 1 or 2.
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
  (if (s/valid? ::trinary-matches-pattern trinary-str) ;Matches against string containing multiple characters but all must be 0, 1 or 2.
  (->> trinary-str
       (map (fn [param1] (Character/getNumericValue param1)));code to convert string to lazy sequence of numbers code is found here https://stackoverflow.com/questions/34985202/character-to-integer-in-clojure
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
;originally this was an case however i thought a map would fit this situation more, plus after some testing it was slighly more efficient to use a map
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

;specifies a protein that is not equal to "STOP" or nil
(s/def ::protein-not-stop-not-nil? (s/and #(not= "STOP" %) #(not= nil %)))
(s/def ::rna-sequence-string? string?)

;This function effectively splits the rna-sequence into a lazy sequence of characters, before using the map function
;to apply the function "apply str" to each col of the lazy sequence to turn the 3 characters into a codon
;we then use another map function to apply the convert-codon-to-protein to each codon in the lazy sequence
;finally we use the take-while to only return the lazy sequence which contains codons until the "Stop"
;I used this https://stackoverflow.com/questions/38633649/how-to-split-string-at-fixed-numbers-of-character-in-clojure to figure out how to split the string and apply string

(defn convert-rna-sequence-to-amino-acids [rna-sequence]
  ;Equivalent to (map #(convert-codon-to-protein %) (map #(apply str %) (partition 3 rna)))
  (if (s/valid? ::rna-sequence-string? rna-sequence)
    (let  [amino-acids (->> rna-sequence                    ;threads the last arugmenet to each function
                            (partition 3)                   ;splits the rna string into a lazy sequence of characters eg (A U G) (A U G)
                            (map (fn [char-sequence]        ;sets up a anonymous functions to loop around each lazy col of chars
                                   (convert-codon-to-protein (apply str char-sequence)))) ;we then apply the str to the col and pass into our convert-codon-to-protein function
                                   )]
      (take-while #(s/valid? ::protein-not-stop-not-nil? %) amino-acids) ;Although not a complete copy, i got this code "Take-While" from https://stackoverflow.com/questions/11866446/how-to-stop-iterating-a-sequence-when-a-condition-is-met and used the clojure cheatsheet to further understand how it works and apply it to this program alongside adding spec/5
      )
    '()                                                     ; this may not be the best way of returning a empty sequence
    )
  )

;Task 2 version 2
;I wanted to explore the use of looping and recuring.
(defn convert-rna-sequence-to-amino-acids2 [rna-sequence]
  (if (s/valid? ::rna-sequence-string? rna-sequence)         ; uses spec to check if the input in a valid string
    (let [rna-sequence (map #(apply str %) (partition 3 rna-sequence))] ;define our scope, and convert the rna into a lazyseq of codons of three characters which are converted into a string
      (loop [index 0 proteins []]                           ;we not loop though each column of the sequence and store the protein
         (if (= index (count rna-sequence))          ;Stop condition if the index becomes larger than our sequence
           (lazy-seq proteins)                              ;If true return a lazyseq of our proteins
             (let [protein (convert-codon-to-protein (nth rna-sequence index))] ;Use a let to define a new scope which calls our function with the protein in index x, this scope will then handle validation and recursion
                (if (s/valid? ::protein-not-stop-not-nil? protein) ;checks if the protein is "STOP" or "Nil" (Nil is incase we get an invalid codon)
                  (recur (inc index) (conj proteins (convert-codon-to-protein (nth rna-sequence index)))) ; if valid we can recur, by increasing index and adding protein to proteins
                  (lazy-seq proteins))))))))                ;if stop or nil we can return the sequence


; i wanted to see if the use of recursion always made it more effective, however in this example we can clearly see that recursion
;is much less effective than the initial solution. However, this could be because i have implemented very similar logic that the above
(time (convert-rna-sequence-to-amino-acids "AUGUUUUAA")) ;0.68msec
(time (convert-rna-sequence-to-amino-acids2 "AUGUUUUAA")) ; 1.7 msec

;task 3

(s/def ::path-ends-with-json-and-is-string? (s/and #(string? %) #(.endsWith % ".json")))
;https://stackoverflow.com/questions/45854023/add-values-of-similar-json
;Figure out how to read from a file and parse it
(defn read-input [path]
  {:pre [(s/valid? ::path-ends-with-json-and-is-string? path)]}
  (try
    (json/read-str (slurp path)
                   :key-fn keyword
                   :value-fn (fn [k v]
                               (if (= k :date)
                                 (java.sql.Date/valueOf v)
                                 v)))
    (catch Exception e
      (println "Exception caught:" (.getMessage e)))))

;https://stackoverflow.com/questions/18125045/clojure-parse-string-to-date
(defn format-date [year]
    (when year                                              ;Ensures date is not nil
      (.format
        (java.text.SimpleDateFormat. "yyyy")
        (.parse
          (java.text.SimpleDateFormat. "yyyy-MM-dd'T'HH:mm:ss.SSS") year))))

(defn format-mass [mass]
  (when mass                                              ;Ensures date is not nil
    (Double/parseDouble mass)))

;1.	Which year saw the most individual meteor falls?
(defn most-individual-meteor-falls-in-year [path]
(->> (read-input path)
     (map #(format-date (get % :year)))                     ;coverts into lazy sequence of years
     (frequencies)                                          ;converts back into a map using the year as a key and the amount of times it has appeared as the value
     (apply max-key val)                                    ;We then apply max-key to find the key with the highest appearance
     ))

(s/fdef most-individual-meteor-falls-in-year
        :args (s/cat :path ::path-ends-with-json-and-is-string?)
        :ret vector?)
(stest/instrument 'heaviest-collective-fall )

;2.	Which year saw the heaviest collective meteor fall? Continue on this
(s/def ::year-keyword-checks (s/and #(string? %) #(not= nil %)))
(s/def ::mass-keyword-checks (s/and #(string? %) #(not= nil %)))
(defn heaviest-collective-fall [path]
  (let [data (read-input path)
        yearsmass (->> data
                       (filter #(and (s/valid? ::year-keyword-checks (:year %)) (s/valid? ::mass-keyword-checks (:mass %)))) ;Filter all records that dont have both a year and a mass
                       (map #(update % :year format-date))  ;format date into a year
                       (map #(update % :mass format-mass))  ;format mass into a double
                       (reduce (fn [m d]                    ;reduce into a new map and go through each col of the previous map
                       (if (find m (:year d))     ;checks if the year from the col is in the new map
                        (assoc m (:year d) (+ (get m (:year d)) (:mass d))) ;if so then we replace the value with the same year but with a combination of both masses
                        (assoc m (:year d) (:mass d)))) ;add the data to the new map
                        {})
                       (sort-by val)                        ;sort by the value on each key
                       )]
    (last yearsmass)))                                     ;return last result as that will be the biggest

(s/fdef heaviest-collective-fall
        :args (s/cat :path ::path-ends-with-json-and-is-string?)
        :ret vector?)
(stest/instrument 'heaviest-collective-fall )

;3.	How many years since the first recorded meteorite and the last
(defn years-between-first-and-last [path]
  (let [years (->> (read-input path)
                   (filter #(s/valid? ::year-keyword-checks (:year %)))              ;returns a sequence of maps where the keyword :Year is within the map
                   (map #(Integer/parseInt (format-date (:year %)))))] ;extracts the year and formats to year, apply parse int resulting in a sequence of intergers
    (- (apply max years) (apply min years))))               ;as we have a sequence of ints we can use apply to find the largest and smallest number

(s/fdef years-between-first-and-last
        :args (s/cat :path ::path-ends-with-json-and-is-string?)
        :ret int?)
(stest/instrument 'years-between-first-and-last)

;4 Which meteorite fell closest to sheffield hallam university cantor building
  ;lat: 53.378292 long:-1.466574

;I wanted to find the distance between two diffrent vectors of cordinates, i came accross the haversine and vincenty formulas which could do this for me
; after some research i found the code below on rosettacode and modified it to take two vectors of cords   https://rosettacode.org/wiki/Haversine_formula
(defn haversine [[lon1 lat1] [lon2 lat2]]
  (let [R 6372.8 ; kilometers
        dlat (Math/toRadians (- lat2 lat1))
        dlon (Math/toRadians (- lon2 lon1))
        lat1 (Math/toRadians lat1)
        lat2 (Math/toRadians lat2)
        a (+ (* (Math/sin (/ dlat 2)) (Math/sin (/ dlat 2))) (* (Math/sin (/ dlon 2)) (Math/sin (/ dlon 2)) (Math/cos lat1) (Math/cos lat2)))]
    (* R 2 (Math/asin (Math/sqrt a)))))
(defn closest-meteorite-fall-to-cantor [path]
  (let [cantor-coordinates [-1.466574 53.378292]
        meteorite-year-masses (->> (read-input path)
                                   (map #(select-keys % [:name :geolocation])) ;returns a new map only containing keywords :name and :geolocation
                                   (reduce (fn [m d]
                                             (let [coords (:coordinates (:geolocation d)) ;Get the vector of long and lat [10 10]
                                                   dis (if coords
                                                         (haversine coords cantor-coordinates)
                                                         nil)] ;uses the haversine to work out the distance between cantor and the fall
                                               (if dis
                                                 (assoc m (:name d) dis) ;if dis is not nil then we can assoc it to the map
                                                 m)))
                                           {})
                                   (sort-by val)
                                   )
        ]
    (first meteorite-year-masses)))

(s/fdef closest-meteorite-fall-to-cantor
        :args (s/cat :path ::path-ends-with-json-and-is-string?)
        :ret vector?)
(stest/instrument 'closest-meteorite-fall-to-cantor )

;5. How many meteorites fell in each decade and what was there adverage mass?

(defn round-down-to-decade [year]
  (* 10 (quot (Integer/parseInt year) 10)))

(defn most-collective-mass-in-decades-with-frequency [path]
  (let [data (read-input path)
        decade-frequences (->> data
                               (filter #(and (s/valid? ::year-keyword-checks (:year %)) (s/valid? ::year-keyword-checks (:year %))))
                               (map #(round-down-to-decade(format-date (get % :year))))
                               (frequencies)
                            )
        decade-mass (->> data
                         (filter #(and (s/valid? ::year-keyword-checks (:year %)) (s/valid? ::mass-keyword-checks (:mass %)))) ;Filter all records that dont have both a year and a mass
                         (map #(update % :year (comp round-down-to-decade format-date)))
                         (map #(update % :mass format-mass))  ;format mass into a double
                         (reduce (fn [m d]                    ;reduce into a new map and go through each col of the previous map
                                   (if (find m (:year d))     ;checks if the year from the col is in the new map
                                     (assoc m (:year d) (+ (get m (:year d)) (:mass d) ) ) ;if so then we replace the value with the same year but with a combination of both masses
                                     (assoc m (:year d) (:mass d)))) ;add the data to the new map
                                 {})
                         )
        ]
    (first (into (sort-by :average-mass #(compare %2 %1)                   ;Used clojure help sheet to find out how to sort by decending order
                   (for [[year mass] decade-mass]           ;destrucres the decade-mass collection into years and mass (goes through each one)
                     {:year year                            ;start the creation of a new sequence of maps, binds year to key word
                      :average-mass (/ mass (get decade-frequences year)) ;divides the mass by the frequences the year appears
                      :frequency (get decade-frequences year)}))) ;adds the frequency to the collection (we could use this later to find the total mass for the decade)
    ))
  )
(s/fdef most-collective-mass-in-decades-with-frequency
        :args (s/cat :path ::path-ends-with-json-and-is-string?)
        :ret vector?)
(stest/instrument 'most-collective-mass-in-decades-with-frequency )

(defn -main []
  (let [trinary-number "112"
   decimal-equivalent (convert-trinary-to-decimal-recursive trinary-number)]
  (println (str "The decimal equivalent of " trinary-number " is " decimal-equivalent)))
  (println (convert-rna-sequence-to-amino-acids "AUGUUUUAA"))
  )
;(println (s/valid? even? 999))                              ;returns true or false
;(println (s/conform even? 999))                            ;returns 999 or :clojure.spec.alpha/invalid