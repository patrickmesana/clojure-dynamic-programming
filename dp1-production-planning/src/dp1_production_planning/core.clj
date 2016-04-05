(ns dp1-production-planning.core
  (:gen-class))

(def D [6 5 3 4 7 8])
(def PC [14 18 15 17 20 14])
(def SC (mapv #(* % 0.04) PC))
(def nbr-of-steps 6)
(def granularity 1)
(def SR (range 0 (+ 9 1) granularity))
(def DR (range 0 (+ 10 1) granularity))

(defn cost-fn
  "cost function"
  [x, s, cp, cs, d]
  (+ (* cp x)
     (* cs
        (/ (+ (* 2 s) x (- d))
           2))))

(defn calculate-costs-matrix
  "calulate best costs with cost fn"
  [decision-range state-range cp cs d]
  (mapv (fn [s]
          (mapv (fn [x]
                  (if (< (+ x s) d)
                    999999
                    (cost-fn x s cp cs d)))
                decision-range))
        state-range))


(defn add-stock-cost
  "ensure that can't add over 999999"
  [& args]
  (let [result (apply + args)]
    (if (< result 999999)
      result
      999999)))

(defn add-stocks-costs-to-matrix
  "Adds n values to a matrix navigating through rows"
  [matrixv costs stocks]
  (mapv (fn [matrix-row stocks-row]
          (mapv (fn [value stock]
                  (if (>= stock 0)
                    (add-stock-cost value
                                    (nth costs
                                         (let [costs-count (count costs)]
                                           (if (< stock costs-count)
                                             stock
                                             (- costs-count 1)))))
                    value))
                matrix-row stocks-row))
        matrixv stocks))

(defn min-column-from-matrix
  "Take a matrix and calculate the min column"
  [matrix]
  (mapv (fn [row] (apply min row)) matrix))


(defn stocks
  "calculate stocks with state and decision matrix"
  [decision-range state-range d]
  (mapv (fn [s]
          (mapv (fn [x]
                  (+ x s (- d)))
                decision-range))
        state-range))



(defn objective-fn
  "recursive objective funciton"
  [steps decision-range state-range best-next-costs prod-costs stock-costs demands]
  (let [costs (min-column-from-matrix
                     (add-stocks-costs-to-matrix
                       (calculate-costs-matrix decision-range
                                               state-range
                                               (nth prod-costs steps)
                                               (nth stock-costs steps)
                                               (nth demands steps))
                       best-next-costs
                       (stocks decision-range state-range (nth demands steps))))]
      (if (> steps 0)
        (objective-fn (- steps 1)
                      decision-range
                      state-range
                      costs
                      prod-costs
                      stock-costs
                      demands)
        costs)))

(defn -main
  "main"
  [& args]
  (objective-fn (- nbr-of-steps 1) DR SR (vec (repeat 10 0)) PC SC D))