(ns dp2-hydro.core)

(def max-water-capacity 70)

(defn p1
  "p1 function"
  [q]
  (if (< q 15)
    0
    (if (cond (>= q 15) (< q 50))
      (+ 4
        (* 0.2 (- q 15))
        (* 0.015 (Math/pow (- q 15) 2)))
      (if (cond (>= q 50) (<= q max-water-capacity))
        (+ 29.375
          (* 1.25 (- q 50))
          (* (- 0.0175) (Math/pow (- q 50) 2)))
        (throw (Exception. "p1 exceed capacity"))))))


(defn p2
  "p2 function"
  [q]
  (if (< q 20)
    0
    (if (cond (>= q 20) (< q 45))
      (+ 3
         (* 0.2 (- q 20))
         (* 0.04 (Math/pow (- q 20) 2)))
      (if (cond (>= q 45) (<= q max-water-capacity))
        (+ 33
           (* 1.25 (- q 45))
           (* (- 0.0175) (Math/pow (- q 45) 2)))
        (throw (Exception. "p2 exceed capacity"))))))

(defn p3
  "p3 function"
  [q]
  (if (< q 25)
    0
    (if (cond (>= q 25) (<= q max-water-capacity))
      (+ 6
         (* 1.25 (- q 25))
         (* 0.012 (Math/pow (- q 25) 2)))
      (throw (Exception. "p3 exceed capacity")))))


(def granularity 5)
(def decision-range (range 0 (+ max-water-capacity granularity) granularity))


(defn state-range-factory
  "create state ranges with a range of water-total"
  [water-total-range]
  (mapv
    (fn [water-total] (range 0 (+ water-total granularity) granularity))
    water-total-range))


(def turbines-list (list
                     {:tag 3 :fn p3}
                     {:tag 2 :fn p2}
                     {:tag 1 :fn p1}))


(defn max-production-sequence
  "max produciton from a list of sequence"
  [production-sequences]
  (reduce (fn [pp1 pp2] (if (> ( (first pp1) :p) ( (first pp2) :p)) pp1 pp2)) [{:p 0}] production-sequences))



(defn find-best-production-for-state
  "Find best production in vector"
  [s production-fn best-next-productions turbine-tag]
  (max-production-sequence
          (mapv (fn [x]
                  (let [water-after (- s x)]
                    ;we first look if there is any water after usages of this turbine
                    (if (> water-after 0)
                      (let [best-next (nth best-next-productions (/ water-after granularity))]
                        (into [{:turbine turbine-tag
                                :p (+ (production-fn x)
                                      ((first best-next) :p))
                                :s s :x x}]
                              best-next))
                      (if (= water-after 0)
                        ;if there is no water afeter turnine n then no need to look for next productions
                        [{:turbine turbine-tag :p (production-fn x) :s s :x x}]
                        ;s - x can be negative, we handle this case by setting a 0 production
                        [{:turbine turbine-tag :p 0 :s s :x x}]))))
                decision-range)))


(defn calculate-best-productions
  "calculate best productions with given production function"
  [production-fn best-next-productions turbine-tag state-range]
  (mapv (fn [s]
          (find-best-production-for-state s production-fn best-next-productions turbine-tag))
        state-range))

(defn maximize-production
  "maximize electric production with given water quantity"
  [turbines best-next-productions state-range]
  (let [turbines-count (count turbines)
        current-turbine (first turbines)
        best-productions (calculate-best-productions
                           (current-turbine :fn)
                           best-next-productions (current-turbine :tag)
                           state-range)]
    (if (> turbines-count 1)
      (maximize-production (rest turbines) best-productions state-range)
      (max-production-sequence best-productions))))


(defn -main
  "Launch optimization solution"
  []
  (let [solution (mapv (fn [sate-range] (maximize-production turbines-list (repeat (count sate-range) [{:p 0}]) sate-range))
                       (state-range-factory (range 50 215 5)))]
    (mapv (fn [partial-solution]
            (subs
              (reduce (fn [cumm turbine] (str cumm "," (turbine :x)))
                    ""
                    partial-solution) 1))
          solution)))



