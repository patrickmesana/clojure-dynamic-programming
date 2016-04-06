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
(def water-total 135)
(def decision-range (range 0 (+ max-water-capacity granularity) granularity))
(def state-range (range 0 (+ water-total granularity) granularity))
(def turbines-list (list p1 p3 p2))




(defn calculate-best-productions
  "calculate best productions with given production function"
  [production-fn best-next-productions]
  (mapv (fn [s]
         (apply max-key :p
           (mapv (fn [x]
                    ;s - x can be negative, we handle this case by setting a low prod
                    (let [water-after (- s x)]
                      (if (> water-after 0)
                        {:p (+ (production-fn x)
                               ((nth best-next-productions (/ water-after granularity)) :p))
                         :s s :x x}
                        (if (= water-after 0)
                          {:p (production-fn x) :s s :x x}
                          {:p 0 :s s :x x}))))
                decision-range)))
        state-range))



(defn maximize-production
  "maximize electric production with given water quantity"
  [turbines best-next-productions]
  (let [best-productions (calculate-best-productions (first turbines) best-next-productions)]
    (if (> (count turbines) 1)
      (maximize-production (rest turbines) best-productions)
      best-productions)))


(defn -main
  "Launch optimization solution"
  []
  (maximize-production turbines-list (repeat (count state-range) {:p 0})))

