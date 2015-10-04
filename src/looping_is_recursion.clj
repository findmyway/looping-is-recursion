(ns looping-is-recursion)

(defn power [base exp]
  (cond 
    (= base 0) 0
    (= exp 0) 1
    :else (* base (power base (dec exp)))))

(defn last-element [a-seq]
  (if (seq (rest a-seq))
    (last-element (rest a-seq))
    (first a-seq)))

(defn seq= [a-seq b-seq]
  (cond 
    (and (empty? a-seq) (empty? b-seq)) true
    (or (empty? a-seq) (empty? b-seq)) false
    (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
    :else false
    ))

(defn find-first-index [pred a-seq]
  (loop [idx 0
         a a-seq]
    (cond
      (= (count a) 0) nil
      (pred (first a)) idx
      :else (recur (inc idx) (rest a)))))

(defn avg [a-seq]
  (loop [sum 0
         c 0
         a a-seq]
    (if (seq a)
      (recur (+ sum (first a)) (inc c) (rest a))
      (/ sum c))))

(defn parity [a-seq]
  (set (keys (filter #(odd? (second %)) (frequencies a-seq)))))

(defn fast-fibo [n]
  (loop [fn0 0
         fn1 1
         cur 1]
    (cond
      (= n 0) fn0
      (= n 1) fn1
      (< cur n) (recur fn1 (+ fn0 fn1) (inc cur))
      :else fn1)))

(defn cut-at-repetition [a-seq]
  (loop [a a-seq
         x (first a)
         res []]
    (cond
      (or (empty? a) (some #{x} res)) res
      :else (recur (rest a) (second a) (conj res x)))))
