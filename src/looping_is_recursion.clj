(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc base exp]
                 (if (< exp 1)
                   acc
                   (recur (* acc base) base (dec exp))))]
    (helper 1 base exp)))

(defn last-element [a-seq]
  (let [helper (fn [acc s]
                 (if (empty? s) acc (recur (first s) (rest s))))]
    (helper nil a-seq)))

(defn seq= [seq1 seq2]
  (let [helper (fn [s1 s2]
                 (cond
                   (and (empty? s1) (empty? s2)) true
                   (or (empty? s1) (empty? s2)) false
                   (not (= (first s1) (first s2))) false
                   :else (recur (rest s1) (rest s2))))]
    (helper seq1 seq2)))

(defn find-first-index [pred a-seq]
  (loop [idx 0
         s a-seq]
    (cond
      (empty? s) nil
      (pred (first s)) idx
      :else (recur (inc idx) (rest s)))))

(defn avg [a-seq]
  (loop [acc 0
         num 0
         s a-seq]
    (if (empty? s)
      (/ acc num)
      (recur (+ acc (first s)) (inc num) (rest s)))))

(defn parity [a-seq]
  (loop [acc #{}
         s a-seq]
    (if (empty? s)
      acc
      (recur
        (let [e (first s)] (if (contains? acc e) (disj acc e) (conj acc e)))
        (rest s)))))

(defn fast-fibo [n]
  (loop [f1 0
         f2 1
         c 1]
    (cond
      (< n 2) n
      (>= c n) f2
      :else (recur f2 (+ f1 f2) (inc c)))))

(defn cut-at-repetition [a-seq]
  (loop [acc #{}
        s a-seq]
    (if (contains? acc (first s))
      (take (count acc) a-seq)
      (recur (conj acc (first s)) (rest s)))))

