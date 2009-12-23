(ns ga (:use (clojure.contrib seq-utils)))

(defn find-index
  "Returns the (1-based) index of sorted coll where val <= coll[index]"
  [val coll]
  (dec (count (take-while #(<= % val) coll))));todo: binary search may be faster

(defn tournament-select
  [pop fit-fn k p]
  (let [indexed (zipmap pop (map fit-fn pop))]
    ))

(defn roulette-select
  "Select num individuals from pop, with an individual's selection likelihood proportional to fitness."
  [pop fit-fn num]
  (let [pop-fits (map fit-fn pop)
	inc-fits (reductions + 0 pop-fits)
        tot-fitness (apply + pop-fits)]
    (take num (repeatedly #(nth pop (find-index (rand tot-fitness) inc-fits))))))

(defn ga
  "Returns and infinite seq of populations.
init-fn: takes no arguments and returns a new population member
fit-fn: takes a population member and outputs fitness
mut-fn: takes a population member and returns a mutated member.
sel-fn: takes a population, a fitness function, and a number to select. Returns selected members.
cross-fn: combines two or more parents into one child
equal-fn is a key-fn for population members.  It is used to ensure population diversity.
pop-size, mut-prob, and cross-prob should be self-explanatory."
  [hmap]
  (let [{:keys [init-fn fit-fn mut-fn sel-fn cross-fn equal-fn
		mut-prob cross-prob cross-num pop-size elite-size init-pop]
	 :or {pop-size 20, init-pop (take pop-size (repeatedly init-fn)), mut-prob 0.01, cross-prob 0.9
	      cross-num 2, elite-size 1, equal-fn identity, sel-fn roulette-select
	      }} hmap]
    (iterate
     (fn [pop]
       (let [elite (take elite-size (sort-by fit-fn > pop))
	     eset (set (map equal-fn elite))
	     new-fn #(let [temp (if (> cross-prob (rand))
				  (apply cross-fn (sel-fn pop fit-fn cross-num))
				  (sel-fn pop fit-fn 1))]
		       (if (> mut-prob (rand))
			 (mut-fn temp) temp))
	     chosen (->> (repeatedly new-fn)
			 (remove #(eset (equal-fn %)))
			 (take (- pop-size (count elite))))]
	 (concat elite chosen))) init-pop)))