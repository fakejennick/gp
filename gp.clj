(ns gp (:use ga [clojure.contrib seq-utils]))

(def nums '(1 2 3 4 5 6 7 8 9 10))

(defn make-args [vars nums]
  (take (inc (rand-int 3))
        (repeatedly #(if (< 0.2 (rand 1))
                       (rand-elt vars)
                       (rand-elt nums)))))

;todo: expand grammer beyond simple math
(def funs `((~'+ ~make-args)
	    (~'- ~make-args)
	    (~'* ~make-args)
	    (~'/ ~make-args)))

(defn tree-height [tree]
  "Returns the height of the deepest subtree."
  (let [th (fn th [atree height]
	     (if (not (coll? atree))
	       height
	       (apply max (map #(th % (inc height)) atree))))]
    (th tree 0)))

(defn random-tree-elt [tree]
  "Selects a random element in tree, or possibly the tree itself. Returns a list of indices needed to reach element."
  (let [height (tree-height tree)
	index (rand-int (count tree))
	element (nth tree index)]
    (if (not (coll? element))
      '()
;pick element from current tree with probability 1/height
      (if (> 1 (rand-int height))
	(list index)
	(cons index (random-tree-elt element))))))

(defn new-random-list [vars nums]
  "Generates a new random list using funs"
  (let [fun (rand-elt funs)]
    (cons (first fun) ((second fun) vars nums))))

(defn my-subseq
  "Works like lisp's subseq"
  ([sequence start length]
     (take length (drop start sequence)))
  ([sequence start]
     (drop start sequence)))

(defn replace-element [new-elt tree indices]
  "Like lisp's subst, but only replaces the specific old element (given by indices) with a new element."
  (if (empty? indices)
    new-elt
    (map #(if (= %2 (first indices))
	    (if (next indices)
	      (replace-element new-elt %1 (next indices))
	      new-elt)
	    %1)
	 tree (iterate inc 0))))

(defn remove-nth [n seq]
  "Removes the nth item from seq, where n is zero based"
   (concat (take n seq) (drop (inc n) seq)))

(defn new-operand [fun vars nums]
  (condp > (rand)
    0.1 (new-random-list vars nums)
    1.0 (rand-elt ((second fun) vars nums))))

(defn insert-arg [lst arg]
  (let [index (inc (rand-int (dec (count lst))))]
    `(~@(my-subseq lst 0 index)
      ~arg
      ~@(my-subseq lst index))))

(defn mutate-list [lst vars nums]
  "Either adds to, removes from, or modifies an element in lst.  lst and its elements may actually be trees."
         ;index from 1 to n-1, so we exclude the function
    (let [fun (first (filter #(= (first %) (first lst)) funs))
	  index (inc (rand-int (dec (count lst))))
	  element (nth lst index)
	  t (rand-int 2)]
      (if (= 0 t)
	(new-random-list vars nums)
	;(insert-arg (new-random-list vars nums) lst)
	;else modify the args
	;todo: don't assume variable-arity: check # of args
	(condp > (rand)
          ;add a new operand
	  0.1
	  (if (< 5 (count lst));limit for now
	    lst
	    (insert-arg lst (new-operand fun vars nums)))
          ;remove an operand
	  0.8
	  (if (< 2 (count lst))
	    (remove-nth index lst)
	    lst)
          ;modify an operand
	  1.0
	  (replace-element ;(if (coll? element)
			     ;(mutate-list element vars nums)
			     (new-operand fun vars nums);)
			    lst (list index))
	  ;permute args?
	  ))))

(defn get-tree-elt [tree indices]
  (if (empty? indices)
    tree
    (let [element (nth tree (first indices))]
      (if (next indices)
	(recur element (next indices))
	element))))

(defn mutate-tree [tree vars nums]
  "Selects a random element in tree, then mutates and replaces it, returning the new tree."
  (let [indices (random-tree-elt tree)
	element (get-tree-elt tree indices)]
    (replace-element (mutate-list element vars nums) tree indices)))

(defn crossover-trees
  "Replaces a random sub-tree in t1 with one from t2."
  [t1 t2]
  (let [i1 (random-tree-elt t1)
	i2 (random-tree-elt t2)
	element (get-tree-elt t2 i2)]
    (replace-element element t1 i1)))

(defmacro capture-vars [vars expr]
  "This intentionally captures vars, so we can include vars as part of expr."
  `(fn [~@vars]
     ~(first (next expr))))

(defn make-vars [n]
  "Returns a seq of symbols (x1 ... xn)"
  (map #(symbol (apply str (concat "x" (str %))))
       (range 1 (inc n))))

(defn expr-scorer [xylist]
  (fn [expr]
    (try
     (let [vars (make-vars (dec (count (first xylist))))
	   my-fn (eval `(capture-vars ~vars '~expr))]
       (/ (reduce + 0.1 (map #(Math/pow (- (first %) (apply my-fn (next %))) 2)
		      xylist))))
     (catch java.lang.RuntimeException e 0))))

(defn init-tree [num-vars] (new-random-list (make-vars num-vars) nums))

(defn gp [xylist options]
  (let [num (dec (count (first xylist)))]
    (ga (merge {:init-fn #(init-tree num)
		:cross-fn crossover-trees
		:mut-fn #(mutate-tree % (make-vars num) nums)
		:fit-fn (expr-scorer xylist)} options))))

(defn my-poly [x]
  "Returns x^3 + 3*x^2 + 4*x + 6"
  (+ (* x x x) (* 3 x x) (* 4 x) 6))

(defn r2 [x y]
  (+ (* x x) (* y y)))

(defmacro make-tuples [n start end step]
  "Creates an n-dimensional grid of points, returning a seq of tuples"
  (let [vars (make-vars n)]
    `(for [~@(interleave vars (repeat `(range ~start ~end ~step)))]
       (list ~@vars))))

(defn get-tuples [f numargs start end step]
  "Given a function of numargs, evaluates f at all points in the n-dimensional grid, and returns a seq of tuples"
  (map #(concat (list (apply f %)) %)
       (eval `(make-tuples ~numargs ~start ~end ~step))))
