(ns numbers.core)

(defn square
  [x]
  (* x x))


(defprotocol Complex
  (real-part [z] "Real part of a complex number")
  (imag-part [z] "Imaginary part of a complex number")
  (magnitude [z] "Magnitude of a complex number")
  (angle [z] "Angular component of a complex number"))


(defprotocol NumberKind
  (kind [n] "The keyword name for numeric type of n"))


(defrecord Complex-r [real imag]
  Complex
  (real-part [z] (:real z))
  (imag-part [z] (:imag z))
  (magnitude [z] (Math/sqrt (+ (square (:real z))
                               (square (:imag z)))))
  (angle [z] (Math/atan (/ (:imag z) (:real z))))

  NumberKind
  (kind [z] ::complex-r))


(defrecord Complex-p [magnitude angle]
  Complex
  (real-part [z]
    (* (:magnitude z) (Math/cos (:angle z))))
  (imag-part [z]
    (* (:magnitude z) (Math/sin (:angle z))))
  (magnitude [z] (:magnitude z))
  (angle [z] (:angle z))

  NumberKind
  (kind [z] ::complex-p))


(defrecord Int [n]
  NumberKind
  (kind [i] ::integer))


(defn integer
  "Creates a new Integer"
  [n]
  {:pre [(number? n)]}
  (->Int (int n)))


(defrecord Real [n]
  NumberKind
  (kind [r] ::real))

(defn real
  "Creates a new Real"
  [n]
  {:pre [(number? n)]}
  (->Real (double n)))


(defn complex-r
  "Creates new Complex-r"
  [real imag]
  {:pre [(number? real)
         (number? imag)]}
  (->Complex-r real imag))


(defn complex-p
  "Creates a new Complex-p"
  [magnitude angle]
  {:pre [(number? magnitude)
         (number? angle)]}
  (->Complex-p magnitude angle))


(defn gcd
  [a b]
  (if (zero? b)
    a
    (recur b (mod a b))))

(defrecord Rational [n d]
  NumberKind
  (kind [r] ::rational))


(defn rational
  "Creates a new Rational"
  [n d]
  {:pre [(number? n)
         (number? d)]}
  (let [g (gcd n d)]
    (->Rational (/ n g) (/ d g))))


(defn numer
  "Numerator of a rational"
  [^Rational r]
  (:n r))


(defn denom
  "Denominator of a rational"
  [^Rational r]
  (:d r))


;; dispatch hierarchy
(derive ::complex-r ::complex)
(derive ::complex-p ::complex)
(derive ::complex ::number)
(derive ::integer ::number)
(derive ::real ::number)
(derive ::rational ::number)


(defprotocol Raise
  (raise [x] "raises a number type to the next type
          in the numeric tower with equivalent value"))

(extend-protocol Raise
  Int
  (raise [x] (rational (:n x) 1))

  Rational
  (raise [x] (real (double (/ (numer x) (denom x)))))

  Real
  (raise [x] (complex-r (:n x) 0)))


;; generic arithmetic

(defn- dispatch-arithmetic
  "Dispatch function for arithmetic of two operands x y"
  [x y]
  [(kind x) (kind y)])

(defmulti add
  "Add two generic numbers"
  dispatch-arithmetic)

(defmulti sub
  "Subtract two generic numbers"
  dispatch-arithmetic)

(defmulti mul
  "Multiply two generic numbers"
  dispatch-arithmetic)

(defmulti div
  "Divide two generic numbers"
  dispatch-arithmetic)

;; integer arithmetic
(defmethod add [::integer ::integer]
  [x y]
  (integer (+ (:n x) (:n y))))

(defmethod sub [::integer ::integer]
  [x y]
  (integer (+ (:n x) (:n y))))

(defmethod mul [::integer ::integer]
  [x y]
  (integer (* (:n x) (:n y))))

(defmethod div [::integer ::integer]
  [x y]
  (integer (/ (:n x) (:n y))))


;; rational arithmetic

(defmethod add [::rational ::rational]
 [x y]
 (rational (+ (* (numer x) (denom y))
              (* (numer y) (denom x)))
           (* (denom x) (denom y))))

(defmethod mul [::rational ::rational]
 [x y]
 (rational (* (numer x) (numer y))
           (* (denom x) (denom y))))

(defmethod sub [::rational ::rational]
 [x y]
 (rational (- (* (numer x) (denom y))
              (* (numer y) (denom x)))
           (* (denom x) (denom y))))

(defmethod div [::rational ::rational]
 [x y]
 (rational (* (numer x) (denom y))
           (* (denom x) (numer y))))


;; real arithmetic
(defmethod add [::real ::real]
  [x y]
  (real (+ (:n x) (:n y))))

(defmethod sub [::real ::real]
  [x y]
  (real (+ (:n x) (:n y))))

(defmethod mul [::real ::real]
  [x y]
  (real (* (:n x) (:n y))))

(defmethod div [::real ::real]
  [x y]
  (real (/ (:n x) (:n y))))


;; complex arithmetic
(defmethod add [::complex ::complex]
  [z1 z2]
  (complex-r (+ (real-part z1) (real-part z2))
             (+ (imag-part z1) (imag-part z2))))

(defmethod sub [::complex ::complex]
  [z1 z2]
  (complex-r (- (real-part z1) (real-part z2))
             (- (imag-part z1) (imag-part z2))))

(defmethod mul [::complex ::complex]
  [z1 z2]
  (complex-p (* (magnitude z1) (magnitude z2))
             (+ (angle z1) (angle z2))))

(defmethod div [::complex ::complex]
  [z1 z2]
  (complex-p (/ (magnitude z1) (magnitude z2))
             (- (angle z1) (angle z2))))


;; generic number


(def numeric-tower
  (-> (make-hierarchy)
      (derive ::complex ::complex-p)
      (derive ::complex ::complex-r)
      (derive ::real ::complex)
      (derive ::rational ::real)
      (derive ::integer ::rational)))

(defn higher?
  "Is the type of x higher than the type of y?"
  [t1 t2]
  (if (contains? (ancestors numeric-tower t2) t1)
    true
    false))

(defn lower?
  "Is the type of x lower than the type of y?"
  [t1 t2]
  (if (contains? (descendants numeric-tower t2) t1)
    true
    false))

(defn coerce-types
  "Given two different number types raise the lesser type up
  to be the same as the greater type"
  [x y]
  (let [t1 (kind x)
        t2 (kind y)]
    (cond
      (lower? t1 t2) (recur (raise x) y)
      (higher? t1 t2) (recur x (raise y))
      :else [x y])))


;; arithmetic of mixed types
(defmethod add [::number ::number]
  [n1 n2]
  (apply add (coerce-types n1 n2)))

(defmethod sub [::number ::number]
  [n1 n2]
  (apply sub (coerce-types n1 n2)))

(defmethod mul [::number ::number]
  [n1 n2]
  (apply mul (coerce-types n1 n2)))

(defmethod div [::number ::number]
  [n1 n2]
  (apply div (coerce-types n1 n2)))


;; !!! Uncomment to enable pretty printing of our new number types. !!!

;; (defmethod print-method Real [x ^java.io.Writer writer]
;;   (print-method (:n x) writer))

;; (defmethod print-method Int [x ^java.io.Writer writer]
;;   (print-method (:n x) writer))


;; (defmethod print-method Rational [x ^java.io.Writer writer]
;;  (.write writer (str (numer x) \/ (denom x))))

;; (defmethod print-method Complex-r [x ^java.io.Writer writer]
;;  (if (pos? (imag-part x))
;;    (.write writer (str \( (real-part x) \+ (imag-part x) \I\) ))
;;    (.write writer (str \( (real-part x) (imag-part x) \I\) ))))

;; (defmethod print-method Complex-p [x ^java.io.Writer writer]
;;  (.write writer (str \( (magnitude x) \* "exp" \( (angle x) \I \) \) )))
