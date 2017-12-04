(defclass Car()
	(
		(name :accessor fl-name :initarg :fname)
		(speed :accessor fl-speed :initarg :fspeed)
		(price :accessor fl-price :initarg :fprice)
		(fuel_consumption :accessor fl-fuel_consumption :initarg :ffuel_consumption)
	)
)

(defclass Porche(Car)
	()
)

(defclass Mercedes(Car)
	()
)

(defclass Lada(Car)
	()
)

(defclass Renault(Car)
	()
)

(defmethod print-object ((v Car) out)
   (format out "~s fuel_consumption:~s price:~s, speed:~s, " (fl-name v) (fl-fuel_consumption v) (fl-price v) (fl-speed v)))


(defclass Park()
	(
	    (Cars :initform '() :accessor Car-lst)
	)
)

(defmethod add-Car ((s Park) (v Car))
	(setf (Car-lst s) (cons v (Car-lst s)))
)

(defun sum-Cars-price (Cars)
	(cond
		((null Cars) 0)
		(T(+ (fl-price (car Cars)) (sum-Cars-price (cdr Cars))))
	)
)

(defmethod get-total-price ((s Park))
	(sum-Cars-price (Car-lst s))
)

(defun make-Park()
	(defparameter s (make-instance 'Park))
	s
)

(defmethod car-with-speed-inner (cars sp-min sp-max)
	(cond
		((null cars) NIL)
		((and (>= (fl-speed (car cars)) sp-min)(<= (fl-speed (car cars)) sp-max))
			 (cons (car cars) (car-with-speed-inner (cdr cars) sp-min sp-max))
		)
		(T(car-with-speed-inner (cdr cars) sp-min sp-max))
	)
)

(defmethod car-with-speed ((s Park) sp-min sp-max)
	(car-with-speed-inner (car-lst s) sp-min sp-max)
)