;********************************* test-scan.lisp ****************************

(scan-reset "ccl;Z I/O:test.io")

(setf a (array '(3 2) :type :quail :initial-contents :scan))

(setf b (array '(3 2) :type :quail :initial-contents :scan))

(setf c (array '(3 2) :type :quail :initial-contents :scan))

(quit)