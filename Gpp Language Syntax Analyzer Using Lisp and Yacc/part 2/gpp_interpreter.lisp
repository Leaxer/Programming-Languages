(defvar *func-hash* (make-hash-table :test 'equal))

(load "gpp_lexer.lisp")
(setq globalFunc nil)

(defun start (token-l)
  (let ((is '()))
    (cond
      ((string= (car token-l) "COMMENT")
       (format t "COMMENT~%")
       (return-from start)
      )
      ((car (setf is (isExp token-l)))
        (let ((expList '()))
          (if (/= (list-length token-l) (cadr is))
              (format t "SYNTAX_ERROR Expression not recognized.~%")
          )
          (setq expList (cfgExp token-l wordlist))
          (if (car expList)
            (format t "Syntax OK~%Result: ~a~%" (nth 2 expList))
            (format t "SYNTAX_ERROR Expression not recognized.~%"))))
      ((car (setf is (isFunc token-l)))
        (if (cfgFunc token-l wordlist (second is))
        (format t "Syntax OK. Function Created~%")
        (format t "Syntax Error. New function couldn't created.~%")))
      ((isExit token-l)
        (exit))
      (t  (format t "SYNTAX_ERROR Expression not recognized.~%"))
    )
  )
)


(defun isExp (token-l)
    (if (and (string= (car token-l) "OP_OP")  ; If expression starts with "(" "OPERATOR"
              (is-token-op (cadr token-l)))
      (let ((n 2)(expList '())(expList2 '()))
        (setq expList (isExp (subseq token-l n (+ n (find-first-op-cp (remove-first-n token-l n)))))) ; Get only one (EXP) Examp: (+ (+ 4b3 2b3) 7b9). Finds first OP_CP index. Subseq the expression between n and OP_CP index. Equals to (+ 4b3 2b3). Performs the function for this expression.
        (setq n (+ n (second expList))) ; It adds the total number of tokens of the expression we looked at above to n. Thus, it continues from its current location
        (setq expList2 (isExp (subseq token-l n (+ n (find-first-op-cp (remove-first-n token-l n)))))) ; Same operations for the 2nd parameter
        (setq n (+ n (second expList2)))
        
        (if (and (car expList)(car expList2)(string= (car (last token-l)) "OP_CP")) ; If two parameters' isExp functions returns true and expression ends with OP_CP
            (progn
              (setq n (+ n 1)) ; Increase n to include last token "OP_CP" in this expression
              (return-from isExp (list t n)) ; returns list. T for if it's exp or not. N is the total number of tokens program looks at in this expression
            )
            (return-from isExp (list nil n))
        )
      )
    )
    (if (and (string= (car token-l) "OP_OP") (is-token-id (cadr token-l))) ; If expression starts with "(" "IDENTIFIER"
      (let ((n 2) (counter 0) (expList '(t 1)))
        ; According to rules expression continues with at most 3 expressions. It will loop until the last OP_CP and checks expressions. 
        ; If counter is equal 4 finish loop. Also checks if return value from below function is nil or true
        (loop while (and (< n (- (length token-l) 1)) (/= counter 4)(car expList))
          do
          (setq expList (isExp (subseq token-l n (+ n (find-first-op-cp (remove-first-n token-l n)))))) ; Get only one (EXP) Examp: (+ (+ 4b3 2b3) 7b9). Finds first OP_CP index. Subseq the expression between n and OP_CP index. Equals to (+ 4b3 2b3). Performs the function for this expression.
          (setq n (+ n (second expList))) ; It adds the total number of tokens of the expression we looked at above to n. Thus, it continues from its current location
          (setq counter (+ counter 1)) ; It checks one expression, so increase counter by one
        )
        (if (and (= n (- (length token-l) 1))(string= (car (last token-l)) "OP_CP")) ; After checking all tokens controls. If it ends before last token that means there is an non-exp token and loop finished early. If n is proper, it checks is last token OP_CP or not
          (setq n (+ n 1)) ; Increase n to include last token "OP_CP" in this expression
          (return-from isExp (list nil n)) ; If it's not exp. returns nil
        )
        (if (and (/= counter 4)(car expList)) ; If counter is less than 4. So it gets at most 3 expression. And last expression program checked is true or not.
          (return-from isExp (list t n))
          (return-from isExp (list nil n))
        )
      )
    )
    (if (or (is-token-id (car token-l)) (is-token-val (car token-l))) ; If expression is id or valuef
      (return-from isExp (list t 1)) ; t for it's expression. Program just checked first token in list. So returns 1
    )
    
    (return-from isExp (list nil 1)) ; If token(s) is none of the above returns nil
)
(defun cfgExp (token-l word-l)
  (if (and (string= (car token-l) "OP_OP") 
              (is-token-op (cadr token-l)))
      (let ((n 2)(expList '())(expList2 '())(result))
        (setq expList (cfgExp (remove-first-n token-l n) (remove-first-n word-l n)))
        (setq n (+ n (second expList)))
        (setq expList2 (cfgExp (remove-first-n token-l n) (remove-first-n word-l n)))
        (setq n (+ n (second expList2)))
        (if (and (car expList)(car expList2))
            (progn
              (setq result (perform-op (cadr token-l) (nth 2 expList) (nth 2 expList2)))
              (return-from cfgExp (list t (+ n 1) result))
            )
        )
      )
    )
  (if (and (string= (car token-l) "OP_OP") 
              (is-token-id (cadr token-l)))
      (let ((keys (reverse (loop for k being each hash-key in *func-hash* collect k)))(func nil)) ; Collect keys of *func-hash* (Name of the functions)
        (loop for key in keys ; 
              do 
                (if (string= key (cadr word-l)) ; If token name is equal to function name
                  (setf func t) ; set func bool true
                )
        )
        (if func ; If there is a function with the token name
          (let* ((entry (gethash (cadr word-l) *func-hash*)) ; Get function hash table
                (inner-hash (first entry)) ; Get function's parameter hash table
                (numOfparam 2)
                (valueList '())
                (currFunc))
            (setq globalFunc (cadr word-l)) ; Sets globalFunc's name to our current function name
            (setq currFunc globalFunc) ; Holds the current function name to avoid name confusion in nested function calls
            (let ((valueKeys (reverse (loop for k being each hash-key in inner-hash collect k)))) ; Collect keys of inner-hash (Parameters of function)
              (loop for key in valueKeys ; For each parameter in the function
                    do  (setq expList (cfgExp (remove-first-n token-l numOfparam) (remove-first-n word-l numOfparam))) ; Get result of expression with calling this function again. It can be valuef or result of nested expression. 
                        (push (nth 2 expList) valueList) ; Holds value in list
                        (setf numOfparam (+ numOfparam (nth 1 expList)))
                        (setq globalFunc currFunc) ; Changed globalFunc to current function name again. User may call another function inside of this function and globalFunc name may change on above cfgExp function call. So it should be set to current function name again
              ))
            (if (null valueList) ; If there is no parameter send to function
              (setParameterFunction globalFunc)
              (setParameterFunction globalFunc (reverse valueList))
            )
            (setq last (cfgExp (nth 2 (gethash globalFunc *func-hash*)) (nth 1 (gethash globalFunc *func-hash*)))) ; After setting parameters. Send expression's token and word lists defined in the function. (def sum x y (+ x y)). "(+ x y)" This expression which holds in the function's hash table
            (setf (nth 1 last) (+ numOfparam 1))
            (return-from cfgExp last)
          )
        )
    )
  )

  (if (is-token-val (car token-l)) ; If token is valuef
    (return-from cfgExp (list t 1 (car word-l))) ; return true, 1 and string value of word
  )
  (if (is-token-id (car token-l)) ; For this program it checks only function parameters
    (if (string/= globalFunc nil) ; It checks function's parameters.
      (let* ((entry (gethash globalFunc *func-hash*)) ; Get function hash from main hash
            (inner-hash (first entry))) ; Get first value of function's hash table. It's also hash table which holds parameters
        (let ((keys (reverse (loop for k being each hash-key in inner-hash collect k)))) ; Collect keys of inner-hash(Parameters of function)
        (loop for key in keys ; Loop each parameters of function
              do 
                (if (string= key (car word-l)) ; If token name is equal to parameter name 
                  (return-from cfgExp (list t 1 (gethash key inner-hash))) ; Get value of this parameter
                )
        ))
      )
    )
  )
   (return-from cfgExp (list nil 0 nil))
)


(defun isFunc (token-l)
  (if (and (string= (car token-l) "OP_OP") 
              (string= (cadr token-l) "KW_DEF")
              (is-token-id (nth 2 token-l)))
      (let ((n 3)(expList '(t 1))(counter 0)(numberOfParam 0))
        (loop while (and (< n (- (length token-l) 1))(car expList)(/= counter 4))
          do
          (if (is-token-id (car (remove-first-n token-l n)))
              (progn
                (setq n (+ n 1))
                (setq numberOfParam (+ numberOfParam 1)) 
              )
              (progn
                (setq expList (isExp (remove-first-n token-l n)))
                (setq n (+ n (second expList)))
              )
          )
          (setq counter (+ counter 1))
        )
        (if (= counter 0)
          (return-from isFunc (list nil n))
        )
        (if (and (= n (- (length token-l) 1))(string= (car (remove-first-n token-l n)) "OP_CP"))
          (setq n (+ n 1))
          (return-from isFunc (list nil n))
        )
        (if (and (= n (length token-l))(car expList)(/= counter 4))
          (return-from isFunc (list t numberOfParam))
          (return-from isFunc (list nil n))
        )
      )
    )
    (return-from isFunc (list nil 0))
)
(defun cfgFunc (token-l word-l paramNum)
  (let ((keys (reverse (loop for k being each hash-key in *func-hash* collect k)))) ; Collect keys of *func-hash* (Name of the functions)
        (loop for key in keys
              do (if (string= key (nth 2 word-l)) ; If token name is equal to function name
                    (return-from cfgFunc nil)
                )))
  (setf (gethash (nth 2 word-l) *func-hash*) (list (make-hash-table :test 'equal) '() '()))
  (let* ((n 0)(temp-wordl (remove-first-n word-l 3))(temp-tokenl (remove-first-n token-l 3))(param-hash (first (gethash (nth 2 word-l) *func-hash*)))
         (funcExpression (second (gethash (nth 2 word-l) *func-hash*)))(newToken-l (third (gethash (nth 2 word-l) *func-hash*)))(paramWords '())(expIdWords '()))
    (loop while (< n paramNum)
          do
            (setf (gethash (nth n temp-wordl) param-hash) nil)
            (push (nth n temp-wordl) paramWords)
            (setq n (+ n 1))
        )
      (setf temp-tokenl (remove-first-n temp-tokenl n))
      (setf temp-wordl (remove-first-n temp-wordl n))
      (setq n 0)
      (loop for word in temp-wordl
            do
              (if (is-token-id (nth n temp-tokenl))
                (push word expIdWords)
              )
              (push (format nil "~a" (nth n temp-tokenl)) newToken-l)
              (push (format nil "~a" word) funcExpression)
              (setq n (+ n 1))
          )
      (if (not (check-words expIdWords paramWords))
        (progn
          (remhash (nth 2 word-l) *func-hash*)
          (return-from cfgFunc nil)
        )
      )
    (setf (second (gethash (nth 2 word-l) *func-hash*)) (reverse (subseq funcExpression 1)))
    (setf (nth 2 (gethash (nth 2 word-l) *func-hash*)) (reverse (subseq newToken-l 1)))
  )
)
(defun setParameterFunction (funcName &optional (values '()))
  (let* ((entry (gethash funcName *func-hash*))
         (inner-hash (first entry))
        )
    (let ((keys (reverse (loop for k being each hash-key in inner-hash collect k)))) ; Collect keys of inner-hash(Parameters of function)
    (loop for key in keys
          for value in values
          do (when value
               (setf (gethash key inner-hash) value))))
  )
)
(defun check-words (expIdWords paramWords)
  (let ((isCorrect t))
    (loop for word in expIdWords
          until (null paramWords)
          do (if (string= word (car paramWords))
                 (setf paramWords (cdr paramWords))
                 (setf isCorrect nil
                       paramWords nil)))
    isCorrect))

(defun isExit (token-l)
  (if (and (string= (car token-l) "OP_OP")(string= (cadr token-l) "KW_EXIT")(string= (caddr token-l) "OP_CP"))
    (return-from isExit t)
  )
)
(defun is-token-op (word)
      (loop for operator in OperatorTokens
            do(if(string= word operator)
                  (return-from is-token-op t)
              )
      )
    (return-from is-token-op nil)
)
(defun is-token-id (word)
    (if (string= word "IDENTIFIER")
      (return-from is-token-id t)
      (return-from is-token-id nil)
    )
)
(defun is-token-val (word)
    (if (string= word "VALUEF")
      (return-from is-token-val t)
      (return-from is-token-val nil)
    )
)


(defun perform-op (op val1 val2)
  (let ((frac1 (parse-fraction val1))
         (frac2 (parse-fraction val2)))
        (cond ((string= "OP_PLUS" op) (sum-fractions frac1 frac2))
              ((string= "OP_MINUS" op) (sub-fractions frac1 frac2))
              ((string= "OP_DIV" op) (divide-fractions frac1 frac2))
              ((string= "OP_MULT" op) (multiply-fractions frac1 frac2))
        )
  )
)
(defun parse-fraction (str)
  (let ((parts (split-string str #\b)))
    (list (parse-integer (first parts))
          (parse-integer (second parts)))
  )
)
(defun split-string (str separator)
  (let ((word "") (wordlist '()))
    (loop for char across str
          do    (if (char= char separator)
                  (progn
                      (push word wordlist)
                      (setf word "")
                  )
                  (setf word (concatenate 'string word (string char)))
                )
    )
    (push word wordlist)
    (reverse wordlist)
  )
)
(defun sum-fractions (frac1 frac2)
  (let* ((common-denominator (lcm (second frac1) (second frac2)))
         (numerator-sum (+ (* (first frac1) (/ common-denominator (second frac1)))
                           (* (first frac2) (/ common-denominator (second frac2)))))
         (gcds (gcd numerator-sum common-denominator))
         (simplified-numerator (/ numerator-sum gcds))
         (simplified-denominator (/ common-denominator gcds))
         )
    (concatenate 'string (write-to-string simplified-numerator) "b" (write-to-string simplified-denominator))
  )
)
(defun sub-fractions (frac1 frac2)
  (let* ((common-denominator (lcm (second frac1) (second frac2)))
         (numerator-sum (- (* (first frac1) (/ common-denominator (second frac1)))
                           (* (first frac2) (/ common-denominator (second frac2)))))
         (gcds (gcd numerator-sum common-denominator))
         (simplified-numerator (/ numerator-sum gcds))
         (simplified-denominator (/ common-denominator gcds))
         )
    (concatenate 'string (write-to-string simplified-numerator) "b" (write-to-string simplified-denominator))
  )
)
(defun multiply-fractions (frac1 frac2)
  (let* ((numerator-product (* (first frac1) (first frac2)))
         (denominator-product (* (second frac1) (second frac2)))
         (gcds (gcd numerator-product denominator-product))
         (simplified-numerator (/ numerator-product gcds))
         (simplified-denominator (/ denominator-product gcds))
        )
      (concatenate 'string (write-to-string simplified-numerator) "b" (write-to-string simplified-denominator))
  )
)

(defun divide-fractions (frac1 frac2)
  (if (zerop (first frac2))
      "Cannot divide by zero"
      (let* ((numerator-quotient (* (first frac1) (second frac2)))
             (denominator-quotient (* (second frac1) (first frac2)))
             (gcds (gcd numerator-quotient denominator-quotient))
             (simplified-numerator (/ numerator-quotient gcds))
             (simplified-denominator (/ denominator-quotient gcds))
          )
        (concatenate 'string (write-to-string simplified-numerator) "b" (write-to-string simplified-denominator))
      )
  )
)


(defun remove-first-n (list n)
  "Remove the first n elements from the list."
  (if (> n 0)
      (subseq list n)
      list))
(defun find-first-op-cp (lst)
  (loop for element in lst
        for index from 1
        until (string= element "OP_CP")
        finally (return-from find-first-op-cp index)))


(defun gppinterpreter (&optional filecont)
  (setq token-list (list))
  (if (null filecont) 
    (progn ; If there is no parameter sent
      (loop (format t "~%>>> ") 
            (setq token-list (gpplexer (read-line)))
            (start token-list))
    )
    (let ((lines (read_file filecont))) ; Define variable named lines and set line list from read_file function to lines
      (loop for line in lines ; Loop all line in lines
        do (setq token-list (gpplexer line)) ; Get list from split_line and set to wordlist
           (if (not (null token-list))
            (start token-list) ; Call controlLine function and send list to function
           )
      )
    )
  )
)

(defun menu ()
  (format t "If you wanna read from a file enter filename. Else enter start. ~%")
  (let ((rline) (choice)) ; Define variables
        (setq rline (read-line)) ; Get input from user
        (if (string= rline "start") ; If user enters start
            (setq choice 0) ; Set choice to 0
            (progn ; If user enters different than start
                (setq choice 1) ; Set choice to 1
                (setq *filename* rline) ; Set user input(new filename) to *filename* global variable
             )
        )
        (return-from menu choice) ; Return choice of user
    )
)

(setf choice (menu))
(if (= choice 0)
    (gppinterpreter))
(if (= choice 1)
    (gppinterpreter *filename*))