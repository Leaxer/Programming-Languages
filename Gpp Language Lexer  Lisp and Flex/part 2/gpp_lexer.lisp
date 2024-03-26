(defvar Keywords (list "and" "or" "not" "equal" "less" "nil" "list" "append" "concat" "set"
                       "def" "for" "if" "exit" "load" "display" "true" "false")) ; List of keywords
(defvar KeywordsTokens (list "KW_AND" "KW_OR" "KW_NOT" "KW_EQUAL" "KW_LESS" "KW_NIL" "KW_LIST" "KW_APPEND" "KW_CONCAT" "KW_SET"
                              "KW_DEF" "KW_FOR" "KW_IF" "KW_EXIT" "KW_LOAD" "KW_DISPLAY" "KW_TRUE" "KW_FALSE")) ; List of keyword tokens

(defvar Operators(list "+" "-" "/" "*" "(" ")" ",")) ; List of operators
(defvar OperatorTokens (list "OP_PLUS" "OP_MINUS" "OP_DIV" "OP_MULT" "OP_OP" "OP_CP" "OP_COMMA")) ; List of operator tokens


(defvar commentWord ";;")
(defvar commentToken "COMMENT")

(defvar *filename*)

(defun isKeyword (word) ; Controls the word,if it's equal to any word in Keywords.
    (let ((index 0)) ; The corresponding index of word in the Token list.
      (loop for keyword in Keywords ; Loop all keywords in Keywords list.
            do(if(string= word keyword) ; If parameter word is equal to current keyword.
                  (return-from isKeyWord index) ; Return current index.
              )
              (setf index (+ index 1)) ; Increases the index for the number that corresponds to the next Keyword.
      )
    )
    (return-from isKeyWord -1) ; If nothing can be found, it returns -1 to indicate that there is no index.
)
(defun isOperator (word) ; Controls the word,if it's equal to any word in Operators.
    (let ((index 0)) ; The corresponding index of word in the OperatorTokens list.
      (loop for operator in Operators ; Loop all operators in Operators list.
            do(if(string= word operator) ; If parameter word is equal to current operator.
                  (return-from isOperator index) ; Return current index.
              )
              (setf index (+ index 1)) ; Increases the index for the number that corresponds to the next Operator.
      )
    )
    (return-from isOperator -1) ; If nothing can be found, it returns -1 to indicate that there is no index.
)
(defun isValue (word)
  (if (not (digit-char-p (char word 0))) ; If first character of word is not digit
    (return-from isValue -1)
  )
  (let ((len (- (length word) 1))) ; Length of word and decreases one
    (loop for i from 1 to len ; Loop from second character of word to the last character
      do(if (not (digit-char-p (char word i))) ; If the current character of the word is not equal to the digit, that is, the first digits of fraction is over.
          (if (and (char= (char word i) #\b) (/= i len)) ; If current character of word is not equal to 'b' and it's not last character
            (progn
              (loop for j from (+ i 1) to len ;  Loop from first character after b to the last character
                do (if (not (digit-char-p (char word j))) ; If current character is not digit
                      (return-from isValue -1) ; Returns -1 to indicate that word is not value.
                   )
              )
              (return-from isValue 1) ; Returns -1 to indicate that word is not value.
            )
            (return-from isValue -1) ; Returns -1 to indicate that word is not value.
          )
        )
    )
  )
  -1
)
(defun isIdentifier (word)
  (if   (alpha-char-p (char word 0)) ; Check if the first character is alphabetical
        (loop for i from 1 to (- (length word) 1) ; Loop from second character of word to last character
            do(if (not (or (alpha-char-p (char word i)) (digit-char-p (char word i)))) ; If current character is not digit or alphabetical character
                  (return-from isIdentifier -1) ; Returns -1 to indicate that word is not identifier
              )
        )
        (return-from isIdentifier -1) ; Returns -1 to indicate that word is not identifier
  )
  (return-from isIdentifier 1) ; Returns -1 to indicate that word is not identifier
)

(defun controlLine (words)
  (loop for word in words ; Loop every word in words
    do  (let ((index 0))
            (cond
                ((string= word commentWord) ; If word is comment
                    (format t "~s~%" commentToken) ; Prints commentToken
                )
                ((/= (setq index (isKeyWord word)) -1) ; Sets index's new value and controls if index is not -1(means that word is a keyword)
                    (format t "~s~%"  (nth index KeywordsTokens)) ; Prints the value corresponding to the index in Keyword Tokens list.
                )
                ((/= (setq index (isOperator word)) -1) ; Sets index's new value and controls if index is not -1(means that word is a operator)
                    (format t "~s~%"  (nth index OperatorTokens)) ; Prints the value corresponding to the index in Operator Tokens list.
                )
                ((/= (setq index (isValue word)) -1) ; Sets index's new value and controls if index is not -1(means that word is a value)
                    (format t "VALUEF~%") ; Prints VALUEF
                )
                ((/= (setq index (isIdentifier word)) -1) ; Sets index's new value and controls if index is not -1(means that word is a identifier)
                    (format t "IDENTIFIER~%") ; Prints IDENTIFIER
                )
                ( t ; If the word is none of the above
					        (format t "~%SYNTAX_ERROR: ~A cannot be tokenized~c" word #\NewLine) ; Prints syntax error mesage and current word
				        )
            )
        )
    )
)

(defun split_line(input)
  (let ((word "") (wordlist '()) (index 0)) ; Define empty string, empty list and int named index
    (loop for char across input ; Loop every character in input
          do    (if (char= char #\;) ; If character is equal to ';'
                  (if (and (/= index (- (length input) 1))(char= (char input (+ index 1)) #\;))  ; Controls current character is not last character and next character is equal to ';'
                    (progn
                      (if (string/= word "") ; If word before ;; is not empty
                          (push word wordlist) ; Add word to wordlist
                      )
                      (push ";;" wordlist) ; Add comment word to wordlist
                      (return-from split_line (reverse wordlist)) ; Reverse wordlist and return
                    )
                  )
                )
                (if (find char '(#\Space #\Newline #\Tab #\( #\))) ; If character is equal to ' ', '\n' or '\t'
                  (progn
                    (if (string/= word "") ; If word is not empty
                        (progn
                            (push word wordlist) ; If word before ;; is not empty
                            (setf word "") ; Set word to empty
                        )
                    )
                        (if (or (char= char #\() (char= char #\))) ; If character is equal to '(' or ')'
                            (push char wordlist) ; Add current char( '(' or ')' ) to wordlist
                        )
                  )
                  (setf word (concatenate 'string word (string char))) ; Combine the word with the current character.
                )
                (setf index (+ index 1)) ; Increases the index for the number that corresponds to the next character.
    )
                (and (string/= word " ") (string/= word "\t") (string/= word "") ; Control last word if it's not equal to ' ', '\n' or '\t'
                  (push word wordlist) ; Add last word to wordlist
                )
                (reverse wordlist) ; Reverse wordlist, list will return since it is the last code of the function
  )
)

(defun read_file (file_name)
  (with-open-file (file file_name :direction :input) ; Open file
    (loop for line = (read-line file nil) ; Loop every line in file
          while line
          collect line) ; Collect all lines and return line list at the end
  )
)


(defun gppinterpreter (&optional filecont)
  (if (null filecont) 
    (progn ; If there is no parameter sent
      (setq wordlist (split_line (read-line))) ; Get list from split_line and set to wordlist
      (controlLine wordlist) ; Call controlLine function and send list to function
    )
    (let ((lines (read_file filecont))) ; Define variable named lines and set line list from read_file function to lines
      (loop for line in lines ; Loop all line in lines
        do(setq wordlist (split_line line)) ; Get list from split_line and set to wordlist
          (controlLine wordlist) ; Call controlLine function and send list to function
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