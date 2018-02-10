(setq input "
@import \"bootstrap\";

.test {
  color: red;
}

.test2 {
  background: green;
}
")

(setq len (length input))
(setq offset -1)
(setq lineno 1)
(setq column 0)
(setq buffer nil)
(setq returned nil)

(setq code 0)
(setq next 0)
(setq prev nil)
(setq strquote nil)
(setq lines nil)
(setq last nil)
(setq content nil)
(setq escape nil)
(setq nextline nil)
(setq nextoffset nil)
(setq escaped nil)
(setq escaped-position nil)
(setq n nil)
(setq current-token nil)

(defun hairy/css-mode-next-token ()
  "CSS Compiler tokenize."
  (let* ((SIGNLE_QUOTE ?\')
         (DOUBLE_QUOTE ?\")
         (BACKSLASH    ?\\)
         (SLASH        ?\/)
         )

    (setq code (aref input column))
    (when (or (eq ?\n code)
              (eq ?\t code)
              (and (eq ?\r code)
                   (not eq ?\n (aref input (1+ column)))))
      (setq offset column)
      (setq lineno (1+ lineno)))

    (cond
     ((or (eq ?\n code)
          (eq (string-to-char " ") code)
          (eq ?\t code)
          (eq ?\r code)
          (eq ?\f code))
      (setq next column)
      (while (progn
               (setq next (1+ next))
               (setq code (aref input next))
               (when (eq ?\n code)
                 (setq offset next)
                 (setq lineno (1+ lineno)))

               (or (eq ?\n code)
                   (eq (string-to-char " ") code)
                   (eq ?\t code)
                   (eq ?\r code)
                   (eq ?\f code))))
      (setq current-token `("space" ,(substring input column next)))
      (setq column (1- next)))

     ((eq ?\[ code)
      (setq current-token `("[" "[" ,lineno ,(- column offset))))
     ((eq ?\] code)
      (setq current-token `("]" "]" ,lineno ,(- column offset))))
     ((eq ?\{ code)
      (setq current-token `("{" "{" ,lineno ,(- column offset))))
     ((eq ?\} code)
      (setq current-token `("}" "}" ,lineno ,(- column offset))))
     ((eq ?\: code)
      (setq current-token `(":" ":" ,lineno ,(- column offset))))
     ((eq ?\; code)
      (setq current-token `(";" ";" ,lineno ,(- column offset))))

     ((eq ?\( code)

      )

     ((eq ?\) code)
      (setq current-token `(")" ")" ,lineno ,(- column offset)))
      )

     ((or (eq ?\' code)
          (eq ?\" code))
      (setq strquote (if (eq ?\' code) "'" "\""))
      (setq next column)

      (while (progn
               (setq escaped nil)
               (setq next (s-index-of strquote (1+ next)))
               (when (eq -1 next)
                 (error "Unclosed string"))
               (setq escaped-position next)
               (while (eq ?\\ (aref input (1- escaped-position)))
                 (setq escaped-position (1- escaped-position))
                 (setq escaped (not escaped)))
               escaped))

      (setq content (substring input column (1+ next)))
      (setq lines (split-string content "\n"))
      (setq last (1- (length lines)))

      (if (> last 0)
          (progn
            (setq nextline (+ lineno last))
            (setq nextoffset (- next (length (nth lines last)))))
        (progn
          (setq nextline lineno)
          (setq nextoffset offset)))

      (setq current-token `("string"
                            ,(substring input column (1+ next))
                            ,lineno
                            ,(- column offset)
                            ,lineno
                            ,(- next offset)))

      (setq offset nextoffset)
      (setq line nextline)
      (setq column next)
      )

     ((eq ?\@ code)
      (let* ((word-end-re (regexp-opt-charset `(,(string-to-char " ")
                                                ?\n
                                                ?\t
                                                ?\r
                                                ?\f
                                                ?\(
                                                ?\)
                                                ?\{
                                                ?\[
                                                ?\]
                                                ?\#
                                                ?\/
                                                ?\;
                                                ?\'
                                                ?\"
                                                ?\\)))
             (match-word (string-match word-end-re input (1+ column))))

        (if (not match-word)
            (setq next (1- len))
          (setq next (1-  match-word)))

        (setq current-token `("at-word"
                              ,(substring input column (1+ next))
                              ,lineno
                              ,(- column offset)
                              ,lineno
                              ,(- next offset)))
        (setq column next))
      )

     ((eq ?\\ code)
      (setq next column)
      (setq escape t)
      (while (eq ?\\ (aref input (1+ next)))
        (setq next (1+ next))
        (setq escape (not escape)))

      (setq code (aref input (1+ next)))

      (when (and escape (and (not (eq ?\/ code))
                             (not (eq (string-to-char " ") code))
                             (not (eq ?\n code))
                             (not (eq ?\t code))
                             (not (eq ?\r code))
                             (not (eq ?\f code))))
        (setq next (1+ next))
        (let ((hex-escape-re "[a-f0-9A-F]"))
          (when (string-match re (aref input next))
            (while (string-match re (aref input (1+ next)))
              (setq next (1+ next)))
            (when (eq (string-to-char " ") (aref input (1+ next)))
              (setq next (1+ next))))))
      )

     (t (if (and (eq ?\\ code)
                 (eq ?\* code))
            (progn
              (setq next (s-index-of "*/" (+ 2 column)))
              (when (eq next 0)
                (error "Unclosed comment"))

              (setq content (substring input column (1+ column)))
              ;; (setq lines (s-split ))
              (setq last (1- (length lines)))

              (if (> last 0)
                  (progn
                    (setq nextline (+ lineno last))
                    (setq nextoffset (- next (length lines)))
                    )
                (progn
                  (setq nextline lineno)
                  (setq nextoffset offset))
                )

              (setq current-token `("comment"
                                    ,content
                                    ,lineno
                                    ,(- column offset)
                                    ,nextline
                                    ,(- next nextoffset)))

              (setq offset nextoffset)
              (setq line nextline)
              (setq column next)
              )
          (progn
            (let* ((word-end-re (regexp-opt-charset `(,(string-to-char " ")
                                                      ?\n
                                                      ?\t
                                                      ?\r
                                                      ?\f
                                                      ?\(
                                                      ?\)
                                                      ?\{
                                                      ?\}
                                                      ?\:
                                                      ?\;
                                                      ?\@
                                                      ?\!
                                                      ?\'
                                                      ?\"
                                                      ?\\)))
                   (match-word (string-match word-end-re input (1+ column))))

              (if (not match-word)
                  (setq next (1- len))
                (setq next (1-  match-word)))

              (setq current-token `("word"
                                    ,(substring input column (1+ next))
                                    ,lineno
                                    ,(- column offset)
                                    ,lineno
                                    ,(- next offset)))

              (push current-token buffer)
              (setq column next)
              ))
          )))

    (setq column (1+ column))

    current-token))

(defun hairy-css-mode-eof? ()
  "EOF test"
  ;; return returned.length === 0 && pos >= length;
  (and ; (eq 0 (length returned))
   (> column len)))


(while (not (hairy-css-mode-eof?))
  (cl-prettyprint (hairy/css-mode-next-token)))
