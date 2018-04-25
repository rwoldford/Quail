(defun choose-variable (&optional dataset (n 1) msg menu-items)
  "Choose n variables from dataset"
  (setq msg (or msg
                (if (= n 1) "Choose a variable"
                    (format nil "Choose ~A  variables" n))))
  ( setq menu-items (or menu-items (if dataset (variable-menu-items dataset))))
    (if menu-items
      (if (= n 1)
        (second (select-from-list menu-items :prompt-text msg
                             :item-function #'first))
        (loop for result = (select-from-list menu-items :prompt-text msg
                                                :item-function #'first
                                                :selection-type :multi)
              when result append result into result-list and
           do (setq msg (format nil "Choose ~A  more" (- n (length result-list))))
              until (>= (length result-list) n )
              finally (return (subseq  (mapcar #'second result-list) 0 n))))
      (loop for i from 1 to n  collect
            (wb::prompt-user :type t :read-type :eval :prompt-string msg))))


(choose-variable q-user::apple)

(select-xy-variables q-user::apple)

(trace selected-data-of)