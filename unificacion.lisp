; Función que intercambia las variables para que e1 sea átomo
;Prueba commit pablo
(defun intercambiar (e1 e2)
    (if (atomo e1)
        (hacercosas e1 e2)
        (if (atomo e2)
            (progn
                (setf temp e2)
                (setf e2 e1)
                (setf e1 temp)
                (hacercosas e1 e2)
            )
            (format t "Salto a 12") ; Salto a linea 12 en lugar del if de arriba
        )
    )
)

; Función que comprueba si var es un átomo
(defun atomo (var)
    (cond ((atom var) t)
        ((eq (first var) '?) t)
        (t nil)
    )
)

; Comprueba si hay algo detras de '?
(defun esvariable (var)

)

; Función que hace cosas
(defun hacercosas (e1 e2)
    (format t "E1=~a, E2=~b" e1 e2)
    (if (equalp e1 e2) ; Si e1=e2 no hacer nada
        ; (return 'nada)
        ; (if (esvariable e1)
        ;     (if (member e1 e2)
        ;          (return 'fallo)
        ;          (return 'e2/e1)
        ;     )
        ;     (if (esvariable e2)
        ;          (return 'e1/e2)
        ;          (return 'fallo)
        ;     )
        ; )
    )
)
