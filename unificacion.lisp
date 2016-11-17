; Función principal que captura excepciones
(defun unificacion (e1 e2)
    (catch 'unificacionException (unificar e1 e2))
)

; Función que comprueba si var es un átomo
(defun atomo (var)
    (cond ((atom var) t)
        ((eq (first var) '?) t)
        (t nil)
    )
)

; Comprueba si hay algo detras del '?
(defun esVariable (var)
    (unless (atom var)
        (cond ((eq (first var) '?) t)
            (t nil)
        )
    )
)

; Función que hace cosas siendo e1 atomo
(defun anadir (e1 e2)
    (format t "E1=~a, E2=~b" e1 e2)
    (unless (equalp (valorVariable e1) (valorVariable e2)) ; Si e1=e2 no hacer nada
        (if (esVariable e1)
            (if (member e1 e2) ; Member es recursivo??
                 (return-from anadir 'fallo)
                 (return-from anadir 'e2/e1)
            )
            (if (esVariable e2)
                 (return-from anadir 'e1/e2)
                 (return-from anadir 'fallo)
            )
        )
    )
)

(defun valorVariable (variable)
    (if (esVariable variable)
      (first(rest variable))
      variable
    )
)

(defun unificar (e1 e2)
    (cond
        (if (atomo e1)
            (if (atomo e2)
                (throw 'unificacionException 'Dos-atomos-No-Unificable)
                (anadir e1 e2)
            )
            (if (atomo e2)
                (anadir e2 e1)
            )
        )
        (t
          ; e1 y e2 son listas
          (format t "Linea 12")
        )
    )
)
