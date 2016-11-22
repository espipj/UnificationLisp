; Pablo Espinosa Bermejo
; Rafael González Martín
; © 2016: Todos los derechos reservados

; Prueba:
; (unificacion '(P '(? x) '((? g) '(? x))) '(P A '(? z)))

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
    (format t "Anadir: E1=~a, E2=~b~%" e1 e2)
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

; Función que extrae el valor de una variable o devuelve el valor de un átomo
(defun valorVariable (variable)
    (if (esVariable variable)
      (first(rest variable))
      variable
    )
)

(defun unificar (e1 e2)
    (cond
        ((or (null e1) (null e2))
            (throw 'unificacionException 'Variable-nula-No-Unificable)
        )
        ; ((and (atom e1) (atom e2))
        ;     (throw 'unificacionException 'Dos-atomos-No-Unificable)
        ; )
        ((atomo e1)
            (progn
                (format t "Atomo e1? E1=~a, E2=~b~%" e1 e2)
                (anadir e1 e2)
            )
        )
        ((atomo e2)
            (progn
                (format t "Atomo e2? E1=~a, E2=~b~%" e1 e2)
                (anadir e2 e1)
            )
        )
        (t
            ; e1 y e2 son listas
            (format t "Linea 12~%")
            (let ((f1 (first e1))
                  (t1 (rest e1))
                  (f2 (first e2))
                  (t2 (rest e2)))
                (setf z1 (unificar f1 f2))
                (format t "z1 = ~a~%" z1)
                (if (equalp z1 'fallo)
                    (progn
                        (format t "Falla aqui~%")
                        (return-from unificar 'fallo)
                    )
                )
                (setf g1 (aplicar z1 t1))
                (format t "g1 = ~g~%" g1)
                (setf g2 (aplicar z1 t2))
                (format t "g2 = ~g~%" g2)
                (setf z2 (unificar g1 g2))
                (format t "z2 = ~a~%" z2)
                (if (equalp z2 'fallo)
                    (return-from unificar 'fallo)
                )
                (setf composicion (list z1 z2))
                (format t "Composicion=~c~%" composicion)
                (return-from unificar composicion)
            )
        )
    )
)

; Función que sustituye en lista siguiendo lo indicado en expresion
(defun aplicar (expresion lista)
    (cond
        ((null expresion) lista)
    )
)
