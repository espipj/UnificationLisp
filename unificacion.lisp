; Pablo Espinosa Bermejo
; Rafael González Martín
; © 2016: All Rights Reserved

; Pruebas:
; (load "unificacion.lisp")
; (unificacion '(P (? x) ((? g) (? x))) '(P A (? z)))
; (aplicar '((A barra (? x)) (B barra (? y)) (C barra (? w)) (D barra (? z))) '(((? g) ((? x)(? y))) (? z)))
; (componer '(((? g) ((? x)(? y))) barra (? z)) '((A barra (? x)) (B barra (? y)) (C barra (? w)) (D barra (? z))))

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
    (unless (equalp (valorVariable e1) (valorVariable e2)) ; Si e1=e2 no hacer nada
        (if (esVariable e1)
            (if (miembro e1 e2)
                 (return-from anadir 'fallo)
                 (list e2 'barra e1)
            )
            (if (esVariable e2)
                 (list e1 'barra e2)
                 (return-from anadir 'fallo)
            )
        )
    )
)

; Redefinición de la función miembro que evita fallos en caso de que e2 sea un átomo
(defun miembro (e1 e2)
    (unless (atom e2)
        (member e1 e2)
    )
)

; Función que extrae el valor de una variable o devuelve el valor de un átomo
(defun valorVariable (variable)
    (if (esVariable variable)
      (first(rest variable))
      variable
    )
)

; Devuelve t en caso de que el argumento sea una excepción
(defun esExpresion (expresion)
    (cond
        ((atomo expresion) nil)
        (t
            (equalp (first (rest expresion)) 'barra)
        )
    )
)

; Función principal
(defun unificacion (e1 e2)
    (cond
        ((or (null e1) (null e2))
            nil
        )
        ((atomo e1)
            (progn
                (anadir e1 e2)
            )
        )
        ((atomo e2)
            (progn
                (anadir e2 e1)
            )
        )
        (t
            (let ((f1 (first e1))
                  (t1 (rest e1))
                  (f2 (first e2))
                  (t2 (rest e2))
                  (z1 nil)
                  (z2 nil))
                (setf z1 (unificacion f1 f2))
                (if (equalp z1 'fallo)
                    (progn
                        (return-from unificacion 'fallo)
                    )
                )
                (setf g1 (aplicar z1 t1))
                (setf g2 (aplicar z1 t2))
                (setf z2 (unificacion g1 g2))
                (if (equalp z2 'fallo)
                    (return-from unificacion 'fallo)
                )
                (setf composicion (componer z1 z2))
                (return-from unificacion composicion)
            )
        )
    )
)

; Sustituye en lista siguiendo lo indicado en expresion
(defun aplicar (expresion lista)
    (cond
        ((or (null expresion) (atomo expresion)) lista)
        ((null lista) nil)
        ((not(equalp (first (rest expresion)) 'barra))
            (aplicar (rest expresion) (aplicar (first expresion) lista))
        )
        ((esVariable lista)
            (if (equalp lista (first (last expresion)))
                (first expresion)
                lista
            )
        )
        ((equalp lista (last expresion))
            (first expresion)
        )
        ((atom lista) lista)
        (t
            (setf parte1 (aplicar expresion (first lista)))
            (setf parte2 (aplicar expresion (rest lista)))
            (if (null parte1)
                parte2
                (if (null parte2)
                    parte1
                    (list
                        (aplicar expresion (first lista))
                        (aplicar expresion (rest lista))
                    )
                )
            )
        )
    )
)

; Función que inicia la composición, aplicando las expresiones y llamando a 'unir'
(defun componer (lista1 lista2)
    (cond
        ((null lista1) lista2)
        ((null lista2) lista1)
        (t
            (cond
                ((and (esExpresion lista1) (esExpresion lista2))
                    (if (equalp lista1 lista2)
                        lista1
                        (list lista1 lista2)
                    )
                )
                ((esExpresion lista1)
                    (progn
                        (setf listaBuena (list (aplicar lista2 (first lista1)) 'barra (first (last lista1))))
                        (flatten (list listaBuena (unir (first (last lista1)) lista2)))
                    )
                )
                ((esExpresion lista2)
                    (progn
                        (setf listaBuena (flatten (list (componer (first lista1) lista2) (componer (rest lista1) lista2))))
                        (flatten (list listaBuena lista2))
                    )
                )
            )
        )
    )
)

; Une los elementos de la composicón tras haber aplicado uno al otro
(defun unir (elemento lista)
    (cond
        ((null lista) nil)
        ((esExpresion lista)
            (if (equalp elemento (first (last lista)))
                    nil
                    lista
            )
        )
        (t
            (let ((parte1 nil)
                  (parte2 nil))
                (setf parte1 (unir elemento (first lista)))
                (setf parte2 (unir elemento (rest lista)))
                (if (null parte1)
                    (if (null parte2)
                        nil
                        parte2
                    )
                    (if (null parte2)
                        parte1
                        (flatten (list parte1 parte2))
                    )
                )
            )
        )
    )
)

; Función que elimina paréntesis innecesarios
(defun flatten (l)
    (cond ((null l) nil)
        ((atom l) (list l))
        ((esExpresion l) (list l))
        (t
            (loop for a in l appending (flatten a))
        )
    )
)
