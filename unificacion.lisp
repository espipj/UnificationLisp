; Pablo Espinosa Bermejo
; Rafael González Martín
; © 2016: Todos los derechos reservados

; Prueba:
; (load "unificacion.lisp")
; (unificacion '(P (? x) ((? g) (? x))) '(P A (? z)))

; (aplicar '((A barra (? x)) (B barra (? y)) (C barra (? w)) (D barra (? z))) '(((? g) ((? x)(? y))) (? z)))
; (componer ('(((? g) ((? x)(? y))) barra (? z)))

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
    ; (format t "Anadir: E1=~a, E2=~b~%" e1 e2)
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

(defun unificar (e1 e2)
    (cond
        ((or (null e1) (null e2))
            ; (throw 'unificacionException 'Variable-nula-No-Unificable)
            nil
        )
        ; ((and (atom e1) (atom e2))
        ;     (throw 'unificacionException 'Dos-atomos-No-Unificable)
        ; )
        ((atomo e1)
            (progn
                ; (format t "Atomo e1? E1=~a, E2=~b~%" e1 e2)
                (anadir e1 e2)
            )
        )
        ((atomo e2)
            (progn
                ; (format t "Atomo e2? E1=~a, E2=~b~%" e1 e2)
                (anadir e2 e1)
            )
        )
        (t
            ; e1 y e2 son listas
            ; (format t "Linea 12~%")
            (let ((f1 (first e1))
                  (t1 (rest e1))
                  (f2 (first e2))
                  (t2 (rest e2))
                  (z1 nil)
                  (z2 nil))
                (setf z1 (unificar f1 f2))
                ; (format t "z1 = ~a~%" z1)
                (if (equalp z1 'fallo)
                    (progn
                        ; (format t "Falla aqui~%")
                        (return-from unificar 'fallo)
                    )
                )
                (setf g1 (aplicar z1 t1))
                ; (format t "g1 = ~g~%" g1)
                (setf g2 (aplicar z1 t2))
                ; (format t "g2 = ~g~%" g2)
                (setf z2 (unificar g1 g2))
                ; (format t "z2 = ~a~%" z2)
                (if (equalp z2 'fallo)
                    (return-from unificar 'fallo)
                )

                ; (format t "z1 = ")
                ; (print z1)
                ; (format t "z2 = ")
                ; (print z2)

                (setf composicion (componer z1 z2))
                ; (format t "composicion = ")
                ; (print composicion)
                (return-from unificar composicion)
            )
        )
    )
)

; Función que sustituye en lista siguiendo lo indicado en expresion
(defun aplicar (expresion lista)
    (format t "Lista:")
    (print lista)
    (format t "~%")
    (format t "Expresion:")
    (print expresion)
    (format t "~%")
    (cond
        ((or (null expresion) (atomo expresion)) lista)
        ((null lista) nil)
        ((not(equalp (first (rest expresion)) 'barra))
            (format t "A~%")
            (aplicar (rest expresion) (aplicar (first expresion) lista))
            ; (setf parte1 (aplicar (first expresion) lista))
            ; (if (null parte1)
            ;     (aplicar (rest expresion) lista)
            ;     (progn
            ;         (setf parte2 (aplicar (rest expresion) parte1))
            ;         (if (null parte2)
            ;             lista
            ;             parte2
            ;         )
            ;     )
            ; )
        )
        ; IMPORTANTE
        ; ((equalp (first (rest lista)) 'barra)
        ;
        ; )

        ; ((atomo lista)
        ;     (if (equalp lista (first (last expresion)))
        ;         (first expresion)
        ;         lista
        ;     )
        ; )
        ; ((equalp lista (last expresion))
        ;     (first expresion)
        ; )
        ((esVariable lista)
          (format t "B~%")
            (if (equalp lista (first (last expresion)))
                (progn (format t "sustitucion: ") (print (first expresion)) (first expresion))
                (progn (format t "no sustitucion~%") lista)
            )
        )
        ((equalp lista (last expresion))
            (format t "C~%")
            (first expresion)
        )
        ((atom lista) lista)
        (t
            (format t "Default~%")
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
            ; (list
            ;     (aplicar expresion (first lista))
            ;     (aplicar expresion (rest lista))
            ; )
        )
    )
)

;Aqui fallecio Agregación

(defun componer (lista1 lista2)
    (cond
        ((null lista1) lista2)
        ((null lista2) lista1)
        (t
            ; (aplicar lista2 lista1)
            (if (equalp (first (rest lista1)) 'barra)
                (setf listaBuena (list (aplicar lista2 (first lista1)) (first(rest lista1)) (first (rest (rest lista1)))))
                (list (componer (first lista1) lista2) (componer (rest lista1) lista2))
            )
            (list listaBuena lista2)
        )
    )
)
; ((A BARRA (? X)) (B BARRA (? Y)) (C BARRA (? W)) (D BARRA (? Z)))
