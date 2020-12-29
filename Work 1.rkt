#lang racket

;**************************************************
;* Christian Camilo Taborda Campiño 1632081-3743  *
;* Cristian Camilo Vallecilla Cuellar 1628790-3743*
;* Esneider Arbey Arango Manzano 1628373-3743     *
;**************************************************

;------------------// TALLER I //------------------

;-- 1.) calcularArea : número número símbolo --> número
;----// Calcula el área de una figura a partir de sus lados.      

(define calcularArea
  (lambda (base altura figura)
    (cond
      [(symbol=? figura 'cuadrado) (* base base)]
      [(symbol=? figura 'triangulo) (/ (* base altura) 2)]
      [(symbol=? figura 'rectangulo) (* base altura)]
      [else "Error en el ingreso de parámetros."])))

;-- 2.) calcularFibonacci : número --> número
;----// Calcula el fibonacci de un número.

(define fibonacci
  (lambda (posicion)
    (cond
      [(= posicion 0) 0]
      [(= posicion 1) 1]
      [else (+ (fibonacci (- posicion 1)) (fibonacci (- posicion 2)))])))

;-- 3.) calculadoraDeListas : lista --> número
;----// Realiza un conjunto de cálculos recibidos en forma de lista.

(define calculadoraDeListas
  (lambda (lista)
    (cond
      [(symbol=? (car lista) '+) (+ (cadr lista) (if (cons? (caddr lista)) (calculadoraDeListas (caddr lista)) (caddr lista)))]
      [(symbol=? (car lista) '-) (- (cadr lista) (if (cons? (caddr lista)) (calculadoraDeListas (caddr lista)) (caddr lista)))]
      [(symbol=? (car lista) '*) (* (cadr lista) (if (cons? (caddr lista)) (calculadoraDeListas (caddr lista)) (caddr lista)))]
      [(symbol=? (car lista) '/) (/ (cadr lista) (if (cons? (caddr lista)) (calculadoraDeListas (caddr lista)) (caddr lista)))]
      [else "Error en el ingreso de parámetros."])))

;-- 4.)

; * buscarEnLista : valor lista --> booleano
;----// Indica si un elemento se encuentra dentro de una lista.

(define buscarEnLista
  (lambda (valor lista)
    (cond
      [(empty? lista) #f]
      [(cons? lista) (if (equal? valor (car lista)) #t (buscarEnLista valor (cdr lista)))]
      [else "Error en el ingreso de parámetros."])))

; * eliminarDeLista : valor lista --> lista
;----// Elimina las ocurrencias de un elemento de una lista.

(define eliminarDeLista
  (lambda (valor lista)
    (cond
     [(not (buscarEnLista valor lista)) lista]
     [(buscarEnLista valor lista) (if (equal? (car lista) valor) (eliminarDeLista valor (cdr lista)) (cons (car lista) (eliminarDeLista valor (cdr lista))))]
     [else "Error en el ingreso de parámetros."])))

;------------------// EJERCICIO DE FÚTBOL //------------------

; 1.) Definición de la estructura : (make-partido string string número número número número)

(define-struct partido (equipo1 equipo2 puntos1 puntos2 goles1 goles2))

; * extraerEquipos : lista --> lista
;----// Extrae la lista de equipos de un torneo de fútbol.

(define extraerEquipos
  (lambda (torneo)
    (cond
      [(empty? torneo) empty]
      [(cons? torneo) (cons (partido-equipo1 (car torneo)) (cons (partido-equipo2 (car torneo)) (extraerEquipos (cdr torneo))))])))

; * extraerPuntos : lista lista --> lista
;----// Retorna una lista con la cantidad de puntos de los equipos dentro de un torneo.

(define extraerPuntos
  (lambda (equipos torneo)
    (cond
      [(empty? equipos) empty]
      [(cons? equipos) (cons (obtenerPuntosTorneo torneo (car equipos)) (extraerPuntos (cdr equipos) torneo))])))

; * extraerMejorEquipo : número lista lista --> string
;----// Compara un puntaje con los puntajes de una lista de equipos para retornar el equipo con dicho puntaje.

(define extraerMejorEquipo
  (lambda (puntaje equipos torneo)
    (cond
      [(empty? equipos) ""]
      [(cons? equipos) (if (= puntaje (obtenerPuntosTorneo torneo (car equipos))) (car equipos) (extraerMejorEquipo puntaje (cdr equipos) torneo))])))

; 2.) mayorPuntosEquipo : lista --> string
;----// Retorna el equipo con más puntos del torneo.

(define mayorPuntosEquipo
  (lambda (torneo)
    (extraerMejorEquipo (car (sort (extraerPuntos (extraerEquipos torneo) torneo) >)) (extraerEquipos torneo) torneo)))

; 3.) calcularEmpates : lista --> número
;----// Retorna la cantidad de empates dentro de un torneo de fútbol.

(define calcularEmpates
  (lambda (torneo)
    (cond
     [(empty? torneo) 0]
     [(cons? torneo) (if (= (partido-goles1 (car torneo)) (partido-goles2 (car torneo)))
                       (+ 1 (calcularEmpates (cdr torneo))) (calcularEmpates (cdr torneo)))])))

; 4.) calcularGoles : lista --> número
;----// Retorna la cantidad de goles dentro de un torneo fútbol.

(define calcularGoles
  (lambda (torneo)
    (cond
      [(empty? torneo) 0]
      [(cons? torneo) (+ (partido-goles1 (car torneo)) (partido-goles2 (car torneo)) (calcularGoles (cdr torneo)))])))

; * obtenerPuntosPartido : partido string --> número
;----// Retorna la cantidad de puntos de un equipo dentro de un partido.

(define obtenerPuntosPartido
  (lambda (partido equipo)
    (cond
      [(equal? equipo (partido-equipo1 partido)) (partido-puntos1 partido)]
      [(equal? equipo (partido-equipo2 partido)) (partido-puntos2 partido)]
      [else 0])))

; 5.) obtenerPuntosTorneo : lista string --> número
;----// Retorna la cantidad de puntos de un equipo dentro de un torneo.

(define obtenerPuntosTorneo
  (lambda (torneo equipo)
    (cond
      [(empty? torneo) 0]
      [(cons? torneo) (+ (obtenerPuntosPartido (car torneo) equipo) (obtenerPuntosTorneo (cdr torneo) equipo))])))

;----// TOMANDO LA MAYOR CANTIDAD DE GOLES ANOTADA POR UN EQUIPO EN EL TORNEO //----

; * obtenerGolesPartido : partido string --> número
;----// Retorna la cantidad de goles de un equipo dentro de un partido.

(define obtenerGolesPartido
  (lambda (partido equipo)
    (cond
      [(equal? equipo (partido-equipo1 partido)) (partido-goles1 partido)]
      [(equal? equipo (partido-equipo2 partido)) (partido-goles2 partido)]
      [else 0])))

; * obtenerGolesTorneo : lista string --> número
;----// Retorna la cantidad de goles de un equipo dentro de un torneo.

(define obtenerGolesTorneo
  (lambda (torneo equipo)
    (cond
      [(empty? torneo) 0]
      [(cons? torneo) (+ (obtenerGolesPartido (car torneo) equipo) (obtenerGolesTorneo (cdr torneo) equipo))])))

; * extraerGoles : lista lista --> lista
;----// Retorna una lista con la cantidad de goles de los equipos dentro de un torneo.

(define extraerGoles
  (lambda (equipos torneo)
    (cond
      [(empty? equipos) empty]
      [(cons? equipos) (cons (obtenerGolesTorneo torneo (car equipos)) (extraerGoles (cdr equipos) torneo))])))

; 6.) mayorGolesEquipo : lista --> número
;----// Retorna la cantidad más alta de goles anotada por un equipo dentro de un torneo.

(define mayorGolesEquipo
  (lambda (torneo)
    (car (sort (extraerGoles (extraerEquipos torneo) torneo) >))))

;----// TOMANDO LA MAYOR CANTIDAD DE GOLES ANOTADA EN UN PARTIDO DEL TORNEO //----

; * extraerGolesParciales : lista --> lista
;----// Retorna una lista con el total de goles anotados por partido.

(define extraerGolesParciales
  (lambda (torneo)
    (cond
      [(empty? torneo) empty]
      [(cons? torneo) (cons (+ (partido-goles1 (car torneo)) (partido-goles2 (car torneo))) (extraerGolesParciales (cdr torneo)))])))

; 6.) mayorGolesPartido : lista --> número
;----// Retorna la cantidad más alta de goles anotada en un partido de un torneo.

(define mayorGolesPartido
  (lambda (torneo)
    (car (sort (extraerGolesParciales torneo) >))))
    

      