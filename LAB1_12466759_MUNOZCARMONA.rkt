
#lang racket
;;TDA
;; Type System = Name (String) X Users (list string) X Drives (list drive) X Current-user (String) X current-drive(char) X current-path(String carpeta de trabajo) X Archivos/Carpetas (list archivos-carpetas)
;; Type archivos-carpetas= (drive-path) x (archivo de ese path) x extension-archivo x fecha x usuario)
;; Type drive= char x string x number
;; Type Papelera = '(drive-path (archivo de ese path) extension-archivo  fecha  usuario)  / asociado a los archivos eliminador y no forma parte del sistema




;; RF2 - Constructor inicial: System sera lista de: Nombre x drives x Usuasios x Folders
(define (system Name)           
   (list Name '() '() " " '() " " '()))

(define (make-system Name Users Drives CurrentUser CurrentDrive CurrentPath Archivos/carpetas)
   (list Name Users Drives CurrentUser CurrentDrive CurrentPath Archivos/carpetas))


;; RF3 -  RUN:Permite ejecutar un comando sobre el sistema
 (define (run system cmd)
   (cmd system))


;;RF4 - add-drive:Permite agregar un drive
;; Type System = Name (String) X Users (list string) X Drives (list drive) X Current-user (String) X current-drive(char) X current-path(String) X Archivos/Carpetas (list archivos-carpetas)
(define get-Name-system car) ;; permite acceder al nombre del sistema 
(define get-Users-system cadr) ;; permite acceder al listado de usuarios
(define get-Drives-system caddr) ;; permite acceder a la lista de drives
(define get-CurrentUser-system cadddr) ;; permite acceder al listado de usuarios
(define (get-CurrentDrive-system system) (car (cdr (cdr (cdr (cdr system ))))))  ;; permite acceder al usuario actual
(define (get-currentPath-system system) (car (cdr (cdr (cdr (cdr (cdr  system   ))))))) ;; permite acceder a la ruta actual
(define (get-Archivos/carpetas system) (car (cdr (cdr (cdr (cdr (cdr (cdr system   )))))))) ;; permite acceder a la lista de archivos y carpetas


;; Se crea función para evitar agregar el mismo drives con el mismo nombre
(define (RepiteDrv? Char listDrive Contador)
  (if (null? listDrive) Contador
      (if (char=?   Char  (car (car listDrive))  )
          (RepiteDrv? Char (cdr listDrive) (+ 1 Contador))
          (RepiteDrv? Char (cdr listDrive) Contador ))))



(define ((add-drive system) drive namedrive capacidad )  ;; es el drive (que es una lista con letra xnombre x capacidad) que se quiere agregar
 (if ( = (RepiteDrv? drive (get-Drives-system system) 0) 1 )
 (make-system                    
  (get-Name-system system)
  (get-Users-system system)
  (get-Drives-system system)
  (get-CurrentUser-system system)
  (get-CurrentDrive-system system)
  (get-currentPath-system system)
  (get-Archivos/carpetas system)
  )
 
  (make-system                    
  (get-Name-system system)
  (get-Users-system system)
  (cons (list drive namedrive capacidad)(get-Drives-system system))
  (get-CurrentUser-system system)
  (get-CurrentDrive-system system)
  (get-currentPath-system system)
  (get-Archivos/carpetas system)
  )))


;;RF5 - add-users:Permite agregar un nuevo usuario
;; Se crea función para evitar agregar UN uSER con el mismo nombre

(define (existe?  listName  Name Acc)                        
  (if (null? listName) Acc
      (if (string=? (car listName) Name)
          (existe? (cdr listName) Name (+ 1 Acc))
          (existe? (cdr listName) Name Acc))))


(define ((add-user system) NewUser )
(if (> (existe?  (get-Users-system system)  NewUser  0)  0) system
 (make-system                    
  (get-Name-system system)
  (cons NewUser (get-Users-system system))
  (get-Drives-system system)
  (get-CurrentUser-system system)
  (get-CurrentDrive-system system)
  (get-currentPath-system system)
  (get-Archivos/carpetas system))))
 

;;RF6 - Función login: que permite iniciar sesión con un usuario del sistema, solo si éste existe.
(define ((login system) NameUser)
 (if (>(existe? (get-Users-system system) NameUser 0) 0)
 (make-system                    
 (get-Name-system system)
 (get-Users-system system)
 (get-Drives-system system)
  NameUser
 (get-CurrentDrive-system system)
 (get-currentPath-system system)
 (get-Archivos/carpetas system))
 system))


;;;;RF7 - Función logout: permite cerrar la sesión de un usuario en el sistema.
(define (logout system)
 (make-system                    
 (get-Name-system system)
 (get-Users-system system)
 (get-Drives-system system)
  " "
 (get-CurrentDrive-system system)
 (get-currentPath-system system)
 (get-Archivos/carpetas system)))



;;RF8.-Permite fijar la unidad en la que el usuario realizará acciones. 
;;La función solo debe funcionar cuando hay un usuario con sesión iniciada en el sistema a partir de la función descrita en 6.
(define (existeDrive?  list Drive Acc)               ;; Funcion auxiliar especifica para ver si exite el drive a activar           
  (if (null? list) Acc
      (if (char=? (car (car list)) Drive)
          (existeDrive? (cdr list) Drive (+ 1 Acc))
          (existeDrive? (cdr list) Drive Acc))))

(define ((switch-drive system) CharDrive)
  (if (string=? (get-CurrentUser-system system) " ")
    system
     (if (>(existeDrive? (get-Drives-system system) CharDrive 0)0)
       (make-system                    
          (get-Name-system system)
          (get-Users-system system)
          (get-Drives-system system)
          (get-CurrentUser-system system)
           CharDrive
          (string CharDrive #\: #\/)
          (get-Archivos/carpetas system))
         system)))
  


;;RF9.- md: Permite crear un directorio dentro de una unidad a partir del nombre especificado.
;;Se crea función para determinar si esta duplicada en el drive
(define (CarpetaDuplicada? Pathfolder listAC Acc)
   (if (null? listAC) Acc
       (if (string=? Pathfolder (car (car listAC)))
           (CarpetaDuplicada? Pathfolder (cdr listAC) ( + Acc 1))
           (CarpetaDuplicada? Pathfolder (cdr listAC) Acc))
       ))


(define ((cd system) Name)
   (if  (= (CarpetaDuplicada? (string-append (string (get-CurrentDrive-system system) #\: #\/) Name "/")  (get-Archivos/carpetas system) 0) 0)
   (make-system                    
  (get-Name-system system)
  (get-Users-system system)
  (get-Drives-system system)
  (get-CurrentUser-system system)
  (get-CurrentDrive-system system)
  (string-append (string (get-CurrentDrive-system system) #\: #\/) Name "/")  ;;evalaur si si drive se cambia por path
  (cons (list (string-append (string (get-CurrentDrive-system system) #\: #\/) Name "/") '() " " (current-seconds) (get-CurrentUser-system system))(get-Archivos/carpetas system)))

   (make-system                    
  (get-Name-system system)
  (get-Users-system system)
  (get-Drives-system system)
  (get-CurrentUser-system system)
  (get-CurrentDrive-system system)
  (get-currentPath-system system)
  (get-Archivos/carpetas system))))


;;R10: cd función que permite cambiar la ruta (path) donde se realizarán operaciones
(define (count-elements lst)                                                  ;; función que cuenta los elementos de una lista
  (if (null? lst)
      0
      (+ 1 (count-elements (cdr lst)))))

(define (substring? str sub)                                                 ;; Funcion que permite saber si un string (sub) es parte de otro (str)
  (cond ((null? sub) #t) 
        ((string-prefix? sub str) #t) 
        (else (substring? (substring str 1) sub)))) 


(define (PathInverse list listFinal)                                         ;; Funcion que a la lista anterior le incluye los "/" pero los deja invertidos
    (if (null? list) listFinal
        (PathInverse (cdr list) (cons (car list)(cons "/" listFinal)))))

(define (BackRoot string)                                                    ;; Función que determina si es carpeta o archivo a partir del ultimo caracter de la ruta y los lleva a una list de string sin "/"
   (string-append (car (string-split string "/")) "/" ))


(define ((cd2 system) Comando)
(make-system                    
  (get-Name-system system)
  (get-Users-system system)
  (get-Drives-system system)
  (get-CurrentUser-system system)
  (get-CurrentDrive-system system)
  (cond
    [(string=? Comando "..")
     (if (= 1(count-elements (string-split (get-currentPath-system system) "/")))
      (get-currentPath-system system)
      (string-join (PathInverse (cdr(reverse(string-split (get-currentPath-system system) "/"))) '() ) ""))]
    [(string=? Comando "/")  (BackRoot(get-currentPath-system system)) ]
    [(string? Comando)  (string-append (get-currentPath-system system) Comando "/")])                ;; falta asegurarse que la carpeta no se repite verificando con con current path
  
  (get-Archivos/carpetas system)))


;;R11: add-file función que permite añadir un archivo en la ruta actual.
;;Se define el constructor del files
(define (file filename extension contenido . atributos)
  (list filename extension contenido atributos))

;;Filter para seleccionar los registros de la base Archivos/carpetas asociados al current-path (Este archivo puede usarse en requerimientos posteriores.
 (define (filterAC listAC CurrentPath)      ;;listAC tiene por objetivo guardar los elementos de Archivos/carpetas
  (if (null? listAC) null
      (if (string=? (car (car listAC)) CurrentPath)
           (cons (car listAC) (filterAC (cdr listAC) CurrentPath))
           (filterAC (cdr listAC) CurrentPath)   )))


(define ((add-file system) list-file)
  (filterAC (get-Archivos/carpetas system) (get-currentPath-system system))
  (list (string-append (car (car(filterAC (get-Archivos/carpetas system) (get-currentPath-system system))))) list-file (cadr list-file) (car(filterAC (get-Archivos/carpetas system) (get-currentPath-system system))) (get-CurrentUser-system system)))
  



;;R12;



;;R13: 
;;Filtra los registros en "Archivos/carpetas" cuyo path contenga el nombre de la carpeta y que tengan archivos
(define (CarpetaVacia? ListAC NameCarpeta Acc)        ;; De manera que si Acc > 1 entonces la carpeta no esta vacía 
(if (null? ListAC) Acc
  (if (boolean=? (memq NameCarpeta (string-split (car(car ListAC)) "/")) false)
      (CarpetaVacia? (cdr ListAC) NameCarpeta Acc)
      (if (null? (caddr(car ListAC)))
          (CarpetaVacia? (cdr ListAC) NameCarpeta Acc)         
          (CarpetaVacia? (cdr ListAC) NameCarpeta (+1 Acc)))  
       )))

(define ((rd system) folder)
  (if (>(CarpetaVacia? (get-Archivos/carpetas system) folder  0 )0)
      system                                                                 ;; Falta borrar la carpeta
      system
  ))

        


;;R14: Copiar archivos y carpetas
(define (DatosFile Namefile ListAC)                 ;; entrega la lista con los datos del archivo a copiar                                          
    (if (null? ListAC) null
            (if (list?(member Namefile (string-split (car(car ListAC)) "/")))
            (car ListAC)
            (DatosFile Namefile (cdr ListAC)))))
                

(define (DatosFolder Namefolder ListAC)               ;; entrega lists con los datos de la carpeta a copiar                                  
    (if (null? ListAC) null
            (if (list?(member Namefolder(string-split (car(car ListAC)) "/")))
            (car ListAC)
            (DatosFile Namefolder (cdr ListAC)))))
        

(define (FileOrFolder StringName)                     ;; entrega si un string es archivo o carpeta dependiendo si tiene extensión
   (if (list? (member #\. (string->list StringName)))
    "A" "C"))


(define ((copy system)NameFileOrFolder PathDestino)
  (if (string=?(FileOrFolder NameFileOrFolder) "A")
    (make-system                    
       (get-Name-system system)
       (get-Users-system system)
       (get-Drives-system system)
       (get-CurrentUser-system system)
       (get-CurrentDrive-system system)
       (get-currentPath-system system)
       (cons (list (string-append PathDestino NameFileOrFolder) (cadr(DatosFile NameFileOrFolder (get-Archivos/carpetas system))) (caddr(DatosFile NameFileOrFolder (get-Archivos/carpetas system))) (cadddr(DatosFile NameFileOrFolder (get-Archivos/carpetas system))) (car (cdr (cdr (cdr (cdr (DatosFile NameFileOrFolder (get-Archivos/carpetas system)))))))) (get-Archivos/carpetas system))
       )
    (make-system                    
       (get-Name-system system)
       (get-Users-system system)
       (get-Drives-system system)
       (get-CurrentUser-system system)
       (get-CurrentDrive-system system)
       (get-currentPath-system system)
       (cons (list (string-append PathDestino NameFileOrFolder)(cadr(DatosFolder NameFileOrFolder (get-Archivos/carpetas system))) (caddr(DatosFolder NameFileOrFolder (get-Archivos/carpetas system))) (cadddr(DatosFolder NameFileOrFolder (get-Archivos/carpetas system))) (car (cdr (cdr (cdr (cdr (DatosFolder NameFileOrFolder (get-Archivos/carpetas system)))))))) (get-Archivos/carpetas system))
             
       )))





  
;;R15




  
;;R16
(define (replace lst old new)                            ;; permite reemplazar un elemento (old) por otro (new)
  (map (lambda (x) (if (eq? x old) new x)) lst))

;;(define (sublist? lst sublst)                            ;; permite determinar si un elemento es sublista de otro
  ;;(cond
    ;;((null? sublst) #t) 
    ;;((null? lst) #f) 
    ;;((equal? lst sublst) #t) 
    ;;((not (pair? lst)) #f) 
    ;;((sublist? (cdr lst) sublst)) 
    ;;((sublist? (cdr lst) (cdr sublst))))) 

;;(define (ActalizarAC listRegistro  OldName NewName)
 ;; (if (list? (member NewName (string-split (car listRegistro)"/")))
   ;;   (replace (string-split (car listRegistro)"/") OldName NewName) 
;;        (if (string=?(FileOrFolder OldName) "A")
  ;;         (replace (cadr listRegistro) OldName NewName)
           
    ;;  )))

;;(define ((ren system) NewName)
;;(map (lambda (x  OldName NewName) (ActalizarAC x OldName NewName)) (get-Archivos/carpetas system)))



;;R18: Formatear un disco y renombrarlo
(define (FiltrarPorDrive NameDrive listAC)      ;;Permite filtrar los registros que tienen de raiz el drive buscado
  (if (null? listAC) null
      (if (list? (member NameDrive (string-split (car(car listAC)) "/")))
          (cons (car listAC) (FiltrarPorDrive NameDrive (cdr listAC)))
          (FiltrarPorDrive NameDrive (cdr listAC)))))


(define (Move sublist list listRef)             ;; Permite mover una sublista de una lista
  (if (null? sublist) listRef
      (if (list? (member (car sublist) list))
          (Move (cdr sublist) list (remove (car sublist) listRef))
          
          (Move (cdr sublist) list listRef) )))

(define (MapRenombrar NameDrive NewName listDrives)   ;; Es un map que renombra el disco formateado
  (if (null? listDrives) null
      (if (char=? (car (string->list NameDrive)) (car (car listDrives)))
                     (cons(cons(car(car listDrives)) (cons NewName (cdr(cdr(car listDrives))))) (MapRenombrar NameDrive NewName (cdr listDrives)))
                     (cons (car listDrives)(MapRenombrar NameDrive NewName (cdr listDrives))))))
   

(define ((Format System) NameDrive NewName)
  (Move (FiltrarPorDrive NameDrive (get-Archivos/carpetas system)) (get-Archivos/carpetas system) (get-Archivos/carpetas system)) ;;Incluir la construcción del sistema
  (MapRenombrar NameDrive NewName (get-Drives-system system) ))


;;R19:
(define (ArchivoOrCarpeta Name)
  (if (list? (member #\. (string->list Name)))
      "File" "Folder"))

;;Función Encryptar:
(define (Encryptar listElement)
  (cons (car listElement) (cons (list (car (cadr listElement)) (cadr (cadr listElement)) (plus-one(caddr (cadr listElement)))) (cdr (cdr listElement)))   ))      



(define (MapEncryp Name listAC)
  (if (null? listAC) null
      (if  (or (and (string=?(ArchivoOrCarpeta Name) "Folder")  (string=?(ArchivoOrCarpeta (car (reverse (string-split (car (car listAC)) "/")))) "File"))      (and (string=?(ArchivoOrCarpeta Name) "File")  (string=?(ArchivoOrCarpeta (car (reverse (string-split (car (car listAC)) "/")))) "File")))
           (cons (Encryptar (car listAC))(MapEncryp Name (cdr listAC)))
           (cons (car listAC)(MapEncryp Name (cdr listAC))))))






;;R20:
;;Crear la funcion encriptar FNDesEncryp


(define (MapDesEncryp Name listAC)
  (if (null? listAC) null
      (if  (or (and (string=?(ArchivoOrCarpeta Name) "Folder")  (string=?(ArchivoOrCarpeta (car (reverse (string-split (car (car listAC)) "/")))) "File"))      (and (string=?(ArchivoOrCarpeta Name) "File")  (string=?(ArchivoOrCarpeta (car (reverse (string-split (car (car listAC)) "/")))) "File")))
           (cons (Encryptar (car listAC))(MapEncryp Name (cdr listAC)))
           (cons (car listAC)(MapEncryp Name (cdr listAC))))))






;;R21 Función plus-one
(define (plus char)
  (integer->char (+ 1 (char->integer char))))

(define (plus-one texto)
  (list->string (map (lambda (char) (plus char)) (string->list texto))))


;;R22 Función plus-one
(define (minus char)
  (integer->char (+ (- 1) (char->integer char))))

(define (minus-one texto)
  (list->string (map (lambda (char) (minus char)) (string->list texto))))



  
;;SCRIPTS:
(define S0 (system "newSystem"))
(define S1 ((run S0 add-drive) #\C "SO" 1000))
(define S2 ((run S1 add-drive) #\C "SO1" 3000))
(define S3 ((run S2 add-drive) #\D "Util" 2000))
(define S4 ((run S3 add-user) "user1"))
(define S5 ((run S4 add-user) "user1"))
(define S6 ((run S5 add-user) "user2"))
(define S7 ((run S6 login) "user1"))
(define S8 ((run S7 login) "user6"))
(define S9 (run S8 logout))
(define S10 ((run S9 login) "user2"))
(define S11 ((run S10 switch-drive) #\K))
(define S12 ((run S11 switch-drive) #\C))
(define S13 ((run S12 cd) "folder1"))
(define S14 ((run S13 cd) "folder2"))
(define S15 ((run S14 cd) "folder2"))
(define S16 ((run S15 cd) "folder3"))
(define S17 ((run S16 cd2) "/"))

