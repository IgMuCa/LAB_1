#lang racket
;;TDA
;;Type System = name (String) x users (String list) x current-user (String) x drives(drive list) x current-drive (char) x folders(list folder) x archivos x current-path(list)
;;Type drive = sigla-drive (string) X name (string) X capacidad (int)
;;Type folder (md) = creador x date-creacion x date-modificacion x name x ubicacion (ruta) 
;;Type archivo = tipo x  name X extención X contenido X atributos X ubicacion


;; RF2 - Constructor inicial: System sera lista de: Nombre x drives x Usuasios x Folders
 (define (system name)           
   (list name '() '() '() '() '() '() '())
  )

 (define (make-system name users current-user drives current-drive folders archivos current-path)
   (list name users current-user drives current-drive folders archivos current-path))

   
;; RF3 -  RUN:Permite ejecutar un comando sobre el sistema
 (define (run system cmd)
   (cmd system))
   


;;RF4 - add-drive:Permite agregar un drive
(define get-name-system car) ;; permite acceder al nombre
(define get-users-system cadr) ;; permite acceder al drive
(define get-current-user-system caddr) ;; permite acceder a la lista de usuarios
(define get-drives-system cadddr) ;; permite acceder a la lista de folders
(define (get-current-drive-system system) (car (cdr (cdr (cdr (cdr system   ))))))
(define (get-folders-system system) (car (cdr (cdr (cdr (cdr (cdr system   )))))))
(define (get-archivos-system system) (car (cdr (cdr (cdr (cdr (cdr (cdr system   ))))))))
(define (get-current-path-system system) (car (cdr (cdr (cdr (cdr (cdr (cdr (cdr system   )))))))))


(define ((add-drive system) sigla-drive newdrive capacidad )  ;; drive es el drive (que es una lista con letra xnombre x capacidad) que se quiere agregar
 (make-system                    
  (get-name-system system)
  (get-users-system system)
  (get-current-user-system system)
  (cons (list sigla-drive newdrive capacidad) (get-drives-system system))
  (get-current-drive-system system)
  (get-folders-system system)
  (get-archivos-system system)
  (get-current-path-system system)
  )
 )


;;RF5 - add-users:Permite agregar un nuevo user
(define ((add-user system) NewUser )  
 (make-system                    
  (get-name-system system)
  (cons NewUser (get-users-system system))
  (get-current-user-system system)
  (get-drives-system system)
  (get-current-drive-system system)
  (get-folders-system system)
  (get-archivos-system system)
  (get-current-path-system system)
  )
 )
 

;;RF6 - Función login: que permite iniciar sesión con un usuario del sistema, solo si éste existe.
(define (existe-user?  system user)
  (member user  (get-users-system system))
  )

(define ((login system) user )
 (cond
  [(existe-user? system user)
 (make-system                    
  (get-name-system system)
  (get-users-system system)
  (cons user (get-current-user-system system))
  (get-drives-system system)
  (get-current-drive-system system)
  (get-folders-system system)
  (get-archivos-system system)
  (get-current-path-system system)
  )
  ]
 ))


;;RF7 - Función logout: permite cerrar la sesión de un usuario en el sistema.
(define (logout system)
 (make-system                    
  (get-name-system system)
  (get-users-system system)
  (cdr (get-current-user-system system))
  (get-drives-system system)
  (get-current-drive-system system)
  (get-folders-system system)
  (get-archivos-system system)
  (get-current-path-system system)
  )
 )


;;RF8.-Permite fijar la unidad en la que el usuario realizará acciones. 
;;La función solo debe funcionar cuando hay un usuario con sesión iniciada en el sistema a partir de la función descrita en 6.
;;Por mejorar: asegurar que el drive que se active esté en la lista de drives
(define (sin-user? system)
  (empty?(get-current-user-system system)))

(define ((switch-drive system) drv)
  (if (sin-user? system)
      (make-system                    
  (get-name-system system)
  (get-users-system system)
  (get-current-user-system system)
  (get-drives-system system)
  (get-current-drive-system system)
  (get-folders-system system)
  (get-archivos-system system)
  (get-current-path-system system)
  )
  (make-system                    
  (get-name-system system)
  (get-users-system system)
  (get-current-user-system system)
  (get-drives-system system)
  (cons drv (get-current-drive-system system))
  (get-folders-system system)
  (get-archivos-system system)
  (get-current-path-system system)
  ))
  )

   
;;RF9.- md: Permite crear un directorio dentro de una unidad a partir del nombre especificado.
(define (make-md name ubicacion creador date-creacion date-modificacion)   ;; Se construye la carpeta
  (list name ubicacion creador date-creacion date-modificacion)
  )

(define ((md system) namecd)
 (make-system                    
  (get-name-system system)
  (get-users-system system)
  (get-current-user-system system)
  (get-drives-system system)
  (get-current-drive-system system)
  (make-md namecd (string (car(get-current-drive-system system))) '() '() '())
  (get-archivos-system system)
  (string-append (string (car(get-current-drive-system system))) "/" namecd)
  )
 )

;;R10: cd función que permite cambiar la ruta (path) donde se realizarán operaciones



;;R11: función que permite añadir un archivo en la ruta actual.
;;Type archivo =  tipo x name X extención X contenido X atributos X ubicacion
(define (make-archivo name extención contenido atributos ubicacion)   
  (list name extención contenido atributos ubicacion))

;;(define S32 ((run S31 add-file) (file "foo1.txt" "txt" "hello world 1")))







;;SCRIPTS:
(define S0 (system "newSystem"))
(define S1 ((run S0 add-drive) #\C "SO" 1000))
(define S2 ((run S1 add-user) "user1"))
(define S3 ((run S2 login) "user1"))
(define S4 (run S3 logout))
(define S5 ((run S4 login) "user1"))
(define S6 ((run S5 switch-drive) #\C))
(define S7 ((run S6 md) "folder2"))
