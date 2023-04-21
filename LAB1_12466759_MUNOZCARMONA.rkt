
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

(define ((add-drive system) drive namedrive capacidad )  ;; es el drive (que es una lista con letra x nombre x capacidad) que se quiere agregar
 (if ( = (RepiteDrv? drive (get-Drives-system system) 0) 1 )
   system
  (make-system                    
  (get-Name-system system)
  (get-Users-system system)
  (cons (list drive namedrive capacidad)(get-Drives-system system))
  (get-CurrentUser-system system)
  (get-CurrentDrive-system system)
  (get-currentPath-system system)
  (cons (list (string drive #\: #\/) '() "" "" "")(get-Archivos/carpetas system))                                                ;; Para facilitar operatividad de R11 se propone crear un regiStro vacío sólo con la raiz en Archivos/Carpetas                                                            
  ))) 


;;RF5 - add-users:Permite agregar un nuevo usuario
(define (existe?  listName  Name Acc)        ;; Se crea función para evitar agregar UN uSER con el mismo nombre                    
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


;;RF7 - Función logout: permite cerrar la sesión de un usuario en el sistema.
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

(define (PathRight list )                                         ;; Funcion que a la lista anterior le incluye los "/" 
    (if (null? list) null
     (cons (car list) (cons "/" (PathRight (cdr list) )))))

(define (BackRoot string)                                           
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
      (string-join (PathRight (cdr(string-split (get-currentPath-system system) "/")) '() ) ""))]
    [(string=? Comando "/")  (BackRoot(get-currentPath-system system)) ]
    [(string? Comando)  (string-append (get-currentPath-system system) Comando "/")])                ;; falta asegurarse que la carpeta no se repite verificando con con current path
  
  (get-Archivos/carpetas system)))


;;R11: add-file función que permite añadir un archivo en la ruta actual.

;;Se define el constructor "file"
(define (file filename extension contenido . atributos)
  (list filename extension contenido atributos))

;;Función para verificar que el archivo no se repite en la ruta
 (define (filterAC listAC FilePath Contador)      ;;listAC tiene por objetivo guardar los elementos de Archivos/carpetas / el FilePath es current path + "7" + nombre archivo 
  (if (null? listAC) Contador
      (if (string=? (car (car listAC)) FilePath)
           (filterAC (cdr listAC) FilePath (+1 Contador))
           (filterAC (cdr listAC) FilePath Contador ))))

(define (findReg listAC CurrentPath listAcc)  ;; selecciona el registro que coincide con el CurrentPath 
  (if (null? listAC) listAcc
      (if (string=? (car (car listAC)) CurrentPath)
          (findReg (cdr listAC) CurrentPath (cons (car listAC)  listAcc))
          (findReg (cdr listAC) CurrentPath listAcc))))
          
(define ((add-file system) listFile)
 (if (= (filterAC (get-Archivos/carpetas system) (string-append (get-currentPath-system system) (car listFile)) 0) 0)
 (make-system                    
  (get-Name-system system)
  (get-Users-system system)
  (get-Drives-system system)
  (get-CurrentUser-system system)
  (get-CurrentDrive-system system)
  (get-currentPath-system system)
     (cons   (cons (string-append (get-currentPath-system system) (car listFile))       (cons listFile      (cdr  (cdr   (car (findReg (get-Archivos/carpetas system) (get-currentPath-system system) '( )))   )  )  )  )       (get-Archivos/carpetas system)   ))
  system))
  


;;R12 Funciones para eliminar un archivo o varios archivos en base a un patrón determinado.
(define (FolderInPath Path)
      (car (reverse (string-split Path "/"))))
  
(define (getExtension Name)
      (cadr(string-split Name ".")))

(define (getLetter Command)
      (car (string-split (car(string-split Command ".")) "*")))

(define (FirstLetter str)
   (string(car(string->list str))))



(define (Delfile CurrentPath Name listAC list)                            ;; Crea nueva lista con los elemnetos seleccionados eliminados los que se deben incluir en la actualizacion del sistema   
  (if (null? listAC) list
      (if (string=?(string-append CurrentPath Name) (car (car listAC)) )
        (Delfile CurrentPath Name (cdr listAC) (remove (car listAC) list))
        (Delfile CurrentPath Name (cdr listAC) list)) ))


(define (DelFileExt CurrentPath ComdExten listAC list)                    ;;Crea nueva lista con los elemnetos seleccionados eliminados los que se deben incluir en la actualizacion del sistema 
  (if (null? listAC) list
      (if  (list? (member (FolderInPath CurrentPath) (string-split (car (car listAC)) "/")))
           (if  (string=? (getExtension(cadr(member (FolderInPath CurrentPath) (string-split (car (car listAC)) "/"))))    (getExtension ComdExten))
           (DelFileExt CurrentPath ComdExten (cdr listAC)  (remove (car listAC) list)) 
           (DelFileExt CurrentPath ComdExten (cdr listAC)  list))
      (DelFileExt CurrentPath ComdExten (cdr listAC)  list))))


(define (DelFileLetterExt CurrentPath Comand listAC list)                    ;;Crea nueva lista con los elemnetos seleccionados eliminados los que se deben incluir en la actualizacion del sistema 
  (if (null? listAC) list
      (if  (list? (member (FolderInPath CurrentPath) (string-split (car (car listAC)) "/")))
           (if  (and     (string=? (getExtension(cadr(member (FolderInPath CurrentPath) (string-split (car (car listAC)) "/")))) (getExtension Comand))  (string=? (getLetter Comand) (FirstLetter (car (car listAC))) ))
           (DelFileLetterExt CurrentPath Comand (cdr listAC)  (remove (car listAC) list)) 
           (DelFileLetterExt CurrentPath Comand (cdr listAC)  list))
      (DelFileLetterExt CurrentPath Comand (cdr listAC)  list))))


(define (Delall CurrentPath Comand listAC list)                    ;;Crea nueva lista con los elemnetos seleccionados eliminados los que se deben incluir en la actualizacion del sistema  
  (if (null? listAC) list
      (if  (list? (member (FolderInPath CurrentPath) (string-split (car (car listAC)) "/")))
           (if   (list? (member #\. (string->list (cadr(member (FolderInPath CurrentPath) (string-split (car (car listAC)) "/"))))))        
           (Delall CurrentPath Comand (cdr listAC)  (remove (car listAC) list)) 
           (Delall CurrentPath Comand (cdr listAC)  list))
      (Delall CurrentPath Comand (cdr listAC)  list))))



       
;;R13: Elimina Carpeta
;;Filtra los registros en "Archivos/carpetas" cuyo path contenga el nombre de la carpeta y que tengan archivos
(define (CarpetaVacia? listAC NameCarpeta listAcc)        ;; De manera que si Acc > 1 entonces la carpeta no esta vacía 
(if (null? listAC) listAcc
  (if (boolean=? (memq NameCarpeta (string-split (car(car listAC)) "/" )) false )      ;; se el nombre de la carpeta no está en el ptah arroja "false"
      (CarpetaVacia? (cdr listAC) NameCarpeta listAcc)    
      (if (null? (cadr(car listAC)))
          (CarpetaVacia? (cdr listAC) NameCarpeta listAcc)         
          (CarpetaVacia? (cdr listAC) NameCarpeta (cons (car listAC) listAcc)))        )))

(define ((rd system) folder)
  (if (null? (CarpetaVacia? (get-Archivos/carpetas system) folder  '()) )
      system                                                                 ;; Falta borrar la carpeta
      (make-system                    
       (get-Name-system system)
       (get-Users-system system)
       (get-Drives-system system)
       (get-CurrentUser-system system)
       (get-CurrentDrive-system system)
       (get-currentPath-system system)
       (remove (CarpetaVacia? (get-Archivos/carpetas system) folder  '()) (get-Archivos/carpetas system))        )))

        
;;R14: Copiar archivos y carpetas
(define (PathNewReg RegistroAC NameElement RootPath)    ;;Entrega la ruta de cada resgistro, cuando requiere copiarse
   (cond (string-append RootPath (PathRight (member NameElement (string-split (car RegistroAC) "/")))) (cdr RegistroAC)))
   
(define (Copiar listAC NameElement RootPath)             ;;Genera una nueva lista que reemplaza a listAC con todos los registros copiados
    (if (null? listAC) null
        (if (null? (member NameElement (string-split (car( car listAC)))))
            (cons (PathNewReg (car listAC) NameElement RootPath)   (cons (car listAC)  (Copiar cdr listAC))) 
             (cons   (car listAC)  (Copiar cdr listAC)))))



;;R15: Mueve archivos
(define (Mover listAC NameElement RootPath)             ;;Genera una nueva lista que reemplaza a listAC con todos los registros copiados
    (if (null? listAC) null
        (if (null? (member NameElement (string-split (car( car listAC)))))
            (cons (PathNewReg (car listAC) NameElement RootPath)   (cons (remove (car listAC)  (Copiar cdr listAC)) ))  ;;remueve el archivo cambiado de ruta
             (cons   (car listAC)  (Copiar cdr listAC)))))



;; Falta crear el nuevo systema
 
;;R16
(define (replace lst old new)                               ;; permite reemplazar un elemento (old) por otro (new)
  (map (lambda (x) (if (eq? x old) new x)) lst))


(define (Subruta CurrentPath NewName Ruta)                  ;; Entrega un boolean asociado a si es ruta o no
   (if (equal? (string-split (string-append CurrentPath NewName) "/") (reverse(member NewName (reverse (string-split Ruta "/")))))  true false))                                     ;; Define su una ruta es subruta 


(define (CarpetaOrArchivo Name)
  (if (list? (member #\. (string->list Name))) "Archivo" "Carpeta"))


(define (RemCarpeta OldName NewName CurrentPath listAC)
  (if (null? listAC) null
      (if (Subruta CurrentPath NewName (car (car listAC)))
           (cons (cons (string-join (PathRight (replace (string-split (car (car listAC))) OldName NewName)))  (cdr (car listAC)))    (RemCarpeta OldName NewName CurrentPath (cdr listAC)))               ;;(define (PathRight list )           ;; validez del nombre (substring? str sub)  
           (cons (car listAC)    (RemCarpeta OldName NewName CurrentPath (cdr listAC)))   )))                                                                                                                                                


(define (RemArchivo OldName NewName CurrentPath listAC)
  (if (null? listAC) null
      (if (Subruta CurrentPath NewName (car (car listAC)))
           (cons      (cons    (string-join (PathRight (replace (string-split (car (car listAC))) OldName  NewName )))   (cons (cons (string-join (PathRight (replace (string-split (car (car listAC))) OldName  NewName ))) (cdr (cadr (car listAC))))      (cdr (cdr (car listAC)))))         (RemArchivo OldName NewName CurrentPath (cdr listAC)))               ;;(define (PathRight list )           ;; validez del nombre (substring? str sub)  
           (cons (car listAC)    (RemArchivo OldName NewName CurrentPath (cdr listAC)))   )))                                                                                                                                                


(define ((ren system) OldName NewName )
  (if   (string=? (CarpetaOrArchivo NewName) "Carpeta")
        (RemCarpeta OldName NewName  (get-currentPath-system system) (get-Archivos/carpetas system))             ;;insertar constructor de sitema
        (RemArchivo OldName NewName  (get-currentPath-system system) (get-Archivos/carpetas system))))
  
  
  
;;R17:
(define (DepliegueList lista)                                            ;;función que despliega listas
    (define my-list lista)
    (for-each (lambda (x) (display x) (newline)) my-list))

(define (OrdenaList lista)
    (sort lista string<?))


  

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
(define S18 ((run S17 add-file) (file "foo1.txt" "txt" "hello world 1")))
