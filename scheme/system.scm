(import-lambda-definition-2 %gethostname () "scm_gethostname")

(define (system-name)
  (byte-vector->string (%gethostname)))

(define-record-type :uname
  (make-uname os-name node-name release version machine)
  uname?
  (os-name    uname:os-name)
  (node-name  uname:node-name)
  (release    uname:release)
  (version    uname:version)
  (machine    uname:machine))

(define (uname)
  (make-uname (os-name)
              (os-node-name)
              (os-release-name)
              (os-version-name)
              (machine-name)))

(define uname-os      os-name)
(define uname-node    os-node-name)
(define uname-release os-release-name)
(define uname-version os-version-name)
(define uname-machine machine-name)

(define-record-discloser :uname
  (lambda (u) 
    (list 'uname 
      (cons 'os-name   (uname:os-name u)) 
      (cons 'node-name (uname:node-name u)) 
      (cons 'release   (uname:release u)) 
      (cons 'version   (uname:version u)) 
      (cons 'machine   (uname:machine u)))))
