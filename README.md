# cl-unicorn

## [UNDER CONSTRUCTION]

Common Lisp bindings for the Unicorn emulation engine (www.unicorn-engine.org)

So far, the ARM and x86 architectures are fairly well-supported. Not quite a finished product, though. Feel free to take it for a spin, but I wouldn't advise using it, as-is, in production, just yet.


## Usage example (take with a grain of salt while under construction):
```
CL-USER> (ql:quickload :cl-unicorn)
To load "cl-unicorn":
  Load 1 ASDF system:
    cl-unicorn
; Loading "cl-unicorn"

(:CL-UNICORN)
CL-USER> (in-package :unicorn)
#<PACKAGE "UNICORN">
UNICORN> (uc-version)
1
0
UNICORN> (defparameter *uc* (uc-open :arm :arm)) ;; ARM arch, ARM mode 
*UC*
UNICORN> (uc-reg-read *uc* :sp :arch :arm)
0
:OK
UNICORN> (uc-reg-read-batch *uc* (range 0 16) :arch :arm) ;; we can also refer to the ARM registers by integer, for convenience
(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
UNICORN> (uc-reg-write-batch *uc* (range 0 16) (loop repeat 16 collect 2))
(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)
UNICORN> (uc-reg-read-batch *uc* (range 0 16) :arch :arm) ;; we can also refer to the ARM registers by integer, for convenience
(2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2)
UNICORN> ;; now let's define a callback function 
; No value
UNICORN> (defun code-hook-show-inst (uc address size user-data)
           (let ((inst (uc-mem-read uc address size))
                 (regs (uc-reg-read-batch uc (range 0 16))))
             ;; we can use the user-data pointer to send data back 
             ;; to the context from which the hooks are placed
             (incf (mem-aref user-data :uint64 0))
             (format t "[~4X] ~S => ~S~%"
                     address inst regs)))
WARNING: redefining UNICORN::CODE-HOOK-SHOW-INST in DEFUN
CODE-HOOK-SHOW-INST
UNICORN> (uc-mem-map *uc* 0 4096 '(:read :exec :write))
:OK
UNICORN> (defparameter *test-code*
           #(#x37 #x00 #xa0 #xe3    ;; mov r0, #x037
             #x03 #x10 #x42 #xe0))  ;; sub r1, r2, r3
*TEST-CODE*
UNICORN> (uc-mem-write *uc* 0 *test-code*)
:OK
UNICORN> (uc-mem-read *uc* 0 (length *test-code*))
#(55 0 160 227 3 16 66 224)
UNICORN> (setf *print-base* 16)
10
UNICORN> (uc-mem-read *uc* 0 (length *test-code*))
#(37 0 A0 E3 3 10 42 E0)
UNICORN> (uc-emu-start *uc* 0 :until 64 :count 16)
:OK
UNICORN> (uc-reg-read-batch *uc* (range 0 16))
(37 0 2 2 2 2 2 2 2 2 2 2 2 2 2 3C)
UNICORN> (defun code-hook-show-inst (uc address size user-data)
           (let ((inst (uc-mem-read uc address size))
                 (regs (uc-reg-read-batch uc (range 0 16))))
             ;; we can use the user-data pointer to send data back 
             ;; to the context from which the hooks are placed
             (incf (mem-aref user-data :uint64 0))
             (format t "[~4X] ~S => ~S~%"
                     address inst regs)))
CODE-HOOK-SHOW-INST
UNICORN> (defparameter user-data (foreign-alloc :pointer))
USER-DATA
UNICORN> (mem-ref user-data :uint64)
0
UNICORN> (uc-hook-add *uc* 0 4096 :fn 'code-hook-show-inst :hook-type :code :user-data user-data)
#.(SB-SYS:INT-SAP #X7FFFE1155FC0)
:OK
UNICORN> (uc-emu-start *uc* 0 :until 16 :count 4)
[   0] #(37 0 A0 E3) => (37 0 2 2 2 2 2 2 2 2 2 2 2 2 2 0)
[   4] #(3 10 42 E0) => (37 0 2 2 2 2 2 2 2 2 2 2 2 2 2 4)
[   8] #(0 0 0 0) => (37 0 2 2 2 2 2 2 2 2 2 2 2 2 2 8)
[   C] #(0 0 0 0) => (37 0 2 2 2 2 2 2 2 2 2 2 2 2 2 C)
:OK
UNICORN> (mem-ref user-data :uint64)
4
UNICORN> (uc-close *uc*)
:OK
UNICORN> 
```
