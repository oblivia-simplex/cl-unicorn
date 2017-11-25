;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interface to the Unicorn Emulation Libary (written in C)         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :unicorn)

(define-foreign-library libunicorn
  (:unix (:or "libunicorn.so.1" "libunicorn.so"))
  (t (:default "libunicorn")))

(use-foreign-library libunicorn)


(defun %reg-keyword (n)
  (intern (format nil "R~D" n) :keyword))

(defun %reg->regid (k &key (arch :arm))
  (cdr (assoc k (cadr (assoc arch +reg-ids+)))))

(defun range (lo hi)
  (loop for i from lo to (1- hi) collect i))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions
;;;;;;;;;;;;;;;;;;;;;;;
(defcfun "uc_arch_supported" :bool (arch uc-arch))

;; here's an important one. will need a wrapper.
(defcfun ("uc_open" %uc-open) uc-err
  (unicorn-arch uc-arch)
  (unicorn-mode uc-mode)
  (uc (:pointer unicorn-engine)))
;; how big should the pointer be? it's a pointer to a pointer to a
;; uc_struct...
(defparameter *pointer-size* 8)
(defun uc-open (unicorn-arch unicorn-mode)
  "Takes unicorn arch and mode parameters, and returns an engine"
  (handler-case
      (with-foreign-pointer (uc *pointer-size*)
        (%uc-open unicorn-arch unicorn-mode uc)
        (mem-ref uc :pointer))
    (error (ex)
      (format t "Error initializing unicorn engine:~%~T-> ~A~%" ex))))


(defcfun ("uc_close" %uc-close) uc-err
  (uc :pointer))

(defun uc-close (uc)
  (handler-case (%uc-close uc)
    (error (ex)
      (format t "Warning: unable to close uc:~%~T-> ~A~%" ex))))

(defcfun ("uc_version" %uc-version) :uint
  (major (:pointer :uint))
  (minor (:pointer :uint)))

(defun uc-version ()
  (with-foreign-pointer (major 8)
    (with-foreign-pointer (minor 8)
      (%uc-version major minor)
      (values (mem-ref major :uint)
              (mem-ref minor :uint)))))

(defcfun ("uc_query" %uc-query) uc-err
  (engine unicorn-engine)
  (query-type :uint)
  (result :pointer))

(defun uc-query (engine query-type)
  (with-foreign-pointer (result 8)
    (%uc-query engine
               (foreign-enum-value 'uc-query-type query-type)
               result)
    (case query-type
      ((:mode) (foreign-enum-keyword 'uc-mode
                                     (mem-ref result :uint)))
      ((:page-size) (mem-ref result :uint))
      (:otherwise result))))

(defun mode-eq (mode1 mode2)
  (= (foreign-enum-value 'uc-mode mode1)
     (foreign-enum-value 'uc-mode mode2)))

(defun in-mode-p (engine uc-mode)
  (mode-eq (uc-query engine :mode) uc-mode))

(defcfun ("uc_reg_write" %uc-reg-write) uc-err
  (uc-engine unicorn-engine)
  (regid :uint)
  (value :pointer))


(export 'uc-reg-write)
(defun uc-reg-write (engine register value &key
                                             (type :uint64)
                                             (arch :arm))
  (with-foreign-pointer (valptr 8)
    (setf (mem-ref valptr type) (convert-to-foreign value type))
    (%uc-reg-write engine (%reg->regid register :arch arch) valptr)))

(defcfun ("uc_reg_read" %uc-reg-read) uc-err
  (uc-engine unicorn-engine)
  (regid :uint)
  (value :pointer))

(export 'uc-reg-read)
(defun uc-reg-read (engine register &key (type :uint64) (arch :arm))
  "Returns the register contents as the first value, and the
error code as the second."
  (with-foreign-pointer (valptr 8)
    (let ((regid (%reg->regid register :arch arch)))
      (assert regid () 
      (let ((err (%uc-reg-read engine
			      regid ;;(%reg->regid register :arch arch)
			      valptr)))
      (values (mem-ref valptr type) err)))))

(defun uc-reg-write-batch (engine registers values
                           &key (type :uint64) (arch :arm))
  (mapc (lambda (r v) (uc-reg-write engine r v :type type :arch arch))
        registers values))

(defun uc-reg-read-batch (engine registers
                          &key (type :uint64) (arch :arm))
  (mapcar (lambda (r) (uc-reg-read engine r :type type :arch arch))
          registers))

(defcfun ("uc_mem_write" %uc-mem-write) uc-err
  (uc-engine unicorn-engine)
  (address :uint64)
  (bytes (:pointer :uint8))
  (size size))


(export 'bytes->pointer)
(defun bytes->pointer (bytes)
  (foreign-alloc :uint8
                 :initial-contents bytes))

(export 'pointer->bytes)
(defun pointer->bytes (pointer size)
  (loop for i below size collect (mem-aref pointer :uint8 i)))

(export 'uc-mem-write)
(defun uc-mem-write (engine address bytes)
  (unwind-protect
       (let* ((length (length bytes))
              (ptr (bytes->pointer bytes))
              (ret (%uc-mem-write engine
                                  (convert-to-foreign address :uint64)
                                  ptr
                                  length)))
         (foreign-free ptr)
         ret)))

(defcfun ("uc_mem_read" %uc-mem-read) uc-err
  (uc-engine unicorn-engine)
  (address :uint64)
  (bytes :pointer)
  (size size))

(export 'uc-mem-read)
(defun uc-mem-read (engine address size)
  (with-foreign-pointer (buffer size)
    (let ((errcode (%uc-mem-read engine address buffer size)))
      (values (if (eq errcode :ok)
		  (pointer->bytes buffer size)
		  ())
	      errcode))))

     

(defcfun ("uc_mem_map" %uc-mem-map) uc-err
  (uc-engine unicorn-engine)
  (address :uint64)
  (size size)
  (perms :uint32))

(defun uc-mem-map (engine address size perms)
  (%uc-mem-map engine address size
               (merge-flags perms :enum-type 'uc-prot)))

(defcfun ("uc_emu_start" %uc-emu-start) uc-err
  (uc-engine unicorn-engine)
  (begin :uint64)
  (until :uint64)
  (timeout :uint64)
  (count :uint64))

(defun uc-emu-start (engine begin &key
                                    (until)
                                    (timeout)
                                    (count))
  (handler-case 
      (%uc-emu-start engine
		     begin
		     (if until until #xFFFFFFFF)
		     (if timeout timeout 0)
		     (if count count -1))
    (error (e)
      (format t "Exception unhandled by unicorn: ~A~%" e)
      :UNHANDLED)))

(defcfun ("uc_hook_del" %uc-hook-del) uc-err
  (uc-engine unicorn-engine)
  (hook-handle :pointer))

(defun uc-hook-del (uc-engine hook-handle)
  (let ((res (%uc-hook-del uc-engine hook-handle)))
    ;(foreign-free hook-handle)
    res))

(defcfun ("uc_hook_add" %uc-hook-add) uc-err
  (uc-engine unicorn-engine)
  (hook-handle :pointer) ;; hook handle used by uc_hook_del()
  (type :uint) ;; hook type enum
  (callback :pointer) ;; pointer to callback function
  (user-data :pointer) ;; passed to callback function in its last arg
  (begin :uint64) ;; start address of region hooked (incl.)
  (end :uint64) ;; end address of region hooked (incl.)
  ;; variable arguments, too... hm...
  ;; see the cffi tutorial for this. we might use foreign-funcall
  ;; instead of defcfun here.
  )

(export 'fn->callback)
(defmacro fn->callback (fn)
  (let ((cb-sym (gensym "cb")))
    `(defcallback ,cb-sym :void
	 ((uc unicorn-engine)
	  (address :uint64)
	  (size :uint32)
	  (user-data :pointer))
       (funcall ,fn uc address size user-data))))

(defun uc-hook-add (engine begin end
                    &key
                      (fn)
                      (callback-ptr)
                      (hook-type :code)
                      (user-data (foreign-alloc :pointer)))
  "You can supply *either* a function (to be converted to a callback) or
a precompiled callback pointer (to save time and space)."
  (let* ((handle (foreign-alloc :pointer))
         (cb-ptr (if (null callback-ptr)
                     (fn->callback fn)
                     callback-ptr))
         (errcode (%uc-hook-add engine handle
                                (foreign-enum-value 'uc-hook-type
                                                    hook-type)
                                cb-ptr
                                user-data
                                begin
                                end)))
    (values handle errcode)))

;;; TODO: uc-hook-remove! 


;;;;;;;;;;;;;;;;;
;; For testing ;;
;;;;;;;;;;;;;;;;;
(defparameter ~test-code~
  #(#x37 #x00 #xa0 #xe3   ;; mov r0, #x037
    #x03 #x10 #x42 #xe0)) ;; sub r1, r2, r3

(defun code-hook-show-inst (uc address size user-data)
  (let ((inst (uc-mem-read uc address size))
        (regs (uc-reg-read-batch uc (range 0 16))))
    ;; a much better way to send data back!
    (incf (mem-aref user-data :uint64 0))
    (format t "[~4X] ~S => ~S~%"
            address inst regs)))

(defvar *uc* nil)
(defun set-up-tester ()
  (unless (null *uc*) (uc-close *uc*))
  (setf *uc* (uc-open :arm :arm))
  (uc-mem-map *uc* 0 #x1000 '(:read :write :exec))
  (uc-mem-write *uc* 0 ~test-code~)
  (uc-reg-write-batch *uc* (range 0 16) (loop repeat 16 collect 5)))


