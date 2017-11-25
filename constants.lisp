(in-package :unicorn)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Some Constants for Mode and Arch ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcenum uc-arch
  (:ARM 1)
  (:ARM64 2)
  (:MIPS 3)
  (:X86 4)
  (:PPC 5)
  (:SPARC 6)
  (:M86K 7)
  (:MAX 8))

(defcenum uc-mode
  (:LITTLE-ENDIAN 0)  ;; little-endian mode (default mode)
  (:BIG-ENDIAN #.(ash 1 30))  ;; big-endian mode
  ;; arm / arm64
  (:ARM 0)  ;; ARM mode
  (:THUMB #.(ash 1 4))  ;; THUMB mode (including Thumb-2)
  (:MCLASS #.(ash 1 5))  ;; ARM's Cortex-M series (currently unsupported)
  (:V8 #.(ash 1 6))  ;; ARMv8 A32 encodings for ARM (currently unsupported)
  ;; mips
  (:MICRO #.(ash 1 4))  ;; MicroMips mode (currently unsupported)
  (:MIPS3 #.(ash 1 5))  ;; Mips III ISA (currently unsupported)
  (:MIPS32R6 #.(ash 1 6))  ;; Mips32r6 ISA (currently unsupported)
  (:MIPS32 #.(ash 1 2))  ;; Mips32 ISA
  (:MIPS64 #.(ash 1 3))  ;; Mips64 ISA
  ;; x86 / x64
  (:16BIT #.(ash 1 1))  ;; 16-bit mode
  (:32BIT #.(ash 1 2))  ;; 32-bit mode
  (:64BIT #.(ash 1 3))  ;; 64-bit mode
  ;; ppc
  (:PPC32 #.(ash 1 2))  ;; 32-bit mode (currently unsupported)
  (:PPC64 #.(ash 1 3))  ;; 64-bit mode (currently unsupported)
  (:QPX #.(ash 1 4))  ;; Quad Processing eXtensions mode (currently unsupported)
  ;; sparc
  (:SPARC32 #.(ash 1 2))  ;; 32-bit mode
  (:SPARC64 #.(ash 1 3))  ;; 64-bit mode
  (:V9 #.(ash 1 4)))  ;; SparcV9 mode (currently unsupported)

(export 'uc-err)
(defcenum uc-err
  :OK  ;; No error: everything was fine
  :NOMEM  ;; Out-Of-Memory error: uc_open(), uc_emulate()
  :ARCH  ;; Unsupported architecture: uc_open()
  :HANDLE  ;; Invalid handle
  :MODE  ;; Invalid/unsupported mode: uc_open()
  :VERSION  ;; Unsupported version (bindings)
  :READ-UNMAPPED  ;; Quit emulation due to READ on unmapped memory: uc_emu_start()
  :WRITE-UNMAPPED  ;; Quit emulation due to WRITE on unmapped memory: uc_emu_start()
  :FETCH-UNMAPPED  ;; Quit emulation due to FETCH on unmapped memory: uc_emu_start()
  :HOOK  ;; Invalid hook type: uc_hook_add()
  :INSN-INVALID  ;; Quit emulation due to invalid instruction: uc_emu_start()
  :MAP  ;; Invalid memory mapping: uc_mem_map()
  :WRITE-PROT  ;; Quit emulation due to UC_MEM_WRITE_PROT violation: uc_emu_start()
  :READ-PROT  ;; Quit emulation due to UC_MEM_READ_PROT violation: uc_emu_start()
  :FETCH-PROT  ;; Quit emulation due to UC_MEM_FETCH_PROT violation: uc_emu_start()
  :ARG  ;; Inavalid argument provided to uc_xxx function (See specific function API)
  :READ-UNALIGNED  ;; Unaligned read
  :WRITE-UNALIGNED  ;; Unaligned write
  :FETCH-UNALIGNED  ;; Unaligned fetch
  :HOOK-EXIST  ;; hook for this event already existed
  :RESOURCE   ;; Insufficient resource: uc_emu_start()
  :EXCEPTION
  :UNHANDLED) ;; Unhandled CPU exception

(export 'errorcode->int)
(defun errorcode->int (errorcode)
  (foreign-enum-value 'uc-err errorcode))

(defcenum uc-mem-type
  (:READ 16)  ;; Memory is read from
  :WRITE   ;; Memory is written to
  :FETCH   ;; Memory is fetched
  :READ-UNMAPPED   ;; Unmapped memory is read from
  :WRITE-UNMAPPED   ;; Unmapped memory is written to
  :FETCH-UNMAPPED   ;; Unmapped memory is fetched
  :WRITE-PROT   ;; Write to write protected, but mapped, memory
  :READ-PROT   ;; Read from read protected, but mapped, memory
  :FETCH-PROT   ;; Fetch from non-executable, but mapped, memory
  :READ-AFTER   ;; Memory is read from (successful access)
  )

(defcstruct uc-mem-region
  (begin :uint64)  ;; begin address of the region (inclusive)
  (end   :uint64)  ;; end address of the region (inclusive)
  (perms :uint32)) ;; memory permissions of the region


(defcenum uc-query-type
  ;; Dynamically query current hardware mode.
  (:MODE 1)
  :PAGE-SIZE)

(defcenum uc-prot
  (:NONE 0)
  (:READ 1)
  (:WRITE 2)
  (:EXEC 4)
  (:ALL 7))

(defun merge-flags (flags &key (enum-type 'uc-prot))
  (reduce #'logior
          flags
          :initial-value 0
          :key (lambda (x)
                 (foreign-enum-value enum-type x))))

(defctype unicorn-engine :pointer)
(defctype size :uint)


(defcenum uc-hook-type
  ;; Hook all interrupt/syscall events
  (:INTR #.(ash 1 0))
  ;; Hook a particular instruction - only a very small subset of instructions supported here
  (:INSN #.(ash 1 1))
  ;; Hook a range of code
  (:CODE #.(ash 1 2))
  ;; Hook basic blocks
  (:BLOCK #.(ash 1 3))
  ;; Hook for memory read on unmapped memory
  (:MEM_READ_UNMAPPED #.(ash 1 4))
  ;; Hook for invalid memory write events
  (:MEM_WRITE_UNMAPPED #.(ash 1 5))
  ;; Hook for invalid memory fetch for execution events
  (:MEM_FETCH_UNMAPPED #.(ash 1 6))
  ;; Hook for memory read on read-protected memory
  (:MEM_READ_PROT #.(ash 1 7))
  ;; Hook for memory write on write-protected memory
  (:MEM_WRITE_PROT #.(ash 1 8))
  ;; Hook for memory fetch on non-executable memory
  (:MEM_FETCH_PROT #.(ash 1 9))
  ;; Hook memory read events.
  (:MEM_READ #.(ash 1 10))
  ;; Hook memory write events.
  (:MEM_WRITE #.(ash 1 11))
  ;; Hook memory fetch for execution events
  (:MEM_FETCH #.(ash 1 12))
  ;; Hook memory read events, but only successful access.
  ;; The callback will be triggered after successful read.
  (:MEM_READ_AFTER #.(ash 1 13)))

(defparameter +reg-ids+
  (list
   (list :x86 (list '(:INVALID . 0)
                    '(:AH . 1)
                    '(:AL . 2)
                    '(:AX . 3)
                    '(:BH . 4)
                    '(:BL . 5)
                    '(:BP . 6)
                    '(:BPL . 7)
                    '(:BX . 8)
                    '(:CH . 9)
                    '(:CL . 10)
                    '(:CS . 11)
                    '(:CX . 12)
                    '(:DH . 13)
                    '(:DI . 14)
                    '(:DIL . 15)
                    '(:DL . 16)
                    '(:DS . 17)
                    '(:DX . 18)
                    '(:EAX . 19)
                    '(:EBP . 20)
                    '(:EBX . 21)
                    '(:ECX . 22)
                    '(:EDI . 23)
                    '(:EDX . 24)
                    '(:EFLAGS . 25)
                    '(:EIP . 26)
                    '(:EIZ . 27)
                    '(:ES . 28)
                    '(:ESI . 29)
                    '(:ESP . 30)
                    '(:FPSW . 31)
                    '(:FS . 32)
                    '(:GS . 33)
                    '(:IP . 34)
                    '(:RAX . 35)
                    '(:RBP . 36)
                    '(:RBX . 37)
                    '(:RCX . 38)
                    '(:RDI . 39)
                    '(:RDX . 40)
                    '(:RIP . 41)
                    '(:RIZ . 42)
                    '(:RSI . 43)
                    '(:RSP . 44)
                    '(:SI . 45)
                    '(:SIL . 46)
                    '(:SP . 47)
                    '(:SPL . 48)
                    '(:SS . 49)
                    '(:CR0 . 50)
                    '(:CR1 . 51)
                    '(:CR2 . 52)
                    '(:CR3 . 53)
                    '(:CR4 . 54)
                    '(:CR5 . 55)
                    '(:CR6 . 56)
                    '(:CR7 . 57)
                    '(:CR8 . 58)
                    '(:CR9 . 59)
                    '(:CR10 . 60)
                    '(:CR11 . 61)
                    '(:CR12 . 62)
                    '(:CR13 . 63)
                    '(:CR14 . 64)
                    '(:CR15 . 65)
                    '(:DR0 . 66)
                    '(:DR1 . 67)
                    '(:DR2 . 68)
                    '(:DR3 . 69)
                    '(:DR4 . 70)
                    '(:DR5 . 71)
                    '(:DR6 . 72)
                    '(:DR7 . 73)
                    '(:DR8 . 74)
                    '(:DR9 . 75)
                    '(:DR10 . 76)
                    '(:DR11 . 77)
                    '(:DR12 . 78)
                    '(:DR13 . 79)
                    '(:DR14 . 80)
                    '(:DR15 . 81)
                    '(:FP0 . 82)
                    '(:FP1 . 83)
                    '(:FP2 . 84)
                    '(:FP3 . 85)
                    '(:FP4 . 86)
                    '(:FP5 . 87)
                    '(:FP6 . 88)
                    '(:FP7 . 89)
                    '(:K0 . 90)
                    '(:K1 . 91)
                    '(:K2 . 92)
                    '(:K3 . 93)
                    '(:K4 . 94)
                    '(:K5 . 95)
                    '(:K6 . 96)
                    '(:K7 . 97)
                    '(:MM0 . 98)
                    '(:MM1 . 99)
                    '(:MM2 . 100)
                    '(:MM3 . 101)
                    '(:MM4 . 102)
                    '(:MM5 . 103)
                    '(:MM6 . 104)
                    '(:MM7 . 105)
                    '(:R8 . 106)
                    '(:R9 . 107)
                    '(:R10 . 108)
                    '(:R11 . 109)
                    '(:R12 . 110)
                    '(:R13 . 111)
                    '(:R14 . 112)
                    '(:R15 . 113)
                    '(:ST0 . 114)
                    '(:ST1 . 115)
                    '(:ST2 . 116)
                    '(:ST3 . 117)
                    '(:ST4 . 118)
                    '(:ST5 . 119)
                    '(:ST6 . 120)
                    '(:ST7 . 121)
                    '(:XMM0 . 122)
                    '(:XMM1 . 123)
                    '(:XMM2 . 124)
                    '(:XMM3 . 125)
                    '(:XMM4 . 126)
                    '(:XMM5 . 127)
                    '(:XMM6 . 128)
                    '(:XMM7 . 129)
                    '(:XMM8 . 130)
                    '(:XMM9 . 131)
                    '(:XMM10 . 132)
                    '(:XMM11 . 133)
                    '(:XMM12 . 134)
                    '(:XMM13 . 135)
                    '(:XMM14 . 136)
                    '(:XMM15 . 137)
                    '(:XMM16 . 138)
                    '(:XMM17 . 139)
                    '(:XMM18 . 140)
                    '(:XMM19 . 141)
                    '(:XMM20 . 142)
                    '(:XMM21 . 143)
                    '(:XMM22 . 144)
                    '(:XMM23 . 145)
                    '(:XMM24 . 146)
                    '(:XMM25 . 147)
                    '(:XMM26 . 148)
                    '(:XMM27 . 149)
                    '(:XMM28 . 150)
                    '(:XMM29 . 151)
                    '(:XMM30 . 152)
                    '(:XMM31 . 153)
                    '(:YMM0 . 154)
                    '(:YMM1 . 155)
                    '(:YMM2 . 156)
                    '(:YMM3 . 157)
                    '(:YMM4 . 158)
                    '(:YMM5 . 159)
                    '(:YMM6 . 160)
                    '(:YMM7 . 161)
                    '(:YMM8 . 162)
                    '(:YMM9 . 163)
                    '(:YMM10 . 164)
                    '(:YMM11 . 165)
                    '(:YMM12 . 166)
                    '(:YMM13 . 167)
                    '(:YMM14 . 168)
                    '(:YMM15 . 169)
                    '(:YMM16 . 170)
                    '(:YMM17 . 171)
                    '(:YMM18 . 172)
                    '(:YMM19 . 173)
                    '(:YMM20 . 174)
                    '(:YMM21 . 175)
                    '(:YMM22 . 176)
                    '(:YMM23 . 177)
                    '(:YMM24 . 178)
                    '(:YMM25 . 179)
                    '(:YMM26 . 180)
                    '(:YMM27 . 181)
                    '(:YMM28 . 182)
                    '(:YMM29 . 183)
                    '(:YMM30 . 184)
                    '(:YMM31 . 185)
                    '(:ZMM0 . 186)
                    '(:ZMM1 . 187)
                    '(:ZMM2 . 188)
                    '(:ZMM3 . 189)
                    '(:ZMM4 . 190)
                    '(:ZMM5 . 191)
                    '(:ZMM6 . 192)
                    '(:ZMM7 . 193)
                    '(:ZMM8 . 194)
                    '(:ZMM9 . 195)
                    '(:ZMM10 . 196)
                    '(:ZMM11 . 197)
                    '(:ZMM12 . 198)
                    '(:ZMM13 . 199)
                    '(:ZMM14 . 200)
                    '(:ZMM15 . 201)
                    '(:ZMM16 . 202)
                    '(:ZMM17 . 203)
                    '(:ZMM18 . 204)
                    '(:ZMM19 . 205)
                    '(:ZMM20 . 206)
                    '(:ZMM21 . 207)
                    '(:ZMM22 . 208)
                    '(:ZMM23 . 209)
                    '(:ZMM24 . 210)
                    '(:ZMM25 . 211)
                    '(:ZMM26 . 212)
                    '(:ZMM27 . 213)
                    '(:ZMM28 . 214)
                    '(:ZMM29 . 215)
                    '(:ZMM30 . 216)
                    '(:ZMM31 . 217)
                    '(:R8B . 218)
                    '(:R9B . 219)
                    '(:R10B . 220)
                    '(:R11B . 221)
                    '(:R12B . 222)
                    '(:R13B . 223)
                    '(:R14B . 224)
                    '(:R15B . 225)
                    '(:R8D . 226)
                    '(:R9D . 227)
                    '(:R10D . 228)
                    '(:R11D . 229)
                    '(:R12D . 230)
                    '(:R13D . 231)
                    '(:R14D . 232)
                    '(:R15D . 233)
                    '(:R8W . 234)
                    '(:R9W . 235)
                    '(:R10W . 236)
                    '(:R11W . 237)
                    '(:R12W . 238)
                    '(:R13W . 239)
                    '(:R14W . 240)
                    '(:R15W . 241)
                    '(:IDTR . 242)
                    '(:GDTR . 243)
                    '(:LDTR . 244)
                    '(:TR . 245)
                    '(:FPCW . 246)
                    '(:FPTAG . 247)
                    '(:MSR . 248)
                    '(:ENDING . 249)))
   (list :mips (list '(:INVALID . 0)
                     ;; General purpose registers
                     '(:PC . 1)
                     '(:R0 . 2)
                     '(:R1 . 3)
                     '(:R2 . 4)
                     '(:R3 . 5)
                     '(:R4 . 6)
                     '(:R5 . 7)
                     '(:R6 . 8)
                     '(:R7 . 9)
                     '(:R8 . 10)
                     '(:R9 . 11)
                     '(:R10 . 12)
                     '(:R11 . 13)
                     '(:R12 . 14)
                     '(:R13 . 15)
                     '(:R14 . 16)
                     '(:R15 . 17)
                     '(:R16 . 18)
                     '(:R17 . 19)
                     '(:R18 . 20)
                     '(:R19 . 21)
                     '(:R20 . 22)
                     '(:R21 . 23)
                     '(:R22 . 24)
                     '(:R23 . 25)
                     '(:R24 . 26)
                     '(:R25 . 27)
                     '(:R26 . 28)
                     '(:R27 . 29)
                     '(:R28 . 30)
                     '(:R29 . 31)
                     '(:R30 . 32)
                     '(:R31 . 33)

                     ;; DSP registers
                     '(:DSPCCOND . 34)
                     '(:DSPCARRY . 35)
                     '(:DSPEFI . 36)
                     '(:DSPOUTFLAG . 37)
                     '(:DSPOUTFLAG16_19 . 38)
                     '(:DSPOUTFLAG20 . 39)
                     '(:DSPOUTFLAG21 . 40)
                     '(:DSPOUTFLAG22 . 41)
                     '(:DSPOUTFLAG23 . 42)
                     '(:DSPPOS . 43)
                     '(:DSPSCOUNT . 44)

                     ;; ACC registers
                     '(:AC0 . 45)
                     '(:AC1 . 46)
                     '(:AC2 . 47)
                     '(:AC3 . 48)

                     ;; COP registers
                     '(:CC0 . 49)
                     '(:CC1 . 50)
                     '(:CC2 . 51)
                     '(:CC3 . 52)
                     '(:CC4 . 53)
                     '(:CC5 . 54)
                     '(:CC6 . 55)
                     '(:CC7 . 56)

                     ;; FPU registers
                     '(:F0 . 57)
                     '(:F1 . 58)
                     '(:F2 . 59)
                     '(:F3 . 60)
                     '(:F4 . 61)
                     '(:F5 . 62)
                     '(:F6 . 63)
                     '(:F7 . 64)
                     '(:F8 . 65)
                     '(:F9 . 66)
                     '(:F10 . 67)
                     '(:F11 . 68)
                     '(:F12 . 69)
                     '(:F13 . 70)
                     '(:F14 . 71)
                     '(:F15 . 72)
                     '(:F16 . 73)
                     '(:F17 . 74)
                     '(:F18 . 75)
                     '(:F19 . 76)
                     '(:F20 . 77)
                     '(:F21 . 78)
                     '(:F22 . 79)
                     '(:F23 . 80)
                     '(:F24 . 81)
                     '(:F25 . 82)
                     '(:F26 . 83)
                     '(:F27 . 84)
                     '(:F28 . 85)
                     '(:F29 . 86)
                     '(:F30 . 87)
                     '(:F31 . 88)
                     '(:FCC0 . 89)
                     '(:FCC1 . 90)
                     '(:FCC2 . 91)
                     '(:FCC3 . 92)
                     '(:FCC4 . 93)
                     '(:FCC5 . 94)
                     '(:FCC6 . 95)
                     '(:FCC7 . 96)

                     ;; AFPR128
                     '(:W0 . 97)
                     '(:W1 . 98)
                     '(:W2 . 99)
                     '(:W3 . 100)
                     '(:W4 . 101)
                     '(:W5 . 102)
                     '(:W6 . 103)
                     '(:W7 . 104)
                     '(:W8 . 105)
                     '(:W9 . 106)
                     '(:W10 . 107)
                     '(:W11 . 108)
                     '(:W12 . 109)
                     '(:W13 . 110)
                     '(:W14 . 111)
                     '(:W15 . 112)
                     '(:W16 . 113)
                     '(:W17 . 114)
                     '(:W18 . 115)
                     '(:W19 . 116)
                     '(:W20 . 117)
                     '(:W21 . 118)
                     '(:W22 . 119)
                     '(:W23 . 120)
                     '(:W24 . 121)
                     '(:W25 . 122)
                     '(:W26 . 123)
                     '(:W27 . 124)
                     '(:W28 . 125)
                     '(:W29 . 126)
                     '(:W30 . 127)
                     '(:W31 . 128)
                     '(:HI . 129)
                     '(:LO . 130)
                     '(:P0 . 131)
                     '(:P1 . 132)
                     '(:P2 . 133)
                     '(:MPL0 . 134)
                     '(:MPL1 . 135)
                     '(:MPL2 . 136)
                     '(:ENDING . 137)
                     '(:ZERO . 2)
                     '(:AT . 3)
                     '(:V0 . 4)
                     '(:V1 . 5)
                     '(:A0 . 6)
                     '(:A1 . 7)
                     '(:A2 . 8)
                     '(:A3 . 9)
                     '(:T0 . 10)
                     '(:T1 . 11)
                     '(:T2 . 12)
                     '(:T3 . 13)
                     '(:T4 . 14)
                     '(:T5 . 15)
                     '(:T6 . 16)
                     '(:T7 . 17)
                     '(:S0 . 18)
                     '(:S1 . 19)
                     '(:S2 . 20)
                     '(:S3 . 21)
                     '(:S4 . 22)
                     '(:S5 . 23)
                     '(:S6 . 24)
                     '(:S7 . 25)
                     '(:T8 . 26)
                     '(:T9 . 27)
                     '(:K0 . 28)
                     '(:K1 . 29)
                     '(:GP . 30)
                     '(:SP . 31)
                     '(:FP . 32)
                     '(:S8 . 32)
                     '(:RA . 33)
                     '(:HI0 . 45)
                     '(:HI1 . 46)
                     '(:HI2 . 47)
                     '(:HI3 . 48)
                     '(:LO0 . 45)
                     '(:LO1 . 46)
                     '(:LO2 . 47)
                     '(:LO3 . 48)))
   ;; ARM registers
   (list :arm (list '(:INVALID . 0)
                    '(:APSR . 1)
                    '(:APSR_NZCV . 2)
                    '(:CPSR . 3)
                    '(:FPEXC . 4)
                    '(:FPINST . 5)
                    '(:FPSCR . 6)
                    '(:FPSCR_NZCV . 7)
                    '(:FPSID . 8)
                    '(:ITSTATE . 9)
                    '(:LR . 10)
                    '(:PC . 11)
                    '(:SP . 12)
                    '(:SPSR . 13)
                    '(:D0 . 14)
                    '(:D1 . 15)
                    '(:D2 . 16)
                    '(:D3 . 17)
                    '(:D4 . 18)
                    '(:D5 . 19)
                    '(:D6 . 20)
                    '(:D7 . 21)
                    '(:D8 . 22)
                    '(:D9 . 23)
                    '(:D10 . 24)
                    '(:D11 . 25)
                    '(:D12 . 26)
                    '(:D13 . 27)
                    '(:D14 . 28)
                    '(:D15 . 29)
                    '(:D16 . 30)
                    '(:D17 . 31)
                    '(:D18 . 32)
                    '(:D19 . 33)
                    '(:D20 . 34)
                    '(:D21 . 35)
                    '(:D22 . 36)
                    '(:D23 . 37)
                    '(:D24 . 38)
                    '(:D25 . 39)
                    '(:D26 . 40)
                    '(:D27 . 41)
                    '(:D28 . 42)
                    '(:D29 . 43)
                    '(:D30 . 44)
                    '(:D31 . 45)
                    '(:FPINST2 . 46)
                    '(:MVFR0 . 47)
                    '(:MVFR1 . 48)
                    '(:MVFR2 . 49)
                    '(:Q0 . 50)
                    '(:Q1 . 51)
                    '(:Q2 . 52)
                    '(:Q3 . 53)
                    '(:Q4 . 54)
                    '(:Q5 . 55)
                    '(:Q6 . 56)
                    '(:Q7 . 57)
                    '(:Q8 . 58)
                    '(:Q9 . 59)
                    '(:Q10 . 60)
                    '(:Q11 . 61)
                    '(:Q12 . 62)
                    '(:Q13 . 63)
                    '(:Q14 . 64)
                    '(:Q15 . 65)
                    '(:R0 . 66)
                    '(:R1 . 67)
                    '(:R2 . 68)
                    '(:R3 . 69)
                    '(:R4 . 70)
                    '(:R5 . 71)
                    '(:R6 . 72)
                    '(:R7 . 73)
                    '(:R8 . 74)
                    '(:R9 . 75)
                    '(:R10 . 76)
                    '(:R11 . 77)
                    '(:R12 . 78)
                    '(:S0 . 79)
                    '(:S1 . 80)
                    '(:S2 . 81)
                    '(:S3 . 82)
                    '(:S4 . 83)
                    '(:S5 . 84)
                    '(:S6 . 85)
                    '(:S7 . 86)
                    '(:S8 . 87)
                    '(:S9 . 88)
                    '(:S10 . 89)
                    '(:S11 . 90)
                    '(:S12 . 91)
                    '(:S13 . 92)
                    '(:S14 . 93)
                    '(:S15 . 94)
                    '(:S16 . 95)
                    '(:S17 . 96)
                    '(:S18 . 97)
                    '(:S19 . 98)
                    '(:S20 . 99)
                    '(:S21 . 100)
                    '(:S22 . 101)
                    '(:S23 . 102)
                    '(:S24 . 103)
                    '(:S25 . 104)
                    '(:S26 . 105)
                    '(:S27 . 106)
                    '(:S28 . 107)
                    '(:S29 . 108)
                    '(:S30 . 109)
                    '(:S31 . 110)
                    '(:C1_C0_2 . 111)
                    '(:C13_C0_2 . 112)
                    '(:C13_C0_3 . 113)
                    '(:ENDING . 114)

                    ;; alias registers
                    '(:R13 . 12)
                    '(:R14 . 10)
                    '(:R15 . 11)
                    '(:SB . 75)
                    '(:SL . 76)
                    '(:FP . 77)
                    '(:IP . 78)

                    ;; some aliases added for convenience
                    '(0 . 66)
                    '(1 . 67)
                    '(2 . 68)
                    '(3 . 69)
                    '(4 . 70)
                    '(5 . 71)
                    '(6 . 72)
                    '(7 . 73)
                    '(8 . 74)
                    '(9 . 75)
                    '(10 . 76)
                    '(11 . 77)
                    '(12 . 78)
                    '(13 . 12)
                    '(14 . 10)
                    '(15 . 11)))))
