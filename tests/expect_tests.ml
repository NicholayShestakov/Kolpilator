open Compile
open Types
open ToAssembly

let%expect_test "let n = 1 in let m = 2 in if n <= m then (m - n) else (n - m)"
    =
  Assembly.print
    (to_assembly_program
       [
         AFun
           ( "mod",
             [],
             ALet
               ( "n",
                 CIExpr (INum 1),
                 ALet
                   ( "m",
                     CIExpr (INum 2),
                     AIte
                       ( CLesseq (IId "n", IId "m"),
                         ACExpr (CSub (IId "m", IId "n")),
                         ACExpr (CSub (IId "n", IId "m")) ) ) ) );
         AFun
           ("main", [], ALet ("n", CCall ("mod", []), ACExpr (CIExpr (IId "n"))));
       ]);
  [%expect {|
    .section .text
    .global _start
    mod:
        addi sp, sp, -256
        sd ra, 0(sp)
        li s1, 1
        li s2, 2
        ble s1, s2, .then
        add s3, s1, s2
        mv a0, s3
        ld ra, 0(sp)
        addi sp, sp, 256
        ret
        j .if_end
    .then:
        add s3, s2, s1
        mv a0, s3
        ld ra, 0(sp)
        addi sp, sp, 256
        ret
    .if_end:
    _start:
        addi sp, sp, -256
        sd ra, 0(sp)
        call mod
        mv s1, a0
        mv s2, s1
        mv a0, s2
        ld ra, 0(sp)
        addi sp, sp, 256
        li a7, 93
        ecall
    |}]

let%expect_test "mccarthy91" =
  Assembly.print
    (to_assembly_program
       [
         AFun
           ( "mcc",
             [ "n" ],
             AIte
               ( CLesseq (IId "n", INum 100),
                 ALet
                   ( "n11",
                     CAdd (IId "n", INum 11),
                     ALet
                       ( "mcc11",
                         CCall ("mcc", [ IId "n11" ]),
                         ALet
                           ( "m",
                             CCall ("mcc", [ IId "mcc11" ]),
                             ACExpr (CIExpr (IId "m")) ) ) ),
                 ALet ("m", CSub (IId "n", INum 10), ACExpr (CIExpr (IId "m")))
               ) );
         AFun
           ( "main",
             [],
             ALet ("res", CCall ("mcc", [ INum 5 ]), ACExpr (CIExpr (IId "res")))
           );
       ]);
  [%expect {|
    .section .text
    .global _start
    mcc:
        addi sp, sp, -256
        sd ra, 0(sp)
        li t0, 100
        ble a0, t0, .then
        addi s1, a0, -10
        mv s2, s1
        mv a0, s2
        ld ra, 0(sp)
        addi sp, sp, 256
        ret
        j .if_end
    .then:
        addi s1, a0, 11
        sd s1, 8(sp)
        sd a0, 16(sp)
        mv a0, s1
        call mcc
        mv s2, a0
        ld s1, 8(sp)
        ld a0, 16(sp)
        sd s2, 8(sp)
        sd s1, 16(sp)
        sd a0, 24(sp)
        mv a0, s2
        call mcc
        mv s3, a0
        ld s2, 8(sp)
        ld s1, 16(sp)
        ld a0, 24(sp)
        mv s4, s3
        mv a0, s4
        ld ra, 0(sp)
        addi sp, sp, 256
        ret
    .if_end:
    _start:
        addi sp, sp, -256
        sd ra, 0(sp)
        li a0, 5
        call mcc
        mv s1, a0
        mv s2, s1
        mv a0, s2
        ld ra, 0(sp)
        addi sp, sp, 256
        li a7, 93
        ecall
    |}]

let%expect_test "fib" =
  Assembly.print
    (to_assembly_program
       [
         AFun
           ( "fib",
             [ "n" ],
             AIte
               ( CLesseq (IId "n", INum 1),
                 ACExpr (CIExpr (IId "n")),
                 ALet
                   ( "n1",
                     CSub (IId "n", INum 1),
                     ALet
                       ( "n2",
                         CSub (IId "n", INum 2),
                         ALet
                           ( "m",
                             CCall ("fib", [ IId "n1" ]),
                             ALet
                               ( "k",
                                 CCall ("fib", [ IId "n2" ]),
                                 ACExpr (CAdd (IId "m", IId "k")) ) ) ) ) ) );
         AFun
           ( "main",
             [],
             ALet ("res", CCall ("fib", [ INum 5 ]), ACExpr (CIExpr (IId "res")))
           );
       ]);
  [%expect {|
    .section .text
    .global _start
    fib:
        addi sp, sp, -256
        sd ra, 0(sp)
        li t0, 1
        ble a0, t0, .then
        addi s1, a0, -1
        addi s2, a0, -2
        sd s2, 8(sp)
        sd s1, 16(sp)
        sd a0, 24(sp)
        mv a0, s1
        call fib
        mv s3, a0
        ld s2, 8(sp)
        ld s1, 16(sp)
        ld a0, 24(sp)
        sd s3, 8(sp)
        sd s2, 16(sp)
        sd s1, 24(sp)
        sd a0, 32(sp)
        mv a0, s2
        call fib
        mv s4, a0
        ld s3, 8(sp)
        ld s2, 16(sp)
        ld s1, 24(sp)
        ld a0, 32(sp)
        add s5, s3, s4
        mv a0, s5
        ld ra, 0(sp)
        addi sp, sp, 256
        ret
        j .if_end
    .then:
        mv s1, a0
        mv a0, s1
        ld ra, 0(sp)
        addi sp, sp, 256
        ret
    .if_end:
    _start:
        addi sp, sp, -256
        sd ra, 0(sp)
        li a0, 5
        call fib
        mv s1, a0
        mv s2, s1
        mv a0, s2
        ld ra, 0(sp)
        addi sp, sp, 256
        li a7, 93
        ecall
    |}]

let%expect_test "fac" =
  Assembly.print
    (to_assembly_program
       [
         AFun
           ( "fac",
             [ "n" ],
             AIte
               ( CLesseq (IId "n", INum 1),
                 ACExpr (CIExpr (INum 1)),
                 ALet
                   ( "n1",
                     CSub (IId "n", INum 1),
                     ALet
                       ( "m",
                         CCall ("fac", [ IId "n1" ]),
                         ACExpr (CMul (IId "n", IId "m")) ) ) ) );
         AFun
           ( "main",
             [],
             ALet ("res", CCall ("fac", [ INum 5 ]), ACExpr (CIExpr (IId "res")))
           );
       ]);
  [%expect {|
    .section .text
    .global _start
    fac:
        addi sp, sp, -256
        sd ra, 0(sp)
        li t0, 1
        ble a0, t0, .then
        addi s1, a0, -1
        sd s1, 8(sp)
        sd a0, 16(sp)
        mv a0, s1
        call fac
        mv s2, a0
        ld s1, 8(sp)
        ld a0, 16(sp)
        mul s3, a0, s2
        mv a0, s3
        ld ra, 0(sp)
        addi sp, sp, 256
        ret
        j .if_end
    .then:
        li s1, 1
        mv a0, s1
        ld ra, 0(sp)
        addi sp, sp, 256
        ret
    .if_end:
    _start:
        addi sp, sp, -256
        sd ra, 0(sp)
        li a0, 5
        call fac
        mv s1, a0
        mv s2, s1
        mv a0, s2
        ld ra, 0(sp)
        addi sp, sp, 256
        li a7, 93
        ecall
    |}]
