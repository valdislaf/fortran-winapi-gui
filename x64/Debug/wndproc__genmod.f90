        !COMPILER-GENERATED INTERFACE MODULE: Fri Jun 20 12:28:47 2025
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE WNDPROC__genmod
          INTERFACE 
            FUNCTION WNDPROC(HWND,MSG,WPARAM,LPARAM) RESULT(RES)        &
     & BIND(C, NAME = 'wndproc')
              USE ISO_C_BINDING
              TYPE (C_PTR) ,VALUE :: HWND
              INTEGER(KIND=4) ,VALUE :: MSG
              INTEGER(KIND=8) ,VALUE :: WPARAM
              INTEGER(KIND=8) ,VALUE :: LPARAM
              INTEGER(KIND=8) :: RES
            END FUNCTION WNDPROC
          END INTERFACE 
        END MODULE WNDPROC__genmod
