        !COMPILER-GENERATED INTERFACE MODULE: Sun Jun 22 23:40:50 2025
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE CREATE_MAIN_WINDOW__genmod
          INTERFACE 
            SUBROUTINE CREATE_MAIN_WINDOW(HWND,HINSTANCE,APPDATAPTR,    &
     &HBRUSH,WCX,REGRESULT,CLASSNAMEW,WINDOWTITLEW)
              USE STANDARD
              TYPE (PTR), INTENT(OUT) :: HWND
              TYPE (PTR), INTENT(IN) :: HINSTANCE
              TYPE (C_PTR), INTENT(IN) :: APPDATAPTR
              TYPE (PTR), INTENT(IN) :: HBRUSH
              TYPE (WNDCLASSEX) ,TARGET, INTENT(OUT) :: WCX
              INTEGER(KIND=4), INTENT(OUT) :: REGRESULT
              CHARACTER(LEN=1) ,TARGET, INTENT(IN) :: CLASSNAMEW(:)
              CHARACTER(LEN=1) ,TARGET, INTENT(IN) :: WINDOWTITLEW(:)
            END SUBROUTINE CREATE_MAIN_WINDOW
          END INTERFACE 
        END MODULE CREATE_MAIN_WINDOW__genmod
