        !COMPILER-GENERATED INTERFACE MODULE: Sun Jun 22 22:31:39 2025
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE CREATE_BUTTON__genmod
          INTERFACE 
            SUBROUTINE CREATE_BUTTON(HWNDPARENT,HBUTTON,ID)
              USE STANDARD
              TYPE (PTR), INTENT(IN) :: HWNDPARENT
              TYPE (PTR), INTENT(OUT) :: HBUTTON
              INTEGER(KIND=8), INTENT(IN) :: ID
            END SUBROUTINE CREATE_BUTTON
          END INTERFACE 
        END MODULE CREATE_BUTTON__genmod
