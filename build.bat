del *.exe
gfortran -c win_types.f90 string_utils.f90 color_utils.f90 standard.f90 win_api.f90 gui_helpers.f90
gfortran -o win.exe win.f90 win_types.o string_utils.o color_utils.o standard.o win_api.o gui_helpers.o -mwindows -std=f2008 -Wall -Wextra -pedantic  -Wimplicit-interface  -Wsurprising & REM -Wno-uninitialized  -fcheck=all 
REM chcp 1251
del *.o
del *.mod
win.exe
pause