gfortran win.f90 -o win.exe -mwindows -std=f2008 -Wall -Wextra -pedantic  -Wimplicit-interface  -Wsurprising & REM -Wno-uninitialized  -fcheck=all 
REM chcp 1251
win.exe
pause