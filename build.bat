gfortran win.f95 -o win.exe -mwindows -std=f2008 -Wall -Wextra -pedantic  -Wimplicit-interface  -Wsurprising & REM -fcheck=all 
REM chcp 1251
win.exe
pause