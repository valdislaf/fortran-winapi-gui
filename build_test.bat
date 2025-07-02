where gfortran
gfortran --version

where ifx
ifx --version

rem --- Критический шаг: активируем окружение Intel oneAPI ---
call "C:\Program Files (x86)\Intel\oneAPI\setvars.bat"

ifx /nologo /standard-semantics /o test.exe test.f90 /link user32.lib gdi32.lib

.\test.exe

pause
