@echo off
REM --- Удалить старый EXE ---
del *.exe
call "C:\Program Files (x86)\Intel\oneAPI\setvars.bat"
REM --- Компиляция модулей ---
ifx /c win_types.f90 string_utils.f90 color_utils.f90 standard.f90 win_api.f90 gui_helpers.f90

REM --- Компоновка всех объектных файлов и main ---
ifx /nologo /standard-semantics /o win.exe win.f90 win_types.obj string_utils.obj color_utils.obj standard.obj win_api.obj gui_helpers.obj user32.lib gdi32.lib

REM --- Очистить объектники и модули ---
del *.obj
del *.mod

REM --- Запустить программу ---
win.exe

pause
