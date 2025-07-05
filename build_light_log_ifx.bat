@echo off
del *.exe

call "C:\Program Files (x86)\Intel\oneAPI\setvars.bat"

rem — compile modules with logical‐to‐C mapping and extended lines
ifx /c ^
    /warn:all ^
    /stand ^
    /check:all ^
    /fpscomp:logicals ^
    /extend_source ^
  win_types.f90 string_utils.f90 color_utils.f90 standard.f90 win_api.f90 gui_helpers.f90

rem — link with the same flags plus debug+opt
ifx /nologo ^
    /warn:all ^
    /stand ^
    /check:all ^
    /fpscomp:logicals ^
    /extend_source ^
    /standard-semantics ^
    /traceback ^
    /debug:full ^
    /O2 ^
    /o win.exe ^
  win.f90 win_types.obj string_utils.obj color_utils.obj standard.obj win_api.obj gui_helpers.obj user32.lib gdi32.lib

del *.obj
del *.mod

start /wait win.exe
pause
