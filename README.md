# Minimal WinAPI GUI App in Fortran

This is a minimal graphical Windows application written in modern Fortran using the WinAPI.  
It demonstrates how to create a basic window, handle events, show Unicode titles, and load custom icons.

## Features

- Written in Fortran (F2008 standard)
- Uses raw Windows API bindings
- Unicode window title (UTF-16 via `c_wchar_t`)
- Loads `.ico` icon via `LoadImageW`
- Custom `WndProc` handler
- Background brush and small/large icon handling

## Build

Use `ifx` with these flags:

```bash
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

```

Use `gfortran` with these flags:

```bash
del *.exe
gfortran -c win_types.f90 string_utils.f90 standard.f90 win_api.f90 gui_helpers.f90
gfortran -o win.exe win.f90 win_types.o string_utils.o standard.o win_api.o gui_helpers.o -mwindows -std=f2008 -Wall -Wextra -pedantic  -Wimplicit-interface  -Wsurprising 
REM chcp 1251
del *.o
del *.mod
win.exe
pause
```

Or use the provided `build.bat`.

## Screenshot

![App Screenshot](screenshot.png)

## License

This project is released under the MIT License. You are free to use, modify, and distribute it.
