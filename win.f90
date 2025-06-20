
module win_types
  use iso_c_binding
  implicit none

  integer(c_int32_t), parameter :: WS_OVERLAPPEDWINDOW = INT(Z'00CF0000', c_int32_t)
  integer(c_int32_t), parameter :: SW_SHOW = 5
  integer(c_int32_t), parameter :: WM_DESTROY = INT(Z'0002', c_int32_t)
  integer, parameter :: c_wchar_t = selected_int_kind(2)
  
  type, bind(C) :: WNDCLASSEX
    integer(c_int32_t) :: cbSize
    integer(c_int32_t) :: style
    type(c_funptr)     :: lpfnWndProc
    integer(c_int32_t) :: cbClsExtra
    integer(c_int32_t) :: cbWndExtra
    type(c_ptr)        :: hInstance
    type(c_ptr)        :: hIcon
    type(c_ptr)        :: hCursor
    type(c_ptr)        :: hbrBackground
    type(c_ptr)        :: lpszMenuName
    type(c_ptr)        :: lpszClassName
    type(c_ptr)        :: hIconSm
  end type

  type, bind(C) :: MSG_T
    type(c_ptr)         :: hwnd
    integer(c_int32_t)  :: message
    integer(c_intptr_t) :: wParam
    integer(c_intptr_t) :: lParam
    integer(c_int32_t)  :: time
    type(c_ptr)         :: pt
  end type

end module win_types

module string_utils
  use iso_c_binding
  use win_types, only: c_wchar_t
  implicit none
contains

  function to_wide_null_terminated(text, max_len) result(wide)
    character(len=*), intent(in) :: text
    integer, intent(in) :: max_len
    character(kind=c_wchar_t), dimension(max_len) :: wide
    integer :: i, k

    wide = char(0, kind=c_wchar_t)  ! Заполнение нулями
    k = 1

    do i = 1, len_trim(text)
      if (k > max_len - 1) exit
      wide(k) = text(i:i)
      k = k + 1
      wide(k) = char(0, kind=c_wchar_t)
      k = k + 1
    end do
  end function

end module string_utils


function WndProc(hWnd, Msg, wParam, lParam) bind(C) result(res)
  use iso_c_binding
  use win_types
  implicit none
  type(c_ptr), value :: hWnd
  integer(c_int32_t), value :: Msg
  integer(c_intptr_t), value :: wParam, lParam
  integer(c_intptr_t) :: res

  interface
    function DefWindowProcW(hWnd, Msg, wParam, lParam) bind(C, name="DefWindowProcW")
      use iso_c_binding
      type(c_ptr), value :: hWnd
      integer(c_int32_t), value :: Msg
      integer(c_intptr_t), value :: wParam, lParam
      integer(c_intptr_t) :: DefWindowProcW
    end function

    subroutine PostQuitMessage(nExitCode) bind(C, name="PostQuitMessage")
      use iso_c_binding
      integer(c_int32_t), value :: nExitCode
    end subroutine
  end interface

  select case (Msg)
  case (WM_DESTROY)
    call PostQuitMessage(0)
    res = 0
  case default
    res = DefWindowProcW(hWnd, Msg, wParam, lParam)
  end select
end function

program WinMain
  use iso_c_binding
  use win_types
  use string_utils
  implicit none

  interface
  
    function RegisterClassExW(lpWndClass) bind(C, name="RegisterClassExW")
      use iso_c_binding
      type(c_ptr), value :: lpWndClass
      integer(c_int32_t) :: RegisterClassExW
    end function

    function GetLastError() bind(C, name="GetLastError")
      use iso_c_binding
      integer(c_int32_t) :: GetLastError
    end function

    function CreateWindowExW(dwExStyle, lpClassName, lpWindowName, dwStyle, x, y, nWidth, nHeight, &
                             hWndParent, hMenu, hInstance, lpParam) bind(C, name="CreateWindowExW")
      use iso_c_binding
      integer(c_int32_t), value :: dwExStyle, dwStyle, x, y, nWidth, nHeight
      type(c_ptr), value :: lpClassName, lpWindowName, hWndParent, hMenu, hInstance, lpParam
      type(c_ptr) :: CreateWindowExW
    end function

    subroutine ShowWindow(hWnd, nCmdShow) bind(C, name="ShowWindow")
      use iso_c_binding
      type(c_ptr), value :: hWnd
      integer(c_int32_t), value :: nCmdShow
    end subroutine

    subroutine UpdateWindow(hWnd) bind(C, name="UpdateWindow")
      use iso_c_binding
      type(c_ptr), value :: hWnd
    end subroutine

    function GetMessageW(lpMsg, hWnd, wMsgFilterMin, wMsgFilterMax) bind(C, name="GetMessageW")
      use iso_c_binding
      type(c_ptr), value :: lpMsg, hWnd
      integer(c_int32_t), value :: wMsgFilterMin, wMsgFilterMax
      integer(c_int32_t) :: GetMessageW
    end function

    subroutine TranslateMessage(lpMsg) bind(C, name="TranslateMessage")
      use iso_c_binding
      type(c_ptr), value :: lpMsg
    end subroutine

    subroutine DispatchMessageW(lpMsg) bind(C, name="DispatchMessageW")
      use iso_c_binding
      type(c_ptr), value :: lpMsg
    end subroutine

    function WndProc(hWnd, Msg, wParam, lParam) bind(C)
      use iso_c_binding
      type(c_ptr), value :: hWnd
      integer(c_int32_t), value :: Msg
      integer(c_intptr_t), value :: wParam, lParam
      integer(c_intptr_t) :: WndProc
    end function
	
    function GetSysColorBrush(nIndex) bind(C, name="GetSysColorBrush")
      use iso_c_binding
      integer(c_int32_t), value :: nIndex
      type(c_ptr) :: GetSysColorBrush
    end function
	
    function CreateSolidBrush(color) bind(C, name="CreateSolidBrush")
      use iso_c_binding
      integer(c_int32_t), value :: color
      type(c_ptr) :: CreateSolidBrush
    end function
	
    function LoadImageW(hInst, lpszName, uType, cxDesired, cyDesired, fuLoad) bind(C, name="LoadImageW")
      use iso_c_binding
      type(c_ptr), value :: hInst, lpszName
      integer(c_int32_t), value :: uType, cxDesired, cyDesired, fuLoad
      type(c_ptr) :: LoadImageW
    end function

  end interface
  
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  integer(c_int32_t) :: regResult
  type(WNDCLASSEX), target :: wcx
  type(MSG_T), target :: msg_inst
  type(c_ptr) :: hwnd, hInstance    
  !integer(c_int32_t), parameter :: COLOR_BTNFACE = 21  ! тёмно-серый системный цвет
  integer(c_int32_t) :: darkBrushColor
  type(c_ptr) :: hBrush  
  character(kind=c_wchar_t), dimension(32), target :: windowTitleW, classNameW
  integer(c_int32_t), parameter :: IMAGE_ICON = 1
  integer(c_int32_t), parameter :: LR_LOADFROMFILE = int(Z'0010', c_int32_t)
  character(kind=c_wchar_t), dimension(256), target :: iconPathW
  character(kind=c_wchar_t), dimension(64), target :: cursorPathW
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  cursorPathW(1:32) = to_wide_null_terminated("cross.ico", 32)
  iconPathW(1:32) = to_wide_null_terminated("favicon.ico", 32)

  darkBrushColor = int(Z'00321E0A', c_int32_t)  ! B=0A, G=1E, R=32
  hBrush = CreateSolidBrush(darkBrushColor)  

  classNameW(1:32) = to_wide_null_terminated("My window class", 32)
  windowTitleW(1:32) = to_wide_null_terminated("Fortran Window", 32)

  hInstance = c_null_ptr
  
  wcx%cbSize = c_sizeof(wcx)
  wcx%style = 0
  wcx%lpfnWndProc = c_funloc(WndProc)
  if (.not. c_associated(wcx%lpfnWndProc)) then
    print *, "WndProc binding failed!"
  else
    print *, "WndProc binding it's ok"
  end if
  wcx%cbClsExtra = 0
  wcx%cbWndExtra = 0
  wcx%hInstance = hInstance
  wcx%hIcon = LoadImageW(c_null_ptr, c_loc(iconPathW(1)), IMAGE_ICON, 0, 0, LR_LOADFROMFILE)
  wcx%hCursor = LoadImageW(c_null_ptr, c_loc(cursorPathW(1)), IMAGE_ICON, 0, 0, LR_LOADFROMFILE)
  !wcx%hbrBackground = GetSysColorBrush(COLOR_BTNFACE)
  wcx%hbrBackground = hBrush
  wcx%lpszMenuName = c_null_ptr
  wcx%lpszClassName = c_loc(classNameW(1))
  wcx%hIconSm = wcx%hIcon

  regResult = RegisterClassExW(c_loc(wcx))
  print *, "[DEBUG] className ptr: ", transfer(c_loc(classNameW(1)), 0_c_intptr_t)
  print *, "[DEBUG] windowTitle ptr: ", transfer(c_loc(windowTitleW(1)), 0_c_intptr_t)

  hwnd = CreateWindowExW(0, c_loc(classNameW(1)), c_loc(windowTitleW(1)), WS_OVERLAPPEDWINDOW, &
                        100, 100, 800, 600, c_null_ptr, c_null_ptr, hInstance, c_null_ptr)

  call ShowWindow(hwnd, SW_SHOW)
  call UpdateWindow(hwnd)

  do while (GetMessageW(c_loc(msg_inst), c_null_ptr, 0, 0) > 0)
    call TranslateMessage(c_loc(msg_inst))
    call DispatchMessageW(c_loc(msg_inst))
  end do
  
end program

