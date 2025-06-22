module win_types
use iso_c_binding, only: int => c_int32_t, i_ptr => c_intptr_t, ptr => c_ptr, f_ptr => c_funptr, nullptr => c_null_ptr, char => c_char
  implicit none

  integer(int), parameter :: WS_OVERLAPPEDWINDOW = Z'00CF0000'
  integer(int), parameter :: SW_SHOW = 5
  integer(int), parameter :: WM_DESTROY = Z'0002'
  
  type, bind(C) :: WNDCLASSEX
    integer(int) :: cbSize
    integer(int) :: style
    type(f_ptr)     :: lpfnWndProc
    integer(int) :: cbClsExtra
    integer(int) :: cbWndExtra
    type(ptr)        :: hInstance
    type(ptr)        :: hIcon
    type(ptr)        :: hCursor
    type(ptr)        :: hbrBackground
    type(ptr)        :: lpszMenuName
    type(ptr)        :: lpszClassName
    type(ptr)        :: hIconSm
  end type

  type, bind(C) :: MSG_T
    type(ptr)         :: hwnd
    integer(int)  :: message
    integer(i_ptr) :: wParam
    integer(i_ptr) :: lParam
    integer(int)  :: time
    type(ptr)         :: pt
  end type

end module win_types

module string_utils
  use win_types
  implicit none
contains
     function to_wide_null_terminated(text) result(wide)
      use iso_c_binding
      implicit none
      character(len=*), intent(in) :: text
      character(kind=char), allocatable :: wide(:)
      integer :: i, k, n
      n = len_trim(text)
      allocate(wide(2 * n + 1))
      wide = achar(0)
      k = 1
      do i = 1, n
        wide(k) = text(i:i)
        k = k + 1
        wide(k) =  achar(0) 
        k = k + 1
      end do
    end function
end module string_utils

module standard
  use iso_c_binding
  use win_types
  use string_utils
end module standard 
    
module win_api
    interface    
    function RegisterClassExW(lpWndClass) bind(C, name="RegisterClassExW")
      use standard
      type(ptr), value :: lpWndClass
      integer(int) :: RegisterClassExW
    end function

    function GetLastError() bind(C, name="GetLastError")
      use standard
      integer(int) :: GetLastError
    end function

    function CreateWindowExW(dwExStyle, lpClassName, lpWindowName, dwStyle, x, y, nWidth, nHeight, &
                             hWndParent, hMenu, hInstance, lpParam) bind(C, name="CreateWindowExW")
      use standard
      integer(int), value :: dwExStyle, dwStyle, x, y, nWidth, nHeight
      type(ptr), value :: lpClassName, lpWindowName, hWndParent, hMenu, hInstance, lpParam
      type(ptr) :: CreateWindowExW
    end function

    subroutine ShowWindow(hWnd, nCmdShow) bind(C, name="ShowWindow")
      use standard
      type(ptr), value :: hWnd
      integer(int), value :: nCmdShow
    end subroutine

    subroutine UpdateWindow(hWnd) bind(C, name="UpdateWindow")
      use standard
      type(ptr), value :: hWnd
    end subroutine

    function GetMessageW(lpMsg, hWnd, wMsgFilterMin, wMsgFilterMax) bind(C, name="GetMessageW")
      use standard
      type(ptr), value :: lpMsg, hWnd
      integer(int), value :: wMsgFilterMin, wMsgFilterMax
      integer(int) :: GetMessageW
    end function

    subroutine TranslateMessage(lpMsg) bind(C, name="TranslateMessage")
      use standard
      type(ptr), value :: lpMsg
    end subroutine

    subroutine DispatchMessageW(lpMsg) bind(C, name="DispatchMessageW")
      use standard
      type(ptr), value :: lpMsg
    end subroutine

    function WndProc(hWnd, Msg, wParam, lParam) bind(C)
      use standard
      type(ptr), value :: hWnd
      integer(int), value :: Msg
      integer(i_ptr), value :: wParam, lParam
      integer(i_ptr) :: WndProc
    end function
	
    function GetSysColorBrush(nIndex) bind(C, name="GetSysColorBrush")
      use standard
      integer(int), value :: nIndex
      type(ptr) :: GetSysColorBrush
    end function
	
    function CreateSolidBrush(color) bind(C, name="CreateSolidBrush")
      use standard
      integer(int), value :: color
      type(ptr) :: CreateSolidBrush
    end function
	
    function LoadImageW(hInst, lpszName, uType, cxDesired, cyDesired, fuLoad) bind(C, name="LoadImageW")
      use standard
      type(ptr), value :: hInst, lpszName
      integer(int), value :: uType, cxDesired, cyDesired, fuLoad
      type(ptr) :: LoadImageW
    end function
    
    function DefWindowProcW(hWnd, Msg, wParam, lParam) bind(C, name="DefWindowProcW")
      use standard
      type(ptr), value :: hWnd
      integer(int), value :: Msg
      integer(i_ptr), value :: wParam, lParam
      integer(i_ptr) :: DefWindowProcW
    end function

    subroutine PostQuitMessage(nExitCode) bind(C, name="PostQuitMessage")
      use standard
      integer(int), value :: nExitCode
    end subroutine
    
  end interface
end module win_api
        
function WndProc(hWnd, Msg, wParam, lParam) bind(C) result(res)
  use standard
  use win_api
  implicit none
  type(ptr), value :: hWnd
  integer(int), value :: Msg
  integer(i_ptr), value :: wParam, lParam
  integer(i_ptr) :: res

  select case (Msg)
  case (WM_DESTROY)
    call PostQuitMessage(0)
    res = 0
  case default
    res = DefWindowProcW(hWnd, Msg, wParam, lParam)
  end select
end function

program WinMain
  use win_api
  use standard
  implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  integer(int) :: regResult
  type(WNDCLASSEX), target :: wcx
  type(MSG_T), target :: msg_inst
  type(ptr) :: hwnd, hInstance    
  !integer(int), parameter :: COLOR_BTNFACE = 21  ! тёмно-серый системный цвет
  integer(int) :: darkBrushColor
  type(ptr) :: hBrush  
  character(kind=char), allocatable, target :: windowTitleW(:), classNameW(:), iconPathW(:), cursorPathW(:)
  integer(int), parameter :: IMAGE_ICON = 1
  integer(int), parameter :: LR_LOADFROMFILE = Z'0010'
  ! доп панель
  integer(int), parameter :: WS_VISIBLE = Z'10000000'
  integer(int), parameter :: WS_CHILD = Z'40000000'
  integer(int), parameter :: WS_CHILD_VISIBLE = WS_CHILD + WS_VISIBLE
  type(ptr) :: hPanel
  integer(int) :: panelWidth
  panelWidth = 800 / 10  ! 10% от ширины окна, если оно фиксировано
  
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  cursorPathW = to_wide_null_terminated("cross.ico")
  iconPathW = to_wide_null_terminated("favicon.ico")

  darkBrushColor = Z'00321E0A'  ! B=0A, G=1E, R=32
  hBrush = CreateSolidBrush(darkBrushColor)  

  classNameW = to_wide_null_terminated("My window class")
  windowTitleW = to_wide_null_terminated("Fortran Window")

  hInstance = nullptr
  
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
  wcx%hIcon = LoadImageW(nullptr, c_loc(iconPathW(1)), IMAGE_ICON, 0, 0, LR_LOADFROMFILE)
  wcx%hCursor = LoadImageW(nullptr, c_loc(cursorPathW(1)), IMAGE_ICON, 0, 0, LR_LOADFROMFILE)
  !wcx%hbrBackground = GetSysColorBrush(COLOR_BTNFACE)
  wcx%hbrBackground = hBrush
  wcx%lpszMenuName = nullptr
  wcx%lpszClassName = c_loc(classNameW(1))
  wcx%hIconSm = wcx%hIcon

  regResult = RegisterClassExW(c_loc(wcx))
  print *, "[DEBUG] className ptr: ", transfer(c_loc(classNameW(1)), 0_c_intptr_t)
  print *, "[DEBUG] windowTitle ptr: ", transfer(c_loc(windowTitleW(1)), 0_c_intptr_t)
  
    !! 1. Создаём главное окно
    hwnd = CreateWindowExW(0, c_loc(classNameW(1)), c_loc(windowTitleW(1)), &
                           WS_OVERLAPPEDWINDOW, 100, 100, 800, 600, nullptr, nullptr, hInstance, nullptr)

    call ShowWindow(hwnd, SW_SHOW)
    call UpdateWindow(hwnd)

    ! 2. Теперь создаём панель
    hPanel = CreateWindowExW(0, c_loc(classNameW(1)), nullptr, &
             WS_CHILD_VISIBLE, 0, 0, panelWidth, 600, hwnd, nullptr, hInstance, nullptr)

    call ShowWindow(hPanel, SW_SHOW)
    call UpdateWindow(hPanel)


  do while (GetMessageW(c_loc(msg_inst), nullptr, 0, 0) > 0)
    call TranslateMessage(c_loc(msg_inst))
    call DispatchMessageW(c_loc(msg_inst))
  end do
  
end program
