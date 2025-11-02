! WinAPI types and constants
module win_types
  use iso_c_binding, only: int32 => c_int32_t, i_ptr => c_intptr_t, ptr => c_ptr, f_ptr => c_funptr, &
      nullptr => c_null_ptr, char => c_char, char0 => c_null_char, bool => c_bool, long => c_long
  implicit none
 
 ! Constants for windows and messages
  integer(int32), parameter :: WS_OVERLAPPEDWINDOW = 13565952     ! 0x00CF0000
  integer(int32), parameter :: WS_VISIBLE          = 268435456    ! 0x10000000
  integer(int32), parameter :: WS_CHILD            = 1073741824   ! 0x40000000
  integer(int32), parameter :: WS_CHILD_VISIBLE    = WS_CHILD + WS_VISIBLE
  integer(int32), parameter :: SW_SHOW             = 5
  integer(int32), parameter :: IMAGE_ICON          = 1
  integer(int32), parameter :: LR_LOADFROMFILE     = 16
  integer(int32), parameter :: ID_ARROW = 32512 ! Cursor identifier (arrow)
  
  ! Windows messages
  integer(int32), parameter :: WM_CREATE           = 1
  integer(int32), parameter :: WM_DESTROY          = 2
  integer(int32), parameter :: WM_SIZE             = 5
  integer(int32), parameter :: WM_COMMAND          = 273          ! 0x0111
  integer(int32), parameter :: WM_PAINT            = 15
  integer(int32), parameter :: WM_TIMER            = 275 !Z'0113'  ! Сообщение таймера
  integer(int32), parameter :: TIMER_ID            = 1        ! ID таймера
  integer(int32), parameter :: GWLP_USERDATA = -21  ! for SetWindowLongPtrW

  ! Button styles
  integer(int32), parameter :: BS_PUSHBUTTON       = 0
  integer(int32), parameter :: BS_DEFPUSHBUTTON    = 1

  ! Control element identifiers
  integer(i_ptr), parameter :: ID_BUTTON1        = 1001
  integer(i_ptr), parameter :: ID_BUTTON2        = 1002

  ! Window class structure
  type, bind(C) :: WNDCLASSEX
    integer(int32)     :: cbSize
    integer(int32)     :: style
    type(f_ptr)      :: lpfnWndProc
    integer(int32)     :: cbClsExtra
    integer(int32)     :: cbWndExtra
    type(ptr)        :: hInstance
    type(ptr)        :: hIcon
    type(ptr)        :: hCursor
    type(ptr)        :: hbrBackground
    type(ptr)        :: lpszMenuName
    type(ptr)        :: lpszClassName
    type(ptr)        :: hIconSm
  end type

  ! Message structure
  type, bind(C) :: MSG_T
    type(ptr)         :: hwnd
    integer(int32)      :: message
    integer(i_ptr)    :: wParam
    integer(i_ptr)    :: lParam
    integer(int32)      :: time
    type(ptr)         :: pt
  end type

  ! Create a structure to pass hPanel
  type, bind(C) :: AppData
    type(ptr) :: hPanel
    type(ptr) :: hwin 
  end type
  
  type, bind(C) :: RECT
    integer(long) :: left
    integer(long) :: top
    integer(long) :: right
    integer(long) :: bottom
  end type RECT

  type, bind(C) :: PAINTSTRUCT
    type(ptr)            :: hdc            ! HDC
    integer(int32)       :: fErase         ! BOOL
    type(RECT)           :: rcPaint
    integer(int32)       :: fRestore
    integer(int32)       :: fIncUpdate
    character(1)         :: rgbReserved(32)
  end type PAINTSTRUCT
  
  type :: GraphData
    type(ptr) :: hbrush
  end type GraphData
  
  type, bind(C) :: AppState
    integer(int32) :: x = 10    ! current X in client coords
    integer(int32) :: y = 10    ! current Y
    integer(int32) :: dx = 2    ! step X per timer tick
    integer(int32) :: dy = 2    ! step Y per timer tick
    integer(int32) :: w  = 2    ! dot width  (>=1)
    integer(int32) :: h  = 2    ! dot height (>=1)
  end type AppState

end module win_types