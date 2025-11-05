! WinAPI types and constants
module win_types
  use iso_c_binding, only: int32 => c_int32_t, i_ptr => c_intptr_t, ptr => c_ptr, f_ptr => c_funptr, &
      nullptr => c_null_ptr, char => c_char, char0 => c_null_char, bool => c_bool, long => c_long, &
      double => c_double, int8 => c_int8_t                                         
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
  integer(int32), parameter :: WM_ERASEBKGND = 20   ! 0x0014
  integer(int32), parameter :: WM_CREATE           = 1
  integer(int32), parameter :: WM_DESTROY          = 2
  integer(int32), parameter :: WM_SIZE             = 5
  integer(int32), parameter :: WM_COMMAND          = 273          ! 0x0111
  integer(int32), parameter :: WM_PAINT            = 15
  integer(int32), parameter :: WM_TIMER            = 275 !Z'0113'  ! Сообщение таймера
  integer(int32), parameter :: TIMER_ID            = 1        ! ID таймера
  integer(int32), parameter :: GWLP_USERDATA = -21  ! for SetWindowLongPtrW
  integer(int32), parameter :: PS_SOLID   = 0
  integer(int32), parameter :: SRCCOPY    = int(Z'00CC0020', int32)
  
  ! Button styles
  integer(int32), parameter :: BS_PUSHBUTTON       = 0
  integer(int32), parameter :: BS_DEFPUSHBUTTON    = 1

  ! Control element identifiers
  integer(i_ptr), parameter :: ID_BUTTON1        = 1001
  integer(i_ptr), parameter :: ID_BUTTON2        = 1002

  ! Window class structure
    type, bind(C) :: WNDCLASSEX
      integer(int32) :: cbSize
      integer(int32) :: style
      type(f_ptr)     :: lpfnWndProc
      integer(int32) :: cbClsExtra
      integer(int32) :: cbWndExtra
      type(ptr)        :: hInstance
      type(ptr)        :: hIcon
      type(ptr)        :: hCursor
      type(ptr)        :: hbrBackground   ! HBRUSH
      type(ptr)        :: lpszMenuName    ! LPCWSTR
      type(ptr)        :: lpszClassName   ! LPCWSTR
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

   
  !type, bind(C) :: RECT
  !  integer(long) :: left
  !  integer(long) :: top
  !  integer(long) :: right
  !  integer(long) :: bottom
  !end type RECT
  !
  !type, bind(C) :: PAINTSTRUCT
  !  type(ptr)            :: hdc            ! HDC
  !  integer(int32)       :: fErase         ! BOOL
  !  type(RECT)           :: rcPaint
  !  integer(int32)       :: fRestore
  !  integer(int32)       :: fIncUpdate
  !  character(1)         :: rgbReserved(32)
  !end type PAINTSTRUCT
type, bind(C) :: RECT
  integer(int32) :: left, top, right, bottom
end type

type, bind(C) :: PAINTSTRUCT
  type(ptr)        :: hdc             ! HDC
  integer(int32) :: fErase          ! BOOL
  type(RECT)         :: rcPaint
  integer(int32) :: fRestore        ! BOOL
  integer(int32) :: fIncUpdate      ! BOOL
  integer(int8)  :: rgbReserved(32) ! BYTE[32]
end type

type :: ColorRef
  integer(int32) :: A  = 0
  integer(int32) :: R  = 0  
  integer(int32) :: G  = 0
  integer(int32) :: B  = 0
end type ColorRef
! geometry only
type :: Clock
  real(double) :: cx, cy      ! center
  real(double) :: rx, ry      ! radii (circle: rx=ry)
  real(double) :: theta       ! fast angle (radians)
  real(double) :: theta2      ! slow angle (radians)
end type Clock

type :: AppState
  integer(int32) :: x = 10
  integer(int32) :: y = 10
  integer(int32) :: dx = 2
  integer(int32) :: dy = 2
  integer(int32) :: w  = 6
  integer(int32) :: h  = 6

  ! circular/elliptic motion params (legacy single-dot, можно оставить)
  real(double) :: cx = 0.0d0
  real(double) :: cy = 0.0d0
  real(double) :: rx = 50.0d0
  real(double) :: ry = 50.0d0
  real(double) :: theta  = 0.0d0
  real(double) :: theta2 = 0.0d0
  real(double) :: omega  = 1.0d0

  type(ptr)    :: hbg_brush = nullptr

  integer(int32) :: nx = 10
  integer(int32) :: ny = 10
  type(Clock), pointer :: clocks(:) => null()          ! OK без bind(C)
  real(double),  pointer :: omega_fast(:) => null()
  real(double),  pointer :: omega_slow(:) => null()
  ! Backbuffer
  type(ptr)    :: hMemDC    = nullptr   ! memory HDC
  type(ptr)    :: hBmp      = nullptr   ! compatible bitmap
  type(ptr)    :: hBmpOld   = nullptr   ! old selected bitmap
  integer(int32) :: backW   = 0
  integer(int32) :: backH   = 0
  type(ColorRef), pointer   :: color_ref(:) => null()
  real(double), pointer :: hue(:) => null()   ! размер N
end type AppState
end module win_types