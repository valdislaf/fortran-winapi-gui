! Типы и константы WinAPI
module win_types
  use iso_c_binding, only: int32 => c_int32_t, i_ptr => c_intptr_t, ptr => c_ptr, f_ptr => c_funptr, &
      nullptr => c_null_ptr, char => c_char, char0 => c_null_char, bool => c_bool, long => c_long
  implicit none
  
  ! Константы для окон и сообщений
  integer(int32), parameter :: WS_OVERLAPPEDWINDOW = 13565952     ! 0x00CF0000
  integer(int32), parameter :: WS_VISIBLE          = 268435456    ! 0x10000000
  integer(int32), parameter :: WS_CHILD            = 1073741824   ! 0x40000000
  integer(int32), parameter :: WS_CHILD_VISIBLE    = WS_CHILD + WS_VISIBLE
  integer(int32), parameter :: SW_SHOW             = 5
  integer(int32), parameter :: IMAGE_ICON          = 1
  integer(int32), parameter :: LR_LOADFROMFILE     = 16

  ! Сообщения Windows
  integer(int32), parameter :: WM_DESTROY          = 2
  integer(int32), parameter :: WM_SIZE             = 5
  integer(int32), parameter :: WM_COMMAND          = 273          ! 0x0111
  integer(int32), parameter :: WM_PAINT = 15
  
  ! Стиль кнопок
  integer(int32), parameter :: BS_PUSHBUTTON       = 0
  integer(int32), parameter :: BS_DEFPUSHBUTTON    = 1

  ! Идентификаторы управляющих элементов
  integer(i_ptr), parameter :: ID_BUTTON1        = 1001
  integer(i_ptr), parameter :: ID_BUTTON2        = 1002

  ! Структура класса окна
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

  ! Структура сообщения
  type, bind(C) :: MSG_T
    type(ptr)         :: hwnd
    integer(int32)      :: message
    integer(i_ptr)    :: wParam
    integer(i_ptr)    :: lParam
    integer(int32)      :: time
    type(ptr)         :: pt
  end type

  !Создаем структуру для передачи hPanel
  type, bind(C) :: AppData
    type(ptr) :: hPanel
    type(ptr) :: hwin 
  end type
  
  type, bind(C) :: RECT
      integer(long) :: left, top, right, bottom
  end type


end module win_types