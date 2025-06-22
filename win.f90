! Глобальные переменные
module globals
  use iso_c_binding, only: c_ptr
  implicit none
  type(c_ptr) :: hPanel  ! Дескриптор панели (дочернего окна)
end module globals

! Типы и константы WinAPI
module win_types
  use iso_c_binding, only: int => c_int32_t, i_ptr => c_intptr_t, ptr => c_ptr, f_ptr => c_funptr, &
      nullptr => c_null_ptr, char => c_char
  implicit none

  ! Константы для окон и сообщений
  integer(int), parameter :: WS_OVERLAPPEDWINDOW = 13565952     ! 0x00CF0000
  integer(int), parameter :: WS_VISIBLE          = 268435456    ! 0x10000000
  integer(int), parameter :: WS_CHILD            = 1073741824   ! 0x40000000
  integer(int), parameter :: WS_CHILD_VISIBLE    = WS_CHILD + WS_VISIBLE
  integer(int), parameter :: SW_SHOW             = 5

  ! Сообщения Windows
  integer(int), parameter :: WM_DESTROY          = 2
  integer(int), parameter :: WM_SIZE             = 5
  integer(int), parameter :: WM_COMMAND          = 273          ! 0x0111

  ! Стиль кнопок
  integer(int), parameter :: BS_PUSHBUTTON       = 0
  integer(int), parameter :: BS_DEFPUSHBUTTON    = 1

  ! Идентификаторы управляющих элементов
  integer(int), parameter :: ID_BUTTON1          = 1001

  ! Структура класса окна
  type, bind(C) :: WNDCLASSEX
    integer(int)     :: cbSize
    integer(int)     :: style
    type(f_ptr)      :: lpfnWndProc
    integer(int)     :: cbClsExtra
    integer(int)     :: cbWndExtra
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
    integer(int)      :: message
    integer(i_ptr)    :: wParam
    integer(i_ptr)    :: lParam
    integer(int)      :: time
    type(ptr)         :: pt
  end type

end module win_types


! Преобразование строк в UTF-16 (для WinAPI)
module string_utils
  use win_types
  implicit none

contains

  ! Преобразует строку в массив символов UTF-16 с завершающим нулём
 function to_wide_null_terminated(text) result(wide)
 
  use iso_c_binding, only: c_char, c_null_char
      implicit none
      character(len=*), intent(in) :: text
      character(kind=c_char), allocatable, target :: wide(:)
      integer :: i, k, n

      n = len_trim(text)
      allocate(wide(2 * n + 2))  ! на 1 WCHAR больше (== 2 char) для завершающего \0

      k = 1
      do i = 1, n
        wide(k) = text(i:i)
        k = k + 1
        wide(k) = c_null_char
        k = k + 1
      end do

      ! Завершающий WCHAR (\0\0)
      wide(k) = c_null_char
      wide(k + 1) = c_null_char
    end function

end module string_utils

! Общие типы и утилиты
module standard
  use iso_c_binding
  use win_types
  use string_utils
end module standard

! Интерфейсы к функциям WinAPI
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

    subroutine MoveWindow(hWnd, X, Y, nWidth, nHeight, bRepaint) bind(C, name="MoveWindow")
      use iso_c_binding
      type(c_ptr), value :: hWnd
      integer(c_int32_t), value :: X, Y, nWidth, nHeight
      logical(c_bool), value :: bRepaint
    end subroutine
    
  end interface
contains
    ! Обработчик сообщений окна (WndProc)
    function WndProc(hWnd, Msg, wParam, lParam) bind(C) result(res)
      use standard
      use globals
      implicit none
      type(ptr), value      :: hWnd
      integer(int), value   :: Msg
      integer(i_ptr), value :: wParam, lParam
      integer(i_ptr)        :: res 
      integer(int)          :: width, height, lp32, panelActualWidth

      select case (Msg)
      case (WM_DESTROY)
        ! Сообщение о закрытии окна — завершить цикл сообщений
        call PostQuitMessage(0)
        res = 0
      case (WM_SIZE)
        ! Сообщение об изменении размера окна
        lp32 = transfer(lParam, 0_int)
        width  = iand(lp32, 65535)              ! ширина окна = младшие 16 бит
        height = iand(ishft(lp32, -16), 65535)  ! высота окна = старшие 16 бит
        print *, "New size: ", width, "x", height

        ! Вычисляем ширину панели: минимум 80 пикселей или 1/10 ширины окна
        panelActualWidth = max(80, width / 10)

        ! Изменяем размер панели вместе с окном
        call MoveWindow(hPanel, 0, 0, panelActualWidth, height, .true._c_bool)
        call UpdateWindow(hPanel)
      case default
        ! Все остальные сообщения — стандартная обработка
        res = DefWindowProcW(hWnd, Msg, wParam, lParam)
      end select
    end function WndProc
    
    function MakeARGB(A, R, G, B) result(color)
      use iso_c_binding
      implicit none
      integer(c_int32_t), intent(in) :: A, R, G, B
      integer(c_int32_t) :: color
      color = IOR(ISHFT(A, 24), IOR(ISHFT(R, 16), IOR(ISHFT(G, 8), B)))
    end function MakeARGB
    
end module win_api

! Главная программа
program WinMain
  use win_api
  use standard
  use globals
  implicit none

  ! Общие параметры
  integer(int) :: regResult
  type(WNDCLASSEX), target :: wcx, wcxPanel
  type(MSG_T), target :: msg_inst
  type(ptr) :: hwnd, hInstance, hBrush, hPanelBrush
  integer(int) :: darkBrushColor
  character(kind=char), allocatable, target :: windowTitleW(:), classNameW(:), panelClassW(:)
  character(kind=char), allocatable, target :: iconPathW(:), cursorPathW(:)
  
  ! Константы для создания окон и ресурсов
  integer(int), parameter :: IMAGE_ICON = 1
  integer(int), parameter :: LR_LOADFROMFILE = 16            ! 0x0010
  integer(int), parameter :: panelWidth = 800 / 10
  
  ! Переменные для кнопки
  type(ptr) :: hButton
  character(kind=char), allocatable, target :: buttonTextW(:)
  type(c_ptr) :: hMenuAsPtr
  integer(i_ptr) :: id_temp
  character(kind=c_char), allocatable, target :: classButtonW(:)
  
  ! Подготовка ресурсов (иконки, курсоры, имена классов)
  !Выделение памяти с нужным размером
  allocate(cursorPathW(0)) ! ← аналог "инициализации значением по умолчанию" как в С++
  cursorPathW    = to_wide_null_terminated("cross.ico")
  iconPathW      = to_wide_null_terminated("favicon.ico")
  classNameW     = to_wide_null_terminated("My window class")
  windowTitleW   = to_wide_null_terminated("Fortran Window")
  panelClassW    = to_wide_null_terminated("PanelClass")
  buttonTextW    = to_wide_null_terminated("Click me")  
  classButtonW   = to_wide_null_terminated("Button")

  id_temp = ID_BUTTON1
  hMenuAsPtr = transfer(id_temp, hMenuAsPtr)
  
  darkBrushColor = MakeARGB(0, 50, 30, 10)                  ! 0x00321E0A
  hBrush         = CreateSolidBrush(darkBrushColor)       ! кисть для фона главного окна
  hPanelBrush    = CreateSolidBrush(MakeARGB(0, 40, 20, 0))             ! кисть для панели 

  hInstance = nullptr  ! В данном примере не используется

  ! Регистрируем класс главного окна
  wcx%cbSize             = c_sizeof(wcx)
  wcx%style              = 0
  wcx%lpfnWndProc        = c_funloc(WndProc)
  wcx%cbClsExtra         = 0
  wcx%cbWndExtra         = 0
  wcx%hInstance          = hInstance
  wcx%hIcon              = LoadImageW(nullptr, c_loc(iconPathW(1)), IMAGE_ICON, 0, 0, LR_LOADFROMFILE)
  wcx%hCursor            = LoadImageW(nullptr, c_loc(cursorPathW(1)), IMAGE_ICON, 0, 0, LR_LOADFROMFILE)
  wcx%hbrBackground      = hBrush
  wcx%lpszMenuName       = nullptr
  wcx%lpszClassName      = c_loc(classNameW(1))
  wcx%hIconSm            = wcx%hIcon
  regResult              = RegisterClassExW(c_loc(wcx))

  ! Регистрируем отдельный класс для панели
  wcxPanel%cbSize        = wcx%cbSize
  wcxPanel%style         = wcx%style
  wcxPanel%lpfnWndProc   = wcx%lpfnWndProc
  wcxPanel%cbClsExtra    = wcx%cbClsExtra
  wcxPanel%cbWndExtra    = wcx%cbWndExtra
  wcxPanel%hInstance     = wcx%hInstance
  wcxPanel%hIcon         = wcx%hIcon
  wcxPanel%hCursor       = wcx%hCursor
  wcxPanel%hbrBackground = wcx%hbrBackground
  wcxPanel%lpszMenuName  = wcx%lpszMenuName
  wcxPanel%lpszClassName = wcx%lpszClassName
  wcxPanel%hIconSm       = wcx%hIconSm

  wcxPanel%hbrBackground = hPanelBrush
  wcxPanel%lpszClassName = c_loc(panelClassW(1))
  regResult              = RegisterClassExW(c_loc(wcxPanel))

  ! Сначала создаём главное окно
  hwnd = CreateWindowExW(0, c_loc(classNameW(1)), c_loc(windowTitleW(1)), &
                         WS_OVERLAPPEDWINDOW, 100, 100, 800, 600, nullptr, nullptr, hInstance, nullptr)

  ! Затем создаём панель как дочернее окно
  hPanel = CreateWindowExW(0, c_loc(panelClassW(1)), nullptr, &
           WS_CHILD_VISIBLE, 0, 0, panelWidth, 600, hwnd, nullptr, hInstance, nullptr)  
  
! Затем создаём кнопку
  hButton = CreateWindowExW(0, c_loc(classButtonW(1)), &
             c_loc(buttonTextW(1)), WS_CHILD_VISIBLE + BS_DEFPUSHBUTTON, &
             2, 2, panelWidth-4, 26, hPanel, hMenuAsPtr, hInstance, nullptr)

  
  if (.not. c_associated(hButton)) then
      print *, "Кнопка не создана! Ошибка:", GetLastError()
  else
      print *, "Кнопка успешно создана!"
  end if

  
  call ShowWindow(hwnd, SW_SHOW)
  call UpdateWindow(hwnd)
  call ShowWindow(hPanel, SW_SHOW)
  call UpdateWindow(hPanel)
  call ShowWindow(hButton, SW_SHOW)
  call UpdateWindow(hButton)
  
  ! Основной цикл обработки сообщений Windows
  do while (GetMessageW(c_loc(msg_inst), nullptr, 0, 0) > 0)
    call TranslateMessage(c_loc(msg_inst))
    call DispatchMessageW(c_loc(msg_inst))
  end do

end program
