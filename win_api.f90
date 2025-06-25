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
    
    function GetWindowLongPtrW(hWnd, nIndex) bind(C, name="GetWindowLongPtrW")
      use standard
      type(ptr), value :: hWnd
      integer(int), value :: nIndex
      integer(i_ptr) :: GetWindowLongPtrW
    end function

    subroutine SetWindowLongPtrW(hWnd, nIndex, dwNewLong) bind(C, name="SetWindowLongPtrW")
      use standard
      type(ptr), value :: hWnd
      integer(int), value :: nIndex
      integer(i_ptr), value :: dwNewLong
    end subroutine

  end interface
contains
    ! Обработчик сообщений окна (WndProc)
    function WndProc(hWnd, Msg, wParam, lParam) bind(C) result(res)      
      use standard
      implicit none
      type(AppData), pointer :: appDataInst
      type(ptr), value      :: hWnd
      integer(int), value   :: Msg
      integer(i_ptr), value :: wParam, lParam
      integer(i_ptr)        :: res 
      integer(int)          :: width, height, lp32, panelActualWidth
      type(c_ptr) :: appDataPtr    
      integer(i_ptr) :: userData
      
      select case (Msg)
      case (1)  ! WM_CREATE
        appDataPtr = transfer(lParam, c_null_ptr)
        call SetWindowLongPtrW(hWnd, -21, transfer(appDataPtr, 0_i_ptr))
        res = 0
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
        userData = GetWindowLongPtrW(hWnd, -21)
        appDataPtr = transfer(userData, appDataPtr)
        call c_f_pointer(appDataPtr, appDataInst)

        call MoveWindow(appDataInst%hPanel, 0, 0, panelActualWidth, height, .true._c_bool)
        call UpdateWindow(appDataInst%hPanel)

      case default
        ! Все остальные сообщения — стандартная обработка
        res = DefWindowProcW(hWnd, Msg, wParam, lParam)
      end select
    end function WndProc
    
end module win_api