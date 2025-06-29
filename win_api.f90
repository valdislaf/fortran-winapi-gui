! Интерфейсы к функциям WinAPI
module win_api
  interface
    function RegisterClassExW(lpWndClass) bind(C, name="RegisterClassExW")
      use standard
      type(ptr), value :: lpWndClass
      integer(int32) :: RegisterClassExW
    end function

    function GetLastError() bind(C, name="GetLastError")
      use standard
      integer(int32) :: GetLastError
    end function

    function CreateWindowExW(dwExStyle, lpClassName, lpWindowName, dwStyle, x, y, nWidth, nHeight, &
                             hWndParent, hMenu, hInstance, lpParam) bind(C, name="CreateWindowExW")
      use standard
      integer(int32), value :: dwExStyle, dwStyle, x, y, nWidth, nHeight
      type(ptr), value :: lpClassName, lpWindowName, hWndParent, hMenu, hInstance, lpParam
      type(ptr) :: CreateWindowExW
    end function

    subroutine ShowWindow(hWnd, nCmdShow) bind(C, name="ShowWindow")
      use standard
      type(ptr), value :: hWnd
      integer(int32), value :: nCmdShow
    end subroutine

    subroutine UpdateWindow(hWnd) bind(C, name="UpdateWindow")
      use standard
      type(ptr), value :: hWnd
    end subroutine

    function GetMessageW(lpMsg, hWnd, wMsgFilterMin, wMsgFilterMax) bind(C, name="GetMessageW")
      use standard
      type(ptr), value :: lpMsg, hWnd
      integer(int32), value :: wMsgFilterMin, wMsgFilterMax
      integer(int32) :: GetMessageW
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
      integer(int32), value :: nIndex
      type(ptr) :: GetSysColorBrush
    end function

    function CreateSolidBrush(color) bind(C, name="CreateSolidBrush")
      use standard
      integer(int32), value :: color
      type(ptr) :: CreateSolidBrush
    end function

    function LoadImageW(hInst, lpszName, uType, cxDesired, cyDesired, fuLoad) bind(C, name="LoadImageW")
      use standard
      type(ptr), value :: hInst, lpszName
      integer(int32), value :: uType, cxDesired, cyDesired, fuLoad
      type(ptr) :: LoadImageW
    end function

    function DefWindowProcW(hWnd, Msg, wParam, lParam) bind(C, name="DefWindowProcW")
      use standard
      type(ptr), value :: hWnd
      integer(int32), value :: Msg
      integer(i_ptr), value :: wParam, lParam
      integer(i_ptr) :: DefWindowProcW
    end function

    subroutine PostQuitMessage(nExitCode) bind(C, name="PostQuitMessage")
      use standard
      integer(int32), value :: nExitCode
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
      integer(int32), value :: nIndex
      integer(i_ptr) :: GetWindowLongPtrW
    end function

    subroutine SetWindowLongPtrW(hWnd, nIndex, dwNewLong) bind(C, name="SetWindowLongPtrW")
      use standard
      type(ptr), value :: hWnd
      integer(int32), value :: nIndex
      integer(i_ptr), value :: dwNewLong
    end subroutine
    
    function GetWindowRect(hWnd, lpRect) bind(C, name="GetWindowRect")
      use standard
      type(ptr), value :: hWnd
      type(ptr) :: lpRect
      logical(c_bool) :: GetWindowRect
    end function
    
    function GetClientRect(hWnd, lpRect) bind(C, name="GetClientRect")
      use standard
      type(ptr), value :: hWnd
      type(ptr) :: lpRect
      logical(bool) :: GetClientRect
    end function
    
    function InvalidateRect(hWnd, lpRect, bErase) bind(C, name="InvalidateRect")
      use standard
      type(ptr), value :: hWnd        ! HWND
      type(ptr), value :: lpRect      ! LPRECT, может быть c_null_ptr
      integer(int32), value :: bErase ! BOOL (1 или 0)
      integer(int32) :: InvalidateRect ! BOOL (0 — ошибка, не 0 — успех)
    end function
    
    function LoadCursorW(hInstance, lpCursorName) bind(C, name="LoadCursorW")
      use standard
      type(ptr), value :: hInstance
      type(ptr), value :: lpCursorName
      type(ptr) :: LoadCursorW
    end function

     
  end interface
contains
    ! Обработчик сообщений окна (WndProc)
    function WndProc(hWnd, Msg, wParam, lParam) bind(C) result(res)      
      use standard      
      type(AppData), pointer :: appDataInst
      type(ptr), value      :: hWnd
      integer(int32), value   :: Msg
      integer(i_ptr), value :: wParam, lParam
      integer(i_ptr)        :: res 
      integer(int32)          :: width, height, lp32, panelActualWidth
      type(c_ptr) :: appDataPtr    
      integer(i_ptr) :: userData
      type(RECT), target :: rc
      logical(bool) :: ok
      integer(i_ptr) :: newLParam
      
      integer(int32) :: resultSendMessageW
      
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
          lp32 = transfer(lParam, int32)
          width  = iand(lp32, 65535)              ! ширина окна = младшие 16 бит
          height = iand(ishft(lp32, -16), 65535)  ! высота окна = старшие 16 бит
          print *, "New size: ", width, "x", height

          panelActualWidth = max(80, width / 10)
          userData = GetWindowLongPtrW(hWnd, -21)
          appDataPtr = transfer(userData, appDataPtr)
          call c_f_pointer(appDataPtr, appDataInst)
          
          print *, panelActualWidth, width - panelActualWidth, height
          resultInvalidate = InvalidateRect(hwnd, c_null_ptr, 1)
          call MoveWindow(appDataInst%hwin, panelActualWidth, 0, &
                          width - panelActualWidth , height, .true._c_bool) 
                    
          call MoveWindow(appDataInst%hPanel, 0, 0, panelActualWidth, height, .true._c_bool)  !<== ПЕРЕМЕСТИЛ СТРОКУ СЮДА
          call UpdateWindow(appDataInst%hPanel)
          call UpdateWindow(appDataInst%hwin)
      
          res = 0  ! ← обязательно

      case default
        ! Все остальные сообщения — стандартная обработка
        res = DefWindowProcW(hWnd, Msg, wParam, lParam)
      end select
    end function WndProc
    
    function GraphWndProc(hwnd, uMsg, wParam, lParam) bind(C, name="GraphWndProc") result(retval)
       use standard
      ! Аргументы, как требует WinAPI
      type(ptr), value :: hwnd
      integer(int32), value :: uMsg
      integer(i_ptr), value :: wParam, lParam
      integer(i_ptr) :: retval
      integer(int32)          :: width, height, lp32, panelActualWidth
      integer(int32) :: resultInvalidate
      ! Константы сообщений     
      integer(i_ptr), parameter :: TRUE = 1, FALSE = 0
      retval = 0
      resultInvalidate = 0
      !print *, "GraphWndProc called: uMsg =", uMsg

      select case (uMsg)
      case (WM_SIZE)
        resultInvalidate = InvalidateRect(hwnd, c_null_ptr, 1)
        print *, "WM_SIZE GraphWndProc, result: ", resultInvalidate
        print *, "GraphWndProc got WM_SIZE"
        retval = 0
          
      case (WM_PAINT)
        !print *, "Graph window WM_PAINT"
        retval = 0
      case default
        ! Вызов стандартного обработчика, если сообщение не обработано
        retval = DefWindowProcW(hwnd, uMsg, wParam, lParam)
      end select
    end function GraphWndProc

end module win_api