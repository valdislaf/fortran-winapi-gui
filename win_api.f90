! Interfaces to WinAPI functions
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

    subroutine MoveWindow(hWnd, X, Y, nW, nH, bRepaint) bind(C, name="MoveWindow")
      use standard
      type(ptr), value        :: hWnd
      integer(int32), value :: X, Y, nW, nH
      integer(int32), value :: bRepaint
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
      logical(bool) :: GetWindowRect
    end function
    
    function GetClientRect(hWnd, lpRect) bind(C, name="GetClientRect")
      use iso_c_binding, only: c_ptr, c_int32_t
      type(c_ptr), value :: hWnd     ! HWND
      type(c_ptr), value :: lpRect   ! LPRECT
      integer(c_int32_t) :: GetClientRect
    end function

    
    function InvalidateRect(hWnd, lpRect, bErase) bind(C, name="InvalidateRect")
      use standard
      type(ptr), value :: hWnd        ! HWND
      type(ptr), value :: lpRect      ! LPRECT, can be c_null_ptr
      integer(int32), value :: bErase ! BOOL (1 or 0)
      integer(int32) :: InvalidateRect ! BOOL (0 — error, not 0 — success)
    end function
    
    function LoadCursorW(hInstance, lpCursorName) bind(C, name="LoadCursorW")
      use standard
      type(ptr), value :: hInstance
      type(ptr), value :: lpCursorName
      type(ptr) :: LoadCursorW
    end function    
   
    function BeginPaint(hWnd, lpPaint) bind(C, name="BeginPaint")
      use standard
      type(ptr), value        :: hWnd
      type(ptr), value        :: lpPaint
      type(ptr)               :: BeginPaint  ! возвращает HDC
    end function 
    ! Диагностика: сколько GDI/USER-объектов у процесса
     function GetGuiResources(hProcess, uiFlags) bind(C,name="GetGuiResources")
       use standard
       type(ptr), value :: hProcess
       integer(int32), value :: uiFlags
       integer(int32) :: GetGuiResources
     end function

     ! Текущий процесс (для GetGuiResources)
     function GetCurrentProcess() bind(C,name="GetCurrentProcess")
       use standard
       type(ptr) :: GetCurrentProcess
     end function
      function EndPaint(hWnd, lpPaint) bind(C, name="EndPaint")
        use standard
        type(ptr), value :: hWnd
        type(ptr), value :: lpPaint
        integer(int32)      :: EndPaint    ! возвращает BOOL
      end function

      function FillRect(hdc, lprc, hbr) bind(C, name="FillRect")
        use standard
        type(ptr), value :: hdc     ! HDC
        type(ptr), value :: lprc    ! const RECT*
        type(ptr), value :: hbr     ! HBRUSH
        integer(int32)     :: FillRect
      end function

      function DeleteObject(hObject) bind(C, name="DeleteObject")
        use standard
        type(ptr), value :: hObject
        integer(int32) :: DeleteObject
      end function   
      
      function SendMessageW(hWnd, Msg, wParam, lParam) bind(C, name="SendMessageW")
        use standard
        type(ptr), value :: hWnd
        integer(int32), value :: Msg
        integer(i_ptr), value :: wParam
        integer(i_ptr), value :: lParam
        integer(i_ptr) :: SendMessageW
      end function SendMessageW
      
      function Rectangle(hdc, left, top, right, bottom) bind(C, name="Rectangle")
        use standard
        type(ptr), value :: hdc
        integer(int32), value :: left, top, right, bottom
        integer(int32) :: Rectangle
      end function
      
    ! Таймеры WinAPI
    function SetTimer(hWnd, nIDEvent, uElapse, lpTimerFunc) bind(C, name="SetTimer")
      use standard
      type(ptr), value :: hWnd
      integer(int32), value :: nIDEvent
      integer(int32), value :: uElapse
      type(ptr), value :: lpTimerFunc
      integer(int32) :: SetTimer
    end function

    function KillTimer(hWnd, nIDEvent) bind(C, name="KillTimer")
      use standard
      type(ptr), value :: hWnd
      integer(int32), value :: nIDEvent
      integer(int32) :: KillTimer
    end function
    
  end interface
  
contains

    ! Window message handler (WndProc)
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
      integer(int32) :: resultbool
      integer(i_ptr) :: resultInvalidate
      !logical(c_bool) :: settrue
      
      select case (Msg)
      case (WM_CREATE) 
        ! Установить таймер на 1000 мс (1 секунда)
        resultbool = SetTimer(hWnd, TIMER_ID, 1000, nullptr)
        appDataPtr = transfer(lParam, c_null_ptr)
        call SetWindowLongPtrW(hWnd, -21, transfer(appDataPtr, 0_i_ptr))
        res = 0
      case (WM_DESTROY)
        ! Удалить таймер
        resultbool =  KillTimer(hWnd, TIMER_ID)
        call PostQuitMessage(0) 
        res = 0
      case (WM_TIMER)
        ! Обработка таймера: wParam содержит ID таймера
        if (wParam == TIMER_ID) then
          print *, "Timer tick!"
          
          gdiCnt = GetGuiResources(GetCurrentProcess(), 0)  ! GDI
          print *,gdiCnt
          ! Здесь можно добавить нужные действия по таймеру
        end if
        res = 0
      case (WM_SIZE) 
          ! Window resize message
          lp32 = transfer(lParam, int32)
          width  = iand(lp32, 65535)              ! window width = lower 16 bits
          height = iand(ishft(lp32, -16), 65535)  ! window height = upper 16 bits
          !settrue = .true._c_bool

          panelActualWidth = max(80, width / 10)
          userData = GetWindowLongPtrW(hWnd, -21)
          appDataPtr = transfer(userData, appDataPtr)
          call c_f_pointer(appDataPtr, appDataInst)
          
          resultInvalidate = InvalidateRect(hwnd, nullptr, 1)
          resultInvalidate = InvalidateRect(appDataInst%hwin, nullptr, 1)
          resultInvalidate = InvalidateRect(appDataInst%hPanel, nullptr, 1)
          call MoveWindow(appDataInst%hwin, panelActualWidth, 0, &
                         width - panelActualWidth , height, 1)
          call MoveWindow(appDataInst%hPanel, 0, 0, &
                         panelActualWidth , height, 1)
          
          
          res = 0  ! ← required

      case default
        ! All other messages — default processing
        res = DefWindowProcW(hWnd, Msg, wParam, lParam)
      end select
    end function WndProc
   
    function GraphWndProc(hwnd, uMsg, wParam, lParam) bind(C, name="GraphWndProc") result(retval)
      use standard
      type(ptr), value :: hwnd
      type(PAINTSTRUCT), target :: ps
      type(RECT), target :: rc
      !type(RECT), target :: rcc
      type(ptr) :: hdc
      integer(int32), value :: uMsg
      integer(i_ptr), value :: wParam, lParam
      integer(i_ptr) :: retval
      integer(int32) :: resultbool
      integer(i_ptr) :: resultInvalidate
      integer(i_ptr) :: userData
      type(GraphData), pointer :: pgraphData
      type(ptr) :: pgraphDataPtr
      type(RECT), target :: rcSmall
      type(ptr) :: hRedBrush
      integer(int32) :: gdiCnt
      !integer(c_long) :: style
      !!!!!!!!!!!!!print *, "GraphWndProc called! hwnd=", transfer(hwnd, 0_i_ptr), " uMsg=", uMsg
      retval = 0
      resultInvalidate = 0
      
      select case (uMsg)     
      case (WM_CREATE) 
        allocate(pgraphData)
        pgraphData%hbrush = CreateSolidBrush(MakeARGB(0, 102, 0, 51))  ! Фиолетовая кисть
        !pgraphData%hbrush = CreateSolidBrush(int(Z'000000FF', int32))  ! R=255
        pgraphDataPtr = c_loc(pgraphData)
        call SetWindowLongPtrW(hwnd, 0, transfer(pgraphDataPtr, 0_i_ptr))
        retval = 0

      case (WM_DESTROY)
          retval = 0

      case (WM_SIZE)
          retval = 0

      case (WM_PAINT)
          !resultInvalidate = InvalidateRect(hwnd, c_null_ptr, 1)
          !res = GetClientRect(hwnd, c_loc(rcc))
        
          hdc = BeginPaint(hwnd, c_loc(ps))
          !resultbool =  Rectangle(hdc, 0, 0, 100, 100)

          resultbool = GetClientRect(hwnd, c_loc(rc))
          userData = GetWindowLongPtrW(hwnd, 0)
          if (userData /= 0) then
            pgraphDataPtr = transfer(userData, nullptr)
            call c_f_pointer(pgraphDataPtr, pgraphData)
            if (associated(pgraphData)) then
              resultbool = FillRect(hdc, c_loc(rc), pgraphData%hbrush)
            end if
          end if
          
          
          !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
          ! --- 2) Подготовим «маленький» RECT 2×2 пикселя ---
          
          rcSmall%left   = rc%left + 10            ! сместим +10px по X
          rcSmall%top    = rc%top  + 10            ! и +10px по Y
          rcSmall%right  = rcSmall%left + 2
          rcSmall%bottom = rcSmall%top  + 2

          ! --- 3) Создадим кисть красного цвета ---
          ! MakeARGB(0, R, G, B) — R=255,G=0,B=0 → чистый красный
          
          hRedBrush = CreateSolidBrush(MakeARGB(0, 0, 0, 255))

          ! --- 4) Закрасим маленький квадрат красной кистью ---
          resultbool = FillRect(hdc, c_loc(rcSmall), hRedBrush)

          ! --- 5) Удалим временную кисть, чтобы не было утечки GDI-объектов ---
          resultbool = DeleteObject(hRedBrush)
          !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
          
          
          resultbool = EndPaint(hwnd, c_loc(ps))
          gdiCnt = GetGuiResources(GetCurrentProcess(), 0)  ! GDI
          print *,gdiCnt
          retval = 0

      case default
        retval = DefWindowProcW(hwnd, uMsg, wParam, lParam)
      end select
    end function GraphWndProc


end module win_api