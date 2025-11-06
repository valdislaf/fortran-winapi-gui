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
      type(ptr), value :: lpRect     ! LPRECT
      integer(int32) :: GetWindowRect  ! BOOL
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
      type(ptr), value :: hWnd        ! HWND
      type(ptr), value :: lpPaint     ! LPPAINTSTRUCT — указатель передаём по VALUE
      type(ptr)        :: BeginPaint  ! HDC
    end function

    function EndPaint(hWnd, lpPaint) bind(C, name="EndPaint")
        use standard
        type(ptr),   value :: hWnd      ! HWND
        type(ptr),   value :: lpPaint   ! const PAINTSTRUCT*
        integer(int32)   :: EndPaint  ! BOOL
    end function

    function GetClientRect(hWnd, lpRect) bind(C, name="GetClientRect")
        use standard
        type(ptr), value :: hWnd        ! HWND
        type(ptr), value :: lpRect      ! LPRECT
        integer(int32) :: GetClientRect ! BOOL            
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
    
    ! --- GDI helpers for double-buffering and lines ---
    function CreateCompatibleDC(hdc) bind(C, name="CreateCompatibleDC")
      use standard
      type(ptr), value :: hdc
      type(ptr) :: CreateCompatibleDC
    end function

    function CreateCompatibleBitmap(hdc, cx, cy) bind(C, name="CreateCompatibleBitmap")
      use standard
      type(ptr), value :: hdc
      integer(int32), value :: cx, cy
      type(ptr) :: CreateCompatibleBitmap
    end function

    function SelectObject(hdc, hgdiobj) bind(C, name="SelectObject")
      use standard
      type(ptr), value :: hdc, hgdiobj
      type(ptr) :: SelectObject
    end function

    function DeleteDC(hdc) bind(C, name="DeleteDC")
      use standard
      type(ptr), value :: hdc
      integer(int32) :: DeleteDC
    end function

    function BitBlt(hdc, x, y, cx, cy, hdcSrc, x1, y1, rop) bind(C, name="BitBlt")
      use standard
      type(ptr), value :: hdc, hdcSrc
      integer(int32), value :: x, y, cx, cy, x1, y1, rop
      integer(int32) :: BitBlt
    end function

    function MoveToEx(hdc, x, y, lpPoint) bind(C, name="MoveToEx")
      use standard
      type(ptr), value :: hdc, lpPoint
      integer(int32), value :: x, y
      integer(int32) :: MoveToEx
    end function

    function LineTo(hdc, x, y) bind(C, name="LineTo")
      use standard
      type(ptr), value :: hdc
      integer(int32), value :: x, y
      integer(int32) :: LineTo
    end function

    function CreatePen(fnPenStyle, width, color) bind(C, name="CreatePen")
      use standard
      integer(int32), value :: fnPenStyle, width, color
      type(ptr) :: CreatePen
    end function
    
    ! GDI stock-pen + цвет
    function GetStockObject(i) bind(C,name="GetStockObject") result(h)
      use standard
      integer(int32), value :: i
      type(ptr) :: h
    end function
    subroutine SetDCPenColor(hdc, color) bind(C,name="SetDCPenColor")
      use standard
      type(ptr), value :: hdc
      integer(int32), value :: color
    end subroutine

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
    ! --- проверки и вспомогательные
    function IsWindow(hWnd) bind(C, name="IsWindow")
      use standard
      type(ptr), value :: hWnd
      integer(int32) :: IsWindow     ! BOOL
    end function

    function WindowFromDC(hdc) bind(C, name="WindowFromDC")
      use standard
      type(ptr), value :: hdc
      type(ptr) :: WindowFromDC        ! HWND
    end function

    function GetParent(hWnd) bind(C, name="GetParent")
      use standard
      type(ptr), value :: hWnd
      type(ptr) :: GetParent           ! HWND
    end function

    function GetAncestor(hWnd, gaFlags) bind(C, name="GetAncestor")
      use standard
      type(ptr), value :: hWnd
      integer(int32), value :: gaFlags
      type(ptr) :: GetAncestor         ! HWND
    end function

    function GetClassNameW(hWnd, lpClassName, nMaxCount) bind(C, name="GetClassNameW")
      use standard
      type(ptr), value :: hWnd
      type(ptr), value :: lpClassName   ! LPWSTR buffer
      integer(int32), value :: nMaxCount
      integer(int32) :: GetClassNameW ! length
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
      type(ptr) :: appDataPtr    
      integer(i_ptr) :: userData
      integer(int32) :: resultbool
      integer(i_ptr) :: resultInvalidate
      integer(int32) :: gdiCnt, usrCnt
      !logical(c_bool) :: settrue
      
      select case (Msg)
      case (WM_CREATE) 
        gdiCnt = GetGuiResources(GetCurrentProcess(), 0)  ! GDI
        appDataPtr = transfer(lParam, c_null_ptr)
        call SetWindowLongPtrW(hWnd, -21, transfer(appDataPtr, 0_i_ptr))
        res = 0
      case (WM_DESTROY)
        ! Удалить таймер
        resultbool =  KillTimer(hWnd, TIMER_ID)
        call PostQuitMessage(0) 
        res = 0
      case (WM_SIZE) 
          gdiCnt = GetGuiResources(GetCurrentProcess(), 0)  ! GDI
          ! Window resize message
          lp32 = transfer(lParam, int32)
          width  = iand(lp32, 65535)              ! window width = lower 16 bits
          height = iand(ishft(lp32, -16), 65535)  ! window height = upper 16 bits
          !settrue = .true._c_bool

          panelActualWidth = max(80, width / 10)
          userData = GetWindowLongPtrW(hWnd, -21)
          appDataPtr = transfer(userData, appDataPtr)
          call c_f_pointer(appDataPtr, appDataInst)
          
          resultInvalidate = InvalidateRect(hwnd, nullptr, 0_int32)
          resultInvalidate = InvalidateRect(appDataInst%hwin, nullptr,0_int32)
          resultInvalidate = InvalidateRect(appDataInst%hPanel, nullptr, 0_int32)
          call MoveWindow(appDataInst%hwin, panelActualWidth, 0, &
                         width - panelActualWidth , height, 1)
          call MoveWindow(appDataInst%hPanel, 0, 0, &
                         panelActualWidth , height, 1)          
          
          res = 0  ! ← required

      case default
        ! All other messages — default processing
        gdiCnt = GetGuiResources(GetCurrentProcess(), 0)  ! GDI
        res = DefWindowProcW(hWnd, Msg, wParam, lParam)
        
      end select
    end function WndProc
   
   function GraphWndProc(hwnd, uMsg, wParam, lParam) bind(C, name="GraphWndProc") result(retval)
      use standard
      type(ptr), value :: hwnd
      type(PAINTSTRUCT), target :: ps
      type(RECT),        target :: rc

      type(ptr)  :: hdc
      integer(int32),  value :: uMsg
      integer(i_ptr),  value :: wParam, lParam
      integer(i_ptr)          :: retval
      integer(int32)          :: resultbool
      integer(i_ptr)          :: resultInvalidate
      integer(i_ptr)          :: userData
      type(RECT),      target :: rcSmall
      type(ptr)               :: hRedBrush
      type(AppState),  pointer :: st
      type(ptr)               :: p

      ! локальные для рисования
      real(double) :: dt
      integer(int32) :: ix, iy, k, N
      real(double)   :: baseX, baseY, step, rad, w_base
      integer(int32) :: ok, ignore
      type(ptr)      :: tmpSel
      integer(int32) :: cx, cy, r2
      integer(int32) :: x1, y1, x2, y2
      integer(int32) :: r9,g9,b9, clB11, clG11, clR11
      type(Clock), pointer :: clk

      real(double), parameter :: PI   = acos(-1.0d0)
      real(double), parameter :: sigma= 1.11d0
      real(double), parameter :: f_center = PI
      real(double), parameter :: f_edge   = 1.0d0
      real(double), parameter :: falloffP = 2.0d0

      real(double) :: cxg, cyg, dxg, dyg, dist, Rmax, mix, mix2, t, H
      integer(int32) :: r8, g8, b8
      integer(int32), parameter :: DC_PEN = 19
      integer(int32), parameter :: GR_GDIOBJECTS = 0, GR_USEROBJECTS = 1
      integer(int32) :: le
      integer(int32) :: gdiCnt, usrCnt     
      integer(int32), parameter :: GA_PARENT = 1, GA_ROOT = 2, GA_ROOTOWNER = 3

      !print *, "sizeof RECT=", c_sizeof(rc)            ! ожидается 16
      !print *, "sizeof PAINTSTRUCT=", c_sizeof(ps)      ! ожидается 72 (x64)
     
      retval = 0
      resultInvalidate = 0

      select case (uMsg)

      case (WM_CREATE)
        resultbool = SetTimer(hwnd, TIMER_ID, 16_int32, nullptr)  ! ~60 FPS

        block
          type(AppState), pointer :: st_local
          type(ptr) :: p_local
          allocate(st_local)

          ok = GetClientRect(hwnd, c_loc(rc))
          st_local%backW = rc%right - rc%left
          st_local%backH = rc%bottom - rc%top

          ! создаём memory-DC; битмап пока не создаём (лениво в WM_PAINT)
          st_local%hMemDC  = CreateCompatibleDC(nullptr)
          st_local%hBmp    = c_null_ptr
          st_local%hBmpOld = c_null_ptr

          st_local%nx = 100; st_local%ny = 100
          N = st_local%nx * st_local%ny

          baseX = 8.0d0
          baseY = 8.0d0
          step  = 7.0d0
          rad   = 4.0d0
          w_base = 2.0d0 * PI / 4.0d0

          allocate(st_local%clocks(N))
          allocate(st_local%omega_fast(N))
          allocate(st_local%omega_slow(N))
          allocate(st_local%color_ref(N))
          allocate(st_local%hue(N))

          cxg = 0.5d0 * real(st_local%nx - 1, double)
          cyg = 0.5d0 * real(st_local%ny - 1, double)
          Rmax = sqrt(cxg*cxg + cyg*cyg); if (Rmax <= 0.0d0) Rmax = 1.0d0

          do iy = 0, st_local%ny-1
            do ix = 0, st_local%nx-1
              k = iy*st_local%nx + ix + 1
              clk => st_local%clocks(k)
              clk%cx = baseX + step*real(ix, double)
              clk%cy = baseY + step*real(iy, double)
              clk%rx = rad; clk%ry = rad
              clk%theta  = 0.0d0
              clk%theta2 = 0.0d0

              dxg  = real(ix, double) - cxg
              dyg  = real(iy, double) - cyg
              dist = sqrt(dxg*dxg + dyg*dyg) / Rmax
              t    = max(0.0d0, min(1.0d0, dist))

              H = 270.0d0 * t
              call hsv_to_rgb_u8(H, 1.0d0, 1.0d0, r8, g8, b8)

              mix  = f_edge + (f_center - f_edge) * exp( - (dist/sigma)**1.0d0 )
              mix2 = real(NINT(mix * 100.0d0)) / 100.0d0

              st_local%omega_fast(k) = w_base * mix2 * 5.0d0
              st_local%omega_slow(k) = (w_base/60.0d0) * mix2 * 5.0d0

              st_local%color_ref(k)%A = 0
              st_local%color_ref(k)%R = r8
              st_local%color_ref(k)%G = g8
              st_local%color_ref(k)%B = b8
              st_local%hue(k) = H
            end do
          end do

          st_local%hbg_brush = CreateSolidBrush(MakeARGB(0,0,0,0))

          p_local = c_loc(st_local)
          call SetWindowLongPtrW(hwnd, GWLP_USERDATA, transfer(p_local, 0_i_ptr))
        end block
        retval = 0

      case (WM_DESTROY)
        resultbool = KillTimer(hwnd, TIMER_ID)

        p = transfer(GetWindowLongPtrW(hwnd, GWLP_USERDATA), nullptr)
        if (c_associated(p)) then
          call c_f_pointer(p, st)
          call CleanupAppState(st)
          if (associated(st)) deallocate(st)
          call SetWindowLongPtrW(hwnd, GWLP_USERDATA, 0_i_ptr)
        end if
        retval = 0

      case (WM_TIMER)
       
        if (wParam == int(TIMER_ID, i_ptr)) then
          p = transfer(GetWindowLongPtrW(hwnd, GWLP_USERDATA), nullptr)
          if (c_associated(p)) then
            call c_f_pointer(p, st)
            if (associated(st)) then
              dt = 0.016d0
              N  = size(st%clocks)
              do k = 1, N
                clk => st%clocks(k)
                clk%theta  = clk%theta  + st%omega_fast(k)*dt
                if (clk%theta  >= 2*PI) clk%theta  = clk%theta  - 2*PI
                clk%theta2 = clk%theta2 + st%omega_slow(k)*dt
                if (clk%theta2 >= 2*PI) clk%theta2 = clk%theta2 - 2*PI
              end do
              ok = InvalidateRect(hwnd, nullptr, 0_int32)
              
            end if
          end if
        end if
        
        retval = 0

      case (WM_SIZE)
        p = transfer(GetWindowLongPtrW(hwnd, GWLP_USERDATA), nullptr)
        if (c_associated(p)) then
          call c_f_pointer(p, st)
          if (associated(st)) then
            ok = GetClientRect(hwnd, c_loc(rc))
            st%backW = rc%right - rc%left
            st%backH = rc%bottom - rc%top

            ! Снимаем и удаляем старый битмап; новый создадим в WM_PAINT
            if (c_associated(st%hBmp)) then
              if (c_associated(st%hBmpOld)) tmpSel = SelectObject(st%hMemDC, st%hBmpOld)
              ignore = DeleteObject(st%hBmp)
              st%hBmp    = nullptr
              st%hBmpOld = nullptr
            end if
          end if
        end if
        retval = 0
        
      case (WM_ERASEBKGND)
          retval = 1

      case (WM_PAINT)
        gdiCnt = GetGuiResources(GetCurrentProcess(), 0)  ! GDI
        usrCnt = GetGuiResources(GetCurrentProcess(), 1)  ! USER
        hdc = BeginPaint(hwnd, c_loc(ps))

        if (c_associated(hdc)) then
            ok  = GetClientRect(hwnd, c_loc(rc))

               if (.not. c_associated(hdc)) then
              
                  le = GetLastError()
                  r9 = 0;
                  ! выведи в лог/консоль le
                  ! 5=ACCESS_DENIED, 6=INVALID_HANDLE, 8=NOT_ENOUGH_MEMORY, 1400=INVALID_WINDOW_HANDLE и т.д.
               end if

            gdiCnt = GetGuiResources(GetCurrentProcess(), 0)  ! GDI
            p = transfer(GetWindowLongPtrW(hwnd, GWLP_USERDATA), nullptr)
            if (c_associated(p)) then
              call c_f_pointer(p, st)
              if (associated(st)) then
                ! ЛЕНИВАЯ (re)инициализация backbuffer’а от экранного HDC
                if (.not. c_associated(st%hBmp)) then
                  ! На всякий случай вернём старый выбранный объект
                  if (c_associated(st%hBmpOld)) tmpSel = SelectObject(st%hMemDC, st%hBmpOld)
                  st%hBmp    = CreateCompatibleBitmap(hdc, st%backW, st%backH)
                  st%hBmpOld = SelectObject(st%hMemDC, st%hBmp)
                end if

                ! 1) фон
                if (c_associated(st%hbg_brush)) ok = FillRect(st%hMemDC, c_loc(rc), st%hbg_brush)

                ! 2) отрисовка
                N = size(st%clocks)
                do k = 1, N
                  clk => st%clocks(k)
                  cx = int(nint(clk%cx), int32) 
                  cy = int(nint(clk%cy), int32) 

                  x1 = cx + int( nint( clk%rx * cos(clk%theta) ), int32 )
                  y1 = cy + int( nint( clk%ry * sin(clk%theta) ), int32 )

                  r2 = int( 0.60d0 * nint(min(clk%rx, clk%ry)), int32 )
                  x2 = cx + int( nint( real(r2,double) * cos(clk%theta2) ), int32 )
                  y2 = cy + int( nint( real(r2,double) * sin(clk%theta2) ), int32 )
                  call hsv_to_rgb_u8( modulo(st%hue(k) + 90.0d0, 360.0d0), 1.0d0, 1.0d0, r9, g9, b9 )
                  clB11 = clamp255(b9 - (1 - sin(clk%theta))*10)
                  clG11 = clamp255(g9 - (1 - sin(clk%theta))*10)
                  clR11 = clamp255(r9 - (1 - sin(clk%theta))*10)
                  !call DrawHand(st%hMemDC, cx, cy, x2, y2, MakeARGB(0, clB11, clG11, clR11), 1)
                  !call DrawOrbiterDot(st%hMemDC, cx, cy, x2, y2, MakeARGB(st%color_ref(k)%A, clB11, clG11, clR11), 1)
                  clB11 = clamp255(st%color_ref(k)%B - (1 - cos(clk%theta))*10)
                  clG11 = clamp255(st%color_ref(k)%G - (1 - cos(clk%theta))*10)
                  clR11 = clamp255(st%color_ref(k)%R - (1 - cos(clk%theta))*10)
                  !call DrawHand(st%hMemDC, cx, cy, x1, y1, MakeARGB(st%color_ref(k)%A, clB11, clG11, clR11), 1)
                  call DrawOrbiterDot(st%hMemDC, cx, cy, x1, y1,  MakeARGB(st%color_ref(k)%A, clB11, clG11, clR11), 1)
                end do

                ! 3) вывод на экран
                ok = BitBlt(hdc, 0, 0, st%backW, st%backH, st%hMemDC, 0, 0, SRCCOPY)
              end if
            end if
            !call dump_hdc_coherence(hwnd, hdc, ps%hdc)
            ok = EndPaint(hwnd, c_loc(ps))
           ! err = GetLastError()
            
            !print *, "                        EndPaint=0, GetLastError=", err  ! 1400/6/8 и пр.

        else
        ! BeginPaint вернул NULL — EndPaint НЕ вызываем
        end if
     
        gdiCnt = GetGuiResources(GetCurrentProcess(), 0)  ! GDI
        usrCnt = GetGuiResources(GetCurrentProcess(), 1)  ! USER
        !print*,gdiCnt
        retval = 0

      case default
        retval = DefWindowProcW(hwnd, uMsg, wParam, lParam)
      end select

   contains
    subroutine DrawHand(hdc, cx, cy, x, y, colorBGR, width)
        use standard
        type(ptr), value :: hdc
        integer(int32), value :: cx, cy, x, y, colorBGR, width
        type(ptr) :: hOld, tmp
        integer(int32) :: ok

        ! выбрать стоковое перо и запомнить предыдущее
        hOld = SelectObject(hdc, GetStockObject(DC_PEN))
        call SetDCPenColor(hdc, colorBGR)     ! это именно subroutine

        ok  = MoveToEx(hdc, cx, cy, nullptr)
        ok  = LineTo(hdc, x, y)

        ! восстановить прежний объект
        tmp = SelectObject(hdc, hOld)
    end subroutine
    subroutine DrawOrbiterDot(hdc, cx, cy, x, y, colorBGR, offset_px)
      use standard
      type(ptr), value :: hdc
      integer(int32), value :: cx, cy, x, y, colorBGR, offset_px

      real(double) :: vx, vy, len, ux, uy
      integer(int32) :: qx, qy, ok
      type(ptr) :: hOld, tmp

      vx = real(x - cx, double)
      vy = real(y - cy, double)
      len = sqrt(vx*vx + vy*vy)
      if (len < 1.0d-9) return   ! слишком близко к центру — нечего смещать

      ux = vx / len
      uy = vy / len

      qx = x - int(nint(ux * real(offset_px, double)), int32)
      qy = y - int(nint(uy * real(offset_px, double)), int32)

      ! Поставим точку (штрих длиной 0)
      hOld = SelectObject(hdc, GetStockObject(DC_PEN))
      call SetDCPenColor(hdc, colorBGR)
      ok = MoveToEx(hdc, qx, qy, nullptr)
      ok  = LineTo(hdc, x, y)
      tmp = SelectObject(hdc, hOld)
    end subroutine

      subroutine CleanupAppState(st)
        type(AppState), pointer :: st
        type(ptr) :: tmp
        integer(int32) :: ignore
        if (.not. associated(st)) return
        if (c_associated(st%hMemDC)) then
          if (c_associated(st%hBmp)) then
            if (c_associated(st%hBmpOld)) tmp = SelectObject(st%hMemDC, st%hBmpOld)
            ignore = DeleteObject(st%hBmp)
            st%hBmp = nullptr
          end if
          ignore = DeleteDC(st%hMemDC)
          st%hMemDC  = nullptr
          st%hBmpOld = nullptr
        end if
        if (c_associated(st%hbg_brush)) then
          ignore = DeleteObject(st%hbg_brush)
          st%hbg_brush = nullptr
        end if
        if (associated(st%clocks))      deallocate(st%clocks)
        if (associated(st%omega_fast))  deallocate(st%omega_fast)
        if (associated(st%omega_slow))  deallocate(st%omega_slow)
        if (associated(st%color_ref))   deallocate(st%color_ref)
        if (associated(st%hue))         deallocate(st%hue)
      end subroutine CleanupAppState

      pure integer(int32) function clamp255(x) result(i)
        use standard
        real(double), value :: x
        real(double) :: y
        y = max(1.0d0, min(253.0d0, x))
        i = int(nint(y), int32)
      end function

      subroutine hsv_to_rgb_u8(H, S, V, r8, g8, b8)
        real(double), value :: H, S, V
        integer(int32) :: r8, g8, b8
        real(double) :: C, X, m, Hp, rp, gp, bp
        C  = V * S
        Hp = H / 60.0d0
        X  = C * (1.0d0 - abs(mod(Hp, 2.0d0) - 1.0d0))
        select case (int(floor(Hp)))
        case (0); rp=C; gp=X; bp=0.0d0
        case (1); rp=X; gp=C; bp=0.0d0
        case (2); rp=0.0d0; gp=C; bp=X
        case (3); rp=0.0d0; gp=X; bp=C
        case (4); rp=X; gp=0.0d0; bp=C
        case default; rp=C; gp=0.0d0; bp=X
        end select
        m  = V - C
        r8 = clamp255( (rp+m)*255.0d0 )
        g8 = clamp255( (gp+m)*255.0d0 )
        b8 = clamp255( (bp+m)*255.0d0 )
      end subroutine
        subroutine dump_hwnd(label, h)
          use standard
          character(*), intent(in) :: label
          type(ptr),  intent(in) :: h
          integer(i_ptr) :: v
          v = transfer(h, 0_c_intptr_t)
          write(*, '(A, Z16.16)') trim(label)//' hwnd=0x', v
        end subroutine

        subroutine dump_hwnd_chain(label, h)
          use standard
          character(*), intent(in) :: label
          type(ptr),  intent(in) :: h
          integer(i_ptr) :: v, vp, vr
          type(ptr) :: p, r
          v  = transfer(h,  0_c_intptr_t)
          p  = GetParent(h);   vp = transfer(p, 0_c_intptr_t)
          r  = GetAncestor(h, 2); vr = transfer(r, 0_c_intptr_t)  ! GA_ROOT=2
          write(*, '(A, Z16.16)') trim(label)//' hwnd=0x', v
          write(*, '(A, Z16.16)') '  parent=0x', vp
          write(*, '(A, Z16.16)') '  root=0x',   vr
        end subroutine

        subroutine dump_hdc_coherence(hwnd, hdc, ps_hdc)
          use standard
          type(ptr), intent(in) :: hwnd, hdc, ps_hdc
          type(ptr) :: wnd_from_dc
          integer(i_ptr) :: v_hwnd, v_hdc, v_ps, v_wnd
          integer(int32) :: ok
          v_hwnd = transfer(hwnd,   0_c_intptr_t)
          v_hdc  = transfer(hdc,    0_c_intptr_t)
          v_ps   = transfer(ps_hdc, 0_c_intptr_t)
          ok = IsWindow(hwnd)
          write(*,'(A,Z16.16, A, I0)') 'coh: hwnd=0x', v_hwnd, ' IsWindow=', ok
          write(*,'(A,Z16.16)') '     hdc=0x',  v_hdc
          write(*,'(A,Z16.16)') '  ps->hdc=0x', v_ps
          wnd_from_dc = WindowFromDC(hdc)
          v_wnd = transfer(wnd_from_dc, 0_c_intptr_t)
          write(*,'(A,Z16.16)') 'WindowFromDC=0x', v_wnd
          if (v_ps /= v_hdc) write(*,*) '  !! ps.hdc != hdc'
          if (v_wnd /= v_hwnd) write(*,*) '  !! WindowFromDC(hdc) != hwnd'
        end subroutine

   end function GraphWndProc
   
end module win_api