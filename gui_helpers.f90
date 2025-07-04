﻿module gui_helpers
  use win_api
  use win_types
  use standard
contains

  ! Creates the main application window and registers its class.
  subroutine create_main_window(hwnd, hInstance, appDataPtr, hBrush, wcx, regResult, &
                                classNameW, windowTitleW, iconPathW, cursorPathW, coordX, coordY, width, height)
    type(ptr), intent(out)        :: hwnd             ! Returned handle of the main window
    type(ptr), intent(in)         :: hInstance        ! Application instance handle
    type(c_ptr), intent(in)       :: appDataPtr       ! Pointer to user data (AppData)
    type(ptr), intent(in)         :: hBrush           ! Window background brush
    type(WNDCLASSEX), intent(out), target :: wcx      ! Window class structure
    character(kind=char), intent(in), target :: classNameW(:), windowTitleW(:)
    character(kind=char), intent(in), target :: iconPathW(:), cursorPathW(:)
    integer(int32), intent(out)     :: regResult        ! Window registration code
    integer(int32), intent(in)      :: coordX, coordY, width, height    ! Panel coordinates and size
    
    ! Configure the window class structure
    wcx%cbSize             = c_sizeof(wcx)
    wcx%style              = 0
    wcx%lpfnWndProc        = c_funloc(WndProc)
    wcx%cbClsExtra         = 0
    wcx%cbWndExtra         = 0
    wcx%hInstance          = hInstance
    wcx%hIcon              = LoadImageW(nullptr, c_loc(iconPathW(1)), IMAGE_ICON, 0, 0, LR_LOADFROMFILE)
    wcx%hCursor            = LoadImageW(nullptr, c_loc(cursorPathW(1)), IMAGE_ICON, 0, 0, LR_LOADFROMFILE)
    wcx%hbrBackground      = hBrush
    !wcx%hbrBackground      = nullptr
    wcx%lpszMenuName       = nullptr
    wcx%lpszClassName      = c_loc(classNameW(1))
    wcx%hIconSm            = wcx%hIcon

    ! Register the window class
    regResult = RegisterClassExW(c_loc(wcx))
    if (regResult /= 0) then
      print *, "Main class registered successfully."
    else
      print *, "Error registering the main class, code:", GetLastError()
    end if

    ! Create the main window
    hwnd = CreateWindowExW(0, c_loc(classNameW(1)), c_loc(windowTitleW(1)), &
            WS_OVERLAPPEDWINDOW, coordX, coordY, width, height, nullptr, nullptr, hInstance, appDataPtr)
  end subroutine create_main_window

  ! Registers and creates a child panel inside the main window
  subroutine create_panel_window(hPanel, hwndParent, hInstance, hBrush, wcxPanel, regResult, panelClassW, width, height)
    type(ptr), intent(out)        :: hPanel           ! Panel handle
    type(ptr), intent(in)         :: hwndParent       ! Parent window
    type(ptr), intent(in)         :: hInstance        ! Application handle
    type(ptr), intent(in)         :: hBrush           ! Background brush
    type(WNDCLASSEX), intent(out), target :: wcxPanel ! Panel class structure
    character(kind=char), intent(in), target :: panelClassW(:)
    integer(int32), intent(out)     :: regResult        ! Registration code
    integer(int32), intent(in)      :: width, height    ! Panel size    
    type(ptr), parameter :: IDC_ARROW = transfer(ID_ARROW, nullptr)
    
    ! Configure the panel class
    wcxPanel%cbSize             = c_sizeof(wcxPanel)
    wcxPanel%style              = 0
    wcxPanel%lpfnWndProc        = c_funloc(WndProc)
    wcxPanel%cbClsExtra         = 0
    wcxPanel%cbWndExtra         = 0
    wcxPanel%hInstance          = hInstance
    wcxPanel%hIcon              = nullptr
    wcxPanel%hCursor            = LoadCursorW(nullptr, IDC_ARROW)
    wcxPanel%hbrBackground      = hBrush
    wcxPanel%lpszMenuName       = nullptr
    wcxPanel%lpszClassName      = c_loc(panelClassW(1))
    wcxPanel%hIconSm            = nullptr

    ! Register the panel class
    regResult = RegisterClassExW(c_loc(wcxPanel))
    if (regResult /= 0) then
      print *, "Panel class registered successfully."
    else
      print *, "Error registering the panel class, code:", GetLastError()
    end if

    ! Create the panel
    hPanel = CreateWindowExW(0, c_loc(panelClassW(1)), nullptr, &
             WS_CHILD_VISIBLE, 0, 0, width, height, hwndParent, nullptr, hInstance, nullptr)
  end subroutine create_panel_window

  ! Registers and creates a graphics widget for 2D plotting
  subroutine create_graph_window(hGraph, hwndParent, hInstance, hPanelBrush, wcxGraph, &
             regResult, graphClassW, cursorPathW, x, y, width, height)
    type(ptr), intent(out)        :: hGraph           ! Graphics window handle
    type(ptr), intent(in)         :: hwndParent, hInstance    
    type(ptr), intent(in)         :: hPanelBrush           ! Background brush
    type(WNDCLASSEX), intent(out), target :: wcxGraph ! Graph class structure
    character(kind=char), intent(in), target :: graphClassW(:)
    integer(int32), intent(out)     :: regResult
    integer(int32), intent(in)      :: x, y, width, height
    character(kind=char), intent(in), target ::  cursorPathW(:)  ! Cursor 
    type(ptr) :: tmp_ptr
    !type(RECT), target :: rc
    !integer(int32) :: res
    
    wcxGraph%cbSize             = c_sizeof(wcxGraph)
    wcxGraph%style              = 0
    wcxGraph%lpfnWndProc        = c_funloc(GraphWndProc)   ! Separate WndProc
    wcxGraph%cbClsExtra         = 0
    wcxGraph%cbWndExtra         = c_sizeof(tmp_ptr)
    wcxGraph%hInstance          = hInstance
    wcxGraph%hIcon              = nullptr
    wcxGraph%hCursor            = LoadImageW(nullptr, c_loc(cursorPathW(1)), IMAGE_ICON, 0, 0, LR_LOADFROMFILE)
    wcxGraph%hbrBackground      = hPanelBrush
   ! wcxGraph%hbrBackground      = nullptr
    wcxGraph%lpszMenuName       = nullptr
    wcxGraph%lpszClassName      = c_loc(graphClassW(1))
    wcxGraph%hIconSm            = nullptr

    regResult = RegisterClassExW(c_loc(wcxGraph))
    if (regResult /= 0) then
      print *, "Graph class registered successfully."
    else
      print *, "Error registering graph class, code:", GetLastError()
    end if

    hGraph = CreateWindowExW(0, c_loc(graphClassW(1)), nullptr, &
             WS_CHILD_VISIBLE, x, y, width, height, hwndParent, nullptr, hInstance, nullptr)
  end subroutine create_graph_window
  
  ! Creates a button in the specified parent window
  subroutine create_button(hButton, hParent, hInstance, buttonTextW, classButtonW, idButton, regResult, x, y, width, height)
    type(ptr), intent(out)                   :: hButton          ! Button handle
    type(ptr), intent(in)                    :: hParent, hInstance
    character(kind=char), intent(in), target :: buttonTextW(:), classButtonW(:)
    integer(i_ptr), intent(in)               :: idButton
    integer(int32), intent(out)                :: regResult
    integer(int32), intent(in)                 :: x, y             ! Button coordinates 
    integer(int32), intent(in)                 :: width, height    ! Button size 
    type(c_ptr) :: hMenuAsPtr

    ! Convert button identifier to pointer
    hMenuAsPtr = transfer(idButton, hMenuAsPtr)

    ! Create the button
    hButton = CreateWindowExW(0, c_loc(classButtonW(1)), c_loc(buttonTextW(1)), &
         WS_CHILD_VISIBLE + BS_DEFPUSHBUTTON, x, y, width, height, hParent, hMenuAsPtr, hInstance, nullptr)

    ! Check for success
    if (.not. c_associated(hButton)) then
      regResult = GetLastError()
      print *, "The button has not been created! Error code:", regResult
    else
      regResult = 0
      print *, "Button created successfully."
    end if
  end subroutine create_button

end module gui_helpers
