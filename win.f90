! Main program
program WinMain
  use win_api
  use gui_helpers

  ! --- WinAPI handles and structures ---
  type(WNDCLASSEX), target :: wcx        ! Main window class
  type(WNDCLASSEX), target :: wcxPanel   ! Panel class
  type(WNDCLASSEX), target :: wcxGraph   ! Graphics window class
  type(MSG_T), target       :: msg_inst  ! Message processing structure
  type(ptr)                 :: hwnd       ! Main window
  type(ptr)                 :: hInstance  ! Application handle
  type(ptr)                 :: hBrush     ! Background brush for window
  type(ptr)                 :: hPanelBrush! Background brush for panel
  type(ptr)                 :: hButton    ! Button (temporarily reused)

  ! --- User data for the window ---
  type(AppData), target :: appDataInst
  type(c_ptr)           :: appDataPtr
  type(c_ptr)           :: msgPtr
  
  ! --- Variables for the graphics widget ---
  type(ptr) :: hGraphBrush  ! variable for passing nullptr
  
  ! --- WinAPI strings (named resources) ---
  character(kind=char), allocatable, target :: windowTitleW(:)
  character(kind=char), allocatable, target :: classNameW(:)
  character(kind=char), allocatable, target :: panelClassW(:)
  character(kind=char), allocatable, target :: graphClassW(:)
  character(kind=char), allocatable, target :: classButtonW(:)
  character(kind=char), allocatable, target :: iconPathW(:)
  character(kind=char), allocatable, target :: cursorPathW(:)
  character(kind=char), allocatable, target :: buttonTextW(:)
  character(kind=char), allocatable, target :: buttonTextW2(:)

  ! --- Other parameters ---
  integer(int32)                  :: regResult             ! Window class registration result
  integer(int32), parameter       :: panelWidth = 800 / 10 ! Panel width
  integer(int32)                  :: width, height         ! Window size
  integer(int32)                  :: coordX = 10
  integer(int32)                  :: coordY = 780 
  
  type(RECT), target  :: rc
  integer(int32) :: res
  integer(int32)                  :: i = 0 
  ! --- String initialization ---
  allocate(cursorPathW(0))  ! ← equivalent to "default value initialization"
  cursorPathW    = to_wide_null_terminated("cross.ico")
  iconPathW      = to_wide_null_terminated("favicon.ico")
  classNameW     = to_wide_null_terminated("My window class")
  windowTitleW   = to_wide_null_terminated("Fortran Window")
  panelClassW    = to_wide_null_terminated("PanelClass")
  graphClassW    = to_wide_null_terminated("GraphClass")
  buttonTextW    = to_wide_null_terminated("Click me")
  buttonTextW2   = to_wide_null_terminated("Click me2")
  classButtonW   = to_wide_null_terminated("Button")

  ! --- Handle and brush initialization ---
  hInstance          = nullptr
  appDataInst%hPanel = nullptr
  appDataInst%hwin   = nullptr
  appDataPtr         = c_loc(appDataInst)
  hBrush             = CreateSolidBrush(MakeARGB(0, 50, 30, 10))
  hPanelBrush        = CreateSolidBrush(MakeARGB(0, 255, 20, 0))
  msgPtr             =  c_loc(msg_inst)
  hGraphBrush        = CreateSolidBrush(MakeARGB(0, 60, 60, 10))
  
  ! initial window size
  width              = 800
  height             = 600
  ! --- Create main window ---
  call create_main_window(hwnd, hInstance, appDataPtr, hBrush, wcx, regResult, &
                          classNameW, windowTitleW, iconPathW, cursorPathW, coordX, coordY, width, height)
  call ShowWindow(hwnd, SW_SHOW)
  call UpdateWindow(hwnd)

  ! --- Create panel ---
  call create_panel_window(appDataInst%hPanel, hwnd, hInstance, hPanelBrush, &
                           wcxPanel, regResult, panelClassW, panelWidth, height)

  call ShowWindow(appDataInst%hPanel, SW_SHOW)
  call UpdateWindow(appDataInst%hPanel)

  ! --- Create graphics widget ---
  call create_graph_window(appDataInst%hwin, hwnd, hInstance, hGraphBrush, &
      wcxGraph, regResult, graphClassW, cursorPathW, panelWidth, 0, width - panelWidth , height)

  res = GetClientRect(appDataInst%hwin, c_loc(rc))
  print *, "GetClientRect create_graph_window : res=", res, " rc=", rc%left, rc%top, rc%right, rc%bottom

  call ShowWindow(appDataInst%hwin, SW_SHOW)
  call UpdateWindow(appDataInst%hwin)
  !do while (i<1000000000)
  !    call UpdateWindow(appDataInst%hwin)
  !    i =i +1
  !end do
  
  res = GetClientRect(appDataInst%hwin, c_loc(rc))
  print *, "GetClientRect ShowWindow: res=", res, " rc=", rc%left, rc%top, rc%right, rc%bottom
  
  ! --- Create buttons ---
  call create_button(hButton, appDataInst%hPanel, hInstance, buttonTextW, &
                     classButtonW, ID_BUTTON1, regResult, 2, 2, 76, 26)
  call ShowWindow(hButton, SW_SHOW)
  call UpdateWindow(hButton)

  call create_button(hButton, appDataInst%hPanel, hInstance, buttonTextW2, &
                     classButtonW, ID_BUTTON2, regResult, 2, 28, 76, 26)
  call ShowWindow(hButton, SW_SHOW)
  call UpdateWindow(hButton)

  ! --- Set window user data ---
  ! Important to call after creating all elements so AppData structure is up to date.
  call SetWindowLongPtrW(hwnd, -21, transfer(appDataPtr, 0_i_ptr))

  ! --- Message processing loop ---
  do while (GetMessageW(msgPtr, nullptr, 0, 0) > 0)
    call TranslateMessage(msgPtr)
    call DispatchMessageW(msgPtr)
  end do

end program WinMain
