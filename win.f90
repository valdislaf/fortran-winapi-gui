! Главная программа
program WinMain
  use win_api
  use gui_helpers
  implicit none

  ! --- Переменные ---
  integer(int) :: regResult
  type(WNDCLASSEX), target :: wcx, wcxPanel
  type(MSG_T), target :: msg_inst
  type(ptr) :: hwnd, hInstance, hBrush, hPanelBrush
  type(AppData), target :: appDataInst
  type(c_ptr) :: appDataPtr
  type(ptr) :: hButton
  character(kind=char), allocatable, target :: windowTitleW(:), classNameW(:), panelClassW(:), buttonTextW(:), classButtonW(:)    
  character(kind=char), allocatable, target :: iconPathW(:), cursorPathW(:)
  integer(int), parameter :: panelWidth = 800 / 10
  
  ! --- Строки ---
  allocate(cursorPathW(0)) ! ← аналог "инициализации значением по умолчанию" как в С++
  cursorPathW    = to_wide_null_terminated("cross.ico")
  iconPathW      = to_wide_null_terminated("favicon.ico")
  classNameW     = to_wide_null_terminated("My window class")
  windowTitleW   = to_wide_null_terminated("Fortran Window")
  panelClassW    = to_wide_null_terminated("PanelClass")
  buttonTextW    = to_wide_null_terminated("Click me")
  classButtonW   = to_wide_null_terminated("Button")

  ! --- Прочее ---
  hInstance = nullptr
  appDataInst%hPanel = c_null_ptr
  appDataPtr = c_loc(appDataInst)
  hBrush = CreateSolidBrush(MakeARGB(0, 50, 30, 10))
  hPanelBrush = CreateSolidBrush(MakeARGB(0, 40, 20, 0))

  ! --- Главное окно ---
  call create_main_window(hwnd, hInstance, appDataPtr, hBrush, wcx, regResult, &
                        classNameW, windowTitleW, iconPathW, cursorPathW)
  call ShowWindow(hwnd, SW_SHOW)
  call UpdateWindow(hwnd)

  ! --- Панель ---
  call create_panel_window(appDataInst%hPanel, hwnd, hInstance, hPanelBrush, wcxPanel, regResult, panelClassW, panelWidth, 600)
  call ShowWindow(appDataInst%hPanel, SW_SHOW)
  call UpdateWindow(appDataInst%hPanel)  

  ! --- Кнопка ---   
  call create_button(hButton, appDataInst%hPanel, hInstance, buttonTextW, classButtonW, transfer(ID_BUTTON1, 0_i_ptr), regResult)
  call ShowWindow(hButton, SW_SHOW)
  call UpdateWindow(hButton)   
  
  ! Устанавливаем пользовательские данные (AppData) в окно.
  ! Это необходимо делать *после* создания панели и всех её компонентов,
  ! чтобы структура данных содержала актуальные указатели (например, на hPanel).
  ! Используется в WndProc для доступа к этим данным через GetWindowLongPtrW.
  call SetWindowLongPtrW(hwnd, -21, transfer(c_loc(appDataInst), 0_i_ptr))
  
  ! --- Цикл сообщений ---
  do while (GetMessageW(c_loc(msg_inst), nullptr, 0, 0) > 0)
    call TranslateMessage(c_loc(msg_inst))
    call DispatchMessageW(c_loc(msg_inst))
  end do
end program WinMain
