*&---------------------------------------------------------------------*
*& 包含               ZSDR0001_ALV
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*       CLASS lcl_event_handler DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_event_handler DEFINITION.
    PUBLIC SECTION.
  
      METHODS:
  *-- To add new functional buttons to the ALV toolbar
      toolbar
      FOR EVENT toolbar OF cl_gui_alv_grid
      IMPORTING e_object
                e_interactive,
  
  *-- To implement user commands
      user_command
      FOR EVENT user_command OF cl_gui_alv_grid
      IMPORTING e_ucomm,
  
  *-- Hotspot click control
      hotspot_click
      FOR EVENT hotspot_click OF cl_gui_alv_grid
      IMPORTING e_row_id e_column_id es_row_no,
  
  *-- Handle double click
      double_click
      FOR EVENT double_click OF cl_gui_alv_grid
      IMPORTING e_row
                e_column
                es_row_no,
  
  *-- To be triggered before user commands
      before_user_command
      FOR EVENT before_user_command OF cl_gui_alv_grid
      IMPORTING e_ucomm,
  
  *-- To be triggered after user commands
      after_user_command
      FOR EVENT after_user_command OF cl_gui_alv_grid
      IMPORTING e_ucomm
                e_saved
                e_not_processed,
  
  *-- Controling data changes where ALV grid is changeable
      data_changed
      FOR EVENT data_changed OF cl_gui_alv_grid
      IMPORTING er_data_changed
                e_onf4
                e_onf4_before
                e_onf4_after
                e_ucomm,
  
  *-- To be triggered after data change has been finished
      data_changed_finished
      FOR EVENT data_changed_finished OF cl_gui_alv_grid
      IMPORTING e_modified
                et_good_cells,
  
  *-- To control menu buttons
      menu_button
      FOR EVENT menu_button OF cl_gui_alv_grid
      IMPORTING e_object
                e_ucomm,
  
  *-- To control button clicks
      button_click
      FOR EVENT button_click OF cl_gui_alv_grid
      IMPORTING es_col_id
                es_row_no,
  
  *-- To control AFTER_REFRESH
      after_refresh
      FOR EVENT after_refresh OF cl_gui_alv_grid,
  
  *-- To control  HANDLE_F4
      onf4
      FOR EVENT onf4 OF cl_gui_alv_grid
      IMPORTING e_fieldname
                es_row_no
                er_event_data
                et_bad_cells,
  *-- To control ONDROPCOMPLETE
      ondropcomplete
      FOR EVENT ondropcomplete OF cl_gui_alv_grid
      IMPORTING e_row
                e_column
                es_row_no
                e_dragdropobj.
  
    PRIVATE SECTION.
  
  ENDCLASS.                    "LCL_EVENT_HANDLER DEFINITION
  *----------------------------------------------------------------------*
  *       CLASS lcl_event_handler IMPLEMENTATION
  *----------------------------------------------------------------------*
  *
  *----------------------------------------------------------------------*
  CLASS lcl_event_handler IMPLEMENTATION.
  
  *-- Handle toolbar
    METHOD toolbar.
      PERFORM frm_alv_toolbar IN PROGRAM (sy-repid) USING e_object e_interactive IF FOUND.
    ENDMETHOD.                    "handle_toolbar
  
  *-- Handle user command
    METHOD user_command.
      PERFORM frm_alv_user_command IN PROGRAM (sy-repid) USING e_ucomm IF FOUND.
    ENDMETHOD.                    "handle_user_command
  
  *-- Handle hotspot click
    METHOD hotspot_click.
      PERFORM frm_alv_hotspot_click IN PROGRAM (sy-repid) USING e_row_id e_column_id es_row_no IF FOUND.
    ENDMETHOD.                    "handle_hotspot_click
  
  *-- Handle double click
    METHOD double_click.
      PERFORM frm_alv_double_click IN PROGRAM (sy-repid) USING e_row e_column es_row_no IF FOUND.
    ENDMETHOD.                    "handle_double_click
  
  *-- Handle before user command
    METHOD before_user_command.
      PERFORM frm_alv_before_user_command IN PROGRAM (sy-repid) USING e_ucomm IF FOUND.
    ENDMETHOD.                    "handle_before_user_command
  
  *-- Handle after user command
    METHOD after_user_command.
      PERFORM frm_alv_after_user_command IN PROGRAM (sy-repid) USING e_ucomm e_saved e_not_processed IF FOUND.
    ENDMETHOD.                    "handle_after_user_command
  
  *-- Handle data changed
    METHOD data_changed.
      PERFORM frm_alv_data_changed IN PROGRAM (sy-repid) USING er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm IF FOUND.
    ENDMETHOD.                    "handle data_changed
  
  *-- Handle data changed finished
    METHOD data_changed_finished.
      PERFORM frm_alv_data_changed_finished IN PROGRAM (sy-repid) USING e_modified et_good_cells IF FOUND.
    ENDMETHOD.                    "handle_data_changed_finished
  
  *-- Handle menu button
    METHOD menu_button.
      PERFORM frm_alv_menu_button IN PROGRAM (sy-repid) USING e_object e_ucomm IF FOUND.
    ENDMETHOD.                    "handle_menu_button
  
  *-- Handle button click
    METHOD button_click.
      PERFORM frm_alv_button_click IN PROGRAM (sy-repid) USING es_col_id es_row_no IF FOUND.
    ENDMETHOD.                    "handle_button_click
  
  *-- Handle after refresh
    METHOD after_refresh.
      PERFORM frm_alv_after_refresh IN PROGRAM (sy-repid) IF FOUND.
    ENDMETHOD.                    "handle_after_refresh
  
  *-- Handle f4
    METHOD onf4.
      PERFORM frm_alv_onf4 IN PROGRAM (sy-repid) USING e_fieldname es_row_no er_event_data et_bad_cells IF FOUND.
    ENDMETHOD.                                                "handle_f4
  
  *-- Handle ondropcomplete
    METHOD ondropcomplete.
      PERFORM frm_alv_ondropcomplete IN PROGRAM (sy-repid) USING e_row e_column es_row_no e_dragdropobj IF FOUND.
    ENDMETHOD.                    "handle_ondropcomplete
  
  ENDCLASS.                    "LCL_EVENT_HANDLER IMPLEMENTATION