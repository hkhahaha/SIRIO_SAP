*&---------------------------------------------------------------------*
*& 包含               ZMMR0064_CLA
*&---------------------------------------------------------------------*
"定义按钮事件
CLASS cl_event_receiver DEFINITION.
    PUBLIC SECTION.
      " 声明单击事件的方法
      METHODS handle_hotspot_click
                  FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id e_column_id.
      " 声明双击事件方法
      METHODS handle_double_click
                  FOR EVENT double_click OF cl_gui_alv_grid
        IMPORTING e_row e_column.
      " 声明Toolbar事件方法
      METHODS handle_toolbar
                  FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object e_interactive.
      METHODS handle_toolbar2
                  FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object e_interactive.
      " 声明USER-COMMAND 事件方法
      METHODS handle_command
                  FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm.
  ENDCLASS.                    "cl_event_receiver DEFINITION
  
  
  CLASS cl_event_receiver IMPLEMENTATION.
    " 单击事件方法的实现
    METHOD handle_hotspot_click.
      CONDENSE e_row_id     NO-GAPS.
      CONDENSE e_column_id  NO-GAPS.
      MESSAGE i001(00) WITH '单击事件->行号:' e_row_id  '、列名：' e_column_id.
    ENDMETHOD.                    "handle_HOTSPOT_CLICK
    " 双击事件方法的实现
    METHOD handle_double_click.
      CONDENSE e_row     NO-GAPS.
      CONDENSE e_column  NO-GAPS.
      MESSAGE i001(00) WITH '双击事件->行号:' e_row  '、列名：' e_column.
    ENDMETHOD.                    "handle_double_click
    " 实现Toolbar事件方法
    METHOD handle_toolbar.
      set_button: e_object->mt_toolbar '&SELECT'  TEXT-a01 0,
                e_object->mt_toolbar '&CANCEL' TEXT-a02 0,
                e_object->mt_toolbar '&EXPORT'     TEXT-a03 0.
  
  
    ENDMETHOD.                    "handle_toolbar
  
    METHOD handle_toolbar2.
      set_button: e_object->mt_toolbar '&PEOPLE'  TEXT-b01 0,
              e_object->mt_toolbar '&AUTO' TEXT-b02 0.
  
    ENDMETHOD.                    "handle_toolbar
    " 实现USER-COMMAND 事件方法
    METHOD handle_command.
      CASE e_ucomm.
        WHEN 'DISP'.
          MESSAGE i001(00) WITH 'Toolbar事件 + USER-COMMAND事件 '.
        WHEN '&SELECT'.
          MESSAGE 'HAHAHHA' TYPE 'S'.
      ENDCASE.
    ENDMETHOD.                    "HANDLE_COMMAND
  
  ENDCLASS.                    "cl_event_receiver IMPLEMENTATION
  
  DATA: event_receiver TYPE REF TO cl_event_receiver.