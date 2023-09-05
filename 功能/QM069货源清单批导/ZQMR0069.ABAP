*&---------------------------------------------------------------------*
*& Report ZQMR0069
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zqmr0069.

"数据定义
TABLES:eord.

DATA:lt_makt TYPE TABLE OF makt.

DATA:filename	TYPE string,
     path     TYPE string,
     fullpath TYPE string.

DATA:i_filename LIKE  rlgrap-filename.

DATA: BEGIN OF t_fieldnames  OCCURS 0,
        name(30),
      END OF t_fieldnames.

DATA: l_rc        TYPE i,
      l_filetable TYPE filetable.
TYPES:BEGIN OF ty_tmp,
        type  TYPE string,
        matnr TYPE string,
        werks TYPE string,
        zeord TYPE string,
        vdatu TYPE string,
        bdatu TYPE string,
        lifnr TYPE string,
        ekorg TYPE string,
        reswk TYPE string,
        meins TYPE string,
        ebeln TYPE string,
        ebelp TYPE string,
        flifn TYPE string,
        notkz TYPE string,
        autet TYPE string,
        ematn TYPE string,
      END OF ty_tmp.
DATA:lt_tmp TYPE TABLE OF ty_tmp,
     ls_tmp LIKE LINE OF lt_tmp.
TYPES:BEGIN OF ty_excel,
        type(1),
        matnr   TYPE eord-matnr,
        werks   TYPE eord-werks,
        zeord   TYPE eord-zeord,
        vdatu   TYPE eord-vdatu,
        bdatu   TYPE eord-bdatu,
        lifnr   TYPE eord-lifnr,
        ekorg   TYPE eord-ekorg,
        reswk   TYPE eord-reswk,
        meins   TYPE eord-meins,
        ebeln   TYPE eord-ebeln,
        ebelp   TYPE eord-ebelp,
        flifn   TYPE eord-flifn,
        notkz   TYPE eord-notkz,
        autet   TYPE eord-autet,
        ematn   TYPE eord-ematn,
      END OF ty_excel.

TYPES:BEGIN OF ty_alv.
        INCLUDE TYPE ty_excel.
        TYPES: code(1),
        msg(100),
      END OF ty_alv.
DATA:lt_excel   TYPE TABLE OF ty_excel,
     ls_excel   LIKE LINE OF lt_excel,
     lt_input   TYPE TABLE OF ty_excel,
     lt_input_a TYPE TABLE OF ty_alv, "新增
     lt_input_b TYPE TABLE OF ty_alv, "修改
     lt_input_c TYPE TABLE OF ty_alv, "删除
     lt_alv     TYPE TABLE OF ty_alv, "alv
     ls_input   LIKE LINE OF lt_input,
     ls_alv     LIKE LINE OF lt_input_a.

"选择屏幕

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(15) TEXT-001.
SELECTION-SCREEN POSITION 19.

PARAMETERS:p_daochu   RADIOBUTTON GROUP rg1 USER-COMMAND crt DEFAULT 'X'.
SELECTION-SCREEN COMMENT 24(8) TEXT-002 FOR FIELD p_daochu.
SELECTION-SCREEN POSITION 40.

PARAMETERS p_daoru RADIOBUTTON GROUP rg1 .
SELECTION-SCREEN COMMENT 48(8) TEXT-003 FOR FIELD p_daoru.

SELECTION-SCREEN END OF LINE.


SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.
SELECT-OPTIONS:s_matnr FOR eord-matnr MODIF ID m1,
               s_werk FOR eord-werks MODIF ID m1.
PARAMETERS: p_down LIKE rlgrap-filename MODIF ID m1.
PARAMETERS: p_up LIKE rlgrap-filename MODIF ID m2.
SELECTION-SCREEN END OF BLOCK b1.

"选择屏幕字段设置显示或者隐藏

AT SELECTION-SCREEN OUTPUT.
  PERFORM z_active_screen.


  "选择屏幕文件帮助

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_up.
  PERFORM frm_f4help_for_filepath.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_down.
  PERFORM frm_f4help_for_filepath.



START-OF-SELECTION.
  CLEAR:lt_excel,ls_excel.
  IF p_daochu = 'X'.
    PERFORM fm_output.
    WRITE p_down.
  ELSEIF p_daoru = 'X'.
    PERFORM fm_upload.
    WRITE p_up.
  ENDIF.




  "选择屏幕字段设置显示或者隐藏
FORM z_active_screen.
  LOOP AT SCREEN.
    IF p_daochu = 'X'.
      IF screen-group1 = 'M1'.
        screen-active = '1'.
      ELSEIF screen-group1 = 'M2'.
        screen-active = '0'.
      ENDIF.
    ELSEIF p_daoru = 'X'.
      IF screen-group1 = 'M2'.
        screen-active = '1'.
      ELSEIF screen-group1 = 'M1'.
        screen-active = '0'.
      ENDIF.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.
ENDFORM.


"选择屏幕文件帮助
FORM frm_f4help_for_filepath.

  IF p_daoru = 'X'.

    CALL METHOD cl_gui_frontend_services=>file_open_dialog
      EXPORTING
        window_title      = '选择文件'
        default_extension = 'xls'
        file_filter       = cl_gui_frontend_services=>filetype_excel
        multiselection    = ''
      CHANGING
        file_table        = l_filetable
        rc                = l_rc.
    IF sy-subrc = 0 AND l_rc = 1.
      READ TABLE l_filetable INTO p_up INDEX 1.
    ENDIF.
  ELSEIF p_daochu = 'X'.


    CALL METHOD cl_gui_frontend_services=>file_save_dialog
      EXPORTING
        window_title              = '请选择导出的目录以及填写文件名'
        default_extension         = 'xls'
        default_file_name         = '导出'
*       with_encoding             =
*       file_filter               =
*       initial_directory         =
*       prompt_on_overwrite       = 'X'
      CHANGING
        filename                  = filename
        path                      = path
        fullpath                  = fullpath
*       user_action               =
*       file_encoding             =
      EXCEPTIONS
        cntl_error                = 1
        error_no_gui              = 2
        not_supported_by_gui      = 3
        invalid_default_file_name = 4
        OTHERS                    = 5.
    IF sy-subrc <> 0.
*     Implement suitable error handling here
    ENDIF.
    p_down = fullpath.
*    READ TABLE fullpath INTO p_down INDEX 1.
  ENDIF.

ENDFORM.

FORM fm_output.
  SELECT * FROM eord INTO CORRESPONDING FIELDS OF TABLE lt_excel WHERE matnr IN s_matnr AND werks IN s_werk.
  "设置第一行
  t_fieldnames-name = '类型'.
  APPEND t_fieldnames.
  t_fieldnames-name = 'SAP物料编码'.
  APPEND t_fieldnames.
  t_fieldnames-name = '工厂'.
  APPEND t_fieldnames.
  t_fieldnames-name = '编号'.
  APPEND t_fieldnames.
  t_fieldnames-name = '有效期从'.
  APPEND t_fieldnames.
  t_fieldnames-name = '有效期到'.
  APPEND t_fieldnames.
  t_fieldnames-name = '供应商编码'.
  APPEND t_fieldnames.
  t_fieldnames-name = '采购组织'.
  APPEND t_fieldnames.
  t_fieldnames-name = '采购工厂'.
  APPEND t_fieldnames.
  t_fieldnames-name = '订单单位'.
  APPEND t_fieldnames.
  t_fieldnames-name = '协议'.
  APPEND t_fieldnames.
  t_fieldnames-name = '项目'.
  APPEND t_fieldnames.
  t_fieldnames-name = '固定源'.
  APPEND t_fieldnames.
  t_fieldnames-name = '冻结的供应源'.
  APPEND t_fieldnames.
  t_fieldnames-name = 'MRP'.
  APPEND t_fieldnames.
  t_fieldnames-name = 'MPN物料编码'.
  APPEND t_fieldnames.
  IF fullpath IS NOT INITIAL OR p_down IS NOT INITIAL.
    "第一种输出的方式
    IF abap_true = 'X'.
      IF fullpath IS INITIAL.
        fullpath = p_down.
      ENDIF.
      CALL FUNCTION 'GUI_DOWNLOAD'
        EXPORTING
*         BIN_FILESIZE            =
          filename                = fullpath
          filetype                = 'DAT'
*         APPEND                  = ' '
*         WRITE_FIELD_SEPARATOR   = ' '
*         HEADER                  = '00'
*         TRUNC_TRAILING_BLANKS   = ' '
*         WRITE_LF                = 'X'
*         COL_SELECT              = ' '
*         COL_SELECT_MASK         = ' '
*         dat_mode                = 'X'
*         CONFIRM_OVERWRITE       = ' '
          no_auth_check           = 'X'
          codepage                = '8404'
*         IGNORE_CERR             = ABAP_TRUE
*         REPLACEMENT             = '#'
*         WRITE_BOM               = ' '
*         TRUNC_TRAILING_BLANKS_EOL       = 'X'
*         WK1_N_FORMAT            = ' '
*         WK1_N_SIZE              = ' '
*         WK1_T_FORMAT            = ' '
*         WK1_T_SIZE              = ' '
*         WRITE_LF_AFTER_LAST_LINE        = ABAP_TRUE
*         SHOW_TRANSFER_STATUS    = ABAP_TRUE
*         VIRUS_SCAN_PROFILE      = '/SCET/GUI_DOWNLOAD'
*     IMPORTING
*         FILELENGTH              =
        TABLES
          data_tab                = lt_excel
          fieldnames              = t_fieldnames
        EXCEPTIONS
          file_write_error        = 1
          no_batch                = 2
          gui_refuse_filetransfer = 3
          invalid_type            = 4
          no_authority            = 5
          unknown_error           = 6
          header_not_allowed      = 7
          separator_not_allowed   = 8
          filesize_not_allowed    = 9
          header_too_long         = 10
          dp_error_create         = 11
          dp_error_send           = 12
          dp_error_write          = 13
          unknown_dp_error        = 14
          access_denied           = 15
          dp_out_of_memory        = 16
          disk_full               = 17
          dp_timeout              = 18
          file_not_found          = 19
          dataprovider_exception  = 20
          control_flush_error     = 21
          OTHERS                  = 22.
      IF sy-subrc <> 0.
* Implement suitable error handling here
      ENDIF.
    ENDIF.
    "导出Excel的第二种方法，但是需要手工弄表头
**    i_filename = fullpath.
**    CALL FUNCTION 'SAP_CONVERT_TO_XLS_FORMAT'
**      EXPORTING
***       I_FIELD_SEPERATOR =
***       I_LINE_HEADER     =
**        i_filename        = i_filename
***       I_APPL_KEEP       = ' '
**      TABLES
**        i_tab_sap_data    = lt_tmp
***   CHANGING
***       I_TAB_CONVERTED_DATA       =
**      EXCEPTIONS
**        conversion_failed = 1
**        OTHERS            = 2.
**    IF sy-subrc <> 0.
*** Implement suitable error handling here
**    ENDIF.



  ENDIF.

ENDFORM.

FORM fm_upload."最后也是最关键的一步"
  "定义表格结构内表。"
  DATA: i_excel TYPE TABLE OF alsmex_tabline, "表格结构。"
        w_excel TYPE alsmex_tabline.
  "ALSMEX_TABLINE是具有Excel数据的表行，有三个组件，row col value 。"

  DATA:get_file TYPE rlgrap-filename."这里参照系统中的结构字段。"



  READ TABLE l_filetable INDEX 1 INTO p_up."将选择的文件地址写入到地址栏"

  MOVE p_up TO get_file."将地址栏的值赋值给GET_FILE"

  "调用此函数，将Excel中的内容以类似坐标的形式存储到I_excel内表中。"
  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      filename                = get_file
      i_begin_col             = 1
      i_begin_row             = 1
      i_end_col               = 16
      i_end_row               = 65535
    TABLES
      intern                  = i_excel
    EXCEPTIONS
      inconsistent_parameters = 1
      upload_ole              = 2
      OTHERS                  = 3.
  DELETE i_excel WHERE row = 1."删除第一行抬头。如果excel文件中不存在抬头的话，可不写此句。"

  IF i_excel IS INITIAL.
    CALL FUNCTION 'POPUP_TO_CONFIRM_WITH_MESSAGE'
      EXPORTING
*       DEFAULTOPTION = 'Y'
        diagnosetext1 = '数据错误'
*       DIAGNOSETEXT2 = ' '
*       DIAGNOSETEXT3 = ' '
        textline1     = 'Excel表格中没有内容！'
        titel         = '提示'.
    RETURN.
  ENDIF.

  CLEAR:lt_input,ls_input.
  FIELD-SYMBOLS:<fs>."这里涉及到SAP内部字段的使用，详情请参见微博相关介绍。"
  SORT i_excel BY row col.
  DATA:num_col TYPE i.
  "将数据进行转换
  LOOP AT i_excel INTO w_excel.
    num_col = w_excel-col.
    ASSIGN COMPONENT  num_col  OF STRUCTURE ls_input TO <fs>.
    "数据格式处理
    "日期转换
    IF num_col = 5 OR num_col = 6.
      CALL FUNCTION 'CONVERT_DATE_TO_INTERNAL'
        EXPORTING
          date_external            = w_excel-value
        IMPORTING
          date_internal            = <fs>
        EXCEPTIONS
          date_external_is_invalid = 1
          OTHERS                   = 2.
      IF sy-subrc <> 0.
      ENDIF.
      "前导0 物料
    ELSEIF num_col = 2 OR num_col = 16.
      CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
        EXPORTING
          input        = w_excel-value
        IMPORTING
          output       = <fs>
        EXCEPTIONS
          length_error = 1
          OTHERS       = 2.
      IF sy-subrc <> 0.
      ENDIF.
      "供应商
    ELSEIF num_col = 7.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = w_excel-value
        IMPORTING
          output = <fs>.

    ELSE.
      <fs> = w_excel-value.
    ENDIF.
**    <fs> = w_excel-value.

    AT END OF row.
      APPEND ls_input TO lt_input.
      CLEAR:ls_input.
    ENDAT.
    CLEAR:w_excel.
  ENDLOOP.


  "上面处理之后的lt_input内表数据按照类型进行分类
  CHECK lt_input IS NOT INITIAL.
  CLEAR:lt_input_a,lt_input_b,lt_input_c.
  "新增
  LOOP AT lt_input INTO ls_input WHERE type = 'A'.
    MOVE-CORRESPONDING ls_input TO ls_alv.
    APPEND ls_alv TO lt_input_a.
    CLEAR:ls_input,ls_alv.
  ENDLOOP.
  "修改
  LOOP AT lt_input INTO ls_input WHERE type = 'B'.
    MOVE-CORRESPONDING ls_input TO ls_alv.
    APPEND ls_alv TO lt_input_b.
    CLEAR:ls_input,ls_alv.
  ENDLOOP.
**  "删除
**  LOOP AT lt_input INTO ls_input WHERE type = 'C'.
**    MOVE-CORRESPONDING ls_input TO ls_alv.
**    APPEND ls_alv TO lt_input_c.
**    CLEAR:ls_input,ls_alv.
**  ENDLOOP.

  "ME01创建货源清单
**  ME01创建货源清单：ME_INITIALIZE_SOURCE_LIST
**                 ME_DIRECT_INPUT_SOURCE_LIST（两个一起用，先CALL第一个不需要传参数）、
**                 ME_POST_SOURCE_LIST_NEW

  DATA: lt_eordu    TYPE STANDARD TABLE OF eordu,
        lw_eordu    TYPE eordu,
        lv_index    TYPE dzeord,
        lv_msg(200).
  CLEAR: lt_eordu, lw_eordu, lv_index.
  SORT lt_input_a BY matnr werks.
  LOOP AT lt_input_a INTO ls_alv.
    lv_index = lv_index + 1.
    lw_eordu-mandt = sy-mandt.
    lw_eordu-ernam = sy-uname.
    lw_eordu-erdat = sy-datum.
    lw_eordu-matnr = ls_alv-matnr.
    lw_eordu-werks = ls_alv-werks.
    lw_eordu-zeord = lv_index.
    lw_eordu-vdatu = ls_alv-vdatu.
    lw_eordu-bdatu = ls_alv-bdatu.
    lw_eordu-lifnr = ls_alv-lifnr.
    lw_eordu-ematn = ls_alv-ematn.
    lw_eordu-ekorg = ls_alv-ekorg.
    lw_eordu-kz = 'I'.
    APPEND lw_eordu TO lt_eordu.
    CLEAR: lw_eordu.
    AT END OF werks.
      CALL FUNCTION 'ME_INITIALIZE_SOURCE_LIST'.
      CLEAR: lv_msg, lv_index.
      CALL FUNCTION 'ME_DIRECT_INPUT_SOURCE_LIST'
        EXPORTING
          i_matnr          = ls_alv-matnr
          i_werks          = ls_alv-werks
        TABLES
          t_eord           = lt_eordu
        EXCEPTIONS
          plant_missing    = 1
          material_missing = 2
          OTHERS           = 3.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
          INTO lv_msg.
      ELSE.
        CALL FUNCTION 'ME_POST_SOURCE_LIST_NEW'
          EXPORTING
            i_matnr = ls_alv-matnr.
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
            INTO lv_msg.
        ENDIF.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.
        IF lv_msg IS INITIAL.
          ls_alv-msg = '创建货源清单成功'.
          ls_alv-code = 'S'.
        ELSE.
          ls_alv-msg = '创建货源清单失败' && lv_msg.
          ls_alv-code = 'E'.
        ENDIF.
        APPEND ls_alv TO lt_alv.
      ENDIF.
      CLEAR lv_msg.
    ENDAT.
  ENDLOOP.


  "ME02更改货源清单
**ME02更改货源清单：ME_UPDATE_SOURCES_OF_SUPPLY
  "删除也是走更新的模块，不同的是，冻结的供应源字段需要打上冻结标识
  SORT lt_input_b BY matnr werks.
  DATA:xeord   LIKE TABLE OF eordu,
       l_xeord LIKE eordu,
       yeord   LIKE TABLE OF eord,
       l_yeord LIKE eord.
  "先取出数据库中最新的数据到yeord
  CHECK lt_input_b IS NOT INITIAL.
  CLEAR:xeord,yeord.


*  "待更新的数据
*  MOVE-CORRESPONDING lt_input_b TO xeord.
  "为了保证更新的数据都能获取到状态，因此采用循环读写的方式
  LOOP AT lt_input_b ASSIGNING FIELD-SYMBOL(<fs_alv>).
    "待更新的数据
    MOVE-CORRESPONDING <fs_alv> TO l_xeord.
    l_xeord-ernam = sy-uname.
    APPEND l_xeord TO xeord.
    CLEAR l_xeord.
    AT END OF werks.
      "旧的数据
      SELECT
        *
      FROM eord
      INTO CORRESPONDING FIELDS OF TABLE yeord
      WHERE matnr = <fs_alv>-matnr
        AND werks = <fs_alv>-werks.

      CALL FUNCTION 'ME_UPDATE_SOURCES_OF_SUPPLY'
*   EXPORTING
*     DOCUMENT_NUMBER        = ' '
*     I_CHANGEDOCUMENT       =
        TABLES
          xeord = xeord
          yeord = yeord.
      IF sy-subrc = 0 .
        COMMIT WORK AND WAIT .
      ELSE .
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                INTO lv_msg.
        ROLLBACK  WORK .
      ENDIF .

      CLEAR:l_xeord,l_yeord,xeord.
    ENDAT.

  ENDLOOP.



ENDFORM.