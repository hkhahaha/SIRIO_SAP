*----------------------------------------------------------------------*
***INCLUDE ZPPR0005_I01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9100 INPUT.
  DATA(lv_code) = ok_code.
  CLEAR ok_code.
  CASE lv_code.
    WHEN 'BACK'.
      DATA: lv_answer TYPE char10,
            lv_exit   TYPE flag.
      lv_exit = abap_true.
      IF go_zcl_pro_grreq_bill->bill_status <> zcl_pro_grreq_bill=>cns_status_display.
        CALL FUNCTION 'POPUP_TO_CONFIRM'
          EXPORTING
            titlebar              = '退出提示'
*           DIAGNOSE_OBJECT       = ' '
            text_question         = '入库申请单还未保存，是否确认退出单据?'
*           TEXT_BUTTON_1         = 'Ja'(001)
*           ICON_BUTTON_1         = ' '
*           TEXT_BUTTON_2         = 'Nein'(002)
*           ICON_BUTTON_2         = ' '
*           DEFAULT_BUTTON        = '1'
            display_cancel_button = ''
*           USERDEFINED_F1_HELP   = ' '
*           START_COLUMN          = 25
*           START_ROW             = 6
*           POPUP_TYPE            =
*           IV_QUICKINFO_BUTTON_1 = ' '
*           IV_QUICKINFO_BUTTON_2 = ' '
          IMPORTING
            answer                = lv_answer.

        IF lv_answer = '1'.
          lv_exit = abap_true.
        ELSE.
          lv_exit = abap_false.
        ENDIF.
      ENDIF.
      IF lv_exit = abap_true.
        "退出入库申请单界面前释放所有已加的锁
        go_zcl_pro_grreq_bill->dequeue_all_lock( ).
        LEAVE TO SCREEN 0.
      ENDIF.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9100_HANDLE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9100_handle INPUT.
  lv_code = ok_code.
  CLEAR ok_code.
  CASE lv_code.
    WHEN '&SAVE'.
      DATA: lv_e_valid       TYPE char1,
            ls_bapiret1      TYPE bapiret1,
            lt_zppt0003_mod  TYPE TABLE OF zppt0003,
            lv_ev_no_changes TYPE flag,
            lt_log_bapiret2  TYPE TABLE OF  bapiret2,
            lt_key_table     TYPE TABLE OF char70.
      "BREAK liujianmin.
      IF go_zcl_pro_grreq_bill->bill_status = zcl_pro_grreq_bill=>cns_status_display.
        MESSAGE '显示状态的单据不需要保存!' TYPE 'S'.
        RETURN.
      ENDIF.

      CLEAR:ls_bapiret1 ,lt_log_bapiret2,lv_ev_no_changes.
      go_alv_grid_create->check_changed_data( IMPORTING e_valid = lv_e_valid )."保存前，执行ALV数据有效性检查
      IF lv_e_valid = ''.
        RETURN.
      ENDIF.

      "修改状态时，检查数据有没有被改变过
      IF go_zcl_pro_grreq_bill->bill_status = zcl_pro_grreq_bill=>cns_status_changed.
        IF gs_zspp034_head_old <> zspp034_head.
          lv_ev_no_changes = ''.
        ELSE.
          lt_key_table = VALUE #( ( 'MANDT' ) ( 'ZSAPNOTE_NO' ) ( 'ZSAPNOTE_LINE' ) ).
          CALL FUNCTION 'CTVB_COMPARE_TABLES_3'
            EXPORTING
              it_table_old  = gt_zppt0003_old
              it_table_new  = gt_zppt0003
              iv_key_count  = 3 "KEY的数量
              iv_key_table  = lt_key_table "KEY的字段名称
            IMPORTING
*             ET_TABLE_DEL  =
*             ET_TABLE_ADD  =
              et_table_mod  = lt_zppt0003_mod "变化过的行
              ev_no_changes = lv_ev_no_changes. "行变化标识符号 'X'-未改变后，space-有改变过
        ENDIF.

        IF lv_ev_no_changes = 'X'.
          MESSAGE '单据数据未改变过,不需要保存!' TYPE 'S'.
          RETURN.
        ENDIF.

      ENDIF.

      "保存前的数据准备及检查
      ls_bapiret1 =  go_zcl_pro_grreq_bill->before_save( ).

      IF ls_bapiret1-type = 'E'.
        lt_log_bapiret2 = VALUE #( ( type = 'E' message = ls_bapiret1-message  ) ).
        CALL FUNCTION 'SUSR_DISPLAY_LOG'
          EXPORTING
            display_in_popup = 'X'
            log_title        = '保存失败'
          TABLES
            it_log_bapiret2  = lt_log_bapiret2
          EXCEPTIONS
            parameter_error  = 1
            OTHERS           = 2.
*        MESSAGE |{ '保存失败,异常信息为：' }{ ls_bapiret1-message }| TYPE 'E'.
      ENDIF.
      CHECK ls_bapiret1-type = 'S'.
      "保存单据
      ls_bapiret1 = go_zcl_pro_grreq_bill->save_bill( ).
      "处理保存结果
      IF ls_bapiret1-type = 'S'.
        go_zcl_pro_grreq_bill->set_bill_status( imp_bill_status = zcl_pro_grreq_bill=>cns_status_display  imp_refresh_alv = abap_true )."设置单据为显示状态及刷新ALV

        MESSAGE '保存成功!' TYPE 'S'.
      ELSE.
        lt_log_bapiret2 = VALUE #( ( type = 'E' message =  ls_bapiret1-message  ) ).
        CALL FUNCTION 'SUSR_DISPLAY_LOG'
          EXPORTING
            display_in_popup = 'X'
            log_title        = '保存失败'
          TABLES
            it_log_bapiret2  = lt_log_bapiret2
          EXCEPTIONS
            parameter_error  = 1
            OTHERS           = 2.
*       MESSAGE | { '保存失败,异常信息为：' }{ ls_bapiret1-message } |  TYPE 'E'.
      ENDIF.

    WHEN '&EDIT'.
      "新增状态单据，在保存前不需要编辑
      IF go_zcl_pro_grreq_bill->bill_status = zcl_pro_grreq_bill=>cns_status_created.
        RETURN.
      ENDIF.

      CLEAR: gt_zppt0003_old,gs_zspp034_head_old.
      gs_zspp034_head_old = zspp034_head ."修改前的单据抬头数据
      gt_zppt0003_old = gt_zppt0003."修改前的单据体数据
      "切换单据编辑模式
      IF go_zcl_pro_grreq_bill->bill_status = zcl_pro_grreq_bill=>cns_status_display.
        "权限检查
        zcl_pro_grreq=>user_authority_check( werks = zspp034_head-werks  actvt = zcl_pro_grreq=>cns_actvt_change )."修改权限检查
        "加锁
        go_zcl_pro_grreq_bill->enqueue_zsapnote_no_lock( zspp034_head-zsapnote_no ).
        "设置修改单据状态
        go_zcl_pro_grreq_bill->set_bill_status( imp_bill_status = zcl_pro_grreq_bill=>cns_status_changed imp_refresh_alv = abap_true ).
      ELSEIF go_zcl_pro_grreq_bill->bill_status = zcl_pro_grreq_bill=>cns_status_changed.
        go_alv_grid_create->check_changed_data( IMPORTING e_valid = lv_e_valid )."执行ALV数据有效性检查
        IF lv_e_valid = ''.
          RETURN.
        ENDIF.
        "设置显示单据状态
        go_zcl_pro_grreq_bill->set_bill_status( imp_bill_status = zcl_pro_grreq_bill=>cns_status_display imp_refresh_alv = abap_true ).
        "释放锁
        go_zcl_pro_grreq_bill->dequeue_zsapnote_no_lock( zspp034_head-zsapnote_no ).
      ENDIF.
    WHEN '&PRINT'.
      IF go_zcl_pro_grreq_bill->bill_status = zcl_pro_grreq_bill=>cns_status_display.
        DATA:lv_formname           TYPE tdsfname, "smartforms函数名称
             ls_head               TYPE zppr0005_sf_head,
             ls_item               TYPE zppr0005_sf_item,
             lt_item               TYPE TABLE OF zppr0005_sf_item,
             ls_control_parameters TYPE ssfctrlop, "smartforms打印参数
             ls_output_options     TYPE ssfcompop, "smartforms打印输出参数
             lv_zjsmng             TYPE char17,
             lv_zwsmng             TYPE char17,
             lv_zstdname           TYPE char70,
             lv_meins              TYPE char30.

        "准备传smartforms参数值
        zppt0003 = gt_zppt0003[ 1 ].
        ls_head-zsapnote_no = zppt0003-zsapnote_no."申请单号
        ls_head-budat = zppt0003-budat. "入库申请日期
        ls_head-zwslgort = zppt0003-zwslgort. "车间仓库编码
        ls_head-zwslgobe = zppt0003-zwslgobe. "车间仓库名称
        "文件体系编号
        SELECT SINGLE ztxbh INTO ls_head-zdoc_num FROM zqmt007 WHERE werks = zppt0003-werks AND zbddh = 'ZPP005'.

        "车间名称处理
        CONDENSE zppt0003-zwslgobe NO-GAPS.
        SPLIT zppt0003-zwslgobe AT '_' INTO TABLE DATA(lt_zwslgobe).
        DATA(lv_lines) = lines( lt_zwslgobe ).
        IF lv_lines >= 2.
          ls_head-zworkshop = lt_zwslgobe[ 2 ].
        ENDIF.

        "制单人完整名称处理
        CALL FUNCTION 'FDM_CUST_USER_NAME_READ_SINGLE'
          EXPORTING
            i_user_id  = zppt0003-uname
          IMPORTING
            e_fullname = ls_head-biller.

        "工厂对应的公司名称处理
        SELECT SINGLE t001~butxt FROM t001
           INNER JOIN t001k ON t001~bukrs = t001k~bukrs
           INNER JOIN t001w ON t001w~bwkey = t001k~bwkey
           WHERE t001w~werks = @zppt0003-werks
         INTO @ls_head-butxt.

        CLEAR gt_matnr.
        MOVE-CORRESPONDING gt_zppt0003 TO gt_matnr.
        SORT gt_matnr BY matnr.
        DELETE ADJACENT DUPLICATES FROM gt_matnr COMPARING matnr.
*        "获取物料组
        SELECT matnr,matkl FROM mara FOR ALL ENTRIES IN @gt_matnr
          WHERE mara~matnr = @gt_matnr-matnr
          INTO TABLE @DATA(lt_mara).
        SORT lt_mara BY matkl.
        CLEAR lt_item.
        LOOP AT gt_zppt0003 INTO zppt0003 WHERE zdelflg = ''.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
            EXPORTING
              input  = zppt0003-zsapnote_line
            IMPORTING
              output = ls_item-zsapnote_line.

          "物料分类的标准名称
          READ TABLE lt_mara WITH KEY matnr = zppt0003-matnr BINARY SEARCH INTO DATA(ls_mara).
          lv_zstdname = zcl_bc_public=>get_class_value( atnam = 'ZSTDNAME' key = zppt0003-matnr classtype = '001' classnum = ls_mara-matkl objtable = 'MARA' ).

          ls_item-matnr = zppt0003-matnr."物料编号
          ls_item-maktx = |{ zppt0003-maktx }   { lv_zstdname }|."物料描述/标准名称
          ls_item-charg = |{ zppt0003-zpack_charg }   { zppt0003-charg }|."包装批号(打印)/SAP批次号
          ls_item-zhsdat = zppt0003-zhsdat."生产日期
          ls_item-menge =  zppt0003-menge."数量
          lv_zjsmng = zcl_bc_public=>trailing_quan_zero( zppt0003-zjsmng )."件数去掉小数点0部分
          IF zppt0003-zwsmng IS NOT INITIAL.
            CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
              EXPORTING
                input  = zppt0003-meins
              IMPORTING
                output = lv_meins.

            lv_zwsmng = zcl_bc_public=>trailing_quan_zero( zppt0003-zwsmng )."尾数数去掉小数点0部分
            IF lv_zjsmng IS INITIAL.
              ls_item-zjsmng = |{ '0件(' }{ lv_zwsmng }{ lv_meins }{ ')' }|."件数
            ELSE.
              ls_item-zjsmng = |{ lv_zjsmng }{ '件(' }{ lv_zwsmng }{ lv_meins }{ ')' }|."件数
            ENDIF.

          ELSE.
            ls_item-zjsmng = lv_zjsmng."件数
          ENDIF.

          ls_item-meins = zppt0003-meins."基本计量单位
          ls_item-kdauf = |{ zppt0003-kdauf ALPHA = OUT }|."销售订单
          ls_item-lgort = zppt0003-lgort."库存地点
          ls_item-groes = zppt0003-zspec."物料的规格
          ls_item-zpspe =  zppt0003-zpspe."包装规格

          "备注
          CONDENSE: zppt0003-wempf,zppt0003-ablad NO-GAPS.
          IF zppt0003-ablad IS NOT INITIAL.
            ls_item-zbktxt = | { zppt0003-wempf }{ ',' }{ zppt0003-ablad }|.
          ELSE.
            ls_item-zbktxt = zppt0003-wempf.
          ENDIF.
          APPEND ls_item TO lt_item.
        ENDLOOP.

*  获取smartforms表单的函数名称
        CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
          EXPORTING
            formname           = 'ZPPR0005_SF'
          IMPORTING
            fm_name            = lv_formname
          EXCEPTIONS
            no_form            = 1
            no_function_module = 2
            OTHERS             = 3.
        IF sy-subrc <> 0.
          MESSAGE '打印表单不存在,请联系IT人员!' TYPE 'E'.
        ENDIF.

*   控制默认打印机隐藏预览功能
        ls_output_options-tddest = 'LP01'.  "指定默认打印机名，一般为四位字符
        ls_output_options-tdarmod = 1."确认本地设置是否有效，必选项
*  调用smartforms表单打印
        CALL FUNCTION lv_formname
          EXPORTING
*           ARCHIVE_INDEX    =
*           ARCHIVE_INDEX_TAB  =
*           ARCHIVE_PARAMETERS =
*           control_parameters = ls_control_parameters
*           MAIL_APPL_OBJ    =
*           MAIL_RECIPIENT   =
*           MAIL_SENDER      =
            output_options   = ls_output_options  "传入用户端设置参数
            user_settings    = '' "启用用户端端设置，必选项
            gs_head          = ls_head
            gv_lines         = lines( lt_item )
* IMPORTING
*           DOCUMENT_OUTPUT_INFO       =
*           JOB_OUTPUT_INFO  =
*           JOB_OUTPUT_OPTIONS =
          TABLES
            gt_detail        = lt_item
          EXCEPTIONS
            formatting_error = 1
            internal_error   = 2
            send_error       = 3
            user_canceled    = 4
            OTHERS           = 5.
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                    INTO DATA(mtext)
                    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.
      ELSE.
        MESSAGE '不是显示状态的单据不能打印' TYPE 'S'.
      ENDIF.
    WHEN 'ZBWART'."移动类型修改重新检查单据体的仓库
      PERFORM frm_check_lgort.
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  MOD_ZWSLGORT_F4  INPUT
*&---------------------------------------------------------------------*
*       抬头屏幕车间仓库F4搜索帮助
*----------------------------------------------------------------------*
MODULE mod_zwslgort_f4 INPUT.
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'LGORT' "返回字段
      dynpprog        = sy-repid  ""返回的输入框所在的main program
      dynpnr          = sy-dynnr "返回的输入框所在屏幕
      dynprofield     = 'ZSPP034_HEAD-ZWSLGORT' "搜索到的仓库编号输入到抬头屏幕的车间仓库字段中
      value_org       = 'S' "固定S
    TABLES
      value_tab       = gt_t001l "提供搜索的仓库列表
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  CHECK_VALUE_INPUT  INPUT
*&---------------------------------------------------------------------*
*       屏幕抬头输入值的检查处理
*----------------------------------------------------------------------*
MODULE check_value_input INPUT.
  "检查输入的车间仓库编号
  IF zspp034_head-zwslgort IS NOT INITIAL.
    READ TABLE gt_t001l  WITH  KEY lgort = zspp034_head-zwslgort INTO gs_lgort.
    IF sy-subrc <> 0.
      CLEAR zspp034_head-zwslgort.
      MESSAGE '输入的车间仓库无效,请检查!' TYPE 'S' DISPLAY LIKE 'W'.
    ENDIF.
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9200_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9200_exit INPUT.
  lv_code = ok_code.
  CLEAR ok_code.
  CASE lv_code.
    WHEN 'BACK'.
*      lv_exit = abap_true.
*      CALL FUNCTION 'POPUP_TO_CONFIRM'
*        EXPORTING
*          titlebar              = '退出提示'
**         DIAGNOSE_OBJECT       = ' '
*          text_question         = '是否返回到上一个屏幕'
**         TEXT_BUTTON_1         = 'Ja'(001)
**         ICON_BUTTON_1         = ' '
**         TEXT_BUTTON_2         = 'Nein'(002)
**         ICON_BUTTON_2         = ' '
**         DEFAULT_BUTTON        = '1'
*          display_cancel_button = ''
**         USERDEFINED_F1_HELP   = ' '
**         START_COLUMN          = 25
**         START_ROW             = 6
**         POPUP_TYPE            =
**         IV_QUICKINFO_BUTTON_1 = ' '
**         IV_QUICKINFO_BUTTON_2 = ' '
*        IMPORTING
*          answer                = lv_answer.
      LEAVE TO SCREEN 0.
    WHEN OTHERS.
  ENDCASE.
*  IF lv_answer = '1'.
*    LEAVE TO SCREEN 0.
*  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9200 INPUT.
  save_code = ok_code.
  CLEAR ok_code.
  CASE save_code.
    WHEN '&EMADDR'.
      AUTHORITY-CHECK OBJECT 'ZPP005'
*        ID 'WERKS' FIELD 'xxxxxxxx'
       ID 'ZQRSCAN' FIELD '7'."邮箱维护权限
      IF sy-subrc <> 0.
        MESSAGE s000 WITH '你没有维护邮箱地址的权限'.
        RETURN.
      ENDIF.
      CALL SCREEN '9300'.
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9300_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9300_exit INPUT.
  save_code = ok_code.
  CLEAR ok_code.
  CASE save_code.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9300  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9300 INPUT.
  save_code = ok_code.
  CLEAR ok_code.
  CASE save_code.
    WHEN 'SAVE'.
      go_qrmail_oper->save_mail( ).
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  CHECK_LGORT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_lgort INPUT.
  "移动类型101不能输入样品仓库，Y01只能输入样品仓库.
  CLEAR gv_msg.
  gv_msg =  go_zcl_pro_grreq_bill->check_lgort_by_bwart( EXPORTING iv_bwart = zspp034_head-bwart iv_lgort = zspp034_head-zwslgort  ).
  IF gv_msg IS NOT INITIAL.
    CLEAR zspp034_head-zwslgort.
    MESSAGE gv_msg TYPE 'S'.
  ENDIF.
ENDMODULE.