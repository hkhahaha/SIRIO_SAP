FUNCTION z_qm_info_if.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     REFERENCE(NEW_INSPLOT) TYPE  QALS
*"----------------------------------------------------------------------
  DATA z1(2).
  DATA:lt_zqmt052a TYPE TABLE OF zqmt052a,
       ls_zqmt052a TYPE zqmt052a.

  SELECT SINGLE
    matnr,
    zif
  INTO @DATA(ls_zqmt052)
  FROM zqmt052
  WHERE matnr = @new_insplot-matnr
    AND zif = '否'.
  "没有找到数据，说明对应的物料编码资料未齐全
  IF sy-subrc = 0.
    "发送邮件
    PERFORM getmail TABLES lt_zqmt052a."获取收件人的邮箱地址
    LOOP AT lt_zqmt052a INTO ls_zqmt052a.
      PERFORM send USING new_insplot ls_zqmt052a."发送邮件
      CLEAR ls_zqmt052a.
    ENDLOOP.

    "弹窗
    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar       = '请确认！'
*       DIAGNOSE_OBJECT             = ' '
        text_question  = '资料未齐全，请确认是否放行'
        text_button_1  = '是'(001)
        icon_button_1  = '是'
        text_button_2  = '否'(002)
        icon_button_2  = '否'
*       DEFAULT_BUTTON = '1'
*       DISPLAY_CANCEL_BUTTON       = 'X'
*       USERDEFINED_F1_HELP         = ' '
*       START_COLUMN   = 25
*       START_ROW      = 6
*       POPUP_TYPE     =
*       IV_QUICKINFO_BUTTON_1       = ' '
*       IV_QUICKINFO_BUTTON_2       = ' '
      IMPORTING
        answer         = z1
*     TABLES
*       PARAMETER      =
      EXCEPTIONS
        text_not_found = 1
        OTHERS         = 2.
    IF sy-subrc <> 0.
* Implement suitable error handling here
      MESSAGE '发生错误，请联系IT' TYPE 'E' DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

    IF z1 = '2' OR z1 = 'A'.
      MESSAGE '操作中止' TYPE 'E' DISPLAY LIKE 'S'.
      CLEAR z1.
      EXIT.
    ENDIF.




  ENDIF.




ENDFUNCTION.
FORM getmail TABLES lt_zqmt052a.
  SELECT
    *
  INTO CORRESPONDING FIELDS OF TABLE lt_zqmt052a
  FROM zqmt052a.
ENDFORM.

FORM send USING new_insplot TYPE qals ls_zqmt052a TYPE zqmt052a.
  CONSTANTS:
    gc_tab  TYPE c VALUE cl_bcs_convert=>gc_tab,
    gc_crlf TYPE c VALUE cl_bcs_convert=>gc_crlf.
  DATA send_request   TYPE REF TO cl_bcs.
  DATA document       TYPE REF TO cl_document_bcs.
  DATA recipient      TYPE REF TO if_recipient_bcs.
  DATA: bcs_exception TYPE REF TO cx_bcs,
        lo_sender     TYPE REF TO cl_sapuser_bcs.
  DATA main_text      TYPE bcsy_text.
  DATA binary_content TYPE solix_tab.
  DATA size           TYPE so_obj_len.
  DATA sent_to_all    TYPE os_boolean.
  CLEAR recipient.
  TRY.
      CLEAR main_text.
*     -------- create persistent send request ------------------------
      send_request = cl_bcs=>create_persistent( ).

*     -------- create and set document with attachment ---------------
*     create document object from internal table with text
      APPEND '您好,' TO main_text.                            "#EC NOTEXT
      APPEND '以下物料编码' && new_insplot-matnr && '对应检验批'
       && new_insplot-prueflos && '资料尚未齐全，请尽快补齐并联系产品专员。' TO main_text.


      DATA:lv_header TYPE so_obj_des,
           lv_file   TYPE so_obj_des.
      GET TIME.
      lv_header = '物料编码' && new_insplot-matnr && '资料不齐'.
      document = cl_document_bcs=>create_document(
      i_type    = 'RAW'
      i_text    = main_text
      i_subject =  lv_header ).                             "#EC NOTEXT



      GET TIME.

*     发件人
*     前提是这个邮箱地址能发邮件，并且不需要密码
      lo_sender = cl_sapuser_bcs=>create( 'MAILSERVER' ).
      send_request->set_sender( lo_sender ).
*     add document object to send request
      send_request->set_document( document ).

*     --------- add recipient (e-mail address) -----------------------
*     create recipient object
*      recipient = cl_cam_address_bcs=>create_internet_address( mailto ).
**     add recipient object to send request
*      send_request->add_recipient( recipient ).
*      recipient = cl_cam_address_bcs=>create_internet_address( 'zhengde.zhang@sirio.cn' ).
*      send_request->add_recipient( recipient ).


***********添加多个收件人************************
      DATA lv_string TYPE ad_smtpadr.
      lv_string = ls_zqmt052a-zmail.
      recipient = cl_cam_address_bcs=>create_internet_address( lv_string ).
      send_request->add_recipient( recipient ).

*****************END*****************************
*     ---------- send document ---------------------------------------
*      sent_to_all = send_request->send( i_with_error_screen = 'X' ).

      send_request->set_send_immediately( 'X' ).  "设置立即发送
      send_request->send_request->set_link_to_outbox( 'X' ). "与outbox关联
*    CALL METHOD send_request->set_document( document ).
      CALL METHOD send_request->send(
        EXPORTING
          i_with_error_screen = 'X'
        RECEIVING
          result              = sent_to_all ).
      COMMIT WORK.

      IF sent_to_all = 'X'.
        MESSAGE '发送成功' TYPE 'S'.
      ELSE.
        ROLLBACK WORK.
        MESSAGE '发送失败' TYPE 'S' DISPLAY LIKE 'E'.
        STOP.
      ENDIF.
      CLEAR lv_string.
*   ------------ exception handling ----------------------------------
*   replace this rudimentary exception handling with your own one !!!
    CATCH cx_bcs INTO bcs_exception.
      MESSAGE i865(so) WITH bcs_exception->error_type.
  ENDTRY.

ENDFORM.