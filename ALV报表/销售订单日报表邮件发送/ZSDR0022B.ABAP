*&---------------------------------------------------------------------*
*& Report ZSDR0022B
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZSDR0022B.

CONSTANTS:
gc_tab  TYPE C VALUE cl_bcs_convert=>gc_tab,
gc_crlf TYPE C VALUE cl_bcs_convert=>gc_crlf.

PARAMETERS:
mailto TYPE ad_smtpadr
DEFAULT 'kai.huang@sirio.cn'.                               "#EC *

DATA send_request   TYPE REF TO cl_bcs.
DATA document       TYPE REF TO cl_document_bcs.
DATA recipient      TYPE REF TO if_recipient_bcs.
DATA: bcs_exception TYPE REF TO cx_bcs,
      lo_sender     TYPE REF TO cl_sapuser_bcs.

DATA main_text      TYPE bcsy_text.
DATA binary_content TYPE solix_tab.
DATA SIZE           TYPE so_obj_len.
DATA sent_to_all    TYPE os_boolean.

START-OF-SELECTION.
PERFORM create_content.
PERFORM send.

*&---------------------------------------------------------------------*
*&      Form  send
*&---------------------------------------------------------------------*
FORM send.

  TRY.

*     -------- create persistent send request ------------------------
    send_request = cl_bcs=>create_persistent( ).

*     -------- create and set document with attachment ---------------
*     create document object from internal table with text
    APPEND '你好' TO main_text.                             "#EC NOTEXT
    document = cl_document_bcs=>create_document(
    i_type    = 'RAW'
    i_text    = main_text
    i_subject = 'Test Created By BCS_EXAMPLE_7' ).        "#EC NOTEXT

*     add the spread sheet as attachment to document object
    document->add_attachment(
    i_attachment_type    = 'xls'                          "#EC NOTEXT
    i_attachment_subject = '附件'                           "#EC NOTEXT
    i_attachment_size    = SIZE
    i_att_content_hex    = binary_content ).
*     发件人
*     前提是这个邮箱地址能发邮件，并且不需要密码
    lo_sender = cl_sapuser_bcs=>create( 'HUANGKAI' ).
    send_request->set_sender( lo_sender ).
*     add document object to send request
    send_request->set_document( document ).

*     --------- add recipient (e-mail address) -----------------------
*     create recipient object
    recipient = cl_cam_address_bcs=>create_internet_address( mailto ).
*     add recipient object to send request
    send_request->add_recipient( recipient ).
    recipient = cl_cam_address_bcs=>create_internet_address( 'zhengde.zhang@sirio.cn' ).
    send_request->add_recipient( recipient ).
*     ---------- send document ---------------------------------------
*      sent_to_all = send_request->send( i_with_error_screen = 'X' ).

    send_request->set_send_immediately( 'X' ).  "设置立即发送
    send_request->send_request->set_link_to_outbox( 'X' ). "与outbox关联
    CALL METHOD send_request->set_document( document ).
    CALL METHOD send_request->send(
    EXPORTING
      i_with_error_screen = 'X'
      RECEIVING
      result              = sent_to_all ).
    COMMIT WORK AND WAIT.

    IF sent_to_all = 'X'.
      MESSAGE '发送成功' TYPE 'S'.
    ELSE.
      ROLLBACK WORK.
      MESSAGE '发送失败' TYPE 'S' DISPLAY LIKE 'E'.
      STOP.
    ENDIF.

*   ------------ exception handling ----------------------------------
*   replace this rudimentary exception handling with your own one !!!
  CATCH cx_bcs INTO bcs_exception.
    MESSAGE i865(so) WITH bcs_exception->error_type.
  ENDTRY.

ENDFORM.                    "send

*&---------------------------------------------------------------------*
*&      Form  create_content
*&---------------------------------------------------------------------*
* Create Example Content
* 1) Write example text into a string
* 2) convert this string to solix_tab
*----------------------------------------------------------------------*
FORM create_content.

  DATA lv_string TYPE string.
  DATA ls_zsdt0019b TYPE zsdt0019b.

* --------------------------------------------------------------
* as example content we use some system messages out of t100
* get them for all installed languages from db
* and write one line for each language into the spread sheet

* columns are separated by TAB and each line ends with CRLF

  CONCATENATE '这个是报表'                                       "#EC NOTEXT
  gc_crlf gc_crlf
  INTO lv_string.

* header line
  CONCATENATE lv_string
  '订单状态'    gc_tab
  '订单状态描述'    gc_tab
  '二审日期'    gc_tab
  '创建日期'    gc_tab
  '售达方'    gc_tab
  '销售组织'    gc_tab
  '分销渠道'    gc_tab
  '产品组'    gc_tab
  '检索项'    gc_tab
  '实际客户'    gc_tab

  '业务员'    gc_tab
  '业务员名称'    gc_tab
  '订单类型'    gc_tab
  '订单类型描述'    gc_tab
  '单据号'    gc_tab
  '付款方式'    gc_tab
  '产品编号'    gc_tab
  '行号'    gc_tab
  '产品名称'    gc_tab
  '规格型号'    gc_tab
  '包装规格'    gc_tab
  '基本计量数量'    gc_tab

  '基本计量单位'    gc_tab
  '销售数量'    gc_tab
  '销售单位'    gc_tab
  '折前单价'    gc_tab
  '价税合计'    gc_tab
  '计划行类型'    gc_tab
  '内部交货日期'    gc_tab
  'ATP答复日期'    gc_tab
  '出库量'    gc_tab
  '制单人'    gc_tab
  '非限制库存'    gc_tab
  '物料中类名称'    gc_crlf "#EC NOTEXT                                        "#EC NOTEXT
  INTO lv_string.
  DATA:lv_kwmeng(30),
        lv_kwmeng2(30),
        lv_kbetr(30),
        lv_netwr(30),
        lv_rfmng(30),
        lv_kalab(30).

* data lines
  SELECT * FROM zsdt0019b INTO ls_zsdt0019b UP TO 20 ROWS.
    CLEAR:
    lv_kwmeng,
    lv_kwmeng2,
    lv_kbetr,
    lv_netwr,
    lv_rfmng,
    lv_kalab.

    lv_kwmeng = ls_zsdt0019b-kwmeng.
    lv_kwmeng2 = ls_zsdt0019b-kwmeng2.
    lv_kbetr = ls_zsdt0019b-kbetr.
    lv_netwr = ls_zsdt0019b-netwr.
    lv_rfmng = ls_zsdt0019b-rfmng.
    lv_kalab = ls_zsdt0019b-kalab.

    CONCATENATE lv_string
    ls_zsdt0019b-stat gc_tab
    ls_zsdt0019b-statdetail gc_tab
    ls_zsdt0019b-udate gc_tab
    ls_zsdt0019b-erdat gc_tab
    ls_zsdt0019b-kunnr gc_tab
    ls_zsdt0019b-vkorg gc_tab
    ls_zsdt0019b-vtweg gc_tab
    ls_zsdt0019b-spart gc_tab
    ls_zsdt0019b-bu_sort1 gc_tab
    ls_zsdt0019b-kunnr2 gc_tab

    ls_zsdt0019b-kunn2 gc_tab
    ls_zsdt0019b-name_org1 gc_tab
    ls_zsdt0019b-auart gc_tab
    ls_zsdt0019b-bezei gc_tab
    ls_zsdt0019b-vbeln gc_tab
    ls_zsdt0019b-text1 gc_tab
    ls_zsdt0019b-matwa gc_tab
    ls_zsdt0019b-posnr gc_tab
    ls_zsdt0019b-maktx gc_tab
    ls_zsdt0019b-groes gc_tab
    ls_zsdt0019b-atwrt gc_tab
    lv_kwmeng gc_tab

    ls_zsdt0019b-meins gc_tab
    lv_kwmeng2 gc_tab
    ls_zsdt0019b-vrkme gc_tab
    lv_kbetr gc_tab
    lv_netwr gc_tab
    ls_zsdt0019b-ettyp gc_tab
    ls_zsdt0019b-znbjhq gc_tab
    ls_zsdt0019b-zatpdfrq gc_tab
    lv_rfmng gc_tab
    ls_zsdt0019b-name_text gc_tab
    lv_kalab gc_tab
    ls_zsdt0019b-wgbez  gc_crlf
    INTO lv_string.

  ENDSELECT.

*  SELECT * FROM t100 INTO ls_t100
*  WHERE arbgb = 'SO' AND msgnr = '316'.
*
*    CONCATENATE lv_string
*    ls_t100-arbgb gc_tab
*    ls_t100-msgnr gc_tab
*    ls_t100-sprsl gc_tab
*    ls_t100-text  gc_crlf
*    INTO lv_string.
*
*  ENDSELECT.

* --------------------------------------------------------------
* convert the text string into UTF-16LE binary data including
* byte-order-mark. Mircosoft Excel prefers these settings
* all this is done by new class cl_bcs_convert (see note 1151257)

  TRY.
    cl_bcs_convert=>string_to_solix(
    EXPORTING
      iv_string   = lv_string
      iv_codepage = '4103'  "suitable for MS Excel, leave empty
      iv_add_bom  = 'X'     "for other doc types
    IMPORTING
      et_solix  = binary_content
      ev_size   = SIZE ).
  CATCH cx_bcs.
    MESSAGE e445(so).
  ENDTRY.

ENDFORM.                    "create_content