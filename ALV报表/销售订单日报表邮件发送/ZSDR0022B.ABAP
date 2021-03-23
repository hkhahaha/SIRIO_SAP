*&---------------------------------------------------------------------*
*& Report ZSDR0022B
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zsdr0022b.
TABLES:vbak,zsdt0019a.

TYPES:BEGIN OF ty_zsdt0019a,
        partner   TYPE zsdt0019a-partner,
        name_org1 TYPE zsdt0019a-name_org1,
        job       TYPE zsdt0019a-job,
        zdomain   TYPE zsdt0019a-zdomain,
        mail      TYPE zsdt0019a-mail,
        vtweg     TYPE zsdt0019c-vtweg,
      END OF ty_zsdt0019a.

CONSTANTS:
  gc_tab        TYPE c VALUE cl_bcs_convert=>gc_tab,
  gc_crlf       TYPE c VALUE cl_bcs_convert=>gc_crlf,
  content1(255) VALUE '您每天早上收到的是前一天下达的订单信息，每周五上午收到报表的是本周下达订单和执行中订单的汇总，订单范围涵盖：按单订单、中试订单、返工订单、公司间订单；',
  content2(255) VALUE '特别提醒：入库日期为为内部时间仅供业务员参考，对外发货日期将由客服根据客户和物流情况后对外答复客户。',
  content3(255) VALUE '如您订单信息有疑惑的，还请跟客服同事了解详情。',
  content4(255) VALUE '谢谢支持。'.
SELECTION-SCREEN BEGIN OF BLOCK blk WITH FRAME TITLE TEXT-001."定义屏幕
SELECT-OPTIONS:
s_erdat FOR vbak-erdat,"创建日期"for前面的是字段显示的名称，可以修改，但是注意不要超过8个字符，否则会报错
s_udate FOR vbak-erdat,"二审日期
s_job FOR zsdt0019a-job."人员职位
SELECTION-SCREEN END OF BLOCK blk.

DATA send_request   TYPE REF TO cl_bcs.
DATA document       TYPE REF TO cl_document_bcs.
DATA recipient      TYPE REF TO if_recipient_bcs.
DATA: bcs_exception TYPE REF TO cx_bcs,
      lo_sender     TYPE REF TO cl_sapuser_bcs.

DATA main_text      TYPE bcsy_text.
DATA binary_content TYPE solix_tab.
DATA size           TYPE so_obj_len.
DATA sent_to_all    TYPE os_boolean.
DATA lt_zsdt0019a TYPE TABLE OF ty_zsdt0019a.
DATA flag(1).
DATA:lt_zsdt0019c TYPE TABLE OF zsdt0019c,
     ls_zsdt0019c TYPE zsdt0019c.

START-OF-SELECTION.
  PERFORM getmail CHANGING lt_zsdt0019a.
  LOOP AT lt_zsdt0019a INTO DATA(ls_zsdt0019a).
    "上面的内表中，业务员和区域经理分成2部分，接下来进行拆分
    "首先是只是业务员的部分
    CLEAR flag.
    IF ls_zsdt0019a-job = '01' OR ls_zsdt0019a-job = '02'.
      PERFORM create_content USING ls_zsdt0019a CHANGING flag.
      IF flag <> 'X'.
        PERFORM send USING ls_zsdt0019a.
      ENDIF.
      CLEAR ls_zsdt0019a.
    ELSEIF ls_zsdt0019a-job = '03'.
      "03类型需要获取对应的分销渠道
      CLEAR lt_zsdt0019c.
      SELECT
      *
      FROM zsdt0019c
      WHERE zsdt0019c~partner = @ls_zsdt0019a-partner
      INTO CORRESPONDING FIELDS OF TABLE @lt_zsdt0019c.
      IF lt_zsdt0019c IS NOT INITIAL.
        LOOP AT lt_zsdt0019c INTO ls_zsdt0019c.
          ls_zsdt0019a-vtweg = ls_zsdt0019c-vtweg.
          PERFORM create_content USING ls_zsdt0019a CHANGING flag.
          IF flag <> 'X'.
            PERFORM send USING ls_zsdt0019a.
          ENDIF.
          CLEAR ls_zsdt0019c.
        ENDLOOP.
      ENDIF.
      CLEAR ls_zsdt0019a.
    ENDIF.

  ENDLOOP.




FORM getmail CHANGING lt_zsdt0019a TYPE STANDARD TABLE.
  "首先先查出全部的数据
  SELECT
  *
  INTO CORRESPONDING FIELDS OF TABLE lt_zsdt0019a
  FROM zsdt0019a
  WHERE zsdt0019a~active = 'X'
  AND zsdt0019a~job IN s_job.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  send
*&---------------------------------------------------------------------*
FORM send USING ls_zsdt0019a TYPE ty_zsdt0019a.
  CLEAR recipient.
  TRY.
      CLEAR main_text.
*     -------- create persistent send request ------------------------
      send_request = cl_bcs=>create_persistent( ).

*     -------- create and set document with attachment ---------------
*     create document object from internal table with text
      APPEND '您好,' TO main_text.                            "#EC NOTEXT
      APPEND '请查收销售报表。' TO main_text.
      APPEND content1 TO main_text.
      APPEND content2 TO main_text.
      APPEND content3 TO main_text.
      APPEND content4 TO main_text.

      DATA:lv_header TYPE so_obj_des,
           lv_file   TYPE so_obj_des.
      IF ls_zsdt0019a-job <> '03'.
        GET TIME.
        lv_header = 'Sales Daily Report 销售日报' && sy-datum && '||' && ls_zsdt0019a-name_org1.
        document = cl_document_bcs=>create_document(
        i_type    = 'RAW'
        i_text    = main_text
        i_subject =  lv_header ).                           "#EC NOTEXT
      ELSEIF ls_zsdt0019a-job = '03'.
        CLEAR lv_header.

        SELECT SINGLE
        vtext
        INTO @DATA(lv_vtweg)
              FROM tvtwt
              WHERE tvtwt~spras = '1'
              AND tvtwt~vtweg = @ls_zsdt0019c-vtweg.
        GET TIME.
        lv_header = 'Sales Daily Report 销售日报' && sy-datum && '||分销渠道：' && lv_vtweg.
        document = cl_document_bcs=>create_document(
        i_type    = 'RAW'
        i_text    = main_text
        i_subject = lv_header ).                            "#EC NOTEXT
      ENDIF.


*      i_subject = '销售日报').  "#EC NOTEXT
      GET TIME.
      lv_file = 'Sales Daily Report 销售日报' && sy-datum.
*     add the spread sheet as attachment to document object
      document->add_attachment(
      i_attachment_type    = 'xls'                          "#EC NOTEXT
      i_attachment_subject = lv_file                        "#EC NOTEXT
*      i_attachment_subject = 'Sales Daily.XLS' "#EC NOTEXT
      i_attachment_size    = size
      i_att_content_hex    = binary_content ).
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

      lv_string = ls_zsdt0019a-mail.
      recipient = cl_cam_address_bcs=>create_internet_address( lv_string ).
      send_request->add_recipient( recipient ).

*****************END*****************************
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
      COMMIT WORK.

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
FORM create_content USING ls_zsdt0019a TYPE ty_zsdt0019a CHANGING flag.

  DATA lv_string TYPE string.
  DATA:ls_zsdt0019b  TYPE zsdt0019b,
       lt_zsdt0019b  TYPE TABLE OF zsdt0019b,
       lt_zsdt0019b2 TYPE TABLE OF zsdt0019b.
  SELECT
  *
  FROM zsdt0019b
  INTO CORRESPONDING FIELDS OF TABLE lt_zsdt0019b.


* --------------------------------------------------------------
* as example content we use some system messages out of t100
* get them for all installed languages from db
* and write one line for each language into the spread sheet

* columns are separated by TAB and each line ends with CRLF

*  CONCATENATE '这个是报表'                                       "#EC NOTEXT
*  gc_crlf gc_crlf
*  INTO lv_string.

* header line
  CLEAR:lv_string.
  CONCATENATE lv_string
  '订单状态'  gc_tab
  '实际客户检索项'	gc_tab
  '实际业务员'	gc_tab
  '制单人'   gc_tab
  '订单类型描述' 	gc_tab
  '单据号'	gc_tab
  '行号'  gc_tab
  '产品编号'  gc_tab
  '产品名称'  gc_tab
  '二审日期（订单下达计划）'  gc_tab
  '原始交货期（客户要求货期)'	gc_tab
  '内部交货日期	'gc_tab
  'ATP答复日期（计划答复入库日期）'	gc_tab
  '规格型号'  gc_tab
  '包装规格Packing Specification'	gc_tab
  '基本计量数量 QTY'  gc_tab
  '基本计量单位'  gc_tab
  '销售数量'  gc_tab
  '销售单位'  gc_tab
  '折前单价'  gc_tab
  '价税合计'  gc_tab
  '不含税金额CNY'  gc_tab
  '计划行类型'	gc_tab
  '工厂'  gc_tab
  '最晚入库日期'  gc_tab
  '非限制库存（已检测放行）'  gc_tab
  '其他库存（检测中未放行）'  gc_tab
  '出库量（已发货数量，销售单位）'	gc_tab
  '开票量（已开票数量，销售单位）'	gc_tab
  '付款方式'  gc_tab
  '物料中类名称'  gc_tab
  '售达方'	gc_tab
  '实际客户'  gc_tab
  '分销渠道'  gc_tab
  '创建日期'  gc_crlf

  INTO lv_string.
  DATA:lv_kwmeng(30),
       lv_kwmeng2(30),
       lv_kbetr(30),
       lv_netwr(30),
       lv_rfmng(30),
       lv_kalab(30),
       lv_netwr2(30),
       lv_invoice(30),
       lv_kains(30),
       "时间加上斜杆
       lv_udate(30),
       lv_zysjhq(30),
       lv_znbjhq(30),
       lv_zatpdfrq(30),
       lv_budat(30),
       lv_erdat(30).

* data lines
*接下来获取ZSDT0019B的全部数据

  IF ls_zsdt0019a-job = '01'.
    DELETE lt_zsdt0019b WHERE salemanno <> ls_zsdt0019a-partner.
    DELETE lt_zsdt0019b WHERE erdat NOT IN s_erdat.
    DELETE lt_zsdt0019b WHERE udate NOT IN s_udate.
    IF lt_zsdt0019b IS INITIAL.
      flag = 'X'.
      EXIT.
    ENDIF.
    LOOP AT lt_zsdt0019b INTO ls_zsdt0019b.
      CLEAR:
      lv_kwmeng,
      lv_kwmeng2,
      lv_kbetr,
      lv_netwr,
      lv_rfmng,
      lv_kalab,
      lv_netwr2,
      lv_invoice,
      lv_kains,
      lv_udate,
      lv_zysjhq,
      lv_znbjhq,
      lv_zatpdfrq,
      lv_budat,
      lv_erdat.

      lv_kwmeng = ls_zsdt0019b-kwmeng.
      lv_kwmeng2 = ls_zsdt0019b-kwmeng2.
      lv_kbetr = ls_zsdt0019b-kbetr.
      lv_netwr = ls_zsdt0019b-netwr.
      lv_rfmng = ls_zsdt0019b-rfmng.
      lv_kalab = ls_zsdt0019b-kalab.
      lv_netwr2 = ls_zsdt0019b-netwr2.
      lv_invoice = ls_zsdt0019b-invoice.
      lv_kains = ls_zsdt0019b-kains.
      IF ls_zsdt0019b-udate IS NOT INITIAL.
        lv_udate = ls_zsdt0019b-udate+0(4) && '/' && ls_zsdt0019b-udate+4(2) && '/' && ls_zsdt0019b-udate+6(2).
      ENDIF.
      IF ls_zsdt0019b-zysjhq IS NOT INITIAL.
        lv_zysjhq = ls_zsdt0019b-zysjhq+0(4) && '/' && ls_zsdt0019b-zysjhq+4(2) && '/' && ls_zsdt0019b-zysjhq+6(2).
      ENDIF.
      IF ls_zsdt0019b-znbjhq IS NOT INITIAL.
        lv_znbjhq = ls_zsdt0019b-znbjhq+0(4) && '/' && ls_zsdt0019b-znbjhq+4(2) && '/' && ls_zsdt0019b-znbjhq+6(2).
      ENDIF.
      IF ls_zsdt0019b-zatpdfrq IS NOT INITIAL.
        lv_zatpdfrq = ls_zsdt0019b-zatpdfrq+0(4) && '/' && ls_zsdt0019b-zatpdfrq+4(2) && '/' && ls_zsdt0019b-zatpdfrq+6(2).
      ENDIF.
      IF ls_zsdt0019b-budat IS NOT INITIAL.
        lv_budat = ls_zsdt0019b-budat+0(4) && '/' && ls_zsdt0019b-budat+4(2) && '/' && ls_zsdt0019b-budat+6(2).
      ENDIF.
      IF ls_zsdt0019b-erdat IS NOT INITIAL.
        lv_erdat = ls_zsdt0019b-erdat+0(4) && '/' && ls_zsdt0019b-erdat+4(2) && '/' && ls_zsdt0019b-erdat+6(2).
      ENDIF.

      CONCATENATE lv_string
      ls_zsdt0019b-statdetail	gc_tab"订单状态
      ls_zsdt0019b-bu_sort1	gc_tab"实际客户检索项
      ls_zsdt0019b-saleman  gc_tab"实际业务员
      ls_zsdt0019b-name_text  gc_tab"制单人
      ls_zsdt0019b-bezei  gc_tab"订单类型描述
      ls_zsdt0019b-vbeln  gc_tab"单据号
      ls_zsdt0019b-posnr  gc_tab"行号
      ls_zsdt0019b-matwa  gc_tab"产品编号
      ls_zsdt0019b-maktx  gc_tab"产品名称
      lv_udate  gc_tab"二审日期
      lv_zysjhq	gc_tab"原始交货期
      lv_znbjhq	gc_tab"内部交货期
      lv_zatpdfrq	gc_tab"ATP答复日期
      ls_zsdt0019b-groes  gc_tab"规格型号
      ls_zsdt0019b-atwrt  gc_tab"包装规格
      lv_kwmeng	gc_tab"基本计量数量
      ls_zsdt0019b-meins  gc_tab"基本计量单位
      lv_kwmeng2  gc_tab"销售数量
      ls_zsdt0019b-vrkme  gc_tab"销售单位
      lv_kbetr  gc_tab"折前单价
      lv_netwr  gc_tab"价税合计
      lv_netwr2	gc_tab"不含税金额
      ls_zsdt0019b-ettyp  gc_tab"计划行类型
      ls_zsdt0019b-werks  gc_tab"工厂
      lv_budat  gc_tab"最晚入库日期
      lv_kalab  gc_tab"非限制库存
      lv_kains  gc_tab"其他库存
      lv_rfmng  gc_tab"出库量
      lv_invoice  gc_tab"开票量
      ls_zsdt0019b-text1  gc_tab"付款方式
      ls_zsdt0019b-wgbez  gc_tab"物料中类名称
      ls_zsdt0019b-kunnr_nam  gc_tab"售达方
      ls_zsdt0019b-kunnr_det  gc_tab"实际客户
      ls_zsdt0019b-vtweg  gc_tab"分销渠道
      lv_erdat  gc_crlf"创建日期
      INTO lv_string.
      CLEAR:ls_zsdt0019b.
    ENDLOOP.
  ELSEIF ls_zsdt0019a-job = '02'.

    SELECT
    *
    FROM @lt_zsdt0019b AS a
    WHERE a~salemanno IN (
    "获取02类型的全部销售区域下面的业务员
    SELECT
    partner
    FROM zsdt0019a
    WHERE zsdt0019a~zdomain = @ls_zsdt0019a-zdomain
    AND zsdt0019a~active = 'X'
    )
    INTO TABLE @lt_zsdt0019b.
    DELETE lt_zsdt0019b WHERE erdat NOT IN s_erdat.
    DELETE lt_zsdt0019b WHERE udate NOT IN s_udate.
    IF lt_zsdt0019b IS INITIAL.
      flag = 'X'.
      EXIT.
    ENDIF.
    LOOP AT lt_zsdt0019b INTO ls_zsdt0019b.
      CLEAR:
      lv_kwmeng,
      lv_kwmeng2,
      lv_kbetr,
      lv_netwr,
      lv_rfmng,
      lv_kalab,
      lv_netwr2,
      lv_invoice,
      lv_kains,
      lv_udate,
      lv_zysjhq,
      lv_znbjhq,
      lv_zatpdfrq,
      lv_budat,
      lv_erdat.

      lv_kwmeng = ls_zsdt0019b-kwmeng.
      lv_kwmeng2 = ls_zsdt0019b-kwmeng2.
      lv_kbetr = ls_zsdt0019b-kbetr.
      lv_netwr = ls_zsdt0019b-netwr.
      lv_rfmng = ls_zsdt0019b-rfmng.
      lv_kalab = ls_zsdt0019b-kalab.
      lv_netwr2 = ls_zsdt0019b-netwr2.
      lv_invoice = ls_zsdt0019b-invoice.
      lv_kains = ls_zsdt0019b-kains.
      IF ls_zsdt0019b-udate IS NOT INITIAL.
        lv_udate = ls_zsdt0019b-udate+0(4) && '/' && ls_zsdt0019b-udate+4(2) && '/' && ls_zsdt0019b-udate+6(2).
      ENDIF.
      IF ls_zsdt0019b-zysjhq IS NOT INITIAL.
        lv_zysjhq = ls_zsdt0019b-zysjhq+0(4) && '/' && ls_zsdt0019b-zysjhq+4(2) && '/' && ls_zsdt0019b-zysjhq+6(2).
      ENDIF.
      IF ls_zsdt0019b-znbjhq IS NOT INITIAL.
        lv_znbjhq = ls_zsdt0019b-znbjhq+0(4) && '/' && ls_zsdt0019b-znbjhq+4(2) && '/' && ls_zsdt0019b-znbjhq+6(2).
      ENDIF.
      IF ls_zsdt0019b-zatpdfrq IS NOT INITIAL.
        lv_zatpdfrq = ls_zsdt0019b-zatpdfrq+0(4) && '/' && ls_zsdt0019b-zatpdfrq+4(2) && '/' && ls_zsdt0019b-zatpdfrq+6(2).
      ENDIF.
      IF ls_zsdt0019b-budat IS NOT INITIAL.
        lv_budat = ls_zsdt0019b-budat+0(4) && '/' && ls_zsdt0019b-budat+4(2) && '/' && ls_zsdt0019b-budat+6(2).
      ENDIF.
      IF ls_zsdt0019b-erdat IS NOT INITIAL.
        lv_erdat = ls_zsdt0019b-erdat+0(4) && '/' && ls_zsdt0019b-erdat+4(2) && '/' && ls_zsdt0019b-erdat+6(2).
      ENDIF.

      CONCATENATE lv_string
      ls_zsdt0019b-statdetail	gc_tab"订单状态
      ls_zsdt0019b-bu_sort1	gc_tab"实际客户检索项
      ls_zsdt0019b-saleman  gc_tab"实际业务员
      ls_zsdt0019b-name_text  gc_tab"制单人
      ls_zsdt0019b-bezei  gc_tab"订单类型描述
      ls_zsdt0019b-vbeln  gc_tab"单据号
      ls_zsdt0019b-posnr  gc_tab"行号
      ls_zsdt0019b-matwa  gc_tab"产品编号
      ls_zsdt0019b-maktx  gc_tab"产品名称
      lv_udate  gc_tab"二审日期
      lv_zysjhq	gc_tab"原始交货期
      lv_znbjhq	gc_tab"内部交货期
      lv_zatpdfrq	gc_tab"ATP答复日期
      ls_zsdt0019b-groes  gc_tab"规格型号
      ls_zsdt0019b-atwrt  gc_tab"包装规格
      lv_kwmeng	gc_tab"基本计量数量
      ls_zsdt0019b-meins  gc_tab"基本计量单位
      lv_kwmeng2  gc_tab"销售数量
      ls_zsdt0019b-vrkme  gc_tab"销售单位
      lv_kbetr  gc_tab"折前单价
      lv_netwr  gc_tab"价税合计
      lv_netwr2	gc_tab"不含税金额
      ls_zsdt0019b-ettyp  gc_tab"计划行类型
      ls_zsdt0019b-werks  gc_tab"工厂
      lv_budat  gc_tab"最晚入库日期
      lv_kalab  gc_tab"非限制库存
      lv_kains  gc_tab"其他库存
      lv_rfmng  gc_tab"出库量
      lv_invoice  gc_tab"开票量
      ls_zsdt0019b-text1  gc_tab"付款方式
      ls_zsdt0019b-wgbez  gc_tab"物料中类名称
      ls_zsdt0019b-kunnr_nam  gc_tab"售达方
      ls_zsdt0019b-kunnr_det  gc_tab"实际客户
      ls_zsdt0019b-vtweg  gc_tab"分销渠道
      lv_erdat  gc_crlf"创建日期
      INTO lv_string.
      CLEAR:ls_zsdt0019b.
    ENDLOOP.
  ELSEIF ls_zsdt0019a-job = '03'.
    DELETE lt_zsdt0019b WHERE vtweg <> ls_zsdt0019c-vtweg.
    DELETE lt_zsdt0019b WHERE erdat NOT IN s_erdat.
    DELETE lt_zsdt0019b WHERE udate NOT IN s_udate.
    IF lt_zsdt0019b IS INITIAL.
      flag = 'X'.
      EXIT.
    ENDIF.

    LOOP AT lt_zsdt0019b INTO ls_zsdt0019b.
      CLEAR:
      lv_kwmeng,
      lv_kwmeng2,
      lv_kbetr,
      lv_netwr,
      lv_rfmng,
      lv_kalab,
      lv_netwr2,
      lv_invoice,
      lv_kains,
      lv_udate,
      lv_zysjhq,
      lv_znbjhq,
      lv_zatpdfrq,
      lv_budat,
      lv_erdat.

      lv_kwmeng = ls_zsdt0019b-kwmeng.
      lv_kwmeng2 = ls_zsdt0019b-kwmeng2.
      lv_kbetr = ls_zsdt0019b-kbetr.
      lv_netwr = ls_zsdt0019b-netwr.
      lv_rfmng = ls_zsdt0019b-rfmng.
      lv_kalab = ls_zsdt0019b-kalab.
      lv_netwr2 = ls_zsdt0019b-netwr2.
      lv_invoice = ls_zsdt0019b-invoice.
      lv_kains = ls_zsdt0019b-kains.
      IF ls_zsdt0019b-udate IS NOT INITIAL.
        lv_udate = ls_zsdt0019b-udate+0(4) && '/' && ls_zsdt0019b-udate+4(2) && '/' && ls_zsdt0019b-udate+6(2).
      ENDIF.
      IF ls_zsdt0019b-zysjhq IS NOT INITIAL.
        lv_zysjhq = ls_zsdt0019b-zysjhq+0(4) && '/' && ls_zsdt0019b-zysjhq+4(2) && '/' && ls_zsdt0019b-zysjhq+6(2).
      ENDIF.
      IF ls_zsdt0019b-znbjhq IS NOT INITIAL.
        lv_znbjhq = ls_zsdt0019b-znbjhq+0(4) && '/' && ls_zsdt0019b-znbjhq+4(2) && '/' && ls_zsdt0019b-znbjhq+6(2).
      ENDIF.
      IF ls_zsdt0019b-zatpdfrq IS NOT INITIAL.
        lv_zatpdfrq = ls_zsdt0019b-zatpdfrq+0(4) && '/' && ls_zsdt0019b-zatpdfrq+4(2) && '/' && ls_zsdt0019b-zatpdfrq+6(2).
      ENDIF.
      IF ls_zsdt0019b-budat IS NOT INITIAL.
        lv_budat = ls_zsdt0019b-budat+0(4) && '/' && ls_zsdt0019b-budat+4(2) && '/' && ls_zsdt0019b-budat+6(2).
      ENDIF.
      IF ls_zsdt0019b-erdat IS NOT INITIAL.
        lv_erdat = ls_zsdt0019b-erdat+0(4) && '/' && ls_zsdt0019b-erdat+4(2) && '/' && ls_zsdt0019b-erdat+6(2).
      ENDIF.

      CONCATENATE lv_string
      ls_zsdt0019b-statdetail	gc_tab"订单状态
      ls_zsdt0019b-bu_sort1	gc_tab"实际客户检索项
      ls_zsdt0019b-saleman  gc_tab"实际业务员
      ls_zsdt0019b-name_text  gc_tab"制单人
      ls_zsdt0019b-bezei  gc_tab"订单类型描述
      ls_zsdt0019b-vbeln  gc_tab"单据号
      ls_zsdt0019b-posnr  gc_tab"行号
      ls_zsdt0019b-matwa  gc_tab"产品编号
      ls_zsdt0019b-maktx  gc_tab"产品名称
      lv_udate  gc_tab"二审日期
      lv_zysjhq	gc_tab"原始交货期
      lv_znbjhq	gc_tab"内部交货期
      lv_zatpdfrq	gc_tab"ATP答复日期
      ls_zsdt0019b-groes  gc_tab"规格型号
      ls_zsdt0019b-atwrt  gc_tab"包装规格
      lv_kwmeng	gc_tab"基本计量数量
      ls_zsdt0019b-meins  gc_tab"基本计量单位
      lv_kwmeng2  gc_tab"销售数量
      ls_zsdt0019b-vrkme  gc_tab"销售单位
      lv_kbetr  gc_tab"折前单价
      lv_netwr  gc_tab"价税合计
      lv_netwr2	gc_tab"不含税金额
      ls_zsdt0019b-ettyp  gc_tab"计划行类型
      ls_zsdt0019b-werks  gc_tab"工厂
      lv_budat  gc_tab"最晚入库日期
      lv_kalab  gc_tab"非限制库存
      lv_kains  gc_tab"其他库存
      lv_rfmng  gc_tab"出库量
      lv_invoice  gc_tab"开票量
      ls_zsdt0019b-text1  gc_tab"付款方式
      ls_zsdt0019b-wgbez  gc_tab"物料中类名称
      ls_zsdt0019b-kunnr_nam  gc_tab"售达方
      ls_zsdt0019b-kunnr_det  gc_tab"实际客户
      ls_zsdt0019b-vtweg  gc_tab"分销渠道
      lv_erdat  gc_crlf"创建日期
      INTO lv_string.
      CLEAR:ls_zsdt0019b.
    ENDLOOP.
  ENDIF.






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
        ev_size   = size ).
    CATCH cx_bcs.
      MESSAGE e445(so).
  ENDTRY.

ENDFORM.                    "create_content