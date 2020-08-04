FUNCTION z_mm_oa2sap_read_reserve.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     VALUE(INPUT) TYPE  ZMT_OA2SAP_READ_RESERVE
*"     VALUE(MESSAGE_ID) TYPE  SXMSGUID
*"  EXPORTING
*"     REFERENCE(OUTPUT) TYPE  ZMT_OA2SAP_READ_RESERVE_RET
*"----------------------------------------------------------------------
  TYPES:BEGIN OF ty_out,
          res_date     TYPE    rkpf-rsdat  , "  预订基准日期
          created_by   TYPE    rkpf-usnam  , "  用户名
          move_type    TYPE    rkpf-bwart  , "  移动类型
          costcenter   TYPE    rkpf-kostl  , "  成本中心
          atpcheck(1)  TYPE    c , "  标识, ATP 检查是否应该执行
          calcheck(1)  TYPE    c , "  标识，工厂日历是否应该检查
          orderid      TYPE    rkpf-aufnr  , "  内部订单
          bktxt        TYPE    rkpf-bktxt  , "  凭证抬头文本
          type(1)      TYPE    c, "消息类型
          message(220) TYPE    c, "消息文本
          material     TYPE    resb-matnr  , "  物料
          plant        TYPE    resb-werks  , "  工厂
          stge_loc     TYPE    resb-lgort  , "  库存地点
          batch        TYPE    resb-charg  , "  SAP批号
          entry_qnt    TYPE    resb-erfmg  , "  数量
          entry_uom    TYPE    resb-erfme  , "  单位
          enmng        TYPE    resb-enmng  , "  提货数
          gl_account   TYPE    resb-saknr  , "  总账科目
          sobkz        TYPE    resb-sobkz  , "  特殊库存
          kdauf        TYPE    resb-kdauf  , "  销售订单
          kdpos        TYPE    resb-kdpos  , "  销售订单项目
          item_text    TYPE    resb-sgtxt  , "  行项目备注
          gr_rcpt      TYPE    resb-wempf  , "  收货方
          unload_pt    TYPE    resb-ablad  , "  卸货点
          movement     TYPE    resb-xwaok  , "  允许移动
        END OF ty_out.
*  TYPES: BEGIN OF ty_option,
*           sign(1),
*           option(2),
*           low(10),
*           high(10),
*         END OF ty_option.
  TYPES: BEGIN OF ty_option,
           sign(1),
           option(2),
           low       TYPE c LENGTH 4,
           high      TYPE c LENGTH 4,
         END OF ty_option.
  TYPES: BEGIN OF ty_option2,
           sign(1),
           option(2),
           low       TYPE resb-matnr,
           high      TYPE resb-matnr,
         END OF ty_option2.
  TYPES: BEGIN OF ty_option3,
           sign(1),
           option(2),
           low       TYPE c LENGTH 8,
           high      TYPE c LENGTH 8,
         END OF ty_option3.
  DATA:s_werks TYPE TABLE OF ty_option WITH HEADER LINE.
  DATA:s_matnr TYPE TABLE OF ty_option2 WITH HEADER LINE.
  DATA:s_bdter TYPE TABLE OF ty_option3 WITH HEADER LINE.
  DATA lv_matnr(18).

  DATA:lt_out TYPE TABLE OF ty_out,
       ls_out LIKE LINE OF lt_out.

  DATA:lt_item TYPE zdt_oa2sap_read_reserve_re_tab,
       ls_item TYPE zdt_oa2sap_read_reserve_ret_li.

  DATA:ls_rkpf TYPE rkpf,
       lt_resb TYPE TABLE OF resb,
       ls_resb TYPE resb.



  "RKPF
  IF input-mt_oa2sap_read_reserve-input-usnam IS NOT INITIAL.
    SELECT SINGLE
    rsnum
    usnam
    rsdat
    usnam
    bwart
    kostl
    aufnr
    bktxt
    INTO CORRESPONDING FIELDS OF ls_rkpf
    FROM rkpf
    WHERE rsnum = input-mt_oa2sap_read_reserve-input-rsnum
      AND usnam = input-mt_oa2sap_read_reserve-input-usnam.
  ELSE.
    SELECT SINGLE
    rsnum
    usnam
    rsdat
    usnam
    bwart
    kostl
    aufnr
    bktxt
    INTO CORRESPONDING FIELDS OF ls_rkpf
    FROM rkpf
    WHERE rsnum = input-mt_oa2sap_read_reserve-input-rsnum.
  ENDIF.

  IF ls_rkpf IS NOT INITIAL.
    CLEAR output.

    output-mt_oa2sap_read_reserve_ret-res_date = ls_rkpf-rsdat.
    output-mt_oa2sap_read_reserve_ret-created_by = ls_rkpf-usnam.
    output-mt_oa2sap_read_reserve_ret-move_type = ls_rkpf-bwart.
    output-mt_oa2sap_read_reserve_ret-costcenter = ls_rkpf-kostl.
    output-mt_oa2sap_read_reserve_ret-atpcheck = 'X'.
    output-mt_oa2sap_read_reserve_ret-calcheck = 'X'.
    output-mt_oa2sap_read_reserve_ret-orderid = ls_rkpf-aufnr.
    output-mt_oa2sap_read_reserve_ret-bktxt = ls_rkpf-bktxt.
    output-mt_oa2sap_read_reserve_ret-message = '成功'.
    output-mt_oa2sap_read_reserve_ret-type = 'S'.

    "行项目条件数据处理
    CLEAR:s_werks,s_matnr,s_bdter.
    IF input-mt_oa2sap_read_reserve-input-werks IS NOT INITIAL.
      s_werks-sign = 'I'.
      s_werks-low = input-mt_oa2sap_read_reserve-input-werks.
      s_werks-option = 'EQ'.
      APPEND s_werks.
    ENDIF.
    IF input-mt_oa2sap_read_reserve-input-matnr IS NOT INITIAL.
      s_matnr-sign = 'I'.
      s_matnr-low = input-mt_oa2sap_read_reserve-input-matnr.
      s_matnr-low = zcl_bc_public=>conv_by_ddic( i_input = s_matnr-low ).
      s_matnr-option = 'EQ'.
      APPEND  s_matnr.
    ENDIF.
    IF input-mt_oa2sap_read_reserve-input-bdter IS NOT INITIAL.
      s_bdter-sign = 'I'.
      s_bdter-low = input-mt_oa2sap_read_reserve-input-bdter.
      s_bdter-option = 'EQ'.
      APPEND  s_bdter.
    ENDIF.

    "RESB
    SELECT
   resb~werks
   resb~matnr
   resb~bdter
   resb~matnr
   resb~werks
   resb~lgort
   resb~charg
   resb~erfmg
   resb~erfme
   resb~enmng
   resb~saknr
   resb~sobkz
   resb~kdauf
   resb~kdpos
   resb~sgtxt
   resb~wempf
   resb~ablad
   resb~xwaok
   resb~rsnum
    INTO CORRESPONDING FIELDS OF TABLE lt_resb
    FROM resb
    WHERE resb~rsnum = input-mt_oa2sap_read_reserve-input-rsnum
      AND resb~werks IN s_werks
      AND resb~matnr IN s_matnr
      AND resb~bdter IN s_bdter.
    IF lt_resb IS NOT INITIAL.
      LOOP AT lt_resb INTO ls_resb.
        CLEAR ls_item.
        ls_item-material = ls_resb-matnr.
        ls_item-plant = ls_resb-werks.
        ls_item-stge_loc = ls_resb-lgort.
        ls_item-batch = ls_resb-charg.
        ls_item-entry_qnt = ls_resb-erfmg.
        ls_item-entry_uom = ls_resb-erfme.
        ls_item-enmng = ls_resb-enmng.
        ls_item-gl_account = ls_resb-saknr.
        ls_item-sobkz = ls_resb-sobkz.
        ls_item-kdauf = ls_resb-kdauf.
        ls_item-kdpos = ls_resb-kdpos.
        ls_item-item_text = ls_resb-sgtxt.
        ls_item-gr_rcpt = ls_resb-wempf.
        ls_item-unload_pt = ls_resb-ablad.
        ls_item-movement = ls_resb-xwaok.
        APPEND ls_item TO lt_item.
        CLEAR:ls_resb.
      ENDLOOP.
      output-mt_oa2sap_read_reserve_ret-list_of_others = lt_item.
    ELSE.
      output-mt_oa2sap_read_reserve_ret-type = 'W'.
      output-mt_oa2sap_read_reserve_ret-message = '预留单存在，但是所查询的条件不存在行项目信息'.
    ENDIF.

  ELSE.
    output-mt_oa2sap_read_reserve_ret-type = 'E'.
    output-mt_oa2sap_read_reserve_ret-message = '数据不存在或者存在异常喔亲'.
  ENDIF.

















ENDFUNCTION.