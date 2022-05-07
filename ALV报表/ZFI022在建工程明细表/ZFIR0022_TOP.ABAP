*&---------------------------------------------------------------------*
*& 包含               ZFIR0022_TOP
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& 结构定义
*&---------------------------------------------------------------------*
TYPES:BEGIN OF ty_data,
        gjahr       TYPE gjahr, "年度
        monat       TYPE monat, "账期
        aufnr       TYPE aufnr, "内部订单号
        contract_no TYPE char20, "合同号
        ebeln       TYPE ebeln, "采购订单号
        ebelp       TYPE ebelp, "采购订单行项目
        txz01       TYPE txz01, "工程内容
        lifnr       TYPE lifnr, "供应商编号
        mcod1       TYPE lfa1-mcod1, "供应商
        brtwr       TYPE bbwert, "总金额
        dmbtr       TYPE dmbtr, "付款金额
        budat       TYPE budat, "付款时间
        rmwwr       TYPE rmwwr, "已收发票金额
        xblnr       TYPE xblnr, "发票号
        sgtxt       TYPE sgtxt, "行项目文本
        belnr       TYPE belnr_d, "付款凭证号
        zbelnr      TYPE belnr_d, "发票凭证号

        koart       TYPE koart,
        augbl       TYPE augbl,
        zuonr       TYPE dzuonr,
        ktosl       TYPE ktosl,
        h_blart     TYPE blart,
        shkzg       TYPE shkzg,
        awtyp       TYPE awtyp,
        awkey       TYPE awkey,
        bukrs       TYPE bukrs,


      END OF ty_data.
TYPES:BEGIN OF ty_lifnr,
        mcod1 TYPE   lfa1-mcod1,
        lifnr TYPE lfa1-lifnr,
      END OF ty_lifnr.
TYPES:BEGIN OF ty_augbl,
        augbl TYPE bseg-augbl,
      END OF ty_augbl.

*&---------------------------------------------------------------------*
*& 内表定义
*&---------------------------------------------------------------------*
DATA:  gt_data TYPE TABLE OF ty_data.
*&---------------------------------------------------------------------*
*& 变量定义
*&---------------------------------------------------------------------*
DATA:gv_lifnr TYPE lifnr,
     gv_ebeln TYPE ebeln,
     gv_aufnr TYPE aufnr,
     gv_aufgr TYPE aufgr,
     gv_gjahr TYPE cjahr.
DATA:gt_fieldcat TYPE lvc_t_fcat,
     gs_fieldcat TYPE lvc_s_fcat.
DATA:gs_layout TYPE lvc_s_layo.