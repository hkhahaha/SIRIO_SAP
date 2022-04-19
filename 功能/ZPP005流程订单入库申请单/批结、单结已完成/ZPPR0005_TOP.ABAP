*&---------------------------------------------------------------------*
*& 包含               ZPPR0005_TOP
*&---------------------------------------------------------------------*

TABLES:afpo,afko, zppt0003,zspp034_head.
TYPES:t_aufnr TYPE TABLE OF afpo-aufnr.
TYPES t_rsdswhere TYPE TABLE OF rsdswhere.
CLASS zcl_pro_grreq DEFINITION DEFERRED.
CLASS zcl_pro_grreq_bill DEFINITION DEFERRED.
CLASS zcl_qr_oper DEFINITION DEFERRED.
CLASS zcl_qrmail_oper DEFINITION DEFERRED.
DATA: ok_code               TYPE sy-ucomm,
      save_code             TYPE sy-ucomm,
      gr_idat               TYPE RANGE OF aufk-idat2, "技术完成日期
      gr_zdelflg            TYPE RANGE OF zppt0003-zdelflg,
      gs_zspp034_head_old   TYPE zspp034_head,
      gt_zppt0003           TYPE TABLE OF zppt0003,
      gt_zppt0003_old       TYPE TABLE OF zppt0003,
      gv_meins              TYPE meins,
      gs_program            TYPE  salv_s_layout_key,
      go_pro_salv           TYPE REF TO cl_salv_table,
      go_docking            TYPE REF TO cl_gui_docking_container,
      go_docking_mail       TYPE REF TO cl_gui_docking_container,
      gt_alv_fidcat         TYPE lvc_t_fcat,
      gt_alv_fidcat_mail    TYPE lvc_t_fcat,
      go_alv_grid_mail      TYPE REF TO cl_gui_alv_grid,
      go_alv_grid_create    TYPE REF TO cl_gui_alv_grid,
      gt_rowid              TYPE salv_t_row,
      go_zcl_pro_grreq      TYPE REF TO zcl_pro_grreq,
      go_zcl_pro_grreq_bill TYPE REF TO zcl_pro_grreq_bill,
      go_zcl_qr_oper        TYPE REF TO zcl_qr_oper,
      go_qrmail_oper        TYPE REF TO zcl_qrmail_oper,
      gs_layout_bill        TYPE lvc_s_layo,
      gs_stable             TYPE lvc_s_stbl,
      gt_vrm_values         TYPE vrm_values,
      gv_msg                TYPE bapi_msg.
DATA:BEGIN OF gs_zppt0003_list.
       INCLUDE TYPE zppt0003.
       DATA: matkl  TYPE mara-matkl, "物料组
       wgbez  TYPE t023t-wgbez, "物料组描述
       zsfxbz TYPE vbap-zsfxbz,
       vtext  TYPE tvk1t-vtext,
       upd    TYPE flag. "修改标识
DATA:END OF gs_zppt0003_list.
"二维码扫描权限对象
DATA:BEGIN OF gs_zqrscanauth,
       query_flag	 TYPE flag, "查询
       upload_flag TYPE flag, "上传
       audit_flag  TYPE flag, "审核
       empty_flag  TYPE flag, "清空文本
       hege_flag   TYPE flag, "检查放行
       save_flag   TYPE flag, "发送邮件及保存
     END OF gs_zqrscanauth.
TYPES ty_tab_zppt0003_list LIKE TABLE OF gs_zppt0003_list.
DATA gt_zppt0003_list      LIKE TABLE OF gs_zppt0003_list.
DATA gs_zppt0003_list_temp      LIKE gs_zppt0003_list.
FIELD-SYMBOLS:<fs_zppt0003_list> LIKE  gs_zppt0003_list.
*流程订单列表ALV输出结构体
DATA :BEGIN OF gs_pro_output,
        exception TYPE char1,   "信号灯字段-处理结果
        errtxt    TYPE bapi_msg, "错误消息文本
        dwerk     TYPE afpo-dwerk, " 工厂
        dauat     TYPE afpo-dauat, " 订单类型
        aufnr     TYPE afpo-aufnr, " 订单号
        ktext     TYPE aufk-ktext, " 描述
*        rsnum     TYPE afko-rsnum, " 预留/相关需求的编号
        matnr     TYPE afpo-matnr, " 物料
        maktx     TYPE makt-maktx, "物料描述
        matkl     TYPE mara-matkl, "物料组
        wgbez     TYPE t023t-wgbez, "物料组描述
        mtart     TYPE mara-mtart, "物料类型
        meins     TYPE afpo-meins, " 基本计量单位
        untto     TYPE afpo-untto, "交货不足限度
        uebto     TYPE afpo-uebto, "过量交货限度
        psmng     TYPE afpo-psmng, " 订单数量
        igmng     TYPE afko-igmng, " 确认的产量
        wemng     TYPE afpo-wemng, " 收货数量
        lgort     TYPE afpo-lgort,  "仓库
        lgobe     TYPE t001l-lgobe, "仓库描述
        kdauf     TYPE afpo-kdauf, " 销售凭证
        kdpos     TYPE afpo-kdpos, " 销售凭证项目
        zypsl     TYPE vbap-zypsl,  "取样量
        elikz     TYPE afpo-elikz, " 交货已完成
        gstrp     TYPE afko-gstrp, " 基本开始日期
        gltrp     TYPE afko-gltrp, " 基本完成日期
        ftrmi     TYPE afko-ftrmi, " 实际下达日期
        idat2     TYPE aufk-idat2, "技术关闭日期
        charg     TYPE afpo-charg, " 批次
        wempf     TYPE afpo-wempf, " 交货日期
        ablad     TYPE afpo-ablad, " 车间
        dispo     TYPE afko-dispo, " MRP控制者
        fevor     TYPE afko-fevor, " 生产管理员
        objnr     TYPE aufk-objnr, " 对象号
        sttxt     TYPE caufvd-sttxt, " 订单状态
*add S4DK904849 start
*        ZJINTGEW  TYPE ZPPT0003-ZJINTGEW, " 净净重
*        ZNTGEW    TYPE ZPPT0003-ZNTGEW, " 净重
*        ZBRGEW    TYPE ZPPT0003-ZBRGEW, " 毛重
*        ZWJNTGEW  TYPE ZPPT0003-ZWJNTGEW, " 尾数箱净净重
*        ZWNTGEW   TYPE ZPPT0003-ZWNTGEW, " 尾数箱净重
*        ZWBRGEW   TYPE ZPPT0003-ZWBRGEW, " 尾数箱毛重
        gewei     TYPE mara-gewei, " 重量单位
*add S4DK904849	end
      END OF gs_pro_output.

DATA :BEGIN OF gs_charg,
        aufnr TYPE afpo-aufnr, " 订单号
        matnr TYPE afpo-matnr,
        charg TYPE afpo-charg, " 批次
        hsdat TYPE mcha-hsdat,  "生产日期
        xchpf TYPE marc-xchpf,  "物料批次管理(工厂)
      END OF gs_charg.

DATA:BEGIN OF gs_matnr,
       matnr TYPE mara-matnr,
       meins TYPE mara-meins, " 基本计量单位
     END OF gs_matnr.
DATA:BEGIN OF gs_zpspe,
       matnr  TYPE mara-matnr,
       zpspe  TYPE zppt0003-zpspe,
       zumren TYPE zppt0003-zumren,
     END OF    gs_zpspe.

DATA:BEGIN OF gs_lgort,
       lgort TYPE t001l-lgort,
       lgobe TYPE t001l-lgobe,
     END OF gs_lgort.

DATA:BEGIN OF gs_kdauf,
       kdauf TYPE afpo-kdauf, " 销售凭证
       kdpos TYPE afpo-kdpos, " 销售凭证项目
     END OF gs_kdauf.
DATA:BEGIN OF gs_qrmail.
       INCLUDE TYPE zppt0009.
       DATA:zupd TYPE zupd.
DATA END OF gs_qrmail.

DATA:BEGIN OF gs_close,
       vbeln TYPE vbep-vbeln,
       posnr TYPE vbep-posnr,
       etenr TYPE vbep-etenr,
     END OF gs_close.
DATA gt_close LIKE TABLE OF gs_close.

DATA: gt_pro_output    LIKE TABLE OF gs_pro_output,
      gt_matnr         LIKE TABLE OF gs_matnr,
      gt_t001l         LIKE TABLE OF gs_lgort,
      gt_kdauf         LIKE TABLE OF gs_kdauf,
      gt_charg         LIKE TABLE OF gs_charg,
      gt_zppt0009_save TYPE TABLE OF zppt0009,
      gt_qrmail        LIKE TABLE OF gs_qrmail,
      gt_sqlwhere      TYPE TABLE OF rsdswhere.
FIELD-SYMBOLS <fs_qrmail> LIKE gs_qrmail.
FIELD-SYMBOLS <fs_zppt0003> TYPE zppt0003.

DATA:gt_zppt0003_lista     LIKE TABLE OF gs_zppt0003_list,
     gt_zppt0003_listb     LIKE TABLE OF gs_zppt0003_list,
     gt_zppt0003_list_temp LIKE TABLE OF gs_zppt0003_list,
     gt_zppt0003_list_all  LIKE TABLE OF gs_zppt0003_list.


DATA:gt_zppt0003_list_del LIKE TABLE OF gs_zppt0003_list,
     gt_zppt0003_list_add LIKE TABLE OF gs_zppt0003_list,
     gt_zppt0003_list_mod LIKE TABLE OF gs_zppt0003_list.
"下发LIMS请求消息结构
*DATA:BEGIN OF gs_lims_request,
*       prueflos TYPE qals-prueflos, "检验批次
*       aufnr    TYPE qals-aufnr, "流程订单号
*       wempf    TYPE zppt0003-wempf, "批结信息
*       zwmsmng  TYPE zppt0003-zwmsmng, "WMS收货数量
*       zmeins_t TYPE zppt0003-meins,
*     END OF gs_lims_request.
*"下发LIMS返回消息结构
*DATA:BEGIN OF gs_lims_response,
*       ret_code TYPE char1, "标记 S-成功，E-失败
*       ret_msg  TYPE char200, "消息文本
*     END OF gs_lims_response.

DATA: salesdocument      LIKE  bapivbeln-vbeln,
      order_header_in    LIKE  bapisdh1,
      order_header_inx   LIKE  bapisdh1x,


      order_item_in      LIKE  bapisditm  OCCURS 0 WITH HEADER LINE,
      ls_order_item_in   LIKE LINE OF order_item_in,
      order_item_inx     LIKE  bapisditmx OCCURS 0 WITH HEADER LINE,
      ls_order_item_inx  LIKE LINE OF order_item_inx,
      return1            LIKE  bapiret2  OCCURS 0 WITH HEADER LINE,
      ls_return1         LIKE  bapiret2,
      return             LIKE  bapiret2  OCCURS 0 WITH HEADER LINE,
      schedule_lines     LIKE  bapischdl  OCCURS 0 WITH HEADER LINE,
      ls_schedule_lines  LIKE LINE OF schedule_lines,
      schedule_linesx    LIKE  bapischdlx OCCURS 0 WITH HEADER LINE,
      ls_schedule_linesx LIKE LINE OF schedule_linesx.