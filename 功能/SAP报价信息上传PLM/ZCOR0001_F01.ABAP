*&---------------------------------------------------------------------*
*& 包含               ZCOR0001_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& 包含               ZPPR0003_F01
*&---------------------------------------------------------------------*
**根据查询条件从数据库获取数据
FORM frm_get_data.
    SELECT keko~matnr,     "物料编码
           makt~maktx,    "物料描述
           keko~werks,     "工厂编码
           t001w~name1,    "工厂描述
           keko~kadky,     "成本核算日期
           keko~kalnr,    "成本估算编号
           keko~bwvar,    "估算变式编号
           keko~losgr,    "成本核算批量
           tck06~txbwv,    "估算变式描述
           ckis~kstar,    "成本要素
           ckis~wertn    "成本要素金额
     INTO TABLE @DATA(lt_list)
     FROM keko
     INNER JOIN ckis ON  keko~kalnr = ckis~kalnr AND keko~kalka = ckis~kalka
                     AND keko~kadky = ckis~kadky AND keko~tvers = ckis~tvers AND keko~bwvar = ckis~bwvar
     INNER JOIN  t001w ON keko~werks = t001w~werks
     INNER JOIN makt ON keko~matnr = makt~matnr AND makt~spras = 1
     INNER JOIN tck06 ON keko~bwvar = tck06~bwvar AND tck06~spras = 1
     WHERE keko~matnr IN @s_matnr
           AND keko~werks = @p_werks
           AND keko~bwdat IN @s_bwdat
          AND keko~bwvar = @p_bwvar.
  
    IF lt_list IS NOT INITIAL.
  
      DATA:lt_temp_key  LIKE lt_list,
           gpreis1_temp LIKE ckis-wertn,
           gpreis2_temp LIKE ckis-wertn,
           gpreis3_temp LIKE ckis-wertn,
           gpreis4_temp LIKE ckis-wertn,
           gpreis5_temp LIKE ckis-wertn,
           fs_flag(1)   TYPE c.
  
  
      CLEAR:lt_temp_key.
  
      lt_temp_key = lt_list.
  
      SORT lt_list BY matnr werks kadky kalnr bwvar kstar losgr.
      SORT lt_temp_key BY matnr werks kadky kalnr bwvar losgr.
      DELETE ADJACENT DUPLICATES FROM lt_temp_key COMPARING matnr werks kadky kalnr bwvar losgr.
  
      LOOP AT lt_temp_key ASSIGNING FIELD-SYMBOL(<fs_head>).
  
        CLEAR:ls_alv,gpreis1_temp,gpreis2_temp,gpreis3_temp,gpreis4_temp,gpreis5_temp,fs_flag,ls_celltab.
        REFRESH lt_celltab.
  
        "ls_alv-matnr = <fs_head>-matnr. "物料编号
        CALL FUNCTION 'CONVERSION_EXIT_MATN1_OUTPUT'
          EXPORTING
            input  = <fs_head>-matnr
          IMPORTING
            output = ls_alv-matnr.
  
  
        ls_alv-maktx = <fs_head>-maktx. "物料描述
        ls_alv-werks = <fs_head>-werks. "工厂编号
        ls_alv-name1 = <fs_head>-name1. "工厂描述
        ls_alv-kadky = <fs_head>-kadky. "成本核算日期
        "ls_alv-kalnr = <fs_head>-kalnr. "成本估算编号
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
          EXPORTING
            input  = <fs_head>-kalnr
          IMPORTING
            output = ls_alv-kalnr.
  
  
        ls_alv-bwvar = <fs_head>-bwvar. "估算变式编号
        ls_alv-txbwv = <fs_head>-txbwv. "估算变式描述
        ls_alv-losgr = <fs_head>-losgr. "成本核算批量
  
  
        "读取已发送成本估价的记录
        SELECT SINGLE flag zz001 gpreis1 gpreis2 gpreis3 gpreis4 gpreis5 uname cpudt
               INTO ( ls_alv-flag,ls_alv-zz001,ls_alv-gpreis1,ls_alv-gpreis2,ls_alv-gpreis3,ls_alv-gpreis4,ls_alv-gpreis5,ls_alv-uname,ls_alv-cpudt )
                FROM zcot0001 WHERE matnr = <fs_head>-matnr AND werks = <fs_head>-werks
                              AND kadky = <fs_head>-kadky  AND kalnr = <fs_head>-kalnr .
        IF sy-subrc = 0.
          ls_alv-message = ls_alv-uname && ' 已于: ' &&  ls_alv-cpudt && ' 发送 '.
          ls_celltab-fieldname = 'ZCHECKED'.
          ls_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
          INSERT ls_celltab INTO TABLE lt_celltab.
          INSERT LINES OF lt_celltab INTO TABLE ls_alv-celltab.
          APPEND ls_alv TO gt_output.
  
        ELSE.
  
  
          "计算成本(原料/包材)
          LOOP AT lt_list ASSIGNING FIELD-SYMBOL(<fs_item_1>) WHERE matnr = <fs_head>-matnr AND werks = <fs_head>-werks AND kadky = <fs_head>-kadky  AND kalnr = <fs_head>-kalnr
                                              AND  bwvar = <fs_head>-bwvar  AND  losgr = <fs_head>-losgr
                                              AND  ( kstar = '5001010000' OR kstar = '5001020000' OR kstar = '5001030000' ).
            gpreis1_temp = gpreis1_temp + <fs_item_1>-wertn.
  
          ENDLOOP.
          ls_alv-gpreis1 = gpreis1_temp.
  
  
          "计算直接人工
          LOOP AT lt_list ASSIGNING FIELD-SYMBOL(<fs_item_2>) WHERE matnr = <fs_head>-matnr AND werks = <fs_head>-werks AND kadky = <fs_head>-kadky  AND kalnr = <fs_head>-kalnr
                                              AND  bwvar = <fs_head>-bwvar  AND  losgr = <fs_head>-losgr
                                              AND  kstar = '8430000001'.
            gpreis2_temp = gpreis2_temp + <fs_item_2>-wertn.
  
          ENDLOOP.
          ls_alv-gpreis2 = gpreis2_temp.
  
          "计算折旧
          LOOP AT lt_list ASSIGNING FIELD-SYMBOL(<fs_item_3>) WHERE matnr = <fs_head>-matnr AND werks = <fs_head>-werks AND kadky = <fs_head>-kadky  AND kalnr = <fs_head>-kalnr
                                              AND  bwvar = <fs_head>-bwvar AND  losgr = <fs_head>-losgr
                                              AND  kstar = '8430000002'.
            gpreis3_temp = gpreis3_temp + <fs_item_3>-wertn.
  
          ENDLOOP.
          ls_alv-gpreis3 = gpreis3_temp.
  
          "计算能耗
          LOOP AT lt_list ASSIGNING FIELD-SYMBOL(<fs_item_4>) WHERE matnr = <fs_head>-matnr AND werks = <fs_head>-werks AND kadky = <fs_head>-kadky  AND kalnr = <fs_head>-kalnr
                                              AND  bwvar = <fs_head>-bwvar  AND  losgr = <fs_head>-losgr
                                              AND  kstar = '8430000003'.
            gpreis4_temp = gpreis4_temp + <fs_item_4>-wertn.
  
          ENDLOOP.
          ls_alv-gpreis4 = gpreis4_temp.
  
          "计算制造费用
          LOOP AT lt_list ASSIGNING FIELD-SYMBOL(<fs_item_5>) WHERE matnr = <fs_head>-matnr AND werks = <fs_head>-werks AND kadky = <fs_head>-kadky  AND kalnr = <fs_head>-kalnr
                                              AND  bwvar = <fs_head>-bwvar  AND  losgr = <fs_head>-losgr
                                              AND  kstar = '8430000004'.
            gpreis5_temp = gpreis5_temp + <fs_item_5>-wertn.
  
          ENDLOOP.
          ls_alv-gpreis5 = gpreis5_temp.
  
          ls_alv-flag = ''.
          ls_alv-message = ''.
  
          ls_celltab-fieldname = 'ZCHECKED'.
          ls_celltab-style = cl_gui_alv_grid=>mc_style_enabled.
          INSERT ls_celltab INTO TABLE lt_celltab.
          INSERT LINES OF lt_celltab INTO TABLE ls_alv-celltab.
  
          APPEND ls_alv TO gt_output.
  
        ENDIF.
  
      ENDLOOP.
  
    ELSE.
  
      MESSAGE '不存在满足条件的数据' TYPE 'E'.
  
    ENDIF.
  
  ENDFORM.
  ** 获取APS与SAP的资源对照表
  FORM frm_get_zz001.
    SELECT matnr,werks,stlnr,stlal,valid_from_date,zz001
       FROM zppt0010
       ORDER BY matnr,werks,zz001
       INTO CORRESPONDING FIELDS OF TABLE @gt_help.
  ENDFORM.
  
  *** 处理数据，扩充其他字段
  FORM frm_process_data.
  
  ENDFORM.
  
  *** ALV界面布局
  FORM frm_set_layout.
    gs_disvariant-report = sy-repid.
    gs_disvariant-username = sy-uname.
    gs_layout-cwidth_opt = abap_true.   "自动调节字段长度
    gs_layout-sel_mode = 'D'.  "选择模式：可选择多行
    gs_layout-stylefname = 'CELLTAB'.  "ALV 控制: 单元格的类型表
    "gs_layout-excp_fname =  'EXCEP'. "EXCEPTION 字段
  ENDFORM.
  
  
  *** 设置字段目录-MAIN ALV
  FORM frm_set_fieldcatalog.
    DATA lv_structure_name TYPE tabname.
  
    lv_structure_name  = 'ZSCOR001'.
  
    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
        i_structure_name       = lv_structure_name
      CHANGING
        ct_fieldcat            = gt_fieldcatalog
      EXCEPTIONS
        inconsistent_interface = 1
        program_error          = 2
        OTHERS                 = 3.
  
    gt_fldcat_detail = gt_fieldcatalog."复制给ALV Detail的字段目录
    LOOP AT gt_fieldcatalog ASSIGNING FIELD-SYMBOL(<fs>).
      CASE <fs>-fieldname.
        WHEN 'ZCHECKED'.
          <fs>-fix_column = abap_true.
          <fs>-edit = abap_true.
          <fs>-checkbox = abap_true.
          <fs>-coltext = '选择'.
        WHEN 'MATNR'.
          <fs>-fix_column = abap_true.
          <fs>-coltext = '物料编号'.
          "<fs>-hotspot = abap_true.
        WHEN 'MAKTX'.
          <fs>-fix_column = abap_true.
          <fs>-coltext = '物料描述'.
        WHEN 'WERKS'.
          <fs>-fix_column = abap_true.
          <fs>-coltext = '工厂编号'.
        WHEN 'NAME1'.
          <fs>-fix_column = abap_true.
          <fs>-coltext = '工厂描述'.
  
        WHEN 'KADKY'.
          <fs>-coltext = '成本核算日期'.
        WHEN 'KALNR'.
          <fs>-coltext = '成本估算号'.
        WHEN 'BWVAR'.
          <fs>-coltext = '估价变式编号'.
        WHEN 'TXBWV'.
          <fs>-coltext = '估价变式名称'.
        WHEN 'LOSGR'.
          <fs>-coltext = '批量'.
        WHEN 'ZZ001'.
          <fs>-coltext = '需求报价单号'.
          <fs>-edit = abap_true.
        WHEN 'GPREIS1'.
          <fs>-coltext = '成本(原料/包材)'.
        WHEN 'GPREIS2'.
          <fs>-coltext = '直接人工'.
        WHEN 'GPREIS3'.
          <fs>-coltext = '折旧'.
        WHEN 'GPREIS4'.
          <fs>-coltext = '能耗'.
        WHEN 'GPREIS5'.
          <fs>-coltext = '制造费用'.
        WHEN 'FLAG'.
          <fs>-coltext = '发送标识'.
        WHEN 'MESSAGE'.
          <fs>-coltext = '消息'.
        WHEN 'UNAME'.
          <fs>-coltext = '用户'.
        WHEN 'CPUDT'.
          <fs>-coltext = '时间戳'.
        WHEN OTHERS.
  
      ENDCASE.
    ENDLOOP.
  ENDFORM.
  
  
  FORM frm_batch_conf_input .
    PERFORM frm_get_zz001.
  ENDFORM.
  *&---------------------------------------------------------------------*
  *& Form FRM_CREATE_DETAIL_LIST
  *&---------------------------------------------------------------------*
  *& 创建APS机台批量报工dialogbox_container
  *&---------------------------------------------------------------------*
  *& -->  p1        text
  *& <--  p2        text
  *&---------------------------------------------------------------------*
  FORM frm_create_detail_list USING pv_grid_title.
  
  ENDFORM.
  *&---------------------------------------------------------------------*
  *& Form FRM_SAVE_DATA
  *&---------------------------------------------------------------------*
  *& 创建报工单
  *&---------------------------------------------------------------------*
  *& -->  p1        text
  *& <--  p2        text
  *&---------------------------------------------------------------------*
  FORM frm_save_data .
    DATA:lo_api          TYPE REF TO zco_si_sap2plm_quotation_to_pl,
         lo_system_fault TYPE REF TO cx_ai_system_fault,
         g_message_id    TYPE sxmsguid,
         lv_guid(32)     TYPE c,   " GUID
         ls_output       TYPE zmt_sap2plm_quotation_to_plm,
         ls_input        TYPE zmt_sap2plm_quotation_to_plm_r,
         tabix           TYPE sy-tabix, "ABAP 系统字段
         lt_zcot0001     LIKE zcot0001.
  
    DATA: date1        TYPE sy-datum,
          date2        TYPE  sy-uzeit,
          lv_cpudt(20) TYPE c.
    MOVE sy-datum TO date1 .
    MOVE sy-uzeit TO date2 .
    CONCATENATE date1  ' ' date2  INTO lv_cpudt.
  
  
  
    LOOP AT gt_output INTO ls_alv WHERE zchecked = 'X' AND flag <> 'Y'.
      CLEAR:ls_output,ls_input,lv_guid,g_message_id,lt_zcot0001,ls_celltab.
  
      REFRESH lt_celltab.
  
      CALL FUNCTION 'RS_DME_MG_GENERATE_GUID'
        IMPORTING
  *       E_GUID_16 =
          e_guid_32 = lv_guid.
  
      ls_output-mt_sap2plm_quotation_to_plm-guid = lv_guid."GUID
      ls_output-mt_sap2plm_quotation_to_plm-uname = sy-uname."UNAME
      ls_output-mt_sap2plm_quotation_to_plm-cn_rfq_code = ls_alv-zz001."报价单号
      ls_output-mt_sap2plm_quotation_to_plm-cn_material = ls_alv-matnr. "物料
      ls_output-mt_sap2plm_quotation_to_plm-cn_part_cost = ls_alv-gpreis1.  "成本(原料/包材)
      ls_output-mt_sap2plm_quotation_to_plm-cn_direct_labor = ls_alv-gpreis2.   "直接人工
      ls_output-mt_sap2plm_quotation_to_plm-cn_depreciation = ls_alv-gpreis3.             "折旧
      ls_output-mt_sap2plm_quotation_to_plm-cn_energy_consumption = ls_alv-gpreis4.      "能耗
      ls_output-mt_sap2plm_quotation_to_plm-cn_manufacturing_costs = ls_alv-gpreis5.     "制造费用
  
      TRY.
          CREATE OBJECT lo_api.
          CALL METHOD lo_api->si_sap2plm_quotation_to_plm_ou
            EXPORTING
              output = ls_output
            IMPORTING
              input  = ls_input.
  
          COMMIT WORK AND WAIT.
        CATCH cx_ai_system_fault INTO lo_system_fault.
          "lv_error = 'E'.
          ls_alv-flag = 'E'.
          ls_alv-message = '发送失败，PI接口错误！'.
          " 输出参数记录日志
          zcl_bc_public=>write_log( iv_logid = CONV #( g_message_id )
                                    iv_intid = 'FI061' iv_ptype = 'E'
                                    is_data = ls_output ).
  
          ls_celltab-fieldname = 'ZCHECKED'.
          ls_celltab-style = cl_gui_alv_grid=>mc_style_enabled.
          INSERT ls_celltab INTO TABLE lt_celltab.
          REFRESH ls_alv-celltab.
          INSERT LINES OF lt_celltab INTO TABLE ls_alv-celltab.
      ENDTRY.
  
  
  
      " 输出参数记录日志
      IF ls_input-mt_sap2plm_quotation_to_plm_re-type = 'S'.
  
        zcl_bc_public=>write_log( iv_logid = CONV #( g_message_id )
                                  iv_intid = 'FI061' iv_ptype = 'S'
                                  is_data = ls_output ).
  
        lt_zcot0001-matnr	=	ls_alv-matnr.	"物料编号
        lt_zcot0001-werks	=	ls_alv-werks.	"	工厂
        lt_zcot0001-kadky	=	ls_alv-kadky.	"	成本核算日期 (码)
        lt_zcot0001-kalnr	=	ls_alv-kalnr.	"	成本估算号（单位成本）
        lt_zcot0001-zz001	=	ls_alv-zz001.	"	需求报价单号
        lt_zcot0001-flag  = 'Y'.  " 发送标识
        lt_zcot0001-gpreis1	=	ls_alv-gpreis1.	"	成本(原料/包材)
        lt_zcot0001-gpreis2	=	ls_alv-gpreis2.	"	直接人工
        lt_zcot0001-gpreis3	=	ls_alv-gpreis3.	"	折旧
        lt_zcot0001-gpreis4	=	ls_alv-gpreis4.	"	能耗
        lt_zcot0001-gpreis5	=	ls_alv-gpreis5.	"	制造费用
        lt_zcot0001-uname	=	sy-uname.	"	发送用户
  
        lt_zcot0001-cpudt	=	lv_cpudt.	"	发送日期及时间
  
        INSERT zcot0001 FROM lt_zcot0001.
  
        ls_alv-flag = 'Y'.
        ls_alv-message = '发送成功！'.
        ls_alv-uname = sy-uname.  " 发送用户
        ls_alv-cpudt  = lv_cpudt. " 发送日期及时间
  
        ls_celltab-fieldname = 'ZCHECKED'.
        ls_celltab-style = cl_gui_alv_grid=>mc_style_enabled.
        INSERT ls_celltab INTO TABLE lt_celltab.
        REFRESH ls_alv-celltab.
        INSERT LINES OF lt_celltab INTO TABLE ls_alv-celltab.
  
      ELSE.
        ls_alv-flag = 'E'.
        ls_alv-message = ls_input-mt_sap2plm_quotation_to_plm_re-message.
        ls_alv-uname = sy-uname.  " 发送用户
        ls_alv-cpudt  = lv_cpudt. " 发送日期及时间
        zcl_bc_public=>write_log( iv_logid = CONV #( g_message_id )
                                iv_intid = 'FI061' iv_ptype = 'E'
                                is_data = ls_output ).
  
        ls_celltab-fieldname = 'ZCHECKED'.
        ls_celltab-style = cl_gui_alv_grid=>mc_style_enabled.
        INSERT ls_celltab INTO TABLE lt_celltab.
        REFRESH ls_alv-celltab.
        INSERT LINES OF lt_celltab INTO TABLE ls_alv-celltab.
  
      ENDIF.
  
      MODIFY gt_output FROM ls_alv  TRANSPORTING flag message uname cpudt celltab.
  
    ENDLOOP.
  
    go_alv_grid_main->refresh_table_display( is_stable = gs_stable ).
  ENDFORM.
  *&---------------------------------------------------------------------*
  *& Form FRM_AUTHORITY_CHECK
  *&---------------------------------------------------------------------*
  *& 权限检查
  *&---------------------------------------------------------------------*
  *& -->  p1        text
  *& <--  p2        text
  *&---------------------------------------------------------------------*
  FORM frm_authority_check .
  *  AUTHORITY-CHECK OBJECT 'C_AFRU_AWK'
  *  ID 'ACTVT' FIELD 'xxxxxxxx'
  *   ID 'WERKS' FIELD p_werks.
  *  ID 'AUFART' FIELD 'xxxxxxxx'.
  
  
     AUTHORITY-CHECK OBJECT 'M_MATE_WRK'
   "   ID 'ACTVT' FIELD 'xxxxxxxx'
      ID 'WERKS' FIELD p_werks.
  
  
    IF sy-subrc <> 0.
      MESSAGE e001(00) WITH '您没有' p_werks '工厂的权限'.
    ENDIF.
  
  ENDFORM.