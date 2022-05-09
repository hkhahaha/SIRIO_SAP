*----------------------------------------------------------------------*
***INCLUDE LZPP016F02.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form FRM_MAINTAIN_BOM
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> I_INPUT
*&      <-- LV_RET_MSG
*&---------------------------------------------------------------------*
FORM frm_maintain_bom USING    u_input TYPE zdt_plm2sap_createchange_bom_2
                       CHANGING c_change_no TYPE aenrb-aennr
                                c_ret_msg TYPE zdt_plm2sap_createchange_bom_1.

  DATA: lt_stpo      TYPE tt_stpo_in,  "  BOM行项目表
        ls_stpo      LIKE LINE OF lt_stpo,
        ls_stko      LIKE stko_api01,           "  BOM抬头
        ls_stko_o    LIKE stko_api02,
        lt_stpo_new  TYPE TABLE OF stpo_api03,
        ls_stpo_new  LIKE LINE OF lt_stpo_new,
        lt_stpo_old  TYPE TABLE OF stpo_api03,
        lt_stpo_old1 TYPE TABLE OF stpo_api03,
        lt_stpo_o1   TYPE TABLE OF stpo_api02,
        lt_stko      TYPE TABLE OF stko_api02,
        ls_stko1     TYPE stko_api02,
        lt_stpo_chk  TYPE TABLE OF ty_stpo_chk,
        ls_stpo_chk  TYPE ty_stpo_chk,
        ls_stpo_old  TYPE stpo_api03.
  DATA: l_acflag          TYPE c,
        l_fl_new_item     TYPE csdata-xfeld,
        l_stlnr           TYPE stko-stlnr,
        l_valid_from_flag TYPE c, "修改有效期自标识
        l_datum           TYPE csap_mbom-datuv,
        l_matnr           TYPE mara-matnr.
  CLEAR: c_ret_msg.
  CLEAR: g_matnr, g_stlal, g_plant, g_stlan, g_valid_from,
         l_valid_from_flag, l_fl_new_item.

  MOVE-CORRESPONDING u_input TO c_ret_msg.
  CLEAR: c_ret_msg-type, c_ret_msg-message.

  CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
    EXPORTING
      input        = u_input-material
    IMPORTING
      output       = g_matnr
    EXCEPTIONS
      length_error = 1
      OTHERS       = 2.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = u_input-stlal
    IMPORTING
      output = g_stlal.
  IF g_stlal IS INITIAL.
    g_stlal = '01'.
*  ELSE.
*    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*      EXPORTING
*        input  = u_input-stlal
*      IMPORTING
*        output = g_stlal.
  ENDIF.
  g_plant = u_input-plant.
  IF u_input-bom_usage IS INITIAL.
    g_stlan = '1'.
  ELSE.
    g_stlan = u_input-bom_usage.
  ENDIF.
  IF u_input-valid_from_date IS INITIAL.
    g_valid_from = sy-datum.
  ELSE.
    g_valid_from = u_input-valid_from_date. "  有效日期起
  ENDIF.
  g_flag = u_input-flag.
  PERFORM frm_check_input USING    u_input
                          CHANGING c_ret_msg.
  IF c_ret_msg-type EQ 'E'.
    RETURN.
  ENDIF.
  REFRESH: lt_stpo, lt_stpo_old, lt_stpo_new.
  CASE g_flag.

      "新增BOM
    WHEN 'A' OR 'C'.
      "BOM新数据获取处理
      PERFORM frm_bom_add_data TABLES lt_stpo USING u_input CHANGING ls_stko.

      CALL FUNCTION 'CSAP_MAT_BOM_READ'
        EXPORTING
          material    = g_matnr
          plant       = g_plant
          alternative = g_stlal
          bom_usage   = g_stlan
*         valid_from  = l_datum
        TABLES
          t_stpo      = lt_stpo_old[]
          t_stko      = lt_stko[]
        EXCEPTIONS
          error       = 1
          OTHERS      = 2.

      IF sy-subrc <> 0.
        l_acflag = 'A'.
      ELSE.
        l_acflag = 'C'.
        READ TABLE lt_stko INTO ls_stko1 INDEX 1.
        g_valid_from = ls_stko1-valid_from.
      ENDIF.
      IF c_change_no IS INITIAL AND l_acflag = 'C'.
        PERFORM frm_create_ecn CHANGING c_change_no.
      ENDIF.

      IF lt_stpo_old[] IS NOT INITIAL.
        APPEND LINES OF lt_stpo_old TO lt_stpo_old1.
        READ TABLE lt_stpo_old INTO ls_stpo_old INDEX 1.
        l_stlnr = ls_stpo_old-bom_no.
      ENDIF.
      LOOP AT lt_stpo_old INTO ls_stpo_old.
        CLEAR ls_stpo_chk.
        MOVE-CORRESPONDING ls_stpo_old TO ls_stpo_chk.
        "比较字段
        REPLACE ALL OCCURRENCES OF ',' IN ls_stpo_old-comp_qty WITH space.
        ls_stpo_chk-comp_qty1 = ls_stpo_old-comp_qty.
        ls_stpo_chk-comp_scrap1 = ls_stpo_old-comp_scrap.
        REPLACE ALL OCCURRENCES OF '.' IN ls_stpo_old-valid_from WITH space.
        ls_stpo_chk-valid_from1 = ls_stpo_old-valid_from.
        ls_stpo_chk-usage_prob1 = ls_stpo_old-usage_prob.
        APPEND ls_stpo_chk TO lt_stpo_chk.
      ENDLOOP.
      LOOP AT lt_stpo INTO ls_stpo.
        CLEAR: l_matnr.
        CALL FUNCTION 'CONVERSION_EXIT_MATN1_OUTPUT'
          EXPORTING
            input        = ls_stpo-component
          IMPORTING
            output       = l_matnr  "ls_stpo-component
          EXCEPTIONS
            length_error = 1
            OTHERS       = 2.
        READ TABLE lt_stpo_chk INTO ls_stpo_chk WITH KEY item_categ = ls_stpo-item_categ
                                                         item_no   = ls_stpo-item_no
                                                         component = l_matnr  "ls_stpo-component
                                                         comp_qty1 = ls_stpo-comp_qty1
                                                         comp_unit = ls_stpo-comp_unit
                                                         comp_scrap1 = ls_stpo-comp_scrap1
                                                         fixed_qty = ls_stpo-fixed_qty
                                                         sortstring = ls_stpo-sortstring
                                                         issue_loc = ls_stpo-issue_loc
*                                                         valid_from1 = ls_stpo-valid_from1
                                                         ai_group = ls_stpo-ai_group
                                                         ai_strateg = ls_stpo-ai_strateg
                                                         usage_prob1 = ls_stpo-usage_prob1
*                                                         rel_cost = ls_stpo-rel_cost 这个字段是自动计算的，不比较
                                                         rec_allowd = ls_stpo-rec_allowd.
        "因为行项目有修改即先删除再新建，故PLM无法知道SAP系统bom行项目的准确valid_from日期，
        "如果用户传过来的有效日期小于当前日期，那就认为是不修改
        "如果用户传过来的有效日期大于等于当前日期，而且和bom原来的有效日期相等，也是不需要修改
        IF sy-subrc = 0 AND ( ls_stpo-valid_from LT sy-datum OR
            ls_stpo-valid_from GE sy-datum AND ls_stpo-valid_from1 = ls_stpo_chk-valid_from1 ).
          DELETE lt_stpo_chk INDEX sy-tabix.
*          MOVE-CORRESPONDING ls_stpo_old TO ls_stpo_new.
*          MOVE-CORRESPONDING ls_stpo TO ls_stpo_new.
*          CLEAR: ls_stpo_new-fldelete.  "可能是把删除标记取消
*          ls_stpo_new-valid_to = '99991231'.  "原来的有效截止日期可能存在
**          ls_stpo_new-change_no = c_change_no.  "更改号放在行项目
*          APPEND ls_stpo_new TO lt_stpo_new.
*          IF ls_stpo_old-valid_from NE ls_stpo-valid_from AND ls_stpo-valid_from > sy-datum.
*            l_valid_from_flag = 'X'.
*          ENDIF.
        ELSE.
          CLEAR: ls_stpo_new.
          MOVE-CORRESPONDING ls_stpo TO ls_stpo_new.
          ls_stpo_new-bom_no = l_stlnr.
*          IF g_stlal = '09'.
*            IF ls_stpo-component = g_matnr.
*              ls_stpo_new-rec_allowd = 'X'.
*            ENDIF.
*          ENDIF.
          APPEND ls_stpo_new TO lt_stpo_new.
          l_fl_new_item = 'X'.
        ENDIF.
      ENDLOOP.
      LOOP AT lt_stpo_chk INTO ls_stpo_chk.
        CLEAR ls_stpo_old.
        MOVE-CORRESPONDING ls_stpo_chk TO ls_stpo_old.
        ls_stpo_old-fldelete = 'X'.
*        MODIFY lt_stpo_old FROM ls_stpo_old TRANSPORTING fldelete.
        APPEND ls_stpo_old TO lt_stpo_new.
      ENDLOOP.
*        IF c_ret_msg-type = 'E'.
*          RETURN.
*        ENDIF.
      "创建/修改单一BOM
      IF l_acflag = 'A'.
        PERFORM frm_create_mat_bom TABLES   lt_stpo_new
                                   USING    u_input ls_stko
                                   CHANGING c_ret_msg.
*      ELSEIF sy-uname(6) = 'ELAINE'.
*        PERFORM frm_modify_mat_bom TABLES lt_stpo lt_stpo_old1
*                                   USING  u_input ls_stko c_change_no
*                                   CHANGING c_ret_msg.
      ELSE.
        PERFORM frm_call_genbom_bapi TABLES   lt_stpo_new lt_stpo_old
                                     USING    u_input ls_stko l_acflag c_change_no l_fl_new_item l_valid_from_flag
                                     CHANGING c_ret_msg.
      ENDIF.
      "删除BOM
    WHEN 'D'.
      "调用BAPI执行删除

      IF c_change_no IS INITIAL.
        PERFORM frm_create_ecn CHANGING c_change_no.
      ENDIF.
      PERFORM delete_bom TABLES   lt_stpo
                         USING    u_input c_change_no
                         CHANGING ls_stko c_ret_msg.
  ENDCASE.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_CALL_GENBOM_BAPI
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_STPO
*&      --> U_INPUT
*&      --> LS_STKO
*&      <-- C_RET_MSG
*&---------------------------------------------------------------------*
FORM frm_call_genbom_bapi  TABLES  t_stpo STRUCTURE stpo_api03
                                   t_stpo_old STRUCTURE stpo_api03
                  USING  u_input TYPE zdt_plm2sap_createchange_bom_2
                         u_stko TYPE stko_api01
                         u_acflag TYPE c
                         u_change_no TYPE csap_mbom-aennr
                         u_fl_new_item TYPE csdata-xfeld
                         u_valid_from_flag TYPE c
                 CHANGING c_ret_msg TYPE zdt_plm2sap_createchange_bom_1.

  DATA: ls_stko TYPE stko_api02,
        lt_stpo TYPE TABLE OF stpo_api03,
        l_msg   TYPE bapi_msg.
  DATA:new_stlnr LIKE mast-stlnr.
  DATA: lt_values TYPE char_allocation_tt,
        ls_value  TYPE bapi1003_alloc_values_char,
        lv_matkl  TYPE mara-matkl.
  LOOP AT t_stpo ASSIGNING FIELD-SYMBOL(<fs_stpo>).
    <fs_stpo>-rel_cost = 'X'.
    "BOM组件成本核算标识
    REFRESH lt_values.
    CLEAR: ls_value,lv_matkl.
    SELECT SINGLE matkl INTO lv_matkl FROM mara WHERE matnr = <fs_stpo>-component.
    IF sy-subrc  = 0.
      lt_values = zcl_bc_public=>get_class_values( key = <fs_stpo>-component  classtype = '001' classnum = lv_matkl objtable = 'MARA' ).
      READ TABLE lt_values WITH KEY charact = 'ZCPM'  INTO ls_value.
      IF sy-subrc = 0 AND ( ls_value-value_neutral = 'X' OR ls_value-value_neutral = '是' ).""客供来料，取消成本核算相关项的标识符
        <fs_stpo>-rel_cost = '!'."cns_rel_cost_null."设置与成本核算相关的项目无关(函数CALO_INIT_API的输入参数data_reset_sign = '!')
      ENDIF.
    ENDIF.
  ENDLOOP.
  "调用BAPI创建BOM
  IF u_acflag = 'A'.
*    CALL FUNCTION 'CSAP_MAT_BOM_MAINTAIN'
*      EXPORTING
*        material           = g_matnr "抬头物料
*        plant              = g_plant "工厂
*        bom_usage          = g_stlan "物料清单用途
*        alternative        = g_stlal "备选物料清单
*        valid_from         = g_valid_from "有效日期自 (BTCI)
*        i_stko             = u_stko "BOM抬头结构
*        fl_commit_and_wait = 'X'
*        fl_bom_create      = 'X'
*        "fl_default_values = ''  "去掉设置默认值，成本核算标识相关等标识才能设置
*        fl_complete        = 'X'
*        fl_new_item        = 'X'
**       fl_default_values  = 'X'
*      IMPORTING
*        fl_warning         = g_warning
*        o_stko             = ls_stko
*      TABLES
*        t_stpo             = t_stpo
**       t_dep_data         = out_dep_data
**       t_dep_descr        = out_dep_descr
**       t_dep_order        = out_dep_order
**       t_dep_source       = out_dep_source
**       t_dep_doc          = out_dep_doc
**       t_doc_link         = out_doc_link
**       t_dmu_tmx          = out_dmu_tmx
**       t_ltx_line         = out_ltx_line
**       t_stpu             = out_stpu
**       t_fsh_bomd         = out_fsh_bomd
**       t_sgt_bomc         = out_sgt_bomc
*      EXCEPTIONS
*        error              = 1
*        OTHERS             = 2.
  ELSE.
*    CALL FUNCTION 'CSAP_MAT_BOM_OPEN'
*      EXPORTING
*        material    = g_matnr
*        plant       = g_plant
*        bom_usage   = g_stlan "BOM用途
*        alternative = g_stlal "备选物料清单
*        valid_from  = g_valid_from
**       change_no   = u_change_no
**       REVISION_LEVEL         =
**       FL_NO_CHANGE_DOC       = ' '
**     IMPORTING
**       O_STKO      =
**       FL_WARNING  =
*      TABLES
*        t_stpo      = lt_stpo
**       T_DEP_DATA  =
**       T_DEP_DESCR =
**       T_DEP_ORDER =
**       T_DEP_SOURCE           =
**       T_DEP_DOC   =
*      EXCEPTIONS
*        error       = 1
*        OTHERS      = 2.
*    IF sy-subrc = 0.
    "改表头
    CALL FUNCTION 'CSAP_MAT_BOM_MAINTAIN'
      EXPORTING
        material           = g_matnr "抬头物料
        plant              = g_plant "工厂
        bom_usage          = g_stlan "物料清单用途
        alternative        = g_stlal "备选物料清单
        valid_from         = g_valid_from "有效日期自 (BTCI)
*       change_no          = u_change_no "改表头不传更改号
        i_stko             = u_stko "BOM抬头结构
        fl_bom_create      = 'X'
        fl_new_item        = ''
        fl_commit_and_wait = 'X'
        fl_complete        = 'X'
        "fl_default_values = ''  "去掉设置默认值，成本核算标识相关等标识才能设置
      IMPORTING
        fl_warning         = g_warning
        o_stko             = ls_stko
*        TABLES
*       t_stpo             = lt_stpo
*       t_dep_data         = out_dep_data
*       t_dep_descr        = out_dep_descr
*       t_dep_order        = out_dep_order
*       t_dep_source       = out_dep_source
*       t_dep_doc          = out_dep_doc
*       t_doc_link         = out_doc_link
*       t_dmu_tmx          = out_dmu_tmx
*       t_ltx_line         = out_ltx_line
*       t_stpu             = out_stpu
*       t_fsh_bomd         = out_fsh_bomd
*       t_sgt_bomc         = out_sgt_bomc
      EXCEPTIONS
        error              = 1
        OTHERS             = 2.
    IF sy-subrc = 0.
      "开启日期记录
      CALL FUNCTION 'CALO_INIT_API'
        EXPORTING
          flag_db_log_on           = 'X'
          flag_msg_on              = 'X'
          flag_api_api_call_on     = ' '
          flag_collect_msg_on      = ' '
          external_log_no          = 'API'
          del_log_after_days       = '10'
          data_reset_sign          = '!'
        EXCEPTIONS
          log_object_not_found     = 1
          log_sub_object_not_found = 2
          OTHERS                   = 3.
      "改行项目
      CALL FUNCTION 'CSAP_MAT_BOM_MAINTAIN'
        EXPORTING
          material           = g_matnr "抬头物料
          plant              = g_plant "工厂
          bom_usage          = g_stlan "物料清单用途
          alternative        = g_stlal "备选物料清单
          valid_from         = g_valid_from "有效日期自 (BTCI)
          change_no          = u_change_no
          i_stko             = u_stko "BOM抬头结构
          fl_bom_create      = 'X'
          fl_new_item        = u_fl_new_item
          fl_commit_and_wait = 'X'
          fl_complete        = 'X'
          fl_default_values = ''  "去掉设置默认值，成本核算标识相关等标识才能设置
        IMPORTING
          fl_warning         = g_warning
          o_stko             = ls_stko
        TABLES
          t_stpo             = t_stpo
*         t_dep_data         = out_dep_data
*         t_dep_descr        = out_dep_descr
*         t_dep_order        = out_dep_order
*         t_dep_source       = out_dep_source
*         t_dep_doc          = out_dep_doc
*         t_doc_link         = out_doc_link
*         t_dmu_tmx          = out_dmu_tmx
*         t_ltx_line         = out_ltx_line
*         t_stpu             = out_stpu
*         t_fsh_bomd         = out_fsh_bomd
*         t_sgt_bomc         = out_sgt_bomc
        EXCEPTIONS
          error              = 1
          OTHERS             = 2.
    ENDIF.
*    ENDIF.

*    IF u_valid_from_flag IS NOT INITIAL.
*      FREE MEMORY ID 'API_MOD'.
*    ENDIF.

    IF sy-subrc = 0 ."操作成功
      SELECT SINGLE stlnr  INTO  new_stlnr FROM mast WHERE matnr = g_matnr AND werks = g_plant AND stlan = g_stlan AND stlal = g_stlal .

      IF u_acflag = 'A'.
        CLEAR: gs_zppt0010.
        gs_zppt0010-guid  = g_guid.                 "唯一标识码
        gs_zppt0010-matnr = g_matnr.                 "物料编码
        gs_zppt0010-werks = g_plant.                 "工厂
        gs_zppt0010-stlnr = new_stlnr.                 "物料清单号
        gs_zppt0010-stlal = g_stlal.                 "备选物料清单
        gs_zppt0010-valid_from_date = g_valid_from .""有效日期自
        gs_zppt0010-zz001 = g_cn_rfq_code. "需求报价单号

        INSERT zppt0010 FROM gs_zppt0010.
      ENDIF.
*      WAIT UP TO 2 SECONDS.
      c_ret_msg-type = 'S'.                 "消息类型
      c_ret_msg-message = g_matnr && ' BOM修改成功'.                 "消息

      CALL FUNCTION 'CSAP_MAT_BOM_CLOSE'
        EXPORTING
          fl_commit_and_wait = ' '
        IMPORTING
          fl_warning         = g_warning
        EXCEPTIONS
          error              = 1
          OTHERS             = 2.
      IF sy-subrc NE 0.            "消息
      ENDIF.

    ELSE.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO l_msg.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

      c_ret_msg-type = 'E'.                 "消息类型
      c_ret_msg-message = g_matnr && ' BOM修改失败：' && l_msg.                 "消息

    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form DELETE_BOM
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_STPO
*&      --> U_INPUT
*&      --> LS_STKO
*&---------------------------------------------------------------------*
FORM delete_bom  TABLES  t_stpo TYPE tt_stpo_in
                  USING  u_input TYPE zdt_plm2sap_createchange_bom_2
                         u_change_no TYPE csap_mbom-aennr
                 CHANGING cs_stko TYPE stko_api01
                          c_ret_msg TYPE zdt_plm2sap_createchange_bom_1.
  DATA: ls_stko TYPE stko_api02.


  cs_stko-base_quan = u_input-base_qty. "  基本数量
  PERFORM frm_get_base_unit CHANGING cs_stko-base_unit.  "  基本单位
  cs_stko-bom_status = g_stlan .        "物料清单状态

  cs_stko-delete_ind = 'X'.  "BOM抬头删除标识符

  "调用BAPI删除BOM
*  CALL FUNCTION 'CSAP_MAT_BOM_MAINTAIN'
*    EXPORTING
*      material    = g_matnr "抬头物料
*      plant       = g_plant "工厂
*      bom_usage   = g_stlan "物料清单用途
*      alternative = g_stlal "备选物料清单
*      "bom_usage   = '1' "物料清单用途
*      "alternative = '01' "备选物料清单
*      valid_from  = g_valid_from "有效日期自 (BTCI)
*      change_no   = u_change_no
*      i_stko      = cs_stko "BOM抬头结构
*      "fl_bom_create = 'X'
*      "fl_default_values = ''  "去掉设置默认值，成本核算标识相关等标识才能设置
*    IMPORTING
*      fl_warning  = g_warning
*      o_stko      = ls_stko
*      "TABLES
*      "t_stpo        = dt_stpo
*    EXCEPTIONS
*      error       = 1
*      OTHERS      = 2.
  CALL FUNCTION 'CSAP_MAT_BOM_DELETE'
    EXPORTING
      material           = g_matnr "抬头物料
      plant              = g_plant "工厂
      bom_usage          = g_stlan "物料清单用途
      alternative        = g_stlal
*     VALID_FROM         =
*     CHANGE_NO          =
*     REVISION_LEVEL     =
      fl_no_change_doc   = 'X'
      fl_commit_and_wait = 'X'
    IMPORTING
      fl_warning         = g_warning
    EXCEPTIONS
      error              = 1
      OTHERS             = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

  IF sy-subrc = 0."操作成功
    c_ret_msg-type = 'S'.                 "消息类型
    c_ret_msg-message = g_matnr && ' BOM删除成功'.                 "消息
  ELSE.
    c_ret_msg-type = 'E'.                 "消息类型
    c_ret_msg-message = g_matnr && ' BOM删除失败'.                 "消息
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_CREATE_ECN
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- C_CHANGE_NO
*&---------------------------------------------------------------------*
FORM frm_create_ecn  CHANGING c_change_no TYPE aenrb-aennr.

  DATA: ls_chng_header LIKE aenr_api01. " 抬头
  DATA: ls_obj_bom LIKE aenv_api01,    "标志（ECN用途）
        l_aennr    TYPE aenr-aennr.

  CHECK g_guid_input IS NOT INITIAL.  "g_chtxt
*--------------------------------------------------------------------*
* 创建更改编号
*--------------------------------------------------------------------*
  CLEAR: ls_chng_header, ls_obj_bom, c_change_no.

  ls_chng_header-status = '01'. "更改号的状态
  ls_chng_header-valid_from = sy-datum. "开始自日期
  ls_chng_header-descript = g_guid_input. "更改号描述 存PLM单据号
  ls_chng_header-reason_chg = g_chtxt. "更改原因

  ls_obj_bom-active = 'X'. "标志： 对象类型因更改号码而激活
  ls_obj_bom-mgtrec_gen = 'X'. "标识：对象管理记录生成
  ls_obj_bom-obj_requ = 'X'. "标识：需要每个对象的管理记录

  CALL FUNCTION 'CCAP_ECN_CREATE'
    EXPORTING
      change_header            = ls_chng_header
      object_bom               = ls_obj_bom
    IMPORTING
      change_no                = c_change_no
    EXCEPTIONS
      change_no_already_exists = 1
      error                    = 2
      OTHERS                   = 3.

  DO 30 TIMES.
    SELECT SINGLE aennr INTO l_aennr
      FROM aenr
      WHERE aennr = c_change_no.
    IF sy-subrc = 0.
      EXIT.
    ELSE.
      WAIT UP TO 1 SECONDS.
    ENDIF.
  ENDDO.

*--------------------------------------------------------------------*
* 创建更改编号
*--------------------------------------------------------------------*

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_CREATE_MAT_BOM
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_STPO_NEW
*&      --> U_INPUT
*&      --> LS_STKO
*&      <-- C_RET_MSG
*&---------------------------------------------------------------------*
FORM frm_create_mat_bom   TABLES  t_stpo STRUCTURE stpo_api03
                           USING  u_input TYPE zdt_plm2sap_createchange_bom_2
                                  u_stko TYPE stko_api01
                         CHANGING c_ret_msg TYPE zdt_plm2sap_createchange_bom_1.


  DATA : bomgroup TYPE TABLE OF bapi1080_bgr_c WITH HEADER LINE .
  DATA : variants TYPE TABLE OF bapi1080_bom_c WITH HEADER LINE .
  DATA : materialrelations TYPE TABLE OF bapi1080_mbm_c WITH HEADER LINE .
  DATA : item TYPE TABLE OF bapi1080_itm_c WITH HEADER LINE .
  DATA : itemassignments TYPE TABLE OF bapi1080_rel_itm_bom_c WITH HEADER LINE .
  DATA : texts TYPE TABLE OF bapi1080_txt_c WITH HEADER LINE .
  DATA : object_id TYPE     bapi1080_rel_itm_bom_c-sub_object_id  VALUE 'SIMPLE1' .
  DATA : return TYPE TABLE OF bapiret2 WITH  HEADER LINE .
  DATA : hc TYPE bapi1080_itm_c-item_id .
  DATA: ls_stpo LIKE LINE OF t_stpo.


  bomgroup-bom_group_identification = 'BAPI_SMP_COL1'."标识物料单组
  bomgroup-object_type  = 'BGR'."BOM 组中的对象类型
  bomgroup-object_id = 'SIMPLE1'."BOM 组中对象的标识
  bomgroup-technical_type = ''.
*  bomgroup-bom_usage = '1' .
*  bomgroup-ltxt_lang       = sy-langu.     "语言
  bomgroup-bom_usage = g_stlan . "leo
  bomgroup-auth_group = u_stko-auth_group.
  bomgroup-created_in_plant = g_plant .
  APPEND bomgroup .

  variants-bom_group_identification = 'BAPI_SMP_COL1' .
  variants-object_type = 'BOM' .
  variants-object_id = 'SIMPLE1'."BOM 组中对象的标识
  variants-alternative_bom = g_stlal .
  variants-bom_status = u_stko-bom_status .
  variants-base_qty = u_stko-base_quan.
  PERFORM frm_conversion_cunit_input USING u_stko-base_unit CHANGING variants-base_unit.

  variants-valid_from_date = g_valid_from ."GC_DATUV .
  variants-function = 'NEW'."功能
  APPEND variants .

  materialrelations-bom_group_identification = 'BAPI_SMP_COL1' .
*MATERIALRELATIONS-OBJECT_TYPE = 'BOM' .
*MATERIALRELATIONS-OBJECT_ID = 'SIMPLE1'."BOM 组中对象的标识
  materialrelations-material = g_matnr .
  materialrelations-plant = g_plant .
  materialrelations-bom_usage = g_stlan .
  materialrelations-alternative_bom = g_stlal .
  APPEND materialrelations .

  DATA: lv_object_id TYPE bapi1080_itm_c-object_id.

  LOOP AT t_stpo INTO ls_stpo .
    CLEAR lv_object_id.
    lv_object_id = 'SIMPLE_LINE' && sy-tabix.

    itemassignments-bom_group_identification = 'BAPI_SMP_COL1'."标识物料单组
    itemassignments-sub_object_type = 'ITM' .
    itemassignments-sub_object_id = lv_object_id."BOM 组中对象的标识
    itemassignments-valid_from_date = g_valid_from .
    itemassignments-super_object_type = 'BOM' .
    itemassignments-super_object_id = 'SIMPLE1' .
    itemassignments-function = 'NEW' .
    APPEND itemassignments .

    "OBJECT_ID =  OBJECT_ID + 1 .
    item-bom_group_identification = 'BAPI_SMP_COL1'."标识物料单组
    item-object_type        = 'ITM'."BOM 组中的对象类型
*    item-object_id         = 'SIMPLE1'."BOM 组中对象的标识
    item-object_id          = lv_object_id."BOM 组中对象的标识
    item-item_no            = ls_stpo-item_no .

    item-component          = ls_stpo-component.
    item-comp_qty           = ls_stpo-comp_qty .
    PERFORM frm_conversion_cunit_input USING ls_stpo-comp_unit CHANGING item-comp_unit.

    item-item_cat           = ls_stpo-item_categ .

*    item-alt_item_group     = gwa_stpo-ai_group .
*    item-alt_item_strategy  = gwa_stpo-ai_strateg .
    item-fixed_qty          = ls_stpo-fixed_qty.  "固定数量标识
*    item-co_product         = gwa_stpo-co_product.  "联产品标识 ADD BY ZBL 20180808
    item-prod_rel           = ls_stpo-rel_prod .
*    item-usage_prob         = gwa_stpo-usage_prob .
    item-iss_st_loc         = ls_stpo-issue_loc .
*    IF ls_stpo-rel_cost = cns_rel_cost_null.
*      item-cost_rel         = ''.
*    ELSE.
*      item-cost_rel         = 'X'.
*    ENDIF.
    IF ls_stpo-rel_cost = '!'.
      CLEAR ls_stpo-rel_cost.
    ENDIF.
    item-cost_rel           =  ls_stpo-rel_cost .
    item-ltxt_lang          = sy-langu .
    " GWA_STPO-AI_PRIO      = GWA_UP_DATA-ALPRF  ."备选项目：分类订单

    item-sort_string        = ls_stpo-sortstring .
*    item-net_scrap_ind      = gwa_stpo-op_net_ind.
    item-opr_scrap          = ls_stpo-op_scrap  .
*    item-rec_allowed        = gwa_stpo-rec_allowd.
*    item-lead_time_offset   = gwa_stpo-lead_time .
*    item-spproctype         = gwa_stpo-spproctype.
*    item-mat_provision      = gwa_stpo-mat_provis.
*    item-bulk_mat           = gwa_stpo-bulk_mat  .
*    item-eng_rel            = gwa_stpo-rel_engin .
*    item-prod_rel           = ls_stpo-rel_prod  .
    item-comp_scrap         = ls_stpo-comp_scrap.
    IF g_stlal = '09'.
      IF ls_stpo-component = g_matnr.
        item-rec_allowed = 'X'.
      ENDIF.
    ENDIF.
    APPEND item  .
    CLEAR: item .
  ENDLOOP .
  " modify by ljm 2020-12-17 begin-------
  "sy-tcode = 'ZPP053'用于PLM创建BOM时触发增强，功能为取消客供料BOM组件的成本核算相关项标识；
  "具体参见增强点ENHANCEMENT 1  ZPP_CANCEL_SANKA_CRTBOM_BY_PLM，所在函数为CS_CL_P_ITM_SAVE。
  sy-tcode = 'ZPP053'.
  " modify by ljm 2020-12-17 end-------
  CALL FUNCTION 'BAPI_MATERIAL_BOM_GROUP_CREATE'
    EXPORTING
      " TESTRUN           = ''
      all_error         = 'X'
    TABLES
      bomgroup          = bomgroup
      variants          = variants
      items             = item
*     SUBITEMS          =
      materialrelations = materialrelations
      itemassignments   = itemassignments
*     SUBITEMASSIGNMENTS       =
      texts             = texts
      return            = return.


  LOOP AT return WHERE type = 'E' OR  type = 'A' .

    c_ret_msg-type = 'E' .
    IF c_ret_msg-message IS INITIAL.
      c_ret_msg-message = g_matnr && ' BOM创建失败：' && return-message .
    ELSE.
      c_ret_msg-message = c_ret_msg-message && return-message .
    ENDIF.

  ENDLOOP .
  IF sy-subrc <> 0 .

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.
    c_ret_msg-type = 'S' .
    c_ret_msg-message = 'BOM创建成功' .
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
  ENDIF .

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_MODIFY_MAT_BOM
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_STPO_NEW
*&      --> LT_STPO_OLD
*&      --> U_INPUT
*&      --> LS_STKO
*&      --> C_CHANGE_NO
*&      <-- C_RET_MSG
*&---------------------------------------------------------------------*
FORM frm_modify_mat_bom  TABLES t_stpo TYPE tt_stpo_in
                                t_stpo_old STRUCTURE stpo_api03
                         USING  u_input TYPE zdt_plm2sap_createchange_bom_2
                                u_stko TYPE stko_api01
                                u_change_no TYPE csap_mbom-aennr
                       CHANGING c_ret_msg TYPE zdt_plm2sap_createchange_bom_1.

  DATA : bomgroup TYPE TABLE OF bapi1080_bgr_c WITH HEADER LINE .
  DATA : variants TYPE TABLE OF bapi1080_bom_c WITH HEADER LINE .
  DATA : materialrelations TYPE TABLE OF bapi1080_mbm_c WITH HEADER LINE .
  DATA : item TYPE TABLE OF bapi1080_itm_c WITH HEADER LINE .
  DATA : itemassignments TYPE TABLE OF bapi1080_rel_itm_bom_c WITH HEADER LINE .
  DATA : texts TYPE TABLE OF bapi1080_txt_c WITH HEADER LINE .
  DATA : object_id TYPE     bapi1080_rel_itm_bom_c-sub_object_id  VALUE 'SIMPLE1' .
  DATA : return TYPE TABLE OF bapiret2 WITH  HEADER LINE .
  DATA : hc TYPE bapi1080_itm_c-item_id .
  DATA: ls_stpo     LIKE LINE OF t_stpo,
        ls_stpo_old LIKE LINE OF t_stpo_old.
  DATA: lt_stb           TYPE TABLE OF stpox,
        ls_stb           TYPE stpox,
        ls_cstmat        TYPE cstmat,
        l_bom_class_data TYPE bom_class_data,
        l_itm_class_data TYPE itm_class_data.

  REFRESH: lt_stb.
  CLEAR ls_cstmat.
  CALL FUNCTION 'CS_BOM_EXPL_MAT_V2'
    EXPORTING
      aumng                 = 0  "输入非零的值，最上级的物料不考虑装配报废率、但下级的物料还是会考虑装配报废，0考虑装配报废，需配合AUSKZ=‘X'使用
      auskz                 = 'X'  "计算组件损耗(会考虑装配报废率，需配合AUMNG使用)
      capid                 = 'PP01'  "BOM应用
      datuv                 = sy-datum  "有效起始日
      delnl                 = abap_false  "组件删除标识
*     emeng                 = iv_bmeng  "需求数量
      mehrs                 = ''    "多层展开标识-多层
      mtnrv                 = g_matnr "展开物料号
      stlan                 = g_stlan  "BOM用途
      stlal                 = g_stlal "可选BOM
      werks                 = g_plant  "工厂
    IMPORTING
      topmat                = ls_cstmat
*     dstst                 = pv_dstst
    TABLES
      stb                   = lt_stb
*     matcat                = pt_matcat
    EXCEPTIONS
      alt_not_found         = 1
      call_invalid          = 2
      material_not_found    = 3
      missing_authorization = 4
      no_bom_found          = 5
      no_plant_data         = 6
      no_suitable_bom_found = 7
      conversion_error      = 8
      OTHERS                = 9.


  CLEAR: l_bom_class_data, variants.
  MOVE-CORRESPONDING ls_cstmat TO l_bom_class_data.
  CALL FUNCTION 'MAP2E_BOM_DATA_TO_BAPI1080'
    EXPORTING
      bom_class_data = l_bom_class_data
    CHANGING
      bapi1080_bom_c = variants.

  IF variants-bom_group_identification IS INITIAL.
    variants-bom_group_identification = 'BAPI_SMP_COL1' .
  ENDIF.
  IF  variants-object_type IS INITIAL.
    variants-object_type = 'BOM' .
  ENDIF.
  IF variants-object_id IS INITIAL.
    variants-object_id = 'SIMPLE1'."BOM 组中对象的标识
  ENDIF.
  variants-alternative_bom = g_stlal .
  variants-bom_status = u_stko-bom_status .
  variants-base_qty = u_stko-base_quan.
  PERFORM frm_conversion_cunit_input USING u_stko-base_unit CHANGING variants-base_unit.

  IF g_valid_from GE sy-datum.
    variants-valid_from_date = g_valid_from ."GC_DATUV .
  ENDIF.

  variants-change_no = u_change_no.
  variants-function = 'CHG'."功能
  APPEND variants .

  bomgroup-bom_group_identification = variants-bom_group_identification."标识物料单组
  bomgroup-object_type  = 'BGR'."BOM 组中的对象类型
  bomgroup-object_id = variants-object_id."BOM 组中对象的标识
  bomgroup-technical_type = ''.
*  bomgroup-ltxt_lang       = sy-langu.     "语言
  bomgroup-bom_usage = g_stlan . "leo
  bomgroup-created_in_plant = g_plant .
  APPEND bomgroup .

  materialrelations-bom_group_identification = variants-bom_group_identification .
*MATERIALRELATIONS-OBJECT_TYPE = 'BOM' .
*MATERIALRELATIONS-OBJECT_ID = 'SIMPLE1'."BOM 组中对象的标识
  materialrelations-material = g_matnr .
  materialrelations-plant = g_plant .
  materialrelations-bom_usage = g_stlan .
  materialrelations-alternative_bom = g_stlal .
  APPEND materialrelations .

  DATA: lv_object_id TYPE bapi1080_itm_c-object_id.

  SORT t_stpo BY item_no.
  SORT lt_stb BY posnr.
  LOOP AT t_stpo INTO ls_stpo .

    CLEAR: ls_stb.
    READ TABLE lt_stb INTO ls_stb WITH KEY posnr = ls_stpo-item_no BINARY SEARCH.
    IF sy-subrc = 0.
      itemassignments-function = 'CHG' .
    ELSE.
      itemassignments-function = 'NEW' .
    ENDIF.
    IF ls_stb IS NOT INITIAL.
      CLEAR: l_itm_class_data.
      MOVE-CORRESPONDING ls_stb TO l_itm_class_data.
      CALL FUNCTION 'MAP2E_ITM_DATA_TO_BAPI1080'
        EXPORTING
          itm_class_data = l_itm_class_data
        CHANGING
          bapi1080_itm_c = item.

    ENDIF.
    CLEAR lv_object_id.
    IF item-object_id IS NOT INITIAL.
      lv_object_id = item-object_id.
    ELSE.
      lv_object_id = 'SIMPLE_LINE' && sy-tabix.
    ENDIF.

    itemassignments-bom_group_identification = variants-bom_group_identification."标识物料单组
    itemassignments-sub_object_type = 'ITM' .
    itemassignments-sub_object_id = lv_object_id."BOM 组中对象的标识
    IF g_valid_from GE sy-datum.
      itemassignments-valid_from_date = g_valid_from .
    ELSEIF itemassignments-function = 'NEW' .
      itemassignments-valid_from_date = sy-datum.
    ENDIF.
    itemassignments-super_object_type = variants-object_type. "'BOM' .
    itemassignments-super_object_id = variants-object_id .

    APPEND itemassignments .

*    IF ls_stpo_old IS NOT INITIAL.
*      MOVE-CORRESPONDING ls_stpo_old TO item.
*    ENDIF.
    "OBJECT_ID =  OBJECT_ID + 1 .
    item-bom_group_identification = variants-bom_group_identification."标识物料单组
    item-object_type        = 'ITM'."BOM 组中的对象类型
*    item-object_id         = 'SIMPLE1'."BOM 组中对象的标识
    item-object_id          = lv_object_id."BOM 组中对象的标识
    item-item_no            = ls_stpo-item_no .

    item-component          = ls_stpo-component.
    item-comp_qty           = ls_stpo-comp_qty .
    PERFORM frm_conversion_cunit_input USING ls_stpo-comp_unit CHANGING item-comp_unit.

    item-item_cat           = ls_stpo-item_categ .

*    item-alt_item_group     = gwa_stpo-ai_group .
*    item-alt_item_strategy  = gwa_stpo-ai_strateg .
    item-fixed_qty          = ls_stpo-fixed_qty.  "固定数量标识
*    item-co_product         = gwa_stpo-co_product.  "联产品标识 ADD BY ZBL 20180808
*    item-prod_rel           = ls_stpo-rel_prod .
*    item-usage_prob         = gwa_stpo-usage_prob .
    item-iss_st_loc         = ls_stpo-issue_loc .
    item-cost_rel           = ls_stpo-rel_cost .
    item-ltxt_lang          = sy-langu .
    " GWA_STPO-AI_PRIO      = GWA_UP_DATA-ALPRF  ."备选项目：分类订单

    item-sort_string        = ls_stpo-sortstring .
*    item-net_scrap_ind      = gwa_stpo-op_net_ind.
*    item-opr_scrap          = ls_stpo-op_scrap  .
*    item-rec_allowed        = gwa_stpo-rec_allowd.
*    item-lead_time_offset   = gwa_stpo-lead_time .
*    item-spproctype         = gwa_stpo-spproctype.
*    item-mat_provision      = gwa_stpo-mat_provis.
*    item-bulk_mat           = gwa_stpo-bulk_mat  .
*    item-eng_rel            = gwa_stpo-rel_engin .
*    item-prod_rel           = ls_stpo-rel_prod  .
    item-comp_scrap         = ls_stpo-comp_scrap.

    APPEND item  .



    CLEAR: item, itemassignments .
  ENDLOOP .
  LOOP AT lt_stb INTO ls_stb WHERE loekz = '' .

    READ TABLE t_stpo INTO ls_stpo WITH KEY item_no = ls_stb-posnr BINARY SEARCH.
    IF sy-subrc = 0.
      CONTINUE.
    ELSE.
      itemassignments-function = 'DEL' .
    ENDIF.
    CLEAR: l_itm_class_data.
    MOVE-CORRESPONDING ls_stb TO l_itm_class_data.
    CALL FUNCTION 'MAP2E_ITM_DATA_TO_BAPI1080'
      EXPORTING
        itm_class_data = l_itm_class_data
      CHANGING
        bapi1080_itm_c = item.
    CLEAR lv_object_id.
    lv_object_id = 'SIMPLE_LINE' && sy-tabix.

    itemassignments-bom_group_identification = variants-bom_group_identification."标识物料单组
    itemassignments-sub_object_type = 'ITM' .
    itemassignments-sub_object_id = lv_object_id."BOM 组中对象的标识
*    itemassignments-valid_from_date = g_valid_from .
    IF g_valid_from GE sy-datum.
      itemassignments-valid_from_date = g_valid_from .
    ELSE.
      itemassignments-valid_from_date = sy-datum.
    ENDIF.
    itemassignments-super_object_type = variants-object_type. " 'BOM' .
    itemassignments-super_object_id = variants-object_id. " 'SIMPLE1' .

    APPEND itemassignments .

    "OBJECT_ID =  OBJECT_ID + 1 .
    item-bom_group_identification = variants-bom_group_identification."标识物料单组
    item-object_type        = 'ITM'."BOM 组中的对象类型
*    item-object_id         = 'SIMPLE1'."BOM 组中对象的标识
    item-object_id          = lv_object_id."BOM 组中对象的标识

    APPEND item  .

    CLEAR: item, itemassignments .
  ENDLOOP .


  CALL FUNCTION 'BAPI_MATERIAL_BOM_GROUP_CREATE'
    EXPORTING
      " TESTRUN           = ''
      all_error         = 'X'
    TABLES
      bomgroup          = bomgroup
      variants          = variants
      items             = item
*     SUBITEMS          =
      materialrelations = materialrelations
      itemassignments   = itemassignments
*     SUBITEMASSIGNMENTS       =
      texts             = texts
      return            = return.


  LOOP AT return WHERE type = 'E' OR  type = 'A' .

    c_ret_msg-type = 'E' .
    IF c_ret_msg-message IS INITIAL.
      c_ret_msg-message = g_matnr && ' BOM修改失败：' && return-message .
    ELSE.
      c_ret_msg-message = c_ret_msg-message && return-message .
    ENDIF.

  ENDLOOP .
  IF sy-subrc <> 0 .

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.
    c_ret_msg-type = 'S' .
    c_ret_msg-message = 'BOM修改成功' .
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
  ENDIF .

ENDFORM.    