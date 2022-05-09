FUNCTION zpp_bom_maintain1.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     VALUE(I_INPUT) TYPE  ZDT_PLM2SAP_CREATECHANGE_BOM
*"  EXPORTING
*"     VALUE(E_OUTPUT) TYPE  ZDT_PLM2SAP_CREATECHANGE_BOM_R
*"----------------------------------------------------------------------
  DATA:ls_ret_msg  TYPE zdt_plm2sap_createchange_bom_1, "返回创建结果消息结构
       lt_ret_msg  TYPE zdt_plm2sap_createchange__tab1, "返回创建结果消息内表
       ls_input    LIKE LINE OF i_input-bom_header, "传入参数的每个BOM
       ls_bom_abap TYPE ty_req_bom_abap, "接口传入参数转换ABAP的BOM结构
       lv_flag     TYPE c. "PLM创建BOM标识符号：A 创建，C 修改，D 删除
  IF i_input-bom_header[] IS INITIAL.
    ls_ret_msg-type = 'E'.
    ls_ret_msg-message = '传入参数为空'.
    APPEND ls_ret_msg TO e_output-detail.
    RETURN.
  ENDIF.
  "传入参数可包含多个BOM，这里循环处理每个BOM并返回结果信息
  LOOP AT i_input-bom_header INTO ls_input.
    CLEAR:ls_bom_abap, ls_ret_msg.
    "检查、转换接口传入的BOM参数
    PERFORM frm_chk_and_conv_req_data USING ls_input CHANGING ls_bom_abap ls_ret_msg.
    IF ls_ret_msg-type <>  'E'."传入参数检查或转换发生错误
      PERFORM frm_ini_resp_msg USING ls_bom_abap CHANGING ls_ret_msg.
      CASE ls_input-flag."接口操作标识:A-创建、C-修改、D-删除
        WHEN 'A' OR 'C'.
          PERFORM frm_create_or_update_bom USING ls_bom_abap CHANGING ls_ret_msg.
          "物料类型为Z050的BOM抬头物料，下发物料分类001类型的特征值给OA
          IF ls_ret_msg-type =  'S'.
            PERFORM frm_send_charval_to_oa USING ls_bom_abap.
          ENDIF.
        WHEN 'D'.
          PERFORM frm_delete_bom USING ls_bom_abap CHANGING ls_ret_msg.
        WHEN OTHERS.
      ENDCASE.
    ENDIF.
    APPEND ls_ret_msg TO e_output-detail."返回操作结果
  ENDLOOP.
ENDFUNCTION.
FORM frm_delete_bom USING is_bom_abap TYPE ty_req_bom_abap CHANGING cs_ret_msg TYPE zdt_plm2sap_createchange_bom_1.
  DATA lv_warning TYPE capiflag-flwarning.
  CALL FUNCTION 'CSAP_MAT_BOM_DELETE'
    EXPORTING
      material           = is_bom_abap-material "抬头物料
      plant              = is_bom_abap-plant "工厂
      bom_usage          = is_bom_abap-bom_usage "物料清单用途
      alternative        = is_bom_abap-stlal
*     VALID_FROM         =
*     CHANGE_NO          =
*     REVISION_LEVEL     =
      fl_no_change_doc   = 'X'
      fl_commit_and_wait = 'X'
    IMPORTING
      fl_warning         = lv_warning
    EXCEPTIONS
      error              = 1
      OTHERS             = 2.
  IF sy-subrc = 0."操作成功
    cs_ret_msg-type = 'S'. "消息类型
    cs_ret_msg-message =  'BOM删除成功'. "消息
  ELSE.
    cs_ret_msg-type = 'E'. "消息类型
    cs_ret_msg-message = ' BOM删除失败'."消息
  ENDIF.
ENDFORM.
FORM frm_ini_resp_msg USING  is_bom_abap TYPE ty_req_bom_abap  CHANGING cs_ret_msg TYPE zdt_plm2sap_createchange_bom_1.
  TRY .
      cs_ret_msg-guid = cl_system_uuid=>create_uuid_c36_static( )."guid唯一标识符
    CATCH cx_uuid_error.
      CONCATENATE sy-datum sy-uzeit '00l' INTO cs_ret_msg-guid.
  ENDTRY.
  cs_ret_msg-material = is_bom_abap-material.
  cs_ret_msg-plant = is_bom_abap-plant.
  cs_ret_msg-stlal = is_bom_abap-stlal.
ENDFORM.

* 创建或修改BOM
FORM frm_create_or_update_bom USING is_bom_abap TYPE ty_req_bom_abap
                              CHANGING cs_ret_msg TYPE zdt_plm2sap_createchange_bom_1.
  DATA:lt_stpo_open TYPE TABLE OF stpo_api02,
       lt_stko_open TYPE TABLE OF stko_api02,
       lv_flwarning TYPE capiflag-flwarning.
  "打开BOM,如果打开成功进行修改操作，打开失败进行新增操作
  CALL FUNCTION 'CSAP_MAT_BOM_READ'
    EXPORTING
      material    = is_bom_abap-material
      plant       = is_bom_abap-plant
      bom_usage   = is_bom_abap-bom_usage
      alternative = is_bom_abap-stlal
    TABLES
      t_stpo      = lt_stpo_open
      t_stko      = lt_stko_open
    EXCEPTIONS
      error       = 1
      OTHERS      = 2.
  IF sy-subrc <> 0.
    is_bom_abap-flag = 'A'."创建BOM标识
    PERFORM frm_create_bom USING is_bom_abap CHANGING cs_ret_msg.
  ELSE.
    is_bom_abap-flag = 'C'."修改BOM标识
    PERFORM frm_modify_bom USING is_bom_abap lt_stko_open lt_stpo_open CHANGING cs_ret_msg.
  ENDIF.
  CALL FUNCTION 'CSAP_MAT_BOM_CLOSE'
    EXPORTING
      fl_commit_and_wait = 'X'
    IMPORTING
      fl_warning         = lv_flwarning
    EXCEPTIONS
      error              = 1
      OTHERS             = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.
ENDFORM.
* 创建BOM
FORM frm_create_bom USING is_bom_abap TYPE ty_req_bom_abap
                              CHANGING cs_ret_msg TYPE zdt_plm2sap_createchange_bom_1.
  DATA : bomgroup          TYPE TABLE OF bapi1080_bgr_c WITH HEADER LINE,
         variants          TYPE TABLE OF bapi1080_bom_c WITH HEADER LINE,
         materialrelations TYPE TABLE OF bapi1080_mbm_c WITH HEADER LINE,
         item              TYPE TABLE OF bapi1080_itm_c WITH HEADER LINE,
         itemassignments   TYPE TABLE OF bapi1080_rel_itm_bom_c WITH HEADER LINE,
         texts             TYPE TABLE OF bapi1080_txt_c WITH HEADER LINE,
         object_id         TYPE bapi1080_rel_itm_bom_c-sub_object_id  VALUE 'SIMPLE1',
         return            TYPE TABLE OF bapiret2 WITH  HEADER LINE,
         hc                TYPE bapi1080_itm_c-item_id,
         lv_object_id      TYPE bapi1080_itm_c-object_id,
         ls_bom_item       TYPE ty_bom_item,
         lv_error          TYPE flag.

  bomgroup-bom_group_identification = 'BAPI_SMP_COL1'."标识物料单组
  bomgroup-object_type  = 'BGR'."BOM 组中的对象类型
  bomgroup-object_id = 'SIMPLE1'."BOM 组中对象的标识
  bomgroup-technical_type = ''.
  bomgroup-bom_usage = is_bom_abap-bom_usage.
*  bomgroup-ltxt_lang       = sy-langu.     "语言
  bomgroup-auth_group = is_bom_abap-auth_group.
  bomgroup-created_in_plant = is_bom_abap-plant .
  APPEND bomgroup .

  variants-bom_group_identification = 'BAPI_SMP_COL1' .
  variants-object_type = 'BOM' .
  variants-object_id = 'SIMPLE1'."BOM 组中对象的标识
  variants-alternative_bom = is_bom_abap-stlal .
  variants-bom_status = '01'. "物料清单状态
  variants-base_qty = is_bom_abap-base_qty.
  variants-base_unit = is_bom_abap-meins.
  variants-valid_from_date = sy-datum ."GC_DATUV .
  variants-function = 'NEW'."功能
  APPEND variants .

  materialrelations-bom_group_identification = 'BAPI_SMP_COL1' .
*MATERIALRELATIONS-OBJECT_TYPE = 'BOM' .
*MATERIALRELATIONS-OBJECT_ID = 'SIMPLE1'."BOM 组中对象的标识
  materialrelations-material = is_bom_abap-material .
  materialrelations-plant = is_bom_abap-plant .
  materialrelations-bom_usage = is_bom_abap-bom_usage .
  materialrelations-alternative_bom = is_bom_abap-stlal .
  APPEND materialrelations .

  LOOP AT is_bom_abap-bom_item INTO ls_bom_item.
    CLEAR:item,lv_object_id.
    lv_object_id = 'SIMPLE_LINE' && sy-tabix.
    itemassignments-bom_group_identification = 'BAPI_SMP_COL1'."标识物料单组
    itemassignments-sub_object_type = 'ITM' .
    itemassignments-sub_object_id = lv_object_id."BOM 组中对象的标识
    itemassignments-valid_from_date = sy-datum .
    itemassignments-super_object_type = 'BOM' .
    itemassignments-super_object_id = 'SIMPLE1' .
    itemassignments-function = 'NEW' .
    APPEND itemassignments .

    "OBJECT_ID =  OBJECT_ID + 1 .
    item-bom_group_identification = 'BAPI_SMP_COL1'."标识物料单组
    item-object_type        = 'ITM'."BOM 组中的对象类型
*    item-object_id         = 'SIMPLE1'."BOM 组中对象的标识
    item-object_id          = lv_object_id."BOM 组中对象的标识
    item-item_no            = ls_bom_item-item_no.
    item-component          = ls_bom_item-component.
    item-comp_qty           = ls_bom_item-comp_qty.
    item-comp_unit = ls_bom_item-comp_unit.
    item-item_cat           = ls_bom_item-item_categ.
    item-fixed_qty          = ls_bom_item-fixed_qty.  "固定数量标识
    item-prod_rel           = 'X'.
    item-iss_st_loc         = ls_bom_item-issue_loc.
    item-cost_rel           =  'X'.
    item-ltxt_lang          = sy-langu.
    item-sort_string        = ls_bom_item-sortstring .
    item-comp_scrap         = ls_bom_item-comp_scrap.
    IF g_stlal = '09' AND ls_bom_item-component = is_bom_abap-material.
      item-rec_allowed = 'X'.
    ENDIF.
    APPEND item  .
  ENDLOOP.

  "sy-tcode = 'ZPP053'用于PLM创建BOM时触发增强，功能为取消客供料BOM组件的成本核算相关项标识；
  "具体参见增强点ENHANCEMENT 1  ZPP_CANCEL_SANKA_CRTBOM_BY_PLM，所在函数为CS_CL_P_ITM_SAVE。
  sy-tcode = 'ZPP053'.
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

  LOOP AT return WHERE type = 'E' OR  type = 'A'.
    lv_error = 'X'.
    cs_ret_msg-type = 'E' .
    IF cs_ret_msg-message IS INITIAL.
      cs_ret_msg-message = |{ 'BOM创建失败：' }{ return-message }|.
    ELSE.
      cs_ret_msg-message = |{ cs_ret_msg-message }{ ',' }{ return-message }|.
    ENDIF.
  ENDLOOP .
  IF lv_error = 'X'.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'  EXPORTING  wait = 'X'.
    cs_ret_msg-type = 'S' .
    cs_ret_msg-message = 'BOM创建成功'.
    "BOM物料清单号
    SELECT SINGLE stlnr INTO cs_ret_msg-stlnr FROM mast
     WHERE matnr = is_bom_abap-material
      AND werks = is_bom_abap-plant
      AND stlan = is_bom_abap-bom_usage
      AND stlal = is_bom_abap-stlal.
  ENDIF.
ENDFORM.
* 修改BOM
FORM frm_modify_bom USING is_bom_abap TYPE ty_req_bom_abap
                          it_stko_open TYPE ty_tab_stko_open
                          it_stpo_open TYPE ty_tab_stpo_open
                   CHANGING cs_ret_msg TYPE zdt_plm2sap_createchange_bom_1.
  DATA:ls_stko_open     TYPE stko_api02,
       ls_stpo_open     TYPE stpo_api02,
       lv_base_qty_open TYPE stko-bmeng, "基本数量
       lv_menge         TYPE stpo-menge,
       ls_stko_modify   TYPE stko_api01,
       lv_subrc         TYPE sy-subrc,
       lv_mein          TYPE msehx,
       lt_bom_item_abap TYPE TABLE OF ty_bom_item,
       " lt_stpo_modify   TYPE TABLE OF stpo_api03 WITH HEADER LINE,
       lt_values        TYPE char_allocation_tt,
       ls_value         TYPE bapi1003_alloc_values_char.
  DATA: BEGIN OF lt_stpo_modify OCCURS 0.
          INCLUDE STRUCTURE stpo_api03.
        DATA: END OF lt_stpo_modify.

*1、先修改BOM表头，这里只修改2个字段：基本数量、权限组
  IF it_stko_open IS NOT INITIAL.
    READ TABLE it_stko_open INDEX 1 INTO ls_stko_open.
    cs_ret_msg-stlnr = ls_stko_open-bom_no."BOM物料清单号
    PERFORM frm_convert_char_to_num USING ls_stko_open-base_quan CHANGING lv_base_qty_open lv_subrc."字符转数值类型
    IF lv_base_qty_open <> is_bom_abap-base_qty.
      ls_stko_modify-base_quan = is_bom_abap-base_qty.
    ENDIF.
    IF ls_stko_open-auth_group <> is_bom_abap-auth_group.
      ls_stko_modify-auth_group = is_bom_abap-auth_group.
    ENDIF.
    IF ls_stko_modify-base_quan IS NOT INITIAL OR ls_stko_modify-auth_group IS NOT INITIAL.
      CALL FUNCTION 'CSAP_MAT_BOM_MAINTAIN'
        EXPORTING
          material           = is_bom_abap-material "抬头物料
          plant              = is_bom_abap-plant "工厂
          bom_usage          = is_bom_abap-bom_usage "物料清单用途
          alternative        = is_bom_abap-stlal "备选物料清单
          "valid_from         = sy-datum "有效日期自-未启用ECM时，此参数不能使用
          fl_bom_create      = 'X'
          i_stko             = ls_stko_modify "BOM抬头结构
          fl_commit_and_wait = 'X'
          fl_complete        = 'X'
          "fl_default_values = ''  "去掉设置默认值，成本核算标识相关等标识才能设置
        EXCEPTIONS
          error              = 1
          OTHERS             = 2.
      IF sy-subrc = 0.
        cs_ret_msg-message =  'BOM表头修改成功'.
      ELSE.
        cs_ret_msg-type = 'E'.
        cs_ret_msg-message = |{ 'BOM表头修改失败：' }{  sy-msgv1 }{ sy-msgv2 }{ sy-msgv3  }{ sy-msgv4 }|.
        RETURN.
      ENDIF.
    ENDIF.
  ENDIF.
* 修改BOM项目
*  1、删除SAP当前BOM比PLM传过来多出来的BOM项目
  lt_bom_item_abap = is_bom_abap-bom_item.
  SORT lt_bom_item_abap BY item_no.
  LOOP AT it_stpo_open INTO ls_stpo_open.
    CLEAR lt_stpo_modify.
    READ TABLE lt_bom_item_abap WITH KEY item_no = ls_stpo_open-item_no BINARY SEARCH TRANSPORTING NO FIELDS.
    IF sy-subrc <> 0.
      lt_stpo_modify-id_item_no = ls_stpo_open-item_no."通过id_item_no识别行项目
      lt_stpo_modify-fldelete = 'X'.
      APPEND  lt_stpo_modify.
    ENDIF.
  ENDLOOP.
* 2、新增、修改数据
  LOOP AT lt_bom_item_abap INTO DATA(ls_bom_item_abap).
    CLEAR:lt_stpo_modify, ls_stpo_open.
    READ TABLE it_stpo_open WITH KEY item_no = ls_bom_item_abap-item_no item_categ = ls_bom_item_abap-item_categ INTO ls_stpo_open.
    IF sy-subrc = 0."修改行操作
      "修改组件物料编码
      CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
        EXPORTING
          input        = ls_stpo_open-component
        IMPORTING
          output       = ls_stpo_open-component
        EXCEPTIONS
          length_error = 1
          OTHERS       = 2.
      IF ls_bom_item_abap-component IS NOT INITIAL AND ls_bom_item_abap-component <> ls_stpo_open-component.
        lt_stpo_modify-id_item_no = ls_stpo_open-item_no."标识行项目
        lt_stpo_modify-component = ls_bom_item_abap-component.
      ENDIF.
      "组件数量
      IF ls_bom_item_abap-comp_qty IS NOT INITIAL.
        PERFORM frm_convert_char_to_num USING ls_stpo_open-comp_qty CHANGING lv_menge lv_subrc."字符转数值类型
        IF ls_bom_item_abap-comp_qty <> lv_menge.
          lt_stpo_modify-id_item_no = ls_stpo_open-item_no."标识行项目
          lt_stpo_modify-comp_qty = ls_bom_item_abap-comp_qty.
        ENDIF.
      ENDIF.
      "组件单位
      "经测试发现事务代码CUNI配置的计量单位ZLI中的商业值为组，内部值为ZLI。
      "调用CSAP_MAT_BOM_MAINTAIN会发现单位转换失败，导致BOM新增行项目报错
      "所以这里先调用例程转换，提前处理这个
      CLEAR lv_mein.
      CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
        EXPORTING
          input          = ls_stpo_open-comp_unit
          language       = sy-langu
        IMPORTING
          output         = lv_mein
        EXCEPTIONS
          unit_not_found = 1
          OTHERS         = 2.
      IF sy-subrc <> 0.
        CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
          EXPORTING
            input          = ls_stpo_open-comp_unit
            language       = sy-langu
          IMPORTING
*           long_text      =
            output         = lv_mein
*           short_text     =
          EXCEPTIONS
            unit_not_found = 1
            OTHERS         = 2.
      ENDIF.
      IF ls_bom_item_abap-comp_unit IS NOT INITIAL AND ls_bom_item_abap-comp_unit <> lv_mein.
        lt_stpo_modify-id_item_no = ls_stpo_open-item_no."标识行项目
        lt_stpo_modify-comp_unit = ls_bom_item_abap-comp_unit.
      ENDIF.
      "子项废品率（%）损耗率(%)
      IF ls_bom_item_abap-comp_scrap <> ls_stpo_open-comp_scrap.
        lt_stpo_modify-id_item_no = ls_stpo_open-item_no."标识行项目
        lt_stpo_modify-comp_scrap = ls_bom_item_abap-comp_scrap.
      ENDIF.
      "固定数量
      IF ls_bom_item_abap-fixed_qty <> ls_stpo_open-fixed_qty.
        lt_stpo_modify-id_item_no = ls_stpo_open-item_no."标识行项目
        lt_stpo_modify-fixed_qty = ls_bom_item_abap-fixed_qty.
      ENDIF.
      "排序字符串量
      IF ls_bom_item_abap-sortstring <> ls_stpo_open-sortstring.
        lt_stpo_modify-id_item_no = ls_stpo_open-item_no."标识行项目
        lt_stpo_modify-sortstring = ls_bom_item_abap-sortstring.
      ENDIF.
      "生产仓储地点
      IF ls_bom_item_abap-issue_loc <> ls_stpo_open-issue_loc.
        lt_stpo_modify-id_item_no = ls_stpo_open-item_no."标识行项目
        lt_stpo_modify-issue_loc = ls_bom_item_abap-issue_loc.
      ENDIF.
      "替代项目组
      IF ls_bom_item_abap-ai_group <> ls_stpo_open-ai_group.
        lt_stpo_modify-id_item_no = ls_stpo_open-item_no."标识行项目
        lt_stpo_modify-ai_group = ls_bom_item_abap-ai_group.
      ENDIF.
      "策略
      IF ls_bom_item_abap-ai_strateg <> ls_stpo_open-ai_strateg.
        lt_stpo_modify-id_item_no = ls_stpo_open-item_no."标识行项目
        lt_stpo_modify-ai_strateg = ls_bom_item_abap-ai_strateg.
      ENDIF.
      "使用可能性
      IF ls_bom_item_abap-usage_prob <> ls_stpo_open-usage_prob.
        lt_stpo_modify-id_item_no = ls_stpo_open-item_no."标识行项目
        lt_stpo_modify-usage_prob = ls_bom_item_abap-usage_prob.
      ENDIF.
    ELSE."新增行操作
      lt_stpo_modify-item_no = ls_bom_item_abap-item_no."新增行标识
      lt_stpo_modify-item_categ  = ls_bom_item_abap-item_categ.
      lt_stpo_modify-component = ls_bom_item_abap-component.
      lt_stpo_modify-comp_qty = ls_bom_item_abap-comp_qty.
      lt_stpo_modify-comp_unit  = ls_bom_item_abap-comp_unit.
      lt_stpo_modify-comp_scrap  = ls_bom_item_abap-comp_scrap.
      lt_stpo_modify-fixed_qty  = ls_bom_item_abap-fixed_qty.
      lt_stpo_modify-sortstring  = ls_bom_item_abap-sortstring."排序字符串量
      lt_stpo_modify-issue_loc  = ls_bom_item_abap-issue_loc."生产仓储地点
      lt_stpo_modify-ai_group  = ls_bom_item_abap-ai_group."替代项目组
      lt_stpo_modify-ai_strateg  = ls_bom_item_abap-ai_strateg."策略
      lt_stpo_modify-usage_prob  = ls_bom_item_abap-usage_prob."使用可能性
      lt_stpo_modify-rel_prod  = 'X'."标识：与生产相关项目
      lt_stpo_modify-rel_cost  = 'X'."标识：与成本核算相关的项目
    ENDIF.

    IF lt_stpo_modify-id_item_no IS NOT INITIAL OR lt_stpo_modify-item_no IS NOT INITIAL.
      "客供料物料的BOM项目取消成本核算相关标识
      IF lt_stpo_modify-component IS NOT INITIAL.
        REFRESH lt_values.
        lt_values = zcl_bc_public=>get_class_values( key = lt_stpo_modify-component  classtype = '001' classnum = ls_bom_item_abap-comp_matkl objtable = 'MARA' ).
        READ TABLE lt_values WITH KEY charact = 'ZCPM'  INTO ls_value.
        IF sy-subrc = 0 AND ( ls_value-value_neutral = 'X' OR ls_value-value_neutral = '是' ).""客供来料，取消成本核算相关项的标识符
          lt_stpo_modify-rel_cost = '!'."cns_rel_cost_null."设置与成本核算相关的项目无关(函数CALO_INIT_API的输入参数data_reset_sign = '!')
        ENDIF.
      ENDIF.
      APPEND lt_stpo_modify.
    ENDIF.
  ENDLOOP.
  CLEAR lt_stpo_modify."初始化表头
  IF lt_stpo_modify[] IS  INITIAL.
     cs_ret_msg-type = 'S'.
      cs_ret_msg-message = |{ cs_ret_msg-message } { 'BOM项目未发生变化，无需修改' }|.
  ELSE.
* 3、修改BOM
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
    CLEAR ls_stko_modify.
    CALL FUNCTION 'CSAP_MAT_BOM_MAINTAIN'
      EXPORTING
        material           = is_bom_abap-material "抬头物料
        plant              = is_bom_abap-plant "工厂
        bom_usage          = is_bom_abap-bom_usage "物料清单用途
        alternative        = is_bom_abap-stlal "备选物料清单
        i_stko             = ls_stko_modify "BOM抬头结构
        "valid_from         = sy-datum "有效日期自-未启用ECM时，不能使用此参数
        fl_bom_create      = 'X' "经测试发现，此标识必须设置X，否则创建失败
        fl_new_item        = 'X'
        fl_commit_and_wait = 'X'
        fl_complete        = 'X'
        fl_default_values  = ''  "去掉设置默认值，成本核算标识相关等标识才能设置
*    IMPORTING
*       fl_warning         = g_warning
*       o_stko             =
      TABLES
        t_stpo             = lt_stpo_modify
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
      cs_ret_msg-type = 'S'.
      cs_ret_msg-message = |{ cs_ret_msg-message } { 'BOM项目修改成功' }|.
    ELSE.
      cs_ret_msg-type = 'E'.                 "消息类型
      cs_ret_msg-message = |{ cs_ret_msg-message } { 'BOM项目修改失败：' }{ sy-msgv1 }{ sy-msgv2 }{  sy-msgv3 }{ sy-msgv4 }|.
    ENDIF.
  ENDIF.
ENDFORM.
FORM frm_convert_char_to_num USING iv_menge_char CHANGING cv_menge TYPE stpo-menge cv-subrc TYPE syst_subrc.
  CLEAR:cv-subrc, cv_menge.
  CALL FUNCTION 'UNITS_STRING_CONVERT'
    EXPORTING
      units_string = iv_menge_char
      dcpfm        = 'X' "这个是根据su01里你的数字格式来设置，sap标准的格式有三种：空，X,Y
*     MLLN         = 'M'
*     TSND         = 'T'
    IMPORTING
      units        = cv_menge
    EXCEPTIONS
      invalid_type = 1
      OTHERS       = 2.
  cv-subrc = sy-subrc.
ENDFORM.
FORM frm_send_charval_to_oa USING is_bom_abap TYPE ty_req_bom_abap.
  DATA ls_mastb TYPE mastb.
  ls_mastb-matnr = is_bom_abap-material.
  ls_mastb-werks = is_bom_abap-plant.
  ls_mastb-stlan = '1'. "物料清单用途
  ls_mastb-stlal = is_bom_abap-stlal."备选物料清单
  CALL FUNCTION 'ZPP_SEND2OA_BY_PLMBOM_INTF' STARTING NEW TASK 'zplm001'
    EXPORTING
      is_mastb = ls_mastb.
ENDFORM.