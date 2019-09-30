FUNCTION z_mm_sap2wms_out_fun.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     REFERENCE(IM_HEADER) TYPE  EBELN
*"     REFERENCE(IV_INTF_ID) TYPE  MMPUR_BOOL OPTIONAL
*"  EXPORTING
*"     REFERENCE(OUTPUT) TYPE  ZSMESSAGE_OUT
*"----------------------------------------------------------------------
  DATA: lr_service      TYPE REF TO zco_si_wms_subk_ret,
        lr_system_fault TYPE REF TO cx_ai_system_fault,
        ls_output       TYPE zmt_wms_subk,
        ls_item         TYPE zdt_wms_subk_list_of_bom,
        lt_item         TYPE TABLE OF zdt_wms_subk_list_of_bom_tab,
        ls_input        TYPE zmt_wms_subk_ret.
  DATA: lv_intf_id(6) TYPE c,
        lv_guid       TYPE sxmsguid,
        lv_err_info   TYPE bapi_msg.
  DEFINE  message_exit.
    output-success = &1.
    output-message = &2.
    IF &3 = 'X'.
      EXIT.
    ENDIF.
  END-OF-DEFINITION.

*----------------------------------------------------------------------
* 检查接口是否激活
*----------------------------------------------------------------------
  lv_intf_id = 'MM022'. "销售订单上传APS->SAP

  CALL FUNCTION 'Z_PI_CK'
    EXPORTING
      intf_id  = lv_intf_id
    IMPORTING
      err_info = lv_err_info.

  IF lv_err_info IS NOT INITIAL.
    output-message =  '接口开关没有打开'.
    output-success =  'E'.
  ENDIF.

*获取接口Message ID
  PERFORM frm_get_guid CHANGING lv_guid.
*日志记录
  DATA(l_flag) = zcl_bc_public=>write_log( iv_logid = CONV #( lv_guid )
        iv_intid = 'MM022'    "OA传输SAP规格标准
        iv_ptype = 'I'
        is_data = im_header ). "记录输入参数

  TRY.
      CREATE OBJECT lr_service.
    CATCH cx_ai_system_fault .
      message_exit 'E' '创建代理类失败，请联系IT' 'X'.
  ENDTRY.

  DATA ls_header TYPE mepoheader.
*  IF sy-tcode = 'ME29N'. " 判断当前的事务代码是不是ME29N，为单行审批
*    IF ls_header-bsart = 'ZWW'." 判断是否是委外采购类型
*      IF ls_header-frgzu = 'X'." 判断是否已经保存，如果保存了的话是X
*首先先扫地
  CLEAR: ls_output, ls_input.
*然后插入数据
  PERFORM insertdata USING im_header CHANGING ls_output.
*      ENDIF.
*    ENDIF.
*  ENDIF.




















*  接口调用
  DATA: lv_error           TYPE c,

        lo_api             TYPE REF TO zco_si_wms_subk_ret,
        lo_system_fault    TYPE REF TO cx_ai_system_fault,
        lo_async_messaging TYPE REF TO if_wsprotocol_async_messaging,
        l_ack_request      TYPE prx_ack_request_details.

  CREATE OBJECT lo_api.
  TRY.
      CALL METHOD lo_api->si_wms_subk_ret
        EXPORTING
          output = ls_output
        IMPORTING
          input  = ls_input.
    CATCH cx_ai_system_fault INTO lo_system_fault.
      lv_error = 'E'.
  ENDTRY.
  IF lv_error <> 'E'.
    COMMIT WORK AND WAIT.
    MESSAGE '数据下发成功！' TYPE 'S'.
  ENDIF.




ENDFUNCTION.

*插入数据的模块
FORM insertdata USING im_header TYPE ebeln
                CHANGING output TYPE zmt_wms_subk.
  DATA: lr_service      TYPE REF TO zco_si_wms_subk_ret,
        lr_system_fault TYPE REF TO cx_ai_system_fault,
        ls_output       TYPE zmt_wms_subk,
        ls_output2      TYPE zdt_wms_subk,
        ls_item         TYPE zdt_wms_subk_list_of_bom,
        lt_item         TYPE zdt_wms_subk_list_of_bom_tab,
        ls_input        TYPE zmt_wms_subk_ret,
        lt_line         TYPE zdt_wms_subk_lines_of_char_tab,
        ls_line         TYPE zdt_wms_subk_lines_of_characte.
  TYPES: BEGIN OF ty_list,
           wms_custid   TYPE ekpo-werks,
           ebeln        TYPE ekko-ebeln,
           bus_type(4)  TYPE c,
           lifnr        TYPE  ekko-lifnr,
           name1_gp     TYPE lfa1-name1,
           ernam        TYPE ekko-ernam,
           note(40)     TYPE c,
           ebelp        TYPE ekpo-ebelp,
           matnr        TYPE ekpo-matnr,
           maktx        TYPE makt-maktx,
           pdlv_date    TYPE string,
           pdlv_menge   TYPE ekpo-menge,
           meins        TYPE ekpo-meins,
           mseht        TYPE t006a-mseht,
           werks        TYPE ekpo-werks,
           lgort        TYPE ekpo-lgort,
           lf_mat(70)   TYPE c,
           st_code      TYPE ekpo-st_code,
           xcanceled(1) TYPE c,
         END OF ty_list.
  TYPES: BEGIN OF ty_list_item,
           stlkn          TYPE resb-stlkn,
           matnr_bom      TYPE resb-matnr,
           matnr_bom_desc TYPE makt-maktx,
           menge_bom      TYPE resb-erfmg,
           meins_bom      TYPE resb-erfme,
           mseht_bom      TYPE t006a-mseht,
           charg          TYPE resb-charg,
           atnam(70)      TYPE c,
           atwrt(70)      TYPE c,
         END OF ty_list_item.

  DATA: gt_list      TYPE TABLE OF ty_list,
        gs_list      TYPE ty_list,
        gt_list_item TYPE TABLE OF ty_list_item,
        gs_list_item TYPE ty_list_item,
        gt_tline     TYPE TABLE OF tline,
        gs_tline     TYPE tline.
  DATA: lt_ekpo  TYPE TABLE OF ekpo,
        ls_ekpo  TYPE ekpo,
        lt_ekko  TYPE TABLE OF ekko,
        ls_ekko  TYPE ekko,
        lt_lfa1  TYPE TABLE OF lfa1,
        ls_lfa1  TYPE lfa1,
        lt_makt  TYPE TABLE OF makt,
        ls_makt  TYPE makt,
        lt_eket  TYPE TABLE OF eket,
        ls_eket  TYPE eket,
        lt_t006a TYPE TABLE OF t006a,
        ls_t006a TYPE t006a,
        lt_resb  TYPE TABLE OF resb,
        ls_resb  TYPE resb,
        lt_resb2 TYPE TABLE OF resb,
        ls_resb2 TYPE resb,
        lt_resb3 TYPE TABLE OF resb,
        ls_resb3 TYPE resb.


  DATA:lv_e_objek TYPE  cuobn,
       lv_e_obtab TYPE  tabelle,
       lv_e_klart TYPE  klassenart,
       lv_e_class TYPE  klasse_d.

  DATA:lv_objectkey      TYPE  bapi1003_key-object,
       lv_objecttable    TYPE  bapi1003_key-objecttable,
       lv_classnum       TYPE  bapi1003_key-classnum,
       lv_classtype      TYPE  bapi1003_key-classtype,
       lv_objectkey_long TYPE  bapi1003_key-object_long.

  DATA: lt_allocvaluesnum  TYPE TABLE OF bapi1003_alloc_values_num,
        lt_allocvalueschar TYPE TABLE OF bapi1003_alloc_values_char,
        lt_allocvaluescurr TYPE TABLE OF bapi1003_alloc_values_curr,
        lt_return          TYPE TABLE OF bapiret2.
  " 行项目查询
  SELECT SINGLE
*    lifnr
*    ernam
*    ebeln
*    lponr
    *
  INTO CORRESPONDING FIELDS OF ls_ekko
  FROM ekko
  WHERE ekko~ebeln = im_header.
  IF ls_ekko IS NOT INITIAL.
    SELECT SINGLE
*      werks
*      ebelp
*      matnr
*      menge
*      meins
*      lgort
*      st_code
*      loekz
*      elikz
      *
    INTO CORRESPONDING FIELDS OF ls_ekpo
    FROM ekpo
    WHERE ekpo~ebeln = im_header
    AND   ekpo~ebelp = ls_ekko-lponr.

    " 添加关闭标记，修改时间2019.9.5号晚
    IF ls_ekpo-loekz <> '' OR ls_ekpo-elikz <> ''.
      gs_list-xcanceled = 'X'.
    ELSE.
      gs_list-xcanceled = ''.
    ENDIF.
    IF ls_ekpo IS NOT INITIAL.
      SELECT SINGLE
*      eindt
        *
      INTO CORRESPONDING FIELDS OF ls_eket
      FROM eket
      WHERE eket~ebeln = im_header
      AND   eket~ebelp = ls_ekpo-ebelp.

      SELECT SINGLE
*      mseht
        *
      INTO CORRESPONDING FIELDS OF ls_t006a
      FROM t006a
      WHERE t006a~spras = '1'
      AND   t006a~msehi = ls_ekpo-meins.
    ENDIF.

    SELECT SINGLE
*    name1
      *
    INTO CORRESPONDING FIELDS OF ls_lfa1
    FROM lfa1
    WHERE lfa1~lifnr = ls_ekko-lifnr.

    SELECT SINGLE
*    maktx
      *
    INTO CORRESPONDING FIELDS OF ls_makt
    FROM makt
    WHERE makt~matnr = ls_ekpo-matnr
    AND   makt~spras = '1'.
  ENDIF.

  " ITEM的内容查询
  SELECT
*    matnr
*    ebeln
*    ebelp
*    stlkn
*    erfme
*    charg
*    werks
    *
  INTO CORRESPONDING FIELDS OF TABLE lt_resb2
  FROM resb
  WHERE resb~ebeln = im_header
  AND   resb~ebelp = ls_ekpo-ebelp.


  SORT lt_resb2 BY stlkn.
  DATA lv_value TYPE thead-tdname.
  lv_value = ls_ekpo-matnr.
* 处理NOTE和LF_MAT
  CALL FUNCTION 'READ_TEXT'
    EXPORTING
*     CLIENT                  = SY-MANDT
      id                      = 'GRUN'
      language                = sy-langu
      name                    = lv_value
      object                  = 'MATERIAL'
*     ARCHIVE_HANDLE          = 0
*     LOCAL_CAT               = ' '
* IMPORTING
*     HEADER                  =
*     OLD_LINE_COUNTER        =
    TABLES
      lines                   = gt_tline
    EXCEPTIONS
      id                      = 1
      language                = 2
      name                    = 3
      not_found               = 4
      object                  = 5
      reference_check         = 6
      wrong_access_to_archive = 7
      OTHERS                  = 8.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.
  READ TABLE gt_tline INTO gs_tline INDEX 1.
  gs_list-note = gs_tline-tdline.
  CLEAR gt_tline.
  CLEAR gs_tline.
  DATA:lv_value2 TYPE thead-tdname.
  lv_value2 = ls_ekpo-infnr.
  CALL FUNCTION 'READ_TEXT'
    EXPORTING
*     CLIENT                  = SY-MANDT
      id                      = 'AT'
      language                = sy-langu
      name                    = lv_value2
      object                  = 'EINA'
*     ARCHIVE_HANDLE          = 0
*     LOCAL_CAT               = ' '
* IMPORTING
*     HEADER                  =
*     OLD_LINE_COUNTER        =
    TABLES
      lines                   = gt_tline
    EXCEPTIONS
      id                      = 1
      language                = 2
      name                    = 3
      not_found               = 4
      object                  = 5
      reference_check         = 6
      wrong_access_to_archive = 7
      OTHERS                  = 8.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.
  READ TABLE gt_tline INTO gs_tline INDEX 1.
  gs_list-lf_mat = gs_tline-tdline.


*强制输入测试数据
*  gs_list-note = 'NOTE'.
*  gs_list-lf_mat = 'LF_MAT'.
*  gs_list-st_code = '1'.


*这中间进行前面取数存入gs_list
  gs_list-wms_custid = ls_ekpo-werks.
  gs_list-ebeln = im_header.
  gs_list-bus_type = '101L'.
  gs_list-lifnr = zcl_bc_public=>conv_by_ddic( i_input = ls_ekko-lifnr i_out = 'X' )."ls_ekko-lifnr.
  gs_list-name1_gp = ls_lfa1-name1.
  gs_list-ernam = ls_ekko-ernam.
  " gs_list-NOTE =
  gs_list-ebelp = ls_ekpo-ebelp.
*  <fs_output>-vbeln = zcl_bc_public=>conv_by_ddic( i_input = i_input-header-vbeln i_out = 'X' ).
*  gs_list-matnr = ls_ekpo-matnr.
  gs_list-matnr = zcl_bc_public=>conv_by_ddic( i_input = ls_ekpo-matnr i_out = 'X' ).
  gs_list-maktx = ls_makt-maktx.
  DATA lv_date TYPE  string.
  DATA lv_string1 TYPE string.
  DATA lv_string2 TYPE string.
  lv_date = ls_eket-eindt.
*  gs_list-pdlv_date = lv_date+0(4) && '-' && lv_date+4(2) && '-' && lv_date+6(2) &&
*  lv_string && '00:00:00'.
  lv_string1 = lv_date+0(4) && '-' && lv_date+4(2) && '-' && lv_date+6(2).
  lv_string2 = '00:00:00'.
  CONCATENATE lv_string1 lv_string2 INTO gs_list-pdlv_date SEPARATED BY '  '.
*  gs_list-pdlv_date = ls_eket-eindt.
  gs_list-pdlv_menge = ls_ekpo-menge.
  gs_list-meins = ls_ekpo-meins.
  gs_list-mseht = ls_t006a-mseht.
  gs_list-werks = ls_ekpo-werks.
  gs_list-lgort = ls_ekpo-lgort.




  gs_list-bus_type = 'B05'.
*  gs_list-lgort = '123'.
*      GS_LIST-lf_mat =
  gs_list-st_code = ls_ekpo-st_code.
*插入表头数据
  DATA:ls_out_header TYPE zdt_wms_subk.
  ls_out_header = ls_output-mt_wms_subk.
  MOVE-CORRESPONDING gs_list TO ls_out_header.


  IF lt_resb2 IS NOT INITIAL.
    LOOP AT lt_resb2 INTO ls_resb2.

*      <fs_output>-ls_resb2-matnr = zcl_bc_public=>conv_by_ddic( i_input = i_input-header-ls_resb2-matnr i_out = 'X' ).

      SELECT
        a~stlkn  AS stlkn
        a~matnr  AS matnr_bom
        c~maktx AS matnr_bom_desc
        a~erfmg  AS menge_bom
        t006a~mseht AS mseht_bom
        a~charg AS charg
        a~erfme AS meins_bom
      FROM resb AS a
      INNER JOIN makt AS c ON c~matnr = ls_resb2-matnr
      INNER JOIN ekpo AS b ON  a~ebeln = b~ebeln AND a~ebelp = b~ebelp
      INNER JOIN t006a
      ON t006a~msehi = ls_resb2-meins
      INTO CORRESPONDING FIELDS OF TABLE gt_list_item
      WHERE a~matnr = ls_resb2-matnr
          AND     a~ebeln = ls_resb2-ebeln
          AND    a~ebelp = ls_resb2-ebelp
          AND t006a~spras = '1'.


*      SELECT
**        t006a~mseht
*        *
*      INTO CORRESPONDING FIELDS OF TABLE gt_list_item
*      FROM resb
*      INNER JOIN ekpo
*      ON ekpo~ebeln = resb~ebeln
*      AND ekpo~ebelp = resb~ebelp
*      INNER JOIN t006a
*      ON t006a~msehi = ekpo~meins
*      WHERE t006a~spras = '1'
*      AND   resb~stlkn = ls_resb2-stlkn.


      LOOP AT gt_list_item INTO gs_list_item.




        " 获取批次属性名还有批次属性值
        IF ls_resb2-charg IS NOT INITIAL.
**          获取物料的批次信息
          CALL FUNCTION 'VB_BATCH_2_CLASS_OBJECT'
            EXPORTING
              i_matnr = ls_resb2-matnr
              i_charg = ls_resb2-charg
              i_werks = ls_resb2-werks
            IMPORTING
              e_objek = lv_e_objek
              e_obtab = lv_e_obtab
              e_klart = lv_e_klart
              e_class = lv_e_class.
**          获取批次对象的属性信息
          IF sy-subrc = 0.
            lv_objectkey  =    lv_e_objek.
            lv_objecttable   = lv_e_obtab.
            lv_classnum  = lv_e_class.
            lv_classtype  = lv_e_klart.
            lv_objectkey_long = lv_e_objek.
            CALL FUNCTION 'BAPI_OBJCL_GETDETAIL'
              EXPORTING
                objectkey       = lv_objectkey
                objecttable     = lv_objecttable
                classnum        = lv_classnum
                classtype       = lv_classtype
                objectkey_long  = lv_objectkey_long
              TABLES
                allocvaluesnum  = lt_allocvaluesnum
                allocvalueschar = lt_allocvalueschar
                allocvaluescurr = lt_allocvaluescurr
                return          = lt_return.
            IF lt_return IS NOT INITIAL AND lt_return[ 1 ]-type = 'S'.

              "lt_allocvaluesnum部分

***           生产日期
              READ TABLE lt_allocvaluesnum WITH KEY charact = 'ZHSDAT' INTO DATA(ls_allocvaluesnum).
              IF sy-subrc = 0.
*              ls_resp-zpspe = ls_allocvaluesnum-value_from.
                ls_line-atnam = 'ZHSDAT'.
                DATA date LIKE cawn-atwrt.
*                将科学计数法的数值转换成为字符型
                CALL FUNCTION 'CTCV_CONVERT_FLOAT_TO_DATE'
                  EXPORTING
                    float = ls_allocvaluesnum-value_from
                  IMPORTING
                    date  = date.
                ls_line-atwrt = date.
                CLEAR date.
                APPEND ls_line TO lt_line.
              ENDIF.
              CLEAR ls_allocvaluesnum.
              " 入库日期
              READ TABLE lt_allocvaluesnum WITH KEY charact = 'ZLWEDT' INTO ls_allocvaluesnum.
              IF sy-subrc = 0.
*              ls_resp-zbatch = ls_allocvaluesnum-value_from.
                ls_line-atnam = 'ZLWEDT'.
                "将科学计数法的数值转换成为字符型
                CALL FUNCTION 'CTCV_CONVERT_FLOAT_TO_DATE'
                  EXPORTING
                    float = ls_allocvaluesnum-value_from
                  IMPORTING
                    date  = date.
                ls_line-atwrt = date.
                CLEAR date.
                APPEND ls_line TO lt_line.
              ENDIF.
              CLEAR ls_allocvaluesnum.
              "失效日期
              READ TABLE lt_allocvaluesnum WITH KEY charact = 'ZVFDAT' INTO ls_allocvaluesnum.
              IF sy-subrc = 0.
*              ls_resp-zbatch = ls_allocvaluesnum-value_from.
                ls_line-atnam = 'ZVFDAT'.
                "将科学计数法的数值转换成为字符型
                CALL FUNCTION 'CTCV_CONVERT_FLOAT_TO_DATE'
                  EXPORTING
                    float = ls_allocvaluesnum-value_from
                  IMPORTING
                    date  = date.
                ls_line-atwrt = date.
                CLEAR date.
                APPEND ls_line TO lt_line.
              ENDIF.
              CLEAR ls_allocvaluesnum.



              " 供应商编码
              "ALLOCVALUESCHAR部分
              READ TABLE lt_allocvalueschar WITH KEY charact = 'ZLIFNR' INTO DATA(ls_allocvalueschar).
              IF sy-subrc = 0.
*              ls_resp-zbatch = ls_allocvalueschar-value_char.
                ls_line-atnam = 'ZLIFNR'.
                ls_line-atwrt = ls_allocvalueschar-value_char.
                APPEND ls_line TO lt_line.
              ENDIF.
              CLEAR ls_allocvalueschar.
              " 包装规格
              READ TABLE lt_allocvalueschar WITH KEY charact = 'ZPACK_STA' INTO ls_allocvalueschar.
              IF sy-subrc = 0.
*              ls_resp-zbatch = ls_allocvalueschar-value_char.
                ls_line-atnam = 'ZPACK_STA'.
                ls_line-atwrt = ls_allocvalueschar-value_char.
                APPEND ls_line TO lt_line.
              ENDIF.
              CLEAR ls_allocvalueschar.
              " 制造商零件编号
              READ TABLE lt_allocvalueschar WITH KEY charact = 'ZPRODUCER' INTO ls_allocvalueschar.
              IF sy-subrc = 0.
*              ls_resp-zbatch = ls_allocvalueschar-value_char.
                ls_line-atnam = 'ZPRODUCER'.
                ls_line-atwrt = ls_allocvalueschar-value_char.
                APPEND ls_line TO lt_line.
              ENDIF.
              CLEAR ls_allocvalueschar.
            ENDIF.
          ENDIF.
        ENDIF.
**
        ls_item-lines_of_character = lt_line.

        gs_list_item-matnr_bom = zcl_bc_public=>conv_by_ddic( i_input = gs_list_item-matnr_bom i_out = 'X' ).
        " 检验加工前物料号是否启用了批次管理,MARC-XCHPF=X,  检查，如果每填，进行提示
        DATA:lt_marc TYPE TABLE OF marc,
             ls_marc TYPE marc.
        SELECT SINGLE
          xchpf
        INTO CORRESPONDING FIELDS OF ls_marc
        FROM marc
        WHERE marc~matnr = gs_list_item-matnr_bom.

        " 下传质量标识、外寄标识、寄售标识
        " ZST_CODE 质量标识
        " ZOUT_X 外寄标识
        DATA:LS_ZMMWMS_EXT_ST TYPE ZMMWMS_EXT_ST.
        " 获取质量标识的值
        SELECT SINGLE
          ATWRT
        INTO LS_ZMMWMS_EXT_ST-zst_code
        FROM AUSP
        WHERE AUSP~objek = gs_list_item-matnr_bom
          AND AUSP~ATINN = 'ZST_CODE'.
        " 获取外寄标识的值
        SELECT SINGLE
        ATWRT
        INTO LS_ZMMWMS_EXT_ST-zout_x
        FROM AUSP
        WHERE AUSP~objek = gs_list_item-matnr_bom
          AND AUSP~ATINN = 'ZOUT_X'.
        " 获取寄售标识
        SELECT SINGLE
          SOBKZ
        INTO LS_ZMMWMS_EXT_ST-xconsign
        FROM MKOL
        WHERE MKOL~matnr = gs_list_item-matnr_bom
        AND MKOL~werks = ls_ekpo-werks
        AND MKOL~charg = gs_list_item-charg.

        SELECT SINGLE
        WMS_EXT_ST
        INTO LS_ZMMWMS_EXT_ST-wms_ext_st
        FROM ZMMWMS_EXT_ST
        WHERE ZMMWMS_EXT_ST~zout_x = LS_ZMMWMS_EXT_ST-zout_x
        AND ZMMWMS_EXT_ST~zst_code = LS_ZMMWMS_EXT_ST-zst_code
        AND ZMMWMS_EXT_ST~xconsign = LS_ZMMWMS_EXT_ST-xconsign.

        ls_item-wms_ext_st = LS_ZMMWMS_EXT_ST-wms_ext_st.
        MOVE-CORRESPONDING gs_list_item TO ls_item.
*        质量状态默认非限制
        ls_item-insmk = ''.
*        添加行项目的库位地点
        SELECT
          *
        FROM resb
        INTO CORRESPONDING FIELDS OF TABLE lt_resb3
        WHERE resb~matnr = ls_resb2-matnr
        AND resb~baugr = ls_ekpo-matnr
        ORDER BY resb~rsnum DESCENDING.
        READ TABLE lt_resb3 INTO ls_resb3 INDEX 1.
        ls_item-lgort_out = ls_resb3-lgort.
        " 检验发出库存地点是否填写，要求的是必填的



        ls_item-stlkn = ls_resb3-rspos.
        APPEND ls_item TO lt_item.
      ENDLOOP.
      " 获取结束
      IF gs_list IS NOT INITIAL AND lt_item IS NOT INITIAL.
*        DATA lv_date TYPE  string.
*        lv_date = gs_list-pdlv_date.
*        CLEAR gs_list-pdlv_date.
*        gs_list-pdlv_date = lv_date+0(4) && '-' && lv_date+4(2) && '-' && lv_date+6(2) &&
*                            ' ' && '00:00:00'.
*        WRITE gs_list-pdlv_date TO lv_date USING EDIT MASK '____-__-__ '.
*        gs_list-pdlv_date = lv_date && ' 00:00:00'.
        ls_output2-list_of_bom = lt_item.
        MOVE-CORRESPONDING ls_output2 TO ls_output-mt_wms_subk.
        MOVE-CORRESPONDING gs_list TO ls_output-mt_wms_subk.
        MOVE-CORRESPONDING ls_output-mt_wms_subk TO output-mt_wms_subk.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDFORM.


*----------------------------------------------------------------------
*   获取接口Message ID
*----------------------------------------------------------------------
FORM frm_get_guid CHANGING cv_guid.
  DATA: lr_server_context   TYPE REF TO if_ws_server_context,
        lr_message_protocol TYPE REF TO if_wsprotocol_message_id.
  TRY .
      lr_server_context  = cl_proxy_access=>get_server_context( ).
      lr_message_protocol ?= lr_server_context->get_protocol( if_wsprotocol=>message_id ).
      cv_guid = lr_message_protocol->get_message_id( ).
    CATCH cx_ai_system_fault INTO DATA(lr_sys_exception).
      EXIT.
  ENDTRY.
ENDFORM.