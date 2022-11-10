FUNCTION z_mm_oa2sap_materialmaxprice.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     REFERENCE(INPUT) TYPE  ZMT_OA2SAP_MATERIAL_MAX_PRICE1
*"     REFERENCE(MESSAGE_ID) TYPE  SXMSGUID
*"  EXPORTING
*"     REFERENCE(OUTPUT) TYPE  ZMT_OA2SAP_MATERIAL_MAX_PRICE
*"----------------------------------------------------------------------
  DATA:lt_input  TYPE zdt_oa2sap_material_max_p_tab1,
       ls_input  TYPE zdt_oa2sap_material_max_price1,
       lt_output TYPE zdt_oa2sap_material_max_pr_tab,
       ls_output TYPE zdt_oa2sap_material_max_price.
  DATA:BEGIN OF ls_head,
         infnr      TYPE eine-infnr,
         ekorg      TYPE eine-ekorg,
         werks      TYPE eine-werks,
         loekz_eine TYPE eine-loekz,
         loekz_eina TYPE eina-loekz,
         matnr      TYPE eina-matnr,
*         date       TYPE a017-datbi,
       END OF ls_head.
  DATA: BEGIN OF ls_price,
          topprice TYPE konp-kbetr,
          kpein    TYPE konp-kpein,
          temp     TYPE konp-kbetr,
          sumprice TYPE konp-kbetr,
          lifnr    TYPE a017-lifnr,
        END OF ls_price.
  DATA:lv_matnr TYPE matnr,
       lv_len   TYPE i.
  DATA:lt_head     LIKE TABLE OF ls_head.




  "数据校验，判断是否非空
  lt_input = input-mt_oa2sap_material_max_price-list_of_inforec.
  IF lt_input IS NOT INITIAL.
    READ TABLE lt_input INTO ls_input INDEX 1.
    IF ls_input-matnr IS INITIAL OR ls_input-werks IS INITIAL.
      output-mt_oa2sap_material_max_price_r-ret_code = 'E'.
      output-mt_oa2sap_material_max_price_r-ret_msg = '必填数据为空，请检查数据'.
      RETURN.
    ELSE.
      CLEAR:ls_input.
    ENDIF.
  ENDIF.

*最高单价逻辑运算：SAP根据同个物料+同个工厂 + 非冻结（关联EINA和EINE表，判断EINE – LOEKZ或者EINA – LOEKZ不为X） + 非失效期（A017表信息记录开始日期 <= 传输日期 <= 信息记录结束日期 ）下所有信息记录最高单价【价格/定价单位】（KONP表）；
*如果定价单位为空的话，最高单价为空，返回给到OA

  "循环读取数据
  LOOP AT lt_input INTO ls_input.

    ls_input-lifnr = zcl_bc_public=>conv_by_ddic( i_input = ls_input-lifnr i_abap = 'LIFNR').
    "先判断是否为MPN物料，如果长度11位为MPN，否则正常
    lv_len = cl_abap_list_utilities=>dynamic_output_length( ls_input-matnr ).
    IF lv_len = 9.
      lv_matnr = '%' && ls_input-matnr && '%'.
    ELSE.
      lv_matnr = '%' && ls_input-matnr+0(9) && '%'.
    ENDIF.

    "筛选出符合条件的物料还有工厂
    SELECT
      eina~infnr
      ekorg
      werks
      eine~loekz AS loekz_eine
      eina~loekz AS loekz_eina
      matnr
    INTO CORRESPONDING FIELDS OF TABLE lt_head
    FROM eine
    INNER JOIN eina
    ON eine~infnr = eina~infnr
    WHERE matnr LIKE lv_matnr
      AND werks = ls_input-werks
      AND  eine~loekz <> 'X' AND eina~loekz <> 'X'
      AND eina~lifnr <> ls_input-lifnr.

    IF lt_head IS NOT INITIAL.

      LOOP AT lt_head INTO ls_head.
        "取出效期内的条件编号
        SELECT
          *
        FROM konp
        INNER JOIN a017
        ON konp~knumh = a017~knumh
        APPENDING TABLE @DATA(lt_a017)
        WHERE a017~matnr = @ls_head-matnr
          AND a017~werks = @ls_head-werks
          AND datab <= @ls_input-date
          AND datbi >= @ls_input-date
          AND a017~lifnr <> @ls_input-lifnr.
        CLEAR ls_head.
      ENDLOOP.

      "对数据进行处理
      IF lt_a017 IS NOT INITIAL.
        "循环判断，取到最高单价、定价单位、价格
        LOOP AT  lt_a017 ASSIGNING FIELD-SYMBOL(<fs_a017>).
          "定价单位为0的话，单价为0
          IF <fs_a017>-konp-kpein IS INITIAL OR <fs_a017>-konp-kpein = 0.
            CONTINUE.
          ENDIF.

          ls_price-temp = <fs_a017>-konp-kbetr / <fs_a017>-konp-kpein.

          IF ls_price-topprice IS INITIAL.
            ls_price-topprice = ls_price-temp.
            ls_price-kpein = <fs_a017>-konp-kpein.
            ls_price-sumprice = <fs_a017>-konp-kbetr.
            ls_price-lifnr = <fs_a017>-a017-lifnr.


            MOVE-CORRESPONDING ls_price TO ls_output.
          ELSEIF ls_price-temp > ls_price-topprice .
            ls_price-topprice = ls_price-temp.
            ls_price-kpein = <fs_a017>-konp-kpein.
            ls_price-sumprice = <fs_a017>-konp-kbetr.
            ls_price-lifnr = <fs_a017>-a017-lifnr.

            MOVE-CORRESPONDING ls_price TO ls_output.
          ENDIF.
        ENDLOOP.

      ENDIF.

    ENDIF.

    "上面取到了最高单价之后，赋值其他字段
    ls_output-matnr = ls_input-matnr.
    ls_output-werks = ls_input-werks.
    APPEND ls_output TO lt_output.
    CLEAR ls_output.
    CLEAR:ls_input,ls_head,lt_a017,ls_price,lv_matnr.
  ENDLOOP.

  output-mt_oa2sap_material_max_price_r-list_of_inforec = lt_output.
  output-mt_oa2sap_material_max_price_r-ret_code = 'S'.
  output-mt_oa2sap_material_max_price_r-ret_msg = '成功'.





ENDFUNCTION.