FUNCTION z_pp_bom_query.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     VALUE(IV_AUMNG) TYPE  STKO-BMENG DEFAULT 0
*"  TABLES
*"      IT_REQ STRUCTURE  ZPPS_BOM_REQ
*"      IT_RESP_HEADER STRUCTURE  ZPPS_BOM_RESP_HEADER
*"      IT_RESP_ITEM STRUCTURE  ZPPS_BOM_RESP_ITEM
*"  EXCEPTIONS
*"      INVALID_REQ_PARAS
*"----------------------------------------------------------------------
***  请求参数不能为空
  CLEAR es_bapiret.
  IF it_req[] IS  INITIAL.
    es_bapiret-type = 'E'.
    es_bapiret-message = '请求参数为空'.
    RETURN.
  ENDIF.
  SORT it_req BY matnr werks zblx zbzkbs.
  DELETE ADJACENT DUPLICATES FROM it_req.
***  请求参数有效性检查
  CLEAR:gt_bom_req_valid,gt_bom_req_valid_1,gt_bom_req_valid_2.
  LOOP AT it_req INTO DATA(ls_req).
    IF ls_req-matnr IS NOT INITIAL AND ls_req-werks IS NOT  INITIAL AND ls_req-zblx IS NOT INITIAL AND ls_req-zbzkbs IS NOT INITIAL.
      CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
        EXPORTING
          input  = ls_req-matnr
        IMPORTING
          output = ls_req-matnr.

      IF ls_req-zblx = '1'. "正常BOM
        gt_bom_req_valid = VALUE #( BASE gt_bom_req_valid (
        werks = ls_req-werks
        matnr = ls_req-matnr
        zbzkbs = ls_req-zbzkbs
        ) ).

        gt_bom_req_valid_1 = VALUE #( BASE gt_bom_req_valid_1 (
        zid = sy-tabix
        werks = ls_req-werks
        matnr = ls_req-matnr
        zbzkbs = ls_req-zbzkbs
        zblx = ls_req-zblx
        ) ).
      ELSEIF ls_req-zblx = '2'. "返工BOM
        gt_bom_req_valid = VALUE #( BASE gt_bom_req_valid (
        werks = ls_req-werks
        matnr = ls_req-matnr
        zbzkbs = ls_req-zbzkbs
        ) ).

        gt_bom_req_valid_2 = VALUE #( BASE gt_bom_req_valid_2 (
        zid = sy-tabix
        werks = ls_req-werks
        matnr = ls_req-matnr
        zbzkbs = ls_req-zbzkbs
        zblx = ls_req-zblx
        ) ).
      ENDIF.
    ENDIF.
  ENDLOOP.

***   请求参数无效
  IF gt_bom_req_valid_1 IS INITIAL AND gt_bom_req_valid_2 IS INITIAL.
    es_bapiret-type = 'E'.
    es_bapiret-message = '请求参数为空'.
    RETURN.
  ENDIF.

*** 获取可选BOM(stlal)为最小值的BOM表头，参数ZBLX = 1 为正常BOM(stlal <> 09)
  IF gt_bom_req_valid_1 IS NOT INITIAL.
    SELECT  mast~matnr,mast~werks,mast~stlnr, mast~stlal,s~bmeng, s~bmein,s~stktx,s~stlst
    FROM mast
    JOIN stko AS s  ON  mast~stlnr = s~stlnr AND mast~stlal = s~stlal
    FOR ALL ENTRIES IN @gt_bom_req_valid_1
    WHERE mast~matnr = @gt_bom_req_valid_1-matnr
    AND mast~werks = @gt_bom_req_valid_1-werks
    AND mast~stlan = '1'  "BOM用途
**      最小的可选BOM
    AND mast~stlal =  ( SELECT MIN( stlal ) FROM mast WHERE stlal <> '09' AND matnr = @gt_bom_req_valid_1-matnr AND werks = @gt_bom_req_valid_1-werks AND stlan = '1')
    AND s~stlty = 'M' "BOM 类别
    AND s~datuv = ( SELECT MAX( datuv ) FROM stko WHERE stlty = 'M' AND stlnr = s~stlnr AND stlal = s~stlal AND  datuv <= @sy-datum )  "有效期
    AND s~loekz = ''    " 删除标识
    INTO TABLE @DATA(lt_stko).
  ENDIF.

*** 获取可选BOM(stlal)为最小值的BOM表头，参数ZBLX = 2为返工BOM(stlal = 9)
  IF gt_bom_req_valid_2 IS NOT INITIAL.
    SELECT  mast~matnr,mast~werks,mast~stlnr, mast~stlal,s~bmeng, s~bmein,s~stktx,s~stlst
    FROM mast
    JOIN stko AS s ON  mast~stlnr = s~stlnr AND mast~stlal = s~stlal
    FOR ALL ENTRIES IN @gt_bom_req_valid_2
    WHERE mast~matnr = @gt_bom_req_valid_2-matnr
    AND mast~werks = @gt_bom_req_valid_2-werks
    AND mast~stlan = '1'  "BOM用途
    AND mast~stlal = '09'  "可选BOM=9，为特殊BOM，用于返工等
    AND s~stlty = 'M' "BOM 类别
    AND s~datuv = ( SELECT MAX( datuv ) FROM stko WHERE stlty = 'M' AND stlnr = s~stlnr AND stlal = s~stlal AND  datuv <= @sy-datum )  "有效期
    AND s~loekz = ''   " 删除标识
    INTO TABLE @DATA(lt_stko_2).
  ENDIF.

  IF lt_stko_2 IS NOT INITIAL.
    INSERT LINES OF lt_stko_2 INTO TABLE lt_stko.
  ENDIF.


*** 展开BOM
  SORT gt_bom_req_valid BY matnr werks zbzkbs.
  DELETE ADJACENT DUPLICATES FROM gt_bom_req_valid.
  CLEAR: it_resp_header,it_resp_item.
  REFRESH:it_resp_header,it_resp_item.
  LOOP AT lt_stko INTO DATA(ls_stko).
    DATA(l_zid) = sy-tabix.
    READ TABLE gt_bom_req_valid  WITH KEY matnr = ls_stko-matnr werks = ls_stko-werks  BINARY SEARCH INTO DATA(ls_req_valid).
    IF sy-subrc <> 0.
      CONTINUE.
    ELSE.
      l_zbzkbs = ls_req_valid-zbzkbs.  "展开级别
    ENDIF.
    CLEAR:gt_stb, gt_matcat, gs_topmat, gv_dstst.
    CLEAR: lt_resp_item,"临时存储BOM项目
    lt_stb_dumps.  "临时存储BOM项目虚拟件
    TRY ."捕获BOM展开时的数量太大溢出异常
        lv_bmeng = ls_stko-bmeng * cns_baseqty. "扩大展BOM需求数量，为了减少张单位的小数误差
        IF l_zbzkbs = '2'.  "单层展开ls_stko-bmeng
          PERFORM bom_expl_mat USING lv_bmeng  abap_false ls_stko-matnr ls_stko-stlal ls_stko-werks iv_aumng
          CHANGING gt_stb gt_matcat gs_topmat gv_dstst.
          IF gt_stb IS NOT INITIAL.
*** BOM表头
            PERFORM set_bom_header USING l_zid gs_topmat CHANGING gs_resp_header.
            APPEND gs_resp_header TO it_resp_header.

**     BOM项目
            PERFORM set_bom_items USING l_zid "内表索引 用于关联表头
                  gt_stb  "BOM展开后的项目内表
                  abap_true   "是否收集虚拟件
                  lt_resp_item  "返回的BOM行项目内表
                  lt_stb_dumps.  " BOM展开后的虚拟件

***          多级展开虚拟物料的BOM
            IF lt_stb_dumps IS NOT INITIAL.
              SORT BY lt_stb_dumps.
              DELETE ADJACENT DUPLICATES FROM lt_stb_dumps.
              CLEAR gt_fmeng."相同物料BOM的固定数量组件
              LOOP AT lt_stb_dumps INTO DATA(ls_stb_dumps).
                CLEAR:gt_stb, gt_matcat, gs_topmat, gv_dstst.

***         虚拟件多级展BOM
                PERFORM bom_expl_mat_dump USING ls_stb_dumps-mnglg
                      ls_stb_dumps-idnrk
                      ls_stb_dumps-stlal
                      ls_stb_dumps-werks
                      ls_stb_dumps-fmeng "固定数量标识
                CHANGING gt_stb
                  gt_matcat
                  gs_topmat
                  gv_dstst.

***         添加非虚拟件的行项目
                PERFORM set_bom_items USING l_zid "  内表索引 用于关联表头
                      gt_stb "  BOM展开后的项目内表
                      abap_false   " 不收集虚拟件
                CHANGING lt_resp_item  "  返回的BOM行项目内表
                  lt_stb_dumps.  " BOM展开后的虚拟件
              ENDLOOP.
            ENDIF.
          ENDIF.
        ELSEIF l_zbzkbs = '1'."多层展开 ls_stko-bmeng
          PERFORM bom_expl_mat USING lv_bmeng  abap_true ls_stko-matnr ls_stko-stlal ls_stko-werks iv_aumng
          CHANGING gt_stb gt_matcat   gs_topmat gv_dstst.
***      *** BOM表头
          PERFORM set_bom_header USING l_zid gs_topmat CHANGING gs_resp_header.
          APPEND gs_resp_header TO it_resp_header.
***     整理非虚拟件的行项目
          PERFORM set_bom_items USING l_zid "  内表索引 用于关联表头
                gt_stb "  BOM展开后的项目内表
                abap_false   " 不收集虚拟件
          CHANGING lt_resp_item  "  返回的BOM行项目内表
            lt_stb_dumps.  " BOM展开后的虚拟件
        ENDIF.
        INSERT LINES OF lt_resp_item INTO TABLE it_resp_item.  "新增BOM行项目
      CATCH cx_sy_arithmetic_overflow INTO DATA(lo_ex).
        es_bapiret-type = 'E'.
        es_bapiret-message = |{ '工厂' }{ ls_stko-werks }{ '、物料' }{ ls_stko-matnr }{ '、备选物料清单' }{ ls_stko-stlal }{ '、展BOM数量放大' }{ cns_baseqty }{ '倍时溢出' }|.
        EXIT.
    ENDTRY.
  ENDLOOP.
ENDFUNCTION.