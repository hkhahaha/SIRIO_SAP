*&---------------------------------------------------------------------*
*& 包含               ZMMR0020F01
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Form FRM_INIT
*&---------------------------------------------------------------------*
*& 初始化
*&---------------------------------------------------------------------*
FORM frm_init .
    gs_stable-row = abap_true.
    gs_stable-col = abap_true.
    gs_layout-cwidth_opt = abap_true.
    gs_layout-sel_mode = 'D'.
  ENDFORM.
  
  *&---------------------------------------------------------------------*
  *&      Form  FRM_CHANGE_SCREEN
  *&---------------------------------------------------------------------*
  *       修改选择屏幕
  *----------------------------------------------------------------------*
  FORM frm_change_screen .
    CASE 'X'.
      WHEN p_query.
        LOOP AT SCREEN.
          IF screen-group1 = 'M1'.
            screen-active = 1.
          ELSEIF screen-group1 = 'M2'. .
            screen-active = 0.
          ENDIF.
          MODIFY SCREEN.
        ENDLOOP.
      WHEN p_time OR p_roh.
        LOOP AT SCREEN.
          IF p_time = 'X'.
            IF screen-group1 = 'M2'.
              screen-active = 1.
            ELSEIF screen-group1 = 'M1'  OR screen-group1 = 'M4'.
              screen-active = 0.
            ENDIF.
          ENDIF.
          IF p_roh = 'X'.
            IF screen-group1 = 'M4'.
              screen-active = 1.
            ELSEIF screen-group1 = 'M1'  OR screen-group1 = 'M3'.
              screen-active = 0.
            ENDIF.
          ENDIF.
          MODIFY SCREEN.
        ENDLOOP.
        "20220531 Seashell 新增供应周期的查询选项
      WHEN p_plifz.
        LOOP AT SCREEN.
  *        IF screen-group1 = 'M1'  OR screen-group1 = 'M3' OR screen-group1 = 'M2' OR screen-group1 = 'M4'.
          IF screen-group1 IS NOT INITIAL.
            screen-active = 0.
          ENDIF.
          MODIFY SCREEN.
        ENDLOOP.
      WHEN OTHERS.
    ENDCASE.
  ENDFORM.
  
  *&---------------------------------------------------------------------*
  *& Form FRM_AUTHORITY_CHECK
  *&---------------------------------------------------------------------*
  *& 权限检查
  *&---------------------------------------------------------------------*
  FORM frm_authority_check .
    DATA lv_timetype TYPE char10.
    DATA lv_timetype_desc TYPE char20.
    SELECT werks FROM t001w WHERE werks IN @s_werks INTO TABLE @DATA(lt_t001w).
    IF lt_t001w IS INITIAL.
      MESSAGE s000 WITH '未查询到工厂,请检查!'.
      LEAVE LIST-PROCESSING.
    ENDIF.
    IF p_halb = 'X'.
      lv_timetype = '1'."半成品生产时间维护权限对象
      lv_timetype_desc = '工厂物料组生产时间维护的权限'.
    ELSE.
      lv_timetype = '2'."原辅料准备时间权限对象
      lv_timetype_desc = '工厂原辅料准备时间维护的权限'.
    ENDIF.
    LOOP AT lt_t001w INTO DATA(ls_t001w).
      AUTHORITY-CHECK OBJECT 'ZPP004'
       ID 'WERKS' FIELD ls_t001w-werks
       ID 'ZTIMETYPE' FIELD lv_timetype.
      IF sy-subrc <> 0.
        MESSAGE s000 WITH |{ '您没有' }{ ls_t001w-werks }{ '工厂' }{ lv_timetype_desc }|.
        LEAVE LIST-PROCESSING.
      ENDIF.
    ENDLOOP.
  ENDFORM.
  
  *&---------------------------------------------------------------------*
  *& 获取销售订单数据
  *&---------------------------------------------------------------------*
  FORM frm_get_sodata.
    SELECT  p~vbeln,
            p~posnr,
            p~werks,
            v~kunnr,
            k~auart,"订单类型
            k~vtweg, "分销渠道(部门)
            k~vkbur,"销售办事处(二级)
            p~zbhcl,                "备货策略
            k~zckgj,                "出口国家
            k~ernam,                "销售订单 创建人
            k~objnr,                "抬头级别对象编号
            p~matnr,
            p~zbzgg,
            p~kwmeng,"以销售单位表示的累计订购数量
            p~vrkme,
            p~meins,
            p~umvkz,
            p~umvkn,
            k~erdat,
            p~znbjhq,
            p~zysjhq,
            p~zscbldh,
            p~zsfxcp,                "是否新产品
            p~zsfxbz,                "是否新包装
            p~zatpdfrq,               "ATP答复时间
            k~vkorg                  "20220530 Seashell 新增
      INTO CORRESPONDING FIELDS OF TABLE @gt_alv_head
      FROM vbak AS k
      JOIN vbap AS p ON k~vbeln = p~vbeln
      LEFT JOIN vbpa AS v ON v~vbeln = k~vbeln AND v~parvw = 'Z1' "实际客户
  *    INNER JOIN mara AS a ON a~matnr = p~matnr
  *    INNER JOIN tvtwt AS t ON t~vtweg = k~vtweg AND t~spras = @sy-langu         "Zhanghb_20191228  部门名称
  *    LEFT JOIN tvkbt AS bt ON bt~vkbur = k~vkbur AND bt~spras = @sy-langu       "Zhanghb_20191228  二级部门名称
  *    LEFT JOIN tvakt AS kt ON kt~auart = k~auart AND kt~spras = @sy-langu       "Zhanghb_20191228  销售凭证类型
  *    LEFT JOIN dd07v AS v ON p~zbhcl = v~domvalue_l AND v~ddlanguage = @sy-langu "Zhanghb_20191231
     WHERE k~vbeln IN @s_vbeln "销售凭证(销售订单号)
       AND v~kunnr IN @s_kunnr "客户售达方
       AND k~erdat IN @s_erdat "记录建立日期(下单时间)
       AND k~vtweg IN @s_vtweg "分销渠道(部门)
       AND k~auart IN @s_auart "销售订单类型
       AND p~matnr IN @s_matnr "物料
       AND p~werks IN @s_werks
       AND p~znbjhq IN @s_znbjhq "要求完工日期
       AND p~matkl IN @s_matkl  "物料组(剂型)
  *     AND (gt_rsdswhere) .
       AND EXISTS ( SELECT vbeln FROM vbpa WHERE vbeln = k~vbeln AND parvw = 'Z3' AND kunnr IN @s_ywy ).
  
    IF gt_alv_head IS INITIAL.
      MESSAGE TEXT-e01 TYPE 'S' DISPLAY LIKE 'E'.
      STOP.
    ENDIF.
  
  *  LOOP AT gt_alv ASSIGNING <fs_alv> WHERE matkl IS NOT INITIAL.
  *    DATA(lv_len) = strlen( <fs_alv>-matkl ).
  *    IF lv_len >= 4.
  *      <fs_alv>-matkl_4 = <fs_alv>-matkl+0(4).
  *    ELSE.
  *      <fs_alv>-matkl_4 = <fs_alv>-matkl.
  *    ENDIF.
  *  ENDLOOP.
  
  ENDFORM.
  
  *&---------------------------------------------------------------------*
  *& Form FRM_PROCESS_DATA
  *&---------------------------------------------------------------------*
  *& 处理数据
  *&---------------------------------------------------------------------*
  FORM frm_process_data.
  
  *  PERFORM frm_get_vbpa.         "客户代码 / 业务员 /2、  客户级别
  * PERFORM frm_read_txtl.        "销售订单行项目 文本
  *  PERFORM frm_get_m_material.   "获取M和m的物料
  *  PERFORM frm_get_s_material.   "获取S物料
  *  PERFORM frm_get_myl.         "获取35、  M物料  39、  内包物料 / 43、  外包物料
  *  PERFORM frm_get_yl.           "获取35、  原辅料 /  39、  内包物料 / 43、  外包物料
  
  
  *  PERFORM frm_get_material_dec. "获取物料描述，单位等信息
  
  
  ENDFORM.
  *处理销售订单数据
  FORM frm_process_sodata .
    DATA: lt_kunnr         LIKE TABLE OF gs_kunnr,
          lt_vtweg         LIKE TABLE OF gs_vtweg,
          lt_vkbur         LIKE TABLE OF gs_vkbur,
          lt_auart         LIKE TABLE OF gs_auart,
          lt_vbeln         LIKE TABLE OF gs_vbeln,
          lt_ernam         LIKE TABLE OF gs_ernam,
          lt_objnr         LIKE TABLE OF gs_objnr,
          lt_zckgj         LIKE TABLE OF gs_zckgj,
          lt_matnr         LIKE TABLE OF gs_matnr,
          lt_zppt0003      LIKE TABLE OF gs_zppt0003 WITH EMPTY KEY WITH NON-UNIQUE SORTED KEY skeys COMPONENTS kdauf kdpos,
          lt_zppt0003_temp LIKE TABLE OF gs_zppt0003,
          lt_matdoc        LIKE TABLE OF gs_matdoc WITH EMPTY KEY WITH NON-UNIQUE SORTED KEY skeys COMPONENTS mat_kdauf mat_kdpos,
          lt_matdoc_temp   LIKE TABLE OF gs_matdoc,
          lt_ltexts        TYPE TABLE OF tline,
          lv_lines         TYPE int4.
    "20220530 Seashell 新增定价参考物料逻辑、自建表
    IF gt_alv_head IS NOT INITIAL..
      SELECT
        matnr,
        vkorg,
        vtweg,
        pmatn
      FROM mvke
      INTO TABLE @DATA(lt_mvke)
      FOR ALL ENTRIES IN @gt_alv_head
      WHERE matnr = @gt_alv_head-matnr
        AND vkorg = @gt_alv_head-vkorg
        AND vtweg = @gt_alv_head-vtweg.
    ENDIF.
  
    "定价参考物料描述
    IF lt_mvke IS NOT INITIAL.
      SELECT
        *
      FROM makt
      INTO TABLE @DATA(lt_makt_pmatn)
      FOR ALL ENTRIES IN @lt_mvke
      WHERE matnr = @lt_mvke-matnr
        AND spras = '1'.
    ENDIF.
  
    "自建表
    SELECT
      *
    FROM zppt0028
    INTO TABLE @DATA(lt_zppt0028).
    SORT lt_zppt0028 BY werks matnr.
  
  
  
  
  
    MOVE-CORRESPONDING gt_alv_head TO lt_kunnr.
    SORT lt_kunnr BY kunnr.
    DELETE ADJACENT DUPLICATES FROM lt_kunnr COMPARING kunnr.
    "客户名称、搜索项、客户分类
    IF lt_kunnr IS NOT INITIAL.
      SELECT kunnr,name1,name2,name3,name4,sortl,kukla
         FROM kna1 FOR ALL ENTRIES IN @lt_kunnr
        WHERE kunnr = @lt_kunnr-kunnr
        INTO TABLE @DATA(lt_kna1).
  
      "20220527 Seashell修复实际客户检索项长度不够的问题
      SELECT partner,bu_sort1
         FROM but000 FOR ALL ENTRIES IN @lt_kunnr
        WHERE partner = @lt_kunnr-kunnr
        INTO TABLE @DATA(lt_but000).
    ENDIF.
    "分销渠道描述
    MOVE-CORRESPONDING gt_alv_head TO lt_vtweg.
    SORT lt_vtweg BY vtweg.
    DELETE ADJACENT DUPLICATES FROM lt_vtweg.
    DELETE lt_vtweg WHERE vtweg IS INITIAL.
    IF lt_vtweg IS NOT INITIAL.
      SELECT vtweg,vtext FROM tvtwt
        FOR ALL ENTRIES IN @lt_vtweg
        WHERE vtweg = @lt_vtweg-vtweg
        AND spras = @sy-langu
        INTO TABLE @DATA(lt_tvtwt).
    ENDIF.
    "销售办事处名称
    MOVE-CORRESPONDING gt_alv_head TO lt_vkbur.
    SORT lt_vkbur BY vkbur.
    DELETE ADJACENT DUPLICATES FROM lt_vkbur COMPARING vkbur.
    DELETE lt_vkbur WHERE vkbur IS INITIAL.
    IF lt_vkbur IS NOT  INITIAL.
      SELECT vkbur,bezei FROM tvkbt FOR ALL ENTRIES IN @lt_vkbur
        WHERE vkbur = @lt_vkbur-vkbur
        AND spras = @sy-langu
        INTO TABLE @DATA(lt_tvkbt).
    ENDIF.
    "销售订单类型描述
    MOVE-CORRESPONDING gt_alv_head TO lt_auart.
    SORT lt_auart BY auart.
    DELETE ADJACENT DUPLICATES FROM lt_auart COMPARING auart.
    DELETE lt_auart WHERE auart IS INITIAL.
    IF lt_auart IS NOT INITIAL.
      SELECT auart,bezei AS auartxt  FROM tvakt FOR ALL ENTRIES IN @lt_auart
        WHERE auart = @lt_auart-auart
        AND spras = @sy-langu
        INTO TABLE @DATA(lt_tvakt).
    ENDIF.
    "销售订单行项目的相关信息
    MOVE-CORRESPONDING gt_alv_head TO lt_vbeln.
    SORT lt_vbeln BY vbeln posnr.
    IF lt_vbeln IS NOT INITIAL.
      "实际客户
  *    SELECT vbpa~vbeln, kna1~kunnr,kna1~name1,kna1~name2,kna1~name3,kna1~name4,kna1~sortl,kna1~kukla
  *    FROM vbpa
  *    INNER JOIN kna1 ON vbpa~kunnr = kna1~kunnr
  *    FOR ALL ENTRIES IN @lt_vbeln
  *    WHERE vbpa~vbeln = @lt_vbeln-vbeln
  *    AND vbpa~parvw = 'Z1' "实际客户
  *    INTO TABLE @DATA(lt_kna1_z1).
      "业务员描述
      SELECT vbpa~vbeln, kna1~kunnr,kna1~name1,kna1~name2,kna1~name3,kna1~name4
        FROM vbpa
        INNER JOIN kna1 ON vbpa~kunnr = kna1~kunnr
        FOR ALL ENTRIES IN @lt_vbeln
        WHERE vbpa~vbeln = @lt_vbeln-vbeln
        AND vbpa~parvw = 'Z3' "业务员
        INTO TABLE @DATA(lt_kna1_z3).
      "销售订单行项目计划行类别
      SELECT vbeln,posnr,etenr,ettyp
        FROM vbep FOR ALL ENTRIES IN @lt_vbeln
        WHERE vbeln = @lt_vbeln-vbeln
        AND posnr = @lt_vbeln-posnr
        INTO TABLE @DATA(lt_vbep).
      " "销售订单关联流程订单的车间
      SELECT kdauf,kdpos,zchej FROM aufk FOR ALL ENTRIES IN @lt_vbeln
        WHERE kdauf = @lt_vbeln-vbeln
        AND kdpos = @lt_vbeln-posnr
        AND zchej IS NOT INITIAL
        INTO TABLE @DATA(lt_aufk).
      "最早入库日期、最晚入库日期
      SELECT kdauf,kdpos,erdat,uname
        FROM zppt0003 FOR ALL ENTRIES IN @lt_vbeln
        WHERE kdauf = @lt_vbeln-vbeln
        AND kdpos = @lt_vbeln-posnr
        AND zdelflg IS INITIAL
        INTO TABLE @lt_zppt0003.
      "销售订单关联流程订单入库的过账时间
      SELECT mat_kdauf ,mat_kdpos,budat,cputm FROM matdoc FOR ALL ENTRIES IN @lt_vbeln
        WHERE record_type = 'MDOC'
        AND mat_kdauf = @lt_vbeln-vbeln
        AND mat_kdpos = @lt_vbeln-posnr
        AND bwart = '101'
        INTO TABLE @lt_matdoc.
      "实际发货日期
      SELECT vbfa~vbelv,vbfa~posnv,likp~wadat_ist
        FROM vbfa INNER JOIN likp ON vbfa~vbeln = likp~vbeln
        FOR ALL ENTRIES IN @lt_vbeln
        WHERE vbfa~vbelv = @lt_vbeln-vbeln
        AND vbfa~posnv = @lt_vbeln-posnr
        AND vbfa~vbtyp_n = 'J'
        AND vbfa~vbtyp_v = 'C'
        AND likp~wbstk = 'C'
        INTO TABLE @DATA(lt_vbfa).
      "销售订单关联流程订单的累计入库量
      SELECT afpo~kdauf,afpo~kdpos,afpo~matnr, SUM( afpo~wemng ) AS wemng
       FROM @lt_vbeln AS t1
      INNER JOIN afpo ON t1~vbeln = afpo~kdauf AND t1~posnr = afpo~kdpos
        GROUP BY afpo~kdauf,afpo~kdpos,afpo~matnr
        ORDER BY afpo~kdauf,afpo~kdpos,afpo~matnr
        INTO TABLE @DATA(lt_afpo).
      "销售订单项目已发货数量
      SELECT k~vbeln, k~posnr, SUM( lips~lfimg ) AS lfimg
        FROM @lt_vbeln AS k
        JOIN lips  ON lips~vgbel = k~vbeln AND lips~vgpos = k~posnr
        GROUP BY k~vbeln, k~posnr
        ORDER BY k~vbeln, k~posnr
        INTO TABLE @DATA(lt_delive_item).
      "车间
      SELECT lt~vbeln,lt~posnr,aufk~zchej,CASE WHEN aufk~aufnr = aufk~zaufnr_split THEN 1 ELSE 0 END AS zyd
        FROM @lt_vbeln AS lt
        INNER JOIN afpo ON lt~vbeln = afpo~kdauf AND lt~posnr = afpo~kdpos
        INNER JOIN aufk ON afpo~aufnr = aufk~aufnr
      INTO TABLE @DATA(lt_zhej).
  
    ENDIF.
    "销售订单创建者名称
    MOVE-CORRESPONDING gt_alv_head TO lt_ernam.
    SORT lt_ernam BY ernam.
    DELETE ADJACENT DUPLICATES FROM lt_ernam COMPARING ernam.
    DELETE lt_ernam WHERE ernam IS INITIAL .
    IF lt_ernam IS NOT INITIAL.
      SELECT  t1~ernam, adrp~name_text
        FROM  @lt_ernam AS t1
        INNER JOIN usr21 ON usr21~bname = t1~ernam
        INNER JOIN adrp ON usr21~persnumber = adrp~persnumber
        INTO TABLE @DATA(lt_adrp).
    ENDIF.
    "销售订单审核状态
    MOVE-CORRESPONDING gt_alv_head TO lt_objnr.
    SORT lt_objnr BY objnr.
    DELETE ADJACENT DUPLICATES FROM lt_objnr COMPARING objnr.
    DELETE lt_objnr WHERE objnr IS INITIAL.
    IF lt_objnr IS NOT INITIAL.
      SELECT objnr,stat, udate,utime
        FROM  jcds FOR ALL ENTRIES IN @lt_objnr
        WHERE objnr = @lt_objnr-objnr
        AND jcds~stat IN ( 'E0002','E0003' )
        AND jcds~inact = ''
        INTO TABLE @DATA(lt_jcds).
    ENDIF.
    "备货策略域值列表
    SELECT domvalue_l,ddtext FROM  dd07t
     WHERE domname = 'ZD_BHCL' "备货策略域名称
       AND ddlanguage = '1' "中文-ZH
      INTO TABLE @DATA(lt_dd07t).
    "出口国家名称
    MOVE-CORRESPONDING gt_alv_head TO lt_zckgj.
    SORT lt_zckgj BY zckgj.
    DELETE ADJACENT DUPLICATES FROM lt_zckgj.
    DELETE lt_zckgj WHERE zckgj IS INITIAL.
    IF lt_zckgj IS NOT INITIAL.
      SELECT land1,landx FROM t005t FOR ALL ENTRIES IN @lt_zckgj
        WHERE land1 = @lt_zckgj-zckgj
        AND spras = @sy-langu
        INTO TABLE @DATA(lt_t005t).
    ENDIF.
    "销售订单项目物料编码描述
    MOVE-CORRESPONDING gt_alv_head TO lt_matnr.
    SORT lt_matnr BY matnr.
    DELETE ADJACENT DUPLICATES FROM lt_matnr COMPARING matnr.
    IF lt_matnr IS NOT INITIAL.
      SELECT mara~matnr,mara~matkl,  makt~maktx
        FROM mara LEFT JOIN makt ON mara~matnr = makt~matnr AND spras = @sy-langu
         FOR ALL ENTRIES IN @lt_matnr
        WHERE mara~matnr = @lt_matnr-matnr
        INTO TABLE @DATA(lt_mara).
    ENDIF.
    "保健品送检规则
    IF lt_kna1 IS NOT INITIAL.
      SELECT kna1~kunnr, tvk1t~vtext
        FROM kna1
        INNER JOIN tvk1t ON kna1~katr1 = tvk1t~katr1 AND tvk1t~spras = @sy-langu
        FOR ALL ENTRIES IN @lt_kna1
        WHERE kna1~kunnr = @lt_kna1-kunnr
        INTO TABLE @DATA(lt_vtext).
    ENDIF.
    "客户分类描述
    SELECT kukla,vtext FROM tkukt WHERE spras = @sy-langu INTO TABLE @DATA(lt_tkukt).
    "更新字段
  *  SORT lt_kna1 BY kunnr.
    SORT lt_tvtwt BY vtweg.
    SORT lt_tvkbt BY vkbur.
    SORT lt_tvakt BY auart.
    SORT lt_kna1 BY kunnr.
    SORT lt_kna1_z3 BY vbeln.
    SORT lt_vbep BY vbeln posnr.
    SORT lt_adrp BY ernam.
    SORT lt_jcds BY objnr stat.
    SORT lt_dd07t BY domvalue_l.
    SORT lt_t005t BY land1.
    SORT lt_mara BY matnr.
    SORT lt_aufk BY kdauf kdpos.
    SORT lt_zppt0003 BY  kdauf kdpos.
    SORT lt_matdoc BY mat_kdauf mat_kdpos.
    SORT lt_vbfa BY vbelv posnv.
    SORT lt_zhej BY vbeln posnr ASCENDING zyd DESCENDING.
    SORT lt_vtext BY kunnr.
    LOOP AT gt_alv_head ASSIGNING <fs_alv>.
      "售达方
      READ TABLE lt_kna1 WITH KEY kunnr = <fs_alv>-kunnr BINARY SEARCH INTO DATA(ls_kna1).
      IF sy-subrc = 0.
        "售达方
        <fs_alv>-kunnr = ls_kna1-kunnr.
        <fs_alv>-kunnr_dec = |{ ls_kna1-name1 }{ ls_kna1-name2 }{ ls_kna1-name3 }{ ls_kna1-name4 }|.
  *      <fs_alv>-bu_sort1 = ls_kna1-sortl."搜索项
        <fs_alv>-kukla = ls_kna1-kukla."客户分类
      ENDIF.
  
      "20220527 Seashell 修复实际客户检索项取值
      READ TABLE lt_but000 INTO DATA(ls_but000) WITH KEY partner = <fs_alv>-kunnr.
      IF sy-subrc = 0.
        <fs_alv>-bu_sort1 = ls_but000-bu_sort1.
        CLEAR ls_but000.
      ENDIF.
      "20220530 Seashell 定价参考物料
      READ TABLE lt_mvke INTO DATA(ls_mvke) WITH KEY matnr = <fs_alv>-matnr vtweg = <fs_alv>-vtweg vkorg = <fs_alv>-vkorg.
      IF sy-subrc = 0.
        <fs_alv>-pmatn = ls_mvke-pmatn.
        CLEAR ls_mvke.
      ENDIF.
      READ TABLE lt_makt_pmatn INTO DATA(ls_makt_pmatn) WITH KEY matnr = <fs_alv>-pmatn.
      IF sy-subrc = 0.
        <fs_alv>-pmatnx = ls_makt_pmatn-maktx.
        CLEAR ls_makt_pmatn.
      ENDIF.
      "供货周期
      READ TABLE lt_zppt0028 INTO DATA(ls_zppt0028) WITH KEY matnr = <fs_alv>-matnr werks = <fs_alv>-werks BINARY SEARCH.
      IF sy-subrc = 0.
        <fs_alv>-plifz = ls_zppt0028-plifz.
        CLEAR ls_zppt0028.
      ENDIF.
  
  
  
  
  
      "客户分类描述
      IF <fs_alv>-kukla IS NOT INITIAL.
        READ TABLE lt_tkukt WITH KEY kukla = <fs_alv>-kukla INTO DATA(ls_tkukt).
        IF sy-subrc = 0.
          <fs_alv>-kuklatxt = ls_tkukt-vtext.
        ENDIF.
      ENDIF.
      "分销渠道描述
      READ TABLE lt_tvtwt WITH KEY vtweg = <fs_alv>-vtweg BINARY SEARCH INTO DATA(ls_tvtwt).
      IF sy-subrc = 0.
        <fs_alv>-vtext = ls_tvtwt-vtext.
      ENDIF.
      "销售办事处名称
      READ TABLE lt_tvkbt WITH KEY vkbur = <fs_alv>-vkbur BINARY SEARCH INTO DATA(ls_tvkbt).
      IF sy-subrc = 0.
        <fs_alv>-bezei = ls_tvkbt-bezei.
      ENDIF.
      "销售订单类型描述
      READ TABLE lt_tvakt WITH KEY auart = <fs_alv>-auart BINARY SEARCH INTO DATA(ls_tvakt).
      IF sy-subrc = 0.
        <fs_alv>-auartxt = ls_tvakt-auartxt.
      ENDIF.
      "业务员
      READ TABLE lt_kna1_z3 WITH KEY vbeln = <fs_alv>-vbeln BINARY SEARCH INTO DATA(ls_kna1_z3).
      IF sy-subrc = 0.
        <fs_alv>-ywy = ls_kna1_z3-kunnr.
        <fs_alv>-ywy_dec = |{ ls_kna1_z3-name1 }{ ls_kna1_z3-name2 }{ ls_kna1_z3-name3 }{ ls_kna1_z3-name4 }  |.
      ENDIF.
      "销售订单行项目计划行类别
      READ TABLE lt_vbep WITH KEY vbeln = <fs_alv>-vbeln posnr = <fs_alv>-posnr BINARY SEARCH INTO DATA(ls_vbep).
      IF sy-subrc = 0.
        <fs_alv>-ettyp = ls_vbep-ettyp.
      ENDIF.
      "销售订单创建者名称
      READ TABLE lt_adrp WITH KEY ernam = <fs_alv>-ernam BINARY SEARCH INTO DATA(ls_adrp).
      IF sy-subrc = 0.
        <fs_alv>-ernamt = ls_adrp-name_text.
      ENDIF.
      "销售订单审核状态
      IF <fs_alv>-objnr IS NOT INITIAL.
        READ TABLE lt_jcds WITH KEY objnr = <fs_alv>-objnr stat = 'E0003' BINARY SEARCH INTO DATA(ls_jcds).
        IF sy-subrc = 0.
          <fs_alv>-txt30 = '财务二审'.
          <fs_alv>-udate = ls_jcds-udate."审核日期
          <fs_alv>-utime = ls_jcds-utime."审核时间
        ELSE.
          READ TABLE lt_jcds WITH KEY objnr = <fs_alv>-objnr stat = 'E0002' BINARY SEARCH INTO ls_jcds.
          IF sy-subrc = 0.
            <fs_alv>-txt30 = '业务一审'.
            <fs_alv>-udate = ls_jcds-udate."审核日期
            <fs_alv>-utime = ls_jcds-utime."审核时间
          ENDIF.
        ENDIF.
      ENDIF.
      "备货策略描述
      IF <fs_alv>-zbhcl IS NOT INITIAL.
        READ TABLE lt_dd07t WITH KEY domvalue_l = <fs_alv>-zbhcl BINARY SEARCH INTO DATA(ls_dd07t).
        IF sy-subrc = 0.
          <fs_alv>-ddtext = ls_dd07t-ddtext.
        ENDIF.
      ENDIF.
      "出口国家名称
      IF <fs_alv>-zckgj IS NOT INITIAL.
        READ TABLE lt_t005t WITH KEY land1 = <fs_alv>-zckgj BINARY SEARCH INTO DATA(ls_t005t).
        IF sy-subrc = 0.
          <fs_alv>-landx = ls_t005t-landx.
        ENDIF.
      ENDIF.
      "销售订单项目物料描述、物料组
      READ TABLE lt_mara WITH KEY matnr = <fs_alv>-matnr BINARY SEARCH INTO DATA(ls_mara).
      IF sy-subrc = 0.
        <fs_alv>-maktx = ls_mara-maktx.
        <fs_alv>-matkl = ls_mara-matkl.
        IF strlen( ls_mara-matkl ) > 4 .
          <fs_alv>-matkl_4 = ls_mara-matkl(4).
        ENDIF.
      ENDIF.
      "销售订单关联流程订单的车间
      READ TABLE lt_aufk WITH KEY kdauf = <fs_alv>-vbeln kdpos = <fs_alv>-posnr BINARY SEARCH INTO DATA(ls_aufk).
      IF sy-subrc = 0.
        <fs_alv>-zchej = ls_aufk-zchej.
      ENDIF.
      "最早入库日期、最晚入库日期
      REFRESH lt_zppt0003_temp.
      lt_zppt0003_temp = FILTER #( lt_zppt0003 USING KEY skeys WHERE kdauf = <fs_alv>-vbeln AND kdpos = <fs_alv>-posnr ).
      IF lt_zppt0003_temp IS NOT INITIAL.
        SORT lt_zppt0003_temp BY erdat.
        lv_lines = lines( lt_zppt0003_temp ).
        <fs_alv>-budat_zao = lt_zppt0003_temp[ 1 ]-erdat.
        <fs_alv>-budat_wan = lt_zppt0003_temp[ lv_lines ]-erdat.
        <fs_alv>-greuname = lt_zppt0003_temp[ lv_lines ]-uname.
      ENDIF.
      "销售订单关联流程订单收货的最早最晚日期时间
      REFRESH lt_matdoc_temp.
      lt_matdoc_temp = FILTER #( lt_matdoc USING KEY skeys WHERE mat_kdauf = <fs_alv>-vbeln AND mat_kdpos = <fs_alv>-posnr ).
      IF lt_matdoc_temp IS NOT INITIAL.
        SORT lt_matdoc_temp BY  budat cputm.
        lv_lines = lines( lt_matdoc_temp ).
        <fs_alv>-budat_b = lt_matdoc_temp[ 1 ]-budat."首次收货过账日期
        <fs_alv>-cputm_b = lt_matdoc_temp[ 1 ]-cputm."首次收货过账时间
        <fs_alv>-budat_e = lt_matdoc_temp[ lv_lines ]-budat."末次收货过账日期
        <fs_alv>-cputm_e = lt_matdoc_temp[ lv_lines ]-cputm."末次收货过账时间
      ENDIF.
      "实际发货日期
      READ TABLE lt_vbfa WITH KEY vbelv = <fs_alv>-vbeln posnv = <fs_alv>-posnr BINARY SEARCH INTO DATA(ls_vbfa).
      IF sy-subrc = 0.
        <fs_alv>-wadat_ist = ls_vbfa-wadat_ist.
      ENDIF.
      "销售订单关联流程订单的累计入库量-销售单位
      READ TABLE lt_afpo WITH KEY kdauf = <fs_alv>-vbeln kdpos = <fs_alv>-posnr matnr = <fs_alv>-matnr BINARY SEARCH INTO DATA(ls_afpo).
      IF sy-subrc = 0.
        <fs_alv>-wemng = ls_afpo-wemng * <fs_alv>-umvkn / <fs_alv>-umvkz."入库数量-销售单位
      ENDIF.
      "销售订单已发货数量
      READ TABLE lt_delive_item WITH KEY vbeln  = <fs_alv>-vbeln posnr = <fs_alv>-posnr BINARY SEARCH INTO DATA(ls_delive_item).
      IF sy-subrc = 0.
        <fs_alv>-yfhsl = ls_delive_item-lfimg."销售订单项目已发货数量
        <fs_alv>-wfhsl = <fs_alv>-wemng - ls_delive_item-lfimg."销售订单项目未发货数量
      ENDIF.
      "车间
      READ TABLE lt_zhej WITH KEY vbeln  = <fs_alv>-vbeln posnr = <fs_alv>-posnr BINARY SEARCH INTO DATA(ls_zhej).
      IF sy-subrc = 0.
        <fs_alv>-zchej = ls_zhej-zchej.
      ENDIF.
      "M实际周期：“最早入库日期”减去“订单日期”
      IF <fs_alv>-budat_zao IS NOT INITIAL.
        <fs_alv>-sjzq = <fs_alv>-budat_zao - <fs_alv>-erdat.
      ENDIF.
      "销售订单项目订单数量-基本单位
      <fs_alv>-bmeng = <fs_alv>-kwmeng * <fs_alv>-umvkz / <fs_alv>-umvkn.
      "销售订单行项目长文本
      REFRESH lt_ltexts.
      CALL FUNCTION 'READ_TEXT'
        EXPORTING
  *       CLIENT                  = SY-MANDT
          id                      = 'Z001'
          language                = sy-langu
          name                    = CONV tdobname( |{ <fs_alv>-vbeln }{ <fs_alv>-posnr }| )
          object                  = 'VBBP'
        TABLES
          lines                   = lt_ltexts
        EXCEPTIONS
          id                      = 1
          language                = 2
          name                    = 3
          not_found               = 4
          object                  = 5
          reference_check         = 6
          wrong_access_to_archive = 7
          OTHERS                  = 8.
      IF sy-subrc  = 0.
        LOOP AT lt_ltexts INTO DATA(ls_ltext).
          <fs_alv>-ltxts = |{ <fs_alv>-ltxts }{ ls_ltext-tdline }|.
        ENDLOOP.
      ENDIF.
      "保健品送检规则
      READ TABLE lt_vtext WITH KEY kunnr = <fs_alv>-kunnr BINARY SEARCH INTO DATA(ls_vtext).
      IF sy-subrc = 0.
        <fs_alv>-sjgz = ls_vtext-vtext.
      ENDIF.
  
      "物料分类001类型的键值
      gs_objek-objek = CONV cuobn( <fs_alv>-matnr ).
      INSERT gs_objek INTO TABLE gt_objek.
      "物料组
      IF <fs_alv>-matkl IS NOT INITIAL.
        gs_matkl-matkl   = <fs_alv>-matkl.
        INSERT gs_matkl INTO TABLE gt_matkl.
        "展BOM抬头物料
        gs_bom_matnr-matnr = <fs_alv>-matnr.
        gs_bom_matnr-werks = <fs_alv>-werks.
        gs_bom_matnr-matkl = <fs_alv>-matkl.
        INSERT gs_bom_matnr INTO TABLE gt_bom_matnr.
      ENDIF.
      IF <fs_alv>-matkl_4 IS NOT INITIAL.
        gs_matkl-matkl = <fs_alv>-matkl_4.
        INSERT gs_matkl INTO TABLE gt_matkl.
      ENDIF.
    ENDLOOP.
  ENDFORM.
  *&---------------------------------------------------------------------*
  *& Form FRM_GET_DETAILDATA
  *&---------------------------------------------------------------------*
  *& text
  *&---------------------------------------------------------------------*
  *& -->  p1        text
  *& <--  p2        text
  *&---------------------------------------------------------------------*
  FORM frm_get_detaildata .
    DATA:lt_stb      TYPE ty_tab_stpox,
         lt_matcat   TYPE ty_tab_cscmat,
         lt_bom_head LIKE TABLE OF gs_bom_head.
    "查询物料BOM抬头基本信息
    SELECT b~matnr,b~werks,b~matkl,mast~stlnr, mast~stlal,s~bmeng, s~bmein
      FROM @gt_bom_matnr AS b
      JOIN mast ON mast~matnr = b~matnr AND  mast~werks = b~werks
      JOIN stko AS s  ON  mast~stlnr = s~stlnr AND mast~stlal = s~stlal
      WHERE  mast~stlan = '1'  "BOM用途
      AND mast~stlal =  ( SELECT MIN( stlal ) FROM mast WHERE stlal < '09' AND matnr = b~matnr AND werks = b~werks AND stlan = '1')"最小的可选BOM
      AND s~stlty = 'M' "BOM 类别
      AND s~datuv = ( SELECT MAX( datuv ) FROM stko WHERE stlty = 'M' AND stlnr = s~stlnr AND stlal = s~stlal AND  datuv <= @sy-datum )  "有效期
      AND s~loekz = ''    " 删除标识
    INTO TABLE  @lt_bom_head.
    "展BOM
    LOOP AT lt_bom_head INTO gs_bom_head.
      "展开工厂物料BOM
      REFRESH:lt_stb,lt_matcat.
      PERFORM frm_exp_bom USING gs_bom_head-bmeng gs_bom_head-matnr gs_bom_head-stlal gs_bom_head-werks CHANGING lt_stb  lt_matcat.
      "设置BOM明细数据
      CHECK lt_stb IS NOT INITIAL.
      PERFORM frm_process_detaildata USING gs_bom_head lt_stb lt_matcat.
    ENDLOOP.
  ENDFORM.
  FORM frm_exp_bom USING VALUE(iv_bmeng) TYPE stko-bmeng
                          VALUE(iv_matnr) TYPE mara-matnr
                          VALUE(iv_stlal) TYPE stko-stlal
                          VALUE(iv_werks) TYPE mast-werks
                    CHANGING ct_stb TYPE ty_tab_stpox
                             ct_matcat TYPE ty_tab_cscmat.
    REFRESH: ct_stb,ct_matcat.
    CALL FUNCTION 'CS_BOM_EXPL_MAT_V2'
      EXPORTING
        aumng                 = 1  "输入非零的值，最上级的物料不考虑装配报废率、但下级的物料还是会考虑装配报废，0考虑装配报废，需配合AUSKZ=‘X'使用
        auskz                 = 'X'  "计算组件损耗(会考虑装配报废率，需配合AUMNG使用)
        capid                 = 'PP01'  "BOM应用
        datuv                 = sy-datum  "有效起始日
        delnl                 = abap_false  "组件删除标识
        emeng                 = cns_bom_bmeng  "需求数量
        mehrs                 = abap_true    "多层展开标识-多层
        mtnrv                 = iv_matnr "展开物料号
        stlan                 = '1'  "BOM用途
        stlal                 = iv_stlal "可选BOM
        werks                 = iv_werks  "工厂
  *    IMPORTING
  *     topmat                = cs_cstmat
  *     dstst                 = pv_dstst
      TABLES
        stb                   = ct_stb
        matcat                = ct_matcat
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
  ENDFORM.
  *&---------------------------------------------------------------------*
  *& Form FRM_PROCESS_DETAILDATA
  *&---------------------------------------------------------------------*
  *& text
  *&---------------------------------------------------------------------*
  *& -->  p1        text
  *& <--  p2        text
  *&---------------------------------------------------------------------*
  FORM frm_process_detaildata USING is_bom_head LIKE gs_bom_head it_stb TYPE ty_tab_stpox  it_matcat TYPE ty_tab_cscmat .
    DATA:lt_alv_detail TYPE TABLE OF ty_detail,
         lt_stb_temp   TYPE TABLE OF stpox,
         lt_stb_wbc    TYPE TABLE OF stpox, "大M物料BOM下面的外包材
         lt_wbc_cycle  TYPE ty_tab_raw_cycle,
         lv_len        TYPE i,
         lv_lines      TYPE i,
         lv_tabix      TYPE sy-tabix,
         lv_mb         TYPE sy-tabix, "小M物料清单在stpox内表中的起始行
         lv_me         TYPE sy-tabix. "小M物料清单在stpox内表中的截止行
    lv_len =  strlen( is_bom_head-matkl ).
    lv_lines = lines( it_stb ).
    "BOM展开物料的物料组为5016开头标识大M,否则为小M
    IF is_bom_head-matkl IS NOT INITIAL AND lv_len > 4 AND is_bom_head-matkl(4) = '5016'.
      LOOP AT it_matcat ASSIGNING FIELD-SYMBOL(<fs_matcat>).
        CLEAR <fs_matcat>-stktx.
        READ TABLE it_stb WITH KEY ttidx = <fs_matcat>-index INTO DATA(ls_stb).
        IF sy-subrc = 0.
          <fs_matcat>-stktx = ls_stb-mtart.
        ENDIF.
      ENDLOOP.
      "大M物料BOM下面的外包材
      LOOP AT it_stb INTO ls_stb.
        CHECK ls_stb-mtart = 'Z020'.
        IF ls_stb-stufe = 1 .
          APPEND ls_stb TO lt_stb_wbc.
        ELSE.
          "上级物料是虚拟BOM物料
          READ TABLE it_matcat WITH KEY index = ls_stb-ttidx INTO DATA(ls_matcat).
          IF sy-subrc = 0 AND ls_matcat-stktx = 'Z020'.
            APPEND ls_stb TO lt_stb_wbc.
          ENDIF.
        ENDIF.
      ENDLOOP.
      "外包材采购周期
      IF lt_stb_wbc IS NOT INITIAL.
        PERFORM frm_get_raw_cycle USING lt_stb_wbc it_matcat CHANGING lt_wbc_cycle.
      ENDIF.
  
      "获取大M物料BOM的小m项目
      "后续维护请观察stpox内表的STUFE及MTART字段值规律
      LOOP AT it_stb INTO ls_stb.
        lv_tabix = sy-tabix.
        "查找小M物料清单在stpox内表中的起始行
        "大M物料BOM的小m项目stufe字段值总是为1
        IF ls_stb-stufe = 1 AND ls_stb-mtart = 'Z050'.
          IF lv_mb IS  INITIAL."
            lv_mb = lv_tabix."小M物料清单在stpox内表中的起始行
  *          lv_matnr_m  = ls_stb-idnrk."小m物料编号
            CONTINUE.
          ENDIF.
        ENDIF.
        "查找小M物料清单在stpox内表中的截止行
        IF ( ls_stb-stufe = 1 OR lv_tabix = lv_lines ) AND lv_mb IS NOT INITIAL .
          lv_me = lv_tabix - 1."小M物料清单在stpox内表中的截止行
          "暂存小m数据到lt_stb_temp
          REFRESH lt_stb_temp.
          INSERT LINES OF it_stb FROM lv_mb TO lv_me INTO TABLE lt_stb_temp.
          PERFORM frm_set_5016m_bom USING is_bom_head lt_stb_temp lt_wbc_cycle it_matcat CHANGING lt_alv_detail.
          INSERT LINES OF lt_alv_detail INTO TABLE gt_alv_detail.
          "下一个小m的起始行
          IF ls_stb-mtart = 'Z050'.
            lv_mb = lv_tabix.
          ELSE.
            CLEAR lv_mb.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ELSE."小m
      PERFORM frm_set_not_5016m_bom USING is_bom_head it_stb it_matcat CHANGING lt_alv_detail.
      INSERT LINES OF lt_alv_detail INTO TABLE gt_alv_detail.
    ENDIF.
  ENDFORM.
  FORM frm_set_not_5016m_bom USING  VALUE(is_bom_head) LIKE gs_bom_head
                            VALUE(it_stb) TYPE ty_tab_stpox
                            VALUE(it_matcat) TYPE ty_tab_cscmat
                      CHANGING ct_alv_detail TYPE ty_tab_detial.
    CHECK it_stb IS NOT INITIAL.
    REFRESH ct_alv_detail.
    APPEND INITIAL LINE TO ct_alv_detail ASSIGNING <fs_detail>.
    CHECK <fs_detail> IS ASSIGNED.
    "大M数据
    <fs_detail>-matnr = is_bom_head-matnr."大M物料编号
    <fs_detail>-werks = is_bom_head-werks."大M物料工程
    "S数据
    READ TABLE it_stb WITH KEY stufe = 1 mtart = 'Z030' INTO DATA(ls_stb).
    IF  sy-subrc = 0.
      <fs_detail>-matnr_s = ls_stb-idnrk."S物料编号
      <fs_detail>-maktx_s = ls_stb-ojtxp."S物料描述
      <fs_detail>-matkl_s = ls_stb-matmk."S物料组
      "S物料组前4位-中分类
      IF ls_stb-matmk IS NOT INITIAL AND strlen( ls_stb-matmk ) > 4.
        <fs_detail>-matkl_s4 = ls_stb-matmk(4).
        gs_matkl-matkl  = ls_stb-matmk.
        INSERT gs_matkl INTO TABLE gt_matkl.
        gs_matkl-matkl   = ls_stb-matmk(4).
        INSERT gs_matkl INTO TABLE gt_matkl.
      ENDIF.
      <fs_detail>-sl_s = ls_stb-menge / is_bom_head-bmeng * cns_bom_bmeng."S组件需求数量-不含任何损耗
      <fs_detail>-meins_s = ls_stb-meins."S组件单位
      "生产时间-通过配置表读取时间
      SELECT SINGLE zdzeit INTO <fs_detail>-zdzeit FROM zppt0006 WHERE werks = ls_stb-werks AND matkl = ls_stb-matmk.
      "压制时间
      <fs_detail>-zmatscat_s = zcl_bc_public=>get_class_value( atnam = 'ZMATSCAT' key = ls_stb-idnrk classtype = '001' classnum = ls_stb-matmk objtable = 'MARA' ).
    ENDIF.
    "原辅料采购周期
    REFRESH gt_raw_cycle.
    PERFORM frm_get_raw_cycle USING it_stb it_matcat CHANGING gt_raw_cycle.
    "外包材物料信息
    IF gt_raw_cycle IS NOT INITIAL.
      LOOP AT gt_raw_cycle INTO gs_raw_cycle WHERE mtart_head <> 'Z030' AND  mtart = 'Z020'.
        IF sy-subrc = 0.
          <fs_detail>-matnr_wb = gs_raw_cycle-idnrk."外包材物料编号
          <fs_detail>-maktx_wb = gs_raw_cycle-ojtxp."外包材物料描述
          <fs_detail>-ruezt_wb = gs_raw_cycle-zpretime."外包材物料准备天数
          <fs_detail>-plifz_wb = gs_raw_cycle-plifz."外包材物料计划交货时间(天)
          <fs_detail>-webaz_wb = gs_raw_cycle-webaz."外包材物料收货处理时间
          DELETE gt_raw_cycle WHERE mtart_head <> 'Z030'  AND mtart = 'Z020'.
          EXIT.
        ENDIF.
      ENDLOOP.
    ENDIF.
    "内包材物料信息
    IF gt_raw_cycle IS NOT INITIAL.
      READ TABLE gt_raw_cycle WITH KEY  mtart_head = 'Z030' mtart = 'Z020' INTO gs_raw_cycle.
      IF sy-subrc = 0.
        <fs_detail>-matnr_nb = gs_raw_cycle-idnrk."内包材物料编号
        <fs_detail>-maktx_nb = gs_raw_cycle-ojtxp."内包材物料描述
        <fs_detail>-ruezt_nb = gs_raw_cycle-zpretime."内包材物料准备天数
        <fs_detail>-plifz_nb = gs_raw_cycle-plifz."内包材物料计划交货时间(天)
        <fs_detail>-webaz_nb = gs_raw_cycle-webaz."内包材物料收货处理时间
      ENDIF.
      "内包材物料信息
      READ TABLE gt_raw_cycle WITH KEY  mtart = 'Z010' INTO gs_raw_cycle.
      IF sy-subrc = 0.
        <fs_detail>-matnr_yfl = gs_raw_cycle-idnrk."外包材物料编号
        <fs_detail>-maktx_yfl = gs_raw_cycle-ojtxp."外包材物料描述
        <fs_detail>-ruezt_yfl = gs_raw_cycle-zpretime."外包材物料准备天数
        <fs_detail>-plifz_yfl = gs_raw_cycle-plifz."外包材物料计划交货时间(天)
        <fs_detail>-webaz_yfl = gs_raw_cycle-webaz."外包材物料收货处理时间
      ENDIF.
    ENDIF.
  ENDFORM.
  FORM frm_set_5016m_bom USING  VALUE(is_bom_head) LIKE gs_bom_head
                            VALUE(it_stb) TYPE ty_tab_stpox
                            VALUE(it_wbc_cycle) TYPE ty_tab_raw_cycle
                            VALUE(it_matcat) TYPE ty_tab_cscmat
                      CHANGING ct_alv_detail TYPE ty_tab_detial.
    DATA ls_raw_cycle LIKE gs_raw_cycle.
    DATA lv_flag TYPE flag.
    CHECK it_stb IS NOT INITIAL.
    READ TABLE it_stb WITH KEY stufe = 1   mtart = 'Z050' INTO DATA(ls_stb).
    CHECK sy-subrc = 0.
    REFRESH ct_alv_detail.
    APPEND INITIAL LINE TO ct_alv_detail ASSIGNING <fs_detail>.
    CHECK <fs_detail> IS ASSIGNED.
    "小m数据
    <fs_detail>-matnr = is_bom_head-matnr."大M物料编号
    <fs_detail>-werks = is_bom_head-werks."大M物料工程
    <fs_detail>-matnr_m = ls_stb-idnrk."小m物料编号
    <fs_detail>-maktx_m = ls_stb-ojtxp."小m物料描述
    <fs_detail>-sl_m = ls_stb-menge / is_bom_head-bmeng * cns_bom_bmeng."小m组件需求数量-不含任何损耗
    <fs_detail>-meins_m = ls_stb-meins."小m组件单位
    "S数据
    READ TABLE it_stb WITH KEY stufe = 2 mtart = 'Z030' INTO ls_stb.
    IF  sy-subrc = 0.
      <fs_detail>-matnr_s = ls_stb-idnrk."S物料编号
      <fs_detail>-maktx_s = ls_stb-ojtxp."S物料描述
      <fs_detail>-matkl_s = ls_stb-matmk."S物料组
      "S物料组前4位-中分类
      IF ls_stb-matmk IS NOT INITIAL AND strlen( ls_stb-matmk ) > 4.
        <fs_detail>-matkl_s4 = ls_stb-matmk(4).
        gs_matkl-matkl  = ls_stb-matmk.
        INSERT gs_matkl INTO TABLE gt_matkl.
        gs_matkl-matkl   = ls_stb-matmk(4).
        INSERT gs_matkl INTO TABLE gt_matkl.
      ENDIF.
      <fs_detail>-sl_s = ls_stb-menge / is_bom_head-bmeng * cns_bom_bmeng."S组件需求数量-不含任何损耗
      <fs_detail>-meins_s = ls_stb-meins."S组件单位
      "生产时间-通过配置表读取时间
      SELECT SINGLE zdzeit INTO <fs_detail>-zdzeit FROM zppt0006 WHERE werks = ls_stb-werks AND matkl = ls_stb-matmk.
      "压制时间
      <fs_detail>-zmatscat_s = zcl_bc_public=>get_class_value( atnam = 'ZMATSCAT' key = ls_stb-idnrk classtype = '001' classnum = ls_stb-matmk objtable = 'MARA' ).
    ENDIF.
    "原辅料采购周期
    REFRESH gt_raw_cycle.
    PERFORM frm_get_raw_cycle USING it_stb it_matcat CHANGING gt_raw_cycle.
    "外包材物料信息
    CLEAR:gs_raw_cycle,ls_raw_cycle.
    READ TABLE it_wbc_cycle INDEX 1 INTO ls_raw_cycle.
    LOOP AT gt_raw_cycle INTO gs_raw_cycle WHERE ( stufe = 1 OR stufe = 2 ) AND mtart = 'Z020'.
      IF sy-subrc = 0.
        lv_flag = abap_true.
        DELETE gt_raw_cycle WHERE ( stufe = 1 OR stufe = 2  ) AND mtart = 'Z020'.
        EXIT.
      ENDIF.
    ENDLOOP.
    IF lv_flag = abap_false.
      CLEAR gs_raw_cycle.
    ENDIF.
    IF ls_raw_cycle-totaltime > gs_raw_cycle-totaltime.
      <fs_detail>-matnr_wb = ls_raw_cycle-idnrk."外包材物料编号
      <fs_detail>-maktx_wb = ls_raw_cycle-ojtxp."外包材物料描述
      <fs_detail>-ruezt_wb = ls_raw_cycle-zpretime."外包材物料准备天数
      <fs_detail>-plifz_wb = ls_raw_cycle-plifz."外包材物料计划交货时间(天)
      <fs_detail>-webaz_wb = ls_raw_cycle-webaz."外包材物料收货处理时间
    ELSE.
      <fs_detail>-matnr_wb = gs_raw_cycle-idnrk."外包材物料编号
      <fs_detail>-maktx_wb = gs_raw_cycle-ojtxp."外包材物料描述
      <fs_detail>-ruezt_wb = gs_raw_cycle-zpretime."外包材物料准备天数
      <fs_detail>-plifz_wb = gs_raw_cycle-plifz."外包材物料计划交货时间(天)
      <fs_detail>-webaz_wb = gs_raw_cycle-webaz."外包材物料收货处理时间
    ENDIF.
  
    "内包材物料信息stufe = 3
    IF gt_raw_cycle IS NOT INITIAL.
      READ TABLE gt_raw_cycle WITH KEY  mtart_head = 'Z030' mtart = 'Z020' INTO gs_raw_cycle.
      IF sy-subrc = 0.
        <fs_detail>-matnr_nb = gs_raw_cycle-idnrk."内包材物料编号
        <fs_detail>-maktx_nb = gs_raw_cycle-ojtxp."内包材物料描述
        <fs_detail>-ruezt_nb = gs_raw_cycle-zpretime."内包材物料准备天数
        <fs_detail>-plifz_nb = gs_raw_cycle-plifz."内包材物料计划交货时间(天)
        <fs_detail>-webaz_nb = gs_raw_cycle-webaz."内包材物料收货处理时间
      ENDIF.
      "内包材物料信息
      READ TABLE gt_raw_cycle WITH KEY  mtart = 'Z010' INTO gs_raw_cycle.
      IF sy-subrc = 0.
        <fs_detail>-matnr_yfl = gs_raw_cycle-idnrk."外包材物料编号
        <fs_detail>-maktx_yfl = gs_raw_cycle-ojtxp."外包材物料描述
        <fs_detail>-ruezt_yfl = gs_raw_cycle-zpretime."外包材物料准备天数
        <fs_detail>-plifz_yfl = gs_raw_cycle-plifz."外包材物料计划交货时间(天)
        <fs_detail>-webaz_yfl = gs_raw_cycle-webaz."外包材物料收货处理时间
      ENDIF.
    ENDIF.
  ENDFORM.
  FORM frm_get_raw_cycle  USING VALUE(it_stb) TYPE ty_tab_stpox VALUE(it_matcat) TYPE ty_tab_cscmat CHANGING ct_raw_cycle TYPE ty_tab_raw_cycle.
    "BOM展开抬头物料的物料类型
    IF it_matcat IS NOT INITIAL.
      SELECT matnr,mtart FROM mara FOR ALL ENTRIES IN @it_matcat WHERE matnr = @it_matcat-matnr INTO TABLE @DATA(lt_mara).
    ENDIF.
  
    SELECT t1~stufe,t1~ttidx,t1~idnrk AS matnr_head,t1~mtart,t1~ojtxp,t1~werks,t1~idnrk,marc~plifz,marc~webaz,z7~zpretime,marc~plifz AS totaltime
   FROM @it_stb AS t1
   INNER JOIN marc ON t1~werks = marc~werks AND t1~idnrk = marc~matnr
   LEFT JOIN zppt0007 AS z7 ON z7~werks = t1~werks AND z7~matnr = t1~idnrk
   WHERE t1~mtart  IN ( 'Z010','Z020' )
   INTO CORRESPONDING FIELDS OF TABLE @ct_raw_cycle.
    LOOP AT ct_raw_cycle ASSIGNING FIELD-SYMBOL(<fs_raw_cycle>).
      CLEAR:<fs_raw_cycle>-matnr_head,<fs_raw_cycle>-totaltime.
      READ TABLE it_matcat WITH KEY index = <fs_raw_cycle>-ttidx INTO DATA(ls_matcat).
      IF sy-subrc = 0.
        <fs_raw_cycle>-matnr_head = ls_matcat-matnr.
  *      READ TABLE it_stb WITH KEY idnrk = ls_matcat-matnr INTO DATA(ls_stb).
  *      IF sy-subrc = 0.
  *        <fs_raw_cycle>-mtart_head = ls_stb-mtart.
  *      ENDIF.
        READ TABLE lt_mara WITH KEY matnr = ls_matcat-matnr INTO DATA(ls_mara).
        IF sy-subrc = 0.
          <fs_raw_cycle>-mtart_head = ls_mara-mtart.
        ENDIF.
      ENDIF.
      IF <fs_raw_cycle>-zpretime IS NOT INITIAL.
        <fs_raw_cycle>-totaltime = <fs_raw_cycle>-zpretime + <fs_raw_cycle>-webaz.
      ELSE.
        <fs_raw_cycle>-totaltime = <fs_raw_cycle>-plifz + <fs_raw_cycle>-webaz.
      ENDIF.
    ENDLOOP.
    SORT ct_raw_cycle BY mtart totaltime DESCENDING.
  ENDFORM.
  *&---------------------------------------------------------------------*
  *& Form FRM_ALV_OUTPUT_DATA
  *&---------------------------------------------------------------------*
  *& text
  *&---------------------------------------------------------------------*
  *& -->  p1        text
  *& <--  p2        text
  *&---------------------------------------------------------------------*
  FORM frm_alv_output_data .
    DATA lt_detail TYPE TABLE OF ty_detail.
    "从AUSP读取物料001分类特征值
    "Seashell 20220530 物料特性新增批文来源
    IF gt_objek IS NOT INITIAL.
      SELECT ausp~objek,cabn~atnam,ausp~atwrt
        FROM cabn
        INNER JOIN ausp ON cabn~atinn = ausp~atinn
        FOR ALL ENTRIES IN @gt_objek
        WHERE cabn~atnam IN ( 'ZPRO','ZAPPSOU'  ) "是否益生菌-物料分类ZPRO特征值\批文来源
        AND ausp~objek  =  @gt_objek-objek
        AND ausp~klart = '001' "物料分类
        INTO TABLE @DATA(lt_ausp).
    ENDIF.
    "物料组描述
    IF gt_matkl IS NOT INITIAL .
      SELECT matkl,wgbez FROM t023t FOR ALL ENTRIES IN @gt_matkl
        WHERE matkl = @gt_matkl-matkl
        AND spras = @sy-langu
        INTO TABLE @DATA(lt_t023t).
    ENDIF.
    SORT lt_ausp BY objek atnam.
    SORT lt_t023t BY matkl.
    LOOP AT gt_alv_head INTO gs_alv.
      READ TABLE lt_ausp WITH KEY objek = CONV char90( gs_alv-matnr ) atnam = 'ZPRO' BINARY SEARCH INTO DATA(ls_ausp).
      IF sy-subrc = 0.
        gs_alv-zpro = ls_ausp-atwrt."是否益生菌-物料分类ZPRO特征值
      ENDIF.
  
      "20220530 Seashell 新增批文来源
      CLEAR ls_ausp.
      READ TABLE lt_ausp WITH KEY objek = CONV char90( gs_alv-matnr ) atnam = 'ZAPPSOU' BINARY SEARCH INTO ls_ausp.
      IF sy-subrc = 0.
        gs_alv-zappsou = ls_ausp-atwrt."是否益生菌-物料分类ZPRO特征值
      ENDIF.
      CLEAR ls_ausp.
  
  
      READ TABLE lt_t023t WITH KEY matkl = gs_alv-matkl INTO DATA(ls_t023t).
      IF sy-subrc = 0.
        gs_alv-cpxl_dec = ls_t023t-wgbez."成品小类-物料组描述
      ENDIF.
      READ TABLE lt_t023t WITH KEY matkl = gs_alv-matkl_4 INTO ls_t023t.
      IF sy-subrc = 0 .
        gs_alv-mjx_dec = ls_t023t-wgbez."剂型-物料组描述
      ENDIF.
      REFRESH lt_detail .
      lt_detail  = FILTER #( gt_alv_detail USING KEY skeys WHERE matnr = gs_alv-matnr AND werks = gs_alv-werks ).
      IF lt_detail IS INITIAL.
        APPEND gs_alv TO  gt_alv_output.
      ELSE.
        LOOP AT lt_detail INTO DATA(ls_detail).
          ls_detail-sl_m = ls_detail-sl_m / cns_bom_bmeng * gs_alv-bmeng.
          ls_detail-sl_s = ls_detail-sl_s / cns_bom_bmeng * gs_alv-bmeng.
          READ TABLE lt_t023t WITH KEY matkl = ls_detail-matkl_s INTO ls_t023t.
          IF sy-subrc = 0.
            ls_detail-bcpxl_dec = ls_t023t-wgbez."S物料组描述
          ENDIF.
          READ TABLE lt_t023t WITH KEY matkl = ls_detail-matkl_s4 INTO ls_t023t.
          IF sy-subrc = 0 .
            ls_detail-sjx_dec = ls_t023t-wgbez."S物料组前4位描述
          ENDIF.
          MOVE-CORRESPONDING ls_detail TO gs_alv.
          APPEND gs_alv TO  gt_alv_output.
        ENDLOOP.
      ENDIF.
    ENDLOOP.
  ENDFORM.
  *&---------------------------------------------------------------------*
  *& Form FRM_SET_RSDSWHERE
  *&---------------------------------------------------------------------*
  *& text
  *&---------------------------------------------------------------------*
  *& -->  p1        text
  *& <--  p2        text
  *&---------------------------------------------------------------------*
  FORM frm_set_rsdswhere .
    REFRESH gt_rsdswhere.
    IF s_kunnr IS NOT INITIAL.
      gs_rsdswhere-line = 'AND EXISTS ( SELECT vbeln FROM vbpa WHERE vbeln = v~vbeln AND parvw = ''Z1'' AND kunnr IN @S_KUNNR )'.
      APPEND gs_rsdswhere TO gt_rsdswhere.
    ENDIF.
    IF s_ywy IS NOT INITIAL .
      gs_rsdswhere-line = 'AND EXISTS ( SELECT vbeln FROM vbpa WHERE vbeln = k~vbeln AND parvw = ''Z3'' AND kunnr IN @s_ywy )'.
      APPEND gs_rsdswhere TO gt_rsdswhere.
    ENDIF.
  ENDFORM.