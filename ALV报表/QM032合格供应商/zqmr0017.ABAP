*&---------------------------------------------------------------------*
*& Report ZQMR0017
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zqmr0017.
TYPE-POOLS:slis."调用系统存在的类型池
*在调用ALV之前，需要先定义Layout和Fieldcat，他们属于slis类型池
DATA:fieldcat TYPE slis_t_fieldcat_alv WITH HEADER LINE,
     layout   TYPE slis_layout_alv,
     w_repid  TYPE sy-repid. "记录系统当前的程序名
TABLES:eord,mara,adrc,lfa1,but0id,bkpf,ekko,makt,zqmt011.
DATA:lbkpf LIKE STANDARD TABLE OF bkpf WITH HEADER LINE."标准表格式的内表lbkpf
TYPES:BEGIN OF ty_alv,
        matnr           TYPE eord-matnr, "物料编码
        werks           TYPE eord-werks, "工厂
        matkx           TYPE makt-maktx, "物料描述（短描述）
        matkl           TYPE mara-matkl, "物料类别
        lifnr           TYPE eord-lifnr, "经销商编号
        lifnr_name(100),"经销商名称
        stras           TYPE lfa1-stras, "经销商地址
        regiogroup      TYPE adrc-regiogroup, "经销商等级
        date1           TYPE but0id-valid_date_from, "经销商评估合格日期
        date2           TYPE but0id-valid_date_to, "经销商计划评估日期
        ematn           TYPE eord-ematn, "MPN料号
        mfrnr           TYPE mara-mfrnr, "生产商编号

        pro_name(100),"生厂商名称
        stras2          TYPE lfa1-stras, "生产厂家地址
        pro_regiogroup  TYPE adrc-regiogroup, "生厂商等级
        date3           TYPE but0id-valid_date_from, "生厂商评估合格日期
        date4           TYPE but0id-valid_date_to, "生产商计划评估日期
        zallergen(80),"过敏原
        ztrsgen(80),"转基因
        zexpmat(80),"贵重物料
        zwhp(80),"危险化学品
        zoriadd(80),"原产地
        zqmsamp(80),"开箱量计算标识
        cctj            TYPE zqmt011-cctj, "存储条件
        zmatdet(80),"材质明细
        zsamping(80),"QA取样量
        time(10)        TYPE c, "最近下单时间
      END OF ty_alv.
TYPES:BEGIN OF ty_all.
        INCLUDE TYPE ty_alv.
      TYPES:END OF ty_all.

DATA:gt_alv TYPE TABLE OF ty_alv,
     gs_alv LIKE LINE OF gt_alv.
DATA gv_sel_error TYPE flag.



SELECTION-SCREEN BEGIN OF BLOCK blk WITH FRAME TITLE TEXT-001."定义屏幕
PARAMETERS p_werks TYPE vbap-werks OBLIGATORY.
SELECT-OPTIONS:"s_werks FOR eord-werks,"条件输入框，一个for对应一个\工厂
s_matnr FOR eord-matnr,"for前面的是字段显示的名称，可以修改，但是注意不要超过8个字符，否则会报错、物料编码
s_lifnr FOR eord-lifnr,"经销商编码
s_mfrnr FOR mara-mfrnr,"制造商编码
s_group1 FOR adrc-regiogroup,"经销商等级
s_group2 FOR adrc-regiogroup,"制造商等级
s_date1 FOR but0id-valid_date_from,"经销商评估合格日期
s_date2 FOR but0id-valid_date_to,"经销商计划评估日期
s_date3 FOR but0id-valid_date_from,"生产商计划评估日期
s_date4 FOR but0id-valid_date_to,"生厂商计划评估日期
s_time FOR ekko-aedat,"最近下单日期
s_matkl FOR mara-matkl."物料组
SELECTION-SCREEN END OF BLOCK blk.

START-OF-SELECTION.
  PERFORM frm_authority_check.
  CHECK gv_sel_error IS INITIAL."权限检查标识
*调用子程序
  PERFORM getdata.
  PERFORM catalog.
  PERFORM alvshow.
*定义子程序
FORM getdata."查询语句
  DATA:lv_zallergen(10) TYPE c,
       lv_ztrsgen(10)   TYPE c,
       lv_zexpmat(10)   TYPE c,
       lv_zwhp(10)      TYPE c,
       lv_zoriadd(10)   TYPE c,
       lv_zqmsamp(10)   TYPE c,
       lv_zmatdet(10)   TYPE c,
       lv_zsamping(10)  TYPE c.
  lv_zallergen = 'ZALLERGEN'.
  lv_ztrsgen = 'ZTRSGEN'.
  lv_zexpmat = 'ZEXPMAT'.
  lv_zwhp = 'ZWHP'.
  lv_zoriadd = 'ZORIADD'.
  lv_zqmsamp = 'ZQMSAMP'.
  lv_zmatdet = 'ZMATDET'.
  lv_zsamping = 'ZSAMPING'.
  "使用宏来改变特征名转换前后的值
  DEFINE changeausp.
    CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
    EXPORTING
      input  = &1
    IMPORTING
      output = &2.
  END-OF-DEFINITION.
  changeausp lv_zallergen lv_zallergen.
  changeausp lv_ztrsgen lv_ztrsgen.
  changeausp lv_zexpmat lv_zexpmat.
  changeausp lv_zwhp lv_zwhp.
  changeausp lv_zoriadd lv_zoriadd.
  changeausp lv_zqmsamp lv_zqmsamp.
  changeausp lv_zmatdet lv_zmatdet.
  changeausp lv_zsamping lv_zsamping.
  "先查询的是经销商的数据
  SELECT
  werks,
  eord~matnr,
  lifnr,
  ematn,
  partner,
  mfrnr,
  matkl,
  valid_date_from AS date1,
  valid_date_to AS date2,
  but0id~type AS flag
  INTO TABLE @DATA(gt_all)
  FROM eord
  LEFT JOIN but0id
  ON but0id~partner = eord~lifnr
  LEFT JOIN mara
  ON mara~matnr = eord~matnr
  WHERE eord~matnr IN @s_matnr
  AND eord~werks = @p_werks
  AND eord~lifnr IN @s_lifnr
  AND eord~notkz <> 'X'
  AND but0id~valid_date_from IN @s_date1
  AND but0id~valid_date_to IN @s_date2
  AND mara~matkl IN @s_matkl.
  SORT gt_all BY werks matnr lifnr ematn.
  DELETE ADJACENT DUPLICATES FROM gt_all COMPARING werks matnr lifnr ematn.
  "然后查询的是生产商的数据

  SELECT
  matkl,
  ematn,
  partner,
  mfrnr,
  valid_date_from AS date3,
  valid_date_to AS date4
  FROM mara
  INNER JOIN eord
  ON eord~ematn = mara~matnr
  INNER JOIN but0id
  ON mara~mfrnr = but0id~partner
  FOR ALL ENTRIES IN @gt_all
  WHERE mara~matnr = @gt_all-ematn
    AND but0id~type = 'ZQM999'
  INTO TABLE @DATA(gt_all2).

















  IF gt_all IS NOT INITIAL.
    "MAKT表
    SELECT
    matnr,
    maktx
    INTO TABLE @DATA(lt_makt)
          FROM makt
          FOR ALL ENTRIES IN @gt_all
          WHERE makt~spras = '1'
          AND makt~matnr = @gt_all-matnr.

    "LFA1表
    SELECT
    name1,
    name2,
    name3,
    name4,
    lifnr,
    stras
    INTO TABLE @DATA(lt_lfa1)
          FROM lfa1
          FOR ALL ENTRIES IN @gt_all
          WHERE lfa1~lifnr = @gt_all-lifnr.

    "ADRC表
    SELECT
    lifnr,
    adrc~name1,
    adrc~name2,
    adrc~name3,
    adrc~name4,
    adrc~regiogroup
    FROM adrc
    INNER JOIN lfa1
    ON lfa1~name1 = adrc~name1
    AND lfa1~name2 = adrc~name2
    AND lfa1~name3 = adrc~name3
    AND lfa1~name4 = adrc~name4
    FOR ALL ENTRIES IN @gt_all
    WHERE lfa1~lifnr = @gt_all-lifnr
    INTO TABLE @DATA(lt_adrc).

    "MARA表
    SELECT
    a~mfrnr,
    b~lifnr,
    b~stras,
    b~name1,
    b~name2,
    b~name3,
    b~name4,
    a~matnr
    FROM mara AS a
    INNER JOIN lfa1 AS b
    ON b~lifnr = a~mfrnr
    FOR ALL ENTRIES IN @gt_all
    WHERE a~matnr = @gt_all-ematn
    INTO TABLE @DATA(lt_mara).

    "ADRC表2
    IF lt_mara IS NOT INITIAL.
      SELECT
      a~mfrnr,
      b~lifnr,
      b~stras,
      adrc~name1,
      adrc~name2,
      adrc~name3,
      adrc~name4,
      a~matnr,
      adrc~regiogroup
      FROM mara AS a
      INNER JOIN lfa1 AS b
      ON b~lifnr = a~mfrnr
      INNER JOIN adrc
      ON adrc~name1 = b~name1
      AND adrc~name2 = b~name2
      AND adrc~name3 = b~name3
      AND adrc~name4 = b~name4
      FOR ALL ENTRIES IN @gt_all
      WHERE a~matnr = @gt_all-ematn
      INTO TABLE @DATA(lt_adrc2).
    ENDIF.

    "ZQMT011表
    SELECT
    matnr,
    raube,
    sc,
    cctj
    FROM zqmt011
    INNER JOIN mara
    ON mara~raube = zqmt011~sc
    FOR ALL ENTRIES IN @gt_all
    WHERE zqmt011~sc = mara~raube
    AND mara~matnr = @gt_all-matnr
    INTO TABLE @DATA(lt_zqmt011).

    "AUSP表
    SELECT
    objek,
    atinn,
    atwrt
    INTO TABLE @DATA(lt_ausp)
          FROM ausp
          WHERE  ausp~atinn IN (@lv_zallergen,@lv_ztrsgen,@lv_zexpmat,
          @lv_zwhp,@lv_zoriadd,@lv_zqmsamp,@lv_zmatdet,@lv_zsamping).

    "最近下单时间相关的查询
    "首先第一个是有MPN物料的
    SELECT
    matnr,
    ematn,
    ekko~ebeln,
    lifnr,
    ekko~aedat
    FROM ekko
    INNER JOIN ekpo
    ON ekpo~ebeln = ekko~ebeln
    FOR ALL ENTRIES IN @gt_all
    WHERE ekpo~ematn = @gt_all-ematn
    AND ekpo~matnr = @gt_all-matnr
    AND ekko~lifnr = @gt_all-lifnr
    AND ekko~aedat <= @sy-datum
    AND ekko~aedat > @( sy-datum - 731 )
    AND ekpo~loekz <> 'X'
    INTO TABLE @DATA(lt_time).
    SORT lt_time BY aedat DESCENDING.
    "第二个是没有MPN物料的
    SELECT
    matnr,
    ekko~ebeln,
    lifnr,
    ekko~aedat
    FROM ekko
    INNER JOIN ekpo
    ON ekpo~ebeln = ekko~ebeln
    FOR ALL ENTRIES IN @gt_all
    WHERE ekpo~matnr = @gt_all-matnr
    AND ekko~lifnr = @gt_all-lifnr
    AND ekko~aedat <= @sy-datum
    AND ekko~aedat > @( sy-datum - 731 )
    AND ekpo~loekz <> 'X'
    INTO TABLE @DATA(lt_time2).
    SORT lt_time2 BY aedat DESCENDING.

    "如果选择屏幕最近下单时间不为空的话，下面对数据进行整理
    IF s_time IS NOT INITIAL.
      SELECT
      *
      FROM @lt_time AS a
      WHERE a~aedat IN @s_time
      INTO TABLE @lt_time.

      SELECT
      *
      FROM @lt_time2 AS a
      WHERE a~aedat IN @s_time
      INTO TABLE @lt_time2.
    ENDIF.






  ENDIF.













  LOOP AT gt_all INTO DATA(gs_all).
    MOVE-CORRESPONDING gs_all TO gs_alv.
    "物料描述（短描述）
    READ TABLE lt_makt INTO DATA(ls_makt) WITH KEY matnr = gs_all-matnr.
    IF sy-subrc = 0.
      gs_alv-matkx = ls_makt-maktx.
      CLEAR ls_makt.
    ENDIF.
    "物料类别
    IF gs_alv-matkl+0(2) = '10' OR gs_alv-matkl+0(4) = '2002' OR gs_alv-matkl+0(4) = '2006'.
      gs_alv-matkl = 'I类'.
    ELSEIF gs_alv-matkl+0(4) = '2004' OR gs_alv-matkl = '2008'.
      gs_alv-matkl = 'III类'.
    ENDIF.
    "经销商名称、经销商地址
    READ TABLE lt_lfa1 INTO DATA(ls_lfa1) WITH KEY lifnr = gs_all-lifnr.
    IF sy-subrc = 0.
      gs_alv-lifnr_name = ls_lfa1-name1 && ls_lfa1-name2 && ls_lfa1-name3 && ls_lfa1-name4.
      gs_alv-stras = ls_lfa1-stras.
      CLEAR ls_lfa1.
    ENDIF.
    "经销商等级
    READ TABLE lt_adrc INTO DATA(ls_adrc) WITH KEY lifnr = gs_all-lifnr.
    IF sy-subrc = 0.
      gs_alv-regiogroup = ls_adrc-regiogroup.
      CLEAR ls_adrc.
    ENDIF.
    "判断经销商等级是否在选择屏幕的条件范围内
    DATA ls_group1 LIKE LINE OF s_group1.
    IF s_group1 IS NOT INITIAL.
      READ TABLE s_group1 INTO ls_group1 WITH KEY low = gs_alv-regiogroup.
      IF sy-subrc <> 0.
        CLEAR ls_group1.
        CONTINUE.
      ENDIF.
    ENDIF.

    "经销商评估合格日期
    "经销商计划评估日期
    "MPN料号
    IF gs_all-ematn IS INITIAL.
      gs_alv-ematn = '无'.
      "不启用MPN的生产商编号及名称、地址、等级、日期和经销商的一致
      gs_alv-mfrnr = gs_all-lifnr.
      gs_alv-pro_name = gs_alv-lifnr_name.
      gs_alv-stras2 = gs_alv-stras.
      gs_alv-pro_regiogroup = gs_alv-regiogroup.
      gs_alv-date3 = gs_alv-date1.
      gs_alv-date4 = gs_alv-date2.

      "最近下单时间
      SELECT
      ekpo~ebeln,
      ekko~aedat
      INTO TABLE @DATA(lt_ebeln2)
            FROM ekpo
            INNER JOIN ekko
            ON ekpo~ebeln = ekko~ebeln
            WHERE ekpo~matnr = @gs_all-matnr
            AND ekko~lifnr = @gs_all-lifnr.
      SORT lt_ebeln2 BY aedat DESCENDING.
      READ TABLE lt_ebeln2 INTO DATA(lv_ebeln2) INDEX 1.
      READ TABLE lt_time2 INTO DATA(ls_time2) WITH KEY matnr = gs_all-matnr
                                                       lifnr = gs_all-lifnr
                                                       ebeln = lv_ebeln2-ebeln.
      IF sy-subrc = 0.
        gs_alv-time = ls_time2-aedat+0(4) && '.' && ls_time2-aedat+4(2) && '.' && ls_time2-aedat+6(2).
      ELSE.
        IF s_time IS NOT INITIAL.
          CONTINUE.
        ENDIF.
        gs_alv-time = '超过2年未下单'.
      ENDIF.
      CLEAR lv_ebeln2.
      CLEAR:ls_time2.
    ELSE.
      READ TABLE lt_mara INTO DATA(ls_mara) WITH KEY matnr = gs_all-ematn.
      IF sy-subrc = 0.
        "生产商编号
        gs_alv-mfrnr = ls_mara-mfrnr.
        "生产商名称
        gs_alv-pro_name = ls_mara-name1 && ls_mara-name2 && ls_mara-name3 && ls_mara-name4.
        "生产厂家地址
        gs_alv-stras2 = ls_mara-stras.
        "生产商等级
        READ TABLE lt_adrc2 INTO DATA(ls_adrc2) WITH KEY matnr = gs_all-ematn.
        IF sy-subrc = 0.
          gs_alv-pro_regiogroup = ls_adrc2-regiogroup.
          CLEAR ls_adrc2.
        ENDIF.
      ENDIF.

      "生产商评估合格日期、生产商计划评估日期
      READ TABLE gt_all2 INTO DATA(gs_all2) WITH KEY ematn = gs_all-ematn
                                                     mfrnr = gs_all-mfrnr.
      IF sy-subrc = 0.
        gs_alv-date3 = gs_all2-date3.
        gs_alv-date4 = gs_all2-date4.
      ENDIF.
      CLEAR gs_all2.

      "最近下单时间
      SELECT
      ekko~ebeln,
      ekko~aedat
      INTO TABLE @DATA(lt_ebeln)
            FROM ekpo
            INNER JOIN ekko
            ON ekko~ebeln = ekpo~ebeln
            WHERE ekpo~matnr = @gs_all-matnr
            AND ekpo~ematn = @gs_all-ematn.
      SORT lt_ebeln BY aedat DESCENDING.
      READ TABLE lt_ebeln INTO DATA(lv_ebeln) INDEX 1.
      READ TABLE lt_time INTO DATA(ls_time) WITH KEY matnr = gs_all-matnr
                                                     ematn = gs_all-ematn
                                                     lifnr = gs_all-lifnr
                                                     ebeln = lv_ebeln-ebeln.
      IF sy-subrc = 0.
        gs_alv-time = ls_time-aedat+0(4) && '.' && ls_time-aedat+4(2) && '.' && ls_time-aedat+6(2).
      ELSE.
        IF s_time IS NOT INITIAL.
          CONTINUE.
        ENDIF.
        gs_alv-time = '超过2年未下单'.
      ENDIF.
      CLEAR lv_ebeln.
      CLEAR ls_time.
    ENDIF.
    "判断生产商是否在选择屏幕条件内
    DATA ls_group2 LIKE LINE OF s_group2.
    IF s_group2 IS NOT INITIAL.
      READ TABLE s_group2 INTO ls_group2 WITH KEY low = gs_alv-pro_regiogroup.
      IF sy-subrc <> 0.
        CLEAR ls_group2.
        CONTINUE.
      ENDIF.
    ENDIF.
    "存储条件
    READ TABLE lt_zqmt011 INTO DATA(ls_zqmt011) WITH KEY matnr = gs_all-matnr.
    IF sy-subrc = 0.
      gs_alv-cctj = ls_zqmt011-cctj.
      CLEAR ls_zqmt011.
    ENDIF.
    "各种特性值相关
    READ TABLE lt_ausp INTO DATA(ls_ausp) WITH KEY objek = gs_all-matnr
          atinn = lv_zallergen.
    IF sy-subrc = 0.
      "过敏原
      gs_alv-zallergen = ls_ausp-atwrt.
      CLEAR ls_ausp.
    ENDIF.
    READ TABLE lt_ausp INTO ls_ausp WITH KEY objek = gs_all-matnr
    atinn = lv_ztrsgen.
    IF sy-subrc = 0.
      "转基因
      gs_alv-ztrsgen = ls_ausp-atwrt.
      CLEAR ls_ausp.
    ENDIF.
    READ TABLE lt_ausp INTO ls_ausp WITH KEY objek = gs_all-matnr
    atinn = lv_zexpmat.
    IF sy-subrc = 0.
      "贵重物料
      gs_alv-zexpmat = ls_ausp-atwrt.
      CLEAR ls_ausp.
    ENDIF.
    READ TABLE lt_ausp INTO ls_ausp WITH KEY objek = gs_all-matnr
    atinn = lv_zwhp.
    IF sy-subrc = 0.
      "危险化学品
      gs_alv-zwhp = ls_ausp-atwrt.
      CLEAR ls_ausp.
    ENDIF.
    READ TABLE lt_ausp INTO ls_ausp WITH KEY objek = gs_all-matnr
    atinn = lv_zoriadd.
    IF sy-subrc = 0.
      "原产地
      gs_alv-zoriadd = ls_ausp-atwrt.
      CLEAR ls_ausp.
    ENDIF.
    READ TABLE lt_ausp INTO ls_ausp WITH KEY objek = gs_all-matnr
    atinn = lv_zqmsamp.
    IF sy-subrc = 0.
      "开箱量计算标识
      gs_alv-zqmsamp = ls_ausp-atwrt.
      CLEAR ls_ausp.
    ENDIF.
    READ TABLE lt_ausp INTO ls_ausp WITH KEY objek = gs_all-matnr
    atinn = lv_zmatdet.
    IF sy-subrc = 0.
      "材质明细
      gs_alv-zmatdet = ls_ausp-atwrt.
      CLEAR ls_ausp.
    ENDIF.
    READ TABLE lt_ausp INTO ls_ausp WITH KEY objek = gs_all-matnr
    atinn = lv_zsamping.
    IF sy-subrc = 0.
      "QA取样量
      gs_alv-zsamping = ls_ausp-atwrt.
      CLEAR ls_ausp.
    ENDIF.

















    IF gs_alv-lifnr = '8100' OR gs_alv-lifnr = '8200'
    OR gs_alv-lifnr = '0000008100' OR gs_alv-lifnr = '0000008200'.
      CONTINUE.
    ENDIF.
    IF gs_all-flag <> 'ZQM999'.
      gs_alv-date1 = ''.
      gs_alv-date2 = ''.
      gs_alv-date3 = ''.
      gs_alv-date4 = ''.
    ENDIF.

    APPEND gs_alv TO gt_alv.
    CLEAR:gs_all,gs_alv.
  ENDLOOP.
  SELECT
    *
  FROM @gt_alv AS a
  WHERE a~date3 IN @s_date3
    AND a~date4 IN @s_date4
    AND a~mfrnr IN @s_mfrnr
  INTO TABLE @gt_alv.
  LOOP AT gt_alv INTO gs_alv.
    "去掉前导0
    gs_alv-matnr = zcl_bc_public=>conv_by_ddic( i_input = gs_alv-matnr i_out = 'X' ).
    gs_alv-lifnr = zcl_bc_public=>conv_by_ddic( i_input = gs_alv-lifnr i_out = 'X' ).
    gs_alv-ematn = zcl_bc_public=>conv_by_ddic( i_input = gs_alv-ematn i_out = 'X' ).
    gs_alv-mfrnr = zcl_bc_public=>conv_by_ddic( i_input = gs_alv-mfrnr i_out = 'X' ).
    MODIFY gt_alv FROM gs_alv.
    CLEAR gs_alv.
  ENDLOOP.
*  SORT gt_alv BY matnr werks matkx matkl lifnr.
*  DELETE ADJACENT DUPLICATES FROM gt_alv COMPARING matnr werks matkx matkl lifnr.


ENDFORM.

FORM catalog."表头
  w_repid = sy-repid.
  CLEAR fieldcat.

  DEFINE fieldcatset."宏定义
    fieldcat-just = 'C'."字段居中显示
    fieldcat-outputlen = 10."自定义字段的长度
    fieldcat-fieldname = &1."透明表字段名
    fieldcat-seltext_l = &2."ALV列名
    fieldcat-col_pos = &3."列位置
    APPEND fieldcat.
  END-OF-DEFINITION.

  fieldcatset   'MATNR'   '物料编码'  sy-tabix.
  fieldcatset   'WERKS'   '工厂'  sy-tabix.
  fieldcatset   'MATKX'   '物料描述（短描述）' sy-tabix.
  fieldcatset   'MATKL'   '物料类别'  sy-tabix.
  fieldcatset   'LIFNR'   '经销商编号' sy-tabix.
  fieldcatset   'LIFNR_NAME'   '经销商名称'  sy-tabix.
  fieldcatset   'STRAS'   '经销商地址' sy-tabix.
  fieldcatset   'REGIOGROUP'   '经销商等级'  sy-tabix.
  fieldcatset   'DATE1'   '经销商评估合格日期' sy-tabix.
  fieldcatset   'DATE2'   '经销商计划评估日期' sy-tabix.
  fieldcatset   'EMATN'   'MPN料号' sy-tabix.
  fieldcatset   'MFRNR'   '生产商编号' sy-tabix.
  fieldcatset   'PRO_NAME'    '生产商名称' sy-tabix.
  fieldcatset   'STRAS2'    '生产厂家地址'  sy-tabix.
  fieldcatset   'PRO_REGIOGROUP'    '生产商等级' sy-tabix.
  fieldcatset   'DATE3'   '生产商评估合格日期' sy-tabix.
  fieldcatset   'DATE4'   '生产商计划评估日期' sy-tabix.
  fieldcatset   'ZALLERGEN'   '过敏原' sy-tabix.
  fieldcatset   'ZTRSGEN'   '转基因' sy-tabix.
  fieldcatset   'ZEXPMAT'   '贵重物料'  sy-tabix.
  fieldcatset   'ZWHP'    '危险化学品' sy-tabix.
  fieldcatset   'ZORIADD'   '原产地' sy-tabix.
  fieldcatset   'ZQMSAMP'   '开箱量计算标识' sy-tabix.
  fieldcatset   'CCTJ'  '存储条件'  sy-tabix.
  fieldcatset   'ZMATDET'   '材质明细'  sy-tabix.
  fieldcatset   'ZSAMPING'    'QA取样量' sy-tabix.
  fieldcatset   'TIME'    '最近下单时间'  sy-tabix.




*layout-colwidth_optimize = 'X'.
  layout-zebra = 'X'."斑马线的样式

ENDFORM.
*响应鼠标点击时间的子程序
FORM alv_user_command USING r_ucomm LIKE sy-ucomm
      rs_selfield TYPE slis_selfield.
  CASE r_ucomm.
    WHEN '&IC1'."默认的值就是这个，网上查资料据说能修改~
      READ TABLE lbkpf INDEX rs_selfield-tabindex.
      SET PARAMETER ID 'BLN' FIELD lbkpf-belnr.
      CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
  ENDCASE.

ENDFORM.

FORM frm_set_pf_status USING pt_extab TYPE slis_t_extab.
  SET PF-STATUS 'ZHKALV1'.
ENDFORM.



FORM alvshow.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
*     I_INTERFACE_CHECK        = ' '
*     I_BYPASSING_BUFFER       = ' '
*     I_BUFFER_ACTIVE          = ' '
      i_callback_program       = w_repid "程序名称
      i_callback_pf_status_set = 'FRM_SET_PF_STATUS'
      i_callback_user_command  = 'ALV_USER_COMMAND' "对ALV操作的时候触发所定义的子程序
*     I_CALLBACK_TOP_OF_PAGE   = ' '
*     I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*     I_CALLBACK_HTML_END_OF_LIST       = ' '
*     I_STRUCTURE_NAME         =
*     I_BACKGROUND_ID          = ' '
*     i_grid_title             = ''' "标题名
*     I_GRID_SETTINGS          =
      is_layout                = layout "程序所定义的layout名称
      it_fieldcat              = fieldcat[] "定义fieldcat数据
*     IT_EXCLUDING             =
*     IT_SPECIAL_GROUPS        =
*     IT_SORT                  =
*     IT_FILTER                =
*     IS_SEL_HIDE              =
*     I_DEFAULT                = 'X'
*     I_SAVE                   = ' '
*     IS_VARIANT               =
*     IT_EVENTS                =
*     IT_EVENT_EXIT            =
*     IS_PRINT                 =
*     IS_REPREP_ID             =
*     I_SCREEN_START_COLUMN    = 0
*     I_SCREEN_START_LINE      = 0
*     I_SCREEN_END_COLUMN      = 0
*     I_SCREEN_END_LINE        = 0
*     I_HTML_HEIGHT_TOP        = 0
*     I_HTML_HEIGHT_END        = 0
*     IT_ALV_GRAPHICS          =
*     IT_HYPERLINK             =
*     IT_ADD_FIELDCAT          =
*     IT_EXCEPT_QINFO          =
*     IR_SALV_FULLSCREEN_ADAPTER        =
* IMPORTING
*     E_EXIT_CAUSED_BY_CALLER  =
*     ES_EXIT_CAUSED_BY_USER   =
    TABLES
      t_outtab                 = gt_alv
    EXCEPTIONS "下面都是默认的
      program_error            = 1
      OTHERS                   = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.



FORM frm_authority_check.

  AUTHORITY-CHECK OBJECT 'M_ORDR_WRK'
   ID 'ACTVT' DUMMY
  ID 'WERKS' FIELD p_werks.
  IF sy-subrc <> 0.
* Implement a suitable exception handling here
    MESSAGE '权限检查失败' TYPE 'E'.
    gv_sel_error = 'X'.
  ELSE.
    MESSAGE '有权限' TYPE 'S'.
    gv_sel_error = ''.
  ENDIF.

ENDFORM.