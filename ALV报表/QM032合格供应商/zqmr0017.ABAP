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
        time            TYPE ekko-aedat, "最近下单时间
      END OF ty_alv.
TYPES:BEGIN OF ty_all.
        INCLUDE TYPE ty_alv.
      TYPES:END OF ty_all.

DATA:gt_alv TYPE TABLE OF ty_alv,
     gs_alv LIKE LINE OF gt_alv.




SELECTION-SCREEN BEGIN OF BLOCK blk WITH FRAME TITLE TEXT-001."定义屏幕
SELECT-OPTIONS:s_werks FOR eord-werks,"条件输入框，一个for对应一个\工厂
               s_matnr FOR eord-matnr,"for前面的是字段显示的名称，可以修改，但是注意不要超过8个字符，否则会报错、物料编码
               s_lifnr FOR eord-lifnr,"制造商编码
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
    valid_date_to AS date2
  INTO TABLE @DATA(gt_all)
  FROM eord
  INNER JOIN but0id
  ON but0id~partner = eord~lifnr
  INNER JOIN mara
  ON mara~matnr = eord~matnr
  WHERE eord~matnr IN @s_matnr
    AND eord~werks IN @s_werks
    AND eord~lifnr IN @s_lifnr
    AND but0id~type = 'ZQM999'
    AND but0id~valid_date_from IN @s_date1
    AND but0id~valid_date_to IN @s_date2
    AND mara~mfrnr IN @s_mfrnr
    AND mara~matkl IN @s_matkl.
  "然后查询的是生产商的数据
  SELECT
    werks,
    a~matkl,
    a~matnr,
    lifnr,
    ematn,
    a~partner,
    a~mfrnr,
    a~date1,
    a~date2,
    valid_date_from AS date3,
    valid_date_to AS date4
  FROM @gt_all AS a
  INNER JOIN mara
  ON mara~mfrnr = a~partner
  INNER JOIN but0id
  ON mara~mfrnr = but0id~partner
  WHERE but0id~type = 'ZQM999'
    AND but0id~valid_date_from IN @s_date3
    AND but0id~valid_date_to IN @s_date4
    AND mara~matkl IN @s_matkl
  INTO TABLE @DATA(gt_all2).



  IF gt_all2 IS NOT INITIAL.
    "MAKT表
    SELECT
    matnr,
    maktx
    INTO TABLE @DATA(lt_makt)
    FROM makt
    FOR ALL ENTRIES IN @gt_all2
    WHERE makt~spras = '1'
    AND makt~matnr = @gt_all2-matnr.

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
    FOR ALL ENTRIES IN @gt_all2
    WHERE lfa1~lifnr = @gt_all2-lifnr.

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
    FOR ALL ENTRIES IN @gt_all2
    WHERE lfa1~lifnr = @gt_all2-lifnr
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
    FOR ALL ENTRIES IN @gt_all2
    WHERE a~matnr = @gt_all2-ematn
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
      FOR ALL ENTRIES IN @gt_all2
      WHERE a~matnr = @gt_all2-ematn
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
    FOR ALL ENTRIES IN @gt_all2
    WHERE zqmt011~sc = mara~raube
      AND mara~matnr = @gt_all2-matnr
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
    FOR ALL ENTRIES IN @gt_all2
    WHERE ekpo~ematn = @gt_all2-ematn
      AND ekpo~matnr = @gt_all2-matnr
      AND ekko~lifnr = @gt_all2-lifnr
      AND ekko~aedat <= @sy-datum
      AND ekko~aedat > @( sy-datum - 731 )
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
    FOR ALL ENTRIES IN @gt_all2
    WHERE ekpo~matnr = @gt_all2-matnr
      AND ekko~lifnr = @gt_all2-lifnr
      AND ekko~aedat <= @sy-datum
      AND ekko~aedat > @( sy-datum - 731 )
    INTO TABLE @DATA(lt_time2).
    SORT lt_time2 BY aedat DESCENDING.








  ENDIF.















  LOOP AT gt_all2 INTO DATA(gs_all2).
    MOVE-CORRESPONDING gs_all2 TO gs_alv.
    "物料描述（短描述）
    READ TABLE lt_makt INTO DATA(ls_makt) WITH KEY matnr = gs_all2-matnr.
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
    READ TABLE lt_lfa1 INTO DATA(ls_lfa1) WITH KEY lifnr = gs_all2-lifnr.
    IF sy-subrc = 0.
      gs_alv-lifnr_name = ls_lfa1-name1 && ls_lfa1-name2 && ls_lfa1-name3 && ls_lfa1-name4.
      gs_alv-stras = ls_lfa1-stras.
      CLEAR ls_lfa1.
    ENDIF.
    "经销商等级
    READ TABLE lt_adrc INTO DATA(ls_adrc) WITH KEY lifnr = gs_all2-lifnr.
    IF sy-subrc = 0.
      gs_alv-regiogroup = ls_adrc-regiogroup.
      CLEAR ls_adrc.
    ENDIF.
    "判断经销商等级是否在选择屏幕的条件范围内
    DATA ls_group1 LIKE LINE OF s_group1.
    IF s_group1 IS NOT INITIAL.
      READ TABLE s_group1 INTO ls_group1 WITH KEY low = gs_alv-regiogroup.
      IF sy-subrc = 0.
        CLEAR ls_group1.
        CONTINUE.
      ENDIF.
    ENDIF.

    "经销商评估合格日期
    "经销商计划评估日期
    "MPN料号
    IF gs_all2-ematn IS INITIAL.
      gs_alv-ematn = '无'.
      "不启用MPN的生产商编号及名称、地址、等级、日期和经销商的一致
      gs_alv-mfrnr = gs_all2-lifnr.
      gs_alv-pro_name = gs_alv-lifnr_name.
      gs_alv-stras2 = gs_alv-stras.
      gs_alv-pro_regiogroup = gs_alv-regiogroup.
      gs_alv-date3 = gs_alv-date1.
      gs_alv-date4 = gs_alv-date2.

      "最近下单时间
      READ TABLE lt_time2 INTO DATA(ls_time2) WITH KEY matnr = gs_all2-matnr
                                                      lifnr = gs_all2-lifnr.
      IF sy-subrc = 0.
        gs_alv-time = ls_time2-aedat.
      ELSE.
        gs_alv-time = '超过2年未下单'.
      ENDIF.
      CLEAR ls_time2.
    ELSE.
      READ TABLE lt_mara INTO DATA(ls_mara) WITH KEY matnr = gs_all2-ematn.
      IF sy-subrc = 0.
        "生产商编号
        gs_alv-mfrnr = ls_mara-mfrnr.
        "生产商名称
        gs_alv-pro_name = ls_mara-name1 && ls_mara-name2 && ls_mara-name3 && ls_mara-name4.
        "生产厂家地址
        gs_alv-stras2 = ls_mara-stras.
        "生产商等级
        READ TABLE lt_adrc2 INTO DATA(ls_adrc2) WITH KEY matnr = gs_all2-ematn.
        IF sy-subrc = 0.
          gs_alv-pro_regiogroup = ls_adrc2-regiogroup.
          CLEAR ls_adrc2.
        ENDIF.
      ENDIF.

      "最近下单时间
      READ TABLE lt_time INTO DATA(ls_time) WITH KEY matnr = gs_all2-matnr
                                                     ematn = gs_all2-ematn.
      IF sy-subrc = 0.
        gs_alv-time = ls_time-aedat.
      ELSE.
        gs_alv-time = '超过2年未下单'.
      ENDIF.
      CLEAR ls_time.
    ENDIF.
    "判断生产商是否在选择屏幕条件内
    DATA ls_group2 LIKE LINE OF s_group2.
    IF s_group2 IS NOT INITIAL.
      READ TABLE s_group2 INTO ls_group2 WITH KEY low = gs_alv-pro_regiogroup.
      IF sy-subrc = 0.
        CLEAR ls_group2.
        CONTINUE.
      ENDIF.
    ENDIF.
    "存储条件
    READ TABLE lt_zqmt011 INTO DATA(ls_zqmt011) WITH KEY matnr = gs_all2-matnr.
    IF sy-subrc = 0.
      gs_alv-cctj = ls_zqmt011-cctj.
      CLEAR ls_zqmt011.
    ENDIF.
    "各种特性值相关
    READ TABLE lt_ausp INTO DATA(ls_ausp) WITH KEY objek = gs_all2-matnr
                                                   atinn = lv_zallergen.
    IF sy-subrc = 0.
      "过敏原
      gs_alv-zallergen = ls_ausp-atwrt.
      CLEAR ls_ausp.
    ENDIF.
    READ TABLE lt_ausp INTO ls_ausp WITH KEY objek = gs_all2-matnr
                                             atinn = lv_ztrsgen.
    IF sy-subrc = 0.
      "转基因
      gs_alv-ztrsgen = ls_ausp-atwrt.
      CLEAR ls_ausp.
    ENDIF.
    READ TABLE lt_ausp INTO ls_ausp WITH KEY objek = gs_all2-matnr
                                             atinn = lv_zexpmat.
    IF sy-subrc = 0.
      "贵重物料
      gs_alv-zexpmat = ls_ausp-atwrt.
      CLEAR ls_ausp.
    ENDIF.
    READ TABLE lt_ausp INTO ls_ausp WITH KEY objek = gs_all2-matnr
                                             atinn = lv_zwhp.
    IF sy-subrc = 0.
      "危险化学品
      gs_alv-zwhp = ls_ausp-atwrt.
      CLEAR ls_ausp.
    ENDIF.
    READ TABLE lt_ausp INTO ls_ausp WITH KEY objek = gs_all2-matnr
                                             atinn = lv_zoriadd.
    IF sy-subrc = 0.
      "原产地
      gs_alv-zoriadd = ls_ausp-atwrt.
      CLEAR ls_ausp.
    ENDIF.
    READ TABLE lt_ausp INTO ls_ausp WITH KEY objek = gs_all2-matnr
                                             atinn = lv_zqmsamp.
    IF sy-subrc = 0.
      "开箱量计算标识
      gs_alv-zqmsamp = ls_ausp-atwrt.
      CLEAR ls_ausp.
    ENDIF.
    READ TABLE lt_ausp INTO ls_ausp WITH KEY objek = gs_all2-matnr
                                             atinn = lv_zmatdet.
    IF sy-subrc = 0.
      "材质明细
      gs_alv-zmatdet = ls_ausp-atwrt.
      CLEAR ls_ausp.
    ENDIF.
    READ TABLE lt_ausp INTO ls_ausp WITH KEY objek = gs_all2-matnr
                                             atinn = lv_zsamping.
    IF sy-subrc = 0.
      "QA取样量
      gs_alv-zsamping = ls_ausp-atwrt.
      CLEAR ls_ausp.
    ENDIF.





















    APPEND gs_alv TO gt_alv.
    CLEAR:gs_all2,gs_alv.
  ENDLOOP.



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
  fieldcatset   'PRO_NAME'    '生厂商名称' sy-tabix.
  fieldcatset   'STRAS2'    '生产厂家地址'  sy-tabix.
  fieldcatset   'PRO_REGIOGROUP'    '生厂商等级' sy-tabix.
  fieldcatset   'DATE3'   '生厂商评估合格日期' sy-tabix.
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


  READ TABLE fieldcat INDEX 3."读取报表第三列
  fieldcat-key = 'X'."定义为主键（颜色改变）
  MODIFY fieldcat INDEX 3."修改样式

  READ TABLE fieldcat INDEX 4.
  fieldcat-emphasize = 'C500'.
  MODIFY fieldcat INDEX 4.

  READ TABLE fieldcat INDEX 1.
  fieldcat-emphasize = 'C600'.
  MODIFY fieldcat INDEX 1.

  READ TABLE fieldcat INDEX 2.
  fieldcat-emphasize = 'C300'.
  MODIFY fieldcat INDEX 2.

  READ TABLE fieldcat INDEX 5.
  fieldcat-emphasize = 'C600'.
  MODIFY fieldcat INDEX 5.

  READ TABLE fieldcat INDEX 6.
  fieldcat-emphasize = 'C600'.
  MODIFY fieldcat INDEX 6.

  READ TABLE fieldcat INDEX 7.
  fieldcat-emphasize = 'C100'.
  MODIFY fieldcat INDEX 7.

  READ TABLE fieldcat INDEX 8.
  fieldcat-emphasize = 'C200'.
  MODIFY fieldcat INDEX 8.

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
      i_grid_title             = '会计凭证' "标题名
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
      t_outtab                 = lbkpf
    EXCEPTIONS "下面都是默认的
      program_error            = 1
      OTHERS                   = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.