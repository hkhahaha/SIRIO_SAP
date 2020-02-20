*&---------------------------------------------------------------------*
*& Report ZSDR0022
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zsdr0022.
TYPE-POOLS:slis."调用系统存在的类型池
*在调用ALV之前，需要先定义Layout和Fieldcat，他们属于slis类型池
DATA:fieldcat TYPE slis_t_fieldcat_alv WITH HEADER LINE,
     layout   TYPE slis_layout_alv,
     w_repid  TYPE sy-repid. "记录系统当前的程序名
TABLES:vbak,zsdt0019,but000,vbpa,knvp,makt,vbap.
TYPES:BEGIN OF ty_alv,
        objnr TYPE vbak-objnr, "对象编号
        matnr TYPE vbap-matnr,
        umvkz TYPE vbap-umvkz,
        umvkn TYPE vbap-umvkn,
        knumv TYPE vbak-knumv,
        mwsbp TYPE vbap-mwsbp,
        ernam TYPE vbak-ernam,
        werks TYPE vbap-werks,
        objek TYPE ausp-objek.
        INCLUDE TYPE zsdt0019b.
      TYPES:END OF ty_alv.
TYPES:BEGIN OF ty_val,
        objek TYPE ausp-objek,
      END OF ty_val.

DATA:ls_zsdt0019  TYPE zsdt0019,
     lt_zsdt0019  TYPE TABLE OF zsdt0019, "自定义表1
     lt_zsdt00192 TYPE TABLE OF zsdt0019, "自定义表1
     lt_zsdt0019b TYPE TABLE OF zsdt0019b,
     ls_zsdt0019b TYPE zsdt0019b,
     lt_knvp      TYPE TABLE OF knvp,
     ls_knvp      TYPE knvp,
     lt_alv       TYPE TABLE OF ty_alv,
     ls_alv       TYPE ty_alv,
     lt_jest      TYPE TABLE OF jest,
     ls_jest      TYPE jest,
     lt_jcds      TYPE TABLE OF jcds,
     ls_jcds      TYPE jcds,
     lt_but000    TYPE TABLE OF but000,
     ls_but000    TYPE but000,
     lt_vbpa      TYPE TABLE OF vbpa,
     ls_vbpa      TYPE vbpa,
     lt_tvakt     TYPE TABLE OF tvakt,
     ls_tvakt     TYPE tvakt,
     lt_val       TYPE TABLE OF ty_val,
     ls_val       TYPE ty_val.

SELECTION-SCREEN BEGIN OF BLOCK blk WITH FRAME TITLE TEXT-001."定义屏幕
SELECT-OPTIONS:s_auart FOR vbak-auart,"订单类型
               s_erdat FOR vbak-erdat,"创建日期"for前面的是字段显示的名称，可以修改，但是注意不要超过8个字符，否则会报错
               s_udate FOR vbak-erdat,"二审日期
               s_job FOR zsdt0019-job."人员职位

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(12) TEXT-002.
PARAMETERS: p_flag AS CHECKBOX DEFAULT 'X'."是否写入后台表
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK blk.



START-OF-SELECTION.
*调用子程序
  PERFORM getdata.
  PERFORM catalog.
  PERFORM alvshow.
*定义子程序
FORM getdata."查询语句
  DATA:ls_s_udate LIKE LINE OF s_udate,
       lv_zpspe   TYPE ausp-atinn.
  "将特性的名称转换成为内码值
  CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
    EXPORTING
      input  = 'ZPSPE'
    IMPORTING
      output = lv_zpspe.


  "首先，先查询所有的业务员
  SELECT
    *
  INTO CORRESPONDING FIELDS OF TABLE @lt_zsdt0019
  FROM zsdt0019
  WHERE zsdt0019~job IN @s_job
    AND zsdt0019~active = 'X'.
  "然后筛选出LT_ZSDT0019里面的区域经理的账号到lt_zsdt_00192
  SELECT
    *
  FROM @lt_zsdt0019 AS a
  WHERE a~job = '02'
  INTO CORRESPONDING FIELDS OF TABLE @lt_zsdt00192.
  "然后重新把区域经理所属的大区重新查询一边，然后append到内表lt_zsdt0019里面
  IF lt_zsdt00192 IS NOT INITIAL.
    SELECT
      *
    APPENDING CORRESPONDING FIELDS OF TABLE lt_zsdt0019
    FROM zsdt0019
    FOR ALL ENTRIES IN lt_zsdt00192
    WHERE zsdt0019~zdomain = lt_zsdt00192-zdomain
      AND zsdt0019~active = 'X'.
  ENDIF.
  "然后对数据进行筛选，去掉重复的数据
  SORT lt_zsdt0019 BY partner.
  DELETE ADJACENT DUPLICATES FROM lt_zsdt0019 COMPARING ALL FIELDS.
  "上面已经完成了对自定表1的数据获取的操作，接下来根据查到的业务员到VBAK表中获取相应的数据

  "根据上面取到的业务员，到KUNN2取得所需要的字段数据
  IF lt_zsdt0019 IS NOT INITIAL.
    SELECT
    kunnr
    vkorg
    vtweg
    spart
    parvw
    kunn2
    INTO CORRESPONDING FIELDS OF TABLE lt_knvp
    FROM knvp
    FOR ALL ENTRIES IN lt_zsdt0019
    WHERE knvp~kunn2  = lt_zsdt0019-partner
      AND knvp~parvw = 'Z3'.
  ELSE.
    EXIT.
  ENDIF.

  "上面已经取到了全部的业务员，接下来关联VBAK、VBAP表进行查询
  SELECT
    auart
    vbak~erdat
    vbak~objnr
    vbak~erdat
    vbak~kunnr
    vbak~vkorg
    vbak~vtweg
    vbak~spart
    vbak~vbeln
    vbap~posnr
    vbak~vbeln
    vbap~matwa
    vbap~posnr
    vbap~matnr
    vbap~kwmeng AS kwmeng2
    vbap~umvkz
    vbap~umvkn
    vbap~meins
    vbap~vrkme
    vbap~znbjhq
    vbap~zatpdfrq
    vbap~netwr
    vbap~mwsbp
    vbak~ernam
    werks
  INTO CORRESPONDING FIELDS OF TABLE lt_alv
  FROM vbap
  INNER JOIN vbak
  ON vbap~vbeln = vbak~vbeln
  FOR ALL ENTRIES IN lt_knvp
  WHERE vbak~auart IN s_auart
    AND vbak~erdat IN s_erdat
    AND vbak~kunnr = lt_knvp-kunnr
    AND vbak~vkorg = lt_knvp-vkorg
    AND vbak~vtweg = lt_knvp-vtweg
    AND vbak~spart = lt_knvp-spart.


  "下面查询的是来自于其他表的数据
  IF lt_alv IS NOT INITIAL.
    "Jest表
    SELECT
      objnr
      stat
      inact
      stat
    INTO CORRESPONDING FIELDS OF TABLE lt_jest
    FROM jest
    FOR ALL ENTRIES IN lt_alv
    WHERE jest~stat LIKE 'E%'
      AND jest~inact = ''
      AND jest~objnr = lt_alv-objnr.
    "JCDS表
    SELECT
      objnr,
      stat,
      udate,
      chgnr
    FROM jcds
    INTO CORRESPONDING FIELDS OF TABLE @lt_jcds
    FOR ALL ENTRIES IN @lt_alv
    WHERE jcds~stat = 'E0003'
      AND jcds~objnr = @lt_alv-objnr.
    SORT lt_jcds BY objnr stat chgnr.
    "BUT000
    SELECT
      partner
      bu_sort1
      name_org1
    INTO CORRESPONDING FIELDS OF TABLE lt_but000
    FROM but000
    FOR ALL ENTRIES IN lt_alv
    WHERE but000~partner = lt_alv-kunnr.
    "VBPA表
    SELECT
      vbeln
      posnr
      parvw
      kunnr
    INTO CORRESPONDING FIELDS OF TABLE lt_vbpa
    FROM vbpa
    FOR ALL ENTRIES IN lt_alv
    WHERE vbpa~parvw = 'Z1'
      AND vbpa~posnr = ''
      AND vbpa~vbeln = lt_alv-vbeln.
    "TVAKT表
    SELECT
      *
    INTO CORRESPONDING FIELDS OF TABLE lt_tvakt
    FROM tvakt
    FOR ALL ENTRIES IN lt_alv
    WHERE tvakt~spras = '1'
      AND tvakt~auart = lt_alv-auart.

    "T052U表
    SELECT
      text1,
      vbeln,
      posnr
    INTO TABLE @DATA(lt_t052u)
    FROM t052u
    INNER JOIN vbkd
    ON vbkd~zterm = t052u~zterm
    FOR ALL ENTRIES IN @lt_alv
    WHERE t052u~spras = '1'
      AND vbkd~posnr = @lt_alv-posnr
      AND vbkd~vbeln = @lt_alv-vbeln.

    "MAKT
    SELECT
      matnr,
      maktx
    INTO TABLE @DATA(lt_makt)
    FROM makt
    FOR ALL ENTRIES IN @lt_alv
    WHERE makt~spras = '1'
      AND makt~matnr = @lt_alv-matnr.

    "MARA表
    SELECT
      matnr,
      groes,
      matkl
    INTO TABLE @DATA(lt_mara)
    FROM mara
    FOR ALL ENTRIES IN @lt_alv
    WHERE mara~matnr = @lt_alv-matnr.

    "对物料号进行处理
    LOOP AT lt_alv INTO ls_alv.
      ls_alv-objek = ls_alv-matnr.
      MODIFY TABLE lt_alv FROM ls_alv.
      CLEAR ls_alv.
    ENDLOOP.
    "上面处理物料号之后，查询对应的全部AUSP的数据
    SELECT
      objek,
      atwrt
    INTO TABLE @DATA(lt_ausp)
    FROM ausp
    FOR ALL ENTRIES IN @lt_alv
    WHERE ausp~objek = @lt_alv-objek.
    "获取文本表的数据
    SELECT
      *
    INTO TABLE @DATA(lt_t006a)
    FROM t006a.
    "PRCD_ELEMENTS表
    SELECT
      knumv,
      kschl,
      kinak,
      kbetr,
      kpein
    INTO TABLE @DATA(lt_prcd_elements)
    FROM prcd_elements
    FOR ALL ENTRIES IN @lt_alv
    WHERE prcd_elements~knumv = @lt_alv-knumv
      AND kschl IN ('ZP00','ZP02')
      AND kinak = ''.
    "VBEP表
    SELECT
      vbeln,
      posnr,
      ettyp
    INTO TABLE @DATA(lt_vbep)
    FROM vbep
    FOR ALL ENTRIES IN @lt_alv
    WHERE vbep~vbeln = @lt_alv-vbeln
      AND vbep~posnr = @lt_alv-posnr.
    "VBFA表
    SELECT
      vbelv,
      posnv,
      vbtyp_n,
      rfmng
    INTO TABLE @DATA(lt_vbfa_all)
    FROM vbfa
    FOR ALL ENTRIES IN @lt_alv
    WHERE vbfa~vbelv = @lt_alv-vbeln
      AND vbfa~posnv = @lt_alv-posnr
      AND vbfa~vbtyp_n IN ('R','h').
    "保存类型为R的
    SELECT
    vbelv,
    posnv,
    vbtyp_n,
    SUM( rfmng ) AS rfmeng
    FROM @lt_vbfa_all AS a
    GROUP BY vbelv,posnv,vbtyp_n
    INTO TABLE @DATA(lt_vbfa_r).
    "保存类型为h的
    SELECT
    vbelv,
    posnv,
    vbtyp_n,
    SUM( rfmng ) AS rfmeng
    FROM @lt_vbfa_all AS a
    GROUP BY vbelv,posnv,vbtyp_n
    INTO TABLE @DATA(lt_vbfa_h).
    "ADRP表
    SELECT
      bname,
      usr21~persnumber,
      name_text
    INTO TABLE @DATA(lt_adrp)
    FROM adrp
    INNER JOIN usr21
    ON adrp~persnumber = usr21~persnumber
    FOR ALL ENTRIES IN @lt_alv
    WHERE usr21~bname = @lt_alv-ernam.
    "MSKA表
    SELECT
      vbeln,
      posnr,
      matnr,
      werks,
      lgort,
      kalab
    INTO TABLE @DATA(lt_mska)
    FROM mska
    FOR ALL ENTRIES IN @lt_alv
    WHERE mska~vbeln = @lt_alv-vbeln
      AND mska~posnr = @lt_alv-posnr
      AND mska~werks = @lt_alv-werks
      AND mska~lgort IN ('1040','2040','3040','1070','2070','3070').

    "T023T表
    SELECT
      matkl,
      wgbez
    INTO TABLE @DATA(lt_t023t)
    FROM t023t.




  ENDIF.

  "继续添加BUT000
  IF lt_knvp IS NOT INITIAL.
    SELECT
    partner
    bu_sort1
    name_org1
    APPENDING CORRESPONDING FIELDS OF TABLE lt_but000
    FROM but000
    FOR ALL ENTRIES IN lt_knvp
    WHERE but000~partner = lt_knvp-kunn2.
  ENDIF.





  "下面开始循环读取数据
  LOOP AT lt_alv INTO ls_alv.
    MOVE-CORRESPONDING ls_alv TO ls_zsdt0019b.
    "订单状态
    READ TABLE lt_jest INTO ls_jest WITH KEY objnr = ls_alv-objnr.
    IF sy-subrc = 0.
      ls_alv-stat = ls_jest-stat.
    ENDIF.
    CLEAR ls_jest.
    "订单状态描述、
    IF ls_alv-stat = 'E0002'.
      ls_alv-statdetail = '业务一审'.
    ELSEIF ls_alv-stat = 'E0003'.
      ls_alv-statdetail = '业务二审'.
    ENDIF.
    "二审日期
    IF ls_alv-stat = 'E0003'.
      READ TABLE lt_jcds INTO ls_jcds WITH KEY objnr = ls_alv-objnr.
      IF sy-subrc = 0.
        ls_alv-udate = ls_jcds-udate.
      ENDIF.
    ENDIF.
    CLEAR ls_jcds.
*    "要查二审，但是二审差不到，这些数据不要
*    IF ls_alv-udate IS not INITIAL AND s_udate IS NOT INITIAL.
*      CONTINUE.
*    ENDIF.
    "判断选择屏幕中的二审日期是否为空，如果为空的话，证明是查全部，如果不为空的话，证明只查询二审
    IF s_udate IS NOT INITIAL.
      READ TABLE s_udate INTO ls_s_udate WITH KEY low = ''.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.
    ENDIF.
    CLEAR ls_s_udate.
    "检索项
    READ TABLE lt_but000 INTO ls_but000 WITH KEY partner = ls_alv-kunnr.
    IF sy-subrc = 0.
      ls_alv-bu_sort1 = ls_but000-bu_sort1.
    ENDIF.
    "实际客户
    CLEAR ls_but000.
    READ TABLE lt_vbpa INTO ls_vbpa WITH KEY vbeln = ls_alv-vbeln.
    IF sy-subrc = 0.
      ls_alv-kunnr2 = ls_vbpa-kunnr.
    ENDIF.
    CLEAR ls_vbpa.
    "业务员
    READ TABLE lt_knvp INTO ls_knvp WITH KEY kunnr = ls_alv-kunnr
                                             vkorg = ls_alv-vkorg
                                             vtweg = ls_alv-vtweg
                                             spart = ls_alv-spart.
    IF sy-subrc = 0.
      ls_alv-kunn2 = ls_alv-kunn2.
    ENDIF.
    CLEAR ls_knvp.
    "业务员名称
    READ TABLE lt_but000 INTO ls_but000 WITH KEY partner = ls_alv-kunn2.
    IF sy-subrc = 0.
      ls_alv-name_org1 = ls_but000-name_org1.
    ENDIF.
    CLEAR ls_but000.
    "订单类型描述
    READ TABLE lt_tvakt INTO ls_tvakt WITH KEY auart = ls_alv-auart.
    IF sy-subrc = 0.
      ls_alv-bezei = ls_tvakt-bezei.
    ENDIF.
    CLEAR ls_tvakt.
    "付款方式
    READ TABLE lt_t052u INTO DATA(ls_t052u) WITH KEY vbeln = ls_alv-vbeln
                                                     posnr = ls_alv-posnr.
    IF sy-subrc = 0.
      ls_alv-text1 = ls_t052u-text1.
    ENDIF.
    CLEAR ls_t052u.
    "产品名称
    READ TABLE lt_makt INTO DATA(ls_makt) WITH KEY matnr = ls_alv-matnr.
    IF sy-subrc = 0.
      ls_alv-maktx = ls_makt-maktx.
    ENDIF.
    CLEAR ls_makt.
    "规格型号
    READ TABLE lt_mara INTO DATA(ls_mara) WITH KEY matnr = ls_alv-matnr.
    IF sy-subrc = 0.
      ls_alv-groes = ls_mara-groes.
      DATA(lv_matkl) = ls_mara-matkl+0(4).
    ENDIF.
    CLEAR ls_mara.
    "包装规格
    READ TABLE lt_ausp INTO DATA(ls_ausp) WITH KEY objek = ls_alv-objek.
    IF sy-subrc = 0.
      ls_alv-atwrt = ls_ausp-atwrt.
    ENDIF.
    CLEAR ls_ausp.
    "基本计量数量
    IF ls_alv-umvkn IS NOT INITIAL.
      ls_alv-kwmeng = ls_alv-kwmeng2 * ls_alv-umvkz / ls_alv-umvkn.
    ENDIF.
    "基本计量单位、销售单位
    READ TABLE lt_t006a INTO DATA(ls_t006a) WITH KEY msehi = ls_alv-meins.
    IF sy-subrc = 0.
      ls_alv-meins = ls_t006a-msehl.
    ENDIF.
    CLEAR ls_t006a.
    READ TABLE lt_t006a INTO ls_t006a WITH KEY msehi = ls_alv-vrkme.
    IF sy-subrc = 0.
      ls_alv-vrkme = ls_t006a-msehl.
    ENDIF.
    CLEAR ls_t006a.
    "价税合计
    ls_alv-netwr = ls_alv-netwr + ls_alv-mwsbp.
    "折前单价
    READ TABLE lt_prcd_elements INTO DATA(ls_prcd_elements) WITH KEY knumv = ls_alv-knumv.
    IF sy-subrc = 0.
      ls_alv-kbetr = ls_prcd_elements-kbetr / ls_prcd_elements-kpein.
    ENDIF.
    CLEAR ls_prcd_elements.
    "计划行类型
    READ TABLE lt_vbep INTO DATA(ls_vbep) WITH KEY vbeln = ls_alv-vbeln
                                                   posnr = ls_alv-posnr.
    IF sy-subrc = 0.
      ls_alv-ettyp = ls_vbep-ettyp.
    ENDIF.
    CLEAR ls_vbep.
    "出库量
    READ TABLE lt_vbfa_r INTO DATA(ls_vbfa_r) WITH KEY vbelv = ls_alv-vbeln
                                                       posnv = ls_alv-posnr
                                                       vbtyp_n = 'R'.
    IF sy-subrc = 0.
      READ TABLE lt_vbfa_h INTO DATA(ls_vbfa_h) WITH KEY vbelv = ls_alv-vbeln
                                                         posnv = ls_alv-posnr
                                                         vbtyp_n = 'h'.
      IF sy-subrc = 0.
        ls_alv-rfmng = ls_vbfa_r-rfmeng - ls_vbfa_h-rfmeng.
      ENDIF.
    ENDIF.
    CLEAR:ls_vbfa_r,ls_vbfa_h.
    "制单人
    READ TABLE lt_adrp INTO DATA(ls_adrp) WITH KEY bname = ls_alv-ernam.
    IF sy-subrc = 0.
      ls_alv-name_text = ls_alv-name_text.
    ENDIF.
    CLEAR ls_adrp.
    "非限制库存
    READ TABLE lt_mska INTO DATA(ls_mska) WITH KEY matnr = ls_alv-matnr
                                                   posnr = ls_alv-posnr
                                                   vbeln = ls_alv-vbeln
                                                   werks = ls_alv-werks.
    IF sy-subrc = 0.
      ls_alv-kalab = ls_mska-kalab.
    ENDIF.
    IF lv_matkl IS NOT INITIAL.
      READ TABLE lt_t023t INTO DATA(ls_t023t) WITH KEY matkl = lv_matkl.
      IF sy-subrc = 0.
        ls_alv-wgbez = ls_t023t-wgbez.
      ENDIF.
      CLEAR ls_t023t.
    ENDIF.



    MOVE-CORRESPONDING ls_alv TO ls_zsdt0019b.
    APPEND ls_zsdt0019b TO lt_zsdt0019b.
    CLEAR:ls_alv,ls_zsdt0019b.
  ENDLOOP.










  "如果标识为需要写入后台表的话，就先清空自定义表里面的数据
  IF p_flag = 'X'.
    DELETE FROM zsdt0019B.
  ENDIF.

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


  fieldcatset 'BUKRS' '公司代码' sy-tabix.
  fieldcatset 'gjahr' '会计年度' sy-tabix.
  fieldcatset 'BUKRS' '会计期间' sy-tabix.
  fieldcatset 'BELNR' '凭证编号' sy-tabix.
  fieldcatset 'BLART' '凭证类型' sy-tabix.
  fieldcatset 'BLDAT' '凭证日期' sy-tabix.
  fieldcatset 'BUDAT' '过账日期' sy-tabix.
  fieldcatset 'XSTOV' '冲销标识' sy-tabix.
  fieldcatset 'STBLG' '冲销凭证' sy-tabix.
  fieldcatset 'PPNAM' '制单人' sy-tabix.
  fieldcatset 'USNAM' '过账人' sy-tabix.

  READ TABLE fieldcat INDEX 3."读取报表第三列
  fieldcat-hotspot = 'X'."鼠标热点事件
  fieldcat-key = 'X'."定义为主键（颜色改变）
  MODIFY fieldcat INDEX 3."修改样式

  READ TABLE fieldcat INDEX 4.
  fieldcat-hotspot = 'X'.
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
  READ TABLE fieldcat INDEX 9.
  fieldcat-checkbox = 'X'.
  MODIFY fieldcat INDEX 9.

*layout-colwidth_optimize = 'X'.
  layout-zebra = 'X'."斑马线的样式

ENDFORM.
*响应鼠标点击时间的子程序
FORM alv_user_command USING r_ucomm LIKE sy-ucomm
      rs_selfield TYPE slis_selfield.
  CASE r_ucomm.
    WHEN '&IC1'."默认的值就是这个，网上查资料据说能修改~
*      READ TABLE lbkpf INDEX rs_selfield-tabindex.
*      SET PARAMETER ID 'BLN' FIELD lbkpf-belnr.
*      CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
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
*     I_CALLBACK_USER_COMMAND  = 'ALV_USER_COMMAND'"对ALV操作的时候触发所定义的子程序
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
      t_outtab                 = lt_zsdt0019
    EXCEPTIONS "下面都是默认的
      program_error            = 1
      OTHERS                   = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.