*&---------------------------------------------------------------------*
*& Report ZFIR0031
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfir0031.
TYPE-POOLS:slis."调用系统存在的类型池
*在调用ALV之前，需要先定义Layout和Fieldcat，他们属于slis类型池
DATA:fieldcat TYPE slis_t_fieldcat_alv WITH HEADER LINE,
     layout   TYPE slis_layout_alv,
     w_repid  TYPE sy-repid. "记录系统当前的程序名
TABLES:anla,anlb,anlz,ankt,bseg,anlc,anlp,t006,t087t,t001.
TYPES:BEGIN OF ty_alv,
        bukrs TYPE anla-bukrs, "公司代码
        anln1 TYPE anla-anln1, "资产号
        txt50 TYPE anla-txt50, "资产名称
        invnr TYPE anla-invnr, "旧资产代码
        anlkl TYPE anla-anlkl, "类别代码
        txk20 TYPE ankt-txk20, "类别名称
        "型号
        msehi TYPE t006-msehi, "单位
        menge TYPE anla-menge, "数量
        ordtx TYPE t087t-ordtx, "增加方式
        aktiv TYPE anla-aktiv, "资本化日期
        waers TYPE t001-waers, "币别
        "汇率（原币除以本币）
        wrbtr TYPE bseg-wrbtr, "原值原币
        dmbtr TYPE bseg-dmbtr, "原值本币
        knafa TYPE anlc-knafa, "累计折旧额
        "净值
        kaufw TYPE anlc-kaufw, "减值准备
        "净额
        nafaz TYPE anlp-nafaz, "本期折旧额
        nafag TYPE anlc-nafag, "本年累计折旧额
        "预计净残值
        ndjar TYPE anlb-ndjar, "使用年限
        ndabj TYPE anlc-ndabj, "剩余年限
        kostl TYPE anlz-kostl, "成本中心代码
        ktext TYPE cskt-ktext, "成本中心描述
        "存放地点
        afasl TYPE anlb-afasl, "折旧码
        ord41 TYPE anla-ord41, "使用情况
        lifnr TYPE anla-lifnr, "供应商
        "进项税额
        "文本
      END OF ty_alv.
TYPES:BEGIN OF ty_all,
        meins TYPE anla-meins,
        ord42 TYPE anla-ord42,
        zugdt TYPE anla-zugdt.
        INCLUDE TYPE ty_alv.
      TYPES:END OF ty_all.

DATA:gt_alv   TYPE TABLE OF ty_alv,
     gs_alv   LIKE LINE OF gt_alv,
     gt_all   TYPE TABLE OF ty_all,
     gs_all   LIKE LINE OF gt_all,
     lt_ankt  TYPE TABLE OF ankt,
     ls_ankt  TYPE ankt,
     lt_t006a TYPE TABLE OF t006a,
     ls_t006a TYPE t006a,
     lt_t087t TYPE TABLE OF t087t,
     ls_t087t TYPE t087t,
     lt_t001  TYPE TABLE OF t001,
     ls_t001  TYPE t001,
     lt_bseg  TYPE TABLE OF bseg,
     ls_bseg  TYPE bseg,
     lt_anlc  TYPE TABLE OF anlc,
     ls_anlc  TYPE anlc,
     lt_anlp  TYPE TABLE OF anlp,
     ls_anlp  TYPE anlp.




SELECTION-SCREEN BEGIN OF BLOCK blk WITH FRAME TITLE TEXT-001."定义屏幕
SELECT-OPTIONS:s_bukrs FOR anla-bukrs NO INTERVALS,"条件输入框，一个for对应一个\公司代码
  s_anln1 FOR anla-anln1,"for前面的是字段显示的名称，可以修改，但是注意不要超过8个字符，否则会报错/主资产号
  s_invnr FOR anla-invnr,"旧资产号
  s_anlkl FOR anla-anlkl,"资产类型
  s_kostl FOR anlz-kostl,"成本中心
  s_zugdt FOR anla-zugdt,"时间
  s_txa50 FOR anla-aktiv,"保管人
  s_aktiv FOR anla-aktiv,"资产化日期
  s_ndjar FOR anlb-ndjar,"折旧年限
  s_afasl FOR anlb-afasl NO INTERVALS."残值率
SELECTION-SCREEN END OF BLOCK blk.

START-OF-SELECTION.
*调用子程序
  PERFORM getdata.
  PERFORM catalog.
  PERFORM alvshow.
*定义子程序
FORM getdata."查询语句
  "关联ANLA、ANLB、ANLZ表的数据
  SELECT
    "anla表里面的字段
    anla~bukrs
    anla~anln1
    anla~txt50
    anla~invnr
    anla~anlkl
    anla~meins
    anla~menge
    anla~ord42
    anla~aktiv
    anla~zugdt
    anla~ord41
    anla~lifnr
    anla~meins
    "anlb表里面的字段
    anlb~ndjar
    anlb~afasl
    "anlz表里面的字段
    anlz~kostl
  INTO CORRESPONDING FIELDS OF TABLE gt_all
  FROM anlb
  INNER JOIN anla
  ON anla~bukrs = anlb~bukrs
  AND anla~anln1 = anlb~anln1
  INNER JOIN anlz
  ON anlz~bukrs = anla~bukrs
  AND anlz~anln1 = anla~anln1
  WHERE anla~bukrs IN s_bukrs
    AND anla~anln1 IN s_anln1
    AND anla~invnr IN s_invnr
    AND anla~anlkl IN s_anlkl
    AND anla~zugdt IN s_zugdt
    AND anla~txa50 IN s_txa50
    AND anla~aktiv IN s_aktiv
    AND anlb~ndjar IN s_ndjar
    AND anlb~afasl IN s_afasl
    AND anlb~bukrs IN s_bukrs
    AND anlb~anln1 IN s_anln1
    AND anlz~kostl IN s_kostl
    AND anlb~afabe = '1'.

  CHECK gt_all IS NOT INITIAL.
  "下面对其他表的内容进行操作
  SELECT
    anlkl
    txk20
  INTO CORRESPONDING FIELDS OF TABLE lt_ankt
  FROM ankt
  FOR ALL ENTRIES IN gt_all
  WHERE ankt~spras = '1'
    AND ankt~anlkl = gt_all-anlkl.

  SELECT
    msehi
    mseht
  INTO CORRESPONDING FIELDS OF TABLE lt_t006a
  FROM t006a
  FOR ALL ENTRIES IN gt_all
  WHERE t006a~msehi = gt_all-meins.

  SELECT
    ord4x
    ordtx
  INTO CORRESPONDING FIELDS OF TABLE lt_t087t
  FROM t087t
  FOR ALL ENTRIES IN gt_all
  WHERE t087t~spras = '1'
    AND t087t~ordnr = '2'
    AND t087t~ord4x = gt_all-ord42.

  SELECT
    waers
    bukrs
  INTO CORRESPONDING FIELDS OF TABLE lt_t001
  FROM t001
  FOR ALL ENTRIES IN gt_all
  WHERE t001~bukrs = gt_all-bukrs.

  SELECT
    wrbtr
    dmbtr
    anln1
    anbwa
    shkzg
  INTO CORRESPONDING FIELDS OF TABLE lt_bseg
  FROM bseg
  FOR ALL ENTRIES IN gt_all
  WHERE bseg~anln1 = gt_all-anln1
    AND bseg~anbwa = '100'
    AND bseg~shkzg = 'S'.



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
*     i_callback_user_command  = 'ALV_USER_COMMAND' "对ALV操作的时候触发所定义的子程序
*     I_CALLBACK_TOP_OF_PAGE   = ' '
*     I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*     I_CALLBACK_HTML_END_OF_LIST       = ' '
*     I_STRUCTURE_NAME         =
*     I_BACKGROUND_ID          = ' '
      i_grid_title             = '固定资产清单及折旧表' "标题名
*     I_GRID_SETTINGS          =
      is_layout                = layout "程序所定义的layout名称
      it_fieldcat              = fieldcat[] "定义fieldcat数据
*     IT_EXCLUDING             =
*     IT_SPECIAL_GROUPS        =
*     IT_SORT                  =
*     IT_FILTER                =
*     IS_SEL_HIDE              =
      i_default                = 'X'
      i_save                   = 'A'
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