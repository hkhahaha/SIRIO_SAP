*&---------------------------------------------------------------------*
*& Report ZQMR0014
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zqmr0014.
*&---------------------------------------------------------------------*
*& 程序名：ZQMR0014
*&作者：Seashell Huang
*&模块：QM
*&创建日期：14.10.2019 09:43:27
*&功能描述： 供应商待审核COA报表
*&---------------------------------------------------------------------*
TABLES:qals,mara.
TYPE-POOLS:slis."调用系统存在的类型池
*在调用ALV之前，需要先定义Layout和Fieldcat，他们属于slis类型池
DATA:fieldcat TYPE slis_t_fieldcat_alv WITH HEADER LINE,
     layout   TYPE slis_layout_alv,
     w_repid  TYPE sy-repid. "记录系统当前的程序名
DATA gv_sel_error TYPE flag.
*定义ALV内表数据
TYPES:BEGIN OF ty_out.
TYPES:matnr   TYPE qals-matnr, "物料编码
      maktx   TYPE makt-maktx, "物料描述
      charg   TYPE qals-charg, "SAP批次号
      zchargl TYPE string, "长批号
      zlicha  TYPE string, "供应商批号
      lifnr   TYPE string, "供应商
      zoriadd TYPE string, "产地
      zflag   TYPE string, "有无报告书
      zflag2  TYPE string, "是否供应商批次非首次
      zflag3  TYPE string, "是否已审核
      z_name  TYPE string. "归档命名规则
TYPES END OF ty_out.

*定义接收主要数据的内表结构
TYPES:BEGIN OF ty_all,
        matnr     TYPE qals-matnr,
        maktx     TYPE makt-maktx,
        werk      TYPE qals-werk,
        enstehdat TYPE qals-enstehdat,
        mtart     TYPE mara-mtart,
        art       TYPE qals-art,
        charg     TYPE qals-charg,
        lifnr     TYPE string,
        zchargl   TYPE string, "长批号
        zlicha    TYPE string, "供应商批号
        zlicha2   TYPE string, "供应商批号
*  LIFNR TYPE QALS-lifnr,"供应商
        zoriadd   TYPE string, "产地
        zflag     TYPE string, "有无报告书
        zflag2    TYPE string, "是否供应商批次非首次
        zflag3    TYPE string, "是否已审核
        z_name    TYPE string, "归档命名规则
        mblnr     TYPE qals-mblnr, "物料凭证
        mjahr     TYPE qals-mjahr, "物料凭证年度
        zeile     TYPE qals-zeile, "物料凭证项目
        prueflos  TYPE qals-prueflos, "检验批次
      END OF ty_all.
TYPES:BEGIN OF zmseg,
        charg  TYPE mseg-charg,
        zlicha TYPE string,
      END OF zmseg.
DATA:lt_qals  TYPE TABLE OF qals,
     ls_qals  TYPE qals,
     lt_makt  TYPE TABLE OF makt,
     ls_makt  TYPE makt,
     lt_mara  TYPE TABLE OF mara,
     ls_mara  TYPE mara,
     lt_out   TYPE TABLE OF ty_out,
     ls_out   TYPE ty_out,
     lt_all   TYPE TABLE OF ty_all,
     ls_all   TYPE ty_all,
     lt_lfa1  TYPE TABLE OF lfa1,
     ls_lfa1  TYPE lfa1,
     lt_mseg  TYPE TABLE OF mseg,
     ls_mseg  TYPE mseg,
     lt_mseg2 TYPE TABLE OF zmseg,
     ls_mseg2 TYPE zmseg,
     lt_mcha  TYPE TABLE OF mcha,
     ls_mcha  TYPE mcha,
     ls_mcha2 TYPE mcha.




SELECTION-SCREEN BEGIN OF BLOCK blk WITH FRAME TITLE TEXT-001.."定义屏幕
SELECT-OPTIONS:s_matnr FOR qals-matnr,"物料"条件输入框，一个for对应一个
*s_werk FOR qals-werk,"工厂"for前面的是字段显示的名称，可以修改，但是注意不要超过8个字符，否则会报错
s_enddat FOR qals-enstehdat,"到货日期
s_mtart FOR mara-mtart,"物料类型
s_art FOR qals-art DEFAULT '01',"检验类型，默认01
s_charg FOR qals-charg."SAP批号
PARAMETERS:   p_werks TYPE werks_d OBLIGATORY.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(20) TEXT-002.
SELECTION-SCREEN POSITION 23.
PARAMETERS flag_a RADIOBUTTON GROUP rg1 DEFAULT 'X'.
SELECTION-SCREEN COMMENT 24(8) TEXT-003 FOR FIELD flag_a.
SELECTION-SCREEN POSITION 44.
PARAMETERS flag_b RADIOBUTTON GROUP rg1.
SELECTION-SCREEN COMMENT 48(8) TEXT-004 FOR FIELD flag_b.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK blk.



START-OF-SELECTION.
  PERFORM frm_authority_check.
  CHECK gv_sel_error IS INITIAL."权限检查标识
  PERFORM getdata.
  PERFORM catalog.
  PERFORM alvshow.

END-OF-SELECTION.


FORM getdata.

  DATA:e_objek TYPE  cuobn,
       e_obtab TYPE  tabelle,
       e_klart TYPE  klassenart,
       e_class TYPE  klasse_d.

  DATA: ls_key             TYPE bapi1003_key,
        lv_status          LIKE bapi1003_key-status,
        lt_allocvaluesnum  LIKE TABLE OF bapi1003_alloc_values_num,
        ls_allocvaluesnum  LIKE bapi1003_alloc_values_num,
        lt_allocvalueschar LIKE TABLE OF bapi1003_alloc_values_char,
        ls_allocvalueschar LIKE bapi1003_alloc_values_char,
        lt_allocvaluescurr LIKE TABLE OF bapi1003_alloc_values_curr,
        ls_allocvaluescurr LIKE bapi1003_alloc_values_curr,
        lt_return          LIKE TABLE OF bapiret2,
        ls_return          LIKE bapiret2.
  DATA:input_matnr TYPE matnr,
       input_charg TYPE charg_d,
       input_werks TYPE werks_d,
       material    TYPE bapibatchkey-material,
       output      TYPE TABLE OF zmaterial_tx_value,
       ls_output   LIKE LINE OF output.

*s_matnr 物料
*s_werk 工厂
*s_enddat 到货日期
*s_mtart 物料类型
*s_art 检验类型
*s_charg SAP批号
  SELECT DISTINCT
  qals~matnr AS matnr
  qals~werk AS werk
  qals~enstehdat AS enstehdat
  qals~art AS art
  qals~charg AS charg
  qals~lifnr AS lifnr
  qals~mblnr AS mblnr
  qals~mjahr AS mjahr
  qals~zeile AS zeile
  mara~mtart AS mtart
  INTO CORRESPONDING FIELDS OF TABLE lt_all
  FROM qals
  INNER JOIN mara
  ON mara~matnr = qals~matnr
  WHERE qals~matnr IN s_matnr
  AND qals~werk = p_werks
  AND qals~enstehdat IN s_enddat
  AND qals~art IN s_art
  AND qals~charg IN s_charg
  AND mara~mtart IN s_mtart.
  SORT lt_all BY matnr charg.
  LOOP AT lt_all INTO ls_all.
    input_matnr = ls_all-matnr.
    input_charg = ls_all-charg.
    input_werks = ls_all-werk.

    "物料描述
    SELECT SINGLE
    maktx
    INTO CORRESPONDING FIELDS OF ls_makt
    FROM makt
    WHERE makt~matnr = ls_all-matnr.
*    MOVE-CORRESPONDING ls_makt TO ls_all.
    MOVE ls_makt-maktx TO ls_all-maktx.
    IF input_matnr IS NOT INITIAL AND input_charg IS NOT INITIAL AND input_werks IS NOT INITIAL.
      "长批号zchargl
      input_matnr = zcl_bc_public=>conv_by_ddic( i_input = input_matnr ).
      CALL FUNCTION 'VB_BATCH_2_CLASS_OBJECT'
        EXPORTING
          i_matnr = input_matnr
          i_charg = input_charg
          i_werks = input_werks
        IMPORTING
          e_objek = e_objek
          e_obtab = e_obtab
          e_klart = e_klart
          e_class = e_class.

*  ls_key-object = e_objek.
      ls_key-objecttable = e_obtab.
      ls_key-classnum = e_class.
      ls_key-classtype = e_klart.
      ls_key-object_long = e_objek.



      CALL FUNCTION 'BAPI_OBJCL_GETDETAIL'
        EXPORTING
*         objectkey       = ls_key-object
          objecttable     = ls_key-objecttable
          classnum        = ls_key-classnum
          classtype       = ls_key-classtype
          objectkey_long  = ls_key-object_long
        IMPORTING
          status          = lv_status
*         STANDARDCLASS   =
        TABLES
          allocvaluesnum  = lt_allocvaluesnum
          allocvalueschar = lt_allocvalueschar
          allocvaluescurr = lt_allocvaluescurr
          return          = lt_return.
*      CHECK lv_status IS NOT INITIAL.
      "长批号
      READ TABLE lt_allocvalueschar INTO ls_allocvalueschar WITH KEY charact = 'ZCHARGL'.
      IF sy-subrc = 0.
        ls_all-zchargl = ls_allocvalueschar-value_char_long.
      ENDIF.
      CLEAR ls_allocvalueschar.
      "供应商批号ZLICHA
      READ TABLE lt_allocvalueschar INTO ls_allocvalueschar WITH KEY charact = 'ZLICHA'.
      IF sy-subrc = 0.
        ls_all-zlicha = ls_allocvalueschar-value_char_long.
      ENDIF.
    ENDIF.
    "供应商
    SELECT SINGLE
    name1
    INTO CORRESPONDING FIELDS OF ls_lfa1
    FROM lfa1
    WHERE lfa1~lifnr =  ls_all-lifnr.
    ls_all-lifnr = ls_lfa1-name1.
    "产地
    CALL FUNCTION 'Z_FI_GET_MATERIAL_VALUE'
      EXPORTING
        matnr  = input_matnr
        atnam  = 'ZORIADD'
      TABLES
        output = output.
    READ TABLE output INTO ls_output INDEX 1.
    ls_all-zoriadd = ls_output-atwrt.
    "有无报告书
    SELECT SINGLE
    answer
    INTO CORRESPONDING FIELDS OF ls_mseg
    FROM mseg
    WHERE mseg~mblnr = ls_all-mblnr
    AND mseg~mjahr = ls_all-mjahr
    AND mseg~zeile = ls_all-zeile.
    IF ls_mseg-answer IS INITIAL OR ls_mseg-answer = 'N'.
      ls_all-zflag = 'N'.
    ELSEIF ls_mseg-answer = 'J'.
      ls_all-zflag = 'Y'.
    ENDIF.

    CLEAR ls_mseg.
    "是否供应商批次非首次
*&---------------------------------------------------------------------*
*& 程序名：ZQMR0014
*&作者：Seashell Huang
*&模块：
*&创建日期：30.10.2019 10:44:13
*&功能描述：
*&---------------------------------------------------------------------*
*&修改记录：修改是否供应商批次非首次的取数逻辑：
*    MCHA
*    物料
*    供应商批次
*    创建日期排序
*    最上面是首次到货
*    下面是非首次
*&---------------------------------------------------------------------*
    "判断供应商批次是否为空的，如果是空的，“是否供应商批次非首次”留空
    IF ls_all-zlicha IS INITIAL.
      ls_all-zflag2 = ''.
    ELSE.
      "如果供应商批次不为空的话
      "首先先从MCHA取出所有物料、供应商批次对应的数据，按照创建日期进行排序
      SELECT
        ersda
        charg
      INTO CORRESPONDING FIELDS OF TABLE lt_mcha
      FROM mcha
      WHERE mcha~matnr = ls_all-matnr
        AND mcha~licha = ls_all-zlicha
      ORDER BY ersda charg.
      "然后检查对应的物料、SAP批次和工厂，然后和上面的内表中的第一条数据进行对比，如果相等的话是首次，否则是非首次
      READ TABLE lt_mcha INTO ls_mcha2 INDEX 1.
      SELECT SINGLE
        ersda
        charg
      INTO CORRESPONDING FIELDS OF ls_mcha
      FROM mcha
      WHERE mcha~matnr = ls_all-matnr
        AND mcha~werks = ls_all-werk
        AND mcha~charg = ls_all-charg.
      IF ls_mcha-ersda = ls_mcha2-ersda AND ls_mcha-charg = ls_mcha2-charg.
        ls_all-zflag2 = '首次'.
      ELSE.
        ls_all-zflag2 = '非首次'.
      ENDIF.
    ENDIF.
*    SELECT DISTINCT
*    charg
*    INTO CORRESPONDING FIELDS OF TABLE lt_mseg2
*    FROM mseg
*    WHERE mseg~matnr = ls_all-matnr.

*    LOOP AT lt_mseg2 INTO ls_mseg2.
*      "检查其他批次的批次属性供应商批号
*      CHECK ls_mseg2-charg IS NOT INITIAL.
*      input_charg = ls_mseg2-charg.
*      CALL FUNCTION 'VB_BATCH_2_CLASS_OBJECT'
*        EXPORTING
*          i_matnr = input_matnr
*          i_charg = input_charg
*          i_werks = input_werks
*        IMPORTING
*          e_objek = e_objek
*          e_obtab = e_obtab
*          e_klart = e_klart
*          e_class = e_class.
*
**  ls_key-object = e_objek.
*      ls_key-objecttable = e_obtab.
*      ls_key-classnum = e_class.
*      ls_key-classtype = e_klart.
*      ls_key-object_long = e_objek.
*
*
*      CALL FUNCTION 'BAPI_OBJCL_GETDETAIL'
*        EXPORTING
**         objectkey       = ls_key-object
*          objecttable     = ls_key-objecttable
*          classnum        = ls_key-classnum
*          classtype       = ls_key-classtype
*          objectkey_long  = ls_key-object_long
*        IMPORTING
*          status          = lv_status
**         STANDARDCLASS   =
*        TABLES
*          allocvaluesnum  = lt_allocvaluesnum
*          allocvalueschar = lt_allocvalueschar
*          allocvaluescurr = lt_allocvaluescurr
*          return          = lt_return.
**      CHECK lv_status IS NOT INITIAL.
*      "供应商批号
*      READ TABLE lt_allocvalueschar INTO ls_allocvalueschar WITH KEY charact = 'ZLICHA'.
*      IF sy-subrc = 0.
*        ls_mseg2-zlicha = ls_allocvalueschar-value_char_long.
*        MODIFY TABLE lt_mseg2 FROM ls_mseg2.
*      ENDIF.
*    ENDLOOP.

*    CLEAR output.
*    CLEAR ls_output.
*    CALL FUNCTION 'Z_FI_GET_MATERIAL_VALUE'
*      EXPORTING
*        matnr  = ls_all-matnr
*        atnam  = 'ZLICHA'
*      TABLES
*        output = output.
*    LOOP AT output INTO ls_output.
*      IF ls_output-atwrt = ls_all-zlicha.
*        ls_all-zflag2 = '非首次'.
*      ELSE.
*        ls_all-zflag2 = '首次'.
*      ENDIF.
*    ENDLOOP.
*    IF output IS INITIAL.
*      ls_all-zflag2 = '首次'.
*    ENDIF.


*    READ TABLE lt_mseg2 INTO ls_mseg2 WITH KEY zlicha = ls_all-zlicha.
    "是否已审核
    CLEAR ls_all-zflag3.
    IF ls_all-zflag2 = '非首次'.
      ls_all-zflag3 = '已审核'.
    ELSE.
      ls_all-zflag3 = '待审核'.
    ENDIF.




    "归档命名规则
    ls_all-z_name = |{ ls_all-maktx }&{ ls_all-charg }&{ ls_all-lifnr }&{ ls_all-zlicha }|.
    "物料编码
*    ls_all-matnr = ls_qals-matnr.
    "SAP批次号
*    ls_all-charg = ls_qals-charg.
*    APPEND ls_all TO lt_all.
    MODIFY lt_all FROM ls_all.
    CLEAR ls_all.
  ENDLOOP.

  LOOP AT lt_all INTO ls_all.
    IF flag_a = 'X'.
      IF ls_all-zflag3 = '已审核'.
        MOVE-CORRESPONDING ls_all TO ls_out.
      ENDIF.
    ENDIF.
    IF flag_b = 'X'.
      IF ls_all-zflag3 = '待审核'.
        MOVE-CORRESPONDING ls_all TO ls_out.
      ENDIF.
    ENDIF.
*&---------------------------------------------------------------------*
*& 程序名：ZQMR0014
*&作者：Seashell Huang
*&模块：
*&创建日期：01.11.2019 16:35:15
*&功能描述：
*&---------------------------------------------------------------------*
*&修改记录：去掉前导0
*&
*&---------------------------------------------------------------------*
    ls_out-matnr = zcl_bc_public=>conv_by_ddic( i_input = ls_out-matnr i_out = 'X' ).
    APPEND ls_out TO lt_out.
  ENDLOOP.
  SORT lt_all BY matnr.
  DELETE lt_out WHERE matnr IS INITIAL.
  DELETE ADJACENT DUPLICATES FROM lt_out COMPARING ALL FIELDS.
ENDFORM.

FORM catalog.
  w_repid = sy-repid.
  CLEAR fieldcat.

  DEFINE fieldcatset."宏定义
    fieldcat-just = 'L'."字段居中显示
    fieldcat-outputlen = 12."自定义字段的长度
*    fieldcat-ref_tabname = ''."调用透明表的数据结构
    fieldcat-fieldname = &1."透明表字段名
    fieldcat-seltext_l = &2."ALV列名
    fieldcat-col_pos = &3."列位置
    APPEND fieldcat.
  END-OF-DEFINITION.


  fieldcatset 'MATNR' '物料编码' sy-tabix.
  fieldcatset 'MAKTX' '物料描述' sy-tabix.
  fieldcatset 'CHARG' 'SAP批次号' sy-tabix.
  fieldcatset 'ZCHARGL' '长批号' sy-tabix.
  fieldcatset 'ZLICHA' '供应商批号' sy-tabix.
  fieldcatset 'LIFNR' '供应商' sy-tabix.
  fieldcatset 'ZORIADD' '产地' sy-tabix.
  fieldcatset 'ZFLAG' '有无报告书' sy-tabix.
  fieldcatset 'ZFLAG2' '是否供应商批次非首次' sy-tabix.
  fieldcatset 'ZFLAG3' '是否已审核' sy-tabix.
  fieldcatset 'Z_NAME' '归档命名规则' sy-tabix.


  layout-zebra = 'X'."斑马线的样式
  layout-colwidth_optimize = 'X'."设置grid的字段列宽度自动适应(会覆盖上面的字段自定义长度限制)
ENDFORM.

FORM alvshow.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
*     I_INTERFACE_CHECK  = ' '
*     I_BYPASSING_BUFFER = ' '
*     I_BUFFER_ACTIVE    = ' '
      i_callback_program = w_repid "程序名称
*     i_callback_pf_status_set = 'FRM_SET_PF_STATUS'
*     i_callback_user_command  = 'ALV_USER_COMMAND' "对ALV操作的时候触发所定义的子程序
*     I_CALLBACK_TOP_OF_PAGE   = ' '
*     I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*     I_CALLBACK_HTML_END_OF_LIST       = ' '
*     I_STRUCTURE_NAME   =
*     I_BACKGROUND_ID    = ' '
      i_grid_title       = 'COA报表' "标题名
*     I_GRID_SETTINGS    =
      is_layout          = layout "程序所定义的layout名称
      it_fieldcat        = fieldcat[] "定义fieldcat数据
*     IT_EXCLUDING       =
*     IT_SPECIAL_GROUPS  =
*     IT_SORT            =
*     IT_FILTER          =
*     IS_SEL_HIDE        =
*     I_DEFAULT          = 'X'
*     I_SAVE             = ' '
*     IS_VARIANT         =
*     IT_EVENTS          =
*     IT_EVENT_EXIT      =
*     IS_PRINT           =
*     IS_REPREP_ID       =
*     I_SCREEN_START_COLUMN    = 0
*     I_SCREEN_START_LINE      = 0
*     I_SCREEN_END_COLUMN      = 0
*     I_SCREEN_END_LINE  = 0
*     I_HTML_HEIGHT_TOP  = 0
*     I_HTML_HEIGHT_END  = 0
*     IT_ALV_GRAPHICS    =
*     IT_HYPERLINK       =
*     IT_ADD_FIELDCAT    =
*     IT_EXCEPT_QINFO    =
*     IR_SALV_FULLSCREEN_ADAPTER        =
* IMPORTING
*     E_EXIT_CAUSED_BY_CALLER  =
*     ES_EXIT_CAUSED_BY_USER   =
    TABLES
      t_outtab           = lt_out
    EXCEPTIONS "下面都是默认的
      program_error      = 1
      OTHERS             = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.

FORM frm_authority_check.
*  DATA lv_werks TYPE werks_d.
  AUTHORITY-CHECK OBJECT 'Q_INSPLOT'
*  ID 'QACT_ILOT' FIELD 'xxxxxxxx'
  ID 'WERKS' FIELD p_werks.
*  ID 'QPRUEFART' FIELD 'xxxxxxxx'
*  ID 'QMATAUTH' FIELD 'xxxxxxxx'.
  IF sy-subrc <> 0.
* Implement a suitable exception handling here
    MESSAGE '权限检查失败' TYPE 'E'.
    gv_sel_error = 'X'.
  ELSE.
    MESSAGE '有权限' TYPE 'S'.
    gv_sel_error = ''.
  ENDIF.

ENDFORM.