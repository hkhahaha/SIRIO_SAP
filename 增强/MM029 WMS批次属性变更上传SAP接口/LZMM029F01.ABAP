*----------------------------------------------------------------------*
***INCLUDE LZMM029F01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form FRM_CHK_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*

*               ii.                                         ;9ABH,
*              SA391,                                    .r9GG35&G
*              &#ii13Gh;                               i3X31i;:,rB1
*              iMs,:,i5895,                         .5G91:,:;:s1:8A
*               33::::,,;5G5,                     ,58Si,,:::,sHX;iH1
*                Sr.,:;rs13BBX35hh11511h5Shhh5S3GAXS:.,,::,,1AG3i,GG
*                .G51S511sr;;iiiishS8G89Shsrrsh59S;.,,,,,..5A85Si,h8
*               :SB9s:,............................,,,.,,,SASh53h,1G.
*            .r18S;..,,,,,,,,,,,,,,,,,,,,,,,,,,,,,....,,.1H315199,rX,
*          ;S89s,..,,,,,,,,,,,,,,,,,,,,,,,....,,.......,,,;r1ShS8,;Xi
*        i55s:.........,,,,,,,,,,,,,,,,.,,,......,.....,,....r9&5.:X1
*       59;.....,.     .,,,,,,,,,,,...        .............,..:1;.:&s
*      s8,..;53S5S3s.   .,,,,,,,.,..      i15S5h1:.........,,,..,,:99
*      93.:39s:rSGB@A;  ..,,,,.....    .SG3hhh9G&BGi..,,,,,,,,,,,,.,83
*      G5.G8  9#@@@@@X. .,,,,,,.....  iA9,.S&B###@@Mr...,,,,,,,,..,.;Xh
*      Gs.X8 S@@@@@@@B:..,,,,,,,,,,. rA1 ,A@@@@@@@@@H:........,,,,,,.iX:
*     ;9. ,8A#@@@@@@#5,.,,,,,,,,,... 9A. 8@@@@@@@@@@M;    ....,,,,,,,,S8
*     X3    iS8XAHH8s.,,,,,,,,,,...,..58hH@@@@@@@@@Hs       ...,,,,,,,:Gs
*    r8,        ,,,...,,,,,,,,,,.....  ,h8XABMMHX3r.          .,,,,,,,.rX:
*   :9, .    .:,..,:;;;::,.,,,,,..          .,,.               ..,,,,,,.59
*  .Si      ,:.i8HBMMMMMB&5,....                    .            .,,,,,.sMr
*  SS       :: h@@@@@@@@@@#; .                     ...  .         ..,,,,iM5
*  91  .    ;:.,1&@@@@@@MXs.                            .          .,,:,:&S
*  hS ....  .:;,,,i3MMS1;..,..... .  .     ...                     ..,:,.99
*  ,8; ..... .,:,..,8Ms:;,,,...                                     .,::.83
*   s&: ....  .sS553B@@HX3s;,.    .,;13h.                            .:::&1
*    SXr  .  ...;s3G99XA&X88Shss11155hi.                             ,;:h&,
*     iH8:  . ..   ,;iiii;,::,,,,,.                                 .;irHA
*      ,8X5;   .     .......                                       ,;iihS8Gi
*         1831,                                                 .,;irrrrrs&@
*           ;5A8r.                                            .:;iiiiirrss1H
*             :X@H3s.......                                .,:;iii;iiiiirsrh
*              r#h:;,...,,.. .,,:;;;;;:::,...              .:;;;;;;iiiirrss1
*             ,M8 ..,....,.....,,::::::,,...         .     .,;;;iiiiiirss11h
*             8B;.,,,,,,,.,.....          .           ..   .:;;;;iirrsss111h
*            i@5,:::,,,,,,,,.... .                   . .:::;;;;;irrrss111111
*            9Bi,:,,,,......                        ..r91;;;;;iirrsss1ss1111

FORM frm_chk_data USING input TYPE zmt_wms2sap_batch_chars
                  CHANGING output TYPE zmt_wms2sap_batch_chars_ret.


  "将物料进行前导零操作
  DATA: lv_matnr(18) TYPE c.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = input-mt_wms2sap_batch_chars-matnr
    IMPORTING
      output = lv_matnr.
  output-mt_wms2sap_batch_chars_ret-matnr = lv_matnr.
  "检查批次是否存在
  SELECT SINGLE *
    INTO @DATA(ls_mcha)
    FROM mcha
    WHERE mcha~matnr = @lv_matnr
      AND mcha~werks = @input-mt_wms2sap_batch_chars-werks
      AND mcha~charg = @input-mt_wms2sap_batch_chars-charg.

  IF ls_mcha IS INITIAL.
    output-mt_wms2sap_batch_chars_ret-ret_code = 'E'.
    output-mt_wms2sap_batch_chars_ret-ret_msg = '物料：[' && output-mt_wms2sap_batch_chars_ret-matnr &&
        '], 工厂：[' && input-mt_wms2sap_batch_chars-werks && ']' &&
        '批次：[' && output-mt_wms2sap_batch_chars_ret-charg && ']不存在！'.
  ENDIF.
  IF sy-subrc = 0.
    output-mt_wms2sap_batch_chars_ret-ret_code = 'S'.
    output-mt_wms2sap_batch_chars_ret-ret_msg = 'success'.
  ENDIF.





* 1.SAP接收WMS上传的批次变更信息，首先调用函数"BAPI_OBJCL_GETCLASSES"取得对应物料的库存批对象


* 2.调用函数"BAPI_BATCH_CHANGE"更改批次主数据字段，


* 3.以及调用"BAPI_MATERIAL_SAVEREPLICA"更新批分类属性。





ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_PRO_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_pro_data USING input TYPE zmt_wms2sap_batch_chars
                  CHANGING output TYPE zmt_wms2sap_batch_chars_ret.


  DATA: ls_batch_attr  LIKE bapibatchatt,
        ls_batch_attrx LIKE bapibatchattx.
  DATA: lv_old TYPE atwrt30.
  DATA: input_matnr TYPE mara-matnr.
  DATA: input_charg(10) TYPE c,
        input_werks(4)  TYPE c.
  DATA:lv_ret    TYPE TABLE OF bapiret2,
       ls_ret    TYPE bapiret2,
       lv_output LIKE bapibatchatt.
  DATA:ls_mara TYPE mara.
  DATA:material         TYPE bapibatchkey-material,
       batch            TYPE bapibatchkey-batch,
       plant            TYPE bapibatchkey-plant,
       batchattributes  LIKE  bapibatchatt,
       batchattributesx LIKE  bapibatchattx,
       matnr            LIKE mara-matnr,
       werks            LIKE marc-werks,
       charg            LIKE mcha-charg.
  input_matnr = input-mt_wms2sap_batch_chars-matnr.
  input_charg = input-mt_wms2sap_batch_chars-charg.
  input_werks = input-mt_wms2sap_batch_chars-werks.
  material    = zcl_bc_public=>conv_by_ddic( i_input = input_matnr ).
  "获取物料类型
  SELECT SINGLE
    mtart
  INTO CORRESPONDING FIELDS OF ls_mara
  FROM mara
  WHERE mara~matnr = material.


*  MATNR 物料号
*  WERKS 工厂
*  CHARG SAP批号

  "生产日期 HSDAT
  "失效日期 VFDAT
  "入库日期 LWEDT
  "厂家批次 LICHA
  "SAP批次号 CHARG
  "供应商/制造商 LIFNR /  PRODUCER_NAME
  "质量状态
  "库存地点
  "外寄+质量状态+寄售 WMS_EXT_ST
  "包装批次 PACK_CHARG
  "长批号 CHARGL
  "包装代码 PACK_CODE
  "包装规格描述 GROES
  "包装量 PACK_QYT

  "更新批次主数据
  " 生产日期
  ls_batch_attr-prod_date = input-mt_wms2sap_batch_chars-hsdat.
  IF ls_batch_attr-prod_date IS NOT INITIAL.
    ls_batch_attrx-prod_date = 'X'.
  ENDIF.
  "失效日期
  ls_batch_attr-expirydate = input-mt_wms2sap_batch_chars-vfdat.
  IF ls_batch_attr-expirydate IS NOT INITIAL.
    ls_batch_attrx-expirydate = 'X'.
  ENDIF.
  "入库日期
  ls_batch_attr-lastgrdate = input-mt_wms2sap_batch_chars-lwedt.
  IF ls_batch_attr-lastgrdate IS NOT INITIAL.
    ls_batch_attrx-lastgrdate = 'X'.
  ENDIF.
  "供应商批次
  CLEAR ls_batch_attr-vendrbatch.
  ls_batch_attr-vendrbatch = input-mt_wms2sap_batch_chars-licha.
*  "修改供应批次的长度 Seashell
  DATA length(2) TYPE c.
*  CLEAR:length,ls_batch_attr-vendrbatch.
*  length = strlen( input-mt_wms2sap_batch_chars-licha ).
*  IF length > 10.
*    ls_batch_attr-vendrbatch = 123.
*  ENDIF.
*  ls_batch_attr-vendrbatch = input-mt_wms2sap_batch_chars-licha+0(10).
  IF ls_batch_attr-vendrbatch IS NOT INITIAL.
    ls_batch_attrx-vendrbatch = 'X'.
  ENDIF.
  "供应商
  DATA lv_a(10) TYPE c.
  DATA lv_b(10) TYPE c.
  IF input-mt_wms2sap_batch_chars-lifnr = '/' OR input-mt_wms2sap_batch_chars-lifnr = ''.
    ls_batch_attr-vendor_no = ''.
  ELSE.
    SPLIT input-mt_wms2sap_batch_chars-lifnr AT '/' INTO lv_a lv_b.
    ls_batch_attr-vendor_no = lv_a.
    ls_batch_attr-vendor_no = zcl_bc_public=>conv_by_ddic( i_input = ls_batch_attr-vendor_no ).
  ENDIF.

  ls_batch_attrx-vendor_no = 'X'.


  "供应商账号
  "1、根据供应商简称找出供应商编码
*&---------------------------------------------------------------------*
*&作者：Seashell Huang
*&修改日期：14.10.2019 21:03:26
*&---------------------------------------------------------------------*
*&修改记录：去掉供应商账号字段
*&
*&---------------------------------------------------------------------*
*  SELECT sortl, lifnr
*    INTO TABLE @DATA(ls_lifnr)
*    FROM lfa1
*    WHERE sortl = @input-mt_wms2sap_batch_chars-lifnr.
*  IF sy-subrc = 0.
*    ls_batch_attr-vendor_no = input-mt_wms2sap_batch_chars-lifnr.
*  ENDIF.
*  IF ls_batch_attr-vendor_no IS NOT INITIAL.
*    ls_batch_attrx-vendor_no = 'X'.
*  ENDIF.


  material    = zcl_bc_public=>conv_by_ddic( i_input = input_matnr ).
  werks    = input_werks.
  charg    = input_charg.
  MOVE-CORRESPONDING ls_batch_attr TO batchattributes.
  MOVE-CORRESPONDING ls_batch_attrx TO batchattributesx.
  CALL FUNCTION 'BAPI_BATCH_CHANGE'
    EXPORTING
      material         = material
      batch            = charg
      plant            = werks
      batchattributes  = ls_batch_attr
      batchattributesx = ls_batch_attrx
*     BATCHCONTROLFIELDS       =
*     INTERNALNUMBERCOM        =
*     EXTENSION1       =
*     MATERIAL_EVG     =
*     MATERIAL_LONG    =
    IMPORTING
      batchattributes  = lv_output
    TABLES
      return           = lv_ret.
  IF lv_ret IS INITIAL.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    READ TABLE lv_ret INTO ls_ret INDEX 1.
    output-mt_wms2sap_batch_chars_ret-ret_msg = ls_ret-message.
    output-mt_wms2sap_batch_chars_ret-ret_code = 'E'.
  ENDIF.



*更新批次特性
* 1.SAP接收WMS上传的批次变更信息，首先调用函数"BAPI_OBJCL_GETCLASSES"取得对应物料的库存批对象
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




*  FIELD-SYMBOLS: <fv> TYPE ANY.
*  CLEAR lv_class2.
*  CALL FUNCTION 'VB_BATCH_2_CLASS_OBJECT'
*    EXPORTING
*      i_matnr = input_matnr
*      i_charg = input_charg
*      i_werks = input_werks
*    IMPORTING
*      e_objek = lv_objek
*      e_obtab = lv_obtab
*      e_klart = lv_klart
*      e_class = lv_class2.
* 解决e_class的值为空的情况：
*  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*    EXPORTING
*      input  = input_matnr
*    IMPORTING
*      output = input_matnr.
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
  "2019.10.7反复提交2次更改


  CALL FUNCTION 'BAPI_OBJCL_GETDETAIL'
    EXPORTING
*     objectkey       = ls_key-object
      objecttable     = ls_key-objecttable
      classnum        = ls_key-classnum
      classtype       = ls_key-classtype
      objectkey_long  = ls_key-object_long
    IMPORTING
      status          = lv_status
*     STANDARDCLASS   =
    TABLES
      allocvaluesnum  = lt_allocvaluesnum
      allocvalueschar = lt_allocvalueschar
      allocvaluescurr = lt_allocvaluescurr
      return          = lt_return.

  CHECK lv_status IS NOT INITIAL.
  " 下面进行内表chartact字段完整性的校验
  READ TABLE lt_allocvalueschar INTO ls_allocvalueschar WITH KEY charact = 'ZOUT_X'.
  IF sy-subrc <> 0 AND ls_mara-mtart <> 'Z050' AND ls_mara-mtart <> 'Z060' AND ls_mara-mtart <> 'Z070' AND ls_mara-mtart <> 'Z080'.
    ls_allocvalueschar-charact = 'ZOUT_X'.
    APPEND ls_allocvalueschar TO lt_allocvalueschar.
  ENDIF.
  READ TABLE lt_allocvalueschar INTO ls_allocvalueschar WITH KEY charact = 'ZST_CODE'.
  IF sy-subrc <> 0 AND ls_mara-mtart <> 'Z050'  AND ls_mara-mtart <> 'Z060' AND ls_mara-mtart <> 'Z070' AND ls_mara-mtart <> 'Z080'.
    ls_allocvalueschar-charact = 'ZST_CODE'.
    APPEND ls_allocvalueschar TO lt_allocvalueschar.
  ENDIF.
  READ TABLE lt_allocvalueschar INTO ls_allocvalueschar WITH KEY charact = 'ZPACK_CHARG'.
  IF sy-subrc <> 0
    AND ls_mara-mtart <> 'Z010' AND ls_mara-mtart <> 'Z020'
    AND ls_mara-mtart <> 'Z030' AND ls_mara-mtart <> 'Z040'
    AND ls_mara-mtart <> 'Z060' AND ls_mara-mtart <> 'Z070'
    AND ls_mara-mtart <> 'Z090' AND ls_mara-mtart <> 'Z050'.
    ls_allocvalueschar-charact = 'ZPACK_CHARG'.
    APPEND ls_allocvalueschar TO lt_allocvalueschar.
  ENDIF.
*  READ TABLE lt_allocvalueschar INTO ls_allocvalueschar WITH KEY charact = 'Z_BATCH_DYPC'."成品的打印批号
*  IF sy-subrc <> 0.
*    ls_allocvalueschar-charact = 'Z_BATCH_DYPC'.
*    APPEND ls_allocvalueschar TO lt_allocvalueschar.
*  ENDIF.
  READ TABLE lt_allocvalueschar INTO ls_allocvalueschar WITH KEY charact = 'ZCHARGL'.
  IF sy-subrc <> 0
    AND ls_mara-mtart <> 'Z050' AND ls_mara-mtart <> 'Z060'
    AND ls_mara-mtart <> 'Z070' AND ls_mara-mtart <> 'Z080'.
    ls_allocvalueschar-charact = 'ZCHARGL'.
    APPEND ls_allocvalueschar TO lt_allocvalueschar.
  ENDIF.
  READ TABLE lt_allocvalueschar INTO ls_allocvalueschar WITH KEY charact = 'ZPACK_CODE'.
  IF sy-subrc <> 0
    AND ls_mara-mtart <> 'Z050' AND ls_mara-mtart <> 'Z060'
    AND ls_mara-mtart <> 'Z070' AND ls_mara-mtart <> 'Z080'.
    ls_allocvalueschar-charact = 'ZPACK_CODE'.
    APPEND ls_allocvalueschar TO lt_allocvalueschar.
  ENDIF.
  READ TABLE lt_allocvalueschar INTO ls_allocvalueschar WITH KEY charact = 'ZPACK_STA'.
  IF sy-subrc <> 0
    AND ls_mara-mtart <> 'Z050' AND ls_mara-mtart <> 'Z060'
    AND ls_mara-mtart <> 'Z070' AND ls_mara-mtart <> 'Z080'.
    ls_allocvalueschar-charact = 'ZPACK_STA'.
    APPEND ls_allocvalueschar TO lt_allocvalueschar.
  ENDIF.
  READ TABLE lt_allocvalueschar INTO ls_allocvalueschar WITH KEY charact = 'ZPACK_QUA'.
  IF sy-subrc <> 0 AND ls_mara-mtart <> 'Z050' AND ls_mara-mtart <> 'Z030'.
    ls_allocvalueschar-charact = 'ZPACK_QUA'.
    APPEND ls_allocvalueschar TO lt_allocvalueschar.
  ENDIF.
  "上面是char类型
  READ TABLE lt_allocvalueschar INTO ls_allocvalueschar WITH KEY charact = 'ZLICHA'.
  IF sy-subrc <> 0
    AND ls_mara-mtart <> 'Z050' AND ls_mara-mtart <> 'Z060'
    AND ls_mara-mtart <> 'Z070' AND ls_mara-mtart <> 'Z080'.
    ls_allocvalueschar-charact = 'ZLICHA'.
    APPEND ls_allocvalueschar TO lt_allocvalueschar.
  ENDIF.
  READ TABLE lt_allocvalueschar INTO ls_allocvalueschar WITH KEY charact = 'ZLIFNR'.
  IF sy-subrc <> 0
  AND ls_mara-mtart <> 'Z050' AND ls_mara-mtart <> 'Z060'
  AND ls_mara-mtart <> 'Z070' AND ls_mara-mtart <> 'Z080'.
    ls_allocvalueschar-charact = 'ZLIFNR'.
    APPEND ls_allocvalueschar TO lt_allocvalueschar.
  ENDIF.
  READ TABLE lt_allocvalueschar INTO ls_allocvalueschar WITH KEY charact = 'ZPRODUCER'.
  IF sy-subrc <> 0
  AND ls_mara-mtart <> 'Z050' AND ls_mara-mtart <> 'Z060'
  AND ls_mara-mtart <> 'Z070' AND ls_mara-mtart <> 'Z080'.
    ls_allocvalueschar-charact = 'ZPRODUCER'.
    APPEND ls_allocvalueschar TO lt_allocvalueschar.
  ENDIF.



  "下面是num类型
  READ TABLE lt_allocvaluesnum INTO ls_allocvaluesnum WITH KEY charact = 'ZHSDAT'.
  IF sy-subrc <> 0 AND ls_mara-mtart <> 'Z060' AND ls_mara-mtart <> 'Z070'.
    ls_allocvaluesnum-charact = 'ZHSDAT'.
    APPEND ls_allocvaluesnum TO lt_allocvaluesnum.
  ENDIF.
  READ TABLE lt_allocvaluesnum INTO ls_allocvaluesnum WITH KEY charact = 'ZVFDAT'.
  IF sy-subrc <> 0 AND ls_mara-mtart <> 'Z060' AND ls_mara-mtart <> 'Z070'.
    ls_allocvaluesnum-charact = 'ZVFDAT'.
    APPEND ls_allocvaluesnum TO lt_allocvaluesnum.
  ENDIF.
  READ TABLE lt_allocvaluesnum INTO ls_allocvaluesnum WITH KEY charact = 'ZLWEDT'.
  IF sy-subrc <> 0 AND ls_mara-mtart <> 'Z060' AND ls_mara-mtart <> 'Z070'.
    ls_allocvaluesnum-charact = 'ZLWEDT'.
    APPEND ls_allocvaluesnum TO lt_allocvaluesnum.
  ENDIF.
  " 修改批次属性
  LOOP AT lt_allocvalueschar INTO ls_allocvalueschar.
    IF ls_allocvalueschar-charact = 'ZPRODUCER'." 制造商编码
      ls_allocvalueschar-value_char_long = ls_allocvalueschar-value_char
      = input-mt_wms2sap_batch_chars-producer_name.
      ls_allocvalueschar-value_neutral_long = ls_allocvalueschar-value_neutral
      = input-mt_wms2sap_batch_chars-producer_name.
      MODIFY lt_allocvalueschar FROM ls_allocvalueschar INDEX sy-tabix.
    ENDIF.
    IF ls_allocvalueschar-charact = 'ZPACK_CHARG'." 包装批号（打印）
      ls_allocvalueschar-value_char_long = ls_allocvalueschar-value_char
      = input-mt_wms2sap_batch_chars-pack_charg.
      ls_allocvalueschar-value_neutral_long = ls_allocvalueschar-value_neutral
      = input-mt_wms2sap_batch_chars-pack_charg.
      MODIFY lt_allocvalueschar FROM ls_allocvalueschar INDEX sy-tabix.
    ENDIF.
    IF ls_allocvalueschar-charact = 'ZCHARGL'. " 长批号
*      ls_allocvalueschar-value_char_long = ls_allocvalueschar-value_char
*      = input-mt_wms2sap_batch_chars-chargl.
*      ls_allocvalueschar-value_neutral_long = ls_allocvalueschar-value_neutral
*      = input-mt_wms2sap_batch_chars-chargl.
      "修复长批号的长度问题，上面注释掉
*      ls_allocvalueschar-value_char = input-mt_wms2sap_batch_chars-chargl.
*      ls_allocvalueschar-value_neutral = input-mt_wms2sap_batch_chars-chargl.
      CLEAR: ls_allocvalueschar-value_char,ls_allocvalueschar-value_neutral.
      ls_allocvalueschar-value_char_long = input-mt_wms2sap_batch_chars-chargl.
      ls_allocvalueschar-value_neutral_long = input-mt_wms2sap_batch_chars-chargl.
      MODIFY lt_allocvalueschar FROM ls_allocvalueschar INDEX sy-tabix.
    ENDIF.
    IF ls_allocvalueschar-charact = 'ZPACK_CODE'. " 包装代码
      ls_allocvalueschar-value_char_long = ls_allocvalueschar-value_char
      = input-mt_wms2sap_batch_chars-pack_code.
      ls_allocvalueschar-value_neutral_long = ls_allocvalueschar-value_neutral
      = input-mt_wms2sap_batch_chars-pack_code.
      MODIFY lt_allocvalueschar FROM ls_allocvalueschar INDEX sy-tabix.
    ENDIF.
*    IF ls_allocvalueschar-charact = 'Z_BATCH_DYPC'. " 包装代码(成品的)
*      ls_allocvalueschar-value_char_long = ls_allocvalueschar-value_char
*      = INPUT-mt_wms2sap_batch_chars-pack_code.
*      ls_allocvalueschar-value_neutral_long = ls_allocvalueschar-value_neutral
*      = INPUT-mt_wms2sap_batch_chars-pack_code.
*      MODIFY lt_allocvalueschar FROM ls_allocvalueschar INDEX sy-tabix.
*    ENDIF.
    IF ls_allocvalueschar-charact = 'ZPACK_STA'." 包装规格
      ls_allocvalueschar-value_char_long = ls_allocvalueschar-value_char
      = input-mt_wms2sap_batch_chars-groes.
      ls_allocvalueschar-value_neutral_long = ls_allocvalueschar-value_neutral
      = input-mt_wms2sap_batch_chars-groes.
      MODIFY lt_allocvalueschar FROM ls_allocvalueschar INDEX sy-tabix.
    ENDIF.
    IF ls_allocvalueschar-charact = 'ZPACK_QUA'." 包装量
      ls_allocvalueschar-value_char_long = ls_allocvalueschar-value_char
      = input-mt_wms2sap_batch_chars-pack_qyt.
      ls_allocvalueschar-value_neutral_long = ls_allocvalueschar-value_neutral
      = input-mt_wms2sap_batch_chars-pack_qyt.
      MODIFY lt_allocvalueschar FROM ls_allocvalueschar INDEX sy-tabix.
    ENDIF.
    IF ls_allocvalueschar-charact = 'ZLICHA'." 厂家批次
      CLEAR:ls_allocvalueschar-value_char_long,ls_allocvalueschar-value_char,ls_allocvalueschar-value_neutral,ls_allocvalueschar-value_neutral_long.
      ls_allocvalueschar-value_char_long = ls_allocvalueschar-value_char
      = input-mt_wms2sap_batch_chars-licha.
      ls_allocvalueschar-value_neutral_long = ls_allocvalueschar-value_neutral
      = input-mt_wms2sap_batch_chars-licha.
      "修复厂家批次过长的问题，上面注释掉Seashell
*      CLEAR:length.
*      length = strlen( input-mt_wms2sap_batch_chars-licha ).
*      IF length > 20.
*        CLEAR: ls_allocvalueschar-value_char,ls_allocvalueschar-value_neutral.
*        ls_allocvalueschar-value_char_long = input-mt_wms2sap_batch_chars-licha.
*        ls_allocvalueschar-value_neutral_long = input-mt_wms2sap_batch_chars-licha.
*      ENDIF.
      MODIFY lt_allocvalueschar FROM ls_allocvalueschar INDEX sy-tabix.
    ENDIF.
*    IF ls_allocvalueschar-charact = 'ZLIFNR'." 供应商编号
*      ls_allocvalueschar-value_char_long = ls_allocvalueschar-value_char
*      = input-mt_wms2sap_batch_chars-lifnr.
*      ls_allocvalueschar-value_neutral_long = ls_allocvalueschar-value_neutral
*      = input-mt_wms2sap_batch_chars-lifnr.
*      MODIFY lt_allocvalueschar FROM ls_allocvalueschar INDEX sy-tabix.
*    ENDIF.
    " 对标识进行拆分
    DATA: string(3) TYPE c,
          p1(1)     TYPE c   VALUE '',
          p2(1)     TYPE c   VALUE '',
          p3(1)     TYPE c   VALUE '',
          del(3)    VALUE ''.
    string = input-mt_wms2sap_batch_chars-wms_ext_st.
*    p1 = string+0(1)."外寄标识
*    p2 = string+1(1). "质量标识
*    p3 = string+2(1)." 寄售标识
    DATA:lt_wms TYPE TABLE OF zmmwms_ext_st,
         ls_wms TYPE zmmwms_ext_st.
    SELECT SINGLE
      *
      INTO CORRESPONDING FIELDS OF ls_wms
      FROM zmmwms_ext_st
      WHERE zmmwms_ext_st~wms_ext_st = string.
    p1 = string+1(1)."外寄标识
    p2 = string+2(1)."质量标识
    p3 = string+0(1).
    " 质量标识
    IF ls_allocvalueschar-charact = 'ZST_CODE'. " 质量标识
      IF p2 = '1'.
        ls_allocvalueschar-value_char_long = ls_allocvalueschar-value_char
        = ' '.
        ls_allocvalueschar-value_neutral_long = ls_allocvalueschar-value_neutral
        = ' '.
        MODIFY lt_allocvalueschar FROM ls_allocvalueschar INDEX sy-tabix.
      ENDIF.
      IF p2 = '2'.
        ls_allocvalueschar-value_char_long = ls_allocvalueschar-value_char
        = 'TT'.
        ls_allocvalueschar-value_neutral_long = ls_allocvalueschar-value_neutral
        = 'TT'.
        MODIFY lt_allocvalueschar FROM ls_allocvalueschar INDEX sy-tabix.
      ENDIF.
      IF p2 = '3'.
        ls_allocvalueschar-value_char_long = ls_allocvalueschar-value_char
        = 'TC'.
        ls_allocvalueschar-value_neutral_long = ls_allocvalueschar-value_neutral
        = 'TC'.
        MODIFY lt_allocvalueschar FROM ls_allocvalueschar INDEX sy-tabix.
      ENDIF.
      IF p2 = '4'.
        ls_allocvalueschar-value_char_long = ls_allocvalueschar-value_char
        = 'ZC'.
        ls_allocvalueschar-value_neutral_long = ls_allocvalueschar-value_neutral
        = 'ZC'.
        MODIFY lt_allocvalueschar FROM ls_allocvalueschar INDEX sy-tabix.
      ENDIF.
    ENDIF.
    " 外寄标识
    IF ls_allocvalueschar-charact = 'ZOUT_X'.
      IF p1 = '1'.
        ls_allocvalueschar-value_char_long = ls_allocvalueschar-value_char
        = ' '.
        ls_allocvalueschar-value_neutral_long = ls_allocvalueschar-value_neutral
        = ' '.
        MODIFY lt_allocvalueschar FROM ls_allocvalueschar INDEX sy-tabix.
      ENDIF.
      IF p1 = '2'.
        ls_allocvalueschar-value_char_long = ls_allocvalueschar-value_char
        = 'X'.
        ls_allocvalueschar-value_neutral_long = ls_allocvalueschar-value_neutral
        = 'X'.
        MODIFY lt_allocvalueschar FROM ls_allocvalueschar INDEX sy-tabix.
      ENDIF.
    ENDIF.
  ENDLOOP.
  LOOP AT lt_allocvaluesnum INTO ls_allocvaluesnum.
    IF ls_allocvaluesnum-charact = 'ZHSDAT'." 生产日期
      ls_allocvaluesnum-value_from = input-mt_wms2sap_batch_chars-hsdat.
      ls_allocvaluesnum-value_to = input-mt_wms2sap_batch_chars-hsdat.
      ls_allocvaluesnum-value_relation = '1'.
*      CLEAR ls_allocvaluesnum-value_from.
*      CLEAR ls_allocvaluesnum-value_to.
*      ls_allocvaluesnum-value_from = zcl_bc_public=>date_to_float( input-mt_wms2sap_batch_chars-hsdat ).
*      ls_allocvaluesnum-value_to = zcl_bc_public=>date_to_float( input-mt_wms2sap_batch_chars-hsdat ).
      MODIFY lt_allocvaluesnum FROM ls_allocvaluesnum INDEX sy-tabix.
    ENDIF.
    IF ls_allocvaluesnum-charact = 'ZVFDAT'." 货架寿命到期日
      ls_allocvaluesnum-value_from = input-mt_wms2sap_batch_chars-vfdat.
      ls_allocvaluesnum-value_to = input-mt_wms2sap_batch_chars-vfdat.
      ls_allocvaluesnum-value_relation = '1'.
*      CLEAR ls_allocvaluesnum-value_from.
*      CLEAR ls_allocvaluesnum-value_to.
*        ls_allocvaluesnum-value_from = zcl_bc_public=>date_to_float( input-mt_wms2sap_batch_chars-hsdat ).
      MODIFY lt_allocvaluesnum FROM ls_allocvaluesnum INDEX sy-tabix.
    ENDIF.
    IF ls_allocvaluesnum-charact = 'ZLWEDT'." 最近的收货日期
      ls_allocvaluesnum-value_from = input-mt_wms2sap_batch_chars-lwedt.
      ls_allocvaluesnum-value_to = input-mt_wms2sap_batch_chars-lwedt.
      ls_allocvaluesnum-value_relation = '1'.
*        ls_allocvaluesnum-value_from = zcl_bc_public=>date_to_float( input-mt_wms2sap_batch_chars-hsdat ).
      MODIFY lt_allocvaluesnum FROM ls_allocvaluesnum INDEX sy-tabix.
    ENDIF.
    CLEAR ls_allocvaluesnum.
  ENDLOOP.




*  CLEAR lv_status.
  CALL FUNCTION 'BAPI_OBJCL_CHANGE'
    EXPORTING
*     OBJECTKEY          =
      objecttable        = ls_key-objecttable
      classnum           = ls_key-classnum
      classtype          = ls_key-classtype
      objectkey_long     = ls_key-object_long
    IMPORTING
      classif_status     = lv_status
    TABLES
      allocvaluesnumnew  = lt_allocvaluesnum
      allocvaluescharnew = lt_allocvalueschar
      allocvaluescurrnew = lt_allocvaluescurr
      return             = lt_return.
* 2.调用函数"BAPI_BATCH_CHANGE"更改批次主数据字段，


* 3.以及调用"BAPI_MATERIAL_SAVEREPLICA"更新批分类属性。

  IF lv_status <> 0.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.

    GV_MARKS = 'X' ."add by lanjingcun 20201207 因为程序循环五次 在此记录成功的状态

  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    "2019.9.26添加返回的错误信息（开始）
    READ TABLE lt_return INTO ls_return INDEX 1.
*    output-mt_wms2sap_batch_chars_ret-ret_msg = '当前批次可能正在被编辑，请稍候重试'.
    output-mt_wms2sap_batch_chars_ret-ret_msg = ls_return-message.
    output-mt_wms2sap_batch_chars_ret-ret_code = 'E'.

    "2019.9.26添加返回的错误信息（结束）
  ENDIF.
  IF sy-subrc = 0.
    output-mt_wms2sap_batch_chars_ret-werks = werks.
    output-mt_wms2sap_batch_chars_ret-charg = charg.
    output-mt_wms2sap_batch_chars_ret-wms_custid = werks.
  ENDIF.
ENDFORM.