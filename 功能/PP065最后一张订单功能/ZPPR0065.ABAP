*&---------------------------------------------------------------------*
*& Report ZPPR0065
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zppr0065.
TABLES:ausp.
DATA:lt_zppt0065a TYPE TABLE OF zppt0065a,
     ls_zppt0065a LIKE LINE OF lt_zppt0065a.
DATA:lt_zppt0065b     TYPE TABLE OF zppt0065b,
     ls_zppt0065b_tmp LIKE LINE OF lt_zppt0065b, "保存着不变的数据
     ls_zppt0065b     LIKE LINE OF lt_zppt0065b.
DATA:stb    TYPE TABLE OF stpox,
     ls_stb TYPE stpox.
DATA:lv_atinn TYPE ausp-atinn.
FIELD-SYMBOLS:<fs_zppt0065a> LIKE zppt0065a.

"定义用于判断内表相同的变量
DATA:lt_zppt0065b_add     TYPE TABLE OF zppt0065b,
     lt_zppt0065b_mod     TYPE TABLE OF zppt0065b,
     lt_zppt0065b_new     TYPE TABLE OF zppt0065b,
     lt_zppt0065b_old     TYPE TABLE OF zppt0065b,
     lt_zppt0065b_old_tmp TYPE TABLE OF zppt0065b.
"用于判断成品物料以及个性物料的删除标识所在的内表
DATA:lt_zppt0065b_cp TYPE TABLE OF zppt0065b,
     lt_zppt0065b_gx TYPE TABLE OF zppt0065b.
"2.1 把所有销售订单表VBEP中“计划行类别”ETTYP=“ZA”的记录，
"当成品物料、销售订单、销售订单行号在ZPPT0065A表不存时插入ZPPT0065A表(期初数据判断：
"如果ZPPT0065A表为空的话，需要查询2021.5.1日至今的数据插入到A表中)，默认传输标识为“N”。

"首先先查询A表的数据，如果查询结果为空的话，证明需要期初导入2021年5月至今的销售订单期初数据
SELECT * FROM zppt0065a INTO TABLE @DATA(lt_zppt0065a_first).
IF lt_zppt0065a_first IS INITIAL.
  SELECT
    mandt,
    vbeln,
    posnr,
    matnr AS zcp,
    werks,
    ( 'N' ) AS zgisn
  FROM vbap
  WHERE EXISTS ( SELECT DISTINCT *
                 FROM vbep WHERE vbeln = vbap~vbeln
                             AND posnr = vbap~posnr )
    AND NOT EXISTS ( SELECT * FROM zppt0065a WHERE vbeln = vbap~vbeln AND  posnr = vbap~posnr )
    AND EXISTS ( SELECT * FROM mara WHERE matnr = vbap~matnr AND mtart = 'Z050' )
  APPENDING TABLE @DATA(lt_vbap).
ENDIF.

"把所有销售订单表VBEP中“计划行类别”ETTYP=“ZA”的记录，当成品物料、销售订单、销售订单行号在A表不存时插入A表，默认传输标识为“N”。
SELECT
  mandt,
  vbeln,
  posnr,
  matnr AS zcp,
  werks,
  ( 'N' ) AS zgisn
FROM vbap
WHERE EXISTS ( SELECT DISTINCT *
               FROM vbep WHERE vbeln = vbap~vbeln
                           AND posnr = vbap~posnr
                           AND ettyp = 'ZA' )
  AND NOT EXISTS ( SELECT * FROM zppt0065a WHERE vbeln = vbap~vbeln AND  posnr = vbap~posnr )
  AND EXISTS ( SELECT * FROM mara WHERE matnr = vbap~matnr AND mtart = 'Z050' )
APPENDING TABLE @lt_vbap.

IF lt_vbap IS NOT INITIAL.
  MOVE-CORRESPONDING lt_vbap TO lt_zppt0065a.
  SORT lt_zppt0065a BY mandt zcp vbeln posnr werks.
  DELETE ADJACENT DUPLICATES FROM lt_zppt0065a COMPARING ALL FIELDS.
  INSERT zppt0065a FROM TABLE lt_zppt0065a.
  COMMIT WORK.
ENDIF.



"2.2循序取出自建ZPPT0065A表中“传输标识”=“N”的记录，
"逐行CS_BOM_EXPL_MAT_V2展BOM取出组件物料，查找物料主数据“分类”视图“使用原则”，
"当“使用原则”为20、30时，插入自建ZPPT0065B表，默认“最后一张订单“标识为“N”，并把ZPPT0065A表的传输标识为“Y”。
CLEAR lt_zppt0065a.
"循序取出自建A表中“传输标识“=”N”的记录，逐行CS_BOM_EXPL_MAT_V2展BOM取出组件物料
SELECT * FROM zppt0065a WHERE zgisn = 'N' INTO CORRESPONDING FIELDS OF TABLE @lt_zppt0065a.

CHECK lt_zppt0065a IS NOT INITIAL.
"获取BOM抬头的数据
SELECT
  matnr,
  werks,
  stlan,
  mast~stlnr,
  mast~stlal,
  stkoz,
  bmeng
  FROM mast
  INNER JOIN stko
  ON stko~stlnr = mast~stlnr
  AND stlty = 'M'
  FOR ALL ENTRIES IN  @lt_zppt0065a
  WHERE mast~matnr = @lt_zppt0065a-zcp
    AND mast~werks = @lt_zppt0065a-werks
    AND stkoz = 1
    AND stlan = 1
  INTO TABLE @DATA(lt_mast).

SORT lt_mast BY matnr werks ASCENDING stkoz DESCENDING.


"数值转换
CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
  EXPORTING
    input  = 'ZUSEPRIN'
  IMPORTING
    output = lv_atinn.


"获取物料特性
SELECT
  objek,
  atinn,
  atwrt
FROM ausp
INTO TABLE @DATA(lt_ausp)
WHERE ausp~atinn = @lv_atinn
  AND ausp~atwrt IN ( '20','30' ).

"循环处理B表前，先对A内表的数据进行处理，对成品物料号还有工厂进行去重
SORT lt_zppt0065a BY zcp werks.
DELETE ADJACENT DUPLICATES FROM lt_zppt0065a COMPARING zcp werks.

"获取B表的记录，如果B表已经存在的记录需要剔除掉
SELECT
  *
FROM zppt0065b
INTO TABLE @DATA(lt_zppt0065b_del).


"循序取出自建A表中“传输标识“=”N”的记录
LOOP AT lt_zppt0065a INTO ls_zppt0065a.
*  READ TABLE lt_ausp INTO DATA(ls_ausp) WITH KEY objek = ls_zppt0065a-zcp.
*  IF sy-subrc = 0.
  "如果有读到的话，需要写进B表
  MOVE-CORRESPONDING ls_zppt0065a TO ls_zppt0065b_tmp.
  "获取抬头的基本数量
  READ TABLE lt_mast INTO DATA(ls_mast) WITH KEY werks = ls_zppt0065a-werks
                                                 matnr = ls_zppt0065a-zcp.
  IF sy-subrc = 0.
    ls_zppt0065b_tmp-zcpmenge = ls_mast-bmeng.

    "接下来进行展BOM，然后判断BOM的物料特性是否符合B表的数据要求
    CALL FUNCTION 'CS_BOM_EXPL_MAT_V2'
      EXPORTING
        capid                 = 'PP01'
        datuv                 = sy-datum
        mktls                 = 'X'
        mehrs                 = 'X'
        mtnrv                 = ls_zppt0065b_tmp-zcp
        stlal                 = '01'
        stlan                 = '1'
        svwvo                 = 'X'
        werks                 = ls_zppt0065b_tmp-werks
        vrsvo                 = 'X'
      TABLES
        stb                   = stb
*       MATCAT                = MATCAT
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
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

    LOOP AT stb INTO ls_stb.
      MOVE-CORRESPONDING ls_zppt0065b_tmp TO ls_zppt0065b.
      ls_zppt0065b-zgx = ls_stb-idnrk.
      ls_zppt0065b-zgxmenge = ls_stb-mnglg.
      READ TABLE lt_ausp INTO DATA(ls_ausp) WITH KEY objek = ls_stb-idnrk.
      IF sy-subrc = 0.
        READ TABLE lt_zppt0065b_del INTO DATA(ls_zppt0065b_del) WITH KEY zcp = ls_zppt0065b_tmp-zcp
                                                                         zgx = ls_stb-idnrk
                                                                         werks = ls_zppt0065b_tmp-werks.
        IF sy-subrc NE 0.
          ls_zppt0065b-zlast_sign = 'N'.
          APPEND ls_zppt0065b TO lt_zppt0065b.
          CLEAR ls_zppt0065b_del.
        ENDIF.

        CLEAR ls_zppt0065b.
      ENDIF.
      CLEAR:ls_stb.
    ENDLOOP.
  ENDIF.
  "展BOM结束
*  ENDIF.
  CLEAR:ls_zppt0065a,ls_zppt0065b_tmp.

ENDLOOP.
"反写a表的数据传输标识为Y
IF lt_zppt0065b IS NOT INITIAL.
  INSERT zppt0065b FROM TABLE lt_zppt0065b.
  IF sy-subrc EQ 0.
    CLEAR:lt_zppt0065a,ls_zppt0065a.
    CHECK lt_zppt0065b IS NOT INITIAL.
    SELECT
      *
      FROM zppt0065a INTO CORRESPONDING FIELDS OF TABLE @lt_zppt0065a
      FOR ALL ENTRIES IN @lt_zppt0065b
      WHERE zcp = @lt_zppt0065b-zcp
        AND werks = @lt_zppt0065b-werks.
    "把A表的传输标识为“Y”
    LOOP AT lt_zppt0065a ASSIGNING <fs_zppt0065a>.
      <fs_zppt0065a>-zgisn = 'Y'.
    ENDLOOP.
    MODIFY zppt0065a FROM TABLE lt_zppt0065a.
  ENDIF.
ENDIF.




"下面对B表的删除标识进行操作
*2.3.1、当个性物料作删除标识时（MARC表LVORM=X），把ZPPT0065B表中所有的该个性物料的“删际标识”为“X”
*2.3.2、当成品物料作删除标识时（MARC表LVORM=X），把ZPPT0065B表中成品物料下的个性物料的“删除标识”为X
*2.3.3、当个性物料取消“删除标识”时，把ZPPT0065B表中的所有的该个性物料的“删际标识”为空
*2.3.4、当成品物料取消“删除标识”时，把ZPPT0065B表中成品下的个性物料把“删除标识”为空
"查询B表的成品物料与MRAC冻结标识不一致的数据出来
SELECT
  zppt0065b~mandt
  zppt0065b~zcp
  zppt0065b~zgx
  zppt0065b~werks
  zppt0065b~zcpmenge
  zppt0065b~zgxmenge
  zppt0065b~zlast_sign
  marc~lvorm AS zdelete
  FROM zppt0065b
  INNER JOIN marc
  ON marc~matnr = zppt0065b~zcp
  AND marc~werks = zppt0065b~werks
  INTO TABLE lt_zppt0065b
  WHERE EXISTS ( SELECT * FROM marc WHERE matnr = zppt0065b~zcp
                                      AND werks = zppt0065b~werks
                                      AND lvorm NE zppt0065b~zdelete ).


SORT lt_zppt0065b BY zcp werks.
DELETE ADJACENT DUPLICATES FROM lt_zppt0065b COMPARING ALL FIELDS.
"循环读取，将相关数据反写回B表
LOOP AT lt_zppt0065b ASSIGNING FIELD-SYMBOL(<fs_zppt0065b>).
  UPDATE zppt0065b SET zdelete = <fs_zppt0065b>-zdelete WHERE zcp = <fs_zppt0065b>-zcp AND werks = <fs_zppt0065b>-werks.
ENDLOOP.
CLEAR lt_zppt0065b.

"查询B表的个性物料与MRAC冻结标识不一致的数据出来
SELECT
  zppt0065b~mandt
  zppt0065b~zcp
  zppt0065b~zgx
  zppt0065b~werks
  zppt0065b~zcpmenge
  zppt0065b~zgxmenge
  zppt0065b~zlast_sign
  marc~lvorm AS zdelete
  FROM zppt0065b
  INNER JOIN marc
  ON marc~matnr = zppt0065b~zgx
  AND marc~werks = zppt0065b~werks
  INTO TABLE lt_zppt0065b
  WHERE EXISTS ( SELECT * FROM marc WHERE matnr = zppt0065b~zgx
                                      AND werks = zppt0065b~werks
                                      AND lvorm NE zppt0065b~zdelete )
    AND NOT EXISTS ( SELECT * FROM marc WHERE matnr = zppt0065b~zcp
                                      AND werks = zppt0065b~werks
                                      AND lvorm = 'X' ).
"循环读取，将相关数据反写回B表
LOOP AT lt_zppt0065b ASSIGNING <fs_zppt0065b>.
  UPDATE zppt0065b SET zdelete = <fs_zppt0065b>-zdelete WHERE zgx = <fs_zppt0065b>-zgx AND werks = <fs_zppt0065b>-werks
                                                          AND zcp = <fs_zppt0065b>-zcp.
ENDLOOP.
CLEAR lt_zppt0065b.


"上面是对删除标识的操作，其他的操作写在增强或者另外一个程序里



**"回查所有的b表的数据，判断是否有不同订单出现同个个性物料的情况，
**"如果存在不同订单是同个个性物料的，对应的个性物料的最后一张
**"订单标识为N
**CLEAR: lt_zppt0065b,lt_zppt0065b_del.
**SELECT
**  *
**FROM zppt0065b
**INTO TABLE lt_zppt0065b_old WHERE zdelete <> 'X'."保存全部的数据，包含有个性物料相同的行项目
**
**"用new表保存个性物料不同的行项目，然后与old表作比较，差异的部分就是相同的行项目
**MOVE lt_zppt0065b_old TO lt_zppt0065b_new.
**MOVE lt_zppt0065b_old TO lt_zppt0065b_old_tmp.
**SORT lt_zppt0065b_new BY zgx.
**DELETE ADJACENT DUPLICATES FROM lt_zppt0065b_new COMPARING zgx.
**"这个时候lt_zppt0065b_new表保存的是不同的数据，接下来回到old表查出相同的数据
**LOOP AT lt_zppt0065b_new ASSIGNING FIELD-SYMBOL(<fs_zppt0065b_new>).
**  DELETE TABLE lt_zppt0065b_old_tmp FROM <fs_zppt0065b_new>.
**ENDLOOP.
**SORT lt_zppt0065b_old_tmp BY zgx.
**DELETE ADJACENT DUPLICATES FROM lt_zppt0065b_old_tmp COMPARING zgx.
**
**
**CALL FUNCTION 'CTVB_COMPARE_TABLES'
**  EXPORTING
**    table_old  = lt_zppt0065b_old
**    table_new  = lt_zppt0065b_old_tmp
**    key_length = 8
***   IF_SORTED  =
**  IMPORTING
**    table_del  = lt_zppt0065b_del
**    table_add  = lt_zppt0065b_add
**    table_mod  = lt_zppt0065b_mod
***   NO_CHANGES =
**  .
**IF lt_zppt0065b_del IS NOT INITIAL.
**  SORT lt_zppt0065b_del BY zgx.
**  DELETE ADJACENT DUPLICATES FROM lt_zppt0065b_del COMPARING zgx.
**ENDIF.
**
**LOOP AT lt_zppt0065b_old ASSIGNING FIELD-SYMBOL(<ls_zppt0065b_old>).
**  READ TABLE lt_zppt0065b_mod INTO ls_zppt0065b_tmp WITH KEY zgx = <ls_zppt0065b_old>-zgx.
**  "如果有读取到的话，证明这个个性物料在其他的成品物料有使用，所以对应的最后一张订单的标识为N
**  IF sy-subrc = 0.
**    <ls_zppt0065b_old>-zlast_sign = 'N'.
**    "修改透明表
**    MODIFY zppt0065b FROM <ls_zppt0065b_old>.
**  ENDIF.
**ENDLOOP.
**COMMIT WORK.
**
**"上面是对最后一张订单标识的处理，接下来是对删除标识的处理
***循环处理B表，3.1当个性物料主数据有删除标识时（MARC表LVORM=X），在ZPPT0065B表中原料对应的个性物料“删除标识”为X
*** 3.2.当成品主数据作删除标识时（MARC表LVORM=X），也把ZPPT0065B表中原料对应的成品下的个性物料把“删除标识”为X
**"首先先判断成品物料是否是有删除标识，如果有删除标识的话对应的个性物料也要有删除标记
**MOVE lt_zppt0065b_old TO lt_zppt0065b_cp.
**SORT lt_zppt0065b_cp BY zcp werks.
**DELETE ADJACENT DUPLICATES FROM lt_zppt0065b_cp COMPARING zcp werks."去重，保留不同的数据
**
**IF lt_zppt0065b_cp IS NOT INITIAL.
**  "找出成品，看是否是带上了删除标识
**  SELECT
**    matnr,
**    werks,
**    lvorm
**  FROM marc
**  INTO TABLE @DATA(lt_marc_cp)
**  FOR ALL ENTRIES IN @lt_zppt0065b_cp
**  WHERE matnr = @lt_zppt0065b_cp-zcp
**    AND werks = @lt_zppt0065b_cp-werks
**    AND lvorm = 'X'.
**  "如果成品物料是删除的，那么对应下面的个性物料也需要删除
**  LOOP AT lt_marc_cp ASSIGNING FIELD-SYMBOL(<ls_marc_cp>).
**    READ TABLE lt_zppt0065b_cp INTO DATA(ls_zppt0065b_cp) WITH KEY werks = <ls_marc_cp>-werks
**                                                                   zcp = <ls_marc_cp>-matnr.
**    IF sy-subrc = 0.
**      UPDATE zppt0065b SET zdelete = 'X' WHERE zcp = <ls_marc_cp>-matnr AND werks = <ls_marc_cp>-werks.
**    ENDIF.
**    CLEAR ls_zppt0065b_cp.
**  ENDLOOP.
**  COMMIT WORK.
**
**  "接下里对个性物料进行判断，如果个性物料是删除的，则对应个性物料打上删除标识
**  SELECT
**    matnr,
**    marc~werks,
**    lvorm
**  FROM marc
**  INNER JOIN zppt0065b
**  ON zppt0065b~zgx = marc~matnr
**  AND zppt0065b~werks = marc~werks
**  WHERE lvorm = 'X'
**    AND zdelete <> 'X'
**  INTO TABLE @DATA(lt_marc_gx).
**  " 查询出个性物料还没有打上删除标识的数据
**  SELECT
**    *
**  FROM zppt0065b
**  WHERE zdelete <> 'X'
**  INTO TABLE @lt_zppt0065b_gx.
**
**  LOOP AT lt_marc_gx ASSIGNING FIELD-SYMBOL(<ls_marc_gx>).
**    READ TABLE lt_zppt0065b_gx INTO DATA(ls_zppt0065b_gx) WITH KEY zgx = <ls_marc_gx>-matnr
**                                                                   werks = <ls_marc_gx>-werks.
**    IF sy-subrc = 0.
**      UPDATE zppt0065b SET zdelete = 'X' WHERE zgx = <ls_marc_gx>-matnr AND werks = <ls_marc_gx>-werks.
**    ENDIF.
**    CLEAR ls_zppt0065b_gx.
**  ENDLOOP.
**
**ENDIF.


CLEAR:lt_vbap.

WRITE '运行结束'.