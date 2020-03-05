*&---------------------------------------------------------------------*
*& Report ZMMR0046
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zmmr0046.
TABLES:mara.
DATA LINES(5).
DATA:i_headdata             LIKE  bapimathead,
      i_storagelocationdata  LIKE  bapi_mard,
      i_storagelocationdatax LIKE  bapi_mardx,
      i_return               LIKE  bapiret2,
      flag                   TYPE flag.
SELECTION-SCREEN BEGIN OF BLOCK blk WITH FRAME TITLE TEXT-001."定义屏幕
  SELECT-OPTIONS:s_matnr FOR mara-matnr.
SELECTION-SCREEN END OF BLOCK blk.

"下面的参数用于进度条
DATA: ll_lines(10),"行数
      ll_tabix(10),"循环标号
      ll_pecnt     TYPE p LENGTH 6 DECIMALS 2, "百分比
      ll_pecet(6),"百分数
      ll_text(40)."消息
CLEAR: ll_lines,ll_tabix,ll_pecnt,ll_pecet,ll_text.

"判断内表的行数
DESCRIBE TABLE s_matnr LINES LINES.

IF LINES > 5000.
  MESSAGE '数据量过多，超过5000行，请去掉部分数据' TYPE 'S' DISPLAY LIKE 'E'.
  EXIT.
ENDIF.

"获取配置表里面的参数
SELECT
 *
INTO TABLE @DATA(lt_zmmt046)
FROM zmmt046.

"首先先获取物料类型
SELECT
matnr,
mtart
INTO TABLE @DATA(lt_mtart)
FROM mara
FOR ALL ENTRIES IN @s_matnr
WHERE mara~matnr = @s_matnr-low.

"然后接下来获取工厂
SELECT
werks,
matnr
INTO TABLE @DATA(lt_marc)
      FROM marc
      FOR ALL ENTRIES IN @s_matnr
      WHERE marc~matnr = @s_matnr-low.

LOOP AT s_matnr INTO DATA(ls_s_matnr).
  ll_tabix = sy-tabix."当前处理的行号
  ll_pecnt = ll_tabix / LINES * 100."已处理百分比
  ll_pecet = ll_pecnt.
  "拼接显示消息
  CONCATENATE '已完成' ll_pecet '%,共' ll_tabix '条/' LINES '条' INTO ll_text.
  CONDENSE ll_text."整理格式，去掉多余空格，格式更整齐
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR' "调用进度条函数，传入百分比和消息
  EXPORTING
    percentage = ll_pecnt
    TEXT       = ll_text.
  CLEAR: ll_pecnt,ll_pecet,ll_text.
  LOOP AT  lt_marc INTO DATA(ls_marc) WHERE matnr = ls_s_matnr-low.
    SELECT
    mard~matnr,
    werks,
    lgort,
    mtart
    INTO TABLE @DATA(lt_mard)
    FROM mard
    INNER JOIN mara
    ON mard~matnr = mara~matnr
    WHERE mard~matnr = @ls_marc-matnr
    AND mard~werks = @ls_marc-werks.

    READ TABLE lt_mtart INTO DATA(ls_mtart) WITH KEY matnr = ls_s_matnr-low.
    IF sy-subrc = 0.
      LOOP AT lt_zmmt046 INTO DATA(ls_zmmt046) WHERE mtart = ls_mtart-mtart.
        READ TABLE lt_mard INTO DATA(ls_mard) WITH KEY werks = ls_zmmt046-werks
                                                       mtart = ls_mtart-mtart
                                                       lgort = ls_zmmt046-lgort.
        IF sy-subrc <> 0.
          CLEAR i_headdata.
          i_headdata-material = ls_mtart-matnr.
          i_headdata-storage_view = 'X'.
          CLEAR i_storagelocationdata.
          CLEAR i_storagelocationdatax.
          i_storagelocationdata-plant = ls_zmmt046-werks.
          i_storagelocationdatax-plant = ls_zmmt046-werks.
          i_storagelocationdata-stge_loc = ls_zmmt046-lgort.
          i_storagelocationdatax-stge_loc = ls_zmmt046-lgort.

          CALL FUNCTION 'BAPI_MATERIAL_SAVEDATA'
          EXPORTING
            headdata             = i_headdata
            storagelocationdata  = i_storagelocationdata
            storagelocationdatax = i_storagelocationdatax
          IMPORTING
            RETURN               = i_return.
          IF i_return-TYPE = 'S'.
            flag = 'X'.
            COMMIT WORK.
          ELSE.
            flag = 'E'.
          ENDIF.
        ENDIF.
      ENDLOOP.
      CLEAR ls_mtart.
    ENDIF.
    CLEAR ls_marc.
  ENDLOOP.
  CLEAR ls_s_matnr.


ENDLOOP.