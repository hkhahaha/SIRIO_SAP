*&---------------------------------------------------------------------*
*& 包含               ZMMR0020F01
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Form FRM_INIT
*&---------------------------------------------------------------------*
*& 初始化
*&---------------------------------------------------------------------*
FORM frm_init .

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FRM_CHANGE_SCREEN
*&---------------------------------------------------------------------*
*       修改选择屏幕
*----------------------------------------------------------------------*
FORM frm_change_screen .

ENDFORM.

*&---------------------------------------------------------------------*
*& Form FRM_AUTHORITY_CHECK
*&---------------------------------------------------------------------*
*& 权限检查
*&---------------------------------------------------------------------*
FORM frm_authority_check .

  AUTHORITY-CHECK OBJECT 'Q_INSPLOT'
   ID 'WERKS' FIELD s_werk-low.
  IF sy-subrc <> 0.
    MESSAGE s002(zauthority) DISPLAY LIKE 'E' WITH '工厂' s_werk-low.
    STOP.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form FRM_GET_DATA
*&---------------------------------------------------------------------*
*& 获取数据
*&---------------------------------------------------------------------*
FORM frm_get_data.

  DATA: lt_status TYPE TABLE OF jstat.

  SELECT  qals~prueflos
          qals~objnr
          qals~werk AS werks
          qals~pastrterm
          qals~paendterm
          afpo~aufnr
          afpo~kdauf
          qals~matnr
          qals~lmengeist
          qals~mengeneinh
          qals~charg
    INTO CORRESPONDING FIELDS OF TABLE gt_alv
    FROM qals
    JOIN afpo
      ON afpo~aufnr = qals~aufnr
    JOIN mara
      ON mara~matnr = qals~matnr
   WHERE qals~prueflos  IN s_pruef
     AND afpo~kdauf     IN s_kdauf
     AND qals~matnr     IN s_matnr
     AND qals~pastrterm IN s_pastr
     AND qals~paendterm IN s_paend
     AND qals~charg     IN s_charg
     AND qals~werk      IN s_werk
     AND mara~mtart     = 'Z050'
  .

  LOOP AT gt_alv INTO DATA(ls_alv).
    DATA(lv_tabix) = sy-tabix.
    CALL FUNCTION 'STATUS_READ'
      EXPORTING
        objnr            = ls_alv-objnr
        only_active      = 'X'
      TABLES
        status           = lt_status
      EXCEPTIONS
        object_not_found = 1
        OTHERS           = 2.
    READ TABLE lt_status TRANSPORTING NO FIELDS WITH KEY stat = 'I0224'. "'LTCA'.
    CHECK sy-subrc = 0.
    DELETE gt_alv INDEX lv_tabix.
    CLEAR ls_alv.              "20191031waymon
  ENDLOOP.

  IF gt_alv[] IS INITIAL.
    MESSAGE TEXT-e01 TYPE 'S' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form FRM_PROCESS_DATA
*&---------------------------------------------------------------------*
*& 处理数据
*&---------------------------------------------------------------------*
FORM frm_process_data.

*处理检验批数据
  PERFORM frm_process_data_jyp.
*获取检验结果
  PERFORM frm_get_jyjg.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form FRM_PROCESS_DATA_JYP
*&---------------------------------------------------------------------*
*& 处理检验批数据
*&---------------------------------------------------------------------*
FORM frm_process_data_jyp .

**批次属性
  DATA(lt_alv) = gt_alv[].
  SORT lt_alv BY matnr .
  DELETE ADJACENT DUPLICATES FROM lt_alv COMPARING matnr.
  SELECT a~objek AS matnr, a~atwrt
    FROM @lt_alv AS alv
    JOIN ausp AS a
      ON a~objek = alv~matnr
    JOIN cabn AS c
      ON a~atinn = c~atinn
   WHERE a~klart = '001'
     AND a~mafid = 'O'
     AND c~atnam = 'ZPSPE'
    INTO TABLE @DATA(lt_ausp).

*出口国家 / 客户名称
  SELECT alv~kdauf, k~kunnr, k~zckgj
    FROM @gt_alv AS alv
    JOIN vbak AS k
      ON k~vbeln = alv~kdauf
    INTO TABLE @DATA(lt_vbak).

*客户名称
  SELECT a~kunnr, a~name1
    FROM @lt_vbak AS k
    JOIN kna1 AS a
      ON a~kunnr = k~kunnr
    INTO TABLE @DATA(lt_kna1).

*出口国家
  SELECT k~zckgj, t~landx
    FROM @lt_vbak AS k
    JOIN t005t AS t
      ON t~land1 = k~zckgj
     AND t~spras = @sy-langu
    INTO TABLE @DATA(lt_t005t).

*半成品编码
  lt_alv = gt_alv[].
  SORT lt_alv BY aufnr .
  DELETE ADJACENT DUPLICATES FROM lt_alv COMPARING aufnr.
  SELECT m~aufnr, m~matnr, m~charg
    FROM @lt_alv AS alv
    JOIN mseg AS m
      ON m~aufnr = alv~aufnr
    JOIN mara AS a
      ON a~matnr = m~matnr
   WHERE m~bwart = '261'
     AND a~mtart = 'Z030'
    INTO TABLE @DATA(lt_mseg).

  SORT lt_ausp  BY matnr.
  SORT lt_mseg  BY aufnr.
  SORT lt_vbak  BY kdauf.
  SORT lt_kna1  BY kunnr.
  SORT lt_t005t BY zckgj.
  DELETE ADJACENT DUPLICATES FROM lt_t005t COMPARING zckgj.

  LOOP AT gt_alv ASSIGNING FIELD-SYMBOL(<fs_alv>).
*出口国家
    READ TABLE lt_vbak INTO DATA(ls_vbak) WITH KEY kdauf = <fs_alv>-kdauf BINARY SEARCH.
    IF sy-subrc = 0.
      <fs_alv>-zckgj = ls_vbak-zckgj.
      <fs_alv>-kunnr = ls_vbak-kunnr.
    ENDIF.

*客户名称
    READ TABLE lt_kna1 INTO DATA(ls_kna1) WITH KEY kunnr = <fs_alv>-kunnr BINARY SEARCH.
    IF sy-subrc = 0.
      <fs_alv>-name1 = ls_kna1-name1.
    ENDIF.

*出口国家
    READ TABLE lt_t005t INTO DATA(ls_t005t) WITH KEY zckgj = <fs_alv>-zckgj BINARY SEARCH.
    IF sy-subrc = 0.
      <fs_alv>-landx = ls_t005t-landx.
    ENDIF.

*半成品编码 / 工艺批号
    READ TABLE lt_mseg INTO DATA(ls_mseg) WITH KEY aufnr = <fs_alv>-aufnr BINARY SEARCH.
    IF sy-subrc = 0.
      <fs_alv>-matnr_bcp = ls_mseg-matnr.
      <fs_alv>-charg_bcp = ls_mseg-charg.
    ENDIF.

*批次属性
    READ TABLE lt_ausp INTO DATA(ls_ausp) WITH KEY matnr = <fs_alv>-matnr BINARY SEARCH.
    IF sy-subrc = 0.
      <fs_alv>-zpspe = ls_ausp-atwrt.
    ENDIF.

  ENDLOOP.

*物料名称
  SELECT t~matnr, t~maktx
    FROM @gt_alv AS alv
    JOIN makt AS t
      ON t~matnr = alv~matnr
      OR t~matnr = alv~matnr_bcp
   WHERE t~spras = @sy-langu
    INTO TABLE @DATA(lt_makt).

  SORT lt_makt BY matnr.

  LOOP AT gt_alv ASSIGNING <fs_alv>.

*产品名称
    READ TABLE lt_makt INTO DATA(ls_makt) WITH KEY matnr = <fs_alv>-matnr BINARY SEARCH.
    IF sy-subrc = 0.
      <fs_alv>-maktx = ls_makt-maktx.
    ENDIF.

*半成品名称
    READ TABLE lt_makt INTO ls_makt WITH KEY matnr = <fs_alv>-matnr_bcp BINARY SEARCH.
    IF sy-subrc = 0.
      <fs_alv>-maktx_bcp = ls_makt-maktx.
    ENDIF.

  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form FRM_GET_JYJG
*&---------------------------------------------------------------------*
*& 获取检验结果
*&---------------------------------------------------------------------*
FORM frm_get_jyjg .

  DATA lt_alv LIKE gt_alv.

*检验项目
  SELECT v~prueflos, v~vorglfnr, v~merknr, q~charg, k~sortfeld
    FROM @gt_alv AS alv
    JOIN qals AS q
      ON q~prueflos = alv~prueflos
      OR ( q~charg  = alv~charg_bcp AND q~charg <> '' )
    JOIN qamv AS v
      ON v~prueflos = q~prueflos
    JOIN qpmk AS k
      ON k~zaehler = v~qpmk_werks
     AND k~mkmnr   = v~verwmerkm
     AND k~version = v~mkversion
    INTO TABLE @DATA(lt_qamv).

  SORT lt_qamv BY prueflos vorglfnr merknr.
  DELETE ADJACENT DUPLICATES FROM lt_qamv COMPARING prueflos vorglfnr merknr.

*结果
*  SELECT v~prueflos, v~vorglfnr, v~merknr, q~charg, r~mittelwert, r~pruefbemkt, r~mbewertg
*    FROM @gt_alv AS alv
*    JOIN qals AS q
*      ON q~prueflos = alv~prueflos
*      OR q~charg    = alv~charg_bcp
*    JOIN qamv AS v
*      ON v~prueflos = q~prueflos
*    JOIN qamr AS r
*      ON r~prueflos = v~prueflos
*     AND r~vorglfnr = v~vorglfnr
*     AND r~merknr   = v~merknr
*    INTO TABLE @DATA(lt_qamr).
*
*  SORT lt_qamr BY prueflos vorglfnr merknr.
*  DELETE ADJACENT DUPLICATES FROM lt_qamr COMPARING prueflos vorglfnr merknr.

*结果 waymon 20191029
  SELECT v~prueflos, v~vorglfnr, v~merknr, q~charg, r~mbewertg, r~satzstatus,r~original_input,r~pruefbemkt
    FROM @gt_alv AS alv
    JOIN qals AS q
      ON q~prueflos = alv~prueflos
      OR q~charg    = alv~charg_bcp
    JOIN qamv AS v
      ON v~prueflos = q~prueflos
    JOIN qasr AS r
      ON r~prueflos = v~prueflos
     AND r~vorglfnr = v~vorglfnr
     AND r~merknr   = v~merknr
    INTO TABLE @DATA(lt_qasr).

  SORT lt_qasr BY prueflos vorglfnr merknr.
  DELETE ADJACENT DUPLICATES FROM lt_qasr COMPARING prueflos vorglfnr merknr.

*取半成品检验批 waymon 20191031

  SELECT q~prueflos,q~charg,q~matnr
     FROM @gt_alv AS alv
    JOIN qals AS q
    ON q~charg = alv~charg_bcp
    AND q~matnr = alv~matnr_bcp
    INTO TABLE @DATA(lt_bcp).

*取半成品检验批 waymon 20191031  end


  LOOP AT gt_alv INTO gs_alv.


    LOOP AT lt_qamv INTO DATA(ls_qamv) WHERE prueflos = gs_alv-prueflos OR charg = gs_alv-charg_bcp.
      DATA(ls_alv) = gs_alv.
      ls_alv-sortfeld     = ls_qamv-sortfeld.


      IF ls_qamv-prueflos <> gs_alv-prueflos.    "waymon 20191031

        ls_alv-prueflos_bcp = ls_qamv-prueflos.   "waymon 20191031

      ENDIF.

*      READ TABLE lt_bcp INTO DATA(ls_bcp) WITH KEY matnr = gs_alv-matnr_bcp
*                                                   charg = gs_alv-charg_bcp.   "waymon 20191031
*      ls_alv = gs_alv.                                                                             "waymon 20191031
*      ls_alv-prueflos_bcp = ls_bcp-prueflos.                                                    "waymon 20191031


      READ TABLE lt_qasr INTO DATA(ls_qasr) WITH KEY prueflos = ls_qamv-prueflos
                                                     vorglfnr = ls_qamv-vorglfnr
                                                     merknr = ls_qamv-merknr
                                                     BINARY SEARCH.
      IF sy-subrc = 0.

        IF ls_qasr-original_input = ''.
          ls_alv-jg = ls_qasr-pruefbemkt.
        ELSE.
          ls_alv-jg = ls_qasr-original_input.
        ENDIF.
*        ENDIF.

        ls_alv-mbewertg = ls_qasr-mbewertg.

*显示评价的文本 waymon 20191029 start
        IF  ls_alv-mbewertg = ''.
          ls_alv-pjzw = '没有评估'.
        ELSEIF ls_alv-mbewertg = 'A'.
          ls_alv-pjzw = '已接受'.
        ELSEIF ls_alv-mbewertg = 'F'.
          ls_alv-pjzw = '失败'.
        ELSEIF ls_alv-mbewertg = 'R'.
          ls_alv-pjzw = '已拒绝'.
        ENDIF.
*显示评价的文本 waymon 20191029 end

      ENDIF.
      APPEND ls_alv TO lt_alv.
*      IF gs_alv-matnr_bcp IS NOT INITIAL.
*        APPEND ls_alv TO lt_alv.
**      else.
**        APPEND ls_alv to lt_alv.
*      ENDIF.
*      CLEAR: ls_alv.
*      CLEAR ls_qamv.

    ENDLOOP.
    IF sy-subrc <> 0 OR gs_alv-matnr_bcp IS INITIAL.


      APPEND gs_alv TO lt_alv.
      CLEAR ls_qamv.
      CLEAR gs_alv.
    ENDIF.
  ENDLOOP.

  CLEAR: gt_alv[].
  DELETE lt_alv WHERE sortfeld IS INITIAL.
  gt_alv[] = lt_alv[].
  FREE lt_alv[].

ENDFORM.