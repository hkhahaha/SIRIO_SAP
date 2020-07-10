METHOD zqm_ifis_qr.

    "物料特征字段结构定义
    DATA: ltoutput   TYPE TABLE OF zmaterial_tx_value.
    DATA: lt_tline TYPE TABLE OF tline.
    DATA: lt_zppt0003 TYPE TABLE OF zppt0003.
    DATA: ls_zppt0003 TYPE zppt0003.

    IF sy-tcode = 'QA11' OR sy-tcode = 'QA12'.
      " 获取物料特征
      CALL FUNCTION 'Z_FI_GET_MATERIAL_VALUE'
        EXPORTING
          matnr  = new_insplot-matnr
*         ATINN  =
        TABLES
          output = ltoutput.
*        " 判断是否二维码物料。
      READ TABLE  ltoutput WITH KEY atnam = 'ZQR' INTO DATA(ls_output).
      IF sy-subrc = 0.
*        " 判断是否审核
        SELECT
  aufnr,matnr,charg,zqrscan,qropersts
        INTO CORRESPONDING FIELDS OF TABLE @lt_zppt0003
             FROM zppt0003
             WHERE matnr = @new_insplot-matnr
             AND charg = @new_insplot-charg
             AND aufnr = @new_insplot-aufnr
             AND lgort = @new_insplot-lagortchrg.

        LOOP AT lt_zppt0003 INTO ls_zppt0003.

          IF ls_zppt0003-qropersts NE '已审批'.

            MESSAGE w001(00) WITH '二维码未完成审核!'.

          ENDIF.
        ENDLOOP.


      ENDIF.

      " 判断样本字段有值时，如果检验批流程订单字段值不为空，报错

      IF new_insplot-lmenge03 IS NOT INITIAL.

        IF new_insplot-aufnr IS NOT INITIAL.
          MESSAGE e001(00) WITH '带有订单的检验批不能使用331移动类型做样本消耗!'.

        ENDIF.
      ENDIF.
    ENDIF.

***************Seashell 增加检验批物料资料是否齐全检查
    CALL FUNCTION 'Z_QM_INFO_IF'
      EXPORTING
        new_insplot = new_insplot.
*******************end
  ENDMETHOD.