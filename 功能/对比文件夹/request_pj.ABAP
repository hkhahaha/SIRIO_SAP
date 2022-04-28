METHOD set_lims_request_pj.
    DATA:ls_zbatch_sign_dto TYPE zbatch_sign_dto,
         lt_batch_sign_dto  TYPE zbatch_sign_dto_tab.
    CLEAR es_output.
    IF it_zppt0003 IS NOT INITIAL.
      "流程订单累计收货
*      SELECT aufnr,wemng FROM afpo FOR ALL ENTRIES IN @it_zppt0003
*        WHERE aufnr = @it_zppt0003-aufnr
*        INTO TABLE @DATA(lt_afpo).
      "检验批次单号,排除批量取消的检验批次
      SELECT qa~aufnr,qa~prueflos FROM qals AS qa FOR ALL ENTRIES IN @it_zppt0003
        WHERE qa~aufnr = @it_zppt0003-aufnr
        AND qa~art <> 'G04' "G04为组合装入库检验类型
       AND EXISTS (  SELECT stat FROM jest WHERE objnr = qa~objnr AND  ( stat <> 'I0224' AND  inact = '' ) )
        INTO TABLE @DATA(lt_qals).
      LOOP AT it_zppt0003 INTO DATA(ls_zppt0003) WHERE zdelflg IS INITIAL.
        CLEAR ls_zbatch_sign_dto.
        READ TABLE lt_qals WITH KEY aufnr = ls_zppt0003-aufnr INTO DATA(ls_qals).
        CHECK sy-subrc = 0.
        ls_zbatch_sign_dto-prueflos = ls_qals-prueflos."检验批次号
        ls_zbatch_sign_dto-aufnr = ls_zppt0003-aufnr."流程订单号
        ls_zbatch_sign_dto-zwmsmng = ls_zppt0003-menge + ls_zppt0003-wemng."本次入库申请数量 + 已入库
        ls_zbatch_sign_dto-wempf = ls_zppt0003-wempf."结案信息
        ls_zbatch_sign_dto-zmeins_t = ls_zppt0003-meins."基本计量单位
        APPEND ls_zbatch_sign_dto TO lt_batch_sign_dto.
      ENDLOOP.
      es_output-user = lv_user.
      es_output-pwd = lv_pwd.
      es_output-arr_parameters-batch_sign_dto = lt_batch_sign_dto.
    ENDIF.
  ENDMETHOD.