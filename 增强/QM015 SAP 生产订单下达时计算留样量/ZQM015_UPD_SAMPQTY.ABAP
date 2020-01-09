FUNCTION zqm015_upd_sampqty.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     REFERENCE(I_QALS) TYPE  QALS
*"  EXPORTING
*"     REFERENCE(E_QALS) TYPE  QALS
*"     REFERENCE(E_ACTIVE) TYPE  QM00-QKZ
*"----------------------------------------------------------------------
  MOVE i_qals TO e_qals.
  DATA:wa_qm002 TYPE zqmt002, "用于更新QALS.
       t_qm002  TYPE STANDARD TABLE OF zqmt002, "获取同类标识的取样数据
       wa_temp  TYPE zqmt002. "
  DATA:l_cq TYPE zqmt002-zcqyly,
       l_js TYPE zqmt002-zcqyly,
       l_dq TYPE zqmt002-zcqyly,
       l_nd TYPE zqmt002-zcqyly.
  SELECT SINGLE * INTO wa_qm002
    FROM zqmt002
    WHERE matnr = i_qals-matnr
    AND   werks = i_qals-werk.
  CHECK wa_qm002 IS NOT INITIAL.
  SELECT * INTO TABLE t_qm002
   FROM zqmt002
    WHERE werks = i_qals-werk
    AND   zsam_flg =   wa_qm002-zsam_flg.
  CHECK t_qm002 IS NOT INITIAL.
  LOOP AT t_qm002 INTO wa_temp  .
    l_cq = l_cq + wa_temp-zcqyly.
    l_dq = l_dq + wa_temp-zdqylyly.
    l_js = l_js + wa_temp-zjsylyly.
    l_nd = l_nd + wa_temp-zndylyly.
  ENDLOOP.
  IF wa_temp-zcqlygz > l_cq.
    e_qals-zcqwdxly = wa_qm002-zcqwdxly.
    wa_qm002-zcqyly =   wa_qm002-zcqyly + 1.
  ENDIF.

  IF wa_temp-zjslygz > l_js.
    e_qals-zjswdxly = wa_qm002-zjswdxly.
    wa_qm002-zjsylyly =   wa_qm002-zjsylyly + 1.
  ENDIF.

  IF wa_temp-zdqlygz > l_dq.
    e_qals-zdqwdxly = wa_qm002-zdqwdxly.
    wa_qm002-zdqylyly =   wa_qm002-zdqylyly + 1.
  ENDIF.

  IF wa_temp-zndlygz > l_nd.
    e_qals-zndwjly = wa_qm002-zndwjly.
    wa_qm002-zndylyly = wa_qm002-zndylyly + 1.
  ENDIF.
  E_ACTIVE = 'X'.
  e_qals-zmeins = wa_qm002-meins.
  MODIFY zqmt002 FROM wa_qm002 .

ENDFUNCTION.