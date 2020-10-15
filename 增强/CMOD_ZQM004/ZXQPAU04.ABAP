*&---------------------------------------------------------------------*
*& 包含               ZXQPAU04
*&---------------------------------------------------------------------*

DATA:ls_qals  TYPE qals,
     ls_mara  TYPE mara,
     ls_mapl  TYPE maplb,
     ls_plko  TYPE plkob,
     ls_afpo  TYPE afpo,
     ls_aufk  TYPE aufk,
     ls_vbak  TYPE vbak,
     lt_maplb TYPE maplb_tt.

CLEAR:ls_qals,ls_mapl,ls_plko,ls_afpo,ls_aufk,ls_vbak,lt_maplb.

****检查检验计划如果大于1条 则按照以下逻辑处理

CHECK lines( t_maplb ) > 1.

IMPORT  a = ls_qals  FROM MEMORY ID 'ZQM01_QALS'.

SELECT SINGLE * FROM mara INTO ls_mara
  WHERE matnr = ls_qals-matnr.



CHECK sy-subrc = 0.

CASE  ls_mara-mtart.

  WHEN 'Z010' OR 'Z020'.

****首先判断检验计划是否能匹配到库存地，如果匹配不到，排除搜索字段不为空时，
****如果只有一个检验计划则匹配

    READ TABLE t_maplb INTO ls_mapl WITH KEY suchfeld =  ls_qals-lagortchrg.

    IF sy-subrc = 0.

      DELETE  t_maplb WHERE plnty <> ls_mapl-plnty
                    OR plnnr <> ls_mapl-plnnr
                    OR plnal <> ls_mapl-plnal
                    OR zaehl <> ls_mapl-zaehl.
    ELSE.

      lt_maplb = t_maplb[].

      DELETE lt_maplb WHERE  suchfeld <> space.

      CHECK lines( lt_maplb ) = 1.

      t_maplb[] = lt_maplb.
    ENDIF.


******如果是成品则在VBAK中读取订单的相关信息
******如果是S码，则通过内存变量ZAPS_VBELN读取订单的相关信息
******在程序Z_PP_PI_PROCESS_ORDER_CREATE里用到

  WHEN  'Z030' OR 'Z050'.

    IF ls_mara-mtart = 'Z030'.

      CLEAR ls_qals-kdauf.
      GET PARAMETER ID 'ZAPS_VBELN' FIELD ls_qals-kdauf.

      "20200116 waymon top. 如果内存变量不存在，判断工单表。
      IF ls_qals-kdauf IS INITIAL.

        IF sy-tcode = 'QA02' OR sy-tcode = 'QA01'.

          "20200116 top waymon.
          SELECT SINGLE * FROM aufk INTO ls_aufk
            WHERE aufnr = ls_qals-aufnr.
          "20200116 end waymon.

          ls_qals-kdauf = ls_aufk-zkdauf_s.

        ENDIF.

      ENDIF.

      "20200116 waymon end.
    ENDIF.

    SELECT SINGLE * INTO ls_vbak
      FROM vbak
         WHERE vbeln = ls_qals-kdauf.

    CHECK sy-subrc = 0.
****优先判断客户，之后判断地区，如果都没有则读取客户跟搜索项为空的那个
    READ TABLE t_maplb INTO ls_mapl WITH KEY kunr =  ls_vbak-kunnr.

    IF sy-subrc = 4." 20200115 top modify by waymon.

      DELETE t_maplb WHERE  kunr <> space.

    ENDIF." 20200115 end modify by waymon.

    READ TABLE t_maplb INTO ls_mapl WITH KEY kunr =  ls_vbak-kunnr." 20200115 modify by waymon.

    IF sy-subrc = 0.

      DELETE  t_maplb WHERE plnty <> ls_mapl-plnty
                         OR plnnr <> ls_mapl-plnnr
                         OR plnal <> ls_mapl-plnal
                         OR zaehl <> ls_mapl-zaehl.
    ELSE.

      READ TABLE t_maplb INTO ls_mapl WITH KEY suchfeld =  ls_vbak-zckgj.

      IF sy-subrc = 0.

        DELETE  t_maplb WHERE plnty <> ls_mapl-plnty
                      OR plnnr <> ls_mapl-plnnr
                      OR plnal <> ls_mapl-plnal
                      OR zaehl <> ls_mapl-zaehl.

      ELSE.

        lt_maplb = t_maplb[].

        DELETE lt_maplb WHERE  kunr <> space OR suchfeld <> space.

        CHECK lines( lt_maplb ) = 1.

        t_maplb[] = lt_maplb.

      ENDIF.
    ENDIF.

  WHEN OTHERS.

ENDCASE.

READ TABLE t_maplb INTO ls_mapl INDEX 1.

CHECK sy-subrc = 0.

DELETE  t_plkob WHERE plnty <> ls_mapl-plnty
                   OR plnnr <> ls_mapl-plnnr
                   OR plnal <> ls_mapl-plnal.


DELETE  t_maplb WHERE plnty <> ls_mapl-plnty
                   OR plnnr <> ls_mapl-plnnr
                   OR plnal <> ls_mapl-plnal
                   OR zaehl <> ls_mapl-zaehl.