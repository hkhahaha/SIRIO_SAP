***INCLUDE MV45AFZZ .

************************************************************************
*                                                                      *
* This include is reserved for user modifications                      *
*                                                                      *
* Forms for sales document processing                                  *
*                                                                      *
* The name of modification modules should begin with 'ZZ'.             *
*                                                                      *
************************************************************************

*---------------------------------------------------------------------*
*       FORM ZZEXAMPLE                                                *
*---------------------------------------------------------------------*
*       text......................................                    *
*---------------------------------------------------------------------*
*FORM ZZEXAMPLE.

*  ...

*ENDFORM.

*eject
*---------------------------------------------------------------------*
*       FORM USEREXIT_DELETE_DOCUMENT                                 *
*---------------------------------------------------------------------*
*       This userexit can be used to delete data in additional tables *
*       when a sales document is deleted.                             *
*                                                                     *
*      This form is called in dialog at the end of form BELEG_LOESCHEN*
*      just before form BELEG_SICHERN is performed to delete the      *
*      datas on the database.                                         *
*                                                                     *
*---------------------------------------------------------------------*
FORM USEREXIT_DELETE_DOCUMENT.

ENDFORM.
*eject

*---------------------------------------------------------------------*
*       FORM USEREXIT_FIELD_MODIFICATION                              *
*---------------------------------------------------------------------*
*       This userexit can be used to modify the attributes of         *
*       screen fields.                                                *
*       This form is processed for each field in the screen.          *
*                                                                     *
*       The use of the fields screen-group1 to screen-group4 is:      *
*                                                                     *
*       Screen-group1: Automatic modification contolles by transaction*
*                      MFAW.                                          *
*       Screen-group2: Contents 'LOO' for steploop-fields.            *
*       Screen-group3: Used for modififaction, which are dependent on *
*                      control tables or other fix information.       *
*       Screen-group4: Unused                                         *
*                                                                     *
*       For field mofifications, which are dependent on the document  *
*       status, you can use the status field in the workareas         *
*       XVBAP for item status and XVBUK for header status.            *
*                                                                     *
*       This form is called from module FELDAUSWAHL.                  *
*                                                                     *
*---------------------------------------------------------------------*
FORM USEREXIT_FIELD_MODIFICATION.

* CASE SCREEN-GROUP3.
*   WHEN '900'.
*     IF VBAK-VBTYP NE IF_SD_DOC_CATEGORY=>INQUIRY.
*       SCREEN-ACTIVE = 0.
*     ENDIF.
* ENDCASE.

* CASE SCREEN-NAME.
*   WHEN 'VBAK-VBELN'.
*     SCREEN-ACTIVE = 0.
* ENDCASE.
*{   INSERT         S4DK900880                                        2
*一审二审订单权限检查不通过灰掉所有字段
  IF sy-tcode EQ 'VA02' AND screen-input EQ 1
    AND NOT ( screen-name EQ 'VBAK-VBELN' OR screen-name EQ 'RV45A-KWMENG' OR screen-name EQ 'VBEP-ETTYP' OR screen-name EQ 'RV45A-ETDAT' OR screen-name = 'VBAP-LGORT' )."黄铠添加RV45A-ETDAT
    CALL FUNCTION 'Z_SD_AUTH_CHECK'
      EXPORTING
        i_vbak       = vbak
      EXCEPTIONS
        check_failed = 1
        OTHERS       = 2.
    IF sy-subrc NE 0.
      screen-input = 0."灰掉屏幕字段
      IF screen-name EQ 'VBAP-ARKTX'."灰掉增减行按钮
        APPEND VALUE #( function = 'POAN' ) TO cua_exclude.
        APPEND VALUE #( function = 'POLO' ) TO cua_exclude.
      ENDIF.
    ENDIF.
  ENDIF.
*黄铠 2020.05.11增加库存地点可编辑
* CASE SCREEN-NAME.
*   WHEN 'VBAP-LGORT'.
*     SCREEN-ACTIVE = 1.
*     SCREEN-OUTPUT = 1.
*     SCREEN-INPUT = 1.
* ENDCASE.
*黄铠 2020.05.11增加库存地点字段的显示，显示在抬头下面行项目里面
  IF SY-tcode = 'VA02'.
    DATA: slcol LIKE LINE OF tctrl_u_erf_auftrag-cols.
    LOOP AT tctrl_u_erf_auftrag-cols INTO slcol.
      IF SLCOL-SCREEN-NAME = 'VBAP-LGORT'.
        slcol-invisible = ''.
        slcol-SCREEN-active = 1.
        slcol-SCREEN-OUTPUT = 1.
        slcol-SCREEN-INPUT = 1.
        MODIFY tctrl_u_erf_auftrag-cols FROM SLCOL.
        CLEAR slcol.
      ENDIF.
    ENDLOOP.
  ENDIF.

*}   INSERT

ENDFORM.
*eject

*---------------------------------------------------------------------*
*       FORM USEREXIT_MOVE_FIELD_TO_VBAK                              *
*---------------------------------------------------------------------*
*       This userexit can be used to move some fields into the sales  *
*       dokument header workaerea VBAK.                               *
*                                                                     *
*       SVBAK-TABIX = 0:  Create header                               *
*       SVBAK-TABIX > 0:  Change header                               *
*                                                                     *
*       This form is called at the end of form VBAK_FUELLEN.          *
*                                                                     *
*---------------------------------------------------------------------*
FORM USEREXIT_MOVE_FIELD_TO_VBAK.

*  vbak-zzfield = xxxx-zzfield2.

ENDFORM.
*eject

*---------------------------------------------------------------------*
*       FORM USEREXIT_MOVE_FIELD_TO_VBAP                              *
*---------------------------------------------------------------------*
*       This userexit can be used to move some fields into the sales  *
*       dokument item workaerea VBAP                                  *
*                                                                     *
*       SVBAP-TABIX = 0:  Create item                                 *
*       SVBAP-TABIX > 0:  Change item                                 *
*                                                                     *
*       This form is called at the end of form VBAP_FUELLEN.          *
*                                                                     *
*---------------------------------------------------------------------*
FORM USEREXIT_MOVE_FIELD_TO_VBAP.

*  VBAP-zzfield = xxxx-zzfield2.

ENDFORM.
*eject

*---------------------------------------------------------------------*
*       FORM USEREXIT_MOVE_FIELD_TO_VBEP                              *
*---------------------------------------------------------------------*
*       This userexit can be used to move some fields into the sales  *
*       dokument schedule line workaerea VBEP                         *
*                                                                     *
*       SVBEP-TABIX = 0:  Create schedule line                        *
*       SVBEP-TABIX > 0:  Change schedule line                        *
*                                                                     *
*       This form is called at the end of form VBEP_FUELLEN.          *
*                                                                     *
*---------------------------------------------------------------------*
FORM USEREXIT_MOVE_FIELD_TO_VBEP.

*  VBEP-zzfield = xxxx-zzfield2.

ENDFORM.
*eject

*---------------------------------------------------------------------*
*       FORM USEREXIT_MOVE_FIELD_TO_VBKD                              *
*---------------------------------------------------------------------*
*       This userexit can be used to move some fields into the sales  *
*       dokument business data workaerea VBKD                         *
*                                                                     *
*       SVBKD-TABIX = 0:  Create data                                 *
*       SVBKD-TABIX > 0:  Change data                                 *
*                                                                     *
*       This form is called at the end of form VBKD_FUELLEN.          *
*                                                                     *
*---------------------------------------------------------------------*
FORM USEREXIT_MOVE_FIELD_TO_VBKD.

*  VBKD-zzfield = xxxx-zzfield2.

ENDFORM.
*eject

*---------------------------------------------------------------------*
*       FORM USEREXIT_NUMBER_RANGE                                    *
*---------------------------------------------------------------------*
*       This userexit can be used to determine the numberranges for   *
*       the internal document number.                                 *
*                                                                     *
*       US_RANGE_INTERN - internal number range                       *
*                                                                     *
*       This form is called from form BELEG_SICHERN                   *
*                                                                     *
*---------------------------------------------------------------------*
FORM USEREXIT_NUMBER_RANGE USING US_RANGE_INTERN.

* Example: Numer range from TVAK like in standard
* US_RANGE_INTERN = TVAK-NUMKI.

ENDFORM.
*eject

*---------------------------------------------------------------------*
*       FORM USEREXIT_PRICING_PREPARE_TKOMK                           *
*---------------------------------------------------------------------*
*       This userexit can be used to move additional fields into the  *
*       communication table which is used for pricing:                *
*                                                                     *
*       TKOMK for header fields                                       *
*                                                                     *
*       This form is called from form PREISFINDUNG_VORBEREITEN.       *
*                                                                     *
*---------------------------------------------------------------------*
FORM USEREXIT_PRICING_PREPARE_TKOMK.

*  TKOMK-zzfield = xxxx-zzfield2.

ENDFORM.
*eject

*---------------------------------------------------------------------*
*       FORM USEREXIT_PRICING_PREPARE_TKOMP                           *
*---------------------------------------------------------------------*
*       This userexit can be used to move additional fields into the  *
*       communication table which is used for pricing:                *
*                                                                     *
*       TKOMP for item fields                                         *
*                                                                     *
*       This form is called from form PREISFINDUNG_VORBEREITEN.       *
*                                                                     *
*---------------------------------------------------------------------*
FORM USEREXIT_PRICING_PREPARE_TKOMP.

*  TKOMP-zzfield = xxxx-zzfield2.

ENDFORM.
*eject

*---------------------------------------------------------------------*
*       FORM USEREXIT_READ_DOCUMENT                                   *
*---------------------------------------------------------------------*
*       This userexit can be used to read data in additional tables   *
*       when the program reads a sales document.                      *
*                                                                     *
*       This form is called at the end of form BELEG_LESEN.           *
*                                                                     *
*---------------------------------------------------------------------*
FORM USEREXIT_READ_DOCUMENT.

ENDFORM.
*eject

*---------------------------------------------------------------------*
*       FORM USEREXIT_SAVE_DOCUMENT                                   *
*---------------------------------------------------------------------*
*       This userexit can be used to save data in additional tables   *
*       when a document is saved.                                     *
*                                                                     *
*       If field T180-TRTYP contents 'H', the document will be        *
*       created, else it will be changed.                             *
*                                                                     *
*       This form is called at from form BELEG_SICHERN, before COMMIT *
*                                                                     *
*---------------------------------------------------------------------*
FORM USEREXIT_SAVE_DOCUMENT.

* Example:
* CALL FUNCTION 'ZZ_EXAMPLE'
*      IN UPDATE TASK
*      EXPORTING
*{   INSERT         S4DK900160                                        1
*订单一审二审增强_修改计划行类别
*  CALL FUNCTION 'Z_SD_ORDER_FIRSTIST_ENHANCE' STARTING NEW TASK 'ZSD018'
*    EXPORTING
*      e_vbak        = vbak
*      e_sy_tcode    = sy-tcode
*      e_asttx       = rv45a-asttx
*    TABLES
*      t_xvbap       = XVBAP[]
*      t_xvbep       = XVBEP[].
          .
************  VA01 / VA02 / MD81 下发SO 给APS
* DATA: LS_VBAK  TYPE VBAK,
*       LV_FLG TYPE C,
*       LV_SECONDS TYPE I,
*       LV_VBELN TYPE VBAK-VBELN.

*      IF SY-TCODE = 'VA01' OR SY-TCODE = 'VA02' OR SY-TCODE = 'MD81' OR SY-TCODE = 'MD82'.
*             WHILE  LV_FLG <> 'X'.
*                WAIT UP TO 1 SECONDS.
*                LV_SECONDS = LV_SECONDS + 1.
*                 SELECT SINGLE VBELN  INTO LV_VBELN FROM VBAK
*                   WHERE VBELN = VBAK-VBELN.
*                   IF SY-SUBRC  = 0.
*                      LV_FLG = 'X'.
*                   ENDIF.
*                 IF LV_SECONDS = 30.
*                      exit." 30 秒超时退出循环
*                 ENDIF.
*             ENDWHILE.
*
*               CHECK LS_VBAK IS NOT INITIAL.

               CALL FUNCTION 'Z_SD_APS_SO' STARTING NEW TASK 'TSK_SO'
                 EXPORTING
                   I_VBAK  = vbak.
*      ENDIF.


*}   INSERT
*           ZZTAB = ZZTAB.

ENDFORM.
*eject
*---------------------------------------------------------------------*
*       FORM USEREXIT_SAVE_DOCUMENT_PREPARE                           *
*---------------------------------------------------------------------*
*       This userexit can be used for changes or checks, before a     *
*       document is saved.                                            *
*                                                                     *
*       If field T180-TRTYP contents 'H', the document will be        *
*       created, else it will be changed.                             *
*                                                                     *
*       This form is called at the beginning of form BELEG_SICHERN    *
*                                                                     *
*---------------------------------------------------------------------*
FORM USEREXIT_SAVE_DOCUMENT_PREPARE.
*{   INSERT         S4DK900160                                        1
*销售订单的客户资质和产品资质检查
    DATA: BEGIN OF ls_valuetab,
        valuetab TYPE bapi_msg,
      END OF   ls_valuetab.
  DATA:ls_return TYPE BAPIRET1,
       lt_warning_msg TYPE TABLE OF BAPIRET1,
       lv_choise TYPE SY-TABIX,
       lt_valuetab like TABLE OF ls_valuetab.

  CALL FUNCTION 'Z_SD_SO_CHECK_QUALIFICATION'
    EXPORTING
      is_vbak         = vbak
   IMPORTING
     IS_RETURN       = ls_return
    tables
      it_vbap        = xvbap[]
      it_vbpa        = xvbpa[]
      IT_WARNING_MSG = lt_WARNING_MSG.

  IF ls_return-type = 'E'.
      GF_EXIT_SAVE_DOCUMENT_PREPARE = 'X'. "取消保存标识
      MESSAGE e000(ZSD) WITH '资质检查失败,错误消息为：'  ls_return-message.
      ELSE.
        CLEAR lt_valuetab.
        IF lt_WARNING_MSG is NOT INITIAL.
          LOOP AT lt_warning_msg INTO DATA(ls_warning_msg).
            lt_valuetab = VALUE #( BASE lt_valuetab ( valuetab = ls_warning_msg-message ) ).
          ENDLOOP.

        CALL FUNCTION 'POPUP_TABLE_DISPLAY'
          EXPORTING
            endpos_col         = 100
            endpos_row         = 20
            startpos_col       = 50
            startpos_row       = 5
            titletext          = '资质检查有效期检查'
         IMPORTING
          CHOISE             = lv_CHOISE
          TABLES
            valuetab           = lt_valuetab
         EXCEPTIONS
           BREAK_OFF          = 1
           OTHERS             = 2
                  .
        IF lv_choise = 0.
          MESSAGE '您已取消保存单据' TYPE 'E' DISPLAY LIKE 'S'.

        ENDIF.

       ENDIF.
  ENDIF.


CALL FUNCTION 'Z_SD_SALESORDER_ENHANCE_ETTYP'
  EXPORTING
    e_vbak        = vbak
    e_asttx       = rv45a-asttx
  TABLES
    t_xvbep       = XVBEP[]
    t_xvbbe       = XVBBE[]
    t_xvbap       = XVBAP[].

*更新vbap增强字段
  CALL FUNCTION 'Z_SD_VBAP_SAVE'
    EXPORTING
      is_vbak       = vbak
      it_vbep       = xvbep[]
    CHANGING
      ct_vbap       = xvbap[].

** 销售订单下发给APS
* DATA: LS_RET TYPE BAPIRET2.
* CALL FUNCTION 'Z_SD_APS_SO'  STARTING NEW TASK 'TSK_SO'
*   EXPORTING
*     I_VBAK         = VBAK
*   TABLES
*     T_XVBAP        = XVBAP[]
*     T_XVBEP        = XVBEP[]
           .
*}   INSERT


ENDFORM.
*eject
