************************************************************************
*     REPORT RM07MLBS   (Transaction MB52)
*
*     History:
* Aug 2013 EH                                               "n1895376
* - corrected display val. blocked stock for E/Q non-batch  "n1895376
* - no value for V,W,O in special circumstances             "n1895376
* - some perfomance optimization by replacing reads         "n1895376

* Jan 2013 EH                                               "n1795093
* - optimzed DB access/ new select statement for stocks     "n1795093
*
* new function Nov 2012 MR                                  "n1710852
* - added secondary database connection                     "n1710852
*
* Dec 2009 MM : errors fixed which were reported by CHECKMAN  "09122009
*
* February 2009 RR                                          "1311282
* added field LGOBE into output list with parameter         "1311282
* "c-no-out" in form F0300_FIELDCAT_FLAT.                   "1311282
*
* July 2006 MS                                              "n960980
* changed output of first line in print modus. If you enter "n960980
* user commands after function print, you get the list in   "n960980
* print modus view.                                         "n960980
*
* Jan. 2006 MS                                              "n912093
* added fields GLGMG,WGLGM,UMLMC,WUMLC,TRAME,WTRAM          "n912093
* into output list with parameter "c-no-out"                "n912093
* corrected documentation for field "Stock in transfer"     "n912093
* down with release 6.0                                     "n912093
* added dynamic break-points from cp-group MMIM_REP_MB52    "n912093

* März 2005 TW                                              "n829722
* authorization check should be always processed            "n829722
*
* Nov. 2004 MM :                                         "AC0K020254
* origin acceptance : process valuated block GR          "AC0K020254
* stock MARC-BWESB as hidden field                       "AC0K020254

* July 2004 MM                                              "n759412
*  Avoid shortdump DBIF_RSQL_INVALID_RSQL
*
* Improvements :                       Nov 2003 TW          "n667256
* - enable Webreporting                                     "n667256
* - print the page numbers                                  "n667256
* - use function module for database commit for the update  "n667256
*   of the parameters in table ESDUS. This allows to record "n667256
*   this transaction for a batch input session using        "n667256
*   transaction SHDB                                        "n667256

* Feb. 2003 MM                                              "n577268
* ignore stocks or special stocks from plant level when     "n577268
* user selects via storage location                         "n577268

* Jan. 2003 MM                                              "n591618
* report displayed stocks for plant although the user has   "n591618
* not the required authorization for that plant             "n591618

* Dec. 2002 MM                                              "n579976
* M7 393 when user deletes the initial display variant      "n579976

*-----------------------------------------------------------"n546707
* note 546707                         August 19th, 2002 MM  "n546707
* wrong parameters for AUTHORITY-CHECK 'F_BKPF_BUK' fixed   "n546707
*-----------------------------------------------------------"n531604
*     performance improved  4.6 and higher   July 2002 MM   "n531604
*     - consider settings from view V_MMIM_REP_PRINT        "n531604
*     - choose flat or hierarchic list                      "n531604
*     - show the individual line of special stocks E and Q  "n531604
*     - authority-check for values per company code         "n531604
*     - work with a lean table ORGAN                        "n531604
*     - new function processing special stocks              "n531604
*     - new function of parameter XLVORM                    "n531604
*     - evaluate and show the flag for deletion             "n531604
*     - contents of parameters :                            "n531604
*       - in dialog mode : preset parameters from last run  "n531604
*       - save the parameters during each run               "n531604
*-----------------------------------------------------------"n531604
*
*     note 494306  FI help dokumentation for parameters improved
*                  Feb. 11th 2002 XJD
*
*     Note 155853: First version
*     Note 161442: Display variante possible again
*     Note 177898: MSKA/MSPR with duplicate entires, SELECT error
*     Note 182322: Error during authorization check for plant
*     Note 307852: Jump back to initial screen for empty selection
*     Note 311770: Batch selection
*     Note 353428: Value even for valuation area with 0 stock
*     Note 304353: New selection fields into the standard.
*     Note 388735: Abort COLLECT_OVERFLOW_TYPE_P due to note 353428
*     Note 407810: Typo-error causes unprecise values (follow up
*                  of note 388735).
*
************************************************************************
*     List of stock quantities and values
************************************************************************
REPORT rm07mlbs MESSAGE-ID m7 NO STANDARD PAGE HEADING LINE-SIZE 170.
ENHANCEMENT-POINT rm07mlbs_g4 SPOTS es_rm07mlbs STATIC.
ENHANCEMENT-POINT rm07mlbs_g5 SPOTS es_rm07mlbs.
ENHANCEMENT-POINT rm07mlbs_g6 SPOTS es_rm07mlbs STATIC.
ENHANCEMENT-POINT rm07mlbs_g7 SPOTS es_rm07mlbs.

************************************************************************
* Data declarations
************************************************************************

* Type pools
TYPE-POOLS: slis, imrep.

* Database tables
TABLES: mara, makt, mard, mchb, mkol, mslb, mska, msku, mssa, mspr,
        mssq, mbew, ebew, qbew, t134m, t001w, t001l, marc, t001, t001k,
        t023, t024.

TABLES : sscrfields.         "for the user-commands

* working table for the entries of all stock tables
DATA: BEGIN OF collector OCCURS 0,
        matnr    LIKE mara-matnr,
        werks    LIKE t001w-werks,
        lgort    LIKE mard-lgort,
        sobkz    LIKE mkol-sobkz,
        bwtar    LIKE mcha-bwtar,                           "1795093
        pspnr    LIKE  mspr-pspnr,
        vbeln    LIKE  mska-vbeln,
        posnr    LIKE  mska-posnr,
        lifnr    LIKE mslb-lifnr,
        kunnr    LIKE msku-kunnr,
        lvorm    LIKE  mard-lvorm,

        kzbws    LIKE mssa-kzbws,
        charg    LIKE mchb-charg,
        labst    LIKE mard-labst,
        insme    LIKE mard-insme,
        speme    LIKE mard-speme,
        einme    LIKE mard-einme,
        retme    LIKE mard-retme,
        umlme    LIKE mard-umlme,
        bwesb    LIKE  marc-bwesb,                          "AC0K020254
        glgmg    LIKE marc-glgmg,                           "n912093
        trame    LIKE marc-trame,                           "n912093
        umlmc    LIKE marc-umlmc,                           "n912093
        maktx    LIKE makt-maktx,                           "1795093
        xchar    LIKE marc-xchar,                           "1795093
        sgt_scat LIKE mchb-sgt_scat.
* CWM Fields
DATA: /cwm/meins  LIKE mari-/cwm/meins,
      /cwm/valum  LIKE mari-/cwm/meins,
      /cwm/xcwmat LIKE mara-/cwm/xcwmat,
      /cwm/labst  LIKE /cwm/emard_pq-/cwm/labst,
      /cwm/insme  LIKE /cwm/emard_pq-/cwm/insme,
      /cwm/einme  LIKE /cwm/emard_pq-/cwm/einme,
      /cwm/speme  LIKE /cwm/emard_pq-/cwm/speme,
      /cwm/retme  LIKE /cwm/emard_pq-/cwm/retme,
      /cwm/umlme  LIKE /cwm/emard_pq-/cwm/umlme,
      /cwm/umlmc  LIKE /cwm/emarc_pq-/cwm/umlmc,
      /cwm/trame  LIKE /cwm/emarc_pq-/cwm/trame,
      /cwm/bwesb  LIKE /cwm/emarc_pq-/cwm/bwesb.

"{ Begin ENHO DIPCS_RM07MLBS001 IS-AD-SSP AD_SUB }
* GA1727687 - A&D special stocks at subcontractor
DATA: owner  LIKE msis-owner,
      lifnr2 LIKE mslb-lifnr.
"{ End ENHO DIPCS_RM07MLBS001 IS-AD-SSP AD_SUB }

ENHANCEMENT-POINT ehp604_rm07mlbs_01 SPOTS es_rm07mlbs STATIC .
DATA: END OF collector.

* Internal tables
DATA: BEGIN OF header OCCURS 0,
        matnr LIKE mara-matnr,
        maktx LIKE makt-maktx,
        werks LIKE t001w-werks,
        name1 LIKE t001w-name1,
        mtart LIKE mara-mtart,
        matkl LIKE mara-matkl,
        "{ Begin ENHO MGV_LAMA_RM07MLBS LO-MD-MM MGV_LAMA }
        emnfr LIKE mara-mfrpn.
"{ End ENHO MGV_LAMA_RM07MLBS LO-MD-MM MGV_LAMA }
* CWM Fields
DATA:   /cwm/xcwmat LIKE mara-/cwm/xcwmat.

ENHANCEMENT-POINT ehp604_rm07mlbs_02 SPOTS es_rm07mlbs STATIC .
DATA: END OF header.

DATA: BEGIN OF bestand OCCURS 0,
*        Key fields
        matnr    LIKE mara-matnr,
        werks    LIKE t001w-werks,
        lgort    LIKE mard-lgort,
        sobkz    LIKE mkol-sobkz,
        ssnum    LIKE  bickey-ssnum,                        "n531604
        pspnr    LIKE  mspr-pspnr,                          "n531604
        vbeln    LIKE  mska-vbeln,                          "n531604
        posnr    LIKE  mska-posnr,                          "n531604
        lifnr    LIKE mkol-lifnr,
        kunnr    LIKE msku-kunnr,
        kzbws    LIKE mssa-kzbws,
        charg    LIKE mchb-charg,
*        Additional data (texts, unit, ...)
        maktx    LIKE marav-maktx,
        bwkey    LIKE mbew-bwkey,
        mtart    LIKE marav-mtart,
        matkl    LIKE marav-matkl,
        meins    LIKE marav-meins,
        bwtty    LIKE marc-bwtty,
        xchar    LIKE marc-xchar,
        lgobe    LIKE t001l-lgobe,
        bwtar    LIKE mcha-bwtar,
        waers    LIKE t001-waers,
        name1    LIKE t001w-name1,
*        Quantities and currencies
        labst    LIKE mard-labst,
        wlabs    LIKE mbew-salk3,
        insme    LIKE mard-insme,
        winsm    LIKE mbew-salk3,
        speme    LIKE mard-speme,
        wspem    LIKE mbew-salk3,
        einme    LIKE mard-einme,
        weinm    LIKE mbew-salk3,
        retme    LIKE mard-retme,
        wretm    LIKE mbew-salk3,
        umlme    LIKE mard-umlme,
        wumlm    LIKE mbew-salk3,
        glgmg    LIKE marc-glgmg,                           "n912093
        wglgm    LIKE mbew-salk3,                           "n912093
        trame    LIKE marc-trame,                           "n912093
        wtram    LIKE mbew-salk3,                           "n912093
        umlmc    LIKE marc-umlmc,                           "n912093
        wumlc    LIKE mbew-salk3,                           "n912093

*        Dummy field
        dummy    TYPE  alv_dummy,
*        Colour
        farbe    TYPE slis_t_specialcol_alv,
        lvorm    LIKE  mard-lvorm,

*        valuated blocked GR stock                       "AC0K020254
        bwesb    LIKE  marc-bwesb,                          "AC0K020254
        wbwesb   LIKE  mbew-salk3,                          "AC0K020254
        sgt_scat LIKE  mchb-sgt_scat.
* CWM Fields
* CWM parallel Quantities
DATA: /cwm/labst  LIKE mard-/cwm/labst,
      /cwm/insme  LIKE mard-/cwm/insme,
      /cwm/speme  LIKE mard-/cwm/speme,
      /cwm/einme  LIKE mard-/cwm/einme,
      /cwm/retme  LIKE mard-/cwm/retme,
      /cwm/umlme  LIKE mard-/cwm/umlme,
*         /cwm/glgmg  LIKE marc-/cwm/glgmg,
      /cwm/trame  LIKE marc-/cwm/trame,
      /cwm/umlmc  LIKE marc-/cwm/umlmc,
      /cwm/bwesb  TYPE marc-/cwm/bwesb,
* CWM
      /cwm/meins  LIKE mari-/cwm/meins,
      /cwm/valum  LIKE mari-/cwm/meins,
      /cwm/xcwmat LIKE mara-/cwm/xcwmat,
*        Dummy field
      /cwm/dummy  TYPE  alv_dummy.

"{ Begin ENHO DIPCS_RM07MLBS001 IS-AD-SSP AD_SUB }
* GA1727687 - A&D special stocks at subcontractor
DATA: owner  LIKE msis-owner,
      lifnr2 LIKE mslb-lifnr.
"{ End ENHO DIPCS_RM07MLBS001 IS-AD-SSP AD_SUB }


ENHANCEMENT-POINT ehp604_rm07mlbs_03 SPOTS es_rm07mlbs STATIC .
DATA:  END OF bestand.

* define a lean table organ
TYPES : BEGIN OF stype_organ,
          werks LIKE  t001w-werks,
          bwkey LIKE  t001w-bwkey,
          name1 LIKE  t001w-name1,
          bukrs LIKE  t001-bukrs,
          waers LIKE  t001-waers,
        END OF stype_organ,

        stab_organ TYPE STANDARD TABLE OF
                             stype_organ
                             WITH DEFAULT KEY.

DATA: g_t_organ TYPE  stab_organ,
      g_s_organ TYPE  stype_organ.

* define a buffer table for the MARD entries with flag
* for deletion
TYPES : BEGIN OF stype_mard_lv,
          matnr LIKE  mard-matnr,
          werks LIKE  mard-werks,
          lgort LIKE  mard-lgort,
          lvorm LIKE  mard-lvorm,
        END OF stype_mard_lv,

        htab_mard_lv TYPE HASHED TABLE OF
                             stype_mard_lv
                   WITH UNIQUE KEY matnr werks lgort.

DATA : g_s_mard_lv TYPE  stype_mard_lv,
       g_t_mard_lv TYPE  htab_mard_lv.

* define a buffer table for the storage bins
TYPES : BEGIN OF stype_t001l,
          werks LIKE  t001l-werks,
          lgort LIKE  t001l-lgort,
          lgobe LIKE  t001l-lgobe,
        END OF stype_t001l,

        htab_t001l TYPE HASHED TABLE OF
                             stype_t001l
                             WITH UNIQUE KEY werks lgort.

DATA : g_s_t001l TYPE  stype_t001l,
       g_t_t001l TYPE  htab_t001l.

TYPES : BEGIN OF stype_t001w,                               "09122009
          werks TYPE  t001w-werks,                          "09122009
          bwkey TYPE  t001w-bwkey,                          "09122009
          name1 TYPE  t001w-name1,                          "09122009
        END OF stype_t001w.                                 "09122009
                                                            "09122009
DATA : gt_t001w              TYPE  STANDARD TABLE           "09122009
                                   OF stype_t001w.          "09122009

* define working areas for access table organ               "n531604
TYPES : BEGIN OF stype_buffer,                              "n531604
          werks LIKE  t001w-werks,
          bukrs LIKE  t001-bukrs,
          subrc LIKE  syst-subrc,
        END OF stype_buffer,

        stab_buffer TYPE STANDARD TABLE OF
              stype_buffer
              WITH DEFAULT KEY.

DATA : g_s_buffer TYPE  stype_buffer,
       g_t_buffer TYPE  stab_buffer.

* Data for listviewer
DATA: repid     LIKE sy-repid.
DATA: fieldcat  TYPE slis_t_fieldcat_alv WITH HEADER LINE.
DATA: keyinfo   TYPE slis_keyinfo_alv.
DATA: color     TYPE slis_t_specialcol_alv WITH HEADER LINE.
DATA: layout    TYPE slis_layout_alv.

DATA: sort      TYPE slis_t_sortinfo_alv WITH HEADER LINE.
DATA: excluding TYPE slis_t_extab WITH HEADER LINE.

* internal working table for events / for the headlines     "n667256
DATA: gs_events     TYPE slis_alv_event.                    "n667256
DATA: gt_events     TYPE slis_t_event.                      "n667256

                                                            "n667256
* for the header of the list, when alv grid is in use       "n667256
DATA : gt_ueb TYPE  slis_t_listheader,                      "n667256
       gs_ueb TYPE  slis_listheader.                        "n667256

* Variants
DATA: variante        LIKE disvariant,
      variante_flat   LIKE disvariant,
      def_variante    LIKE disvariant,
      def_variante_f4 LIKE disvariant,
      variant_exit(1) TYPE c.


"{ Begin ENHO AD_MPN_MD_RM07MLBS IS-AD-MPN-MD AD_MPN }

DATA: l_buffer TYPE mpnbuffr OCCURS 0 WITH HEADER LINE.
DATA: show_ext_manufacturer LIKE  t130f-kzkma.
"{ End ENHO AD_MPN_MD_RM07MLBS IS-AD-MPN-MD AD_MPN }

"{ Begin ENHO DIPCS_RM07MLBS001 IS-AD-SSP AD_SUB }
* GA1727687 - A&D special stocks at subcontractor
DATA: lt_fieldcat TYPE slis_t_fieldcat_alv WITH HEADER LINE,
      ls_fieldcat TYPE slis_fieldcat_alv,
      lv_subrc    TYPE subrc.                               "GA1956994

DATA: collector_dipcs  LIKE STANDARD TABLE OF collector.    "1920470
"{ End ENHO DIPCS_RM07MLBS001 IS-AD-SSP AD_SUB }

ENHANCEMENT-POINT rm07mlbs_01 SPOTS es_rm07mlbs STATIC.
ENHANCEMENT-POINT rm07mlbs_13 SPOTS es_rm07mlbs STATIC .
DATA : g_f_vari_hsq LIKE  disvariant-variant,
       g_f_vari_flt LIKE  disvariant-variant.

* working fields to save the initial display variants       "n579976
DATA : g_f_vari_hsq_initial LIKE  disvariant-variant,       "n579976
       g_f_vari_flt_initial LIKE  disvariant-variant.       "n579976

* Global variables for handling ALV functionality
TABLES: mmim_rep_print.

DATA: alv_keyinfo      TYPE slis_keyinfo_alv.
DATA: alv_variant      LIKE disvariant.
DATA: alv_layout       TYPE slis_layout_alv.
DATA: alv_repid        LIKE sy-repid.
DATA: alv_print        TYPE slis_print_alv.
DATA: alv_detail_func(30) TYPE  c,
      alv_color           LIKE      mmim_rep_print-color.

* User settings for the checkboxes
DATA: oref_settings TYPE REF TO cl_mmim_userdefaults.

* define working fields
DATA : g_cnt_col_pos TYPE i,
       g_cnt_spos    TYPE i.

DATA : g_flag_ok(01)       TYPE c,
       g_flag_mess_333(01) TYPE c,
       g_flag_t001l(01)    TYPE c.

DATA : g_cnt_variant_error   TYPE i.                        "n667256

* does the user want to suppress objects from plant level ? "n577268
DATA : g_flag_suppress_init_lgort(01)  TYPE c.              "n577268

DATA : BEGIN OF g_flag_sobkz,
         vbeln(01) TYPE c,
         pspnr(01) TYPE c,
         lifnr(01) TYPE c,
         kunnr(01) TYPE c,
       END OF g_flag_sobkz.

CONSTANTS : c_no_out(01) TYPE c    VALUE 'X',
            c_out(01)    TYPE c    VALUE space.

* flag to be set when INITIALIZATION was processed          "n667256
DATA g_flag_initialization(01) TYPE c.                      "n667256

* authorization check should be always processed            "n829722
DATA t_flag_launched(01) TYPE c.                            "n829722

DATA:
  dbcon        TYPE dbcon_name,                             "n1710852
  newsel(01)   TYPE c,                                      "n1795093
  lv_dontpanic TYPE symsgv.                                 "n1795093

CONSTANTS:
  c_hdb_dbcon_get TYPE funcname VALUE 'MM_HDB_DBCON_GET',   "n1710852
  c_hdb_subappl   TYPE program  VALUE 'MB52'.               "n1710852

DATA:                                                       "n1795093
  collector_mard LIKE STANDARD TABLE OF collector,          "n1795093
  collector_mchb LIKE STANDARD TABLE OF collector,          "n1795093
  collector_mkol LIKE STANDARD TABLE OF collector,          "n1795093
  collector_mska LIKE STANDARD TABLE OF collector,          "n1795093
  collector_mssa LIKE STANDARD TABLE OF collector,          "n1795093
  collector_mspr LIKE STANDARD TABLE OF collector,          "n1795093
  collector_mssq LIKE STANDARD TABLE OF collector,          "n1795093
  collector_mbew LIKE STANDARD TABLE OF collector,          "n1795093
  collector_ebew LIKE STANDARD TABLE OF collector,          "n1795093
  collector_msku LIKE STANDARD TABLE OF collector,          "n1795093
  collector_mslb LIKE STANDARD TABLE OF collector,          "n1795093
  collector_mstb LIKE STANDARD TABLE OF collector,          "n1795093
  collector_mste LIKE STANDARD TABLE OF collector,          "n1795093
  collector_mstq LIKE STANDARD TABLE OF collector,          "n1795093
  collector_uml  LIKE STANDARD TABLE OF collector.          "n1795093

************************************************************************
* Selection screen
************************************************************************

SELECTION-SCREEN BEGIN OF BLOCK abgrenzung WITH FRAME TITLE TEXT-001.

SELECT-OPTIONS:
  matnr  FOR mara-matnr MEMORY ID mat MATCHCODE OBJECT mat1,
  werks  FOR t001l-werks  MEMORY ID wrk,                    "718285
  lgort  FOR t001l-lgort  MEMORY ID lag,
  charg  FOR mchb-charg  MEMORY ID cha MATCHCODE OBJECT mch1.

SELECTION-SCREEN END OF BLOCK abgrenzung.

*----------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK lbs WITH FRAME TITLE TEXT-070.

SELECT-OPTIONS:
  matart FOR mara-mtart,
  matkla FOR mara-matkl,
  ekgrup FOR marc-ekgrp.


"{ Begin ENHO MGV_LAMA_RM07MLBS LO-MD-MM MGV_LAMA }
"Since this declaration was used in enhancement implementation of ad_mpn_RM07MLBS hence part of the core.
SELECT-OPTIONS:
*  Modification for PIC-Supersession/MPN
 mfrpn FOR mara-mfrpn MATCHCODE OBJECT mat1_mpn.
"{ End ENHO MGV_LAMA_RM07MLBS LO-MD-MM MGV_LAMA }


ENHANCEMENT-POINT rm07mlbs_3 SPOTS es_rm07mlbs STATIC .

SELECTION-SCREEN END OF BLOCK lbs.

*----------------------------------------------------------------------*

* for the selection os special stocks
SELECTION-SCREEN BEGIN OF BLOCK lb2 WITH FRAME TITLE TEXT-071.

* for the selection os special stocks
PARAMETERS : pa_sond       LIKE      rmmmb-kzlso
                           DEFAULT   'X'.

SELECT-OPTIONS:
  so_sobkz                 FOR  mkol-sobkz.

SELECTION-SCREEN END OF BLOCK lb2.

*----------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK lb1 WITH FRAME TITLE TEXT-080.

* select only lines who contain at least one negative stock
PARAMETERS: negativ LIKE am07m-seneg.

* Documentation for parameter XMCHB improved                "n494306
PARAMETERS: xmchb            LIKE      am07m-mb52_xmchb     "n494306
                             DEFAULT   'X'.

* Checkbox to eliminate lines with zero stocks
PARAMETERS: nozero   LIKE rmmmb-kznul.

* Checkbox to disable value processing.
* Documentation for parameter NOVALUES improved             "n494306
PARAMETERS: novalues         LIKE      am07m-mb52_noval.    "n494306


SELECTION-SCREEN END OF BLOCK lb1.

*----------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK liste WITH FRAME TITLE TEXT-005.

* choose flat or hierarchic list                            "n531604
SELECTION-SCREEN BEGIN OF LINE.                             "n531604
SELECTION-SCREEN         POSITION 1.                        "n531604
PARAMETERS : pa_hsq      LIKE  am07m-mb52_alv_hsq        "#EC SEL_WRONG
                         DEFAULT  'X'                       "n531604
                         RADIOBUTTON GROUP alvv             "n531604
                         USER-COMMAND alvv.                 "n531604
SELECTION-SCREEN         COMMENT 3(40)  TEXT-006            "n531604
                         FOR FIELD pa_hsq.                  "n531604
SELECTION-SCREEN END OF LINE.                               "n531604
                                                            "n531604
SELECTION-SCREEN BEGIN OF LINE.                             "n531604
SELECTION-SCREEN         POSITION 1.                        "n531604
PARAMETERS : pa_flt      LIKE  am07m-mb52_alv_flt        "#EC SEL_WRONG
                         RADIOBUTTON GROUP alvv.            "n531604
SELECTION-SCREEN         COMMENT 3(40)  TEXT-007            "n531604
                         FOR FIELD pa_flt.                  "n531604
SELECTION-SCREEN END OF LINE.                               "n531604

*  parameters :
*    pa_grid                  type  MB_XFELD
*                             default 'X'
*                             radiobutton group alv1,
*    pa_class                 type  MB_XFELD
*                             radiobutton group alv1.

PARAMETERS: p_vari LIKE disvariant-variant.

SELECTION-SCREEN END OF BLOCK liste.
*
PARAMETERS /cwm/exp TYPE checkbox NO-DISPLAY. " if marked: just give
*                                                 data back via memory
*                                                 - no display of data

ENHANCEMENT-POINT ehp604_rm07mlbs_04 SPOTS es_rm07mlbs STATIC .

*----------------------------------------------------------------------*

* F4-Help for variant
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_vari.
  PERFORM f4_for_variant.

************************************************************************
* Main program
************************************************************************

AT SELECTION-SCREEN.

* check radiobuttons                                          "n667256
  IF  pa_hsq IS INITIAL.                                    "n667256
    IF  pa_flt IS INITIAL.                                  "n667256
*     not allowed                                             "n667256
      MOVE  'X'              TO  pa_hsq.                    "n667256
      ADD  1                 TO  g_cnt_variant_error.       "n667256
    ENDIF.                                                  "n667256
  ELSE.                                                     "n667256
    IF  pa_flt IS INITIAL.                                  "n667256
    ELSE.                                                   "n667256
*     not allowed                                             "n667256
      CLEAR                  pa_flt.                        "n667256
      ADD  1                 TO  g_cnt_variant_error.       "n667256
    ENDIF.                                                  "n667256
  ENDIF.                                                    "n667256
                                                            "n667256
* the user will get the info about the old variant only once  "n667256
  IF  g_cnt_variant_error = 1.                              "n667256
    IF  NOT sy-slset IS INITIAL.                            "n667256
      MESSAGE i634(db)       WITH  sy-slset sy-repid.       "n667256
    ENDIF.                                                  "n667256
  ENDIF.                                                    "n667256

* has the user changed the radiobuttons for the mode of the "n531604
* SAP-LIST-VIEWER ?                                         "n531604
  IF  sscrfields-ucomm  =  'ALVV'.
*   yes, restore the old entry if extists                   "n531604
    IF      NOT pa_hsq IS INITIAL.
      MOVE  g_f_vari_hsq     TO  p_vari.
    ELSEIF  NOT  pa_flt IS INITIAL.
*     for flat ( simple ) list
      MOVE  g_f_vari_flt     TO  p_vari.
    ENDIF.

  ELSE.
*   save the display variant depending on the selected mode "n531604
*   of the SAP-LIST-VIEWER
    IF      NOT pa_hsq IS INITIAL.
*     for hierarchic seq. list
      MOVE p_vari            TO  g_f_vari_hsq.

    ELSEIF  NOT  pa_flt IS INITIAL.
*     for flat ( simple ) list
      MOVE p_vari            TO  g_f_vari_flt.

    ENDIF.
  ENDIF.

* it is necessary to set flag xmchb if batch has been entered because
* otherwise MCHB will not be read and non suitable items can't be
* be removed later on in form data_selection
  IF NOT charg[] IS INITIAL. xmchb = 'X'. ENDIF.    "note 311770

* send a warning if the user starts this report without any "n531604
* restrictions for the database selection                   "n531604
* only when this report is started                          "n531604
  IF matnr IS INITIAL AND                                   "n531604
     werks IS INITIAL AND                                   "n531604
     lgort IS INITIAL AND                                   "n531604
     charg IS INITIAL.                                      "n531604
    IF  sy-ucomm  =  'ONLI'  OR                             "n531604
        sy-ucomm  =  'PRIN'.                                "n531604
ENHANCEMENT-SECTION ehp604_rm07mlbs_43 SPOTS es_rm07mlbs .
      LOG-POINT ID /cwm/enh SUBKEY to_upper( sy-tcode && '\/CWM/APPL_MM_RM07MLBS_EHP4\EHP604_RM07MLBS_43\' && sy-cprog ) FIELDS /cwm/cl_enh_layer=>get_field( ).
      IF /cwm/exp IS INITIAL.
        MESSAGE  w689.    "The selection was not restricted   "n531604
      ENDIF.
END-ENHANCEMENT-SECTION.
    ENDIF.                                                  "n531604
  ENDIF.                                                    "n531604
* go on only if the user wants to launch this report        "n667256
* the authorization check should be always processed        "n829722
  IF sy-ucomm = 'ONLI' OR                                   "n829722
     sy-ucomm = 'PRIN' OR                                   "n829722
     sy-ucomm = 'SJOB' OR                                   "n829722
     sy-ucomm = space.                                      "n829722
    MOVE 'X' TO t_flag_launched.                            "n829722
  ELSE.                                                     "n829722
    IF sy-ucomm <> space.                                   "n829722
      CLEAR t_flag_launched.                                "n829722
    ENDIF.                                                  "n829722
  ENDIF.                                                    "n829722
  CHECK t_flag_launched = 'X'.                              "n829722

  PERFORM organisation.

  PERFORM check_entry.

  PERFORM check_authorization.

* save the parameters of this run in database table ESDUS   "n531604
  PERFORM                    f0200_settings_save.           "n531604

*------------------------ Initialisierung -----------------------------*
INITIALIZATION.
  "{ Begin ENHO AD_MPN_MD_RM07MLBS IS-AD-MPN-MD AD_MPN }
  DATA: lt_sel_dtel TYPE  rsseldtel OCCURS 0,
        ls_sel_dtel TYPE  rsseldtel.
  "{ End ENHO AD_MPN_MD_RM07MLBS IS-AD-MPN-MD AD_MPN }
ENHANCEMENT-POINT rm07mlbs_05 SPOTS es_rm07mlbs.
  PERFORM                    f0000_get_print_settings.      "n531604

* look for the setting of the parameters from the last run  "n531604
  PERFORM                    f0100_settings_init.           "n531604

  PERFORM initialisierung.

* set flag when INITILIZATION is processed                  "n667256
  MOVE  'X'        TO  g_flag_initialization.               "n667256
                                                            "n667256
*-----------------------------------------------------------"n667256



  "{ Begin ENHO AD_MPN_MD_RM07MLBS IS-AD-MPN-MD AD_MPN }

  IF cl_immpn_cust=>check_mpn_active( ) = abap_true.
    ls_sel_dtel-name = 'MFRPN'.
    ls_sel_dtel-kind = 'S'.
    ls_sel_dtel-datenelment = 'MFRPN'.
    APPEND ls_sel_dtel TO lt_sel_dtel.

    CALL FUNCTION 'SELECTION_TEXTS_MODIFY_DTEL'
      EXPORTING
        program                     = sy-repid
      TABLES
        sel_dtel                    = lt_sel_dtel
      EXCEPTIONS
        program_not_found           = 1
        program_cannot_be_generated = 2
        OTHERS                      = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDIF.
  "{ End ENHO AD_MPN_MD_RM07MLBS IS-AD-MPN-MD AD_MPN }

  "{ Begin ENHO MGV_LAMA_RM07MLBS LO-MD-MM MGV_LAMA }
  IF cl_immpn_cust=>check_mpn_active( ) = abap_true.

    CALL FUNCTION 'MPN01_SHOW_EXT_MANUFACTURER'
      IMPORTING
        show_ext_manufacturer = show_ext_manufacturer.
  ENDIF.
  "{ End ENHO MGV_LAMA_RM07MLBS LO-MD-MM MGV_LAMA }


                                                            "n667256
ENHANCEMENT-POINT rm07mlbs_07 SPOTS es_rm07mlbs.

START-OF-SELECTION.

* it makes no sence to carry out this report                "n667256
  IF  g_cnt_variant_error > 0.                              "n667256
    IF  NOT sy-slset IS INITIAL.                            "n667256
      MESSAGE e634(db)       WITH  sy-slset sy-repid.       "n667256
    ENDIF.                                                  "n667256
  ENDIF.                                                    "n667256

* does the user restrict the storage locations and want to  "n577268
* suppress stock objects from plant level ?                 "n577268
  CLEAR                      collector-lgort.               "n577268
                                                            "n577268
  IF  collector-lgort IN lgort.                             "n577268
    CLEAR                    g_flag_suppress_init_lgort.    "n577268
  ELSE.                                                     "n577268
    MOVE  'X'                TO  g_flag_suppress_init_lgort. "n577268
  ENDIF.                                                    "n577268

  BREAK-POINT ID mmim_rep_mb52.                             "n1795093

  IF newsel = '1'.                                          "n1795093
    IF novalues IS INITIAL.
      PERFORM data_selection_join.
    ELSE.
      PERFORM data_selection_new.
    ENDIF.
  ELSEIF newsel = 'X'.
    PERFORM data_selection_new.
  ELSE.                                                     "n1795093
    PERFORM data_selection.                                 "n1795093
  ENDIF.                                                    "n1795093

*-------------------------- Datenausgabe-------------------------------*
END-OF-SELECTION.
  READ TABLE bestand INDEX 1 TRANSPORTING NO FIELDS.

  IF sy-subrc = 0.
*   the fieldcatalog depends on the type of list            "n531604
    IF      NOT pa_hsq IS INITIAL.                          "n531604
*     create hierarchic list                                "n531604
      PERFORM                fieldcatalog.                  "n531604
    ELSEIF  NOT  pa_flt IS INITIAL.                         "n531604
      PERFORM                f0300_fieldcat_flat.           "n531604
    ENDIF.                                                  "n531604

    IF  g_flag_mess_333 = 'X'.
*     "The list is incomplete due to lacking authorization
      MESSAGE                s333.
    ENDIF.

    PERFORM list_output.
  ELSE.
    MESSAGE s843.
*   Zu den vorgegebenen Daten ist kein Bestand vorhanden
    IF NOT sy-calld IS INITIAL.                             "307852
      IF sy-tcode = 'MB52'.
        SUBMIT rm07mlbs
          WITH matnr    IN matnr
          WITH werks    IN werks
          WITH lgort    IN lgort
          WITH charg    IN charg
          WITH matart   IN matart
          WITH matkla   IN matkla
          WITH ekgrup   IN ekgrup
          WITH pa_sond  =  pa_sond
          WITH so_sobkz IN so_sobkz
          WITH negativ  =  negativ
          WITH xmchb    = xmchb
          WITH nozero   = nozero
          WITH novalues = novalues
          WITH pa_hsq   = pa_hsq
          WITH pa_flt   = pa_flt
          WITH p_vari   = p_vari
          WITH mfrpn IN mfrpn                               "N_2547726
          VIA SELECTION-SCREEN.
      ELSE.
        LEAVE.                                              "307852
      ENDIF.
    ELSE.                                                   "307852
*      LEAVE TO TRANSACTION sy-tcode.                            "n1590783
*     SELECT-OPTIONS will be lost with leave to transaction..    "n1590783
*     Change logic to submit and provide select-options          "n1590783
      SUBMIT rm07mlbs                                       "n1590783
       WITH matnr    IN matnr                               "n1590783
       WITH werks    IN werks                               "n1590783
       WITH lgort    IN lgort                               "n1590783
       WITH charg    IN charg                               "n1590783
       WITH matart   IN matart                              "n1590783
       WITH matkla   IN matkla                              "n1590783
       WITH ekgrup   IN ekgrup                              "n1590783
       WITH pa_sond  =  pa_sond                             "n1590783
       WITH so_sobkz IN so_sobkz                            "n1590783
       WITH negativ  =  negativ                             "n1590783
       WITH xmchb    = xmchb                                "n1590783
       WITH nozero   = nozero                               "n1590783
       WITH novalues = novalues                             "n1590783
       WITH pa_hsq   = pa_hsq                               "n1590783
       WITH pa_flt   = pa_flt                               "n1590783
       WITH p_vari   = p_vari                               "n1590783
       WITH mfrpn IN mfrpn                                  "N_2547726
       VIA SELECTION-SCREEN.                                "n1590783
    ENDIF.                                                  "307852
  ENDIF.

************************************************************************
* Read organisation
************************************************************************
FORM organisation.

* define working areas                                        "09122009
  DATA : ls_organ            LIKE  LINE OF g_t_organ.       "09122009
  FIELD-SYMBOLS : <ls_t001w> LIKE  LINE OF gt_t001w.        "09122009

* get all existing storage bins of the required plants
  REFRESH : g_t_t001l, g_t_organ.
  REFRESH : gt_t001w.                                       "09122009
  CLEAR   : g_s_t001l, g_s_organ, g_flag_t001l.

  SELECT  werks lgort lgobe  FROM t001l        "#EC CI_GENBUFF 09122009
                   INTO CORRESPONDING FIELDS OF TABLE g_t_t001l
                   WHERE  werks IN werks
                     AND  lgort IN lgort
                     AND  lgobe NE space.

  IF  sy-subrc IS INITIAL.
    MOVE  'X'                TO  g_flag_t001l.
  ENDIF.

  IF newsel IS INITIAL.                                     "n1795093
* read setting of the plants                                  "09122009
    SELECT werks name1 bwkey    FROM t001w     "#EC CI_GENBUFF 09122009
      INTO CORRESPONDING FIELDS OF TABLE gt_t001w           "09122009
        WHERE werks IN werks.
                                                            "09122009
    LOOP AT gt_t001w           ASSIGNING  <ls_t001w>.       "09122009
      MOVE-CORRESPONDING  <ls_t001w>                        "09122009
                               TO  ls_organ.        "#EC ENHOK 09122009
      APPEND  ls_organ         TO  g_t_organ.               "09122009
    ENDLOOP.                                                "09122009

    SORT g_t_organ             BY bwkey.

    LOOP AT g_t_organ          INTO  g_s_organ.
      ON CHANGE OF g_s_organ-bwkey.
        CLEAR                  g_flag_ok.

        SELECT SINGLE * FROM t001k
                     WHERE bwkey EQ g_s_organ-bwkey.

        IF sy-subrc IS INITIAL.
          SELECT SINGLE * FROM t001
                     WHERE bukrs EQ t001k-bukrs.

          IF sy-subrc IS INITIAL.
            MOVE  'X'          TO  g_flag_ok.
          ENDIF.
        ENDIF.
      ENDON.

      IF  g_flag_ok = 'X'.
        MOVE-CORRESPONDING t001  TO  g_s_organ.     "#EC ENHOK 09122009

        MODIFY  g_t_organ        FROM  g_s_organ.
      ENDIF.
    ENDLOOP.

  ELSE.                                                     "n1795093
    SELECT t001w~werks t001w~name1 t001w~bwkey t001k~bukrs t001~waers
      INTO CORRESPONDING FIELDS OF TABLE g_t_organ
      FROM t001w
      INNER JOIN t001k
      ON t001k~bwkey = t001w~bwkey
      LEFT JOIN t001
      ON t001~bukrs = t001k~bukrs
      WHERE t001w~werks IN werks.
  ENDIF.                                                    "n1795093

ENDFORM.                               " ORGANISATION

************************************************************************
* Check authorization on plant level for all selected plants
************************************************************************
FORM check_authorization.

* define local working areas
  DATA : l_s_bukrs TYPE  stype_buffer,
         l_t_bukrs TYPE  stab_buffer.

  SORT   g_t_organ           BY werks.

* report displayed stocks for plant although the user has   "n591618
* not the required authorization for that plant             "n591618
  CLEAR                      g_s_buffer.                    "n591618

  LOOP AT g_t_organ          INTO  g_s_organ.
*   check the authority only after the plant has changed    "n531604
    IF  g_s_organ-werks NE g_s_buffer-werks.
      MOVE  g_s_organ-werks  TO  g_s_buffer-werks.

      AUTHORITY-CHECK OBJECT 'M_MATE_WRK'
                     ID 'ACTVT' FIELD '03'
                     ID 'WERKS' FIELD g_s_organ-werks.

      IF NOT sy-subrc IS INITIAL.
        SET  CURSOR          FIELD  'WERKS-LOW'.
        MESSAGE e120         WITH  g_s_organ-werks.
      ENDIF.
    ENDIF.

    IF  novalues IS INITIAL.                                "n531604
*     the user wants to see the values
      IF  g_s_organ-bukrs NE g_s_buffer-bukrs.
*       check the authority after the company code changed

        AUTHORITY-CHECK OBJECT 'F_BKPF_BUK'
*           parameters for AUTHORITY-CHECK command fixed    "n546707
            ID 'BUKRS' FIELD g_s_organ-bukrs                "n546707
            ID 'ACTVT' FIELD '03'.                          "n546707

        MOVE : g_s_organ-bukrs    TO  g_s_buffer-bukrs,     "n667256
               sy-subrc           TO  g_s_buffer-subrc.     "n667256

        IF sy-subrc <> 0.
*         no authorization -> save the company code
          MOVE : 'X'              TO  g_flag_mess_333,
                 g_s_organ-bukrs  TO  l_s_bukrs-bukrs.
          COLLECT l_s_bukrs       INTO  l_t_bukrs.
        ENDIF.
      ENDIF.

*     use the result from the buffer
      IF  g_s_buffer-subrc <> 0.
        CLEAR                g_s_organ-waers.
        MODIFY  g_t_organ    FROM  g_s_organ
                             TRANSPORTING  waers.
      ENDIF.
    ENDIF.
  ENDLOOP.

* send the info for each missing autority
  IF  g_flag_mess_333  =  'X'.
    SORT                     l_t_bukrs.
    SET CURSOR               FIELD  'WERKS-LOW'.

    LOOP AT l_t_bukrs        INTO  l_s_bukrs.
*     No authorization to display data for company code &
      MESSAGE  i862(m3)      WITH  l_s_bukrs-bukrs.
    ENDLOOP.
  ENDIF.

ENDFORM.                    "check_authorization

************************************************************************
* Check data on selection screen
************************************************************************
FORM check_entry.

* define working table                                        "09122009
  DATA : lt_t134m TYPE  STANDARD TABLE OF t134m,            "09122009
         lt_t023  TYPE  STANDARD TABLE OF t023.             "09122009

* Check some entered data for consistency
  CALL FUNCTION 'MMIM_ENTRYCHECK_MAIN'
    TABLES
      it_matnr = matnr
      it_werks = werks
      it_lgort = lgort
      it_ekgrp = ekgrup
      it_sobkz = so_sobkz.

* Material type
  IF NOT matart[] IS INITIAL.                               "09122009
    SELECT * FROM t134m      INTO TABLE lt_t134m            "09122009
      UP TO 1 ROWS                                          "09122009
        FOR ALL ENTRIES IN gt_t001w                         "09122009
          WHERE bwkey =  gt_t001w-bwkey                     "09122009
            AND mtart IN matart.                            "09122009

    IF NOT sy-subrc IS INITIAL.
      MESSAGE e104(m3) WITH matart-low.
    ENDIF.
  ENDIF.

* Material class
  IF NOT matkla[] IS INITIAL.                               "09122009
    SELECT *  FROM t023      INTO TABLE  lt_t023            "09122009
      UP TO 1 ROWS                                          "09122009
        WHERE matkl IN matkla.                              "09122009

    IF NOT sy-subrc IS INITIAL.
      MESSAGE e883 WITH matkla-low.
    ENDIF.
  ENDIF.

* Display variant
  IF NOT p_vari IS INITIAL.
    SET CURSOR               FIELD  'P_VARI'.

    IF      NOT pa_hsq IS INITIAL.
*     for hierarchic seq. list
      MOVE : p_vari          TO  variante-variant,
             variante        TO  def_variante.

    ELSEIF  NOT  pa_flt IS INITIAL.
*     for flat ( simple ) list
      MOVE : p_vari          TO  variante_flat-variant,
             variante_flat   TO  def_variante.
    ENDIF.

    CALL FUNCTION 'REUSE_ALV_VARIANT_EXISTENCE'
      EXPORTING
        i_save     = 'A'
      CHANGING
        cs_variant = def_variante.
  ELSE.                                                     "n579976
*   the user wants no initial display variant               "n579976
                                                            "n579976
    IF      NOT pa_hsq IS INITIAL.                          "n579976
*     for hierarchic seq. list                              "n579976
      IF  NOT g_f_vari_hsq_initial IS INITIAL.              "n579976
*       but the SAP-LIST-VIEWER will apply the existing     "n579976
*       initial display variant for hierarchic lists        "n579976
        PERFORM  f3000_send_warning_m7_393                  "n579976
                             USING  g_f_vari_hsq_initial.   "n579976
      ENDIF.                                                "n579976
                                                            "n579976
    ELSEIF  NOT  pa_flt IS INITIAL.                         "n579976
*     for flat ( simple ) list                              "n579976
      IF  NOT g_f_vari_flt_initial IS INITIAL.              "n579976
*       but the SAP-LIST-VIEWER will apply the existing     "n579976
*       initial display variant for flat lists              "n579976
        PERFORM  f3000_send_warning_m7_393                  "n579976
                             USING  g_f_vari_flt_initial.   "n579976
      ENDIF.                                                "n579976
    ENDIF.                                                  "n579976
  ENDIF.

ENDFORM.                               "check_entry

************************************************************************
* Initialization: Read default variant
************************************************************************
FORM initialisierung.

* prepare the areas for the different display variants
  repid = sy-repid.
  CLEAR : variante,          variante_flat.
  MOVE  : repid              TO  variante-report,
          repid              TO  variante_flat-report,
          'FLAT'             TO  variante_flat-handle.

* the display variant is depending on the seleted mode of   "n531604
* the SAP-LIST-VIEWER : look for both variants              "n531604

* a) Get default variant for the hierarchic list
  def_variante = variante.

  CALL FUNCTION 'REUSE_ALV_VARIANT_DEFAULT_GET'
    EXPORTING
      i_save     = 'A'
    CHANGING
      cs_variant = def_variante
    EXCEPTIONS
      not_found  = 2.

  IF sy-subrc = 0.
    MOVE  def_variante-variant    TO  g_f_vari_hsq.
*   save the initial display variant for the hierseq. list  "n579976
    MOVE  def_variante-variant    TO  g_f_vari_hsq_initial. "n579976
  ENDIF.

* b) Get default variant for the non-hierarchic list
  def_variante = variante_flat.

  CALL FUNCTION 'REUSE_ALV_VARIANT_DEFAULT_GET'
    EXPORTING
      i_save     = 'A'
    CHANGING
      cs_variant = def_variante
    EXCEPTIONS
      not_found  = 2.

  IF sy-subrc = 0.
    MOVE  def_variante-variant    TO  g_f_vari_flt.
*   save the initial display variant for the flat list      "n579976
    MOVE  def_variante-variant    TO  g_f_vari_flt_initial. "n579976
  ENDIF.

* take the required variant
  IF      NOT pa_hsq IS INITIAL.
*   for hierarchic seq. list
    p_vari =  g_f_vari_hsq.

  ELSEIF  NOT  pa_flt IS INITIAL.
*   for flat ( simple ) list
    p_vari = g_f_vari_flt.
  ENDIF.

* Begin of note 1902840
  IF cl_db_sys=>dbsys_type = 'HDB'.
* Accelerator shall run on a HANA DB in any case
    newsel = 'X'.                                           "n2336340
* End of note 1902840
  ELSE.
* begin of secondary database settings                     "n1710852
    CALL FUNCTION 'FUNCTION_EXISTS'
      EXPORTING
        funcname           = c_hdb_dbcon_get
      EXCEPTIONS
        function_not_exist = 1
        OTHERS             = 2.
    IF sy-subrc = 0.
      CALL FUNCTION c_hdb_dbcon_get
        EXPORTING
          i_subappl = c_hdb_subappl
        IMPORTING
          e_dbcon   = dbcon.
      IF dbcon IS NOT INITIAL.
        newsel = 'X'. "set new selection active            "n2336340
      ENDIF.
    ENDIF. "sy-subrc = 0.
* end of secondary database settings                       "n1710852
  ENDIF.
  BREAK-POINT ID mmim_rep_mb52.                             "n1795093

*check for other settings to activate new selection    "v_n1795093
*1. check control table for system setting on new selection
  SELECT COUNT(*) FROM mmim_control_log WHERE action = 'MB52_NEW_SEL'
                                        AND   repid  = 'RM07MLBS'
                                        AND   status = 'X'.
  IF sy-subrc IS INITIAL.
    newsel = 'X'. "set new selection active
  ENDIF.
*2. check control table for system setting on new joined selection
  SELECT COUNT(*) FROM mmim_control_log WHERE action = 'MB52_NJOINED'
                                        AND   repid  = 'RM07MLBS'
                                        AND   status = 'X'.
  IF sy-subrc IS INITIAL.
    newsel = '1'. "set new selection with valuation joins active
  ENDIF.
*3.check control table for system setting on new joined selection
  SELECT COUNT(*) FROM mmim_control_log WHERE action = 'MB52_COMPATI'
                                        AND   repid  = 'RM07MLBS'
                                        AND   status = 'X'.
  IF sy-subrc IS INITIAL.
    CLEAR newsel. "revert to old selection
  ENDIF.
*4.check user setting
  GET PARAMETER ID 'DONTPANIC' FIELD  lv_dontpanic.
  IF sy-subrc IS INITIAL.
    TRANSLATE : lv_dontpanic TO  UPPER CASE.             "#EC TRANSLANG
*   special user parameter for analysis is available
    FIND FIRST OCCURRENCE OF 'MB52NSEL'  IN  lv_dontpanic.
    IF  sy-subrc IS INITIAL.
      newsel = 'X'. "new selection active
    ENDIF.
    FIND FIRST OCCURRENCE OF 'MB52JSEL'  IN  lv_dontpanic.
    IF  sy-subrc IS INITIAL.
      newsel = '1'. "new selection with joined valuation active
    ENDIF.
    FIND FIRST OCCURRENCE OF 'MB52OSEL'  IN  lv_dontpanic.
    IF  sy-subrc IS INITIAL.
      CLEAR newsel. "revert back to old selection
    ENDIF.
  ENDIF.                                                    "^_n1795093
  IF   /cwm/cl_switch_check=>client( ) = /cwm/cl_switch_check=>true.
    LOG-POINT ID /cwm/enh SUBKEY to_upper( sy-tcode && '\/CWM/APPL_MM_RM07MLBS_EHP4\EHP604_RM07MLBS_05\' && sy-cprog ) FIELDS /cwm/cl_enh_layer=>get_field( ).
*
    PERFORM /cwm/set_default_trsti.
* 1858354: After re-design by note 1795093, ERP accelerator is not
* supported in an IS-CWM system, old logic in form data_selection
* should be used in this case.
    CLEAR: dbcon.                                           "1858354
    newsel = abap_false.                                    "1858354
  ENDIF.
ENHANCEMENT-POINT ehp604_rm07mlbs_05 SPOTS es_rm07mlbs .

ENDFORM.                               " INITIALISIERUNG

************************************************************************
* Main data selection routine
************************************************************************
FORM data_selection.

* Materials to be processed
  TYPES: BEGIN OF ty_mat,
           matnr      LIKE mara-matnr,
           werks      LIKE marc-werks,
           xchar      LIKE marc-xchar,
           mtart      LIKE mara-mtart,
           matkl      LIKE mara-matkl,
           meins      LIKE mara-meins,
           trame      LIKE marc-trame,
           umlmc      LIKE marc-umlmc,
           glgmg      LIKE marc-glgmg,                      "n912093
           bwesb      LIKE  marc-bwesb,                     "AC0K020254
           lvorm_mara LIKE  mara-lvorm,
           lvorm_marc LIKE  marc-lvorm.
*
  TYPES: /cwm/meins  TYPE /cwm/meins,
         /cwm/valum  TYPE /cwm/valum,
         /cwm/xcwmat TYPE mara-/cwm/xcwmat,
*
         /cwm/umlmc  LIKE  marc-/cwm/umlmc,
         /cwm/trame  LIKE marc-/cwm/trame,
*           /cwm/glgmg LIKE marc-glgmg,
         /cwm/bwesb  LIKE  marc-bwesb.

ENHANCEMENT-POINT ehp604_rm07mlbs_06 SPOTS es_rm07mlbs STATIC .
  TYPES:  END OF ty_mat.

  DATA: t_mat     TYPE ty_mat OCCURS 0 WITH HEADER LINE,
        t_batch   TYPE ty_mat OCCURS 0 WITH HEADER LINE,
        t_nobatch TYPE ty_mat OCCURS 0 WITH HEADER LINE.
*
  DATA: lth_/cwm/mat     TYPE HASHED TABLE OF ty_mat WITH UNIQUE KEY matnr werks,
        lth_/cwm/batch   TYPE HASHED TABLE OF ty_mat WITH UNIQUE KEY matnr werks,
        lth_/cwm/nobatch TYPE HASHED TABLE OF ty_mat WITH UNIQUE KEY matnr werks.
  DATA: lt_/cwm/matnr    TYPE RANGE OF mara-matnr.
  DATA: lv_/cwm/check_matnr.

ENHANCEMENT-POINT ehp604_rm07mlbs_07 SPOTS es_rm07mlbs STATIC .

* buffer for reading working tables
  DATA : l_s_mat   TYPE  ty_mat,
         l_f_matnr LIKE  makt-matnr.

  RANGES: r_sobkz FOR mkol-sobkz.

  DATA: l_cnt_matnr_i_eq     TYPE i.                        "n759412
  DATA: l_cnt_matnr_total    TYPE i.                        "n759412

************************************************************************
* Read material master data (MARA and MARC)
************************************************************************
  REFRESH collector.

* take all matching entries, do not consider the deletion
* indicator

*** BEGIN INSERT n_759412
* analyse the select-option table for material
* numbers
  CLEAR : l_cnt_matnr_total, l_cnt_matnr_i_eq.

  LOOP AT matnr.
    ADD  1             TO  l_cnt_matnr_total.

    IF  NOT matnr-low     IS INITIAL  AND
            matnr-sign    =  'I'      AND
            matnr-option  =  'EQ'     AND
            matnr-high    IS INITIAL.
*     the table contains single a material number
      ADD  1           TO  l_cnt_matnr_i_eq.
    ELSE.
      EXIT.
    ENDIF.
  ENDLOOP.
* added dynamic break-point ID MMIM_REP_MB52              "n912093
  BREAK-POINT ID mmim_rep_mb52.                             "n912093
* process SELECT command depending on the
* required material selection
  IF  l_cnt_matnr_total  > 0                 AND
      l_cnt_matnr_total  = l_cnt_matnr_i_eq.
    LOG-POINT ID /cwm/enh SUBKEY to_upper( sy-tcode && '\/CWM/APPL_MM_RM07MLBS_EHP4\EHP604_RM07MLBS_08\' && sy-cprog ) FIELDS /cwm/cl_enh_layer=>get_field( ).
    IF l_cnt_matnr_total  < 1000.
      lt_/cwm/matnr = matnr[].
      CLEAR lv_/cwm/check_matnr. " do not check after selection
    ELSE.
      lv_/cwm/check_matnr = 'X'. " do check after selection
    ENDIF.
ENHANCEMENT-SECTION ehp604_rm07mlbs_08 SPOTS es_rm07mlbs .

    "{ Begin ENHO MGV_LAMA_RM07MLBS LO-MD-MM MGV_LAMA }
    IF cl_immpn_cust=>check_mpn_active( ) = abap_true.
      SELECT mara~matnr werks xchar mtart matkl meins trame umlmc
         bwesb glgmg                       "AC0K020254  "n912093
       mara~lvorm AS lvorm_mara
       marc~lvorm AS lvorm_marc
       /cwm/valum
       /cwm/xcwmat
       /cwm/trame
       /cwm/umlmc
       /cwm/bwesb
       CONNECTION (dbcon)                                   "1792036
       INTO CORRESPONDING FIELDS OF TABLE t_mat
       FROM mara INNER JOIN marc
       ON mara~matnr = marc~matnr
       FOR ALL ENTRIES IN matnr
       WHERE mara~matnr = matnr-low
         AND werks IN werks
         AND mtart IN matart
         AND matkl IN matkla
* Modification for PIC-Supersession/MPN
         AND mfrpn IN mfrpn
         AND ekgrp IN ekgrup.
    ELSE.
      SELECT mara~matnr werks xchar mtart matkl meins trame umlmc
             bwesb glgmg                       "AC0K020254  "n912093
           mara~lvorm AS lvorm_mara
           marc~lvorm AS lvorm_marc
           /cwm/valum
           /cwm/xcwmat
           /cwm/trame
           /cwm/umlmc
           /cwm/bwesb
           CONNECTION (dbcon)                               "1710852
           INTO CORRESPONDING FIELDS OF TABLE t_mat
           FROM mara INNER JOIN marc
           ON mara~matnr = marc~matnr
           FOR ALL ENTRIES IN matnr
           WHERE mara~matnr = matnr-low
             AND werks IN werks
             AND mtart IN matart
             AND matkl IN matkla
             AND ekgrp IN ekgrup.
    ENDIF.
    "{ End ENHO MGV_LAMA_RM07MLBS LO-MD-MM MGV_LAMA }

END-ENHANCEMENT-SECTION.
  ELSE.
* END INSERT n_759412
    LOG-POINT ID /cwm/enh SUBKEY to_upper( sy-tcode && '\/CWM/APPL_MM_RM07MLBS_EHP4\EHP604_RM07MLBS_09\' && sy-cprog ) FIELDS /cwm/cl_enh_layer=>get_field( ).
    lv_/cwm/check_matnr = 'X'. " do check after selection
    lt_/cwm/matnr[] = matnr[].
ENHANCEMENT-SECTION ehp604_rm07mlbs_09 SPOTS es_rm07mlbs .

    "{ Begin ENHO MGV_LAMA_RM07MLBS LO-MD-MM MGV_LAMA }
    IF cl_immpn_cust=>check_mpn_active( ) = abap_true.

      SELECT mara~matnr werks xchar mtart matkl meins trame umlmc
             bwesb glgmg                          "AC0K020254 "912093
           mara~lvorm AS lvorm_mara
           marc~lvorm AS lvorm_marc
           /cwm/valum
           /cwm/xcwmat
           /cwm/trame
           /cwm/umlmc
           /cwm/bwesb
           CONNECTION (dbcon)                               "1792036
           INTO CORRESPONDING FIELDS OF TABLE t_mat
           FROM mara INNER JOIN marc
           ON mara~matnr = marc~matnr
           WHERE mara~matnr IN matnr
             AND werks IN werks
             AND mtart IN matart
             AND matkl IN matkla
* Modification for PIC-Supersession/MPN
             AND mfrpn IN mfrpn
             AND ekgrp IN ekgrup.

    ELSE.
      SELECT mara~matnr werks xchar mtart matkl meins trame umlmc
        bwesb glgmg                          "AC0K020254 "912093
      mara~lvorm AS lvorm_mara
      marc~lvorm AS lvorm_marc
      /cwm/valum
      /cwm/xcwmat
      /cwm/trame
      /cwm/umlmc
      /cwm/bwesb
      CONNECTION (dbcon)                                    "1710852
      INTO CORRESPONDING FIELDS OF TABLE t_mat
      FROM mara INNER JOIN marc
      ON mara~matnr = marc~matnr
      WHERE mara~matnr IN matnr
        AND werks IN werks
        AND mtart IN matart
        AND matkl IN matkla
        AND ekgrp IN ekgrup.
    ENDIF.

    "{ End ENHO MGV_LAMA_RM07MLBS LO-MD-MM MGV_LAMA }

END-ENHANCEMENT-SECTION.
  ENDIF.

  "{ Begin ENHO MGV_LAMA_RM07MLBS LO-MD-MM MGV_LAMA }
  IF cl_immpn_cust=>check_mpn_active( ) = abap_true.
* Modification for PIC-Supersession/MPN
    LOOP AT t_mat.
      MOVE-CORRESPONDING t_mat TO l_buffer.
      APPEND l_buffer.
    ENDLOOP.
    CALL FUNCTION 'MPN01_BUFFER_MATNR_MPN'
      TABLES
        t_buffer = l_buffer.
  ENDIF.
  "{ End ENHO MGV_LAMA_RM07MLBS LO-MD-MM MGV_LAMA }

ENHANCEMENT-POINT ehp604_rm07mlbs_10 SPOTS es_rm07mlbs .
************************************************************************
* Get "normal" stocks.
* If no detailed batch display is required,
* all data come from MARD. Otherwise, materials with batch
* management are extracted from MCHB, the rest from MARD.
************************************************************************
  REFRESH: t_batch, t_nobatch.
* Split the worklist into the parts for each table...
  IF xmchb IS INITIAL.
    t_nobatch[] = t_mat[].
  ELSE.
    LOOP AT t_mat.
      IF t_mat-xchar IS INITIAL.
        APPEND t_mat TO t_nobatch.
      ELSE.
        APPEND t_mat TO t_batch.
      ENDIF.
    ENDLOOP.
  ENDIF.
  LOG-POINT ID /cwm/enh SUBKEY to_upper( sy-tcode && '\/CWM/APPL_MM_RM07MLBS_EHP4\EHP604_RM07MLBS_11\' && sy-cprog ) FIELDS /cwm/cl_enh_layer=>get_field( ).
*
  REFRESH: lth_/cwm/mat, lth_/cwm/batch, lth_/cwm/nobatch.
  INSERT LINES OF t_mat INTO TABLE lth_/cwm/mat.
  INSERT LINES OF t_batch INTO TABLE lth_/cwm/batch.
  INSERT LINES OF t_nobatch INTO TABLE lth_/cwm/nobatch.

ENHANCEMENT-POINT ehp604_rm07mlbs_11 SPOTS es_rm07mlbs .
* Access MARD
* I you think that instead of SELECT-APPEND we could have used
* an array fetch, please wait for the table names to become different
* from the internal fields. B.T.W.: The DB-interface also buffers.
  CLEAR collector.
  READ TABLE t_nobatch INDEX 1 TRANSPORTING NO FIELDS.
  IF sy-subrc = 0.
ENHANCEMENT-SECTION ehp604_rm07mlbs_12 SPOTS es_rm07mlbs .
    LOG-POINT ID /cwm/enh SUBKEY to_upper( sy-tcode && '\/CWM/APPL_MM_RM07MLBS_EHP4\EHP604_RM07MLBS_12\' && sy-cprog ) FIELDS /cwm/cl_enh_layer=>get_field( ).
    SELECT matnr werks lgort
           labst umlme insme einme speme retme lvorm
      /cwm/labst
      /cwm/umlme
      /cwm/insme
      /cwm/einme
      /cwm/speme
      /cwm/retme
      CONNECTION (dbcon)                                "1791847/1710852
      INTO CORRESPONDING FIELDS OF collector
           FROM mard
           WHERE matnr IN lt_/cwm/matnr
             AND werks IN werks
             AND lgort IN lgort.
      READ TABLE lth_/cwm/nobatch
      WITH TABLE KEY matnr = collector-matnr werks = collector-werks
       TRANSPORTING NO FIELDS.
      CHECK sy-subrc = 0.

*     save the MARD Key and deletion indicator for later
*     in table G_T_MARD_LV for use with special stocks
      IF NOT pa_sond         IS INITIAL  AND
         NOT collector-lvorm IS INITIAL.
        MOVE-CORRESPONDING  collector
                             TO  g_s_mard_lv.
        INSERT  g_s_mard_lv  INTO  TABLE  g_t_mard_lv.
      ENDIF.

      PERFORM                f2000_collect_collector.
    ENDSELECT.
END-ENHANCEMENT-SECTION.
  ENDIF.

* Access MCHB
  CLEAR collector.
  READ TABLE t_batch INDEX 1 TRANSPORTING NO FIELDS.
  IF sy-subrc = 0.
ENHANCEMENT-SECTION ehp604_rm07mlbs_13 SPOTS es_rm07mlbs .
    LOG-POINT ID /cwm/enh SUBKEY to_upper( sy-tcode && '\/CWM/APPL_MM_RM07MLBS_EHP4\EHP604_RM07MLBS_13\' && sy-cprog ) FIELDS /cwm/cl_enh_layer=>get_field( ).
    IF cl_ops_switch_check=>sfsw_segmentation( ) EQ abap_on.
      SELECT matnr werks lgort charg
        clabs  AS labst
        cumlm  AS umlme
        cinsm  AS insme
        ceinm  AS einme
        cspem  AS speme
        cretm  AS retme
        lvorm
        sgt_scat
        /cwm/clabs AS /cwm/labst
        /cwm/cumlm AS /cwm/umlme
        /cwm/cinsm AS /cwm/insme
        /cwm/ceinm AS /cwm/einme
        /cwm/cspem AS /cwm/speme
        /cwm/cretm AS /cwm/retme
        CONNECTION (dbcon)                                "1791847/1710852
        INTO CORRESPONDING FIELDS OF  collector
             FROM mchb
             WHERE matnr IN lt_/cwm/matnr
               AND werks IN werks
               AND lgort IN lgort
               AND charg IN charg.
        READ TABLE lth_/cwm/batch
        WITH TABLE KEY matnr = collector-matnr werks = collector-werks
         TRANSPORTING NO FIELDS.
        CHECK sy-subrc = 0.

        PERFORM                f2000_collect_collector.
      ENDSELECT.
    ELSE.
      SELECT matnr werks lgort charg
        clabs  AS labst
        cumlm  AS umlme
        cinsm  AS insme
        ceinm  AS einme
        cspem  AS speme
        cretm  AS retme
        lvorm
*        sgt_scat
        /cwm/clabs AS /cwm/labst
        /cwm/cumlm AS /cwm/umlme
        /cwm/cinsm AS /cwm/insme
        /cwm/ceinm AS /cwm/einme
        /cwm/cspem AS /cwm/speme
        /cwm/cretm AS /cwm/retme
        CONNECTION (dbcon)                                "1791847/1710852
        INTO CORRESPONDING FIELDS OF  collector
             FROM mchb
             WHERE matnr IN lt_/cwm/matnr
               AND werks IN werks
               AND lgort IN lgort
               AND charg IN charg.
        READ TABLE lth_/cwm/batch
        WITH TABLE KEY matnr = collector-matnr werks = collector-werks
         TRANSPORTING NO FIELDS.
        CHECK sy-subrc = 0.

        PERFORM                f2000_collect_collector.
      ENDSELECT.
    ENDIF.
END-ENHANCEMENT-SECTION.
  ENDIF.

************************************************************************
* Transfer stocks from MARC (TRAME, UMLMC)
************************************************************************
  CLEAR collector.

ENHANCEMENT-SECTION ehp604_rm07mlbs_14 SPOTS es_rm07mlbs .
  LOG-POINT ID /cwm/enh SUBKEY to_upper( sy-tcode && '\/CWM/APPL_MM_RM07MLBS_EHP4\EHP604_RM07MLBS_14\' && sy-cprog ) FIELDS /cwm/cl_enh_layer=>get_field( ).
  LOOP AT t_mat WHERE umlmc <> 0 OR trame <> 0 OR           "AC0K020254
                 /cwm/umlmc <> 0 OR /cwm/trame <> 0 OR
                    bwesb <> 0 OR                           "AC0K020254
                    /cwm/bwesb <> 0 OR                    " EHP5
                    glgmg <> 0.                             "n912093
*     there are no lines with stock = zero

*     take the stocks from plant level only when the user   "n577268
*     does not restrict the storage location;               "n577268
    CHECK : g_flag_suppress_init_lgort IS INITIAL.          "n577268

    IF negativ = 'X'.
*       ignore entry if all stocks are zero or greater
      IF  t_mat-trame >= 0 AND
          t_mat-umlmc >= 0 AND
          t_mat-glgmg >= 0.                                 "n912093
        IF  t_mat-/cwm/trame >= 0 AND
                  t_mat-/cwm/umlmc >= 0
*          AND
*                  t_mat-/cwm/glgmg >= 0
          .
          CONTINUE.          "take the next entry
        ENDIF.
      ENDIF.
    ENDIF.
    IF t_mat-/cwm/xcwmat IS NOT INITIAL.
      CALL FUNCTION '/CWM/MDMM_2TQ_BASE_UNIT_GET'
        EXPORTING
          i_material         = t_mat-matnr
        IMPORTING
          e_base_unit_of_2tq = t_mat-/cwm/meins
        EXCEPTIONS
          OTHERS             = 0.
    ENDIF.

    collector-matnr = t_mat-matnr.
    collector-werks = t_mat-werks.
    collector-umlme = t_mat-trame + t_mat-umlmc.
    collector-/cwm/umlme = t_mat-/cwm/trame + t_mat-/cwm/umlmc.
    collector-lvorm = t_mat-lvorm_marc.
    collector-bwesb = t_mat-bwesb.                          "AC0K020254
    collector-glgmg = t_mat-glgmg.                          "n912093
    collector-trame = t_mat-trame.                          "n912093
    collector-umlmc = t_mat-umlmc.                          "n912093
    collector-/cwm/trame = t_mat-/cwm/trame.                "n912093
    collector-/cwm/umlmc = t_mat-/cwm/umlmc.                "n912093
    collector-/cwm/bwesb = t_mat-/cwm/bwesb.                "AC0K020254
*    collector-/cwm/glgmg = t_mat-/cwm/glgmg.              "n912093
    collector-/cwm/meins = t_mat-/cwm/meins.
    collector-/cwm/valum = t_mat-/cwm/valum.
    collector-/cwm/xcwmat = t_mat-/cwm/xcwmat.
    APPEND collector.

  ENDLOOP.
END-ENHANCEMENT-SECTION.

************************************************************************
* Consignment from vendor (MKOL)
* Read only if requested by one of the
* flags on the selection screen. Absolutely inconsistent, but
* due to compatibility...
* MKOL has a flag for deletion
************************************************************************

  IF  NOT pa_sond IS INITIAL  AND
      NOT t_mat[] IS INITIAL.
    IF  'K' IN so_sobkz  OR
        'M' IN so_sobkz.
      CLEAR                  collector.

ENHANCEMENT-SECTION ehp604_rm07mlbs_15 SPOTS es_rm07mlbs .
      LOG-POINT ID /cwm/enh SUBKEY to_upper( sy-tcode && '\/CWM/APPL_MM_RM07MLBS_EHP4\EHP604_RM07MLBS_15\' && sy-cprog ) FIELDS /cwm/cl_enh_layer=>get_field( ).
      SELECT matnr werks lgort charg sobkz lifnr lvorm
             slabs AS labst
             sinsm AS insme
             seinm AS einme
             sspem AS speme
             /cwm/slabs AS /cwm/labst
             /cwm/sinsm AS /cwm/insme
             /cwm/seinm AS /cwm/einme
             /cwm/sspem AS /cwm/speme
             CONNECTION (dbcon)                         "1791847/1710852
             INTO CORRESPONDING FIELDS OF  collector
             FROM mkol
             WHERE matnr IN lt_/cwm/matnr
               AND werks IN werks
               AND lgort IN lgort
               AND charg IN charg
               AND sobkz IN so_sobkz.
        IF lv_/cwm/check_matnr IS NOT INITIAL.
          READ TABLE lth_/cwm/mat
          WITH TABLE KEY matnr = collector-matnr werks = collector-werks
           TRANSPORTING NO FIELDS.
          CHECK sy-subrc = 0.
        ENDIF.

        PERFORM              f2000_collect_collector.
      ENDSELECT.
END-ENHANCEMENT-SECTION.
    ENDIF.
  ENDIF.

************************************************************************
* Special stocks at customer side (MSKU)
* MSKU has no flag for deletion
************************************************************************

  IF  NOT  pa_sond IS INITIAL  AND
      NOT  t_mat[] IS INITIAL.
    IF  'V' IN so_sobkz  OR
        'W' IN so_sobkz.
      CLEAR collector.

ENHANCEMENT-SECTION ehp604_rm07mlbs_16 SPOTS es_rm07mlbs .
      LOG-POINT ID /cwm/enh SUBKEY to_upper( sy-tcode && '\/CWM/APPL_MM_RM07MLBS_EHP4\EHP604_RM07MLBS_16\' && sy-cprog ) FIELDS /cwm/cl_enh_layer=>get_field( ).
      IF cl_ops_switch_check=>sfsw_segmentation( ) EQ abap_on.
        SELECT matnr werks charg sobkz kunnr
       kulab  AS labst
       kuins  AS insme
       kuein  AS einme
       kuuml  AS umlme
       sgt_scat
       /cwm/kulab  AS /cwm/labst
       /cwm/kuins  AS /cwm/insme
       /cwm/kuein  AS /cwm/einme
       /cwm/kuuml  AS /cwm/umlme
       CONNECTION (dbcon)                           "1791847/1710852
       INTO CORRESPONDING FIELDS OF collector
       FROM msku
       WHERE matnr IN lt_/cwm/matnr
         AND werks IN werks
         AND charg IN charg
         AND sobkz  IN  so_sobkz.
          IF lv_/cwm/check_matnr IS NOT INITIAL.
            READ TABLE lth_/cwm/mat
            WITH TABLE KEY matnr = collector-matnr werks = collector-werks
             TRANSPORTING NO FIELDS.
            CHECK sy-subrc = 0.
          ENDIF.

          PERFORM              f2000_collect_collector.
        ENDSELECT.
      ELSE.
        SELECT matnr werks charg sobkz kunnr
       kulab  AS labst
       kuins  AS insme
       kuein  AS einme
       kuuml  AS umlme
*       sgt_scat
       /cwm/kulab  AS /cwm/labst
       /cwm/kuins  AS /cwm/insme
       /cwm/kuein  AS /cwm/einme
       /cwm/kuuml  AS /cwm/umlme
       CONNECTION (dbcon)                           "1791847/1710852
       INTO CORRESPONDING FIELDS OF collector
       FROM msku
       WHERE matnr IN lt_/cwm/matnr
         AND werks IN werks
         AND charg IN charg
         AND sobkz  IN  so_sobkz.
          IF lv_/cwm/check_matnr IS NOT INITIAL.
            READ TABLE lth_/cwm/mat
            WITH TABLE KEY matnr = collector-matnr werks = collector-werks
             TRANSPORTING NO FIELDS.
            CHECK sy-subrc = 0.
          ENDIF.

          PERFORM              f2000_collect_collector.
        ENDSELECT.
      ENDIF.
END-ENHANCEMENT-SECTION.
    ENDIF.
  ENDIF.

************************************************************************
* Special stocks at vendor provision (MSLB)
* MSLB has no flag for deletion
************************************************************************

  IF  NOT  pa_sond IS INITIAL  AND
      NOT  t_mat[] IS INITIAL  AND
           'O'     IN so_sobkz.
    CLEAR collector.

ENHANCEMENT-SECTION ehp604_rm07mlbs_17 SPOTS es_rm07mlbs .
    LOG-POINT ID /cwm/enh SUBKEY to_upper( sy-tcode && '\/CWM/APPL_MM_RM07MLBS_EHP4\EHP604_RM07MLBS_17\' && sy-cprog ) FIELDS /cwm/cl_enh_layer=>get_field( ).
    SELECT matnr werks charg sobkz lifnr
           lblab AS labst
           lbins AS insme
           lbein AS einme
           lbuml AS umlme                       " OM EHP604
           /cwm/lblab AS /cwm/labst
           /cwm/lbins AS /cwm/insme
           /cwm/lbein AS /cwm/einme
           /cwm/lbuml AS /cwm/umlme             " OM EHP604
           CONNECTION (dbcon)                           "1791847/1710852
           INTO CORRESPONDING FIELDS OF collector
           FROM mslb
           WHERE matnr IN lt_/cwm/matnr
             AND werks IN werks
             AND charg IN charg
             AND sobkz  IN  so_sobkz.
      IF lv_/cwm/check_matnr IS NOT INITIAL.
        READ TABLE lth_/cwm/mat
        WITH TABLE KEY matnr = collector-matnr werks = collector-werks
         TRANSPORTING NO FIELDS.
        CHECK sy-subrc = 0.
      ENDIF.

      PERFORM                f2000_collect_collector.
    ENDSELECT.
END-ENHANCEMENT-SECTION.
  ENDIF.

************************************************************************
* Customer order stock (MSKA) and sum segment (MSSA) for valuation.
* Sum on the database and FOR ALL ENTRIES is not allowed from
* release 4.5 onwards, so the summation has to be done
* on the application server (here!).
* MSKA has no flag for deletion
************************************************************************

  IF  NOT  pa_sond IS INITIAL  AND
      NOT  t_mat[] IS INITIAL  AND
           'E'     IN so_sobkz.
    CLEAR collector.

ENHANCEMENT-SECTION ehp604_rm07mlbs_18 SPOTS es_rm07mlbs .
    LOG-POINT ID /cwm/enh SUBKEY to_upper( sy-tcode && '\/CWM/APPL_MM_RM07MLBS_EHP4\EHP604_RM07MLBS_18\' && sy-cprog ) FIELDS /cwm/cl_enh_layer=>get_field( ).
    IF cl_ops_switch_check=>sfsw_segmentation( ) EQ abap_on.
      SELECT mska~matnr mska~werks lgort charg mska~sobkz
           mska~vbeln mska~posnr kzbws
           kalab AS labst
           kains AS insme
           kaspe AS speme
           kaein AS einme
           sgt_scat
           /cwm/kalab AS /cwm/labst
           /cwm/kains AS /cwm/insme
           /cwm/kaspe AS /cwm/speme
           /cwm/kaein AS /cwm/einme
           CONNECTION (dbcon)                           "1791847/1710852
           INTO CORRESPONDING FIELDS OF collector
           FROM mska INNER JOIN mssa
           ON   mska~matnr = mssa~matnr
            AND mska~werks = mssa~werks
            AND mska~sobkz = mssa~sobkz
            AND mska~vbeln = mssa~vbeln
            AND mska~posnr = mssa~posnr
           WHERE mska~matnr IN lt_/cwm/matnr
             AND mska~werks IN werks
             AND mska~sobkz = 'E'
             AND mska~lgort IN lgort
             AND mska~charg IN charg.
        IF lv_/cwm/check_matnr IS NOT INITIAL.
          READ TABLE lth_/cwm/mat
          WITH TABLE KEY matnr = collector-matnr werks = collector-werks
           TRANSPORTING NO FIELDS.
          CHECK sy-subrc = 0.
        ENDIF.

        PERFORM                f2000_collect_collector.
      ENDSELECT.
    ELSE.
      SELECT mska~matnr mska~werks lgort charg mska~sobkz
         mska~vbeln mska~posnr kzbws
         kalab AS labst
         kains AS insme
         kaspe AS speme
         kaein AS einme
*         sgt_scat
         /cwm/kalab AS /cwm/labst
         /cwm/kains AS /cwm/insme
         /cwm/kaspe AS /cwm/speme
         /cwm/kaein AS /cwm/einme
         CONNECTION (dbcon)                           "1791847/1710852
         INTO CORRESPONDING FIELDS OF collector
         FROM mska INNER JOIN mssa
         ON   mska~matnr = mssa~matnr
          AND mska~werks = mssa~werks
          AND mska~sobkz = mssa~sobkz
          AND mska~vbeln = mssa~vbeln
          AND mska~posnr = mssa~posnr
         WHERE mska~matnr IN lt_/cwm/matnr
           AND mska~werks IN werks
           AND mska~sobkz = 'E'
           AND mska~lgort IN lgort
           AND mska~charg IN charg.
        IF lv_/cwm/check_matnr IS NOT INITIAL.
          READ TABLE lth_/cwm/mat
          WITH TABLE KEY matnr = collector-matnr werks = collector-werks
           TRANSPORTING NO FIELDS.
          CHECK sy-subrc = 0.
        ENDIF.

        PERFORM                f2000_collect_collector.
      ENDSELECT.
    ENDIF.
END-ENHANCEMENT-SECTION.

*   Transfer stocks for customer order (SATRA in MSSA)
    CLEAR collector.
ENHANCEMENT-SECTION ehp604_rm07mlbs_19 SPOTS es_rm07mlbs .
    LOG-POINT ID /cwm/enh SUBKEY to_upper( sy-tcode && '\/CWM/APPL_MM_RM07MLBS_EHP4\EHP604_RM07MLBS_19\' && sy-cprog ) FIELDS /cwm/cl_enh_layer=>get_field( ).
    SELECT matnr werks sobkz vbeln posnr kzbws
          satra AS umlme
          /cwm/satra AS /cwm/umlme
          CONNECTION (dbcon)                           "1791847/1710852
          INTO CORRESPONDING FIELDS OF collector
          FROM mssa
          WHERE matnr IN lt_/cwm/matnr
            AND werks IN werks
            AND sobkz  IN  so_sobkz
            AND satra <> 0.
      IF lv_/cwm/check_matnr IS NOT INITIAL.
        READ TABLE lth_/cwm/mat
        WITH TABLE KEY matnr = collector-matnr werks = collector-werks
         TRANSPORTING NO FIELDS.
        CHECK sy-subrc = 0.
      ENDIF.

      PERFORM                f2000_collect_collector.
    ENDSELECT.
END-ENHANCEMENT-SECTION.
  ENDIF.

************************************************************************
* The same game for project stocks (MSPR/MSSQ).
* MSPR has no flag for deletion
************************************************************************

  IF  NOT  pa_sond IS INITIAL  AND
      NOT  t_mat[] IS INITIAL  AND
           'Q'     IN so_sobkz.
    CLEAR collector.

ENHANCEMENT-SECTION ehp604_rm07mlbs_20 SPOTS es_rm07mlbs .
    LOG-POINT ID /cwm/enh SUBKEY to_upper( sy-tcode && '\/CWM/APPL_MM_RM07MLBS_EHP4\EHP604_RM07MLBS_20\' && sy-cprog ) FIELDS /cwm/cl_enh_layer=>get_field( ).
    SELECT mspr~matnr mspr~werks lgort charg mspr~sobkz mspr~pspnr
           kzbws
           prlab AS labst
           prins AS insme
           prspe AS speme
           prein AS einme
           /cwm/prlab AS /cwm/labst
           /cwm/prins AS /cwm/insme
           /cwm/prspe AS /cwm/speme
           /cwm/prein AS /cwm/einme
           CONNECTION (dbcon)                           "1791847/1710852
           INTO CORRESPONDING FIELDS OF collector
           FROM mspr INNER JOIN mssq
           ON   mspr~matnr = mssq~matnr
            AND mspr~werks = mssq~werks
            AND mspr~sobkz = mssq~sobkz
            AND mspr~pspnr = mssq~pspnr
           WHERE mspr~matnr IN lt_/cwm/matnr
             AND mspr~werks IN werks
             AND mspr~lgort IN lgort
             AND mspr~charg IN charg.
      IF lv_/cwm/check_matnr IS NOT INITIAL.
        READ TABLE lth_/cwm/mat
        WITH TABLE KEY matnr = collector-matnr werks = collector-werks
         TRANSPORTING NO FIELDS.
        CHECK sy-subrc = 0.
      ENDIF.

      PERFORM                f2000_collect_collector.
    ENDSELECT.
END-ENHANCEMENT-SECTION.

*   Transfer stocks for projects (SQTRA in MSSQ)
    CLEAR collector.
ENHANCEMENT-SECTION ehp604_rm07mlbs_21 SPOTS es_rm07mlbs .
    LOG-POINT ID /cwm/enh SUBKEY to_upper( sy-tcode && '\/CWM/APPL_MM_RM07MLBS_EHP4\EHP604_RM07MLBS_21\' && sy-cprog ) FIELDS /cwm/cl_enh_layer=>get_field( ).
    SELECT matnr werks sobkz pspnr kzbws
          sqtra AS umlme
*          /cwm/sqtra as /cwm/umlme
           CONNECTION (dbcon)                           "1791847/1710852
           INTO CORRESPONDING FIELDS OF collector
           FROM mssq
           WHERE matnr IN lt_/cwm/matnr
             AND werks IN werks
             AND sobkz  IN  so_sobkz
             AND sqtra <> 0.
      IF lv_/cwm/check_matnr IS NOT INITIAL.
        READ TABLE lth_/cwm/mat
        WITH TABLE KEY matnr = collector-matnr werks = collector-werks
         TRANSPORTING NO FIELDS.
        CHECK sy-subrc = 0.
      ENDIF.

      PERFORM                f2000_collect_collector.
    ENDSELECT.
END-ENHANCEMENT-SECTION.
  ENDIF.

  "{ Begin ENHO DIPCS_RM07MLBS001 IS-AD-SSP AD_SUB }
* DI A&D PCS
************************************************************************
* DI IS-ADEC-SSP Customer Stocks(MCSD/MCSS).
* MCSD has no flag for deletion
************************************************************************
  DATA: lv_flag TYPE c.                                     "GA1727687

  IF  NOT  pa_sond IS INITIAL  AND
      NOT  t_mat[] IS INITIAL  AND
           'B'     IN so_sobkz.
    CLEAR collector.
*    perform hdb_check_table using 'MCSD' c_blank.       "SH note 1790860 / 1856829
    SELECT mcsd~matnr mcsd~werks lgort charg mcsd~sobkz
           mcsd~kunnr
           sdlab sdins sdspe sdein kzbws
           CONNECTION (dbcon)                           "SH note 1790860
           INTO (collector-matnr, collector-werks, collector-lgort,
                 collector-charg, collector-sobkz,
                 collector-kunnr,
                 collector-labst, collector-insme, collector-speme,
                 collector-einme, collector-kzbws)
           FROM mcsd INNER JOIN mcss
           ON   mcsd~matnr = mcss~matnr
            AND mcsd~werks = mcss~werks
            AND mcsd~sobkz = mcss~sobkz
            AND mcsd~kunnr = mcss~kunnr
           FOR ALL ENTRIES IN t_mat
           WHERE mcsd~matnr = t_mat-matnr
             AND mcsd~werks = t_mat-werks
             AND mcsd~lgort IN lgort
             AND mcsd~charg IN charg.

      PERFORM                f2000_collect_collector.
    ENDSELECT.

*   Transfer stocks for customer stock (SSTRA in MCSS)
    CLEAR collector.
*    perform hdb_check_table using 'MCSS' c_blank.       "SH note 1790860 / 1856829
    SELECT matnr werks sobkz kunnr kzbws sstra
           CONNECTION (dbcon)                           "SH note 1790860
          INTO (collector-matnr, collector-werks, collector-sobkz,
                 collector-kunnr,
                 collector-kzbws, collector-umlme)
           FROM mcss
           FOR ALL ENTRIES IN t_mat
           WHERE matnr = t_mat-matnr
             AND werks = t_mat-werks
             AND sobkz  IN  so_sobkz
             AND sstra <> 0.

      PERFORM                f2000_collect_collector.
    ENDSELECT.
  ENDIF.
*                                                         "v_n_GA1727687
* DI IS-ADEC-SUB customer stock at subcontractor (MSCS/MSCD).
  IF  NOT  pa_sond IS INITIAL  AND
      NOT  t_mat[] IS INITIAL  AND
           cl_adsub_constants=>c IN so_sobkz.
    CLEAR collector.
    SELECT mscd~matnr mscd~werks mscd~charg mscd~sobkz
           mscd~kunnr mscd~lifnr
           mscd~cdlab mscd~cdins mscd~cdein
           INTO (collector-matnr, collector-werks,
                 collector-charg, collector-sobkz,
                 collector-kunnr,
                 collector-lifnr2,
                 collector-labst, collector-insme,
                 collector-einme)
           FROM ( mscd INNER JOIN mscs
           ON   mscd~matnr = mscs~matnr
            AND mscd~werks = mscs~werks
            AND mscd~sobkz = mscs~sobkz
            AND mscd~kunnr = mscs~kunnr )
           FOR ALL ENTRIES IN t_mat
           WHERE mscd~matnr = t_mat-matnr
             AND mscd~werks = t_mat-werks
             AND mscd~charg IN charg.

      PERFORM                f2000_collect_collector.
    ENDSELECT.
  ENDIF.
* DI IS-ADEC-SUB sales order stock at subcontractor (MSFS/MSFD).
  IF  NOT  pa_sond IS INITIAL  AND
      NOT  t_mat[] IS INITIAL  AND
           cl_adsub_constants=>f IN so_sobkz.
    CLEAR collector.
    SELECT msfd~matnr msfd~werks msfd~charg msfd~sobkz
           msfd~vbeln msfd~posnr msfd~lifnr
           msfd~fdlab msfd~fdins msfd~fdein mssa~kzbws
           INTO (collector-matnr, collector-werks,
                 collector-charg, collector-sobkz,
                 collector-vbeln,
                 collector-posnr,
                 collector-lifnr2,
                 collector-labst, collector-insme,
                 collector-einme, collector-kzbws)
           FROM ( msfd INNER JOIN msfs
           ON   msfd~matnr = msfs~matnr
            AND msfd~werks = msfs~werks
            AND msfd~sobkz = msfs~sobkz
            AND msfd~vbeln = msfs~vbeln
            AND msfd~posnr = msfs~posnr )
           INNER JOIN mssa
           ON   msfs~matnr = mssa~matnr
            AND msfs~werks = mssa~werks
            AND msfs~vbeln = mssa~vbeln
            AND msfs~posnr = mssa~posnr
           FOR ALL ENTRIES IN t_mat
           WHERE msfd~matnr = t_mat-matnr
             AND msfd~werks = t_mat-werks
             AND msfd~charg IN charg
             AND mssa~sobkz = cl_adsub_constants=>e.

      PERFORM                f2000_collect_collector.
    ENDSELECT.
  ENDIF.
* DI IS-ADEC-SUB consignment or RTP stock at subcontractor (MSIS/MSID).
  IF  NOT  pa_sond IS INITIAL  AND
      NOT  t_mat[] IS INITIAL  AND
         ( cl_adsub_constants=>i IN so_sobkz OR
           cl_adsub_constants=>j IN so_sobkz ).
    CLEAR collector.
    SELECT msid~matnr msid~werks msid~charg msid~sobkz
           msid~owner msid~lifnr
           msid~idlab msid~idins msid~idein
           INTO (collector-matnr, collector-werks,
                 collector-charg, collector-sobkz,
                 collector-owner,
                 collector-lifnr2,
                 collector-labst, collector-insme,
                 collector-einme)
           FROM ( msid INNER JOIN msis
           ON   msid~matnr = msis~matnr
            AND msid~werks = msis~werks
            AND msid~sobkz = msis~sobkz
            AND msid~owner = msis~owner )
           FOR ALL ENTRIES IN t_mat
           WHERE msid~matnr = t_mat-matnr
             AND msid~werks = t_mat-werks
             AND msid~charg IN charg.

      PERFORM                f2000_collect_collector.
    ENDSELECT.
  ENDIF.
* DI IS-ADEC-SUB project stock at subcontractor (MSRS/MSRD).
  IF  NOT  pa_sond IS INITIAL  AND
      NOT  t_mat[] IS INITIAL  AND
           cl_adsub_constants=>r IN so_sobkz.
    CLEAR collector.
    SELECT msrd~matnr msrd~werks msrd~charg msrd~sobkz
           msrd~pspnr msrd~lifnr
           msrd~rdlab msrd~rdins msrd~rdein mssq~kzbws
           INTO (collector-matnr, collector-werks,
                 collector-charg, collector-sobkz,
                 collector-pspnr,
                 collector-lifnr2,
                 collector-labst, collector-insme,
                 collector-einme, collector-kzbws)
           FROM ( msrd INNER JOIN msrs
           ON   msrd~matnr = msrs~matnr
            AND msrd~werks = msrs~werks
            AND msrd~sobkz = msrs~sobkz
            AND msrd~pspnr = msrs~pspnr )
           INNER JOIN mssq
           ON   msrs~matnr = mssq~matnr
            AND msrs~werks = mssq~werks
            AND msrs~pspnr = mssq~pspnr
           FOR ALL ENTRIES IN t_mat
           WHERE msrd~matnr = t_mat-matnr
             AND msrd~werks = t_mat-werks
             AND msrd~charg IN charg
             AND mssq~sobkz  = cl_adsub_constants=>q.

      PERFORM                f2000_collect_collector.
    ENDSELECT.
  ENDIF.
*                                                         "^_n_GA1727687
  "{ End ENHO DIPCS_RM07MLBS001 IS-AD-SSP AD_SUB }

ENHANCEMENT-POINT rm07mlbs_02 SPOTS es_rm07mlbs.

************************************************************************
* Extract key-data for other tables.
************************************************************************
  DATA: BEGIN OF t_maktkey OCCURS 0,
          matnr LIKE makt-matnr,
        END OF t_maktkey,

* working area for the material description
        BEGIN OF l_s_makt,
          matnr LIKE makt-matnr,
          maktx LIKE makt-maktx,
        END OF l_s_makt,

        BEGIN OF t_makt OCCURS 0,
          matnr LIKE makt-matnr,
          maktx LIKE makt-maktx,
        END OF t_makt,

        BEGIN OF t_mchakey OCCURS 0,
          matnr LIKE mcha-matnr,
          werks LIKE mcha-werks,
          charg LIKE mcha-charg,
        END OF t_mchakey,
        BEGIN OF t_mcha OCCURS 0,
          matnr LIKE mcha-matnr,
          werks LIKE mcha-werks,
          charg LIKE mcha-charg,
          bwtar LIKE mcha-bwtar,
        END OF t_mcha.

  REFRESH: t_maktkey, t_mchakey, bestand.
* remove all items in bestand with wrong batch number. If we would
* not remove this, report will e.g. show materials which has
* only MARD entries, too.
  LOOP AT collector WHERE charg IN charg.               "note 311770
    MOVE-CORRESPONDING collector TO bestand.

*   fill the key of the special stocks into the field
*   assigment
ENHANCEMENT-SECTION     rm07mlbs_03 SPOTS es_rm07mlbs.

    "{ Begin ENHO DIPCS_RM07MLBS001 IS-AD-SSP AD_SUB }
* DI A&D PCS
    CASE    collector-sobkz.
      WHEN  'E'.
        MOVE  : 'X'               TO  g_flag_sobkz-vbeln.
        WRITE : collector-vbeln   TO  bestand-ssnum.
        MOVE  : '/'               TO  bestand-ssnum+10(01).
        WRITE : collector-posnr   TO  bestand-ssnum+12(08)
                                  NO-ZERO.
        CONDENSE                  bestand-ssnum.

      WHEN  'T'.
        MOVE  : 'X'               TO  g_flag_sobkz-vbeln.
        WRITE : collector-vbeln   TO  bestand-ssnum.
        MOVE  : '/'               TO  bestand-ssnum+10(01).
        WRITE : collector-posnr   TO  bestand-ssnum+12(08)
                                  NO-ZERO.
        CONDENSE                  bestand-ssnum.

      WHEN  'K'.
        MOVE  : 'X'               TO  g_flag_sobkz-lifnr.
        WRITE : collector-lifnr   TO  bestand-ssnum.

      WHEN  'M'.
        MOVE  : 'X'               TO  g_flag_sobkz-lifnr.
        WRITE : collector-lifnr   TO  bestand-ssnum.

      WHEN  'O'.
        MOVE  : 'X'               TO  g_flag_sobkz-lifnr.
        WRITE : collector-lifnr   TO  bestand-ssnum.

      WHEN  'Q'.
        MOVE  : 'X'               TO  g_flag_sobkz-pspnr.
        WRITE : collector-pspnr   TO  bestand-ssnum.

      WHEN  'V'.
        MOVE  : 'X'               TO  g_flag_sobkz-kunnr.
        WRITE : collector-kunnr   TO  bestand-ssnum.

      WHEN  'W'.
        MOVE  : 'X'               TO  g_flag_sobkz-kunnr.
        WRITE : collector-kunnr   TO  bestand-ssnum.

** DI IS-ADEC-SSP Customer Stock      " note 669630
      WHEN  'B'.
        MOVE  : 'X'               TO  g_flag_sobkz-kunnr.
        WRITE : collector-kunnr   TO  bestand-ssnum.

** DI IS-ADEC-SUB customer stock at subcontractor         "v_n_GA1727687
      WHEN  cl_adsub_constants=>c.
        MOVE  : 'X'               TO  g_flag_sobkz-kunnr.
        WRITE : collector-kunnr   TO  bestand-ssnum.
        MOVE  : 'X'               TO  g_flag_sobkz-lifnr.
        WRITE : collector-lifnr2  TO  bestand-lifnr2.
** DI IS-ADEC-SUB sales order stock at subcontractor
      WHEN  cl_adsub_constants=>f.
        MOVE  : 'X'               TO  g_flag_sobkz-vbeln.
        WRITE : collector-vbeln   TO  bestand-ssnum.
        MOVE  : '/'               TO  bestand-ssnum+10(01).
        WRITE : collector-posnr   TO  bestand-ssnum+12(08)
                                  NO-ZERO.
        CONDENSE                  bestand-ssnum.
        MOVE  : 'X'               TO  g_flag_sobkz-lifnr.
        WRITE : collector-lifnr2  TO  bestand-lifnr2.
** DI IS-ADEC-SUB consignment or RTP stock at subcontractor
      WHEN  cl_adsub_constants=>i OR cl_adsub_constants=>j.
        MOVE  : 'X'               TO  g_flag_sobkz-kunnr.
        WRITE : collector-owner   TO  bestand-ssnum.
        MOVE  : 'X'               TO  g_flag_sobkz-lifnr.
        WRITE : collector-lifnr2  TO  bestand-lifnr2.
** DI IS-ADEC-SUB project stock at subcontractor
      WHEN  cl_adsub_constants=>r.
        MOVE  : 'X'               TO  g_flag_sobkz-pspnr.
        WRITE : collector-pspnr   TO  bestand-ssnum.
        MOVE  : 'X'               TO  g_flag_sobkz-lifnr.
        WRITE : collector-lifnr2  TO  bestand-lifnr2.
** DI IS-ADEC-SUB                                         "^_n_GA1727687

      WHEN  OTHERS.
        CLEAR                     bestand-ssnum.
    ENDCASE.

    "{ End ENHO DIPCS_RM07MLBS001 IS-AD-SSP AD_SUB }

END-ENHANCEMENT-SECTION.

ENHANCEMENT-POINT rm07mlbs_12 SPOTS es_rm07mlbs .
    APPEND bestand.

    t_maktkey-matnr = bestand-matnr.
    COLLECT t_maktkey.

    IF bestand-charg <> space.
      t_mchakey-matnr = bestand-matnr.
      t_mchakey-werks = bestand-werks.
      t_mchakey-charg = bestand-charg.
      COLLECT t_mchakey.
    ENDIF.
  ENDLOOP.

  FREE                       collector.

************************************************************************
* Read additional tables
************************************************************************
  READ TABLE t_maktkey INDEX 1 TRANSPORTING NO FIELDS.
  IF sy-subrc = 0.
    SELECT matnr maktx INTO CORRESPONDING FIELDS OF TABLE t_makt
           FROM makt
           CONNECTION (dbcon)                               "1710852
           FOR ALL ENTRIES IN t_maktkey
           WHERE matnr = t_maktkey-matnr
             AND spras = sy-langu.
    SORT t_makt BY matnr.
  ENDIF.

* Read batch data only if values are requested
  READ TABLE t_mchakey INDEX 1 TRANSPORTING NO FIELDS.
  IF sy-subrc = 0 AND novalues IS INITIAL.
    SELECT matnr werks charg bwtar
           INTO CORRESPONDING FIELDS OF TABLE t_mcha
           FROM mcha
           CONNECTION (dbcon)                               "1710852
           FOR ALL ENTRIES IN t_mchakey
           WHERE matnr = t_mchakey-matnr
             AND werks = t_mchakey-werks
             AND charg = t_mchakey-charg.
    SORT t_mcha BY matnr werks charg.
  ENDIF.

************************************************************************
* Data definitions for the valuation extraction
************************************************************************
  DATA: BEGIN OF t_mbewkey OCCURS 0,
          matnr LIKE mbew-matnr,
          bwkey LIKE mbew-bwkey,
          bwtar LIKE mbew-bwtar,
        END OF t_mbewkey,

*       workin table for the material stock valuation
        BEGIN OF t_mbew OCCURS 0,
          matnr     LIKE mbew-matnr,
          bwkey     LIKE mbew-bwkey,
          bwtar     LIKE mbew-bwtar,
*         consider the valuation of the special stocks E, Q "n531604
          sobkz     LIKE  ebew-sobkz,                       "n531604
          vbeln     LIKE  ebew-vbeln,                       "n531604
          posnr     LIKE  ebew-posnr,                       "n531604
          pspnr     LIKE  qbew-pspnr,                       "n531604

          lbkum(12) TYPE p DECIMALS 3,                      "407810
          salk3(12) TYPE p DECIMALS 2,                      "388735
          vprsv     LIKE mbew-vprsv,                        "353428
          verpr     LIKE mbew-verpr,                        "353428
          stprs     LIKE mbew-stprs,                        "353428
          peinh     LIKE mbew-peinh,                        "353428
        END OF t_mbew.

  DATA: t_ebewkey LIKE t_mbewkey OCCURS 0 WITH HEADER LINE.
  DATA: t_qbewkey LIKE t_mbewkey OCCURS 0 WITH HEADER LINE.
  DATA: t_ebew    LIKE t_mbew    OCCURS 0 WITH HEADER LINE.
  DATA: t_qbew    LIKE t_mbew    OCCURS 0 WITH HEADER LINE.

  DATA: BEGIN OF t_t134mkey OCCURS 0,
          bwkey LIKE t134m-bwkey,
          mtart LIKE t134m-mtart,
        END OF t_t134mkey,
        BEGIN OF t_t134m OCCURS 0,
          bwkey LIKE t134m-bwkey,
          mtart LIKE t134m-mtart,
          wertu LIKE t134m-wertu,
        END OF t_t134m.

************************************************************************
* Fill in additional data (first round) and extract the data
* for the access to the valuation tables.
************************************************************************
  SORT t_mat BY matnr werks.
  SORT g_t_organ             BY werks.
  CLEAR : g_s_t001l,         g_s_organ.

  LOOP AT bestand.
*   get the information per plant and storage location
*   with buffer
    IF  g_flag_t001l = 'X'.
      IF  bestand-werks = g_s_t001l-werks  AND
          bestand-lgort = g_s_t001l-lgort.
      ELSE.
*       read with plant and storage location
        READ TABLE g_t_t001l   INTO  g_s_t001l
          WITH TABLE KEY werks = bestand-werks
                         lgort = bestand-lgort.

        MOVE : bestand-werks TO g_s_t001l-werks,
               bestand-lgort TO g_s_t001l-lgort.

        IF sy-subrc <> 0.
          CLEAR              g_s_t001l-lgobe.
        ENDIF.
      ENDIF.
    ENDIF.

*   take the storage bin from the buffer
    MOVE : g_s_t001l-lgobe   TO  bestand-lgobe.


*   get the information per plant with buffer
    IF  bestand-werks  NE  g_s_organ-werks.
      READ TABLE g_t_organ   INTO  g_s_organ
          WITH KEY werks = bestand-werks
                             BINARY SEARCH.

      IF sy-subrc <> 0.
*       sorry nothing found
        CLEAR                g_s_organ.
        MOVE : bestand-werks TO  g_s_organ-werks.
      ENDIF.
    ENDIF.

*   take the following fields from the buffer
    MOVE : g_s_organ-name1   TO  bestand-name1,
           g_s_organ-waers   TO  bestand-waers,
           g_s_organ-bwkey   TO  bestand-bwkey.


*   get the information from the material master MARC
*   with buffer
    IF  bestand-matnr = l_s_mat-matnr  AND
        bestand-werks = l_s_mat-werks.
*     results are in the buffer
    ELSE.
      CLEAR                  l_s_mat.
      MOVE : bestand-matnr   TO  l_s_mat-matnr,
             bestand-werks   TO  l_s_mat-werks.

      READ TABLE t_mat       INTO  l_s_mat
         WITH KEY matnr = bestand-matnr
                  werks = bestand-werks
                             BINARY SEARCH.

      IF  sy-subrc <> 0.
*       sorry nothing found
        CLEAR                l_s_mat.
        MOVE : bestand-matnr TO  l_s_mat-matnr,
               bestand-werks TO  l_s_mat-werks.
      ENDIF.
    ENDIF.

*   take the results the buffer
    MOVE : l_s_mat-mtart     TO  bestand-mtart,
           l_s_mat-matkl     TO  bestand-matkl,
           l_s_mat-meins     TO  bestand-meins.


*   if this entry has no deletion flag, take the
*   deletion flag from a higher level like MARA, MARC,
*   or MARDA
    IF  bestand-lvorm IS INITIAL.
      IF      NOT  l_s_mat-lvorm_marc IS INITIAL.
        MOVE  l_s_mat-lvorm_marc
                             TO  bestand-lvorm.
      ELSEIF  NOT  l_s_mat-lvorm_mara IS INITIAL.
        MOVE  l_s_mat-lvorm_mara
                             TO  bestand-lvorm.

      ELSEIF  NOT g_t_mard_lv[] IS INITIAL  AND
              NOT bestand-lgort IS INITIAL  AND
              NOT bestand-sobkz IS INITIAL.
*       look for deletion flag in working table
*       g_t_mard_lv for a line with special stock
        IF  bestand-matnr = g_s_mard_lv-matnr  AND
            bestand-werks = g_s_mard_lv-werks  AND
            bestand-lgort = g_s_mard_lv-lgort.
        ELSE.
*         read table only after the key has changed
          READ TABLE g_t_mard_lv  INTO  g_s_mard_lv
          WITH TABLE KEY matnr = bestand-matnr
                         werks = bestand-werks
                         lgort = bestand-lgort.

          IF sy-subrc <> 0.
*           fill the buffer in case the entry does not exist
            MOVE : bestand-matnr  TO  g_s_mard_lv-matnr,
                   bestand-werks  TO  g_s_mard_lv-werks,
                   bestand-lgort  TO  g_s_mard_lv-lgort.
            CLEAR                 g_s_mard_lv-lvorm.
          ENDIF.

*         take the result from the buffer
          MOVE  g_s_mard_lv-lvorm   TO  bestand-lvorm.
        ENDIF.
      ENDIF.
    ENDIF.


*   read the material short description after the material
*   number has changed
    IF  bestand-matnr NE l_s_makt-matnr.
      READ TABLE t_makt      INTO  l_s_makt
                   WITH KEY matnr = bestand-matnr
                             BINARY SEARCH.

      IF sy-subrc <> 0.
*       sorry nothing found
        CLEAR                l_s_makt-maktx.
        MOVE  bestand-matnr  TO  l_s_makt-matnr.
      ENDIF.
    ENDIF.

*   take the results the buffer
    MOVE : l_s_makt-maktx    TO  bestand-maktx.

* added dynamic break-point ID MMIM_REP_MB52              "n912093
    BREAK-POINT ID mmim_rep_mb52.                           "n912093

    IF bestand-charg <> space AND novalues IS INITIAL.
      READ TABLE t_mcha WITH KEY matnr = bestand-matnr
                                 werks = bestand-werks
                                 charg = bestand-charg
                                 BINARY SEARCH.
      IF sy-subrc = 0.
        bestand-bwtar = t_mcha-bwtar.
      ENDIF.
    ENDIF.
    MODIFY bestand.
* Valuation keys
    IF novalues IS INITIAL.
      IF  bestand-sobkz = ' ' OR bestand-sobkz = 'O' OR
          bestand-sobkz = 'W' OR bestand-sobkz = 'V' OR
          bestand-kzbws = 'A'.
        t_mbewkey-matnr = bestand-matnr.
        t_mbewkey-bwkey = bestand-bwkey.
        t_mbewkey-bwtar = bestand-bwtar.
        COLLECT t_mbewkey.
      ELSEIF bestand-sobkz = 'E' AND bestand-kzbws = 'M'.
        t_ebewkey-matnr = bestand-matnr.
        t_ebewkey-bwkey = bestand-bwkey.
        t_ebewkey-bwtar = bestand-bwtar.
        COLLECT t_ebewkey.
      ELSEIF bestand-sobkz = 'Q' AND bestand-kzbws = 'M'.
        t_qbewkey-matnr = bestand-matnr.
        t_qbewkey-bwkey = bestand-bwkey.
        t_qbewkey-bwtar = bestand-bwtar.
        COLLECT t_qbewkey.
      ENDIF.

      "{ Begin ENHO DIPCS_RM07MLBS001 IS-AD-SSP AD_SUB }
* GA1727687 - A&D special stocks at subcontractor
      IF cl_adsub_switch_check=>get_ad01_switch_state( ) IS NOT INITIAL.
        IF bestand-kzbws = 'M'.
          CASE bestand-sobkz.
            WHEN cl_adsub_constants=>f.
              t_ebewkey-matnr = bestand-matnr.
              t_ebewkey-bwkey = bestand-bwkey.
              t_ebewkey-bwtar = bestand-bwtar.
              COLLECT t_ebewkey.
            WHEN cl_adsub_constants=>r.
              t_qbewkey-matnr = bestand-matnr.
              t_qbewkey-bwkey = bestand-bwkey.
              t_qbewkey-bwtar = bestand-bwtar.
              COLLECT t_qbewkey.
          ENDCASE.
        ENDIF.
      ENDIF.
      "{ End ENHO DIPCS_RM07MLBS001 IS-AD-SSP AD_SUB }

ENHANCEMENT-POINT ehp604_rm07mlbs_44 SPOTS es_rm07mlbs .
      t_t134mkey-bwkey = bestand-bwkey.
      t_t134mkey-mtart = bestand-mtart.
      COLLECT t_t134mkey.
    ENDIF.                             " novalues is initial
  ENDLOOP.

* release the space of global working tables after use
  FREE : g_t_mard_lv, g_t_t001l, g_t_organ.

************************************************************************
* Read the valuation tables
************************************************************************
  IF novalues IS INITIAL.
    READ TABLE t_mbewkey INDEX 1 TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      SELECT matnr bwkey bwtar lbkum salk3   ##too_many_itab_fields
             vprsv verpr stprs peinh                        "353428
             CONNECTION (dbcon)                             "1710852
             INTO CORRESPONDING FIELDS OF TABLE t_mbew
             FROM mbew
             FOR ALL ENTRIES IN t_mbewkey
             WHERE matnr = t_mbewkey-matnr
               AND bwkey = t_mbewkey-bwkey
               AND bwtar = t_mbewkey-bwtar.
      SORT t_mbew BY matnr bwkey bwtar.
    ENDIF.

    READ TABLE t_ebewkey INDEX 1 TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
*     "Unfortunately", EBEW and QBEW do not have sum segments over
*     the valuation types. Therefore, without batch data, another
*     SELECT-statement is needed.
      IF xmchb = 'X'.
        SELECT matnr bwkey bwtar sobkz vbeln posnr lbkum salk3
               vprsv verpr stprs peinh
               CONNECTION (dbcon)                           "1710852
               INTO (t_ebew-matnr, t_ebew-bwkey, t_ebew-bwtar,
                     t_ebew-sobkz,  t_ebew-vbeln, t_ebew-posnr,
                     t_ebew-lbkum, t_ebew-salk3,
                     t_ebew-vprsv, t_ebew-verpr,
                     t_ebew-stprs, t_ebew-peinh)
               FROM ebew
               FOR ALL ENTRIES IN t_ebewkey
               WHERE matnr = t_ebewkey-matnr
                 AND bwkey = t_ebewkey-bwkey
                 AND bwtar = t_ebewkey-bwtar.
          COLLECT t_ebew.
        ENDSELECT.
      ELSE.
        SELECT matnr bwkey bwtar sobkz vbeln posnr lbkum salk3
               vprsv verpr stprs peinh
               CONNECTION (dbcon)                           "1710852
               INTO (t_ebew-matnr, t_ebew-bwkey,  t_ebew-bwtar,
                     t_ebew-sobkz,  t_ebew-vbeln, t_ebew-posnr,
                     t_ebew-lbkum, t_ebew-salk3,
                     t_ebew-vprsv, t_ebew-verpr,
                     t_ebew-stprs, t_ebew-peinh)
               FROM ebew
               FOR ALL ENTRIES IN t_ebewkey
               WHERE matnr = t_ebewkey-matnr
                 AND bwkey = t_ebewkey-bwkey.
          COLLECT t_ebew.
        ENDSELECT.
      ENDIF.

      SORT t_ebew BY matnr bwkey bwtar sobkz vbeln posnr.
    ENDIF.

    READ TABLE t_qbewkey INDEX 1 TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      IF xmchb = 'X'.
        SELECT matnr bwkey bwtar sobkz pspnr lbkum salk3
               vprsv verpr stprs peinh
               CONNECTION (dbcon)                           "1710852
               INTO (t_qbew-matnr, t_qbew-bwkey, t_qbew-bwtar,
                     t_qbew-sobkz, t_qbew-pspnr,
                     t_qbew-lbkum, t_qbew-salk3,
                     t_qbew-vprsv, t_qbew-verpr,
                     t_qbew-stprs, t_qbew-peinh)
               FROM qbew
               FOR ALL ENTRIES IN t_qbewkey
               WHERE matnr = t_qbewkey-matnr
                 AND bwkey = t_qbewkey-bwkey
                AND bwtar = t_qbewkey-bwtar.
          COLLECT t_qbew.
        ENDSELECT.
      ELSE.
        SELECT matnr bwkey bwtar sobkz pspnr lbkum salk3
               vprsv verpr stprs peinh
               CONNECTION (dbcon)                           "1710852
               INTO (t_qbew-matnr, t_qbew-bwkey, t_qbew-bwtar,
                     t_qbew-sobkz, t_qbew-pspnr,
                     t_qbew-lbkum, t_qbew-salk3,
                     t_qbew-vprsv, t_qbew-verpr,
                     t_qbew-stprs, t_qbew-peinh)
               FROM qbew
               FOR ALL ENTRIES IN t_qbewkey
               WHERE matnr = t_qbewkey-matnr
                 AND bwkey = t_qbewkey-bwkey.
          COLLECT t_qbew.
        ENDSELECT.
      ENDIF.

      SORT t_qbew BY matnr bwkey bwtar sobkz pspnr.
    ENDIF.

    READ TABLE t_t134mkey INDEX 1 TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      SELECT bwkey mtart wertu
             INTO CORRESPONDING FIELDS OF TABLE t_t134m
             FROM t134m
             FOR ALL ENTRIES IN t_t134mkey
             WHERE bwkey = t_t134mkey-bwkey
               AND mtart = t_t134mkey-mtart.
      SORT t_t134m BY bwkey mtart.
    ENDIF.
************************************************************************
* Fill the valuation data
************************************************************************
    DATA: factor TYPE f.
    LOOP AT bestand.
      CHECK bestand-waers <> space.  "Do nothing for failed Auth-Checks
      READ TABLE t_t134m WITH KEY bwkey = bestand-bwkey
                                  mtart = bestand-mtart
                                  BINARY SEARCH.
      CHECK sy-subrc = 0 AND t_t134m-wertu = 'X'.
*     Set SY-SUBRC = 4. A successful table read resets it an starts
*     the value filling.
      sy-subrc = 4.
      IF  bestand-sobkz = ' ' OR bestand-sobkz = 'O' OR
          bestand-sobkz = 'W' OR bestand-sobkz = 'V' OR
          bestand-kzbws = 'A'.
        READ TABLE t_mbew WITH KEY matnr = bestand-matnr
                                   bwkey = bestand-bwkey
                                   bwtar = bestand-bwtar
                                   BINARY SEARCH.

      ELSEIF bestand-sobkz = 'E' AND bestand-kzbws = 'M'.
        READ TABLE t_ebew WITH KEY matnr = bestand-matnr
                                   bwkey = bestand-bwkey
                                   bwtar = bestand-bwtar
                                   sobkz = bestand-sobkz
                                   vbeln = bestand-vbeln    "n531604
                                   posnr = bestand-posnr    "n531604
                                   BINARY SEARCH.
        MOVE-CORRESPONDING t_ebew TO t_mbew.

      ELSEIF bestand-sobkz = 'Q' AND bestand-kzbws = 'M'.
        READ TABLE t_qbew WITH KEY matnr = bestand-matnr
                                   bwkey = bestand-bwkey
                                   bwtar = bestand-bwtar
                                   sobkz = bestand-sobkz
                                   pspnr = bestand-pspnr
                                   BINARY SEARCH.
        MOVE-CORRESPONDING t_qbew TO t_mbew.
      ENDIF.

      "{ Begin ENHO DIPCS_RM07MLBS001 IS-AD-SSP AD_SUB }
* GA1727687 - A&D special stocks at subcontractor
* GA1956994 - setting sy-subrc properly
      lv_subrc = sy-subrc.
      IF cl_adsub_switch_check=>get_ad01_switch_state( ) IS NOT INITIAL.
        sy-subrc = lv_subrc.
        IF bestand-sobkz EQ cl_adsub_constants=>f AND
           bestand-kzbws = 'M'.
          READ TABLE t_ebew WITH KEY matnr = bestand-matnr
                                     bwkey = bestand-bwkey
                                     bwtar = bestand-bwtar
                                     sobkz = cl_adsub_constants=>e
                                     vbeln = bestand-vbeln  "n531604
                                     posnr = bestand-posnr  "n531604
                                     BINARY SEARCH.
          MOVE-CORRESPONDING t_ebew TO t_mbew.
        ELSEIF bestand-sobkz EQ cl_adsub_constants=>r AND
               bestand-kzbws = 'M'.
          READ TABLE t_qbew WITH KEY matnr = bestand-matnr
                                     bwkey = bestand-bwkey
                                     bwtar = bestand-bwtar
                                     sobkz = cl_adsub_constants=>q
                                     pspnr = bestand-pspnr
                                     BINARY SEARCH.
          MOVE-CORRESPONDING t_qbew TO t_mbew.
        ENDIF.
      ELSE.
        sy-subrc = lv_subrc.
      ENDIF.
      "{ End ENHO DIPCS_RM07MLBS001 IS-AD-SSP AD_SUB }

ENHANCEMENT-POINT ehp604_rm07mlbs_45 SPOTS es_rm07mlbs .

      IF sy-subrc = 0.
        IF t_mbew-lbkum = 0.
*         Cannot happen, but in R/3 this does not hold in all cases...
          IF t_mbew-peinh = 0.                              "353428
            t_mbew-peinh = 1.                               "353428
          ENDIF.                                            "353428
*         Calculation of value in case of LBKUM = 0 only possible
*         for MBEW. EBEW and QBEW are collected over all subitems
*         (VBELN...), so the data are not available.
          IF bestand-sobkz = 'E' OR bestand-sobkz = 'Q'.    "388735
            factor = 0.                                     "388735
            CLEAR bestand-waers.                            "388735
          ELSE.                                             "388735
            CASE t_mbew-vprsv.
              WHEN 'V'. factor = t_mbew-verpr / t_mbew-peinh.
              WHEN 'S'. factor = t_mbew-stprs / t_mbew-peinh.
            ENDCASE.
          ENDIF.                                            "388735
        ELSE.
          factor = t_mbew-salk3 / t_mbew-lbkum.
        ENDIF.

        bestand-wlabs = bestand-labst * factor.
        bestand-winsm = bestand-insme * factor.
        bestand-wspem = bestand-speme * factor.
        bestand-weinm = bestand-einme * factor.
        bestand-wumlm = bestand-umlme * factor.
        bestand-wbwesb = bestand-bwesb * factor.            "AC0K020254
        bestand-wglgm = bestand-glgmg * factor.             "n912093
        bestand-wtram = bestand-trame * factor.             "n912093
        bestand-wumlc = bestand-umlmc * factor.             "n912093
* CWM uses fields with valuation unit relation
        IF  bestand-/cwm/xcwmat IS NOT INITIAL
        AND bestand-/cwm/valum <> bestand-meins.
          LOG-POINT ID /cwm/enh SUBKEY to_upper( sy-tcode && '\/CWM/APPL_MM_RM07MLBS_EHP4\EHP604_RM07MLBS_22\' && sy-cprog ) FIELDS /cwm/cl_enh_layer=>get_field( ).
          bestand-wlabs = bestand-/cwm/labst * factor.
          bestand-winsm = bestand-/cwm/insme * factor.
          bestand-wspem = bestand-/cwm/speme * factor.
          bestand-weinm = bestand-/cwm/einme * factor.
          bestand-wumlm = bestand-/cwm/umlme * factor.
          bestand-wbwesb = bestand-/cwm/bwesb * factor.     "AC0K020254
          bestand-wglgm = bestand-glgmg * factor.           "n912093
          bestand-wtram = bestand-/cwm/trame * factor.      "n912093
          bestand-wumlc = bestand-/cwm/umlmc * factor.
        ENDIF.
ENHANCEMENT-POINT ehp604_rm07mlbs_22 SPOTS es_rm07mlbs .
        MODIFY bestand.
      ENDIF.
    ENDLOOP.
  ENDIF.                               "novalues is initial
  LOG-POINT ID /cwm/enh SUBKEY to_upper( sy-tcode && '\/CWM/APPL_MM_RM07MLBS_EHP4\EHP604_RM07MLBS_23\' && sy-cprog ) FIELDS /cwm/cl_enh_layer=>get_field( ).
* export for /CWM/STOCK_CHECK
  IF /cwm/exp IS NOT INITIAL.
    EXPORT bestand FROM bestand[] TO MEMORY ID '/CWM/RM07MLBS'.
    LEAVE PROGRAM.
  ENDIF.
ENHANCEMENT-POINT ehp604_rm07mlbs_23 SPOTS es_rm07mlbs .
ENDFORM.                    "data_selection

************************************************************************
* Build fieldcatalog for list viewer
************************************************************************
FORM fieldcatalog.

* Header fields
  CLEAR fieldcat.
ENHANCEMENT-SECTION     rm07mlbs_08 SPOTS es_rm07mlbs.
  fieldcat-fieldname     = 'MATNR'.
  fieldcat-tabname       = 'HEADER'.
  fieldcat-ref_tabname   = 'MARA'.
END-ENHANCEMENT-SECTION.
  APPEND fieldcat.
  CLEAR fieldcat.
  fieldcat-fieldname     = 'MAKTX'.
  fieldcat-tabname       = 'HEADER'.
  fieldcat-ref_tabname   = 'MAKT'.
  APPEND fieldcat.
  CLEAR fieldcat.
  fieldcat-fieldname     = 'WERKS'.
  fieldcat-tabname       = 'HEADER'.
  fieldcat-ref_tabname   = 'T001W'.
  APPEND fieldcat.
  CLEAR fieldcat.
  fieldcat-fieldname     = 'NAME1'.
  fieldcat-tabname       = 'HEADER'.
  fieldcat-ref_tabname   = 'T001W'.
  APPEND fieldcat.
  CLEAR fieldcat.

  "{ Begin ENHO MGV_LAMA_RM07MLBS LO-MD-MM MGV_LAMA }
  IF cl_immpn_cust=>check_mpn_active( ) = abap_true.
* Modification for PIC-Supersession/MPN

    IF show_ext_manufacturer EQ 'X'.

      CLEAR fieldcat.
      fieldcat-fieldname     = 'EMNFR'.
      fieldcat-tabname       = 'HEADER'.
      fieldcat-ref_tabname   = 'LFA1'.
*      fieldcat-key           = 'X'.
*      fieldcat-col_pos       = '5'.
      APPEND fieldcat.
    ENDIF.

  ENDIF.
  "{ End ENHO MGV_LAMA_RM07MLBS LO-MD-MM MGV_LAMA }

ENHANCEMENT-POINT rm07mlbs_1 SPOTS es_rm07mlbs .
  fieldcat-fieldname     = 'MTART'.
  fieldcat-tabname       = 'HEADER'.
  fieldcat-ref_tabname   = 'MARA'.
  fieldcat-no_out        = 'X'.
  APPEND fieldcat.
  CLEAR fieldcat.
  fieldcat-fieldname     = 'MATKL'.
  fieldcat-tabname       = 'HEADER'.
  fieldcat-ref_tabname   = 'MARA'.
  fieldcat-no_out        = 'X'.
  APPEND fieldcat.

* List body
  CLEAR fieldcat.
  fieldcat-fieldname     = 'LGORT'.
  fieldcat-tabname       = 'BESTAND'.
  fieldcat-ref_tabname   = 'MARD'.
  fieldcat-outputlen     = 5.
  APPEND fieldcat.

  CLEAR fieldcat.
  fieldcat-fieldname     = 'LGOBE'.
  fieldcat-tabname       = 'BESTAND'.
  fieldcat-ref_tabname   = 'T001L'.
  fieldcat-no_out        = 'X'.
  APPEND fieldcat.

  IF NOT pa_sond IS INITIAL.
    CLEAR fieldcat.
    fieldcat-fieldname     = 'SOBKZ'.
    fieldcat-tabname       = 'BESTAND'.
    fieldcat-ref_tabname   = 'MKOL'.
    APPEND fieldcat.

    CLEAR fieldcat.
    fieldcat-fieldname     = 'KZBWS'.
    fieldcat-tabname       = 'BESTAND'.
    fieldcat-ref_tabname   = 'MSSA'.
    fieldcat-outputlen     = 1.
    APPEND fieldcat.

    CLEAR fieldcat.
    fieldcat-fieldname     = 'SSNUM'.
    fieldcat-tabname       = 'BESTAND'.
    fieldcat-ref_tabname   = 'BICKEY'.
    APPEND fieldcat.
  ENDIF.

  IF  xmchb = 'X'.
    CLEAR fieldcat.
    fieldcat-fieldname     = 'CHARG'.
    fieldcat-tabname       = 'BESTAND'.
    fieldcat-ref_tabname   = 'MCHB'.
    APPEND fieldcat.
  ENDIF.

  CLEAR fieldcat.
  fieldcat-fieldname     = 'LVORM'.
  fieldcat-tabname       = 'BESTAND'.
  fieldcat-ref_tabname   = 'MARD'.
  fieldcat-outputlen     = 3.
  APPEND fieldcat.

* Quantities
  CLEAR fieldcat.
  fieldcat-fieldname     = 'LABST'.
  fieldcat-tabname       = 'BESTAND'.
  fieldcat-ref_tabname   = 'MARD'.
  fieldcat-qfieldname    = 'MEINS'.
  APPEND fieldcat.

  CLEAR fieldcat.
  fieldcat-fieldname     = 'MEINS'.
  fieldcat-tabname       = 'BESTAND'.
  fieldcat-ref_tabname   = 'MARA'.
  fieldcat-outputlen     = '5'.
  APPEND fieldcat.

  CLEAR fieldcat.                                           "n912093
  fieldcat-fieldname     = 'UMLME'.                         "n912093
  fieldcat-tabname       = 'BESTAND'.                       "n912093
  fieldcat-ref_tabname   = 'AM07M'.                         "n912093
  fieldcat-ref_fieldname = 'MB52_TRAUML'.                   "n912093
  fieldcat-qfieldname    = 'MEINS'.                         "n912093
  APPEND fieldcat.                                          "n912093

  CLEAR fieldcat.
  fieldcat-fieldname     = 'INSME'.
  fieldcat-tabname       = 'BESTAND'.
  fieldcat-ref_tabname   = 'MARD'.
  fieldcat-qfieldname    = 'MEINS'.
  APPEND fieldcat.

  CLEAR fieldcat.
  fieldcat-fieldname     = 'EINME'.
  fieldcat-tabname       = 'BESTAND'.
  fieldcat-ref_tabname   = 'MARD'.
  fieldcat-qfieldname    = 'MEINS'.
  APPEND fieldcat.

  CLEAR fieldcat.
  fieldcat-fieldname     = 'SPEME'.
  fieldcat-tabname       = 'BESTAND'.
  fieldcat-ref_tabname   = 'MARD'.
  fieldcat-qfieldname    = 'MEINS'.
  APPEND fieldcat.

  CLEAR fieldcat.
  fieldcat-fieldname     = 'RETME'.
  fieldcat-tabname       = 'BESTAND'.
  fieldcat-ref_tabname   = 'MARD'.
  fieldcat-qfieldname    = 'MEINS'.
  APPEND fieldcat.

* process MARC-BWESB as hidden field                     "AC0K020254
  CLEAR fieldcat.                                           "AC0K020254
  fieldcat-fieldname     = 'BWESB'.                         "AC0K020254
  fieldcat-tabname       = 'BESTAND'.                       "AC0K020254
  fieldcat-ref_tabname   = 'MARC'.                          "AC0K020254
  fieldcat-qfieldname    = 'MEINS'.                         "AC0K020254
  fieldcat-no_out        = 'X'.                             "AC0K020254
  APPEND fieldcat.                                          "AC0K020254

* tied empties stock                                     "n912093
  fieldcat-fieldname     = 'GLGMG'.                         "n912093
  fieldcat-tabname       = 'BESTAND'.                       "n912093
  fieldcat-ref_tabname   = 'MARC'.                          "n912093
  fieldcat-qfieldname    = 'MEINS'.                         "n912093
  fieldcat-no_out      = 'X'.                               "n912093
  APPEND fieldcat.                                          "n912093
  CLEAR fieldcat.                                           "n912093

* stock in transit                                       "n912093
  fieldcat-fieldname     = 'TRAME'.                         "n912093
  fieldcat-tabname       = 'BESTAND'.                       "n912093
  fieldcat-ref_tabname   = 'MARC'.                          "n912093
  fieldcat-qfieldname    = 'MEINS'.                         "n912093
  fieldcat-no_out      = 'X'.                               "n912093
  APPEND fieldcat.                                          "n912093
  CLEAR fieldcat.                                           "n912093

* stock in uml                                           "n912093
  fieldcat-fieldname     = 'UMLMC'.                         "n912093
  fieldcat-tabname       = 'BESTAND'.                       "n912093
  fieldcat-ref_tabname   = 'MARC'.                          "n912093
  fieldcat-qfieldname    = 'MEINS'.                         "n912093
  fieldcat-no_out      = 'X'.                               "n912093
  APPEND fieldcat.                                          "n912093
  CLEAR fieldcat.                                           "n912093


* set the key fields of the special stock as hidden fields
  IF  g_flag_sobkz-lifnr = 'X'.
    CLEAR fieldcat.
    fieldcat-fieldname     = 'LIFNR'.
    fieldcat-tabname       = 'BESTAND'.
    fieldcat-ref_tabname   = 'MKOL'.
    fieldcat-no_out        = 'X'.
    APPEND fieldcat.
  ENDIF.

  IF  g_flag_sobkz-kunnr = 'X'.
    CLEAR fieldcat.
    fieldcat-fieldname     = 'KUNNR'.
    fieldcat-tabname       = 'BESTAND'.
    fieldcat-ref_tabname   = 'MSKU'.
    fieldcat-no_out        = 'X'.
    APPEND fieldcat.
  ENDIF.

  IF  g_flag_sobkz-vbeln = 'X'.
    CLEAR fieldcat.
    fieldcat-fieldname     = 'VBELN'.
    fieldcat-tabname       = 'BESTAND'.
    fieldcat-ref_tabname   = 'MSKA'.
    fieldcat-no_out        = 'X'.
    APPEND fieldcat.

    CLEAR fieldcat.
    fieldcat-fieldname     = 'POSNR'.
    fieldcat-tabname       = 'BESTAND'.
    fieldcat-ref_tabname   = 'MSKA'.
    fieldcat-no_out        = 'X'.
    APPEND fieldcat.
  ENDIF.

  IF  g_flag_sobkz-pspnr = 'X'.
    CLEAR fieldcat.
    fieldcat-fieldname     = 'PSPNR'.
    fieldcat-tabname       = 'BESTAND'.
    fieldcat-ref_tabname   = 'MSPR'.
    fieldcat-no_out        = 'X'.
    APPEND fieldcat.
  ENDIF.

* here starts the second row
* Values
  IF novalues IS INITIAL.
    CLEAR fieldcat.
    fieldcat-fieldname     = 'DUMMY'.
    fieldcat-tabname       = 'BESTAND'.
    fieldcat-row_pos       = '2'.
    fieldcat-outputlen     = 9.

*   calculate the length of the dummy field
    IF xmchb = 'X'.
      ADD 11 TO fieldcat-outputlen.
    ENDIF.

    IF NOT pa_sond IS INITIAL.
      ADD 29 TO fieldcat-outputlen.
    ENDIF.

    APPEND fieldcat.

    CLEAR fieldcat.
    fieldcat-fieldname     = 'WLABS'.
    fieldcat-tabname       = 'BESTAND'.
    fieldcat-ref_fieldname = 'SALK3'.
    fieldcat-ref_tabname   = 'MBEW'.
    fieldcat-cfieldname    = 'WAERS'.
    fieldcat-row_pos       = '2'.
    fieldcat-seltext_s     = TEXT-020.
    fieldcat-seltext_m     = TEXT-020.
    fieldcat-seltext_m     = TEXT-020.
    fieldcat-do_sum        = 'X'.
    APPEND fieldcat.
    CLEAR fieldcat.
    fieldcat-fieldname     = 'WAERS'.
    fieldcat-tabname       = 'BESTAND'.
    fieldcat-ref_fieldname = 'WAERS'.
    fieldcat-ref_tabname   = 'T001'.
    fieldcat-row_pos       = '2'.
    fieldcat-outputlen     = 5.
    APPEND fieldcat.
    CLEAR fieldcat.
    fieldcat-fieldname     = 'WUMLM'.
    fieldcat-tabname       = 'BESTAND'.
    fieldcat-ref_fieldname = 'SALK3'.
    fieldcat-ref_tabname   = 'MBEW'.
    fieldcat-cfieldname    = 'WAERS'.
    fieldcat-row_pos       = '2'.
    fieldcat-seltext_s     = TEXT-020.
    fieldcat-seltext_m     = TEXT-020.
    fieldcat-seltext_m     = TEXT-020.
    fieldcat-outputlen     = '18'.
    fieldcat-do_sum        = 'X'.
    APPEND fieldcat.
    CLEAR fieldcat.
    fieldcat-fieldname     = 'WINSM'.
    fieldcat-tabname       = 'BESTAND'.
    fieldcat-ref_fieldname = 'SALK3'.
    fieldcat-ref_tabname   = 'MBEW'.
    fieldcat-cfieldname    = 'WAERS'.
    fieldcat-row_pos       = '2'.
    fieldcat-seltext_s     = TEXT-020.
    fieldcat-seltext_m     = TEXT-020.
    fieldcat-seltext_m     = TEXT-020.
    fieldcat-outputlen     = '18'.
    fieldcat-do_sum        = 'X'.
    APPEND fieldcat.
    CLEAR fieldcat.
    fieldcat-fieldname     = 'WEINM'.
    fieldcat-tabname       = 'BESTAND'.
    fieldcat-ref_fieldname = 'SALK3'.
    fieldcat-ref_tabname   = 'MBEW'.
    fieldcat-cfieldname    = 'WAERS'.
    fieldcat-row_pos       = '2'.
    fieldcat-seltext_s     = TEXT-020.
    fieldcat-seltext_m     = TEXT-020.
    fieldcat-seltext_m     = TEXT-020.
    fieldcat-outputlen     = '18'.
    fieldcat-do_sum        = 'X'.
    APPEND fieldcat.
    CLEAR fieldcat.
    fieldcat-fieldname     = 'WSPEM'.
    fieldcat-tabname       = 'BESTAND'.
    fieldcat-ref_fieldname = 'SALK3'.
    fieldcat-ref_tabname   = 'MBEW'.
    fieldcat-cfieldname    = 'WAERS'.
    fieldcat-row_pos       = '2'.
    fieldcat-seltext_s     = TEXT-020.
    fieldcat-seltext_m     = TEXT-020.
    fieldcat-seltext_m     = TEXT-020.
    fieldcat-outputlen     = '18'.
    fieldcat-do_sum        = 'X'.
    APPEND fieldcat.

    CLEAR fieldcat.
    fieldcat-fieldname     = 'WRETM'.
    fieldcat-tabname       = 'BESTAND'.
    fieldcat-ref_fieldname = 'SALK3'.
    fieldcat-ref_tabname   = 'MBEW'.
    fieldcat-cfieldname    = 'WAERS'.
    fieldcat-row_pos       = '2'.
    fieldcat-seltext_s     = TEXT-020.
    fieldcat-seltext_m     = TEXT-020.
    fieldcat-seltext_m     = TEXT-020.
    fieldcat-outputlen     = '18'.
    fieldcat-do_sum        = 'X'.
    APPEND fieldcat.

*  process estimated value MARC-BWESB as hidden field    "AC0K020254
    CLEAR fieldcat.                                         "AC0K020254
    fieldcat-fieldname     = 'WBWESB'.                      "AC0K020254
    fieldcat-tabname       = 'BESTAND'.                     "AC0K020254
    fieldcat-ref_tabname   = 'MBEW'.                        "AC0K020254
    fieldcat-ref_fieldname = 'SALK3'.                       "AC0K020254
    fieldcat-cfieldname    = 'WAERS'.                       "AC0K020254
    fieldcat-row_pos       = '2'.                           "AC0K020254
    fieldcat-seltext_s     = TEXT-020.                      "AC0K020254
    fieldcat-seltext_m     = TEXT-020.                      "AC0K020254
    fieldcat-seltext_m     = TEXT-020.                      "AC0K020254
    fieldcat-outputlen     = '18'.                          "AC0K020254
    fieldcat-do_sum        = 'X'.                           "AC0K020254
    fieldcat-no_out        = 'X'.                           "AC0K020254
    APPEND fieldcat.                                        "AC0K020254

*   value for tied empties                               "n912093
    fieldcat-no_out      = 'X'.                             "n912093
    fieldcat-fieldname     = 'WGLGM'.                       "n912093
    fieldcat-tabname       = 'BESTAND'.                     "n912093
    fieldcat-ref_fieldname = 'SALK3'.                       "n912093
    fieldcat-ref_tabname   = 'MBEW'.                        "n912093
    fieldcat-cfieldname    = 'WAERS'.                       "n912093
    fieldcat-row_pos       = '2'.                           "n912093
    fieldcat-seltext_s     = TEXT-020.                      "n912093
    fieldcat-seltext_m     = TEXT-020.                      "n912093
    fieldcat-seltext_m     = TEXT-020.                      "n912093
    fieldcat-outputlen     = '18'.                          "n912093
    fieldcat-do_sum        = 'X'.                           "n912093
    APPEND fieldcat.                                        "n912093

*   value for transit                                    "n912093
    fieldcat-no_out      = 'X'.                             "n912093
    fieldcat-fieldname     = 'WTRAM'.                       "n912093
    fieldcat-tabname       = 'BESTAND'.                     "n912093
    fieldcat-ref_fieldname = 'SALK3'.                       "n912093
    fieldcat-ref_tabname   = 'MBEW'.                        "n912093
    fieldcat-cfieldname    = 'WAERS'.                       "n912093
    fieldcat-row_pos       = '2'.                           "n912093
    fieldcat-seltext_s     = TEXT-020.                      "n912093
    fieldcat-seltext_m     = TEXT-020.                      "n912093
    fieldcat-seltext_m     = TEXT-020.                      "n912093
    fieldcat-outputlen     = '18'.                          "n912093
    fieldcat-do_sum        = 'X'.                           "n912093
    APPEND fieldcat.

*   value for uml at plant                               "n912093
    fieldcat-no_out      = 'X'.                             "n912093
    fieldcat-fieldname     = 'WUMLC'.                       "n912093
    fieldcat-tabname       = 'BESTAND'.                     "n912093
    fieldcat-ref_fieldname = 'SALK3'.                       "n912093
    fieldcat-ref_tabname   = 'MBEW'.                        "n912093
    fieldcat-cfieldname    = 'WAERS'.                       "n912093
    fieldcat-row_pos       = '2'.                           "n912093
    fieldcat-seltext_s     = TEXT-020.                      "n912093
    fieldcat-seltext_m     = TEXT-020.                      "n912093
    fieldcat-seltext_m     = TEXT-020.                      "n912093
    fieldcat-outputlen     = '18'.                          "n912093
    fieldcat-do_sum        = 'X'.                           "n912093
    APPEND fieldcat.

  ENDIF.                               "novalues is initial
* CWM fields
  LOOP AT bestand WHERE /cwm/xcwmat IS NOT INITIAL.
    LOG-POINT ID /cwm/enh SUBKEY to_upper( sy-tcode && '\/CWM/APPL_MM_RM07MLBS_EHP4\EHP604_RM07MLBS_24\' && sy-cprog ) FIELDS /cwm/cl_enh_layer=>get_field( ).
    EXIT.
  ENDLOOP.
  IF sy-subrc = 0.
    CLEAR fieldcat.
    fieldcat-fieldname     = '/CWM/XCWMAT'.
    fieldcat-tabname       = 'HEADER'.
    fieldcat-ref_tabname   = 'MARA'.
    fieldcat-no_out        = ' '.
    APPEND fieldcat.

    LOOP AT fieldcat WHERE row_pos = '2'.
      fieldcat-row_pos = '3'.
      MODIFY fieldcat TRANSPORTING row_pos.
    ENDLOOP.

    CLEAR fieldcat.
    fieldcat-fieldname     = '/CWM/DUMMY'.
    fieldcat-tabname       = 'BESTAND'.
    fieldcat-row_pos       = '2'.
    fieldcat-outputlen     = 9.

*   calculate the length of the dummy field
    IF xmchb = 'X'.
      ADD 11 TO fieldcat-outputlen.
    ENDIF.

    IF NOT pa_sond IS INITIAL.
      ADD 29 TO fieldcat-outputlen.
    ENDIF.

    IF NOT pa_sond IS INITIAL   AND
       g_flag_sobkz-lifnr EQ 'X'.
      ADD 11 TO fieldcat-outputlen.
    ENDIF.

    APPEND fieldcat.

    CLEAR fieldcat.
    fieldcat-fieldname     = '/CWM/LABST'.
    fieldcat-tabname       = 'BESTAND'.
    fieldcat-ref_tabname   = 'MARD'.
    fieldcat-qfieldname    = '/CWM/MEINS'.
    fieldcat-row_pos       = '2'.
    APPEND fieldcat.

    CLEAR fieldcat.
    fieldcat-fieldname     = '/CWM/MEINS'.
    fieldcat-tabname       = 'BESTAND'.
    fieldcat-ref_tabname   = '/CWM/MARD_PQ'.
    fieldcat-outputlen     = '5'.
    fieldcat-row_pos       = '2'.
    APPEND fieldcat.

    CLEAR fieldcat.                                         "n912093
    fieldcat-fieldname     = '/CWM/UMLME'.                  "n912093
    fieldcat-tabname       = 'BESTAND'.                     "n912093
    fieldcat-ref_tabname   = 'AM07M'.                       "n912093
    fieldcat-ref_fieldname = 'MB52_TRAUML'.                 "n912093
    fieldcat-qfieldname    = '/CWM/MEINS'.                  "n912093
    fieldcat-row_pos       = '2'.
    APPEND fieldcat.


    CLEAR fieldcat.
    fieldcat-fieldname     = '/CWM/INSME'.
    fieldcat-tabname       = 'BESTAND'.
    fieldcat-ref_tabname   = 'MARD'.
    fieldcat-qfieldname    = '/CWM/MEINS'.
    fieldcat-row_pos       = '2'.
    APPEND fieldcat.



    CLEAR fieldcat.
    fieldcat-fieldname     = '/CWM/EINME'.
    fieldcat-tabname       = 'BESTAND'.
    fieldcat-ref_tabname   = 'MARD'.
    fieldcat-qfieldname    = '/CWM/MEINS'.
    fieldcat-row_pos       = '2'.

    APPEND fieldcat.

    CLEAR fieldcat.
    fieldcat-fieldname     = '/CWM/SPEME'.
    fieldcat-tabname       = 'BESTAND'.
    fieldcat-ref_tabname   = 'MARD'.
    fieldcat-qfieldname    = '/CWM/MEINS'.
    fieldcat-row_pos       = '2'.
    APPEND fieldcat.

    CLEAR fieldcat.
    fieldcat-fieldname     = '/CWM/RETME'.
    fieldcat-tabname       = 'BESTAND'.
    fieldcat-ref_tabname   = 'MARD'.
    fieldcat-qfieldname    = '/CWM/MEINS'.
    fieldcat-row_pos       = '2'.
    APPEND fieldcat.

* stock in transit                                       "n912093
    CLEAR fieldcat.
    fieldcat-fieldname     = '/CWM/TRAME'.                  "n912093
    fieldcat-tabname       = 'BESTAND'.                     "n912093
    fieldcat-ref_tabname   = 'MARC'.                        "n912093
    fieldcat-qfieldname    = '/CWM/MEINS'.                  "n912093
    fieldcat-no_out      = 'X'.                             "n912093
    fieldcat-row_pos       = '2'.
    APPEND fieldcat.                                        "n912093
                                                            "n912093

* stock in uml                                           "n912093
    CLEAR fieldcat.                                         "n912093
    fieldcat-fieldname     = '/CWM/UMLMC'.                  "n912093
    fieldcat-tabname       = 'BESTAND'.                     "n912093
    fieldcat-ref_tabname   = 'MARC'.                        "n912093
    fieldcat-qfieldname    = '/CWM/MEINS'.                  "n912093
    fieldcat-no_out      = 'X'.                             "n912093
    fieldcat-row_pos       = '2'.
    APPEND fieldcat.                                        "n912093

* stock in bwsp                                           "n912093
    CLEAR fieldcat.                                         "n912093
    fieldcat-fieldname     = '/CWM/BWESB'.                  "n912093
    fieldcat-tabname       = 'BESTAND'.                     "n912093
    fieldcat-ref_tabname   = 'MARC'.                        "n912093
    fieldcat-qfieldname    = '/CWM/MEINS'.                  "n912093
    fieldcat-no_out      = 'X'.                             "n912093
    fieldcat-row_pos       = '2'.
    APPEND fieldcat.
  ENDIF.  " cwm

  "{ Begin ENHO DIPCS_RM07MLBS001 IS-AD-SSP AD_SUB }
* GA1727687 - A&D special stocks at subcontractor
  IF NOT pa_sond IS INITIAL   AND
     g_flag_sobkz-lifnr EQ 'X'.
    CLEAR fieldcat.
    fieldcat-fieldname     = 'LIFNR2'.
    fieldcat-tabname       = 'BESTAND'.
    fieldcat-ref_fieldname = 'LIFNR'.
    fieldcat-ref_tabname   = 'MSRD'.
    APPEND fieldcat.
    MOVE fieldcat TO ls_fieldcat.
    LOOP AT fieldcat.
      CASE fieldcat-fieldname.
        WHEN 'LIFNR2'.
          CONTINUE.
        WHEN 'SSNUM'. " field LIFNR is put after SSNUM
          MOVE fieldcat TO lt_fieldcat.
          APPEND lt_fieldcat.
          MOVE ls_fieldcat TO lt_fieldcat.
          APPEND lt_fieldcat.
        WHEN 'DUMMY'.
          ADD 11 TO fieldcat-outputlen.
          MODIFY fieldcat.
          MOVE fieldcat TO lt_fieldcat.
          APPEND lt_fieldcat.
        WHEN OTHERS.
          MOVE fieldcat TO lt_fieldcat.
          APPEND lt_fieldcat.
      ENDCASE.
    ENDLOOP.
    fieldcat[] = lt_fieldcat[].
    CLEAR fieldcat.
  ENDIF.
  "{ End ENHO DIPCS_RM07MLBS001 IS-AD-SSP AD_SUB }

ENHANCEMENT-POINT ehp604_rm07mlbs_24 SPOTS es_rm07mlbs .
  IF  xmchb = 'X'
  OR pa_sond IS NOT INITIAL.
    CLEAR fieldcat.
    fieldcat-fieldname     = 'SGT_SCAT'.
    fieldcat-tabname       = 'BESTAND'.
    fieldcat-ref_tabname   = 'MCHB'.
    APPEND fieldcat.
  ENDIF.
ENDFORM.                               " FELDKATALOG_AUFBAUEN

************************************************************************
* Show the result list
************************************************************************
FORM list_output.

*  set pf-status 'STANDARD'.

ENHANCEMENT-SECTION     rm07mlbs_09 SPOTS es_rm07mlbs.
  keyinfo-header01 = 'MATNR'.
  keyinfo-header02 = 'WERKS'.
  keyinfo-item01   = 'MATNR'.
  keyinfo-item02   = 'WERKS'.
  keyinfo-item03   = 'LGORT'.

* new sort order
  SORT bestand BY matnr werks lgort
                  sobkz kzbws
                  lifnr kunnr vbeln  posnr  pspnr charg.
  "{ Begin ENHO MGV_LAMA_RM07MLBS LO-MD-MM MGV_LAMA }
* Modification for PIC-Supersession/MPN
  DATA:  emnfr LIKE lfa1-emnfr.
  "{ End ENHO MGV_LAMA_RM07MLBS LO-MD-MM MGV_LAMA }
END-ENHANCEMENT-SECTION.


  REFRESH : sort.
  CLEAR   : sort, g_cnt_spos.

* create the sort table for the ALV depending on the
* list type
  IF      NOT pa_hsq IS INITIAL.
*   for hierarchic seq. list

  ELSEIF  NOT  pa_flt IS INITIAL.
*   for flat ( simple ) list
ENHANCEMENT-SECTION     rm07mlbs_10 SPOTS es_rm07mlbs.
    PERFORM  f0400_create_sort    USING  'MATNR'.
END-ENHANCEMENT-SECTION.
    PERFORM  f0400_create_sort    USING  'WERKS'.
  ENDIF.

  PERFORM  f0400_create_sort      USING  'LGORT'.
  PERFORM  f0400_create_sort      USING  'SOBKZ'.
  PERFORM  f0400_create_sort      USING  'KZBWS'.
  PERFORM  f0400_create_sort      USING  'LIFNR'.
  PERFORM  f0400_create_sort      USING  'KUNNR'.

  PERFORM  f0400_create_sort      USING  'VBELN'.
  PERFORM  f0400_create_sort      USING  'POSNR'.
  PERFORM  f0400_create_sort      USING  'PSPNR'.


  DEFINE colourize.
    CLEAR color.
    color-fieldname = &1.
    color-color-int = '0'.
    IF &2 > 0.
      color-color-col = '5'.
    ELSEIF &2 < 0.
      color-color-col = '6'.
    ENDIF.
    APPEND color.
    CASE &1.
      WHEN 'LABST'.
        color-fieldname = 'MEINS'.
        APPEND color.
      WHEN 'WLABS'.
        color-fieldname = 'WAERS'.
        APPEND color.
    ENDCASE.
  END-OF-DEFINITION.
  LOG-POINT ID /cwm/enh SUBKEY to_upper( sy-tcode && '\/CWM/APPL_MM_RM07MLBS_EHP4\EHP604_RM07MLBS_26\' && sy-cprog ) FIELDS /cwm/cl_enh_layer=>get_field( ).
*
  DEFINE /cwm/colourize.
    IF   /cwm/cl_switch_check=>client( ) = /cwm/cl_switch_check=>true.
 CLEAR color.
 color-fieldname = &1.
 color-color-int = '0'.
 IF &2 > 0.
   color-color-col = '5'.
 ELSEIF &2 < 0.
   color-color-col = '6'.
 ENDIF.
 APPEND color.
 CASE &1.
   WHEN '/CWM/LABST'.
     color-fieldname = '/CWM/MEINS'.
     APPEND color.
 ENDCASE.
 ENDIF.
  END-OF-DEFINITION.
ENHANCEMENT-POINT ehp604_rm07mlbs_26 SPOTS es_rm07mlbs .

* skip this loop when the user wants a flat list without
* colors
  IF  NOT pa_hsq     IS INITIAL  OR
      NOT alv_color  =  'X'.
    LOOP AT bestand.
      IF  NOT pa_hsq IS INITIAL.                            "n531604
*       create working table header only if a hierarchic    "n531604
*       list is required                                    "n531604
        ON CHANGE OF bestand-matnr OR bestand-werks.
          MOVE-CORRESPONDING bestand TO header.
          "{ Begin ENHO MGV_LAMA_RM07MLBS LO-MD-MM MGV_LAMA }
          IF cl_immpn_cust=>check_mpn_active( ) = abap_true.
* Modification for PIC-Supersession/MPN

            IF show_ext_manufacturer EQ 'X'.
              CLEAR emnfr.
              CALL FUNCTION 'MPN01_READ_MPN_VIA_MATNR'
                EXPORTING
*                 I_MATNR            = HEADER-MATNR "DI note 762421
                  i_matnr            = bestand-matnr "DI note 762421
                IMPORTING
*                 E_MFRPN            =
                  e_emnfr            = emnfr
                EXCEPTIONS
                  material_is_no_mpn = 1.

              IF sy-subrc EQ 0.
                MOVE emnfr TO header-emnfr.
              ENDIF.
            ENDIF.
          ENDIF.

          "{ End ENHO MGV_LAMA_RM07MLBS LO-MD-MM MGV_LAMA }

ENHANCEMENT-POINT rm07mlbs_2 SPOTS es_rm07mlbs .
          APPEND header.
        ENDON.
      ENDIF.                                                "n531604

*     create the table with the colour information          "n531604
*     depending on the customizing settings in table        "n531604
*     V_MMIM_REP_PRINT   'X' = no colors                    "n531604
      IF  alv_color NE 'X'.                                 "n531604
        REFRESH color.
        colourize 'LABST' bestand-labst.
        colourize 'UMLME' bestand-umlme.
        colourize 'EINME' bestand-einme.
        colourize 'SPEME' bestand-speme.
        colourize 'RETME' bestand-retme.
        colourize 'INSME' bestand-insme.
        colourize 'WLABS' bestand-wlabs.
        colourize 'WUMLM' bestand-wumlm.
        colourize 'WEINM' bestand-weinm.
        colourize 'WSPEM' bestand-wspem.
        colourize 'WRETM' bestand-wretm.
        colourize 'WINSM' bestand-winsm.
        colourize 'BWESB' bestand-bwesb.                    "AC0K020254
        colourize 'WBWESB' bestand-wbwesb.                  "AC0K020254
        colourize 'GLGMG' bestand-glgmg.                    "n912093
        colourize 'WGLGM' bestand-wglgm.                    "n912093
        colourize 'TRAME' bestand-trame.                    "n912093
        colourize 'WTRAM' bestand-wtram.                    "n912093
        colourize 'UMLMC' bestand-umlmc.                    "n912093
        colourize 'WUMLC' bestand-wumlc.                    "n912093
        LOG-POINT ID /cwm/enh SUBKEY to_upper( sy-tcode && '\/CWM/APPL_MM_RM07MLBS_EHP4\EHP604_RM07MLBS_27\' && sy-cprog ) FIELDS /cwm/cl_enh_layer=>get_field( ).
* cwm fields
        /cwm/colourize '/CWM/LABST' bestand-/cwm/labst.
        /cwm/colourize '/CWM/UMLME' bestand-/cwm/umlme.
        /cwm/colourize '/CWM/EINME' bestand-/cwm/einme.
        /cwm/colourize '/CWM/SPEME' bestand-/cwm/speme.
        /cwm/colourize '/CWM/RETME' bestand-/cwm/retme.
        /cwm/colourize '/CWM/INSME' bestand-/cwm/insme.
        /cwm/colourize '/CWM/BWESB' bestand-/cwm/bwesb.
*        /cwm/colourize '/CWM/GLGMG' bestand-/CWM/glgmg.
        /cwm/colourize '/CWM/TRAME' bestand-/cwm/trame.
        /cwm/colourize '/CWM/UMLMC' bestand-/cwm/umlmc.
ENHANCEMENT-POINT ehp604_rm07mlbs_27 SPOTS es_rm07mlbs .
        bestand-farbe = color[].
        MODIFY bestand.
      ENDIF.                                                "n531604
    ENDLOOP.
  ENDIF.                                                    "n531604

* set the name for color table when required                "n531604
  IF  alv_color = 'X'.                                      "n531604
    CLEAR                    layout-coltab_fieldname.       "n531604
  ELSE.                                                     "n531604
    MOVE  'FARBE'            TO  layout-coltab_fieldname.   "n531604
  ENDIF.                                                    "n531604

  layout-group_change_edit = 'X'.

  DATA : l_f_check(01)       TYPE c.
*
  FIELD-SYMBOLS <ls_fieldcat> TYPE slis_fieldcat_alv.
ENHANCEMENT-POINT ehp604_rm07mlbs_28 SPOTS es_rm07mlbs STATIC .

* added dynamic break-point ID MMIM_REP_MB52                "n912093
  BREAK-POINT ID mmim_rep_mb52.                             "n912093

* process the list according the parameters                 "n531604
  IF      NOT pa_hsq IS INITIAL.                            "n531604
*     create a hierarchic list                              "n531604

*     assign form routine for page numbering                "n667256
    gs_events-name         =       'TOP_OF_PAGE'.           "n667256
    gs_events-form         = 'F4000_TOP_OF_PAGE'.           "n667256
    APPEND  gs_events      TO gt_events.                    "n667256
    IF   /cwm/cl_switch_check=>client( ) = /cwm/cl_switch_check=>true.
      LOG-POINT ID /cwm/enh SUBKEY to_upper( sy-tcode && '\/CWM/APPL_MM_RM07MLBS_EHP4\EHP604_RM07MLBS_29\' && sy-cprog ) FIELDS /cwm/cl_enh_layer=>get_field( ).
* CWM activate REPREP Interface
      layout-reprep = 'X'.
      LOOP AT fieldcat ASSIGNING <ls_fieldcat>.
        <ls_fieldcat>-reprep = 'X'.
      ENDLOOP.
    ENDIF.
ENHANCEMENT-POINT ehp604_rm07mlbs_29 SPOTS es_rm07mlbs .


    CALL FUNCTION 'REUSE_ALV_HIERSEQ_LIST_DISPLAY'
      EXPORTING
        i_interface_check  = l_f_check
        i_callback_program = repid
        is_layout          = layout
        it_fieldcat        = fieldcat[]
        i_default          = 'X'
        i_save             = 'A'
        is_variant         = variante
        it_events          = gt_events[]                    "n667256
        i_tabname_header   = 'HEADER'
        i_tabname_item     = 'BESTAND'
        is_keyinfo         = keyinfo
        is_print           = alv_print
        it_sort            = sort[]
        it_excluding       = excluding[]
      TABLES
        t_outtab_header    = header
        t_outtab_item      = bestand
      EXCEPTIONS
        OTHERS             = 2.

  ELSEIF  NOT  pa_flt IS INITIAL.                           "n531604
*   create a flat non-hierarchic list                     "n531604

*   assign form routine for page numbering only for       "n667256
*   classic ALV                                           "n667256
    IF  alv_detail_func = 'REUSE_ALV_LIST_DISPLAY'.         "n667256
      gs_events-name       =       'TOP_OF_PAGE'.           "n667256
      gs_events-form       = 'F4000_TOP_OF_PAGE'.           "n667256
      APPEND  gs_events    TO gt_events.                    "n667256
    ENDIF.
    IF   /cwm/cl_switch_check=>client( ) = /cwm/cl_switch_check=>true.
      LOG-POINT ID /cwm/enh SUBKEY to_upper( sy-tcode && '\/CWM/APPL_MM_RM07MLBS_EHP4\EHP604_RM07MLBS_30\' && sy-cprog ) FIELDS /cwm/cl_enh_layer=>get_field( ).
* CWM activate REPREP Interface
      layout-reprep = 'X'.
      LOOP AT fieldcat ASSIGNING <ls_fieldcat>.
        <ls_fieldcat>-reprep = 'X'.
      ENDLOOP.
    ENDIF.

    "{ Begin ENHO DIPCS_RM07MLBS001 IS-AD-SSP AD_SUB }
* GA1727687 - A&D special stocks at subcontractor
    IF NOT pa_sond IS INITIAL AND
       g_flag_sobkz-lifnr EQ 'X'.
      CLEAR fieldcat.
      fieldcat-fieldname     = 'LIFNR2'.
      fieldcat-tabname       = 'BESTAND'.
      fieldcat-ref_fieldname = 'LIFNR'.
      fieldcat-ref_tabname   = 'MSRD'.
      APPEND fieldcat.
      LOOP AT fieldcat.
        CASE fieldcat-fieldname.
          WHEN 'SSNUM'.
            MOVE fieldcat TO ls_fieldcat.
          WHEN 'LIFNR2'.
            IF ls_fieldcat-col_pos IS NOT INITIAL.
              fieldcat-col_pos = ls_fieldcat-col_pos.
            ENDIF.
            MODIFY fieldcat.
        ENDCASE.
      ENDLOOP.
    ENDIF.
    "{ End ENHO DIPCS_RM07MLBS001 IS-AD-SSP AD_SUB }

ENHANCEMENT-POINT ehp604_rm07mlbs_30 SPOTS es_rm07mlbs .

    CALL FUNCTION alv_detail_func
      EXPORTING
        i_interface_check  = l_f_check
        i_callback_program = repid
        is_layout          = layout
        it_fieldcat        = fieldcat[]
        it_sort            = sort[]
        i_default          = 'X'
        i_save             = 'A'
        is_variant         = variante_flat
        it_events          = gt_events[]                    "n667256
        is_print           = alv_print
      TABLES
        t_outtab           = bestand
      EXCEPTIONS
        OTHERS             = 2.
  ENDIF.                                                    "n531604

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
         WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                               " LISTAUSGABE

*&---------------------------------------------------------------------*
*&      Form  F4_FOR_VARIANT
*&---------------------------------------------------------------------*
*       F4-Hilfe für Reportvariante                                    *
*----------------------------------------------------------------------*
FORM f4_for_variant.

* look for the available display variant depending on the   "n531604
* selected mode of the SAP-LIST-VIEWER
  IF      NOT pa_hsq IS INITIAL.
*     for hierarchic seq. list
    MOVE variante          TO  def_variante_f4.

  ELSEIF  NOT  pa_flt IS INITIAL.
*     for flat ( simple ) list
    MOVE variante_flat     TO  def_variante_f4.
  ENDIF.

  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
    EXPORTING
      is_variant = def_variante_f4
      i_save     = 'A'
*     it_default_fieldcat =
    IMPORTING
      e_exit     = variant_exit
      es_variant = def_variante
    EXCEPTIONS
      not_found  = 2.

  IF sy-subrc = 2.
    MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    IF variant_exit = space.
*     save the selected display variant
      p_vari = def_variante-variant.

      IF      NOT pa_hsq IS INITIAL.
*       for hierarchic seq. list
        MOVE  p_vari         TO  g_f_vari_hsq.

      ELSEIF  NOT  pa_flt IS INITIAL.
*       for flat ( simple ) list
        MOVE  p_vari         TO  g_f_vari_flt.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                               " F4_FOR_VARIANT

*-----------------------------------------------------------"n531604
*    f0000_get_print_settings
*-----------------------------------------------------------"n531604

FORM f0000_get_print_settings.

* get the settings for the SAP-LIST-VIEWER
  SELECT SINGLE * FROM mmim_rep_print
                             WHERE report = sy-repid.

  IF sy-subrc <> 0.
*   if this entry is missing, set default values and insert
*   a new entry into database tabel MMIM_REP_PRINT
    CLEAR                    mmim_rep_print.
    MOVE : sy-repid          TO  mmim_rep_print-report,
           'X'               TO  mmim_rep_print-selinfo.
    INSERT                   mmim_rep_print.

    IF  sy-subrc IS INITIAL.
      COMMIT                 WORK.
    ELSE.
*     error during insert into table MMIN_RE_PRINT
      MESSAGE  s895          WITH  TEXT-895.
    ENDIF.
  ENDIF.

* set the parameter for this run
  IF mmim_rep_print-selinfo = 'X'.
    alv_print-no_print_selinfos = ' '.
  ELSE.
    alv_print-no_print_selinfos = 'X'.
  ENDIF.

  IF mmim_rep_print-coverpage = 'X'.
    alv_print-no_coverpage = ' '.
  ELSE.
    alv_print-no_coverpage = 'X'.
  ENDIF.

  IF mmim_rep_print-listinfo = 'X'.
    alv_print-no_print_listinfos = ' '.
  ELSE.
    alv_print-no_print_listinfos = 'X'.
  ENDIF.

  IF mmim_rep_print-gridcontrol = 'X'.
    alv_detail_func = 'REUSE_ALV_GRID_DISPLAY'.
  ELSE.
    alv_detail_func = 'REUSE_ALV_LIST_DISPLAY'.
  ENDIF.

  IF mmim_rep_print-color = 'X'.
    alv_color       = 'X'.
  ELSE.
    alv_color       = space.
  ENDIF.

ENDFORM.                     "f0000_get_print_settings

*-----------------------------------------------------------"n531604
* Initialization of the user defaults for the checkboxes
*    f0100_settings_init.                                   "n531604
*-----------------------------------------------------------"n531604

FORM f0100_settings_init.                                   "n531604

* only in dialog mode
  CHECK : sy-batch IS INITIAL.

  IF oref_settings IS INITIAL.
    CREATE OBJECT oref_settings
      EXPORTING
        i_action = 'RM07MLBS'.
  ENDIF.

* get the parameters from the last run
  pa_sond   = oref_settings->get( 'PA_SOND' ).
  pa_hsq    = oref_settings->get( 'PA_HSQ' ).

  pa_flt    = oref_settings->get( 'PA_FLT' ).
  negativ   = oref_settings->get( 'NEGATIV' ).
  xmchb     = oref_settings->get( 'XMCHB'   ).
  nozero    = oref_settings->get( 'NOZERO'  ).
  novalues  = oref_settings->get( 'NOVALUES' ).

* check radiobuttons
  IF  pa_hsq IS INITIAL.
    IF  pa_flt IS INITIAL.
*     not allowed
      MOVE  'X'              TO  pa_hsq.
    ENDIF.
  ELSE.
    IF  pa_flt IS INITIAL.
    ELSE.
*     not allowed
      CLEAR                  pa_flt.
    ENDIF.
  ENDIF.

ENDFORM.                     "f0100_settings_init           "n531604

************************************************************************
* Save the user settings
************************************************************************

FORM f0200_settings_save.                                   "n531604

* only in dialog mode
  CHECK : sy-batch IS INITIAL.

* go on when the FORM routines of INITILIZATION             "n667256
* were processed                                            "n667256
  CHECK : g_flag_initialization = 'X'.                      "n667256

* Save the settings
  CALL METHOD oref_settings->set(
      i_element = 'PA_SOND'
      i_active  = pa_sond ).
  CALL METHOD oref_settings->set(
      i_element = 'PA_HSQ'
      i_active  = pa_hsq ).
  CALL METHOD oref_settings->set(
      i_element = 'PA_FLT'
      i_active  = pa_flt ).
  CALL METHOD oref_settings->set(
      i_element = 'NEGATIV'
      i_active  = negativ ).
  CALL METHOD oref_settings->set(
      i_element = 'XMCHB'
      i_active  = xmchb ).

  CALL METHOD oref_settings->set(
      i_element = 'NOZERO'
      i_active  = nozero ).
  CALL METHOD oref_settings->set(
      i_element = 'NOVALUES'
      i_active  = novalues ).

  CALL METHOD oref_settings->flush.

* carry out the database updates only; the normal commit    "n667256
* command does not allow to record this transaction for     "n667256
* a batch input session using transaction SHDB              "n667256
  CALL FUNCTION 'DB_COMMIT'. "n667256

ENDFORM.                     "f0200_settings_save           "n531604

*----------------------------------------------------------------------*
*    f0300_fieldcat_flat
*----------------------------------------------------------------------*

FORM f0300_fieldcat_flat.

* define macro
  DEFINE macro_fill_fieldcat.
    ADD  : 1                 TO  g_cnt_col_pos.
    MOVE : g_cnt_col_pos     TO  fieldcat-col_pos,
           &1                TO  fieldcat-fieldname,
           'BESTAND'         TO  fieldcat-tabname,
           &2                TO  fieldcat-ref_tabname,
           &3                TO  fieldcat-no_out.

    IF  NOT fieldcat-seltext_l IS INITIAL.
      MOVE : fieldcat-seltext_l   TO  fieldcat-seltext_m,
             fieldcat-seltext_l   TO  fieldcat-seltext_s.
    ENDIF.

    APPEND                   fieldcat.
    CLEAR                    fieldcat.
  END-OF-DEFINITION.

ENHANCEMENT-SECTION     rm07mlbs_11 SPOTS es_rm07mlbs.
  macro_fill_fieldcat 'MATNR'  'MARA'   c_out.
END-ENHANCEMENT-SECTION.
  macro_fill_fieldcat 'MAKTX'  'MAKT'   c_no_out.
  macro_fill_fieldcat 'WERKS'  'T001W'  c_out.
  macro_fill_fieldcat 'NAME1'  'T001W'  c_no_out.
  macro_fill_fieldcat 'MTART'  'MARA'   c_no_out.
  macro_fill_fieldcat 'MATKL'  'MARA'   c_no_out.
  macro_fill_fieldcat 'LGORT'  'MARD'   c_out.
  macro_fill_fieldcat 'LGOBE'  'T001L'  c_no_out.           "1311282

  IF NOT pa_sond IS INITIAL.
    macro_fill_fieldcat 'SOBKZ'  'MKOL'   c_out.

    IF  novalues IS INITIAL.
      macro_fill_fieldcat 'KZBWS'  'MSSA'   c_out.
    ENDIF.

    MOVE : 'SSNUM'           TO  fieldcat-ref_fieldname.
    macro_fill_fieldcat 'SSNUM' 'BICKEY' c_out.
  ENDIF.

  macro_fill_fieldcat 'LVORM'  'MARD'   c_out.

  IF xmchb = 'X'.
    macro_fill_fieldcat 'CHARG'  'MCHB'   c_out.
  ENDIF.

  macro_fill_fieldcat 'MEINS'  'MARA'   c_out.

* Stock and value for stock unrestrestricted use
  fieldcat-qfieldname    = 'MEINS'.
  macro_fill_fieldcat 'LABST'  'MARD'   c_out.

  IF xmchb = 'X'
  OR pa_sond IS NOT INITIAL.
    macro_fill_fieldcat 'SGT_SCAT'  'MCHB'   c_out.
  ENDIF.
  IF   /cwm/cl_switch_check=>client( ) = /cwm/cl_switch_check=>true.
    LOG-POINT ID /cwm/enh SUBKEY to_upper( sy-tcode && '\/CWM/APPL_MM_RM07MLBS_EHP4\EHP604_RM07MLBS_31\' && sy-cprog ) FIELDS /cwm/cl_enh_layer=>get_field( ).
*
    macro_fill_fieldcat '/CWM/MEINS'  '/CWM/MARD_PQ'   c_out.
    fieldcat-qfieldname    = '/CWM/MEINS'.
    macro_fill_fieldcat '/CWM/LABST'  'MARD'   c_out.
  ENDIF.
ENHANCEMENT-POINT ehp604_rm07mlbs_31 SPOTS es_rm07mlbs .

  IF novalues IS INITIAL.
    fieldcat-outputlen     = 5.
    macro_fill_fieldcat 'WAERS'  'T001'   c_out.

    fieldcat-ref_fieldname = 'SALK3'.
    fieldcat-cfieldname    = 'WAERS'.
*   'Wert frei verwend.'.
    fieldcat-seltext_l     = TEXT-021.
    fieldcat-do_sum        = 'X'.
    fieldcat-outputlen     = '18'.
    macro_fill_fieldcat 'WLABS'  'MBEW'   c_out.
  ENDIF.

* stock and value for stock in transfer
  fieldcat-qfieldname    = 'MEINS'.
  fieldcat-ref_fieldname = 'MB52_TRAUML'.                   "n912093
  macro_fill_fieldcat 'UMLME'  'AM07M'   c_out.             "n912093
  IF   /cwm/cl_switch_check=>client( ) = /cwm/cl_switch_check=>true.
    LOG-POINT ID /cwm/enh SUBKEY to_upper( sy-tcode && '\/CWM/APPL_MM_RM07MLBS_EHP4\EHP604_RM07MLBS_32\' && sy-cprog ) FIELDS /cwm/cl_enh_layer=>get_field( ).
*
    fieldcat-qfieldname    = '/CWM/MEINS'.
    fieldcat-ref_fieldname = 'MB52_TRAUML'.                 "n912093
    macro_fill_fieldcat '/CWM/UMLME'  'AM07M'   c_out.      "n912093
  ENDIF.
ENHANCEMENT-POINT ehp604_rm07mlbs_32 SPOTS es_rm07mlbs .

  IF novalues IS INITIAL.
    fieldcat-ref_fieldname = 'SALK3'.
    fieldcat-cfieldname    = 'WAERS'.
*   'Wert in Umlagerung'. // Wert Umlag u. Transit     "n912093
*   'Wert Umlag.Bestand'. // n912093                   "n912093
    fieldcat-seltext_l     = TEXT-030.                      "n912093
    fieldcat-do_sum        = 'X'.
    fieldcat-outputlen     = '18'.
    macro_fill_fieldcat 'WUMLM'  'MBEW'   c_out.
  ENDIF.

* stock and value for stock in quality inspection
  fieldcat-qfieldname    = 'MEINS'.
  macro_fill_fieldcat 'INSME'  'MARD'   c_out.
  IF   /cwm/cl_switch_check=>client( ) = /cwm/cl_switch_check=>true.
    LOG-POINT ID /cwm/enh SUBKEY to_upper( sy-tcode && '\/CWM/APPL_MM_RM07MLBS_EHP4\EHP604_RM07MLBS_33\' && sy-cprog ) FIELDS /cwm/cl_enh_layer=>get_field( ).
*
    fieldcat-qfieldname    = '/CWM/MEINS'.
    macro_fill_fieldcat '/CWM/INSME'  'MARD'   c_out.
  ENDIF.
ENHANCEMENT-POINT ehp604_rm07mlbs_33 SPOTS es_rm07mlbs .

  IF novalues IS INITIAL.
    fieldcat-ref_fieldname = 'SALK3'.
    fieldcat-cfieldname    = 'WAERS'.
*   "Wert in QualPrüfng'
    fieldcat-seltext_l     = TEXT-023.
    fieldcat-do_sum        = 'X'.
    fieldcat-outputlen     = '18'.
    macro_fill_fieldcat 'WINSM'  'MBEW'   c_out.
  ENDIF.

* stock and value for restricted stock
  fieldcat-qfieldname    = 'MEINS'.
  macro_fill_fieldcat 'EINME'  'MARD'   c_out.
  IF   /cwm/cl_switch_check=>client( ) = /cwm/cl_switch_check=>true.
    LOG-POINT ID /cwm/enh SUBKEY to_upper( sy-tcode && '\/CWM/APPL_MM_RM07MLBS_EHP4\EHP604_RM07MLBS_34\' && sy-cprog ) FIELDS /cwm/cl_enh_layer=>get_field( ).
*
    fieldcat-qfieldname    = '/CWM/MEINS'.
    macro_fill_fieldcat '/CWM/EINME'  'MARD'   c_out.
  ENDIF.
ENHANCEMENT-POINT ehp604_rm07mlbs_34 SPOTS es_rm07mlbs .

  IF novalues IS INITIAL.
    fieldcat-ref_fieldname = 'SALK3'.
    fieldcat-cfieldname    = 'WAERS'.
*   "Wert nicht frei'
    fieldcat-seltext_l     = TEXT-024.
    fieldcat-do_sum        = 'X'.
    fieldcat-outputlen     = '18'.
    macro_fill_fieldcat 'WEINM'  'MBEW'   c_out.
  ENDIF.

* stock and value for blocked stock
  fieldcat-qfieldname    = 'MEINS'.
  macro_fill_fieldcat 'SPEME'  'MARD'   c_out.
  IF   /cwm/cl_switch_check=>client( ) = /cwm/cl_switch_check=>true.
    LOG-POINT ID /cwm/enh SUBKEY to_upper( sy-tcode && '\/CWM/APPL_MM_RM07MLBS_EHP4\EHP604_RM07MLBS_35\' && sy-cprog ) FIELDS /cwm/cl_enh_layer=>get_field( ).
*
    fieldcat-qfieldname    = '/CWM/MEINS'.
    macro_fill_fieldcat '/CWM/SPEME'  'MARD'   c_out.
  ENDIF.
ENHANCEMENT-POINT ehp604_rm07mlbs_35 SPOTS es_rm07mlbs .

  IF novalues IS INITIAL.
    fieldcat-ref_fieldname = 'SALK3'.
    fieldcat-cfieldname    = 'WAERS'.
*   'Wert Sperrbestand'
    fieldcat-seltext_l     = TEXT-025.
    fieldcat-do_sum        = 'X'.
    fieldcat-outputlen     = '18'.
    macro_fill_fieldcat 'WSPEM'  'MBEW'   c_out.
  ENDIF.

* stock and value for blocked returns
  fieldcat-qfieldname    = 'MEINS'.
  macro_fill_fieldcat 'RETME'  'MARD'   c_out.
  IF   /cwm/cl_switch_check=>client( ) = /cwm/cl_switch_check=>true.
    LOG-POINT ID /cwm/enh SUBKEY to_upper( sy-tcode && '\/CWM/APPL_MM_RM07MLBS_EHP4\EHP604_RM07MLBS_36\' && sy-cprog ) FIELDS /cwm/cl_enh_layer=>get_field( ).
*
    fieldcat-qfieldname    = '/CWM/MEINS'.
    macro_fill_fieldcat '/CWM/RETME'  'MARD'   c_out.
  ENDIF.
ENHANCEMENT-POINT ehp604_rm07mlbs_36 SPOTS es_rm07mlbs .

* Values
  IF novalues IS INITIAL.
    fieldcat-ref_fieldname = 'SALK3'.
    fieldcat-cfieldname    = 'WAERS'.
*   "Wert RetourenSperr'.
    fieldcat-seltext_l     = TEXT-026.
    fieldcat-do_sum        = 'X'.
    fieldcat-outputlen     = '18'.
    macro_fill_fieldcat 'WRETM'  'MBEW'   c_out.
  ENDIF.                               "novalues is initial

* process valuated block GR stock as hidden field        "AC0K020254
  fieldcat-qfieldname    = 'MEINS'.                         "AC0K020254
  macro_fill_fieldcat 'BWESB'  'MARC'   c_no_out.           "AC0K020254
  IF   /cwm/cl_switch_check=>client( ) = /cwm/cl_switch_check=>true.
    LOG-POINT ID /cwm/enh SUBKEY to_upper( sy-tcode && '\/CWM/APPL_MM_RM07MLBS_EHP4\EHP604_RM07MLBS_37\' && sy-cprog ) FIELDS /cwm/cl_enh_layer=>get_field( ).
*
    fieldcat-qfieldname    = '/CWM/MEINS'.                  "AC0K020254
    macro_fill_fieldcat '/CWM/BWESB'  'MARC'   c_no_out.    "AC0K020254
  ENDIF.
ENHANCEMENT-POINT ehp604_rm07mlbs_37 SPOTS es_rm07mlbs .
                                                            "AC0K020254
* the estimated value for the valuated block GR stock    "AC0K020254
* as hidden field, too                                   "AC0K020254
  IF novalues IS INITIAL.                                   "AC0K020254
    fieldcat-ref_fieldname = 'SALK3'.                       "AC0K020254
    fieldcat-cfieldname    = 'WAERS'.                       "AC0K020254
*   value blocked GR stock                               "AC0K020254
    fieldcat-seltext_l     = TEXT-027.                      "AC0K020254
* fill the DDIC text to prevent reading DDIC for SALK3          "2323380
    fieldcat-reptext_ddic  = TEXT-027.                      "2323380
    fieldcat-do_sum        = 'X'.                           "AC0K020254
    fieldcat-outputlen     = '18'.                          "AC0K020254
    macro_fill_fieldcat 'WBWESB'  'MBEW'   c_no_out.        "AC0K020254
    CLEAR fieldcat-reptext_ddic.                            "2323380
  ENDIF.

* stock and value for tied empties                    "n912093
  fieldcat-qfieldname    = 'MEINS'.                         "n912093
  macro_fill_fieldcat 'GLGMG'  'MARC'   c_no_out.           "n912093
  IF   /cwm/cl_switch_check=>client( ) = /cwm/cl_switch_check=>true.
    LOG-POINT ID /cwm/enh SUBKEY to_upper( sy-tcode && '\/CWM/APPL_MM_RM07MLBS_EHP4\EHP604_RM07MLBS_38\' && sy-cprog ) FIELDS /cwm/cl_enh_layer=>get_field( ).
*
*  fieldcat-qfieldname    = '/CWM/MEINS'.                   "n912093
*  macro_fill_fieldcat '/CWM/GLGMG'  'MARC'   c_no_out.     "n912093
  ENDIF.
ENHANCEMENT-POINT ehp604_rm07mlbs_38 SPOTS es_rm07mlbs .
                                                            "n912093
* Values                                              "n912093
  IF novalues IS INITIAL.                                   "n912093
    fieldcat-ref_fieldname = 'SALK3'.                       "n912093
    fieldcat-cfieldname    = 'WAERS'.                       "n912093
*   'Wert gebundenes Leergut'                         "n912093
    fieldcat-seltext_l     = TEXT-028.                      "n912093
    fieldcat-do_sum        = 'X'.                           "n912093
    fieldcat-outputlen     = '18'.                          "n912093
    macro_fill_fieldcat 'WGLGM'  'MBEW'   c_no_out.         "n912093
  ENDIF.                                                    "n912093


* stock and value for stock in transit                "n912093
  fieldcat-qfieldname    = 'MEINS'.                         "n912093
  macro_fill_fieldcat 'TRAME'  'MARC'   c_no_out.           "n912093
  IF   /cwm/cl_switch_check=>client( ) = /cwm/cl_switch_check=>true.
    LOG-POINT ID /cwm/enh SUBKEY to_upper( sy-tcode && '\/CWM/APPL_MM_RM07MLBS_EHP4\EHP604_RM07MLBS_39\' && sy-cprog ) FIELDS /cwm/cl_enh_layer=>get_field( ).
*
    fieldcat-qfieldname    = '/CWM/MEINS'.                  "n912093
    macro_fill_fieldcat '/CWM/TRAME'  'MARC'   c_no_out.    "n912093
  ENDIF.
ENHANCEMENT-POINT ehp604_rm07mlbs_39 SPOTS es_rm07mlbs .
                                                            "n912093
* Values                                              "n912093
  IF novalues IS INITIAL.                                   "n912093
    fieldcat-ref_fieldname = 'SALK3'.                       "n912093
    fieldcat-cfieldname    = 'WAERS'.                       "n912093
*   'Wert Transitbestand'                             "n912093
    fieldcat-seltext_l     = TEXT-029.                      "n912093
    fieldcat-do_sum        = 'X'.                           "n912093
    fieldcat-outputlen     = '18'.                          "n912093
    macro_fill_fieldcat 'WTRAM'  'MBEW'   c_no_out.         "n912093
  ENDIF.                                                    "n912093

* stock and value for stock in transit                "n912093
  fieldcat-qfieldname    = 'MEINS'.                         "n912093
  macro_fill_fieldcat 'UMLMC'  'MARC'   c_no_out.           "n912093
  IF   /cwm/cl_switch_check=>client( ) = /cwm/cl_switch_check=>true.
    LOG-POINT ID /cwm/enh SUBKEY to_upper( sy-tcode && '\/CWM/APPL_MM_RM07MLBS_EHP4\EHP604_RM07MLBS_40\' && sy-cprog ) FIELDS /cwm/cl_enh_layer=>get_field( ).
*
    fieldcat-qfieldname    = '/CWM/MEINS'.                  "n912093
    macro_fill_fieldcat '/CWM/UMLMC'  'MARC'   c_no_out.    "n912093
  ENDIF.
ENHANCEMENT-POINT ehp604_rm07mlbs_40 SPOTS es_rm07mlbs .
                                                            "n912093
* Values                                              "n912093
  IF novalues IS INITIAL.                                   "n912093
    fieldcat-ref_fieldname = 'SALK3'.                       "n912093
    fieldcat-cfieldname    = 'WAERS'.                       "n912093
*   'Wert Umlagerung an Werk'                         "n912093
    fieldcat-seltext_l     = TEXT-022.                      "n912093
    fieldcat-do_sum        = 'X'.                           "n912093
    fieldcat-outputlen     = '18'.                          "n912093
    macro_fill_fieldcat 'WUMLC'  'MBEW'   c_no_out.         "n912093
  ENDIF.                                                    "n912093

* set the key fields of the special stock as hidden fields
  IF  g_flag_sobkz-lifnr = 'X'.
    macro_fill_fieldcat 'LIFNR'  'MKOL'   c_no_out.
  ENDIF.

  IF  g_flag_sobkz-kunnr = 'X'.
    macro_fill_fieldcat 'KUNNR'  'MSKU'   c_no_out.
  ENDIF.

  IF  g_flag_sobkz-vbeln = 'X'.
    macro_fill_fieldcat 'VBELN'  'MSKA'   c_no_out.
    macro_fill_fieldcat 'POSNR'  'MSKA'   c_no_out.
  ENDIF.

  IF  g_flag_sobkz-pspnr = 'X'.
    macro_fill_fieldcat 'PSPNR'  'MSPR'   c_no_out.
  ENDIF.

ENDFORM.                     "f0300_fieldcat_flat

*----------------------------------------------------------------------*
*    f0400_create_sort
*----------------------------------------------------------------------*

FORM f0400_create_sort
                   USING     l_f_fieldname LIKE sort-fieldname.

* create the table with the alv sort information

  IF      NOT pa_hsq IS INITIAL.
*   for hierarchic seq. list
*   check whether this is an active field is in the
*   fieldcat
    READ TABLE fieldcat WITH KEY
                             fieldname = l_f_fieldname
                             no_out    = space.

  ELSEIF  NOT  pa_flt IS INITIAL.
*   for flat ( simple ) list
*   check whether this field is in the fieldcat
    READ TABLE fieldcat WITH KEY
                             fieldname = l_f_fieldname.
  ENDIF.

  IF sy-subrc IS INITIAL.
    ADD  1                   TO  g_cnt_spos.
    MOVE : g_cnt_spos        TO  sort-spos,
           l_f_fieldname     TO  sort-fieldname,
           'X'               TO  sort-up,
           'BESTAND'         TO  sort-tabname.
    APPEND                   sort.
    CLEAR                    sort.
  ENDIF.

ENDFORM.                     "f0400_create_sort

*----------------------------------------------------------------------*
*    f2000_COLLECT_collector.
*----------------------------------------------------------------------*

FORM f2000_collect_collector.

* does the user want to suppress stock objects from plant   "n577268
* level ?                                                   "n577268
  IF  g_flag_suppress_init_lgort = 'X'.                     "n577268
    IF  collector-lgort IS INITIAL.                         "n577268
*     ignore stock objects without storage location         "n577268
      EXIT.                  "--> go to exit                "n577268
    ENDIF.                                                  "n577268
  ENDIF.                                                    "n577268

************************************************************************
* process the functions "No zero stocks",
* "Negative stocks only", "Without batches" here
************************************************************************
ENHANCEMENT-POINT ehp604_rm07mlbs_41 SPOTS es_rm07mlbs .

  LOG-POINT ID /cwm/enh SUBKEY to_upper( sy-tcode && '\/CWM/APPL_MM_RM07MLBS_EHP4\EHP604_RM07MLBS_41\' && sy-cprog ) FIELDS /cwm/cl_enh_layer=>get_field( ).
*
  IF negativ = 'X'.
*   ignore entry if all stocks are zero or greater
    IF collector-labst >= 0 AND collector-einme >= 0 AND
       collector-insme >= 0 AND collector-retme >= 0 AND
       collector-speme >= 0 AND collector-umlme >= 0.
      IF collector-/cwm/labst >= 0 AND collector-/cwm/einme >= 0 AND
       collector-/cwm/insme >= 0 AND collector-/cwm/retme >= 0 AND
       collector-/cwm/speme >= 0 AND collector-/cwm/umlme >= 0.
        EXIT.                  "--> go to exit
      ENDIF.
    ENDIF.
  ENDIF.

  IF nozero = 'X'.
*   ignore all entries without stock
    IF collector-labst = 0 AND collector-einme = 0 AND
       collector-insme = 0 AND collector-retme = 0 AND
       collector-speme = 0 AND collector-umlme = 0.
      IF collector-/cwm/labst = 0 AND collector-/cwm/einme = 0 AND
         collector-/cwm/insme = 0 AND collector-/cwm/retme = 0 AND
         collector-/cwm/speme = 0 AND collector-/cwm/umlme = 0.
        EXIT.                  "--> go to exit
      ENDIF.
    ENDIF.
  ENDIF.

  IF xmchb IS INITIAL.
    CLEAR collector-charg.
  ENDIF.
  IF /cwm/cl_switch_check=>client( ) = /cwm/cl_switch_check=>true.
    CALL FUNCTION '/CWM/MDMM_VALUATION_UNIT_GET'
      EXPORTING
        i_material              = collector-matnr
      IMPORTING
        e_catch_weight_material = collector-/cwm/xcwmat
        e_unit_of_valuation     = collector-/cwm/valum
      EXCEPTIONS
        material_not_found      = 0
        conversion_not_found    = 0
        OTHERS                  = 0.
    IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
    IF collector-/cwm/xcwmat IS NOT INITIAL. " !!!!!
      CALL FUNCTION '/CWM/MDMM_2TQ_BASE_UNIT_GET'
        EXPORTING
          i_material              = collector-matnr
        IMPORTING
          e_catch_weight_material = collector-/cwm/xcwmat
          e_base_unit_of_2tq      = collector-/cwm/meins
        EXCEPTIONS
          OTHERS                  = 0.
    ELSE.
      CLEAR  collector-/cwm/meins.
    ENDIF.
  ENDIF.
  COLLECT                    collector.

ENDFORM.                     "f2000_COLLECT_collector.

*-----------------------------------------------------------"n579976
*     F3000_SEND_WARNING_M7_393                             "n579976
*-----------------------------------------------------------"n579976
                                                            "n579976
*&---------------------------------------------------------------------*
*&      Form  F3000_SEND_WARNING_M7_393
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->L_VARIANT  text
*----------------------------------------------------------------------*
FORM f3000_send_warning_m7_393                              "n579976
         USING     l_variant LIKE  disvariant-variant.      "n579976
                                                            "n579976
* check the customising settings : emerge warning 393 ?     "n579976
  CALL FUNCTION 'ME_CHECK_T160M'               "n579976
    EXPORTING                                               "n579976
      i_arbgb = 'M7'                         "n579976
      i_msgnr = '393'                        "n579976
    EXCEPTIONS                                              "n579976
      nothing = 0                            "n579976
      OTHERS  = 1.                           "n579976
                                                            "n579976
  IF sy-subrc <> 0.                                         "n579976
*   list will be created using the initial layout &         "n579976
    MESSAGE i393             WITH  l_variant.               "n667256
  ENDIF.                                                    "n579976
                                                            "n579976
ENDFORM.                     "F3000_SEND_WARNING_M7_393     "n579976
                                                            "n579976
*-----------------------------------------------------------"n579976


*-----------------------------------------------------------"n667256
*  F4000_TOP_OF_PAGE.                                       "n667256
*-----------------------------------------------------------"n667256
                                                            "n667256

AT SELECTION-SCREEN OUTPUT.                                 "n667256
                                                            "n667256

  "{ Begin ENHO AD_MPN_MD_RM07MLBS IS-AD-MPN-MD AD_MPN }

* Modification for PIC-Supersession/MPN
* Perform some Modifications @ the Selection Screen, if needed.
  IF cl_immpn_cust=>check_mpn_active( ) = abap_false.

    LOOP AT SCREEN.
      IF screen-name EQ 'MFRPN-LOW' OR
         screen-name EQ 'MFRPN-HIGH' OR
         screen-name EQ '%_MFRPN_%_APP_%-TEXT' OR
         screen-name EQ '%_MFRPN_%_APP_%-OPTI_PUSH' OR
         screen-name EQ '%_MFRPN_%_APP_%-TO_TEXT' OR
         screen-name EQ '%_MFRPN_%_APP_%-VALU_PUSH'.
        screen-invisible = 1.
        screen-active = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.

  ENDIF.
  "{ End ENHO AD_MPN_MD_RM07MLBS IS-AD-MPN-MD AD_MPN }

ENHANCEMENT-POINT rm07mlbs_04 SPOTS es_rm07mlbs.
  IF  g_flag_initialization IS INITIAL.                     "n667256
*   the process time INITIALIZATION was not done, so        "n667256
*   carry out the functions here                            "n667256
    MOVE  'X'                TO g_flag_initialization.      "n667256
                                                            "n667256
    PERFORM                  f0000_get_print_settings.      "n667256
                                                            "n667256
*   look for the setting of the parameters from the         "n667256
*   last run                                                "n667256
    PERFORM                  f0100_settings_init.           "n667256
                                                            "n667256
    PERFORM                  initialisierung.               "n667256
  ENDIF.                                                    "n667256
                                                            "n667256
*-----------------------------------------------------------"n667256

*------------------------- Datenselektion -----------------------------*
FORM f4000_top_of_page.                       "#EC CALLED      "n667256
                                                            "n667256
* go on if there is a print destination set                 "n667256
  CHECK   NOT sy-prdsn IS INITIAL.                          "n667256

* go on if it is in print modus, only                       "n960980
  CHECK sy-ucomm = '&RNT' OR sy-ucomm IS INITIAL.           "n960980

                                                            "n667256
*     classic ALV : use the simple write command            "n667256
  WRITE : sy-datlo DD/MM/YYYY, sy-title, sy-pagno.          "n667256
                                                            "n667256
                                                            "n667256
ENDFORM.                     "F4000_TOP_OF_PAGE.            "n667256
                                                            "n667256
*-----------------------------------------------------------"n667256

*&---------------------------------------------------------------------*
*&      Form  data_selection_new
*&---------------------------------------------------------------------*
FORM data_selection_new.                                    "n1795093

* Materials to be processed
  TYPES: BEGIN OF ty_mat,
           matnr      LIKE mara-matnr,
           werks      LIKE marc-werks,
           xchar      LIKE marc-xchar,
           mtart      LIKE mara-mtart,
           matkl      LIKE mara-matkl,
           meins      LIKE mara-meins,
           trame      LIKE marc-trame,
           umlmc      LIKE marc-umlmc,
           glgmg      LIKE marc-glgmg,                      "n912093
           bwesb      LIKE  marc-bwesb,                     "AC0K020254
           lvorm_mara LIKE  mara-lvorm,
           lvorm_marc LIKE  marc-lvorm,
           maktx      LIKE makt-maktx.                      "n1795093
  TYPES:  END OF ty_mat.

  DATA: t_mat     TYPE ty_mat OCCURS 0 WITH HEADER LINE.

* buffer for reading working tables
  DATA : l_s_mat   TYPE  ty_mat,
         l_f_matnr LIKE  makt-matnr.

  DATA: sql_ex TYPE REF TO cx_sy_open_sql_db.

  RANGES: r_sobkz FOR mkol-sobkz.

  DATA:
*       workin table for the material stock valuation
    BEGIN OF t_mbew OCCURS 0,
      matnr     LIKE mbew-matnr,
      bwkey     LIKE mbew-bwkey,
      bwtar     LIKE mbew-bwtar,
*         consider the valuation of the special stocks E, Q "n531604
      sobkz     LIKE  ebew-sobkz,                           "n531604
      vbeln     LIKE  ebew-vbeln,                           "n531604
      posnr     LIKE  ebew-posnr,                           "n531604
      pspnr     LIKE  qbew-pspnr,                           "n531604

      lbkum(12) TYPE p DECIMALS 3,                          "407810
      salk3(12) TYPE p DECIMALS 2,                          "388735
      vprsv     LIKE mbew-vprsv,                            "353428
      verpr     LIKE mbew-verpr,                            "353428
      stprs     LIKE mbew-stprs,                            "353428
      peinh     LIKE mbew-peinh,                            "353428
    END OF t_mbew.

  DATA: t_ebew    LIKE t_mbew    OCCURS 0 WITH HEADER LINE.
  DATA: t_qbew    LIKE t_mbew    OCCURS 0 WITH HEADER LINE.

  DATA: lv_subrc LIKE sy-subrc.

  DATA: r_stopt   TYPE RANGE OF labst,
        lsr_stopt LIKE LINE OF r_stopt.

  DATA: factor TYPE f.

  DATA: r_matnr   TYPE RANGE OF matnr,
        lsr_matnr LIKE LINE OF r_matnr,
        ls_matnr  LIKE LINE OF collector.

  FIELD-SYMBOLS: <fs_col_mstb> LIKE LINE OF collector_mstb,
                 <fs_col_uml>  LIKE LINE OF collector_uml.

  DATA: collector_mstb_agr LIKE collector_mstb,
        lv_exists          TYPE mandt.

  DATA: lv_xmbew TYPE xfeld,                                "1895376
        lv_xebew TYPE xfeld,                                "1895376
        lv_xqbew TYPE xfeld.                                "1895376

  DATA: lv_dynwhere TYPE string.                            "1917052
  DATA: lv_dynhvng  TYPE string.                            "2561903

  DATA: ls_lgort      LIKE LINE OF lgort,                   "2225802
        l_flag_xlgort TYPE xfeld.

  DATA: ls_table_range  TYPE rsds_range,
        lt_table_range  TYPE rsds_trange,
        ls_field_range  TYPE rsds_frange,
        lt_where_clause TYPE rsds_twhere,
        lt_where        TYPE rsds_where_tab,
        ls_where_clause TYPE rsds_where.
  FIELD-SYMBOLS: <fs_rsds_range> TYPE LINE OF rsds_selopt_t. "2225802

  DATA: lt_mbew TYPE STANDARD TABLE OF mbew,                "v2561903
        ls_mbew LIKE LINE OF lt_mbew,
        lt_ebew TYPE STANDARD TABLE OF ebew,
        ls_ebew LIKE LINE OF lt_ebew,
        lt_qbew TYPE STANDARD TABLE OF qbew,
        ls_qbew LIKE LINE OF lt_qbew,
        lt_obew TYPE STANDARD TABLE OF obew,
        ls_obew LIKE LINE OF lt_obew.                       "^2561903

  DATA  lv_rere_wertu TYPE abap_bool.                        "2697257

************************************************************************
* Read material master data (MARA and MARC)
************************************************************************
  REFRESH collector.

* re-read WERTU to sort out non-valuated materials with MBEW "2697257
  SELECT SINGLE STATUS FROM mmim_control_log                 "2697257
                       INTO lv_rere_wertu                    "2697257
                       WHERE action = 'RE_READ_WERTU'        "2697257
                       AND   repid  = 'RM07MLBS'.            "2697257

* evaluate check boxes...
  IF xmchb IS INITIAL.                                      "v2045154
    IF nozero = 'X'.
      lsr_stopt-sign   = 'I'.
      lsr_stopt-option = 'GT'. "greater than
      lsr_stopt-low    = '0'.
      APPEND lsr_stopt TO r_stopt.
      lsr_stopt-sign   = 'I'.
      lsr_stopt-option = 'LT'. "less than
      lsr_stopt-low    = '0'.
      APPEND lsr_stopt TO r_stopt.
      lv_dynhvng = 'SUM( vmsl ) < 0 OR SUM( vmsl ) > 0' ##NO_TEXT. "2561903
    ELSE.
      REFRESH r_stopt.
      CLEAR lv_dynhvng.                                     "2561903
    ENDIF.
  ELSE.                                                     "^2045154
    IF ( negativ = 'X' AND nozero = 'X' ) OR negativ = 'X'.
      lsr_stopt-sign   = 'I'.
      lsr_stopt-option = 'LT'. "less than
      lsr_stopt-low    = '0'.
      APPEND lsr_stopt TO r_stopt.
      lv_dynhvng = 'SUM( vmsl ) < 0' ##NO_TEXT.             "2561903
    ELSEIF nozero = 'X'.
      lsr_stopt-sign   = 'I'.
      lsr_stopt-option = 'GT'. "greater than
      lsr_stopt-low    = '0'.
      APPEND lsr_stopt TO r_stopt.
      lsr_stopt-sign   = 'I'.
      lsr_stopt-option = 'LT'. "less than
      lsr_stopt-low    = '0'.
      APPEND lsr_stopt TO r_stopt.
      lv_dynhvng = 'SUM( vmsl ) < 0 OR SUM( vmsl ) > 0' ##NO_TEXT. "2561903
    ELSE.
      REFRESH r_stopt.
      CLEAR lv_dynhvng.                                     "2561903
    ENDIF.
  ENDIF.                                                    "2045154

  LOOP AT lgort INTO ls_lgort.                              "2225802
    IF NOT ( ls_lgort-option = 'EQ' AND ls_lgort-sign = 'I' ) "2225802
        OR ( ls_lgort-option = 'EQ' AND ls_lgort-sign = 'I' "2289076
        AND ls_lgort-low IS INITIAL ).                      "2289076
      l_flag_xlgort = abap_true.                            "2225802
      EXIT.                                                 "2225802
    ENDIF.                                                  "2225802
  ENDLOOP.                                                  "2225802

  BREAK-POINT ID mmim_rep_mb52.


  "{ Begin ENHO AD_MPN_MD_RM07MLBS IS-AD-MPN-MD AD_MPN }
*ehp604_rm07mlbs_08
* transform range MFRPN to dynmaic where which will be applied to the
* select statements within form data_selection_new
  IF cl_immpn_cust=>check_mpn_active( ) = abap_true.

    LOOP AT mfrpn.
      APPEND INITIAL LINE TO ls_field_range-selopt_t ASSIGNING <fs_rsds_range>.
      MOVE-CORRESPONDING mfrpn TO <fs_rsds_range>.
    ENDLOOP.

    ls_table_range-tablename = 'MARA'.
    ls_field_range-fieldname = 'MFRPN'.
    APPEND ls_field_range TO ls_table_range-frange_t.
    APPEND ls_table_range TO lt_table_range.

    CALL FUNCTION 'FREE_SELECTIONS_RANGE_2_WHERE'
      EXPORTING
        field_ranges  = lt_table_range
      IMPORTING
        where_clauses = lt_where_clause.

    READ TABLE lt_where_clause INTO ls_where_clause INDEX 1.
    APPEND LINES OF ls_where_clause-where_tab TO lt_where.
    CONCATENATE LINES OF lt_where INTO lv_dynwhere SEPARATED BY space.
*                                                                          "^1939557
  ENDIF.
  "{ End ENHO AD_MPN_MD_RM07MLBS IS-AD-MPN-MD AD_MPN }


ENHANCEMENT-POINT rm07mlbs_14 SPOTS es_rm07mlbs .

  TRY.

* process SELECT command depending on the
* required material selection

* Get MARC stock and master data plus maktx
      CLEAR t_mat[].

      IF lgort[] IS INITIAL OR l_flag_xlgort IS NOT INITIAL. "2225802
        SELECT mara~matnr werks xchar mtart matkl meins SUM( trame ) AS trame SUM( umlmc ) AS umlmc
               SUM( bwesb ) AS bwesb SUM( glgmg ) AS glgmg maktx
             mara~lvorm AS lvorm_mara
             marc~lvorm AS lvorm_marc
        CONNECTION (dbcon)
             INTO CORRESPONDING FIELDS OF TABLE t_mat
             FROM mara
             INNER JOIN marc
                ON mara~matnr = marc~matnr
             LEFT JOIN makt
                ON makt~matnr = mara~matnr AND
                   makt~spras = sy-langu
             WHERE mara~matnr IN matnr
               AND werks IN werks
               AND mtart IN matart
               AND matkl IN matkla
               AND ekgrp IN ekgrup
               AND (lv_dynwhere)                            "1917052
             GROUP BY mara~matnr werks xchar mtart matkl meins mara~lvorm marc~lvorm maktx.
      ELSE.                                                 "v1904998
        SELECT mara~matnr mard~werks xchar mtart matkl meins SUM( trame ) AS trame SUM( umlmc ) AS umlmc
              SUM( bwesb ) AS bwesb SUM( glgmg ) AS glgmg maktx
            mara~lvorm AS lvorm_mara
            marc~lvorm AS lvorm_marc
       CONNECTION (dbcon)
            INTO CORRESPONDING FIELDS OF TABLE t_mat
            FROM mara
            INNER JOIN marc
               ON mara~matnr = marc~matnr
            INNER JOIN mard
              ON  mard~matnr = marc~matnr AND
                  mard~werks = marc~werks
            LEFT JOIN makt
               ON makt~matnr = mara~matnr AND
                  makt~spras = sy-langu
            WHERE mara~matnr IN matnr
              AND mard~werks IN werks
              AND mard~lgort IN lgort
              AND mtart IN matart
              AND matkl IN matkla
              AND ekgrp IN ekgrup
              AND (lv_dynwhere)                             "1917052
            GROUP BY mara~matnr mard~werks xchar mtart matkl meins mara~lvorm marc~lvorm maktx.
      ENDIF.                                                "^1904998

      SELECT SINGLE mandt FROM mstb CONNECTION (dbcon) INTO lv_exists.
      IF sy-subrc IS INITIAL. "at least one entry found: continue
*     select additionally valuated SiT & GR blocked stock details
        CLEAR collector_mstb.

*     only select stocks at plant level if a storage location has
*     not been entered as a selection option, the entered special
*     stock indicator should not be used for MSTB
        IF g_flag_suppress_init_lgort IS INITIAL.           "2181243
          IF xmchb = 'X'.
*        batch & bwtar details
            SELECT mstb~matnr mstb~werks charg sobkz bwtar
                  SUM( cwesb ) AS bwesb
                  CONNECTION (dbcon)
                  APPENDING CORRESPONDING FIELDS OF TABLE collector_mstb
                  FROM mstb
                  INNER JOIN v_marc_md AS marc              "2561903
                   ON mstb~matnr = marc~matnr AND
                      mstb~werks = marc~werks
                  INNER JOIN mara
                   ON marc~matnr = mara~matnr
                  WHERE mstb~matnr IN matnr
                    AND mstb~werks IN werks
                    AND mstb~charg IN charg
*                 AND mstb~sobkz IN so_sobkz                    "2181243
                    AND mstb~cwesb IN r_stopt
                    AND marc~ekgrp IN ekgrup
                    AND mara~matkl IN matkla
                    AND mara~mtart IN matart
                    AND (lv_dynwhere)                       "1917052
                 GROUP BY mstb~matnr mstb~werks mstb~charg mstb~sobkz mstb~bwtar.
          ELSE.
*      with batch/bwtar in total
            SELECT mstb~matnr mstb~werks
                  SUM( cwesb ) AS bwesb
                  CONNECTION (dbcon)
                  APPENDING CORRESPONDING FIELDS OF TABLE collector_mstb
                  FROM mstb
                  INNER JOIN v_marc_md AS marc              "2561903
                   ON mstb~matnr = marc~matnr AND
                      mstb~werks = marc~werks
                  INNER JOIN mara
                   ON marc~matnr = mara~matnr
                  WHERE mstb~matnr IN matnr
                    AND mstb~werks IN werks
                    AND mstb~charg IN charg
*                 AND mstb~sobkz IN so_sobkz                    "2181243
                    AND mstb~cwesb IN r_stopt
                    AND marc~ekgrp IN ekgrup
                    AND mara~matkl IN matkla
                    AND mara~mtart IN matart
                    AND (lv_dynwhere)                       "1917052
                 GROUP BY mstb~matnr mstb~werks.
          ENDIF.
        ENDIF.                                              "2181243
      ENDIF.

************************************************************************
* Get "normal" stocks.
* If no detailed batch display is required,
* all data come from MARD. Otherwise, materials with batch
* management are extracted from MCHB, the rest from MARD.
************************************************************************

* Access MARD
      CLEAR collector_mard.

      FIELD-SYMBOLS:
          <collector_mard> LIKE LINE OF collector.

*     NO BATCH
      READ TABLE t_mat TRANSPORTING NO FIELDS WITH KEY xchar = space." BINARY SEARCH.
      IF sy-subrc = 0.
        SELECT mard~matnr mard~werks lgort
            SUM( mard~labst ) AS labst SUM( umlme ) AS umlme SUM( insme ) AS insme
            SUM( einme ) AS einme SUM( speme ) AS speme SUM( retme ) AS retme
            mard~lvorm
            CONNECTION (dbcon)
            INTO CORRESPONDING FIELDS OF
            TABLE collector_mard
            FROM mard JOIN v_marc_md AS marc                "2561903
            ON mard~werks = marc~werks AND
               mard~matnr = marc~matnr
            JOIN mara ON mara~matnr = marc~matnr
            WHERE mard~matnr IN matnr
              AND mard~werks IN werks
              AND lgort IN lgort
              AND xchar EQ space
              AND ( labst IN r_stopt
               OR   umlme IN r_stopt
               OR   insme IN r_stopt
               OR   einme IN r_stopt
               OR   speme IN r_stopt
               OR   retme IN r_stopt )
           AND marc~ekgrp IN ekgrup
           AND mara~matkl IN matkla
           AND mara~mtart IN matart
           AND (lv_dynwhere)                                "1917052
            GROUP BY mard~matnr mard~werks mard~lgort mard~lvorm.

        LOOP AT collector_mard ASSIGNING <collector_mard>.
          IF NOT pa_sond IS INITIAL                AND
                 NOT <collector_mard>-lvorm IS INITIAL.
            MOVE-CORRESPONDING <collector_mard> TO g_s_mard_lv.
            INSERT g_s_mard_lv INTO TABLE g_t_mard_lv.
          ENDIF.
        ENDLOOP.
      ENDIF.

      IF collector_mard[] IS NOT INITIAL.                   "1895376
        lv_xmbew = 'X'.                                     "1895376
      ENDIF.                                                "1895376
      APPEND LINES OF collector_mard[] TO collector[].
      FREE collector_mard.

* Access MCHB
      CLEAR collector_mchb.
*    BATCH
      READ TABLE t_mat TRANSPORTING NO FIELDS WITH KEY xchar = 'X'." BINARY SEARCH.
      IF sy-subrc = 0.
        IF xmchb = 'X'.
ENHANCEMENT-SECTION rm07mlbs SPOTS es_rm07mlbs .
          IF cl_ops_switch_check=>sfsw_segmentation( ) EQ abap_on.
            SELECT mchb~matnr mchb~werks lgort mchb~charg
                SUM( clabs ) AS labst SUM( cumlm ) AS umlme SUM( cinsm ) AS insme
                SUM( ceinm ) AS einme SUM( cspem ) AS speme SUM( cretm ) AS retme
                mchb~lvorm mcha~bwtar mchb~sgt_scat
                CONNECTION (dbcon)
                APPENDING CORRESPONDING FIELDS OF TABLE collector_mchb
                FROM mchb INNER JOIN v_marc_md AS marc      "2561903
                ON mchb~matnr = marc~matnr AND
                   mchb~werks = marc~werks
                INNER JOIN mara
                ON mara~matnr = mchb~matnr
                LEFT JOIN mcha
                ON mcha~matnr = mchb~matnr AND
                   mcha~werks = mchb~werks AND
                   mcha~charg = mchb~charg
                WHERE mchb~matnr IN matnr
                  AND mchb~werks IN werks
                  AND lgort IN lgort
                  AND mchb~charg IN charg
                  AND xchar = 'X'
                  AND ( clabs IN r_stopt
                   OR   cumlm IN r_stopt
                   OR   cinsm IN r_stopt
                   OR   ceinm IN r_stopt
                   OR   cspem IN r_stopt
                   OR   cretm IN r_stopt )
               AND marc~ekgrp IN ekgrup
               AND mara~matkl IN matkla
               AND mara~mtart IN matart
               AND (lv_dynwhere)                            "1917052
               GROUP BY mchb~matnr mchb~werks mchb~lgort mchb~charg mchb~sgt_scat
                        mchb~lvorm mcha~bwtar.
          ELSE.
            SELECT mchb~matnr mchb~werks lgort mchb~charg
                SUM( clabs ) AS labst SUM( cumlm ) AS umlme SUM( cinsm ) AS insme
                SUM( ceinm ) AS einme SUM( cspem ) AS speme SUM( cretm ) AS retme
                mchb~lvorm mcha~bwtar
                CONNECTION (dbcon)
                APPENDING CORRESPONDING FIELDS OF TABLE collector_mchb
                FROM mchb INNER JOIN v_marc_md AS marc      "2561903
                ON mchb~matnr = marc~matnr AND
                   mchb~werks = marc~werks
                INNER JOIN mara
                ON mara~matnr = mchb~matnr
                LEFT JOIN mcha
                ON mcha~matnr = mchb~matnr AND
                   mcha~werks = mchb~werks AND
                   mcha~charg = mchb~charg
                WHERE mchb~matnr IN matnr
                  AND mchb~werks IN werks
                  AND lgort IN lgort
                  AND mchb~charg IN charg
                  AND xchar = 'X'
                  AND ( clabs IN r_stopt
                   OR   cumlm IN r_stopt
                   OR   cinsm IN r_stopt
                   OR   ceinm IN r_stopt
                   OR   cspem IN r_stopt
                   OR   cretm IN r_stopt )
               AND marc~ekgrp IN ekgrup
               AND mara~matkl IN matkla
               AND mara~mtart IN matart
               AND (lv_dynwhere)                            "1917052
               GROUP BY mchb~matnr mchb~werks mchb~lgort mchb~charg mchb~lvorm mcha~bwtar.
          ENDIF.
END-ENHANCEMENT-SECTION.
        ELSE.
          "sums without batch from MCHB
*          replaced with select from mard to select zero batch stocks            "v2283091
*          SELECT mchb~matnr mchb~werks mchb~lgort
*             SUM( clabs ) AS labst SUM( cumlm ) AS umlme SUM( cinsm ) AS insme
*             SUM( ceinm ) AS einme SUM( cspem ) AS speme SUM( cretm ) AS retme
*             mard~lvorm                                                         "2157991
*             CONNECTION (dbcon)
*             APPENDING CORRESPONDING FIELDS OF TABLE collector_mchb
*             FROM mchb INNER JOIN marc
*             ON mchb~matnr = marc~matnr AND
*                mchb~werks = marc~werks
*             INNER JOIN mara
*             ON mara~matnr = mchb~matnr
*             LEFT JOIN mard
*             ON mard~matnr = mchb~matnr AND                                     "2157991
*                mard~werks = mchb~werks AND                                     "2157991
*                mard~lgort = mchb~lgort                                         "2157991
*             WHERE mchb~matnr IN matnr
*               AND mchb~werks IN werks
*               AND mchb~lgort IN lgort                                          "2157991
*               AND charg IN charg
*               AND xchar = 'X'
*               AND ( clabs IN r_stopt
*                OR   cumlm IN r_stopt
*                OR   cinsm IN r_stopt
*                OR   ceinm IN r_stopt
*                OR   cspem IN r_stopt
*                OR   cretm IN r_stopt )
*            AND marc~ekgrp IN ekgrup
*            AND mara~matkl IN matkla
*            AND mara~mtart IN matart
*            AND (lv_dynwhere)                                                   "1917052
*            GROUP BY mchb~matnr mchb~werks mchb~lgort mard~lvorm.               "2157991
          SELECT mard~matnr mard~werks lgort                "^v2283091
              SUM( mard~labst ) AS labst SUM( umlme ) AS umlme SUM( insme ) AS insme
              SUM( einme ) AS einme SUM( speme ) AS speme SUM( retme ) AS retme
              mard~lvorm
              CONNECTION (dbcon)
              INTO CORRESPONDING FIELDS OF
              TABLE collector_mchb
              FROM mard JOIN v_marc_md AS marc              "2561903
              ON mard~werks = marc~werks AND
                 mard~matnr = marc~matnr
              JOIN mara ON mara~matnr = marc~matnr
              WHERE mard~matnr IN matnr
                AND mard~werks IN werks
                AND lgort IN lgort
                AND xchar EQ abap_true
                AND ( labst IN r_stopt
                 OR   umlme IN r_stopt
                 OR   insme IN r_stopt
                 OR   einme IN r_stopt
                 OR   speme IN r_stopt
                 OR   retme IN r_stopt )
              AND marc~ekgrp IN ekgrup
              AND mara~matkl IN matkla
              AND mara~mtart IN matart
             AND (lv_dynwhere)
              GROUP BY mard~matnr mard~werks mard~lgort mard~lvorm. "^2283091
        ENDIF.
      ENDIF.

      IF collector_mchb[] IS NOT INITIAL.                   "1895376
        lv_xmbew = 'X'.                                     "1895376
      ENDIF.                                                "1895376
      APPEND LINES OF collector_mchb[] TO collector[].
      FREE collector_mchb.


************************************************************************
* Transfer stocks from MARC (TRAME, UMLMC)
************************************************************************
      CLEAR collector.

      LOOP AT t_mat WHERE umlmc <> 0 OR trame <> 0 OR
                          bwesb <> 0 OR
                          glgmg <> 0.

*     there are no lines with stock = zero

*     take the stocks from plant level only when the user
*     does not restrict the storage location;
        CHECK : g_flag_suppress_init_lgort IS INITIAL.

        IF negativ = 'X'.
*       ignore entry if all stocks are zero or greater
          IF  t_mat-trame >= 0 AND
              t_mat-umlmc >= 0 AND
              t_mat-glgmg >= 0.
            CONTINUE.          "take the next entry
          ENDIF.
        ENDIF.

        collector-matnr = t_mat-matnr.
        collector-werks = t_mat-werks.
        collector-umlme = t_mat-trame + t_mat-umlmc.
        collector-lvorm = t_mat-lvorm_marc.
        collector-bwesb = t_mat-bwesb.
        collector-glgmg = t_mat-glgmg.
        collector-trame = t_mat-trame.
        collector-umlmc = t_mat-umlmc.

        APPEND collector TO collector_uml.

      ENDLOOP.

************************************************************************
* merge valuated stock in transit from marc/mstb to avoid 2 lines for
* plant stocks
************************************************************************

      IF xmchb = 'X'.
*   delete bwesb from uml as we have more details
*   copy table and delete doubles (batches, bwtar)
        collector_mstb_agr[] = collector_mstb[].
        SORT collector_mstb_agr BY matnr werks.
        DELETE ADJACENT DUPLICATES FROM collector_mstb_agr COMPARING matnr werks.

        LOOP AT collector_mstb_agr ASSIGNING <fs_col_mstb>.
          READ TABLE collector_uml ASSIGNING <fs_col_uml>
                                   WITH KEY matnr = <fs_col_mstb>-matnr
                                            werks = <fs_col_mstb>-werks.
          IF sy-subrc = 0.
            CLEAR <fs_col_uml>-bwesb.
          ENDIF.
        ENDLOOP.
      ELSE.
*    overwrite bwesb in uml with sum of singles
        LOOP AT collector_mstb ASSIGNING <fs_col_mstb>.
          READ TABLE collector_uml ASSIGNING <fs_col_uml>
                                   WITH KEY matnr = <fs_col_mstb>-matnr
                                            werks = <fs_col_mstb>-werks.
          IF sy-subrc = 0.
            <fs_col_uml>-bwesb = <fs_col_mstb>-bwesb.
            DELETE TABLE collector_mstb FROM <fs_col_mstb>.
          ENDIF.
        ENDLOOP.
      ENDIF.

      IF collector_mstb[] IS NOT INITIAL.                   "1895376
        lv_xmbew = 'X'.                                     "1895376
      ENDIF.                                                "1895376
      APPEND LINES OF collector_mstb[] TO collector[].
      FREE collector_mstb.
      IF collector_uml[] IS NOT INITIAL.                    "1895376
        lv_xmbew = 'X'.                                     "1895376
      ENDIF.                                                "1895376
      APPEND LINES OF collector_uml[] TO collector[].
      FREE collector_uml.

************************************************************************
* Consignment from vendor (MKOL)
* Read only if requested by one of the
* flags on the selection screen. Absolutely inconsistent, but
* due to compatibility...
* MKOL has a flag for deletion
************************************************************************

      IF  NOT pa_sond IS INITIAL.
*          AND NOT t_mat[] IS INITIAL.                            "2056114
        IF  'K' IN so_sobkz  OR
            'M' IN so_sobkz.

          SELECT SINGLE mandt FROM mkol CONNECTION (dbcon) INTO lv_exists.
          IF sy-subrc IS INITIAL. "at least one entry found: continue

            CLEAR collector_mkol.
            IF xmchb IS NOT INITIAL.
*           sum with batch
              SELECT mkol~matnr mkol~werks lgort mkol~charg sobkz mkol~lifnr
                     SUM( slabs ) AS labst SUM( sinsm ) AS insme
                     SUM( seinm ) AS einme SUM( sspem ) AS speme
                     mkol~lvorm bwtar
                   CONNECTION (dbcon)
                     INTO CORRESPONDING FIELDS OF TABLE collector_mkol
                     FROM mkol
                     JOIN mara
                     ON mara~matnr = mkol~matnr
                     JOIN v_marc_md AS marc                 "2561903
                     ON marc~matnr = mkol~matnr
                    AND marc~werks = mkol~werks
                     LEFT JOIN mcha
                     ON mcha~matnr = mkol~matnr AND
                        mcha~werks = mkol~werks AND
                        mcha~charg = mkol~charg
                     WHERE mkol~matnr IN matnr
                       AND mkol~werks IN werks
                       AND lgort IN lgort
                       AND mkol~charg IN charg
                       AND sobkz IN so_sobkz
                       AND ( slabs IN r_stopt
                        OR   sinsm IN r_stopt
                        OR   seinm IN r_stopt
                        OR   sspem IN r_stopt )
                    AND marc~ekgrp IN ekgrup
                    AND mara~matkl IN matkla
                    AND mara~mtart IN matart
                    AND (lv_dynwhere)                       "1917052
                     GROUP BY mkol~matnr mkol~werks lgort mkol~charg sobkz mkol~lifnr mkol~lvorm bwtar.
            ELSE.
*           sum without batch
              SELECT mkol~matnr mkol~werks lgort sobkz lifnr
                     SUM( slabs ) AS labst SUM( sinsm ) AS insme
                     SUM( seinm ) AS einme SUM( sspem ) AS speme
                     mkol~lvorm
                   CONNECTION (dbcon)
                     INTO CORRESPONDING FIELDS OF TABLE collector_mkol
                     FROM mkol
                     JOIN mara
                     ON mara~matnr = mkol~matnr
                     JOIN v_marc_md AS marc                 "2561903
                     ON marc~matnr = mkol~matnr
                    AND marc~werks = mkol~werks
                     WHERE mkol~matnr IN matnr
                       AND mkol~werks IN werks
                       AND mkol~lgort IN lgort
                       AND sobkz IN so_sobkz
                       AND ( slabs IN r_stopt
                        OR   sinsm IN r_stopt
                        OR   seinm IN r_stopt
                        OR   sspem IN r_stopt )
                    AND marc~ekgrp IN ekgrup
                    AND mara~matkl IN matkla
                    AND mara~mtart IN matart
                    AND (lv_dynwhere)                       "1917052
                    GROUP BY mkol~matnr mkol~werks lgort sobkz lifnr mkol~lvorm.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.

      APPEND LINES OF collector_mkol[] TO collector[].
      FREE collector_mkol.

************************************************************************
* Special stocks at customer side (MSKU)
* MSKU has no flag for deletion
************************************************************************

      IF  NOT  pa_sond IS INITIAL.
*          AND NOT  t_mat[] IS INITIAL.                                     "2056114
        IF  'V' IN so_sobkz  OR
            'W' IN so_sobkz.

          SELECT SINGLE mandt FROM msku CONNECTION (dbcon) INTO lv_exists.
          IF sy-subrc IS INITIAL "at least one entry found: continue
             AND g_flag_suppress_init_lgort IS INITIAL.     "2056114

            CLEAR collector_msku.
ENHANCEMENT-SECTION rm07mlbs_g1 SPOTS es_rm07mlbs .
            IF xmchb IS NOT INITIAL.
              IF cl_ops_switch_check=>sfsw_segmentation( ) EQ abap_on.
*               sum with batch
                SELECT msku~matnr msku~werks msku~charg msku~sobkz msku~kunnr
                       SUM( kulab ) AS labst SUM( kuins ) AS insme
                       SUM( kuein ) AS einme SUM( kuuml ) AS umlme
                       bwtar msku~sgt_scat
                     CONNECTION (dbcon)
                       INTO CORRESPONDING FIELDS OF TABLE collector_msku
                       FROM msku
                       JOIN mara
                       ON mara~matnr = msku~matnr
                       JOIN v_marc_md AS marc               "2561903
                       ON marc~matnr = msku~matnr
                      AND marc~werks = msku~werks
                       LEFT JOIN mcha
                       ON mcha~matnr = msku~matnr AND
                          mcha~werks = msku~werks AND
                          mcha~charg = msku~charg
                       WHERE msku~matnr IN matnr
                         AND msku~werks IN werks
                         AND msku~charg IN charg
                         AND msku~sobkz IN so_sobkz
                         AND ( kulab IN r_stopt
                          OR   kuins IN r_stopt
                          OR   kuein IN r_stopt
                          OR   kuuml IN r_stopt )
                      AND marc~ekgrp IN ekgrup
                      AND mara~matkl IN matkla
                      AND mara~mtart IN matart
                      AND (lv_dynwhere)                     "1917052
                       GROUP BY msku~matnr msku~werks msku~charg msku~sgt_scat
                               msku~sobkz msku~kunnr bwtar.
              ELSE.
*               sum with batch
                SELECT msku~matnr msku~werks msku~charg msku~sobkz msku~kunnr
                       SUM( kulab ) AS labst SUM( kuins ) AS insme
                       SUM( kuein ) AS einme SUM( kuuml ) AS umlme
                       bwtar
                     CONNECTION (dbcon)
                       INTO CORRESPONDING FIELDS OF TABLE collector_msku
                       FROM msku
                       JOIN mara
                       ON mara~matnr = msku~matnr
                       JOIN v_marc_md AS marc               "2561903
                       ON marc~matnr = msku~matnr
                      AND marc~werks = msku~werks
                       LEFT JOIN mcha
                       ON mcha~matnr = msku~matnr AND
                          mcha~werks = msku~werks AND
                          mcha~charg = msku~charg
                       WHERE msku~matnr IN matnr
                         AND msku~werks IN werks
                         AND msku~charg IN charg
                         AND msku~sobkz IN so_sobkz
                         AND ( kulab IN r_stopt
                          OR   kuins IN r_stopt
                          OR   kuein IN r_stopt
                          OR   kuuml IN r_stopt )
                      AND marc~ekgrp IN ekgrup
                      AND mara~matkl IN matkla
                      AND mara~mtart IN matart
                      AND (lv_dynwhere)                     "1917052
                       GROUP BY msku~matnr msku~werks msku~charg msku~sobkz msku~kunnr bwtar.
              ENDIF.
            ELSE.
              IF cl_ops_switch_check=>sfsw_segmentation( ) EQ abap_on.
*             sum without batch
                SELECT msku~matnr msku~werks sobkz msku~kunnr
                       SUM( kulab ) AS labst SUM( kuins ) AS insme
                       SUM( kuein ) AS einme SUM( kuuml ) AS umlme
                       msku~sgt_scat
                     CONNECTION (dbcon)
                       INTO CORRESPONDING FIELDS OF TABLE collector_msku
                       FROM msku
                       JOIN mara
                       ON mara~matnr = msku~matnr
                       JOIN v_marc_md AS marc               "2561903
                       ON marc~matnr = msku~matnr
                      AND marc~werks = msku~werks
                       WHERE msku~matnr IN matnr
                         AND msku~werks IN werks
                         AND msku~sobkz IN so_sobkz
                         AND ( kulab IN r_stopt
                          OR   kuins IN r_stopt
                          OR   kuein IN r_stopt
                          OR   kuuml IN r_stopt )
                      AND marc~ekgrp IN ekgrup
                      AND mara~matkl IN matkla
                      AND mara~mtart IN matart
                      AND (lv_dynwhere)                     "1917052
                       GROUP BY msku~matnr msku~werks sobkz msku~kunnr msku~sgt_scat.
              ELSE.
*             sum without batch
                SELECT msku~matnr msku~werks sobkz msku~kunnr
                       SUM( kulab ) AS labst SUM( kuins ) AS insme
                       SUM( kuein ) AS einme SUM( kuuml ) AS umlme
                     CONNECTION (dbcon)
                       INTO CORRESPONDING FIELDS OF TABLE collector_msku
                       FROM msku
                       JOIN mara
                       ON mara~matnr = msku~matnr
                       JOIN v_marc_md AS marc               "2561903
                       ON marc~matnr = msku~matnr
                      AND marc~werks = msku~werks
                       WHERE msku~matnr IN matnr
                         AND msku~werks IN werks
                         AND msku~sobkz IN so_sobkz
                         AND ( kulab IN r_stopt
                          OR   kuins IN r_stopt
                          OR   kuein IN r_stopt
                          OR   kuuml IN r_stopt )
                      AND marc~ekgrp IN ekgrup
                      AND mara~matkl IN matkla
                      AND mara~mtart IN matart
                      AND (lv_dynwhere)                     "1917052
                       GROUP BY msku~matnr msku~werks sobkz msku~kunnr.
              ENDIF.
            ENDIF.
END-ENHANCEMENT-SECTION.
          ENDIF.
        ENDIF.
      ENDIF.

      IF collector_msku[] IS NOT INITIAL.                   "1895376
        lv_xmbew = 'X'.                                     "1895376
      ENDIF.                                                "1895376
      APPEND LINES OF collector_msku[] TO collector[].
      FREE collector_msku.

************************************************************************
* Special stocks at vendor provision (MSLB)
* MSLB has no flag for deletion
************************************************************************

      IF  NOT  pa_sond IS INITIAL  AND
*          NOT  t_mat[] IS INITIAL  AND                         "2056114
               'O'     IN so_sobkz.

        SELECT SINGLE mandt FROM mslb CONNECTION (dbcon) INTO lv_exists.
        IF sy-subrc IS INITIAL. "at least one entry found: continue
          CLEAR collector_mslb.

          IF g_flag_suppress_init_lgort IS INITIAL.
            IF xmchb IS NOT INITIAL.
              SELECT mslb~matnr mslb~werks mslb~charg mslb~sobkz mslb~lifnr
                 SUM( lblab ) AS labst SUM( lbins ) AS insme
                 SUM( lbein ) AS einme SUM( lbuml ) AS umlme
                 bwtar
               CONNECTION (dbcon)
                 INTO CORRESPONDING FIELDS OF TABLE collector_mslb
                 FROM mslb
                 JOIN mara
                 ON mara~matnr = mslb~matnr
                 JOIN v_marc_md AS marc                     "2561903
                 ON marc~matnr = mslb~matnr
                AND marc~werks = mslb~werks
                 LEFT JOIN mcha
                   ON mcha~matnr = mslb~matnr AND
                      mcha~werks = mslb~werks AND
                      mcha~charg = mslb~charg
                 WHERE mslb~matnr IN matnr
                   AND mslb~werks IN werks
                   AND mslb~charg IN charg
                   AND mslb~sobkz IN so_sobkz
                   AND ( lblab IN r_stopt
                    OR   lbins IN r_stopt
                    OR   lbein IN r_stopt
                    OR   lbuml IN r_stopt )
                    AND marc~ekgrp IN ekgrup
                    AND mara~matkl IN matkla
                    AND mara~mtart IN matart
                    AND (lv_dynwhere)                       "1917052
                 GROUP BY mslb~matnr mslb~werks mslb~charg mslb~sobkz mslb~lifnr bwtar.

            ELSE.
*           sum without batch
              SELECT mslb~matnr mslb~werks sobkz mslb~lifnr
                 SUM( lblab ) AS labst SUM( lbins ) AS insme
                 SUM( lbein ) AS einme SUM( lbuml ) AS umlme
               CONNECTION (dbcon)
                 APPENDING CORRESPONDING FIELDS OF TABLE collector_mslb
                 FROM mslb
                 JOIN mara
                 ON mara~matnr = mslb~matnr
                 JOIN v_marc_md AS marc                     "2561903
                 ON marc~matnr = mslb~matnr
                AND marc~werks = mslb~werks
                 WHERE mslb~matnr IN matnr
                   AND mslb~werks IN werks
                   AND mslb~sobkz IN so_sobkz
                   AND ( lblab IN r_stopt
                    OR   lbins IN r_stopt
                    OR   lbein IN r_stopt
                    OR   lbuml IN r_stopt )
                    AND marc~ekgrp IN ekgrup
                    AND mara~matkl IN matkla
                    AND mara~mtart IN matart
                    AND (lv_dynwhere)                       "1917052
                 GROUP BY mslb~matnr mslb~werks mslb~sobkz mslb~lifnr.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.

      IF collector_mslb[] IS NOT INITIAL.                   "1895376
        lv_xmbew = 'X'.                                     "1895376
      ENDIF.                                                "1895376
      APPEND LINES OF collector_mslb[] TO collector[].
      FREE collector_mslb.

************************************************************************
* Customer order stock (MSKA) and sum segment (MSSA) for valuation.
* Sum on the database and FOR ALL ENTRIES is not allowed from
* release 4.5 onwards, so the summation has to be done
* on the application server (here!).
* MSKA has no flag for deletion
************************************************************************

      IF  NOT  pa_sond IS INITIAL  AND
*          NOT  t_mat[] IS INITIAL  AND                         "2056114
               ( 'E' IN so_sobkz OR 'T' IN so_sobkz ).

        SELECT SINGLE mandt FROM mssa CONNECTION (dbcon) INTO lv_exists.
        IF sy-subrc IS INITIAL. "at least one entry found: continue

          CLEAR collector_mska.

ENHANCEMENT-SECTION rm07mlbs_g2 SPOTS es_rm07mlbs .
          IF xmchb IS NOT INITIAL.
            IF cl_ops_switch_check=>sfsw_segmentation( ) EQ abap_on.
*             sum of all materials w/ and w/o batch
              SELECT mska~matnr mska~werks lgort mska~charg mska~sobkz
                   mska~vbeln mska~posnr kzbws
                   SUM( kalab ) AS labst SUM( kains ) AS insme
                   SUM( kaspe ) AS speme SUM( kaein ) AS einme
                   bwtar mska~sgt_scat
               CONNECTION (dbcon)
               INTO CORRESPONDING FIELDS OF TABLE collector_mska
                   FROM mska INNER JOIN mssa
                   ON   mska~matnr = mssa~matnr
                    AND mska~werks = mssa~werks
                    AND mska~sobkz = mssa~sobkz
                    AND mska~vbeln = mssa~vbeln
                    AND mska~posnr = mssa~posnr
                   JOIN v_marc_md AS marc                   "2561903
                     ON marc~werks = mssa~werks AND
                        marc~matnr = mssa~matnr
                   JOIN mara
                     ON mara~matnr = mssa~matnr
                   LEFT JOIN mcha
                     ON mcha~matnr = mska~matnr AND
                        mcha~werks = mska~werks AND
                        mcha~charg = mska~charg
                   WHERE mska~matnr IN matnr
                     AND mska~werks IN werks
                     AND mska~lgort IN lgort
                     AND mska~sobkz IN so_sobkz
                     AND mska~charg IN charg
                     AND ( mska~kalab IN r_stopt
                      OR   mska~kains IN r_stopt
                      OR   mska~kaspe IN r_stopt
                      OR   mska~kaein IN r_stopt )
                      AND marc~ekgrp IN ekgrup
                      AND mara~matkl IN matkla
                      AND mara~mtart IN matart
                      AND (lv_dynwhere)                     "1917052
                   GROUP BY mska~matnr mska~werks mska~lgort
                            mska~sobkz mska~vbeln mska~posnr
                            kzbws mska~charg mska~sgt_scat bwtar.
            ELSE.
*             sum of all materials w/ and w/o batch
              SELECT mska~matnr mska~werks lgort mska~charg mska~sobkz
                   mska~vbeln mska~posnr kzbws
                   SUM( kalab ) AS labst SUM( kains ) AS insme
                   SUM( kaspe ) AS speme SUM( kaein ) AS einme
                   bwtar
               CONNECTION (dbcon)
               INTO CORRESPONDING FIELDS OF TABLE collector_mska
                   FROM mska INNER JOIN mssa
                   ON   mska~matnr = mssa~matnr
                    AND mska~werks = mssa~werks
                    AND mska~sobkz = mssa~sobkz
                    AND mska~vbeln = mssa~vbeln
                    AND mska~posnr = mssa~posnr
                   JOIN v_marc_md AS marc                   "2561903
                     ON marc~werks = mssa~werks AND
                        marc~matnr = mssa~matnr
                   JOIN mara
                     ON mara~matnr = mssa~matnr
                   LEFT JOIN mcha
                     ON mcha~matnr = mska~matnr AND
                        mcha~werks = mska~werks AND
                        mcha~charg = mska~charg
                   WHERE mska~matnr IN matnr
                     AND mska~werks IN werks
                     AND mska~lgort IN lgort
                     AND mska~sobkz IN so_sobkz
                     AND mska~charg IN charg
                     AND ( mska~kalab IN r_stopt
                      OR   mska~kains IN r_stopt
                      OR   mska~kaspe IN r_stopt
                      OR   mska~kaein IN r_stopt )
                      AND marc~ekgrp IN ekgrup
                      AND mara~matkl IN matkla
                      AND mara~mtart IN matart
                      AND (lv_dynwhere)                     "1917052
                   GROUP BY mska~matnr mska~werks mska~lgort
                            mska~sobkz mska~vbeln mska~posnr
                            kzbws mska~charg bwtar.
            ENDIF.
          ELSE.
            IF cl_ops_switch_check=>sfsw_segmentation( ) EQ abap_on.
*           sum without batch
              SELECT mska~matnr mska~werks lgort mska~sobkz
                    mska~vbeln mska~posnr kzbws  mska~sgt_scat
                    SUM( kalab ) AS labst SUM( kains ) AS insme
                    SUM( kaspe ) AS speme SUM( kaein ) AS einme
                CONNECTION (dbcon)
                APPENDING CORRESPONDING FIELDS OF TABLE collector_mska
                    FROM mska INNER JOIN mssa
                    ON   mska~matnr = mssa~matnr
                     AND mska~werks = mssa~werks
                     AND mska~sobkz = mssa~sobkz
                     AND mska~vbeln = mssa~vbeln
                     AND mska~posnr = mssa~posnr
                   JOIN v_marc_md AS marc                   "2561903
                     ON marc~werks = mssa~werks AND
                        marc~matnr = mssa~matnr
                   JOIN mara
                     ON mara~matnr = mssa~matnr
                    WHERE mska~matnr IN matnr
                      AND mska~werks IN werks
                      AND mska~lgort IN lgort
                      AND mska~sobkz IN so_sobkz
                      AND ( mska~kalab IN r_stopt
                       OR   mska~kains IN r_stopt
                       OR   mska~kaspe IN r_stopt
                       OR   mska~kaein IN r_stopt )
                      AND marc~ekgrp IN ekgrup
                      AND mara~matkl IN matkla
                      AND mara~mtart IN matart
                      AND (lv_dynwhere)                     "1917052
                    GROUP BY mska~matnr mska~werks mska~lgort
                             mska~sobkz mska~vbeln mska~posnr
                             kzbws      mska~sgt_scat.
            ELSE.
*           sum without batch
              SELECT mska~matnr mska~werks lgort mska~sobkz
                    mska~vbeln mska~posnr kzbws
                    SUM( kalab ) AS labst SUM( kains ) AS insme
                    SUM( kaspe ) AS speme SUM( kaein ) AS einme
                CONNECTION (dbcon)
                APPENDING CORRESPONDING FIELDS OF TABLE collector_mska
                    FROM mska INNER JOIN mssa
                    ON   mska~matnr = mssa~matnr
                     AND mska~werks = mssa~werks
                     AND mska~sobkz = mssa~sobkz
                     AND mska~vbeln = mssa~vbeln
                     AND mska~posnr = mssa~posnr
                   JOIN v_marc_md AS marc                   "2561903
                     ON marc~werks = mssa~werks AND
                        marc~matnr = mssa~matnr
                   JOIN mara
                     ON mara~matnr = mssa~matnr
                    WHERE mska~matnr IN matnr
                      AND mska~werks IN werks
                      AND mska~lgort IN lgort
                      AND mska~sobkz IN so_sobkz
                      AND ( mska~kalab IN r_stopt
                       OR   mska~kains IN r_stopt
                       OR   mska~kaspe IN r_stopt
                       OR   mska~kaein IN r_stopt )
                      AND marc~ekgrp IN ekgrup
                      AND mara~matkl IN matkla
                      AND mara~mtart IN matart
                      AND (lv_dynwhere)                     "1917052
                    GROUP BY mska~matnr mska~werks mska~lgort
                             mska~sobkz mska~vbeln mska~posnr
                             kzbws.
            ENDIF.
          ENDIF.
END-ENHANCEMENT-SECTION.
*         Transfer stocks for customer order (SATRA in MSSA)
          CLEAR collector_mssa.
          CLEAR collector_mste.

*     only select stocks at plant level if a storage location has
*     not been entered as a selection option
          IF g_flag_suppress_init_lgort IS INITIAL.         "2181243
            IF xmchb = 'X'.
              SELECT SINGLE mandt FROM mste CONNECTION (dbcon) INTO lv_exists.
              IF sy-subrc IS INITIAL. "at least one entry found: continue
*          get transfer stock details with charg/ bwtar
                SELECT mste~matnr mste~werks charg mste~sobkz
                       mste~vbeln mste~posnr bwtar kzbws    "1895376
                   SUM( ewesb ) AS bwesb
                   CONNECTION (dbcon)
                   APPENDING CORRESPONDING FIELDS OF TABLE collector_mste
                   FROM mste
                   INNER JOIN v_marc_md AS marc             "2561903
                    ON mste~matnr = marc~matnr AND
                       mste~werks = marc~werks
                   INNER JOIN mara
                    ON marc~matnr = mara~matnr
                   INNER JOIN mssa                          "1895376
                    ON mssa~matnr = mste~matnr AND          "1895376
                       mssa~werks = mste~werks AND          "1895376
                       mssa~sobkz = mste~sobkz AND          "1895376
                       mssa~vbeln = mste~vbeln AND          "1895376
                       mssa~posnr = mste~posnr              "1895376
                   WHERE mste~matnr IN matnr
                     AND mste~werks IN werks
                     AND mste~charg IN charg
                     AND mste~sobkz IN so_sobkz
                     AND mste~ewesb IN r_stopt
                     AND marc~ekgrp IN ekgrup
                     AND mara~matkl IN matkla
                     AND mara~mtart IN matart
                     AND (lv_dynwhere)                      "1917052
                  GROUP BY mste~matnr mste~werks mste~charg mste~sobkz
                           mste~vbeln mste~posnr mste~bwtar kzbws. "1895376
              ENDIF.
*           get transit for batch/split managed materials                          "1895376
              SELECT mssa~matnr mssa~werks sobkz vbeln posnr kzbws
                   SUM( satra ) AS umlme                    "1895376
                 CONNECTION (dbcon)
                   INTO CORRESPONDING FIELDS OF TABLE collector_mssa
                   FROM mssa
                   JOIN v_marc_md AS marc                   "2561903
                   ON   marc~matnr = mssa~matnr
                   AND  marc~werks = mssa~werks
                   JOIN mara
                   ON   mara~matnr = mssa~matnr
                   WHERE mssa~matnr IN matnr
                     AND mssa~werks IN werks
                     AND mssa~sobkz IN so_sobkz
                     AND satra <> 0
                     AND satra IN r_stopt
                     AND marc~ekgrp IN ekgrup
                     AND mara~matkl IN matkla
                     AND mara~mtart IN matart
                     AND (lv_dynwhere)                      "1917052
                     AND (   marc~xchpf = 'X'               "1895376
                          OR marc~xchar = 'X'               "1895376
                          OR marc~bwtty <> space )          "1895376
                   GROUP BY mssa~matnr mssa~werks sobkz vbeln posnr kzbws.
*           get val blocked stock and transit for all other materials              "1895376
              SELECT mssa~matnr mssa~werks sobkz vbeln posnr kzbws
                   SUM( satra ) AS umlme SUM( sabwe ) AS bwesb "1895376
                 CONNECTION (dbcon)
                   APPENDING CORRESPONDING FIELDS OF TABLE collector_mssa
                   FROM mssa
                   JOIN v_marc_md AS marc                   "2561903
                   ON   marc~matnr = mssa~matnr
                   AND  marc~werks = mssa~werks
                   JOIN mara
                   ON   mara~matnr = mssa~matnr
                   WHERE mssa~matnr IN matnr
                     AND mssa~werks IN werks
                     AND mssa~sobkz IN so_sobkz
                     AND ( ( satra <> 0 AND                 "1895376
                             satra IN r_stopt )             "1895376
                        OR ( sabwe <> 0 AND                 "1895376
                             sabwe IN r_stopt ) )           "1895376
                     AND marc~ekgrp IN ekgrup
                     AND mara~matkl IN matkla
                     AND mara~mtart IN matart
                     AND (lv_dynwhere)                      "1917052
                     AND marc~xchpf = space                 "1895376
                     AND marc~xchar = space                 "1895376
                     AND marc~bwtty = space                 "1895376
                   GROUP BY mssa~matnr mssa~werks sobkz vbeln posnr kzbws.
            ELSE.
*        select only sums
              SELECT mssa~matnr mssa~werks sobkz vbeln posnr kzbws
                 SUM( satra ) AS umlme SUM( sabwe ) AS bwesb
               CONNECTION (dbcon)
                 INTO CORRESPONDING FIELDS OF TABLE collector_mssa
                 FROM mssa
                 JOIN v_marc_md AS marc                     "2561903
                 ON   marc~matnr = mssa~matnr
                 AND  marc~werks = mssa~werks
                 JOIN mara
                 ON   mara~matnr = mssa~matnr
                 WHERE mssa~matnr IN matnr
                   AND mssa~werks IN werks
                   AND mssa~sobkz IN so_sobkz
                   AND  ( ( satra <> 0
                    AND     satra IN r_stopt )
                   OR     ( sabwe <> 0
                    AND     sabwe IN r_stopt ) )
                   AND marc~ekgrp IN ekgrup
                   AND mara~matkl IN matkla
                   AND mara~mtart IN matart
                   AND (lv_dynwhere)                        "1917052
                 GROUP BY mssa~matnr mssa~werks sobkz vbeln posnr kzbws.
            ENDIF.
          ENDIF.                                            "2181243
        ENDIF.
      ENDIF.

      IF collector_mska[] IS NOT INITIAL.                   "1895376
        lv_xebew = 'X'.                                     "1895376
      ENDIF.                                                "1895376
      APPEND LINES OF collector_mska[] TO collector[].
      FREE collector_mska.
      IF collector_mssa[] IS NOT INITIAL.                   "1895376
        lv_xebew = 'X'.                                     "1895376
      ENDIF.                                                "1895376
      APPEND LINES OF collector_mssa[] TO collector[].
      FREE collector_mssa.
      IF collector_mste[] IS NOT INITIAL.                   "1895376
        lv_xebew = 'X'.                                     "1895376
      ENDIF.                                                "1895376
      APPEND LINES OF collector_mste[] TO collector[].
      FREE collector_mste.

************************************************************************
* The same game for project stocks (MSPR/MSSQ).
* MSPR has no flag for deletion
************************************************************************

      IF  NOT  pa_sond IS INITIAL  AND
*          NOT  t_mat[] IS INITIAL  AND                         "2056114
               'Q'     IN so_sobkz.

        SELECT SINGLE mandt FROM mssq CONNECTION (dbcon) INTO lv_exists.
        IF sy-subrc IS INITIAL. "at least one entry found: continue
          CLEAR collector_mspr.

          IF xmchb IS NOT INITIAL.
            SELECT mspr~matnr mspr~werks lgort mspr~charg mspr~sobkz mspr~pspnr kzbws
               SUM( prlab ) AS labst  SUM( prins ) AS insme
               SUM( prspe ) AS speme  SUM( prein ) AS einme
               bwtar
             CONNECTION (dbcon)
             INTO CORRESPONDING FIELDS OF TABLE collector_mspr
               FROM mspr INNER JOIN mssq
               ON   mspr~matnr = mssq~matnr
                AND mspr~werks = mssq~werks
                AND mspr~sobkz = mssq~sobkz
                AND mspr~pspnr = mssq~pspnr
                JOIN v_marc_md AS marc                      "2561903
                 ON marc~matnr = mssq~matnr AND
                    marc~werks = mssq~werks
                JOIN mara
                 ON mara~matnr = mssq~matnr
                LEFT JOIN mcha
                 ON mcha~matnr = mspr~matnr AND
                    mcha~werks = mspr~werks AND
                    mcha~charg = mspr~charg
               WHERE mspr~matnr IN matnr
                 AND mspr~werks IN werks
                 AND mspr~lgort IN lgort
                 AND mspr~charg IN charg
                 AND ( prlab IN r_stopt
                  OR   prins IN r_stopt
                  OR   prspe IN r_stopt
                  OR   prein IN r_stopt )
              AND marc~ekgrp IN ekgrup
              AND mara~matkl IN matkla
              AND mara~mtart IN matart
              AND (lv_dynwhere)                             "1917052
              GROUP BY mspr~matnr mspr~werks lgort mspr~charg mspr~sobkz mspr~pspnr kzbws bwtar.
          ELSE.
*     sum without batch
            SELECT mspr~matnr mspr~werks lgort mspr~sobkz mspr~pspnr kzbws
               SUM( prlab ) AS labst  SUM( prins ) AS insme
               SUM( prspe ) AS speme  SUM( prein ) AS einme
             CONNECTION (dbcon)
             APPENDING CORRESPONDING FIELDS OF TABLE collector_mspr
               FROM mspr INNER JOIN mssq
               ON   mspr~matnr = mssq~matnr
                AND mspr~werks = mssq~werks
                AND mspr~sobkz = mssq~sobkz
                AND mspr~pspnr = mssq~pspnr
              JOIN v_marc_md AS marc                        "2561903
               ON   marc~matnr = mssq~matnr AND
                    marc~werks = mssq~werks
              JOIN mara
               ON   mara~matnr = mssq~matnr
               WHERE mspr~matnr IN matnr
                 AND mspr~werks IN werks
                 AND mspr~lgort IN lgort
                 AND ( prlab IN r_stopt
                  OR   prins IN r_stopt
                  OR   prspe IN r_stopt
                  OR   prein IN r_stopt )
              AND marc~ekgrp IN ekgrup
              AND mara~matkl IN matkla
              AND mara~mtart IN matart
              AND (lv_dynwhere)                             "1917052
              GROUP BY mspr~matnr mspr~werks lgort mspr~sobkz mspr~pspnr kzbws.
          ENDIF.

*   Transfer stocks for projects (SQTRA in MSSQ)
          CLEAR collector_mssq.
          CLEAR collector_mstq.

*     only select stocks at plant level if a storage location has
*     not been entered as a selection option
          IF g_flag_suppress_init_lgort IS INITIAL.         "2181243
            IF xmchb = 'X'.
*       get transfer stock details with charg/ bwtar
              SELECT SINGLE mandt FROM mstq CONNECTION (dbcon) INTO lv_exists.
              IF sy-subrc IS INITIAL. "at least one entry found: continue
                SELECT mstq~matnr mstq~werks charg mstq~sobkz mstq~pspnr
                       bwtar kzbws SUM( qwesb ) AS bwesb    "1895376
                   CONNECTION (dbcon)
                   APPENDING CORRESPONDING FIELDS OF TABLE collector_mstq
                   FROM mstq
                   INNER JOIN v_marc_md AS marc             "2561903
                    ON mstq~matnr = marc~matnr AND
                       mstq~werks = marc~werks
                   INNER JOIN mara
                    ON marc~matnr = mara~matnr
                   INNER JOIN mssq                          "1895376
                    ON mssq~matnr = mstq~matnr AND          "1895376
                       mssq~werks = mstq~werks AND          "1895376
                       mssq~sobkz = mstq~sobkz AND          "1895376
                       mssq~pspnr = mstq~pspnr              "1895376
                   WHERE mstq~matnr IN matnr
                     AND mstq~werks IN werks
                     AND mstq~charg IN charg
                     AND mstq~sobkz IN so_sobkz
                     AND mstq~qwesb IN r_stopt
                     AND marc~ekgrp IN ekgrup
                     AND mara~matkl IN matkla
                     AND mara~mtart IN matart
                     AND (lv_dynwhere)                      "1917052
                  GROUP BY mstq~matnr mstq~werks mstq~charg mstq~sobkz
                           mstq~pspnr mstq~bwtar kzbws.     "1895376
              ENDIF.
*          get transit stock for batch/ split managed materials                    "1895376
              SELECT mssq~matnr mssq~werks sobkz pspnr kzbws
                    SUM( sqtra ) AS umlme                   "1895376
                    CONNECTION (dbcon)
                    INTO CORRESPONDING FIELDS OF TABLE collector_mssq
                     FROM mssq
                     JOIN v_marc_md AS marc                 "2561903
                     ON   marc~matnr = mssq~matnr AND
                          marc~werks = mssq~werks
                     JOIN mara
                     ON   mara~matnr = mssq~matnr
                     WHERE mssq~matnr IN matnr
                       AND mssq~werks IN werks
                       AND sobkz  IN  so_sobkz
                       AND sqtra <> 0
                       AND sqtra IN r_stopt
                       AND marc~ekgrp IN ekgrup
                       AND mara~matkl IN matkla
                       AND mara~mtart IN matart
                       AND (lv_dynwhere)                    "1917052
                       AND ( marc~xchpf = 'X'               "1895376
                          OR marc~xchar = 'X'               "1895376
                          OR marc~bwtty <> space )          "1895376
                     GROUP BY mssq~matnr mssq~werks sobkz pspnr kzbws.
*           get val blocked stock and transit for all other materials              "1895376
              SELECT mssq~matnr mssq~werks sobkz pspnr kzbws
                    SUM( sqtra ) AS umlme SUM( sqbwe ) AS bwesb
                    CONNECTION (dbcon)
                    APPENDING CORRESPONDING FIELDS OF TABLE collector_mssq "1895376
                     FROM mssq
                     JOIN v_marc_md AS marc                 "2561903
                     ON   marc~matnr = mssq~matnr AND
                          marc~werks = mssq~werks
                     JOIN mara
                     ON   mara~matnr = mssq~matnr
                     WHERE mssq~matnr IN matnr
                       AND mssq~werks IN werks
                       AND sobkz  IN  so_sobkz
                       AND ( ( sqtra <> 0 AND               "1895376
                               sqtra IN r_stopt )           "1895376
                          OR ( sqbwe <> 0 AND               "1895376
                               sqbwe IN r_stopt ) )         "1895376
                       AND marc~ekgrp IN ekgrup
                       AND mara~matkl IN matkla
                       AND mara~mtart IN matart
                       AND (lv_dynwhere)                    "1917052
                       AND marc~xchpf = space               "1895376
                       AND marc~xchar = space               "1895376
                       AND marc~bwtty = space               "1895376
                     GROUP BY mssq~matnr mssq~werks sobkz pspnr kzbws.
            ELSE.
*     select only sums
              SELECT mssq~matnr mssq~werks sobkz pspnr kzbws
                   SUM( sqtra ) AS umlme SUM( sqbwe ) AS bwesb
                   CONNECTION (dbcon)
                   INTO CORRESPONDING FIELDS OF TABLE collector_mssq
                    FROM mssq
                    JOIN v_marc_md AS marc                  "2561903
                    ON   marc~matnr = mssq~matnr AND
                         marc~werks = mssq~werks
                    JOIN mara
                    ON   mara~matnr = mssq~matnr
                    WHERE mssq~matnr IN matnr
                      AND mssq~werks IN werks
                      AND sobkz  IN  so_sobkz
                      AND  ( ( sqtra <> 0
                       AND     sqtra IN r_stopt )
                      OR     ( sqbwe <> 0
                       AND     sqbwe IN r_stopt ) )
                      AND marc~ekgrp IN ekgrup
                      AND mara~matkl IN matkla
                      AND mara~mtart IN matart
                      AND (lv_dynwhere)                     "1917052
                    GROUP BY mssq~matnr mssq~werks sobkz pspnr kzbws.
            ENDIF.
          ENDIF.                                            "2181243
        ENDIF.
      ENDIF.

      IF collector_mspr[] IS NOT INITIAL.                   "1895376
        lv_xqbew = 'X'.                                     "1895376
      ENDIF.                                                "1895376
      APPEND LINES OF collector_mspr[] TO collector[].
      FREE collector_mspr.
      IF collector_mssq[] IS NOT INITIAL.                   "1895376
        lv_xqbew = 'X'.                                     "1895376
      ENDIF.                                                "1895376
      APPEND LINES OF collector_mssq[] TO collector[].
      FREE collector_mssq.
      IF collector_mstq[] IS NOT INITIAL.                   "1895376
        lv_xqbew = 'X'.                                     "1895376
      ENDIF.                                                "1895376
      APPEND LINES OF collector_mstq[] TO collector[].
      FREE collector_mstq.

      "{ Begin ENHO DIPCS_RM07MLBS001 IS-AD-SSP AD_SUB }
* DI A&D PCS                                                               "v1920470
************************************************************************
* DI IS-ADEC-SSP Customer Stocks(MCSD/MCSS).
* MCSD has no flag for deletion
************************************************************************
      IF  NOT  pa_sond IS INITIAL  AND
          NOT  t_mat[] IS INITIAL  AND
               'B'     IN so_sobkz.

        SELECT SINGLE mandt FROM mcss CONNECTION (dbcon) INTO lv_exists.
        IF sy-subrc IS INITIAL. "at least one entry found: continue

          CLEAR collector_dipcs.

          IF xmchb IS NOT INITIAL.
*       sum of all materials w/ and w/o batch
            SELECT mcsd~matnr mcsd~werks lgort mcsd~charg mcsd~sobkz
                 mcsd~kunnr bwtar kzbws
                 SUM( sdlab ) AS labst SUM( sdins ) AS insme
                 SUM( sdspe ) AS speme SUM( sdein ) AS einme
                 CONNECTION (dbcon)
                 INTO CORRESPONDING FIELDS OF TABLE collector_dipcs
                 FROM mcsd INNER JOIN mcss
                 ON   mcsd~matnr = mcss~matnr
                  AND mcsd~werks = mcss~werks
                  AND mcsd~sobkz = mcss~sobkz
                  AND mcsd~kunnr = mcss~kunnr
                 JOIN v_marc_md AS marc                     "2561903
                   ON marc~werks = mcss~werks AND
                      marc~matnr = mcss~matnr
                 JOIN mara
                   ON mara~matnr = mcss~matnr
                 LEFT JOIN mcha
                   ON mcha~matnr = mcsd~matnr AND
                      mcha~werks = mcsd~werks AND
                      mcha~charg = mcsd~charg
                  WHERE mcsd~matnr IN matnr
                    AND mcsd~werks IN werks
                    AND mcsd~lgort IN lgort
                    AND mcsd~sobkz IN so_sobkz
                    AND mcsd~charg IN charg
                    AND ( mcsd~sdlab IN r_stopt
                     OR   mcsd~sdins IN r_stopt
                     OR   mcsd~sdspe IN r_stopt
                     OR   mcsd~sdein IN r_stopt )
                     AND marc~ekgrp IN ekgrup
                     AND mara~matkl IN matkla
                     AND mara~mtart IN matart
                     AND (lv_dynwhere)
                  GROUP BY mcsd~matnr mcsd~werks mcsd~lgort
                           mcsd~sobkz mcsd~kunnr
                           kzbws mcsd~charg bwtar.
          ELSE.
*       sum without batch
            SELECT mcsd~matnr mcsd~werks lgort mcsd~sobkz
                 mcsd~kunnr kzbws
                 SUM( sdlab ) AS labst SUM( sdins ) AS insme
                 SUM( sdspe ) AS speme SUM( sdein ) AS einme
                 CONNECTION (dbcon)
                 INTO CORRESPONDING FIELDS OF TABLE collector_dipcs
                 FROM mcsd INNER JOIN mcss
                 ON   mcsd~matnr = mcss~matnr
                  AND mcsd~werks = mcss~werks
                  AND mcsd~sobkz = mcss~sobkz
                  AND mcsd~kunnr = mcss~kunnr
                 JOIN v_marc_md AS marc                     "2561903
                   ON marc~werks = mcss~werks AND
                      marc~matnr = mcss~matnr
                 JOIN mara
                   ON mara~matnr = mcss~matnr
                  WHERE mcsd~matnr IN matnr
                    AND mcsd~werks IN werks
                    AND mcsd~lgort IN lgort
                    AND mcsd~sobkz IN so_sobkz
                    AND ( mcsd~sdlab IN r_stopt
                     OR   mcsd~sdins IN r_stopt
                     OR   mcsd~sdspe IN r_stopt
                     OR   mcsd~sdein IN r_stopt )
                     AND marc~ekgrp IN ekgrup
                     AND mara~matkl IN matkla
                     AND mara~mtart IN matart
                     AND (lv_dynwhere)
                  GROUP BY mcsd~matnr mcsd~werks mcsd~lgort
                           mcsd~sobkz mcsd~kunnr
                           kzbws.
          ENDIF.
          IF collector_dipcs[] IS NOT INITIAL.
            lv_xebew = 'X'.
            lv_xmbew = 'X'.
          ENDIF.
          APPEND LINES OF collector_dipcs[] TO collector[].
        ENDIF.
*   Transfer stocks for customer stock (SSTRA in MCSS)
        CLEAR collector_dipcs.
        SELECT mcss~matnr mcss~werks sobkz mcss~kunnr kzbws
            SUM( sstra ) AS umlme
          CONNECTION (dbcon)
            INTO CORRESPONDING FIELDS OF TABLE collector_dipcs
            FROM mcss
            JOIN v_marc_md AS marc                          "2561903
            ON   marc~matnr = mcss~matnr
            AND  marc~werks = mcss~werks
            JOIN mara
            ON   mara~matnr = mcss~matnr
            WHERE mcss~matnr IN matnr
              AND mcss~werks IN werks
              AND mcss~sobkz IN so_sobkz
              AND ( sstra <> 0
               AND  sstra IN r_stopt )
              AND marc~ekgrp IN ekgrup
              AND mara~matkl IN matkla
              AND mara~mtart IN matart
              AND (lv_dynwhere)
            GROUP BY mcss~matnr mcss~werks sobkz mcss~kunnr kzbws.
        APPEND LINES OF collector_dipcs[] TO collector[].
      ENDIF.

* DI IS-ADEC-SUB customer stock at subcontractor (MSCS/MSCD).
      IF  NOT  pa_sond IS INITIAL  AND
          NOT  t_mat[] IS INITIAL  AND
               cl_adsub_constants=>c IN so_sobkz.
        SELECT SINGLE mandt FROM mscd CONNECTION (dbcon) INTO lv_exists.
        IF sy-subrc IS INITIAL. "at least one entry found: continue

          CLEAR collector_dipcs.
          IF lgort[] IS INITIAL.                            "2280679
            IF xmchb IS NOT INITIAL.
*         sum of all materials w/ and w/o batch
              SELECT mscd~matnr mscd~werks mscd~charg mscd~sobkz
                   mscd~kunnr mscd~lifnr AS lifnr2 bwtar
                   SUM( cdlab ) AS labst SUM( cdins ) AS insme
                   SUM( cdein ) AS einme
                   CONNECTION (dbcon)
                   INTO CORRESPONDING FIELDS OF TABLE collector_dipcs
                   FROM mscd INNER JOIN mscs
                   ON   mscd~matnr = mscs~matnr
                    AND mscd~werks = mscs~werks
                    AND mscd~sobkz = mscs~sobkz
                    AND mscd~lifnr = mscs~lifnr             "2303699
                    AND mscd~kunnr = mscs~kunnr
                   JOIN v_marc_md AS marc                   "2561903
                     ON marc~werks = mscs~werks AND
                        marc~matnr = mscs~matnr
                   JOIN mara
                     ON mara~matnr = mscs~matnr
                   LEFT JOIN mcha
                     ON mcha~matnr = mscd~matnr AND
                        mcha~werks = mscd~werks AND
                        mcha~charg = mscd~charg
                    WHERE mscd~matnr IN matnr
                      AND mscd~werks IN werks
                      AND mscd~sobkz IN so_sobkz
                      AND mscd~charg IN charg
                      AND ( mscd~cdlab IN r_stopt
                       OR   mscd~cdins IN r_stopt
                       OR   mscd~cdein IN r_stopt )
                       AND marc~ekgrp IN ekgrup
                       AND mara~matkl IN matkla
                       AND mara~mtart IN matart
                       AND (lv_dynwhere)
                    GROUP BY mscd~matnr mscd~werks
                             mscd~sobkz mscd~kunnr mscd~lifnr
                             mscd~charg bwtar.
            ELSE.
*         sum without batch
              SELECT mscd~matnr mscd~werks mscd~sobkz
                   mscd~kunnr mscd~lifnr AS lifnr2
                   SUM( cdlab ) AS labst SUM( cdins ) AS insme
                   SUM( cdein ) AS einme
                   CONNECTION (dbcon)
                   INTO CORRESPONDING FIELDS OF TABLE collector_dipcs
                   FROM mscd INNER JOIN mscs
                   ON   mscd~matnr = mscs~matnr
                    AND mscd~werks = mscs~werks
                    AND mscd~sobkz = mscs~sobkz
                    AND mscd~lifnr = mscs~lifnr             "2303699
                    AND mscd~kunnr = mscs~kunnr
                   JOIN v_marc_md AS marc                   "2561903
                     ON marc~werks = mscs~werks AND
                        marc~matnr = mscs~matnr
                   JOIN mara
                     ON mara~matnr = mscs~matnr
                    WHERE mscd~matnr IN matnr
                      AND mscd~werks IN werks
                      AND mscd~sobkz IN so_sobkz
                      AND ( mscd~cdlab IN r_stopt
                       OR   mscd~cdins IN r_stopt
                       OR   mscd~cdein IN r_stopt )
                       AND marc~ekgrp IN ekgrup
                       AND mara~matkl IN matkla
                       AND mara~mtart IN matart
                       AND (lv_dynwhere)
                    GROUP BY mscd~matnr mscd~werks
                             mscd~sobkz mscd~kunnr mscd~lifnr.
            ENDIF.
            APPEND LINES OF collector_dipcs[] TO collector[].
          ENDIF.                                            "2280679
        ENDIF.
      ENDIF.
* DI IS-ADEC-SUB sales order stock at subcontractor (MSFS/MSFD).
      IF  NOT  pa_sond IS INITIAL  AND
          NOT  t_mat[] IS INITIAL  AND
               cl_adsub_constants=>f IN so_sobkz.

        SELECT SINGLE mandt FROM msfd CONNECTION (dbcon) INTO lv_exists.
        IF sy-subrc IS INITIAL. "at least one entry found: continue

          CLEAR collector_dipcs.

          IF lgort[] IS INITIAL.                            "2280679
            IF xmchb IS NOT INITIAL.
*         sum of all materials w/ and w/o batch
              SELECT msfd~matnr msfd~werks msfd~charg msfd~sobkz bwtar
                     msfd~vbeln msfd~posnr msfd~lifnr AS lifnr2 mssa~kzbws
                   SUM( fdlab ) AS labst SUM( fdins ) AS insme
                   SUM( fdein ) AS einme
                   CONNECTION (dbcon)
                   INTO CORRESPONDING FIELDS OF TABLE collector_dipcs
                   FROM msfd INNER JOIN msfs
                   ON   msfd~matnr = msfs~matnr
                    AND msfd~werks = msfs~werks
                    AND msfd~sobkz = msfs~sobkz
                    AND msfd~lifnr = msfs~lifnr             "2303699
                    AND msfd~vbeln = msfs~vbeln
                    AND msfd~posnr = msfs~posnr
                  INNER JOIN mssa
                   ON   msfs~matnr = mssa~matnr
                    AND msfs~werks = mssa~werks
                    AND msfs~vbeln = mssa~vbeln
                    AND msfs~posnr = mssa~posnr
                   JOIN v_marc_md AS marc                   "2561903
                     ON marc~werks = msfs~werks AND
                        marc~matnr = msfs~matnr
                   JOIN mara
                     ON mara~matnr = msfs~matnr
                   LEFT JOIN mcha
                     ON mcha~matnr = msfd~matnr AND
                        mcha~werks = msfd~werks AND
                        mcha~charg = msfd~charg
                    WHERE msfd~matnr IN matnr
                      AND msfd~werks IN werks
                      AND msfd~sobkz IN so_sobkz
                      AND msfd~charg IN charg
                      AND ( msfd~fdlab IN r_stopt
                       OR   msfd~fdins IN r_stopt
                       OR   msfd~fdein IN r_stopt )
                       AND marc~ekgrp IN ekgrup
                       AND mara~matkl IN matkla
                       AND mara~mtart IN matart
                       AND mssa~sobkz = cl_adsub_constants=>e
                       AND (lv_dynwhere)
                    GROUP BY msfd~matnr msfd~werks msfd~sobkz
                             msfd~vbeln msfd~posnr msfd~lifnr
                             mssa~kzbws msfd~charg bwtar.
            ELSE.
*         sum without batch
              SELECT msfd~matnr msfd~werks msfd~sobkz
                     msfd~vbeln msfd~posnr msfd~lifnr AS lifnr2 mssa~kzbws
                   SUM( fdlab ) AS labst SUM( fdins ) AS insme
                   SUM( fdein ) AS einme
                   CONNECTION (dbcon)
                   INTO CORRESPONDING FIELDS OF TABLE collector_dipcs
                   FROM msfd INNER JOIN msfs
                   ON   msfd~matnr = msfs~matnr
                    AND msfd~werks = msfs~werks
                    AND msfd~sobkz = msfs~sobkz
                    AND msfd~lifnr = msfs~lifnr             "2303699
                    AND msfd~vbeln = msfs~vbeln
                    AND msfd~posnr = msfs~posnr
                  INNER JOIN mssa
                   ON   msfs~matnr = mssa~matnr
                    AND msfs~werks = mssa~werks
                    AND msfs~vbeln = mssa~vbeln
                    AND msfs~posnr = mssa~posnr
                   JOIN v_marc_md AS marc                   "2561903
                     ON marc~werks = msfs~werks AND
                        marc~matnr = msfs~matnr
                   JOIN mara
                     ON mara~matnr = msfs~matnr
                    WHERE msfd~matnr IN matnr
                      AND msfd~werks IN werks
                      AND msfd~sobkz IN so_sobkz
                      AND ( msfd~fdlab IN r_stopt
                       OR   msfd~fdins IN r_stopt
                       OR   msfd~fdein IN r_stopt )
                       AND marc~ekgrp IN ekgrup
                       AND mara~matkl IN matkla
                       AND mara~mtart IN matart
                       AND mssa~sobkz = cl_adsub_constants=>e
                       AND (lv_dynwhere)
                    GROUP BY msfd~matnr msfd~werks msfd~sobkz
                             msfd~vbeln msfd~posnr msfd~lifnr
                             mssa~kzbws.
            ENDIF.
            IF collector_dipcs[] IS NOT INITIAL.
              lv_xebew = 'X'.
              lv_xmbew = 'X'.
            ENDIF.
            APPEND LINES OF collector_dipcs[] TO collector[].
          ENDIF.                                            "2280679
        ENDIF.
      ENDIF.
* DI IS-ADEC-SUB consignment or RTP stock at subcontractor (MSIS/MSID).
      IF  NOT  pa_sond IS INITIAL  AND
          NOT  t_mat[] IS INITIAL  AND
             ( cl_adsub_constants=>i IN so_sobkz OR
               cl_adsub_constants=>j IN so_sobkz ).

        SELECT SINGLE mandt FROM msid CONNECTION (dbcon) INTO lv_exists.
        IF sy-subrc IS INITIAL. "at least one entry found: continue

          CLEAR collector_dipcs.

          IF lgort[] IS INITIAL.                            "2280679
            IF xmchb IS NOT INITIAL.
*         sum of all materials w/ and w/o batch
              SELECT msid~matnr msid~werks msid~charg msid~sobkz
                   msid~owner msid~lifnr AS lifnr2 bwtar
                   SUM( idlab ) AS labst SUM( idins ) AS insme
                   SUM( idein ) AS einme
                   CONNECTION (dbcon)
                   INTO CORRESPONDING FIELDS OF TABLE collector_dipcs
                   FROM msid INNER JOIN msis
                   ON   msid~matnr = msis~matnr
                    AND msid~werks = msis~werks
                    AND msid~sobkz = msis~sobkz
                    AND msid~lifnr = msis~lifnr             "2303699
                    AND msid~owner = msis~owner
                   JOIN v_marc_md AS marc                   "2561903
                     ON marc~werks = msis~werks AND
                        marc~matnr = msis~matnr
                   JOIN mara
                     ON mara~matnr = msis~matnr
                   LEFT JOIN mcha
                     ON mcha~matnr = msid~matnr AND
                        mcha~werks = msid~werks AND
                        mcha~charg = msid~charg
                    WHERE msid~matnr IN matnr
                      AND msid~werks IN werks
                      AND msid~sobkz IN so_sobkz
                      AND msid~charg IN charg
                      AND ( msid~idlab IN r_stopt
                       OR   msid~idins IN r_stopt
                       OR   msid~idein IN r_stopt )
                       AND marc~ekgrp IN ekgrup
                       AND mara~matkl IN matkla
                       AND mara~mtart IN matart
                       AND (lv_dynwhere)
                    GROUP BY msid~matnr msid~werks
                             msid~sobkz msid~owner msid~lifnr
                             msid~charg bwtar.
            ELSE.
*         sum without batch
              SELECT msid~matnr msid~werks msid~sobkz
                   msid~owner msid~lifnr AS lifnr2
                   SUM( idlab ) AS labst SUM( idins ) AS insme
                   SUM( idein ) AS einme
                   CONNECTION (dbcon)
                   INTO CORRESPONDING FIELDS OF TABLE collector_dipcs
                   FROM msid INNER JOIN msis
                   ON   msid~matnr = msis~matnr
                    AND msid~werks = msis~werks
                    AND msid~sobkz = msis~sobkz
                    AND msid~lifnr = msis~lifnr             "2303699
                    AND msid~owner = msis~owner
                   JOIN v_marc_md AS marc                   "2561903
                     ON marc~werks = msis~werks AND
                        marc~matnr = msis~matnr
                   JOIN mara
                     ON mara~matnr = msis~matnr
                    WHERE msid~matnr IN matnr
                      AND msid~werks IN werks
                      AND msid~sobkz IN so_sobkz
                      AND ( msid~idlab IN r_stopt
                       OR   msid~idins IN r_stopt
                       OR   msid~idein IN r_stopt )
                       AND marc~ekgrp IN ekgrup
                       AND mara~matkl IN matkla
                       AND mara~mtart IN matart
                       AND (lv_dynwhere)
                    GROUP BY msid~matnr msid~werks
                             msid~sobkz msid~owner msid~lifnr.
            ENDIF.
            APPEND LINES OF collector_dipcs[] TO collector[].
          ENDIF.                                            "2280679
        ENDIF.
      ENDIF.
* DI IS-ADEC-SUB project stock at subcontractor (MSRS/MSRD).
      IF  NOT  pa_sond IS INITIAL  AND
          NOT  t_mat[] IS INITIAL  AND
               cl_adsub_constants=>r IN so_sobkz.

        SELECT SINGLE mandt FROM msrd CONNECTION (dbcon) INTO lv_exists.
        IF sy-subrc IS INITIAL. "at least one entry found: continue

          CLEAR collector_dipcs.

          IF lgort[] IS INITIAL.                            "2280679
            IF xmchb IS NOT INITIAL.
*         sum of all materials w/ and w/o batch
              SELECT msrd~matnr msrd~werks msrd~charg msrd~sobkz bwtar
                     msrd~pspnr msrd~lifnr AS lifnr2 mssq~kzbws
                   SUM( rdlab ) AS labst SUM( rdins ) AS insme
                   SUM( rdein ) AS einme
                   CONNECTION (dbcon)
                   INTO CORRESPONDING FIELDS OF TABLE collector_dipcs
                   FROM msrd INNER JOIN msrs
                   ON   msrd~matnr = msrs~matnr
                    AND msrd~werks = msrs~werks
                    AND msrd~sobkz = msrs~sobkz
                    AND msrd~lifnr = msrs~lifnr             "2303699
                    AND msrd~pspnr = msrs~pspnr
                  INNER JOIN mssq
                   ON   msrs~matnr = mssq~matnr
                    AND msrs~werks = mssq~werks
                    AND msrs~pspnr = mssq~pspnr
                   JOIN v_marc_md AS marc                   "2561903
                     ON marc~werks = msrs~werks AND
                        marc~matnr = msrs~matnr
                   JOIN mara
                     ON mara~matnr = msrs~matnr
                   LEFT JOIN mcha
                     ON mcha~matnr = msrd~matnr AND
                        mcha~werks = msrd~werks AND
                        mcha~charg = msrd~charg
                    WHERE msrd~matnr IN matnr
                      AND msrd~werks IN werks
                      AND msrd~sobkz IN so_sobkz
                      AND msrd~charg IN charg
                      AND ( msrd~rdlab IN r_stopt
                       OR   msrd~rdins IN r_stopt
                       OR   msrd~rdein IN r_stopt )
                       AND marc~ekgrp IN ekgrup
                       AND mara~matkl IN matkla
                       AND mara~mtart IN matart
                       AND mssq~sobkz = cl_adsub_constants=>q
                       AND (lv_dynwhere)
                    GROUP BY msrd~matnr msrd~werks msrd~sobkz
                             msrd~pspnr msrd~lifnr
                             mssq~kzbws msrd~charg bwtar.
            ELSE.
*         sum without batch
              SELECT msrd~matnr msrd~werks msrd~sobkz
                     msrd~pspnr msrd~lifnr AS lifnr2 mssq~kzbws
                   SUM( rdlab ) AS labst SUM( rdins ) AS insme
                   SUM( rdein ) AS einme
                   CONNECTION (dbcon)
                   INTO CORRESPONDING FIELDS OF TABLE collector_dipcs
                   FROM msrd INNER JOIN msrs
                   ON   msrd~matnr = msrs~matnr
                    AND msrd~werks = msrs~werks
                    AND msrd~sobkz = msrs~sobkz
                    AND msrd~lifnr = msrs~lifnr             "2303699
                    AND msrd~pspnr = msrs~pspnr
                  INNER JOIN mssq
                   ON   msrs~matnr = mssq~matnr
                    AND msrs~werks = mssq~werks
                    AND msrs~pspnr = mssq~pspnr
                   JOIN v_marc_md AS marc                   "2561903
                     ON marc~werks = msrs~werks AND
                        marc~matnr = msrs~matnr
                   JOIN mara
                     ON mara~matnr = msrs~matnr
                    WHERE msrd~matnr IN matnr
                      AND msrd~werks IN werks
                      AND msrd~sobkz IN so_sobkz
                      AND ( msrd~rdlab IN r_stopt
                       OR   msrd~rdins IN r_stopt
                       OR   msrd~rdein IN r_stopt )
                       AND marc~ekgrp IN ekgrup
                       AND mara~matkl IN matkla
                       AND mara~mtart IN matart
                       AND mssq~sobkz = cl_adsub_constants=>q
                       AND (lv_dynwhere)
                    GROUP BY msrd~matnr msrd~werks msrd~sobkz
                             msrd~pspnr msrd~lifnr
                             mssq~kzbws.
            ENDIF.
            IF collector_dipcs[] IS NOT INITIAL.
              lv_xqbew = 'X'.
            ENDIF.
            APPEND LINES OF collector_dipcs[] TO collector[].
          ENDIF.                                            "2280679
        ENDIF.
      ENDIF.

      FREE collector_dipcs.
*                                                                          "^1920470
      "{ End ENHO DIPCS_RM07MLBS001 IS-AD-SSP AD_SUB }

ENHANCEMENT-POINT rm07mlbs_15 SPOTS es_rm07mlbs .

************************************************************************
************************************************************************

      IF nozero = abap_true OR negativ = abap_true.         "2045154
        DELETE collector WHERE labst = 0 AND einme = 0 AND
           insme = 0 AND retme = 0 AND
           speme = 0 AND umlme = 0 AND
           bwesb = 0 AND bwesb = 0 AND
           trame = 0 AND glgmg = 0 AND
           umlmc = 0.
      ENDIF.
      IF negativ = abap_true.                               "v2045154
        DELETE collector WHERE labst GE 0 AND einme GE 0 AND
           insme GE 0 AND retme GE 0 AND
           speme GE 0 AND umlme GE 0 AND
           bwesb GE 0 AND bwesb GE 0 AND
           trame GE 0 AND glgmg GE 0 AND
           umlmc GE 0.
      ENDIF.                                                "^2045154

      IF collector[] IS INITIAL.
*     nothing found for selection - leave
        RETURN.
      ENDIF.

************************************************************************
* Fill in additional data (first round) and extract the data
* for the access to the valuation tables.
************************************************************************
      SORT t_mat BY matnr werks.
      SORT g_t_organ             BY werks.
      CLEAR : g_s_t001l,         g_s_organ.

*************************************************************************
** Read the valuation tables
*************************************************************************
      BREAK-POINT ID mmim_rep_mb52.

* begin of 2707685                                              "v2707685
      DATA: lv_columns    TYPE TABLE OF string,
            lv_columns_e  TYPE TABLE OF string,
            lv_columns_q  TYPE TABLE OF string,
            lv_group_by   TYPE TABLE OF string,
            lv_group_by_e TYPE TABLE OF string,
            lv_group_by_q TYPE TABLE OF string.

*     build the field catalog and group by clause depending
*     on the selection (w/ or w/o valuation type)
      IF xmchb IS INITIAL.
*       without valuation type
        lv_group_by = VALUE #( ( `xbew~matnr,` )
                               ( `xbew~bwkey,` )
                               ( `xbew~vprsv,` )
                               ( `xbew~verpr,` )
                               ( `xbew~stprs,` )
                               ( `xbew~peinh` ) ).
      ELSE.
*       with valuation type
        lv_group_by = VALUE #( ( `xbew~matnr,` )
                               ( `xbew~bwkey,` )
                               ( `xbew~bwtar,` )
                               ( `xbew~vprsv,` )
                               ( `xbew~verpr,` )
                               ( `xbew~stprs,` )
                               ( `xbew~peinh` ) ).
      ENDIF.

      lv_columns = VALUE #(  BASE lv_group_by
                            ( `, SUM( vmsl ) AS lbkum,` )
                            ( `  SUM( hsl ) AS salk3` ) ).

      lv_group_by_e = VALUE #(  BASE lv_group_by
                               ( `, xbew~sobkz,` )
                               ( `xbew~vbeln,` )
                               ( `xbew~posnr` ) ).

      lv_columns_e = VALUE #(  BASE lv_group_by_e
                            ( `, SUM( vmsl ) AS lbkum,` )
                            ( `  SUM( hsl ) AS salk3` ) ).

      lv_group_by_q = VALUE #(  BASE lv_group_by
                               ( `, xbew~sobkz,` )
                               ( `xbew~pspnr` ) ).

      lv_columns_q = VALUE #(  BASE lv_group_by_q
                            ( `, SUM( vmsl ) AS lbkum,` )
                            ( `  SUM( hsl ) AS salk3` ) ).

      IF novalues IS INITIAL.
        READ TABLE collector WITH KEY kzbws = 'A' TRANSPORTING NO FIELDS.
        IF sy-subrc = 0 OR lv_xmbew = 'X'.
          IF werks[] IS NOT INITIAL AND matnr[] IS INITIAL AND "v2561903
             lgort[] IS INITIAL AND charg[] IS INITIAL.
*            restriction for values only by plant
            SELECT (lv_columns)
               INTO CORRESPONDING FIELDS OF TABLE @lt_mbew
               FROM v_mbew_md AS xbew
                 JOIN t001w
                  ON  t001w~bwkey = xbew~bwkey
                 JOIN acdoca_m_extract
                  ON  acdoca_m_extract~kalnr = xbew~kaln1
                  AND acdoca_m_extract~bwkey = xbew~bwkey
                WHERE t001w~werks IN @werks
                  AND fiscyearper = '9999999'
                  AND  (lv_dynwhere)
               GROUP BY
                 (lv_group_by)
               HAVING (lv_dynhvng).
          ELSE.
*           normal selection with restriction plant & matnr if provided
            IF lgort[] IS INITIAL OR l_flag_xlgort IS NOT INITIAL OR    "2673346
               pa_sond IS NOT INITIAL.                                  "2673346
              SELECT (lv_columns)
                INTO CORRESPONDING FIELDS OF TABLE @lt_mbew
                FROM acdoca_m_extract
                   JOIN v_mbew_md AS xbew
                    ON  acdoca_m_extract~kalnr = xbew~kaln1
                    AND acdoca_m_extract~bwkey = xbew~bwkey
                   JOIN t001w
                    ON  xbew~bwkey = t001w~bwkey
                   JOIN v_marc_md AS marc
                    ON  t001w~werks = marc~werks
                    AND xbew~matnr = marc~matnr
                   WHERE marc~werks IN @werks
                   AND   marc~matnr IN @matnr
                   AND fiscyearper = '9999999'
                   AND  (lv_dynwhere)
                GROUP BY
                  (lv_group_by)
                HAVING (lv_dynhvng).
            ELSE.
*             restriction by storage location, plant, material if provided
              SELECT (lv_columns)
                INTO CORRESPONDING FIELDS OF TABLE @lt_mbew
                FROM acdoca_m_extract
                   JOIN v_mbew_md AS xbew
                    ON  acdoca_m_extract~kalnr = xbew~kaln1
                    AND acdoca_m_extract~bwkey = xbew~bwkey
                   JOIN t001w
                    ON  xbew~bwkey = t001w~bwkey
                   JOIN v_mard_md AS mard
                    ON   mard~matnr = xbew~matnr
                    AND  mard~werks = t001w~werks
                   WHERE mard~werks IN @werks
                   AND   mard~matnr IN @matnr
                   AND   mard~lgort IN @lgort
                   AND fiscyearper = '9999999'
                   AND  (lv_dynwhere)
                GROUP BY
                   (lv_group_by)
                HAVING (lv_dynhvng).
            ENDIF.
          ENDIF.
          SORT lt_mbew BY matnr bwkey bwtar.                "^2561903
        ENDIF.

        IF lv_xebew = 'X'.
          READ TABLE collector WITH KEY kzbws = 'M' TRANSPORTING NO FIELDS.
          IF sy-subrc = 0.
            IF werks[] IS NOT INITIAL AND matnr[] IS INITIAL AND "v2561903
               lgort[] IS INITIAL AND charg[] IS INITIAL.
*              restriction for values only by plant
              SELECT (lv_columns_e)
                 INTO CORRESPONDING FIELDS OF TABLE @lt_ebew
                 FROM v_ebew_md AS xbew
                   JOIN t001w
                    ON  t001w~bwkey = xbew~bwkey
                   JOIN acdoca_m_extract
                    ON  acdoca_m_extract~kalnr = xbew~kaln1
                    AND acdoca_m_extract~bwkey = xbew~bwkey
                  WHERE t001w~werks IN @werks
                    AND fiscyearper = '9999999'
                    AND  (lv_dynwhere)
                 GROUP BY
                    (lv_group_by_e)
                HAVING (lv_dynhvng).
            ELSE.
*             normal selection with restriction plant & matnr if provided
              IF lgort[] IS INITIAL OR l_flag_xlgort IS NOT INITIAL.
                SELECT (lv_columns_e)
                  INTO CORRESPONDING FIELDS OF TABLE @lt_ebew
                  FROM acdoca_m_extract
                     JOIN v_ebew_md AS xbew
                      ON  acdoca_m_extract~kalnr = xbew~kaln1
                      AND acdoca_m_extract~bwkey = xbew~bwkey
                     JOIN t001w
                      ON  xbew~bwkey = t001w~bwkey
                     JOIN v_marc_md AS marc
                      ON  t001w~werks = marc~werks
                      AND xbew~matnr = marc~matnr
                     WHERE marc~werks IN @werks
                     AND   marc~matnr IN @matnr
                     AND fiscyearper = '9999999'
                     AND  (lv_dynwhere)
                  GROUP BY
                     (lv_group_by_e)
                  HAVING (lv_dynhvng).
              ELSE.
*               restriction by storage location, plant, material if provided
                SELECT DISTINCT
                  (lv_columns_e)
                  INTO CORRESPONDING FIELDS OF TABLE @lt_ebew
                  FROM acdoca_m_extract
                     JOIN v_ebew_md AS xbew
                      ON  acdoca_m_extract~kalnr = xbew~kaln1
                      AND acdoca_m_extract~bwkey = xbew~bwkey
                     JOIN t001w
                      ON  xbew~bwkey = t001w~bwkey
                     JOIN v_mska_md AS mska
                      ON   mska~matnr = xbew~matnr
                      AND  mska~werks = t001w~werks
                     WHERE mska~werks IN @werks
                     AND   mska~matnr IN @matnr
                     AND   mska~lgort IN @lgort
                     AND fiscyearper = '9999999'
                     AND  (lv_dynwhere)
                  GROUP BY
                     (lv_group_by_e)
                  HAVING (lv_dynhvng).
              ENDIF.
            ENDIF.
            SORT lt_ebew BY matnr bwkey bwtar sobkz vbeln posnr. "^2561903
          ENDIF.
        ENDIF.

        IF lv_xqbew = 'X'.
          READ TABLE collector WITH KEY kzbws = 'M' TRANSPORTING NO FIELDS.
          IF sy-subrc = 0.
            IF werks[] IS NOT INITIAL AND matnr[] IS INITIAL AND "v2561903
               lgort[] IS INITIAL AND charg[] IS INITIAL.
*              restriction for values only by plant
              SELECT (lv_columns_q)
                 INTO CORRESPONDING FIELDS OF TABLE @lt_qbew
                 FROM v_qbew_md AS xbew
                   JOIN t001w
                    ON  t001w~bwkey = xbew~bwkey
                   JOIN acdoca_m_extract
                    ON  acdoca_m_extract~kalnr = xbew~kaln1
                    AND acdoca_m_extract~bwkey = xbew~bwkey
                  WHERE t001w~werks IN @werks
                    AND fiscyearper = '9999999'
                    AND  (lv_dynwhere)
                 GROUP BY
                    (lv_group_by_q)
                HAVING (lv_dynhvng).
            ELSE.
*             normal selection with restriction plant & matnr if provided
              IF lgort[] IS INITIAL OR l_flag_xlgort IS NOT INITIAL.
                SELECT (lv_columns_q)
                  INTO CORRESPONDING FIELDS OF TABLE @lt_qbew
                  FROM acdoca_m_extract
                     JOIN v_qbew_md AS xbew
                      ON  acdoca_m_extract~kalnr = xbew~kaln1
                      AND acdoca_m_extract~bwkey = xbew~bwkey
                     JOIN t001w
                      ON  xbew~bwkey = t001w~bwkey
                     JOIN v_marc_md AS marc
                      ON  t001w~werks = marc~werks
                      AND xbew~matnr = marc~matnr
                     WHERE marc~werks IN @werks
                     AND   marc~matnr IN @matnr
                     AND fiscyearper = '9999999'
                     AND  (lv_dynwhere)
                  GROUP BY
                     (lv_group_by_q)
                  HAVING (lv_dynhvng).
              ELSE.
*               restriction by storage location, plant, material if provided
                SELECT DISTINCT (lv_columns_q)
                  INTO CORRESPONDING FIELDS OF TABLE @lt_qbew
                  FROM acdoca_m_extract
                     JOIN v_qbew_md AS xbew
                      ON  acdoca_m_extract~kalnr = xbew~kaln1
                      AND acdoca_m_extract~bwkey = xbew~bwkey
                     JOIN t001w
                      ON  xbew~bwkey = t001w~bwkey
                     JOIN v_mspr_md AS mspr
                      ON   mspr~matnr = xbew~matnr
                      AND  mspr~werks = t001w~werks
                     WHERE mspr~werks IN @werks
                     AND   mspr~matnr IN @matnr
                     AND   mspr~lgort IN @lgort
                     AND fiscyearper = '9999999'
                     AND  (lv_dynwhere)
                  GROUP BY
                     (lv_group_by_q)
                  HAVING (lv_dynhvng).
              ENDIF.
            ENDIF.
            SORT lt_qbew BY matnr bwkey bwtar sobkz pspnr.  "^2561903
          ENDIF.
        ENDIF.
      ENDIF. "novalues is initial
* end of 2707685                                                "^2707685
************************************************************************
* Extract key-data for other tables.
************************************************************************
      REFRESH:     bestand.

      BREAK-POINT ID mmim_rep_mb52.
      LOOP AT collector  WHERE charg IN charg.              "2035059
        CLEAR factor.
        CLEAR bestand. "header line!
        MOVE-CORRESPONDING collector TO bestand.

*   fill the key of the special stocks into the field
*   assigment
        CASE    collector-sobkz.
          WHEN  'E'.
            MOVE  : 'X'               TO  g_flag_sobkz-vbeln.
            WRITE : collector-vbeln   TO  bestand-ssnum.
            MOVE  : '/'               TO  bestand-ssnum+10(01).
            WRITE : collector-posnr   TO  bestand-ssnum+12(08)
                                      NO-ZERO.
            CONDENSE                  bestand-ssnum.

          WHEN  'T'.
            MOVE  : 'X'               TO  g_flag_sobkz-vbeln.
            WRITE : collector-vbeln   TO  bestand-ssnum.
            MOVE  : '/'               TO  bestand-ssnum+10(01).
            WRITE : collector-posnr   TO  bestand-ssnum+12(08)
                                      NO-ZERO.
            CONDENSE                  bestand-ssnum.

          WHEN  'K'.
            MOVE  : 'X'               TO  g_flag_sobkz-lifnr.
            WRITE : collector-lifnr   TO  bestand-ssnum.

          WHEN  'M'.
            MOVE  : 'X'               TO  g_flag_sobkz-lifnr.
            WRITE : collector-lifnr   TO  bestand-ssnum.

          WHEN  'O'.
            MOVE  : 'X'               TO  g_flag_sobkz-lifnr.
            WRITE : collector-lifnr   TO  bestand-ssnum.

          WHEN  'Q'.
            MOVE  : 'X'               TO  g_flag_sobkz-pspnr.
            WRITE : collector-pspnr   TO  bestand-ssnum.

          WHEN  'V'.
            MOVE  : 'X'               TO  g_flag_sobkz-kunnr.
            WRITE : collector-kunnr   TO  bestand-ssnum.

          WHEN  'W'.
            MOVE  : 'X'               TO  g_flag_sobkz-kunnr.
            WRITE : collector-kunnr   TO  bestand-ssnum.

          WHEN  OTHERS.
            CLEAR                     bestand-ssnum.
        ENDCASE.

        "{ Begin ENHO DIPCS_RM07MLBS001 IS-AD-SSP AD_SUB }
* DI IS-ADEC-SSP Customer Stock                                            "v1920470
        CASE    collector-sobkz.
          WHEN  'B'.
            MOVE  : 'X'               TO  g_flag_sobkz-kunnr.
            WRITE : collector-kunnr   TO  bestand-ssnum.
*    * DI IS-ADEC-SUB customer stock at subcontractor         "v_n_GA1727687
          WHEN  cl_adsub_constants=>c.
            MOVE  : 'X'               TO  g_flag_sobkz-kunnr.
            WRITE : collector-kunnr   TO  bestand-ssnum.
            MOVE  : 'X'               TO  g_flag_sobkz-lifnr.
            WRITE : collector-lifnr2  TO  bestand-lifnr2.
*    * DI IS-ADEC-SUB sales order stock at subcontractor
          WHEN  cl_adsub_constants=>f.
            MOVE  : 'X'               TO  g_flag_sobkz-vbeln.
            WRITE : collector-vbeln   TO  bestand-ssnum.
            MOVE  : '/'               TO  bestand-ssnum+10(01).
            WRITE : collector-posnr   TO  bestand-ssnum+12(08)
                                      NO-ZERO.
            CONDENSE                  bestand-ssnum.
            MOVE  : 'X'               TO  g_flag_sobkz-lifnr.
            WRITE : collector-lifnr2  TO  bestand-lifnr2.
*    * DI IS-ADEC-SUB consignment or RTP stock at subcontractor
          WHEN  cl_adsub_constants=>i OR cl_adsub_constants=>j.
            MOVE  : 'X'               TO  g_flag_sobkz-kunnr.
            WRITE : collector-owner   TO  bestand-ssnum.
            MOVE  : 'X'               TO  g_flag_sobkz-lifnr.
            WRITE : collector-lifnr2  TO  bestand-lifnr2.
*    * DI IS-ADEC-SUB project stock at subcontractor
          WHEN  cl_adsub_constants=>r.
            MOVE  : 'X'               TO  g_flag_sobkz-pspnr.
            WRITE : collector-pspnr   TO  bestand-ssnum.
            MOVE  : 'X'               TO  g_flag_sobkz-lifnr.
            WRITE : collector-lifnr2  TO  bestand-lifnr2.
*    * DI IS-ADEC-SUB                                         "^_n_GA1727687
          WHEN  OTHERS.
            IF collector-sobkz NA 'ETKMOQVW'.               "SH note 2011731
              CLEAR                 bestand-ssnum.
            ENDIF.                                          "SH note 2011731
        ENDCASE.
*                                                                          "^1920470
        "{ End ENHO DIPCS_RM07MLBS001 IS-AD-SSP AD_SUB }

ENHANCEMENT-POINT rm07mlbs_16 SPOTS es_rm07mlbs .

        IF  g_flag_t001l = 'X'.
          IF  bestand-werks = g_s_t001l-werks  AND
              bestand-lgort = g_s_t001l-lgort.
          ELSE.
*       read with plant and storage location
            READ TABLE g_t_t001l   INTO  g_s_t001l
              WITH TABLE KEY werks = bestand-werks
                             lgort = bestand-lgort.

            MOVE : bestand-werks TO g_s_t001l-werks,
                   bestand-lgort TO g_s_t001l-lgort.

            IF sy-subrc <> 0.
              CLEAR              g_s_t001l-lgobe.
            ENDIF.
          ENDIF.
        ENDIF.

*   take the storage bin from the buffer
        MOVE : g_s_t001l-lgobe   TO  bestand-lgobe.


*   get the information per plant with buffer
        IF  bestand-werks  NE  g_s_organ-werks.
          READ TABLE g_t_organ   INTO  g_s_organ
              WITH KEY werks = bestand-werks
                                 BINARY SEARCH.

          IF sy-subrc <> 0.
*       sorry nothing found
            CLEAR                g_s_organ.
            MOVE : bestand-werks TO  g_s_organ-werks.
          ENDIF.
        ENDIF.

*   take the following fields from the buffer
        MOVE : g_s_organ-name1   TO  bestand-name1,
               g_s_organ-waers   TO  bestand-waers,
               g_s_organ-bwkey   TO  bestand-bwkey.

*   get the information from the material master MARC
*   with buffer
        IF  bestand-matnr = l_s_mat-matnr  AND
            bestand-werks = l_s_mat-werks.
*     results are in the buffer
        ELSE.
          CLEAR                  l_s_mat.
          MOVE : bestand-matnr   TO  l_s_mat-matnr,
                 bestand-werks   TO  l_s_mat-werks.

          READ TABLE t_mat       INTO  l_s_mat
             WITH KEY matnr = bestand-matnr
                      werks = bestand-werks
                                 BINARY SEARCH.

          IF  sy-subrc <> 0.                                "v2056114
            SELECT SINGLE mara~matnr werks xchar mtart matkl meins maktx
                   mara~lvorm AS lvorm_mara
                   marc~lvorm AS lvorm_marc
                 CONNECTION (dbcon)
                INTO CORRESPONDING FIELDS OF l_s_mat
                FROM mara
                 INNER JOIN marc
                    ON mara~matnr = marc~matnr
                 LEFT JOIN makt
                    ON makt~matnr = mara~matnr AND
                       makt~spras = sy-langu
                 WHERE mara~matnr = bestand-matnr
                   AND      werks = bestand-werks.
            IF sy-subrc = 0.
              APPEND l_s_mat TO t_mat.
              SORT t_mat BY matnr werks.
            ELSE.
*           sorry nothing found
              CLEAR                l_s_mat.
              MOVE : bestand-matnr TO  l_s_mat-matnr,
                     bestand-werks TO  l_s_mat-werks.
            ENDIF.                                          "^2056114
          ENDIF.
        ENDIF.

*   take the results the buffer
        MOVE : l_s_mat-mtart     TO  bestand-mtart,
               l_s_mat-matkl     TO  bestand-matkl,
               l_s_mat-meins     TO  bestand-meins,
               l_s_mat-maktx     TO  bestand-maktx.


*   if this entry has no deletion flag, take the
*   deletion flag from a higher level like MARA, MARC,
*   or MARDA
        IF  bestand-lvorm IS INITIAL.
          IF      NOT  l_s_mat-lvorm_marc IS INITIAL.
            MOVE  l_s_mat-lvorm_marc
                                 TO  bestand-lvorm.
          ELSEIF  NOT  l_s_mat-lvorm_mara IS INITIAL.
            MOVE  l_s_mat-lvorm_mara
                                 TO  bestand-lvorm.

          ELSEIF  NOT g_t_mard_lv[] IS INITIAL  AND
                  NOT bestand-lgort IS INITIAL  AND
                  NOT bestand-sobkz IS INITIAL.
*       look for deletion flag in working table
*       g_t_mard_lv for a line with special stock
            IF  bestand-matnr = g_s_mard_lv-matnr  AND
                bestand-werks = g_s_mard_lv-werks  AND
                bestand-lgort = g_s_mard_lv-lgort.
            ELSE.
*         read table only after the key has changed
              READ TABLE g_t_mard_lv  INTO  g_s_mard_lv
              WITH TABLE KEY matnr = bestand-matnr
                             werks = bestand-werks
                             lgort = bestand-lgort.

              IF sy-subrc <> 0.
*           fill the buffer in case the entry does not exist
                MOVE : bestand-matnr  TO  g_s_mard_lv-matnr,
                       bestand-werks  TO  g_s_mard_lv-werks,
                       bestand-lgort  TO  g_s_mard_lv-lgort.
                CLEAR                 g_s_mard_lv-lvorm.
              ENDIF.

*         take the result from the buffer
              MOVE  g_s_mard_lv-lvorm   TO  bestand-lvorm.
            ENDIF.
          ENDIF.
        ENDIF.

*   ***** STOP here if value is not relevant ********************
        IF ( bestand-waers = space ) OR "failed Auth-Checks (value)
           ( novalues IS NOT INITIAL ) OR
           ( bestand-labst IS INITIAL AND bestand-insme IS INITIAL AND "2561903
             bestand-speme IS INITIAL AND bestand-einme IS INITIAL AND "2561903
             bestand-umlme IS INITIAL AND bestand-bwesb IS INITIAL AND "2561903
             bestand-glgmg IS INITIAL AND bestand-trame IS INITIAL AND "2561903
             bestand-umlmc IS INITIAL ).
          APPEND bestand.
          CONTINUE.
        ENDIF.

        sy-subrc = 4.
        CLEAR: ls_mbew, ls_ebew, ls_qbew, ls_obew, t_mbew.  "2561903

        IF  bestand-sobkz = ' ' OR bestand-sobkz = 'O' OR
            bestand-sobkz = 'W' OR bestand-sobkz = 'V' OR
            bestand-kzbws = 'A'.
          READ TABLE lt_mbew WITH KEY matnr = bestand-matnr "2561903
                                      bwkey = bestand-bwkey
                                      bwtar = bestand-bwtar
                                      BINARY SEARCH
                                      INTO ls_mbew.         "2561903
          IF sy-subrc <> 0.
            IF lv_rere_wertu = abap_true.                             "v2697257
              SELECT count(*) FROM t134m WHERE mtart = bestand-mtart
                                          AND  bwkey = bestand-bwkey
                                          AND  wertu = abap_false. "#EC CI_BYPASS.
              if sy-dbcnt >= 1.
*               material is non-valuated
                APPEND bestand.
                CONTINUE.
              ENDIF.
            ENDIF.                                                    "^2697257
*           try to fetch master record as it may be that there was no valuated
*           posting yet and therefore no result from acdoca
            SELECT SINGLE * FROM v_mbew_md INTO CORRESPONDING FIELDS OF ls_mbew
                            WHERE matnr = bestand-matnr AND
                                  bwkey = bestand-bwkey AND
                                  bwtar = bestand-bwtar.
          ENDIF.
          MOVE-CORRESPONDING ls_mbew TO t_mbew.             "2561903

        ELSEIF ( bestand-sobkz = 'E' OR bestand-sobkz = 'T' )
             AND bestand-kzbws = 'M'.
          READ TABLE lt_ebew WITH KEY matnr = bestand-matnr "2561903
                                     bwkey = bestand-bwkey
                                     bwtar = bestand-bwtar
                                     sobkz = bestand-sobkz
                                     vbeln = bestand-vbeln
                                     posnr = bestand-posnr
                                     BINARY SEARCH
                                     INTO ls_ebew.          "2561903
          IF sy-subrc <> 0.
            IF lv_rere_wertu = abap_true.                             "v2697257
              SELECT count(*) FROM t134m WHERE mtart = bestand-mtart
                                          AND  bwkey = bestand-bwkey
                                          AND  wertu = abap_false. "#EC CI_BYPASS.
              if sy-dbcnt >= 1.
*               material is non-valuated
                APPEND bestand.
                CONTINUE.
              ENDIF.
            ENDIF.                                                    "^2697257
*           try to fetch master record as it might be that there was no valuated
*           posting yet and therefore no result from acdoca
            SELECT SINGLE * FROM v_ebew_md INTO CORRESPONDING FIELDS OF ls_ebew
                            WHERE matnr = bestand-matnr AND
                                  bwkey = bestand-bwkey AND
                                  bwtar = bestand-bwtar AND
                                  sobkz = bestand-sobkz AND
                                  vbeln = bestand-vbeln AND
                                  posnr = bestand-posnr.
          ENDIF.
          MOVE-CORRESPONDING ls_ebew TO t_mbew.             "2561903

        ELSEIF bestand-sobkz = 'Q' AND bestand-kzbws = 'M'.
          READ TABLE lt_qbew WITH KEY matnr = bestand-matnr "2561903
                                      bwkey = bestand-bwkey
                                      bwtar = bestand-bwtar
                                      sobkz = bestand-sobkz
                                      pspnr = bestand-pspnr
                                      BINARY SEARCH
                                      INTO ls_qbew.         "2561903
          IF sy-subrc <> 0.
            IF lv_rere_wertu = abap_true.                             "v2697257
              SELECT count(*) FROM t134m WHERE mtart = bestand-mtart
                                          AND  bwkey = bestand-bwkey
                                          AND  wertu = abap_false. "#EC CI_BYPASS.
              if sy-dbcnt >= 1.
*               material is non-valuated
                APPEND bestand.
                CONTINUE.
              ENDIF.
            ENDIF.                                                    "^2697257
*           try to fetch master record as it may be that there was no valuated
*           posting yet and therefore no result from acdoca
            SELECT SINGLE * FROM v_qbew_md INTO CORRESPONDING FIELDS OF ls_qbew
                            WHERE matnr = bestand-matnr AND
                                  bwkey = bestand-bwkey AND
                                  bwtar = bestand-bwtar AND
                                  sobkz = bestand-sobkz AND
                                  pspnr = bestand-pspnr.
          ENDIF.
          MOVE-CORRESPONDING ls_qbew TO t_mbew.             "2561903
        ENDIF.


        "{ Begin ENHO DIPCS_RM07MLBS001 IS-AD-SSP AD_SUB }
* GA1727687 - A&D special stocks at subcontractor                          "v1920470
* GA1956994 - setting sy-subrc properly
        lv_subrc = sy-subrc.
        IF cl_adsub_switch_check=>get_ad01_switch_state( ) IS NOT INITIAL.
          sy-subrc = lv_subrc.
          IF bestand-sobkz EQ cl_adsub_constants=>f AND
             bestand-kzbws = 'M'.
            READ TABLE lt_ebew WITH KEY matnr = bestand-matnr "2561903
                                        bwkey = bestand-bwkey
                                        bwtar = bestand-bwtar
                                        sobkz = cl_adsub_constants=>e
                                        vbeln = bestand-vbeln "n531604
                                        posnr = bestand-posnr "n531604
                                        BINARY SEARCH
                                        INTO ls_ebew.       "2561903
            IF sy-subrc <> 0.
              IF lv_rere_wertu = abap_true.                             "v2697257
                SELECT count(*) FROM t134m WHERE mtart = bestand-mtart
                                            AND  bwkey = bestand-bwkey
                                            AND  wertu = abap_false. "#EC CI_BYPASS.
                if sy-dbcnt >= 1.
*                 material is non-valuated
                  APPEND bestand.
                  CONTINUE.
                ENDIF.
              ENDIF.                                                    "^2697257
*             try to fetch master record as it might be that there was no valuated
*             posting yet and therefore no result from acdoca
              SELECT SINGLE * FROM v_ebew_md INTO CORRESPONDING FIELDS OF ls_ebew
                              WHERE matnr = bestand-matnr AND
                                    bwkey = bestand-bwkey AND
                                    bwtar = bestand-bwtar AND
                                    sobkz = cl_adsub_constants=>e AND
                                    vbeln = bestand-vbeln AND
                                    posnr = bestand-posnr.
            ENDIF.
            MOVE-CORRESPONDING ls_ebew TO t_mbew.           "2561903
          ELSEIF bestand-sobkz EQ cl_adsub_constants=>r AND
                 bestand-kzbws = 'M'.
            READ TABLE lt_qbew WITH KEY matnr = bestand-matnr "2561903
                                        bwkey = bestand-bwkey
                                        bwtar = bestand-bwtar
                                        sobkz = cl_adsub_constants=>q
                                        pspnr = bestand-pspnr
                                        BINARY SEARCH
                                        INTO ls_qbew.       "2561903
            IF sy-subrc <> 0.
              IF lv_rere_wertu = abap_true.                             "v2697257
                SELECT count(*) FROM t134m WHERE mtart = bestand-mtart
                                            AND  bwkey = bestand-bwkey
                                            AND  wertu = abap_false. "#EC CI_BYPASS.
                if sy-dbcnt >= 1.
*                 material is non-valuated
                  APPEND bestand.
                  CONTINUE.
                ENDIF.
              ENDIF.                                                    "^2697257
*             try to fetch master record as it may be that there was no valuated
*             posting yet and therefore no result from acdoca
              SELECT SINGLE * FROM v_qbew_md INTO CORRESPONDING FIELDS OF ls_qbew
                              WHERE matnr = bestand-matnr AND
                                    bwkey = bestand-bwkey AND
                                    bwtar = bestand-bwtar AND
                                    sobkz = cl_adsub_constants=>q AND
                                    pspnr = bestand-pspnr.
            ENDIF.
            MOVE-CORRESPONDING ls_qbew TO t_mbew.           "2561903
          ENDIF.
        ELSE.
          sy-subrc = lv_subrc.
        ENDIF.
*                                                                          "^1920470
        "{ End ENHO DIPCS_RM07MLBS001 IS-AD-SSP AD_SUB }

ENHANCEMENT-POINT rm07mlbs_17 SPOTS es_rm07mlbs .

        IF sy-subrc = 0.
          IF t_mbew-lbkum = 0.
*         Cannot happen, but in R/3 this does not hold in all cases...
            IF t_mbew-peinh = 0.                            "353428
              t_mbew-peinh = 1.                             "353428
            ENDIF.                                          "353428
*         Calculation of value in case of LBKUM = 0 only possible
*         for MBEW. EBEW and QBEW are collected over all subitems
*         (VBELN...), so the data are not available.
            IF bestand-sobkz = 'E' OR bestand-sobkz = 'Q'.  "388735
              factor = 0.                                   "388735
              CLEAR bestand-waers.                          "388735
            ELSE.                                           "388735
              CASE t_mbew-vprsv.
                WHEN 'V'. factor = t_mbew-verpr / t_mbew-peinh.
                WHEN 'S'. factor = t_mbew-stprs / t_mbew-peinh.
              ENDCASE.
            ENDIF.                                          "388735
          ELSE.
            factor = t_mbew-salk3 / t_mbew-lbkum.
          ENDIF.

          bestand-wlabs = bestand-labst * factor.
          bestand-winsm = bestand-insme * factor.
          bestand-wspem = bestand-speme * factor.
          bestand-weinm = bestand-einme * factor.
          bestand-wumlm = bestand-umlme * factor.
          bestand-wbwesb = bestand-bwesb * factor.          "AC0K020254
          bestand-wglgm = bestand-glgmg * factor.           "n912093
          bestand-wtram = bestand-trame * factor.           "n912093
          bestand-wumlc = bestand-umlmc * factor.           "n912093
        ENDIF.

        APPEND bestand.

      ENDLOOP.

      FREE                       collector.

    CATCH cx_sy_open_sql_db INTO sql_ex.
      DATA: lv_msgtxt TYPE t100-text.                       "v2283091
      SELECT SINGLE text FROM t100 INTO lv_msgtxt
                          WHERE sprsl = sy-langu
                            AND arbgb = 'HDB'
                            AND msgnr = '001'.
      IF sy-subrc IS INITIAL.
        MESSAGE i895 WITH lv_msgtxt. "no hdb usage
      ENDIF.                                                "^2283091
*     use backup for selection
      FREE collector.
      PERFORM data_selection.
  ENDTRY.
ENDFORM.                    "data_selection_new
*
*&---------------------------------------------------------------------*
*&      Form  /cwm/set_default_trsti
*&---------------------------------------------------------------------*
FORM /cwm/set_default_trsti.

  STATICS lv_trsti_read. " x = done

  DATA:
    lv_trsti_set  TYPE c,
    lv_repid      LIKE sy-repid,
    lv_onamext    TYPE rstionamex,
    lv_onamext2   TYPE rstionamex,
    gt_recipients TYPE rsti_t_rep,
    gs_recipients TYPE rstirep,
    lv_tabix      TYPE sytabix,
    lv_text       LIKE rstirec-rtext,
    gs_config     TYPE smp_dyntxt,
    gs_rstirec    TYPE rstirec,
    lt_t002c      TYPE TABLE OF t002c,
    ls_t002c      TYPE          t002c,
    lt_trsti      TYPE TABLE OF trsti,
    ls_trsti      TYPE          trsti,
    lt_trstit     TYPE TABLE OF trstit,
    ls_trstit     TYPE          trstit,
    lv_sernr      TYPE          trsti-sernr.


  CHECK lv_trsti_read IS INITIAL.
  lv_trsti_read = 'X'.

  SELECT * FROM t002c INTO TABLE lt_t002c.

* entries exist?
  SELECT SINGLE * FROM trsti INTO ls_trsti
                  WHERE stool = 'RT'
                    AND fccls = '9'
                    AND sotyp = '1'
                    AND sonam = sy-repid.
  IF sy-subrc = 0.
*   all languages?
    LOOP AT lt_t002c INTO ls_t002c.

      SELECT SINGLE * FROM trstit INTO ls_trstit
                      WHERE langu =  ls_t002c-spras
                        AND stool = 'RT'
                        AND fccls = '9'
                        AND sotyp = '1'
                        AND sonam = sy-repid.
      IF sy-subrc = 0.
        DELETE lt_t002c.
      ENDIF.

    ENDLOOP.
  ENDIF.

*
  IF lt_t002c[] IS INITIAL.
    EXIT.
  ENDIF.
* create all entries of missing languages
  CLEAR: ls_t002c, ls_trsti, ls_trstit.
  REFRESH: lt_trsti, lt_trstit.

  DATA lt_textpool TYPE TABLE OF textpool.
  FIELD-SYMBOLS <ls_textpool> TYPE textpool.
  DATA: BEGIN OF lt_spras_text OCCURS 0,
          spras  TYPE sylangu,
          id     TYPE textpool-id,
          key    TYPE textpool-key,
          entry  TYPE textpool-entry,
          length TYPE textpool-length,
        END OF lt_spras_text.
  DATA lv_langu TYPE sylangu.
  DATA: program LIKE sy-repid VALUE '/CWM/STOCK_OVERVIEW'. " not RM07MLBS !!!

  LOOP AT lt_t002c INTO ls_t002c.
    REFRESH lt_textpool.
    READ TEXTPOOL program INTO lt_textpool LANGUAGE ls_t002c-spras.
    LOOP AT lt_textpool ASSIGNING <ls_textpool>
      WHERE id = 'I' AND key CP 'B0*'.
      MOVE-CORRESPONDING <ls_textpool> TO lt_spras_text.
      lt_spras_text-spras = ls_t002c-spras.
      APPEND lt_spras_text.
    ENDLOOP.
  ENDLOOP.

* exit if no texts exist
  IF lt_spras_text[] IS INITIAL.
    EXIT.
  ENDIF.

  DEFINE set_rep.
    CLEAR ls_trsti.
    ls_trsti-stool = 'RT'.
    ls_trsti-fccls = '9'.
    ls_trsti-sotyp = '1'.
    ls_trsti-sonam = lv_repid.
    ADD 1 TO lv_sernr.
    ls_trsti-sernr = lv_sernr.
    ls_trsti-rotyp = '1'.
    ls_trsti-rtool = &1.
    ls_trsti-ronam = &2.
    ls_trsti-rvari = &3.
    APPEND ls_trsti TO lt_trsti.
    LOOP AT lt_spras_text
      WHERE key = &4.
      MOVE-CORRESPONDING ls_trsti TO ls_trstit.
      MOVE lt_spras_text-spras TO ls_trstit-langu.
      MOVE lt_spras_text-entry TO ls_trstit-text.
      APPEND ls_trstit TO lt_trstit.
    ENDLOOP.
  END-OF-DEFINITION.
  lv_repid = sy-repid.

**
*    Texte:                        Form
* B00 Materialbelegliste            RR_MAT_STOCK (MB51)
* B01 Transitbestand                RR_TRANSIT_STOCK(rm07mtrb)
* B02 Zentrallager(WM)              RR_WM (LS26)
* B03 Handling Units                RR_HANDLING_UNIT(HUMO)
* B04 Reservierungsliste Bestandsf. RR_RESERVATION  (rm07resl)
* B05 Akt. Bedarfs- u. Bestandsl.   RR_REQUIREMENT  (MD04)
* B06 Einkaufsbelege                RR_PURCHASINGDOC (RM06EM00)
* B07 Material Serialnummern        RR_SERIALNUMBER (IQ09)
* B08 Produktionsaufträge           RR_PRODUCTION_ORDER
* B09 Prozessaufträge               RR_PROCESS_ORDER

*         RTOOL     RONAM                   RVARI          TEXT
  set_rep 'RT'      '/CWM/STOCK_OVERVIEW'  'SAP&_BRANCH_0' 'B00'."MB51
  set_rep 'RT'      '/CWM/STOCK_OVERVIEW'  'SAP&_BRANCH_1' 'B01'."rm07mtrb
  set_rep 'RT'      '/CWM/STOCK_OVERVIEW'  'SAP&_BRANCH_2' 'B02'."LS26
  set_rep 'RT'      '/CWM/STOCK_OVERVIEW'  'SAP&_BRANCH_3' 'B03'."HUMO
  set_rep 'RT'      '/CWM/STOCK_OVERVIEW'  'SAP&_BRANCH_4' 'B04'."rm07resl
  set_rep 'RT'      '/CWM/STOCK_OVERVIEW'  'SAP&_BRANCH_5' 'B05'."MD04
  set_rep 'RT'      '/CWM/STOCK_OVERVIEW'  'SAP&_BRANCH_6' 'B06'."RM06EM00
  set_rep 'RT'      '/CWM/STOCK_OVERVIEW'  'SAP&_BRANCH_7' 'B07'."IQ09
  set_rep 'RT'      '/CWM/STOCK_OVERVIEW'  'SAP&_BRANCH_8' 'B08'."PPIO_ENTRY
  set_rep 'RT'      '/CWM/STOCK_OVERVIEW'  'SAP&_BRANCH_9' 'B09'."PPIO_ENTRY

*  INSERT trsti FROM TABLE lt_trsti ACCEPTING DUPLICATE KEYS.
*  INSERT trstit FROM TABLE lt_trstit ACCEPTING DUPLICATE KEYS.
  MODIFY trsti FROM TABLE lt_trsti.
  MODIFY trstit FROM TABLE lt_trstit.

*  IF lv_text = text-b01.
*    lv_text = text-b01.
*    lv_text = text-b02.
*    lv_text = text-b03.
*    lv_text = text-b04.
*    lv_text = text-b05.
*    lv_text = text-b06.
*    lv_text = text-b07.
*    lv_text = text-b08.
*    lv_text = text-b09.
*  ENDIF.

ENDFORM.                    " /cwm/set_default_trsti

ENHANCEMENT-POINT ehp604_rm07mlbs_42 SPOTS es_rm07mlbs STATIC .

*&--------------------------------------------------------------------*
*&      Form  data_selection_join
*&---------------------------------------------------------------------*
FORM data_selection_join .

  TYPES: BEGIN OF ty_collector_joined,
           matnr LIKE mara-matnr,
           werks LIKE t001w-werks,
           lgort LIKE mard-lgort,
           sobkz LIKE mkol-sobkz,
           bwtar LIKE mcha-bwtar,
           pspnr LIKE  mspr-pspnr,
           vbeln LIKE  mska-vbeln,
           posnr LIKE  mska-posnr,
           lifnr LIKE mslb-lifnr,
           kunnr LIKE msku-kunnr,
           lvorm LIKE  mard-lvorm,
           kzbws LIKE mssa-kzbws,
           charg LIKE mchb-charg,
           labst LIKE mard-labst,
           insme LIKE mard-insme,
           speme LIKE mard-speme,
           einme LIKE mard-einme,
           retme LIKE mard-retme,
           umlme LIKE mard-umlme,
           bwesb LIKE  marc-bwesb,
           glgmg LIKE marc-glgmg,
           trame LIKE marc-trame,
           umlmc LIKE marc-umlmc,
           maktx LIKE makt-maktx,
           xchar LIKE marc-xchar,
           salk3 TYPE p LENGTH 16 DECIMALS 2,
           lbkum TYPE p LENGTH 16 DECIMALS 3.
  TYPES: END OF ty_collector_joined.

  DATA: collector_joined TYPE TABLE OF ty_collector_joined.

  DATA: collector_joined_mstb TYPE TABLE OF ty_collector_joined.
  DATA: collector_joined_uml TYPE TABLE OF ty_collector_joined.
  DATA: collector_joined_mard TYPE TABLE OF ty_collector_joined.

  FIELD-SYMBOLS: <fs_collector_uml> LIKE LINE OF collector_joined_uml.
  FIELD-SYMBOLS: <fs_collector_mstb> LIKE LINE OF collector_joined_mstb.
  FIELD-SYMBOLS: <fs_collector_mard> LIKE LINE OF collector_joined_mard.
  FIELD-SYMBOLS: <fs_collector_joined> LIKE LINE OF collector_joined.

* Materials to be processed
  TYPES: BEGIN OF ty_mat,
           matnr      LIKE mara-matnr,
           werks      LIKE marc-werks,
           xchar      LIKE marc-xchar,
           mtart      LIKE mara-mtart,
           matkl      LIKE mara-matkl,
           meins      LIKE mara-meins,
           trame      LIKE marc-trame,
           umlmc      LIKE marc-umlmc,
           glgmg      LIKE marc-glgmg,                      "n912093
           bwesb      LIKE marc-bwesb,                      "AC0K020254
           lvorm_mara LIKE mara-lvorm,
           lvorm_marc LIKE marc-lvorm,
           maktx      LIKE makt-maktx.                      "n1795093
  TYPES:  END OF ty_mat.

  DATA: t_mat TYPE ty_mat OCCURS 0 WITH HEADER LINE.

  DATA: r_stopt   TYPE RANGE OF labst,
        lsr_stopt LIKE LINE OF  r_stopt.

  DATA: factor TYPE float.

  DATA: l_s_mat   TYPE ty_mat,
        l_f_matnr LIKE makt-matnr.

  DATA: lv_exists TYPE mandt.

************************************************************************
* Read material master data (MARA and MARC)
************************************************************************
  REFRESH collector_joined.

* evaluate check boxes...
  IF ( negativ = 'X' AND nozero = 'X' ) OR negativ = 'X'.
    lsr_stopt-sign   = 'I'.
    lsr_stopt-option = 'LT'. "less than
    lsr_stopt-low    = '0'.
    APPEND lsr_stopt TO r_stopt.
  ELSEIF nozero = 'X'.
    lsr_stopt-sign   = 'I'.
    lsr_stopt-option = 'GT'. "greater than
    lsr_stopt-low    = '0'.
    APPEND lsr_stopt TO r_stopt.
    lsr_stopt-sign   = 'I'.
    lsr_stopt-option = 'LT'. "less than
    lsr_stopt-low    = '0'.
    APPEND lsr_stopt TO r_stopt.
  ELSE.
    REFRESH r_stopt.
  ENDIF.

  BREAK-POINT ID mmim_rep_mb52.

* Get MARC master data plus maktx
  CLEAR t_mat[].

  SELECT mara~matnr werks xchar mtart matkl meins maktx
         mara~lvorm AS lvorm_mara
         marc~lvorm AS lvorm_marc
  CONNECTION (dbcon)
       INTO CORRESPONDING FIELDS OF TABLE t_mat
       FROM mara
       INNER JOIN marc
          ON mara~matnr = marc~matnr
       LEFT JOIN makt
          ON makt~matnr = mara~matnr AND
             makt~spras = sy-langu
       WHERE mara~matnr IN matnr
         AND werks IN werks
         AND mtart IN matart
         AND matkl IN matkla
         AND ekgrp IN ekgrup
       GROUP BY mara~matnr werks xchar mtart matkl meins mara~lvorm marc~lvorm maktx.

**********************************************************************
* Get stock and corresponding valuation
**********************************************************************
************************************************************************
* Get "normal" stocks.
* If no detailed batch display is required,
* all data come from MARD. Otherwise, materials with batch
* management are extracted from MCHB, the rest from MARD.
************************************************************************
* NO BATCH
  READ TABLE t_mat TRANSPORTING NO FIELDS WITH KEY xchar = space." BINARY SEARCH.
  IF sy-subrc = 0.
    SELECT mard~mandt, mard~matnr, mard~werks, mard~lgort, mard~lvorm,
         SUM( mard~labst ) AS labst, SUM( mard~umlme ) AS umlme, SUM( mard~insme ) AS insme, SUM( mard~einme ) AS einme, SUM( mard~speme ) AS speme, SUM( mard~retme ) AS retme,
          SUM( salk3 ) AS salk3, SUM( lbkum ) AS lbkum
      CONNECTION (dbcon)
      FROM mard
        INNER JOIN marc ON mard~matnr = marc~matnr AND mard~werks = marc~werks
        INNER JOIN mara ON mard~matnr = mara~matnr
        INNER JOIN t001w ON mard~werks = t001w~werks
        LEFT OUTER JOIN mbew ON t001w~bwkey = mbew~bwkey AND mard~matnr = mbew~matnr AND mbew~bwtar = ''
      INTO CORRESPONDING FIELDS OF TABLE @collector_joined_mard
      WHERE mard~matnr IN @matnr
        AND mard~werks IN @werks
        AND lgort IN @lgort
        AND xchar EQ @space
        AND ( labst IN @r_stopt
         OR   umlme IN @r_stopt
         OR   insme IN @r_stopt
         OR   einme IN @r_stopt
         OR   speme IN @r_stopt
         OR   retme IN @r_stopt )
        AND marc~ekgrp IN @ekgrup
        AND mara~matkl IN @matkla
        AND mara~mtart IN @matart
        GROUP BY mard~mandt, mard~matnr, mard~werks, mard~lgort, mard~lvorm.
  ENDIF.

  IF pa_sond IS NOT INITIAL.
    LOOP AT collector_joined_mard ASSIGNING <fs_collector_mard> WHERE lvorm IS NOT INITIAL.
      MOVE-CORRESPONDING <fs_collector_mard> TO g_s_mard_lv.
      INSERT g_s_mard_lv INTO TABLE g_t_mard_lv.
    ENDLOOP.
  ENDIF.

  APPEND LINES OF collector_joined_mard TO collector_joined.

**********************************************************************
* Access MCHB
**********************************************************************
* BATCH
  READ TABLE t_mat TRANSPORTING NO FIELDS WITH KEY xchar = 'X'." BINARY SEARCH.
  IF sy-subrc = 0.
    IF xmchb IS NOT INITIAL.

      SELECT mchb~matnr, mchb~werks, mchb~lgort, mchb~lvorm, mchb~charg,
         SUM( mchb~clabs ) AS labst, SUM( mchb~cumlm ) AS umlme, SUM( mchb~cinsm ) AS insme, SUM( mchb~ceinm ) AS einme, SUM( mchb~cspem ) AS speme, SUM( mchb~cretm ) AS retme,
         SUM( salk3 ) AS salk3, SUM( lbkum ) AS lbkum
      CONNECTION (dbcon)
      FROM mchb
        INNER JOIN marc ON mchb~matnr = marc~matnr AND mchb~werks = marc~werks
        INNER JOIN mara ON mchb~matnr = mara~matnr
        INNER JOIN t001w ON mchb~werks = t001w~werks
        LEFT OUTER JOIN mcha ON mchb~matnr = mcha~matnr AND mchb~werks = mcha~werks AND mchb~charg = mcha~charg
        LEFT OUTER JOIN mbew ON t001w~bwkey = mbew~bwkey AND mchb~matnr = mbew~matnr AND mbew~bwtar = mcha~bwtar
      APPENDING CORRESPONDING FIELDS OF TABLE @collector_joined
      WHERE mchb~matnr IN @matnr
        AND mchb~werks IN @werks
        AND lgort IN @lgort
        AND mchb~charg IN @charg
        AND xchar = 'X'
        AND ( clabs IN @r_stopt
         OR   cumlm IN @r_stopt
         OR   cinsm IN @r_stopt
         OR   ceinm IN @r_stopt
         OR   cspem IN @r_stopt
         OR   cretm IN @r_stopt )
        AND marc~ekgrp IN @ekgrup
        AND mara~matkl IN @matkla
        AND mara~mtart IN @matart
      GROUP BY mchb~matnr, mchb~werks, mchb~lgort, mchb~lvorm, mchb~charg.

    ELSE.

      SELECT mchb~matnr, mchb~werks, mchb~lgort, mchb~lvorm,
             SUM( mchb~clabs ) AS labst, SUM( mchb~cumlm ) AS umlme, SUM( mchb~cinsm ) AS insme, SUM( mchb~ceinm ) AS einme, SUM( mchb~cspem ) AS speme, SUM( mchb~cretm ) AS retme,
             SUM( salk3 ) AS salk3, SUM( lbkum ) AS lbkum
        CONNECTION (dbcon)
        FROM mchb
          INNER JOIN marc ON mchb~matnr = marc~matnr AND mchb~werks = marc~werks
          INNER JOIN mara ON mchb~matnr = mara~matnr
          INNER JOIN t001w ON mchb~werks = t001w~werks
          LEFT OUTER JOIN mbew ON t001w~bwkey = mbew~bwkey AND mchb~matnr = mbew~matnr AND mbew~bwtar = ''
        APPENDING CORRESPONDING FIELDS OF TABLE @collector_joined
        WHERE mchb~matnr IN @matnr
          AND mchb~werks IN @werks
          AND lgort IN @lgort
          AND xchar = 'X'
          AND ( clabs IN @r_stopt
           OR   cumlm IN @r_stopt
           OR   cinsm IN @r_stopt
           OR   ceinm IN @r_stopt
           OR   cspem IN @r_stopt
           OR   cretm IN @r_stopt )
          AND marc~ekgrp IN @ekgrup
          AND mara~matkl IN @matkla
          AND mara~mtart IN @matart
        GROUP BY mchb~matnr, mchb~werks, mchb~lgort, mchb~lvorm.

    ENDIF.
  ENDIF.

**********************************************************************
* Access MARC
**********************************************************************
  IF xmchb IS NOT INITIAL.
    IF g_flag_suppress_init_lgort IS INITIAL.

      SELECT DISTINCT marc~matnr, marc~werks, marc~lvorm,
            trame AS trame, umlmc AS umlmc, marc~bwesb AS bwesb, glgmg AS glgmg,
            SUM( salk3 ) AS salk3, SUM( lbkum ) AS lbkum
      CONNECTION (dbcon)
      FROM marc
        INNER JOIN mara ON marc~matnr = mara~matnr
        INNER JOIN t001w ON marc~werks = t001w~werks
        LEFT OUTER JOIN mbew ON t001w~bwkey = mbew~bwkey AND marc~matnr = mbew~matnr
      INTO CORRESPONDING FIELDS OF TABLE @collector_joined_uml
      WHERE ( trame <> 0 OR umlmc <> 0 OR bwesb <> 0 OR glgmg <> 0 )
            AND marc~matnr IN @matnr
            AND marc~werks IN @werks
            AND marc~ekgrp IN @ekgrup
            AND mara~matkl IN @matkla
            AND mara~mtart IN @matart
            AND ( trame IN @r_stopt
             OR   umlmc IN @r_stopt
             OR   glgmg IN @r_stopt )
            AND ( mbew~bwtar <> '' OR marc~bwtty = '' )
      GROUP BY marc~mandt, marc~matnr, marc~werks, marc~lvorm, trame, umlmc, bwesb, glgmg.

    ENDIF.
  ELSE.
    IF g_flag_suppress_init_lgort IS INITIAL.

      "sums without batch from MCHB
      SELECT marc~matnr, marc~werks,  marc~lvorm,
             trame AS trame, umlmc AS umlmc, marc~bwesb AS bwesb, glgmg AS glgmg,
             SUM( salk3 ) AS salk3, SUM( lbkum ) AS lbkum
        CONNECTION (dbcon)
        FROM marc
          INNER JOIN mara ON marc~matnr = mara~matnr
          INNER JOIN t001w ON marc~werks = t001w~werks
          LEFT OUTER JOIN mbew ON t001w~bwkey = mbew~bwkey AND marc~matnr = mbew~matnr
        INTO CORRESPONDING FIELDS OF TABLE @collector_joined_uml
        WHERE ( trame <> 0 OR umlmc <> 0 OR bwesb <> 0 OR glgmg <> 0 )
          AND marc~matnr IN @matnr
          AND marc~werks IN @werks
          AND marc~ekgrp IN @ekgrup
          AND mara~matkl IN @matkla
          AND mara~mtart IN @matart
          AND ( trame IN @r_stopt
           OR   umlmc IN @r_stopt
           OR   glgmg IN @r_stopt
           OR   bwesb IN @r_stopt )
          AND ( mbew~bwtar <> '' OR marc~bwtty = '' )
        GROUP BY marc~mandt, marc~matnr, marc~werks, marc~lvorm, trame, umlmc, bwesb, glgmg.

    ENDIF.
  ENDIF.

**********************************************************************
* select additionally valuated SiT & GR blocked stock details
**********************************************************************
  SELECT SINGLE mandt FROM mstb CONNECTION (dbcon) INTO lv_exists.
  IF sy-subrc IS INITIAL. "at least one entry found: continue

    IF xmchb IS NOT INITIAL.
*     batch & bwtar details
      SELECT mstb~matnr, mstb~werks, mstb~charg, mstb~sobkz,
           SUM( mstb~cwesb ) AS bwesb,
           SUM( salk3 ) AS salk3, SUM( lbkum ) AS lbkum
        CONNECTION (dbcon)
        FROM mstb
          INNER JOIN mara ON mstb~matnr = mara~matnr
          INNER JOIN marc ON mstb~matnr = marc~matnr AND mstb~werks = marc~werks
          INNER JOIN t001w ON mstb~werks = t001w~werks
          LEFT OUTER JOIN mbew ON t001w~bwkey = mbew~bwkey AND mstb~matnr = mbew~matnr AND mstb~bwtar = mbew~bwtar
          INTO CORRESPONDING FIELDS OF TABLE @collector_joined_mstb
          WHERE mstb~matnr IN @matnr
            AND mstb~werks IN @werks
            AND mstb~charg IN @charg
            AND mstb~sobkz IN @so_sobkz
            AND mstb~cwesb IN @r_stopt
            AND marc~ekgrp IN @ekgrup
            AND mara~matkl IN @matkla
            AND mara~mtart IN @matart
        GROUP BY mstb~matnr, mstb~werks, mstb~charg, mstb~sobkz.

    ELSE.

*     with batch/bwtar in total
      SELECT mstb~matnr, mstb~werks,
             SUM( mstb~cwesb ) AS bwesb,
             SUM( salk3 ) AS salk3, SUM( lbkum ) AS lbkum
        CONNECTION (dbcon)
        FROM mstb
          INNER JOIN mara ON mstb~matnr = mara~matnr
          INNER JOIN marc ON mstb~matnr = marc~matnr AND mstb~werks = marc~werks
          INNER JOIN t001w ON mstb~werks = t001w~werks
          LEFT OUTER JOIN mbew ON t001w~bwkey = mbew~bwkey AND mstb~matnr = mbew~matnr AND mbew~bwtar = ''
        INTO CORRESPONDING FIELDS OF TABLE @collector_joined_mstb
        WHERE mstb~matnr IN @matnr
          AND mstb~werks IN @werks
          AND mstb~sobkz IN @so_sobkz
          AND mstb~cwesb IN @r_stopt
          AND marc~ekgrp IN @ekgrup
          AND mara~matkl IN @matkla
          AND mara~mtart IN @matart
        GROUP BY mstb~matnr, mstb~werks, matkl, mtart, ekgrp.

    ENDIF.
  ENDIF.

************************************************************************
* merge valuated stock in transit from marc/mstb to avoid 2 lines for
* plant stocks
************************************************************************
  IF xmchb IS NOT INITIAL.

    SORT collector_joined_mstb BY matnr werks.
    LOOP AT collector_joined_uml ASSIGNING <fs_collector_uml>.

      <fs_collector_uml>-umlme = <fs_collector_uml>-umlmc + <fs_collector_uml>-trame.

      READ TABLE collector_joined_mstb TRANSPORTING NO FIELDS
                                       WITH KEY matnr = <fs_collector_uml>-matnr
                                                werks = <fs_collector_uml>-werks.
      IF sy-subrc = 0.
        CLEAR <fs_collector_uml>-bwesb.
      ENDIF.
    ENDLOOP.

  ELSE.

    LOOP AT collector_joined_uml ASSIGNING <fs_collector_uml>.

      <fs_collector_uml>-umlme = <fs_collector_uml>-umlmc + <fs_collector_uml>-trame.

      READ TABLE collector_joined_mstb ASSIGNING <fs_collector_mstb>
                                       WITH KEY matnr = <fs_collector_uml>-matnr
                                                werks = <fs_collector_uml>-werks.
      IF sy-subrc = 0.
        <fs_collector_uml>-bwesb = <fs_collector_mstb>-bwesb.
        DELETE TABLE collector_joined_mstb FROM <fs_collector_mstb>.
      ENDIF.
    ENDLOOP.


  ENDIF.

  APPEND LINES OF collector_joined_uml TO collector_joined.
  FREE collector_joined_uml.
  APPEND LINES OF collector_joined_mstb TO collector_joined.
  FREE collector_joined_mstb.

************************************************************************
* Consignment from vendor (MKOL)
* Read only if requested by one of the
* flags on the selection screen. Absolutely inconsistent, but
* due to compatibility...
* MKOL has a flag for deletion
************************************************************************

  IF NOT pa_sond IS INITIAL AND
     NOT t_mat[] IS INITIAL AND
     ( 'K' IN so_sobkz  OR
       'M' IN so_sobkz ).
    SELECT SINGLE mandt FROM mkol CONNECTION (dbcon) INTO lv_exists.
    IF sy-subrc IS INITIAL. "at least one entry found: continue

      IF xmchb IS NOT INITIAL.
*       sum with batch
        SELECT mkol~matnr, mkol~werks, mkol~lgort, mkol~lvorm, mkol~charg, mkol~sobkz, mkol~lifnr,
               SUM( slabs ) AS labst, SUM( sinsm ) AS insme, SUM( seinm ) AS einme, SUM( sspem ) AS speme
          CONNECTION (dbcon)
          FROM mkol
            INNER JOIN mara ON mkol~matnr = mara~matnr
            INNER JOIN marc ON mkol~matnr = marc~matnr AND mkol~werks = marc~werks
            INNER JOIN t001w ON mkol~werks = t001w~werks
          APPENDING CORRESPONDING FIELDS OF TABLE @collector_joined
          WHERE mkol~matnr IN @matnr
            AND mkol~werks IN @werks
            AND lgort IN @lgort
            AND mkol~charg IN @charg
            AND sobkz IN @so_sobkz
            AND ( slabs IN @r_stopt
             OR   sinsm IN @r_stopt
             OR   seinm IN @r_stopt
             OR   sspem IN @r_stopt )
            AND marc~ekgrp IN @ekgrup
            AND mara~matkl IN @matkla
            AND mara~mtart IN @matart
          GROUP BY mkol~matnr, mkol~werks, mkol~lgort, mkol~lvorm, mkol~charg, mkol~sobkz, mkol~lifnr.

      ELSE.
*       sum without batch
        SELECT mkol~matnr, mkol~werks, mkol~lgort, mkol~lvorm, mkol~sobkz, mkol~lifnr,
               SUM( slabs ) AS labst, SUM( sinsm ) AS insme, SUM( seinm ) AS einme, SUM( sspem ) AS speme
          CONNECTION (dbcon)
          FROM mkol
            INNER JOIN mara ON mkol~matnr = mara~matnr
            INNER JOIN marc ON mkol~matnr = marc~matnr AND mkol~werks = marc~werks
            INNER JOIN t001w ON mkol~werks = t001w~werks
          APPENDING CORRESPONDING FIELDS OF TABLE @collector_joined
          WHERE mkol~matnr IN @matnr
            AND mkol~werks IN @werks
            AND lgort IN @lgort
            AND sobkz IN @so_sobkz
            AND ( slabs IN @r_stopt
             OR   sinsm IN @r_stopt
             OR   seinm IN @r_stopt
             OR   sspem IN @r_stopt )
            AND marc~ekgrp IN @ekgrup
            AND mara~matkl IN @matkla
            AND mara~mtart IN @matart
          GROUP BY mkol~matnr, mkol~werks, mkol~lgort, mkol~lvorm, mkol~sobkz, mkol~lifnr.

      ENDIF.
    ENDIF.
  ENDIF.

************************************************************************
* Special stocks at customer side (MSKU)
* MSKU has no flag for deletion
************************************************************************

  IF  NOT  pa_sond IS INITIAL AND
      NOT  t_mat[] IS INITIAL AND
    ( 'V' IN so_sobkz  OR
      'W' IN so_sobkz ).
    SELECT SINGLE mandt FROM msku CONNECTION (dbcon) INTO lv_exists.
    IF sy-subrc IS INITIAL. "at least one entry found: continue

      IF xmchb IS NOT INITIAL.
*       sum with batch
        SELECT msku~matnr, msku~werks, msku~charg, msku~sobkz, msku~kunnr,
               SUM( kulab ) AS labst, SUM( kuuml ) AS umlme, SUM( kuins ) AS insme, SUM( kuein ) AS einme,
               SUM( salk3 ) AS salk3, SUM( lbkum ) AS lbkum
          CONNECTION (dbcon)
          FROM msku
            INNER JOIN mara ON msku~matnr = mara~matnr
            INNER JOIN marc ON msku~matnr = marc~matnr AND msku~werks = marc~werks
            INNER JOIN t001w ON msku~werks = t001w~werks
            LEFT OUTER JOIN mcha ON msku~matnr = mcha~matnr AND msku~werks = mcha~werks AND msku~charg = mcha~charg
            LEFT OUTER JOIN mbew ON t001w~bwkey = mbew~bwkey AND msku~matnr = mbew~matnr AND mcha~bwtar = mbew~bwtar
          APPENDING CORRESPONDING FIELDS OF TABLE @collector_joined
          WHERE xchar = 'X'
            AND msku~matnr IN @matnr
            AND msku~werks IN @werks
            AND msku~charg IN @charg
            AND msku~sobkz IN @so_sobkz
            AND ( kulab IN @r_stopt
             OR   kuins IN @r_stopt
             OR   kuein IN @r_stopt
             OR   kuuml IN @r_stopt )
            AND marc~ekgrp IN @ekgrup
            AND mara~matkl IN @matkla
            AND mara~mtart IN @matart
          GROUP BY msku~matnr, msku~werks, msku~charg, msku~sobkz, msku~kunnr.

        SELECT msku~matnr, msku~werks, msku~charg, msku~sobkz, msku~kunnr,
               SUM( kulab ) AS labst, SUM( kuuml ) AS umlme, SUM( kuins ) AS insme, SUM( kuein ) AS einme,
               SUM( salk3 ) AS salk3, SUM( lbkum ) AS lbkum
          CONNECTION (dbcon)
          FROM msku
            INNER JOIN mara ON msku~matnr = mara~matnr
            INNER JOIN marc ON msku~matnr = marc~matnr AND msku~werks = marc~werks
            INNER JOIN t001w ON msku~werks = t001w~werks
            LEFT OUTER JOIN mbew ON t001w~bwkey = mbew~bwkey AND msku~matnr = mbew~matnr AND mbew~bwtar = ''
          APPENDING CORRESPONDING FIELDS OF TABLE @collector_joined
          WHERE xchar = ''
            AND msku~matnr IN @matnr
            AND msku~werks IN @werks
            AND msku~charg IN @charg
            AND msku~sobkz IN @so_sobkz
            AND ( kulab IN @r_stopt
             OR   kuins IN @r_stopt
             OR   kuein IN @r_stopt
             OR   kuuml IN @r_stopt )
            AND marc~ekgrp IN @ekgrup
            AND mara~matkl IN @matkla
            AND mara~mtart IN @matart
          GROUP BY msku~matnr, msku~werks, msku~charg, msku~sobkz, msku~kunnr.

      ELSE.
*       sum without batch
        SELECT msku~matnr, msku~werks, msku~sobkz, msku~kunnr,
               SUM( kulab ) AS labst, SUM( kuuml ) AS umlme, SUM( kuins ) AS insme, SUM( kuein ) AS einme,
               SUM( salk3 ) AS salk3, SUM( lbkum ) AS lbkum
          CONNECTION (dbcon)
          FROM msku
            INNER JOIN mara ON msku~matnr = mara~matnr
            INNER JOIN marc ON msku~matnr = marc~matnr AND msku~werks = marc~werks
            INNER JOIN t001w ON msku~werks = t001w~werks
            LEFT OUTER JOIN mcha ON msku~matnr = mcha~matnr AND msku~werks = mcha~werks AND msku~charg = mcha~charg
            LEFT OUTER JOIN mbew ON t001w~bwkey = mbew~bwkey AND msku~matnr = mbew~matnr AND mcha~bwtar = mbew~bwtar
          APPENDING CORRESPONDING FIELDS OF TABLE @collector_joined
          WHERE xchar = 'X'
            AND msku~matnr IN @matnr
            AND msku~werks IN @werks
            AND msku~sobkz IN @so_sobkz
            AND ( kulab IN @r_stopt
             OR   kuins IN @r_stopt
             OR   kuein IN @r_stopt
             OR   kuuml IN @r_stopt )
            AND marc~ekgrp IN @ekgrup
            AND mara~matkl IN @matkla
            AND mara~mtart IN @matart
          GROUP BY msku~matnr, msku~werks, msku~sobkz, msku~kunnr.

        SELECT msku~matnr, msku~werks, msku~sobkz, msku~kunnr,
               SUM( kulab ) AS labst, SUM( kuuml ) AS umlme, SUM( kuins ) AS insme, SUM( kuein ) AS einme,
               SUM( salk3 ) AS salk3, SUM( lbkum ) AS lbkum
          CONNECTION (dbcon)
          FROM msku
            INNER JOIN mara ON msku~matnr = mara~matnr
            INNER JOIN marc ON msku~matnr = marc~matnr AND msku~werks = marc~werks
            INNER JOIN t001w ON msku~werks = t001w~werks
            LEFT OUTER JOIN mbew ON t001w~bwkey = mbew~bwkey AND msku~matnr = mbew~matnr AND mbew~bwtar = ''
          APPENDING CORRESPONDING FIELDS OF TABLE @collector_joined
          WHERE xchar = ''
            AND msku~matnr IN @matnr
            AND msku~werks IN @werks
            AND msku~sobkz IN @so_sobkz
            AND ( kulab IN @r_stopt
             OR   kuins IN @r_stopt
             OR   kuein IN @r_stopt
             OR   kuuml IN @r_stopt )
            AND marc~ekgrp IN @ekgrup
            AND mara~matkl IN @matkla
            AND mara~mtart IN @matart
          GROUP BY msku~matnr, msku~werks, msku~sobkz, msku~kunnr.

      ENDIF.
    ENDIF.
  ENDIF.

************************************************************************
* Special stocks at vendor provision (MSLB)
* MSLB has no flag for deletion
************************************************************************

  IF NOT pa_sond IS INITIAL  AND
     NOT t_mat[] IS INITIAL  AND
     'O'     IN so_sobkz AND
    g_flag_suppress_init_lgort IS INITIAL.

    SELECT SINGLE mandt FROM mslb CONNECTION (dbcon) INTO lv_exists.
    IF sy-subrc IS INITIAL. "at least one entry found: continue

      IF xmchb IS NOT INITIAL.
*       sum with batch
        SELECT mslb~matnr, mslb~werks, mslb~charg, mslb~sobkz, mslb~lifnr,
               SUM( lblab ) AS labst, SUM( lbuml ) AS umlme, SUM( lbins ) AS insme, SUM( lbein ) AS einme,
               SUM( salk3 ) AS salk3, SUM( lbkum ) AS lbkum
          CONNECTION (dbcon)
          FROM mslb
            INNER JOIN mara ON mslb~matnr = mara~matnr
            INNER JOIN marc ON mslb~matnr = marc~matnr AND mslb~werks = marc~werks
            INNER JOIN t001w ON mslb~werks = t001w~werks
            LEFT OUTER JOIN mcha ON mslb~matnr = mcha~matnr AND mslb~werks = mcha~werks AND mslb~charg = mcha~charg
            LEFT OUTER JOIN mbew ON t001w~bwkey = mbew~bwkey AND mslb~matnr = mbew~matnr AND mcha~bwtar = mbew~bwtar
          APPENDING CORRESPONDING FIELDS OF TABLE @collector_joined
          WHERE xchar = 'X'
            AND mslb~matnr IN @matnr
            AND mslb~werks IN @werks
            AND mslb~charg IN @charg
            AND mslb~sobkz IN @so_sobkz
            AND ( lblab IN @r_stopt
             OR   lbins IN @r_stopt
             OR   lbein IN @r_stopt
             OR   lbuml IN @r_stopt )
             AND marc~ekgrp IN @ekgrup
             AND mara~matkl IN @matkla
             AND mara~mtart IN @matart
          GROUP BY mslb~matnr, mslb~werks, mslb~charg, mslb~sobkz, mslb~lifnr.

        SELECT mslb~matnr, mslb~werks, mslb~charg, mslb~sobkz, mslb~lifnr,
               SUM( lblab ) AS labst, SUM( lbuml ) AS umlme, SUM( lbins ) AS insme, SUM( lbein ) AS einme,
               SUM( salk3 ) AS salk3, SUM( lbkum ) AS lbkum
          CONNECTION (dbcon)
          FROM mslb
            INNER JOIN mara ON mslb~matnr = mara~matnr
            INNER JOIN marc ON mslb~matnr = marc~matnr AND mslb~werks = marc~werks
            INNER JOIN t001w ON mslb~werks = t001w~werks
            LEFT OUTER JOIN mbew ON t001w~bwkey = mbew~bwkey AND mslb~matnr = mbew~matnr AND mbew~bwtar = ''
          APPENDING CORRESPONDING FIELDS OF TABLE @collector_joined
          WHERE xchar = ''
            AND mslb~matnr IN @matnr
            AND mslb~werks IN @werks
            AND mslb~charg IN @charg
            AND mslb~sobkz IN @so_sobkz
            AND ( lblab IN @r_stopt
             OR   lbins IN @r_stopt
             OR   lbein IN @r_stopt
             OR   lbuml IN @r_stopt )
             AND marc~ekgrp IN @ekgrup
             AND mara~matkl IN @matkla
             AND mara~mtart IN @matart
          GROUP BY mslb~matnr, mslb~werks, mslb~charg, mslb~sobkz, mslb~lifnr.

      ELSE.
*       sum without batch
        SELECT mslb~matnr, mslb~werks, mslb~sobkz, mslb~lifnr,
               SUM( lblab ) AS labst, SUM( lbuml ) AS umlme, SUM( lbins ) AS insme, SUM( lbein ) AS einme,
               SUM( salk3 ) AS salk3, SUM( lbkum ) AS lbkum
          CONNECTION (dbcon)
          FROM mslb
            INNER JOIN mara ON mslb~matnr = mara~matnr
            INNER JOIN marc ON mslb~matnr = marc~matnr AND mslb~werks = marc~werks
            INNER JOIN t001w ON mslb~werks = t001w~werks
            LEFT OUTER JOIN mcha ON mslb~matnr = mcha~matnr AND mslb~werks = mcha~werks AND mslb~charg = mcha~charg
            LEFT OUTER JOIN mbew ON t001w~bwkey = mbew~bwkey AND mslb~matnr = mbew~matnr AND mbew~bwtar = ''
          APPENDING CORRESPONDING FIELDS OF TABLE @collector_joined
          WHERE xchar = 'X'
            AND mslb~matnr IN @matnr
            AND mslb~werks IN @werks
            AND mslb~sobkz IN @so_sobkz
            AND ( lblab IN @r_stopt
             OR   lbins IN @r_stopt
             OR   lbein IN @r_stopt
             OR   lbuml IN @r_stopt )
             AND marc~ekgrp IN @ekgrup
             AND mara~matkl IN @matkla
             AND mara~mtart IN @matart
          GROUP BY mslb~matnr, mslb~werks, mslb~sobkz, mslb~lifnr.

        SELECT mslb~matnr, mslb~werks, mslb~sobkz, mslb~lifnr,
               SUM( lblab ) AS labst, SUM( lbuml ) AS umlme, SUM( lbins ) AS insme, SUM( lbein ) AS einme,
               SUM( salk3 ) AS salk3, SUM( lbkum ) AS lbkum
          CONNECTION (dbcon)
          FROM mslb
            INNER JOIN mara ON mslb~matnr = mara~matnr
            INNER JOIN marc ON mslb~matnr = marc~matnr AND mslb~werks = marc~werks
            INNER JOIN t001w ON mslb~werks = t001w~werks
            LEFT OUTER JOIN mbew ON t001w~bwkey = mbew~bwkey AND mslb~matnr = mbew~matnr AND mbew~bwtar = ''
          APPENDING CORRESPONDING FIELDS OF TABLE @collector_joined
          WHERE xchar = ''
            AND mslb~matnr IN @matnr
            AND mslb~werks IN @werks
            AND mslb~sobkz IN @so_sobkz
            AND ( lblab IN @r_stopt
             OR   lbins IN @r_stopt
             OR   lbein IN @r_stopt
             OR   lbuml IN @r_stopt )
             AND marc~ekgrp IN @ekgrup
             AND mara~matkl IN @matkla
             AND mara~mtart IN @matart
          GROUP BY mslb~matnr, mslb~werks, mslb~sobkz, mslb~lifnr.

      ENDIF.
    ENDIF.
  ENDIF.

************************************************************************
* Customer order stock (MSKA) and sum segment (MSSA) for valuation.
* MSKA has no flag for deletion
************************************************************************

  IF NOT  pa_sond IS INITIAL  AND
     NOT  t_mat[] IS INITIAL  AND
         ( 'E' IN so_sobkz OR 'T' IN so_sobkz ).
    SELECT SINGLE mandt FROM mssa CONNECTION (dbcon) INTO lv_exists.
    IF sy-subrc IS INITIAL. "at least one entry found: continue

      IF xmchb IS NOT INITIAL.
*       First select stock valuated in MBEW
*       sum of all materials w/ batch
        SELECT mska~matnr, mska~werks, mska~lgort, mska~charg, mska~sobkz, mska~vbeln, mska~posnr, kzbws,
               SUM( kalab ) AS labst, SUM( kains ) AS insme, SUM( kaein ) AS einme, SUM( kaspe ) AS speme,
               SUM( salk3 ) AS salk3, SUM( lbkum ) AS lbkum
          CONNECTION (dbcon)
          FROM mska
            INNER JOIN mssa ON mska~matnr = mssa~matnr AND mska~werks = mssa~werks AND mska~sobkz = mssa~sobkz AND mska~vbeln = mssa~vbeln AND mska~posnr = mssa~posnr
            INNER JOIN mara ON mska~matnr = mara~matnr
            INNER JOIN marc ON mska~matnr = marc~matnr AND mska~werks = marc~werks
            INNER JOIN t001w ON mska~werks = t001w~werks
            LEFT OUTER JOIN mcha ON mska~matnr = mcha~matnr AND mska~werks = mcha~werks AND mska~charg = mcha~charg
            LEFT OUTER JOIN mbew ON t001w~bwkey = mbew~bwkey AND mska~matnr = mbew~matnr AND mcha~bwtar = mbew~bwtar
          APPENDING CORRESPONDING FIELDS OF TABLE @collector_joined
          WHERE kzbws = 'A'
            AND xchar = 'X'
            AND mska~matnr IN @matnr
            AND mska~werks IN @werks
            AND mska~lgort IN @lgort
            AND mska~sobkz IN @so_sobkz
            AND mska~charg IN @charg
            AND ( mska~kalab IN @r_stopt
             OR   mska~kains IN @r_stopt
             OR   mska~kaspe IN @r_stopt
             OR   mska~kaein IN @r_stopt )
            AND marc~ekgrp IN @ekgrup
            AND mara~matkl IN @matkla
            AND mara~mtart IN @matart
          GROUP BY mska~matnr, mska~werks, mska~lgort, mska~charg, mska~sobkz, mska~vbeln, mska~posnr, kzbws.

*       sum of all materials w/o batch
        SELECT mska~matnr, mska~werks, mska~lgort, mska~charg, mska~sobkz, mska~vbeln, mska~posnr, kzbws,
               SUM( kalab ) AS labst, SUM( kains ) AS insme, SUM( kaein ) AS einme, SUM( kaspe ) AS speme,
               SUM( salk3 ) AS salk3, SUM( lbkum ) AS lbkum
          CONNECTION (dbcon)
          FROM mska
            INNER JOIN mssa ON mska~matnr = mssa~matnr AND mska~werks = mssa~werks AND mska~sobkz = mssa~sobkz AND mska~vbeln = mssa~vbeln AND mska~posnr = mssa~posnr
            INNER JOIN mara ON mska~matnr = mara~matnr
            INNER JOIN marc ON mska~matnr = marc~matnr AND mska~werks = marc~werks
            INNER JOIN t001w ON mska~werks = t001w~werks
            LEFT OUTER JOIN mbew ON t001w~bwkey = mbew~bwkey AND mska~matnr = mbew~matnr AND mbew~bwtar = ''
          APPENDING CORRESPONDING FIELDS OF TABLE @collector_joined
          WHERE kzbws = 'A'
            AND xchar = ''
            AND mska~matnr IN @matnr
            AND mska~werks IN @werks
            AND mska~lgort IN @lgort
            AND mska~sobkz IN @so_sobkz
            AND mska~charg IN @charg
            AND ( mska~kalab IN @r_stopt
             OR   mska~kains IN @r_stopt
             OR   mska~kaspe IN @r_stopt
             OR   mska~kaein IN @r_stopt )
            AND marc~ekgrp IN @ekgrup
            AND mara~matkl IN @matkla
            AND mara~mtart IN @matart
          GROUP BY mska~matnr, mska~werks, mska~lgort, mska~charg, mska~sobkz, mska~vbeln, mska~posnr, kzbws.

*       Transfer stocks for customer order (SATRA in MSSA)
        SELECT SINGLE mandt FROM mste CONNECTION (dbcon) INTO lv_exists.
        IF sy-subrc IS INITIAL. "at least one entry found: continue
*         get transfer stock details with charg/ bwtar
          SELECT mste~matnr, mste~werks, mste~charg, mste~sobkz, mste~vbeln, mste~posnr, kzbws,
                 SUM( ewesb ) AS bwesb,
                 SUM( salk3 ) AS salk3, SUM( lbkum ) AS lbkum
            CONNECTION (dbcon)
            FROM mste
              INNER JOIN mssa ON mste~matnr = mssa~matnr AND mste~werks = mssa~werks AND mste~sobkz = mssa~sobkz AND mste~vbeln = mssa~vbeln AND mste~posnr = mssa~posnr
              INNER JOIN mara ON mste~matnr = mara~matnr
              INNER JOIN marc ON mste~matnr = marc~matnr AND mste~werks = marc~werks
              INNER JOIN t001w ON mste~werks = t001w~werks
              LEFT OUTER JOIN mbew ON t001w~bwkey = mbew~bwkey AND mste~matnr = mbew~matnr AND mste~bwtar = mbew~bwtar
            APPENDING CORRESPONDING FIELDS OF TABLE @collector_joined
            WHERE kzbws = 'A'
              AND mste~matnr IN @matnr
              AND mste~werks IN @werks
              AND mste~charg IN @charg
              AND mste~sobkz IN @so_sobkz
              AND mste~ewesb IN @r_stopt
              AND marc~ekgrp IN @ekgrup
              AND mara~matkl IN @matkla
              AND mara~mtart IN @matart
            GROUP BY mste~matnr, mste~werks, mste~charg, mste~sobkz, mste~vbeln, mste~posnr, kzbws.

        ENDIF.

*       get transit for batch/split managed materials
        SELECT mssa~matnr, mssa~werks, mssa~sobkz, mssa~vbeln, mssa~posnr, kzbws,
               SUM( satra ) AS umlme,
               SUM( salk3 ) AS salk3, SUM( lbkum ) AS lbkum
          CONNECTION (dbcon)
          FROM mssa
            INNER JOIN mara ON mssa~matnr = mara~matnr
            INNER JOIN marc ON mssa~matnr = marc~matnr AND mssa~werks = marc~werks
            INNER JOIN t001w ON mssa~werks = t001w~werks
            LEFT OUTER JOIN mbew ON t001w~bwkey = mbew~bwkey AND mssa~matnr = mbew~matnr AND mbew~bwtar = ''
          APPENDING CORRESPONDING FIELDS OF TABLE @collector_joined
          WHERE satra <> 0
            AND (  marc~xchpf = 'X' OR
                   marc~xchar = 'X' OR
                   marc~bwtty <> ''  )
            AND kzbws = 'A'
            AND mssa~matnr IN @matnr
            AND mssa~werks IN @werks
            AND mssa~sobkz IN @so_sobkz
            AND satra IN @r_stopt
            AND marc~ekgrp IN @ekgrup
            AND mara~matkl IN @matkla
            AND mara~mtart IN @matart
          GROUP BY mssa~matnr, mssa~werks, mssa~sobkz, mssa~vbeln, mssa~posnr, kzbws.

*       get val blocked stock and transit for all other materials
        SELECT mssa~matnr, mssa~werks, mssa~sobkz, mssa~vbeln, mssa~posnr, kzbws,
               SUM( satra ) AS umlme, SUM( sabwe ) AS bwesb,
               SUM( salk3 ) AS salk3, SUM( lbkum ) AS lbkum
          CONNECTION (dbcon)
          FROM mssa
            INNER JOIN mara ON mssa~matnr = mara~matnr
            INNER JOIN marc ON mssa~matnr = marc~matnr AND mssa~werks = marc~werks
            INNER JOIN t001w ON mssa~werks = t001w~werks
            LEFT OUTER JOIN mbew ON t001w~bwkey = mbew~bwkey AND mssa~matnr = mbew~matnr AND mbew~bwtar = ''
          APPENDING CORRESPONDING FIELDS OF TABLE @collector_joined
          WHERE marc~xchpf = ''
            AND marc~xchar = ''
            AND marc~bwtty = ''
            AND kzbws = 'A'
            AND mssa~matnr IN @matnr
            AND mssa~werks IN @werks
            AND mssa~sobkz IN @so_sobkz
            AND ( ( satra <> 0 AND
                    satra IN @r_stopt )
               OR ( sabwe <> 0 AND
                    sabwe IN @r_stopt ) )
            AND marc~ekgrp IN @ekgrup
            AND mara~matkl IN @matkla
            AND mara~mtart IN @matart
          GROUP BY mssa~matnr, mssa~werks, mssa~sobkz, mssa~vbeln, mssa~posnr, kzbws.

*       Next select single valuated stocks & non-valuated in EBEW
*       left join ebew will always “return” salk3 = zero for kzbws=blank
*       sum of all materials w/ batch
        SELECT mska~matnr, mska~werks, mska~lgort, mska~charg, mska~sobkz, mska~vbeln, mska~posnr, kzbws,
               SUM( kalab ) AS labst, SUM( kains ) AS insme, SUM( kaein ) AS einme, SUM( kaspe ) AS speme,
               SUM( salk3 ) AS salk3, SUM( lbkum ) AS lbkum
          CONNECTION (dbcon)
          FROM mska
            INNER JOIN mssa ON mska~matnr = mssa~matnr AND mska~werks = mssa~werks AND mska~sobkz = mssa~sobkz AND mska~vbeln = mssa~vbeln AND mska~posnr = mssa~posnr
            INNER JOIN mara ON mska~matnr = mara~matnr
            INNER JOIN marc ON mska~matnr = marc~matnr AND mska~werks = marc~werks
            INNER JOIN t001w ON mska~werks = t001w~werks
            LEFT OUTER JOIN mcha ON mska~matnr = mcha~matnr AND mska~werks = mcha~werks AND mska~charg = mcha~charg
         LEFT OUTER JOIN ebew ON t001w~bwkey = ebew~bwkey AND mska~matnr = ebew~matnr AND mcha~bwtar = ebew~bwtar AND mska~sobkz = ebew~sobkz AND mska~vbeln = ebew~vbeln AND mska~posnr = ebew~posnr
       APPENDING CORRESPONDING FIELDS OF TABLE @collector_joined
       WHERE kzbws <> 'A'
         AND xchar = 'X'
         AND mska~matnr IN @matnr
         AND mska~werks IN @werks
         AND mska~lgort IN @lgort
         AND mska~sobkz IN @so_sobkz
         AND mska~charg IN @charg
         AND ( mska~kalab IN @r_stopt
          OR   mska~kains IN @r_stopt
          OR   mska~kaspe IN @r_stopt
          OR   mska~kaein IN @r_stopt )
         AND marc~ekgrp IN @ekgrup
         AND mara~matkl IN @matkla
         AND mara~mtart IN @matart
       GROUP BY mska~matnr, mska~werks, mska~lgort, mska~charg, mska~sobkz, mska~vbeln, mska~posnr, kzbws.

*       sum of all materials w/o batch
        SELECT mska~matnr, mska~werks, mska~lgort, mska~charg, mska~sobkz,mska~vbeln, mska~posnr, kzbws,
               SUM( kalab ) AS labst, SUM( kains ) AS insme, SUM( kaein ) AS einme, SUM( kaspe ) AS speme,
               SUM( salk3 ) AS salk3, SUM( lbkum ) AS lbkum
          CONNECTION (dbcon)
          FROM mska
            INNER JOIN mssa ON mska~matnr = mssa~matnr AND mska~werks = mssa~werks AND mska~sobkz = mssa~sobkz AND mska~vbeln = mssa~vbeln AND mska~posnr = mssa~posnr
            INNER JOIN mara ON mska~matnr = mara~matnr
            INNER JOIN marc ON mska~matnr = marc~matnr AND mska~werks = marc~werks
            INNER JOIN t001w ON mska~werks = t001w~werks
            LEFT OUTER JOIN ebew ON t001w~bwkey = ebew~bwkey AND mska~matnr = ebew~matnr AND ebew~bwtar = '' AND mska~sobkz = ebew~sobkz AND mska~vbeln = ebew~vbeln AND mska~posnr = ebew~posnr
          APPENDING CORRESPONDING FIELDS OF TABLE @collector_joined
          WHERE kzbws <> 'A'
            AND xchar = ''
            AND mska~matnr IN @matnr
            AND mska~werks IN @werks
            AND mska~lgort IN @lgort
            AND mska~sobkz IN @so_sobkz
            AND mska~charg IN @charg
            AND ( mska~kalab IN @r_stopt
             OR   mska~kains IN @r_stopt
             OR   mska~kaspe IN @r_stopt
             OR   mska~kaein IN @r_stopt )
            AND marc~ekgrp IN @ekgrup
            AND mara~matkl IN @matkla
            AND mara~mtart IN @matart
          GROUP BY mska~matnr, mska~werks, mska~lgort, mska~charg, mska~sobkz, mska~vbeln, mska~posnr, kzbws.

*       Transfer stocks for customer order (SATRA in MSSA)
        SELECT SINGLE mandt FROM mste CONNECTION (dbcon) INTO lv_exists.
        IF sy-subrc IS INITIAL. "at least one entry found: continue
*         get transfer stock details with charg/ bwtar
          SELECT mste~matnr, mste~werks, mste~charg, mste~sobkz, mste~vbeln, mste~posnr,
                 SUM( ewesb ) AS bwesb,
                 SUM( salk3 ) AS salk3, SUM( lbkum ) AS lbkum
            CONNECTION (dbcon)
            FROM mste
              INNER JOIN mssa ON mste~matnr = mssa~matnr AND mste~werks = mssa~werks AND mste~sobkz = mssa~sobkz AND mste~vbeln = mssa~vbeln AND mste~posnr = mssa~posnr
              INNER JOIN mara ON mste~matnr = mara~matnr
              INNER JOIN marc ON mste~matnr = marc~matnr AND mste~werks = marc~werks
              INNER JOIN t001w ON mste~werks = t001w~werks
   LEFT OUTER JOIN ebew ON t001w~bwkey = ebew~bwkey AND mste~matnr = ebew~matnr AND mste~bwtar = ebew~bwtar AND mste~sobkz = ebew~sobkz AND mste~vbeln = ebew~vbeln AND mste~posnr = ebew~posnr
 APPENDING CORRESPONDING FIELDS OF TABLE @collector_joined
 WHERE kzbws <> 'A'
   AND mste~matnr IN @matnr
   AND mste~werks IN @werks
   AND mste~charg IN @charg
   AND mste~sobkz IN @so_sobkz
   AND mste~ewesb IN @r_stopt
   AND marc~ekgrp IN @ekgrup
   AND mara~matkl IN @matkla
   AND mara~mtart IN @matart
 GROUP BY mste~matnr, mste~werks, mste~charg, mste~sobkz, mste~vbeln, mste~posnr.

        ENDIF.

*       get transit for batch/split managed materials
        SELECT mssa~matnr, mssa~werks, mssa~sobkz, mssa~vbeln, mssa~posnr, kzbws,
               SUM( satra ) AS umlme,
               SUM( salk3 ) AS salk3, SUM( lbkum ) AS lbkum
          CONNECTION (dbcon)
          FROM mssa
            INNER JOIN mara ON mssa~matnr = mara~matnr
            INNER JOIN marc ON mssa~matnr = marc~matnr AND mssa~werks = marc~werks
            INNER JOIN t001w ON mssa~werks = t001w~werks
         LEFT OUTER JOIN ebew ON t001w~bwkey = ebew~bwkey AND mssa~matnr = ebew~matnr AND ebew~bwtar = '' AND mssa~sobkz = ebew~sobkz AND mssa~vbeln = ebew~vbeln AND mssa~posnr = ebew~posnr
       APPENDING CORRESPONDING FIELDS OF TABLE @collector_joined
       WHERE satra <> 0
         AND (  marc~xchpf = 'X' OR
                marc~xchar = 'X' OR
                marc~bwtty <> ''  )
         AND kzbws <> 'A'
         AND mssa~matnr IN @matnr
         AND mssa~werks IN @werks
         AND mssa~sobkz IN @so_sobkz
         AND satra IN @r_stopt
         AND marc~ekgrp IN @ekgrup
         AND mara~matkl IN @matkla
         AND mara~mtart IN @matart
       GROUP BY mssa~matnr, mssa~werks, mssa~sobkz, mssa~vbeln, mssa~posnr, kzbws.

*       get val blocked stock and transit for all other materials
        SELECT mssa~matnr, mssa~werks, mssa~sobkz, mssa~vbeln, mssa~posnr, kzbws,
               SUM( satra ) AS umlme, SUM( sabwe ) AS bwesb,
               SUM( salk3 ) AS salk3, SUM( lbkum ) AS lbkum
          CONNECTION (dbcon)
          FROM mssa
            INNER JOIN mara ON mssa~matnr = mara~matnr
            INNER JOIN marc ON mssa~matnr = marc~matnr AND mssa~werks = marc~werks
            INNER JOIN t001w ON mssa~werks = t001w~werks
       LEFT OUTER JOIN ebew ON t001w~bwkey = ebew~bwkey AND mssa~matnr = ebew~matnr AND ebew~bwtar = '' AND mssa~sobkz = ebew~sobkz AND mssa~vbeln = ebew~vbeln AND mssa~posnr = ebew~posnr
     APPENDING CORRESPONDING FIELDS OF TABLE @collector_joined
     WHERE marc~xchpf = ''
       AND marc~xchar = ''
       AND marc~bwtty = ''
       AND kzbws <> 'A'
       AND mssa~matnr IN @matnr
           AND mssa~werks IN @werks
           AND mssa~sobkz IN @so_sobkz
           AND ( ( satra <> 0 AND
                   satra IN @r_stopt )
              OR ( sabwe <> 0 AND
                   sabwe IN @r_stopt ) )
           AND marc~ekgrp IN @ekgrup
           AND mara~matkl IN @matkla
           AND mara~mtart IN @matart
      GROUP BY mssa~matnr, mssa~werks, mssa~sobkz, mssa~vbeln, mssa~posnr, kzbws.

      ELSE.

*       Next select single valuated stocks & non-valuated in EBEW
*       left join ebew will always “return” salk3 = zero for kzbws=blank
*       sum without batch for material w/ batch
        SELECT mska~matnr, mska~werks, mska~lgort, mska~sobkz, mska~vbeln, mska~posnr, kzbws,
               SUM( kalab ) AS labst, SUM( kains ) AS insme, SUM( kaein ) AS einme, SUM( kaspe ) AS speme,
               SUM( salk3 ) AS salk3, SUM( lbkum ) AS lbkum
          CONNECTION (dbcon)
          FROM mska
            INNER JOIN mssa ON mska~matnr = mssa~matnr AND mska~werks = mssa~werks AND mska~sobkz = mssa~sobkz AND mska~vbeln = mssa~vbeln AND mska~posnr = mssa~posnr
            INNER JOIN mara ON mska~matnr = mara~matnr
            INNER JOIN marc ON mska~matnr = marc~matnr AND mska~werks = marc~werks
            INNER JOIN t001w ON mska~werks = t001w~werks
            LEFT OUTER JOIN mcha ON mska~matnr = mcha~matnr AND mska~werks = mcha~werks AND mska~charg = mcha~charg
            LEFT OUTER JOIN mbew ON t001w~bwkey = mbew~bwkey AND mska~matnr = mbew~matnr AND mcha~bwtar = mbew~bwtar
          APPENDING CORRESPONDING FIELDS OF TABLE @collector_joined
          WHERE kzbws = 'A'
            AND xchar = 'X'
            AND mska~matnr IN @matnr
            AND mska~werks IN @werks
            AND mska~lgort IN @lgort
            AND mska~sobkz IN @so_sobkz
            AND ( mska~kalab IN @r_stopt
             OR   mska~kains IN @r_stopt
             OR   mska~kaspe IN @r_stopt
             OR   mska~kaein IN @r_stopt )
            AND marc~ekgrp IN @ekgrup
            AND mara~matkl IN @matkla
            AND mara~mtart IN @matart
          GROUP BY mska~matnr, mska~werks, mska~lgort, mska~sobkz, mska~vbeln, mska~posnr, kzbws.

*       sum without batch for material w/o batch
        SELECT mska~matnr, mska~werks, mska~lgort, mska~sobkz, mska~vbeln, mska~posnr, kzbws,
               SUM( kalab ) AS labst, SUM( kains ) AS insme, SUM( kaein ) AS einme, SUM( kaspe ) AS speme,
               SUM( salk3 ) AS salk3, SUM( lbkum ) AS lbkum
          CONNECTION (dbcon)
          FROM mska
            INNER JOIN mssa ON mska~matnr = mssa~matnr AND mska~werks = mssa~werks AND mska~sobkz = mssa~sobkz AND mska~vbeln = mssa~vbeln AND mska~posnr = mssa~posnr
            INNER JOIN mara ON mska~matnr = mara~matnr
            INNER JOIN marc ON mska~matnr = marc~matnr AND mska~werks = marc~werks
            INNER JOIN t001w ON mska~werks = t001w~werks
            LEFT OUTER JOIN mbew ON t001w~bwkey = mbew~bwkey AND mska~matnr = mbew~matnr AND mbew~bwtar = ''
          APPENDING CORRESPONDING FIELDS OF TABLE @collector_joined
          WHERE kzbws = 'A'
            AND xchar = ''
            AND mska~matnr IN @matnr
            AND mska~werks IN @werks
            AND mska~lgort IN @lgort
            AND mska~sobkz IN @so_sobkz
            AND ( mska~kalab IN @r_stopt
             OR   mska~kains IN @r_stopt
             OR   mska~kaspe IN @r_stopt
             OR   mska~kaein IN @r_stopt )
            AND marc~ekgrp IN @ekgrup
            AND mara~matkl IN @matkla
            AND mara~mtart IN @matart
          GROUP BY mska~matnr, mska~werks, mska~lgort, mska~sobkz, mska~vbeln, mska~posnr, kzbws.

*       Transfer stocks for customer order (SATRA in MSSA)
*       select only sums
        SELECT mssa~matnr, mssa~werks, mssa~sobkz, mssa~vbeln, mssa~posnr, kzbws,
               SUM( satra ) AS umlme,
               SUM( salk3 ) AS salk3, SUM( lbkum ) AS lbkum
          CONNECTION (dbcon)
          FROM mssa
            INNER JOIN mara ON mssa~matnr = mara~matnr
            INNER JOIN marc ON mssa~matnr = marc~matnr AND mssa~werks = marc~werks
            INNER JOIN t001w ON mssa~werks = t001w~werks
            LEFT OUTER JOIN mbew ON t001w~bwkey = mbew~bwkey AND mssa~matnr = mbew~matnr AND mbew~bwtar = ''
          APPENDING CORRESPONDING FIELDS OF TABLE @collector_joined
          WHERE kzbws = 'A'
            AND mssa~matnr IN @matnr
            AND mssa~werks IN @werks
            AND mssa~sobkz IN @so_sobkz
            AND  ( ( satra <> 0
                AND     satra IN @r_stopt )
              OR     ( sabwe <> 0
                AND     sabwe IN @r_stopt ) )
            AND marc~ekgrp IN @ekgrup
            AND mara~matkl IN @matkla
            AND mara~mtart IN @matart
          GROUP BY mssa~matnr, mssa~werks, mssa~sobkz, mssa~vbeln, mssa~posnr, kzbws.

*       Next select stock valuated in EBEW
*       sum without batch for material w/ batch
        SELECT mska~matnr, mska~werks, mska~lgort, mska~sobkz, mska~vbeln, mska~posnr, kzbws,
               SUM( kalab ) AS labst, SUM( kains ) AS insme, SUM( kaein ) AS einme, SUM( kaspe ) AS speme,
               SUM( salk3 ) AS salk3, SUM( lbkum ) AS lbkum
          CONNECTION (dbcon)
          FROM mska
            INNER JOIN mssa ON mska~matnr = mssa~matnr AND mska~werks = mssa~werks AND mska~sobkz = mssa~sobkz AND mska~vbeln = mssa~vbeln AND mska~posnr = mssa~posnr
            INNER JOIN mara ON mska~matnr = mara~matnr
            INNER JOIN marc ON mska~matnr = marc~matnr AND mska~werks = marc~werks
            INNER JOIN t001w ON mska~werks = t001w~werks
            LEFT OUTER JOIN mcha ON mska~matnr = mcha~matnr AND mska~werks = mcha~werks AND mska~charg = mcha~charg
LEFT OUTER JOIN ebew ON t001w~bwkey = ebew~bwkey AND mska~matnr = ebew~matnr AND mcha~bwtar = ebew~bwtar AND mska~sobkz = ebew~sobkz AND mska~vbeln = ebew~vbeln AND mska~posnr = ebew~posnr
APPENDING CORRESPONDING FIELDS OF TABLE @collector_joined
WHERE kzbws <> 'A'
AND xchar = 'X'
AND mska~matnr IN @matnr
AND mska~werks IN @werks
AND mska~lgort IN @lgort
AND mska~sobkz IN @so_sobkz
AND ( mska~kalab IN @r_stopt
OR   mska~kains IN @r_stopt
OR   mska~kaspe IN @r_stopt
OR   mska~kaein IN @r_stopt )
AND marc~ekgrp IN @ekgrup
AND mara~matkl IN @matkla
AND mara~mtart IN @matart
GROUP BY mska~matnr, mska~werks, mska~lgort, mska~sobkz, mska~vbeln, mska~posnr, kzbws.

*       sum without batch for material w/o batch
        SELECT mska~matnr, mska~werks, mska~lgort, mska~sobkz, mska~vbeln, mska~posnr, kzbws,
               SUM( kalab ) AS labst, SUM( kains ) AS insme, SUM( kaein ) AS einme, SUM( kaspe ) AS speme,
               SUM( salk3 ) AS salk3, SUM( lbkum ) AS lbkum
          CONNECTION (dbcon)
          FROM mska
            INNER JOIN mssa ON mska~matnr = mssa~matnr AND mska~werks = mssa~werks AND mska~sobkz = mssa~sobkz AND mska~vbeln = mssa~vbeln AND mska~posnr = mssa~posnr
            INNER JOIN mara ON mska~matnr = mara~matnr
            INNER JOIN marc ON mska~matnr = marc~matnr AND mska~werks = marc~werks
            INNER JOIN t001w ON mska~werks = t001w~werks
LEFT OUTER JOIN ebew ON t001w~bwkey = ebew~bwkey AND mska~matnr = ebew~matnr AND ebew~bwtar = '' AND mska~sobkz = ebew~sobkz AND mska~vbeln = ebew~vbeln AND mska~posnr = ebew~posnr
APPENDING CORRESPONDING FIELDS OF TABLE @collector_joined
WHERE kzbws <> 'A'
AND xchar = ''
AND mska~matnr IN @matnr
AND mska~werks IN @werks
AND mska~lgort IN @lgort
AND mska~sobkz IN @so_sobkz
AND ( mska~kalab IN @r_stopt
OR   mska~kains IN @r_stopt
OR   mska~kaspe IN @r_stopt
OR   mska~kaein IN @r_stopt )
AND marc~ekgrp IN @ekgrup
AND mara~matkl IN @matkla
AND mara~mtart IN @matart
GROUP BY mska~matnr, mska~werks, mska~lgort, mska~sobkz, mska~vbeln, mska~posnr, kzbws.

*       Transfer stocks for customer order (SATRA in MSSA)
*       select only sums
        SELECT mssa~matnr, mssa~werks, mssa~sobkz, mssa~vbeln, mssa~posnr, kzbws,
               SUM( satra ) AS umlme, SUM( sabwe ) AS bwesb,
               SUM( salk3 ) AS salk3, SUM( lbkum ) AS lbkum
          CONNECTION (dbcon)
          FROM mssa
            INNER JOIN mara ON mssa~matnr = mara~matnr
            INNER JOIN marc ON mssa~matnr = marc~matnr AND mssa~werks = marc~werks
            INNER JOIN t001w ON mssa~werks = t001w~werks
            LEFT OUTER JOIN ebew ON t001w~bwkey = ebew~bwkey AND mssa~matnr = ebew~matnr AND mssa~sobkz = ebew~sobkz AND mssa~vbeln = ebew~vbeln AND mssa~posnr = ebew~posnr
          APPENDING CORRESPONDING FIELDS OF TABLE @collector_joined
          WHERE kzbws <> 'A'
            AND mssa~matnr IN @matnr
            AND mssa~werks IN @werks
            AND mssa~sobkz IN @so_sobkz
            AND  ( ( satra <> 0
                AND     satra IN @r_stopt )
              OR     ( sabwe <> 0
                AND     sabwe IN @r_stopt ) )
            AND marc~ekgrp IN @ekgrup
            AND mara~matkl IN @matkla
            AND mara~mtart IN @matart
          GROUP BY mssa~matnr, mssa~werks, mssa~sobkz, mssa~vbeln, mssa~posnr, kzbws.

      ENDIF.
    ENDIF.
  ENDIF.

************************************************************************
* The same for project stocks (MSPR/MSSQ).
* MSPR has no flag for deletion
************************************************************************

  IF NOT pa_sond IS INITIAL AND
     NOT t_mat[] IS INITIAL AND
     'Q' IN so_sobkz.
    SELECT SINGLE mandt FROM mssq CONNECTION (dbcon) INTO lv_exists.
    IF sy-subrc IS INITIAL. "at least one entry found: continue

      IF xmchb IS NOT INITIAL.
*       sum of all materials w/ batch
        SELECT mspr~matnr, mspr~werks, mspr~lgort, mspr~charg, mspr~sobkz, mspr~pspnr, kzbws,
               SUM( prlab ) AS labst, SUM( prins ) AS insme, SUM( prein ) AS einme, SUM( prspe ) AS speme,
               SUM( salk3 ) AS salk3, SUM( lbkum ) AS lbkum
          CONNECTION (dbcon)
          FROM mspr
            INNER JOIN mssq ON mspr~matnr = mssq~matnr AND mspr~werks = mssq~werks AND mspr~sobkz = mssq~sobkz AND mspr~pspnr = mssq~pspnr
            INNER JOIN mara ON mspr~matnr = mara~matnr
            INNER JOIN marc ON mspr~matnr = marc~matnr AND mspr~werks = marc~werks
            INNER JOIN t001w ON mspr~werks = t001w~werks
            LEFT OUTER JOIN mcha ON mspr~matnr = mcha~matnr AND mspr~werks = mcha~werks AND mspr~charg = mcha~charg
           LEFT OUTER JOIN qbew ON t001w~bwkey = qbew~bwkey AND mspr~matnr = qbew~matnr AND mcha~bwtar = qbew~bwtar AND mspr~sobkz = qbew~sobkz AND mspr~pspnr = qbew~pspnr
         APPENDING CORRESPONDING FIELDS OF TABLE @collector_joined
         WHERE ( kzbws = 'M' OR kzbws = '' )
           AND xchar = 'X'
           AND mspr~matnr IN @matnr
           AND mspr~werks IN @werks
           AND mspr~lgort IN @lgort
           AND mspr~charg IN @charg
           AND ( prlab IN @r_stopt
            OR   prins IN @r_stopt
            OR   prspe IN @r_stopt
            OR   prein IN @r_stopt )
           AND marc~ekgrp IN @ekgrup
           AND mara~matkl IN @matkla
           AND mara~mtart IN @matart
         GROUP BY mspr~matnr, mspr~werks, mspr~lgort, mspr~charg, mspr~sobkz, mspr~pspnr, kzbws.

*       sum of all materials w/o batch
        SELECT mspr~matnr, mspr~werks, mspr~lgort, mspr~charg, mspr~sobkz, mspr~pspnr, kzbws,
               SUM( prlab ) AS labst, SUM( prins ) AS insme, SUM( prein ) AS einme, SUM( prspe ) AS speme,
               SUM( salk3 ) AS salk3, SUM( lbkum ) AS lbkum
          CONNECTION (dbcon)
          FROM mspr
            INNER JOIN mssq ON mspr~matnr = mssq~matnr AND mspr~werks = mssq~werks AND mspr~sobkz = mssq~sobkz AND mspr~pspnr = mssq~pspnr
            INNER JOIN mara ON mspr~matnr = mara~matnr
            INNER JOIN marc ON mspr~matnr = marc~matnr AND mspr~werks = marc~werks
            INNER JOIN t001w ON mspr~werks = t001w~werks
            LEFT OUTER JOIN qbew ON t001w~bwkey = qbew~bwkey AND mspr~matnr = qbew~matnr AND qbew~bwtar = '' AND mspr~sobkz = qbew~sobkz AND mspr~pspnr = qbew~pspnr
          APPENDING CORRESPONDING FIELDS OF TABLE @collector_joined
          WHERE ( kzbws = 'M' OR kzbws = '' )
            AND xchar = ''
            AND mspr~matnr IN @matnr
            AND mspr~werks IN @werks
            AND mspr~lgort IN @lgort
            AND mspr~charg IN @charg
            AND ( prlab IN @r_stopt
             OR   prins IN @r_stopt
             OR   prspe IN @r_stopt
             OR   prein IN @r_stopt )
            AND marc~ekgrp IN @ekgrup
            AND mara~matkl IN @matkla
            AND mara~mtart IN @matart
          GROUP BY mspr~matnr, mspr~werks, mspr~lgort, mspr~charg, mspr~sobkz, mspr~pspnr, kzbws.

*       Transfer stocks for projects (SQTRA in MSSQ)
        SELECT SINGLE mandt FROM mstq CONNECTION (dbcon) INTO lv_exists.
        IF sy-subrc IS INITIAL. "at least one entry found: continue

*         get transfer stock details with charg/ bwtar
          SELECT mstq~matnr, mstq~werks, mstq~charg, mstq~sobkz, mstq~pspnr, kzbws,
               SUM( qwesb ) AS bwesb,
               SUM( salk3 ) AS salk3, SUM( lbkum ) AS lbkum
            CONNECTION (dbcon)
            FROM mstq
              INNER JOIN mssq ON mstq~matnr = mssq~matnr AND mstq~werks = mssq~werks AND mstq~sobkz = mssq~sobkz AND mstq~pspnr = mssq~pspnr
              INNER JOIN mara ON mstq~matnr = mara~matnr
              INNER JOIN marc ON mstq~matnr = marc~matnr AND mstq~werks = marc~werks
              INNER JOIN t001w ON mstq~werks = t001w~werks
     LEFT OUTER JOIN qbew ON t001w~bwkey = qbew~bwkey AND mstq~matnr = qbew~matnr AND mstq~bwtar = qbew~bwtar AND mstq~sobkz = qbew~sobkz AND mstq~pspnr = qbew~pspnr
   APPENDING CORRESPONDING FIELDS OF TABLE @collector_joined
   WHERE ( kzbws = 'M' OR kzbws = '' )
     AND mstq~matnr IN @matnr
     AND mstq~werks IN @werks
     AND mstq~charg IN @charg
     AND mstq~sobkz IN @so_sobkz
     AND mstq~qwesb IN @r_stopt
     AND marc~ekgrp IN @ekgrup
     AND mara~matkl IN @matkla
     AND mara~mtart IN @matart
   GROUP BY mstq~matnr, mstq~werks, mstq~charg, mstq~sobkz, mstq~pspnr, kzbws.

        ENDIF.

*       get transit stock for batch/ split managed materials
        SELECT mssq~matnr, mssq~werks, mssq~sobkz, mssq~pspnr, kzbws,
             SUM( sqtra ) AS umlme,
             SUM( salk3 ) AS salk3, SUM( lbkum ) AS lbkum
          CONNECTION (dbcon)
          FROM mssq
            INNER JOIN mara ON mssq~matnr = mara~matnr
            INNER JOIN marc ON mssq~matnr = marc~matnr AND mssq~werks = marc~werks
            INNER JOIN t001w ON mssq~werks = t001w~werks
           LEFT OUTER JOIN qbew ON t001w~bwkey = qbew~bwkey AND mssq~matnr = qbew~matnr AND qbew~bwtar = '' AND mssq~sobkz = qbew~sobkz AND mssq~pspnr = qbew~pspnr
         APPENDING CORRESPONDING FIELDS OF TABLE @collector_joined
         WHERE (  marc~xchpf = 'X' OR
                  marc~xchar = 'X' OR
                  marc~bwtty <> ''  )
           AND ( kzbws = 'M' OR kzbws = '' )
           AND mssq~matnr IN @matnr
           AND mssq~werks IN @werks
           AND mssq~sobkz  IN  @so_sobkz
           AND sqtra <> 0
           AND sqtra IN @r_stopt
           AND marc~ekgrp IN @ekgrup
           AND mara~matkl IN @matkla
           AND mara~mtart IN @matart
         GROUP BY mssq~matnr, mssq~werks, mssq~sobkz, mssq~pspnr, kzbws.

*       get val blocked stock and transit for all other materials
        SELECT mssq~matnr, mssq~werks, mssq~sobkz, mssq~pspnr, kzbws,
             SUM( sqtra ) AS umlme, SUM( sqbwe ) AS bwesb,
             SUM( salk3 ) AS salk3, SUM( lbkum ) AS lbkum
          CONNECTION (dbcon)
          FROM mssq
            INNER JOIN mara ON mssq~matnr = mara~matnr
            INNER JOIN marc ON mssq~matnr = marc~matnr AND mssq~werks = marc~werks
            INNER JOIN t001w ON mssq~werks = t001w~werks
         LEFT OUTER JOIN qbew ON t001w~bwkey = qbew~bwkey AND mssq~matnr = qbew~matnr AND qbew~bwtar = '' AND mssq~sobkz = qbew~sobkz AND mssq~pspnr = qbew~pspnr
         APPENDING CORRESPONDING FIELDS OF TABLE @collector_joined
       WHERE marc~xchpf = ''
         AND marc~xchar = ''
         AND marc~bwtty = ''
         AND ( kzbws = 'M' OR kzbws = '' )
         AND mssq~matnr IN @matnr
         AND mssq~werks IN @werks
         AND mssq~sobkz  IN @so_sobkz
         AND ( ( sqtra <> 0 AND
                 sqtra IN @r_stopt )
            OR ( sqbwe <> 0 AND
                 sqbwe IN @r_stopt ) )
         AND marc~ekgrp IN @ekgrup
         AND mara~matkl IN @matkla
         AND mara~mtart IN @matart
         GROUP BY mssq~matnr, mssq~werks, mssq~sobkz, mssq~pspnr, kzbws.

      ELSE.

*       sum without batch for material w/ batch
        SELECT mspr~matnr, mspr~werks, mspr~lgort, mspr~sobkz, mspr~pspnr, kzbws,
               SUM( prlab ) AS labst, SUM( prins ) AS insme, SUM( prein ) AS einme, SUM( prspe ) AS speme,
               SUM( salk3 ) AS salk3, SUM( lbkum ) AS lbkum
          CONNECTION (dbcon)
          FROM mspr
            INNER JOIN mssq ON mspr~matnr = mssq~matnr AND mspr~werks = mssq~werks AND mspr~sobkz = mssq~sobkz AND mspr~pspnr = mssq~pspnr
            INNER JOIN mara ON mspr~matnr = mara~matnr
            INNER JOIN marc ON mspr~matnr = marc~matnr AND mspr~werks = marc~werks
            INNER JOIN t001w ON mspr~werks = t001w~werks
            LEFT OUTER JOIN mcha ON mspr~matnr = mcha~matnr AND mspr~werks = mcha~werks AND mspr~charg = mcha~charg
LEFT OUTER JOIN qbew ON t001w~bwkey = qbew~bwkey AND mspr~matnr = qbew~matnr AND mcha~bwtar = qbew~bwtar AND mspr~sobkz = qbew~sobkz AND mspr~pspnr = qbew~pspnr
APPENDING CORRESPONDING FIELDS OF TABLE @collector_joined
WHERE ( kzbws = 'M' OR kzbws = '' )
AND xchar = 'X'
AND mspr~matnr IN @matnr
AND mspr~werks IN @werks
AND mspr~lgort IN @lgort
AND ( prlab IN @r_stopt
OR   prins IN @r_stopt
OR   prspe IN @r_stopt
OR   prein IN @r_stopt )
AND marc~ekgrp IN @ekgrup
AND mara~matkl IN @matkla
AND mara~mtart IN @matart
GROUP BY mspr~matnr, mspr~werks, mspr~lgort, mspr~sobkz, mspr~pspnr, kzbws.

*       sum without batch for material w/o batch
        SELECT mspr~matnr, mspr~werks, mspr~lgort, mspr~sobkz, mspr~pspnr, kzbws,
               SUM( prlab ) AS labst, SUM( prins ) AS insme, SUM( prein ) AS einme, SUM( prspe ) AS speme,
               SUM( salk3 ) AS salk3, SUM( lbkum ) AS lbkum
          CONNECTION (dbcon)
          FROM mspr
            INNER JOIN mssq ON mspr~matnr = mssq~matnr AND mspr~werks = mssq~werks AND mspr~sobkz = mssq~sobkz AND mspr~pspnr = mssq~pspnr
            INNER JOIN mara ON mspr~matnr = mara~matnr
            INNER JOIN marc ON mspr~matnr = marc~matnr AND mspr~werks = marc~werks
            INNER JOIN t001w ON mspr~werks = t001w~werks
     LEFT OUTER JOIN qbew ON t001w~bwkey = qbew~bwkey AND mspr~matnr = qbew~matnr AND qbew~bwtar = '' AND mspr~sobkz = qbew~sobkz AND mspr~pspnr = qbew~pspnr
   APPENDING CORRESPONDING FIELDS OF TABLE @collector_joined
   WHERE ( kzbws = 'M' OR kzbws = '' )
     AND xchar = ''
     AND mspr~matnr IN @matnr
     AND mspr~werks IN @werks
     AND mspr~lgort IN @lgort
     AND ( prlab IN @r_stopt
      OR   prins IN @r_stopt
      OR   prspe IN @r_stopt
      OR   prein IN @r_stopt )
    AND marc~ekgrp IN @ekgrup
    AND mara~matkl IN @matkla
    AND mara~mtart IN @matart
   GROUP BY mspr~matnr, mspr~werks, mspr~lgort, mspr~sobkz, mspr~pspnr, kzbws.

*       select only sums
        SELECT mssq~matnr, mssq~werks, mssq~sobkz, mssq~pspnr, kzbws,
               SUM( sqtra ) AS umlme, SUM( sqbwe ) AS bwesb,
               SUM( salk3 ) AS salk3, SUM( lbkum ) AS lbkum
          CONNECTION (dbcon)
          FROM mssq
            INNER JOIN mara ON mssq~matnr = mara~matnr
            INNER JOIN marc ON mssq~matnr = marc~matnr AND mssq~werks = marc~werks
            INNER JOIN t001w ON mssq~werks = t001w~werks
            LEFT OUTER JOIN qbew ON t001w~bwkey = qbew~bwkey AND mssq~matnr = qbew~matnr AND mssq~sobkz = qbew~sobkz AND mssq~pspnr = qbew~pspnr
          APPENDING CORRESPONDING FIELDS OF TABLE @collector_joined
          WHERE ( kzbws = 'M' OR kzbws = '' )
           AND mssq~matnr IN @matnr
           AND mssq~werks IN @werks
           AND mssq~sobkz IN @so_sobkz
           AND  ( ( sqtra <> 0
            AND     sqtra IN @r_stopt )
           OR     ( sqbwe <> 0
            AND     sqbwe IN @r_stopt ) )
           AND marc~ekgrp IN @ekgrup
           AND mara~matkl IN @matkla
           AND mara~mtart IN @matart
          GROUP BY mssq~matnr, mssq~werks, mssq~sobkz, mssq~pspnr, kzbws.

      ENDIF.
    ENDIF.
  ENDIF.

**********************************************************************
  IF nozero = abap_true.
    DELETE collector_joined WHERE labst = 0 AND einme = 0 AND
                                  insme = 0 AND retme = 0 AND
                                  speme = 0 AND umlme = 0 AND
                                  bwesb = 0 AND bwesb = 0 AND
                                  trame = 0 AND glgmg = 0 AND
                                  umlmc = 0.
  ENDIF.
  IF collector_joined[] IS INITIAL.
* nothing found for selection - leave
    RETURN.
  ENDIF.


************************************************************************
* Fill in additional data (first round) and extract the data
* for the access to the valuation tables.
************************************************************************
  SORT t_mat BY matnr werks.
  SORT g_t_organ             BY werks.
  CLEAR : g_s_t001l,         g_s_organ.

**********************************************************************
* Consolidate all stock into BESTAND table
**********************************************************************
  REFRESH: bestand.
  LOOP AT collector_joined ASSIGNING <fs_collector_joined>
    WHERE charg IN charg.                                   "2035059

    CLEAR bestand.
    CLEAR factor.
    MOVE-CORRESPONDING <fs_collector_joined> TO bestand.

*   fill the key of the special stocks into the field
*   assigment
    CASE    <fs_collector_joined>-sobkz.
      WHEN  'E'.
        MOVE  : 'X'               TO  g_flag_sobkz-vbeln.
        WRITE : <fs_collector_joined>-vbeln   TO  bestand-ssnum.
        MOVE  : '/'               TO  bestand-ssnum+10(01).
        WRITE : <fs_collector_joined>-posnr   TO  bestand-ssnum+12(08)
                                  NO-ZERO.
        CONDENSE                  bestand-ssnum.

      WHEN  'T'.
        MOVE  : 'X'               TO  g_flag_sobkz-vbeln.
        WRITE : <fs_collector_joined>-vbeln   TO  bestand-ssnum.
        MOVE  : '/'               TO  bestand-ssnum+10(01).
        WRITE : <fs_collector_joined>-posnr   TO  bestand-ssnum+12(08)
                                  NO-ZERO.
        CONDENSE                  bestand-ssnum.

      WHEN  'K'.
        MOVE  : 'X'               TO  g_flag_sobkz-lifnr.
        WRITE : <fs_collector_joined>-lifnr   TO  bestand-ssnum.

      WHEN  'M'.
        MOVE  : 'X'               TO  g_flag_sobkz-lifnr.
        WRITE : <fs_collector_joined>-lifnr   TO  bestand-ssnum.

      WHEN  'O'.
        MOVE  : 'X'               TO  g_flag_sobkz-lifnr.
        WRITE : <fs_collector_joined>-lifnr   TO  bestand-ssnum.

      WHEN  'Q'.
        MOVE  : 'X'               TO  g_flag_sobkz-pspnr.
        WRITE : <fs_collector_joined>-pspnr   TO  bestand-ssnum.

      WHEN  'V'.
        MOVE  : 'X'               TO  g_flag_sobkz-kunnr.
        WRITE : <fs_collector_joined>-kunnr   TO  bestand-ssnum.

      WHEN  'W'.
        MOVE  : 'X'               TO  g_flag_sobkz-kunnr.
        WRITE : <fs_collector_joined>-kunnr   TO  bestand-ssnum.

      WHEN  OTHERS.
        CLEAR                     bestand-ssnum.
    ENDCASE.

    IF  g_flag_t001l = 'X'.
      IF  bestand-werks = g_s_t001l-werks  AND
          bestand-lgort = g_s_t001l-lgort.
      ELSE.
*       read with plant and storage location
        READ TABLE g_t_t001l   INTO  g_s_t001l
          WITH TABLE KEY werks = bestand-werks
                         lgort = bestand-lgort.

        MOVE : bestand-werks TO g_s_t001l-werks,
               bestand-lgort TO g_s_t001l-lgort.

        IF sy-subrc <> 0.
          CLEAR              g_s_t001l-lgobe.
        ENDIF.
      ENDIF.
    ENDIF.

*   take the storage bin from the buffer
    MOVE : g_s_t001l-lgobe   TO  bestand-lgobe.


*   get the information per plant with buffer
    IF  bestand-werks  NE  g_s_organ-werks.
      READ TABLE g_t_organ   INTO  g_s_organ
          WITH KEY werks = bestand-werks
                             BINARY SEARCH.

      IF sy-subrc <> 0.
*       sorry nothing found
        CLEAR                g_s_organ.
        MOVE : bestand-werks TO  g_s_organ-werks.
      ENDIF.
    ENDIF.

*   take the following fields from the buffer
    MOVE : g_s_organ-name1   TO  bestand-name1,
           g_s_organ-waers   TO  bestand-waers,
           g_s_organ-bwkey   TO  bestand-bwkey.

*   get the information from the material master MARC
*   with buffer
    IF  bestand-matnr = l_s_mat-matnr  AND
        bestand-werks = l_s_mat-werks.
*     results are in the buffer
    ELSE.
      CLEAR                  l_s_mat.
      MOVE : bestand-matnr   TO  l_s_mat-matnr,
             bestand-werks   TO  l_s_mat-werks.

      READ TABLE t_mat       INTO  l_s_mat
         WITH KEY matnr = bestand-matnr
                  werks = bestand-werks
                             BINARY SEARCH.

      IF  sy-subrc <> 0.
*       sorry nothing found
        CLEAR                l_s_mat.
        MOVE : bestand-matnr TO  l_s_mat-matnr,
               bestand-werks TO  l_s_mat-werks.
      ENDIF.
    ENDIF.

*   take the results the buffer
    MOVE : l_s_mat-mtart     TO  bestand-mtart,
           l_s_mat-matkl     TO  bestand-matkl,
           l_s_mat-meins     TO  bestand-meins,
           l_s_mat-maktx     TO  bestand-maktx.

*   if this entry has no deletion flag, take the
*   deletion flag from a higher level like MARA, MARC,
*   or MARDA
    IF  bestand-lvorm IS INITIAL.
      IF      NOT  l_s_mat-lvorm_marc IS INITIAL.
        MOVE  l_s_mat-lvorm_marc
                             TO  bestand-lvorm.
      ELSEIF  NOT  l_s_mat-lvorm_mara IS INITIAL.
        MOVE  l_s_mat-lvorm_mara
                             TO  bestand-lvorm.

      ELSEIF  NOT g_t_mard_lv[] IS INITIAL  AND
              NOT bestand-lgort IS INITIAL  AND
              NOT bestand-sobkz IS INITIAL.
*       look for deletion flag in working table
*       g_t_mard_lv for a line with special stock
        IF  bestand-matnr = g_s_mard_lv-matnr  AND
            bestand-werks = g_s_mard_lv-werks  AND
            bestand-lgort = g_s_mard_lv-lgort.
        ELSE.
*         read table only after the key has changed
          READ TABLE g_t_mard_lv  INTO  g_s_mard_lv
          WITH TABLE KEY matnr = bestand-matnr
                         werks = bestand-werks
                         lgort = bestand-lgort.

          IF sy-subrc <> 0.
*           fill the buffer in case the entry does not exist
            MOVE : bestand-matnr  TO  g_s_mard_lv-matnr,
                   bestand-werks  TO  g_s_mard_lv-werks,
                   bestand-lgort  TO  g_s_mard_lv-lgort.
            CLEAR                 g_s_mard_lv-lvorm.
          ENDIF.

*         take the result from the buffer
          MOVE  g_s_mard_lv-lvorm   TO  bestand-lvorm.
        ENDIF.
      ENDIF.
    ENDIF.

    IF <fs_collector_joined>-lbkum <> 0.
      factor = <fs_collector_joined>-salk3 / <fs_collector_joined>-lbkum.
    ELSE.
      factor = <fs_collector_joined>-salk3  .
    ENDIF.

    bestand-wlabs = bestand-labst * factor.
    bestand-winsm = bestand-insme * factor.
    bestand-wspem = bestand-speme * factor.
    bestand-weinm = bestand-einme * factor.
    bestand-wumlm = bestand-umlme * factor.
    bestand-wbwesb = bestand-bwesb * factor.
    bestand-wglgm = bestand-glgmg * factor.
    bestand-wtram = bestand-trame * factor.
    bestand-wumlc = bestand-umlmc * factor.

    APPEND bestand.
  ENDLOOP.

  FREE collector_joined.

ENDFORM.                    " DATA_SELECTION_JOIN