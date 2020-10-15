FUNCTION qeev_create_one_characteristic.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(I_PLMK) LIKE  PLMKB STRUCTURE  PLMKB
*"     VALUE(I_QALS) LIKE  QALS STRUCTURE  QALS
*"     VALUE(I_RUECKMELNR) LIKE  QAMV-RUECKMELNR
*"     VALUE(I_UNPLANNED_CHARACTERISTIC) LIKE  QAMV-NIPLANMKKZ DEFAULT
*"       SPACE
*"     VALUE(I_QAPO) LIKE  QAPO STRUCTURE  QAPO
*"     VALUE(I_LANGZEITPRUEFUNG_AKTIV) LIKE  QM00-QKZ DEFAULT SPACE
*"     VALUE(I_LAST_CHAR) LIKE  QAMKR-MERKNR DEFAULT '0000'
*"     REFERENCE(IV_OA_ID) TYPE  MPE_OA_ID OPTIONAL
*"     REFERENCE(IV_OA_VERSION_NO) TYPE  MPE_OA_VERSION_NO OPTIONAL
*"  EXPORTING
*"     VALUE(E_QALS) LIKE  QALS STRUCTURE  QALS
*"     VALUE(E_QAMV) LIKE  QAMV STRUCTURE  QAMV
*"     VALUE(E_QASV) LIKE  QASV STRUCTURE  QASV
*"  EXCEPTIONS
*"      NO_TQSS1_ENTRY
*"      NO_UNPLANNED_ENTRY
*"      ERROR_MESSAGE_QRK
*"      FOREIGN_LOCK_QASH
*"----------------------------------------------------------------------
  TABLES: tqss1,
          tqss.
ENHANCEMENT-POINT qeev_create_one_characteris_01 SPOTS es_saplqeev STATIC.
*$*$-Start: QEEV_CREATE_ONE_CHARACTERIS_01------------------------------------------------------$*$*
ENHANCEMENT 1  ZCHANGE_PROCESS_SAMPLING.    "active version

*******制造商等级不同，则采样过程不同
*******首先判断物料类型是否为包材，然后通过制造编码获取制造商等级字段。
*******在自定义表ZQMT003 中获取制造商等级的抽样水平
DATA:LS_ZQMT003 type ZQMT003,
     LV_REGIOGROUP type REGIOGROUP,
     LV_FIELD TYPE CHAR30.

FIELD-SYMBOLS <FS>.

SELECT COUNT(*) FROM MARA
 WHERE MATNR = I_QALS-MATNR
   AND MTART = 'Z020'.

IF SY-SUBRC = 0.

SELECT SINGLE REGIOGROUP INTO LV_REGIOGROUP
  FROM LFA1
  INNER JOIN ADRC ON LFA1~ADRNR = ADRC~ADDRNUMBER
    WHERE LIFNR = I_QALS-SELHERST.

  IF SY-SUBRC <> 0.
    SELECT SINGLE REGIOGROUP INTO LV_REGIOGROUP
      FROM LFA1
     INNER JOIN ADRC ON LFA1~ADRNR = ADRC~ADDRNUMBER
        WHERE LIFNR = I_QALS-SELLIFNR.
  ENDIF.

 IF SY-SUBRC = 0.

SELECT SINGLE * FROM ZQMT003
  INTO LS_ZQMT003
   WHERE GRADE = I_PLMK-STICHPRVER+7(1).

    IF sy-subrc = 0.
     CONCATENATE 'LS_ZQMT003-MFRNR' LV_REGIOGROUP INTO LV_FIELD.
     ASSIGN (LV_FIELD) TO <FS>.
   IF <FS> IS ASSIGNED.
     I_PLMK-STICHPRVER+7(1) = <FS>.
    ENDIF.

    ENDIF.
 ENDIF.
ENDIF.
*******END BY
ENDENHANCEMENT.
*$*$-End:   QEEV_CREATE_ONE_CHARACTERIS_01------------------------------------------------------$*$*

  DATA:
    l_qpmk_exp     LIKE qpmk,
    l_werk         LIKE qapo-werks,
    lo_mpe_oa_info TYPE REF TO cl_qm_retrieve_oa_info.

  CONSTANTS: hk_fertigung TYPE qherk VALUE '03'.

  IF i_qapo-werks IS INITIAL.
    MOVE i_qals-werk TO l_werk.
  ELSE.
    MOVE i_qapo-werks TO l_werk.
  ENDIF.

* Workareas initialisieren
  CLEAR e_qamv.
  CLEAR e_qasv.
  MOVE i_qals TO e_qals.

  MOVE-CORRESPONDING i_plmk TO e_qamv.
  MOVE-CORRESPONDING i_plmk TO e_qasv.

*-- Mandant explizit setzen
  MOVE sy-mandt             TO e_qamv-mandant.
  MOVE sy-mandt             TO e_qasv-mandant.

  MOVE e_qamv-steuerkz TO qmkst.

  IF    NOT e_qamv-atinn   IS INITIAL
    AND NOT qmkst-pruefkat IS INITIAL
    AND ( e_qamv-qpmk_ref EQ c_plan_config
    OR    e_qamv-qpmk_ref EQ c_only_config
    OR    e_qamv-qpmk_ref EQ c_plan_material_atinn
    OR    e_qamv-qpmk_ref EQ c_only_material_atinn ).
*-- gültige Codes lesen, falls Vorgaben aus Konfiguration oder
*-- Spezifikation kommen
    CALL FUNCTION 'QMCF_DETERMINE_VALID_CODES'
      EXPORTING
        i_qals     = i_qals
        i_atinn    = e_qamv-atinn
        i_herkunft = e_qamv-qpmk_ref
      EXCEPTIONS
        no_entry   = 1
        OTHERS     = 2.

    IF sy-subrc EQ null.
      MOVE kreuz TO e_qamv-abw_codes.
    ENDIF.

  ENDIF.

*-- Erfassungsformen verproben und gegebenenfalls umswitschen
  CASE qmkst-estukz.
    WHEN keineerf_char.
*--   keine Aktion
    WHEN klassen_char.
*--   qualitative Klassen ohne Ausprägungscodes werden auf summarisch
*--   zu erfassende Merkmale umgeswitcht.
      IF    qmkst-quantitat IS INITIAL
        AND qmkst-pruefkat  IS INITIAL.
        MOVE summerf_char TO qmkst-estukz.
        MOVE qmkst TO e_qamv-steuerkz.
      ENDIF.
    WHEN summerf_char.
*--   keine Aktion
    WHEN urwert_char.
*--   keine Aktion
    WHEN urwrueckw_char.
*--   Urwerteführung bei Rückweisung -> Urwertführung immer
      MOVE urwert_char  TO qmkst-estukz.
      MOVE qmkst TO e_qamv-steuerkz.
    WHEN OTHERS.
*     keine Aktion
  ENDCASE.

*-- Steuer-Kz zum Merkmal auswerten, um Satzstatus zu ermitteln
  CASE qmkst-rzwang.
    WHEN g_muss_kz.
*--   Muß-Merkmal
      MOVE eest_ubm TO e_qamv-satzstatus.
    WHEN g_kann_kz.
*--   Kann-Merkmal
      MOVE eest_ubk TO e_qamv-satzstatus.
    WHEN g_bed_annahme_kz.
*--   bedingtes Merkmal nach Annahme des Leitmerkmals
      MOVE eest_bda TO e_qamv-satzstatus.
    WHEN g_bed_rueckweis_kz.
*--   bedingtes Merkmal nach Rückweisung des Leitmerkmals
      MOVE eest_bdr TO e_qamv-satzstatus.
    WHEN OTHERS.
*--   keine Aktion -> darf nicht auftreten
  ENDCASE.
  MOVE e_qamv-satzstatus TO e_qasv-satzstatus.

* Sonderbehandlung einiger Felder
*-- QPMK_ZAEHL in QPMK_WERKS umswitschen
  MOVE i_plmk-qpmk_zaehl      TO e_qamv-qpmk_werks .

* Kurztext eines Stammprüfmerkmals ist in der Los-Anlege-Sprache
* nicht vorhanden -> Sortfeldtext suchen
  IF       e_qamv-kurztext   IS INITIAL
   AND NOT e_qamv-verwmerkm  IS INITIAL
   AND NOT e_qamv-mkversion  IS INITIAL
   AND NOT e_qamv-qpmk_werks IS INITIAL.

    CALL FUNCTION 'QPSD_FEATURE_VERSION_READ'
      EXPORTING
        merkmal         = e_qamv-verwmerkm
        version         = e_qamv-mkversion
        werk            = e_qamv-qpmk_werks
        i_no_catalogs   = kreuz
      IMPORTING
        qpmk_exp        = l_qpmk_exp
      EXCEPTIONS
        no_feature      = 0
        no_free_version = 0
        no_version      = 0
        OTHERS          = 0.

    MOVE l_qpmk_exp-sortfeld TO e_qamv-kurztext.
  ENDIF.

*-- Ausschußanteil losrelevant
  MOVE qmkst-ausslos           TO e_qamv-anteilkz.

*-- Verfahren Ausschußanteil
  MOVE i_plmk-eeantverf TO e_qamv-antverf.
  MOVE i_plmk-eeantverf TO e_qasv-antverf.

*-- Phys. Probenr
  MOVE i_plmk-probenr         TO e_qamv-physprobe.
*
  IF NOT i_plmk-pruefeinh IS INITIAL.
    MOVE i_plmk-pruefeinh     TO e_qamv-probmgfak.
    MOVE i_plmk-pruefeinh     TO e_qasv-probmgfak.
  ELSE.
    MOVE ini_probmgfak         TO e_qamv-probmgfak.
    MOVE ini_probmgfak         TO e_qasv-probmgfak.
  ENDIF.

*  Probemengeneinheit aus Prüflos falls initial aus Planmerkmal
  IF e_qamv-probemgeh IS INITIAL.
    MOVE i_qals-mengeneinh TO e_qamv-probemgeh.
  ENDIF.

  IF e_qasv-probemgeh IS INITIAL.
    MOVE i_qals-mengeneinh TO e_qasv-probemgeh.
  ENDIF.

* Möglichen Änderer und Änderdatum aus Planmerkmal nicht in Merkmals-
* vorgabensatz übernehmen
  CLEAR e_qamv-aenderer.
  CLEAR e_qamv-aenderdat.

* Möglichen Änderer und Änderdatum aus Planmerkmal nicht in Proben-
* vorgabensatz übernehmen
  CLEAR e_qasv-aenderer.
  CLEAR e_qasv-aenderdat.

*   Mengeneinheit aus Materialstamm in Merkmal übernehmen
  MOVE i_qals-mengeneinh TO e_qamv-mengeneinh.
  MOVE i_qals-mengeneinh TO e_qasv-mengeneinh.

  MOVE:
    i_qals-prueflos  TO e_qamv-prueflos,
    i_qals-prueflos  TO e_qasv-prueflos.

  MOVE:
    i_qapo-vorglfnr  TO e_qamv-vorglfnr,
    i_qapo-vorglfnr  TO e_qasv-vorglfnr,

    init_probenr     TO e_qasv-probenr.

* Ersteller -datum eintragen
  MOVE:
    sy-uname           TO e_qamv-ersteller,
    sy-datum           TO e_qamv-erstelldat,
    sy-uname           TO e_qasv-ersteller,
    sy-datum           TO e_qasv-erstelldat.

  IF e_qamv-merkgew IS INITIAL.
*-  Merkmalsgewichtung aus Prüfplan lesen
    CALL FUNCTION 'QEZZ_TQSS1_READ'
      EXPORTING
        i_werk         = l_werk
      IMPORTING
        e_tqss1        = tqss1
      EXCEPTIONS
        no_entry_found = 01.

    IF sy-subrc EQ eins.
      MESSAGE e165 WITH l_werk RAISING no_tqss1_entry.
    ENDIF.

*     Übernehmen in Merkmal
    MOVE tqss1-merkkl TO e_qamv-merkgew.
  ENDIF.
*
  IF NOT qmkst-messwerte IS INITIAL.
*   Plausifaktoren für messende Merkmale ermitteln, falls sie nicht
*   im Plan gepflegt wurden
    IF e_qamv-plausiobni IS INITIAL
    OR e_qamv-plausiunni IS INITIAL.

      IF g_plausibility_faktor EQ kreuz.
        CALL FUNCTION fkt_plausi_grenz
          EXPORTING
            toleranzob             = e_qamv-toleranzob
            toleranzun             = e_qamv-toleranzun
            tolobni                = e_qamv-tolobni
            tolunni                = e_qamv-tolunni
            werk                   = l_werk
            plausioben             = e_qamv-plausioben
            plausiobni             = e_qamv-plausiobni
            plausiunni             = e_qamv-plausiunni
            plausiunte             = e_qamv-plausiunte
          IMPORTING
            plausioben             = e_qamv-plausioben
            plausiobni             = e_qamv-plausiobni
            plausiunni             = e_qamv-plausiunni
            plausiunte             = e_qamv-plausiunte
          EXCEPTIONS
            no_tqss1_entry         = 1
            no_plausibility_faktor = 2.
        IF sy-subrc EQ eins.
          MESSAGE e165 WITH l_werk RAISING no_tqss1_entry.
        ELSEIF sy-subrc EQ zwei.
          CLEAR g_plausibility_faktor.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
*
  IF NOT e_qamv-tolunni IS INITIAL.
*   Merkmal mit unterer Toleranzgrenze
    IF NOT i_plmk-tolwunni IS INITIAL.
*     Prüfen auf Toleranzerweiterung und Gültigkeit
      IF  sy-datlo GE i_plmk-tolerwab
      AND sy-datlo LE i_plmk-tolerwbis.
        e_qamv-toleranzun =
           e_qamv-toleranzun - i_plmk-tolerweiun.
        MOVE kreuz TO e_qamv-tolerwkz.
      ENDIF.
    ENDIF.
  ENDIF.

  IF NOT e_qamv-tolobni IS INITIAL.
*   Merkmal mit oberer Toleranzgrenze
    IF NOT i_plmk-tolwobni IS INITIAL.
*     Prüfen auf Toleranzerweiterung und Gültigkeit
      IF  sy-datlo GE i_plmk-tolerwab
      AND sy-datlo LE i_plmk-tolerwbis.
        e_qamv-toleranzob =
           e_qamv-toleranzob + i_plmk-tolerweiob.
        MOVE kreuz TO e_qamv-tolerwkz.
      ENDIF.
    ENDIF.
  ENDIF.

*   Rückmeldenummer des Merkmals setzen
  MOVE i_rueckmelnr TO e_qamv-rueckmelnr.

*-  Felder bei ungeplanten Merkmalen füllen
  IF NOT i_unplanned_characteristic IS INITIAL.
    CALL FUNCTION 'QEZZ_TQSS_READ'
      IMPORTING
        e_tqss         = tqss
      EXCEPTIONS
        no_entry_found = 01.
    IF NOT tqss-unplmk IS INITIAL.
*--     Merkmalsnummer setzen
      IF i_last_char GE tqss-unplmk.
        e_qamv-merknr = i_last_char + g_faktor_merkm.
      ELSE.
        e_qamv-merknr = tqss-unplmk.
      ENDIF.
*     E_QAMV-MERKNR
*       TQSS-UNPLMK + E_QALS-ZUSMKZAEHL * G_FAKTOR_MERKM.
    ELSE.
      MESSAGE e115 RAISING no_unplanned_entry.
    ENDIF.
    MOVE e_qamv-merknr TO e_qasv-merknr.
    MOVE kreuz TO e_qamv-niplanmkkz.
    ADD 1 TO e_qals-zusmkzaehl.

*--   Ungeplantes Merkmal -> Kann-Merkmal
    MOVE e_qamv-steuerkz TO qmkst.
    IF qmkst-rzwang NE g_kann_kz.
      MOVE c_char_changed TO e_qamv-qpmk_ref.
      MOVE g_kann_kz TO qmkst-rzwang.
      MOVE eest_ubk TO e_qamv-satzstatus.
      MOVE eest_ubk TO e_qasv-satzstatus.
    ENDIF.

*--   Ungeplantes Merkmal -> Kein Formelmerkmal
ENHANCEMENT-SECTION     qeev_create_one_characteris_02 SPOTS es_saplqeev.
    IF qmkst-formelmk EQ 'X'.
      MOVE c_char_changed TO e_qamv-qpmk_ref.
      CLEAR qmkst-formelmk.
      CLEAR e_qamv-formelsl.
      MESSAGE w140.
    ENDIF.
END-ENHANCEMENT-SECTION.
*--   SPC-Merkmal -> wird zurückgenommen
    IF NOT qmkst-qspcmk IS INITIAL.
      MOVE c_char_changed TO e_qamv-qpmk_ref.
      CLEAR qmkst-qspcmk.
      MESSAGE w718.
    ENDIF.
*--   Langzeitprüfung aktiv -> Ungeplantes Merkmal muß Langzeitmk sein
    IF NOT i_langzeitpruefung_aktiv IS INITIAL AND
      qmkst-lzeitkz IS INITIAL.
      MOVE kreuz TO qmkst-lzeitkz.
      MESSAGE w081.
    ENDIF.

    MOVE qmkst TO e_qamv-steuerkz.
  ENDIF.

  IF NOT i_plmk-spckrit IS INITIAL.
*--   Zuordnung QRK-Karte
    CALL FUNCTION 'QRKD_SEARCH_CONTROL_CHART'
      EXPORTING
        i_qals              = i_qals
        i_qapo              = i_qapo
        i_qamv              = e_qamv
        i_spckrit           = i_plmk-spckrit
      IMPORTING
        e_qrknr             = e_qasv-qrknr
      EXCEPTIONS
        no_entry_in_qdsv    = 01
        no_entry_in_qpsh    = 01
        no_entry_in_qpsp    = 01
        no_entry_in_qpst    = 01
        no_function_in_qpsp = 01
        error_message       = 01
        foreign_lock        = 02.
    CASE sy-subrc.
      WHEN 00.
*--     nichts tun
      WHEN 01.
        RAISE error_message_qrk.
      WHEN 02.
        RAISE foreign_lock_qash.
    ENDCASE.
  ENDIF.

  IF i_qals-herkunft EQ hk_fertigung.
    IF iv_oa_id IS INITIAL AND iv_oa_version_no IS INITIAL.
      CALL METHOD cl_qm_retrieve_oa_info=>get_instance
        EXPORTING
          iv_plnty    = i_qapo-plnty
          iv_plnnr    = i_qapo-plnnr
        IMPORTING
          eo_instance = lo_mpe_oa_info.
      IF lo_mpe_oa_info IS BOUND.
        CALL METHOD lo_mpe_oa_info->get_oa_info
        EXPORTING
          iv_plnkn         = i_qapo-pplnkn
          iv_merknr        = i_plmk-merknr
        IMPORTING
          ev_oa_id         = e_qamv-oa_id
          ev_oa_version_no = e_qamv-oa_version_no.
      ENDIF.
    ELSE.
      e_qamv-oa_id = iv_oa_id.
      e_qamv-oa_version_no = iv_oa_version_no.
    ENDIF.
  ENDIF.
ENDFUNCTION.