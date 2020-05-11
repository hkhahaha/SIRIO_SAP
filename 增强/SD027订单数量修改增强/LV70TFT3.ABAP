*----------------------------------------------------------------------*
*   INCLUDE LV70TFT3                                                   *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  TEXT_CONTROL_EINGABEBEREIT
*&---------------------------------------------------------------------*
*       Die Eingabeberitschaft des Text Control wird gesetzt
*----------------------------------------------------------------------*
form text_control_eingabebereit.

    data: lv_formatierung(1) type c,
          lv_text_not_exist(1) type c,
          l_token type itf_token.
  
  
    if display_flag <> charx.
  
      loop at tlinetab.
        if not tlinetab-tdformat is initial and "Fließtext
           tlinetab-tdformat <> '* '. "Default-Absatz
  *         tlinetab-tdformat <> '/ ' and "Zeilenvorschub
  *         tlinetab-tdformat <> '= ' and "Langzeile
  *         tlinetab-tdformat <> '( ' and "Rohzeile
  *         tlinetab-tdformat <> '/=' and "Zeilenvorschub, Langzeile
  *         tlinetab-tdformat <> '/(' and "Zeilenvorschub, Rohzeile
  *        tlinetab-tdformat <> '/:' and        "Kommando-Zeile
  *         tlinetab-tdformat <> '/*'.  "Kommentarzeile
          lv_formatierung = charx.
          exit.
        endif.
      endloop.
  
  * check for special sapscript format
      perform init_itf_scanner(rstxscan) using c_without_command_scan
                                               c_form_text.
      perform set_itf_text(rstxscan) using tlinetab[].
  
     if lv_formatierung is initial.
        do.
          perform read_next_itf_token(rstxscan) using l_token.
            if l_token-code <> c_token_element_begin and
               l_token-code <> c_token_element_end and
               l_token-code <> c_token_paragraph_begin and
               l_token-code <> c_token_paragraph_end and
               l_token-code <> c_token_line_begin and
               l_token-code <> c_token_line_end and
               l_token-code <> c_token_string.
                 " text besitzt formatierung
                 " c_token_tab
                 " c_token_ltag_begin
                 " c_token_ctag_begin
                 " c_token_url_begin
                 " c_token_symbol_begin
                 " c_token_symbol
                 " c_token_prefix_begin
                 " c_token_option
                 " c_token_suffix_begin
                 " c_token_sap_char
                 " c_token_template_string
                 " c_token_command_begin
                 " c_token_command
                 " c_token_comment
                 if l_token-code = c_token_text_end.
                   exit.
                 else.
                   lv_formatierung = charx.
                   exit.
                 endif.
            endif.
        enddo.
     endif.
  
  * Meldung ausgeben -> Text ist formatiert
      if not lv_formatierung is initial.
        message s202.
      endif.
    endif.
  
  * Keine Eingabebereitschaft des Multiline Editors für
  * Word Dokumente
    if thead-tdtexttype eq 'DOC' or
       thead-tdtexttype eq 'RTF'.
      message s202.
      lv_formatierung = charx.
    endif.
  
  * ausgewählter Text nicht vorhanden?
    if gv_tree_active = charx and
      NOT NODE_IS_FOLDER IS INITIAL.
        lv_text_not_exist = charx.
    endif.
  
  * Eingabebereitschaft setzen
    if display_flag = charx or
       lv_formatierung = charx
       .
  
      call method gv_text_editor->set_readonly_mode
           exporting
                readonly_mode = gv_text_editor->true
           exceptions
                error_cntl_call_method = 1
                invalid_parameter      = 2
                others                 = 3.
  
      CLEAR GV_LISTBOX_LANGUAGE_ACTIVE.
  
    else.
      call method gv_text_editor->set_readonly_mode
           exporting
                readonly_mode = gv_text_editor->false
           exceptions
                error_cntl_call_method = 1
                invalid_parameter      = 2
                others                 = 3.
    endif.
  *{   INSERT         S4DK903080                                        1
  *对文本取消权限的判断，任何用户都可以进行编辑
  *  IF sy-tcode EQ 'VA02'."权限检查不通过灰掉长文本
  *  CALL FUNCTION 'Z_SD_AUTH_CHECK'
  *  EXCEPTIONS
  *    check_failed = 1
  *    OTHERS       = 2.
  *  IF sy-subrc NE 0.
  *    DISPLAY_FLAG = 'X'.
  *  ENDIF.
  *  ENDIF.
    "黄铠修改文本可编辑（任何人）
    IF SY-TCODE = 'VA02'.
      CALL METHOD gv_text_editor->set_readonly_mode
      EXPORTING
        readonly_mode = gv_text_editor->false
      EXCEPTIONS
        error_cntl_call_method = 1
        invalid_parameter      = 2
        OTHERS                 = 3.
  
  *    CLEAR GV_LISTBOX_LANGUAGE_ACTIVE.
    ENDIF.
  
  *}   INSERT
  
  endform.                               " TEXT_CONTROL_EINGABEBEREIT