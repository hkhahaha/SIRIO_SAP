*&---------------------------------------------------------------------*
*& Report ZHKALV9
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zhkalv9.
*& Table declaration
*&---------------------------------------------------------------------*
TABLES: ekko.
*&---------------------------------------------------------------------*
*& Type pool declaration
*&---------------------------------------------------------------------*
TYPE-POOLS: slis. " Type pool for ALV
*&---------------------------------------------------------------------*
*& Selection screen
*&---------------------------------------------------------------------*
SELECT-OPTIONS: s_ebeln FOR ekko-ebeln.
*&---------------------------------------------------------------------*
*& Type declaration
*&---------------------------------------------------------------------*
* Type declaration for internal table to store EKPO data
TYPES: BEGIN OF x_data,
       ebeln  TYPE char30,  " Document no.
       ebelp  TYPE ebelp,   " Item no
       matnr  TYPE matnr,   " Material no
       matnr1 TYPE matnr,   " Material no
       werks  TYPE werks_d, " Plant
       werks1 TYPE werks_d, " Plant
       ntgew  TYPE entge,   " Net weight
       gewe   TYPE egewe,   " Unit of weight
       END OF x_data.
*&---------------------------------------------------------------------*
*& Internal table declaration
*&---------------------------------------------------------------------*
DATA:
* Internal table to store EKPO data
  i_ekpo TYPE STANDARD TABLE OF x_data INITIAL SIZE 0,
* Internal table for storing field catalog information
  i_fieldcat TYPE slis_t_fieldcat_alv,
* Internal table for Top of Page info. in ALV Display
  i_alv_top_of_page TYPE slis_t_listheader,
* Internal table for ALV Display events
  i_events TYPE slis_t_event,
* Internal table for storing ALV sort information
  i_sort TYPE  slis_t_sortinfo_alv,
  i_event TYPE slis_t_event.
*&---------------------------------------------------------------------*
*& Work area declaration
*&---------------------------------------------------------------------*
DATA:
  wa_ekko TYPE x_data,
  wa_layout     TYPE slis_layout_alv,
  wa_events         TYPE slis_alv_event,
  wa_sort TYPE slis_sortinfo_alv.
*&---------------------------------------------------------------------*
*& Constant declaration
*&---------------------------------------------------------------------*
CONSTANTS:
   c_header   TYPE char1
              VALUE 'H',                    "Header in ALV
   c_item     TYPE char1
              VALUE 'S'.
*&---------------------------------------------------------------------*
*& Start-of-selection event
*&---------------------------------------------------------------------*
START-OF-SELECTION.
* Select data from ekpo
  SELECT ebeln " Doc no
         ebelp " Item
         matnr " Material
         matnr " Material
         werks " Plant
         werks " Plant
         ntgew " Quantity
         gewei " Unit
         FROM ekpo
         INTO TABLE i_ekpo
         WHERE ebeln IN s_ebeln
         AND ntgew NE '0.00'.
  IF sy-subrc = 0.
    SORT i_ekpo BY ebeln ebelp matnr .
  ENDIF.
* To build the Page header
  PERFORM sub_build_header.
* To prepare field catalog
  PERFORM sub_field_catalog.
* Perform to populate the layout structure
  PERFORM sub_populate_layout.
* Perform to populate the sort table.
  PERFORM sub_populate_sort.
* Perform to populate ALV event
  PERFORM sub_get_event.
END-OF-SELECTION.
* Perform to display ALV report
  PERFORM sub_alv_report_display.

*&---------------------------------------------------------------------*
*&      Form  sub_build_header
*&---------------------------------------------------------------------*
*       To build the header
*----------------------------------------------------------------------*
*       No Parameter
*----------------------------------------------------------------------*
FORM sub_build_header .
* Local data declaration
  DATA: l_system     TYPE char10 ,          "System id
        l_r_line     TYPE slis_listheader,  "Hold list header
        l_date       TYPE char10,           "Date
        l_time       TYPE char10,           "Time
        l_success_records TYPE i,           "No of success records
        l_title(300) TYPE c.                " Title

* Title  Display
  l_r_line-typ = c_header.               " header
  l_title = 'Test report'(001).
  l_r_line-info = l_title.
  APPEND l_r_line TO i_alv_top_of_page.
  CLEAR l_r_line.
* Run date Display
  CLEAR l_date.
  l_r_line-typ  = c_item.                " Item
  WRITE: sy-datum  TO l_date MM/DD/YYYY.
  l_r_line-key = 'Run Date :'(002).
  l_r_line-info = l_date.
  APPEND l_r_line TO i_alv_top_of_page.
  CLEAR: l_r_line,
         l_date.
ENDFORM.                    " sub_build_header

*&---------------------------------------------------------------------*
*&      Form  sub_field_catalog
*&---------------------------------------------------------------------*
*       Build Field Catalog
*----------------------------------------------------------------------*
*       No Parameter
*----------------------------------------------------------------------*
FORM sub_field_catalog .
*  Build Field Catalog
  PERFORM sub_fill_alv_field_catalog USING:
     '01' '01' 'EBELN' 'I_EKPO' 'L'
     'Doc No'(003) ' ' ' ' ' ' ' ',
     '01' '02' 'EBELP' 'I_EKPO' 'L'
     'Item No'(004) 'X' 'X' ' ' ' ',
     '01' '03' 'MATNR' 'I_EKPO' 'L'
     'Material No'(005) 'X' 'X' ' ' ' ',
     '01' '03' 'MATNR1' 'I_EKPO' 'L'
     'Material No'(005) ' ' ' ' ' ' ' ',

     '01' '04' 'WERKS' 'I_EKPO' 'L'
     'Plant'(006) 'X' 'X' ' ' ' ',
     '01' '04' 'WERKS1' 'I_EKPO' 'L'
     'Plant'(006) ' ' ' ' ' ' ' ',
     '01' '05' 'NTGEW' 'I_EKPO' 'R'
     'Net Weight'(007) ' ' ' ' 'GEWE' 'I_EKPO'.
ENDFORM.                    " sub_field_catalog
*&---------------------------------------------------------------------*
*&     Form  sub_fill_alv_field_catalog
*&---------------------------------------------------------------------*
*&     For building Field Catalog
*&---------------------------------------------------------------------*
*&     p_rowpos   Row position
*&     p_colpos   Col position
*&     p_fldnam   Fldname
*&     p_tabnam   Tabname
*&     p_justif   Justification
*&     p_seltext  Seltext
*&     p_out      no out
*&     p_tech     Technical field
*&     p_qfield   Quantity field
*&     p_qtab     Quantity table
*&---------------------------------------------------------------------*
FORM sub_fill_alv_field_catalog  USING  p_rowpos    TYPE sycurow
                                        p_colpos    TYPE sycucol
                                        p_fldnam    TYPE fieldname
                                        p_tabnam    TYPE tabname
                                        p_justif    TYPE char1
                                        p_seltext   TYPE dd03p-scrtext_l
                                        p_out       TYPE char1
                                        p_tech      TYPE char1
                                        p_qfield    TYPE slis_fieldname
                                        p_qtab      TYPE slis_tabname.
* Local declaration for field catalog
  DATA: wa_lfl_fcat    TYPE  slis_fieldcat_alv.
  wa_lfl_fcat-row_pos        =  p_rowpos.     "Row
  wa_lfl_fcat-col_pos        =  p_colpos.     "Column
  wa_lfl_fcat-fieldname      =  p_fldnam.     "Field Name
  wa_lfl_fcat-tabname        =  p_tabnam.     "Internal Table Name
  wa_lfl_fcat-just           =  p_justif.     "Screen Justified
  wa_lfl_fcat-seltext_l      =  p_seltext.    "Field Text
  wa_lfl_fcat-no_out         =  p_out.        "No output
  wa_lfl_fcat-tech           =  p_tech.       "Technical field
  wa_lfl_fcat-qfieldname     =  p_qfield.     "Quantity unit
  wa_lfl_fcat-qtabname       =  p_qtab .      "Quantity table
  IF p_fldnam = 'NTGEW'.
    wa_lfl_fcat-do_sum  = 'X'.
  ENDIF.
  APPEND wa_lfl_fcat TO i_fieldcat.
  CLEAR wa_lfl_fcat.
ENDFORM.                    " sub_fill_alv_field_catalog
*&---------------------------------------------------------------------*
*&      Form  sub_populate_layout
*&---------------------------------------------------------------------*
*       Populate ALV layout
*----------------------------------------------------------------------*
*       No Parameter
*----------------------------------------------------------------------*
FORM sub_populate_layout .
  CLEAR wa_layout.
  wa_layout-colwidth_optimize = 'X'." Optimization of Col width
ENDFORM.                    " sub_populate_layout
*&---------------------------------------------------------------------*
*&      Form  sub_populate_sort
*&---------------------------------------------------------------------*
*       Populate ALV sort table
*----------------------------------------------------------------------*
*       No Parameter
*----------------------------------------------------------------------*
FORM sub_populate_sort .
* Sort on material
  wa_sort-spos = '01' .
  wa_sort-fieldname = 'MATNR'.
  wa_sort-tabname = 'I_EKPO'.
  wa_sort-up = 'X'.
  wa_sort-subtot = 'X'.
  APPEND wa_sort TO i_sort .
  CLEAR wa_sort.
* Sort on plant
  wa_sort-spos = '02'.
  wa_sort-fieldname = 'WERKS'.
  wa_sort-tabname = 'I_EKPO'.
  wa_sort-up = 'X'.
  wa_sort-subtot = 'X'.
  APPEND wa_sort TO i_sort .
  CLEAR wa_sort.

ENDFORM.                    " sub_populate_sort
*&---------------------------------------------------------------------*
*&      Form  sub_get_event
*&---------------------------------------------------------------------*
*       Get ALV grid event and pass the form name to subtotal_text
*       event
*----------------------------------------------------------------------*
*       No Parameter
*----------------------------------------------------------------------*
FORM sub_get_event .
  CONSTANTS : c_formname_subtotal_text TYPE slis_formname VALUE
'SUBTOTAL_TEXT'.
  DATA: l_s_event TYPE slis_alv_event.

  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
    EXPORTING
      i_list_type     = 4
    IMPORTING
      et_events       = i_event
    EXCEPTIONS
      list_type_wrong = 0
      OTHERS          = 0.
* Subtotal
  READ TABLE i_event  INTO l_s_event
                    WITH KEY name = slis_ev_subtotal_text."在类型池中默认的值为SUBTOTAL_TEXT
  IF sy-subrc = 0.
    MOVE c_formname_subtotal_text TO l_s_event-form.
    MODIFY i_event FROM l_s_event INDEX sy-tabix.
  ENDIF.
ENDFORM.                    " sub_get_event
*&---------------------------------------------------------------------*
*&      Form  sub_alv_report_display
*&---------------------------------------------------------------------*
*       For ALV Report Display
*----------------------------------------------------------------------*
*       No Parameter
*----------------------------------------------------------------------*
FORM sub_alv_report_display .
  DATA: l_repid TYPE syrepid .
  l_repid = sy-repid .
* This function module for displaying the ALV report
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program       = l_repid
      i_callback_top_of_page   = 'SUB_ALV_TOP_OF_PAGE'
      is_layout                = wa_layout
      it_fieldcat              = i_fieldcat
      it_sort = i_sort
      it_events                = i_event
      i_default                = 'X'
      i_save                   = 'A'
    TABLES
      t_outtab                 = i_ekpo
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.
  IF sy-subrc <> 0.
*    MESSAGE i000 WITH 'Error in ALV report display'(055).
  ENDIF.
ENDFORM.                    " sub_alv_report_display
*&---------------------------------------------------------------------*
*       FORM sub_alv_top_of_page
*---------------------------------------------------------------------*
*       Call ALV top of page
*---------------------------------------------------------------------*
*       No parameter
*---------------------------------------------------------------------*
FORM sub_alv_top_of_page.                                   "#EC CALLED
* To write header for the ALV
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = i_alv_top_of_page.

ENDFORM.                    "alv_top_of_page
*&---------------------------------------------------------------------*
*&      Form  subtotal_text
*&---------------------------------------------------------------------*
*       Build subtotal text
*----------------------------------------------------------------------*
*       P_total  Total
*       p_subtot_text Subtotal text info
*----------------------------------------------------------------------*
FORM subtotal_text CHANGING
               p_total TYPE any
               p_subtot_text TYPE slis_subtot_text.

* Material level sub total
  IF p_subtot_text-criteria = 'MATNR'.
    p_subtot_text-display_text_for_subtotal
    = 'Material level total'(009).
  ENDIF.
* Plant level sub total
  IF p_subtot_text-criteria = 'WERKS'.
    p_subtot_text-display_text_for_subtotal = 'Plant level total'(010).
  ENDIF.

ENDFORM.                    "subtotal_text