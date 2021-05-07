REPORT zhkalv5.

TYPE-POOLS: slis, icon.

TABLES: sflight, spfli, scarr, sbook.
*   Define okcode
DATA: ok_code TYPE sy-ucomm.

*   Define objects used in screen
DATA: go_split_container TYPE REF TO cl_gui_splitter_container.
DATA: go_container TYPE REF TO cl_gui_custom_container.
DATA: go_container1 TYPE REF TO cl_gui_container.
DATA: go_container2 TYPE REF TO cl_gui_container.
DATA: go_container3 TYPE REF TO cl_gui_container.
DATA: go_container4 TYPE REF TO cl_gui_container.
DATA: go_alv_control1 TYPE REF TO cl_gui_alv_grid.
DATA: go_alv_control2 TYPE REF TO cl_gui_alv_grid.
DATA: go_alv_control3 TYPE REF TO cl_gui_alv_grid.
DATA: go_alv_control4 TYPE REF TO cl_gui_alv_grid.

*   Define internal tables and structures
DATA: BEGIN OF gs_sflight,
        carrid TYPE sflight-carrid,
        connid TYPE sflight-connid,
        fldate TYPE sflight-fldate,
        price  TYPE sflight-price,
      END   OF gs_sflight.

DATA: BEGIN OF gs_spfli,
        carrid   TYPE spfli-carrid,
        connid   TYPE spfli-connid,
        cityfrom TYPE spfli-cityfrom,
        cityto   TYPE spfli-cityto,
      END   OF gs_spfli.

DATA: BEGIN OF gs_scarr,
        carrid   TYPE scarr-carrid,
        carrname TYPE scarr-carrname,
      END   OF gs_scarr.

DATA: BEGIN OF gs_sbook,
        carrid   TYPE sbook-carrid,
        connid   TYPE sbook-connid,
        bookid   TYPE sbook-bookid,
        customid TYPE sbook-customid,
      END   OF gs_sbook.

*   Define alv components
DATA: gs_field_cat1 TYPE lvc_s_fcat.
DATA: gs_field_cat2 TYPE lvc_s_fcat.
DATA: gs_field_cat3 TYPE lvc_s_fcat.
DATA: gs_field_cat4 TYPE lvc_s_fcat.
DATA: gt_field_cat1 TYPE lvc_t_fcat.
DATA: gt_field_cat2 TYPE lvc_t_fcat.
DATA: gt_field_cat3 TYPE lvc_t_fcat.
DATA: gt_field_cat4 TYPE lvc_t_fcat.

*   Define displayed data
DATA: gt_sflight LIKE STANDARD TABLE OF gs_sflight,
      gt_spfli   LIKE STANDARD TABLE OF gs_spfli,
      gt_scarr   LIKE STANDARD TABLE OF gs_scarr,
      gt_sbook   LIKE STANDARD TABLE OF gs_sbook.

START-OF-SELECTION.
  PERFORM sub_retrieve_data.
  PERFORM sub_alv_data.
  CALL SCREEN 9100.
*&---------------------------------------------------------------------*
*&      Form  SUB_RETRIEVE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sub_retrieve_data .

  SELECT
    mandt
    carrid
    connid
    fldate
    price
    currency
    planetype
    seatsmax
    seatsocc
    paymentsum
    seatsmax_b
    seatsocc_b
    seatsmax_f
    seatsocc_f


    INTO CORRESPONDING FIELDS OF TABLE gt_sflight
    FROM sflight.

  SELECT
    carrid
    connid
    cityfrom
    cityto
    INTO CORRESPONDING FIELDS OF TABLE gt_spfli
    FROM spfli.

  SELECT
    carrid
    carrname
    INTO CORRESPONDING FIELDS OF TABLE gt_scarr
    FROM scarr.

  SELECT
    carrid
    connid
    bookid
    customid
    INTO CORRESPONDING FIELDS OF TABLE gt_sbook
    FROM sbook.
ENDFORM.                    " SUB_RETRIEVE_DATA
*&---------------------------------------------------------------------*
*&      Form  SUB_ALV_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM sub_alv_data .
*   Build field catelog 1
  gs_field_cat1-col_pos = 1.
  gs_field_cat1-fieldname = 'CARRID'.
  gs_field_cat1-scrtext_m = 'airline code'.
  APPEND gs_field_cat1 TO gt_field_cat1.
  gs_field_cat1-col_pos = 2.
  gs_field_cat1-fieldname = 'CONNID'.
  gs_field_cat1-scrtext_m = 'connection number'.
  APPEND gs_field_cat1 TO gt_field_cat1.
  gs_field_cat1-col_pos = 3.
  gs_field_cat1-fieldname = 'FLDATE'.
  gs_field_cat1-scrtext_m = 'flight date'.
  APPEND gs_field_cat1 TO gt_field_cat1.
  gs_field_cat1-col_pos = 4.
  gs_field_cat1-fieldname = 'PRICE'.
  gs_field_cat1-scrtext_m = 'flight price'.
  APPEND gs_field_cat1 TO gt_field_cat1.
  gs_field_cat1-col_pos = 5.
  gs_field_cat1-fieldname = 'PRICE'.
  gs_field_cat1-scrtext_m = 'flight price'.
  APPEND gs_field_cat1 TO gt_field_cat1.
  gs_field_cat1-col_pos = 6.
  gs_field_cat1-fieldname = 'PRICE'.
  gs_field_cat1-scrtext_m = 'flight price'.
  APPEND gs_field_cat1 TO gt_field_cat1.
  gs_field_cat1-col_pos = 7.
  gs_field_cat1-fieldname = 'PRICE'.
  gs_field_cat1-scrtext_m = 'flight price'.
  APPEND gs_field_cat1 TO gt_field_cat1.
  gs_field_cat1-col_pos = 8.
  gs_field_cat1-fieldname = 'PRICE'.
  gs_field_cat1-scrtext_m = 'flight price'.
  APPEND gs_field_cat1 TO gt_field_cat1.



*   Build field catelog 2
  gs_field_cat2-col_pos = 1.
  gs_field_cat2-fieldname = 'CARRID'.
  gs_field_cat2-scrtext_m = 'airline code'.
  APPEND gs_field_cat2 TO gt_field_cat2.
  gs_field_cat2-col_pos = 2.
  gs_field_cat2-fieldname = 'CONNID'.
  gs_field_cat2-scrtext_m = 'connection number'.
  APPEND gs_field_cat2 TO gt_field_cat2.
  gs_field_cat2-col_pos = 3.
  gs_field_cat2-fieldname = 'CITYFROM'.
  gs_field_cat2-scrtext_m = 'city from'.
  APPEND gs_field_cat2 TO gt_field_cat2.
  gs_field_cat2-col_pos = 4.
  gs_field_cat2-fieldname = 'CITYTO'.
  gs_field_cat2-scrtext_m = 'city to'.
  APPEND gs_field_cat2 TO gt_field_cat2.

*   Build field catelog 3
  gs_field_cat3-col_pos = 1.
  gs_field_cat3-fieldname = 'CARRID'.
  gs_field_cat3-scrtext_m = 'airline code'.
  APPEND gs_field_cat3 TO gt_field_cat3.
  gs_field_cat3-col_pos = 2.
  gs_field_cat3-fieldname = 'CONNID'.
  gs_field_cat3-scrtext_m = 'connection number'.
  APPEND gs_field_cat3 TO gt_field_cat3.

*   Build field catelog 4
  gs_field_cat4-col_pos = 1.
  gs_field_cat4-fieldname = 'CARRID'.
  gs_field_cat4-scrtext_m = 'airline code'.
  APPEND gs_field_cat4 TO gt_field_cat4.
  gs_field_cat4-col_pos = 2.
  gs_field_cat4-fieldname = 'CONNID'.
  gs_field_cat4-scrtext_m = 'connection number'.
  APPEND gs_field_cat4 TO gt_field_cat4.
  gs_field_cat4-col_pos = 3.
  gs_field_cat4-fieldname = 'BOOKID'.
  gs_field_cat4-scrtext_m = 'book id'.
  APPEND gs_field_cat4 TO gt_field_cat4.
  gs_field_cat4-col_pos = 4.
  gs_field_cat4-fieldname = 'CUSTOMID'.
  gs_field_cat4-scrtext_m = 'custom id'.
  APPEND gs_field_cat4 TO gt_field_cat4.
ENDFORM.                    " SUB_ALV_DATA
*&---------------------------------------------------------------------*
*&      Module  ALV_DISPLAY  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE alv_display OUTPUT.
  IF go_split_container IS INITIAL.

    CREATE OBJECT go_container
      EXPORTING
        container_name = 'GO_CONTAINER'.

    CREATE OBJECT go_split_container
      EXPORTING
        link_dynnr = sy-dynnr
        link_repid = sy-repid
        parent     = go_container
        rows       = 2
        columns    = 2
        name       = 'GO_SPLIT_CONTAINER'.



    CALL METHOD go_split_container->get_container
      EXPORTING
        row       = 1
        column    = 1
      RECEIVING
        container = go_container1.

    CALL METHOD go_split_container->get_container
      EXPORTING
        row       = 1
        column    = 2
      RECEIVING
        container = go_container2.

    CALL METHOD go_split_container->get_container
      EXPORTING
        row       = 2
        column    = 1
      RECEIVING
        container = go_container3.

    CALL METHOD go_split_container->get_container
      EXPORTING
        row       = 2
        column    = 2
      RECEIVING
        container = go_container4.

    CREATE OBJECT go_alv_control1
      EXPORTING
        i_parent = go_container1.

    CREATE OBJECT go_alv_control2
      EXPORTING
        i_parent = go_container2.

    CREATE OBJECT go_alv_control3
      EXPORTING
        i_parent = go_container3.

    CREATE OBJECT go_alv_control4
      EXPORTING
        i_parent = go_container4.

    CALL METHOD go_alv_control1->set_table_for_first_display
      CHANGING
        it_outtab       = gt_sflight
        it_fieldcatalog = gt_field_cat1.

    CALL METHOD go_alv_control2->set_table_for_first_display
      CHANGING
        it_outtab       = gt_spfli
        it_fieldcatalog = gt_field_cat2.

    CALL METHOD go_alv_control3->set_table_for_first_display
      CHANGING
        it_outtab       = gt_scarr
        it_fieldcatalog = gt_field_cat3.

    CALL METHOD go_alv_control4->set_table_for_first_display
      CHANGING
        it_outtab       = gt_sbook
        it_fieldcatalog = gt_field_cat4.

  ELSE.
    CALL METHOD go_alv_control1->refresh_table_display.
    CALL METHOD go_alv_control2->refresh_table_display.
    CALL METHOD go_alv_control3->refresh_table_display.
    CALL METHOD go_alv_control4->refresh_table_display.
  ENDIF.
ENDMODULE.                 " ALV_DISPLAY  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_9100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_9100 OUTPUT.
  SET PF-STATUS 'STATUS9100'.
ENDMODULE.                 " STATUS_9100  OUTPUT

MODULE exit_program  INPUT.
  LEAVE PROGRAM.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9100 INPUT.
  ok_code = sy-ucomm.
  CASE ok_code.
    WHEN 'PBAK'.
      CALL METHOD go_alv_control1->free.
      CALL METHOD go_alv_control2->free.
      CALL METHOD go_alv_control3->free.
      CALL METHOD go_alv_control4->free.
      CALL METHOD go_container1->free.
      CALL METHOD go_container2->free.
      CALL METHOD go_container3->free.
      CALL METHOD go_container4->free.
      CALL METHOD go_split_container->free.
      CALL METHOD go_container->free.
      LEAVE PROGRAM.
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_9100  INPUT