FUNCTION z_mm_sap2wms_out_fun_out.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     REFERENCE(IM_HEADER) TYPE REF TO  IF_PURCHASE_ORDER_MM
*"  EXPORTING
*"     REFERENCE(CH_FAILED) TYPE  MMPUR_BOOL
*"----------------------------------------------------------------------
  DATA: lr_items TYPE purchase_order_items,
        lr_item  TYPE purchase_order_item,
        ls_item  TYPE mepoitem,
        lt_item  TYPE TABLE OF mepoitem,
        ls_head  TYPE mepoheader.

  DATA: lt_poh  TYPE TABLE OF zmmwms_poh,
        lt_pol  TYPE TABLE OF zmmwms_pol,
        lt_temp TYPE TABLE OF zmmaps_temp.

  DATA: ls_input  TYPE zszmmwms_poh_pol,
        ls_output TYPE zsmessage_out.

  DATA:li_items TYPE REF TO if_purchase_order_item_mm,
       re_data  TYPE mepoitem,
       lt_resb  TYPE TABLE OF resb,
       ls_resb  TYPE resb.


*抬头信息
  ls_head = im_header->get_data( ).

  lr_items = im_header->get_items( ).

*行项目信息
  LOOP AT lr_items INTO lr_item.
    ls_item = lr_item-item->get_data( ).
    IF ls_item-lgort IS INITIAL.

      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          text_question  = '库存地点为空'
        EXCEPTIONS
          text_not_found = 1
          OTHERS         = 2.
      IF sy-subrc <> 0.
* Implement suitable error handling here
      ENDIF.
      MESSAGE '库存地点为空' TYPE 'E'.
      ch_failed = 'X'.

      " 查询组件里面的批次和库存地点是否为空
    ENDIF.


    SELECT
      *
    INTO CORRESPONDING FIELDS OF TABLE lt_resb
    FROM resb
    WHERE resb~ebeln = ls_item-ebeln
    AND resb~ebelp = ls_item-ebelp.
    IF lt_resb IS NOT INITIAL.
    LOOP AT lt_resb INTO ls_resb.
      IF ls_resb-charg IS INITIAL.
        CALL FUNCTION 'POPUP_TO_CONFIRM'
          EXPORTING
            text_question  = '请输入组件批次'
          EXCEPTIONS
            text_not_found = 1
            OTHERS         = 2.
        IF sy-subrc <> 0.
* Implement suitable error handling here
        ENDIF.
        MESSAGE '请输入组件批次' TYPE 'E'.
        ch_failed = 'X'.
      ELSEIF ls_resb-lgort IS INITIAL.
        CALL FUNCTION 'POPUP_TO_CONFIRM'
          EXPORTING
            text_question  = '请输入组件库存地点'
          EXCEPTIONS
            text_not_found = 1
            OTHERS         = 2.
        IF sy-subrc <> 0.
* Implement suitable error handling here
        ENDIF.
        MESSAGE '请输入组件库存地点' TYPE 'E'.
        ch_failed = 'X'.
      ENDIF.
    ENDLOOP.
    ENDIF.
  ENDLOOP.
  ch_failed = ch_failed.








ENDFUNCTION.