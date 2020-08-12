METHOD get_number_range_next_no.

    DATA: ls_inri TYPE inri.

    ls_inri-object = iv_object.
    ls_inri-nrrangenr = iv_nrrangenr.

    CALL FUNCTION 'NUMBER_RANGE_ENQUEUE'
      EXPORTING
        object           = ls_inri-object
      EXCEPTIONS
        foreign_lock     = 1
        object_not_found = 2
        system_failure   = 3
        OTHERS           = 4.
*  **如果号码范围存在
    IF sy-subrc EQ 0 .
*  ****得到一个号码，
      CALL FUNCTION 'NUMBER_GET_NEXT'
        EXPORTING
          nr_range_nr             = ls_inri-nrrangenr
          object                  = ls_inri-object
        IMPORTING
          number                  = ev_number
*         quantity                = quant
*         returncode              = code
        EXCEPTIONS
          interval_not_found      = 1
          number_range_not_intern = 2
          object_not_found        = 3
          quantity_is_0           = 4
          quantity_is_not_1       = 5
          interval_overflow       = 6
          buffer_overflow         = 7
          OTHERS                  = 8.
*  ***将号码累加
      CALL FUNCTION 'NUMBER_RANGE_DEQUEUE'
        EXPORTING
          object           = ls_inri-object
        EXCEPTIONS
          object_not_found = 1
          OTHERS           = 2.
    ENDIF .

  ENDMETHOD.