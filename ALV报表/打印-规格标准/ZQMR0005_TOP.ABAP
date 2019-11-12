*&---------------------------------------------------------------------*
*& 包含               ZQMR0005_TOP
*&---------------------------------------------------------------------*



DATA: gs_output TYPE ZQMS_GGBZ_HEAD,
      gt_output TYPE TABLE OF ZQMS_GGBZ_HEAD,
      gs_qpmk   LIKE ZQMS_GGBZ_DETAIL,
      gt_qpmk type TABLE OF ZQMS_GGBZ_DETAIL,
      gt_qpmk_print   LIKE TABLE OF ZQMS_GGBZ_DETAIL,
      GT_qddrt type TABLE OF qddrt .