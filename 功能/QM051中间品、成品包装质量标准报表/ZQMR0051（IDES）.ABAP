*&---------------------------------------------------------------------*
*& Report ZQMR0051
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZQMR0051.

TABLES:ZQMT0051A,ZQMT0051B.
SELECTION-SCREEN BEGIN OF BLOCK BLK WITH FRAME TITLE TEXT-001.
PARAMETERS: R_01 RADIOBUTTON GROUP RG1 USER-COMMAND CRT DEFAULT 'X',"创建通知单
            R_02 RADIOBUTTON GROUP RG1.
SELECTION-SCREEN END OF BLOCK BLK.

SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN COMMENT 1(8) TEXT-003.
  SELECTION-SCREEN POSITION 10.
  PARAMETERS R_03 RADIOBUTTON GROUP rg2 USER-COMMAND CRT DEFAULT 'X'.
  SELECTION-SCREEN COMMENT 14(4) TEXT-004 FOR FIELD R_03.
  SELECTION-SCREEN POSITION 20.
  PARAMETERS R_04 RADIOBUTTON GROUP rg2.
  SELECTION-SCREEN COMMENT 21(4) TEXT-005 FOR FIELD R_04.
  SELECTION-SCREEN POSITION 28.
  PARAMETERS R_05 RADIOBUTTON GROUP rg2.
  SELECTION-SCREEN COMMENT 32(4) TEXT-006 FOR FIELD R_05.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF BLOCK BLK1 WITH FRAME TITLE TEXT-002.
PARAMETERS:P_FILE1 TYPE CHAR128 MODIF ID M1 OBLIGATORY.
*           P_FILE2 TYPE CHAR128 MODIF ID M2 OBLIGATORY.
SELECT-OPTIONS:ZGONGXU FOR ZQMT0051A-ZGONGXU MODIF ID M3,"工序
               ZJIXING FOR ZQMT0051A-ZJIXING MODIF ID M3,"剂型/包装形式
               ZPARTNER FOR ZQMT0051A-ZPARTNER MODIF ID M3,"客户代码
               ZNAME FOR ZQMT0051A-ZNAME MODIF ID M3,"客户名称
               ZITEM FOR ZQMT0051A-ZITEM MODIF ID M3,"项目
               ZGRADE FOR ZQMT0051A-ZGRADE MODIF ID M3,"标准等级
               ZPART FOR ZQMT0051A-ZPART MODIF ID M3."标准类别
SELECT-OPTIONS:ZBU_SORT FOR ZQMT0051B-ZBU_SORT MODIF ID M4,"客户检索项
               ZNUM2 FOR ZQMT0051B-ZNUM MODIF ID M4,"客户代码
               ZNAME2  FOR ZQMT0051B-ZNAME MODIF ID M4,"客户名称
               ZRANGE2 FOR ZQMT0051B-ZRANGE MODIF ID M4."质量标准等级
SELECTION-SCREEN END OF BLOCK BLK1.

AT SELECTION-SCREEN OUTPUT.
*修改屏幕
PERFORM frm_change_screen.

FORM frm_change_screen.
LOOP AT SCREEN.
  CASE SCREEN-group1.
  WHEN 'M1'.
    IF ( R_01 = 'X' OR R_02 = 'X') AND R_03 = 'X'.
      SCREEN-ACTIVE = '1'.
    ELSE.
      SCREEN-ACTIVE = '0'.
    ENDIF.
*  WHEN 'M2'.
*    IF R_02 = 'X' AND R_03 = 'X'.
*      SCREEN-ACTIVE = '1'.
*    ELSE.
*      SCREEN-ACTIVE = '0'.
*    ENDIF.
  WHEN 'M3'.
    IF r_01 = 'X' AND ( R_04 = 'X' OR R_05 = 'X').
      SCREEN-active = '1'.
    ELSE.
      SCREEN-active = '0'.
    ENDIF.
  WHEN 'M4'.
    IF r_02 = 'X' AND ( R_04 = 'X' OR R_05 = 'X').
      SCREEN-active = '1'.
    ELSE.
      SCREEN-active = '0'.
    ENDIF.
  ENDCASE.

  MODIFY SCREEN.
ENDLOOP.
ENDFORM.