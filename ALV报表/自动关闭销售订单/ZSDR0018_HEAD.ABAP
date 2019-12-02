*&---------------------------------------------------------------------*
*& 包含               ZSDR0018_HEAD
*&---------------------------------------------------------------------*
DATA:lbkpf LIKE STANDARD TABLE OF bkpf WITH HEADER LINE."标准表格式的内表lbkpf
TABLES:aufm,vbak,vbap.
SELECTION-SCREEN BEGIN OF BLOCK blk WITH FRAME TITLE TEXT-001."定义屏幕
SELECT-OPTIONS:s_budat FOR aufm-budat DEFAULT sy-datum,"生产入库日期"条件输入框，一个for对应一个
s_werks FOR aufm-werks,"工厂"for前面的是字段显示的名称，可以修改，但是注意不要超过8个字符，否则会报错
s_kunnr FOR vbak-kunnr,"客户
s_ernam FOR vbak-ernam,"创建人
s_vbeln FOR vbak-vbeln,"销售订单
s_posnr FOR vbap-posnr,"行项目
s_vkorg FOR vbak-vkorg,"销售组织
s_vtweg FOR vbak-vtweg."分销渠道
"定义单选框
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(20) TEXT-002.
SELECTION-SCREEN POSITION 23.
PARAMETERS flag_a AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN COMMENT 24(8) TEXT-003 FOR FIELD flag_a.
SELECTION-SCREEN POSITION 44.
PARAMETERS flag_b AS CHECKBOX.
SELECTION-SCREEN COMMENT 48(8) TEXT-004 FOR FIELD flag_b.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK blk.