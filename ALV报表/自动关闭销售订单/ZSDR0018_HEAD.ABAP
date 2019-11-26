*&---------------------------------------------------------------------*
*& 包含               ZSDR0018_HEAD
*&---------------------------------------------------------------------*
DATA:lbkpf LIKE STANDARD TABLE OF bkpf WITH HEADER LINE."标准表格式的内表lbkpf
TABLES:AUFM,VBAK,VBAP.
SELECTION-SCREEN BEGIN OF BLOCK blk WITH FRAME TITLE TEXT-001."定义屏幕
  SELECT-OPTIONS:s_BUDAT FOR AUFM-budat,"生产入库日期"条件输入框，一个for对应一个
  s_WERKS FOR AUFM-werks,"工厂"for前面的是字段显示的名称，可以修改，但是注意不要超过8个字符，否则会报错
  s_KUNNR FOR VBAK-kunnr,"客户
  s_ERNAM FOR VBAK-ernam,"创建人
  s_VBELN FOR VBAK-vbeln,"销售订单
  s_POSNR FOR VBAP-posnr,"行项目
  s_VKORG FOR VBAK-vkorg,"销售组织
  s_VTWEG FOR VBAK-vtweg."分销渠道
SELECTION-SCREEN END OF BLOCK blk.