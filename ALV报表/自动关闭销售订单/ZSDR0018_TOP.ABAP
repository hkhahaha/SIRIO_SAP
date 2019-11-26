*&---------------------------------------------------------------------*
*& 包含               ZSDR0018_TOP
*&---------------------------------------------------------------------*
TYPES:BEGIN OF ty_alv,
        vkorg         TYPE vbak-vkorg  , "  销售组织
        vtweg         TYPE vbak-vtweg  , "  分销渠道
        vbeln         TYPE vbak-vbeln  , "  销售订单
        ernam         TYPE vbak-ernam  , "  创建人
        kunnr         TYPE vbak-kunnr  , "  客户
        name_org(120) TYPE c , "  客户描述
        auart         TYPE vbak-auart  , "  凭证类型
        bezei         TYPE tvakt-bezei , "  凭证类型描述
        posnr         TYPE vbap-posnr  , "  行项目
        matnr         TYPE vbap-matnr  , "  物料
        arktx         TYPE vbap-arktx  , "  物料描述
        kwmeng        TYPE vbap-kwmeng , "  数量
        number1       TYPE p DECIMALS 3, "  已过账数量（销售单位）
        number2       TYPE p DECIMALS 3, "  当前库存数量（销售单位）
        number3       TYPE p DECIMALS 3, "  已入库数量：
        wempf         TYPE mseg-wempf, "收货方
        zieme         TYPE vbap-zieme  , "  销售单位
      END OF ty_alv.

"定义用于存储VBAP、VBAK、VBEP字段的结构
TYPES:BEGIN OF ty_vbapk,
        kunnr  TYPE  vbak-kunnr,
        ernam  TYPE  vbak-ernam,
        vbeln  TYPE  vbak-vbeln,
        posnr  TYPE  vbap-posnr,
        vkorg  TYPE  vbak-vkorg,
        vtweg  TYPE  vbak-vtweg,
        auart  TYPE  vbak-auart,
        matnr  TYPE  vbap-matnr,
        arktx  TYPE  vbap-arktx,
        kwmeng TYPE  vbap-kwmeng,
        zieme  TYPE  vbap-zieme,
      END OF ty_vbapk.