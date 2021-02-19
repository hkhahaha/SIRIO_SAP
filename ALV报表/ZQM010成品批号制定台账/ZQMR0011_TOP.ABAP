*&---------------------------------------------------------------------*
*& 包含               ZQMR0011_TOP
*&---------------------------------------------------------------------*
TYPE-POOLS: SLIS.

TABLES: MCHA, BUT000, AFKO, AFPO, MSEG.

TYPES: BEGIN OF GTY_OUT,
         AUFNR     TYPE AFPO-AUFNR,
         POSNR     TYPE AFPO-POSNR,
         PWERK     TYPE AFPO-PWERK,
         CHARG     TYPE AFPO-CHARG,
         ERSDA     TYPE MCHA-ERSDA,           " 日期
         KDAUF     TYPE AFPO-KDAUF,           " 销售订单号
         KDPOS     TYPE AFPO-KDPOS,           " 销售订单行号
         BU_SORT1  TYPE BUT000-BU_SORT1,      " 客户
         MATNR     TYPE AFPO-MATNR,           " 产品代码
         MAKTX     TYPE MAKT-MAKTX,           " 产品名称
         GROES     TYPE CHAR70,               " 规格型号
         ZDYPC     TYPE CHAR70,               " 包装批号（打印）
         MENGE     TYPE MSEG-MENGE,           " 生产批量
         PSMNG     TYPE AFPO-PSMNG,           " 包装数量
         MEINS     TYPE AFPO-MEINS,           " 基本计量单位
         ZPACK_STA TYPE AFPO-PSMNG,           " 包装数量(瓶盒)
         BRGEW     TYPE MARA-BRGEW,           " 毛重
         NTGEW     TYPE MARA-NTGEW,           " 净重
         ZSCRQ     TYPE DATUM,                " 生产日期
         MHDHB     TYPE MARA-MHDHB,           " 保质期（天）
         ZYXQZ     TYPE DATUM,                " 有效期至
         ZBQBB     TYPE CHAR70,               " 标签版本
         ZCHEJ     TYPE ZPPT0001-ZCHEJ,       " 车间
         SCRQA     TYPE CHAR70,               " 生产日期（PVC/瓶）(打印)
         SCRQB     TYPE CHAR70,               " 生产日期（袋）(打印)
         SCRQC     TYPE CHAR70,               " 生产日期（盒）(打印)
         SCRQD     TYPE CHAR70,               " 生产日期（礼盒）(打印)
         YXQZA     TYPE CHAR70,               " 有效期至（PVC/瓶）(打印)
         YXQZB     TYPE CHAR70,               " 有效期至（袋）(打印)
         YXQZC     TYPE CHAR70,               " 有效期至（盒）(打印)
         YXQZD     TYPE CHAR70,               " 有效期至（礼盒）(打印)
         YTGBQ     TYPE CHAR70,               " 已提供标签
         SCHARG    TYPE MSEG-CHARG,           " 工艺批号
         SMATNR    TYPE MSEG-MATNR,           " S产品代码
         SMAKTX    TYPE MAKT-MAKTX,           " S产品名称
         SWERKS    TYPE MSEG-WERKS,           " S工厂
         SGROES    TYPE MARA-GROES,           " S规格型号
       END OF GTY_OUT.

DATA: GT_OUT TYPE STANDARD TABLE OF GTY_OUT,
      GS_OUT TYPE GTY_OUT.

DATA: GS_LAYOUT   TYPE  SLIS_LAYOUT_ALV,
      GT_FIELDCAT TYPE  SLIS_T_FIELDCAT_ALV.