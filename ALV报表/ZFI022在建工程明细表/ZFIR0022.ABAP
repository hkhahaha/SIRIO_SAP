*&---------------------------------------------------------------------*
*& Report ZFIR0022
*&---------------------------------------------------------------------*
*& 作者：    ZHANGZY
*& 开发日期：2019/10/08
*& 描述：   在建工程明细表
*&---------------------------------------------------------------------
*&  版本号 日期      作者        修改描述    功能更改说明书
*&  1.0   20191008  ZHANGZY     初始开发
*&  2.0   20191101  SOARSTARS   BUG修复     1、发票凭证号取数逻辑调整
*&                                            当参考过程（BSEG~AWTYP）等于‘RMRP’时，截取其对象键值（BSEG~AWKEY）前10位作为发票凭证编号
*&                                            当参考过程（BSEG~AWTYP）不等于‘RMRP’时
*&                                                a.将参考过程（BSEG~AWTYP）不等于‘RMRP’的凭证编号（BSEG~BELNR）取出，作为清账凭证（BSEG~AUGBL）进行筛选，
*&                                                  找出所有排除掉凭证编号（BSEG~BELNR）等于清账凭证（BSEG~AUGBL）的凭证编号（BSEG~BELNR）;
*&                                                b.通过上述凭证编号（BSEG~BELNR），找出所有事务类型（BSEG~UMSKS）不为空的清账凭证（BSEG~AUGBL）;
*&                                                c.通过上述清账凭证（BSEG~AUGBL）找出参考过程（BSEG~AWTYP）等于‘RMRP’的行项目，截取其对象键值（BSEG~AWKEY）前10位作为发票凭证编号
*&                                          2、已收发票金额取数逻辑
*&                                             根据发票凭证号（RBKP-BELNR），取RBKP-STBLG=‘ ’，RBKP-RBSTAT=“5”（已过账），取出发票凭证的总发票金额(RBKP~RMWWR);
*&  2.1   20191104  soarstars   BUG修复     1、修正公司代码条件无效的问题
*&  2.2   20191224  soarstars   优化        1、选择条件年度更改为区间
*&                                          2、ALV界面增加年度字段
*&---------------------------------------------------------------------*
REPORT zfir0022 MESSAGE-ID zfi.

INCLUDE: zfir0022_top,
         zfir0022_scr,
         zfir0022_f01.

AT SELECTION-SCREEN.
  PERFORM frm_check_author."权限检查


START-OF-SELECTION.

  PERFORM frm_get_data."查询数据


END-OF-SELECTION.
  IF gt_data IS NOT INITIAL.

    PERFORM frm_data_output."ALV输出

  ELSE.

    MESSAGE s000(zfi) DISPLAY LIKE 'E'.

  ENDIF.