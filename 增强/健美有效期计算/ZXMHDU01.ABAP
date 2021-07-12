*&---------------------------------------------------------------------*
*& 包含               ZXMHDU01
*&---------------------------------------------------------------------*
*
*以公司的实际业务需求，重新计算货架寿命到期日
*计算公式：货架寿命到日期 = 生产日期 + 总货架寿命 / 30 -  1(SLED 舍入规则为"-"时才减)
*c_geshb = i_geshb "总货架寿命
*c_hsdat = i_hsdat "生产日期
*c_rdmhd = i_rdmhd "SLED 舍入规则,在物料主数据的工厂数据/储存1视图的字段RDMHD维护
*c_calc = calc "计算标识 X-计算，SPACE-不计算

IF c_calc  IS NOT INITIAL.
    DATA: lv_month       TYPE dlymo,
          lv_calc_date   TYPE p0001-begda,
          lv_flag        TYPE flag,
          lv_mod         TYPE i,
          lv_hsdat_date  TYPE char4,
          lv_calc_date_4 TYPE char4,
          lv_e_date      LIKE sy-datum.
    TRY .
        IF c_rdmhd = ''. "SLED舍入规则为''时，直接加天数
          "计算货架寿命到日期 = 生产日期 + 总货架寿命天数
          CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
            EXPORTING
              date      = c_hsdat  "生产日期
              days      = c_geshb "天数加N或者减N  不加也不减 写0
              months    = 0 "月份加N或者减N 不加也不减 写0
              signum    = '+'  "可以是+或者-
              years     = 0 "年份加N或者减N 不加也不减 写0
            IMPORTING
              calc_date = lv_calc_date. "货架寿命到日期
  
        ELSEIF c_rdmhd = '-'."SLED舍入规则为'-'时
  
          lv_month = floor( c_geshb / 30 )."总货架寿命折算成最小月份数
          "计算货架寿命到日期 = 生产日期 + 总货架寿命（月份）
          CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
            EXPORTING
              date      = c_hsdat  "生产日期
              days      = 0 "天数加N或者减N  不加也不减 写0
              months    = lv_month "月份加N或者减N 不加也不减 写0
              signum    = '+'  "可以是+或者-
              years     = 0 "年份加N或者减N 不加也不减 写0
            IMPORTING
              calc_date = lv_calc_date. "货架寿命到日期
  
          "当SLED 舍入规则为"-"时，货架寿命到日期 = 货架寿命到期日 - 1
          CLEAR:lv_mod, lv_flag.
  
  
          CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
            EXPORTING
              date      = lv_calc_date "货架寿命到日期
              days      = 1 "天数加N或者减N  不加也不减 写0
              months    = 0 "月份加N或者减N 不加也不减 写0
              signum    = '-'  "可以是+或者-
              years     = 0 "年份加N或者减N 不加也不减 写0
            IMPORTING
              calc_date = lv_calc_date. "货架寿命到日期
  
  ***1.如果生产日期是1/3/5/7/8/10/12 月份的31号（1-31、3-31、5-31、7-31、8-31、10-31、12-31），有效期按《有效期=生产日期+[有效期天数/30](取整)-1》运算后为2/4/6/9/11月份（即2-28,2-29,4-30,6-30,9-30,11-30），有效期在原有基础上再减1天——即2-27,2-28,4-29,6-29,9-29,11-29；
  ***2.如果生产日期是1,3,4,5,6,7,8,9,10,11,12月份的30号（1-30，3-30,4-30,5-30,6-30,7-30,8-30,9-30,10-30,11-30,12-30号），有效期按《有效期=生产日期+[有效期天数/30](取整)-1》运算后为2月份（即2-28，2-29号），有效期在原有基础上再减1天——即2-27,2-28；
  ***3.如果生产日期是2月29号，有效期按《有效期=生产日期+[有效期天数/30](取整)-1》运算后为2月份（即2-28号），有效期在原有基础上再减1天——即2-27
  ***4.如果生产日期是1/3/4/5/6/7/8/9/10/11/12月份的29号（1-29,3-29,4-29,5-29,6-29,7-29,8-29,9-29，10-29,11-29,12-29），有效期按《有效期=生产日期+[有效期天数/30](取整)-1》运算后为2月份（即2-28），且有效期年份为平年（年份/4不整除） ，则有效期在原有基础上再减1天
  
          lv_hsdat_date = c_hsdat+4(4). "生产日期（MMDD)
          lv_calc_date_4 = lv_calc_date+4(4). "计算后的货架寿命到期日
          lv_mod = CONV i( lv_calc_date(4) ) MOD 4."计算是否为平年：非0-平年,0-闰年
          IF ( lv_hsdat_date = '0131' OR lv_hsdat_date = '0331' OR lv_hsdat_date = '0531' OR lv_hsdat_date = '0731' OR lv_hsdat_date = '0831' OR lv_hsdat_date = '1031' OR lv_hsdat_date = '1231' )
           AND ( lv_calc_date_4 = '0228' OR lv_calc_date_4 = '0229' OR lv_calc_date_4 = '0430' OR lv_calc_date_4 = '0630' OR lv_calc_date_4 = '0930' OR lv_calc_date_4 = '1130' ).
            lv_flag = abap_true.
  
  
          ELSEIF ( lv_hsdat_date = '0130' OR lv_hsdat_date = '0330' OR lv_hsdat_date = '0430' OR lv_hsdat_date = '0530' OR lv_hsdat_date = '0630' OR lv_hsdat_date = '0730'
                     OR lv_hsdat_date = '0830' OR lv_hsdat_date = '0930' OR lv_hsdat_date = '1030' OR lv_hsdat_date = '1130' OR lv_hsdat_date = '1230' )
              AND ( lv_calc_date_4 = '0228' OR lv_calc_date_4 = '0229'  ).
            lv_flag = abap_true.
  
          ELSEIF  lv_hsdat_date = '0229'  AND  lv_calc_date_4 = '0228' .
            lv_flag = abap_true.
  
          ELSEIF ( lv_hsdat_date = '0129' OR lv_hsdat_date = '0329' OR lv_hsdat_date = '0429' OR lv_hsdat_date = '0529' OR lv_hsdat_date = '0629' OR lv_hsdat_date = '0729'
                     OR lv_hsdat_date = '0829' OR lv_hsdat_date = '0929' OR lv_hsdat_date = '1029' OR lv_hsdat_date = '1129' OR lv_hsdat_date = '1229' )
                 AND  lv_calc_date_4 = '0228'
                 AND  lv_mod IS NOT INITIAL.
            lv_flag = abap_true.
          ENDIF.
  
  
          IF lv_flag = abap_true.
            CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
              EXPORTING
                date      = lv_calc_date "货架寿命到日期
                days      = 1 "天数加N或者减N  不加也不减 写0
                months    = 0 "月份加N或者减N 不加也不减 写0
                signum    = '-'  "可以是+或者-
                years     = 0 "年份加N或者减N 不加也不减 写0
              IMPORTING
                calc_date = lv_calc_date. "货架寿命到日期
          ENDIF.
  
        ENDIF.
  
        c_mhdat = lv_calc_date.
        "初始化传入的货架寿命天数，是为了不再执行外面的ADD_TIME_TO_DATE函数，避免货架寿命到期日再次被计算
        CLEAR c_geshb.
  
  
      CATCH cx_root.
  
    ENDTRY.
  
  
  ENDIF.