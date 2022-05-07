*&---------------------------------------------------------------------*
*& 包含               ZFIR0022_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form FRM_CHECK_AUTHOR
*&---------------------------------------------------------------------*
*& text 检查公司代码权限
*&---------------------------------------------------------------------*
FORM frm_check_author .

    IF p_bukrs <> ''.
      AUTHORITY-CHECK OBJECT 'F_BKPF_BUK'
        ID 'ACTVT' FIELD '03'
        ID 'BUKRS' FIELD p_bukrs.
      IF sy-subrc <> 0.
        MESSAGE e002(zfi) .
  *      LEAVE LIST-PROCESSING.
      ENDIF.
    ENDIF.
  
  ENDFORM.
  *&---------------------------------------------------------------------*
  *& Form FRM_GET_DATA
  *&---------------------------------------------------------------------*
  *& text 获取ALV显示数据
  *&---------------------------------------------------------------------*
  FORM frm_get_data.
    DATA:lt_lifnr TYPE TABLE OF ty_lifnr.
    DATA lv_auart TYPE aufart.
    DATA:lt_augbl TYPE TABLE OF ty_augbl.
  
  * 获取清账凭证
    SELECT
      bseg~augbl
      INTO TABLE lt_augbl
      FROM bseg
      WHERE bseg~bukrs   EQ p_bukrs
        AND bseg~lifnr   IN s_lifnr
        AND bseg~gjahr   in s_gjahr
        AND bseg~augbl <> ''
        AND bseg~koart   EQ 'K'
        AND bseg~h_blart EQ 'ZP'.
  
  
    CHECK lt_augbl IS NOT INITIAL.
  
  * 根据清账凭证反找获取BSEG信息
    SELECT
      bkpf~gjahr
      bseg~bukrs
      bseg~belnr
      bseg~gjahr
      bseg~koart
      bseg~h_blart
      bseg~lifnr
      bseg~augbl
      bseg~belnr
      bseg~zuonr
      bseg~shkzg
      bseg~dmbtr
      bkpf~monat
      bkpf~budat
      bseg~awtyp
      bseg~awkey
      INTO CORRESPONDING FIELDS OF TABLE gt_data
      FROM bseg
      INNER JOIN bkpf
      ON   bseg~bukrs = bkpf~bukrs
      AND  bseg~belnr = bkpf~belnr
      AND  bseg~gjahr = bkpf~gjahr
      FOR ALL ENTRIES IN lt_augbl
      WHERE augbl = lt_augbl-augbl
      and bseg~bukrs   EQ p_bukrs
      AND bseg~gjahr IN s_gjahr
  *----------------------------------------------------------------------------------------------------------*
  * Title：修正公司代码条件无效的问题
  * Version：2.1
  * Auth：soarstars
  * Date：20191104
  *------------------------------------------start-----------------------------------------------------------*
      AND bseg~bukrs EQ p_bukrs.
  *----------------------------------------------------------------------------------------------------------*
  * Title：修正公司代码条件无效的问题
  * Version：2.1
  * Auth：soarstars
  * Date：20191104
  *------------------------------------------end-------------------------------------------------------------*
  
    CHECK gt_data IS NOT INITIAL.
  
  
  
  
  
  * 获取供应商
    SELECT
      lfa1~mcod1
      lfa1~lifnr
      INTO TABLE lt_lifnr
      FROM lfa1
      FOR ALL ENTRIES IN gt_data
      WHERE lfa1~lifnr = gt_data-lifnr.
  
  
  *----------------------------------------------------------------------------------------------------------*
  * Title：发票凭证号取数逻辑调整
  * Version：2.0
  * Auth：soarstars
  * Date：20191101
  *------------------------------------------start-----------------------------------------------------------*
  
    "将参考过程（BSEG~AWTYP）不等于‘RMRP’的凭证编号（BSEG~BELNR）取出
    SELECT belnr
      INTO TABLE @DATA(lt_not_rmrp)
      FROM bseg
      FOR ALL ENTRIES IN @gt_data
      WHERE belnr = @gt_data-belnr
      AND awtyp <> 'RMRP'
      and bukrs = @p_bukrs
      "and belnr = '2000000252'
      .
  
  
    "找出事务类型BSEG~UMSKS不为空的清账凭证
    SELECT belnr, augbl AS belnr_origin
      INTO TABLE @DATA(lt_belnr_augbl)
      FROM bseg
      FOR ALL ENTRIES IN @lt_not_rmrp
      WHERE augbl = @lt_not_rmrp-belnr
      AND bseg~belnr <> bseg~augbl
      and bukrs = @p_bukrs
      .
  
  
    DATA: BEGIN OF ls_bseg,
            belnr        TYPE belnr_d,
            augbl        TYPE augbl,
            belnr_origin TYPE belnr_d,
          END OF ls_bseg.
    DATA: lt_bseg LIKE TABLE OF ls_bseg.
  
    SELECT belnr, augbl
      INTO CORRESPONDING FIELDS OF TABLE @lt_bseg
      FROM bseg
      FOR ALL ENTRIES IN @lt_belnr_augbl
      WHERE belnr = @lt_belnr_augbl-belnr
      and bukrs = @p_bukrs
      AND bseg~umsks <> ''."事务类型不为空
  
    LOOP AT lt_bseg ASSIGNING FIELD-SYMBOL(<fs_bseg>).
      READ TABLE lt_belnr_augbl INTO DATA(ls_belnr_augbl) WITH KEY belnr = <fs_bseg>-belnr.
      IF sy-subrc EQ 0.
        <fs_bseg>-belnr_origin = ls_belnr_augbl-belnr_origin.
      ENDIF.
    ENDLOOP.
  
    DATA: BEGIN OF ls_bseg_ret,
            belnr        TYPE belnr_d,
            augbl        TYPE augbl,
            awtyp        TYPE awtyp,
            awkey        TYPE awkey,
            belnr_origin TYPE belnr_d,
          END OF ls_bseg_ret.
    DATA: lt_bseg_ret LIKE TABLE OF ls_bseg_ret.
  
    SELECT belnr, augbl, awtyp, awkey
      INTO CORRESPONDING FIELDS OF TABLE @lt_bseg_ret
      FROM bseg
      FOR ALL ENTRIES IN @lt_bseg
      WHERE augbl = @lt_bseg-augbl
      AND awtyp = 'RMRP'
      and augbl <> ''
      and bukrs = @p_bukrs.
  
  
    LOOP AT lt_bseg_ret ASSIGNING FIELD-SYMBOL(<fs_bseg_ret>).
      READ TABLE lt_bseg INTO ls_bseg WITH KEY augbl = <fs_bseg_ret>-augbl.
      IF sy-subrc EQ 0.
        <fs_bseg_ret>-belnr_origin = ls_bseg-belnr_origin.
      ENDIF.
    ENDLOOP.
  *----------------------------------------------------------------------------------------------------------*
  * Title：发票凭证号取数逻辑调整
  * Version：2.0
  * Auth：soarstars
  * Date：20191101
  *--------------------------------------------end-----------------------------------------------------------*
  
  
    LOOP AT gt_data ASSIGNING FIELD-SYMBOL(<fs_data>).
      IF <fs_data>-belnr NE <fs_data>-augbl."排除凭证编号(BSEG~BELNR)等于清账凭证本身(BSEG~AUGBL)的凭证编号
  
        READ TABLE lt_lifnr ASSIGNING FIELD-SYMBOL(<fs_lifnr>) WITH KEY lifnr = <fs_data>-lifnr.
        IF sy-subrc EQ 0.
          <fs_data>-mcod1 = <fs_lifnr>-mcod1."供应商
        ENDIF.
  
  *----------------------------------------------------------------------------------------------------------*
  * Title：发票凭证号取数逻辑调整
  * Desc：根据付款凭证编号（BSEG ~BELNR），判断凭证记录中的参考过程（BSEG~AWTYP）=‘RMRP’时，截取其对象键值（BSEG~AWKEY）前10位作为发票凭证编号
  *       根据付款凭证编号（BSEG ~BELNR），判断凭证记录中的参考过程（BSEG~AWTYP）<>‘RMRP’时，
  *       将凭证编号作为清账凭证对BSEG进行筛选找出对应开票产生的会计凭证A（事务类型不为空）对应的清账凭证B，
  *       在清账凭证B作为清账凭证找出参考过程（BSEG~AWTYP）等于‘RMRP’，截取其对象键值（BSEG~AWKEY）前10位作为发票凭证编号
  * Auth：soarstars
  * Date：20191030
  *------------------------------------------start-----------------------------------------------------------*
        IF <fs_data>-awtyp = 'RMRP'.
          <fs_data>-zbelnr = <fs_data>-awkey(10).
  
        ELSE.
          READ TABLE lt_bseg_ret INTO ls_bseg_ret WITH KEY belnr_origin = <fs_data>-augbl.
          IF sy-subrc EQ 0.
            <fs_data>-zbelnr = ls_bseg_ret-awkey(10).
          ENDIF.
        ENDIF.
  *----------------------------------------------------------------------------------------------------------*
  * Title：发票凭证号取数逻辑调整
  * Desc：根据付款凭证编号（BSEG ~BELNR），判断凭证记录中的参考过程（BSEG~AWTYP）=‘RMRP’时，截取其对象键值（BSEG~AWKEY）前10位作为发票凭证编号
  *       根据付款凭证编号（BSEG ~BELNR），判断凭证记录中的参考过程（BSEG~AWTYP）<>‘RMRP’时，
  *       将凭证编号作为清账凭证对BSEG进行筛选找出对应开票产生的会计凭证A（事务类型不为空）对应的清账凭证B，
  *       在清账凭证B作为清账凭证找出参考过程（BSEG~AWTYP）等于‘RMRP’，截取其对象键值（BSEG~AWKEY）前10位作为发票凭证编号
  * Auth：soarstars
  * Date：20191030
  *--------------------------------------------end-----------------------------------------------------------*
  
        IF <fs_data>-zuonr IS NOT INITIAL.
          "排除凭证编号等于清账凭证号的凭证后 通过拆分分配号获取采购凭证信息 若该条数据能在EKKN中获取到
          "记录此时的清账凭证 再次根据该清账凭证反找凭证编号 并基于此条记录算付款金额字段
          SELECT SINGLE ekkn~aufnr
            INTO <fs_data>-aufnr
            FROM ekkn
            WHERE ebeln = <fs_data>-zuonr(10)
              AND ebelp = <fs_data>-zuonr+10(5)
  *            and bukrs = <fs_data>-bukrs
            .
          IF <fs_data>-aufnr IS NOT INITIAL."验证分配号成功 同时得到内部订单号
            "根据当前清账凭证反找BSEG中 凭证号=清账凭证号 的数据计算付款金额
            <fs_data>-belnr = <fs_data>-augbl.
            SELECT SINGLE
              bseg~shkzg
              bseg~dmbtr
            INTO CORRESPONDING FIELDS OF <fs_data>
            FROM bseg
            WHERE belnr = <fs_data>-augbl
              AND augbl = <fs_data>-augbl
            AND bukrs = p_bukrs.
  
            IF <fs_data>-shkzg EQ 'S'.
              <fs_data>-dmbtr = <fs_data>-dmbtr.
            ELSEIF <fs_data>-shkzg EQ 'H'.
              <fs_data>-dmbtr = - <fs_data>-dmbtr.
            ENDIF.
            <fs_data>-ebeln = <fs_data>-zuonr(10).
            <fs_data>-ebelp = <fs_data>-zuonr+10(5).
            PERFORM frm_get_ekko_info USING <fs_data>.
          ENDIF.
        ELSE." 验证分配号失败 此时根据凭证号重新查询凭证号 找到KTOSL=WRX的数据
  
          SELECT SINGLE
            bseg~belnr
            bseg~ebeln
            bseg~ebelp
            bseg~ktosl
            INTO CORRESPONDING FIELDS OF <fs_data>
            FROM bseg
            WHERE belnr = <fs_data>-belnr
            AND bseg~ktosl EQ 'WRX'.
  
          PERFORM frm_get_ekko_info USING <fs_data>.
  
          SELECT SINGLE ekkn~aufnr
            INTO <fs_data>-aufnr
            FROM ekkn
            WHERE ebeln = <fs_data>-ebeln
              AND ebelp = <fs_data>-ebelp.
          "--------------------NEW ADD IN 20191025 BY ABAP10-----
  
          SELECT SINGLE
            shkzg
            dmbtr
            INTO CORRESPONDING FIELDS OF <fs_data>
          FROM bseg
          WHERE belnr = <fs_data>-augbl
            AND augbl = <fs_data>-augbl
            AND bukrs = p_bukrs.
  
          IF <fs_data>-shkzg EQ 'S'.
            <fs_data>-dmbtr = <fs_data>-dmbtr.
          ELSEIF <fs_data>-shkzg EQ 'H'.
            <fs_data>-dmbtr = - <fs_data>-dmbtr.
          ENDIF.
          "--------------------NEW ADD END------------------------
        ENDIF.
  
  
  
      ELSE.
        DELETE  gt_data INDEX  sy-tabix.
      ENDIF.
  
    ENDLOOP.
  
    DELETE gt_data WHERE aufnr NOT IN s_aufnr OR aufnr IS INITIAL.
  
    SORT gt_data BY monat ASCENDING ebeln ASCENDING ebelp ASCENDING budat ASCENDING.
  
    "验证内部订单类型
    LOOP AT gt_data ASSIGNING FIELD-SYMBOL(<fs_check_aufnr>).
      CLEAR lv_auart.
      SELECT SINGLE auart
        INTO lv_auart
        FROM aufk
        WHERE aufnr = <fs_check_aufnr>-aufnr.
  
      IF   lv_auart NE 'Z001'
        AND lv_auart NE 'Z002'
        AND lv_auart NE 'Z003'
        AND lv_auart NE 'Z004'.
  
        DELETE gt_data INDEX sy-tabix.
      ENDIF.
    ENDLOOP.
  
  
  ENDFORM.
  *&---------------------------------------------------------------------*
  *& Form FRM_GET_EKKO_INFO
  *&---------------------------------------------------------------------*
  *& text 获取采购凭证及发票信息
  *&---------------------------------------------------------------------*
  *&      --> <FS_DATA> 传入结构
  *&---------------------------------------------------------------------*
  FORM frm_get_ekko_info USING u_data TYPE ty_data.
  * 获取采购凭证信息
    SELECT SINGLE
      ekko~contract_no
      ekpo~txz01
      ekpo~brtwr
      INTO CORRESPONDING FIELDS OF u_data
      FROM ekpo
      INNER JOIN ekko
      ON ekpo~ebeln = ekko~ebeln
      WHERE ekpo~ebeln = u_data-ebeln
      AND ekpo~ebelp = u_data-ebelp.
  
  *----------------------------------------------------------------------------------------------------------*
  * Title：已收发票金额取数逻辑
  * Desc：根据发票凭证号(BSEG~BELNR)关联（RBKP-BELNR），取 （RBKP-STBLG=‘ ’和RBKP-RBSTAT=“5”）记录中的发票金额(RBKP~RMWWR)
  * Auth：soarstars
  * Date：20191030
  *--------------------------------------------start---------------------------------------------------------*
  * 获取已收发票金额
  *  SELECT SINGLE
  *    rseg~ebeln
  *    rbkp~belnr AS zbelnr "发票凭证号
  *    rbkp~rmwwr
  *    rbkp~xblnr
  *    rbkp~sgtxt
  *    INTO CORRESPONDING FIELDS OF u_data
  *    FROM rbkp
  *    INNER JOIN rseg
  *    ON rbkp~belnr = rseg~belnr
  *    AND rbkp~gjahr = rseg~gjahr
  *    WHERE rseg~ebeln = u_data-ebeln
  *    AND rseg~ebelp = u_data-ebelp
  *    AND rbkp~stblg = space
  *    AND rbkp~rbstat = '5'.
  
    SELECT SINGLE
      "rseg~ebeln
      "rbkp~belnr AS zbelnr "发票凭证号
      rbkp~rmwwr
      rbkp~xblnr
      rbkp~sgtxt
      INTO CORRESPONDING FIELDS OF u_data
      FROM rbkp
      WHERE rbkp~belnr = u_data-zbelnr
      AND rbkp~stblg = space
      AND rbkp~rbstat = '5'.
  *----------------------------------------------------------------------------------------------------------*
  * Title：已收发票金额取数逻辑
  * Desc：根据发票凭证号(BSEG~BELNR)关联（RBKP-BELNR），取 （RBKP-STBLG=‘ ’和RBKP-RBSTAT=“5”）记录中的发票金额(RBKP~RMWWR)
  * Auth：soarstars
  * Date：20191030
  *--------------------------------------------end-----------------------------------------------------------*
  ENDFORM.
  *&---------------------------------------------------------------------*
  *& Form FRM_DATA_OUTPUT
  *&---------------------------------------------------------------------*
  *& text 显示ALV数据
  *&---------------------------------------------------------------------*
  FORM frm_data_output .
    PERFORM frm_set_fieldcat.
    PERFORM frm_set_layout.
    PERFORM frm_show_alv.
  ENDFORM.
  *&---------------------------------------------------------------------*
  *& Form FRM_SET_FIELDCAT
  *&---------------------------------------------------------------------*
  *& text 填充字段格式
  *&---------------------------------------------------------------------*
  FORM frm_set_fieldcat .
    PERFORM frm_add_fieldcat USING 'GJAHR' '年度'(018) '' ''.
    PERFORM frm_add_fieldcat USING 'MONAT' '账期'(002) '' ''.
    PERFORM frm_add_fieldcat USING 'AUFNR' '内部订单号'(003) '' ''.
    PERFORM frm_add_fieldcat USING 'CONTRACT_NO' '合同号'(004) '' ''.
    PERFORM frm_add_fieldcat USING 'EBELN' '采购订单号'(005) '' 'X'.
    PERFORM frm_add_fieldcat USING 'EBELP' '采购订单行项目'(006) '' ''.
    PERFORM frm_add_fieldcat USING 'TXZ01' '工程内容'(007) '' ''.
    PERFORM frm_add_fieldcat USING 'LIFNR' '供应商编号'(008) 'X' ''.
    PERFORM frm_add_fieldcat USING 'MCOD1' '供应商'(009) '' ''.
    PERFORM frm_add_fieldcat USING 'BRTWR' '总金额'(010) '' ''.
    PERFORM frm_add_fieldcat USING 'DMBTR' '付款金额'(011) '' ''.
    PERFORM frm_add_fieldcat USING 'BUDAT' '付款时间'(012) '' ''.
    PERFORM frm_add_fieldcat USING 'RMWWR' '已收发票金额'(013) '' ''.
    PERFORM frm_add_fieldcat USING 'XBLNR' '参照'(014) '' ''.
    PERFORM frm_add_fieldcat USING 'SGTXT' '行项目文本'(015) '' ''.
    PERFORM frm_add_fieldcat USING 'BELNR' '付款凭证号'(016) '' 'X'.
    PERFORM frm_add_fieldcat USING 'ZBELNR' '发票凭证号'(017) '' 'X'.
  
  ENDFORM.
  *&---------------------------------------------------------------------*
  *& Form FRM_ADD_FIELDCAT
  *&---------------------------------------------------------------------*
  *& text 填充字段格式
  *&---------------------------------------------------------------------*
  FORM frm_add_fieldcat USING VALUE(p_fieldname)
                              VALUE(p_scrtext_l)
                              VALUE(p_no_zero)
                              hotspot.
  
    gs_fieldcat-fieldname = p_fieldname.
    gs_fieldcat-scrtext_l = p_scrtext_l.
    gs_fieldcat-no_zero = p_no_zero.
    gs_fieldcat-hotspot = hotspot.
    APPEND gs_fieldcat TO gt_fieldcat.
  
  ENDFORM.
  *&---------------------------------------------------------------------*
  *& Form FRM_SET_LAYOUT
  *&---------------------------------------------------------------------*
  *& text LAYOUT设置
  *&---------------------------------------------------------------------*
  FORM frm_set_layout .
  
    gs_layout-cwidth_opt = 'X'.
  
  ENDFORM.
  FORM frm_pf_status USING rt_extab TYPE slis_t_extab.
    SET PF-STATUS 'ZFIR0022_GUI' .
  ENDFORM.
  *&---------------------------------------------------------------------*
  *& Form FRM_SHOW_ALV
  *&---------------------------------------------------------------------*
  *& text ALV输出
  *&---------------------------------------------------------------------*
  FORM frm_show_alv .
  
    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
      EXPORTING
        i_callback_program       = sy-repid
        i_callback_pf_status_set = 'FRM_PF_STATUS'
        i_callback_user_command  = 'FRM_USER_COMMAND'
        is_layout_lvc            = gs_layout
        it_fieldcat_lvc          = gt_fieldcat[]
      TABLES
        t_outtab                 = gt_data
      EXCEPTIONS
        program_error            = 1
        OTHERS                   = 2.
    IF sy-subrc <> 0.
      CLEAR gt_data.
    ENDIF.
  
  ENDFORM.
  
  FORM frm_user_command USING p_ucomm        TYPE sy-ucomm
                               p_rs_selfield  TYPE slis_selfield.
  
    CASE p_ucomm.
      WHEN '&IC1' .   " 判断用户的动作
        READ TABLE gt_data INTO DATA(ls_data) INDEX p_rs_selfield-tabindex .
        IF p_rs_selfield-fieldname EQ 'EBELN'.      "跳转采购订单
          "判断用户点击的是哪一列
          IF NOT ls_data-ebeln IS INITIAL .
            SET PARAMETER ID 'BES' FIELD ls_data-ebeln.
            CALL TRANSACTION 'ME23N' AND SKIP FIRST SCREEN.
          ENDIF.
  
        ELSEIF p_rs_selfield-fieldname EQ 'BELNR' . "付款凭证号
          IF NOT ls_data-belnr IS INITIAL .
            SET PARAMETER ID 'BLN' FIELD ls_data-belnr.
            SET PARAMETER ID 'GJR' FIELD ls_data-gjahr.
            SET PARAMETER ID 'BUK' FIELD ls_data-bukrs.
            CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
          ENDIF .
  
        ELSEIF p_rs_selfield-fieldname EQ 'ZBELNR' ."发票凭证号
  
          IF NOT ls_data-zbelnr IS INITIAL .
            SET PARAMETER ID 'RBN' FIELD ls_data-zbelnr.
            SET PARAMETER ID 'GJR' FIELD ls_data-gjahr.
            SET PARAMETER ID 'BUK' FIELD ls_data-bukrs.
            CALL TRANSACTION 'MIR4' AND SKIP FIRST SCREEN.
  
          ENDIF .
  
        ENDIF.
    ENDCASE.
  ENDFORM.