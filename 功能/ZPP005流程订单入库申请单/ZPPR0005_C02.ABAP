*&---------------------------------------------------------------------*
*& 包含               ZPPR0005_C02
*&---------------------------------------------------------------------*
CLASS zcl_qr_oper DEFINITION.
    PUBLIC SECTION.
      METHODS:
        get_qr_data,"获取二维码扫描数据
        "声明ALV事件处理程序方法
        handle_alv_toolbar FOR EVENT toolbar OF cl_gui_alv_grid IMPORTING e_object e_interactive,"增加/减少toolbar按钮
        handle_after_user_command FOR EVENT after_user_command OF  cl_gui_alv_grid IMPORTING e_ucomm  e_saved e_not_processed,
        "ALV单元格编辑后的处理事件
        handle_data_changed_finished FOR EVENT data_changed_finished OF cl_gui_alv_grid IMPORTING sender e_modified et_good_cells,
        set_drdn_table."设置下拉框列表值
    PRIVATE SECTION.
      DATA:lt_sel_rowids    TYPE lvc_t_roid,
           lv_e_valid       TYPE flag,
           lo_document      TYPE REF TO cl_document_bcs, "用来放发送的内容的类
           lit_contents     TYPE soli_tab,
           l_cc             TYPE adr6-smtp_addr,
           l_to             TYPE adr6-smtp_addr,
           l_bcs_to         TYPE REF TO if_recipient_bcs,
           l_bcs_cc         TYPE REF TO if_recipient_bcs,
           lo_sender        TYPE REF TO cl_sapuser_bcs,
           l_result         TYPE os_boolean,
  *      cl_bcs发送邮件主要用到的功能类, 包括创建发送请求, 添加发送内容,添加发送地址, 到最终的发送指令发出.
           w_document       TYPE REF TO cl_bcs,
           l_file_size_char TYPE so_obj_len,
           l_filen          TYPE string,
           lit_mailhex      TYPE solix_tab,
           l_file_size      TYPE i,
           l_rc             TYPE i,
           l_string         TYPE string,
           l_subject        TYPE so_obj_des,
  *      cx_bcs异常类, 捕捉发送邮件过程中出现的异常.
           lo_fail          TYPE REF TO cx_bcs.
      METHODS:
        refresh_alv_grid,
        get_selected_rows RETURNING VALUE(rt_rows) TYPE lvc_t_roid,"批量设置前的检查
        set_qropersts_text IMPORTING iv_qropersts TYPE zqropersts,"批量设置二维码扫描状态文本
        send_mail IMPORTING it_zppt0003_list TYPE ty_tab_zppt0003_list
                            it_sqlwhere TYPE t_rsdswhere
                            iv_title TYPE char70
                  RETURNING VALUE(rv_flag) TYPE flag,
        set_sendmail_contents IMPORTING it_zppt0003_list TYPE ty_tab_zppt0003_list
                                        iv_title TYPE char70
                              RETURNING VALUE(rt_contents) TYPE soli_tab.
  ENDCLASS.
  CLASS zcl_qr_oper IMPLEMENTATION.
    METHOD get_qr_data.
      DATA:BEGIN OF ls_objek,
             objek TYPE ausp-objek,
           END OF ls_objek.
      DATA:lt_kdauf LIKE TABLE OF gs_kdauf,
           lt_objek LIKE TABLE OF ls_objek.
      SELECT t1~*,t2~wemng
      FROM zppt0003 AS t1
      LEFT JOIN afpo AS t2  ON t1~aufnr = t2~aufnr
      WHERE t1~zsapnote_no IN @s_reqno
      AND t1~werks = @p_dwerk
      AND t1~aufnr IN @s_aufnr
      AND t1~matnr IN @s_matnr
      AND t1~charg IN @s_charg
      AND t1~uname IN @s_uname
      AND t1~zdelflg IN @gr_zdelflg
      AND t1~budat IN @s_budat
      AND t1~zdelflg = ''
      INTO CORRESPONDING FIELDS OF TABLE @gt_zppt0003_list.
  
      MOVE-CORRESPONDING gt_zppt0003_list TO lt_kdauf.
      SORT lt_kdauf BY kdauf kdpos.
      DELETE ADJACENT DUPLICATES FROM lt_kdauf COMPARING kdauf kdpos.
      IF lt_kdauf IS NOT INITIAL.
        SELECT vbeln AS kdauf,posnr AS kdpos,zsfxbz
         FROM vbap FOR ALL ENTRIES IN @lt_kdauf
        WHERE  vbeln = @lt_kdauf-kdauf
          AND posnr = @lt_kdauf-kdpos
          INTO TABLE @DATA(lt_vbap1).
        "销售订单的实际客户的属性1描述
        DELETE ADJACENT DUPLICATES FROM lt_kdauf COMPARING kdauf.
        SELECT vbeln, kunnr FROM vbpa FOR ALL ENTRIES IN @lt_kdauf
          WHERE vbeln = @lt_kdauf-kdauf
          AND posnr IS INITIAL
          AND parvw = 'Z1'
          INTO TABLE @DATA(lt_vbpa).
        SELECT v~vbeln AS kdauf, tvk1t~vtext
          FROM @lt_vbpa AS v
          INNER JOIN kna1 ON v~kunnr = kna1~kunnr
          INNER JOIN tvk1t ON kna1~katr1 = tvk1t~katr1 AND tvk1t~spras = @sy-langu
          INTO TABLE @DATA(lt_kna1).
        SORT lt_vbap1 BY kdauf kdpos.
        SORT lt_kna1 BY kdauf.
      ENDIF.
      MOVE-CORRESPONDING gt_zppt0003_list TO gt_matnr.
      SORT gt_matnr BY matnr.
      DELETE ADJACENT DUPLICATES FROM gt_matnr.
      LOOP AT gt_matnr INTO gs_matnr.
        ls_objek-objek = CONV char90( gs_matnr-matnr ).
        APPEND ls_objek TO lt_objek.
      ENDLOOP.
      "从AUSP读取物料001分类特征值
      IF lt_objek IS NOT INITIAL.
        SELECT ausp~objek,cabn~atnam,ausp~atwrt
        FROM cabn
        INNER JOIN ausp ON cabn~atinn = ausp~atinn
        FOR ALL ENTRIES IN @lt_objek
        WHERE cabn~atnam IN ( 'ZQR'  ) "二维码扫描特征值
        AND ausp~objek  =  @lt_objek-objek
        AND ausp~klart = '001' "物料分类
        INTO TABLE @DATA(lt_ausp_001).
      ENDIF.
      SORT lt_ausp_001 BY objek.
      LOOP AT gt_zppt0003_list ASSIGNING <fs_zppt0003_list>.
        CLEAR <fs_zppt0003_list>-type.
        READ TABLE lt_vbap1 WITH KEY kdauf = <fs_zppt0003_list>-kdauf kdpos = <fs_zppt0003_list>-kdpos BINARY SEARCH INTO DATA(ls_vbap1).
        IF sy-subrc = 0.
          <fs_zppt0003_list>-zsfxbz = ls_vbap1-zsfxbz.
        ENDIF.
        READ TABLE lt_kna1 WITH KEY kdauf = <fs_zppt0003_list>-kdauf  BINARY SEARCH INTO DATA(ls_kna1).
        IF sy-subrc  = 0.
          <fs_zppt0003_list>-vtext = ls_kna1-vtext.
        ENDIF.
  
        IF p_cqr = abap_true.
          READ TABLE lt_ausp_001 WITH KEY objek = CONV char90( <fs_zppt0003_list>-matnr ) BINARY SEARCH INTO DATA(ls_ausp_001).
          IF sy-subrc = 0 AND ls_ausp_001-atwrt = 'X'.
            <fs_zppt0003_list>-zqrscan = '是'.
          ENDIF.
        ENDIF.
      ENDLOOP.
      "只显示已维护二维码扫描特征值等于'是'的物料
      IF p_cqr = abap_true.
        DELETE gt_zppt0003_list WHERE zqrscan = ''.
      ENDIF.
      IF gt_zppt0003_list IS INITIAL.
        MESSAGE s000 WITH '未查询到数据' .
        LEAVE TO LIST-PROCESSING ."返回到选中屏幕
      ENDIF.
    ENDMETHOD.
    "ALV按钮事件处理程序方法：删除或增加按钮
    METHOD handle_alv_toolbar.
      "删除不需要的按钮
      DELETE e_object->mt_toolbar
       WHERE function = '&&SEP00'
         OR function = '&REFRESH'
         OR function = '&LOCAL&APPEND'
         OR function = '&LOCAL&UNDO'
         OR function ='&LOCAL&INSERT_ROW'
        OR function ='&LOCAL&DELETE_ROW'
         OR function = '&LOCAL&COPY_ROW'
         OR function = '&&SEP01'
         OR function = '&LOCAL&CUT'
         OR function = '&LOCAL&COPY'
         OR function = '&LOCAL&PASTE'
        OR function = '&LOCAL&UNDO'
         OR function = '&&SEP02'.
  
      "增加一个新按钮
      APPEND INITIAL LINE TO e_object->mt_toolbar ASSIGNING FIELD-SYMBOL(<fs_button>).
      <fs_button>-butn_type = '3'.  "分割符
      APPEND INITIAL LINE TO e_object->mt_toolbar ASSIGNING <fs_button>.
      <fs_button>-function = 'ZHEGE'.
      <fs_button>-icon = '@MC@'.
      <fs_button>-text = '批设置检验合格'.
  
      "增加一个新按钮
      APPEND INITIAL LINE TO e_object->mt_toolbar ASSIGNING <fs_button>.
      <fs_button>-butn_type = '3'.  "分割符
      APPEND INITIAL LINE TO e_object->mt_toolbar ASSIGNING <fs_button>.
      <fs_button>-function = 'ZUPLOAD'.
      <fs_button>-icon = '@MC@'.
      <fs_button>-text = '批量设置上传'.
  
      "增加一个新按钮
      APPEND INITIAL LINE TO e_object->mt_toolbar ASSIGNING <fs_button>.
      <fs_button>-butn_type = '3'.  "分割符
      APPEND INITIAL LINE TO e_object->mt_toolbar ASSIGNING <fs_button>.
      <fs_button>-function = 'ZCHECK'.
      <fs_button>-icon = '@8X@'.
      <fs_button>-text = '批量设置审核'.
  
      "增加一个新按钮
      APPEND INITIAL LINE TO e_object->mt_toolbar ASSIGNING <fs_button>.
      <fs_button>-butn_type = '3'.  "分割符
      APPEND INITIAL LINE TO e_object->mt_toolbar ASSIGNING <fs_button>.
      <fs_button>-function = 'ZEMPTY'.
      <fs_button>-icon = '@0Z@'.
      <fs_button>-text = '批量清空状态'.
  
      "增加一个新按钮
      APPEND INITIAL LINE TO e_object->mt_toolbar ASSIGNING <fs_button>.
      <fs_button>-butn_type = '3'.  "分割符
      APPEND INITIAL LINE TO e_object->mt_toolbar ASSIGNING <fs_button>.
      <fs_button>-function = 'BATCHCHG'.
      <fs_button>-icon = '@HB@'.
      <fs_button>-text = '发送邮件及保存'.
    ENDMETHOD.
    "设置下拉框列表值
    METHOD set_drdn_table.
      DATA: lt_dral TYPE lvc_t_dral,                          "#EC NEEDED
            ls_dral TYPE lvc_s_dral.                          "#EC NEEDED
      SELECT * FROM dd07v WHERE domname  = 'ZQROPERSTS' AND ddlanguage = @sy-langu INTO TABLE @DATA(lt_dd07v).
      LOOP AT lt_dd07v INTO DATA(ls_dd07v).
        ls_dral-handle = '1'.
        ls_dral-int_value = ls_dd07v-domvalue_l.
        ls_dral-value = ls_dd07v-ddtext.
        APPEND ls_dral TO lt_dral.
      ENDLOOP.
      go_alv_grid_create->set_drop_down_table( it_drop_down_alias = lt_dral ).
  *    go_alv_grid_create->set_drop_down_table( it_drop_down = lt_dropdown ).
    ENDMETHOD.
    METHOD get_selected_rows.
      "执行ALV数据有效性检查
      CLEAR lv_e_valid.
      go_alv_grid_create->check_changed_data( IMPORTING e_valid = lv_e_valid ).
      IF lv_e_valid = ''.
        RETURN.
      ENDIF.
      go_alv_grid_create->get_selected_rows( IMPORTING et_row_no = rt_rows )."ALV选中行
    ENDMETHOD.
    METHOD set_qropersts_text.
      lt_sel_rowids =  get_selected_rows( ).
      IF lt_sel_rowids IS INITIAL.
        MESSAGE s000 WITH '请选中行'.
        RETURN.
      ENDIF.
      LOOP AT lt_sel_rowids INTO DATA(ls_row).
        gt_zppt0003_list[ ls_row-row_id ]-qropersts = iv_qropersts.
        gt_zppt0003_list[ ls_row-row_id ]-upd = abap_true.
      ENDLOOP.
    ENDMETHOD.
  
    METHOD handle_after_user_command.
      DATA:lv_e_valid           TYPE flag,
           lt_zppt0003_save     TYPE TABLE OF zppt0003,
           lt_zppt0003_email_hg TYPE ty_tab_zppt0003_list,
           lt_zppt0003_email_sc TYPE ty_tab_zppt0003_list,
           lt_zppt0003_email_sp TYPE ty_tab_zppt0003_list,
           lt_sel_rowids        TYPE lvc_t_roid,
           lv_qropersts         TYPE zqropersts,
           lv_flag              TYPE flag,
           lt_index             TYPE TABLE OF sy-tabix.
      CASE e_ucomm.
        WHEN 'ZHEGE'.
          IF gs_zqrscanauth-hege_flag = abap_false.
            MESSAGE s000 WITH '您没有设置检验合格的权限' .
            RETURN.
          ENDIF.
          set_qropersts_text( '合格' ).
          refresh_alv_grid( ).
          MESSAGE s000 WITH '已设置检验合格文本'.
        WHEN 'ZUPLOAD'.
          IF gs_zqrscanauth-upload_flag = abap_false.
            MESSAGE s000 WITH '您没有上传权限' .
            RETURN.
          ENDIF.
          set_qropersts_text( '已上传' ).
          refresh_alv_grid( ).
          MESSAGE s000 WITH '已设置上传文本'.
        WHEN 'ZCHECK'.
          IF gs_zqrscanauth-audit_flag = abap_false.
            MESSAGE s000 WITH '您没有审批权限'.
            RETURN.
          ENDIF.
          set_qropersts_text( '已审批' ).
          refresh_alv_grid( ).
          MESSAGE s000 WITH '已设置审批文本'.
        WHEN 'ZEMPTY'.
          IF gs_zqrscanauth-empty_flag = abap_false.
            MESSAGE s000 WITH '您没有清空文本权限'.
            RETURN.
          ENDIF.
          set_qropersts_text( '' )."清空文本
          refresh_alv_grid( ).
          MESSAGE s000 WITH '已清空文本'.
        WHEN 'BATCHCHG'.
          IF gs_zqrscanauth-save_flag = abap_false.
            MESSAGE s000 WITH '您没有发送邮件及保存权限' .
            RETURN.
          ENDIF.
          "执行ALV数据有效性检查
          go_alv_grid_create->check_changed_data( IMPORTING e_valid = lv_e_valid ).
          IF lv_e_valid = ''.
            RETURN.
          ENDIF.
          LOOP AT gt_zppt0003_list INTO gs_zppt0003_list WHERE upd = abap_true.
            lt_index = VALUE #( BASE lt_index ( sy-tabix ) ).
            MOVE-CORRESPONDING gs_zppt0003_list TO zppt0003.
            CLEAR zppt0003-message.
            APPEND zppt0003 TO lt_zppt0003_save.
            "需发送邮件的记录
            IF gs_zppt0003_list-qropersts = '合格'.
              APPEND gs_zppt0003_list TO lt_zppt0003_email_hg.
            ELSEIF gs_zppt0003_list-qropersts = '已上传'.
              APPEND gs_zppt0003_list TO lt_zppt0003_email_sc.
            ELSEIF gs_zppt0003_list-qropersts = '已审批'.
              APPEND gs_zppt0003_list TO lt_zppt0003_email_sp.
            ENDIF.
          ENDLOOP.
          IF lt_zppt0003_save IS INITIAL.
            MESSAGE s000 WITH '没有数据被修改过,无需保存'.
            RETURN.
          ENDIF.
          "检验合格邮件通知
          IF lt_zppt0003_email_hg IS NOT INITIAL.
            REFRESH gt_sqlwhere.
            gt_sqlwhere = VALUE #( ( line = 'ZSENDHG = ''X''' ) ).
            lv_flag = send_mail( EXPORTING it_zppt0003_list = lt_zppt0003_email_hg
                                           it_sqlwhere = gt_sqlwhere
                                           iv_title = '以下二维码产品已检验合格,请尽快完成上传' ).
            IF lv_flag = abap_false.
              RETURN.
            ENDIF.
          ENDIF.
          IF lt_zppt0003_email_sc IS NOT INITIAL.
            REFRESH gt_sqlwhere.
            gt_sqlwhere = VALUE #( ( line = 'ZSENDSC = ''X''' ) ).
            lv_flag = send_mail( EXPORTING it_zppt0003_list = lt_zppt0003_email_sc
                                           it_sqlwhere = gt_sqlwhere
                                           iv_title = '以下二维码产品已上传,请尽快完成审核' ).
            IF lv_flag = abap_false.
              RETURN.
            ENDIF.
          ENDIF.
          IF lt_zppt0003_email_sp IS NOT INITIAL.
            REFRESH gt_sqlwhere.
            gt_sqlwhere = VALUE #( ( line = 'ZSENDSP = ''X''' ) ).
            lv_flag = send_mail( EXPORTING it_zppt0003_list = lt_zppt0003_email_sp
                                           it_sqlwhere = gt_sqlwhere
                                           iv_title = '以下二维码产品已审核,请尽快完成使用决策' ).
            IF lv_flag = abap_false.
              RETURN.
            ENDIF.
          ENDIF.
          "保存数据
          MODIFY zppt0003 FROM TABLE lt_zppt0003_save.
          COMMIT WORK.
          LOOP AT lt_index INTO DATA(ls_index).
            ASSIGN gt_zppt0003_list[ ls_index ] TO <fs_zppt0003_list>.
            CLEAR <fs_zppt0003_list>-upd.
            IF <fs_zppt0003_list>-qropersts = '已上传'.
              <fs_zppt0003_list>-message = '邮件发送、保存成功'.
            ELSE.
              <fs_zppt0003_list>-message = '保存成功'.
            ENDIF.
          ENDLOOP.
          refresh_alv_grid( ).
          MESSAGE s000 WITH '邮件发送及保存成功!'.
      ENDCASE.
    ENDMETHOD.
    "设置发送邮件正文
    METHOD set_sendmail_contents.
      DATA:ls_contents TYPE LINE OF soli_tab.
      "邮件正文内容html
      ls_contents-line = '<html><body><head><style type="text/css">table {border-collapse: collapse;margin: 0 auto;text-align: center;}'.
      APPEND ls_contents TO rt_contents.
      ls_contents-line = 'table td, table th {border: 1px solid #cad9ea;color: #666;height: 30px;}'.
      APPEND ls_contents TO rt_contents.
      ls_contents-line = 'table thead th{background-color: #CCE8EB;width: 100px;}'.
      APPEND ls_contents TO rt_contents.
      ls_contents-line = 'table tr:nth-child(odd){background: #fff;}'.
      APPEND ls_contents TO rt_contents.
      ls_contents-line = 'table tr:nth-child(even){background: #F5FAFA;}</style></head>'.
      APPEND ls_contents TO rt_contents.
      ls_contents-line = |{ '<body><table width="90%" class="table"><caption><h2>' }{ iv_title }{ '</h2></caption>' }|.
      APPEND ls_contents TO rt_contents.
      ls_contents-line = '<thead><tr><th>申请单号</th><th>申请单行号</th><th>入库申请日期</th><th>工厂</th>'.
      APPEND ls_contents TO rt_contents.
      ls_contents-line = '<th>订单</th><th>物料</th><th>物料描述</th><th>物料规格</th><th>包装规格</th><th>库存地点</th>'.
      APPEND ls_contents TO rt_contents.
      ls_contents-line = '<th>车间仓库名称</th><th>申请数量</th><th>批次</th><th>包装批号(打印)</th><th>生产日期</th><th>有效期</th><th>QR上传状态</th>'.
      APPEND ls_contents TO rt_contents.
      ls_contents-line = '</tr></thead>'.
      APPEND ls_contents TO rt_contents.
      LOOP AT it_zppt0003_list INTO gs_zppt0003_list.
        APPEND '<tr>' TO rt_contents.
        APPEND |{ '<td>' }{ gs_zppt0003_list-zsapnote_no }{ '</td>' }| TO rt_contents.
        APPEND |{ '<td>' }{ gs_zppt0003_list-zsapnote_line }{ '</td>' }| TO rt_contents.
        APPEND |{ '<td>' }{ gs_zppt0003_list-budat DATE = ISO }{ '</td>' }| TO rt_contents.
        APPEND |{ '<td>' }{ gs_zppt0003_list-werks }{ '</td>' }| TO rt_contents.
        APPEND |{ '<td>' }{ zcl_bc_public=>conv_by_ddic( i_input = gs_zppt0003_list-aufnr i_out = 'X' ) }{ '</td>' }| TO rt_contents.
        APPEND |{ '<td>' }{ zcl_bc_public=>conv_by_ddic( i_input = gs_zppt0003_list-matnr i_out = 'X' ) }{ '</td>' }| TO rt_contents.
        APPEND |{ '<td>' }{ gs_zppt0003_list-maktx }{ '</td>' }| TO rt_contents.
        APPEND |{ '<td>' }{ gs_zppt0003_list-zspec }{ '</td>' }| TO rt_contents.
        APPEND |{ '<td>' }{ gs_zppt0003_list-zpspe }{ '</td>' }| TO rt_contents.
        APPEND |{ '<td>' }{ gs_zppt0003_list-lgort }{ '</td>' }| TO rt_contents.
        APPEND |{ '<td>' }{ gs_zppt0003_list-zwslgobe }{ '</td>' }| TO rt_contents.
        APPEND |{ '<td>' }{ gs_zppt0003_list-menge }{ '</td>' }| TO rt_contents.
        APPEND |{ '<td>' }{ gs_zppt0003_list-charg }{ '</td>' }| TO rt_contents.
        APPEND |{ '<td>' }{ gs_zppt0003_list-zpack_charg }{ '</td>' }| TO rt_contents.
        APPEND |{ '<td>' }{ gs_zppt0003_list-zhsdat DATE = ISO }{ '</td>' }| TO rt_contents.
        APPEND |{ '<td>' }{ gs_zppt0003_list-zvfdat DATE = ISO }{ '</td>' }| TO rt_contents.
        APPEND |{ '<td>' }{ gs_zppt0003_list-qropersts }{ '</td>' }| TO rt_contents.
        APPEND '</tr>' TO rt_contents.
      ENDLOOP.
      APPEND '</table></body></html>' TO rt_contents.
    ENDMETHOD.
    METHOD send_mail.
      DATA:ls_contents           TYPE LINE OF soli_tab,
           lt_contents           TYPE soli_tab,
           lo_cl_cam_address_bcs TYPE REF TO cl_cam_address_bcs,
           lo_bcs                TYPE REF TO cl_bcs.
      TRY .
          "邮件正文
          lt_contents = set_sendmail_contents( EXPORTING  it_zppt0003_list = it_zppt0003_list iv_title = iv_title ).
  *  create the document with contents
          lo_document = cl_document_bcs=>create_document(
            i_type       = 'HTM'
            i_subject    = '二维码扫描操作通知'  "邮件标题
  *          i_length     = l_file_size_char
            i_language   = sy-langu
            i_importance = '1'
            i_text       = lt_contents
            ).
  *  CREATING PERSISTENT OBJECT WILL ALLOW YOU TO SET THE DOCUMENT IN THE MAIL
          lo_bcs = cl_bcs=>create_persistent( ).
  *  发件人
  *  前提是这个邮箱地址能发邮件，并且不需要密码
          DATA(lo_sender) = cl_sapuser_bcs=>create( 'MAILSERVER' ).
          lo_bcs->set_sender( lo_sender ).
  
  *  收件人
          SELECT * FROM zppt0009 WHERE werks = @p_dwerk AND (it_sqlwhere) INTO TABLE @DATA(lt_zppt0009).
          IF lt_zppt0009 IS INITIAL.
            MESSAGE s000 WITH |{ iv_title }{ '邮件地址为维护' }|.
            RETURN.
          ENDIF.
          LOOP AT lt_zppt0009 INTO DATA(ls_email).
            lo_cl_cam_address_bcs  = cl_cam_address_bcs=>create_internet_address( i_address_string = ls_email-smtp_addr ).
  *     Add recipient to send request
            CALL METHOD lo_bcs->add_recipient
              EXPORTING
                i_recipient = lo_cl_cam_address_bcs.
          ENDLOOP.
  
  *抄送人
  *        l_cc = 'test@test.com'.
  *        l_bcs_cc = cl_cam_address_bcs=>create_internet_address( l_cc ).
  *
  *        CALL METHOD w_document->add_recipient
  *          EXPORTING
  *            i_recipient = l_bcs_cc
  *            i_copy      = 'X'.
  *        " i_express   = 'X'.
  
  *     send the mail
          lo_bcs->set_send_immediately( 'X' ).  "设置立即发送
          lo_bcs->send_request->set_link_to_outbox( 'X' ). "与outbox关联
          CALL METHOD lo_bcs->set_document( lo_document ).
          CALL METHOD lo_bcs->send(
            EXPORTING
              i_with_error_screen = 'X'
            RECEIVING
              result              = l_result ).
        CATCH cx_bcs INTO lo_fail.
          MESSAGE s000 WITH lo_fail->get_text( ).
      ENDTRY.
  *YOU CAN VERIFY THE STATUS IN THE LIST, YOU CAN ALSO SUBMIT THIS AS A BACKGROUND JOB.
      IF l_result = 'X'.
        COMMIT WORK AND WAIT.
        rv_flag = abap_true.
        MESSAGE '发送成功' TYPE 'S'.
      ELSE.
        ROLLBACK WORK.
        rv_flag = abap_false.
        MESSAGE '发送失败' TYPE 'S' DISPLAY LIKE 'E'.
      ENDIF.
    ENDMETHOD.
    "ALV单元格编辑后的处理程序
    METHOD handle_data_changed_finished.
      DATA:lv_refresh TYPE char1.
      LOOP AT et_good_cells INTO DATA(ls_good_cells) WHERE   error = ''.
        ASSIGN gt_zppt0003_list[ ls_good_cells-row_id ] TO <fs_zppt0003_list>.
        CHECK <fs_zppt0003_list> IS ASSIGNED.
        CASE ls_good_cells-fieldname.
          WHEN 'QROPERSTS'."QR上传状态
            IF <fs_zppt0003_list>-zqrscan = '是'.
              <fs_zppt0003_list>-upd = abap_true.
            ELSE.
              MESSAGE s000 WITH '物料分类未维护二维码扫描特征值'.
              CLEAR: <fs_zppt0003_list>-upd, <fs_zppt0003_list>-qropersts.
            ENDIF.
            lv_refresh = abap_true.
          WHEN OTHERS.
        ENDCASE.
      ENDLOOP.
      IF   lv_refresh = abap_true.
        CALL METHOD go_alv_grid_create->refresh_table_display
          EXPORTING
            is_stable = gs_stable
          EXCEPTIONS
            finished  = 1
            OTHERS    = 2.
      ENDIF.
    ENDMETHOD.
    "刷新ALV
    METHOD refresh_alv_grid.
  *    go_alv_grid_conf->set_frontend_layout( gs_layout )."布局重置，自动优化列宽等
      CALL METHOD go_alv_grid_create->refresh_table_display
        EXPORTING
          is_stable = gs_stable
        EXCEPTIONS
          finished  = 1
          OTHERS    = 2.
    ENDMETHOD.
  ENDCLASS.