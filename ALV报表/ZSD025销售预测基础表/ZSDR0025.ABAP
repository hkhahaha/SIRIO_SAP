*&---------------------------------------------------------------------*
*& Report ZSDR0025
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
report zsdr0025.
tables: likp,lips,vbak,vbap,knvv,but000,mara,makt,tcurr.
*在调用ALV之前，需要先定义Layout和Fieldcat，他们属于slis类型池
data:fieldcat type slis_t_fieldcat_alv with header line,
     layout   type slis_layout_alv,
     w_repid  type sy-repid. "记录系统当前的程序名
****ALV显示
**********************ADD BY LANJINGCUN 20210225-S13
data : gv_sd type sy-datum .
data : gv_ed type sy-datum .
types : begin of ty_vbakl ,
          vbeln  type vbak-vbeln,
          erdat  type vbak-erdat,
          erzet  type vbak-erzet,
          ernam  type vbak-ernam,
          audat  type vbak-audat,
          vbtyp  type vbak-vbtyp,
          auart  type vbak-auart,
          waerk  type vbak-waerk,
          vkorg  type vbak-vkorg,
          vtweg  type vbak-vtweg,
          spart  type vbak-spart,
          vkgrp  type vbak-vkgrp,
          vkbur  type vbak-vkbur,
          gsber  type vbak-gsber,
          knumv  type vbak-knumv,
          kunnr  type vbak-kunnr,
          zleix  type vbak-zleix,
          zjied  type vbak-zjied,
          posnr  type vbap-posnr,
          matnr  type vbap-matnr,
          charg  type vbap-charg,
          matkl  type vbap-matkl,
          arktx  type vbap-arktx,
          pstyv  type vbap-pstyv,
          umziz  type vbap-umziz,
          umzin  type vbap-umzin,
          meins  type vbap-meins,
          netpr  type vbap-netpr,
          netwr  type vbap-netwr,
          kpein  type vbap-kpein,
          kwmeng type vbap-kwmeng,
          kzwi1  type vbap-kzwi1,
          werks  type vbap-werks,
          znbjhq type vbap-znbjhq,
        end of ty_vbakl .
data : gs_vbakl type ty_vbakl .
data : gt_vbakl type table of ty_vbakl .

data : gs_vbepl type vbep .
data : gt_vbepl type table of vbep .

data : gs_vbpal type vbpa .
data : gt_vbpal type table of vbpa .

data : gs_vbkdl type vbkd .
data : gt_vbkdl type table of vbkd .

data : gs_but000l type but000 .
data : gt_but000l type table of but000 .

data : gs_lipsl type lips .
data : gt_lipsl type table of lips .

data : gs_maktl type makt .
data : gt_maktl type table of makt .
**********************ADD BY LANJINGCUN 20210225-E13


types: begin of ly_out,
         sel           type c, "BOX
         vkbur         type knvv-vkbur, "销售办公室
         vkbur_des     type tvkbt-bezei, "销售办公室描述
         sjkh          type vbpa-kunnr, "实际客户
         sjkh_des(80)  type c, "实际客户描述
         sjywy         type knvp-kunn2, "实际业务员
         sjywy_des(20) type c, "实际业务员描述
         bu_sort1      type but000-bu_sort1, "客户简称
         werks         type lips-werks, "出货工厂
         matnr         type lips-matnr, "物料
         objek         type ausp-objek, "物料特性对象
         maktx         type makt-maktx, "物料描述
         bzgg_des      type ausp-atwrt, "包装规格
         bzmc_des      type ausp-atwrt, "标准名称
         khwlbm_des    type ausp-atwrt, "客户物料编码
         meins         type lips-meins, "基本单位
         meins_des     type t006a-mseh6, "基本单位
         matkl         type mara-matkl, "物料组
         matkl_des     type t023t-wgbez, "物料组1描述
         matkl2        type mara-matkl, "中类
         matkl2_des    type t023t-wgbez, "中类描述
         groes         type mara-groes, "产品规格(大小量纲)
         m1sl          type lips-lfimg, "第1个月出货数（基本单位数量）
         m2sl          type lips-lfimg, "第2个月出货数（基本单位数量）
         m3sl          type lips-lfimg, "第3个月出货数（基本单位数量）
         m4sl          type lips-lfimg, "第4个月出货数（基本单位数量）
         m5sl          type lips-lfimg, "第5个月出货数（基本单位数量）
         m6sl          type lips-lfimg, "第6个月出货数（基本单位数量）
         m7sl          type lips-lfimg, "第7个月出货数（基本单位数量）
         m8sl          type lips-lfimg, "第8个月出货数（基本单位数量）
         m9sl          type lips-lfimg, "第9个月出货数（基本单位数量）
         m10sl         type lips-lfimg, "第10个月出货数（基本单位数量）
         m11sl         type lips-lfimg, "第11个月出货数（基本单位数量）
         m12sl         type lips-lfimg, "第12个月出货数（基本单位数量）
         m1jz          type lips-lfimg, "第1个月出货数不含税金额
         m2jz          type lips-lfimg, "第2个月出货数不含税金额
         m3jz          type lips-lfimg, "第3个月出货数不含税金额
         m4jz          type lips-lfimg, "第4个月出货数不含税金额
         m5jz          type lips-lfimg, "第5个月出货数不含税金额
         m6jz          type lips-lfimg, "第6个月出货数不含税金额
         m7jz          type lips-lfimg, "第7个月出货数不含税金额
         m8jz          type lips-lfimg, "第8个月出货数不含税金额
         m9jz          type lips-lfimg, "第9个月出货数不含税金额
         m10jz         type lips-lfimg, "第10个月出货数不含税金额
         m11jz         type lips-lfimg, "第11个月出货数不含税金额
         m12jz         type lips-lfimg, "第12个月出货数不含税金额
         m13sl          type lips-lfimg, "第13个月出货数（基本单位数量）
         m14sl          type lips-lfimg, "第14个月出货数（基本单位数量）
         m15sl          type lips-lfimg, "第15个月出货数（基本单位数量）
         m16sl          type lips-lfimg, "第16个月出货数（基本单位数量）
         m17sl          type lips-lfimg, "第17个月出货数（基本单位数量）
         m18sl          type lips-lfimg, "第18个月出货数（基本单位数量）
         m19sl          type lips-lfimg, "第19个月出货数（基本单位数量）
         m20sl          type lips-lfimg, "第20个月出货数（基本单位数量）
         m21sl          type lips-lfimg, "第21个月出货数（基本单位数量）
         m22sl         type lips-lfimg, "第22个月出货数（基本单位数量）
         m23sl         type lips-lfimg, "第23个月出货数（基本单位数量）
         m24sl         type lips-lfimg, "第24个月出货数（基本单位数量）
         m25sl         type lips-lfimg, "第25个月出货数（基本单位数量）
         m13jz          type lips-lfimg, "第13个月出货数不含税金额
         m14jz          type lips-lfimg, "第14个月出货数不含税金额
         m15jz          type lips-lfimg, "第15个月出货数不含税金额
         m16jz          type lips-lfimg, "第16个月出货数不含税金额
         m17jz          type lips-lfimg, "第17个月出货数不含税金额
         m18jz          type lips-lfimg, "第18个月出货数不含税金额
         m19jz          type lips-lfimg, "第19个月出货数不含税金额
         m20jz          type lips-lfimg, "第20个月出货数不含税金额
         m21jz          type lips-lfimg, "第21个月出货数不含税金额
         m22jz         type lips-lfimg, "第22个月出货数不含税金额
         m23jz         type lips-lfimg, "第23个月出货数不含税金额
         m24jz         type lips-lfimg, "第24个月出货数不含税金额
         m25jz         type lips-lfimg, "第25个月出货数不含税金额
       end of ly_out.

types: begin of ly_hz,"汇总数量
         vbeln     type likp-vbeln, "交货单
         posnr     type lips-posnr, "行项目
         matnr     type lips-matnr, "笏料
         wadat_ist type likp-wadat_ist, "实际发货日期
         lfimg     type lips-lfimg, "发货数量
         meins     type lips-meins, "基本单位
         umvkz     type lips-umvkz, "分子
         umvkn     type lips-umvkn, "分母
         jbsl      type lips-lfimg, "发货数量（基本数量）
         vgbel     type lips-vgbel, "参考凭证
         vgpos     type lips-vgpos, "参考凭证项目
         vkorg     type likp-vkorg, "销售组织
         vtweg     type lips-vtweg, "渠道
         spart     type lips-spart, "产品组
         sjkh      type vbpa-kunnr, "实际客户
         sjywy     type knvp-kunn2, "实际业务员
         vkbur     type knvv-vkbur, "销售办公室
         werks     type likp-werks, "出货工厂
         ny(6)     type c, "年月
         hl        type vbkd-kurrf, "汇率
         bhsje     type vbap-kzwi1, "不含税金额
         waerk     type vbak-waerk, "币别
       end of ly_hz.



data: gt_alv    type table of ly_out,
      gs_alv    type ly_out,
      gt_hz1    type table of ly_hz,
      gs_hz1    type ly_hz,
      gt_hz2    type table of ly_hz,
      gs_hz2    type ly_hz,
      gt_mara   type table of mara,
      gs_mara   type mara,
      gt_makt   type table of makt,
      gs_makt   type makt,
      gt_vbak   type table of vbak,
      gs_vbak   type vbak,
      gt_vbap   type table of vbap,
      gs_vbap   type vbap,
      gt_vbpa   type table of vbpa,
      gs_vbpa   type vbpa,
      gt_knvp   type table of knvp,
      gs_knvp   type knvp,
      gt_knvv   type table of knvv,
      gs_knvv   type knvv,
      gt_but000 type table of but000,
      gs_but000 type but000,
      gt_tvkbt  type table of tvkbt,
      gs_tvkbt  type tvkbt,
      gt_t006a  type table of t006a,
      gs_t006a  type t006a,
      gt_vbkd1  type table of vbkd,
      gs_vbkd1  type vbkd,
      gt_vbkd2  type table of vbkd,
      gs_vbkd2  type vbkd,
      gt_tvap   type table of tvap,
      gs_tvap   type tvap,
      gt_tcurr  type table of tcurr,
      gs_tcurr  type tcurr,
      gt_t023t  type table of t023t,
      gs_t023t  type t023t
      .


selection-screen begin of block blk with frame title  text-001.
parameters: s_ksrq like likp-wadat_ist  obligatory ."开始日期
parameters:s_jsrq like likp-wadat_ist obligatory."结束日期
select-options:s_vbeln for likp-vbeln ."交货单
select-options:s_werks for lips-werks ."工厂
select-options:s_matnr for lips-matnr ."物料
select-options:s_vkorg for vbak-vkorg obligatory ."销售组织
select-options:s_vtweg for vbak-vtweg obligatory."渠道
select-options:s_sjkh for but000-partner ."实际客户
select-options:s_sjywy for but000-partner ."实际业务员
selection-screen end of block blk.



start-of-selection.

*权限检查
  data:a01(1) type c.
  data:a02(1) type c.
  perform frm_authority_check.
  if a01 = 'X'.
    write '无销售组织权限'.
    exit.
  endif.
  if a02 = 'X'.
    write '无分销渠道权限'.
    exit.
  endif.




*获取交货单；
  select
    a~vbeln      "交货单
    b~posnr      "行项目
    b~matnr"物料
    a~wadat_ist "实际发货日期
    b~lfimg     "发货数量
    b~umvkz     "分子
    b~umvkn     "分母
    a~vkorg "销售组织
    b~vtweg "渠道
    b~spart "产品
    b~vgbel "参考凭证
    b~vgpos "参考项目
    b~werks "出货工厂
    b~meins "基本单位
c~waerk "币别
    into corresponding fields of table gt_hz1
    from likp as a
    join lips as b  on a~vbeln = b~vbeln
    join vbak as c on b~vgbel = c~vbeln
    where a~wadat_ist => s_ksrq and a~wadat_ist <= s_jsrq
    and b~werks in s_werks
    and b~matnr in s_matnr
    and a~vbeln in s_vbeln
*    AND b~vgtyp = 'C'
    and a~wbstk = 'C'
    and a~vkorg in s_vkorg
    and c~vtweg in s_vtweg
    and b~lfimg <> 0

    .


  if gt_hz1 is initial.
    write '无数据'.
    exit.
  endif.

*根据交货单获取销售订单行项目对应的净值；
  select
    *
    into corresponding fields of table gt_vbap
    from vbap
    for all entries in gt_hz1
    where vbeln = gt_hz1-vgbel
    and posnr = gt_hz1-vgpos.




*******根据开始日期计算25个月的年月

  data: a1     type p0001-begda,
        a2     type p0001-begda,
        months type t5a4a-dlymo,
        signum type  t5a4a-split,
        m1(6)  type c, "第1个月年月
        m2(6)  type c, "第2个月年月
        m3(6)  type c, "第3个月年月
        m4(6)  type c, "第4个月年月
        m5(6)  type c, "第5个月年月
        m6(6)  type c, "第6个月年月
        m7(6)  type c, "第7个月年月
        m8(6)  type c, "第8个月年月
        m9(6)  type c, "第9个月年月
        m10(6) type c, "第10个月年月
        m11(6) type c, "第11个月年月
        m12(6) type c, "第12个月年月
        m13(6)  type c, "第13个月年月
        m14(6)  type c, "第14个月年月
        m15(6)  type c, "第15个月年月
        m16(6)  type c, "第16个月年月
        m17(6)  type c, "第17个月年月
        m18(6)  type c, "第18个月年月
        m19(6)  type c, "第19个月年月
        m20(6)  type c, "第20个月年月
        m21(6)  type c, "第21个月年月
        m22(6) type c, "第22个月年月
        m23(6) type c, "第23个月年月
        m24(6) type c, "第24个月年月
        m25(6) type c "第25个月年月
        .

  types: begin of ly_rq,
           rq type sy-datum,  "日期
           ny type sy-datum, "年月
         end of ly_rq.

  data:gt_rq type  table of ly_rq,
       gs_rq type ly_rq.
  a1 = s_ksrq.
  call function 'RP_CALC_DATE_IN_INTERVAL'
      exporting
        date      = a1
        days      = '00'
        months    = '12'
        signum    = '-'
        years     = '00'
      importing
        calc_date = a1.
  gs_rq-rq = a1.
  gs_rq-ny = a1+0(6).
  append gs_rq to gt_rq.

  do 24 times.

    call function 'RP_CALC_DATE_IN_INTERVAL'
      exporting
        date      = a1
        days      = '00'
        months    = '01'
        signum    = '+'
        years     = '00'
      importing
        calc_date = a1.

    gs_rq-rq = a1.
    gs_rq-ny = a1+0(6).
    append gs_rq to gt_rq.

  enddo.

  read table gt_rq into gs_rq index  1.
  m1 = gs_rq-ny.
  clear gs_rq.
  read table gt_rq into gs_rq index  2.
  m2 = gs_rq-ny.
  clear gs_rq.
  read table gt_rq into gs_rq index  3.
  m3 = gs_rq-ny.
  clear gs_rq.
  read table gt_rq into gs_rq index  4.
  m4 = gs_rq-ny.
  clear gs_rq.
  read table gt_rq into gs_rq index  5.
  m5 = gs_rq-ny.
  clear gs_rq.
  read table gt_rq into gs_rq index  6.
  m6 = gs_rq-ny.
  clear gs_rq.
  read table gt_rq into gs_rq index  7.
  m7 = gs_rq-ny.
  clear gs_rq.
  read table gt_rq into gs_rq index  8.
  m8 = gs_rq-ny.
  clear gs_rq.
  read table gt_rq into gs_rq index  9.
  m9 = gs_rq-ny.
  clear gs_rq.
  read table gt_rq into gs_rq index  10.
  m10 = gs_rq-ny.
  clear gs_rq.
  read table gt_rq into gs_rq index  11.
  m11 = gs_rq-ny.
  clear gs_rq.
  read table gt_rq into gs_rq index  12.
  m12 = gs_rq-ny.
  clear gs_rq.
  read table gt_rq into gs_rq index  13.
  m13 = gs_rq-ny.
  clear gs_rq.
  read table gt_rq into gs_rq index  14.
  m14 = gs_rq-ny.
  clear gs_rq.
  read table gt_rq into gs_rq index  15.
  m15 = gs_rq-ny.
  clear gs_rq.
  read table gt_rq into gs_rq index  16.
  m16 = gs_rq-ny.
  clear gs_rq.
  read table gt_rq into gs_rq index  17.
  m17 = gs_rq-ny.
  clear gs_rq.
  read table gt_rq into gs_rq index  18.
  m18 = gs_rq-ny.
  clear gs_rq.
  read table gt_rq into gs_rq index  19.
  m19 = gs_rq-ny.
  clear gs_rq.
  read table gt_rq into gs_rq index  20.
  m20 = gs_rq-ny.
  clear gs_rq.
  read table gt_rq into gs_rq index  21.
  m21 = gs_rq-ny.
  clear gs_rq.
  read table gt_rq into gs_rq index  22.
  m22 = gs_rq-ny.
  clear gs_rq.
  read table gt_rq into gs_rq index  23.
  m23 = gs_rq-ny.
  clear gs_rq.
  read table gt_rq into gs_rq index  24.
  m24 = gs_rq-ny.
  clear gs_rq.
  read table gt_rq into gs_rq index  25.
  m25 = gs_rq-ny.
  clear gs_rq.
*******根据开始日期计算25个月的年月

*取销售订单实际客户
  select *
    into corresponding fields of table gt_vbpa
    from vbpa
    for all entries in gt_hz1
    where vbeln = gt_hz1-vgbel
    and parvw = 'Z1'
    and posnr = ''.

*根据实际客户取实际业务员
  select *
    into corresponding fields of table gt_knvp
    from knvp
    for all entries in gt_vbpa
    where kunnr = gt_vbpa-kunnr
    and parvw = 'Z3'.

*获取BP描述
  select *
    into corresponding fields of table gt_but000
    from but000.

*取实际客户对应的销售办公室
  select *
    into corresponding fields of table gt_knvv
    from knvv
    for all entries in gt_knvp
    where kunnr = gt_knvp-kunnr.

*取销售办公室描述
  select *
    into corresponding fields of table gt_tvkbt
    from tvkbt.

*获取物料描述
  select *
    into corresponding fields of table gt_makt
    from makt
    for all entries in gt_hz1
    where matnr = gt_hz1-matnr and spras = '1'.

*取物料基本单位
  select *
    into corresponding fields of table gt_mara
    from mara
    for all entries in gt_hz1
    where matnr = gt_hz1-matnr.

*单位描述
  select *
    into corresponding fields of table gt_t006a
    from t006a.

*取销售订单汇率
  select *
    into corresponding fields of table gt_vbkd1
    from vbkd
    for all entries in gt_hz1
    where vbeln = gt_hz1-vgbel
    and posnr = ''.

  select *
    into corresponding fields of table gt_vbkd2
    from vbkd
    for all entries in gt_hz1
    where vbeln = gt_hz1-vgbel
    and posnr = gt_hz1-vgpos.

*取物料组描述
  select *
    into corresponding fields of table gt_t023t
    from t023t
    where spras = '1'.



*获取退货类型
  select
    pstyv "行项目类别
    shkzg "退货标志
    into corresponding fields of table gt_tvap
    from tvap.

***************取财务汇率
  types: begin of ly_tcurr,
           kurst  type tcurr-kurst,
           fcurr  type tcurr-fcurr,
           tcurr  type tcurr-tcurr,
           gdatu  type tcurr-gdatu,
           zgdatu type tcurr-gdatu,
           ukurs  type tcurr-ukurs,
           ffact  type tcurr-ffact,
           tfact  type tcurr-tfact,
         end of ly_tcurr.



  data: gt_hl  type table of ly_tcurr,
        gs_hl  type ly_tcurr,
        gt_hl2 type table of ly_tcurr,
        gs_hl2 type ly_tcurr.

  select *
    into corresponding fields of table gt_hl
    from tcurr
    where kurst = 'M'
    and tcurr = 'CNY'
    .

  loop at gt_hl into gs_hl.
    gs_hl-zgdatu = '99999999' - gs_hl-gdatu.
    modify gt_hl from gs_hl.
    clear gs_hl.
  endloop.


***************取财务汇率：根据财务汇率表，根据过账日期取最近的一笔汇率，处理逻辑：取出整张汇率表，降序，取第一笔；

  loop at gt_hz1 into gs_hz1.


    if gs_hz1-waerk <> 'CNY'.
      append lines of gt_hl to gt_hl2.
      delete gt_hl2 where zgdatu > gs_hz1-wadat_ist.
      sort gt_hl2 descending by zgdatu.
      read table gt_hl2 into gs_hl2 with key fcurr = gs_hz1-waerk.
      if sy-subrc = 0 .
        gs_hz1-hl = gs_hl2-ukurs.
      endif.
    else.
      gs_hz1-hl = 1.
    endif.
    clear gt_hl2.


    read table gt_vbpa into gs_vbpa with key vbeln = gs_hz1-vgbel.
    if sy-subrc = 0.
      gs_hz1-sjkh = gs_vbpa-kunnr.
      read table gt_knvp into gs_knvp with key kunnr = gs_hz1-sjkh   vkorg = gs_hz1-vkorg vtweg = gs_hz1-vtweg spart = gs_hz1-spart.
      if sy-subrc = 0.
        gs_hz1-sjywy =  gs_knvp-kunn2. "实际业务员
      endif.
      read table gt_knvv into gs_knvv with key kunnr = gs_hz1-sjkh vkorg = gs_hz1-vkorg vtweg = gs_hz1-vtweg spart = gs_hz1-spart.
      if sy-subrc = 0.
        gs_hz1-vkbur = gs_knvv-vkbur. "实际客户销售办公室
      endif.


    endif.

    if gs_hz1-umvkn is not initial."计算基本数量，若分母为空时，值=交货数；若值不为空或0，值=交货数*分子*分母
      gs_hz1-jbsl =  gs_hz1-lfimg * gs_hz1-umvkz / gs_hz1-umvkn.
    else.
      gs_hz1-jbsl =  gs_hz1-lfimg.
    endif.
*计算不含税金额
    read table gt_vbap into gs_vbap with key vbeln = gs_hz1-vgbel posnr = gs_hz1-vgpos.
    if sy-subrc = 0.
      if gs_vbap-kwmeng is not initial.
        gs_hz1-bhsje = ( gs_hz1-lfimg / gs_vbap-kwmeng ) * gs_vbap-netwr * gs_hz1-hl .
      endif.
    endif.

    clear gs_vbap.

    gs_hz1-ny = gs_hz1-wadat_ist+0(6). "获取过账日期年月

    modify gt_hz1 from gs_hz1.
    clear gs_knvp.
    clear gs_knvv.
    clear gs_vbpa.
    clear gs_hz1.
  endloop.

*过滤实际业务员、实际客户
  delete gt_hz1 where sjkh not in s_sjkh.
  delete gt_hz1 where sjywy not in s_sjywy.



*根据物料、年月、销售办公室、实际业务员、实际客户汇总基本数量、不含税金额
  loop at gt_hz1 into gs_hz1.
    read table gt_vbap into gs_vbap with key vbeln = gs_hz1-vgbel posnr = gs_hz1-vgpos .
    if sy-subrc = 0.
      read table gt_tvap into gs_tvap with key pstyv = gs_vbap-pstyv.
      if sy-subrc = 0.
        if gs_tvap-shkzg = 'X'.   "退货时，将数量金额变为负数
          gs_hz2-jbsl = 0 - gs_hz1-jbsl.
          gs_hz2-bhsje = 0 - gs_hz1-bhsje .
        else.
          gs_hz2-jbsl = gs_hz1-jbsl.
          gs_hz2-bhsje =  gs_hz1-bhsje .

        endif.
      endif.
    endif.
    clear gs_tvap.


    gs_hz2-matnr = gs_hz1-matnr.
    gs_hz2-ny = gs_hz1-ny.

    gs_hz2-vkbur = gs_hz1-vkbur.
    gs_hz2-sjywy = gs_hz1-sjywy.
    gs_hz2-sjkh = gs_hz1-sjkh.
    gs_hz2-werks = gs_hz1-werks.

    collect gs_hz2 into gt_hz2.
    clear gs_hz1.
    clear gs_hz2.
  endloop.

*  cl_demo_output=>display( gt_alv ).

*将交货单汇总结果中的物料、实际客户、实际业务员、销售办公室、工厂写入输出表GT_ALV中，再去重
  loop at gt_hz2 into gs_hz2.
    gs_alv-matnr = gs_hz2-matnr. "物料
    gs_alv-objek = gs_hz2-matnr."物料对象
    gs_alv-sjkh = gs_hz2-sjkh. "实际客户
    gs_alv-sjywy = gs_hz2-sjywy."实际业务员
    gs_alv-vkbur = gs_hz2-vkbur."销售办公室
    gs_alv-werks = gs_hz2-werks."工厂

    append gs_alv to gt_alv.
  endloop.
  sort gt_alv by matnr sjkh sjywy vkbur werks. "排序
  delete adjacent duplicates from gt_alv comparing matnr sjkh sjywy vkbur werks."去重


********************取物料特性(包装规格 ZPSPE；标准名称 ZSTDNAME；客户物料编码：ZCUMCODE)

  data:bzgg   type cabn-atinn,
       bzmc   type cabn-atinn, "标准名称
       khwlbm type cabn-atinn. "客户笏料编码
  data: gt_cabn type table of cabn,
        gs_cabn type cabn,
        gt_ausp type table of ausp,
        gs_ausp type ausp
        .

  select *
    into corresponding fields of table gt_cabn
    from cabn
    where atnam in ('ZPSPE','ZSTDNAME','ZCUMCODE').

  read table gt_cabn into gs_cabn with key atnam = 'ZPSPE'.
  if sy-subrc = 0.
    bzgg = gs_cabn-atinn.
  endif.

  read table gt_cabn into gs_cabn with key atnam = 'ZSTDNAME'.
  if sy-subrc = 0.
    bzmc = gs_cabn-atinn.
  endif.

  read table gt_cabn into gs_cabn with key atnam = 'ZCUMCODE'.
  if sy-subrc = 0.
    khwlbm = gs_cabn-atinn.
  endif.


  select *
    into corresponding fields of table gt_ausp
    from ausp
    for all entries in gt_alv
    where objek = gt_alv-objek
    and atinn in (bzgg,bzmc,khwlbm).


*********************取物料特性(包装规格 ZPSPE)

*输出表增加数量
  loop at gt_alv into gs_alv.
    read table gt_mara into gs_mara with key matnr = gs_alv-matnr .
    if sy-subrc = 0.
      gs_alv-meins = gs_mara-meins. "单位
      gs_alv-groes = gs_mara-groes."规格

      gs_alv-matkl = gs_mara-matkl. "剂型
      read table gt_t023t into gs_t023t with key matkl = gs_alv-matkl.
      if sy-subrc = 0.
        gs_alv-matkl_des = gs_t023t-wgbez."剂型描述
      endif.
      clear gs_t023t.

      gs_alv-matkl2 = gs_mara-matkl+0(4)."中类
      read table gt_t023t into gs_t023t with key matkl = gs_alv-matkl2.
      if sy-subrc = 0.
        gs_alv-matkl2_des = gs_t023t-wgbez."中类描述
      endif.
      clear gs_t023t.

      read table gt_t006a into gs_t006a with key msehi = gs_mara-meins.
      if sy-subrc = 0.
        gs_alv-meins_des = gs_t006a-mseh6.
      endif.
      clear gs_t006a.
    endif.
    clear gs_mara.

    read table gt_but000 into gs_but000 with key partner = gs_alv-sjkh. "实际客户简称、描述
    if sy-subrc = 0.
      gs_alv-bu_sort1 = gs_but000-bu_sort1.
      concatenate gs_but000-name_org1 gs_but000-name_org2 gs_but000-name_org3 into gs_alv-sjkh_des.
    endif.
    clear gs_but000.


    read table gt_but000 into gs_but000 with key partner = gs_alv-sjywy. "实际业务员名称
    if sy-subrc = 0.
      gs_alv-sjywy_des = gs_but000-name_org1.
    endif.
    clear gs_but000.

    read table gt_tvkbt into gs_tvkbt with key vkbur = gs_alv-vkbur spras = '1'. "销售办公室名称
    if sy-subrc = 0 .
      gs_alv-vkbur_des = gs_tvkbt-bezei.
    endif.

    read table gt_makt into gs_makt with key matnr =  gs_alv-matnr."物料描述
    if sy-subrc = 0.
      gs_alv-maktx = gs_makt-maktx.
    endif.
    clear gs_makt.

    read table gt_ausp into gs_ausp with key objek = gs_alv-objek atinn = bzgg. "包装规格
    if sy-subrc = 0.
      gs_alv-bzgg_des = gs_ausp-atwrt.
    endif.
    clear gs_ausp.

    read table gt_ausp into gs_ausp with key objek = gs_alv-objek atinn = bzmc. "标准名称
    if sy-subrc = 0.
      gs_alv-bzmc_des = gs_ausp-atwrt.
    endif.
    clear gs_ausp.

    read table gt_ausp into gs_ausp with key objek = gs_alv-objek atinn = khwlbm. "客户物料编码
    if sy-subrc = 0.
      gs_alv-khwlbm_des = gs_ausp-atwrt.
    endif.
    clear gs_ausp.

*根据物料、实际客户、实际业务员、销售办公室、工厂取汇总的数量与金额。
    read table gt_hz2 into gs_hz2 with key matnr = gs_alv-matnr sjkh = gs_alv-sjkh sjywy = gs_alv-sjywy vkbur = gs_alv-vkbur werks = gs_alv-werks ny = m1. "第1个月汇总
    if sy-subrc = 0 .
      gs_alv-m1sl = gs_hz2-jbsl.
      gs_alv-m1jz = gs_hz2-bhsje.
    endif.


    read table gt_hz2 into gs_hz2 with key matnr = gs_alv-matnr sjkh = gs_alv-sjkh sjywy = gs_alv-sjywy vkbur = gs_alv-vkbur werks = gs_alv-werks ny = m2. "第2个月汇总
    if sy-subrc = 0 .
      gs_alv-m2sl = gs_hz2-jbsl.
      gs_alv-m2jz = gs_hz2-bhsje.
    endif.


    read table gt_hz2 into gs_hz2 with key matnr = gs_alv-matnr sjkh = gs_alv-sjkh sjywy = gs_alv-sjywy vkbur = gs_alv-vkbur werks = gs_alv-werks ny = m3. "第一个月汇总
    if sy-subrc = 0 .
      gs_alv-m3sl = gs_hz2-jbsl.
      gs_alv-m3jz = gs_hz2-bhsje.
    endif.


    read table gt_hz2 into gs_hz2 with key matnr = gs_alv-matnr sjkh = gs_alv-sjkh sjywy = gs_alv-sjywy vkbur = gs_alv-vkbur werks = gs_alv-werks ny = m4. "第4个月汇总
    if sy-subrc = 0 .
      gs_alv-m4sl = gs_hz2-jbsl.
      gs_alv-m4jz = gs_hz2-bhsje.
    endif.


    read table gt_hz2 into gs_hz2 with key matnr = gs_alv-matnr sjkh = gs_alv-sjkh sjywy = gs_alv-sjywy vkbur = gs_alv-vkbur werks = gs_alv-werks ny = m5. "第5个月汇总
    if sy-subrc = 0 .
      gs_alv-m5sl = gs_hz2-jbsl.
      gs_alv-m5jz = gs_hz2-bhsje.
    endif.


    read table gt_hz2 into gs_hz2 with key matnr = gs_alv-matnr sjkh = gs_alv-sjkh sjywy = gs_alv-sjywy vkbur = gs_alv-vkbur werks = gs_alv-werks ny = m6. "第6个月汇总
    if sy-subrc = 0 .
      gs_alv-m6sl = gs_hz2-jbsl.
      gs_alv-m6jz = gs_hz2-bhsje.
    endif.


    read table gt_hz2 into gs_hz2 with key matnr = gs_alv-matnr sjkh = gs_alv-sjkh sjywy = gs_alv-sjywy vkbur = gs_alv-vkbur werks = gs_alv-werks ny = m7. "第7个月汇总
    if sy-subrc = 0 .
      gs_alv-m7sl = gs_hz2-jbsl.
      gs_alv-m7jz = gs_hz2-bhsje.
    endif.


    read table gt_hz2 into gs_hz2 with key matnr = gs_alv-matnr sjkh = gs_alv-sjkh sjywy = gs_alv-sjywy vkbur = gs_alv-vkbur werks = gs_alv-werks ny = m8. "第8个月汇总
    if sy-subrc = 0 .
      gs_alv-m8sl = gs_hz2-jbsl.
      gs_alv-m8jz = gs_hz2-bhsje.
    endif.


    read table gt_hz2 into gs_hz2 with key matnr = gs_alv-matnr sjkh = gs_alv-sjkh sjywy = gs_alv-sjywy vkbur = gs_alv-vkbur werks = gs_alv-werks ny = m9. "第9个月汇总
    if sy-subrc = 0 .
      gs_alv-m9sl = gs_hz2-jbsl.
      gs_alv-m9jz = gs_hz2-bhsje.
    endif.


    read table gt_hz2 into gs_hz2 with key matnr = gs_alv-matnr sjkh = gs_alv-sjkh sjywy = gs_alv-sjywy vkbur = gs_alv-vkbur werks = gs_alv-werks ny = m10. "第10个月汇总
    if sy-subrc = 0 .
      gs_alv-m10sl = gs_hz2-jbsl.
      gs_alv-m10jz = gs_hz2-bhsje.
    endif.


    read table gt_hz2 into gs_hz2 with key matnr = gs_alv-matnr sjkh = gs_alv-sjkh sjywy = gs_alv-sjywy vkbur = gs_alv-vkbur werks = gs_alv-werks ny = m11. "第11个月汇总
    if sy-subrc = 0 .
      gs_alv-m11sl = gs_hz2-jbsl.
      gs_alv-m11jz = gs_hz2-bhsje.
    endif.


    read table gt_hz2 into gs_hz2 with key matnr = gs_alv-matnr sjkh = gs_alv-sjkh sjywy = gs_alv-sjywy vkbur = gs_alv-vkbur werks = gs_alv-werks ny = m12. "第12个月汇总
    if sy-subrc = 0 .
      gs_alv-m12sl = gs_hz2-jbsl.
      gs_alv-m12jz = gs_hz2-bhsje.
    endif.
*去前导0
    call function 'CONVERSION_EXIT_ALPHA_OUTPUT' "去前导零
      exporting
        input  = gs_alv-matnr
      importing
        output = gs_alv-matnr.


    modify gt_alv from gs_alv.
    clear gs_hz2.
    clear gs_alv.
  endloop.

  "ADD BY LANJINGCUN 添加未发货销售订单数据-S742
  perform sub_get_day ."获取开始日期和结束日期
  perform sub_get_vbak ."获取销售订单
  perform sub_get_vbep ."获取计划行
  perform sub_get_vbkd ."获取汇率
  perform sub_get_vbpa ."获取合作伙伴
  perform sub_get_but000 ."
  perform sub_get_lips ."获取已发货
  perform sub_add_alv ."加入alv
  "ADD BY LANJINGCUN 添加未发货销售订单数据-E742



*替换输出描述
  data: m1sl(20)  type c,
        m2sl(20)  type c,
        m3sl(20)  type c,
        m4sl(20)  type c,
        m5sl(20)  type c,
        m6sl(20)  type c,
        m7sl(20)  type c,
        m8sl(20)  type c,
        m9sl(20)  type c,
        m10sl(20) type c,
        m11sl(20) type c,
        m12sl(20) type c,
        m13sl(20)  type c,
        m14sl(20)  type c,
        m15sl(20)  type c,
        m16sl(20)  type c,
        m17sl(20)  type c,
        m18sl(20)  type c,
        m19sl(20)  type c,
        m20sl(20)  type c,
        m21sl(20)  type c,
        m22sl(20)  type c,
        m23sl(20)  type c,
        m24sl(20)  type c,
        m25sl(20)  type c,

        m1jz(20)  type c,
        m2jz(20)  type c,
        m3jz(20)  type c,
        m4jz(20)  type c,
        m5jz(20)  type c,
        m6jz(20)  type c,
        m7jz(20)  type c,
        m8jz(20)  type c,
        m9jz(20)  type c,
        m10jz(20) type c,
        m11jz(20) type c,
        m12jz(20) type c,
        m13jz(20)  type c,
        m14jz(20)  type c,
        m15jz(20)  type c,
        m16jz(20)  type c,
        m17jz(20)  type c,
        m18jz(20)  type c,
        m19jz(20)  type c,
        m20jz(20)  type c,
        m21jz(20)  type c,
        m22jz(20)  type c,
        m23jz(20)  type c,
        m24jz(20)  type c,
        m25jz(20)  type c.

  concatenate m1 '出库数量（基本单位）' into m1sl.
  concatenate m2 '出库数量（基本单位）' into m2sl.
  concatenate m3 '出库数量（基本单位）' into m3sl.
  concatenate m4 '出库数量（基本单位）' into m4sl.
  concatenate m5 '出库数量（基本单位）' into m5sl.
  concatenate m6 '出库数量（基本单位）' into m6sl.
  concatenate m7 '出库数量（基本单位）' into m7sl.
  concatenate m8 '出库数量（基本单位）' into m8sl.
  concatenate m9 '出库数量（基本单位）' into m9sl.
  concatenate m10 '出库数量（基本单位）' into m10sl.
  concatenate m11 '出库数量（基本单位）' into m11sl.
  concatenate m12 '出库数量（基本单位）' into m12sl.
  concatenate m13 '出库数量（基本单位）' into m13sl.
  concatenate m14 '出库数量（基本单位）' into m14sl.
  concatenate m15 '出库数量（基本单位）' into m15sl.
  concatenate m16 '出库数量（基本单位）' into m16sl.
  concatenate m17 '出库数量（基本单位）' into m17sl.
  concatenate m18 '出库数量（基本单位）' into m18sl.
  concatenate m19 '出库数量（基本单位）' into m19sl.
  concatenate m20 '出库数量（基本单位）' into m20sl.
  concatenate m21 '出库数量（基本单位）' into m21sl.
  concatenate m22 '出库数量（基本单位）' into m22sl.
  concatenate m23 '出库数量（基本单位）' into m23sl.
  concatenate m24 '出库数量（基本单位）' into m24sl.
  concatenate m25 '出库数量（基本单位）' into m25sl.
  concatenate m1 '不含税金额CNY' into m1jz.
  concatenate m2 '不含税金额CNY' into m2jz.
  concatenate m3 '不含税金额CNY' into m3jz.
  concatenate m4 '不含税金额CNY' into m4jz.
  concatenate m5 '不含税金额CNY' into m5jz.
  concatenate m6 '不含税金额CNY' into m6jz.
  concatenate m7 '不含税金额CNY' into m7jz.
  concatenate m8 '不含税金额CNY' into m8jz.
  concatenate m9 '不含税金额CNY' into m9jz.
  concatenate m10 '不含税金额CNY' into m10jz.
  concatenate m11 '不含税金额CNY' into m11jz.
  concatenate m12 '不含税金额CNY' into m12jz.
  concatenate m13 '不含税金额CNY' into m13jz.
  concatenate m14 '不含税金额CNY' into m14jz.
  concatenate m15 '不含税金额CNY' into m15jz.
  concatenate m16 '不含税金额CNY' into m16jz.
  concatenate m17 '不含税金额CNY' into m17jz.
  concatenate m18 '不含税金额CNY' into m18jz.
  concatenate m19 '不含税金额CNY' into m19jz.
  concatenate m20 '不含税金额CNY' into m20jz.
  concatenate m21 '不含税金额CNY' into m21jz.
  concatenate m22 '不含税金额CNY' into m22jz.
  concatenate m23 '不含税金额CNY' into m23jz.
  concatenate m24 '不含税金额CNY' into m24jz.
  concatenate m25 '不含税金额CNY' into m25jz.



  perform catalog.
  perform output.

form catalog."表头
  w_repid = sy-repid.
  clear fieldcat.
  clear sy-tabix.
  define fieldcatset."宏定义
    fieldcat-fieldname = &1."透明表字段名
    fieldcat-seltext_l = &2."ALV列名
    fieldcat-col_pos = &3."列位置

    APPEND fieldcat.
  end-of-definition.

*layout-colwidth_optimize = 'X'.
*  layout-zebra = 'X'."斑马线的样式

  fieldcatset       'MATNR'     '物料'              sy-tabix.
  fieldcatset       'MAKTX'     '物料描述'              sy-tabix.
  fieldcatset       'MATKL'     '剂型'              sy-tabix.
  fieldcatset       'MATKL_DES'     '剂型'              sy-tabix.
  fieldcatset       'MATKL2'     '物料中类'              sy-tabix.
  fieldcatset       'MATKL2_DES'     '物料中类'              sy-tabix.
  fieldcatset       'GROES'     '产品规格'              sy-tabix.
  fieldcatset       'VKBUR'     '二级部门'            sy-tabix.
  fieldcatset       'VKBUR_DES'     '二级部门'            sy-tabix.
  fieldcatset       'WERKS'     '工厂'              sy-tabix.
  fieldcatset       'SJKH'     '实际客户'          sy-tabix.
  fieldcatset       'SJKH_DES'     '实际客户描述'          sy-tabix.
  fieldcatset       'BU_SORT1'     '实际客户简称'          sy-tabix.
  fieldcatset       'SJYWY'     '实际业务员'          sy-tabix.
  fieldcatset       'SJYWY_DES'     '实际业务员'          sy-tabix.
  fieldcatset       'BZGG_DES'     '包装规格'          sy-tabix.
  fieldcatset       'BZMC_DES'     '标准名称'          sy-tabix.
  fieldcatset       'KHWLBM_DES'     '客户物料编码'          sy-tabix.
  fieldcatset       'MEINS'     '基本单位'          sy-tabix.
  fieldcatset       'MEINS_DES'     '基本单位'          sy-tabix.
  fieldcatset       'M1SL'     m1sl          sy-tabix.
  fieldcatset       'M1JZ'     m1jz          sy-tabix.
  fieldcatset       'M2SL'     m2sl          sy-tabix.
  fieldcatset       'M2JZ'     m2jz          sy-tabix.
  fieldcatset       'M3SL'     m3sl          sy-tabix.
  fieldcatset       'M3JZ'     m3jz          sy-tabix.
  fieldcatset       'M4SL'     m4sl          sy-tabix.
  fieldcatset       'M4JZ'     m4jz          sy-tabix.
  fieldcatset       'M5SL'     m5sl          sy-tabix.
  fieldcatset       'M5JZ'     m5jz          sy-tabix.
  fieldcatset       'M6SL'     m6sl          sy-tabix.
  fieldcatset       'M6JZ'     m6jz          sy-tabix.
  fieldcatset       'M7SL'     m7sl          sy-tabix.
  fieldcatset       'M7JZ'     m7jz          sy-tabix.
  fieldcatset       'M8SL'     m8sl          sy-tabix.
  fieldcatset       'M8JZ'     m8jz          sy-tabix.
  fieldcatset       'M9SL'     m9sl          sy-tabix.
  fieldcatset       'M9JZ'     m9jz          sy-tabix.
  fieldcatset       'M10SL'     m10sl          sy-tabix.
  fieldcatset       'M10JZ'     m10jz          sy-tabix.
  fieldcatset       'M11SL'     m11sl          sy-tabix.
  fieldcatset       'M11JZ'     m11jz          sy-tabix.
  fieldcatset       'M12SL'     m12sl          sy-tabix.
  fieldcatset       'M12JZ'     m12jz          sy-tabix.

  fieldcatset       'M13SL'     m13sl          sy-tabix.
  fieldcatset       'M13JZ'     m13jz          sy-tabix.
  fieldcatset       'M14SL'     m14sl          sy-tabix.
  fieldcatset       'M14JZ'     m14jz          sy-tabix.
  fieldcatset       'M15SL'     m15sl          sy-tabix.
  fieldcatset       'M15JZ'     m15jz          sy-tabix.
  fieldcatset       'M16SL'     m16sl          sy-tabix.
  fieldcatset       'M16JZ'     m16jz          sy-tabix.
  fieldcatset       'M17SL'     m17sl          sy-tabix.
  fieldcatset       'M17JZ'     m17jz          sy-tabix.
  fieldcatset       'M18SL'     m18sl          sy-tabix.
  fieldcatset       'M18JZ'     m18jz          sy-tabix.
  fieldcatset       'M19SL'     m19sl          sy-tabix.
  fieldcatset       'M19JZ'     m19jz          sy-tabix.
  fieldcatset       'M20SL'     m20sl          sy-tabix.
  fieldcatset       'M20JZ'     m20jz          sy-tabix.
  fieldcatset       'M21SL'     m21sl          sy-tabix.
  fieldcatset       'M21JZ'     m21jz          sy-tabix.
  fieldcatset       'M22SL'     m22sl          sy-tabix.
  fieldcatset       'M22JZ'     m22jz          sy-tabix.
  fieldcatset       'M23SL'     m23sl          sy-tabix.
  fieldcatset       'M23JZ'     m23jz          sy-tabix.
  fieldcatset       'M24SL'     m24sl          sy-tabix.
  fieldcatset       'M24JZ'     m24jz          sy-tabix.
  fieldcatset       'M25SL'     m25sl          sy-tabix.
  fieldcatset       'M25JZ'     m25jz          sy-tabix.

endform.

form output.
  "layout-
  layout-box_fieldname = 'SEL'.


  call function 'REUSE_ALV_GRID_DISPLAY'
    exporting
      i_callback_program = w_repid    "程序名，必填
      it_fieldcat        = fieldcat[]        "输出字段
      is_layout          = layout
      i_save             = 'A'
    tables
      t_outtab           = gt_alv
    exceptions
      program_error      = 1
      others             = 2.
  if sy-subrc <> 0.
* Implement suitable error handling here
  endif.
endform.

form frm_authority_check.
  data lv_vkorg like line of s_vkorg.
  data lv_vtweg like line of s_vtweg.
  loop at s_vkorg into lv_vkorg.
    authority-check object 'V_VBAK_VKO'
     id 'VKORG'  field lv_vkorg-low
     id 'VTWEG'  dummy
     id 'SPART'  dummy
     id 'ACTVT'  dummy.
    if sy-subrc <> 0.
      a01 = 'X'.
* Implement a suitable exception handling here
      message '无相关销售组织权限' type 'S' display like 'E'.

    endif.
    if a01 = 'X'.
      exit.
    endif.

    clear lv_vkorg.
  endloop.

  loop at s_vtweg into lv_vtweg.
    authority-check object 'V_VBAK_VKO'
     id 'VKORG'  dummy
     id 'VTWEG'  field lv_vtweg-low
     id 'SPART'  dummy
     id 'ACTVT'  dummy.
    if sy-subrc <> 0.
      a02 = 'X'.
      message '无相关渠道权限' type 'S' display like 'E'.
    endif.
    if a02 = 'X'.
      exit.
    endif.

    clear lv_vtweg.
  endloop.
*      MESSAGE 'YES' TYPE 'S' DISPLAY LIKE 'E'.

endform.

*cl_demo_output=>display( gt_alv ).
*&---------------------------------------------------------------------*
*& Form SUB_GET_DAY
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
form sub_get_day .

  "开始日期 如果开始查询日期比系统小 以实际为准 否者以查询日期为准
  if sy-datum > s_ksrq .
    gv_sd = sy-datum+0(6) && '01' .
  else .
    gv_sd = s_ksrq+0(6) && '01' .
  endif .

  "结束日期 取最后的月份
  clear gs_rq.
  read table gt_rq into gs_rq index  12.
  gv_ed = gs_rq-ny && '01' .
  call function 'DATE_GET_MONTH_LASTDAY'
    exporting
      i_date = gv_ed
    importing
      e_date = gv_ed.



endform.
*&---------------------------------------------------------------------*
*& Form SUB_GET_VBAK
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
form sub_get_vbak .
  clear gt_vbak .
  select
  vbak~vbeln  ,
  vbak~erdat  ,
  vbak~erzet  ,
  vbak~ernam  ,
  vbak~audat  ,
  vbak~vbtyp  ,
  vbak~auart  ,
  vbak~waerk  ,
  vbak~vkorg  ,
  vbak~vtweg  ,
  vbak~spart  ,
  vbak~vkgrp  ,
  vbak~vkbur  ,
  vbak~gsber  ,
  vbak~knumv  ,
  vbak~kunnr  ,
  vbak~zleix  ,
  vbak~zjied  ,
  vbap~posnr  ,
  vbap~matnr  ,
  vbap~charg  ,
  vbap~matkl  ,
  vbap~arktx  ,
  vbap~pstyv  ,
  vbap~umziz  ,
  vbap~umzin  ,
  vbap~meins  ,
  vbap~netpr  ,
  vbap~netwr  ,
  vbap~kpein  ,
  vbap~kwmeng ,
  vbap~kzwi1  ,
  vbap~werks  ,
  vbap~znbjhq
    from vbak
  left join vbap
  on vbak~vbeln = vbap~vbeln

  where vbak~vkorg in @s_vkorg
    and vbak~vtweg in @s_vtweg
    and vbap~matnr in @s_matnr
    and vbap~werks in @s_werks
    and vbap~znbjhq >= @gv_sd
    and vbap~znbjhq <= @gv_ed
    and exists ( select *
                    from vbpa
                    where vbeln = vbak~vbeln
                      and ( ( parvw = 'Z1' and kunnr in @s_sjkh ) or ( parvw = 'Z3'  and kunnr in @s_sjywy ) )
                      and posnr is initial

                   )
     and exists ( select *
                     from lips
                     where vbeln in @s_vbeln
                       and vgbel = vbak~vbeln
                        )
                        into table @gt_vbakl.
endform.
*&---------------------------------------------------------------------*
*& Form SUB_GET_VBEP
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
form sub_get_vbep .

*  DATA : LT_VBAK TYPE TABLE OF VBAK .
*  LT_VBAK[] = GT_VBAK[] .
*  SORT LT_VBAK BY VBELN  POSNR.
*  DELETE ADJACENT DUPLICATES FROM LT_VBAK
*                   COMPARING VBELN POSNR.

  select *
    into table gt_vbepl
    from vbep
    for all entries in gt_vbakl
    where vbeln = gt_vbakl-vbeln
      and posnr = gt_vbakl-posnr .


endform.
*&---------------------------------------------------------------------*
*& Form SUB_GET_VBKD
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
form sub_get_vbkd .
  data : lt_vbak type table of ty_vbakl .
  lt_vbak[] = gt_vbakl[] .
  sort lt_vbak by vbeln .
  delete adjacent duplicates from lt_vbak
                   comparing vbeln .

  select *
    from vbkd
    into table gt_vbkdl
    for all entries in lt_vbak
    where vbeln = lt_vbak-vbeln
      and posnr = '' .

endform.
*&---------------------------------------------------------------------*
*& Form SUB_GET_VBPA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
form sub_get_vbpa .
  data : lt_vbak type table of ty_vbakl .
  lt_vbak[] = gt_vbakl[] .
  sort lt_vbak by vbeln .
  delete adjacent duplicates from lt_vbak
                   comparing vbeln .

  select *
    into table gt_vbpal
    from vbpa
    for all entries in lt_vbak
    where vbeln = lt_vbak-vbeln
      and posnr = ''
      and parvw in ('Z1','Z3').

endform.
*&---------------------------------------------------------------------*
*& Form SUB_GET_BUT000
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
form sub_get_but000 .

  data : lt_vbpa type table of vbpa .

  lt_vbpa[] = gt_vbpal[] .
  sort lt_vbpa by kunnr .
  delete adjacent duplicates from lt_vbpa
           comparing kunnr .
  check lt_vbpa[] is not initial .

  select *
    into table gt_but000l
    from but000
    for all entries in lt_vbpa
    where partner = lt_vbpa-kunnr .

endform.
*&---------------------------------------------------------------------*
*& Form SUB_GET_LIPS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
form sub_get_lips .

  select *
    into table gt_lipsl
    from lips
    for all entries in gt_vbakl
    where vgbel = gt_vbakl-vbeln
      and vgpos = gt_vbakl-posnr
      and wbsta = 'C' .

endform.
*&---------------------------------------------------------------------*
*& Form SUB_ADD_ALV
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
form sub_add_alv .

  data : ls_alv type ly_out .
  data : lt_ausp type table of ausp .
  data : lv_sl type ekpo-menge .
  data : lv_je type vbap-netpr .
  data : lv_ny(6) type c .

  clear   gs_vbakl.
  loop at gt_vbakl into gs_vbakl .

    gs_alv-objek = gs_vbakl-matnr .
    gs_alv-matnr = gs_vbakl-matnr .
    gs_alv-matkl = gs_vbakl-matkl .
    gs_alv-werks = gs_vbakl-werks .
    gs_alv-meins = gs_vbakl-meins .
    gs_alv-vkbur = gs_vbakl-vkbur .

    lv_sl = gs_vbakl-kwmeng .
    clear gs_lipsl .
    loop at gt_lipsl into gs_lipsl where vgbel = gs_vbakl-vbeln
                                      and vgpos = gs_vbakl-posnr .

      lv_sl = lv_sl - gs_lipsl-lfimg .

    endloop .

    check lv_sl > 0 ."只有存在未发货数量才需要统计

    clear gs_vbkdl .
    read table gt_vbkdl into gs_vbkdl with key vbeln = gs_vbakl-vbeln .

    if gs_vbakl-waerk <> 'CNY'.
      if gs_vbkdl-kursk = 1.

        append lines of gt_hl to gt_hl2.
        delete gt_hl2 where zgdatu > gs_vbkdl-prsdt.
        sort gt_hl2 descending by zgdatu.
        read table gt_hl2 into gs_hl2 with key fcurr = gs_vbakl-waerk.
        if sy-subrc = 0 .
          gs_vbkdl-kurrf = gs_hl2-ukurs.
        endif.

      else.
        if gs_vbkdl-kurrf is initial.
          gs_vbkdl-kurrf = gs_vbkdl-kursk.
        else.
          gs_vbkdl-kurrf = gs_vbkdl-kurrf.
        endif.
      endif.
    else.
      gs_vbkdl-kurrf = 1.
    endif.




    lv_je = gs_vbakl-netwr /  gs_vbakl-kwmeng  * lv_sl * gs_vbkdl-kurrf."不含税金额
    lv_sl = lv_sl * gs_vbakl-umziz / gs_vbakl-umzin ."数量

    if gs_vbakl-znbjhq+0(6) = gv_sd+0(6) ."如果为开始日期的 并且计划行为ZB 数据加到下一个月
      read table gt_vbepl into gs_vbepl with  key vbeln = gs_vbakl-vbeln
                                                posnr = gs_vbakl-posnr
                                                ettyp = 'ZB' .
      if sy-subrc <> 0 .
        continue .
      else .
        call function 'RP_CALC_DATE_IN_INTERVAL'
          exporting
            date      = gs_vbakl-znbjhq
            days      = '00'
            months    = '01'
            signum    = '+'
            years     = '00'
          importing
            calc_date = gs_vbakl-znbjhq.
      endif .
    endif .
    lv_ny = gs_vbakl-znbjhq+0(6) .

    case lv_ny+0(6).
      when m1.
        gs_alv-m1sl = lv_sl.
        gs_alv-m1jz = lv_je.
      when m2.
        gs_alv-m2sl = lv_sl.
        gs_alv-m2jz = lv_je.
      when m3.
        gs_alv-m3sl = lv_sl.
        gs_alv-m3jz = lv_je.
      when m4.
        gs_alv-m4sl = lv_sl.
        gs_alv-m4jz = lv_je.
      when m5.
        gs_alv-m5sl = lv_sl.
        gs_alv-m5jz = lv_je.
      when m6.
        gs_alv-m6sl = lv_sl.
        gs_alv-m6jz = lv_je.
      when m7.
        gs_alv-m7sl = lv_sl.
        gs_alv-m7jz = lv_je.
      when m8.
        gs_alv-m8sl = lv_sl.
        gs_alv-m8jz = lv_je.
      when m9.
        gs_alv-m9sl = lv_sl.
        gs_alv-m9jz = lv_je.
      when m10.
        gs_alv-m10sl = lv_sl.
        gs_alv-m10jz = lv_je.
      when m11.
        gs_alv-m11sl = lv_sl.
        gs_alv-m11jz = lv_je.
      when m12.
        gs_alv-m12sl = lv_sl.
        gs_alv-m12jz = lv_je.
    endcase.


*************************************************************
    read table gt_alv into ls_alv with key objek = gs_alv-objek
                                           werks = gs_alv-werks .
    if sy-subrc = 0 .
      gs_alv-maktx       = ls_alv-maktx .
      gs_alv-matkl_des   = ls_alv-matkl_des .
      gs_alv-matkl2      = ls_alv-matkl2 .
      gs_alv-matkl2_des  = ls_alv-matkl2_des .
      gs_alv-groes       = ls_alv-groes .
      gs_alv-bzgg_des    = ls_alv-bzgg_des .
      gs_alv-bzmc_des    = ls_alv-bzmc_des .
      gs_alv-khwlbm_des  = ls_alv-khwlbm_des .
      gs_alv-meins_des   = ls_alv-meins_des .
    else .
      select single maktx
        into gs_alv-maktx
        from makt
        where matnr = gs_alv-matnr .

      read table gt_t023t into gs_t023t with key matkl = gs_alv-matkl.
      if sy-subrc = 0.
        gs_alv-matkl_des = gs_t023t-wgbez."剂型描述
      endif.

      gs_alv-matkl2 = gs_mara-matkl+0(4)."中类
      read table gt_t023t into gs_t023t with key matkl = gs_alv-matkl2.
      if sy-subrc = 0.
        gs_alv-matkl2_des = gs_t023t-wgbez."中类描述
      endif.

      select single groes
        into gs_alv-groes
        from mara
        where matnr = gs_alv-matnr .

      clear lt_ausp[] .
      select *
        into corresponding fields of table lt_ausp
        from ausp
        for all entries in gt_alv
        where objek = gt_alv-objek
        and atinn in (bzgg,bzmc,khwlbm).

      read table lt_ausp into gs_ausp with key objek = gs_alv-objek atinn = bzgg. "包装规格
      if sy-subrc = 0.
        gs_alv-bzgg_des = gs_ausp-atwrt.
      endif.
      clear gs_ausp.

      read table lt_ausp into gs_ausp with key objek = gs_alv-objek atinn = bzmc. "标准名称
      if sy-subrc = 0.
        gs_alv-bzmc_des = gs_ausp-atwrt.
      endif.
      clear gs_ausp.

      read table lt_ausp into gs_ausp with key objek = gs_alv-objek atinn = khwlbm. "客户物料编码
      if sy-subrc = 0.
        gs_alv-khwlbm_des = gs_ausp-atwrt.
      endif.
      clear gs_ausp.

      read table gt_t006a into gs_t006a with key msehi = gs_mara-meins.
      if sy-subrc = 0.
        gs_alv-meins_des = gs_t006a-mseh6.
      endif.

    endif .

    read table gt_tvkbt into gs_tvkbt with key vkbur = gs_alv-vkbur spras = '1'. "销售办公室名称
    if sy-subrc = 0 .
      gs_alv-vkbur_des = gs_tvkbt-bezei.
    endif.

    clear gs_vbpa .
    read table gt_vbpal into gs_vbpal with key vbeln = gs_vbakl-vbeln
                                             parvw = 'Z1' .
    if sy-subrc = 0 .
      gs_alv-sjkh = gs_vbpal-kunnr .
      read table gt_but000l into gs_but000l with key partner = gs_vbpal-kunnr .
      gs_alv-sjkh_des = gs_but000l-name_org1 .
      gs_alv-bu_sort1 = gs_but000l-bu_sort1 .
    endif .

    clear gs_vbpa .
    read table gt_vbpal into gs_vbpal with key vbeln = gs_vbakl-vbeln
                                             parvw = 'Z3' .
    if sy-subrc = 0 .
      gs_alv-sjywy = gs_vbpal-kunnr .
      read table gt_but000l into gs_but000l with key partner = gs_vbpal-kunnr .
      gs_alv-sjywy_des = gs_but000l-name_org1 .
    endif .

*去前导0
    call function 'CONVERSION_EXIT_ALPHA_OUTPUT' "去前导零
      exporting
        input  = gs_alv-matnr
      importing
        output = gs_alv-matnr.

    collect gs_alv into gt_alv .

  endloop .

endform.