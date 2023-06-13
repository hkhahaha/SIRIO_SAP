create or replace procedure SPCUS_TASKGROUP_engine
(IN_organizationId In Varchar2,
 IN_Warehouse     In Varchar2,
 IN_FMLOCATION    in varchar2, --请求位置
 IN_TRACEID       in varchar2, --请求托盘号
 IN_TOLOCATION    in varchar2, --目标位置，实体任务可为*
 IN_SKU           in varchar2,
 IN_qty           in number,
 IN_TYPE          in varchar2, -- 0 路由申请，1 空托盘补给，2 自由移动为虚拟任务（from - to ）
                               -- 以下为RF调用
                               -- 3 拣货完成，扫托盘进行下一步走向（余料回库，至配盘接驳位 共用）
                               -- 4 上架/上架完成，扫描货架产生回库指令
                               -- 5 盘点完成，盘点完扫描托盘或货架产生回库指令
                               -- 6 配盘完成，配盘好的货产生去缓存区暂存指令
                               -- 7 增加占位，手动放托盘在接驳位
                               -- 8 释放占位，手动从接驳位搬走托盘
                               -- 9 单个空托盘回收，回收至拆码盘机
                               -- 10 整垛空托盘回库，回立库上架存储
                               -- 11 任务取消
                               -- 99 为定时器调用，不考虑其他参数
 IN_UserID        In  Varchar2,
 IN_Language      In  Varchar2,
 OUT_Return_Code  out varchar2
)
is
/*
*****************************************************************
作者：      songyx
日期：    2022/03/14
功能描述：  同一托盘任务生成组号下发至WCS,同时控制任务是否具备下发条件，未生成组号不下发
单据.udfwcs = 'Y'     单据控制下发
bas_zone.udf05 = 'WCS1'/'WCS2' 设备可达，WCS1可达接驳位、WCS2可达货位
bas_locgroup2.udf01 = 'IN' / 'OUT'  出入库模式
bas_location.locationcategory  货位类型 JBW:接驳位，ZZW: 货架周转位
bas_location.udf01  双深位组号（每组双深位一个组号，用于控制同组同品同批，组号的可使用状态（如有远深位出禁止入，有近深位入远深位禁止出））
bas_location.udf02  默认指令优先级，空值 为50，（1远深位）50， （2近深位）51
bas_location.udf03  占位标识 Y ，当有任务使用时将往 cus_buff_location 写入数据
bas_zone.udf03      拣货托盘去向 PUT 配盘，BUF 缓存区，TO 直接产线
bas_zone.udf04      拣货去向目标库区
bas_location.plcount 用于控制接驳位的缓存数量（基于指令数来控制）
bas_location.locgroup2 区分同库区下的接驳位编组 （特别说明：kiva货位区为2层关系，接驳位+货架周转位 绑定为一个组号，根据实际情况编多少个组）
bsm_code.codetype = 'TSK_TYP' & bsm_code.UDF01 为默认优先级
bsm_code.codetype = 'SO_TYP' & bsm_code.UDF01 为订单目标在线拣选库组，对应bas_location.locgroup2 组号
bas_sku.reservedfield16 默认缠膜标记
inv_lot_loc_id.udf01  收货线下缠膜标记
doc_order_header.hedi11 --领料前置时间 （YYYY-MM-DD HH24:MI:SS）
doc_order_header.hedi12 --领料单目标接驳位（或接驳位组号--考虑后期扩展用）
doc_order_header.hedi13 --手功指定出口，绕开正常逻辑直达
******************************************************************
*         Change  Log               *
******************************************************************
Date    CR/EN No.    Programmer  Descr
------------------------------------------------------------------
 */
r_nrow int;
r_GROUPTASKID   varchar2(50); --任务组号
r_GROUPTASKID_NEW   varchar2(50); --任务组号
r_GROUPTASKID_OLD   varchar2(50); --任务组号
r_fmlocation    varchar2(50); --任务来源货位，如 kiva 移出需要转换为货架的移出位
r_fmzone        varchar2(50); --任务来源所在库存
r_fmlocgrp2     varchar2(50); --任务来源所在库位组2
r_tolocation    varchar2(50); --任务目标所在货位
r_tolocation_tmp varchar2(50);
r_putloction    varchar2(50);
r_tolocation_WCS    varchar2(50); --任务目标所在货位,发送给WCS的货位
r_tolocation_WCS_DOC varchar2(50);
r_tozone        varchar2(50); --任务目标所在区域
r_tozone_tmp        varchar2(50);
r_tolocgrp2     varchar2(50); --任务目标所在库位组2
r_fmloctype_wcs varchar2(50); --任务来源的可达类型 ，WCS1 可达接驳位 ,WCS2 可达货位 ,KIVA 可达kiva货架
r_toloctype_wcs varchar2(50); --任务目标的可达类型 ，WCS1 可达接驳位 ,WCS2 可达货位 ,KIVA 可达kiva货架
r_fmloctype     varchar2(50); --任务来源货位类型
r_toloctype     varchar2(50); --任务目标货位类型，根据可达类型 会转换为 JBW 接驳位 或 ZZW 货架周转位
r_fmlocmode     varchar2(50); --任务来源货位的模式 IN 入库模式，OUT 出库模式
r_tolocmode     varchar2(50); --任务目标货位的模式 IN 入库模式，OUT 出库模式
r_docno         varchar2(500);--单号拼接
r_doors         varchar2(500);--码头拼接
r_waveno        varchar2(50); --波次号
r_palletid      varchar2(50); --任务托务号，如是kiva 移出需要转换为货架号
r_tasktype      varchar2(50); --任务类型，V_ 开头为虚拟任务
r_tasktype_SEND      varchar2(50); --任务类型，V_ 开头为虚拟任务
r_tolocmode_SEND  varchar2(20);
r_message       varchar2(200);--错误信息
r_err_out_mesg  varchar2(2000);
r_send          varchar2(10); --是否下发任务标记
r_linkpalletid  varchar2(50); --关联托盘，上架时要求托盘到达接驳位才下发任务
r_taskstatus    varchar2(10); --初始任务状态,00 待处理，05 等待中，10 可以下发，70 已取货,80 到达，90 取消，99 完成
r_priority_doc          int;          --优先级
r_sendtime_appointment date;  --预约下发时间
r_TSK_FMBUF_MIN int; --预约前置时间(分)
r_sendtime_BUF         date;  --
r_TSK_TOBUF_MIN   int; --提前多少时间到缓存位，整托送产线托盘
r_in_taskflag   varchar2(10);
r_sku           varchar2(50);
r_IN_TRACEID    varchar2(50);
r_in_reqid      varchar2(50);
r_docout        varchar2(50); --订单指定的出口
r_docJBW        varchar2(50); --订单指定的目标交接位
r_direction       varchar2(50); --去向 PUT 配盘，BUF 缓存，TO 直送,OUT 出口
r_direction_zone  varchar2(50); --去向区域
r_fmaisleno         varchar2(50);
r_toaisleno         varchar2(50);
r_rpl_qty      number(12,2);
r_kc_qty      number(12,2);
r_linktask    int;
r_isPack      varchar2(50);
r_XWFLAG      varchar2(50);
R_EMPTYFLAG    varchar2(50);--空托盘标记--by zf 20220704
r_model  varchar2(10);
r_isgroupid varchar2(10);
r_doctype   varchar2(50);
r_xwsku     varchar2(50);
r_type_toloctype varchar(50);
r_type_toloction  varchar2(50);
r_type_toloction2 varchar2(50);
r_end_location varchar2(50);
r_SEND_DEBUG_MODEL varchar2(10);
r_SEND_FLAG   varchar2(10);
r_end_loctype varchar2(50);
r_end_zone varchar2(50);
r_end_model varchar2(50);
r_tolocation_1 varchar2(50);--by zf
r_parainfo     varchar(50);
r_int      int;
r_notes   varchar2(200);
r_tmp   varchar(50);
r_locgrouptype varchar2(200);
r_expectedshipmenttime2 date;
r_dateint int;
r_receivelocation varchar2(200);
r_crossdockflag varchar(50);

begin


--记日志
insert into cus_exec_engine_log (organizationid,warehouseid,in_fmlocation,in_traceid,in_tolocation,in_sku,in_qty,in_type,in_userid,in_language)
values (IN_organizationId,IN_Warehouse,IN_FMLOCATION,IN_TRACEID,IN_TOLOCATION,IN_SKU,IN_qty,IN_TYPE,IN_UserID,IN_Language);
commit;

 --加锁，等待完成 songyx 2022-10-21
 begin
   select id into r_tmp from  cus_engine_bf_control where id = 'SPCUS_TASKGROUP_ENGINE' for update wait 30; --等30秒

   update cus_engine_bf_control set id = 'SPCUS_TASKGROUP_ENGINE' where  id = 'SPCUS_TASKGROUP_ENGINE';
 exception
   when no_data_found then
     insert into cus_engine_bf_control (id) values ('SPCUS_TASKGROUP_ENGINE');
   end;


OUT_Return_Code:='000';
r_in_taskflag :='N';
r_IN_TRACEID  :=IN_TRACEID;
--获取系统参数
r_TSK_FMBUF_MIN :=nvl(GETSYS_configuration ('SIRIO', '*', '*', '*', 'TSK_FMBUF_MIN'),30);
r_TSK_TOBUF_MIN    :=nvl(GETSYS_configuration ('SIRIO', '*', '*', '*', 'TSK_TOBUF_MIN'),15);
r_SEND_DEBUG_MODEL := nvl(GETSYS_configuration ('SIRIO', '*', '*', '*', 'TSK_WCS_SEND_DEBUG'),'N');
--获取参数--基于性能考虑--by zf 20230111
r_dateint :=  nvl(GETSYS_configuration ('SIRIO', '*', '*', '*', 'CUS_WCSTASK_DATE'),7);
r_tasktype_SEND:='';
if r_SEND_DEBUG_MODEL = 'Y' then
  r_SEND_FLAG:='D'; --DEBUG 模式不下发任务
else
  r_SEND_FLAG:='N';
end if;
--***************************************************** in_type  = 0 路由申请***********************************************************************
--WCS请求入库
 --路向申请
 if IN_TYPE = '0' then

    if IN_FMLOCATION is null then
       rollback;
        r_err_out_mesg:='engine(0): 请求货位不能为空';
        r_direction:='ERR-OUT';
        goto ERR_OUT;
    end if;
    if r_IN_TRACEID is null then
       rollback;
        r_err_out_mesg:='engine(0): 请求托盘不能为空';
        r_direction:='ERR-OUT';
        goto ERR_OUT;
    end if;
   --增加一层处理 0 库存的逻辑 --2022-6-29 syx
   delete from inv_lot_loc_id iv where iv.organizationid = IN_organizationId
   and iv.warehouseid = IN_Warehouse
   and iv.qty = 0 and (iv.qtyallocated + iv.qtyrpin + iv.qtyrpout + iv.qtymvin + iv.qtymvout + iv.qtyonhold + iv.qtypa) = 0;

    --如果有同托盘的未完成的ERR-OUT 任务，直接完成
    --如果有从这个库位发起的请求，说明其它托盘已处理掉了，未完成的ERR-OUT 任务，直接完成
     update idx_asrs_send ias set ias.taskstatus = '85' where ias.organizationid = IN_organizationId and ias.warehouseid = IN_Warehouse
     and (ias.palletid = r_IN_TRACEID --or ias.fmlocation = IN_FMLOCATION
     )
     and ias.taskstatus <= '81' and ias.tasktype = 'ERR-OUT';

    --原货位
    begin
    select l.locationcategory into r_fmloctype from bas_location l where l.organizationid = IN_organizationId and l.warehouseid = IN_Warehouse and l.locationid = IN_FMLOCATION
    and rownum = 1 ;
    exception
      when no_data_found then
        r_fmloctype:='';
      end;

    --如果异常口的托盘，不能回立库的则退回异常口
    if r_fmloctype = 'ABNORMAL' then
        if nvl(IN_TOLOCATION,'*') = '*' then
         r_direction:='';
         --计算上架库位
         SIRIO_SPCUS_Calc_loc
         (IN_organizationId  ,
         IN_Warehouse      ,
         IN_FMLOCATION       ,
         r_IN_TRACEID          , --托盘号
         ''              , --SKU
         r_putloction        ,
         r_model             , --LOC 货位，LOG 货架
         'DATAHUB'           ,
         IN_Language        ,
         OUT_Return_Code
         );
         if  substr(OUT_Return_Code,1,3) <> '000' then
              rollback;
              --退出
              --return;
                r_err_out_mesg:='engine(0): 错误信息：'||OUT_Return_Code;
                r_direction:='ERR-OUT';
                goto ERR_OUT;
         end if;
         r_tolocation:=r_putloction;
       else
        --如果有货位传入，就不再计算目位库位
        r_tolocation := IN_TOLOCATION;
       end if;

       if  nvl(r_tolocation,'*') = '*' then
            r_err_out_mesg:='engine(0): 没有获取到目标货位，请检查！';
            r_direction:='ERR-OUT';
            goto ERR_OUT;
       end if;

       if r_model <> 'LOC1' then
            r_err_out_mesg:='engine(0): 非立库产品，不可入立库，请检查！';
            r_direction:='ERR-OUT';
            goto ERR_OUT;
       end if;

    end if;

    --如果立库有相应的库存，再从入库口或异常口发起任务，很可能是托盘码不一致，先处理好再入库
    if r_fmloctype in ('IN','ABNORMAL') then
      select count(0) into r_nrow from inv_lot_loc_id iv
      inner join bas_location bs on iv.organizationid = bs.organizationid and iv.warehouseid = bs.warehouseid and iv.locationid = bs.locationid
      where iv.organizationid = IN_organizationId and iv.warehouseid = IN_Warehouse and iv.traceid = r_IN_TRACEID
      and bs.zoneid in ('01','02','02A','03') and iv.qty > 0 ;
      if r_nrow > 0 then
            r_err_out_mesg:='engine(0): 托盘已在立库存有库存，可能为托盘标签重复，请检查！';
            r_direction:='ERR-OUT';
            goto ERR_OUT;
      end if;
    end if;


    --是否有待请求任务
    select count(0) into r_nrow from idx_asrs_send ias where ias.organizationid = IN_organizationId and ias.warehouseid = IN_Warehouse
     and ias.palletid = r_IN_TRACEID and ias.taskstatus < '70' and ias.tasktype <> 'ERR-OUT' and (ias.fmlocation = '待请求'
     or ias.fmlocation like 'STAGE%' or r_fmloctype in ('IN','IN_E','ABNORMAL')  )
     and ias.tasktype in ('PA','MV','V_MV') and ias.taskstatus < '10' ;
    if r_nrow > 0 then
      --有请求任务直接下发
      update idx_asrs_send ias set ias.fmlocation = IN_FMLOCATION,ias.wait_time = null,ias.edisendflag = 'N' where ias.organizationid = IN_organizationId
      and ias.warehouseid = IN_Warehouse and ias.palletid = r_IN_TRACEID and ias.taskstatus <= '10'
      and (ias.fmlocation = '待请求' or ias.fmlocation like 'STAGE%'  or r_fmloctype in ('IN','IN_E','ABNORMAL')  )
      and ias.tasktype in ('PA','MV','V_MV')
      and ias.taskstatus < '10' ;
      commit;
      r_in_taskflag  :='Y';
      goto to_send;  --直接到发送
    end if;
    --看是否重复请求
    select count(0) into r_nrow from idx_asrs_send ias where ias.organizationid = IN_organizationId and ias.warehouseid = IN_Warehouse
     and ias.palletid = r_IN_TRACEID and ias.fmlocation = IN_FMLOCATION and ias.taskstatus <= '10'  and ias.tasktype <> 'ERR-OUT' ;
    if r_nrow > 0 then
        r_err_out_mesg:='engine(0): 已有任务,不可重复请求,发送至WCS';
        --r_direction:='ERR-OUT';
        --goto ERR_OUT;
        --不用再下发
           update idx_asrs_send ias set ias.edisendflag = r_SEND_FLAG, ias.notes = r_err_out_mesg
           ,ias.wait_time = null
           where ias.organizationid = IN_organizationId
           and ias.warehouseid = IN_Warehouse and ias.palletid = r_IN_TRACEID and ias.fmlocation = IN_FMLOCATION and ias.taskstatus < '10';
           commit;
           goto to_send;  --直接到发送
         --return;
    end if;
    --查询托盘是否有任务存在
    /*select count(0),max(bzt.udf05) into r_nrow,r_toloctype_wcs from tsk_tasklists tk
    inner join bas_location l on tk.organizationid = l.organizationid and tk.warehouseid = l.warehouseid and tk.plantolocation = l.locationid
    inner join bas_zone bzt on l.organizationid = bzt.organizationid  and l.warehouseid = bzt.warehouseid and l.zoneid = bzt.zoneid
    where tk.organizationid = IN_organizationId and tk.warehouseid = IN_Warehouse
    and tk.fmid = r_IN_TRACEID  and tk.taskprocess = '00' -- and bzt.udf05 in ('WCS1','KIVA')
    and nvl(tk.addtime,sysdate) >= sysdate -7 and rownum = 1 ;
    if r_nrow = 0 then --没有任务就生成任务
      r_nrow:=1;
      r_toloctype_wcs:='';
    else
      if nvl(r_toloctype_wcs,'*') in ('WCS1','KIVA') then  --有生成任务还需考虑接驳位的移动
        r_nrow:=1;
        r_toloctype_wcs:='';
      end if;
    end if;*/


    --处理洗盘库位请求
     select count(0),max(l.zoneid),max(l.locgroup2)
      into r_nrow,r_tozone,r_tolocgrp2 from bas_location l where l.organizationid = IN_organizationId and l.warehouseid = IN_Warehouse
     and l.locationid = IN_FMLOCATION and l.locationcategory = 'OUT_X' ;
     if r_nrow > 0 then
       r_fmzone:=r_tozone;
       r_fmloctype:=r_toloctype;
       r_fmlocgrp2:=r_tolocgrp2;
       if r_IN_TRACEID IN ('ERR') or IN_TOLOCATION = '3FTRAYWASHER' then
         r_toloctype:='JBW';
         r_tozone:=r_tozone;
         r_tolocgrp2:=r_tolocgrp2;
         r_tolocation:='';
         r_tolocation_WCS:='';
         r_tasktype:='V_MV';
       else
         r_toloctype:='IN';
         r_tozone:='3FINSTEREO';
         r_tolocgrp2:='3FINPORT';
         r_tolocation:='';
         r_tolocation_WCS:='';
         r_tasktype:='V_MV';
       end if;

         --生成任务指令
          spcom_getidsequence(IN_organizationId,IN_Warehouse,IN_Language,'GROUPTASKID',r_GROUPTASKID,out_return_code);
             if substr(OUT_Return_Code,1,3) <> '000' then
               rollback;
               return;
              end if;
             insert into idx_asrs_send (organizationid,warehouseid,grouptaskid,tasktype,palletid,fmlocation,tolocation,tolocation_WCS,
                 taskstatus,priority,sendtime_appointment,addtime,addwho,docno,equiptype,businesstype,ispackage,waveno,emptyflag,palletdetails,tolocation_type
                 ,fmzone,fmlocgrp2,fmloctype_wcs,fmloctype,fmlocmode,tozone,tolocgrp2,toloctype_wcs,toloctype,tolocmode,notes,udf05,edisendflag)
             values (IN_organizationId,IN_Warehouse,r_GROUPTASKID,r_tasktype,(case when  r_IN_TRACEID = 'ERR' then r_GROUPTASKID else r_IN_TRACEID end),IN_FMLOCATION,'','',
                '00',10,r_sendtime_appointment,sysdate,IN_UserID,r_docno,'LT','',r_isPack,r_waveno,'N','',''
                ,r_fmzone,r_fmlocgrp2,'',r_fmloctype,'',r_tozone,r_tolocgrp2,'WCS2',r_toloctype,'','','in_type:0',r_SEND_FLAG);
             ---
              --写入日志
          insert into idx_asrs_send_log (organizationid,warehouseid,grouptaskid,palletid,status,addtime,addwho,notes)
                             values     (IN_organizationId,IN_Warehouse,r_GROUPTASKID,r_IN_TRACEID,'00',sysdate,IN_UserID,'');

        r_send:='Y';
       goto end_0;
     end if;



      --查询是不已下发任务
      select count(0) into r_nrow from idx_asrs_send ias where ias.organizationid = IN_organizationId and ias.warehouseid = IN_Warehouse
      and ias.palletid = r_IN_TRACEID and ias.fmlocation = IN_FMLOCATION and ias.taskstatus < '81';
      if r_nrow > 0 then
        rollback;
           --去异常口或退出
             r_err_out_mesg:='engine(0): 已有任务未完成，不可重复下发,重推至WCS';
             --r_direction:='ERR-OUT';
             --goto ERR_OUT;
           --不用再下发
           update idx_asrs_send ias set ias.edisendflag = r_SEND_FLAG , ias.notes = r_err_out_mesg where ias.organizationid = IN_organizationId
           and ias.warehouseid = IN_Warehouse and ias.palletid = r_IN_TRACEID and  ias.fmlocation = IN_FMLOCATION and ias.taskstatus < '81';
           commit;
           return;
      end if;
      --查询是否有关联托盘任务
      select count(0),max(tk.plantolocation) into r_linktask,r_tolocation from tsk_tasklists tk where tk.organizationid = IN_organizationId
      and tk.warehouseid = IN_Warehouse and tk.taskprocess = '00' and tk.fmid = r_IN_TRACEID and tk.addtime >= sysdate - r_dateint and rownum = 1
      and sirio_get_udfwcs(tk.organizationid,tk.warehouseid,tk.tasktype,tk.docno,tk.doclineno) = 'Y'
      and tk.tasktype not in ('CC') and nvl(tk.grouptaskid_wcs,'*') = '*' ;
      if r_linktask > 0 then
        goto end_0;
      end if;

      --查询是否有库存（空托垛要先组盘）
       select count(0) into r_nrow from inv_lot_loc_id iv where iv.organizationid = IN_organizationId
       and iv.warehouseid = IN_Warehouse and iv.traceid = r_IN_TRACEID and iv.qty > 0 ;
        if r_nrow = 0 then
           OUT_Return_Code:='999#engine(0): 托盘没有库存';
           rollback;
           --去异常口或退出
             r_err_out_mesg:='engine(0): 托盘没有库存';
             r_direction:='ERR-OUT';
             goto ERR_OUT;
        end if;

         --调用生成移库或上架任务
        OUT_Return_Code:='*_*';

       if nvl(IN_TOLOCATION,'*') = '*' then
       r_direction:='';
         --计算上架库位
         SIRIO_SPCUS_Calc_loc
         (IN_organizationId  ,
         IN_Warehouse      ,
         IN_FMLOCATION       ,
         r_IN_TRACEID          , --托盘号
         ''              , --SKU
         r_putloction        ,
         r_model             , --LOC 货位，LOG 货架
         'DATAHUB'           ,
         IN_Language        ,
         OUT_Return_Code
         );
         if  substr(OUT_Return_Code,1,3) <> '000' then
              rollback;
              --退出
              --return;
                r_err_out_mesg:='engine(0): 错误信息：'||OUT_Return_Code;
                r_direction:='ERR-OUT';
                goto ERR_OUT;
         end if;
         r_tolocation:=r_putloction;
       else
        --如果有货位传入，就不再计算目位库位
        r_tolocation := IN_TOLOCATION;
       end if;

       if  nvl(r_tolocation,'*') = '*' then
            r_err_out_mesg:='engine(0): 没有获取到目标货位，请检查！';
            r_direction:='ERR-OUT';
            goto ERR_OUT;
       end if;

       --检查货位是否合法
       select count(0) into r_nrow from bas_location l where l.organizationid = IN_organizationId and l.warehouseid = IN_Warehouse
       and (l.locationid = r_tolocation or l.udf01 = r_tolocation) and rownum =1 ;
       if r_nrow = 0 then
            r_err_out_mesg:='engine(0): 计算目标货位:'||r_tolocation||'不合法，请检查！';
            r_direction:='ERR-OUT';
            goto ERR_OUT;
       end if;

       if r_model = 'LOC1' then
         OUT_Return_Code := '*_*';
         SIRIO_SPCUS_getMOVETASK
         (IN_organizationId,
          IN_Warehouse,
          r_IN_TRACEID, --托盘号
          r_tolocation, --目标库位，留空时调用目标库位计算逻辑
          '', --移动类型，区分是否需要生成移动单
          '', --预留
          '', --预留
          'Y', --预留
          IN_UserID,
          IN_Language,
          OUT_Return_Code
          );
         if substr(OUT_Return_Code,1,3) <> '000' then
           OUT_Return_Code:='999#engine(0): 生成上架/移动任务失败:'||OUT_Return_Code;
           rollback;
             --异常排除
                 r_err_out_mesg:='engine(0): 生成上架/移动任务失败:'||OUT_Return_Code;
                 r_direction:='ERR-OUT';
                 goto ERR_OUT;
          end if;

            --查目标库区是否为立库
           begin
             select 1,bl.zoneid,bzt.udf05,bl.locgroup2,bl.locationcategory,tk.plantolocation,tk.tasktype,tk.udf01,tk.docno
             into r_nrow,r_tozone,r_tolocation_WCS,r_tolocgrp2,r_toloctype,r_tolocation,r_tasktype,r_isPack,r_docno
             from tsk_tasklists tk
             inner join bas_location bl on tk.plantolocation = bl.locationid and tk.organizationid = bl.organizationid and tk.warehouseid = bl.warehouseid
             inner join bas_zone bzt on tk.organizationid = bzt.organizationid and tk.warehouseid = bzt.warehouseid and bl.zoneid = bzt.zoneid
             where tk.organizationid = IN_organizationId and tk.warehouseid = IN_Warehouse
              and tk.fmid = r_IN_TRACEID and tk.taskprocess = '00' and tk.addtime >= sysdate -r_dateint
              and bzt.udf05 in ('WCS2') and rownum =1 ; --上架托盘能到达货位
           exception
           when no_data_found then
             r_nrow := 0;
           end;
           if r_nrow > 0 then
             --先下发一个实体任务，任务tsk_tasklists 下发时会反写当前 GROUPTASKID 到任务表，目的实现从当前点位置进行移动，因为收货可能是在STAGE货位，不具备设备移动条件
             spcom_getidsequence(IN_organizationId,IN_Warehouse,IN_Language,'GROUPTASKID',r_GROUPTASKID,out_return_code);
             if substr(OUT_Return_Code,1,3) <> '000' then
               rollback;
               return;
              end if;
             insert into idx_asrs_send (organizationid,warehouseid,grouptaskid,tasktype,palletid,fmlocation,tolocation,tolocation_WCS,
                 taskstatus,priority,sendtime_appointment,addtime,addwho,docno,equiptype,businesstype,ispackage,waveno,emptyflag,palletdetails,tolocation_type
                 ,fmzone,fmlocgrp2,fmloctype_wcs,fmloctype,fmlocmode,tozone,tolocgrp2,toloctype_wcs,toloctype,tolocmode,notes,udf05,edisendflag)
             values (IN_organizationId,IN_Warehouse,r_GROUPTASKID,r_tasktype,r_IN_TRACEID,IN_FMLOCATION,r_tolocation,r_tolocation,
                '00',10,r_sendtime_appointment,sysdate,IN_UserID,r_docno,'LT','',r_isPack,r_waveno,'N','',''
                ,'','','','','','','','WCS2','','','','in_type:0',r_SEND_FLAG);
             ---
              --写入日志
          insert into idx_asrs_send_log (organizationid,warehouseid,grouptaskid,palletid,status,addtime,addwho,notes)
                             values     (IN_organizationId,IN_Warehouse,r_GROUPTASKID,r_IN_TRACEID,'00',sysdate,IN_UserID,'');
           --完成之前的拣货任务
           update idx_asrs_send a set a.taskstatus = '85' where a.organizationid = IN_organizationId and a.warehouseid = IN_Warehouse
            and a.tolocation = IN_FMLOCATION and a.palletid = r_IN_TRACEID and a.taskstatus = '81' and a.tasktype in  ('V_PK','PK')
            and a.grouptaskid <>  r_GROUPTASKID ;
             commit;
              r_in_taskflag  :='Y';  --进入实际任务下发过程
              goto END_0;
           else
                OUT_Return_Code:='999#engine(0): 设备不可达货位发，不能下发，请检查';
                rollback;
                --return;
                 r_err_out_mesg:='engine(0): 设备不可达货位发，不能下发，请检查';
                r_direction:='ERR-OUT';
                goto ERR_OUT;
           end if;

        end if;





       if r_model = 'LOC2' then  --LOC2 为平库
         --如果去取样缓存生成上架或移动任务
         --从平库中查是否去取样缓存
         select count(0) into r_nrow from bas_location bl where bl.organizationid = IN_organizationId
          and bl.warehouseid = IN_Warehouse and bl.locationid = r_tolocation and bl.zoneid = '3FBUFFER';
         if r_nrow > 0 then
           OUT_Return_Code := '*_*';
           SIRIO_SPCUS_getMOVETASK
            (IN_organizationId,
             IN_Warehouse,
             r_IN_TRACEID, --托盘号
             '30310000', --目标库位，取样区采用大库位管理
             '', --移动类型，区分是否需要生成移动单
             '', --预留
             '', --预留
             'Y', --预留
             IN_UserID,
             IN_Language,
             OUT_Return_Code
            );
           if substr(OUT_Return_Code,1,3) <> '000' then
              OUT_Return_Code:='999#engine(0): 生成上架/移动任务失败:'||OUT_Return_Code;
               rollback;
             --异常排除
                 r_err_out_mesg:='engine(0): 生成上架/移动任务失败:'||OUT_Return_Code;
                 r_direction:='ERR-OUT';
                 goto ERR_OUT;
           end if;
            --查目标库区是否为平库
           begin
             select 1,tk.tasktype,tk.udf01,tk.docno
             into r_nrow,r_tasktype,r_isPack,r_docno
             from tsk_tasklists tk
             inner join bas_location bl on tk.plantolocation = bl.locationid and tk.organizationid = bl.organizationid and tk.warehouseid = bl.warehouseid
             inner join bas_zone bzt on tk.organizationid = bzt.organizationid and tk.warehouseid = bzt.warehouseid and bl.zoneid = bzt.zoneid
             where tk.organizationid = IN_organizationId and tk.warehouseid = IN_Warehouse
              and tk.fmid = r_IN_TRACEID and tk.plantolocation = r_tolocation and tk.taskprocess = '00' and tk.addtime >= sysdate -r_dateint
              and bzt.udf05 in ('WCS1') and rownum =1 ; --上架托盘能到达货位
           exception
           when no_data_found then
             r_nrow := 0;
           end;
           if r_nrow > 0 then
             r_toloctype:='BUF_P'; --平库缓存
             r_tozone:='3FBUFFER';
             r_tasktype_SEND:=r_tasktype;
             r_tolocmode_SEND:='';
             r_tolocmode:='';
             r_tolocation:='';
             r_tolocation_WCS:='';

           --计算接驳位
             for r in (select l.locationid,nvl(l.plcount,1) as plcount from bas_location l
                 left join bas_zone z on l.zoneid = z.zoneid and l.organizationid = z.organizationid and l.warehouseid = z.warehouseid
                 left join bas_locgroup2 g2 on g2.locgroup2 = l.locgroup2  and  l.organizationid = g2.organizationid and l.warehouseid = g2.warehouseid
                where l.organizationid = IN_organizationId and l.warehouseid = IN_Warehouse
                and l.zoneid = nvl(nvl(r_tozone,l.zoneid),l.zoneid) --区域
                and l.locationcategory = r_toloctype
                and nvl(g2.udf01,'*') = case when nvl(r_tolocmode,'*') = '*' then nvl(g2.udf01,'*') else nvl(r_tolocmode,'*') end
                and l.locationattribute = 'OK'
                ) loop
            --
             --查询接驳位缓存数
             select count(0) into r_nrow from cus_buff_location cb where cb.organizationid = IN_organizationId and cb.warehouseid = IN_Warehouse
             and cb.locationid = r.locationid ;

              if r_nrow = 0 then
               select count(distinct ias.grouptaskid) into r_nrow from idx_asrs_send ias
              left join bas_location bl on ias.tolocation = bl.locationid and ias.organizationid = bl.organizationid and ias.warehouseid = bl.warehouseid
               where ias.organizationid = IN_organizationId and ias.warehouseid = IN_Warehouse
              and ias.tolocation = r.locationid and ias.taskstatus >= '10' and bl.udf03 = 'Y' ;
             end if;

              --再看有没有进行中的任务
             if r_nrow = 0 then
              select count(distinct ias.grouptaskid) into r_nrow from idx_asrs_send ias
              left join bas_location bl on ias.tolocation = bl.locationid and ias.organizationid = bl.organizationid and ias.warehouseid = bl.warehouseid
               where ias.organizationid = IN_organizationId and ias.warehouseid = IN_Warehouse
              and ias.tolocation = r.locationid and ias.taskstatus >= '10' and bl.udf03 = 'Y' ;

             end if;

             if r_nrow < r.plcount then
                insert into tmp_code (codeid,code,udfint) values ('JBW_USE',r.locationid,r.plcount - r_nrow);
             end if;
            end loop;
           begin
           select code into r_tolocation  from (
           select code from tmp_code where codeid = 'JBW_USE'
           order by  udfint desc,code) where rownum = 1 ; --找空余数最大的
           exception
             when no_data_found then
                   rollback;
                   r_err_out_mesg:='engine(0): '||r_tozone||'区，无空闲的接驳位，请等待';
                   r_direction:='ERR-OUT';
                   goto ERR_OUT;
               end;


             --先下发一个实体任务，任务tsk_tasklists 下发时会反写当前 GROUPTASKID 到任务表，目的实现从当前点位置进行移动，因为收货可能是在STAGE货位，不具备设备移动条件
             spcom_getidsequence(IN_organizationId,IN_Warehouse,IN_Language,'GROUPTASKID',r_GROUPTASKID,out_return_code);
             if substr(OUT_Return_Code,1,3) <> '000' then
               rollback;
               return;
              end if;
             insert into idx_asrs_send (organizationid,warehouseid,grouptaskid,tasktype,palletid,fmlocation,tolocation,tolocation_WCS,
                 taskstatus,priority,sendtime_appointment,addtime,addwho,docno,equiptype,businesstype,ispackage,waveno,emptyflag,palletdetails,tolocation_type
                 ,fmzone,fmlocgrp2,fmloctype_wcs,fmloctype,fmlocmode,tozone,tolocgrp2,toloctype_wcs,toloctype,tolocmode,notes,udf05,edisendflag)
             values (IN_organizationId,IN_Warehouse,r_GROUPTASKID,r_tasktype,r_IN_TRACEID,IN_FMLOCATION,r_tolocation,r_tolocation,
                '00',10,r_sendtime_appointment,sysdate,IN_UserID,r_docno,'LT','',r_isPack,r_waveno,'N','',''
                ,'','','','','',r_tozone,'','WCS1',r_toloctype,'','','in_type:0',r_SEND_FLAG);
             ---
              --写入日志
              insert into idx_asrs_send_log (organizationid,warehouseid,grouptaskid,palletid,status,addtime,addwho,notes)
                             values     (IN_organizationId,IN_Warehouse,r_GROUPTASKID,r_IN_TRACEID,'00',sysdate,IN_UserID,'');
             commit;
               r_in_taskflag  :='Y';  --进入实际任务下发过程
               goto end_0;
           else
                OUT_Return_Code:='999#engine(0): 设备不可达货位发，不能下发，请检查';
                rollback;
                --return;
                 r_err_out_mesg:='engine(0): 设备不可达货位发，不能下发，请检查';
                r_direction:='ERR-OUT';
                goto ERR_OUT;
           end if;
          --跳出以下逻辑
          goto end_0;
         end if;

       end if;


        --处理平库接驳位
        begin
         select 1,bl.zoneid,bz.udf05,bl.locgroup2,'JBW' --bl.locationcategory
           into r_nrow,r_tozone,r_tolocation_WCS,r_tolocgrp2,r_toloctype
          from bas_location bl
          inner join bas_zone bz on bl.organizationid = bz.organizationid and bl.warehouseid = bz.warehouseid and bl.zoneid = bz.zoneid
          where bl.organizationid = IN_organizationId and bl.warehouseid = IN_Warehouse
          and (bl.locationid = r_tolocation or bl.udf01 = r_tolocation )
          and bz.udf05 in ('WCS1','KIVA') and rownum = 1;
        exception
           when no_data_found then
             r_nrow := 0;
           end;
        if r_nrow > 0 then
             r_tasktype:='V_MV';
             r_tolocmode:='IN';
             r_toloctype:='JBW';
           --计算接驳位
             for r in (select l.locationid,nvl(l.plcount,1) as plcount from bas_location l
                 left join bas_zone z on l.zoneid = z.zoneid and l.organizationid = z.organizationid and l.warehouseid = z.warehouseid
                 left join bas_locgroup2 g2 on g2.locgroup2 = l.locgroup2  and  l.organizationid = g2.organizationid and l.warehouseid = g2.warehouseid
                where l.organizationid = IN_organizationId and l.warehouseid = IN_Warehouse
                and l.zoneid = nvl(nvl(r_tozone,l.zoneid),l.zoneid) --区域
                and l.locationcategory = r_toloctype
                and nvl(g2.udf01,'*') = case when nvl(r_tolocmode,'*') = '*' then nvl(g2.udf01,'*') else nvl(r_tolocmode,'*') end
                and l.locationattribute = 'OK'
                ) loop
            --
             --查询接驳位缓存数
             select count(0) into r_nrow from cus_buff_location cb where cb.organizationid = IN_organizationId and cb.warehouseid = IN_Warehouse
             and cb.locationid = r.locationid ;
             if r_nrow < r.plcount then
                insert into tmp_code (codeid,code,udfint) values ('JBW_USE',r.locationid,r.plcount - r_nrow);
             end if;
            end loop;
           begin
           select code into r_tolocation  from (
           select code from tmp_code where codeid = 'JBW_USE'
           order by  udfint desc) where rownum = 1 ; --找空余数最大的
           exception
             when no_data_found then
                   r_err_out_mesg:='engine(0): '||r_tozone||'区，无空闲的接驳位，请等待';
                   r_direction:='ERR-OUT';
                   goto ERR_OUT;
               end;

           --从事务中取单号 modify syx 2022-8-31
           begin
           select docno into r_docno from (
            select tl.docno from act_transaction_log tl where tl.organizationid = IN_organizationId and tl.warehouseid = IN_Warehouse
            and tl.toid = r_IN_TRACEID  and tl.transactiontype = 'IN' and tl.status = '99' and tl.addtime >= sysdate -30
           order by tl.addtime desc ) where rownum = 1;
           exception
             when no_data_found then
               r_docno:='';
             end;

             --写入虚拟移动任务
           spcom_getidsequence(IN_organizationId,IN_Warehouse,IN_Language,'GROUPTASKID',r_GROUPTASKID,out_return_code);
           if substr(OUT_Return_Code,1,3) <> '000' then
               return;
           end if;
          insert into idx_asrs_send (organizationid,warehouseid,grouptaskid,tasktype,palletid,fmlocation,tolocation,tolocation_WCS,
                 taskstatus,priority,sendtime_appointment,addtime,addwho,docno,equiptype,businesstype,ispackage,waveno,emptyflag,palletdetails,tolocation_type
                 ,fmzone,fmlocgrp2,fmloctype_wcs,fmloctype,fmlocmode,tozone,tolocgrp2,toloctype_wcs,toloctype,tolocmode,udf05,edisendflag)
          values (IN_organizationId,IN_Warehouse,r_GROUPTASKID,r_tasktype,r_IN_TRACEID,IN_FMLOCATION,r_tolocation,r_tolocation,
                '00',10,null,sysdate,IN_UserID,r_docno,'LT','',r_isPack,r_waveno,'N','',''
                ,r_fmzone,r_fmlocgrp2,r_fmloctype_wcs,r_fmloctype,r_fmlocmode,r_tozone,r_tolocgrp2,r_toloctype_wcs,r_toloctype,r_tolocmode,'in_type:0',r_SEND_FLAG);
          --写入日志
          insert into idx_asrs_send_log (organizationid,warehouseid,grouptaskid,palletid,status,addtime,addwho,notes)
                             values     (IN_organizationId,IN_Warehouse,r_GROUPTASKID,r_IN_TRACEID,'10',sysdate,IN_UserID,'');

         --再下发关联货架任务,改为到达接驳位再下发 2022-09-29
         /* if r_model = 'LOG' then
              r_tolocation:=r_putloction;
              select count(0) into r_nrow from idx_asrs_send ias where ias.organizationid = IN_organizationId and ias.warehouseid = IN_Warehouse
              and ias.palletid = r_tolocation and ias.taskstatus in ('-1','00','05');
              if r_nrow = 0 then
                  spcom_getidsequence(IN_organizationId,IN_Warehouse,IN_Language,'GROUPTASKID',r_GROUPTASKID,out_return_code);
                if substr(OUT_Return_Code,1,3) <> '000' then
                  rollback;
                end if;
                r_toloctype:='ZZW';
                r_tasktype:='V_MV';
                r_tolocmode:='IN';
                r_tolocgrp2:='';
                begin
                 select iv.locationid into r_fmlocation from inv_lot_loc_id iv where iv.organizationid = IN_organizationId and iv.warehouseid = IN_Warehouse
                  and iv.traceid = r_tolocation and iv.qty > 0 and iv.sku = 'KIVASHELF' and rownum = 1 ;
                 exception
                when no_data_found then
                   r_err_out_mesg:='engine(0): 货架库存不存在';
                   r_direction:='ERR-OUT';
                   goto ERR_OUT;
                end;
                  insert into idx_asrs_send (organizationid,warehouseid,grouptaskid,tasktype,palletid,fmlocation,tolocation,tolocation_WCS,
                 taskstatus,priority,sendtime_appointment,addtime,addwho,docno,equiptype,businesstype,ispackage,waveno,emptyflag,palletdetails,tolocation_type
                  ,fmzone,fmlocgrp2,fmloctype_wcs,fmloctype,fmlocmode,tozone,tolocgrp2,toloctype_wcs,toloctype,tolocmode,fmaisleno,toaisleno,linkpalletid,edisendflag,udf05)
                 values (IN_organizationId,IN_Warehouse,r_GROUPTASKID,r_tasktype,r_tolocation,r_fmlocation,'','',
                  '-1',10,r_sendtime_appointment,sysdate,( case when IN_Language = 'DEBUG' then 'DEBUG' else IN_UserID end),r_docno,'LT','',r_isPack,r_waveno,'N','',''
                 ,'','','','','',r_tozone,r_tolocgrp2,r_toloctype_wcs,'ZZW',r_tolocmode,r_fmaisleno,r_toaisleno,r_IN_TRACEID,r_SEND_FLAG,'in_type:0');
                                --写入日志
                   insert into idx_asrs_send_log (organizationid,warehouseid,grouptaskid,palletid,status,addtime,addwho,notes)
                             values     (IN_organizationId,IN_Warehouse,r_GROUPTASKID,r_palletid,'-1',sysdate,IN_UserID,'');
              end if;
          end if;*/
          end if;

   <<END_0>>
      r_in_taskflag  :='Y';
 end if;
--***************************************************** in_type  = 1 空托盘补给 ***********************************************************************
if IN_TYPE = '1' then
  if IN_TOLOCATION is null then
       OUT_Return_Code:='999#engine(1): 目标位不能为空';
       rollback;
       return;
  end if;
  --校验货位是否合法
  select count(0) into r_nrow from bas_location l where l.organizationid = IN_organizationId and l.warehouseid = IN_Warehouse
  and l.locationid = IN_TOLOCATION;
  if r_nrow = 0 then
     OUT_Return_Code:='999#engine(1): 目标位不合法';
       rollback;
       return;
  end if;
   --查找可用库存货位
   --还要有补什么类型托盘的计算，待定
   ---若物料号为非空，判断为是纤维托盘
   r_parainfo:='Y';
   if nvl(IN_SKU,'*') <> '*' then
     r_XWFLAG := 'Y';
     r_xwsku  := IN_SKU;
     r_parainfo:='Y,'||r_xwsku; --纤维托盘标记
   end if;
   ---
   r_sku:= nvl(IN_SKU,'PALLET01'); --默认为塑料托盘
   if r_sku = 'PALLET01' then
     R_EMPTYFLAG := 'Y';
    end if;
  --记录原始请求码
  r_in_reqid:=nvl(r_IN_TRACEID,'*');
   ---------
  r_rpl_qty:=nvl(IN_qty,1);
  while r_rpl_qty > 0 loop
   begin
   select traceid,kcqty into r_palletid,r_kc_qty from (
   select lotatt05,locationid,traceid,kcqty from (
   select ia.lotatt05,bl.aisleno,bl.locationid,iv.traceid,count(0) as pallets,sum(iv.qty) as kcqty from bas_location bl
   inner join inv_lot_loc_id iv on bl.organizationid = iv.organizationid and bl.warehouseid = iv.warehouseid and bl.locationid = iv.locationid
   inner join bas_zone bz on bl.zoneid = bz.zoneid and bl.organizationid = bz.organizationid and bz.warehouseid = bl.warehouseid
   inner join inv_lot_att ia on iv.lotnum = ia.lotnum and iv.sku = ia.sku and iv.organizationid = ia.organizationid and iv.customerid = ia.customerid
   where bz.udf05  = 'WCS2'  and bl.locationattribute = 'OK'
   and bl.zoneid in ('01','02','03','02A') ---by zf
   and iv.sku = r_sku and iv.qty > 0 and (iv.qtyrpout-iv.qtyallocated-iv.qtymvout-iv.qtyonhold) = 0
   and nvl(ia.lotatt14,'*') =  (case when r_sku <> 'PALLET01' then '8299'  else nvl(ia.lotatt14,'*')  end) --纤维托盘看我补8299库存地的库存
   group by ia.lotatt05,bl.aisleno,bl.locationid,iv.traceid
   ) order by lotatt05,kcqty,pallets desc -- SAP批次优先,库存托数排序,项道库存最多的先出
   ) where rownum = 1 ;
   exception
     when no_data_found then
        --OUT_Return_Code:='999# SPCUS_TASKGROUP_engine(1): 空托盘没有可用库存';
       -- rollback;
       -- return;
       exit;
     end;
   r_IN_TRACEID:=r_palletid;
   --判断目标去向是否为出口
   select count(0) into r_nrow from bas_location l where l.organizationid = IN_organizationId and l.warehouseid = IN_Warehouse
   and l.locationid = IN_TOLOCATION and l.locationcategory = 'OUT' ;
   if r_nrow > 0 then
      r_tolocation:=IN_TOLOCATION;
      r_tolocation_WCS:='';
     else
      r_tolocation:='PALLET01';
      r_tolocation_WCS:=IN_TOLOCATION;
   end if;
  --调用通用移库任务

  --生成移动任务
         SIRIO_SPCUS_getMOVETASK
         (IN_organizationId,
          IN_Warehouse,
          r_IN_TRACEID, --托盘号
          r_tolocation, --目标库位，留空时调用目标库位计算逻辑
          '', --移动类型，区分是否需要生成移动单
          r_tolocation_WCS, --预留
          'V_MV1', --预留
          r_parainfo, --预留
          IN_UserID,
          IN_Language,
          OUT_Return_Code
          );
   if substr(OUT_Return_Code,1,3) <> '000' then
           --OUT_Return_Code:='999# SPCUS_TASKGROUP_engine(1): 生成上架/移动任务失败:'||OUT_Return_Code;
           --rollback;
           --return;
           exit;
   end if;
   r_rpl_qty:=r_rpl_qty - nvl(r_kc_qty,1);
   --删除记录的待补指令
    delete from cus_asrs_autoexec aa where aa.organizationid = IN_organizationId and aa.warehouseid = IN_Warehouse
          and aa.tolocation = IN_TOLOCATION and aa.sku = r_sku and aa.intype = IN_TYPE and aa.traceid = r_in_reqid ;

      ---补托盘成功，更新库位表中该托盘的申请码，防止申请码在连续申请2次---从interface里面将逻辑搬入--
    ---20221007 by zf begin--
    select count(0) into r_nrow from bas_location l where l.locationid = IN_TOLOCATION
     and l.organizationid = IN_organizationId and l.warehouseid = IN_Warehouse and nvl(l.udf05,'*') = r_in_reqid;
     if r_nrow = 0 then
           update bas_location l set l.udf05 = IN_TRACEID,l.editwho = IN_UserID,l.edittime = sysdate
           where l.locationid = IN_TOLOCATION  and l.organizationid = IN_organizationId and l.warehouseid = IN_Warehouse;
     end if;
    ---end-----

  end loop;

    --WCS请求如果库存不足，WMS先存下发，等有库存再补
    if  r_sku = 'PALLET01' and   r_rpl_qty > 0  then
       select count(0) into r_nrow from bas_location l where l.organizationid = IN_organizationId and l.warehouseid = IN_Warehouse
       and l.locationid = IN_TOLOCATION and l.locationcategory = 'APS';
       if r_nrow > 0 then
         select count(0) into r_nrow from cus_asrs_autoexec aa where aa.organizationid = IN_organizationId and aa.warehouseid = IN_Warehouse
          and aa.tolocation = IN_TOLOCATION and aa.sku = r_sku and aa.intype = IN_TYPE and aa.traceid = r_in_reqid ;
          if r_nrow = 0 then
             insert into cus_asrs_autoexec (organizationid,warehouseid,fmlocation,traceid,tolocation,sku,qty,intype,notes,addtime,addwho)
             values (IN_organizationId,IN_Warehouse,'',r_in_reqid,IN_TOLOCATION,r_sku,r_rpl_qty,IN_TYPE,'无空托垛库存,等待有库存再补',sysdate,IN_UserID);
          else
            update  cus_asrs_autoexec aa set aa.edittime = sysdate,aa.editwho = IN_UserID where aa.organizationid = IN_organizationId and aa.warehouseid = IN_Warehouse
             and aa.tolocation = IN_TOLOCATION and aa.sku = r_sku and aa.intype = IN_TYPE and aa.traceid = r_in_reqid ;
          end if;
       end if;
    end if;
   r_IN_TRACEID:='';
   r_in_reqid:='';
   r_in_taskflag :='Y'; --进入任务下发
  commit;
end if;
--***************************************************** in_type  = 2 自由移动为虚拟任务（from - to ）**************************************************
if IN_TYPE = '2' then
    if nvl(IN_FMLOCATION,'*') = '*' then
     OUT_Return_Code:='999#engine(2): 原库位不能为空';
     rollback;
      return;
  end if;
  if nvl(r_IN_TRACEID,'*') = '*' then
     OUT_Return_Code:='999#engine(2): 托盘号/货架号不能为空';
     rollback;
      return;
  end if;
   select  count(*),max(l.locationcategory) into r_nrow ,r_fmloctype from bas_location l where l.organizationid = IN_organizationId and l.warehouseid = IN_Warehouse
   and (l.locationid = IN_FMLOCATION or l.locgroup2 = IN_FMLOCATION) and rownum =1 ;
  if r_nrow = 0 then
       OUT_Return_Code:='999#engine(2): 原货位:'||IN_FMLOCATION||'不合法，请检查';
           rollback;
           return;
  end if;

  --ZZW不允许随意移动
  if r_fmloctype in ('ZZW') then
     if nvl(IN_TOLOCATION,'*') <> '*' then
           OUT_Return_Code:='999#engine(2): ZZW类型不能自由移动，请检查';
           rollback;
           return;
     end if;
  end if;

  --检原库位是否有托盘到位
  if r_fmloctype not in ('JBW_TL','TL','PP','OUT_C','OUT_X') then --退料，配盘，成品下线 不验证占位
   select count(0) into r_nrow from idx_asrs_send a where a.organizationid = IN_organizationId and a.warehouseid = IN_Warehouse
     and a.tolocation = IN_FMLOCATION and a.palletid = r_IN_TRACEID and a.taskstatus = '81' ;--and a.tasktype in  ('V_PK','PK') ;
    if r_nrow = 0 then
        OUT_Return_Code:='999#engine(2): 识别到任务没有到达对应位置，请检查';
        rollback;
        return;
    end if;
   end if;

  --退料/及需要跟据上架规则处理的源
  r_tolocation:=IN_TOLOCATION;
  --指定位置的自由移动
  select  count(*),max(l.locationcategory) into r_nrow,r_toloctype from bas_location l where l.organizationid = IN_organizationId and l.warehouseid = IN_Warehouse
  and (l.locationid = r_tolocation or l.locgroup2 = r_tolocation);
  if r_nrow = 0 then
  --返拣回库情况支持，从PP/BUF 回库的没有目标库位的需要计算目标货位
  if nvl(r_tolocation,'*') <> 'GET_LOC' then
   if r_fmloctype in ('PP','BUF') then
    select count(0) into r_nrow from inv_lot_loc_id iv where iv.organizationid = IN_organizationId
     and iv.warehouseid = IN_Warehouse and iv.traceid = r_IN_TRACEID and iv.locationid like 'REVERSEPICK%' ;
    if r_nrow > 0 then
      r_tolocation:='GET_LOC';
    end if;

   end if;
  end if;

   select  count(*) into r_nrow from bas_location l where l.organizationid = IN_organizationId and l.warehouseid = IN_Warehouse
    and l.zoneid = '22' and l.locationid = IN_FMLOCATION ; --(22区) 退料区回库自动计算去向
  if (r_nrow > 0) or nvl(r_tolocation,'*') = 'GET_LOC'  then
     --
     begin
       select iv.sku into r_sku  from inv_lot_loc_id iv where iv.traceid = r_IN_TRACEID and iv.organizationid = IN_organizationId and iv.warehouseid = IN_Warehouse
       and iv.qty > 0 and rownum = 1;
     exception
       when no_data_found then
           OUT_Return_Code:='999#engine(2): 托盘没有库存，请检查';
           rollback;
           return;
       end;
       
       
         ---判断托盘对应的订单是否为越库订单--by zf 20230501 begin---
     if r_receivelocation = 'STAGE_WH03' then
      begin
       select crossdockflag1
       into r_crossdockflag
       from (select h.crossdockflag1
       from act_transaction_log l
       left join doc_asn_header h on h.organizationid = l.organizationid and h.warehouseid = l.warehouseid and h.customerid = l.fmcustomerid and l.docno = h.asnno
       where l.organizationid = IN_organizationId
       and l.warehouseid = IN_Warehouse
       and l.fmcustomerid = 'SIRIO'
       and l.transactiontype = 'IN' 
       and l.status = '99'
       and l.TOid = r_IN_TRACEID 
       order by l.edittime desc)
       where rownum = 1;
       exception
         when no_data_found then
           r_crossdockflag := 'N';
       end;
       end if;
       
      ---判断托盘对应的订单是否为越库订单--by zf 20230501 end---
      ---判断托盘是否越库区，是则直接默认越库区库位，不管是否立库---begin--20230529--byzf
      if r_crossdockflag = 'Y' then
        r_tolocation := '3010O02';
         r_toloctype:='IN';
         r_tozone:='2FINSTEREO';
         r_model:='LOC1';
        else
     --通过目标库位来确定去向
        if sirio_Get_sku_area(IN_OrganizationID,IN_Warehouse,'','',r_IN_TRACEID) = 1   then -- 返回 1为立库，其它为平库
            select  count(*) into r_nrow from bas_location l where l.organizationid = IN_organizationId and l.warehouseid = IN_Warehouse
             and l.zoneid = '3FBUFFER' and l.locationid = IN_FMLOCATION ;
            if r_nrow > 0 then
               r_toloctype:='IN';
               r_tolocation:='3FINPORT';
               r_tozone:='3FINSTEREO';
               r_model:='LOC1';
            else
           -- r_tozone:='';--2楼入口品区域
               r_toloctype:='IN';
               r_tolocation:='3020I01';
               r_tozone:='2FINSTEREO';
               r_model:='LOC1';
            end if;


         select count(0),max(locationid),max(udf02),max(locgroup2),max(zoneid),max(locationcategory)
            into r_nrow,r_tolocation,r_isgroupid,r_tolocation_WCS,r_tozone,r_toloctype
           from (
             select l.locationid,lg.udf02,l.locgroup2,l.zoneid,l.locationcategory
               from bas_location l
               left join bas_locgroup2 lg on l.organizationid = lg.organizationid and l.warehouseid = lg.warehouseid
                and l.locgroup2 = lg.locgroup2
               where (l.locgroup2 = r_tolocation or l.locationid = r_tolocation ) and l.organizationid = IN_organizationId
               and l.warehouseid = IN_Warehouse and l.locationattribute = 'OK'
              order by  decode(l.locationcategory,'JBW',0,1),l.locationid) --接驳位排前
              where rownum = 1;

                if r_nrow = 0 then
                   OUT_Return_Code:='999#engine(2): 目标货位不合法(或非正常状态)';
                    rollback;
                     return;
                end if;

                 --如果传入的是组号，测由send计算目标位货
             if IN_TOLOCATION = r_tolocation_WCS then
                r_tolocgrp2 := r_tolocation_WCS;
                r_tolocation:='';
                r_tolocation_WCS:='';
             end if;

             if nvl(r_isgroupid,'N') = 'N' and  nvl(r_tolocation_WCS,'*') <>  nvl(r_tolocation,'*') then
              r_tolocation_WCS := r_tolocation;
             end if;


         else
           --根据平库规则查找上架库位
           for r in (select iv.organizationid,iv.warehouseid,iv.customerid,iv.sku,iv.locationid,iv.qty,
                           s.packid,s.defaultreceivinguom,'STAGE_'||iv.warehouseid as rece_location,iv.lotnum from inv_lot_loc_id iv
                     inner join inv_lot_att ia on iv.organizationid = iv.organizationid and ia.lotnum = iv.lotnum and iv.sku = ia.sku and iv.customerid = ia.customerid
                     inner join bas_sku s on iv.sku = s.sku and iv.organizationid = s.organizationid and s.customerid =  iv.customerid
                      where iv.organizationid = IN_organizationId and iv.warehouseid = IN_Warehouse and iv.traceid = r_IN_TRACEID
                      and rownum = 1 and iv.qty > 0  ) loop
                 r_tolocation :=  FNCOM_PUTAWAY_CALCULATION
                     (r.organizationid ,
                      r.warehouseid    ,
                      '*'        ,--入库计算传具体单号，其他传*
                      '*'     ,--入库计算传具体行号，其他传*
                      r.customerid    ,
                      r.sku            ,--包装ID
                      r.packid         ,--包装ID
                      r.defaultreceivinguom       ,--包装单位
                      /*r.rece_location*/''     ,--收货库位
                      r.qty       ,--上架数量
                      0         ,--产品总毛重
                      0          ,--产品总体积
                      r.lotnum         ,--批次
                      'DATAHUB'         ,
                      IN_LANGUAGE
                     );
              end loop;
              if nvl(r_tolocation,'*') in ('*','NO-LOC')  then
                 --异常排除
                 OUT_Return_Code:='999#engine(2): 没有获取到目标货位(非立库)';
                 return;
              end if;
           --找目标位区应的库区
           select count(0),max(l.zoneid),max(lg.udf02) into r_nrow,r_tozone,r_isgroupid from bas_location l
           left join bas_locgroup2 lg on l.organizationid = lg.organizationid and l.warehouseid = lg.warehouseid and l.locgroup2 = lg.locgroup2
           where l.organizationid = IN_organizationId and l.warehouseid = IN_Warehouse
           and l.locationid = r_tolocation --and l.zoneid in ('21','25')
           and rownum = 1  ;
           if r_nrow > 0 and r_tozone in ('21','42') then  --KIVA区
             --去二楼接驳位
             r_toloctype:='JBW';
             r_tolocmode:='IN';
             --r_tolocation:='';
             r_model:='LOG';
           else
             --r_tolocation:='3020I04';
             --r_tolocation_WCS:='3020I04';
             r_toloctype:='JBW';
             r_tolocmode:='IN';
             r_model:='LOC2';
           end if;
         end if;
         
        end if; 
      ---判断托盘是否越库区，是则直接默认越库区库位，不管是否立库---end--20230529--byzf
  else
    if nvl(r_tolocation,'*') = '*'   then
     OUT_Return_Code:='999#engine(2): 目标库位不能为空';
     rollback;
      return;
    end if;
  end if;

  else

   --货位去向校验
  if r_fmloctype = 'JBW' and r_toloctype not in ('JBW','IN','IN_E','BUF','TL','PP','SAM','IN_X') then
    ---20220826--by zf --begin-----
    select count(1)
    into r_int
    from bsm_code bc
    where bc.organizationid = IN_organizationId
    and bc.codetype = 'CUS_LOC_ALLOWMV'
    and bc.codeid = IN_TOLOCATION
    and bc.activeflag = 'Y';
    ---20220826--by zf --end-----

     if r_int = 0 then
        OUT_Return_Code:='999#(JBW)只能移动至（JBW/IN/IN_E/BUF/TL/PP/SAM/IN_X）类型货位';
        return;
     end if;
  end if;
    if r_fmloctype = 'PK2' and r_toloctype not in ('JBW','IN','IN_E','BUF','PP') then
        OUT_Return_Code:='999#(PK2)只能移动至（JBW/IN/IN_E/BUF/PP）类型货位';
        return;
    end if;
    if r_fmloctype = 'PK' and nvl(IN_TOLOCATION,'*') <> '*' then
        OUT_Return_Code:='999#(原始PK)不可指定目标货位';
        return;
    end if;
    if r_fmloctype = 'ZZW' and nvl(IN_TOLOCATION,'*') <> '*' then
        OUT_Return_Code:='999#(原始ZZW)不可指定目标货位';
        return;
    end if;
    if r_fmloctype = 'TL' and r_toloctype not in ('JBW','IN','IN_E','BUF','TL','IN_X') then
        OUT_Return_Code:='999#(TL)只能移动至（JBW/IN/IN_E/TL/IN_X）类型货位';
        return;
    end if;
    if r_fmloctype = 'PP' and r_toloctype not in ('JBW','IN','IN_E','BUF','PP') then
        OUT_Return_Code:='999#(PP)只能移动至（JBW/IN/IN_E/BUF/PP）类型货位';
        return;
    end if;
    if r_fmloctype = 'BUF' and r_toloctype not in ('JBW','IN','IN_E','BUF','PP','OUT') then
        OUT_Return_Code:='999#(BUF)只能移动至（JBW/IN/IN_E/BUF/PP/OUT）类型货位';
        return;
    end if;


  end if;
  ---校验逻辑
  if IN_FMLOCATION = IN_TOLOCATION then
    OUT_Return_Code:='999#engine(2): 源库位与目标库位不能相同';
     rollback;
      return;
  end if;
  --查询是否已下发任务
  select count(0) into r_nrow from idx_asrs_send a where a.organizationid = IN_organizationId and a.warehouseid = IN_Warehouse
  and a.fmlocation = IN_FMLOCATION and a.palletid = r_IN_TRACEID  and a.taskstatus < '81' ;
  if r_nrow > 0 then
     OUT_Return_Code:='999#engine(2): 已有任务在运行，不可重复下发';
     rollback;
      return;
  end if;

  if nvl(r_model,'*') = '*' then --如果不是自动计算货位
    /*select count(0),max(l.locationid),max(lg.udf02),max(l.locgroup2),max(l.zoneid),max(l.locationcategory)
     into r_nrow,r_tolocation,r_isgroupid,r_tolocation_WCS,r_tozone,r_toloctype
    from bas_location l
      left join bas_locgroup2 lg on l.organizationid = lg.organizationid and l.warehouseid = lg.warehouseid
      and l.locgroup2 = lg.locgroup2
     where (l.locgroup2 = r_tolocation or l.locationid = r_tolocation ) and l.organizationid = IN_organizationId
    and l.warehouseid = IN_Warehouse  and rownum = 1 ;*/
     select count(0),max(locationid),max(udf02),max(locgroup2),max(zoneid),max(locationcategory)
      into r_nrow,r_tolocation,r_isgroupid,r_tolocation_WCS,r_tozone,r_toloctype
       from (
             select l.locationid,lg.udf02,l.locgroup2,l.zoneid,l.locationcategory
               from bas_location l
               left join bas_locgroup2 lg on l.organizationid = lg.organizationid and l.warehouseid = lg.warehouseid
                and l.locgroup2 = lg.locgroup2
               where (l.locgroup2 = r_tolocation or l.locationid = r_tolocation ) and l.organizationid = IN_organizationId
               and l.warehouseid = IN_Warehouse and l.locationattribute = 'OK'
              order by  decode(l.locationcategory,'JBW',0,1),l.locationid) --接驳位排前
              where rownum = 1;

    if r_nrow = 0 then
       OUT_Return_Code:='999#engine(2): 目标货位不合法(或非正常状态)';
      rollback;
      return;
    end if;

    --如果传入的是组号，测由send计算目标位货
    if IN_TOLOCATION = r_tolocation_WCS then
       r_tolocgrp2 := r_tolocation_WCS;
       r_tolocation:='';
       r_tolocation_WCS:='';
    end if;

    if nvl(r_isgroupid,'N') = 'N' and  nvl(r_tolocation_WCS,'*') <>  nvl(r_tolocation,'*') then
       r_tolocation_WCS := r_tolocation;
    end if;

   end if;



    if nvl(r_model,'*') in ('LOC2','LOG') then --由send计算
      r_tolocation_1 := r_tolocation;--by zf
      r_tolocation:='';
      r_tolocation_WCS:='';
    end if;

       --成品下线如目标区无空闲接驳位先去缓存等待
       if r_model in ('LOC2','LOG') then
         select count(0) into r_nrow from bas_location bl where bl.organizationid = IN_organizationId
          and bl.warehouseid = IN_Warehouse and bl.locationid = IN_FMLOCATION and bl.locationcategory = 'OUT_C' ;
         if r_nrow > 0 then
            --查目标库区是否为平库
           begin
             select 1,'V_MV','JBW',bl.zoneid,'',''
             into r_nrow,r_tasktype,r_toloctype,r_tozone,r_tolocation,r_tolocation_WCS
             from  bas_location bl
             inner join bas_zone bzt on bl.organizationid = bzt.organizationid and bl.warehouseid = bzt.warehouseid and bl.zoneid = bzt.zoneid
             where bl.organizationid = IN_organizationId and bl.warehouseid = IN_Warehouse
              and bl.locationid = r_tolocation_1/*r_tolocation--by zf*/  and bzt.udf05 in ('WCS1','KIVA') and rownum =1 ; --上架托盘能到达货位
           exception
           when no_data_found then
             r_nrow := 0;
           end;
           if r_nrow > 0 then

           --计算接驳位
             for r in (select l.locationid,nvl(l.plcount,1) as plcount from bas_location l
                 left join bas_zone z on l.zoneid = z.zoneid and l.organizationid = z.organizationid and l.warehouseid = z.warehouseid
                 left join bas_locgroup2 g2 on g2.locgroup2 = l.locgroup2  and  l.organizationid = g2.organizationid and l.warehouseid = g2.warehouseid
                where l.organizationid = IN_organizationId and l.warehouseid = IN_Warehouse
                and l.zoneid = nvl(nvl(r_tozone,l.zoneid),l.zoneid) --区域
                and l.locationcategory = r_toloctype
                and nvl(g2.udf01,'*') = case when nvl(r_tolocmode,'*') = '*' then nvl(g2.udf01,'*') else nvl(r_tolocmode,'*') end
                and l.locationattribute = 'OK'
                ) loop
            --
             --查询接驳位缓存数
             select count(0) into r_nrow from cus_buff_location cb where cb.organizationid = IN_organizationId and cb.warehouseid = IN_Warehouse
             and cb.locationid = r.locationid ;
             if r_nrow < r.plcount then
                insert into tmp_code (codeid,code,udfint) values ('JBW_USE',r.locationid,r.plcount - r_nrow);
             end if;
            end loop;
           begin
           select code into r_tolocation  from (
           select code from tmp_code where codeid = 'JBW_USE'
           order by  udfint desc,code) where rownum = 1 ; --找空余数最大的
           exception
             when no_data_found then
                   --r_err_out_mesg:='SPCUS_TASKGROUP_engine(0): 无空闲的接驳位，请等待';
                   --r_direction:='ERR-OUT';
                   --goto ERR_OUT;
                   --去缓存区暂存
                   r_end_loctype:=r_toloctype;
                   r_end_zone:=r_tozone;
                   r_end_model:='IN';
                   r_toloctype:='BUF';
                   r_tozone:='XLHC_AREA';
                   r_tolocmode:='';
                   r_tolocation:='';
                   r_tolocation_WCS:='';
              end;


           else
                OUT_Return_Code:='999#engine(2): 设备不可达货位发，不能下发，请检查';
                rollback;
                --return;
                 r_err_out_mesg:='engine(2): 设备不可达货位发，不能下发，请检查';
                r_direction:='ERR-OUT';
                goto ERR_OUT;
           end if;


         end if;

      end if;

    --目标位占用情况检查
    if nvl(r_tolocation,'*') <> '*' then
    select count(0) into r_nrow from idx_asrs_send a where a.organizationid = IN_organizationId and a.warehouseid = IN_Warehouse
     and a.tolocation = r_tolocation and a.taskstatus = '81' and a.tasktype <> 'ERR-OUT'
     and not exists (
       select 1 from bas_location l where a.organizationid = l.organizationid and a.warehouseid = l.warehouseid and a.tolocation = l.locationid
       and l.locationcategory in ('SAM','IN','IN_E','ABNORMAL') --取样间可以重复生成
     )  ;
      if r_nrow > 0 then
        OUT_Return_Code:='999#engine(2): 识别到目标托盘有占位，请检查';
        rollback;
        return;
      end if;

     /* --如果目标库位有库存，限制下发
      select count(0) into r_nrow from inv_lot_loc_id iv where iv.organizationid = IN_organizationId
      and iv.warehouseid = IN_Warehouse and iv.locationid = r_tolocation and iv.qty > 0;
      if r_nrow > 0 then
        OUT_Return_Code:='999# SPCUS_TASKGROUP_engine(2): 识别到目标货位有库存，请检查';
        rollback;
        return;
      end if;
      */
      --如从取样缓存生成测不分段

      select count(0) into r_nrow from bas_location l where l.organizationid = IN_organizationId and l.warehouseid = IN_Warehouse
      and l.locationid = IN_FMLOCATION and l.zoneid = '3FBUFFER' and rownum = 1;
      if r_nrow > 0 then
         r_end_location:='';
         r_tolocation:=r_tolocation;
         r_toloctype:='SAM';
         r_tolocation_WCS:='';
         r_tolocmode:='';
         r_tolocgrp2:='';
         r_tozone:='3FBUFFER';
      else
      --去取样间分段处理
      select count(0),max(l.zoneid) into r_nrow,r_tozone from bas_location l where l.organizationid = IN_organizationId and l.warehouseid = IN_Warehouse
      and l.locationid = r_tolocation  and l.locationcategory = 'SAM' ;
       if r_nrow > 0 then
         r_end_location:=r_tolocation;
         r_tolocation:='';
         r_toloctype:='BUF_P';
         r_tolocation_WCS:='';
         r_tolocmode:='';
         r_tolocgrp2:='';
       end if;
      end if;

    end if;


  --下发虚拟任务
         spcom_getidsequence(IN_organizationId,IN_Warehouse,IN_Language,'GROUPTASKID',r_GROUPTASKID,out_return_code);
          if substr(OUT_Return_Code,1,3) <> '000' then
            rollback;
            return;
          end if;
          insert into idx_asrs_send (organizationid,warehouseid,grouptaskid,tasktype,palletid,fmlocation,tolocation,tolocation_WCS,
                 taskstatus,priority,sendtime_appointment,addtime,addwho,docno,equiptype,businesstype,ispackage,waveno,emptyflag,palletdetails,tolocation_type
                 ,fmzone,fmlocgrp2,fmloctype_wcs,fmloctype,fmlocmode,tozone,tolocgrp2,toloctype_wcs,toloctype,tolocmode,notes,udf05,edisendflag,end_loctype,end_zone,end_model,end_location)
          values (IN_organizationId,IN_Warehouse,r_GROUPTASKID,'V_MV',(case when r_IN_TRACEID = 'ERR' then r_GROUPTASKID else r_IN_TRACEID end ),IN_FMLOCATION,r_tolocation,nvl(r_tolocation_WCS,r_tolocation),
                '00',10,null,sysdate,IN_UserID,'','LT','','','','N','',''
                ,'','','','','',r_tozone,r_tolocgrp2,'',r_toloctype,r_tolocmode,'from-to移动','in_type:2',r_SEND_FLAG,r_end_loctype,r_end_zone,r_end_model,r_end_location);
         --写入日志
          insert into idx_asrs_send_log (organizationid,warehouseid,grouptaskid,palletid,status,addtime,addwho,notes)
                             values     (IN_organizationId,IN_Warehouse,r_GROUPTASKID,r_IN_TRACEID,'00',sysdate,IN_UserID,'from-to移动');

     commit;
     goto to_send;
     return;
end if;
--***************************************************** in_type  = 3 拣货完成，扫托盘进行下一步走向（余料回库，至配盘接驳位 共用）*********************
if IN_TYPE = '3' then
  if nvl(IN_FMLOCATION,'*') = '*' then
     OUT_Return_Code:='999#engine(3): 库位不能为空';
     rollback;
      return;
  end if;
  if nvl(r_IN_TRACEID,'*') = '*' then
     OUT_Return_Code:='999#engine(3): 托盘号/货架号不能为空';
     rollback;
      return;
  end if;
  select  count(*) into r_nrow from bas_location l where l.organizationid = IN_organizationId and l.warehouseid = IN_Warehouse
   and l.locationid = IN_FMLOCATION;
  if r_nrow = 0 then
       OUT_Return_Code:='999#engine(3): 原货位:'||IN_FMLOCATION||'不合法，请检查';
           rollback;
           return;
  end if;
  begin
  select l.locationcategory into r_fmloctype  from bas_location l where l.organizationid = IN_organizationId and l.warehouseid = IN_Warehouse and l.locationid = IN_FMLOCATION
  and l.locationcategory in ('JBW','ZZW','PK','PK2','FCD','SAM') ; --接驳位、货架周转位、在线拣选工位，在线拣选目标货位,自动拆垛拣货工位
  exception
    when no_data_found then
        OUT_Return_Code:='999#engine(3): 原货位不是 接驳位、货架周转位、在线拣选工位、自动拆垛拣货工位';
        rollback;
        return;
    end;

     --重复任务检查
    select count(0) into r_nrow from idx_asrs_send a where a.organizationid = IN_organizationId and a.warehouseid = IN_Warehouse
     and a.fmlocation = IN_FMLOCATION and a.palletid = r_IN_TRACEID and a.taskstatus < '81' ;
    if r_nrow > 0 then
        OUT_Return_Code:='999#engine(3): 已有任务在运行，不可重复下发';
        rollback;
        return;
    end if;

   --接驳位处理去向
  if r_fmloctype = 'JBW' then
  --查询当前任务状态
    select count(0),max(a.grouptaskid),max(a.tasktype) into r_nrow,r_GROUPTASKID_OLD,r_tasktype from idx_asrs_send a where a.organizationid = IN_organizationId and a.warehouseid = IN_Warehouse
     and a.tolocation = IN_FMLOCATION and a.palletid = r_IN_TRACEID and a.taskstatus = '81' ;--and a.tasktype in  ('V_PK','PK') ;
    if r_nrow = 0 then
        OUT_Return_Code:='999#engine(3): 识别到任务没有到达对应位置，请检查';
        rollback;
        return;
    end if;
  --查询库存是否为拣货目标托盘
  select count(0) into r_nrow from inv_lot_loc_id iv where iv.organizationid = IN_organizationId and iv.warehouseid = IN_Warehouse and iv.traceid = r_IN_TRACEID
  and iv.qty > 0  and iv.locationid like 'SORTATION%';
  if r_nrow > 0 then
     --查订单是否指定出口
      begin
        select h.hedi12,h.hedi13,h.waveno,h.orderno,null,bc.udf02,bc.udf03,h.ordertype
        into r_docJBW,r_docout,r_waveno,r_docno ,r_sendtime_appointment,r_type_toloctype,r_type_toloction,r_doctype
        from doc_order_header h
        inner join act_allocation_details ad on h.organizationid = ad.organizationid and h.warehouseid = ad.warehouseid and h.orderno = ad.orderno
        left join bsm_code bc on h.ordertype = bc.codeid and bc.organizationid = h.organizationid --and bc.warehouseid = h.warehouseid
        and bc.codetype = 'SO_TYP'
        where ad.picktotraceid = r_IN_TRACEID and ad.status = '60'
        and ad.addtime >= sysdate -r_dateint
        and h.organizationid = IN_organizationId and h.warehouseid = IN_Warehouse and rownum = 1;
      exception
        when no_data_found then
            OUT_Return_Code:='999#engine(3): 托盘号不能定位到拣货定单，请检查';
            rollback;
            return;
        end;
      --处理到取样间的去向
       select count(0),max(bl.locationid) into r_nrow,r_tolocation from bas_location bl where bl.organizationid = IN_organizationId and bl.warehouseid = IN_Warehouse
        and (bl.locationid = r_docJBW or bl.locgroup2 = r_docJBW or bl.zoneid = r_docJBW ) and bl.zoneid = '3FBUFFER'  and rownum = 1 ;
        if r_nrow > 0 then
                --去缓存区，自动找空闲
                r_tozone:='3FBUFFER';
                r_toloctype:='BUF_P';
                r_end_location:=r_tolocation; --最终去向货位
                r_tolocation:='';
                r_tolocation_WCS:='';

           r_palletid:=r_IN_TRACEID;
           r_fmlocation := IN_FMLOCATION;
           r_send:='Y';
           r_taskstatus:='00';
           if r_doctype = 'B15' then  --复检拣货
            r_tasktype:='V_MV4'; --到达后返拣
             else
            r_tasktype:='V_MV2';
           end if;

          spcom_getidsequence(IN_organizationId,IN_Warehouse,IN_Language,'GROUPTASKID',r_GROUPTASKID,out_return_code);
          if substr(OUT_Return_Code,1,3) <> '000' then
            rollback;
            return;
          end if;
          r_docout:=nvl(r_docout,r_type_toloction);
          insert into idx_asrs_send (organizationid,warehouseid,grouptaskid,tasktype,palletid,fmlocation,tolocation,tolocation_WCS,
                 taskstatus,priority,sendtime_appointment,addtime,addwho,docno,equiptype,businesstype,ispackage,waveno,emptyflag,palletdetails,tolocation_type
                 ,fmzone,fmlocgrp2,fmloctype_wcs,fmloctype,fmlocmode,tozone,tolocgrp2,toloctype_wcs,toloctype,tolocmode,notes,udf05,end_location,edisendflag)
          values (IN_organizationId,IN_Warehouse,r_GROUPTASKID,r_tasktype,r_IN_TRACEID,IN_FMLOCATION,r_tolocation,r_tolocation_WCS,
                '00',10,r_sendtime_appointment,sysdate,IN_UserID,r_docno,'LT','','',r_waveno,'N','',''
                ,'','','','','',r_tozone,'','',r_toloctype,'','送取样区','in_type:3',r_end_location,r_SEND_FLAG);
      ---
       --写入日志
          insert into idx_asrs_send_log (organizationid,warehouseid,grouptaskid,palletid,status,addtime,addwho,notes)
                             values     (IN_organizationId,IN_Warehouse,r_GROUPTASKID,r_IN_TRACEID,'00',sysdate,IN_UserID,'人工指定出口');
          commit;

         OUT_Return_Code:='000';
         return;
       end if;

       --人工指定出口的情况,或定单类型指定出口，直接生成目标库位的虚拟移动指令
       if nvl(nvl(r_docout,r_type_toloction),'*') <> '*' then
          spcom_getidsequence(IN_organizationId,IN_Warehouse,IN_Language,'GROUPTASKID',r_GROUPTASKID,out_return_code);
          if substr(OUT_Return_Code,1,3) <> '000' then
            rollback;
            return;
          end if;
          r_docout:=nvl(r_docout,r_type_toloction);
          insert into idx_asrs_send (organizationid,warehouseid,grouptaskid,tasktype,palletid,fmlocation,tolocation,tolocation_WCS,
                 taskstatus,priority,sendtime_appointment,addtime,addwho,docno,equiptype,businesstype,ispackage,waveno,emptyflag,palletdetails,tolocation_type
                 ,fmzone,fmlocgrp2,fmloctype_wcs,fmloctype,fmlocmode,tozone,tolocgrp2,toloctype_wcs,toloctype,tolocmode,notes,udf05,edisendflag)
          values (IN_organizationId,IN_Warehouse,r_GROUPTASKID,'V_MV',r_IN_TRACEID,IN_FMLOCATION,r_docout,r_docout,
                '00',10,r_sendtime_appointment,sysdate,IN_UserID,r_docno,'LT','','',r_waveno,'N','',''
                ,'','','','','','','','','','','人工指定出口','in_type:3',r_SEND_FLAG);
      ---
       --写入日志
          insert into idx_asrs_send_log (organizationid,warehouseid,grouptaskid,palletid,status,addtime,addwho,notes)
                             values     (IN_organizationId,IN_Warehouse,r_GROUPTASKID,r_IN_TRACEID,'00',sysdate,IN_UserID,'人工指定出口');
          commit;
          OUT_Return_Code:='000';
          return;
       else
         --
         --查询订单类型是否定义去向类型
          if nvl(r_type_toloctype,'*') = '*' then
         --查询拣货库区配置的去向
          begin
          select z.udf03,z.udf04 into r_direction,r_direction_zone from bas_location l
          inner join bas_zone z on l.zoneid = z.zoneid and l.organizationid = z.organizationid and l.warehouseid = z.warehouseid
          where l.locationid = IN_FMLOCATION and rownum =1
           and l.organizationid = IN_organizationId and l.warehouseid = IN_Warehouse;
          exception
            when no_data_found then
              OUT_Return_Code:='999#engine(3): 货位没有配置区域，请检查';
              rollback;
              return;
            end;
           --检查区域去向配置情况
          if nvl(r_direction,'*')  not in ('PUT','BUF','TO','OUT')  then
              OUT_Return_Code:='999#engine(3): 区域去向配置有误，请检查';
              rollback;
              return;
          end if;
          --去配盘区
          if r_direction = 'PUT' then
             r_tozone:=r_direction_zone;
             r_toloctype:='JBW';
             r_tolocation_WCS:='';
          end if;
          --去缓存
          if r_direction = 'BUF' then
             r_tozone:=r_direction_zone;
             r_toloctype:='BUF';
             r_tolocation_WCS:='';
          end if;
          --直送
          if r_direction = 'TO' then
             r_tozone:='';
             r_toloctype:=''; --未定义，暂取区域下的
             r_tolocation_WCS:=r_docJBW;
          end if;
          --去出口
          if r_direction = 'OUT' then
             r_tozone:=r_direction_zone;
             r_toloctype:='OUT';
             r_tolocation_WCS:='';
          end if;
          else
             r_tozone:='24'; --缓存区下的指定位置类型
             r_toloctype:=r_type_toloctype;
             r_tolocation_WCS:='';
          end if;
             spcom_getidsequence(IN_organizationId,IN_Warehouse,IN_Language,'GROUPTASKID',r_GROUPTASKID,out_return_code);
             if substr(OUT_Return_Code,1,3) <> '000' then
               rollback;
               return;
              end if;
             insert into idx_asrs_send (organizationid,warehouseid,grouptaskid,tasktype,palletid,fmlocation,tolocation,tolocation_WCS,
                 taskstatus,priority,sendtime_appointment,addtime,addwho,docno,equiptype,businesstype,ispackage,waveno,emptyflag,palletdetails,tolocation_type
                 ,fmzone,fmlocgrp2,fmloctype_wcs,fmloctype,fmlocmode,tozone,tolocgrp2,toloctype_wcs,toloctype,tolocmode,notes,udf05,edisendflag)
             values (IN_organizationId,IN_Warehouse,r_GROUPTASKID,'V_MV',r_IN_TRACEID,IN_FMLOCATION,'',r_tolocation_WCS,
                '00',10,r_sendtime_appointment,sysdate,IN_UserID,r_docno,'LT','','',r_waveno,'N','',''
                ,'','','','JBW','',r_tozone,'','',r_toloctype,'','','in_type:3',r_SEND_FLAG);
             ---
              --写入日志
          insert into idx_asrs_send_log (organizationid,warehouseid,grouptaskid,palletid,status,addtime,addwho,notes)
                             values     (IN_organizationId,IN_Warehouse,r_GROUPTASKID,r_IN_TRACEID,'00',sysdate,IN_UserID,'');
             commit;
      end if;
    else
            OUT_Return_Code:='999#engine(3): 非拣货托盘，如是托盘回收请用托盘回收功能，请检查';
            rollback;
            return;
   end if;
  end if;
   --货架周转位处理去向
   --如果拣货没有完成发起的移动回库，需要再插入一条任务等待下发
  if r_fmloctype = 'ZZW' then
    --查询当前任务状态
    select count(0),max(a.grouptaskid),max(a.tasktype) into r_nrow,r_GROUPTASKID_OLD,r_tasktype from idx_asrs_send a where a.organizationid = IN_organizationId and a.warehouseid = IN_Warehouse
     and a.tolocation = IN_FMLOCATION and a.palletid = r_IN_TRACEID and a.taskstatus = '81' ;--and a.tasktype in  ('V_PK','PK') ;
    if r_nrow = 0 then
        OUT_Return_Code:='999#engine(3): 识别到任务没有到达对应位置，请检查';
        rollback;
        return;
    end if;
    --查询货架对应货位的拣货任务是否有完成
    --select count(0),max(l.locationid),max(l.zoneid) into r_nrow,r_fmlocation,r_tozone from inv_lot_loc_id iv
    --inner join bas_location l on iv.organizationid = l.organizationid and iv.warehouseid = l.warehouseid and iv.locationid = l.locationid
    --where l.udf01 = r_IN_TRACEID and iv.qty > 0 and iv.qtyallocated > 0;

     select count(0),max(bl.locationid),max(bl.zoneid) into r_nrow,r_fmlocation,r_tozone
      from act_allocation_details aad
     inner join doc_order_header oh on aad.organizationid = oh.organizationid and aad.warehouseid = oh.warehouseid and aad.orderno = oh.orderno
     inner join bas_location bl on bl.organizationid = aad.organizationid and bl.warehouseid = aad.warehouseid and bl.locationid = aad.location
     where aad.addtime >= sysdate -r_dateint and aad.status = '40' and bl.udf01 = r_IN_TRACEID  and oh.udfwcs = 'Y'
      and aad.organizationid = IN_organizationId and aad.warehouseid = IN_Warehouse ;
     if r_nrow > 0 then
        OUT_Return_Code:='999#engine(3): 该货架已下发的任务没有完成拣货，请检查';
        rollback;
        return;
     end if;
    --如果还有待拣任务，回库任需要再下发一条待拣货货架移出任务 (拦截后将不再下发 2022-7-16  syx )
    if r_nrow > 0 then
            --重新下发新任务
            spcom_getidsequence(IN_organizationId,IN_Warehouse,IN_Language,'GROUPTASKID',r_GROUPTASKID_NEW,out_return_code);
             if substr(OUT_Return_Code,1,3) <> '000' then
               rollback;
               return;
              end if;
            --取货架原始位置
            begin
              select iv.locationid into r_fmlocation  from inv_lot_loc_id iv where iv.traceid = r_IN_TRACEID and iv.organizationid = IN_organizationId and iv.warehouseid = IN_Warehouse
              and iv.qty > 0 and rownum = 1;
            exception
              when no_data_found then
                 OUT_Return_Code:='999#engine(3): 货架原始位置没定位到，请检查';
                 rollback;
                 return;
              end;
           insert into idx_asrs_send (organizationid,warehouseid,grouptaskid,tasktype,palletid,fmlocation,tolocation,tolocation_WCS,
                 taskstatus,priority,sendtime_appointment,addtime,addwho,docno,equiptype,businesstype,ispackage,waveno,emptyflag,palletdetails,tolocation_type
                 ,fmzone,fmlocgrp2,fmloctype_wcs,fmloctype,fmlocmode,tozone,tolocgrp2,toloctype_wcs,toloctype,tolocmode,notes,udf05,edisendflag)
             values (IN_organizationId,IN_Warehouse,r_GROUPTASKID_NEW,r_tasktype,r_IN_TRACEID,r_fmlocation,'','',
                '00',10,r_sendtime_appointment,sysdate,IN_UserID,r_docno,'LT','','',r_waveno,'N','',''
                ,'','','','','',r_tozone,'','','ZZW','OUT','','in_type:3',r_SEND_FLAG);
             --写入日志
          insert into idx_asrs_send_log (organizationid,warehouseid,grouptaskid,palletid,status,addtime,addwho,notes)
                             values     (IN_organizationId,IN_Warehouse,r_GROUPTASKID_NEW,r_IN_TRACEID,'00',sysdate,IN_UserID,'');
    end if;
          --下发回库任务
          --取货架原始库位
          begin
            select iv.locationid into r_tolocation from inv_lot_loc_id iv where iv.warehouseid = IN_Warehouse
             and iv.organizationid = IN_organizationId and iv.traceid = r_IN_TRACEID and iv.qty > 0
             and iv.sku = 'KIVASHELF' and rownum = 1  ;
          exception
            when no_data_found then
               OUT_Return_Code:='999#engine(3): 回库货架计算目标位失败，请检查';
               rollback;
               return;
            end;
           spcom_getidsequence(IN_organizationId,IN_Warehouse,IN_Language,'GROUPTASKID',r_GROUPTASKID,out_return_code);
             if substr(OUT_Return_Code,1,3) <> '000' then
               rollback;
               return;
              end if;
           insert into idx_asrs_send (organizationid,warehouseid,grouptaskid,tasktype,palletid,fmlocation,tolocation,tolocation_WCS,
                 taskstatus,priority,sendtime_appointment,addtime,addwho,docno,equiptype,businesstype,ispackage,waveno,emptyflag,palletdetails,tolocation_type
                 ,fmzone,fmlocgrp2,fmloctype_wcs,fmloctype,fmlocmode,tozone,tolocgrp2,toloctype_wcs,toloctype,tolocmode,notes,udf05,edisendflag)
             values (IN_organizationId,IN_Warehouse,r_GROUPTASKID,'V_MV',r_IN_TRACEID,IN_FMLOCATION,r_tolocation,r_tolocation,
                '00',10,r_sendtime_appointment,sysdate,IN_UserID,r_docno,'LT','','',r_waveno,'N','',''
                ,'','','','ZZW','','','','','','','货架回库','in_type:3',r_SEND_FLAG);
            --写入日志
          insert into idx_asrs_send_log (organizationid,warehouseid,grouptaskid,palletid,status,addtime,addwho,notes)
                             values     (IN_organizationId,IN_Warehouse,r_GROUPTASKID,r_IN_TRACEID,'00',sysdate,IN_UserID,'货架回库');
        commit;
  end if;
   --在线拣选工位、自动拆垛拣货处理去向
  if r_fmloctype in ('PK','PK2','FCD','SAM') then
  --查询当前任务状态
  if r_fmloctype in ('PK2') then
     select count(0) into r_nrow from inv_lot_loc_id iv where iv.organizationid = IN_organizationId and iv.warehouseid = IN_Warehouse
     and iv.traceid = r_IN_TRACEID and iv.qty > 0 ;
     if r_nrow = 0 then
        OUT_Return_Code:='999#engine(3): 反拣托盘没有库存，请检查';
        rollback;
        return;
     end if;
    --需要WCS反馈托盘到达来点位
    select count(0) into r_nrow from idx_asrs_send a where a.organizationid = IN_organizationId and a.warehouseid = IN_Warehouse
     and a.tolocation = IN_FMLOCATION and a.palletid = r_IN_TRACEID and a.taskstatus = '81' ;--and a.tasktype in  ('V_PK','PK') ;
    if r_nrow = 0 then
        OUT_Return_Code:='999#engine(3): 识别到任务没有到达对应位置，请检查';
        rollback;
        return;
    end if;

   else
    --PK 位置要看当前托盘任务是不是拣货
     /*select count(0) into r_nrow from act_allocation_details aad
     inner join doc_order_header oh on aad.organizationid = oh.organizationid and aad.warehouseid = oh.warehouseid and aad.orderno = oh.orderno
     where aad.addtime >= sysdate -7 and aad.status = '40' and aad.traceid = r_IN_TRACEID and oh.udfwcs = 'Y'
     and aad.organizationid = IN_organizationId and aad.warehouseid = IN_Warehouse  ;
     if r_nrow > 0 then
        OUT_Return_Code:='999# SPCUS_TASKGROUP_engine(3): 该托盘已下发的任务没有完成拣货，请检查';
        rollback;
        return;
     end if;*/

     --如当前位置有库存，允许再发起
     select count(0) into r_nrow from inv_lot_loc_id iv where iv.organizationid = IN_organizationId and iv.warehouseid = IN_Warehouse
     and iv.traceid = r_IN_TRACEID and iv.qty > 0 and iv.locationid = IN_FMLOCATION ;
     if  r_nrow = 0 then
    select count(0),max(a.grouptaskid),max(a.tasktype) into r_nrow,r_GROUPTASKID_OLD,r_tasktype from idx_asrs_send a where a.organizationid = IN_organizationId and a.warehouseid = IN_Warehouse
     and a.tolocation = IN_FMLOCATION and a.palletid = r_IN_TRACEID and a.taskstatus = '81' ;--and a.tasktype in  ('V_PK','PK') ;
    if r_nrow = 0 then
        OUT_Return_Code:='999#engine(3): 识别到任务没有到达对应位置，请检查';
        rollback;
        return;
    end if;
    end if;
   end if;
  --查询库存是否为拣货目标托盘
  select count(0) into r_nrow from inv_lot_loc_id iv where iv.organizationid = IN_organizationId and iv.warehouseid = IN_Warehouse and iv.traceid = r_IN_TRACEID
  and iv.qty > 0  and iv.locationid like 'SORTATION%';
  if r_nrow > 0 then
     --查订单是否指定出口
      begin
        select h.hedi12,nvl(h.hedi13,bc.udf04),h.waveno,h.orderno ,bc.udf02,h.ordertype
            into r_docJBW,r_docout,r_waveno,r_docno,r_type_toloctype,r_doctype
        from doc_order_header h
        inner join act_allocation_details ad on h.organizationid = ad.organizationid and h.warehouseid = ad.warehouseid and h.orderno = ad.orderno
        left join bsm_code bc on h.ordertype = bc.codeid and bc.organizationid = h.organizationid --and bc.warehouseid = h.warehouseid
        and bc.codetype = 'SO_TYP'
        where ad.picktotraceid = r_IN_TRACEID and ad.status = '60' and ad.addtime >= sysdate -r_dateint
        and h.organizationid = IN_organizationId and h.warehouseid = IN_Warehouse and rownum = 1;
      exception
        when no_data_found then
            OUT_Return_Code:='999#engine(3): 托盘号不能定位到拣货定单，请检查';
            rollback;
            return;
      end;


      --处理到取样间的去向
       select count(0),max(bl.locationid) into r_nrow,r_tolocation from bas_location bl where bl.organizationid = IN_organizationId and bl.warehouseid = IN_Warehouse
        and (bl.locationid = r_docJBW or bl.locgroup2 = r_docJBW or bl.zoneid = r_docJBW ) and bl.zoneid = '3FBUFFER'  and rownum = 1 ;
        if r_nrow > 0 then

                --去缓存区，自动找空闲
                r_tozone:='3FBUFFER';
                r_toloctype:='BUF_P';
                r_end_location:=r_tolocation; --最终去向货位
                r_tolocation:='';
                r_tolocation_WCS:='';


           r_palletid:=r_IN_TRACEID;
           r_fmlocation := IN_FMLOCATION;
           r_send:='Y';
           r_taskstatus:='00';

           if r_doctype = 'B15' then  --复检拣货
            r_tasktype:='V_MV4'; --到达后返拣
             else
            r_tasktype:='V_MV2';
           end if;

          spcom_getidsequence(IN_organizationId,IN_Warehouse,IN_Language,'GROUPTASKID',r_GROUPTASKID,out_return_code);
          if substr(OUT_Return_Code,1,3) <> '000' then
            rollback;
            return;
          end if;
          r_docout:=nvl(r_docout,r_type_toloction);
          insert into idx_asrs_send (organizationid,warehouseid,grouptaskid,tasktype,palletid,fmlocation,tolocation,tolocation_WCS,
                 taskstatus,priority,sendtime_appointment,addtime,addwho,docno,equiptype,businesstype,ispackage,waveno,emptyflag,palletdetails,tolocation_type
                 ,fmzone,fmlocgrp2,fmloctype_wcs,fmloctype,fmlocmode,tozone,tolocgrp2,toloctype_wcs,toloctype,tolocmode,notes,udf05,end_location,edisendflag)
          values (IN_organizationId,IN_Warehouse,r_GROUPTASKID,r_tasktype,r_IN_TRACEID,IN_FMLOCATION,r_tolocation,r_tolocation_WCS,
                '00',10,r_sendtime_appointment,sysdate,IN_UserID,r_docno,'LT','','',r_waveno,'N','',''
                ,'','','','','',r_tozone,'','',r_toloctype,'','送取样区','in_type:3',r_end_location,r_SEND_FLAG);
      ---
       --写入日志
          insert into idx_asrs_send_log (organizationid,warehouseid,grouptaskid,palletid,status,addtime,addwho,notes)
                             values     (IN_organizationId,IN_Warehouse,r_GROUPTASKID,r_IN_TRACEID,'00',sysdate,IN_UserID,'人工指定出口');
          commit;

         OUT_Return_Code:='000';
         return;
       end if;



       --人工指定出口的情况，直接生成目标库位的虚拟移动指令
       if nvl(r_docout,'*') <> '*' then

         --检查目标位是否合法
         begin
          select bl.locationid,bl.locgroup2,lg.udf02 into r_tolocation,r_tolocation_WCS,r_isgroupid  from bas_location bl
          left join bas_locgroup2 lg on bl.locgroup2 = lg.locgroup2 and lg.organizationid = bl.organizationid and lg.warehouseid = bl.warehouseid
          where bl.organizationid = IN_organizationId and bl.warehouseid = IN_Warehouse
           and (bl.locationid = r_docout or bl.locgroup2 = r_docout) and rownum = 1;
         exception
           when no_data_found then
               OUT_Return_Code:='999#engine(3): 单号指定货位不合法，请检查';
                rollback;
                return;
           end;
           if nvl(r_isgroupid,'N') = 'N' then
            r_tolocation_WCS:=r_tolocation;
           end if;

          spcom_getidsequence(IN_organizationId,IN_Warehouse,IN_Language,'GROUPTASKID',r_GROUPTASKID,out_return_code);
          if substr(OUT_Return_Code,1,3) <> '000' then
            rollback;
            return;
          end if;
          insert into idx_asrs_send (organizationid,warehouseid,grouptaskid,tasktype,palletid,fmlocation,tolocation,tolocation_WCS,
                 taskstatus,priority,sendtime_appointment,addtime,addwho,docno,equiptype,businesstype,ispackage,waveno,emptyflag,palletdetails,tolocation_type
                 ,fmzone,fmlocgrp2,fmloctype_wcs,fmloctype,fmlocmode,tozone,tolocgrp2,toloctype_wcs,toloctype,tolocmode,notes,udf05,edisendflag)
          values (IN_organizationId,IN_Warehouse,r_GROUPTASKID,'V_MV',r_IN_TRACEID,IN_FMLOCATION,r_tolocation,r_tolocation_WCS,
                '00',10,null,sysdate,IN_UserID,r_docno,'LT','','',r_waveno,'N','',''
                ,'','','','','','','','','','','人工指定出口','in_type:3',r_SEND_FLAG);
      ---
        --写入日志
          insert into idx_asrs_send_log (organizationid,warehouseid,grouptaskid,palletid,status,addtime,addwho,notes)
                             values     (IN_organizationId,IN_Warehouse,r_GROUPTASKID,r_IN_TRACEID,'00',sysdate,IN_UserID,'人工指定出口');
          commit;
       else
         if nvl(r_type_toloctype,'*') = '*' then
         --查询拣货库区配置的去向
          begin
          select nvl(z.udf03,'OUT'),nvl(z.udf04,l.zoneid),l.locationcategory into r_direction,r_direction_zone,r_fmloctype from bas_location l
          inner join bas_zone z on l.zoneid = z.zoneid and l.organizationid = z.organizationid and l.warehouseid = z.warehouseid
          where l.locationid = IN_FMLOCATION and rownum =1
           and l.organizationid = IN_organizationId and l.warehouseid = IN_Warehouse;
          exception
            when no_data_found then
              OUT_Return_Code:='999#engine(3): 货位没有配置区域，请检查';
              rollback;
              return;
            end;
           --检查区域去向配置情况
          if nvl(r_direction,'*')  not in ('PUT','BUF','TO','OUT')  then
              OUT_Return_Code:='999#engine(3): 区域去向配置有误，请检查';
              rollback;
              return;
          end if;
          --去配盘区
          if r_direction = 'PUT' then
            --是否同一时间，同一目的地
            select count(0),max(udf01) into r_nrow,r_tolocgrp2 from (
            select oh.expectedshipmenttime2,oh.hedi12,w.udf01,oh.consigneeid from act_allocation_details ad
            inner join doc_order_header oh on oh.organizationid = ad.organizationid and oh.warehouseid = ad.warehouseid and ad.orderno = oh.orderno
            left join doc_wave_header w on oh.organizationid = w.organizationid and oh.warehouseid = w.warehouseid and oh.waveno = w.waveno
            where ad.organizationid = IN_organizationId and ad.warehouseid = IN_Warehouse and oh.udfwcs = 'Y'
            and ad.picktotraceid = r_IN_TRACEID and ad.status = '60' and ad.addtime >= sysdate -r_dateint
            group by oh.expectedshipmenttime2,oh.hedi12,w.udf01,oh.consigneeid);
           if r_nrow > 1 then
              r_tozone:='23';
              r_toloctype:='JBW';
              r_tolocation_WCS:='';
              r_tolocgrp2:='';
           else
              r_tozone:='24';
              r_toloctype:='BUF';
              r_tolocgrp2:='';
           end if;
          end if;
          --去缓存区
          if r_direction = 'BUF' then
             r_tozone:=r_direction_zone;
             r_toloctype:='BUF';
             r_tolocation_WCS:='';
          end if;
          --直送
          if r_direction = 'TO' then
             r_tozone:='';
             r_toloctype:=''; --未定义，暂取区域下的
             r_tolocation_WCS:=r_docJBW;
          end if;
          --去出口
          if r_direction = 'OUT' then
             r_tozone:=r_direction_zone;
             r_toloctype:='OUT'; --未定义，暂取区域下的
             r_tolocation_WCS:='';

              --如果是1F 在线发起的反拣，去指定locgroup2 出口 2022-9-24 syx
             if r_direction_zone = '1FOUTSTEREO' and r_fmloctype = 'PK' then
                r_tolocgrp2:='1FOUTPORT';
             end if;

          end if;
          else
            --订单类型有定义的目标货位类型
             r_tozone:='24';
             r_toloctype:=r_type_toloctype;
             r_tolocation_WCS:='';
         end if;
             spcom_getidsequence(IN_organizationId,IN_Warehouse,IN_Language,'GROUPTASKID',r_GROUPTASKID,out_return_code);
             if substr(OUT_Return_Code,1,3) <> '000' then
               rollback;
               return;
              end if;
             insert into idx_asrs_send (organizationid,warehouseid,grouptaskid,tasktype,palletid,fmlocation,tolocation,tolocation_WCS,
                 taskstatus,priority,sendtime_appointment,addtime,addwho,docno,equiptype,businesstype,ispackage,waveno,emptyflag,palletdetails,tolocation_type
                 ,fmzone,fmlocgrp2,fmloctype_wcs,fmloctype,fmlocmode,tozone,tolocgrp2,toloctype_wcs,toloctype,tolocmode,notes,udf05,edisendflag)
             values (IN_organizationId,IN_Warehouse,r_GROUPTASKID,'V_MV',r_IN_TRACEID,IN_FMLOCATION,'',r_tolocation_WCS,
                '00',10,r_sendtime_appointment,sysdate,IN_UserID,r_docno,'LT','','',r_waveno,'N','',''
                ,'','','','','',r_tozone,r_tolocgrp2 ,'',r_toloctype,'','','in_type:3',r_SEND_FLAG);
             ---
             --写入日志
          insert into idx_asrs_send_log (organizationid,warehouseid,grouptaskid,palletid,status,addtime,addwho,notes)
                             values     (IN_organizationId,IN_Warehouse,r_GROUPTASKID,r_IN_TRACEID,'00',sysdate,IN_UserID,'');
            commit;
      end if;
    else
    --查询是否已生成回库任务
      select count(0) into r_nrow from inv_lot_loc_id iv where iv.organizationid = IN_organizationId and iv.warehouseid = IN_Warehouse
      and iv.traceid = r_IN_TRACEID and iv.qtymvin > 0 and iv.qty = 0 and iv.locationid not like 'SORTATION%';
     --如果没有产生任务，回原库位
     if r_nrow = 0 then
          --产生回库任务
       if nvl(IN_TOLOCATION,'*') = '*' then
           r_direction:='';
           --计算上架库位
                   SIRIO_SPCUS_Calc_loc
                     (IN_organizationId  ,
                      IN_Warehouse      ,
                      ''       ,
                      r_IN_TRACEID          , --托盘号
                      ''              , --SKU
                      r_tolocation        ,
                      r_model             , --LOC 货位，LOG 货架
                      'DATAHUB'           ,
                      IN_Language         ,
                      OUT_Return_Code
                      );
                   if  substr(OUT_Return_Code,1,3) <> '000' then
                     --OUT_Return_Code:='999# SPCUS_TASKGROUP_engine(3): 没有获取到目标货位(非立库) ';
                      rollback;
                      return;
                   end if;
       else
        --如果有货位传入，就不再计算目位库位
        r_tolocation := IN_TOLOCATION;
       end if;
       if  nvl(r_tolocation,'*') <> '*' and r_model = 'LOC1' then --立库才生成任务
        /*OUT_Return_Code := '*_*';
         SIRIO_SPCUS_getMOVETASK
         (IN_organizationId,
          IN_Warehouse,
          r_IN_TRACEID, --托盘号
          r_tolocation, --目标库位，留空时调用目标库位计算逻辑
          '', --移动类型，区分是否需要生成移动单
          '', --预留
          '', --预留
          'Y', --预留,是否允许下发
          IN_UserID,
          IN_Language,
          OUT_Return_Code
          );
         if substr(OUT_Return_Code,1,3) <> '000' then
                 OUT_Return_Code:='999# SPCUS_TASKGROUP_engine(3): 生成上架/移动任务失败,错误信息：'||OUT_Return_Code;
                 rollback;
                 return;
         end if;
         */

           /*--处理当前库位81任务
            select * from idx_asrs_send ias where ias.organizationid = IN_organizationId and ias.warehouseid = IN_Warehouse
            and ias.palletid = r_IN_TRACEID and ias.tolocation = IN_FMLOCATION and ias.taskstatus = '81';*/


             spcom_getidsequence(IN_organizationId,IN_Warehouse,IN_Language,'GROUPTASKID',r_GROUPTASKID,out_return_code);
             if substr(OUT_Return_Code,1,3) <> '000' then
               rollback;
               return;
              end if;
             insert into idx_asrs_send (organizationid,warehouseid,grouptaskid,tasktype,palletid,fmlocation,tolocation,tolocation_WCS,
                 taskstatus,priority,sendtime_appointment,addtime,addwho,docno,equiptype,businesstype,ispackage,waveno,emptyflag,palletdetails,tolocation_type
                 ,fmzone,fmlocgrp2,fmloctype_wcs,fmloctype,fmlocmode,tozone,tolocgrp2,toloctype_wcs,toloctype,tolocmode,notes,udf05,edisendflag)
             values (IN_organizationId,IN_Warehouse,r_GROUPTASKID,'V_MV3',r_IN_TRACEID,IN_FMLOCATION,r_tolocation,r_tolocation,
                '05',10,r_sendtime_appointment,sysdate,IN_UserID,r_docno,'LT','',r_isPack,r_waveno,'N','',''
                ,'','','','','','','','','','','','in_type:3',r_SEND_FLAG);
             ---
              --写入日志
              insert into idx_asrs_send_log (organizationid,warehouseid,grouptaskid,palletid,status,addtime,addwho,notes)
                             values     (IN_organizationId,IN_Warehouse,r_GROUPTASKID,r_IN_TRACEID,'05',sysdate,IN_UserID,'');
             r_in_taskflag:='N';
             commit;

        end if;

        --立库转清单的情况续要在拣货完后进行收货
        if  nvl(r_tolocation,'*') <> '*' and r_model = 'LOC2' and r_fmloctype = 'PK2' then --平库的去接驳位

            begin
             select l.zoneid,'IN','JBW' into r_tozone,r_tolocmode,r_toloctype
               from bas_location l where l.organizationid = IN_organizationId
             and l.warehouseid = IN_Warehouse and l.locationid = r_tolocation and rownum = 1;
             r_tolocation:='';
             r_tolocation_WCS:='';
            exception
              when no_data_found then
                OUT_Return_Code:='999#engine(3)目标货位不合法';
                return;
              end;

             spcom_getidsequence(IN_organizationId,IN_Warehouse,IN_Language,'GROUPTASKID',r_GROUPTASKID,out_return_code);
             if substr(OUT_Return_Code,1,3) <> '000' then
               rollback;
               return;
              end if;
             insert into idx_asrs_send (organizationid,warehouseid,grouptaskid,tasktype,palletid,fmlocation,tolocation,tolocation_WCS,
                 taskstatus,priority,sendtime_appointment,addtime,addwho,docno,equiptype,businesstype,ispackage,waveno,emptyflag,palletdetails,tolocation_type
                 ,fmzone,fmlocgrp2,fmloctype_wcs,fmloctype,fmlocmode,tozone,tolocgrp2,toloctype_wcs,toloctype,tolocmode,notes,udf05,edisendflag)
             values (IN_organizationId,IN_Warehouse,r_GROUPTASKID,'V_MV',r_IN_TRACEID,IN_FMLOCATION,r_tolocation,r_tolocation,
                '00',10,r_sendtime_appointment,sysdate,IN_UserID,r_docno,'LT','',r_isPack,r_waveno,'N','',''
                ,'','','','','',r_tozone,'','',r_toloctype,r_tolocmode,'','in_type:3',r_SEND_FLAG);
             ---
              --写入日志
              insert into idx_asrs_send_log (organizationid,warehouseid,grouptaskid,palletid,status,addtime,addwho,notes)
                             values     (IN_organizationId,IN_Warehouse,r_GROUPTASKID,r_IN_TRACEID,'05',sysdate,IN_UserID,'');
             r_in_taskflag:='N';
             commit;

        end if;

           r_in_taskflag:='Y'; --交用实际任务下发
          commit;
        else
         r_in_taskflag:='Y'; --交用实际任务下发
     end if;
   end if;
  end if;
end if;
--***************************************************** in_type  = 4 移库/上架完成，扫描货架产生回库指令***********************************************
--应用场景在kiva货架的回库
 if IN_TYPE = '4' then
  if nvl(IN_FMLOCATION,'*') = '*' then
     OUT_Return_Code:='999#engine(4): 库位不能为空';
     rollback;
      return;
  end if;
  if nvl(r_IN_TRACEID,'*') = '*' then
     OUT_Return_Code:='999#engine(4): 托盘号/货架号不能为空';
     rollback;
      return;
  end if;
  select  count(*) into r_nrow from bas_location l where l.organizationid = IN_organizationId and l.warehouseid = IN_Warehouse
   and l.locationid = IN_FMLOCATION;
  if r_nrow = 0 then
       OUT_Return_Code:='999#engine(4): 原货位:'||IN_FMLOCATION||'不合法，请检查';
           rollback;
           return;
  end if;
   --只限货架回库操作
   begin
   select l.locationcategory into r_fmloctype
    from bas_location l where l.locationid = IN_FMLOCATION and l.organizationid = IN_organizationId and l.warehouseid = IN_Warehouse;
   exception
     when no_data_found then
         OUT_Return_Code:='999#engine(4): 请检查货位是否正确';
         rollback;
         return;
     end;

   --重复任务检查
    select count(0) into r_nrow from idx_asrs_send a where a.organizationid = IN_organizationId and a.warehouseid = IN_Warehouse
     and a.fmlocation = IN_FMLOCATION and a.palletid = r_IN_TRACEID and a.taskstatus < '81' ;
    if r_nrow > 0 then
        OUT_Return_Code:='999#engine(4): 已有任务在运行，不可重复下发';
        rollback;
        return;
    end if;

   --货架周转位处理去向
   --如果拣货没有完成发起的移动回库，需要再插入一条任务等待下发
  if r_fmloctype = 'ZZW' then
    --查询当前任务状态
    begin
    select 1,a.grouptaskid,a.tasktype,a.linkpalletid,a.tozone,a.toloctype
        into r_nrow,r_GROUPTASKID,r_tasktype,r_linkpalletid,r_tozone,r_toloctype
     from idx_asrs_send a where a.organizationid = IN_organizationId and a.warehouseid = IN_Warehouse
     and a.tolocation = IN_FMLOCATION and a.palletid = r_IN_TRACEID and a.taskstatus = '81' and a.tasktype in  ('V_MV','MV','V_PA','PA')
     and rownum = 1 ;
    exception
      when no_data_found then
        r_nrow := 0 ;
      end;
    if r_nrow = 0 then
        OUT_Return_Code:='999#engine(4): 识别到托盘/货架没有到达对应位置，请检查';
        rollback;
        return;
    end if;
    if nvl(r_linkpalletid,'*') <> '*'  then --移库入、上架 会关联原始托盘
         --移库入任可必须完成才可回库
         select count(0) into r_nrow from inv_lot_loc_id iv
         inner join bas_location l on iv.organizationid = l.organizationid and iv.warehouseid = l.warehouseid and iv.locationid = l.locationid
         where l.udf01 = r_IN_TRACEID  and ( iv.qtymvin > 0 or iv.qtypa > 0 )
         and iv.traceid in (
           --只取到达接驳位对应托盘号
           select ias.palletid from idx_asrs_send ias where ias.organizationid = IN_organizationId and ias.warehouseid = IN_Warehouse
           and exists(
             select 1 from idx_asrs_send a where a.organizationid = ias.organizationid and a.warehouseid = ias.warehouseid
              and a.tozone = ias.tozone and a.tolocmode = ias.tolocmode and a.taskstatus = '81'
              and a.tolocation = IN_FMLOCATION and a.palletid = r_IN_TRACEID
              )  and ias.taskstatus = '81' and ias.linkpalletid is null
          );
         if r_nrow > 0 then
            OUT_Return_Code:='999#engine(4): 移入任务必须完成才可以回库';
            rollback;
            return;
         end if;
          --下发回库任务
          --取货架原始库位
          begin
            select iv.locationid into r_tolocation from inv_lot_loc_id iv where iv.warehouseid = IN_Warehouse
             and iv.organizationid = IN_organizationId and iv.traceid = r_IN_TRACEID and iv.qty > 0
             and iv.sku = 'KIVASHELF' and rownum = 1  ;
          exception
            when no_data_found then
               OUT_Return_Code:='999#engine(4): 回库货架计算目标位失败，请检查';
               rollback;
               return;
            end;
          --下发回库任务
           spcom_getidsequence(IN_organizationId,IN_Warehouse,IN_Language,'GROUPTASKID',r_GROUPTASKID,out_return_code);
             if substr(OUT_Return_Code,1,3) <> '000' then
               rollback;
               return;
              end if;
           insert into idx_asrs_send (organizationid,warehouseid,grouptaskid,tasktype,palletid,fmlocation,tolocation,tolocation_WCS,
                 taskstatus,priority,sendtime_appointment,addtime,addwho,docno,equiptype,businesstype,ispackage,waveno,emptyflag,palletdetails,tolocation_type
                 ,fmzone,fmlocgrp2,fmloctype_wcs,fmloctype,fmlocmode,tozone,tolocgrp2,toloctype_wcs,toloctype,tolocmode,notes,udf05,edisendflag)
             values (IN_organizationId,IN_Warehouse,r_GROUPTASKID,r_tasktype,r_IN_TRACEID,IN_FMLOCATION,r_tolocation,r_tolocation,
                '00',10,r_sendtime_appointment,sysdate,IN_UserID,'','LT','','','','N','',''
                ,'','','',r_fmloctype,'','','','','','','货架回库','in_type:4',r_SEND_FLAG);
                --写入日志
          insert into idx_asrs_send_log (organizationid,warehouseid,grouptaskid,palletid,status,addtime,addwho,notes)
                             values     (IN_organizationId,IN_Warehouse,r_GROUPTASKID,r_IN_TRACEID,'00',sysdate,IN_UserID,'货架回库');
          commit;
      else --移库出
    --查询货架对应货位的移库是否有完成
    select count(0),max(iv.locationid) into r_nrow,r_fmlocation from inv_lot_loc_id iv
    inner join bas_location l on iv.organizationid = l.organizationid and iv.warehouseid = l.warehouseid and iv.locationid = l.locationid
    where l.udf01 = r_IN_TRACEID and iv.qty > 0 and  iv.qtymvout > 0 ;
    --如果还有移入任务，回库任需要再下发一条待拣货货架移出任务
        if r_nrow > 0 then
            --重新下发新任务
            spcom_getidsequence(IN_organizationId,IN_Warehouse,IN_Language,'GROUPTASKID',r_GROUPTASKID_NEW,out_return_code);
             if substr(OUT_Return_Code,1,3) <> '000' then
               rollback;
               return;
              end if;
           insert into idx_asrs_send (organizationid,warehouseid,grouptaskid,tasktype,palletid,fmlocation,tolocation,tolocation_WCS,
                 taskstatus,priority,sendtime_appointment,addtime,addwho,docno,equiptype,businesstype,ispackage,waveno,emptyflag,palletdetails,tolocation_type
                 ,fmzone,fmlocgrp2,fmloctype_wcs,fmloctype,fmlocmode,tozone,tolocgrp2,toloctype_wcs,toloctype,tolocmode,notes,udf05,edisendflag)
             values (IN_organizationId,IN_Warehouse,r_GROUPTASKID_NEW,r_tasktype,r_IN_TRACEID,r_fmlocation,'','',
                '00',10,r_sendtime_appointment,sysdate,IN_UserID,'','LT','','','','N','',''
                ,'','','','','',r_tozone,'','','ZZW','OUT','','in_type:4',r_SEND_FLAG);
                 --写入日志
          insert into idx_asrs_send_log (organizationid,warehouseid,grouptaskid,palletid,status,addtime,addwho,notes)
                             values     (IN_organizationId,IN_Warehouse,r_GROUPTASKID,r_IN_TRACEID,'00',sysdate,IN_UserID,'');
        end if;
          --下发回库任务
          --取货架原始库位
          begin
            select iv.locationid into r_tolocation from inv_lot_loc_id iv where iv.warehouseid = IN_Warehouse
             and iv.organizationid = IN_organizationId and iv.traceid = r_IN_TRACEID and iv.qty > 0
             and iv.sku = 'KIVASHELF' and rownum = 1  ;
          exception
            when no_data_found then
               OUT_Return_Code:='999#engine(4): 回库货架计算目标位失败，请检查';
               rollback;
               return;
            end;
           spcom_getidsequence(IN_organizationId,IN_Warehouse,IN_Language,'GROUPTASKID',r_GROUPTASKID,out_return_code);
             if substr(OUT_Return_Code,1,3) <> '000' then
               rollback;
               return;
              end if;
           insert into idx_asrs_send (organizationid,warehouseid,grouptaskid,tasktype,palletid,fmlocation,tolocation,tolocation_WCS,
                 taskstatus,priority,sendtime_appointment,addtime,addwho,docno,equiptype,businesstype,ispackage,waveno,emptyflag,palletdetails,tolocation_type
                 ,fmzone,fmlocgrp2,fmloctype_wcs,fmloctype,fmlocmode,tozone,tolocgrp2,toloctype_wcs,toloctype,tolocmode,notes,udf05,edisendflag)
             values (IN_organizationId,IN_Warehouse,r_GROUPTASKID,'V_MV',r_IN_TRACEID,IN_FMLOCATION,r_tolocation,r_tolocation,
                '00',10,r_sendtime_appointment,sysdate,IN_UserID,'','LT','','','','N','',''
                ,'','','','','','','','','','','货架回库','in_type:4',r_SEND_FLAG);
                  --写入日志
          insert into idx_asrs_send_log (organizationid,warehouseid,grouptaskid,palletid,status,addtime,addwho,notes)
                             values     (IN_organizationId,IN_Warehouse,r_GROUPTASKID,r_IN_TRACEID,'00',sysdate,IN_UserID,'货架回库');
        commit;
    end if;
    else
       OUT_Return_Code:='999#engine(4): '||IN_FMLOCATION||' 货位不是周转库位，请检查';
       rollback;
       return;
   end if;
   goto to_send;
  end if;
--***************************************************** in_type  = 5 盘点完成，盘点完扫描托盘或货架产生回库指令****************************************
if IN_TYPE = '5' then
   if nvl(IN_FMLOCATION,'*') = '*' then
     OUT_Return_Code:='999#engine(5): 库位不能为空';
     rollback;
      return;
  end if;
  if nvl(r_IN_TRACEID,'*') = '*' then
     OUT_Return_Code:='999#engine(5): 托盘号/货架号不能为空';
     rollback;
      return;
  end if;
  select  count(*) into r_nrow from bas_location l where l.organizationid = IN_organizationId and l.warehouseid = IN_Warehouse
   and l.locationid = IN_FMLOCATION;
  if r_nrow = 0 then
       OUT_Return_Code:='999#engine(5): 原货位:'||IN_FMLOCATION||'不合法，请检查';
           rollback;
           return;
  end if;
  begin
  select l.locationcategory into r_fmloctype  from bas_location l where l.organizationid = IN_organizationId and l.warehouseid = IN_Warehouse and l.locationid = IN_FMLOCATION
  and l.locationcategory in ('ZZW','PK') ; --货架周转位、在线拣选工位
  exception
    when no_data_found then
        OUT_Return_Code:='999#engine(5): 原货位不是 货架周转位、在线拣选工位';
        rollback;
        return;
    end;
  --重复任务检查
    select count(0) into r_nrow from idx_asrs_send a where a.organizationid = IN_organizationId and a.warehouseid = IN_Warehouse
     and a.fmlocation = IN_FMLOCATION and a.palletid = r_IN_TRACEID and a.taskstatus < '81' ;
    if r_nrow > 0 then
        OUT_Return_Code:='999#engine(5): 已有任务在运行，不可重复下发';
        rollback;
        return;
    end if;

   --货架周转位处理去向
   --如果盘点没有完成发起的移动回库，需要再插入一条任务等待下发
  if r_fmloctype = 'ZZW' then
    --查询当前任务状态
    select count(0),max(a.grouptaskid) into r_nrow,r_GROUPTASKID from idx_asrs_send a where a.organizationid = IN_organizationId and a.warehouseid = IN_Warehouse
     and a.tolocation = IN_FMLOCATION and a.palletid = r_IN_TRACEID and a.taskstatus = '81' and a.tasktype in  ('V_CC','CC') ;
    if r_nrow = 0 then
        OUT_Return_Code:='999#engine(5): 识别到任务没有到达对应位置，请检查';
        rollback;
        return;
    end if;
    --查询货架对应货位的盘点任务是否有完成

     select count(0) into r_nrow  from tsk_tasklists tk
     inner join bas_location l on tk.fmlocation = l.locationid and tk.organizationid = l.organizationid and tk.warehouseid = l.warehouseid
     where l.udf01 = r_IN_TRACEID and tk.taskprocess = '00' and tk.tasktype = 'CC' and rownum = 1
       and tk.organizationid = IN_organizationId and tk.warehouseid = IN_Warehouse ;
     if r_nrow > 0 then
         OUT_Return_Code:='999#engine(5): 货架没有完成盘点，请检查';
              rollback;
              return;
     end if;
     /*
    --如果还有盘点任务，回库任需要再下发一条待盘点货架移出任务
    if r_nrow > 0 then

            --重新下发新任务
            spcom_getidsequence(IN_organizationId,IN_Warehouse,IN_Language,'GROUPTASKID',r_GROUPTASKID_NEW,out_return_code);
             if substr(OUT_Return_Code,1,3) <> '000' then
               rollback;
               return;
              end if;
            --取货架原始位置
            begin
              select iv.locationid into r_fmlocation  from inv_lot_loc_id iv where iv.traceid = r_IN_TRACEID and iv.organizationid = IN_organizationId and iv.warehouseid = IN_Warehouse
              and iv.qty > 0 and iv.sku = 'KIVASHELF' and rownum = 1;
            exception
              when no_data_found then
                 OUT_Return_Code:='999# SPCUS_TASKGROUP_engine(5): 货架原始位置没定位到，请检查';
                 rollback;
                 return;
              end;
           insert into idx_asrs_send (organizationid,warehouseid,grouptaskid,tasktype,palletid,fmlocation,tolocation,tolocation_WCS,
                 taskstatus,priority,sendtime_appointment,addtime,addwho,docno,equiptype,businesstype,ispackage,waveno,emptyflag,palletdetails,tolocation_type
                 ,fmzone,fmlocgrp2,fmloctype_wcs,fmloctype,fmlocmode,tozone,tolocgrp2,toloctype_wcs,toloctype,tolocmode,notes,udf05,edisendflag)
             values (IN_organizationId,IN_Warehouse,r_GROUPTASKID_NEW,'V_CC',r_IN_TRACEID,IN_FMLOCATION,r_fmlocation,r_fmlocation,
                '00',10,r_sendtime_appointment,sysdate,IN_UserID,r_docno,'LT','','',r_waveno,'N','',''
                ,r_tozone,'','','ZZW','',r_tozone,'','','','','','in_type:5',r_SEND_FLAG);
                    --写入日志
          insert into idx_asrs_send_log (organizationid,warehouseid,grouptaskid,palletid,status,addtime,addwho,notes)
                             values     (IN_organizationId,IN_Warehouse,r_GROUPTASKID,r_IN_TRACEID,'00',sysdate,IN_UserID,'');
    end if;*/

          --下发回库任务
          --取货架原始库位
          begin
            select iv.locationid into r_tolocation from inv_lot_loc_id iv where iv.warehouseid = IN_Warehouse
             and iv.organizationid = IN_organizationId and iv.traceid = r_IN_TRACEID and iv.qty > 0
             and iv.sku = 'KIVASHELF' and rownum = 1  ;
          exception
            when no_data_found then
               OUT_Return_Code:='999#engine(5): 回库货架计算目标位失败，请检查';
               rollback;
               return;
            end;
           spcom_getidsequence(IN_organizationId,IN_Warehouse,IN_Language,'GROUPTASKID',r_GROUPTASKID,out_return_code);
             if substr(OUT_Return_Code,1,3) <> '000' then
               rollback;
               return;
              end if;
           insert into idx_asrs_send (organizationid,warehouseid,grouptaskid,tasktype,palletid,fmlocation,tolocation,tolocation_WCS,
                 taskstatus,priority,sendtime_appointment,addtime,addwho,docno,equiptype,businesstype,ispackage,waveno,emptyflag,palletdetails,tolocation_type
                 ,fmzone,fmlocgrp2,fmloctype_wcs,fmloctype,fmlocmode,tozone,tolocgrp2,toloctype_wcs,toloctype,tolocmode,notes,udf05,edisendflag)
             values (IN_organizationId,IN_Warehouse,r_GROUPTASKID,'V_CC',r_IN_TRACEID,IN_FMLOCATION,r_tolocation,r_tolocation,
                '00',10,r_sendtime_appointment,sysdate,IN_UserID,r_docno,'LT','','',r_waveno,'N','',''
                ,'','','','','','','','','','','货架回库','in_type:5',r_SEND_FLAG);
                  --写入日志
          insert into idx_asrs_send_log (organizationid,warehouseid,grouptaskid,palletid,status,addtime,addwho,notes)
                             values     (IN_organizationId,IN_Warehouse,r_GROUPTASKID,r_IN_TRACEID,'00',sysdate,IN_UserID,'货架回库');
        commit;
  end if;
   --在线拣选工位盘点后处理去向
  if r_fmloctype in ('PK') then
      --先看盘点是否完成
       select count(0) into r_nrow from tsk_tasklists tk where tk.warehouseid = IN_Warehouse and tk.organizationid = IN_organizationId
       and tk.fmid = r_IN_TRACEID and tk.taskprocess = '00' and tk.tasktype = 'CC' ;
       if r_nrow > 0 then
              OUT_Return_Code:='999#engine(5): 托盘没有完成盘点，请检查';
              rollback;
              return;
       end if;
       --查询当前任务状态
    select count(0),max(a.grouptaskid) into r_nrow,r_GROUPTASKID from idx_asrs_send a where a.organizationid = IN_organizationId
     and a.warehouseid = IN_Warehouse  and a.tolocation = IN_FMLOCATION and a.palletid = r_IN_TRACEID and a.taskstatus = '81' and a.tasktype in  ('V_CC','CC') ;
    if r_nrow = 0 then
        OUT_Return_Code:='999#engine(5): 识别到托盘/货架没有到达对应位置，请检查';
        rollback;
        return;
    end if;
    --查询是否已生成回库任务
      select count(0) into r_nrow from inv_lot_loc_id iv where iv.organizationid = IN_organizationId and iv.warehouseid = IN_Warehouse
      and iv.traceid = r_IN_TRACEID and iv.qtymvin > 0 and iv.qty = 0 and iv.locationid = IN_FMLOCATION and iv.locationid not like 'SORTATION%';
     --如果没有产生任务，回原库位
     if r_nrow = 0 then

        --查托盘原货位
          begin
            select iv.locationid into r_tolocation from inv_lot_loc_id iv where iv.organizationid = IN_organizationId and iv.warehouseid = IN_Warehouse
            and iv.traceid = r_IN_TRACEID  and iv.qty > 0  and iv.locationid <> IN_FMLOCATION  and iv.locationid not like 'SORTATION%' and rownum = 1;
           exception
            when no_data_found then
               OUT_Return_Code:='999#engine(5): 没找到托盘原始库位，请检查';
               rollback;
               return;
            end;
           --如果对应双深位还有任务未完成，生成占位移动3任务
           select count(0) into r_nrow from idx_asrs_send ias
           where ias.organizationid = IN_organizationId and ias.warehouseid = IN_Warehouse
            and ias.fmlocation in (
             select bl1.locationid from bas_location bl1 where  bl1.organizationid = IN_organizationId
             and bl1.warehouseid = IN_Warehouse and  bl1.udf01 in (
               select bl.udf01 from bas_location bl where bl.locationid = r_tolocation and bl.organizationid = IN_organizationId
               and bl.warehouseid = IN_Warehouse and nvl(bl.udf01,'*') <> '*')
              ) and ias.taskstatus < '70' ;
           if r_nrow > 0 then

           --计算上架库位
            SIRIO_SPCUS_Calc_loc
            (IN_organizationId  ,
            IN_Warehouse      ,
            r_tolocation       ,
            r_IN_TRACEID          , --托盘号
            ''              , --SKU
            r_putloction        ,
            r_model             , --LOC 货位，LOG 货架
            'DATAHUB'           ,
            IN_Language        ,
            OUT_Return_Code
            );
           if  substr(OUT_Return_Code,1,3) <> '000' then
                 rollback;
                 return;
            end if;
         if  nvl(r_putloction,'*')  in ('*','NO-LOC') then
             OUT_Return_Code:='999#engine(5): 未能计算到上架货位，请检查';
               rollback;
               return;
         end if;
         r_tolocation:=r_putloction;
         /*OUT_Return_Code := '*_*';
         SIRIO_SPCUS_getMOVETASK
         (IN_organizationId,
          IN_Warehouse,
          r_IN_TRACEID, --托盘号
          r_tolocation, --目标库位，留空时调用目标库位计算逻辑
          '', --移动类型，区分是否需要生成移动单
          '', --预留
          '', --预留
          'Y', --预留,是否允许下发
          IN_UserID,
          IN_Language,
          OUT_Return_Code
          );
         if substr(OUT_Return_Code,1,3) <> '000' then
                 OUT_Return_Code:='999# SPCUS_TASKGROUP_engine(5): 生成上架/移动任务失败,错误信息：'||OUT_Return_Code;
                 rollback;
                 return;
         end if;
          */
           r_tasktype:='V_MV3';
         else
           r_tasktype:='V_MV';
        end if;

            spcom_getidsequence(IN_organizationId,IN_Warehouse,IN_Language,'GROUPTASKID',r_GROUPTASKID,out_return_code);
             if substr(OUT_Return_Code,1,3) <> '000' then
               rollback;
               return;
              end if;
             insert into idx_asrs_send (organizationid,warehouseid,grouptaskid,tasktype,palletid,fmlocation,tolocation,tolocation_WCS,
                 taskstatus,priority,sendtime_appointment,addtime,addwho,docno,equiptype,businesstype,ispackage,waveno,emptyflag,palletdetails,tolocation_type
                 ,fmzone,fmlocgrp2,fmloctype_wcs,fmloctype,fmlocmode,tozone,tolocgrp2,toloctype_wcs,toloctype,tolocmode,notes,udf05,edisendflag)
             values (IN_organizationId,IN_Warehouse,r_GROUPTASKID,r_tasktype,r_IN_TRACEID,IN_FMLOCATION,r_tolocation,r_tolocation,
                '00',10,r_sendtime_appointment,sysdate,IN_UserID,r_docno,'LT','','',r_waveno,'N','',''
                ,'','','','','','','','','','','','in_type:5',r_SEND_FLAG);
             ---
                    --写入日志
             insert into idx_asrs_send_log (organizationid,warehouseid,grouptaskid,palletid,status,addtime,addwho,notes)
                             values     (IN_organizationId,IN_Warehouse,r_GROUPTASKID,r_IN_TRACEID,'00',sysdate,IN_UserID,'');

          commit;
       else
         r_in_taskflag :='Y';
         goto to_send;
     end if;
   end if;

 end if;
--***************************************************** in_type  = 6 配盘完成，配盘好的货产生去缓存区暂存指令******************************************
if IN_TYPE = '6' then
  if nvl(IN_FMLOCATION,'*') = '*' then
     OUT_Return_Code:='999#engine(6): 库位不能为空';
     rollback;
      return;
  end if;
  if nvl(r_IN_TRACEID,'*') = '*' then
     OUT_Return_Code:='999#engine(6): 托盘号不能为空';
     rollback;
      return;
  end if;
    select  count(*) into r_nrow from bas_location l where l.organizationid = IN_organizationId and l.warehouseid = IN_Warehouse
   and l.locationid = IN_FMLOCATION;
  if r_nrow = 0 then
       OUT_Return_Code:='999#engine(6): 原货位:'||IN_FMLOCATION||'不合法，请检查';
           rollback;
           return;
  end if;

  --重复任务检查
    select count(0) into r_nrow from idx_asrs_send a where a.organizationid = IN_organizationId and a.warehouseid = IN_Warehouse
     and a.fmlocation = IN_FMLOCATION and a.palletid = r_IN_TRACEID and a.taskstatus < '81' ;
    if r_nrow > 0 then
        OUT_Return_Code:='999#engine(6): 已有任务在运行，不可重复下发';
        rollback;
        return;
    end if;


   begin
  select decode(l.zoneid,'22','TL','23','PP')
        ,z.udf03,z.udf04
    into r_fmloctype,r_direction,r_direction_zone
  from bas_location l
   left join bas_zone z on l.zoneid = z.zoneid and l.organizationid = z.organizationid and l.warehouseid = z.warehouseid
  where l.organizationid = IN_organizationId and l.warehouseid = IN_Warehouse and l.locationid = IN_FMLOCATION
    and l.zoneid in ('22','23');
  --and (l.locationcategory in ('PP','TL')
  --  or IN_FMLOCATION in ('30230001','30230008','30230014','30230021')  --配盘接驳位
  --); --配盘工位,退料配盘位
  exception
    when no_data_found then
        OUT_Return_Code:='999#engine(6): 原货位不是配盘工位';
        rollback;
        return;
    end;
  --配盘 ==> 缓存
  if r_fmloctype  = 'PP' then
     --检查去向合理性
     if r_direction <> 'BUF' then
        OUT_Return_Code:='999#engine(6): 配盘去向只能为缓存';
        rollback;
        return;
      end if;
      if r_direction_zone is null then
        OUT_Return_Code:='999#engine(6): 去向目标区不能为空';
        rollback;
        return;
      end if;
          begin
           select h.hedi12,h.hedi13,h.waveno,h.orderno ,null
                 into r_docJBW,r_docout,r_waveno,r_docno ,r_sendtime_appointment
             from doc_order_header h
              inner join act_allocation_details ad on h.organizationid = ad.organizationid and h.warehouseid = ad.warehouseid and h.orderno = ad.orderno
             where ad.picktotraceid = r_IN_TRACEID and ad.status = '60' and ad.addtime >= sysdate -r_dateint
                and h.organizationid = IN_organizationId and h.warehouseid = IN_Warehouse and rownum = 1;
           exception
             when no_data_found then
                OUT_Return_Code:='999#engine(6): 托盘号不能定位到拣货定单，请检查';
                rollback;
                 return;
             end;
            spcom_getidsequence(IN_organizationId,IN_Warehouse,IN_Language,'GROUPTASKID',r_GROUPTASKID,out_return_code);
             if substr(OUT_Return_Code,1,3) <> '000' then
               rollback;
               return;
              end if;
             insert into idx_asrs_send (organizationid,warehouseid,grouptaskid,tasktype,palletid,fmlocation,tolocation,tolocation_WCS,
                 taskstatus,priority,sendtime_appointment,addtime,addwho,docno,equiptype,businesstype,ispackage,waveno,emptyflag,palletdetails,tolocation_type
                 ,fmzone,fmlocgrp2,fmloctype_wcs,fmloctype,fmlocmode,tozone,tolocgrp2,toloctype_wcs,toloctype,tolocmode,notes,udf05,edisendflag)
             values (IN_organizationId,IN_Warehouse,r_GROUPTASKID,'V_MV',r_IN_TRACEID,IN_FMLOCATION,'','',
                '00',10,r_sendtime_appointment,sysdate,IN_UserID,r_docno,'LT','','',r_waveno,'N','',''
                ,'','','','','',r_direction_zone,'','',r_direction,'','','in_type:6',r_SEND_FLAG);
             ---
                    --写入日志
          insert into idx_asrs_send_log (organizationid,warehouseid,grouptaskid,palletid,status,addtime,addwho,notes)
                             values     (IN_organizationId,IN_Warehouse,r_GROUPTASKID,r_IN_TRACEID,'00',sysdate,IN_UserID,'');
             commit;
   end if;
  --退料回库
  if r_fmloctype = 'TL' then
       OUT_Return_Code:='999#engine(6): 退料请用自由移动，请检查';
        rollback;
        return;
     --查生是否有库存
     select count(0) into r_nrow from inv_lot_loc_id iv where iv.organizationid = IN_organizationId
     and iv.warehouseid = IN_Warehouse and iv.traceid = r_IN_TRACEID;
     if r_nrow = 0 then
        OUT_Return_Code:='999#engine(6): 退料托盘没有收货，请检查';
        rollback;
        return;
     end if;
     --查询是否有生成移库或上架任务
     select count(0) into r_nrow from inv_lot_loc_id iv where iv.organizationid = IN_organizationId
     and iv.warehouseid = IN_Warehouse and iv.traceid = r_IN_TRACEID and iv.qty = 0 and (iv.qtymvin+iv.qtypa) > 0  ;
     if r_nrow = 0 then
        OUT_Return_Code:='999#engine(6): 退料托盘没有收货，请检查';
        rollback;
        return;
       else
         --生成移库任务
          --调用生成移库或上架任务
         OUT_Return_Code:='*_*';
         if nvl(IN_TOLOCATION,'*') = '*'  then
         --计算上架库位
                   SIRIO_SPCUS_Calc_loc
                     (IN_organizationId  ,
                      IN_Warehouse      ,
                      ''       ,
                      r_IN_TRACEID          , --托盘号
                      ''              , --SKU
                      r_tolocation        ,
                      r_model             , --LOC 货位，LOG 货架
                      'DATAHUB'           ,
                      IN_Language         ,
                      OUT_Return_Code
                      );
                   if  substr(OUT_Return_Code,1,3) <> '000' then
                       rollback;
                       return;
                   end if;
         else
            --如果有传入货位，测不再计算上架位
            r_tolocation := IN_TOLOCATION;
         end if;
         --loc1 为立库，立库才生成上架任务
         if r_model = 'LOC1' then
         --
         SIRIO_SPCUS_getMOVETASK
         (IN_organizationId,
          IN_Warehouse,
          r_IN_TRACEID, --托盘号
          r_tolocation, --目标库位，留空时调用目标库位计算逻辑
          '', --移动类型，区分是否需要生成移动单
          '', --预留
          '', --预留
          'Y', --预留
          IN_UserID,
          IN_Language,
          OUT_Return_Code
          );
         if substr(OUT_Return_Code,1,3) <> '000' then
           OUT_Return_Code:='999#engine(6): 生成上架/移动任务失败：'||OUT_Return_Code;
           rollback;
           return;
         end if;
         end if;
           --先下发一个实体任务，任务tsk_tasklists 下发时会反写当前 GROUPTASKID 到任务表，目的实现从当前点位置进行移动，因为收货可能是在STAGE货位，不具备设备移动条件
           spcom_getidsequence(IN_organizationId,IN_Warehouse,IN_Language,'GROUPTASKID',r_GROUPTASKID,out_return_code);
             if substr(OUT_Return_Code,1,3) <> '000' then
               rollback;
               return;
              end if;
          --查询任务对应的类型
          begin
          select tk.tasktype into r_tasktype from tsk_tasklists tk where tk.organizationid = IN_organizationId and tk.warehouseid = IN_Warehouse
           and tk.fmid = r_IN_TRACEID and tk.plantolocation = r_tolocation and nvl(tk.grouptaskid_wcs,'*') = '*' and rownum = 1
           and tk.taskprocess = '00';
          exception
            when no_data_found then
               OUT_Return_Code:='999#engine(6): 无法获取任务类型';
               rollback;
                return;
            end;
             insert into idx_asrs_send (organizationid,warehouseid,grouptaskid,tasktype,palletid,fmlocation,tolocation,tolocation_WCS,
                 taskstatus,priority,sendtime_appointment,addtime,addwho,docno,equiptype,businesstype,ispackage,waveno,emptyflag,palletdetails,tolocation_type
                 ,fmzone,fmlocgrp2,fmloctype_wcs,fmloctype,fmlocmode,tozone,tolocgrp2,toloctype_wcs,toloctype,tolocmode,notes,udf05,edisendflag)
             values (IN_organizationId,IN_Warehouse,r_GROUPTASKID,r_tasktype,r_IN_TRACEID,IN_FMLOCATION,'','',
                '00',10,r_sendtime_appointment,sysdate,IN_UserID,r_docno,'LT','','',r_waveno,'N','',''
                ,'','','','','','','','','','','','in_type:6',r_SEND_FLAG);
             ---
                    --写入日志
          insert into idx_asrs_send_log (organizationid,warehouseid,grouptaskid,palletid,status,addtime,addwho,notes)
                             values     (IN_organizationId,IN_Warehouse,r_GROUPTASKID,r_IN_TRACEID,'00',sysdate,IN_UserID,'');
             commit;
         r_in_taskflag:='Y';
     end if;
  end if;
end if;
--***************************************************** in_type  = 7 增加占位，手动放托盘在接驳位******************************************************
if IN_TYPE = '7' then
   if nvl(IN_FMLOCATION,'*') = '*' then
     OUT_Return_Code:='999#engine(7): 库位不能为空';
     rollback;
      return;
    end if;
    if nvl(r_IN_TRACEID,'*') = '*' then
     OUT_Return_Code:='999#engine(7): 托盘号不能为空';
     rollback;
      return;
    end if;
    select  count(*),max(l.zoneid) into r_nrow,r_fmzone from bas_location l where l.organizationid = IN_organizationId and l.warehouseid = IN_Warehouse
   and l.locationid = IN_FMLOCATION and l.locationattribute = 'OK';
  if r_nrow = 0 then
       OUT_Return_Code:='999#engine(7): 原货位:'||IN_FMLOCATION||'不合法，请检查';
           rollback;
           return;
  end if;

  if r_fmzone  = '3FRESERVEDSTATION02' then
  select count(0) into r_nrow from inv_lot_loc_id iv where iv.organizationid = IN_organizationId and iv.warehouseid = IN_Warehouse
   and iv.traceid = r_IN_TRACEID and (iv.qtyallocated+iv.qtymvout+iv.qtyrpout) > 0 ;
   if r_nrow > 0 then
       OUT_Return_Code:='999#engine(7): 托盘: '||r_IN_TRACEID||' 有拣货或移动任务，请勿占位';
           rollback;
           return;
   end if;
  end if;


  --如果是3楼成品抽样的JBW ，不能进行非立库的占位
    if r_fmzone = '3FRESERVEDSTATION02' then
       --计算上架库位
         SIRIO_SPCUS_Calc_loc
         (IN_organizationId  ,
         IN_Warehouse      ,
         '3030I01'       ,
         r_IN_TRACEID          , --托盘号
         ''              , --SKU
         r_tolocation        ,
         r_model             , --LOC 货位，LOG 货架
         'DATAHUB'           ,
         IN_Language        ,
         OUT_Return_Code
         );
         if  substr(OUT_Return_Code,1,3) <> '000' then
              rollback;
              return;
         end if;

        if r_model <> 'LOC1' then
           OUT_Return_Code:='999#engine(7): 托盘库存不可去立库，请勿占位';
           rollback;
           return;
        end if;

    end if;

   --查一下是否已下发任务
   select count(0) into r_nrow  from idx_asrs_send a where a.organizationid = IN_organizationId and a.warehouseid = r_IN_TRACEID and
    a.fmlocation = IN_FMLOCATION and a.palletid = r_IN_TRACEID  and a.tasktype = 'ADD';
    if r_nrow > 0 then
      OUT_Return_Code:='999#engine(7): 货位、托盘已占位下发';
       rollback;
      return;
    end if ;
   --查询当前是否已占位
   select count(0),max(l.traceid) into r_nrow,r_palletid from cus_buff_location l where l.organizationid = IN_organizationId and l.warehouseid = IN_Warehouse
   and l.locationid = IN_FMLOCATION ;--and l.traceid = r_IN_TRACEID;
    if r_nrow > 0 then
      OUT_Return_Code:='999#engine(7): 货位已被 '||r_palletid||' 占位,不可操作';
       rollback;
      return;
    end if ;
   --同一托盘只能占一个位
   select count(0),min(cb.locationid) into r_nrow,r_tolocation from cus_buff_location cb where cb.organizationid = IN_organizationId and cb.warehouseid = IN_Warehouse
   and cb.traceid = r_IN_TRACEID;
   if r_nrow > 0 then
       OUT_Return_Code:='999#engine(7): 托盘已占位:'||r_tolocation||',不可操作';
       rollback;
       return;
   end if;
   --托盘有任务在运行，不用占位
   select count(0) into r_nrow from idx_asrs_send ias where ias.organizationid = IN_organizationId and ias.warehouseid = IN_Warehouse
   and ias.palletid = r_IN_TRACEID;
   if r_nrow > 0 then
       OUT_Return_Code:='999#engine(7): 托盘已有任务在运行,不可操作';
       rollback;
       return;
   end if;


   --下发占位任务
         spcom_getidsequence(IN_organizationId,IN_Warehouse,IN_Language,'GROUPTASKID',r_GROUPTASKID,out_return_code);
             if substr(OUT_Return_Code,1,3) <> '000' then
               rollback;
               return;
              end if;
             insert into idx_asrs_send (organizationid,warehouseid,grouptaskid,tasktype,palletid,fmlocation,tolocation,tolocation_WCS,
                 taskstatus,priority,sendtime_appointment,addtime,addwho,docno,equiptype,businesstype,ispackage,waveno,emptyflag,palletdetails,tolocation_type
                 ,fmzone,fmlocgrp2,fmloctype_wcs,fmloctype,fmlocmode,tozone,tolocgrp2,toloctype_wcs,toloctype,tolocmode,notes,udf05,edisendflag)
             values (IN_organizationId,IN_Warehouse,r_GROUPTASKID,'ADD',r_IN_TRACEID,IN_FMLOCATION,IN_FMLOCATION,IN_FMLOCATION,
                '05',10,r_sendtime_appointment,sysdate,IN_UserID,r_docno,'LT','','',r_waveno,'N','',''
                ,'','','','','','','','','','','','in_type:7',r_SEND_FLAG);
             --- 状态用05立即占位
                       --写入日志
          insert into idx_asrs_send_log (organizationid,warehouseid,grouptaskid,palletid,status,addtime,addwho,notes)
                             values     (IN_organizationId,IN_Warehouse,r_GROUPTASKID,r_IN_TRACEID,'00',sysdate,IN_UserID,'');
             commit;
     goto to_send;
end if;
--***************************************************** in_type  = 8 释放占位，手动从接驳位搬走托盘****************************************************
if IN_TYPE = '8' then
   if nvl(IN_FMLOCATION,'*') = '*' then
     OUT_Return_Code:='999#engine(8): 库位不能为空';
     rollback;
      return;
    end if;
    if nvl(r_IN_TRACEID,'*') = '*' then
     OUT_Return_Code:='999#engine(8): 托盘号不能为空';
     rollback;
      return;
    end if;
  select  count(*) into r_nrow from bas_location l where l.organizationid = IN_organizationId and l.warehouseid = IN_Warehouse
   and l.locationid = IN_FMLOCATION ;
  if r_nrow = 0 then
       OUT_Return_Code:='999#engine(8): 原货位:'||IN_FMLOCATION||'不合法，请检查';
           rollback;
           return;
  end if;

  --3030A01,3030A02 洗盘异常库位
  if IN_FMLOCATION = r_IN_TRACEID then
    begin
     select ias.palletid into r_IN_TRACEID from idx_asrs_send ias where ias.tolocation = IN_FMLOCATION and ias.taskstatus = '81' and rownum = 1
     and ias.organizationid = IN_organizationId and ias.warehouseid = IN_Warehouse ;
    exception
      when no_data_found then
           OUT_Return_Code:='999#engine(8): 原货位:'||IN_FMLOCATION||'没有到达任务，请检查';
           rollback;
           return;
      end;
  end if;

  --如托盘有库存，限制释放
  /*select count(0) into r_nrow from inv_lot_loc_id iv
  left join bas_location bl on iv.organizationid = bl.organizationid and iv.warehouseid = bl.warehouseid and iv.locationid = bl.locationid
  where iv.traceid = r_IN_TRACEID
  and iv.organizationid = IN_organizationId and iv.warehouseid = IN_Warehouse
   and iv.qty > 0 and not exists (
     select 1 from bas_location l where l.organizationid = iv.organizationid and l.warehouseid = iv.warehouseid
     and l.locationid = IN_FMLOCATION and l.zoneid = bl.zoneid
   ) ;
   if r_nrow > 0 then
      OUT_Return_Code:='999# SPCUS_TASKGROUP_engine(8): 托盘上有非当前区域库存,请先处理';
       rollback;
       return;
   end if;*/

   --查一下是否已下发任务
   select count(0) into r_nrow  from idx_asrs_send a where a.organizationid = IN_organizationId and a.warehouseid = r_IN_TRACEID and
    a.fmlocation = IN_FMLOCATION and a.palletid = r_IN_TRACEID  and a.tasktype = 'DEL';
    if r_nrow > 0 then
      OUT_Return_Code:='999#engine(8): 货位、托盘已释放下发';
       rollback;
      return;
    end if ;

    --原货位有移出任务
    select count(0) into r_nrow from idx_asrs_send a where a.organizationid = IN_organizationId and a.warehouseid = r_IN_TRACEID and
     a.fmlocation = IN_FMLOCATION and a.taskstatus < '70'  and a.tasktype not in ( 'DEL','ADD','ERR-OUT');
    if r_nrow > 0 then
        OUT_Return_Code:='999#engine(8): 原货位存在移出任务未完成任务，请检查';
         rollback;
         return;
    end if;
    --目标货位有移入任务
    select count(0) into r_nrow from idx_asrs_send ias where ias.organizationid = IN_organizationId and ias.warehouseid = IN_Warehouse
      and ias.tolocation = IN_FMLOCATION and ias.palletid = r_IN_TRACEID  and ias.taskstatus < '81' and ias.tasktype not in ( 'DEL','ADD','ERR-OUT');
      if r_nrow > 0 then
        OUT_Return_Code:='999#engine(8): 目标货位存在移入未完成任务，请检查';
         rollback;
        return;
      end if;

  /* --查询当前是否已占位
   select count(0) into r_nrow from cus_buff_location l where l.organizationid = IN_organizationId and l.warehouseid = IN_Warehouse
   and l.locationid = IN_FMLOCATION and l.traceid = r_IN_TRACEID;
    if r_nrow = 0 then
     --再看是否有81的任务
        OUT_Return_Code:='999# SPCUS_TASKGROUP_engine(8): 该货位不存在占位，请检查';
         rollback;
        return;

    end if ;
  */
   --下发占位任务
         spcom_getidsequence(IN_organizationId,IN_Warehouse,IN_Language,'GROUPTASKID',r_GROUPTASKID,out_return_code);
             if substr(OUT_Return_Code,1,3) <> '000' then
               rollback;
               return;
              end if;
             insert into idx_asrs_send (organizationid,warehouseid,grouptaskid,tasktype,palletid,fmlocation,tolocation,tolocation_WCS,
                 taskstatus,priority,sendtime_appointment,addtime,addwho,docno,equiptype,businesstype,ispackage,waveno,emptyflag,palletdetails,tolocation_type
                 ,fmzone,fmlocgrp2,fmloctype_wcs,fmloctype,fmlocmode,tozone,tolocgrp2,toloctype_wcs,toloctype,tolocmode,notes,udf05,edisendflag)
             values (IN_organizationId,IN_Warehouse,r_GROUPTASKID,'DEL',r_IN_TRACEID,IN_FMLOCATION,IN_FMLOCATION,IN_FMLOCATION,
                '05',10,r_sendtime_appointment,sysdate,IN_UserID,r_docno,'LT','','',r_waveno,'N','',''
                ,'','','','','','','','','','','','in_type:8',r_SEND_FLAG);
             ---
                       --写入日志
          insert into idx_asrs_send_log (organizationid,warehouseid,grouptaskid,palletid,status,addtime,addwho,notes)
                             values     (IN_organizationId,IN_Warehouse,r_GROUPTASKID,r_IN_TRACEID,'00',sysdate,IN_UserID,'');
             commit;
     goto to_send;
end if;
--***************************************************** in_type  = 9 单个空托盘回收，回收至拆码盘机****************************************************
if IN_TYPE = 9 then
 if nvl(IN_FMLOCATION,'*') = '*' then
     OUT_Return_Code:='999#engine(9): 库位不能为空';
     rollback;
      return;
    end if;
  if IN_TRACEID is null then
     OUT_Return_Code:='999#engine(9): 托盘号不能为空';
       rollback;
      return;
 end if;
    select  count(*) into r_nrow from bas_location l where l.organizationid = IN_organizationId and l.warehouseid = IN_Warehouse
   and l.locationid = IN_FMLOCATION and l.locationcategory = 'PK';
  if r_nrow = 0 then
       OUT_Return_Code:='999#engine(9): 原货位:'||IN_FMLOCATION||'不合法，请检查(仅支持在线拣位发起空托盘回收)';
           rollback;
           return;
  end if;


   select count(0) into r_nrow from idx_asrs_send a where a.organizationid = IN_organizationId
     and a.warehouseid = IN_Warehouse  and a.tolocation = IN_FMLOCATION and a.palletid = r_IN_TRACEID and a.taskstatus = '81' ;
    if r_nrow = 0 then
        OUT_Return_Code:='999#engine(9): 识别到托盘/货架没有到达对应位置，请检查';
        rollback;
        return;
    end if;

 --是否有任务在执行
 select count(*) into r_nrow from idx_asrs_send ias where ias.organizationid = IN_organizationId and ias.warehouseid = IN_Warehouse
  and ias.palletid = r_IN_TRACEID and ias.taskstatus < '81';
 if r_nrow > 0 then
     OUT_Return_Code:='999#engine(9): 已有任务在运行，不可重复下发';
       rollback;
      return;
 end if;
 --查询是否为空托盘(是否有库存判断)
 select count(0) into r_nrow from inv_lot_loc_id iv where iv.organizationid = IN_organizationId and iv.warehouseid = IN_Warehouse
 and iv.traceid = r_IN_TRACEID and rownum = 1;-- and iv.locationid not like 'SORTATION%';
 if r_nrow = 0 then
    --查找临近的码盘机,后期改为固定位置 3000T01（与WCS约定）
     r_tolocation :='3000T01';
        spcom_getidsequence(IN_organizationId,IN_Warehouse,IN_Language,'GROUPTASKID',r_GROUPTASKID,out_return_code);
             if substr(OUT_Return_Code,1,3) <> '000' then
               rollback;
               return;
              end if;
             insert into idx_asrs_send (organizationid,warehouseid,grouptaskid,tasktype,palletid,fmlocation,tolocation,tolocation_WCS,
                 taskstatus,priority,sendtime_appointment,addtime,addwho,docno,equiptype,businesstype,ispackage,waveno,emptyflag,palletdetails,tolocation_type
                 ,fmzone,fmlocgrp2,fmloctype_wcs,fmloctype,fmlocmode,tozone,tolocgrp2,toloctype_wcs,toloctype,tolocmode,notes,udf05,edisendflag)
             values (IN_organizationId,IN_Warehouse,r_GROUPTASKID,'V_MV',r_IN_TRACEID,IN_FMLOCATION,r_tolocation,r_tolocation,
                '05',10,r_sendtime_appointment,sysdate,IN_UserID,r_docno,'LT','','',r_waveno,'N','',''
                ,'','','','','','','','','','','空托盘回收','in_type:9',r_SEND_FLAG);
             ---
                       --写入日志
          insert into idx_asrs_send_log (organizationid,warehouseid,grouptaskid,palletid,status,addtime,addwho,notes)
                             values     (IN_organizationId,IN_Warehouse,r_GROUPTASKID,r_IN_TRACEID,'10',sysdate,IN_UserID,'');
             commit;
     r_in_taskflag :='Y';
 else
      OUT_Return_Code:='999#engine(9): 非空托盘，不可生成移动任务';
      rollback;
      return;
 end if;
end if;
--***************************************************** in_type  = 10 人工组整垛空托盘回库，回立库上架存储*************************************************
if IN_TYPE = 10 then
 if   nvl(IN_TRACEID,'*') = '*' then
     OUT_Return_Code:='999#engine(11): 托盘号不能为空';
       rollback;
      return;
 end if;
 if nvl(IN_FMLOCATION,'*') = '*' then
     OUT_Return_Code:='999#engine(9): 库位不能为空';
     rollback;
      return;
  end if;
end if;
----**************************************************  in_type  = 11 任务取消 ************************************************************************
if IN_TYPE = 11 then
 if  nvl(IN_TRACEID,'*') = '*' then
     OUT_Return_Code:='999#engine(11): 托盘号不能为空';
       rollback;
      return;
 end if;
--处理指令
for r in (select ias.* from idx_asrs_send ias where ias.organizationid = IN_organizationId and ias.warehouseid = IN_Warehouse and ias.palletid = IN_TRACEID
          and ias.taskstatus <= 10 ) loop
     if (r.taskstatus < '10') or ( r.taskstatus = 10 and nvl(r.edisendflag,'N') = 'N' ) then
        update idx_asrs_send ias set ias.taskstatus = '90' where ias.organizationid = IN_organizationId and ias.warehouseid = IN_Warehouse
            and ias.palletid = IN_TRACEID and ias.grouptaskid = r.grouptaskid;
     else
       update idx_asrs_send ias set ias.calcel_flag = 'Y'  where ias.organizationid = IN_organizationId and ias.warehouseid = IN_Warehouse
            and ias.palletid = IN_TRACEID and ias.grouptaskid = r.grouptaskid;
     end if;
end loop;
 commit;
end if;

----**************************************************  in_type  = 12 任务取消 ************************************************************************
if IN_TYPE = 12 then
    if nvl(IN_FMLOCATION,'*') <> '*' then
       OUT_Return_Code:='999#engine(12): 原货位不用录入，请检查';
       rollback;
       return;
    end if;

    if nvl(IN_TOLOCATION,'*') <> '*' then
       OUT_Return_Code:='999#engine(12): 目标货位不用录入，请检查';
       rollback;
       return;
    end if;

    if nvl(r_IN_TRACEID,'*') = '*' then
       OUT_Return_Code:='999#engine(12): 请输入正确的货架号，请检查';
       rollback;
       return;
    end if;

    --检查是否为KIVA货位/货架
    select count(0) into r_nrow from  inv_lot_loc_id iv
    left join bas_location l on iv.organizationid = l.organizationid and iv.warehouseid = l.warehouseid and iv.locationid = l.locationid
    where iv.organizationid = IN_organizationId and iv.warehouseid = IN_Warehouse
     and iv.traceid = r_IN_TRACEID and l.zoneid in ('21','42') and iv.traceid <> '*' ; --KIVA区
     if r_nrow = 0 then
       OUT_Return_Code:='999#engine(12): 只能从KIVA区移出，请检查';
       rollback;
       return;
     end if;
    --查询是否有任务在运行
    select count(0) into r_nrow from idx_asrs_send ias where ias.organizationid = IN_organizationId and ias.warehouseid = IN_Warehouse
    and ias.palletid = r_IN_TRACEID;
    if r_nrow > 0 then
      OUT_Return_Code:='999#engine(12): 该货位还有未完成的任务，请检查';
       rollback;
       return;
    end if;


           r_tasktype:='V_MV';

            spcom_getidsequence(IN_organizationId,IN_Warehouse,IN_Language,'GROUPTASKID',r_GROUPTASKID_NEW,out_return_code);
             if substr(OUT_Return_Code,1,3) <> '000' then
               rollback;
               return;
              end if;
            --取货架原始位置
            begin
              select iv.locationid,l.zoneid into r_fmlocation ,r_tozone from inv_lot_loc_id iv
              left join bas_location l on iv.organizationid = l.organizationid and iv.warehouseid = l.warehouseid and iv.locationid = l.locationid
               where iv.traceid = r_IN_TRACEID and iv.sku = 'KIVASHELF'
              and iv.organizationid = IN_organizationId and iv.warehouseid = IN_Warehouse
              and iv.qty > 0 and rownum = 1;
            exception
              when no_data_found then
                 OUT_Return_Code:='999#engine(3): 货架原始位置没定位到，请检查';
                 rollback;
                 return;
              end;
           insert into idx_asrs_send (organizationid,warehouseid,grouptaskid,tasktype,palletid,fmlocation,tolocation,tolocation_WCS,
                 taskstatus,priority,sendtime_appointment,addtime,addwho,docno,equiptype,businesstype,ispackage,waveno,emptyflag,palletdetails,tolocation_type
                 ,fmzone,fmlocgrp2,fmloctype_wcs,fmloctype,fmlocmode,tozone,tolocgrp2,toloctype_wcs,toloctype,tolocmode,notes,udf05,edisendflag)
             values (IN_organizationId,IN_Warehouse,r_GROUPTASKID_NEW,r_tasktype,r_IN_TRACEID,r_fmlocation,'','',
                '00',10,null,sysdate,IN_UserID,'','LT','','','','N','',''
                ,'','','','','',r_tozone,'','','ZZW','OUT','','in_type:12',r_SEND_FLAG);
             --写入日志
          insert into idx_asrs_send_log (organizationid,warehouseid,grouptaskid,palletid,status,addtime,addwho,notes)
                             values     (IN_organizationId,IN_Warehouse,r_GROUPTASKID_NEW,r_IN_TRACEID,'00',sysdate,IN_UserID,'非任务移动');
     commit;
     goto to_send;
 end if;
----**************************************************************************************************************************



----**************************************************  异常退出 ERR-OUT 任务取消 ************************************************************************

--异常退出
<<ERR_OUT>>
if r_direction = 'ERR-OUT' then
           --完成之前的任务
            update idx_asrs_send ias set ias.taskstatus = '90',ias.notetext = '重复，执行归档' where ias.organizationid = IN_organizationId
            and ias.warehouseid = IN_Warehouse and ias.palletid = r_IN_TRACEID and ias.tasktype = 'ERR-OUT' and ias.taskstatus <= '81';

         r_taskstatus:='00';

         spcom_getidsequence(IN_organizationId,IN_Warehouse,IN_Language,'GROUPTASKID',r_GROUPTASKID,out_return_code);
             if substr(OUT_Return_Code,1,3) <> '000' then
               rollback;
               return;
              end if;
            select count(0),max(l.zoneid),max(l.locationcategory) into r_nrow,r_tozone,r_toloctype from bas_location l where l.organizationid = IN_organizationId and l.warehouseid = IN_Warehouse
            and l.locationid = nvl(IN_FMLOCATION,'*');
            if r_nrow > 0 then
               r_tolocation:='';

               r_tasktype:='ERR-OUT';
             --成品下线处理
             if r_toloctype = 'OUT_C' then
               r_tasktype:='V_MV';
               r_tozone:='XLHC_AREA';
               r_toloctype:='ERR';
               r_tolocation:='';
               r_tolocation_WCS:='';
              else
              --升降梯处理
              if r_toloctype = 'IN_E' then
               --再查看是否有库存
               select count(0) into r_nrow from inv_lot_loc_id iv where iv.organizationid = IN_organizationId and iv.warehouseid = IN_Warehouse
                and iv.traceid = r_IN_TRACEID and iv.qty > 0;

                --由参数控制目的去向,TSK_WCS_ERR_LOCATION
                 r_tolocation := GETSYS_configuration ('SIRIO', '*', '*', '*', 'TSK_WCS_ERR_LOCATION');---by zf 20220829 放到if外面

               if r_nrow = 0 and nvl(r_tolocation,'*') <> '*' then  --没有库存的托盘取系统参数定义的库位
                 --by zf -r_tolocation := GETSYS_configuration ('SIRIO', '*', '*', '*', 'TSK_WCS_ERR_LOCATION');
                 r_taskstatus:='10';
                else
                 r_tolocation:=IN_FMLOCATION;
                 r_tolocgrp2:='';
                 r_taskstatus:='10';
               end if;
               else
                 r_toloctype:='ABNORMAL';

                 if r_tozone = '3FINSTEREO' then  --如果是3楼入库口，直接退出
                     r_tolocation:=IN_FMLOCATION;
                     r_tolocgrp2:='';
                     r_taskstatus:='10';
                 end if;
              end if;

             end if;

            end if;


             insert into idx_asrs_send (organizationid,warehouseid,grouptaskid,tasktype,palletid,fmlocation,tolocation,tolocation_WCS,
                 taskstatus,priority,sendtime_appointment,addtime,addwho,docno,equiptype,businesstype,ispackage,waveno,emptyflag,palletdetails,tolocation_type
                 ,fmzone,fmlocgrp2,fmloctype_wcs,fmloctype,fmlocmode,tozone,tolocgrp2,toloctype_wcs,toloctype,tolocmode,notes,notetext,udf05,edisendflag)
             values (IN_organizationId,IN_Warehouse,r_GROUPTASKID,r_tasktype,r_IN_TRACEID,IN_FMLOCATION,r_tolocation,r_tolocation,
                r_taskstatus,10,null,sysdate,IN_UserID,'','LT','','','','N','',''
                ,'','','','','',r_tozone,r_tolocgrp2,'',r_toloctype,'',r_err_out_mesg,r_err_out_mesg,'in_type:err--'||IN_TYPE,r_SEND_FLAG);
             ---
              --写入日志
             insert into idx_asrs_send_log (organizationid,warehouseid,grouptaskid,palletid,status,addtime,addwho,notes)
                             values     (IN_organizationId,IN_Warehouse,r_GROUPTASKID,r_IN_TRACEID,'00',sysdate,IN_UserID,'(engine)无法申请上架库位，去向OUT');
             commit;
             r_in_taskflag  :='Y';  --进入实际任务下发过程
end if;

--定时器 或 指定进入条件 时调用
if IN_TYPE = '99' or r_in_taskflag = 'Y' then
for r in (
    select tk.organizationid,tk.warehouseid,tk.taskid,tk.fmlocation,tk.fmid,tk.plantolocation,tk.tasktype,tk.grouptaskid
                ,tk.addtime,tk.taskprocess,tk.docno,
                to_number(nvl(bc.udf01,'50')) - to_number(nvl(bl.udf02,'0'))  as priority,tk.udf01 as isPack,
                0 as  priority_doc,tk.udf02 as isPallet_type
          from tsk_tasklists tk
           left join bsm_code bc on tk.tasktype = bc.codeid and bc.codetype = 'TSK_TYP'
           left join bas_location bl on bl.organizationid = tk.organizationid and bl.warehouseid = tk.warehouseid and tk.fmlocation = bl.locationid
       where tk.organizationid = IN_organizationId and tk.warehouseid = IN_Warehouse
          and  tk.fmid =  case when nvl(r_IN_TRACEID,'*') = '*' then tk.fmid else r_IN_TRACEID end
          and  nvl(tk.grouptaskid_wcs,'*') in ('*',(case when IN_Language = 'DEBUG' then 'DEBUG' else '*' end )) -- IN_Language 传入DEBUG,用于调式
          and tk.taskprocess = '00' and nvl(tk.addtime,sysdate) >= sysdate - r_dateint
         and  tk.tasktype in ('MV','PA','CC','PK')
         --限制从在线拣选发起的分配下发
          and not exists
         (
           select 1 from bas_location l where  l.locationid = tk.fmlocation and l.organizationid = tk.organizationid and l.warehouseid = tk.warehouseid
           and l.locationcategory = 'PK' and tk.tasktype = 'PK'
         )
         --单据控制下发
          and sirio_Get_UDFWCS(tk.organizationid,tk.warehouseid,tk.tasktype,tk.docno,tk.doclineno) = 'Y'
          --and tk.taskid = 'TS00034443'

          order by tk.addtime
         ) loop
    r_nrow          :=0;
    r_GROUPTASKID   :=''; --任务组号
    r_fmlocation    :=''; --任务来源货位，如 kiva 移出需要转换为货架的移出位
    r_fmzone        :=''; --任务来源所在库存
    r_fmlocgrp2     :=''; --任务来源所在库位组2
    r_tolocation    :=''; --任务目标所在货位
    r_tolocation_WCS:='';
    r_tozone        :=''; --任务目标所在区域
    r_tolocgrp2     :=''; --任务目标所在库位组2
    r_fmloctype_wcs :=''; --任务来源的可达类型 ，WCS1 可达接驳位 ,WCS2 可达货位 ,KIVA 可达kiva货架
    r_toloctype_wcs :=''; --任务目标的可达类型 ，WCS1 可达接驳位 ,WCS2 可达货位 ,KIVA 可达kiva货架
    r_fmloctype     :=''; --任务来源货位类型
    r_toloctype     :=''; --任务目标货位类型，根据可达类型 会转换为 JBW 接驳位 或 ZZW 货架周转位
    r_fmlocmode     :=''; --任务来源货位的模式 IN 入库模式，OUT 出库模式
    r_tolocmode     :=r_tolocmode_SEND ; --任务目标货位的模式 IN 入库模式，OUT 出库模式
    r_docno         :='';--单号拼接
    r_waveno        :=''; --波次号
    r_palletid      :=''; --任务托务号，如是kiva 移出需要转换为货架号
    r_tasktype      :=r_tasktype_SEND; --任务类型，V_ 开头为虚拟任务
    r_message       :='';--错误信息
    r_send          :='N'; --是否下发任务标记
    r_linkpalletid  :=''; --关联托盘，上架时要求托盘到达接驳位才下发任务
    r_taskstatus    :='00';--
    r_priority_doc  :=0;
    r_sendtime_appointment := null;
    r_fmaisleno     :='';
    r_toaisleno     :='';
    r_IN_TRACEID :=r.fmid;
    r_type_toloctype:='';
    r_xwsku:='';
    r_XWFLAG:='';
    r_tolocation_WCS_DOC:='';
    r_notes:='';
    r_locgrouptype:='';
    r_expectedshipmenttime2 := null;
  --******************************************** MV/PA 任务**********************************************
   --移库/上架任务
   if r.tasktype in ('MV','PA') then
     r_tolocation_WCS:='';
     --控制是否下发
     if r.docno <> '*' and r.tasktype = 'MV' then --直接移动事务不控制
      select count(0),max(m.udf01),max(m.udf02),max(m.udf06) into r_nrow,r_tolocation_WCS,r_tasktype,r_xwsku from doc_movement_header m
       where m.mdocno = r.docno and m.organizationid = r.organizationid
      and m.warehouseid = r.warehouseid
      and nvl(m.udfwcs,'N') = 'Y';
      if r_nrow = 0 then
          r_message:='单据控制不可下发，请检查.';
          goto next_loop;
      end if;
     --纤维托盘标记
      if nvl(r_xwsku,'*') <> '*' then
        r_XWFLAG:='Y';
      end if;
       --移库单强制指定出口
     if nvl(r_tolocation_WCS,'*') <> '*' then
       --成品抽样
         select count(0),max(bl.locationid),max(bl.zoneid) into r_nrow,r_tolocation,r_tozone
         from bas_location bl where bl.organizationid = IN_organizationId and bl.warehouseid = IN_Warehouse
         and (bl.locationid = r_tolocation_WCS   or bl.locgroup2 = r_tolocation_WCS or bl.zoneid = r_tolocation_WCS)
         and bl.zoneid = '3FRESERVEDSTATION02'  and rownum = 1 ;
       if r_nrow > 0 then
             --去缓存区，自动找空闲
               r_tozone:='3FRESERVEDSTATION02';
               r_toloctype:='JBW';
               r_tolocation:='';
               r_tolocation_WCS:='';
           r_palletid:=r.fmid;
           r_fmlocation := r.fmlocation;
           r_send:='Y';
           r_taskstatus:='00';
           r_tasktype:=nvl(r_tasktype,r.tasktype);
         goto next_loop;
       end if;

       --目标去向是否为 3FBUFFER 取样区
       select count(0),max(bl.locationid),max(bl.zoneid) into r_nrow,r_tolocation,r_tozone
         from bas_location bl where bl.organizationid = IN_organizationId and bl.warehouseid = IN_Warehouse
         and (bl.locationid = r_tolocation_WCS   or bl.locgroup2 = r_tolocation_WCS or bl.zoneid = r_tolocation_WCS)
         and bl.zoneid = '3FBUFFER'  and rownum = 1 ;
       if r_nrow > 0 then
         --去取样间
             select count(0) into r_nrow from bas_location l
             left join bas_zone z on l.organizationid = z.organizationid and l.warehouseid = z.warehouseid and  l.zoneid = z.zoneid
             where l.organizationid = IN_organizationId and l.warehouseid = IN_Warehouse and l.locationid = r.fmlocation
              and z.udf05 = 'WCS2' ;
              if r_nrow > 0 then
                r_toloctype:='BUF_L';
              else
                r_toloctype:='BUF_P';
              end if;
                --去缓存区，自动找空闲
                r_tozone:='3FBUFFER';
                r_end_location:=r_tolocation; --最终去向货位
                r_tolocation:='';
                r_tolocation_WCS:='';

           r_palletid:=r.fmid;
           r_fmlocation := r.fmlocation;
           r_send:='Y';
           r_taskstatus:='00';
           r_tasktype:=nvl(r_tasktype,r.tasktype);
         goto next_loop;
       end if;

       select count(0),max(l.locationcategory),max(l.zoneid),max(l.locationid),max(l.locgroup2) into r_nrow,r_toloctype,r_tozone,r_tolocation,r_tolocgrp2 from bas_location l where l.organizationid = IN_organizationId and l.warehouseid = IN_Warehouse
        and (l.locationid =   r_tolocation_WCS or l.locgroup2 = r_tolocation_WCS );
        if r_nrow > 0 then   --如请求货位无效，会取移动单的目标货位
           r_tasktype:=nvl(r_tasktype,r.tasktype);
           --r_toloctype:='OUT'; --接驳位
           r_tolocmode:='';  --入库模式
           if r_tolocation = r_tolocation_WCS then --指定为具体口
               r_tolocation_WCS := r_tolocation_WCS;
               r_tolocation     := r_tolocation_WCS;
             else
               r_tolocation:='';
               r_tolocation_WCS:='';
           end if;

           r_palletid:=r.fmid;
           r_fmlocation := r.fmlocation;
           r_send:='Y';
           r_taskstatus:='00';
          goto next_loop;
        end if;
     end if;
    end if;

    --PA 去取样间
    if r.tasktype = 'PA' then
    --目标去向是否为 3FBUFFER 取样区
       select count(0),max(bl.locationid),max(bl.zoneid) into r_nrow,r_tolocation,r_tozone
         from bas_location bl where bl.organizationid = IN_organizationId and bl.warehouseid = IN_Warehouse
         and bl.locationid = r.plantolocation  and bl.zoneid = '3FBUFFER'  and rownum = 1 ;
       if r_nrow > 0 then
         --去取样间
             select count(0) into r_nrow from bas_location l
             left join bas_zone z on l.organizationid = z.organizationid and l.warehouseid = z.warehouseid and  l.zoneid = z.zoneid
             where l.organizationid = IN_organizationId and l.warehouseid = IN_Warehouse and l.locationid = r.fmlocation
              and z.udf05 = 'WCS2' ;
              if r_nrow > 0 then
                r_toloctype:='BUF_L';
              else
                r_toloctype:='BUF_P';
              end if;
                --去缓存区，自动找空闲
                r_tozone:='3FBUFFER';
                r_end_location:=''; --最终去向货位
                r_tolocation:='';
                r_tolocation_WCS:='';

           r_palletid:=r.fmid;
           r_fmlocation := r.fmlocation;
           r_send:='Y';
           r_taskstatus:='00';
           r_tasktype:=nvl(r_tasktype,r.tasktype);
         goto next_loop;
       end if;
    end if;

     --取原始货位
     begin
     select l.locationid,l.zoneid,z.udf05,
     --l.locgroup2
     l.udf01 --kiva 货架组
     ,g2.udf01,l.locationcategory,l.aisleno
         into r_fmlocation,r_fmzone,r_fmloctype_wcs,r_fmlocgrp2,r_fmlocmode,r_fmloctype,r_fmaisleno
     from bas_location l
     inner join bas_zone z on l.organizationid = z.organizationid and l.warehouseid = z.warehouseid and l.zoneid = z.zoneid
     left  join bas_locgroup2 g2 on l.organizationid = g2.organizationid and l.warehouseid = g2.warehouseid and l.locgroup2 = g2.locgroup2
      where l.organizationid = r.organizationid and l.warehouseid = r.warehouseid
     and l.locationid = r.fmlocation;
     exception
       when no_data_found then
          r_message:='没有原始货位数据，请检查.';
          goto next_loop;
       end;
     --取目位货位
     begin
     select l.locationid,l.zoneid,z.udf05,
     l.udf01 --kiva 货架组
     ,g2.udf01,l.locationcategory,l.aisleno
         into r_tolocation,r_tozone,r_toloctype_wcs,r_tolocgrp2,r_tolocmode,r_toloctype,r_toaisleno
     from bas_location l
     inner join bas_zone z on l.organizationid = z.organizationid and l.warehouseid = z.warehouseid and l.zoneid = z.zoneid
     left  join bas_locgroup2 g2 on l.organizationid = g2.organizationid and l.warehouseid = g2.warehouseid and l.locgroup2 = g2.locgroup2
      where l.organizationid = r.organizationid and l.warehouseid = r.warehouseid
     and l.locationid = r.plantolocation;
     exception
       when no_data_found then
          r_message:='没有目标货位数据，请检查.';
          goto next_loop;
       end;
     --检查不合规移库任务
     if r_fmloctype_wcs = 'WCS1' and r_toloctype_wcs = 'KIVA' and r_toloctype <> 'JBW' then
       update tsk_tasklists tk set tk.notetext = '(平库)WCS1==>KIVA 不能直接移库，请用两步法，请检查.' where tk.organizationid = r.organizationid and tk.warehouseid = r.warehouseid
        and tk.taskid = r.taskid;
       goto next_loop;
     end if;
     if r_fmloctype_wcs = 'WCS2' and r_toloctype_wcs = 'KIVA'  and r_toloctype <> 'JBW' then
       update tsk_tasklists tk set tk.notetext = '(立库)WCS2==>KIVA 不能直接移库，请用两步法，请检查.' where tk.organizationid = r.organizationid and tk.warehouseid = r.warehouseid
        and tk.taskid = r.taskid;
       goto next_loop;
     end if;
     if r_fmloctype_wcs = 'KIVA' and r_toloctype_wcs = 'KIVA'  and r_fmzone <> r_tozone then
       update tsk_tasklists tk set tk.notetext = 'KIVA==>KIVA 不能跨区移库，请用两步法，请检查.' where tk.organizationid = r.organizationid and tk.warehouseid = r.warehouseid
        and tk.taskid = r.taskid;
       goto next_loop;
     end if;
     if r_fmloctype_wcs in ('WCS1','WCS2','KIVA') or r_toloctype_wcs in ('WCS1','WCS2','KIVA') then
        --由kiva移出,产生从货架到出库周转位的虚拟移动任务
        if r_fmloctype_wcs in ('KIVA') then
          r_tasktype:=nvl(r_tasktype,'V_'||r.tasktype);
          r_tolocation:=''; --目标周转位待下一步根据空闲情况处理
          r_toloctype:='ZZW'; --周转位
          r_tolocmode:='OUT';  --出库模式
          r_palletid:=r_fmlocgrp2; --按货架移出
          r_tolocgrp2:='';
          --查询货架原始库位
          begin
          select iv.locationid into r_fmlocation from inv_lot_loc_id iv where iv.organizationid = r.organizationid and iv.warehouseid = r.warehouseid
            and iv.traceid = r_palletid and iv.qty > 0 and iv.sku = 'KIVASHELF' and rownum = 1 ;
          exception
            when no_data_found then
               update tsk_tasklists tk set tk.notetext = '没有找到贷架的库存，请检查.' where tk.organizationid = r.organizationid and tk.warehouseid = r.warehouseid
               and tk.taskid = r.taskid;
               goto next_loop;
            end;
          r_send:='Y';
          r_taskstatus:='00';
          goto next_loop;
        end if;
        --立库移出到自动打托
        if r_fmloctype_wcs = 'WCS2' then
           if r_toloctype = 'OUT' then
              r_tasktype:=nvl(r_tasktype,r.tasktype);
              r_tolocation:=nvl(r_tolocation_WCS,r.plantolocation); --目标周转位待下一步根据空闲情况处理
              r_tolocation_WCS:=nvl(r_tolocation_WCS,r.plantolocation);
              r_toloctype:='OUT'; --周转位
              r_tolocmode:='';  --入库模式
              r_linkpalletid:=''; --记录上架原始托盘
             -- r_palletid:=''; --按货架移出  ---by zf
              r_palletid := r.fmid;--by zf
              r_send:='Y';
              r_taskstatus:='00';
              goto next_loop;
           end if;
        end if;
         --移入到kiva,产生从货架到入库周转位的虚拟移动任务
        if r_toloctype_wcs in ('KIVA') then
          r_tasktype:=nvl(r_tasktype,'V_'||r.tasktype);
          r_tolocation:=''; --目标周转位待下一步根据空闲情况处理
          r_toloctype:='ZZW'; --周转位
          r_tolocmode:='IN';  --入库模式
          r_linkpalletid:=r.fmid; --记录上架原始托盘
          r_taskstatus:='-1';
          r_palletid:=r_tolocgrp2; --按货架移出
          r_tolocgrp2:='';
          --查询货架原始库位
          begin
          select iv.locationid into r_fmlocation from inv_lot_loc_id iv where iv.organizationid = r.organizationid and iv.warehouseid = r.warehouseid
            and iv.traceid = r_palletid and iv.qty > 0 and iv.sku = 'KIVASHELF' and rownum = 1 ;
          exception
            when no_data_found then
               update tsk_tasklists tk set tk.notetext = '没有找到贷架的库存，请检查.' where tk.organizationid = r.organizationid and tk.warehouseid = r.warehouseid
               and tk.taskid = r.taskid;
               goto next_loop;
            end;
          r_send:='Y';
          goto next_loop;
        end if;
        --目标只能达接驳位,如果移动目标位本来就是接驳位则目标就直接使用，不要转移为接驳位
        if r_toloctype_wcs in ('WCS1') then
          --如果目标位是接驳位类型，则直接使用
          if r_toloctype = 'JBW' then
             r_tolocation_WCS:=r_tolocation; --依然要判断是否空闲才可下发给WMS
          else
             r_tolocation:='';
             r_toloctype:='JBW'; --接驳位
             r_tolocmode:=nvl(r_tolocmode,'IN');  --入库模式
          end if;
          --r_tasktype:='V_'||r.tasktype;
          r_tasktype:=nvl(r_tasktype,'V_'||r.tasktype);
          r_palletid:=r.fmid;
          r_send:='Y';
          r_taskstatus:='00';
          goto next_loop;
        end if;
        --目标能达货位,直接移库,直接可下发
        if r_toloctype_wcs in ('WCS2') then
          r_tasktype:=nvl(r_tasktype,r.tasktype);
          r_toloctype:=''; --接驳位
          r_tolocmode:='';  --入库模式
          r_tolocation_WCS := r_tolocation;
          r_palletid:=r.fmid;
          r_send:='Y';
          r_taskstatus:='00';
          goto next_loop;
        end if;
     else
       --非设备可达货位，不下发
       update tsk_tasklists tk set tk.grouptaskid_wcs = '-',tk.notetext='非设备可达任务，不下发' where tk.organizationid = r.organizationid and tk.warehouseid = r.warehouseid and tk.taskid = r.taskid;
       goto next_loop;
     end if;
   end if;
   --******************************************** CC 任务**********************************************
   --盘点任务
   if r.tasktype in ('CC') then
     --控制是否下发
     if r.docno <> '*' and r.tasktype = 'CC' then --直接移动事务不控制
      select count(0) into r_nrow from cou_request m where m.cyclecountno = r.docno and m.organizationid = r.organizationid and m.warehouseid = r.warehouseid
      and nvl(m.udfwcs,'N') = 'Y';
      if r_nrow = 0 then
          r_message:='单据控制不可下发，请检查.';
          goto next_loop;
      end if;
     end if;
     --取原始货位
     begin
     select l.locationid,l.zoneid,z.udf05,
     --l.locgroup2
     l.udf01 --kiva 货架组 ，取代库位组2
     ,g2.udf01,l.locationcategory
         into r_fmlocation,r_fmzone,r_fmloctype_wcs,r_fmlocgrp2,r_fmlocmode,r_fmloctype
     from bas_location l
     inner join bas_zone z on l.organizationid = z.organizationid and l.warehouseid = z.warehouseid and l.zoneid = z.zoneid
     left  join bas_locgroup2 g2 on l.organizationid = g2.organizationid and l.warehouseid = g2.warehouseid and l.locgroup2 = g2.locgroup2
      where l.organizationid = r.organizationid and l.warehouseid = r.warehouseid
     and l.locationid = r.fmlocation;
     exception
       when no_data_found then
          r_message:='没有原始货位数据，请检查.';
          goto next_loop;
       end;
     if r_fmloctype_wcs in ('WCS2','KIVA')  then
        --由kiva盘点,产生从货架到出库周转位的虚拟移动任务
        if r_fmloctype_wcs in ('KIVA') then
          r_tasktype:='V_'||r.tasktype;
          r_tolocation:=''; --目标周转位待下一步根据空闲情况处理
          r_toloctype:='ZZW'; --周转位
          r_tolocmode:='OUT';  --出库模式,暂定利用出库位进行盘点
          r_palletid:=r_fmlocgrp2; --按货架移出
          r_tozone:= r_fmzone;
          r_fmloctype:='';
          --查询货架原始库位
          begin
          select iv.locationid into r_fmlocation from inv_lot_loc_id iv where iv.organizationid = r.organizationid and iv.warehouseid = r.warehouseid
            and iv.traceid = r_palletid and iv.qty > 0 and iv.sku = 'KIVASHELF' and rownum = 1 ;
          exception
            when no_data_found then
               update tsk_tasklists tk set tk.notetext = '没有找到贷架的库存，请检查.' where tk.organizationid = r.organizationid and tk.warehouseid = r.warehouseid
               and tk.taskid = r.taskid;
               goto next_loop;
            end;
          r_send:='Y';
          r_taskstatus:='00';
          goto next_loop;
        end if;
        --从立库移出盘点任务,送到对应的在线拣线工位进行盘点
        if r_fmloctype_wcs in ('WCS2') then
          r_tasktype:='V_'||r.tasktype;
          r_toloctype:='PK'; --在选拣选工站
          r_tolocmode:='';
          r_tolocation_WCS := '';
          r_palletid:=r.fmid;
          r_tozone:='';
          --先查订单是否指定库区
          begin
            select cr.udf06 into r_tozone from cou_request cr where  cr.organizationid = r.organizationid and cr.warehouseid = r.warehouseid
             and cr.cyclecountno = r.docno and rownum = 1  ;
          exception
            when no_data_found then
               r_tozone:='';
            end;
         if nvl(r_tozone,'*') = '*' then
          --查找盘点类型对应的库区
          begin
            select bc.udf01 into r_tozone from cou_request cr
            inner join bsm_code bc on cr.counttype = bc.codeid and bc.codetype = 'CNT_TYP' and bc.organizationid = cr.organizationid --and bc.warehouseid = cr.warehouseid
           where cr.cyclecountno = r.docno and bc.udf01 is not null
             and cr.organizationid = r.organizationid and cr.warehouseid = r.warehouseid
            and rownum = 1 ;
          exception
            when no_data_found then
                --r_message:='立库盘点对应的类型没有配置盘点工位，请检查.';
                --goto next_loop;
               r_tozone:='';
            end;
           end if;
           if nvl(r_tozone,'*') = '*' then
           --如果都未指定，负载均衡,交替下发
           begin
           select zoneid into r_tozone from (
            select zoneid,fcnt from (
            select bl.zoneid,count(0) as fcnt  from idx_asrs_send ias
            inner join bas_location bl on ias.organizationid = bl.organizationid and ias.warehouseid = bl.warehouseid and ias.tolocation = bl.locationid
            where ias.organizationid = r.organizationid and ias.warehouseid = r.warehouseid
             and bl.zoneid in ('01','02','02A','03')
            and ias.tasktype = 'V_CC') a
             order by fcnt
            ) where rownum = 1;
            exception
              when no_data_found then
                r_tozone:='';
              end;
           end if;
          r_send:='Y';
          r_taskstatus:='00';
          goto next_loop;
        end if;
     else
       --非设备可达货位，不下发
       update tsk_tasklists tk set tk.grouptaskid_wcs = '-',tk.notetext='非设备可达任务，不下发' where tk.organizationid = r.organizationid and tk.warehouseid = r.warehouseid and tk.taskid = r.taskid;
       goto next_loop;
     end if;
   end if;
   --******************************************** PK 任务**********************************************
   --拣货任务
   if r.tasktype in ('PK') then
     --控制是否下发
     if r.docno <> '*' and r.tasktype = 'PK' then --直接移动事务不控制
      select count(0),max(to_number(nvl(m.priority,3))),null,max(m.ordertype),max(nvl(m.hedi12,bc.udf04)),max(bc.udf02)
         into r_nrow,r_priority_doc,r_sendtime_appointment,r_doctype,r_tolocation_WCS_DOC,r_type_toloctype
      from doc_order_header m
      left join bsm_code bc on m.ordertype = bc.codeid and bc.codetype = 'SO_TYP' and bc.organizationid = m.organizationid
      where m.orderno = r.docno and m.organizationid = r.organizationid and m.warehouseid = r.warehouseid
      and (nvl(m.udfwcs,'N') = 'Y' or (nvl(m.waveno,'*') <> '*' and m.ordertype = 'B02')) and rownum = 1 ;
      if r_nrow = 0 then
          r_message:='单据控制不可下发，请检查.';
          goto next_loop;
      end if;
     end if;

     --其它出库如果有非其它出库的分配任务则不下发
     if r_doctype = 'B03' and r_tolocation_WCS_DOC in ('30310001','30310002','30310003') and nvl(r.fmid,'*') <> '*' then
        select count(distinct  oh.ordertype) into r_nrow from tsk_tasklists tk1
         left join doc_order_header oh on tk1.docno = oh.orderno and oh.organizationid = tk1.organizationid and oh.warehouseid = tk1.warehouseid
         left join bas_location bl1 on bl1.organizationid = tk1.organizationid and bl1.warehouseid = tk1.warehouseid and bl1.locationid = tk1.fmlocation
         left join bas_zone bz1 on bz1.organizationid = tk1.organizationid and bz1.warehouseid = tk1.warehouseid and bz1.zoneid = bl1.zoneid
      where  tk1.tasktype = 'PK' and tk1.organizationid = r.organizationid
         and tk1.warehouseid = r.warehouseid
         and tk1.taskprocess = '00'
         and tk1.addtime >= sysdate -r_dateint
         and nvl(tk1.grouptaskid_wcs,'*') = '*'
         and bz1.udf05 = 'WCS2'
         and oh.ordertype <> 'B03'
         and tk1.docno <> r.docno
         and nvl(oh.hedi12,'*') not in ('30310001','30310002','30310003')
         and tk1.fmid = r.fmid;
        if r_nrow > 0 then
           r_message:='取样托盘有混订单等待下发，请检查.';
           goto next_loop;
        end if;
     end if;

     --取原始货位
     begin
     select l.locationid,l.zoneid,z.udf05,
     --l.locgroup2
     l.udf01 --kiva 货架组
     ,g2.udf01,l.locationcategory ,z.udf03,z.udf04
         into r_fmlocation,r_fmzone,r_fmloctype_wcs,r_fmlocgrp2,r_fmlocmode,r_fmloctype,r_direction,r_direction_zone
     from bas_location l
     inner join bas_zone z on l.organizationid = z.organizationid and l.warehouseid = z.warehouseid and l.zoneid = z.zoneid
     left  join bas_locgroup2 g2 on l.organizationid = g2.organizationid and l.warehouseid = g2.warehouseid and l.locgroup2 = g2.locgroup2
      where l.organizationid = r.organizationid and l.warehouseid = r.warehouseid
     and l.locationid = r.fmlocation and rownum = 1;
     exception
       when no_data_found then
          r_message:='没有原始货位数据，请检查.';
          goto next_loop;
       end;
     if r_fmloctype_wcs in ('WCS2','KIVA')  then
        --由kiva拣货,产生从货架到出库周转位的虚拟移动任务
        if r_fmloctype_wcs in ('KIVA') then
          r_tasktype:='V_'||r.tasktype;
          r_tolocation:=''; --目标周转位待下一步根据空闲情况处理
          r_toloctype:='ZZW'; --周转位
          r_tolocmode:='OUT';  --出库模式
          r_palletid:=r_fmlocgrp2; --按货架移出
          r_tolocgrp2:='';
          r_tozone:= r_fmzone;
          --查询货架原始库位
          begin
          select iv.locationid into r_fmlocation from inv_lot_loc_id iv where iv.organizationid = r.organizationid and iv.warehouseid = r.warehouseid
            and iv.traceid = r_palletid and iv.qty > 0 and iv.sku = 'KIVASHELF' and rownum = 1 ;
          exception
            when no_data_found then
               update tsk_tasklists tk set tk.notetext = '没有找到贷架的库存，请检查.' where tk.organizationid = r.organizationid and tk.warehouseid = r.warehouseid
               and tk.taskid = r.taskid;
               goto next_loop;
            end;
          r_send:='Y';
          r_taskstatus:='00';
          goto next_loop;
        end if;
        --从立库拣货任务
        if r_fmloctype_wcs in ('WCS2') then
          r_tasktype:=r.tasktype;
          --r_toloctype:='PUT/BUF/PK/OUT/FCD'; --在选拣选工站、或出库口，或自动拆躲，下一步判断
          r_tolocmode:='';
          r_tolocation_WCS := '';
          r_palletid:=r.fmid;
          --r_tozone:= r_fmzone;
          r_tolocgrp2:='';
          r_toloctype:='';
          --查找拣货类型对应的库区

          begin
            select bc.udf01,nvl(nvl(cr.hedi13,cr.hedi12),bc.udf04),bc.udf02 into r_tozone,r_tolocation_WCS_DOC,r_type_toloctype from doc_order_header cr
            inner join bsm_code bc on cr.ordertype = bc.codeid and bc.codetype = 'SO_TYP' and bc.organizationid = cr.organizationid --and bc.warehouseid = cr.warehouseid
           where cr.orderno = r.docno and nvl(bc.udf01,'*') <> '*' and rownum = 1 ;
           --and cr.ordertype not in ('B02') ; --生产领料不使用指定库位下发
          exception
            when no_data_found then
                --r_message:='拣货对应的类型没有配工位库区，请检查.';
                --goto next_loop;
                r_tolocation:='';
                r_tolocation_WCS_DOC:='';
                r_tolocation_WCS:='';
                r_tozone:='';
                r_type_toloctype:='';
            end;

          --r_tolocation_WCS:=r_tolocation;

          --如果订单有指定出口
          /*if nvl(r_tolocation,'*') <> '*'  then
             --货位是否合法
             begin
             select bl.locationid,bl.locgroup2,nvl(lg.udf02,'N'),bl.zoneid,bl.locationcategory
               into r_tolocation,r_tolocation_WCS,r_isgroupid,r_tozone,r_toloctype
             from bas_location bl
             left join bas_locgroup2 lg on bl.locgroup2 = lg.locgroup2 and bl.organizationid = lg.organizationid and bl.warehouseid = lg.warehouseid
             where bl.organizationid = IN_organizationId and bl.warehouseid = IN_Warehouse
             and (bl.locationid = r_tolocation ) and rownum = 1;
               --如果不可以以组号下发
               if r_isgroupid = 'N' then
                 r_tolocation_WCS:=r_tolocation;
               end if;
             exception
               when no_data_found then
                 r_tolocation:='';
                 r_tolocation_WCS:='';
               end;
          end if;*/



         --如果是去2楼的再考虑去缓存或配盘
          if  r_tozone = '2FOUTSTEREO' then
           --换标业务都去在线拣选
           if r_doctype in ('C02') then
             r_toloctype:='PK';
           else
           --立库去取样间都整托配送
             if r_tolocation_WCS_DOC in ('30310001','30310002','30310003') and r_doctype in ('B03','B15')  then
                  --不管分配多少都当整托处理
                  --去抽样
                     if nvl(r_tolocation_WCS_DOC,'*') <> '*' then
                       --目标去向是否为 3FBUFFER 取样区
                         select count(0),max(bl.locationid),max(bl.zoneid) into r_nrow,r_tolocation,r_tozone
                         from bas_location bl where bl.organizationid = IN_organizationId and bl.warehouseid = IN_Warehouse
                         and (bl.locationid = r_tolocation_WCS_DOC   or bl.locgroup2 = r_tolocation_WCS_DOC or bl.zoneid = r_tolocation_WCS_DOC)
                         and bl.zoneid = '3FBUFFER'  and rownum = 1 ;
                         if r_nrow > 0 then

                       select count(0) into r_nrow from bas_location l
                        left join bas_zone z on l.organizationid = z.organizationid and l.warehouseid = z.warehouseid and  l.zoneid = z.zoneid
                         where l.organizationid = IN_organizationId and l.warehouseid = IN_Warehouse and l.locationid = r.fmlocation
                         and z.udf05 = 'WCS2' ;
                         if r_nrow > 0 then
                           r_toloctype:='BUF_L';
                         else
                           r_toloctype:='BUF_P';
                         end if;
                          --去缓存区，自动找空闲
                         r_tozone:='3FBUFFER';
                         r_end_location:=r_tolocation; --最终去向货位
                         r_tolocation:='';
                         r_tolocation_WCS:='';
                         r_tolocgrp2:='';

                         r_palletid:=r.fmid;
                         r_fmlocation := r.fmlocation;
                         r_send:='Y';
                         r_taskstatus:='00';
                         r_tasktype:=nvl(r_tasktype,r.tasktype);
                         --if nvl(r_type_toloctype,'*') <> '*' then --如：其他出库到提定的类型货位，在订单类型中配置
                         --   r_toloctype:=r_type_toloctype;
                         --end if;
                         goto next_loop;
                       end if;
                 end if;
            end if;
             --是否整托任务
              select count(distinct iv.sku) into r_nrow  from inv_lot_loc_id iv where iv.traceid = r_palletid
               and iv.organizationid = r.organizationid and iv.warehouseid = r.warehouseid
               and iv.qty > 0 and iv.qtyallocated <> iv.qty ; --整托分配

           if r_nrow = 0 then --整托分配去向

               --立库转清真,整托分配
               if r_doctype = 'B06' then
                 if nvl(r_tolocation_WCS_DOC,'*') <> '*' then
                    begin
                      select l.zoneid into r_tozone from bas_location l where l.organizationid = IN_organizationId and l.warehouseid = IN_Warehouse
                      and (l.locationid = r_tolocation_WCS_DOC or l.locgroup2 = r_tolocation_WCS_DOC ) and rownum = 1;
                         r_toloctype:='JBW';
                         r_tolocmode:='IN';
                    exception
                      when no_data_found then
                         r_tozone:='26';
                         r_toloctype:='JBW';
                         r_tolocmode:='IN';
                      end;

                 else
                         r_tozone:='26';
                         r_toloctype:='JBW';
                         r_tolocmode:='IN';
                 end if;
                 r_send:='Y';
                 goto next_loop;
              end if;

              --有指定出库口的定单

              if nvl(r_tolocation_WCS_DOC,'*') <> '*' and r_doctype <> 'B02'  then

                  --是否为整托
               select count(distinct iv.sku) into r_nrow  from inv_lot_loc_id iv where iv.traceid = r_palletid
                and iv.organizationid = r.organizationid and  iv.warehouseid = r.warehouseid
                and iv.qty > 0 and iv.qtyallocated <> iv.qty ; --整托分配
             --整托再查定单是否为同一定单类型
              if r_nrow = 0 then
                 select count(distinct oh.ordertype ) into r_nrow from act_allocation_details ad
                 inner join doc_order_header oh on ad.warehouseid = oh.warehouseid and ad.organizationid = ad.organizationid and ad.orderno = oh.orderno
                where ad.organizationid = r.organizationid and ad.warehouseid = r.warehouseid
                and ad.traceid = r_palletid and ad.status = '40' and ad.addtime >= sysdate - r_dateint ;
                 if r_nrow = 1 then
                   r_nrow :=0; --同一定单类型视为整托
                 end if;
               end if;
               if r_nrow = 0 then
                    begin
                      select l.locationid
                       into r_tolocation
                      from bas_location l where l.organizationid = IN_organizationId and l.warehouseid = IN_Warehouse
                      and (l.locationid = r_tolocation_WCS_DOC or l.locgroup2 = r_tolocation_WCS_DOC ) and rownum = 1;
                      r_tolocation_WCS:=r_tolocation;
                      r_send:='Y';
                    exception
                      when no_data_found then
                         --r_tozone:='26';
                         --r_toloctype:='JBW';
                         --r_tolocmode:='IN';
                         r_tolocation_WCS_DOC:='';
                         r_tolocation:='';
                         r_tolocation_WCS:='';
                      end;
                  end if;

              end if;

           /*--其他指定出口情况
            if nvl(nvl(r_tolocation_WCS_DOC,,'*') <> '*' then
             --货位是否合法
             begin
             select bl.locationid,bl.locgroup2,nvl(lg.udf02,'N') into r_tolocation,r_tolocation_WCS,r_isgroupid
             from bas_location bl
             left join bas_locgroup2 lg on bl.locgroup2 = lg.locgroup2 and bl.organizationid = lg.organizationid and bl.warehouseid = lg.warehouseid
             where bl.organizationid = IN_organizationId and bl.warehouseid = IN_Warehouse
             and (bl.locationid = r_tolocation_WCS_DOC or bl.locgroup2 = r_tolocation_WCS_DOC or bl.zoneid = r_tolocation_WCS_DOC ) and rownum = 1;
               --如果不可以以组号下发
               if r_isgroupid = 'N' then
                 r_tolocation_WCS:=r_tolocation;
               end if;
               r_send:='Y';
               goto next_loop;
             exception
               when no_data_found then
                 r_tolocation:='';
               end;
            end if;  */

            --如果存在非B02的定单，只能去在线拣
           /* select count(0),max(udf01) into r_nrow,r_tolocgrp2 from (
            select oh.expectedshipmenttime2,oh.hedi12,w.udf01,oh.consigneeid from act_allocation_details ad
            inner join doc_order_header oh on oh.organizationid = ad.organizationid and oh.warehouseid = ad.warehouseid and ad.orderno = oh.orderno
            left join  doc_wave_header w on oh.organizationid = w.organizationid and oh.warehouseid = w.warehouseid and oh.waveno = w.waveno
            where ad.organizationid = IN_organizationId and ad.warehouseid = IN_Warehouse and oh.udfwcs = 'Y'
            and ad.traceid = r_palletid and ad.status = '40' and ad.addtime >= sysdate -7
            and oh.ordertype not in ('B02') --生产领料
            group by oh.expectedshipmenttime2,oh.consigneeid,oh.hedi12,w.udf01);
            if r_nrow = 0 then
           */

            --是否同一时间，同一目的地
            select count(0),max(udf01) into r_nrow,r_tolocgrp2 from (
            select oh.expectedshipmenttime2,oh.hedi12,w.udf01,oh.consigneeid from act_allocation_details ad
            inner join doc_order_header oh on oh.organizationid = ad.organizationid and oh.warehouseid = ad.warehouseid and ad.orderno = oh.orderno
            left join  doc_wave_header w on oh.organizationid = w.organizationid and oh.warehouseid = w.warehouseid and oh.waveno = w.waveno
            where ad.organizationid = IN_organizationId and ad.warehouseid = IN_Warehouse and oh.udfwcs = 'Y'
            and ad.traceid = r_palletid and ad.status = '40' and ad.addtime >= sysdate -r_dateint
            --and oh.ordertype in ('B02') --生产领料
            group by oh.expectedshipmenttime2,oh.consigneeid,oh.hedi12,w.udf01,oh.ordertype);
             if r_nrow > 0  then
              if r_nrow > 1 then
                --多订单类型混合分配情况
                select count(distinct oh.ordertype) into r_nrow from act_allocation_details ad
                 inner join doc_order_header oh on oh.organizationid = ad.organizationid and oh.warehouseid = ad.warehouseid and ad.orderno = oh.orderno
                 left join  doc_wave_header w on oh.organizationid = w.organizationid and oh.warehouseid = w.warehouseid and oh.waveno = w.waveno
                 where ad.organizationid = IN_organizationId and ad.warehouseid = IN_Warehouse and oh.udfwcs = 'Y'
                 and ad.traceid = r_palletid and ad.status = '40' and ad.addtime >= sysdate -r_dateint;
                 if r_nrow > 1 then
                   r_toloctype:='PK';
                 else
                  --如果同一托盘有已下及未下的则去在线--add by zf 20221005--begin
                  select  count(distinct  nvl(oh.udfwcs,'N')) into r_nrow from tsk_tasklists tk
                  inner join doc_order_header oh on tk.organizationid = oh.organizationid and tk.warehouseid = oh.warehouseid and tk.docno = oh.orderno
                  inner join bas_location bl on tk.fmlocation = bl.locationid and tk.organizationid = bl.organizationid and tk.warehouseid = bl.warehouseid
                 where tk.organizationid = IN_organizationId and tk.warehouseid = IN_Warehouse
                  and tk.fmid = r.fmid and tk.tasktype = 'PK'  and tk.taskprocess = '00'
                  and bl.zoneid in ('01','02','03','02A')
                  and tk.addtime >= sysdate - r_dateint;

                  if r_nrow > 1 then
                     r_toloctype:='PK';
                    else
                      r_toloctype:='JBW'; --配盘区接驳位
                      r_tozone:=nvl(r_direction_zone,'23');
                    end if;
                  ---add by zf 20221005--end---
                 end if;
              else
                --如果同一托盘有已下及未下的则去在线
                 select  count(distinct  nvl(oh.udfwcs,'N')) into r_nrow from tsk_tasklists tk
                  inner join doc_order_header oh on tk.organizationid = oh.organizationid and tk.warehouseid = oh.warehouseid and tk.docno = oh.orderno
                  inner join bas_location bl on tk.fmlocation = bl.locationid and tk.organizationid = bl.organizationid and tk.warehouseid = bl.warehouseid
                 where tk.organizationid = IN_organizationId and tk.warehouseid = IN_Warehouse
                  and tk.fmid = r.fmid and tk.tasktype = 'PK'  and tk.taskprocess = '00'
                  and bl.zoneid in ('01','02','03','02A')
                  and tk.addtime >= sysdate -r_dateint;
                   if r_nrow <=1 then
                     r_toloctype:='BUF'; --去缓存位
                     r_tozone:='24';
                     r_tolocgrp2:='';
                   else
                     r_toloctype:='PK';
                     if nvl(r_tozone,'*') = '*' then
                       r_message:='拣货对应的类型没有配工位库区，请检查.';
                       goto next_loop;
                     end if;
                   end if;
                 if nvl(r_type_toloctype,'*') <> '*' then --如：其他出库到提定的类型货位，在订单类型中配置
                    r_toloctype:=r_type_toloctype;
                 end if;
              end if;
              end if;

             /* else
                r_toloctype:='PK';
              end if;*/
              else
                if nvl(r_tozone,'*') = '*' then
                    r_message:='拣货对应的类型没有配工位库区，请检查.';
                    goto next_loop;
                end if;
                r_toloctype:='PK';
            end if;
           end if;


          end if;



          r_send:='Y';
          r_taskstatus:='00';
          goto next_loop;
        end if;
     else
       --非设备可达货位，不下发
       update tsk_tasklists tk set tk.grouptaskid_wcs = '-',tk.notetext='非设备可达任务，不下发' where tk.organizationid = r.organizationid and tk.warehouseid = r.warehouseid and tk.taskid = r.taskid;
       goto next_loop;
     end if;
   end if;
    <<next_loop>>
    --同时只能有一种类型任务允许下发
       select count(0) into r_nrow from idx_asrs_send ias where ias.organizationid = IN_organizationId and ias.warehouseid = IN_Warehouse
        and ias.palletid = r_palletid and ias.tasktype <> r_tasktype
        and ias.tasktype not in ('ERR-OUT');
       if r_nrow > 0 then
          r_send:='N';
          r_message:='等待其它类型任务完成';
       end if;
    --更新处理信息
    if nvl(r_message,'*') <> '*'  then
      update tsk_tasklists tk set tk.notetext = r_message where tk.organizationid = r.organizationid and tk.warehouseid = r.warehouseid
               and tk.taskid = r.taskid;
    end if;
    --下发任务
    if r_send = 'Y' then
       --空托盘出库任务标记
          if r.ispallet_type = 'V_MV1' then
             r_tasktype:='V_MV1';
          end if;

      --如同一托盘去向不同，先不下发，等回库之后再下
      if r_tasktype in ('PK') then
       --如果已有托盘去向PK,不考虑不同去向
       select count(0) into r_nrow from idx_asrs_send ias where ias.organizationid = IN_organizationId and ias.warehouseid = IN_Warehouse
        and ias.fmlocation = r_fmlocation and ias.palletid = r_palletid and ias.toloctype = 'PK' ;
       if r_nrow > 0 then
        select count(0) into r_nrow from  idx_asrs_send ias where ias.palletid = r_palletid and nvl(ias.tozone,'*') <> nvl(r_tozone,'*')
        and ias.organizationid = IN_organizationId and ias.warehouseid = IN_Warehouse;
         if r_nrow > 0 then
            --rollback;
            --update tsk_tasklists tk set tk.notetext='等待其它区域任务完成才能下发' where tk.organizationid = r.organizationid and tk.warehouseid = r.warehouseid and tk.taskid = r.taskid;
            --goto next_end;
          r_taskstatus:='-1';
          r_notes:='挂起，等待其它区域任务完成才能下发';
         end if;
        end if;
       end if;



      --先看是否有已待下发的任务
       begin
        select a.grouptaskid into r_GROUPTASKID from idx_asrs_send a
        where a.organizationid = r.organizationid and a.warehouseid = r.warehouseid
         and a.palletid = r_palletid and a.tasktype in (r_tasktype )
         and (case when a.tasktype = 'PK' then  a.tozone else '*' end)  =  (case when a.tasktype = 'PK' then  nvl(r_tozone,a.tozone) else '*' end)  --PK 不同区域视为不同任务 modiy by songyx 2022-9-7
         --and a.taskstatus in ('-2','-1','00','05','10','70','75','80') --取货前都可以视为同一任务
         and a.taskstatus <= '81' --未回收前视为同一任务
         and rownum = 1 and a.addtime >= sysdate - r_dateint and a.tasktype not in ('ERR-OUT','ADD','DEL') ;
         --and nvl(a.linkpalletid,'*') = case when  nvl(a.linkpalletid,'*') = '*' then '*' else r.fmid end  ;
         r_nrow:=1;
       exception
        when no_data_found then
          r_nrow:=0;
        end;



      if r_nrow = 1 then  --已有任务
         update tsk_tasklists a set a.grouptaskid_wcs = r_GROUPTASKID where a.organizationid = r.organizationid and a.warehouseid = r.warehouseid
         and a.taskid = r.taskid and nvl(a.grouptaskid_wcs,'*')  in ('*','DEBUG')  and a.taskprocess = '00';

         --拼接单号
         begin
         select regexp_replace(listagg(a.docno,',') within group(order by a.docno),'([^,]+)(,\1)+', '\1')
           into r_docno
         from tsk_tasklists a where a.organizationid = r.organizationid and a.warehouseid = r.warehouseid
         and a.grouptaskid_wcs = r_GROUPTASKID and a.addtime >= sysdate -r_dateint and a.taskprocess = '00' ;
         exception
           when no_data_found then
             r_docno := '';
           end;
          --拼接码头
         begin
         select regexp_replace(listagg(nvl(h.door,'*'),',') within group(order by a.docno),'([^,]+)(,\1)+', '\1')
           into r_doors
         from tsk_tasklists a
         inner join doc_order_header h on a.organizationid = h.organizationid and a.warehouseid = h.warehouseid and a.docno = h.orderno
         where a.organizationid = r.organizationid and a.warehouseid = r.warehouseid
         and a.grouptaskid_wcs = r_GROUPTASKID and a.addtime >= sysdate -r_dateint and a.taskprocess = '00' ;
         exception
           when no_data_found then
             r_doors := '';
           end;

         --songyx 2022-11-3 取消更新
          --获取生产领料最小时间
          /* select min(h.expectedshipmenttime2) into r_expectedshipmenttime2
           from tsk_tasklists a
         inner join doc_order_header h on a.organizationid = h.organizationid and a.warehouseid = h.warehouseid and a.docno = h.orderno
         where a.organizationid = r.organizationid and a.warehouseid = r.warehouseid
         and a.grouptaskid_wcs = r_GROUPTASKID and a.addtime >= sysdate -7 and a.taskprocess = '00'
         and h.ordertype = 'B02';*/

         if r_docno is not null then
            update idx_asrs_send a set a.docno = r_docno,a.doors = r_doors
            --,a.sendtime_appointment = (case when r_expectedshipmenttime2 is not null then r_expectedshipmenttime2 else a.sendtime_appointment end)
            where a.organizationid = r.organizationid and a.warehouseid = r.warehouseid
            and a.grouptaskid = r_GROUPTASKID ;
         end if;

      else   --新任务

           --如果订单有指定出口
          if nvl(r_tolocation,'*') <> '*'  then
             --货位是否合法
             begin
             select bl.locationid,bl.locgroup2,nvl(lg.udf02,'N'),bl.zoneid,bl.locationcategory
               into r_tolocation,r_tolocation_WCS,r_isgroupid,r_tozone,r_toloctype
             from bas_location bl
             left join bas_locgroup2 lg on bl.locgroup2 = lg.locgroup2 and bl.organizationid = lg.organizationid and bl.warehouseid = lg.warehouseid
             where bl.organizationid = IN_organizationId and bl.warehouseid = IN_Warehouse
             and (bl.locationid = r_tolocation ) and rownum = 1;
               --如果不可以以组号下发
               if r_isgroupid = 'N' then
                 r_tolocation_WCS:=r_tolocation;
               end if;
             exception
               when no_data_found then
                 r_tolocation:='';
                 r_tolocation_WCS:='';
               end;
          end if;


          --取波次号
          begin
           select a.waveno,a.door,a.expectedshipmenttime2
           into r_waveno,r_doors,r_sendtime_BUF
           from doc_order_header a where a.organizationid = r.organizationid and a.warehouseid = r.warehouseid
           and a.orderno = r.docno and rownum = 1;
         exception
           when no_data_found then
             r_waveno := '';
           end;
           --计算等待时间
          if r_toloctype = 'BUF' then
            if r_sendtime_BUF is not null then
             r_sendtime_BUF:=r_sendtime_BUF - (r_TSK_TOBUF_MIN/60/24);
            end if;
           else
            r_sendtime_BUF:=null;
          end if;
          spcom_getidsequence(IN_organizationId,IN_Warehouse,IN_Language,'GROUPTASKID',r_GROUPTASKID,out_return_code);
          if substr(OUT_Return_Code,1,3) <> '000' then
            rollback;
            goto next_end;
          end if;

         ---判断库位是否近深位--by zf 20221025 生产领料次日订单优先级---begin--
         select max(l.locgroup1) into r_locgrouptype
         from bas_location l
         where l.organizationid = IN_organizationId
         and l.warehouseid = IN_Warehouse
         and l.locationid = r_fmlocation;

         if r_locgrouptype = 'RACK_SSW_2' then
           r_priority_doc := '3';
         end if;
         ---判断库位是否近深位--by zf 20221025 生产领料次日订单优先级---end--
          insert into idx_asrs_send (organizationid,warehouseid,grouptaskid,tasktype,palletid,fmlocation,tolocation,tolocation_WCS,
                 taskstatus,priority,sendtime_appointment,addtime,addwho,docno,equiptype,businesstype,ispackage,waveno,emptyflag,palletdetails,tolocation_type
                 ,fmzone,fmlocgrp2,fmloctype_wcs,fmloctype,fmlocmode,tozone,tolocgrp2,toloctype_wcs,toloctype,tolocmode,fmaisleno,toaisleno,linkpalletid,doors,xwflag,edisendflag,
                 wait_time,udf05,xwsku,end_location,notes)
          values (r.organizationid,r.warehouseid,r_GROUPTASKID,r_tasktype,r_palletid,r_fmlocation,r_tolocation,r_tolocation_WCS,
                r_taskstatus,abs(nvl(r.priority,50)-nvl(/*r.priority_doc*/r_priority_doc,3)),r_sendtime_appointment,sysdate,( case when IN_Language = 'DEBUG' then 'DEBUG' else IN_UserID end),/*r_docno*/ r.docno ,'LT','',r.ispack,r_waveno,nvl(r_emptyflag,'N'),'',''
                ,r_fmzone,r_fmlocgrp2,r_fmloctype_wcs,r_fmloctype,r_fmlocmode,r_tozone,r_tolocgrp2,r_toloctype_wcs,r_toloctype,r_tolocmode,r_fmaisleno,r_toaisleno,r_linkpalletid,r_doors,r_XWFLAG,r_SEND_FLAG
                ,r_sendtime_BUF,'in_type:99--'||IN_TYPE,r_xwsku,r_end_location,r_notes);

          update tsk_tasklists a set a.grouptaskid_wcs = r_GROUPTASKID where a.organizationid = r.organizationid and a.warehouseid = r.warehouseid
           and a.taskid = r.taskid and nvl(a.grouptaskid_wcs,'*') in ('*','DEBUG') and a.taskprocess = '00';
         --如关联托盘任务，再下发一条托盘的移动任务
           if r_linkpalletid is not null then
              select count(0) into r_nrow from idx_asrs_send ias where ias.organizationid = r.organizationid and ias.warehouseid = r.warehouseid
              and ias.palletid = r_linkpalletid and ias.taskstatus in ('-1','00','05');
              if r_nrow = 0 then
                  spcom_getidsequence(IN_organizationId,IN_Warehouse,IN_Language,'GROUPTASKID',r_GROUPTASKID,out_return_code);
                if substr(OUT_Return_Code,1,3) <> '000' then
                  rollback;
                  goto next_loop;
                end if;
                  insert into idx_asrs_send (organizationid,warehouseid,grouptaskid,tasktype,palletid,fmlocation,tolocation,tolocation_WCS,
                 taskstatus,priority,sendtime_appointment,addtime,addwho,docno,equiptype,businesstype,ispackage,waveno,emptyflag,palletdetails,tolocation_type
                 ,fmzone,fmlocgrp2,fmloctype_wcs,fmloctype,fmlocmode,tozone,tolocgrp2,toloctype_wcs,toloctype,tolocmode,fmaisleno,toaisleno,linkpalletid,edisendflag,udf05,end_location,notes)
                 values (r.organizationid,r.warehouseid,r_GROUPTASKID,r_tasktype,r_linkpalletid,'待请求',r_tolocation,r_tolocation_WCS,
                  '00',abs(nvl(r.priority,50)-nvl(r_priority_doc,3)),r_sendtime_appointment,sysdate,( case when IN_Language = 'DEBUG' then 'DEBUG' else IN_UserID end),r_docno,'LT','',r.ispack,r_waveno,'N','',''
                 ,'','','','','',r_tozone,r_tolocgrp2,r_toloctype_wcs,'JBW',r_tolocmode,r_fmaisleno,r_toaisleno,r_linkpalletid,r_SEND_FLAG,'in_type:99--'||IN_TYPE,r_end_location,r_notes);
                                --写入日志
                   insert into idx_asrs_send_log (organizationid,warehouseid,grouptaskid,palletid,status,addtime,addwho,notes)
                             values     (IN_organizationId,IN_Warehouse,r_GROUPTASKID,r_palletid,r_taskstatus,sysdate,IN_UserID,r_notes);
              end if;
           end if;
                   --写入日志
          insert into idx_asrs_send_log (organizationid,warehouseid,grouptaskid,palletid,status,addtime,addwho,notes)
                             values     (IN_organizationId,IN_Warehouse,r_GROUPTASKID,r_palletid,r_taskstatus,sysdate,IN_UserID,r_notes);
      end if;
    end if;

    <<next_end>>

    commit;
  end loop;
end if;
<<to_send>>
--99 定时器下发控制不立即下发(由send定时器延迟下发),避免并发问题产生
--if IN_TYPE <> 99 then
--立即发下,为*部分由定时器处理下发
if nvl(r_IN_TRACEID,'*') <> '*'   then
  SPCUS_TASKGROUP_SEND
  (IN_organizationId ,
   IN_Warehouse      ,
   IN_FMLOCATION     ,
   r_IN_TRACEID      ,
   IN_UserID         ,
   IN_Language       ,
   OUT_Return_Code
  );
end if;
--end if;

OUT_Return_Code:='000';
return;
EXCEPTION
   WHEN OTHERS THEN
      Out_Return_Code :='SYS_0199 SPCUS_TASKGROUP_engine '||SQLerrm || DBMS_UTILITY.format_error_backtrace;
      ROLLBACK;
end;
