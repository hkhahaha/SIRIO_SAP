FUNCTION-POOL zmm006.                       "MESSAGE-ID ..

* INCLUDE LZMM006D...                        " Local class definition

CONSTANTS: cns_sep                  TYPE c VALUE '/',
           cns_mcflag_create        TYPE c VALUE 'C',
           cns_mcflag_modify        TYPE c VALUE 'M',
           cns_bptype_customer      TYPE zbct060-bptype VALUE 'C',
           cns_bptype_supplier      TYPE zbct060-bptype VALUE 'S',
           cns_bp_role_po           TYPE cvis_bp_role-data_key VALUE 'ZLVN01',
           cns_bp_role_comp         TYPE cvis_bp_role-data_key VALUE 'ZLVN00',
           cns_tdid_sales           TYPE thead-tdid VALUE '0003',

           cns_bp_role_cust_fin     TYPE cvis_bp_role-data_key VALUE 'FLCU00',
           cns_bp_role_cust_sales   TYPE cvis_bp_role-data_key VALUE 'FLCU01',
           cns_bp_role_cust_ukm     TYPE cvis_bp_role-data_key VALUE 'UKM000',
           cns_address_type_default TYPE but021_fs-adr_kind VALUE 'XXDEFAULT',
           cns_address_type_pi      TYPE but021_fs-adr_kind VALUE 'Z00001',
           cns_modifymode_insert    TYPE char1 VALUE 'I',
           cns_modifymode_update    TYPE char1 VALUE 'U',
           cns_modifymode_delete    TYPE char1 VALUE 'D',
           cns_parvw_z3             TYPE knvp-parvw VALUE 'Z3',
           cns_parvw_ag             TYPE knvp-parvw VALUE 'AG',
           cns_parvw_re             TYPE knvp-parvw VALUE 'RE',
           cns_parvw_rg             TYPE knvp-parvw VALUE 'RG',
           cns_parvw_we             TYPE knvp-parvw VALUE 'WE',
           cns_parvw_z1             TYPE knvp-parvw VALUE 'Z1',
           cns_parvw_z2             TYPE knvp-parvw VALUE 'Z2',
           cns_aland_cn             TYPE knvi-aland VALUE 'CN',
           cns_tatyp_mwst           TYPE knvi-tatyp VALUE 'MWST'
           .

TYPES: BEGIN OF ty_email,
         smtp_addr TYPE adr6-smtp_addr,
       END OF ty_email.

TYPES:BEGIN OF ty_ekorg,
      ekorg TYPE lfm1-ekorg,
      sperm TYPE lfm1-sperm,
      END OF ty_ekorg,
      tt_ekorg TYPE TABLE OF ty_ekorg,
      BEGIN OF ty_bukrs,
      bukrs TYPE lfb1-bukrs,
      loevm TYPE lfb1-loevm,
      END OF ty_bukrs,
      tt_bukrs TYPE TABLE OF ty_bukrs.

DATA: gs_email TYPE ty_email,
      gt_email LIKE TABLE OF gs_email.
DATA: g_partner   TYPE but000-partner,  "新建BP时外部编号或修改BP时，补0后的BP编号；内部编号的BP新建成功后产生的BP编号
      g_mcflag    TYPE zbct060-mcflag, "除了日志，其他地方都用g_mcflag
      g_insertlog TYPE c.