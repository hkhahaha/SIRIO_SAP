*&---------------------------------------------------------------------*
*& Report ZSDR0018
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zsdr0018.
TYPE-POOLS:slis."调用系统存在的类型池
*在调用ALV之前，需要先定义Layout和Fieldcat，他们属于slis类型池
DATA:fieldcat TYPE lvc_t_fcat WITH HEADER LINE,
     layout   TYPE lvc_s_layo,
     w_repid  TYPE sy-repid. "记录系统当前的程序名

INCLUDE ZSDR0018_TOP.

INCLUDE ZSDR0018_HEAD.

INCLUDE ZSDR0018_MAIN.