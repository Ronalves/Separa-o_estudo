*&---------------------------------------------------------------------*
*& Include          ZWMR_PICKING_SEM_OT_TOP
*&---------------------------------------------------------------------*

REPORT zwmr_picking_sem_ot.

************************************************************************
* TYPES                                                                *
************************************************************************
TYPES:
  BEGIN OF ty_9000,
    ref TYPE c LENGTH 10,
  END OF ty_9000,

  BEGIN OF ty_9100,
    ebeln      TYPE ekpo-ebeln, "Ordem
    ebelp      TYPE posnr_vl,   "Item
    descricao  TYPE makt-maktg, "Descrição
    matnr      TYPE mara-matnr, "Cod
    matnr_tela TYPE mara-matnr, "Cod
    lgort      TYPE ekpo-lgort, "DEP
    ud_v       TYPE c LENGTH 35, "Pallet
    lote       TYPE lqua-charg, "Lote
    matnr_v    TYPE c LENGTH 35, "Cod validação
    lote_v     TYPE lqua-charg, "Lote validação
    posicao    TYPE lqua-lgpla, "Posição
    posicao_v  TYPE lqua-lgpla, "Posição validação
    qtd_nec    TYPE ekpo-menge, "Qtd NEC
    qtd_disp   TYPE ekpo-menge, "Qtd DISP
    qtd_v      TYPE ekpo-menge, "Qtd validação
    und        TYPE ekpo-meins, "Unidade de medida
    vbeln      TYPE lips-vbeln,
  END OF ty_9100.

TYPES: BEGIN OF ty_total_aux.
    INCLUDE STRUCTURE l2sktotal.
TYPES: excl  TYPE c.
TYPES: END OF ty_total_aux.

TYPES: BEGIN OF ty_grupo.
TYPES: grupo TYPE char10.
TYPES: vbeln TYPE lips-vbeln.
TYPES: matnr TYPE lips-matnr.
TYPES: END OF ty_grupo.
DATA: gt_grupo TYPE TABLE OF ty_grupo.
************************************************************************
* TABELAS/WORKAREAS                                                    *
************************************************************************
DATA:
  gs_9000     TYPE ty_9000,
  gs_9100     TYPE ty_9100,
  gs_lrf_wkqu TYPE lrf_wkqu,
  gt_ekko     TYPE TABLE OF ekko,
  gs_ekko     TYPE ekko,
  gt_t311     TYPE TABLE OF t311,
  gs_t311     TYPE t311,
  gs_t331     TYPE t331,
  gt_ekpo     TYPE TABLE OF ekpo,
  gt_lqua     TYPE TABLE OF lqua,
  gs_lqua     TYPE lqua,
  gt_total    TYPE TABLE OF ty_total_aux,
  gs_total    TYPE  ty_total_aux.

*  gt_total     TYPE TABLE OF l2sktotal.

************************************************************************
* VARIÁVEIS                                                            *
************************************************************************
DATA:
  gv_erro       TYPE c,
  gv_answer     TYPE c,
  gv_msgv1      TYPE sprot_u-var1,
  gv_msgv2      TYPE sprot_u-var2,
  gv_msgv3      TYPE sprot_u-var3,
  gv_msgv4      TYPE sprot_u-var4,
  gv_werks      TYPE werks_d,
  gv_lgort      TYPE lgort_d,
  gv_bloq       TYPE c,
  gv_ekbe_menge TYPE ekbe-menge,
  gv_item       TYPE i,
  gv_palete     TYPE c,
  gv_lgtyp_1    TYPE lqua-lgtyp,
  gv_lgtyp_2    TYPE lqua-lgtyp.

************************************************************************
*** RANGES                                                           ***
************************************************************************
DATA:
  gr_werks TYPE RANGE OF resb-werks,
  gr_lgort TYPE RANGE OF resb-lgort,
  gr_lgtyp TYPE RANGE OF lqua-lgtyp.

*************************************************************************
** EVENTOS                                                              *
*************************************************************************

INITIALIZATION.

  PERFORM f_seleciona_tvavr.
  PERFORM f_verificar_usuario CHANGING gv_erro.

  CHECK gv_erro IS INITIAL.

  CALL SCREEN 9000.
