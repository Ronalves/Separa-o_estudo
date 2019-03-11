*----------------------------------------------------------------------*
***INCLUDE ZWMR_PICKING_SEM_OT_O01.
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Module M_TELA_9000 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE m_tela_9000 OUTPUT.

  SET PF-STATUS 'ST_9000'.

  CLEAR: gt_total,
         gt_ekko,
         gt_ekpo,
         gt_t311,
*         gt_t331,
         gt_total,
         gt_lqua,
         gs_9000-ref,
         gv_palete.

ENDMODULE.

*&---------------------------------------------------------------------*
*& Module M_TELA_9100 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE m_tela_9100 OUTPUT.

  SET PF-STATUS 'ST_9100'.

  IF gs_ekko IS NOT INITIAL. " Pedido de transferência

    CASE sy-ucomm.
      WHEN '9000ENTER'.
        PERFORM f_enter_9000.
      WHEN '9100ENTER'.
        PERFORM f_select_lqua.
        PERFORM f_enter_valida_9100.
        PERFORM f_saldo_pedido_atendido_ekbe USING gs_9100-ebeln gs_9100-ebelp CHANGING gv_ekbe_menge.
        PERFORM f_atualiza_dados_tela_lqua USING gs_9100-matnr gs_9100-posicao_v gs_9100-ud_v gv_ekbe_menge CHANGING gs_9100.
        PERFORM f_gerar_ot.
      WHEN 'BACK'.
        PERFORM f_select_lqua.
        PERFORM f_back_9100.
      WHEN 'NEXT'.
        PERFORM f_select_lqua.
        PERFORM f_next_9100.
      WHEN OTHERS.
    ENDCASE.

  ELSEIF gs_t311 IS NOT INITIAL. " Grupo de remessas

    CASE sy-ucomm.
      WHEN '9000ENTER'.
        PERFORM f_grp_enter_9000.
      WHEN '9100ENTER'.
*        PERFORM f_grp_select_lqua.
        PERFORM f_grp_select_lqua_new.

        PERFORM f_enter_valida_9100.
        PERFORM f_grp_atualiza_dados_tela_lqua USING gs_9100-matnr gs_9100-posicao_v gs_9100-ud_v CHANGING gs_9100.
* SAP - MGS -  Reinaldo Carvalho - 07.12.2018 17:01:46 - Início
        gs_9100-posicao_v = gs_9100-posicao.
* SAP - MGS - Reinaldo Carvalho - 07.12.2018 17:01:49 - Fim
        PERFORM f_gerar_ot.
      WHEN 'BACK'.
*        PERFORM f_grp_select_lqua.
        PERFORM f_grp_select_lqua_new.
        PERFORM f_grp_back_9100.
      WHEN 'NEXT'.
*        PERFORM f_grp_select_lqua.
        PERFORM f_grp_select_lqua_new.
        PERFORM f_grp_next_9100.
      WHEN OTHERS.
    ENDCASE.

  ENDIF.

ENDMODULE.
