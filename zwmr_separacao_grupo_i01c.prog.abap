*----------------------------------------------------------------------*
***INCLUDE ZWMR_PICKING_SEM_OT_I01.
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  M_USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE m_user_command_9000 INPUT.

  CASE sy-ucomm.
    WHEN '9000ENTER'.

      PERFORM f_verifica_ref USING gv_erro.
      perform f_ajusta_saldo_trans USING gs_9000-ref.

      CHECK gv_erro IS INITIAL.

      IF gs_ekko IS NOT INITIAL.
        "Pedido de transferência
        PERFORM f_pedido_transferencia.
      ELSEIF gs_t311 IS NOT INITIAL.
        "Grupo de remessas
        PERFORM f_grupo_remessas.
      ENDIF.

    WHEN 'SAIR' OR 'BACK1'.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  M_USER_COMMAND_9100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE m_user_command_9100 INPUT.

  CASE sy-ucomm.
    WHEN 'SAIR' OR 'BACK1'.

      PERFORM f_dequeue_ezob_bloq_mat USING gs_9100-ebeln gs_9100-matnr.
      PERFORM f_dequeue_emekpoe       USING gs_9100-ebeln gs_9100-ebelp.

      CLEAR: gs_9000,
             gs_9100,
             gt_ekko,
             gs_ekko,
             gt_t311,
             gs_t311,
             gt_ekpo,
             gt_lqua,
             gs_lqua,
             gt_total,
             gv_palete.

      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.
