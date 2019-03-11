*----------------------------------------------------------------------*
***INCLUDE ZWMR_PICKING_SEM_OT_F01.
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Form F_VERIFICAR_USUARIO
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_verificar_usuario CHANGING c_erro.

  CLEAR: c_erro,
         gs_lrf_wkqu.

  SELECT SINGLE *
    FROM lrf_wkqu
    INTO gs_lrf_wkqu
   WHERE bname = sy-uname
     AND statu = 'X'.

  IF sy-subrc IS NOT INITIAL.
    "USUÁRIO NÃO ENCONTRADO
    c_erro = abap_true.
    PERFORM f_exibir_msg USING 'ZWM_BR' '001' '' '' '' '' '0999' .
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form F_EXIBIR_MSG
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*      -->P_I_MSGID  text
*      -->P_I_MSGV1  text
*      -->P_I_MSGV2  text
*      -->P_I_MSGV  text
*      -->P_I_MSGV4  text
*      -->P_TELA  text
*&---------------------------------------------------------------------*
FORM f_exibir_msg  USING    p_msgid TYPE t100-arbgb
                            p_msgno TYPE t100-msgnr
                            p_msgv1 TYPE sprot_u-var1
                            p_msgv2 TYPE sprot_u-var2
                            p_msgv3 TYPE sprot_u-var3
                            p_msgv4 TYPE sprot_u-var4
                            p_tela.

  DATA: lv_answer TYPE c.

  CALL FUNCTION 'CALL_MESSAGE_SCREEN'
    EXPORTING
      i_msgid          = p_msgid
      i_lang           = sy-langu
      i_msgno          = p_msgno
      i_msgv1          = p_msgv1
      i_msgv2          = p_msgv2
      i_msgv3          = p_msgv3
      i_msgv4          = p_msgv4
*     I_SEPERATE       = ' '
*     I_CONDENSE       = ' '
      i_message_screen = p_tela
*     I_LINE_SIZE      = 0
*     I_LINES          = 0
      i_non_lmob_envt  = 'X'
*     I_MODPL          =
    IMPORTING
      o_answer         = lv_answer
* TABLES
*     T_MSG_TEXT       =
    EXCEPTIONS
      invalid_message1 = 1
      OTHERS           = 2.

  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form F_VERIFICA_REF
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_verifica_ref CHANGING c_erro.

  CLEAR: c_erro,
         gs_ekko,
         gs_t311.

*Grupo de remessas
  SELECT SINGLE *
    FROM t311
    INTO gs_t311
   WHERE lgnum = gs_lrf_wkqu-lgnum
     AND refnr = gs_9000-ref.

  IF sy-subrc IS NOT INITIAL.
*Pedido de transferência
    SELECT SINGLE *
      FROM ekko
      INTO gs_ekko
     WHERE ebeln = gs_9000-ref.

    IF sy-subrc IS NOT INITIAL.
      gv_msgv1 = gs_9000-ref.
      "PEDIDO/GRUPO & INVÁLIDO
      c_erro = abap_true.
      PERFORM f_exibir_msg USING 'ZWM_BR' '002' gv_msgv1 '' '' '' '0999'.
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form F_PEDIDO_TRANSFERENCIA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_pedido_transferencia .

  DATA: lfrgke LIKE ekko-frgke.

  CLEAR: gv_werks.

* Buscar cento do operador
*
  SELECT lgnum, werks, lgort
    FROM t320 INTO TABLE @DATA(gt_t320)
   WHERE lgnum EQ @gs_lrf_wkqu-lgnum.

  REFRESH: gr_werks , gr_lgort.

  LOOP AT gt_t320 ASSIGNING FIELD-SYMBOL(<fs_data>).
    APPEND  VALUE #( sign = 'I'
                     option = 'EQ'
                     low = <fs_data>-werks ) TO gr_werks.

    APPEND  VALUE #( sign = 'I'
                     option = 'EQ'
                     low = <fs_data>-lgort ) TO gr_lgort.

  ENDLOOP.

***
***  SELECT SINGLE werks lgort
***    FROM t320
***    INTO (gv_werks, gv_lgort)
***   WHERE lgnum =  gs_lrf_wkqu-lgnum
***     AND werks <> space.


*1) Selecionar o número do pedido informado na tela verificado se o mesmo está liberado
*   e se o centro fornecedor é o mesmo do usuário.
***  SELECT COUNT(*)
***    FROM ekko
***   WHERE ebeln = gs_ekko-ebeln
***     AND reswk = gv_werks.

  SELECT SINGLE reswk , frgke INTO  (@gv_werks, @lfrgke )
    FROM ekko
     WHERE ebeln = @gs_ekko-ebeln.

  IF sy-subrc IS INITIAL.

    IF lfrgke = '1' OR  lfrgke = 'B'.
      "PEDIDO SEM LIBERAÇÃO
      PERFORM f_exibir_msg USING 'ZWM_BR' '004' '' '' '' '' '0999' .
      EXIT.
    ENDIF.

    READ TABLE gr_werks TRANSPORTING NO FIELDS WITH KEY low = gv_werks.
    IF sy-subrc <> 0.
      PERFORM f_exibir_msg USING 'ZWM_BR' '040' '' '' '' '' '0999' .
      EXIT.
    ELSE.

      SELECT SINGLE lgort  INTO gv_lgort  " O depósito de transferencia na saída não é definido.
         FROM ekpo                        " Assumir o mesmo depósito na entrada
        WHERE ebeln = gs_ekko-ebeln
          AND lgort IN gr_lgort.


*2)	Verificar se pedido de transferência tem itinerário
      SELECT COUNT(*)
        FROM ekpv
       WHERE ebeln = gs_ekko-ebeln
         AND route <> space.

      IF sy-subrc IS INITIAL.

*3)	Selecionar itens do pedido que são válidos
        SELECT *
          FROM ekpo
          INTO TABLE gt_ekpo
         WHERE ebeln = gs_ekko-ebeln "Numero de pedido informado pelo operador
           AND pstyp = '7'           "Item de transferência
           AND elikz = space.        "Flag de recebimento final

        IF sy-subrc IS INITIAL.
*        SORT gt_ekpo BY matnr ebelp.
          CALL SCREEN 9100.
        ELSE.
          "PEDIDO DE TRANSFERÊNCIA INVÁLIDO
          PERFORM f_exibir_msg USING 'ZWM_BR' '005' '' '' '' '' '0999' .
        ENDIF.
      ELSE.
        "PEDIDO SEM TRANSPORTE
        PERFORM f_exibir_msg USING 'ZWM_BR' '005' '' '' '' '' '0999' .
      ENDIF.
    ENDIF.
  ELSE.
    "PEDIDO SEM TRANSPORTE
    PERFORM f_exibir_msg USING 'ZWM_BR' '003' ''  '' '' '' '0999' .

  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form F_GRUPO_REMESSAS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_grupo_remessas .

  DATA: ls_total_aux TYPE ty_total_aux.

  CLEAR: gt_total,
         gv_werks,
         gv_item.

* Buscar cento do operador
  SELECT SINGLE werks
    FROM t320
    INTO gv_werks
   WHERE lgnum =  gs_lrf_wkqu-lgnum
     AND werks <> space.

  DATA: lt_total     TYPE TABLE OF l2sktotal.
  DATA: lt_delitem   TYPE TABLE OF l2skdlitem.


*João Bitar - joaobn - 03/04/2018 - limpeza da tabela com chave duplicada
*inicio
*    DELETE FROM ztwm_saldo_trans
*          WHERE ebeln = gs_9000-ref.
*fim


*1)	Buscar as quantidades consolidadas do grupo.
  CALL FUNCTION 'L_2_STEP_QUANTITY_REMOVAL'
    EXPORTING
      i_lgnum                       = gs_lrf_wkqu-lgnum
      i_refnr                       = gs_9000-ref
    TABLES
      t_delitem                     = lt_delitem
      t_total                       = lt_total
    EXCEPTIONS
      refnr_no_found                = 1
      refnr_documents_no_found      = 2
      no_relevant_for_2step_picking = 3
      item_for_removal_not_found    = 4
      OTHERS                        = 5.

  LOOP AT lt_delitem INTO DATA(ls_delitem).
    APPEND INITIAL LINE TO gt_grupo ASSIGNING FIELD-SYMBOL(<fs_grupo>).
    <fs_grupo>-grupo = ls_delitem-refnr.
    <fs_grupo>-vbeln = ls_delitem-vbeln.
    <fs_grupo>-matnr = ls_delitem-matnr.
  ENDLOOP.

  SELECT *
    INTO TABLE @DATA(lt_saldo_trans)
    FROM ztwm_saldo_trans
    WHERE ebeln = @gs_9000-ref.


  LOOP AT lt_total INTO DATA(ls_total).

    DATA(l_ebelp) = sy-tabix.

    MOVE-CORRESPONDING ls_total TO ls_total_aux.
    APPEND ls_total_aux TO gt_total.

    READ TABLE lt_saldo_trans TRANSPORTING NO FIELDS WITH KEY matnr = ls_total-matnr.
    IF sy-subrc <> 0.
      APPEND VALUE #( ebeln = ls_total-refnr
                     ebelp = l_ebelp
                     matnr = ls_total-matnr
                     charg = ls_total-charg
                     bestq = ls_total-bestq  ) TO lt_saldo_trans.


      MODIFY ztwm_saldo_trans FROM TABLE lt_saldo_trans.
    ENDIF.

  ENDLOOP.

*--------------------------------------------------------------------
*--------------------------------------------------------------------
*--------------------------------------------------------------------
  DATA: lv_material TYPE sprot_u-var1.


  IF ( NOT lt_delitem[] IS INITIAL ).

    SELECT vbeln, posnr, matnr,
      lfimg, lvsta, wbsta
      INTO TABLE @DATA(lt_check)
        FROM lips
          FOR ALL ENTRIES IN @lt_delitem
            WHERE vbeln = @lt_delitem-vbeln
              AND wbsta <> 'C'.


    SORT lt_check BY  lvsta matnr.
    DELETE lt_check WHERE lvsta <> 'A'.  "Status de pick de WM - NÃO EFETUADO ==> A

    LOOP AT gt_total INTO gs_total.
      READ TABLE lt_check INTO DATA(ls_check) WITH KEY matnr = gs_total-matnr.

      IF ( sy-subrc <> 0 ).
        DELETE gt_total WHERE matnr = gs_total-matnr.
      ENDIF.
    ENDLOOP.

    IF gt_total[] IS INITIAL.
      CLEAR: lv_material.
      lv_material = gs_9000-ref.
      PERFORM f_exibir_msg USING 'ZWM_BR' '041' lv_material '' '' '' '0999'.
      RETURN.
    ENDIF.

* Ajustar a quantidade a separar do material
    gs_total-ofmna = 0.

    MODIFY gt_total  FROM gs_total TRANSPORTING ofmna  WHERE ofmna > 0.

    LOOP AT lt_check INTO ls_check.

      READ TABLE gt_total INTO gs_total WITH KEY matnr = ls_check-matnr.
      IF sy-subrc = 0.
        gs_total-ofmna =  gs_total-ofmna + ls_check-lfimg.
        MODIFY gt_total FROM gs_total TRANSPORTING ofmna  WHERE matnr = ls_check-matnr.

      ENDIF.
    ENDLOOP.
  ENDIF.


*--------------------------------------------------------------------
*--------------------------------------------------------------------
*--------------------------------------------------------------------
  TYPES: BEGIN OF ty_lqua.
  TYPES: lgnum TYPE lqua-lgnum.
  TYPES: matnr TYPE lqua-matnr.
  TYPES: werks TYPE lqua-werks.
  TYPES: verme TYPE lqua-verme.
  TYPES: END OF ty_lqua.

  TYPES: BEGIN OF ty_total.
  TYPES: lgnum TYPE l2sktotal-lgnum.
  TYPES: matnr TYPE l2sktotal-matnr.
  TYPES: werks TYPE l2sktotal-werks.
  TYPES: lgort TYPE l2sktotal-lgort.
  TYPES: gemng TYPE l2sktotal-gemng.
  TYPES: END OF ty_total.

  DATA: lt_zztotal TYPE TABLE OF ty_total.
  DATA: ls_zztotal TYPE ty_total.
  DATA: lt_zzlqua TYPE TABLE OF ty_lqua.
  DATA: lt_collect_lqua TYPE TABLE OF ty_lqua.

  DATA: lv_labst TYPE mard-labst.

*--- ler os centros da tvarv
  SELECT *
    INTO TABLE @DATA(lt_stvarv)
      FROM tvarvc
        WHERE name = 'ZMM_WERKS_LGORT_CJ'
          AND type = 'S'.

  IF ( sy-subrc = 0 ).

*--- validar se o centro é de cajamar
    LOOP AT gt_total INTO DATA(ls_ztotal).
      READ TABLE lt_stvarv INTO DATA(ls_stvarv) WITH KEY low = ls_ztotal-werks.
      EXIT.
    ENDLOOP.

    IF ( ls_stvarv-low IS NOT INITIAL ).
*--- centro é de cajamar
      SELECT *
        INTO TABLE @DATA(lt_zzmard)
          FROM mard
            FOR ALL ENTRIES IN @gt_total
              WHERE matnr = @gt_total-matnr
                AND werks = @gt_total-werks
                AND lgort = @gt_total-lgort
                AND labst > 0.

      IF ( sy-subrc <> 0 ).
        READ TABLE gt_total INTO ls_ztotal INDEX 1.
        lv_material = ls_ztotal-matnr.
        PACK lv_material TO lv_material.
        CONDENSE lv_material NO-GAPS.
        PERFORM f_exibir_msg USING 'ZWM_BR' '039' lv_material '' '' '' '0999'.
        RETURN.
      ENDIF.

      IF ( NOT lt_zzmard[] IS INITIAL ).
        SELECT lgnum matnr werks verme
          INTO TABLE lt_zzlqua
            FROM lqua
              FOR ALL ENTRIES IN gt_total
                WHERE lgnum = gs_lrf_wkqu-lgnum
                  AND matnr = gt_total-matnr
                  AND werks = gt_total-werks
                  AND lgort = gt_total-lgort
                  AND bestq = ''
*                  AND lgtyp IN ('CF1', 'EBQ').
                  AND lgtyp IN gr_lgtyp.       "Substituição por TVARV

        IF ( sy-subrc = 0 ).
          CLEAR: lt_collect_lqua[], lt_collect_lqua.
          LOOP AT lt_zzlqua INTO DATA(ls_zzlqua).
            COLLECT ls_zzlqua INTO lt_collect_lqua.
          ENDLOOP.
        ENDIF.

        LOOP AT gt_total INTO ls_ztotal.
          CLEAR ls_zztotal.
          ls_zztotal-lgnum = ls_ztotal-lgnum.
          ls_zztotal-matnr = ls_ztotal-matnr.
          ls_zztotal-werks = ls_ztotal-werks.
          ls_zztotal-lgort = ls_ztotal-lgort.
          ls_zztotal-gemng = ls_ztotal-gemng.
          COLLECT ls_zztotal INTO lt_zztotal.
        ENDLOOP.

        LOOP AT lt_zzmard INTO DATA(ls_zzmard).

          CLEAR: ls_zzlqua, lv_labst.
          READ TABLE lt_zzlqua INTO ls_zzlqua WITH KEY lgnum = gs_lrf_wkqu-lgnum
                                                       matnr = ls_zzmard-matnr
                                                       werks = ls_zzmard-werks.

          CLEAR: ls_zztotal.
          READ TABLE lt_zztotal INTO ls_zztotal WITH KEY lgnum = gs_lrf_wkqu-lgnum
                                                         matnr = ls_zzmard-matnr
                                                         werks = ls_zzmard-werks
                                                         lgort = ls_zzmard-lgort.

          lv_labst = ( ls_zzmard-labst - ls_zzlqua-verme ).
          IF ( lv_labst < 0 ).
*          IF ( ls_zztotal-gemng > ( ls_zzmard-labst - ls_zzlqua-verme ) ).
            lv_material = ls_zzmard-matnr.
            PACK lv_material TO lv_material.
            CONDENSE lv_material NO-GAPS.
            PERFORM f_exibir_msg USING 'ZWM_BR' '039' lv_material '' '' '' '0999'.
            RETURN.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.
  ENDIF.
*--------------------------------------------------------------------
*--------------------------------------------------------------------
*--------------------------------------------------------------------

  IF gt_total IS NOT INITIAL.
    CALL SCREEN 9100.
  ELSE.
    "PEDIDO DE TRANSFERÊNCIA INVÁLIDO
    PERFORM f_exibir_msg USING 'ZWM_BR' '013' '' '' '' '' '0999' .
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form F_ENQUEUE_EMEKPOE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM f_enqueue_emekpoe  USING    p_ebeln
                                 p_ebelp
                        CHANGING c_bloq.

  DATA: lv_ebelp TYPE ekpo-ebelp.

  CLEAR: c_bloq.

  lv_ebelp = p_ebelp.

  CALL FUNCTION 'ENQUEUE_EMEKPOE'
    EXPORTING
      mode_ekpo      = 'E'
      mandt          = sy-mandt
      ebeln          = p_ebeln
      ebelp          = lv_ebelp
    EXCEPTIONS
      foreign_lock   = 1
      system_failure = 2
      error_message  = 3
      OTHERS         = 4.

  IF sy-subrc IS NOT INITIAL.
    c_bloq = abap_true.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form F_ENQUEUE_EZOB_BLOQ_MAT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM f_enqueue_ezob_bloq_mat  USING p_refnr
                                    p_matnr
                           CHANGING c_bloq.

  CLEAR: c_bloq.

  CALL FUNCTION 'ENQUEUE_EZOB_BLOQ_MAT'
    EXPORTING
      mode_ztwm_bloq_mat = 'E'
      mandt              = sy-mandt
      refnr              = p_refnr
      matnr              = p_matnr
    EXCEPTIONS
      foreign_lock       = 1
      system_failure     = 2
      OTHERS             = 3.

  IF sy-subrc IS NOT INITIAL.
    c_bloq = abap_true.
  ENDIF.


*  CALL FUNCTION 'ENQUEUE_EZOB_LIPS'
*    EXPORTING
*      mode_lips      = 'E'
*      mandt          = sy-mandt
*      refnr          = p_refnr
*      posnr          = p_posnr
*    EXCEPTIONS
*      foreign_lock   = 1
*      system_failure = 2
*      OTHERS         = 3.

*  IF sy-subrc IS NOT INITIAL.
*    c_bloq = abap_true.
*  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form F_DEQUEUE_EMEKPOE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM f_dequeue_emekpoe  USING    p_ebeln
                                 p_ebelp.

  DATA: lv_ebelp TYPE ekpo-ebelp.

  lv_ebelp = p_ebelp.

  CALL FUNCTION 'DEQUEUE_EMEKPOE'
    EXPORTING
      mode_ekpo = 'E'
      mandt     = sy-mandt
      ebeln     = p_ebeln
      ebelp     = lv_ebelp.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form F_DEQUEUE_EZOB_BLOQ_MAT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM f_dequeue_ezob_bloq_mat  USING p_refnr
                                    p_matnr.

  CALL FUNCTION 'DEQUEUE_EZOB_BLOQ_MAT'
    EXPORTING
      mode_ztwm_bloq_mat = 'E'
      mandt              = sy-mandt
      refnr              = p_refnr
      matnr              = p_matnr.

*  CALL FUNCTION 'DEQUEUE_EZOB_LIPS'
*    EXPORTING
*      mode_lips = 'E'
*      mandt     = sy-mandt
*      refnr     = p_refnr
*      posnr     = p_posnr.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form F_PREENCHE_TELA9100
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*      -->P_LS_EKPO  text
*&---------------------------------------------------------------------*
FORM f_preenche_tela9100  USING p_ekpo TYPE ekpo.

  DATA: lv_qtd_posicao TYPE lqua-verme.

  CLEAR: gs_9100.

  SELECT SINGLE maktg
    FROM makt
    INTO @DATA(lv_descricao)
   WHERE matnr = @p_ekpo-matnr
     AND spras = @sy-langu.

  gs_9100-ebeln      = p_ekpo-ebeln.
  gs_9100-ebelp      = p_ekpo-ebelp.
  gs_9100-lgort      = p_ekpo-lgort.
  gs_9100-matnr      = p_ekpo-matnr.
  gs_9100-matnr_tela = p_ekpo-matnr.
  gs_9100-descricao  = lv_descricao.
  gs_9100-und        = p_ekpo-meins.

  PERFORM f_saldo_pedido_atendido_ekbe USING p_ekpo-ebeln p_ekpo-ebelp CHANGING gv_ekbe_menge.
  PERFORM f_atualiza_dados_tela_lqua USING gs_9100-matnr gs_9100-posicao '' gv_ekbe_menge CHANGING gs_9100.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form F_GRP_PREENCHE_TELA9100
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*      -->P_LS_EKPO  text
*&---------------------------------------------------------------------*
FORM f_grp_preenche_tela9100  USING p_total TYPE ty_total_aux."l2sktotal.

  CLEAR: gs_9100.

  SELECT SINGLE maktg
    FROM makt
    INTO @DATA(lv_descricao)
   WHERE matnr = @p_total-matnr
     AND spras = @sy-langu.

  READ TABLE gt_grupo
  INTO DATA(ls_grupo)
  WITH KEY grupo = p_total-refnr
           matnr = p_total-matnr.
  IF sy-subrc <> 0.
    CLEAR: ls_grupo.
  ELSE.
    gs_9100-vbeln = ls_grupo-vbeln.
  ENDIF.

  gs_9100-ebeln      = p_total-refnr.
  gs_9100-ebelp      = gv_item.
  gs_9100-lgort      = p_total-lgort.
  gs_9100-matnr      = p_total-matnr.
  gs_9100-matnr_tela = p_total-matnr.
  gs_9100-descricao  = lv_descricao.
  gs_9100-und        = p_total-meins.

  PERFORM f_grp_atualiza_dados_tela_lqua USING gs_9100-matnr gs_9100-posicao '' CHANGING gs_9100.

* SAP - MGS -  Reinaldo Carvalho - 07.12.2018 17:01:46 - Início
  gs_9100-posicao_v = gs_9100-posicao.
* SAP - MGS - Reinaldo Carvalho - 07.12.2018 17:01:49 - Fim
ENDFORM.

*&---------------------------------------------------------------------*
*& Form F_NEXT_9100
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM f_next_9100 .

  DESCRIBE TABLE gt_ekpo LINES DATA(lv_linhas).

  gv_item = gv_item + 1.

  IF gv_item > lv_linhas.
    gv_item = lv_linhas.
  ENDIF.

* Verificar se tem item maior que o atual
  TRY.
      DATA(ls_ekpo) = gt_ekpo[ gv_item ].
      PERFORM f_enqueue_emekpoe USING ls_ekpo-ebeln ls_ekpo-ebelp CHANGING gv_bloq.
      IF gv_bloq IS INITIAL.
        PERFORM f_dequeue_emekpoe   USING gs_9100-ebeln gs_9100-ebelp.
        PERFORM f_preenche_tela9100 USING ls_ekpo.
        EXIT.
      ENDIF.
    CATCH cx_sy_itab_line_not_found INTO DATA(lv_erro).
  ENDTRY.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form F_GRP_NEXT_9100
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM f_grp_next_9100 .

  DATA(lv_item_ant) = gv_item.

  DESCRIBE TABLE gt_total LINES DATA(lv_linhas).

  gv_item = gv_item + 1.

  IF gv_item > lv_linhas.
    gv_item = lv_linhas.
  ENDIF.

  DO lv_linhas TIMES.
    READ TABLE gt_total INTO DATA(ls_total) INDEX gv_item.
    IF sy-subrc IS INITIAL.
      IF ls_total-excl = 'X' .
        IF gv_item = lv_linhas.
          gv_item = lv_item_ant.
        ELSE.
          gv_item = gv_item + 1.
        ENDIF.
      ELSE.
        EXIT.
      ENDIF.
    ENDIF.
  ENDDO.

* Verificar se tem item maior que o atual
  TRY.
      ls_total = gt_total[ gv_item ].
      PERFORM f_enqueue_ezob_bloq_mat USING ls_total-refnr ls_total-matnr CHANGING gv_bloq.
      IF gv_bloq IS INITIAL.
        PERFORM f_dequeue_ezob_bloq_mat USING gs_9100-ebeln gs_9100-matnr.
        PERFORM f_grp_preenche_tela9100 USING ls_total.
        EXIT.
      ENDIF.
    CATCH cx_sy_itab_line_not_found INTO DATA(lv_erro).
  ENDTRY.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form F_BACK_9100
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM f_back_9100.

  gv_item = gv_item - 1.

  IF gv_item <= 0.
    gv_item = 1.
  ENDIF.

* Verificar se tem item menor que o atual
  TRY.
      DATA(ls_ekpo) = gt_ekpo[ gv_item ].
      PERFORM f_enqueue_emekpoe USING ls_ekpo-ebeln ls_ekpo-ebelp CHANGING gv_bloq.
      IF gv_bloq IS INITIAL.
        PERFORM f_dequeue_emekpoe   USING gs_9100-ebeln gs_9100-ebelp.
        PERFORM f_preenche_tela9100 USING ls_ekpo.
        EXIT.
      ENDIF.
    CATCH cx_sy_itab_line_not_found INTO DATA(lv_erro).
  ENDTRY.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form F_GRP_BACK_9100
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM f_grp_back_9100.

  DATA(lv_item_ant) = gv_item.

  DESCRIBE TABLE gt_total LINES DATA(lv_linhas).

  gv_item = gv_item - 1.

  IF gv_item <= 0.
    gv_item = 1.
  ENDIF.

  DO lv_linhas TIMES.
    READ TABLE gt_total INTO DATA(ls_total) INDEX gv_item.
    IF sy-subrc IS INITIAL.
      IF ls_total-excl = 'X' .
        IF gv_item = 1.
          gv_item = lv_item_ant.
        ELSE.
          gv_item = gv_item - 1.
        ENDIF.
      ELSE.
        EXIT.
      ENDIF.
    ENDIF.
  ENDDO.

* Verificar se tem item menor que o atual
  TRY.
      ls_total = gt_total[ gv_item ].
      PERFORM f_enqueue_ezob_bloq_mat USING ls_total-refnr ls_total-matnr CHANGING gv_bloq.
      IF gv_bloq IS INITIAL.
        PERFORM f_dequeue_ezob_bloq_mat USING gs_9100-ebeln gs_9100-matnr.
        PERFORM f_grp_preenche_tela9100 USING ls_total.
        EXIT.
      ENDIF.
    CATCH cx_sy_itab_line_not_found INTO DATA(lv_erro).
  ENDTRY.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form F_ENTER_9000
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM f_enter_9000 .

  PERFORM f_select_lqua.

  LOOP AT gt_ekpo INTO DATA(ls_ekpo) FROM gv_item.
    gv_item = sy-tabix.

*4)	Validar se outro usuário já esta com o item na tela. Não permitir dois ou mais usuários separar o mesmo item
    PERFORM f_enqueue_emekpoe USING ls_ekpo-ebeln ls_ekpo-ebelp CHANGING gv_bloq.
    IF gv_bloq IS INITIAL.
      PERFORM f_preenche_tela9100 USING ls_ekpo.
      EXIT.
    ENDIF.
  ENDLOOP.

  IF gt_ekpo[] IS INITIAL.
    PERFORM f_exibir_msg USING 'ZWM_BR' '033' '' '' '' '' '0999' .
    LEAVE TO SCREEN 0.
  ELSE.

    IF gs_9100 IS INITIAL.
      "NENHUM ITEM DISPONÍVEL
      sy-msgv2 = gs_9000-ref.
      PERFORM f_exibir_msg USING 'ZWM_BR' '042' sy-msgv2 sy-msgv1 '' '' '0999' .

*    PERFORM f_exibir_msg USING 'ZWM_BR' '007' '' '' '' '' '0999' .
      LEAVE TO SCREEN 0.
    ENDIF.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form F_GRP_ENTER_9000
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM f_grp_enter_9000.

*GRO
*  PERFORM f_grp_select_lqua.
  PERFORM f_grp_select_lqua_new.
*GRO

  LOOP AT gt_total INTO DATA(ls_total) WHERE excl = space.
    gv_item = sy-tabix.
*4)  Validar se outro usuário já esta com o item na tela. Não permitir dois ou mais usuários separar o mesmo item
    PERFORM f_enqueue_ezob_bloq_mat USING ls_total-refnr ls_total-matnr CHANGING gv_bloq.
    IF gv_bloq IS INITIAL.
      PERFORM f_grp_preenche_tela9100 USING ls_total.
      EXIT.
    ENDIF.
  ENDLOOP.

  READ TABLE gt_total TRANSPORTING NO FIELDS WITH KEY excl = space.
  IF sy-subrc = 0.

    IF gs_9100 IS INITIAL.
      "NENHUM ITEM DISPONÍVEL
      sy-msgv2 = gs_9000-ref.
      PERFORM f_exibir_msg USING 'ZWM_BR' '042' sy-msgv2 sy-msgv1 '' '' '0999' .
      LEAVE TO SCREEN 0.
    ENDIF.
  ELSE.
    PERFORM f_exibir_msg USING 'ZWM_BR' '033' '' '' '' '' '0999' .
    LEAVE TO SCREEN 0.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form F_ENTER_VALIDA_9100
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM f_enter_valida_9100 .

  DATA: lv_material    TYPE mara-matnr,
        exp_material   TYPE c LENGTH 6,
        exp_lote       TYPE c LENGTH 10,
        imp_cod_barras TYPE c LENGTH 35,
        len            TYPE i,
        lv_lenum       TYPE ltap-vlenr,
        leitura_pallet TYPE c LENGTH 3.

  CONSTANTS: c_tvarv_deposito_pallet TYPE c LENGTH 18 VALUE 'ZWM_LEITURA_PALLET'.

  CLEAR: gv_erro.


  SELECT SINGLE low
    FROM tvarvc
    INTO leitura_pallet
  WHERE name = c_tvarv_deposito_pallet.

  IF gs_9100-posicao_v IS NOT INITIAL.
*Valida Posição
    IF gs_9100-ud_v IS INITIAL.
      TRY.
          gs_lqua = gt_lqua[ lgpla = gs_9100-posicao_v matnr = gs_9100-matnr ].
          gs_9100-posicao_v = gs_lqua-lgpla.
          IF gv_palete IS INITIAL.
            SET CURSOR FIELD 'GS_9100-MATNR_V'.
          ELSE.
            SET CURSOR FIELD 'GS_9100-UD_V'.
          ENDIF.
        CATCH cx_sy_itab_line_not_found INTO DATA(lv_erro).
          CLEAR: gs_9100-posicao_v.
*          SET CURSOR FIELD 'GS_9100-POSICAO_V'.
          gv_erro = abap_true.
          "POSIÇÃO INVÁLIDA
          PERFORM f_exibir_msg USING 'ZWM_BR' '009' '' '' '' '' '0999'.
      ENDTRY.
    ELSE.
*Valida UD/PALETE
      TRY.

          CLEAR: len.

          len = strlen( gs_9100-ud_v ).


          IF len = 35.
            SHIFT gs_9100-ud_v BY 25 PLACES LEFT.
          ENDIF.

          CALL FUNCTION 'CONVERSION_EXIT_LENUM_INPUT'
            EXPORTING
              input           = gs_9100-ud_v
            IMPORTING
              output          = lv_lenum
            EXCEPTIONS
              check_failed    = 1
              not_numeric     = 2
              t344_get_failed = 3
              wrong_length    = 4
              OTHERS          = 5.

          gs_lqua = gt_lqua[ lgpla = gs_9100-posicao_v matnr = gs_9100-matnr lenum = lv_lenum ].
          SET CURSOR FIELD 'GS_9100-MATNR_V'.
        CATCH cx_sy_itab_line_not_found INTO lv_erro.
          CLEAR:  gs_9100-posicao_v, gs_9100-ud_v.
          SET CURSOR FIELD 'GS_9100-POSICAO_V'.
          gv_erro = abap_true.
          "POSIÇÃO E/OU UD/PALETE INVÁLIDO
          PERFORM f_exibir_msg USING 'ZWM_BR' '017' '' '' '' '' '0999'.
      ENDTRY.
    ENDIF.
  ENDIF.

  CLEAR: lv_material,
         exp_material,
         exp_lote,
         imp_cod_barras.

  IF gs_9100-matnr_v IS NOT INITIAL.

*INICIO alteração bitar chamado I1804-0076 data 25/04/2018
    IF gs_lrf_wkqu-lgnum = leitura_pallet.
      imp_cod_barras = gs_9100-matnr_v(6).
    ELSE.
      imp_cod_barras = gs_9100-matnr_v.
    ENDIF.
*FIM alteração bitar chamado I1804-0076 data 25/04/2018


    CALL FUNCTION 'ZFMWM_VALIDA_MAT_ETIQUETA'
      EXPORTING
        i_cod_barras               = imp_cod_barras
      IMPORTING
        e_material                 = exp_material
        e_lote                     = exp_lote
      EXCEPTIONS
        ex_material_nao_encontrado = 1
        ex_cod_barras_invalido     = 2
        OTHERS                     = 3.

    IF sy-subrc IS INITIAL.
      CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
        EXPORTING
          input        = exp_material
        IMPORTING
          output       = lv_material
        EXCEPTIONS
          length_error = 1
          OTHERS       = 2.

      IF lv_material IS NOT INITIAL AND
         lv_material = gs_9100-matnr.


        IF gs_9100-lote IS NOT INITIAL.
          SET CURSOR FIELD 'GS_9100-LOTE_V'.
        ELSE.
          SET CURSOR FIELD 'GS_9100-QTD_V'.
        ENDIF.

      ELSE.
        CLEAR: gs_9100-matnr_v.
        SET CURSOR FIELD 'GS_9100-MATNR_V'.
        gv_erro = abap_true.
        "MATERIAL INVÁLIDO
        PERFORM f_exibir_msg USING 'ZWM_BR' '010' '' '' '' '' '0999'.
      ENDIF.

    ELSEIF sy-subrc = 1.
      CLEAR: gs_9100-matnr_v.
      SET CURSOR FIELD 'GS_9100-MATNR_V'.
      gv_erro = abap_true.
      "MATERIAL INVÁLIDO
      PERFORM f_exibir_msg USING 'ZWM_BR' '010' '' '' '' '' '0999'.
    ELSEIF sy-subrc = 2.
      CLEAR: gs_9100-matnr_v.
      SET CURSOR FIELD 'GS_9100-MATNR_V'.
      gv_erro = abap_true.
      "CÓDIGO DE BARRAS INVÁLIDO
      PERFORM f_exibir_msg USING 'ZWM_BR' '019' '' '' '' '' '0999'.
    ENDIF.

  ENDIF.

  IF gs_9100-lote_v IS NOT INITIAL.
    IF gs_9100-lote_v = gs_9100-lote.
      SET CURSOR FIELD 'GS_9100-QTD_V'.
    ELSE.
      CLEAR: gs_9100-lote_v.
      SET CURSOR FIELD 'GS_9100-LOTE_V'.
      gv_erro = abap_true.
      "LOTE INVÁLIDO
      PERFORM f_exibir_msg USING 'ZWM_BR' '008' '' '' '' '' '0999'.
    ENDIF.
  ELSE.

    IF exp_lote IS NOT INITIAL.
      IF exp_lote = gs_9100-lote.
        gs_9100-lote_v = exp_lote.
        SET CURSOR FIELD 'GS_9100-QTD_V'.
      ELSE.
        CLEAR: gs_9100-lote_v.
        SET CURSOR FIELD 'GS_9100-LOTE_V'.
        gv_erro = abap_true.
        "LOTE INVÁLIDO
        PERFORM f_exibir_msg USING 'ZWM_BR' '008' '' '' '' '' '0999'.
      ENDIF.
    ENDIF.
  ENDIF.

  IF lv_material IS NOT INITIAL AND gs_9100-posicao_v IS NOT INITIAL.

    SELECT COUNT(*)
      FROM lqua
     WHERE lgnum = @gs_lrf_wkqu-lgnum
       AND matnr = @lv_material
       AND lgpla = @gs_9100-posicao_v.

    IF sy-subrc IS NOT INITIAL.
      CLEAR: gs_9100-matnr_v, gs_9100-posicao_v, gs_9100-ud_v.
*      SET CURSOR FIELD 'GS_9100-POSICAO_V'.
      gv_erro = abap_true.
      "MATERIAL x POSIÇÃO INVÁLIDO
      PERFORM f_exibir_msg USING 'ZWM_BR' '011' '' '' '' '' '0999'.
    ENDIF.

  ENDIF.

  IF gs_9100-qtd_v IS NOT INITIAL.
    IF gs_9100-qtd_v > gs_9100-qtd_disp OR gs_9100-qtd_v > gs_9100-qtd_nec.
      CLEAR: gs_9100-qtd_v.
      SET CURSOR FIELD 'GS_9100-QTD_V'.
      gv_erro = abap_true.
      "QUANTIDADE INFORMADA NÃO PODE SER MAIOR QUE QUANTIDADE DISP/NEC
      PERFORM f_exibir_msg USING 'ZWM_BR' '012' '' '' '' '' '0999'.
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_atualiza_dados_tela_lqua
*&---------------------------------------------------------------------*
*& textd
*&---------------------------------------------------------------------*
FORM f_atualiza_dados_tela_lqua USING p_matnr p_posicao_v p_ud_v p_ekbe_menge CHANGING c_9100 TYPE ty_9100.

  DATA: lv_saldo_trans TYPE ekpo-menge,
        lv_matnr_tela  TYPE matnr,
        lv_lenum       TYPE ltap-vlenr.

  TRY.
      IF p_posicao_v IS INITIAL.
* Selecionar o item mais antigo com saldo para exibir na tela como sugestão de separação do FIFO.
        SORT gt_lqua BY lgpla bdatu ASCENDING.
        gs_lqua = gt_lqua[ matnr = p_matnr ].
      ELSE.

        IF p_ud_v IS INITIAL.
          gs_lqua = gt_lqua[ matnr = p_matnr lgpla = p_posicao_v ].
        ELSE.

*          shift p_ud_v by 25 places left.

          CALL FUNCTION 'CONVERSION_EXIT_LENUM_INPUT'
            EXPORTING
              input           = p_ud_v
            IMPORTING
              output          = lv_lenum
            EXCEPTIONS
              check_failed    = 1
              not_numeric     = 2
              t344_get_failed = 3
              wrong_length    = 4
              OTHERS          = 5.

          gs_lqua = gt_lqua[ matnr = p_matnr lgpla = p_posicao_v lenum = lv_lenum ].
        ENDIF.
      ENDIF.

*9)	Verificar se o tipo de depósito tem controle de UD (Palete)
      PERFORM f_verifica_palete USING gs_lqua-lgnum gs_lqua-lgtyp.

      IF gs_lqua-charg IS NOT INITIAL.
        CLEAR: lv_matnr_tela.
        lv_matnr_tela = c_9100-matnr.
        PACK lv_matnr_tela TO lv_matnr_tela.
        CONDENSE: lv_matnr_tela NO-GAPS.
        CONCATENATE lv_matnr_tela '/' gs_lqua-charg INTO c_9100-matnr_tela SEPARATED BY space.
        c_9100-lote = gs_lqua-charg.
      ENDIF.

      c_9100-posicao  = gs_lqua-lgpla.
      c_9100-qtd_disp = gs_lqua-verme.

*6)  Descontar saldo de pedido separado.
      SELECT *
        FROM ztwm_saldo_trans
        INTO TABLE @DATA(lt_saldo_trans)
       WHERE ebeln = @gs_9100-ebeln
         AND ebelp = @gs_9100-ebelp.

      LOOP AT lt_saldo_trans INTO DATA(ls_saldo_trans).
        lv_saldo_trans = lv_saldo_trans + ls_saldo_trans-menge.
      ENDLOOP.

      DATA(ls_ekpo) = gt_ekpo[ ebeln = gs_9100-ebeln ebelp = gs_9100-ebelp ].
      gs_9100-qtd_nec = ls_ekpo-menge - lv_saldo_trans - p_ekbe_menge.

    CATCH cx_sy_itab_line_not_found INTO DATA(lv_erro).
  ENDTRY.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_grp_atualiza_dados_tela_lqua
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM f_grp_atualiza_dados_tela_lqua  USING p_matnr p_posicao_v p_ud_v CHANGING c_9100 TYPE ty_9100.

  DATA: lv_saldo_trans TYPE ekpo-menge,
        lv_matnr_tela  TYPE matnr,
        lv_lenum       TYPE ltap-vlenr.

  TRY.
      IF p_posicao_v IS INITIAL.
* Selecionar o item mais antigo com saldo para exibir na tela como sugestão de separação do FIFO.
        SORT gt_lqua BY lgpla bdatu ASCENDING.
        gs_lqua = gt_lqua[ matnr = p_matnr ].
      ELSE.
        IF p_ud_v IS INITIAL.
          gs_lqua = gt_lqua[ matnr = p_matnr lgpla = p_posicao_v ].
        ELSE.

          CALL FUNCTION 'CONVERSION_EXIT_LENUM_INPUT'
            EXPORTING
              input           = p_ud_v
            IMPORTING
              output          = lv_lenum
            EXCEPTIONS
              check_failed    = 1
              not_numeric     = 2
              t344_get_failed = 3
              wrong_length    = 4
              OTHERS          = 5.

          gs_lqua = gt_lqua[ matnr = p_matnr lgpla = p_posicao_v lenum = lv_lenum ].
        ENDIF.
      ENDIF.

*9)	Verificar se o tipo de depósito tem controle de UD (Palete)
      PERFORM f_verifica_palete USING gs_lqua-lgnum gs_lqua-lgtyp.

      IF gs_lqua-charg IS NOT INITIAL.
        CLEAR: lv_matnr_tela.
        lv_matnr_tela = c_9100-matnr.
        PACK lv_matnr_tela TO lv_matnr_tela.
        CONDENSE: lv_matnr_tela NO-GAPS.
        CONCATENATE lv_matnr_tela '/' gs_lqua-charg INTO c_9100-matnr_tela SEPARATED BY space.
        c_9100-lote = gs_lqua-charg.
      ENDIF.

      c_9100-posicao  = gs_lqua-lgpla.
      c_9100-qtd_disp = gs_lqua-verme.

*6)  Descontar saldo de pedido separado.
      SELECT *
        FROM ztwm_saldo_trans
        INTO TABLE @DATA(lt_saldo_trans)
       WHERE ebeln = @gs_9100-ebeln
         AND ebelp = @gs_9100-ebelp.

      LOOP AT lt_saldo_trans INTO DATA(ls_saldo_trans).
        lv_saldo_trans = lv_saldo_trans + ls_saldo_trans-menge.
      ENDLOOP.

      DATA(ls_total) = gt_total[ gs_9100-ebelp ]. "index
      gs_9100-qtd_nec = ls_total-ofmna - lv_saldo_trans.

    CATCH cx_sy_itab_line_not_found INTO DATA(lv_erro).
  ENDTRY.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form F_GERAR_OT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_gerar_ot .

  DATA: lt_saldo_trans TYPE TABLE OF ztwm_saldo_trans,
        lv_lgnum       TYPE ltak-lgnum,
        lv_bwlvs       TYPE ltak-bwlvs,
        lv_benum       TYPE ltak-benum,
        lv_matnr       TYPE ltap-matnr,
        lv_werks       TYPE ltap-werks,
        lv_lgort       TYPE ltap-lgort,
        lv_charg       TYPE ltap-charg,
        lv_bestq       TYPE ltap-bestq,
        lv_anfme       TYPE rl03t-anfme,
        lv_altme       TYPE ltap-altme,
        lv_zeugn       TYPE ltap-zeugn,
        lv_squit       TYPE rl03t-squit,
        lv_vltyp       TYPE ltap-vltyp,
        lv_vlpla       TYPE ltap-vlpla,
        lv_vlenr       TYPE ltap-vlenr.

  IF gv_erro           IS INITIAL AND
     gs_9100-posicao_v IS NOT INITIAL AND
     gs_9100-matnr_v   IS NOT INITIAL AND
     gs_9100-qtd_v     IS NOT INITIAL .

    IF gv_palete = abap_true AND gs_9100-ud_v IS INITIAL.

      SET CURSOR FIELD 'GS_9100-UD_V'.
      "OBRIGATÓRIO INFORMAR O UD/PALETE
      PERFORM f_exibir_msg USING 'ZWM_BR' '016' '' '' '' '' '0999'.

    ELSE.

      lv_lgnum = gs_lrf_wkqu-lgnum.
      lv_benum = gs_9100-ebeln.
      lv_zeugn = gs_9100-ebelp.
      lv_matnr = gs_9100-matnr.
      lv_anfme = gs_9100-qtd_v.
      lv_werks = gs_lqua-werks.
      lv_lgort = gs_lqua-lgort.
      lv_charg = gs_lqua-charg.
      lv_bestq = gs_lqua-bestq.
      lv_altme = gs_lqua-meins.
      lv_vltyp = gs_lqua-lgtyp.
      lv_vlpla = gs_lqua-lgpla.
      lv_squit = 'X'.
      lv_bwlvs = '950'.

      CALL FUNCTION 'CONVERSION_EXIT_LENUM_INPUT'
        EXPORTING
          input           = gs_9100-ud_v
        IMPORTING
          output          = lv_vlenr
        EXCEPTIONS
          check_failed    = 1
          not_numeric     = 2
          t344_get_failed = 3
          wrong_length    = 4
          OTHERS          = 5.

      SELECT SINGLE *
        FROM ztwm_saldo_trans
        INTO @DATA(ls_saldo_trans)
       WHERE ebeln = @gs_9100-ebeln
         AND ebelp = @gs_9100-ebelp
         AND matnr = @gs_9100-matnr
         AND charg = @gs_lqua-charg
         AND bestq = @gs_lqua-bestq.

      IF sy-subrc IS INITIAL.
        ls_saldo_trans-menge = ls_saldo_trans-menge + gs_9100-qtd_v.
      ELSE.
        ls_saldo_trans-menge = gs_9100-qtd_v.
      ENDIF.

      APPEND VALUE #( ebeln = gs_9100-ebeln
                      ebelp = gs_9100-ebelp
                      matnr = gs_9100-matnr
                      charg = gs_lqua-charg
                      bestq = gs_lqua-bestq
                      menge = ls_saldo_trans-menge ) TO lt_saldo_trans.

      CALL FUNCTION 'L_TO_CREATE_SINGLE'
        EXPORTING
          i_lgnum               = lv_lgnum
          i_bwlvs               = lv_bwlvs
          i_benum               = lv_benum
          i_matnr               = lv_matnr
          i_werks               = lv_werks
          i_lgort               = lv_lgort
          i_charg               = lv_charg
          i_bestq               = lv_bestq
          i_anfme               = lv_anfme
          i_altme               = lv_altme
          i_zeugn               = lv_zeugn
          i_squit               = lv_squit
          i_vltyp               = lv_vltyp
          i_vlpla               = lv_vlpla
          i_vlenr               = lv_vlenr
*      IMPORTING
*         e_tanum               = e_tanum
        EXCEPTIONS
          no_to_created         = 1
          bwlvs_wrong           = 2
          betyp_wrong           = 3
          benum_missing         = 4
          betyp_missing         = 5
          foreign_lock          = 6
          vltyp_wrong           = 7
          vlpla_wrong           = 8
          vltyp_missing         = 9
          nltyp_wrong           = 10
          nlpla_wrong           = 11
          nltyp_missing         = 12
          rltyp_wrong           = 13
          rlpla_wrong           = 14
          rltyp_missing         = 15
          squit_forbidden       = 16
          manual_to_forbidden   = 17
          letyp_wrong           = 18
          vlpla_missing         = 19
          nlpla_missing         = 20
          sobkz_wrong           = 21
          sobkz_missing         = 22
          sonum_missing         = 23
          bestq_wrong           = 24
          lgber_wrong           = 25
          xfeld_wrong           = 26
          date_wrong            = 27
          drukz_wrong           = 28
          ldest_wrong           = 29
          update_without_commit = 30
          no_authority          = 31
          material_not_found    = 32
          lenum_wrong           = 33
          OTHERS                = 34.

      IF sy-subrc IS INITIAL.

        MODIFY ztwm_saldo_trans FROM TABLE lt_saldo_trans.

        IF gs_ekko IS NOT INITIAL.     " Pedido de transferência
          CLEAR: gs_9100.
          PERFORM f_enter_9000.
        ELSEIF gs_t311 IS NOT INITIAL. " Grupo de remessas
          CLEAR: gs_9100.
          PERFORM f_grp_enter_9000.
        ENDIF.
*        SET CURSOR FIELD 'GS_9100-POSICAO_V'.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form F_SALDO_PEDIDO_ATENDIDO_EKBE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM f_saldo_pedido_atendido_ekbe USING p_ebeln p_ebelp CHANGING c_ekbe_menge.

  CLEAR: c_ekbe_menge.

*5)	Descontar saldo de pedido atendido.
  SELECT SUM( menge )
    FROM ekbe
    INTO @c_ekbe_menge
   WHERE ebeln = @p_ebeln
     AND ebelp = @p_ebelp
     AND bewtp = 'L'.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form F_SELECT_LQUA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_select_lqua .

  DATA: lv_qtd_nec TYPE ekpo-menge,
        lv_tabix   TYPE sy-tabix,
        lv_msg     TYPE symsgv.

  LOOP AT gt_ekpo INTO DATA(ls_ekpo).
    lv_tabix = sy-tabix.
    PERFORM f_verificar_qtd_nec USING ls_ekpo-ebeln ls_ekpo-ebelp CHANGING lv_qtd_nec.
    IF lv_qtd_nec = 0.
      DELETE gt_ekpo INDEX lv_tabix.
    ENDIF.
  ENDLOOP.

  CLEAR: gt_lqua.

*  DELETE gt_ekpo WHERE lgort <> gv_lgort.
*
*  IF gt_ekpo IS NOT INITIAL.

  SELECT *
    FROM lqua
    INTO TABLE @gt_lqua
    FOR ALL ENTRIES IN @gt_ekpo
   WHERE lgnum = @gs_lrf_wkqu-lgnum
     AND matnr = @gt_ekpo-matnr
     AND lgort = @gv_lgort
     AND werks = @gv_werks
     AND ( bestq = @space OR bestq = 'Q' )
     AND verme > 0.

*7)	Buscar o saldo do item na tabela LQUA para conferência do item  do pedido considerando os tipos de depósitos na tabela de controle de separação
* Tabela de controle de tipos de depósitos válidos para o processo de separação da expedição
  SELECT *
    FROM ztwm_ctrl_tp_dep
    INTO TABLE @DATA(lt_tp_dep).

  LOOP AT lt_tp_dep INTO DATA(ls_tp_dep).
    DELETE gt_lqua WHERE lgnum = ls_tp_dep-lgnum AND lgtyp = ls_tp_dep-lgtyp.
  ENDLOOP.


  SELECT *
    INTO TABLE @DATA(lt_lagp)
      FROM lagp
        FOR ALL ENTRIES IN @gt_lqua
          WHERE lgnum = @gt_lqua-lgnum
            AND lgtyp = @gt_lqua-lgtyp
            AND lgpla = @gt_lqua-lgpla.


  LOOP AT lt_lagp INTO DATA(ls_lagp) WHERE skzua = abap_true OR skzsi = abap_true.

    DELETE gt_lqua WHERE lgnum = ls_lagp-lgnum
                     AND lgtyp = ls_lagp-lgtyp
                     AND lgpla = ls_lagp-lgpla.

  ENDLOOP.


  LOOP AT gt_ekpo INTO ls_ekpo.
    lv_tabix = sy-tabix.
    READ TABLE gt_lqua INTO DATA(ls_lqua) WITH KEY matnr = ls_ekpo-matnr.
    IF sy-subrc IS NOT INITIAL.
      DELETE gt_ekpo INDEX lv_tabix.
    ENDIF.
  ENDLOOP.




*  ELSE.
*
*    lv_msg = gv_lgort.
*    "DEPÓSITO X DO USUÁRIO DIFERENTE DO PEDIDO
*    PERFORM f_exibir_msg USING 'ZWM_BR' '014' lv_msg '' '' '' '0999' .
*    LEAVE TO SCREEN 0.
*  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form F_GRP_SELECT_LQUA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_grp_select_lqua .

  DATA: lv_qtd_nec TYPE ekpo-menge,
        lv_tabix   TYPE sy-tabix.

  LOOP AT gt_total INTO DATA(ls_total).
    lv_tabix = sy-tabix.
    PERFORM f_grp_verificar_qtd_nec USING ls_total-refnr ls_total-matnr lv_tabix CHANGING lv_qtd_nec.
    IF lv_qtd_nec = 0.
      DELETE gt_total INDEX lv_tabix.
    ENDIF.
  ENDLOOP.

  CLEAR: gt_lqua.

  SELECT *
    FROM lqua
    INTO TABLE @gt_lqua
    FOR ALL ENTRIES IN @gt_total
   WHERE lgnum = @gs_lrf_wkqu-lgnum
     AND matnr = @gt_total-matnr
     AND lgort = @gt_total-lgort
     AND werks = @gv_werks
     AND ( bestq = @space OR bestq = 'Q' )
     AND verme > 0.

*7)  Buscar o saldo do item na tabela LQUA para conferência do item  do pedido considerando os tipos de depósitos na tabela de controle de separação
* Tabela de controle de tipos de depósitos válidos para o processo de separação da expedição
  SELECT *
    FROM ztwm_ctrl_tp_dep
    INTO TABLE @DATA(lt_tp_dep).

  LOOP AT lt_tp_dep INTO DATA(ls_tp_dep).
    DELETE gt_lqua WHERE lgnum = ls_tp_dep-lgnum AND lgtyp = ls_tp_dep-lgtyp.
  ENDLOOP.

  SELECT *
    INTO TABLE @DATA(lt_lagp)
      FROM lagp
        FOR ALL ENTRIES IN @gt_lqua
          WHERE lgnum = @gt_lqua-lgnum
            AND lgtyp = @gt_lqua-lgtyp
            AND lgpla = @gt_lqua-lgpla.


  LOOP AT lt_lagp INTO DATA(ls_lagp) WHERE skzua = abap_true OR skzsi = abap_true.

    DELETE gt_lqua WHERE lgnum = ls_lagp-lgnum
                     AND lgtyp = ls_lagp-lgtyp
                     AND lgpla = ls_lagp-lgpla.

  ENDLOOP.

  LOOP AT gt_total INTO ls_total.
    lv_tabix = sy-tabix.
    READ TABLE gt_lqua INTO DATA(ls_lqua) WITH KEY matnr = ls_total-matnr.
    IF sy-subrc IS NOT INITIAL.
      DELETE gt_total INDEX lv_tabix.
    ENDIF.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form F_VERIFICAR_QTD_NEC
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*      -->P_LS_EKPO_EBELN  text
*      -->P_LS_EKPO_EBELP  text
*      <--P_LV_QTD_NEC  text
*&---------------------------------------------------------------------*
FORM f_verificar_qtd_nec  USING    p_ekpo_ebeln
                                   p_ekpo_ebelp
                          CHANGING p_qtd_nec.

  DATA: lv_saldo_trans TYPE ekpo-menge.

  PERFORM f_saldo_pedido_atendido_ekbe USING p_ekpo_ebeln p_ekpo_ebelp CHANGING gv_ekbe_menge.

  TRY.
*6)  Descontar saldo de pedido separado.
      SELECT *
        FROM ztwm_saldo_trans
        INTO TABLE @DATA(lt_saldo_trans)
       WHERE ebeln = @p_ekpo_ebeln
         AND ebelp = @p_ekpo_ebelp.

      LOOP AT lt_saldo_trans INTO DATA(ls_saldo_trans).
        lv_saldo_trans = lv_saldo_trans + ls_saldo_trans-menge.
      ENDLOOP.

      DATA(ls_ekpo) = gt_ekpo[ ebeln = p_ekpo_ebeln ebelp = p_ekpo_ebelp ].

      p_qtd_nec = ls_ekpo-menge - lv_saldo_trans - gv_ekbe_menge.

    CATCH cx_sy_itab_line_not_found INTO DATA(lv_erro).
  ENDTRY.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form F_GRP_VERIFICAR_QTD_NEC
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM f_grp_verificar_qtd_nec  USING    p_refnr
                                       p_matnr
                                       p_tabix
                              CHANGING p_qtd_nec.

  DATA: lv_saldo_trans TYPE ekpo-menge.

  TRY.
*6)  Descontar saldo de pedido separado.
      SELECT *
        FROM ztwm_saldo_trans
        INTO TABLE @DATA(lt_saldo_trans)
       WHERE ebeln = @p_refnr
         AND matnr = @p_matnr.
*         AND ebelp = @p_tabix.

      LOOP AT lt_saldo_trans INTO DATA(ls_saldo_trans).
        lv_saldo_trans = lv_saldo_trans + ls_saldo_trans-menge.
      ENDLOOP.

      DATA(ls_total) = gt_total[ p_tabix ].

      p_qtd_nec = ls_total-ofmna - lv_saldo_trans .

*     Para não dar Dump no processo
      IF  p_qtd_nec  < 0.
        p_qtd_nec = 0.
      ENDIF.

    CATCH cx_sy_itab_line_not_found INTO DATA(lv_erro).
  ENDTRY.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form F_VERIFICA_PALETE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM f_verifica_palete USING p_lgnum p_lgtyp.

  SELECT COUNT(*)
    FROM t331
   WHERE lgnum = p_lgnum
     AND lgtyp = p_lgtyp
     AND lenvw = abap_true.

  IF sy-subrc IS INITIAL.
    LOOP AT SCREEN.
      CASE screen-name.
        WHEN 'GS_9100-UD_V' OR 'L_UD'.
          gv_palete = abap_true.
          screen-output    = 1.
          screen-active    = 1.
          screen-invisible = 0.
          MODIFY SCREEN.
        WHEN OTHERS.
      ENDCASE.
    ENDLOOP.
  ELSE.
    LOOP AT SCREEN.
      CASE screen-name.
        WHEN 'GS_9100-UD_V' OR 'L_UD'.
          gv_palete = abap_false.
          screen-output    = 0.
          screen-active    = 0.
          screen-invisible = 1.
          MODIFY SCREEN.
        WHEN OTHERS.
      ENDCASE.
    ENDLOOP.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form F_GRP_SELECT_LQUA_NEW
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_grp_select_lqua_new .

  DATA: lv_qtd_nec TYPE ekpo-menge,
        lv_tabix   TYPE sy-tabix.

  LOOP AT gt_total INTO DATA(ls_total).
    lv_tabix = sy-tabix.
    PERFORM f_grp_verificar_qtd_nec USING ls_total-refnr ls_total-matnr lv_tabix CHANGING lv_qtd_nec.
    IF lv_qtd_nec = 0.
      ls_total-excl = 'X'.
      MODIFY gt_total FROM ls_total INDEX lv_tabix.
    ENDIF.
  ENDLOOP.

  CLEAR: gt_lqua.

  SELECT *
    FROM lqua
    INTO TABLE @gt_lqua
    FOR ALL ENTRIES IN @gt_total
   WHERE lgnum = @gs_lrf_wkqu-lgnum
     AND matnr = @gt_total-matnr
     AND lgort = @gt_total-lgort
     AND werks = @gv_werks
     AND bestq = @space
     AND lgtyp IN ('PA1', 'PA2')
     AND verme > 0.

*7)  Buscar o saldo do item na tabela LQUA para conferência do item  do pedido considerando os tipos de depósitos na tabela de controle de separação
* Tabela de controle de tipos de depósitos válidos para o processo de separação da expedição
  SELECT *
    FROM ztwm_ctrl_tp_dep
    INTO TABLE @DATA(lt_tp_dep).

  LOOP AT lt_tp_dep INTO DATA(ls_tp_dep).
    DELETE gt_lqua WHERE lgnum = ls_tp_dep-lgnum AND lgtyp = ls_tp_dep-lgtyp.
  ENDLOOP.


  SELECT *
    INTO TABLE @DATA(lt_lagp)
      FROM lagp
        FOR ALL ENTRIES IN @gt_lqua
          WHERE lgnum = @gt_lqua-lgnum
            AND lgtyp = @gt_lqua-lgtyp
            AND lgpla = @gt_lqua-lgpla.


  LOOP AT lt_lagp INTO DATA(ls_lagp) WHERE skzua = abap_true OR skzsi = abap_true.

    DELETE gt_lqua WHERE lgnum = ls_lagp-lgnum
                     AND lgtyp = ls_lagp-lgtyp
                     AND lgpla = ls_lagp-lgpla.

  ENDLOOP.

*GRO
*  LOOP AT gt_total INTO ls_total.
*    lv_tabix = sy-tabix.
*    READ TABLE gt_lqua INTO DATA(ls_lqua) WITH KEY matnr = ls_total-matnr.
*    IF sy-subrc IS NOT INITIAL.
*      DELETE gt_total INDEX lv_tabix.
*    ENDIF.
*  ENDLOOP.

  LOOP AT gt_total INTO ls_total." WHERE excl = 'X'.
    lv_tabix = sy-tabix.
    IF ls_total-excl = 'X'.
      READ TABLE gt_lqua INTO DATA(ls_lqua) WITH KEY matnr = ls_total-matnr.
      IF sy-subrc IS INITIAL.
        DELETE gt_lqua WHERE matnr = ls_total-matnr AND lgort = ls_total-lgort.
      ENDIF.
    ELSE.
      READ TABLE gt_lqua INTO ls_lqua WITH KEY matnr = ls_total-matnr.
      IF sy-subrc IS NOT INITIAL.

*       Marcar item como totalmente atendido para ser desconsiderado
*       Isto garante que o item não será selecionado quando não houver saldo
*       foi alterado a lógica de exclusão para item atendido, assim o ponteiro interno
*       do indice não se perde.
        ls_total-excl = 'X'.
        MODIFY gt_total FROM ls_total INDEX lv_tabix.

*        DELETE gt_total INDEX lv_tabix.
      ENDIF.
    ENDIF.
  ENDLOOP.
*GRO

ENDFORM.
*****************************************************************************************
FORM f_ajusta_saldo_trans USING p_ref.

  DATA: a            TYPE i,
        lqua_cf1_aux TYPE TABLE OF lqua,
        lqua_cf1     TYPE TABLE OF lqua,
        lqua_aux1    TYPE lqua,
        lqua_aux     TYPE lqua.


  SELECT * INTO TABLE @lqua_cf1
     FROM lqua
      WHERE lgnum = @gs_lrf_wkqu-lgnum
*        AND lgtyp = 'CF1'
        AND lgtyp = @gv_lgtyp_1   "Substituição por TVARV
        AND lgpla = @p_ref.

  SELECT lgnum, refnr, rbnum INTO TABLE @DATA(lt311a)
       FROM t311a
         WHERE lgnum = @gs_lrf_wkqu-lgnum
           AND refnr = @p_ref.

  IF lt311a[] IS INITIAL.    " É pedido de transferencia
    SELECT * APPENDING TABLE @lqua_cf1
     FROM lqua
      WHERE lgnum = @gs_lrf_wkqu-lgnum
*        AND lgtyp = 'EBQ'
        AND lgtyp = @gv_lgtyp_2  "Substituição por TVARV
        AND lgpla = @p_ref.
  ELSE.
    SELECT * FROM lqua
        APPENDING TABLE @lqua_cf1
          FOR ALL ENTRIES IN @lt311a
        WHERE lgnum = @lt311a-lgnum
*          AND lgtyp = 'EBQ'
          AND lgtyp = @gv_lgtyp_2  "Substituição por TVARV
          AND lgpla = @lt311a-rbnum.

  ENDIF.

  MOVE lqua_cf1[] TO lqua_cf1_aux[].
  CLEAR lqua_cf1.

  LOOP AT lqua_cf1_aux INTO lqua_aux1.
    lqua_aux-matnr = lqua_aux1-matnr.
    lqua_aux-bestq = lqua_aux1-bestq.
    lqua_aux-verme = lqua_aux1-verme.
    COLLECT lqua_aux  INTO lqua_cf1.
  ENDLOOP.


  IF lqua_cf1[] IS INITIAL.

    UPDATE ztwm_saldo_trans SET menge =  0
        WHERE ebeln = p_ref.
    COMMIT WORK AND WAIT.

  ELSE.

    SORT lqua_cf1 BY matnr bestq.

*joaobn - inclusão do delete para limpeza de dados duplicados - 05/04/2018
    DELETE FROM ztwm_saldo_trans
          WHERE ebeln = @p_ref AND menge = 0.
**************************************************************************


    SELECT * INTO TABLE @DATA(ztwm_saldo)
        FROM  ztwm_saldo_trans
          WHERE ebeln = @p_ref.

    IF  NOT ztwm_saldo[] IS INITIAL.

      LOOP AT ztwm_saldo INTO DATA(wsaldo_tran).

        READ TABLE lqua_cf1 INTO DATA(wlqua_cf1) WITH KEY matnr = wsaldo_tran-matnr
                                                          bestq = wsaldo_tran-bestq.


        IF sy-subrc = 0.


          UPDATE ztwm_saldo_trans SET menge =  wlqua_cf1-verme
                                 WHERE ebeln = wsaldo_tran-ebeln
                                   AND ebelp = wsaldo_tran-ebelp
                                   AND matnr = wsaldo_tran-matnr
                                   AND charg = wsaldo_tran-charg
                                   AND bestq = wsaldo_tran-bestq.


        ELSE.

          UPDATE ztwm_saldo_trans SET menge =  0
                               WHERE ebeln = wsaldo_tran-ebeln
                                 AND ebelp = wsaldo_tran-ebelp
                                 AND matnr = wsaldo_tran-matnr
                                 AND charg = wsaldo_tran-charg
                                 AND bestq = wsaldo_tran-bestq.

        ENDIF.
      ENDLOOP.
      COMMIT WORK AND WAIT.
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form F_SELECIONA_TVAVR
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM f_seleciona_tvavr .

* Objeto da TVARV
  DATA: o_tvarv TYPE REF TO lcl_tvarv.

  CREATE OBJECT o_tvarv
    EXPORTING
      prefix    = 'ZWM'
      separator = '_'.

  o_tvarv->get_parameter(
   EXPORTING
     suffix = 'PICKING_SOT_LGTYP_1'
   IMPORTING
     value  = gv_lgtyp_1 ).

  o_tvarv->get_parameter(
   EXPORTING
     suffix = 'PICKING_SOT_LGTYP_2'
   IMPORTING
     value  = gv_lgtyp_2 ).

  o_tvarv->get_seloption(
   EXPORTING
     suffix = 'PICKING_SOT_LGTYP' "Sufixo qualquer de Select Option
   IMPORTING
     value = gr_lgtyp ).

ENDFORM.
