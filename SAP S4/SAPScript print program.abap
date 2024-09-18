REPORT zlopack_carton LINE-COUNT 100 MESSAGE-ID vv.

INCLUDE rvadtabl.
INCLUDE palidata.

CONSTANTS: c_multiple(8)  TYPE c VALUE 'MULTIPLE',
           c_mixed_sku(9) TYPE c VALUE 'Mixed SKU',
           c_lang         TYPE makt-spras VALUE 'E'.

DATA: retcode        LIKE sy-subrc,
      lw_printername TYPE zlw_default-lw_printername,
      lw_label       TYPE zlw_default-lw_label,
      lw_jobname     TYPE zlw_default-lw_jobname,
      lw_quantity    TYPE zlw_default-lw_quantity,
      v_count        TYPE int1 VALUE 1,
      g_ponum        TYPE vbkd-bstkd,
      g_ponum2       TYPE vbkd-bstkd,
      BEGIN OF lvbplp1,
        matnr  TYPE lips-matnr,
        arktx  TYPE makt-maktx,
        vemng  TYPE vepo-vemng,
        matnr2 TYPE lips-matnr,
      END OF lvbplp1.

TABLES vbco3.

FORM lw_entry USING return_code us_screen.
  CLEAR: retcode,
         lw_printername,
         lw_label,
         lw_jobname,
         lw_quantity.

  PERFORM lw_processing.
  IF retcode NE 0.
    return_code = 1.
  ELSE.
    return_code = 0.
  ENDIF.
ENDFORM.

FORM lw_processing.

  PERFORM get_data.
  CHECK retcode = 0.

  PERFORM lw_call_script CHANGING retcode.
  IF retcode > 0.
    EXIT.
  ENDIF.
  MESSAGE ID zlwx_prn_msgcl TYPE 'S' NUMBER '101'.

ENDFORM.

FORM get_data.
  REFRESH: lvbplk, lvbpla.
  CLEAR: lvbplk, lvbpla.

  vbco3-venum = nast-objky.      "00000.....
  vbco3-spras = nast-spras.      "D
  vbco3-kunde = nast-parnr.      "KUNDE
  vbco3-parvw = nast-parvw.      "WE
  vbco3-packd = 'X'.

  CALL FUNCTION 'SD_PACKING_PRINT_VIEW_SINGLE'
    EXPORTING
      comwa                    = vbco3
    IMPORTING
      vbplk_wa                 = lvbplk
      vbpla_wa                 = lvbpla
    TABLES
      vbplp_tab                = lvbplp
    EXCEPTIONS
      shipping_unit_not_unique = 1
      shipping_unit_not_found  = 2
      OTHERS                   = 3.

  IF sy-subrc NE 0.
    retcode = 1.
    PERFORM protocol_update.
  ELSE.
    SELECT SINGLE adrc~*
      FROM adrc JOIN t001w ON t001w~adrnr = adrc~addrnumber
      WHERE t001w~werks = @lvbplk-werks
      INTO @DATA(l_adrc_from).
    lvbpla-name1_vst = l_adrc_from-name1.
    lvbpla-name2_vst = l_adrc_from-name2.
    lvbpla-stras_vst = COND #( WHEN l_adrc_from-house_num1 IS INITIAL THEN '' ELSE l_adrc_from-house_num1 && ` ` ) && l_adrc_from-street.
    lvbpla-ort01_vst = l_adrc_from-city1.
    lvbpla-regio_vst = l_adrc_from-region.
    lvbpla-pstlz_vst = l_adrc_from-post_code1.
    lvbpla-land1_vst = l_adrc_from-country.

    SELECT SINGLE kna1~*
      FROM kna1 JOIN vbpa ON vbpa~kunnr = kna1~kunnr
      WHERE vbpa~vbeln = @lvbplk-vpobjkey AND vbpa~parvw = 'WE'
      INTO @DATA(l_adrc_to).
    lvbpla-name1_we = l_adrc_to-name1.
    lvbpla-name2_we = l_adrc_to-name2.
    lvbpla-stras_we = l_adrc_to-stras.
    lvbpla-ort01_we = l_adrc_to-ort01.
    lvbpla-regio_we = l_adrc_to-regio.
    lvbpla-pstlz_we = l_adrc_to-pstlz.
    lvbpla-land1_we = l_adrc_to-land1.

    SELECT DISTINCT vbkd~bstkd
      FROM vbkd
        JOIN vbfa ON vbkd~vbeln = vbfa~vbelv
        JOIN vepo ON vbfa~vbeln = vepo~vbeln AND vbfa~vbtyp_v = 'C' AND vbfa~posnn = vepo~vepos
      WHERE vepo~venum = @lvbplk-venum
      INTO TABLE @DATA(lt_pos).
    READ TABLE lt_pos INTO DATA(l_po) INDEX 1.
    g_ponum = COND #( WHEN lines( lt_pos ) <= 1 THEN l_po-bstkd ELSE c_multiple ).
    g_ponum2 = g_ponum.

    DATA: BEGIN OF lt_matnr OCCURS 0,
            matnr TYPE lips-matnr,
            vemng TYPE vepo-vemng,
          END OF lt_matnr.
    LOOP AT lvbplp ASSIGNING FIELD-SYMBOL(<wa>)
         GROUP BY ( key = <wa>-matnr )
         ASCENDING
         ASSIGNING FIELD-SYMBOL(<group_key>).
      lt_matnr-matnr = <group_key>-key.
      lt_matnr-vemng = REDUCE vepo-vemng( INIT sum = 0
                                FOR m IN GROUP <group_key>
                                NEXT sum = sum + m-vemng ).
      APPEND lt_matnr TO lt_matnr.
    ENDLOOP.
    IF lines( lt_matnr ) <= 1.
      lvbplp1-matnr = lt_matnr-matnr.
      lvbplp1-vemng = lt_matnr-vemng.
      SELECT SINGLE maktx
        FROM makt
        WHERE matnr = @lvbplp1-matnr AND spras = @c_lang
        INTO @lvbplp1-arktx.
    ELSE.
      lvbplp1-matnr = c_mixed_sku.
    ENDIF.
    lvbplp1-matnr2 = lvbplp1-matnr.
  ENDIF.
ENDFORM.

FORM protocol_update.
  CHECK screen = ' '.
  CALL FUNCTION 'NAST_PROTOCOL_UPDATE'
    EXPORTING
      msg_arbgb = sy-msgid
      msg_nr    = sy-msgno
      msg_ty    = sy-msgty
      msg_v1    = sy-msgv1
      msg_v2    = sy-msgv2
      msg_v3    = sy-msgv3
      msg_v4    = sy-msgv4
    EXCEPTIONS
      OTHERS    = 1.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  LW_CALL_SCRIPT
*&---------------------------------------------------------------------*
*       exec Loftware RDI(SapScript) for header and each detail
*----------------------------------------------------------------------*
FORM lw_call_script CHANGING retcode.

  DATA: l_lw     TYPE zlw_default,
        l_error  TYPE abap_bool,
        ls_itcpo TYPE itcpo.

**********************************************************************
***Get Printer and LW Override values defaults
**********************************************************************
  zcl_lw_maps=>get_lw_settings_nace(
    EXPORTING
      nast         = nast
      rdi_driver   = sy-repid
    IMPORTING
      itcpo        = ls_itcpo
      lw_override  = l_lw
      is_error     = l_error ).
  IF l_error EQ abap_true.
    retcode = 8.
    syst-msgty = 'E'.
    PERFORM protocol_update.
    RETURN.
  ELSE.
* Set the printer based on terminal PC settings and output setting
* Template to picked up in Loftware
* Jobname in Loftware and spool title in SAP
* No of prints
    lw_printername = l_lw-lw_printername.
    lw_label       = l_lw-lw_label.
    lw_jobname     = l_lw-lw_jobname.
    lw_quantity    = l_lw-lw_quantity.
  ENDIF.

**********************************************************************
***Call SAP Script for printing
**********************************************************************

  CALL FUNCTION 'OPEN_FORM'
    EXPORTING
      device                      = 'PRINTER'
      dialog                      = ' '
      form                        = tnapr-fonam
      language                    = nast-spras
      options                     = ls_itcpo
    EXCEPTIONS
      canceled                    = 1
      device                      = 2
      form                        = 3
      options                     = 4
      unclosed                    = 5
      mail_options                = 6
      archive_error               = 7
      invalid_fax_number          = 8
      more_params_needed_in_batch = 9
      spool_error                 = 10
      codepage                    = 11
      OTHERS                      = 12.
  IF sy-subrc <> 0.
    retcode = sy-subrc.
    PERFORM protocol_update.
    RETURN.
  ELSE.
    CALL FUNCTION 'WRITE_FORM'
      EXPORTING
        element = 'HEADER'
        window  = 'LABEL'
      EXCEPTIONS
        element = 1
        window  = 2.
    IF sy-subrc <> 0.
      retcode = sy-subrc.
      PERFORM protocol_update.
      RETURN.
    ENDIF.

    CALL FUNCTION 'CONTROL_FORM'
      EXPORTING
        command   = 'NEW-WINDOW'
      EXCEPTIONS
        unopened  = 1
        unstarted = 2
        OTHERS    = 3.

    CALL FUNCTION 'WRITE_FORM'
      EXPORTING
        element = 'HEADER_DATA'
        window  = 'LABEL'
      EXCEPTIONS
        element = 1
        window  = 2.
    IF sy-subrc <> 0.
      retcode = sy-subrc.
      PERFORM protocol_update.
      RETURN.
    ENDIF.
  ENDIF.

  CALL FUNCTION 'CLOSE_FORM'
    EXCEPTIONS
      OTHERS = 1.

ENDFORM.                    " call_lwscript
