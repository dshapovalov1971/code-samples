REPORT zlopack_carton_sf.

INCLUDE rvadtabl.
INCLUDE palidata.

CONSTANTS: c_multiple(8)  TYPE c VALUE 'MULTIPLE',
           c_mixed_sku(9) TYPE c VALUE 'Mixed SKU'.

DATA: fm_name        TYPE rs38l_fnam,
      retcode        LIKE sy-subrc,
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
      END OF lvbplp1,
      xscreen(1) TYPE c.

TABLES vbco3.

FORM entry USING return_code us_screen.
  xscreen = us_screen.
  PERFORM get_data.
  PERFORM print_data.
  IF retcode NE 0.
    return_code = 1.
  ELSE.
    return_code = 0.
  ENDIF.
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
        WHERE matnr = @lvbplp1-matnr AND spras = @nast-spras
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

FORM print_data.
  DATA: l_lw           TYPE zlw_default,
        l_error        TYPE abap_bool,
        ls_itcpo       TYPE itcpo,
        control        TYPE ssfctrlop,
        output_options TYPE ssfcompop,
        lf_repid       TYPE sy-repid,
        lf_device      TYPE tddevice,
        lf_retcode     TYPE sy-subrc.


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

  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname           = tnapr-sform
*     variant            = ' '
*     direct_call        = ' '
    IMPORTING
      fm_name            = fm_name
    EXCEPTIONS
      no_form            = 1
      no_function_module = 2
      OTHERS             = 3.

  IF sy-subrc <> 0.
*   error handling
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    EXIT.
  ENDIF.

  lf_repid = sy-repid.

  CALL FUNCTION 'WFMC_PREPARE_SMART_FORM'
    EXPORTING
      pi_nast       = nast
      pi_repid      = lf_repid
      pi_screen     = xscreen
    IMPORTING
      pe_returncode = lf_retcode
      pe_itcpo      = ls_itcpo
      pe_device     = lf_device.

  IF lf_RETCODE = 0.
    MOVE-CORRESPONDING ls_itcpo TO output_options.
    control-device = lf_device.
    control-no_dialog = abap_true.
    control-preview = xscreen.
    control-getotf = ls_itcpo-tdgetotf.
    control-langu = nast-spras.
  ENDIF.

* now call the generated function module
  CALL FUNCTION fm_name
    EXPORTING
      control_parameters = control
      output_options     = output_options
      user_settings      = space
      is_nast            = nast
      lw_printername     = lw_printername
      lw_label           = lw_label
      v_count            = v_count
      lw_jobname         = lw_jobname
      g_ponum            = g_ponum
      g_ponum2           = g_ponum2
      lw_quantity        = lw_quantity
      lvbplp1            = lvbplp1
      lvbplk             = lvbplk
      lvbpla             = lvbpla
    EXCEPTIONS
      formatting_error   = 1
      internal_error     = 2
      send_error         = 3
      user_canceled      = 4
      OTHERS             = 5.

  IF sy-subrc <> 0.
*   error handling
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.
