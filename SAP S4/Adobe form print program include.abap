*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Class (Implementation) lcl_bol
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
CLASS lcl_bol IMPLEMENTATION.

  METHOD get_main_data.
    DATA: ltr_vbeln                 TYPE RANGE OF vbeln,
          lta_et_delivery_header    TYPE TABLE OF bapidlvhdr WITH EMPTY KEY,
          lta_et_delivery_partner   TYPE TABLE OF bapidlvpartners WITH EMPTY KEY,
          lta_et_delivery_item      TYPE TABLE OF bapidlvitem WITH EMPTY KEY,
          lta_et_hu_header          TYPE TABLE OF bapidlvhandlingunithdr WITH EMPTY KEY,
          lta_shipment_items        TYPE TABLE OF vttp WITH EMPTY KEY,
          lta_shipment_segments     TYPE TABLE OF vtts WITH EMPTY KEY,
          lta_shipment_segment_item TYPE TABLE OF vtsp WITH EMPTY KEY,
          lta_delivery_headers      TYPE TABLE OF vtrlk WITH EMPTY KEY,
          l_ctrl                    TYPE bapidlvbuffercontrol,
          lt_header                 TYPE bapidlvhdr_t.

    l_ctrl = VALUE #( head_status = abap_true head_partner = abap_true item = abap_true hu_data = abap_true ).
    CASE nast-kschl.
      WHEN co_del_output_type.
        " if delivery level, objky has delivery number, just use it
        APPEND VALUE #( sign   = 'I' option = 'EQ' low = nast-objky ) TO ltr_vbeln.
      WHEN co_ship_output_type.
        " if shipment level, objky has shipment number, need to retrieve all deliveries
        CALL FUNCTION 'RV_SHIPMENT_READ'
          EXPORTING
            shipment_number              = CONV vttk-tknum( nast-objky )
            option_items                 = abap_true
            option_delivery_headers_only = abap_true
            i_language                   = space
          TABLES
            shipment_segments            = lta_shipment_segments
            shipment_segment_item        = lta_shipment_segment_item
            delivery_headers             = lta_delivery_headers
            shipment_items               = lta_shipment_items.

        SELECT 'I' AS sign, 'EQ' AS option, vbeln AS low
          FROM @lta_shipment_items AS si
          INTO TABLE @ltr_vbeln.
    ENDCASE.

    " get deliveries information
    CALL FUNCTION 'BAPI_DELIVERY_GETLIST'
      EXPORTING
        is_dlv_data_control = l_ctrl
      TABLES
        et_delivery_header  = lta_et_delivery_header
        et_delivery_partner = lta_et_delivery_partner
        et_delivery_item    = lta_et_delivery_item
        et_hu_header        = lta_et_hu_header
        it_vbeln            = ltr_vbeln.

    IF sy-subrc = 0.
      " get ship to, forward to addresses and carrier name
      SELECT DISTINCT p~parvw, adrc~*
          FROM @lta_et_delivery_partner AS p
            JOIN adrc ON p~adrnr = adrc~addrnumber
          WHERE @sy-datum BETWEEN adrc~date_from AND adrc~date_to
            AND p~parvw IN (@co_ship_to, @co_forward_to, @co_carrier)
          INTO @DATA(ls_addr).
        CASE ls_addr-parvw.
          WHEN co_ship_to.
            gs_ship_to = ls_addr-adrc.
            CHECK gt_ship_to IS INITIAL.
            go_bol->compose_address( CHANGING gs_address = gs_ship_to
                                              gt_tline   = gt_ship_to ).
          WHEN co_forward_to.
            gs_forward_to = ls_addr-adrc.
            CHECK gt_forward IS INITIAL.
            go_bol->compose_address( CHANGING gs_address = gs_forward_to
                                              gt_tline   = gt_forward ).
          WHEN co_carrier.
            CHECK g_carrier IS INITIAL.
            g_carrier = ls_addr-adrc-name1.
        ENDCASE.
      ENDSELECT.

      " get ship from address
      SELECT SINGLE adrc~*
        FROM @lta_et_delivery_header AS h
          JOIN tvst ON h~vstel = tvst~vstel
          JOIN adrc ON tvst~adrnr = adrc~addrnumber
        WHERE @sy-datum BETWEEN adrc~date_from AND adrc~date_to
        INTO @gs_ship_from.
      IF sy-subrc = 0.
        CHECK gt_ship_from IS INITIAL.
        go_bol->compose_address( CHANGING gs_address = gs_ship_from
                                          gt_tline   = gt_ship_from ).
      ENDIF.

      " get bill of lading number, incoterms, tracking and shipment numbers
      SELECT SINGLE h~vbeln, h~inco1, h~inco2_l, h~bolnr, vbfa~vbeln
        FROM @lta_et_delivery_header AS h
          LEFT JOIN vbfa ON h~vbeln = vbfa~vbelv AND vbfa~vbtyp_n = '8'
        INTO (@g_bol_number, @g_incoterms, @g_incoterms2, @g_tracking_no, @g_shipment_no).

      " if shipment level, pallets might not be included in deliveries,
      " if cartons are in pallets, need to add pallets to the list
      IF nast-kschl = co_ship_output_type.
        SELECT DISTINCT vekp~*
          FROM @lta_et_hu_header AS huh
            JOIN vepo ON huh~venum = vepo~unvel
            JOIN vekp ON vepo~venum = vekp~venum
          APPENDING CORRESPONDING FIELDS OF TABLE @lta_et_hu_header.
      ENDIF.

      " build handling units list
      "   include pallet or carton number depending on type
      "   round weight units
      "   round volume units
      "   count number of cartons in pallets
      "   build dimensions
      "   include commodity description
      SELECT
          CASE WHEN huh~vhart EQ @co_pallet THEN huh~exidv END AS exidv_palt,
          CASE WHEN huh~vhart EQ @co_carton THEN huh~exidv END AS exidv_cart,
          round( huh~brgew, 0 ) AS brgew,
          round( huh~btvol, 1 ) AS btvol,
          COUNT( huh~exidv ) AS cartons,
          concat( concat( CAST( CAST( round( huh~laeng, 0 ) AS INT2 ) AS CHAR( 16 ) ), '*' ),
            concat( concat( CAST( CAST( round( huh~breit, 0 ) AS INT2 ) AS CHAR( 16 ) ), '*' ),
              CAST( CAST( round( huh~hoehe, 0 ) AS INT2 ) AS CHAR( 16 ) ) ) ) AS dimensions,
          @co_audio_equipment AS stawn
        FROM @lta_et_hu_header AS huh
          LEFT JOIN vepo ON huh~venum = vepo~venum
        WHERE huh~vhart EQ @co_pallet
          OR NOT EXISTS ( SELECT unvel FROM vepo AS v WHERE huh~venum = v~unvel )
            AND vepo~posnr = ( SELECT MIN( posnr ) FROM vepo AS v WHERE huh~venum = v~venum )
        GROUP BY huh~vhart, huh~exidv, huh~brgew, huh~btvol,
          huh~laeng, huh~breit, huh~hoehe
        INTO CORRESPONDING FIELDS OF TABLE @gta_handling_units.

      "   add hazardous material indicator
      LOOP AT gta_handling_units ASSIGNING FIELD-SYMBOL(<fs>).
        DATA stoff TYPE int1.
        IF <fs>-exidv_cart IS NOT INITIAL.
          SELECT
              MAX( CASE
                  WHEN mara~profl = @co_profile_ba2 OR mara~profl = @co_profile_ba3 THEN 2
                  WHEN mara~profl = @co_profile_ba1 THEN 1
                  ELSE 0 END )
            FROM vekp
              JOIN vepo ON vekp~venum = vepo~venum
              JOIN mara ON vepo~matnr = mara~matnr
            WHERE vekp~exidv = @<fs>-exidv_cart INTO @stoff.
        ELSE.
          SELECT
              MAX( CASE
                  WHEN mara~profl = @co_profile_ba2 OR mara~profl = @co_profile_ba3 THEN 2
                  WHEN mara~profl = @co_profile_ba1 THEN 1
                  ELSE 0 END )
            FROM vekp
              JOIN vepo AS p ON vekp~venum = p~venum
              JOIN vepo ON vepo~venum = p~unvel
              JOIN mara ON vepo~matnr = mara~matnr
            WHERE vekp~exidv = @<fs>-exidv_palt INTO @stoff.
        ENDIF.
        CASE stoff.
          WHEN 2.
            <fs>-stoff = co_hm_un3481.
          WHEN 1.
            <fs>-stoff = co_hm_un3480.
          WHEN OTHERS.
            <fs>-stoff = co_hm_no.
        ENDCASE.
        CLEAR stoff.
      ENDLOOP.

      " calculate totals
      LOOP AT gta_handling_units INTO DATA(l_hu).
        IF l_hu-exidv_palt IS NOT INITIAL.
          g_pallet_cnt += 1.
        ENDIF.
        IF l_hu-exidv_cart IS NOT INITIAL.
          g_carton_cnt += 1.
        ENDIF.
        g_hu_cnt += 1.
        g_carton_ttl += l_hu-cartons.
        g_weight_ttl += l_hu-brgew.
        g_volume_ttl += l_hu-btvol.
      ENDLOOP.

      "Get Special Instructions
      CALL FUNCTION 'READ_TEXT'
        EXPORTING
          id                      = co_text_id
          language                = sy-langu
          name                    = CONV thead-tdname( lta_et_delivery_header[ 1 ]-vbeln ) "( nast-objky )
          object                  = co_esll
        TABLES
          lines                   = lt_text_lines
        EXCEPTIONS
          id                      = 1
          language                = 2
          name                    = 3
          not_found               = 4
          object                  = 5
          reference_check         = 6
          wrong_access_to_archive = 7
          OTHERS                  = 8.

      IF sy-subrc = 0.
        LOOP AT lt_text_lines INTO DATA(ls_text_lines).
          CONCATENATE g_special_instructions ls_text_lines-tdline
            INTO g_special_instructions SEPARATED BY space.
        ENDLOOP.
      ENDIF.

    ENDIF.
  ENDMETHOD.

  METHOD compose_address.
    DATA: ls_line TYPE tline.
*
    IF gs_address-name1 IS NOT INITIAL.
      ls_line-tdformat = '*'.
      ls_line-tdline =  gs_address-name1.
      CONDENSE ls_line-tdline.
      APPEND ls_line TO gt_tline.
    ENDIF.
    IF gs_address-name2 IS NOT INITIAL.
      ls_line-tdformat = '*'.
      ls_line-tdline =   gs_address-name2.
      CONDENSE ls_line-tdline.
      APPEND ls_line TO gt_tline.
    ENDIF.

    IF gs_address-house_num1 IS NOT INITIAL OR gs_address-street IS NOT INITIAL.
      ls_line-tdformat = '*'.

      CONCATENATE gs_address-house_num1  space  gs_address-street INTO ls_line-tdline SEPARATED BY space.
      CONDENSE ls_line-tdline.
      APPEND ls_line TO gt_tline.

    ENDIF.
    IF gs_address-str_suppl1 IS NOT INITIAL.
      ls_line-tdformat = '*'.
      ls_line-tdline = gs_address-str_suppl1.
      CONDENSE ls_line-tdline.
      APPEND ls_line TO gt_tline.

    ENDIF.

    IF gs_address-city1 IS NOT INITIAL OR gs_address-region IS NOT INITIAL OR gs_address-post_code1 IS NOT INITIAL OR gs_address-country IS NOT INITIAL.
      ls_line-tdformat = '*'.
      CONCATENATE gs_address-city1 gs_address-region gs_address-post_code1  gs_address-country INTO ls_line-tdline SEPARATED BY space.
      CONDENSE ls_line-tdline.
      APPEND ls_line TO gt_tline.
    ENDIF.
  ENDMETHOD.



  METHOD print_form.

    DATA: g_fm_name      TYPE funcname,
          g_outputparams TYPE sfpoutputparams.

    "Determine PDF function module for CMR Belgium
    CALL FUNCTION 'FP_FUNCTION_MODULE_NAME'
      EXPORTING
        i_name     = tnapr-sform
      IMPORTING
        e_funcname = g_fm_name.

    IF sy-subrc = 0.
      CALL FUNCTION 'FP_JOB_OPEN'
        CHANGING
          ie_outputparams = g_outputparams
        EXCEPTIONS
          cancel          = 1
          usage_error     = 2
          system_error    = 3
          internal_error  = 4
          OTHERS          = 5.

      IF sy-subrc = 0.
        " Calling Function module
        CALL FUNCTION g_fm_name
          EXPORTING
            gt_ship_to             = gt_ship_to
            gt_ship_from           = gt_ship_from
            gt_forward             = gt_forward
            g_bol_number           = g_bol_number
            g_incoterms            = g_incoterms
            g_incoterms2           = g_incoterms2
            g_tracking_no          = g_tracking_no
            g_carrier              = g_carrier
            g_shipment_no          = g_shipment_no
            g_special_instructions = g_special_instructions
            gta_handling_units     = gta_handling_units
            g_pallet_cnt           = g_pallet_cnt
            g_carton_cnt           = g_carton_cnt
            g_hu_cnt               = g_hu_cnt
            g_carton_ttl           = g_carton_ttl
            g_weight_ttl           = g_weight_ttl
            g_volume_ttl           = g_volume_ttl
          EXCEPTIONS
            usage_error            = 1
            system_error           = 2
            internal_error         = 3
            OTHERS                 = 4.

        "Close spool job
        CALL FUNCTION 'FP_JOB_CLOSE'
          EXCEPTIONS
            usage_error    = 1
            system_error   = 2
            internal_error = 3
            OTHERS         = 4.
      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
