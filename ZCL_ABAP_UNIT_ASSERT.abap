class ZCL_ABAP_UNIT_ASSERT definition
  public
  final
  create public .

public section.

  types:
    BEGIN OF gts_assert_bapiret2,
        type       TYPE bapiret2-type,
        id         TYPE bapiret2-id,
        number     TYPE bapiret2-number,
        message_v1 TYPE bapiret2-message_v1,
        message_v2 TYPE bapiret2-message_v2,
        message_v3 TYPE bapiret2-message_v3,
        message_v4 TYPE bapiret2-message_v4,
      END OF gts_assert_bapiret2 .
  types:
    gtt_assert_bapiret2 TYPE STANDARD TABLE OF gts_assert_bapiret2
          WITH DEFAULT KEY .

  class-methods ASSERT_EQUALS
    importing
      !FIELD_NAME type CHAR30 optional
      value(ACT) type ANY
      value(EXP) type ANY
      !IGNORE_HASH_SEQUENCE type ABAP_BOOL default ABAP_FALSE
      !TOL type F optional
      !MSG type CSEQUENCE optional
      !LEVEL type INT1 default IF_AUNIT_CONSTANTS=>SEVERITY-MEDIUM
      !QUIT type INT1 default IF_AUNIT_CONSTANTS=>METHOD
      !SKIP_DEFAULT_ASSERT type ABAP_BOOL default ABAP_FALSE
      !SORT_STANDARD_TABLE_IND type ABAP_BOOL default ABAP_FALSE
    returning
      value(ASSERTION_FAILED) type ABAP_BOOL .
  class-methods ASSERT_BAPIRET2_T
    importing
      !IV_CHECK_TYPE_I_AND_S_IND type ABAP_BOOL default ABAP_FALSE
      !IT_ACT_BAPIRET2 type BAPIRET2_T
      !IT_EXP_BAPIRET2 type GTT_ASSERT_BAPIRET2
      !IV_QUIT type INT1 default IF_AUNIT_CONSTANTS=>METHOD .
  class-methods FAIL_BAPIRET2_T
    importing
      !IV_TEXT type STRING optional
      !IV_LEVEL type INT1 default IF_AUNIT_CONSTANTS=>SEVERITY-MEDIUM
      !IV_QUIT type INT1 default IF_AUNIT_CONSTANTS=>QUIT-TEST
      !IT_BAPIRET2_T type BAPIRET2_T .
protected section.

  class-methods ASSERT_EQUALS_BY_REF
    importing
      !FIELD_NAME type CHAR30 optional
      !IGNORE_HASH_SEQUENCE type ABAP_BOOL
      !TOL type F
      !MSG type CSEQUENCE
      !LEVEL type INT1
      !SKIP_DEFAULT_ASSERT type ABAP_BOOL
      !SORT_STANDARD_TABLE_IND type ABAP_BOOL
    changing
      !ACT type ANY
      !EXP type ANY
    returning
      value(ASSERTION_FAILED) type ABAP_BOOL .
  class-methods ASSERT_STANDARD_TABLE
    importing
      !IV_CLEAR_TABLE_IND type ABAP_BOOL
      !IV_TABLE_NAME type TYPENAME
      !IV_SORT_STANDARD_TABLE_IND type ABAP_BOOL
    changing
      !CT_ACT_TABLE type STANDARD TABLE
      !CT_EXP_TABLE type STANDARD TABLE
    returning
      value(RV_ASSERT_FAILED) type ABAP_BOOL .
  class-methods GET_ALL_ELEMENT_COMPONENTS
    importing
      !IR_STRUCT_DESCR type ref to CL_ABAP_STRUCTDESCR
    returning
      value(RT_COMPONENTS) type CL_ABAP_STRUCTDESCR=>COMPONENT_TABLE .
private section.

  class-methods CONVERT_TO_STRING
    importing
      !VALUE type ANY
    returning
      value(VALUE_TEXT) type STRING .
  class-methods GET_RECORD_TEXT
    importing
      !RECORD type ANY
      !COMPONENTS type CL_ABAP_STRUCTDESCR=>COMPONENT_TABLE
    returning
      value(TEXT) type STRING .
ENDCLASS.



CLASS ZCL_ABAP_UNIT_ASSERT IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_ABAP_UNIT_ASSERT=>ASSERT_BAPIRET2_T
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_CHECK_TYPE_I_AND_S_IND      TYPE        ABAP_BOOL (default =ABAP_FALSE)
* | [--->] IT_ACT_BAPIRET2                TYPE        BAPIRET2_T
* | [--->] IT_EXP_BAPIRET2                TYPE        GTT_ASSERT_BAPIRET2
* | [--->] IV_QUIT                        TYPE        INT1 (default =IF_AUNIT_CONSTANTS=>METHOD)
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD assert_bapiret2_t.

    DATA(lt_act_bapiret2) = it_act_bapiret2.

    IF iv_check_type_i_and_s_ind = abap_false.

      DELETE lt_act_bapiret2
        WHERE type CA 'IS'.

    ENDIF.

    "************************************************
    "Check unknow messages
    LOOP AT it_exp_bapiret2
      ASSIGNING FIELD-SYMBOL(<ls_exp_bapiret2>).

      DATA(lv_found_ind) = abap_false.

      LOOP AT lt_act_bapiret2
        ASSIGNING FIELD-SYMBOL(<ls_act_bapiret2_2>)
        WHERE
          type       = <ls_exp_bapiret2>-type AND
          id         = <ls_exp_bapiret2>-id AND
          number     = <ls_exp_bapiret2>-number.

        "'*' is a wild card, so the value won't be checked.
        IF <ls_exp_bapiret2>-message_v1 = '*'.
          <ls_act_bapiret2_2>-message_v1 = '*'.
        ENDIF.

        IF <ls_exp_bapiret2>-message_v2 = '*'.
          <ls_act_bapiret2_2>-message_v2 = '*'.
        ENDIF.

        IF <ls_exp_bapiret2>-message_v3 = '*'.
          <ls_act_bapiret2_2>-message_v3 = '*'.
        ENDIF.

        IF <ls_exp_bapiret2>-message_v4 = '*'.
          <ls_act_bapiret2_2>-message_v4 = '*'.
        ENDIF.

        IF <ls_act_bapiret2_2>-message_v1 = <ls_exp_bapiret2>-message_v1 AND
           <ls_act_bapiret2_2>-message_v2 = <ls_exp_bapiret2>-message_v2 AND
           <ls_act_bapiret2_2>-message_v3 = <ls_exp_bapiret2>-message_v3 AND
           <ls_act_bapiret2_2>-message_v4 = <ls_exp_bapiret2>-message_v4.

          DELETE lt_act_bapiret2.

          lv_found_ind = abap_true.

          EXIT.

        ENDIF.

      ENDLOOP.

      IF lv_found_ind = abap_false.

        MESSAGE
           ID <ls_exp_bapiret2>-id TYPE <ls_exp_bapiret2>-type NUMBER <ls_exp_bapiret2>-number
           WITH
             <ls_exp_bapiret2>-message_v1
             <ls_exp_bapiret2>-message_v2
             <ls_exp_bapiret2>-message_v3
             <ls_exp_bapiret2>-message_v4
           INTO DATA(lv_message).

        cl_abap_unit_assert=>fail(
            msg    = 'ASSERT_BAPIRET2_T: Miss actual.' &&
              |Tp: { <ls_exp_bapiret2>-type } ,| &&
              |id: { <ls_exp_bapiret2>-id } ,| &&
              |no: { <ls_exp_bapiret2>-number } ,| &&
              |v1: { <ls_exp_bapiret2>-message_v1 } ,| &&
              |v2: { <ls_exp_bapiret2>-message_v2 } ,| &&
              |v3: { <ls_exp_bapiret2>-message_v3 } ,| &&
              |v4: { <ls_exp_bapiret2>-message_v4 }|
            level  = if_aunit_constants=>severity-medium
            quit   = if_aunit_constants=>no
            detail = lv_message ).

        DATA(lv_failed) = abap_true.

      ENDIF.

    ENDLOOP.

    "************************************************
    "Check unknow messages

    LOOP AT lt_act_bapiret2
      ASSIGNING FIELD-SYMBOL(<ls_act_bapiret2>).

      MESSAGE
         ID <ls_act_bapiret2>-id TYPE <ls_act_bapiret2>-type NUMBER <ls_act_bapiret2>-number
         WITH
           <ls_act_bapiret2>-message_v1
           <ls_act_bapiret2>-message_v2
           <ls_act_bapiret2>-message_v3
           <ls_act_bapiret2>-message_v4
         INTO DATA(lv_act_message).

      cl_abap_unit_assert=>fail(
          msg    = 'ASSERT_BAPIRET2_T: Not expected.' &&
            |Tp: { <ls_act_bapiret2>-type } ,| &&
            |id: { <ls_act_bapiret2>-id } ,| &&
            |no: { <ls_act_bapiret2>-number } ,| &&
            |v1: { <ls_act_bapiret2>-message_v1 } ,| &&
            |v2: { <ls_act_bapiret2>-message_v2 } ,| &&
            |v3: { <ls_act_bapiret2>-message_v3 } ,| &&
            |v4: { <ls_act_bapiret2>-message_v4 }|
          level  = if_aunit_constants=>severity-medium
          quit   = if_aunit_constants=>no
          detail = lv_act_message ).

      lv_failed = abap_true.

    ENDLOOP.

    "************************************************
    "Handling quit

    IF lv_failed = abap_true.

      CASE iv_quit.
*      when if_Aunit_Constants=>program.
*        raise exception type cx_Aunit_Sbx_Quit_Test_Prog.
        WHEN if_aunit_constants=>class.
          RAISE EXCEPTION TYPE cx_aunit_sbx_quit_test_class.
        WHEN if_aunit_constants=>method.
          RAISE EXCEPTION TYPE cx_aunit_sbx_quit_test_method.
        WHEN if_aunit_constants=>quit-no.
          "
        WHEN OTHERS.
          RAISE EXCEPTION TYPE cx_aunit_sbx_quit_test_method.
      ENDCASE.

    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_ABAP_UNIT_ASSERT=>ASSERT_EQUALS
* +-------------------------------------------------------------------------------------------------+
* | [--->] FIELD_NAME                     TYPE        CHAR30(optional)
* | [--->] ACT                            TYPE        ANY
* | [--->] EXP                            TYPE        ANY
* | [--->] IGNORE_HASH_SEQUENCE           TYPE        ABAP_BOOL (default =ABAP_FALSE)
* | [--->] TOL                            TYPE        F(optional)
* | [--->] MSG                            TYPE        CSEQUENCE(optional)
* | [--->] LEVEL                          TYPE        INT1 (default =IF_AUNIT_CONSTANTS=>SEVERITY-MEDIUM)
* | [--->] QUIT                           TYPE        INT1 (default =IF_AUNIT_CONSTANTS=>METHOD)
* | [--->] SKIP_DEFAULT_ASSERT            TYPE        ABAP_BOOL (default =ABAP_FALSE)
* | [--->] SORT_STANDARD_TABLE_IND        TYPE        ABAP_BOOL (default =ABAP_FALSE)
* | [<-()] ASSERTION_FAILED               TYPE        ABAP_BOOL
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD assert_equals.

    "Pass act and exp by changing
    assertion_failed =
      assert_equals_by_ref(
        EXPORTING field_name              = field_name
                  ignore_hash_sequence    = ignore_hash_sequence
                  tol                     = tol
                  msg                     = msg
                  level                   = level
                  skip_default_assert     = skip_default_assert
                  sort_standard_table_ind = sort_standard_table_ind
        CHANGING act                      = act
                 exp                      = exp ).


    IF assertion_failed = abap_true.

      "Handling quit
      CASE quit.
*      when if_Aunit_Constants=>program.
*        raise exception type cx_Aunit_Sbx_Quit_Test_Prog.
        WHEN if_aunit_constants=>class.
          RAISE EXCEPTION TYPE cx_aunit_sbx_quit_test_class.
        WHEN if_aunit_constants=>method.
          RAISE EXCEPTION TYPE cx_aunit_sbx_quit_test_method.
        WHEN if_aunit_constants=>quit-no.
          "
        WHEN OTHERS.
          RAISE EXCEPTION TYPE cx_aunit_sbx_quit_test_method.
      ENDCASE.

    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Protected Method ZCL_ABAP_UNIT_ASSERT=>ASSERT_EQUALS_BY_REF
* +-------------------------------------------------------------------------------------------------+
* | [--->] FIELD_NAME                     TYPE        CHAR30(optional)
* | [--->] IGNORE_HASH_SEQUENCE           TYPE        ABAP_BOOL
* | [--->] TOL                            TYPE        F
* | [--->] MSG                            TYPE        CSEQUENCE
* | [--->] LEVEL                          TYPE        INT1
* | [--->] SKIP_DEFAULT_ASSERT            TYPE        ABAP_BOOL
* | [--->] SORT_STANDARD_TABLE_IND        TYPE        ABAP_BOOL
* | [<-->] ACT                            TYPE        ANY
* | [<-->] EXP                            TYPE        ANY
* | [<-()] ASSERTION_FAILED               TYPE        ABAP_BOOL
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD assert_equals_by_ref.

    DATA(lr_act_type_descr) = cl_abap_typedescr=>describe_by_data( act ).

    CASE lr_act_type_descr->kind.

      WHEN cl_abap_typedescr=>kind_table.

        DATA(lr_act_table_descr) = CAST cl_abap_tabledescr( lr_act_type_descr ).

        CASE lr_act_table_descr->table_kind.

          WHEN cl_abap_tabledescr=>tablekind_std.

            FIELD-SYMBOLS <act_standard_table> TYPE STANDARD TABLE.
            FIELD-SYMBOLS <exp_standard_table> TYPE STANDARD TABLE.

            ASSIGN act TO <act_standard_table>.
            ASSIGN exp TO <exp_standard_table>.

            DATA(lv_std_table_assert_failed) =
              assert_standard_table(
                EXPORTING iv_clear_table_ind          = abap_true
                          iv_table_name               = field_name  "Todo: delete Hungarian naming
                          iv_sort_standard_table_ind  = sort_standard_table_ind
                CHANGING  ct_act_table = <act_standard_table>
                          ct_exp_table = <exp_standard_table> ).

            IF lv_std_table_assert_failed = abap_true.
              assertion_failed = abap_true.
            ENDIF.

            REFRESH <act_standard_table>.

        ENDCASE.

      WHEN cl_abap_typedescr=>kind_struct.

        DATA(lr_act_struct_descr) = CAST cl_abap_structdescr( lr_act_type_descr ).

        DATA(lt_components) = lr_act_struct_descr->get_components( ).

        LOOP AT lt_components
          ASSIGNING FIELD-SYMBOL(<ls_component>).

          "Todo: testen met structure includes

          ASSIGN COMPONENT <ls_component>-name
            OF STRUCTURE act
            TO FIELD-SYMBOL(<component_act>).

          ASSIGN COMPONENT <ls_component>-name
            OF STRUCTURE exp
            TO FIELD-SYMBOL(<component_exp>).

          DATA(lv_assert_failed) =
            zcl_abap_unit_assert=>assert_equals_by_ref(
              EXPORTING
                field_name           =
                  COND string( WHEN field_name IS NOT INITIAL THEN field_name && |-| ELSE || ) &&
                  <ls_component>-name
                ignore_hash_sequence    = ignore_hash_sequence
                tol                     = tol
                msg                     = msg
                level                   = if_aunit_constants=>severity-medium
                skip_default_assert     = abap_true
                sort_standard_table_ind = sort_standard_table_ind
              CHANGING
                act                  = <component_act>
                exp                  = <component_exp> ).

          IF lv_assert_failed = abap_true.
            assertion_failed = abap_true.
          ENDIF.

        ENDLOOP.

        "Todo: loop

    ENDCASE.


    IF skip_default_assert  = abap_false.

      "Assert the rest in a standard way.
      DATA(lv_sap_assertion_failed) = cl_abap_unit_assert=>assert_equals(
          act                  = act
          exp                  = exp
          ignore_hash_sequence = ignore_hash_sequence
          tol                  = tol
          msg                  = msg
          level                = if_aunit_constants=>severity-medium
          quit                 = if_aunit_constants=>quit-test ).

      IF lv_sap_assertion_failed = abap_true.
        assertion_failed = abap_true.
      ENDIF.

    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Protected Method ZCL_ABAP_UNIT_ASSERT=>ASSERT_STANDARD_TABLE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_CLEAR_TABLE_IND             TYPE        ABAP_BOOL
* | [--->] IV_TABLE_NAME                  TYPE        TYPENAME
* | [--->] IV_SORT_STANDARD_TABLE_IND     TYPE        ABAP_BOOL
* | [<-->] CT_ACT_TABLE                   TYPE        STANDARD TABLE
* | [<-->] CT_EXP_TABLE                   TYPE        STANDARD TABLE
* | [<-()] RV_ASSERT_FAILED               TYPE        ABAP_BOOL
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD assert_standard_table.

    "**************************************************************
    "Sort tables
    IF iv_sort_standard_table_ind = abap_true.
      SORT ct_act_table.
      SORT ct_exp_table.
    ENDIF.

    "**************************************************************
    "Check line count
    DATA(act_table_line_count) = lines( ct_act_table ).
    DATA(exp_table_line_count) = lines( ct_exp_table ).

    DATA msg TYPE c LENGTH 200.

    DATA(table_line_count_failed) = cl_abap_unit_assert=>assert_equals(
      act  = act_table_line_count
      exp  = exp_table_line_count
      msg  =
             |Table line count | &&
             COND string( WHEN iv_table_name IS NOT INITIAL THEN |of | && iv_table_name ELSE || ) &&
             | are not equal.|
      quit = if_aunit_constants=>no ).

    "**************************************************************
    "Set max. field differences
    IF table_line_count_failed = abap_true.
      DATA(field_diff_max_count) = 30.
      rv_assert_failed = abap_true.
    ELSE.
      field_diff_max_count = 30.
    ENDIF.

    "**************************************************************
    "Loop at records
    DATA(field_diff_count) = 0.

    LOOP AT ct_act_table
      ASSIGNING FIELD-SYMBOL(<ls_act_record>).

      DATA(lv_tabix) = sy-tabix.

      READ TABLE ct_exp_table
        INDEX lv_tabix
        ASSIGNING FIELD-SYMBOL(<ls_exp_record>).

      "If expected table has less records, than stop comparing
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.

      DATA(lr_struct_descr) = CAST cl_abap_structdescr( cl_abap_typedescr=>describe_by_data( <ls_act_record> ) ).

      DATA(lt_components) = get_all_element_components( lr_struct_descr ).

      LOOP AT lt_components
        ASSIGNING FIELD-SYMBOL(<ls_comp>).

        ASSIGN COMPONENT <ls_comp>-name
          OF STRUCTURE <ls_act_record>
          TO FIELD-SYMBOL(<ls_act_value>).

        IF sy-subrc <> 0.
          cl_abap_unit_assert=>fail(
            msg  = |Actual record has no field { <ls_comp>-name }.|
            quit = if_aunit_constants=>method ).
        ENDIF.

        ASSIGN COMPONENT <ls_comp>-name
          OF STRUCTURE <ls_exp_record>
          TO FIELD-SYMBOL(<ls_exp_value>).

        IF sy-subrc <> 0.
          cl_abap_unit_assert=>fail(
            msg  = |Expected record has no field { <ls_comp>-name }.|
            quit = if_aunit_constants=>method ).
        ENDIF.

        IF <ls_act_value> <> <ls_exp_value>.

          DATA(lv_record_text) = get_record_text(
            record     = <ls_act_record>
            components = lt_components ).

          DATA(act_text) = convert_to_string( <ls_act_value> ).
          DATA(exp_text) = convert_to_string( <ls_exp_value> ).

          cl_abap_unit_assert=>fail(
            msg    = |Standard table: | && iv_table_name && |, | &&
                     |index: | && lv_tabix && |, | &&
                     |field: | && <ls_comp>-name
            detail = |Expected [{ exp_text }] Actual [{ act_text }]. Record: { lv_record_text }| ).

        ENDIF.



*         cl_abap_unit_assert=>assert_equals(
*           act = <ls_act_value>
*           exp = <ls_exp_value>
*           msg = |Standard table: | && iv_table_name && |, | &&
*             |index: | && lv_tabix && |, | &&
*             |field: | && <ls_comp>-name
*            quit = if_aunit_constants=>no ).




*        IF lv_assert_failed = abap_true.
*
*          field_diff_count = field_diff_count + 1.
*
*          IF field_diff_count >= field_diff_max_count.
*
*            cl_abap_unit_assert=>fail(
*              msg  = |Maximum number of field differences { field_diff_max_count } is reached. There might be more differences.|
*              quit = if_aunit_constants=>no ).
*
*            EXIT.
*
*          ENDIF.
*
*          rv_assert_failed = abap_true.
*
*        ENDIF.

      ENDLOOP.

    ENDLOOP.

    IF iv_clear_table_ind = abap_true.
      REFRESH ct_act_table.
      REFRESH ct_exp_table.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_ABAP_UNIT_ASSERT=>CONVERT_TO_STRING
* +-------------------------------------------------------------------------------------------------+
* | [--->] VALUE                          TYPE        ANY
* | [<-()] VALUE_TEXT                     TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD convert_to_string.

    value_text = |{ value }|.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_ABAP_UNIT_ASSERT=>FAIL_BAPIRET2_T
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_TEXT                        TYPE        STRING(optional)
* | [--->] IV_LEVEL                       TYPE        INT1 (default =IF_AUNIT_CONSTANTS=>SEVERITY-MEDIUM)
* | [--->] IV_QUIT                        TYPE        INT1 (default =IF_AUNIT_CONSTANTS=>QUIT-TEST)
* | [--->] IT_BAPIRET2_T                  TYPE        BAPIRET2_T
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD fail_bapiret2_t.

    DATA(lt_bapiret2_t) = it_bapiret2_t[].

    DELETE lt_bapiret2_t
      WHERE type CN 'EAX'.

    DATA(lv_lines) = lines( lt_bapiret2_t ).

    LOOP AT lt_bapiret2_t
      ASSIGNING FIELD-SYMBOL(<ls_return>).

      "Critical Assertion Error: 'ZCX_RETURN melding - Type: E ,ID: BS ,No.: 013 ,Values.: VRIJ ORD AP00001003 Vrijgegeven Terugname te
      DATA lv_message TYPE string.

      lv_message =
        iv_text && | | &&
        |BAPIRET2 message: | &&
        |Type: { <ls_return>-type } , | &&
        |ID: { <ls_return>-id } , | &&
        |No.: { <ls_return>-number } , | &&
        |Values.: { <ls_return>-message_v1 } # { <ls_return>-message_v2 } # { <ls_return>-message_v3 } # { <ls_return>-message_v4 }|.

      MESSAGE ID <ls_return>-id TYPE <ls_return>-type NUMBER <ls_return>-number
        WITH <ls_return>-message_v1 <ls_return>-message_v2 <ls_return>-message_v3 <ls_return>-message_v4
        INTO DATA(lv_detail).

      "Quit at last line
      IF lv_lines = sy-tabix.
        DATA(lv_quit) = iv_quit.
      ELSE.
        lv_quit = if_aunit_constants=>no.
      ENDIF.

      cl_abap_unit_assert=>fail(
        msg         = lv_message
        level	      = iv_level
        quit        = lv_quit
        detail      = lv_detail ).

    ENDLOOP.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Protected Method ZCL_ABAP_UNIT_ASSERT=>GET_ALL_ELEMENT_COMPONENTS
* +-------------------------------------------------------------------------------------------------+
* | [--->] IR_STRUCT_DESCR                TYPE REF TO CL_ABAP_STRUCTDESCR
* | [<-()] RT_COMPONENTS                  TYPE        CL_ABAP_STRUCTDESCR=>COMPONENT_TABLE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_all_element_components.

    DATA(lt_components) = ir_struct_descr->get_components( ).

    LOOP AT lt_components
      ASSIGNING FIELD-SYMBOL(<ls_component>).

      IF <ls_component>-as_include = abap_false.

        APPEND <ls_component> TO rt_components.

      ELSE.

        DATA(lr_struct_descr) = CAST cl_abap_structdescr( <ls_component>-type ).
        DATA(lt_include_components) = get_all_element_components( lr_struct_descr ).

        APPEND LINES OF lt_include_components
          TO rt_components.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_ABAP_UNIT_ASSERT=>GET_RECORD_TEXT
* +-------------------------------------------------------------------------------------------------+
* | [--->] RECORD                         TYPE        ANY
* | [--->] COMPONENTS                     TYPE        CL_ABAP_STRUCTDESCR=>COMPONENT_TABLE
* | [<-()] TEXT                           TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_record_text.

    LOOP AT components
      ASSIGNING FIELD-SYMBOL(<component>).

      IF sy-tabix > 100.
        text = text && | ...|.
        EXIT.
      ENDIF.

      IF sy-tabix > 1.
        text = text && |\||.
      ENDIF.

      ASSIGN COMPONENT <component>-name
        OF STRUCTURE record
        TO FIELD-SYMBOL(<value>).

      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      DATA(value_text) = convert_to_string( <value> ).

      text = text && value_text.

    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
