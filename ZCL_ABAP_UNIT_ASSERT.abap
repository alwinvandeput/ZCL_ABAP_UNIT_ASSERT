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
    returning
      value(ASSERTION_FAILED) type ABAP_BOOL .
  class-methods ASSERT_BAPIRET2_T
    importing
      !IT_ACT_BAPIRET2 type BAPIRET2_T
      !IT_EXP_BAPIRET2 type GTT_ASSERT_BAPIRET2
      !IV_QUIT type INT1 default IF_AUNIT_CONSTANTS=>METHOD .
protected section.

  class-methods ASSERT_EQUALS_BY_REF
    importing
      !FIELD_NAME type CHAR30 optional
      !IGNORE_HASH_SEQUENCE type ABAP_BOOL
      !TOL type F
      !MSG type CSEQUENCE
      !LEVEL type INT1
      !SKIP_DEFAULT_ASSERT type ABAP_BOOL
    changing
      !ACT type ANY
      !EXP type ANY
    returning
      value(ASSERTION_FAILED) type ABAP_BOOL .
  class-methods ASSERT_STANDARD_TABLE
    importing
      !IV_CLEAR_TABLE_IND type ABAP_BOOL
      !IV_TABLE_NAME type TYPENAME
    changing
      !CT_ACT_TABLE type STANDARD TABLE
      !CT_EXP_TABLE type STANDARD TABLE
    returning
      value(RV_ASSERT_FAILED) type ABAP_BOOL .
private section.
ENDCLASS.



CLASS ZCL_ABAP_UNIT_ASSERT IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_ABAP_UNIT_ASSERT=>ASSERT_BAPIRET2_T
* +-------------------------------------------------------------------------------------------------+
* | [--->] IT_ACT_BAPIRET2                TYPE        BAPIRET2_T
* | [--->] IT_EXP_BAPIRET2                TYPE        GTT_ASSERT_BAPIRET2
* | [--->] IV_QUIT                        TYPE        INT1 (default =IF_AUNIT_CONSTANTS=>METHOD)
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD ASSERT_BAPIRET2_T.

    DATA(lt_act_bapiret2) = it_act_bapiret2.

    DELETE lt_act_bapiret2
      WHERE type CA 'IS'.

    "************************************************
    "Check unknow messages
    LOOP AT it_exp_bapiret2
      ASSIGNING FIELD-SYMBOL(<ls_exp_bapiret2>).

      data(lv_found_ind) = abap_false.

      LOOP AT lt_act_bapiret2
        ASSIGNING FIELD-SYMBOL(<ls_act_bapiret2_2>)
        WHERE
          type       = <ls_exp_bapiret2>-type AND
          id         = <ls_exp_bapiret2>-id AND
          number     = <ls_exp_bapiret2>-number.

        "'*' is a wild card, so the value won't be checked.
        if <ls_exp_bapiret2>-message_v1 = '*'.
          <ls_act_bapiret2_2>-message_v1 = '*'.
        endif.

        if <ls_exp_bapiret2>-message_v2 = '*'.
          <ls_act_bapiret2_2>-message_v2 = '*'.
        endif.

        if <ls_exp_bapiret2>-message_v3 = '*'.
          <ls_act_bapiret2_2>-message_v3 = '*'.
        endif.

        if <ls_exp_bapiret2>-message_v4 = '*'.
          <ls_act_bapiret2_2>-message_v4 = '*'.
        endif.


        IF <ls_act_bapiret2_2>-message_v1 = <ls_exp_bapiret2>-message_v1 AND
           <ls_act_bapiret2_2>-message_v2 = <ls_exp_bapiret2>-message_v2 AND
           <ls_act_bapiret2_2>-message_v3 = <ls_exp_bapiret2>-message_v3 AND
           <ls_act_bapiret2_2>-message_v4 = <ls_exp_bapiret2>-message_v4.

          DELETE lt_act_bapiret2.

          lv_found_ind = abap_true.

        ENDIF.

      ENDLOOP.

      IF lv_found_ind = abap_false.

        cl_abap_unit_assert=>fail(
            msg    = 'ASSERT_BAPIRET2_T: Missing actual message.'
            level  = if_aunit_constants=>severity-medium
            quit   = if_aunit_constants=>no
            detail =
              |Type: { <ls_exp_bapiret2>-type } ,| &&
              |id: { <ls_exp_bapiret2>-id } ,| &&
              |Number: { <ls_exp_bapiret2>-number } ,| &&
              |V1: { <ls_exp_bapiret2>-message_v1 } ,| &&
              |V2: { <ls_exp_bapiret2>-message_v2 } ,| &&
              |V3: { <ls_exp_bapiret2>-message_v3 } ,| &&
              |V4: { <ls_exp_bapiret2>-message_v4 }| ).

        DATA(lv_failed) = abap_true.

      ENDIF.

    ENDLOOP.

    "************************************************
    "Check unknow messages

    LOOP AT lt_act_bapiret2
      ASSIGNING FIELD-SYMBOL(<ls_act_bapiret2>).

      cl_abap_unit_assert=>fail(
          msg    = 'ASSERT_BAPIRET2_T: Not expected message.'
          level  = if_aunit_constants=>severity-medium
          quit   = if_aunit_constants=>no
          detail =
            |Type: { <ls_act_bapiret2>-type } ,| &&
            |id: { <ls_act_bapiret2>-id } ,| &&
            |Number: { <ls_act_bapiret2>-number } ,| &&
            |V1: { <ls_act_bapiret2>-message_v1 } ,| &&
            |V2: { <ls_act_bapiret2>-message_v2 } ,| &&
            |V3: { <ls_act_bapiret2>-message_v3 } ,| &&
            |V4: { <ls_act_bapiret2>-message_v4 }| ).

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
                EXPORTING iv_clear_table_ind = abap_true
                          iv_table_name     = field_name  "Todo: delete Hungarian naming
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
                ignore_hash_sequence = ignore_hash_sequence
                tol                  = tol
                msg                  = msg
                level                = if_aunit_constants=>severity-medium
                skip_default_assert  = abap_true
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
* | [<-->] CT_ACT_TABLE                   TYPE        STANDARD TABLE
* | [<-->] CT_EXP_TABLE                   TYPE        STANDARD TABLE
* | [<-()] RV_ASSERT_FAILED               TYPE        ABAP_BOOL
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD assert_standard_table.

    "**************************************************************
    "Sort tables
    SORT ct_act_table.
    SORT ct_exp_table.

    "**************************************************************
    "Check line count
    DATA(act_table_line_count) = lines( ct_act_table ).
    DATA(exp_table_line_count) = lines( ct_exp_table ).

    DATA msg TYPE c LENGTH 200.

    DATA(table_line_count_failed) = cl_abap_unit_assert=>assert_equals(
        act                  = act_table_line_count
        exp                  = exp_table_line_count
        msg                  =
          |Table line count | &&
          COND string( WHEN iv_table_name IS NOT INITIAL THEN |of | && iv_table_name ELSE || ) &&
          | are not equal.|
        quit                 = if_aunit_constants=>no ).

    "**************************************************************
    "Set max. field differences
    IF table_line_count_failed = abap_true.
      DATA(field_diff_max_count) = 5.
      rv_assert_failed = abap_true.
    ELSE.
      field_diff_max_count = 10.
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

      DATA(lt_components) = lr_struct_descr->get_components( ).

      LOOP AT lt_components
        ASSIGNING FIELD-SYMBOL(<ls_comp>).

        ASSIGN COMPONENT <ls_comp>-name
          OF STRUCTURE <ls_act_record>
          TO FIELD-SYMBOL(<ls_act_value>).

        IF sy-subrc <> 0.
          cl_abap_unit_assert=>fail(
            msg	= |Actual record has no field { <ls_comp>-name }.|
            quit = if_aunit_constants=>method ).
        ENDIF.

        ASSIGN COMPONENT <ls_comp>-name
          OF STRUCTURE <ls_exp_record>
          TO FIELD-SYMBOL(<ls_exp_value>).

        IF sy-subrc <> 0.
          cl_abap_unit_assert=>fail(
            msg	= |Expected record has no field { <ls_comp>-name }.|
            quit = if_aunit_constants=>method ).
        ENDIF.

        DATA(lv_assert_failed) =
         cl_abap_unit_assert=>assert_equals(
           act = <ls_act_value>
           exp = <ls_exp_value>
           msg = |Standard table: | && iv_table_name && |, | &&
             |index: | && lv_tabix && |, | &&
             |field: | && <ls_comp>-name
            quit = if_aunit_constants=>no ).

        IF lv_assert_failed = abap_true.

          field_diff_count = field_diff_count + 1.

          IF field_diff_count >= field_diff_max_count.

            cl_abap_unit_assert=>fail(
              msg	= |Maximum number of field differences { field_diff_max_count } is reached. There might be more differences.|
              quit = if_aunit_constants=>no ).

            EXIT.

          ENDIF.

          rv_assert_failed = abap_true.

        ENDIF.

      ENDLOOP.

    ENDLOOP.

    IF iv_clear_table_ind = abap_true.
      REFRESH ct_act_table.
      REFRESH ct_exp_table.
    ENDIF.

  ENDMETHOD.
ENDCLASS.