*&---------------------------------------------------------------------*
*& Report zml_check_abapn
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zml_check_abapn.

CLASS lcl_demo DEFINITION.
  PUBLIC SECTION.
    METHODS : disp IMPORTING it_value TYPE any.
    TYPES: ty_flight TYPE TABLE OF spfli WITH EMPTY KEY.
    METHODS : display IMPORTING it_flight TYPE ty_flight.
    METHODS : displayany IMPORTING it_flight TYPE any.
ENDCLASS.

CLASS lcl_demo IMPLEMENTATION.
  METHOD disp.
    "WRITE :/ 'Hello'.
    cl_demo_output=>write( it_value ).
  ENDMETHOD.

  METHOD display.
    cl_demo_output=>display_data( EXPORTING value = it_flight ).
  ENDMETHOD.

  METHOD displayany.
    cl_demo_output=>display_data( EXPORTING value = it_flight ).
  ENDMETHOD.

ENDCLASS.


START-OF-SELECTION.

* 1. Instance Operator NEW.

* 1.1 A constructor expression with the instance operator NEW creates an
* anonymous data object.
* example of in-line creating object using NEW
  DATA lo_dref TYPE REF TO data.
  FIELD-SYMBOLS <fs> TYPE data.

  lo_dref = NEW i( 212 ).
  ASSIGN lo_dref->* TO <fs>.

* 1.1.1 using NEW to create local instance of class by modern way.
  DATA(my_lcl_demo) = NEW lcl_demo( ).

  my_lcl_demo->disp( it_value = '1. Instance Operator NEW.' ).
  my_lcl_demo->disp( it_value = <fs> ).


* 1.2. A constructor expression with the instance operator NEW creates an
* instance of a class.
* example of in-line creating of object using NEW #( )
  DATA : lo_obj2 TYPE REF TO lcl_demo.
  lo_obj2 = NEW #( ).
  CALL METHOD lo_obj2->disp
    EXPORTING
      it_value = 'Hello 1'.

* new way(2) to create object
  DATA : lo_obj3 TYPE REF TO lcl_demo.
  lo_obj3 = NEW lcl_demo(  ).
  CALL METHOD lo_obj3->disp
    EXPORTING
      it_value = 'Hello 2'.

* new way(3) to create object
  NEW lcl_demo(  )->disp( it_value = 'Hello 3'  ).

* 1.3. Instance Operator NEW - Internal Tables
* Create simple program where the instance operator NEW is used with Internal Tables.
* E.g.: Constructs three anonymous internal tables with an elementary row type. The
* first table is filled with three rows. The second row is initial. The second and third
* tables are filled with the rows of the first table and three further rows. Using BASE
* and LINES OF here has the same effect.

  TYPES lt_tab TYPE TABLE OF i WITH EMPTY KEY.

  DATA(lt_dref1) = NEW lt_tab( ( 1 ) (  ) ( 3 ) ).
  DATA(lt_dref2) = NEW lt_tab( BASE lt_dref1->* (  ) ( 5 ) (  ) ).
  DATA(lt_dref3) = NEW lt_tab( ( LINES OF lt_dref1->* ) (  ) ( 5 ) (  ) ).
  DATA(lt_dref4) = NEW lt_tab( ( LINES OF lt_dref3->* ) ( 6 ) ( 7 ) ( 8 ) ).



  cl_demo_output=>write( lt_dref1->* ).
  cl_demo_output=>write( lt_dref2->* ).
  cl_demo_output=>write( lt_dref3->* ).
  cl_demo_output=>display( lt_dref4->* ). " it is about visualization: use display in the end to show it from a program

* 2. Value Operator VALUE.

  cl_demo_output=>write( `2. Value Operator VALUE.` ).
* 2.1. A constructor expression with the value operator VALUE constructs a
* structure contains value for each component.

  DATA:
    BEGIN OF ls_struct,
      col1 TYPE i VALUE 1,
      col2 TYPE i VALUE 2,
      col3 TYPE i VALUE 3,
      col4 TYPE i VALUE 4,
    END OF ls_struct,

    ls_struct1 LIKE ls_struct,
    ls_struct2 LIKE ls_struct.
  ls_struct1 = ls_struct2 = ls_struct.

  ls_struct1 = VALUE #(
    col1 = ls_struct1-col2
    col2 = ls_struct1-col1
    col4 = 5
    col3 = ls_struct1-col4
    ).
  cl_demo_output=>write( ls_struct1 ).

  ls_struct2 = VALUE #(
    LET x = ls_struct2 IN
    col1 = x-col2
    col2 = x-col1
    col3 = x-col4
    col4 = 5
   ).

* 2.2. A constructor expression with the value operator VALUE used for Internal
* Tables.

  TYPES: scarr_tab TYPE STANDARD TABLE OF scarr WITH EMPTY KEY.

  DATA(lt_scarr1) = VALUE scarr_tab(

    ( mandt = syst-mandt carrid = '11' carrname = '11 Carrier' currcode = 'EUR' url = '11Carrier.com' )
    ( mandt = syst-mandt carrid = '22' carrname = '22 Carrier' currcode = 'EUR' url = '22Carrier.com' )
    ( mandt = syst-mandt carrid = '33' carrname = '33 Carrier' currcode = 'EUR' url = '33Carrier.com' )
    ( mandt = syst-mandt carrid = '44' carrname = '44 Carrier' currcode = 'EUR' url = '44Carrier.com' )
    ( mandt = syst-mandt carrid = '55' carrname = '55 Carrier' currcode = 'EUR' url = '55Carrier.com' )
  ).
  cl_demo_output=>write( lt_scarr1 ).
  " it is the same like lt_scarr2 = lt_scarr1.
  DATA(lt_scarr2) = VALUE scarr_tab( ( LINES OF lt_scarr1 ) ).
  cl_demo_output=>write( lt_scarr2 ).

  DATA(lt_scarr3) = VALUE scarr_tab(
    ( LINES OF lt_scarr1 )
    ( mandt = syst-mandt carrid = '66' carrname = '66 Carrier' currcode = 'EUR' url = '66Carrier.com' )
    ( mandt = syst-mandt carrid = '77' carrname = '77 Carrier' currcode = 'EUR' url = '77Carrier.com' )
  ).

  DATA(lt_scarr4) = VALUE scarr_tab(
    ( LINES OF lt_scarr1 FROM 2 TO 4 )
    ( mandt = syst-mandt carrid = '66' carrname = '66 Carrier' currcode = 'EUR' url = '66Carrier.com' )
    ( mandt = syst-mandt carrid = '77' carrname = '77 Carrier' currcode = 'EUR' url = '77Carrier.com' )
  ).

  DATA(lt_scarr5) = VALUE scarr_tab(
    BASE lt_scarr1
    ( mandt = syst-mandt carrid = '66' carrname = '66 Carrier' currcode = 'EUR' url = '66Carrier.com' )
    ( mandt = syst-mandt carrid = '77' carrname = '77 Carrier' currcode = 'EUR' url = '77Carrier.com' )
    ( mandt = syst-mandt carrid = '88' carrname = '88 Carrier' currcode = 'EUR' url = '88Carrier.com' )
  ).

  DATA(lt_scarr6) = VALUE scarr_tab(
    BASE lt_scarr1
    ( LINES OF lt_scarr1 )
    ( mandt = syst-mandt carrid = '66' carrname = '66 Carrier' currcode = 'EUR' url = '66Carrier.com' )
    ( mandt = syst-mandt carrid = '77' carrname = '77 Carrier' currcode = 'EUR' url = '77Carrier.com' )
    ( mandt = syst-mandt carrid = '88' carrname = '88 Carrier' currcode = 'EUR' url = '88Carrier.com' )
  ).

  cl_demo_output=>write( lt_scarr3 ).
  cl_demo_output=>write( lt_scarr4 ).
  cl_demo_output=>write( lt_scarr5 ).
  cl_demo_output=>display( lt_scarr6 ).

* 3. Component Operator CORRESPONDING.

  cl_demo_output=>write( `3. Component Operator CORRESPONDING.` ).

  TYPES: BEGIN OF lst_line1,
           col1 TYPE i,
           col2 TYPE i,
         END OF lst_line1.

  TYPES: BEGIN OF lst_line2,
           col1 TYPE i,
           col2 TYPE i,
           col3 TYPE i,
         END OF lst_line2.

  DATA(ls_line1) = VALUE lst_line1( col1 = 1 col2 = 2 ).

  cl_demo_output=>write( |ls_line1 = ' { ls_line1-col1 } {  ls_line1-col2 } | ).

  DATA(ls_line2) = VALUE lst_line2( col1 = 4 col2 = 5 col3 = 6 ).
  cl_demo_output=>write( |ls_line2 = { ls_line2-col1 } { ls_line2-col2 } { ls_line2-col3 } | ).

  SKIP 2.

  ls_line2 = CORRESPONDING #( ls_line1 ).
  cl_demo_output=>write( |ls_line2 = CORRESPONDING #( ls_line1 ) 'Result is ls_line2 = { ls_line2-col1 } { ls_line2-col2 } { ls_line2-col3 } | ).

  SKIP.
  ls_line2 = VALUE lst_line2( col1 = 4 col2 = 5 col3 = 6 ). "Restore ls_line2
  ls_line2 = CORRESPONDING #( BASE ( ls_line2 ) ls_line1 ).
  cl_demo_output=>write( |ls_line2 = CORRESPONDING #( BASE ( ls_line2 ) ls_line1 ) 'Result is ls_line2 = { ls_line2-col1 } { ls_line2-col2 } { ls_line2-col3 } | ).

  SKIP.
  ls_line2 = VALUE lst_line2( col1 = 4 col2 = 5 col3 = 6 ). "Restore ls_line2
  DATA(ls_line3) = CORRESPONDING lst_line2( BASE ( ls_line2 ) ls_line1 ).
  cl_demo_output=>write( |DATA(ls_line3) = CORRESPONDING line2( BASE ( ls_line2 ) ls_line1 ) 'Result is ls_line3 = { ls_line3-col1 } { ls_line3-col2 } { ls_line3-col3 } | ).

  DATA(line22) = VALUE lst_line1( col1 = 1 col2 = 25 ).
  DATA(line4) = CORRESPONDING lst_line2( BASE ( ls_line3 ) line22 ).
  SKIP.
  cl_demo_output=>write( |DATA(line4) = CORRESPONDING lst_line2( BASE ( ls_line3 ) line22 ). line4 = { line4-col1 } { line4-col2 } { line4-col3 } | ).

  cl_demo_output=>display( ).


* 4. Conversion Operator CONV.

  cl_demo_output=>write( `4. Conversion Operator CONV.` ).

* we will use class lcl_demo for output where we added new method display and type ty_flight

* E.g.: Here we have a method display for class lcl_demo which accepts an importing table,
* but in the method call of this method we use a little different type of table.
* If we assign table with slightly different type we will have compilation time error as type
* incompatible.
* Using function CONV in such case, we can make conversion valid with the use of # operand.

  DATA: lt_flight TYPE TABLE OF spfli WITH KEY carrid connid.

  SELECT * FROM spfli INTO TABLE lt_flight.

  DATA(lr_demo) = NEW lcl_demo(  ).
  " conv # here makes possible to use table with type to table which is used for type of parameter
  " without such conv  # we will have ERROR of incompatibility of types
  lr_demo->display( exporting it_flight = conv #( lt_flight ) ).


* 4.01 it is mine
* Using type ANY where you can use conv # or direct assignment

  " or this way (exporting is not needing as you can see)
  SELECT * FROM scarr INTO TABLE @DATA(zscarr).
  "  lr_demo->DISPLAYANY( exporting IT_FLIGHT = conv #( zscarr ) ).


  " My experiment with type any (it is slightly different):
  SELECT * FROM scarr INTO TABLE @DATA(zflights).
  "  lr_demo->DISPLAYANY( exporting IT_FLIGHT =  zflights ).

  cl_demo_output=>display( ).

* 5. Table Expressions.
* Create simple program where the Table expressions are used.

  cl_demo_output=>write( `5. Table Expressions.` ).

* E.g.: Reading table entry using the Key TABLE_LINE

  DATA: lt_data TYPE STANDARD TABLE OF i.
  DO 3 TIMES.
    APPEND sy-index TO lt_data.
  ENDDO.

  " Mikhail's way:
  DESCRIBE TABLE lt_data LINES DATA(z_s_lt_data).
  DO z_s_lt_data TIMES.
    cl_demo_output=>write( |ZML: { lt_data[ sy-index ] } | ).
  ENDDO.

  " Mikhail's 2 way:
  DO lines( lt_data ) TIMES.
    cl_demo_output=>write( |ZML: { lt_data[ sy-index ] } | ).
  ENDDO.

  "Old way:
  DATA: lv_data TYPE i.
  DO 5 TIMES.
    READ TABLE lt_data INTO lv_data WITH KEY table_line = sy-index.
    IF sy-subrc EQ 0.
      cl_demo_output=>write( lv_data ).
    ELSE.
      cl_demo_output=>write( |Not found { sy-index  } | ).
    ENDIF.
  ENDDO.

  " new way using Table expressions:
  DO 5 TIMES.
    TRY .
        cl_demo_output=>write( lt_data[ table_line = sy-index ] ).
      CATCH cx_sy_itab_line_not_found.
        cl_demo_output=>write( |Not found { sy-index  } | ).
    ENDTRY.
  ENDDO.

  cl_demo_output=>display( ).


* 6. LET expression.
* Create simple program where the LET expression is used.

  cl_demo_output=>write( `6. LET expression.` ).

  TYPES :
    BEGIN OF ty_date,
      year  TYPE c LENGTH 4,
      month TYPE c LENGTH 2,
      day   TYPE c LENGTH 2,
    END OF ty_date,
    lt_dates TYPE TABLE OF ty_date WITH EMPTY KEY.

  DATA(lt_dates) = VALUE lt_dates(
    ( year = '2018' month = '07' day = '16' )
    ( year = '2019' month = '08' day = '20' )
    ( year = '2020' month = '05' day = '07' )
  ).

  DO lines( lt_dates ) TIMES.
    DATA(lv_isodate) = CONV string(
    LET ls_date = lt_dates[ sy-index ]
    sep = '-'
    IN ls_date-year && sep && ls_date-month && sep && ls_date-day ).
    "WRITE: / lv_isodate.
    cl_demo_output=>write( lv_isodate ).
  ENDDO.

  cl_demo_output=>display( ).

* 7. Constructor expression FILTER.

  cl_demo_output=>write( `7. Constructor expression FILTER.` ).

  TYPES:
    BEGIN OF ty_flt,
      coun_from TYPE land1,
    END OF ty_flt,
    tt_flt TYPE HASHED TABLE OF ty_flt WITH UNIQUE KEY coun_from.

  DATA:
    lt_spfli1 TYPE TABLE OF spfli,
    lt_spfli2 TYPE TABLE OF spfli,
    lt_flt    TYPE tt_flt.

  SELECT * FROM spfli INTO TABLE lt_spfli1.

  " Fill value on filter table LT_FLT.
  lt_flt = VALUE tt_flt(
    ( coun_from = 'US' )
    ( coun_from = 'DE' )
    ( coun_from = 'JP' )
    ( coun_from = 'SG' )
  ).

  " FILTERING apply on internal table LT_SPFLI1 based on filter table.
  lt_spfli2 = FILTER #( lt_spfli1 IN lt_flt WHERE countryfr = coun_from ).

  cl_demo_output=>write( lt_spfli1 ).
  cl_demo_output=>display( lt_spfli2 ).



* 8. Internal Tables - LOOP AT .. GROUP BY.

  DATA(lo_out) = cl_demo_output=>new( ).
  lo_out->write( `8. Internal Tables - LOOP AT .. GROUP BY.` ).

  TYPES:
    BEGIN OF ty_employees,
      position(10) TYPE c,
      name         TYPE char10,
      emp_name(20) TYPE c,
    END OF ty_employees.

  TYPES:
    ltt_employees TYPE STANDARD TABLE OF ty_employees WITH EMPTY KEY.

  " Populating internal table for our example.

  DATA(lt_emp_info) = VALUE ltt_employees(
    ( position ='p001' name = 'Manager' emp_name = 'Amanada' )
    ( position ='p001' name = 'Executive' emp_name = 'Sandy' )
    ( position ='p001' name = 'Executive' emp_name = 'Sam' )
    ( position ='p004' name = 'Fin.mang' emp_name = 'Nagen' )
    ( position ='p005' name = 'Fin.mang' emp_name = 'Tom' )
  ).

  "DATA(lt_members2) = VALUE ltt_employees(  ).

  "cl_demo_output=>display( lt_members2 ).

  "EXIT.


  LOOP AT lt_emp_info INTO DATA(ls_wa)
    GROUP BY ( position = ls_wa-position grouped_nb_recs = GROUP SIZE group_index = GROUP INDEX )
      ASCENDING
        ASSIGNING FIELD-SYMBOL(<lfs_group_info>).

    lo_out->begin_section( | Group No: { <lfs_group_info>-group_index } | &&
    | no.of records in each group : { <lfs_group_info>-grouped_nb_recs } | ).

    DATA(lt_members) = VALUE ltt_employees( FOR ls_wa2 IN GROUP <lfs_group_info> ( ls_wa2 ) ).
    lo_out->write( lt_members ).
    lo_out->end_section( ).

  ENDLOOP.

  lo_out->display( ).



* 9. Iteration Expression in ABAP 7.40 +, FOR Expression.

* 9.1. FOR expression in ABAP 7.40.

  cl_demo_output=>write( '9. Iteration Expression in ABAP 7.40 +, FOR Expression.' ).

  TYPES:
    BEGIN OF ty_struct1,
      field1 TYPE i,
      field2 TYPE string,
    END OF ty_struct1,
    BEGIN OF ty_struct2,
      field1 TYPE i,
      field2 TYPE string,
      field3 TYPE i,
    END OF ty_struct2.

  TYPES:
    gtt_struct1 TYPE STANDARD TABLE OF ty_struct1 WITH DEFAULT KEY,
    gtt_struct2 TYPE STANDARD TABLE OF ty_struct2 WITH DEFAULT KEY.

  DATA(lt_source) = VALUE gtt_struct1(
    ( field1 = 1 field2 = 'A' )
    ( field1 = 2 field2 = 'B' )
  ).

  " use FOR like simple move CORRESPONDING
  DATA(lt_target1) = VALUE gtt_struct2(
    FOR lwa_source IN lt_source (  CORRESPONDING
      #( lwa_source )
     )
  ).
  cl_demo_output=>write_data( lt_target1 ).

  " populate sy-tabix in the additional fields within the for loop.
  DATA(lt_target2) = VALUE gtt_struct2(
    FOR lwa_source IN lt_source INDEX INTO index
    LET base = VALUE ty_struct2( field3 = index )
    IN ( CORRESPONDING #( BASE ( base ) lwa_source ) )

  ).
  cl_demo_output=>write_data( lt_target2 ).


  " populate any value or call custom method in additional fields within the for loop:
  DATA(lt_target3) = VALUE gtt_struct2(
    FOR lwa_source IN lt_source INDEX INTO index
    LET base = VALUE ty_struct2( field3 = 10 ) " <<< here we can use custom method or any value
    IN ( CORRESPONDING #( BASE ( base ) lwa_source ) )
  ).
  cl_demo_output=>write_data( lt_target3 ).


* 9.2. Replace the LOOP for REDUCE operator

  DATA lt_itab TYPE STANDARD TABLE OF i WITH EMPTY KEY.

  lt_itab = VALUE #( FOR j = 1 WHILE j <= 3 ( j ) ).

  DATA(lv_sum) = REDUCE i( INIT xx = 0 FOR wa IN lt_itab NEXT xx = xx + wa ).

  DATA(lv_result) = REDUCE string(
    INIT text = `Count up:`
    FOR n = 1 UNTIL n > 3 NEXT text = text && | { n }|
  ).

  cl_demo_output=>write_data( lt_itab ).
  cl_demo_output=>write_data( lv_sum ).
  cl_demo_output=>write_data( lv_result ).

  cl_demo_output=>display( ).



* 10. Advanced part - A new special type of structure – MESH.

  cl_demo_output=>write( '10. Advanced part - A new special type of structure – MESH.' ).

  TYPES :
    BEGIN OF ty_tab1,
      col1(2),
      col2(2),
    END OF ty_tab1,
    BEGIN OF ty_tab2,
      col11(2),
      col22(2),
      col33(2),
    END OF ty_tab2.
  TYPES :
    ltt_tab1 TYPE SORTED TABLE OF ty_tab1 WITH UNIQUE KEY col1,
    ltt_tab2 TYPE SORTED TABLE OF ty_tab2 WITH UNIQUE KEY col11.

  " define MESH type with association name as to_tab2.

  TYPES :
    BEGIN OF MESH my_tab,
      tab1 TYPE ltt_tab1 ASSOCIATION to_tab2 TO tab2 ON col11 = col1,
      tab2 TYPE ltt_tab2,
    END OF MESH my_tab.

  DATA :
    ms_tab  TYPE my_tab,
    ls_tab2 TYPE ty_tab2.

  " populating tables for our example

  ms_tab-tab1 = VALUE ltt_tab1(
    ( col1 = '11' col2 = '12' )
    ( col1 = '21' col2 = '22' )
    ( col1 = '31' col2 = '32' )
    ( col1 = '41' col2 = '42' )
    ( col1 = '51' col2 = '52' )
    ).
  ms_tab-tab2 = VALUE ltt_tab2(
    ( col11 = '11' col22 = '12' col33 = '13' )
    ( col11 = '21' col22 = '22' col33 = '23' )
    ( col11 = '31' col22 = '32' col33 = '33' )
    ( col11 = '41' col22 = '42' col33 = '43' )
    ( col11 = '51' col22 = '52' col33 = '53' )
  ).

  ls_tab2 = ms_tab-tab1\to_tab2[ ms_tab-tab1[ col1 = '31' ] ].

  cl_demo_output=>write_data( ms_tab-tab1 ).
  cl_demo_output=>write_data( ms_tab-tab2 ).
  cl_demo_output=>write( `ls_tab2 = ms_tab-tab1\to_tab2[ ms_tab-tab1[ col1 = '31' ] ].` ).
  cl_demo_output=>write_data( ls_tab2 ).
  cl_demo_output=>display( ).
