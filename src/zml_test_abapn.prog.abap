*&---------------------------------------------------------------------*
*& Report zml_test_abapn
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zml_test_abapn.

START-OF-SELECTION.

  cl_demo_output=>write( 'Fill internal referenced object by NEW or object table by VALUE:' ).

  TYPES lt_tab TYPE STANDARD TABLE OF i WITH EMPTY KEY.

  DATA(lt_ref1) = NEW lt_tab( ( 1 ) ( 2 ) ( 3 ) ).
  DATA(lt_tab1) = VALUE lt_tab( ( 1 ) ( 2 ) ( 3 ) ).
  " this is example of extracting using object as a reference to table type
  cl_demo_output=>write_data( lt_ref1->* ).
  " this is example of extracting using object as a table
  cl_demo_output=>write_data( lt_tab1 ).

  " how it can be used with base for referenced object
  DO 3 TIMES.
    lt_ref1 = NEW lt_tab( BASE lt_ref1->* ( sy-index * 3 + 1 ) ( sy-index * 3 + 2 ) ( sy-index * 3 + 3 ) ).
  ENDDO.
  cl_demo_output=>write_data( lt_ref1->* ).

  " how it can be used with base for real object
  DO 3 TIMES.
    lt_tab1 = VALUE lt_tab( BASE lt_tab1 ( sy-index * 3 + 1 ) ( sy-index * 3 + 2 ) ( sy-index * 3 + 3 ) ).
  ENDDO.
  cl_demo_output=>write_data( lt_tab1 ).

  cl_demo_output=>display(  ).

* combining, sorting, extracting fast from internal tables

  cl_demo_output=>write( 'Combining, sorting, extractign fast from internal tbales:' ).

  TYPES:
    BEGIN OF ty_stru1,
      col1    TYPE i,
      col2(2),
      col3    TYPE c LENGTH 4,
    END OF ty_stru1.

  TYPES:
    ltt_tab2 TYPE STANDARD TABLE OF ty_stru1 WITH EMPTY KEY,
    ltt_tab3 TYPE SORTED TABLE OF ty_stru1 WITH UNIQUE KEY col1.

  DATA(lt_ref2) = NEW ltt_tab2( ( col1 = 1 col2 = 'AB' col3 = 'ABCD' ) ).
  cl_demo_output=>write_data( lt_ref2->* ).

  DATA(lt_tab2) = VALUE ltt_tab2( ( col1 = 2 col2 = 'SD' col3 = 'SDFG' ) ).
  lt_tab2 = VALUE ltt_tab2( BASE lt_tab2 ( LINES OF lt_ref2->* ) ).
  cl_demo_output=>write_data( lt_tab2 ).

  DATA(lt_tab3) = VALUE ltt_tab2(
    ( LINES OF lt_tab2  )
    ( col1 = 3 col2 = 'JK' col3 = 'JKHG' )
  ).
  cl_demo_output=>write_data( lt_tab3 ).

  "DATA lt_tab4 TYPE ltt_tab3.
  DATA(lt_tab4) = VALUE ltt_tab3( ( LINES OF lt_tab3 ) ).

  cl_demo_output=>display( lt_tab4 ).

* very old approach working with internal tables

  cl_demo_output=>write( 'Old approach' ).

  DATA:
    BEGIN OF itable1 OCCURS 0,
      f1 LIKE sy-index,
    END OF itable1.

  DATA itline1 LIKE itable1.

  DO 4 TIMES.
    itline1-f1 = sy-index.
    APPEND itline1 TO itable1.
  ENDDO.

  "itable1-f1 = -96.
  "INSERT itable1 index 2.

  cl_demo_output=>write_data( itable1[] ).

  cl_demo_output=>display( ).


* Example of using standard, sorted and hashed tables. And operator FILTER which can be used only with sorted and hashed tables


  cl_demo_output=>write( 'Example of using standard, sorted and hashed tables to extract dat by INDEX, KEY or FILTER from SFLIGHT' ).
  cl_demo_output=>write( 'Important: - You can use FILTER by where conditions against only fields from key' ).
  cl_demo_output=>write( '           - You can use FILTER only with type of table: sorted, hashed' ).

  "Standard
  DATA standard_flights TYPE STANDARD TABLE OF spfli WITH NON-UNIQUE KEY carrid.

  "Sorted
  DATA sorted_flights TYPE SORTED TABLE OF spfli
       WITH UNIQUE KEY carrid connid.

  "Hashed
  DATA hashed_flights TYPE HASHED TABLE OF spfli
       WITH UNIQUE KEY carrid connid.

  "Sorted
  DATA sorted_flt_cities TYPE SORTED TABLE OF spfli
       WITH NON-UNIQUE KEY cityfrom cityto.

  SELECT
  FROM spfli
  FIELDS *
  INTO TABLE @standard_flights.

  DATA(some_flight_std) = standard_flights[ 1 ].

  SELECT
  FROM spfli
  FIELDS *
  INTO TABLE @sorted_flights.

  DATA(some_flight_srt) = sorted_flights[ 1 ].

  SELECT
  FROM spfli
  FIELDS *
  INTO TABLE @hashed_flights.

  DATA(some_flight_htd) = hashed_flights[
    KEY primary_key
    carrid = 'LH'
    connid = '0400'
  ].

  DATA(some_flight_cond) = FILTER #( sorted_flights WHERE carrid = CONV #( 'LH' ) ).


  SELECT
  FROM spfli
  FIELDS *
  INTO TABLE @sorted_flt_cities.

  DATA(some_flight_cond2) = FILTER #( sorted_flt_cities WHERE cityfrom = CONV #( 'TOKYO' ) ).

  cl_demo_output=>write_data( some_flight_std ).
  cl_demo_output=>write_data( some_flight_srt ).
  cl_demo_output=>write_data( some_flight_htd ).

  cl_demo_output=>write( 'Examples of using FILTER. Notice: FILTER can be used only for sorted or hashed tables'  ).
  cl_demo_output=>write_data( value = some_flight_cond name = `FILTER #( sorted_flights WHERE carrid = CONV #( 'LH' ) )` ).
  cl_demo_output=>write_data( value = some_flight_cond2 name = `FILTER #( sorted_flt_cities WHERE cityfrom = CONV #( 'TOKYO' ) )` ).

  cl_demo_output=>display( ).


* using FOR - condition to filter table

  cl_demo_output=>write( 'Example of using operator FOR to filter data in itabs that taken from T100 or SFLIGHT' ).
  cl_demo_output=>write( 'Important: - You can use filter with FOR by where conditions against any field even this field is not in key' ).
  cl_demo_output=>write( '           - You can use filter with FOR with any type of table: standard, sorted, hashed' ).

  DATA messages TYPE SORTED TABLE OF t100 WITH NON-UNIQUE KEY sprsl msgnr.
  SELECT *
         FROM t100
         WHERE arbgb = 'SABAPDEMOS'
         INTO TABLE @messages.

  cl_demo_output=>write_data(
    EXPORTING
      value = messages
      name  = `itab messages from table T100 where arbg = 'SABAPDEMOS'`
  ).

  DATA value_tab LIKE messages.
  value_tab = VALUE #( FOR wa IN messages WHERE ( sprsl = 'E' ) ( wa ) ).

  cl_demo_output=>write_data(
    EXPORTING
      value = value_tab
      name  = `itab messages by filter using FOR where sprsl = 'E'`
  ).

  DATA std_flights_filtered LIKE standard_flights.
  std_flights_filtered = VALUE #( FOR flt_lines IN standard_flights WHERE ( cityfrom = 'TOKYO' ) ( flt_lines ) ).
  cl_demo_output=>write_data(
    EXPORTING
      value = std_flights_filtered
      name  = `itab standard_flights by filter using FOR where cityfrom = 'TOKYO'`
  ).

  DATA srt_flights_filtered LIKE sorted_flights.
  srt_flights_filtered = VALUE #( FOR flt_lines IN sorted_flights WHERE ( countryto = 'US' ) ( flt_lines ) ).
  cl_demo_output=>write_data(
    EXPORTING
      value = srt_flights_filtered
      name  = `itab sorted_flights by filter using FOR where countryto = 'US'`
  ).

  cl_demo_output=>display( ).

* using FOR ... IN itab

  cl_demo_output=>write( 'Example of using operator FOR ... IN itab to provide Transformation and Mapping' ).

  SELECT carrid, carrname
       FROM scarr
       INTO TABLE @DATA(scarr_tab).

  SELECT carrid, connid, cityfrom, cityto
         FROM spfli
         WHERE carrid = 'LH'
         ORDER BY carrid, connid, cityfrom, cityto
         INTO TABLE @DATA(spfli_tab).

  TYPES:
    BEGIN OF flight,
      carrier     TYPE scarr-carrname,
      number      TYPE spfli-connid,
      departure   TYPE spfli-cityfrom,
      destination TYPE spfli-cityto,
    END OF flight,
    ty_flights TYPE TABLE OF flight WITH EMPTY KEY.

  DATA(lt_flights) =
  VALUE ty_flights( FOR <fs> IN spfli_tab
    ( carrier     = VALUE #( scarr_tab[ carrid = <fs>-carrid ]
                             DEFAULT '???' )
      number      = <fs>-connid
      departure   = <fs>-cityfrom
      destination = <fs>-cityto ) ).

  cl_demo_output=>write_data( value = spfli_tab name = 'Flights of carrierid LH' ).
  cl_demo_output=>write_data( value = lt_flights name = `Flights with required field names after Transformation and mapping by FOR ...IN` ).


  SELECT
  FROM sflight
    FIELDS *
  WHERE fldate = '20210122'
  INTO TABLE @DATA(flights_per_day).

  SELECT carrid, connid, cityfrom, cityto
       FROM spfli
       ORDER BY carrid, connid, cityfrom, cityto
       INTO TABLE @DATA(spfli_tab2).

  cl_demo_output=>write_data( value = flights_per_day name = 'Flights of 2021-01-22' ).

  " example of expanded type wiht include of previously declared type.
  TYPES: BEGIN OF flight_day,
           date TYPE s_date,
           cost TYPE s_price.
      INCLUDE TYPE flight.
  TYPES:  END OF flight_day.

  TYPES:
    ty_flight_day TYPE TABLE OF flight_day WITH EMPTY KEY.


  DATA(lt_flt_per_day) =
    VALUE ty_flight_day( FOR <fsd> IN flights_per_day (
      date = <fsd>-fldate
      carrier = VALUE #( scarr_tab[ carrid = <fsd>-carrid ] DEFAULT '???' )
      number = <fsd>-connid
      departure = VALUE #( spfli_tab2[ carrid = <fsd>-carrid connid = <fsd>-connid ]-cityfrom DEFAULT '???' )
      destination = VALUE #( spfli_tab2[ carrid = <fsd>-carrid connid = <fsd>-connid ]-cityto DEFAULT '???' )
      cost = <fsd>-price

    ) ).

  cl_demo_output=>write_data( value = lt_flt_per_day name = 'Mapped values about flights and price of 2021-01-22' ).


  cl_demo_output=>display( ).

* using line_index
  cl_demo_output=>write( 'Example of using line_index' ).

  DATA:    flight_tab
        TYPE STANDARD TABLE OF spfli
        WITH EMPTY KEY
        WITH UNIQUE HASHED KEY id COMPONENTS carrid connid
        WITH NON-UNIQUE SORTED KEY cities COMPONENTS cityfrom cityto.

  SELECT *
       FROM spfli
       ORDER BY carrid, connid
       INTO TABLE @flight_tab.

  DATA idx TYPE TABLE OF i.

  idx = VALUE #(
        ( line_index( flight_tab[ carrid = 'UA'
                                  connid = '0941'
                                  ##primkey[id] ] ) )
        ( line_index( flight_tab[ KEY id
                                  carrid = 'UA'
                                  connid = '0941' ] ) )
        ( line_index( flight_tab[ KEY id
                                  carrid = 'xx'
                                  connid = 'yyyy' ] ) )
        ( line_index( flight_tab[ cityfrom = 'FRANKFURT'
                                  cityto   = 'NEW YORK'
                                  ##primkey[cities] ] ) )
        ( line_index( flight_tab[ KEY cities
                                  cityfrom = 'FRANKFURT'
                                  cityto   = 'NEW YORK'  ] ) )
        ( line_index( flight_tab[ KEY cities
                                  cityfrom = 'xxxxxxxx'
                                  cityto   = 'yyyyyyyy'  ] ) ) ).

  cl_demo_output=>write_data( idx ).

  LOOP AT idx INTO DATA(lr_idx).

    IF ( lr_idx > 0 ).
      cl_demo_output=>write_data( name = | data for index { lr_idx }: | value = flight_tab[ lr_idx ] ).
    ENDIF.
  ENDLOOP.
  .

  cl_demo_output=>display( ).

* using line_index
  cl_demo_output=>write( 'Example of using sort' ).
  DATA carriers TYPE HASHED TABLE OF scarr
            WITH UNIQUE KEY carrid.

  SELECT *
         FROM scarr
         INTO TABLE @carriers.

  SORT carriers BY currcode DESCENDING.

  cl_demo_output=>write_data( name = `SORT carriers by currcode DESCENDING. : ` value = carriers ).


  SELECT carrid, connid, cityfrom, cityto
       FROM spfli
       ORDER BY carrid, connid
       INTO TABLE @DATA(flights).

  SORT flights STABLE BY cityfrom cityto.

  cl_demo_output=>write_data( name = `SORT flights STABLE BY cityfrom cityto. : ` value = flights ).


  cl_demo_output=>display( ).
