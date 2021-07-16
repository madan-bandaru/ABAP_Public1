class /DS1/CL_EHS_LCVDAF01_CVD1_AUTH definition
  public
  create public .

public section.

  interfaces /DS1/IF_EHS_LCVDAF01_CVD1_AUTH .
protected section.
private section.
ENDCLASS.



CLASS /DS1/CL_EHS_LCVDAF01_CVD1_AUTH IMPLEMENTATION.


METHOD /ds1/if_ehs_lcvdaf01_cvd1_auth~auth_obj.

************************************************************************
*                Conventions used in the Program                       *
************************************************************************
*	Global variables 	--> g_*                                    *
*	Local variables  	--> l_*                                    *
*	Constants        	--> c_*                                    *
*	Internal tables  	--> t_*                                    *
*	Structures       	--> x_*                                    *
*	Parameters       	--> p_*                                    *
*	Select-options   	--> s_*                                    *
*	Ranges           	--> r_*                                    *
* Workarea          --> w_*                                    *
* Field symbol      --> fs_*                                   *
************************************************************************
************************************************************************
* CHANGE HISTORY
************************************************************************
* DATE CHANGE... 14/11/2013                                            *
* AUTHOR........ Sumanth (INSLBZ)                                      *
* CHANGE DESCR.. Authorization check for CVD1 tocde for the custom     *
*                authorization object z_cvd1_byp                       *
* TRANSPORTNR... RT  68350                                             *
************************************************************************



  TYPES: BEGIN OF x_users,
         agr_name TYPE agr_name,
         uname    TYPE xubname,
        END OF x_users.

  TYPES: BEGIN OF x_1251,
         agr_name TYPE agr_name,
         object   TYPE agobject,
         auth     TYPE agauth,
         field    TYPE agrfield,
         low      TYPE agval,
         END OF x_1251.


  DATA: t_users TYPE STANDARD TABLE OF x_users,
        t_1251  TYPE STANDARD TABLE OF x_1251,
        w_users TYPE x_users,
        w_1251  TYPE x_1251,
        w_tab   TYPE tcgsgp,
        lv_titlebar TYPE string,
        g_object TYPE agobject,
        g_initiator TYPE char9,
        g_object_vbak TYPE char10,
        l_answer TYPE c.

  CONSTANTS:lc_pronam TYPE programm VALUE '/DS1/CL_EHS_LCVDAF01_CVD1_AUTH',
            lc_proind TYPE /ds1/buss_ind1 VALUE 'LSCE369'.



  get-prog-var  lc_pronam  lc_proind.

  AUTHORITY-CHECK OBJECT g_object
                           ID g_initiator FIELD 'Y'
                           ID 'ACTVT'     FIELD '03'.
  IF sy-subrc NE 0.
    READ TABLE it_exit_param_tab[] INTO w_tab WITH KEY fieldname = g_initiator.
    IF sy-subrc = 0.
      AUTHORITY-CHECK OBJECT g_object_vbak
                    ID 'VKORG' FIELD  w_tab-value
                    ID 'ACTVT' FIELD '02'.

      IF sy-subrc NE 0.

*        CONCATENATE 'You are not authorised for Sales Organisation' w_tab-value INTO lv_titlebar SEPARATED BY space.
*
*        CALL FUNCTION 'POPUP_TO_CONFIRM'
*          EXPORTING
*            titlebar       = 'Authorization check'
*            text_question  = lv_titlebar
*          EXCEPTIONS
*            text_not_found = 1
*            OTHERS         = 2.


        CALL FUNCTION 'POPUP_TO_INFORM'
          EXPORTING
            TITEL = 'Authorization check'
            TXT1  = 'You are not authorised for Sales Organisation'
            TXT2  = w_tab-value.

        IF sy-subrc = 0.

          CALL TRANSACTION 'CVD1'.
        ENDIF.
      ENDIF.




    ELSE.
*      CALL FUNCTION 'POPUP_TO_CONFIRM'
*        EXPORTING
*          titlebar              = 'Initiator Validation'
*          text_question         = 'Initiator is Mandatory, please maintain in the search criteria for Shipping order with header data'
*          text_button_1         = 'OK'
*          text_button_2         = ''
*          ICON_BUTTON_2         = ''
*          display_cancel_button = ''
*        IMPORTING
*          answer                = l_answer
*        EXCEPTIONS
*          text_not_found        = 1
*          OTHERS                = 2.
*
*      IF l_answer = '2'.
*
*        CALL TRANSACTION 'CVD1'.
*
*      ENDIF.

      CALL FUNCTION 'POPUP_TO_INFORM'
        EXPORTING
          TITEL = 'Initiator Validation'
          TXT1  = 'Initiator is Mandatory,'
          TXT2  = 'please maintain in the search criteria for Shipping order with header data'.

      IF sy-subrc = 0.

        CALL TRANSACTION 'CVD1'.
      ENDIF.


    ENDIF.
  ENDIF.





**Fetching the role names for the user
*  SELECT agr_name
*         uname
*     FROM agr_users
*     INTO TABLE t_users
*     WHERE uname = sy-uname
*     AND  from_dat LE sy-datum
*     AND  to_dat GE sy-datum.
*
*
*  IF t_users IS NOT INITIAL.
*
** Fetching the data based on the role names assigned to the user for authorization object 'Z_CVD1_BYP'
*    SELECT agr_name
*           object
*           auth
*           field
*           low
*      FROM agr_1251
*      INTO TABLE t_1251
*      FOR ALL ENTRIES IN t_users
*      WHERE agr_name = t_users-agr_name
*      AND   object =  g_object.
*
*
*
*    IF sy-subrc EQ 0.
*
*
*      LOOP AT t_1251 INTO w_1251.
*
*        AUTHORITY-CHECK OBJECT g_object
*                         ID g_initiator FIELD w_1251-low
*                         ID 'ACTVT'     FIELD '03'.
*
*        IF  sy-subrc EQ 0.
*          READ TABLE exit_param_tab INTO w_tab WITH KEY fieldname = g_initiator.
*          IF sy-subrc = 0.
*
*            AUTHORITY-CHECK OBJECT g_object_vbak
*                          ID 'VKORG' FIELD  w_tab-value
*                          ID 'ACTVT' FIELD '02'.
*
*            IF sy-subrc NE 0.
*
*              CONCATENATE 'You are not authorised for Sales Organisation' w_tab-value INTO lv_titlebar SEPARATED BY space.
*
*              CALL FUNCTION 'POPUP_TO_CONFIRM'
*                EXPORTING
*                  TITLEBAR       = lv_titlebar
*                  TEXT_QUESTION  = 'Are you sure ? Want to exit ? '
*                EXCEPTIONS
*                  TEXT_NOT_FOUND = 1
*                  OTHERS         = 2.
*            ENDIF.
*
*          ELSE.
*            CALL FUNCTION 'POPUP_TO_CONFIRM'
*              EXPORTING
*                TITLEBAR       = 'Initiator is Mandatory, please maintain in the search criteria for Shipping order with header data'
*                TEXT_QUESTION  = 'Are you sure ? Want to exit ? '
*              EXCEPTIONS
*                TEXT_NOT_FOUND = 1
*                OTHERS         = 2.
*
*          ENDIF.
*        ENDIF.
*      ENDLOOP.
*
*
*
*    ENDIF.
*  ENDIF.

ENDMETHOD.
ENDCLASS.
