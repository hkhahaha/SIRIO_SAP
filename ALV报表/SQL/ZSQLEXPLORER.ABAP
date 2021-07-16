************************************************************************
*^ Report                 ZSQLEXPLORER
*^ Written By           : Huxy
*^ Date Written         : 2019/08/06
*^ Program Purpose      : Provide The Service That User Key Open SQL
*^                        Into The SQL Editor And Execute it , To Get Data What
*^                        You Wants
*^ Run Frequency        : as needed
*^ Transaction Codes    : ZSQL
*^ Input File Names     :
*^ Output File Names    :
*^ Reports Generated    :
*^ Related Processes    :
*^ Others               :
************************************************************************
*^ Maintenance History (latest on top)
*
*^ Code Tag :              Date:               Author:
*^ Transport:
*^ Description of Change:
*
*^ Code Tag :              Date:               Author:
*^ Transport:
*^ Description of Change:
*
*
************************************************************************
REPORT zsqlexplorer .

INCLUDE zsqlexplorertop.

*INCLUDE zsqlexplorerf01.
INCLUDE zsqlexplorerf02.
INCLUDE zsqlexplorerf03.
INCLUDE zsqlexplorerf04.
INCLUDE zsqlexplorerf05.
INCLUDE zsqlexplorerf06.
INCLUDE zsqlexplorerf07.
*INCLUDE zsqlexplorerf08.
INCLUDE zsqlexplorero01.
INCLUDE zsqlexplorero02.
INCLUDE zsqlexploreri01.
INCLUDE zsqlexploreri02.

start-OF-selection.
CALL SCREEN c_100.