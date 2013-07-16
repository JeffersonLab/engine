      SUBROUTINE s_prt_raw_scin(ABORT,err)
*--------------------------------------------------------
*-
*-   Purpose and Methods : Dump SOS_RAW_SCIN BANKS
*-
*-      Required Input BANKS     SOS_RAW_SCIN
*-
*-   Output: ABORT           - success or failure
*-         : err             - reason for failure, if any
*- 
*-   Created 29-FEB-1994   D. F. Geesaman
* $Log: s_prt_raw_scin.f,v $
* Revision 1.6  2003/09/05 19:58:29  jones
* Merge in online03 changes (mkj)
*
* Revision 1.5.2.1  2003/04/10 12:42:26  cdaq
* Print out additional info on raw scint
*
* Revision 1.5  1995/07/20 18:59:35  cdaq
* (SAW) Fix format
*
* Revision 1.4  1995/05/22  19:45:50  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.3  1995/04/06  19:42:03  cdaq
* (JRA) SSCIN_TOT_HITS -> SSCIN_ALL_TOT_HITS
*
* Revision 1.2  1994/11/23  13:56:57  cdaq
* (SPB) Recopied from hms file and modified names for SOS
*
* Revision 1.1  1994/04/13  18:21:45  cdaq
* Initial revision
*
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*
      character*50 here
      parameter (here= 's_prt_raw_scin')
*
      logical ABORT
      character*(*) err
*
      integer*4 j
      include 'sos_data_structures.cmn'
      include 'gen_constants.par'
      include 'gen_units.par'
      include 'sos_tracking.cmn'
      include 'sos_scin_parms.cmn'
*
*--------------------------------------------------------
      ABORT = .FALSE.
      err = ' '
      write(sluno,'(''        SOS_RAW_SCIN BANKS'')')
      write(sluno,'(''     SSCIN_ALL_TOT_HITS='',I4)') SSCIN_ALL_TOT_HITS
      if(SSCIN_ALL_TOT_HITS.GT.0) then
        write(sluno,'('' Num  Plane    Counter      ADC_POS  ''
     &       '' ADC_NEG  TDC_POS  TDC_NEG'')')
        write(sluno,'(1x,i2,2x,i3,7x,i4,8x,2f8.1,2i8)')
     &       (j,SSCIN_ALL_PLANE_NUM(j),SSCIN_ALL_COUNTER_NUM(j),
     &       (SSCIN_ALL_ADC_POS(j)
     $       -SSCIN_ALL_PED_POS(sscin_all_plane_num(j)
     $       ,sscin_all_counter_num(j)))
     $       ,(SSCIN_ALL_ADC_NEG(j)
     $       -SSCIN_ALL_PED_NEG(sscin_all_plane_num(j)
     $       ,sscin_all_counter_num(j)))
     $       ,SSCIN_ALL_TDC_POS(j)
     $       ,SSCIN_ALL_TDC_NEG(j),j=1,SSCIN_ALL_TOT_HITS )

        write(sluno,'('' Num  Plane    Counter    RAW_ADC_+ ''
     &       ''RAW_ADC_-  PED_POS  PED_NEG'')')
        write(sluno,'(3i5,4f10.2)')
     &       (j,SSCIN_ALL_PLANE_NUM(j),SSCIN_ALL_COUNTER_NUM(j),
     &       float(SSCIN_ALL_ADC_POS(j)),
     $       float(SSCIN_ALL_ADC_NEG(j)),
     $       SSCIN_ALL_PED_POS(sscin_all_plane_num(j),sscin_all_counter_num(j)),
     $       SSCIN_ALL_PED_NEG(sscin_all_plane_num(j),sscin_all_counter_num(j)),
     $       j=1,SSCIN_ALL_TOT_HITS )

      endif
      RETURN
      END
