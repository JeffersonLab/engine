      SUBROUTINE S_REPORT_BAD_DATA(lunout,ABORT,errmsg)

*--------------------------------------------------------
*
*   Purpose and Methods: Output warnings for possible hardware problems
*          in file 'bad<runnum>.txt' (unit=lunout)
*
*      NOTE: Nothing should be written to the file unless there is a warning
*             to be reported.  (i.e. check for error messages before writing
*             headers.
*
*  Required Input BANKS: 
*
*                Output: ABORT           - success or failure
*                      : err             - reason for failure, if any
* 
* author: John Arrington
* created: 8/17/95
* $Log: s_report_bad_data.f,v $
* Revision 1.3  1996/09/05 20:14:25  saw
* (JRA) Don't report difference between input pedestals and pedestals from
*       pedestal events
*
* Revision 1.2  1996/01/17 18:59:46  cdaq
* (JRA) Warn when pedestals change too much
*
* Revision 1.1  1995/08/31 20:43:52  cdaq
* Initial revision
*
*--------------------------------------------------------

      IMPLICIT NONE
*
      character*17 here
      parameter (here= 'S_REPORT_BAD_DATA')
*
      logical ABORT
      character*(*) errmsg
*
      include 'sos_data_structures.cmn'
      include 'sos_calorimeter.cmn'
      include 'sos_pedestals.cmn'
      include 'sos_cer_parms.cmn'

      integer*4 lunout
      integer*4 ind
      integer*4 icol,irow

      character*4 pln(snum_scin_planes)
      character*2 cnt(snum_scin_elements)
      character*1 sgn(2)
      character*2 col(smax_cal_columns)
      character*2 row(smax_cal_rows)
      character*5 mir(scer_num_mirrors)
      save

      data pln/'sS1X','sS1Y','sS2X','sS2Y'/
      data cnt/'01','02','03','04','05','06','07','08',
     &      '09','10','11','12','13','14','15','16'/
      data sgn/'+','-'/

      data col/'sA','sB','sC','sD'/
      data row/'01','02','03','04','05','06','07',
     &         '08','09','10','11'/

      data mir/'scer1','scer2','scer3','scer4'/

! Remove reporting of difference between pedestals and input pedestals
! from parameter files now that we always use the pedestal events.
!
* report channels where the pedestal analysis differs from the param file.
!      if ((shodo_num_ped_changes+scal_num_ped_changes+scer_num_ped_changes)
!     &     .gt. 0) then
!
!        write(lunout,*) '  SOS detectors with large (>2sigma) pedestal changes'
!        write(lunout,*)
!        write(lunout,*) ' Signal  Pedestal change(new-old)'
!
!        if (shodo_num_ped_changes.gt.0) then
!          do ind=1,shodo_num_ped_changes
!            write(lunout,'(2x,a4,a2,a1,f9.1)')
!     $           pln(shodo_changed_plane(ind))
!     $           ,cnt(shodo_changed_element(ind))
!     $           ,sgn(shodo_changed_sign(ind)),shodo_ped_change(ind)
!          enddo
!        endif
!
!        if (scal_num_ped_changes.gt.0) then
!          do ind=1,scal_num_ped_changes
!            icol=(scal_changed_block(ind)-0.5)/smax_cal_rows + 1
!            irow=scal_changed_block(ind)-smax_cal_rows*(icol-1)
!            write(lunout,'(4x,a2,a2,f9.1)') col(icol),row(irow),
!     &           scal_ped_change(ind)
!          enddo
!        endif
!
!        if (scer_num_ped_changes.gt.0) then
!          do ind=1,scer_num_ped_changes
!            write(lunout,'(3x,a4,f9.1)') mir(scer_changed_tube(ind)),
!     &           scer_ped_change(ind)
!          enddo
!        endif
!      endif              ! are there pedestal changes to report?

      return
      end
