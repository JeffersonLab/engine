      SUBROUTINE H_REPORT_BAD_DATA(lunout,ABORT,errmsg)

*--------------------------------------------------------
*
*   Purpose and Methods: Output warnings for possible hardware problems
*          in file 'bad<runnum>.txt' (unit=lunout)
*
*	NOTE: Nothing should be written to the file unless there is a warning
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
* $Log: h_report_bad_data.f,v $
* Revision 1.3  1996/08/30 20:34:49  saw
* (JRA) Don't report difference between input pedestals and pedestals from
*       pedestal events
*
* Revision 1.2  1996/01/16 21:56:20  cdaq
* (JRA) Warn when pedestals change too much
*
* Revision 1.1  1995/08/31 14:44:52  cdaq
* Initial revision
*
*--------------------------------------------------------

      IMPLICIT NONE
*
      character*17 here
      parameter (here= 'H_REPORT_BAD_DATA')
*
      logical ABORT
      character*(*) errmsg
*
      include 'hms_data_structures.cmn'
      include 'hms_calorimeter.cmn'
      include 'hms_pedestals.cmn'
      include 'hms_cer_parms.cmn'

      integer*4 lunout
      integer*4 ind
      integer*4 icol,irow

      character*4 pln(hnum_scin_planes)
      character*2 cnt(hnum_scin_elements)
      character*1 sgn(2)
      character*2 col(hmax_cal_columns)
      character*2 row(hmax_cal_rows)
      character*5 mir(hcer_num_mirrors)
      save

      data pln/'hS1X','hS1Y','hS2X','hS2Y'/
      data cnt/'01','02','03','04','05','06','07','08',
     &      '09','10','11','12','13','14','15','16'/
      data sgn/'+','-'/

      data col/'hA','hB','hC','hD'/
      data row/'01','02','03','04','05','06','07',
     &         '08','09','10','11','12','13'/

      data mir/'hcer1','hcer2'/

! Remove reporting of difference between pedestals and input pedestals
! from parameter files now that we always use the pedestal events.
!
* report channels where the pedestal analysis differs from the param file.
!      if ((hhodo_num_ped_changes+hcal_num_ped_changes+hcer_num_ped_changes)
!     &     .gt. 0) then
!
!        write(lunout,*) '  HMS detectors with large (>2sigma) pedestal changes'
!        write(lunout,*)
!        write(lunout,*) ' Signal  Pedestal change(new-old)'
!
!        if (hhodo_num_ped_changes.gt.0) then
!          do ind=1,hhodo_num_ped_changes
!            write(lunout,'(2x,a4,a2,a1,f9.1)')
!     $           pln(hhodo_changed_plane(ind))
!     $           ,cnt(hhodo_changed_element(ind))
!     $           ,sgn(hhodo_changed_sign(ind)),hhodo_ped_change(ind)
!          enddo
!        endif
!
!        if (hcal_num_ped_changes.gt.0) then
!          do ind=1,hcal_num_ped_changes
!            icol=(hcal_changed_block(ind)-0.5)/hmax_cal_rows + 1
!            irow=hcal_changed_block(ind)-hmax_cal_rows*(icol-1)
!            write(lunout,'(4x,a2,a2,f9.1)') col(icol),row(irow),
!     &           hcal_ped_change(ind)
!          enddo
!        endif
!
!        if (hcer_num_ped_changes.gt.0) then
!          do ind=1,hcer_num_ped_changes
!            write(lunout,'(3x,a4,f9.1)') mir(hcer_changed_tube(ind)),
!     &           hcer_ped_change(ind)
!          enddo
!        endif
!      endif              ! are there pedestal changes to report?

      return
      end
