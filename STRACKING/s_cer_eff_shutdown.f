      SUBROUTINE S_CER_EFF_SHUTDOWN(lunout,ABORT,errmsg)

*--------------------------------------------------------
*
*   Purpose and Methods: Output Cerenkov efficiency information
*
*  Required Input BANKS: SOS_CER_DIAGNOSTICS
*
*                Output: ABORT           - success or failure
*                      : err             - reason for failure, if any
* 
* author: Chris Cothran
* created: 5/25/95
* $Log: s_cer_eff_shutdown.f,v $
* Revision 1.1  1995/08/31 15:04:56  cdaq
* Initial revision
*
*--------------------------------------------------------

      IMPLICIT NONE
*
      character*18 here
      parameter (here= 'S_CER_EFF_SHUTDOWN')
*
      logical ABORT
      character*(*) errmsg
*
      INCLUDE 'sos_data_structures.cmn'
      INCLUDE 'sos_cer_parms.cmn'

      integer*4 lunout
      integer*4 nr
      logical written_header

      save

      written_header = .false.    !haven't done the header yet

      do nr = 1, scer_num_regions
        if (scer_track_counter(nr) .gt. scer_min_counts) then
          scer_region_eff(nr) = float(scer_fired_counter(nr))
     >                         /float(scer_track_counter(nr))
        else
          scer_region_eff(nr) = 1.0
c          write (lunout,'(A,I1,A)')
c     >      'Warning: Not enough counts for SOS Cerenkov efficiency
c     > measurement in Region #',nr,'.'
        endif
        if (scer_region_eff(nr) .lt. scer_min_eff) then
          if (.not.written_header) then
            write(lunout,*)
            write(lunout,'(a,f6.3)') ' SOS cerenkov regions with effic. < ',scer_min_eff
          endif
          write (lunout,'(2x,a,i1,a,f7.4)') 'region ',nr,' has eff = ',
     &         scer_region_eff(nr)
c          write (lunout,'(A,I1,A,F7.5,A)')
c     >      'Warning: Efficiency of SOS Cerekov Region #',nr,' is ',
c    >      scer_region_eff(nr),'.'
        endif
      enddo

      return
      end
