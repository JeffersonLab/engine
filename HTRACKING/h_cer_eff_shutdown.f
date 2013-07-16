      SUBROUTINE H_CER_EFF_SHUTDOWN(lunout,ABORT,errmsg)

*--------------------------------------------------------
*
*   Purpose and Methods: Output Cerenkov efficiency information
*
*  Required Input BANKS: HMS_CER_DIAGNOSTICS
*
*                Output: ABORT           - success or failure
*                      : err             - reason for failure, if any
* 
* author: Chris Cothran
* created: 5/25/95
* $Log: h_cer_eff_shutdown.f,v $
* Revision 1.1  1995/08/31 14:54:31  cdaq
* Initial revision
*
*--------------------------------------------------------

      IMPLICIT NONE
*
      character*18 here
      parameter (here= 'H_CER_EFF_SHUTDOWN')
*
      logical ABORT
      character*(*) errmsg
*
      INCLUDE 'hms_data_structures.cmn'
      INCLUDE 'hms_cer_parms.cmn'

      integer*4 lunout
      integer*4 nr
      logical written_header

      save

      written_header = .false.    !haven't done the header yet

      do nr = 1, hcer_num_regions
        if (hcer_track_counter(nr) .gt. hcer_min_counts) then
          hcer_region_eff(nr) = float(hcer_fired_counter(nr))
     >                         /float(hcer_track_counter(nr))
        else
          hcer_region_eff(nr) = 1.0
c          write (lunout,'(A,I1,A)')
c     >      'Warning: Not enough counts for HMS Cerenkov efficiency
c     > measurement in Region #',nr,'.'
        endif
        if (hcer_region_eff(nr) .lt. hcer_min_eff) then
          if (.not.written_header) then
            write(lunout,*)
            write(lunout,'(a,f6.3)') ' HMS cerenkov regions with effic. < ',hcer_min_eff
          endif
          write (lunout,'(2x,a,i1,a,f7.4)') 'region ',nr,' has eff = ',
     &         hcer_region_eff(nr)
c          write (lunout,'(A,I1,A,F7.5,A)')
c     >      'Warning: Efficiency of HMS Cerekov Region #',nr,' is ',
c    >      hcer_region_eff(nr),'.'
        endif
      enddo

      return
      end
