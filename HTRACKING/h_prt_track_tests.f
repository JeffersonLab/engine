        subroutine h_prt_track_tests

*-------------------------------------------------------------------
* author: John Arrington
* created: 3/28/94
*
* h_prt_track_tests dumps the hms_track_tests bank.
*
* modifications:
* $Log: h_prt_track_tests.f,v $
* Revision 1.2  1995/05/22 19:39:24  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.1  1994/04/13  15:43:55  cdaq
* Initial revision
*
*-------------------------------------------------------------------

        implicit none

        include 'hms_data_structures.cmn'
        include 'hms_scin_parms.cmn'
        include 'hms_scin_tof.cmn'
        include 'hms_tracking.cmn'

        logical abort
        integer*4 ihit, itrk
        character*1024 errmsg
        character*25 here
        parameter (here = 'h_prt_track_tests')

        save

       if(hntracks_fp.gt.0) then
        write(hluno,'(''        h_TRACK_TESTS BANK'')')
        write(hluno,'(''SHOWER COUNTER TESTS'')')
        write(hluno,'(''  num_blks   plane1   plane2   plane3   plane4'', 
     &        ''    shtrk    prtrk'')')
        do itrk=1, hntracks_fp
          write(hluno,'(i10,6f9.3)') hnblocks_cal(itrk), 
     &          htrack_e1(itrk), htrack_e2(itrk),
     &          htrack_e3(itrk), htrack_e4(itrk),
     &          htrack_et(itrk), htrack_preshower_e(itrk)
        enddo
        write(hluno,'(''SCIN/CERENKOV TESTS'')')
        write(hluno,'(''  trk   beta  chisq_beta  fp_time  '',
     &        ''num_scin_hit'')')
        do itrk=1, hntracks_fp
          write(hluno,'(i4,f8.4,f10.4,f9.3,i12)') itrk,
     &          hbeta(itrk), hbeta_chisq(itrk), htime_at_fp(itrk),
     &          hnum_scin_hit(itrk)
        enddo

        do itrk=1, hntracks_fp
          write(hluno,'(''hits on track number'',i3,'', and dE/dx:'')') itrk
          write(hluno,'(16i6)') 
     &       (hscin_hit(itrk,ihit),ihit=1,hnum_scin_hit(itrk))
          write(hluno,'(16f6.1)') 
     &       (hdedx(itrk,ihit),ihit=1,hnum_scin_hit(itrk))
        enddo
       endif         ! end check on zero focal plane tracks
        return
        end
