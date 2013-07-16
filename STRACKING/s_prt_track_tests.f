        subroutine s_prt_track_tests

*-------------------------------------------------------------------
* author: John Arrington
* created: 3/28/94
*
* s_prt_track_tests dumps the sos_track_tests bank.
*
* modifications:
* $Log: s_prt_track_tests.f,v $
* Revision 1.2  1995/05/22 19:45:52  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.1  1994/04/13  18:22:19  cdaq
* Initial revision
*
*-------------------------------------------------------------------

        implicit none

        include 'sos_data_structures.cmn'
        include 'sos_scin_parms.cmn'
        include 'sos_scin_tof.cmn'
        include 'sos_tracking.cmn'

        logical abort
        integer*4 ihit, itrk
        character*1024 errmsg
        character*25 here
        parameter (here = 's_prt_track_tests')

        save

       if(sntracks_fp.gt.0) then
        write(sluno,'(''        SOS_TRACK_TESTS BANK'')')
        write(sluno,'(''SHOWER COUNTER TESTS'')')
        write(sluno,'(''  num_blks   plane1   plane2   plane3   plane4'', 
     &        ''    shtrk    prtrk'')')
        do itrk=1, sntracks_fp
          write(sluno,'(i10,6f9.3)') snblocks_cal(itrk), 
     &          strack_e1(itrk), strack_e2(itrk),
     &          strack_e3(itrk), strack_e4(itrk),
     &          strack_et(itrk), strack_preshower_e(itrk)
        enddo
        write(sluno,'(''SCIN/CERENKOV TESTS'')')
        write(sluno,'(''  trk   beta  chisq_beta  fp_time  '',
     &        ''num_scin_hit'')')
        do itrk=1, sntracks_fp
          write(sluno,'(i4,f8.4,f10.4,f9.3,i12)') itrk,
     &          sbeta(itrk), sbeta_chisq(itrk), stime_at_fp(itrk),
     &          snum_scin_hit(itrk)
        enddo

        do itrk=1, sntracks_fp
          write(sluno,'(''hits on track number'',i3,'', and dE/dx:'')') itrk
          write(sluno,'(16i6)') 
     &       (sscin_hit(itrk,ihit),ihit=1,snum_scin_hit(itrk))
          write(sluno,'(16f6.1)') 
     &       (sdedx(itrk,ihit),ihit=1,snum_scin_hit(itrk))
        enddo
       endif         ! end check on zero focal plane tracks
        return
        end
