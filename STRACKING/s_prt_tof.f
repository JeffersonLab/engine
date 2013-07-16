      subroutine s_prt_tof(itrk)

*-------------------------------------------------------------------
* author: John Arrington
* created: 3/27/94
*
* s_prt_tof dumps the sos_scin_tof bank.
*
* modifications:
* $Log: s_prt_tof.f,v $
* Revision 1.3  1995/05/22 19:45:51  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.2  1994/11/23  13:57:39  cdaq
* (SPB) Recopied from hms file and modified names for SOS
*
* Revision 1.1  1994/04/13  18:22:01  cdaq
* Initial revision
*
*-------------------------------------------------------------------

      implicit none

      include 'sos_data_structures.cmn'
      include 'sos_scin_parms.cmn'
      include 'sos_scin_tof.cmn'
      include 'sos_tracking.cmn'

      integer*4 ihit, itrk

      save

      write(sluno,'(''              ***S_SCIN_TOF BANK***'')')
      write(sluno,'(''        TRACK NUMBER'',i3)') itrk
      write(sluno,'(''POSITION/CALIBRATION VARIABLES:'')')
      write(sluno,'(''  +coord  -coord '',
     &     '' pos_dt  neg_dt  +sigma  +sigma'')')
      do ihit=1,sscin_tot_hits
        write(sluno,'(f8.3,f8.3,2f8.3,2f8.3)')
     &       sscin_pos_coord(ihit), sscin_neg_coord(ihit),
     &       sscin_neg_time_offset(ihit), sscin_pos_time_offset(ihit),
     &       sscin_pos_sigma(ihit), sscin_neg_sigma(ihit)
      enddo
      write(sluno,'(''HIT POSITION AND OTHER CALCULATED VARIABLES:'')')
      write(sluno,'(''  long_coord trans_coord    +time    -time'',
     &     '' scin_time scin_sig  on_track  time@fp'')')
      do ihit=1,sscin_tot_hits
        write(sluno,'(2f12.4,2f9.3,2f10.3,l2,f10.3)')
     &       sscin_long_coord(ihit), sscin_trans_coord(ihit),
     &       sscin_pos_time(ihit), sscin_neg_time(ihit),
     &       sscin_time(ihit),  sscin_sigma(ihit),
     &       sscin_on_track(itrk,ihit),sscin_time_fp(ihit)
      enddo
      write(sluno,'(''  trk  beta     chisq_beta  fp_time '',
     &     ''num_scin_hit'')')
      write(sluno,'(i4,f8.4,f14.3,f9.3,i8)') itrk,
     &     sbeta(itrk), sbeta_chisq(itrk), stime_at_fp(itrk),
     &     snum_scin_hit(itrk)
      write(sluno,*)

      return
      end
