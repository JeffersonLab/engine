        subroutine s_prt_tof(itrk)

*-------------------------------------------------------------------
* author: John Arrington
* created: 3/27/94
*
* s_prt_tof dumps the sos_scin_tof bank.
*
* modifications:
* $Log$
* Revision 1.1  1994/04/13 18:22:01  cdaq
* Initial revision
*
*-------------------------------------------------------------------

        implicit none

        include 'gen_data_structures.cmn'
        include 'sos_scin_parms.cmn'
        include 'sos_scin_tof.cmn'
        include 'sos_tracking.cmn'

        logical abort
        integer*4 ihit, itrk
        character*1024 errmsg
        character*25 here
        parameter (here = 's_prt_tof')

        save

        write(sluno,'(''        H_SCIN_TOF BANK'')')
        write(sluno,'(''        TRACK NUMBER'',i3)') itrk
        write(sluno,'(''POSITION/CALIBRATION VARIABLES:'')')
        write(sluno,'(''  +coord  -coord vel_light +phc_coef'',
     &        '' -phc_coef  pos_dt  neg_dt  +sigma  +sigma'')')
        do ihit=1,sscin_tot_hits
          write(sluno,'(f8.3,f8.3,f10.4,2f10.4,2f8.3,2f8.3)')
     &          sscin_pos_coord(ihit), sscin_neg_coord(ihit),
     &          sscin_vel_light(ihit), 
     &          sscin_pos_phc_coeff(ihit), sscin_neg_phc_coeff(ihit),
     &          sscin_neg_time_offset(ihit), sscin_pos_time_offset(ihit)
        enddo
        write(sluno,'(''HIT POSITION AND OTHER CALCULATED VARIABLES:'')')
        write(sluno,'(''  long_coord trans_coord    +time    -time'',
     &        '' scin_time scin_sig'')')
        do ihit=1,sscin_tot_hits
          write(sluno,'(2f12.4,2f9.3,2f10.3)')
     &          sscin_long_coord(ihit), sscin_trans_coord(ihit),
     &          sscin_pos_time(ihit), sscin_neg_time(ihit),
     &          sscin_time(ihit),  sscin_sigma(ihit)
        enddo

        return
        end
