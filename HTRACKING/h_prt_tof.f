        subroutine h_prt_tof(itrk)

*-------------------------------------------------------------------
* author: John Arrington
* created: 3/27/94
*
* h_prt_tof dumps the hms_scin_tof bank.
*
* modifications:
* $Log$
* Revision 1.1  1994/04/13 15:43:38  cdaq
* Initial revision
*
*-------------------------------------------------------------------

        implicit none

        include 'gen_data_structures.cmn'
        include 'hms_scin_parms.cmn'
        include 'hms_scin_tof.cmn'
        include 'hms_tracking.cmn'

        logical abort
        integer*4 ihit, itrk
        character*1024 errmsg
        character*25 here
        parameter (here = 'h_prt_tof')

        save

        write(hluno,'(''        h_SCIN_TOF BANK'')')
        write(hluno,'(''        TRACK NUMBER'',i3)') itrk
        write(hluno,'(''POSITION/CALIBRATION VARIABLES:'')')
        write(hluno,'(''  +coord  -coord vel_light +phc_coef'',
     &        '' -phc_coef  pos_dt  neg_dt  +sigma  +sigma'')')
        do ihit=1,hscin_tot_hits
          write(hluno,'(f8.3,f8.3,f10.4,2f10.4,2f8.3,2f8.3)')
     &          hscin_pos_coord(ihit), hscin_neg_coord(ihit),
     &          hscin_vel_light(ihit), 
     &          hscin_pos_phc_coeff(ihit), hscin_neg_phc_coeff(ihit),
     &          hscin_neg_time_offset(ihit), hscin_pos_time_offset(ihit)
        enddo
        write(hluno,'(''HIT POSITION AND OTHER CALCULATED VARIABLES:'')')
        write(hluno,'(''  long_coord trans_coord    +time    -time'',
     &        '' scin_time scin_sig'')')
        do ihit=1,hscin_tot_hits
          write(hluno,'(2f12.4,2f9.3,2f10.3)')
     &          hscin_long_coord(ihit), hscin_trans_coord(ihit),
     &          hscin_pos_time(ihit), hscin_neg_time(ihit),
     &          hscin_time(ihit),  hscin_sigma(ihit)
        enddo

        return
        end
