      SUBROUTINE H_DUMP_TOF(ABORT,errmsg)
*--------------------------------------------------------
*-
*-   Purpose and Methods : Analyze scintillator information for each track 
*-
*-      Required Input BANKS     HMS_SCIN_TOF
*-                               GEN_DATA_STRUCTURES
*-
*-   Output: ABORT           - success or failure
*-         : err             - reason for failure, if any
*- 
* author: John Arrington
* created: 1/30/95
*
* h_dump_tof writes out the raw timing information for the final chosen tracks.
* This data is analyzed by independent routines to fit the corrections for
* pulse height walk, time lag from the hit to the pmt signal, and time offsets
* for each signal.
*
* $Log$
* Revision 1.4  1995/10/09 20:20:00  cdaq
* (JRA) Subtract hstart_time from tdc output
*
* Revision 1.3  1995/05/22 19:39:09  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.2  1995/05/11  19:03:50  cdaq
* (JRA) Formatting changes
*
* Revision 1.1  1995/01/31  21:34:03  cdaq
* Initial revision
*
*--------------------------------------------------------
      IMPLICIT NONE
*
      character*50 here
      parameter (here= 'H_DUMP_TOF')
*
      logical ABORT
      character*(*) errmsg
*
      INCLUDE 'hms_data_structures.cmn'
      INCLUDE 'gen_constants.par'
      INCLUDE 'gen_units.par'
      include 'hms_scin_parms.cmn'
      include 'hms_scin_tof.cmn'
      integer*4 hit, ind
      integer*4 pmt,cnt,lay,dir
      real*4 ph,tim,betap
      save
*
*  Write out TOF fitting data.
*
      if (hsnum_pmt_hit.ge.4 .and. hsnum_pmt_hit.le.12) then
        betap=1.
        write(37,111) hsnum_pmt_hit,hsx_fp,hsxp_fp,
     $       hsy_fp,hsyp_fp,betap
 111    format(i3,f10.5,f8.5,f10.5,f8.5,f7.3)
* 111    format(i4,5f10.5)
        do ind = 1, hsnum_scin_hit
          hit = hscin_hit(hsnum_fptrack,ind)
          if (hscin_tdc_pos(hit) .ge. hscin_tdc_min .and.  
     1         hscin_tdc_pos(hit) .le. hscin_tdc_max) then
            cnt=hscin_counter_num(hit)
            lay=int((hscin_plane_num(hit)+1)/2)
            dir=mod(hscin_plane_num(hit)+1,2)+1
            pmt=1
            tim=hscin_tdc_pos(hit)*hscin_tdc_to_time-hstart_time
            ph=hscin_adc_pos(hit)
            write(37,112) pmt,cnt,lay,dir,ph,tim
          endif
          if (hscin_tdc_neg(hit) .ge. hscin_tdc_min .and.  
     1         hscin_tdc_neg(hit) .le. hscin_tdc_max) then
            cnt=hscin_counter_num(hit)
            lay=int((hscin_plane_num(hit)+1)/2)
            dir=mod(hscin_plane_num(hit)+1,2)+1
            pmt=2
            tim=hscin_tdc_neg(hit)*hscin_tdc_to_time-hstart_time
            ph=hscin_adc_neg(hit)
            write(37,112) pmt,cnt,lay,dir,ph,tim
          endif
        enddo
 112    format(i2,i3,i2,i2,f7.1,f8.3)
      endif
      RETURN
      END
