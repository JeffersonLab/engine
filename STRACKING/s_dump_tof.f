      SUBROUTINE S_DUMP_TOF(ABORT,errmsg)
*--------------------------------------------------------
*-
*-   Purpose and Methods : Analyze scintillator information for each track 
*-
*-      Required Input BANKS     SOS_SCIN_TOF
*-                               GEN_DATA_STRUCTURES
*-
*-   Output: ABORT           - success or failure
*-         : err             - reason for failure, if any
*- 
* author: John Arrington
* created: 1/30/95
*
* s_dump_tof writes out the raw timing information for the final chosen tracks.
* This data is analyzed by independent routines to fit the corrections for
* pulse height walk, time lag from the hit to the pmt signal, and time offsets
* for each signal.
*
* $Log: s_dump_tof.f,v $
* Revision 1.7  1999/11/04 20:36:47  saw
* Linux/G77 compatibility fixes
*
* Revision 1.6  1999/06/10 16:57:17  csa
* (JRA) Added test on scer_npe_sum, changed output formats
*
* Revision 1.5  1999/02/10 18:20:40  csa
* Fixed format problem with ph > 10,000
*
* Revision 1.4  1995/10/09 20:20:18  cdaq
* (JRA) Subtract sstart_time from tdc output
*
* Revision 1.3  1995/05/22 19:45:36  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.2  1995/05/11  21:03:28  cdaq
* (JRA) Formatting changes
*
* Revision 1.1  1995/04/01  20:39:50  cdaq
* Initial revision
*
*--------------------------------------------------------
      IMPLICIT NONE
*
      character*50 here
      parameter (here= 'S_DUMP_TOF')
*
      logical ABORT
      character*(*) errmsg
*
      INCLUDE 'sos_data_structures.cmn'
      INCLUDE 'gen_constants.par'
      INCLUDE 'gen_units.par'
      include 'sos_scin_parms.cmn'
      include 'sos_scin_tof.cmn'
      integer*4 hit, ind
      integer*4 pmt,cnt,lay,dir
      real*4 ph,tim,betap
      save
*
*  Write out TOF fitting data.
*

*     In some circumstances you might also cut on
*     scer_npe_sum and/or ssshtrk:
      if (ssnum_pmt_hit.ge.4 .and. ssnum_pmt_hit.le.12 .and. scer_npe_sum.gt.2) then
        betap=1.
        write(38,111) ssnum_pmt_hit,ssx_fp,ssxp_fp,
     $       ssy_fp,ssyp_fp,betap
111     format(i3,1x,f10.5,1x,f8.5,1x,f10.5,1x,f8.5,1x,f7.3)
        do ind = 1, ssnum_scin_hit
          hit = sscin_hit(ssnum_fptrack,ind)
          if (sscin_tdc_pos(hit) .ge. sscin_tdc_min .and.  
     1         sscin_tdc_pos(hit) .le. sscin_tdc_max) then
            cnt=sscin_counter_num(hit)
            lay=int((sscin_plane_num(hit)+1)/2)
            dir=mod(sscin_plane_num(hit)+1,2)+1
            pmt=1
            tim=sscin_tdc_pos(hit)*sscin_tdc_to_time-sstart_time
            ph=sscin_adc_pos(hit)
            write(38,112) pmt,cnt,lay,dir,ph,tim
          endif
          if (sscin_tdc_neg(hit) .ge. sscin_tdc_min .and.  
     1         sscin_tdc_neg(hit) .le. sscin_tdc_max) then
            cnt=sscin_counter_num(hit)
            lay=int((sscin_plane_num(hit)+1)/2)
            dir=mod(sscin_plane_num(hit)+1,2)+1
            pmt=2
            tim=sscin_tdc_neg(hit)*sscin_tdc_to_time-sstart_time
            ph=sscin_adc_neg(hit)
            write(38,112) pmt,cnt,lay,dir,ph,tim
          endif
        enddo
 112    format(i2,1x,i3,2(1x,i2),1x,f7.1,1x,f8.3)
      endif
      RETURN
      END
