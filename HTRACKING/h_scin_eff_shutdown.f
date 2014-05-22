      SUBROUTINE H_SCIN_EFF_SHUTDOWN(lunout,ABORT,errmsg)
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
* created: 2/15/95
*
* h_scin_eff calculates efficiencies for the hodoscope.
* h_scin_eff_shutdown does some final manipulation of the numbers.
*
* $Log: h_scin_eff_shutdown.f,v $
* Revision 1.9  1999/02/23 18:40:48  csa
* (JRA) Remove hdebugcalcpeds stuff
*
* Revision 1.8  1996/08/30 20:35:14  saw
* (JRA) Cosmetic
*
* Revision 1.7  1996/01/16 21:57:27  cdaq
* (JRA) Add debug control flag around write statements
*
* Revision 1.6  1995/08/30 18:14:15  cdaq
* (JRA) Dump bad counter infomation
*
* Revision 1.5  1995/07/19  19:04:02  cdaq
* (SAW) Move data statement for f2c compatibility
*
* Revision 1.4  1995/05/22  19:39:26  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.3  1995/05/17  13:58:29  cdaq
* (JRA) Write out list of potential PMT problems
*
* Revision 1.2  1995/05/11  20:27:13  cdaq
* (JRA) Add position calibration variables
*
* Revision 1.1  1995/02/23  13:31:36  cdaq
* Initial revision
*
*--------------------------------------------------------
      IMPLICIT NONE
*
      character*19 here
      parameter (here= 'H_SCIN_EFF_SHUTDOWN')
*
      logical ABORT
      character*(*) errmsg
*
      INCLUDE 'hms_data_structures.cmn'
      INCLUDE 'gen_constants.par'
      INCLUDE 'gen_units.par'
      include 'hms_scin_parms.cmn'
      include 'hms_scin_tof.cmn'
      include 'hms_statistics.cmn'
      include 'hms_tracking.cmn'

      logical written_header
      integer pln,cnt
      integer lunout
      real*4 num_real,nhits_real
      real*4 p1,p2,p3,p4         !prob. of having both tubes fire for planes1-4
      real*4 p1234,p123,p124,p134,p234 !prob. of having combos fire

      character*4 planename(HNUM_SCIN_PLANES)
      data planename/'hS1X','hS1Y','hS2X','hS2Y'/

      save

      written_header = .false.

! fill sums over counters
      do pln=1,hnum_scin_planes
        hstat_trksum(pln)=0
        hstat_possum(pln)=0
        hstat_negsum(pln)=0
        hstat_andsum(pln)=0
        hstat_orsum(pln)=0
        do cnt=1,hnum_scin_counters(pln)
          num_real=float(max(1,hscin_zero_num(pln,cnt)))
          hscin_zero_pave(pln,cnt)=float(hscin_zero_pos(pln,cnt))/num_real
          hscin_zero_nave(pln,cnt)=float(hscin_zero_neg(pln,cnt))/num_real
          hstat_trksum(pln)=hstat_trksum(pln)+hstat_trk(pln,cnt)
          hstat_possum(pln)=hstat_possum(pln)+hstat_poshit(pln,cnt)
          hstat_negsum(pln)=hstat_negsum(pln)+hstat_neghit(pln,cnt)
          hstat_andsum(pln)=hstat_andsum(pln)+hstat_andhit(pln,cnt)
          hstat_orsum(pln)=hstat_orsum(pln)+hstat_orhit(pln,cnt)
*
* write out list of possible problms
*
          nhits_real = max(1.,float(hstat_trk(pln,cnt)))
          hstat_peff(pln,cnt)=float(hstat_poshit(pln,cnt))/nhits_real
          hstat_neff(pln,cnt)=float(hstat_neghit(pln,cnt))/nhits_real
          hstat_oeff(pln,cnt)=float(hstat_orhit(pln,cnt))/nhits_real
          hstat_aeff(pln,cnt)=float(hstat_andhit(pln,cnt))/nhits_real
          if (nhits_real .gt. 100.) then   !dump bad counter information
            if (hstat_peff(pln,cnt).le.hstat_mineff) then
            if (.not.written_header) then
                write(lunout,*)
                write(lunout,'(a,f6.3)') ' HMS scintillators with tracking based effic. < '
     $               ,hstat_mineff
                written_header = .true.
              endif
              write(lunout,'(5x,a4,i2,a,f7.4)') planename(pln),cnt,'+',hstat_peff(pln,cnt)
            endif
            if (hstat_neff(pln,cnt).le.hstat_mineff) then
              if (.not.written_header) then
                write(lunout,*)
                write(lunout,'(a,f6.3)') ' HMS scintilators with effic. < '
     $               ,hstat_mineff
                written_header = .true.
              endif
              write(lunout,'(5x,a4,i2,a,f7.4)') planename(pln),cnt,'-',hstat_neff(pln,cnt)
            endif
          endif
        enddo
        hstat_poseff(pln)=hstat_possum(pln)/max(1.,float(hstat_trksum(pln)))
        hstat_negeff(pln)=hstat_negsum(pln)/max(1.,float(hstat_trksum(pln)))
        hstat_andeff(pln)=hstat_andsum(pln)/max(1.,float(hstat_trksum(pln)))
        hstat_oreff(pln)=hstat_orsum(pln)/max(1.,float(hstat_trksum(pln)))
      enddo

      write(lunout,*) ' '

      p1=hstat_andeff(1)
      p2=hstat_andeff(2)
      p3=hstat_andeff(3)
      p4=hstat_andeff(4)

! probability that ONLY the listed planes had triggers
      p1234= p1*p2*p3*p4
      p123 = p1*p2*p3*(1.-p4)
      p124 = p1*p2*(1.-p3)*p4
      p134 = p1*(1.-p2)*p3*p4
      p234 = (1.-p1)*p2*p3*p4

      heff_s1 = 1. - ((1.-p1)*(1.-p2))
      heff_s2 = 1. - ((1.-p3)*(1.-p4))
      heff_stof=heff_s1 * heff_s2
      heff_3_of_4=p1234+p123+p124+p134+p234
      heff_4_of_4=p1234

      return
      end
