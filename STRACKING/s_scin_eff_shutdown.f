      SUBROUTINE S_SCIN_EFF_SHUTDOWN(lunout,ABORT,errmsg)
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
* created: 2/15/95
*
* s_scin_eff calculates efficiencies for the hodoscope.
* s_scin_eff_shutdown does some final manipulation of the numbers.
*
* $Log: s_scin_eff_shutdown.f,v $
* Revision 1.9  1999/02/23 18:59:27  csa
* (JRA) Remove sdebugcalcpeds stuff
*
* Revision 1.8  1996/09/05 20:15:12  saw
* (JRA) Cosmetic
*
* Revision 1.7  1996/01/17 18:58:53  cdaq
* (JRA) Add debug control flag around write statements
*
* Revision 1.6  1995/08/31 15:08:52  cdaq
* (JRA) Dump bad counter infomation
*
* Revision 1.5  1995/07/20  19:00:54  cdaq
* (SAW) Move data statement for f2c compatibility
*
* Revision 1.4  1995/05/22  19:45:54  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.3  1995/05/17  16:44:17  cdaq
* (JRA) Write out list of potential PMT problems
*
* Revision 1.2  1995/05/11  21:17:34  cdaq
* (JRA) Add position calibration variables
*
* Revision 1.1  1995/03/13  18:18:07  cdaq
* Initial revision
*
*--------------------------------------------------------
      IMPLICIT NONE
*
      character*19 here
      parameter (here= 'S_SCIN_EFF_SHUTDOWN')
*
      logical ABORT
      character*(*) errmsg
*
      INCLUDE 'sos_data_structures.cmn'
      INCLUDE 'gen_constants.par'
      INCLUDE 'gen_units.par'
      include 'sos_scin_parms.cmn'
      include 'sos_scin_tof.cmn'
      include 'sos_statistics.cmn'
      include 'sos_tracking.cmn'

      logical written_header
      integer pln,cnt
      integer lunout
      real*4 num_real,nhits_real
      real*4 p1,p2,p3,p4         !prob. of having both tubes fire for planes1-4
      real*4 p1234,p123,p124,p134,p234 !prob. of having combos fire

      character*4 planename(SNUM_SCIN_PLANES)
      data planename/'sS1X','sS1Y','sS2X','sS2Y'/

      save

      written_header = .false.

! fill sums over counters
      do pln=1,snum_scin_planes
        sstat_trksum(pln)=0
        sstat_possum(pln)=0
        sstat_negsum(pln)=0
        sstat_andsum(pln)=0
        sstat_orsum(pln)=0
        do cnt=1,snum_scin_counters(pln)
          num_real=float(max(1,sscin_zero_num(pln,cnt)))
          sscin_zero_pave(pln,cnt)=float(sscin_zero_pos(pln,cnt))/num_real
          sscin_zero_nave(pln,cnt)=float(sscin_zero_neg(pln,cnt))/num_real
          sstat_trksum(pln)=sstat_trksum(pln)+sstat_trk(pln,cnt)
          sstat_possum(pln)=sstat_possum(pln)+sstat_poshit(pln,cnt)
          sstat_negsum(pln)=sstat_negsum(pln)+sstat_neghit(pln,cnt)
          sstat_andsum(pln)=sstat_andsum(pln)+sstat_andhit(pln,cnt)
          sstat_orsum(pln)=sstat_orsum(pln)+sstat_orhit(pln,cnt)
*
* write out list of possible problms
*
          nhits_real = max(1.,float(sstat_trk(pln,cnt)))
          sstat_neff(pln,cnt)=float(sstat_neghit(pln,cnt))/nhits_real
          sstat_peff(pln,cnt)=float(sstat_poshit(pln,cnt))/nhits_real
          sstat_oeff(pln,cnt)=float(sstat_orhit(pln,cnt))/nhits_real
          sstat_aeff(pln,cnt)=float(sstat_andhit(pln,cnt))/nhits_real
          if (nhits_real .gt. 100.) then   !dump bad counter information
            if (sstat_peff(pln,cnt).le.sstat_mineff) then
              if (.not.written_header) then
                write(lunout,*)
                write(lunout,'(a,f6.3)') ' SOS scintilators with effic. < '
     $               ,sstat_mineff
                written_header = .true.
              endif
              write(lunout,'(5x,a4,i2,a,f7.4)') planename(pln),cnt,'+',sstat_peff(pln,cnt)
            endif
            if (sstat_neff(pln,cnt).le.sstat_mineff) then
              if (.not.written_header) then
                write(lunout,*)
                write(lunout,'(a,f6.3)') ' SOS scintillators with tracking based effic. < '
     $               ,sstat_mineff
                written_header = .true.
              endif
              write(lunout,'(5x,a4,i2,a,f7.4)') planename(pln),cnt,'-',sstat_neff(pln,cnt)
            endif
          endif
        enddo
        sstat_poseff(pln)=sstat_possum(pln)/max(1.,float(sstat_trksum(pln)))
        sstat_negeff(pln)=sstat_negsum(pln)/max(1.,float(sstat_trksum(pln)))
        sstat_andeff(pln)=sstat_andsum(pln)/max(1.,float(sstat_trksum(pln)))
        sstat_oreff(pln)=sstat_orsum(pln)/max(1.,float(sstat_trksum(pln)))
      enddo

      write(lunout,*) ' '
      p1=sstat_andeff(1)
      p2=sstat_andeff(2)
      p3=sstat_andeff(3)
      p4=sstat_andeff(4)

! probability that ONLY the listed planes had triggers
      p1234= p1*p2*p3*p4
      p123 = p1*p2*p3*(1.-p4)
      p124 = p1*p2*(1.-p3)*p4
      p134 = p1*(1.-p2)*p3*p4
      p234 = (1.-p1)*p2*p3*p4

      seff_s1 = 1. - ((1.-p1)*(1.-p2))
      seff_s2 = 1. - ((1.-p3)*(1.-p4))
      seff_stof=seff_s1 * seff_s2
      seff_3_of_4=p1234+p123+p124+p134+p234
      seff_4_of_4=p1234

      return
      end
