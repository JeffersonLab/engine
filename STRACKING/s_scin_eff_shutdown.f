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
* $Log$
* Revision 1.3  1995/05/17 16:44:17  cdaq
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
      character*50 here
      parameter (here= 'S_SCIN_EFF')
*
      logical ABORT
      character*(*) errmsg
*
      INCLUDE 'gen_data_structures.cmn'
      INCLUDE 'gen_constants.par'
      INCLUDE 'gen_units.par'
      include 'sos_scin_parms.cmn'
      include 'sos_scin_tof.cmn'
      include 'sos_statistics.cmn'

      integer pln,cnt
      real ave,ave2,num        !intermediate variables for sigma(position)
      real p1,p2,p3,p4         !prob. of having both tubes fire for planes1-4
      real p1234,p123,p124,p134,p234 !prob. of having combos fire
      integer lunout
      character*4 planename(SNUM_SCIN_PLANES)
      data planename/'sS1X','sS1Y','sS2X','sS2Y'/
      real*4 peff,neff
      real*4 mineff
      parameter (mineff=.95)
      save
      write(lunout,*)
      write(lunout,*) ' scintilators with effic. < ',mineff

! fill sums over counters
      do pln=1,snum_scin_planes
        sstat_trksum(pln)=0
        sstat_possum(pln)=0
        sstat_negsum(pln)=0
        sstat_andsum(pln)=0
        sstat_orsum(pln)=0
        sscin_tot_dpos_sum(pln)=0.
        sscin_tot_num_dpos(pln)=0
        do cnt=1,snum_scin_counters(pln)
          sstat_trksum(pln)=sstat_trksum(pln)+sstat_trk(pln,cnt)
          sstat_possum(pln)=sstat_possum(pln)+sstat_poshit(pln,cnt)
          sstat_negsum(pln)=sstat_negsum(pln)+sstat_neghit(pln,cnt)
          sstat_andsum(pln)=sstat_andsum(pln)+sstat_andhit(pln,cnt)
          sstat_orsum(pln)=sstat_orsum(pln)+sstat_orhit(pln,cnt)

          num=float(max(1,sscin_num_dpos(pln,cnt)))
          ave=sscin_dpos_sum(pln,cnt)/num
          sscin_dpos_ave(pln,cnt)=ave
          ave2=sscin_dpos_sum2(pln,cnt)/num
          sscin_dpos_sig(pln,cnt)=sqrt(max(0.,(ave2/num)-ave*ave))
          sscin_tot_dpos_sum(pln)=
     &            sscin_tot_dpos_sum(pln)+sscin_dpos_sum(pln,cnt)
          sscin_tot_num_dpos(pln)=
     &            sscin_tot_num_dpos(pln)+sscin_num_dpos(pln,cnt)
*
* write out list of possible problms
*
          if (sstat_trk(pln,cnt).ge.50) then
            peff=float(sstat_poshit(pln,cnt))/float(sstat_trk(pln,cnt))
           if (peff.le.mineff) then
              write(lunout,'(5x,a4,i2,a,f7.4)') planename(pln),cnt,'+',peff
            endif
            neff=float(sstat_neghit(pln,cnt))/float(sstat_trk(pln,cnt))
           if (neff.le.mineff) then
              write(lunout,'(5x,a4,i2,a,f7.4)') planename(pln),cnt,'-',neff
            endif
          endif
        enddo
        sstat_poseff(pln)=sstat_possum(pln)/max(1.,float(sstat_trksum(pln)))
        sstat_negeff(pln)=sstat_negsum(pln)/max(1.,float(sstat_trksum(pln)))
        sstat_andeff(pln)=sstat_andsum(pln)/max(1.,float(sstat_trksum(pln)))
        sstat_oreff(pln)=sstat_orsum(pln)/max(1.,float(sstat_trksum(pln)))
        sscin_tot_dpos_ave(pln)=
     &        sscin_tot_dpos_sum(pln)/max(1.,float(sscin_tot_num_dpos(pln)))
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
