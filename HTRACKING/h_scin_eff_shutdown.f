      SUBROUTINE H_SCIN_EFF_SHUTDOWN(ABORT,errmsg)
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
* $Log$
* Revision 1.2  1995/05/11 20:27:13  cdaq
* (JRA) Add position calibration variables
*
* Revision 1.1  1995/02/23  13:31:36  cdaq
* Initial revision
*
*--------------------------------------------------------
      IMPLICIT NONE
*
      character*50 here
      parameter (here= 'H_SCIN_EFF')
*
      logical ABORT
      character*(*) errmsg
*
      INCLUDE 'gen_data_structures.cmn'
      INCLUDE 'gen_constants.par'
      INCLUDE 'gen_units.par'
      include 'hms_scin_parms.cmn'
      include 'hms_scin_tof.cmn'
      include 'hms_statistics.cmn'

      integer pln,cnt
      real ave,ave2,num        !intermediate variables for sigma(position)
      real p1,p2,p3,p4         !prob. of having both tubes fire for planes1-4
      real p1234,p123,p124,p134,p234 !prob. of having combos fire
      save

! fill sums over counters
      do pln=1,hnum_scin_planes
        hstat_trksum(pln)=0
        hstat_possum(pln)=0
        hstat_negsum(pln)=0
        hstat_andsum(pln)=0
        hstat_orsum(pln)=0
        hscin_tot_dpos_sum(pln)=0.
        hscin_tot_num_dpos(pln)=0
        do cnt=1,hnum_scin_counters(pln)
          hstat_trksum(pln)=hstat_trksum(pln)+hstat_trk(pln,cnt)
          hstat_possum(pln)=hstat_possum(pln)+hstat_poshit(pln,cnt)
          hstat_negsum(pln)=hstat_negsum(pln)+hstat_neghit(pln,cnt)
          hstat_andsum(pln)=hstat_andsum(pln)+hstat_andhit(pln,cnt)
          hstat_orsum(pln)=hstat_orsum(pln)+hstat_orhit(pln,cnt)
          num=float(max(1,hscin_num_dpos(pln,cnt)))
          ave=hscin_dpos_sum(pln,cnt)/num
          hscin_dpos_ave(pln,cnt)=ave
          ave2=hscin_dpos_sum2(pln,cnt)/num
          hscin_dpos_sig(pln,cnt)=sqrt(max(0.,(ave2/num)-ave*ave))
          hscin_tot_dpos_sum(pln)=
     &            hscin_tot_dpos_sum(pln)+hscin_dpos_sum(pln,cnt)
          hscin_tot_num_dpos(pln)=
     &            hscin_tot_num_dpos(pln)+hscin_num_dpos(pln,cnt)
        enddo
        hstat_poseff(pln)=hstat_possum(pln)/max(1.,float(hstat_trksum(pln)))
        hstat_negeff(pln)=hstat_negsum(pln)/max(1.,float(hstat_trksum(pln)))
        hstat_andeff(pln)=hstat_andsum(pln)/max(1.,float(hstat_trksum(pln)))
        hstat_oreff(pln)=hstat_orsum(pln)/max(1.,float(hstat_trksum(pln)))
        hscin_tot_dpos_ave(pln)=
     &        hscin_tot_dpos_sum(pln)/max(1.,float(hscin_tot_num_dpos(pln)))
      enddo

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
