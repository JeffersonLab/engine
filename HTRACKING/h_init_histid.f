      subroutine h_init_histid(Abort,err)
*
*     routine to get HBOOK histogram ID numbers for all hard coded
*     histograms.
*
*     Author:	D. F. Geesaman
*     Date:      9 April 1994
*
* $Log: h_init_histid.f,v $
* Revision 1.8  2002/12/20 21:53:33  jones
* Modified by Hamlet for new HMS aerogel
*
* Revision 1.8  2002/10/05 (Hamlet)
* Add HMS Aerogel
*
* Revision 1.7  1999/02/23 18:38:56  csa
* (JRA) Add pos/neg cal stuff
*
* Revision 1.6  1999/02/03 21:13:23  saw
* Code for new Shower counter tubes
*
* Revision 1.5  1996/08/30 19:55:09  saw
* (JRA) Get id for misc. TDC's
*
* Revision 1.4  1996/01/16 21:52:05  cdaq
* (JRA) Add hidcuttdc, hidscinalltimes, and hidscintimes
*
* Revision 1.3  1995/08/31 14:53:47  cdaq
* (JRA) Add dpos (pos. track - pos. hit) histograms
*
* Revision 1.2  1995/07/19  18:20:41  cdaq
* (JRA) Add per hit adc/tdc sums for hodo and calormeter
* (SAW) Relocate data statements for f2c compatibility
*
* Revision 1.1  1995/05/22  18:33:05  cdaq
* Initial revision
*
* Revision 1.6  1995/05/12  12:23:22  cdaq
* (JRA) Modify/add user histograms
*
* Revision 1.5  1995/04/06  20:33:34  cdaq
* (SAW) Fix SOS wc plane names.  Add SOS residuals histogram id's
*
c Revision 1.4  1995/03/14  21:01:18  cdaq
c (SAW) Change ?scin_num_counters to ?num_scin_counters
c
c Revision 1.3  1994/08/18  03:13:51  cdaq
c (SAW) Use arrays of histids for residuals, new names for residuals histos
c
c Revision 1.2  1994/05/12  18:59:14  cdaq
c (DFG) Add hms_target and sos_target histid
c
c Revision 1.1  1994/05/12  18:56:22  cdaq
c Initial revision
c
* Revision 1.1  1994/04/12  21:00:57  cdaq
* Initial revision
*
*-
*--------------------------------------------------------
      IMPLICIT NONE
*
      character*13 here
      parameter (here= 'h_init_histid')
*
      logical ABORT
      character*(*) err
      external thgetid
      integer*4 thgetid
      integer*4 plane,counter
*
      include 'hms_data_structures.cmn'
      include 'hms_tracking.cmn'
      include 'hms_track_histid.cmn'          
      include 'hms_scin_parms.cmn'
      include 'hms_id_histid.cmn'   
  
      character*32 histname

      character*8 wiremap
      character*10 drifttime
      character*9 driftdis
      character*9 wirecent
      character*9 residual
      character*9 singres
      character*6 hdcplanename(hmax_num_dc_planes)
      character*1 hscinplanenum(HNUM_SCIN_PLANES)
      character*10 hscinplane
      character*7 hscinplanename(HNUM_SCIN_PLANES)
      character*6 posadc,negadc,postdc,negtdc
      character*7 hposadc,hnegadc,hpostdc,hnegtdc
      character*9 posadctdc, negadctdc

      data wiremap/'_wiremap'/       
      data drifttime/'_drifttime'/
      data driftdis /'_driftdis'/
      data wirecent/'_wirecent'/
      data residual/'_residual'/
      data singres/'_sing_res'/
      data hdcplanename/'hdc1x1','hdc1y1','hdc1u1','hdc1v1','hdc1y2'
     $     ,'hdc1x2','hdc2x1','hdc2y1','hdc2u1','hdc2v1','hdc2y2','hdc2x2'/
      data hscinplanenum/'1','2','3','4'/
      data hscinplane /'hscinplane'/
      data hscinplanename/'hscin1x','hscin1y','hscin2x','hscin2y'/
      data posadc /'posadc'/
      data negadc /'negadc'/
      data postdc /'postdc'/
      data negtdc /'negtdc'/       
      data hposadc /'hposadc'/
      data hnegadc /'hnegadc'/
      data hpostdc /'hpostdc'/
      data hnegtdc /'hnegtdc'/

      data posadctdc /'pos2d'/
      data negadctdc /'neg2d'/
*     
      SAVE
*--------------------------------------------------------
*     
      ABORT= .FALSE.
      err= ' '
*     Histogram block hms_target
*
      hidhx_tar = thgetid('hx_tar')
      hidhy_tar = thgetid('hy_tar')
      hidhz_tar = thgetid('hz_tar')
      hidhxp_tar = thgetid('hxp_tar')
      hidhyp_tar = thgetid('hyp_tar')
      hidhdelta_tar = thgetid('hdelta_tar')
      hidhp_tar = thgetid('hp_tar')   

*     histogram block hms_focal_plane
*
      hidhx_fp = thgetid('hx_fp')
      hidhy_fp = thgetid('hy_fp')
      hidhxp_fp = thgetid('hxp_fp')
      hidhyp_fp = thgetid('hyp_fp')
      hidhlogchi2_fp = thgetid('hlogchi2_fp')
      hidhnfree_fp = thgetid('hnfree_fp')
      hidhchi2perdeg_fp = thgetid('hchi2perdeg_fp')

*     histogram block hms_decoded_dc
      hidrawtdc = thgetid('hdcrawtdc')
      hidcuttdc = thgetid('hdccuttdc')
      do plane = 1, hdc_num_planes
        histname = hdcplanename(plane)//wiremap
        hiddcwiremap(plane) = thgetid(histname)
        histname = hdcplanename(plane)//drifttime
        hiddcdrifttime(plane) = thgetid(histname)
        histname = hdcplanename(plane)//driftdis
        hiddcdriftdis(plane) = thgetid(histname)
        histname = hdcplanename(plane)//wirecent
        hiddcwirecent(plane) = thgetid(histname)
        histname = hdcplanename(plane)//residual
        hidres_fp(plane) = thgetid(histname)
        histname = hdcplanename(plane)//singres
        hidsingres_fp(plane) = thgetid(histname)
      enddo                             ! end loop over dc planes 

*     histogram block hms_raw_sc

      hidscinrawtothits = thgetid('hscintothits')
      hidscinplane = thgetid('hscinplane')
      hidscinalltimes = thgetid('hscinalltimes')
      hidscintimes = thgetid('hscintimes')
      hnum_scin_counters(1) = hscin_1x_nr
      hnum_scin_counters(2) = hscin_1y_nr
      hnum_scin_counters(3) = hscin_2x_nr
      hnum_scin_counters(4) = hscin_2y_nr

      hiddcdposx = thgetid('hdcdposx')
      hiddcdposy = thgetid('hdcdposy')
      hiddcdposxp = thgetid('hdcdposxp')
      hiddcdposyp = thgetid('hdcdposyp')
      hidcaldpos = thgetid('hcaldpos')

      do plane = 1, HNUM_SCIN_PLANES
        histname = hscinplane//hscinplanenum(plane)
        hidscincounters(plane) = thgetid(histname)
        histname = hpostdc//hscinplanenum(plane)
        hidscinallpostdc(plane) = thgetid(histname)
        histname = hnegtdc//hscinplanenum(plane)
        hidscinallnegtdc(plane) = thgetid(histname)
        histname = hposadc//hscinplanenum(plane)

*        print *, 'h_init_histid: histname ', histname

        hidscinallposadc(plane) = thgetid(histname)

*        print *, hidscinallposadc(plane)

        histname = hnegadc//hscinplanenum(plane)
        hidscinallnegadc(plane) = thgetid(histname)

        histname = "hsumpostdc"//hscinplanenum(plane)
        hidsumpostdc(plane) = thgetid(histname)
        histname = "hsumnegtdc"//hscinplanenum(plane)
        hidsumnegtdc(plane) = thgetid(histname)
        histname = "hsumposadc"//hscinplanenum(plane)
        hidsumposadc(plane) = thgetid(histname)
        histname = "hsumnegadc"//hscinplanenum(plane)
        hidsumnegadc(plane) = thgetid(histname)
* For 2D histograms
*        histname = "hposadctdc"//hscinplanenum(plane)
*        print *, '#### histname ', histname
*        hidscinposadctdc(plane, 1) = thgetid(histname)
*        print *, hidscinposadctdc(plane, 1)
*        histname = "hnegadctdc"//hscinplanenum(plane)
*        hidscinnegadctdc(plane, 1) = thgetid(histname)

        histname = "hscindpos"//hscinplanenum(plane)
        hidscindpos(plane) = thgetid(histname)
        histname = "hscindpos_pid"//hscinplanenum(plane)
        hidscindpos_pid(plane) = thgetid(histname)

        do counter = 1,hnum_scin_counters(plane)
*     this is probably very awkward character manipulation
*     
          if(counter.lt.10) then
            write(histname,'(a7,i1,a6)') hscinplanename(plane),counter,posadc
          else
            write(histname,'(a7,i2,a6)') hscinplanename(plane),counter,posadc
          endif
          hidscinposadc(plane,counter) = thgetid(histname)
          if(counter.lt.10) then
            write(histname,'(a7,i1,a6)') hscinplanename(plane),counter,negadc
          else
            write(histname,'(a7,i2,a6)') hscinplanename(plane),counter,negadc
          endif
          hidscinnegadc(plane,counter) = thgetid(histname)
          if(counter.lt.10) then
            write(histname,'(a7,i1,a6)') hscinplanename(plane),counter,postdc
          else
            write(histname,'(a7,i2,a6)') hscinplanename(plane),counter,postdc
          endif
          hidscinpostdc(plane,counter) = thgetid(histname)
          if(counter.lt.10) then
            write(histname,'(a7,i1,a6)') hscinplanename(plane),counter,negtdc
          else
            write(histname,'(a7,i2,a6)') hscinplanename(plane),counter,negtdc
          endif
          hidscinnegtdc(plane,counter) = thgetid(histname)  


          if(counter.lt.10) then
            write(histname,'(a7,i1,a9)') hscinplanename(plane),counter,posadctdc
          else
            write(histname,'(a7,i2,a9)') hscinplanename(plane),counter,posadctdc
          endif
          hidscinposadctdc(plane,counter) = thgetid(histname)
*          write(*,*) '####', histname, hidscinposadctdc(plane,counter), plane, counter 
          if(counter.lt.10) then
            write(histname,'(a7,i1,a9)') hscinplanename(plane),counter,negadctdc
          else
            write(histname,'(a7,i2,a9)') hscinplanename(plane),counter,negadctdc
          endif
          hidscinnegadctdc(plane,counter) = thgetid(histname)   
        enddo                           ! end loop over scintillator counters
      enddo                             ! end loop over scintillator plane

*
      hidcalplane = thgetid('hcalplane')
      hidcalposhits(1) = thgetid('hcalaposhits')
      hidcalposhits(2) = thgetid('hcalbposhits')
      hidcalposhits(3) = thgetid('hcalcposhits')
      hidcalposhits(4) = thgetid('hcaldposhits')
      hidcalneghits(1) = thgetid('hcalaneghits')
      hidcalneghits(2) = thgetid('hcalbneghits')
      hidcalneghits(3) = thgetid('hcalcneghits')
      hidcalneghits(4) = thgetid('hcaldneghits')
      hidcalsumadc = thgetid('hcalsumadc')

      hidmisctdcs = thgetid('hmisctdcs')

c
* HMS Aerogel

      hidhaero_adc_pos_hits = thgetid('haeroadcposhits')
      hidhaero_adc_neg_hits = thgetid('haeroadcneghits')
      hidhaero_tdc_pos_hits = thgetid('haerotdcposhits')
      hidhaero_tdc_neg_hits = thgetid('haerotdcneghits')
      hidhaero_adc_pos_pedsubtr = thgetid('haeroadcpospedsubtr')
      hidhaero_adc_neg_pedsubtr = thgetid('haeroadcnegpedsubtr')
c

      RETURN
      END

