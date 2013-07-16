      subroutine s_init_histid(Abort,err)
*
*     routine to get HBOOK histogram ID numbers for all hard coded
*     histograms.
*
*     Author:	D. F. Geesaman
*     Date:      9 April 1994
*
* $Log: s_init_histid.f,v $
* Revision 1.7  1999/02/23 18:58:40  csa
* (JRA) Add pos/neg cal stuff
*
* Revision 1.6  1999/02/03 21:13:45  saw
* Code for new Shower counter tubes
*
* Revision 1.5  1996/09/05 19:53:39  saw
* (JRA) Get id for misc. TDC's
*
* Revision 1.4  1996/01/17 19:02:45  cdaq
* (JRA) Add sidcuttdc, sidscinalltimes, and sidscintimes
*
* Revision 1.3  1995/08/31 18:43:01  cdaq
* (JRA) Add dpos (pos. track - pos. hit) histograms
*
* Revision 1.2  1995/07/20  18:57:12  cdaq
* (JRA) Add per hit adc/tdc sums for hodo and calormeter
* (SAW) Relocate data statements for f2c compatibility
*
* Revision 1.1  1995/05/22  18:32:10  cdaq
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
      parameter (here= 's_init_histid')
*
      logical ABORT
      character*(*) err
      external thgetid
      integer*4 thgetid
      integer*4 plane,counter
*
      include 'sos_data_structures.cmn'
      include 'sos_tracking.cmn'
      include 'sos_track_histid.cmn'          
      include 'sos_scin_parms.cmn'
      include 'sos_id_histid.cmn'
  
      character*32 histname
      character*8 wiremap
      character*10 drifttime
      character*9 driftdis
      character*9 wirecent
      character*9 residual
      character*9 singres
      character*6 posadc,negadc,postdc,negtdc
      character*6 sdcplanename(smax_num_dc_planes)
      character*1 sscinplanenum(SNUM_SCIN_PLANES)
      character*10 sscinplane
      character*7 sposadc,snegadc,spostdc,snegtdc
      character*7 sscinplanename(SNUM_SCIN_PLANES)

      data wiremap/'_wiremap'/       
      data drifttime/'_drifttime'/
      data driftdis /'_driftdis'/
      data wirecent/'_wirecent'/
      data residual/'_residual'/
      data singres/'_sing_res'/
      data posadc /'posadc'/
      data negadc /'negadc'/
      data postdc /'postdc'/
      data negtdc /'negtdc'/
      data sdcplanename/'sdc1u1','sdc1u2','sdc1x1','sdc1x2','sdc1v1'
     $     ,'sdc1v2','sdc2u1','sdc2u2','sdc2x1','sdc2x2','sdc2v1','sdc2v2'/
      data sscinplanenum/'1','2','3','4'/
      data sscinplane /'sscinplane'/
      data sposadc /'sposadc'/
      data snegadc /'snegadc'/
      data spostdc /'spostdc'/
      data snegtdc /'snegtdc'/
      data sscinplanename/'sscin1x','sscin1y','sscin2x','sscin2y'/
*     
      SAVE
*--------------------------------------------------------
*     
      ABORT= .FALSE.
      err= ' '
*     
*     Histogram block sos_target
*
      sidsx_tar = thgetid('sx_tar')
      sidsy_tar = thgetid('sy_tar')
      sidsz_tar = thgetid('sz_tar')
      sidsxp_tar = thgetid('sxp_tar')
      sidsyp_tar = thgetid('syp_tar')
      sidsdelta_tar = thgetid('sdelta_tar')
      sidsp_tar = thgetid('sp_tar')   

*     
*     histogram block sos_focal_plane

      sidsx_fp = thgetid('sx_fp')
      sidsy_fp = thgetid('sy_fp')
      sidsxp_fp = thgetid('sxp_fp')
      sidsyp_fp = thgetid('syp_fp')
      sidslogchi2_fp = thgetid('slogchi2_fp')
      sidsnfree_fp = thgetid('snfree_fp')
      sidschi2perdeg_fp = thgetid('schi2perdeg_fp')
*     histogram block sos_decoded_dc
      sidrawtdc = thgetid('sdcrawtdc')
      sidcuttdc = thgetid('sdccuttdc')
      do plane = 1, sdc_num_planes
        histname = sdcplanename(plane)//wiremap
        siddcwiremap(plane) = thgetid(histname)
        histname = sdcplanename(plane)//drifttime
        siddcdrifttime(plane) = thgetid(histname)
        histname = sdcplanename(plane)//driftdis
        siddcdriftdis(plane) = thgetid(histname)
        histname = sdcplanename(plane)//wirecent
        siddcwirecent(plane) = thgetid(histname)
        histname = sdcplanename(plane)//residual
        sidres_fp(plane) = thgetid(histname)
        histname = sdcplanename(plane)//singres
        sidsingres_fp(plane) = thgetid(histname)
      enddo                             ! end loop over dc planes 

*     histogram block sos_raw_sc

      sidscinrawtothits = thgetid('sscintothits')
      sidscinplane = thgetid('sscinplane')
      sidscinalltimes = thgetid('sscinalltimes')
      sidscintimes = thgetid('sscintimes')

      snum_scin_counters(1) = sscin_1x_nr
      snum_scin_counters(2) = sscin_1y_nr
      snum_scin_counters(3) = sscin_2x_nr
      snum_scin_counters(4) = sscin_2y_nr

      siddcdposx = thgetid('sdcdposx')
      siddcdposy = thgetid('sdcdposy')
      siddcdposxp = thgetid('sdcdposxp')
      siddcdposyp = thgetid('sdcdposyp') 
      sidcaldpos = thgetid('scaldpos')

      do plane = 1, SNUM_SCIN_PLANES
        histname = sscinplane//sscinplanenum(plane)
        sidscincounters(plane) = thgetid(histname)
        histname = spostdc//sscinplanenum(plane)
        sidscinallpostdc(plane) = thgetid(histname)
        histname = snegtdc//sscinplanenum(plane)
        sidscinallnegtdc(plane) = thgetid(histname)
        histname = sposadc//sscinplanenum(plane)
        sidscinallposadc(plane) = thgetid(histname)
        histname = snegadc//sscinplanenum(plane)
        sidscinallnegadc(plane) = thgetid(histname)

        histname = "ssumpostdc"//sscinplanenum(plane)
        sidsumpostdc(plane) = thgetid(histname)
        histname = "ssumnegtdc"//sscinplanenum(plane)
        sidsumnegtdc(plane) = thgetid(histname)
        histname = "ssumposadc"//sscinplanenum(plane)
        sidsumposadc(plane) = thgetid(histname)
        histname = "ssumnegadc"//sscinplanenum(plane)
        sidsumnegadc(plane) = thgetid(histname)

        histname = "sscindpos"//sscinplanenum(plane)
        sidscindpos(plane) = thgetid(histname)

        histname = "sscindpos_pid"//sscinplanenum(plane)
        sidscindpos_pid(plane) = thgetid(histname)

        do counter = 1,snum_scin_counters(plane)
*     this is probably very awkward character manipulation
*     
          if(counter.lt.10) then
            write(histname,'(a7,i1,a6)') sscinplanename(plane),counter,posadc
          else
            write(histname,'(a7,i2,a6)') sscinplanename(plane),counter,posadc
          endif
          sidscinposadc(plane,counter) = thgetid(histname)
          if(counter.lt.10) then
            write(histname,'(a7,i1,a6)') sscinplanename(plane),counter,negadc
          else
            write(histname,'(a7,i2,a6)') sscinplanename(plane),counter,negadc
          endif
          sidscinnegadc(plane,counter) = thgetid(histname)
          if(counter.lt.10) then
            write(histname,'(a7,i1,a6)') sscinplanename(plane),counter,postdc
          else
            write(histname,'(a7,i2,a6)') sscinplanename(plane),counter,postdc
          endif
          sidscinpostdc(plane,counter) = thgetid(histname)
          if(counter.lt.10) then
            write(histname,'(a7,i1,a6)') sscinplanename(plane),counter,negtdc
          else
            write(histname,'(a7,i2,a6)') sscinplanename(plane),counter,negtdc
          endif
          sidscinnegtdc(plane,counter) = thgetid(histname)     
        enddo                           ! end loop over scintillator counters
      enddo                             ! end loop over scintillator plane

      sidcalplane = thgetid('scalplane')
      sidcalposhits(1) = thgetid('scalaposhits')
      sidcalposhits(2) = thgetid('scalbposhits')
      sidcalposhits(3) = thgetid('scalcposhits')
      sidcalposhits(4) = thgetid('scaldposhits')
      sidcalneghits(1) = thgetid('scalaneghits')
      sidcalneghits(2) = thgetid('scalbneghits')
      sidcalneghits(3) = thgetid('scalcneghits')
      sidcalneghits(4) = thgetid('scaldneghits')
      sidcalsumadc = thgetid('scalsumadc')

      sidmisctdcs = thgetid('smisctdcs')

      RETURN
      END

