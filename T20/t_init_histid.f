      subroutine t_init_histid(abort,err)
*
*     routine to get HBOOK histogram ID numbers for all hard coded
*     histograms.
*
*     Author:	 S.A. Wood
*     Date:      23 Jan 1997
*
* $Log$
* Revision 1.1  1998/12/01 20:56:35  saw
* Initial revision
*
*-
*--------------------------------------------------------
      IMPLICIT NONE
*
      character*13 here
      parameter (here= 't_init_histid')
*
      logical ABORT
      character*(*) err
      external thgetid
      integer*4 thgetid
c      integer*4 plane,counter
*
      include 't20_data_structures.cmn'
      include 't20_hodo.cmn'
      include 't20_tracking.cmn'
c      include 't20_track_histid.cmn'          
c      include 't20_scin_parms.cmn'
      include 't20_test_histid.cmn'
      include 't20_misc.cmn'
      include 'gen_misc.cmn'
  
c      character*32 histname
c      character*8 wiremap
c      character*10 drifttime
c      character*9 driftdis
c      character*9 wirecent
c      character*9 residual
c      character*9 singres
c      character*6 posadc,negadc,postdc,negtdc
c      character*6 sdcplanename(smax_num_dc_planes)
c      character*1 sscinplanenum(SNUM_SCIN_PLANES)
c      character*10 sscinplane
c      character*7 sposadc,snegadc,spostdc,snegtdc
c      character*7 sscinplanename(SNUM_SCIN_PLANES)
c
c      data wiremap/'_wiremap'/       
c      data drifttime/'_drifttime'/
c      data driftdis /'_driftdis'/
c      data wirecent/'_wirecent'/
c      data residual/'_residual'/
c      data singres/'_sing_res'/
c      data posadc /'posadc'/
c      data negadc /'negadc'/
c      data postdc /'postdc'/
c      data negtdc /'negtdc'/
c      data sdcplanename/'sdc1u1','sdc1u2','sdc1x1','sdc1x2','sdc1v1'
c     $     ,'sdc1v2','sdc2u1','sdc2u2','sdc2x1','sdc2x2','sdc2v1','sdc2v2'/
c      data sscinplanenum/'1','2','3','4'/
c      data sscinplane /'sscinplane'/
c      data sposadc /'sposadc'/
c      data snegadc /'snegadc'/
c      data spostdc /'spostdc'/
c      data snegtdc /'snegtdc'/
c      data sscinplanename/'sscin1x','sscin1y','sscin2x','sscin2y'/
*     
      SAVE
*--------------------------------------------------------
*     
      ABORT= .FALSE.
      err= ' '

      ttst_hid_straw_wletdc = thgetid('ttst_letdc_wide')
      ttst_hid_straw_letdc = thgetid('ttst_letdc')
      ttst_hid_straw_width = thgetid('ttst_strawwidth')
      ttst_hid_wg          = thgetid('ttst_wg')

c      ttst_hid_strawmap(1) = thgetid('ttstwiremapx1')
c      ttst_hid_strawmap(2) = thgetid('ttstwiremapx2')
c      ttst_hid_strawmap(3) = thgetid('ttstwiremapx3')
c      ttst_hid_strawmap(4) = thgetid('ttstwiremapx4')
c      ttst_hid_strawmap(5) = thgetid('ttstwiremapy1')
c      ttst_hid_strawmap(6) = thgetid('ttstwiremapy2')
c      ttst_hid_strawmap(7) = thgetid('ttstwiremapy3')
c      ttst_hid_strawmap(8) = thgetid('ttstwiremapy4')
      ttst_hid_strawmap = thgetid('ttst_straw_wiremaps')
      ttst_hid_numhitsonplanes = thgetid('num_hits_on_all_planes')
      ttst_hid_dmxchck = thgetid('ttst_demux_check')
      ttst_hid_driftdist   = thgetid('ttstdriftdist')
      ttst_hid_driftdistv0 = thgetid('ttstdriftdistv0')
      ttst_hid_drifttime   = thgetid('ttstdrifttime')
      ttst_hid_drifttimet0 = thgetid('ttstdrifttimet0')

      ttst_hid_xoryhits   = 
     +     thgetid('ttststrawhitsinxory')
      ttst_hid_evtstrcked = 
     +     thgetid('ttststrawtrckd_vs_hits')
c      ttst_hid_strawres   = thgetid('ttststrawres')
      ttst_hid_strawres   = thgetid('ttsttrackchisq')
      ttst_hid_strawhitgd = thgetid('ttststrawgoodhit')
      ttst_hid_strawhitbd = thgetid('ttststrawbadhit')
      ttst_hid_strawhitms = thgetid('ttststrawmisshit')

c      ttst_hid_planeoffs(1) = thgetid('ttststrawoffpl1')
c      ttst_hid_planeoffs(2) = thgetid('ttststrawoffpl2')
c      ttst_hid_planeoffs(3) = thgetid('ttststrawoffpl3')
c      ttst_hid_planeoffs(4) = thgetid('ttststrawoffpl4')
c      ttst_hid_planeoffs(5) = thgetid('ttststrawoffpl5')
c      ttst_hid_planeoffs(6) = thgetid('ttststrawoffpl6')
c      ttst_hid_planeoffs(7) = thgetid('ttststrawoffpl7')
c      ttst_hid_planeoffs(8) = thgetid('ttststrawoffpl8')

      ttst_hid_linoff(3) = thgetid('ttststrlinoffpl3')
      ttst_hid_linoff(4) = thgetid('ttststrlinoffpl4')
      ttst_hid_linoff(7) = thgetid('ttststrlinoffpl7')
      ttst_hid_linoff(8) = thgetid('ttststrlinoffpl8')

      ttst_hid_trackcode = thgetid('ttsttrackcode')
      ttst_hid_trackcodet = thgetid('ttsttrackcodet')
      ttst_hid_oot_thvx = thgetid('ttstoot-thvx-ch')
      ttst_hid_oot_phvy = thgetid('ttstoot-phvy-ch')
      ttst_hid_oot_yvx = thgetid('ttstoot-yvx-han')
*     
c ** POLDER hodoscope information
c  * last tdc for each hodoscope in each plane
c       tidh1p1_tdc_i = thgetid('th1p1_tdc_i')
c       tidh1p2_tdc_i = thgetid('th1p2_tdc_i')
c       tidh2p1_tdc_i = thgetid('th2p1_tdc_i')
c       tidh2p2_tdc_i = thgetid('th2p2_tdc_i')
c  * all tdc values for all hodoscopes in one plane
       tidh1p1_tdc_all = thgetid('th1p1_tdc_all')
       tidh1p2_tdc_all = thgetid('th1p2_tdc_all')
       tidh2p1_tdc_all = thgetid('th2p1_tdc_all')
       tidh2p2_tdc_all = thgetid('th2p2_tdc_all')
c  * 2dim: all tdc vs all bars (any plane)
       tidhod_allbars_vs_tdc = thgetid('tidhod_allbars_vs_tdc')
c
       tidmwpl1 = thgetid('tmwpl1')
       tidmwpl2 = thgetid('tmwpl2')
       tidmwpl3 = thgetid('tmwpl3')
       tidmwpl4 = thgetid('tmwpl4')
       tidmwpl5 = thgetid('tmwpl5')
       tidmwpl6 = thgetid('tmwpl6')
c
       tidmwpl1_anytdc = thgetid('tmwpl1_anytdc')
       tidmwpl2_anytdc = thgetid('tmwpl2_anytdc')
       tidmwpl3_anytdc = thgetid('tmwpl3_anytdc')
       tidmwpl4_anytdc = thgetid('tmwpl4_anytdc')
       tidmwpl5_anytdc = thgetid('tmwpl5_anytdc')
       tidmwpl6_anytdc = thgetid('tmwpl6_anytdc')
c
       tidmwpl1_tdc_vs_wire = thgetid('tmwpl1_tdc_vs_wire')
       tidmwpl2_tdc_vs_wire = thgetid('tmwpl2_tdc_vs_wire')
       tidmwpl3_tdc_vs_wire = thgetid('tmwpl3_tdc_vs_wire')
       tidmwpl4_tdc_vs_wire = thgetid('tmwpl4_tdc_vs_wire')
       tidmwpl5_tdc_vs_wire = thgetid('tmwpl5_tdc_vs_wire')
       tidmwpl6_tdc_vs_wire = thgetid('tmwpl6_tdc_vs_wire')
c
cc       tmwpl1_wire_mult = thgetid('tmwpl1_wire_mult')
cc       tmwpl2_wire_mult = thgetid('tmwpl2_wire_mult')
cc       tmwpl3_wire_mult = thgetid('tmwpl3_wire_mult')
cc       tmwpl4_wire_mult = thgetid('tmwpl4_wire_mult')
cc       tmwpl5_wire_mult = thgetid('tmwpl5_wire_mult')
cc       tmwpl6_wire_mult = thgetid('tmwpl6_wire_mult')
c
       tidmwpl1_multperwire = thgetid('tmwpl1_multperwire')
       tidmwpl2_multperwire = thgetid('tmwpl2_multperwire')
       tidmwpl3_multperwire = thgetid('tmwpl3_multperwire')
       tidmwpl4_multperwire = thgetid('tmwpl4_multperwire')
       tidmwpl5_multperwire = thgetid('tmwpl5_multperwire')
       tidmwpl6_multperwire = thgetid('tmwpl6_multperwire')

c       t_rwirepl7 = thgetid('t_rwirepl7')
c       t_rwirepl8 = thgetid('t_rwirepl8')
c       t_rwirepl9 = thgetid('t_rwirepl9')
c       t_lwirepl7  = thgetid('t_lwirepl7')
c       t_lwirepl8  = thgetid('t_lwirepl8')
c       t_lwirepl9  = thgetid('t_lwirepl9')

c       tmwpc3sumpl7  = thgetid('tmwpc3sumpl7')
c       tmwpc3sumpl8  = thgetid('tmwpc3sumpl8')
c       tmwpc3sumpl9  = thgetid('tmwpc3sumpl9')
c       tmwpc3diffpl7 = thgetid('tmwpc3diffpl7')
c       tmwpc3diffpl8 = thgetid('tmwpc3diffpl8')
c       tmwpc3diffpl9 = thgetid('tmwpc3diffpl9')

       g_scal_his1 = thgetid('g_scal_his1')
       g_scal_his2 = thgetid('g_scal_his2')
       g_scal_his3 = thgetid('g_scal_his3')
       g_scal_his4 = thgetid('g_scal_his4')
       g_scal_his5 = thgetid('g_scal_his5')
       g_scal_his6 = thgetid('g_scal_his6')
       g_scal_his7 = thgetid('g_scal_his7')
       g_scal_his8 = thgetid('g_scal_his8')
c 
      RETURN
      END

