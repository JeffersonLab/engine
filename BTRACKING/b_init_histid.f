      subroutine b_init_histid(ABORT,err)

      implicit none
      save
      
      character*13 here
      parameter(here='b_init_histid')

      logical ABORT
      character*(*) err

      external thgetid
      integer*4 thgetid
      integer i,j,irow,tens,ones,itens,iones,jtens,jones,icell
      
      include 'bigcal_data_structures.cmn'
      include 'bigcal_gain_parms.cmn'
      include 'bigcal_shower_parms.cmn'
      include 'bigcal_tof_parms.cmn'
      include 'bigcal_geometry.cmn'
      include 'bigcal_bypass_switches.cmn'
      include 'bigcal_hist_id.cmn'

      character*10 histname

c     For now, don't do anything here: I don't necessarily want to do 
c     hard-coded histograms. I would like to use CTP. Will come back later
c     if this proves more convenient
c     It's later, I'm back. CTP histos on their own aren't terribly useful, 
c     need to hardcode some:

      bid_bcal_row = thgetid('bcal_row')
      bid_bcal_col = thgetid('bcal_col')
      bid_bcal_rowcol = thgetid('bcal_rowcol')
      bid_bcal_tadcvsum64 = thgetid('bcal_tadc_v_sum64')
      bid_bcal_trchvmax64 = thgetid('bcal_trch_v_max64')
      bid_bcal_ttdcvtdc = thgetid('bcal_ttdc_v_tdc')
      bid_bcal_prot_eff = thgetid('bcal_prot_eff')
      bid_bcal_rcs_eff = thgetid('bcal_rcs_eff')
      
      bid_bcal_ixclust = thgetid('bcal_ixclust')
      bid_bcal_iyclust = thgetid('bcal_iyclust')
      bid_bcal_rowcolclust = thgetid('bcal_rowcolclust')

      bid_bcal_eclust = thgetid('bcal_eclust')
      
      bid_bcal_ncellclst = thgetid('bcal_ncellclust')
      bid_bcal_nxclust = thgetid('bcal_nxclust')
      bid_bcal_nyclust = thgetid('bcal_nyclust')
      
      bid_bcal_xmom = thgetid('bcal_xmoment')
      bid_bcal_ymom = thgetid('bcal_ymoment')
      bid_bcal_nxny = thgetid('bcal_nxny')
      
      bid_bcal_tmean = thgetid('bcal_tmean')
      bid_bcal_trms = thgetid('bcal_trms')
         
      bid_bcal_xclust = thgetid('bcal_xclust')
      bid_bcal_yclust = thgetid('bcal_yclust')
      bid_bcal_xy = thgetid('bcal_xy')
      bid_bcal_exy = thgetid('bcal_exy')
      bid_bcal_theta = thgetid('bcal_thetaclst')
      bid_bcal_phi = thgetid('bcal_phiclst')

      bid_bcal_ped_mean_prot = thgetid('bcal_ped_mean_prot')
      bid_bcal_ped_mean_rcs  = thgetid('bcal_ped_mean_rcs')
      bid_bcal_ped_mean_trig = thgetid('bcal_ped_mean_trig')
      bid_bcal_ped_rms_prot = thgetid('bcal_ped_rms_prot')
      bid_bcal_ped_rms_rcs = thgetid('bcal_ped_rms_rcs')
      bid_bcal_ped_rms_trig = thgetid('bcal_ped_rms_trig')
      bid_bcal_pedw_prot = thgetid('bcal_pedw_prot')
      bid_bcal_pedw_rcs = thgetid('bcal_pedw_rcs')
      bid_bcal_pedw_trig = thgetid('bcal_pedw_trig')
      bid_bcal_raw_photodiode = thgetid('bcal_raw_photodiode')

      do i=1,32
         itens = i/10
         iones = mod(i,10)
         do j=1,32
            jtens = j/10
            jones = mod(j,10)

            icell = j + 32*(i-1)

            histname='ADC'//char(itens+ichar('0'))//
     $           char(iones+ichar('0'))//char(jtens+ichar('0'))//
     $           char(jones+ichar('0'))
            bid_badc(icell) = thgetid(histname)
         enddo
      enddo

      do i=33,56
         itens = i/10
         iones = mod(i,10)
         do j=1,30
            jtens = j/10
            jones = mod(j,10)
            
            icell = 1024 + j + 30*(i-33)

            histname='ADC'//char(itens+ichar('0'))//
     $           char(iones+ichar('0'))//char(jtens+ichar('0'))//
     $           char(jones+ichar('0'))
            bid_badc(icell) = thgetid(histname)
         enddo
      enddo

      do i=1,56
         tens = i/10
         ones = mod(i,10)
         histname = 'TDC'//char(tens + ichar('0'))//
     $        char(ones + ichar('0'))//'A'
         bid_btdc(1 + 4*(i-1) ) = thgetid(histname)
         histname = 'TDC'//char(tens + ichar('0'))//
     $        char(ones + ichar('0'))//'B'
         bid_btdc(2 + 4*(i-1) ) = thgetid(histname)
         histname = 'TDC'//char(tens + ichar('0'))//
     $        char(ones + ichar('0'))//'C'
         bid_btdc(3 + 4*(i-1) ) = thgetid(histname)
         histname = 'TDC'//char(tens + ichar('0'))//
     $        char(ones + ichar('0'))//'D'
         bid_btdc(4 + 4*(i-1) ) = thgetid(histname)

c$$$         histname = 'TWALK'//char(tens + ichar('0'))//
c$$$     $        char(ones + ichar('0'))//'A'
c$$$         bid_btimewalk(1 + 4*(i-1) ) = thgetid(histname)
c$$$         histname = 'TWALK'//char(tens + ichar('0'))//
c$$$     $        char(ones + ichar('0'))//'B'
c$$$         bid_btimewalk(2 + 4*(i-1) ) = thgetid(histname)
c$$$         histname = 'TWALK'//char(tens + ichar('0'))//
c$$$     $        char(ones + ichar('0'))//'C'
c$$$         bid_btimewalk(3 + 4*(i-1) ) = thgetid(histname)
c$$$         histname = 'TWALK'//char(tens + ichar('0'))//
c$$$     $        char(ones + ichar('0'))//'D'
c$$$         bid_btimewalk(4 + 4*(i-1) ) = thgetid(histname)
      enddo
      
      do i=1,21
         irow = 1 + 3*(i-1)
         tens = irow/10
         ones = mod(irow,10)

         histname = 'TTDC'//char(tens + ichar('0'))//
     $        char(ones + ichar('0'))//'AB'
c         write(*,*) 'histname=',histname
         bid_bttdc(1 + 2*(i-1)) = thgetid(histname)
         
         histname = 'TTDC'//char(tens + ichar('0'))//
     $        char(ones + ichar('0'))//'CD'
c         write(*,*) 'histname=',histname
         bid_bttdc(2 + 2*(i-1)) = thgetid(histname)

         if(i.lt.20) then
            histname = 'TADC'//char(tens+ichar('0'))//
     $           char(ones+ichar('0'))//'AB'
            bid_btadc(1 + 2*(i-1)) = thgetid(histname)
            histname = 'TADC'//char(tens+ichar('0'))//
     $           char(ones+ichar('0'))//'CD'
            bid_btadc(2 + 2*(i-1)) = thgetid(histname)
         endif

      enddo

      bid_bcal_empty = thgetid('bcal_empty')
      bid_bcal_small = thgetid('bcal_small')
      bid_bcal_cfac_old = thgetid('bcal_cfac_old')
      bid_bcal_cfac_new = thgetid('bcal_cfac_new')
      bid_bcal_cfac_dist = thgetid('bcal_cfac_dist')
      bid_bcal_oldxnew = thgetid('bcal_oldxnew')

      bid_bcal_row8 = thgetid('bcal_row8')
      bid_bcal_col8 = thgetid('bcal_col8')
      bid_bcal_row8vscol8 = thgetid('bcal_row8vscol8')
      bid_bcal_trow64 = thgetid('bcal_trow64')
      bid_bcal_tcol64 = thgetid('bcal_tcol64')
      bid_bcal_trow64vstcol64 = thgetid('bcal_trow64vstcol64')
      bid_bcal_arow64 = thgetid('bcal_arow64')
      bid_bcal_acol64 = thgetid('bcal_acol64')
      bid_bcal_arow64vsacol64 = thgetid('bcal_arow64vsacol64')
      bid_bcal_ttchanvstachan = thgetid('bcal_ttchanvstachan')
      bid_bcal_ttchanvstgroup = thgetid('bcal_ttchanvstgroup')

      abort=.false.
      err=' '
      
      return
      end
