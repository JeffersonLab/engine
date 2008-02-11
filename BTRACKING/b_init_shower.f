      subroutine b_init_shower(ABORT,err)

      implicit none
      save

      character*13 here
      parameter(here='b_init_shower')

      logical ABORT
      character*(*) err

      integer irow,icol,ipar,icell,j

      include 'bigcal_data_structures.cmn'
      include 'bigcal_shower_parms.cmn'

      ABORT=.false.
      err=' '

c     don't do anything for now...
c     boy do I hope I can get this CTP parm stuff working!!!!

c$$$      write(*,*) 'pxdet_par = ',bigcal_pxdet_par
c$$$      write(*,*) 'pydet_par = ',bigcal_pydet_par
c$$$      write(*,*) 'rxdet_par = ',bigcal_rxdet_par
c$$$      write(*,*) 'rydet_par = ',bigcal_rydet_par

      do icol=1,bigcal_prot_nx
         do ipar=1,bigcal_shower_npar
            icell = ipar + (icol-1)*bigcal_shower_npar
            bigcal_prot_xpar(icol,ipar) = bigcal_pxdet_par(icell)
         enddo
      enddo

      do irow=1,bigcal_prot_ny
         do ipar=1,bigcal_shower_npar
            icell = ipar + (irow-1)*bigcal_shower_npar
            bigcal_prot_ypar(irow,ipar) = bigcal_pydet_par(icell)
         enddo
      enddo

      do icol=1,bigcal_rcs_nx
         do ipar=1,bigcal_shower_npar
            icell = ipar + (icol-1)*bigcal_shower_npar
            bigcal_rcs_xpar(icol,ipar) = bigcal_rxdet_par(icell)
         enddo
      enddo

      do irow=1,bigcal_rcs_ny
         do ipar=1,bigcal_shower_npar
            icell = ipar + (irow-1)*bigcal_shower_npar
            bigcal_rcs_ypar(irow,ipar) = bigcal_rydet_par(icell)
         enddo
      enddo

c$$$      do ipar=1,6
c$$$         write(*,*) bigcal_prot_ypar(32,ipar)
c$$$      enddo

c$$$      write(*,*) 'prot_xpar = ',bigcal_prot_xpar
c$$$      write(*,*) 'prot_ypar = ',bigcal_prot_ypar
c$$$      write(*,*) 'rcs_xpar = ',bigcal_rcs_xpar
c$$$      write(*,*) 'rcs_ypar = ',bigcal_rcs_ypar

      bigcal_pxmap_nbin(1) = bigcal_px1map_nbin
      bigcal_pxmap_nbin(2) = bigcal_px2map_nbin
      bigcal_pxmap_nbin(3) = bigcal_px3map_nbin
      bigcal_pxmap_nbin(4) = bigcal_px4map_nbin

      bigcal_pymap_nbin(1) = bigcal_py1map_nbin
      bigcal_pymap_nbin(2) = bigcal_py2map_nbin
      bigcal_pymap_nbin(3) = bigcal_py3map_nbin
      bigcal_pymap_nbin(4) = bigcal_py4map_nbin

      bigcal_rxmap_nbin(1) = bigcal_rx1map_nbin
      bigcal_rxmap_nbin(2) = bigcal_rx2map_nbin
      bigcal_rxmap_nbin(3) = bigcal_rx3map_nbin
      bigcal_rxmap_nbin(4) = bigcal_rx4map_nbin

      bigcal_rymap_nbin(1) = bigcal_ry1map_nbin
      bigcal_rymap_nbin(2) = bigcal_ry2map_nbin
      bigcal_rymap_nbin(3) = bigcal_ry3map_nbin
c      bigcal_rymap_nbin(4) = bigcal_ry4map_nbin

      bigcal_pxmap_mmin(1) = bigcal_px1map_xmom_min
      bigcal_pxmap_mmin(2) = bigcal_px2map_xmom_min
      bigcal_pxmap_mmin(3) = bigcal_px3map_xmom_min
      bigcal_pxmap_mmin(4) = bigcal_px4map_xmom_min

      bigcal_pymap_mmin(1) = bigcal_py1map_xmom_min
      bigcal_pymap_mmin(2) = bigcal_py2map_xmom_min
      bigcal_pymap_mmin(3) = bigcal_py3map_xmom_min
      bigcal_pymap_mmin(4) = bigcal_py4map_xmom_min

      bigcal_rxmap_mmin(1) = bigcal_rx1map_xmom_min
      bigcal_rxmap_mmin(2) = bigcal_rx2map_xmom_min
      bigcal_rxmap_mmin(3) = bigcal_rx3map_xmom_min
      bigcal_rxmap_mmin(4) = bigcal_rx4map_xmom_min

      bigcal_rymap_mmin(1) = bigcal_ry1map_xmom_min
      bigcal_rymap_mmin(2) = bigcal_ry2map_xmom_min
      bigcal_rymap_mmin(3) = bigcal_ry3map_xmom_min



      bigcal_pxmap_mmax(1) = bigcal_px1map_xmom_max
      bigcal_pxmap_mmax(2) = bigcal_px2map_xmom_max
      bigcal_pxmap_mmax(3) = bigcal_px3map_xmom_max
      bigcal_pxmap_mmax(4) = bigcal_px4map_xmom_max

      bigcal_pymap_mmax(1) = bigcal_py1map_xmom_max
      bigcal_pymap_mmax(2) = bigcal_py2map_xmom_max
      bigcal_pymap_mmax(3) = bigcal_py3map_xmom_max
      bigcal_pymap_mmax(4) = bigcal_py4map_xmom_max

      bigcal_rxmap_mmax(1) = bigcal_rx1map_xmom_max
      bigcal_rxmap_mmax(2) = bigcal_rx2map_xmom_max
      bigcal_rxmap_mmax(3) = bigcal_rx3map_xmom_max
      bigcal_rxmap_mmax(4) = bigcal_rx4map_xmom_max

      bigcal_rymap_mmax(1) = bigcal_ry1map_xmom_max
      bigcal_rymap_mmax(2) = bigcal_ry2map_xmom_max
      bigcal_rymap_mmax(3) = bigcal_ry3map_xmom_max

      
      do j=1,bigcal_map_maxbins
         bigcal_pxmap_frac(1,j) = bigcal_px1map_xmom_frac(j)
         bigcal_pxmap_frac(2,j) = bigcal_px2map_xmom_frac(j)
         bigcal_pxmap_frac(3,j) = bigcal_px3map_xmom_frac(j)
         bigcal_pxmap_frac(4,j) = bigcal_px4map_xmom_frac(j)
         
         bigcal_pymap_frac(1,j) = bigcal_py1map_xmom_frac(j)
         bigcal_pymap_frac(2,j) = bigcal_py2map_xmom_frac(j)
         bigcal_pymap_frac(3,j) = bigcal_py3map_xmom_frac(j)
         bigcal_pymap_frac(4,j) = bigcal_py4map_xmom_frac(j)
         
         bigcal_rxmap_frac(1,j) = bigcal_rx1map_xmom_frac(j)
         bigcal_rxmap_frac(2,j) = bigcal_rx2map_xmom_frac(j)
         bigcal_rxmap_frac(3,j) = bigcal_rx3map_xmom_frac(j)
         bigcal_rxmap_frac(4,j) = bigcal_rx4map_xmom_frac(j)
         
         bigcal_rymap_frac(1,j) = bigcal_ry1map_xmom_frac(j)
         bigcal_rymap_frac(2,j) = bigcal_ry2map_xmom_frac(j)
         bigcal_rymap_frac(3,j) = bigcal_ry3map_xmom_frac(j)
      enddo

      return 
      end
