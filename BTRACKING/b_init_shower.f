      subroutine b_init_shower(ABORT,err)

      implicit none
      save

      character*13 here
      parameter(here='b_init_shower')

      logical ABORT
      character*(*) err

      integer irow,icol,ipar,icell,j,i

      include 'bigcal_data_structures.cmn'
      include 'bigcal_shower_parms.cmn'
      include 'bigcal_bypass_switches.cmn'

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

      if(b_recon_using_map.ne.0) then
         do i=1,28
            do j=1,bigcal_xmap_nbin(i)
               bigcal_xmap_xfrac(i,j) = bigcal_xmap_frac(j+
     $              bigcal_xmap_nbin(i)*(i-1))
            enddo
         enddo
         
         do i=1,28
            do j=1,bigcal_ymap_nbin(i)
               bigcal_ymap_yfrac(i,j) = bigcal_ymap_frac(j+
     $              bigcal_ymap_nbin(i)*(i-1))
            enddo
         enddo
      endif

      return 
      end
