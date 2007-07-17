      subroutine b_fill_bigcal_arrays(abort,err)
      
      implicit none
      save

      logical abort
      character*(*) err
      
      character*20 here
      parameter(here='b_fill_bigcal_arrays')

      integer ihit,irow,icol
      integer icell,ngood

      include 'bigcal_data_structures.cmn'
      include 'bigcal_geometry.cmn'

      abort = .false.
      err = ' '

c     this routine fills the arrays that are used by the cluster finding algorithm

      ngood = 0

      if(bigcal_prot_ngood.gt.0) then
         do ihit=1,bigcal_prot_ngood
            irow = bigcal_prot_iygood(ihit)
            icol = bigcal_prot_ixgood(ihit)
            icell = icol + bigcal_prot_nx*(irow-1)

            ngood = ngood + 1

            bigcal_all_adc_good(ngood) = bigcal_prot_adc_good(ihit)
            bigcal_all_ecell(ngood) = bigcal_prot_ecell(ihit)
            bigcal_all_xgood(ngood) = bigcal_prot_xgood(ihit)
            bigcal_all_ygood(ngood) = bigcal_prot_ygood(ihit)

            bigcal_all_iygood(ngood) = irow
            bigcal_all_ixgood(ngood) = icol

            bigcal_all_good_det(icell) = bigcal_prot_ecell(ihit)

         enddo
      endif

      if(bigcal_rcs_ngood.gt.0) then
         do ihit=1,bigcal_rcs_ngood
            irow = bigcal_rcs_iygood(ihit)
            icol = bigcal_rcs_ixgood(ihit)
            icell = icol + bigcal_rcs_nx*(irow-1) + bigcal_prot_maxhits
            
            ngood = ngood + 1
            bigcal_all_adc_good(ngood) = bigcal_rcs_adc_good(ihit)
            bigcal_all_ecell(ngood) = bigcal_rcs_ecell(ihit)
            bigcal_all_xgood(ngood) = bigcal_rcs_xgood(ihit)
            bigcal_all_ygood(ngood) = bigcal_rcs_ygood(ihit)

            bigcal_all_iygood(ngood) = irow + bigcal_prot_ny
            bigcal_all_ixgood(ngood) = icol
            
            bigcal_all_good_det(icell) = bigcal_rcs_ecell(ihit)
         enddo
      endif

      bigcal_all_ngood = ngood

      return 
      end

      

            
