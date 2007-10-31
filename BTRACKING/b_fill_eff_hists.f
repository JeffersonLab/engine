      subroutine b_fill_eff_hists(ABORT,err)

      implicit none
      save
      
      character*16 here
      parameter(here='b_fill_eff_hists')

      logical abort
      character*(*) err

      integer irow,icol,icell
      real Eavg,eff

      include 'bigcal_data_structures.cmn'
      include 'gen_run_info.cmn'
      include 'bigcal_hist_id.cmn'

c     calculate final "efficiencies" and fill histograms
      
      do icell=1,bigcal_all_maxhits
         if(icell.le.bigcal_prot_maxhits) then
            irow=(icell-1)/32 + 1
            icol=mod(icell-1,32) + 1
         else
            irow=(icell-1-bigcal_prot_maxhits)/30 + 33
            icol=mod(icell-1-bigcal_prot_maxhits,30)+1
         endif

         if(b_all_run_Enum(icell).gt.0) then
            Eavg = b_all_run_Esum(icell)/float(b_all_run_Enum(icell))
         else 
            Eavg = 0.
         endif
         if(bid_bcal_exy.gt.0) call hf2(bid_bcal_exy,float(icol),float(irow),Eavg)

         if(b_all_run_clst_good(icell)+b_all_run_clst_bad(icell)
     $        .gt.0)then
            eff = float(b_all_run_clst_good(icell)) / 
     $           float(b_all_run_clst_good(icell) + 
     $           b_all_run_clst_bad(icell))
         else 
            eff = 0.
         endif
         if(icell.le.bigcal_prot_maxhits) then
            if(bid_bcal_prot_eff.gt.0) call hf1(bid_bcal_prot_eff,float(icell),eff)
         else
            if(bid_bcal_rcs_eff.gt.0) call hf1(bid_bcal_rcs_eff,float(icell-bigcal_prot_maxhits),eff)
         endif

c         b_all_run_Enum(icell) = 0
c         b_all_run_Esum(icell) = 0.
         

      enddo

      abort=.false.
      err=' '

      return 
      end
