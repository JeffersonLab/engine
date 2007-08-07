      subroutine b_proper_shutdown(lunout,ABORT,err)

      implicit none
      save

      character*17 here
      parameter(here='b_proper_shutdown')

      include 'gen_routines.dec'
      include 'gen_filenames.cmn'
      include 'gen_run_info.cmn'
      include 'bigcal_data_structures.cmn'
      include 'bigcal_filenames.cmn'
      include 'bigcal_bypass_switches.cmn'
      include 'bigcal_hist_id.cmn'

      logical abort, report_abort
      character*(*) err

      integer lunout
      integer ierr
      character*132 file

      integer irow,icol,icell
      real Eavg,eff

      abort=.false.
      err=' '

      call b_report_bad_data(lunout,ABORT,err)

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
         call hf2(bid_bcal_exy,float(icol),float(irow),Eavg)

         if(b_all_run_clst_good(icell)+b_all_run_clst_bad(icell)
     $        .gt.0)then
            eff = float(b_all_run_clst_good(icell)) / 
     $           float(b_all_run_clst_good(icell) + 
     $           b_all_run_clst_bad(icell))
         else 
            eff = 0.
         endif
         if(icell.le.bigcal_prot_maxhits) then
            call hf1(bid_bcal_prot_eff,float(icell),eff)
         else
            call hf1(bid_bcal_rcs_eff,float(icell-bigcal_prot_maxhits),eff)
         endif
      enddo


      if(b_report_blockname.ne.' '.and.
     $     b_report_output_filename.ne.' ') then
         file = b_report_output_filename
         call g_sub_run_number(file,gen_run_number)
         ierr = threp(b_report_blockname,file)
         if(ierr.ne.0) then
            call g_append(err,'& threp failed to create report in file'
     $           //file)
            report_abort=.true.
         endif
      endif

      if(ABORT.or.report_abort) then
         call G_add_path(here,err)
      else
         err=' '
      endif
      
      return 
      end
