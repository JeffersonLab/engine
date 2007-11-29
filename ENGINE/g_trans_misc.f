      subroutine g_trans_misc(abort,errmsg)
*-------------------------------------------------------------------
* author: John Arrington
* created: 1/16/96
*
* g_trans_misc fills the gen_decoded_misc common block
*
* $Log$
* Revision 1.2.24.5  2007/11/29 18:33:05  cdaq
* commented out diagnostic message
*
* Revision 1.2.24.4  2007/11/12 03:16:22  cdaq
* added timing window for triggers
*
* Revision 1.2.24.3  2007/10/29 19:44:07  cdaq
* Added handling of multi-hits for HMS and BigCal trigger TDCs
*
* Revision 1.2.24.2  2007/10/23 13:23:32  cdaq
* Added filling of raw trigger TDC histograms, signals are in the gmisc hit array
*
* Revision 1.2.24.1  2007/10/17 15:52:29  cdaq
* *** empty log message ***
*
* Revision 1.2  2002/09/25 14:37:32  jones
* character*1024 errmsg changed to character*(*) errmsgCVS: ----------------------------------------------------------------------
*
* Revision 1.1  1996/01/22 15:14:10  saw
* Initial revision
*
*--------------------------------------------------------

      implicit none

      include 'gen_data_structures.cmn'
      include 'gep_data_structures.cmn'
      include 'gep_hist_id.cmn'

      logical abort
      character*(*) errmsg
      character*20 here
      parameter (here = 'g_trans_misc')

      integer*4 ihit
      integer*4 nH1,nH2,nB
      real hittime

      save

      abort = .false.
      errmsg = ' '

      do ihit = 1 , gmax_misc_hits
        gmisc_dec_data(ihit,1) = 0     ! Clear TDC's
        gmisc_dec_data(ihit,2) = -1     ! Clear ADC's
      enddo
      
      nH1 = 0
      nH2 = 0
      nB  = 0

c      write(*,*) 'gmisc_tot_hits=',gmisc_tot_hits

      do ihit = 1 , gmisc_tot_hits
c$$$         if((gmisc_raw_addr2(ihit).eq.1.or.gmisc_raw_addr2(ihit).eq.2)
c$$$     $        .and. gmisc_raw_addr1(ihit).eq.2) then
c$$$            if(gmisc_raw_addr2(ihit).eq.1) then ! h+
c$$$               write(*,*) 'h+ hit, raw ADC=',gmisc_raw_data(ihit)
c$$$            endif
c$$$            if(gmisc_raw_addr2(ihit).eq.2) then ! h-
c$$$               write(*,*) 'h- hit, raw ADC=',gmisc_raw_data(ihit)
c$$$            endif
c$$$         endif

         if(gmisc_raw_addr1(ihit).eq.1) then
c            write(*,*) 'gmisc TDC hit ctr,TDCraw=',gmisc_raw_addr2(ihit),
c     $           gmisc_raw_data(ihit)
            if(gmisc_raw_addr2(ihit).eq.1) then !HMS1 trigger: fill hist

               hittime = .5*gmisc_raw_data(ihit)
               
               if(abs(hittime-gep_h1time_center).le.gep_h1time_slop)then
                  nH1 = nH1 + 1
                  if(nH1.le.8) then
                     GEP_H1time(nH1) = hittime
                  endif
                  if(gepid_gep_HMS1_rawtdc.gt.0) then
                     call hf1(gepid_gep_HMS1_rawtdc,float(gmisc_raw_data(ihit)),
     $                    1.)
                  endif
               endif
            endif
            if(gmisc_raw_addr2(ihit).eq.2) then !HMS2 trigger: fill hist
               hittime = .5*gmisc_raw_data(ihit)
               
               if(abs(hittime-gep_h2time_center).le.gep_h2time_slop)then
                  nH2 = nH2 + 1
                  if(nH2.le.8) then
                     GEP_H2time(nH2) = hittime
                  endif
                  if(gepid_gep_HMS2_rawtdc.gt.0) then
                     call hf1(gepid_gep_HMS2_rawtdc,float(gmisc_raw_data(ihit)),
     $                    1.)
                  endif
               endif
            endif
            if(gmisc_raw_addr2(ihit).eq.3) then !BigCal trigger: fill hist
               hittime = .5*gmisc_raw_data(ihit)
               if(abs(hittime-gep_btime_center).le.gep_btime_slop) then
                  nB = nB + 1             
                  if(nB.le.8) then
                     GEP_Btime(nB) = hittime
                  endif
                  if(gepid_gep_bigcal_rawtdc.gt.0) then
                     call hf1(gepid_gep_bigcal_rawtdc,float(gmisc_raw_data(ihit)),
     $                    1.)
                  endif
               endif
            endif
         endif

         gmisc_dec_data(gmisc_raw_addr2(ihit),gmisc_raw_addr1(ihit)) =
     $       gmisc_raw_data(ihit)
      enddo

      ntrigH1 = nH1
      ntrigH2 = nH2
      ntrigB =  nB

      return
      end
