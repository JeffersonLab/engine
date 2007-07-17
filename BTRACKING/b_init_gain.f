      subroutine b_init_gain(ABORT,err)

      implicit none
      save
      
      character*11 here
      parameter(here='b_init_gain')

      logical ABORT
      character*(*) err

      include 'bigcal_data_structures.cmn'
      include 'bigcal_gain_parms.cmn'

      integer ix,iy,icell,ig64,ih64,ilogic
      real gainold,gainnew

      abort=.false.
      err=' '

c     calculate gain correction factors. hopefully last and current gain factors
c     are correctly read in from CTP parm files

c$$$      do ix=1,BIGCAL_PROT_NX
c$$$         do iy=1,BIGCAL_PROT_NY
c$$$            icell=ix + (iy-1)*BIGCAL_PROT_NX
c$$$           
c$$$            gainold = bigcal_prot_gain_last(icell)
c$$$            gainnew = bigcal_prot_gain_now(icell)
c$$$
c$$$            bigcal_prot_gain_cor(icell) = gainnew / gainold
c$$$
c$$$         enddo
c$$$      enddo
c$$$
c$$$      do ix=1,BIGCAL_RCS_NX
c$$$         do iy=1,BIGCAL_RCS_NY
c$$$            icell = ix + (iy-1)*BIGCAL_RCS_NX
c$$$
c$$$            gainold = bigcal_rcs_gain_last(icell)
c$$$            gainnew = bigcal_rcs_gain_now(icell)
c$$$
c$$$            bigcal_rcs_gain_cor(icell) = gainnew / gainold
c$$$            
c$$$         enddo
c$$$      enddo
c$$$
c$$$      do ilogic=1,BIGCAL_ATRIG_MAXHITS
c$$$         gainold = bigcal_trig_gain_last(ilogic)
c$$$         gainnew = bigcal_trig_gain_now(ilogic)
c$$$         bigcal_trig_gain_cor(ilogic) = gainnew / gainold
c$$$      enddo

c     Decided gain_last, gain_now are redundant. Just use cfac and gain_cor!!!!

      return 
      end
