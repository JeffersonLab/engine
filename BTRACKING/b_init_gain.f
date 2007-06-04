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

      do ix=1,BIGCAL_PROT_NX
         do iy=1,BIGCAL_PROT_NY
            icell=ix + (iy-1)*BIGCAL_PROT_NX
           
            gainold = bigcal_prot_gain_last(icell)
            gainnew = bigcal_prot_gain_now(icell)

            bigcal_prot_gain_cor(icell) = gainnew / gainold

         enddo
      enddo

      do ix=1,BIGCAL_RCS_NX
         do iy=1,BIGCAL_RCS_NY
            icell = ix + (iy-1)*BIGCAL_RCS_NX

            gainold = bigcal_rcs_gain_last(icell)
            gainnew = bigcal_rcs_gain_now(icell)

            bigcal_prot_gain_cor(icell) = gainnew / gainold
            
         enddo
      enddo

      do ilogic=1,BIGCAL_ATRIG_MAXHITS
         gainold = bigcal_trig_gain_last(ilogic)
         gainnew = bigcal_trig_gain_now(ilogic)
         bigcal_trig_gain_cor(ilogic) = gainnew / gainold
      enddo

      return 
      end
