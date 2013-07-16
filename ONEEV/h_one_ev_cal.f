      subroutine h_one_ev_cal
*
* $Log: h_one_ev_cal.f,v $
* Revision 1.3  1996/11/22 15:36:14  saw
* (SAW) Fix some error messages at startup, some code cleanup
*
* Revision 1.2  1996/06/13 13:03:49  saw
* (SAW) Replace huge list of gsdet/gsdeth calls with do loops over the
* detector geometry.
*
* Revision 1.1  1995/09/18 13:48:10  cdaq
* Initial revision
*
      implicit none

      include 'hms_data_structures.cmn'
      include 'hms_calorimeter.cmn'
      include 'hms_one_ev.par'

      integer iset, idet
      character*4 varinames(3)
      integer varibits(3)
      integer calbits(2)
      real origin(3),factor(3)

      integer ilayer,iblock

      character*4 specname
      character*4 calname(2)

      data specname /'HMS '/

      data varinames /'x', 'y', 'z'/
      data varibits /32, 32, 32/
      data calbits /4,4/
      data origin /HHUT_HEIGHT, HHUT_HEIGHT, HHUT_HEIGHT/
      data factor /1e3, 1e3, 1e3/


      do ilayer=1,hmax_cal_columns
        write (calname(1),'("LAY",i1)') ilayer
        do iblock=1,hmax_cal_rows
          write (calname(2),'("BL",i1,a1)') ilayer, char(ichar('A')+iblock-1)
          call gsdet(specname,calname(2),2,calname,calbits,
     $         2,100,100,iset,idet)
          call gsdeth(specname,calname(2),3,varinames,varibits,origin,factor)
        enddo
      enddo

      return
      end
