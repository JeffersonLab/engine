      subroutine h_one_ev_wc
*
* $Log: h_one_ev_wc.f,v $
* Revision 1.2  1996/01/17 16:39:33  cdaq
* (DVW) Fixes
*
* Revision 1.1  1995/09/14 15:42:40  cdaq
* Initial revision
*

      implicit none
      include 'hms_one_ev.par'

      integer iset, idet
      character*4 varinames(3)
      integer varibits(3)
      integer chambits(3)
      real origin(3),factor(3)
      integer i

      character*4 wire
      integer ichamber
      integer isector
      integer iwire
      integer iplane
      
      character*4 specname
      character*4 wcname(3)
      character*1 sectorthingy
      integer nchambers
      integer nplanes
      character*1 planenames(4)
      integer sectors(4),wires(4)

      data specname,nchambers /'HMS ',2/
      data nplanes /4/
      data planenames /'X','Y','U','V'/
      data sectors /12,4,6,6/
      data wires /19,26,18,18/

      data varinames /'x','y','z'/
      data chambits /2,3,8/

      do i=1,3
        varibits(i)=32
        origin(i)=HHUT_HEIGHT
        factor(i)=1e3
      enddo

      do ichamber=1,nchambers
        write (wcname(1),'("WCH",a1)') char(64 + ichamber)
        do iplane=1,nplanes
          do isector=1,sectors(iplane)
            sectorthingy = 'A'
*
*     There is only one U and one V plane per chamber
*
            if(planenames(iplane).ne.'U'.and.planenames(iplane).ne.'V'
     $           .and.isector.gt.sectors(iplane)/2) then
              sectorthingy = 'B'
            endif
            write(wcname(2),'("W",3a1)')  char(64 + ichamber)
     $           ,sectorthingy,planenames(iplane)
            do iwire = 1,wires(iplane)
              write (wire,'(a,a,a,a)') char(64 + ichamber)
     $             ,planenames(iplane),char(64 + isector)
     $             ,char(64 + iwire)
              wcname(3) = wire
              call gsdet(specname,wire,3,wcname,chambits,
     $             2, 100, 100, iset, idet)
              call gsdeth(specname,wire,3,varinames,varibits,origin,factor)
            enddo
          enddo
        enddo
      enddo

      return
      end

