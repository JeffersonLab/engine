      subroutine s_one_ev_wc
*
* $Log: s_one_ev_wc.f,v $
* Revision 1.1  1996/01/17 16:38:04  cdaq
* Initial revision
*

      implicit none
      include 'sos_one_ev.par'

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

      data specname,nchambers /'SOS ',2/
      data nplanes /3/
      data planenames /'U','X','V',' '/
      data sectors /4,8,4,0/
      data wires /24,16,24,0/

      data varinames /'x','y','z'/
      data chambits /2,3,8/

      do i=1,3
        varibits(i)=32
        origin(i)=SHUT_HEIGHT
        factor(i)=1e3
      enddo

      do ichamber=1,nchambers
        write (wcname(1),'("WCH",a1)') char(64 + ichamber)
        do iplane=1,nplanes
          do isector=1,sectors(iplane)
            sectorthingy = 'A'
            if(isector.gt.sectors(iplane)/2) then
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
