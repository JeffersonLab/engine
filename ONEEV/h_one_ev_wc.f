      subroutine h_one_ev_wc
*
* $Log$
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

      character*10 chambername
      character*4 wire
      integer ichamber
      integer isector
      integer iwire
      integer iplane
      
      character*4 specname
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
        write (44,'(a,i1)') "      subroutine h_one_ev_wc",ichamber
        do iplane=1,nplanes
          do isector=1,sectors(iplane)
            do iwire = 1,wires(iplane)
              write (wire,'(a,a,a,a)') char(64 + ichamber)
     $             ,planenames(iplane),char(64 + isector)
     $             ,char(64 + iwire)
              write (chambername,'(a,a,a)') 'WC',wire,'NAME'
              call gsdet(specname,wire,3,chambername,chambits,
     $             2, 100, 100, iset, idet)
              call gsdeth(specname,wire,varinames,varibits,origin,factor)
            enddo
          enddo
        enddo
      enddo

      return
      end

