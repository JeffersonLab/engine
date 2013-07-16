      Subroutine h_one_ev_hodo
*
* $Log: h_one_ev_hodo.f,v $
* Revision 1.3  1996/11/22 15:37:36  saw
* (SAW) Fix some startup errors
*
* Revision 1.2  1996/06/13 14:50:23  saw
* (SAW) Replace huge list of gsdet/gsdeth calls with do loops over the
* detector geometry.
*
* Revision 1.1  1995/09/18 14:38:53  cdaq
* Initial revision
*
      implicit none

      include 'hms_one_ev.par'

      integer iset, idet
      character*4 varinames(3)
      integer     varibits(3)
      integer     hodobits(3)
      real origin(3), factor(3)

      integer ihod,iplane,ipaddle

      character*4 specname
      character*4 hodoname(3)
      character*1 planenames(2)
      integer nhods,nplanes,npaddles(4)

      data specname /'HMS'/
      data planenames /'X','Y'/
      data nhods,nplanes,npaddles /2,2,16,10,16,10/

      data varinames /'x', 'y', 'z'/
      data varibits /32, 32, 32/
      data hodobits /2,2,5/
      data origin /HHUT_HEIGHT, HHUT_HEIGHT, HHUT_HEIGHT/
      data factor /1e3, 1e3, 1e3/

      do ihod=1,nhods
        write (hodoname(1),'("HOD",i1)') ihod
        do iplane=1,nplanes
          write (hodoname(2),'("HD",a1,i1)') planenames(iplane),ihod
          do ipaddle=1,npaddles((ihod-1)*2+iplane)
            write (hodoname(3),'("H",i1,a1,a1)') ihod,planenames(iplane)
     $           ,char(ichar('A')+ipaddle-1)
            call gsdet(specname,hodoname(3),3,hodoname,hodobits,
     $           2,100,100,iset,idet)
            call gsdeth(specname,hodoname(3),3,varinames,varibits,
     $           origin,factor)
          enddo
        enddo
      enddo

      return
      end
