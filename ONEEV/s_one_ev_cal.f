      subroutine s_one_ev_cal
*
* $Log$
* Revision 1.1  1995/09/18 14:37:33  cdaq
* Initial revision
*

      implicit none

      include 'sos_one_ev.par'

      character*4 shoname1A(2) /'LAY1', 'BL1A'/
      character*4 shoname1B(2) /'LAY1', 'BL1B'/
      character*4 shoname1C(2) /'LAY1', 'BL1C'/
      character*4 shoname1D(2) /'LAY1', 'BL1D'/
      character*4 shoname1E(2) /'LAY1', 'BL1E'/
      character*4 shoname1F(2) /'LAY1', 'BL1F'/
      character*4 shoname1G(2) /'LAY1', 'BL1G'/
      character*4 shoname1H(2) /'LAY1', 'BL1H'/
      character*4 shoname1I(2) /'LAY1', 'BL1I'/
      character*4 shoname1J(2) /'LAY1', 'BL1J'/
      character*4 shoname1K(2) /'LAY1', 'BL1K'/

      character*4 shoname2A(2) /'LAY2', 'BL2A'/
      character*4 shoname2B(2) /'LAY2', 'BL2B'/
      character*4 shoname2C(2) /'LAY2', 'BL2C'/
      character*4 shoname2D(2) /'LAY2', 'BL2D'/
      character*4 shoname2E(2) /'LAY2', 'BL2E'/
      character*4 shoname2F(2) /'LAY2', 'BL2F'/
      character*4 shoname2G(2) /'LAY2', 'BL2G'/
      character*4 shoname2H(2) /'LAY2', 'BL2H'/
      character*4 shoname2I(2) /'LAY2', 'BL2I'/
      character*4 shoname2J(2) /'LAY2', 'BL2J'/
      character*4 shoname2K(2) /'LAY2', 'BL2K'/

      character*4 shoname3A(2) /'LAY3', 'BL3A'/
      character*4 shoname3B(2) /'LAY3', 'BL3B'/
      character*4 shoname3C(2) /'LAY3', 'BL3C'/
      character*4 shoname3D(2) /'LAY3', 'BL3D'/
      character*4 shoname3E(2) /'LAY3', 'BL3E'/
      character*4 shoname3F(2) /'LAY3', 'BL3F'/
      character*4 shoname3G(2) /'LAY3', 'BL3G'/
      character*4 shoname3H(2) /'LAY3', 'BL3H'/
      character*4 shoname3I(2) /'LAY3', 'BL3I'/
      character*4 shoname3J(2) /'LAY3', 'BL3J'/
      character*4 shoname3K(2) /'LAY3', 'BL3K'/

      character*4 shoname4A(2) /'LAY4', 'BL4A'/
      character*4 shoname4B(2) /'LAY4', 'BL4B'/
      character*4 shoname4C(2) /'LAY4', 'BL4C'/
      character*4 shoname4D(2) /'LAY4', 'BL4D'/
      character*4 shoname4E(2) /'LAY4', 'BL4E'/
      character*4 shoname4F(2) /'LAY4', 'BL4F'/
      character*4 shoname4G(2) /'LAY4', 'BL4G'/
      character*4 shoname4H(2) /'LAY4', 'BL4H'/
      character*4 shoname4I(2) /'LAY4', 'BL4I'/
      character*4 shoname4J(2) /'LAY4', 'BL4J'/
      character*4 shoname4K(2) /'LAY4', 'BL4K'/

      integer     shobits(2) / 4, 4/

      integer iset, idet

      character*4 varinames(3) /'x', 'y', 'z'/
      integer     varibits(3)  /32, 32, 32/
      real origin(3) /SHUT_HEIGHT, SHUT_HEIGHT, SHUT_HEIGHT/
      real factor(3) /1e3, 1e3, 1e3/

      call gsdet ('SOS ', 'BL1A', 2, shoname1A, shobits, 2,
     >     100, 100, iset, idet)
      call gsdeth('SOS ', 'BL1A', 3, varinames, varibits, 
     >     origin, factor)
      call gsdet ('SOS ', 'BL1B', 2, shoname1B, shobits, 2,
     >     100, 100, iset, idet)
      call gsdeth('SOS ', 'BL1B', 3, varinames, varibits, 
     >     origin, factor)
      call gsdet ('SOS ', 'BL1C', 2, shoname1C, shobits, 2,
     >     100, 100, iset, idet)
      call gsdeth('SOS ', 'BL1C', 3, varinames, varibits, 
     >     origin, factor)
      call gsdet ('SOS ', 'BL1D', 2, shoname1D, shobits, 2,
     >     100, 100, iset, idet)
      call gsdeth('SOS ', 'BL1D', 3, varinames, varibits, 
     >     origin, factor)
      call gsdet ('SOS ', 'BL1E', 2, shoname1E, shobits, 2,
     >     100, 100, iset, idet)
      call gsdeth('SOS ', 'BL1E', 3, varinames, varibits, 
     >     origin, factor)
      call gsdet ('SOS ', 'BL1F', 2, shoname1F, shobits, 2,
     >     100, 100, iset, idet)
      call gsdeth('SOS ', 'BL1F', 3, varinames, varibits, 
     >     origin, factor)
      call gsdet ('SOS ', 'BL1G', 2, shoname1G, shobits, 2,
     >     100, 100, iset, idet)
      call gsdeth('SOS ', 'BL1G', 3, varinames, varibits, 
     >     origin, factor)
      call gsdet ('SOS ', 'BL1H', 2, shoname1H, shobits, 2,
     >     100, 100, iset, idet)
      call gsdeth('SOS ', 'BL1H', 3, varinames, varibits, 
     >     origin, factor)
      call gsdet ('SOS ', 'BL1I', 2, shoname1I, shobits, 2,
     >     100, 100, iset, idet)
      call gsdeth('SOS ', 'BL1I', 3, varinames, varibits, 
     >     origin, factor)
      call gsdet ('SOS ', 'BL1J', 2, shoname1J, shobits, 2,
     >     100, 100, iset, idet)
      call gsdeth('SOS ', 'BL1J', 3, varinames, varibits, 
     >     origin, factor)
      call gsdet ('SOS ', 'BL1K', 2, shoname1K, shobits, 2,
     >     100, 100, iset, idet)
      call gsdeth('SOS ', 'BL1K', 3, varinames, varibits, 
     >     origin, factor)

      call gsdet ('SOS ', 'BL2A', 2, shoname2A, shobits, 2,
     >     100, 100, iset, idet)
      call gsdeth('SOS ', 'BL2A', 3, varinames, varibits, 
     >     origin, factor)
      call gsdet ('SOS ', 'BL2B', 2, shoname2B, shobits, 2,
     >     100, 100, iset, idet)
      call gsdeth('SOS ', 'BL2B', 3, varinames, varibits, 
     >     origin, factor)
      call gsdet ('SOS ', 'BL2C', 2, shoname2C, shobits, 2,
     >     100, 100, iset, idet)
      call gsdeth('SOS ', 'BL2C', 3, varinames, varibits, 
     >     origin, factor)
      call gsdet ('SOS ', 'BL2D', 2, shoname2D, shobits, 2,
     >     100, 100, iset, idet)
      call gsdeth('SOS ', 'BL2D', 3, varinames, varibits, 
     >     origin, factor)
      call gsdet ('SOS ', 'BL2E', 2, shoname2E, shobits, 2,
     >     100, 100, iset, idet)
      call gsdeth('SOS ', 'BL2E', 3, varinames, varibits, 
     >     origin, factor)
      call gsdet ('SOS ', 'BL2F', 2, shoname2F, shobits, 2,
     >     100, 100, iset, idet)
      call gsdeth('SOS ', 'BL2F', 3, varinames, varibits, 
     >     origin, factor)
      call gsdet ('SOS ', 'BL2G', 2, shoname2G, shobits, 2,
     >     100, 100, iset, idet)
      call gsdeth('SOS ', 'BL2G', 3, varinames, varibits, 
     >     origin, factor)
      call gsdet ('SOS ', 'BL2H', 2, shoname2H, shobits, 2,
     >     100, 100, iset, idet)
      call gsdeth('SOS ', 'BL2H', 3, varinames, varibits, 
     >     origin, factor)
      call gsdet ('SOS ', 'BL2I', 2, shoname2I, shobits, 2,
     >     100, 100, iset, idet)
      call gsdeth('SOS ', 'BL2I', 3, varinames, varibits, 
     >     origin, factor)
      call gsdet ('SOS ', 'BL2J', 2, shoname2J, shobits, 2,
     >     100, 100, iset, idet)
      call gsdeth('SOS ', 'BL2J', 3, varinames, varibits, 
     >     origin, factor)
      call gsdet ('SOS ', 'BL2K', 2, shoname2K, shobits, 2,
     >     100, 100, iset, idet)
      call gsdeth('SOS ', 'BL2K', 3, varinames, varibits, 
     >     origin, factor)

      call gsdet ('SOS ', 'BL3A', 2, shoname3A, shobits, 2,
     >     100, 100, iset, idet)
      call gsdeth('SOS ', 'BL3A', 3, varinames, varibits, 
     >     origin, factor)
      call gsdet ('SOS ', 'BL3B', 2, shoname3B, shobits, 2,
     >     100, 100, iset, idet)
      call gsdeth('SOS ', 'BL3B', 3, varinames, varibits, 
     >     origin, factor)
      call gsdet ('SOS ', 'BL3C', 2, shoname3C, shobits, 2,
     >     100, 100, iset, idet)
      call gsdeth('SOS ', 'BL3C', 3, varinames, varibits, 
     >     origin, factor)
      call gsdet ('SOS ', 'BL3D', 2, shoname3D, shobits, 2,
     >     100, 100, iset, idet)
      call gsdeth('SOS ', 'BL3D', 3, varinames, varibits, 
     >     origin, factor)
      call gsdet ('SOS ', 'BL3E', 2, shoname3E, shobits, 2,
     >     100, 100, iset, idet)
      call gsdeth('SOS ', 'BL3E', 3, varinames, varibits, 
     >     origin, factor)
      call gsdet ('SOS ', 'BL3F', 2, shoname3F, shobits, 2,
     >     100, 100, iset, idet)
      call gsdeth('SOS ', 'BL3F', 3, varinames, varibits, 
     >     origin, factor)
      call gsdet ('SOS ', 'BL3G', 2, shoname3G, shobits, 2,
     >     100, 100, iset, idet)
      call gsdeth('SOS ', 'BL3G', 3, varinames, varibits, 
     >     origin, factor)
      call gsdet ('SOS ', 'BL3H', 2, shoname3H, shobits, 2,
     >     100, 100, iset, idet)
      call gsdeth('SOS ', 'BL3H', 3, varinames, varibits, 
     >     origin, factor)
      call gsdet ('SOS ', 'BL3I', 2, shoname3I, shobits, 2,
     >     100, 100, iset, idet) 
      call gsdeth('SOS ', 'BL3I', 3, varinames, varibits, 
     >     origin, factor)
      call gsdet ('SOS ', 'BL3J', 2, shoname3J, shobits, 2,
     >     100, 100, iset, idet)
      call gsdeth('SOS ', 'BL3J', 3, varinames, varibits, 
     >     origin, factor)
      call gsdet ('SOS ', 'BL3K', 2, shoname3K, shobits, 2,
     >     100, 100, iset, idet)
      call gsdeth('SOS ', 'BL3K', 3, varinames, varibits, 
     >     origin, factor)

      call gsdet ('SOS ', 'BL4A', 2, shoname4A, shobits, 2,
     >     100, 100, iset, idet)
      call gsdeth('SOS ', 'BL4A', 3, varinames, varibits, 
     >     origin, factor)
      call gsdet ('SOS ', 'BL4B', 2, shoname4B, shobits, 2,
     >     100, 100, iset, idet)
      call gsdeth('SOS ', 'BL4B', 3, varinames, varibits, 
     >     origin, factor)
      call gsdet ('SOS ', 'BL4C', 2, shoname4C, shobits, 2,
     >     100, 100, iset, idet)
      call gsdeth('SOS ', 'BL4C', 3, varinames, varibits, 
     >     origin, factor)
      call gsdet ('SOS ', 'BL4D', 2, shoname4D, shobits, 2,
     >     100, 100, iset, idet)
      call gsdeth('SOS ', 'BL4D', 3, varinames, varibits, 
     >     origin, factor)
      call gsdet ('SOS ', 'BL4E', 2, shoname4E, shobits, 2,
     >     100, 100, iset, idet)
      call gsdeth('SOS ', 'BL4E', 3, varinames, varibits, 
     >     origin, factor)
      call gsdet ('SOS ', 'BL4F', 2, shoname4F, shobits, 2,
     >     100, 100, iset, idet)
      call gsdeth('SOS ', 'BL4F', 3, varinames, varibits, 
     >     origin, factor)
      call gsdet ('SOS ', 'BL4G', 2, shoname4G, shobits, 2,
     >     100, 100, iset, idet)
      call gsdeth('SOS ', 'BL4G', 3, varinames, varibits, 
     >     origin, factor)
      call gsdet ('SOS ', 'BL4H', 2, shoname4H, shobits, 2,
     >     100, 100, iset, idet)
      call gsdeth('SOS ', 'BL4H', 3, varinames, varibits, 
     >     origin, factor)
      call gsdet ('SOS ', 'BL4I', 2, shoname4I, shobits, 2,
     >     100, 100, iset, idet)
      call gsdeth('SOS ', 'BL4I', 3, varinames, varibits, 
     >     origin, factor)
      call gsdet ('SOS ', 'BL4J', 2, shoname4J, shobits, 2,
     >     100, 100, iset, idet)
      call gsdeth('SOS ', 'BL4J', 3, varinames, varibits, 
     >     origin, factor)
      call gsdet ('SOS ', 'BL4K', 2, shoname4K, shobits, 2,
     >     100, 100, iset, idet)
      call gsdeth('SOS ', 'BL4K', 3, varinames, varibits, 
     >     origin, factor)

      end
