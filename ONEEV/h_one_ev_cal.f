      subroutine h_one_ev_cal
*
* $Log$
* Revision 1.1  1995/09/18 13:48:10  cdaq
* Initial revision
*

      implicit none

      include 'hms_one_ev.par'

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
      character*4 shoname1L(2) /'LAY1', 'BL1L'/
      character*4 shoname1M(2) /'LAY1', 'BL1M'/

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
      character*4 shoname2L(2) /'LAY2', 'BL2L'/
      character*4 shoname2M(2) /'LAY2', 'BL2M'/

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
      character*4 shoname3L(2) /'LAY3', 'BL3L'/
      character*4 shoname3M(2) /'LAY3', 'BL3M'/

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
      character*4 shoname4L(2) /'LAY4', 'BL4L'/
      character*4 shoname4M(2) /'LAY4', 'BL4M'/

      integer     shobits(2) / 4, 4/

      integer iset, idet

      character*4 varinames(3) /'x', 'y', 'z'/
      integer     varibits(3)  /32, 32, 32/
      real origin(3) /HHUT_HEIGHT, HHUT_HEIGHT, HHUT_HEIGHT/
      real factor(3) /1e3, 1e3, 1e3/

      call gsdet ('HMS ', 'BL1A', 2, shoname1A, shobits, 2,
     >     100, 100, iset, idet)
      call gsdeth('HMS ', 'BL1A', 3, varinames, varibits, 
     >     origin, factor)
      call gsdet ('HMS ', 'BL1B', 2, shoname1B, shobits, 2,
     >     100, 100, iset, idet)
      call gsdeth('HMS ', 'BL1B', 3, varinames, varibits, 
     >     origin, factor)
      call gsdet ('HMS ', 'BL1C', 2, shoname1C, shobits, 2,
     >     100, 100, iset, idet)
      call gsdeth('HMS ', 'BL1C', 3, varinames, varibits, 
     >     origin, factor)
      call gsdet ('HMS ', 'BL1D', 2, shoname1D, shobits, 2,
     >     100, 100, iset, idet)
      call gsdeth('HMS ', 'BL1D', 3, varinames, varibits, 
     >     origin, factor)
      call gsdet ('HMS ', 'BL1E', 2, shoname1E, shobits, 2,
     >     100, 100, iset, idet)
      call gsdeth('HMS ', 'BL1E', 3, varinames, varibits, 
     >     origin, factor)
      call gsdet ('HMS ', 'BL1F', 2, shoname1F, shobits, 2,
     >     100, 100, iset, idet)
      call gsdeth('HMS ', 'BL1F', 3, varinames, varibits, 
     >     origin, factor)
      call gsdet ('HMS ', 'BL1G', 2, shoname1G, shobits, 2,
     >     100, 100, iset, idet)
      call gsdeth('HMS ', 'BL1G', 3, varinames, varibits, 
     >     origin, factor)
      call gsdet ('HMS ', 'BL1H', 2, shoname1H, shobits, 2,
     >     100, 100, iset, idet)
      call gsdeth('HMS ', 'BL1H', 3, varinames, varibits, 
     >     origin, factor)
      call gsdet ('HMS ', 'BL1I', 2, shoname1I, shobits, 2,
     >     100, 100, iset, idet)
      call gsdeth('HMS ', 'BL1I', 3, varinames, varibits, 
     >     origin, factor)
      call gsdet ('HMS ', 'BL1J', 2, shoname1J, shobits, 2,
     >     100, 100, iset, idet)
      call gsdeth('HMS ', 'BL1J', 3, varinames, varibits, 
     >     origin, factor)
      call gsdet ('HMS ', 'BL1K', 2, shoname1K, shobits, 2,
     >     100, 100, iset, idet)
      call gsdeth('HMS ', 'BL1K', 3, varinames, varibits, 
     >     origin, factor)
      call gsdet ('HMS ', 'BL1L', 2, shoname1L, shobits, 2,
     >     100, 100, iset, idet)
      call gsdeth('HMS ', 'BL1L', 3, varinames, varibits, 
     >     origin, factor)
      call gsdet ('HMS ', 'BL1M', 2, shoname1M, shobits, 2,
     >     100, 100, iset, idet)
      call gsdeth('HMS ', 'BL1M', 3, varinames, varibits, 
     >     origin, factor)

      call gsdet ('HMS ', 'BL2A', 2, shoname2A, shobits, 2,
     >     100, 100, iset, idet)
      call gsdeth('HMS ', 'BL2A', 3, varinames, varibits, 
     >     origin, factor)
      call gsdet ('HMS ', 'BL2B', 2, shoname2B, shobits, 2,
     >     100, 100, iset, idet)
      call gsdeth('HMS ', 'BL2B', 3, varinames, varibits, 
     >     origin, factor)
      call gsdet ('HMS ', 'BL2C', 2, shoname2C, shobits, 2,
     >     100, 100, iset, idet)
      call gsdeth('HMS ', 'BL2C', 3, varinames, varibits, 
     >     origin, factor)
      call gsdet ('HMS ', 'BL2D', 2, shoname2D, shobits, 2,
     >     100, 100, iset, idet)
      call gsdeth('HMS ', 'BL2D', 3, varinames, varibits, 
     >     origin, factor)
      call gsdet ('HMS ', 'BL2E', 2, shoname2E, shobits, 2,
     >     100, 100, iset, idet)
      call gsdeth('HMS ', 'BL2E', 3, varinames, varibits, 
     >     origin, factor)
      call gsdet ('HMS ', 'BL2F', 2, shoname2F, shobits, 2,
     >     100, 100, iset, idet)
      call gsdeth('HMS ', 'BL2F', 3, varinames, varibits, 
     >     origin, factor)
      call gsdet ('HMS ', 'BL2G', 2, shoname2G, shobits, 2,
     >     100, 100, iset, idet)
      call gsdeth('HMS ', 'BL2G', 3, varinames, varibits, 
     >     origin, factor)
      call gsdet ('HMS ', 'BL2H', 2, shoname2H, shobits, 2,
     >     100, 100, iset, idet)
      call gsdeth('HMS ', 'BL2H', 3, varinames, varibits, 
     >     origin, factor)
      call gsdet ('HMS ', 'BL2I', 2, shoname2I, shobits, 2,
     >     100, 100, iset, idet)
      call gsdeth('HMS ', 'BL2I', 3, varinames, varibits, 
     >     origin, factor)
      call gsdet ('HMS ', 'BL2J', 2, shoname2J, shobits, 2,
     >     100, 100, iset, idet)
      call gsdeth('HMS ', 'BL2J', 3, varinames, varibits, 
     >     origin, factor)
      call gsdet ('HMS ', 'BL2K', 2, shoname2K, shobits, 2,
     >     100, 100, iset, idet)
      call gsdeth('HMS ', 'BL2K', 3, varinames, varibits, 
     >     origin, factor)
      call gsdet ('HMS ', 'BL2L', 2, shoname2L, shobits, 2,
     >     100, 100, iset, idet)
      call gsdeth('HMS ', 'BL2L', 3, varinames, varibits, 
     >     origin, factor)
      call gsdet ('HMS ', 'BL2M', 2, shoname2M, shobits, 2,
     >     100, 100, iset, idet)
      call gsdeth('HMS ', 'BL2M', 3, varinames, varibits, 
     >     origin, factor)

      call gsdet ('HMS ', 'BL3A', 2, shoname3A, shobits, 2,
     >     100, 100, iset, idet)
      call gsdeth('HMS ', 'BL3A', 3, varinames, varibits, 
     >     origin, factor)
      call gsdet ('HMS ', 'BL3B', 2, shoname3B, shobits, 2,
     >     100, 100, iset, idet)
      call gsdeth('HMS ', 'BL3B', 3, varinames, varibits, 
     >     origin, factor)
      call gsdet ('HMS ', 'BL3C', 2, shoname3C, shobits, 2,
     >     100, 100, iset, idet)
      call gsdeth('HMS ', 'BL3C', 3, varinames, varibits, 
     >     origin, factor)
      call gsdet ('HMS ', 'BL3D', 2, shoname3D, shobits, 2,
     >     100, 100, iset, idet)
      call gsdeth('HMS ', 'BL3D', 3, varinames, varibits, 
     >     origin, factor)
      call gsdet ('HMS ', 'BL3E', 2, shoname3E, shobits, 2,
     >     100, 100, iset, idet)
      call gsdeth('HMS ', 'BL3E', 3, varinames, varibits, 
     >     origin, factor)
      call gsdet ('HMS ', 'BL3F', 2, shoname3F, shobits, 2,
     >     100, 100, iset, idet)
      call gsdeth('HMS ', 'BL3F', 3, varinames, varibits, 
     >     origin, factor)
      call gsdet ('HMS ', 'BL3G', 2, shoname3G, shobits, 2,
     >     100, 100, iset, idet)
      call gsdeth('HMS ', 'BL3G', 3, varinames, varibits, 
     >     origin, factor)
      call gsdet ('HMS ', 'BL3H', 2, shoname3H, shobits, 2,
     >     100, 100, iset, idet)
      call gsdeth('HMS ', 'BL3H', 3, varinames, varibits, 
     >     origin, factor)
      call gsdet ('HMS ', 'BL3I', 2, shoname3I, shobits, 2,
     >     100, 100, iset, idet) 
      call gsdeth('HMS ', 'BL3I', 3, varinames, varibits, 
     >     origin, factor)
      call gsdet ('HMS ', 'BL3J', 2, shoname3J, shobits, 2,
     >     100, 100, iset, idet)
      call gsdeth('HMS ', 'BL3J', 3, varinames, varibits, 
     >     origin, factor)
      call gsdet ('HMS ', 'BL3K', 2, shoname3K, shobits, 2,
     >     100, 100, iset, idet)
      call gsdeth('HMS ', 'BL3K', 3, varinames, varibits, 
     >     origin, factor)
      call gsdet ('HMS ', 'BL3L', 2, shoname3L, shobits, 2,
     >     100, 100, iset, idet)
      call gsdeth('HMS ', 'BL3L', 3, varinames, varibits, 
     >     origin, factor)
      call gsdet ('HMS ', 'BL3M', 2, shoname3M, shobits, 2,
     >     100, 100, iset, idet)
      call gsdeth('HMS ', 'BL3M', 3, varinames, varibits, 
     >     origin, factor)

      call gsdet ('HMS ', 'BL4A', 2, shoname4A, shobits, 2,
     >     100, 100, iset, idet)
      call gsdeth('HMS ', 'BL4A', 3, varinames, varibits, 
     >     origin, factor)
      call gsdet ('HMS ', 'BL4B', 2, shoname4B, shobits, 2,
     >     100, 100, iset, idet)
      call gsdeth('HMS ', 'BL4B', 3, varinames, varibits, 
     >     origin, factor)
      call gsdet ('HMS ', 'BL4C', 2, shoname4C, shobits, 2,
     >     100, 100, iset, idet)
      call gsdeth('HMS ', 'BL4C', 3, varinames, varibits, 
     >     origin, factor)
      call gsdet ('HMS ', 'BL4D', 2, shoname4D, shobits, 2,
     >     100, 100, iset, idet)
      call gsdeth('HMS ', 'BL4D', 3, varinames, varibits, 
     >     origin, factor)
      call gsdet ('HMS ', 'BL4E', 2, shoname4E, shobits, 2,
     >     100, 100, iset, idet)
      call gsdeth('HMS ', 'BL4E', 3, varinames, varibits, 
     >     origin, factor)
      call gsdet ('HMS ', 'BL4F', 2, shoname4F, shobits, 2,
     >     100, 100, iset, idet)
      call gsdeth('HMS ', 'BL4F', 3, varinames, varibits, 
     >     origin, factor)
      call gsdet ('HMS ', 'BL4G', 2, shoname4G, shobits, 2,
     >     100, 100, iset, idet)
      call gsdeth('HMS ', 'BL4G', 3, varinames, varibits, 
     >     origin, factor)
      call gsdet ('HMS ', 'BL4H', 2, shoname4H, shobits, 2,
     >     100, 100, iset, idet)
      call gsdeth('HMS ', 'BL4H', 3, varinames, varibits, 
     >     origin, factor)
      call gsdet ('HMS ', 'BL4I', 2, shoname4I, shobits, 2,
     >     100, 100, iset, idet)
      call gsdeth('HMS ', 'BL4I', 3, varinames, varibits, 
     >     origin, factor)
      call gsdet ('HMS ', 'BL4J', 2, shoname4J, shobits, 2,
     >     100, 100, iset, idet)
      call gsdeth('HMS ', 'BL4J', 3, varinames, varibits, 
     >     origin, factor)
      call gsdet ('HMS ', 'BL4K', 2, shoname4K, shobits, 2,
     >     100, 100, iset, idet)
      call gsdeth('HMS ', 'BL4K', 3, varinames, varibits, 
     >     origin, factor)
      call gsdet ('HMS ', 'BL4L', 2, shoname4L, shobits, 2,
     >     100, 100, iset, idet)
      call gsdeth('HMS ', 'BL4L', 3, varinames, varibits, 
     >     origin, factor)
      call gsdet ('HMS ', 'BL4M', 2, shoname4M, shobits, 2,
     >     100, 100, iset, idet)
      call gsdeth('HMS ', 'BL4M', 3, varinames, varibits, 
     >     origin, factor)

      end
