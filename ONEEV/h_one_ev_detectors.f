      subroutine h_one_ev_detectors
*
* Define geant detector sets
*
* August, 1994, Pat Welch, Oregon State University, tpw@physics.orst.edu
*
* $Log$
* Revision 1.1  1995/03/14 21:26:57  cdaq
* Initial revision
*

      implicit none

      include 'hms_one_ev.par'

      integer iset, idet

      character*4 shoname(2) /'LAYE', 'BLOC'/
      integer     shobits(2) / 4, 4/
      
      character*4 hodoxname(3) /'HODO', 'HODX', 'HOXS'/
      character*4 hodoyname(3) /'HODO', 'HODY', 'HOYS'/
      integer     hodobits(3) /2, 2, 5/

      character*4 chamxname(3) /'WCHA', 'WCXP', 'WCWX'/
      character*4 chamyname(3) /'WCHA', 'WCYP', 'WCWY'/
      character*4 chamuname(3) /'WCHA', 'WCUP', 'WCWU'/
      character*4 chamvname(3) /'WCHA', 'WCVP', 'WCWV'/
      integer     chambits(3) /2, 3, 8/

      character*4 varinames(3) /'x', 'y', 'z'/
      integer     varibits(3)  /32, 32, 32/
      real origin(3) /HHUT_HEIGHT, HHUT_HEIGHT, HHUT_HEIGHT/
      real factor(3) /1e3, 1e3, 1e3/

      call gsdet ('HMS ', 'BLOC', 2, shoname, shobits, 2,
     >     100, 100, iset, idet)
      call gsdeth('HMS ', 'BLOC', 3, varinames, varibits, 
     >     origin, factor)

      call gsdet ('HMS ', 'HOXS', 3, hodoxname, hodobits, 
     >     2, 100, 100, iset, idet) 
      call gsdeth('HMS ', 'HOXS', 3, varinames, varibits, 
     >     origin, factor)
      call gsdet ('HMS ', 'HOYS', 3, hodoyname, hodobits, 
     >     2, 100, 100, iset, idet) 
      call gsdeth('HMS ', 'HOYS', 3, varinames, varibits, 
     >     origin, factor)

      call gsdet ('HMS ', 'WCWX', 3, chamxname, chambits, 
     >     2, 100, 100, iset, idet) 
      call gsdeth('HMS ', 'WCWX', 3, varinames, varibits, 
     >     origin, factor)
      call gsdet ('HMS ', 'WCWY', 3, chamyname, chambits, 
     >     2, 100, 100, iset, idet) 
      call gsdeth('HMS ', 'WCWY', 3, varinames, varibits, 
     >     origin, factor)
      call gsdet ('HMS ', 'WCWU', 3, chamuname, chambits, 
     >     2, 100, 100, iset, idet) 
      call gsdeth('HMS ', 'WCWU', 3, varinames, varibits, 
     >     origin, factor)
      call gsdet ('HMS ', 'WCWV', 3, chamvname, chambits, 
     >     2, 100, 100, iset, idet) 
      call gsdeth('HMS ', 'WCWV', 3, varinames, varibits, 
     >     origin, factor)

      end
