      Subroutine s_one_ev_hodo
*
* $Log$
* Revision 1.1  1995/09/18 14:38:47  cdaq
* Initial revision
*
      implicit none

      include 'sos_one_ev.par'

      character*4 hodox1nameA(3) /'HOD1', 'HDX1', 'H1XA'/
      character*4 hodox1nameB(3) /'HOD1', 'HDX1', 'H1XB'/
      character*4 hodox1nameC(3) /'HOD1', 'HDX1', 'H1XC'/
      character*4 hodox1nameD(3) /'HOD1', 'HDX1', 'H1XD'/
      character*4 hodox1nameE(3) /'HOD1', 'HDX1', 'H1XE'/
      character*4 hodox1nameF(3) /'HOD1', 'HDX1', 'H1XF'/
      character*4 hodox1nameG(3) /'HOD1', 'HDX1', 'H1XG'/
      character*4 hodox1nameH(3) /'HOD1', 'HDX1', 'H1XH'/
      character*4 hodox1nameI(3) /'HOD1', 'HDX1', 'H1XI'/

      character*4 hodoy1nameA(3) /'HOD1', 'HDY1', 'H1YA'/
      character*4 hodoy1nameB(3) /'HOD1', 'HDY1', 'H1YB'/
      character*4 hodoy1nameC(3) /'HOD1', 'HDY1', 'H1YC'/
      character*4 hodoy1nameD(3) /'HOD1', 'HDY1', 'H1YD'/
      character*4 hodoy1nameE(3) /'HOD1', 'HDY1', 'H1YE'/
      character*4 hodoy1nameF(3) /'HOD1', 'HDY1', 'H1YF'/
      character*4 hodoy1nameG(3) /'HOD1', 'HDY1', 'H1YG'/
      character*4 hodoy1nameH(3) /'HOD1', 'HDY1', 'H1YH'/
      character*4 hodoy1nameI(3) /'HOD1', 'HDY1', 'H1YI'/

      character*4 hodox2nameA(3) /'HOD2', 'HDX2', 'H2XA'/
      character*4 hodox2nameB(3) /'HOD2', 'HDX2', 'H2XB'/
      character*4 hodox2nameC(3) /'HOD2', 'HDX2', 'H2XC'/
      character*4 hodox2nameD(3) /'HOD2', 'HDX2', 'H2XD'/
      character*4 hodox2nameE(3) /'HOD2', 'HDX2', 'H2XE'/
      character*4 hodox2nameF(3) /'HOD2', 'HDX2', 'H2XF'/
      character*4 hodox2nameG(3) /'HOD2', 'HDX2', 'H2XG'/
      character*4 hodox2nameH(3) /'HOD2', 'HDX2', 'H2XH'/
      character*4 hodox2nameI(3) /'HOD2', 'HDX2', 'H2XI'/
      character*4 hodox2nameJ(3) /'HOD2', 'HDX2', 'H2XJ'/
      character*4 hodox2nameK(3) /'HOD2', 'HDX2', 'H2XK'/
      character*4 hodox2nameL(3) /'HOD2', 'HDX2', 'H2XL'/
      character*4 hodox2nameM(3) /'HOD2', 'HDX2', 'H2XM'/
      character*4 hodox2nameN(3) /'HOD2', 'HDX2', 'H2XN'/
      character*4 hodox2nameO(3) /'HOD2', 'HDX2', 'H2XO'/
      character*4 hodox2nameP(3) /'HOD2', 'HDX2', 'H2XP'/

      character*4 hodoy2nameA(3) /'HOD2', 'HDY2', 'H2YA'/
      character*4 hodoy2nameB(3) /'HOD2', 'HDY2', 'H2YB'/
      character*4 hodoy2nameC(3) /'HOD2', 'HDY2', 'H2YC'/
      character*4 hodoy2nameD(3) /'HOD2', 'HDY2', 'H2YD'/
      character*4 hodoy2nameE(3) /'HOD2', 'HDY2', 'H2YE'/
      character*4 hodoy2nameF(3) /'HOD2', 'HDY2', 'H2YF'/
      character*4 hodoy2nameG(3) /'HOD2', 'HDY2', 'H2YG'/
      character*4 hodoy2nameH(3) /'HOD2', 'HDY2', 'H2YH'/
      character*4 hodoy2nameI(3) /'HOD2', 'HDY2', 'H2YI'/

      integer     hodobits(3) /2, 2, 5/

      integer iset, idet

      character*4 varinames(3) /'x', 'y', 'z'/
      integer     varibits(3)  /32, 32, 32/
      real origin(3) /SHUT_HEIGHT, SHUT_HEIGHT, SHUT_HEIGHT/
      real factor(3) /1e3, 1e3, 1e3/

      call gsdet ('SOS ', 'H1XA', 3, hodox1nameA, hodobits, 
     >     2, 100, 100, iset, idet) 
      call gsdeth('SOS ', 'H1XA', 3, varinames, varibits, 
     >     origin, factor)
      call gsdet ('SOS ', 'H1XB', 3, hodox1nameB, hodobits, 
     >     2, 100, 100, iset, idet) 
      call gsdeth('SOS ', 'H1XB', 3, varinames, varibits, 
     >     origin, factor)
      call gsdet ('SOS ', 'H1XC', 3, hodox1nameC, hodobits, 
     >     2, 100, 100, iset, idet) 
      call gsdeth('SOS ', 'H1XC', 3, varinames, varibits, 
     >     origin, factor)
      call gsdet ('SOS ', 'H1XD', 3, hodox1nameD, hodobits, 
     >     2, 100, 100, iset, idet) 
      call gsdeth('SOS ', 'H1XD', 3, varinames, varibits, 
     >     origin, factor)
      call gsdet ('SOS ', 'H1XE', 3, hodox1nameE, hodobits, 
     >     2, 100, 100, iset, idet) 
      call gsdeth('SOS ', 'H1XE', 3, varinames, varibits, 
     >     origin, factor)
      call gsdet ('SOS ', 'H1XF', 3, hodox1nameF, hodobits, 
     >     2, 100, 100, iset, idet) 
      call gsdeth('SOS ', 'H1XF', 3, varinames, varibits, 
     >     origin, factor)
      call gsdet ('SOS ', 'H1XG', 3, hodox1nameG, hodobits, 
     >     2, 100, 100, iset, idet) 
      call gsdeth('SOS ', 'H1XG', 3, varinames, varibits, 
     >     origin, factor)
      call gsdet ('SOS ', 'H1XH', 3, hodox1nameH, hodobits, 
     >     2, 100, 100, iset, idet) 
      call gsdeth('SOS ', 'H1XH', 3, varinames, varibits, 
     >     origin, factor)
      call gsdet ('SOS ', 'H1XI', 3, hodox1nameI, hodobits, 
     >     2, 100, 100, iset, idet) 
      call gsdeth('SOS ', 'H1XI', 3, varinames, varibits, 
     >     origin, factor)
*****************************************************************
      call gsdet ('SOS ', 'H1YA', 3, hodoy1nameA, hodobits, 
     >     2, 100, 100, iset, idet) 
      call gsdeth('SOS ', 'H1YA', 3, varinames, varibits, 
     >     origin, factor)
      call gsdet ('SOS ', 'H1YB', 3, hodoy1nameB, hodobits, 
     >     2, 100, 100, iset, idet) 
      call gsdeth('SOS ', 'H1YB', 3, varinames, varibits, 
     >     origin, factor)
      call gsdet ('SOS ', 'H1YC', 3, hodoy1nameC, hodobits, 
     >     2, 100, 100, iset, idet) 
      call gsdeth('SOS ', 'H1YC', 3, varinames, varibits, 
     >     origin, factor)
      call gsdet ('SOS ', 'H1YD', 3, hodoy1nameD, hodobits, 
     >     2, 100, 100, iset, idet) 
      call gsdeth('SOS ', 'H1YD', 3, varinames, varibits, 
     >     origin, factor)
      call gsdet ('SOS ', 'H1YE', 3, hodoy1nameE, hodobits, 
     >     2, 100, 100, iset, idet) 
      call gsdeth('SOS ', 'H1YE', 3, varinames, varibits, 
     >     origin, factor)
      call gsdet ('SOS ', 'H1YF', 3, hodoy1nameF, hodobits, 
     >     2, 100, 100, iset, idet) 
      call gsdeth('SOS ', 'H1YF', 3, varinames, varibits, 
     >     origin, factor)
      call gsdet ('SOS ', 'H1YG', 3, hodoy1nameG, hodobits, 
     >     2, 100, 100, iset, idet) 
      call gsdeth('SOS ', 'H1YG', 3, varinames, varibits, 
     >     origin, factor)
      call gsdet ('SOS ', 'H1YH', 3, hodoy1nameH, hodobits, 
     >     2, 100, 100, iset, idet) 
      call gsdeth('SOS ', 'H1YH', 3, varinames, varibits, 
     >     origin, factor)
      call gsdet ('SOS ', 'H1YI', 3, hodoy1nameI, hodobits, 
     >     2, 100, 100, iset, idet) 
      call gsdeth('SOS ', 'H1YI', 3, varinames, varibits, 
     >     origin, factor)
*****************************************************************
      call gsdet ('SOS ', 'H2XA', 3, hodox2nameA, hodobits, 
     >     2, 100, 100, iset, idet) 
      call gsdeth('SOS ', 'H2XA', 3, varinames, varibits, 
     >     origin, factor)
      call gsdet ('SOS ', 'H2XB', 3, hodox2nameB, hodobits, 
     >     2, 100, 100, iset, idet) 
      call gsdeth('SOS ', 'H2XB', 3, varinames, varibits, 
     >     origin, factor)
      call gsdet ('SOS ', 'H2XC', 3, hodox2nameC, hodobits, 
     >     2, 100, 100, iset, idet) 
      call gsdeth('SOS ', 'H2XC', 3, varinames, varibits, 
     >     origin, factor)
      call gsdet ('SOS ', 'H2XD', 3, hodox2nameD, hodobits, 
     >     2, 100, 100, iset, idet) 
      call gsdeth('SOS ', 'H2XD', 3, varinames, varibits, 
     >     origin, factor)
      call gsdet ('SOS ', 'H2XE', 3, hodox2nameE, hodobits, 
     >     2, 100, 100, iset, idet) 
      call gsdeth('SOS ', 'H2XE', 3, varinames, varibits, 
     >     origin, factor)
      call gsdet ('SOS ', 'H2XF', 3, hodox2nameF, hodobits, 
     >     2, 100, 100, iset, idet) 
      call gsdeth('SOS ', 'H2XF', 3, varinames, varibits, 
     >     origin, factor)
      call gsdet ('SOS ', 'H2XG', 3, hodox2nameG, hodobits, 
     >     2, 100, 100, iset, idet) 
      call gsdeth('SOS ', 'H2XG', 3, varinames, varibits, 
     >     origin, factor)
      call gsdet ('SOS ', 'H2XH', 3, hodox2nameH, hodobits, 
     >     2, 100, 100, iset, idet) 
      call gsdeth('SOS ', 'H2XH', 3, varinames, varibits, 
     >     origin, factor)
      call gsdet ('SOS ', 'H2XI', 3, hodox2nameI, hodobits, 
     >     2, 100, 100, iset, idet) 
      call gsdeth('SOS ', 'H2XI', 3, varinames, varibits, 
     >     origin, factor)
      call gsdet ('SOS ', 'H2XJ', 3, hodox2nameJ, hodobits, 
     >     2, 100, 100, iset, idet) 
      call gsdeth('SOS ', 'H2XJ', 3, varinames, varibits, 
     >     origin, factor)
      call gsdet ('SOS ', 'H2XK', 3, hodox2nameK, hodobits, 
     >     2, 100, 100, iset, idet) 
      call gsdeth('SOS ', 'H2XK', 3, varinames, varibits, 
     >     origin, factor)
      call gsdet ('SOS ', 'H2XL', 3, hodox2nameL, hodobits, 
     >     2, 100, 100, iset, idet) 
      call gsdeth('SOS ', 'H2XL', 3, varinames, varibits, 
     >     origin, factor)
      call gsdet ('SOS ', 'H2XM', 3, hodox2nameM, hodobits, 
     >     2, 100, 100, iset, idet) 
      call gsdeth('SOS ', 'H2XM', 3, varinames, varibits, 
     >     origin, factor)
      call gsdet ('SOS ', 'H2XN', 3, hodox2nameN, hodobits, 
     >     2, 100, 100, iset, idet) 
      call gsdeth('SOS ', 'H2XN', 3, varinames, varibits, 
     >     origin, factor)
      call gsdet ('SOS ', 'H2XO', 3, hodox2nameO, hodobits, 
     >     2, 100, 100, iset, idet) 
      call gsdeth('SOS ', 'H2XO', 3, varinames, varibits, 
     >     origin, factor)
      call gsdet ('SOS ', 'H2XP', 3, hodox2nameP, hodobits, 
     >     2, 100, 100, iset, idet) 
      call gsdeth('SOS ', 'H2XP', 3, varinames, varibits, 
     >     origin, factor)
*****************************************************************
      call gsdet ('SOS ', 'H2YA', 3, hodoy2nameA, hodobits, 
     >     2, 100, 100, iset, idet) 
      call gsdeth('SOS ', 'H2YA', 3, varinames, varibits, 
     >     origin, factor)
      call gsdet ('SOS ', 'H2YB', 3, hodoy2nameB, hodobits, 
     >     2, 100, 100, iset, idet) 
      call gsdeth('SOS ', 'H2YB', 3, varinames, varibits, 
     >     origin, factor)
      call gsdet ('SOS ', 'H2YC', 3, hodoy2nameC, hodobits, 
     >     2, 100, 100, iset, idet) 
      call gsdeth('SOS ', 'H2YC', 3, varinames, varibits, 
     >     origin, factor)
      call gsdet ('SOS ', 'H2YD', 3, hodoy2nameD, hodobits, 
     >     2, 100, 100, iset, idet) 
      call gsdeth('SOS ', 'H2YD', 3, varinames, varibits, 
     >     origin, factor)
      call gsdet ('SOS ', 'H2YE', 3, hodoy2nameE , hodobits, 
     >     2, 100, 100, iset, idet) 
      call gsdeth('SOS ', 'H2YE', 3, varinames, varibits, 
     >     origin, factor)
      call gsdet ('SOS ', 'H2YF', 3, hodoy2nameF, hodobits, 
     >     2, 100, 100, iset, idet) 
      call gsdeth('SOS ', 'H2YF', 3, varinames, varibits, 
     >     origin, factor)
      call gsdet ('SOS ', 'H2YG', 3, hodoy2nameG, hodobits, 
     >     2, 100, 100, iset, idet) 
      call gsdeth('SOS ', 'H2YG', 3, varinames, varibits, 
     >     origin, factor)
      call gsdet ('SOS ', 'H2YH', 3, hodoy2nameH, hodobits, 
     >     2, 100, 100, iset, idet) 
      call gsdeth('SOS ', 'H2YH', 3, varinames, varibits, 
     >     origin, factor)
      call gsdet ('SOS ', 'H2YI', 3, hodoy2nameI, hodobits, 
     >     2, 100, 100, iset, idet) 
      call gsdeth('SOS ', 'H2YI', 3, varinames, varibits, 
     >     origin, factor)

      end
