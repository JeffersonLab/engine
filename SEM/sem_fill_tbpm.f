      subroutine sem_fill_tbpm()

***********************************************************
*     Author:   B. Zihlmann
*     Date:     16 june 1998
*
************************************************************
* THE ADC IS LOCATED IN THE HMS FASTBUS CRATE SLOT 14
* IN THE INPUT CHANNELS 0 - 7
*
* N_TBPM_RAW_DATA(1): I_CENTER_X
* N_TBPM_RAW_DATA(2): INV I_CENTER_X
* N_TBPM_RAW_DATA(3): I_EDGE_X
* N_TBPM_RAW_DATA(4): INV I_EDGE_X
*
* N_TBPM_RAW_DATA(5): I_CENTER_Y
* N_TBPM_RAW_DATA(6): INV I_CENTER_Y
* N_TBPM_RAW_DATA(7): I_EDGE_Y
* N_TBPM_RAW_DATA(8): INV I_EDGE_Y
*
* VARIABLE N_TBPM_CUTOFF: USED TO ELIMINATE ZERO DATA DUE TO BEAM OFF
* VARIABLE N_TBPM_ADCCUT: CHECKS FOR ADC OVERFLOW
*
* ERROR MESSAGE IS CREATED WHEN ADC OVERFLOW OCCURE TOO FREQUENETLY
*******************************************************************
* changes
* changes
*
* mz 04/17/00 Changed n_fill_tbpm.f from foil numbers to mm
*             (according to propsition from M. Steinacher)
*
* frw 11-2000  changed code to allow for out-of-plane reconstruction
*             even without raster.
*             new hms_recon_xychoice value of 0 forces bad SEM
*             event to be interpreted as being at center of target
* frw 8-2001  new hms_recon_xychoice value of -1 forces SEM event
*             mode position to whatever user has specified
*             changed mode=0 to also use these values (0 is an option)
* vht 11-2008 re-coded the new method of the old code, changed
*             the mapping of raw data in accordance with cabling
*             for SANE experiment.
*******************************************************************

      IMPLICIT NONE

      character*50 here
      parameter (here= 'sem_fill_tbpm')
      
      include 'sem_data_structures.cmn'
      include 'gen_data_structures.cmn'
      include 'gen_constants.par'

      
      integer*4 i,counter,plane

      integer PLANE_TBPM
      parameter (PLANE_TBPM=2)
      
      real*4 cent_x, edge_x, cent_y, edge_y
      real*4 cent_x_inv, edge_x_inv, cent_y_inv, edge_y_inv
      real*4 cent,edge

      logical inv

      real*4 sem_coordinate
      sem_coordinate(cent,edge)=-7.5*(1.+(edge-cent)/(edge+cent))+0.5

*--------------------------------------------------------

*     decode TBPM hit array
      do i = 1 , N_TBPM_TOT_HITS

        plane   = N_TBPM_ADDR1(i)
        counter = N_TBPM_ADDR2(i)

        if (plane .ne. PLANE_TBPM) then
          write(6,*) ' !!!!! bad SEM plane ID=',plane,
     >       ' !!!!   (should be=',PLANE_TBPM,')   counter=',counter
          pause
        elseif (counter .gt. num_tbpm) then
          write(6,*) ' !!!!! bad SEM counter ID=,counter,
     >         ' !!!!   (should be<=',num_tbpm,')'
          pause
        else
          N_TBPM_DATA(counter) = float(N_TBPM_RAW_DATA(i))
        endif

      enddo

*     subtract pedestals
      do i = 1,NUM_TBPM
        N_TBPM_DATA(i) = MAX((N_TBPM_DATA(i) - ndet_ped_tbpm(i)),0.)
      enddo


*     This mapping is for SANE.
      edge_x    =n_tbpm_data(1)
      edge_x_inv=n_tbpm_data(2)
      cent_x    =n_tbpm_data(3)
      cent_x_inv=n_tbpm_data(4)

      edge_y    =n_tbpm_data(5)
      edge_y_inv=n_tbpm_data(6)
      cent_y    =n_tbpm_data(7)
      cent_y_inv=n_tbpm_data(8)

*     X coordinate.

      if(cent_x+edge_x.gt.cent_x_inv+edge_x_inv) then
         cent=cent_x
         edge=edge_x
         inv=.false.
      else
         cent=cent_x_inv
         edge=edge_x_inv
         inv=.true.
      end if
      if(edge+cent.lt.n_tbpm_cutoff.or.
     , abs(edge-cent).eq.abs(edge+cent)) then
         ntbpmx=-20.
      elseif(edge+cent.gt.n_tbpm_adccut) then
         ntbpmx= 20.
      else
         ntbpmx=sem_coordinate(cent,edge)
      end if
c      write(2,*)edge_x,edge_x_inv,cent_x,cent_x_inv,edge_y,edge_y_inv,cent_y,cent_y_inv

      if(inv) ntbpmx=-ntbpmx

*     Y coordinate

      if(cent_y+edge_y.gt.cent_y_inv+edge_y_inv) then
         cent=cent_y
         edge=edge_y
         inv=.false.
      else
         cent=cent_y_inv
         edge=edge_y_inv
         inv=.true.
      end if


      if(edge+cent.lt.n_tbpm_cutoff.or.
     , abs(edge-cent).eq.abs(edge+cent)) then
         ntbpmy=-20.
      elseif(edge+cent.gt.n_tbpm_adccut) then
         ntbpmy= 20.
      else
         ntbpmy=sem_coordinate(cent,edge)
      end if

      if(inv) ntbpmy=-ntbpmy

      end
