*=======================================================================
      subroutine s_init_cal(abort,errmsg)
*=======================================================================
*-
*-      Purpose: SOS Calorimeter Initialization
*-
*-      Created: 20 Mar 1994      Tsolak A. Amatuni
* $Log: s_init_cal.f,v $
* Revision 1.3  1995/05/22 19:45:40  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.2  1994/06/14  04:30:27  cdaq
* (DFG) Remove hardwired parameters
*
* Revision 1.1  1994/04/13  18:18:40  cdaq
* Initial revision
*
*-----------------------------------------------------------------------
*
*
      implicit none
      save
*
      logical abort
      character*(*) errmsg
      character*10 here
      parameter (here='S_INIT_CAL')
*
      integer*4 block      !Block number
      integer*4 row        !Row number
      integer*4 column     !Column number
*
      include 'sos_data_structures.cmn'
      include 'sos_calorimeter.cmn'
*
*-----Initialize the positions
      do column=1,smax_cal_columns
         do row=1,smax_cal_rows
            block=row+smax_cal_rows*(column-1)
*
            if(column.eq.1) then
               scal_block_xc(block)=scal_1pr_top(row)+0.5*scal_1pr_thick
               scal_block_yc(block)=0.5*(scal_1pr_left+scal_1pr_right)
               scal_block_zc(block)=scal_1pr_zpos+0.5*scal_1pr_thick
            else if(column.eq.2) then
               scal_block_xc(block)=scal_2ta_top(row)+0.5*scal_2ta_thick
               scal_block_yc(block)=0.5*(scal_2ta_left+scal_2ta_right)
               scal_block_zc(block)=scal_2ta_zpos+0.5*scal_2ta_thick
            else if(column.eq.3) then
               scal_block_xc(block)=scal_3ta_top(row)+0.5*scal_3ta_thick
               scal_block_yc(block)=0.5*(scal_3ta_left+scal_3ta_right)
               scal_block_zc(block)=scal_3ta_zpos+0.5*scal_3ta_thick
            else
               scal_block_xc(block)=scal_4ta_top(row)+0.5*scal_4ta_thick
               scal_block_yc(block)=0.5*(scal_4ta_left+scal_4ta_right)
               scal_block_zc(block)=scal_4ta_zpos+0.5*scal_4ta_thick
            endif
         enddo   !End loop over rows
      enddo   !End loop over columns
*
*
      scal_block_xsize= scal_4ta_top(2) - scal_4ta_top(1)
      scal_block_ysize= scal_4ta_left - scal_4ta_right
      scal_block_zsize= scal_4ta_thick
      scal_xmax= scal_4ta_top(scal_4ta_nr) + scal_block_xsize
      scal_xmin= scal_4ta_top(1)
      scal_ymax= scal_4ta_left
      scal_ymin= scal_4ta_right
      scal_zmin= scal_1pr_zpos
      scal_zmax= scal_4ta_zpos
      scal_fv_xmin=scal_xmin+5.
      scal_fv_xmax=scal_xmax-5.
      scal_fv_ymin=scal_ymin+5.
      scal_fv_ymax=scal_ymax-5.
      scal_fv_zmin=scal_zmin
      scal_fv_zmax=scal_zmax
*
      return
      end
