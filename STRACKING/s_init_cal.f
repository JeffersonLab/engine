*=======================================================================
      subroutine s_init_cal(abort,errmsg)
*=======================================================================
*-
*-      Purpose: SOS Calorimeter Initialization
*-
*-      Created: 20 Mar 1994      Tsolak A. Amatuni
* $Log$
* Revision 1.1  1994/04/13 18:18:40  cdaq
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
      real*4 xi   !Temporary
      real*4 zi   !Temporary
*
      include 'gen_data_structures.cmn'
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
******Temporary Section
      do block=1,smax_cal_blocks
         scal_ped_mean(block) =0.
         scal_ped_rms(block)  =0.
         scal_threshold(block)=0.
*
         scal_cal_const(block)=1.
*
         scal_gain_ini(block)=1.
         scal_gain_cur(block)=1.
         scal_gain_cor(block)=1.
      enddo
*
*      set by CTP
*      slun_dbg_cal=6
*      open(slun_dbg_cal,file='sos_calorimeter.dbg',status='new')
*
      scal_block_xsize=10.
      scal_block_ysize=70.
      scal_block_zsize=10.
      scal_xmax=+60.
      scal_xmin=-60.
      scal_ymax=+35
      scal_ymin=-35.
      scal_zmin=325.
      scal_zmax=365.
      scal_fv_xmin=scal_xmin+5.
      scal_fv_xmax=scal_xmax-5.
      scal_fv_ymin=scal_ymin+5.
      scal_fv_ymax=scal_ymax-5.
      scal_fv_zmin=scal_zmin
      scal_fv_zmax=scal_zmax
*
      do row=1,smax_cal_rows
      do column=1,smax_cal_columns
         block=row+smax_cal_rows*(column-1)
         xi   =scal_block_xsize*(float(row)-0.5)
         zi   =scal_block_zsize*(float(column)-0.5)
         scal_block_xc(block)=scal_xmin+xi
         scal_block_yc(block)=0.
         scal_block_zc(block)=scal_zmin+zi
      enddo
      enddo
******Temporary Section
*      write(6,'('' Completed s_init_cal'')')
*
      return
      end
