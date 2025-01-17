c ########################################################################################
      real*8 function hsv_get_scalar(nameX, history)
c
      implicit none
c
      character(len=5), intent(in) :: nameX
      real*8, intent(in), dimension(*) :: history
      integer :: start_index
c
      start_index = hsv_get_sth(nameX, 'start_',history)

      if ( hsv_get_sth(nameX, 'length',history) /= 1 ) then
           write(*,*) "hsv_get_scalar<< Selected nameX (",nameX,
     &      " is not a scalar. Check for typos or choose a different
     &      function that also handles this."
            call cstop('E R R O R  T E R M I N A T I O N')
      endif
c
      hsv_get_scalar = history(start_index)
c
      end function hsv_get_scalar
      
c ########################################################################################
      subroutine hsv_get_symTen2_sub(nameX, history, symTen)
c
      use Tensor
      !implicit none
c
      !type(Tensor2) :: hsv_get_symTen2
      character(len=5), intent(in) :: nameX
      !dimension history(*)
      real*8, intent(in) :: history(*)
      type(Tensor2), intent(out) :: symTen
      integer :: start_index, end_index
c
      start_index = hsv_get_sth(nameX, 'start_',history)
      end_index =   hsv_get_sth(nameX, 'end__',history)

      if ( hsv_get_sth(nameX, 'length',history) /= 6 ) then
           write(*,*) "hsv_get_symTen2<< Selected nameX (",nameX,
     &      " is not a sym tensor. Check for typos or choose a different
     &      function that also handles this."
            call cstop('E R R O R  T E R M I N A T I O N')
      endif
c
      symTen = symstore_2sa(
     &                     real(history(start_index:end_index),8) )

c
      end subroutine hsv_get_symTen2_sub

c ########################################################################################
      type(Tensor2) function hsv_get_symTen2(nameX, history)
c
      use Tensor
      !implicit none
c
      !type(Tensor2) :: hsv_get_symTen2
      character(len=5), intent(in) :: nameX
      !dimension history(*)
      real*8, intent(in) :: history(*)
      integer :: start_index, end_index
c
      start_index = hsv_get_sth(nameX, 'start_',history)
      end_index =   hsv_get_sth(nameX, 'end__',history)

      if ( hsv_get_sth(nameX, 'length',history) /= 6 ) then
           write(*,*) "hsv_get_symTen2<< Selected nameX (",nameX,
     &      " is not a sym tensor. Check for typos or choose a different
     &      function that also handles this."
            call cstop('E R R O R  T E R M I N A T I O N')
      endif
c
      hsv_get_symTen2 = symstore_2sa(
     &                     real(history(start_index:end_index),8) )

c
      end function hsv_get_symTen2
c ########################################################################################
      type(Tensor2) function hsv_get_unsymTen2( nameX, history,
     &                                          index_offset_in )
c
      use Tensor
      use TensorXLSDYNA
      !implicit none
c
      character(len=5), intent(in) :: nameX
      real*8, intent(in) :: history(*)
      real*8, optional :: index_offset_in
      integer index_offset
      integer :: start_index, end_index
c
c If we get an offset for the indices, we apply this,
c else we keep the offset as zero
      if ( present(index_offset_in) ) then
            index_offset = INT(index_offset_in)
            if (index_offset_in == -1) then
             write(*,*) "hsv_get_unsymTen2<< Activated debugging output
     & to determine the offset of the deformation gradient."
             write(*,*) "hsv(1:100)=", history(1:100)
             call cstop ('E R R O R  T E R M I N A T I O N')
            elseif (index_offset_in == -2) then
             ! special case treated in material models, so no offset intended
             index_offset = 0
            elseif (index_offset_in < 0) then
             write(*,*) "hsv_get_unsymTen2<< Input index_offset_in (",
     & index_offset,") is
     & negative, but only offsets >=0 are valid. Check whether you
     & actually want an offset for this unsymmetric tensor."
             call cstop ('E R R O R  T E R M I N A T I O N')
            endif
      else
            index_offset = 0
      endif
c
      start_index = hsv_get_sth(nameX, 'start_',history) + index_offset
      end_index =   hsv_get_sth(nameX, 'end__',history)  + index_offset
c
      if ( hsv_get_sth(nameX, 'length',history) /= 9 ) then
           write(*,*) "hsv_get_unsymTen2<< Selected nameX (",nameX,
     & " is not an unsym tensor. Check for typos or choose a different
     & function that also handles this. nameX-Length=",
     & hsv_get_sth(nameX, 'length',history),
     & ". Start=",start_index,". End=",end_index
           call cstop ('E R R O R  T E R M I N A T I O N')
      endif
c
      hsv_get_unsymTen2 = defoGrad(history(start_index:end_index))
c
      end function hsv_get_unsymTen2      
      
c ########################################################################################

