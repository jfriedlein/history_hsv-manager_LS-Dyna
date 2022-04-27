c ########################################################################################
      real(kind=8) function hsv_get_scalar(nameX, history)
c
      !implicit none
c
      character(len=5), intent(in) :: nameX
      real*8, intent(in) :: history(*)
      integer :: start_index
c
      start_index = hsv_get_sth(nameX, 'start_',history)

      if ( hsv_get_sth(nameX, 'length',history) /= 1 ) then
           write(*,*) "hsv_get_scalar<< Selected nameX (",nameX,
     &      " is not a scalar. Check for typos or choose a different
     &      function that also handles this."
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
      endif
c
      hsv_get_symTen2 = symstore_2sa(
     &                     real(history(start_index:end_index),8) )

c
      end function hsv_get_symTen2
c ########################################################################################
      type(Tensor2) function hsv_get_unsymTen2(nameX, history)
c
      use Tensor
      use TensorXLSDYNA
      !implicit none
c
      !type(Tensor2) :: hsv_get_symTen2
      character(len=5), intent(in) :: nameX
      real*8, intent(in) :: history(*)
      integer :: start_index, end_index
c
      start_index = hsv_get_sth(nameX, 'start_',history)
      end_index =   hsv_get_sth(nameX, 'end__',history)

      if ( hsv_get_sth(nameX, 'length',history) /= 9 ) then
           write(*,*) "hsv_get_symTen2<< Selected nameX (",nameX,
     &   " is not an unsym tensor. Check for typos or choose a different
     &   function that also handles this."
      endif
c
      hsv_get_unsymTen2 = defoGrad(history(start_index:end_index))
c
      end function hsv_get_unsymTen2      
      
c ########################################################################################

