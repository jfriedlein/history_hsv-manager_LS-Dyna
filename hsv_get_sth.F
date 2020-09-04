c ########################################################################################
      integer function hsv_get_sth(nameX, sth_string)
c
      implicit none
c
      character(len=5), intent(in) :: nameX
      character(len=6), intent(in) :: sth_string
      character(len=10), dimension(5) :: list_name
      integer, dimension(5) :: list_start_index
      integer, dimension(5) :: list_length
      integer, dimension(5) :: list_end_index
      integer, dimension(5) :: list_pair
      integer :: i, len_X
      len_X = len(nameX)
c Init the strings with blanks
      !do i=1,5
      !    list_name(i) = '          '
      !enddo
c       CONVENTION:
c       es . alpha . eps_p . xxx. defoGrad
c       09    02      03     00      45
c       0902030045
c       CONTENT:
c       1    . 2     . 3  4  5  6  7  8 . 9 10 ...  44 . 45 46 ...            53
c       hsv1 . alpha . eps_p1 ...eps_p6 . es1 ... es36 . defoGrad1 ... defoGrad9
! @WARNING: Most of the numbers are not updated automatically, so check every number
! after you changed something.
c Tangent in Voigt 6x6 notation
      list_name(1) = 'es___'
      list_start_index(1) = 09
      list_length(1) = 36
      list_end_index(1) =  list_start_index(1) + list_length(1)-1
      list_pair(1) = 01
c Internal variable alpha
      list_name(2) = 'alpha'
      list_start_index(2) = 02
      list_length(2) = 1
      list_end_index(2) =  list_start_index(2) + list_length(2)-1
      list_pair(2) = 02    
c Plastic strain tensor epsp
      list_name(3) = 'eps_p'
      list_start_index(3) = 03
      list_length(3) = 6
      list_end_index(3) =  list_start_index(3) + list_length(3)-1
      list_pair(3) = 03       
c xxx unused (e.g. for scalar damage)
      list_name(4) = 'xxxxx'
      list_start_index(4) = 00
      list_length(4) = 0
      list_end_index(4) =  list_start_index(4) + list_length(4)-1
      list_pair(4) = 04 
c Deformation gradient F
      list_name(5) = 'defoG'
      list_start_index(5) = 45
      list_length(5) = 9
      list_end_index(5) =  list_start_index(5) + list_length(5)-1
      list_pair(5) = 05
      
      do i=1,5
          ! Look for the nameX in the list of names to find the index 'i'
          if ( index(nameX,trim(list_name(i))) /= 0 ) then
              if ( index(sth_string, 'start_') /= 0 ) then
                    hsv_get_sth = list_start_index(i)
                    exit
              else if ( index(sth_string, 'length') /= 0 ) then
                    hsv_get_sth = list_length(i)
                    exit
              else if ( index(sth_string, 'end__') /= 0 ) then
                    hsv_get_sth = list_end_index(i)
                    exit
              else if ( index(sth_string, 'ipair_') /= 0 ) then
                    hsv_get_sth = list_pair(i)
                    exit
              else
                  write(*,*) "hsv_get_sth<< Could not find sth_string"
              endif
          elseif ( i == 5 ) then
              write(*,*) "hsv_get_sth<< Could not find the nameX"
              pause
          endif
      enddo

      end function hsv_get_sth