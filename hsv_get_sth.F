c ########################################################################################
      integer function hsv_get_sth(nameX, sth_string)
c
      implicit none
c
      character(len=5), intent(in) :: nameX
      character(len=6), intent(in) :: sth_string
      character(len=10), dimension(:), allocatable :: list_name
      integer, dimension(:), allocatable :: list_start_index
      integer, dimension(:), allocatable :: list_length
      integer, dimension(:), allocatable :: list_end_index
      integer, dimension(:), allocatable :: list_pair
      integer :: i, len_X, j, n_entries
      len_X = len(nameX)
c
c USER-INPUT: Update the number of entities (including "es" and "defoGrad")
      n_entries=8+1
c
      allocate(list_name(n_entries))
      allocate(list_start_index(n_entries))
      allocate(list_length(n_entries))
      allocate(list_end_index(n_entries))
      allocate(list_pair(n_entries))
c
c @note The max size of hsv is 200, however this can be increased in "nhisparm.inc"
c Init the strings with blanks
      !do i=1,5
      !    list_name(i) = '          '
      !enddo
c @todo Try to implement the below convention to unify the parameters also for different code versions      
c       CONVENTION:
c       es . alpha . eps_p . damag . defoGrad
c       10    02      03      09      45
c       1002030945
c       CONTENT:
c       1    . 2     . 3  4  5  6  7  8 . 9     . 10    . 11 ...  46   . 47 48 49  50 51 52  53 54 55
c       hsv1 . alpha . eps_p1 ...eps_p6 . damag . a_til . es1 ... es36 . defoGrad1 ... defoGrad9
c @note Use nhsv=46, where nhsv corresponds to the index of the last entry es36
c
c two back stresses
c @note Use nhsv=58, where nhsv corresponds to the index of the last entry es36
c       1    . 2     . 3  4  5  6  7  8 . 9     . 10    . 11 ... 16 . 17 ... 22 . 23 ...  58 . 59 60 61  62 63 64  65 66 67
c       hsv1 . alpha . eps_p1 ...eps_p6 . damag . a_til . B_1_ij    . B_2_ij    . es1...es36 . defoGrad1 ... defoGrad9
c
c three back stresses
c @note Use nhsv=64, where nhsv corresponds to the index of the last entry es36
c       1    . 2     . 3  4  5  6  7  8 . 9     . 10    . 11 ... 16 . 17 ... 22 . 23 ... 28 . 29 ...  64 . 65 ...              73
c       hsv1 . alpha . eps_p1 ...eps_p6 . damag . a_til . B_1_ij    . B_2_ij    . B_3_ij    . es1...es36 . defoGrad1 ... defoGrad9
c
c @WARNING: Most of the numbers are not updated automatically, so check every number
c after you changed something.
c @todo clean this use index "j" to avoid typing the same thing several times
c @todo Check that the "nhv" is large enough for everything      
c
c @note The order in the list_* is irrelevant and only locally used
c Tangent in Voigt 6x6 notation
      list_name(1) = 'es___'
      list_start_index(1) = 11+6+6+6
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
c damage damag
      list_name(4) = 'damag'
      list_start_index(4) = 09
      list_length(4) = 1
      list_end_index(4) =  list_start_index(4) + list_length(4)-1
      list_pair(4) = 04 
c alpha_tilde
      list_name(5) = 'a_til'
      list_start_index(5) = 10
      list_length(5) = 1
      list_end_index(5) =  list_start_index(5) + list_length(5)-1
      list_pair(5) = 05 
c @todo Apply the following quicker assignment using "j=6" for all entries
c  and apply everything that solely relies on "j" afterwards in a loop over all "j"
c Deformation gradient F
      j = 6
      list_pair(j) = j
      list_name(j) = 'defoG'
      list_start_index(j) = 47+6+6+6
      list_length(j) = 9
      list_end_index(j) =  list_start_index(j) + list_length(j)-1
c B_1: first back stress
      j = 7
      list_pair(j) = j
      list_name(j) = 'B_1__'
      list_start_index(j) = 11
      list_length(j) = 6
      list_end_index(j) =  list_start_index(j) + list_length(j)-1
c B_2: second back stress
      j = 8
      list_pair(j) = j
      list_name(j) = 'B_2__'
      list_start_index(j) = 11+6
      list_length(j) = 6
      list_end_index(j) =  list_start_index(j) + list_length(j)-1
c B_3: third back stress
      j = 9
      list_pair(j) = j
      list_name(j) = 'B_3__'
      list_start_index(j) = 11+6+6
      list_length(j) = 6
      list_end_index(j) =  list_start_index(j) + list_length(j)-1
c
c
      do i=1,n_entries
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
          elseif ( i == n_entries ) then
              write(*,*) "hsv_get_sth<< Could not find the nameX"
              write(*,*) "nameX: ",nameX
              write(*,*) "sth_string: ",sth_string
              pause
          endif
      enddo

      end function hsv_get_sth
