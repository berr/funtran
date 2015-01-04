program euler05

  integer, parameter :: maximum = 20
  ! inneficient storage, should be only primes
  integer, dimension(maximum) :: maximum_times_factor
  integer :: i
  integer :: j
  integer :: current_number
  integer :: temp
  integer :: mult

  do i=1, maximum
     maximum_times_factor(i) = 0
  end do

  maximum_times_factor(1) = 1
  do i=2, maximum
     current_number = i
     do j=2, i
        ! should iterate over primes only
        maximum_times_factor(j) = max(maximum_times_factor(j), times_factor(current_number, j))
     end do
  end do

  mult = 1
  do i=1, maximum
     mult = mult * i ** maximum_times_factor(i)
  end do
  
  write (*,*) mult

contains

  integer function times_factor(number, factor)
    integer, intent(inout) :: number
    integer, intent(in) :: factor

    times_factor = 0
    do while(modulo(number, factor) == 0)
       number = number / factor
       times_factor = times_factor + 1
    end do

  end function times_factor
  
end program euler05

