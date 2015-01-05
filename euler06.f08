program euler06

  integer, parameter :: maximum = 100
  integer :: sum_of_squares
  integer :: square_of_sum
  integer :: i

  sum_of_squares = 0
  do i=1, maximum
     sum_of_squares = sum_of_squares + i * i
  end do

  square_of_sum = ((maximum * (1 + maximum)) / 2) ** 2

  write (*,*) square_of_sum - sum_of_squares
  

end program euler06

