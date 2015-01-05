program euler07

  integer, parameter :: wanted_prime = 10001
  integer :: i
  integer :: j
  integer :: number_primes_found
  logical :: prime_found

  ! 2 is prime
  number_primes_found = 1
  i = 1

  do while(number_primes_found < wanted_prime .and. i > 0)
     ! increment first so we have the last found prime outside the loop
     i = i + 2 
     prime_found = .true.
     
     do  j=3, int(sqrt(real(i+1)))
        if (modulo(i,j) == 0) then
           prime_found = .false.
           exit
        end if        
     end do

     if (prime_found) then
        number_primes_found = number_primes_found + 1
     end if
          
  end do

  write (*,*) i

end program euler07
