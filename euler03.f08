program euler03

integer, parameter :: BTS = selected_int_kind(13)
integer(kind=BTS) :: number  = 600851475143_BTS
integer(kind=BTS) :: i = 3_BTS


do while (number > 1)

   do while (modulo(number, i) == 0)
      number = number / i     
   end do

   i = i + 2

end do

write (*,*) i - 2

end program euler03
