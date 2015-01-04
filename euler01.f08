program euler1

integer :: i
integer :: sum

sum = 0
do i=1, 999
   if (modulo(i, 3) == 0 .or. modulo(i,5) == 0) then
      sum = sum + i
   end if   
end do

write (*,*) sum


end program
