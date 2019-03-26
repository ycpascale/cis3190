! Name: Pascale Young Choang
! Student ID: 0985717
! CIS * 3190 A1: Forthran

! Program starts here, calling function getLOGdata to ask
! user inputs to calculate the voumes
program logs

implicit none

   print *,"~~~ Welcome to the log volume calculator ~~~"
   call getLOGdata()

end program logs

subroutine getLOGdata()

   real :: DIBsmall, DIBlarge, volBoardFeet, len, volmetre3
   integer :: kerfVal, option
   logical :: isQuit

   isQuit = .false.

   ! The following do while loop will continue to ask user for input until user decides to quit by pressing 2
   ! when asked if they want to quit
   do while (isQuit .eqv. .false.)
      write (*,*) 'Input the length(feet) of the log(feet):'
      read (*,*) len

      ! The following do loop will continue to ask user for a valid input
      ! diameter inside bark at small end should be more than 0.0 to be valid
      do
         write (*,*) 'Input the diameter inside bark at the small end of the log(inches): '
         read (*,*) DIBsmall
         if (DIBsmall > 0) then
            exit
         else
            write(*,*) 'The diameter inside bark cannot be 0 or negative. Please try again.'
         end if
      end do

      ! The following statements asks user for diameter inside bark at large end's input
      write (*,*) 'Input the diameter inside bark at the large end of the log: '
      read (*,*) DIBlarge

      ! The following do loop will continue if kerf value do not match 0 or 1
      do
         write (*,*) 'Input the kerf value, type 0 for 1/8" and type 1 for 1/4": '
         read (*,*) kerfVal
         if ((kerfVal == 0) .or. (kerfVal == 1)) then
            exit
         else
            write(*,*) 'Wrong input. Please try again.'
         end if
      end do

      call calcLOGjclark(DIBsmall, DIBlarge, len, kerfVal, volBoardFeet)
     ! write(*,*) volBoardFeet

      call calcLOGvolume(DIBsmall, DIBlarge, len, volmetre3)
      write(*,1) volBoardFeet
      1 format ('Volume of log in board feet: ', F9.2)
      write(*,2) volmetre3
      2 format ('Volume of log in cubic metres: ', F9.2)

      ! The following do loop asks the user whether they want to exit.
      ! Loop will continue to execute if user do not type the right number
      do
         write (*,*) 'Do you want to exit? Type 1 to exit and type 2 to continue'
         read (*,*) option
         if (option == 1) then
            isQuit = .true.
            exit
         else if (option == 2) then
            exit
         else
            print*,"You type in the wrong number. Try again."
         end if
      end do
   end do

end subroutine getLOGdata

subroutine calcLOGjclark (DIBsmall, DIBlarge, len, kerf, volBF)
! This subroutine was written by J.E Brickell of the U.S Forest Service
! to calculate board foot volume of sawlogs by the international rule.
! Variables in thee subroutine:
! DIBsmall = log's scaling diameter (inches)
! DIBlarge = diameter inside bark at log's large end(inches) (0.0 if 1/2 inch taper)
! len = total log length (feet)
! kerf > 0 if kerf assumption is 1/8 inch
! kerf < 0 if kerf assumption is 1/4 inch

   implicit none

   real, intent(in) :: DIBsmall
   real, intent(in) :: DIBlarge
   real, intent(in) :: len
   integer, intent(in) :: kerf
   real, intent(out) :: volBF

   real :: taperRate, totalSegInFeet, d, xi, xl, dex, vadd, dc
   integer :: i, numOfSegments

   ! If the total log length is less than four feet no board foot volume will be
   ! computed
   volBF = 0.0
   if ((len - 4.0) < 0.0) then
      return
   end if

   ! if the log's large end diameter is furnishes to calcLOGjclark() a taper rate
   ! will be computed. If dl = 0 the standard assumption of 1/2 inch per 4
   ! feet of log length will be used
   if (DIBlarge > 0.0) then
      taperRate = 4.0* (DIBlarge-DIBsmall) / len
   else
      taperRate = 0.5
   end if

   !The following loop finds out how many full 4 foot segments the log contains
   do i=1, 20
      if ((len - float(4*i)) < 0.0) then
         exit
      end if
   end do
   ! numOfSegments = Number of 4 foot segments
   numOfSegments = i - 1
   ! totalSegInFeet = Total of full 4 foot segments in feet
   totalSegInFeet = float(4* numOfSegments)

   ! The following statement moves the scaling diameter down to the end of
   ! the 4 foot segments and increases it according to taper
   d = DIBsmall +(taperRate/4.0) * (len- totalSegInFeet)

   ! The following loop finds out how many full feet
   ! of the length are in the segment less than 4 feet long
   do i=1, 4
      xi = float(i)
      if (totalSegInFeet- len + xi > 0.0) then
         exit
      end if
   end do

   ! The next three statement calculate log volume in the 1, 2, or 3 foot
   ! segment at the small end of the log
   ! xl = number of full feet in the segment less than 4 feet long
   xl = xi - 1.0
   dex = DIBsmall + (taperRate/4.0) * (len - totalSegInFeet - xl)
   vadd = 0.055*xl*dex*dex - 0.1775*xl*dex

   ! The following loop calculates volume in the portion of
   ! the log containing whole 4 foot segments
   do i=1, numOfSegments
      dc = d + taperRate * float(i-1)
      volBF = volBF + 0.22*dc*dc - 0.71*dc
   end do
   volBF = volBF + vadd

   ! If 'kerf' is greater than zero, international 1/8 inch volume as
   ! computed above will be converted to international 1/4 inch volume
   if (kerf > 0) then
      volBF = 0.905 * volBF
   end if
   return
end subroutine calcLOGjclark

subroutine calcLOGvolume (dibs, dibl, len, volM3)
! This subroutine calculates the volume of a log in cubic metres
! according to the following formula:
! V = ((area of dibs + area of dibl) /2)*length of log
   implicit none

   real, intent(in) :: dibs
   real, intent(in) :: dibl
   real, intent(in) :: len
   real, intent(out) :: volM3

   real :: dibsMetre, diblMetre, lenMetre, newDibl, n

   real, parameter :: pi = 3.1415927

   ! The following statement calculates the dibs in metre
   dibsMetre = (dibs/39.37)/2.0

   ! The following statement checks if dibl has been provided(>0)
   ! else value of dibl is calculated according the 1/2" per 4 feet metric
   ! n = number of 4 feet metric
   if (dibl <= 0.0) then
      n = (len - amod(len,4.0)) / 4.0
      newDibl = dibs + (0.5 * n)
      diblMetre = (newDibl/39.37)/2.0
   else
      diblMetre = (dibl/39.37)/2.0
   end if
   ! The following statement converts the len into metre
   lenMetre = len / 3.2808
   ! The following calculate the volume in cubic metres according to the formula
   volM3 = (((pi * dibsMetre * dibsMetre) + (pi * diblMetre * diblMetre)) / 2 ) * lenMetre

end subroutine calcLOGvolume
