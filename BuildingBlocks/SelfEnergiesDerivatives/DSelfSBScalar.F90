double complex function DSelfSBScalar(x)
 use constants
 implicit none
#include "looptools.h"
 double precision, intent(in) :: x
 integer :: j
 double complex :: totalAmplitude
 double complex :: amplitudes(9)

 amplitudes(1) = (-0.03125D0*CKM12*CKMC13*EL2*MU2*YukS3Quark1*DB0(x, MU2, MW2))/(MW2*PI2*SW2)

 amplitudes(2) = (-0.03125D0*CKM22*CKMC23*EL2*MC2*YukS3Quark1*DB0(x, MC2, MW2))/(MW2*PI2*SW2)

 amplitudes(3) = (-0.03125D0*CKM32*CKMC33*EL2*MT2*YukS3Quark1*DB0(x, MT2, MW2))/(MW2*PI2*SW2)

 amplitudes(4) = (-0.03125D0*CB*CKM12*CKMC13*EL2*MU2*YukS3Quark2*DB0(x, MHp2, MU2))/(MW2*PI2*SB*SW2)

 amplitudes(5) = (-0.03125D0*CB*CKM22*CKMC23*EL2*MC2*YukS3Quark2*DB0(x, MC2, MHp2))/(MW2*PI2*SB*SW2)

 amplitudes(6) = (-0.03125D0*CB*CKM32*CKMC33*EL2*MT2*YukS3Quark2*DB0(x, MHp2, MT2))/(MW2*PI2*SB*SW2)

 amplitudes(7) = 0.D0

 amplitudes(8) = 0.D0

 amplitudes(9) = 0.D0

  totalAmplitude = (0D0,0D0)
 do j=1,9
  totalAmplitude = totalAmplitude + amplitudes(j)
 end do
 DSelfSBScalar = totalAmplitude
end function DSelfSBScalar

