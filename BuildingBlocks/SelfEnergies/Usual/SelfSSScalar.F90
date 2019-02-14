double complex function SelfSSScalarUsual(x)
 use constants
 implicit none
#include "looptools.h"
 double precision, intent(in) :: x
 integer :: j
 double complex :: totalAmplitude
 double complex :: amplitudes(16)

 amplitudes(1) = (0.015625D0*EL2*MS2*B0(x, MH12, MS2)*DBLE(YukS1Quark1**INT(2.D0)))/(MW2*PI2*SW2)

 amplitudes(2) = (0.015625D0*EL2*MS2*B0(x, MH22, MS2)*DBLE(YukS1Quark2**INT(2.D0)))/(MW2*PI2*SW2)

 amplitudes(3) = (0.015625D0*EL2*MS2*B0(x, MH32, MS2)*DBLE(YukS1Quark3**INT(2.D0)))/(MW2*PI2*SW2)

 amplitudes(4) = (-0.015625D0*EL2*MS2*B0(x, MS2, MZ2)*DBLE(YukS2Quark1**INT(2.D0)))/(MW2*PI2*SW2)

 amplitudes(5) = (-0.015625D0*EL2*MS2*B0(x, MA02, MS2)*DBLE(YukS2Quark2**INT(2.D0)))/(MW2*PI2*SW2)

 amplitudes(6) = (-0.03125D0*CKM12*CKMC12*EL2*MU2*YukS3Quark1*B0(x, MU2, MW2))/(MW2*PI2*SW2)

 amplitudes(7) = (-0.03125D0*CKM22*CKMC22*EL2*MC2*YukS3Quark1*B0(x, MC2, MW2))/(MW2*PI2*SW2)

 amplitudes(8) = (-0.03125D0*CKM32*CKMC32*EL2*MT2*YukS3Quark1*B0(x, MT2, MW2))/(MW2*PI2*SW2)

 amplitudes(9) = (-0.03125D0*CB*CKM12*CKMC12*EL2*MU2*YukS3Quark2*B0(x, MHp2, MU2))/(MW2*PI2*SB*SW2)

 amplitudes(10) = (-0.03125D0*CB*CKM22*CKMC22*EL2*MC2*YukS3Quark2*B0(x, MC2, MHp2))/(MW2*PI2*SB*SW2)

 amplitudes(11) = (-0.03125D0*CB*CKM32*CKMC32*EL2*MT2*YukS3Quark2*B0(x, MHp2, MT2))/(MW2*PI2*SB*SW2)

 amplitudes(12) = (-0.013888888888888888D0*EL2*(-1.D0 + 2.D0*B0(x, 0.D0, MS2)))/PI2

 amplitudes(13) = (0.006944444444444444D0*EL2*(3.D0*CW2 + SW2)*(-1.D0 + 2.D0*B0(x, MS2, MZ2)))/(CW2*PI2)

 amplitudes(14) = 0.D0

 amplitudes(15) = 0.D0

 amplitudes(16) = 0.D0

  totalAmplitude = (0D0,0D0)
 do j=1,16
  totalAmplitude = totalAmplitude + amplitudes(j)
 end do
 SelfSSScalarUsual = totalAmplitude
end function SelfSSScalarUsual

