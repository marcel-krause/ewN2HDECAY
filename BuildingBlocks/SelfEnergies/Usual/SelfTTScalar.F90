double complex function SelfTTScalarUsual(x)
 use constants
 implicit none
#include "looptools.h"
 double precision, intent(in) :: x
 integer :: j
 double complex :: totalAmplitude
 double complex :: amplitudes(16)

 amplitudes(1) = (0.015625D0*CA22*EL2*MT2*SA12*B0(x, MH12, MT2))/(MW2*PI2*SB2*SW2)

 amplitudes(2) = (0.015625D0*EL2*MT2*B0(x, MH22, MT2)*DBLE((CA1*CA3 - 1.D0*SA1*SA2*SA3)**INT(2.D0)))/(MW2*PI2*SB2*SW2)

 amplitudes(3) = (0.015625D0*EL2*MT2*B0(x, MH32, MT2)*DBLE((CA3*SA1*SA2 + CA1*SA3)**INT(2.D0)))/(MW2*PI2*SB2*SW2)

 amplitudes(4) = (-0.015625D0*EL2*MT2*B0(x, MT2, MZ2))/(MW2*PI2*SW2)

 amplitudes(5) = (-0.015625D0*CB2*EL2*MT2*B0(x, MA02, MT2))/(MW2*PI2*SB2*SW2)

 amplitudes(6) = (-0.03125D0*CKM31*CKMC31*EL2*MD2*YukS3Quark1*B0(x, MD2, MW2))/(MW2*PI2*SW2)

 amplitudes(7) = (-0.03125D0*CKM32*CKMC32*EL2*MS2*YukS3Quark1*B0(x, MS2, MW2))/(MW2*PI2*SW2)

 amplitudes(8) = (-0.03125D0*CKM33*CKMC33*EL2*MB2*YukS3Quark1*B0(x, MB2, MW2))/(MW2*PI2*SW2)

 amplitudes(9) = (-0.03125D0*CB*CKM31*CKMC31*EL2*MD2*YukS3Quark2*B0(x, MD2, MHp2))/(MW2*PI2*SB*SW2)

 amplitudes(10) = (-0.03125D0*CB*CKM32*CKMC32*EL2*MS2*YukS3Quark2*B0(x, MHp2, MS2))/(MW2*PI2*SB*SW2)

 amplitudes(11) = (-0.03125D0*CB*CKM33*CKMC33*EL2*MB2*YukS3Quark2*B0(x, MB2, MHp2))/(MW2*PI2*SB*SW2)

 amplitudes(12) = (-0.05555555555555555D0*EL2*(-1.D0 + 2.D0*B0(x, 0.D0, MT2)))/PI2

 amplitudes(13) = (0.013888888888888888D0*EL2*(3.D0*CW2 - 1.D0*SW2)*(-1.D0 + 2.D0*B0(x, MT2, MZ2)))/(CW2*PI2)

 amplitudes(14) = 0.D0

 amplitudes(15) = 0.D0

 amplitudes(16) = 0.D0

  totalAmplitude = (0D0,0D0)
 do j=1,16
  totalAmplitude = totalAmplitude + amplitudes(j)
 end do
 SelfTTScalarUsual = totalAmplitude
end function SelfTTScalarUsual

