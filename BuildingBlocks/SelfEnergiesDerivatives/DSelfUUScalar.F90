double complex function DSelfUUScalar(x)
 use constants
 implicit none
#include "looptools.h"
 double precision, intent(in) :: x
 integer :: j
 double complex :: totalAmplitude
 double complex :: amplitudes(16)

 amplitudes(1) = (0.015625D0*CA22*EL2*MU2*SA12*DB0(x, MH12, MU2))/(MW2*PI2*SB2*SW2)

 amplitudes(2) = (0.015625D0*EL2*MU2*DB0(x, MH22, MU2)*DBLE((CA1*CA3 - 1.D0*SA1*SA2*SA3)**INT(2.D0)))/(MW2*PI2*SB2*SW2)

 amplitudes(3) = (0.015625D0*EL2*MU2*DB0(x, MH32, MU2)*DBLE((CA3*SA1*SA2 + CA1*SA3)**INT(2.D0)))/(MW2*PI2*SB2*SW2)

 amplitudes(4) = (-0.015625D0*EL2*MU2*DB0(x, MU2, MZ2))/(MW2*PI2*SW2)

 amplitudes(5) = (-0.015625D0*CB2*EL2*MU2*DB0(x, MA02, MU2))/(MW2*PI2*SB2*SW2)

 amplitudes(6) = (-0.03125D0*CKM11*CKMC11*EL2*MD2*YukS3Quark1*DB0(x, MD2, MW2))/(MW2*PI2*SW2)

 amplitudes(7) = (-0.03125D0*CKM12*CKMC12*EL2*MS2*YukS3Quark1*DB0(x, MS2, MW2))/(MW2*PI2*SW2)

 amplitudes(8) = (-0.03125D0*CKM13*CKMC13*EL2*MB2*YukS3Quark1*DB0(x, MB2, MW2))/(MW2*PI2*SW2)

 amplitudes(9) = (-0.03125D0*CB*CKM11*CKMC11*EL2*MD2*YukS3Quark2*DB0(x, MD2, MHp2))/(MW2*PI2*SB*SW2)

 amplitudes(10) = (-0.03125D0*CB*CKM12*CKMC12*EL2*MS2*YukS3Quark2*DB0(x, MHp2, MS2))/(MW2*PI2*SB*SW2)

 amplitudes(11) = (-0.03125D0*CB*CKM13*CKMC13*EL2*MB2*YukS3Quark2*DB0(x, MB2, MHp2))/(MW2*PI2*SB*SW2)

 amplitudes(12) = (-0.1111111111111111D0*EL2*DB0(x, 0.D0, MU2))/PI2

 amplitudes(13) = (0.027777777777777776D0*EL2*(3.D0*CW2 - 1.D0*SW2)*DB0(x, MU2, MZ2))/(CW2*PI2)

 amplitudes(14) = 0.D0

 amplitudes(15) = 0.D0

 amplitudes(16) = 0.D0

  totalAmplitude = (0D0,0D0)
 do j=1,16
  totalAmplitude = totalAmplitude + amplitudes(j)
 end do
 DSelfUUScalar = totalAmplitude
end function DSelfUUScalar

