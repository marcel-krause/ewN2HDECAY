double complex function TadH1()
 use constants
 implicit none
#include "looptools.h"
 integer :: j
 double complex :: totalAmplitude
 double complex :: amplitudes(21)

 amplitudes(1) = (-0.125D0*EL*ME2*YukS1Lep1*A0(ME2))/(MW*PI2*SW)

 amplitudes(2) = (-0.125D0*EL*MM2*YukS1Lep1*A0(MM2))/(MW*PI2*SW)

 amplitudes(3) = (-0.125D0*EL*ML2*YukS1Lep1*A0(ML2))/(MW*PI2*SW)

 amplitudes(4) = (-0.375D0*CA2*EL*MU2*SA1*A0(MU2))/(MW*PI2*SB*SW)

 amplitudes(5) = (-0.375D0*CA2*EL*MC2*SA1*A0(MC2))/(MW*PI2*SB*SW)

 amplitudes(6) = (-0.375D0*CA2*EL*MT2*SA1*A0(MT2))/(MW*PI2*SB*SW)

 amplitudes(7) = (-0.375D0*EL*MD2*YukS1Quark1*A0(MD2))/(MW*PI2*SW)

 amplitudes(8) = (-0.375D0*EL*MS2*YukS1Quark1*A0(MS2))/(MW*PI2*SW)

 amplitudes(9) = (-0.375D0*EL*MB2*YukS1Quark1*A0(MB2))/(MW*PI2*SW)

 amplitudes(10) = (-0.03125D0*CS1S1S1f111*A0(MH12))/PI2

 amplitudes(11) = (-0.03125D0*CS1S1S1f122*A0(MH22))/PI2

 amplitudes(12) = (-0.03125D0*CS1S1S1f133*A0(MH32))/PI2

 amplitudes(13) = (-0.03125D0*CS2S2S1f111*A0(MZ2))/PI2

 amplitudes(14) = (-0.03125D0*CS2S2S1f221*A0(MA02))/PI2

 amplitudes(15) = (-0.0625D0*CS1S3S3f111*A0(MW2))/PI2

 amplitudes(16) = (-0.0625D0*CS1S3S3f122*A0(MHp2))/PI2

 amplitudes(17) = (-0.015625D0*EL2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*A0(MZ2)*DBLE((CW2 + SW2)**INT(2.D0))&
  &)/(CW2*PI2*SW2)

 amplitudes(18) = (-0.015625D0*EL2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*A0(MW2))/(PI2*SW2)

 amplitudes(19) = (-0.015625D0*EL2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*A0(MW2))/(PI2*SW2)

 amplitudes(20) = (-0.03125D0*EL2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*(MZ2 - 2.D0*A0(MZ2))*DBLE((CW2 + SW2)&
  &**INT(2.D0)))/(CW2*PI2*SW2)

 amplitudes(21) = (-0.0625D0*EL2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*(MW2 - 2.D0*A0(MW2)))/(PI2*SW2)

  totalAmplitude = (0D0,0D0)
 do j=1,21
  totalAmplitude = totalAmplitude + amplitudes(j)
 end do
 TadH1 = totalAmplitude
end function TadH1

