double complex function SelfBBRightUsual(x)
 use constants
 implicit none
#include "looptools.h"
 double precision, intent(in) :: x
 integer :: j
 double complex :: totalAmplitude
 double complex :: amplitudes(16)

 amplitudes(1) = (0.0078125D0*EL2*MB2*(-1.D0*A0(MB2) + A0(MH12) + MB2*B0(x, MB2, MH12) - 1.D0*MH12*B0(x, MB2, MH12) + x*B0(x, MB2&
  &, MH12))* DBLE(YukS1Quark1**INT(2.D0)))/(MW2*PI2*SW2*x)

 amplitudes(2) = (0.0078125D0*EL2*MB2*(-1.D0*A0(MB2) + A0(MH22) + MB2*B0(x, MB2, MH22) - 1.D0*MH22*B0(x, MB2, MH22) + x*B0(x, MB2&
  &, MH22))* DBLE(YukS1Quark2**INT(2.D0)))/(MW2*PI2*SW2*x)

 amplitudes(3) = (0.0078125D0*EL2*MB2*(-1.D0*A0(MB2) + A0(MH32) + MB2*B0(x, MB2, MH32) - 1.D0*MH32*B0(x, MB2, MH32) + x*B0(x, MB2&
  &, MH32))* DBLE(YukS1Quark3**INT(2.D0)))/(MW2*PI2*SW2*x)

 amplitudes(4) = (0.0078125D0*EL2*MB2*(-1.D0*A0(MB2) + A0(MZ2) + MB2*B0(x, MB2, MZ2) - 1.D0*MZ2*B0(x, MB2, MZ2) + x*B0(x, MB2, MZ&
  &2))* DBLE(YukS2Quark1**INT(2.D0)))/(MW2*PI2*SW2*x)

 amplitudes(5) = (0.0078125D0*EL2*MB2*(A0(MA02) - 1.D0*A0(MB2) - 1.D0*MA02*B0(x, MA02, MB2) + MB2*B0(x, MA02, MB2) + x*B0(x, MA02&
  &, MB2))* DBLE(YukS2Quark2**INT(2.D0)))/(MW2*PI2*SW2*x)

 amplitudes(6) = (0.03125D0*CKM13*CKMC13*((-0.5D0*EL2*MB2*SB2*A0(MU2)*DBLE(YukS3Quark1**INT(2.D0)))/(MW2*SW2) + (0.5D0*EL2*MB2*SB&
  &2*A0(MW2)*DBLE(YukS3Quark1**INT(2.D0)))/(MW2*SW2) - (0.5D0*EL2*MB2*SB2*B0(x, MU2, MW2)*DBLE(YukS3Quark1**INT(2.D0)))/SW2 + (0.&
  &5D0*EL2*MB2*MU2*SB2*B0(x, MU2, MW2)*DBLE(YukS3Quark1**INT(2.D0)))/(MW2*SW2) + (0.5D0*EL2*MB2*SB2*x*B0(x, MU2, MW2)*DBLE(YukS3Q&
  &uark1**INT(2.D0)))/(MW2*SW2)))/(PI2*SB2*x)

 amplitudes(7) = (0.03125D0*CKM23*CKMC23*((-0.5D0*EL2*MB2*SB2*A0(MC2)*DBLE(YukS3Quark1**INT(2.D0)))/(MW2*SW2) + (0.5D0*EL2*MB2*SB&
  &2*A0(MW2)*DBLE(YukS3Quark1**INT(2.D0)))/(MW2*SW2) - (0.5D0*EL2*MB2*SB2*B0(x, MC2, MW2)*DBLE(YukS3Quark1**INT(2.D0)))/SW2 + (0.&
  &5D0*EL2*MB2*MC2*SB2*B0(x, MC2, MW2)*DBLE(YukS3Quark1**INT(2.D0)))/(MW2*SW2) + (0.5D0*EL2*MB2*SB2*x*B0(x, MC2, MW2)*DBLE(YukS3Q&
  &uark1**INT(2.D0)))/(MW2*SW2)))/(PI2*SB2*x)

 amplitudes(8) = (0.03125D0*CKM33*CKMC33*((-0.5D0*EL2*MB2*SB2*A0(MT2)*DBLE(YukS3Quark1**INT(2.D0)))/(MW2*SW2) + (0.5D0*EL2*MB2*SB&
  &2*A0(MW2)*DBLE(YukS3Quark1**INT(2.D0)))/(MW2*SW2) - (0.5D0*EL2*MB2*SB2*B0(x, MT2, MW2)*DBLE(YukS3Quark1**INT(2.D0)))/SW2 + (0.&
  &5D0*EL2*MB2*MT2*SB2*B0(x, MT2, MW2)*DBLE(YukS3Quark1**INT(2.D0)))/(MW2*SW2) + (0.5D0*EL2*MB2*SB2*x*B0(x, MT2, MW2)*DBLE(YukS3Q&
  &uark1**INT(2.D0)))/(MW2*SW2)))/(PI2*SB2*x)

 amplitudes(9) = (0.03125D0*CKM13*CKMC13*((0.5D0*EL2*MB2*SB2*A0(MHp2)*DBLE(YukS3Quark2**INT(2.D0)))/(MW2*SW2) - (0.5D0*EL2*MB2*SB&
  &2*A0(MU2)*DBLE(YukS3Quark2**INT(2.D0)))/(MW2*SW2) - (0.5D0*EL2*MB2*MHp2*SB2*B0(x, MHp2, MU2)*DBLE(YukS3Quark2**INT(2.D0)))/ (M&
  &W2*SW2) + (0.5D0*EL2*MB2*MU2*SB2*B0(x, MHp2, MU2)*DBLE(YukS3Quark2**INT(2.D0)))/(MW2*SW2) + (0.5D0*EL2*MB2*SB2*x*B0(x, MHp2, M&
  &U2)*DBLE(YukS3Quark2**INT(2.D0)))/(MW2*SW2)))/(PI2*SB2*x)

 amplitudes(10) = (0.03125D0*CKM23*CKMC23*((-0.5D0*EL2*MB2*SB2*A0(MC2)*DBLE(YukS3Quark2**INT(2.D0)))/(MW2*SW2) + (0.5D0*EL2*MB2*S&
  &B2*A0(MHp2)*DBLE(YukS3Quark2**INT(2.D0)))/(MW2*SW2) + (0.5D0*EL2*MB2*MC2*SB2*B0(x, MC2, MHp2)*DBLE(YukS3Quark2**INT(2.D0)))/ (&
  &MW2*SW2) - (0.5D0*EL2*MB2*MHp2*SB2*B0(x, MC2, MHp2)*DBLE(YukS3Quark2**INT(2.D0)))/(MW2*SW2) + (0.5D0*EL2*MB2*SB2*x*B0(x, MC2, &
  &MHp2)*DBLE(YukS3Quark2**INT(2.D0)))/(MW2*SW2)))/(PI2*SB2*x)

 amplitudes(11) = (0.03125D0*CKM33*CKMC33*((0.5D0*EL2*MB2*SB2*A0(MHp2)*DBLE(YukS3Quark2**INT(2.D0)))/(MW2*SW2) - (0.5D0*EL2*MB2*S&
  &B2*A0(MT2)*DBLE(YukS3Quark2**INT(2.D0)))/(MW2*SW2) - (0.5D0*EL2*MB2*MHp2*SB2*B0(x, MHp2, MT2)*DBLE(YukS3Quark2**INT(2.D0)))/ (&
  &MW2*SW2) + (0.5D0*EL2*MB2*MT2*SB2*B0(x, MHp2, MT2)*DBLE(YukS3Quark2**INT(2.D0)))/(MW2*SW2) + (0.5D0*EL2*MB2*SB2*x*B0(x, MHp2, &
  &MT2)*DBLE(YukS3Quark2**INT(2.D0)))/(MW2*SW2)))/(PI2*SB2*x)

 amplitudes(12) = (0.006944444444444444D0*EL2*(-1.D0*x - 1.D0*A0(MB2) + MB2*B0(x, 0.D0, MB2) + x*B0(x, 0.D0, MB2)))/(PI2*x)

 amplitudes(13) = (0.001736111111111111D0*EL2*(-4.D0*x*DBLE(SW**INT(4.D0)) - 4.D0*A0(MB2)*DBLE(SW**INT(4.D0)) + 4.D0*A0(MZ2)*DBLE&
  &(SW**INT(4.D0)) + 4.D0*MB2*B0(x, MB2, MZ2)*DBLE(SW**INT(4.D0)) - 4.D0*MZ2*B0(x, MB2, MZ2)*DBLE(SW**INT(4.D0)) + 4.D0*x*B0(x, M&
  &B2, MZ2)*DBLE(SW**INT(4.D0))))/ (CW2*PI2*SW2*x)

 amplitudes(14) = 0.D0

 amplitudes(15) = 0.D0

 amplitudes(16) = 0.D0

  totalAmplitude = (0D0,0D0)
 do j=1,16
  totalAmplitude = totalAmplitude + amplitudes(j)
 end do
 SelfBBRightUsual = totalAmplitude
end function SelfBBRightUsual

