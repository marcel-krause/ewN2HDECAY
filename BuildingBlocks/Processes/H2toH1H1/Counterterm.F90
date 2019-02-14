double precision function H2toH1H1CT(x)
 use constants
 use counterterms
 implicit none
#include "looptools.h"
 integer, intent(in) :: x
 double precision :: totalAmplitude
 double precision :: dRR11, dRR12, dRR13, dRR21, dRR22, dRR23, dRR31, dRR32, dRR33

 select case (x)
	case (1)
		dRR11 = -CA2*SA1*dAlpha1KanUsual() - CA1*SA2*dAlpha2KanUsual()
		dRR12 = CA1*CA2*dAlpha1KanUsual() - SA1*SA2*dAlpha2KanUsual()
		dRR13 = CA2*dAlpha2KanUsual()
		dRR21 = -CA1*CA3*dAlpha1KanUsual() - CA1*CA3*SA2*dAlpha3KanUsual() - CA1*CA2*SA3*dAlpha2KanUsual() + &
			& SA1*SA3*dAlpha3KanUsual() + SA1*SA2*SA3*dAlpha1KanUsual()
		dRR22 = -CA3*SA1*dAlpha1KanUsual() - CA3*SA1*SA2*dAlpha3KanUsual() - CA1*SA3*dAlpha3KanUsual() - &
			& CA2*SA1*SA3*dAlpha2KanUsual() - CA1*SA2*SA3*dAlpha1KanUsual()
		dRR23 = CA2*CA3*dAlpha3KanUsual() - SA2*SA3*dAlpha2KanUsual()
		dRR31 = -CA1*CA2*CA3*dAlpha2KanUsual() + CA3*SA1*dAlpha3KanUsual() + CA3*SA1*SA2*dAlpha1KanUsual() + &
			& CA1*SA3*dAlpha1KanUsual() + CA1*SA2*SA3*dAlpha3KanUsual()
		dRR32 = -CA1*CA3*dAlpha3KanUsual() - CA2*CA3*SA1*dAlpha2KanUsual() - CA1*CA3*SA2*dAlpha1KanUsual() + &
			& SA1*SA3*dAlpha1KanUsual() + SA1*SA2*SA3*dAlpha3KanUsual()
		dRR33 = -CA3*SA2*dAlpha2KanUsual() - CA2*SA3*dAlpha3KanUsual()

		totalAmplitude = CS1S1S1f211*( &
&(0.125D0*CA1*CA3*EL*MH12*dAlpha1KanUsual())/(CB*MW*SW) + (0.125D0*CA1*CA22*CA3*EL*MH12*dAlpha1KanUsual())/(CB*MW*SW) + (0.0625D0*&
  &CA1*CA3*EL*MH22*dAlpha1KanUsual())/(CB*MW*SW) + (0.0625D0*CA1*CA22*CA3*EL*MH22*dAlpha1KanUsual())/(CB*MW*SW) + (0.125D0*CA3*EL&
  &*m12squared*SA1*dAlpha1KanUsual())/(CB*MW*SW) + (0.125D0*CA22*CA3*EL*m12squared*SA1*dAlpha1KanUsual())/(CB*MW*SW) - (1.125D0*C&
  &A1*CA3*EL*MH12*SA12*dAlpha1KanUsual())/(CB*MW*SW) - (1.125D0*CA1*CA22*CA3*EL*MH12*SA12*dAlpha1KanUsual())/(CB*MW*SW) - (0.5625&
  &D0*CA1*CA3*EL*MH22*SA12*dAlpha1KanUsual())/(CB*MW*SW) - (0.5625D0*CA1*CA22*CA3*EL*MH22*SA12*dAlpha1KanUsual())/(CB*MW*SW) - (0&
  &.125D0*CA1*CA3*EL*MH12*SA22*dAlpha1KanUsual())/(CB*MW*SW) - (0.0625D0*CA1*CA3*EL*MH22*SA22*dAlpha1KanUsual())/(CB*MW*SW) - (0.&
  &125D0*CA3*EL*m12squared*SA1*SA22*dAlpha1KanUsual())/(CB*MW*SW) + (1.125D0*CA1*CA3*EL*MH12*SA12*SA22*dAlpha1KanUsual())/(CB*MW*&
  &SW) + (0.5625D0*CA1*CA3*EL*MH22*SA12*SA22*dAlpha1KanUsual())/(CB*MW*SW) + (0.28125D0*CA1*EL*m12squared*SA2*SA3*dAlpha1KanUsual&
  &())/(CB*MW*SW) + (0.84375D0*CA1*CA22*EL*m12squared*SA2*SA3*dAlpha1KanUsual())/(CB*MW*SW) - (0.1875D0*EL*MH12*SA1*SA2*SA3*dAlph&
  &a1KanUsual())/(CB*MW*SW) - (0.5625D0*CA12*EL*MH12*SA1*SA2*SA3*dAlpha1KanUsual())/(CB*MW*SW) - (0.5625D0*CA22*EL*MH12*SA1*SA2*S&
  &A3*dAlpha1KanUsual())/(CB*MW*SW) - (1.6875D0*CA12*CA22*EL*MH12*SA1*SA2*SA3*dAlpha1KanUsual())/(CB*MW*SW) - (0.09375D0*EL*MH22*&
  &SA1*SA2*SA3*dAlpha1KanUsual())/(CB*MW*SW) - (0.28125D0*CA12*EL*MH22*SA1*SA2*SA3*dAlpha1KanUsual())/(CB*MW*SW) - (0.28125D0*CA2&
  &2*EL*MH22*SA1*SA2*SA3*dAlpha1KanUsual())/(CB*MW*SW) - (0.84375D0*CA12*CA22*EL*MH22*SA1*SA2*SA3*dAlpha1KanUsual())/(CB*MW*SW) +&
  & (0.21875D0*CA1*CA3*EL*m12squared*dAlpha1KanUsual())/(MW*SB*SW) + (0.21875D0*CA1*CA22*CA3*EL*m12squared*dAlpha1KanUsual())/(MW&
  &*SB*SW) - (0.15625D0*CA1*CA3*EL*m12squared*dAlpha1KanUsual())/(CB2*MW*SB*SW) - (0.15625D0*CA1*CA22*CA3*EL*m12squared*dAlpha1Ka&
  &nUsual())/(CB2*MW*SB*SW) + (0.125D0*CA3*EL*MH12*SA1*dAlpha1KanUsual())/(MW*SB*SW) - (1.125D0*CA12*CA3*EL*MH12*SA1*dAlpha1KanUs&
  &ual())/(MW*SB*SW) + (0.125D0*CA22*CA3*EL*MH12*SA1*dAlpha1KanUsual())/(MW*SB*SW) - (1.125D0*CA12*CA22*CA3*EL*MH12*SA1*dAlpha1Ka&
  &nUsual())/(MW*SB*SW) + (0.0625D0*CA3*EL*MH22*SA1*dAlpha1KanUsual())/(MW*SB*SW) - (0.5625D0*CA12*CA3*EL*MH22*SA1*dAlpha1KanUsua&
  &l())/(MW*SB*SW) + (0.0625D0*CA22*CA3*EL*MH22*SA1*dAlpha1KanUsual())/(MW*SB*SW) - (0.5625D0*CA12*CA22*CA3*EL*MH22*SA1*dAlpha1Ka&
  &nUsual())/(MW*SB*SW) + (1.6875D0*CA1*CA3*EL*m12squared*SA12*dAlpha1KanUsual())/(CB2*MW*SB*SW) + (1.6875D0*CA1*CA22*CA3*EL*m12s&
  &quared*SA12*dAlpha1KanUsual())/(CB2*MW*SB*SW) - (0.21875D0*CA1*CA3*EL*m12squared*SA22*dAlpha1KanUsual())/(MW*SB*SW) + (0.15625&
  &D0*CA1*CA3*EL*m12squared*SA22*dAlpha1KanUsual())/(CB2*MW*SB*SW) - (0.125D0*CA3*EL*MH12*SA1*SA22*dAlpha1KanUsual())/(MW*SB*SW) &
  &+ (1.125D0*CA12*CA3*EL*MH12*SA1*SA22*dAlpha1KanUsual())/(MW*SB*SW) - (0.0625D0*CA3*EL*MH22*SA1*SA22*dAlpha1KanUsual())/(MW*SB*&
  &SW) + (0.5625D0*CA12*CA3*EL*MH22*SA1*SA22*dAlpha1KanUsual())/(MW*SB*SW) - (1.6875D0*CA1*CA3*EL*m12squared*SA12*SA22*dAlpha1Kan&
  &Usual())/(CB2*MW*SB*SW) + (0.1875D0*CA1*EL*MH12*SA2*SA3*dAlpha1KanUsual())/(MW*SB*SW) + (0.5625D0*CA1*CA22*EL*MH12*SA2*SA3*dAl&
  &pha1KanUsual())/(MW*SB*SW) + (0.09375D0*CA1*EL*MH22*SA2*SA3*dAlpha1KanUsual())/(MW*SB*SW) + (0.28125D0*CA1*CA22*EL*MH22*SA2*SA&
  &3*dAlpha1KanUsual())/(MW*SB*SW) - (0.28125D0*EL*m12squared*SA1*SA2*SA3*dAlpha1KanUsual())/(MW*SB*SW) - (0.84375D0*CA22*EL*m12s&
  &quared*SA1*SA2*SA3*dAlpha1KanUsual())/(MW*SB*SW) + (0.1875D0*EL*m12squared*SA1*SA2*SA3*dAlpha1KanUsual())/(CB2*MW*SB*SW) + (0.&
  &84375D0*CA12*EL*m12squared*SA1*SA2*SA3*dAlpha1KanUsual())/(CB2*MW*SB*SW) + (0.5625D0*CA22*EL*m12squared*SA1*SA2*SA3*dAlpha1Kan&
  &Usual())/(CB2*MW*SB*SW) + (2.53125D0*CA12*CA22*EL*m12squared*SA1*SA2*SA3*dAlpha1KanUsual())/(CB2*MW*SB*SW) + (0.5625D0*CA1*EL*&
  &MH12*SA12*SA2*SA3*dAlpha1KanUsual())/(MW*SB*SW) + (1.6875D0*CA1*CA22*EL*MH12*SA12*SA2*SA3*dAlpha1KanUsual())/(MW*SB*SW) + (0.2&
  &8125D0*CA1*EL*MH22*SA12*SA2*SA3*dAlpha1KanUsual())/(MW*SB*SW) + (0.84375D0*CA1*CA22*EL*MH22*SA12*SA2*SA3*dAlpha1KanUsual())/(M&
  &W*SB*SW) - (0.0625D0*CA3*EL*m12squared*SA1*dAlpha1KanUsual())/(CB*MW*SB2*SW) + (1.6875D0*CA12*CA3*EL*m12squared*SA1*dAlpha1Kan&
  &Usual())/(CB*MW*SB2*SW) - (0.0625D0*CA22*CA3*EL*m12squared*SA1*dAlpha1KanUsual())/(CB*MW*SB2*SW) + (1.6875D0*CA12*CA22*CA3*EL*&
  &m12squared*SA1*dAlpha1KanUsual())/(CB*MW*SB2*SW) + (0.0625D0*CA3*EL*m12squared*SA1*SA22*dAlpha1KanUsual())/(CB*MW*SB2*SW) - (1&
  &.6875D0*CA12*CA3*EL*m12squared*SA1*SA22*dAlpha1KanUsual())/(CB*MW*SB2*SW) - (0.1875D0*CA1*EL*m12squared*SA2*SA3*dAlpha1KanUsua&
  &l())/(CB*MW*SB2*SW) - (0.5625D0*CA1*CA22*EL*m12squared*SA2*SA3*dAlpha1KanUsual())/(CB*MW*SB2*SW) - (0.84375D0*CA1*EL*m12square&
  &d*SA12*SA2*SA3*dAlpha1KanUsual())/(CB*MW*SB2*SW) - (2.53125D0*CA1*CA22*EL*m12squared*SA12*SA2*SA3*dAlpha1KanUsual())/(CB*MW*SB&
  &2*SW) - (0.125D0*CA3*EL*m12squared*SA1*dAlpha1KanUsual())/(MW*SB*SW*TB) - (0.125D0*CA22*CA3*EL*m12squared*SA1*dAlpha1KanUsual(&
  &))/(MW*SB*SW*TB) + (0.125D0*CA3*EL*m12squared*SA1*SA22*dAlpha1KanUsual())/(MW*SB*SW*TB) - (0.09375D0*CA1*EL*m12squared*SA2*SA3&
  &*dAlpha1KanUsual())/(MW*SB*SW*TB) - (0.28125D0*CA1*CA22*EL*m12squared*SA2*SA3*dAlpha1KanUsual())/(MW*SB*SW*TB) - (0.03125D0*CA&
  &1*CA3*EL*m12squared*TB*dAlpha1KanUsual())/(CB*MW*SW) - (0.03125D0*CA1*CA22*CA3*EL*m12squared*TB*dAlpha1KanUsual())/(CB*MW*SW) &
  &+ (0.03125D0*CA1*CA3*EL*m12squared*SA22*TB*dAlpha1KanUsual())/(CB*MW*SW) + (0.09375D0*EL*m12squared*SA1*SA2*SA3*TB*dAlpha1KanU&
  &sual())/(CB*MW*SW) + (0.28125D0*CA22*EL*m12squared*SA1*SA2*SA3*TB*dAlpha1KanUsual())/(CB*MW*SW) + (0.5D0*CA1*CA2*CA3*EL*m12squ&
  &ared*SA2*dAlpha2KanUsual())/(CB*MW*SW) - (0.5D0*CA2*CA3*EL*MH12*SA1*SA2*dAlpha2KanUsual())/(CB*MW*SW) - (1.5D0*CA12*CA2*CA3*EL&
  &*MH12*SA1*SA2*dAlpha2KanUsual())/(CB*MW*SW) - (0.25D0*CA2*CA3*EL*MH22*SA1*SA2*dAlpha2KanUsual())/(CB*MW*SW) - (0.75D0*CA12*CA2&
  &*CA3*EL*MH22*SA1*SA2*dAlpha2KanUsual())/(CB*MW*SW) + (0.1875D0*CA1*CA2*EL*MH12*SA3*dAlpha2KanUsual())/(CB*MW*SW) + (0.09375D0*&
  &CA1*CA2*EL*MH22*SA3*dAlpha2KanUsual())/(CB*MW*SW) + (0.28125D0*CA2*EL*m12squared*SA1*SA3*dAlpha2KanUsual())/(CB*MW*SW) - (0.18&
  &75D0*CA1*CA2*EL*MH12*SA12*SA3*dAlpha2KanUsual())/(CB*MW*SW) - (0.09375D0*CA1*CA2*EL*MH22*SA12*SA3*dAlpha2KanUsual())/(CB*MW*SW&
  &) - (1.6875D0*CA1*CA2*EL*MH12*SA22*SA3*dAlpha2KanUsual())/(CB*MW*SW) - (0.84375D0*CA1*CA2*EL*MH22*SA22*SA3*dAlpha2KanUsual())/&
  &(CB*MW*SW) - (2.53125D0*CA2*EL*m12squared*SA1*SA22*SA3*dAlpha2KanUsual())/(CB*MW*SW) + (1.6875D0*CA1*CA2*EL*MH12*SA12*SA22*SA3&
  &*dAlpha2KanUsual())/(CB*MW*SW) + (0.84375D0*CA1*CA2*EL*MH22*SA12*SA22*SA3*dAlpha2KanUsual())/(CB*MW*SW) + (0.5D0*CA1*CA2*CA3*E&
  &L*MH12*SA2*dAlpha2KanUsual())/(MW*SB*SW) + (0.25D0*CA1*CA2*CA3*EL*MH22*SA2*dAlpha2KanUsual())/(MW*SB*SW) - (0.875D0*CA2*CA3*EL&
  &*m12squared*SA1*SA2*dAlpha2KanUsual())/(MW*SB*SW) + (0.625D0*CA2*CA3*EL*m12squared*SA1*SA2*dAlpha2KanUsual())/(CB2*MW*SB*SW) +&
  & (2.25D0*CA12*CA2*CA3*EL*m12squared*SA1*SA2*dAlpha2KanUsual())/(CB2*MW*SB*SW) + (1.5D0*CA1*CA2*CA3*EL*MH12*SA12*SA2*dAlpha2Kan&
  &Usual())/(MW*SB*SW) + (0.75D0*CA1*CA2*CA3*EL*MH22*SA12*SA2*dAlpha2KanUsual())/(MW*SB*SW) + (0.28125D0*CA1*CA2*EL*m12squared*SA&
  &3*dAlpha2KanUsual())/(MW*SB*SW) - (0.1875D0*CA1*CA2*EL*m12squared*SA3*dAlpha2KanUsual())/(CB2*MW*SB*SW) + (0.1875D0*CA2*EL*MH1&
  &2*SA1*SA3*dAlpha2KanUsual())/(MW*SB*SW) - (0.1875D0*CA12*CA2*EL*MH12*SA1*SA3*dAlpha2KanUsual())/(MW*SB*SW) + (0.09375D0*CA2*EL&
  &*MH22*SA1*SA3*dAlpha2KanUsual())/(MW*SB*SW) - (0.09375D0*CA12*CA2*EL*MH22*SA1*SA3*dAlpha2KanUsual())/(MW*SB*SW) + (0.28125D0*C&
  &A1*CA2*EL*m12squared*SA12*SA3*dAlpha2KanUsual())/(CB2*MW*SB*SW) - (2.53125D0*CA1*CA2*EL*m12squared*SA22*SA3*dAlpha2KanUsual())&
  &/(MW*SB*SW) + (1.6875D0*CA1*CA2*EL*m12squared*SA22*SA3*dAlpha2KanUsual())/(CB2*MW*SB*SW) - (1.6875D0*CA2*EL*MH12*SA1*SA22*SA3*&
  &dAlpha2KanUsual())/(MW*SB*SW) + (1.6875D0*CA12*CA2*EL*MH12*SA1*SA22*SA3*dAlpha2KanUsual())/(MW*SB*SW) - (0.84375D0*CA2*EL*MH22&
  &*SA1*SA22*SA3*dAlpha2KanUsual())/(MW*SB*SW) + (0.84375D0*CA12*CA2*EL*MH22*SA1*SA22*SA3*dAlpha2KanUsual())/(MW*SB*SW) - (2.5312&
  &5D0*CA1*CA2*EL*m12squared*SA12*SA22*SA3*dAlpha2KanUsual())/(CB2*MW*SB*SW) - (0.25D0*CA1*CA2*CA3*EL*m12squared*SA2*dAlpha2KanUs&
  &ual())/(CB*MW*SB2*SW) - (2.25D0*CA1*CA2*CA3*EL*m12squared*SA12*SA2*dAlpha2KanUsual())/(CB*MW*SB2*SW) - (0.1875D0*CA2*EL*m12squ&
  &ared*SA1*SA3*dAlpha2KanUsual())/(CB*MW*SB2*SW) + (0.28125D0*CA12*CA2*EL*m12squared*SA1*SA3*dAlpha2KanUsual())/(CB*MW*SB2*SW) +&
  & (1.6875D0*CA2*EL*m12squared*SA1*SA22*SA3*dAlpha2KanUsual())/(CB*MW*SB2*SW) - (2.53125D0*CA12*CA2*EL*m12squared*SA1*SA22*SA3*d&
  &Alpha2KanUsual())/(CB*MW*SB2*SW) - (0.5D0*CA1*CA2*CA3*EL*m12squared*SA2*dAlpha2KanUsual())/(MW*SB*SW*TB) - (0.09375D0*CA2*EL*m&
  &12squared*SA1*SA3*dAlpha2KanUsual())/(MW*SB*SW*TB) + (0.84375D0*CA2*EL*m12squared*SA1*SA22*SA3*dAlpha2KanUsual())/(MW*SB*SW*TB&
  &) + (0.125D0*CA2*CA3*EL*m12squared*SA1*SA2*TB*dAlpha2KanUsual())/(CB*MW*SW) - (0.09375D0*CA1*CA2*EL*m12squared*SA3*TB*dAlpha2K&
  &anUsual())/(CB*MW*SW) + (0.84375D0*CA1*CA2*EL*m12squared*SA22*SA3*TB*dAlpha2KanUsual())/(CB*MW*SW) + (0.5D0*MH12*SA2*SA3*dAlph&
  &a2KanUsual())/vS - (4.5D0*CA22*MH12*SA2*SA3*dAlpha2KanUsual())/vS + (0.25D0*MH22*SA2*SA3*dAlpha2KanUsual())/vS - (2.25D0*CA22*&
  &MH22*SA2*SA3*dAlpha2KanUsual())/vS + (0.1875D0*CA1*CA3*EL*MH12*SA2*dAlpha3KanUsual())/(CB*MW*SW) + (0.5625D0*CA1*CA22*CA3*EL*M&
  &H12*SA2*dAlpha3KanUsual())/(CB*MW*SW) + (0.09375D0*CA1*CA3*EL*MH22*SA2*dAlpha3KanUsual())/(CB*MW*SW) + (0.28125D0*CA1*CA22*CA3&
  &*EL*MH22*SA2*dAlpha3KanUsual())/(CB*MW*SW) + (0.28125D0*CA3*EL*m12squared*SA1*SA2*dAlpha3KanUsual())/(CB*MW*SW) + (0.84375D0*C&
  &A22*CA3*EL*m12squared*SA1*SA2*dAlpha3KanUsual())/(CB*MW*SW) - (0.1875D0*CA1*CA3*EL*MH12*SA12*SA2*dAlpha3KanUsual())/(CB*MW*SW)&
  & - (0.5625D0*CA1*CA22*CA3*EL*MH12*SA12*SA2*dAlpha3KanUsual())/(CB*MW*SW) - (0.09375D0*CA1*CA3*EL*MH22*SA12*SA2*dAlpha3KanUsual&
  &())/(CB*MW*SW) - (0.28125D0*CA1*CA22*CA3*EL*MH22*SA12*SA2*dAlpha3KanUsual())/(CB*MW*SW) + (0.125D0*CA1*EL*m12squared*SA3*dAlph&
  &a3KanUsual())/(CB*MW*SW) + (0.125D0*CA1*CA22*EL*m12squared*SA3*dAlpha3KanUsual())/(CB*MW*SW) - (0.125D0*EL*MH12*SA1*SA3*dAlpha&
  &3KanUsual())/(CB*MW*SW) - (0.375D0*CA12*EL*MH12*SA1*SA3*dAlpha3KanUsual())/(CB*MW*SW) - (0.125D0*CA22*EL*MH12*SA1*SA3*dAlpha3K&
  &anUsual())/(CB*MW*SW) - (0.375D0*CA12*CA22*EL*MH12*SA1*SA3*dAlpha3KanUsual())/(CB*MW*SW) - (0.0625D0*EL*MH22*SA1*SA3*dAlpha3Ka&
  &nUsual())/(CB*MW*SW) - (0.1875D0*CA12*EL*MH22*SA1*SA3*dAlpha3KanUsual())/(CB*MW*SW) - (0.0625D0*CA22*EL*MH22*SA1*SA3*dAlpha3Ka&
  &nUsual())/(CB*MW*SW) - (0.1875D0*CA12*CA22*EL*MH22*SA1*SA3*dAlpha3KanUsual())/(CB*MW*SW) - (0.125D0*CA1*EL*m12squared*SA22*SA3&
  &*dAlpha3KanUsual())/(CB*MW*SW) + (0.125D0*EL*MH12*SA1*SA22*SA3*dAlpha3KanUsual())/(CB*MW*SW) + (0.375D0*CA12*EL*MH12*SA1*SA22*&
  &SA3*dAlpha3KanUsual())/(CB*MW*SW) + (0.0625D0*EL*MH22*SA1*SA22*SA3*dAlpha3KanUsual())/(CB*MW*SW) + (0.1875D0*CA12*EL*MH22*SA1*&
  &SA22*SA3*dAlpha3KanUsual())/(CB*MW*SW) + (0.28125D0*CA1*CA3*EL*m12squared*SA2*dAlpha3KanUsual())/(MW*SB*SW) + (0.84375D0*CA1*C&
  &A22*CA3*EL*m12squared*SA2*dAlpha3KanUsual())/(MW*SB*SW) - (0.1875D0*CA1*CA3*EL*m12squared*SA2*dAlpha3KanUsual())/(CB2*MW*SB*SW&
  &) - (0.5625D0*CA1*CA22*CA3*EL*m12squared*SA2*dAlpha3KanUsual())/(CB2*MW*SB*SW) + (0.1875D0*CA3*EL*MH12*SA1*SA2*dAlpha3KanUsual&
  &())/(MW*SB*SW) - (0.1875D0*CA12*CA3*EL*MH12*SA1*SA2*dAlpha3KanUsual())/(MW*SB*SW) + (0.5625D0*CA22*CA3*EL*MH12*SA1*SA2*dAlpha3&
  &KanUsual())/(MW*SB*SW) - (0.5625D0*CA12*CA22*CA3*EL*MH12*SA1*SA2*dAlpha3KanUsual())/(MW*SB*SW) + (0.09375D0*CA3*EL*MH22*SA1*SA&
  &2*dAlpha3KanUsual())/(MW*SB*SW) - (0.09375D0*CA12*CA3*EL*MH22*SA1*SA2*dAlpha3KanUsual())/(MW*SB*SW) + (0.28125D0*CA22*CA3*EL*M&
  &H22*SA1*SA2*dAlpha3KanUsual())/(MW*SB*SW) - (0.28125D0*CA12*CA22*CA3*EL*MH22*SA1*SA2*dAlpha3KanUsual())/(MW*SB*SW) + (0.28125D&
  &0*CA1*CA3*EL*m12squared*SA12*SA2*dAlpha3KanUsual())/(CB2*MW*SB*SW) + (0.84375D0*CA1*CA22*CA3*EL*m12squared*SA12*SA2*dAlpha3Kan&
  &Usual())/(CB2*MW*SB*SW) + (0.125D0*CA1*EL*MH12*SA3*dAlpha3KanUsual())/(MW*SB*SW) + (0.125D0*CA1*CA22*EL*MH12*SA3*dAlpha3KanUsu&
  &al())/(MW*SB*SW) + (0.0625D0*CA1*EL*MH22*SA3*dAlpha3KanUsual())/(MW*SB*SW) + (0.0625D0*CA1*CA22*EL*MH22*SA3*dAlpha3KanUsual())&
  &/(MW*SB*SW) - (0.21875D0*EL*m12squared*SA1*SA3*dAlpha3KanUsual())/(MW*SB*SW) - (0.21875D0*CA22*EL*m12squared*SA1*SA3*dAlpha3Ka&
  &nUsual())/(MW*SB*SW) + (0.15625D0*EL*m12squared*SA1*SA3*dAlpha3KanUsual())/(CB2*MW*SB*SW) + (0.5625D0*CA12*EL*m12squared*SA1*S&
  &A3*dAlpha3KanUsual())/(CB2*MW*SB*SW) + (0.15625D0*CA22*EL*m12squared*SA1*SA3*dAlpha3KanUsual())/(CB2*MW*SB*SW) + (0.5625D0*CA1&
  &2*CA22*EL*m12squared*SA1*SA3*dAlpha3KanUsual())/(CB2*MW*SB*SW) + (0.375D0*CA1*EL*MH12*SA12*SA3*dAlpha3KanUsual())/(MW*SB*SW) +&
  & (0.375D0*CA1*CA22*EL*MH12*SA12*SA3*dAlpha3KanUsual())/(MW*SB*SW) + (0.1875D0*CA1*EL*MH22*SA12*SA3*dAlpha3KanUsual())/(MW*SB*S&
  &W) + (0.1875D0*CA1*CA22*EL*MH22*SA12*SA3*dAlpha3KanUsual())/(MW*SB*SW) - (0.125D0*CA1*EL*MH12*SA22*SA3*dAlpha3KanUsual())/(MW*&
  &SB*SW) - (0.0625D0*CA1*EL*MH22*SA22*SA3*dAlpha3KanUsual())/(MW*SB*SW) + (0.21875D0*EL*m12squared*SA1*SA22*SA3*dAlpha3KanUsual(&
  &))/(MW*SB*SW) - (0.15625D0*EL*m12squared*SA1*SA22*SA3*dAlpha3KanUsual())/(CB2*MW*SB*SW) - (0.5625D0*CA12*EL*m12squared*SA1*SA2&
  &2*SA3*dAlpha3KanUsual())/(CB2*MW*SB*SW) - (0.375D0*CA1*EL*MH12*SA12*SA22*SA3*dAlpha3KanUsual())/(MW*SB*SW) - (0.1875D0*CA1*EL*&
  &MH22*SA12*SA22*SA3*dAlpha3KanUsual())/(MW*SB*SW) - (0.1875D0*CA3*EL*m12squared*SA1*SA2*dAlpha3KanUsual())/(CB*MW*SB2*SW) + (0.&
  &28125D0*CA12*CA3*EL*m12squared*SA1*SA2*dAlpha3KanUsual())/(CB*MW*SB2*SW) - (0.5625D0*CA22*CA3*EL*m12squared*SA1*SA2*dAlpha3Kan&
  &Usual())/(CB*MW*SB2*SW) + (0.84375D0*CA12*CA22*CA3*EL*m12squared*SA1*SA2*dAlpha3KanUsual())/(CB*MW*SB2*SW) - (0.0625D0*CA1*EL*&
  &m12squared*SA3*dAlpha3KanUsual())/(CB*MW*SB2*SW) - (0.0625D0*CA1*CA22*EL*m12squared*SA3*dAlpha3KanUsual())/(CB*MW*SB2*SW) - (0&
  &.5625D0*CA1*EL*m12squared*SA12*SA3*dAlpha3KanUsual())/(CB*MW*SB2*SW) - (0.5625D0*CA1*CA22*EL*m12squared*SA12*SA3*dAlpha3KanUsu&
  &al())/(CB*MW*SB2*SW) + (0.0625D0*CA1*EL*m12squared*SA22*SA3*dAlpha3KanUsual())/(CB*MW*SB2*SW) + (0.5625D0*CA1*EL*m12squared*SA&
  &12*SA22*SA3*dAlpha3KanUsual())/(CB*MW*SB2*SW) - (0.09375D0*CA3*EL*m12squared*SA1*SA2*dAlpha3KanUsual())/(MW*SB*SW*TB) - (0.281&
  &25D0*CA22*CA3*EL*m12squared*SA1*SA2*dAlpha3KanUsual())/(MW*SB*SW*TB) - (0.125D0*CA1*EL*m12squared*SA3*dAlpha3KanUsual())/(MW*S&
  &B*SW*TB) - (0.125D0*CA1*CA22*EL*m12squared*SA3*dAlpha3KanUsual())/(MW*SB*SW*TB) + (0.125D0*CA1*EL*m12squared*SA22*SA3*dAlpha3K&
  &anUsual())/(MW*SB*SW*TB) - (0.09375D0*CA1*CA3*EL*m12squared*SA2*TB*dAlpha3KanUsual())/(CB*MW*SW) - (0.28125D0*CA1*CA22*CA3*EL*&
  &m12squared*SA2*TB*dAlpha3KanUsual())/(CB*MW*SW) + (0.03125D0*EL*m12squared*SA1*SA3*TB*dAlpha3KanUsual())/(CB*MW*SW) + (0.03125&
  &D0*CA22*EL*m12squared*SA1*SA3*TB*dAlpha3KanUsual())/(CB*MW*SW) - (0.03125D0*EL*m12squared*SA1*SA22*SA3*TB*dAlpha3KanUsual())/(&
  &CB*MW*SW) - (0.5D0*CA2*CA3*MH12*dAlpha3KanUsual())/vS - (0.25D0*CA2*CA3*MH22*dAlpha3KanUsual())/vS - (1.5D0*CA2*CA3*MH12*SA22*&
  &dAlpha3KanUsual())/vS - (0.75D0*CA2*CA3*MH22*SA22*dAlpha3KanUsual())/vS + (0.015625D0*CA3*EL*m12squared*SA1*dBeta1KanUsual())/&
  &(CB*MW*SW) + (0.015625D0*CA22*CA3*EL*m12squared*SA1*dBeta1KanUsual())/(CB*MW*SW) - (0.015625D0*CA3*EL*m12squared*SA1*SA22*dBet&
  &a1KanUsual())/(CB*MW*SW) + (0.09375D0*CA1*EL*m12squared*SA2*SA3*dBeta1KanUsual())/(CB*MW*SW) + (0.28125D0*CA1*CA22*EL*m12squar&
  &ed*SA2*SA3*dBeta1KanUsual())/(CB*MW*SW) - (0.0625D0*CA1*CA3*EL*m12squared*dBeta1KanUsual())/(MW*SB*SW) - (0.0625D0*CA1*CA22*CA&
  &3*EL*m12squared*dBeta1KanUsual())/(MW*SB*SW) + (0.0625D0*CA1*CA3*EL*m12squared*dBeta1KanUsual())/(CB2*MW*SB*SW) + (0.0625D0*CA&
  &1*CA22*CA3*EL*m12squared*dBeta1KanUsual())/(CB2*MW*SB*SW) - (0.0625D0*CA3*EL*MH12*SA1*dBeta1KanUsual())/(MW*SB*SW) - (0.1875D0&
  &*CA12*CA3*EL*MH12*SA1*dBeta1KanUsual())/(MW*SB*SW) - (0.0625D0*CA22*CA3*EL*MH12*SA1*dBeta1KanUsual())/(MW*SB*SW) - (0.1875D0*C&
  &A12*CA22*CA3*EL*MH12*SA1*dBeta1KanUsual())/(MW*SB*SW) + (0.0625D0*CA3*EL*MH12*SA1*dBeta1KanUsual())/(CB2*MW*SB*SW) + (0.1875D0&
  &*CA12*CA3*EL*MH12*SA1*dBeta1KanUsual())/(CB2*MW*SB*SW) + (0.0625D0*CA22*CA3*EL*MH12*SA1*dBeta1KanUsual())/(CB2*MW*SB*SW) + (0.&
  &1875D0*CA12*CA22*CA3*EL*MH12*SA1*dBeta1KanUsual())/(CB2*MW*SB*SW) - (0.03125D0*CA3*EL*MH22*SA1*dBeta1KanUsual())/(MW*SB*SW) - &
  &(0.09375D0*CA12*CA3*EL*MH22*SA1*dBeta1KanUsual())/(MW*SB*SW) - (0.03125D0*CA22*CA3*EL*MH22*SA1*dBeta1KanUsual())/(MW*SB*SW) - &
  &(0.09375D0*CA12*CA22*CA3*EL*MH22*SA1*dBeta1KanUsual())/(MW*SB*SW) + (0.03125D0*CA3*EL*MH22*SA1*dBeta1KanUsual())/(CB2*MW*SB*SW&
  &) + (0.09375D0*CA12*CA3*EL*MH22*SA1*dBeta1KanUsual())/(CB2*MW*SB*SW) + (0.03125D0*CA22*CA3*EL*MH22*SA1*dBeta1KanUsual())/(CB2*&
  &MW*SB*SW) + (0.09375D0*CA12*CA22*CA3*EL*MH22*SA1*dBeta1KanUsual())/(CB2*MW*SB*SW) + (0.5625D0*CA1*CA3*EL*m12squared*SA12*dBeta&
  &1KanUsual())/(CB2*MW*SB*SW) + (0.5625D0*CA1*CA22*CA3*EL*m12squared*SA12*dBeta1KanUsual())/(CB2*MW*SB*SW) + (0.0625D0*CA1*CA3*E&
  &L*m12squared*SA22*dBeta1KanUsual())/(MW*SB*SW) - (0.0625D0*CA1*CA3*EL*m12squared*SA22*dBeta1KanUsual())/(CB2*MW*SB*SW) + (0.06&
  &25D0*CA3*EL*MH12*SA1*SA22*dBeta1KanUsual())/(MW*SB*SW) + (0.1875D0*CA12*CA3*EL*MH12*SA1*SA22*dBeta1KanUsual())/(MW*SB*SW) - (0&
  &.0625D0*CA3*EL*MH12*SA1*SA22*dBeta1KanUsual())/(CB2*MW*SB*SW) - (0.1875D0*CA12*CA3*EL*MH12*SA1*SA22*dBeta1KanUsual())/(CB2*MW*&
  &SB*SW) + (0.03125D0*CA3*EL*MH22*SA1*SA22*dBeta1KanUsual())/(MW*SB*SW) + (0.09375D0*CA12*CA3*EL*MH22*SA1*SA22*dBeta1KanUsual())&
  &/(MW*SB*SW) - (0.03125D0*CA3*EL*MH22*SA1*SA22*dBeta1KanUsual())/(CB2*MW*SB*SW) - (0.09375D0*CA12*CA3*EL*MH22*SA1*SA22*dBeta1Ka&
  &nUsual())/(CB2*MW*SB*SW) - (0.5625D0*CA1*CA3*EL*m12squared*SA12*SA22*dBeta1KanUsual())/(CB2*MW*SB*SW) - (0.09375D0*CA1*EL*MH12&
  &*SA2*SA3*dBeta1KanUsual())/(MW*SB*SW) - (0.28125D0*CA1*CA22*EL*MH12*SA2*SA3*dBeta1KanUsual())/(MW*SB*SW) + (0.09375D0*CA1*EL*M&
  &H12*SA2*SA3*dBeta1KanUsual())/(CB2*MW*SB*SW) + (0.28125D0*CA1*CA22*EL*MH12*SA2*SA3*dBeta1KanUsual())/(CB2*MW*SB*SW) - (0.04687&
  &5D0*CA1*EL*MH22*SA2*SA3*dBeta1KanUsual())/(MW*SB*SW) - (0.140625D0*CA1*CA22*EL*MH22*SA2*SA3*dBeta1KanUsual())/(MW*SB*SW) + (0.&
  &046875D0*CA1*EL*MH22*SA2*SA3*dBeta1KanUsual())/(CB2*MW*SB*SW) + (0.140625D0*CA1*CA22*EL*MH22*SA2*SA3*dBeta1KanUsual())/(CB2*MW&
  &*SB*SW) - (0.140625D0*EL*m12squared*SA1*SA2*SA3*dBeta1KanUsual())/(MW*SB*SW) - (0.421875D0*CA22*EL*m12squared*SA1*SA2*SA3*dBet&
  &a1KanUsual())/(MW*SB*SW) - (0.046875D0*EL*m12squared*SA1*SA2*SA3*dBeta1KanUsual())/(CB2*MW*SB*SW) + (0.28125D0*CA12*EL*m12squa&
  &red*SA1*SA2*SA3*dBeta1KanUsual())/(CB2*MW*SB*SW) - (0.140625D0*CA22*EL*m12squared*SA1*SA2*SA3*dBeta1KanUsual())/(CB2*MW*SB*SW)&
  & + (0.84375D0*CA12*CA22*EL*m12squared*SA1*SA2*SA3*dBeta1KanUsual())/(CB2*MW*SB*SW) + (0.09375D0*CA1*EL*MH12*SA12*SA2*SA3*dBeta&
  &1KanUsual())/(MW*SB*SW) + (0.28125D0*CA1*CA22*EL*MH12*SA12*SA2*SA3*dBeta1KanUsual())/(MW*SB*SW) - (0.09375D0*CA1*EL*MH12*SA12*&
  &SA2*SA3*dBeta1KanUsual())/(CB2*MW*SB*SW) - (0.28125D0*CA1*CA22*EL*MH12*SA12*SA2*SA3*dBeta1KanUsual())/(CB2*MW*SB*SW) + (0.0468&
  &75D0*CA1*EL*MH22*SA12*SA2*SA3*dBeta1KanUsual())/(MW*SB*SW) + (0.140625D0*CA1*CA22*EL*MH22*SA12*SA2*SA3*dBeta1KanUsual())/(MW*S&
  &B*SW) - (0.046875D0*CA1*EL*MH22*SA12*SA2*SA3*dBeta1KanUsual())/(CB2*MW*SB*SW) - (0.140625D0*CA1*CA22*EL*MH22*SA12*SA2*SA3*dBet&
  &a1KanUsual())/(CB2*MW*SB*SW) + (0.203125D0*CA3*EL*m12squared*SA1*dBeta1KanUsual())/(CB*MW*SB2*SW) + (0.84375D0*CA12*CA3*EL*m12&
  &squared*SA1*dBeta1KanUsual())/(CB*MW*SB2*SW) + (0.203125D0*CA22*CA3*EL*m12squared*SA1*dBeta1KanUsual())/(CB*MW*SB2*SW) + (0.84&
  &375D0*CA12*CA22*CA3*EL*m12squared*SA1*dBeta1KanUsual())/(CB*MW*SB2*SW) - (0.203125D0*CA3*EL*m12squared*SA1*SA22*dBeta1KanUsual&
  &())/(CB*MW*SB2*SW) - (0.84375D0*CA12*CA3*EL*m12squared*SA1*SA22*dBeta1KanUsual())/(CB*MW*SB2*SW) + (0.10546875D0*CA1*EL*m12squ&
  &ared*SA2*SA3*dBeta1KanUsual())/(CB*MW*SB2*SW) + (0.31640625D0*CA1*CA22*EL*m12squared*SA2*SA3*dBeta1KanUsual())/(CB*MW*SB2*SW) &
  &- (0.38671875D0*CA1*EL*m12squared*SA12*SA2*SA3*dBeta1KanUsual())/(CB*MW*SB2*SW) - (1.16015625D0*CA1*CA22*EL*m12squared*SA12*SA&
  &2*SA3*dBeta1KanUsual())/(CB*MW*SB2*SW) + (0.125D0*CA1*CA3*EL*MH12*dBeta1KanUsual())/(MW*SB*SW*TB) + (0.125D0*CA1*CA22*CA3*EL*M&
  &H12*dBeta1KanUsual())/(MW*SB*SW*TB) + (0.0625D0*CA1*CA3*EL*MH22*dBeta1KanUsual())/(MW*SB*SW*TB) + (0.0625D0*CA1*CA22*CA3*EL*MH&
  &22*dBeta1KanUsual())/(MW*SB*SW*TB) - (0.203125D0*CA3*EL*m12squared*SA1*dBeta1KanUsual())/(MW*SB*SW*TB) - (0.203125D0*CA22*CA3*&
  &EL*m12squared*SA1*dBeta1KanUsual())/(MW*SB*SW*TB) + (0.375D0*CA1*CA3*EL*MH12*SA12*dBeta1KanUsual())/(MW*SB*SW*TB) + (0.375D0*C&
  &A1*CA22*CA3*EL*MH12*SA12*dBeta1KanUsual())/(MW*SB*SW*TB) + (0.1875D0*CA1*CA3*EL*MH22*SA12*dBeta1KanUsual())/(MW*SB*SW*TB) + (0&
  &.1875D0*CA1*CA22*CA3*EL*MH22*SA12*dBeta1KanUsual())/(MW*SB*SW*TB) - (0.125D0*CA1*CA3*EL*MH12*SA22*dBeta1KanUsual())/(MW*SB*SW*&
  &TB) - (0.0625D0*CA1*CA3*EL*MH22*SA22*dBeta1KanUsual())/(MW*SB*SW*TB) + (0.203125D0*CA3*EL*m12squared*SA1*SA22*dBeta1KanUsual()&
  &)/(MW*SB*SW*TB) - (0.375D0*CA1*CA3*EL*MH12*SA12*SA22*dBeta1KanUsual())/(MW*SB*SW*TB) - (0.1875D0*CA1*CA3*EL*MH22*SA12*SA22*dBe&
  &ta1KanUsual())/(MW*SB*SW*TB) - (0.140625D0*CA1*EL*m12squared*SA2*SA3*dBeta1KanUsual())/(MW*SB*SW*TB) - (0.421875D0*CA1*CA22*EL&
  &*m12squared*SA2*SA3*dBeta1KanUsual())/(MW*SB*SW*TB) - (0.1875D0*EL*MH12*SA1*SA2*SA3*dBeta1KanUsual())/(MW*SB*SW*TB) + (0.1875D&
  &0*CA12*EL*MH12*SA1*SA2*SA3*dBeta1KanUsual())/(MW*SB*SW*TB) - (0.5625D0*CA22*EL*MH12*SA1*SA2*SA3*dBeta1KanUsual())/(MW*SB*SW*TB&
  &) + (0.5625D0*CA12*CA22*EL*MH12*SA1*SA2*SA3*dBeta1KanUsual())/(MW*SB*SW*TB) - (0.09375D0*EL*MH22*SA1*SA2*SA3*dBeta1KanUsual())&
  &/(MW*SB*SW*TB) + (0.09375D0*CA12*EL*MH22*SA1*SA2*SA3*dBeta1KanUsual())/(MW*SB*SW*TB) - (0.28125D0*CA22*EL*MH22*SA1*SA2*SA3*dBe&
  &ta1KanUsual())/(MW*SB*SW*TB) + (0.28125D0*CA12*CA22*EL*MH22*SA1*SA2*SA3*dBeta1KanUsual())/(MW*SB*SW*TB) - (0.125D0*CA1*CA3*EL*&
  &m12squared*TB*dBeta1KanUsual())/(CB*MW*SW) - (0.125D0*CA1*CA22*CA3*EL*m12squared*TB*dBeta1KanUsual())/(CB*MW*SW) + (0.0625D0*C&
  &A3*EL*MH12*SA1*TB*dBeta1KanUsual())/(CB*MW*SW) + (0.1875D0*CA12*CA3*EL*MH12*SA1*TB*dBeta1KanUsual())/(CB*MW*SW) + (0.0625D0*CA&
  &22*CA3*EL*MH12*SA1*TB*dBeta1KanUsual())/(CB*MW*SW) + (0.1875D0*CA12*CA22*CA3*EL*MH12*SA1*TB*dBeta1KanUsual())/(CB*MW*SW) + (0.&
  &03125D0*CA3*EL*MH22*SA1*TB*dBeta1KanUsual())/(CB*MW*SW) + (0.09375D0*CA12*CA3*EL*MH22*SA1*TB*dBeta1KanUsual())/(CB*MW*SW) + (0&
  &.03125D0*CA22*CA3*EL*MH22*SA1*TB*dBeta1KanUsual())/(CB*MW*SW) + (0.09375D0*CA12*CA22*CA3*EL*MH22*SA1*TB*dBeta1KanUsual())/(CB*&
  &MW*SW) + (0.125D0*CA1*CA3*EL*m12squared*SA22*TB*dBeta1KanUsual())/(CB*MW*SW) - (0.0625D0*CA3*EL*MH12*SA1*SA22*TB*dBeta1KanUsua&
  &l())/(CB*MW*SW) - (0.1875D0*CA12*CA3*EL*MH12*SA1*SA22*TB*dBeta1KanUsual())/(CB*MW*SW) - (0.03125D0*CA3*EL*MH22*SA1*SA22*TB*dBe&
  &ta1KanUsual())/(CB*MW*SW) - (0.09375D0*CA12*CA3*EL*MH22*SA1*SA22*TB*dBeta1KanUsual())/(CB*MW*SW) + (0.09375D0*CA1*EL*MH12*SA2*&
  &SA3*TB*dBeta1KanUsual())/(CB*MW*SW) + (0.28125D0*CA1*CA22*EL*MH12*SA2*SA3*TB*dBeta1KanUsual())/(CB*MW*SW) + (0.046875D0*CA1*EL&
  &*MH22*SA2*SA3*TB*dBeta1KanUsual())/(CB*MW*SW) + (0.140625D0*CA1*CA22*EL*MH22*SA2*SA3*TB*dBeta1KanUsual())/(CB*MW*SW) + (0.1406&
  &25D0*EL*m12squared*SA1*SA2*SA3*TB*dBeta1KanUsual())/(CB*MW*SW) + (0.421875D0*CA22*EL*m12squared*SA1*SA2*SA3*TB*dBeta1KanUsual(&
  &))/(CB*MW*SW) - (0.09375D0*CA1*EL*MH12*SA12*SA2*SA3*TB*dBeta1KanUsual())/(CB*MW*SW) - (0.28125D0*CA1*CA22*EL*MH12*SA12*SA2*SA3&
  &*TB*dBeta1KanUsual())/(CB*MW*SW) - (0.046875D0*CA1*EL*MH22*SA12*SA2*SA3*TB*dBeta1KanUsual())/(CB*MW*SW) - (0.140625D0*CA1*CA22&
  &*EL*MH22*SA12*SA2*SA3*TB*dBeta1KanUsual())/(CB*MW*SW) - (0.1875D0*CA1*CA3*EL*m12squared*dBeta1KanUsual())/(MW*SB*SW*TB2) - (0.&
  &1875D0*CA1*CA22*CA3*EL*m12squared*dBeta1KanUsual())/(MW*SB*SW*TB2) + (0.1875D0*CA1*CA3*EL*m12squared*SA22*dBeta1KanUsual())/(M&
  &W*SB*SW*TB2) + (0.09375D0*EL*m12squared*SA1*SA2*SA3*dBeta1KanUsual())/(MW*SB*SW*TB2) + (0.28125D0*CA22*EL*m12squared*SA1*SA2*S&
  &A3*dBeta1KanUsual())/(MW*SB*SW*TB2) - (0.03125D0*CA3*EL*m12squared*SA1*TB2*dBeta1KanUsual())/(CB*MW*SW) - (0.03125D0*CA22*CA3*&
  &EL*m12squared*SA1*TB2*dBeta1KanUsual())/(CB*MW*SW) + (0.03125D0*CA3*EL*m12squared*SA1*SA22*TB2*dBeta1KanUsual())/(CB*MW*SW) - &
  &(0.140625D0*CA1*EL*m12squared*SA2*SA3*TB2*dBeta1KanUsual())/(CB*MW*SW) - (0.421875D0*CA1*CA22*EL*m12squared*SA2*SA3*TB2*dBeta1&
  &KanUsual())/(CB*MW*SW) + (0.375D0*CA3*EL*MH12*dAlpha1KanUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.375D0*CA22*CA3*EL*MH12*d&
  &Alpha1KanUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.1875D0*CA3*EL*MH22*dAlpha1KanUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) +&
  & (0.1875D0*CA22*CA3*EL*MH22*dAlpha1KanUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.375D0*CA3*EL*MH12*SA22*dAlpha1KanUsual()*D&
  &BLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.1875D0*CA3*EL*MH22*SA22*dAlpha1KanUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.5625D0*CA&
  &3*EL*m12squared*dAlpha1KanUsual()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.5625D0*CA22*CA3*EL*m12squared*dAlpha1KanUsual()*DB&
  &LE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) + (0.5625D0*CA3*EL*m12squared*SA22*dAlpha1KanUsual()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) -&
  & (0.1875D0*EL*MH12*SA2*SA3*dAlpha1KanUsual()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.5625D0*CA22*EL*MH12*SA2*SA3*dAlpha1KanUsual&
  &()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.09375D0*EL*MH22*SA2*SA3*dAlpha1KanUsual()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.28125&
  &D0*CA22*EL*MH22*SA2*SA3*dAlpha1KanUsual()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) + (0.28125D0*EL*m12squared*SA2*SA3*dAlpha1KanUsual(&
  &)*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.84375D0*CA22*EL*m12squared*SA2*SA3*dAlpha1KanUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW*&
  &SB2*SW) + (0.0625D0*CA2*EL*MH12*SA3*dAlpha2KanUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.03125D0*CA2*EL*MH22*SA3*dAlpha2Kan&
  &Usual()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.5625D0*CA2*EL*MH12*SA22*SA3*dAlpha2KanUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) -&
  & (0.28125D0*CA2*EL*MH22*SA22*SA3*dAlpha2KanUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.5D0*CA2*CA3*EL*MH12*SA2*dAlpha2KanUsu&
  &al()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.25D0*CA2*CA3*EL*MH22*SA2*dAlpha2KanUsual()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.09&
  &375D0*CA2*EL*m12squared*SA3*dAlpha2KanUsual()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) + (0.84375D0*CA2*EL*m12squared*SA22*SA3*dAl&
  &pha2KanUsual()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) + (0.75D0*CA2*CA3*EL*m12squared*SA2*dAlpha2KanUsual()*DBLE(CA1**INT(3.D0))&
  &)/(CB*MW*SB2*SW) + (0.0625D0*CA3*EL*MH12*SA2*dAlpha3KanUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.1875D0*CA22*CA3*EL*MH12*S&
  &A2*dAlpha3KanUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.03125D0*CA3*EL*MH22*SA2*dAlpha3KanUsual()*DBLE(CA1**INT(3.D0)))/(CB&
  &*MW*SW) + (0.09375D0*CA22*CA3*EL*MH22*SA2*dAlpha3KanUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.09375D0*CA3*EL*m12squared*SA&
  &2*dAlpha3KanUsual()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.28125D0*CA22*CA3*EL*m12squared*SA2*dAlpha3KanUsual()*DBLE(CA1**I&
  &NT(3.D0)))/(CB2*MW*SB*SW) - (0.125D0*EL*MH12*SA3*dAlpha3KanUsual()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.125D0*CA22*EL*MH12*SA&
  &3*dAlpha3KanUsual()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.0625D0*EL*MH22*SA3*dAlpha3KanUsual()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW&
  &) - (0.0625D0*CA22*EL*MH22*SA3*dAlpha3KanUsual()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) + (0.125D0*EL*MH12*SA22*SA3*dAlpha3KanUsual(&
  &)*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) + (0.0625D0*EL*MH22*SA22*SA3*dAlpha3KanUsual()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) + (0.1875D0&
  &*EL*m12squared*SA3*dAlpha3KanUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.1875D0*CA22*EL*m12squared*SA3*dAlpha3KanUsual()&
  &*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.1875D0*EL*m12squared*SA22*SA3*dAlpha3KanUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW&
  &) - (0.1875D0*CA3*EL*m12squared*dBeta1KanUsual()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.1875D0*CA22*CA3*EL*m12squared*dBeta&
  &1KanUsual()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) + (0.1875D0*CA3*EL*m12squared*SA22*dBeta1KanUsual()*DBLE(CA1**INT(3.D0)))/(CB&
  &2*MW*SB*SW) - (0.03125D0*EL*MH12*SA2*SA3*dBeta1KanUsual()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.09375D0*CA22*EL*MH12*SA2*SA3*d&
  &Beta1KanUsual()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) + (0.03125D0*EL*MH12*SA2*SA3*dBeta1KanUsual()*DBLE(CA1**INT(3.D0)))/(CB2*MW*S&
  &B*SW) + (0.09375D0*CA22*EL*MH12*SA2*SA3*dBeta1KanUsual()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.015625D0*EL*MH22*SA2*SA3*dB&
  &eta1KanUsual()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.046875D0*CA22*EL*MH22*SA2*SA3*dBeta1KanUsual()*DBLE(CA1**INT(3.D0)))/(MW*&
  &SB*SW) + (0.015625D0*EL*MH22*SA2*SA3*dBeta1KanUsual()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) + (0.046875D0*CA22*EL*MH22*SA2*SA3*&
  &dBeta1KanUsual()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) + (0.12890625D0*EL*m12squared*SA2*SA3*dBeta1KanUsual()*DBLE(CA1**INT(3.D&
  &0)))/(CB*MW*SB2*SW) + (0.38671875D0*CA22*EL*m12squared*SA2*SA3*dBeta1KanUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.125D&
  &0*CA3*EL*MH12*dBeta1KanUsual()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW*TB) - (0.125D0*CA22*CA3*EL*MH12*dBeta1KanUsual()*DBLE(CA1**INT(&
  &3.D0)))/(MW*SB*SW*TB) - (0.0625D0*CA3*EL*MH22*dBeta1KanUsual()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW*TB) - (0.0625D0*CA22*CA3*EL*MH2&
  &2*dBeta1KanUsual()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW*TB) + (0.125D0*CA3*EL*MH12*SA22*dBeta1KanUsual()*DBLE(CA1**INT(3.D0)))/(MW*&
  &SB*SW*TB) + (0.0625D0*CA3*EL*MH22*SA22*dBeta1KanUsual()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW*TB) + (0.03125D0*EL*MH12*SA2*SA3*TB*dB&
  &eta1KanUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.09375D0*CA22*EL*MH12*SA2*SA3*TB*dBeta1KanUsual()*DBLE(CA1**INT(3.D0)))/(C&
  &B*MW*SW) + (0.015625D0*EL*MH22*SA2*SA3*TB*dBeta1KanUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.046875D0*CA22*EL*MH22*SA2*SA3&
  &*TB*dBeta1KanUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.5625D0*CA1*EL*MH12*SA3*dAlpha2KanUsual()*DBLE(CA2**INT(3.D0)))/(CB*&
  &MW*SW) + (0.28125D0*CA1*EL*MH22*SA3*dAlpha2KanUsual()*DBLE(CA2**INT(3.D0)))/(CB*MW*SW) + (0.84375D0*EL*m12squared*SA1*SA3*dAlp&
  &ha2KanUsual()*DBLE(CA2**INT(3.D0)))/(CB*MW*SW) - (0.5625D0*CA1*EL*MH12*SA12*SA3*dAlpha2KanUsual()*DBLE(CA2**INT(3.D0)))/(CB*MW&
  &*SW) - (0.28125D0*CA1*EL*MH22*SA12*SA3*dAlpha2KanUsual()*DBLE(CA2**INT(3.D0)))/(CB*MW*SW) + (0.84375D0*CA1*EL*m12squared*SA3*d&
  &Alpha2KanUsual()*DBLE(CA2**INT(3.D0)))/(MW*SB*SW) - (0.5625D0*CA1*EL*m12squared*SA3*dAlpha2KanUsual()*DBLE(CA2**INT(3.D0)))/(C&
  &B2*MW*SB*SW) + (0.5625D0*EL*MH12*SA1*SA3*dAlpha2KanUsual()*DBLE(CA2**INT(3.D0)))/(MW*SB*SW) - (0.5625D0*CA12*EL*MH12*SA1*SA3*d&
  &Alpha2KanUsual()*DBLE(CA2**INT(3.D0)))/(MW*SB*SW) + (0.28125D0*EL*MH22*SA1*SA3*dAlpha2KanUsual()*DBLE(CA2**INT(3.D0)))/(MW*SB*&
  &SW) - (0.28125D0*CA12*EL*MH22*SA1*SA3*dAlpha2KanUsual()*DBLE(CA2**INT(3.D0)))/(MW*SB*SW) + (0.84375D0*CA1*EL*m12squared*SA12*S&
  &A3*dAlpha2KanUsual()*DBLE(CA2**INT(3.D0)))/(CB2*MW*SB*SW) - (0.5625D0*EL*m12squared*SA1*SA3*dAlpha2KanUsual()*DBLE(CA2**INT(3.&
  &D0)))/(CB*MW*SB2*SW) + (0.84375D0*CA12*EL*m12squared*SA1*SA3*dAlpha2KanUsual()*DBLE(CA2**INT(3.D0)))/(CB*MW*SB2*SW) - (0.28125&
  &D0*EL*m12squared*SA1*SA3*dAlpha2KanUsual()*DBLE(CA2**INT(3.D0)))/(MW*SB*SW*TB) - (0.28125D0*CA1*EL*m12squared*SA3*TB*dAlpha2Ka&
  &nUsual()*DBLE(CA2**INT(3.D0)))/(CB*MW*SW) + (0.5D0*CA3*MH12*dAlpha3KanUsual()*DBLE(CA2**INT(3.D0)))/vS + (0.25D0*CA3*MH22*dAlp&
  &ha3KanUsual()*DBLE(CA2**INT(3.D0)))/vS + (0.1875D0*EL*MH12*SA3*dAlpha2KanUsual()*DBLE(CA1**INT(3.D0))*DBLE(CA2**INT(3.D0)))/(C&
  &B*MW*SW) + (0.09375D0*EL*MH22*SA3*dAlpha2KanUsual()*DBLE(CA1**INT(3.D0))*DBLE(CA2**INT(3.D0)))/(CB*MW*SW) - (0.28125D0*EL*m12s&
  &quared*SA3*dAlpha2KanUsual()*DBLE(CA1**INT(3.D0))*DBLE(CA2**INT(3.D0)))/(CB2*MW*SB*SW) - (0.28125D0*CA3*EL*m12squared*SA1*dBet&
  &a1KanUsual()*DBLE(CB**INT(-3.D0)))/(MW*SW) - (0.84375D0*CA12*CA3*EL*m12squared*SA1*dBeta1KanUsual()*DBLE(CB**INT(-3.D0)))/(MW*&
  &SW) - (0.28125D0*CA22*CA3*EL*m12squared*SA1*dBeta1KanUsual()*DBLE(CB**INT(-3.D0)))/(MW*SW) - (0.84375D0*CA12*CA22*CA3*EL*m12sq&
  &uared*SA1*dBeta1KanUsual()*DBLE(CB**INT(-3.D0)))/(MW*SW) + (0.28125D0*CA3*EL*m12squared*SA1*SA22*dBeta1KanUsual()*DBLE(CB**INT&
  &(-3.D0)))/(MW*SW) + (0.84375D0*CA12*CA3*EL*m12squared*SA1*SA22*dBeta1KanUsual()*DBLE(CB**INT(-3.D0)))/(MW*SW) - (0.36328125D0*&
  &CA1*EL*m12squared*SA2*SA3*dBeta1KanUsual()*DBLE(CB**INT(-3.D0)))/(MW*SW) - (1.08984375D0*CA1*CA22*EL*m12squared*SA2*SA3*dBeta1&
  &KanUsual()*DBLE(CB**INT(-3.D0)))/(MW*SW) + (0.45703125D0*CA1*EL*m12squared*SA12*SA2*SA3*dBeta1KanUsual()*DBLE(CB**INT(-3.D0)))&
  &/(MW*SW) + (1.37109375D0*CA1*CA22*EL*m12squared*SA12*SA2*SA3*dBeta1KanUsual()*DBLE(CB**INT(-3.D0)))/(MW*SW) - (0.0625D0*CA3*EL&
  &*m12squared*SA1*dBeta1KanUsual()*DBLE(CB**INT(-3.D0)))/(MW*SB2*SW) - (0.28125D0*CA12*CA3*EL*m12squared*SA1*dBeta1KanUsual()*DB&
  &LE(CB**INT(-3.D0)))/(MW*SB2*SW) - (0.0625D0*CA22*CA3*EL*m12squared*SA1*dBeta1KanUsual()*DBLE(CB**INT(-3.D0)))/(MW*SB2*SW) - (0&
  &.28125D0*CA12*CA22*CA3*EL*m12squared*SA1*dBeta1KanUsual()*DBLE(CB**INT(-3.D0)))/(MW*SB2*SW) + (0.0625D0*CA3*EL*m12squared*SA1*&
  &SA22*dBeta1KanUsual()*DBLE(CB**INT(-3.D0)))/(MW*SB2*SW) + (0.28125D0*CA12*CA3*EL*m12squared*SA1*SA22*dBeta1KanUsual()*DBLE(CB*&
  &*INT(-3.D0)))/(MW*SB2*SW) - (0.05859375D0*CA1*EL*m12squared*SA2*SA3*dBeta1KanUsual()*DBLE(CB**INT(-3.D0)))/(MW*SB2*SW) - (0.17&
  &578125D0*CA1*CA22*EL*m12squared*SA2*SA3*dBeta1KanUsual()*DBLE(CB**INT(-3.D0)))/(MW*SB2*SW) + (0.10546875D0*CA1*EL*m12squared*S&
  &A12*SA2*SA3*dBeta1KanUsual()*DBLE(CB**INT(-3.D0)))/(MW*SB2*SW) + (0.31640625D0*CA1*CA22*EL*m12squared*SA12*SA2*SA3*dBeta1KanUs&
  &ual()*DBLE(CB**INT(-3.D0)))/(MW*SB2*SW) - (0.15234375D0*EL*m12squared*SA2*SA3*dBeta1KanUsual()*DBLE(CA1**INT(3.D0))*DBLE(CB**I&
  &NT(-3.D0)))/(MW*SW) - (0.45703125D0*CA22*EL*m12squared*SA2*SA3*dBeta1KanUsual()*DBLE(CA1**INT(3.D0))*DBLE(CB**INT(-3.D0)))/(MW&
  &*SW) - (0.03515625D0*EL*m12squared*SA2*SA3*dBeta1KanUsual()*DBLE(CA1**INT(3.D0))*DBLE(CB**INT(-3.D0)))/(MW*SB2*SW) - (0.105468&
  &75D0*CA22*EL*m12squared*SA2*SA3*dBeta1KanUsual()*DBLE(CA1**INT(3.D0))*DBLE(CB**INT(-3.D0)))/(MW*SB2*SW) + (0.1875D0*EL*MH12*SA&
  &2*SA3*dAlpha1KanUsual()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.5625D0*CA22*EL*MH12*SA2*SA3*dAlpha1KanUsual()*DBLE(SA1**INT(3.D0&
  &)))/(CB*MW*SW) + (0.09375D0*EL*MH22*SA2*SA3*dAlpha1KanUsual()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.28125D0*CA22*EL*MH22*SA2*S&
  &A3*dAlpha1KanUsual()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.375D0*CA3*EL*MH12*dAlpha1KanUsual()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW&
  &) + (0.375D0*CA22*CA3*EL*MH12*dAlpha1KanUsual()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.1875D0*CA3*EL*MH22*dAlpha1KanUsual()*DBL&
  &E(SA1**INT(3.D0)))/(MW*SB*SW) + (0.1875D0*CA22*CA3*EL*MH22*dAlpha1KanUsual()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0.375D0*CA3*E&
  &L*MH12*SA22*dAlpha1KanUsual()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0.1875D0*CA3*EL*MH22*SA22*dAlpha1KanUsual()*DBLE(SA1**INT(3.&
  &D0)))/(MW*SB*SW) - (0.28125D0*EL*m12squared*SA2*SA3*dAlpha1KanUsual()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.84375D0*CA22*E&
  &L*m12squared*SA2*SA3*dAlpha1KanUsual()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.5625D0*CA3*EL*m12squared*dAlpha1KanUsual()*DB&
  &LE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.5625D0*CA22*CA3*EL*m12squared*dAlpha1KanUsual()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) +&
  & (0.5625D0*CA3*EL*m12squared*SA22*dAlpha1KanUsual()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.5D0*CA2*CA3*EL*MH12*SA2*dAlpha2K&
  &anUsual()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.25D0*CA2*CA3*EL*MH22*SA2*dAlpha2KanUsual()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) - &
  &(0.75D0*CA2*CA3*EL*m12squared*SA2*dAlpha2KanUsual()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) + (0.0625D0*CA2*EL*MH12*SA3*dAlpha2Ka&
  &nUsual()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.03125D0*CA2*EL*MH22*SA3*dAlpha2KanUsual()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0&
  &.5625D0*CA2*EL*MH12*SA22*SA3*dAlpha2KanUsual()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0.28125D0*CA2*EL*MH22*SA22*SA3*dAlpha2KanUs&
  &ual()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0.09375D0*CA2*EL*m12squared*SA3*dAlpha2KanUsual()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*S&
  &W) + (0.84375D0*CA2*EL*m12squared*SA22*SA3*dAlpha2KanUsual()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.125D0*EL*MH12*SA3*dAlph&
  &a3KanUsual()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.125D0*CA22*EL*MH12*SA3*dAlpha3KanUsual()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) +&
  & (0.0625D0*EL*MH22*SA3*dAlpha3KanUsual()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.0625D0*CA22*EL*MH22*SA3*dAlpha3KanUsual()*DBLE(&
  &SA1**INT(3.D0)))/(CB*MW*SW) - (0.125D0*EL*MH12*SA22*SA3*dAlpha3KanUsual()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) - (0.0625D0*EL*MH22&
  &*SA22*SA3*dAlpha3KanUsual()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.0625D0*CA3*EL*MH12*SA2*dAlpha3KanUsual()*DBLE(SA1**INT(3.D0)&
  &))/(MW*SB*SW) + (0.1875D0*CA22*CA3*EL*MH12*SA2*dAlpha3KanUsual()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.03125D0*CA3*EL*MH22*SA2&
  &*dAlpha3KanUsual()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.09375D0*CA22*CA3*EL*MH22*SA2*dAlpha3KanUsual()*DBLE(SA1**INT(3.D0)))/&
  &(MW*SB*SW) - (0.1875D0*EL*m12squared*SA3*dAlpha3KanUsual()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.1875D0*CA22*EL*m12squared&
  &*SA3*dAlpha3KanUsual()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) + (0.1875D0*EL*m12squared*SA22*SA3*dAlpha3KanUsual()*DBLE(SA1**INT&
  &(3.D0)))/(CB2*MW*SB*SW) - (0.09375D0*CA3*EL*m12squared*SA2*dAlpha3KanUsual()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.28125D0&
  &*CA22*CA3*EL*m12squared*SA2*dAlpha3KanUsual()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.0625D0*CA3*EL*MH12*dBeta1KanUsual()*DB&
  &LE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.0625D0*CA22*CA3*EL*MH12*dBeta1KanUsual()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0.0625D0*CA3*&
  &EL*MH12*dBeta1KanUsual()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.0625D0*CA22*CA3*EL*MH12*dBeta1KanUsual()*DBLE(SA1**INT(3.D0&
  &)))/(CB2*MW*SB*SW) + (0.03125D0*CA3*EL*MH22*dBeta1KanUsual()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.03125D0*CA22*CA3*EL*MH22*dB&
  &eta1KanUsual()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0.03125D0*CA3*EL*MH22*dBeta1KanUsual()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW)&
  & - (0.03125D0*CA22*CA3*EL*MH22*dBeta1KanUsual()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.0625D0*CA3*EL*MH12*SA22*dBeta1KanUsu&
  &al()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.0625D0*CA3*EL*MH12*SA22*dBeta1KanUsual()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.&
  &03125D0*CA3*EL*MH22*SA22*dBeta1KanUsual()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.03125D0*CA3*EL*MH22*SA22*dBeta1KanUsual()*DBLE&
  &(SA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.09375D0*EL*m12squared*SA2*SA3*dBeta1KanUsual()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0&
  &.28125D0*CA22*EL*m12squared*SA2*SA3*dBeta1KanUsual()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.28125D0*CA3*EL*m12squared*dBeta&
  &1KanUsual()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.28125D0*CA22*CA3*EL*m12squared*dBeta1KanUsual()*DBLE(SA1**INT(3.D0)))/(C&
  &B*MW*SB2*SW) + (0.28125D0*CA3*EL*m12squared*SA22*dBeta1KanUsual()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.0625D0*EL*MH12*SA2&
  &*SA3*dBeta1KanUsual()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW*TB) - (0.1875D0*CA22*EL*MH12*SA2*SA3*dBeta1KanUsual()*DBLE(SA1**INT(3.D0&
  &)))/(MW*SB*SW*TB) - (0.03125D0*EL*MH22*SA2*SA3*dBeta1KanUsual()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW*TB) - (0.09375D0*CA22*EL*MH22*&
  &SA2*SA3*dBeta1KanUsual()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW*TB) - (0.0625D0*CA3*EL*MH12*TB*dBeta1KanUsual()*DBLE(SA1**INT(3.D0)))&
  &/(CB*MW*SW) - (0.0625D0*CA22*CA3*EL*MH12*TB*dBeta1KanUsual()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) - (0.03125D0*CA3*EL*MH22*TB*dBet&
  &a1KanUsual()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) - (0.03125D0*CA22*CA3*EL*MH22*TB*dBeta1KanUsual()*DBLE(SA1**INT(3.D0)))/(CB*MW*S&
  &W) + (0.0625D0*CA3*EL*MH12*SA22*TB*dBeta1KanUsual()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.03125D0*CA3*EL*MH22*SA22*TB*dBeta1Ka&
  &nUsual()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.1875D0*EL*MH12*SA3*dAlpha2KanUsual()*DBLE(CA2**INT(3.D0))*DBLE(SA1**INT(3.D0)))&
  &/(MW*SB*SW) + (0.09375D0*EL*MH22*SA3*dAlpha2KanUsual()*DBLE(CA2**INT(3.D0))*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0.28125D0*EL*m&
  &12squared*SA3*dAlpha2KanUsual()*DBLE(CA2**INT(3.D0))*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.28125D0*CA3*EL*m12squared*dBeta&
  &1KanUsual()*DBLE(CB**INT(-3.D0))*DBLE(SA1**INT(3.D0)))/(MW*SW) + (0.28125D0*CA22*CA3*EL*m12squared*dBeta1KanUsual()*DBLE(CB**I&
  &NT(-3.D0))*DBLE(SA1**INT(3.D0)))/(MW*SW) - (0.28125D0*CA3*EL*m12squared*SA22*dBeta1KanUsual()*DBLE(CB**INT(-3.D0))*DBLE(SA1**I&
  &NT(3.D0)))/(MW*SW) + (0.09375D0*CA3*EL*m12squared*dBeta1KanUsual()*DBLE(CB**INT(-3.D0))*DBLE(SA1**INT(3.D0)))/(MW*SB2*SW) + (0&
  &.09375D0*CA22*CA3*EL*m12squared*dBeta1KanUsual()*DBLE(CB**INT(-3.D0))*DBLE(SA1**INT(3.D0)))/(MW*SB2*SW) - (0.09375D0*CA3*EL*m1&
  &2squared*SA22*dBeta1KanUsual()*DBLE(CB**INT(-3.D0))*DBLE(SA1**INT(3.D0)))/(MW*SB2*SW) - (0.28125D0*CA1*EL*m12squared*SA3*dAlph&
  &a1KanUsual()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.1875D0*EL*MH12*SA1*SA3*dAlpha1KanUsual()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) +&
  & (0.5625D0*CA12*EL*MH12*SA1*SA3*dAlpha1KanUsual()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.09375D0*EL*MH22*SA1*SA3*dAlpha1KanUsua&
  &l()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.28125D0*CA12*EL*MH22*SA1*SA3*dAlpha1KanUsual()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0&
  &.1875D0*CA1*EL*MH12*SA3*dAlpha1KanUsual()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.09375D0*CA1*EL*MH22*SA3*dAlpha1KanUsual()*DBLE&
  &(SA2**INT(3.D0)))/(MW*SB*SW) + (0.28125D0*EL*m12squared*SA1*SA3*dAlpha1KanUsual()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.1875D0&
  &*EL*m12squared*SA1*SA3*dAlpha1KanUsual()*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) - (0.84375D0*CA12*EL*m12squared*SA1*SA3*dAlpha1K&
  &anUsual()*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) - (0.5625D0*CA1*EL*MH12*SA12*SA3*dAlpha1KanUsual()*DBLE(SA2**INT(3.D0)))/(MW*SB&
  &*SW) - (0.28125D0*CA1*EL*MH22*SA12*SA3*dAlpha1KanUsual()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) + (0.1875D0*CA1*EL*m12squared*SA3*dA&
  &lpha1KanUsual()*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) + (0.84375D0*CA1*EL*m12squared*SA12*SA3*dAlpha1KanUsual()*DBLE(SA2**INT(3&
  &.D0)))/(CB*MW*SB2*SW) + (0.09375D0*CA1*EL*m12squared*SA3*dAlpha1KanUsual()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) - (0.09375D0*EL&
  &*m12squared*SA1*SA3*TB*dAlpha1KanUsual()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (1.5D0*MH12*SA3*dAlpha2KanUsual()*DBLE(SA2**INT(3.&
  &D0)))/vS + (0.75D0*MH22*SA3*dAlpha2KanUsual()*DBLE(SA2**INT(3.D0)))/vS - (0.1875D0*CA1*CA3*EL*MH12*dAlpha3KanUsual()*DBLE(SA2*&
  &*INT(3.D0)))/(CB*MW*SW) - (0.09375D0*CA1*CA3*EL*MH22*dAlpha3KanUsual()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.28125D0*CA3*EL*m1&
  &2squared*SA1*dAlpha3KanUsual()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.1875D0*CA1*CA3*EL*MH12*SA12*dAlpha3KanUsual()*DBLE(SA2**I&
  &NT(3.D0)))/(CB*MW*SW) + (0.09375D0*CA1*CA3*EL*MH22*SA12*dAlpha3KanUsual()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.28125D0*CA1*CA&
  &3*EL*m12squared*dAlpha3KanUsual()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) + (0.1875D0*CA1*CA3*EL*m12squared*dAlpha3KanUsual()*DBLE(SA&
  &2**INT(3.D0)))/(CB2*MW*SB*SW) - (0.1875D0*CA3*EL*MH12*SA1*dAlpha3KanUsual()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) + (0.1875D0*CA12*&
  &CA3*EL*MH12*SA1*dAlpha3KanUsual()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.09375D0*CA3*EL*MH22*SA1*dAlpha3KanUsual()*DBLE(SA2**IN&
  &T(3.D0)))/(MW*SB*SW) + (0.09375D0*CA12*CA3*EL*MH22*SA1*dAlpha3KanUsual()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.28125D0*CA1*CA3&
  &*EL*m12squared*SA12*dAlpha3KanUsual()*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) + (0.1875D0*CA3*EL*m12squared*SA1*dAlpha3KanUsual()&
  &*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) - (0.28125D0*CA12*CA3*EL*m12squared*SA1*dAlpha3KanUsual()*DBLE(SA2**INT(3.D0)))/(CB*MW*S&
  &B2*SW) + (0.09375D0*CA3*EL*m12squared*SA1*dAlpha3KanUsual()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) + (0.09375D0*CA1*CA3*EL*m12squ&
  &ared*TB*dAlpha3KanUsual()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.09375D0*CA1*EL*m12squared*SA3*dBeta1KanUsual()*DBLE(SA2**INT(3&
  &.D0)))/(CB*MW*SW) + (0.09375D0*CA1*EL*MH12*SA3*dBeta1KanUsual()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.09375D0*CA1*EL*MH12*SA3*&
  &dBeta1KanUsual()*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) + (0.046875D0*CA1*EL*MH22*SA3*dBeta1KanUsual()*DBLE(SA2**INT(3.D0)))/(MW&
  &*SB*SW) - (0.046875D0*CA1*EL*MH22*SA3*dBeta1KanUsual()*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) + (0.140625D0*EL*m12squared*SA1*SA&
  &3*dBeta1KanUsual()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) + (0.046875D0*EL*m12squared*SA1*SA3*dBeta1KanUsual()*DBLE(SA2**INT(3.D0)))&
  &/(CB2*MW*SB*SW) - (0.28125D0*CA12*EL*m12squared*SA1*SA3*dBeta1KanUsual()*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) - (0.09375D0*CA1&
  &*EL*MH12*SA12*SA3*dBeta1KanUsual()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) + (0.09375D0*CA1*EL*MH12*SA12*SA3*dBeta1KanUsual()*DBLE(SA&
  &2**INT(3.D0)))/(CB2*MW*SB*SW) - (0.046875D0*CA1*EL*MH22*SA12*SA3*dBeta1KanUsual()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) + (0.046875&
  &D0*CA1*EL*MH22*SA12*SA3*dBeta1KanUsual()*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) - (0.10546875D0*CA1*EL*m12squared*SA3*dBeta1KanU&
  &sual()*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) + (0.38671875D0*CA1*EL*m12squared*SA12*SA3*dBeta1KanUsual()*DBLE(SA2**INT(3.D0)))/&
  &(CB*MW*SB2*SW) + (0.140625D0*CA1*EL*m12squared*SA3*dBeta1KanUsual()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) + (0.1875D0*EL*MH12*SA&
  &1*SA3*dBeta1KanUsual()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) - (0.1875D0*CA12*EL*MH12*SA1*SA3*dBeta1KanUsual()*DBLE(SA2**INT(3.D&
  &0)))/(MW*SB*SW*TB) + (0.09375D0*EL*MH22*SA1*SA3*dBeta1KanUsual()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) - (0.09375D0*CA12*EL*MH22&
  &*SA1*SA3*dBeta1KanUsual()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) - (0.09375D0*CA1*EL*MH12*SA3*TB*dBeta1KanUsual()*DBLE(SA2**INT(3&
  &.D0)))/(CB*MW*SW) - (0.046875D0*CA1*EL*MH22*SA3*TB*dBeta1KanUsual()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.140625D0*EL*m12squar&
  &ed*SA1*SA3*TB*dBeta1KanUsual()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.09375D0*CA1*EL*MH12*SA12*SA3*TB*dBeta1KanUsual()*DBLE(SA2&
  &**INT(3.D0)))/(CB*MW*SW) + (0.046875D0*CA1*EL*MH22*SA12*SA3*TB*dBeta1KanUsual()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.09375D0*&
  &EL*m12squared*SA1*SA3*dBeta1KanUsual()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB2) + (0.140625D0*CA1*EL*m12squared*SA3*TB2*dBeta1KanU&
  &sual()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.1875D0*EL*MH12*SA3*dAlpha1KanUsual()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(&
  &MW*SB*SW) + (0.09375D0*EL*MH22*SA3*dAlpha1KanUsual()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.28125D0*EL*m12&
  &squared*SA3*dAlpha1KanUsual()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) - (0.0625D0*CA3*EL*MH12*dAlpha3KanUsua&
  &l()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.03125D0*CA3*EL*MH22*dAlpha3KanUsual()*DBLE(CA1**INT(3.D0))*DBLE&
  &(SA2**INT(3.D0)))/(CB*MW*SW) + (0.09375D0*CA3*EL*m12squared*dAlpha3KanUsual()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB2*&
  &MW*SB*SW) + (0.03125D0*EL*MH12*SA3*dBeta1KanUsual()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.03125D0*EL*MH12&
  &*SA3*dBeta1KanUsual()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) + (0.015625D0*EL*MH22*SA3*dBeta1KanUsual()*DBL&
  &E(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.015625D0*EL*MH22*SA3*dBeta1KanUsual()*DBLE(CA1**INT(3.D0))*DBLE(SA2**I&
  &NT(3.D0)))/(CB2*MW*SB*SW) - (0.12890625D0*EL*m12squared*SA3*dBeta1KanUsual()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW&
  &*SB2*SW) - (0.03125D0*EL*MH12*SA3*TB*dBeta1KanUsual()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.015625D0*EL*M&
  &H22*SA3*TB*dBeta1KanUsual()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.36328125D0*CA1*EL*m12squared*SA3*dBeta1&
  &KanUsual()*DBLE(CB**INT(-3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SW) - (0.45703125D0*CA1*EL*m12squared*SA12*SA3*dBeta1KanUsual()*DBLE&
  &(CB**INT(-3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SW) + (0.05859375D0*CA1*EL*m12squared*SA3*dBeta1KanUsual()*DBLE(CB**INT(-3.D0))*DBL&
  &E(SA2**INT(3.D0)))/(MW*SB2*SW) - (0.10546875D0*CA1*EL*m12squared*SA12*SA3*dBeta1KanUsual()*DBLE(CB**INT(-3.D0))*DBLE(SA2**INT(&
  &3.D0)))/(MW*SB2*SW) + (0.15234375D0*EL*m12squared*SA3*dBeta1KanUsual()*DBLE(CA1**INT(3.D0))*DBLE(CB**INT(-3.D0))*DBLE(SA2**INT&
  &(3.D0)))/(MW*SW) + (0.03515625D0*EL*m12squared*SA3*dBeta1KanUsual()*DBLE(CA1**INT(3.D0))*DBLE(CB**INT(-3.D0))*DBLE(SA2**INT(3.&
  &D0)))/(MW*SB2*SW) - (0.1875D0*EL*MH12*SA3*dAlpha1KanUsual()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.09375D0&
  &*EL*MH22*SA3*dAlpha1KanUsual()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.28125D0*EL*m12squared*SA3*dAlpha1Kan&
  &Usual()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) - (0.0625D0*CA3*EL*MH12*dAlpha3KanUsual()*DBLE(SA1**INT(3.D0&
  &))*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.03125D0*CA3*EL*MH22*dAlpha3KanUsual()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(MW*&
  &SB*SW) + (0.09375D0*CA3*EL*m12squared*dAlpha3KanUsual()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) + (0.09375D0&
  &*EL*m12squared*SA3*dBeta1KanUsual()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) + (0.0625D0*EL*MH12*SA3*dBeta1Ka&
  &nUsual()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) + (0.03125D0*EL*MH22*SA3*dBeta1KanUsual()*DBLE(SA1**INT(3.D0&
  &))*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) - (0.1875D0*CA1*CA3*EL*m12squared*dBeta1KanUsual()*DBLE(SB**INT(-3.D0)))/(MW*SW) - (0.1&
  &875D0*CA1*CA22*CA3*EL*m12squared*dBeta1KanUsual()*DBLE(SB**INT(-3.D0)))/(MW*SW) - (1.125D0*CA1*CA3*EL*m12squared*SA12*dBeta1Ka&
  &nUsual()*DBLE(SB**INT(-3.D0)))/(MW*SW) - (1.125D0*CA1*CA22*CA3*EL*m12squared*SA12*dBeta1KanUsual()*DBLE(SB**INT(-3.D0)))/(MW*S&
  &W) + (0.1875D0*CA1*CA3*EL*m12squared*SA22*dBeta1KanUsual()*DBLE(SB**INT(-3.D0)))/(MW*SW) + (1.125D0*CA1*CA3*EL*m12squared*SA12&
  &*SA22*dBeta1KanUsual()*DBLE(SB**INT(-3.D0)))/(MW*SW) + (0.46875D0*EL*m12squared*SA1*SA2*SA3*dBeta1KanUsual()*DBLE(SB**INT(-3.D&
  &0)))/(MW*SW) - (0.5625D0*CA12*EL*m12squared*SA1*SA2*SA3*dBeta1KanUsual()*DBLE(SB**INT(-3.D0)))/(MW*SW) + (1.40625D0*CA22*EL*m1&
  &2squared*SA1*SA2*SA3*dBeta1KanUsual()*DBLE(SB**INT(-3.D0)))/(MW*SW) - (1.6875D0*CA12*CA22*EL*m12squared*SA1*SA2*SA3*dBeta1KanU&
  &sual()*DBLE(SB**INT(-3.D0)))/(MW*SW) + (0.375D0*CA3*EL*m12squared*dBeta1KanUsual()*DBLE(CA1**INT(3.D0))*DBLE(SB**INT(-3.D0)))/&
  &(MW*SW) + (0.375D0*CA22*CA3*EL*m12squared*dBeta1KanUsual()*DBLE(CA1**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(MW*SW) - (0.375D0*CA3*E&
  &L*m12squared*SA22*dBeta1KanUsual()*DBLE(CA1**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(MW*SW) + (0.1875D0*EL*m12squared*SA2*SA3*dBeta1&
  &KanUsual()*DBLE(SA1**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(MW*SW) + (0.5625D0*CA22*EL*m12squared*SA2*SA3*dBeta1KanUsual()*DBLE(SA1&
  &**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(MW*SW) - (0.46875D0*EL*m12squared*SA1*SA3*dBeta1KanUsual()*DBLE(SA2**INT(3.D0))*DBLE(SB**I&
  &NT(-3.D0)))/(MW*SW) + (0.5625D0*CA12*EL*m12squared*SA1*SA3*dBeta1KanUsual()*DBLE(SA2**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(MW*SW)&
  & - (0.1875D0*EL*m12squared*SA3*dBeta1KanUsual()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(MW*SW) - (0.1&
  &25D0*CA1*CA3*m12squared*dgAtMZ())/(CB*MW) - (0.125D0*CA1*CA22*CA3*m12squared*dgAtMZ())/(CB*MW) + (0.125D0*CA3*MH12*SA1*dgAtMZ(&
  &))/(CB*MW) + (0.375D0*CA12*CA3*MH12*SA1*dgAtMZ())/(CB*MW) + (0.125D0*CA22*CA3*MH12*SA1*dgAtMZ())/(CB*MW) + (0.375D0*CA12*CA22*&
  &CA3*MH12*SA1*dgAtMZ())/(CB*MW) + (0.0625D0*CA3*MH22*SA1*dgAtMZ())/(CB*MW) + (0.1875D0*CA12*CA3*MH22*SA1*dgAtMZ())/(CB*MW) + (0&
  &.0625D0*CA22*CA3*MH22*SA1*dgAtMZ())/(CB*MW) + (0.1875D0*CA12*CA22*CA3*MH22*SA1*dgAtMZ())/(CB*MW) + (0.125D0*CA1*CA3*m12squared&
  &*SA22*dgAtMZ())/(CB*MW) - (0.125D0*CA3*MH12*SA1*SA22*dgAtMZ())/(CB*MW) - (0.375D0*CA12*CA3*MH12*SA1*SA22*dgAtMZ())/(CB*MW) - (&
  &0.0625D0*CA3*MH22*SA1*SA22*dgAtMZ())/(CB*MW) - (0.1875D0*CA12*CA3*MH22*SA1*SA22*dgAtMZ())/(CB*MW) + (0.1875D0*CA1*MH12*SA2*SA3&
  &*dgAtMZ())/(CB*MW) + (0.5625D0*CA1*CA22*MH12*SA2*SA3*dgAtMZ())/(CB*MW) + (0.09375D0*CA1*MH22*SA2*SA3*dgAtMZ())/(CB*MW) + (0.28&
  &125D0*CA1*CA22*MH22*SA2*SA3*dgAtMZ())/(CB*MW) + (0.28125D0*m12squared*SA1*SA2*SA3*dgAtMZ())/(CB*MW) + (0.84375D0*CA22*m12squar&
  &ed*SA1*SA2*SA3*dgAtMZ())/(CB*MW) - (0.1875D0*CA1*MH12*SA12*SA2*SA3*dgAtMZ())/(CB*MW) - (0.5625D0*CA1*CA22*MH12*SA12*SA2*SA3*dg&
  &AtMZ())/(CB*MW) - (0.09375D0*CA1*MH22*SA12*SA2*SA3*dgAtMZ())/(CB*MW) - (0.28125D0*CA1*CA22*MH22*SA12*SA2*SA3*dgAtMZ())/(CB*MW)&
  & - (0.125D0*CA1*CA3*MH12*dgAtMZ())/(MW*SB) - (0.125D0*CA1*CA22*CA3*MH12*dgAtMZ())/(MW*SB) - (0.0625D0*CA1*CA3*MH22*dgAtMZ())/(&
  &MW*SB) - (0.0625D0*CA1*CA22*CA3*MH22*dgAtMZ())/(MW*SB) + (0.21875D0*CA3*m12squared*SA1*dgAtMZ())/(MW*SB) + (0.21875D0*CA22*CA3&
  &*m12squared*SA1*dgAtMZ())/(MW*SB) - (0.15625D0*CA3*m12squared*SA1*dgAtMZ())/(CB2*MW*SB) - (0.5625D0*CA12*CA3*m12squared*SA1*dg&
  &AtMZ())/(CB2*MW*SB) - (0.15625D0*CA22*CA3*m12squared*SA1*dgAtMZ())/(CB2*MW*SB) - (0.5625D0*CA12*CA22*CA3*m12squared*SA1*dgAtMZ&
  &())/(CB2*MW*SB) - (0.375D0*CA1*CA3*MH12*SA12*dgAtMZ())/(MW*SB) - (0.375D0*CA1*CA22*CA3*MH12*SA12*dgAtMZ())/(MW*SB) - (0.1875D0&
  &*CA1*CA3*MH22*SA12*dgAtMZ())/(MW*SB) - (0.1875D0*CA1*CA22*CA3*MH22*SA12*dgAtMZ())/(MW*SB) + (0.125D0*CA1*CA3*MH12*SA22*dgAtMZ(&
  &))/(MW*SB) + (0.0625D0*CA1*CA3*MH22*SA22*dgAtMZ())/(MW*SB) - (0.21875D0*CA3*m12squared*SA1*SA22*dgAtMZ())/(MW*SB) + (0.15625D0&
  &*CA3*m12squared*SA1*SA22*dgAtMZ())/(CB2*MW*SB) + (0.5625D0*CA12*CA3*m12squared*SA1*SA22*dgAtMZ())/(CB2*MW*SB) + (0.375D0*CA1*C&
  &A3*MH12*SA12*SA22*dgAtMZ())/(MW*SB) + (0.1875D0*CA1*CA3*MH22*SA12*SA22*dgAtMZ())/(MW*SB) + (0.28125D0*CA1*m12squared*SA2*SA3*d&
  &gAtMZ())/(MW*SB) + (0.84375D0*CA1*CA22*m12squared*SA2*SA3*dgAtMZ())/(MW*SB) - (0.1875D0*CA1*m12squared*SA2*SA3*dgAtMZ())/(CB2*&
  &MW*SB) - (0.5625D0*CA1*CA22*m12squared*SA2*SA3*dgAtMZ())/(CB2*MW*SB) + (0.1875D0*MH12*SA1*SA2*SA3*dgAtMZ())/(MW*SB) - (0.1875D&
  &0*CA12*MH12*SA1*SA2*SA3*dgAtMZ())/(MW*SB) + (0.5625D0*CA22*MH12*SA1*SA2*SA3*dgAtMZ())/(MW*SB) - (0.5625D0*CA12*CA22*MH12*SA1*S&
  &A2*SA3*dgAtMZ())/(MW*SB) + (0.09375D0*MH22*SA1*SA2*SA3*dgAtMZ())/(MW*SB) - (0.09375D0*CA12*MH22*SA1*SA2*SA3*dgAtMZ())/(MW*SB) &
  &+ (0.28125D0*CA22*MH22*SA1*SA2*SA3*dgAtMZ())/(MW*SB) - (0.28125D0*CA12*CA22*MH22*SA1*SA2*SA3*dgAtMZ())/(MW*SB) + (0.28125D0*CA&
  &1*m12squared*SA12*SA2*SA3*dgAtMZ())/(CB2*MW*SB) + (0.84375D0*CA1*CA22*m12squared*SA12*SA2*SA3*dgAtMZ())/(CB2*MW*SB) + (0.0625D&
  &0*CA1*CA3*m12squared*dgAtMZ())/(CB*MW*SB2) + (0.0625D0*CA1*CA22*CA3*m12squared*dgAtMZ())/(CB*MW*SB2) + (0.5625D0*CA1*CA3*m12sq&
  &uared*SA12*dgAtMZ())/(CB*MW*SB2) + (0.5625D0*CA1*CA22*CA3*m12squared*SA12*dgAtMZ())/(CB*MW*SB2) - (0.0625D0*CA1*CA3*m12squared&
  &*SA22*dgAtMZ())/(CB*MW*SB2) - (0.5625D0*CA1*CA3*m12squared*SA12*SA22*dgAtMZ())/(CB*MW*SB2) - (0.1875D0*m12squared*SA1*SA2*SA3*&
  &dgAtMZ())/(CB*MW*SB2) + (0.28125D0*CA12*m12squared*SA1*SA2*SA3*dgAtMZ())/(CB*MW*SB2) - (0.5625D0*CA22*m12squared*SA1*SA2*SA3*d&
  &gAtMZ())/(CB*MW*SB2) + (0.84375D0*CA12*CA22*m12squared*SA1*SA2*SA3*dgAtMZ())/(CB*MW*SB2) + (0.125D0*CA1*CA3*m12squared*dgAtMZ(&
  &))/(MW*SB*TB) + (0.125D0*CA1*CA22*CA3*m12squared*dgAtMZ())/(MW*SB*TB) - (0.125D0*CA1*CA3*m12squared*SA22*dgAtMZ())/(MW*SB*TB) &
  &- (0.09375D0*m12squared*SA1*SA2*SA3*dgAtMZ())/(MW*SB*TB) - (0.28125D0*CA22*m12squared*SA1*SA2*SA3*dgAtMZ())/(MW*SB*TB) - (0.03&
  &125D0*CA3*m12squared*SA1*TB*dgAtMZ())/(CB*MW) - (0.03125D0*CA22*CA3*m12squared*SA1*TB*dgAtMZ())/(CB*MW) + (0.03125D0*CA3*m12sq&
  &uared*SA1*SA22*TB*dgAtMZ())/(CB*MW) - (0.09375D0*CA1*m12squared*SA2*SA3*TB*dgAtMZ())/(CB*MW) - (0.28125D0*CA1*CA22*m12squared*&
  &SA2*SA3*TB*dgAtMZ())/(CB*MW) + (0.0625D0*MH12*SA2*SA3*DBLE(CA1**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.1875D0*CA22*MH12*SA2*SA3*DBL&
  &E(CA1**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.03125D0*MH22*SA2*SA3*DBLE(CA1**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.09375D0*CA22*MH22*SA&
  &2*SA3*DBLE(CA1**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.125D0*CA3*MH12*DBLE(CA1**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.125D0*CA22*CA3*MH&
  &12*DBLE(CA1**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.0625D0*CA3*MH22*DBLE(CA1**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.0625D0*CA22*CA3*MH2&
  &2*DBLE(CA1**INT(3.D0))*dgAtMZ())/(MW*SB) - (0.125D0*CA3*MH12*SA22*DBLE(CA1**INT(3.D0))*dgAtMZ())/(MW*SB) - (0.0625D0*CA3*MH22*&
  &SA22*DBLE(CA1**INT(3.D0))*dgAtMZ())/(MW*SB) - (0.09375D0*m12squared*SA2*SA3*DBLE(CA1**INT(3.D0))*dgAtMZ())/(CB2*MW*SB) - (0.28&
  &125D0*CA22*m12squared*SA2*SA3*DBLE(CA1**INT(3.D0))*dgAtMZ())/(CB2*MW*SB) - (0.1875D0*CA3*m12squared*DBLE(CA1**INT(3.D0))*dgAtM&
  &Z())/(CB*MW*SB2) - (0.1875D0*CA22*CA3*m12squared*DBLE(CA1**INT(3.D0))*dgAtMZ())/(CB*MW*SB2) + (0.1875D0*CA3*m12squared*SA22*DB&
  &LE(CA1**INT(3.D0))*dgAtMZ())/(CB*MW*SB2) - (0.125D0*CA3*MH12*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB*MW) - (0.125D0*CA22*CA3*MH12*D&
  &BLE(SA1**INT(3.D0))*dgAtMZ())/(CB*MW) - (0.0625D0*CA3*MH22*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB*MW) - (0.0625D0*CA22*CA3*MH22*DB&
  &LE(SA1**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.125D0*CA3*MH12*SA22*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.0625D0*CA3*MH22*SA22&
  &*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.1875D0*CA3*m12squared*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB2*MW*SB) + (0.1875D0*CA22&
  &*CA3*m12squared*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB2*MW*SB) - (0.1875D0*CA3*m12squared*SA22*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB2&
  &*MW*SB) + (0.0625D0*MH12*SA2*SA3*DBLE(SA1**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.1875D0*CA22*MH12*SA2*SA3*DBLE(SA1**INT(3.D0))*dgA&
  &tMZ())/(MW*SB) + (0.03125D0*MH22*SA2*SA3*DBLE(SA1**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.09375D0*CA22*MH22*SA2*SA3*DBLE(SA1**INT(3&
  &.D0))*dgAtMZ())/(MW*SB) - (0.09375D0*m12squared*SA2*SA3*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB*MW*SB2) - (0.28125D0*CA22*m12square&
  &d*SA2*SA3*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB*MW*SB2) - (0.1875D0*CA1*MH12*SA3*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW) - (0.0937&
  &5D0*CA1*MH22*SA3*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW) - (0.28125D0*m12squared*SA1*SA3*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW)&
  & + (0.1875D0*CA1*MH12*SA12*SA3*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.09375D0*CA1*MH22*SA12*SA3*DBLE(SA2**INT(3.D0))*dgAt&
  &MZ())/(CB*MW) - (0.28125D0*CA1*m12squared*SA3*DBLE(SA2**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.1875D0*CA1*m12squared*SA3*DBLE(SA2**&
  &INT(3.D0))*dgAtMZ())/(CB2*MW*SB) - (0.1875D0*MH12*SA1*SA3*DBLE(SA2**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.1875D0*CA12*MH12*SA1*SA3&
  &*DBLE(SA2**INT(3.D0))*dgAtMZ())/(MW*SB) - (0.09375D0*MH22*SA1*SA3*DBLE(SA2**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.09375D0*CA12*MH2&
  &2*SA1*SA3*DBLE(SA2**INT(3.D0))*dgAtMZ())/(MW*SB) - (0.28125D0*CA1*m12squared*SA12*SA3*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB2*MW*S&
  &B) + (0.1875D0*m12squared*SA1*SA3*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW*SB2) - (0.28125D0*CA12*m12squared*SA1*SA3*DBLE(SA2**IN&
  &T(3.D0))*dgAtMZ())/(CB*MW*SB2) + (0.09375D0*m12squared*SA1*SA3*DBLE(SA2**INT(3.D0))*dgAtMZ())/(MW*SB*TB) + (0.09375D0*CA1*m12s&
  &quared*SA3*TB*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW) - (0.0625D0*MH12*SA3*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dgAtMZ())/&
  &(CB*MW) - (0.03125D0*MH22*SA3*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.09375D0*m12squared*SA3*DBLE(CA1&
  &**INT(3.D0))*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB2*MW*SB) - (0.0625D0*MH12*SA3*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dgAtMZ(&
  &))/(MW*SB) - (0.03125D0*MH22*SA3*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.09375D0*m12squared*SA3*DBLE(&
  &SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW*SB2) - (0.125D0*CA1*CA3*EL*dm122MSBarUsual())/(CB*MW*SW) - (0.125D0*CA1*&
  &CA22*CA3*EL*dm122MSBarUsual())/(CB*MW*SW) + (0.125D0*CA1*CA3*EL*SA22*dm122MSBarUsual())/(CB*MW*SW) + (0.28125D0*EL*SA1*SA2*SA3&
  &*dm122MSBarUsual())/(CB*MW*SW) + (0.84375D0*CA22*EL*SA1*SA2*SA3*dm122MSBarUsual())/(CB*MW*SW) + (0.21875D0*CA3*EL*SA1*dm122MSB&
  &arUsual())/(MW*SB*SW) + (0.21875D0*CA22*CA3*EL*SA1*dm122MSBarUsual())/(MW*SB*SW) - (0.15625D0*CA3*EL*SA1*dm122MSBarUsual())/(C&
  &B2*MW*SB*SW) - (0.5625D0*CA12*CA3*EL*SA1*dm122MSBarUsual())/(CB2*MW*SB*SW) - (0.15625D0*CA22*CA3*EL*SA1*dm122MSBarUsual())/(CB&
  &2*MW*SB*SW) - (0.5625D0*CA12*CA22*CA3*EL*SA1*dm122MSBarUsual())/(CB2*MW*SB*SW) - (0.21875D0*CA3*EL*SA1*SA22*dm122MSBarUsual())&
  &/(MW*SB*SW) + (0.15625D0*CA3*EL*SA1*SA22*dm122MSBarUsual())/(CB2*MW*SB*SW) + (0.5625D0*CA12*CA3*EL*SA1*SA22*dm122MSBarUsual())&
  &/(CB2*MW*SB*SW) + (0.28125D0*CA1*EL*SA2*SA3*dm122MSBarUsual())/(MW*SB*SW) + (0.84375D0*CA1*CA22*EL*SA2*SA3*dm122MSBarUsual())/&
  &(MW*SB*SW) - (0.1875D0*CA1*EL*SA2*SA3*dm122MSBarUsual())/(CB2*MW*SB*SW) - (0.5625D0*CA1*CA22*EL*SA2*SA3*dm122MSBarUsual())/(CB&
  &2*MW*SB*SW) + (0.28125D0*CA1*EL*SA12*SA2*SA3*dm122MSBarUsual())/(CB2*MW*SB*SW) + (0.84375D0*CA1*CA22*EL*SA12*SA2*SA3*dm122MSBa&
  &rUsual())/(CB2*MW*SB*SW) + (0.0625D0*CA1*CA3*EL*dm122MSBarUsual())/(CB*MW*SB2*SW) + (0.0625D0*CA1*CA22*CA3*EL*dm122MSBarUsual(&
  &))/(CB*MW*SB2*SW) + (0.5625D0*CA1*CA3*EL*SA12*dm122MSBarUsual())/(CB*MW*SB2*SW) + (0.5625D0*CA1*CA22*CA3*EL*SA12*dm122MSBarUsu&
  &al())/(CB*MW*SB2*SW) - (0.0625D0*CA1*CA3*EL*SA22*dm122MSBarUsual())/(CB*MW*SB2*SW) - (0.5625D0*CA1*CA3*EL*SA12*SA22*dm122MSBar&
  &Usual())/(CB*MW*SB2*SW) - (0.1875D0*EL*SA1*SA2*SA3*dm122MSBarUsual())/(CB*MW*SB2*SW) + (0.28125D0*CA12*EL*SA1*SA2*SA3*dm122MSB&
  &arUsual())/(CB*MW*SB2*SW) - (0.5625D0*CA22*EL*SA1*SA2*SA3*dm122MSBarUsual())/(CB*MW*SB2*SW) + (0.84375D0*CA12*CA22*EL*SA1*SA2*&
  &SA3*dm122MSBarUsual())/(CB*MW*SB2*SW) + (0.125D0*CA1*CA3*EL*dm122MSBarUsual())/(MW*SB*SW*TB) + (0.125D0*CA1*CA22*CA3*EL*dm122M&
  &SBarUsual())/(MW*SB*SW*TB) - (0.125D0*CA1*CA3*EL*SA22*dm122MSBarUsual())/(MW*SB*SW*TB) - (0.09375D0*EL*SA1*SA2*SA3*dm122MSBarU&
  &sual())/(MW*SB*SW*TB) - (0.28125D0*CA22*EL*SA1*SA2*SA3*dm122MSBarUsual())/(MW*SB*SW*TB) - (0.03125D0*CA3*EL*SA1*TB*dm122MSBarU&
  &sual())/(CB*MW*SW) - (0.03125D0*CA22*CA3*EL*SA1*TB*dm122MSBarUsual())/(CB*MW*SW) + (0.03125D0*CA3*EL*SA1*SA22*TB*dm122MSBarUsu&
  &al())/(CB*MW*SW) - (0.09375D0*CA1*EL*SA2*SA3*TB*dm122MSBarUsual())/(CB*MW*SW) - (0.28125D0*CA1*CA22*EL*SA2*SA3*TB*dm122MSBarUs&
  &ual())/(CB*MW*SW) - (0.09375D0*EL*SA2*SA3*DBLE(CA1**INT(3.D0))*dm122MSBarUsual())/(CB2*MW*SB*SW) - (0.28125D0*CA22*EL*SA2*SA3*&
  &DBLE(CA1**INT(3.D0))*dm122MSBarUsual())/(CB2*MW*SB*SW) - (0.1875D0*CA3*EL*DBLE(CA1**INT(3.D0))*dm122MSBarUsual())/(CB*MW*SB2*S&
  &W) - (0.1875D0*CA22*CA3*EL*DBLE(CA1**INT(3.D0))*dm122MSBarUsual())/(CB*MW*SB2*SW) + (0.1875D0*CA3*EL*SA22*DBLE(CA1**INT(3.D0))&
  &*dm122MSBarUsual())/(CB*MW*SB2*SW) + (0.1875D0*CA3*EL*DBLE(SA1**INT(3.D0))*dm122MSBarUsual())/(CB2*MW*SB*SW) + (0.1875D0*CA22*&
  &CA3*EL*DBLE(SA1**INT(3.D0))*dm122MSBarUsual())/(CB2*MW*SB*SW) - (0.1875D0*CA3*EL*SA22*DBLE(SA1**INT(3.D0))*dm122MSBarUsual())/&
  &(CB2*MW*SB*SW) - (0.09375D0*EL*SA2*SA3*DBLE(SA1**INT(3.D0))*dm122MSBarUsual())/(CB*MW*SB2*SW) - (0.28125D0*CA22*EL*SA2*SA3*DBL&
  &E(SA1**INT(3.D0))*dm122MSBarUsual())/(CB*MW*SB2*SW) - (0.28125D0*EL*SA1*SA3*DBLE(SA2**INT(3.D0))*dm122MSBarUsual())/(CB*MW*SW)&
  & - (0.28125D0*CA1*EL*SA3*DBLE(SA2**INT(3.D0))*dm122MSBarUsual())/(MW*SB*SW) + (0.1875D0*CA1*EL*SA3*DBLE(SA2**INT(3.D0))*dm122M&
  &SBarUsual())/(CB2*MW*SB*SW) - (0.28125D0*CA1*EL*SA12*SA3*DBLE(SA2**INT(3.D0))*dm122MSBarUsual())/(CB2*MW*SB*SW) + (0.1875D0*EL&
  &*SA1*SA3*DBLE(SA2**INT(3.D0))*dm122MSBarUsual())/(CB*MW*SB2*SW) - (0.28125D0*CA12*EL*SA1*SA3*DBLE(SA2**INT(3.D0))*dm122MSBarUs&
  &ual())/(CB*MW*SB2*SW) + (0.09375D0*EL*SA1*SA3*DBLE(SA2**INT(3.D0))*dm122MSBarUsual())/(MW*SB*SW*TB) + (0.09375D0*CA1*EL*SA3*TB&
  &*DBLE(SA2**INT(3.D0))*dm122MSBarUsual())/(CB*MW*SW) + (0.09375D0*EL*SA3*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dm122MSBarUs&
  &ual())/(CB2*MW*SB*SW) + (0.09375D0*EL*SA3*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dm122MSBarUsual())/(CB*MW*SB2*SW) + (0.125&
  &D0*CA3*EL*SA1*dMH12OSUsual())/(CB*MW*SW) + (0.375D0*CA12*CA3*EL*SA1*dMH12OSUsual())/(CB*MW*SW) + (0.125D0*CA22*CA3*EL*SA1*dMH1&
  &2OSUsual())/(CB*MW*SW) + (0.375D0*CA12*CA22*CA3*EL*SA1*dMH12OSUsual())/(CB*MW*SW) - (0.125D0*CA3*EL*SA1*SA22*dMH12OSUsual())/(&
  &CB*MW*SW) - (0.375D0*CA12*CA3*EL*SA1*SA22*dMH12OSUsual())/(CB*MW*SW) + (0.1875D0*CA1*EL*SA2*SA3*dMH12OSUsual())/(CB*MW*SW) + (&
  &0.5625D0*CA1*CA22*EL*SA2*SA3*dMH12OSUsual())/(CB*MW*SW) - (0.1875D0*CA1*EL*SA12*SA2*SA3*dMH12OSUsual())/(CB*MW*SW) - (0.5625D0&
  &*CA1*CA22*EL*SA12*SA2*SA3*dMH12OSUsual())/(CB*MW*SW) - (0.125D0*CA1*CA3*EL*dMH12OSUsual())/(MW*SB*SW) - (0.125D0*CA1*CA22*CA3*&
  &EL*dMH12OSUsual())/(MW*SB*SW) - (0.375D0*CA1*CA3*EL*SA12*dMH12OSUsual())/(MW*SB*SW) - (0.375D0*CA1*CA22*CA3*EL*SA12*dMH12OSUsu&
  &al())/(MW*SB*SW) + (0.125D0*CA1*CA3*EL*SA22*dMH12OSUsual())/(MW*SB*SW) + (0.375D0*CA1*CA3*EL*SA12*SA22*dMH12OSUsual())/(MW*SB*&
  &SW) + (0.1875D0*EL*SA1*SA2*SA3*dMH12OSUsual())/(MW*SB*SW) - (0.1875D0*CA12*EL*SA1*SA2*SA3*dMH12OSUsual())/(MW*SB*SW) + (0.5625&
  &D0*CA22*EL*SA1*SA2*SA3*dMH12OSUsual())/(MW*SB*SW) - (0.5625D0*CA12*CA22*EL*SA1*SA2*SA3*dMH12OSUsual())/(MW*SB*SW) - (0.5D0*CA2&
  &*SA3*dMH12OSUsual())/vS - (1.5D0*CA2*SA22*SA3*dMH12OSUsual())/vS + (0.0625D0*EL*SA2*SA3*DBLE(CA1**INT(3.D0))*dMH12OSUsual())/(&
  &CB*MW*SW) + (0.1875D0*CA22*EL*SA2*SA3*DBLE(CA1**INT(3.D0))*dMH12OSUsual())/(CB*MW*SW) + (0.125D0*CA3*EL*DBLE(CA1**INT(3.D0))*d&
  &MH12OSUsual())/(MW*SB*SW) + (0.125D0*CA22*CA3*EL*DBLE(CA1**INT(3.D0))*dMH12OSUsual())/(MW*SB*SW) - (0.125D0*CA3*EL*SA22*DBLE(C&
  &A1**INT(3.D0))*dMH12OSUsual())/(MW*SB*SW) + (0.5D0*SA3*DBLE(CA2**INT(3.D0))*dMH12OSUsual())/vS - (0.125D0*CA3*EL*DBLE(SA1**INT&
  &(3.D0))*dMH12OSUsual())/(CB*MW*SW) - (0.125D0*CA22*CA3*EL*DBLE(SA1**INT(3.D0))*dMH12OSUsual())/(CB*MW*SW) + (0.125D0*CA3*EL*SA&
  &22*DBLE(SA1**INT(3.D0))*dMH12OSUsual())/(CB*MW*SW) + (0.0625D0*EL*SA2*SA3*DBLE(SA1**INT(3.D0))*dMH12OSUsual())/(MW*SB*SW) + (0&
  &.1875D0*CA22*EL*SA2*SA3*DBLE(SA1**INT(3.D0))*dMH12OSUsual())/(MW*SB*SW) - (0.1875D0*CA1*EL*SA3*DBLE(SA2**INT(3.D0))*dMH12OSUsu&
  &al())/(CB*MW*SW) + (0.1875D0*CA1*EL*SA12*SA3*DBLE(SA2**INT(3.D0))*dMH12OSUsual())/(CB*MW*SW) - (0.1875D0*EL*SA1*SA3*DBLE(SA2**&
  &INT(3.D0))*dMH12OSUsual())/(MW*SB*SW) + (0.1875D0*CA12*EL*SA1*SA3*DBLE(SA2**INT(3.D0))*dMH12OSUsual())/(MW*SB*SW) - (0.0625D0*&
  &EL*SA3*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dMH12OSUsual())/(CB*MW*SW) - (0.0625D0*EL*SA3*DBLE(SA1**INT(3.D0))*DBLE(SA2**&
  &INT(3.D0))*dMH12OSUsual())/(MW*SB*SW) + (0.0625D0*CA3*EL*SA1*dMH22OSUsual())/(CB*MW*SW) + (0.1875D0*CA12*CA3*EL*SA1*dMH22OSUsu&
  &al())/(CB*MW*SW) + (0.0625D0*CA22*CA3*EL*SA1*dMH22OSUsual())/(CB*MW*SW) + (0.1875D0*CA12*CA22*CA3*EL*SA1*dMH22OSUsual())/(CB*M&
  &W*SW) - (0.0625D0*CA3*EL*SA1*SA22*dMH22OSUsual())/(CB*MW*SW) - (0.1875D0*CA12*CA3*EL*SA1*SA22*dMH22OSUsual())/(CB*MW*SW) + (0.&
  &09375D0*CA1*EL*SA2*SA3*dMH22OSUsual())/(CB*MW*SW) + (0.28125D0*CA1*CA22*EL*SA2*SA3*dMH22OSUsual())/(CB*MW*SW) - (0.09375D0*CA1&
  &*EL*SA12*SA2*SA3*dMH22OSUsual())/(CB*MW*SW) - (0.28125D0*CA1*CA22*EL*SA12*SA2*SA3*dMH22OSUsual())/(CB*MW*SW) - (0.0625D0*CA1*C&
  &A3*EL*dMH22OSUsual())/(MW*SB*SW) - (0.0625D0*CA1*CA22*CA3*EL*dMH22OSUsual())/(MW*SB*SW) - (0.1875D0*CA1*CA3*EL*SA12*dMH22OSUsu&
  &al())/(MW*SB*SW) - (0.1875D0*CA1*CA22*CA3*EL*SA12*dMH22OSUsual())/(MW*SB*SW) + (0.0625D0*CA1*CA3*EL*SA22*dMH22OSUsual())/(MW*S&
  &B*SW) + (0.1875D0*CA1*CA3*EL*SA12*SA22*dMH22OSUsual())/(MW*SB*SW) + (0.09375D0*EL*SA1*SA2*SA3*dMH22OSUsual())/(MW*SB*SW) - (0.&
  &09375D0*CA12*EL*SA1*SA2*SA3*dMH22OSUsual())/(MW*SB*SW) + (0.28125D0*CA22*EL*SA1*SA2*SA3*dMH22OSUsual())/(MW*SB*SW) - (0.28125D&
  &0*CA12*CA22*EL*SA1*SA2*SA3*dMH22OSUsual())/(MW*SB*SW) - (0.25D0*CA2*SA3*dMH22OSUsual())/vS - (0.75D0*CA2*SA22*SA3*dMH22OSUsual&
  &())/vS + (0.03125D0*EL*SA2*SA3*DBLE(CA1**INT(3.D0))*dMH22OSUsual())/(CB*MW*SW) + (0.09375D0*CA22*EL*SA2*SA3*DBLE(CA1**INT(3.D0&
  &))*dMH22OSUsual())/(CB*MW*SW) + (0.0625D0*CA3*EL*DBLE(CA1**INT(3.D0))*dMH22OSUsual())/(MW*SB*SW) + (0.0625D0*CA22*CA3*EL*DBLE(&
  &CA1**INT(3.D0))*dMH22OSUsual())/(MW*SB*SW) - (0.0625D0*CA3*EL*SA22*DBLE(CA1**INT(3.D0))*dMH22OSUsual())/(MW*SB*SW) + (0.25D0*S&
  &A3*DBLE(CA2**INT(3.D0))*dMH22OSUsual())/vS - (0.0625D0*CA3*EL*DBLE(SA1**INT(3.D0))*dMH22OSUsual())/(CB*MW*SW) - (0.0625D0*CA22&
  &*CA3*EL*DBLE(SA1**INT(3.D0))*dMH22OSUsual())/(CB*MW*SW) + (0.0625D0*CA3*EL*SA22*DBLE(SA1**INT(3.D0))*dMH22OSUsual())/(CB*MW*SW&
  &) + (0.03125D0*EL*SA2*SA3*DBLE(SA1**INT(3.D0))*dMH22OSUsual())/(MW*SB*SW) + (0.09375D0*CA22*EL*SA2*SA3*DBLE(SA1**INT(3.D0))*dM&
  &H22OSUsual())/(MW*SB*SW) - (0.09375D0*CA1*EL*SA3*DBLE(SA2**INT(3.D0))*dMH22OSUsual())/(CB*MW*SW) + (0.09375D0*CA1*EL*SA12*SA3*&
  &DBLE(SA2**INT(3.D0))*dMH22OSUsual())/(CB*MW*SW) - (0.09375D0*EL*SA1*SA3*DBLE(SA2**INT(3.D0))*dMH22OSUsual())/(MW*SB*SW) + (0.0&
  &9375D0*CA12*EL*SA1*SA3*DBLE(SA2**INT(3.D0))*dMH22OSUsual())/(MW*SB*SW) - (0.03125D0*EL*SA3*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(&
  &3.D0))*dMH22OSUsual())/(CB*MW*SW) - (0.03125D0*EL*SA3*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dMH22OSUsual())/(MW*SB*SW) + (&
  &0.0625D0*CA1*CA3*EL*m12squared*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SW) + (0.0625D0*CA1*CA22*CA3*EL*m12squared*DBLE(MW**INT(-&
  &3.D0))*dMW2Usual())/(CB*SW) - (0.0625D0*CA3*EL*MH12*SA1*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SW) - (0.1875D0*CA12*CA3*EL*MH12&
  &*SA1*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SW) - (0.0625D0*CA22*CA3*EL*MH12*SA1*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SW) - (0&
  &.1875D0*CA12*CA22*CA3*EL*MH12*SA1*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SW) - (0.03125D0*CA3*EL*MH22*SA1*DBLE(MW**INT(-3.D0))*&
  &dMW2Usual())/(CB*SW) - (0.09375D0*CA12*CA3*EL*MH22*SA1*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SW) - (0.03125D0*CA22*CA3*EL*MH22&
  &*SA1*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SW) - (0.09375D0*CA12*CA22*CA3*EL*MH22*SA1*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SW&
  &) - (0.0625D0*CA1*CA3*EL*m12squared*SA22*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SW) + (0.0625D0*CA3*EL*MH12*SA1*SA22*DBLE(MW**I&
  &NT(-3.D0))*dMW2Usual())/(CB*SW) + (0.1875D0*CA12*CA3*EL*MH12*SA1*SA22*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SW) + (0.03125D0*C&
  &A3*EL*MH22*SA1*SA22*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SW) + (0.09375D0*CA12*CA3*EL*MH22*SA1*SA22*DBLE(MW**INT(-3.D0))*dMW2&
  &Usual())/(CB*SW) - (0.09375D0*CA1*EL*MH12*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SW) - (0.28125D0*CA1*CA22*EL*MH12*SA2*&
  &SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SW) - (0.046875D0*CA1*EL*MH22*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SW) - (0&
  &.140625D0*CA1*CA22*EL*MH22*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SW) - (0.140625D0*EL*m12squared*SA1*SA2*SA3*DBLE(MW**&
  &INT(-3.D0))*dMW2Usual())/(CB*SW) - (0.421875D0*CA22*EL*m12squared*SA1*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SW) + (0.0&
  &9375D0*CA1*EL*MH12*SA12*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SW) + (0.28125D0*CA1*CA22*EL*MH12*SA12*SA2*SA3*DBLE(MW**&
  &INT(-3.D0))*dMW2Usual())/(CB*SW) + (0.046875D0*CA1*EL*MH22*SA12*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SW) + (0.140625D&
  &0*CA1*CA22*EL*MH22*SA12*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SW) + (0.0625D0*CA1*CA3*EL*MH12*DBLE(MW**INT(-3.D0))*dMW&
  &2Usual())/(SB*SW) + (0.0625D0*CA1*CA22*CA3*EL*MH12*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW) + (0.03125D0*CA1*CA3*EL*MH22*DBLE&
  &(MW**INT(-3.D0))*dMW2Usual())/(SB*SW) + (0.03125D0*CA1*CA22*CA3*EL*MH22*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW) - (0.109375D&
  &0*CA3*EL*m12squared*SA1*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW) - (0.109375D0*CA22*CA3*EL*m12squared*SA1*DBLE(MW**INT(-3.D0)&
  &)*dMW2Usual())/(SB*SW) + (0.078125D0*CA3*EL*m12squared*SA1*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB2*SB*SW) + (0.28125D0*CA12*CA3&
  &*EL*m12squared*SA1*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB2*SB*SW) + (0.078125D0*CA22*CA3*EL*m12squared*SA1*DBLE(MW**INT(-3.D0))&
  &*dMW2Usual())/(CB2*SB*SW) + (0.28125D0*CA12*CA22*CA3*EL*m12squared*SA1*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB2*SB*SW) + (0.1875&
  &D0*CA1*CA3*EL*MH12*SA12*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW) + (0.1875D0*CA1*CA22*CA3*EL*MH12*SA12*DBLE(MW**INT(-3.D0))*d&
  &MW2Usual())/(SB*SW) + (0.09375D0*CA1*CA3*EL*MH22*SA12*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW) + (0.09375D0*CA1*CA22*CA3*EL*M&
  &H22*SA12*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW) - (0.0625D0*CA1*CA3*EL*MH12*SA22*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW) &
  &- (0.03125D0*CA1*CA3*EL*MH22*SA22*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW) + (0.109375D0*CA3*EL*m12squared*SA1*SA22*DBLE(MW**&
  &INT(-3.D0))*dMW2Usual())/(SB*SW) - (0.078125D0*CA3*EL*m12squared*SA1*SA22*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB2*SB*SW) - (0.2&
  &8125D0*CA12*CA3*EL*m12squared*SA1*SA22*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB2*SB*SW) - (0.1875D0*CA1*CA3*EL*MH12*SA12*SA22*DBL&
  &E(MW**INT(-3.D0))*dMW2Usual())/(SB*SW) - (0.09375D0*CA1*CA3*EL*MH22*SA12*SA22*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW) - (0.1&
  &40625D0*CA1*EL*m12squared*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW) - (0.421875D0*CA1*CA22*EL*m12squared*SA2*SA3*DBLE(&
  &MW**INT(-3.D0))*dMW2Usual())/(SB*SW) + (0.09375D0*CA1*EL*m12squared*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB2*SB*SW) + (0&
  &.28125D0*CA1*CA22*EL*m12squared*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB2*SB*SW) - (0.09375D0*EL*MH12*SA1*SA2*SA3*DBLE(MW&
  &**INT(-3.D0))*dMW2Usual())/(SB*SW) + (0.09375D0*CA12*EL*MH12*SA1*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW) - (0.28125D&
  &0*CA22*EL*MH12*SA1*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW) + (0.28125D0*CA12*CA22*EL*MH12*SA1*SA2*SA3*DBLE(MW**INT(-&
  &3.D0))*dMW2Usual())/(SB*SW) - (0.046875D0*EL*MH22*SA1*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW) + (0.046875D0*CA12*EL*&
  &MH22*SA1*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW) - (0.140625D0*CA22*EL*MH22*SA1*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Usu&
  &al())/(SB*SW) + (0.140625D0*CA12*CA22*EL*MH22*SA1*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW) - (0.140625D0*CA1*EL*m12sq&
  &uared*SA12*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB2*SB*SW) - (0.421875D0*CA1*CA22*EL*m12squared*SA12*SA2*SA3*DBLE(MW**IN&
  &T(-3.D0))*dMW2Usual())/(CB2*SB*SW) - (0.03125D0*CA1*CA3*EL*m12squared*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SB2*SW) - (0.03125&
  &D0*CA1*CA22*CA3*EL*m12squared*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SB2*SW) - (0.28125D0*CA1*CA3*EL*m12squared*SA12*DBLE(MW**I&
  &NT(-3.D0))*dMW2Usual())/(CB*SB2*SW) - (0.28125D0*CA1*CA22*CA3*EL*m12squared*SA12*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SB2*SW)&
  & + (0.03125D0*CA1*CA3*EL*m12squared*SA22*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SB2*SW) + (0.28125D0*CA1*CA3*EL*m12squared*SA12&
  &*SA22*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SB2*SW) + (0.09375D0*EL*m12squared*SA1*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(&
  &CB*SB2*SW) - (0.140625D0*CA12*EL*m12squared*SA1*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SB2*SW) + (0.28125D0*CA22*EL*m12&
  &squared*SA1*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SB2*SW) - (0.421875D0*CA12*CA22*EL*m12squared*SA1*SA2*SA3*DBLE(MW**I&
  &NT(-3.D0))*dMW2Usual())/(CB*SB2*SW) - (0.0625D0*CA1*CA3*EL*m12squared*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW*TB) - (0.0625D0&
  &*CA1*CA22*CA3*EL*m12squared*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW*TB) + (0.0625D0*CA1*CA3*EL*m12squared*SA22*DBLE(MW**INT(-&
  &3.D0))*dMW2Usual())/(SB*SW*TB) + (0.046875D0*EL*m12squared*SA1*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW*TB) + (0.14062&
  &5D0*CA22*EL*m12squared*SA1*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW*TB) + (0.015625D0*CA3*EL*m12squared*SA1*TB*DBLE(MW&
  &**INT(-3.D0))*dMW2Usual())/(CB*SW) + (0.015625D0*CA22*CA3*EL*m12squared*SA1*TB*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SW) - (0.&
  &015625D0*CA3*EL*m12squared*SA1*SA22*TB*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SW) + (0.046875D0*CA1*EL*m12squared*SA2*SA3*TB*DB&
  &LE(MW**INT(-3.D0))*dMW2Usual())/(CB*SW) + (0.140625D0*CA1*CA22*EL*m12squared*SA2*SA3*TB*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*&
  &SW) - (0.03125D0*EL*MH12*SA2*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SW) - (0.09375D0*CA22*EL*MH12*SA2*&
  &SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SW) - (0.015625D0*EL*MH22*SA2*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW*&
  &*INT(-3.D0))*dMW2Usual())/(CB*SW) - (0.046875D0*CA22*EL*MH22*SA2*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Usual())/(C&
  &B*SW) - (0.0625D0*CA3*EL*MH12*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW) - (0.0625D0*CA22*CA3*EL*MH12*DBLE&
  &(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW) - (0.03125D0*CA3*EL*MH22*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*&
  &dMW2Usual())/(SB*SW) - (0.03125D0*CA22*CA3*EL*MH22*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW) + (0.0625D0*&
  &CA3*EL*MH12*SA22*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW) + (0.03125D0*CA3*EL*MH22*SA22*DBLE(CA1**INT(3.&
  &D0))*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW) + (0.046875D0*EL*m12squared*SA2*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*d&
  &MW2Usual())/(CB2*SB*SW) + (0.140625D0*CA22*EL*m12squared*SA2*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB2*S&
  &B*SW) + (0.09375D0*CA3*EL*m12squared*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SB2*SW) + (0.09375D0*CA22*CA3*&
  &EL*m12squared*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SB2*SW) - (0.09375D0*CA3*EL*m12squared*SA22*DBLE(CA1*&
  &*INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SB2*SW) + (0.0625D0*CA3*EL*MH12*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dM&
  &W2Usual())/(CB*SW) + (0.0625D0*CA22*CA3*EL*MH12*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Usual())/(CB*SW) + (0.03125D0*CA&
  &3*EL*MH22*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Usual())/(CB*SW) + (0.03125D0*CA22*CA3*EL*MH22*DBLE(MW**INT(-3.D0))*DB&
  &LE(SA1**INT(3.D0))*dMW2Usual())/(CB*SW) - (0.0625D0*CA3*EL*MH12*SA22*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Usual())/(C&
  &B*SW) - (0.03125D0*CA3*EL*MH22*SA22*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Usual())/(CB*SW) - (0.09375D0*CA3*EL*m12squa&
  &red*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Usual())/(CB2*SB*SW) - (0.09375D0*CA22*CA3*EL*m12squared*DBLE(MW**INT(-3.D0)&
  &)*DBLE(SA1**INT(3.D0))*dMW2Usual())/(CB2*SB*SW) + (0.09375D0*CA3*EL*m12squared*SA22*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*&
  &dMW2Usual())/(CB2*SB*SW) - (0.03125D0*EL*MH12*SA2*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Usual())/(SB*SW) - (0.0937&
  &5D0*CA22*EL*MH12*SA2*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Usual())/(SB*SW) - (0.015625D0*EL*MH22*SA2*SA3*DBLE(MW*&
  &*INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Usual())/(SB*SW) - (0.046875D0*CA22*EL*MH22*SA2*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3&
  &.D0))*dMW2Usual())/(SB*SW) + (0.046875D0*EL*m12squared*SA2*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Usual())/(CB*SB2*&
  &SW) + (0.140625D0*CA22*EL*m12squared*SA2*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Usual())/(CB*SB2*SW) + (0.09375D0*C&
  &A1*EL*MH12*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Usual())/(CB*SW) + (0.046875D0*CA1*EL*MH22*SA3*DBLE(MW**INT(-3.D0&
  &))*DBLE(SA2**INT(3.D0))*dMW2Usual())/(CB*SW) + (0.140625D0*EL*m12squared*SA1*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW&
  &2Usual())/(CB*SW) - (0.09375D0*CA1*EL*MH12*SA12*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Usual())/(CB*SW) - (0.046875&
  &D0*CA1*EL*MH22*SA12*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Usual())/(CB*SW) + (0.140625D0*CA1*EL*m12squared*SA3*DBL&
  &E(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Usual())/(SB*SW) - (0.09375D0*CA1*EL*m12squared*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA2**&
  &INT(3.D0))*dMW2Usual())/(CB2*SB*SW) + (0.09375D0*EL*MH12*SA1*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Usual())/(SB*SW&
  &) - (0.09375D0*CA12*EL*MH12*SA1*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Usual())/(SB*SW) + (0.046875D0*EL*MH22*SA1*S&
  &A3*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Usual())/(SB*SW) - (0.046875D0*CA12*EL*MH22*SA1*SA3*DBLE(MW**INT(-3.D0))*DBLE&
  &(SA2**INT(3.D0))*dMW2Usual())/(SB*SW) + (0.140625D0*CA1*EL*m12squared*SA12*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2U&
  &sual())/(CB2*SB*SW) - (0.09375D0*EL*m12squared*SA1*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Usual())/(CB*SB2*SW) + (0&
  &.140625D0*CA12*EL*m12squared*SA1*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Usual())/(CB*SB2*SW) - (0.046875D0*EL*m12sq&
  &uared*SA1*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Usual())/(SB*SW*TB) - (0.046875D0*CA1*EL*m12squared*SA3*TB*DBLE(MW&
  &**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Usual())/(CB*SW) + (0.03125D0*EL*MH12*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*DBL&
  &E(SA2**INT(3.D0))*dMW2Usual())/(CB*SW) + (0.015625D0*EL*MH22*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0)&
  &)*dMW2Usual())/(CB*SW) - (0.046875D0*EL*m12squared*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Usua&
  &l())/(CB2*SB*SW) + (0.03125D0*EL*MH12*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dMW2Usual())/(SB*SW) &
  &+ (0.015625D0*EL*MH22*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dMW2Usual())/(SB*SW) - (0.046875D0*EL&
  &*m12squared*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dMW2Usual())/(CB*SB2*SW) + 0.5D0*CA2*MH12*SA3*D&
  &BLE(vS**INT(-2.D0))*dvSMSBarUsual() + 0.25D0*CA2*MH22*SA3*DBLE(vS**INT(-2.D0))*dvSMSBarUsual() + 1.5D0*CA2*MH12*SA22*SA3*DBLE(&
  &vS**INT(-2.D0))*dvSMSBarUsual() + 0.75D0*CA2*MH22*SA22*SA3*DBLE(vS**INT(-2.D0))*dvSMSBarUsual() - 0.5D0*MH12*SA3*DBLE(CA2**INT&
  &(3.D0))*DBLE(vS**INT(-2.D0))*dvSMSBarUsual() - 0.25D0*MH22*SA3*DBLE(CA2**INT(3.D0))*DBLE(vS**INT(-2.D0))*dvSMSBarUsual() &
		& + CS1S1S1f111*dZH1H2OSUsual()/2D0 + CS1S1S1f211*dZH2H2OS()/2D0 + CS1S1S1f311*dZH3H2OSUsual()/2D0 &
		& + CS1S1S1f121*dZH1H1OS()/2D0 + CS1S1S1f221*dZH2H1OSUsual()/2D0 + CS1S1S1f321*dZH3H1OSUsual()/2D0 &
		& + CS1S1S1f121*dZH1H1OS()/2D0 + CS1S1S1f221*dZH2H1OSUsual()/2D0 + CS1S1S1f321*dZH3H1OSUsual()/2D0 &
		& )
	case (2)
		dRR11 = -CA2*SA1*dAlpha1KanUsual() - CA1*SA2*dAlpha2KanUsual()
		dRR12 = CA1*CA2*dAlpha1KanUsual() - SA1*SA2*dAlpha2KanUsual()
		dRR13 = CA2*dAlpha2KanUsual()
		dRR21 = -CA1*CA3*dAlpha1KanUsual() - CA1*CA3*SA2*dAlpha3KanUsual() - CA1*CA2*SA3*dAlpha2KanUsual() + &
			& SA1*SA3*dAlpha3KanUsual() + SA1*SA2*SA3*dAlpha1KanUsual()
		dRR22 = -CA3*SA1*dAlpha1KanUsual() - CA3*SA1*SA2*dAlpha3KanUsual() - CA1*SA3*dAlpha3KanUsual() - &
			& CA2*SA1*SA3*dAlpha2KanUsual() - CA1*SA2*SA3*dAlpha1KanUsual()
		dRR23 = CA2*CA3*dAlpha3KanUsual() - SA2*SA3*dAlpha2KanUsual()
		dRR31 = -CA1*CA2*CA3*dAlpha2KanUsual() + CA3*SA1*dAlpha3KanUsual() + CA3*SA1*SA2*dAlpha1KanUsual() + &
			& CA1*SA3*dAlpha1KanUsual() + CA1*SA2*SA3*dAlpha3KanUsual()
		dRR32 = -CA1*CA3*dAlpha3KanUsual() - CA2*CA3*SA1*dAlpha2KanUsual() - CA1*CA3*SA2*dAlpha1KanUsual() + &
			& SA1*SA3*dAlpha1KanUsual() + SA1*SA2*SA3*dAlpha3KanUsual()
		dRR33 = -CA3*SA2*dAlpha2KanUsual() - CA2*SA3*dAlpha3KanUsual()

		totalAmplitude = CS1S1S1f211*( &
&(0.125D0*CA1*CA3*EL*MH12*dAlpha1KanUsual())/(CB*MW*SW) + (0.125D0*CA1*CA22*CA3*EL*MH12*dAlpha1KanUsual())/(CB*MW*SW) + (0.0625D0*&
  &CA1*CA3*EL*MH22*dAlpha1KanUsual())/(CB*MW*SW) + (0.0625D0*CA1*CA22*CA3*EL*MH22*dAlpha1KanUsual())/(CB*MW*SW) + (0.125D0*CA3*EL&
  &*m12squared*SA1*dAlpha1KanUsual())/(CB*MW*SW) + (0.125D0*CA22*CA3*EL*m12squared*SA1*dAlpha1KanUsual())/(CB*MW*SW) - (1.125D0*C&
  &A1*CA3*EL*MH12*SA12*dAlpha1KanUsual())/(CB*MW*SW) - (1.125D0*CA1*CA22*CA3*EL*MH12*SA12*dAlpha1KanUsual())/(CB*MW*SW) - (0.5625&
  &D0*CA1*CA3*EL*MH22*SA12*dAlpha1KanUsual())/(CB*MW*SW) - (0.5625D0*CA1*CA22*CA3*EL*MH22*SA12*dAlpha1KanUsual())/(CB*MW*SW) - (0&
  &.125D0*CA1*CA3*EL*MH12*SA22*dAlpha1KanUsual())/(CB*MW*SW) - (0.0625D0*CA1*CA3*EL*MH22*SA22*dAlpha1KanUsual())/(CB*MW*SW) - (0.&
  &125D0*CA3*EL*m12squared*SA1*SA22*dAlpha1KanUsual())/(CB*MW*SW) + (1.125D0*CA1*CA3*EL*MH12*SA12*SA22*dAlpha1KanUsual())/(CB*MW*&
  &SW) + (0.5625D0*CA1*CA3*EL*MH22*SA12*SA22*dAlpha1KanUsual())/(CB*MW*SW) + (0.28125D0*CA1*EL*m12squared*SA2*SA3*dAlpha1KanUsual&
  &())/(CB*MW*SW) + (0.84375D0*CA1*CA22*EL*m12squared*SA2*SA3*dAlpha1KanUsual())/(CB*MW*SW) - (0.1875D0*EL*MH12*SA1*SA2*SA3*dAlph&
  &a1KanUsual())/(CB*MW*SW) - (0.5625D0*CA12*EL*MH12*SA1*SA2*SA3*dAlpha1KanUsual())/(CB*MW*SW) - (0.5625D0*CA22*EL*MH12*SA1*SA2*S&
  &A3*dAlpha1KanUsual())/(CB*MW*SW) - (1.6875D0*CA12*CA22*EL*MH12*SA1*SA2*SA3*dAlpha1KanUsual())/(CB*MW*SW) - (0.09375D0*EL*MH22*&
  &SA1*SA2*SA3*dAlpha1KanUsual())/(CB*MW*SW) - (0.28125D0*CA12*EL*MH22*SA1*SA2*SA3*dAlpha1KanUsual())/(CB*MW*SW) - (0.28125D0*CA2&
  &2*EL*MH22*SA1*SA2*SA3*dAlpha1KanUsual())/(CB*MW*SW) - (0.84375D0*CA12*CA22*EL*MH22*SA1*SA2*SA3*dAlpha1KanUsual())/(CB*MW*SW) +&
  & (0.21875D0*CA1*CA3*EL*m12squared*dAlpha1KanUsual())/(MW*SB*SW) + (0.21875D0*CA1*CA22*CA3*EL*m12squared*dAlpha1KanUsual())/(MW&
  &*SB*SW) - (0.15625D0*CA1*CA3*EL*m12squared*dAlpha1KanUsual())/(CB2*MW*SB*SW) - (0.15625D0*CA1*CA22*CA3*EL*m12squared*dAlpha1Ka&
  &nUsual())/(CB2*MW*SB*SW) + (0.125D0*CA3*EL*MH12*SA1*dAlpha1KanUsual())/(MW*SB*SW) - (1.125D0*CA12*CA3*EL*MH12*SA1*dAlpha1KanUs&
  &ual())/(MW*SB*SW) + (0.125D0*CA22*CA3*EL*MH12*SA1*dAlpha1KanUsual())/(MW*SB*SW) - (1.125D0*CA12*CA22*CA3*EL*MH12*SA1*dAlpha1Ka&
  &nUsual())/(MW*SB*SW) + (0.0625D0*CA3*EL*MH22*SA1*dAlpha1KanUsual())/(MW*SB*SW) - (0.5625D0*CA12*CA3*EL*MH22*SA1*dAlpha1KanUsua&
  &l())/(MW*SB*SW) + (0.0625D0*CA22*CA3*EL*MH22*SA1*dAlpha1KanUsual())/(MW*SB*SW) - (0.5625D0*CA12*CA22*CA3*EL*MH22*SA1*dAlpha1Ka&
  &nUsual())/(MW*SB*SW) + (1.6875D0*CA1*CA3*EL*m12squared*SA12*dAlpha1KanUsual())/(CB2*MW*SB*SW) + (1.6875D0*CA1*CA22*CA3*EL*m12s&
  &quared*SA12*dAlpha1KanUsual())/(CB2*MW*SB*SW) - (0.21875D0*CA1*CA3*EL*m12squared*SA22*dAlpha1KanUsual())/(MW*SB*SW) + (0.15625&
  &D0*CA1*CA3*EL*m12squared*SA22*dAlpha1KanUsual())/(CB2*MW*SB*SW) - (0.125D0*CA3*EL*MH12*SA1*SA22*dAlpha1KanUsual())/(MW*SB*SW) &
  &+ (1.125D0*CA12*CA3*EL*MH12*SA1*SA22*dAlpha1KanUsual())/(MW*SB*SW) - (0.0625D0*CA3*EL*MH22*SA1*SA22*dAlpha1KanUsual())/(MW*SB*&
  &SW) + (0.5625D0*CA12*CA3*EL*MH22*SA1*SA22*dAlpha1KanUsual())/(MW*SB*SW) - (1.6875D0*CA1*CA3*EL*m12squared*SA12*SA22*dAlpha1Kan&
  &Usual())/(CB2*MW*SB*SW) + (0.1875D0*CA1*EL*MH12*SA2*SA3*dAlpha1KanUsual())/(MW*SB*SW) + (0.5625D0*CA1*CA22*EL*MH12*SA2*SA3*dAl&
  &pha1KanUsual())/(MW*SB*SW) + (0.09375D0*CA1*EL*MH22*SA2*SA3*dAlpha1KanUsual())/(MW*SB*SW) + (0.28125D0*CA1*CA22*EL*MH22*SA2*SA&
  &3*dAlpha1KanUsual())/(MW*SB*SW) - (0.28125D0*EL*m12squared*SA1*SA2*SA3*dAlpha1KanUsual())/(MW*SB*SW) - (0.84375D0*CA22*EL*m12s&
  &quared*SA1*SA2*SA3*dAlpha1KanUsual())/(MW*SB*SW) + (0.1875D0*EL*m12squared*SA1*SA2*SA3*dAlpha1KanUsual())/(CB2*MW*SB*SW) + (0.&
  &84375D0*CA12*EL*m12squared*SA1*SA2*SA3*dAlpha1KanUsual())/(CB2*MW*SB*SW) + (0.5625D0*CA22*EL*m12squared*SA1*SA2*SA3*dAlpha1Kan&
  &Usual())/(CB2*MW*SB*SW) + (2.53125D0*CA12*CA22*EL*m12squared*SA1*SA2*SA3*dAlpha1KanUsual())/(CB2*MW*SB*SW) + (0.5625D0*CA1*EL*&
  &MH12*SA12*SA2*SA3*dAlpha1KanUsual())/(MW*SB*SW) + (1.6875D0*CA1*CA22*EL*MH12*SA12*SA2*SA3*dAlpha1KanUsual())/(MW*SB*SW) + (0.2&
  &8125D0*CA1*EL*MH22*SA12*SA2*SA3*dAlpha1KanUsual())/(MW*SB*SW) + (0.84375D0*CA1*CA22*EL*MH22*SA12*SA2*SA3*dAlpha1KanUsual())/(M&
  &W*SB*SW) - (0.0625D0*CA3*EL*m12squared*SA1*dAlpha1KanUsual())/(CB*MW*SB2*SW) + (1.6875D0*CA12*CA3*EL*m12squared*SA1*dAlpha1Kan&
  &Usual())/(CB*MW*SB2*SW) - (0.0625D0*CA22*CA3*EL*m12squared*SA1*dAlpha1KanUsual())/(CB*MW*SB2*SW) + (1.6875D0*CA12*CA22*CA3*EL*&
  &m12squared*SA1*dAlpha1KanUsual())/(CB*MW*SB2*SW) + (0.0625D0*CA3*EL*m12squared*SA1*SA22*dAlpha1KanUsual())/(CB*MW*SB2*SW) - (1&
  &.6875D0*CA12*CA3*EL*m12squared*SA1*SA22*dAlpha1KanUsual())/(CB*MW*SB2*SW) - (0.1875D0*CA1*EL*m12squared*SA2*SA3*dAlpha1KanUsua&
  &l())/(CB*MW*SB2*SW) - (0.5625D0*CA1*CA22*EL*m12squared*SA2*SA3*dAlpha1KanUsual())/(CB*MW*SB2*SW) - (0.84375D0*CA1*EL*m12square&
  &d*SA12*SA2*SA3*dAlpha1KanUsual())/(CB*MW*SB2*SW) - (2.53125D0*CA1*CA22*EL*m12squared*SA12*SA2*SA3*dAlpha1KanUsual())/(CB*MW*SB&
  &2*SW) - (0.125D0*CA3*EL*m12squared*SA1*dAlpha1KanUsual())/(MW*SB*SW*TB) - (0.125D0*CA22*CA3*EL*m12squared*SA1*dAlpha1KanUsual(&
  &))/(MW*SB*SW*TB) + (0.125D0*CA3*EL*m12squared*SA1*SA22*dAlpha1KanUsual())/(MW*SB*SW*TB) - (0.09375D0*CA1*EL*m12squared*SA2*SA3&
  &*dAlpha1KanUsual())/(MW*SB*SW*TB) - (0.28125D0*CA1*CA22*EL*m12squared*SA2*SA3*dAlpha1KanUsual())/(MW*SB*SW*TB) - (0.03125D0*CA&
  &1*CA3*EL*m12squared*TB*dAlpha1KanUsual())/(CB*MW*SW) - (0.03125D0*CA1*CA22*CA3*EL*m12squared*TB*dAlpha1KanUsual())/(CB*MW*SW) &
  &+ (0.03125D0*CA1*CA3*EL*m12squared*SA22*TB*dAlpha1KanUsual())/(CB*MW*SW) + (0.09375D0*EL*m12squared*SA1*SA2*SA3*TB*dAlpha1KanU&
  &sual())/(CB*MW*SW) + (0.28125D0*CA22*EL*m12squared*SA1*SA2*SA3*TB*dAlpha1KanUsual())/(CB*MW*SW) + (0.5D0*CA1*CA2*CA3*EL*m12squ&
  &ared*SA2*dAlpha2KanUsual())/(CB*MW*SW) - (0.5D0*CA2*CA3*EL*MH12*SA1*SA2*dAlpha2KanUsual())/(CB*MW*SW) - (1.5D0*CA12*CA2*CA3*EL&
  &*MH12*SA1*SA2*dAlpha2KanUsual())/(CB*MW*SW) - (0.25D0*CA2*CA3*EL*MH22*SA1*SA2*dAlpha2KanUsual())/(CB*MW*SW) - (0.75D0*CA12*CA2&
  &*CA3*EL*MH22*SA1*SA2*dAlpha2KanUsual())/(CB*MW*SW) + (0.1875D0*CA1*CA2*EL*MH12*SA3*dAlpha2KanUsual())/(CB*MW*SW) + (0.09375D0*&
  &CA1*CA2*EL*MH22*SA3*dAlpha2KanUsual())/(CB*MW*SW) + (0.28125D0*CA2*EL*m12squared*SA1*SA3*dAlpha2KanUsual())/(CB*MW*SW) - (0.18&
  &75D0*CA1*CA2*EL*MH12*SA12*SA3*dAlpha2KanUsual())/(CB*MW*SW) - (0.09375D0*CA1*CA2*EL*MH22*SA12*SA3*dAlpha2KanUsual())/(CB*MW*SW&
  &) - (1.6875D0*CA1*CA2*EL*MH12*SA22*SA3*dAlpha2KanUsual())/(CB*MW*SW) - (0.84375D0*CA1*CA2*EL*MH22*SA22*SA3*dAlpha2KanUsual())/&
  &(CB*MW*SW) - (2.53125D0*CA2*EL*m12squared*SA1*SA22*SA3*dAlpha2KanUsual())/(CB*MW*SW) + (1.6875D0*CA1*CA2*EL*MH12*SA12*SA22*SA3&
  &*dAlpha2KanUsual())/(CB*MW*SW) + (0.84375D0*CA1*CA2*EL*MH22*SA12*SA22*SA3*dAlpha2KanUsual())/(CB*MW*SW) + (0.5D0*CA1*CA2*CA3*E&
  &L*MH12*SA2*dAlpha2KanUsual())/(MW*SB*SW) + (0.25D0*CA1*CA2*CA3*EL*MH22*SA2*dAlpha2KanUsual())/(MW*SB*SW) - (0.875D0*CA2*CA3*EL&
  &*m12squared*SA1*SA2*dAlpha2KanUsual())/(MW*SB*SW) + (0.625D0*CA2*CA3*EL*m12squared*SA1*SA2*dAlpha2KanUsual())/(CB2*MW*SB*SW) +&
  & (2.25D0*CA12*CA2*CA3*EL*m12squared*SA1*SA2*dAlpha2KanUsual())/(CB2*MW*SB*SW) + (1.5D0*CA1*CA2*CA3*EL*MH12*SA12*SA2*dAlpha2Kan&
  &Usual())/(MW*SB*SW) + (0.75D0*CA1*CA2*CA3*EL*MH22*SA12*SA2*dAlpha2KanUsual())/(MW*SB*SW) + (0.28125D0*CA1*CA2*EL*m12squared*SA&
  &3*dAlpha2KanUsual())/(MW*SB*SW) - (0.1875D0*CA1*CA2*EL*m12squared*SA3*dAlpha2KanUsual())/(CB2*MW*SB*SW) + (0.1875D0*CA2*EL*MH1&
  &2*SA1*SA3*dAlpha2KanUsual())/(MW*SB*SW) - (0.1875D0*CA12*CA2*EL*MH12*SA1*SA3*dAlpha2KanUsual())/(MW*SB*SW) + (0.09375D0*CA2*EL&
  &*MH22*SA1*SA3*dAlpha2KanUsual())/(MW*SB*SW) - (0.09375D0*CA12*CA2*EL*MH22*SA1*SA3*dAlpha2KanUsual())/(MW*SB*SW) + (0.28125D0*C&
  &A1*CA2*EL*m12squared*SA12*SA3*dAlpha2KanUsual())/(CB2*MW*SB*SW) - (2.53125D0*CA1*CA2*EL*m12squared*SA22*SA3*dAlpha2KanUsual())&
  &/(MW*SB*SW) + (1.6875D0*CA1*CA2*EL*m12squared*SA22*SA3*dAlpha2KanUsual())/(CB2*MW*SB*SW) - (1.6875D0*CA2*EL*MH12*SA1*SA22*SA3*&
  &dAlpha2KanUsual())/(MW*SB*SW) + (1.6875D0*CA12*CA2*EL*MH12*SA1*SA22*SA3*dAlpha2KanUsual())/(MW*SB*SW) - (0.84375D0*CA2*EL*MH22&
  &*SA1*SA22*SA3*dAlpha2KanUsual())/(MW*SB*SW) + (0.84375D0*CA12*CA2*EL*MH22*SA1*SA22*SA3*dAlpha2KanUsual())/(MW*SB*SW) - (2.5312&
  &5D0*CA1*CA2*EL*m12squared*SA12*SA22*SA3*dAlpha2KanUsual())/(CB2*MW*SB*SW) - (0.25D0*CA1*CA2*CA3*EL*m12squared*SA2*dAlpha2KanUs&
  &ual())/(CB*MW*SB2*SW) - (2.25D0*CA1*CA2*CA3*EL*m12squared*SA12*SA2*dAlpha2KanUsual())/(CB*MW*SB2*SW) - (0.1875D0*CA2*EL*m12squ&
  &ared*SA1*SA3*dAlpha2KanUsual())/(CB*MW*SB2*SW) + (0.28125D0*CA12*CA2*EL*m12squared*SA1*SA3*dAlpha2KanUsual())/(CB*MW*SB2*SW) +&
  & (1.6875D0*CA2*EL*m12squared*SA1*SA22*SA3*dAlpha2KanUsual())/(CB*MW*SB2*SW) - (2.53125D0*CA12*CA2*EL*m12squared*SA1*SA22*SA3*d&
  &Alpha2KanUsual())/(CB*MW*SB2*SW) - (0.5D0*CA1*CA2*CA3*EL*m12squared*SA2*dAlpha2KanUsual())/(MW*SB*SW*TB) - (0.09375D0*CA2*EL*m&
  &12squared*SA1*SA3*dAlpha2KanUsual())/(MW*SB*SW*TB) + (0.84375D0*CA2*EL*m12squared*SA1*SA22*SA3*dAlpha2KanUsual())/(MW*SB*SW*TB&
  &) + (0.125D0*CA2*CA3*EL*m12squared*SA1*SA2*TB*dAlpha2KanUsual())/(CB*MW*SW) - (0.09375D0*CA1*CA2*EL*m12squared*SA3*TB*dAlpha2K&
  &anUsual())/(CB*MW*SW) + (0.84375D0*CA1*CA2*EL*m12squared*SA22*SA3*TB*dAlpha2KanUsual())/(CB*MW*SW) + (0.5D0*MH12*SA2*SA3*dAlph&
  &a2KanUsual())/vS - (4.5D0*CA22*MH12*SA2*SA3*dAlpha2KanUsual())/vS + (0.25D0*MH22*SA2*SA3*dAlpha2KanUsual())/vS - (2.25D0*CA22*&
  &MH22*SA2*SA3*dAlpha2KanUsual())/vS + (0.1875D0*CA1*CA3*EL*MH12*SA2*dAlpha3KanUsual())/(CB*MW*SW) + (0.5625D0*CA1*CA22*CA3*EL*M&
  &H12*SA2*dAlpha3KanUsual())/(CB*MW*SW) + (0.09375D0*CA1*CA3*EL*MH22*SA2*dAlpha3KanUsual())/(CB*MW*SW) + (0.28125D0*CA1*CA22*CA3&
  &*EL*MH22*SA2*dAlpha3KanUsual())/(CB*MW*SW) + (0.28125D0*CA3*EL*m12squared*SA1*SA2*dAlpha3KanUsual())/(CB*MW*SW) + (0.84375D0*C&
  &A22*CA3*EL*m12squared*SA1*SA2*dAlpha3KanUsual())/(CB*MW*SW) - (0.1875D0*CA1*CA3*EL*MH12*SA12*SA2*dAlpha3KanUsual())/(CB*MW*SW)&
  & - (0.5625D0*CA1*CA22*CA3*EL*MH12*SA12*SA2*dAlpha3KanUsual())/(CB*MW*SW) - (0.09375D0*CA1*CA3*EL*MH22*SA12*SA2*dAlpha3KanUsual&
  &())/(CB*MW*SW) - (0.28125D0*CA1*CA22*CA3*EL*MH22*SA12*SA2*dAlpha3KanUsual())/(CB*MW*SW) + (0.125D0*CA1*EL*m12squared*SA3*dAlph&
  &a3KanUsual())/(CB*MW*SW) + (0.125D0*CA1*CA22*EL*m12squared*SA3*dAlpha3KanUsual())/(CB*MW*SW) - (0.125D0*EL*MH12*SA1*SA3*dAlpha&
  &3KanUsual())/(CB*MW*SW) - (0.375D0*CA12*EL*MH12*SA1*SA3*dAlpha3KanUsual())/(CB*MW*SW) - (0.125D0*CA22*EL*MH12*SA1*SA3*dAlpha3K&
  &anUsual())/(CB*MW*SW) - (0.375D0*CA12*CA22*EL*MH12*SA1*SA3*dAlpha3KanUsual())/(CB*MW*SW) - (0.0625D0*EL*MH22*SA1*SA3*dAlpha3Ka&
  &nUsual())/(CB*MW*SW) - (0.1875D0*CA12*EL*MH22*SA1*SA3*dAlpha3KanUsual())/(CB*MW*SW) - (0.0625D0*CA22*EL*MH22*SA1*SA3*dAlpha3Ka&
  &nUsual())/(CB*MW*SW) - (0.1875D0*CA12*CA22*EL*MH22*SA1*SA3*dAlpha3KanUsual())/(CB*MW*SW) - (0.125D0*CA1*EL*m12squared*SA22*SA3&
  &*dAlpha3KanUsual())/(CB*MW*SW) + (0.125D0*EL*MH12*SA1*SA22*SA3*dAlpha3KanUsual())/(CB*MW*SW) + (0.375D0*CA12*EL*MH12*SA1*SA22*&
  &SA3*dAlpha3KanUsual())/(CB*MW*SW) + (0.0625D0*EL*MH22*SA1*SA22*SA3*dAlpha3KanUsual())/(CB*MW*SW) + (0.1875D0*CA12*EL*MH22*SA1*&
  &SA22*SA3*dAlpha3KanUsual())/(CB*MW*SW) + (0.28125D0*CA1*CA3*EL*m12squared*SA2*dAlpha3KanUsual())/(MW*SB*SW) + (0.84375D0*CA1*C&
  &A22*CA3*EL*m12squared*SA2*dAlpha3KanUsual())/(MW*SB*SW) - (0.1875D0*CA1*CA3*EL*m12squared*SA2*dAlpha3KanUsual())/(CB2*MW*SB*SW&
  &) - (0.5625D0*CA1*CA22*CA3*EL*m12squared*SA2*dAlpha3KanUsual())/(CB2*MW*SB*SW) + (0.1875D0*CA3*EL*MH12*SA1*SA2*dAlpha3KanUsual&
  &())/(MW*SB*SW) - (0.1875D0*CA12*CA3*EL*MH12*SA1*SA2*dAlpha3KanUsual())/(MW*SB*SW) + (0.5625D0*CA22*CA3*EL*MH12*SA1*SA2*dAlpha3&
  &KanUsual())/(MW*SB*SW) - (0.5625D0*CA12*CA22*CA3*EL*MH12*SA1*SA2*dAlpha3KanUsual())/(MW*SB*SW) + (0.09375D0*CA3*EL*MH22*SA1*SA&
  &2*dAlpha3KanUsual())/(MW*SB*SW) - (0.09375D0*CA12*CA3*EL*MH22*SA1*SA2*dAlpha3KanUsual())/(MW*SB*SW) + (0.28125D0*CA22*CA3*EL*M&
  &H22*SA1*SA2*dAlpha3KanUsual())/(MW*SB*SW) - (0.28125D0*CA12*CA22*CA3*EL*MH22*SA1*SA2*dAlpha3KanUsual())/(MW*SB*SW) + (0.28125D&
  &0*CA1*CA3*EL*m12squared*SA12*SA2*dAlpha3KanUsual())/(CB2*MW*SB*SW) + (0.84375D0*CA1*CA22*CA3*EL*m12squared*SA12*SA2*dAlpha3Kan&
  &Usual())/(CB2*MW*SB*SW) + (0.125D0*CA1*EL*MH12*SA3*dAlpha3KanUsual())/(MW*SB*SW) + (0.125D0*CA1*CA22*EL*MH12*SA3*dAlpha3KanUsu&
  &al())/(MW*SB*SW) + (0.0625D0*CA1*EL*MH22*SA3*dAlpha3KanUsual())/(MW*SB*SW) + (0.0625D0*CA1*CA22*EL*MH22*SA3*dAlpha3KanUsual())&
  &/(MW*SB*SW) - (0.21875D0*EL*m12squared*SA1*SA3*dAlpha3KanUsual())/(MW*SB*SW) - (0.21875D0*CA22*EL*m12squared*SA1*SA3*dAlpha3Ka&
  &nUsual())/(MW*SB*SW) + (0.15625D0*EL*m12squared*SA1*SA3*dAlpha3KanUsual())/(CB2*MW*SB*SW) + (0.5625D0*CA12*EL*m12squared*SA1*S&
  &A3*dAlpha3KanUsual())/(CB2*MW*SB*SW) + (0.15625D0*CA22*EL*m12squared*SA1*SA3*dAlpha3KanUsual())/(CB2*MW*SB*SW) + (0.5625D0*CA1&
  &2*CA22*EL*m12squared*SA1*SA3*dAlpha3KanUsual())/(CB2*MW*SB*SW) + (0.375D0*CA1*EL*MH12*SA12*SA3*dAlpha3KanUsual())/(MW*SB*SW) +&
  & (0.375D0*CA1*CA22*EL*MH12*SA12*SA3*dAlpha3KanUsual())/(MW*SB*SW) + (0.1875D0*CA1*EL*MH22*SA12*SA3*dAlpha3KanUsual())/(MW*SB*S&
  &W) + (0.1875D0*CA1*CA22*EL*MH22*SA12*SA3*dAlpha3KanUsual())/(MW*SB*SW) - (0.125D0*CA1*EL*MH12*SA22*SA3*dAlpha3KanUsual())/(MW*&
  &SB*SW) - (0.0625D0*CA1*EL*MH22*SA22*SA3*dAlpha3KanUsual())/(MW*SB*SW) + (0.21875D0*EL*m12squared*SA1*SA22*SA3*dAlpha3KanUsual(&
  &))/(MW*SB*SW) - (0.15625D0*EL*m12squared*SA1*SA22*SA3*dAlpha3KanUsual())/(CB2*MW*SB*SW) - (0.5625D0*CA12*EL*m12squared*SA1*SA2&
  &2*SA3*dAlpha3KanUsual())/(CB2*MW*SB*SW) - (0.375D0*CA1*EL*MH12*SA12*SA22*SA3*dAlpha3KanUsual())/(MW*SB*SW) - (0.1875D0*CA1*EL*&
  &MH22*SA12*SA22*SA3*dAlpha3KanUsual())/(MW*SB*SW) - (0.1875D0*CA3*EL*m12squared*SA1*SA2*dAlpha3KanUsual())/(CB*MW*SB2*SW) + (0.&
  &28125D0*CA12*CA3*EL*m12squared*SA1*SA2*dAlpha3KanUsual())/(CB*MW*SB2*SW) - (0.5625D0*CA22*CA3*EL*m12squared*SA1*SA2*dAlpha3Kan&
  &Usual())/(CB*MW*SB2*SW) + (0.84375D0*CA12*CA22*CA3*EL*m12squared*SA1*SA2*dAlpha3KanUsual())/(CB*MW*SB2*SW) - (0.0625D0*CA1*EL*&
  &m12squared*SA3*dAlpha3KanUsual())/(CB*MW*SB2*SW) - (0.0625D0*CA1*CA22*EL*m12squared*SA3*dAlpha3KanUsual())/(CB*MW*SB2*SW) - (0&
  &.5625D0*CA1*EL*m12squared*SA12*SA3*dAlpha3KanUsual())/(CB*MW*SB2*SW) - (0.5625D0*CA1*CA22*EL*m12squared*SA12*SA3*dAlpha3KanUsu&
  &al())/(CB*MW*SB2*SW) + (0.0625D0*CA1*EL*m12squared*SA22*SA3*dAlpha3KanUsual())/(CB*MW*SB2*SW) + (0.5625D0*CA1*EL*m12squared*SA&
  &12*SA22*SA3*dAlpha3KanUsual())/(CB*MW*SB2*SW) - (0.09375D0*CA3*EL*m12squared*SA1*SA2*dAlpha3KanUsual())/(MW*SB*SW*TB) - (0.281&
  &25D0*CA22*CA3*EL*m12squared*SA1*SA2*dAlpha3KanUsual())/(MW*SB*SW*TB) - (0.125D0*CA1*EL*m12squared*SA3*dAlpha3KanUsual())/(MW*S&
  &B*SW*TB) - (0.125D0*CA1*CA22*EL*m12squared*SA3*dAlpha3KanUsual())/(MW*SB*SW*TB) + (0.125D0*CA1*EL*m12squared*SA22*SA3*dAlpha3K&
  &anUsual())/(MW*SB*SW*TB) - (0.09375D0*CA1*CA3*EL*m12squared*SA2*TB*dAlpha3KanUsual())/(CB*MW*SW) - (0.28125D0*CA1*CA22*CA3*EL*&
  &m12squared*SA2*TB*dAlpha3KanUsual())/(CB*MW*SW) + (0.03125D0*EL*m12squared*SA1*SA3*TB*dAlpha3KanUsual())/(CB*MW*SW) + (0.03125&
  &D0*CA22*EL*m12squared*SA1*SA3*TB*dAlpha3KanUsual())/(CB*MW*SW) - (0.03125D0*EL*m12squared*SA1*SA22*SA3*TB*dAlpha3KanUsual())/(&
  &CB*MW*SW) - (0.5D0*CA2*CA3*MH12*dAlpha3KanUsual())/vS - (0.25D0*CA2*CA3*MH22*dAlpha3KanUsual())/vS - (1.5D0*CA2*CA3*MH12*SA22*&
  &dAlpha3KanUsual())/vS - (0.75D0*CA2*CA3*MH22*SA22*dAlpha3KanUsual())/vS + (0.015625D0*CA3*EL*m12squared*SA1*dBeta2KanUsual())/&
  &(CB*MW*SW) + (0.015625D0*CA22*CA3*EL*m12squared*SA1*dBeta2KanUsual())/(CB*MW*SW) - (0.015625D0*CA3*EL*m12squared*SA1*SA22*dBet&
  &a2KanUsual())/(CB*MW*SW) + (0.09375D0*CA1*EL*m12squared*SA2*SA3*dBeta2KanUsual())/(CB*MW*SW) + (0.28125D0*CA1*CA22*EL*m12squar&
  &ed*SA2*SA3*dBeta2KanUsual())/(CB*MW*SW) - (0.0625D0*CA1*CA3*EL*m12squared*dBeta2KanUsual())/(MW*SB*SW) - (0.0625D0*CA1*CA22*CA&
  &3*EL*m12squared*dBeta2KanUsual())/(MW*SB*SW) + (0.0625D0*CA1*CA3*EL*m12squared*dBeta2KanUsual())/(CB2*MW*SB*SW) + (0.0625D0*CA&
  &1*CA22*CA3*EL*m12squared*dBeta2KanUsual())/(CB2*MW*SB*SW) - (0.0625D0*CA3*EL*MH12*SA1*dBeta2KanUsual())/(MW*SB*SW) - (0.1875D0&
  &*CA12*CA3*EL*MH12*SA1*dBeta2KanUsual())/(MW*SB*SW) - (0.0625D0*CA22*CA3*EL*MH12*SA1*dBeta2KanUsual())/(MW*SB*SW) - (0.1875D0*C&
  &A12*CA22*CA3*EL*MH12*SA1*dBeta2KanUsual())/(MW*SB*SW) + (0.0625D0*CA3*EL*MH12*SA1*dBeta2KanUsual())/(CB2*MW*SB*SW) + (0.1875D0&
  &*CA12*CA3*EL*MH12*SA1*dBeta2KanUsual())/(CB2*MW*SB*SW) + (0.0625D0*CA22*CA3*EL*MH12*SA1*dBeta2KanUsual())/(CB2*MW*SB*SW) + (0.&
  &1875D0*CA12*CA22*CA3*EL*MH12*SA1*dBeta2KanUsual())/(CB2*MW*SB*SW) - (0.03125D0*CA3*EL*MH22*SA1*dBeta2KanUsual())/(MW*SB*SW) - &
  &(0.09375D0*CA12*CA3*EL*MH22*SA1*dBeta2KanUsual())/(MW*SB*SW) - (0.03125D0*CA22*CA3*EL*MH22*SA1*dBeta2KanUsual())/(MW*SB*SW) - &
  &(0.09375D0*CA12*CA22*CA3*EL*MH22*SA1*dBeta2KanUsual())/(MW*SB*SW) + (0.03125D0*CA3*EL*MH22*SA1*dBeta2KanUsual())/(CB2*MW*SB*SW&
  &) + (0.09375D0*CA12*CA3*EL*MH22*SA1*dBeta2KanUsual())/(CB2*MW*SB*SW) + (0.03125D0*CA22*CA3*EL*MH22*SA1*dBeta2KanUsual())/(CB2*&
  &MW*SB*SW) + (0.09375D0*CA12*CA22*CA3*EL*MH22*SA1*dBeta2KanUsual())/(CB2*MW*SB*SW) + (0.5625D0*CA1*CA3*EL*m12squared*SA12*dBeta&
  &2KanUsual())/(CB2*MW*SB*SW) + (0.5625D0*CA1*CA22*CA3*EL*m12squared*SA12*dBeta2KanUsual())/(CB2*MW*SB*SW) + (0.0625D0*CA1*CA3*E&
  &L*m12squared*SA22*dBeta2KanUsual())/(MW*SB*SW) - (0.0625D0*CA1*CA3*EL*m12squared*SA22*dBeta2KanUsual())/(CB2*MW*SB*SW) + (0.06&
  &25D0*CA3*EL*MH12*SA1*SA22*dBeta2KanUsual())/(MW*SB*SW) + (0.1875D0*CA12*CA3*EL*MH12*SA1*SA22*dBeta2KanUsual())/(MW*SB*SW) - (0&
  &.0625D0*CA3*EL*MH12*SA1*SA22*dBeta2KanUsual())/(CB2*MW*SB*SW) - (0.1875D0*CA12*CA3*EL*MH12*SA1*SA22*dBeta2KanUsual())/(CB2*MW*&
  &SB*SW) + (0.03125D0*CA3*EL*MH22*SA1*SA22*dBeta2KanUsual())/(MW*SB*SW) + (0.09375D0*CA12*CA3*EL*MH22*SA1*SA22*dBeta2KanUsual())&
  &/(MW*SB*SW) - (0.03125D0*CA3*EL*MH22*SA1*SA22*dBeta2KanUsual())/(CB2*MW*SB*SW) - (0.09375D0*CA12*CA3*EL*MH22*SA1*SA22*dBeta2Ka&
  &nUsual())/(CB2*MW*SB*SW) - (0.5625D0*CA1*CA3*EL*m12squared*SA12*SA22*dBeta2KanUsual())/(CB2*MW*SB*SW) - (0.09375D0*CA1*EL*MH12&
  &*SA2*SA3*dBeta2KanUsual())/(MW*SB*SW) - (0.28125D0*CA1*CA22*EL*MH12*SA2*SA3*dBeta2KanUsual())/(MW*SB*SW) + (0.09375D0*CA1*EL*M&
  &H12*SA2*SA3*dBeta2KanUsual())/(CB2*MW*SB*SW) + (0.28125D0*CA1*CA22*EL*MH12*SA2*SA3*dBeta2KanUsual())/(CB2*MW*SB*SW) - (0.04687&
  &5D0*CA1*EL*MH22*SA2*SA3*dBeta2KanUsual())/(MW*SB*SW) - (0.140625D0*CA1*CA22*EL*MH22*SA2*SA3*dBeta2KanUsual())/(MW*SB*SW) + (0.&
  &046875D0*CA1*EL*MH22*SA2*SA3*dBeta2KanUsual())/(CB2*MW*SB*SW) + (0.140625D0*CA1*CA22*EL*MH22*SA2*SA3*dBeta2KanUsual())/(CB2*MW&
  &*SB*SW) - (0.140625D0*EL*m12squared*SA1*SA2*SA3*dBeta2KanUsual())/(MW*SB*SW) - (0.421875D0*CA22*EL*m12squared*SA1*SA2*SA3*dBet&
  &a2KanUsual())/(MW*SB*SW) - (0.046875D0*EL*m12squared*SA1*SA2*SA3*dBeta2KanUsual())/(CB2*MW*SB*SW) + (0.28125D0*CA12*EL*m12squa&
  &red*SA1*SA2*SA3*dBeta2KanUsual())/(CB2*MW*SB*SW) - (0.140625D0*CA22*EL*m12squared*SA1*SA2*SA3*dBeta2KanUsual())/(CB2*MW*SB*SW)&
  & + (0.84375D0*CA12*CA22*EL*m12squared*SA1*SA2*SA3*dBeta2KanUsual())/(CB2*MW*SB*SW) + (0.09375D0*CA1*EL*MH12*SA12*SA2*SA3*dBeta&
  &2KanUsual())/(MW*SB*SW) + (0.28125D0*CA1*CA22*EL*MH12*SA12*SA2*SA3*dBeta2KanUsual())/(MW*SB*SW) - (0.09375D0*CA1*EL*MH12*SA12*&
  &SA2*SA3*dBeta2KanUsual())/(CB2*MW*SB*SW) - (0.28125D0*CA1*CA22*EL*MH12*SA12*SA2*SA3*dBeta2KanUsual())/(CB2*MW*SB*SW) + (0.0468&
  &75D0*CA1*EL*MH22*SA12*SA2*SA3*dBeta2KanUsual())/(MW*SB*SW) + (0.140625D0*CA1*CA22*EL*MH22*SA12*SA2*SA3*dBeta2KanUsual())/(MW*S&
  &B*SW) - (0.046875D0*CA1*EL*MH22*SA12*SA2*SA3*dBeta2KanUsual())/(CB2*MW*SB*SW) - (0.140625D0*CA1*CA22*EL*MH22*SA12*SA2*SA3*dBet&
  &a2KanUsual())/(CB2*MW*SB*SW) + (0.203125D0*CA3*EL*m12squared*SA1*dBeta2KanUsual())/(CB*MW*SB2*SW) + (0.84375D0*CA12*CA3*EL*m12&
  &squared*SA1*dBeta2KanUsual())/(CB*MW*SB2*SW) + (0.203125D0*CA22*CA3*EL*m12squared*SA1*dBeta2KanUsual())/(CB*MW*SB2*SW) + (0.84&
  &375D0*CA12*CA22*CA3*EL*m12squared*SA1*dBeta2KanUsual())/(CB*MW*SB2*SW) - (0.203125D0*CA3*EL*m12squared*SA1*SA22*dBeta2KanUsual&
  &())/(CB*MW*SB2*SW) - (0.84375D0*CA12*CA3*EL*m12squared*SA1*SA22*dBeta2KanUsual())/(CB*MW*SB2*SW) + (0.10546875D0*CA1*EL*m12squ&
  &ared*SA2*SA3*dBeta2KanUsual())/(CB*MW*SB2*SW) + (0.31640625D0*CA1*CA22*EL*m12squared*SA2*SA3*dBeta2KanUsual())/(CB*MW*SB2*SW) &
  &- (0.38671875D0*CA1*EL*m12squared*SA12*SA2*SA3*dBeta2KanUsual())/(CB*MW*SB2*SW) - (1.16015625D0*CA1*CA22*EL*m12squared*SA12*SA&
  &2*SA3*dBeta2KanUsual())/(CB*MW*SB2*SW) + (0.125D0*CA1*CA3*EL*MH12*dBeta2KanUsual())/(MW*SB*SW*TB) + (0.125D0*CA1*CA22*CA3*EL*M&
  &H12*dBeta2KanUsual())/(MW*SB*SW*TB) + (0.0625D0*CA1*CA3*EL*MH22*dBeta2KanUsual())/(MW*SB*SW*TB) + (0.0625D0*CA1*CA22*CA3*EL*MH&
  &22*dBeta2KanUsual())/(MW*SB*SW*TB) - (0.203125D0*CA3*EL*m12squared*SA1*dBeta2KanUsual())/(MW*SB*SW*TB) - (0.203125D0*CA22*CA3*&
  &EL*m12squared*SA1*dBeta2KanUsual())/(MW*SB*SW*TB) + (0.375D0*CA1*CA3*EL*MH12*SA12*dBeta2KanUsual())/(MW*SB*SW*TB) + (0.375D0*C&
  &A1*CA22*CA3*EL*MH12*SA12*dBeta2KanUsual())/(MW*SB*SW*TB) + (0.1875D0*CA1*CA3*EL*MH22*SA12*dBeta2KanUsual())/(MW*SB*SW*TB) + (0&
  &.1875D0*CA1*CA22*CA3*EL*MH22*SA12*dBeta2KanUsual())/(MW*SB*SW*TB) - (0.125D0*CA1*CA3*EL*MH12*SA22*dBeta2KanUsual())/(MW*SB*SW*&
  &TB) - (0.0625D0*CA1*CA3*EL*MH22*SA22*dBeta2KanUsual())/(MW*SB*SW*TB) + (0.203125D0*CA3*EL*m12squared*SA1*SA22*dBeta2KanUsual()&
  &)/(MW*SB*SW*TB) - (0.375D0*CA1*CA3*EL*MH12*SA12*SA22*dBeta2KanUsual())/(MW*SB*SW*TB) - (0.1875D0*CA1*CA3*EL*MH22*SA12*SA22*dBe&
  &ta2KanUsual())/(MW*SB*SW*TB) - (0.140625D0*CA1*EL*m12squared*SA2*SA3*dBeta2KanUsual())/(MW*SB*SW*TB) - (0.421875D0*CA1*CA22*EL&
  &*m12squared*SA2*SA3*dBeta2KanUsual())/(MW*SB*SW*TB) - (0.1875D0*EL*MH12*SA1*SA2*SA3*dBeta2KanUsual())/(MW*SB*SW*TB) + (0.1875D&
  &0*CA12*EL*MH12*SA1*SA2*SA3*dBeta2KanUsual())/(MW*SB*SW*TB) - (0.5625D0*CA22*EL*MH12*SA1*SA2*SA3*dBeta2KanUsual())/(MW*SB*SW*TB&
  &) + (0.5625D0*CA12*CA22*EL*MH12*SA1*SA2*SA3*dBeta2KanUsual())/(MW*SB*SW*TB) - (0.09375D0*EL*MH22*SA1*SA2*SA3*dBeta2KanUsual())&
  &/(MW*SB*SW*TB) + (0.09375D0*CA12*EL*MH22*SA1*SA2*SA3*dBeta2KanUsual())/(MW*SB*SW*TB) - (0.28125D0*CA22*EL*MH22*SA1*SA2*SA3*dBe&
  &ta2KanUsual())/(MW*SB*SW*TB) + (0.28125D0*CA12*CA22*EL*MH22*SA1*SA2*SA3*dBeta2KanUsual())/(MW*SB*SW*TB) - (0.125D0*CA1*CA3*EL*&
  &m12squared*TB*dBeta2KanUsual())/(CB*MW*SW) - (0.125D0*CA1*CA22*CA3*EL*m12squared*TB*dBeta2KanUsual())/(CB*MW*SW) + (0.0625D0*C&
  &A3*EL*MH12*SA1*TB*dBeta2KanUsual())/(CB*MW*SW) + (0.1875D0*CA12*CA3*EL*MH12*SA1*TB*dBeta2KanUsual())/(CB*MW*SW) + (0.0625D0*CA&
  &22*CA3*EL*MH12*SA1*TB*dBeta2KanUsual())/(CB*MW*SW) + (0.1875D0*CA12*CA22*CA3*EL*MH12*SA1*TB*dBeta2KanUsual())/(CB*MW*SW) + (0.&
  &03125D0*CA3*EL*MH22*SA1*TB*dBeta2KanUsual())/(CB*MW*SW) + (0.09375D0*CA12*CA3*EL*MH22*SA1*TB*dBeta2KanUsual())/(CB*MW*SW) + (0&
  &.03125D0*CA22*CA3*EL*MH22*SA1*TB*dBeta2KanUsual())/(CB*MW*SW) + (0.09375D0*CA12*CA22*CA3*EL*MH22*SA1*TB*dBeta2KanUsual())/(CB*&
  &MW*SW) + (0.125D0*CA1*CA3*EL*m12squared*SA22*TB*dBeta2KanUsual())/(CB*MW*SW) - (0.0625D0*CA3*EL*MH12*SA1*SA22*TB*dBeta2KanUsua&
  &l())/(CB*MW*SW) - (0.1875D0*CA12*CA3*EL*MH12*SA1*SA22*TB*dBeta2KanUsual())/(CB*MW*SW) - (0.03125D0*CA3*EL*MH22*SA1*SA22*TB*dBe&
  &ta2KanUsual())/(CB*MW*SW) - (0.09375D0*CA12*CA3*EL*MH22*SA1*SA22*TB*dBeta2KanUsual())/(CB*MW*SW) + (0.09375D0*CA1*EL*MH12*SA2*&
  &SA3*TB*dBeta2KanUsual())/(CB*MW*SW) + (0.28125D0*CA1*CA22*EL*MH12*SA2*SA3*TB*dBeta2KanUsual())/(CB*MW*SW) + (0.046875D0*CA1*EL&
  &*MH22*SA2*SA3*TB*dBeta2KanUsual())/(CB*MW*SW) + (0.140625D0*CA1*CA22*EL*MH22*SA2*SA3*TB*dBeta2KanUsual())/(CB*MW*SW) + (0.1406&
  &25D0*EL*m12squared*SA1*SA2*SA3*TB*dBeta2KanUsual())/(CB*MW*SW) + (0.421875D0*CA22*EL*m12squared*SA1*SA2*SA3*TB*dBeta2KanUsual(&
  &))/(CB*MW*SW) - (0.09375D0*CA1*EL*MH12*SA12*SA2*SA3*TB*dBeta2KanUsual())/(CB*MW*SW) - (0.28125D0*CA1*CA22*EL*MH12*SA12*SA2*SA3&
  &*TB*dBeta2KanUsual())/(CB*MW*SW) - (0.046875D0*CA1*EL*MH22*SA12*SA2*SA3*TB*dBeta2KanUsual())/(CB*MW*SW) - (0.140625D0*CA1*CA22&
  &*EL*MH22*SA12*SA2*SA3*TB*dBeta2KanUsual())/(CB*MW*SW) - (0.1875D0*CA1*CA3*EL*m12squared*dBeta2KanUsual())/(MW*SB*SW*TB2) - (0.&
  &1875D0*CA1*CA22*CA3*EL*m12squared*dBeta2KanUsual())/(MW*SB*SW*TB2) + (0.1875D0*CA1*CA3*EL*m12squared*SA22*dBeta2KanUsual())/(M&
  &W*SB*SW*TB2) + (0.09375D0*EL*m12squared*SA1*SA2*SA3*dBeta2KanUsual())/(MW*SB*SW*TB2) + (0.28125D0*CA22*EL*m12squared*SA1*SA2*S&
  &A3*dBeta2KanUsual())/(MW*SB*SW*TB2) - (0.03125D0*CA3*EL*m12squared*SA1*TB2*dBeta2KanUsual())/(CB*MW*SW) - (0.03125D0*CA22*CA3*&
  &EL*m12squared*SA1*TB2*dBeta2KanUsual())/(CB*MW*SW) + (0.03125D0*CA3*EL*m12squared*SA1*SA22*TB2*dBeta2KanUsual())/(CB*MW*SW) - &
  &(0.140625D0*CA1*EL*m12squared*SA2*SA3*TB2*dBeta2KanUsual())/(CB*MW*SW) - (0.421875D0*CA1*CA22*EL*m12squared*SA2*SA3*TB2*dBeta2&
  &KanUsual())/(CB*MW*SW) + (0.375D0*CA3*EL*MH12*dAlpha1KanUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.375D0*CA22*CA3*EL*MH12*d&
  &Alpha1KanUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.1875D0*CA3*EL*MH22*dAlpha1KanUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) +&
  & (0.1875D0*CA22*CA3*EL*MH22*dAlpha1KanUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.375D0*CA3*EL*MH12*SA22*dAlpha1KanUsual()*D&
  &BLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.1875D0*CA3*EL*MH22*SA22*dAlpha1KanUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.5625D0*CA&
  &3*EL*m12squared*dAlpha1KanUsual()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.5625D0*CA22*CA3*EL*m12squared*dAlpha1KanUsual()*DB&
  &LE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) + (0.5625D0*CA3*EL*m12squared*SA22*dAlpha1KanUsual()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) -&
  & (0.1875D0*EL*MH12*SA2*SA3*dAlpha1KanUsual()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.5625D0*CA22*EL*MH12*SA2*SA3*dAlpha1KanUsual&
  &()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.09375D0*EL*MH22*SA2*SA3*dAlpha1KanUsual()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.28125&
  &D0*CA22*EL*MH22*SA2*SA3*dAlpha1KanUsual()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) + (0.28125D0*EL*m12squared*SA2*SA3*dAlpha1KanUsual(&
  &)*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.84375D0*CA22*EL*m12squared*SA2*SA3*dAlpha1KanUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW*&
  &SB2*SW) + (0.0625D0*CA2*EL*MH12*SA3*dAlpha2KanUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.03125D0*CA2*EL*MH22*SA3*dAlpha2Kan&
  &Usual()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.5625D0*CA2*EL*MH12*SA22*SA3*dAlpha2KanUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) -&
  & (0.28125D0*CA2*EL*MH22*SA22*SA3*dAlpha2KanUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.5D0*CA2*CA3*EL*MH12*SA2*dAlpha2KanUsu&
  &al()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.25D0*CA2*CA3*EL*MH22*SA2*dAlpha2KanUsual()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.09&
  &375D0*CA2*EL*m12squared*SA3*dAlpha2KanUsual()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) + (0.84375D0*CA2*EL*m12squared*SA22*SA3*dAl&
  &pha2KanUsual()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) + (0.75D0*CA2*CA3*EL*m12squared*SA2*dAlpha2KanUsual()*DBLE(CA1**INT(3.D0))&
  &)/(CB*MW*SB2*SW) + (0.0625D0*CA3*EL*MH12*SA2*dAlpha3KanUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.1875D0*CA22*CA3*EL*MH12*S&
  &A2*dAlpha3KanUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.03125D0*CA3*EL*MH22*SA2*dAlpha3KanUsual()*DBLE(CA1**INT(3.D0)))/(CB&
  &*MW*SW) + (0.09375D0*CA22*CA3*EL*MH22*SA2*dAlpha3KanUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.09375D0*CA3*EL*m12squared*SA&
  &2*dAlpha3KanUsual()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.28125D0*CA22*CA3*EL*m12squared*SA2*dAlpha3KanUsual()*DBLE(CA1**I&
  &NT(3.D0)))/(CB2*MW*SB*SW) - (0.125D0*EL*MH12*SA3*dAlpha3KanUsual()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.125D0*CA22*EL*MH12*SA&
  &3*dAlpha3KanUsual()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.0625D0*EL*MH22*SA3*dAlpha3KanUsual()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW&
  &) - (0.0625D0*CA22*EL*MH22*SA3*dAlpha3KanUsual()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) + (0.125D0*EL*MH12*SA22*SA3*dAlpha3KanUsual(&
  &)*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) + (0.0625D0*EL*MH22*SA22*SA3*dAlpha3KanUsual()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) + (0.1875D0&
  &*EL*m12squared*SA3*dAlpha3KanUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.1875D0*CA22*EL*m12squared*SA3*dAlpha3KanUsual()&
  &*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.1875D0*EL*m12squared*SA22*SA3*dAlpha3KanUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW&
  &) - (0.1875D0*CA3*EL*m12squared*dBeta2KanUsual()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.1875D0*CA22*CA3*EL*m12squared*dBeta&
  &2KanUsual()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) + (0.1875D0*CA3*EL*m12squared*SA22*dBeta2KanUsual()*DBLE(CA1**INT(3.D0)))/(CB&
  &2*MW*SB*SW) - (0.03125D0*EL*MH12*SA2*SA3*dBeta2KanUsual()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.09375D0*CA22*EL*MH12*SA2*SA3*d&
  &Beta2KanUsual()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) + (0.03125D0*EL*MH12*SA2*SA3*dBeta2KanUsual()*DBLE(CA1**INT(3.D0)))/(CB2*MW*S&
  &B*SW) + (0.09375D0*CA22*EL*MH12*SA2*SA3*dBeta2KanUsual()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.015625D0*EL*MH22*SA2*SA3*dB&
  &eta2KanUsual()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.046875D0*CA22*EL*MH22*SA2*SA3*dBeta2KanUsual()*DBLE(CA1**INT(3.D0)))/(MW*&
  &SB*SW) + (0.015625D0*EL*MH22*SA2*SA3*dBeta2KanUsual()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) + (0.046875D0*CA22*EL*MH22*SA2*SA3*&
  &dBeta2KanUsual()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) + (0.12890625D0*EL*m12squared*SA2*SA3*dBeta2KanUsual()*DBLE(CA1**INT(3.D&
  &0)))/(CB*MW*SB2*SW) + (0.38671875D0*CA22*EL*m12squared*SA2*SA3*dBeta2KanUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.125D&
  &0*CA3*EL*MH12*dBeta2KanUsual()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW*TB) - (0.125D0*CA22*CA3*EL*MH12*dBeta2KanUsual()*DBLE(CA1**INT(&
  &3.D0)))/(MW*SB*SW*TB) - (0.0625D0*CA3*EL*MH22*dBeta2KanUsual()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW*TB) - (0.0625D0*CA22*CA3*EL*MH2&
  &2*dBeta2KanUsual()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW*TB) + (0.125D0*CA3*EL*MH12*SA22*dBeta2KanUsual()*DBLE(CA1**INT(3.D0)))/(MW*&
  &SB*SW*TB) + (0.0625D0*CA3*EL*MH22*SA22*dBeta2KanUsual()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW*TB) + (0.03125D0*EL*MH12*SA2*SA3*TB*dB&
  &eta2KanUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.09375D0*CA22*EL*MH12*SA2*SA3*TB*dBeta2KanUsual()*DBLE(CA1**INT(3.D0)))/(C&
  &B*MW*SW) + (0.015625D0*EL*MH22*SA2*SA3*TB*dBeta2KanUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.046875D0*CA22*EL*MH22*SA2*SA3&
  &*TB*dBeta2KanUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.5625D0*CA1*EL*MH12*SA3*dAlpha2KanUsual()*DBLE(CA2**INT(3.D0)))/(CB*&
  &MW*SW) + (0.28125D0*CA1*EL*MH22*SA3*dAlpha2KanUsual()*DBLE(CA2**INT(3.D0)))/(CB*MW*SW) + (0.84375D0*EL*m12squared*SA1*SA3*dAlp&
  &ha2KanUsual()*DBLE(CA2**INT(3.D0)))/(CB*MW*SW) - (0.5625D0*CA1*EL*MH12*SA12*SA3*dAlpha2KanUsual()*DBLE(CA2**INT(3.D0)))/(CB*MW&
  &*SW) - (0.28125D0*CA1*EL*MH22*SA12*SA3*dAlpha2KanUsual()*DBLE(CA2**INT(3.D0)))/(CB*MW*SW) + (0.84375D0*CA1*EL*m12squared*SA3*d&
  &Alpha2KanUsual()*DBLE(CA2**INT(3.D0)))/(MW*SB*SW) - (0.5625D0*CA1*EL*m12squared*SA3*dAlpha2KanUsual()*DBLE(CA2**INT(3.D0)))/(C&
  &B2*MW*SB*SW) + (0.5625D0*EL*MH12*SA1*SA3*dAlpha2KanUsual()*DBLE(CA2**INT(3.D0)))/(MW*SB*SW) - (0.5625D0*CA12*EL*MH12*SA1*SA3*d&
  &Alpha2KanUsual()*DBLE(CA2**INT(3.D0)))/(MW*SB*SW) + (0.28125D0*EL*MH22*SA1*SA3*dAlpha2KanUsual()*DBLE(CA2**INT(3.D0)))/(MW*SB*&
  &SW) - (0.28125D0*CA12*EL*MH22*SA1*SA3*dAlpha2KanUsual()*DBLE(CA2**INT(3.D0)))/(MW*SB*SW) + (0.84375D0*CA1*EL*m12squared*SA12*S&
  &A3*dAlpha2KanUsual()*DBLE(CA2**INT(3.D0)))/(CB2*MW*SB*SW) - (0.5625D0*EL*m12squared*SA1*SA3*dAlpha2KanUsual()*DBLE(CA2**INT(3.&
  &D0)))/(CB*MW*SB2*SW) + (0.84375D0*CA12*EL*m12squared*SA1*SA3*dAlpha2KanUsual()*DBLE(CA2**INT(3.D0)))/(CB*MW*SB2*SW) - (0.28125&
  &D0*EL*m12squared*SA1*SA3*dAlpha2KanUsual()*DBLE(CA2**INT(3.D0)))/(MW*SB*SW*TB) - (0.28125D0*CA1*EL*m12squared*SA3*TB*dAlpha2Ka&
  &nUsual()*DBLE(CA2**INT(3.D0)))/(CB*MW*SW) + (0.5D0*CA3*MH12*dAlpha3KanUsual()*DBLE(CA2**INT(3.D0)))/vS + (0.25D0*CA3*MH22*dAlp&
  &ha3KanUsual()*DBLE(CA2**INT(3.D0)))/vS + (0.1875D0*EL*MH12*SA3*dAlpha2KanUsual()*DBLE(CA1**INT(3.D0))*DBLE(CA2**INT(3.D0)))/(C&
  &B*MW*SW) + (0.09375D0*EL*MH22*SA3*dAlpha2KanUsual()*DBLE(CA1**INT(3.D0))*DBLE(CA2**INT(3.D0)))/(CB*MW*SW) - (0.28125D0*EL*m12s&
  &quared*SA3*dAlpha2KanUsual()*DBLE(CA1**INT(3.D0))*DBLE(CA2**INT(3.D0)))/(CB2*MW*SB*SW) - (0.28125D0*CA3*EL*m12squared*SA1*dBet&
  &a2KanUsual()*DBLE(CB**INT(-3.D0)))/(MW*SW) - (0.84375D0*CA12*CA3*EL*m12squared*SA1*dBeta2KanUsual()*DBLE(CB**INT(-3.D0)))/(MW*&
  &SW) - (0.28125D0*CA22*CA3*EL*m12squared*SA1*dBeta2KanUsual()*DBLE(CB**INT(-3.D0)))/(MW*SW) - (0.84375D0*CA12*CA22*CA3*EL*m12sq&
  &uared*SA1*dBeta2KanUsual()*DBLE(CB**INT(-3.D0)))/(MW*SW) + (0.28125D0*CA3*EL*m12squared*SA1*SA22*dBeta2KanUsual()*DBLE(CB**INT&
  &(-3.D0)))/(MW*SW) + (0.84375D0*CA12*CA3*EL*m12squared*SA1*SA22*dBeta2KanUsual()*DBLE(CB**INT(-3.D0)))/(MW*SW) - (0.36328125D0*&
  &CA1*EL*m12squared*SA2*SA3*dBeta2KanUsual()*DBLE(CB**INT(-3.D0)))/(MW*SW) - (1.08984375D0*CA1*CA22*EL*m12squared*SA2*SA3*dBeta2&
  &KanUsual()*DBLE(CB**INT(-3.D0)))/(MW*SW) + (0.45703125D0*CA1*EL*m12squared*SA12*SA2*SA3*dBeta2KanUsual()*DBLE(CB**INT(-3.D0)))&
  &/(MW*SW) + (1.37109375D0*CA1*CA22*EL*m12squared*SA12*SA2*SA3*dBeta2KanUsual()*DBLE(CB**INT(-3.D0)))/(MW*SW) - (0.0625D0*CA3*EL&
  &*m12squared*SA1*dBeta2KanUsual()*DBLE(CB**INT(-3.D0)))/(MW*SB2*SW) - (0.28125D0*CA12*CA3*EL*m12squared*SA1*dBeta2KanUsual()*DB&
  &LE(CB**INT(-3.D0)))/(MW*SB2*SW) - (0.0625D0*CA22*CA3*EL*m12squared*SA1*dBeta2KanUsual()*DBLE(CB**INT(-3.D0)))/(MW*SB2*SW) - (0&
  &.28125D0*CA12*CA22*CA3*EL*m12squared*SA1*dBeta2KanUsual()*DBLE(CB**INT(-3.D0)))/(MW*SB2*SW) + (0.0625D0*CA3*EL*m12squared*SA1*&
  &SA22*dBeta2KanUsual()*DBLE(CB**INT(-3.D0)))/(MW*SB2*SW) + (0.28125D0*CA12*CA3*EL*m12squared*SA1*SA22*dBeta2KanUsual()*DBLE(CB*&
  &*INT(-3.D0)))/(MW*SB2*SW) - (0.05859375D0*CA1*EL*m12squared*SA2*SA3*dBeta2KanUsual()*DBLE(CB**INT(-3.D0)))/(MW*SB2*SW) - (0.17&
  &578125D0*CA1*CA22*EL*m12squared*SA2*SA3*dBeta2KanUsual()*DBLE(CB**INT(-3.D0)))/(MW*SB2*SW) + (0.10546875D0*CA1*EL*m12squared*S&
  &A12*SA2*SA3*dBeta2KanUsual()*DBLE(CB**INT(-3.D0)))/(MW*SB2*SW) + (0.31640625D0*CA1*CA22*EL*m12squared*SA12*SA2*SA3*dBeta2KanUs&
  &ual()*DBLE(CB**INT(-3.D0)))/(MW*SB2*SW) - (0.15234375D0*EL*m12squared*SA2*SA3*dBeta2KanUsual()*DBLE(CA1**INT(3.D0))*DBLE(CB**I&
  &NT(-3.D0)))/(MW*SW) - (0.45703125D0*CA22*EL*m12squared*SA2*SA3*dBeta2KanUsual()*DBLE(CA1**INT(3.D0))*DBLE(CB**INT(-3.D0)))/(MW&
  &*SW) - (0.03515625D0*EL*m12squared*SA2*SA3*dBeta2KanUsual()*DBLE(CA1**INT(3.D0))*DBLE(CB**INT(-3.D0)))/(MW*SB2*SW) - (0.105468&
  &75D0*CA22*EL*m12squared*SA2*SA3*dBeta2KanUsual()*DBLE(CA1**INT(3.D0))*DBLE(CB**INT(-3.D0)))/(MW*SB2*SW) + (0.1875D0*EL*MH12*SA&
  &2*SA3*dAlpha1KanUsual()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.5625D0*CA22*EL*MH12*SA2*SA3*dAlpha1KanUsual()*DBLE(SA1**INT(3.D0&
  &)))/(CB*MW*SW) + (0.09375D0*EL*MH22*SA2*SA3*dAlpha1KanUsual()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.28125D0*CA22*EL*MH22*SA2*S&
  &A3*dAlpha1KanUsual()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.375D0*CA3*EL*MH12*dAlpha1KanUsual()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW&
  &) + (0.375D0*CA22*CA3*EL*MH12*dAlpha1KanUsual()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.1875D0*CA3*EL*MH22*dAlpha1KanUsual()*DBL&
  &E(SA1**INT(3.D0)))/(MW*SB*SW) + (0.1875D0*CA22*CA3*EL*MH22*dAlpha1KanUsual()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0.375D0*CA3*E&
  &L*MH12*SA22*dAlpha1KanUsual()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0.1875D0*CA3*EL*MH22*SA22*dAlpha1KanUsual()*DBLE(SA1**INT(3.&
  &D0)))/(MW*SB*SW) - (0.28125D0*EL*m12squared*SA2*SA3*dAlpha1KanUsual()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.84375D0*CA22*E&
  &L*m12squared*SA2*SA3*dAlpha1KanUsual()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.5625D0*CA3*EL*m12squared*dAlpha1KanUsual()*DB&
  &LE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.5625D0*CA22*CA3*EL*m12squared*dAlpha1KanUsual()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) +&
  & (0.5625D0*CA3*EL*m12squared*SA22*dAlpha1KanUsual()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.5D0*CA2*CA3*EL*MH12*SA2*dAlpha2K&
  &anUsual()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.25D0*CA2*CA3*EL*MH22*SA2*dAlpha2KanUsual()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) - &
  &(0.75D0*CA2*CA3*EL*m12squared*SA2*dAlpha2KanUsual()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) + (0.0625D0*CA2*EL*MH12*SA3*dAlpha2Ka&
  &nUsual()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.03125D0*CA2*EL*MH22*SA3*dAlpha2KanUsual()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0&
  &.5625D0*CA2*EL*MH12*SA22*SA3*dAlpha2KanUsual()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0.28125D0*CA2*EL*MH22*SA22*SA3*dAlpha2KanUs&
  &ual()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0.09375D0*CA2*EL*m12squared*SA3*dAlpha2KanUsual()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*S&
  &W) + (0.84375D0*CA2*EL*m12squared*SA22*SA3*dAlpha2KanUsual()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.125D0*EL*MH12*SA3*dAlph&
  &a3KanUsual()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.125D0*CA22*EL*MH12*SA3*dAlpha3KanUsual()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) +&
  & (0.0625D0*EL*MH22*SA3*dAlpha3KanUsual()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.0625D0*CA22*EL*MH22*SA3*dAlpha3KanUsual()*DBLE(&
  &SA1**INT(3.D0)))/(CB*MW*SW) - (0.125D0*EL*MH12*SA22*SA3*dAlpha3KanUsual()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) - (0.0625D0*EL*MH22&
  &*SA22*SA3*dAlpha3KanUsual()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.0625D0*CA3*EL*MH12*SA2*dAlpha3KanUsual()*DBLE(SA1**INT(3.D0)&
  &))/(MW*SB*SW) + (0.1875D0*CA22*CA3*EL*MH12*SA2*dAlpha3KanUsual()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.03125D0*CA3*EL*MH22*SA2&
  &*dAlpha3KanUsual()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.09375D0*CA22*CA3*EL*MH22*SA2*dAlpha3KanUsual()*DBLE(SA1**INT(3.D0)))/&
  &(MW*SB*SW) - (0.1875D0*EL*m12squared*SA3*dAlpha3KanUsual()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.1875D0*CA22*EL*m12squared&
  &*SA3*dAlpha3KanUsual()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) + (0.1875D0*EL*m12squared*SA22*SA3*dAlpha3KanUsual()*DBLE(SA1**INT&
  &(3.D0)))/(CB2*MW*SB*SW) - (0.09375D0*CA3*EL*m12squared*SA2*dAlpha3KanUsual()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.28125D0&
  &*CA22*CA3*EL*m12squared*SA2*dAlpha3KanUsual()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.0625D0*CA3*EL*MH12*dBeta2KanUsual()*DB&
  &LE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.0625D0*CA22*CA3*EL*MH12*dBeta2KanUsual()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0.0625D0*CA3*&
  &EL*MH12*dBeta2KanUsual()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.0625D0*CA22*CA3*EL*MH12*dBeta2KanUsual()*DBLE(SA1**INT(3.D0&
  &)))/(CB2*MW*SB*SW) + (0.03125D0*CA3*EL*MH22*dBeta2KanUsual()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.03125D0*CA22*CA3*EL*MH22*dB&
  &eta2KanUsual()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0.03125D0*CA3*EL*MH22*dBeta2KanUsual()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW)&
  & - (0.03125D0*CA22*CA3*EL*MH22*dBeta2KanUsual()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.0625D0*CA3*EL*MH12*SA22*dBeta2KanUsu&
  &al()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.0625D0*CA3*EL*MH12*SA22*dBeta2KanUsual()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.&
  &03125D0*CA3*EL*MH22*SA22*dBeta2KanUsual()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.03125D0*CA3*EL*MH22*SA22*dBeta2KanUsual()*DBLE&
  &(SA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.09375D0*EL*m12squared*SA2*SA3*dBeta2KanUsual()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0&
  &.28125D0*CA22*EL*m12squared*SA2*SA3*dBeta2KanUsual()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.28125D0*CA3*EL*m12squared*dBeta&
  &2KanUsual()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.28125D0*CA22*CA3*EL*m12squared*dBeta2KanUsual()*DBLE(SA1**INT(3.D0)))/(C&
  &B*MW*SB2*SW) + (0.28125D0*CA3*EL*m12squared*SA22*dBeta2KanUsual()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.0625D0*EL*MH12*SA2&
  &*SA3*dBeta2KanUsual()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW*TB) - (0.1875D0*CA22*EL*MH12*SA2*SA3*dBeta2KanUsual()*DBLE(SA1**INT(3.D0&
  &)))/(MW*SB*SW*TB) - (0.03125D0*EL*MH22*SA2*SA3*dBeta2KanUsual()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW*TB) - (0.09375D0*CA22*EL*MH22*&
  &SA2*SA3*dBeta2KanUsual()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW*TB) - (0.0625D0*CA3*EL*MH12*TB*dBeta2KanUsual()*DBLE(SA1**INT(3.D0)))&
  &/(CB*MW*SW) - (0.0625D0*CA22*CA3*EL*MH12*TB*dBeta2KanUsual()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) - (0.03125D0*CA3*EL*MH22*TB*dBet&
  &a2KanUsual()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) - (0.03125D0*CA22*CA3*EL*MH22*TB*dBeta2KanUsual()*DBLE(SA1**INT(3.D0)))/(CB*MW*S&
  &W) + (0.0625D0*CA3*EL*MH12*SA22*TB*dBeta2KanUsual()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.03125D0*CA3*EL*MH22*SA22*TB*dBeta2Ka&
  &nUsual()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.1875D0*EL*MH12*SA3*dAlpha2KanUsual()*DBLE(CA2**INT(3.D0))*DBLE(SA1**INT(3.D0)))&
  &/(MW*SB*SW) + (0.09375D0*EL*MH22*SA3*dAlpha2KanUsual()*DBLE(CA2**INT(3.D0))*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0.28125D0*EL*m&
  &12squared*SA3*dAlpha2KanUsual()*DBLE(CA2**INT(3.D0))*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.28125D0*CA3*EL*m12squared*dBeta&
  &2KanUsual()*DBLE(CB**INT(-3.D0))*DBLE(SA1**INT(3.D0)))/(MW*SW) + (0.28125D0*CA22*CA3*EL*m12squared*dBeta2KanUsual()*DBLE(CB**I&
  &NT(-3.D0))*DBLE(SA1**INT(3.D0)))/(MW*SW) - (0.28125D0*CA3*EL*m12squared*SA22*dBeta2KanUsual()*DBLE(CB**INT(-3.D0))*DBLE(SA1**I&
  &NT(3.D0)))/(MW*SW) + (0.09375D0*CA3*EL*m12squared*dBeta2KanUsual()*DBLE(CB**INT(-3.D0))*DBLE(SA1**INT(3.D0)))/(MW*SB2*SW) + (0&
  &.09375D0*CA22*CA3*EL*m12squared*dBeta2KanUsual()*DBLE(CB**INT(-3.D0))*DBLE(SA1**INT(3.D0)))/(MW*SB2*SW) - (0.09375D0*CA3*EL*m1&
  &2squared*SA22*dBeta2KanUsual()*DBLE(CB**INT(-3.D0))*DBLE(SA1**INT(3.D0)))/(MW*SB2*SW) - (0.28125D0*CA1*EL*m12squared*SA3*dAlph&
  &a1KanUsual()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.1875D0*EL*MH12*SA1*SA3*dAlpha1KanUsual()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) +&
  & (0.5625D0*CA12*EL*MH12*SA1*SA3*dAlpha1KanUsual()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.09375D0*EL*MH22*SA1*SA3*dAlpha1KanUsua&
  &l()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.28125D0*CA12*EL*MH22*SA1*SA3*dAlpha1KanUsual()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0&
  &.1875D0*CA1*EL*MH12*SA3*dAlpha1KanUsual()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.09375D0*CA1*EL*MH22*SA3*dAlpha1KanUsual()*DBLE&
  &(SA2**INT(3.D0)))/(MW*SB*SW) + (0.28125D0*EL*m12squared*SA1*SA3*dAlpha1KanUsual()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.1875D0&
  &*EL*m12squared*SA1*SA3*dAlpha1KanUsual()*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) - (0.84375D0*CA12*EL*m12squared*SA1*SA3*dAlpha1K&
  &anUsual()*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) - (0.5625D0*CA1*EL*MH12*SA12*SA3*dAlpha1KanUsual()*DBLE(SA2**INT(3.D0)))/(MW*SB&
  &*SW) - (0.28125D0*CA1*EL*MH22*SA12*SA3*dAlpha1KanUsual()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) + (0.1875D0*CA1*EL*m12squared*SA3*dA&
  &lpha1KanUsual()*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) + (0.84375D0*CA1*EL*m12squared*SA12*SA3*dAlpha1KanUsual()*DBLE(SA2**INT(3&
  &.D0)))/(CB*MW*SB2*SW) + (0.09375D0*CA1*EL*m12squared*SA3*dAlpha1KanUsual()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) - (0.09375D0*EL&
  &*m12squared*SA1*SA3*TB*dAlpha1KanUsual()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (1.5D0*MH12*SA3*dAlpha2KanUsual()*DBLE(SA2**INT(3.&
  &D0)))/vS + (0.75D0*MH22*SA3*dAlpha2KanUsual()*DBLE(SA2**INT(3.D0)))/vS - (0.1875D0*CA1*CA3*EL*MH12*dAlpha3KanUsual()*DBLE(SA2*&
  &*INT(3.D0)))/(CB*MW*SW) - (0.09375D0*CA1*CA3*EL*MH22*dAlpha3KanUsual()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.28125D0*CA3*EL*m1&
  &2squared*SA1*dAlpha3KanUsual()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.1875D0*CA1*CA3*EL*MH12*SA12*dAlpha3KanUsual()*DBLE(SA2**I&
  &NT(3.D0)))/(CB*MW*SW) + (0.09375D0*CA1*CA3*EL*MH22*SA12*dAlpha3KanUsual()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.28125D0*CA1*CA&
  &3*EL*m12squared*dAlpha3KanUsual()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) + (0.1875D0*CA1*CA3*EL*m12squared*dAlpha3KanUsual()*DBLE(SA&
  &2**INT(3.D0)))/(CB2*MW*SB*SW) - (0.1875D0*CA3*EL*MH12*SA1*dAlpha3KanUsual()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) + (0.1875D0*CA12*&
  &CA3*EL*MH12*SA1*dAlpha3KanUsual()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.09375D0*CA3*EL*MH22*SA1*dAlpha3KanUsual()*DBLE(SA2**IN&
  &T(3.D0)))/(MW*SB*SW) + (0.09375D0*CA12*CA3*EL*MH22*SA1*dAlpha3KanUsual()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.28125D0*CA1*CA3&
  &*EL*m12squared*SA12*dAlpha3KanUsual()*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) + (0.1875D0*CA3*EL*m12squared*SA1*dAlpha3KanUsual()&
  &*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) - (0.28125D0*CA12*CA3*EL*m12squared*SA1*dAlpha3KanUsual()*DBLE(SA2**INT(3.D0)))/(CB*MW*S&
  &B2*SW) + (0.09375D0*CA3*EL*m12squared*SA1*dAlpha3KanUsual()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) + (0.09375D0*CA1*CA3*EL*m12squ&
  &ared*TB*dAlpha3KanUsual()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.09375D0*CA1*EL*m12squared*SA3*dBeta2KanUsual()*DBLE(SA2**INT(3&
  &.D0)))/(CB*MW*SW) + (0.09375D0*CA1*EL*MH12*SA3*dBeta2KanUsual()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.09375D0*CA1*EL*MH12*SA3*&
  &dBeta2KanUsual()*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) + (0.046875D0*CA1*EL*MH22*SA3*dBeta2KanUsual()*DBLE(SA2**INT(3.D0)))/(MW&
  &*SB*SW) - (0.046875D0*CA1*EL*MH22*SA3*dBeta2KanUsual()*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) + (0.140625D0*EL*m12squared*SA1*SA&
  &3*dBeta2KanUsual()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) + (0.046875D0*EL*m12squared*SA1*SA3*dBeta2KanUsual()*DBLE(SA2**INT(3.D0)))&
  &/(CB2*MW*SB*SW) - (0.28125D0*CA12*EL*m12squared*SA1*SA3*dBeta2KanUsual()*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) - (0.09375D0*CA1&
  &*EL*MH12*SA12*SA3*dBeta2KanUsual()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) + (0.09375D0*CA1*EL*MH12*SA12*SA3*dBeta2KanUsual()*DBLE(SA&
  &2**INT(3.D0)))/(CB2*MW*SB*SW) - (0.046875D0*CA1*EL*MH22*SA12*SA3*dBeta2KanUsual()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) + (0.046875&
  &D0*CA1*EL*MH22*SA12*SA3*dBeta2KanUsual()*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) - (0.10546875D0*CA1*EL*m12squared*SA3*dBeta2KanU&
  &sual()*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) + (0.38671875D0*CA1*EL*m12squared*SA12*SA3*dBeta2KanUsual()*DBLE(SA2**INT(3.D0)))/&
  &(CB*MW*SB2*SW) + (0.140625D0*CA1*EL*m12squared*SA3*dBeta2KanUsual()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) + (0.1875D0*EL*MH12*SA&
  &1*SA3*dBeta2KanUsual()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) - (0.1875D0*CA12*EL*MH12*SA1*SA3*dBeta2KanUsual()*DBLE(SA2**INT(3.D&
  &0)))/(MW*SB*SW*TB) + (0.09375D0*EL*MH22*SA1*SA3*dBeta2KanUsual()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) - (0.09375D0*CA12*EL*MH22&
  &*SA1*SA3*dBeta2KanUsual()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) - (0.09375D0*CA1*EL*MH12*SA3*TB*dBeta2KanUsual()*DBLE(SA2**INT(3&
  &.D0)))/(CB*MW*SW) - (0.046875D0*CA1*EL*MH22*SA3*TB*dBeta2KanUsual()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.140625D0*EL*m12squar&
  &ed*SA1*SA3*TB*dBeta2KanUsual()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.09375D0*CA1*EL*MH12*SA12*SA3*TB*dBeta2KanUsual()*DBLE(SA2&
  &**INT(3.D0)))/(CB*MW*SW) + (0.046875D0*CA1*EL*MH22*SA12*SA3*TB*dBeta2KanUsual()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.09375D0*&
  &EL*m12squared*SA1*SA3*dBeta2KanUsual()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB2) + (0.140625D0*CA1*EL*m12squared*SA3*TB2*dBeta2KanU&
  &sual()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.1875D0*EL*MH12*SA3*dAlpha1KanUsual()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(&
  &MW*SB*SW) + (0.09375D0*EL*MH22*SA3*dAlpha1KanUsual()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.28125D0*EL*m12&
  &squared*SA3*dAlpha1KanUsual()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) - (0.0625D0*CA3*EL*MH12*dAlpha3KanUsua&
  &l()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.03125D0*CA3*EL*MH22*dAlpha3KanUsual()*DBLE(CA1**INT(3.D0))*DBLE&
  &(SA2**INT(3.D0)))/(CB*MW*SW) + (0.09375D0*CA3*EL*m12squared*dAlpha3KanUsual()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB2*&
  &MW*SB*SW) + (0.03125D0*EL*MH12*SA3*dBeta2KanUsual()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.03125D0*EL*MH12&
  &*SA3*dBeta2KanUsual()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) + (0.015625D0*EL*MH22*SA3*dBeta2KanUsual()*DBL&
  &E(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.015625D0*EL*MH22*SA3*dBeta2KanUsual()*DBLE(CA1**INT(3.D0))*DBLE(SA2**I&
  &NT(3.D0)))/(CB2*MW*SB*SW) - (0.12890625D0*EL*m12squared*SA3*dBeta2KanUsual()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW&
  &*SB2*SW) - (0.03125D0*EL*MH12*SA3*TB*dBeta2KanUsual()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.015625D0*EL*M&
  &H22*SA3*TB*dBeta2KanUsual()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.36328125D0*CA1*EL*m12squared*SA3*dBeta2&
  &KanUsual()*DBLE(CB**INT(-3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SW) - (0.45703125D0*CA1*EL*m12squared*SA12*SA3*dBeta2KanUsual()*DBLE&
  &(CB**INT(-3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SW) + (0.05859375D0*CA1*EL*m12squared*SA3*dBeta2KanUsual()*DBLE(CB**INT(-3.D0))*DBL&
  &E(SA2**INT(3.D0)))/(MW*SB2*SW) - (0.10546875D0*CA1*EL*m12squared*SA12*SA3*dBeta2KanUsual()*DBLE(CB**INT(-3.D0))*DBLE(SA2**INT(&
  &3.D0)))/(MW*SB2*SW) + (0.15234375D0*EL*m12squared*SA3*dBeta2KanUsual()*DBLE(CA1**INT(3.D0))*DBLE(CB**INT(-3.D0))*DBLE(SA2**INT&
  &(3.D0)))/(MW*SW) + (0.03515625D0*EL*m12squared*SA3*dBeta2KanUsual()*DBLE(CA1**INT(3.D0))*DBLE(CB**INT(-3.D0))*DBLE(SA2**INT(3.&
  &D0)))/(MW*SB2*SW) - (0.1875D0*EL*MH12*SA3*dAlpha1KanUsual()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.09375D0&
  &*EL*MH22*SA3*dAlpha1KanUsual()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.28125D0*EL*m12squared*SA3*dAlpha1Kan&
  &Usual()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) - (0.0625D0*CA3*EL*MH12*dAlpha3KanUsual()*DBLE(SA1**INT(3.D0&
  &))*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.03125D0*CA3*EL*MH22*dAlpha3KanUsual()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(MW*&
  &SB*SW) + (0.09375D0*CA3*EL*m12squared*dAlpha3KanUsual()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) + (0.09375D0&
  &*EL*m12squared*SA3*dBeta2KanUsual()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) + (0.0625D0*EL*MH12*SA3*dBeta2Ka&
  &nUsual()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) + (0.03125D0*EL*MH22*SA3*dBeta2KanUsual()*DBLE(SA1**INT(3.D0&
  &))*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) - (0.1875D0*CA1*CA3*EL*m12squared*dBeta2KanUsual()*DBLE(SB**INT(-3.D0)))/(MW*SW) - (0.1&
  &875D0*CA1*CA22*CA3*EL*m12squared*dBeta2KanUsual()*DBLE(SB**INT(-3.D0)))/(MW*SW) - (1.125D0*CA1*CA3*EL*m12squared*SA12*dBeta2Ka&
  &nUsual()*DBLE(SB**INT(-3.D0)))/(MW*SW) - (1.125D0*CA1*CA22*CA3*EL*m12squared*SA12*dBeta2KanUsual()*DBLE(SB**INT(-3.D0)))/(MW*S&
  &W) + (0.1875D0*CA1*CA3*EL*m12squared*SA22*dBeta2KanUsual()*DBLE(SB**INT(-3.D0)))/(MW*SW) + (1.125D0*CA1*CA3*EL*m12squared*SA12&
  &*SA22*dBeta2KanUsual()*DBLE(SB**INT(-3.D0)))/(MW*SW) + (0.46875D0*EL*m12squared*SA1*SA2*SA3*dBeta2KanUsual()*DBLE(SB**INT(-3.D&
  &0)))/(MW*SW) - (0.5625D0*CA12*EL*m12squared*SA1*SA2*SA3*dBeta2KanUsual()*DBLE(SB**INT(-3.D0)))/(MW*SW) + (1.40625D0*CA22*EL*m1&
  &2squared*SA1*SA2*SA3*dBeta2KanUsual()*DBLE(SB**INT(-3.D0)))/(MW*SW) - (1.6875D0*CA12*CA22*EL*m12squared*SA1*SA2*SA3*dBeta2KanU&
  &sual()*DBLE(SB**INT(-3.D0)))/(MW*SW) + (0.375D0*CA3*EL*m12squared*dBeta2KanUsual()*DBLE(CA1**INT(3.D0))*DBLE(SB**INT(-3.D0)))/&
  &(MW*SW) + (0.375D0*CA22*CA3*EL*m12squared*dBeta2KanUsual()*DBLE(CA1**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(MW*SW) - (0.375D0*CA3*E&
  &L*m12squared*SA22*dBeta2KanUsual()*DBLE(CA1**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(MW*SW) + (0.1875D0*EL*m12squared*SA2*SA3*dBeta2&
  &KanUsual()*DBLE(SA1**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(MW*SW) + (0.5625D0*CA22*EL*m12squared*SA2*SA3*dBeta2KanUsual()*DBLE(SA1&
  &**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(MW*SW) - (0.46875D0*EL*m12squared*SA1*SA3*dBeta2KanUsual()*DBLE(SA2**INT(3.D0))*DBLE(SB**I&
  &NT(-3.D0)))/(MW*SW) + (0.5625D0*CA12*EL*m12squared*SA1*SA3*dBeta2KanUsual()*DBLE(SA2**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(MW*SW)&
  & - (0.1875D0*EL*m12squared*SA3*dBeta2KanUsual()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(MW*SW) - (0.1&
  &25D0*CA1*CA3*m12squared*dgAtMZ())/(CB*MW) - (0.125D0*CA1*CA22*CA3*m12squared*dgAtMZ())/(CB*MW) + (0.125D0*CA3*MH12*SA1*dgAtMZ(&
  &))/(CB*MW) + (0.375D0*CA12*CA3*MH12*SA1*dgAtMZ())/(CB*MW) + (0.125D0*CA22*CA3*MH12*SA1*dgAtMZ())/(CB*MW) + (0.375D0*CA12*CA22*&
  &CA3*MH12*SA1*dgAtMZ())/(CB*MW) + (0.0625D0*CA3*MH22*SA1*dgAtMZ())/(CB*MW) + (0.1875D0*CA12*CA3*MH22*SA1*dgAtMZ())/(CB*MW) + (0&
  &.0625D0*CA22*CA3*MH22*SA1*dgAtMZ())/(CB*MW) + (0.1875D0*CA12*CA22*CA3*MH22*SA1*dgAtMZ())/(CB*MW) + (0.125D0*CA1*CA3*m12squared&
  &*SA22*dgAtMZ())/(CB*MW) - (0.125D0*CA3*MH12*SA1*SA22*dgAtMZ())/(CB*MW) - (0.375D0*CA12*CA3*MH12*SA1*SA22*dgAtMZ())/(CB*MW) - (&
  &0.0625D0*CA3*MH22*SA1*SA22*dgAtMZ())/(CB*MW) - (0.1875D0*CA12*CA3*MH22*SA1*SA22*dgAtMZ())/(CB*MW) + (0.1875D0*CA1*MH12*SA2*SA3&
  &*dgAtMZ())/(CB*MW) + (0.5625D0*CA1*CA22*MH12*SA2*SA3*dgAtMZ())/(CB*MW) + (0.09375D0*CA1*MH22*SA2*SA3*dgAtMZ())/(CB*MW) + (0.28&
  &125D0*CA1*CA22*MH22*SA2*SA3*dgAtMZ())/(CB*MW) + (0.28125D0*m12squared*SA1*SA2*SA3*dgAtMZ())/(CB*MW) + (0.84375D0*CA22*m12squar&
  &ed*SA1*SA2*SA3*dgAtMZ())/(CB*MW) - (0.1875D0*CA1*MH12*SA12*SA2*SA3*dgAtMZ())/(CB*MW) - (0.5625D0*CA1*CA22*MH12*SA12*SA2*SA3*dg&
  &AtMZ())/(CB*MW) - (0.09375D0*CA1*MH22*SA12*SA2*SA3*dgAtMZ())/(CB*MW) - (0.28125D0*CA1*CA22*MH22*SA12*SA2*SA3*dgAtMZ())/(CB*MW)&
  & - (0.125D0*CA1*CA3*MH12*dgAtMZ())/(MW*SB) - (0.125D0*CA1*CA22*CA3*MH12*dgAtMZ())/(MW*SB) - (0.0625D0*CA1*CA3*MH22*dgAtMZ())/(&
  &MW*SB) - (0.0625D0*CA1*CA22*CA3*MH22*dgAtMZ())/(MW*SB) + (0.21875D0*CA3*m12squared*SA1*dgAtMZ())/(MW*SB) + (0.21875D0*CA22*CA3&
  &*m12squared*SA1*dgAtMZ())/(MW*SB) - (0.15625D0*CA3*m12squared*SA1*dgAtMZ())/(CB2*MW*SB) - (0.5625D0*CA12*CA3*m12squared*SA1*dg&
  &AtMZ())/(CB2*MW*SB) - (0.15625D0*CA22*CA3*m12squared*SA1*dgAtMZ())/(CB2*MW*SB) - (0.5625D0*CA12*CA22*CA3*m12squared*SA1*dgAtMZ&
  &())/(CB2*MW*SB) - (0.375D0*CA1*CA3*MH12*SA12*dgAtMZ())/(MW*SB) - (0.375D0*CA1*CA22*CA3*MH12*SA12*dgAtMZ())/(MW*SB) - (0.1875D0&
  &*CA1*CA3*MH22*SA12*dgAtMZ())/(MW*SB) - (0.1875D0*CA1*CA22*CA3*MH22*SA12*dgAtMZ())/(MW*SB) + (0.125D0*CA1*CA3*MH12*SA22*dgAtMZ(&
  &))/(MW*SB) + (0.0625D0*CA1*CA3*MH22*SA22*dgAtMZ())/(MW*SB) - (0.21875D0*CA3*m12squared*SA1*SA22*dgAtMZ())/(MW*SB) + (0.15625D0&
  &*CA3*m12squared*SA1*SA22*dgAtMZ())/(CB2*MW*SB) + (0.5625D0*CA12*CA3*m12squared*SA1*SA22*dgAtMZ())/(CB2*MW*SB) + (0.375D0*CA1*C&
  &A3*MH12*SA12*SA22*dgAtMZ())/(MW*SB) + (0.1875D0*CA1*CA3*MH22*SA12*SA22*dgAtMZ())/(MW*SB) + (0.28125D0*CA1*m12squared*SA2*SA3*d&
  &gAtMZ())/(MW*SB) + (0.84375D0*CA1*CA22*m12squared*SA2*SA3*dgAtMZ())/(MW*SB) - (0.1875D0*CA1*m12squared*SA2*SA3*dgAtMZ())/(CB2*&
  &MW*SB) - (0.5625D0*CA1*CA22*m12squared*SA2*SA3*dgAtMZ())/(CB2*MW*SB) + (0.1875D0*MH12*SA1*SA2*SA3*dgAtMZ())/(MW*SB) - (0.1875D&
  &0*CA12*MH12*SA1*SA2*SA3*dgAtMZ())/(MW*SB) + (0.5625D0*CA22*MH12*SA1*SA2*SA3*dgAtMZ())/(MW*SB) - (0.5625D0*CA12*CA22*MH12*SA1*S&
  &A2*SA3*dgAtMZ())/(MW*SB) + (0.09375D0*MH22*SA1*SA2*SA3*dgAtMZ())/(MW*SB) - (0.09375D0*CA12*MH22*SA1*SA2*SA3*dgAtMZ())/(MW*SB) &
  &+ (0.28125D0*CA22*MH22*SA1*SA2*SA3*dgAtMZ())/(MW*SB) - (0.28125D0*CA12*CA22*MH22*SA1*SA2*SA3*dgAtMZ())/(MW*SB) + (0.28125D0*CA&
  &1*m12squared*SA12*SA2*SA3*dgAtMZ())/(CB2*MW*SB) + (0.84375D0*CA1*CA22*m12squared*SA12*SA2*SA3*dgAtMZ())/(CB2*MW*SB) + (0.0625D&
  &0*CA1*CA3*m12squared*dgAtMZ())/(CB*MW*SB2) + (0.0625D0*CA1*CA22*CA3*m12squared*dgAtMZ())/(CB*MW*SB2) + (0.5625D0*CA1*CA3*m12sq&
  &uared*SA12*dgAtMZ())/(CB*MW*SB2) + (0.5625D0*CA1*CA22*CA3*m12squared*SA12*dgAtMZ())/(CB*MW*SB2) - (0.0625D0*CA1*CA3*m12squared&
  &*SA22*dgAtMZ())/(CB*MW*SB2) - (0.5625D0*CA1*CA3*m12squared*SA12*SA22*dgAtMZ())/(CB*MW*SB2) - (0.1875D0*m12squared*SA1*SA2*SA3*&
  &dgAtMZ())/(CB*MW*SB2) + (0.28125D0*CA12*m12squared*SA1*SA2*SA3*dgAtMZ())/(CB*MW*SB2) - (0.5625D0*CA22*m12squared*SA1*SA2*SA3*d&
  &gAtMZ())/(CB*MW*SB2) + (0.84375D0*CA12*CA22*m12squared*SA1*SA2*SA3*dgAtMZ())/(CB*MW*SB2) + (0.125D0*CA1*CA3*m12squared*dgAtMZ(&
  &))/(MW*SB*TB) + (0.125D0*CA1*CA22*CA3*m12squared*dgAtMZ())/(MW*SB*TB) - (0.125D0*CA1*CA3*m12squared*SA22*dgAtMZ())/(MW*SB*TB) &
  &- (0.09375D0*m12squared*SA1*SA2*SA3*dgAtMZ())/(MW*SB*TB) - (0.28125D0*CA22*m12squared*SA1*SA2*SA3*dgAtMZ())/(MW*SB*TB) - (0.03&
  &125D0*CA3*m12squared*SA1*TB*dgAtMZ())/(CB*MW) - (0.03125D0*CA22*CA3*m12squared*SA1*TB*dgAtMZ())/(CB*MW) + (0.03125D0*CA3*m12sq&
  &uared*SA1*SA22*TB*dgAtMZ())/(CB*MW) - (0.09375D0*CA1*m12squared*SA2*SA3*TB*dgAtMZ())/(CB*MW) - (0.28125D0*CA1*CA22*m12squared*&
  &SA2*SA3*TB*dgAtMZ())/(CB*MW) + (0.0625D0*MH12*SA2*SA3*DBLE(CA1**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.1875D0*CA22*MH12*SA2*SA3*DBL&
  &E(CA1**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.03125D0*MH22*SA2*SA3*DBLE(CA1**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.09375D0*CA22*MH22*SA&
  &2*SA3*DBLE(CA1**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.125D0*CA3*MH12*DBLE(CA1**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.125D0*CA22*CA3*MH&
  &12*DBLE(CA1**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.0625D0*CA3*MH22*DBLE(CA1**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.0625D0*CA22*CA3*MH2&
  &2*DBLE(CA1**INT(3.D0))*dgAtMZ())/(MW*SB) - (0.125D0*CA3*MH12*SA22*DBLE(CA1**INT(3.D0))*dgAtMZ())/(MW*SB) - (0.0625D0*CA3*MH22*&
  &SA22*DBLE(CA1**INT(3.D0))*dgAtMZ())/(MW*SB) - (0.09375D0*m12squared*SA2*SA3*DBLE(CA1**INT(3.D0))*dgAtMZ())/(CB2*MW*SB) - (0.28&
  &125D0*CA22*m12squared*SA2*SA3*DBLE(CA1**INT(3.D0))*dgAtMZ())/(CB2*MW*SB) - (0.1875D0*CA3*m12squared*DBLE(CA1**INT(3.D0))*dgAtM&
  &Z())/(CB*MW*SB2) - (0.1875D0*CA22*CA3*m12squared*DBLE(CA1**INT(3.D0))*dgAtMZ())/(CB*MW*SB2) + (0.1875D0*CA3*m12squared*SA22*DB&
  &LE(CA1**INT(3.D0))*dgAtMZ())/(CB*MW*SB2) - (0.125D0*CA3*MH12*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB*MW) - (0.125D0*CA22*CA3*MH12*D&
  &BLE(SA1**INT(3.D0))*dgAtMZ())/(CB*MW) - (0.0625D0*CA3*MH22*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB*MW) - (0.0625D0*CA22*CA3*MH22*DB&
  &LE(SA1**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.125D0*CA3*MH12*SA22*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.0625D0*CA3*MH22*SA22&
  &*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.1875D0*CA3*m12squared*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB2*MW*SB) + (0.1875D0*CA22&
  &*CA3*m12squared*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB2*MW*SB) - (0.1875D0*CA3*m12squared*SA22*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB2&
  &*MW*SB) + (0.0625D0*MH12*SA2*SA3*DBLE(SA1**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.1875D0*CA22*MH12*SA2*SA3*DBLE(SA1**INT(3.D0))*dgA&
  &tMZ())/(MW*SB) + (0.03125D0*MH22*SA2*SA3*DBLE(SA1**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.09375D0*CA22*MH22*SA2*SA3*DBLE(SA1**INT(3&
  &.D0))*dgAtMZ())/(MW*SB) - (0.09375D0*m12squared*SA2*SA3*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB*MW*SB2) - (0.28125D0*CA22*m12square&
  &d*SA2*SA3*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB*MW*SB2) - (0.1875D0*CA1*MH12*SA3*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW) - (0.0937&
  &5D0*CA1*MH22*SA3*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW) - (0.28125D0*m12squared*SA1*SA3*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW)&
  & + (0.1875D0*CA1*MH12*SA12*SA3*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.09375D0*CA1*MH22*SA12*SA3*DBLE(SA2**INT(3.D0))*dgAt&
  &MZ())/(CB*MW) - (0.28125D0*CA1*m12squared*SA3*DBLE(SA2**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.1875D0*CA1*m12squared*SA3*DBLE(SA2**&
  &INT(3.D0))*dgAtMZ())/(CB2*MW*SB) - (0.1875D0*MH12*SA1*SA3*DBLE(SA2**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.1875D0*CA12*MH12*SA1*SA3&
  &*DBLE(SA2**INT(3.D0))*dgAtMZ())/(MW*SB) - (0.09375D0*MH22*SA1*SA3*DBLE(SA2**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.09375D0*CA12*MH2&
  &2*SA1*SA3*DBLE(SA2**INT(3.D0))*dgAtMZ())/(MW*SB) - (0.28125D0*CA1*m12squared*SA12*SA3*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB2*MW*S&
  &B) + (0.1875D0*m12squared*SA1*SA3*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW*SB2) - (0.28125D0*CA12*m12squared*SA1*SA3*DBLE(SA2**IN&
  &T(3.D0))*dgAtMZ())/(CB*MW*SB2) + (0.09375D0*m12squared*SA1*SA3*DBLE(SA2**INT(3.D0))*dgAtMZ())/(MW*SB*TB) + (0.09375D0*CA1*m12s&
  &quared*SA3*TB*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW) - (0.0625D0*MH12*SA3*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dgAtMZ())/&
  &(CB*MW) - (0.03125D0*MH22*SA3*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.09375D0*m12squared*SA3*DBLE(CA1&
  &**INT(3.D0))*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB2*MW*SB) - (0.0625D0*MH12*SA3*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dgAtMZ(&
  &))/(MW*SB) - (0.03125D0*MH22*SA3*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.09375D0*m12squared*SA3*DBLE(&
  &SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW*SB2) - (0.125D0*CA1*CA3*EL*dm122MSBarUsual())/(CB*MW*SW) - (0.125D0*CA1*&
  &CA22*CA3*EL*dm122MSBarUsual())/(CB*MW*SW) + (0.125D0*CA1*CA3*EL*SA22*dm122MSBarUsual())/(CB*MW*SW) + (0.28125D0*EL*SA1*SA2*SA3&
  &*dm122MSBarUsual())/(CB*MW*SW) + (0.84375D0*CA22*EL*SA1*SA2*SA3*dm122MSBarUsual())/(CB*MW*SW) + (0.21875D0*CA3*EL*SA1*dm122MSB&
  &arUsual())/(MW*SB*SW) + (0.21875D0*CA22*CA3*EL*SA1*dm122MSBarUsual())/(MW*SB*SW) - (0.15625D0*CA3*EL*SA1*dm122MSBarUsual())/(C&
  &B2*MW*SB*SW) - (0.5625D0*CA12*CA3*EL*SA1*dm122MSBarUsual())/(CB2*MW*SB*SW) - (0.15625D0*CA22*CA3*EL*SA1*dm122MSBarUsual())/(CB&
  &2*MW*SB*SW) - (0.5625D0*CA12*CA22*CA3*EL*SA1*dm122MSBarUsual())/(CB2*MW*SB*SW) - (0.21875D0*CA3*EL*SA1*SA22*dm122MSBarUsual())&
  &/(MW*SB*SW) + (0.15625D0*CA3*EL*SA1*SA22*dm122MSBarUsual())/(CB2*MW*SB*SW) + (0.5625D0*CA12*CA3*EL*SA1*SA22*dm122MSBarUsual())&
  &/(CB2*MW*SB*SW) + (0.28125D0*CA1*EL*SA2*SA3*dm122MSBarUsual())/(MW*SB*SW) + (0.84375D0*CA1*CA22*EL*SA2*SA3*dm122MSBarUsual())/&
  &(MW*SB*SW) - (0.1875D0*CA1*EL*SA2*SA3*dm122MSBarUsual())/(CB2*MW*SB*SW) - (0.5625D0*CA1*CA22*EL*SA2*SA3*dm122MSBarUsual())/(CB&
  &2*MW*SB*SW) + (0.28125D0*CA1*EL*SA12*SA2*SA3*dm122MSBarUsual())/(CB2*MW*SB*SW) + (0.84375D0*CA1*CA22*EL*SA12*SA2*SA3*dm122MSBa&
  &rUsual())/(CB2*MW*SB*SW) + (0.0625D0*CA1*CA3*EL*dm122MSBarUsual())/(CB*MW*SB2*SW) + (0.0625D0*CA1*CA22*CA3*EL*dm122MSBarUsual(&
  &))/(CB*MW*SB2*SW) + (0.5625D0*CA1*CA3*EL*SA12*dm122MSBarUsual())/(CB*MW*SB2*SW) + (0.5625D0*CA1*CA22*CA3*EL*SA12*dm122MSBarUsu&
  &al())/(CB*MW*SB2*SW) - (0.0625D0*CA1*CA3*EL*SA22*dm122MSBarUsual())/(CB*MW*SB2*SW) - (0.5625D0*CA1*CA3*EL*SA12*SA22*dm122MSBar&
  &Usual())/(CB*MW*SB2*SW) - (0.1875D0*EL*SA1*SA2*SA3*dm122MSBarUsual())/(CB*MW*SB2*SW) + (0.28125D0*CA12*EL*SA1*SA2*SA3*dm122MSB&
  &arUsual())/(CB*MW*SB2*SW) - (0.5625D0*CA22*EL*SA1*SA2*SA3*dm122MSBarUsual())/(CB*MW*SB2*SW) + (0.84375D0*CA12*CA22*EL*SA1*SA2*&
  &SA3*dm122MSBarUsual())/(CB*MW*SB2*SW) + (0.125D0*CA1*CA3*EL*dm122MSBarUsual())/(MW*SB*SW*TB) + (0.125D0*CA1*CA22*CA3*EL*dm122M&
  &SBarUsual())/(MW*SB*SW*TB) - (0.125D0*CA1*CA3*EL*SA22*dm122MSBarUsual())/(MW*SB*SW*TB) - (0.09375D0*EL*SA1*SA2*SA3*dm122MSBarU&
  &sual())/(MW*SB*SW*TB) - (0.28125D0*CA22*EL*SA1*SA2*SA3*dm122MSBarUsual())/(MW*SB*SW*TB) - (0.03125D0*CA3*EL*SA1*TB*dm122MSBarU&
  &sual())/(CB*MW*SW) - (0.03125D0*CA22*CA3*EL*SA1*TB*dm122MSBarUsual())/(CB*MW*SW) + (0.03125D0*CA3*EL*SA1*SA22*TB*dm122MSBarUsu&
  &al())/(CB*MW*SW) - (0.09375D0*CA1*EL*SA2*SA3*TB*dm122MSBarUsual())/(CB*MW*SW) - (0.28125D0*CA1*CA22*EL*SA2*SA3*TB*dm122MSBarUs&
  &ual())/(CB*MW*SW) - (0.09375D0*EL*SA2*SA3*DBLE(CA1**INT(3.D0))*dm122MSBarUsual())/(CB2*MW*SB*SW) - (0.28125D0*CA22*EL*SA2*SA3*&
  &DBLE(CA1**INT(3.D0))*dm122MSBarUsual())/(CB2*MW*SB*SW) - (0.1875D0*CA3*EL*DBLE(CA1**INT(3.D0))*dm122MSBarUsual())/(CB*MW*SB2*S&
  &W) - (0.1875D0*CA22*CA3*EL*DBLE(CA1**INT(3.D0))*dm122MSBarUsual())/(CB*MW*SB2*SW) + (0.1875D0*CA3*EL*SA22*DBLE(CA1**INT(3.D0))&
  &*dm122MSBarUsual())/(CB*MW*SB2*SW) + (0.1875D0*CA3*EL*DBLE(SA1**INT(3.D0))*dm122MSBarUsual())/(CB2*MW*SB*SW) + (0.1875D0*CA22*&
  &CA3*EL*DBLE(SA1**INT(3.D0))*dm122MSBarUsual())/(CB2*MW*SB*SW) - (0.1875D0*CA3*EL*SA22*DBLE(SA1**INT(3.D0))*dm122MSBarUsual())/&
  &(CB2*MW*SB*SW) - (0.09375D0*EL*SA2*SA3*DBLE(SA1**INT(3.D0))*dm122MSBarUsual())/(CB*MW*SB2*SW) - (0.28125D0*CA22*EL*SA2*SA3*DBL&
  &E(SA1**INT(3.D0))*dm122MSBarUsual())/(CB*MW*SB2*SW) - (0.28125D0*EL*SA1*SA3*DBLE(SA2**INT(3.D0))*dm122MSBarUsual())/(CB*MW*SW)&
  & - (0.28125D0*CA1*EL*SA3*DBLE(SA2**INT(3.D0))*dm122MSBarUsual())/(MW*SB*SW) + (0.1875D0*CA1*EL*SA3*DBLE(SA2**INT(3.D0))*dm122M&
  &SBarUsual())/(CB2*MW*SB*SW) - (0.28125D0*CA1*EL*SA12*SA3*DBLE(SA2**INT(3.D0))*dm122MSBarUsual())/(CB2*MW*SB*SW) + (0.1875D0*EL&
  &*SA1*SA3*DBLE(SA2**INT(3.D0))*dm122MSBarUsual())/(CB*MW*SB2*SW) - (0.28125D0*CA12*EL*SA1*SA3*DBLE(SA2**INT(3.D0))*dm122MSBarUs&
  &ual())/(CB*MW*SB2*SW) + (0.09375D0*EL*SA1*SA3*DBLE(SA2**INT(3.D0))*dm122MSBarUsual())/(MW*SB*SW*TB) + (0.09375D0*CA1*EL*SA3*TB&
  &*DBLE(SA2**INT(3.D0))*dm122MSBarUsual())/(CB*MW*SW) + (0.09375D0*EL*SA3*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dm122MSBarUs&
  &ual())/(CB2*MW*SB*SW) + (0.09375D0*EL*SA3*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dm122MSBarUsual())/(CB*MW*SB2*SW) + (0.125&
  &D0*CA3*EL*SA1*dMH12OSUsual())/(CB*MW*SW) + (0.375D0*CA12*CA3*EL*SA1*dMH12OSUsual())/(CB*MW*SW) + (0.125D0*CA22*CA3*EL*SA1*dMH1&
  &2OSUsual())/(CB*MW*SW) + (0.375D0*CA12*CA22*CA3*EL*SA1*dMH12OSUsual())/(CB*MW*SW) - (0.125D0*CA3*EL*SA1*SA22*dMH12OSUsual())/(&
  &CB*MW*SW) - (0.375D0*CA12*CA3*EL*SA1*SA22*dMH12OSUsual())/(CB*MW*SW) + (0.1875D0*CA1*EL*SA2*SA3*dMH12OSUsual())/(CB*MW*SW) + (&
  &0.5625D0*CA1*CA22*EL*SA2*SA3*dMH12OSUsual())/(CB*MW*SW) - (0.1875D0*CA1*EL*SA12*SA2*SA3*dMH12OSUsual())/(CB*MW*SW) - (0.5625D0&
  &*CA1*CA22*EL*SA12*SA2*SA3*dMH12OSUsual())/(CB*MW*SW) - (0.125D0*CA1*CA3*EL*dMH12OSUsual())/(MW*SB*SW) - (0.125D0*CA1*CA22*CA3*&
  &EL*dMH12OSUsual())/(MW*SB*SW) - (0.375D0*CA1*CA3*EL*SA12*dMH12OSUsual())/(MW*SB*SW) - (0.375D0*CA1*CA22*CA3*EL*SA12*dMH12OSUsu&
  &al())/(MW*SB*SW) + (0.125D0*CA1*CA3*EL*SA22*dMH12OSUsual())/(MW*SB*SW) + (0.375D0*CA1*CA3*EL*SA12*SA22*dMH12OSUsual())/(MW*SB*&
  &SW) + (0.1875D0*EL*SA1*SA2*SA3*dMH12OSUsual())/(MW*SB*SW) - (0.1875D0*CA12*EL*SA1*SA2*SA3*dMH12OSUsual())/(MW*SB*SW) + (0.5625&
  &D0*CA22*EL*SA1*SA2*SA3*dMH12OSUsual())/(MW*SB*SW) - (0.5625D0*CA12*CA22*EL*SA1*SA2*SA3*dMH12OSUsual())/(MW*SB*SW) - (0.5D0*CA2&
  &*SA3*dMH12OSUsual())/vS - (1.5D0*CA2*SA22*SA3*dMH12OSUsual())/vS + (0.0625D0*EL*SA2*SA3*DBLE(CA1**INT(3.D0))*dMH12OSUsual())/(&
  &CB*MW*SW) + (0.1875D0*CA22*EL*SA2*SA3*DBLE(CA1**INT(3.D0))*dMH12OSUsual())/(CB*MW*SW) + (0.125D0*CA3*EL*DBLE(CA1**INT(3.D0))*d&
  &MH12OSUsual())/(MW*SB*SW) + (0.125D0*CA22*CA3*EL*DBLE(CA1**INT(3.D0))*dMH12OSUsual())/(MW*SB*SW) - (0.125D0*CA3*EL*SA22*DBLE(C&
  &A1**INT(3.D0))*dMH12OSUsual())/(MW*SB*SW) + (0.5D0*SA3*DBLE(CA2**INT(3.D0))*dMH12OSUsual())/vS - (0.125D0*CA3*EL*DBLE(SA1**INT&
  &(3.D0))*dMH12OSUsual())/(CB*MW*SW) - (0.125D0*CA22*CA3*EL*DBLE(SA1**INT(3.D0))*dMH12OSUsual())/(CB*MW*SW) + (0.125D0*CA3*EL*SA&
  &22*DBLE(SA1**INT(3.D0))*dMH12OSUsual())/(CB*MW*SW) + (0.0625D0*EL*SA2*SA3*DBLE(SA1**INT(3.D0))*dMH12OSUsual())/(MW*SB*SW) + (0&
  &.1875D0*CA22*EL*SA2*SA3*DBLE(SA1**INT(3.D0))*dMH12OSUsual())/(MW*SB*SW) - (0.1875D0*CA1*EL*SA3*DBLE(SA2**INT(3.D0))*dMH12OSUsu&
  &al())/(CB*MW*SW) + (0.1875D0*CA1*EL*SA12*SA3*DBLE(SA2**INT(3.D0))*dMH12OSUsual())/(CB*MW*SW) - (0.1875D0*EL*SA1*SA3*DBLE(SA2**&
  &INT(3.D0))*dMH12OSUsual())/(MW*SB*SW) + (0.1875D0*CA12*EL*SA1*SA3*DBLE(SA2**INT(3.D0))*dMH12OSUsual())/(MW*SB*SW) - (0.0625D0*&
  &EL*SA3*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dMH12OSUsual())/(CB*MW*SW) - (0.0625D0*EL*SA3*DBLE(SA1**INT(3.D0))*DBLE(SA2**&
  &INT(3.D0))*dMH12OSUsual())/(MW*SB*SW) + (0.0625D0*CA3*EL*SA1*dMH22OSUsual())/(CB*MW*SW) + (0.1875D0*CA12*CA3*EL*SA1*dMH22OSUsu&
  &al())/(CB*MW*SW) + (0.0625D0*CA22*CA3*EL*SA1*dMH22OSUsual())/(CB*MW*SW) + (0.1875D0*CA12*CA22*CA3*EL*SA1*dMH22OSUsual())/(CB*M&
  &W*SW) - (0.0625D0*CA3*EL*SA1*SA22*dMH22OSUsual())/(CB*MW*SW) - (0.1875D0*CA12*CA3*EL*SA1*SA22*dMH22OSUsual())/(CB*MW*SW) + (0.&
  &09375D0*CA1*EL*SA2*SA3*dMH22OSUsual())/(CB*MW*SW) + (0.28125D0*CA1*CA22*EL*SA2*SA3*dMH22OSUsual())/(CB*MW*SW) - (0.09375D0*CA1&
  &*EL*SA12*SA2*SA3*dMH22OSUsual())/(CB*MW*SW) - (0.28125D0*CA1*CA22*EL*SA12*SA2*SA3*dMH22OSUsual())/(CB*MW*SW) - (0.0625D0*CA1*C&
  &A3*EL*dMH22OSUsual())/(MW*SB*SW) - (0.0625D0*CA1*CA22*CA3*EL*dMH22OSUsual())/(MW*SB*SW) - (0.1875D0*CA1*CA3*EL*SA12*dMH22OSUsu&
  &al())/(MW*SB*SW) - (0.1875D0*CA1*CA22*CA3*EL*SA12*dMH22OSUsual())/(MW*SB*SW) + (0.0625D0*CA1*CA3*EL*SA22*dMH22OSUsual())/(MW*S&
  &B*SW) + (0.1875D0*CA1*CA3*EL*SA12*SA22*dMH22OSUsual())/(MW*SB*SW) + (0.09375D0*EL*SA1*SA2*SA3*dMH22OSUsual())/(MW*SB*SW) - (0.&
  &09375D0*CA12*EL*SA1*SA2*SA3*dMH22OSUsual())/(MW*SB*SW) + (0.28125D0*CA22*EL*SA1*SA2*SA3*dMH22OSUsual())/(MW*SB*SW) - (0.28125D&
  &0*CA12*CA22*EL*SA1*SA2*SA3*dMH22OSUsual())/(MW*SB*SW) - (0.25D0*CA2*SA3*dMH22OSUsual())/vS - (0.75D0*CA2*SA22*SA3*dMH22OSUsual&
  &())/vS + (0.03125D0*EL*SA2*SA3*DBLE(CA1**INT(3.D0))*dMH22OSUsual())/(CB*MW*SW) + (0.09375D0*CA22*EL*SA2*SA3*DBLE(CA1**INT(3.D0&
  &))*dMH22OSUsual())/(CB*MW*SW) + (0.0625D0*CA3*EL*DBLE(CA1**INT(3.D0))*dMH22OSUsual())/(MW*SB*SW) + (0.0625D0*CA22*CA3*EL*DBLE(&
  &CA1**INT(3.D0))*dMH22OSUsual())/(MW*SB*SW) - (0.0625D0*CA3*EL*SA22*DBLE(CA1**INT(3.D0))*dMH22OSUsual())/(MW*SB*SW) + (0.25D0*S&
  &A3*DBLE(CA2**INT(3.D0))*dMH22OSUsual())/vS - (0.0625D0*CA3*EL*DBLE(SA1**INT(3.D0))*dMH22OSUsual())/(CB*MW*SW) - (0.0625D0*CA22&
  &*CA3*EL*DBLE(SA1**INT(3.D0))*dMH22OSUsual())/(CB*MW*SW) + (0.0625D0*CA3*EL*SA22*DBLE(SA1**INT(3.D0))*dMH22OSUsual())/(CB*MW*SW&
  &) + (0.03125D0*EL*SA2*SA3*DBLE(SA1**INT(3.D0))*dMH22OSUsual())/(MW*SB*SW) + (0.09375D0*CA22*EL*SA2*SA3*DBLE(SA1**INT(3.D0))*dM&
  &H22OSUsual())/(MW*SB*SW) - (0.09375D0*CA1*EL*SA3*DBLE(SA2**INT(3.D0))*dMH22OSUsual())/(CB*MW*SW) + (0.09375D0*CA1*EL*SA12*SA3*&
  &DBLE(SA2**INT(3.D0))*dMH22OSUsual())/(CB*MW*SW) - (0.09375D0*EL*SA1*SA3*DBLE(SA2**INT(3.D0))*dMH22OSUsual())/(MW*SB*SW) + (0.0&
  &9375D0*CA12*EL*SA1*SA3*DBLE(SA2**INT(3.D0))*dMH22OSUsual())/(MW*SB*SW) - (0.03125D0*EL*SA3*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(&
  &3.D0))*dMH22OSUsual())/(CB*MW*SW) - (0.03125D0*EL*SA3*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dMH22OSUsual())/(MW*SB*SW) + (&
  &0.0625D0*CA1*CA3*EL*m12squared*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SW) + (0.0625D0*CA1*CA22*CA3*EL*m12squared*DBLE(MW**INT(-&
  &3.D0))*dMW2Usual())/(CB*SW) - (0.0625D0*CA3*EL*MH12*SA1*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SW) - (0.1875D0*CA12*CA3*EL*MH12&
  &*SA1*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SW) - (0.0625D0*CA22*CA3*EL*MH12*SA1*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SW) - (0&
  &.1875D0*CA12*CA22*CA3*EL*MH12*SA1*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SW) - (0.03125D0*CA3*EL*MH22*SA1*DBLE(MW**INT(-3.D0))*&
  &dMW2Usual())/(CB*SW) - (0.09375D0*CA12*CA3*EL*MH22*SA1*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SW) - (0.03125D0*CA22*CA3*EL*MH22&
  &*SA1*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SW) - (0.09375D0*CA12*CA22*CA3*EL*MH22*SA1*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SW&
  &) - (0.0625D0*CA1*CA3*EL*m12squared*SA22*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SW) + (0.0625D0*CA3*EL*MH12*SA1*SA22*DBLE(MW**I&
  &NT(-3.D0))*dMW2Usual())/(CB*SW) + (0.1875D0*CA12*CA3*EL*MH12*SA1*SA22*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SW) + (0.03125D0*C&
  &A3*EL*MH22*SA1*SA22*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SW) + (0.09375D0*CA12*CA3*EL*MH22*SA1*SA22*DBLE(MW**INT(-3.D0))*dMW2&
  &Usual())/(CB*SW) - (0.09375D0*CA1*EL*MH12*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SW) - (0.28125D0*CA1*CA22*EL*MH12*SA2*&
  &SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SW) - (0.046875D0*CA1*EL*MH22*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SW) - (0&
  &.140625D0*CA1*CA22*EL*MH22*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SW) - (0.140625D0*EL*m12squared*SA1*SA2*SA3*DBLE(MW**&
  &INT(-3.D0))*dMW2Usual())/(CB*SW) - (0.421875D0*CA22*EL*m12squared*SA1*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SW) + (0.0&
  &9375D0*CA1*EL*MH12*SA12*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SW) + (0.28125D0*CA1*CA22*EL*MH12*SA12*SA2*SA3*DBLE(MW**&
  &INT(-3.D0))*dMW2Usual())/(CB*SW) + (0.046875D0*CA1*EL*MH22*SA12*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SW) + (0.140625D&
  &0*CA1*CA22*EL*MH22*SA12*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SW) + (0.0625D0*CA1*CA3*EL*MH12*DBLE(MW**INT(-3.D0))*dMW&
  &2Usual())/(SB*SW) + (0.0625D0*CA1*CA22*CA3*EL*MH12*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW) + (0.03125D0*CA1*CA3*EL*MH22*DBLE&
  &(MW**INT(-3.D0))*dMW2Usual())/(SB*SW) + (0.03125D0*CA1*CA22*CA3*EL*MH22*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW) - (0.109375D&
  &0*CA3*EL*m12squared*SA1*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW) - (0.109375D0*CA22*CA3*EL*m12squared*SA1*DBLE(MW**INT(-3.D0)&
  &)*dMW2Usual())/(SB*SW) + (0.078125D0*CA3*EL*m12squared*SA1*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB2*SB*SW) + (0.28125D0*CA12*CA3&
  &*EL*m12squared*SA1*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB2*SB*SW) + (0.078125D0*CA22*CA3*EL*m12squared*SA1*DBLE(MW**INT(-3.D0))&
  &*dMW2Usual())/(CB2*SB*SW) + (0.28125D0*CA12*CA22*CA3*EL*m12squared*SA1*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB2*SB*SW) + (0.1875&
  &D0*CA1*CA3*EL*MH12*SA12*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW) + (0.1875D0*CA1*CA22*CA3*EL*MH12*SA12*DBLE(MW**INT(-3.D0))*d&
  &MW2Usual())/(SB*SW) + (0.09375D0*CA1*CA3*EL*MH22*SA12*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW) + (0.09375D0*CA1*CA22*CA3*EL*M&
  &H22*SA12*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW) - (0.0625D0*CA1*CA3*EL*MH12*SA22*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW) &
  &- (0.03125D0*CA1*CA3*EL*MH22*SA22*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW) + (0.109375D0*CA3*EL*m12squared*SA1*SA22*DBLE(MW**&
  &INT(-3.D0))*dMW2Usual())/(SB*SW) - (0.078125D0*CA3*EL*m12squared*SA1*SA22*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB2*SB*SW) - (0.2&
  &8125D0*CA12*CA3*EL*m12squared*SA1*SA22*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB2*SB*SW) - (0.1875D0*CA1*CA3*EL*MH12*SA12*SA22*DBL&
  &E(MW**INT(-3.D0))*dMW2Usual())/(SB*SW) - (0.09375D0*CA1*CA3*EL*MH22*SA12*SA22*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW) - (0.1&
  &40625D0*CA1*EL*m12squared*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW) - (0.421875D0*CA1*CA22*EL*m12squared*SA2*SA3*DBLE(&
  &MW**INT(-3.D0))*dMW2Usual())/(SB*SW) + (0.09375D0*CA1*EL*m12squared*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB2*SB*SW) + (0&
  &.28125D0*CA1*CA22*EL*m12squared*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB2*SB*SW) - (0.09375D0*EL*MH12*SA1*SA2*SA3*DBLE(MW&
  &**INT(-3.D0))*dMW2Usual())/(SB*SW) + (0.09375D0*CA12*EL*MH12*SA1*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW) - (0.28125D&
  &0*CA22*EL*MH12*SA1*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW) + (0.28125D0*CA12*CA22*EL*MH12*SA1*SA2*SA3*DBLE(MW**INT(-&
  &3.D0))*dMW2Usual())/(SB*SW) - (0.046875D0*EL*MH22*SA1*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW) + (0.046875D0*CA12*EL*&
  &MH22*SA1*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW) - (0.140625D0*CA22*EL*MH22*SA1*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Usu&
  &al())/(SB*SW) + (0.140625D0*CA12*CA22*EL*MH22*SA1*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW) - (0.140625D0*CA1*EL*m12sq&
  &uared*SA12*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB2*SB*SW) - (0.421875D0*CA1*CA22*EL*m12squared*SA12*SA2*SA3*DBLE(MW**IN&
  &T(-3.D0))*dMW2Usual())/(CB2*SB*SW) - (0.03125D0*CA1*CA3*EL*m12squared*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SB2*SW) - (0.03125&
  &D0*CA1*CA22*CA3*EL*m12squared*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SB2*SW) - (0.28125D0*CA1*CA3*EL*m12squared*SA12*DBLE(MW**I&
  &NT(-3.D0))*dMW2Usual())/(CB*SB2*SW) - (0.28125D0*CA1*CA22*CA3*EL*m12squared*SA12*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SB2*SW)&
  & + (0.03125D0*CA1*CA3*EL*m12squared*SA22*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SB2*SW) + (0.28125D0*CA1*CA3*EL*m12squared*SA12&
  &*SA22*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SB2*SW) + (0.09375D0*EL*m12squared*SA1*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(&
  &CB*SB2*SW) - (0.140625D0*CA12*EL*m12squared*SA1*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SB2*SW) + (0.28125D0*CA22*EL*m12&
  &squared*SA1*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SB2*SW) - (0.421875D0*CA12*CA22*EL*m12squared*SA1*SA2*SA3*DBLE(MW**I&
  &NT(-3.D0))*dMW2Usual())/(CB*SB2*SW) - (0.0625D0*CA1*CA3*EL*m12squared*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW*TB) - (0.0625D0&
  &*CA1*CA22*CA3*EL*m12squared*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW*TB) + (0.0625D0*CA1*CA3*EL*m12squared*SA22*DBLE(MW**INT(-&
  &3.D0))*dMW2Usual())/(SB*SW*TB) + (0.046875D0*EL*m12squared*SA1*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW*TB) + (0.14062&
  &5D0*CA22*EL*m12squared*SA1*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW*TB) + (0.015625D0*CA3*EL*m12squared*SA1*TB*DBLE(MW&
  &**INT(-3.D0))*dMW2Usual())/(CB*SW) + (0.015625D0*CA22*CA3*EL*m12squared*SA1*TB*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SW) - (0.&
  &015625D0*CA3*EL*m12squared*SA1*SA22*TB*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SW) + (0.046875D0*CA1*EL*m12squared*SA2*SA3*TB*DB&
  &LE(MW**INT(-3.D0))*dMW2Usual())/(CB*SW) + (0.140625D0*CA1*CA22*EL*m12squared*SA2*SA3*TB*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*&
  &SW) - (0.03125D0*EL*MH12*SA2*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SW) - (0.09375D0*CA22*EL*MH12*SA2*&
  &SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SW) - (0.015625D0*EL*MH22*SA2*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW*&
  &*INT(-3.D0))*dMW2Usual())/(CB*SW) - (0.046875D0*CA22*EL*MH22*SA2*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Usual())/(C&
  &B*SW) - (0.0625D0*CA3*EL*MH12*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW) - (0.0625D0*CA22*CA3*EL*MH12*DBLE&
  &(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW) - (0.03125D0*CA3*EL*MH22*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*&
  &dMW2Usual())/(SB*SW) - (0.03125D0*CA22*CA3*EL*MH22*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW) + (0.0625D0*&
  &CA3*EL*MH12*SA22*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW) + (0.03125D0*CA3*EL*MH22*SA22*DBLE(CA1**INT(3.&
  &D0))*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW) + (0.046875D0*EL*m12squared*SA2*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*d&
  &MW2Usual())/(CB2*SB*SW) + (0.140625D0*CA22*EL*m12squared*SA2*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB2*S&
  &B*SW) + (0.09375D0*CA3*EL*m12squared*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SB2*SW) + (0.09375D0*CA22*CA3*&
  &EL*m12squared*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SB2*SW) - (0.09375D0*CA3*EL*m12squared*SA22*DBLE(CA1*&
  &*INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SB2*SW) + (0.0625D0*CA3*EL*MH12*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dM&
  &W2Usual())/(CB*SW) + (0.0625D0*CA22*CA3*EL*MH12*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Usual())/(CB*SW) + (0.03125D0*CA&
  &3*EL*MH22*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Usual())/(CB*SW) + (0.03125D0*CA22*CA3*EL*MH22*DBLE(MW**INT(-3.D0))*DB&
  &LE(SA1**INT(3.D0))*dMW2Usual())/(CB*SW) - (0.0625D0*CA3*EL*MH12*SA22*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Usual())/(C&
  &B*SW) - (0.03125D0*CA3*EL*MH22*SA22*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Usual())/(CB*SW) - (0.09375D0*CA3*EL*m12squa&
  &red*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Usual())/(CB2*SB*SW) - (0.09375D0*CA22*CA3*EL*m12squared*DBLE(MW**INT(-3.D0)&
  &)*DBLE(SA1**INT(3.D0))*dMW2Usual())/(CB2*SB*SW) + (0.09375D0*CA3*EL*m12squared*SA22*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*&
  &dMW2Usual())/(CB2*SB*SW) - (0.03125D0*EL*MH12*SA2*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Usual())/(SB*SW) - (0.0937&
  &5D0*CA22*EL*MH12*SA2*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Usual())/(SB*SW) - (0.015625D0*EL*MH22*SA2*SA3*DBLE(MW*&
  &*INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Usual())/(SB*SW) - (0.046875D0*CA22*EL*MH22*SA2*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3&
  &.D0))*dMW2Usual())/(SB*SW) + (0.046875D0*EL*m12squared*SA2*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Usual())/(CB*SB2*&
  &SW) + (0.140625D0*CA22*EL*m12squared*SA2*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Usual())/(CB*SB2*SW) + (0.09375D0*C&
  &A1*EL*MH12*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Usual())/(CB*SW) + (0.046875D0*CA1*EL*MH22*SA3*DBLE(MW**INT(-3.D0&
  &))*DBLE(SA2**INT(3.D0))*dMW2Usual())/(CB*SW) + (0.140625D0*EL*m12squared*SA1*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW&
  &2Usual())/(CB*SW) - (0.09375D0*CA1*EL*MH12*SA12*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Usual())/(CB*SW) - (0.046875&
  &D0*CA1*EL*MH22*SA12*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Usual())/(CB*SW) + (0.140625D0*CA1*EL*m12squared*SA3*DBL&
  &E(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Usual())/(SB*SW) - (0.09375D0*CA1*EL*m12squared*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA2**&
  &INT(3.D0))*dMW2Usual())/(CB2*SB*SW) + (0.09375D0*EL*MH12*SA1*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Usual())/(SB*SW&
  &) - (0.09375D0*CA12*EL*MH12*SA1*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Usual())/(SB*SW) + (0.046875D0*EL*MH22*SA1*S&
  &A3*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Usual())/(SB*SW) - (0.046875D0*CA12*EL*MH22*SA1*SA3*DBLE(MW**INT(-3.D0))*DBLE&
  &(SA2**INT(3.D0))*dMW2Usual())/(SB*SW) + (0.140625D0*CA1*EL*m12squared*SA12*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2U&
  &sual())/(CB2*SB*SW) - (0.09375D0*EL*m12squared*SA1*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Usual())/(CB*SB2*SW) + (0&
  &.140625D0*CA12*EL*m12squared*SA1*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Usual())/(CB*SB2*SW) - (0.046875D0*EL*m12sq&
  &uared*SA1*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Usual())/(SB*SW*TB) - (0.046875D0*CA1*EL*m12squared*SA3*TB*DBLE(MW&
  &**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Usual())/(CB*SW) + (0.03125D0*EL*MH12*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*DBL&
  &E(SA2**INT(3.D0))*dMW2Usual())/(CB*SW) + (0.015625D0*EL*MH22*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0)&
  &)*dMW2Usual())/(CB*SW) - (0.046875D0*EL*m12squared*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Usua&
  &l())/(CB2*SB*SW) + (0.03125D0*EL*MH12*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dMW2Usual())/(SB*SW) &
  &+ (0.015625D0*EL*MH22*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dMW2Usual())/(SB*SW) - (0.046875D0*EL&
  &*m12squared*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dMW2Usual())/(CB*SB2*SW) + 0.5D0*CA2*MH12*SA3*D&
  &BLE(vS**INT(-2.D0))*dvSMSBarUsual() + 0.25D0*CA2*MH22*SA3*DBLE(vS**INT(-2.D0))*dvSMSBarUsual() + 1.5D0*CA2*MH12*SA22*SA3*DBLE(&
  &vS**INT(-2.D0))*dvSMSBarUsual() + 0.75D0*CA2*MH22*SA22*SA3*DBLE(vS**INT(-2.D0))*dvSMSBarUsual() - 0.5D0*MH12*SA3*DBLE(CA2**INT&
  &(3.D0))*DBLE(vS**INT(-2.D0))*dvSMSBarUsual() - 0.25D0*MH22*SA3*DBLE(CA2**INT(3.D0))*DBLE(vS**INT(-2.D0))*dvSMSBarUsual() &
		& + CS1S1S1f111*dZH1H2OSUsual()/2D0 + CS1S1S1f211*dZH2H2OS()/2D0 + CS1S1S1f311*dZH3H2OSUsual()/2D0 &
		& + CS1S1S1f121*dZH1H1OS()/2D0 + CS1S1S1f221*dZH2H1OSUsual()/2D0 + CS1S1S1f321*dZH3H1OSUsual()/2D0 &
		& + CS1S1S1f121*dZH1H1OS()/2D0 + CS1S1S1f221*dZH2H1OSUsual()/2D0 + CS1S1S1f321*dZH3H1OSUsual()/2D0 &
		& )
	case (3)
		dRR11 = -CA2*SA1*dAlpha1KanAlter() - CA1*SA2*dAlpha2KanAlter()
		dRR12 = CA1*CA2*dAlpha1KanAlter() - SA1*SA2*dAlpha2KanAlter()
		dRR13 = CA2*dAlpha2KanAlter()
		dRR21 = -CA1*CA3*dAlpha1KanAlter() - CA1*CA3*SA2*dAlpha3KanAlter() - CA1*CA2*SA3*dAlpha2KanAlter() + &
			& SA1*SA3*dAlpha3KanAlter() + SA1*SA2*SA3*dAlpha1KanAlter()
		dRR22 = -CA3*SA1*dAlpha1KanAlter() - CA3*SA1*SA2*dAlpha3KanAlter() - CA1*SA3*dAlpha3KanAlter() - &
			& CA2*SA1*SA3*dAlpha2KanAlter() - CA1*SA2*SA3*dAlpha1KanAlter()
		dRR23 = CA2*CA3*dAlpha3KanAlter() - SA2*SA3*dAlpha2KanAlter()
		dRR31 = -CA1*CA2*CA3*dAlpha2KanAlter() + CA3*SA1*dAlpha3KanAlter() + CA3*SA1*SA2*dAlpha1KanAlter() + &
			& CA1*SA3*dAlpha1KanAlter() + CA1*SA2*SA3*dAlpha3KanAlter()
		dRR32 = -CA1*CA3*dAlpha3KanAlter() - CA2*CA3*SA1*dAlpha2KanAlter() - CA1*CA3*SA2*dAlpha1KanAlter() + &
			& SA1*SA3*dAlpha1KanAlter() + SA1*SA2*SA3*dAlpha3KanAlter()
		dRR33 = -CA3*SA2*dAlpha2KanAlter() - CA2*SA3*dAlpha3KanAlter()

		totalAmplitude = CS1S1S1f211*( &
&(0.125D0*CA1*CA3*EL*MH12*dAlpha1KanAlter())/(CB*MW*SW) + (0.125D0*CA1*CA22*CA3*EL*MH12*dAlpha1KanAlter())/(CB*MW*SW) + (0.0625D0*&
  &CA1*CA3*EL*MH22*dAlpha1KanAlter())/(CB*MW*SW) + (0.0625D0*CA1*CA22*CA3*EL*MH22*dAlpha1KanAlter())/(CB*MW*SW) + (0.125D0*CA3*EL&
  &*m12squared*SA1*dAlpha1KanAlter())/(CB*MW*SW) + (0.125D0*CA22*CA3*EL*m12squared*SA1*dAlpha1KanAlter())/(CB*MW*SW) - (1.125D0*C&
  &A1*CA3*EL*MH12*SA12*dAlpha1KanAlter())/(CB*MW*SW) - (1.125D0*CA1*CA22*CA3*EL*MH12*SA12*dAlpha1KanAlter())/(CB*MW*SW) - (0.5625&
  &D0*CA1*CA3*EL*MH22*SA12*dAlpha1KanAlter())/(CB*MW*SW) - (0.5625D0*CA1*CA22*CA3*EL*MH22*SA12*dAlpha1KanAlter())/(CB*MW*SW) - (0&
  &.125D0*CA1*CA3*EL*MH12*SA22*dAlpha1KanAlter())/(CB*MW*SW) - (0.0625D0*CA1*CA3*EL*MH22*SA22*dAlpha1KanAlter())/(CB*MW*SW) - (0.&
  &125D0*CA3*EL*m12squared*SA1*SA22*dAlpha1KanAlter())/(CB*MW*SW) + (1.125D0*CA1*CA3*EL*MH12*SA12*SA22*dAlpha1KanAlter())/(CB*MW*&
  &SW) + (0.5625D0*CA1*CA3*EL*MH22*SA12*SA22*dAlpha1KanAlter())/(CB*MW*SW) + (0.28125D0*CA1*EL*m12squared*SA2*SA3*dAlpha1KanAlter&
  &())/(CB*MW*SW) + (0.84375D0*CA1*CA22*EL*m12squared*SA2*SA3*dAlpha1KanAlter())/(CB*MW*SW) - (0.1875D0*EL*MH12*SA1*SA2*SA3*dAlph&
  &a1KanAlter())/(CB*MW*SW) - (0.5625D0*CA12*EL*MH12*SA1*SA2*SA3*dAlpha1KanAlter())/(CB*MW*SW) - (0.5625D0*CA22*EL*MH12*SA1*SA2*S&
  &A3*dAlpha1KanAlter())/(CB*MW*SW) - (1.6875D0*CA12*CA22*EL*MH12*SA1*SA2*SA3*dAlpha1KanAlter())/(CB*MW*SW) - (0.09375D0*EL*MH22*&
  &SA1*SA2*SA3*dAlpha1KanAlter())/(CB*MW*SW) - (0.28125D0*CA12*EL*MH22*SA1*SA2*SA3*dAlpha1KanAlter())/(CB*MW*SW) - (0.28125D0*CA2&
  &2*EL*MH22*SA1*SA2*SA3*dAlpha1KanAlter())/(CB*MW*SW) - (0.84375D0*CA12*CA22*EL*MH22*SA1*SA2*SA3*dAlpha1KanAlter())/(CB*MW*SW) +&
  & (0.21875D0*CA1*CA3*EL*m12squared*dAlpha1KanAlter())/(MW*SB*SW) + (0.21875D0*CA1*CA22*CA3*EL*m12squared*dAlpha1KanAlter())/(MW&
  &*SB*SW) - (0.15625D0*CA1*CA3*EL*m12squared*dAlpha1KanAlter())/(CB2*MW*SB*SW) - (0.15625D0*CA1*CA22*CA3*EL*m12squared*dAlpha1Ka&
  &nAlter())/(CB2*MW*SB*SW) + (0.125D0*CA3*EL*MH12*SA1*dAlpha1KanAlter())/(MW*SB*SW) - (1.125D0*CA12*CA3*EL*MH12*SA1*dAlpha1KanAl&
  &ter())/(MW*SB*SW) + (0.125D0*CA22*CA3*EL*MH12*SA1*dAlpha1KanAlter())/(MW*SB*SW) - (1.125D0*CA12*CA22*CA3*EL*MH12*SA1*dAlpha1Ka&
  &nAlter())/(MW*SB*SW) + (0.0625D0*CA3*EL*MH22*SA1*dAlpha1KanAlter())/(MW*SB*SW) - (0.5625D0*CA12*CA3*EL*MH22*SA1*dAlpha1KanAlte&
  &r())/(MW*SB*SW) + (0.0625D0*CA22*CA3*EL*MH22*SA1*dAlpha1KanAlter())/(MW*SB*SW) - (0.5625D0*CA12*CA22*CA3*EL*MH22*SA1*dAlpha1Ka&
  &nAlter())/(MW*SB*SW) + (1.6875D0*CA1*CA3*EL*m12squared*SA12*dAlpha1KanAlter())/(CB2*MW*SB*SW) + (1.6875D0*CA1*CA22*CA3*EL*m12s&
  &quared*SA12*dAlpha1KanAlter())/(CB2*MW*SB*SW) - (0.21875D0*CA1*CA3*EL*m12squared*SA22*dAlpha1KanAlter())/(MW*SB*SW) + (0.15625&
  &D0*CA1*CA3*EL*m12squared*SA22*dAlpha1KanAlter())/(CB2*MW*SB*SW) - (0.125D0*CA3*EL*MH12*SA1*SA22*dAlpha1KanAlter())/(MW*SB*SW) &
  &+ (1.125D0*CA12*CA3*EL*MH12*SA1*SA22*dAlpha1KanAlter())/(MW*SB*SW) - (0.0625D0*CA3*EL*MH22*SA1*SA22*dAlpha1KanAlter())/(MW*SB*&
  &SW) + (0.5625D0*CA12*CA3*EL*MH22*SA1*SA22*dAlpha1KanAlter())/(MW*SB*SW) - (1.6875D0*CA1*CA3*EL*m12squared*SA12*SA22*dAlpha1Kan&
  &Alter())/(CB2*MW*SB*SW) + (0.1875D0*CA1*EL*MH12*SA2*SA3*dAlpha1KanAlter())/(MW*SB*SW) + (0.5625D0*CA1*CA22*EL*MH12*SA2*SA3*dAl&
  &pha1KanAlter())/(MW*SB*SW) + (0.09375D0*CA1*EL*MH22*SA2*SA3*dAlpha1KanAlter())/(MW*SB*SW) + (0.28125D0*CA1*CA22*EL*MH22*SA2*SA&
  &3*dAlpha1KanAlter())/(MW*SB*SW) - (0.28125D0*EL*m12squared*SA1*SA2*SA3*dAlpha1KanAlter())/(MW*SB*SW) - (0.84375D0*CA22*EL*m12s&
  &quared*SA1*SA2*SA3*dAlpha1KanAlter())/(MW*SB*SW) + (0.1875D0*EL*m12squared*SA1*SA2*SA3*dAlpha1KanAlter())/(CB2*MW*SB*SW) + (0.&
  &84375D0*CA12*EL*m12squared*SA1*SA2*SA3*dAlpha1KanAlter())/(CB2*MW*SB*SW) + (0.5625D0*CA22*EL*m12squared*SA1*SA2*SA3*dAlpha1Kan&
  &Alter())/(CB2*MW*SB*SW) + (2.53125D0*CA12*CA22*EL*m12squared*SA1*SA2*SA3*dAlpha1KanAlter())/(CB2*MW*SB*SW) + (0.5625D0*CA1*EL*&
  &MH12*SA12*SA2*SA3*dAlpha1KanAlter())/(MW*SB*SW) + (1.6875D0*CA1*CA22*EL*MH12*SA12*SA2*SA3*dAlpha1KanAlter())/(MW*SB*SW) + (0.2&
  &8125D0*CA1*EL*MH22*SA12*SA2*SA3*dAlpha1KanAlter())/(MW*SB*SW) + (0.84375D0*CA1*CA22*EL*MH22*SA12*SA2*SA3*dAlpha1KanAlter())/(M&
  &W*SB*SW) - (0.0625D0*CA3*EL*m12squared*SA1*dAlpha1KanAlter())/(CB*MW*SB2*SW) + (1.6875D0*CA12*CA3*EL*m12squared*SA1*dAlpha1Kan&
  &Alter())/(CB*MW*SB2*SW) - (0.0625D0*CA22*CA3*EL*m12squared*SA1*dAlpha1KanAlter())/(CB*MW*SB2*SW) + (1.6875D0*CA12*CA22*CA3*EL*&
  &m12squared*SA1*dAlpha1KanAlter())/(CB*MW*SB2*SW) + (0.0625D0*CA3*EL*m12squared*SA1*SA22*dAlpha1KanAlter())/(CB*MW*SB2*SW) - (1&
  &.6875D0*CA12*CA3*EL*m12squared*SA1*SA22*dAlpha1KanAlter())/(CB*MW*SB2*SW) - (0.1875D0*CA1*EL*m12squared*SA2*SA3*dAlpha1KanAlte&
  &r())/(CB*MW*SB2*SW) - (0.5625D0*CA1*CA22*EL*m12squared*SA2*SA3*dAlpha1KanAlter())/(CB*MW*SB2*SW) - (0.84375D0*CA1*EL*m12square&
  &d*SA12*SA2*SA3*dAlpha1KanAlter())/(CB*MW*SB2*SW) - (2.53125D0*CA1*CA22*EL*m12squared*SA12*SA2*SA3*dAlpha1KanAlter())/(CB*MW*SB&
  &2*SW) - (0.125D0*CA3*EL*m12squared*SA1*dAlpha1KanAlter())/(MW*SB*SW*TB) - (0.125D0*CA22*CA3*EL*m12squared*SA1*dAlpha1KanAlter(&
  &))/(MW*SB*SW*TB) + (0.125D0*CA3*EL*m12squared*SA1*SA22*dAlpha1KanAlter())/(MW*SB*SW*TB) - (0.09375D0*CA1*EL*m12squared*SA2*SA3&
  &*dAlpha1KanAlter())/(MW*SB*SW*TB) - (0.28125D0*CA1*CA22*EL*m12squared*SA2*SA3*dAlpha1KanAlter())/(MW*SB*SW*TB) - (0.03125D0*CA&
  &1*CA3*EL*m12squared*TB*dAlpha1KanAlter())/(CB*MW*SW) - (0.03125D0*CA1*CA22*CA3*EL*m12squared*TB*dAlpha1KanAlter())/(CB*MW*SW) &
  &+ (0.03125D0*CA1*CA3*EL*m12squared*SA22*TB*dAlpha1KanAlter())/(CB*MW*SW) + (0.09375D0*EL*m12squared*SA1*SA2*SA3*TB*dAlpha1KanA&
  &lter())/(CB*MW*SW) + (0.28125D0*CA22*EL*m12squared*SA1*SA2*SA3*TB*dAlpha1KanAlter())/(CB*MW*SW) + (0.5D0*CA1*CA2*CA3*EL*m12squ&
  &ared*SA2*dAlpha2KanAlter())/(CB*MW*SW) - (0.5D0*CA2*CA3*EL*MH12*SA1*SA2*dAlpha2KanAlter())/(CB*MW*SW) - (1.5D0*CA12*CA2*CA3*EL&
  &*MH12*SA1*SA2*dAlpha2KanAlter())/(CB*MW*SW) - (0.25D0*CA2*CA3*EL*MH22*SA1*SA2*dAlpha2KanAlter())/(CB*MW*SW) - (0.75D0*CA12*CA2&
  &*CA3*EL*MH22*SA1*SA2*dAlpha2KanAlter())/(CB*MW*SW) + (0.1875D0*CA1*CA2*EL*MH12*SA3*dAlpha2KanAlter())/(CB*MW*SW) + (0.09375D0*&
  &CA1*CA2*EL*MH22*SA3*dAlpha2KanAlter())/(CB*MW*SW) + (0.28125D0*CA2*EL*m12squared*SA1*SA3*dAlpha2KanAlter())/(CB*MW*SW) - (0.18&
  &75D0*CA1*CA2*EL*MH12*SA12*SA3*dAlpha2KanAlter())/(CB*MW*SW) - (0.09375D0*CA1*CA2*EL*MH22*SA12*SA3*dAlpha2KanAlter())/(CB*MW*SW&
  &) - (1.6875D0*CA1*CA2*EL*MH12*SA22*SA3*dAlpha2KanAlter())/(CB*MW*SW) - (0.84375D0*CA1*CA2*EL*MH22*SA22*SA3*dAlpha2KanAlter())/&
  &(CB*MW*SW) - (2.53125D0*CA2*EL*m12squared*SA1*SA22*SA3*dAlpha2KanAlter())/(CB*MW*SW) + (1.6875D0*CA1*CA2*EL*MH12*SA12*SA22*SA3&
  &*dAlpha2KanAlter())/(CB*MW*SW) + (0.84375D0*CA1*CA2*EL*MH22*SA12*SA22*SA3*dAlpha2KanAlter())/(CB*MW*SW) + (0.5D0*CA1*CA2*CA3*E&
  &L*MH12*SA2*dAlpha2KanAlter())/(MW*SB*SW) + (0.25D0*CA1*CA2*CA3*EL*MH22*SA2*dAlpha2KanAlter())/(MW*SB*SW) - (0.875D0*CA2*CA3*EL&
  &*m12squared*SA1*SA2*dAlpha2KanAlter())/(MW*SB*SW) + (0.625D0*CA2*CA3*EL*m12squared*SA1*SA2*dAlpha2KanAlter())/(CB2*MW*SB*SW) +&
  & (2.25D0*CA12*CA2*CA3*EL*m12squared*SA1*SA2*dAlpha2KanAlter())/(CB2*MW*SB*SW) + (1.5D0*CA1*CA2*CA3*EL*MH12*SA12*SA2*dAlpha2Kan&
  &Alter())/(MW*SB*SW) + (0.75D0*CA1*CA2*CA3*EL*MH22*SA12*SA2*dAlpha2KanAlter())/(MW*SB*SW) + (0.28125D0*CA1*CA2*EL*m12squared*SA&
  &3*dAlpha2KanAlter())/(MW*SB*SW) - (0.1875D0*CA1*CA2*EL*m12squared*SA3*dAlpha2KanAlter())/(CB2*MW*SB*SW) + (0.1875D0*CA2*EL*MH1&
  &2*SA1*SA3*dAlpha2KanAlter())/(MW*SB*SW) - (0.1875D0*CA12*CA2*EL*MH12*SA1*SA3*dAlpha2KanAlter())/(MW*SB*SW) + (0.09375D0*CA2*EL&
  &*MH22*SA1*SA3*dAlpha2KanAlter())/(MW*SB*SW) - (0.09375D0*CA12*CA2*EL*MH22*SA1*SA3*dAlpha2KanAlter())/(MW*SB*SW) + (0.28125D0*C&
  &A1*CA2*EL*m12squared*SA12*SA3*dAlpha2KanAlter())/(CB2*MW*SB*SW) - (2.53125D0*CA1*CA2*EL*m12squared*SA22*SA3*dAlpha2KanAlter())&
  &/(MW*SB*SW) + (1.6875D0*CA1*CA2*EL*m12squared*SA22*SA3*dAlpha2KanAlter())/(CB2*MW*SB*SW) - (1.6875D0*CA2*EL*MH12*SA1*SA22*SA3*&
  &dAlpha2KanAlter())/(MW*SB*SW) + (1.6875D0*CA12*CA2*EL*MH12*SA1*SA22*SA3*dAlpha2KanAlter())/(MW*SB*SW) - (0.84375D0*CA2*EL*MH22&
  &*SA1*SA22*SA3*dAlpha2KanAlter())/(MW*SB*SW) + (0.84375D0*CA12*CA2*EL*MH22*SA1*SA22*SA3*dAlpha2KanAlter())/(MW*SB*SW) - (2.5312&
  &5D0*CA1*CA2*EL*m12squared*SA12*SA22*SA3*dAlpha2KanAlter())/(CB2*MW*SB*SW) - (0.25D0*CA1*CA2*CA3*EL*m12squared*SA2*dAlpha2KanAl&
  &ter())/(CB*MW*SB2*SW) - (2.25D0*CA1*CA2*CA3*EL*m12squared*SA12*SA2*dAlpha2KanAlter())/(CB*MW*SB2*SW) - (0.1875D0*CA2*EL*m12squ&
  &ared*SA1*SA3*dAlpha2KanAlter())/(CB*MW*SB2*SW) + (0.28125D0*CA12*CA2*EL*m12squared*SA1*SA3*dAlpha2KanAlter())/(CB*MW*SB2*SW) +&
  & (1.6875D0*CA2*EL*m12squared*SA1*SA22*SA3*dAlpha2KanAlter())/(CB*MW*SB2*SW) - (2.53125D0*CA12*CA2*EL*m12squared*SA1*SA22*SA3*d&
  &Alpha2KanAlter())/(CB*MW*SB2*SW) - (0.5D0*CA1*CA2*CA3*EL*m12squared*SA2*dAlpha2KanAlter())/(MW*SB*SW*TB) - (0.09375D0*CA2*EL*m&
  &12squared*SA1*SA3*dAlpha2KanAlter())/(MW*SB*SW*TB) + (0.84375D0*CA2*EL*m12squared*SA1*SA22*SA3*dAlpha2KanAlter())/(MW*SB*SW*TB&
  &) + (0.125D0*CA2*CA3*EL*m12squared*SA1*SA2*TB*dAlpha2KanAlter())/(CB*MW*SW) - (0.09375D0*CA1*CA2*EL*m12squared*SA3*TB*dAlpha2K&
  &anAlter())/(CB*MW*SW) + (0.84375D0*CA1*CA2*EL*m12squared*SA22*SA3*TB*dAlpha2KanAlter())/(CB*MW*SW) + (0.5D0*MH12*SA2*SA3*dAlph&
  &a2KanAlter())/vS - (4.5D0*CA22*MH12*SA2*SA3*dAlpha2KanAlter())/vS + (0.25D0*MH22*SA2*SA3*dAlpha2KanAlter())/vS - (2.25D0*CA22*&
  &MH22*SA2*SA3*dAlpha2KanAlter())/vS + (0.1875D0*CA1*CA3*EL*MH12*SA2*dAlpha3KanAlter())/(CB*MW*SW) + (0.5625D0*CA1*CA22*CA3*EL*M&
  &H12*SA2*dAlpha3KanAlter())/(CB*MW*SW) + (0.09375D0*CA1*CA3*EL*MH22*SA2*dAlpha3KanAlter())/(CB*MW*SW) + (0.28125D0*CA1*CA22*CA3&
  &*EL*MH22*SA2*dAlpha3KanAlter())/(CB*MW*SW) + (0.28125D0*CA3*EL*m12squared*SA1*SA2*dAlpha3KanAlter())/(CB*MW*SW) + (0.84375D0*C&
  &A22*CA3*EL*m12squared*SA1*SA2*dAlpha3KanAlter())/(CB*MW*SW) - (0.1875D0*CA1*CA3*EL*MH12*SA12*SA2*dAlpha3KanAlter())/(CB*MW*SW)&
  & - (0.5625D0*CA1*CA22*CA3*EL*MH12*SA12*SA2*dAlpha3KanAlter())/(CB*MW*SW) - (0.09375D0*CA1*CA3*EL*MH22*SA12*SA2*dAlpha3KanAlter&
  &())/(CB*MW*SW) - (0.28125D0*CA1*CA22*CA3*EL*MH22*SA12*SA2*dAlpha3KanAlter())/(CB*MW*SW) + (0.125D0*CA1*EL*m12squared*SA3*dAlph&
  &a3KanAlter())/(CB*MW*SW) + (0.125D0*CA1*CA22*EL*m12squared*SA3*dAlpha3KanAlter())/(CB*MW*SW) - (0.125D0*EL*MH12*SA1*SA3*dAlpha&
  &3KanAlter())/(CB*MW*SW) - (0.375D0*CA12*EL*MH12*SA1*SA3*dAlpha3KanAlter())/(CB*MW*SW) - (0.125D0*CA22*EL*MH12*SA1*SA3*dAlpha3K&
  &anAlter())/(CB*MW*SW) - (0.375D0*CA12*CA22*EL*MH12*SA1*SA3*dAlpha3KanAlter())/(CB*MW*SW) - (0.0625D0*EL*MH22*SA1*SA3*dAlpha3Ka&
  &nAlter())/(CB*MW*SW) - (0.1875D0*CA12*EL*MH22*SA1*SA3*dAlpha3KanAlter())/(CB*MW*SW) - (0.0625D0*CA22*EL*MH22*SA1*SA3*dAlpha3Ka&
  &nAlter())/(CB*MW*SW) - (0.1875D0*CA12*CA22*EL*MH22*SA1*SA3*dAlpha3KanAlter())/(CB*MW*SW) - (0.125D0*CA1*EL*m12squared*SA22*SA3&
  &*dAlpha3KanAlter())/(CB*MW*SW) + (0.125D0*EL*MH12*SA1*SA22*SA3*dAlpha3KanAlter())/(CB*MW*SW) + (0.375D0*CA12*EL*MH12*SA1*SA22*&
  &SA3*dAlpha3KanAlter())/(CB*MW*SW) + (0.0625D0*EL*MH22*SA1*SA22*SA3*dAlpha3KanAlter())/(CB*MW*SW) + (0.1875D0*CA12*EL*MH22*SA1*&
  &SA22*SA3*dAlpha3KanAlter())/(CB*MW*SW) + (0.28125D0*CA1*CA3*EL*m12squared*SA2*dAlpha3KanAlter())/(MW*SB*SW) + (0.84375D0*CA1*C&
  &A22*CA3*EL*m12squared*SA2*dAlpha3KanAlter())/(MW*SB*SW) - (0.1875D0*CA1*CA3*EL*m12squared*SA2*dAlpha3KanAlter())/(CB2*MW*SB*SW&
  &) - (0.5625D0*CA1*CA22*CA3*EL*m12squared*SA2*dAlpha3KanAlter())/(CB2*MW*SB*SW) + (0.1875D0*CA3*EL*MH12*SA1*SA2*dAlpha3KanAlter&
  &())/(MW*SB*SW) - (0.1875D0*CA12*CA3*EL*MH12*SA1*SA2*dAlpha3KanAlter())/(MW*SB*SW) + (0.5625D0*CA22*CA3*EL*MH12*SA1*SA2*dAlpha3&
  &KanAlter())/(MW*SB*SW) - (0.5625D0*CA12*CA22*CA3*EL*MH12*SA1*SA2*dAlpha3KanAlter())/(MW*SB*SW) + (0.09375D0*CA3*EL*MH22*SA1*SA&
  &2*dAlpha3KanAlter())/(MW*SB*SW) - (0.09375D0*CA12*CA3*EL*MH22*SA1*SA2*dAlpha3KanAlter())/(MW*SB*SW) + (0.28125D0*CA22*CA3*EL*M&
  &H22*SA1*SA2*dAlpha3KanAlter())/(MW*SB*SW) - (0.28125D0*CA12*CA22*CA3*EL*MH22*SA1*SA2*dAlpha3KanAlter())/(MW*SB*SW) + (0.28125D&
  &0*CA1*CA3*EL*m12squared*SA12*SA2*dAlpha3KanAlter())/(CB2*MW*SB*SW) + (0.84375D0*CA1*CA22*CA3*EL*m12squared*SA12*SA2*dAlpha3Kan&
  &Alter())/(CB2*MW*SB*SW) + (0.125D0*CA1*EL*MH12*SA3*dAlpha3KanAlter())/(MW*SB*SW) + (0.125D0*CA1*CA22*EL*MH12*SA3*dAlpha3KanAlt&
  &er())/(MW*SB*SW) + (0.0625D0*CA1*EL*MH22*SA3*dAlpha3KanAlter())/(MW*SB*SW) + (0.0625D0*CA1*CA22*EL*MH22*SA3*dAlpha3KanAlter())&
  &/(MW*SB*SW) - (0.21875D0*EL*m12squared*SA1*SA3*dAlpha3KanAlter())/(MW*SB*SW) - (0.21875D0*CA22*EL*m12squared*SA1*SA3*dAlpha3Ka&
  &nAlter())/(MW*SB*SW) + (0.15625D0*EL*m12squared*SA1*SA3*dAlpha3KanAlter())/(CB2*MW*SB*SW) + (0.5625D0*CA12*EL*m12squared*SA1*S&
  &A3*dAlpha3KanAlter())/(CB2*MW*SB*SW) + (0.15625D0*CA22*EL*m12squared*SA1*SA3*dAlpha3KanAlter())/(CB2*MW*SB*SW) + (0.5625D0*CA1&
  &2*CA22*EL*m12squared*SA1*SA3*dAlpha3KanAlter())/(CB2*MW*SB*SW) + (0.375D0*CA1*EL*MH12*SA12*SA3*dAlpha3KanAlter())/(MW*SB*SW) +&
  & (0.375D0*CA1*CA22*EL*MH12*SA12*SA3*dAlpha3KanAlter())/(MW*SB*SW) + (0.1875D0*CA1*EL*MH22*SA12*SA3*dAlpha3KanAlter())/(MW*SB*S&
  &W) + (0.1875D0*CA1*CA22*EL*MH22*SA12*SA3*dAlpha3KanAlter())/(MW*SB*SW) - (0.125D0*CA1*EL*MH12*SA22*SA3*dAlpha3KanAlter())/(MW*&
  &SB*SW) - (0.0625D0*CA1*EL*MH22*SA22*SA3*dAlpha3KanAlter())/(MW*SB*SW) + (0.21875D0*EL*m12squared*SA1*SA22*SA3*dAlpha3KanAlter(&
  &))/(MW*SB*SW) - (0.15625D0*EL*m12squared*SA1*SA22*SA3*dAlpha3KanAlter())/(CB2*MW*SB*SW) - (0.5625D0*CA12*EL*m12squared*SA1*SA2&
  &2*SA3*dAlpha3KanAlter())/(CB2*MW*SB*SW) - (0.375D0*CA1*EL*MH12*SA12*SA22*SA3*dAlpha3KanAlter())/(MW*SB*SW) - (0.1875D0*CA1*EL*&
  &MH22*SA12*SA22*SA3*dAlpha3KanAlter())/(MW*SB*SW) - (0.1875D0*CA3*EL*m12squared*SA1*SA2*dAlpha3KanAlter())/(CB*MW*SB2*SW) + (0.&
  &28125D0*CA12*CA3*EL*m12squared*SA1*SA2*dAlpha3KanAlter())/(CB*MW*SB2*SW) - (0.5625D0*CA22*CA3*EL*m12squared*SA1*SA2*dAlpha3Kan&
  &Alter())/(CB*MW*SB2*SW) + (0.84375D0*CA12*CA22*CA3*EL*m12squared*SA1*SA2*dAlpha3KanAlter())/(CB*MW*SB2*SW) - (0.0625D0*CA1*EL*&
  &m12squared*SA3*dAlpha3KanAlter())/(CB*MW*SB2*SW) - (0.0625D0*CA1*CA22*EL*m12squared*SA3*dAlpha3KanAlter())/(CB*MW*SB2*SW) - (0&
  &.5625D0*CA1*EL*m12squared*SA12*SA3*dAlpha3KanAlter())/(CB*MW*SB2*SW) - (0.5625D0*CA1*CA22*EL*m12squared*SA12*SA3*dAlpha3KanAlt&
  &er())/(CB*MW*SB2*SW) + (0.0625D0*CA1*EL*m12squared*SA22*SA3*dAlpha3KanAlter())/(CB*MW*SB2*SW) + (0.5625D0*CA1*EL*m12squared*SA&
  &12*SA22*SA3*dAlpha3KanAlter())/(CB*MW*SB2*SW) - (0.09375D0*CA3*EL*m12squared*SA1*SA2*dAlpha3KanAlter())/(MW*SB*SW*TB) - (0.281&
  &25D0*CA22*CA3*EL*m12squared*SA1*SA2*dAlpha3KanAlter())/(MW*SB*SW*TB) - (0.125D0*CA1*EL*m12squared*SA3*dAlpha3KanAlter())/(MW*S&
  &B*SW*TB) - (0.125D0*CA1*CA22*EL*m12squared*SA3*dAlpha3KanAlter())/(MW*SB*SW*TB) + (0.125D0*CA1*EL*m12squared*SA22*SA3*dAlpha3K&
  &anAlter())/(MW*SB*SW*TB) - (0.09375D0*CA1*CA3*EL*m12squared*SA2*TB*dAlpha3KanAlter())/(CB*MW*SW) - (0.28125D0*CA1*CA22*CA3*EL*&
  &m12squared*SA2*TB*dAlpha3KanAlter())/(CB*MW*SW) + (0.03125D0*EL*m12squared*SA1*SA3*TB*dAlpha3KanAlter())/(CB*MW*SW) + (0.03125&
  &D0*CA22*EL*m12squared*SA1*SA3*TB*dAlpha3KanAlter())/(CB*MW*SW) - (0.03125D0*EL*m12squared*SA1*SA22*SA3*TB*dAlpha3KanAlter())/(&
  &CB*MW*SW) - (0.5D0*CA2*CA3*MH12*dAlpha3KanAlter())/vS - (0.25D0*CA2*CA3*MH22*dAlpha3KanAlter())/vS - (1.5D0*CA2*CA3*MH12*SA22*&
  &dAlpha3KanAlter())/vS - (0.75D0*CA2*CA3*MH22*SA22*dAlpha3KanAlter())/vS + (0.015625D0*CA3*EL*m12squared*SA1*dBeta1KanAlter())/&
  &(CB*MW*SW) + (0.015625D0*CA22*CA3*EL*m12squared*SA1*dBeta1KanAlter())/(CB*MW*SW) - (0.015625D0*CA3*EL*m12squared*SA1*SA22*dBet&
  &a1KanAlter())/(CB*MW*SW) + (0.09375D0*CA1*EL*m12squared*SA2*SA3*dBeta1KanAlter())/(CB*MW*SW) + (0.28125D0*CA1*CA22*EL*m12squar&
  &ed*SA2*SA3*dBeta1KanAlter())/(CB*MW*SW) - (0.0625D0*CA1*CA3*EL*m12squared*dBeta1KanAlter())/(MW*SB*SW) - (0.0625D0*CA1*CA22*CA&
  &3*EL*m12squared*dBeta1KanAlter())/(MW*SB*SW) + (0.0625D0*CA1*CA3*EL*m12squared*dBeta1KanAlter())/(CB2*MW*SB*SW) + (0.0625D0*CA&
  &1*CA22*CA3*EL*m12squared*dBeta1KanAlter())/(CB2*MW*SB*SW) - (0.0625D0*CA3*EL*MH12*SA1*dBeta1KanAlter())/(MW*SB*SW) - (0.1875D0&
  &*CA12*CA3*EL*MH12*SA1*dBeta1KanAlter())/(MW*SB*SW) - (0.0625D0*CA22*CA3*EL*MH12*SA1*dBeta1KanAlter())/(MW*SB*SW) - (0.1875D0*C&
  &A12*CA22*CA3*EL*MH12*SA1*dBeta1KanAlter())/(MW*SB*SW) + (0.0625D0*CA3*EL*MH12*SA1*dBeta1KanAlter())/(CB2*MW*SB*SW) + (0.1875D0&
  &*CA12*CA3*EL*MH12*SA1*dBeta1KanAlter())/(CB2*MW*SB*SW) + (0.0625D0*CA22*CA3*EL*MH12*SA1*dBeta1KanAlter())/(CB2*MW*SB*SW) + (0.&
  &1875D0*CA12*CA22*CA3*EL*MH12*SA1*dBeta1KanAlter())/(CB2*MW*SB*SW) - (0.03125D0*CA3*EL*MH22*SA1*dBeta1KanAlter())/(MW*SB*SW) - &
  &(0.09375D0*CA12*CA3*EL*MH22*SA1*dBeta1KanAlter())/(MW*SB*SW) - (0.03125D0*CA22*CA3*EL*MH22*SA1*dBeta1KanAlter())/(MW*SB*SW) - &
  &(0.09375D0*CA12*CA22*CA3*EL*MH22*SA1*dBeta1KanAlter())/(MW*SB*SW) + (0.03125D0*CA3*EL*MH22*SA1*dBeta1KanAlter())/(CB2*MW*SB*SW&
  &) + (0.09375D0*CA12*CA3*EL*MH22*SA1*dBeta1KanAlter())/(CB2*MW*SB*SW) + (0.03125D0*CA22*CA3*EL*MH22*SA1*dBeta1KanAlter())/(CB2*&
  &MW*SB*SW) + (0.09375D0*CA12*CA22*CA3*EL*MH22*SA1*dBeta1KanAlter())/(CB2*MW*SB*SW) + (0.5625D0*CA1*CA3*EL*m12squared*SA12*dBeta&
  &1KanAlter())/(CB2*MW*SB*SW) + (0.5625D0*CA1*CA22*CA3*EL*m12squared*SA12*dBeta1KanAlter())/(CB2*MW*SB*SW) + (0.0625D0*CA1*CA3*E&
  &L*m12squared*SA22*dBeta1KanAlter())/(MW*SB*SW) - (0.0625D0*CA1*CA3*EL*m12squared*SA22*dBeta1KanAlter())/(CB2*MW*SB*SW) + (0.06&
  &25D0*CA3*EL*MH12*SA1*SA22*dBeta1KanAlter())/(MW*SB*SW) + (0.1875D0*CA12*CA3*EL*MH12*SA1*SA22*dBeta1KanAlter())/(MW*SB*SW) - (0&
  &.0625D0*CA3*EL*MH12*SA1*SA22*dBeta1KanAlter())/(CB2*MW*SB*SW) - (0.1875D0*CA12*CA3*EL*MH12*SA1*SA22*dBeta1KanAlter())/(CB2*MW*&
  &SB*SW) + (0.03125D0*CA3*EL*MH22*SA1*SA22*dBeta1KanAlter())/(MW*SB*SW) + (0.09375D0*CA12*CA3*EL*MH22*SA1*SA22*dBeta1KanAlter())&
  &/(MW*SB*SW) - (0.03125D0*CA3*EL*MH22*SA1*SA22*dBeta1KanAlter())/(CB2*MW*SB*SW) - (0.09375D0*CA12*CA3*EL*MH22*SA1*SA22*dBeta1Ka&
  &nAlter())/(CB2*MW*SB*SW) - (0.5625D0*CA1*CA3*EL*m12squared*SA12*SA22*dBeta1KanAlter())/(CB2*MW*SB*SW) - (0.09375D0*CA1*EL*MH12&
  &*SA2*SA3*dBeta1KanAlter())/(MW*SB*SW) - (0.28125D0*CA1*CA22*EL*MH12*SA2*SA3*dBeta1KanAlter())/(MW*SB*SW) + (0.09375D0*CA1*EL*M&
  &H12*SA2*SA3*dBeta1KanAlter())/(CB2*MW*SB*SW) + (0.28125D0*CA1*CA22*EL*MH12*SA2*SA3*dBeta1KanAlter())/(CB2*MW*SB*SW) - (0.04687&
  &5D0*CA1*EL*MH22*SA2*SA3*dBeta1KanAlter())/(MW*SB*SW) - (0.140625D0*CA1*CA22*EL*MH22*SA2*SA3*dBeta1KanAlter())/(MW*SB*SW) + (0.&
  &046875D0*CA1*EL*MH22*SA2*SA3*dBeta1KanAlter())/(CB2*MW*SB*SW) + (0.140625D0*CA1*CA22*EL*MH22*SA2*SA3*dBeta1KanAlter())/(CB2*MW&
  &*SB*SW) - (0.140625D0*EL*m12squared*SA1*SA2*SA3*dBeta1KanAlter())/(MW*SB*SW) - (0.421875D0*CA22*EL*m12squared*SA1*SA2*SA3*dBet&
  &a1KanAlter())/(MW*SB*SW) - (0.046875D0*EL*m12squared*SA1*SA2*SA3*dBeta1KanAlter())/(CB2*MW*SB*SW) + (0.28125D0*CA12*EL*m12squa&
  &red*SA1*SA2*SA3*dBeta1KanAlter())/(CB2*MW*SB*SW) - (0.140625D0*CA22*EL*m12squared*SA1*SA2*SA3*dBeta1KanAlter())/(CB2*MW*SB*SW)&
  & + (0.84375D0*CA12*CA22*EL*m12squared*SA1*SA2*SA3*dBeta1KanAlter())/(CB2*MW*SB*SW) + (0.09375D0*CA1*EL*MH12*SA12*SA2*SA3*dBeta&
  &1KanAlter())/(MW*SB*SW) + (0.28125D0*CA1*CA22*EL*MH12*SA12*SA2*SA3*dBeta1KanAlter())/(MW*SB*SW) - (0.09375D0*CA1*EL*MH12*SA12*&
  &SA2*SA3*dBeta1KanAlter())/(CB2*MW*SB*SW) - (0.28125D0*CA1*CA22*EL*MH12*SA12*SA2*SA3*dBeta1KanAlter())/(CB2*MW*SB*SW) + (0.0468&
  &75D0*CA1*EL*MH22*SA12*SA2*SA3*dBeta1KanAlter())/(MW*SB*SW) + (0.140625D0*CA1*CA22*EL*MH22*SA12*SA2*SA3*dBeta1KanAlter())/(MW*S&
  &B*SW) - (0.046875D0*CA1*EL*MH22*SA12*SA2*SA3*dBeta1KanAlter())/(CB2*MW*SB*SW) - (0.140625D0*CA1*CA22*EL*MH22*SA12*SA2*SA3*dBet&
  &a1KanAlter())/(CB2*MW*SB*SW) + (0.203125D0*CA3*EL*m12squared*SA1*dBeta1KanAlter())/(CB*MW*SB2*SW) + (0.84375D0*CA12*CA3*EL*m12&
  &squared*SA1*dBeta1KanAlter())/(CB*MW*SB2*SW) + (0.203125D0*CA22*CA3*EL*m12squared*SA1*dBeta1KanAlter())/(CB*MW*SB2*SW) + (0.84&
  &375D0*CA12*CA22*CA3*EL*m12squared*SA1*dBeta1KanAlter())/(CB*MW*SB2*SW) - (0.203125D0*CA3*EL*m12squared*SA1*SA22*dBeta1KanAlter&
  &())/(CB*MW*SB2*SW) - (0.84375D0*CA12*CA3*EL*m12squared*SA1*SA22*dBeta1KanAlter())/(CB*MW*SB2*SW) + (0.10546875D0*CA1*EL*m12squ&
  &ared*SA2*SA3*dBeta1KanAlter())/(CB*MW*SB2*SW) + (0.31640625D0*CA1*CA22*EL*m12squared*SA2*SA3*dBeta1KanAlter())/(CB*MW*SB2*SW) &
  &- (0.38671875D0*CA1*EL*m12squared*SA12*SA2*SA3*dBeta1KanAlter())/(CB*MW*SB2*SW) - (1.16015625D0*CA1*CA22*EL*m12squared*SA12*SA&
  &2*SA3*dBeta1KanAlter())/(CB*MW*SB2*SW) + (0.125D0*CA1*CA3*EL*MH12*dBeta1KanAlter())/(MW*SB*SW*TB) + (0.125D0*CA1*CA22*CA3*EL*M&
  &H12*dBeta1KanAlter())/(MW*SB*SW*TB) + (0.0625D0*CA1*CA3*EL*MH22*dBeta1KanAlter())/(MW*SB*SW*TB) + (0.0625D0*CA1*CA22*CA3*EL*MH&
  &22*dBeta1KanAlter())/(MW*SB*SW*TB) - (0.203125D0*CA3*EL*m12squared*SA1*dBeta1KanAlter())/(MW*SB*SW*TB) - (0.203125D0*CA22*CA3*&
  &EL*m12squared*SA1*dBeta1KanAlter())/(MW*SB*SW*TB) + (0.375D0*CA1*CA3*EL*MH12*SA12*dBeta1KanAlter())/(MW*SB*SW*TB) + (0.375D0*C&
  &A1*CA22*CA3*EL*MH12*SA12*dBeta1KanAlter())/(MW*SB*SW*TB) + (0.1875D0*CA1*CA3*EL*MH22*SA12*dBeta1KanAlter())/(MW*SB*SW*TB) + (0&
  &.1875D0*CA1*CA22*CA3*EL*MH22*SA12*dBeta1KanAlter())/(MW*SB*SW*TB) - (0.125D0*CA1*CA3*EL*MH12*SA22*dBeta1KanAlter())/(MW*SB*SW*&
  &TB) - (0.0625D0*CA1*CA3*EL*MH22*SA22*dBeta1KanAlter())/(MW*SB*SW*TB) + (0.203125D0*CA3*EL*m12squared*SA1*SA22*dBeta1KanAlter()&
  &)/(MW*SB*SW*TB) - (0.375D0*CA1*CA3*EL*MH12*SA12*SA22*dBeta1KanAlter())/(MW*SB*SW*TB) - (0.1875D0*CA1*CA3*EL*MH22*SA12*SA22*dBe&
  &ta1KanAlter())/(MW*SB*SW*TB) - (0.140625D0*CA1*EL*m12squared*SA2*SA3*dBeta1KanAlter())/(MW*SB*SW*TB) - (0.421875D0*CA1*CA22*EL&
  &*m12squared*SA2*SA3*dBeta1KanAlter())/(MW*SB*SW*TB) - (0.1875D0*EL*MH12*SA1*SA2*SA3*dBeta1KanAlter())/(MW*SB*SW*TB) + (0.1875D&
  &0*CA12*EL*MH12*SA1*SA2*SA3*dBeta1KanAlter())/(MW*SB*SW*TB) - (0.5625D0*CA22*EL*MH12*SA1*SA2*SA3*dBeta1KanAlter())/(MW*SB*SW*TB&
  &) + (0.5625D0*CA12*CA22*EL*MH12*SA1*SA2*SA3*dBeta1KanAlter())/(MW*SB*SW*TB) - (0.09375D0*EL*MH22*SA1*SA2*SA3*dBeta1KanAlter())&
  &/(MW*SB*SW*TB) + (0.09375D0*CA12*EL*MH22*SA1*SA2*SA3*dBeta1KanAlter())/(MW*SB*SW*TB) - (0.28125D0*CA22*EL*MH22*SA1*SA2*SA3*dBe&
  &ta1KanAlter())/(MW*SB*SW*TB) + (0.28125D0*CA12*CA22*EL*MH22*SA1*SA2*SA3*dBeta1KanAlter())/(MW*SB*SW*TB) - (0.125D0*CA1*CA3*EL*&
  &m12squared*TB*dBeta1KanAlter())/(CB*MW*SW) - (0.125D0*CA1*CA22*CA3*EL*m12squared*TB*dBeta1KanAlter())/(CB*MW*SW) + (0.0625D0*C&
  &A3*EL*MH12*SA1*TB*dBeta1KanAlter())/(CB*MW*SW) + (0.1875D0*CA12*CA3*EL*MH12*SA1*TB*dBeta1KanAlter())/(CB*MW*SW) + (0.0625D0*CA&
  &22*CA3*EL*MH12*SA1*TB*dBeta1KanAlter())/(CB*MW*SW) + (0.1875D0*CA12*CA22*CA3*EL*MH12*SA1*TB*dBeta1KanAlter())/(CB*MW*SW) + (0.&
  &03125D0*CA3*EL*MH22*SA1*TB*dBeta1KanAlter())/(CB*MW*SW) + (0.09375D0*CA12*CA3*EL*MH22*SA1*TB*dBeta1KanAlter())/(CB*MW*SW) + (0&
  &.03125D0*CA22*CA3*EL*MH22*SA1*TB*dBeta1KanAlter())/(CB*MW*SW) + (0.09375D0*CA12*CA22*CA3*EL*MH22*SA1*TB*dBeta1KanAlter())/(CB*&
  &MW*SW) + (0.125D0*CA1*CA3*EL*m12squared*SA22*TB*dBeta1KanAlter())/(CB*MW*SW) - (0.0625D0*CA3*EL*MH12*SA1*SA22*TB*dBeta1KanAlte&
  &r())/(CB*MW*SW) - (0.1875D0*CA12*CA3*EL*MH12*SA1*SA22*TB*dBeta1KanAlter())/(CB*MW*SW) - (0.03125D0*CA3*EL*MH22*SA1*SA22*TB*dBe&
  &ta1KanAlter())/(CB*MW*SW) - (0.09375D0*CA12*CA3*EL*MH22*SA1*SA22*TB*dBeta1KanAlter())/(CB*MW*SW) + (0.09375D0*CA1*EL*MH12*SA2*&
  &SA3*TB*dBeta1KanAlter())/(CB*MW*SW) + (0.28125D0*CA1*CA22*EL*MH12*SA2*SA3*TB*dBeta1KanAlter())/(CB*MW*SW) + (0.046875D0*CA1*EL&
  &*MH22*SA2*SA3*TB*dBeta1KanAlter())/(CB*MW*SW) + (0.140625D0*CA1*CA22*EL*MH22*SA2*SA3*TB*dBeta1KanAlter())/(CB*MW*SW) + (0.1406&
  &25D0*EL*m12squared*SA1*SA2*SA3*TB*dBeta1KanAlter())/(CB*MW*SW) + (0.421875D0*CA22*EL*m12squared*SA1*SA2*SA3*TB*dBeta1KanAlter(&
  &))/(CB*MW*SW) - (0.09375D0*CA1*EL*MH12*SA12*SA2*SA3*TB*dBeta1KanAlter())/(CB*MW*SW) - (0.28125D0*CA1*CA22*EL*MH12*SA12*SA2*SA3&
  &*TB*dBeta1KanAlter())/(CB*MW*SW) - (0.046875D0*CA1*EL*MH22*SA12*SA2*SA3*TB*dBeta1KanAlter())/(CB*MW*SW) - (0.140625D0*CA1*CA22&
  &*EL*MH22*SA12*SA2*SA3*TB*dBeta1KanAlter())/(CB*MW*SW) - (0.1875D0*CA1*CA3*EL*m12squared*dBeta1KanAlter())/(MW*SB*SW*TB2) - (0.&
  &1875D0*CA1*CA22*CA3*EL*m12squared*dBeta1KanAlter())/(MW*SB*SW*TB2) + (0.1875D0*CA1*CA3*EL*m12squared*SA22*dBeta1KanAlter())/(M&
  &W*SB*SW*TB2) + (0.09375D0*EL*m12squared*SA1*SA2*SA3*dBeta1KanAlter())/(MW*SB*SW*TB2) + (0.28125D0*CA22*EL*m12squared*SA1*SA2*S&
  &A3*dBeta1KanAlter())/(MW*SB*SW*TB2) - (0.03125D0*CA3*EL*m12squared*SA1*TB2*dBeta1KanAlter())/(CB*MW*SW) - (0.03125D0*CA22*CA3*&
  &EL*m12squared*SA1*TB2*dBeta1KanAlter())/(CB*MW*SW) + (0.03125D0*CA3*EL*m12squared*SA1*SA22*TB2*dBeta1KanAlter())/(CB*MW*SW) - &
  &(0.140625D0*CA1*EL*m12squared*SA2*SA3*TB2*dBeta1KanAlter())/(CB*MW*SW) - (0.421875D0*CA1*CA22*EL*m12squared*SA2*SA3*TB2*dBeta1&
  &KanAlter())/(CB*MW*SW) + (0.375D0*CA3*EL*MH12*dAlpha1KanAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.375D0*CA22*CA3*EL*MH12*d&
  &Alpha1KanAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.1875D0*CA3*EL*MH22*dAlpha1KanAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) +&
  & (0.1875D0*CA22*CA3*EL*MH22*dAlpha1KanAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.375D0*CA3*EL*MH12*SA22*dAlpha1KanAlter()*D&
  &BLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.1875D0*CA3*EL*MH22*SA22*dAlpha1KanAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.5625D0*CA&
  &3*EL*m12squared*dAlpha1KanAlter()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.5625D0*CA22*CA3*EL*m12squared*dAlpha1KanAlter()*DB&
  &LE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) + (0.5625D0*CA3*EL*m12squared*SA22*dAlpha1KanAlter()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) -&
  & (0.1875D0*EL*MH12*SA2*SA3*dAlpha1KanAlter()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.5625D0*CA22*EL*MH12*SA2*SA3*dAlpha1KanAlter&
  &()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.09375D0*EL*MH22*SA2*SA3*dAlpha1KanAlter()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.28125&
  &D0*CA22*EL*MH22*SA2*SA3*dAlpha1KanAlter()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) + (0.28125D0*EL*m12squared*SA2*SA3*dAlpha1KanAlter(&
  &)*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.84375D0*CA22*EL*m12squared*SA2*SA3*dAlpha1KanAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW*&
  &SB2*SW) + (0.0625D0*CA2*EL*MH12*SA3*dAlpha2KanAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.03125D0*CA2*EL*MH22*SA3*dAlpha2Kan&
  &Alter()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.5625D0*CA2*EL*MH12*SA22*SA3*dAlpha2KanAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) -&
  & (0.28125D0*CA2*EL*MH22*SA22*SA3*dAlpha2KanAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.5D0*CA2*CA3*EL*MH12*SA2*dAlpha2KanAlt&
  &er()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.25D0*CA2*CA3*EL*MH22*SA2*dAlpha2KanAlter()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.09&
  &375D0*CA2*EL*m12squared*SA3*dAlpha2KanAlter()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) + (0.84375D0*CA2*EL*m12squared*SA22*SA3*dAl&
  &pha2KanAlter()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) + (0.75D0*CA2*CA3*EL*m12squared*SA2*dAlpha2KanAlter()*DBLE(CA1**INT(3.D0))&
  &)/(CB*MW*SB2*SW) + (0.0625D0*CA3*EL*MH12*SA2*dAlpha3KanAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.1875D0*CA22*CA3*EL*MH12*S&
  &A2*dAlpha3KanAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.03125D0*CA3*EL*MH22*SA2*dAlpha3KanAlter()*DBLE(CA1**INT(3.D0)))/(CB&
  &*MW*SW) + (0.09375D0*CA22*CA3*EL*MH22*SA2*dAlpha3KanAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.09375D0*CA3*EL*m12squared*SA&
  &2*dAlpha3KanAlter()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.28125D0*CA22*CA3*EL*m12squared*SA2*dAlpha3KanAlter()*DBLE(CA1**I&
  &NT(3.D0)))/(CB2*MW*SB*SW) - (0.125D0*EL*MH12*SA3*dAlpha3KanAlter()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.125D0*CA22*EL*MH12*SA&
  &3*dAlpha3KanAlter()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.0625D0*EL*MH22*SA3*dAlpha3KanAlter()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW&
  &) - (0.0625D0*CA22*EL*MH22*SA3*dAlpha3KanAlter()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) + (0.125D0*EL*MH12*SA22*SA3*dAlpha3KanAlter(&
  &)*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) + (0.0625D0*EL*MH22*SA22*SA3*dAlpha3KanAlter()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) + (0.1875D0&
  &*EL*m12squared*SA3*dAlpha3KanAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.1875D0*CA22*EL*m12squared*SA3*dAlpha3KanAlter()&
  &*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.1875D0*EL*m12squared*SA22*SA3*dAlpha3KanAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW&
  &) - (0.1875D0*CA3*EL*m12squared*dBeta1KanAlter()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.1875D0*CA22*CA3*EL*m12squared*dBeta&
  &1KanAlter()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) + (0.1875D0*CA3*EL*m12squared*SA22*dBeta1KanAlter()*DBLE(CA1**INT(3.D0)))/(CB&
  &2*MW*SB*SW) - (0.03125D0*EL*MH12*SA2*SA3*dBeta1KanAlter()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.09375D0*CA22*EL*MH12*SA2*SA3*d&
  &Beta1KanAlter()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) + (0.03125D0*EL*MH12*SA2*SA3*dBeta1KanAlter()*DBLE(CA1**INT(3.D0)))/(CB2*MW*S&
  &B*SW) + (0.09375D0*CA22*EL*MH12*SA2*SA3*dBeta1KanAlter()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.015625D0*EL*MH22*SA2*SA3*dB&
  &eta1KanAlter()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.046875D0*CA22*EL*MH22*SA2*SA3*dBeta1KanAlter()*DBLE(CA1**INT(3.D0)))/(MW*&
  &SB*SW) + (0.015625D0*EL*MH22*SA2*SA3*dBeta1KanAlter()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) + (0.046875D0*CA22*EL*MH22*SA2*SA3*&
  &dBeta1KanAlter()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) + (0.12890625D0*EL*m12squared*SA2*SA3*dBeta1KanAlter()*DBLE(CA1**INT(3.D&
  &0)))/(CB*MW*SB2*SW) + (0.38671875D0*CA22*EL*m12squared*SA2*SA3*dBeta1KanAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.125D&
  &0*CA3*EL*MH12*dBeta1KanAlter()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW*TB) - (0.125D0*CA22*CA3*EL*MH12*dBeta1KanAlter()*DBLE(CA1**INT(&
  &3.D0)))/(MW*SB*SW*TB) - (0.0625D0*CA3*EL*MH22*dBeta1KanAlter()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW*TB) - (0.0625D0*CA22*CA3*EL*MH2&
  &2*dBeta1KanAlter()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW*TB) + (0.125D0*CA3*EL*MH12*SA22*dBeta1KanAlter()*DBLE(CA1**INT(3.D0)))/(MW*&
  &SB*SW*TB) + (0.0625D0*CA3*EL*MH22*SA22*dBeta1KanAlter()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW*TB) + (0.03125D0*EL*MH12*SA2*SA3*TB*dB&
  &eta1KanAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.09375D0*CA22*EL*MH12*SA2*SA3*TB*dBeta1KanAlter()*DBLE(CA1**INT(3.D0)))/(C&
  &B*MW*SW) + (0.015625D0*EL*MH22*SA2*SA3*TB*dBeta1KanAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.046875D0*CA22*EL*MH22*SA2*SA3&
  &*TB*dBeta1KanAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.5625D0*CA1*EL*MH12*SA3*dAlpha2KanAlter()*DBLE(CA2**INT(3.D0)))/(CB*&
  &MW*SW) + (0.28125D0*CA1*EL*MH22*SA3*dAlpha2KanAlter()*DBLE(CA2**INT(3.D0)))/(CB*MW*SW) + (0.84375D0*EL*m12squared*SA1*SA3*dAlp&
  &ha2KanAlter()*DBLE(CA2**INT(3.D0)))/(CB*MW*SW) - (0.5625D0*CA1*EL*MH12*SA12*SA3*dAlpha2KanAlter()*DBLE(CA2**INT(3.D0)))/(CB*MW&
  &*SW) - (0.28125D0*CA1*EL*MH22*SA12*SA3*dAlpha2KanAlter()*DBLE(CA2**INT(3.D0)))/(CB*MW*SW) + (0.84375D0*CA1*EL*m12squared*SA3*d&
  &Alpha2KanAlter()*DBLE(CA2**INT(3.D0)))/(MW*SB*SW) - (0.5625D0*CA1*EL*m12squared*SA3*dAlpha2KanAlter()*DBLE(CA2**INT(3.D0)))/(C&
  &B2*MW*SB*SW) + (0.5625D0*EL*MH12*SA1*SA3*dAlpha2KanAlter()*DBLE(CA2**INT(3.D0)))/(MW*SB*SW) - (0.5625D0*CA12*EL*MH12*SA1*SA3*d&
  &Alpha2KanAlter()*DBLE(CA2**INT(3.D0)))/(MW*SB*SW) + (0.28125D0*EL*MH22*SA1*SA3*dAlpha2KanAlter()*DBLE(CA2**INT(3.D0)))/(MW*SB*&
  &SW) - (0.28125D0*CA12*EL*MH22*SA1*SA3*dAlpha2KanAlter()*DBLE(CA2**INT(3.D0)))/(MW*SB*SW) + (0.84375D0*CA1*EL*m12squared*SA12*S&
  &A3*dAlpha2KanAlter()*DBLE(CA2**INT(3.D0)))/(CB2*MW*SB*SW) - (0.5625D0*EL*m12squared*SA1*SA3*dAlpha2KanAlter()*DBLE(CA2**INT(3.&
  &D0)))/(CB*MW*SB2*SW) + (0.84375D0*CA12*EL*m12squared*SA1*SA3*dAlpha2KanAlter()*DBLE(CA2**INT(3.D0)))/(CB*MW*SB2*SW) - (0.28125&
  &D0*EL*m12squared*SA1*SA3*dAlpha2KanAlter()*DBLE(CA2**INT(3.D0)))/(MW*SB*SW*TB) - (0.28125D0*CA1*EL*m12squared*SA3*TB*dAlpha2Ka&
  &nAlter()*DBLE(CA2**INT(3.D0)))/(CB*MW*SW) + (0.5D0*CA3*MH12*dAlpha3KanAlter()*DBLE(CA2**INT(3.D0)))/vS + (0.25D0*CA3*MH22*dAlp&
  &ha3KanAlter()*DBLE(CA2**INT(3.D0)))/vS + (0.1875D0*EL*MH12*SA3*dAlpha2KanAlter()*DBLE(CA1**INT(3.D0))*DBLE(CA2**INT(3.D0)))/(C&
  &B*MW*SW) + (0.09375D0*EL*MH22*SA3*dAlpha2KanAlter()*DBLE(CA1**INT(3.D0))*DBLE(CA2**INT(3.D0)))/(CB*MW*SW) - (0.28125D0*EL*m12s&
  &quared*SA3*dAlpha2KanAlter()*DBLE(CA1**INT(3.D0))*DBLE(CA2**INT(3.D0)))/(CB2*MW*SB*SW) - (0.28125D0*CA3*EL*m12squared*SA1*dBet&
  &a1KanAlter()*DBLE(CB**INT(-3.D0)))/(MW*SW) - (0.84375D0*CA12*CA3*EL*m12squared*SA1*dBeta1KanAlter()*DBLE(CB**INT(-3.D0)))/(MW*&
  &SW) - (0.28125D0*CA22*CA3*EL*m12squared*SA1*dBeta1KanAlter()*DBLE(CB**INT(-3.D0)))/(MW*SW) - (0.84375D0*CA12*CA22*CA3*EL*m12sq&
  &uared*SA1*dBeta1KanAlter()*DBLE(CB**INT(-3.D0)))/(MW*SW) + (0.28125D0*CA3*EL*m12squared*SA1*SA22*dBeta1KanAlter()*DBLE(CB**INT&
  &(-3.D0)))/(MW*SW) + (0.84375D0*CA12*CA3*EL*m12squared*SA1*SA22*dBeta1KanAlter()*DBLE(CB**INT(-3.D0)))/(MW*SW) - (0.36328125D0*&
  &CA1*EL*m12squared*SA2*SA3*dBeta1KanAlter()*DBLE(CB**INT(-3.D0)))/(MW*SW) - (1.08984375D0*CA1*CA22*EL*m12squared*SA2*SA3*dBeta1&
  &KanAlter()*DBLE(CB**INT(-3.D0)))/(MW*SW) + (0.45703125D0*CA1*EL*m12squared*SA12*SA2*SA3*dBeta1KanAlter()*DBLE(CB**INT(-3.D0)))&
  &/(MW*SW) + (1.37109375D0*CA1*CA22*EL*m12squared*SA12*SA2*SA3*dBeta1KanAlter()*DBLE(CB**INT(-3.D0)))/(MW*SW) - (0.0625D0*CA3*EL&
  &*m12squared*SA1*dBeta1KanAlter()*DBLE(CB**INT(-3.D0)))/(MW*SB2*SW) - (0.28125D0*CA12*CA3*EL*m12squared*SA1*dBeta1KanAlter()*DB&
  &LE(CB**INT(-3.D0)))/(MW*SB2*SW) - (0.0625D0*CA22*CA3*EL*m12squared*SA1*dBeta1KanAlter()*DBLE(CB**INT(-3.D0)))/(MW*SB2*SW) - (0&
  &.28125D0*CA12*CA22*CA3*EL*m12squared*SA1*dBeta1KanAlter()*DBLE(CB**INT(-3.D0)))/(MW*SB2*SW) + (0.0625D0*CA3*EL*m12squared*SA1*&
  &SA22*dBeta1KanAlter()*DBLE(CB**INT(-3.D0)))/(MW*SB2*SW) + (0.28125D0*CA12*CA3*EL*m12squared*SA1*SA22*dBeta1KanAlter()*DBLE(CB*&
  &*INT(-3.D0)))/(MW*SB2*SW) - (0.05859375D0*CA1*EL*m12squared*SA2*SA3*dBeta1KanAlter()*DBLE(CB**INT(-3.D0)))/(MW*SB2*SW) - (0.17&
  &578125D0*CA1*CA22*EL*m12squared*SA2*SA3*dBeta1KanAlter()*DBLE(CB**INT(-3.D0)))/(MW*SB2*SW) + (0.10546875D0*CA1*EL*m12squared*S&
  &A12*SA2*SA3*dBeta1KanAlter()*DBLE(CB**INT(-3.D0)))/(MW*SB2*SW) + (0.31640625D0*CA1*CA22*EL*m12squared*SA12*SA2*SA3*dBeta1KanAl&
  &ter()*DBLE(CB**INT(-3.D0)))/(MW*SB2*SW) - (0.15234375D0*EL*m12squared*SA2*SA3*dBeta1KanAlter()*DBLE(CA1**INT(3.D0))*DBLE(CB**I&
  &NT(-3.D0)))/(MW*SW) - (0.45703125D0*CA22*EL*m12squared*SA2*SA3*dBeta1KanAlter()*DBLE(CA1**INT(3.D0))*DBLE(CB**INT(-3.D0)))/(MW&
  &*SW) - (0.03515625D0*EL*m12squared*SA2*SA3*dBeta1KanAlter()*DBLE(CA1**INT(3.D0))*DBLE(CB**INT(-3.D0)))/(MW*SB2*SW) - (0.105468&
  &75D0*CA22*EL*m12squared*SA2*SA3*dBeta1KanAlter()*DBLE(CA1**INT(3.D0))*DBLE(CB**INT(-3.D0)))/(MW*SB2*SW) + (0.1875D0*EL*MH12*SA&
  &2*SA3*dAlpha1KanAlter()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.5625D0*CA22*EL*MH12*SA2*SA3*dAlpha1KanAlter()*DBLE(SA1**INT(3.D0&
  &)))/(CB*MW*SW) + (0.09375D0*EL*MH22*SA2*SA3*dAlpha1KanAlter()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.28125D0*CA22*EL*MH22*SA2*S&
  &A3*dAlpha1KanAlter()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.375D0*CA3*EL*MH12*dAlpha1KanAlter()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW&
  &) + (0.375D0*CA22*CA3*EL*MH12*dAlpha1KanAlter()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.1875D0*CA3*EL*MH22*dAlpha1KanAlter()*DBL&
  &E(SA1**INT(3.D0)))/(MW*SB*SW) + (0.1875D0*CA22*CA3*EL*MH22*dAlpha1KanAlter()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0.375D0*CA3*E&
  &L*MH12*SA22*dAlpha1KanAlter()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0.1875D0*CA3*EL*MH22*SA22*dAlpha1KanAlter()*DBLE(SA1**INT(3.&
  &D0)))/(MW*SB*SW) - (0.28125D0*EL*m12squared*SA2*SA3*dAlpha1KanAlter()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.84375D0*CA22*E&
  &L*m12squared*SA2*SA3*dAlpha1KanAlter()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.5625D0*CA3*EL*m12squared*dAlpha1KanAlter()*DB&
  &LE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.5625D0*CA22*CA3*EL*m12squared*dAlpha1KanAlter()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) +&
  & (0.5625D0*CA3*EL*m12squared*SA22*dAlpha1KanAlter()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.5D0*CA2*CA3*EL*MH12*SA2*dAlpha2K&
  &anAlter()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.25D0*CA2*CA3*EL*MH22*SA2*dAlpha2KanAlter()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) - &
  &(0.75D0*CA2*CA3*EL*m12squared*SA2*dAlpha2KanAlter()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) + (0.0625D0*CA2*EL*MH12*SA3*dAlpha2Ka&
  &nAlter()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.03125D0*CA2*EL*MH22*SA3*dAlpha2KanAlter()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0&
  &.5625D0*CA2*EL*MH12*SA22*SA3*dAlpha2KanAlter()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0.28125D0*CA2*EL*MH22*SA22*SA3*dAlpha2KanAl&
  &ter()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0.09375D0*CA2*EL*m12squared*SA3*dAlpha2KanAlter()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*S&
  &W) + (0.84375D0*CA2*EL*m12squared*SA22*SA3*dAlpha2KanAlter()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.125D0*EL*MH12*SA3*dAlph&
  &a3KanAlter()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.125D0*CA22*EL*MH12*SA3*dAlpha3KanAlter()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) +&
  & (0.0625D0*EL*MH22*SA3*dAlpha3KanAlter()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.0625D0*CA22*EL*MH22*SA3*dAlpha3KanAlter()*DBLE(&
  &SA1**INT(3.D0)))/(CB*MW*SW) - (0.125D0*EL*MH12*SA22*SA3*dAlpha3KanAlter()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) - (0.0625D0*EL*MH22&
  &*SA22*SA3*dAlpha3KanAlter()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.0625D0*CA3*EL*MH12*SA2*dAlpha3KanAlter()*DBLE(SA1**INT(3.D0)&
  &))/(MW*SB*SW) + (0.1875D0*CA22*CA3*EL*MH12*SA2*dAlpha3KanAlter()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.03125D0*CA3*EL*MH22*SA2&
  &*dAlpha3KanAlter()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.09375D0*CA22*CA3*EL*MH22*SA2*dAlpha3KanAlter()*DBLE(SA1**INT(3.D0)))/&
  &(MW*SB*SW) - (0.1875D0*EL*m12squared*SA3*dAlpha3KanAlter()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.1875D0*CA22*EL*m12squared&
  &*SA3*dAlpha3KanAlter()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) + (0.1875D0*EL*m12squared*SA22*SA3*dAlpha3KanAlter()*DBLE(SA1**INT&
  &(3.D0)))/(CB2*MW*SB*SW) - (0.09375D0*CA3*EL*m12squared*SA2*dAlpha3KanAlter()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.28125D0&
  &*CA22*CA3*EL*m12squared*SA2*dAlpha3KanAlter()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.0625D0*CA3*EL*MH12*dBeta1KanAlter()*DB&
  &LE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.0625D0*CA22*CA3*EL*MH12*dBeta1KanAlter()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0.0625D0*CA3*&
  &EL*MH12*dBeta1KanAlter()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.0625D0*CA22*CA3*EL*MH12*dBeta1KanAlter()*DBLE(SA1**INT(3.D0&
  &)))/(CB2*MW*SB*SW) + (0.03125D0*CA3*EL*MH22*dBeta1KanAlter()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.03125D0*CA22*CA3*EL*MH22*dB&
  &eta1KanAlter()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0.03125D0*CA3*EL*MH22*dBeta1KanAlter()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW)&
  & - (0.03125D0*CA22*CA3*EL*MH22*dBeta1KanAlter()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.0625D0*CA3*EL*MH12*SA22*dBeta1KanAlt&
  &er()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.0625D0*CA3*EL*MH12*SA22*dBeta1KanAlter()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.&
  &03125D0*CA3*EL*MH22*SA22*dBeta1KanAlter()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.03125D0*CA3*EL*MH22*SA22*dBeta1KanAlter()*DBLE&
  &(SA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.09375D0*EL*m12squared*SA2*SA3*dBeta1KanAlter()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0&
  &.28125D0*CA22*EL*m12squared*SA2*SA3*dBeta1KanAlter()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.28125D0*CA3*EL*m12squared*dBeta&
  &1KanAlter()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.28125D0*CA22*CA3*EL*m12squared*dBeta1KanAlter()*DBLE(SA1**INT(3.D0)))/(C&
  &B*MW*SB2*SW) + (0.28125D0*CA3*EL*m12squared*SA22*dBeta1KanAlter()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.0625D0*EL*MH12*SA2&
  &*SA3*dBeta1KanAlter()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW*TB) - (0.1875D0*CA22*EL*MH12*SA2*SA3*dBeta1KanAlter()*DBLE(SA1**INT(3.D0&
  &)))/(MW*SB*SW*TB) - (0.03125D0*EL*MH22*SA2*SA3*dBeta1KanAlter()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW*TB) - (0.09375D0*CA22*EL*MH22*&
  &SA2*SA3*dBeta1KanAlter()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW*TB) - (0.0625D0*CA3*EL*MH12*TB*dBeta1KanAlter()*DBLE(SA1**INT(3.D0)))&
  &/(CB*MW*SW) - (0.0625D0*CA22*CA3*EL*MH12*TB*dBeta1KanAlter()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) - (0.03125D0*CA3*EL*MH22*TB*dBet&
  &a1KanAlter()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) - (0.03125D0*CA22*CA3*EL*MH22*TB*dBeta1KanAlter()*DBLE(SA1**INT(3.D0)))/(CB*MW*S&
  &W) + (0.0625D0*CA3*EL*MH12*SA22*TB*dBeta1KanAlter()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.03125D0*CA3*EL*MH22*SA22*TB*dBeta1Ka&
  &nAlter()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.1875D0*EL*MH12*SA3*dAlpha2KanAlter()*DBLE(CA2**INT(3.D0))*DBLE(SA1**INT(3.D0)))&
  &/(MW*SB*SW) + (0.09375D0*EL*MH22*SA3*dAlpha2KanAlter()*DBLE(CA2**INT(3.D0))*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0.28125D0*EL*m&
  &12squared*SA3*dAlpha2KanAlter()*DBLE(CA2**INT(3.D0))*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.28125D0*CA3*EL*m12squared*dBeta&
  &1KanAlter()*DBLE(CB**INT(-3.D0))*DBLE(SA1**INT(3.D0)))/(MW*SW) + (0.28125D0*CA22*CA3*EL*m12squared*dBeta1KanAlter()*DBLE(CB**I&
  &NT(-3.D0))*DBLE(SA1**INT(3.D0)))/(MW*SW) - (0.28125D0*CA3*EL*m12squared*SA22*dBeta1KanAlter()*DBLE(CB**INT(-3.D0))*DBLE(SA1**I&
  &NT(3.D0)))/(MW*SW) + (0.09375D0*CA3*EL*m12squared*dBeta1KanAlter()*DBLE(CB**INT(-3.D0))*DBLE(SA1**INT(3.D0)))/(MW*SB2*SW) + (0&
  &.09375D0*CA22*CA3*EL*m12squared*dBeta1KanAlter()*DBLE(CB**INT(-3.D0))*DBLE(SA1**INT(3.D0)))/(MW*SB2*SW) - (0.09375D0*CA3*EL*m1&
  &2squared*SA22*dBeta1KanAlter()*DBLE(CB**INT(-3.D0))*DBLE(SA1**INT(3.D0)))/(MW*SB2*SW) - (0.28125D0*CA1*EL*m12squared*SA3*dAlph&
  &a1KanAlter()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.1875D0*EL*MH12*SA1*SA3*dAlpha1KanAlter()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) +&
  & (0.5625D0*CA12*EL*MH12*SA1*SA3*dAlpha1KanAlter()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.09375D0*EL*MH22*SA1*SA3*dAlpha1KanAlte&
  &r()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.28125D0*CA12*EL*MH22*SA1*SA3*dAlpha1KanAlter()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0&
  &.1875D0*CA1*EL*MH12*SA3*dAlpha1KanAlter()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.09375D0*CA1*EL*MH22*SA3*dAlpha1KanAlter()*DBLE&
  &(SA2**INT(3.D0)))/(MW*SB*SW) + (0.28125D0*EL*m12squared*SA1*SA3*dAlpha1KanAlter()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.1875D0&
  &*EL*m12squared*SA1*SA3*dAlpha1KanAlter()*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) - (0.84375D0*CA12*EL*m12squared*SA1*SA3*dAlpha1K&
  &anAlter()*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) - (0.5625D0*CA1*EL*MH12*SA12*SA3*dAlpha1KanAlter()*DBLE(SA2**INT(3.D0)))/(MW*SB&
  &*SW) - (0.28125D0*CA1*EL*MH22*SA12*SA3*dAlpha1KanAlter()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) + (0.1875D0*CA1*EL*m12squared*SA3*dA&
  &lpha1KanAlter()*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) + (0.84375D0*CA1*EL*m12squared*SA12*SA3*dAlpha1KanAlter()*DBLE(SA2**INT(3&
  &.D0)))/(CB*MW*SB2*SW) + (0.09375D0*CA1*EL*m12squared*SA3*dAlpha1KanAlter()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) - (0.09375D0*EL&
  &*m12squared*SA1*SA3*TB*dAlpha1KanAlter()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (1.5D0*MH12*SA3*dAlpha2KanAlter()*DBLE(SA2**INT(3.&
  &D0)))/vS + (0.75D0*MH22*SA3*dAlpha2KanAlter()*DBLE(SA2**INT(3.D0)))/vS - (0.1875D0*CA1*CA3*EL*MH12*dAlpha3KanAlter()*DBLE(SA2*&
  &*INT(3.D0)))/(CB*MW*SW) - (0.09375D0*CA1*CA3*EL*MH22*dAlpha3KanAlter()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.28125D0*CA3*EL*m1&
  &2squared*SA1*dAlpha3KanAlter()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.1875D0*CA1*CA3*EL*MH12*SA12*dAlpha3KanAlter()*DBLE(SA2**I&
  &NT(3.D0)))/(CB*MW*SW) + (0.09375D0*CA1*CA3*EL*MH22*SA12*dAlpha3KanAlter()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.28125D0*CA1*CA&
  &3*EL*m12squared*dAlpha3KanAlter()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) + (0.1875D0*CA1*CA3*EL*m12squared*dAlpha3KanAlter()*DBLE(SA&
  &2**INT(3.D0)))/(CB2*MW*SB*SW) - (0.1875D0*CA3*EL*MH12*SA1*dAlpha3KanAlter()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) + (0.1875D0*CA12*&
  &CA3*EL*MH12*SA1*dAlpha3KanAlter()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.09375D0*CA3*EL*MH22*SA1*dAlpha3KanAlter()*DBLE(SA2**IN&
  &T(3.D0)))/(MW*SB*SW) + (0.09375D0*CA12*CA3*EL*MH22*SA1*dAlpha3KanAlter()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.28125D0*CA1*CA3&
  &*EL*m12squared*SA12*dAlpha3KanAlter()*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) + (0.1875D0*CA3*EL*m12squared*SA1*dAlpha3KanAlter()&
  &*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) - (0.28125D0*CA12*CA3*EL*m12squared*SA1*dAlpha3KanAlter()*DBLE(SA2**INT(3.D0)))/(CB*MW*S&
  &B2*SW) + (0.09375D0*CA3*EL*m12squared*SA1*dAlpha3KanAlter()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) + (0.09375D0*CA1*CA3*EL*m12squ&
  &ared*TB*dAlpha3KanAlter()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.09375D0*CA1*EL*m12squared*SA3*dBeta1KanAlter()*DBLE(SA2**INT(3&
  &.D0)))/(CB*MW*SW) + (0.09375D0*CA1*EL*MH12*SA3*dBeta1KanAlter()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.09375D0*CA1*EL*MH12*SA3*&
  &dBeta1KanAlter()*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) + (0.046875D0*CA1*EL*MH22*SA3*dBeta1KanAlter()*DBLE(SA2**INT(3.D0)))/(MW&
  &*SB*SW) - (0.046875D0*CA1*EL*MH22*SA3*dBeta1KanAlter()*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) + (0.140625D0*EL*m12squared*SA1*SA&
  &3*dBeta1KanAlter()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) + (0.046875D0*EL*m12squared*SA1*SA3*dBeta1KanAlter()*DBLE(SA2**INT(3.D0)))&
  &/(CB2*MW*SB*SW) - (0.28125D0*CA12*EL*m12squared*SA1*SA3*dBeta1KanAlter()*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) - (0.09375D0*CA1&
  &*EL*MH12*SA12*SA3*dBeta1KanAlter()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) + (0.09375D0*CA1*EL*MH12*SA12*SA3*dBeta1KanAlter()*DBLE(SA&
  &2**INT(3.D0)))/(CB2*MW*SB*SW) - (0.046875D0*CA1*EL*MH22*SA12*SA3*dBeta1KanAlter()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) + (0.046875&
  &D0*CA1*EL*MH22*SA12*SA3*dBeta1KanAlter()*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) - (0.10546875D0*CA1*EL*m12squared*SA3*dBeta1KanA&
  &lter()*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) + (0.38671875D0*CA1*EL*m12squared*SA12*SA3*dBeta1KanAlter()*DBLE(SA2**INT(3.D0)))/&
  &(CB*MW*SB2*SW) + (0.140625D0*CA1*EL*m12squared*SA3*dBeta1KanAlter()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) + (0.1875D0*EL*MH12*SA&
  &1*SA3*dBeta1KanAlter()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) - (0.1875D0*CA12*EL*MH12*SA1*SA3*dBeta1KanAlter()*DBLE(SA2**INT(3.D&
  &0)))/(MW*SB*SW*TB) + (0.09375D0*EL*MH22*SA1*SA3*dBeta1KanAlter()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) - (0.09375D0*CA12*EL*MH22&
  &*SA1*SA3*dBeta1KanAlter()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) - (0.09375D0*CA1*EL*MH12*SA3*TB*dBeta1KanAlter()*DBLE(SA2**INT(3&
  &.D0)))/(CB*MW*SW) - (0.046875D0*CA1*EL*MH22*SA3*TB*dBeta1KanAlter()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.140625D0*EL*m12squar&
  &ed*SA1*SA3*TB*dBeta1KanAlter()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.09375D0*CA1*EL*MH12*SA12*SA3*TB*dBeta1KanAlter()*DBLE(SA2&
  &**INT(3.D0)))/(CB*MW*SW) + (0.046875D0*CA1*EL*MH22*SA12*SA3*TB*dBeta1KanAlter()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.09375D0*&
  &EL*m12squared*SA1*SA3*dBeta1KanAlter()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB2) + (0.140625D0*CA1*EL*m12squared*SA3*TB2*dBeta1KanA&
  &lter()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.1875D0*EL*MH12*SA3*dAlpha1KanAlter()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(&
  &MW*SB*SW) + (0.09375D0*EL*MH22*SA3*dAlpha1KanAlter()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.28125D0*EL*m12&
  &squared*SA3*dAlpha1KanAlter()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) - (0.0625D0*CA3*EL*MH12*dAlpha3KanAlte&
  &r()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.03125D0*CA3*EL*MH22*dAlpha3KanAlter()*DBLE(CA1**INT(3.D0))*DBLE&
  &(SA2**INT(3.D0)))/(CB*MW*SW) + (0.09375D0*CA3*EL*m12squared*dAlpha3KanAlter()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB2*&
  &MW*SB*SW) + (0.03125D0*EL*MH12*SA3*dBeta1KanAlter()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.03125D0*EL*MH12&
  &*SA3*dBeta1KanAlter()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) + (0.015625D0*EL*MH22*SA3*dBeta1KanAlter()*DBL&
  &E(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.015625D0*EL*MH22*SA3*dBeta1KanAlter()*DBLE(CA1**INT(3.D0))*DBLE(SA2**I&
  &NT(3.D0)))/(CB2*MW*SB*SW) - (0.12890625D0*EL*m12squared*SA3*dBeta1KanAlter()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW&
  &*SB2*SW) - (0.03125D0*EL*MH12*SA3*TB*dBeta1KanAlter()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.015625D0*EL*M&
  &H22*SA3*TB*dBeta1KanAlter()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.36328125D0*CA1*EL*m12squared*SA3*dBeta1&
  &KanAlter()*DBLE(CB**INT(-3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SW) - (0.45703125D0*CA1*EL*m12squared*SA12*SA3*dBeta1KanAlter()*DBLE&
  &(CB**INT(-3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SW) + (0.05859375D0*CA1*EL*m12squared*SA3*dBeta1KanAlter()*DBLE(CB**INT(-3.D0))*DBL&
  &E(SA2**INT(3.D0)))/(MW*SB2*SW) - (0.10546875D0*CA1*EL*m12squared*SA12*SA3*dBeta1KanAlter()*DBLE(CB**INT(-3.D0))*DBLE(SA2**INT(&
  &3.D0)))/(MW*SB2*SW) + (0.15234375D0*EL*m12squared*SA3*dBeta1KanAlter()*DBLE(CA1**INT(3.D0))*DBLE(CB**INT(-3.D0))*DBLE(SA2**INT&
  &(3.D0)))/(MW*SW) + (0.03515625D0*EL*m12squared*SA3*dBeta1KanAlter()*DBLE(CA1**INT(3.D0))*DBLE(CB**INT(-3.D0))*DBLE(SA2**INT(3.&
  &D0)))/(MW*SB2*SW) - (0.1875D0*EL*MH12*SA3*dAlpha1KanAlter()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.09375D0&
  &*EL*MH22*SA3*dAlpha1KanAlter()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.28125D0*EL*m12squared*SA3*dAlpha1Kan&
  &Alter()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) - (0.0625D0*CA3*EL*MH12*dAlpha3KanAlter()*DBLE(SA1**INT(3.D0&
  &))*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.03125D0*CA3*EL*MH22*dAlpha3KanAlter()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(MW*&
  &SB*SW) + (0.09375D0*CA3*EL*m12squared*dAlpha3KanAlter()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) + (0.09375D0&
  &*EL*m12squared*SA3*dBeta1KanAlter()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) + (0.0625D0*EL*MH12*SA3*dBeta1Ka&
  &nAlter()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) + (0.03125D0*EL*MH22*SA3*dBeta1KanAlter()*DBLE(SA1**INT(3.D0&
  &))*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) - (0.1875D0*CA1*CA3*EL*m12squared*dBeta1KanAlter()*DBLE(SB**INT(-3.D0)))/(MW*SW) - (0.1&
  &875D0*CA1*CA22*CA3*EL*m12squared*dBeta1KanAlter()*DBLE(SB**INT(-3.D0)))/(MW*SW) - (1.125D0*CA1*CA3*EL*m12squared*SA12*dBeta1Ka&
  &nAlter()*DBLE(SB**INT(-3.D0)))/(MW*SW) - (1.125D0*CA1*CA22*CA3*EL*m12squared*SA12*dBeta1KanAlter()*DBLE(SB**INT(-3.D0)))/(MW*S&
  &W) + (0.1875D0*CA1*CA3*EL*m12squared*SA22*dBeta1KanAlter()*DBLE(SB**INT(-3.D0)))/(MW*SW) + (1.125D0*CA1*CA3*EL*m12squared*SA12&
  &*SA22*dBeta1KanAlter()*DBLE(SB**INT(-3.D0)))/(MW*SW) + (0.46875D0*EL*m12squared*SA1*SA2*SA3*dBeta1KanAlter()*DBLE(SB**INT(-3.D&
  &0)))/(MW*SW) - (0.5625D0*CA12*EL*m12squared*SA1*SA2*SA3*dBeta1KanAlter()*DBLE(SB**INT(-3.D0)))/(MW*SW) + (1.40625D0*CA22*EL*m1&
  &2squared*SA1*SA2*SA3*dBeta1KanAlter()*DBLE(SB**INT(-3.D0)))/(MW*SW) - (1.6875D0*CA12*CA22*EL*m12squared*SA1*SA2*SA3*dBeta1KanA&
  &lter()*DBLE(SB**INT(-3.D0)))/(MW*SW) + (0.375D0*CA3*EL*m12squared*dBeta1KanAlter()*DBLE(CA1**INT(3.D0))*DBLE(SB**INT(-3.D0)))/&
  &(MW*SW) + (0.375D0*CA22*CA3*EL*m12squared*dBeta1KanAlter()*DBLE(CA1**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(MW*SW) - (0.375D0*CA3*E&
  &L*m12squared*SA22*dBeta1KanAlter()*DBLE(CA1**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(MW*SW) + (0.1875D0*EL*m12squared*SA2*SA3*dBeta1&
  &KanAlter()*DBLE(SA1**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(MW*SW) + (0.5625D0*CA22*EL*m12squared*SA2*SA3*dBeta1KanAlter()*DBLE(SA1&
  &**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(MW*SW) - (0.46875D0*EL*m12squared*SA1*SA3*dBeta1KanAlter()*DBLE(SA2**INT(3.D0))*DBLE(SB**I&
  &NT(-3.D0)))/(MW*SW) + (0.5625D0*CA12*EL*m12squared*SA1*SA3*dBeta1KanAlter()*DBLE(SA2**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(MW*SW)&
  & - (0.1875D0*EL*m12squared*SA3*dBeta1KanAlter()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(MW*SW) - (0.1&
  &25D0*CA1*CA3*m12squared*dgAtMZ())/(CB*MW) - (0.125D0*CA1*CA22*CA3*m12squared*dgAtMZ())/(CB*MW) + (0.125D0*CA3*MH12*SA1*dgAtMZ(&
  &))/(CB*MW) + (0.375D0*CA12*CA3*MH12*SA1*dgAtMZ())/(CB*MW) + (0.125D0*CA22*CA3*MH12*SA1*dgAtMZ())/(CB*MW) + (0.375D0*CA12*CA22*&
  &CA3*MH12*SA1*dgAtMZ())/(CB*MW) + (0.0625D0*CA3*MH22*SA1*dgAtMZ())/(CB*MW) + (0.1875D0*CA12*CA3*MH22*SA1*dgAtMZ())/(CB*MW) + (0&
  &.0625D0*CA22*CA3*MH22*SA1*dgAtMZ())/(CB*MW) + (0.1875D0*CA12*CA22*CA3*MH22*SA1*dgAtMZ())/(CB*MW) + (0.125D0*CA1*CA3*m12squared&
  &*SA22*dgAtMZ())/(CB*MW) - (0.125D0*CA3*MH12*SA1*SA22*dgAtMZ())/(CB*MW) - (0.375D0*CA12*CA3*MH12*SA1*SA22*dgAtMZ())/(CB*MW) - (&
  &0.0625D0*CA3*MH22*SA1*SA22*dgAtMZ())/(CB*MW) - (0.1875D0*CA12*CA3*MH22*SA1*SA22*dgAtMZ())/(CB*MW) + (0.1875D0*CA1*MH12*SA2*SA3&
  &*dgAtMZ())/(CB*MW) + (0.5625D0*CA1*CA22*MH12*SA2*SA3*dgAtMZ())/(CB*MW) + (0.09375D0*CA1*MH22*SA2*SA3*dgAtMZ())/(CB*MW) + (0.28&
  &125D0*CA1*CA22*MH22*SA2*SA3*dgAtMZ())/(CB*MW) + (0.28125D0*m12squared*SA1*SA2*SA3*dgAtMZ())/(CB*MW) + (0.84375D0*CA22*m12squar&
  &ed*SA1*SA2*SA3*dgAtMZ())/(CB*MW) - (0.1875D0*CA1*MH12*SA12*SA2*SA3*dgAtMZ())/(CB*MW) - (0.5625D0*CA1*CA22*MH12*SA12*SA2*SA3*dg&
  &AtMZ())/(CB*MW) - (0.09375D0*CA1*MH22*SA12*SA2*SA3*dgAtMZ())/(CB*MW) - (0.28125D0*CA1*CA22*MH22*SA12*SA2*SA3*dgAtMZ())/(CB*MW)&
  & - (0.125D0*CA1*CA3*MH12*dgAtMZ())/(MW*SB) - (0.125D0*CA1*CA22*CA3*MH12*dgAtMZ())/(MW*SB) - (0.0625D0*CA1*CA3*MH22*dgAtMZ())/(&
  &MW*SB) - (0.0625D0*CA1*CA22*CA3*MH22*dgAtMZ())/(MW*SB) + (0.21875D0*CA3*m12squared*SA1*dgAtMZ())/(MW*SB) + (0.21875D0*CA22*CA3&
  &*m12squared*SA1*dgAtMZ())/(MW*SB) - (0.15625D0*CA3*m12squared*SA1*dgAtMZ())/(CB2*MW*SB) - (0.5625D0*CA12*CA3*m12squared*SA1*dg&
  &AtMZ())/(CB2*MW*SB) - (0.15625D0*CA22*CA3*m12squared*SA1*dgAtMZ())/(CB2*MW*SB) - (0.5625D0*CA12*CA22*CA3*m12squared*SA1*dgAtMZ&
  &())/(CB2*MW*SB) - (0.375D0*CA1*CA3*MH12*SA12*dgAtMZ())/(MW*SB) - (0.375D0*CA1*CA22*CA3*MH12*SA12*dgAtMZ())/(MW*SB) - (0.1875D0&
  &*CA1*CA3*MH22*SA12*dgAtMZ())/(MW*SB) - (0.1875D0*CA1*CA22*CA3*MH22*SA12*dgAtMZ())/(MW*SB) + (0.125D0*CA1*CA3*MH12*SA22*dgAtMZ(&
  &))/(MW*SB) + (0.0625D0*CA1*CA3*MH22*SA22*dgAtMZ())/(MW*SB) - (0.21875D0*CA3*m12squared*SA1*SA22*dgAtMZ())/(MW*SB) + (0.15625D0&
  &*CA3*m12squared*SA1*SA22*dgAtMZ())/(CB2*MW*SB) + (0.5625D0*CA12*CA3*m12squared*SA1*SA22*dgAtMZ())/(CB2*MW*SB) + (0.375D0*CA1*C&
  &A3*MH12*SA12*SA22*dgAtMZ())/(MW*SB) + (0.1875D0*CA1*CA3*MH22*SA12*SA22*dgAtMZ())/(MW*SB) + (0.28125D0*CA1*m12squared*SA2*SA3*d&
  &gAtMZ())/(MW*SB) + (0.84375D0*CA1*CA22*m12squared*SA2*SA3*dgAtMZ())/(MW*SB) - (0.1875D0*CA1*m12squared*SA2*SA3*dgAtMZ())/(CB2*&
  &MW*SB) - (0.5625D0*CA1*CA22*m12squared*SA2*SA3*dgAtMZ())/(CB2*MW*SB) + (0.1875D0*MH12*SA1*SA2*SA3*dgAtMZ())/(MW*SB) - (0.1875D&
  &0*CA12*MH12*SA1*SA2*SA3*dgAtMZ())/(MW*SB) + (0.5625D0*CA22*MH12*SA1*SA2*SA3*dgAtMZ())/(MW*SB) - (0.5625D0*CA12*CA22*MH12*SA1*S&
  &A2*SA3*dgAtMZ())/(MW*SB) + (0.09375D0*MH22*SA1*SA2*SA3*dgAtMZ())/(MW*SB) - (0.09375D0*CA12*MH22*SA1*SA2*SA3*dgAtMZ())/(MW*SB) &
  &+ (0.28125D0*CA22*MH22*SA1*SA2*SA3*dgAtMZ())/(MW*SB) - (0.28125D0*CA12*CA22*MH22*SA1*SA2*SA3*dgAtMZ())/(MW*SB) + (0.28125D0*CA&
  &1*m12squared*SA12*SA2*SA3*dgAtMZ())/(CB2*MW*SB) + (0.84375D0*CA1*CA22*m12squared*SA12*SA2*SA3*dgAtMZ())/(CB2*MW*SB) + (0.0625D&
  &0*CA1*CA3*m12squared*dgAtMZ())/(CB*MW*SB2) + (0.0625D0*CA1*CA22*CA3*m12squared*dgAtMZ())/(CB*MW*SB2) + (0.5625D0*CA1*CA3*m12sq&
  &uared*SA12*dgAtMZ())/(CB*MW*SB2) + (0.5625D0*CA1*CA22*CA3*m12squared*SA12*dgAtMZ())/(CB*MW*SB2) - (0.0625D0*CA1*CA3*m12squared&
  &*SA22*dgAtMZ())/(CB*MW*SB2) - (0.5625D0*CA1*CA3*m12squared*SA12*SA22*dgAtMZ())/(CB*MW*SB2) - (0.1875D0*m12squared*SA1*SA2*SA3*&
  &dgAtMZ())/(CB*MW*SB2) + (0.28125D0*CA12*m12squared*SA1*SA2*SA3*dgAtMZ())/(CB*MW*SB2) - (0.5625D0*CA22*m12squared*SA1*SA2*SA3*d&
  &gAtMZ())/(CB*MW*SB2) + (0.84375D0*CA12*CA22*m12squared*SA1*SA2*SA3*dgAtMZ())/(CB*MW*SB2) + (0.125D0*CA1*CA3*m12squared*dgAtMZ(&
  &))/(MW*SB*TB) + (0.125D0*CA1*CA22*CA3*m12squared*dgAtMZ())/(MW*SB*TB) - (0.125D0*CA1*CA3*m12squared*SA22*dgAtMZ())/(MW*SB*TB) &
  &- (0.09375D0*m12squared*SA1*SA2*SA3*dgAtMZ())/(MW*SB*TB) - (0.28125D0*CA22*m12squared*SA1*SA2*SA3*dgAtMZ())/(MW*SB*TB) - (0.03&
  &125D0*CA3*m12squared*SA1*TB*dgAtMZ())/(CB*MW) - (0.03125D0*CA22*CA3*m12squared*SA1*TB*dgAtMZ())/(CB*MW) + (0.03125D0*CA3*m12sq&
  &uared*SA1*SA22*TB*dgAtMZ())/(CB*MW) - (0.09375D0*CA1*m12squared*SA2*SA3*TB*dgAtMZ())/(CB*MW) - (0.28125D0*CA1*CA22*m12squared*&
  &SA2*SA3*TB*dgAtMZ())/(CB*MW) + (0.0625D0*MH12*SA2*SA3*DBLE(CA1**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.1875D0*CA22*MH12*SA2*SA3*DBL&
  &E(CA1**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.03125D0*MH22*SA2*SA3*DBLE(CA1**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.09375D0*CA22*MH22*SA&
  &2*SA3*DBLE(CA1**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.125D0*CA3*MH12*DBLE(CA1**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.125D0*CA22*CA3*MH&
  &12*DBLE(CA1**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.0625D0*CA3*MH22*DBLE(CA1**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.0625D0*CA22*CA3*MH2&
  &2*DBLE(CA1**INT(3.D0))*dgAtMZ())/(MW*SB) - (0.125D0*CA3*MH12*SA22*DBLE(CA1**INT(3.D0))*dgAtMZ())/(MW*SB) - (0.0625D0*CA3*MH22*&
  &SA22*DBLE(CA1**INT(3.D0))*dgAtMZ())/(MW*SB) - (0.09375D0*m12squared*SA2*SA3*DBLE(CA1**INT(3.D0))*dgAtMZ())/(CB2*MW*SB) - (0.28&
  &125D0*CA22*m12squared*SA2*SA3*DBLE(CA1**INT(3.D0))*dgAtMZ())/(CB2*MW*SB) - (0.1875D0*CA3*m12squared*DBLE(CA1**INT(3.D0))*dgAtM&
  &Z())/(CB*MW*SB2) - (0.1875D0*CA22*CA3*m12squared*DBLE(CA1**INT(3.D0))*dgAtMZ())/(CB*MW*SB2) + (0.1875D0*CA3*m12squared*SA22*DB&
  &LE(CA1**INT(3.D0))*dgAtMZ())/(CB*MW*SB2) - (0.125D0*CA3*MH12*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB*MW) - (0.125D0*CA22*CA3*MH12*D&
  &BLE(SA1**INT(3.D0))*dgAtMZ())/(CB*MW) - (0.0625D0*CA3*MH22*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB*MW) - (0.0625D0*CA22*CA3*MH22*DB&
  &LE(SA1**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.125D0*CA3*MH12*SA22*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.0625D0*CA3*MH22*SA22&
  &*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.1875D0*CA3*m12squared*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB2*MW*SB) + (0.1875D0*CA22&
  &*CA3*m12squared*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB2*MW*SB) - (0.1875D0*CA3*m12squared*SA22*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB2&
  &*MW*SB) + (0.0625D0*MH12*SA2*SA3*DBLE(SA1**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.1875D0*CA22*MH12*SA2*SA3*DBLE(SA1**INT(3.D0))*dgA&
  &tMZ())/(MW*SB) + (0.03125D0*MH22*SA2*SA3*DBLE(SA1**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.09375D0*CA22*MH22*SA2*SA3*DBLE(SA1**INT(3&
  &.D0))*dgAtMZ())/(MW*SB) - (0.09375D0*m12squared*SA2*SA3*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB*MW*SB2) - (0.28125D0*CA22*m12square&
  &d*SA2*SA3*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB*MW*SB2) - (0.1875D0*CA1*MH12*SA3*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW) - (0.0937&
  &5D0*CA1*MH22*SA3*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW) - (0.28125D0*m12squared*SA1*SA3*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW)&
  & + (0.1875D0*CA1*MH12*SA12*SA3*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.09375D0*CA1*MH22*SA12*SA3*DBLE(SA2**INT(3.D0))*dgAt&
  &MZ())/(CB*MW) - (0.28125D0*CA1*m12squared*SA3*DBLE(SA2**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.1875D0*CA1*m12squared*SA3*DBLE(SA2**&
  &INT(3.D0))*dgAtMZ())/(CB2*MW*SB) - (0.1875D0*MH12*SA1*SA3*DBLE(SA2**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.1875D0*CA12*MH12*SA1*SA3&
  &*DBLE(SA2**INT(3.D0))*dgAtMZ())/(MW*SB) - (0.09375D0*MH22*SA1*SA3*DBLE(SA2**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.09375D0*CA12*MH2&
  &2*SA1*SA3*DBLE(SA2**INT(3.D0))*dgAtMZ())/(MW*SB) - (0.28125D0*CA1*m12squared*SA12*SA3*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB2*MW*S&
  &B) + (0.1875D0*m12squared*SA1*SA3*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW*SB2) - (0.28125D0*CA12*m12squared*SA1*SA3*DBLE(SA2**IN&
  &T(3.D0))*dgAtMZ())/(CB*MW*SB2) + (0.09375D0*m12squared*SA1*SA3*DBLE(SA2**INT(3.D0))*dgAtMZ())/(MW*SB*TB) + (0.09375D0*CA1*m12s&
  &quared*SA3*TB*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW) - (0.0625D0*MH12*SA3*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dgAtMZ())/&
  &(CB*MW) - (0.03125D0*MH22*SA3*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.09375D0*m12squared*SA3*DBLE(CA1&
  &**INT(3.D0))*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB2*MW*SB) - (0.0625D0*MH12*SA3*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dgAtMZ(&
  &))/(MW*SB) - (0.03125D0*MH22*SA3*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.09375D0*m12squared*SA3*DBLE(&
  &SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW*SB2) - (0.125D0*CA1*CA3*EL*dm122MSBarAlter())/(CB*MW*SW) - (0.125D0*CA1*&
  &CA22*CA3*EL*dm122MSBarAlter())/(CB*MW*SW) + (0.125D0*CA1*CA3*EL*SA22*dm122MSBarAlter())/(CB*MW*SW) + (0.28125D0*EL*SA1*SA2*SA3&
  &*dm122MSBarAlter())/(CB*MW*SW) + (0.84375D0*CA22*EL*SA1*SA2*SA3*dm122MSBarAlter())/(CB*MW*SW) + (0.21875D0*CA3*EL*SA1*dm122MSB&
  &arAlter())/(MW*SB*SW) + (0.21875D0*CA22*CA3*EL*SA1*dm122MSBarAlter())/(MW*SB*SW) - (0.15625D0*CA3*EL*SA1*dm122MSBarAlter())/(C&
  &B2*MW*SB*SW) - (0.5625D0*CA12*CA3*EL*SA1*dm122MSBarAlter())/(CB2*MW*SB*SW) - (0.15625D0*CA22*CA3*EL*SA1*dm122MSBarAlter())/(CB&
  &2*MW*SB*SW) - (0.5625D0*CA12*CA22*CA3*EL*SA1*dm122MSBarAlter())/(CB2*MW*SB*SW) - (0.21875D0*CA3*EL*SA1*SA22*dm122MSBarAlter())&
  &/(MW*SB*SW) + (0.15625D0*CA3*EL*SA1*SA22*dm122MSBarAlter())/(CB2*MW*SB*SW) + (0.5625D0*CA12*CA3*EL*SA1*SA22*dm122MSBarAlter())&
  &/(CB2*MW*SB*SW) + (0.28125D0*CA1*EL*SA2*SA3*dm122MSBarAlter())/(MW*SB*SW) + (0.84375D0*CA1*CA22*EL*SA2*SA3*dm122MSBarAlter())/&
  &(MW*SB*SW) - (0.1875D0*CA1*EL*SA2*SA3*dm122MSBarAlter())/(CB2*MW*SB*SW) - (0.5625D0*CA1*CA22*EL*SA2*SA3*dm122MSBarAlter())/(CB&
  &2*MW*SB*SW) + (0.28125D0*CA1*EL*SA12*SA2*SA3*dm122MSBarAlter())/(CB2*MW*SB*SW) + (0.84375D0*CA1*CA22*EL*SA12*SA2*SA3*dm122MSBa&
  &rAlter())/(CB2*MW*SB*SW) + (0.0625D0*CA1*CA3*EL*dm122MSBarAlter())/(CB*MW*SB2*SW) + (0.0625D0*CA1*CA22*CA3*EL*dm122MSBarAlter(&
  &))/(CB*MW*SB2*SW) + (0.5625D0*CA1*CA3*EL*SA12*dm122MSBarAlter())/(CB*MW*SB2*SW) + (0.5625D0*CA1*CA22*CA3*EL*SA12*dm122MSBarAlt&
  &er())/(CB*MW*SB2*SW) - (0.0625D0*CA1*CA3*EL*SA22*dm122MSBarAlter())/(CB*MW*SB2*SW) - (0.5625D0*CA1*CA3*EL*SA12*SA22*dm122MSBar&
  &Alter())/(CB*MW*SB2*SW) - (0.1875D0*EL*SA1*SA2*SA3*dm122MSBarAlter())/(CB*MW*SB2*SW) + (0.28125D0*CA12*EL*SA1*SA2*SA3*dm122MSB&
  &arAlter())/(CB*MW*SB2*SW) - (0.5625D0*CA22*EL*SA1*SA2*SA3*dm122MSBarAlter())/(CB*MW*SB2*SW) + (0.84375D0*CA12*CA22*EL*SA1*SA2*&
  &SA3*dm122MSBarAlter())/(CB*MW*SB2*SW) + (0.125D0*CA1*CA3*EL*dm122MSBarAlter())/(MW*SB*SW*TB) + (0.125D0*CA1*CA22*CA3*EL*dm122M&
  &SBarAlter())/(MW*SB*SW*TB) - (0.125D0*CA1*CA3*EL*SA22*dm122MSBarAlter())/(MW*SB*SW*TB) - (0.09375D0*EL*SA1*SA2*SA3*dm122MSBarA&
  &lter())/(MW*SB*SW*TB) - (0.28125D0*CA22*EL*SA1*SA2*SA3*dm122MSBarAlter())/(MW*SB*SW*TB) - (0.03125D0*CA3*EL*SA1*TB*dm122MSBarA&
  &lter())/(CB*MW*SW) - (0.03125D0*CA22*CA3*EL*SA1*TB*dm122MSBarAlter())/(CB*MW*SW) + (0.03125D0*CA3*EL*SA1*SA22*TB*dm122MSBarAlt&
  &er())/(CB*MW*SW) - (0.09375D0*CA1*EL*SA2*SA3*TB*dm122MSBarAlter())/(CB*MW*SW) - (0.28125D0*CA1*CA22*EL*SA2*SA3*TB*dm122MSBarAl&
  &ter())/(CB*MW*SW) - (0.09375D0*EL*SA2*SA3*DBLE(CA1**INT(3.D0))*dm122MSBarAlter())/(CB2*MW*SB*SW) - (0.28125D0*CA22*EL*SA2*SA3*&
  &DBLE(CA1**INT(3.D0))*dm122MSBarAlter())/(CB2*MW*SB*SW) - (0.1875D0*CA3*EL*DBLE(CA1**INT(3.D0))*dm122MSBarAlter())/(CB*MW*SB2*S&
  &W) - (0.1875D0*CA22*CA3*EL*DBLE(CA1**INT(3.D0))*dm122MSBarAlter())/(CB*MW*SB2*SW) + (0.1875D0*CA3*EL*SA22*DBLE(CA1**INT(3.D0))&
  &*dm122MSBarAlter())/(CB*MW*SB2*SW) + (0.1875D0*CA3*EL*DBLE(SA1**INT(3.D0))*dm122MSBarAlter())/(CB2*MW*SB*SW) + (0.1875D0*CA22*&
  &CA3*EL*DBLE(SA1**INT(3.D0))*dm122MSBarAlter())/(CB2*MW*SB*SW) - (0.1875D0*CA3*EL*SA22*DBLE(SA1**INT(3.D0))*dm122MSBarAlter())/&
  &(CB2*MW*SB*SW) - (0.09375D0*EL*SA2*SA3*DBLE(SA1**INT(3.D0))*dm122MSBarAlter())/(CB*MW*SB2*SW) - (0.28125D0*CA22*EL*SA2*SA3*DBL&
  &E(SA1**INT(3.D0))*dm122MSBarAlter())/(CB*MW*SB2*SW) - (0.28125D0*EL*SA1*SA3*DBLE(SA2**INT(3.D0))*dm122MSBarAlter())/(CB*MW*SW)&
  & - (0.28125D0*CA1*EL*SA3*DBLE(SA2**INT(3.D0))*dm122MSBarAlter())/(MW*SB*SW) + (0.1875D0*CA1*EL*SA3*DBLE(SA2**INT(3.D0))*dm122M&
  &SBarAlter())/(CB2*MW*SB*SW) - (0.28125D0*CA1*EL*SA12*SA3*DBLE(SA2**INT(3.D0))*dm122MSBarAlter())/(CB2*MW*SB*SW) + (0.1875D0*EL&
  &*SA1*SA3*DBLE(SA2**INT(3.D0))*dm122MSBarAlter())/(CB*MW*SB2*SW) - (0.28125D0*CA12*EL*SA1*SA3*DBLE(SA2**INT(3.D0))*dm122MSBarAl&
  &ter())/(CB*MW*SB2*SW) + (0.09375D0*EL*SA1*SA3*DBLE(SA2**INT(3.D0))*dm122MSBarAlter())/(MW*SB*SW*TB) + (0.09375D0*CA1*EL*SA3*TB&
  &*DBLE(SA2**INT(3.D0))*dm122MSBarAlter())/(CB*MW*SW) + (0.09375D0*EL*SA3*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dm122MSBarAl&
  &ter())/(CB2*MW*SB*SW) + (0.09375D0*EL*SA3*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dm122MSBarAlter())/(CB*MW*SB2*SW) + (0.125&
  &D0*CA3*EL*SA1*dMH12OSAlter())/(CB*MW*SW) + (0.375D0*CA12*CA3*EL*SA1*dMH12OSAlter())/(CB*MW*SW) + (0.125D0*CA22*CA3*EL*SA1*dMH1&
  &2OSAlter())/(CB*MW*SW) + (0.375D0*CA12*CA22*CA3*EL*SA1*dMH12OSAlter())/(CB*MW*SW) - (0.125D0*CA3*EL*SA1*SA22*dMH12OSAlter())/(&
  &CB*MW*SW) - (0.375D0*CA12*CA3*EL*SA1*SA22*dMH12OSAlter())/(CB*MW*SW) + (0.1875D0*CA1*EL*SA2*SA3*dMH12OSAlter())/(CB*MW*SW) + (&
  &0.5625D0*CA1*CA22*EL*SA2*SA3*dMH12OSAlter())/(CB*MW*SW) - (0.1875D0*CA1*EL*SA12*SA2*SA3*dMH12OSAlter())/(CB*MW*SW) - (0.5625D0&
  &*CA1*CA22*EL*SA12*SA2*SA3*dMH12OSAlter())/(CB*MW*SW) - (0.125D0*CA1*CA3*EL*dMH12OSAlter())/(MW*SB*SW) - (0.125D0*CA1*CA22*CA3*&
  &EL*dMH12OSAlter())/(MW*SB*SW) - (0.375D0*CA1*CA3*EL*SA12*dMH12OSAlter())/(MW*SB*SW) - (0.375D0*CA1*CA22*CA3*EL*SA12*dMH12OSAlt&
  &er())/(MW*SB*SW) + (0.125D0*CA1*CA3*EL*SA22*dMH12OSAlter())/(MW*SB*SW) + (0.375D0*CA1*CA3*EL*SA12*SA22*dMH12OSAlter())/(MW*SB*&
  &SW) + (0.1875D0*EL*SA1*SA2*SA3*dMH12OSAlter())/(MW*SB*SW) - (0.1875D0*CA12*EL*SA1*SA2*SA3*dMH12OSAlter())/(MW*SB*SW) + (0.5625&
  &D0*CA22*EL*SA1*SA2*SA3*dMH12OSAlter())/(MW*SB*SW) - (0.5625D0*CA12*CA22*EL*SA1*SA2*SA3*dMH12OSAlter())/(MW*SB*SW) - (0.5D0*CA2&
  &*SA3*dMH12OSAlter())/vS - (1.5D0*CA2*SA22*SA3*dMH12OSAlter())/vS + (0.0625D0*EL*SA2*SA3*DBLE(CA1**INT(3.D0))*dMH12OSAlter())/(&
  &CB*MW*SW) + (0.1875D0*CA22*EL*SA2*SA3*DBLE(CA1**INT(3.D0))*dMH12OSAlter())/(CB*MW*SW) + (0.125D0*CA3*EL*DBLE(CA1**INT(3.D0))*d&
  &MH12OSAlter())/(MW*SB*SW) + (0.125D0*CA22*CA3*EL*DBLE(CA1**INT(3.D0))*dMH12OSAlter())/(MW*SB*SW) - (0.125D0*CA3*EL*SA22*DBLE(C&
  &A1**INT(3.D0))*dMH12OSAlter())/(MW*SB*SW) + (0.5D0*SA3*DBLE(CA2**INT(3.D0))*dMH12OSAlter())/vS - (0.125D0*CA3*EL*DBLE(SA1**INT&
  &(3.D0))*dMH12OSAlter())/(CB*MW*SW) - (0.125D0*CA22*CA3*EL*DBLE(SA1**INT(3.D0))*dMH12OSAlter())/(CB*MW*SW) + (0.125D0*CA3*EL*SA&
  &22*DBLE(SA1**INT(3.D0))*dMH12OSAlter())/(CB*MW*SW) + (0.0625D0*EL*SA2*SA3*DBLE(SA1**INT(3.D0))*dMH12OSAlter())/(MW*SB*SW) + (0&
  &.1875D0*CA22*EL*SA2*SA3*DBLE(SA1**INT(3.D0))*dMH12OSAlter())/(MW*SB*SW) - (0.1875D0*CA1*EL*SA3*DBLE(SA2**INT(3.D0))*dMH12OSAlt&
  &er())/(CB*MW*SW) + (0.1875D0*CA1*EL*SA12*SA3*DBLE(SA2**INT(3.D0))*dMH12OSAlter())/(CB*MW*SW) - (0.1875D0*EL*SA1*SA3*DBLE(SA2**&
  &INT(3.D0))*dMH12OSAlter())/(MW*SB*SW) + (0.1875D0*CA12*EL*SA1*SA3*DBLE(SA2**INT(3.D0))*dMH12OSAlter())/(MW*SB*SW) - (0.0625D0*&
  &EL*SA3*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dMH12OSAlter())/(CB*MW*SW) - (0.0625D0*EL*SA3*DBLE(SA1**INT(3.D0))*DBLE(SA2**&
  &INT(3.D0))*dMH12OSAlter())/(MW*SB*SW) + (0.0625D0*CA3*EL*SA1*dMH22OSAlter())/(CB*MW*SW) + (0.1875D0*CA12*CA3*EL*SA1*dMH22OSAlt&
  &er())/(CB*MW*SW) + (0.0625D0*CA22*CA3*EL*SA1*dMH22OSAlter())/(CB*MW*SW) + (0.1875D0*CA12*CA22*CA3*EL*SA1*dMH22OSAlter())/(CB*M&
  &W*SW) - (0.0625D0*CA3*EL*SA1*SA22*dMH22OSAlter())/(CB*MW*SW) - (0.1875D0*CA12*CA3*EL*SA1*SA22*dMH22OSAlter())/(CB*MW*SW) + (0.&
  &09375D0*CA1*EL*SA2*SA3*dMH22OSAlter())/(CB*MW*SW) + (0.28125D0*CA1*CA22*EL*SA2*SA3*dMH22OSAlter())/(CB*MW*SW) - (0.09375D0*CA1&
  &*EL*SA12*SA2*SA3*dMH22OSAlter())/(CB*MW*SW) - (0.28125D0*CA1*CA22*EL*SA12*SA2*SA3*dMH22OSAlter())/(CB*MW*SW) - (0.0625D0*CA1*C&
  &A3*EL*dMH22OSAlter())/(MW*SB*SW) - (0.0625D0*CA1*CA22*CA3*EL*dMH22OSAlter())/(MW*SB*SW) - (0.1875D0*CA1*CA3*EL*SA12*dMH22OSAlt&
  &er())/(MW*SB*SW) - (0.1875D0*CA1*CA22*CA3*EL*SA12*dMH22OSAlter())/(MW*SB*SW) + (0.0625D0*CA1*CA3*EL*SA22*dMH22OSAlter())/(MW*S&
  &B*SW) + (0.1875D0*CA1*CA3*EL*SA12*SA22*dMH22OSAlter())/(MW*SB*SW) + (0.09375D0*EL*SA1*SA2*SA3*dMH22OSAlter())/(MW*SB*SW) - (0.&
  &09375D0*CA12*EL*SA1*SA2*SA3*dMH22OSAlter())/(MW*SB*SW) + (0.28125D0*CA22*EL*SA1*SA2*SA3*dMH22OSAlter())/(MW*SB*SW) - (0.28125D&
  &0*CA12*CA22*EL*SA1*SA2*SA3*dMH22OSAlter())/(MW*SB*SW) - (0.25D0*CA2*SA3*dMH22OSAlter())/vS - (0.75D0*CA2*SA22*SA3*dMH22OSAlter&
  &())/vS + (0.03125D0*EL*SA2*SA3*DBLE(CA1**INT(3.D0))*dMH22OSAlter())/(CB*MW*SW) + (0.09375D0*CA22*EL*SA2*SA3*DBLE(CA1**INT(3.D0&
  &))*dMH22OSAlter())/(CB*MW*SW) + (0.0625D0*CA3*EL*DBLE(CA1**INT(3.D0))*dMH22OSAlter())/(MW*SB*SW) + (0.0625D0*CA22*CA3*EL*DBLE(&
  &CA1**INT(3.D0))*dMH22OSAlter())/(MW*SB*SW) - (0.0625D0*CA3*EL*SA22*DBLE(CA1**INT(3.D0))*dMH22OSAlter())/(MW*SB*SW) + (0.25D0*S&
  &A3*DBLE(CA2**INT(3.D0))*dMH22OSAlter())/vS - (0.0625D0*CA3*EL*DBLE(SA1**INT(3.D0))*dMH22OSAlter())/(CB*MW*SW) - (0.0625D0*CA22&
  &*CA3*EL*DBLE(SA1**INT(3.D0))*dMH22OSAlter())/(CB*MW*SW) + (0.0625D0*CA3*EL*SA22*DBLE(SA1**INT(3.D0))*dMH22OSAlter())/(CB*MW*SW&
  &) + (0.03125D0*EL*SA2*SA3*DBLE(SA1**INT(3.D0))*dMH22OSAlter())/(MW*SB*SW) + (0.09375D0*CA22*EL*SA2*SA3*DBLE(SA1**INT(3.D0))*dM&
  &H22OSAlter())/(MW*SB*SW) - (0.09375D0*CA1*EL*SA3*DBLE(SA2**INT(3.D0))*dMH22OSAlter())/(CB*MW*SW) + (0.09375D0*CA1*EL*SA12*SA3*&
  &DBLE(SA2**INT(3.D0))*dMH22OSAlter())/(CB*MW*SW) - (0.09375D0*EL*SA1*SA3*DBLE(SA2**INT(3.D0))*dMH22OSAlter())/(MW*SB*SW) + (0.0&
  &9375D0*CA12*EL*SA1*SA3*DBLE(SA2**INT(3.D0))*dMH22OSAlter())/(MW*SB*SW) - (0.03125D0*EL*SA3*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(&
  &3.D0))*dMH22OSAlter())/(CB*MW*SW) - (0.03125D0*EL*SA3*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dMH22OSAlter())/(MW*SB*SW) + (&
  &0.0625D0*CA1*CA3*EL*m12squared*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.0625D0*CA1*CA22*CA3*EL*m12squared*DBLE(MW**INT(-&
  &3.D0))*dMW2Alter())/(CB*SW) - (0.0625D0*CA3*EL*MH12*SA1*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.1875D0*CA12*CA3*EL*MH12&
  &*SA1*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.0625D0*CA22*CA3*EL*MH12*SA1*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0&
  &.1875D0*CA12*CA22*CA3*EL*MH12*SA1*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.03125D0*CA3*EL*MH22*SA1*DBLE(MW**INT(-3.D0))*&
  &dMW2Alter())/(CB*SW) - (0.09375D0*CA12*CA3*EL*MH22*SA1*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.03125D0*CA22*CA3*EL*MH22&
  &*SA1*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.09375D0*CA12*CA22*CA3*EL*MH22*SA1*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW&
  &) - (0.0625D0*CA1*CA3*EL*m12squared*SA22*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.0625D0*CA3*EL*MH12*SA1*SA22*DBLE(MW**I&
  &NT(-3.D0))*dMW2Alter())/(CB*SW) + (0.1875D0*CA12*CA3*EL*MH12*SA1*SA22*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.03125D0*C&
  &A3*EL*MH22*SA1*SA22*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.09375D0*CA12*CA3*EL*MH22*SA1*SA22*DBLE(MW**INT(-3.D0))*dMW2&
  &Alter())/(CB*SW) - (0.09375D0*CA1*EL*MH12*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.28125D0*CA1*CA22*EL*MH12*SA2*&
  &SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.046875D0*CA1*EL*MH22*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0&
  &.140625D0*CA1*CA22*EL*MH22*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.140625D0*EL*m12squared*SA1*SA2*SA3*DBLE(MW**&
  &INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.421875D0*CA22*EL*m12squared*SA1*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.0&
  &9375D0*CA1*EL*MH12*SA12*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.28125D0*CA1*CA22*EL*MH12*SA12*SA2*SA3*DBLE(MW**&
  &INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.046875D0*CA1*EL*MH22*SA12*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.140625D&
  &0*CA1*CA22*EL*MH22*SA12*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.0625D0*CA1*CA3*EL*MH12*DBLE(MW**INT(-3.D0))*dMW&
  &2Alter())/(SB*SW) + (0.0625D0*CA1*CA22*CA3*EL*MH12*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.03125D0*CA1*CA3*EL*MH22*DBLE&
  &(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.03125D0*CA1*CA22*CA3*EL*MH22*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.109375D&
  &0*CA3*EL*m12squared*SA1*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.109375D0*CA22*CA3*EL*m12squared*SA1*DBLE(MW**INT(-3.D0)&
  &)*dMW2Alter())/(SB*SW) + (0.078125D0*CA3*EL*m12squared*SA1*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB2*SB*SW) + (0.28125D0*CA12*CA3&
  &*EL*m12squared*SA1*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB2*SB*SW) + (0.078125D0*CA22*CA3*EL*m12squared*SA1*DBLE(MW**INT(-3.D0))&
  &*dMW2Alter())/(CB2*SB*SW) + (0.28125D0*CA12*CA22*CA3*EL*m12squared*SA1*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB2*SB*SW) + (0.1875&
  &D0*CA1*CA3*EL*MH12*SA12*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.1875D0*CA1*CA22*CA3*EL*MH12*SA12*DBLE(MW**INT(-3.D0))*d&
  &MW2Alter())/(SB*SW) + (0.09375D0*CA1*CA3*EL*MH22*SA12*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.09375D0*CA1*CA22*CA3*EL*M&
  &H22*SA12*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.0625D0*CA1*CA3*EL*MH12*SA22*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) &
  &- (0.03125D0*CA1*CA3*EL*MH22*SA22*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.109375D0*CA3*EL*m12squared*SA1*SA22*DBLE(MW**&
  &INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.078125D0*CA3*EL*m12squared*SA1*SA22*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB2*SB*SW) - (0.2&
  &8125D0*CA12*CA3*EL*m12squared*SA1*SA22*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB2*SB*SW) - (0.1875D0*CA1*CA3*EL*MH12*SA12*SA22*DBL&
  &E(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.09375D0*CA1*CA3*EL*MH22*SA12*SA22*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.1&
  &40625D0*CA1*EL*m12squared*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.421875D0*CA1*CA22*EL*m12squared*SA2*SA3*DBLE(&
  &MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.09375D0*CA1*EL*m12squared*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB2*SB*SW) + (0&
  &.28125D0*CA1*CA22*EL*m12squared*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB2*SB*SW) - (0.09375D0*EL*MH12*SA1*SA2*SA3*DBLE(MW&
  &**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.09375D0*CA12*EL*MH12*SA1*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.28125D&
  &0*CA22*EL*MH12*SA1*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.28125D0*CA12*CA22*EL*MH12*SA1*SA2*SA3*DBLE(MW**INT(-&
  &3.D0))*dMW2Alter())/(SB*SW) - (0.046875D0*EL*MH22*SA1*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.046875D0*CA12*EL*&
  &MH22*SA1*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.140625D0*CA22*EL*MH22*SA1*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alt&
  &er())/(SB*SW) + (0.140625D0*CA12*CA22*EL*MH22*SA1*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.140625D0*CA1*EL*m12sq&
  &uared*SA12*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB2*SB*SW) - (0.421875D0*CA1*CA22*EL*m12squared*SA12*SA2*SA3*DBLE(MW**IN&
  &T(-3.D0))*dMW2Alter())/(CB2*SB*SW) - (0.03125D0*CA1*CA3*EL*m12squared*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SB2*SW) - (0.03125&
  &D0*CA1*CA22*CA3*EL*m12squared*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SB2*SW) - (0.28125D0*CA1*CA3*EL*m12squared*SA12*DBLE(MW**I&
  &NT(-3.D0))*dMW2Alter())/(CB*SB2*SW) - (0.28125D0*CA1*CA22*CA3*EL*m12squared*SA12*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SB2*SW)&
  & + (0.03125D0*CA1*CA3*EL*m12squared*SA22*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SB2*SW) + (0.28125D0*CA1*CA3*EL*m12squared*SA12&
  &*SA22*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SB2*SW) + (0.09375D0*EL*m12squared*SA1*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(&
  &CB*SB2*SW) - (0.140625D0*CA12*EL*m12squared*SA1*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SB2*SW) + (0.28125D0*CA22*EL*m12&
  &squared*SA1*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SB2*SW) - (0.421875D0*CA12*CA22*EL*m12squared*SA1*SA2*SA3*DBLE(MW**I&
  &NT(-3.D0))*dMW2Alter())/(CB*SB2*SW) - (0.0625D0*CA1*CA3*EL*m12squared*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW*TB) - (0.0625D0&
  &*CA1*CA22*CA3*EL*m12squared*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW*TB) + (0.0625D0*CA1*CA3*EL*m12squared*SA22*DBLE(MW**INT(-&
  &3.D0))*dMW2Alter())/(SB*SW*TB) + (0.046875D0*EL*m12squared*SA1*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW*TB) + (0.14062&
  &5D0*CA22*EL*m12squared*SA1*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW*TB) + (0.015625D0*CA3*EL*m12squared*SA1*TB*DBLE(MW&
  &**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.015625D0*CA22*CA3*EL*m12squared*SA1*TB*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.&
  &015625D0*CA3*EL*m12squared*SA1*SA22*TB*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.046875D0*CA1*EL*m12squared*SA2*SA3*TB*DB&
  &LE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.140625D0*CA1*CA22*EL*m12squared*SA2*SA3*TB*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*&
  &SW) - (0.03125D0*EL*MH12*SA2*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.09375D0*CA22*EL*MH12*SA2*&
  &SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.015625D0*EL*MH22*SA2*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW*&
  &*INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.046875D0*CA22*EL*MH22*SA2*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(C&
  &B*SW) - (0.0625D0*CA3*EL*MH12*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.0625D0*CA22*CA3*EL*MH12*DBLE&
  &(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.03125D0*CA3*EL*MH22*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*&
  &dMW2Alter())/(SB*SW) - (0.03125D0*CA22*CA3*EL*MH22*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.0625D0*&
  &CA3*EL*MH12*SA22*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.03125D0*CA3*EL*MH22*SA22*DBLE(CA1**INT(3.&
  &D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.046875D0*EL*m12squared*SA2*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*d&
  &MW2Alter())/(CB2*SB*SW) + (0.140625D0*CA22*EL*m12squared*SA2*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB2*S&
  &B*SW) + (0.09375D0*CA3*EL*m12squared*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SB2*SW) + (0.09375D0*CA22*CA3*&
  &EL*m12squared*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SB2*SW) - (0.09375D0*CA3*EL*m12squared*SA22*DBLE(CA1*&
  &*INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SB2*SW) + (0.0625D0*CA3*EL*MH12*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dM&
  &W2Alter())/(CB*SW) + (0.0625D0*CA22*CA3*EL*MH12*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(CB*SW) + (0.03125D0*CA&
  &3*EL*MH22*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(CB*SW) + (0.03125D0*CA22*CA3*EL*MH22*DBLE(MW**INT(-3.D0))*DB&
  &LE(SA1**INT(3.D0))*dMW2Alter())/(CB*SW) - (0.0625D0*CA3*EL*MH12*SA22*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(C&
  &B*SW) - (0.03125D0*CA3*EL*MH22*SA22*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(CB*SW) - (0.09375D0*CA3*EL*m12squa&
  &red*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(CB2*SB*SW) - (0.09375D0*CA22*CA3*EL*m12squared*DBLE(MW**INT(-3.D0)&
  &)*DBLE(SA1**INT(3.D0))*dMW2Alter())/(CB2*SB*SW) + (0.09375D0*CA3*EL*m12squared*SA22*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*&
  &dMW2Alter())/(CB2*SB*SW) - (0.03125D0*EL*MH12*SA2*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(SB*SW) - (0.0937&
  &5D0*CA22*EL*MH12*SA2*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(SB*SW) - (0.015625D0*EL*MH22*SA2*SA3*DBLE(MW*&
  &*INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(SB*SW) - (0.046875D0*CA22*EL*MH22*SA2*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3&
  &.D0))*dMW2Alter())/(SB*SW) + (0.046875D0*EL*m12squared*SA2*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(CB*SB2*&
  &SW) + (0.140625D0*CA22*EL*m12squared*SA2*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(CB*SB2*SW) + (0.09375D0*C&
  &A1*EL*MH12*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(CB*SW) + (0.046875D0*CA1*EL*MH22*SA3*DBLE(MW**INT(-3.D0&
  &))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(CB*SW) + (0.140625D0*EL*m12squared*SA1*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW&
  &2Alter())/(CB*SW) - (0.09375D0*CA1*EL*MH12*SA12*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(CB*SW) - (0.046875&
  &D0*CA1*EL*MH22*SA12*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(CB*SW) + (0.140625D0*CA1*EL*m12squared*SA3*DBL&
  &E(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(SB*SW) - (0.09375D0*CA1*EL*m12squared*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA2**&
  &INT(3.D0))*dMW2Alter())/(CB2*SB*SW) + (0.09375D0*EL*MH12*SA1*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(SB*SW&
  &) - (0.09375D0*CA12*EL*MH12*SA1*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(SB*SW) + (0.046875D0*EL*MH22*SA1*S&
  &A3*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(SB*SW) - (0.046875D0*CA12*EL*MH22*SA1*SA3*DBLE(MW**INT(-3.D0))*DBLE&
  &(SA2**INT(3.D0))*dMW2Alter())/(SB*SW) + (0.140625D0*CA1*EL*m12squared*SA12*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2A&
  &lter())/(CB2*SB*SW) - (0.09375D0*EL*m12squared*SA1*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(CB*SB2*SW) + (0&
  &.140625D0*CA12*EL*m12squared*SA1*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(CB*SB2*SW) - (0.046875D0*EL*m12sq&
  &uared*SA1*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(SB*SW*TB) - (0.046875D0*CA1*EL*m12squared*SA3*TB*DBLE(MW&
  &**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(CB*SW) + (0.03125D0*EL*MH12*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*DBL&
  &E(SA2**INT(3.D0))*dMW2Alter())/(CB*SW) + (0.015625D0*EL*MH22*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0)&
  &)*dMW2Alter())/(CB*SW) - (0.046875D0*EL*m12squared*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alte&
  &r())/(CB2*SB*SW) + (0.03125D0*EL*MH12*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(SB*SW) &
  &+ (0.015625D0*EL*MH22*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(SB*SW) - (0.046875D0*EL&
  &*m12squared*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(CB*SB2*SW) + 0.5D0*CA2*MH12*SA3*D&
  &BLE(vS**INT(-2.D0))*dvSMSBarAlter() + 0.25D0*CA2*MH22*SA3*DBLE(vS**INT(-2.D0))*dvSMSBarAlter() + 1.5D0*CA2*MH12*SA22*SA3*DBLE(&
  &vS**INT(-2.D0))*dvSMSBarAlter() + 0.75D0*CA2*MH22*SA22*SA3*DBLE(vS**INT(-2.D0))*dvSMSBarAlter() - 0.5D0*MH12*SA3*DBLE(CA2**INT&
  &(3.D0))*DBLE(vS**INT(-2.D0))*dvSMSBarAlter() - 0.25D0*MH22*SA3*DBLE(CA2**INT(3.D0))*DBLE(vS**INT(-2.D0))*dvSMSBarAlter() &
		& + CS1S1S1f111*dZH1H2OSAlter()/2D0 + CS1S1S1f211*dZH2H2OS()/2D0 + CS1S1S1f311*dZH3H2OSAlter()/2D0 &
		& + CS1S1S1f121*dZH1H1OS()/2D0 + CS1S1S1f221*dZH2H1OSAlter()/2D0 + CS1S1S1f321*dZH3H1OSAlter()/2D0 &
		& + CS1S1S1f121*dZH1H1OS()/2D0 + CS1S1S1f221*dZH2H1OSAlter()/2D0 + CS1S1S1f321*dZH3H1OSAlter()/2D0 &
		& )
	case (4)
		dRR11 = -CA2*SA1*dAlpha1KanAlter() - CA1*SA2*dAlpha2KanAlter()
		dRR12 = CA1*CA2*dAlpha1KanAlter() - SA1*SA2*dAlpha2KanAlter()
		dRR13 = CA2*dAlpha2KanAlter()
		dRR21 = -CA1*CA3*dAlpha1KanAlter() - CA1*CA3*SA2*dAlpha3KanAlter() - CA1*CA2*SA3*dAlpha2KanAlter() + &
			& SA1*SA3*dAlpha3KanAlter() + SA1*SA2*SA3*dAlpha1KanAlter()
		dRR22 = -CA3*SA1*dAlpha1KanAlter() - CA3*SA1*SA2*dAlpha3KanAlter() - CA1*SA3*dAlpha3KanAlter() - &
			& CA2*SA1*SA3*dAlpha2KanAlter() - CA1*SA2*SA3*dAlpha1KanAlter()
		dRR23 = CA2*CA3*dAlpha3KanAlter() - SA2*SA3*dAlpha2KanAlter()
		dRR31 = -CA1*CA2*CA3*dAlpha2KanAlter() + CA3*SA1*dAlpha3KanAlter() + CA3*SA1*SA2*dAlpha1KanAlter() + &
			& CA1*SA3*dAlpha1KanAlter() + CA1*SA2*SA3*dAlpha3KanAlter()
		dRR32 = -CA1*CA3*dAlpha3KanAlter() - CA2*CA3*SA1*dAlpha2KanAlter() - CA1*CA3*SA2*dAlpha1KanAlter() + &
			& SA1*SA3*dAlpha1KanAlter() + SA1*SA2*SA3*dAlpha3KanAlter()
		dRR33 = -CA3*SA2*dAlpha2KanAlter() - CA2*SA3*dAlpha3KanAlter()

		totalAmplitude = CS1S1S1f211*( &
&(0.125D0*CA1*CA3*EL*MH12*dAlpha1KanAlter())/(CB*MW*SW) + (0.125D0*CA1*CA22*CA3*EL*MH12*dAlpha1KanAlter())/(CB*MW*SW) + (0.0625D0*&
  &CA1*CA3*EL*MH22*dAlpha1KanAlter())/(CB*MW*SW) + (0.0625D0*CA1*CA22*CA3*EL*MH22*dAlpha1KanAlter())/(CB*MW*SW) + (0.125D0*CA3*EL&
  &*m12squared*SA1*dAlpha1KanAlter())/(CB*MW*SW) + (0.125D0*CA22*CA3*EL*m12squared*SA1*dAlpha1KanAlter())/(CB*MW*SW) - (1.125D0*C&
  &A1*CA3*EL*MH12*SA12*dAlpha1KanAlter())/(CB*MW*SW) - (1.125D0*CA1*CA22*CA3*EL*MH12*SA12*dAlpha1KanAlter())/(CB*MW*SW) - (0.5625&
  &D0*CA1*CA3*EL*MH22*SA12*dAlpha1KanAlter())/(CB*MW*SW) - (0.5625D0*CA1*CA22*CA3*EL*MH22*SA12*dAlpha1KanAlter())/(CB*MW*SW) - (0&
  &.125D0*CA1*CA3*EL*MH12*SA22*dAlpha1KanAlter())/(CB*MW*SW) - (0.0625D0*CA1*CA3*EL*MH22*SA22*dAlpha1KanAlter())/(CB*MW*SW) - (0.&
  &125D0*CA3*EL*m12squared*SA1*SA22*dAlpha1KanAlter())/(CB*MW*SW) + (1.125D0*CA1*CA3*EL*MH12*SA12*SA22*dAlpha1KanAlter())/(CB*MW*&
  &SW) + (0.5625D0*CA1*CA3*EL*MH22*SA12*SA22*dAlpha1KanAlter())/(CB*MW*SW) + (0.28125D0*CA1*EL*m12squared*SA2*SA3*dAlpha1KanAlter&
  &())/(CB*MW*SW) + (0.84375D0*CA1*CA22*EL*m12squared*SA2*SA3*dAlpha1KanAlter())/(CB*MW*SW) - (0.1875D0*EL*MH12*SA1*SA2*SA3*dAlph&
  &a1KanAlter())/(CB*MW*SW) - (0.5625D0*CA12*EL*MH12*SA1*SA2*SA3*dAlpha1KanAlter())/(CB*MW*SW) - (0.5625D0*CA22*EL*MH12*SA1*SA2*S&
  &A3*dAlpha1KanAlter())/(CB*MW*SW) - (1.6875D0*CA12*CA22*EL*MH12*SA1*SA2*SA3*dAlpha1KanAlter())/(CB*MW*SW) - (0.09375D0*EL*MH22*&
  &SA1*SA2*SA3*dAlpha1KanAlter())/(CB*MW*SW) - (0.28125D0*CA12*EL*MH22*SA1*SA2*SA3*dAlpha1KanAlter())/(CB*MW*SW) - (0.28125D0*CA2&
  &2*EL*MH22*SA1*SA2*SA3*dAlpha1KanAlter())/(CB*MW*SW) - (0.84375D0*CA12*CA22*EL*MH22*SA1*SA2*SA3*dAlpha1KanAlter())/(CB*MW*SW) +&
  & (0.21875D0*CA1*CA3*EL*m12squared*dAlpha1KanAlter())/(MW*SB*SW) + (0.21875D0*CA1*CA22*CA3*EL*m12squared*dAlpha1KanAlter())/(MW&
  &*SB*SW) - (0.15625D0*CA1*CA3*EL*m12squared*dAlpha1KanAlter())/(CB2*MW*SB*SW) - (0.15625D0*CA1*CA22*CA3*EL*m12squared*dAlpha1Ka&
  &nAlter())/(CB2*MW*SB*SW) + (0.125D0*CA3*EL*MH12*SA1*dAlpha1KanAlter())/(MW*SB*SW) - (1.125D0*CA12*CA3*EL*MH12*SA1*dAlpha1KanAl&
  &ter())/(MW*SB*SW) + (0.125D0*CA22*CA3*EL*MH12*SA1*dAlpha1KanAlter())/(MW*SB*SW) - (1.125D0*CA12*CA22*CA3*EL*MH12*SA1*dAlpha1Ka&
  &nAlter())/(MW*SB*SW) + (0.0625D0*CA3*EL*MH22*SA1*dAlpha1KanAlter())/(MW*SB*SW) - (0.5625D0*CA12*CA3*EL*MH22*SA1*dAlpha1KanAlte&
  &r())/(MW*SB*SW) + (0.0625D0*CA22*CA3*EL*MH22*SA1*dAlpha1KanAlter())/(MW*SB*SW) - (0.5625D0*CA12*CA22*CA3*EL*MH22*SA1*dAlpha1Ka&
  &nAlter())/(MW*SB*SW) + (1.6875D0*CA1*CA3*EL*m12squared*SA12*dAlpha1KanAlter())/(CB2*MW*SB*SW) + (1.6875D0*CA1*CA22*CA3*EL*m12s&
  &quared*SA12*dAlpha1KanAlter())/(CB2*MW*SB*SW) - (0.21875D0*CA1*CA3*EL*m12squared*SA22*dAlpha1KanAlter())/(MW*SB*SW) + (0.15625&
  &D0*CA1*CA3*EL*m12squared*SA22*dAlpha1KanAlter())/(CB2*MW*SB*SW) - (0.125D0*CA3*EL*MH12*SA1*SA22*dAlpha1KanAlter())/(MW*SB*SW) &
  &+ (1.125D0*CA12*CA3*EL*MH12*SA1*SA22*dAlpha1KanAlter())/(MW*SB*SW) - (0.0625D0*CA3*EL*MH22*SA1*SA22*dAlpha1KanAlter())/(MW*SB*&
  &SW) + (0.5625D0*CA12*CA3*EL*MH22*SA1*SA22*dAlpha1KanAlter())/(MW*SB*SW) - (1.6875D0*CA1*CA3*EL*m12squared*SA12*SA22*dAlpha1Kan&
  &Alter())/(CB2*MW*SB*SW) + (0.1875D0*CA1*EL*MH12*SA2*SA3*dAlpha1KanAlter())/(MW*SB*SW) + (0.5625D0*CA1*CA22*EL*MH12*SA2*SA3*dAl&
  &pha1KanAlter())/(MW*SB*SW) + (0.09375D0*CA1*EL*MH22*SA2*SA3*dAlpha1KanAlter())/(MW*SB*SW) + (0.28125D0*CA1*CA22*EL*MH22*SA2*SA&
  &3*dAlpha1KanAlter())/(MW*SB*SW) - (0.28125D0*EL*m12squared*SA1*SA2*SA3*dAlpha1KanAlter())/(MW*SB*SW) - (0.84375D0*CA22*EL*m12s&
  &quared*SA1*SA2*SA3*dAlpha1KanAlter())/(MW*SB*SW) + (0.1875D0*EL*m12squared*SA1*SA2*SA3*dAlpha1KanAlter())/(CB2*MW*SB*SW) + (0.&
  &84375D0*CA12*EL*m12squared*SA1*SA2*SA3*dAlpha1KanAlter())/(CB2*MW*SB*SW) + (0.5625D0*CA22*EL*m12squared*SA1*SA2*SA3*dAlpha1Kan&
  &Alter())/(CB2*MW*SB*SW) + (2.53125D0*CA12*CA22*EL*m12squared*SA1*SA2*SA3*dAlpha1KanAlter())/(CB2*MW*SB*SW) + (0.5625D0*CA1*EL*&
  &MH12*SA12*SA2*SA3*dAlpha1KanAlter())/(MW*SB*SW) + (1.6875D0*CA1*CA22*EL*MH12*SA12*SA2*SA3*dAlpha1KanAlter())/(MW*SB*SW) + (0.2&
  &8125D0*CA1*EL*MH22*SA12*SA2*SA3*dAlpha1KanAlter())/(MW*SB*SW) + (0.84375D0*CA1*CA22*EL*MH22*SA12*SA2*SA3*dAlpha1KanAlter())/(M&
  &W*SB*SW) - (0.0625D0*CA3*EL*m12squared*SA1*dAlpha1KanAlter())/(CB*MW*SB2*SW) + (1.6875D0*CA12*CA3*EL*m12squared*SA1*dAlpha1Kan&
  &Alter())/(CB*MW*SB2*SW) - (0.0625D0*CA22*CA3*EL*m12squared*SA1*dAlpha1KanAlter())/(CB*MW*SB2*SW) + (1.6875D0*CA12*CA22*CA3*EL*&
  &m12squared*SA1*dAlpha1KanAlter())/(CB*MW*SB2*SW) + (0.0625D0*CA3*EL*m12squared*SA1*SA22*dAlpha1KanAlter())/(CB*MW*SB2*SW) - (1&
  &.6875D0*CA12*CA3*EL*m12squared*SA1*SA22*dAlpha1KanAlter())/(CB*MW*SB2*SW) - (0.1875D0*CA1*EL*m12squared*SA2*SA3*dAlpha1KanAlte&
  &r())/(CB*MW*SB2*SW) - (0.5625D0*CA1*CA22*EL*m12squared*SA2*SA3*dAlpha1KanAlter())/(CB*MW*SB2*SW) - (0.84375D0*CA1*EL*m12square&
  &d*SA12*SA2*SA3*dAlpha1KanAlter())/(CB*MW*SB2*SW) - (2.53125D0*CA1*CA22*EL*m12squared*SA12*SA2*SA3*dAlpha1KanAlter())/(CB*MW*SB&
  &2*SW) - (0.125D0*CA3*EL*m12squared*SA1*dAlpha1KanAlter())/(MW*SB*SW*TB) - (0.125D0*CA22*CA3*EL*m12squared*SA1*dAlpha1KanAlter(&
  &))/(MW*SB*SW*TB) + (0.125D0*CA3*EL*m12squared*SA1*SA22*dAlpha1KanAlter())/(MW*SB*SW*TB) - (0.09375D0*CA1*EL*m12squared*SA2*SA3&
  &*dAlpha1KanAlter())/(MW*SB*SW*TB) - (0.28125D0*CA1*CA22*EL*m12squared*SA2*SA3*dAlpha1KanAlter())/(MW*SB*SW*TB) - (0.03125D0*CA&
  &1*CA3*EL*m12squared*TB*dAlpha1KanAlter())/(CB*MW*SW) - (0.03125D0*CA1*CA22*CA3*EL*m12squared*TB*dAlpha1KanAlter())/(CB*MW*SW) &
  &+ (0.03125D0*CA1*CA3*EL*m12squared*SA22*TB*dAlpha1KanAlter())/(CB*MW*SW) + (0.09375D0*EL*m12squared*SA1*SA2*SA3*TB*dAlpha1KanA&
  &lter())/(CB*MW*SW) + (0.28125D0*CA22*EL*m12squared*SA1*SA2*SA3*TB*dAlpha1KanAlter())/(CB*MW*SW) + (0.5D0*CA1*CA2*CA3*EL*m12squ&
  &ared*SA2*dAlpha2KanAlter())/(CB*MW*SW) - (0.5D0*CA2*CA3*EL*MH12*SA1*SA2*dAlpha2KanAlter())/(CB*MW*SW) - (1.5D0*CA12*CA2*CA3*EL&
  &*MH12*SA1*SA2*dAlpha2KanAlter())/(CB*MW*SW) - (0.25D0*CA2*CA3*EL*MH22*SA1*SA2*dAlpha2KanAlter())/(CB*MW*SW) - (0.75D0*CA12*CA2&
  &*CA3*EL*MH22*SA1*SA2*dAlpha2KanAlter())/(CB*MW*SW) + (0.1875D0*CA1*CA2*EL*MH12*SA3*dAlpha2KanAlter())/(CB*MW*SW) + (0.09375D0*&
  &CA1*CA2*EL*MH22*SA3*dAlpha2KanAlter())/(CB*MW*SW) + (0.28125D0*CA2*EL*m12squared*SA1*SA3*dAlpha2KanAlter())/(CB*MW*SW) - (0.18&
  &75D0*CA1*CA2*EL*MH12*SA12*SA3*dAlpha2KanAlter())/(CB*MW*SW) - (0.09375D0*CA1*CA2*EL*MH22*SA12*SA3*dAlpha2KanAlter())/(CB*MW*SW&
  &) - (1.6875D0*CA1*CA2*EL*MH12*SA22*SA3*dAlpha2KanAlter())/(CB*MW*SW) - (0.84375D0*CA1*CA2*EL*MH22*SA22*SA3*dAlpha2KanAlter())/&
  &(CB*MW*SW) - (2.53125D0*CA2*EL*m12squared*SA1*SA22*SA3*dAlpha2KanAlter())/(CB*MW*SW) + (1.6875D0*CA1*CA2*EL*MH12*SA12*SA22*SA3&
  &*dAlpha2KanAlter())/(CB*MW*SW) + (0.84375D0*CA1*CA2*EL*MH22*SA12*SA22*SA3*dAlpha2KanAlter())/(CB*MW*SW) + (0.5D0*CA1*CA2*CA3*E&
  &L*MH12*SA2*dAlpha2KanAlter())/(MW*SB*SW) + (0.25D0*CA1*CA2*CA3*EL*MH22*SA2*dAlpha2KanAlter())/(MW*SB*SW) - (0.875D0*CA2*CA3*EL&
  &*m12squared*SA1*SA2*dAlpha2KanAlter())/(MW*SB*SW) + (0.625D0*CA2*CA3*EL*m12squared*SA1*SA2*dAlpha2KanAlter())/(CB2*MW*SB*SW) +&
  & (2.25D0*CA12*CA2*CA3*EL*m12squared*SA1*SA2*dAlpha2KanAlter())/(CB2*MW*SB*SW) + (1.5D0*CA1*CA2*CA3*EL*MH12*SA12*SA2*dAlpha2Kan&
  &Alter())/(MW*SB*SW) + (0.75D0*CA1*CA2*CA3*EL*MH22*SA12*SA2*dAlpha2KanAlter())/(MW*SB*SW) + (0.28125D0*CA1*CA2*EL*m12squared*SA&
  &3*dAlpha2KanAlter())/(MW*SB*SW) - (0.1875D0*CA1*CA2*EL*m12squared*SA3*dAlpha2KanAlter())/(CB2*MW*SB*SW) + (0.1875D0*CA2*EL*MH1&
  &2*SA1*SA3*dAlpha2KanAlter())/(MW*SB*SW) - (0.1875D0*CA12*CA2*EL*MH12*SA1*SA3*dAlpha2KanAlter())/(MW*SB*SW) + (0.09375D0*CA2*EL&
  &*MH22*SA1*SA3*dAlpha2KanAlter())/(MW*SB*SW) - (0.09375D0*CA12*CA2*EL*MH22*SA1*SA3*dAlpha2KanAlter())/(MW*SB*SW) + (0.28125D0*C&
  &A1*CA2*EL*m12squared*SA12*SA3*dAlpha2KanAlter())/(CB2*MW*SB*SW) - (2.53125D0*CA1*CA2*EL*m12squared*SA22*SA3*dAlpha2KanAlter())&
  &/(MW*SB*SW) + (1.6875D0*CA1*CA2*EL*m12squared*SA22*SA3*dAlpha2KanAlter())/(CB2*MW*SB*SW) - (1.6875D0*CA2*EL*MH12*SA1*SA22*SA3*&
  &dAlpha2KanAlter())/(MW*SB*SW) + (1.6875D0*CA12*CA2*EL*MH12*SA1*SA22*SA3*dAlpha2KanAlter())/(MW*SB*SW) - (0.84375D0*CA2*EL*MH22&
  &*SA1*SA22*SA3*dAlpha2KanAlter())/(MW*SB*SW) + (0.84375D0*CA12*CA2*EL*MH22*SA1*SA22*SA3*dAlpha2KanAlter())/(MW*SB*SW) - (2.5312&
  &5D0*CA1*CA2*EL*m12squared*SA12*SA22*SA3*dAlpha2KanAlter())/(CB2*MW*SB*SW) - (0.25D0*CA1*CA2*CA3*EL*m12squared*SA2*dAlpha2KanAl&
  &ter())/(CB*MW*SB2*SW) - (2.25D0*CA1*CA2*CA3*EL*m12squared*SA12*SA2*dAlpha2KanAlter())/(CB*MW*SB2*SW) - (0.1875D0*CA2*EL*m12squ&
  &ared*SA1*SA3*dAlpha2KanAlter())/(CB*MW*SB2*SW) + (0.28125D0*CA12*CA2*EL*m12squared*SA1*SA3*dAlpha2KanAlter())/(CB*MW*SB2*SW) +&
  & (1.6875D0*CA2*EL*m12squared*SA1*SA22*SA3*dAlpha2KanAlter())/(CB*MW*SB2*SW) - (2.53125D0*CA12*CA2*EL*m12squared*SA1*SA22*SA3*d&
  &Alpha2KanAlter())/(CB*MW*SB2*SW) - (0.5D0*CA1*CA2*CA3*EL*m12squared*SA2*dAlpha2KanAlter())/(MW*SB*SW*TB) - (0.09375D0*CA2*EL*m&
  &12squared*SA1*SA3*dAlpha2KanAlter())/(MW*SB*SW*TB) + (0.84375D0*CA2*EL*m12squared*SA1*SA22*SA3*dAlpha2KanAlter())/(MW*SB*SW*TB&
  &) + (0.125D0*CA2*CA3*EL*m12squared*SA1*SA2*TB*dAlpha2KanAlter())/(CB*MW*SW) - (0.09375D0*CA1*CA2*EL*m12squared*SA3*TB*dAlpha2K&
  &anAlter())/(CB*MW*SW) + (0.84375D0*CA1*CA2*EL*m12squared*SA22*SA3*TB*dAlpha2KanAlter())/(CB*MW*SW) + (0.5D0*MH12*SA2*SA3*dAlph&
  &a2KanAlter())/vS - (4.5D0*CA22*MH12*SA2*SA3*dAlpha2KanAlter())/vS + (0.25D0*MH22*SA2*SA3*dAlpha2KanAlter())/vS - (2.25D0*CA22*&
  &MH22*SA2*SA3*dAlpha2KanAlter())/vS + (0.1875D0*CA1*CA3*EL*MH12*SA2*dAlpha3KanAlter())/(CB*MW*SW) + (0.5625D0*CA1*CA22*CA3*EL*M&
  &H12*SA2*dAlpha3KanAlter())/(CB*MW*SW) + (0.09375D0*CA1*CA3*EL*MH22*SA2*dAlpha3KanAlter())/(CB*MW*SW) + (0.28125D0*CA1*CA22*CA3&
  &*EL*MH22*SA2*dAlpha3KanAlter())/(CB*MW*SW) + (0.28125D0*CA3*EL*m12squared*SA1*SA2*dAlpha3KanAlter())/(CB*MW*SW) + (0.84375D0*C&
  &A22*CA3*EL*m12squared*SA1*SA2*dAlpha3KanAlter())/(CB*MW*SW) - (0.1875D0*CA1*CA3*EL*MH12*SA12*SA2*dAlpha3KanAlter())/(CB*MW*SW)&
  & - (0.5625D0*CA1*CA22*CA3*EL*MH12*SA12*SA2*dAlpha3KanAlter())/(CB*MW*SW) - (0.09375D0*CA1*CA3*EL*MH22*SA12*SA2*dAlpha3KanAlter&
  &())/(CB*MW*SW) - (0.28125D0*CA1*CA22*CA3*EL*MH22*SA12*SA2*dAlpha3KanAlter())/(CB*MW*SW) + (0.125D0*CA1*EL*m12squared*SA3*dAlph&
  &a3KanAlter())/(CB*MW*SW) + (0.125D0*CA1*CA22*EL*m12squared*SA3*dAlpha3KanAlter())/(CB*MW*SW) - (0.125D0*EL*MH12*SA1*SA3*dAlpha&
  &3KanAlter())/(CB*MW*SW) - (0.375D0*CA12*EL*MH12*SA1*SA3*dAlpha3KanAlter())/(CB*MW*SW) - (0.125D0*CA22*EL*MH12*SA1*SA3*dAlpha3K&
  &anAlter())/(CB*MW*SW) - (0.375D0*CA12*CA22*EL*MH12*SA1*SA3*dAlpha3KanAlter())/(CB*MW*SW) - (0.0625D0*EL*MH22*SA1*SA3*dAlpha3Ka&
  &nAlter())/(CB*MW*SW) - (0.1875D0*CA12*EL*MH22*SA1*SA3*dAlpha3KanAlter())/(CB*MW*SW) - (0.0625D0*CA22*EL*MH22*SA1*SA3*dAlpha3Ka&
  &nAlter())/(CB*MW*SW) - (0.1875D0*CA12*CA22*EL*MH22*SA1*SA3*dAlpha3KanAlter())/(CB*MW*SW) - (0.125D0*CA1*EL*m12squared*SA22*SA3&
  &*dAlpha3KanAlter())/(CB*MW*SW) + (0.125D0*EL*MH12*SA1*SA22*SA3*dAlpha3KanAlter())/(CB*MW*SW) + (0.375D0*CA12*EL*MH12*SA1*SA22*&
  &SA3*dAlpha3KanAlter())/(CB*MW*SW) + (0.0625D0*EL*MH22*SA1*SA22*SA3*dAlpha3KanAlter())/(CB*MW*SW) + (0.1875D0*CA12*EL*MH22*SA1*&
  &SA22*SA3*dAlpha3KanAlter())/(CB*MW*SW) + (0.28125D0*CA1*CA3*EL*m12squared*SA2*dAlpha3KanAlter())/(MW*SB*SW) + (0.84375D0*CA1*C&
  &A22*CA3*EL*m12squared*SA2*dAlpha3KanAlter())/(MW*SB*SW) - (0.1875D0*CA1*CA3*EL*m12squared*SA2*dAlpha3KanAlter())/(CB2*MW*SB*SW&
  &) - (0.5625D0*CA1*CA22*CA3*EL*m12squared*SA2*dAlpha3KanAlter())/(CB2*MW*SB*SW) + (0.1875D0*CA3*EL*MH12*SA1*SA2*dAlpha3KanAlter&
  &())/(MW*SB*SW) - (0.1875D0*CA12*CA3*EL*MH12*SA1*SA2*dAlpha3KanAlter())/(MW*SB*SW) + (0.5625D0*CA22*CA3*EL*MH12*SA1*SA2*dAlpha3&
  &KanAlter())/(MW*SB*SW) - (0.5625D0*CA12*CA22*CA3*EL*MH12*SA1*SA2*dAlpha3KanAlter())/(MW*SB*SW) + (0.09375D0*CA3*EL*MH22*SA1*SA&
  &2*dAlpha3KanAlter())/(MW*SB*SW) - (0.09375D0*CA12*CA3*EL*MH22*SA1*SA2*dAlpha3KanAlter())/(MW*SB*SW) + (0.28125D0*CA22*CA3*EL*M&
  &H22*SA1*SA2*dAlpha3KanAlter())/(MW*SB*SW) - (0.28125D0*CA12*CA22*CA3*EL*MH22*SA1*SA2*dAlpha3KanAlter())/(MW*SB*SW) + (0.28125D&
  &0*CA1*CA3*EL*m12squared*SA12*SA2*dAlpha3KanAlter())/(CB2*MW*SB*SW) + (0.84375D0*CA1*CA22*CA3*EL*m12squared*SA12*SA2*dAlpha3Kan&
  &Alter())/(CB2*MW*SB*SW) + (0.125D0*CA1*EL*MH12*SA3*dAlpha3KanAlter())/(MW*SB*SW) + (0.125D0*CA1*CA22*EL*MH12*SA3*dAlpha3KanAlt&
  &er())/(MW*SB*SW) + (0.0625D0*CA1*EL*MH22*SA3*dAlpha3KanAlter())/(MW*SB*SW) + (0.0625D0*CA1*CA22*EL*MH22*SA3*dAlpha3KanAlter())&
  &/(MW*SB*SW) - (0.21875D0*EL*m12squared*SA1*SA3*dAlpha3KanAlter())/(MW*SB*SW) - (0.21875D0*CA22*EL*m12squared*SA1*SA3*dAlpha3Ka&
  &nAlter())/(MW*SB*SW) + (0.15625D0*EL*m12squared*SA1*SA3*dAlpha3KanAlter())/(CB2*MW*SB*SW) + (0.5625D0*CA12*EL*m12squared*SA1*S&
  &A3*dAlpha3KanAlter())/(CB2*MW*SB*SW) + (0.15625D0*CA22*EL*m12squared*SA1*SA3*dAlpha3KanAlter())/(CB2*MW*SB*SW) + (0.5625D0*CA1&
  &2*CA22*EL*m12squared*SA1*SA3*dAlpha3KanAlter())/(CB2*MW*SB*SW) + (0.375D0*CA1*EL*MH12*SA12*SA3*dAlpha3KanAlter())/(MW*SB*SW) +&
  & (0.375D0*CA1*CA22*EL*MH12*SA12*SA3*dAlpha3KanAlter())/(MW*SB*SW) + (0.1875D0*CA1*EL*MH22*SA12*SA3*dAlpha3KanAlter())/(MW*SB*S&
  &W) + (0.1875D0*CA1*CA22*EL*MH22*SA12*SA3*dAlpha3KanAlter())/(MW*SB*SW) - (0.125D0*CA1*EL*MH12*SA22*SA3*dAlpha3KanAlter())/(MW*&
  &SB*SW) - (0.0625D0*CA1*EL*MH22*SA22*SA3*dAlpha3KanAlter())/(MW*SB*SW) + (0.21875D0*EL*m12squared*SA1*SA22*SA3*dAlpha3KanAlter(&
  &))/(MW*SB*SW) - (0.15625D0*EL*m12squared*SA1*SA22*SA3*dAlpha3KanAlter())/(CB2*MW*SB*SW) - (0.5625D0*CA12*EL*m12squared*SA1*SA2&
  &2*SA3*dAlpha3KanAlter())/(CB2*MW*SB*SW) - (0.375D0*CA1*EL*MH12*SA12*SA22*SA3*dAlpha3KanAlter())/(MW*SB*SW) - (0.1875D0*CA1*EL*&
  &MH22*SA12*SA22*SA3*dAlpha3KanAlter())/(MW*SB*SW) - (0.1875D0*CA3*EL*m12squared*SA1*SA2*dAlpha3KanAlter())/(CB*MW*SB2*SW) + (0.&
  &28125D0*CA12*CA3*EL*m12squared*SA1*SA2*dAlpha3KanAlter())/(CB*MW*SB2*SW) - (0.5625D0*CA22*CA3*EL*m12squared*SA1*SA2*dAlpha3Kan&
  &Alter())/(CB*MW*SB2*SW) + (0.84375D0*CA12*CA22*CA3*EL*m12squared*SA1*SA2*dAlpha3KanAlter())/(CB*MW*SB2*SW) - (0.0625D0*CA1*EL*&
  &m12squared*SA3*dAlpha3KanAlter())/(CB*MW*SB2*SW) - (0.0625D0*CA1*CA22*EL*m12squared*SA3*dAlpha3KanAlter())/(CB*MW*SB2*SW) - (0&
  &.5625D0*CA1*EL*m12squared*SA12*SA3*dAlpha3KanAlter())/(CB*MW*SB2*SW) - (0.5625D0*CA1*CA22*EL*m12squared*SA12*SA3*dAlpha3KanAlt&
  &er())/(CB*MW*SB2*SW) + (0.0625D0*CA1*EL*m12squared*SA22*SA3*dAlpha3KanAlter())/(CB*MW*SB2*SW) + (0.5625D0*CA1*EL*m12squared*SA&
  &12*SA22*SA3*dAlpha3KanAlter())/(CB*MW*SB2*SW) - (0.09375D0*CA3*EL*m12squared*SA1*SA2*dAlpha3KanAlter())/(MW*SB*SW*TB) - (0.281&
  &25D0*CA22*CA3*EL*m12squared*SA1*SA2*dAlpha3KanAlter())/(MW*SB*SW*TB) - (0.125D0*CA1*EL*m12squared*SA3*dAlpha3KanAlter())/(MW*S&
  &B*SW*TB) - (0.125D0*CA1*CA22*EL*m12squared*SA3*dAlpha3KanAlter())/(MW*SB*SW*TB) + (0.125D0*CA1*EL*m12squared*SA22*SA3*dAlpha3K&
  &anAlter())/(MW*SB*SW*TB) - (0.09375D0*CA1*CA3*EL*m12squared*SA2*TB*dAlpha3KanAlter())/(CB*MW*SW) - (0.28125D0*CA1*CA22*CA3*EL*&
  &m12squared*SA2*TB*dAlpha3KanAlter())/(CB*MW*SW) + (0.03125D0*EL*m12squared*SA1*SA3*TB*dAlpha3KanAlter())/(CB*MW*SW) + (0.03125&
  &D0*CA22*EL*m12squared*SA1*SA3*TB*dAlpha3KanAlter())/(CB*MW*SW) - (0.03125D0*EL*m12squared*SA1*SA22*SA3*TB*dAlpha3KanAlter())/(&
  &CB*MW*SW) - (0.5D0*CA2*CA3*MH12*dAlpha3KanAlter())/vS - (0.25D0*CA2*CA3*MH22*dAlpha3KanAlter())/vS - (1.5D0*CA2*CA3*MH12*SA22*&
  &dAlpha3KanAlter())/vS - (0.75D0*CA2*CA3*MH22*SA22*dAlpha3KanAlter())/vS + (0.015625D0*CA3*EL*m12squared*SA1*dBeta2KanAlter())/&
  &(CB*MW*SW) + (0.015625D0*CA22*CA3*EL*m12squared*SA1*dBeta2KanAlter())/(CB*MW*SW) - (0.015625D0*CA3*EL*m12squared*SA1*SA22*dBet&
  &a2KanAlter())/(CB*MW*SW) + (0.09375D0*CA1*EL*m12squared*SA2*SA3*dBeta2KanAlter())/(CB*MW*SW) + (0.28125D0*CA1*CA22*EL*m12squar&
  &ed*SA2*SA3*dBeta2KanAlter())/(CB*MW*SW) - (0.0625D0*CA1*CA3*EL*m12squared*dBeta2KanAlter())/(MW*SB*SW) - (0.0625D0*CA1*CA22*CA&
  &3*EL*m12squared*dBeta2KanAlter())/(MW*SB*SW) + (0.0625D0*CA1*CA3*EL*m12squared*dBeta2KanAlter())/(CB2*MW*SB*SW) + (0.0625D0*CA&
  &1*CA22*CA3*EL*m12squared*dBeta2KanAlter())/(CB2*MW*SB*SW) - (0.0625D0*CA3*EL*MH12*SA1*dBeta2KanAlter())/(MW*SB*SW) - (0.1875D0&
  &*CA12*CA3*EL*MH12*SA1*dBeta2KanAlter())/(MW*SB*SW) - (0.0625D0*CA22*CA3*EL*MH12*SA1*dBeta2KanAlter())/(MW*SB*SW) - (0.1875D0*C&
  &A12*CA22*CA3*EL*MH12*SA1*dBeta2KanAlter())/(MW*SB*SW) + (0.0625D0*CA3*EL*MH12*SA1*dBeta2KanAlter())/(CB2*MW*SB*SW) + (0.1875D0&
  &*CA12*CA3*EL*MH12*SA1*dBeta2KanAlter())/(CB2*MW*SB*SW) + (0.0625D0*CA22*CA3*EL*MH12*SA1*dBeta2KanAlter())/(CB2*MW*SB*SW) + (0.&
  &1875D0*CA12*CA22*CA3*EL*MH12*SA1*dBeta2KanAlter())/(CB2*MW*SB*SW) - (0.03125D0*CA3*EL*MH22*SA1*dBeta2KanAlter())/(MW*SB*SW) - &
  &(0.09375D0*CA12*CA3*EL*MH22*SA1*dBeta2KanAlter())/(MW*SB*SW) - (0.03125D0*CA22*CA3*EL*MH22*SA1*dBeta2KanAlter())/(MW*SB*SW) - &
  &(0.09375D0*CA12*CA22*CA3*EL*MH22*SA1*dBeta2KanAlter())/(MW*SB*SW) + (0.03125D0*CA3*EL*MH22*SA1*dBeta2KanAlter())/(CB2*MW*SB*SW&
  &) + (0.09375D0*CA12*CA3*EL*MH22*SA1*dBeta2KanAlter())/(CB2*MW*SB*SW) + (0.03125D0*CA22*CA3*EL*MH22*SA1*dBeta2KanAlter())/(CB2*&
  &MW*SB*SW) + (0.09375D0*CA12*CA22*CA3*EL*MH22*SA1*dBeta2KanAlter())/(CB2*MW*SB*SW) + (0.5625D0*CA1*CA3*EL*m12squared*SA12*dBeta&
  &2KanAlter())/(CB2*MW*SB*SW) + (0.5625D0*CA1*CA22*CA3*EL*m12squared*SA12*dBeta2KanAlter())/(CB2*MW*SB*SW) + (0.0625D0*CA1*CA3*E&
  &L*m12squared*SA22*dBeta2KanAlter())/(MW*SB*SW) - (0.0625D0*CA1*CA3*EL*m12squared*SA22*dBeta2KanAlter())/(CB2*MW*SB*SW) + (0.06&
  &25D0*CA3*EL*MH12*SA1*SA22*dBeta2KanAlter())/(MW*SB*SW) + (0.1875D0*CA12*CA3*EL*MH12*SA1*SA22*dBeta2KanAlter())/(MW*SB*SW) - (0&
  &.0625D0*CA3*EL*MH12*SA1*SA22*dBeta2KanAlter())/(CB2*MW*SB*SW) - (0.1875D0*CA12*CA3*EL*MH12*SA1*SA22*dBeta2KanAlter())/(CB2*MW*&
  &SB*SW) + (0.03125D0*CA3*EL*MH22*SA1*SA22*dBeta2KanAlter())/(MW*SB*SW) + (0.09375D0*CA12*CA3*EL*MH22*SA1*SA22*dBeta2KanAlter())&
  &/(MW*SB*SW) - (0.03125D0*CA3*EL*MH22*SA1*SA22*dBeta2KanAlter())/(CB2*MW*SB*SW) - (0.09375D0*CA12*CA3*EL*MH22*SA1*SA22*dBeta2Ka&
  &nAlter())/(CB2*MW*SB*SW) - (0.5625D0*CA1*CA3*EL*m12squared*SA12*SA22*dBeta2KanAlter())/(CB2*MW*SB*SW) - (0.09375D0*CA1*EL*MH12&
  &*SA2*SA3*dBeta2KanAlter())/(MW*SB*SW) - (0.28125D0*CA1*CA22*EL*MH12*SA2*SA3*dBeta2KanAlter())/(MW*SB*SW) + (0.09375D0*CA1*EL*M&
  &H12*SA2*SA3*dBeta2KanAlter())/(CB2*MW*SB*SW) + (0.28125D0*CA1*CA22*EL*MH12*SA2*SA3*dBeta2KanAlter())/(CB2*MW*SB*SW) - (0.04687&
  &5D0*CA1*EL*MH22*SA2*SA3*dBeta2KanAlter())/(MW*SB*SW) - (0.140625D0*CA1*CA22*EL*MH22*SA2*SA3*dBeta2KanAlter())/(MW*SB*SW) + (0.&
  &046875D0*CA1*EL*MH22*SA2*SA3*dBeta2KanAlter())/(CB2*MW*SB*SW) + (0.140625D0*CA1*CA22*EL*MH22*SA2*SA3*dBeta2KanAlter())/(CB2*MW&
  &*SB*SW) - (0.140625D0*EL*m12squared*SA1*SA2*SA3*dBeta2KanAlter())/(MW*SB*SW) - (0.421875D0*CA22*EL*m12squared*SA1*SA2*SA3*dBet&
  &a2KanAlter())/(MW*SB*SW) - (0.046875D0*EL*m12squared*SA1*SA2*SA3*dBeta2KanAlter())/(CB2*MW*SB*SW) + (0.28125D0*CA12*EL*m12squa&
  &red*SA1*SA2*SA3*dBeta2KanAlter())/(CB2*MW*SB*SW) - (0.140625D0*CA22*EL*m12squared*SA1*SA2*SA3*dBeta2KanAlter())/(CB2*MW*SB*SW)&
  & + (0.84375D0*CA12*CA22*EL*m12squared*SA1*SA2*SA3*dBeta2KanAlter())/(CB2*MW*SB*SW) + (0.09375D0*CA1*EL*MH12*SA12*SA2*SA3*dBeta&
  &2KanAlter())/(MW*SB*SW) + (0.28125D0*CA1*CA22*EL*MH12*SA12*SA2*SA3*dBeta2KanAlter())/(MW*SB*SW) - (0.09375D0*CA1*EL*MH12*SA12*&
  &SA2*SA3*dBeta2KanAlter())/(CB2*MW*SB*SW) - (0.28125D0*CA1*CA22*EL*MH12*SA12*SA2*SA3*dBeta2KanAlter())/(CB2*MW*SB*SW) + (0.0468&
  &75D0*CA1*EL*MH22*SA12*SA2*SA3*dBeta2KanAlter())/(MW*SB*SW) + (0.140625D0*CA1*CA22*EL*MH22*SA12*SA2*SA3*dBeta2KanAlter())/(MW*S&
  &B*SW) - (0.046875D0*CA1*EL*MH22*SA12*SA2*SA3*dBeta2KanAlter())/(CB2*MW*SB*SW) - (0.140625D0*CA1*CA22*EL*MH22*SA12*SA2*SA3*dBet&
  &a2KanAlter())/(CB2*MW*SB*SW) + (0.203125D0*CA3*EL*m12squared*SA1*dBeta2KanAlter())/(CB*MW*SB2*SW) + (0.84375D0*CA12*CA3*EL*m12&
  &squared*SA1*dBeta2KanAlter())/(CB*MW*SB2*SW) + (0.203125D0*CA22*CA3*EL*m12squared*SA1*dBeta2KanAlter())/(CB*MW*SB2*SW) + (0.84&
  &375D0*CA12*CA22*CA3*EL*m12squared*SA1*dBeta2KanAlter())/(CB*MW*SB2*SW) - (0.203125D0*CA3*EL*m12squared*SA1*SA22*dBeta2KanAlter&
  &())/(CB*MW*SB2*SW) - (0.84375D0*CA12*CA3*EL*m12squared*SA1*SA22*dBeta2KanAlter())/(CB*MW*SB2*SW) + (0.10546875D0*CA1*EL*m12squ&
  &ared*SA2*SA3*dBeta2KanAlter())/(CB*MW*SB2*SW) + (0.31640625D0*CA1*CA22*EL*m12squared*SA2*SA3*dBeta2KanAlter())/(CB*MW*SB2*SW) &
  &- (0.38671875D0*CA1*EL*m12squared*SA12*SA2*SA3*dBeta2KanAlter())/(CB*MW*SB2*SW) - (1.16015625D0*CA1*CA22*EL*m12squared*SA12*SA&
  &2*SA3*dBeta2KanAlter())/(CB*MW*SB2*SW) + (0.125D0*CA1*CA3*EL*MH12*dBeta2KanAlter())/(MW*SB*SW*TB) + (0.125D0*CA1*CA22*CA3*EL*M&
  &H12*dBeta2KanAlter())/(MW*SB*SW*TB) + (0.0625D0*CA1*CA3*EL*MH22*dBeta2KanAlter())/(MW*SB*SW*TB) + (0.0625D0*CA1*CA22*CA3*EL*MH&
  &22*dBeta2KanAlter())/(MW*SB*SW*TB) - (0.203125D0*CA3*EL*m12squared*SA1*dBeta2KanAlter())/(MW*SB*SW*TB) - (0.203125D0*CA22*CA3*&
  &EL*m12squared*SA1*dBeta2KanAlter())/(MW*SB*SW*TB) + (0.375D0*CA1*CA3*EL*MH12*SA12*dBeta2KanAlter())/(MW*SB*SW*TB) + (0.375D0*C&
  &A1*CA22*CA3*EL*MH12*SA12*dBeta2KanAlter())/(MW*SB*SW*TB) + (0.1875D0*CA1*CA3*EL*MH22*SA12*dBeta2KanAlter())/(MW*SB*SW*TB) + (0&
  &.1875D0*CA1*CA22*CA3*EL*MH22*SA12*dBeta2KanAlter())/(MW*SB*SW*TB) - (0.125D0*CA1*CA3*EL*MH12*SA22*dBeta2KanAlter())/(MW*SB*SW*&
  &TB) - (0.0625D0*CA1*CA3*EL*MH22*SA22*dBeta2KanAlter())/(MW*SB*SW*TB) + (0.203125D0*CA3*EL*m12squared*SA1*SA22*dBeta2KanAlter()&
  &)/(MW*SB*SW*TB) - (0.375D0*CA1*CA3*EL*MH12*SA12*SA22*dBeta2KanAlter())/(MW*SB*SW*TB) - (0.1875D0*CA1*CA3*EL*MH22*SA12*SA22*dBe&
  &ta2KanAlter())/(MW*SB*SW*TB) - (0.140625D0*CA1*EL*m12squared*SA2*SA3*dBeta2KanAlter())/(MW*SB*SW*TB) - (0.421875D0*CA1*CA22*EL&
  &*m12squared*SA2*SA3*dBeta2KanAlter())/(MW*SB*SW*TB) - (0.1875D0*EL*MH12*SA1*SA2*SA3*dBeta2KanAlter())/(MW*SB*SW*TB) + (0.1875D&
  &0*CA12*EL*MH12*SA1*SA2*SA3*dBeta2KanAlter())/(MW*SB*SW*TB) - (0.5625D0*CA22*EL*MH12*SA1*SA2*SA3*dBeta2KanAlter())/(MW*SB*SW*TB&
  &) + (0.5625D0*CA12*CA22*EL*MH12*SA1*SA2*SA3*dBeta2KanAlter())/(MW*SB*SW*TB) - (0.09375D0*EL*MH22*SA1*SA2*SA3*dBeta2KanAlter())&
  &/(MW*SB*SW*TB) + (0.09375D0*CA12*EL*MH22*SA1*SA2*SA3*dBeta2KanAlter())/(MW*SB*SW*TB) - (0.28125D0*CA22*EL*MH22*SA1*SA2*SA3*dBe&
  &ta2KanAlter())/(MW*SB*SW*TB) + (0.28125D0*CA12*CA22*EL*MH22*SA1*SA2*SA3*dBeta2KanAlter())/(MW*SB*SW*TB) - (0.125D0*CA1*CA3*EL*&
  &m12squared*TB*dBeta2KanAlter())/(CB*MW*SW) - (0.125D0*CA1*CA22*CA3*EL*m12squared*TB*dBeta2KanAlter())/(CB*MW*SW) + (0.0625D0*C&
  &A3*EL*MH12*SA1*TB*dBeta2KanAlter())/(CB*MW*SW) + (0.1875D0*CA12*CA3*EL*MH12*SA1*TB*dBeta2KanAlter())/(CB*MW*SW) + (0.0625D0*CA&
  &22*CA3*EL*MH12*SA1*TB*dBeta2KanAlter())/(CB*MW*SW) + (0.1875D0*CA12*CA22*CA3*EL*MH12*SA1*TB*dBeta2KanAlter())/(CB*MW*SW) + (0.&
  &03125D0*CA3*EL*MH22*SA1*TB*dBeta2KanAlter())/(CB*MW*SW) + (0.09375D0*CA12*CA3*EL*MH22*SA1*TB*dBeta2KanAlter())/(CB*MW*SW) + (0&
  &.03125D0*CA22*CA3*EL*MH22*SA1*TB*dBeta2KanAlter())/(CB*MW*SW) + (0.09375D0*CA12*CA22*CA3*EL*MH22*SA1*TB*dBeta2KanAlter())/(CB*&
  &MW*SW) + (0.125D0*CA1*CA3*EL*m12squared*SA22*TB*dBeta2KanAlter())/(CB*MW*SW) - (0.0625D0*CA3*EL*MH12*SA1*SA22*TB*dBeta2KanAlte&
  &r())/(CB*MW*SW) - (0.1875D0*CA12*CA3*EL*MH12*SA1*SA22*TB*dBeta2KanAlter())/(CB*MW*SW) - (0.03125D0*CA3*EL*MH22*SA1*SA22*TB*dBe&
  &ta2KanAlter())/(CB*MW*SW) - (0.09375D0*CA12*CA3*EL*MH22*SA1*SA22*TB*dBeta2KanAlter())/(CB*MW*SW) + (0.09375D0*CA1*EL*MH12*SA2*&
  &SA3*TB*dBeta2KanAlter())/(CB*MW*SW) + (0.28125D0*CA1*CA22*EL*MH12*SA2*SA3*TB*dBeta2KanAlter())/(CB*MW*SW) + (0.046875D0*CA1*EL&
  &*MH22*SA2*SA3*TB*dBeta2KanAlter())/(CB*MW*SW) + (0.140625D0*CA1*CA22*EL*MH22*SA2*SA3*TB*dBeta2KanAlter())/(CB*MW*SW) + (0.1406&
  &25D0*EL*m12squared*SA1*SA2*SA3*TB*dBeta2KanAlter())/(CB*MW*SW) + (0.421875D0*CA22*EL*m12squared*SA1*SA2*SA3*TB*dBeta2KanAlter(&
  &))/(CB*MW*SW) - (0.09375D0*CA1*EL*MH12*SA12*SA2*SA3*TB*dBeta2KanAlter())/(CB*MW*SW) - (0.28125D0*CA1*CA22*EL*MH12*SA12*SA2*SA3&
  &*TB*dBeta2KanAlter())/(CB*MW*SW) - (0.046875D0*CA1*EL*MH22*SA12*SA2*SA3*TB*dBeta2KanAlter())/(CB*MW*SW) - (0.140625D0*CA1*CA22&
  &*EL*MH22*SA12*SA2*SA3*TB*dBeta2KanAlter())/(CB*MW*SW) - (0.1875D0*CA1*CA3*EL*m12squared*dBeta2KanAlter())/(MW*SB*SW*TB2) - (0.&
  &1875D0*CA1*CA22*CA3*EL*m12squared*dBeta2KanAlter())/(MW*SB*SW*TB2) + (0.1875D0*CA1*CA3*EL*m12squared*SA22*dBeta2KanAlter())/(M&
  &W*SB*SW*TB2) + (0.09375D0*EL*m12squared*SA1*SA2*SA3*dBeta2KanAlter())/(MW*SB*SW*TB2) + (0.28125D0*CA22*EL*m12squared*SA1*SA2*S&
  &A3*dBeta2KanAlter())/(MW*SB*SW*TB2) - (0.03125D0*CA3*EL*m12squared*SA1*TB2*dBeta2KanAlter())/(CB*MW*SW) - (0.03125D0*CA22*CA3*&
  &EL*m12squared*SA1*TB2*dBeta2KanAlter())/(CB*MW*SW) + (0.03125D0*CA3*EL*m12squared*SA1*SA22*TB2*dBeta2KanAlter())/(CB*MW*SW) - &
  &(0.140625D0*CA1*EL*m12squared*SA2*SA3*TB2*dBeta2KanAlter())/(CB*MW*SW) - (0.421875D0*CA1*CA22*EL*m12squared*SA2*SA3*TB2*dBeta2&
  &KanAlter())/(CB*MW*SW) + (0.375D0*CA3*EL*MH12*dAlpha1KanAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.375D0*CA22*CA3*EL*MH12*d&
  &Alpha1KanAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.1875D0*CA3*EL*MH22*dAlpha1KanAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) +&
  & (0.1875D0*CA22*CA3*EL*MH22*dAlpha1KanAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.375D0*CA3*EL*MH12*SA22*dAlpha1KanAlter()*D&
  &BLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.1875D0*CA3*EL*MH22*SA22*dAlpha1KanAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.5625D0*CA&
  &3*EL*m12squared*dAlpha1KanAlter()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.5625D0*CA22*CA3*EL*m12squared*dAlpha1KanAlter()*DB&
  &LE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) + (0.5625D0*CA3*EL*m12squared*SA22*dAlpha1KanAlter()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) -&
  & (0.1875D0*EL*MH12*SA2*SA3*dAlpha1KanAlter()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.5625D0*CA22*EL*MH12*SA2*SA3*dAlpha1KanAlter&
  &()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.09375D0*EL*MH22*SA2*SA3*dAlpha1KanAlter()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.28125&
  &D0*CA22*EL*MH22*SA2*SA3*dAlpha1KanAlter()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) + (0.28125D0*EL*m12squared*SA2*SA3*dAlpha1KanAlter(&
  &)*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.84375D0*CA22*EL*m12squared*SA2*SA3*dAlpha1KanAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW*&
  &SB2*SW) + (0.0625D0*CA2*EL*MH12*SA3*dAlpha2KanAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.03125D0*CA2*EL*MH22*SA3*dAlpha2Kan&
  &Alter()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.5625D0*CA2*EL*MH12*SA22*SA3*dAlpha2KanAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) -&
  & (0.28125D0*CA2*EL*MH22*SA22*SA3*dAlpha2KanAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.5D0*CA2*CA3*EL*MH12*SA2*dAlpha2KanAlt&
  &er()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.25D0*CA2*CA3*EL*MH22*SA2*dAlpha2KanAlter()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.09&
  &375D0*CA2*EL*m12squared*SA3*dAlpha2KanAlter()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) + (0.84375D0*CA2*EL*m12squared*SA22*SA3*dAl&
  &pha2KanAlter()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) + (0.75D0*CA2*CA3*EL*m12squared*SA2*dAlpha2KanAlter()*DBLE(CA1**INT(3.D0))&
  &)/(CB*MW*SB2*SW) + (0.0625D0*CA3*EL*MH12*SA2*dAlpha3KanAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.1875D0*CA22*CA3*EL*MH12*S&
  &A2*dAlpha3KanAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.03125D0*CA3*EL*MH22*SA2*dAlpha3KanAlter()*DBLE(CA1**INT(3.D0)))/(CB&
  &*MW*SW) + (0.09375D0*CA22*CA3*EL*MH22*SA2*dAlpha3KanAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.09375D0*CA3*EL*m12squared*SA&
  &2*dAlpha3KanAlter()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.28125D0*CA22*CA3*EL*m12squared*SA2*dAlpha3KanAlter()*DBLE(CA1**I&
  &NT(3.D0)))/(CB2*MW*SB*SW) - (0.125D0*EL*MH12*SA3*dAlpha3KanAlter()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.125D0*CA22*EL*MH12*SA&
  &3*dAlpha3KanAlter()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.0625D0*EL*MH22*SA3*dAlpha3KanAlter()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW&
  &) - (0.0625D0*CA22*EL*MH22*SA3*dAlpha3KanAlter()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) + (0.125D0*EL*MH12*SA22*SA3*dAlpha3KanAlter(&
  &)*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) + (0.0625D0*EL*MH22*SA22*SA3*dAlpha3KanAlter()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) + (0.1875D0&
  &*EL*m12squared*SA3*dAlpha3KanAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.1875D0*CA22*EL*m12squared*SA3*dAlpha3KanAlter()&
  &*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.1875D0*EL*m12squared*SA22*SA3*dAlpha3KanAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW&
  &) - (0.1875D0*CA3*EL*m12squared*dBeta2KanAlter()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.1875D0*CA22*CA3*EL*m12squared*dBeta&
  &2KanAlter()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) + (0.1875D0*CA3*EL*m12squared*SA22*dBeta2KanAlter()*DBLE(CA1**INT(3.D0)))/(CB&
  &2*MW*SB*SW) - (0.03125D0*EL*MH12*SA2*SA3*dBeta2KanAlter()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.09375D0*CA22*EL*MH12*SA2*SA3*d&
  &Beta2KanAlter()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) + (0.03125D0*EL*MH12*SA2*SA3*dBeta2KanAlter()*DBLE(CA1**INT(3.D0)))/(CB2*MW*S&
  &B*SW) + (0.09375D0*CA22*EL*MH12*SA2*SA3*dBeta2KanAlter()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.015625D0*EL*MH22*SA2*SA3*dB&
  &eta2KanAlter()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.046875D0*CA22*EL*MH22*SA2*SA3*dBeta2KanAlter()*DBLE(CA1**INT(3.D0)))/(MW*&
  &SB*SW) + (0.015625D0*EL*MH22*SA2*SA3*dBeta2KanAlter()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) + (0.046875D0*CA22*EL*MH22*SA2*SA3*&
  &dBeta2KanAlter()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) + (0.12890625D0*EL*m12squared*SA2*SA3*dBeta2KanAlter()*DBLE(CA1**INT(3.D&
  &0)))/(CB*MW*SB2*SW) + (0.38671875D0*CA22*EL*m12squared*SA2*SA3*dBeta2KanAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.125D&
  &0*CA3*EL*MH12*dBeta2KanAlter()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW*TB) - (0.125D0*CA22*CA3*EL*MH12*dBeta2KanAlter()*DBLE(CA1**INT(&
  &3.D0)))/(MW*SB*SW*TB) - (0.0625D0*CA3*EL*MH22*dBeta2KanAlter()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW*TB) - (0.0625D0*CA22*CA3*EL*MH2&
  &2*dBeta2KanAlter()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW*TB) + (0.125D0*CA3*EL*MH12*SA22*dBeta2KanAlter()*DBLE(CA1**INT(3.D0)))/(MW*&
  &SB*SW*TB) + (0.0625D0*CA3*EL*MH22*SA22*dBeta2KanAlter()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW*TB) + (0.03125D0*EL*MH12*SA2*SA3*TB*dB&
  &eta2KanAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.09375D0*CA22*EL*MH12*SA2*SA3*TB*dBeta2KanAlter()*DBLE(CA1**INT(3.D0)))/(C&
  &B*MW*SW) + (0.015625D0*EL*MH22*SA2*SA3*TB*dBeta2KanAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.046875D0*CA22*EL*MH22*SA2*SA3&
  &*TB*dBeta2KanAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.5625D0*CA1*EL*MH12*SA3*dAlpha2KanAlter()*DBLE(CA2**INT(3.D0)))/(CB*&
  &MW*SW) + (0.28125D0*CA1*EL*MH22*SA3*dAlpha2KanAlter()*DBLE(CA2**INT(3.D0)))/(CB*MW*SW) + (0.84375D0*EL*m12squared*SA1*SA3*dAlp&
  &ha2KanAlter()*DBLE(CA2**INT(3.D0)))/(CB*MW*SW) - (0.5625D0*CA1*EL*MH12*SA12*SA3*dAlpha2KanAlter()*DBLE(CA2**INT(3.D0)))/(CB*MW&
  &*SW) - (0.28125D0*CA1*EL*MH22*SA12*SA3*dAlpha2KanAlter()*DBLE(CA2**INT(3.D0)))/(CB*MW*SW) + (0.84375D0*CA1*EL*m12squared*SA3*d&
  &Alpha2KanAlter()*DBLE(CA2**INT(3.D0)))/(MW*SB*SW) - (0.5625D0*CA1*EL*m12squared*SA3*dAlpha2KanAlter()*DBLE(CA2**INT(3.D0)))/(C&
  &B2*MW*SB*SW) + (0.5625D0*EL*MH12*SA1*SA3*dAlpha2KanAlter()*DBLE(CA2**INT(3.D0)))/(MW*SB*SW) - (0.5625D0*CA12*EL*MH12*SA1*SA3*d&
  &Alpha2KanAlter()*DBLE(CA2**INT(3.D0)))/(MW*SB*SW) + (0.28125D0*EL*MH22*SA1*SA3*dAlpha2KanAlter()*DBLE(CA2**INT(3.D0)))/(MW*SB*&
  &SW) - (0.28125D0*CA12*EL*MH22*SA1*SA3*dAlpha2KanAlter()*DBLE(CA2**INT(3.D0)))/(MW*SB*SW) + (0.84375D0*CA1*EL*m12squared*SA12*S&
  &A3*dAlpha2KanAlter()*DBLE(CA2**INT(3.D0)))/(CB2*MW*SB*SW) - (0.5625D0*EL*m12squared*SA1*SA3*dAlpha2KanAlter()*DBLE(CA2**INT(3.&
  &D0)))/(CB*MW*SB2*SW) + (0.84375D0*CA12*EL*m12squared*SA1*SA3*dAlpha2KanAlter()*DBLE(CA2**INT(3.D0)))/(CB*MW*SB2*SW) - (0.28125&
  &D0*EL*m12squared*SA1*SA3*dAlpha2KanAlter()*DBLE(CA2**INT(3.D0)))/(MW*SB*SW*TB) - (0.28125D0*CA1*EL*m12squared*SA3*TB*dAlpha2Ka&
  &nAlter()*DBLE(CA2**INT(3.D0)))/(CB*MW*SW) + (0.5D0*CA3*MH12*dAlpha3KanAlter()*DBLE(CA2**INT(3.D0)))/vS + (0.25D0*CA3*MH22*dAlp&
  &ha3KanAlter()*DBLE(CA2**INT(3.D0)))/vS + (0.1875D0*EL*MH12*SA3*dAlpha2KanAlter()*DBLE(CA1**INT(3.D0))*DBLE(CA2**INT(3.D0)))/(C&
  &B*MW*SW) + (0.09375D0*EL*MH22*SA3*dAlpha2KanAlter()*DBLE(CA1**INT(3.D0))*DBLE(CA2**INT(3.D0)))/(CB*MW*SW) - (0.28125D0*EL*m12s&
  &quared*SA3*dAlpha2KanAlter()*DBLE(CA1**INT(3.D0))*DBLE(CA2**INT(3.D0)))/(CB2*MW*SB*SW) - (0.28125D0*CA3*EL*m12squared*SA1*dBet&
  &a2KanAlter()*DBLE(CB**INT(-3.D0)))/(MW*SW) - (0.84375D0*CA12*CA3*EL*m12squared*SA1*dBeta2KanAlter()*DBLE(CB**INT(-3.D0)))/(MW*&
  &SW) - (0.28125D0*CA22*CA3*EL*m12squared*SA1*dBeta2KanAlter()*DBLE(CB**INT(-3.D0)))/(MW*SW) - (0.84375D0*CA12*CA22*CA3*EL*m12sq&
  &uared*SA1*dBeta2KanAlter()*DBLE(CB**INT(-3.D0)))/(MW*SW) + (0.28125D0*CA3*EL*m12squared*SA1*SA22*dBeta2KanAlter()*DBLE(CB**INT&
  &(-3.D0)))/(MW*SW) + (0.84375D0*CA12*CA3*EL*m12squared*SA1*SA22*dBeta2KanAlter()*DBLE(CB**INT(-3.D0)))/(MW*SW) - (0.36328125D0*&
  &CA1*EL*m12squared*SA2*SA3*dBeta2KanAlter()*DBLE(CB**INT(-3.D0)))/(MW*SW) - (1.08984375D0*CA1*CA22*EL*m12squared*SA2*SA3*dBeta2&
  &KanAlter()*DBLE(CB**INT(-3.D0)))/(MW*SW) + (0.45703125D0*CA1*EL*m12squared*SA12*SA2*SA3*dBeta2KanAlter()*DBLE(CB**INT(-3.D0)))&
  &/(MW*SW) + (1.37109375D0*CA1*CA22*EL*m12squared*SA12*SA2*SA3*dBeta2KanAlter()*DBLE(CB**INT(-3.D0)))/(MW*SW) - (0.0625D0*CA3*EL&
  &*m12squared*SA1*dBeta2KanAlter()*DBLE(CB**INT(-3.D0)))/(MW*SB2*SW) - (0.28125D0*CA12*CA3*EL*m12squared*SA1*dBeta2KanAlter()*DB&
  &LE(CB**INT(-3.D0)))/(MW*SB2*SW) - (0.0625D0*CA22*CA3*EL*m12squared*SA1*dBeta2KanAlter()*DBLE(CB**INT(-3.D0)))/(MW*SB2*SW) - (0&
  &.28125D0*CA12*CA22*CA3*EL*m12squared*SA1*dBeta2KanAlter()*DBLE(CB**INT(-3.D0)))/(MW*SB2*SW) + (0.0625D0*CA3*EL*m12squared*SA1*&
  &SA22*dBeta2KanAlter()*DBLE(CB**INT(-3.D0)))/(MW*SB2*SW) + (0.28125D0*CA12*CA3*EL*m12squared*SA1*SA22*dBeta2KanAlter()*DBLE(CB*&
  &*INT(-3.D0)))/(MW*SB2*SW) - (0.05859375D0*CA1*EL*m12squared*SA2*SA3*dBeta2KanAlter()*DBLE(CB**INT(-3.D0)))/(MW*SB2*SW) - (0.17&
  &578125D0*CA1*CA22*EL*m12squared*SA2*SA3*dBeta2KanAlter()*DBLE(CB**INT(-3.D0)))/(MW*SB2*SW) + (0.10546875D0*CA1*EL*m12squared*S&
  &A12*SA2*SA3*dBeta2KanAlter()*DBLE(CB**INT(-3.D0)))/(MW*SB2*SW) + (0.31640625D0*CA1*CA22*EL*m12squared*SA12*SA2*SA3*dBeta2KanAl&
  &ter()*DBLE(CB**INT(-3.D0)))/(MW*SB2*SW) - (0.15234375D0*EL*m12squared*SA2*SA3*dBeta2KanAlter()*DBLE(CA1**INT(3.D0))*DBLE(CB**I&
  &NT(-3.D0)))/(MW*SW) - (0.45703125D0*CA22*EL*m12squared*SA2*SA3*dBeta2KanAlter()*DBLE(CA1**INT(3.D0))*DBLE(CB**INT(-3.D0)))/(MW&
  &*SW) - (0.03515625D0*EL*m12squared*SA2*SA3*dBeta2KanAlter()*DBLE(CA1**INT(3.D0))*DBLE(CB**INT(-3.D0)))/(MW*SB2*SW) - (0.105468&
  &75D0*CA22*EL*m12squared*SA2*SA3*dBeta2KanAlter()*DBLE(CA1**INT(3.D0))*DBLE(CB**INT(-3.D0)))/(MW*SB2*SW) + (0.1875D0*EL*MH12*SA&
  &2*SA3*dAlpha1KanAlter()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.5625D0*CA22*EL*MH12*SA2*SA3*dAlpha1KanAlter()*DBLE(SA1**INT(3.D0&
  &)))/(CB*MW*SW) + (0.09375D0*EL*MH22*SA2*SA3*dAlpha1KanAlter()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.28125D0*CA22*EL*MH22*SA2*S&
  &A3*dAlpha1KanAlter()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.375D0*CA3*EL*MH12*dAlpha1KanAlter()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW&
  &) + (0.375D0*CA22*CA3*EL*MH12*dAlpha1KanAlter()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.1875D0*CA3*EL*MH22*dAlpha1KanAlter()*DBL&
  &E(SA1**INT(3.D0)))/(MW*SB*SW) + (0.1875D0*CA22*CA3*EL*MH22*dAlpha1KanAlter()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0.375D0*CA3*E&
  &L*MH12*SA22*dAlpha1KanAlter()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0.1875D0*CA3*EL*MH22*SA22*dAlpha1KanAlter()*DBLE(SA1**INT(3.&
  &D0)))/(MW*SB*SW) - (0.28125D0*EL*m12squared*SA2*SA3*dAlpha1KanAlter()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.84375D0*CA22*E&
  &L*m12squared*SA2*SA3*dAlpha1KanAlter()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.5625D0*CA3*EL*m12squared*dAlpha1KanAlter()*DB&
  &LE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.5625D0*CA22*CA3*EL*m12squared*dAlpha1KanAlter()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) +&
  & (0.5625D0*CA3*EL*m12squared*SA22*dAlpha1KanAlter()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.5D0*CA2*CA3*EL*MH12*SA2*dAlpha2K&
  &anAlter()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.25D0*CA2*CA3*EL*MH22*SA2*dAlpha2KanAlter()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) - &
  &(0.75D0*CA2*CA3*EL*m12squared*SA2*dAlpha2KanAlter()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) + (0.0625D0*CA2*EL*MH12*SA3*dAlpha2Ka&
  &nAlter()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.03125D0*CA2*EL*MH22*SA3*dAlpha2KanAlter()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0&
  &.5625D0*CA2*EL*MH12*SA22*SA3*dAlpha2KanAlter()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0.28125D0*CA2*EL*MH22*SA22*SA3*dAlpha2KanAl&
  &ter()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0.09375D0*CA2*EL*m12squared*SA3*dAlpha2KanAlter()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*S&
  &W) + (0.84375D0*CA2*EL*m12squared*SA22*SA3*dAlpha2KanAlter()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.125D0*EL*MH12*SA3*dAlph&
  &a3KanAlter()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.125D0*CA22*EL*MH12*SA3*dAlpha3KanAlter()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) +&
  & (0.0625D0*EL*MH22*SA3*dAlpha3KanAlter()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.0625D0*CA22*EL*MH22*SA3*dAlpha3KanAlter()*DBLE(&
  &SA1**INT(3.D0)))/(CB*MW*SW) - (0.125D0*EL*MH12*SA22*SA3*dAlpha3KanAlter()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) - (0.0625D0*EL*MH22&
  &*SA22*SA3*dAlpha3KanAlter()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.0625D0*CA3*EL*MH12*SA2*dAlpha3KanAlter()*DBLE(SA1**INT(3.D0)&
  &))/(MW*SB*SW) + (0.1875D0*CA22*CA3*EL*MH12*SA2*dAlpha3KanAlter()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.03125D0*CA3*EL*MH22*SA2&
  &*dAlpha3KanAlter()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.09375D0*CA22*CA3*EL*MH22*SA2*dAlpha3KanAlter()*DBLE(SA1**INT(3.D0)))/&
  &(MW*SB*SW) - (0.1875D0*EL*m12squared*SA3*dAlpha3KanAlter()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.1875D0*CA22*EL*m12squared&
  &*SA3*dAlpha3KanAlter()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) + (0.1875D0*EL*m12squared*SA22*SA3*dAlpha3KanAlter()*DBLE(SA1**INT&
  &(3.D0)))/(CB2*MW*SB*SW) - (0.09375D0*CA3*EL*m12squared*SA2*dAlpha3KanAlter()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.28125D0&
  &*CA22*CA3*EL*m12squared*SA2*dAlpha3KanAlter()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.0625D0*CA3*EL*MH12*dBeta2KanAlter()*DB&
  &LE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.0625D0*CA22*CA3*EL*MH12*dBeta2KanAlter()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0.0625D0*CA3*&
  &EL*MH12*dBeta2KanAlter()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.0625D0*CA22*CA3*EL*MH12*dBeta2KanAlter()*DBLE(SA1**INT(3.D0&
  &)))/(CB2*MW*SB*SW) + (0.03125D0*CA3*EL*MH22*dBeta2KanAlter()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.03125D0*CA22*CA3*EL*MH22*dB&
  &eta2KanAlter()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0.03125D0*CA3*EL*MH22*dBeta2KanAlter()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW)&
  & - (0.03125D0*CA22*CA3*EL*MH22*dBeta2KanAlter()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.0625D0*CA3*EL*MH12*SA22*dBeta2KanAlt&
  &er()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.0625D0*CA3*EL*MH12*SA22*dBeta2KanAlter()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.&
  &03125D0*CA3*EL*MH22*SA22*dBeta2KanAlter()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.03125D0*CA3*EL*MH22*SA22*dBeta2KanAlter()*DBLE&
  &(SA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.09375D0*EL*m12squared*SA2*SA3*dBeta2KanAlter()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0&
  &.28125D0*CA22*EL*m12squared*SA2*SA3*dBeta2KanAlter()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.28125D0*CA3*EL*m12squared*dBeta&
  &2KanAlter()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.28125D0*CA22*CA3*EL*m12squared*dBeta2KanAlter()*DBLE(SA1**INT(3.D0)))/(C&
  &B*MW*SB2*SW) + (0.28125D0*CA3*EL*m12squared*SA22*dBeta2KanAlter()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.0625D0*EL*MH12*SA2&
  &*SA3*dBeta2KanAlter()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW*TB) - (0.1875D0*CA22*EL*MH12*SA2*SA3*dBeta2KanAlter()*DBLE(SA1**INT(3.D0&
  &)))/(MW*SB*SW*TB) - (0.03125D0*EL*MH22*SA2*SA3*dBeta2KanAlter()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW*TB) - (0.09375D0*CA22*EL*MH22*&
  &SA2*SA3*dBeta2KanAlter()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW*TB) - (0.0625D0*CA3*EL*MH12*TB*dBeta2KanAlter()*DBLE(SA1**INT(3.D0)))&
  &/(CB*MW*SW) - (0.0625D0*CA22*CA3*EL*MH12*TB*dBeta2KanAlter()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) - (0.03125D0*CA3*EL*MH22*TB*dBet&
  &a2KanAlter()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) - (0.03125D0*CA22*CA3*EL*MH22*TB*dBeta2KanAlter()*DBLE(SA1**INT(3.D0)))/(CB*MW*S&
  &W) + (0.0625D0*CA3*EL*MH12*SA22*TB*dBeta2KanAlter()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.03125D0*CA3*EL*MH22*SA22*TB*dBeta2Ka&
  &nAlter()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.1875D0*EL*MH12*SA3*dAlpha2KanAlter()*DBLE(CA2**INT(3.D0))*DBLE(SA1**INT(3.D0)))&
  &/(MW*SB*SW) + (0.09375D0*EL*MH22*SA3*dAlpha2KanAlter()*DBLE(CA2**INT(3.D0))*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0.28125D0*EL*m&
  &12squared*SA3*dAlpha2KanAlter()*DBLE(CA2**INT(3.D0))*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.28125D0*CA3*EL*m12squared*dBeta&
  &2KanAlter()*DBLE(CB**INT(-3.D0))*DBLE(SA1**INT(3.D0)))/(MW*SW) + (0.28125D0*CA22*CA3*EL*m12squared*dBeta2KanAlter()*DBLE(CB**I&
  &NT(-3.D0))*DBLE(SA1**INT(3.D0)))/(MW*SW) - (0.28125D0*CA3*EL*m12squared*SA22*dBeta2KanAlter()*DBLE(CB**INT(-3.D0))*DBLE(SA1**I&
  &NT(3.D0)))/(MW*SW) + (0.09375D0*CA3*EL*m12squared*dBeta2KanAlter()*DBLE(CB**INT(-3.D0))*DBLE(SA1**INT(3.D0)))/(MW*SB2*SW) + (0&
  &.09375D0*CA22*CA3*EL*m12squared*dBeta2KanAlter()*DBLE(CB**INT(-3.D0))*DBLE(SA1**INT(3.D0)))/(MW*SB2*SW) - (0.09375D0*CA3*EL*m1&
  &2squared*SA22*dBeta2KanAlter()*DBLE(CB**INT(-3.D0))*DBLE(SA1**INT(3.D0)))/(MW*SB2*SW) - (0.28125D0*CA1*EL*m12squared*SA3*dAlph&
  &a1KanAlter()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.1875D0*EL*MH12*SA1*SA3*dAlpha1KanAlter()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) +&
  & (0.5625D0*CA12*EL*MH12*SA1*SA3*dAlpha1KanAlter()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.09375D0*EL*MH22*SA1*SA3*dAlpha1KanAlte&
  &r()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.28125D0*CA12*EL*MH22*SA1*SA3*dAlpha1KanAlter()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0&
  &.1875D0*CA1*EL*MH12*SA3*dAlpha1KanAlter()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.09375D0*CA1*EL*MH22*SA3*dAlpha1KanAlter()*DBLE&
  &(SA2**INT(3.D0)))/(MW*SB*SW) + (0.28125D0*EL*m12squared*SA1*SA3*dAlpha1KanAlter()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.1875D0&
  &*EL*m12squared*SA1*SA3*dAlpha1KanAlter()*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) - (0.84375D0*CA12*EL*m12squared*SA1*SA3*dAlpha1K&
  &anAlter()*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) - (0.5625D0*CA1*EL*MH12*SA12*SA3*dAlpha1KanAlter()*DBLE(SA2**INT(3.D0)))/(MW*SB&
  &*SW) - (0.28125D0*CA1*EL*MH22*SA12*SA3*dAlpha1KanAlter()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) + (0.1875D0*CA1*EL*m12squared*SA3*dA&
  &lpha1KanAlter()*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) + (0.84375D0*CA1*EL*m12squared*SA12*SA3*dAlpha1KanAlter()*DBLE(SA2**INT(3&
  &.D0)))/(CB*MW*SB2*SW) + (0.09375D0*CA1*EL*m12squared*SA3*dAlpha1KanAlter()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) - (0.09375D0*EL&
  &*m12squared*SA1*SA3*TB*dAlpha1KanAlter()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (1.5D0*MH12*SA3*dAlpha2KanAlter()*DBLE(SA2**INT(3.&
  &D0)))/vS + (0.75D0*MH22*SA3*dAlpha2KanAlter()*DBLE(SA2**INT(3.D0)))/vS - (0.1875D0*CA1*CA3*EL*MH12*dAlpha3KanAlter()*DBLE(SA2*&
  &*INT(3.D0)))/(CB*MW*SW) - (0.09375D0*CA1*CA3*EL*MH22*dAlpha3KanAlter()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.28125D0*CA3*EL*m1&
  &2squared*SA1*dAlpha3KanAlter()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.1875D0*CA1*CA3*EL*MH12*SA12*dAlpha3KanAlter()*DBLE(SA2**I&
  &NT(3.D0)))/(CB*MW*SW) + (0.09375D0*CA1*CA3*EL*MH22*SA12*dAlpha3KanAlter()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.28125D0*CA1*CA&
  &3*EL*m12squared*dAlpha3KanAlter()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) + (0.1875D0*CA1*CA3*EL*m12squared*dAlpha3KanAlter()*DBLE(SA&
  &2**INT(3.D0)))/(CB2*MW*SB*SW) - (0.1875D0*CA3*EL*MH12*SA1*dAlpha3KanAlter()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) + (0.1875D0*CA12*&
  &CA3*EL*MH12*SA1*dAlpha3KanAlter()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.09375D0*CA3*EL*MH22*SA1*dAlpha3KanAlter()*DBLE(SA2**IN&
  &T(3.D0)))/(MW*SB*SW) + (0.09375D0*CA12*CA3*EL*MH22*SA1*dAlpha3KanAlter()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.28125D0*CA1*CA3&
  &*EL*m12squared*SA12*dAlpha3KanAlter()*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) + (0.1875D0*CA3*EL*m12squared*SA1*dAlpha3KanAlter()&
  &*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) - (0.28125D0*CA12*CA3*EL*m12squared*SA1*dAlpha3KanAlter()*DBLE(SA2**INT(3.D0)))/(CB*MW*S&
  &B2*SW) + (0.09375D0*CA3*EL*m12squared*SA1*dAlpha3KanAlter()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) + (0.09375D0*CA1*CA3*EL*m12squ&
  &ared*TB*dAlpha3KanAlter()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.09375D0*CA1*EL*m12squared*SA3*dBeta2KanAlter()*DBLE(SA2**INT(3&
  &.D0)))/(CB*MW*SW) + (0.09375D0*CA1*EL*MH12*SA3*dBeta2KanAlter()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.09375D0*CA1*EL*MH12*SA3*&
  &dBeta2KanAlter()*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) + (0.046875D0*CA1*EL*MH22*SA3*dBeta2KanAlter()*DBLE(SA2**INT(3.D0)))/(MW&
  &*SB*SW) - (0.046875D0*CA1*EL*MH22*SA3*dBeta2KanAlter()*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) + (0.140625D0*EL*m12squared*SA1*SA&
  &3*dBeta2KanAlter()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) + (0.046875D0*EL*m12squared*SA1*SA3*dBeta2KanAlter()*DBLE(SA2**INT(3.D0)))&
  &/(CB2*MW*SB*SW) - (0.28125D0*CA12*EL*m12squared*SA1*SA3*dBeta2KanAlter()*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) - (0.09375D0*CA1&
  &*EL*MH12*SA12*SA3*dBeta2KanAlter()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) + (0.09375D0*CA1*EL*MH12*SA12*SA3*dBeta2KanAlter()*DBLE(SA&
  &2**INT(3.D0)))/(CB2*MW*SB*SW) - (0.046875D0*CA1*EL*MH22*SA12*SA3*dBeta2KanAlter()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) + (0.046875&
  &D0*CA1*EL*MH22*SA12*SA3*dBeta2KanAlter()*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) - (0.10546875D0*CA1*EL*m12squared*SA3*dBeta2KanA&
  &lter()*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) + (0.38671875D0*CA1*EL*m12squared*SA12*SA3*dBeta2KanAlter()*DBLE(SA2**INT(3.D0)))/&
  &(CB*MW*SB2*SW) + (0.140625D0*CA1*EL*m12squared*SA3*dBeta2KanAlter()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) + (0.1875D0*EL*MH12*SA&
  &1*SA3*dBeta2KanAlter()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) - (0.1875D0*CA12*EL*MH12*SA1*SA3*dBeta2KanAlter()*DBLE(SA2**INT(3.D&
  &0)))/(MW*SB*SW*TB) + (0.09375D0*EL*MH22*SA1*SA3*dBeta2KanAlter()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) - (0.09375D0*CA12*EL*MH22&
  &*SA1*SA3*dBeta2KanAlter()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) - (0.09375D0*CA1*EL*MH12*SA3*TB*dBeta2KanAlter()*DBLE(SA2**INT(3&
  &.D0)))/(CB*MW*SW) - (0.046875D0*CA1*EL*MH22*SA3*TB*dBeta2KanAlter()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.140625D0*EL*m12squar&
  &ed*SA1*SA3*TB*dBeta2KanAlter()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.09375D0*CA1*EL*MH12*SA12*SA3*TB*dBeta2KanAlter()*DBLE(SA2&
  &**INT(3.D0)))/(CB*MW*SW) + (0.046875D0*CA1*EL*MH22*SA12*SA3*TB*dBeta2KanAlter()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.09375D0*&
  &EL*m12squared*SA1*SA3*dBeta2KanAlter()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB2) + (0.140625D0*CA1*EL*m12squared*SA3*TB2*dBeta2KanA&
  &lter()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.1875D0*EL*MH12*SA3*dAlpha1KanAlter()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(&
  &MW*SB*SW) + (0.09375D0*EL*MH22*SA3*dAlpha1KanAlter()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.28125D0*EL*m12&
  &squared*SA3*dAlpha1KanAlter()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) - (0.0625D0*CA3*EL*MH12*dAlpha3KanAlte&
  &r()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.03125D0*CA3*EL*MH22*dAlpha3KanAlter()*DBLE(CA1**INT(3.D0))*DBLE&
  &(SA2**INT(3.D0)))/(CB*MW*SW) + (0.09375D0*CA3*EL*m12squared*dAlpha3KanAlter()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB2*&
  &MW*SB*SW) + (0.03125D0*EL*MH12*SA3*dBeta2KanAlter()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.03125D0*EL*MH12&
  &*SA3*dBeta2KanAlter()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) + (0.015625D0*EL*MH22*SA3*dBeta2KanAlter()*DBL&
  &E(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.015625D0*EL*MH22*SA3*dBeta2KanAlter()*DBLE(CA1**INT(3.D0))*DBLE(SA2**I&
  &NT(3.D0)))/(CB2*MW*SB*SW) - (0.12890625D0*EL*m12squared*SA3*dBeta2KanAlter()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW&
  &*SB2*SW) - (0.03125D0*EL*MH12*SA3*TB*dBeta2KanAlter()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.015625D0*EL*M&
  &H22*SA3*TB*dBeta2KanAlter()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.36328125D0*CA1*EL*m12squared*SA3*dBeta2&
  &KanAlter()*DBLE(CB**INT(-3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SW) - (0.45703125D0*CA1*EL*m12squared*SA12*SA3*dBeta2KanAlter()*DBLE&
  &(CB**INT(-3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SW) + (0.05859375D0*CA1*EL*m12squared*SA3*dBeta2KanAlter()*DBLE(CB**INT(-3.D0))*DBL&
  &E(SA2**INT(3.D0)))/(MW*SB2*SW) - (0.10546875D0*CA1*EL*m12squared*SA12*SA3*dBeta2KanAlter()*DBLE(CB**INT(-3.D0))*DBLE(SA2**INT(&
  &3.D0)))/(MW*SB2*SW) + (0.15234375D0*EL*m12squared*SA3*dBeta2KanAlter()*DBLE(CA1**INT(3.D0))*DBLE(CB**INT(-3.D0))*DBLE(SA2**INT&
  &(3.D0)))/(MW*SW) + (0.03515625D0*EL*m12squared*SA3*dBeta2KanAlter()*DBLE(CA1**INT(3.D0))*DBLE(CB**INT(-3.D0))*DBLE(SA2**INT(3.&
  &D0)))/(MW*SB2*SW) - (0.1875D0*EL*MH12*SA3*dAlpha1KanAlter()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.09375D0&
  &*EL*MH22*SA3*dAlpha1KanAlter()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.28125D0*EL*m12squared*SA3*dAlpha1Kan&
  &Alter()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) - (0.0625D0*CA3*EL*MH12*dAlpha3KanAlter()*DBLE(SA1**INT(3.D0&
  &))*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.03125D0*CA3*EL*MH22*dAlpha3KanAlter()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(MW*&
  &SB*SW) + (0.09375D0*CA3*EL*m12squared*dAlpha3KanAlter()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) + (0.09375D0&
  &*EL*m12squared*SA3*dBeta2KanAlter()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) + (0.0625D0*EL*MH12*SA3*dBeta2Ka&
  &nAlter()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) + (0.03125D0*EL*MH22*SA3*dBeta2KanAlter()*DBLE(SA1**INT(3.D0&
  &))*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) - (0.1875D0*CA1*CA3*EL*m12squared*dBeta2KanAlter()*DBLE(SB**INT(-3.D0)))/(MW*SW) - (0.1&
  &875D0*CA1*CA22*CA3*EL*m12squared*dBeta2KanAlter()*DBLE(SB**INT(-3.D0)))/(MW*SW) - (1.125D0*CA1*CA3*EL*m12squared*SA12*dBeta2Ka&
  &nAlter()*DBLE(SB**INT(-3.D0)))/(MW*SW) - (1.125D0*CA1*CA22*CA3*EL*m12squared*SA12*dBeta2KanAlter()*DBLE(SB**INT(-3.D0)))/(MW*S&
  &W) + (0.1875D0*CA1*CA3*EL*m12squared*SA22*dBeta2KanAlter()*DBLE(SB**INT(-3.D0)))/(MW*SW) + (1.125D0*CA1*CA3*EL*m12squared*SA12&
  &*SA22*dBeta2KanAlter()*DBLE(SB**INT(-3.D0)))/(MW*SW) + (0.46875D0*EL*m12squared*SA1*SA2*SA3*dBeta2KanAlter()*DBLE(SB**INT(-3.D&
  &0)))/(MW*SW) - (0.5625D0*CA12*EL*m12squared*SA1*SA2*SA3*dBeta2KanAlter()*DBLE(SB**INT(-3.D0)))/(MW*SW) + (1.40625D0*CA22*EL*m1&
  &2squared*SA1*SA2*SA3*dBeta2KanAlter()*DBLE(SB**INT(-3.D0)))/(MW*SW) - (1.6875D0*CA12*CA22*EL*m12squared*SA1*SA2*SA3*dBeta2KanA&
  &lter()*DBLE(SB**INT(-3.D0)))/(MW*SW) + (0.375D0*CA3*EL*m12squared*dBeta2KanAlter()*DBLE(CA1**INT(3.D0))*DBLE(SB**INT(-3.D0)))/&
  &(MW*SW) + (0.375D0*CA22*CA3*EL*m12squared*dBeta2KanAlter()*DBLE(CA1**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(MW*SW) - (0.375D0*CA3*E&
  &L*m12squared*SA22*dBeta2KanAlter()*DBLE(CA1**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(MW*SW) + (0.1875D0*EL*m12squared*SA2*SA3*dBeta2&
  &KanAlter()*DBLE(SA1**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(MW*SW) + (0.5625D0*CA22*EL*m12squared*SA2*SA3*dBeta2KanAlter()*DBLE(SA1&
  &**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(MW*SW) - (0.46875D0*EL*m12squared*SA1*SA3*dBeta2KanAlter()*DBLE(SA2**INT(3.D0))*DBLE(SB**I&
  &NT(-3.D0)))/(MW*SW) + (0.5625D0*CA12*EL*m12squared*SA1*SA3*dBeta2KanAlter()*DBLE(SA2**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(MW*SW)&
  & - (0.1875D0*EL*m12squared*SA3*dBeta2KanAlter()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(MW*SW) - (0.1&
  &25D0*CA1*CA3*m12squared*dgAtMZ())/(CB*MW) - (0.125D0*CA1*CA22*CA3*m12squared*dgAtMZ())/(CB*MW) + (0.125D0*CA3*MH12*SA1*dgAtMZ(&
  &))/(CB*MW) + (0.375D0*CA12*CA3*MH12*SA1*dgAtMZ())/(CB*MW) + (0.125D0*CA22*CA3*MH12*SA1*dgAtMZ())/(CB*MW) + (0.375D0*CA12*CA22*&
  &CA3*MH12*SA1*dgAtMZ())/(CB*MW) + (0.0625D0*CA3*MH22*SA1*dgAtMZ())/(CB*MW) + (0.1875D0*CA12*CA3*MH22*SA1*dgAtMZ())/(CB*MW) + (0&
  &.0625D0*CA22*CA3*MH22*SA1*dgAtMZ())/(CB*MW) + (0.1875D0*CA12*CA22*CA3*MH22*SA1*dgAtMZ())/(CB*MW) + (0.125D0*CA1*CA3*m12squared&
  &*SA22*dgAtMZ())/(CB*MW) - (0.125D0*CA3*MH12*SA1*SA22*dgAtMZ())/(CB*MW) - (0.375D0*CA12*CA3*MH12*SA1*SA22*dgAtMZ())/(CB*MW) - (&
  &0.0625D0*CA3*MH22*SA1*SA22*dgAtMZ())/(CB*MW) - (0.1875D0*CA12*CA3*MH22*SA1*SA22*dgAtMZ())/(CB*MW) + (0.1875D0*CA1*MH12*SA2*SA3&
  &*dgAtMZ())/(CB*MW) + (0.5625D0*CA1*CA22*MH12*SA2*SA3*dgAtMZ())/(CB*MW) + (0.09375D0*CA1*MH22*SA2*SA3*dgAtMZ())/(CB*MW) + (0.28&
  &125D0*CA1*CA22*MH22*SA2*SA3*dgAtMZ())/(CB*MW) + (0.28125D0*m12squared*SA1*SA2*SA3*dgAtMZ())/(CB*MW) + (0.84375D0*CA22*m12squar&
  &ed*SA1*SA2*SA3*dgAtMZ())/(CB*MW) - (0.1875D0*CA1*MH12*SA12*SA2*SA3*dgAtMZ())/(CB*MW) - (0.5625D0*CA1*CA22*MH12*SA12*SA2*SA3*dg&
  &AtMZ())/(CB*MW) - (0.09375D0*CA1*MH22*SA12*SA2*SA3*dgAtMZ())/(CB*MW) - (0.28125D0*CA1*CA22*MH22*SA12*SA2*SA3*dgAtMZ())/(CB*MW)&
  & - (0.125D0*CA1*CA3*MH12*dgAtMZ())/(MW*SB) - (0.125D0*CA1*CA22*CA3*MH12*dgAtMZ())/(MW*SB) - (0.0625D0*CA1*CA3*MH22*dgAtMZ())/(&
  &MW*SB) - (0.0625D0*CA1*CA22*CA3*MH22*dgAtMZ())/(MW*SB) + (0.21875D0*CA3*m12squared*SA1*dgAtMZ())/(MW*SB) + (0.21875D0*CA22*CA3&
  &*m12squared*SA1*dgAtMZ())/(MW*SB) - (0.15625D0*CA3*m12squared*SA1*dgAtMZ())/(CB2*MW*SB) - (0.5625D0*CA12*CA3*m12squared*SA1*dg&
  &AtMZ())/(CB2*MW*SB) - (0.15625D0*CA22*CA3*m12squared*SA1*dgAtMZ())/(CB2*MW*SB) - (0.5625D0*CA12*CA22*CA3*m12squared*SA1*dgAtMZ&
  &())/(CB2*MW*SB) - (0.375D0*CA1*CA3*MH12*SA12*dgAtMZ())/(MW*SB) - (0.375D0*CA1*CA22*CA3*MH12*SA12*dgAtMZ())/(MW*SB) - (0.1875D0&
  &*CA1*CA3*MH22*SA12*dgAtMZ())/(MW*SB) - (0.1875D0*CA1*CA22*CA3*MH22*SA12*dgAtMZ())/(MW*SB) + (0.125D0*CA1*CA3*MH12*SA22*dgAtMZ(&
  &))/(MW*SB) + (0.0625D0*CA1*CA3*MH22*SA22*dgAtMZ())/(MW*SB) - (0.21875D0*CA3*m12squared*SA1*SA22*dgAtMZ())/(MW*SB) + (0.15625D0&
  &*CA3*m12squared*SA1*SA22*dgAtMZ())/(CB2*MW*SB) + (0.5625D0*CA12*CA3*m12squared*SA1*SA22*dgAtMZ())/(CB2*MW*SB) + (0.375D0*CA1*C&
  &A3*MH12*SA12*SA22*dgAtMZ())/(MW*SB) + (0.1875D0*CA1*CA3*MH22*SA12*SA22*dgAtMZ())/(MW*SB) + (0.28125D0*CA1*m12squared*SA2*SA3*d&
  &gAtMZ())/(MW*SB) + (0.84375D0*CA1*CA22*m12squared*SA2*SA3*dgAtMZ())/(MW*SB) - (0.1875D0*CA1*m12squared*SA2*SA3*dgAtMZ())/(CB2*&
  &MW*SB) - (0.5625D0*CA1*CA22*m12squared*SA2*SA3*dgAtMZ())/(CB2*MW*SB) + (0.1875D0*MH12*SA1*SA2*SA3*dgAtMZ())/(MW*SB) - (0.1875D&
  &0*CA12*MH12*SA1*SA2*SA3*dgAtMZ())/(MW*SB) + (0.5625D0*CA22*MH12*SA1*SA2*SA3*dgAtMZ())/(MW*SB) - (0.5625D0*CA12*CA22*MH12*SA1*S&
  &A2*SA3*dgAtMZ())/(MW*SB) + (0.09375D0*MH22*SA1*SA2*SA3*dgAtMZ())/(MW*SB) - (0.09375D0*CA12*MH22*SA1*SA2*SA3*dgAtMZ())/(MW*SB) &
  &+ (0.28125D0*CA22*MH22*SA1*SA2*SA3*dgAtMZ())/(MW*SB) - (0.28125D0*CA12*CA22*MH22*SA1*SA2*SA3*dgAtMZ())/(MW*SB) + (0.28125D0*CA&
  &1*m12squared*SA12*SA2*SA3*dgAtMZ())/(CB2*MW*SB) + (0.84375D0*CA1*CA22*m12squared*SA12*SA2*SA3*dgAtMZ())/(CB2*MW*SB) + (0.0625D&
  &0*CA1*CA3*m12squared*dgAtMZ())/(CB*MW*SB2) + (0.0625D0*CA1*CA22*CA3*m12squared*dgAtMZ())/(CB*MW*SB2) + (0.5625D0*CA1*CA3*m12sq&
  &uared*SA12*dgAtMZ())/(CB*MW*SB2) + (0.5625D0*CA1*CA22*CA3*m12squared*SA12*dgAtMZ())/(CB*MW*SB2) - (0.0625D0*CA1*CA3*m12squared&
  &*SA22*dgAtMZ())/(CB*MW*SB2) - (0.5625D0*CA1*CA3*m12squared*SA12*SA22*dgAtMZ())/(CB*MW*SB2) - (0.1875D0*m12squared*SA1*SA2*SA3*&
  &dgAtMZ())/(CB*MW*SB2) + (0.28125D0*CA12*m12squared*SA1*SA2*SA3*dgAtMZ())/(CB*MW*SB2) - (0.5625D0*CA22*m12squared*SA1*SA2*SA3*d&
  &gAtMZ())/(CB*MW*SB2) + (0.84375D0*CA12*CA22*m12squared*SA1*SA2*SA3*dgAtMZ())/(CB*MW*SB2) + (0.125D0*CA1*CA3*m12squared*dgAtMZ(&
  &))/(MW*SB*TB) + (0.125D0*CA1*CA22*CA3*m12squared*dgAtMZ())/(MW*SB*TB) - (0.125D0*CA1*CA3*m12squared*SA22*dgAtMZ())/(MW*SB*TB) &
  &- (0.09375D0*m12squared*SA1*SA2*SA3*dgAtMZ())/(MW*SB*TB) - (0.28125D0*CA22*m12squared*SA1*SA2*SA3*dgAtMZ())/(MW*SB*TB) - (0.03&
  &125D0*CA3*m12squared*SA1*TB*dgAtMZ())/(CB*MW) - (0.03125D0*CA22*CA3*m12squared*SA1*TB*dgAtMZ())/(CB*MW) + (0.03125D0*CA3*m12sq&
  &uared*SA1*SA22*TB*dgAtMZ())/(CB*MW) - (0.09375D0*CA1*m12squared*SA2*SA3*TB*dgAtMZ())/(CB*MW) - (0.28125D0*CA1*CA22*m12squared*&
  &SA2*SA3*TB*dgAtMZ())/(CB*MW) + (0.0625D0*MH12*SA2*SA3*DBLE(CA1**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.1875D0*CA22*MH12*SA2*SA3*DBL&
  &E(CA1**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.03125D0*MH22*SA2*SA3*DBLE(CA1**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.09375D0*CA22*MH22*SA&
  &2*SA3*DBLE(CA1**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.125D0*CA3*MH12*DBLE(CA1**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.125D0*CA22*CA3*MH&
  &12*DBLE(CA1**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.0625D0*CA3*MH22*DBLE(CA1**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.0625D0*CA22*CA3*MH2&
  &2*DBLE(CA1**INT(3.D0))*dgAtMZ())/(MW*SB) - (0.125D0*CA3*MH12*SA22*DBLE(CA1**INT(3.D0))*dgAtMZ())/(MW*SB) - (0.0625D0*CA3*MH22*&
  &SA22*DBLE(CA1**INT(3.D0))*dgAtMZ())/(MW*SB) - (0.09375D0*m12squared*SA2*SA3*DBLE(CA1**INT(3.D0))*dgAtMZ())/(CB2*MW*SB) - (0.28&
  &125D0*CA22*m12squared*SA2*SA3*DBLE(CA1**INT(3.D0))*dgAtMZ())/(CB2*MW*SB) - (0.1875D0*CA3*m12squared*DBLE(CA1**INT(3.D0))*dgAtM&
  &Z())/(CB*MW*SB2) - (0.1875D0*CA22*CA3*m12squared*DBLE(CA1**INT(3.D0))*dgAtMZ())/(CB*MW*SB2) + (0.1875D0*CA3*m12squared*SA22*DB&
  &LE(CA1**INT(3.D0))*dgAtMZ())/(CB*MW*SB2) - (0.125D0*CA3*MH12*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB*MW) - (0.125D0*CA22*CA3*MH12*D&
  &BLE(SA1**INT(3.D0))*dgAtMZ())/(CB*MW) - (0.0625D0*CA3*MH22*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB*MW) - (0.0625D0*CA22*CA3*MH22*DB&
  &LE(SA1**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.125D0*CA3*MH12*SA22*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.0625D0*CA3*MH22*SA22&
  &*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.1875D0*CA3*m12squared*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB2*MW*SB) + (0.1875D0*CA22&
  &*CA3*m12squared*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB2*MW*SB) - (0.1875D0*CA3*m12squared*SA22*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB2&
  &*MW*SB) + (0.0625D0*MH12*SA2*SA3*DBLE(SA1**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.1875D0*CA22*MH12*SA2*SA3*DBLE(SA1**INT(3.D0))*dgA&
  &tMZ())/(MW*SB) + (0.03125D0*MH22*SA2*SA3*DBLE(SA1**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.09375D0*CA22*MH22*SA2*SA3*DBLE(SA1**INT(3&
  &.D0))*dgAtMZ())/(MW*SB) - (0.09375D0*m12squared*SA2*SA3*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB*MW*SB2) - (0.28125D0*CA22*m12square&
  &d*SA2*SA3*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB*MW*SB2) - (0.1875D0*CA1*MH12*SA3*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW) - (0.0937&
  &5D0*CA1*MH22*SA3*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW) - (0.28125D0*m12squared*SA1*SA3*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW)&
  & + (0.1875D0*CA1*MH12*SA12*SA3*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.09375D0*CA1*MH22*SA12*SA3*DBLE(SA2**INT(3.D0))*dgAt&
  &MZ())/(CB*MW) - (0.28125D0*CA1*m12squared*SA3*DBLE(SA2**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.1875D0*CA1*m12squared*SA3*DBLE(SA2**&
  &INT(3.D0))*dgAtMZ())/(CB2*MW*SB) - (0.1875D0*MH12*SA1*SA3*DBLE(SA2**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.1875D0*CA12*MH12*SA1*SA3&
  &*DBLE(SA2**INT(3.D0))*dgAtMZ())/(MW*SB) - (0.09375D0*MH22*SA1*SA3*DBLE(SA2**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.09375D0*CA12*MH2&
  &2*SA1*SA3*DBLE(SA2**INT(3.D0))*dgAtMZ())/(MW*SB) - (0.28125D0*CA1*m12squared*SA12*SA3*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB2*MW*S&
  &B) + (0.1875D0*m12squared*SA1*SA3*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW*SB2) - (0.28125D0*CA12*m12squared*SA1*SA3*DBLE(SA2**IN&
  &T(3.D0))*dgAtMZ())/(CB*MW*SB2) + (0.09375D0*m12squared*SA1*SA3*DBLE(SA2**INT(3.D0))*dgAtMZ())/(MW*SB*TB) + (0.09375D0*CA1*m12s&
  &quared*SA3*TB*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW) - (0.0625D0*MH12*SA3*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dgAtMZ())/&
  &(CB*MW) - (0.03125D0*MH22*SA3*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.09375D0*m12squared*SA3*DBLE(CA1&
  &**INT(3.D0))*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB2*MW*SB) - (0.0625D0*MH12*SA3*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dgAtMZ(&
  &))/(MW*SB) - (0.03125D0*MH22*SA3*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.09375D0*m12squared*SA3*DBLE(&
  &SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW*SB2) - (0.125D0*CA1*CA3*EL*dm122MSBarAlter())/(CB*MW*SW) - (0.125D0*CA1*&
  &CA22*CA3*EL*dm122MSBarAlter())/(CB*MW*SW) + (0.125D0*CA1*CA3*EL*SA22*dm122MSBarAlter())/(CB*MW*SW) + (0.28125D0*EL*SA1*SA2*SA3&
  &*dm122MSBarAlter())/(CB*MW*SW) + (0.84375D0*CA22*EL*SA1*SA2*SA3*dm122MSBarAlter())/(CB*MW*SW) + (0.21875D0*CA3*EL*SA1*dm122MSB&
  &arAlter())/(MW*SB*SW) + (0.21875D0*CA22*CA3*EL*SA1*dm122MSBarAlter())/(MW*SB*SW) - (0.15625D0*CA3*EL*SA1*dm122MSBarAlter())/(C&
  &B2*MW*SB*SW) - (0.5625D0*CA12*CA3*EL*SA1*dm122MSBarAlter())/(CB2*MW*SB*SW) - (0.15625D0*CA22*CA3*EL*SA1*dm122MSBarAlter())/(CB&
  &2*MW*SB*SW) - (0.5625D0*CA12*CA22*CA3*EL*SA1*dm122MSBarAlter())/(CB2*MW*SB*SW) - (0.21875D0*CA3*EL*SA1*SA22*dm122MSBarAlter())&
  &/(MW*SB*SW) + (0.15625D0*CA3*EL*SA1*SA22*dm122MSBarAlter())/(CB2*MW*SB*SW) + (0.5625D0*CA12*CA3*EL*SA1*SA22*dm122MSBarAlter())&
  &/(CB2*MW*SB*SW) + (0.28125D0*CA1*EL*SA2*SA3*dm122MSBarAlter())/(MW*SB*SW) + (0.84375D0*CA1*CA22*EL*SA2*SA3*dm122MSBarAlter())/&
  &(MW*SB*SW) - (0.1875D0*CA1*EL*SA2*SA3*dm122MSBarAlter())/(CB2*MW*SB*SW) - (0.5625D0*CA1*CA22*EL*SA2*SA3*dm122MSBarAlter())/(CB&
  &2*MW*SB*SW) + (0.28125D0*CA1*EL*SA12*SA2*SA3*dm122MSBarAlter())/(CB2*MW*SB*SW) + (0.84375D0*CA1*CA22*EL*SA12*SA2*SA3*dm122MSBa&
  &rAlter())/(CB2*MW*SB*SW) + (0.0625D0*CA1*CA3*EL*dm122MSBarAlter())/(CB*MW*SB2*SW) + (0.0625D0*CA1*CA22*CA3*EL*dm122MSBarAlter(&
  &))/(CB*MW*SB2*SW) + (0.5625D0*CA1*CA3*EL*SA12*dm122MSBarAlter())/(CB*MW*SB2*SW) + (0.5625D0*CA1*CA22*CA3*EL*SA12*dm122MSBarAlt&
  &er())/(CB*MW*SB2*SW) - (0.0625D0*CA1*CA3*EL*SA22*dm122MSBarAlter())/(CB*MW*SB2*SW) - (0.5625D0*CA1*CA3*EL*SA12*SA22*dm122MSBar&
  &Alter())/(CB*MW*SB2*SW) - (0.1875D0*EL*SA1*SA2*SA3*dm122MSBarAlter())/(CB*MW*SB2*SW) + (0.28125D0*CA12*EL*SA1*SA2*SA3*dm122MSB&
  &arAlter())/(CB*MW*SB2*SW) - (0.5625D0*CA22*EL*SA1*SA2*SA3*dm122MSBarAlter())/(CB*MW*SB2*SW) + (0.84375D0*CA12*CA22*EL*SA1*SA2*&
  &SA3*dm122MSBarAlter())/(CB*MW*SB2*SW) + (0.125D0*CA1*CA3*EL*dm122MSBarAlter())/(MW*SB*SW*TB) + (0.125D0*CA1*CA22*CA3*EL*dm122M&
  &SBarAlter())/(MW*SB*SW*TB) - (0.125D0*CA1*CA3*EL*SA22*dm122MSBarAlter())/(MW*SB*SW*TB) - (0.09375D0*EL*SA1*SA2*SA3*dm122MSBarA&
  &lter())/(MW*SB*SW*TB) - (0.28125D0*CA22*EL*SA1*SA2*SA3*dm122MSBarAlter())/(MW*SB*SW*TB) - (0.03125D0*CA3*EL*SA1*TB*dm122MSBarA&
  &lter())/(CB*MW*SW) - (0.03125D0*CA22*CA3*EL*SA1*TB*dm122MSBarAlter())/(CB*MW*SW) + (0.03125D0*CA3*EL*SA1*SA22*TB*dm122MSBarAlt&
  &er())/(CB*MW*SW) - (0.09375D0*CA1*EL*SA2*SA3*TB*dm122MSBarAlter())/(CB*MW*SW) - (0.28125D0*CA1*CA22*EL*SA2*SA3*TB*dm122MSBarAl&
  &ter())/(CB*MW*SW) - (0.09375D0*EL*SA2*SA3*DBLE(CA1**INT(3.D0))*dm122MSBarAlter())/(CB2*MW*SB*SW) - (0.28125D0*CA22*EL*SA2*SA3*&
  &DBLE(CA1**INT(3.D0))*dm122MSBarAlter())/(CB2*MW*SB*SW) - (0.1875D0*CA3*EL*DBLE(CA1**INT(3.D0))*dm122MSBarAlter())/(CB*MW*SB2*S&
  &W) - (0.1875D0*CA22*CA3*EL*DBLE(CA1**INT(3.D0))*dm122MSBarAlter())/(CB*MW*SB2*SW) + (0.1875D0*CA3*EL*SA22*DBLE(CA1**INT(3.D0))&
  &*dm122MSBarAlter())/(CB*MW*SB2*SW) + (0.1875D0*CA3*EL*DBLE(SA1**INT(3.D0))*dm122MSBarAlter())/(CB2*MW*SB*SW) + (0.1875D0*CA22*&
  &CA3*EL*DBLE(SA1**INT(3.D0))*dm122MSBarAlter())/(CB2*MW*SB*SW) - (0.1875D0*CA3*EL*SA22*DBLE(SA1**INT(3.D0))*dm122MSBarAlter())/&
  &(CB2*MW*SB*SW) - (0.09375D0*EL*SA2*SA3*DBLE(SA1**INT(3.D0))*dm122MSBarAlter())/(CB*MW*SB2*SW) - (0.28125D0*CA22*EL*SA2*SA3*DBL&
  &E(SA1**INT(3.D0))*dm122MSBarAlter())/(CB*MW*SB2*SW) - (0.28125D0*EL*SA1*SA3*DBLE(SA2**INT(3.D0))*dm122MSBarAlter())/(CB*MW*SW)&
  & - (0.28125D0*CA1*EL*SA3*DBLE(SA2**INT(3.D0))*dm122MSBarAlter())/(MW*SB*SW) + (0.1875D0*CA1*EL*SA3*DBLE(SA2**INT(3.D0))*dm122M&
  &SBarAlter())/(CB2*MW*SB*SW) - (0.28125D0*CA1*EL*SA12*SA3*DBLE(SA2**INT(3.D0))*dm122MSBarAlter())/(CB2*MW*SB*SW) + (0.1875D0*EL&
  &*SA1*SA3*DBLE(SA2**INT(3.D0))*dm122MSBarAlter())/(CB*MW*SB2*SW) - (0.28125D0*CA12*EL*SA1*SA3*DBLE(SA2**INT(3.D0))*dm122MSBarAl&
  &ter())/(CB*MW*SB2*SW) + (0.09375D0*EL*SA1*SA3*DBLE(SA2**INT(3.D0))*dm122MSBarAlter())/(MW*SB*SW*TB) + (0.09375D0*CA1*EL*SA3*TB&
  &*DBLE(SA2**INT(3.D0))*dm122MSBarAlter())/(CB*MW*SW) + (0.09375D0*EL*SA3*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dm122MSBarAl&
  &ter())/(CB2*MW*SB*SW) + (0.09375D0*EL*SA3*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dm122MSBarAlter())/(CB*MW*SB2*SW) + (0.125&
  &D0*CA3*EL*SA1*dMH12OSAlter())/(CB*MW*SW) + (0.375D0*CA12*CA3*EL*SA1*dMH12OSAlter())/(CB*MW*SW) + (0.125D0*CA22*CA3*EL*SA1*dMH1&
  &2OSAlter())/(CB*MW*SW) + (0.375D0*CA12*CA22*CA3*EL*SA1*dMH12OSAlter())/(CB*MW*SW) - (0.125D0*CA3*EL*SA1*SA22*dMH12OSAlter())/(&
  &CB*MW*SW) - (0.375D0*CA12*CA3*EL*SA1*SA22*dMH12OSAlter())/(CB*MW*SW) + (0.1875D0*CA1*EL*SA2*SA3*dMH12OSAlter())/(CB*MW*SW) + (&
  &0.5625D0*CA1*CA22*EL*SA2*SA3*dMH12OSAlter())/(CB*MW*SW) - (0.1875D0*CA1*EL*SA12*SA2*SA3*dMH12OSAlter())/(CB*MW*SW) - (0.5625D0&
  &*CA1*CA22*EL*SA12*SA2*SA3*dMH12OSAlter())/(CB*MW*SW) - (0.125D0*CA1*CA3*EL*dMH12OSAlter())/(MW*SB*SW) - (0.125D0*CA1*CA22*CA3*&
  &EL*dMH12OSAlter())/(MW*SB*SW) - (0.375D0*CA1*CA3*EL*SA12*dMH12OSAlter())/(MW*SB*SW) - (0.375D0*CA1*CA22*CA3*EL*SA12*dMH12OSAlt&
  &er())/(MW*SB*SW) + (0.125D0*CA1*CA3*EL*SA22*dMH12OSAlter())/(MW*SB*SW) + (0.375D0*CA1*CA3*EL*SA12*SA22*dMH12OSAlter())/(MW*SB*&
  &SW) + (0.1875D0*EL*SA1*SA2*SA3*dMH12OSAlter())/(MW*SB*SW) - (0.1875D0*CA12*EL*SA1*SA2*SA3*dMH12OSAlter())/(MW*SB*SW) + (0.5625&
  &D0*CA22*EL*SA1*SA2*SA3*dMH12OSAlter())/(MW*SB*SW) - (0.5625D0*CA12*CA22*EL*SA1*SA2*SA3*dMH12OSAlter())/(MW*SB*SW) - (0.5D0*CA2&
  &*SA3*dMH12OSAlter())/vS - (1.5D0*CA2*SA22*SA3*dMH12OSAlter())/vS + (0.0625D0*EL*SA2*SA3*DBLE(CA1**INT(3.D0))*dMH12OSAlter())/(&
  &CB*MW*SW) + (0.1875D0*CA22*EL*SA2*SA3*DBLE(CA1**INT(3.D0))*dMH12OSAlter())/(CB*MW*SW) + (0.125D0*CA3*EL*DBLE(CA1**INT(3.D0))*d&
  &MH12OSAlter())/(MW*SB*SW) + (0.125D0*CA22*CA3*EL*DBLE(CA1**INT(3.D0))*dMH12OSAlter())/(MW*SB*SW) - (0.125D0*CA3*EL*SA22*DBLE(C&
  &A1**INT(3.D0))*dMH12OSAlter())/(MW*SB*SW) + (0.5D0*SA3*DBLE(CA2**INT(3.D0))*dMH12OSAlter())/vS - (0.125D0*CA3*EL*DBLE(SA1**INT&
  &(3.D0))*dMH12OSAlter())/(CB*MW*SW) - (0.125D0*CA22*CA3*EL*DBLE(SA1**INT(3.D0))*dMH12OSAlter())/(CB*MW*SW) + (0.125D0*CA3*EL*SA&
  &22*DBLE(SA1**INT(3.D0))*dMH12OSAlter())/(CB*MW*SW) + (0.0625D0*EL*SA2*SA3*DBLE(SA1**INT(3.D0))*dMH12OSAlter())/(MW*SB*SW) + (0&
  &.1875D0*CA22*EL*SA2*SA3*DBLE(SA1**INT(3.D0))*dMH12OSAlter())/(MW*SB*SW) - (0.1875D0*CA1*EL*SA3*DBLE(SA2**INT(3.D0))*dMH12OSAlt&
  &er())/(CB*MW*SW) + (0.1875D0*CA1*EL*SA12*SA3*DBLE(SA2**INT(3.D0))*dMH12OSAlter())/(CB*MW*SW) - (0.1875D0*EL*SA1*SA3*DBLE(SA2**&
  &INT(3.D0))*dMH12OSAlter())/(MW*SB*SW) + (0.1875D0*CA12*EL*SA1*SA3*DBLE(SA2**INT(3.D0))*dMH12OSAlter())/(MW*SB*SW) - (0.0625D0*&
  &EL*SA3*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dMH12OSAlter())/(CB*MW*SW) - (0.0625D0*EL*SA3*DBLE(SA1**INT(3.D0))*DBLE(SA2**&
  &INT(3.D0))*dMH12OSAlter())/(MW*SB*SW) + (0.0625D0*CA3*EL*SA1*dMH22OSAlter())/(CB*MW*SW) + (0.1875D0*CA12*CA3*EL*SA1*dMH22OSAlt&
  &er())/(CB*MW*SW) + (0.0625D0*CA22*CA3*EL*SA1*dMH22OSAlter())/(CB*MW*SW) + (0.1875D0*CA12*CA22*CA3*EL*SA1*dMH22OSAlter())/(CB*M&
  &W*SW) - (0.0625D0*CA3*EL*SA1*SA22*dMH22OSAlter())/(CB*MW*SW) - (0.1875D0*CA12*CA3*EL*SA1*SA22*dMH22OSAlter())/(CB*MW*SW) + (0.&
  &09375D0*CA1*EL*SA2*SA3*dMH22OSAlter())/(CB*MW*SW) + (0.28125D0*CA1*CA22*EL*SA2*SA3*dMH22OSAlter())/(CB*MW*SW) - (0.09375D0*CA1&
  &*EL*SA12*SA2*SA3*dMH22OSAlter())/(CB*MW*SW) - (0.28125D0*CA1*CA22*EL*SA12*SA2*SA3*dMH22OSAlter())/(CB*MW*SW) - (0.0625D0*CA1*C&
  &A3*EL*dMH22OSAlter())/(MW*SB*SW) - (0.0625D0*CA1*CA22*CA3*EL*dMH22OSAlter())/(MW*SB*SW) - (0.1875D0*CA1*CA3*EL*SA12*dMH22OSAlt&
  &er())/(MW*SB*SW) - (0.1875D0*CA1*CA22*CA3*EL*SA12*dMH22OSAlter())/(MW*SB*SW) + (0.0625D0*CA1*CA3*EL*SA22*dMH22OSAlter())/(MW*S&
  &B*SW) + (0.1875D0*CA1*CA3*EL*SA12*SA22*dMH22OSAlter())/(MW*SB*SW) + (0.09375D0*EL*SA1*SA2*SA3*dMH22OSAlter())/(MW*SB*SW) - (0.&
  &09375D0*CA12*EL*SA1*SA2*SA3*dMH22OSAlter())/(MW*SB*SW) + (0.28125D0*CA22*EL*SA1*SA2*SA3*dMH22OSAlter())/(MW*SB*SW) - (0.28125D&
  &0*CA12*CA22*EL*SA1*SA2*SA3*dMH22OSAlter())/(MW*SB*SW) - (0.25D0*CA2*SA3*dMH22OSAlter())/vS - (0.75D0*CA2*SA22*SA3*dMH22OSAlter&
  &())/vS + (0.03125D0*EL*SA2*SA3*DBLE(CA1**INT(3.D0))*dMH22OSAlter())/(CB*MW*SW) + (0.09375D0*CA22*EL*SA2*SA3*DBLE(CA1**INT(3.D0&
  &))*dMH22OSAlter())/(CB*MW*SW) + (0.0625D0*CA3*EL*DBLE(CA1**INT(3.D0))*dMH22OSAlter())/(MW*SB*SW) + (0.0625D0*CA22*CA3*EL*DBLE(&
  &CA1**INT(3.D0))*dMH22OSAlter())/(MW*SB*SW) - (0.0625D0*CA3*EL*SA22*DBLE(CA1**INT(3.D0))*dMH22OSAlter())/(MW*SB*SW) + (0.25D0*S&
  &A3*DBLE(CA2**INT(3.D0))*dMH22OSAlter())/vS - (0.0625D0*CA3*EL*DBLE(SA1**INT(3.D0))*dMH22OSAlter())/(CB*MW*SW) - (0.0625D0*CA22&
  &*CA3*EL*DBLE(SA1**INT(3.D0))*dMH22OSAlter())/(CB*MW*SW) + (0.0625D0*CA3*EL*SA22*DBLE(SA1**INT(3.D0))*dMH22OSAlter())/(CB*MW*SW&
  &) + (0.03125D0*EL*SA2*SA3*DBLE(SA1**INT(3.D0))*dMH22OSAlter())/(MW*SB*SW) + (0.09375D0*CA22*EL*SA2*SA3*DBLE(SA1**INT(3.D0))*dM&
  &H22OSAlter())/(MW*SB*SW) - (0.09375D0*CA1*EL*SA3*DBLE(SA2**INT(3.D0))*dMH22OSAlter())/(CB*MW*SW) + (0.09375D0*CA1*EL*SA12*SA3*&
  &DBLE(SA2**INT(3.D0))*dMH22OSAlter())/(CB*MW*SW) - (0.09375D0*EL*SA1*SA3*DBLE(SA2**INT(3.D0))*dMH22OSAlter())/(MW*SB*SW) + (0.0&
  &9375D0*CA12*EL*SA1*SA3*DBLE(SA2**INT(3.D0))*dMH22OSAlter())/(MW*SB*SW) - (0.03125D0*EL*SA3*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(&
  &3.D0))*dMH22OSAlter())/(CB*MW*SW) - (0.03125D0*EL*SA3*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dMH22OSAlter())/(MW*SB*SW) + (&
  &0.0625D0*CA1*CA3*EL*m12squared*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.0625D0*CA1*CA22*CA3*EL*m12squared*DBLE(MW**INT(-&
  &3.D0))*dMW2Alter())/(CB*SW) - (0.0625D0*CA3*EL*MH12*SA1*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.1875D0*CA12*CA3*EL*MH12&
  &*SA1*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.0625D0*CA22*CA3*EL*MH12*SA1*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0&
  &.1875D0*CA12*CA22*CA3*EL*MH12*SA1*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.03125D0*CA3*EL*MH22*SA1*DBLE(MW**INT(-3.D0))*&
  &dMW2Alter())/(CB*SW) - (0.09375D0*CA12*CA3*EL*MH22*SA1*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.03125D0*CA22*CA3*EL*MH22&
  &*SA1*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.09375D0*CA12*CA22*CA3*EL*MH22*SA1*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW&
  &) - (0.0625D0*CA1*CA3*EL*m12squared*SA22*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.0625D0*CA3*EL*MH12*SA1*SA22*DBLE(MW**I&
  &NT(-3.D0))*dMW2Alter())/(CB*SW) + (0.1875D0*CA12*CA3*EL*MH12*SA1*SA22*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.03125D0*C&
  &A3*EL*MH22*SA1*SA22*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.09375D0*CA12*CA3*EL*MH22*SA1*SA22*DBLE(MW**INT(-3.D0))*dMW2&
  &Alter())/(CB*SW) - (0.09375D0*CA1*EL*MH12*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.28125D0*CA1*CA22*EL*MH12*SA2*&
  &SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.046875D0*CA1*EL*MH22*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0&
  &.140625D0*CA1*CA22*EL*MH22*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.140625D0*EL*m12squared*SA1*SA2*SA3*DBLE(MW**&
  &INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.421875D0*CA22*EL*m12squared*SA1*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.0&
  &9375D0*CA1*EL*MH12*SA12*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.28125D0*CA1*CA22*EL*MH12*SA12*SA2*SA3*DBLE(MW**&
  &INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.046875D0*CA1*EL*MH22*SA12*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.140625D&
  &0*CA1*CA22*EL*MH22*SA12*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.0625D0*CA1*CA3*EL*MH12*DBLE(MW**INT(-3.D0))*dMW&
  &2Alter())/(SB*SW) + (0.0625D0*CA1*CA22*CA3*EL*MH12*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.03125D0*CA1*CA3*EL*MH22*DBLE&
  &(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.03125D0*CA1*CA22*CA3*EL*MH22*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.109375D&
  &0*CA3*EL*m12squared*SA1*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.109375D0*CA22*CA3*EL*m12squared*SA1*DBLE(MW**INT(-3.D0)&
  &)*dMW2Alter())/(SB*SW) + (0.078125D0*CA3*EL*m12squared*SA1*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB2*SB*SW) + (0.28125D0*CA12*CA3&
  &*EL*m12squared*SA1*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB2*SB*SW) + (0.078125D0*CA22*CA3*EL*m12squared*SA1*DBLE(MW**INT(-3.D0))&
  &*dMW2Alter())/(CB2*SB*SW) + (0.28125D0*CA12*CA22*CA3*EL*m12squared*SA1*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB2*SB*SW) + (0.1875&
  &D0*CA1*CA3*EL*MH12*SA12*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.1875D0*CA1*CA22*CA3*EL*MH12*SA12*DBLE(MW**INT(-3.D0))*d&
  &MW2Alter())/(SB*SW) + (0.09375D0*CA1*CA3*EL*MH22*SA12*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.09375D0*CA1*CA22*CA3*EL*M&
  &H22*SA12*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.0625D0*CA1*CA3*EL*MH12*SA22*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) &
  &- (0.03125D0*CA1*CA3*EL*MH22*SA22*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.109375D0*CA3*EL*m12squared*SA1*SA22*DBLE(MW**&
  &INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.078125D0*CA3*EL*m12squared*SA1*SA22*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB2*SB*SW) - (0.2&
  &8125D0*CA12*CA3*EL*m12squared*SA1*SA22*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB2*SB*SW) - (0.1875D0*CA1*CA3*EL*MH12*SA12*SA22*DBL&
  &E(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.09375D0*CA1*CA3*EL*MH22*SA12*SA22*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.1&
  &40625D0*CA1*EL*m12squared*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.421875D0*CA1*CA22*EL*m12squared*SA2*SA3*DBLE(&
  &MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.09375D0*CA1*EL*m12squared*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB2*SB*SW) + (0&
  &.28125D0*CA1*CA22*EL*m12squared*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB2*SB*SW) - (0.09375D0*EL*MH12*SA1*SA2*SA3*DBLE(MW&
  &**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.09375D0*CA12*EL*MH12*SA1*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.28125D&
  &0*CA22*EL*MH12*SA1*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.28125D0*CA12*CA22*EL*MH12*SA1*SA2*SA3*DBLE(MW**INT(-&
  &3.D0))*dMW2Alter())/(SB*SW) - (0.046875D0*EL*MH22*SA1*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.046875D0*CA12*EL*&
  &MH22*SA1*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.140625D0*CA22*EL*MH22*SA1*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alt&
  &er())/(SB*SW) + (0.140625D0*CA12*CA22*EL*MH22*SA1*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.140625D0*CA1*EL*m12sq&
  &uared*SA12*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB2*SB*SW) - (0.421875D0*CA1*CA22*EL*m12squared*SA12*SA2*SA3*DBLE(MW**IN&
  &T(-3.D0))*dMW2Alter())/(CB2*SB*SW) - (0.03125D0*CA1*CA3*EL*m12squared*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SB2*SW) - (0.03125&
  &D0*CA1*CA22*CA3*EL*m12squared*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SB2*SW) - (0.28125D0*CA1*CA3*EL*m12squared*SA12*DBLE(MW**I&
  &NT(-3.D0))*dMW2Alter())/(CB*SB2*SW) - (0.28125D0*CA1*CA22*CA3*EL*m12squared*SA12*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SB2*SW)&
  & + (0.03125D0*CA1*CA3*EL*m12squared*SA22*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SB2*SW) + (0.28125D0*CA1*CA3*EL*m12squared*SA12&
  &*SA22*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SB2*SW) + (0.09375D0*EL*m12squared*SA1*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(&
  &CB*SB2*SW) - (0.140625D0*CA12*EL*m12squared*SA1*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SB2*SW) + (0.28125D0*CA22*EL*m12&
  &squared*SA1*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SB2*SW) - (0.421875D0*CA12*CA22*EL*m12squared*SA1*SA2*SA3*DBLE(MW**I&
  &NT(-3.D0))*dMW2Alter())/(CB*SB2*SW) - (0.0625D0*CA1*CA3*EL*m12squared*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW*TB) - (0.0625D0&
  &*CA1*CA22*CA3*EL*m12squared*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW*TB) + (0.0625D0*CA1*CA3*EL*m12squared*SA22*DBLE(MW**INT(-&
  &3.D0))*dMW2Alter())/(SB*SW*TB) + (0.046875D0*EL*m12squared*SA1*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW*TB) + (0.14062&
  &5D0*CA22*EL*m12squared*SA1*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW*TB) + (0.015625D0*CA3*EL*m12squared*SA1*TB*DBLE(MW&
  &**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.015625D0*CA22*CA3*EL*m12squared*SA1*TB*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.&
  &015625D0*CA3*EL*m12squared*SA1*SA22*TB*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.046875D0*CA1*EL*m12squared*SA2*SA3*TB*DB&
  &LE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.140625D0*CA1*CA22*EL*m12squared*SA2*SA3*TB*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*&
  &SW) - (0.03125D0*EL*MH12*SA2*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.09375D0*CA22*EL*MH12*SA2*&
  &SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.015625D0*EL*MH22*SA2*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW*&
  &*INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.046875D0*CA22*EL*MH22*SA2*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(C&
  &B*SW) - (0.0625D0*CA3*EL*MH12*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.0625D0*CA22*CA3*EL*MH12*DBLE&
  &(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.03125D0*CA3*EL*MH22*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*&
  &dMW2Alter())/(SB*SW) - (0.03125D0*CA22*CA3*EL*MH22*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.0625D0*&
  &CA3*EL*MH12*SA22*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.03125D0*CA3*EL*MH22*SA22*DBLE(CA1**INT(3.&
  &D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.046875D0*EL*m12squared*SA2*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*d&
  &MW2Alter())/(CB2*SB*SW) + (0.140625D0*CA22*EL*m12squared*SA2*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB2*S&
  &B*SW) + (0.09375D0*CA3*EL*m12squared*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SB2*SW) + (0.09375D0*CA22*CA3*&
  &EL*m12squared*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SB2*SW) - (0.09375D0*CA3*EL*m12squared*SA22*DBLE(CA1*&
  &*INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SB2*SW) + (0.0625D0*CA3*EL*MH12*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dM&
  &W2Alter())/(CB*SW) + (0.0625D0*CA22*CA3*EL*MH12*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(CB*SW) + (0.03125D0*CA&
  &3*EL*MH22*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(CB*SW) + (0.03125D0*CA22*CA3*EL*MH22*DBLE(MW**INT(-3.D0))*DB&
  &LE(SA1**INT(3.D0))*dMW2Alter())/(CB*SW) - (0.0625D0*CA3*EL*MH12*SA22*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(C&
  &B*SW) - (0.03125D0*CA3*EL*MH22*SA22*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(CB*SW) - (0.09375D0*CA3*EL*m12squa&
  &red*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(CB2*SB*SW) - (0.09375D0*CA22*CA3*EL*m12squared*DBLE(MW**INT(-3.D0)&
  &)*DBLE(SA1**INT(3.D0))*dMW2Alter())/(CB2*SB*SW) + (0.09375D0*CA3*EL*m12squared*SA22*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*&
  &dMW2Alter())/(CB2*SB*SW) - (0.03125D0*EL*MH12*SA2*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(SB*SW) - (0.0937&
  &5D0*CA22*EL*MH12*SA2*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(SB*SW) - (0.015625D0*EL*MH22*SA2*SA3*DBLE(MW*&
  &*INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(SB*SW) - (0.046875D0*CA22*EL*MH22*SA2*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3&
  &.D0))*dMW2Alter())/(SB*SW) + (0.046875D0*EL*m12squared*SA2*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(CB*SB2*&
  &SW) + (0.140625D0*CA22*EL*m12squared*SA2*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(CB*SB2*SW) + (0.09375D0*C&
  &A1*EL*MH12*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(CB*SW) + (0.046875D0*CA1*EL*MH22*SA3*DBLE(MW**INT(-3.D0&
  &))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(CB*SW) + (0.140625D0*EL*m12squared*SA1*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW&
  &2Alter())/(CB*SW) - (0.09375D0*CA1*EL*MH12*SA12*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(CB*SW) - (0.046875&
  &D0*CA1*EL*MH22*SA12*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(CB*SW) + (0.140625D0*CA1*EL*m12squared*SA3*DBL&
  &E(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(SB*SW) - (0.09375D0*CA1*EL*m12squared*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA2**&
  &INT(3.D0))*dMW2Alter())/(CB2*SB*SW) + (0.09375D0*EL*MH12*SA1*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(SB*SW&
  &) - (0.09375D0*CA12*EL*MH12*SA1*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(SB*SW) + (0.046875D0*EL*MH22*SA1*S&
  &A3*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(SB*SW) - (0.046875D0*CA12*EL*MH22*SA1*SA3*DBLE(MW**INT(-3.D0))*DBLE&
  &(SA2**INT(3.D0))*dMW2Alter())/(SB*SW) + (0.140625D0*CA1*EL*m12squared*SA12*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2A&
  &lter())/(CB2*SB*SW) - (0.09375D0*EL*m12squared*SA1*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(CB*SB2*SW) + (0&
  &.140625D0*CA12*EL*m12squared*SA1*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(CB*SB2*SW) - (0.046875D0*EL*m12sq&
  &uared*SA1*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(SB*SW*TB) - (0.046875D0*CA1*EL*m12squared*SA3*TB*DBLE(MW&
  &**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(CB*SW) + (0.03125D0*EL*MH12*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*DBL&
  &E(SA2**INT(3.D0))*dMW2Alter())/(CB*SW) + (0.015625D0*EL*MH22*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0)&
  &)*dMW2Alter())/(CB*SW) - (0.046875D0*EL*m12squared*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alte&
  &r())/(CB2*SB*SW) + (0.03125D0*EL*MH12*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(SB*SW) &
  &+ (0.015625D0*EL*MH22*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(SB*SW) - (0.046875D0*EL&
  &*m12squared*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(CB*SB2*SW) + 0.5D0*CA2*MH12*SA3*D&
  &BLE(vS**INT(-2.D0))*dvSMSBarAlter() + 0.25D0*CA2*MH22*SA3*DBLE(vS**INT(-2.D0))*dvSMSBarAlter() + 1.5D0*CA2*MH12*SA22*SA3*DBLE(&
  &vS**INT(-2.D0))*dvSMSBarAlter() + 0.75D0*CA2*MH22*SA22*SA3*DBLE(vS**INT(-2.D0))*dvSMSBarAlter() - 0.5D0*MH12*SA3*DBLE(CA2**INT&
  &(3.D0))*DBLE(vS**INT(-2.D0))*dvSMSBarAlter() - 0.25D0*MH22*SA3*DBLE(CA2**INT(3.D0))*DBLE(vS**INT(-2.D0))*dvSMSBarAlter() &
		& + CS1S1S1f111*dZH1H2OSAlter()/2D0 + CS1S1S1f211*dZH2H2OS()/2D0 + CS1S1S1f311*dZH3H2OSAlter()/2D0 &
		& + CS1S1S1f121*dZH1H1OS()/2D0 + CS1S1S1f221*dZH2H1OSAlter()/2D0 + CS1S1S1f321*dZH3H1OSAlter()/2D0 &
		& + CS1S1S1f121*dZH1H1OS()/2D0 + CS1S1S1f221*dZH2H1OSAlter()/2D0 + CS1S1S1f321*dZH3H1OSAlter()/2D0 &
		& )
	case (5)
		dRR11 = -CA2*SA1*dAlpha1PinchPStar() - CA1*SA2*dAlpha2PinchPStar()
		dRR12 = CA1*CA2*dAlpha1PinchPStar() - SA1*SA2*dAlpha2PinchPStar()
		dRR13 = CA2*dAlpha2PinchPStar()
		dRR21 = -CA1*CA3*dAlpha1PinchPStar() - CA1*CA3*SA2*dAlpha3PinchPStar() - CA1*CA2*SA3*dAlpha2PinchPStar() + &
			& SA1*SA3*dAlpha3PinchPStar() + SA1*SA2*SA3*dAlpha1PinchPStar()
		dRR22 = -CA3*SA1*dAlpha1PinchPStar() - CA3*SA1*SA2*dAlpha3PinchPStar() - CA1*SA3*dAlpha3PinchPStar() - &
			& CA2*SA1*SA3*dAlpha2PinchPStar() - CA1*SA2*SA3*dAlpha1PinchPStar()
		dRR23 = CA2*CA3*dAlpha3PinchPStar() - SA2*SA3*dAlpha2PinchPStar()
		dRR31 = -CA1*CA2*CA3*dAlpha2PinchPStar() + CA3*SA1*dAlpha3PinchPStar() + CA3*SA1*SA2*dAlpha1PinchPStar() + &
			& CA1*SA3*dAlpha1PinchPStar() + CA1*SA2*SA3*dAlpha3PinchPStar()
		dRR32 = -CA1*CA3*dAlpha3PinchPStar() - CA2*CA3*SA1*dAlpha2PinchPStar() - CA1*CA3*SA2*dAlpha1PinchPStar() + &
			& SA1*SA3*dAlpha1PinchPStar() + SA1*SA2*SA3*dAlpha3PinchPStar()
		dRR33 = -CA3*SA2*dAlpha2PinchPStar() - CA2*SA3*dAlpha3PinchPStar()

		totalAmplitude = CS1S1S1f211*( &
&(0.125D0*CA1*CA3*EL*MH12*dAlpha1PinchPStar())/(CB*MW*SW) + (0.125D0*CA1*CA22*CA3*EL*MH12*dAlpha1PinchPStar())/(CB*MW*SW) + (0.062&
  &5D0*CA1*CA3*EL*MH22*dAlpha1PinchPStar())/(CB*MW*SW) + (0.0625D0*CA1*CA22*CA3*EL*MH22*dAlpha1PinchPStar())/(CB*MW*SW) + (0.125D&
  &0*CA3*EL*m12squared*SA1*dAlpha1PinchPStar())/(CB*MW*SW) + (0.125D0*CA22*CA3*EL*m12squared*SA1*dAlpha1PinchPStar())/(CB*MW*SW) &
  &- (1.125D0*CA1*CA3*EL*MH12*SA12*dAlpha1PinchPStar())/(CB*MW*SW) - (1.125D0*CA1*CA22*CA3*EL*MH12*SA12*dAlpha1PinchPStar())/(CB*&
  &MW*SW) - (0.5625D0*CA1*CA3*EL*MH22*SA12*dAlpha1PinchPStar())/(CB*MW*SW) - (0.5625D0*CA1*CA22*CA3*EL*MH22*SA12*dAlpha1PinchPSta&
  &r())/(CB*MW*SW) - (0.125D0*CA1*CA3*EL*MH12*SA22*dAlpha1PinchPStar())/(CB*MW*SW) - (0.0625D0*CA1*CA3*EL*MH22*SA22*dAlpha1PinchP&
  &Star())/(CB*MW*SW) - (0.125D0*CA3*EL*m12squared*SA1*SA22*dAlpha1PinchPStar())/(CB*MW*SW) + (1.125D0*CA1*CA3*EL*MH12*SA12*SA22*&
  &dAlpha1PinchPStar())/(CB*MW*SW) + (0.5625D0*CA1*CA3*EL*MH22*SA12*SA22*dAlpha1PinchPStar())/(CB*MW*SW) + (0.28125D0*CA1*EL*m12s&
  &quared*SA2*SA3*dAlpha1PinchPStar())/(CB*MW*SW) + (0.84375D0*CA1*CA22*EL*m12squared*SA2*SA3*dAlpha1PinchPStar())/(CB*MW*SW) - (&
  &0.1875D0*EL*MH12*SA1*SA2*SA3*dAlpha1PinchPStar())/(CB*MW*SW) - (0.5625D0*CA12*EL*MH12*SA1*SA2*SA3*dAlpha1PinchPStar())/(CB*MW*&
  &SW) - (0.5625D0*CA22*EL*MH12*SA1*SA2*SA3*dAlpha1PinchPStar())/(CB*MW*SW) - (1.6875D0*CA12*CA22*EL*MH12*SA1*SA2*SA3*dAlpha1Pinc&
  &hPStar())/(CB*MW*SW) - (0.09375D0*EL*MH22*SA1*SA2*SA3*dAlpha1PinchPStar())/(CB*MW*SW) - (0.28125D0*CA12*EL*MH22*SA1*SA2*SA3*dA&
  &lpha1PinchPStar())/(CB*MW*SW) - (0.28125D0*CA22*EL*MH22*SA1*SA2*SA3*dAlpha1PinchPStar())/(CB*MW*SW) - (0.84375D0*CA12*CA22*EL*&
  &MH22*SA1*SA2*SA3*dAlpha1PinchPStar())/(CB*MW*SW) + (0.21875D0*CA1*CA3*EL*m12squared*dAlpha1PinchPStar())/(MW*SB*SW) + (0.21875&
  &D0*CA1*CA22*CA3*EL*m12squared*dAlpha1PinchPStar())/(MW*SB*SW) - (0.15625D0*CA1*CA3*EL*m12squared*dAlpha1PinchPStar())/(CB2*MW*&
  &SB*SW) - (0.15625D0*CA1*CA22*CA3*EL*m12squared*dAlpha1PinchPStar())/(CB2*MW*SB*SW) + (0.125D0*CA3*EL*MH12*SA1*dAlpha1PinchPSta&
  &r())/(MW*SB*SW) - (1.125D0*CA12*CA3*EL*MH12*SA1*dAlpha1PinchPStar())/(MW*SB*SW) + (0.125D0*CA22*CA3*EL*MH12*SA1*dAlpha1PinchPS&
  &tar())/(MW*SB*SW) - (1.125D0*CA12*CA22*CA3*EL*MH12*SA1*dAlpha1PinchPStar())/(MW*SB*SW) + (0.0625D0*CA3*EL*MH22*SA1*dAlpha1Pinc&
  &hPStar())/(MW*SB*SW) - (0.5625D0*CA12*CA3*EL*MH22*SA1*dAlpha1PinchPStar())/(MW*SB*SW) + (0.0625D0*CA22*CA3*EL*MH22*SA1*dAlpha1&
  &PinchPStar())/(MW*SB*SW) - (0.5625D0*CA12*CA22*CA3*EL*MH22*SA1*dAlpha1PinchPStar())/(MW*SB*SW) + (1.6875D0*CA1*CA3*EL*m12squar&
  &ed*SA12*dAlpha1PinchPStar())/(CB2*MW*SB*SW) + (1.6875D0*CA1*CA22*CA3*EL*m12squared*SA12*dAlpha1PinchPStar())/(CB2*MW*SB*SW) - &
  &(0.21875D0*CA1*CA3*EL*m12squared*SA22*dAlpha1PinchPStar())/(MW*SB*SW) + (0.15625D0*CA1*CA3*EL*m12squared*SA22*dAlpha1PinchPSta&
  &r())/(CB2*MW*SB*SW) - (0.125D0*CA3*EL*MH12*SA1*SA22*dAlpha1PinchPStar())/(MW*SB*SW) + (1.125D0*CA12*CA3*EL*MH12*SA1*SA22*dAlph&
  &a1PinchPStar())/(MW*SB*SW) - (0.0625D0*CA3*EL*MH22*SA1*SA22*dAlpha1PinchPStar())/(MW*SB*SW) + (0.5625D0*CA12*CA3*EL*MH22*SA1*S&
  &A22*dAlpha1PinchPStar())/(MW*SB*SW) - (1.6875D0*CA1*CA3*EL*m12squared*SA12*SA22*dAlpha1PinchPStar())/(CB2*MW*SB*SW) + (0.1875D&
  &0*CA1*EL*MH12*SA2*SA3*dAlpha1PinchPStar())/(MW*SB*SW) + (0.5625D0*CA1*CA22*EL*MH12*SA2*SA3*dAlpha1PinchPStar())/(MW*SB*SW) + (&
  &0.09375D0*CA1*EL*MH22*SA2*SA3*dAlpha1PinchPStar())/(MW*SB*SW) + (0.28125D0*CA1*CA22*EL*MH22*SA2*SA3*dAlpha1PinchPStar())/(MW*S&
  &B*SW) - (0.28125D0*EL*m12squared*SA1*SA2*SA3*dAlpha1PinchPStar())/(MW*SB*SW) - (0.84375D0*CA22*EL*m12squared*SA1*SA2*SA3*dAlph&
  &a1PinchPStar())/(MW*SB*SW) + (0.1875D0*EL*m12squared*SA1*SA2*SA3*dAlpha1PinchPStar())/(CB2*MW*SB*SW) + (0.84375D0*CA12*EL*m12s&
  &quared*SA1*SA2*SA3*dAlpha1PinchPStar())/(CB2*MW*SB*SW) + (0.5625D0*CA22*EL*m12squared*SA1*SA2*SA3*dAlpha1PinchPStar())/(CB2*MW&
  &*SB*SW) + (2.53125D0*CA12*CA22*EL*m12squared*SA1*SA2*SA3*dAlpha1PinchPStar())/(CB2*MW*SB*SW) + (0.5625D0*CA1*EL*MH12*SA12*SA2*&
  &SA3*dAlpha1PinchPStar())/(MW*SB*SW) + (1.6875D0*CA1*CA22*EL*MH12*SA12*SA2*SA3*dAlpha1PinchPStar())/(MW*SB*SW) + (0.28125D0*CA1&
  &*EL*MH22*SA12*SA2*SA3*dAlpha1PinchPStar())/(MW*SB*SW) + (0.84375D0*CA1*CA22*EL*MH22*SA12*SA2*SA3*dAlpha1PinchPStar())/(MW*SB*S&
  &W) - (0.0625D0*CA3*EL*m12squared*SA1*dAlpha1PinchPStar())/(CB*MW*SB2*SW) + (1.6875D0*CA12*CA3*EL*m12squared*SA1*dAlpha1PinchPS&
  &tar())/(CB*MW*SB2*SW) - (0.0625D0*CA22*CA3*EL*m12squared*SA1*dAlpha1PinchPStar())/(CB*MW*SB2*SW) + (1.6875D0*CA12*CA22*CA3*EL*&
  &m12squared*SA1*dAlpha1PinchPStar())/(CB*MW*SB2*SW) + (0.0625D0*CA3*EL*m12squared*SA1*SA22*dAlpha1PinchPStar())/(CB*MW*SB2*SW) &
  &- (1.6875D0*CA12*CA3*EL*m12squared*SA1*SA22*dAlpha1PinchPStar())/(CB*MW*SB2*SW) - (0.1875D0*CA1*EL*m12squared*SA2*SA3*dAlpha1P&
  &inchPStar())/(CB*MW*SB2*SW) - (0.5625D0*CA1*CA22*EL*m12squared*SA2*SA3*dAlpha1PinchPStar())/(CB*MW*SB2*SW) - (0.84375D0*CA1*EL&
  &*m12squared*SA12*SA2*SA3*dAlpha1PinchPStar())/(CB*MW*SB2*SW) - (2.53125D0*CA1*CA22*EL*m12squared*SA12*SA2*SA3*dAlpha1PinchPSta&
  &r())/(CB*MW*SB2*SW) - (0.125D0*CA3*EL*m12squared*SA1*dAlpha1PinchPStar())/(MW*SB*SW*TB) - (0.125D0*CA22*CA3*EL*m12squared*SA1*&
  &dAlpha1PinchPStar())/(MW*SB*SW*TB) + (0.125D0*CA3*EL*m12squared*SA1*SA22*dAlpha1PinchPStar())/(MW*SB*SW*TB) - (0.09375D0*CA1*E&
  &L*m12squared*SA2*SA3*dAlpha1PinchPStar())/(MW*SB*SW*TB) - (0.28125D0*CA1*CA22*EL*m12squared*SA2*SA3*dAlpha1PinchPStar())/(MW*S&
  &B*SW*TB) - (0.03125D0*CA1*CA3*EL*m12squared*TB*dAlpha1PinchPStar())/(CB*MW*SW) - (0.03125D0*CA1*CA22*CA3*EL*m12squared*TB*dAlp&
  &ha1PinchPStar())/(CB*MW*SW) + (0.03125D0*CA1*CA3*EL*m12squared*SA22*TB*dAlpha1PinchPStar())/(CB*MW*SW) + (0.09375D0*EL*m12squa&
  &red*SA1*SA2*SA3*TB*dAlpha1PinchPStar())/(CB*MW*SW) + (0.28125D0*CA22*EL*m12squared*SA1*SA2*SA3*TB*dAlpha1PinchPStar())/(CB*MW*&
  &SW) + (0.5D0*CA1*CA2*CA3*EL*m12squared*SA2*dAlpha2PinchPStar())/(CB*MW*SW) - (0.5D0*CA2*CA3*EL*MH12*SA1*SA2*dAlpha2PinchPStar(&
  &))/(CB*MW*SW) - (1.5D0*CA12*CA2*CA3*EL*MH12*SA1*SA2*dAlpha2PinchPStar())/(CB*MW*SW) - (0.25D0*CA2*CA3*EL*MH22*SA1*SA2*dAlpha2P&
  &inchPStar())/(CB*MW*SW) - (0.75D0*CA12*CA2*CA3*EL*MH22*SA1*SA2*dAlpha2PinchPStar())/(CB*MW*SW) + (0.1875D0*CA1*CA2*EL*MH12*SA3&
  &*dAlpha2PinchPStar())/(CB*MW*SW) + (0.09375D0*CA1*CA2*EL*MH22*SA3*dAlpha2PinchPStar())/(CB*MW*SW) + (0.28125D0*CA2*EL*m12squar&
  &ed*SA1*SA3*dAlpha2PinchPStar())/(CB*MW*SW) - (0.1875D0*CA1*CA2*EL*MH12*SA12*SA3*dAlpha2PinchPStar())/(CB*MW*SW) - (0.09375D0*C&
  &A1*CA2*EL*MH22*SA12*SA3*dAlpha2PinchPStar())/(CB*MW*SW) - (1.6875D0*CA1*CA2*EL*MH12*SA22*SA3*dAlpha2PinchPStar())/(CB*MW*SW) -&
  & (0.84375D0*CA1*CA2*EL*MH22*SA22*SA3*dAlpha2PinchPStar())/(CB*MW*SW) - (2.53125D0*CA2*EL*m12squared*SA1*SA22*SA3*dAlpha2PinchP&
  &Star())/(CB*MW*SW) + (1.6875D0*CA1*CA2*EL*MH12*SA12*SA22*SA3*dAlpha2PinchPStar())/(CB*MW*SW) + (0.84375D0*CA1*CA2*EL*MH22*SA12&
  &*SA22*SA3*dAlpha2PinchPStar())/(CB*MW*SW) + (0.5D0*CA1*CA2*CA3*EL*MH12*SA2*dAlpha2PinchPStar())/(MW*SB*SW) + (0.25D0*CA1*CA2*C&
  &A3*EL*MH22*SA2*dAlpha2PinchPStar())/(MW*SB*SW) - (0.875D0*CA2*CA3*EL*m12squared*SA1*SA2*dAlpha2PinchPStar())/(MW*SB*SW) + (0.6&
  &25D0*CA2*CA3*EL*m12squared*SA1*SA2*dAlpha2PinchPStar())/(CB2*MW*SB*SW) + (2.25D0*CA12*CA2*CA3*EL*m12squared*SA1*SA2*dAlpha2Pin&
  &chPStar())/(CB2*MW*SB*SW) + (1.5D0*CA1*CA2*CA3*EL*MH12*SA12*SA2*dAlpha2PinchPStar())/(MW*SB*SW) + (0.75D0*CA1*CA2*CA3*EL*MH22*&
  &SA12*SA2*dAlpha2PinchPStar())/(MW*SB*SW) + (0.28125D0*CA1*CA2*EL*m12squared*SA3*dAlpha2PinchPStar())/(MW*SB*SW) - (0.1875D0*CA&
  &1*CA2*EL*m12squared*SA3*dAlpha2PinchPStar())/(CB2*MW*SB*SW) + (0.1875D0*CA2*EL*MH12*SA1*SA3*dAlpha2PinchPStar())/(MW*SB*SW) - &
  &(0.1875D0*CA12*CA2*EL*MH12*SA1*SA3*dAlpha2PinchPStar())/(MW*SB*SW) + (0.09375D0*CA2*EL*MH22*SA1*SA3*dAlpha2PinchPStar())/(MW*S&
  &B*SW) - (0.09375D0*CA12*CA2*EL*MH22*SA1*SA3*dAlpha2PinchPStar())/(MW*SB*SW) + (0.28125D0*CA1*CA2*EL*m12squared*SA12*SA3*dAlpha&
  &2PinchPStar())/(CB2*MW*SB*SW) - (2.53125D0*CA1*CA2*EL*m12squared*SA22*SA3*dAlpha2PinchPStar())/(MW*SB*SW) + (1.6875D0*CA1*CA2*&
  &EL*m12squared*SA22*SA3*dAlpha2PinchPStar())/(CB2*MW*SB*SW) - (1.6875D0*CA2*EL*MH12*SA1*SA22*SA3*dAlpha2PinchPStar())/(MW*SB*SW&
  &) + (1.6875D0*CA12*CA2*EL*MH12*SA1*SA22*SA3*dAlpha2PinchPStar())/(MW*SB*SW) - (0.84375D0*CA2*EL*MH22*SA1*SA22*SA3*dAlpha2Pinch&
  &PStar())/(MW*SB*SW) + (0.84375D0*CA12*CA2*EL*MH22*SA1*SA22*SA3*dAlpha2PinchPStar())/(MW*SB*SW) - (2.53125D0*CA1*CA2*EL*m12squa&
  &red*SA12*SA22*SA3*dAlpha2PinchPStar())/(CB2*MW*SB*SW) - (0.25D0*CA1*CA2*CA3*EL*m12squared*SA2*dAlpha2PinchPStar())/(CB*MW*SB2*&
  &SW) - (2.25D0*CA1*CA2*CA3*EL*m12squared*SA12*SA2*dAlpha2PinchPStar())/(CB*MW*SB2*SW) - (0.1875D0*CA2*EL*m12squared*SA1*SA3*dAl&
  &pha2PinchPStar())/(CB*MW*SB2*SW) + (0.28125D0*CA12*CA2*EL*m12squared*SA1*SA3*dAlpha2PinchPStar())/(CB*MW*SB2*SW) + (1.6875D0*C&
  &A2*EL*m12squared*SA1*SA22*SA3*dAlpha2PinchPStar())/(CB*MW*SB2*SW) - (2.53125D0*CA12*CA2*EL*m12squared*SA1*SA22*SA3*dAlpha2Pinc&
  &hPStar())/(CB*MW*SB2*SW) - (0.5D0*CA1*CA2*CA3*EL*m12squared*SA2*dAlpha2PinchPStar())/(MW*SB*SW*TB) - (0.09375D0*CA2*EL*m12squa&
  &red*SA1*SA3*dAlpha2PinchPStar())/(MW*SB*SW*TB) + (0.84375D0*CA2*EL*m12squared*SA1*SA22*SA3*dAlpha2PinchPStar())/(MW*SB*SW*TB) &
  &+ (0.125D0*CA2*CA3*EL*m12squared*SA1*SA2*TB*dAlpha2PinchPStar())/(CB*MW*SW) - (0.09375D0*CA1*CA2*EL*m12squared*SA3*TB*dAlpha2P&
  &inchPStar())/(CB*MW*SW) + (0.84375D0*CA1*CA2*EL*m12squared*SA22*SA3*TB*dAlpha2PinchPStar())/(CB*MW*SW) + (0.5D0*MH12*SA2*SA3*d&
  &Alpha2PinchPStar())/vS - (4.5D0*CA22*MH12*SA2*SA3*dAlpha2PinchPStar())/vS + (0.25D0*MH22*SA2*SA3*dAlpha2PinchPStar())/vS - (2.&
  &25D0*CA22*MH22*SA2*SA3*dAlpha2PinchPStar())/vS + (0.1875D0*CA1*CA3*EL*MH12*SA2*dAlpha3PinchPStar())/(CB*MW*SW) + (0.5625D0*CA1&
  &*CA22*CA3*EL*MH12*SA2*dAlpha3PinchPStar())/(CB*MW*SW) + (0.09375D0*CA1*CA3*EL*MH22*SA2*dAlpha3PinchPStar())/(CB*MW*SW) + (0.28&
  &125D0*CA1*CA22*CA3*EL*MH22*SA2*dAlpha3PinchPStar())/(CB*MW*SW) + (0.28125D0*CA3*EL*m12squared*SA1*SA2*dAlpha3PinchPStar())/(CB&
  &*MW*SW) + (0.84375D0*CA22*CA3*EL*m12squared*SA1*SA2*dAlpha3PinchPStar())/(CB*MW*SW) - (0.1875D0*CA1*CA3*EL*MH12*SA12*SA2*dAlph&
  &a3PinchPStar())/(CB*MW*SW) - (0.5625D0*CA1*CA22*CA3*EL*MH12*SA12*SA2*dAlpha3PinchPStar())/(CB*MW*SW) - (0.09375D0*CA1*CA3*EL*M&
  &H22*SA12*SA2*dAlpha3PinchPStar())/(CB*MW*SW) - (0.28125D0*CA1*CA22*CA3*EL*MH22*SA12*SA2*dAlpha3PinchPStar())/(CB*MW*SW) + (0.1&
  &25D0*CA1*EL*m12squared*SA3*dAlpha3PinchPStar())/(CB*MW*SW) + (0.125D0*CA1*CA22*EL*m12squared*SA3*dAlpha3PinchPStar())/(CB*MW*S&
  &W) - (0.125D0*EL*MH12*SA1*SA3*dAlpha3PinchPStar())/(CB*MW*SW) - (0.375D0*CA12*EL*MH12*SA1*SA3*dAlpha3PinchPStar())/(CB*MW*SW) &
  &- (0.125D0*CA22*EL*MH12*SA1*SA3*dAlpha3PinchPStar())/(CB*MW*SW) - (0.375D0*CA12*CA22*EL*MH12*SA1*SA3*dAlpha3PinchPStar())/(CB*&
  &MW*SW) - (0.0625D0*EL*MH22*SA1*SA3*dAlpha3PinchPStar())/(CB*MW*SW) - (0.1875D0*CA12*EL*MH22*SA1*SA3*dAlpha3PinchPStar())/(CB*M&
  &W*SW) - (0.0625D0*CA22*EL*MH22*SA1*SA3*dAlpha3PinchPStar())/(CB*MW*SW) - (0.1875D0*CA12*CA22*EL*MH22*SA1*SA3*dAlpha3PinchPStar&
  &())/(CB*MW*SW) - (0.125D0*CA1*EL*m12squared*SA22*SA3*dAlpha3PinchPStar())/(CB*MW*SW) + (0.125D0*EL*MH12*SA1*SA22*SA3*dAlpha3Pi&
  &nchPStar())/(CB*MW*SW) + (0.375D0*CA12*EL*MH12*SA1*SA22*SA3*dAlpha3PinchPStar())/(CB*MW*SW) + (0.0625D0*EL*MH22*SA1*SA22*SA3*d&
  &Alpha3PinchPStar())/(CB*MW*SW) + (0.1875D0*CA12*EL*MH22*SA1*SA22*SA3*dAlpha3PinchPStar())/(CB*MW*SW) + (0.28125D0*CA1*CA3*EL*m&
  &12squared*SA2*dAlpha3PinchPStar())/(MW*SB*SW) + (0.84375D0*CA1*CA22*CA3*EL*m12squared*SA2*dAlpha3PinchPStar())/(MW*SB*SW) - (0&
  &.1875D0*CA1*CA3*EL*m12squared*SA2*dAlpha3PinchPStar())/(CB2*MW*SB*SW) - (0.5625D0*CA1*CA22*CA3*EL*m12squared*SA2*dAlpha3PinchP&
  &Star())/(CB2*MW*SB*SW) + (0.1875D0*CA3*EL*MH12*SA1*SA2*dAlpha3PinchPStar())/(MW*SB*SW) - (0.1875D0*CA12*CA3*EL*MH12*SA1*SA2*dA&
  &lpha3PinchPStar())/(MW*SB*SW) + (0.5625D0*CA22*CA3*EL*MH12*SA1*SA2*dAlpha3PinchPStar())/(MW*SB*SW) - (0.5625D0*CA12*CA22*CA3*E&
  &L*MH12*SA1*SA2*dAlpha3PinchPStar())/(MW*SB*SW) + (0.09375D0*CA3*EL*MH22*SA1*SA2*dAlpha3PinchPStar())/(MW*SB*SW) - (0.09375D0*C&
  &A12*CA3*EL*MH22*SA1*SA2*dAlpha3PinchPStar())/(MW*SB*SW) + (0.28125D0*CA22*CA3*EL*MH22*SA1*SA2*dAlpha3PinchPStar())/(MW*SB*SW) &
  &- (0.28125D0*CA12*CA22*CA3*EL*MH22*SA1*SA2*dAlpha3PinchPStar())/(MW*SB*SW) + (0.28125D0*CA1*CA3*EL*m12squared*SA12*SA2*dAlpha3&
  &PinchPStar())/(CB2*MW*SB*SW) + (0.84375D0*CA1*CA22*CA3*EL*m12squared*SA12*SA2*dAlpha3PinchPStar())/(CB2*MW*SB*SW) + (0.125D0*C&
  &A1*EL*MH12*SA3*dAlpha3PinchPStar())/(MW*SB*SW) + (0.125D0*CA1*CA22*EL*MH12*SA3*dAlpha3PinchPStar())/(MW*SB*SW) + (0.0625D0*CA1&
  &*EL*MH22*SA3*dAlpha3PinchPStar())/(MW*SB*SW) + (0.0625D0*CA1*CA22*EL*MH22*SA3*dAlpha3PinchPStar())/(MW*SB*SW) - (0.21875D0*EL*&
  &m12squared*SA1*SA3*dAlpha3PinchPStar())/(MW*SB*SW) - (0.21875D0*CA22*EL*m12squared*SA1*SA3*dAlpha3PinchPStar())/(MW*SB*SW) + (&
  &0.15625D0*EL*m12squared*SA1*SA3*dAlpha3PinchPStar())/(CB2*MW*SB*SW) + (0.5625D0*CA12*EL*m12squared*SA1*SA3*dAlpha3PinchPStar()&
  &)/(CB2*MW*SB*SW) + (0.15625D0*CA22*EL*m12squared*SA1*SA3*dAlpha3PinchPStar())/(CB2*MW*SB*SW) + (0.5625D0*CA12*CA22*EL*m12squar&
  &ed*SA1*SA3*dAlpha3PinchPStar())/(CB2*MW*SB*SW) + (0.375D0*CA1*EL*MH12*SA12*SA3*dAlpha3PinchPStar())/(MW*SB*SW) + (0.375D0*CA1*&
  &CA22*EL*MH12*SA12*SA3*dAlpha3PinchPStar())/(MW*SB*SW) + (0.1875D0*CA1*EL*MH22*SA12*SA3*dAlpha3PinchPStar())/(MW*SB*SW) + (0.18&
  &75D0*CA1*CA22*EL*MH22*SA12*SA3*dAlpha3PinchPStar())/(MW*SB*SW) - (0.125D0*CA1*EL*MH12*SA22*SA3*dAlpha3PinchPStar())/(MW*SB*SW)&
  & - (0.0625D0*CA1*EL*MH22*SA22*SA3*dAlpha3PinchPStar())/(MW*SB*SW) + (0.21875D0*EL*m12squared*SA1*SA22*SA3*dAlpha3PinchPStar())&
  &/(MW*SB*SW) - (0.15625D0*EL*m12squared*SA1*SA22*SA3*dAlpha3PinchPStar())/(CB2*MW*SB*SW) - (0.5625D0*CA12*EL*m12squared*SA1*SA2&
  &2*SA3*dAlpha3PinchPStar())/(CB2*MW*SB*SW) - (0.375D0*CA1*EL*MH12*SA12*SA22*SA3*dAlpha3PinchPStar())/(MW*SB*SW) - (0.1875D0*CA1&
  &*EL*MH22*SA12*SA22*SA3*dAlpha3PinchPStar())/(MW*SB*SW) - (0.1875D0*CA3*EL*m12squared*SA1*SA2*dAlpha3PinchPStar())/(CB*MW*SB2*S&
  &W) + (0.28125D0*CA12*CA3*EL*m12squared*SA1*SA2*dAlpha3PinchPStar())/(CB*MW*SB2*SW) - (0.5625D0*CA22*CA3*EL*m12squared*SA1*SA2*&
  &dAlpha3PinchPStar())/(CB*MW*SB2*SW) + (0.84375D0*CA12*CA22*CA3*EL*m12squared*SA1*SA2*dAlpha3PinchPStar())/(CB*MW*SB2*SW) - (0.&
  &0625D0*CA1*EL*m12squared*SA3*dAlpha3PinchPStar())/(CB*MW*SB2*SW) - (0.0625D0*CA1*CA22*EL*m12squared*SA3*dAlpha3PinchPStar())/(&
  &CB*MW*SB2*SW) - (0.5625D0*CA1*EL*m12squared*SA12*SA3*dAlpha3PinchPStar())/(CB*MW*SB2*SW) - (0.5625D0*CA1*CA22*EL*m12squared*SA&
  &12*SA3*dAlpha3PinchPStar())/(CB*MW*SB2*SW) + (0.0625D0*CA1*EL*m12squared*SA22*SA3*dAlpha3PinchPStar())/(CB*MW*SB2*SW) + (0.562&
  &5D0*CA1*EL*m12squared*SA12*SA22*SA3*dAlpha3PinchPStar())/(CB*MW*SB2*SW) - (0.09375D0*CA3*EL*m12squared*SA1*SA2*dAlpha3PinchPSt&
  &ar())/(MW*SB*SW*TB) - (0.28125D0*CA22*CA3*EL*m12squared*SA1*SA2*dAlpha3PinchPStar())/(MW*SB*SW*TB) - (0.125D0*CA1*EL*m12square&
  &d*SA3*dAlpha3PinchPStar())/(MW*SB*SW*TB) - (0.125D0*CA1*CA22*EL*m12squared*SA3*dAlpha3PinchPStar())/(MW*SB*SW*TB) + (0.125D0*C&
  &A1*EL*m12squared*SA22*SA3*dAlpha3PinchPStar())/(MW*SB*SW*TB) - (0.09375D0*CA1*CA3*EL*m12squared*SA2*TB*dAlpha3PinchPStar())/(C&
  &B*MW*SW) - (0.28125D0*CA1*CA22*CA3*EL*m12squared*SA2*TB*dAlpha3PinchPStar())/(CB*MW*SW) + (0.03125D0*EL*m12squared*SA1*SA3*TB*&
  &dAlpha3PinchPStar())/(CB*MW*SW) + (0.03125D0*CA22*EL*m12squared*SA1*SA3*TB*dAlpha3PinchPStar())/(CB*MW*SW) - (0.03125D0*EL*m12&
  &squared*SA1*SA22*SA3*TB*dAlpha3PinchPStar())/(CB*MW*SW) - (0.5D0*CA2*CA3*MH12*dAlpha3PinchPStar())/vS - (0.25D0*CA2*CA3*MH22*d&
  &Alpha3PinchPStar())/vS - (1.5D0*CA2*CA3*MH12*SA22*dAlpha3PinchPStar())/vS - (0.75D0*CA2*CA3*MH22*SA22*dAlpha3PinchPStar())/vS &
  &+ (0.015625D0*CA3*EL*m12squared*SA1*dBeta1PinchPStar())/(CB*MW*SW) + (0.015625D0*CA22*CA3*EL*m12squared*SA1*dBeta1PinchPStar()&
  &)/(CB*MW*SW) - (0.015625D0*CA3*EL*m12squared*SA1*SA22*dBeta1PinchPStar())/(CB*MW*SW) + (0.09375D0*CA1*EL*m12squared*SA2*SA3*dB&
  &eta1PinchPStar())/(CB*MW*SW) + (0.28125D0*CA1*CA22*EL*m12squared*SA2*SA3*dBeta1PinchPStar())/(CB*MW*SW) - (0.0625D0*CA1*CA3*EL&
  &*m12squared*dBeta1PinchPStar())/(MW*SB*SW) - (0.0625D0*CA1*CA22*CA3*EL*m12squared*dBeta1PinchPStar())/(MW*SB*SW) + (0.0625D0*C&
  &A1*CA3*EL*m12squared*dBeta1PinchPStar())/(CB2*MW*SB*SW) + (0.0625D0*CA1*CA22*CA3*EL*m12squared*dBeta1PinchPStar())/(CB2*MW*SB*&
  &SW) - (0.0625D0*CA3*EL*MH12*SA1*dBeta1PinchPStar())/(MW*SB*SW) - (0.1875D0*CA12*CA3*EL*MH12*SA1*dBeta1PinchPStar())/(MW*SB*SW)&
  & - (0.0625D0*CA22*CA3*EL*MH12*SA1*dBeta1PinchPStar())/(MW*SB*SW) - (0.1875D0*CA12*CA22*CA3*EL*MH12*SA1*dBeta1PinchPStar())/(MW&
  &*SB*SW) + (0.0625D0*CA3*EL*MH12*SA1*dBeta1PinchPStar())/(CB2*MW*SB*SW) + (0.1875D0*CA12*CA3*EL*MH12*SA1*dBeta1PinchPStar())/(C&
  &B2*MW*SB*SW) + (0.0625D0*CA22*CA3*EL*MH12*SA1*dBeta1PinchPStar())/(CB2*MW*SB*SW) + (0.1875D0*CA12*CA22*CA3*EL*MH12*SA1*dBeta1P&
  &inchPStar())/(CB2*MW*SB*SW) - (0.03125D0*CA3*EL*MH22*SA1*dBeta1PinchPStar())/(MW*SB*SW) - (0.09375D0*CA12*CA3*EL*MH22*SA1*dBet&
  &a1PinchPStar())/(MW*SB*SW) - (0.03125D0*CA22*CA3*EL*MH22*SA1*dBeta1PinchPStar())/(MW*SB*SW) - (0.09375D0*CA12*CA22*CA3*EL*MH22&
  &*SA1*dBeta1PinchPStar())/(MW*SB*SW) + (0.03125D0*CA3*EL*MH22*SA1*dBeta1PinchPStar())/(CB2*MW*SB*SW) + (0.09375D0*CA12*CA3*EL*M&
  &H22*SA1*dBeta1PinchPStar())/(CB2*MW*SB*SW) + (0.03125D0*CA22*CA3*EL*MH22*SA1*dBeta1PinchPStar())/(CB2*MW*SB*SW) + (0.09375D0*C&
  &A12*CA22*CA3*EL*MH22*SA1*dBeta1PinchPStar())/(CB2*MW*SB*SW) + (0.5625D0*CA1*CA3*EL*m12squared*SA12*dBeta1PinchPStar())/(CB2*MW&
  &*SB*SW) + (0.5625D0*CA1*CA22*CA3*EL*m12squared*SA12*dBeta1PinchPStar())/(CB2*MW*SB*SW) + (0.0625D0*CA1*CA3*EL*m12squared*SA22*&
  &dBeta1PinchPStar())/(MW*SB*SW) - (0.0625D0*CA1*CA3*EL*m12squared*SA22*dBeta1PinchPStar())/(CB2*MW*SB*SW) + (0.0625D0*CA3*EL*MH&
  &12*SA1*SA22*dBeta1PinchPStar())/(MW*SB*SW) + (0.1875D0*CA12*CA3*EL*MH12*SA1*SA22*dBeta1PinchPStar())/(MW*SB*SW) - (0.0625D0*CA&
  &3*EL*MH12*SA1*SA22*dBeta1PinchPStar())/(CB2*MW*SB*SW) - (0.1875D0*CA12*CA3*EL*MH12*SA1*SA22*dBeta1PinchPStar())/(CB2*MW*SB*SW)&
  & + (0.03125D0*CA3*EL*MH22*SA1*SA22*dBeta1PinchPStar())/(MW*SB*SW) + (0.09375D0*CA12*CA3*EL*MH22*SA1*SA22*dBeta1PinchPStar())/(&
  &MW*SB*SW) - (0.03125D0*CA3*EL*MH22*SA1*SA22*dBeta1PinchPStar())/(CB2*MW*SB*SW) - (0.09375D0*CA12*CA3*EL*MH22*SA1*SA22*dBeta1Pi&
  &nchPStar())/(CB2*MW*SB*SW) - (0.5625D0*CA1*CA3*EL*m12squared*SA12*SA22*dBeta1PinchPStar())/(CB2*MW*SB*SW) - (0.09375D0*CA1*EL*&
  &MH12*SA2*SA3*dBeta1PinchPStar())/(MW*SB*SW) - (0.28125D0*CA1*CA22*EL*MH12*SA2*SA3*dBeta1PinchPStar())/(MW*SB*SW) + (0.09375D0*&
  &CA1*EL*MH12*SA2*SA3*dBeta1PinchPStar())/(CB2*MW*SB*SW) + (0.28125D0*CA1*CA22*EL*MH12*SA2*SA3*dBeta1PinchPStar())/(CB2*MW*SB*SW&
  &) - (0.046875D0*CA1*EL*MH22*SA2*SA3*dBeta1PinchPStar())/(MW*SB*SW) - (0.140625D0*CA1*CA22*EL*MH22*SA2*SA3*dBeta1PinchPStar())/&
  &(MW*SB*SW) + (0.046875D0*CA1*EL*MH22*SA2*SA3*dBeta1PinchPStar())/(CB2*MW*SB*SW) + (0.140625D0*CA1*CA22*EL*MH22*SA2*SA3*dBeta1P&
  &inchPStar())/(CB2*MW*SB*SW) - (0.140625D0*EL*m12squared*SA1*SA2*SA3*dBeta1PinchPStar())/(MW*SB*SW) - (0.421875D0*CA22*EL*m12sq&
  &uared*SA1*SA2*SA3*dBeta1PinchPStar())/(MW*SB*SW) - (0.046875D0*EL*m12squared*SA1*SA2*SA3*dBeta1PinchPStar())/(CB2*MW*SB*SW) + &
  &(0.28125D0*CA12*EL*m12squared*SA1*SA2*SA3*dBeta1PinchPStar())/(CB2*MW*SB*SW) - (0.140625D0*CA22*EL*m12squared*SA1*SA2*SA3*dBet&
  &a1PinchPStar())/(CB2*MW*SB*SW) + (0.84375D0*CA12*CA22*EL*m12squared*SA1*SA2*SA3*dBeta1PinchPStar())/(CB2*MW*SB*SW) + (0.09375D&
  &0*CA1*EL*MH12*SA12*SA2*SA3*dBeta1PinchPStar())/(MW*SB*SW) + (0.28125D0*CA1*CA22*EL*MH12*SA12*SA2*SA3*dBeta1PinchPStar())/(MW*S&
  &B*SW) - (0.09375D0*CA1*EL*MH12*SA12*SA2*SA3*dBeta1PinchPStar())/(CB2*MW*SB*SW) - (0.28125D0*CA1*CA22*EL*MH12*SA12*SA2*SA3*dBet&
  &a1PinchPStar())/(CB2*MW*SB*SW) + (0.046875D0*CA1*EL*MH22*SA12*SA2*SA3*dBeta1PinchPStar())/(MW*SB*SW) + (0.140625D0*CA1*CA22*EL&
  &*MH22*SA12*SA2*SA3*dBeta1PinchPStar())/(MW*SB*SW) - (0.046875D0*CA1*EL*MH22*SA12*SA2*SA3*dBeta1PinchPStar())/(CB2*MW*SB*SW) - &
  &(0.140625D0*CA1*CA22*EL*MH22*SA12*SA2*SA3*dBeta1PinchPStar())/(CB2*MW*SB*SW) + (0.203125D0*CA3*EL*m12squared*SA1*dBeta1PinchPS&
  &tar())/(CB*MW*SB2*SW) + (0.84375D0*CA12*CA3*EL*m12squared*SA1*dBeta1PinchPStar())/(CB*MW*SB2*SW) + (0.203125D0*CA22*CA3*EL*m12&
  &squared*SA1*dBeta1PinchPStar())/(CB*MW*SB2*SW) + (0.84375D0*CA12*CA22*CA3*EL*m12squared*SA1*dBeta1PinchPStar())/(CB*MW*SB2*SW)&
  & - (0.203125D0*CA3*EL*m12squared*SA1*SA22*dBeta1PinchPStar())/(CB*MW*SB2*SW) - (0.84375D0*CA12*CA3*EL*m12squared*SA1*SA22*dBet&
  &a1PinchPStar())/(CB*MW*SB2*SW) + (0.10546875D0*CA1*EL*m12squared*SA2*SA3*dBeta1PinchPStar())/(CB*MW*SB2*SW) + (0.31640625D0*CA&
  &1*CA22*EL*m12squared*SA2*SA3*dBeta1PinchPStar())/(CB*MW*SB2*SW) - (0.38671875D0*CA1*EL*m12squared*SA12*SA2*SA3*dBeta1PinchPSta&
  &r())/(CB*MW*SB2*SW) - (1.16015625D0*CA1*CA22*EL*m12squared*SA12*SA2*SA3*dBeta1PinchPStar())/(CB*MW*SB2*SW) + (0.125D0*CA1*CA3*&
  &EL*MH12*dBeta1PinchPStar())/(MW*SB*SW*TB) + (0.125D0*CA1*CA22*CA3*EL*MH12*dBeta1PinchPStar())/(MW*SB*SW*TB) + (0.0625D0*CA1*CA&
  &3*EL*MH22*dBeta1PinchPStar())/(MW*SB*SW*TB) + (0.0625D0*CA1*CA22*CA3*EL*MH22*dBeta1PinchPStar())/(MW*SB*SW*TB) - (0.203125D0*C&
  &A3*EL*m12squared*SA1*dBeta1PinchPStar())/(MW*SB*SW*TB) - (0.203125D0*CA22*CA3*EL*m12squared*SA1*dBeta1PinchPStar())/(MW*SB*SW*&
  &TB) + (0.375D0*CA1*CA3*EL*MH12*SA12*dBeta1PinchPStar())/(MW*SB*SW*TB) + (0.375D0*CA1*CA22*CA3*EL*MH12*SA12*dBeta1PinchPStar())&
  &/(MW*SB*SW*TB) + (0.1875D0*CA1*CA3*EL*MH22*SA12*dBeta1PinchPStar())/(MW*SB*SW*TB) + (0.1875D0*CA1*CA22*CA3*EL*MH22*SA12*dBeta1&
  &PinchPStar())/(MW*SB*SW*TB) - (0.125D0*CA1*CA3*EL*MH12*SA22*dBeta1PinchPStar())/(MW*SB*SW*TB) - (0.0625D0*CA1*CA3*EL*MH22*SA22&
  &*dBeta1PinchPStar())/(MW*SB*SW*TB) + (0.203125D0*CA3*EL*m12squared*SA1*SA22*dBeta1PinchPStar())/(MW*SB*SW*TB) - (0.375D0*CA1*C&
  &A3*EL*MH12*SA12*SA22*dBeta1PinchPStar())/(MW*SB*SW*TB) - (0.1875D0*CA1*CA3*EL*MH22*SA12*SA22*dBeta1PinchPStar())/(MW*SB*SW*TB)&
  & - (0.140625D0*CA1*EL*m12squared*SA2*SA3*dBeta1PinchPStar())/(MW*SB*SW*TB) - (0.421875D0*CA1*CA22*EL*m12squared*SA2*SA3*dBeta1&
  &PinchPStar())/(MW*SB*SW*TB) - (0.1875D0*EL*MH12*SA1*SA2*SA3*dBeta1PinchPStar())/(MW*SB*SW*TB) + (0.1875D0*CA12*EL*MH12*SA1*SA2&
  &*SA3*dBeta1PinchPStar())/(MW*SB*SW*TB) - (0.5625D0*CA22*EL*MH12*SA1*SA2*SA3*dBeta1PinchPStar())/(MW*SB*SW*TB) + (0.5625D0*CA12&
  &*CA22*EL*MH12*SA1*SA2*SA3*dBeta1PinchPStar())/(MW*SB*SW*TB) - (0.09375D0*EL*MH22*SA1*SA2*SA3*dBeta1PinchPStar())/(MW*SB*SW*TB)&
  & + (0.09375D0*CA12*EL*MH22*SA1*SA2*SA3*dBeta1PinchPStar())/(MW*SB*SW*TB) - (0.28125D0*CA22*EL*MH22*SA1*SA2*SA3*dBeta1PinchPSta&
  &r())/(MW*SB*SW*TB) + (0.28125D0*CA12*CA22*EL*MH22*SA1*SA2*SA3*dBeta1PinchPStar())/(MW*SB*SW*TB) - (0.125D0*CA1*CA3*EL*m12squar&
  &ed*TB*dBeta1PinchPStar())/(CB*MW*SW) - (0.125D0*CA1*CA22*CA3*EL*m12squared*TB*dBeta1PinchPStar())/(CB*MW*SW) + (0.0625D0*CA3*E&
  &L*MH12*SA1*TB*dBeta1PinchPStar())/(CB*MW*SW) + (0.1875D0*CA12*CA3*EL*MH12*SA1*TB*dBeta1PinchPStar())/(CB*MW*SW) + (0.0625D0*CA&
  &22*CA3*EL*MH12*SA1*TB*dBeta1PinchPStar())/(CB*MW*SW) + (0.1875D0*CA12*CA22*CA3*EL*MH12*SA1*TB*dBeta1PinchPStar())/(CB*MW*SW) +&
  & (0.03125D0*CA3*EL*MH22*SA1*TB*dBeta1PinchPStar())/(CB*MW*SW) + (0.09375D0*CA12*CA3*EL*MH22*SA1*TB*dBeta1PinchPStar())/(CB*MW*&
  &SW) + (0.03125D0*CA22*CA3*EL*MH22*SA1*TB*dBeta1PinchPStar())/(CB*MW*SW) + (0.09375D0*CA12*CA22*CA3*EL*MH22*SA1*TB*dBeta1PinchP&
  &Star())/(CB*MW*SW) + (0.125D0*CA1*CA3*EL*m12squared*SA22*TB*dBeta1PinchPStar())/(CB*MW*SW) - (0.0625D0*CA3*EL*MH12*SA1*SA22*TB&
  &*dBeta1PinchPStar())/(CB*MW*SW) - (0.1875D0*CA12*CA3*EL*MH12*SA1*SA22*TB*dBeta1PinchPStar())/(CB*MW*SW) - (0.03125D0*CA3*EL*MH&
  &22*SA1*SA22*TB*dBeta1PinchPStar())/(CB*MW*SW) - (0.09375D0*CA12*CA3*EL*MH22*SA1*SA22*TB*dBeta1PinchPStar())/(CB*MW*SW) + (0.09&
  &375D0*CA1*EL*MH12*SA2*SA3*TB*dBeta1PinchPStar())/(CB*MW*SW) + (0.28125D0*CA1*CA22*EL*MH12*SA2*SA3*TB*dBeta1PinchPStar())/(CB*M&
  &W*SW) + (0.046875D0*CA1*EL*MH22*SA2*SA3*TB*dBeta1PinchPStar())/(CB*MW*SW) + (0.140625D0*CA1*CA22*EL*MH22*SA2*SA3*TB*dBeta1Pinc&
  &hPStar())/(CB*MW*SW) + (0.140625D0*EL*m12squared*SA1*SA2*SA3*TB*dBeta1PinchPStar())/(CB*MW*SW) + (0.421875D0*CA22*EL*m12square&
  &d*SA1*SA2*SA3*TB*dBeta1PinchPStar())/(CB*MW*SW) - (0.09375D0*CA1*EL*MH12*SA12*SA2*SA3*TB*dBeta1PinchPStar())/(CB*MW*SW) - (0.2&
  &8125D0*CA1*CA22*EL*MH12*SA12*SA2*SA3*TB*dBeta1PinchPStar())/(CB*MW*SW) - (0.046875D0*CA1*EL*MH22*SA12*SA2*SA3*TB*dBeta1PinchPS&
  &tar())/(CB*MW*SW) - (0.140625D0*CA1*CA22*EL*MH22*SA12*SA2*SA3*TB*dBeta1PinchPStar())/(CB*MW*SW) - (0.1875D0*CA1*CA3*EL*m12squa&
  &red*dBeta1PinchPStar())/(MW*SB*SW*TB2) - (0.1875D0*CA1*CA22*CA3*EL*m12squared*dBeta1PinchPStar())/(MW*SB*SW*TB2) + (0.1875D0*C&
  &A1*CA3*EL*m12squared*SA22*dBeta1PinchPStar())/(MW*SB*SW*TB2) + (0.09375D0*EL*m12squared*SA1*SA2*SA3*dBeta1PinchPStar())/(MW*SB&
  &*SW*TB2) + (0.28125D0*CA22*EL*m12squared*SA1*SA2*SA3*dBeta1PinchPStar())/(MW*SB*SW*TB2) - (0.03125D0*CA3*EL*m12squared*SA1*TB2&
  &*dBeta1PinchPStar())/(CB*MW*SW) - (0.03125D0*CA22*CA3*EL*m12squared*SA1*TB2*dBeta1PinchPStar())/(CB*MW*SW) + (0.03125D0*CA3*EL&
  &*m12squared*SA1*SA22*TB2*dBeta1PinchPStar())/(CB*MW*SW) - (0.140625D0*CA1*EL*m12squared*SA2*SA3*TB2*dBeta1PinchPStar())/(CB*MW&
  &*SW) - (0.421875D0*CA1*CA22*EL*m12squared*SA2*SA3*TB2*dBeta1PinchPStar())/(CB*MW*SW) + (0.375D0*CA3*EL*MH12*dAlpha1PinchPStar(&
  &)*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.375D0*CA22*CA3*EL*MH12*dAlpha1PinchPStar()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.1875D&
  &0*CA3*EL*MH22*dAlpha1PinchPStar()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.1875D0*CA22*CA3*EL*MH22*dAlpha1PinchPStar()*DBLE(CA1**&
  &INT(3.D0)))/(CB*MW*SW) - (0.375D0*CA3*EL*MH12*SA22*dAlpha1PinchPStar()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.1875D0*CA3*EL*MH2&
  &2*SA22*dAlpha1PinchPStar()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.5625D0*CA3*EL*m12squared*dAlpha1PinchPStar()*DBLE(CA1**INT(3.&
  &D0)))/(CB2*MW*SB*SW) - (0.5625D0*CA22*CA3*EL*m12squared*dAlpha1PinchPStar()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) + (0.5625D0*C&
  &A3*EL*m12squared*SA22*dAlpha1PinchPStar()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.1875D0*EL*MH12*SA2*SA3*dAlpha1PinchPStar()&
  &*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.5625D0*CA22*EL*MH12*SA2*SA3*dAlpha1PinchPStar()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.0&
  &9375D0*EL*MH22*SA2*SA3*dAlpha1PinchPStar()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.28125D0*CA22*EL*MH22*SA2*SA3*dAlpha1PinchPSta&
  &r()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) + (0.28125D0*EL*m12squared*SA2*SA3*dAlpha1PinchPStar()*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*S&
  &W) + (0.84375D0*CA22*EL*m12squared*SA2*SA3*dAlpha1PinchPStar()*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.0625D0*CA2*EL*MH12*SA&
  &3*dAlpha2PinchPStar()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.03125D0*CA2*EL*MH22*SA3*dAlpha2PinchPStar()*DBLE(CA1**INT(3.D0)))/&
  &(CB*MW*SW) - (0.5625D0*CA2*EL*MH12*SA22*SA3*dAlpha2PinchPStar()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.28125D0*CA2*EL*MH22*SA22&
  &*SA3*dAlpha2PinchPStar()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.5D0*CA2*CA3*EL*MH12*SA2*dAlpha2PinchPStar()*DBLE(CA1**INT(3.D0)&
  &))/(MW*SB*SW) - (0.25D0*CA2*CA3*EL*MH22*SA2*dAlpha2PinchPStar()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.09375D0*CA2*EL*m12square&
  &d*SA3*dAlpha2PinchPStar()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) + (0.84375D0*CA2*EL*m12squared*SA22*SA3*dAlpha2PinchPStar()*DBL&
  &E(CA1**INT(3.D0)))/(CB2*MW*SB*SW) + (0.75D0*CA2*CA3*EL*m12squared*SA2*dAlpha2PinchPStar()*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW)&
  & + (0.0625D0*CA3*EL*MH12*SA2*dAlpha3PinchPStar()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.1875D0*CA22*CA3*EL*MH12*SA2*dAlpha3Pinc&
  &hPStar()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.03125D0*CA3*EL*MH22*SA2*dAlpha3PinchPStar()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + &
  &(0.09375D0*CA22*CA3*EL*MH22*SA2*dAlpha3PinchPStar()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.09375D0*CA3*EL*m12squared*SA2*dAlpha&
  &3PinchPStar()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.28125D0*CA22*CA3*EL*m12squared*SA2*dAlpha3PinchPStar()*DBLE(CA1**INT(3&
  &.D0)))/(CB2*MW*SB*SW) - (0.125D0*EL*MH12*SA3*dAlpha3PinchPStar()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.125D0*CA22*EL*MH12*SA3*&
  &dAlpha3PinchPStar()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.0625D0*EL*MH22*SA3*dAlpha3PinchPStar()*DBLE(CA1**INT(3.D0)))/(MW*SB*&
  &SW) - (0.0625D0*CA22*EL*MH22*SA3*dAlpha3PinchPStar()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) + (0.125D0*EL*MH12*SA22*SA3*dAlpha3Pinch&
  &PStar()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) + (0.0625D0*EL*MH22*SA22*SA3*dAlpha3PinchPStar()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) + (&
  &0.1875D0*EL*m12squared*SA3*dAlpha3PinchPStar()*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.1875D0*CA22*EL*m12squared*SA3*dAlpha3&
  &PinchPStar()*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.1875D0*EL*m12squared*SA22*SA3*dAlpha3PinchPStar()*DBLE(CA1**INT(3.D0)))&
  &/(CB*MW*SB2*SW) - (0.1875D0*CA3*EL*m12squared*dBeta1PinchPStar()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.1875D0*CA22*CA3*EL*&
  &m12squared*dBeta1PinchPStar()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) + (0.1875D0*CA3*EL*m12squared*SA22*dBeta1PinchPStar()*DBLE(&
  &CA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.03125D0*EL*MH12*SA2*SA3*dBeta1PinchPStar()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.09375D0*&
  &CA22*EL*MH12*SA2*SA3*dBeta1PinchPStar()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) + (0.03125D0*EL*MH12*SA2*SA3*dBeta1PinchPStar()*DBLE(&
  &CA1**INT(3.D0)))/(CB2*MW*SB*SW) + (0.09375D0*CA22*EL*MH12*SA2*SA3*dBeta1PinchPStar()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0&
  &.015625D0*EL*MH22*SA2*SA3*dBeta1PinchPStar()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.046875D0*CA22*EL*MH22*SA2*SA3*dBeta1PinchPS&
  &tar()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) + (0.015625D0*EL*MH22*SA2*SA3*dBeta1PinchPStar()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) +&
  & (0.046875D0*CA22*EL*MH22*SA2*SA3*dBeta1PinchPStar()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) + (0.12890625D0*EL*m12squared*SA2*SA&
  &3*dBeta1PinchPStar()*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.38671875D0*CA22*EL*m12squared*SA2*SA3*dBeta1PinchPStar()*DBLE(C&
  &A1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.125D0*CA3*EL*MH12*dBeta1PinchPStar()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW*TB) - (0.125D0*CA22*C&
  &A3*EL*MH12*dBeta1PinchPStar()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW*TB) - (0.0625D0*CA3*EL*MH22*dBeta1PinchPStar()*DBLE(CA1**INT(3.D&
  &0)))/(MW*SB*SW*TB) - (0.0625D0*CA22*CA3*EL*MH22*dBeta1PinchPStar()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW*TB) + (0.125D0*CA3*EL*MH12*&
  &SA22*dBeta1PinchPStar()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW*TB) + (0.0625D0*CA3*EL*MH22*SA22*dBeta1PinchPStar()*DBLE(CA1**INT(3.D0&
  &)))/(MW*SB*SW*TB) + (0.03125D0*EL*MH12*SA2*SA3*TB*dBeta1PinchPStar()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.09375D0*CA22*EL*MH1&
  &2*SA2*SA3*TB*dBeta1PinchPStar()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.015625D0*EL*MH22*SA2*SA3*TB*dBeta1PinchPStar()*DBLE(CA1*&
  &*INT(3.D0)))/(CB*MW*SW) + (0.046875D0*CA22*EL*MH22*SA2*SA3*TB*dBeta1PinchPStar()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.5625D0*&
  &CA1*EL*MH12*SA3*dAlpha2PinchPStar()*DBLE(CA2**INT(3.D0)))/(CB*MW*SW) + (0.28125D0*CA1*EL*MH22*SA3*dAlpha2PinchPStar()*DBLE(CA2&
  &**INT(3.D0)))/(CB*MW*SW) + (0.84375D0*EL*m12squared*SA1*SA3*dAlpha2PinchPStar()*DBLE(CA2**INT(3.D0)))/(CB*MW*SW) - (0.5625D0*C&
  &A1*EL*MH12*SA12*SA3*dAlpha2PinchPStar()*DBLE(CA2**INT(3.D0)))/(CB*MW*SW) - (0.28125D0*CA1*EL*MH22*SA12*SA3*dAlpha2PinchPStar()&
  &*DBLE(CA2**INT(3.D0)))/(CB*MW*SW) + (0.84375D0*CA1*EL*m12squared*SA3*dAlpha2PinchPStar()*DBLE(CA2**INT(3.D0)))/(MW*SB*SW) - (0&
  &.5625D0*CA1*EL*m12squared*SA3*dAlpha2PinchPStar()*DBLE(CA2**INT(3.D0)))/(CB2*MW*SB*SW) + (0.5625D0*EL*MH12*SA1*SA3*dAlpha2Pinc&
  &hPStar()*DBLE(CA2**INT(3.D0)))/(MW*SB*SW) - (0.5625D0*CA12*EL*MH12*SA1*SA3*dAlpha2PinchPStar()*DBLE(CA2**INT(3.D0)))/(MW*SB*SW&
  &) + (0.28125D0*EL*MH22*SA1*SA3*dAlpha2PinchPStar()*DBLE(CA2**INT(3.D0)))/(MW*SB*SW) - (0.28125D0*CA12*EL*MH22*SA1*SA3*dAlpha2P&
  &inchPStar()*DBLE(CA2**INT(3.D0)))/(MW*SB*SW) + (0.84375D0*CA1*EL*m12squared*SA12*SA3*dAlpha2PinchPStar()*DBLE(CA2**INT(3.D0)))&
  &/(CB2*MW*SB*SW) - (0.5625D0*EL*m12squared*SA1*SA3*dAlpha2PinchPStar()*DBLE(CA2**INT(3.D0)))/(CB*MW*SB2*SW) + (0.84375D0*CA12*E&
  &L*m12squared*SA1*SA3*dAlpha2PinchPStar()*DBLE(CA2**INT(3.D0)))/(CB*MW*SB2*SW) - (0.28125D0*EL*m12squared*SA1*SA3*dAlpha2PinchP&
  &Star()*DBLE(CA2**INT(3.D0)))/(MW*SB*SW*TB) - (0.28125D0*CA1*EL*m12squared*SA3*TB*dAlpha2PinchPStar()*DBLE(CA2**INT(3.D0)))/(CB&
  &*MW*SW) + (0.5D0*CA3*MH12*dAlpha3PinchPStar()*DBLE(CA2**INT(3.D0)))/vS + (0.25D0*CA3*MH22*dAlpha3PinchPStar()*DBLE(CA2**INT(3.&
  &D0)))/vS + (0.1875D0*EL*MH12*SA3*dAlpha2PinchPStar()*DBLE(CA1**INT(3.D0))*DBLE(CA2**INT(3.D0)))/(CB*MW*SW) + (0.09375D0*EL*MH2&
  &2*SA3*dAlpha2PinchPStar()*DBLE(CA1**INT(3.D0))*DBLE(CA2**INT(3.D0)))/(CB*MW*SW) - (0.28125D0*EL*m12squared*SA3*dAlpha2PinchPSt&
  &ar()*DBLE(CA1**INT(3.D0))*DBLE(CA2**INT(3.D0)))/(CB2*MW*SB*SW) - (0.28125D0*CA3*EL*m12squared*SA1*dBeta1PinchPStar()*DBLE(CB**&
  &INT(-3.D0)))/(MW*SW) - (0.84375D0*CA12*CA3*EL*m12squared*SA1*dBeta1PinchPStar()*DBLE(CB**INT(-3.D0)))/(MW*SW) - (0.28125D0*CA2&
  &2*CA3*EL*m12squared*SA1*dBeta1PinchPStar()*DBLE(CB**INT(-3.D0)))/(MW*SW) - (0.84375D0*CA12*CA22*CA3*EL*m12squared*SA1*dBeta1Pi&
  &nchPStar()*DBLE(CB**INT(-3.D0)))/(MW*SW) + (0.28125D0*CA3*EL*m12squared*SA1*SA22*dBeta1PinchPStar()*DBLE(CB**INT(-3.D0)))/(MW*&
  &SW) + (0.84375D0*CA12*CA3*EL*m12squared*SA1*SA22*dBeta1PinchPStar()*DBLE(CB**INT(-3.D0)))/(MW*SW) - (0.36328125D0*CA1*EL*m12sq&
  &uared*SA2*SA3*dBeta1PinchPStar()*DBLE(CB**INT(-3.D0)))/(MW*SW) - (1.08984375D0*CA1*CA22*EL*m12squared*SA2*SA3*dBeta1PinchPStar&
  &()*DBLE(CB**INT(-3.D0)))/(MW*SW) + (0.45703125D0*CA1*EL*m12squared*SA12*SA2*SA3*dBeta1PinchPStar()*DBLE(CB**INT(-3.D0)))/(MW*S&
  &W) + (1.37109375D0*CA1*CA22*EL*m12squared*SA12*SA2*SA3*dBeta1PinchPStar()*DBLE(CB**INT(-3.D0)))/(MW*SW) - (0.0625D0*CA3*EL*m12&
  &squared*SA1*dBeta1PinchPStar()*DBLE(CB**INT(-3.D0)))/(MW*SB2*SW) - (0.28125D0*CA12*CA3*EL*m12squared*SA1*dBeta1PinchPStar()*DB&
  &LE(CB**INT(-3.D0)))/(MW*SB2*SW) - (0.0625D0*CA22*CA3*EL*m12squared*SA1*dBeta1PinchPStar()*DBLE(CB**INT(-3.D0)))/(MW*SB2*SW) - &
  &(0.28125D0*CA12*CA22*CA3*EL*m12squared*SA1*dBeta1PinchPStar()*DBLE(CB**INT(-3.D0)))/(MW*SB2*SW) + (0.0625D0*CA3*EL*m12squared*&
  &SA1*SA22*dBeta1PinchPStar()*DBLE(CB**INT(-3.D0)))/(MW*SB2*SW) + (0.28125D0*CA12*CA3*EL*m12squared*SA1*SA22*dBeta1PinchPStar()*&
  &DBLE(CB**INT(-3.D0)))/(MW*SB2*SW) - (0.05859375D0*CA1*EL*m12squared*SA2*SA3*dBeta1PinchPStar()*DBLE(CB**INT(-3.D0)))/(MW*SB2*S&
  &W) - (0.17578125D0*CA1*CA22*EL*m12squared*SA2*SA3*dBeta1PinchPStar()*DBLE(CB**INT(-3.D0)))/(MW*SB2*SW) + (0.10546875D0*CA1*EL*&
  &m12squared*SA12*SA2*SA3*dBeta1PinchPStar()*DBLE(CB**INT(-3.D0)))/(MW*SB2*SW) + (0.31640625D0*CA1*CA22*EL*m12squared*SA12*SA2*S&
  &A3*dBeta1PinchPStar()*DBLE(CB**INT(-3.D0)))/(MW*SB2*SW) - (0.15234375D0*EL*m12squared*SA2*SA3*dBeta1PinchPStar()*DBLE(CA1**INT&
  &(3.D0))*DBLE(CB**INT(-3.D0)))/(MW*SW) - (0.45703125D0*CA22*EL*m12squared*SA2*SA3*dBeta1PinchPStar()*DBLE(CA1**INT(3.D0))*DBLE(&
  &CB**INT(-3.D0)))/(MW*SW) - (0.03515625D0*EL*m12squared*SA2*SA3*dBeta1PinchPStar()*DBLE(CA1**INT(3.D0))*DBLE(CB**INT(-3.D0)))/(&
  &MW*SB2*SW) - (0.10546875D0*CA22*EL*m12squared*SA2*SA3*dBeta1PinchPStar()*DBLE(CA1**INT(3.D0))*DBLE(CB**INT(-3.D0)))/(MW*SB2*SW&
  &) + (0.1875D0*EL*MH12*SA2*SA3*dAlpha1PinchPStar()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.5625D0*CA22*EL*MH12*SA2*SA3*dAlpha1Pin&
  &chPStar()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.09375D0*EL*MH22*SA2*SA3*dAlpha1PinchPStar()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) +&
  & (0.28125D0*CA22*EL*MH22*SA2*SA3*dAlpha1PinchPStar()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.375D0*CA3*EL*MH12*dAlpha1PinchPStar&
  &()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.375D0*CA22*CA3*EL*MH12*dAlpha1PinchPStar()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.1875&
  &D0*CA3*EL*MH22*dAlpha1PinchPStar()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.1875D0*CA22*CA3*EL*MH22*dAlpha1PinchPStar()*DBLE(SA1*&
  &*INT(3.D0)))/(MW*SB*SW) - (0.375D0*CA3*EL*MH12*SA22*dAlpha1PinchPStar()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0.1875D0*CA3*EL*MH&
  &22*SA22*dAlpha1PinchPStar()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0.28125D0*EL*m12squared*SA2*SA3*dAlpha1PinchPStar()*DBLE(SA1**&
  &INT(3.D0)))/(CB2*MW*SB*SW) - (0.84375D0*CA22*EL*m12squared*SA2*SA3*dAlpha1PinchPStar()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) - &
  &(0.5625D0*CA3*EL*m12squared*dAlpha1PinchPStar()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.5625D0*CA22*CA3*EL*m12squared*dAlpha&
  &1PinchPStar()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.5625D0*CA3*EL*m12squared*SA22*dAlpha1PinchPStar()*DBLE(SA1**INT(3.D0))&
  &)/(CB*MW*SB2*SW) + (0.5D0*CA2*CA3*EL*MH12*SA2*dAlpha2PinchPStar()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.25D0*CA2*CA3*EL*MH22*S&
  &A2*dAlpha2PinchPStar()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) - (0.75D0*CA2*CA3*EL*m12squared*SA2*dAlpha2PinchPStar()*DBLE(SA1**INT(&
  &3.D0)))/(CB2*MW*SB*SW) + (0.0625D0*CA2*EL*MH12*SA3*dAlpha2PinchPStar()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.03125D0*CA2*EL*MH&
  &22*SA3*dAlpha2PinchPStar()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0.5625D0*CA2*EL*MH12*SA22*SA3*dAlpha2PinchPStar()*DBLE(SA1**INT&
  &(3.D0)))/(MW*SB*SW) - (0.28125D0*CA2*EL*MH22*SA22*SA3*dAlpha2PinchPStar()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0.09375D0*CA2*EL&
  &*m12squared*SA3*dAlpha2PinchPStar()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.84375D0*CA2*EL*m12squared*SA22*SA3*dAlpha2PinchP&
  &Star()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.125D0*EL*MH12*SA3*dAlpha3PinchPStar()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.1&
  &25D0*CA22*EL*MH12*SA3*dAlpha3PinchPStar()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.0625D0*EL*MH22*SA3*dAlpha3PinchPStar()*DBLE(SA&
  &1**INT(3.D0)))/(CB*MW*SW) + (0.0625D0*CA22*EL*MH22*SA3*dAlpha3PinchPStar()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) - (0.125D0*EL*MH12&
  &*SA22*SA3*dAlpha3PinchPStar()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) - (0.0625D0*EL*MH22*SA22*SA3*dAlpha3PinchPStar()*DBLE(SA1**INT(&
  &3.D0)))/(CB*MW*SW) + (0.0625D0*CA3*EL*MH12*SA2*dAlpha3PinchPStar()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.1875D0*CA22*CA3*EL*MH&
  &12*SA2*dAlpha3PinchPStar()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.03125D0*CA3*EL*MH22*SA2*dAlpha3PinchPStar()*DBLE(SA1**INT(3.D&
  &0)))/(MW*SB*SW) + (0.09375D0*CA22*CA3*EL*MH22*SA2*dAlpha3PinchPStar()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0.1875D0*EL*m12squar&
  &ed*SA3*dAlpha3PinchPStar()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.1875D0*CA22*EL*m12squared*SA3*dAlpha3PinchPStar()*DBLE(SA&
  &1**INT(3.D0)))/(CB2*MW*SB*SW) + (0.1875D0*EL*m12squared*SA22*SA3*dAlpha3PinchPStar()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0&
  &.09375D0*CA3*EL*m12squared*SA2*dAlpha3PinchPStar()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.28125D0*CA22*CA3*EL*m12squared*SA&
  &2*dAlpha3PinchPStar()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.0625D0*CA3*EL*MH12*dBeta1PinchPStar()*DBLE(SA1**INT(3.D0)))/(M&
  &W*SB*SW) + (0.0625D0*CA22*CA3*EL*MH12*dBeta1PinchPStar()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0.0625D0*CA3*EL*MH12*dBeta1PinchP&
  &Star()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.0625D0*CA22*CA3*EL*MH12*dBeta1PinchPStar()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*S&
  &W) + (0.03125D0*CA3*EL*MH22*dBeta1PinchPStar()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.03125D0*CA22*CA3*EL*MH22*dBeta1PinchPStar&
  &()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0.03125D0*CA3*EL*MH22*dBeta1PinchPStar()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.0312&
  &5D0*CA22*CA3*EL*MH22*dBeta1PinchPStar()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.0625D0*CA3*EL*MH12*SA22*dBeta1PinchPStar()*D&
  &BLE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.0625D0*CA3*EL*MH12*SA22*dBeta1PinchPStar()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.0312&
  &5D0*CA3*EL*MH22*SA22*dBeta1PinchPStar()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.03125D0*CA3*EL*MH22*SA22*dBeta1PinchPStar()*DBLE&
  &(SA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.09375D0*EL*m12squared*SA2*SA3*dBeta1PinchPStar()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) - &
  &(0.28125D0*CA22*EL*m12squared*SA2*SA3*dBeta1PinchPStar()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.28125D0*CA3*EL*m12squared*d&
  &Beta1PinchPStar()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.28125D0*CA22*CA3*EL*m12squared*dBeta1PinchPStar()*DBLE(SA1**INT(3.&
  &D0)))/(CB*MW*SB2*SW) + (0.28125D0*CA3*EL*m12squared*SA22*dBeta1PinchPStar()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.0625D0*E&
  &L*MH12*SA2*SA3*dBeta1PinchPStar()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW*TB) - (0.1875D0*CA22*EL*MH12*SA2*SA3*dBeta1PinchPStar()*DBLE&
  &(SA1**INT(3.D0)))/(MW*SB*SW*TB) - (0.03125D0*EL*MH22*SA2*SA3*dBeta1PinchPStar()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW*TB) - (0.09375&
  &D0*CA22*EL*MH22*SA2*SA3*dBeta1PinchPStar()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW*TB) - (0.0625D0*CA3*EL*MH12*TB*dBeta1PinchPStar()*D&
  &BLE(SA1**INT(3.D0)))/(CB*MW*SW) - (0.0625D0*CA22*CA3*EL*MH12*TB*dBeta1PinchPStar()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) - (0.03125&
  &D0*CA3*EL*MH22*TB*dBeta1PinchPStar()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) - (0.03125D0*CA22*CA3*EL*MH22*TB*dBeta1PinchPStar()*DBLE&
  &(SA1**INT(3.D0)))/(CB*MW*SW) + (0.0625D0*CA3*EL*MH12*SA22*TB*dBeta1PinchPStar()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.03125D0*&
  &CA3*EL*MH22*SA22*TB*dBeta1PinchPStar()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.1875D0*EL*MH12*SA3*dAlpha2PinchPStar()*DBLE(CA2**&
  &INT(3.D0))*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.09375D0*EL*MH22*SA3*dAlpha2PinchPStar()*DBLE(CA2**INT(3.D0))*DBLE(SA1**INT(3.&
  &D0)))/(MW*SB*SW) - (0.28125D0*EL*m12squared*SA3*dAlpha2PinchPStar()*DBLE(CA2**INT(3.D0))*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) &
  &+ (0.28125D0*CA3*EL*m12squared*dBeta1PinchPStar()*DBLE(CB**INT(-3.D0))*DBLE(SA1**INT(3.D0)))/(MW*SW) + (0.28125D0*CA22*CA3*EL*&
  &m12squared*dBeta1PinchPStar()*DBLE(CB**INT(-3.D0))*DBLE(SA1**INT(3.D0)))/(MW*SW) - (0.28125D0*CA3*EL*m12squared*SA22*dBeta1Pin&
  &chPStar()*DBLE(CB**INT(-3.D0))*DBLE(SA1**INT(3.D0)))/(MW*SW) + (0.09375D0*CA3*EL*m12squared*dBeta1PinchPStar()*DBLE(CB**INT(-3&
  &.D0))*DBLE(SA1**INT(3.D0)))/(MW*SB2*SW) + (0.09375D0*CA22*CA3*EL*m12squared*dBeta1PinchPStar()*DBLE(CB**INT(-3.D0))*DBLE(SA1**&
  &INT(3.D0)))/(MW*SB2*SW) - (0.09375D0*CA3*EL*m12squared*SA22*dBeta1PinchPStar()*DBLE(CB**INT(-3.D0))*DBLE(SA1**INT(3.D0)))/(MW*&
  &SB2*SW) - (0.28125D0*CA1*EL*m12squared*SA3*dAlpha1PinchPStar()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.1875D0*EL*MH12*SA1*SA3*dA&
  &lpha1PinchPStar()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.5625D0*CA12*EL*MH12*SA1*SA3*dAlpha1PinchPStar()*DBLE(SA2**INT(3.D0)))/&
  &(CB*MW*SW) + (0.09375D0*EL*MH22*SA1*SA3*dAlpha1PinchPStar()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.28125D0*CA12*EL*MH22*SA1*SA3&
  &*dAlpha1PinchPStar()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.1875D0*CA1*EL*MH12*SA3*dAlpha1PinchPStar()*DBLE(SA2**INT(3.D0)))/(M&
  &W*SB*SW) - (0.09375D0*CA1*EL*MH22*SA3*dAlpha1PinchPStar()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) + (0.28125D0*EL*m12squared*SA1*SA3*&
  &dAlpha1PinchPStar()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.1875D0*EL*m12squared*SA1*SA3*dAlpha1PinchPStar()*DBLE(SA2**INT(3.D0)&
  &))/(CB2*MW*SB*SW) - (0.84375D0*CA12*EL*m12squared*SA1*SA3*dAlpha1PinchPStar()*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) - (0.5625D0&
  &*CA1*EL*MH12*SA12*SA3*dAlpha1PinchPStar()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.28125D0*CA1*EL*MH22*SA12*SA3*dAlpha1PinchPStar&
  &()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) + (0.1875D0*CA1*EL*m12squared*SA3*dAlpha1PinchPStar()*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW)&
  & + (0.84375D0*CA1*EL*m12squared*SA12*SA3*dAlpha1PinchPStar()*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) + (0.09375D0*CA1*EL*m12squar&
  &ed*SA3*dAlpha1PinchPStar()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) - (0.09375D0*EL*m12squared*SA1*SA3*TB*dAlpha1PinchPStar()*DBLE(&
  &SA2**INT(3.D0)))/(CB*MW*SW) + (1.5D0*MH12*SA3*dAlpha2PinchPStar()*DBLE(SA2**INT(3.D0)))/vS + (0.75D0*MH22*SA3*dAlpha2PinchPSta&
  &r()*DBLE(SA2**INT(3.D0)))/vS - (0.1875D0*CA1*CA3*EL*MH12*dAlpha3PinchPStar()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.09375D0*CA1&
  &*CA3*EL*MH22*dAlpha3PinchPStar()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.28125D0*CA3*EL*m12squared*SA1*dAlpha3PinchPStar()*DBLE(&
  &SA2**INT(3.D0)))/(CB*MW*SW) + (0.1875D0*CA1*CA3*EL*MH12*SA12*dAlpha3PinchPStar()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.09375D0&
  &*CA1*CA3*EL*MH22*SA12*dAlpha3PinchPStar()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.28125D0*CA1*CA3*EL*m12squared*dAlpha3PinchPSta&
  &r()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) + (0.1875D0*CA1*CA3*EL*m12squared*dAlpha3PinchPStar()*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW&
  &) - (0.1875D0*CA3*EL*MH12*SA1*dAlpha3PinchPStar()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) + (0.1875D0*CA12*CA3*EL*MH12*SA1*dAlpha3Pin&
  &chPStar()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.09375D0*CA3*EL*MH22*SA1*dAlpha3PinchPStar()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) +&
  & (0.09375D0*CA12*CA3*EL*MH22*SA1*dAlpha3PinchPStar()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.28125D0*CA1*CA3*EL*m12squared*SA12*&
  &dAlpha3PinchPStar()*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) + (0.1875D0*CA3*EL*m12squared*SA1*dAlpha3PinchPStar()*DBLE(SA2**INT(3&
  &.D0)))/(CB*MW*SB2*SW) - (0.28125D0*CA12*CA3*EL*m12squared*SA1*dAlpha3PinchPStar()*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) + (0.09&
  &375D0*CA3*EL*m12squared*SA1*dAlpha3PinchPStar()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) + (0.09375D0*CA1*CA3*EL*m12squared*TB*dAlp&
  &ha3PinchPStar()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.09375D0*CA1*EL*m12squared*SA3*dBeta1PinchPStar()*DBLE(SA2**INT(3.D0)))/(&
  &CB*MW*SW) + (0.09375D0*CA1*EL*MH12*SA3*dBeta1PinchPStar()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.09375D0*CA1*EL*MH12*SA3*dBeta1&
  &PinchPStar()*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) + (0.046875D0*CA1*EL*MH22*SA3*dBeta1PinchPStar()*DBLE(SA2**INT(3.D0)))/(MW*S&
  &B*SW) - (0.046875D0*CA1*EL*MH22*SA3*dBeta1PinchPStar()*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) + (0.140625D0*EL*m12squared*SA1*SA&
  &3*dBeta1PinchPStar()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) + (0.046875D0*EL*m12squared*SA1*SA3*dBeta1PinchPStar()*DBLE(SA2**INT(3.D&
  &0)))/(CB2*MW*SB*SW) - (0.28125D0*CA12*EL*m12squared*SA1*SA3*dBeta1PinchPStar()*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) - (0.09375&
  &D0*CA1*EL*MH12*SA12*SA3*dBeta1PinchPStar()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) + (0.09375D0*CA1*EL*MH12*SA12*SA3*dBeta1PinchPStar&
  &()*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) - (0.046875D0*CA1*EL*MH22*SA12*SA3*dBeta1PinchPStar()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW)&
  & + (0.046875D0*CA1*EL*MH22*SA12*SA3*dBeta1PinchPStar()*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) - (0.10546875D0*CA1*EL*m12squared*&
  &SA3*dBeta1PinchPStar()*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) + (0.38671875D0*CA1*EL*m12squared*SA12*SA3*dBeta1PinchPStar()*DBLE&
  &(SA2**INT(3.D0)))/(CB*MW*SB2*SW) + (0.140625D0*CA1*EL*m12squared*SA3*dBeta1PinchPStar()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) + &
  &(0.1875D0*EL*MH12*SA1*SA3*dBeta1PinchPStar()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) - (0.1875D0*CA12*EL*MH12*SA1*SA3*dBeta1PinchP&
  &Star()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) + (0.09375D0*EL*MH22*SA1*SA3*dBeta1PinchPStar()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB)&
  & - (0.09375D0*CA12*EL*MH22*SA1*SA3*dBeta1PinchPStar()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) - (0.09375D0*CA1*EL*MH12*SA3*TB*dBet&
  &a1PinchPStar()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.046875D0*CA1*EL*MH22*SA3*TB*dBeta1PinchPStar()*DBLE(SA2**INT(3.D0)))/(CB*&
  &MW*SW) - (0.140625D0*EL*m12squared*SA1*SA3*TB*dBeta1PinchPStar()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.09375D0*CA1*EL*MH12*SA1&
  &2*SA3*TB*dBeta1PinchPStar()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.046875D0*CA1*EL*MH22*SA12*SA3*TB*dBeta1PinchPStar()*DBLE(SA2&
  &**INT(3.D0)))/(CB*MW*SW) - (0.09375D0*EL*m12squared*SA1*SA3*dBeta1PinchPStar()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB2) + (0.14062&
  &5D0*CA1*EL*m12squared*SA3*TB2*dBeta1PinchPStar()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.1875D0*EL*MH12*SA3*dAlpha1PinchPStar()*&
  &DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) + (0.09375D0*EL*MH22*SA3*dAlpha1PinchPStar()*DBLE(CA1**INT(3.D0))*DBLE(S&
  &A2**INT(3.D0)))/(MW*SB*SW) - (0.28125D0*EL*m12squared*SA3*dAlpha1PinchPStar()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*M&
  &W*SB2*SW) - (0.0625D0*CA3*EL*MH12*dAlpha3PinchPStar()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.03125D0*CA3*E&
  &L*MH22*dAlpha3PinchPStar()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.09375D0*CA3*EL*m12squared*dAlpha3PinchPS&
  &tar()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) + (0.03125D0*EL*MH12*SA3*dBeta1PinchPStar()*DBLE(CA1**INT(3.D0&
  &))*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.03125D0*EL*MH12*SA3*dBeta1PinchPStar()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB&
  &2*MW*SB*SW) + (0.015625D0*EL*MH22*SA3*dBeta1PinchPStar()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.015625D0*E&
  &L*MH22*SA3*dBeta1PinchPStar()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) - (0.12890625D0*EL*m12squared*SA3*dBet&
  &a1PinchPStar()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) - (0.03125D0*EL*MH12*SA3*TB*dBeta1PinchPStar()*DBLE(C&
  &A1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.015625D0*EL*MH22*SA3*TB*dBeta1PinchPStar()*DBLE(CA1**INT(3.D0))*DBLE(SA2*&
  &*INT(3.D0)))/(CB*MW*SW) + (0.36328125D0*CA1*EL*m12squared*SA3*dBeta1PinchPStar()*DBLE(CB**INT(-3.D0))*DBLE(SA2**INT(3.D0)))/(M&
  &W*SW) - (0.45703125D0*CA1*EL*m12squared*SA12*SA3*dBeta1PinchPStar()*DBLE(CB**INT(-3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SW) + (0.05&
  &859375D0*CA1*EL*m12squared*SA3*dBeta1PinchPStar()*DBLE(CB**INT(-3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SB2*SW) - (0.10546875D0*CA1*E&
  &L*m12squared*SA12*SA3*dBeta1PinchPStar()*DBLE(CB**INT(-3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SB2*SW) + (0.15234375D0*EL*m12squared*&
  &SA3*dBeta1PinchPStar()*DBLE(CA1**INT(3.D0))*DBLE(CB**INT(-3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SW) + (0.03515625D0*EL*m12squared*S&
  &A3*dBeta1PinchPStar()*DBLE(CA1**INT(3.D0))*DBLE(CB**INT(-3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SB2*SW) - (0.1875D0*EL*MH12*SA3*dAlp&
  &ha1PinchPStar()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.09375D0*EL*MH22*SA3*dAlpha1PinchPStar()*DBLE(SA1**I&
  &NT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.28125D0*EL*m12squared*SA3*dAlpha1PinchPStar()*DBLE(SA1**INT(3.D0))*DBLE(SA2**I&
  &NT(3.D0)))/(CB2*MW*SB*SW) - (0.0625D0*CA3*EL*MH12*dAlpha3PinchPStar()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - &
  &(0.03125D0*CA3*EL*MH22*dAlpha3PinchPStar()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) + (0.09375D0*CA3*EL*m12square&
  &d*dAlpha3PinchPStar()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) + (0.09375D0*EL*m12squared*SA3*dBeta1PinchPSta&
  &r()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) + (0.0625D0*EL*MH12*SA3*dBeta1PinchPStar()*DBLE(SA1**INT(3.D0))*&
  &DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) + (0.03125D0*EL*MH22*SA3*dBeta1PinchPStar()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(MW&
  &*SB*SW*TB) - (0.1875D0*CA1*CA3*EL*m12squared*dBeta1PinchPStar()*DBLE(SB**INT(-3.D0)))/(MW*SW) - (0.1875D0*CA1*CA22*CA3*EL*m12s&
  &quared*dBeta1PinchPStar()*DBLE(SB**INT(-3.D0)))/(MW*SW) - (1.125D0*CA1*CA3*EL*m12squared*SA12*dBeta1PinchPStar()*DBLE(SB**INT(&
  &-3.D0)))/(MW*SW) - (1.125D0*CA1*CA22*CA3*EL*m12squared*SA12*dBeta1PinchPStar()*DBLE(SB**INT(-3.D0)))/(MW*SW) + (0.1875D0*CA1*C&
  &A3*EL*m12squared*SA22*dBeta1PinchPStar()*DBLE(SB**INT(-3.D0)))/(MW*SW) + (1.125D0*CA1*CA3*EL*m12squared*SA12*SA22*dBeta1PinchP&
  &Star()*DBLE(SB**INT(-3.D0)))/(MW*SW) + (0.46875D0*EL*m12squared*SA1*SA2*SA3*dBeta1PinchPStar()*DBLE(SB**INT(-3.D0)))/(MW*SW) -&
  & (0.5625D0*CA12*EL*m12squared*SA1*SA2*SA3*dBeta1PinchPStar()*DBLE(SB**INT(-3.D0)))/(MW*SW) + (1.40625D0*CA22*EL*m12squared*SA1&
  &*SA2*SA3*dBeta1PinchPStar()*DBLE(SB**INT(-3.D0)))/(MW*SW) - (1.6875D0*CA12*CA22*EL*m12squared*SA1*SA2*SA3*dBeta1PinchPStar()*D&
  &BLE(SB**INT(-3.D0)))/(MW*SW) + (0.375D0*CA3*EL*m12squared*dBeta1PinchPStar()*DBLE(CA1**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(MW*SW&
  &) + (0.375D0*CA22*CA3*EL*m12squared*dBeta1PinchPStar()*DBLE(CA1**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(MW*SW) - (0.375D0*CA3*EL*m1&
  &2squared*SA22*dBeta1PinchPStar()*DBLE(CA1**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(MW*SW) + (0.1875D0*EL*m12squared*SA2*SA3*dBeta1Pi&
  &nchPStar()*DBLE(SA1**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(MW*SW) + (0.5625D0*CA22*EL*m12squared*SA2*SA3*dBeta1PinchPStar()*DBLE(S&
  &A1**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(MW*SW) - (0.46875D0*EL*m12squared*SA1*SA3*dBeta1PinchPStar()*DBLE(SA2**INT(3.D0))*DBLE(S&
  &B**INT(-3.D0)))/(MW*SW) + (0.5625D0*CA12*EL*m12squared*SA1*SA3*dBeta1PinchPStar()*DBLE(SA2**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(&
  &MW*SW) - (0.1875D0*EL*m12squared*SA3*dBeta1PinchPStar()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(MW*SW&
  &) - (0.125D0*CA1*CA3*m12squared*dgAtMZ())/(CB*MW) - (0.125D0*CA1*CA22*CA3*m12squared*dgAtMZ())/(CB*MW) + (0.125D0*CA3*MH12*SA1&
  &*dgAtMZ())/(CB*MW) + (0.375D0*CA12*CA3*MH12*SA1*dgAtMZ())/(CB*MW) + (0.125D0*CA22*CA3*MH12*SA1*dgAtMZ())/(CB*MW) + (0.375D0*CA&
  &12*CA22*CA3*MH12*SA1*dgAtMZ())/(CB*MW) + (0.0625D0*CA3*MH22*SA1*dgAtMZ())/(CB*MW) + (0.1875D0*CA12*CA3*MH22*SA1*dgAtMZ())/(CB*&
  &MW) + (0.0625D0*CA22*CA3*MH22*SA1*dgAtMZ())/(CB*MW) + (0.1875D0*CA12*CA22*CA3*MH22*SA1*dgAtMZ())/(CB*MW) + (0.125D0*CA1*CA3*m1&
  &2squared*SA22*dgAtMZ())/(CB*MW) - (0.125D0*CA3*MH12*SA1*SA22*dgAtMZ())/(CB*MW) - (0.375D0*CA12*CA3*MH12*SA1*SA22*dgAtMZ())/(CB&
  &*MW) - (0.0625D0*CA3*MH22*SA1*SA22*dgAtMZ())/(CB*MW) - (0.1875D0*CA12*CA3*MH22*SA1*SA22*dgAtMZ())/(CB*MW) + (0.1875D0*CA1*MH12&
  &*SA2*SA3*dgAtMZ())/(CB*MW) + (0.5625D0*CA1*CA22*MH12*SA2*SA3*dgAtMZ())/(CB*MW) + (0.09375D0*CA1*MH22*SA2*SA3*dgAtMZ())/(CB*MW)&
  & + (0.28125D0*CA1*CA22*MH22*SA2*SA3*dgAtMZ())/(CB*MW) + (0.28125D0*m12squared*SA1*SA2*SA3*dgAtMZ())/(CB*MW) + (0.84375D0*CA22*&
  &m12squared*SA1*SA2*SA3*dgAtMZ())/(CB*MW) - (0.1875D0*CA1*MH12*SA12*SA2*SA3*dgAtMZ())/(CB*MW) - (0.5625D0*CA1*CA22*MH12*SA12*SA&
  &2*SA3*dgAtMZ())/(CB*MW) - (0.09375D0*CA1*MH22*SA12*SA2*SA3*dgAtMZ())/(CB*MW) - (0.28125D0*CA1*CA22*MH22*SA12*SA2*SA3*dgAtMZ())&
  &/(CB*MW) - (0.125D0*CA1*CA3*MH12*dgAtMZ())/(MW*SB) - (0.125D0*CA1*CA22*CA3*MH12*dgAtMZ())/(MW*SB) - (0.0625D0*CA1*CA3*MH22*dgA&
  &tMZ())/(MW*SB) - (0.0625D0*CA1*CA22*CA3*MH22*dgAtMZ())/(MW*SB) + (0.21875D0*CA3*m12squared*SA1*dgAtMZ())/(MW*SB) + (0.21875D0*&
  &CA22*CA3*m12squared*SA1*dgAtMZ())/(MW*SB) - (0.15625D0*CA3*m12squared*SA1*dgAtMZ())/(CB2*MW*SB) - (0.5625D0*CA12*CA3*m12square&
  &d*SA1*dgAtMZ())/(CB2*MW*SB) - (0.15625D0*CA22*CA3*m12squared*SA1*dgAtMZ())/(CB2*MW*SB) - (0.5625D0*CA12*CA22*CA3*m12squared*SA&
  &1*dgAtMZ())/(CB2*MW*SB) - (0.375D0*CA1*CA3*MH12*SA12*dgAtMZ())/(MW*SB) - (0.375D0*CA1*CA22*CA3*MH12*SA12*dgAtMZ())/(MW*SB) - (&
  &0.1875D0*CA1*CA3*MH22*SA12*dgAtMZ())/(MW*SB) - (0.1875D0*CA1*CA22*CA3*MH22*SA12*dgAtMZ())/(MW*SB) + (0.125D0*CA1*CA3*MH12*SA22&
  &*dgAtMZ())/(MW*SB) + (0.0625D0*CA1*CA3*MH22*SA22*dgAtMZ())/(MW*SB) - (0.21875D0*CA3*m12squared*SA1*SA22*dgAtMZ())/(MW*SB) + (0&
  &.15625D0*CA3*m12squared*SA1*SA22*dgAtMZ())/(CB2*MW*SB) + (0.5625D0*CA12*CA3*m12squared*SA1*SA22*dgAtMZ())/(CB2*MW*SB) + (0.375&
  &D0*CA1*CA3*MH12*SA12*SA22*dgAtMZ())/(MW*SB) + (0.1875D0*CA1*CA3*MH22*SA12*SA22*dgAtMZ())/(MW*SB) + (0.28125D0*CA1*m12squared*S&
  &A2*SA3*dgAtMZ())/(MW*SB) + (0.84375D0*CA1*CA22*m12squared*SA2*SA3*dgAtMZ())/(MW*SB) - (0.1875D0*CA1*m12squared*SA2*SA3*dgAtMZ(&
  &))/(CB2*MW*SB) - (0.5625D0*CA1*CA22*m12squared*SA2*SA3*dgAtMZ())/(CB2*MW*SB) + (0.1875D0*MH12*SA1*SA2*SA3*dgAtMZ())/(MW*SB) - &
  &(0.1875D0*CA12*MH12*SA1*SA2*SA3*dgAtMZ())/(MW*SB) + (0.5625D0*CA22*MH12*SA1*SA2*SA3*dgAtMZ())/(MW*SB) - (0.5625D0*CA12*CA22*MH&
  &12*SA1*SA2*SA3*dgAtMZ())/(MW*SB) + (0.09375D0*MH22*SA1*SA2*SA3*dgAtMZ())/(MW*SB) - (0.09375D0*CA12*MH22*SA1*SA2*SA3*dgAtMZ())/&
  &(MW*SB) + (0.28125D0*CA22*MH22*SA1*SA2*SA3*dgAtMZ())/(MW*SB) - (0.28125D0*CA12*CA22*MH22*SA1*SA2*SA3*dgAtMZ())/(MW*SB) + (0.28&
  &125D0*CA1*m12squared*SA12*SA2*SA3*dgAtMZ())/(CB2*MW*SB) + (0.84375D0*CA1*CA22*m12squared*SA12*SA2*SA3*dgAtMZ())/(CB2*MW*SB) + &
  &(0.0625D0*CA1*CA3*m12squared*dgAtMZ())/(CB*MW*SB2) + (0.0625D0*CA1*CA22*CA3*m12squared*dgAtMZ())/(CB*MW*SB2) + (0.5625D0*CA1*C&
  &A3*m12squared*SA12*dgAtMZ())/(CB*MW*SB2) + (0.5625D0*CA1*CA22*CA3*m12squared*SA12*dgAtMZ())/(CB*MW*SB2) - (0.0625D0*CA1*CA3*m1&
  &2squared*SA22*dgAtMZ())/(CB*MW*SB2) - (0.5625D0*CA1*CA3*m12squared*SA12*SA22*dgAtMZ())/(CB*MW*SB2) - (0.1875D0*m12squared*SA1*&
  &SA2*SA3*dgAtMZ())/(CB*MW*SB2) + (0.28125D0*CA12*m12squared*SA1*SA2*SA3*dgAtMZ())/(CB*MW*SB2) - (0.5625D0*CA22*m12squared*SA1*S&
  &A2*SA3*dgAtMZ())/(CB*MW*SB2) + (0.84375D0*CA12*CA22*m12squared*SA1*SA2*SA3*dgAtMZ())/(CB*MW*SB2) + (0.125D0*CA1*CA3*m12squared&
  &*dgAtMZ())/(MW*SB*TB) + (0.125D0*CA1*CA22*CA3*m12squared*dgAtMZ())/(MW*SB*TB) - (0.125D0*CA1*CA3*m12squared*SA22*dgAtMZ())/(MW&
  &*SB*TB) - (0.09375D0*m12squared*SA1*SA2*SA3*dgAtMZ())/(MW*SB*TB) - (0.28125D0*CA22*m12squared*SA1*SA2*SA3*dgAtMZ())/(MW*SB*TB)&
  & - (0.03125D0*CA3*m12squared*SA1*TB*dgAtMZ())/(CB*MW) - (0.03125D0*CA22*CA3*m12squared*SA1*TB*dgAtMZ())/(CB*MW) + (0.03125D0*C&
  &A3*m12squared*SA1*SA22*TB*dgAtMZ())/(CB*MW) - (0.09375D0*CA1*m12squared*SA2*SA3*TB*dgAtMZ())/(CB*MW) - (0.28125D0*CA1*CA22*m12&
  &squared*SA2*SA3*TB*dgAtMZ())/(CB*MW) + (0.0625D0*MH12*SA2*SA3*DBLE(CA1**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.1875D0*CA22*MH12*SA2&
  &*SA3*DBLE(CA1**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.03125D0*MH22*SA2*SA3*DBLE(CA1**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.09375D0*CA22&
  &*MH22*SA2*SA3*DBLE(CA1**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.125D0*CA3*MH12*DBLE(CA1**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.125D0*CA2&
  &2*CA3*MH12*DBLE(CA1**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.0625D0*CA3*MH22*DBLE(CA1**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.0625D0*CA22&
  &*CA3*MH22*DBLE(CA1**INT(3.D0))*dgAtMZ())/(MW*SB) - (0.125D0*CA3*MH12*SA22*DBLE(CA1**INT(3.D0))*dgAtMZ())/(MW*SB) - (0.0625D0*C&
  &A3*MH22*SA22*DBLE(CA1**INT(3.D0))*dgAtMZ())/(MW*SB) - (0.09375D0*m12squared*SA2*SA3*DBLE(CA1**INT(3.D0))*dgAtMZ())/(CB2*MW*SB)&
  & - (0.28125D0*CA22*m12squared*SA2*SA3*DBLE(CA1**INT(3.D0))*dgAtMZ())/(CB2*MW*SB) - (0.1875D0*CA3*m12squared*DBLE(CA1**INT(3.D0&
  &))*dgAtMZ())/(CB*MW*SB2) - (0.1875D0*CA22*CA3*m12squared*DBLE(CA1**INT(3.D0))*dgAtMZ())/(CB*MW*SB2) + (0.1875D0*CA3*m12squared&
  &*SA22*DBLE(CA1**INT(3.D0))*dgAtMZ())/(CB*MW*SB2) - (0.125D0*CA3*MH12*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB*MW) - (0.125D0*CA22*CA&
  &3*MH12*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB*MW) - (0.0625D0*CA3*MH22*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB*MW) - (0.0625D0*CA22*CA3&
  &*MH22*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.125D0*CA3*MH12*SA22*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.0625D0*CA3*M&
  &H22*SA22*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.1875D0*CA3*m12squared*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB2*MW*SB) + (0.187&
  &5D0*CA22*CA3*m12squared*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB2*MW*SB) - (0.1875D0*CA3*m12squared*SA22*DBLE(SA1**INT(3.D0))*dgAtMZ&
  &())/(CB2*MW*SB) + (0.0625D0*MH12*SA2*SA3*DBLE(SA1**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.1875D0*CA22*MH12*SA2*SA3*DBLE(SA1**INT(3.&
  &D0))*dgAtMZ())/(MW*SB) + (0.03125D0*MH22*SA2*SA3*DBLE(SA1**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.09375D0*CA22*MH22*SA2*SA3*DBLE(SA&
  &1**INT(3.D0))*dgAtMZ())/(MW*SB) - (0.09375D0*m12squared*SA2*SA3*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB*MW*SB2) - (0.28125D0*CA22*m&
  &12squared*SA2*SA3*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB*MW*SB2) - (0.1875D0*CA1*MH12*SA3*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW) -&
  & (0.09375D0*CA1*MH22*SA3*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW) - (0.28125D0*m12squared*SA1*SA3*DBLE(SA2**INT(3.D0))*dgAtMZ())&
  &/(CB*MW) + (0.1875D0*CA1*MH12*SA12*SA3*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.09375D0*CA1*MH22*SA12*SA3*DBLE(SA2**INT(3.D&
  &0))*dgAtMZ())/(CB*MW) - (0.28125D0*CA1*m12squared*SA3*DBLE(SA2**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.1875D0*CA1*m12squared*SA3*DB&
  &LE(SA2**INT(3.D0))*dgAtMZ())/(CB2*MW*SB) - (0.1875D0*MH12*SA1*SA3*DBLE(SA2**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.1875D0*CA12*MH12&
  &*SA1*SA3*DBLE(SA2**INT(3.D0))*dgAtMZ())/(MW*SB) - (0.09375D0*MH22*SA1*SA3*DBLE(SA2**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.09375D0*&
  &CA12*MH22*SA1*SA3*DBLE(SA2**INT(3.D0))*dgAtMZ())/(MW*SB) - (0.28125D0*CA1*m12squared*SA12*SA3*DBLE(SA2**INT(3.D0))*dgAtMZ())/(&
  &CB2*MW*SB) + (0.1875D0*m12squared*SA1*SA3*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW*SB2) - (0.28125D0*CA12*m12squared*SA1*SA3*DBLE&
  &(SA2**INT(3.D0))*dgAtMZ())/(CB*MW*SB2) + (0.09375D0*m12squared*SA1*SA3*DBLE(SA2**INT(3.D0))*dgAtMZ())/(MW*SB*TB) + (0.09375D0*&
  &CA1*m12squared*SA3*TB*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW) - (0.0625D0*MH12*SA3*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dg&
  &AtMZ())/(CB*MW) - (0.03125D0*MH22*SA3*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.09375D0*m12squared*SA3*&
  &DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB2*MW*SB) - (0.0625D0*MH12*SA3*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))&
  &*dgAtMZ())/(MW*SB) - (0.03125D0*MH22*SA3*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.09375D0*m12squared*S&
  &A3*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW*SB2) - (0.125D0*CA1*CA3*EL*dm122MSBarAlter())/(CB*MW*SW) - (0.12&
  &5D0*CA1*CA22*CA3*EL*dm122MSBarAlter())/(CB*MW*SW) + (0.125D0*CA1*CA3*EL*SA22*dm122MSBarAlter())/(CB*MW*SW) + (0.28125D0*EL*SA1&
  &*SA2*SA3*dm122MSBarAlter())/(CB*MW*SW) + (0.84375D0*CA22*EL*SA1*SA2*SA3*dm122MSBarAlter())/(CB*MW*SW) + (0.21875D0*CA3*EL*SA1*&
  &dm122MSBarAlter())/(MW*SB*SW) + (0.21875D0*CA22*CA3*EL*SA1*dm122MSBarAlter())/(MW*SB*SW) - (0.15625D0*CA3*EL*SA1*dm122MSBarAlt&
  &er())/(CB2*MW*SB*SW) - (0.5625D0*CA12*CA3*EL*SA1*dm122MSBarAlter())/(CB2*MW*SB*SW) - (0.15625D0*CA22*CA3*EL*SA1*dm122MSBarAlte&
  &r())/(CB2*MW*SB*SW) - (0.5625D0*CA12*CA22*CA3*EL*SA1*dm122MSBarAlter())/(CB2*MW*SB*SW) - (0.21875D0*CA3*EL*SA1*SA22*dm122MSBar&
  &Alter())/(MW*SB*SW) + (0.15625D0*CA3*EL*SA1*SA22*dm122MSBarAlter())/(CB2*MW*SB*SW) + (0.5625D0*CA12*CA3*EL*SA1*SA22*dm122MSBar&
  &Alter())/(CB2*MW*SB*SW) + (0.28125D0*CA1*EL*SA2*SA3*dm122MSBarAlter())/(MW*SB*SW) + (0.84375D0*CA1*CA22*EL*SA2*SA3*dm122MSBarA&
  &lter())/(MW*SB*SW) - (0.1875D0*CA1*EL*SA2*SA3*dm122MSBarAlter())/(CB2*MW*SB*SW) - (0.5625D0*CA1*CA22*EL*SA2*SA3*dm122MSBarAlte&
  &r())/(CB2*MW*SB*SW) + (0.28125D0*CA1*EL*SA12*SA2*SA3*dm122MSBarAlter())/(CB2*MW*SB*SW) + (0.84375D0*CA1*CA22*EL*SA12*SA2*SA3*d&
  &m122MSBarAlter())/(CB2*MW*SB*SW) + (0.0625D0*CA1*CA3*EL*dm122MSBarAlter())/(CB*MW*SB2*SW) + (0.0625D0*CA1*CA22*CA3*EL*dm122MSB&
  &arAlter())/(CB*MW*SB2*SW) + (0.5625D0*CA1*CA3*EL*SA12*dm122MSBarAlter())/(CB*MW*SB2*SW) + (0.5625D0*CA1*CA22*CA3*EL*SA12*dm122&
  &MSBarAlter())/(CB*MW*SB2*SW) - (0.0625D0*CA1*CA3*EL*SA22*dm122MSBarAlter())/(CB*MW*SB2*SW) - (0.5625D0*CA1*CA3*EL*SA12*SA22*dm&
  &122MSBarAlter())/(CB*MW*SB2*SW) - (0.1875D0*EL*SA1*SA2*SA3*dm122MSBarAlter())/(CB*MW*SB2*SW) + (0.28125D0*CA12*EL*SA1*SA2*SA3*&
  &dm122MSBarAlter())/(CB*MW*SB2*SW) - (0.5625D0*CA22*EL*SA1*SA2*SA3*dm122MSBarAlter())/(CB*MW*SB2*SW) + (0.84375D0*CA12*CA22*EL*&
  &SA1*SA2*SA3*dm122MSBarAlter())/(CB*MW*SB2*SW) + (0.125D0*CA1*CA3*EL*dm122MSBarAlter())/(MW*SB*SW*TB) + (0.125D0*CA1*CA22*CA3*E&
  &L*dm122MSBarAlter())/(MW*SB*SW*TB) - (0.125D0*CA1*CA3*EL*SA22*dm122MSBarAlter())/(MW*SB*SW*TB) - (0.09375D0*EL*SA1*SA2*SA3*dm1&
  &22MSBarAlter())/(MW*SB*SW*TB) - (0.28125D0*CA22*EL*SA1*SA2*SA3*dm122MSBarAlter())/(MW*SB*SW*TB) - (0.03125D0*CA3*EL*SA1*TB*dm1&
  &22MSBarAlter())/(CB*MW*SW) - (0.03125D0*CA22*CA3*EL*SA1*TB*dm122MSBarAlter())/(CB*MW*SW) + (0.03125D0*CA3*EL*SA1*SA22*TB*dm122&
  &MSBarAlter())/(CB*MW*SW) - (0.09375D0*CA1*EL*SA2*SA3*TB*dm122MSBarAlter())/(CB*MW*SW) - (0.28125D0*CA1*CA22*EL*SA2*SA3*TB*dm12&
  &2MSBarAlter())/(CB*MW*SW) - (0.09375D0*EL*SA2*SA3*DBLE(CA1**INT(3.D0))*dm122MSBarAlter())/(CB2*MW*SB*SW) - (0.28125D0*CA22*EL*&
  &SA2*SA3*DBLE(CA1**INT(3.D0))*dm122MSBarAlter())/(CB2*MW*SB*SW) - (0.1875D0*CA3*EL*DBLE(CA1**INT(3.D0))*dm122MSBarAlter())/(CB*&
  &MW*SB2*SW) - (0.1875D0*CA22*CA3*EL*DBLE(CA1**INT(3.D0))*dm122MSBarAlter())/(CB*MW*SB2*SW) + (0.1875D0*CA3*EL*SA22*DBLE(CA1**IN&
  &T(3.D0))*dm122MSBarAlter())/(CB*MW*SB2*SW) + (0.1875D0*CA3*EL*DBLE(SA1**INT(3.D0))*dm122MSBarAlter())/(CB2*MW*SB*SW) + (0.1875&
  &D0*CA22*CA3*EL*DBLE(SA1**INT(3.D0))*dm122MSBarAlter())/(CB2*MW*SB*SW) - (0.1875D0*CA3*EL*SA22*DBLE(SA1**INT(3.D0))*dm122MSBarA&
  &lter())/(CB2*MW*SB*SW) - (0.09375D0*EL*SA2*SA3*DBLE(SA1**INT(3.D0))*dm122MSBarAlter())/(CB*MW*SB2*SW) - (0.28125D0*CA22*EL*SA2&
  &*SA3*DBLE(SA1**INT(3.D0))*dm122MSBarAlter())/(CB*MW*SB2*SW) - (0.28125D0*EL*SA1*SA3*DBLE(SA2**INT(3.D0))*dm122MSBarAlter())/(C&
  &B*MW*SW) - (0.28125D0*CA1*EL*SA3*DBLE(SA2**INT(3.D0))*dm122MSBarAlter())/(MW*SB*SW) + (0.1875D0*CA1*EL*SA3*DBLE(SA2**INT(3.D0)&
  &)*dm122MSBarAlter())/(CB2*MW*SB*SW) - (0.28125D0*CA1*EL*SA12*SA3*DBLE(SA2**INT(3.D0))*dm122MSBarAlter())/(CB2*MW*SB*SW) + (0.1&
  &875D0*EL*SA1*SA3*DBLE(SA2**INT(3.D0))*dm122MSBarAlter())/(CB*MW*SB2*SW) - (0.28125D0*CA12*EL*SA1*SA3*DBLE(SA2**INT(3.D0))*dm12&
  &2MSBarAlter())/(CB*MW*SB2*SW) + (0.09375D0*EL*SA1*SA3*DBLE(SA2**INT(3.D0))*dm122MSBarAlter())/(MW*SB*SW*TB) + (0.09375D0*CA1*E&
  &L*SA3*TB*DBLE(SA2**INT(3.D0))*dm122MSBarAlter())/(CB*MW*SW) + (0.09375D0*EL*SA3*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dm12&
  &2MSBarAlter())/(CB2*MW*SB*SW) + (0.09375D0*EL*SA3*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dm122MSBarAlter())/(CB*MW*SB2*SW) &
  &+ (0.125D0*CA3*EL*SA1*dMH12OSAlter())/(CB*MW*SW) + (0.375D0*CA12*CA3*EL*SA1*dMH12OSAlter())/(CB*MW*SW) + (0.125D0*CA22*CA3*EL*&
  &SA1*dMH12OSAlter())/(CB*MW*SW) + (0.375D0*CA12*CA22*CA3*EL*SA1*dMH12OSAlter())/(CB*MW*SW) - (0.125D0*CA3*EL*SA1*SA22*dMH12OSAl&
  &ter())/(CB*MW*SW) - (0.375D0*CA12*CA3*EL*SA1*SA22*dMH12OSAlter())/(CB*MW*SW) + (0.1875D0*CA1*EL*SA2*SA3*dMH12OSAlter())/(CB*MW&
  &*SW) + (0.5625D0*CA1*CA22*EL*SA2*SA3*dMH12OSAlter())/(CB*MW*SW) - (0.1875D0*CA1*EL*SA12*SA2*SA3*dMH12OSAlter())/(CB*MW*SW) - (&
  &0.5625D0*CA1*CA22*EL*SA12*SA2*SA3*dMH12OSAlter())/(CB*MW*SW) - (0.125D0*CA1*CA3*EL*dMH12OSAlter())/(MW*SB*SW) - (0.125D0*CA1*C&
  &A22*CA3*EL*dMH12OSAlter())/(MW*SB*SW) - (0.375D0*CA1*CA3*EL*SA12*dMH12OSAlter())/(MW*SB*SW) - (0.375D0*CA1*CA22*CA3*EL*SA12*dM&
  &H12OSAlter())/(MW*SB*SW) + (0.125D0*CA1*CA3*EL*SA22*dMH12OSAlter())/(MW*SB*SW) + (0.375D0*CA1*CA3*EL*SA12*SA22*dMH12OSAlter())&
  &/(MW*SB*SW) + (0.1875D0*EL*SA1*SA2*SA3*dMH12OSAlter())/(MW*SB*SW) - (0.1875D0*CA12*EL*SA1*SA2*SA3*dMH12OSAlter())/(MW*SB*SW) +&
  & (0.5625D0*CA22*EL*SA1*SA2*SA3*dMH12OSAlter())/(MW*SB*SW) - (0.5625D0*CA12*CA22*EL*SA1*SA2*SA3*dMH12OSAlter())/(MW*SB*SW) - (0&
  &.5D0*CA2*SA3*dMH12OSAlter())/vS - (1.5D0*CA2*SA22*SA3*dMH12OSAlter())/vS + (0.0625D0*EL*SA2*SA3*DBLE(CA1**INT(3.D0))*dMH12OSAl&
  &ter())/(CB*MW*SW) + (0.1875D0*CA22*EL*SA2*SA3*DBLE(CA1**INT(3.D0))*dMH12OSAlter())/(CB*MW*SW) + (0.125D0*CA3*EL*DBLE(CA1**INT(&
  &3.D0))*dMH12OSAlter())/(MW*SB*SW) + (0.125D0*CA22*CA3*EL*DBLE(CA1**INT(3.D0))*dMH12OSAlter())/(MW*SB*SW) - (0.125D0*CA3*EL*SA2&
  &2*DBLE(CA1**INT(3.D0))*dMH12OSAlter())/(MW*SB*SW) + (0.5D0*SA3*DBLE(CA2**INT(3.D0))*dMH12OSAlter())/vS - (0.125D0*CA3*EL*DBLE(&
  &SA1**INT(3.D0))*dMH12OSAlter())/(CB*MW*SW) - (0.125D0*CA22*CA3*EL*DBLE(SA1**INT(3.D0))*dMH12OSAlter())/(CB*MW*SW) + (0.125D0*C&
  &A3*EL*SA22*DBLE(SA1**INT(3.D0))*dMH12OSAlter())/(CB*MW*SW) + (0.0625D0*EL*SA2*SA3*DBLE(SA1**INT(3.D0))*dMH12OSAlter())/(MW*SB*&
  &SW) + (0.1875D0*CA22*EL*SA2*SA3*DBLE(SA1**INT(3.D0))*dMH12OSAlter())/(MW*SB*SW) - (0.1875D0*CA1*EL*SA3*DBLE(SA2**INT(3.D0))*dM&
  &H12OSAlter())/(CB*MW*SW) + (0.1875D0*CA1*EL*SA12*SA3*DBLE(SA2**INT(3.D0))*dMH12OSAlter())/(CB*MW*SW) - (0.1875D0*EL*SA1*SA3*DB&
  &LE(SA2**INT(3.D0))*dMH12OSAlter())/(MW*SB*SW) + (0.1875D0*CA12*EL*SA1*SA3*DBLE(SA2**INT(3.D0))*dMH12OSAlter())/(MW*SB*SW) - (0&
  &.0625D0*EL*SA3*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dMH12OSAlter())/(CB*MW*SW) - (0.0625D0*EL*SA3*DBLE(SA1**INT(3.D0))*DB&
  &LE(SA2**INT(3.D0))*dMH12OSAlter())/(MW*SB*SW) + (0.0625D0*CA3*EL*SA1*dMH22OSAlter())/(CB*MW*SW) + (0.1875D0*CA12*CA3*EL*SA1*dM&
  &H22OSAlter())/(CB*MW*SW) + (0.0625D0*CA22*CA3*EL*SA1*dMH22OSAlter())/(CB*MW*SW) + (0.1875D0*CA12*CA22*CA3*EL*SA1*dMH22OSAlter(&
  &))/(CB*MW*SW) - (0.0625D0*CA3*EL*SA1*SA22*dMH22OSAlter())/(CB*MW*SW) - (0.1875D0*CA12*CA3*EL*SA1*SA22*dMH22OSAlter())/(CB*MW*S&
  &W) + (0.09375D0*CA1*EL*SA2*SA3*dMH22OSAlter())/(CB*MW*SW) + (0.28125D0*CA1*CA22*EL*SA2*SA3*dMH22OSAlter())/(CB*MW*SW) - (0.093&
  &75D0*CA1*EL*SA12*SA2*SA3*dMH22OSAlter())/(CB*MW*SW) - (0.28125D0*CA1*CA22*EL*SA12*SA2*SA3*dMH22OSAlter())/(CB*MW*SW) - (0.0625&
  &D0*CA1*CA3*EL*dMH22OSAlter())/(MW*SB*SW) - (0.0625D0*CA1*CA22*CA3*EL*dMH22OSAlter())/(MW*SB*SW) - (0.1875D0*CA1*CA3*EL*SA12*dM&
  &H22OSAlter())/(MW*SB*SW) - (0.1875D0*CA1*CA22*CA3*EL*SA12*dMH22OSAlter())/(MW*SB*SW) + (0.0625D0*CA1*CA3*EL*SA22*dMH22OSAlter(&
  &))/(MW*SB*SW) + (0.1875D0*CA1*CA3*EL*SA12*SA22*dMH22OSAlter())/(MW*SB*SW) + (0.09375D0*EL*SA1*SA2*SA3*dMH22OSAlter())/(MW*SB*S&
  &W) - (0.09375D0*CA12*EL*SA1*SA2*SA3*dMH22OSAlter())/(MW*SB*SW) + (0.28125D0*CA22*EL*SA1*SA2*SA3*dMH22OSAlter())/(MW*SB*SW) - (&
  &0.28125D0*CA12*CA22*EL*SA1*SA2*SA3*dMH22OSAlter())/(MW*SB*SW) - (0.25D0*CA2*SA3*dMH22OSAlter())/vS - (0.75D0*CA2*SA22*SA3*dMH2&
  &2OSAlter())/vS + (0.03125D0*EL*SA2*SA3*DBLE(CA1**INT(3.D0))*dMH22OSAlter())/(CB*MW*SW) + (0.09375D0*CA22*EL*SA2*SA3*DBLE(CA1**&
  &INT(3.D0))*dMH22OSAlter())/(CB*MW*SW) + (0.0625D0*CA3*EL*DBLE(CA1**INT(3.D0))*dMH22OSAlter())/(MW*SB*SW) + (0.0625D0*CA22*CA3*&
  &EL*DBLE(CA1**INT(3.D0))*dMH22OSAlter())/(MW*SB*SW) - (0.0625D0*CA3*EL*SA22*DBLE(CA1**INT(3.D0))*dMH22OSAlter())/(MW*SB*SW) + (&
  &0.25D0*SA3*DBLE(CA2**INT(3.D0))*dMH22OSAlter())/vS - (0.0625D0*CA3*EL*DBLE(SA1**INT(3.D0))*dMH22OSAlter())/(CB*MW*SW) - (0.062&
  &5D0*CA22*CA3*EL*DBLE(SA1**INT(3.D0))*dMH22OSAlter())/(CB*MW*SW) + (0.0625D0*CA3*EL*SA22*DBLE(SA1**INT(3.D0))*dMH22OSAlter())/(&
  &CB*MW*SW) + (0.03125D0*EL*SA2*SA3*DBLE(SA1**INT(3.D0))*dMH22OSAlter())/(MW*SB*SW) + (0.09375D0*CA22*EL*SA2*SA3*DBLE(SA1**INT(3&
  &.D0))*dMH22OSAlter())/(MW*SB*SW) - (0.09375D0*CA1*EL*SA3*DBLE(SA2**INT(3.D0))*dMH22OSAlter())/(CB*MW*SW) + (0.09375D0*CA1*EL*S&
  &A12*SA3*DBLE(SA2**INT(3.D0))*dMH22OSAlter())/(CB*MW*SW) - (0.09375D0*EL*SA1*SA3*DBLE(SA2**INT(3.D0))*dMH22OSAlter())/(MW*SB*SW&
  &) + (0.09375D0*CA12*EL*SA1*SA3*DBLE(SA2**INT(3.D0))*dMH22OSAlter())/(MW*SB*SW) - (0.03125D0*EL*SA3*DBLE(CA1**INT(3.D0))*DBLE(S&
  &A2**INT(3.D0))*dMH22OSAlter())/(CB*MW*SW) - (0.03125D0*EL*SA3*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dMH22OSAlter())/(MW*SB&
  &*SW) + (0.0625D0*CA1*CA3*EL*m12squared*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.0625D0*CA1*CA22*CA3*EL*m12squared*DBLE(M&
  &W**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.0625D0*CA3*EL*MH12*SA1*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.1875D0*CA12*CA3&
  &*EL*MH12*SA1*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.0625D0*CA22*CA3*EL*MH12*SA1*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*&
  &SW) - (0.1875D0*CA12*CA22*CA3*EL*MH12*SA1*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.03125D0*CA3*EL*MH22*SA1*DBLE(MW**INT(&
  &-3.D0))*dMW2Alter())/(CB*SW) - (0.09375D0*CA12*CA3*EL*MH22*SA1*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.03125D0*CA22*CA3&
  &*EL*MH22*SA1*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.09375D0*CA12*CA22*CA3*EL*MH22*SA1*DBLE(MW**INT(-3.D0))*dMW2Alter()&
  &)/(CB*SW) - (0.0625D0*CA1*CA3*EL*m12squared*SA22*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.0625D0*CA3*EL*MH12*SA1*SA22*DB&
  &LE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.1875D0*CA12*CA3*EL*MH12*SA1*SA22*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.0&
  &3125D0*CA3*EL*MH22*SA1*SA22*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.09375D0*CA12*CA3*EL*MH22*SA1*SA22*DBLE(MW**INT(-3.D&
  &0))*dMW2Alter())/(CB*SW) - (0.09375D0*CA1*EL*MH12*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.28125D0*CA1*CA22*EL*M&
  &H12*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.046875D0*CA1*EL*MH22*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*&
  &SW) - (0.140625D0*CA1*CA22*EL*MH22*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.140625D0*EL*m12squared*SA1*SA2*SA3*D&
  &BLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.421875D0*CA22*EL*m12squared*SA1*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW&
  &) + (0.09375D0*CA1*EL*MH12*SA12*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.28125D0*CA1*CA22*EL*MH12*SA12*SA2*SA3*D&
  &BLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.046875D0*CA1*EL*MH22*SA12*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0&
  &.140625D0*CA1*CA22*EL*MH22*SA12*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.0625D0*CA1*CA3*EL*MH12*DBLE(MW**INT(-3.&
  &D0))*dMW2Alter())/(SB*SW) + (0.0625D0*CA1*CA22*CA3*EL*MH12*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.03125D0*CA1*CA3*EL*M&
  &H22*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.03125D0*CA1*CA22*CA3*EL*MH22*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0&
  &.109375D0*CA3*EL*m12squared*SA1*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.109375D0*CA22*CA3*EL*m12squared*SA1*DBLE(MW**IN&
  &T(-3.D0))*dMW2Alter())/(SB*SW) + (0.078125D0*CA3*EL*m12squared*SA1*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB2*SB*SW) + (0.28125D0*&
  &CA12*CA3*EL*m12squared*SA1*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB2*SB*SW) + (0.078125D0*CA22*CA3*EL*m12squared*SA1*DBLE(MW**INT&
  &(-3.D0))*dMW2Alter())/(CB2*SB*SW) + (0.28125D0*CA12*CA22*CA3*EL*m12squared*SA1*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB2*SB*SW) +&
  & (0.1875D0*CA1*CA3*EL*MH12*SA12*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.1875D0*CA1*CA22*CA3*EL*MH12*SA12*DBLE(MW**INT(-&
  &3.D0))*dMW2Alter())/(SB*SW) + (0.09375D0*CA1*CA3*EL*MH22*SA12*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.09375D0*CA1*CA22*&
  &CA3*EL*MH22*SA12*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.0625D0*CA1*CA3*EL*MH12*SA22*DBLE(MW**INT(-3.D0))*dMW2Alter())/&
  &(SB*SW) - (0.03125D0*CA1*CA3*EL*MH22*SA22*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.109375D0*CA3*EL*m12squared*SA1*SA22*D&
  &BLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.078125D0*CA3*EL*m12squared*SA1*SA22*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB2*SB*SW&
  &) - (0.28125D0*CA12*CA3*EL*m12squared*SA1*SA22*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB2*SB*SW) - (0.1875D0*CA1*CA3*EL*MH12*SA12*&
  &SA22*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.09375D0*CA1*CA3*EL*MH22*SA12*SA22*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW&
  &) - (0.140625D0*CA1*EL*m12squared*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.421875D0*CA1*CA22*EL*m12squared*SA2*S&
  &A3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.09375D0*CA1*EL*m12squared*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB2*SB*&
  &SW) + (0.28125D0*CA1*CA22*EL*m12squared*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB2*SB*SW) - (0.09375D0*EL*MH12*SA1*SA2*SA3&
  &*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.09375D0*CA12*EL*MH12*SA1*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (&
  &0.28125D0*CA22*EL*MH12*SA1*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.28125D0*CA12*CA22*EL*MH12*SA1*SA2*SA3*DBLE(M&
  &W**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.046875D0*EL*MH22*SA1*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.046875D0*&
  &CA12*EL*MH22*SA1*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.140625D0*CA22*EL*MH22*SA1*SA2*SA3*DBLE(MW**INT(-3.D0))&
  &*dMW2Alter())/(SB*SW) + (0.140625D0*CA12*CA22*EL*MH22*SA1*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.140625D0*CA1*&
  &EL*m12squared*SA12*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB2*SB*SW) - (0.421875D0*CA1*CA22*EL*m12squared*SA12*SA2*SA3*DBL&
  &E(MW**INT(-3.D0))*dMW2Alter())/(CB2*SB*SW) - (0.03125D0*CA1*CA3*EL*m12squared*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SB2*SW) - &
  &(0.03125D0*CA1*CA22*CA3*EL*m12squared*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SB2*SW) - (0.28125D0*CA1*CA3*EL*m12squared*SA12*DB&
  &LE(MW**INT(-3.D0))*dMW2Alter())/(CB*SB2*SW) - (0.28125D0*CA1*CA22*CA3*EL*m12squared*SA12*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB&
  &*SB2*SW) + (0.03125D0*CA1*CA3*EL*m12squared*SA22*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SB2*SW) + (0.28125D0*CA1*CA3*EL*m12squa&
  &red*SA12*SA22*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SB2*SW) + (0.09375D0*EL*m12squared*SA1*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Al&
  &ter())/(CB*SB2*SW) - (0.140625D0*CA12*EL*m12squared*SA1*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SB2*SW) + (0.28125D0*CA2&
  &2*EL*m12squared*SA1*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SB2*SW) - (0.421875D0*CA12*CA22*EL*m12squared*SA1*SA2*SA3*DB&
  &LE(MW**INT(-3.D0))*dMW2Alter())/(CB*SB2*SW) - (0.0625D0*CA1*CA3*EL*m12squared*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW*TB) - (&
  &0.0625D0*CA1*CA22*CA3*EL*m12squared*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW*TB) + (0.0625D0*CA1*CA3*EL*m12squared*SA22*DBLE(M&
  &W**INT(-3.D0))*dMW2Alter())/(SB*SW*TB) + (0.046875D0*EL*m12squared*SA1*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW*TB) + &
  &(0.140625D0*CA22*EL*m12squared*SA1*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW*TB) + (0.015625D0*CA3*EL*m12squared*SA1*TB&
  &*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.015625D0*CA22*CA3*EL*m12squared*SA1*TB*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*S&
  &W) - (0.015625D0*CA3*EL*m12squared*SA1*SA22*TB*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.046875D0*CA1*EL*m12squared*SA2*S&
  &A3*TB*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.140625D0*CA1*CA22*EL*m12squared*SA2*SA3*TB*DBLE(MW**INT(-3.D0))*dMW2Alter&
  &())/(CB*SW) - (0.03125D0*EL*MH12*SA2*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.09375D0*CA22*EL*M&
  &H12*SA2*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.015625D0*EL*MH22*SA2*SA3*DBLE(CA1**INT(3.D0))*&
  &DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.046875D0*CA22*EL*MH22*SA2*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alt&
  &er())/(CB*SW) - (0.0625D0*CA3*EL*MH12*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.0625D0*CA22*CA3*EL*M&
  &H12*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.03125D0*CA3*EL*MH22*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(&
  &-3.D0))*dMW2Alter())/(SB*SW) - (0.03125D0*CA22*CA3*EL*MH22*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0&
  &.0625D0*CA3*EL*MH12*SA22*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.03125D0*CA3*EL*MH22*SA22*DBLE(CA1&
  &**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.046875D0*EL*m12squared*SA2*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-&
  &3.D0))*dMW2Alter())/(CB2*SB*SW) + (0.140625D0*CA22*EL*m12squared*SA2*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter()&
  &)/(CB2*SB*SW) + (0.09375D0*CA3*EL*m12squared*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SB2*SW) + (0.09375D0*C&
  &A22*CA3*EL*m12squared*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SB2*SW) - (0.09375D0*CA3*EL*m12squared*SA22*D&
  &BLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SB2*SW) + (0.0625D0*CA3*EL*MH12*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3&
  &.D0))*dMW2Alter())/(CB*SW) + (0.0625D0*CA22*CA3*EL*MH12*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(CB*SW) + (0.03&
  &125D0*CA3*EL*MH22*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(CB*SW) + (0.03125D0*CA22*CA3*EL*MH22*DBLE(MW**INT(-3&
  &.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(CB*SW) - (0.0625D0*CA3*EL*MH12*SA22*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alt&
  &er())/(CB*SW) - (0.03125D0*CA3*EL*MH22*SA22*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(CB*SW) - (0.09375D0*CA3*EL&
  &*m12squared*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(CB2*SB*SW) - (0.09375D0*CA22*CA3*EL*m12squared*DBLE(MW**IN&
  &T(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(CB2*SB*SW) + (0.09375D0*CA3*EL*m12squared*SA22*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT&
  &(3.D0))*dMW2Alter())/(CB2*SB*SW) - (0.03125D0*EL*MH12*SA2*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(SB*SW) -&
  & (0.09375D0*CA22*EL*MH12*SA2*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(SB*SW) - (0.015625D0*EL*MH22*SA2*SA3*&
  &DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(SB*SW) - (0.046875D0*CA22*EL*MH22*SA2*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA&
  &1**INT(3.D0))*dMW2Alter())/(SB*SW) + (0.046875D0*EL*m12squared*SA2*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/&
  &(CB*SB2*SW) + (0.140625D0*CA22*EL*m12squared*SA2*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(CB*SB2*SW) + (0.0&
  &9375D0*CA1*EL*MH12*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(CB*SW) + (0.046875D0*CA1*EL*MH22*SA3*DBLE(MW**I&
  &NT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(CB*SW) + (0.140625D0*EL*m12squared*SA1*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.&
  &D0))*dMW2Alter())/(CB*SW) - (0.09375D0*CA1*EL*MH12*SA12*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(CB*SW) - (&
  &0.046875D0*CA1*EL*MH22*SA12*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(CB*SW) + (0.140625D0*CA1*EL*m12squared&
  &*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(SB*SW) - (0.09375D0*CA1*EL*m12squared*SA3*DBLE(MW**INT(-3.D0))*DB&
  &LE(SA2**INT(3.D0))*dMW2Alter())/(CB2*SB*SW) + (0.09375D0*EL*MH12*SA1*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter()&
  &)/(SB*SW) - (0.09375D0*CA12*EL*MH12*SA1*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(SB*SW) + (0.046875D0*EL*MH&
  &22*SA1*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(SB*SW) - (0.046875D0*CA12*EL*MH22*SA1*SA3*DBLE(MW**INT(-3.D&
  &0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(SB*SW) + (0.140625D0*CA1*EL*m12squared*SA12*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0&
  &))*dMW2Alter())/(CB2*SB*SW) - (0.09375D0*EL*m12squared*SA1*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(CB*SB2*&
  &SW) + (0.140625D0*CA12*EL*m12squared*SA1*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(CB*SB2*SW) - (0.046875D0*&
  &EL*m12squared*SA1*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(SB*SW*TB) - (0.046875D0*CA1*EL*m12squared*SA3*TB&
  &*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(CB*SW) + (0.03125D0*EL*MH12*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.&
  &D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(CB*SW) + (0.015625D0*EL*MH22*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*DBLE(SA2**I&
  &NT(3.D0))*dMW2Alter())/(CB*SW) - (0.046875D0*EL*m12squared*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*&
  &dMW2Alter())/(CB2*SB*SW) + (0.03125D0*EL*MH12*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/&
  &(SB*SW) + (0.015625D0*EL*MH22*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(SB*SW) - (0.046&
  &875D0*EL*m12squared*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(CB*SB2*SW) + 0.5D0*CA2*MH&
  &12*SA3*DBLE(vS**INT(-2.D0))*dvSMSBarAlter() + 0.25D0*CA2*MH22*SA3*DBLE(vS**INT(-2.D0))*dvSMSBarAlter() + 1.5D0*CA2*MH12*SA22*S&
  &A3*DBLE(vS**INT(-2.D0))*dvSMSBarAlter() + 0.75D0*CA2*MH22*SA22*SA3*DBLE(vS**INT(-2.D0))*dvSMSBarAlter() - 0.5D0*MH12*SA3*DBLE(&
  &CA2**INT(3.D0))*DBLE(vS**INT(-2.D0))*dvSMSBarAlter() - 0.25D0*MH22*SA3*DBLE(CA2**INT(3.D0))*DBLE(vS**INT(-2.D0))*dvSMSBarAlter&
  &() &
		& + CS1S1S1f111*dZH1H2OSAlter()/2D0 + CS1S1S1f211*dZH2H2OS()/2D0 + CS1S1S1f311*dZH3H2OSAlter()/2D0 &
		& + CS1S1S1f121*dZH1H1OS()/2D0 + CS1S1S1f221*dZH2H1OSAlter()/2D0 + CS1S1S1f321*dZH3H1OSAlter()/2D0 &
		& + CS1S1S1f121*dZH1H1OS()/2D0 + CS1S1S1f221*dZH2H1OSAlter()/2D0 + CS1S1S1f321*dZH3H1OSAlter()/2D0 &
		& )
	case (6)
		dRR11 = -CA2*SA1*dAlpha1PinchPStar() - CA1*SA2*dAlpha2PinchPStar()
		dRR12 = CA1*CA2*dAlpha1PinchPStar() - SA1*SA2*dAlpha2PinchPStar()
		dRR13 = CA2*dAlpha2PinchPStar()
		dRR21 = -CA1*CA3*dAlpha1PinchPStar() - CA1*CA3*SA2*dAlpha3PinchPStar() - CA1*CA2*SA3*dAlpha2PinchPStar() + &
			& SA1*SA3*dAlpha3PinchPStar() + SA1*SA2*SA3*dAlpha1PinchPStar()
		dRR22 = -CA3*SA1*dAlpha1PinchPStar() - CA3*SA1*SA2*dAlpha3PinchPStar() - CA1*SA3*dAlpha3PinchPStar() - &
			& CA2*SA1*SA3*dAlpha2PinchPStar() - CA1*SA2*SA3*dAlpha1PinchPStar()
		dRR23 = CA2*CA3*dAlpha3PinchPStar() - SA2*SA3*dAlpha2PinchPStar()
		dRR31 = -CA1*CA2*CA3*dAlpha2PinchPStar() + CA3*SA1*dAlpha3PinchPStar() + CA3*SA1*SA2*dAlpha1PinchPStar() + &
			& CA1*SA3*dAlpha1PinchPStar() + CA1*SA2*SA3*dAlpha3PinchPStar()
		dRR32 = -CA1*CA3*dAlpha3PinchPStar() - CA2*CA3*SA1*dAlpha2PinchPStar() - CA1*CA3*SA2*dAlpha1PinchPStar() + &
			& SA1*SA3*dAlpha1PinchPStar() + SA1*SA2*SA3*dAlpha3PinchPStar()
		dRR33 = -CA3*SA2*dAlpha2PinchPStar() - CA2*SA3*dAlpha3PinchPStar()

		totalAmplitude = CS1S1S1f211*( &
&(0.125D0*CA1*CA3*EL*MH12*dAlpha1PinchPStar())/(CB*MW*SW) + (0.125D0*CA1*CA22*CA3*EL*MH12*dAlpha1PinchPStar())/(CB*MW*SW) + (0.062&
  &5D0*CA1*CA3*EL*MH22*dAlpha1PinchPStar())/(CB*MW*SW) + (0.0625D0*CA1*CA22*CA3*EL*MH22*dAlpha1PinchPStar())/(CB*MW*SW) + (0.125D&
  &0*CA3*EL*m12squared*SA1*dAlpha1PinchPStar())/(CB*MW*SW) + (0.125D0*CA22*CA3*EL*m12squared*SA1*dAlpha1PinchPStar())/(CB*MW*SW) &
  &- (1.125D0*CA1*CA3*EL*MH12*SA12*dAlpha1PinchPStar())/(CB*MW*SW) - (1.125D0*CA1*CA22*CA3*EL*MH12*SA12*dAlpha1PinchPStar())/(CB*&
  &MW*SW) - (0.5625D0*CA1*CA3*EL*MH22*SA12*dAlpha1PinchPStar())/(CB*MW*SW) - (0.5625D0*CA1*CA22*CA3*EL*MH22*SA12*dAlpha1PinchPSta&
  &r())/(CB*MW*SW) - (0.125D0*CA1*CA3*EL*MH12*SA22*dAlpha1PinchPStar())/(CB*MW*SW) - (0.0625D0*CA1*CA3*EL*MH22*SA22*dAlpha1PinchP&
  &Star())/(CB*MW*SW) - (0.125D0*CA3*EL*m12squared*SA1*SA22*dAlpha1PinchPStar())/(CB*MW*SW) + (1.125D0*CA1*CA3*EL*MH12*SA12*SA22*&
  &dAlpha1PinchPStar())/(CB*MW*SW) + (0.5625D0*CA1*CA3*EL*MH22*SA12*SA22*dAlpha1PinchPStar())/(CB*MW*SW) + (0.28125D0*CA1*EL*m12s&
  &quared*SA2*SA3*dAlpha1PinchPStar())/(CB*MW*SW) + (0.84375D0*CA1*CA22*EL*m12squared*SA2*SA3*dAlpha1PinchPStar())/(CB*MW*SW) - (&
  &0.1875D0*EL*MH12*SA1*SA2*SA3*dAlpha1PinchPStar())/(CB*MW*SW) - (0.5625D0*CA12*EL*MH12*SA1*SA2*SA3*dAlpha1PinchPStar())/(CB*MW*&
  &SW) - (0.5625D0*CA22*EL*MH12*SA1*SA2*SA3*dAlpha1PinchPStar())/(CB*MW*SW) - (1.6875D0*CA12*CA22*EL*MH12*SA1*SA2*SA3*dAlpha1Pinc&
  &hPStar())/(CB*MW*SW) - (0.09375D0*EL*MH22*SA1*SA2*SA3*dAlpha1PinchPStar())/(CB*MW*SW) - (0.28125D0*CA12*EL*MH22*SA1*SA2*SA3*dA&
  &lpha1PinchPStar())/(CB*MW*SW) - (0.28125D0*CA22*EL*MH22*SA1*SA2*SA3*dAlpha1PinchPStar())/(CB*MW*SW) - (0.84375D0*CA12*CA22*EL*&
  &MH22*SA1*SA2*SA3*dAlpha1PinchPStar())/(CB*MW*SW) + (0.21875D0*CA1*CA3*EL*m12squared*dAlpha1PinchPStar())/(MW*SB*SW) + (0.21875&
  &D0*CA1*CA22*CA3*EL*m12squared*dAlpha1PinchPStar())/(MW*SB*SW) - (0.15625D0*CA1*CA3*EL*m12squared*dAlpha1PinchPStar())/(CB2*MW*&
  &SB*SW) - (0.15625D0*CA1*CA22*CA3*EL*m12squared*dAlpha1PinchPStar())/(CB2*MW*SB*SW) + (0.125D0*CA3*EL*MH12*SA1*dAlpha1PinchPSta&
  &r())/(MW*SB*SW) - (1.125D0*CA12*CA3*EL*MH12*SA1*dAlpha1PinchPStar())/(MW*SB*SW) + (0.125D0*CA22*CA3*EL*MH12*SA1*dAlpha1PinchPS&
  &tar())/(MW*SB*SW) - (1.125D0*CA12*CA22*CA3*EL*MH12*SA1*dAlpha1PinchPStar())/(MW*SB*SW) + (0.0625D0*CA3*EL*MH22*SA1*dAlpha1Pinc&
  &hPStar())/(MW*SB*SW) - (0.5625D0*CA12*CA3*EL*MH22*SA1*dAlpha1PinchPStar())/(MW*SB*SW) + (0.0625D0*CA22*CA3*EL*MH22*SA1*dAlpha1&
  &PinchPStar())/(MW*SB*SW) - (0.5625D0*CA12*CA22*CA3*EL*MH22*SA1*dAlpha1PinchPStar())/(MW*SB*SW) + (1.6875D0*CA1*CA3*EL*m12squar&
  &ed*SA12*dAlpha1PinchPStar())/(CB2*MW*SB*SW) + (1.6875D0*CA1*CA22*CA3*EL*m12squared*SA12*dAlpha1PinchPStar())/(CB2*MW*SB*SW) - &
  &(0.21875D0*CA1*CA3*EL*m12squared*SA22*dAlpha1PinchPStar())/(MW*SB*SW) + (0.15625D0*CA1*CA3*EL*m12squared*SA22*dAlpha1PinchPSta&
  &r())/(CB2*MW*SB*SW) - (0.125D0*CA3*EL*MH12*SA1*SA22*dAlpha1PinchPStar())/(MW*SB*SW) + (1.125D0*CA12*CA3*EL*MH12*SA1*SA22*dAlph&
  &a1PinchPStar())/(MW*SB*SW) - (0.0625D0*CA3*EL*MH22*SA1*SA22*dAlpha1PinchPStar())/(MW*SB*SW) + (0.5625D0*CA12*CA3*EL*MH22*SA1*S&
  &A22*dAlpha1PinchPStar())/(MW*SB*SW) - (1.6875D0*CA1*CA3*EL*m12squared*SA12*SA22*dAlpha1PinchPStar())/(CB2*MW*SB*SW) + (0.1875D&
  &0*CA1*EL*MH12*SA2*SA3*dAlpha1PinchPStar())/(MW*SB*SW) + (0.5625D0*CA1*CA22*EL*MH12*SA2*SA3*dAlpha1PinchPStar())/(MW*SB*SW) + (&
  &0.09375D0*CA1*EL*MH22*SA2*SA3*dAlpha1PinchPStar())/(MW*SB*SW) + (0.28125D0*CA1*CA22*EL*MH22*SA2*SA3*dAlpha1PinchPStar())/(MW*S&
  &B*SW) - (0.28125D0*EL*m12squared*SA1*SA2*SA3*dAlpha1PinchPStar())/(MW*SB*SW) - (0.84375D0*CA22*EL*m12squared*SA1*SA2*SA3*dAlph&
  &a1PinchPStar())/(MW*SB*SW) + (0.1875D0*EL*m12squared*SA1*SA2*SA3*dAlpha1PinchPStar())/(CB2*MW*SB*SW) + (0.84375D0*CA12*EL*m12s&
  &quared*SA1*SA2*SA3*dAlpha1PinchPStar())/(CB2*MW*SB*SW) + (0.5625D0*CA22*EL*m12squared*SA1*SA2*SA3*dAlpha1PinchPStar())/(CB2*MW&
  &*SB*SW) + (2.53125D0*CA12*CA22*EL*m12squared*SA1*SA2*SA3*dAlpha1PinchPStar())/(CB2*MW*SB*SW) + (0.5625D0*CA1*EL*MH12*SA12*SA2*&
  &SA3*dAlpha1PinchPStar())/(MW*SB*SW) + (1.6875D0*CA1*CA22*EL*MH12*SA12*SA2*SA3*dAlpha1PinchPStar())/(MW*SB*SW) + (0.28125D0*CA1&
  &*EL*MH22*SA12*SA2*SA3*dAlpha1PinchPStar())/(MW*SB*SW) + (0.84375D0*CA1*CA22*EL*MH22*SA12*SA2*SA3*dAlpha1PinchPStar())/(MW*SB*S&
  &W) - (0.0625D0*CA3*EL*m12squared*SA1*dAlpha1PinchPStar())/(CB*MW*SB2*SW) + (1.6875D0*CA12*CA3*EL*m12squared*SA1*dAlpha1PinchPS&
  &tar())/(CB*MW*SB2*SW) - (0.0625D0*CA22*CA3*EL*m12squared*SA1*dAlpha1PinchPStar())/(CB*MW*SB2*SW) + (1.6875D0*CA12*CA22*CA3*EL*&
  &m12squared*SA1*dAlpha1PinchPStar())/(CB*MW*SB2*SW) + (0.0625D0*CA3*EL*m12squared*SA1*SA22*dAlpha1PinchPStar())/(CB*MW*SB2*SW) &
  &- (1.6875D0*CA12*CA3*EL*m12squared*SA1*SA22*dAlpha1PinchPStar())/(CB*MW*SB2*SW) - (0.1875D0*CA1*EL*m12squared*SA2*SA3*dAlpha1P&
  &inchPStar())/(CB*MW*SB2*SW) - (0.5625D0*CA1*CA22*EL*m12squared*SA2*SA3*dAlpha1PinchPStar())/(CB*MW*SB2*SW) - (0.84375D0*CA1*EL&
  &*m12squared*SA12*SA2*SA3*dAlpha1PinchPStar())/(CB*MW*SB2*SW) - (2.53125D0*CA1*CA22*EL*m12squared*SA12*SA2*SA3*dAlpha1PinchPSta&
  &r())/(CB*MW*SB2*SW) - (0.125D0*CA3*EL*m12squared*SA1*dAlpha1PinchPStar())/(MW*SB*SW*TB) - (0.125D0*CA22*CA3*EL*m12squared*SA1*&
  &dAlpha1PinchPStar())/(MW*SB*SW*TB) + (0.125D0*CA3*EL*m12squared*SA1*SA22*dAlpha1PinchPStar())/(MW*SB*SW*TB) - (0.09375D0*CA1*E&
  &L*m12squared*SA2*SA3*dAlpha1PinchPStar())/(MW*SB*SW*TB) - (0.28125D0*CA1*CA22*EL*m12squared*SA2*SA3*dAlpha1PinchPStar())/(MW*S&
  &B*SW*TB) - (0.03125D0*CA1*CA3*EL*m12squared*TB*dAlpha1PinchPStar())/(CB*MW*SW) - (0.03125D0*CA1*CA22*CA3*EL*m12squared*TB*dAlp&
  &ha1PinchPStar())/(CB*MW*SW) + (0.03125D0*CA1*CA3*EL*m12squared*SA22*TB*dAlpha1PinchPStar())/(CB*MW*SW) + (0.09375D0*EL*m12squa&
  &red*SA1*SA2*SA3*TB*dAlpha1PinchPStar())/(CB*MW*SW) + (0.28125D0*CA22*EL*m12squared*SA1*SA2*SA3*TB*dAlpha1PinchPStar())/(CB*MW*&
  &SW) + (0.5D0*CA1*CA2*CA3*EL*m12squared*SA2*dAlpha2PinchPStar())/(CB*MW*SW) - (0.5D0*CA2*CA3*EL*MH12*SA1*SA2*dAlpha2PinchPStar(&
  &))/(CB*MW*SW) - (1.5D0*CA12*CA2*CA3*EL*MH12*SA1*SA2*dAlpha2PinchPStar())/(CB*MW*SW) - (0.25D0*CA2*CA3*EL*MH22*SA1*SA2*dAlpha2P&
  &inchPStar())/(CB*MW*SW) - (0.75D0*CA12*CA2*CA3*EL*MH22*SA1*SA2*dAlpha2PinchPStar())/(CB*MW*SW) + (0.1875D0*CA1*CA2*EL*MH12*SA3&
  &*dAlpha2PinchPStar())/(CB*MW*SW) + (0.09375D0*CA1*CA2*EL*MH22*SA3*dAlpha2PinchPStar())/(CB*MW*SW) + (0.28125D0*CA2*EL*m12squar&
  &ed*SA1*SA3*dAlpha2PinchPStar())/(CB*MW*SW) - (0.1875D0*CA1*CA2*EL*MH12*SA12*SA3*dAlpha2PinchPStar())/(CB*MW*SW) - (0.09375D0*C&
  &A1*CA2*EL*MH22*SA12*SA3*dAlpha2PinchPStar())/(CB*MW*SW) - (1.6875D0*CA1*CA2*EL*MH12*SA22*SA3*dAlpha2PinchPStar())/(CB*MW*SW) -&
  & (0.84375D0*CA1*CA2*EL*MH22*SA22*SA3*dAlpha2PinchPStar())/(CB*MW*SW) - (2.53125D0*CA2*EL*m12squared*SA1*SA22*SA3*dAlpha2PinchP&
  &Star())/(CB*MW*SW) + (1.6875D0*CA1*CA2*EL*MH12*SA12*SA22*SA3*dAlpha2PinchPStar())/(CB*MW*SW) + (0.84375D0*CA1*CA2*EL*MH22*SA12&
  &*SA22*SA3*dAlpha2PinchPStar())/(CB*MW*SW) + (0.5D0*CA1*CA2*CA3*EL*MH12*SA2*dAlpha2PinchPStar())/(MW*SB*SW) + (0.25D0*CA1*CA2*C&
  &A3*EL*MH22*SA2*dAlpha2PinchPStar())/(MW*SB*SW) - (0.875D0*CA2*CA3*EL*m12squared*SA1*SA2*dAlpha2PinchPStar())/(MW*SB*SW) + (0.6&
  &25D0*CA2*CA3*EL*m12squared*SA1*SA2*dAlpha2PinchPStar())/(CB2*MW*SB*SW) + (2.25D0*CA12*CA2*CA3*EL*m12squared*SA1*SA2*dAlpha2Pin&
  &chPStar())/(CB2*MW*SB*SW) + (1.5D0*CA1*CA2*CA3*EL*MH12*SA12*SA2*dAlpha2PinchPStar())/(MW*SB*SW) + (0.75D0*CA1*CA2*CA3*EL*MH22*&
  &SA12*SA2*dAlpha2PinchPStar())/(MW*SB*SW) + (0.28125D0*CA1*CA2*EL*m12squared*SA3*dAlpha2PinchPStar())/(MW*SB*SW) - (0.1875D0*CA&
  &1*CA2*EL*m12squared*SA3*dAlpha2PinchPStar())/(CB2*MW*SB*SW) + (0.1875D0*CA2*EL*MH12*SA1*SA3*dAlpha2PinchPStar())/(MW*SB*SW) - &
  &(0.1875D0*CA12*CA2*EL*MH12*SA1*SA3*dAlpha2PinchPStar())/(MW*SB*SW) + (0.09375D0*CA2*EL*MH22*SA1*SA3*dAlpha2PinchPStar())/(MW*S&
  &B*SW) - (0.09375D0*CA12*CA2*EL*MH22*SA1*SA3*dAlpha2PinchPStar())/(MW*SB*SW) + (0.28125D0*CA1*CA2*EL*m12squared*SA12*SA3*dAlpha&
  &2PinchPStar())/(CB2*MW*SB*SW) - (2.53125D0*CA1*CA2*EL*m12squared*SA22*SA3*dAlpha2PinchPStar())/(MW*SB*SW) + (1.6875D0*CA1*CA2*&
  &EL*m12squared*SA22*SA3*dAlpha2PinchPStar())/(CB2*MW*SB*SW) - (1.6875D0*CA2*EL*MH12*SA1*SA22*SA3*dAlpha2PinchPStar())/(MW*SB*SW&
  &) + (1.6875D0*CA12*CA2*EL*MH12*SA1*SA22*SA3*dAlpha2PinchPStar())/(MW*SB*SW) - (0.84375D0*CA2*EL*MH22*SA1*SA22*SA3*dAlpha2Pinch&
  &PStar())/(MW*SB*SW) + (0.84375D0*CA12*CA2*EL*MH22*SA1*SA22*SA3*dAlpha2PinchPStar())/(MW*SB*SW) - (2.53125D0*CA1*CA2*EL*m12squa&
  &red*SA12*SA22*SA3*dAlpha2PinchPStar())/(CB2*MW*SB*SW) - (0.25D0*CA1*CA2*CA3*EL*m12squared*SA2*dAlpha2PinchPStar())/(CB*MW*SB2*&
  &SW) - (2.25D0*CA1*CA2*CA3*EL*m12squared*SA12*SA2*dAlpha2PinchPStar())/(CB*MW*SB2*SW) - (0.1875D0*CA2*EL*m12squared*SA1*SA3*dAl&
  &pha2PinchPStar())/(CB*MW*SB2*SW) + (0.28125D0*CA12*CA2*EL*m12squared*SA1*SA3*dAlpha2PinchPStar())/(CB*MW*SB2*SW) + (1.6875D0*C&
  &A2*EL*m12squared*SA1*SA22*SA3*dAlpha2PinchPStar())/(CB*MW*SB2*SW) - (2.53125D0*CA12*CA2*EL*m12squared*SA1*SA22*SA3*dAlpha2Pinc&
  &hPStar())/(CB*MW*SB2*SW) - (0.5D0*CA1*CA2*CA3*EL*m12squared*SA2*dAlpha2PinchPStar())/(MW*SB*SW*TB) - (0.09375D0*CA2*EL*m12squa&
  &red*SA1*SA3*dAlpha2PinchPStar())/(MW*SB*SW*TB) + (0.84375D0*CA2*EL*m12squared*SA1*SA22*SA3*dAlpha2PinchPStar())/(MW*SB*SW*TB) &
  &+ (0.125D0*CA2*CA3*EL*m12squared*SA1*SA2*TB*dAlpha2PinchPStar())/(CB*MW*SW) - (0.09375D0*CA1*CA2*EL*m12squared*SA3*TB*dAlpha2P&
  &inchPStar())/(CB*MW*SW) + (0.84375D0*CA1*CA2*EL*m12squared*SA22*SA3*TB*dAlpha2PinchPStar())/(CB*MW*SW) + (0.5D0*MH12*SA2*SA3*d&
  &Alpha2PinchPStar())/vS - (4.5D0*CA22*MH12*SA2*SA3*dAlpha2PinchPStar())/vS + (0.25D0*MH22*SA2*SA3*dAlpha2PinchPStar())/vS - (2.&
  &25D0*CA22*MH22*SA2*SA3*dAlpha2PinchPStar())/vS + (0.1875D0*CA1*CA3*EL*MH12*SA2*dAlpha3PinchPStar())/(CB*MW*SW) + (0.5625D0*CA1&
  &*CA22*CA3*EL*MH12*SA2*dAlpha3PinchPStar())/(CB*MW*SW) + (0.09375D0*CA1*CA3*EL*MH22*SA2*dAlpha3PinchPStar())/(CB*MW*SW) + (0.28&
  &125D0*CA1*CA22*CA3*EL*MH22*SA2*dAlpha3PinchPStar())/(CB*MW*SW) + (0.28125D0*CA3*EL*m12squared*SA1*SA2*dAlpha3PinchPStar())/(CB&
  &*MW*SW) + (0.84375D0*CA22*CA3*EL*m12squared*SA1*SA2*dAlpha3PinchPStar())/(CB*MW*SW) - (0.1875D0*CA1*CA3*EL*MH12*SA12*SA2*dAlph&
  &a3PinchPStar())/(CB*MW*SW) - (0.5625D0*CA1*CA22*CA3*EL*MH12*SA12*SA2*dAlpha3PinchPStar())/(CB*MW*SW) - (0.09375D0*CA1*CA3*EL*M&
  &H22*SA12*SA2*dAlpha3PinchPStar())/(CB*MW*SW) - (0.28125D0*CA1*CA22*CA3*EL*MH22*SA12*SA2*dAlpha3PinchPStar())/(CB*MW*SW) + (0.1&
  &25D0*CA1*EL*m12squared*SA3*dAlpha3PinchPStar())/(CB*MW*SW) + (0.125D0*CA1*CA22*EL*m12squared*SA3*dAlpha3PinchPStar())/(CB*MW*S&
  &W) - (0.125D0*EL*MH12*SA1*SA3*dAlpha3PinchPStar())/(CB*MW*SW) - (0.375D0*CA12*EL*MH12*SA1*SA3*dAlpha3PinchPStar())/(CB*MW*SW) &
  &- (0.125D0*CA22*EL*MH12*SA1*SA3*dAlpha3PinchPStar())/(CB*MW*SW) - (0.375D0*CA12*CA22*EL*MH12*SA1*SA3*dAlpha3PinchPStar())/(CB*&
  &MW*SW) - (0.0625D0*EL*MH22*SA1*SA3*dAlpha3PinchPStar())/(CB*MW*SW) - (0.1875D0*CA12*EL*MH22*SA1*SA3*dAlpha3PinchPStar())/(CB*M&
  &W*SW) - (0.0625D0*CA22*EL*MH22*SA1*SA3*dAlpha3PinchPStar())/(CB*MW*SW) - (0.1875D0*CA12*CA22*EL*MH22*SA1*SA3*dAlpha3PinchPStar&
  &())/(CB*MW*SW) - (0.125D0*CA1*EL*m12squared*SA22*SA3*dAlpha3PinchPStar())/(CB*MW*SW) + (0.125D0*EL*MH12*SA1*SA22*SA3*dAlpha3Pi&
  &nchPStar())/(CB*MW*SW) + (0.375D0*CA12*EL*MH12*SA1*SA22*SA3*dAlpha3PinchPStar())/(CB*MW*SW) + (0.0625D0*EL*MH22*SA1*SA22*SA3*d&
  &Alpha3PinchPStar())/(CB*MW*SW) + (0.1875D0*CA12*EL*MH22*SA1*SA22*SA3*dAlpha3PinchPStar())/(CB*MW*SW) + (0.28125D0*CA1*CA3*EL*m&
  &12squared*SA2*dAlpha3PinchPStar())/(MW*SB*SW) + (0.84375D0*CA1*CA22*CA3*EL*m12squared*SA2*dAlpha3PinchPStar())/(MW*SB*SW) - (0&
  &.1875D0*CA1*CA3*EL*m12squared*SA2*dAlpha3PinchPStar())/(CB2*MW*SB*SW) - (0.5625D0*CA1*CA22*CA3*EL*m12squared*SA2*dAlpha3PinchP&
  &Star())/(CB2*MW*SB*SW) + (0.1875D0*CA3*EL*MH12*SA1*SA2*dAlpha3PinchPStar())/(MW*SB*SW) - (0.1875D0*CA12*CA3*EL*MH12*SA1*SA2*dA&
  &lpha3PinchPStar())/(MW*SB*SW) + (0.5625D0*CA22*CA3*EL*MH12*SA1*SA2*dAlpha3PinchPStar())/(MW*SB*SW) - (0.5625D0*CA12*CA22*CA3*E&
  &L*MH12*SA1*SA2*dAlpha3PinchPStar())/(MW*SB*SW) + (0.09375D0*CA3*EL*MH22*SA1*SA2*dAlpha3PinchPStar())/(MW*SB*SW) - (0.09375D0*C&
  &A12*CA3*EL*MH22*SA1*SA2*dAlpha3PinchPStar())/(MW*SB*SW) + (0.28125D0*CA22*CA3*EL*MH22*SA1*SA2*dAlpha3PinchPStar())/(MW*SB*SW) &
  &- (0.28125D0*CA12*CA22*CA3*EL*MH22*SA1*SA2*dAlpha3PinchPStar())/(MW*SB*SW) + (0.28125D0*CA1*CA3*EL*m12squared*SA12*SA2*dAlpha3&
  &PinchPStar())/(CB2*MW*SB*SW) + (0.84375D0*CA1*CA22*CA3*EL*m12squared*SA12*SA2*dAlpha3PinchPStar())/(CB2*MW*SB*SW) + (0.125D0*C&
  &A1*EL*MH12*SA3*dAlpha3PinchPStar())/(MW*SB*SW) + (0.125D0*CA1*CA22*EL*MH12*SA3*dAlpha3PinchPStar())/(MW*SB*SW) + (0.0625D0*CA1&
  &*EL*MH22*SA3*dAlpha3PinchPStar())/(MW*SB*SW) + (0.0625D0*CA1*CA22*EL*MH22*SA3*dAlpha3PinchPStar())/(MW*SB*SW) - (0.21875D0*EL*&
  &m12squared*SA1*SA3*dAlpha3PinchPStar())/(MW*SB*SW) - (0.21875D0*CA22*EL*m12squared*SA1*SA3*dAlpha3PinchPStar())/(MW*SB*SW) + (&
  &0.15625D0*EL*m12squared*SA1*SA3*dAlpha3PinchPStar())/(CB2*MW*SB*SW) + (0.5625D0*CA12*EL*m12squared*SA1*SA3*dAlpha3PinchPStar()&
  &)/(CB2*MW*SB*SW) + (0.15625D0*CA22*EL*m12squared*SA1*SA3*dAlpha3PinchPStar())/(CB2*MW*SB*SW) + (0.5625D0*CA12*CA22*EL*m12squar&
  &ed*SA1*SA3*dAlpha3PinchPStar())/(CB2*MW*SB*SW) + (0.375D0*CA1*EL*MH12*SA12*SA3*dAlpha3PinchPStar())/(MW*SB*SW) + (0.375D0*CA1*&
  &CA22*EL*MH12*SA12*SA3*dAlpha3PinchPStar())/(MW*SB*SW) + (0.1875D0*CA1*EL*MH22*SA12*SA3*dAlpha3PinchPStar())/(MW*SB*SW) + (0.18&
  &75D0*CA1*CA22*EL*MH22*SA12*SA3*dAlpha3PinchPStar())/(MW*SB*SW) - (0.125D0*CA1*EL*MH12*SA22*SA3*dAlpha3PinchPStar())/(MW*SB*SW)&
  & - (0.0625D0*CA1*EL*MH22*SA22*SA3*dAlpha3PinchPStar())/(MW*SB*SW) + (0.21875D0*EL*m12squared*SA1*SA22*SA3*dAlpha3PinchPStar())&
  &/(MW*SB*SW) - (0.15625D0*EL*m12squared*SA1*SA22*SA3*dAlpha3PinchPStar())/(CB2*MW*SB*SW) - (0.5625D0*CA12*EL*m12squared*SA1*SA2&
  &2*SA3*dAlpha3PinchPStar())/(CB2*MW*SB*SW) - (0.375D0*CA1*EL*MH12*SA12*SA22*SA3*dAlpha3PinchPStar())/(MW*SB*SW) - (0.1875D0*CA1&
  &*EL*MH22*SA12*SA22*SA3*dAlpha3PinchPStar())/(MW*SB*SW) - (0.1875D0*CA3*EL*m12squared*SA1*SA2*dAlpha3PinchPStar())/(CB*MW*SB2*S&
  &W) + (0.28125D0*CA12*CA3*EL*m12squared*SA1*SA2*dAlpha3PinchPStar())/(CB*MW*SB2*SW) - (0.5625D0*CA22*CA3*EL*m12squared*SA1*SA2*&
  &dAlpha3PinchPStar())/(CB*MW*SB2*SW) + (0.84375D0*CA12*CA22*CA3*EL*m12squared*SA1*SA2*dAlpha3PinchPStar())/(CB*MW*SB2*SW) - (0.&
  &0625D0*CA1*EL*m12squared*SA3*dAlpha3PinchPStar())/(CB*MW*SB2*SW) - (0.0625D0*CA1*CA22*EL*m12squared*SA3*dAlpha3PinchPStar())/(&
  &CB*MW*SB2*SW) - (0.5625D0*CA1*EL*m12squared*SA12*SA3*dAlpha3PinchPStar())/(CB*MW*SB2*SW) - (0.5625D0*CA1*CA22*EL*m12squared*SA&
  &12*SA3*dAlpha3PinchPStar())/(CB*MW*SB2*SW) + (0.0625D0*CA1*EL*m12squared*SA22*SA3*dAlpha3PinchPStar())/(CB*MW*SB2*SW) + (0.562&
  &5D0*CA1*EL*m12squared*SA12*SA22*SA3*dAlpha3PinchPStar())/(CB*MW*SB2*SW) - (0.09375D0*CA3*EL*m12squared*SA1*SA2*dAlpha3PinchPSt&
  &ar())/(MW*SB*SW*TB) - (0.28125D0*CA22*CA3*EL*m12squared*SA1*SA2*dAlpha3PinchPStar())/(MW*SB*SW*TB) - (0.125D0*CA1*EL*m12square&
  &d*SA3*dAlpha3PinchPStar())/(MW*SB*SW*TB) - (0.125D0*CA1*CA22*EL*m12squared*SA3*dAlpha3PinchPStar())/(MW*SB*SW*TB) + (0.125D0*C&
  &A1*EL*m12squared*SA22*SA3*dAlpha3PinchPStar())/(MW*SB*SW*TB) - (0.09375D0*CA1*CA3*EL*m12squared*SA2*TB*dAlpha3PinchPStar())/(C&
  &B*MW*SW) - (0.28125D0*CA1*CA22*CA3*EL*m12squared*SA2*TB*dAlpha3PinchPStar())/(CB*MW*SW) + (0.03125D0*EL*m12squared*SA1*SA3*TB*&
  &dAlpha3PinchPStar())/(CB*MW*SW) + (0.03125D0*CA22*EL*m12squared*SA1*SA3*TB*dAlpha3PinchPStar())/(CB*MW*SW) - (0.03125D0*EL*m12&
  &squared*SA1*SA22*SA3*TB*dAlpha3PinchPStar())/(CB*MW*SW) - (0.5D0*CA2*CA3*MH12*dAlpha3PinchPStar())/vS - (0.25D0*CA2*CA3*MH22*d&
  &Alpha3PinchPStar())/vS - (1.5D0*CA2*CA3*MH12*SA22*dAlpha3PinchPStar())/vS - (0.75D0*CA2*CA3*MH22*SA22*dAlpha3PinchPStar())/vS &
  &+ (0.015625D0*CA3*EL*m12squared*SA1*dBeta2PinchPStar())/(CB*MW*SW) + (0.015625D0*CA22*CA3*EL*m12squared*SA1*dBeta2PinchPStar()&
  &)/(CB*MW*SW) - (0.015625D0*CA3*EL*m12squared*SA1*SA22*dBeta2PinchPStar())/(CB*MW*SW) + (0.09375D0*CA1*EL*m12squared*SA2*SA3*dB&
  &eta2PinchPStar())/(CB*MW*SW) + (0.28125D0*CA1*CA22*EL*m12squared*SA2*SA3*dBeta2PinchPStar())/(CB*MW*SW) - (0.0625D0*CA1*CA3*EL&
  &*m12squared*dBeta2PinchPStar())/(MW*SB*SW) - (0.0625D0*CA1*CA22*CA3*EL*m12squared*dBeta2PinchPStar())/(MW*SB*SW) + (0.0625D0*C&
  &A1*CA3*EL*m12squared*dBeta2PinchPStar())/(CB2*MW*SB*SW) + (0.0625D0*CA1*CA22*CA3*EL*m12squared*dBeta2PinchPStar())/(CB2*MW*SB*&
  &SW) - (0.0625D0*CA3*EL*MH12*SA1*dBeta2PinchPStar())/(MW*SB*SW) - (0.1875D0*CA12*CA3*EL*MH12*SA1*dBeta2PinchPStar())/(MW*SB*SW)&
  & - (0.0625D0*CA22*CA3*EL*MH12*SA1*dBeta2PinchPStar())/(MW*SB*SW) - (0.1875D0*CA12*CA22*CA3*EL*MH12*SA1*dBeta2PinchPStar())/(MW&
  &*SB*SW) + (0.0625D0*CA3*EL*MH12*SA1*dBeta2PinchPStar())/(CB2*MW*SB*SW) + (0.1875D0*CA12*CA3*EL*MH12*SA1*dBeta2PinchPStar())/(C&
  &B2*MW*SB*SW) + (0.0625D0*CA22*CA3*EL*MH12*SA1*dBeta2PinchPStar())/(CB2*MW*SB*SW) + (0.1875D0*CA12*CA22*CA3*EL*MH12*SA1*dBeta2P&
  &inchPStar())/(CB2*MW*SB*SW) - (0.03125D0*CA3*EL*MH22*SA1*dBeta2PinchPStar())/(MW*SB*SW) - (0.09375D0*CA12*CA3*EL*MH22*SA1*dBet&
  &a2PinchPStar())/(MW*SB*SW) - (0.03125D0*CA22*CA3*EL*MH22*SA1*dBeta2PinchPStar())/(MW*SB*SW) - (0.09375D0*CA12*CA22*CA3*EL*MH22&
  &*SA1*dBeta2PinchPStar())/(MW*SB*SW) + (0.03125D0*CA3*EL*MH22*SA1*dBeta2PinchPStar())/(CB2*MW*SB*SW) + (0.09375D0*CA12*CA3*EL*M&
  &H22*SA1*dBeta2PinchPStar())/(CB2*MW*SB*SW) + (0.03125D0*CA22*CA3*EL*MH22*SA1*dBeta2PinchPStar())/(CB2*MW*SB*SW) + (0.09375D0*C&
  &A12*CA22*CA3*EL*MH22*SA1*dBeta2PinchPStar())/(CB2*MW*SB*SW) + (0.5625D0*CA1*CA3*EL*m12squared*SA12*dBeta2PinchPStar())/(CB2*MW&
  &*SB*SW) + (0.5625D0*CA1*CA22*CA3*EL*m12squared*SA12*dBeta2PinchPStar())/(CB2*MW*SB*SW) + (0.0625D0*CA1*CA3*EL*m12squared*SA22*&
  &dBeta2PinchPStar())/(MW*SB*SW) - (0.0625D0*CA1*CA3*EL*m12squared*SA22*dBeta2PinchPStar())/(CB2*MW*SB*SW) + (0.0625D0*CA3*EL*MH&
  &12*SA1*SA22*dBeta2PinchPStar())/(MW*SB*SW) + (0.1875D0*CA12*CA3*EL*MH12*SA1*SA22*dBeta2PinchPStar())/(MW*SB*SW) - (0.0625D0*CA&
  &3*EL*MH12*SA1*SA22*dBeta2PinchPStar())/(CB2*MW*SB*SW) - (0.1875D0*CA12*CA3*EL*MH12*SA1*SA22*dBeta2PinchPStar())/(CB2*MW*SB*SW)&
  & + (0.03125D0*CA3*EL*MH22*SA1*SA22*dBeta2PinchPStar())/(MW*SB*SW) + (0.09375D0*CA12*CA3*EL*MH22*SA1*SA22*dBeta2PinchPStar())/(&
  &MW*SB*SW) - (0.03125D0*CA3*EL*MH22*SA1*SA22*dBeta2PinchPStar())/(CB2*MW*SB*SW) - (0.09375D0*CA12*CA3*EL*MH22*SA1*SA22*dBeta2Pi&
  &nchPStar())/(CB2*MW*SB*SW) - (0.5625D0*CA1*CA3*EL*m12squared*SA12*SA22*dBeta2PinchPStar())/(CB2*MW*SB*SW) - (0.09375D0*CA1*EL*&
  &MH12*SA2*SA3*dBeta2PinchPStar())/(MW*SB*SW) - (0.28125D0*CA1*CA22*EL*MH12*SA2*SA3*dBeta2PinchPStar())/(MW*SB*SW) + (0.09375D0*&
  &CA1*EL*MH12*SA2*SA3*dBeta2PinchPStar())/(CB2*MW*SB*SW) + (0.28125D0*CA1*CA22*EL*MH12*SA2*SA3*dBeta2PinchPStar())/(CB2*MW*SB*SW&
  &) - (0.046875D0*CA1*EL*MH22*SA2*SA3*dBeta2PinchPStar())/(MW*SB*SW) - (0.140625D0*CA1*CA22*EL*MH22*SA2*SA3*dBeta2PinchPStar())/&
  &(MW*SB*SW) + (0.046875D0*CA1*EL*MH22*SA2*SA3*dBeta2PinchPStar())/(CB2*MW*SB*SW) + (0.140625D0*CA1*CA22*EL*MH22*SA2*SA3*dBeta2P&
  &inchPStar())/(CB2*MW*SB*SW) - (0.140625D0*EL*m12squared*SA1*SA2*SA3*dBeta2PinchPStar())/(MW*SB*SW) - (0.421875D0*CA22*EL*m12sq&
  &uared*SA1*SA2*SA3*dBeta2PinchPStar())/(MW*SB*SW) - (0.046875D0*EL*m12squared*SA1*SA2*SA3*dBeta2PinchPStar())/(CB2*MW*SB*SW) + &
  &(0.28125D0*CA12*EL*m12squared*SA1*SA2*SA3*dBeta2PinchPStar())/(CB2*MW*SB*SW) - (0.140625D0*CA22*EL*m12squared*SA1*SA2*SA3*dBet&
  &a2PinchPStar())/(CB2*MW*SB*SW) + (0.84375D0*CA12*CA22*EL*m12squared*SA1*SA2*SA3*dBeta2PinchPStar())/(CB2*MW*SB*SW) + (0.09375D&
  &0*CA1*EL*MH12*SA12*SA2*SA3*dBeta2PinchPStar())/(MW*SB*SW) + (0.28125D0*CA1*CA22*EL*MH12*SA12*SA2*SA3*dBeta2PinchPStar())/(MW*S&
  &B*SW) - (0.09375D0*CA1*EL*MH12*SA12*SA2*SA3*dBeta2PinchPStar())/(CB2*MW*SB*SW) - (0.28125D0*CA1*CA22*EL*MH12*SA12*SA2*SA3*dBet&
  &a2PinchPStar())/(CB2*MW*SB*SW) + (0.046875D0*CA1*EL*MH22*SA12*SA2*SA3*dBeta2PinchPStar())/(MW*SB*SW) + (0.140625D0*CA1*CA22*EL&
  &*MH22*SA12*SA2*SA3*dBeta2PinchPStar())/(MW*SB*SW) - (0.046875D0*CA1*EL*MH22*SA12*SA2*SA3*dBeta2PinchPStar())/(CB2*MW*SB*SW) - &
  &(0.140625D0*CA1*CA22*EL*MH22*SA12*SA2*SA3*dBeta2PinchPStar())/(CB2*MW*SB*SW) + (0.203125D0*CA3*EL*m12squared*SA1*dBeta2PinchPS&
  &tar())/(CB*MW*SB2*SW) + (0.84375D0*CA12*CA3*EL*m12squared*SA1*dBeta2PinchPStar())/(CB*MW*SB2*SW) + (0.203125D0*CA22*CA3*EL*m12&
  &squared*SA1*dBeta2PinchPStar())/(CB*MW*SB2*SW) + (0.84375D0*CA12*CA22*CA3*EL*m12squared*SA1*dBeta2PinchPStar())/(CB*MW*SB2*SW)&
  & - (0.203125D0*CA3*EL*m12squared*SA1*SA22*dBeta2PinchPStar())/(CB*MW*SB2*SW) - (0.84375D0*CA12*CA3*EL*m12squared*SA1*SA22*dBet&
  &a2PinchPStar())/(CB*MW*SB2*SW) + (0.10546875D0*CA1*EL*m12squared*SA2*SA3*dBeta2PinchPStar())/(CB*MW*SB2*SW) + (0.31640625D0*CA&
  &1*CA22*EL*m12squared*SA2*SA3*dBeta2PinchPStar())/(CB*MW*SB2*SW) - (0.38671875D0*CA1*EL*m12squared*SA12*SA2*SA3*dBeta2PinchPSta&
  &r())/(CB*MW*SB2*SW) - (1.16015625D0*CA1*CA22*EL*m12squared*SA12*SA2*SA3*dBeta2PinchPStar())/(CB*MW*SB2*SW) + (0.125D0*CA1*CA3*&
  &EL*MH12*dBeta2PinchPStar())/(MW*SB*SW*TB) + (0.125D0*CA1*CA22*CA3*EL*MH12*dBeta2PinchPStar())/(MW*SB*SW*TB) + (0.0625D0*CA1*CA&
  &3*EL*MH22*dBeta2PinchPStar())/(MW*SB*SW*TB) + (0.0625D0*CA1*CA22*CA3*EL*MH22*dBeta2PinchPStar())/(MW*SB*SW*TB) - (0.203125D0*C&
  &A3*EL*m12squared*SA1*dBeta2PinchPStar())/(MW*SB*SW*TB) - (0.203125D0*CA22*CA3*EL*m12squared*SA1*dBeta2PinchPStar())/(MW*SB*SW*&
  &TB) + (0.375D0*CA1*CA3*EL*MH12*SA12*dBeta2PinchPStar())/(MW*SB*SW*TB) + (0.375D0*CA1*CA22*CA3*EL*MH12*SA12*dBeta2PinchPStar())&
  &/(MW*SB*SW*TB) + (0.1875D0*CA1*CA3*EL*MH22*SA12*dBeta2PinchPStar())/(MW*SB*SW*TB) + (0.1875D0*CA1*CA22*CA3*EL*MH22*SA12*dBeta2&
  &PinchPStar())/(MW*SB*SW*TB) - (0.125D0*CA1*CA3*EL*MH12*SA22*dBeta2PinchPStar())/(MW*SB*SW*TB) - (0.0625D0*CA1*CA3*EL*MH22*SA22&
  &*dBeta2PinchPStar())/(MW*SB*SW*TB) + (0.203125D0*CA3*EL*m12squared*SA1*SA22*dBeta2PinchPStar())/(MW*SB*SW*TB) - (0.375D0*CA1*C&
  &A3*EL*MH12*SA12*SA22*dBeta2PinchPStar())/(MW*SB*SW*TB) - (0.1875D0*CA1*CA3*EL*MH22*SA12*SA22*dBeta2PinchPStar())/(MW*SB*SW*TB)&
  & - (0.140625D0*CA1*EL*m12squared*SA2*SA3*dBeta2PinchPStar())/(MW*SB*SW*TB) - (0.421875D0*CA1*CA22*EL*m12squared*SA2*SA3*dBeta2&
  &PinchPStar())/(MW*SB*SW*TB) - (0.1875D0*EL*MH12*SA1*SA2*SA3*dBeta2PinchPStar())/(MW*SB*SW*TB) + (0.1875D0*CA12*EL*MH12*SA1*SA2&
  &*SA3*dBeta2PinchPStar())/(MW*SB*SW*TB) - (0.5625D0*CA22*EL*MH12*SA1*SA2*SA3*dBeta2PinchPStar())/(MW*SB*SW*TB) + (0.5625D0*CA12&
  &*CA22*EL*MH12*SA1*SA2*SA3*dBeta2PinchPStar())/(MW*SB*SW*TB) - (0.09375D0*EL*MH22*SA1*SA2*SA3*dBeta2PinchPStar())/(MW*SB*SW*TB)&
  & + (0.09375D0*CA12*EL*MH22*SA1*SA2*SA3*dBeta2PinchPStar())/(MW*SB*SW*TB) - (0.28125D0*CA22*EL*MH22*SA1*SA2*SA3*dBeta2PinchPSta&
  &r())/(MW*SB*SW*TB) + (0.28125D0*CA12*CA22*EL*MH22*SA1*SA2*SA3*dBeta2PinchPStar())/(MW*SB*SW*TB) - (0.125D0*CA1*CA3*EL*m12squar&
  &ed*TB*dBeta2PinchPStar())/(CB*MW*SW) - (0.125D0*CA1*CA22*CA3*EL*m12squared*TB*dBeta2PinchPStar())/(CB*MW*SW) + (0.0625D0*CA3*E&
  &L*MH12*SA1*TB*dBeta2PinchPStar())/(CB*MW*SW) + (0.1875D0*CA12*CA3*EL*MH12*SA1*TB*dBeta2PinchPStar())/(CB*MW*SW) + (0.0625D0*CA&
  &22*CA3*EL*MH12*SA1*TB*dBeta2PinchPStar())/(CB*MW*SW) + (0.1875D0*CA12*CA22*CA3*EL*MH12*SA1*TB*dBeta2PinchPStar())/(CB*MW*SW) +&
  & (0.03125D0*CA3*EL*MH22*SA1*TB*dBeta2PinchPStar())/(CB*MW*SW) + (0.09375D0*CA12*CA3*EL*MH22*SA1*TB*dBeta2PinchPStar())/(CB*MW*&
  &SW) + (0.03125D0*CA22*CA3*EL*MH22*SA1*TB*dBeta2PinchPStar())/(CB*MW*SW) + (0.09375D0*CA12*CA22*CA3*EL*MH22*SA1*TB*dBeta2PinchP&
  &Star())/(CB*MW*SW) + (0.125D0*CA1*CA3*EL*m12squared*SA22*TB*dBeta2PinchPStar())/(CB*MW*SW) - (0.0625D0*CA3*EL*MH12*SA1*SA22*TB&
  &*dBeta2PinchPStar())/(CB*MW*SW) - (0.1875D0*CA12*CA3*EL*MH12*SA1*SA22*TB*dBeta2PinchPStar())/(CB*MW*SW) - (0.03125D0*CA3*EL*MH&
  &22*SA1*SA22*TB*dBeta2PinchPStar())/(CB*MW*SW) - (0.09375D0*CA12*CA3*EL*MH22*SA1*SA22*TB*dBeta2PinchPStar())/(CB*MW*SW) + (0.09&
  &375D0*CA1*EL*MH12*SA2*SA3*TB*dBeta2PinchPStar())/(CB*MW*SW) + (0.28125D0*CA1*CA22*EL*MH12*SA2*SA3*TB*dBeta2PinchPStar())/(CB*M&
  &W*SW) + (0.046875D0*CA1*EL*MH22*SA2*SA3*TB*dBeta2PinchPStar())/(CB*MW*SW) + (0.140625D0*CA1*CA22*EL*MH22*SA2*SA3*TB*dBeta2Pinc&
  &hPStar())/(CB*MW*SW) + (0.140625D0*EL*m12squared*SA1*SA2*SA3*TB*dBeta2PinchPStar())/(CB*MW*SW) + (0.421875D0*CA22*EL*m12square&
  &d*SA1*SA2*SA3*TB*dBeta2PinchPStar())/(CB*MW*SW) - (0.09375D0*CA1*EL*MH12*SA12*SA2*SA3*TB*dBeta2PinchPStar())/(CB*MW*SW) - (0.2&
  &8125D0*CA1*CA22*EL*MH12*SA12*SA2*SA3*TB*dBeta2PinchPStar())/(CB*MW*SW) - (0.046875D0*CA1*EL*MH22*SA12*SA2*SA3*TB*dBeta2PinchPS&
  &tar())/(CB*MW*SW) - (0.140625D0*CA1*CA22*EL*MH22*SA12*SA2*SA3*TB*dBeta2PinchPStar())/(CB*MW*SW) - (0.1875D0*CA1*CA3*EL*m12squa&
  &red*dBeta2PinchPStar())/(MW*SB*SW*TB2) - (0.1875D0*CA1*CA22*CA3*EL*m12squared*dBeta2PinchPStar())/(MW*SB*SW*TB2) + (0.1875D0*C&
  &A1*CA3*EL*m12squared*SA22*dBeta2PinchPStar())/(MW*SB*SW*TB2) + (0.09375D0*EL*m12squared*SA1*SA2*SA3*dBeta2PinchPStar())/(MW*SB&
  &*SW*TB2) + (0.28125D0*CA22*EL*m12squared*SA1*SA2*SA3*dBeta2PinchPStar())/(MW*SB*SW*TB2) - (0.03125D0*CA3*EL*m12squared*SA1*TB2&
  &*dBeta2PinchPStar())/(CB*MW*SW) - (0.03125D0*CA22*CA3*EL*m12squared*SA1*TB2*dBeta2PinchPStar())/(CB*MW*SW) + (0.03125D0*CA3*EL&
  &*m12squared*SA1*SA22*TB2*dBeta2PinchPStar())/(CB*MW*SW) - (0.140625D0*CA1*EL*m12squared*SA2*SA3*TB2*dBeta2PinchPStar())/(CB*MW&
  &*SW) - (0.421875D0*CA1*CA22*EL*m12squared*SA2*SA3*TB2*dBeta2PinchPStar())/(CB*MW*SW) + (0.375D0*CA3*EL*MH12*dAlpha1PinchPStar(&
  &)*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.375D0*CA22*CA3*EL*MH12*dAlpha1PinchPStar()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.1875D&
  &0*CA3*EL*MH22*dAlpha1PinchPStar()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.1875D0*CA22*CA3*EL*MH22*dAlpha1PinchPStar()*DBLE(CA1**&
  &INT(3.D0)))/(CB*MW*SW) - (0.375D0*CA3*EL*MH12*SA22*dAlpha1PinchPStar()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.1875D0*CA3*EL*MH2&
  &2*SA22*dAlpha1PinchPStar()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.5625D0*CA3*EL*m12squared*dAlpha1PinchPStar()*DBLE(CA1**INT(3.&
  &D0)))/(CB2*MW*SB*SW) - (0.5625D0*CA22*CA3*EL*m12squared*dAlpha1PinchPStar()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) + (0.5625D0*C&
  &A3*EL*m12squared*SA22*dAlpha1PinchPStar()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.1875D0*EL*MH12*SA2*SA3*dAlpha1PinchPStar()&
  &*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.5625D0*CA22*EL*MH12*SA2*SA3*dAlpha1PinchPStar()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.0&
  &9375D0*EL*MH22*SA2*SA3*dAlpha1PinchPStar()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.28125D0*CA22*EL*MH22*SA2*SA3*dAlpha1PinchPSta&
  &r()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) + (0.28125D0*EL*m12squared*SA2*SA3*dAlpha1PinchPStar()*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*S&
  &W) + (0.84375D0*CA22*EL*m12squared*SA2*SA3*dAlpha1PinchPStar()*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.0625D0*CA2*EL*MH12*SA&
  &3*dAlpha2PinchPStar()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.03125D0*CA2*EL*MH22*SA3*dAlpha2PinchPStar()*DBLE(CA1**INT(3.D0)))/&
  &(CB*MW*SW) - (0.5625D0*CA2*EL*MH12*SA22*SA3*dAlpha2PinchPStar()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.28125D0*CA2*EL*MH22*SA22&
  &*SA3*dAlpha2PinchPStar()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.5D0*CA2*CA3*EL*MH12*SA2*dAlpha2PinchPStar()*DBLE(CA1**INT(3.D0)&
  &))/(MW*SB*SW) - (0.25D0*CA2*CA3*EL*MH22*SA2*dAlpha2PinchPStar()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.09375D0*CA2*EL*m12square&
  &d*SA3*dAlpha2PinchPStar()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) + (0.84375D0*CA2*EL*m12squared*SA22*SA3*dAlpha2PinchPStar()*DBL&
  &E(CA1**INT(3.D0)))/(CB2*MW*SB*SW) + (0.75D0*CA2*CA3*EL*m12squared*SA2*dAlpha2PinchPStar()*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW)&
  & + (0.0625D0*CA3*EL*MH12*SA2*dAlpha3PinchPStar()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.1875D0*CA22*CA3*EL*MH12*SA2*dAlpha3Pinc&
  &hPStar()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.03125D0*CA3*EL*MH22*SA2*dAlpha3PinchPStar()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + &
  &(0.09375D0*CA22*CA3*EL*MH22*SA2*dAlpha3PinchPStar()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.09375D0*CA3*EL*m12squared*SA2*dAlpha&
  &3PinchPStar()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.28125D0*CA22*CA3*EL*m12squared*SA2*dAlpha3PinchPStar()*DBLE(CA1**INT(3&
  &.D0)))/(CB2*MW*SB*SW) - (0.125D0*EL*MH12*SA3*dAlpha3PinchPStar()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.125D0*CA22*EL*MH12*SA3*&
  &dAlpha3PinchPStar()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.0625D0*EL*MH22*SA3*dAlpha3PinchPStar()*DBLE(CA1**INT(3.D0)))/(MW*SB*&
  &SW) - (0.0625D0*CA22*EL*MH22*SA3*dAlpha3PinchPStar()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) + (0.125D0*EL*MH12*SA22*SA3*dAlpha3Pinch&
  &PStar()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) + (0.0625D0*EL*MH22*SA22*SA3*dAlpha3PinchPStar()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) + (&
  &0.1875D0*EL*m12squared*SA3*dAlpha3PinchPStar()*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.1875D0*CA22*EL*m12squared*SA3*dAlpha3&
  &PinchPStar()*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.1875D0*EL*m12squared*SA22*SA3*dAlpha3PinchPStar()*DBLE(CA1**INT(3.D0)))&
  &/(CB*MW*SB2*SW) - (0.1875D0*CA3*EL*m12squared*dBeta2PinchPStar()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.1875D0*CA22*CA3*EL*&
  &m12squared*dBeta2PinchPStar()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) + (0.1875D0*CA3*EL*m12squared*SA22*dBeta2PinchPStar()*DBLE(&
  &CA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.03125D0*EL*MH12*SA2*SA3*dBeta2PinchPStar()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.09375D0*&
  &CA22*EL*MH12*SA2*SA3*dBeta2PinchPStar()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) + (0.03125D0*EL*MH12*SA2*SA3*dBeta2PinchPStar()*DBLE(&
  &CA1**INT(3.D0)))/(CB2*MW*SB*SW) + (0.09375D0*CA22*EL*MH12*SA2*SA3*dBeta2PinchPStar()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0&
  &.015625D0*EL*MH22*SA2*SA3*dBeta2PinchPStar()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.046875D0*CA22*EL*MH22*SA2*SA3*dBeta2PinchPS&
  &tar()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) + (0.015625D0*EL*MH22*SA2*SA3*dBeta2PinchPStar()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) +&
  & (0.046875D0*CA22*EL*MH22*SA2*SA3*dBeta2PinchPStar()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) + (0.12890625D0*EL*m12squared*SA2*SA&
  &3*dBeta2PinchPStar()*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.38671875D0*CA22*EL*m12squared*SA2*SA3*dBeta2PinchPStar()*DBLE(C&
  &A1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.125D0*CA3*EL*MH12*dBeta2PinchPStar()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW*TB) - (0.125D0*CA22*C&
  &A3*EL*MH12*dBeta2PinchPStar()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW*TB) - (0.0625D0*CA3*EL*MH22*dBeta2PinchPStar()*DBLE(CA1**INT(3.D&
  &0)))/(MW*SB*SW*TB) - (0.0625D0*CA22*CA3*EL*MH22*dBeta2PinchPStar()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW*TB) + (0.125D0*CA3*EL*MH12*&
  &SA22*dBeta2PinchPStar()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW*TB) + (0.0625D0*CA3*EL*MH22*SA22*dBeta2PinchPStar()*DBLE(CA1**INT(3.D0&
  &)))/(MW*SB*SW*TB) + (0.03125D0*EL*MH12*SA2*SA3*TB*dBeta2PinchPStar()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.09375D0*CA22*EL*MH1&
  &2*SA2*SA3*TB*dBeta2PinchPStar()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.015625D0*EL*MH22*SA2*SA3*TB*dBeta2PinchPStar()*DBLE(CA1*&
  &*INT(3.D0)))/(CB*MW*SW) + (0.046875D0*CA22*EL*MH22*SA2*SA3*TB*dBeta2PinchPStar()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.5625D0*&
  &CA1*EL*MH12*SA3*dAlpha2PinchPStar()*DBLE(CA2**INT(3.D0)))/(CB*MW*SW) + (0.28125D0*CA1*EL*MH22*SA3*dAlpha2PinchPStar()*DBLE(CA2&
  &**INT(3.D0)))/(CB*MW*SW) + (0.84375D0*EL*m12squared*SA1*SA3*dAlpha2PinchPStar()*DBLE(CA2**INT(3.D0)))/(CB*MW*SW) - (0.5625D0*C&
  &A1*EL*MH12*SA12*SA3*dAlpha2PinchPStar()*DBLE(CA2**INT(3.D0)))/(CB*MW*SW) - (0.28125D0*CA1*EL*MH22*SA12*SA3*dAlpha2PinchPStar()&
  &*DBLE(CA2**INT(3.D0)))/(CB*MW*SW) + (0.84375D0*CA1*EL*m12squared*SA3*dAlpha2PinchPStar()*DBLE(CA2**INT(3.D0)))/(MW*SB*SW) - (0&
  &.5625D0*CA1*EL*m12squared*SA3*dAlpha2PinchPStar()*DBLE(CA2**INT(3.D0)))/(CB2*MW*SB*SW) + (0.5625D0*EL*MH12*SA1*SA3*dAlpha2Pinc&
  &hPStar()*DBLE(CA2**INT(3.D0)))/(MW*SB*SW) - (0.5625D0*CA12*EL*MH12*SA1*SA3*dAlpha2PinchPStar()*DBLE(CA2**INT(3.D0)))/(MW*SB*SW&
  &) + (0.28125D0*EL*MH22*SA1*SA3*dAlpha2PinchPStar()*DBLE(CA2**INT(3.D0)))/(MW*SB*SW) - (0.28125D0*CA12*EL*MH22*SA1*SA3*dAlpha2P&
  &inchPStar()*DBLE(CA2**INT(3.D0)))/(MW*SB*SW) + (0.84375D0*CA1*EL*m12squared*SA12*SA3*dAlpha2PinchPStar()*DBLE(CA2**INT(3.D0)))&
  &/(CB2*MW*SB*SW) - (0.5625D0*EL*m12squared*SA1*SA3*dAlpha2PinchPStar()*DBLE(CA2**INT(3.D0)))/(CB*MW*SB2*SW) + (0.84375D0*CA12*E&
  &L*m12squared*SA1*SA3*dAlpha2PinchPStar()*DBLE(CA2**INT(3.D0)))/(CB*MW*SB2*SW) - (0.28125D0*EL*m12squared*SA1*SA3*dAlpha2PinchP&
  &Star()*DBLE(CA2**INT(3.D0)))/(MW*SB*SW*TB) - (0.28125D0*CA1*EL*m12squared*SA3*TB*dAlpha2PinchPStar()*DBLE(CA2**INT(3.D0)))/(CB&
  &*MW*SW) + (0.5D0*CA3*MH12*dAlpha3PinchPStar()*DBLE(CA2**INT(3.D0)))/vS + (0.25D0*CA3*MH22*dAlpha3PinchPStar()*DBLE(CA2**INT(3.&
  &D0)))/vS + (0.1875D0*EL*MH12*SA3*dAlpha2PinchPStar()*DBLE(CA1**INT(3.D0))*DBLE(CA2**INT(3.D0)))/(CB*MW*SW) + (0.09375D0*EL*MH2&
  &2*SA3*dAlpha2PinchPStar()*DBLE(CA1**INT(3.D0))*DBLE(CA2**INT(3.D0)))/(CB*MW*SW) - (0.28125D0*EL*m12squared*SA3*dAlpha2PinchPSt&
  &ar()*DBLE(CA1**INT(3.D0))*DBLE(CA2**INT(3.D0)))/(CB2*MW*SB*SW) - (0.28125D0*CA3*EL*m12squared*SA1*dBeta2PinchPStar()*DBLE(CB**&
  &INT(-3.D0)))/(MW*SW) - (0.84375D0*CA12*CA3*EL*m12squared*SA1*dBeta2PinchPStar()*DBLE(CB**INT(-3.D0)))/(MW*SW) - (0.28125D0*CA2&
  &2*CA3*EL*m12squared*SA1*dBeta2PinchPStar()*DBLE(CB**INT(-3.D0)))/(MW*SW) - (0.84375D0*CA12*CA22*CA3*EL*m12squared*SA1*dBeta2Pi&
  &nchPStar()*DBLE(CB**INT(-3.D0)))/(MW*SW) + (0.28125D0*CA3*EL*m12squared*SA1*SA22*dBeta2PinchPStar()*DBLE(CB**INT(-3.D0)))/(MW*&
  &SW) + (0.84375D0*CA12*CA3*EL*m12squared*SA1*SA22*dBeta2PinchPStar()*DBLE(CB**INT(-3.D0)))/(MW*SW) - (0.36328125D0*CA1*EL*m12sq&
  &uared*SA2*SA3*dBeta2PinchPStar()*DBLE(CB**INT(-3.D0)))/(MW*SW) - (1.08984375D0*CA1*CA22*EL*m12squared*SA2*SA3*dBeta2PinchPStar&
  &()*DBLE(CB**INT(-3.D0)))/(MW*SW) + (0.45703125D0*CA1*EL*m12squared*SA12*SA2*SA3*dBeta2PinchPStar()*DBLE(CB**INT(-3.D0)))/(MW*S&
  &W) + (1.37109375D0*CA1*CA22*EL*m12squared*SA12*SA2*SA3*dBeta2PinchPStar()*DBLE(CB**INT(-3.D0)))/(MW*SW) - (0.0625D0*CA3*EL*m12&
  &squared*SA1*dBeta2PinchPStar()*DBLE(CB**INT(-3.D0)))/(MW*SB2*SW) - (0.28125D0*CA12*CA3*EL*m12squared*SA1*dBeta2PinchPStar()*DB&
  &LE(CB**INT(-3.D0)))/(MW*SB2*SW) - (0.0625D0*CA22*CA3*EL*m12squared*SA1*dBeta2PinchPStar()*DBLE(CB**INT(-3.D0)))/(MW*SB2*SW) - &
  &(0.28125D0*CA12*CA22*CA3*EL*m12squared*SA1*dBeta2PinchPStar()*DBLE(CB**INT(-3.D0)))/(MW*SB2*SW) + (0.0625D0*CA3*EL*m12squared*&
  &SA1*SA22*dBeta2PinchPStar()*DBLE(CB**INT(-3.D0)))/(MW*SB2*SW) + (0.28125D0*CA12*CA3*EL*m12squared*SA1*SA22*dBeta2PinchPStar()*&
  &DBLE(CB**INT(-3.D0)))/(MW*SB2*SW) - (0.05859375D0*CA1*EL*m12squared*SA2*SA3*dBeta2PinchPStar()*DBLE(CB**INT(-3.D0)))/(MW*SB2*S&
  &W) - (0.17578125D0*CA1*CA22*EL*m12squared*SA2*SA3*dBeta2PinchPStar()*DBLE(CB**INT(-3.D0)))/(MW*SB2*SW) + (0.10546875D0*CA1*EL*&
  &m12squared*SA12*SA2*SA3*dBeta2PinchPStar()*DBLE(CB**INT(-3.D0)))/(MW*SB2*SW) + (0.31640625D0*CA1*CA22*EL*m12squared*SA12*SA2*S&
  &A3*dBeta2PinchPStar()*DBLE(CB**INT(-3.D0)))/(MW*SB2*SW) - (0.15234375D0*EL*m12squared*SA2*SA3*dBeta2PinchPStar()*DBLE(CA1**INT&
  &(3.D0))*DBLE(CB**INT(-3.D0)))/(MW*SW) - (0.45703125D0*CA22*EL*m12squared*SA2*SA3*dBeta2PinchPStar()*DBLE(CA1**INT(3.D0))*DBLE(&
  &CB**INT(-3.D0)))/(MW*SW) - (0.03515625D0*EL*m12squared*SA2*SA3*dBeta2PinchPStar()*DBLE(CA1**INT(3.D0))*DBLE(CB**INT(-3.D0)))/(&
  &MW*SB2*SW) - (0.10546875D0*CA22*EL*m12squared*SA2*SA3*dBeta2PinchPStar()*DBLE(CA1**INT(3.D0))*DBLE(CB**INT(-3.D0)))/(MW*SB2*SW&
  &) + (0.1875D0*EL*MH12*SA2*SA3*dAlpha1PinchPStar()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.5625D0*CA22*EL*MH12*SA2*SA3*dAlpha1Pin&
  &chPStar()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.09375D0*EL*MH22*SA2*SA3*dAlpha1PinchPStar()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) +&
  & (0.28125D0*CA22*EL*MH22*SA2*SA3*dAlpha1PinchPStar()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.375D0*CA3*EL*MH12*dAlpha1PinchPStar&
  &()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.375D0*CA22*CA3*EL*MH12*dAlpha1PinchPStar()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.1875&
  &D0*CA3*EL*MH22*dAlpha1PinchPStar()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.1875D0*CA22*CA3*EL*MH22*dAlpha1PinchPStar()*DBLE(SA1*&
  &*INT(3.D0)))/(MW*SB*SW) - (0.375D0*CA3*EL*MH12*SA22*dAlpha1PinchPStar()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0.1875D0*CA3*EL*MH&
  &22*SA22*dAlpha1PinchPStar()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0.28125D0*EL*m12squared*SA2*SA3*dAlpha1PinchPStar()*DBLE(SA1**&
  &INT(3.D0)))/(CB2*MW*SB*SW) - (0.84375D0*CA22*EL*m12squared*SA2*SA3*dAlpha1PinchPStar()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) - &
  &(0.5625D0*CA3*EL*m12squared*dAlpha1PinchPStar()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.5625D0*CA22*CA3*EL*m12squared*dAlpha&
  &1PinchPStar()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.5625D0*CA3*EL*m12squared*SA22*dAlpha1PinchPStar()*DBLE(SA1**INT(3.D0))&
  &)/(CB*MW*SB2*SW) + (0.5D0*CA2*CA3*EL*MH12*SA2*dAlpha2PinchPStar()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.25D0*CA2*CA3*EL*MH22*S&
  &A2*dAlpha2PinchPStar()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) - (0.75D0*CA2*CA3*EL*m12squared*SA2*dAlpha2PinchPStar()*DBLE(SA1**INT(&
  &3.D0)))/(CB2*MW*SB*SW) + (0.0625D0*CA2*EL*MH12*SA3*dAlpha2PinchPStar()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.03125D0*CA2*EL*MH&
  &22*SA3*dAlpha2PinchPStar()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0.5625D0*CA2*EL*MH12*SA22*SA3*dAlpha2PinchPStar()*DBLE(SA1**INT&
  &(3.D0)))/(MW*SB*SW) - (0.28125D0*CA2*EL*MH22*SA22*SA3*dAlpha2PinchPStar()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0.09375D0*CA2*EL&
  &*m12squared*SA3*dAlpha2PinchPStar()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.84375D0*CA2*EL*m12squared*SA22*SA3*dAlpha2PinchP&
  &Star()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.125D0*EL*MH12*SA3*dAlpha3PinchPStar()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.1&
  &25D0*CA22*EL*MH12*SA3*dAlpha3PinchPStar()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.0625D0*EL*MH22*SA3*dAlpha3PinchPStar()*DBLE(SA&
  &1**INT(3.D0)))/(CB*MW*SW) + (0.0625D0*CA22*EL*MH22*SA3*dAlpha3PinchPStar()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) - (0.125D0*EL*MH12&
  &*SA22*SA3*dAlpha3PinchPStar()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) - (0.0625D0*EL*MH22*SA22*SA3*dAlpha3PinchPStar()*DBLE(SA1**INT(&
  &3.D0)))/(CB*MW*SW) + (0.0625D0*CA3*EL*MH12*SA2*dAlpha3PinchPStar()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.1875D0*CA22*CA3*EL*MH&
  &12*SA2*dAlpha3PinchPStar()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.03125D0*CA3*EL*MH22*SA2*dAlpha3PinchPStar()*DBLE(SA1**INT(3.D&
  &0)))/(MW*SB*SW) + (0.09375D0*CA22*CA3*EL*MH22*SA2*dAlpha3PinchPStar()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0.1875D0*EL*m12squar&
  &ed*SA3*dAlpha3PinchPStar()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.1875D0*CA22*EL*m12squared*SA3*dAlpha3PinchPStar()*DBLE(SA&
  &1**INT(3.D0)))/(CB2*MW*SB*SW) + (0.1875D0*EL*m12squared*SA22*SA3*dAlpha3PinchPStar()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0&
  &.09375D0*CA3*EL*m12squared*SA2*dAlpha3PinchPStar()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.28125D0*CA22*CA3*EL*m12squared*SA&
  &2*dAlpha3PinchPStar()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.0625D0*CA3*EL*MH12*dBeta2PinchPStar()*DBLE(SA1**INT(3.D0)))/(M&
  &W*SB*SW) + (0.0625D0*CA22*CA3*EL*MH12*dBeta2PinchPStar()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0.0625D0*CA3*EL*MH12*dBeta2PinchP&
  &Star()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.0625D0*CA22*CA3*EL*MH12*dBeta2PinchPStar()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*S&
  &W) + (0.03125D0*CA3*EL*MH22*dBeta2PinchPStar()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.03125D0*CA22*CA3*EL*MH22*dBeta2PinchPStar&
  &()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0.03125D0*CA3*EL*MH22*dBeta2PinchPStar()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.0312&
  &5D0*CA22*CA3*EL*MH22*dBeta2PinchPStar()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.0625D0*CA3*EL*MH12*SA22*dBeta2PinchPStar()*D&
  &BLE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.0625D0*CA3*EL*MH12*SA22*dBeta2PinchPStar()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.0312&
  &5D0*CA3*EL*MH22*SA22*dBeta2PinchPStar()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.03125D0*CA3*EL*MH22*SA22*dBeta2PinchPStar()*DBLE&
  &(SA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.09375D0*EL*m12squared*SA2*SA3*dBeta2PinchPStar()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) - &
  &(0.28125D0*CA22*EL*m12squared*SA2*SA3*dBeta2PinchPStar()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.28125D0*CA3*EL*m12squared*d&
  &Beta2PinchPStar()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.28125D0*CA22*CA3*EL*m12squared*dBeta2PinchPStar()*DBLE(SA1**INT(3.&
  &D0)))/(CB*MW*SB2*SW) + (0.28125D0*CA3*EL*m12squared*SA22*dBeta2PinchPStar()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.0625D0*E&
  &L*MH12*SA2*SA3*dBeta2PinchPStar()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW*TB) - (0.1875D0*CA22*EL*MH12*SA2*SA3*dBeta2PinchPStar()*DBLE&
  &(SA1**INT(3.D0)))/(MW*SB*SW*TB) - (0.03125D0*EL*MH22*SA2*SA3*dBeta2PinchPStar()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW*TB) - (0.09375&
  &D0*CA22*EL*MH22*SA2*SA3*dBeta2PinchPStar()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW*TB) - (0.0625D0*CA3*EL*MH12*TB*dBeta2PinchPStar()*D&
  &BLE(SA1**INT(3.D0)))/(CB*MW*SW) - (0.0625D0*CA22*CA3*EL*MH12*TB*dBeta2PinchPStar()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) - (0.03125&
  &D0*CA3*EL*MH22*TB*dBeta2PinchPStar()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) - (0.03125D0*CA22*CA3*EL*MH22*TB*dBeta2PinchPStar()*DBLE&
  &(SA1**INT(3.D0)))/(CB*MW*SW) + (0.0625D0*CA3*EL*MH12*SA22*TB*dBeta2PinchPStar()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.03125D0*&
  &CA3*EL*MH22*SA22*TB*dBeta2PinchPStar()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.1875D0*EL*MH12*SA3*dAlpha2PinchPStar()*DBLE(CA2**&
  &INT(3.D0))*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.09375D0*EL*MH22*SA3*dAlpha2PinchPStar()*DBLE(CA2**INT(3.D0))*DBLE(SA1**INT(3.&
  &D0)))/(MW*SB*SW) - (0.28125D0*EL*m12squared*SA3*dAlpha2PinchPStar()*DBLE(CA2**INT(3.D0))*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) &
  &+ (0.28125D0*CA3*EL*m12squared*dBeta2PinchPStar()*DBLE(CB**INT(-3.D0))*DBLE(SA1**INT(3.D0)))/(MW*SW) + (0.28125D0*CA22*CA3*EL*&
  &m12squared*dBeta2PinchPStar()*DBLE(CB**INT(-3.D0))*DBLE(SA1**INT(3.D0)))/(MW*SW) - (0.28125D0*CA3*EL*m12squared*SA22*dBeta2Pin&
  &chPStar()*DBLE(CB**INT(-3.D0))*DBLE(SA1**INT(3.D0)))/(MW*SW) + (0.09375D0*CA3*EL*m12squared*dBeta2PinchPStar()*DBLE(CB**INT(-3&
  &.D0))*DBLE(SA1**INT(3.D0)))/(MW*SB2*SW) + (0.09375D0*CA22*CA3*EL*m12squared*dBeta2PinchPStar()*DBLE(CB**INT(-3.D0))*DBLE(SA1**&
  &INT(3.D0)))/(MW*SB2*SW) - (0.09375D0*CA3*EL*m12squared*SA22*dBeta2PinchPStar()*DBLE(CB**INT(-3.D0))*DBLE(SA1**INT(3.D0)))/(MW*&
  &SB2*SW) - (0.28125D0*CA1*EL*m12squared*SA3*dAlpha1PinchPStar()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.1875D0*EL*MH12*SA1*SA3*dA&
  &lpha1PinchPStar()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.5625D0*CA12*EL*MH12*SA1*SA3*dAlpha1PinchPStar()*DBLE(SA2**INT(3.D0)))/&
  &(CB*MW*SW) + (0.09375D0*EL*MH22*SA1*SA3*dAlpha1PinchPStar()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.28125D0*CA12*EL*MH22*SA1*SA3&
  &*dAlpha1PinchPStar()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.1875D0*CA1*EL*MH12*SA3*dAlpha1PinchPStar()*DBLE(SA2**INT(3.D0)))/(M&
  &W*SB*SW) - (0.09375D0*CA1*EL*MH22*SA3*dAlpha1PinchPStar()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) + (0.28125D0*EL*m12squared*SA1*SA3*&
  &dAlpha1PinchPStar()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.1875D0*EL*m12squared*SA1*SA3*dAlpha1PinchPStar()*DBLE(SA2**INT(3.D0)&
  &))/(CB2*MW*SB*SW) - (0.84375D0*CA12*EL*m12squared*SA1*SA3*dAlpha1PinchPStar()*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) - (0.5625D0&
  &*CA1*EL*MH12*SA12*SA3*dAlpha1PinchPStar()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.28125D0*CA1*EL*MH22*SA12*SA3*dAlpha1PinchPStar&
  &()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) + (0.1875D0*CA1*EL*m12squared*SA3*dAlpha1PinchPStar()*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW)&
  & + (0.84375D0*CA1*EL*m12squared*SA12*SA3*dAlpha1PinchPStar()*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) + (0.09375D0*CA1*EL*m12squar&
  &ed*SA3*dAlpha1PinchPStar()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) - (0.09375D0*EL*m12squared*SA1*SA3*TB*dAlpha1PinchPStar()*DBLE(&
  &SA2**INT(3.D0)))/(CB*MW*SW) + (1.5D0*MH12*SA3*dAlpha2PinchPStar()*DBLE(SA2**INT(3.D0)))/vS + (0.75D0*MH22*SA3*dAlpha2PinchPSta&
  &r()*DBLE(SA2**INT(3.D0)))/vS - (0.1875D0*CA1*CA3*EL*MH12*dAlpha3PinchPStar()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.09375D0*CA1&
  &*CA3*EL*MH22*dAlpha3PinchPStar()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.28125D0*CA3*EL*m12squared*SA1*dAlpha3PinchPStar()*DBLE(&
  &SA2**INT(3.D0)))/(CB*MW*SW) + (0.1875D0*CA1*CA3*EL*MH12*SA12*dAlpha3PinchPStar()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.09375D0&
  &*CA1*CA3*EL*MH22*SA12*dAlpha3PinchPStar()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.28125D0*CA1*CA3*EL*m12squared*dAlpha3PinchPSta&
  &r()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) + (0.1875D0*CA1*CA3*EL*m12squared*dAlpha3PinchPStar()*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW&
  &) - (0.1875D0*CA3*EL*MH12*SA1*dAlpha3PinchPStar()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) + (0.1875D0*CA12*CA3*EL*MH12*SA1*dAlpha3Pin&
  &chPStar()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.09375D0*CA3*EL*MH22*SA1*dAlpha3PinchPStar()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) +&
  & (0.09375D0*CA12*CA3*EL*MH22*SA1*dAlpha3PinchPStar()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.28125D0*CA1*CA3*EL*m12squared*SA12*&
  &dAlpha3PinchPStar()*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) + (0.1875D0*CA3*EL*m12squared*SA1*dAlpha3PinchPStar()*DBLE(SA2**INT(3&
  &.D0)))/(CB*MW*SB2*SW) - (0.28125D0*CA12*CA3*EL*m12squared*SA1*dAlpha3PinchPStar()*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) + (0.09&
  &375D0*CA3*EL*m12squared*SA1*dAlpha3PinchPStar()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) + (0.09375D0*CA1*CA3*EL*m12squared*TB*dAlp&
  &ha3PinchPStar()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.09375D0*CA1*EL*m12squared*SA3*dBeta2PinchPStar()*DBLE(SA2**INT(3.D0)))/(&
  &CB*MW*SW) + (0.09375D0*CA1*EL*MH12*SA3*dBeta2PinchPStar()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.09375D0*CA1*EL*MH12*SA3*dBeta2&
  &PinchPStar()*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) + (0.046875D0*CA1*EL*MH22*SA3*dBeta2PinchPStar()*DBLE(SA2**INT(3.D0)))/(MW*S&
  &B*SW) - (0.046875D0*CA1*EL*MH22*SA3*dBeta2PinchPStar()*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) + (0.140625D0*EL*m12squared*SA1*SA&
  &3*dBeta2PinchPStar()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) + (0.046875D0*EL*m12squared*SA1*SA3*dBeta2PinchPStar()*DBLE(SA2**INT(3.D&
  &0)))/(CB2*MW*SB*SW) - (0.28125D0*CA12*EL*m12squared*SA1*SA3*dBeta2PinchPStar()*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) - (0.09375&
  &D0*CA1*EL*MH12*SA12*SA3*dBeta2PinchPStar()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) + (0.09375D0*CA1*EL*MH12*SA12*SA3*dBeta2PinchPStar&
  &()*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) - (0.046875D0*CA1*EL*MH22*SA12*SA3*dBeta2PinchPStar()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW)&
  & + (0.046875D0*CA1*EL*MH22*SA12*SA3*dBeta2PinchPStar()*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) - (0.10546875D0*CA1*EL*m12squared*&
  &SA3*dBeta2PinchPStar()*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) + (0.38671875D0*CA1*EL*m12squared*SA12*SA3*dBeta2PinchPStar()*DBLE&
  &(SA2**INT(3.D0)))/(CB*MW*SB2*SW) + (0.140625D0*CA1*EL*m12squared*SA3*dBeta2PinchPStar()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) + &
  &(0.1875D0*EL*MH12*SA1*SA3*dBeta2PinchPStar()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) - (0.1875D0*CA12*EL*MH12*SA1*SA3*dBeta2PinchP&
  &Star()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) + (0.09375D0*EL*MH22*SA1*SA3*dBeta2PinchPStar()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB)&
  & - (0.09375D0*CA12*EL*MH22*SA1*SA3*dBeta2PinchPStar()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) - (0.09375D0*CA1*EL*MH12*SA3*TB*dBet&
  &a2PinchPStar()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.046875D0*CA1*EL*MH22*SA3*TB*dBeta2PinchPStar()*DBLE(SA2**INT(3.D0)))/(CB*&
  &MW*SW) - (0.140625D0*EL*m12squared*SA1*SA3*TB*dBeta2PinchPStar()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.09375D0*CA1*EL*MH12*SA1&
  &2*SA3*TB*dBeta2PinchPStar()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.046875D0*CA1*EL*MH22*SA12*SA3*TB*dBeta2PinchPStar()*DBLE(SA2&
  &**INT(3.D0)))/(CB*MW*SW) - (0.09375D0*EL*m12squared*SA1*SA3*dBeta2PinchPStar()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB2) + (0.14062&
  &5D0*CA1*EL*m12squared*SA3*TB2*dBeta2PinchPStar()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.1875D0*EL*MH12*SA3*dAlpha1PinchPStar()*&
  &DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) + (0.09375D0*EL*MH22*SA3*dAlpha1PinchPStar()*DBLE(CA1**INT(3.D0))*DBLE(S&
  &A2**INT(3.D0)))/(MW*SB*SW) - (0.28125D0*EL*m12squared*SA3*dAlpha1PinchPStar()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*M&
  &W*SB2*SW) - (0.0625D0*CA3*EL*MH12*dAlpha3PinchPStar()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.03125D0*CA3*E&
  &L*MH22*dAlpha3PinchPStar()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.09375D0*CA3*EL*m12squared*dAlpha3PinchPS&
  &tar()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) + (0.03125D0*EL*MH12*SA3*dBeta2PinchPStar()*DBLE(CA1**INT(3.D0&
  &))*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.03125D0*EL*MH12*SA3*dBeta2PinchPStar()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB&
  &2*MW*SB*SW) + (0.015625D0*EL*MH22*SA3*dBeta2PinchPStar()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.015625D0*E&
  &L*MH22*SA3*dBeta2PinchPStar()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) - (0.12890625D0*EL*m12squared*SA3*dBet&
  &a2PinchPStar()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) - (0.03125D0*EL*MH12*SA3*TB*dBeta2PinchPStar()*DBLE(C&
  &A1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.015625D0*EL*MH22*SA3*TB*dBeta2PinchPStar()*DBLE(CA1**INT(3.D0))*DBLE(SA2*&
  &*INT(3.D0)))/(CB*MW*SW) + (0.36328125D0*CA1*EL*m12squared*SA3*dBeta2PinchPStar()*DBLE(CB**INT(-3.D0))*DBLE(SA2**INT(3.D0)))/(M&
  &W*SW) - (0.45703125D0*CA1*EL*m12squared*SA12*SA3*dBeta2PinchPStar()*DBLE(CB**INT(-3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SW) + (0.05&
  &859375D0*CA1*EL*m12squared*SA3*dBeta2PinchPStar()*DBLE(CB**INT(-3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SB2*SW) - (0.10546875D0*CA1*E&
  &L*m12squared*SA12*SA3*dBeta2PinchPStar()*DBLE(CB**INT(-3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SB2*SW) + (0.15234375D0*EL*m12squared*&
  &SA3*dBeta2PinchPStar()*DBLE(CA1**INT(3.D0))*DBLE(CB**INT(-3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SW) + (0.03515625D0*EL*m12squared*S&
  &A3*dBeta2PinchPStar()*DBLE(CA1**INT(3.D0))*DBLE(CB**INT(-3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SB2*SW) - (0.1875D0*EL*MH12*SA3*dAlp&
  &ha1PinchPStar()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.09375D0*EL*MH22*SA3*dAlpha1PinchPStar()*DBLE(SA1**I&
  &NT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.28125D0*EL*m12squared*SA3*dAlpha1PinchPStar()*DBLE(SA1**INT(3.D0))*DBLE(SA2**I&
  &NT(3.D0)))/(CB2*MW*SB*SW) - (0.0625D0*CA3*EL*MH12*dAlpha3PinchPStar()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - &
  &(0.03125D0*CA3*EL*MH22*dAlpha3PinchPStar()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) + (0.09375D0*CA3*EL*m12square&
  &d*dAlpha3PinchPStar()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) + (0.09375D0*EL*m12squared*SA3*dBeta2PinchPSta&
  &r()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) + (0.0625D0*EL*MH12*SA3*dBeta2PinchPStar()*DBLE(SA1**INT(3.D0))*&
  &DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) + (0.03125D0*EL*MH22*SA3*dBeta2PinchPStar()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(MW&
  &*SB*SW*TB) - (0.1875D0*CA1*CA3*EL*m12squared*dBeta2PinchPStar()*DBLE(SB**INT(-3.D0)))/(MW*SW) - (0.1875D0*CA1*CA22*CA3*EL*m12s&
  &quared*dBeta2PinchPStar()*DBLE(SB**INT(-3.D0)))/(MW*SW) - (1.125D0*CA1*CA3*EL*m12squared*SA12*dBeta2PinchPStar()*DBLE(SB**INT(&
  &-3.D0)))/(MW*SW) - (1.125D0*CA1*CA22*CA3*EL*m12squared*SA12*dBeta2PinchPStar()*DBLE(SB**INT(-3.D0)))/(MW*SW) + (0.1875D0*CA1*C&
  &A3*EL*m12squared*SA22*dBeta2PinchPStar()*DBLE(SB**INT(-3.D0)))/(MW*SW) + (1.125D0*CA1*CA3*EL*m12squared*SA12*SA22*dBeta2PinchP&
  &Star()*DBLE(SB**INT(-3.D0)))/(MW*SW) + (0.46875D0*EL*m12squared*SA1*SA2*SA3*dBeta2PinchPStar()*DBLE(SB**INT(-3.D0)))/(MW*SW) -&
  & (0.5625D0*CA12*EL*m12squared*SA1*SA2*SA3*dBeta2PinchPStar()*DBLE(SB**INT(-3.D0)))/(MW*SW) + (1.40625D0*CA22*EL*m12squared*SA1&
  &*SA2*SA3*dBeta2PinchPStar()*DBLE(SB**INT(-3.D0)))/(MW*SW) - (1.6875D0*CA12*CA22*EL*m12squared*SA1*SA2*SA3*dBeta2PinchPStar()*D&
  &BLE(SB**INT(-3.D0)))/(MW*SW) + (0.375D0*CA3*EL*m12squared*dBeta2PinchPStar()*DBLE(CA1**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(MW*SW&
  &) + (0.375D0*CA22*CA3*EL*m12squared*dBeta2PinchPStar()*DBLE(CA1**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(MW*SW) - (0.375D0*CA3*EL*m1&
  &2squared*SA22*dBeta2PinchPStar()*DBLE(CA1**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(MW*SW) + (0.1875D0*EL*m12squared*SA2*SA3*dBeta2Pi&
  &nchPStar()*DBLE(SA1**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(MW*SW) + (0.5625D0*CA22*EL*m12squared*SA2*SA3*dBeta2PinchPStar()*DBLE(S&
  &A1**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(MW*SW) - (0.46875D0*EL*m12squared*SA1*SA3*dBeta2PinchPStar()*DBLE(SA2**INT(3.D0))*DBLE(S&
  &B**INT(-3.D0)))/(MW*SW) + (0.5625D0*CA12*EL*m12squared*SA1*SA3*dBeta2PinchPStar()*DBLE(SA2**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(&
  &MW*SW) - (0.1875D0*EL*m12squared*SA3*dBeta2PinchPStar()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(MW*SW&
  &) - (0.125D0*CA1*CA3*m12squared*dgAtMZ())/(CB*MW) - (0.125D0*CA1*CA22*CA3*m12squared*dgAtMZ())/(CB*MW) + (0.125D0*CA3*MH12*SA1&
  &*dgAtMZ())/(CB*MW) + (0.375D0*CA12*CA3*MH12*SA1*dgAtMZ())/(CB*MW) + (0.125D0*CA22*CA3*MH12*SA1*dgAtMZ())/(CB*MW) + (0.375D0*CA&
  &12*CA22*CA3*MH12*SA1*dgAtMZ())/(CB*MW) + (0.0625D0*CA3*MH22*SA1*dgAtMZ())/(CB*MW) + (0.1875D0*CA12*CA3*MH22*SA1*dgAtMZ())/(CB*&
  &MW) + (0.0625D0*CA22*CA3*MH22*SA1*dgAtMZ())/(CB*MW) + (0.1875D0*CA12*CA22*CA3*MH22*SA1*dgAtMZ())/(CB*MW) + (0.125D0*CA1*CA3*m1&
  &2squared*SA22*dgAtMZ())/(CB*MW) - (0.125D0*CA3*MH12*SA1*SA22*dgAtMZ())/(CB*MW) - (0.375D0*CA12*CA3*MH12*SA1*SA22*dgAtMZ())/(CB&
  &*MW) - (0.0625D0*CA3*MH22*SA1*SA22*dgAtMZ())/(CB*MW) - (0.1875D0*CA12*CA3*MH22*SA1*SA22*dgAtMZ())/(CB*MW) + (0.1875D0*CA1*MH12&
  &*SA2*SA3*dgAtMZ())/(CB*MW) + (0.5625D0*CA1*CA22*MH12*SA2*SA3*dgAtMZ())/(CB*MW) + (0.09375D0*CA1*MH22*SA2*SA3*dgAtMZ())/(CB*MW)&
  & + (0.28125D0*CA1*CA22*MH22*SA2*SA3*dgAtMZ())/(CB*MW) + (0.28125D0*m12squared*SA1*SA2*SA3*dgAtMZ())/(CB*MW) + (0.84375D0*CA22*&
  &m12squared*SA1*SA2*SA3*dgAtMZ())/(CB*MW) - (0.1875D0*CA1*MH12*SA12*SA2*SA3*dgAtMZ())/(CB*MW) - (0.5625D0*CA1*CA22*MH12*SA12*SA&
  &2*SA3*dgAtMZ())/(CB*MW) - (0.09375D0*CA1*MH22*SA12*SA2*SA3*dgAtMZ())/(CB*MW) - (0.28125D0*CA1*CA22*MH22*SA12*SA2*SA3*dgAtMZ())&
  &/(CB*MW) - (0.125D0*CA1*CA3*MH12*dgAtMZ())/(MW*SB) - (0.125D0*CA1*CA22*CA3*MH12*dgAtMZ())/(MW*SB) - (0.0625D0*CA1*CA3*MH22*dgA&
  &tMZ())/(MW*SB) - (0.0625D0*CA1*CA22*CA3*MH22*dgAtMZ())/(MW*SB) + (0.21875D0*CA3*m12squared*SA1*dgAtMZ())/(MW*SB) + (0.21875D0*&
  &CA22*CA3*m12squared*SA1*dgAtMZ())/(MW*SB) - (0.15625D0*CA3*m12squared*SA1*dgAtMZ())/(CB2*MW*SB) - (0.5625D0*CA12*CA3*m12square&
  &d*SA1*dgAtMZ())/(CB2*MW*SB) - (0.15625D0*CA22*CA3*m12squared*SA1*dgAtMZ())/(CB2*MW*SB) - (0.5625D0*CA12*CA22*CA3*m12squared*SA&
  &1*dgAtMZ())/(CB2*MW*SB) - (0.375D0*CA1*CA3*MH12*SA12*dgAtMZ())/(MW*SB) - (0.375D0*CA1*CA22*CA3*MH12*SA12*dgAtMZ())/(MW*SB) - (&
  &0.1875D0*CA1*CA3*MH22*SA12*dgAtMZ())/(MW*SB) - (0.1875D0*CA1*CA22*CA3*MH22*SA12*dgAtMZ())/(MW*SB) + (0.125D0*CA1*CA3*MH12*SA22&
  &*dgAtMZ())/(MW*SB) + (0.0625D0*CA1*CA3*MH22*SA22*dgAtMZ())/(MW*SB) - (0.21875D0*CA3*m12squared*SA1*SA22*dgAtMZ())/(MW*SB) + (0&
  &.15625D0*CA3*m12squared*SA1*SA22*dgAtMZ())/(CB2*MW*SB) + (0.5625D0*CA12*CA3*m12squared*SA1*SA22*dgAtMZ())/(CB2*MW*SB) + (0.375&
  &D0*CA1*CA3*MH12*SA12*SA22*dgAtMZ())/(MW*SB) + (0.1875D0*CA1*CA3*MH22*SA12*SA22*dgAtMZ())/(MW*SB) + (0.28125D0*CA1*m12squared*S&
  &A2*SA3*dgAtMZ())/(MW*SB) + (0.84375D0*CA1*CA22*m12squared*SA2*SA3*dgAtMZ())/(MW*SB) - (0.1875D0*CA1*m12squared*SA2*SA3*dgAtMZ(&
  &))/(CB2*MW*SB) - (0.5625D0*CA1*CA22*m12squared*SA2*SA3*dgAtMZ())/(CB2*MW*SB) + (0.1875D0*MH12*SA1*SA2*SA3*dgAtMZ())/(MW*SB) - &
  &(0.1875D0*CA12*MH12*SA1*SA2*SA3*dgAtMZ())/(MW*SB) + (0.5625D0*CA22*MH12*SA1*SA2*SA3*dgAtMZ())/(MW*SB) - (0.5625D0*CA12*CA22*MH&
  &12*SA1*SA2*SA3*dgAtMZ())/(MW*SB) + (0.09375D0*MH22*SA1*SA2*SA3*dgAtMZ())/(MW*SB) - (0.09375D0*CA12*MH22*SA1*SA2*SA3*dgAtMZ())/&
  &(MW*SB) + (0.28125D0*CA22*MH22*SA1*SA2*SA3*dgAtMZ())/(MW*SB) - (0.28125D0*CA12*CA22*MH22*SA1*SA2*SA3*dgAtMZ())/(MW*SB) + (0.28&
  &125D0*CA1*m12squared*SA12*SA2*SA3*dgAtMZ())/(CB2*MW*SB) + (0.84375D0*CA1*CA22*m12squared*SA12*SA2*SA3*dgAtMZ())/(CB2*MW*SB) + &
  &(0.0625D0*CA1*CA3*m12squared*dgAtMZ())/(CB*MW*SB2) + (0.0625D0*CA1*CA22*CA3*m12squared*dgAtMZ())/(CB*MW*SB2) + (0.5625D0*CA1*C&
  &A3*m12squared*SA12*dgAtMZ())/(CB*MW*SB2) + (0.5625D0*CA1*CA22*CA3*m12squared*SA12*dgAtMZ())/(CB*MW*SB2) - (0.0625D0*CA1*CA3*m1&
  &2squared*SA22*dgAtMZ())/(CB*MW*SB2) - (0.5625D0*CA1*CA3*m12squared*SA12*SA22*dgAtMZ())/(CB*MW*SB2) - (0.1875D0*m12squared*SA1*&
  &SA2*SA3*dgAtMZ())/(CB*MW*SB2) + (0.28125D0*CA12*m12squared*SA1*SA2*SA3*dgAtMZ())/(CB*MW*SB2) - (0.5625D0*CA22*m12squared*SA1*S&
  &A2*SA3*dgAtMZ())/(CB*MW*SB2) + (0.84375D0*CA12*CA22*m12squared*SA1*SA2*SA3*dgAtMZ())/(CB*MW*SB2) + (0.125D0*CA1*CA3*m12squared&
  &*dgAtMZ())/(MW*SB*TB) + (0.125D0*CA1*CA22*CA3*m12squared*dgAtMZ())/(MW*SB*TB) - (0.125D0*CA1*CA3*m12squared*SA22*dgAtMZ())/(MW&
  &*SB*TB) - (0.09375D0*m12squared*SA1*SA2*SA3*dgAtMZ())/(MW*SB*TB) - (0.28125D0*CA22*m12squared*SA1*SA2*SA3*dgAtMZ())/(MW*SB*TB)&
  & - (0.03125D0*CA3*m12squared*SA1*TB*dgAtMZ())/(CB*MW) - (0.03125D0*CA22*CA3*m12squared*SA1*TB*dgAtMZ())/(CB*MW) + (0.03125D0*C&
  &A3*m12squared*SA1*SA22*TB*dgAtMZ())/(CB*MW) - (0.09375D0*CA1*m12squared*SA2*SA3*TB*dgAtMZ())/(CB*MW) - (0.28125D0*CA1*CA22*m12&
  &squared*SA2*SA3*TB*dgAtMZ())/(CB*MW) + (0.0625D0*MH12*SA2*SA3*DBLE(CA1**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.1875D0*CA22*MH12*SA2&
  &*SA3*DBLE(CA1**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.03125D0*MH22*SA2*SA3*DBLE(CA1**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.09375D0*CA22&
  &*MH22*SA2*SA3*DBLE(CA1**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.125D0*CA3*MH12*DBLE(CA1**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.125D0*CA2&
  &2*CA3*MH12*DBLE(CA1**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.0625D0*CA3*MH22*DBLE(CA1**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.0625D0*CA22&
  &*CA3*MH22*DBLE(CA1**INT(3.D0))*dgAtMZ())/(MW*SB) - (0.125D0*CA3*MH12*SA22*DBLE(CA1**INT(3.D0))*dgAtMZ())/(MW*SB) - (0.0625D0*C&
  &A3*MH22*SA22*DBLE(CA1**INT(3.D0))*dgAtMZ())/(MW*SB) - (0.09375D0*m12squared*SA2*SA3*DBLE(CA1**INT(3.D0))*dgAtMZ())/(CB2*MW*SB)&
  & - (0.28125D0*CA22*m12squared*SA2*SA3*DBLE(CA1**INT(3.D0))*dgAtMZ())/(CB2*MW*SB) - (0.1875D0*CA3*m12squared*DBLE(CA1**INT(3.D0&
  &))*dgAtMZ())/(CB*MW*SB2) - (0.1875D0*CA22*CA3*m12squared*DBLE(CA1**INT(3.D0))*dgAtMZ())/(CB*MW*SB2) + (0.1875D0*CA3*m12squared&
  &*SA22*DBLE(CA1**INT(3.D0))*dgAtMZ())/(CB*MW*SB2) - (0.125D0*CA3*MH12*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB*MW) - (0.125D0*CA22*CA&
  &3*MH12*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB*MW) - (0.0625D0*CA3*MH22*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB*MW) - (0.0625D0*CA22*CA3&
  &*MH22*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.125D0*CA3*MH12*SA22*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.0625D0*CA3*M&
  &H22*SA22*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.1875D0*CA3*m12squared*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB2*MW*SB) + (0.187&
  &5D0*CA22*CA3*m12squared*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB2*MW*SB) - (0.1875D0*CA3*m12squared*SA22*DBLE(SA1**INT(3.D0))*dgAtMZ&
  &())/(CB2*MW*SB) + (0.0625D0*MH12*SA2*SA3*DBLE(SA1**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.1875D0*CA22*MH12*SA2*SA3*DBLE(SA1**INT(3.&
  &D0))*dgAtMZ())/(MW*SB) + (0.03125D0*MH22*SA2*SA3*DBLE(SA1**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.09375D0*CA22*MH22*SA2*SA3*DBLE(SA&
  &1**INT(3.D0))*dgAtMZ())/(MW*SB) - (0.09375D0*m12squared*SA2*SA3*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB*MW*SB2) - (0.28125D0*CA22*m&
  &12squared*SA2*SA3*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB*MW*SB2) - (0.1875D0*CA1*MH12*SA3*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW) -&
  & (0.09375D0*CA1*MH22*SA3*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW) - (0.28125D0*m12squared*SA1*SA3*DBLE(SA2**INT(3.D0))*dgAtMZ())&
  &/(CB*MW) + (0.1875D0*CA1*MH12*SA12*SA3*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.09375D0*CA1*MH22*SA12*SA3*DBLE(SA2**INT(3.D&
  &0))*dgAtMZ())/(CB*MW) - (0.28125D0*CA1*m12squared*SA3*DBLE(SA2**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.1875D0*CA1*m12squared*SA3*DB&
  &LE(SA2**INT(3.D0))*dgAtMZ())/(CB2*MW*SB) - (0.1875D0*MH12*SA1*SA3*DBLE(SA2**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.1875D0*CA12*MH12&
  &*SA1*SA3*DBLE(SA2**INT(3.D0))*dgAtMZ())/(MW*SB) - (0.09375D0*MH22*SA1*SA3*DBLE(SA2**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.09375D0*&
  &CA12*MH22*SA1*SA3*DBLE(SA2**INT(3.D0))*dgAtMZ())/(MW*SB) - (0.28125D0*CA1*m12squared*SA12*SA3*DBLE(SA2**INT(3.D0))*dgAtMZ())/(&
  &CB2*MW*SB) + (0.1875D0*m12squared*SA1*SA3*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW*SB2) - (0.28125D0*CA12*m12squared*SA1*SA3*DBLE&
  &(SA2**INT(3.D0))*dgAtMZ())/(CB*MW*SB2) + (0.09375D0*m12squared*SA1*SA3*DBLE(SA2**INT(3.D0))*dgAtMZ())/(MW*SB*TB) + (0.09375D0*&
  &CA1*m12squared*SA3*TB*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW) - (0.0625D0*MH12*SA3*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dg&
  &AtMZ())/(CB*MW) - (0.03125D0*MH22*SA3*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.09375D0*m12squared*SA3*&
  &DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB2*MW*SB) - (0.0625D0*MH12*SA3*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))&
  &*dgAtMZ())/(MW*SB) - (0.03125D0*MH22*SA3*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.09375D0*m12squared*S&
  &A3*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW*SB2) - (0.125D0*CA1*CA3*EL*dm122MSBarAlter())/(CB*MW*SW) - (0.12&
  &5D0*CA1*CA22*CA3*EL*dm122MSBarAlter())/(CB*MW*SW) + (0.125D0*CA1*CA3*EL*SA22*dm122MSBarAlter())/(CB*MW*SW) + (0.28125D0*EL*SA1&
  &*SA2*SA3*dm122MSBarAlter())/(CB*MW*SW) + (0.84375D0*CA22*EL*SA1*SA2*SA3*dm122MSBarAlter())/(CB*MW*SW) + (0.21875D0*CA3*EL*SA1*&
  &dm122MSBarAlter())/(MW*SB*SW) + (0.21875D0*CA22*CA3*EL*SA1*dm122MSBarAlter())/(MW*SB*SW) - (0.15625D0*CA3*EL*SA1*dm122MSBarAlt&
  &er())/(CB2*MW*SB*SW) - (0.5625D0*CA12*CA3*EL*SA1*dm122MSBarAlter())/(CB2*MW*SB*SW) - (0.15625D0*CA22*CA3*EL*SA1*dm122MSBarAlte&
  &r())/(CB2*MW*SB*SW) - (0.5625D0*CA12*CA22*CA3*EL*SA1*dm122MSBarAlter())/(CB2*MW*SB*SW) - (0.21875D0*CA3*EL*SA1*SA22*dm122MSBar&
  &Alter())/(MW*SB*SW) + (0.15625D0*CA3*EL*SA1*SA22*dm122MSBarAlter())/(CB2*MW*SB*SW) + (0.5625D0*CA12*CA3*EL*SA1*SA22*dm122MSBar&
  &Alter())/(CB2*MW*SB*SW) + (0.28125D0*CA1*EL*SA2*SA3*dm122MSBarAlter())/(MW*SB*SW) + (0.84375D0*CA1*CA22*EL*SA2*SA3*dm122MSBarA&
  &lter())/(MW*SB*SW) - (0.1875D0*CA1*EL*SA2*SA3*dm122MSBarAlter())/(CB2*MW*SB*SW) - (0.5625D0*CA1*CA22*EL*SA2*SA3*dm122MSBarAlte&
  &r())/(CB2*MW*SB*SW) + (0.28125D0*CA1*EL*SA12*SA2*SA3*dm122MSBarAlter())/(CB2*MW*SB*SW) + (0.84375D0*CA1*CA22*EL*SA12*SA2*SA3*d&
  &m122MSBarAlter())/(CB2*MW*SB*SW) + (0.0625D0*CA1*CA3*EL*dm122MSBarAlter())/(CB*MW*SB2*SW) + (0.0625D0*CA1*CA22*CA3*EL*dm122MSB&
  &arAlter())/(CB*MW*SB2*SW) + (0.5625D0*CA1*CA3*EL*SA12*dm122MSBarAlter())/(CB*MW*SB2*SW) + (0.5625D0*CA1*CA22*CA3*EL*SA12*dm122&
  &MSBarAlter())/(CB*MW*SB2*SW) - (0.0625D0*CA1*CA3*EL*SA22*dm122MSBarAlter())/(CB*MW*SB2*SW) - (0.5625D0*CA1*CA3*EL*SA12*SA22*dm&
  &122MSBarAlter())/(CB*MW*SB2*SW) - (0.1875D0*EL*SA1*SA2*SA3*dm122MSBarAlter())/(CB*MW*SB2*SW) + (0.28125D0*CA12*EL*SA1*SA2*SA3*&
  &dm122MSBarAlter())/(CB*MW*SB2*SW) - (0.5625D0*CA22*EL*SA1*SA2*SA3*dm122MSBarAlter())/(CB*MW*SB2*SW) + (0.84375D0*CA12*CA22*EL*&
  &SA1*SA2*SA3*dm122MSBarAlter())/(CB*MW*SB2*SW) + (0.125D0*CA1*CA3*EL*dm122MSBarAlter())/(MW*SB*SW*TB) + (0.125D0*CA1*CA22*CA3*E&
  &L*dm122MSBarAlter())/(MW*SB*SW*TB) - (0.125D0*CA1*CA3*EL*SA22*dm122MSBarAlter())/(MW*SB*SW*TB) - (0.09375D0*EL*SA1*SA2*SA3*dm1&
  &22MSBarAlter())/(MW*SB*SW*TB) - (0.28125D0*CA22*EL*SA1*SA2*SA3*dm122MSBarAlter())/(MW*SB*SW*TB) - (0.03125D0*CA3*EL*SA1*TB*dm1&
  &22MSBarAlter())/(CB*MW*SW) - (0.03125D0*CA22*CA3*EL*SA1*TB*dm122MSBarAlter())/(CB*MW*SW) + (0.03125D0*CA3*EL*SA1*SA22*TB*dm122&
  &MSBarAlter())/(CB*MW*SW) - (0.09375D0*CA1*EL*SA2*SA3*TB*dm122MSBarAlter())/(CB*MW*SW) - (0.28125D0*CA1*CA22*EL*SA2*SA3*TB*dm12&
  &2MSBarAlter())/(CB*MW*SW) - (0.09375D0*EL*SA2*SA3*DBLE(CA1**INT(3.D0))*dm122MSBarAlter())/(CB2*MW*SB*SW) - (0.28125D0*CA22*EL*&
  &SA2*SA3*DBLE(CA1**INT(3.D0))*dm122MSBarAlter())/(CB2*MW*SB*SW) - (0.1875D0*CA3*EL*DBLE(CA1**INT(3.D0))*dm122MSBarAlter())/(CB*&
  &MW*SB2*SW) - (0.1875D0*CA22*CA3*EL*DBLE(CA1**INT(3.D0))*dm122MSBarAlter())/(CB*MW*SB2*SW) + (0.1875D0*CA3*EL*SA22*DBLE(CA1**IN&
  &T(3.D0))*dm122MSBarAlter())/(CB*MW*SB2*SW) + (0.1875D0*CA3*EL*DBLE(SA1**INT(3.D0))*dm122MSBarAlter())/(CB2*MW*SB*SW) + (0.1875&
  &D0*CA22*CA3*EL*DBLE(SA1**INT(3.D0))*dm122MSBarAlter())/(CB2*MW*SB*SW) - (0.1875D0*CA3*EL*SA22*DBLE(SA1**INT(3.D0))*dm122MSBarA&
  &lter())/(CB2*MW*SB*SW) - (0.09375D0*EL*SA2*SA3*DBLE(SA1**INT(3.D0))*dm122MSBarAlter())/(CB*MW*SB2*SW) - (0.28125D0*CA22*EL*SA2&
  &*SA3*DBLE(SA1**INT(3.D0))*dm122MSBarAlter())/(CB*MW*SB2*SW) - (0.28125D0*EL*SA1*SA3*DBLE(SA2**INT(3.D0))*dm122MSBarAlter())/(C&
  &B*MW*SW) - (0.28125D0*CA1*EL*SA3*DBLE(SA2**INT(3.D0))*dm122MSBarAlter())/(MW*SB*SW) + (0.1875D0*CA1*EL*SA3*DBLE(SA2**INT(3.D0)&
  &)*dm122MSBarAlter())/(CB2*MW*SB*SW) - (0.28125D0*CA1*EL*SA12*SA3*DBLE(SA2**INT(3.D0))*dm122MSBarAlter())/(CB2*MW*SB*SW) + (0.1&
  &875D0*EL*SA1*SA3*DBLE(SA2**INT(3.D0))*dm122MSBarAlter())/(CB*MW*SB2*SW) - (0.28125D0*CA12*EL*SA1*SA3*DBLE(SA2**INT(3.D0))*dm12&
  &2MSBarAlter())/(CB*MW*SB2*SW) + (0.09375D0*EL*SA1*SA3*DBLE(SA2**INT(3.D0))*dm122MSBarAlter())/(MW*SB*SW*TB) + (0.09375D0*CA1*E&
  &L*SA3*TB*DBLE(SA2**INT(3.D0))*dm122MSBarAlter())/(CB*MW*SW) + (0.09375D0*EL*SA3*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dm12&
  &2MSBarAlter())/(CB2*MW*SB*SW) + (0.09375D0*EL*SA3*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dm122MSBarAlter())/(CB*MW*SB2*SW) &
  &+ (0.125D0*CA3*EL*SA1*dMH12OSAlter())/(CB*MW*SW) + (0.375D0*CA12*CA3*EL*SA1*dMH12OSAlter())/(CB*MW*SW) + (0.125D0*CA22*CA3*EL*&
  &SA1*dMH12OSAlter())/(CB*MW*SW) + (0.375D0*CA12*CA22*CA3*EL*SA1*dMH12OSAlter())/(CB*MW*SW) - (0.125D0*CA3*EL*SA1*SA22*dMH12OSAl&
  &ter())/(CB*MW*SW) - (0.375D0*CA12*CA3*EL*SA1*SA22*dMH12OSAlter())/(CB*MW*SW) + (0.1875D0*CA1*EL*SA2*SA3*dMH12OSAlter())/(CB*MW&
  &*SW) + (0.5625D0*CA1*CA22*EL*SA2*SA3*dMH12OSAlter())/(CB*MW*SW) - (0.1875D0*CA1*EL*SA12*SA2*SA3*dMH12OSAlter())/(CB*MW*SW) - (&
  &0.5625D0*CA1*CA22*EL*SA12*SA2*SA3*dMH12OSAlter())/(CB*MW*SW) - (0.125D0*CA1*CA3*EL*dMH12OSAlter())/(MW*SB*SW) - (0.125D0*CA1*C&
  &A22*CA3*EL*dMH12OSAlter())/(MW*SB*SW) - (0.375D0*CA1*CA3*EL*SA12*dMH12OSAlter())/(MW*SB*SW) - (0.375D0*CA1*CA22*CA3*EL*SA12*dM&
  &H12OSAlter())/(MW*SB*SW) + (0.125D0*CA1*CA3*EL*SA22*dMH12OSAlter())/(MW*SB*SW) + (0.375D0*CA1*CA3*EL*SA12*SA22*dMH12OSAlter())&
  &/(MW*SB*SW) + (0.1875D0*EL*SA1*SA2*SA3*dMH12OSAlter())/(MW*SB*SW) - (0.1875D0*CA12*EL*SA1*SA2*SA3*dMH12OSAlter())/(MW*SB*SW) +&
  & (0.5625D0*CA22*EL*SA1*SA2*SA3*dMH12OSAlter())/(MW*SB*SW) - (0.5625D0*CA12*CA22*EL*SA1*SA2*SA3*dMH12OSAlter())/(MW*SB*SW) - (0&
  &.5D0*CA2*SA3*dMH12OSAlter())/vS - (1.5D0*CA2*SA22*SA3*dMH12OSAlter())/vS + (0.0625D0*EL*SA2*SA3*DBLE(CA1**INT(3.D0))*dMH12OSAl&
  &ter())/(CB*MW*SW) + (0.1875D0*CA22*EL*SA2*SA3*DBLE(CA1**INT(3.D0))*dMH12OSAlter())/(CB*MW*SW) + (0.125D0*CA3*EL*DBLE(CA1**INT(&
  &3.D0))*dMH12OSAlter())/(MW*SB*SW) + (0.125D0*CA22*CA3*EL*DBLE(CA1**INT(3.D0))*dMH12OSAlter())/(MW*SB*SW) - (0.125D0*CA3*EL*SA2&
  &2*DBLE(CA1**INT(3.D0))*dMH12OSAlter())/(MW*SB*SW) + (0.5D0*SA3*DBLE(CA2**INT(3.D0))*dMH12OSAlter())/vS - (0.125D0*CA3*EL*DBLE(&
  &SA1**INT(3.D0))*dMH12OSAlter())/(CB*MW*SW) - (0.125D0*CA22*CA3*EL*DBLE(SA1**INT(3.D0))*dMH12OSAlter())/(CB*MW*SW) + (0.125D0*C&
  &A3*EL*SA22*DBLE(SA1**INT(3.D0))*dMH12OSAlter())/(CB*MW*SW) + (0.0625D0*EL*SA2*SA3*DBLE(SA1**INT(3.D0))*dMH12OSAlter())/(MW*SB*&
  &SW) + (0.1875D0*CA22*EL*SA2*SA3*DBLE(SA1**INT(3.D0))*dMH12OSAlter())/(MW*SB*SW) - (0.1875D0*CA1*EL*SA3*DBLE(SA2**INT(3.D0))*dM&
  &H12OSAlter())/(CB*MW*SW) + (0.1875D0*CA1*EL*SA12*SA3*DBLE(SA2**INT(3.D0))*dMH12OSAlter())/(CB*MW*SW) - (0.1875D0*EL*SA1*SA3*DB&
  &LE(SA2**INT(3.D0))*dMH12OSAlter())/(MW*SB*SW) + (0.1875D0*CA12*EL*SA1*SA3*DBLE(SA2**INT(3.D0))*dMH12OSAlter())/(MW*SB*SW) - (0&
  &.0625D0*EL*SA3*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dMH12OSAlter())/(CB*MW*SW) - (0.0625D0*EL*SA3*DBLE(SA1**INT(3.D0))*DB&
  &LE(SA2**INT(3.D0))*dMH12OSAlter())/(MW*SB*SW) + (0.0625D0*CA3*EL*SA1*dMH22OSAlter())/(CB*MW*SW) + (0.1875D0*CA12*CA3*EL*SA1*dM&
  &H22OSAlter())/(CB*MW*SW) + (0.0625D0*CA22*CA3*EL*SA1*dMH22OSAlter())/(CB*MW*SW) + (0.1875D0*CA12*CA22*CA3*EL*SA1*dMH22OSAlter(&
  &))/(CB*MW*SW) - (0.0625D0*CA3*EL*SA1*SA22*dMH22OSAlter())/(CB*MW*SW) - (0.1875D0*CA12*CA3*EL*SA1*SA22*dMH22OSAlter())/(CB*MW*S&
  &W) + (0.09375D0*CA1*EL*SA2*SA3*dMH22OSAlter())/(CB*MW*SW) + (0.28125D0*CA1*CA22*EL*SA2*SA3*dMH22OSAlter())/(CB*MW*SW) - (0.093&
  &75D0*CA1*EL*SA12*SA2*SA3*dMH22OSAlter())/(CB*MW*SW) - (0.28125D0*CA1*CA22*EL*SA12*SA2*SA3*dMH22OSAlter())/(CB*MW*SW) - (0.0625&
  &D0*CA1*CA3*EL*dMH22OSAlter())/(MW*SB*SW) - (0.0625D0*CA1*CA22*CA3*EL*dMH22OSAlter())/(MW*SB*SW) - (0.1875D0*CA1*CA3*EL*SA12*dM&
  &H22OSAlter())/(MW*SB*SW) - (0.1875D0*CA1*CA22*CA3*EL*SA12*dMH22OSAlter())/(MW*SB*SW) + (0.0625D0*CA1*CA3*EL*SA22*dMH22OSAlter(&
  &))/(MW*SB*SW) + (0.1875D0*CA1*CA3*EL*SA12*SA22*dMH22OSAlter())/(MW*SB*SW) + (0.09375D0*EL*SA1*SA2*SA3*dMH22OSAlter())/(MW*SB*S&
  &W) - (0.09375D0*CA12*EL*SA1*SA2*SA3*dMH22OSAlter())/(MW*SB*SW) + (0.28125D0*CA22*EL*SA1*SA2*SA3*dMH22OSAlter())/(MW*SB*SW) - (&
  &0.28125D0*CA12*CA22*EL*SA1*SA2*SA3*dMH22OSAlter())/(MW*SB*SW) - (0.25D0*CA2*SA3*dMH22OSAlter())/vS - (0.75D0*CA2*SA22*SA3*dMH2&
  &2OSAlter())/vS + (0.03125D0*EL*SA2*SA3*DBLE(CA1**INT(3.D0))*dMH22OSAlter())/(CB*MW*SW) + (0.09375D0*CA22*EL*SA2*SA3*DBLE(CA1**&
  &INT(3.D0))*dMH22OSAlter())/(CB*MW*SW) + (0.0625D0*CA3*EL*DBLE(CA1**INT(3.D0))*dMH22OSAlter())/(MW*SB*SW) + (0.0625D0*CA22*CA3*&
  &EL*DBLE(CA1**INT(3.D0))*dMH22OSAlter())/(MW*SB*SW) - (0.0625D0*CA3*EL*SA22*DBLE(CA1**INT(3.D0))*dMH22OSAlter())/(MW*SB*SW) + (&
  &0.25D0*SA3*DBLE(CA2**INT(3.D0))*dMH22OSAlter())/vS - (0.0625D0*CA3*EL*DBLE(SA1**INT(3.D0))*dMH22OSAlter())/(CB*MW*SW) - (0.062&
  &5D0*CA22*CA3*EL*DBLE(SA1**INT(3.D0))*dMH22OSAlter())/(CB*MW*SW) + (0.0625D0*CA3*EL*SA22*DBLE(SA1**INT(3.D0))*dMH22OSAlter())/(&
  &CB*MW*SW) + (0.03125D0*EL*SA2*SA3*DBLE(SA1**INT(3.D0))*dMH22OSAlter())/(MW*SB*SW) + (0.09375D0*CA22*EL*SA2*SA3*DBLE(SA1**INT(3&
  &.D0))*dMH22OSAlter())/(MW*SB*SW) - (0.09375D0*CA1*EL*SA3*DBLE(SA2**INT(3.D0))*dMH22OSAlter())/(CB*MW*SW) + (0.09375D0*CA1*EL*S&
  &A12*SA3*DBLE(SA2**INT(3.D0))*dMH22OSAlter())/(CB*MW*SW) - (0.09375D0*EL*SA1*SA3*DBLE(SA2**INT(3.D0))*dMH22OSAlter())/(MW*SB*SW&
  &) + (0.09375D0*CA12*EL*SA1*SA3*DBLE(SA2**INT(3.D0))*dMH22OSAlter())/(MW*SB*SW) - (0.03125D0*EL*SA3*DBLE(CA1**INT(3.D0))*DBLE(S&
  &A2**INT(3.D0))*dMH22OSAlter())/(CB*MW*SW) - (0.03125D0*EL*SA3*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dMH22OSAlter())/(MW*SB&
  &*SW) + (0.0625D0*CA1*CA3*EL*m12squared*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.0625D0*CA1*CA22*CA3*EL*m12squared*DBLE(M&
  &W**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.0625D0*CA3*EL*MH12*SA1*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.1875D0*CA12*CA3&
  &*EL*MH12*SA1*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.0625D0*CA22*CA3*EL*MH12*SA1*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*&
  &SW) - (0.1875D0*CA12*CA22*CA3*EL*MH12*SA1*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.03125D0*CA3*EL*MH22*SA1*DBLE(MW**INT(&
  &-3.D0))*dMW2Alter())/(CB*SW) - (0.09375D0*CA12*CA3*EL*MH22*SA1*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.03125D0*CA22*CA3&
  &*EL*MH22*SA1*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.09375D0*CA12*CA22*CA3*EL*MH22*SA1*DBLE(MW**INT(-3.D0))*dMW2Alter()&
  &)/(CB*SW) - (0.0625D0*CA1*CA3*EL*m12squared*SA22*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.0625D0*CA3*EL*MH12*SA1*SA22*DB&
  &LE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.1875D0*CA12*CA3*EL*MH12*SA1*SA22*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.0&
  &3125D0*CA3*EL*MH22*SA1*SA22*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.09375D0*CA12*CA3*EL*MH22*SA1*SA22*DBLE(MW**INT(-3.D&
  &0))*dMW2Alter())/(CB*SW) - (0.09375D0*CA1*EL*MH12*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.28125D0*CA1*CA22*EL*M&
  &H12*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.046875D0*CA1*EL*MH22*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*&
  &SW) - (0.140625D0*CA1*CA22*EL*MH22*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.140625D0*EL*m12squared*SA1*SA2*SA3*D&
  &BLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.421875D0*CA22*EL*m12squared*SA1*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW&
  &) + (0.09375D0*CA1*EL*MH12*SA12*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.28125D0*CA1*CA22*EL*MH12*SA12*SA2*SA3*D&
  &BLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.046875D0*CA1*EL*MH22*SA12*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0&
  &.140625D0*CA1*CA22*EL*MH22*SA12*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.0625D0*CA1*CA3*EL*MH12*DBLE(MW**INT(-3.&
  &D0))*dMW2Alter())/(SB*SW) + (0.0625D0*CA1*CA22*CA3*EL*MH12*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.03125D0*CA1*CA3*EL*M&
  &H22*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.03125D0*CA1*CA22*CA3*EL*MH22*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0&
  &.109375D0*CA3*EL*m12squared*SA1*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.109375D0*CA22*CA3*EL*m12squared*SA1*DBLE(MW**IN&
  &T(-3.D0))*dMW2Alter())/(SB*SW) + (0.078125D0*CA3*EL*m12squared*SA1*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB2*SB*SW) + (0.28125D0*&
  &CA12*CA3*EL*m12squared*SA1*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB2*SB*SW) + (0.078125D0*CA22*CA3*EL*m12squared*SA1*DBLE(MW**INT&
  &(-3.D0))*dMW2Alter())/(CB2*SB*SW) + (0.28125D0*CA12*CA22*CA3*EL*m12squared*SA1*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB2*SB*SW) +&
  & (0.1875D0*CA1*CA3*EL*MH12*SA12*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.1875D0*CA1*CA22*CA3*EL*MH12*SA12*DBLE(MW**INT(-&
  &3.D0))*dMW2Alter())/(SB*SW) + (0.09375D0*CA1*CA3*EL*MH22*SA12*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.09375D0*CA1*CA22*&
  &CA3*EL*MH22*SA12*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.0625D0*CA1*CA3*EL*MH12*SA22*DBLE(MW**INT(-3.D0))*dMW2Alter())/&
  &(SB*SW) - (0.03125D0*CA1*CA3*EL*MH22*SA22*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.109375D0*CA3*EL*m12squared*SA1*SA22*D&
  &BLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.078125D0*CA3*EL*m12squared*SA1*SA22*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB2*SB*SW&
  &) - (0.28125D0*CA12*CA3*EL*m12squared*SA1*SA22*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB2*SB*SW) - (0.1875D0*CA1*CA3*EL*MH12*SA12*&
  &SA22*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.09375D0*CA1*CA3*EL*MH22*SA12*SA22*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW&
  &) - (0.140625D0*CA1*EL*m12squared*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.421875D0*CA1*CA22*EL*m12squared*SA2*S&
  &A3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.09375D0*CA1*EL*m12squared*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB2*SB*&
  &SW) + (0.28125D0*CA1*CA22*EL*m12squared*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB2*SB*SW) - (0.09375D0*EL*MH12*SA1*SA2*SA3&
  &*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.09375D0*CA12*EL*MH12*SA1*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (&
  &0.28125D0*CA22*EL*MH12*SA1*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.28125D0*CA12*CA22*EL*MH12*SA1*SA2*SA3*DBLE(M&
  &W**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.046875D0*EL*MH22*SA1*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.046875D0*&
  &CA12*EL*MH22*SA1*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.140625D0*CA22*EL*MH22*SA1*SA2*SA3*DBLE(MW**INT(-3.D0))&
  &*dMW2Alter())/(SB*SW) + (0.140625D0*CA12*CA22*EL*MH22*SA1*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.140625D0*CA1*&
  &EL*m12squared*SA12*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB2*SB*SW) - (0.421875D0*CA1*CA22*EL*m12squared*SA12*SA2*SA3*DBL&
  &E(MW**INT(-3.D0))*dMW2Alter())/(CB2*SB*SW) - (0.03125D0*CA1*CA3*EL*m12squared*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SB2*SW) - &
  &(0.03125D0*CA1*CA22*CA3*EL*m12squared*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SB2*SW) - (0.28125D0*CA1*CA3*EL*m12squared*SA12*DB&
  &LE(MW**INT(-3.D0))*dMW2Alter())/(CB*SB2*SW) - (0.28125D0*CA1*CA22*CA3*EL*m12squared*SA12*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB&
  &*SB2*SW) + (0.03125D0*CA1*CA3*EL*m12squared*SA22*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SB2*SW) + (0.28125D0*CA1*CA3*EL*m12squa&
  &red*SA12*SA22*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SB2*SW) + (0.09375D0*EL*m12squared*SA1*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Al&
  &ter())/(CB*SB2*SW) - (0.140625D0*CA12*EL*m12squared*SA1*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SB2*SW) + (0.28125D0*CA2&
  &2*EL*m12squared*SA1*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SB2*SW) - (0.421875D0*CA12*CA22*EL*m12squared*SA1*SA2*SA3*DB&
  &LE(MW**INT(-3.D0))*dMW2Alter())/(CB*SB2*SW) - (0.0625D0*CA1*CA3*EL*m12squared*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW*TB) - (&
  &0.0625D0*CA1*CA22*CA3*EL*m12squared*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW*TB) + (0.0625D0*CA1*CA3*EL*m12squared*SA22*DBLE(M&
  &W**INT(-3.D0))*dMW2Alter())/(SB*SW*TB) + (0.046875D0*EL*m12squared*SA1*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW*TB) + &
  &(0.140625D0*CA22*EL*m12squared*SA1*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW*TB) + (0.015625D0*CA3*EL*m12squared*SA1*TB&
  &*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.015625D0*CA22*CA3*EL*m12squared*SA1*TB*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*S&
  &W) - (0.015625D0*CA3*EL*m12squared*SA1*SA22*TB*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.046875D0*CA1*EL*m12squared*SA2*S&
  &A3*TB*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.140625D0*CA1*CA22*EL*m12squared*SA2*SA3*TB*DBLE(MW**INT(-3.D0))*dMW2Alter&
  &())/(CB*SW) - (0.03125D0*EL*MH12*SA2*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.09375D0*CA22*EL*M&
  &H12*SA2*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.015625D0*EL*MH22*SA2*SA3*DBLE(CA1**INT(3.D0))*&
  &DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.046875D0*CA22*EL*MH22*SA2*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alt&
  &er())/(CB*SW) - (0.0625D0*CA3*EL*MH12*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.0625D0*CA22*CA3*EL*M&
  &H12*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.03125D0*CA3*EL*MH22*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(&
  &-3.D0))*dMW2Alter())/(SB*SW) - (0.03125D0*CA22*CA3*EL*MH22*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0&
  &.0625D0*CA3*EL*MH12*SA22*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.03125D0*CA3*EL*MH22*SA22*DBLE(CA1&
  &**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.046875D0*EL*m12squared*SA2*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-&
  &3.D0))*dMW2Alter())/(CB2*SB*SW) + (0.140625D0*CA22*EL*m12squared*SA2*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter()&
  &)/(CB2*SB*SW) + (0.09375D0*CA3*EL*m12squared*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SB2*SW) + (0.09375D0*C&
  &A22*CA3*EL*m12squared*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SB2*SW) - (0.09375D0*CA3*EL*m12squared*SA22*D&
  &BLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SB2*SW) + (0.0625D0*CA3*EL*MH12*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3&
  &.D0))*dMW2Alter())/(CB*SW) + (0.0625D0*CA22*CA3*EL*MH12*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(CB*SW) + (0.03&
  &125D0*CA3*EL*MH22*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(CB*SW) + (0.03125D0*CA22*CA3*EL*MH22*DBLE(MW**INT(-3&
  &.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(CB*SW) - (0.0625D0*CA3*EL*MH12*SA22*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alt&
  &er())/(CB*SW) - (0.03125D0*CA3*EL*MH22*SA22*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(CB*SW) - (0.09375D0*CA3*EL&
  &*m12squared*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(CB2*SB*SW) - (0.09375D0*CA22*CA3*EL*m12squared*DBLE(MW**IN&
  &T(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(CB2*SB*SW) + (0.09375D0*CA3*EL*m12squared*SA22*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT&
  &(3.D0))*dMW2Alter())/(CB2*SB*SW) - (0.03125D0*EL*MH12*SA2*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(SB*SW) -&
  & (0.09375D0*CA22*EL*MH12*SA2*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(SB*SW) - (0.015625D0*EL*MH22*SA2*SA3*&
  &DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(SB*SW) - (0.046875D0*CA22*EL*MH22*SA2*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA&
  &1**INT(3.D0))*dMW2Alter())/(SB*SW) + (0.046875D0*EL*m12squared*SA2*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/&
  &(CB*SB2*SW) + (0.140625D0*CA22*EL*m12squared*SA2*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(CB*SB2*SW) + (0.0&
  &9375D0*CA1*EL*MH12*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(CB*SW) + (0.046875D0*CA1*EL*MH22*SA3*DBLE(MW**I&
  &NT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(CB*SW) + (0.140625D0*EL*m12squared*SA1*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.&
  &D0))*dMW2Alter())/(CB*SW) - (0.09375D0*CA1*EL*MH12*SA12*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(CB*SW) - (&
  &0.046875D0*CA1*EL*MH22*SA12*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(CB*SW) + (0.140625D0*CA1*EL*m12squared&
  &*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(SB*SW) - (0.09375D0*CA1*EL*m12squared*SA3*DBLE(MW**INT(-3.D0))*DB&
  &LE(SA2**INT(3.D0))*dMW2Alter())/(CB2*SB*SW) + (0.09375D0*EL*MH12*SA1*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter()&
  &)/(SB*SW) - (0.09375D0*CA12*EL*MH12*SA1*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(SB*SW) + (0.046875D0*EL*MH&
  &22*SA1*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(SB*SW) - (0.046875D0*CA12*EL*MH22*SA1*SA3*DBLE(MW**INT(-3.D&
  &0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(SB*SW) + (0.140625D0*CA1*EL*m12squared*SA12*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0&
  &))*dMW2Alter())/(CB2*SB*SW) - (0.09375D0*EL*m12squared*SA1*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(CB*SB2*&
  &SW) + (0.140625D0*CA12*EL*m12squared*SA1*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(CB*SB2*SW) - (0.046875D0*&
  &EL*m12squared*SA1*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(SB*SW*TB) - (0.046875D0*CA1*EL*m12squared*SA3*TB&
  &*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(CB*SW) + (0.03125D0*EL*MH12*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.&
  &D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(CB*SW) + (0.015625D0*EL*MH22*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*DBLE(SA2**I&
  &NT(3.D0))*dMW2Alter())/(CB*SW) - (0.046875D0*EL*m12squared*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*&
  &dMW2Alter())/(CB2*SB*SW) + (0.03125D0*EL*MH12*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/&
  &(SB*SW) + (0.015625D0*EL*MH22*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(SB*SW) - (0.046&
  &875D0*EL*m12squared*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(CB*SB2*SW) + 0.5D0*CA2*MH&
  &12*SA3*DBLE(vS**INT(-2.D0))*dvSMSBarAlter() + 0.25D0*CA2*MH22*SA3*DBLE(vS**INT(-2.D0))*dvSMSBarAlter() + 1.5D0*CA2*MH12*SA22*S&
  &A3*DBLE(vS**INT(-2.D0))*dvSMSBarAlter() + 0.75D0*CA2*MH22*SA22*SA3*DBLE(vS**INT(-2.D0))*dvSMSBarAlter() - 0.5D0*MH12*SA3*DBLE(&
  &CA2**INT(3.D0))*DBLE(vS**INT(-2.D0))*dvSMSBarAlter() - 0.25D0*MH22*SA3*DBLE(CA2**INT(3.D0))*DBLE(vS**INT(-2.D0))*dvSMSBarAlter&
  &() &
		& + CS1S1S1f111*dZH1H2OSAlter()/2D0 + CS1S1S1f211*dZH2H2OS()/2D0 + CS1S1S1f311*dZH3H2OSAlter()/2D0 &
		& + CS1S1S1f121*dZH1H1OS()/2D0 + CS1S1S1f221*dZH2H1OSAlter()/2D0 + CS1S1S1f321*dZH3H1OSAlter()/2D0 &
		& + CS1S1S1f121*dZH1H1OS()/2D0 + CS1S1S1f221*dZH2H1OSAlter()/2D0 + CS1S1S1f321*dZH3H1OSAlter()/2D0 &
		& )
	case (7)
		dRR11 = -CA2*SA1*dAlpha1PinchOS() - CA1*SA2*dAlpha2PinchOS()
		dRR12 = CA1*CA2*dAlpha1PinchOS() - SA1*SA2*dAlpha2PinchOS()
		dRR13 = CA2*dAlpha2PinchOS()
		dRR21 = -CA1*CA3*dAlpha1PinchOS() - CA1*CA3*SA2*dAlpha3PinchOS() - CA1*CA2*SA3*dAlpha2PinchOS() + &
			& SA1*SA3*dAlpha3PinchOS() + SA1*SA2*SA3*dAlpha1PinchOS()
		dRR22 = -CA3*SA1*dAlpha1PinchOS() - CA3*SA1*SA2*dAlpha3PinchOS() - CA1*SA3*dAlpha3PinchOS() - &
			& CA2*SA1*SA3*dAlpha2PinchOS() - CA1*SA2*SA3*dAlpha1PinchOS()
		dRR23 = CA2*CA3*dAlpha3PinchOS() - SA2*SA3*dAlpha2PinchOS()
		dRR31 = -CA1*CA2*CA3*dAlpha2PinchOS() + CA3*SA1*dAlpha3PinchOS() + CA3*SA1*SA2*dAlpha1PinchOS() + &
			& CA1*SA3*dAlpha1PinchOS() + CA1*SA2*SA3*dAlpha3PinchOS()
		dRR32 = -CA1*CA3*dAlpha3PinchOS() - CA2*CA3*SA1*dAlpha2PinchOS() - CA1*CA3*SA2*dAlpha1PinchOS() + &
			& SA1*SA3*dAlpha1PinchOS() + SA1*SA2*SA3*dAlpha3PinchOS()
		dRR33 = -CA3*SA2*dAlpha2PinchOS() - CA2*SA3*dAlpha3PinchOS()

		totalAmplitude = CS1S1S1f211*( &
&(0.125D0*CA1*CA3*EL*MH12*dAlpha1PinchOS())/(CB*MW*SW) + (0.125D0*CA1*CA22*CA3*EL*MH12*dAlpha1PinchOS())/(CB*MW*SW) + (0.0625D0*CA&
  &1*CA3*EL*MH22*dAlpha1PinchOS())/(CB*MW*SW) + (0.0625D0*CA1*CA22*CA3*EL*MH22*dAlpha1PinchOS())/(CB*MW*SW) + (0.125D0*CA3*EL*m12&
  &squared*SA1*dAlpha1PinchOS())/(CB*MW*SW) + (0.125D0*CA22*CA3*EL*m12squared*SA1*dAlpha1PinchOS())/(CB*MW*SW) - (1.125D0*CA1*CA3&
  &*EL*MH12*SA12*dAlpha1PinchOS())/(CB*MW*SW) - (1.125D0*CA1*CA22*CA3*EL*MH12*SA12*dAlpha1PinchOS())/(CB*MW*SW) - (0.5625D0*CA1*C&
  &A3*EL*MH22*SA12*dAlpha1PinchOS())/(CB*MW*SW) - (0.5625D0*CA1*CA22*CA3*EL*MH22*SA12*dAlpha1PinchOS())/(CB*MW*SW) - (0.125D0*CA1&
  &*CA3*EL*MH12*SA22*dAlpha1PinchOS())/(CB*MW*SW) - (0.0625D0*CA1*CA3*EL*MH22*SA22*dAlpha1PinchOS())/(CB*MW*SW) - (0.125D0*CA3*EL&
  &*m12squared*SA1*SA22*dAlpha1PinchOS())/(CB*MW*SW) + (1.125D0*CA1*CA3*EL*MH12*SA12*SA22*dAlpha1PinchOS())/(CB*MW*SW) + (0.5625D&
  &0*CA1*CA3*EL*MH22*SA12*SA22*dAlpha1PinchOS())/(CB*MW*SW) + (0.28125D0*CA1*EL*m12squared*SA2*SA3*dAlpha1PinchOS())/(CB*MW*SW) +&
  & (0.84375D0*CA1*CA22*EL*m12squared*SA2*SA3*dAlpha1PinchOS())/(CB*MW*SW) - (0.1875D0*EL*MH12*SA1*SA2*SA3*dAlpha1PinchOS())/(CB*&
  &MW*SW) - (0.5625D0*CA12*EL*MH12*SA1*SA2*SA3*dAlpha1PinchOS())/(CB*MW*SW) - (0.5625D0*CA22*EL*MH12*SA1*SA2*SA3*dAlpha1PinchOS()&
  &)/(CB*MW*SW) - (1.6875D0*CA12*CA22*EL*MH12*SA1*SA2*SA3*dAlpha1PinchOS())/(CB*MW*SW) - (0.09375D0*EL*MH22*SA1*SA2*SA3*dAlpha1Pi&
  &nchOS())/(CB*MW*SW) - (0.28125D0*CA12*EL*MH22*SA1*SA2*SA3*dAlpha1PinchOS())/(CB*MW*SW) - (0.28125D0*CA22*EL*MH22*SA1*SA2*SA3*d&
  &Alpha1PinchOS())/(CB*MW*SW) - (0.84375D0*CA12*CA22*EL*MH22*SA1*SA2*SA3*dAlpha1PinchOS())/(CB*MW*SW) + (0.21875D0*CA1*CA3*EL*m1&
  &2squared*dAlpha1PinchOS())/(MW*SB*SW) + (0.21875D0*CA1*CA22*CA3*EL*m12squared*dAlpha1PinchOS())/(MW*SB*SW) - (0.15625D0*CA1*CA&
  &3*EL*m12squared*dAlpha1PinchOS())/(CB2*MW*SB*SW) - (0.15625D0*CA1*CA22*CA3*EL*m12squared*dAlpha1PinchOS())/(CB2*MW*SB*SW) + (0&
  &.125D0*CA3*EL*MH12*SA1*dAlpha1PinchOS())/(MW*SB*SW) - (1.125D0*CA12*CA3*EL*MH12*SA1*dAlpha1PinchOS())/(MW*SB*SW) + (0.125D0*CA&
  &22*CA3*EL*MH12*SA1*dAlpha1PinchOS())/(MW*SB*SW) - (1.125D0*CA12*CA22*CA3*EL*MH12*SA1*dAlpha1PinchOS())/(MW*SB*SW) + (0.0625D0*&
  &CA3*EL*MH22*SA1*dAlpha1PinchOS())/(MW*SB*SW) - (0.5625D0*CA12*CA3*EL*MH22*SA1*dAlpha1PinchOS())/(MW*SB*SW) + (0.0625D0*CA22*CA&
  &3*EL*MH22*SA1*dAlpha1PinchOS())/(MW*SB*SW) - (0.5625D0*CA12*CA22*CA3*EL*MH22*SA1*dAlpha1PinchOS())/(MW*SB*SW) + (1.6875D0*CA1*&
  &CA3*EL*m12squared*SA12*dAlpha1PinchOS())/(CB2*MW*SB*SW) + (1.6875D0*CA1*CA22*CA3*EL*m12squared*SA12*dAlpha1PinchOS())/(CB2*MW*&
  &SB*SW) - (0.21875D0*CA1*CA3*EL*m12squared*SA22*dAlpha1PinchOS())/(MW*SB*SW) + (0.15625D0*CA1*CA3*EL*m12squared*SA22*dAlpha1Pin&
  &chOS())/(CB2*MW*SB*SW) - (0.125D0*CA3*EL*MH12*SA1*SA22*dAlpha1PinchOS())/(MW*SB*SW) + (1.125D0*CA12*CA3*EL*MH12*SA1*SA22*dAlph&
  &a1PinchOS())/(MW*SB*SW) - (0.0625D0*CA3*EL*MH22*SA1*SA22*dAlpha1PinchOS())/(MW*SB*SW) + (0.5625D0*CA12*CA3*EL*MH22*SA1*SA22*dA&
  &lpha1PinchOS())/(MW*SB*SW) - (1.6875D0*CA1*CA3*EL*m12squared*SA12*SA22*dAlpha1PinchOS())/(CB2*MW*SB*SW) + (0.1875D0*CA1*EL*MH1&
  &2*SA2*SA3*dAlpha1PinchOS())/(MW*SB*SW) + (0.5625D0*CA1*CA22*EL*MH12*SA2*SA3*dAlpha1PinchOS())/(MW*SB*SW) + (0.09375D0*CA1*EL*M&
  &H22*SA2*SA3*dAlpha1PinchOS())/(MW*SB*SW) + (0.28125D0*CA1*CA22*EL*MH22*SA2*SA3*dAlpha1PinchOS())/(MW*SB*SW) - (0.28125D0*EL*m1&
  &2squared*SA1*SA2*SA3*dAlpha1PinchOS())/(MW*SB*SW) - (0.84375D0*CA22*EL*m12squared*SA1*SA2*SA3*dAlpha1PinchOS())/(MW*SB*SW) + (&
  &0.1875D0*EL*m12squared*SA1*SA2*SA3*dAlpha1PinchOS())/(CB2*MW*SB*SW) + (0.84375D0*CA12*EL*m12squared*SA1*SA2*SA3*dAlpha1PinchOS&
  &())/(CB2*MW*SB*SW) + (0.5625D0*CA22*EL*m12squared*SA1*SA2*SA3*dAlpha1PinchOS())/(CB2*MW*SB*SW) + (2.53125D0*CA12*CA22*EL*m12sq&
  &uared*SA1*SA2*SA3*dAlpha1PinchOS())/(CB2*MW*SB*SW) + (0.5625D0*CA1*EL*MH12*SA12*SA2*SA3*dAlpha1PinchOS())/(MW*SB*SW) + (1.6875&
  &D0*CA1*CA22*EL*MH12*SA12*SA2*SA3*dAlpha1PinchOS())/(MW*SB*SW) + (0.28125D0*CA1*EL*MH22*SA12*SA2*SA3*dAlpha1PinchOS())/(MW*SB*S&
  &W) + (0.84375D0*CA1*CA22*EL*MH22*SA12*SA2*SA3*dAlpha1PinchOS())/(MW*SB*SW) - (0.0625D0*CA3*EL*m12squared*SA1*dAlpha1PinchOS())&
  &/(CB*MW*SB2*SW) + (1.6875D0*CA12*CA3*EL*m12squared*SA1*dAlpha1PinchOS())/(CB*MW*SB2*SW) - (0.0625D0*CA22*CA3*EL*m12squared*SA1&
  &*dAlpha1PinchOS())/(CB*MW*SB2*SW) + (1.6875D0*CA12*CA22*CA3*EL*m12squared*SA1*dAlpha1PinchOS())/(CB*MW*SB2*SW) + (0.0625D0*CA3&
  &*EL*m12squared*SA1*SA22*dAlpha1PinchOS())/(CB*MW*SB2*SW) - (1.6875D0*CA12*CA3*EL*m12squared*SA1*SA22*dAlpha1PinchOS())/(CB*MW*&
  &SB2*SW) - (0.1875D0*CA1*EL*m12squared*SA2*SA3*dAlpha1PinchOS())/(CB*MW*SB2*SW) - (0.5625D0*CA1*CA22*EL*m12squared*SA2*SA3*dAlp&
  &ha1PinchOS())/(CB*MW*SB2*SW) - (0.84375D0*CA1*EL*m12squared*SA12*SA2*SA3*dAlpha1PinchOS())/(CB*MW*SB2*SW) - (2.53125D0*CA1*CA2&
  &2*EL*m12squared*SA12*SA2*SA3*dAlpha1PinchOS())/(CB*MW*SB2*SW) - (0.125D0*CA3*EL*m12squared*SA1*dAlpha1PinchOS())/(MW*SB*SW*TB)&
  & - (0.125D0*CA22*CA3*EL*m12squared*SA1*dAlpha1PinchOS())/(MW*SB*SW*TB) + (0.125D0*CA3*EL*m12squared*SA1*SA22*dAlpha1PinchOS())&
  &/(MW*SB*SW*TB) - (0.09375D0*CA1*EL*m12squared*SA2*SA3*dAlpha1PinchOS())/(MW*SB*SW*TB) - (0.28125D0*CA1*CA22*EL*m12squared*SA2*&
  &SA3*dAlpha1PinchOS())/(MW*SB*SW*TB) - (0.03125D0*CA1*CA3*EL*m12squared*TB*dAlpha1PinchOS())/(CB*MW*SW) - (0.03125D0*CA1*CA22*C&
  &A3*EL*m12squared*TB*dAlpha1PinchOS())/(CB*MW*SW) + (0.03125D0*CA1*CA3*EL*m12squared*SA22*TB*dAlpha1PinchOS())/(CB*MW*SW) + (0.&
  &09375D0*EL*m12squared*SA1*SA2*SA3*TB*dAlpha1PinchOS())/(CB*MW*SW) + (0.28125D0*CA22*EL*m12squared*SA1*SA2*SA3*TB*dAlpha1PinchO&
  &S())/(CB*MW*SW) + (0.5D0*CA1*CA2*CA3*EL*m12squared*SA2*dAlpha2PinchOS())/(CB*MW*SW) - (0.5D0*CA2*CA3*EL*MH12*SA1*SA2*dAlpha2Pi&
  &nchOS())/(CB*MW*SW) - (1.5D0*CA12*CA2*CA3*EL*MH12*SA1*SA2*dAlpha2PinchOS())/(CB*MW*SW) - (0.25D0*CA2*CA3*EL*MH22*SA1*SA2*dAlph&
  &a2PinchOS())/(CB*MW*SW) - (0.75D0*CA12*CA2*CA3*EL*MH22*SA1*SA2*dAlpha2PinchOS())/(CB*MW*SW) + (0.1875D0*CA1*CA2*EL*MH12*SA3*dA&
  &lpha2PinchOS())/(CB*MW*SW) + (0.09375D0*CA1*CA2*EL*MH22*SA3*dAlpha2PinchOS())/(CB*MW*SW) + (0.28125D0*CA2*EL*m12squared*SA1*SA&
  &3*dAlpha2PinchOS())/(CB*MW*SW) - (0.1875D0*CA1*CA2*EL*MH12*SA12*SA3*dAlpha2PinchOS())/(CB*MW*SW) - (0.09375D0*CA1*CA2*EL*MH22*&
  &SA12*SA3*dAlpha2PinchOS())/(CB*MW*SW) - (1.6875D0*CA1*CA2*EL*MH12*SA22*SA3*dAlpha2PinchOS())/(CB*MW*SW) - (0.84375D0*CA1*CA2*E&
  &L*MH22*SA22*SA3*dAlpha2PinchOS())/(CB*MW*SW) - (2.53125D0*CA2*EL*m12squared*SA1*SA22*SA3*dAlpha2PinchOS())/(CB*MW*SW) + (1.687&
  &5D0*CA1*CA2*EL*MH12*SA12*SA22*SA3*dAlpha2PinchOS())/(CB*MW*SW) + (0.84375D0*CA1*CA2*EL*MH22*SA12*SA22*SA3*dAlpha2PinchOS())/(C&
  &B*MW*SW) + (0.5D0*CA1*CA2*CA3*EL*MH12*SA2*dAlpha2PinchOS())/(MW*SB*SW) + (0.25D0*CA1*CA2*CA3*EL*MH22*SA2*dAlpha2PinchOS())/(MW&
  &*SB*SW) - (0.875D0*CA2*CA3*EL*m12squared*SA1*SA2*dAlpha2PinchOS())/(MW*SB*SW) + (0.625D0*CA2*CA3*EL*m12squared*SA1*SA2*dAlpha2&
  &PinchOS())/(CB2*MW*SB*SW) + (2.25D0*CA12*CA2*CA3*EL*m12squared*SA1*SA2*dAlpha2PinchOS())/(CB2*MW*SB*SW) + (1.5D0*CA1*CA2*CA3*E&
  &L*MH12*SA12*SA2*dAlpha2PinchOS())/(MW*SB*SW) + (0.75D0*CA1*CA2*CA3*EL*MH22*SA12*SA2*dAlpha2PinchOS())/(MW*SB*SW) + (0.28125D0*&
  &CA1*CA2*EL*m12squared*SA3*dAlpha2PinchOS())/(MW*SB*SW) - (0.1875D0*CA1*CA2*EL*m12squared*SA3*dAlpha2PinchOS())/(CB2*MW*SB*SW) &
  &+ (0.1875D0*CA2*EL*MH12*SA1*SA3*dAlpha2PinchOS())/(MW*SB*SW) - (0.1875D0*CA12*CA2*EL*MH12*SA1*SA3*dAlpha2PinchOS())/(MW*SB*SW)&
  & + (0.09375D0*CA2*EL*MH22*SA1*SA3*dAlpha2PinchOS())/(MW*SB*SW) - (0.09375D0*CA12*CA2*EL*MH22*SA1*SA3*dAlpha2PinchOS())/(MW*SB*&
  &SW) + (0.28125D0*CA1*CA2*EL*m12squared*SA12*SA3*dAlpha2PinchOS())/(CB2*MW*SB*SW) - (2.53125D0*CA1*CA2*EL*m12squared*SA22*SA3*d&
  &Alpha2PinchOS())/(MW*SB*SW) + (1.6875D0*CA1*CA2*EL*m12squared*SA22*SA3*dAlpha2PinchOS())/(CB2*MW*SB*SW) - (1.6875D0*CA2*EL*MH1&
  &2*SA1*SA22*SA3*dAlpha2PinchOS())/(MW*SB*SW) + (1.6875D0*CA12*CA2*EL*MH12*SA1*SA22*SA3*dAlpha2PinchOS())/(MW*SB*SW) - (0.84375D&
  &0*CA2*EL*MH22*SA1*SA22*SA3*dAlpha2PinchOS())/(MW*SB*SW) + (0.84375D0*CA12*CA2*EL*MH22*SA1*SA22*SA3*dAlpha2PinchOS())/(MW*SB*SW&
  &) - (2.53125D0*CA1*CA2*EL*m12squared*SA12*SA22*SA3*dAlpha2PinchOS())/(CB2*MW*SB*SW) - (0.25D0*CA1*CA2*CA3*EL*m12squared*SA2*dA&
  &lpha2PinchOS())/(CB*MW*SB2*SW) - (2.25D0*CA1*CA2*CA3*EL*m12squared*SA12*SA2*dAlpha2PinchOS())/(CB*MW*SB2*SW) - (0.1875D0*CA2*E&
  &L*m12squared*SA1*SA3*dAlpha2PinchOS())/(CB*MW*SB2*SW) + (0.28125D0*CA12*CA2*EL*m12squared*SA1*SA3*dAlpha2PinchOS())/(CB*MW*SB2&
  &*SW) + (1.6875D0*CA2*EL*m12squared*SA1*SA22*SA3*dAlpha2PinchOS())/(CB*MW*SB2*SW) - (2.53125D0*CA12*CA2*EL*m12squared*SA1*SA22*&
  &SA3*dAlpha2PinchOS())/(CB*MW*SB2*SW) - (0.5D0*CA1*CA2*CA3*EL*m12squared*SA2*dAlpha2PinchOS())/(MW*SB*SW*TB) - (0.09375D0*CA2*E&
  &L*m12squared*SA1*SA3*dAlpha2PinchOS())/(MW*SB*SW*TB) + (0.84375D0*CA2*EL*m12squared*SA1*SA22*SA3*dAlpha2PinchOS())/(MW*SB*SW*T&
  &B) + (0.125D0*CA2*CA3*EL*m12squared*SA1*SA2*TB*dAlpha2PinchOS())/(CB*MW*SW) - (0.09375D0*CA1*CA2*EL*m12squared*SA3*TB*dAlpha2P&
  &inchOS())/(CB*MW*SW) + (0.84375D0*CA1*CA2*EL*m12squared*SA22*SA3*TB*dAlpha2PinchOS())/(CB*MW*SW) + (0.5D0*MH12*SA2*SA3*dAlpha2&
  &PinchOS())/vS - (4.5D0*CA22*MH12*SA2*SA3*dAlpha2PinchOS())/vS + (0.25D0*MH22*SA2*SA3*dAlpha2PinchOS())/vS - (2.25D0*CA22*MH22*&
  &SA2*SA3*dAlpha2PinchOS())/vS + (0.1875D0*CA1*CA3*EL*MH12*SA2*dAlpha3PinchOS())/(CB*MW*SW) + (0.5625D0*CA1*CA22*CA3*EL*MH12*SA2&
  &*dAlpha3PinchOS())/(CB*MW*SW) + (0.09375D0*CA1*CA3*EL*MH22*SA2*dAlpha3PinchOS())/(CB*MW*SW) + (0.28125D0*CA1*CA22*CA3*EL*MH22*&
  &SA2*dAlpha3PinchOS())/(CB*MW*SW) + (0.28125D0*CA3*EL*m12squared*SA1*SA2*dAlpha3PinchOS())/(CB*MW*SW) + (0.84375D0*CA22*CA3*EL*&
  &m12squared*SA1*SA2*dAlpha3PinchOS())/(CB*MW*SW) - (0.1875D0*CA1*CA3*EL*MH12*SA12*SA2*dAlpha3PinchOS())/(CB*MW*SW) - (0.5625D0*&
  &CA1*CA22*CA3*EL*MH12*SA12*SA2*dAlpha3PinchOS())/(CB*MW*SW) - (0.09375D0*CA1*CA3*EL*MH22*SA12*SA2*dAlpha3PinchOS())/(CB*MW*SW) &
  &- (0.28125D0*CA1*CA22*CA3*EL*MH22*SA12*SA2*dAlpha3PinchOS())/(CB*MW*SW) + (0.125D0*CA1*EL*m12squared*SA3*dAlpha3PinchOS())/(CB&
  &*MW*SW) + (0.125D0*CA1*CA22*EL*m12squared*SA3*dAlpha3PinchOS())/(CB*MW*SW) - (0.125D0*EL*MH12*SA1*SA3*dAlpha3PinchOS())/(CB*MW&
  &*SW) - (0.375D0*CA12*EL*MH12*SA1*SA3*dAlpha3PinchOS())/(CB*MW*SW) - (0.125D0*CA22*EL*MH12*SA1*SA3*dAlpha3PinchOS())/(CB*MW*SW)&
  & - (0.375D0*CA12*CA22*EL*MH12*SA1*SA3*dAlpha3PinchOS())/(CB*MW*SW) - (0.0625D0*EL*MH22*SA1*SA3*dAlpha3PinchOS())/(CB*MW*SW) - &
  &(0.1875D0*CA12*EL*MH22*SA1*SA3*dAlpha3PinchOS())/(CB*MW*SW) - (0.0625D0*CA22*EL*MH22*SA1*SA3*dAlpha3PinchOS())/(CB*MW*SW) - (0&
  &.1875D0*CA12*CA22*EL*MH22*SA1*SA3*dAlpha3PinchOS())/(CB*MW*SW) - (0.125D0*CA1*EL*m12squared*SA22*SA3*dAlpha3PinchOS())/(CB*MW*&
  &SW) + (0.125D0*EL*MH12*SA1*SA22*SA3*dAlpha3PinchOS())/(CB*MW*SW) + (0.375D0*CA12*EL*MH12*SA1*SA22*SA3*dAlpha3PinchOS())/(CB*MW&
  &*SW) + (0.0625D0*EL*MH22*SA1*SA22*SA3*dAlpha3PinchOS())/(CB*MW*SW) + (0.1875D0*CA12*EL*MH22*SA1*SA22*SA3*dAlpha3PinchOS())/(CB&
  &*MW*SW) + (0.28125D0*CA1*CA3*EL*m12squared*SA2*dAlpha3PinchOS())/(MW*SB*SW) + (0.84375D0*CA1*CA22*CA3*EL*m12squared*SA2*dAlpha&
  &3PinchOS())/(MW*SB*SW) - (0.1875D0*CA1*CA3*EL*m12squared*SA2*dAlpha3PinchOS())/(CB2*MW*SB*SW) - (0.5625D0*CA1*CA22*CA3*EL*m12s&
  &quared*SA2*dAlpha3PinchOS())/(CB2*MW*SB*SW) + (0.1875D0*CA3*EL*MH12*SA1*SA2*dAlpha3PinchOS())/(MW*SB*SW) - (0.1875D0*CA12*CA3*&
  &EL*MH12*SA1*SA2*dAlpha3PinchOS())/(MW*SB*SW) + (0.5625D0*CA22*CA3*EL*MH12*SA1*SA2*dAlpha3PinchOS())/(MW*SB*SW) - (0.5625D0*CA1&
  &2*CA22*CA3*EL*MH12*SA1*SA2*dAlpha3PinchOS())/(MW*SB*SW) + (0.09375D0*CA3*EL*MH22*SA1*SA2*dAlpha3PinchOS())/(MW*SB*SW) - (0.093&
  &75D0*CA12*CA3*EL*MH22*SA1*SA2*dAlpha3PinchOS())/(MW*SB*SW) + (0.28125D0*CA22*CA3*EL*MH22*SA1*SA2*dAlpha3PinchOS())/(MW*SB*SW) &
  &- (0.28125D0*CA12*CA22*CA3*EL*MH22*SA1*SA2*dAlpha3PinchOS())/(MW*SB*SW) + (0.28125D0*CA1*CA3*EL*m12squared*SA12*SA2*dAlpha3Pin&
  &chOS())/(CB2*MW*SB*SW) + (0.84375D0*CA1*CA22*CA3*EL*m12squared*SA12*SA2*dAlpha3PinchOS())/(CB2*MW*SB*SW) + (0.125D0*CA1*EL*MH1&
  &2*SA3*dAlpha3PinchOS())/(MW*SB*SW) + (0.125D0*CA1*CA22*EL*MH12*SA3*dAlpha3PinchOS())/(MW*SB*SW) + (0.0625D0*CA1*EL*MH22*SA3*dA&
  &lpha3PinchOS())/(MW*SB*SW) + (0.0625D0*CA1*CA22*EL*MH22*SA3*dAlpha3PinchOS())/(MW*SB*SW) - (0.21875D0*EL*m12squared*SA1*SA3*dA&
  &lpha3PinchOS())/(MW*SB*SW) - (0.21875D0*CA22*EL*m12squared*SA1*SA3*dAlpha3PinchOS())/(MW*SB*SW) + (0.15625D0*EL*m12squared*SA1&
  &*SA3*dAlpha3PinchOS())/(CB2*MW*SB*SW) + (0.5625D0*CA12*EL*m12squared*SA1*SA3*dAlpha3PinchOS())/(CB2*MW*SB*SW) + (0.15625D0*CA2&
  &2*EL*m12squared*SA1*SA3*dAlpha3PinchOS())/(CB2*MW*SB*SW) + (0.5625D0*CA12*CA22*EL*m12squared*SA1*SA3*dAlpha3PinchOS())/(CB2*MW&
  &*SB*SW) + (0.375D0*CA1*EL*MH12*SA12*SA3*dAlpha3PinchOS())/(MW*SB*SW) + (0.375D0*CA1*CA22*EL*MH12*SA12*SA3*dAlpha3PinchOS())/(M&
  &W*SB*SW) + (0.1875D0*CA1*EL*MH22*SA12*SA3*dAlpha3PinchOS())/(MW*SB*SW) + (0.1875D0*CA1*CA22*EL*MH22*SA12*SA3*dAlpha3PinchOS())&
  &/(MW*SB*SW) - (0.125D0*CA1*EL*MH12*SA22*SA3*dAlpha3PinchOS())/(MW*SB*SW) - (0.0625D0*CA1*EL*MH22*SA22*SA3*dAlpha3PinchOS())/(M&
  &W*SB*SW) + (0.21875D0*EL*m12squared*SA1*SA22*SA3*dAlpha3PinchOS())/(MW*SB*SW) - (0.15625D0*EL*m12squared*SA1*SA22*SA3*dAlpha3P&
  &inchOS())/(CB2*MW*SB*SW) - (0.5625D0*CA12*EL*m12squared*SA1*SA22*SA3*dAlpha3PinchOS())/(CB2*MW*SB*SW) - (0.375D0*CA1*EL*MH12*S&
  &A12*SA22*SA3*dAlpha3PinchOS())/(MW*SB*SW) - (0.1875D0*CA1*EL*MH22*SA12*SA22*SA3*dAlpha3PinchOS())/(MW*SB*SW) - (0.1875D0*CA3*E&
  &L*m12squared*SA1*SA2*dAlpha3PinchOS())/(CB*MW*SB2*SW) + (0.28125D0*CA12*CA3*EL*m12squared*SA1*SA2*dAlpha3PinchOS())/(CB*MW*SB2&
  &*SW) - (0.5625D0*CA22*CA3*EL*m12squared*SA1*SA2*dAlpha3PinchOS())/(CB*MW*SB2*SW) + (0.84375D0*CA12*CA22*CA3*EL*m12squared*SA1*&
  &SA2*dAlpha3PinchOS())/(CB*MW*SB2*SW) - (0.0625D0*CA1*EL*m12squared*SA3*dAlpha3PinchOS())/(CB*MW*SB2*SW) - (0.0625D0*CA1*CA22*E&
  &L*m12squared*SA3*dAlpha3PinchOS())/(CB*MW*SB2*SW) - (0.5625D0*CA1*EL*m12squared*SA12*SA3*dAlpha3PinchOS())/(CB*MW*SB2*SW) - (0&
  &.5625D0*CA1*CA22*EL*m12squared*SA12*SA3*dAlpha3PinchOS())/(CB*MW*SB2*SW) + (0.0625D0*CA1*EL*m12squared*SA22*SA3*dAlpha3PinchOS&
  &())/(CB*MW*SB2*SW) + (0.5625D0*CA1*EL*m12squared*SA12*SA22*SA3*dAlpha3PinchOS())/(CB*MW*SB2*SW) - (0.09375D0*CA3*EL*m12squared&
  &*SA1*SA2*dAlpha3PinchOS())/(MW*SB*SW*TB) - (0.28125D0*CA22*CA3*EL*m12squared*SA1*SA2*dAlpha3PinchOS())/(MW*SB*SW*TB) - (0.125D&
  &0*CA1*EL*m12squared*SA3*dAlpha3PinchOS())/(MW*SB*SW*TB) - (0.125D0*CA1*CA22*EL*m12squared*SA3*dAlpha3PinchOS())/(MW*SB*SW*TB) &
  &+ (0.125D0*CA1*EL*m12squared*SA22*SA3*dAlpha3PinchOS())/(MW*SB*SW*TB) - (0.09375D0*CA1*CA3*EL*m12squared*SA2*TB*dAlpha3PinchOS&
  &())/(CB*MW*SW) - (0.28125D0*CA1*CA22*CA3*EL*m12squared*SA2*TB*dAlpha3PinchOS())/(CB*MW*SW) + (0.03125D0*EL*m12squared*SA1*SA3*&
  &TB*dAlpha3PinchOS())/(CB*MW*SW) + (0.03125D0*CA22*EL*m12squared*SA1*SA3*TB*dAlpha3PinchOS())/(CB*MW*SW) - (0.03125D0*EL*m12squ&
  &ared*SA1*SA22*SA3*TB*dAlpha3PinchOS())/(CB*MW*SW) - (0.5D0*CA2*CA3*MH12*dAlpha3PinchOS())/vS - (0.25D0*CA2*CA3*MH22*dAlpha3Pin&
  &chOS())/vS - (1.5D0*CA2*CA3*MH12*SA22*dAlpha3PinchOS())/vS - (0.75D0*CA2*CA3*MH22*SA22*dAlpha3PinchOS())/vS + (0.015625D0*CA3*&
  &EL*m12squared*SA1*dBeta1PinchOS())/(CB*MW*SW) + (0.015625D0*CA22*CA3*EL*m12squared*SA1*dBeta1PinchOS())/(CB*MW*SW) - (0.015625&
  &D0*CA3*EL*m12squared*SA1*SA22*dBeta1PinchOS())/(CB*MW*SW) + (0.09375D0*CA1*EL*m12squared*SA2*SA3*dBeta1PinchOS())/(CB*MW*SW) +&
  & (0.28125D0*CA1*CA22*EL*m12squared*SA2*SA3*dBeta1PinchOS())/(CB*MW*SW) - (0.0625D0*CA1*CA3*EL*m12squared*dBeta1PinchOS())/(MW*&
  &SB*SW) - (0.0625D0*CA1*CA22*CA3*EL*m12squared*dBeta1PinchOS())/(MW*SB*SW) + (0.0625D0*CA1*CA3*EL*m12squared*dBeta1PinchOS())/(&
  &CB2*MW*SB*SW) + (0.0625D0*CA1*CA22*CA3*EL*m12squared*dBeta1PinchOS())/(CB2*MW*SB*SW) - (0.0625D0*CA3*EL*MH12*SA1*dBeta1PinchOS&
  &())/(MW*SB*SW) - (0.1875D0*CA12*CA3*EL*MH12*SA1*dBeta1PinchOS())/(MW*SB*SW) - (0.0625D0*CA22*CA3*EL*MH12*SA1*dBeta1PinchOS())/&
  &(MW*SB*SW) - (0.1875D0*CA12*CA22*CA3*EL*MH12*SA1*dBeta1PinchOS())/(MW*SB*SW) + (0.0625D0*CA3*EL*MH12*SA1*dBeta1PinchOS())/(CB2&
  &*MW*SB*SW) + (0.1875D0*CA12*CA3*EL*MH12*SA1*dBeta1PinchOS())/(CB2*MW*SB*SW) + (0.0625D0*CA22*CA3*EL*MH12*SA1*dBeta1PinchOS())/&
  &(CB2*MW*SB*SW) + (0.1875D0*CA12*CA22*CA3*EL*MH12*SA1*dBeta1PinchOS())/(CB2*MW*SB*SW) - (0.03125D0*CA3*EL*MH22*SA1*dBeta1PinchO&
  &S())/(MW*SB*SW) - (0.09375D0*CA12*CA3*EL*MH22*SA1*dBeta1PinchOS())/(MW*SB*SW) - (0.03125D0*CA22*CA3*EL*MH22*SA1*dBeta1PinchOS(&
  &))/(MW*SB*SW) - (0.09375D0*CA12*CA22*CA3*EL*MH22*SA1*dBeta1PinchOS())/(MW*SB*SW) + (0.03125D0*CA3*EL*MH22*SA1*dBeta1PinchOS())&
  &/(CB2*MW*SB*SW) + (0.09375D0*CA12*CA3*EL*MH22*SA1*dBeta1PinchOS())/(CB2*MW*SB*SW) + (0.03125D0*CA22*CA3*EL*MH22*SA1*dBeta1Pinc&
  &hOS())/(CB2*MW*SB*SW) + (0.09375D0*CA12*CA22*CA3*EL*MH22*SA1*dBeta1PinchOS())/(CB2*MW*SB*SW) + (0.5625D0*CA1*CA3*EL*m12squared&
  &*SA12*dBeta1PinchOS())/(CB2*MW*SB*SW) + (0.5625D0*CA1*CA22*CA3*EL*m12squared*SA12*dBeta1PinchOS())/(CB2*MW*SB*SW) + (0.0625D0*&
  &CA1*CA3*EL*m12squared*SA22*dBeta1PinchOS())/(MW*SB*SW) - (0.0625D0*CA1*CA3*EL*m12squared*SA22*dBeta1PinchOS())/(CB2*MW*SB*SW) &
  &+ (0.0625D0*CA3*EL*MH12*SA1*SA22*dBeta1PinchOS())/(MW*SB*SW) + (0.1875D0*CA12*CA3*EL*MH12*SA1*SA22*dBeta1PinchOS())/(MW*SB*SW)&
  & - (0.0625D0*CA3*EL*MH12*SA1*SA22*dBeta1PinchOS())/(CB2*MW*SB*SW) - (0.1875D0*CA12*CA3*EL*MH12*SA1*SA22*dBeta1PinchOS())/(CB2*&
  &MW*SB*SW) + (0.03125D0*CA3*EL*MH22*SA1*SA22*dBeta1PinchOS())/(MW*SB*SW) + (0.09375D0*CA12*CA3*EL*MH22*SA1*SA22*dBeta1PinchOS()&
  &)/(MW*SB*SW) - (0.03125D0*CA3*EL*MH22*SA1*SA22*dBeta1PinchOS())/(CB2*MW*SB*SW) - (0.09375D0*CA12*CA3*EL*MH22*SA1*SA22*dBeta1Pi&
  &nchOS())/(CB2*MW*SB*SW) - (0.5625D0*CA1*CA3*EL*m12squared*SA12*SA22*dBeta1PinchOS())/(CB2*MW*SB*SW) - (0.09375D0*CA1*EL*MH12*S&
  &A2*SA3*dBeta1PinchOS())/(MW*SB*SW) - (0.28125D0*CA1*CA22*EL*MH12*SA2*SA3*dBeta1PinchOS())/(MW*SB*SW) + (0.09375D0*CA1*EL*MH12*&
  &SA2*SA3*dBeta1PinchOS())/(CB2*MW*SB*SW) + (0.28125D0*CA1*CA22*EL*MH12*SA2*SA3*dBeta1PinchOS())/(CB2*MW*SB*SW) - (0.046875D0*CA&
  &1*EL*MH22*SA2*SA3*dBeta1PinchOS())/(MW*SB*SW) - (0.140625D0*CA1*CA22*EL*MH22*SA2*SA3*dBeta1PinchOS())/(MW*SB*SW) + (0.046875D0&
  &*CA1*EL*MH22*SA2*SA3*dBeta1PinchOS())/(CB2*MW*SB*SW) + (0.140625D0*CA1*CA22*EL*MH22*SA2*SA3*dBeta1PinchOS())/(CB2*MW*SB*SW) - &
  &(0.140625D0*EL*m12squared*SA1*SA2*SA3*dBeta1PinchOS())/(MW*SB*SW) - (0.421875D0*CA22*EL*m12squared*SA1*SA2*SA3*dBeta1PinchOS()&
  &)/(MW*SB*SW) - (0.046875D0*EL*m12squared*SA1*SA2*SA3*dBeta1PinchOS())/(CB2*MW*SB*SW) + (0.28125D0*CA12*EL*m12squared*SA1*SA2*S&
  &A3*dBeta1PinchOS())/(CB2*MW*SB*SW) - (0.140625D0*CA22*EL*m12squared*SA1*SA2*SA3*dBeta1PinchOS())/(CB2*MW*SB*SW) + (0.84375D0*C&
  &A12*CA22*EL*m12squared*SA1*SA2*SA3*dBeta1PinchOS())/(CB2*MW*SB*SW) + (0.09375D0*CA1*EL*MH12*SA12*SA2*SA3*dBeta1PinchOS())/(MW*&
  &SB*SW) + (0.28125D0*CA1*CA22*EL*MH12*SA12*SA2*SA3*dBeta1PinchOS())/(MW*SB*SW) - (0.09375D0*CA1*EL*MH12*SA12*SA2*SA3*dBeta1Pinc&
  &hOS())/(CB2*MW*SB*SW) - (0.28125D0*CA1*CA22*EL*MH12*SA12*SA2*SA3*dBeta1PinchOS())/(CB2*MW*SB*SW) + (0.046875D0*CA1*EL*MH22*SA1&
  &2*SA2*SA3*dBeta1PinchOS())/(MW*SB*SW) + (0.140625D0*CA1*CA22*EL*MH22*SA12*SA2*SA3*dBeta1PinchOS())/(MW*SB*SW) - (0.046875D0*CA&
  &1*EL*MH22*SA12*SA2*SA3*dBeta1PinchOS())/(CB2*MW*SB*SW) - (0.140625D0*CA1*CA22*EL*MH22*SA12*SA2*SA3*dBeta1PinchOS())/(CB2*MW*SB&
  &*SW) + (0.203125D0*CA3*EL*m12squared*SA1*dBeta1PinchOS())/(CB*MW*SB2*SW) + (0.84375D0*CA12*CA3*EL*m12squared*SA1*dBeta1PinchOS&
  &())/(CB*MW*SB2*SW) + (0.203125D0*CA22*CA3*EL*m12squared*SA1*dBeta1PinchOS())/(CB*MW*SB2*SW) + (0.84375D0*CA12*CA22*CA3*EL*m12s&
  &quared*SA1*dBeta1PinchOS())/(CB*MW*SB2*SW) - (0.203125D0*CA3*EL*m12squared*SA1*SA22*dBeta1PinchOS())/(CB*MW*SB2*SW) - (0.84375&
  &D0*CA12*CA3*EL*m12squared*SA1*SA22*dBeta1PinchOS())/(CB*MW*SB2*SW) + (0.10546875D0*CA1*EL*m12squared*SA2*SA3*dBeta1PinchOS())/&
  &(CB*MW*SB2*SW) + (0.31640625D0*CA1*CA22*EL*m12squared*SA2*SA3*dBeta1PinchOS())/(CB*MW*SB2*SW) - (0.38671875D0*CA1*EL*m12square&
  &d*SA12*SA2*SA3*dBeta1PinchOS())/(CB*MW*SB2*SW) - (1.16015625D0*CA1*CA22*EL*m12squared*SA12*SA2*SA3*dBeta1PinchOS())/(CB*MW*SB2&
  &*SW) + (0.125D0*CA1*CA3*EL*MH12*dBeta1PinchOS())/(MW*SB*SW*TB) + (0.125D0*CA1*CA22*CA3*EL*MH12*dBeta1PinchOS())/(MW*SB*SW*TB) &
  &+ (0.0625D0*CA1*CA3*EL*MH22*dBeta1PinchOS())/(MW*SB*SW*TB) + (0.0625D0*CA1*CA22*CA3*EL*MH22*dBeta1PinchOS())/(MW*SB*SW*TB) - (&
  &0.203125D0*CA3*EL*m12squared*SA1*dBeta1PinchOS())/(MW*SB*SW*TB) - (0.203125D0*CA22*CA3*EL*m12squared*SA1*dBeta1PinchOS())/(MW*&
  &SB*SW*TB) + (0.375D0*CA1*CA3*EL*MH12*SA12*dBeta1PinchOS())/(MW*SB*SW*TB) + (0.375D0*CA1*CA22*CA3*EL*MH12*SA12*dBeta1PinchOS())&
  &/(MW*SB*SW*TB) + (0.1875D0*CA1*CA3*EL*MH22*SA12*dBeta1PinchOS())/(MW*SB*SW*TB) + (0.1875D0*CA1*CA22*CA3*EL*MH22*SA12*dBeta1Pin&
  &chOS())/(MW*SB*SW*TB) - (0.125D0*CA1*CA3*EL*MH12*SA22*dBeta1PinchOS())/(MW*SB*SW*TB) - (0.0625D0*CA1*CA3*EL*MH22*SA22*dBeta1Pi&
  &nchOS())/(MW*SB*SW*TB) + (0.203125D0*CA3*EL*m12squared*SA1*SA22*dBeta1PinchOS())/(MW*SB*SW*TB) - (0.375D0*CA1*CA3*EL*MH12*SA12&
  &*SA22*dBeta1PinchOS())/(MW*SB*SW*TB) - (0.1875D0*CA1*CA3*EL*MH22*SA12*SA22*dBeta1PinchOS())/(MW*SB*SW*TB) - (0.140625D0*CA1*EL&
  &*m12squared*SA2*SA3*dBeta1PinchOS())/(MW*SB*SW*TB) - (0.421875D0*CA1*CA22*EL*m12squared*SA2*SA3*dBeta1PinchOS())/(MW*SB*SW*TB)&
  & - (0.1875D0*EL*MH12*SA1*SA2*SA3*dBeta1PinchOS())/(MW*SB*SW*TB) + (0.1875D0*CA12*EL*MH12*SA1*SA2*SA3*dBeta1PinchOS())/(MW*SB*S&
  &W*TB) - (0.5625D0*CA22*EL*MH12*SA1*SA2*SA3*dBeta1PinchOS())/(MW*SB*SW*TB) + (0.5625D0*CA12*CA22*EL*MH12*SA1*SA2*SA3*dBeta1Pinc&
  &hOS())/(MW*SB*SW*TB) - (0.09375D0*EL*MH22*SA1*SA2*SA3*dBeta1PinchOS())/(MW*SB*SW*TB) + (0.09375D0*CA12*EL*MH22*SA1*SA2*SA3*dBe&
  &ta1PinchOS())/(MW*SB*SW*TB) - (0.28125D0*CA22*EL*MH22*SA1*SA2*SA3*dBeta1PinchOS())/(MW*SB*SW*TB) + (0.28125D0*CA12*CA22*EL*MH2&
  &2*SA1*SA2*SA3*dBeta1PinchOS())/(MW*SB*SW*TB) - (0.125D0*CA1*CA3*EL*m12squared*TB*dBeta1PinchOS())/(CB*MW*SW) - (0.125D0*CA1*CA&
  &22*CA3*EL*m12squared*TB*dBeta1PinchOS())/(CB*MW*SW) + (0.0625D0*CA3*EL*MH12*SA1*TB*dBeta1PinchOS())/(CB*MW*SW) + (0.1875D0*CA1&
  &2*CA3*EL*MH12*SA1*TB*dBeta1PinchOS())/(CB*MW*SW) + (0.0625D0*CA22*CA3*EL*MH12*SA1*TB*dBeta1PinchOS())/(CB*MW*SW) + (0.1875D0*C&
  &A12*CA22*CA3*EL*MH12*SA1*TB*dBeta1PinchOS())/(CB*MW*SW) + (0.03125D0*CA3*EL*MH22*SA1*TB*dBeta1PinchOS())/(CB*MW*SW) + (0.09375&
  &D0*CA12*CA3*EL*MH22*SA1*TB*dBeta1PinchOS())/(CB*MW*SW) + (0.03125D0*CA22*CA3*EL*MH22*SA1*TB*dBeta1PinchOS())/(CB*MW*SW) + (0.0&
  &9375D0*CA12*CA22*CA3*EL*MH22*SA1*TB*dBeta1PinchOS())/(CB*MW*SW) + (0.125D0*CA1*CA3*EL*m12squared*SA22*TB*dBeta1PinchOS())/(CB*&
  &MW*SW) - (0.0625D0*CA3*EL*MH12*SA1*SA22*TB*dBeta1PinchOS())/(CB*MW*SW) - (0.1875D0*CA12*CA3*EL*MH12*SA1*SA22*TB*dBeta1PinchOS(&
  &))/(CB*MW*SW) - (0.03125D0*CA3*EL*MH22*SA1*SA22*TB*dBeta1PinchOS())/(CB*MW*SW) - (0.09375D0*CA12*CA3*EL*MH22*SA1*SA22*TB*dBeta&
  &1PinchOS())/(CB*MW*SW) + (0.09375D0*CA1*EL*MH12*SA2*SA3*TB*dBeta1PinchOS())/(CB*MW*SW) + (0.28125D0*CA1*CA22*EL*MH12*SA2*SA3*T&
  &B*dBeta1PinchOS())/(CB*MW*SW) + (0.046875D0*CA1*EL*MH22*SA2*SA3*TB*dBeta1PinchOS())/(CB*MW*SW) + (0.140625D0*CA1*CA22*EL*MH22*&
  &SA2*SA3*TB*dBeta1PinchOS())/(CB*MW*SW) + (0.140625D0*EL*m12squared*SA1*SA2*SA3*TB*dBeta1PinchOS())/(CB*MW*SW) + (0.421875D0*CA&
  &22*EL*m12squared*SA1*SA2*SA3*TB*dBeta1PinchOS())/(CB*MW*SW) - (0.09375D0*CA1*EL*MH12*SA12*SA2*SA3*TB*dBeta1PinchOS())/(CB*MW*S&
  &W) - (0.28125D0*CA1*CA22*EL*MH12*SA12*SA2*SA3*TB*dBeta1PinchOS())/(CB*MW*SW) - (0.046875D0*CA1*EL*MH22*SA12*SA2*SA3*TB*dBeta1P&
  &inchOS())/(CB*MW*SW) - (0.140625D0*CA1*CA22*EL*MH22*SA12*SA2*SA3*TB*dBeta1PinchOS())/(CB*MW*SW) - (0.1875D0*CA1*CA3*EL*m12squa&
  &red*dBeta1PinchOS())/(MW*SB*SW*TB2) - (0.1875D0*CA1*CA22*CA3*EL*m12squared*dBeta1PinchOS())/(MW*SB*SW*TB2) + (0.1875D0*CA1*CA3&
  &*EL*m12squared*SA22*dBeta1PinchOS())/(MW*SB*SW*TB2) + (0.09375D0*EL*m12squared*SA1*SA2*SA3*dBeta1PinchOS())/(MW*SB*SW*TB2) + (&
  &0.28125D0*CA22*EL*m12squared*SA1*SA2*SA3*dBeta1PinchOS())/(MW*SB*SW*TB2) - (0.03125D0*CA3*EL*m12squared*SA1*TB2*dBeta1PinchOS(&
  &))/(CB*MW*SW) - (0.03125D0*CA22*CA3*EL*m12squared*SA1*TB2*dBeta1PinchOS())/(CB*MW*SW) + (0.03125D0*CA3*EL*m12squared*SA1*SA22*&
  &TB2*dBeta1PinchOS())/(CB*MW*SW) - (0.140625D0*CA1*EL*m12squared*SA2*SA3*TB2*dBeta1PinchOS())/(CB*MW*SW) - (0.421875D0*CA1*CA22&
  &*EL*m12squared*SA2*SA3*TB2*dBeta1PinchOS())/(CB*MW*SW) + (0.375D0*CA3*EL*MH12*dAlpha1PinchOS()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW&
  &) + (0.375D0*CA22*CA3*EL*MH12*dAlpha1PinchOS()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.1875D0*CA3*EL*MH22*dAlpha1PinchOS()*DBLE(&
  &CA1**INT(3.D0)))/(CB*MW*SW) + (0.1875D0*CA22*CA3*EL*MH22*dAlpha1PinchOS()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.375D0*CA3*EL*M&
  &H12*SA22*dAlpha1PinchOS()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.1875D0*CA3*EL*MH22*SA22*dAlpha1PinchOS()*DBLE(CA1**INT(3.D0)))&
  &/(CB*MW*SW) - (0.5625D0*CA3*EL*m12squared*dAlpha1PinchOS()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.5625D0*CA22*CA3*EL*m12squ&
  &ared*dAlpha1PinchOS()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) + (0.5625D0*CA3*EL*m12squared*SA22*dAlpha1PinchOS()*DBLE(CA1**INT(3&
  &.D0)))/(CB2*MW*SB*SW) - (0.1875D0*EL*MH12*SA2*SA3*dAlpha1PinchOS()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.5625D0*CA22*EL*MH12*S&
  &A2*SA3*dAlpha1PinchOS()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.09375D0*EL*MH22*SA2*SA3*dAlpha1PinchOS()*DBLE(CA1**INT(3.D0)))/(&
  &MW*SB*SW) - (0.28125D0*CA22*EL*MH22*SA2*SA3*dAlpha1PinchOS()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) + (0.28125D0*EL*m12squared*SA2*S&
  &A3*dAlpha1PinchOS()*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.84375D0*CA22*EL*m12squared*SA2*SA3*dAlpha1PinchOS()*DBLE(CA1**IN&
  &T(3.D0)))/(CB*MW*SB2*SW) + (0.0625D0*CA2*EL*MH12*SA3*dAlpha2PinchOS()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.03125D0*CA2*EL*MH2&
  &2*SA3*dAlpha2PinchOS()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.5625D0*CA2*EL*MH12*SA22*SA3*dAlpha2PinchOS()*DBLE(CA1**INT(3.D0))&
  &)/(CB*MW*SW) - (0.28125D0*CA2*EL*MH22*SA22*SA3*dAlpha2PinchOS()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.5D0*CA2*CA3*EL*MH12*SA2*&
  &dAlpha2PinchOS()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.25D0*CA2*CA3*EL*MH22*SA2*dAlpha2PinchOS()*DBLE(CA1**INT(3.D0)))/(MW*SB*&
  &SW) - (0.09375D0*CA2*EL*m12squared*SA3*dAlpha2PinchOS()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) + (0.84375D0*CA2*EL*m12squared*SA&
  &22*SA3*dAlpha2PinchOS()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) + (0.75D0*CA2*CA3*EL*m12squared*SA2*dAlpha2PinchOS()*DBLE(CA1**IN&
  &T(3.D0)))/(CB*MW*SB2*SW) + (0.0625D0*CA3*EL*MH12*SA2*dAlpha3PinchOS()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.1875D0*CA22*CA3*EL&
  &*MH12*SA2*dAlpha3PinchOS()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.03125D0*CA3*EL*MH22*SA2*dAlpha3PinchOS()*DBLE(CA1**INT(3.D0))&
  &)/(CB*MW*SW) + (0.09375D0*CA22*CA3*EL*MH22*SA2*dAlpha3PinchOS()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.09375D0*CA3*EL*m12square&
  &d*SA2*dAlpha3PinchOS()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.28125D0*CA22*CA3*EL*m12squared*SA2*dAlpha3PinchOS()*DBLE(CA1*&
  &*INT(3.D0)))/(CB2*MW*SB*SW) - (0.125D0*EL*MH12*SA3*dAlpha3PinchOS()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.125D0*CA22*EL*MH12*S&
  &A3*dAlpha3PinchOS()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.0625D0*EL*MH22*SA3*dAlpha3PinchOS()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW)&
  & - (0.0625D0*CA22*EL*MH22*SA3*dAlpha3PinchOS()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) + (0.125D0*EL*MH12*SA22*SA3*dAlpha3PinchOS()*D&
  &BLE(CA1**INT(3.D0)))/(MW*SB*SW) + (0.0625D0*EL*MH22*SA22*SA3*dAlpha3PinchOS()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) + (0.1875D0*EL*&
  &m12squared*SA3*dAlpha3PinchOS()*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.1875D0*CA22*EL*m12squared*SA3*dAlpha3PinchOS()*DBLE(&
  &CA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.1875D0*EL*m12squared*SA22*SA3*dAlpha3PinchOS()*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.&
  &1875D0*CA3*EL*m12squared*dBeta1PinchOS()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.1875D0*CA22*CA3*EL*m12squared*dBeta1PinchOS&
  &()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) + (0.1875D0*CA3*EL*m12squared*SA22*dBeta1PinchOS()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW&
  &) - (0.03125D0*EL*MH12*SA2*SA3*dBeta1PinchOS()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.09375D0*CA22*EL*MH12*SA2*SA3*dBeta1PinchO&
  &S()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) + (0.03125D0*EL*MH12*SA2*SA3*dBeta1PinchOS()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) + (0.09&
  &375D0*CA22*EL*MH12*SA2*SA3*dBeta1PinchOS()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.015625D0*EL*MH22*SA2*SA3*dBeta1PinchOS()*&
  &DBLE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.046875D0*CA22*EL*MH22*SA2*SA3*dBeta1PinchOS()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) + (0.0156&
  &25D0*EL*MH22*SA2*SA3*dBeta1PinchOS()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) + (0.046875D0*CA22*EL*MH22*SA2*SA3*dBeta1PinchOS()*D&
  &BLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) + (0.12890625D0*EL*m12squared*SA2*SA3*dBeta1PinchOS()*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW)&
  & + (0.38671875D0*CA22*EL*m12squared*SA2*SA3*dBeta1PinchOS()*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.125D0*CA3*EL*MH12*dBeta1&
  &PinchOS()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW*TB) - (0.125D0*CA22*CA3*EL*MH12*dBeta1PinchOS()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW*TB) &
  &- (0.0625D0*CA3*EL*MH22*dBeta1PinchOS()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW*TB) - (0.0625D0*CA22*CA3*EL*MH22*dBeta1PinchOS()*DBLE(&
  &CA1**INT(3.D0)))/(MW*SB*SW*TB) + (0.125D0*CA3*EL*MH12*SA22*dBeta1PinchOS()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW*TB) + (0.0625D0*CA3&
  &*EL*MH22*SA22*dBeta1PinchOS()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW*TB) + (0.03125D0*EL*MH12*SA2*SA3*TB*dBeta1PinchOS()*DBLE(CA1**IN&
  &T(3.D0)))/(CB*MW*SW) + (0.09375D0*CA22*EL*MH12*SA2*SA3*TB*dBeta1PinchOS()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.015625D0*EL*MH&
  &22*SA2*SA3*TB*dBeta1PinchOS()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.046875D0*CA22*EL*MH22*SA2*SA3*TB*dBeta1PinchOS()*DBLE(CA1*&
  &*INT(3.D0)))/(CB*MW*SW) + (0.5625D0*CA1*EL*MH12*SA3*dAlpha2PinchOS()*DBLE(CA2**INT(3.D0)))/(CB*MW*SW) + (0.28125D0*CA1*EL*MH22&
  &*SA3*dAlpha2PinchOS()*DBLE(CA2**INT(3.D0)))/(CB*MW*SW) + (0.84375D0*EL*m12squared*SA1*SA3*dAlpha2PinchOS()*DBLE(CA2**INT(3.D0)&
  &))/(CB*MW*SW) - (0.5625D0*CA1*EL*MH12*SA12*SA3*dAlpha2PinchOS()*DBLE(CA2**INT(3.D0)))/(CB*MW*SW) - (0.28125D0*CA1*EL*MH22*SA12&
  &*SA3*dAlpha2PinchOS()*DBLE(CA2**INT(3.D0)))/(CB*MW*SW) + (0.84375D0*CA1*EL*m12squared*SA3*dAlpha2PinchOS()*DBLE(CA2**INT(3.D0)&
  &))/(MW*SB*SW) - (0.5625D0*CA1*EL*m12squared*SA3*dAlpha2PinchOS()*DBLE(CA2**INT(3.D0)))/(CB2*MW*SB*SW) + (0.5625D0*EL*MH12*SA1*&
  &SA3*dAlpha2PinchOS()*DBLE(CA2**INT(3.D0)))/(MW*SB*SW) - (0.5625D0*CA12*EL*MH12*SA1*SA3*dAlpha2PinchOS()*DBLE(CA2**INT(3.D0)))/&
  &(MW*SB*SW) + (0.28125D0*EL*MH22*SA1*SA3*dAlpha2PinchOS()*DBLE(CA2**INT(3.D0)))/(MW*SB*SW) - (0.28125D0*CA12*EL*MH22*SA1*SA3*dA&
  &lpha2PinchOS()*DBLE(CA2**INT(3.D0)))/(MW*SB*SW) + (0.84375D0*CA1*EL*m12squared*SA12*SA3*dAlpha2PinchOS()*DBLE(CA2**INT(3.D0)))&
  &/(CB2*MW*SB*SW) - (0.5625D0*EL*m12squared*SA1*SA3*dAlpha2PinchOS()*DBLE(CA2**INT(3.D0)))/(CB*MW*SB2*SW) + (0.84375D0*CA12*EL*m&
  &12squared*SA1*SA3*dAlpha2PinchOS()*DBLE(CA2**INT(3.D0)))/(CB*MW*SB2*SW) - (0.28125D0*EL*m12squared*SA1*SA3*dAlpha2PinchOS()*DB&
  &LE(CA2**INT(3.D0)))/(MW*SB*SW*TB) - (0.28125D0*CA1*EL*m12squared*SA3*TB*dAlpha2PinchOS()*DBLE(CA2**INT(3.D0)))/(CB*MW*SW) + (0&
  &.5D0*CA3*MH12*dAlpha3PinchOS()*DBLE(CA2**INT(3.D0)))/vS + (0.25D0*CA3*MH22*dAlpha3PinchOS()*DBLE(CA2**INT(3.D0)))/vS + (0.1875&
  &D0*EL*MH12*SA3*dAlpha2PinchOS()*DBLE(CA1**INT(3.D0))*DBLE(CA2**INT(3.D0)))/(CB*MW*SW) + (0.09375D0*EL*MH22*SA3*dAlpha2PinchOS(&
  &)*DBLE(CA1**INT(3.D0))*DBLE(CA2**INT(3.D0)))/(CB*MW*SW) - (0.28125D0*EL*m12squared*SA3*dAlpha2PinchOS()*DBLE(CA1**INT(3.D0))*D&
  &BLE(CA2**INT(3.D0)))/(CB2*MW*SB*SW) - (0.28125D0*CA3*EL*m12squared*SA1*dBeta1PinchOS()*DBLE(CB**INT(-3.D0)))/(MW*SW) - (0.8437&
  &5D0*CA12*CA3*EL*m12squared*SA1*dBeta1PinchOS()*DBLE(CB**INT(-3.D0)))/(MW*SW) - (0.28125D0*CA22*CA3*EL*m12squared*SA1*dBeta1Pin&
  &chOS()*DBLE(CB**INT(-3.D0)))/(MW*SW) - (0.84375D0*CA12*CA22*CA3*EL*m12squared*SA1*dBeta1PinchOS()*DBLE(CB**INT(-3.D0)))/(MW*SW&
  &) + (0.28125D0*CA3*EL*m12squared*SA1*SA22*dBeta1PinchOS()*DBLE(CB**INT(-3.D0)))/(MW*SW) + (0.84375D0*CA12*CA3*EL*m12squared*SA&
  &1*SA22*dBeta1PinchOS()*DBLE(CB**INT(-3.D0)))/(MW*SW) - (0.36328125D0*CA1*EL*m12squared*SA2*SA3*dBeta1PinchOS()*DBLE(CB**INT(-3&
  &.D0)))/(MW*SW) - (1.08984375D0*CA1*CA22*EL*m12squared*SA2*SA3*dBeta1PinchOS()*DBLE(CB**INT(-3.D0)))/(MW*SW) + (0.45703125D0*CA&
  &1*EL*m12squared*SA12*SA2*SA3*dBeta1PinchOS()*DBLE(CB**INT(-3.D0)))/(MW*SW) + (1.37109375D0*CA1*CA22*EL*m12squared*SA12*SA2*SA3&
  &*dBeta1PinchOS()*DBLE(CB**INT(-3.D0)))/(MW*SW) - (0.0625D0*CA3*EL*m12squared*SA1*dBeta1PinchOS()*DBLE(CB**INT(-3.D0)))/(MW*SB2&
  &*SW) - (0.28125D0*CA12*CA3*EL*m12squared*SA1*dBeta1PinchOS()*DBLE(CB**INT(-3.D0)))/(MW*SB2*SW) - (0.0625D0*CA22*CA3*EL*m12squa&
  &red*SA1*dBeta1PinchOS()*DBLE(CB**INT(-3.D0)))/(MW*SB2*SW) - (0.28125D0*CA12*CA22*CA3*EL*m12squared*SA1*dBeta1PinchOS()*DBLE(CB&
  &**INT(-3.D0)))/(MW*SB2*SW) + (0.0625D0*CA3*EL*m12squared*SA1*SA22*dBeta1PinchOS()*DBLE(CB**INT(-3.D0)))/(MW*SB2*SW) + (0.28125&
  &D0*CA12*CA3*EL*m12squared*SA1*SA22*dBeta1PinchOS()*DBLE(CB**INT(-3.D0)))/(MW*SB2*SW) - (0.05859375D0*CA1*EL*m12squared*SA2*SA3&
  &*dBeta1PinchOS()*DBLE(CB**INT(-3.D0)))/(MW*SB2*SW) - (0.17578125D0*CA1*CA22*EL*m12squared*SA2*SA3*dBeta1PinchOS()*DBLE(CB**INT&
  &(-3.D0)))/(MW*SB2*SW) + (0.10546875D0*CA1*EL*m12squared*SA12*SA2*SA3*dBeta1PinchOS()*DBLE(CB**INT(-3.D0)))/(MW*SB2*SW) + (0.31&
  &640625D0*CA1*CA22*EL*m12squared*SA12*SA2*SA3*dBeta1PinchOS()*DBLE(CB**INT(-3.D0)))/(MW*SB2*SW) - (0.15234375D0*EL*m12squared*S&
  &A2*SA3*dBeta1PinchOS()*DBLE(CA1**INT(3.D0))*DBLE(CB**INT(-3.D0)))/(MW*SW) - (0.45703125D0*CA22*EL*m12squared*SA2*SA3*dBeta1Pin&
  &chOS()*DBLE(CA1**INT(3.D0))*DBLE(CB**INT(-3.D0)))/(MW*SW) - (0.03515625D0*EL*m12squared*SA2*SA3*dBeta1PinchOS()*DBLE(CA1**INT(&
  &3.D0))*DBLE(CB**INT(-3.D0)))/(MW*SB2*SW) - (0.10546875D0*CA22*EL*m12squared*SA2*SA3*dBeta1PinchOS()*DBLE(CA1**INT(3.D0))*DBLE(&
  &CB**INT(-3.D0)))/(MW*SB2*SW) + (0.1875D0*EL*MH12*SA2*SA3*dAlpha1PinchOS()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.5625D0*CA22*EL&
  &*MH12*SA2*SA3*dAlpha1PinchOS()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.09375D0*EL*MH22*SA2*SA3*dAlpha1PinchOS()*DBLE(SA1**INT(3.&
  &D0)))/(CB*MW*SW) + (0.28125D0*CA22*EL*MH22*SA2*SA3*dAlpha1PinchOS()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.375D0*CA3*EL*MH12*dA&
  &lpha1PinchOS()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.375D0*CA22*CA3*EL*MH12*dAlpha1PinchOS()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) &
  &+ (0.1875D0*CA3*EL*MH22*dAlpha1PinchOS()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.1875D0*CA22*CA3*EL*MH22*dAlpha1PinchOS()*DBLE(S&
  &A1**INT(3.D0)))/(MW*SB*SW) - (0.375D0*CA3*EL*MH12*SA22*dAlpha1PinchOS()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0.1875D0*CA3*EL*MH&
  &22*SA22*dAlpha1PinchOS()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0.28125D0*EL*m12squared*SA2*SA3*dAlpha1PinchOS()*DBLE(SA1**INT(3.&
  &D0)))/(CB2*MW*SB*SW) - (0.84375D0*CA22*EL*m12squared*SA2*SA3*dAlpha1PinchOS()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.5625D0&
  &*CA3*EL*m12squared*dAlpha1PinchOS()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.5625D0*CA22*CA3*EL*m12squared*dAlpha1PinchOS()*D&
  &BLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.5625D0*CA3*EL*m12squared*SA22*dAlpha1PinchOS()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) +&
  & (0.5D0*CA2*CA3*EL*MH12*SA2*dAlpha2PinchOS()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.25D0*CA2*CA3*EL*MH22*SA2*dAlpha2PinchOS()*D&
  &BLE(SA1**INT(3.D0)))/(CB*MW*SW) - (0.75D0*CA2*CA3*EL*m12squared*SA2*dAlpha2PinchOS()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) + (0&
  &.0625D0*CA2*EL*MH12*SA3*dAlpha2PinchOS()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.03125D0*CA2*EL*MH22*SA3*dAlpha2PinchOS()*DBLE(S&
  &A1**INT(3.D0)))/(MW*SB*SW) - (0.5625D0*CA2*EL*MH12*SA22*SA3*dAlpha2PinchOS()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0.28125D0*CA2&
  &*EL*MH22*SA22*SA3*dAlpha2PinchOS()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0.09375D0*CA2*EL*m12squared*SA3*dAlpha2PinchOS()*DBLE(S&
  &A1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.84375D0*CA2*EL*m12squared*SA22*SA3*dAlpha2PinchOS()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) +&
  & (0.125D0*EL*MH12*SA3*dAlpha3PinchOS()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.125D0*CA22*EL*MH12*SA3*dAlpha3PinchOS()*DBLE(SA1*&
  &*INT(3.D0)))/(CB*MW*SW) + (0.0625D0*EL*MH22*SA3*dAlpha3PinchOS()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.0625D0*CA22*EL*MH22*SA3&
  &*dAlpha3PinchOS()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) - (0.125D0*EL*MH12*SA22*SA3*dAlpha3PinchOS()*DBLE(SA1**INT(3.D0)))/(CB*MW*S&
  &W) - (0.0625D0*EL*MH22*SA22*SA3*dAlpha3PinchOS()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.0625D0*CA3*EL*MH12*SA2*dAlpha3PinchOS()&
  &*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.1875D0*CA22*CA3*EL*MH12*SA2*dAlpha3PinchOS()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.0312&
  &5D0*CA3*EL*MH22*SA2*dAlpha3PinchOS()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.09375D0*CA22*CA3*EL*MH22*SA2*dAlpha3PinchOS()*DBLE(&
  &SA1**INT(3.D0)))/(MW*SB*SW) - (0.1875D0*EL*m12squared*SA3*dAlpha3PinchOS()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.1875D0*CA&
  &22*EL*m12squared*SA3*dAlpha3PinchOS()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) + (0.1875D0*EL*m12squared*SA22*SA3*dAlpha3PinchOS()&
  &*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.09375D0*CA3*EL*m12squared*SA2*dAlpha3PinchOS()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW)&
  & - (0.28125D0*CA22*CA3*EL*m12squared*SA2*dAlpha3PinchOS()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.0625D0*CA3*EL*MH12*dBeta1P&
  &inchOS()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.0625D0*CA22*CA3*EL*MH12*dBeta1PinchOS()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0.0&
  &625D0*CA3*EL*MH12*dBeta1PinchOS()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.0625D0*CA22*CA3*EL*MH12*dBeta1PinchOS()*DBLE(SA1**&
  &INT(3.D0)))/(CB2*MW*SB*SW) + (0.03125D0*CA3*EL*MH22*dBeta1PinchOS()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.03125D0*CA22*CA3*EL*&
  &MH22*dBeta1PinchOS()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0.03125D0*CA3*EL*MH22*dBeta1PinchOS()*DBLE(SA1**INT(3.D0)))/(CB2*MW*S&
  &B*SW) - (0.03125D0*CA22*CA3*EL*MH22*dBeta1PinchOS()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.0625D0*CA3*EL*MH12*SA22*dBeta1Pi&
  &nchOS()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.0625D0*CA3*EL*MH12*SA22*dBeta1PinchOS()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) - (&
  &0.03125D0*CA3*EL*MH22*SA22*dBeta1PinchOS()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.03125D0*CA3*EL*MH22*SA22*dBeta1PinchOS()*DBLE&
  &(SA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.09375D0*EL*m12squared*SA2*SA3*dBeta1PinchOS()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.&
  &28125D0*CA22*EL*m12squared*SA2*SA3*dBeta1PinchOS()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.28125D0*CA3*EL*m12squared*dBeta1P&
  &inchOS()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.28125D0*CA22*CA3*EL*m12squared*dBeta1PinchOS()*DBLE(SA1**INT(3.D0)))/(CB*MW&
  &*SB2*SW) + (0.28125D0*CA3*EL*m12squared*SA22*dBeta1PinchOS()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.0625D0*EL*MH12*SA2*SA3*&
  &dBeta1PinchOS()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW*TB) - (0.1875D0*CA22*EL*MH12*SA2*SA3*dBeta1PinchOS()*DBLE(SA1**INT(3.D0)))/(MW&
  &*SB*SW*TB) - (0.03125D0*EL*MH22*SA2*SA3*dBeta1PinchOS()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW*TB) - (0.09375D0*CA22*EL*MH22*SA2*SA3*&
  &dBeta1PinchOS()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW*TB) - (0.0625D0*CA3*EL*MH12*TB*dBeta1PinchOS()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW&
  &) - (0.0625D0*CA22*CA3*EL*MH12*TB*dBeta1PinchOS()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) - (0.03125D0*CA3*EL*MH22*TB*dBeta1PinchOS()&
  &*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) - (0.03125D0*CA22*CA3*EL*MH22*TB*dBeta1PinchOS()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.0625D&
  &0*CA3*EL*MH12*SA22*TB*dBeta1PinchOS()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.03125D0*CA3*EL*MH22*SA22*TB*dBeta1PinchOS()*DBLE(S&
  &A1**INT(3.D0)))/(CB*MW*SW) + (0.1875D0*EL*MH12*SA3*dAlpha2PinchOS()*DBLE(CA2**INT(3.D0))*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) + (0&
  &.09375D0*EL*MH22*SA3*dAlpha2PinchOS()*DBLE(CA2**INT(3.D0))*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0.28125D0*EL*m12squared*SA3*dAl&
  &pha2PinchOS()*DBLE(CA2**INT(3.D0))*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.28125D0*CA3*EL*m12squared*dBeta1PinchOS()*DBLE(CB&
  &**INT(-3.D0))*DBLE(SA1**INT(3.D0)))/(MW*SW) + (0.28125D0*CA22*CA3*EL*m12squared*dBeta1PinchOS()*DBLE(CB**INT(-3.D0))*DBLE(SA1*&
  &*INT(3.D0)))/(MW*SW) - (0.28125D0*CA3*EL*m12squared*SA22*dBeta1PinchOS()*DBLE(CB**INT(-3.D0))*DBLE(SA1**INT(3.D0)))/(MW*SW) + &
  &(0.09375D0*CA3*EL*m12squared*dBeta1PinchOS()*DBLE(CB**INT(-3.D0))*DBLE(SA1**INT(3.D0)))/(MW*SB2*SW) + (0.09375D0*CA22*CA3*EL*m&
  &12squared*dBeta1PinchOS()*DBLE(CB**INT(-3.D0))*DBLE(SA1**INT(3.D0)))/(MW*SB2*SW) - (0.09375D0*CA3*EL*m12squared*SA22*dBeta1Pin&
  &chOS()*DBLE(CB**INT(-3.D0))*DBLE(SA1**INT(3.D0)))/(MW*SB2*SW) - (0.28125D0*CA1*EL*m12squared*SA3*dAlpha1PinchOS()*DBLE(SA2**IN&
  &T(3.D0)))/(CB*MW*SW) + (0.1875D0*EL*MH12*SA1*SA3*dAlpha1PinchOS()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.5625D0*CA12*EL*MH12*SA&
  &1*SA3*dAlpha1PinchOS()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.09375D0*EL*MH22*SA1*SA3*dAlpha1PinchOS()*DBLE(SA2**INT(3.D0)))/(C&
  &B*MW*SW) + (0.28125D0*CA12*EL*MH22*SA1*SA3*dAlpha1PinchOS()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.1875D0*CA1*EL*MH12*SA3*dAlph&
  &a1PinchOS()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.09375D0*CA1*EL*MH22*SA3*dAlpha1PinchOS()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) + &
  &(0.28125D0*EL*m12squared*SA1*SA3*dAlpha1PinchOS()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.1875D0*EL*m12squared*SA1*SA3*dAlpha1Pi&
  &nchOS()*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) - (0.84375D0*CA12*EL*m12squared*SA1*SA3*dAlpha1PinchOS()*DBLE(SA2**INT(3.D0)))/(C&
  &B2*MW*SB*SW) - (0.5625D0*CA1*EL*MH12*SA12*SA3*dAlpha1PinchOS()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.28125D0*CA1*EL*MH22*SA12*&
  &SA3*dAlpha1PinchOS()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) + (0.1875D0*CA1*EL*m12squared*SA3*dAlpha1PinchOS()*DBLE(SA2**INT(3.D0)))&
  &/(CB*MW*SB2*SW) + (0.84375D0*CA1*EL*m12squared*SA12*SA3*dAlpha1PinchOS()*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) + (0.09375D0*CA1&
  &*EL*m12squared*SA3*dAlpha1PinchOS()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) - (0.09375D0*EL*m12squared*SA1*SA3*TB*dAlpha1PinchOS()&
  &*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (1.5D0*MH12*SA3*dAlpha2PinchOS()*DBLE(SA2**INT(3.D0)))/vS + (0.75D0*MH22*SA3*dAlpha2PinchO&
  &S()*DBLE(SA2**INT(3.D0)))/vS - (0.1875D0*CA1*CA3*EL*MH12*dAlpha3PinchOS()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.09375D0*CA1*CA&
  &3*EL*MH22*dAlpha3PinchOS()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.28125D0*CA3*EL*m12squared*SA1*dAlpha3PinchOS()*DBLE(SA2**INT(&
  &3.D0)))/(CB*MW*SW) + (0.1875D0*CA1*CA3*EL*MH12*SA12*dAlpha3PinchOS()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.09375D0*CA1*CA3*EL*&
  &MH22*SA12*dAlpha3PinchOS()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.28125D0*CA1*CA3*EL*m12squared*dAlpha3PinchOS()*DBLE(SA2**INT(&
  &3.D0)))/(MW*SB*SW) + (0.1875D0*CA1*CA3*EL*m12squared*dAlpha3PinchOS()*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) - (0.1875D0*CA3*EL*&
  &MH12*SA1*dAlpha3PinchOS()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) + (0.1875D0*CA12*CA3*EL*MH12*SA1*dAlpha3PinchOS()*DBLE(SA2**INT(3.D&
  &0)))/(MW*SB*SW) - (0.09375D0*CA3*EL*MH22*SA1*dAlpha3PinchOS()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) + (0.09375D0*CA12*CA3*EL*MH22*S&
  &A1*dAlpha3PinchOS()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.28125D0*CA1*CA3*EL*m12squared*SA12*dAlpha3PinchOS()*DBLE(SA2**INT(3.&
  &D0)))/(CB2*MW*SB*SW) + (0.1875D0*CA3*EL*m12squared*SA1*dAlpha3PinchOS()*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) - (0.28125D0*CA12&
  &*CA3*EL*m12squared*SA1*dAlpha3PinchOS()*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) + (0.09375D0*CA3*EL*m12squared*SA1*dAlpha3PinchOS&
  &()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) + (0.09375D0*CA1*CA3*EL*m12squared*TB*dAlpha3PinchOS()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW)&
  & - (0.09375D0*CA1*EL*m12squared*SA3*dBeta1PinchOS()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.09375D0*CA1*EL*MH12*SA3*dBeta1PinchO&
  &S()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.09375D0*CA1*EL*MH12*SA3*dBeta1PinchOS()*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) + (0.04&
  &6875D0*CA1*EL*MH22*SA3*dBeta1PinchOS()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.046875D0*CA1*EL*MH22*SA3*dBeta1PinchOS()*DBLE(SA2&
  &**INT(3.D0)))/(CB2*MW*SB*SW) + (0.140625D0*EL*m12squared*SA1*SA3*dBeta1PinchOS()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) + (0.046875D&
  &0*EL*m12squared*SA1*SA3*dBeta1PinchOS()*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) - (0.28125D0*CA12*EL*m12squared*SA1*SA3*dBeta1Pin&
  &chOS()*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) - (0.09375D0*CA1*EL*MH12*SA12*SA3*dBeta1PinchOS()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW)&
  & + (0.09375D0*CA1*EL*MH12*SA12*SA3*dBeta1PinchOS()*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) - (0.046875D0*CA1*EL*MH22*SA12*SA3*dBe&
  &ta1PinchOS()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) + (0.046875D0*CA1*EL*MH22*SA12*SA3*dBeta1PinchOS()*DBLE(SA2**INT(3.D0)))/(CB2*MW&
  &*SB*SW) - (0.10546875D0*CA1*EL*m12squared*SA3*dBeta1PinchOS()*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) + (0.38671875D0*CA1*EL*m12s&
  &quared*SA12*SA3*dBeta1PinchOS()*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) + (0.140625D0*CA1*EL*m12squared*SA3*dBeta1PinchOS()*DBLE(&
  &SA2**INT(3.D0)))/(MW*SB*SW*TB) + (0.1875D0*EL*MH12*SA1*SA3*dBeta1PinchOS()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) - (0.1875D0*CA1&
  &2*EL*MH12*SA1*SA3*dBeta1PinchOS()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) + (0.09375D0*EL*MH22*SA1*SA3*dBeta1PinchOS()*DBLE(SA2**I&
  &NT(3.D0)))/(MW*SB*SW*TB) - (0.09375D0*CA12*EL*MH22*SA1*SA3*dBeta1PinchOS()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) - (0.09375D0*CA&
  &1*EL*MH12*SA3*TB*dBeta1PinchOS()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.046875D0*CA1*EL*MH22*SA3*TB*dBeta1PinchOS()*DBLE(SA2**I&
  &NT(3.D0)))/(CB*MW*SW) - (0.140625D0*EL*m12squared*SA1*SA3*TB*dBeta1PinchOS()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.09375D0*CA1&
  &*EL*MH12*SA12*SA3*TB*dBeta1PinchOS()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.046875D0*CA1*EL*MH22*SA12*SA3*TB*dBeta1PinchOS()*DB&
  &LE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.09375D0*EL*m12squared*SA1*SA3*dBeta1PinchOS()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB2) + (0.14&
  &0625D0*CA1*EL*m12squared*SA3*TB2*dBeta1PinchOS()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.1875D0*EL*MH12*SA3*dAlpha1PinchOS()*DBL&
  &E(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) + (0.09375D0*EL*MH22*SA3*dAlpha1PinchOS()*DBLE(CA1**INT(3.D0))*DBLE(SA2**IN&
  &T(3.D0)))/(MW*SB*SW) - (0.28125D0*EL*m12squared*SA3*dAlpha1PinchOS()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW)&
  & - (0.0625D0*CA3*EL*MH12*dAlpha3PinchOS()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.03125D0*CA3*EL*MH22*dAlph&
  &a3PinchOS()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.09375D0*CA3*EL*m12squared*dAlpha3PinchOS()*DBLE(CA1**IN&
  &T(3.D0))*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) + (0.03125D0*EL*MH12*SA3*dBeta1PinchOS()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0&
  &)))/(MW*SB*SW) - (0.03125D0*EL*MH12*SA3*dBeta1PinchOS()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) + (0.015625D&
  &0*EL*MH22*SA3*dBeta1PinchOS()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.015625D0*EL*MH22*SA3*dBeta1PinchOS()*&
  &DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) - (0.12890625D0*EL*m12squared*SA3*dBeta1PinchOS()*DBLE(CA1**INT(3.D0&
  &))*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) - (0.03125D0*EL*MH12*SA3*TB*dBeta1PinchOS()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))&
  &/(CB*MW*SW) - (0.015625D0*EL*MH22*SA3*TB*dBeta1PinchOS()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.36328125D0&
  &*CA1*EL*m12squared*SA3*dBeta1PinchOS()*DBLE(CB**INT(-3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SW) - (0.45703125D0*CA1*EL*m12squared*SA&
  &12*SA3*dBeta1PinchOS()*DBLE(CB**INT(-3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SW) + (0.05859375D0*CA1*EL*m12squared*SA3*dBeta1PinchOS(&
  &)*DBLE(CB**INT(-3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SB2*SW) - (0.10546875D0*CA1*EL*m12squared*SA12*SA3*dBeta1PinchOS()*DBLE(CB**I&
  &NT(-3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SB2*SW) + (0.15234375D0*EL*m12squared*SA3*dBeta1PinchOS()*DBLE(CA1**INT(3.D0))*DBLE(CB**I&
  &NT(-3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SW) + (0.03515625D0*EL*m12squared*SA3*dBeta1PinchOS()*DBLE(CA1**INT(3.D0))*DBLE(CB**INT(-&
  &3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SB2*SW) - (0.1875D0*EL*MH12*SA3*dAlpha1PinchOS()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(&
  &CB*MW*SW) - (0.09375D0*EL*MH22*SA3*dAlpha1PinchOS()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.28125D0*EL*m12s&
  &quared*SA3*dAlpha1PinchOS()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) - (0.0625D0*CA3*EL*MH12*dAlpha3PinchOS()&
  &*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.03125D0*CA3*EL*MH22*dAlpha3PinchOS()*DBLE(SA1**INT(3.D0))*DBLE(SA2&
  &**INT(3.D0)))/(MW*SB*SW) + (0.09375D0*CA3*EL*m12squared*dAlpha3PinchOS()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2&
  &*SW) + (0.09375D0*EL*m12squared*SA3*dBeta1PinchOS()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) + (0.0625D0*EL*M&
  &H12*SA3*dBeta1PinchOS()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) + (0.03125D0*EL*MH22*SA3*dBeta1PinchOS()*DBLE&
  &(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) - (0.1875D0*CA1*CA3*EL*m12squared*dBeta1PinchOS()*DBLE(SB**INT(-3.D0)))/(&
  &MW*SW) - (0.1875D0*CA1*CA22*CA3*EL*m12squared*dBeta1PinchOS()*DBLE(SB**INT(-3.D0)))/(MW*SW) - (1.125D0*CA1*CA3*EL*m12squared*S&
  &A12*dBeta1PinchOS()*DBLE(SB**INT(-3.D0)))/(MW*SW) - (1.125D0*CA1*CA22*CA3*EL*m12squared*SA12*dBeta1PinchOS()*DBLE(SB**INT(-3.D&
  &0)))/(MW*SW) + (0.1875D0*CA1*CA3*EL*m12squared*SA22*dBeta1PinchOS()*DBLE(SB**INT(-3.D0)))/(MW*SW) + (1.125D0*CA1*CA3*EL*m12squ&
  &ared*SA12*SA22*dBeta1PinchOS()*DBLE(SB**INT(-3.D0)))/(MW*SW) + (0.46875D0*EL*m12squared*SA1*SA2*SA3*dBeta1PinchOS()*DBLE(SB**I&
  &NT(-3.D0)))/(MW*SW) - (0.5625D0*CA12*EL*m12squared*SA1*SA2*SA3*dBeta1PinchOS()*DBLE(SB**INT(-3.D0)))/(MW*SW) + (1.40625D0*CA22&
  &*EL*m12squared*SA1*SA2*SA3*dBeta1PinchOS()*DBLE(SB**INT(-3.D0)))/(MW*SW) - (1.6875D0*CA12*CA22*EL*m12squared*SA1*SA2*SA3*dBeta&
  &1PinchOS()*DBLE(SB**INT(-3.D0)))/(MW*SW) + (0.375D0*CA3*EL*m12squared*dBeta1PinchOS()*DBLE(CA1**INT(3.D0))*DBLE(SB**INT(-3.D0)&
  &))/(MW*SW) + (0.375D0*CA22*CA3*EL*m12squared*dBeta1PinchOS()*DBLE(CA1**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(MW*SW) - (0.375D0*CA3&
  &*EL*m12squared*SA22*dBeta1PinchOS()*DBLE(CA1**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(MW*SW) + (0.1875D0*EL*m12squared*SA2*SA3*dBeta&
  &1PinchOS()*DBLE(SA1**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(MW*SW) + (0.5625D0*CA22*EL*m12squared*SA2*SA3*dBeta1PinchOS()*DBLE(SA1*&
  &*INT(3.D0))*DBLE(SB**INT(-3.D0)))/(MW*SW) - (0.46875D0*EL*m12squared*SA1*SA3*dBeta1PinchOS()*DBLE(SA2**INT(3.D0))*DBLE(SB**INT&
  &(-3.D0)))/(MW*SW) + (0.5625D0*CA12*EL*m12squared*SA1*SA3*dBeta1PinchOS()*DBLE(SA2**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(MW*SW) - &
  &(0.1875D0*EL*m12squared*SA3*dBeta1PinchOS()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(MW*SW) - (0.125D0&
  &*CA1*CA3*m12squared*dgAtMZ())/(CB*MW) - (0.125D0*CA1*CA22*CA3*m12squared*dgAtMZ())/(CB*MW) + (0.125D0*CA3*MH12*SA1*dgAtMZ())/(&
  &CB*MW) + (0.375D0*CA12*CA3*MH12*SA1*dgAtMZ())/(CB*MW) + (0.125D0*CA22*CA3*MH12*SA1*dgAtMZ())/(CB*MW) + (0.375D0*CA12*CA22*CA3*&
  &MH12*SA1*dgAtMZ())/(CB*MW) + (0.0625D0*CA3*MH22*SA1*dgAtMZ())/(CB*MW) + (0.1875D0*CA12*CA3*MH22*SA1*dgAtMZ())/(CB*MW) + (0.062&
  &5D0*CA22*CA3*MH22*SA1*dgAtMZ())/(CB*MW) + (0.1875D0*CA12*CA22*CA3*MH22*SA1*dgAtMZ())/(CB*MW) + (0.125D0*CA1*CA3*m12squared*SA2&
  &2*dgAtMZ())/(CB*MW) - (0.125D0*CA3*MH12*SA1*SA22*dgAtMZ())/(CB*MW) - (0.375D0*CA12*CA3*MH12*SA1*SA22*dgAtMZ())/(CB*MW) - (0.06&
  &25D0*CA3*MH22*SA1*SA22*dgAtMZ())/(CB*MW) - (0.1875D0*CA12*CA3*MH22*SA1*SA22*dgAtMZ())/(CB*MW) + (0.1875D0*CA1*MH12*SA2*SA3*dgA&
  &tMZ())/(CB*MW) + (0.5625D0*CA1*CA22*MH12*SA2*SA3*dgAtMZ())/(CB*MW) + (0.09375D0*CA1*MH22*SA2*SA3*dgAtMZ())/(CB*MW) + (0.28125D&
  &0*CA1*CA22*MH22*SA2*SA3*dgAtMZ())/(CB*MW) + (0.28125D0*m12squared*SA1*SA2*SA3*dgAtMZ())/(CB*MW) + (0.84375D0*CA22*m12squared*S&
  &A1*SA2*SA3*dgAtMZ())/(CB*MW) - (0.1875D0*CA1*MH12*SA12*SA2*SA3*dgAtMZ())/(CB*MW) - (0.5625D0*CA1*CA22*MH12*SA12*SA2*SA3*dgAtMZ&
  &())/(CB*MW) - (0.09375D0*CA1*MH22*SA12*SA2*SA3*dgAtMZ())/(CB*MW) - (0.28125D0*CA1*CA22*MH22*SA12*SA2*SA3*dgAtMZ())/(CB*MW) - (&
  &0.125D0*CA1*CA3*MH12*dgAtMZ())/(MW*SB) - (0.125D0*CA1*CA22*CA3*MH12*dgAtMZ())/(MW*SB) - (0.0625D0*CA1*CA3*MH22*dgAtMZ())/(MW*S&
  &B) - (0.0625D0*CA1*CA22*CA3*MH22*dgAtMZ())/(MW*SB) + (0.21875D0*CA3*m12squared*SA1*dgAtMZ())/(MW*SB) + (0.21875D0*CA22*CA3*m12&
  &squared*SA1*dgAtMZ())/(MW*SB) - (0.15625D0*CA3*m12squared*SA1*dgAtMZ())/(CB2*MW*SB) - (0.5625D0*CA12*CA3*m12squared*SA1*dgAtMZ&
  &())/(CB2*MW*SB) - (0.15625D0*CA22*CA3*m12squared*SA1*dgAtMZ())/(CB2*MW*SB) - (0.5625D0*CA12*CA22*CA3*m12squared*SA1*dgAtMZ())/&
  &(CB2*MW*SB) - (0.375D0*CA1*CA3*MH12*SA12*dgAtMZ())/(MW*SB) - (0.375D0*CA1*CA22*CA3*MH12*SA12*dgAtMZ())/(MW*SB) - (0.1875D0*CA1&
  &*CA3*MH22*SA12*dgAtMZ())/(MW*SB) - (0.1875D0*CA1*CA22*CA3*MH22*SA12*dgAtMZ())/(MW*SB) + (0.125D0*CA1*CA3*MH12*SA22*dgAtMZ())/(&
  &MW*SB) + (0.0625D0*CA1*CA3*MH22*SA22*dgAtMZ())/(MW*SB) - (0.21875D0*CA3*m12squared*SA1*SA22*dgAtMZ())/(MW*SB) + (0.15625D0*CA3&
  &*m12squared*SA1*SA22*dgAtMZ())/(CB2*MW*SB) + (0.5625D0*CA12*CA3*m12squared*SA1*SA22*dgAtMZ())/(CB2*MW*SB) + (0.375D0*CA1*CA3*M&
  &H12*SA12*SA22*dgAtMZ())/(MW*SB) + (0.1875D0*CA1*CA3*MH22*SA12*SA22*dgAtMZ())/(MW*SB) + (0.28125D0*CA1*m12squared*SA2*SA3*dgAtM&
  &Z())/(MW*SB) + (0.84375D0*CA1*CA22*m12squared*SA2*SA3*dgAtMZ())/(MW*SB) - (0.1875D0*CA1*m12squared*SA2*SA3*dgAtMZ())/(CB2*MW*S&
  &B) - (0.5625D0*CA1*CA22*m12squared*SA2*SA3*dgAtMZ())/(CB2*MW*SB) + (0.1875D0*MH12*SA1*SA2*SA3*dgAtMZ())/(MW*SB) - (0.1875D0*CA&
  &12*MH12*SA1*SA2*SA3*dgAtMZ())/(MW*SB) + (0.5625D0*CA22*MH12*SA1*SA2*SA3*dgAtMZ())/(MW*SB) - (0.5625D0*CA12*CA22*MH12*SA1*SA2*S&
  &A3*dgAtMZ())/(MW*SB) + (0.09375D0*MH22*SA1*SA2*SA3*dgAtMZ())/(MW*SB) - (0.09375D0*CA12*MH22*SA1*SA2*SA3*dgAtMZ())/(MW*SB) + (0&
  &.28125D0*CA22*MH22*SA1*SA2*SA3*dgAtMZ())/(MW*SB) - (0.28125D0*CA12*CA22*MH22*SA1*SA2*SA3*dgAtMZ())/(MW*SB) + (0.28125D0*CA1*m1&
  &2squared*SA12*SA2*SA3*dgAtMZ())/(CB2*MW*SB) + (0.84375D0*CA1*CA22*m12squared*SA12*SA2*SA3*dgAtMZ())/(CB2*MW*SB) + (0.0625D0*CA&
  &1*CA3*m12squared*dgAtMZ())/(CB*MW*SB2) + (0.0625D0*CA1*CA22*CA3*m12squared*dgAtMZ())/(CB*MW*SB2) + (0.5625D0*CA1*CA3*m12square&
  &d*SA12*dgAtMZ())/(CB*MW*SB2) + (0.5625D0*CA1*CA22*CA3*m12squared*SA12*dgAtMZ())/(CB*MW*SB2) - (0.0625D0*CA1*CA3*m12squared*SA2&
  &2*dgAtMZ())/(CB*MW*SB2) - (0.5625D0*CA1*CA3*m12squared*SA12*SA22*dgAtMZ())/(CB*MW*SB2) - (0.1875D0*m12squared*SA1*SA2*SA3*dgAt&
  &MZ())/(CB*MW*SB2) + (0.28125D0*CA12*m12squared*SA1*SA2*SA3*dgAtMZ())/(CB*MW*SB2) - (0.5625D0*CA22*m12squared*SA1*SA2*SA3*dgAtM&
  &Z())/(CB*MW*SB2) + (0.84375D0*CA12*CA22*m12squared*SA1*SA2*SA3*dgAtMZ())/(CB*MW*SB2) + (0.125D0*CA1*CA3*m12squared*dgAtMZ())/(&
  &MW*SB*TB) + (0.125D0*CA1*CA22*CA3*m12squared*dgAtMZ())/(MW*SB*TB) - (0.125D0*CA1*CA3*m12squared*SA22*dgAtMZ())/(MW*SB*TB) - (0&
  &.09375D0*m12squared*SA1*SA2*SA3*dgAtMZ())/(MW*SB*TB) - (0.28125D0*CA22*m12squared*SA1*SA2*SA3*dgAtMZ())/(MW*SB*TB) - (0.03125D&
  &0*CA3*m12squared*SA1*TB*dgAtMZ())/(CB*MW) - (0.03125D0*CA22*CA3*m12squared*SA1*TB*dgAtMZ())/(CB*MW) + (0.03125D0*CA3*m12square&
  &d*SA1*SA22*TB*dgAtMZ())/(CB*MW) - (0.09375D0*CA1*m12squared*SA2*SA3*TB*dgAtMZ())/(CB*MW) - (0.28125D0*CA1*CA22*m12squared*SA2*&
  &SA3*TB*dgAtMZ())/(CB*MW) + (0.0625D0*MH12*SA2*SA3*DBLE(CA1**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.1875D0*CA22*MH12*SA2*SA3*DBLE(CA&
  &1**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.03125D0*MH22*SA2*SA3*DBLE(CA1**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.09375D0*CA22*MH22*SA2*SA&
  &3*DBLE(CA1**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.125D0*CA3*MH12*DBLE(CA1**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.125D0*CA22*CA3*MH12*D&
  &BLE(CA1**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.0625D0*CA3*MH22*DBLE(CA1**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.0625D0*CA22*CA3*MH22*DB&
  &LE(CA1**INT(3.D0))*dgAtMZ())/(MW*SB) - (0.125D0*CA3*MH12*SA22*DBLE(CA1**INT(3.D0))*dgAtMZ())/(MW*SB) - (0.0625D0*CA3*MH22*SA22&
  &*DBLE(CA1**INT(3.D0))*dgAtMZ())/(MW*SB) - (0.09375D0*m12squared*SA2*SA3*DBLE(CA1**INT(3.D0))*dgAtMZ())/(CB2*MW*SB) - (0.28125D&
  &0*CA22*m12squared*SA2*SA3*DBLE(CA1**INT(3.D0))*dgAtMZ())/(CB2*MW*SB) - (0.1875D0*CA3*m12squared*DBLE(CA1**INT(3.D0))*dgAtMZ())&
  &/(CB*MW*SB2) - (0.1875D0*CA22*CA3*m12squared*DBLE(CA1**INT(3.D0))*dgAtMZ())/(CB*MW*SB2) + (0.1875D0*CA3*m12squared*SA22*DBLE(C&
  &A1**INT(3.D0))*dgAtMZ())/(CB*MW*SB2) - (0.125D0*CA3*MH12*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB*MW) - (0.125D0*CA22*CA3*MH12*DBLE(&
  &SA1**INT(3.D0))*dgAtMZ())/(CB*MW) - (0.0625D0*CA3*MH22*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB*MW) - (0.0625D0*CA22*CA3*MH22*DBLE(S&
  &A1**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.125D0*CA3*MH12*SA22*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.0625D0*CA3*MH22*SA22*DBL&
  &E(SA1**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.1875D0*CA3*m12squared*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB2*MW*SB) + (0.1875D0*CA22*CA3&
  &*m12squared*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB2*MW*SB) - (0.1875D0*CA3*m12squared*SA22*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB2*MW*&
  &SB) + (0.0625D0*MH12*SA2*SA3*DBLE(SA1**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.1875D0*CA22*MH12*SA2*SA3*DBLE(SA1**INT(3.D0))*dgAtMZ(&
  &))/(MW*SB) + (0.03125D0*MH22*SA2*SA3*DBLE(SA1**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.09375D0*CA22*MH22*SA2*SA3*DBLE(SA1**INT(3.D0)&
  &)*dgAtMZ())/(MW*SB) - (0.09375D0*m12squared*SA2*SA3*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB*MW*SB2) - (0.28125D0*CA22*m12squared*SA&
  &2*SA3*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB*MW*SB2) - (0.1875D0*CA1*MH12*SA3*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW) - (0.09375D0*&
  &CA1*MH22*SA3*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW) - (0.28125D0*m12squared*SA1*SA3*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW) + (&
  &0.1875D0*CA1*MH12*SA12*SA3*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.09375D0*CA1*MH22*SA12*SA3*DBLE(SA2**INT(3.D0))*dgAtMZ()&
  &)/(CB*MW) - (0.28125D0*CA1*m12squared*SA3*DBLE(SA2**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.1875D0*CA1*m12squared*SA3*DBLE(SA2**INT(&
  &3.D0))*dgAtMZ())/(CB2*MW*SB) - (0.1875D0*MH12*SA1*SA3*DBLE(SA2**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.1875D0*CA12*MH12*SA1*SA3*DBL&
  &E(SA2**INT(3.D0))*dgAtMZ())/(MW*SB) - (0.09375D0*MH22*SA1*SA3*DBLE(SA2**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.09375D0*CA12*MH22*SA&
  &1*SA3*DBLE(SA2**INT(3.D0))*dgAtMZ())/(MW*SB) - (0.28125D0*CA1*m12squared*SA12*SA3*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB2*MW*SB) +&
  & (0.1875D0*m12squared*SA1*SA3*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW*SB2) - (0.28125D0*CA12*m12squared*SA1*SA3*DBLE(SA2**INT(3.&
  &D0))*dgAtMZ())/(CB*MW*SB2) + (0.09375D0*m12squared*SA1*SA3*DBLE(SA2**INT(3.D0))*dgAtMZ())/(MW*SB*TB) + (0.09375D0*CA1*m12squar&
  &ed*SA3*TB*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW) - (0.0625D0*MH12*SA3*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*&
  &MW) - (0.03125D0*MH22*SA3*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.09375D0*m12squared*SA3*DBLE(CA1**IN&
  &T(3.D0))*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB2*MW*SB) - (0.0625D0*MH12*SA3*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dgAtMZ())/(&
  &MW*SB) - (0.03125D0*MH22*SA3*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.09375D0*m12squared*SA3*DBLE(SA1*&
  &*INT(3.D0))*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW*SB2) - (0.125D0*CA1*CA3*EL*dm122MSBarAlter())/(CB*MW*SW) - (0.125D0*CA1*CA22&
  &*CA3*EL*dm122MSBarAlter())/(CB*MW*SW) + (0.125D0*CA1*CA3*EL*SA22*dm122MSBarAlter())/(CB*MW*SW) + (0.28125D0*EL*SA1*SA2*SA3*dm1&
  &22MSBarAlter())/(CB*MW*SW) + (0.84375D0*CA22*EL*SA1*SA2*SA3*dm122MSBarAlter())/(CB*MW*SW) + (0.21875D0*CA3*EL*SA1*dm122MSBarAl&
  &ter())/(MW*SB*SW) + (0.21875D0*CA22*CA3*EL*SA1*dm122MSBarAlter())/(MW*SB*SW) - (0.15625D0*CA3*EL*SA1*dm122MSBarAlter())/(CB2*M&
  &W*SB*SW) - (0.5625D0*CA12*CA3*EL*SA1*dm122MSBarAlter())/(CB2*MW*SB*SW) - (0.15625D0*CA22*CA3*EL*SA1*dm122MSBarAlter())/(CB2*MW&
  &*SB*SW) - (0.5625D0*CA12*CA22*CA3*EL*SA1*dm122MSBarAlter())/(CB2*MW*SB*SW) - (0.21875D0*CA3*EL*SA1*SA22*dm122MSBarAlter())/(MW&
  &*SB*SW) + (0.15625D0*CA3*EL*SA1*SA22*dm122MSBarAlter())/(CB2*MW*SB*SW) + (0.5625D0*CA12*CA3*EL*SA1*SA22*dm122MSBarAlter())/(CB&
  &2*MW*SB*SW) + (0.28125D0*CA1*EL*SA2*SA3*dm122MSBarAlter())/(MW*SB*SW) + (0.84375D0*CA1*CA22*EL*SA2*SA3*dm122MSBarAlter())/(MW*&
  &SB*SW) - (0.1875D0*CA1*EL*SA2*SA3*dm122MSBarAlter())/(CB2*MW*SB*SW) - (0.5625D0*CA1*CA22*EL*SA2*SA3*dm122MSBarAlter())/(CB2*MW&
  &*SB*SW) + (0.28125D0*CA1*EL*SA12*SA2*SA3*dm122MSBarAlter())/(CB2*MW*SB*SW) + (0.84375D0*CA1*CA22*EL*SA12*SA2*SA3*dm122MSBarAlt&
  &er())/(CB2*MW*SB*SW) + (0.0625D0*CA1*CA3*EL*dm122MSBarAlter())/(CB*MW*SB2*SW) + (0.0625D0*CA1*CA22*CA3*EL*dm122MSBarAlter())/(&
  &CB*MW*SB2*SW) + (0.5625D0*CA1*CA3*EL*SA12*dm122MSBarAlter())/(CB*MW*SB2*SW) + (0.5625D0*CA1*CA22*CA3*EL*SA12*dm122MSBarAlter()&
  &)/(CB*MW*SB2*SW) - (0.0625D0*CA1*CA3*EL*SA22*dm122MSBarAlter())/(CB*MW*SB2*SW) - (0.5625D0*CA1*CA3*EL*SA12*SA22*dm122MSBarAlte&
  &r())/(CB*MW*SB2*SW) - (0.1875D0*EL*SA1*SA2*SA3*dm122MSBarAlter())/(CB*MW*SB2*SW) + (0.28125D0*CA12*EL*SA1*SA2*SA3*dm122MSBarAl&
  &ter())/(CB*MW*SB2*SW) - (0.5625D0*CA22*EL*SA1*SA2*SA3*dm122MSBarAlter())/(CB*MW*SB2*SW) + (0.84375D0*CA12*CA22*EL*SA1*SA2*SA3*&
  &dm122MSBarAlter())/(CB*MW*SB2*SW) + (0.125D0*CA1*CA3*EL*dm122MSBarAlter())/(MW*SB*SW*TB) + (0.125D0*CA1*CA22*CA3*EL*dm122MSBar&
  &Alter())/(MW*SB*SW*TB) - (0.125D0*CA1*CA3*EL*SA22*dm122MSBarAlter())/(MW*SB*SW*TB) - (0.09375D0*EL*SA1*SA2*SA3*dm122MSBarAlter&
  &())/(MW*SB*SW*TB) - (0.28125D0*CA22*EL*SA1*SA2*SA3*dm122MSBarAlter())/(MW*SB*SW*TB) - (0.03125D0*CA3*EL*SA1*TB*dm122MSBarAlter&
  &())/(CB*MW*SW) - (0.03125D0*CA22*CA3*EL*SA1*TB*dm122MSBarAlter())/(CB*MW*SW) + (0.03125D0*CA3*EL*SA1*SA22*TB*dm122MSBarAlter()&
  &)/(CB*MW*SW) - (0.09375D0*CA1*EL*SA2*SA3*TB*dm122MSBarAlter())/(CB*MW*SW) - (0.28125D0*CA1*CA22*EL*SA2*SA3*TB*dm122MSBarAlter(&
  &))/(CB*MW*SW) - (0.09375D0*EL*SA2*SA3*DBLE(CA1**INT(3.D0))*dm122MSBarAlter())/(CB2*MW*SB*SW) - (0.28125D0*CA22*EL*SA2*SA3*DBLE&
  &(CA1**INT(3.D0))*dm122MSBarAlter())/(CB2*MW*SB*SW) - (0.1875D0*CA3*EL*DBLE(CA1**INT(3.D0))*dm122MSBarAlter())/(CB*MW*SB2*SW) -&
  & (0.1875D0*CA22*CA3*EL*DBLE(CA1**INT(3.D0))*dm122MSBarAlter())/(CB*MW*SB2*SW) + (0.1875D0*CA3*EL*SA22*DBLE(CA1**INT(3.D0))*dm1&
  &22MSBarAlter())/(CB*MW*SB2*SW) + (0.1875D0*CA3*EL*DBLE(SA1**INT(3.D0))*dm122MSBarAlter())/(CB2*MW*SB*SW) + (0.1875D0*CA22*CA3*&
  &EL*DBLE(SA1**INT(3.D0))*dm122MSBarAlter())/(CB2*MW*SB*SW) - (0.1875D0*CA3*EL*SA22*DBLE(SA1**INT(3.D0))*dm122MSBarAlter())/(CB2&
  &*MW*SB*SW) - (0.09375D0*EL*SA2*SA3*DBLE(SA1**INT(3.D0))*dm122MSBarAlter())/(CB*MW*SB2*SW) - (0.28125D0*CA22*EL*SA2*SA3*DBLE(SA&
  &1**INT(3.D0))*dm122MSBarAlter())/(CB*MW*SB2*SW) - (0.28125D0*EL*SA1*SA3*DBLE(SA2**INT(3.D0))*dm122MSBarAlter())/(CB*MW*SW) - (&
  &0.28125D0*CA1*EL*SA3*DBLE(SA2**INT(3.D0))*dm122MSBarAlter())/(MW*SB*SW) + (0.1875D0*CA1*EL*SA3*DBLE(SA2**INT(3.D0))*dm122MSBar&
  &Alter())/(CB2*MW*SB*SW) - (0.28125D0*CA1*EL*SA12*SA3*DBLE(SA2**INT(3.D0))*dm122MSBarAlter())/(CB2*MW*SB*SW) + (0.1875D0*EL*SA1&
  &*SA3*DBLE(SA2**INT(3.D0))*dm122MSBarAlter())/(CB*MW*SB2*SW) - (0.28125D0*CA12*EL*SA1*SA3*DBLE(SA2**INT(3.D0))*dm122MSBarAlter(&
  &))/(CB*MW*SB2*SW) + (0.09375D0*EL*SA1*SA3*DBLE(SA2**INT(3.D0))*dm122MSBarAlter())/(MW*SB*SW*TB) + (0.09375D0*CA1*EL*SA3*TB*DBL&
  &E(SA2**INT(3.D0))*dm122MSBarAlter())/(CB*MW*SW) + (0.09375D0*EL*SA3*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dm122MSBarAlter(&
  &))/(CB2*MW*SB*SW) + (0.09375D0*EL*SA3*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dm122MSBarAlter())/(CB*MW*SB2*SW) + (0.125D0*C&
  &A3*EL*SA1*dMH12OSAlter())/(CB*MW*SW) + (0.375D0*CA12*CA3*EL*SA1*dMH12OSAlter())/(CB*MW*SW) + (0.125D0*CA22*CA3*EL*SA1*dMH12OSA&
  &lter())/(CB*MW*SW) + (0.375D0*CA12*CA22*CA3*EL*SA1*dMH12OSAlter())/(CB*MW*SW) - (0.125D0*CA3*EL*SA1*SA22*dMH12OSAlter())/(CB*M&
  &W*SW) - (0.375D0*CA12*CA3*EL*SA1*SA22*dMH12OSAlter())/(CB*MW*SW) + (0.1875D0*CA1*EL*SA2*SA3*dMH12OSAlter())/(CB*MW*SW) + (0.56&
  &25D0*CA1*CA22*EL*SA2*SA3*dMH12OSAlter())/(CB*MW*SW) - (0.1875D0*CA1*EL*SA12*SA2*SA3*dMH12OSAlter())/(CB*MW*SW) - (0.5625D0*CA1&
  &*CA22*EL*SA12*SA2*SA3*dMH12OSAlter())/(CB*MW*SW) - (0.125D0*CA1*CA3*EL*dMH12OSAlter())/(MW*SB*SW) - (0.125D0*CA1*CA22*CA3*EL*d&
  &MH12OSAlter())/(MW*SB*SW) - (0.375D0*CA1*CA3*EL*SA12*dMH12OSAlter())/(MW*SB*SW) - (0.375D0*CA1*CA22*CA3*EL*SA12*dMH12OSAlter()&
  &)/(MW*SB*SW) + (0.125D0*CA1*CA3*EL*SA22*dMH12OSAlter())/(MW*SB*SW) + (0.375D0*CA1*CA3*EL*SA12*SA22*dMH12OSAlter())/(MW*SB*SW) &
  &+ (0.1875D0*EL*SA1*SA2*SA3*dMH12OSAlter())/(MW*SB*SW) - (0.1875D0*CA12*EL*SA1*SA2*SA3*dMH12OSAlter())/(MW*SB*SW) + (0.5625D0*C&
  &A22*EL*SA1*SA2*SA3*dMH12OSAlter())/(MW*SB*SW) - (0.5625D0*CA12*CA22*EL*SA1*SA2*SA3*dMH12OSAlter())/(MW*SB*SW) - (0.5D0*CA2*SA3&
  &*dMH12OSAlter())/vS - (1.5D0*CA2*SA22*SA3*dMH12OSAlter())/vS + (0.0625D0*EL*SA2*SA3*DBLE(CA1**INT(3.D0))*dMH12OSAlter())/(CB*M&
  &W*SW) + (0.1875D0*CA22*EL*SA2*SA3*DBLE(CA1**INT(3.D0))*dMH12OSAlter())/(CB*MW*SW) + (0.125D0*CA3*EL*DBLE(CA1**INT(3.D0))*dMH12&
  &OSAlter())/(MW*SB*SW) + (0.125D0*CA22*CA3*EL*DBLE(CA1**INT(3.D0))*dMH12OSAlter())/(MW*SB*SW) - (0.125D0*CA3*EL*SA22*DBLE(CA1**&
  &INT(3.D0))*dMH12OSAlter())/(MW*SB*SW) + (0.5D0*SA3*DBLE(CA2**INT(3.D0))*dMH12OSAlter())/vS - (0.125D0*CA3*EL*DBLE(SA1**INT(3.D&
  &0))*dMH12OSAlter())/(CB*MW*SW) - (0.125D0*CA22*CA3*EL*DBLE(SA1**INT(3.D0))*dMH12OSAlter())/(CB*MW*SW) + (0.125D0*CA3*EL*SA22*D&
  &BLE(SA1**INT(3.D0))*dMH12OSAlter())/(CB*MW*SW) + (0.0625D0*EL*SA2*SA3*DBLE(SA1**INT(3.D0))*dMH12OSAlter())/(MW*SB*SW) + (0.187&
  &5D0*CA22*EL*SA2*SA3*DBLE(SA1**INT(3.D0))*dMH12OSAlter())/(MW*SB*SW) - (0.1875D0*CA1*EL*SA3*DBLE(SA2**INT(3.D0))*dMH12OSAlter()&
  &)/(CB*MW*SW) + (0.1875D0*CA1*EL*SA12*SA3*DBLE(SA2**INT(3.D0))*dMH12OSAlter())/(CB*MW*SW) - (0.1875D0*EL*SA1*SA3*DBLE(SA2**INT(&
  &3.D0))*dMH12OSAlter())/(MW*SB*SW) + (0.1875D0*CA12*EL*SA1*SA3*DBLE(SA2**INT(3.D0))*dMH12OSAlter())/(MW*SB*SW) - (0.0625D0*EL*S&
  &A3*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dMH12OSAlter())/(CB*MW*SW) - (0.0625D0*EL*SA3*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(&
  &3.D0))*dMH12OSAlter())/(MW*SB*SW) + (0.0625D0*CA3*EL*SA1*dMH22OSAlter())/(CB*MW*SW) + (0.1875D0*CA12*CA3*EL*SA1*dMH22OSAlter()&
  &)/(CB*MW*SW) + (0.0625D0*CA22*CA3*EL*SA1*dMH22OSAlter())/(CB*MW*SW) + (0.1875D0*CA12*CA22*CA3*EL*SA1*dMH22OSAlter())/(CB*MW*SW&
  &) - (0.0625D0*CA3*EL*SA1*SA22*dMH22OSAlter())/(CB*MW*SW) - (0.1875D0*CA12*CA3*EL*SA1*SA22*dMH22OSAlter())/(CB*MW*SW) + (0.0937&
  &5D0*CA1*EL*SA2*SA3*dMH22OSAlter())/(CB*MW*SW) + (0.28125D0*CA1*CA22*EL*SA2*SA3*dMH22OSAlter())/(CB*MW*SW) - (0.09375D0*CA1*EL*&
  &SA12*SA2*SA3*dMH22OSAlter())/(CB*MW*SW) - (0.28125D0*CA1*CA22*EL*SA12*SA2*SA3*dMH22OSAlter())/(CB*MW*SW) - (0.0625D0*CA1*CA3*E&
  &L*dMH22OSAlter())/(MW*SB*SW) - (0.0625D0*CA1*CA22*CA3*EL*dMH22OSAlter())/(MW*SB*SW) - (0.1875D0*CA1*CA3*EL*SA12*dMH22OSAlter()&
  &)/(MW*SB*SW) - (0.1875D0*CA1*CA22*CA3*EL*SA12*dMH22OSAlter())/(MW*SB*SW) + (0.0625D0*CA1*CA3*EL*SA22*dMH22OSAlter())/(MW*SB*SW&
  &) + (0.1875D0*CA1*CA3*EL*SA12*SA22*dMH22OSAlter())/(MW*SB*SW) + (0.09375D0*EL*SA1*SA2*SA3*dMH22OSAlter())/(MW*SB*SW) - (0.0937&
  &5D0*CA12*EL*SA1*SA2*SA3*dMH22OSAlter())/(MW*SB*SW) + (0.28125D0*CA22*EL*SA1*SA2*SA3*dMH22OSAlter())/(MW*SB*SW) - (0.28125D0*CA&
  &12*CA22*EL*SA1*SA2*SA3*dMH22OSAlter())/(MW*SB*SW) - (0.25D0*CA2*SA3*dMH22OSAlter())/vS - (0.75D0*CA2*SA22*SA3*dMH22OSAlter())/&
  &vS + (0.03125D0*EL*SA2*SA3*DBLE(CA1**INT(3.D0))*dMH22OSAlter())/(CB*MW*SW) + (0.09375D0*CA22*EL*SA2*SA3*DBLE(CA1**INT(3.D0))*d&
  &MH22OSAlter())/(CB*MW*SW) + (0.0625D0*CA3*EL*DBLE(CA1**INT(3.D0))*dMH22OSAlter())/(MW*SB*SW) + (0.0625D0*CA22*CA3*EL*DBLE(CA1*&
  &*INT(3.D0))*dMH22OSAlter())/(MW*SB*SW) - (0.0625D0*CA3*EL*SA22*DBLE(CA1**INT(3.D0))*dMH22OSAlter())/(MW*SB*SW) + (0.25D0*SA3*D&
  &BLE(CA2**INT(3.D0))*dMH22OSAlter())/vS - (0.0625D0*CA3*EL*DBLE(SA1**INT(3.D0))*dMH22OSAlter())/(CB*MW*SW) - (0.0625D0*CA22*CA3&
  &*EL*DBLE(SA1**INT(3.D0))*dMH22OSAlter())/(CB*MW*SW) + (0.0625D0*CA3*EL*SA22*DBLE(SA1**INT(3.D0))*dMH22OSAlter())/(CB*MW*SW) + &
  &(0.03125D0*EL*SA2*SA3*DBLE(SA1**INT(3.D0))*dMH22OSAlter())/(MW*SB*SW) + (0.09375D0*CA22*EL*SA2*SA3*DBLE(SA1**INT(3.D0))*dMH22O&
  &SAlter())/(MW*SB*SW) - (0.09375D0*CA1*EL*SA3*DBLE(SA2**INT(3.D0))*dMH22OSAlter())/(CB*MW*SW) + (0.09375D0*CA1*EL*SA12*SA3*DBLE&
  &(SA2**INT(3.D0))*dMH22OSAlter())/(CB*MW*SW) - (0.09375D0*EL*SA1*SA3*DBLE(SA2**INT(3.D0))*dMH22OSAlter())/(MW*SB*SW) + (0.09375&
  &D0*CA12*EL*SA1*SA3*DBLE(SA2**INT(3.D0))*dMH22OSAlter())/(MW*SB*SW) - (0.03125D0*EL*SA3*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0&
  &))*dMH22OSAlter())/(CB*MW*SW) - (0.03125D0*EL*SA3*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dMH22OSAlter())/(MW*SB*SW) + (0.06&
  &25D0*CA1*CA3*EL*m12squared*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.0625D0*CA1*CA22*CA3*EL*m12squared*DBLE(MW**INT(-3.D0&
  &))*dMW2Alter())/(CB*SW) - (0.0625D0*CA3*EL*MH12*SA1*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.1875D0*CA12*CA3*EL*MH12*SA1&
  &*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.0625D0*CA22*CA3*EL*MH12*SA1*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.187&
  &5D0*CA12*CA22*CA3*EL*MH12*SA1*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.03125D0*CA3*EL*MH22*SA1*DBLE(MW**INT(-3.D0))*dMW2&
  &Alter())/(CB*SW) - (0.09375D0*CA12*CA3*EL*MH22*SA1*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.03125D0*CA22*CA3*EL*MH22*SA1&
  &*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.09375D0*CA12*CA22*CA3*EL*MH22*SA1*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - &
  &(0.0625D0*CA1*CA3*EL*m12squared*SA22*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.0625D0*CA3*EL*MH12*SA1*SA22*DBLE(MW**INT(-&
  &3.D0))*dMW2Alter())/(CB*SW) + (0.1875D0*CA12*CA3*EL*MH12*SA1*SA22*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.03125D0*CA3*E&
  &L*MH22*SA1*SA22*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.09375D0*CA12*CA3*EL*MH22*SA1*SA22*DBLE(MW**INT(-3.D0))*dMW2Alte&
  &r())/(CB*SW) - (0.09375D0*CA1*EL*MH12*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.28125D0*CA1*CA22*EL*MH12*SA2*SA3*&
  &DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.046875D0*CA1*EL*MH22*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.140&
  &625D0*CA1*CA22*EL*MH22*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.140625D0*EL*m12squared*SA1*SA2*SA3*DBLE(MW**INT(&
  &-3.D0))*dMW2Alter())/(CB*SW) - (0.421875D0*CA22*EL*m12squared*SA1*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.09375&
  &D0*CA1*EL*MH12*SA12*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.28125D0*CA1*CA22*EL*MH12*SA12*SA2*SA3*DBLE(MW**INT(&
  &-3.D0))*dMW2Alter())/(CB*SW) + (0.046875D0*CA1*EL*MH22*SA12*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.140625D0*CA&
  &1*CA22*EL*MH22*SA12*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.0625D0*CA1*CA3*EL*MH12*DBLE(MW**INT(-3.D0))*dMW2Alt&
  &er())/(SB*SW) + (0.0625D0*CA1*CA22*CA3*EL*MH12*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.03125D0*CA1*CA3*EL*MH22*DBLE(MW*&
  &*INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.03125D0*CA1*CA22*CA3*EL*MH22*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.109375D0*CA&
  &3*EL*m12squared*SA1*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.109375D0*CA22*CA3*EL*m12squared*SA1*DBLE(MW**INT(-3.D0))*dM&
  &W2Alter())/(SB*SW) + (0.078125D0*CA3*EL*m12squared*SA1*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB2*SB*SW) + (0.28125D0*CA12*CA3*EL*&
  &m12squared*SA1*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB2*SB*SW) + (0.078125D0*CA22*CA3*EL*m12squared*SA1*DBLE(MW**INT(-3.D0))*dMW&
  &2Alter())/(CB2*SB*SW) + (0.28125D0*CA12*CA22*CA3*EL*m12squared*SA1*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB2*SB*SW) + (0.1875D0*C&
  &A1*CA3*EL*MH12*SA12*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.1875D0*CA1*CA22*CA3*EL*MH12*SA12*DBLE(MW**INT(-3.D0))*dMW2A&
  &lter())/(SB*SW) + (0.09375D0*CA1*CA3*EL*MH22*SA12*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.09375D0*CA1*CA22*CA3*EL*MH22*&
  &SA12*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.0625D0*CA1*CA3*EL*MH12*SA22*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0&
  &.03125D0*CA1*CA3*EL*MH22*SA22*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.109375D0*CA3*EL*m12squared*SA1*SA22*DBLE(MW**INT(&
  &-3.D0))*dMW2Alter())/(SB*SW) - (0.078125D0*CA3*EL*m12squared*SA1*SA22*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB2*SB*SW) - (0.28125&
  &D0*CA12*CA3*EL*m12squared*SA1*SA22*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB2*SB*SW) - (0.1875D0*CA1*CA3*EL*MH12*SA12*SA22*DBLE(MW&
  &**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.09375D0*CA1*CA3*EL*MH22*SA12*SA22*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.14062&
  &5D0*CA1*EL*m12squared*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.421875D0*CA1*CA22*EL*m12squared*SA2*SA3*DBLE(MW**&
  &INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.09375D0*CA1*EL*m12squared*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB2*SB*SW) + (0.281&
  &25D0*CA1*CA22*EL*m12squared*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB2*SB*SW) - (0.09375D0*EL*MH12*SA1*SA2*SA3*DBLE(MW**IN&
  &T(-3.D0))*dMW2Alter())/(SB*SW) + (0.09375D0*CA12*EL*MH12*SA1*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.28125D0*CA&
  &22*EL*MH12*SA1*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.28125D0*CA12*CA22*EL*MH12*SA1*SA2*SA3*DBLE(MW**INT(-3.D0&
  &))*dMW2Alter())/(SB*SW) - (0.046875D0*EL*MH22*SA1*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.046875D0*CA12*EL*MH22&
  &*SA1*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.140625D0*CA22*EL*MH22*SA1*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter()&
  &)/(SB*SW) + (0.140625D0*CA12*CA22*EL*MH22*SA1*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.140625D0*CA1*EL*m12square&
  &d*SA12*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB2*SB*SW) - (0.421875D0*CA1*CA22*EL*m12squared*SA12*SA2*SA3*DBLE(MW**INT(-3&
  &.D0))*dMW2Alter())/(CB2*SB*SW) - (0.03125D0*CA1*CA3*EL*m12squared*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SB2*SW) - (0.03125D0*C&
  &A1*CA22*CA3*EL*m12squared*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SB2*SW) - (0.28125D0*CA1*CA3*EL*m12squared*SA12*DBLE(MW**INT(-&
  &3.D0))*dMW2Alter())/(CB*SB2*SW) - (0.28125D0*CA1*CA22*CA3*EL*m12squared*SA12*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SB2*SW) + (&
  &0.03125D0*CA1*CA3*EL*m12squared*SA22*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SB2*SW) + (0.28125D0*CA1*CA3*EL*m12squared*SA12*SA2&
  &2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SB2*SW) + (0.09375D0*EL*m12squared*SA1*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*S&
  &B2*SW) - (0.140625D0*CA12*EL*m12squared*SA1*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SB2*SW) + (0.28125D0*CA22*EL*m12squa&
  &red*SA1*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SB2*SW) - (0.421875D0*CA12*CA22*EL*m12squared*SA1*SA2*SA3*DBLE(MW**INT(-&
  &3.D0))*dMW2Alter())/(CB*SB2*SW) - (0.0625D0*CA1*CA3*EL*m12squared*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW*TB) - (0.0625D0*CA1&
  &*CA22*CA3*EL*m12squared*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW*TB) + (0.0625D0*CA1*CA3*EL*m12squared*SA22*DBLE(MW**INT(-3.D0&
  &))*dMW2Alter())/(SB*SW*TB) + (0.046875D0*EL*m12squared*SA1*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW*TB) + (0.140625D0*&
  &CA22*EL*m12squared*SA1*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW*TB) + (0.015625D0*CA3*EL*m12squared*SA1*TB*DBLE(MW**IN&
  &T(-3.D0))*dMW2Alter())/(CB*SW) + (0.015625D0*CA22*CA3*EL*m12squared*SA1*TB*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.0156&
  &25D0*CA3*EL*m12squared*SA1*SA22*TB*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.046875D0*CA1*EL*m12squared*SA2*SA3*TB*DBLE(M&
  &W**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.140625D0*CA1*CA22*EL*m12squared*SA2*SA3*TB*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) &
  &- (0.03125D0*EL*MH12*SA2*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.09375D0*CA22*EL*MH12*SA2*SA3*&
  &DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.015625D0*EL*MH22*SA2*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT&
  &(-3.D0))*dMW2Alter())/(CB*SW) - (0.046875D0*CA22*EL*MH22*SA2*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW&
  &) - (0.0625D0*CA3*EL*MH12*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.0625D0*CA22*CA3*EL*MH12*DBLE(CA1&
  &**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.03125D0*CA3*EL*MH22*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2&
  &Alter())/(SB*SW) - (0.03125D0*CA22*CA3*EL*MH22*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.0625D0*CA3*&
  &EL*MH12*SA22*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.03125D0*CA3*EL*MH22*SA22*DBLE(CA1**INT(3.D0))&
  &*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.046875D0*EL*m12squared*SA2*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2A&
  &lter())/(CB2*SB*SW) + (0.140625D0*CA22*EL*m12squared*SA2*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB2*SB*SW&
  &) + (0.09375D0*CA3*EL*m12squared*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SB2*SW) + (0.09375D0*CA22*CA3*EL*m&
  &12squared*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SB2*SW) - (0.09375D0*CA3*EL*m12squared*SA22*DBLE(CA1**INT&
  &(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SB2*SW) + (0.0625D0*CA3*EL*MH12*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Al&
  &ter())/(CB*SW) + (0.0625D0*CA22*CA3*EL*MH12*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(CB*SW) + (0.03125D0*CA3*EL&
  &*MH22*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(CB*SW) + (0.03125D0*CA22*CA3*EL*MH22*DBLE(MW**INT(-3.D0))*DBLE(S&
  &A1**INT(3.D0))*dMW2Alter())/(CB*SW) - (0.0625D0*CA3*EL*MH12*SA22*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(CB*SW&
  &) - (0.03125D0*CA3*EL*MH22*SA22*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(CB*SW) - (0.09375D0*CA3*EL*m12squared*&
  &DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(CB2*SB*SW) - (0.09375D0*CA22*CA3*EL*m12squared*DBLE(MW**INT(-3.D0))*DB&
  &LE(SA1**INT(3.D0))*dMW2Alter())/(CB2*SB*SW) + (0.09375D0*CA3*EL*m12squared*SA22*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2&
  &Alter())/(CB2*SB*SW) - (0.03125D0*EL*MH12*SA2*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(SB*SW) - (0.09375D0*&
  &CA22*EL*MH12*SA2*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(SB*SW) - (0.015625D0*EL*MH22*SA2*SA3*DBLE(MW**INT&
  &(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(SB*SW) - (0.046875D0*CA22*EL*MH22*SA2*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0)&
  &)*dMW2Alter())/(SB*SW) + (0.046875D0*EL*m12squared*SA2*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(CB*SB2*SW) &
  &+ (0.140625D0*CA22*EL*m12squared*SA2*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(CB*SB2*SW) + (0.09375D0*CA1*E&
  &L*MH12*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(CB*SW) + (0.046875D0*CA1*EL*MH22*SA3*DBLE(MW**INT(-3.D0))*D&
  &BLE(SA2**INT(3.D0))*dMW2Alter())/(CB*SW) + (0.140625D0*EL*m12squared*SA1*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alt&
  &er())/(CB*SW) - (0.09375D0*CA1*EL*MH12*SA12*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(CB*SW) - (0.046875D0*C&
  &A1*EL*MH22*SA12*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(CB*SW) + (0.140625D0*CA1*EL*m12squared*SA3*DBLE(MW&
  &**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(SB*SW) - (0.09375D0*CA1*EL*m12squared*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(&
  &3.D0))*dMW2Alter())/(CB2*SB*SW) + (0.09375D0*EL*MH12*SA1*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(SB*SW) - &
  &(0.09375D0*CA12*EL*MH12*SA1*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(SB*SW) + (0.046875D0*EL*MH22*SA1*SA3*D&
  &BLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(SB*SW) - (0.046875D0*CA12*EL*MH22*SA1*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA2&
  &**INT(3.D0))*dMW2Alter())/(SB*SW) + (0.140625D0*CA1*EL*m12squared*SA12*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter&
  &())/(CB2*SB*SW) - (0.09375D0*EL*m12squared*SA1*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(CB*SB2*SW) + (0.140&
  &625D0*CA12*EL*m12squared*SA1*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(CB*SB2*SW) - (0.046875D0*EL*m12square&
  &d*SA1*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(SB*SW*TB) - (0.046875D0*CA1*EL*m12squared*SA3*TB*DBLE(MW**IN&
  &T(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(CB*SW) + (0.03125D0*EL*MH12*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*DBLE(SA&
  &2**INT(3.D0))*dMW2Alter())/(CB*SW) + (0.015625D0*EL*MH22*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dM&
  &W2Alter())/(CB*SW) - (0.046875D0*EL*m12squared*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())&
  &/(CB2*SB*SW) + (0.03125D0*EL*MH12*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(SB*SW) + (0&
  &.015625D0*EL*MH22*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(SB*SW) - (0.046875D0*EL*m12&
  &squared*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(CB*SB2*SW) + 0.5D0*CA2*MH12*SA3*DBLE(&
  &vS**INT(-2.D0))*dvSMSBarAlter() + 0.25D0*CA2*MH22*SA3*DBLE(vS**INT(-2.D0))*dvSMSBarAlter() + 1.5D0*CA2*MH12*SA22*SA3*DBLE(vS**&
  &INT(-2.D0))*dvSMSBarAlter() + 0.75D0*CA2*MH22*SA22*SA3*DBLE(vS**INT(-2.D0))*dvSMSBarAlter() - 0.5D0*MH12*SA3*DBLE(CA2**INT(3.D&
  &0))*DBLE(vS**INT(-2.D0))*dvSMSBarAlter() - 0.25D0*MH22*SA3*DBLE(CA2**INT(3.D0))*DBLE(vS**INT(-2.D0))*dvSMSBarAlter() &
		& + CS1S1S1f111*dZH1H2OSAlter()/2D0 + CS1S1S1f211*dZH2H2OS()/2D0 + CS1S1S1f311*dZH3H2OSAlter()/2D0 &
		& + CS1S1S1f121*dZH1H1OS()/2D0 + CS1S1S1f221*dZH2H1OSAlter()/2D0 + CS1S1S1f321*dZH3H1OSAlter()/2D0 &
		& + CS1S1S1f121*dZH1H1OS()/2D0 + CS1S1S1f221*dZH2H1OSAlter()/2D0 + CS1S1S1f321*dZH3H1OSAlter()/2D0 &
		& )
	case (8)
		dRR11 = -CA2*SA1*dAlpha1PinchOS() - CA1*SA2*dAlpha2PinchOS()
		dRR12 = CA1*CA2*dAlpha1PinchOS() - SA1*SA2*dAlpha2PinchOS()
		dRR13 = CA2*dAlpha2PinchOS()
		dRR21 = -CA1*CA3*dAlpha1PinchOS() - CA1*CA3*SA2*dAlpha3PinchOS() - CA1*CA2*SA3*dAlpha2PinchOS() + &
			& SA1*SA3*dAlpha3PinchOS() + SA1*SA2*SA3*dAlpha1PinchOS()
		dRR22 = -CA3*SA1*dAlpha1PinchOS() - CA3*SA1*SA2*dAlpha3PinchOS() - CA1*SA3*dAlpha3PinchOS() - &
			& CA2*SA1*SA3*dAlpha2PinchOS() - CA1*SA2*SA3*dAlpha1PinchOS()
		dRR23 = CA2*CA3*dAlpha3PinchOS() - SA2*SA3*dAlpha2PinchOS()
		dRR31 = -CA1*CA2*CA3*dAlpha2PinchOS() + CA3*SA1*dAlpha3PinchOS() + CA3*SA1*SA2*dAlpha1PinchOS() + &
			& CA1*SA3*dAlpha1PinchOS() + CA1*SA2*SA3*dAlpha3PinchOS()
		dRR32 = -CA1*CA3*dAlpha3PinchOS() - CA2*CA3*SA1*dAlpha2PinchOS() - CA1*CA3*SA2*dAlpha1PinchOS() + &
			& SA1*SA3*dAlpha1PinchOS() + SA1*SA2*SA3*dAlpha3PinchOS()
		dRR33 = -CA3*SA2*dAlpha2PinchOS() - CA2*SA3*dAlpha3PinchOS()

		totalAmplitude = CS1S1S1f211*( &
&(0.125D0*CA1*CA3*EL*MH12*dAlpha1PinchOS())/(CB*MW*SW) + (0.125D0*CA1*CA22*CA3*EL*MH12*dAlpha1PinchOS())/(CB*MW*SW) + (0.0625D0*CA&
  &1*CA3*EL*MH22*dAlpha1PinchOS())/(CB*MW*SW) + (0.0625D0*CA1*CA22*CA3*EL*MH22*dAlpha1PinchOS())/(CB*MW*SW) + (0.125D0*CA3*EL*m12&
  &squared*SA1*dAlpha1PinchOS())/(CB*MW*SW) + (0.125D0*CA22*CA3*EL*m12squared*SA1*dAlpha1PinchOS())/(CB*MW*SW) - (1.125D0*CA1*CA3&
  &*EL*MH12*SA12*dAlpha1PinchOS())/(CB*MW*SW) - (1.125D0*CA1*CA22*CA3*EL*MH12*SA12*dAlpha1PinchOS())/(CB*MW*SW) - (0.5625D0*CA1*C&
  &A3*EL*MH22*SA12*dAlpha1PinchOS())/(CB*MW*SW) - (0.5625D0*CA1*CA22*CA3*EL*MH22*SA12*dAlpha1PinchOS())/(CB*MW*SW) - (0.125D0*CA1&
  &*CA3*EL*MH12*SA22*dAlpha1PinchOS())/(CB*MW*SW) - (0.0625D0*CA1*CA3*EL*MH22*SA22*dAlpha1PinchOS())/(CB*MW*SW) - (0.125D0*CA3*EL&
  &*m12squared*SA1*SA22*dAlpha1PinchOS())/(CB*MW*SW) + (1.125D0*CA1*CA3*EL*MH12*SA12*SA22*dAlpha1PinchOS())/(CB*MW*SW) + (0.5625D&
  &0*CA1*CA3*EL*MH22*SA12*SA22*dAlpha1PinchOS())/(CB*MW*SW) + (0.28125D0*CA1*EL*m12squared*SA2*SA3*dAlpha1PinchOS())/(CB*MW*SW) +&
  & (0.84375D0*CA1*CA22*EL*m12squared*SA2*SA3*dAlpha1PinchOS())/(CB*MW*SW) - (0.1875D0*EL*MH12*SA1*SA2*SA3*dAlpha1PinchOS())/(CB*&
  &MW*SW) - (0.5625D0*CA12*EL*MH12*SA1*SA2*SA3*dAlpha1PinchOS())/(CB*MW*SW) - (0.5625D0*CA22*EL*MH12*SA1*SA2*SA3*dAlpha1PinchOS()&
  &)/(CB*MW*SW) - (1.6875D0*CA12*CA22*EL*MH12*SA1*SA2*SA3*dAlpha1PinchOS())/(CB*MW*SW) - (0.09375D0*EL*MH22*SA1*SA2*SA3*dAlpha1Pi&
  &nchOS())/(CB*MW*SW) - (0.28125D0*CA12*EL*MH22*SA1*SA2*SA3*dAlpha1PinchOS())/(CB*MW*SW) - (0.28125D0*CA22*EL*MH22*SA1*SA2*SA3*d&
  &Alpha1PinchOS())/(CB*MW*SW) - (0.84375D0*CA12*CA22*EL*MH22*SA1*SA2*SA3*dAlpha1PinchOS())/(CB*MW*SW) + (0.21875D0*CA1*CA3*EL*m1&
  &2squared*dAlpha1PinchOS())/(MW*SB*SW) + (0.21875D0*CA1*CA22*CA3*EL*m12squared*dAlpha1PinchOS())/(MW*SB*SW) - (0.15625D0*CA1*CA&
  &3*EL*m12squared*dAlpha1PinchOS())/(CB2*MW*SB*SW) - (0.15625D0*CA1*CA22*CA3*EL*m12squared*dAlpha1PinchOS())/(CB2*MW*SB*SW) + (0&
  &.125D0*CA3*EL*MH12*SA1*dAlpha1PinchOS())/(MW*SB*SW) - (1.125D0*CA12*CA3*EL*MH12*SA1*dAlpha1PinchOS())/(MW*SB*SW) + (0.125D0*CA&
  &22*CA3*EL*MH12*SA1*dAlpha1PinchOS())/(MW*SB*SW) - (1.125D0*CA12*CA22*CA3*EL*MH12*SA1*dAlpha1PinchOS())/(MW*SB*SW) + (0.0625D0*&
  &CA3*EL*MH22*SA1*dAlpha1PinchOS())/(MW*SB*SW) - (0.5625D0*CA12*CA3*EL*MH22*SA1*dAlpha1PinchOS())/(MW*SB*SW) + (0.0625D0*CA22*CA&
  &3*EL*MH22*SA1*dAlpha1PinchOS())/(MW*SB*SW) - (0.5625D0*CA12*CA22*CA3*EL*MH22*SA1*dAlpha1PinchOS())/(MW*SB*SW) + (1.6875D0*CA1*&
  &CA3*EL*m12squared*SA12*dAlpha1PinchOS())/(CB2*MW*SB*SW) + (1.6875D0*CA1*CA22*CA3*EL*m12squared*SA12*dAlpha1PinchOS())/(CB2*MW*&
  &SB*SW) - (0.21875D0*CA1*CA3*EL*m12squared*SA22*dAlpha1PinchOS())/(MW*SB*SW) + (0.15625D0*CA1*CA3*EL*m12squared*SA22*dAlpha1Pin&
  &chOS())/(CB2*MW*SB*SW) - (0.125D0*CA3*EL*MH12*SA1*SA22*dAlpha1PinchOS())/(MW*SB*SW) + (1.125D0*CA12*CA3*EL*MH12*SA1*SA22*dAlph&
  &a1PinchOS())/(MW*SB*SW) - (0.0625D0*CA3*EL*MH22*SA1*SA22*dAlpha1PinchOS())/(MW*SB*SW) + (0.5625D0*CA12*CA3*EL*MH22*SA1*SA22*dA&
  &lpha1PinchOS())/(MW*SB*SW) - (1.6875D0*CA1*CA3*EL*m12squared*SA12*SA22*dAlpha1PinchOS())/(CB2*MW*SB*SW) + (0.1875D0*CA1*EL*MH1&
  &2*SA2*SA3*dAlpha1PinchOS())/(MW*SB*SW) + (0.5625D0*CA1*CA22*EL*MH12*SA2*SA3*dAlpha1PinchOS())/(MW*SB*SW) + (0.09375D0*CA1*EL*M&
  &H22*SA2*SA3*dAlpha1PinchOS())/(MW*SB*SW) + (0.28125D0*CA1*CA22*EL*MH22*SA2*SA3*dAlpha1PinchOS())/(MW*SB*SW) - (0.28125D0*EL*m1&
  &2squared*SA1*SA2*SA3*dAlpha1PinchOS())/(MW*SB*SW) - (0.84375D0*CA22*EL*m12squared*SA1*SA2*SA3*dAlpha1PinchOS())/(MW*SB*SW) + (&
  &0.1875D0*EL*m12squared*SA1*SA2*SA3*dAlpha1PinchOS())/(CB2*MW*SB*SW) + (0.84375D0*CA12*EL*m12squared*SA1*SA2*SA3*dAlpha1PinchOS&
  &())/(CB2*MW*SB*SW) + (0.5625D0*CA22*EL*m12squared*SA1*SA2*SA3*dAlpha1PinchOS())/(CB2*MW*SB*SW) + (2.53125D0*CA12*CA22*EL*m12sq&
  &uared*SA1*SA2*SA3*dAlpha1PinchOS())/(CB2*MW*SB*SW) + (0.5625D0*CA1*EL*MH12*SA12*SA2*SA3*dAlpha1PinchOS())/(MW*SB*SW) + (1.6875&
  &D0*CA1*CA22*EL*MH12*SA12*SA2*SA3*dAlpha1PinchOS())/(MW*SB*SW) + (0.28125D0*CA1*EL*MH22*SA12*SA2*SA3*dAlpha1PinchOS())/(MW*SB*S&
  &W) + (0.84375D0*CA1*CA22*EL*MH22*SA12*SA2*SA3*dAlpha1PinchOS())/(MW*SB*SW) - (0.0625D0*CA3*EL*m12squared*SA1*dAlpha1PinchOS())&
  &/(CB*MW*SB2*SW) + (1.6875D0*CA12*CA3*EL*m12squared*SA1*dAlpha1PinchOS())/(CB*MW*SB2*SW) - (0.0625D0*CA22*CA3*EL*m12squared*SA1&
  &*dAlpha1PinchOS())/(CB*MW*SB2*SW) + (1.6875D0*CA12*CA22*CA3*EL*m12squared*SA1*dAlpha1PinchOS())/(CB*MW*SB2*SW) + (0.0625D0*CA3&
  &*EL*m12squared*SA1*SA22*dAlpha1PinchOS())/(CB*MW*SB2*SW) - (1.6875D0*CA12*CA3*EL*m12squared*SA1*SA22*dAlpha1PinchOS())/(CB*MW*&
  &SB2*SW) - (0.1875D0*CA1*EL*m12squared*SA2*SA3*dAlpha1PinchOS())/(CB*MW*SB2*SW) - (0.5625D0*CA1*CA22*EL*m12squared*SA2*SA3*dAlp&
  &ha1PinchOS())/(CB*MW*SB2*SW) - (0.84375D0*CA1*EL*m12squared*SA12*SA2*SA3*dAlpha1PinchOS())/(CB*MW*SB2*SW) - (2.53125D0*CA1*CA2&
  &2*EL*m12squared*SA12*SA2*SA3*dAlpha1PinchOS())/(CB*MW*SB2*SW) - (0.125D0*CA3*EL*m12squared*SA1*dAlpha1PinchOS())/(MW*SB*SW*TB)&
  & - (0.125D0*CA22*CA3*EL*m12squared*SA1*dAlpha1PinchOS())/(MW*SB*SW*TB) + (0.125D0*CA3*EL*m12squared*SA1*SA22*dAlpha1PinchOS())&
  &/(MW*SB*SW*TB) - (0.09375D0*CA1*EL*m12squared*SA2*SA3*dAlpha1PinchOS())/(MW*SB*SW*TB) - (0.28125D0*CA1*CA22*EL*m12squared*SA2*&
  &SA3*dAlpha1PinchOS())/(MW*SB*SW*TB) - (0.03125D0*CA1*CA3*EL*m12squared*TB*dAlpha1PinchOS())/(CB*MW*SW) - (0.03125D0*CA1*CA22*C&
  &A3*EL*m12squared*TB*dAlpha1PinchOS())/(CB*MW*SW) + (0.03125D0*CA1*CA3*EL*m12squared*SA22*TB*dAlpha1PinchOS())/(CB*MW*SW) + (0.&
  &09375D0*EL*m12squared*SA1*SA2*SA3*TB*dAlpha1PinchOS())/(CB*MW*SW) + (0.28125D0*CA22*EL*m12squared*SA1*SA2*SA3*TB*dAlpha1PinchO&
  &S())/(CB*MW*SW) + (0.5D0*CA1*CA2*CA3*EL*m12squared*SA2*dAlpha2PinchOS())/(CB*MW*SW) - (0.5D0*CA2*CA3*EL*MH12*SA1*SA2*dAlpha2Pi&
  &nchOS())/(CB*MW*SW) - (1.5D0*CA12*CA2*CA3*EL*MH12*SA1*SA2*dAlpha2PinchOS())/(CB*MW*SW) - (0.25D0*CA2*CA3*EL*MH22*SA1*SA2*dAlph&
  &a2PinchOS())/(CB*MW*SW) - (0.75D0*CA12*CA2*CA3*EL*MH22*SA1*SA2*dAlpha2PinchOS())/(CB*MW*SW) + (0.1875D0*CA1*CA2*EL*MH12*SA3*dA&
  &lpha2PinchOS())/(CB*MW*SW) + (0.09375D0*CA1*CA2*EL*MH22*SA3*dAlpha2PinchOS())/(CB*MW*SW) + (0.28125D0*CA2*EL*m12squared*SA1*SA&
  &3*dAlpha2PinchOS())/(CB*MW*SW) - (0.1875D0*CA1*CA2*EL*MH12*SA12*SA3*dAlpha2PinchOS())/(CB*MW*SW) - (0.09375D0*CA1*CA2*EL*MH22*&
  &SA12*SA3*dAlpha2PinchOS())/(CB*MW*SW) - (1.6875D0*CA1*CA2*EL*MH12*SA22*SA3*dAlpha2PinchOS())/(CB*MW*SW) - (0.84375D0*CA1*CA2*E&
  &L*MH22*SA22*SA3*dAlpha2PinchOS())/(CB*MW*SW) - (2.53125D0*CA2*EL*m12squared*SA1*SA22*SA3*dAlpha2PinchOS())/(CB*MW*SW) + (1.687&
  &5D0*CA1*CA2*EL*MH12*SA12*SA22*SA3*dAlpha2PinchOS())/(CB*MW*SW) + (0.84375D0*CA1*CA2*EL*MH22*SA12*SA22*SA3*dAlpha2PinchOS())/(C&
  &B*MW*SW) + (0.5D0*CA1*CA2*CA3*EL*MH12*SA2*dAlpha2PinchOS())/(MW*SB*SW) + (0.25D0*CA1*CA2*CA3*EL*MH22*SA2*dAlpha2PinchOS())/(MW&
  &*SB*SW) - (0.875D0*CA2*CA3*EL*m12squared*SA1*SA2*dAlpha2PinchOS())/(MW*SB*SW) + (0.625D0*CA2*CA3*EL*m12squared*SA1*SA2*dAlpha2&
  &PinchOS())/(CB2*MW*SB*SW) + (2.25D0*CA12*CA2*CA3*EL*m12squared*SA1*SA2*dAlpha2PinchOS())/(CB2*MW*SB*SW) + (1.5D0*CA1*CA2*CA3*E&
  &L*MH12*SA12*SA2*dAlpha2PinchOS())/(MW*SB*SW) + (0.75D0*CA1*CA2*CA3*EL*MH22*SA12*SA2*dAlpha2PinchOS())/(MW*SB*SW) + (0.28125D0*&
  &CA1*CA2*EL*m12squared*SA3*dAlpha2PinchOS())/(MW*SB*SW) - (0.1875D0*CA1*CA2*EL*m12squared*SA3*dAlpha2PinchOS())/(CB2*MW*SB*SW) &
  &+ (0.1875D0*CA2*EL*MH12*SA1*SA3*dAlpha2PinchOS())/(MW*SB*SW) - (0.1875D0*CA12*CA2*EL*MH12*SA1*SA3*dAlpha2PinchOS())/(MW*SB*SW)&
  & + (0.09375D0*CA2*EL*MH22*SA1*SA3*dAlpha2PinchOS())/(MW*SB*SW) - (0.09375D0*CA12*CA2*EL*MH22*SA1*SA3*dAlpha2PinchOS())/(MW*SB*&
  &SW) + (0.28125D0*CA1*CA2*EL*m12squared*SA12*SA3*dAlpha2PinchOS())/(CB2*MW*SB*SW) - (2.53125D0*CA1*CA2*EL*m12squared*SA22*SA3*d&
  &Alpha2PinchOS())/(MW*SB*SW) + (1.6875D0*CA1*CA2*EL*m12squared*SA22*SA3*dAlpha2PinchOS())/(CB2*MW*SB*SW) - (1.6875D0*CA2*EL*MH1&
  &2*SA1*SA22*SA3*dAlpha2PinchOS())/(MW*SB*SW) + (1.6875D0*CA12*CA2*EL*MH12*SA1*SA22*SA3*dAlpha2PinchOS())/(MW*SB*SW) - (0.84375D&
  &0*CA2*EL*MH22*SA1*SA22*SA3*dAlpha2PinchOS())/(MW*SB*SW) + (0.84375D0*CA12*CA2*EL*MH22*SA1*SA22*SA3*dAlpha2PinchOS())/(MW*SB*SW&
  &) - (2.53125D0*CA1*CA2*EL*m12squared*SA12*SA22*SA3*dAlpha2PinchOS())/(CB2*MW*SB*SW) - (0.25D0*CA1*CA2*CA3*EL*m12squared*SA2*dA&
  &lpha2PinchOS())/(CB*MW*SB2*SW) - (2.25D0*CA1*CA2*CA3*EL*m12squared*SA12*SA2*dAlpha2PinchOS())/(CB*MW*SB2*SW) - (0.1875D0*CA2*E&
  &L*m12squared*SA1*SA3*dAlpha2PinchOS())/(CB*MW*SB2*SW) + (0.28125D0*CA12*CA2*EL*m12squared*SA1*SA3*dAlpha2PinchOS())/(CB*MW*SB2&
  &*SW) + (1.6875D0*CA2*EL*m12squared*SA1*SA22*SA3*dAlpha2PinchOS())/(CB*MW*SB2*SW) - (2.53125D0*CA12*CA2*EL*m12squared*SA1*SA22*&
  &SA3*dAlpha2PinchOS())/(CB*MW*SB2*SW) - (0.5D0*CA1*CA2*CA3*EL*m12squared*SA2*dAlpha2PinchOS())/(MW*SB*SW*TB) - (0.09375D0*CA2*E&
  &L*m12squared*SA1*SA3*dAlpha2PinchOS())/(MW*SB*SW*TB) + (0.84375D0*CA2*EL*m12squared*SA1*SA22*SA3*dAlpha2PinchOS())/(MW*SB*SW*T&
  &B) + (0.125D0*CA2*CA3*EL*m12squared*SA1*SA2*TB*dAlpha2PinchOS())/(CB*MW*SW) - (0.09375D0*CA1*CA2*EL*m12squared*SA3*TB*dAlpha2P&
  &inchOS())/(CB*MW*SW) + (0.84375D0*CA1*CA2*EL*m12squared*SA22*SA3*TB*dAlpha2PinchOS())/(CB*MW*SW) + (0.5D0*MH12*SA2*SA3*dAlpha2&
  &PinchOS())/vS - (4.5D0*CA22*MH12*SA2*SA3*dAlpha2PinchOS())/vS + (0.25D0*MH22*SA2*SA3*dAlpha2PinchOS())/vS - (2.25D0*CA22*MH22*&
  &SA2*SA3*dAlpha2PinchOS())/vS + (0.1875D0*CA1*CA3*EL*MH12*SA2*dAlpha3PinchOS())/(CB*MW*SW) + (0.5625D0*CA1*CA22*CA3*EL*MH12*SA2&
  &*dAlpha3PinchOS())/(CB*MW*SW) + (0.09375D0*CA1*CA3*EL*MH22*SA2*dAlpha3PinchOS())/(CB*MW*SW) + (0.28125D0*CA1*CA22*CA3*EL*MH22*&
  &SA2*dAlpha3PinchOS())/(CB*MW*SW) + (0.28125D0*CA3*EL*m12squared*SA1*SA2*dAlpha3PinchOS())/(CB*MW*SW) + (0.84375D0*CA22*CA3*EL*&
  &m12squared*SA1*SA2*dAlpha3PinchOS())/(CB*MW*SW) - (0.1875D0*CA1*CA3*EL*MH12*SA12*SA2*dAlpha3PinchOS())/(CB*MW*SW) - (0.5625D0*&
  &CA1*CA22*CA3*EL*MH12*SA12*SA2*dAlpha3PinchOS())/(CB*MW*SW) - (0.09375D0*CA1*CA3*EL*MH22*SA12*SA2*dAlpha3PinchOS())/(CB*MW*SW) &
  &- (0.28125D0*CA1*CA22*CA3*EL*MH22*SA12*SA2*dAlpha3PinchOS())/(CB*MW*SW) + (0.125D0*CA1*EL*m12squared*SA3*dAlpha3PinchOS())/(CB&
  &*MW*SW) + (0.125D0*CA1*CA22*EL*m12squared*SA3*dAlpha3PinchOS())/(CB*MW*SW) - (0.125D0*EL*MH12*SA1*SA3*dAlpha3PinchOS())/(CB*MW&
  &*SW) - (0.375D0*CA12*EL*MH12*SA1*SA3*dAlpha3PinchOS())/(CB*MW*SW) - (0.125D0*CA22*EL*MH12*SA1*SA3*dAlpha3PinchOS())/(CB*MW*SW)&
  & - (0.375D0*CA12*CA22*EL*MH12*SA1*SA3*dAlpha3PinchOS())/(CB*MW*SW) - (0.0625D0*EL*MH22*SA1*SA3*dAlpha3PinchOS())/(CB*MW*SW) - &
  &(0.1875D0*CA12*EL*MH22*SA1*SA3*dAlpha3PinchOS())/(CB*MW*SW) - (0.0625D0*CA22*EL*MH22*SA1*SA3*dAlpha3PinchOS())/(CB*MW*SW) - (0&
  &.1875D0*CA12*CA22*EL*MH22*SA1*SA3*dAlpha3PinchOS())/(CB*MW*SW) - (0.125D0*CA1*EL*m12squared*SA22*SA3*dAlpha3PinchOS())/(CB*MW*&
  &SW) + (0.125D0*EL*MH12*SA1*SA22*SA3*dAlpha3PinchOS())/(CB*MW*SW) + (0.375D0*CA12*EL*MH12*SA1*SA22*SA3*dAlpha3PinchOS())/(CB*MW&
  &*SW) + (0.0625D0*EL*MH22*SA1*SA22*SA3*dAlpha3PinchOS())/(CB*MW*SW) + (0.1875D0*CA12*EL*MH22*SA1*SA22*SA3*dAlpha3PinchOS())/(CB&
  &*MW*SW) + (0.28125D0*CA1*CA3*EL*m12squared*SA2*dAlpha3PinchOS())/(MW*SB*SW) + (0.84375D0*CA1*CA22*CA3*EL*m12squared*SA2*dAlpha&
  &3PinchOS())/(MW*SB*SW) - (0.1875D0*CA1*CA3*EL*m12squared*SA2*dAlpha3PinchOS())/(CB2*MW*SB*SW) - (0.5625D0*CA1*CA22*CA3*EL*m12s&
  &quared*SA2*dAlpha3PinchOS())/(CB2*MW*SB*SW) + (0.1875D0*CA3*EL*MH12*SA1*SA2*dAlpha3PinchOS())/(MW*SB*SW) - (0.1875D0*CA12*CA3*&
  &EL*MH12*SA1*SA2*dAlpha3PinchOS())/(MW*SB*SW) + (0.5625D0*CA22*CA3*EL*MH12*SA1*SA2*dAlpha3PinchOS())/(MW*SB*SW) - (0.5625D0*CA1&
  &2*CA22*CA3*EL*MH12*SA1*SA2*dAlpha3PinchOS())/(MW*SB*SW) + (0.09375D0*CA3*EL*MH22*SA1*SA2*dAlpha3PinchOS())/(MW*SB*SW) - (0.093&
  &75D0*CA12*CA3*EL*MH22*SA1*SA2*dAlpha3PinchOS())/(MW*SB*SW) + (0.28125D0*CA22*CA3*EL*MH22*SA1*SA2*dAlpha3PinchOS())/(MW*SB*SW) &
  &- (0.28125D0*CA12*CA22*CA3*EL*MH22*SA1*SA2*dAlpha3PinchOS())/(MW*SB*SW) + (0.28125D0*CA1*CA3*EL*m12squared*SA12*SA2*dAlpha3Pin&
  &chOS())/(CB2*MW*SB*SW) + (0.84375D0*CA1*CA22*CA3*EL*m12squared*SA12*SA2*dAlpha3PinchOS())/(CB2*MW*SB*SW) + (0.125D0*CA1*EL*MH1&
  &2*SA3*dAlpha3PinchOS())/(MW*SB*SW) + (0.125D0*CA1*CA22*EL*MH12*SA3*dAlpha3PinchOS())/(MW*SB*SW) + (0.0625D0*CA1*EL*MH22*SA3*dA&
  &lpha3PinchOS())/(MW*SB*SW) + (0.0625D0*CA1*CA22*EL*MH22*SA3*dAlpha3PinchOS())/(MW*SB*SW) - (0.21875D0*EL*m12squared*SA1*SA3*dA&
  &lpha3PinchOS())/(MW*SB*SW) - (0.21875D0*CA22*EL*m12squared*SA1*SA3*dAlpha3PinchOS())/(MW*SB*SW) + (0.15625D0*EL*m12squared*SA1&
  &*SA3*dAlpha3PinchOS())/(CB2*MW*SB*SW) + (0.5625D0*CA12*EL*m12squared*SA1*SA3*dAlpha3PinchOS())/(CB2*MW*SB*SW) + (0.15625D0*CA2&
  &2*EL*m12squared*SA1*SA3*dAlpha3PinchOS())/(CB2*MW*SB*SW) + (0.5625D0*CA12*CA22*EL*m12squared*SA1*SA3*dAlpha3PinchOS())/(CB2*MW&
  &*SB*SW) + (0.375D0*CA1*EL*MH12*SA12*SA3*dAlpha3PinchOS())/(MW*SB*SW) + (0.375D0*CA1*CA22*EL*MH12*SA12*SA3*dAlpha3PinchOS())/(M&
  &W*SB*SW) + (0.1875D0*CA1*EL*MH22*SA12*SA3*dAlpha3PinchOS())/(MW*SB*SW) + (0.1875D0*CA1*CA22*EL*MH22*SA12*SA3*dAlpha3PinchOS())&
  &/(MW*SB*SW) - (0.125D0*CA1*EL*MH12*SA22*SA3*dAlpha3PinchOS())/(MW*SB*SW) - (0.0625D0*CA1*EL*MH22*SA22*SA3*dAlpha3PinchOS())/(M&
  &W*SB*SW) + (0.21875D0*EL*m12squared*SA1*SA22*SA3*dAlpha3PinchOS())/(MW*SB*SW) - (0.15625D0*EL*m12squared*SA1*SA22*SA3*dAlpha3P&
  &inchOS())/(CB2*MW*SB*SW) - (0.5625D0*CA12*EL*m12squared*SA1*SA22*SA3*dAlpha3PinchOS())/(CB2*MW*SB*SW) - (0.375D0*CA1*EL*MH12*S&
  &A12*SA22*SA3*dAlpha3PinchOS())/(MW*SB*SW) - (0.1875D0*CA1*EL*MH22*SA12*SA22*SA3*dAlpha3PinchOS())/(MW*SB*SW) - (0.1875D0*CA3*E&
  &L*m12squared*SA1*SA2*dAlpha3PinchOS())/(CB*MW*SB2*SW) + (0.28125D0*CA12*CA3*EL*m12squared*SA1*SA2*dAlpha3PinchOS())/(CB*MW*SB2&
  &*SW) - (0.5625D0*CA22*CA3*EL*m12squared*SA1*SA2*dAlpha3PinchOS())/(CB*MW*SB2*SW) + (0.84375D0*CA12*CA22*CA3*EL*m12squared*SA1*&
  &SA2*dAlpha3PinchOS())/(CB*MW*SB2*SW) - (0.0625D0*CA1*EL*m12squared*SA3*dAlpha3PinchOS())/(CB*MW*SB2*SW) - (0.0625D0*CA1*CA22*E&
  &L*m12squared*SA3*dAlpha3PinchOS())/(CB*MW*SB2*SW) - (0.5625D0*CA1*EL*m12squared*SA12*SA3*dAlpha3PinchOS())/(CB*MW*SB2*SW) - (0&
  &.5625D0*CA1*CA22*EL*m12squared*SA12*SA3*dAlpha3PinchOS())/(CB*MW*SB2*SW) + (0.0625D0*CA1*EL*m12squared*SA22*SA3*dAlpha3PinchOS&
  &())/(CB*MW*SB2*SW) + (0.5625D0*CA1*EL*m12squared*SA12*SA22*SA3*dAlpha3PinchOS())/(CB*MW*SB2*SW) - (0.09375D0*CA3*EL*m12squared&
  &*SA1*SA2*dAlpha3PinchOS())/(MW*SB*SW*TB) - (0.28125D0*CA22*CA3*EL*m12squared*SA1*SA2*dAlpha3PinchOS())/(MW*SB*SW*TB) - (0.125D&
  &0*CA1*EL*m12squared*SA3*dAlpha3PinchOS())/(MW*SB*SW*TB) - (0.125D0*CA1*CA22*EL*m12squared*SA3*dAlpha3PinchOS())/(MW*SB*SW*TB) &
  &+ (0.125D0*CA1*EL*m12squared*SA22*SA3*dAlpha3PinchOS())/(MW*SB*SW*TB) - (0.09375D0*CA1*CA3*EL*m12squared*SA2*TB*dAlpha3PinchOS&
  &())/(CB*MW*SW) - (0.28125D0*CA1*CA22*CA3*EL*m12squared*SA2*TB*dAlpha3PinchOS())/(CB*MW*SW) + (0.03125D0*EL*m12squared*SA1*SA3*&
  &TB*dAlpha3PinchOS())/(CB*MW*SW) + (0.03125D0*CA22*EL*m12squared*SA1*SA3*TB*dAlpha3PinchOS())/(CB*MW*SW) - (0.03125D0*EL*m12squ&
  &ared*SA1*SA22*SA3*TB*dAlpha3PinchOS())/(CB*MW*SW) - (0.5D0*CA2*CA3*MH12*dAlpha3PinchOS())/vS - (0.25D0*CA2*CA3*MH22*dAlpha3Pin&
  &chOS())/vS - (1.5D0*CA2*CA3*MH12*SA22*dAlpha3PinchOS())/vS - (0.75D0*CA2*CA3*MH22*SA22*dAlpha3PinchOS())/vS + (0.015625D0*CA3*&
  &EL*m12squared*SA1*dBeta2PinchOS())/(CB*MW*SW) + (0.015625D0*CA22*CA3*EL*m12squared*SA1*dBeta2PinchOS())/(CB*MW*SW) - (0.015625&
  &D0*CA3*EL*m12squared*SA1*SA22*dBeta2PinchOS())/(CB*MW*SW) + (0.09375D0*CA1*EL*m12squared*SA2*SA3*dBeta2PinchOS())/(CB*MW*SW) +&
  & (0.28125D0*CA1*CA22*EL*m12squared*SA2*SA3*dBeta2PinchOS())/(CB*MW*SW) - (0.0625D0*CA1*CA3*EL*m12squared*dBeta2PinchOS())/(MW*&
  &SB*SW) - (0.0625D0*CA1*CA22*CA3*EL*m12squared*dBeta2PinchOS())/(MW*SB*SW) + (0.0625D0*CA1*CA3*EL*m12squared*dBeta2PinchOS())/(&
  &CB2*MW*SB*SW) + (0.0625D0*CA1*CA22*CA3*EL*m12squared*dBeta2PinchOS())/(CB2*MW*SB*SW) - (0.0625D0*CA3*EL*MH12*SA1*dBeta2PinchOS&
  &())/(MW*SB*SW) - (0.1875D0*CA12*CA3*EL*MH12*SA1*dBeta2PinchOS())/(MW*SB*SW) - (0.0625D0*CA22*CA3*EL*MH12*SA1*dBeta2PinchOS())/&
  &(MW*SB*SW) - (0.1875D0*CA12*CA22*CA3*EL*MH12*SA1*dBeta2PinchOS())/(MW*SB*SW) + (0.0625D0*CA3*EL*MH12*SA1*dBeta2PinchOS())/(CB2&
  &*MW*SB*SW) + (0.1875D0*CA12*CA3*EL*MH12*SA1*dBeta2PinchOS())/(CB2*MW*SB*SW) + (0.0625D0*CA22*CA3*EL*MH12*SA1*dBeta2PinchOS())/&
  &(CB2*MW*SB*SW) + (0.1875D0*CA12*CA22*CA3*EL*MH12*SA1*dBeta2PinchOS())/(CB2*MW*SB*SW) - (0.03125D0*CA3*EL*MH22*SA1*dBeta2PinchO&
  &S())/(MW*SB*SW) - (0.09375D0*CA12*CA3*EL*MH22*SA1*dBeta2PinchOS())/(MW*SB*SW) - (0.03125D0*CA22*CA3*EL*MH22*SA1*dBeta2PinchOS(&
  &))/(MW*SB*SW) - (0.09375D0*CA12*CA22*CA3*EL*MH22*SA1*dBeta2PinchOS())/(MW*SB*SW) + (0.03125D0*CA3*EL*MH22*SA1*dBeta2PinchOS())&
  &/(CB2*MW*SB*SW) + (0.09375D0*CA12*CA3*EL*MH22*SA1*dBeta2PinchOS())/(CB2*MW*SB*SW) + (0.03125D0*CA22*CA3*EL*MH22*SA1*dBeta2Pinc&
  &hOS())/(CB2*MW*SB*SW) + (0.09375D0*CA12*CA22*CA3*EL*MH22*SA1*dBeta2PinchOS())/(CB2*MW*SB*SW) + (0.5625D0*CA1*CA3*EL*m12squared&
  &*SA12*dBeta2PinchOS())/(CB2*MW*SB*SW) + (0.5625D0*CA1*CA22*CA3*EL*m12squared*SA12*dBeta2PinchOS())/(CB2*MW*SB*SW) + (0.0625D0*&
  &CA1*CA3*EL*m12squared*SA22*dBeta2PinchOS())/(MW*SB*SW) - (0.0625D0*CA1*CA3*EL*m12squared*SA22*dBeta2PinchOS())/(CB2*MW*SB*SW) &
  &+ (0.0625D0*CA3*EL*MH12*SA1*SA22*dBeta2PinchOS())/(MW*SB*SW) + (0.1875D0*CA12*CA3*EL*MH12*SA1*SA22*dBeta2PinchOS())/(MW*SB*SW)&
  & - (0.0625D0*CA3*EL*MH12*SA1*SA22*dBeta2PinchOS())/(CB2*MW*SB*SW) - (0.1875D0*CA12*CA3*EL*MH12*SA1*SA22*dBeta2PinchOS())/(CB2*&
  &MW*SB*SW) + (0.03125D0*CA3*EL*MH22*SA1*SA22*dBeta2PinchOS())/(MW*SB*SW) + (0.09375D0*CA12*CA3*EL*MH22*SA1*SA22*dBeta2PinchOS()&
  &)/(MW*SB*SW) - (0.03125D0*CA3*EL*MH22*SA1*SA22*dBeta2PinchOS())/(CB2*MW*SB*SW) - (0.09375D0*CA12*CA3*EL*MH22*SA1*SA22*dBeta2Pi&
  &nchOS())/(CB2*MW*SB*SW) - (0.5625D0*CA1*CA3*EL*m12squared*SA12*SA22*dBeta2PinchOS())/(CB2*MW*SB*SW) - (0.09375D0*CA1*EL*MH12*S&
  &A2*SA3*dBeta2PinchOS())/(MW*SB*SW) - (0.28125D0*CA1*CA22*EL*MH12*SA2*SA3*dBeta2PinchOS())/(MW*SB*SW) + (0.09375D0*CA1*EL*MH12*&
  &SA2*SA3*dBeta2PinchOS())/(CB2*MW*SB*SW) + (0.28125D0*CA1*CA22*EL*MH12*SA2*SA3*dBeta2PinchOS())/(CB2*MW*SB*SW) - (0.046875D0*CA&
  &1*EL*MH22*SA2*SA3*dBeta2PinchOS())/(MW*SB*SW) - (0.140625D0*CA1*CA22*EL*MH22*SA2*SA3*dBeta2PinchOS())/(MW*SB*SW) + (0.046875D0&
  &*CA1*EL*MH22*SA2*SA3*dBeta2PinchOS())/(CB2*MW*SB*SW) + (0.140625D0*CA1*CA22*EL*MH22*SA2*SA3*dBeta2PinchOS())/(CB2*MW*SB*SW) - &
  &(0.140625D0*EL*m12squared*SA1*SA2*SA3*dBeta2PinchOS())/(MW*SB*SW) - (0.421875D0*CA22*EL*m12squared*SA1*SA2*SA3*dBeta2PinchOS()&
  &)/(MW*SB*SW) - (0.046875D0*EL*m12squared*SA1*SA2*SA3*dBeta2PinchOS())/(CB2*MW*SB*SW) + (0.28125D0*CA12*EL*m12squared*SA1*SA2*S&
  &A3*dBeta2PinchOS())/(CB2*MW*SB*SW) - (0.140625D0*CA22*EL*m12squared*SA1*SA2*SA3*dBeta2PinchOS())/(CB2*MW*SB*SW) + (0.84375D0*C&
  &A12*CA22*EL*m12squared*SA1*SA2*SA3*dBeta2PinchOS())/(CB2*MW*SB*SW) + (0.09375D0*CA1*EL*MH12*SA12*SA2*SA3*dBeta2PinchOS())/(MW*&
  &SB*SW) + (0.28125D0*CA1*CA22*EL*MH12*SA12*SA2*SA3*dBeta2PinchOS())/(MW*SB*SW) - (0.09375D0*CA1*EL*MH12*SA12*SA2*SA3*dBeta2Pinc&
  &hOS())/(CB2*MW*SB*SW) - (0.28125D0*CA1*CA22*EL*MH12*SA12*SA2*SA3*dBeta2PinchOS())/(CB2*MW*SB*SW) + (0.046875D0*CA1*EL*MH22*SA1&
  &2*SA2*SA3*dBeta2PinchOS())/(MW*SB*SW) + (0.140625D0*CA1*CA22*EL*MH22*SA12*SA2*SA3*dBeta2PinchOS())/(MW*SB*SW) - (0.046875D0*CA&
  &1*EL*MH22*SA12*SA2*SA3*dBeta2PinchOS())/(CB2*MW*SB*SW) - (0.140625D0*CA1*CA22*EL*MH22*SA12*SA2*SA3*dBeta2PinchOS())/(CB2*MW*SB&
  &*SW) + (0.203125D0*CA3*EL*m12squared*SA1*dBeta2PinchOS())/(CB*MW*SB2*SW) + (0.84375D0*CA12*CA3*EL*m12squared*SA1*dBeta2PinchOS&
  &())/(CB*MW*SB2*SW) + (0.203125D0*CA22*CA3*EL*m12squared*SA1*dBeta2PinchOS())/(CB*MW*SB2*SW) + (0.84375D0*CA12*CA22*CA3*EL*m12s&
  &quared*SA1*dBeta2PinchOS())/(CB*MW*SB2*SW) - (0.203125D0*CA3*EL*m12squared*SA1*SA22*dBeta2PinchOS())/(CB*MW*SB2*SW) - (0.84375&
  &D0*CA12*CA3*EL*m12squared*SA1*SA22*dBeta2PinchOS())/(CB*MW*SB2*SW) + (0.10546875D0*CA1*EL*m12squared*SA2*SA3*dBeta2PinchOS())/&
  &(CB*MW*SB2*SW) + (0.31640625D0*CA1*CA22*EL*m12squared*SA2*SA3*dBeta2PinchOS())/(CB*MW*SB2*SW) - (0.38671875D0*CA1*EL*m12square&
  &d*SA12*SA2*SA3*dBeta2PinchOS())/(CB*MW*SB2*SW) - (1.16015625D0*CA1*CA22*EL*m12squared*SA12*SA2*SA3*dBeta2PinchOS())/(CB*MW*SB2&
  &*SW) + (0.125D0*CA1*CA3*EL*MH12*dBeta2PinchOS())/(MW*SB*SW*TB) + (0.125D0*CA1*CA22*CA3*EL*MH12*dBeta2PinchOS())/(MW*SB*SW*TB) &
  &+ (0.0625D0*CA1*CA3*EL*MH22*dBeta2PinchOS())/(MW*SB*SW*TB) + (0.0625D0*CA1*CA22*CA3*EL*MH22*dBeta2PinchOS())/(MW*SB*SW*TB) - (&
  &0.203125D0*CA3*EL*m12squared*SA1*dBeta2PinchOS())/(MW*SB*SW*TB) - (0.203125D0*CA22*CA3*EL*m12squared*SA1*dBeta2PinchOS())/(MW*&
  &SB*SW*TB) + (0.375D0*CA1*CA3*EL*MH12*SA12*dBeta2PinchOS())/(MW*SB*SW*TB) + (0.375D0*CA1*CA22*CA3*EL*MH12*SA12*dBeta2PinchOS())&
  &/(MW*SB*SW*TB) + (0.1875D0*CA1*CA3*EL*MH22*SA12*dBeta2PinchOS())/(MW*SB*SW*TB) + (0.1875D0*CA1*CA22*CA3*EL*MH22*SA12*dBeta2Pin&
  &chOS())/(MW*SB*SW*TB) - (0.125D0*CA1*CA3*EL*MH12*SA22*dBeta2PinchOS())/(MW*SB*SW*TB) - (0.0625D0*CA1*CA3*EL*MH22*SA22*dBeta2Pi&
  &nchOS())/(MW*SB*SW*TB) + (0.203125D0*CA3*EL*m12squared*SA1*SA22*dBeta2PinchOS())/(MW*SB*SW*TB) - (0.375D0*CA1*CA3*EL*MH12*SA12&
  &*SA22*dBeta2PinchOS())/(MW*SB*SW*TB) - (0.1875D0*CA1*CA3*EL*MH22*SA12*SA22*dBeta2PinchOS())/(MW*SB*SW*TB) - (0.140625D0*CA1*EL&
  &*m12squared*SA2*SA3*dBeta2PinchOS())/(MW*SB*SW*TB) - (0.421875D0*CA1*CA22*EL*m12squared*SA2*SA3*dBeta2PinchOS())/(MW*SB*SW*TB)&
  & - (0.1875D0*EL*MH12*SA1*SA2*SA3*dBeta2PinchOS())/(MW*SB*SW*TB) + (0.1875D0*CA12*EL*MH12*SA1*SA2*SA3*dBeta2PinchOS())/(MW*SB*S&
  &W*TB) - (0.5625D0*CA22*EL*MH12*SA1*SA2*SA3*dBeta2PinchOS())/(MW*SB*SW*TB) + (0.5625D0*CA12*CA22*EL*MH12*SA1*SA2*SA3*dBeta2Pinc&
  &hOS())/(MW*SB*SW*TB) - (0.09375D0*EL*MH22*SA1*SA2*SA3*dBeta2PinchOS())/(MW*SB*SW*TB) + (0.09375D0*CA12*EL*MH22*SA1*SA2*SA3*dBe&
  &ta2PinchOS())/(MW*SB*SW*TB) - (0.28125D0*CA22*EL*MH22*SA1*SA2*SA3*dBeta2PinchOS())/(MW*SB*SW*TB) + (0.28125D0*CA12*CA22*EL*MH2&
  &2*SA1*SA2*SA3*dBeta2PinchOS())/(MW*SB*SW*TB) - (0.125D0*CA1*CA3*EL*m12squared*TB*dBeta2PinchOS())/(CB*MW*SW) - (0.125D0*CA1*CA&
  &22*CA3*EL*m12squared*TB*dBeta2PinchOS())/(CB*MW*SW) + (0.0625D0*CA3*EL*MH12*SA1*TB*dBeta2PinchOS())/(CB*MW*SW) + (0.1875D0*CA1&
  &2*CA3*EL*MH12*SA1*TB*dBeta2PinchOS())/(CB*MW*SW) + (0.0625D0*CA22*CA3*EL*MH12*SA1*TB*dBeta2PinchOS())/(CB*MW*SW) + (0.1875D0*C&
  &A12*CA22*CA3*EL*MH12*SA1*TB*dBeta2PinchOS())/(CB*MW*SW) + (0.03125D0*CA3*EL*MH22*SA1*TB*dBeta2PinchOS())/(CB*MW*SW) + (0.09375&
  &D0*CA12*CA3*EL*MH22*SA1*TB*dBeta2PinchOS())/(CB*MW*SW) + (0.03125D0*CA22*CA3*EL*MH22*SA1*TB*dBeta2PinchOS())/(CB*MW*SW) + (0.0&
  &9375D0*CA12*CA22*CA3*EL*MH22*SA1*TB*dBeta2PinchOS())/(CB*MW*SW) + (0.125D0*CA1*CA3*EL*m12squared*SA22*TB*dBeta2PinchOS())/(CB*&
  &MW*SW) - (0.0625D0*CA3*EL*MH12*SA1*SA22*TB*dBeta2PinchOS())/(CB*MW*SW) - (0.1875D0*CA12*CA3*EL*MH12*SA1*SA22*TB*dBeta2PinchOS(&
  &))/(CB*MW*SW) - (0.03125D0*CA3*EL*MH22*SA1*SA22*TB*dBeta2PinchOS())/(CB*MW*SW) - (0.09375D0*CA12*CA3*EL*MH22*SA1*SA22*TB*dBeta&
  &2PinchOS())/(CB*MW*SW) + (0.09375D0*CA1*EL*MH12*SA2*SA3*TB*dBeta2PinchOS())/(CB*MW*SW) + (0.28125D0*CA1*CA22*EL*MH12*SA2*SA3*T&
  &B*dBeta2PinchOS())/(CB*MW*SW) + (0.046875D0*CA1*EL*MH22*SA2*SA3*TB*dBeta2PinchOS())/(CB*MW*SW) + (0.140625D0*CA1*CA22*EL*MH22*&
  &SA2*SA3*TB*dBeta2PinchOS())/(CB*MW*SW) + (0.140625D0*EL*m12squared*SA1*SA2*SA3*TB*dBeta2PinchOS())/(CB*MW*SW) + (0.421875D0*CA&
  &22*EL*m12squared*SA1*SA2*SA3*TB*dBeta2PinchOS())/(CB*MW*SW) - (0.09375D0*CA1*EL*MH12*SA12*SA2*SA3*TB*dBeta2PinchOS())/(CB*MW*S&
  &W) - (0.28125D0*CA1*CA22*EL*MH12*SA12*SA2*SA3*TB*dBeta2PinchOS())/(CB*MW*SW) - (0.046875D0*CA1*EL*MH22*SA12*SA2*SA3*TB*dBeta2P&
  &inchOS())/(CB*MW*SW) - (0.140625D0*CA1*CA22*EL*MH22*SA12*SA2*SA3*TB*dBeta2PinchOS())/(CB*MW*SW) - (0.1875D0*CA1*CA3*EL*m12squa&
  &red*dBeta2PinchOS())/(MW*SB*SW*TB2) - (0.1875D0*CA1*CA22*CA3*EL*m12squared*dBeta2PinchOS())/(MW*SB*SW*TB2) + (0.1875D0*CA1*CA3&
  &*EL*m12squared*SA22*dBeta2PinchOS())/(MW*SB*SW*TB2) + (0.09375D0*EL*m12squared*SA1*SA2*SA3*dBeta2PinchOS())/(MW*SB*SW*TB2) + (&
  &0.28125D0*CA22*EL*m12squared*SA1*SA2*SA3*dBeta2PinchOS())/(MW*SB*SW*TB2) - (0.03125D0*CA3*EL*m12squared*SA1*TB2*dBeta2PinchOS(&
  &))/(CB*MW*SW) - (0.03125D0*CA22*CA3*EL*m12squared*SA1*TB2*dBeta2PinchOS())/(CB*MW*SW) + (0.03125D0*CA3*EL*m12squared*SA1*SA22*&
  &TB2*dBeta2PinchOS())/(CB*MW*SW) - (0.140625D0*CA1*EL*m12squared*SA2*SA3*TB2*dBeta2PinchOS())/(CB*MW*SW) - (0.421875D0*CA1*CA22&
  &*EL*m12squared*SA2*SA3*TB2*dBeta2PinchOS())/(CB*MW*SW) + (0.375D0*CA3*EL*MH12*dAlpha1PinchOS()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW&
  &) + (0.375D0*CA22*CA3*EL*MH12*dAlpha1PinchOS()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.1875D0*CA3*EL*MH22*dAlpha1PinchOS()*DBLE(&
  &CA1**INT(3.D0)))/(CB*MW*SW) + (0.1875D0*CA22*CA3*EL*MH22*dAlpha1PinchOS()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.375D0*CA3*EL*M&
  &H12*SA22*dAlpha1PinchOS()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.1875D0*CA3*EL*MH22*SA22*dAlpha1PinchOS()*DBLE(CA1**INT(3.D0)))&
  &/(CB*MW*SW) - (0.5625D0*CA3*EL*m12squared*dAlpha1PinchOS()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.5625D0*CA22*CA3*EL*m12squ&
  &ared*dAlpha1PinchOS()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) + (0.5625D0*CA3*EL*m12squared*SA22*dAlpha1PinchOS()*DBLE(CA1**INT(3&
  &.D0)))/(CB2*MW*SB*SW) - (0.1875D0*EL*MH12*SA2*SA3*dAlpha1PinchOS()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.5625D0*CA22*EL*MH12*S&
  &A2*SA3*dAlpha1PinchOS()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.09375D0*EL*MH22*SA2*SA3*dAlpha1PinchOS()*DBLE(CA1**INT(3.D0)))/(&
  &MW*SB*SW) - (0.28125D0*CA22*EL*MH22*SA2*SA3*dAlpha1PinchOS()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) + (0.28125D0*EL*m12squared*SA2*S&
  &A3*dAlpha1PinchOS()*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.84375D0*CA22*EL*m12squared*SA2*SA3*dAlpha1PinchOS()*DBLE(CA1**IN&
  &T(3.D0)))/(CB*MW*SB2*SW) + (0.0625D0*CA2*EL*MH12*SA3*dAlpha2PinchOS()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.03125D0*CA2*EL*MH2&
  &2*SA3*dAlpha2PinchOS()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.5625D0*CA2*EL*MH12*SA22*SA3*dAlpha2PinchOS()*DBLE(CA1**INT(3.D0))&
  &)/(CB*MW*SW) - (0.28125D0*CA2*EL*MH22*SA22*SA3*dAlpha2PinchOS()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.5D0*CA2*CA3*EL*MH12*SA2*&
  &dAlpha2PinchOS()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.25D0*CA2*CA3*EL*MH22*SA2*dAlpha2PinchOS()*DBLE(CA1**INT(3.D0)))/(MW*SB*&
  &SW) - (0.09375D0*CA2*EL*m12squared*SA3*dAlpha2PinchOS()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) + (0.84375D0*CA2*EL*m12squared*SA&
  &22*SA3*dAlpha2PinchOS()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) + (0.75D0*CA2*CA3*EL*m12squared*SA2*dAlpha2PinchOS()*DBLE(CA1**IN&
  &T(3.D0)))/(CB*MW*SB2*SW) + (0.0625D0*CA3*EL*MH12*SA2*dAlpha3PinchOS()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.1875D0*CA22*CA3*EL&
  &*MH12*SA2*dAlpha3PinchOS()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.03125D0*CA3*EL*MH22*SA2*dAlpha3PinchOS()*DBLE(CA1**INT(3.D0))&
  &)/(CB*MW*SW) + (0.09375D0*CA22*CA3*EL*MH22*SA2*dAlpha3PinchOS()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.09375D0*CA3*EL*m12square&
  &d*SA2*dAlpha3PinchOS()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.28125D0*CA22*CA3*EL*m12squared*SA2*dAlpha3PinchOS()*DBLE(CA1*&
  &*INT(3.D0)))/(CB2*MW*SB*SW) - (0.125D0*EL*MH12*SA3*dAlpha3PinchOS()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.125D0*CA22*EL*MH12*S&
  &A3*dAlpha3PinchOS()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.0625D0*EL*MH22*SA3*dAlpha3PinchOS()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW)&
  & - (0.0625D0*CA22*EL*MH22*SA3*dAlpha3PinchOS()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) + (0.125D0*EL*MH12*SA22*SA3*dAlpha3PinchOS()*D&
  &BLE(CA1**INT(3.D0)))/(MW*SB*SW) + (0.0625D0*EL*MH22*SA22*SA3*dAlpha3PinchOS()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) + (0.1875D0*EL*&
  &m12squared*SA3*dAlpha3PinchOS()*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.1875D0*CA22*EL*m12squared*SA3*dAlpha3PinchOS()*DBLE(&
  &CA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.1875D0*EL*m12squared*SA22*SA3*dAlpha3PinchOS()*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.&
  &1875D0*CA3*EL*m12squared*dBeta2PinchOS()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.1875D0*CA22*CA3*EL*m12squared*dBeta2PinchOS&
  &()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) + (0.1875D0*CA3*EL*m12squared*SA22*dBeta2PinchOS()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW&
  &) - (0.03125D0*EL*MH12*SA2*SA3*dBeta2PinchOS()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.09375D0*CA22*EL*MH12*SA2*SA3*dBeta2PinchO&
  &S()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) + (0.03125D0*EL*MH12*SA2*SA3*dBeta2PinchOS()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) + (0.09&
  &375D0*CA22*EL*MH12*SA2*SA3*dBeta2PinchOS()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.015625D0*EL*MH22*SA2*SA3*dBeta2PinchOS()*&
  &DBLE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.046875D0*CA22*EL*MH22*SA2*SA3*dBeta2PinchOS()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) + (0.0156&
  &25D0*EL*MH22*SA2*SA3*dBeta2PinchOS()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) + (0.046875D0*CA22*EL*MH22*SA2*SA3*dBeta2PinchOS()*D&
  &BLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) + (0.12890625D0*EL*m12squared*SA2*SA3*dBeta2PinchOS()*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW)&
  & + (0.38671875D0*CA22*EL*m12squared*SA2*SA3*dBeta2PinchOS()*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.125D0*CA3*EL*MH12*dBeta2&
  &PinchOS()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW*TB) - (0.125D0*CA22*CA3*EL*MH12*dBeta2PinchOS()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW*TB) &
  &- (0.0625D0*CA3*EL*MH22*dBeta2PinchOS()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW*TB) - (0.0625D0*CA22*CA3*EL*MH22*dBeta2PinchOS()*DBLE(&
  &CA1**INT(3.D0)))/(MW*SB*SW*TB) + (0.125D0*CA3*EL*MH12*SA22*dBeta2PinchOS()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW*TB) + (0.0625D0*CA3&
  &*EL*MH22*SA22*dBeta2PinchOS()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW*TB) + (0.03125D0*EL*MH12*SA2*SA3*TB*dBeta2PinchOS()*DBLE(CA1**IN&
  &T(3.D0)))/(CB*MW*SW) + (0.09375D0*CA22*EL*MH12*SA2*SA3*TB*dBeta2PinchOS()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.015625D0*EL*MH&
  &22*SA2*SA3*TB*dBeta2PinchOS()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.046875D0*CA22*EL*MH22*SA2*SA3*TB*dBeta2PinchOS()*DBLE(CA1*&
  &*INT(3.D0)))/(CB*MW*SW) + (0.5625D0*CA1*EL*MH12*SA3*dAlpha2PinchOS()*DBLE(CA2**INT(3.D0)))/(CB*MW*SW) + (0.28125D0*CA1*EL*MH22&
  &*SA3*dAlpha2PinchOS()*DBLE(CA2**INT(3.D0)))/(CB*MW*SW) + (0.84375D0*EL*m12squared*SA1*SA3*dAlpha2PinchOS()*DBLE(CA2**INT(3.D0)&
  &))/(CB*MW*SW) - (0.5625D0*CA1*EL*MH12*SA12*SA3*dAlpha2PinchOS()*DBLE(CA2**INT(3.D0)))/(CB*MW*SW) - (0.28125D0*CA1*EL*MH22*SA12&
  &*SA3*dAlpha2PinchOS()*DBLE(CA2**INT(3.D0)))/(CB*MW*SW) + (0.84375D0*CA1*EL*m12squared*SA3*dAlpha2PinchOS()*DBLE(CA2**INT(3.D0)&
  &))/(MW*SB*SW) - (0.5625D0*CA1*EL*m12squared*SA3*dAlpha2PinchOS()*DBLE(CA2**INT(3.D0)))/(CB2*MW*SB*SW) + (0.5625D0*EL*MH12*SA1*&
  &SA3*dAlpha2PinchOS()*DBLE(CA2**INT(3.D0)))/(MW*SB*SW) - (0.5625D0*CA12*EL*MH12*SA1*SA3*dAlpha2PinchOS()*DBLE(CA2**INT(3.D0)))/&
  &(MW*SB*SW) + (0.28125D0*EL*MH22*SA1*SA3*dAlpha2PinchOS()*DBLE(CA2**INT(3.D0)))/(MW*SB*SW) - (0.28125D0*CA12*EL*MH22*SA1*SA3*dA&
  &lpha2PinchOS()*DBLE(CA2**INT(3.D0)))/(MW*SB*SW) + (0.84375D0*CA1*EL*m12squared*SA12*SA3*dAlpha2PinchOS()*DBLE(CA2**INT(3.D0)))&
  &/(CB2*MW*SB*SW) - (0.5625D0*EL*m12squared*SA1*SA3*dAlpha2PinchOS()*DBLE(CA2**INT(3.D0)))/(CB*MW*SB2*SW) + (0.84375D0*CA12*EL*m&
  &12squared*SA1*SA3*dAlpha2PinchOS()*DBLE(CA2**INT(3.D0)))/(CB*MW*SB2*SW) - (0.28125D0*EL*m12squared*SA1*SA3*dAlpha2PinchOS()*DB&
  &LE(CA2**INT(3.D0)))/(MW*SB*SW*TB) - (0.28125D0*CA1*EL*m12squared*SA3*TB*dAlpha2PinchOS()*DBLE(CA2**INT(3.D0)))/(CB*MW*SW) + (0&
  &.5D0*CA3*MH12*dAlpha3PinchOS()*DBLE(CA2**INT(3.D0)))/vS + (0.25D0*CA3*MH22*dAlpha3PinchOS()*DBLE(CA2**INT(3.D0)))/vS + (0.1875&
  &D0*EL*MH12*SA3*dAlpha2PinchOS()*DBLE(CA1**INT(3.D0))*DBLE(CA2**INT(3.D0)))/(CB*MW*SW) + (0.09375D0*EL*MH22*SA3*dAlpha2PinchOS(&
  &)*DBLE(CA1**INT(3.D0))*DBLE(CA2**INT(3.D0)))/(CB*MW*SW) - (0.28125D0*EL*m12squared*SA3*dAlpha2PinchOS()*DBLE(CA1**INT(3.D0))*D&
  &BLE(CA2**INT(3.D0)))/(CB2*MW*SB*SW) - (0.28125D0*CA3*EL*m12squared*SA1*dBeta2PinchOS()*DBLE(CB**INT(-3.D0)))/(MW*SW) - (0.8437&
  &5D0*CA12*CA3*EL*m12squared*SA1*dBeta2PinchOS()*DBLE(CB**INT(-3.D0)))/(MW*SW) - (0.28125D0*CA22*CA3*EL*m12squared*SA1*dBeta2Pin&
  &chOS()*DBLE(CB**INT(-3.D0)))/(MW*SW) - (0.84375D0*CA12*CA22*CA3*EL*m12squared*SA1*dBeta2PinchOS()*DBLE(CB**INT(-3.D0)))/(MW*SW&
  &) + (0.28125D0*CA3*EL*m12squared*SA1*SA22*dBeta2PinchOS()*DBLE(CB**INT(-3.D0)))/(MW*SW) + (0.84375D0*CA12*CA3*EL*m12squared*SA&
  &1*SA22*dBeta2PinchOS()*DBLE(CB**INT(-3.D0)))/(MW*SW) - (0.36328125D0*CA1*EL*m12squared*SA2*SA3*dBeta2PinchOS()*DBLE(CB**INT(-3&
  &.D0)))/(MW*SW) - (1.08984375D0*CA1*CA22*EL*m12squared*SA2*SA3*dBeta2PinchOS()*DBLE(CB**INT(-3.D0)))/(MW*SW) + (0.45703125D0*CA&
  &1*EL*m12squared*SA12*SA2*SA3*dBeta2PinchOS()*DBLE(CB**INT(-3.D0)))/(MW*SW) + (1.37109375D0*CA1*CA22*EL*m12squared*SA12*SA2*SA3&
  &*dBeta2PinchOS()*DBLE(CB**INT(-3.D0)))/(MW*SW) - (0.0625D0*CA3*EL*m12squared*SA1*dBeta2PinchOS()*DBLE(CB**INT(-3.D0)))/(MW*SB2&
  &*SW) - (0.28125D0*CA12*CA3*EL*m12squared*SA1*dBeta2PinchOS()*DBLE(CB**INT(-3.D0)))/(MW*SB2*SW) - (0.0625D0*CA22*CA3*EL*m12squa&
  &red*SA1*dBeta2PinchOS()*DBLE(CB**INT(-3.D0)))/(MW*SB2*SW) - (0.28125D0*CA12*CA22*CA3*EL*m12squared*SA1*dBeta2PinchOS()*DBLE(CB&
  &**INT(-3.D0)))/(MW*SB2*SW) + (0.0625D0*CA3*EL*m12squared*SA1*SA22*dBeta2PinchOS()*DBLE(CB**INT(-3.D0)))/(MW*SB2*SW) + (0.28125&
  &D0*CA12*CA3*EL*m12squared*SA1*SA22*dBeta2PinchOS()*DBLE(CB**INT(-3.D0)))/(MW*SB2*SW) - (0.05859375D0*CA1*EL*m12squared*SA2*SA3&
  &*dBeta2PinchOS()*DBLE(CB**INT(-3.D0)))/(MW*SB2*SW) - (0.17578125D0*CA1*CA22*EL*m12squared*SA2*SA3*dBeta2PinchOS()*DBLE(CB**INT&
  &(-3.D0)))/(MW*SB2*SW) + (0.10546875D0*CA1*EL*m12squared*SA12*SA2*SA3*dBeta2PinchOS()*DBLE(CB**INT(-3.D0)))/(MW*SB2*SW) + (0.31&
  &640625D0*CA1*CA22*EL*m12squared*SA12*SA2*SA3*dBeta2PinchOS()*DBLE(CB**INT(-3.D0)))/(MW*SB2*SW) - (0.15234375D0*EL*m12squared*S&
  &A2*SA3*dBeta2PinchOS()*DBLE(CA1**INT(3.D0))*DBLE(CB**INT(-3.D0)))/(MW*SW) - (0.45703125D0*CA22*EL*m12squared*SA2*SA3*dBeta2Pin&
  &chOS()*DBLE(CA1**INT(3.D0))*DBLE(CB**INT(-3.D0)))/(MW*SW) - (0.03515625D0*EL*m12squared*SA2*SA3*dBeta2PinchOS()*DBLE(CA1**INT(&
  &3.D0))*DBLE(CB**INT(-3.D0)))/(MW*SB2*SW) - (0.10546875D0*CA22*EL*m12squared*SA2*SA3*dBeta2PinchOS()*DBLE(CA1**INT(3.D0))*DBLE(&
  &CB**INT(-3.D0)))/(MW*SB2*SW) + (0.1875D0*EL*MH12*SA2*SA3*dAlpha1PinchOS()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.5625D0*CA22*EL&
  &*MH12*SA2*SA3*dAlpha1PinchOS()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.09375D0*EL*MH22*SA2*SA3*dAlpha1PinchOS()*DBLE(SA1**INT(3.&
  &D0)))/(CB*MW*SW) + (0.28125D0*CA22*EL*MH22*SA2*SA3*dAlpha1PinchOS()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.375D0*CA3*EL*MH12*dA&
  &lpha1PinchOS()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.375D0*CA22*CA3*EL*MH12*dAlpha1PinchOS()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) &
  &+ (0.1875D0*CA3*EL*MH22*dAlpha1PinchOS()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.1875D0*CA22*CA3*EL*MH22*dAlpha1PinchOS()*DBLE(S&
  &A1**INT(3.D0)))/(MW*SB*SW) - (0.375D0*CA3*EL*MH12*SA22*dAlpha1PinchOS()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0.1875D0*CA3*EL*MH&
  &22*SA22*dAlpha1PinchOS()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0.28125D0*EL*m12squared*SA2*SA3*dAlpha1PinchOS()*DBLE(SA1**INT(3.&
  &D0)))/(CB2*MW*SB*SW) - (0.84375D0*CA22*EL*m12squared*SA2*SA3*dAlpha1PinchOS()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.5625D0&
  &*CA3*EL*m12squared*dAlpha1PinchOS()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.5625D0*CA22*CA3*EL*m12squared*dAlpha1PinchOS()*D&
  &BLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.5625D0*CA3*EL*m12squared*SA22*dAlpha1PinchOS()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) +&
  & (0.5D0*CA2*CA3*EL*MH12*SA2*dAlpha2PinchOS()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.25D0*CA2*CA3*EL*MH22*SA2*dAlpha2PinchOS()*D&
  &BLE(SA1**INT(3.D0)))/(CB*MW*SW) - (0.75D0*CA2*CA3*EL*m12squared*SA2*dAlpha2PinchOS()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) + (0&
  &.0625D0*CA2*EL*MH12*SA3*dAlpha2PinchOS()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.03125D0*CA2*EL*MH22*SA3*dAlpha2PinchOS()*DBLE(S&
  &A1**INT(3.D0)))/(MW*SB*SW) - (0.5625D0*CA2*EL*MH12*SA22*SA3*dAlpha2PinchOS()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0.28125D0*CA2&
  &*EL*MH22*SA22*SA3*dAlpha2PinchOS()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0.09375D0*CA2*EL*m12squared*SA3*dAlpha2PinchOS()*DBLE(S&
  &A1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.84375D0*CA2*EL*m12squared*SA22*SA3*dAlpha2PinchOS()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) +&
  & (0.125D0*EL*MH12*SA3*dAlpha3PinchOS()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.125D0*CA22*EL*MH12*SA3*dAlpha3PinchOS()*DBLE(SA1*&
  &*INT(3.D0)))/(CB*MW*SW) + (0.0625D0*EL*MH22*SA3*dAlpha3PinchOS()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.0625D0*CA22*EL*MH22*SA3&
  &*dAlpha3PinchOS()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) - (0.125D0*EL*MH12*SA22*SA3*dAlpha3PinchOS()*DBLE(SA1**INT(3.D0)))/(CB*MW*S&
  &W) - (0.0625D0*EL*MH22*SA22*SA3*dAlpha3PinchOS()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.0625D0*CA3*EL*MH12*SA2*dAlpha3PinchOS()&
  &*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.1875D0*CA22*CA3*EL*MH12*SA2*dAlpha3PinchOS()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.0312&
  &5D0*CA3*EL*MH22*SA2*dAlpha3PinchOS()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.09375D0*CA22*CA3*EL*MH22*SA2*dAlpha3PinchOS()*DBLE(&
  &SA1**INT(3.D0)))/(MW*SB*SW) - (0.1875D0*EL*m12squared*SA3*dAlpha3PinchOS()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.1875D0*CA&
  &22*EL*m12squared*SA3*dAlpha3PinchOS()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) + (0.1875D0*EL*m12squared*SA22*SA3*dAlpha3PinchOS()&
  &*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.09375D0*CA3*EL*m12squared*SA2*dAlpha3PinchOS()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW)&
  & - (0.28125D0*CA22*CA3*EL*m12squared*SA2*dAlpha3PinchOS()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.0625D0*CA3*EL*MH12*dBeta2P&
  &inchOS()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.0625D0*CA22*CA3*EL*MH12*dBeta2PinchOS()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0.0&
  &625D0*CA3*EL*MH12*dBeta2PinchOS()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.0625D0*CA22*CA3*EL*MH12*dBeta2PinchOS()*DBLE(SA1**&
  &INT(3.D0)))/(CB2*MW*SB*SW) + (0.03125D0*CA3*EL*MH22*dBeta2PinchOS()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.03125D0*CA22*CA3*EL*&
  &MH22*dBeta2PinchOS()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0.03125D0*CA3*EL*MH22*dBeta2PinchOS()*DBLE(SA1**INT(3.D0)))/(CB2*MW*S&
  &B*SW) - (0.03125D0*CA22*CA3*EL*MH22*dBeta2PinchOS()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.0625D0*CA3*EL*MH12*SA22*dBeta2Pi&
  &nchOS()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.0625D0*CA3*EL*MH12*SA22*dBeta2PinchOS()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) - (&
  &0.03125D0*CA3*EL*MH22*SA22*dBeta2PinchOS()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.03125D0*CA3*EL*MH22*SA22*dBeta2PinchOS()*DBLE&
  &(SA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.09375D0*EL*m12squared*SA2*SA3*dBeta2PinchOS()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.&
  &28125D0*CA22*EL*m12squared*SA2*SA3*dBeta2PinchOS()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.28125D0*CA3*EL*m12squared*dBeta2P&
  &inchOS()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.28125D0*CA22*CA3*EL*m12squared*dBeta2PinchOS()*DBLE(SA1**INT(3.D0)))/(CB*MW&
  &*SB2*SW) + (0.28125D0*CA3*EL*m12squared*SA22*dBeta2PinchOS()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.0625D0*EL*MH12*SA2*SA3*&
  &dBeta2PinchOS()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW*TB) - (0.1875D0*CA22*EL*MH12*SA2*SA3*dBeta2PinchOS()*DBLE(SA1**INT(3.D0)))/(MW&
  &*SB*SW*TB) - (0.03125D0*EL*MH22*SA2*SA3*dBeta2PinchOS()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW*TB) - (0.09375D0*CA22*EL*MH22*SA2*SA3*&
  &dBeta2PinchOS()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW*TB) - (0.0625D0*CA3*EL*MH12*TB*dBeta2PinchOS()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW&
  &) - (0.0625D0*CA22*CA3*EL*MH12*TB*dBeta2PinchOS()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) - (0.03125D0*CA3*EL*MH22*TB*dBeta2PinchOS()&
  &*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) - (0.03125D0*CA22*CA3*EL*MH22*TB*dBeta2PinchOS()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.0625D&
  &0*CA3*EL*MH12*SA22*TB*dBeta2PinchOS()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.03125D0*CA3*EL*MH22*SA22*TB*dBeta2PinchOS()*DBLE(S&
  &A1**INT(3.D0)))/(CB*MW*SW) + (0.1875D0*EL*MH12*SA3*dAlpha2PinchOS()*DBLE(CA2**INT(3.D0))*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) + (0&
  &.09375D0*EL*MH22*SA3*dAlpha2PinchOS()*DBLE(CA2**INT(3.D0))*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0.28125D0*EL*m12squared*SA3*dAl&
  &pha2PinchOS()*DBLE(CA2**INT(3.D0))*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.28125D0*CA3*EL*m12squared*dBeta2PinchOS()*DBLE(CB&
  &**INT(-3.D0))*DBLE(SA1**INT(3.D0)))/(MW*SW) + (0.28125D0*CA22*CA3*EL*m12squared*dBeta2PinchOS()*DBLE(CB**INT(-3.D0))*DBLE(SA1*&
  &*INT(3.D0)))/(MW*SW) - (0.28125D0*CA3*EL*m12squared*SA22*dBeta2PinchOS()*DBLE(CB**INT(-3.D0))*DBLE(SA1**INT(3.D0)))/(MW*SW) + &
  &(0.09375D0*CA3*EL*m12squared*dBeta2PinchOS()*DBLE(CB**INT(-3.D0))*DBLE(SA1**INT(3.D0)))/(MW*SB2*SW) + (0.09375D0*CA22*CA3*EL*m&
  &12squared*dBeta2PinchOS()*DBLE(CB**INT(-3.D0))*DBLE(SA1**INT(3.D0)))/(MW*SB2*SW) - (0.09375D0*CA3*EL*m12squared*SA22*dBeta2Pin&
  &chOS()*DBLE(CB**INT(-3.D0))*DBLE(SA1**INT(3.D0)))/(MW*SB2*SW) - (0.28125D0*CA1*EL*m12squared*SA3*dAlpha1PinchOS()*DBLE(SA2**IN&
  &T(3.D0)))/(CB*MW*SW) + (0.1875D0*EL*MH12*SA1*SA3*dAlpha1PinchOS()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.5625D0*CA12*EL*MH12*SA&
  &1*SA3*dAlpha1PinchOS()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.09375D0*EL*MH22*SA1*SA3*dAlpha1PinchOS()*DBLE(SA2**INT(3.D0)))/(C&
  &B*MW*SW) + (0.28125D0*CA12*EL*MH22*SA1*SA3*dAlpha1PinchOS()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.1875D0*CA1*EL*MH12*SA3*dAlph&
  &a1PinchOS()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.09375D0*CA1*EL*MH22*SA3*dAlpha1PinchOS()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) + &
  &(0.28125D0*EL*m12squared*SA1*SA3*dAlpha1PinchOS()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.1875D0*EL*m12squared*SA1*SA3*dAlpha1Pi&
  &nchOS()*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) - (0.84375D0*CA12*EL*m12squared*SA1*SA3*dAlpha1PinchOS()*DBLE(SA2**INT(3.D0)))/(C&
  &B2*MW*SB*SW) - (0.5625D0*CA1*EL*MH12*SA12*SA3*dAlpha1PinchOS()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.28125D0*CA1*EL*MH22*SA12*&
  &SA3*dAlpha1PinchOS()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) + (0.1875D0*CA1*EL*m12squared*SA3*dAlpha1PinchOS()*DBLE(SA2**INT(3.D0)))&
  &/(CB*MW*SB2*SW) + (0.84375D0*CA1*EL*m12squared*SA12*SA3*dAlpha1PinchOS()*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) + (0.09375D0*CA1&
  &*EL*m12squared*SA3*dAlpha1PinchOS()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) - (0.09375D0*EL*m12squared*SA1*SA3*TB*dAlpha1PinchOS()&
  &*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (1.5D0*MH12*SA3*dAlpha2PinchOS()*DBLE(SA2**INT(3.D0)))/vS + (0.75D0*MH22*SA3*dAlpha2PinchO&
  &S()*DBLE(SA2**INT(3.D0)))/vS - (0.1875D0*CA1*CA3*EL*MH12*dAlpha3PinchOS()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.09375D0*CA1*CA&
  &3*EL*MH22*dAlpha3PinchOS()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.28125D0*CA3*EL*m12squared*SA1*dAlpha3PinchOS()*DBLE(SA2**INT(&
  &3.D0)))/(CB*MW*SW) + (0.1875D0*CA1*CA3*EL*MH12*SA12*dAlpha3PinchOS()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.09375D0*CA1*CA3*EL*&
  &MH22*SA12*dAlpha3PinchOS()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.28125D0*CA1*CA3*EL*m12squared*dAlpha3PinchOS()*DBLE(SA2**INT(&
  &3.D0)))/(MW*SB*SW) + (0.1875D0*CA1*CA3*EL*m12squared*dAlpha3PinchOS()*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) - (0.1875D0*CA3*EL*&
  &MH12*SA1*dAlpha3PinchOS()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) + (0.1875D0*CA12*CA3*EL*MH12*SA1*dAlpha3PinchOS()*DBLE(SA2**INT(3.D&
  &0)))/(MW*SB*SW) - (0.09375D0*CA3*EL*MH22*SA1*dAlpha3PinchOS()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) + (0.09375D0*CA12*CA3*EL*MH22*S&
  &A1*dAlpha3PinchOS()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.28125D0*CA1*CA3*EL*m12squared*SA12*dAlpha3PinchOS()*DBLE(SA2**INT(3.&
  &D0)))/(CB2*MW*SB*SW) + (0.1875D0*CA3*EL*m12squared*SA1*dAlpha3PinchOS()*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) - (0.28125D0*CA12&
  &*CA3*EL*m12squared*SA1*dAlpha3PinchOS()*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) + (0.09375D0*CA3*EL*m12squared*SA1*dAlpha3PinchOS&
  &()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) + (0.09375D0*CA1*CA3*EL*m12squared*TB*dAlpha3PinchOS()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW)&
  & - (0.09375D0*CA1*EL*m12squared*SA3*dBeta2PinchOS()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.09375D0*CA1*EL*MH12*SA3*dBeta2PinchO&
  &S()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.09375D0*CA1*EL*MH12*SA3*dBeta2PinchOS()*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) + (0.04&
  &6875D0*CA1*EL*MH22*SA3*dBeta2PinchOS()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.046875D0*CA1*EL*MH22*SA3*dBeta2PinchOS()*DBLE(SA2&
  &**INT(3.D0)))/(CB2*MW*SB*SW) + (0.140625D0*EL*m12squared*SA1*SA3*dBeta2PinchOS()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) + (0.046875D&
  &0*EL*m12squared*SA1*SA3*dBeta2PinchOS()*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) - (0.28125D0*CA12*EL*m12squared*SA1*SA3*dBeta2Pin&
  &chOS()*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) - (0.09375D0*CA1*EL*MH12*SA12*SA3*dBeta2PinchOS()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW)&
  & + (0.09375D0*CA1*EL*MH12*SA12*SA3*dBeta2PinchOS()*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) - (0.046875D0*CA1*EL*MH22*SA12*SA3*dBe&
  &ta2PinchOS()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) + (0.046875D0*CA1*EL*MH22*SA12*SA3*dBeta2PinchOS()*DBLE(SA2**INT(3.D0)))/(CB2*MW&
  &*SB*SW) - (0.10546875D0*CA1*EL*m12squared*SA3*dBeta2PinchOS()*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) + (0.38671875D0*CA1*EL*m12s&
  &quared*SA12*SA3*dBeta2PinchOS()*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) + (0.140625D0*CA1*EL*m12squared*SA3*dBeta2PinchOS()*DBLE(&
  &SA2**INT(3.D0)))/(MW*SB*SW*TB) + (0.1875D0*EL*MH12*SA1*SA3*dBeta2PinchOS()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) - (0.1875D0*CA1&
  &2*EL*MH12*SA1*SA3*dBeta2PinchOS()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) + (0.09375D0*EL*MH22*SA1*SA3*dBeta2PinchOS()*DBLE(SA2**I&
  &NT(3.D0)))/(MW*SB*SW*TB) - (0.09375D0*CA12*EL*MH22*SA1*SA3*dBeta2PinchOS()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) - (0.09375D0*CA&
  &1*EL*MH12*SA3*TB*dBeta2PinchOS()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.046875D0*CA1*EL*MH22*SA3*TB*dBeta2PinchOS()*DBLE(SA2**I&
  &NT(3.D0)))/(CB*MW*SW) - (0.140625D0*EL*m12squared*SA1*SA3*TB*dBeta2PinchOS()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.09375D0*CA1&
  &*EL*MH12*SA12*SA3*TB*dBeta2PinchOS()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.046875D0*CA1*EL*MH22*SA12*SA3*TB*dBeta2PinchOS()*DB&
  &LE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.09375D0*EL*m12squared*SA1*SA3*dBeta2PinchOS()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB2) + (0.14&
  &0625D0*CA1*EL*m12squared*SA3*TB2*dBeta2PinchOS()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.1875D0*EL*MH12*SA3*dAlpha1PinchOS()*DBL&
  &E(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) + (0.09375D0*EL*MH22*SA3*dAlpha1PinchOS()*DBLE(CA1**INT(3.D0))*DBLE(SA2**IN&
  &T(3.D0)))/(MW*SB*SW) - (0.28125D0*EL*m12squared*SA3*dAlpha1PinchOS()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW)&
  & - (0.0625D0*CA3*EL*MH12*dAlpha3PinchOS()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.03125D0*CA3*EL*MH22*dAlph&
  &a3PinchOS()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.09375D0*CA3*EL*m12squared*dAlpha3PinchOS()*DBLE(CA1**IN&
  &T(3.D0))*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) + (0.03125D0*EL*MH12*SA3*dBeta2PinchOS()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0&
  &)))/(MW*SB*SW) - (0.03125D0*EL*MH12*SA3*dBeta2PinchOS()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) + (0.015625D&
  &0*EL*MH22*SA3*dBeta2PinchOS()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.015625D0*EL*MH22*SA3*dBeta2PinchOS()*&
  &DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) - (0.12890625D0*EL*m12squared*SA3*dBeta2PinchOS()*DBLE(CA1**INT(3.D0&
  &))*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) - (0.03125D0*EL*MH12*SA3*TB*dBeta2PinchOS()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))&
  &/(CB*MW*SW) - (0.015625D0*EL*MH22*SA3*TB*dBeta2PinchOS()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.36328125D0&
  &*CA1*EL*m12squared*SA3*dBeta2PinchOS()*DBLE(CB**INT(-3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SW) - (0.45703125D0*CA1*EL*m12squared*SA&
  &12*SA3*dBeta2PinchOS()*DBLE(CB**INT(-3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SW) + (0.05859375D0*CA1*EL*m12squared*SA3*dBeta2PinchOS(&
  &)*DBLE(CB**INT(-3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SB2*SW) - (0.10546875D0*CA1*EL*m12squared*SA12*SA3*dBeta2PinchOS()*DBLE(CB**I&
  &NT(-3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SB2*SW) + (0.15234375D0*EL*m12squared*SA3*dBeta2PinchOS()*DBLE(CA1**INT(3.D0))*DBLE(CB**I&
  &NT(-3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SW) + (0.03515625D0*EL*m12squared*SA3*dBeta2PinchOS()*DBLE(CA1**INT(3.D0))*DBLE(CB**INT(-&
  &3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SB2*SW) - (0.1875D0*EL*MH12*SA3*dAlpha1PinchOS()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(&
  &CB*MW*SW) - (0.09375D0*EL*MH22*SA3*dAlpha1PinchOS()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.28125D0*EL*m12s&
  &quared*SA3*dAlpha1PinchOS()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) - (0.0625D0*CA3*EL*MH12*dAlpha3PinchOS()&
  &*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.03125D0*CA3*EL*MH22*dAlpha3PinchOS()*DBLE(SA1**INT(3.D0))*DBLE(SA2&
  &**INT(3.D0)))/(MW*SB*SW) + (0.09375D0*CA3*EL*m12squared*dAlpha3PinchOS()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2&
  &*SW) + (0.09375D0*EL*m12squared*SA3*dBeta2PinchOS()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) + (0.0625D0*EL*M&
  &H12*SA3*dBeta2PinchOS()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) + (0.03125D0*EL*MH22*SA3*dBeta2PinchOS()*DBLE&
  &(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) - (0.1875D0*CA1*CA3*EL*m12squared*dBeta2PinchOS()*DBLE(SB**INT(-3.D0)))/(&
  &MW*SW) - (0.1875D0*CA1*CA22*CA3*EL*m12squared*dBeta2PinchOS()*DBLE(SB**INT(-3.D0)))/(MW*SW) - (1.125D0*CA1*CA3*EL*m12squared*S&
  &A12*dBeta2PinchOS()*DBLE(SB**INT(-3.D0)))/(MW*SW) - (1.125D0*CA1*CA22*CA3*EL*m12squared*SA12*dBeta2PinchOS()*DBLE(SB**INT(-3.D&
  &0)))/(MW*SW) + (0.1875D0*CA1*CA3*EL*m12squared*SA22*dBeta2PinchOS()*DBLE(SB**INT(-3.D0)))/(MW*SW) + (1.125D0*CA1*CA3*EL*m12squ&
  &ared*SA12*SA22*dBeta2PinchOS()*DBLE(SB**INT(-3.D0)))/(MW*SW) + (0.46875D0*EL*m12squared*SA1*SA2*SA3*dBeta2PinchOS()*DBLE(SB**I&
  &NT(-3.D0)))/(MW*SW) - (0.5625D0*CA12*EL*m12squared*SA1*SA2*SA3*dBeta2PinchOS()*DBLE(SB**INT(-3.D0)))/(MW*SW) + (1.40625D0*CA22&
  &*EL*m12squared*SA1*SA2*SA3*dBeta2PinchOS()*DBLE(SB**INT(-3.D0)))/(MW*SW) - (1.6875D0*CA12*CA22*EL*m12squared*SA1*SA2*SA3*dBeta&
  &2PinchOS()*DBLE(SB**INT(-3.D0)))/(MW*SW) + (0.375D0*CA3*EL*m12squared*dBeta2PinchOS()*DBLE(CA1**INT(3.D0))*DBLE(SB**INT(-3.D0)&
  &))/(MW*SW) + (0.375D0*CA22*CA3*EL*m12squared*dBeta2PinchOS()*DBLE(CA1**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(MW*SW) - (0.375D0*CA3&
  &*EL*m12squared*SA22*dBeta2PinchOS()*DBLE(CA1**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(MW*SW) + (0.1875D0*EL*m12squared*SA2*SA3*dBeta&
  &2PinchOS()*DBLE(SA1**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(MW*SW) + (0.5625D0*CA22*EL*m12squared*SA2*SA3*dBeta2PinchOS()*DBLE(SA1*&
  &*INT(3.D0))*DBLE(SB**INT(-3.D0)))/(MW*SW) - (0.46875D0*EL*m12squared*SA1*SA3*dBeta2PinchOS()*DBLE(SA2**INT(3.D0))*DBLE(SB**INT&
  &(-3.D0)))/(MW*SW) + (0.5625D0*CA12*EL*m12squared*SA1*SA3*dBeta2PinchOS()*DBLE(SA2**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(MW*SW) - &
  &(0.1875D0*EL*m12squared*SA3*dBeta2PinchOS()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(MW*SW) - (0.125D0&
  &*CA1*CA3*m12squared*dgAtMZ())/(CB*MW) - (0.125D0*CA1*CA22*CA3*m12squared*dgAtMZ())/(CB*MW) + (0.125D0*CA3*MH12*SA1*dgAtMZ())/(&
  &CB*MW) + (0.375D0*CA12*CA3*MH12*SA1*dgAtMZ())/(CB*MW) + (0.125D0*CA22*CA3*MH12*SA1*dgAtMZ())/(CB*MW) + (0.375D0*CA12*CA22*CA3*&
  &MH12*SA1*dgAtMZ())/(CB*MW) + (0.0625D0*CA3*MH22*SA1*dgAtMZ())/(CB*MW) + (0.1875D0*CA12*CA3*MH22*SA1*dgAtMZ())/(CB*MW) + (0.062&
  &5D0*CA22*CA3*MH22*SA1*dgAtMZ())/(CB*MW) + (0.1875D0*CA12*CA22*CA3*MH22*SA1*dgAtMZ())/(CB*MW) + (0.125D0*CA1*CA3*m12squared*SA2&
  &2*dgAtMZ())/(CB*MW) - (0.125D0*CA3*MH12*SA1*SA22*dgAtMZ())/(CB*MW) - (0.375D0*CA12*CA3*MH12*SA1*SA22*dgAtMZ())/(CB*MW) - (0.06&
  &25D0*CA3*MH22*SA1*SA22*dgAtMZ())/(CB*MW) - (0.1875D0*CA12*CA3*MH22*SA1*SA22*dgAtMZ())/(CB*MW) + (0.1875D0*CA1*MH12*SA2*SA3*dgA&
  &tMZ())/(CB*MW) + (0.5625D0*CA1*CA22*MH12*SA2*SA3*dgAtMZ())/(CB*MW) + (0.09375D0*CA1*MH22*SA2*SA3*dgAtMZ())/(CB*MW) + (0.28125D&
  &0*CA1*CA22*MH22*SA2*SA3*dgAtMZ())/(CB*MW) + (0.28125D0*m12squared*SA1*SA2*SA3*dgAtMZ())/(CB*MW) + (0.84375D0*CA22*m12squared*S&
  &A1*SA2*SA3*dgAtMZ())/(CB*MW) - (0.1875D0*CA1*MH12*SA12*SA2*SA3*dgAtMZ())/(CB*MW) - (0.5625D0*CA1*CA22*MH12*SA12*SA2*SA3*dgAtMZ&
  &())/(CB*MW) - (0.09375D0*CA1*MH22*SA12*SA2*SA3*dgAtMZ())/(CB*MW) - (0.28125D0*CA1*CA22*MH22*SA12*SA2*SA3*dgAtMZ())/(CB*MW) - (&
  &0.125D0*CA1*CA3*MH12*dgAtMZ())/(MW*SB) - (0.125D0*CA1*CA22*CA3*MH12*dgAtMZ())/(MW*SB) - (0.0625D0*CA1*CA3*MH22*dgAtMZ())/(MW*S&
  &B) - (0.0625D0*CA1*CA22*CA3*MH22*dgAtMZ())/(MW*SB) + (0.21875D0*CA3*m12squared*SA1*dgAtMZ())/(MW*SB) + (0.21875D0*CA22*CA3*m12&
  &squared*SA1*dgAtMZ())/(MW*SB) - (0.15625D0*CA3*m12squared*SA1*dgAtMZ())/(CB2*MW*SB) - (0.5625D0*CA12*CA3*m12squared*SA1*dgAtMZ&
  &())/(CB2*MW*SB) - (0.15625D0*CA22*CA3*m12squared*SA1*dgAtMZ())/(CB2*MW*SB) - (0.5625D0*CA12*CA22*CA3*m12squared*SA1*dgAtMZ())/&
  &(CB2*MW*SB) - (0.375D0*CA1*CA3*MH12*SA12*dgAtMZ())/(MW*SB) - (0.375D0*CA1*CA22*CA3*MH12*SA12*dgAtMZ())/(MW*SB) - (0.1875D0*CA1&
  &*CA3*MH22*SA12*dgAtMZ())/(MW*SB) - (0.1875D0*CA1*CA22*CA3*MH22*SA12*dgAtMZ())/(MW*SB) + (0.125D0*CA1*CA3*MH12*SA22*dgAtMZ())/(&
  &MW*SB) + (0.0625D0*CA1*CA3*MH22*SA22*dgAtMZ())/(MW*SB) - (0.21875D0*CA3*m12squared*SA1*SA22*dgAtMZ())/(MW*SB) + (0.15625D0*CA3&
  &*m12squared*SA1*SA22*dgAtMZ())/(CB2*MW*SB) + (0.5625D0*CA12*CA3*m12squared*SA1*SA22*dgAtMZ())/(CB2*MW*SB) + (0.375D0*CA1*CA3*M&
  &H12*SA12*SA22*dgAtMZ())/(MW*SB) + (0.1875D0*CA1*CA3*MH22*SA12*SA22*dgAtMZ())/(MW*SB) + (0.28125D0*CA1*m12squared*SA2*SA3*dgAtM&
  &Z())/(MW*SB) + (0.84375D0*CA1*CA22*m12squared*SA2*SA3*dgAtMZ())/(MW*SB) - (0.1875D0*CA1*m12squared*SA2*SA3*dgAtMZ())/(CB2*MW*S&
  &B) - (0.5625D0*CA1*CA22*m12squared*SA2*SA3*dgAtMZ())/(CB2*MW*SB) + (0.1875D0*MH12*SA1*SA2*SA3*dgAtMZ())/(MW*SB) - (0.1875D0*CA&
  &12*MH12*SA1*SA2*SA3*dgAtMZ())/(MW*SB) + (0.5625D0*CA22*MH12*SA1*SA2*SA3*dgAtMZ())/(MW*SB) - (0.5625D0*CA12*CA22*MH12*SA1*SA2*S&
  &A3*dgAtMZ())/(MW*SB) + (0.09375D0*MH22*SA1*SA2*SA3*dgAtMZ())/(MW*SB) - (0.09375D0*CA12*MH22*SA1*SA2*SA3*dgAtMZ())/(MW*SB) + (0&
  &.28125D0*CA22*MH22*SA1*SA2*SA3*dgAtMZ())/(MW*SB) - (0.28125D0*CA12*CA22*MH22*SA1*SA2*SA3*dgAtMZ())/(MW*SB) + (0.28125D0*CA1*m1&
  &2squared*SA12*SA2*SA3*dgAtMZ())/(CB2*MW*SB) + (0.84375D0*CA1*CA22*m12squared*SA12*SA2*SA3*dgAtMZ())/(CB2*MW*SB) + (0.0625D0*CA&
  &1*CA3*m12squared*dgAtMZ())/(CB*MW*SB2) + (0.0625D0*CA1*CA22*CA3*m12squared*dgAtMZ())/(CB*MW*SB2) + (0.5625D0*CA1*CA3*m12square&
  &d*SA12*dgAtMZ())/(CB*MW*SB2) + (0.5625D0*CA1*CA22*CA3*m12squared*SA12*dgAtMZ())/(CB*MW*SB2) - (0.0625D0*CA1*CA3*m12squared*SA2&
  &2*dgAtMZ())/(CB*MW*SB2) - (0.5625D0*CA1*CA3*m12squared*SA12*SA22*dgAtMZ())/(CB*MW*SB2) - (0.1875D0*m12squared*SA1*SA2*SA3*dgAt&
  &MZ())/(CB*MW*SB2) + (0.28125D0*CA12*m12squared*SA1*SA2*SA3*dgAtMZ())/(CB*MW*SB2) - (0.5625D0*CA22*m12squared*SA1*SA2*SA3*dgAtM&
  &Z())/(CB*MW*SB2) + (0.84375D0*CA12*CA22*m12squared*SA1*SA2*SA3*dgAtMZ())/(CB*MW*SB2) + (0.125D0*CA1*CA3*m12squared*dgAtMZ())/(&
  &MW*SB*TB) + (0.125D0*CA1*CA22*CA3*m12squared*dgAtMZ())/(MW*SB*TB) - (0.125D0*CA1*CA3*m12squared*SA22*dgAtMZ())/(MW*SB*TB) - (0&
  &.09375D0*m12squared*SA1*SA2*SA3*dgAtMZ())/(MW*SB*TB) - (0.28125D0*CA22*m12squared*SA1*SA2*SA3*dgAtMZ())/(MW*SB*TB) - (0.03125D&
  &0*CA3*m12squared*SA1*TB*dgAtMZ())/(CB*MW) - (0.03125D0*CA22*CA3*m12squared*SA1*TB*dgAtMZ())/(CB*MW) + (0.03125D0*CA3*m12square&
  &d*SA1*SA22*TB*dgAtMZ())/(CB*MW) - (0.09375D0*CA1*m12squared*SA2*SA3*TB*dgAtMZ())/(CB*MW) - (0.28125D0*CA1*CA22*m12squared*SA2*&
  &SA3*TB*dgAtMZ())/(CB*MW) + (0.0625D0*MH12*SA2*SA3*DBLE(CA1**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.1875D0*CA22*MH12*SA2*SA3*DBLE(CA&
  &1**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.03125D0*MH22*SA2*SA3*DBLE(CA1**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.09375D0*CA22*MH22*SA2*SA&
  &3*DBLE(CA1**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.125D0*CA3*MH12*DBLE(CA1**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.125D0*CA22*CA3*MH12*D&
  &BLE(CA1**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.0625D0*CA3*MH22*DBLE(CA1**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.0625D0*CA22*CA3*MH22*DB&
  &LE(CA1**INT(3.D0))*dgAtMZ())/(MW*SB) - (0.125D0*CA3*MH12*SA22*DBLE(CA1**INT(3.D0))*dgAtMZ())/(MW*SB) - (0.0625D0*CA3*MH22*SA22&
  &*DBLE(CA1**INT(3.D0))*dgAtMZ())/(MW*SB) - (0.09375D0*m12squared*SA2*SA3*DBLE(CA1**INT(3.D0))*dgAtMZ())/(CB2*MW*SB) - (0.28125D&
  &0*CA22*m12squared*SA2*SA3*DBLE(CA1**INT(3.D0))*dgAtMZ())/(CB2*MW*SB) - (0.1875D0*CA3*m12squared*DBLE(CA1**INT(3.D0))*dgAtMZ())&
  &/(CB*MW*SB2) - (0.1875D0*CA22*CA3*m12squared*DBLE(CA1**INT(3.D0))*dgAtMZ())/(CB*MW*SB2) + (0.1875D0*CA3*m12squared*SA22*DBLE(C&
  &A1**INT(3.D0))*dgAtMZ())/(CB*MW*SB2) - (0.125D0*CA3*MH12*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB*MW) - (0.125D0*CA22*CA3*MH12*DBLE(&
  &SA1**INT(3.D0))*dgAtMZ())/(CB*MW) - (0.0625D0*CA3*MH22*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB*MW) - (0.0625D0*CA22*CA3*MH22*DBLE(S&
  &A1**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.125D0*CA3*MH12*SA22*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.0625D0*CA3*MH22*SA22*DBL&
  &E(SA1**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.1875D0*CA3*m12squared*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB2*MW*SB) + (0.1875D0*CA22*CA3&
  &*m12squared*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB2*MW*SB) - (0.1875D0*CA3*m12squared*SA22*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB2*MW*&
  &SB) + (0.0625D0*MH12*SA2*SA3*DBLE(SA1**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.1875D0*CA22*MH12*SA2*SA3*DBLE(SA1**INT(3.D0))*dgAtMZ(&
  &))/(MW*SB) + (0.03125D0*MH22*SA2*SA3*DBLE(SA1**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.09375D0*CA22*MH22*SA2*SA3*DBLE(SA1**INT(3.D0)&
  &)*dgAtMZ())/(MW*SB) - (0.09375D0*m12squared*SA2*SA3*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB*MW*SB2) - (0.28125D0*CA22*m12squared*SA&
  &2*SA3*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB*MW*SB2) - (0.1875D0*CA1*MH12*SA3*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW) - (0.09375D0*&
  &CA1*MH22*SA3*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW) - (0.28125D0*m12squared*SA1*SA3*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW) + (&
  &0.1875D0*CA1*MH12*SA12*SA3*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.09375D0*CA1*MH22*SA12*SA3*DBLE(SA2**INT(3.D0))*dgAtMZ()&
  &)/(CB*MW) - (0.28125D0*CA1*m12squared*SA3*DBLE(SA2**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.1875D0*CA1*m12squared*SA3*DBLE(SA2**INT(&
  &3.D0))*dgAtMZ())/(CB2*MW*SB) - (0.1875D0*MH12*SA1*SA3*DBLE(SA2**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.1875D0*CA12*MH12*SA1*SA3*DBL&
  &E(SA2**INT(3.D0))*dgAtMZ())/(MW*SB) - (0.09375D0*MH22*SA1*SA3*DBLE(SA2**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.09375D0*CA12*MH22*SA&
  &1*SA3*DBLE(SA2**INT(3.D0))*dgAtMZ())/(MW*SB) - (0.28125D0*CA1*m12squared*SA12*SA3*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB2*MW*SB) +&
  & (0.1875D0*m12squared*SA1*SA3*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW*SB2) - (0.28125D0*CA12*m12squared*SA1*SA3*DBLE(SA2**INT(3.&
  &D0))*dgAtMZ())/(CB*MW*SB2) + (0.09375D0*m12squared*SA1*SA3*DBLE(SA2**INT(3.D0))*dgAtMZ())/(MW*SB*TB) + (0.09375D0*CA1*m12squar&
  &ed*SA3*TB*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW) - (0.0625D0*MH12*SA3*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*&
  &MW) - (0.03125D0*MH22*SA3*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.09375D0*m12squared*SA3*DBLE(CA1**IN&
  &T(3.D0))*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB2*MW*SB) - (0.0625D0*MH12*SA3*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dgAtMZ())/(&
  &MW*SB) - (0.03125D0*MH22*SA3*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.09375D0*m12squared*SA3*DBLE(SA1*&
  &*INT(3.D0))*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW*SB2) - (0.125D0*CA1*CA3*EL*dm122MSBarAlter())/(CB*MW*SW) - (0.125D0*CA1*CA22&
  &*CA3*EL*dm122MSBarAlter())/(CB*MW*SW) + (0.125D0*CA1*CA3*EL*SA22*dm122MSBarAlter())/(CB*MW*SW) + (0.28125D0*EL*SA1*SA2*SA3*dm1&
  &22MSBarAlter())/(CB*MW*SW) + (0.84375D0*CA22*EL*SA1*SA2*SA3*dm122MSBarAlter())/(CB*MW*SW) + (0.21875D0*CA3*EL*SA1*dm122MSBarAl&
  &ter())/(MW*SB*SW) + (0.21875D0*CA22*CA3*EL*SA1*dm122MSBarAlter())/(MW*SB*SW) - (0.15625D0*CA3*EL*SA1*dm122MSBarAlter())/(CB2*M&
  &W*SB*SW) - (0.5625D0*CA12*CA3*EL*SA1*dm122MSBarAlter())/(CB2*MW*SB*SW) - (0.15625D0*CA22*CA3*EL*SA1*dm122MSBarAlter())/(CB2*MW&
  &*SB*SW) - (0.5625D0*CA12*CA22*CA3*EL*SA1*dm122MSBarAlter())/(CB2*MW*SB*SW) - (0.21875D0*CA3*EL*SA1*SA22*dm122MSBarAlter())/(MW&
  &*SB*SW) + (0.15625D0*CA3*EL*SA1*SA22*dm122MSBarAlter())/(CB2*MW*SB*SW) + (0.5625D0*CA12*CA3*EL*SA1*SA22*dm122MSBarAlter())/(CB&
  &2*MW*SB*SW) + (0.28125D0*CA1*EL*SA2*SA3*dm122MSBarAlter())/(MW*SB*SW) + (0.84375D0*CA1*CA22*EL*SA2*SA3*dm122MSBarAlter())/(MW*&
  &SB*SW) - (0.1875D0*CA1*EL*SA2*SA3*dm122MSBarAlter())/(CB2*MW*SB*SW) - (0.5625D0*CA1*CA22*EL*SA2*SA3*dm122MSBarAlter())/(CB2*MW&
  &*SB*SW) + (0.28125D0*CA1*EL*SA12*SA2*SA3*dm122MSBarAlter())/(CB2*MW*SB*SW) + (0.84375D0*CA1*CA22*EL*SA12*SA2*SA3*dm122MSBarAlt&
  &er())/(CB2*MW*SB*SW) + (0.0625D0*CA1*CA3*EL*dm122MSBarAlter())/(CB*MW*SB2*SW) + (0.0625D0*CA1*CA22*CA3*EL*dm122MSBarAlter())/(&
  &CB*MW*SB2*SW) + (0.5625D0*CA1*CA3*EL*SA12*dm122MSBarAlter())/(CB*MW*SB2*SW) + (0.5625D0*CA1*CA22*CA3*EL*SA12*dm122MSBarAlter()&
  &)/(CB*MW*SB2*SW) - (0.0625D0*CA1*CA3*EL*SA22*dm122MSBarAlter())/(CB*MW*SB2*SW) - (0.5625D0*CA1*CA3*EL*SA12*SA22*dm122MSBarAlte&
  &r())/(CB*MW*SB2*SW) - (0.1875D0*EL*SA1*SA2*SA3*dm122MSBarAlter())/(CB*MW*SB2*SW) + (0.28125D0*CA12*EL*SA1*SA2*SA3*dm122MSBarAl&
  &ter())/(CB*MW*SB2*SW) - (0.5625D0*CA22*EL*SA1*SA2*SA3*dm122MSBarAlter())/(CB*MW*SB2*SW) + (0.84375D0*CA12*CA22*EL*SA1*SA2*SA3*&
  &dm122MSBarAlter())/(CB*MW*SB2*SW) + (0.125D0*CA1*CA3*EL*dm122MSBarAlter())/(MW*SB*SW*TB) + (0.125D0*CA1*CA22*CA3*EL*dm122MSBar&
  &Alter())/(MW*SB*SW*TB) - (0.125D0*CA1*CA3*EL*SA22*dm122MSBarAlter())/(MW*SB*SW*TB) - (0.09375D0*EL*SA1*SA2*SA3*dm122MSBarAlter&
  &())/(MW*SB*SW*TB) - (0.28125D0*CA22*EL*SA1*SA2*SA3*dm122MSBarAlter())/(MW*SB*SW*TB) - (0.03125D0*CA3*EL*SA1*TB*dm122MSBarAlter&
  &())/(CB*MW*SW) - (0.03125D0*CA22*CA3*EL*SA1*TB*dm122MSBarAlter())/(CB*MW*SW) + (0.03125D0*CA3*EL*SA1*SA22*TB*dm122MSBarAlter()&
  &)/(CB*MW*SW) - (0.09375D0*CA1*EL*SA2*SA3*TB*dm122MSBarAlter())/(CB*MW*SW) - (0.28125D0*CA1*CA22*EL*SA2*SA3*TB*dm122MSBarAlter(&
  &))/(CB*MW*SW) - (0.09375D0*EL*SA2*SA3*DBLE(CA1**INT(3.D0))*dm122MSBarAlter())/(CB2*MW*SB*SW) - (0.28125D0*CA22*EL*SA2*SA3*DBLE&
  &(CA1**INT(3.D0))*dm122MSBarAlter())/(CB2*MW*SB*SW) - (0.1875D0*CA3*EL*DBLE(CA1**INT(3.D0))*dm122MSBarAlter())/(CB*MW*SB2*SW) -&
  & (0.1875D0*CA22*CA3*EL*DBLE(CA1**INT(3.D0))*dm122MSBarAlter())/(CB*MW*SB2*SW) + (0.1875D0*CA3*EL*SA22*DBLE(CA1**INT(3.D0))*dm1&
  &22MSBarAlter())/(CB*MW*SB2*SW) + (0.1875D0*CA3*EL*DBLE(SA1**INT(3.D0))*dm122MSBarAlter())/(CB2*MW*SB*SW) + (0.1875D0*CA22*CA3*&
  &EL*DBLE(SA1**INT(3.D0))*dm122MSBarAlter())/(CB2*MW*SB*SW) - (0.1875D0*CA3*EL*SA22*DBLE(SA1**INT(3.D0))*dm122MSBarAlter())/(CB2&
  &*MW*SB*SW) - (0.09375D0*EL*SA2*SA3*DBLE(SA1**INT(3.D0))*dm122MSBarAlter())/(CB*MW*SB2*SW) - (0.28125D0*CA22*EL*SA2*SA3*DBLE(SA&
  &1**INT(3.D0))*dm122MSBarAlter())/(CB*MW*SB2*SW) - (0.28125D0*EL*SA1*SA3*DBLE(SA2**INT(3.D0))*dm122MSBarAlter())/(CB*MW*SW) - (&
  &0.28125D0*CA1*EL*SA3*DBLE(SA2**INT(3.D0))*dm122MSBarAlter())/(MW*SB*SW) + (0.1875D0*CA1*EL*SA3*DBLE(SA2**INT(3.D0))*dm122MSBar&
  &Alter())/(CB2*MW*SB*SW) - (0.28125D0*CA1*EL*SA12*SA3*DBLE(SA2**INT(3.D0))*dm122MSBarAlter())/(CB2*MW*SB*SW) + (0.1875D0*EL*SA1&
  &*SA3*DBLE(SA2**INT(3.D0))*dm122MSBarAlter())/(CB*MW*SB2*SW) - (0.28125D0*CA12*EL*SA1*SA3*DBLE(SA2**INT(3.D0))*dm122MSBarAlter(&
  &))/(CB*MW*SB2*SW) + (0.09375D0*EL*SA1*SA3*DBLE(SA2**INT(3.D0))*dm122MSBarAlter())/(MW*SB*SW*TB) + (0.09375D0*CA1*EL*SA3*TB*DBL&
  &E(SA2**INT(3.D0))*dm122MSBarAlter())/(CB*MW*SW) + (0.09375D0*EL*SA3*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dm122MSBarAlter(&
  &))/(CB2*MW*SB*SW) + (0.09375D0*EL*SA3*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dm122MSBarAlter())/(CB*MW*SB2*SW) + (0.125D0*C&
  &A3*EL*SA1*dMH12OSAlter())/(CB*MW*SW) + (0.375D0*CA12*CA3*EL*SA1*dMH12OSAlter())/(CB*MW*SW) + (0.125D0*CA22*CA3*EL*SA1*dMH12OSA&
  &lter())/(CB*MW*SW) + (0.375D0*CA12*CA22*CA3*EL*SA1*dMH12OSAlter())/(CB*MW*SW) - (0.125D0*CA3*EL*SA1*SA22*dMH12OSAlter())/(CB*M&
  &W*SW) - (0.375D0*CA12*CA3*EL*SA1*SA22*dMH12OSAlter())/(CB*MW*SW) + (0.1875D0*CA1*EL*SA2*SA3*dMH12OSAlter())/(CB*MW*SW) + (0.56&
  &25D0*CA1*CA22*EL*SA2*SA3*dMH12OSAlter())/(CB*MW*SW) - (0.1875D0*CA1*EL*SA12*SA2*SA3*dMH12OSAlter())/(CB*MW*SW) - (0.5625D0*CA1&
  &*CA22*EL*SA12*SA2*SA3*dMH12OSAlter())/(CB*MW*SW) - (0.125D0*CA1*CA3*EL*dMH12OSAlter())/(MW*SB*SW) - (0.125D0*CA1*CA22*CA3*EL*d&
  &MH12OSAlter())/(MW*SB*SW) - (0.375D0*CA1*CA3*EL*SA12*dMH12OSAlter())/(MW*SB*SW) - (0.375D0*CA1*CA22*CA3*EL*SA12*dMH12OSAlter()&
  &)/(MW*SB*SW) + (0.125D0*CA1*CA3*EL*SA22*dMH12OSAlter())/(MW*SB*SW) + (0.375D0*CA1*CA3*EL*SA12*SA22*dMH12OSAlter())/(MW*SB*SW) &
  &+ (0.1875D0*EL*SA1*SA2*SA3*dMH12OSAlter())/(MW*SB*SW) - (0.1875D0*CA12*EL*SA1*SA2*SA3*dMH12OSAlter())/(MW*SB*SW) + (0.5625D0*C&
  &A22*EL*SA1*SA2*SA3*dMH12OSAlter())/(MW*SB*SW) - (0.5625D0*CA12*CA22*EL*SA1*SA2*SA3*dMH12OSAlter())/(MW*SB*SW) - (0.5D0*CA2*SA3&
  &*dMH12OSAlter())/vS - (1.5D0*CA2*SA22*SA3*dMH12OSAlter())/vS + (0.0625D0*EL*SA2*SA3*DBLE(CA1**INT(3.D0))*dMH12OSAlter())/(CB*M&
  &W*SW) + (0.1875D0*CA22*EL*SA2*SA3*DBLE(CA1**INT(3.D0))*dMH12OSAlter())/(CB*MW*SW) + (0.125D0*CA3*EL*DBLE(CA1**INT(3.D0))*dMH12&
  &OSAlter())/(MW*SB*SW) + (0.125D0*CA22*CA3*EL*DBLE(CA1**INT(3.D0))*dMH12OSAlter())/(MW*SB*SW) - (0.125D0*CA3*EL*SA22*DBLE(CA1**&
  &INT(3.D0))*dMH12OSAlter())/(MW*SB*SW) + (0.5D0*SA3*DBLE(CA2**INT(3.D0))*dMH12OSAlter())/vS - (0.125D0*CA3*EL*DBLE(SA1**INT(3.D&
  &0))*dMH12OSAlter())/(CB*MW*SW) - (0.125D0*CA22*CA3*EL*DBLE(SA1**INT(3.D0))*dMH12OSAlter())/(CB*MW*SW) + (0.125D0*CA3*EL*SA22*D&
  &BLE(SA1**INT(3.D0))*dMH12OSAlter())/(CB*MW*SW) + (0.0625D0*EL*SA2*SA3*DBLE(SA1**INT(3.D0))*dMH12OSAlter())/(MW*SB*SW) + (0.187&
  &5D0*CA22*EL*SA2*SA3*DBLE(SA1**INT(3.D0))*dMH12OSAlter())/(MW*SB*SW) - (0.1875D0*CA1*EL*SA3*DBLE(SA2**INT(3.D0))*dMH12OSAlter()&
  &)/(CB*MW*SW) + (0.1875D0*CA1*EL*SA12*SA3*DBLE(SA2**INT(3.D0))*dMH12OSAlter())/(CB*MW*SW) - (0.1875D0*EL*SA1*SA3*DBLE(SA2**INT(&
  &3.D0))*dMH12OSAlter())/(MW*SB*SW) + (0.1875D0*CA12*EL*SA1*SA3*DBLE(SA2**INT(3.D0))*dMH12OSAlter())/(MW*SB*SW) - (0.0625D0*EL*S&
  &A3*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dMH12OSAlter())/(CB*MW*SW) - (0.0625D0*EL*SA3*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(&
  &3.D0))*dMH12OSAlter())/(MW*SB*SW) + (0.0625D0*CA3*EL*SA1*dMH22OSAlter())/(CB*MW*SW) + (0.1875D0*CA12*CA3*EL*SA1*dMH22OSAlter()&
  &)/(CB*MW*SW) + (0.0625D0*CA22*CA3*EL*SA1*dMH22OSAlter())/(CB*MW*SW) + (0.1875D0*CA12*CA22*CA3*EL*SA1*dMH22OSAlter())/(CB*MW*SW&
  &) - (0.0625D0*CA3*EL*SA1*SA22*dMH22OSAlter())/(CB*MW*SW) - (0.1875D0*CA12*CA3*EL*SA1*SA22*dMH22OSAlter())/(CB*MW*SW) + (0.0937&
  &5D0*CA1*EL*SA2*SA3*dMH22OSAlter())/(CB*MW*SW) + (0.28125D0*CA1*CA22*EL*SA2*SA3*dMH22OSAlter())/(CB*MW*SW) - (0.09375D0*CA1*EL*&
  &SA12*SA2*SA3*dMH22OSAlter())/(CB*MW*SW) - (0.28125D0*CA1*CA22*EL*SA12*SA2*SA3*dMH22OSAlter())/(CB*MW*SW) - (0.0625D0*CA1*CA3*E&
  &L*dMH22OSAlter())/(MW*SB*SW) - (0.0625D0*CA1*CA22*CA3*EL*dMH22OSAlter())/(MW*SB*SW) - (0.1875D0*CA1*CA3*EL*SA12*dMH22OSAlter()&
  &)/(MW*SB*SW) - (0.1875D0*CA1*CA22*CA3*EL*SA12*dMH22OSAlter())/(MW*SB*SW) + (0.0625D0*CA1*CA3*EL*SA22*dMH22OSAlter())/(MW*SB*SW&
  &) + (0.1875D0*CA1*CA3*EL*SA12*SA22*dMH22OSAlter())/(MW*SB*SW) + (0.09375D0*EL*SA1*SA2*SA3*dMH22OSAlter())/(MW*SB*SW) - (0.0937&
  &5D0*CA12*EL*SA1*SA2*SA3*dMH22OSAlter())/(MW*SB*SW) + (0.28125D0*CA22*EL*SA1*SA2*SA3*dMH22OSAlter())/(MW*SB*SW) - (0.28125D0*CA&
  &12*CA22*EL*SA1*SA2*SA3*dMH22OSAlter())/(MW*SB*SW) - (0.25D0*CA2*SA3*dMH22OSAlter())/vS - (0.75D0*CA2*SA22*SA3*dMH22OSAlter())/&
  &vS + (0.03125D0*EL*SA2*SA3*DBLE(CA1**INT(3.D0))*dMH22OSAlter())/(CB*MW*SW) + (0.09375D0*CA22*EL*SA2*SA3*DBLE(CA1**INT(3.D0))*d&
  &MH22OSAlter())/(CB*MW*SW) + (0.0625D0*CA3*EL*DBLE(CA1**INT(3.D0))*dMH22OSAlter())/(MW*SB*SW) + (0.0625D0*CA22*CA3*EL*DBLE(CA1*&
  &*INT(3.D0))*dMH22OSAlter())/(MW*SB*SW) - (0.0625D0*CA3*EL*SA22*DBLE(CA1**INT(3.D0))*dMH22OSAlter())/(MW*SB*SW) + (0.25D0*SA3*D&
  &BLE(CA2**INT(3.D0))*dMH22OSAlter())/vS - (0.0625D0*CA3*EL*DBLE(SA1**INT(3.D0))*dMH22OSAlter())/(CB*MW*SW) - (0.0625D0*CA22*CA3&
  &*EL*DBLE(SA1**INT(3.D0))*dMH22OSAlter())/(CB*MW*SW) + (0.0625D0*CA3*EL*SA22*DBLE(SA1**INT(3.D0))*dMH22OSAlter())/(CB*MW*SW) + &
  &(0.03125D0*EL*SA2*SA3*DBLE(SA1**INT(3.D0))*dMH22OSAlter())/(MW*SB*SW) + (0.09375D0*CA22*EL*SA2*SA3*DBLE(SA1**INT(3.D0))*dMH22O&
  &SAlter())/(MW*SB*SW) - (0.09375D0*CA1*EL*SA3*DBLE(SA2**INT(3.D0))*dMH22OSAlter())/(CB*MW*SW) + (0.09375D0*CA1*EL*SA12*SA3*DBLE&
  &(SA2**INT(3.D0))*dMH22OSAlter())/(CB*MW*SW) - (0.09375D0*EL*SA1*SA3*DBLE(SA2**INT(3.D0))*dMH22OSAlter())/(MW*SB*SW) + (0.09375&
  &D0*CA12*EL*SA1*SA3*DBLE(SA2**INT(3.D0))*dMH22OSAlter())/(MW*SB*SW) - (0.03125D0*EL*SA3*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0&
  &))*dMH22OSAlter())/(CB*MW*SW) - (0.03125D0*EL*SA3*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dMH22OSAlter())/(MW*SB*SW) + (0.06&
  &25D0*CA1*CA3*EL*m12squared*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.0625D0*CA1*CA22*CA3*EL*m12squared*DBLE(MW**INT(-3.D0&
  &))*dMW2Alter())/(CB*SW) - (0.0625D0*CA3*EL*MH12*SA1*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.1875D0*CA12*CA3*EL*MH12*SA1&
  &*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.0625D0*CA22*CA3*EL*MH12*SA1*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.187&
  &5D0*CA12*CA22*CA3*EL*MH12*SA1*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.03125D0*CA3*EL*MH22*SA1*DBLE(MW**INT(-3.D0))*dMW2&
  &Alter())/(CB*SW) - (0.09375D0*CA12*CA3*EL*MH22*SA1*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.03125D0*CA22*CA3*EL*MH22*SA1&
  &*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.09375D0*CA12*CA22*CA3*EL*MH22*SA1*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - &
  &(0.0625D0*CA1*CA3*EL*m12squared*SA22*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.0625D0*CA3*EL*MH12*SA1*SA22*DBLE(MW**INT(-&
  &3.D0))*dMW2Alter())/(CB*SW) + (0.1875D0*CA12*CA3*EL*MH12*SA1*SA22*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.03125D0*CA3*E&
  &L*MH22*SA1*SA22*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.09375D0*CA12*CA3*EL*MH22*SA1*SA22*DBLE(MW**INT(-3.D0))*dMW2Alte&
  &r())/(CB*SW) - (0.09375D0*CA1*EL*MH12*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.28125D0*CA1*CA22*EL*MH12*SA2*SA3*&
  &DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.046875D0*CA1*EL*MH22*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.140&
  &625D0*CA1*CA22*EL*MH22*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.140625D0*EL*m12squared*SA1*SA2*SA3*DBLE(MW**INT(&
  &-3.D0))*dMW2Alter())/(CB*SW) - (0.421875D0*CA22*EL*m12squared*SA1*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.09375&
  &D0*CA1*EL*MH12*SA12*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.28125D0*CA1*CA22*EL*MH12*SA12*SA2*SA3*DBLE(MW**INT(&
  &-3.D0))*dMW2Alter())/(CB*SW) + (0.046875D0*CA1*EL*MH22*SA12*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.140625D0*CA&
  &1*CA22*EL*MH22*SA12*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.0625D0*CA1*CA3*EL*MH12*DBLE(MW**INT(-3.D0))*dMW2Alt&
  &er())/(SB*SW) + (0.0625D0*CA1*CA22*CA3*EL*MH12*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.03125D0*CA1*CA3*EL*MH22*DBLE(MW*&
  &*INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.03125D0*CA1*CA22*CA3*EL*MH22*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.109375D0*CA&
  &3*EL*m12squared*SA1*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.109375D0*CA22*CA3*EL*m12squared*SA1*DBLE(MW**INT(-3.D0))*dM&
  &W2Alter())/(SB*SW) + (0.078125D0*CA3*EL*m12squared*SA1*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB2*SB*SW) + (0.28125D0*CA12*CA3*EL*&
  &m12squared*SA1*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB2*SB*SW) + (0.078125D0*CA22*CA3*EL*m12squared*SA1*DBLE(MW**INT(-3.D0))*dMW&
  &2Alter())/(CB2*SB*SW) + (0.28125D0*CA12*CA22*CA3*EL*m12squared*SA1*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB2*SB*SW) + (0.1875D0*C&
  &A1*CA3*EL*MH12*SA12*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.1875D0*CA1*CA22*CA3*EL*MH12*SA12*DBLE(MW**INT(-3.D0))*dMW2A&
  &lter())/(SB*SW) + (0.09375D0*CA1*CA3*EL*MH22*SA12*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.09375D0*CA1*CA22*CA3*EL*MH22*&
  &SA12*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.0625D0*CA1*CA3*EL*MH12*SA22*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0&
  &.03125D0*CA1*CA3*EL*MH22*SA22*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.109375D0*CA3*EL*m12squared*SA1*SA22*DBLE(MW**INT(&
  &-3.D0))*dMW2Alter())/(SB*SW) - (0.078125D0*CA3*EL*m12squared*SA1*SA22*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB2*SB*SW) - (0.28125&
  &D0*CA12*CA3*EL*m12squared*SA1*SA22*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB2*SB*SW) - (0.1875D0*CA1*CA3*EL*MH12*SA12*SA22*DBLE(MW&
  &**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.09375D0*CA1*CA3*EL*MH22*SA12*SA22*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.14062&
  &5D0*CA1*EL*m12squared*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.421875D0*CA1*CA22*EL*m12squared*SA2*SA3*DBLE(MW**&
  &INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.09375D0*CA1*EL*m12squared*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB2*SB*SW) + (0.281&
  &25D0*CA1*CA22*EL*m12squared*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB2*SB*SW) - (0.09375D0*EL*MH12*SA1*SA2*SA3*DBLE(MW**IN&
  &T(-3.D0))*dMW2Alter())/(SB*SW) + (0.09375D0*CA12*EL*MH12*SA1*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.28125D0*CA&
  &22*EL*MH12*SA1*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.28125D0*CA12*CA22*EL*MH12*SA1*SA2*SA3*DBLE(MW**INT(-3.D0&
  &))*dMW2Alter())/(SB*SW) - (0.046875D0*EL*MH22*SA1*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.046875D0*CA12*EL*MH22&
  &*SA1*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.140625D0*CA22*EL*MH22*SA1*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter()&
  &)/(SB*SW) + (0.140625D0*CA12*CA22*EL*MH22*SA1*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.140625D0*CA1*EL*m12square&
  &d*SA12*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB2*SB*SW) - (0.421875D0*CA1*CA22*EL*m12squared*SA12*SA2*SA3*DBLE(MW**INT(-3&
  &.D0))*dMW2Alter())/(CB2*SB*SW) - (0.03125D0*CA1*CA3*EL*m12squared*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SB2*SW) - (0.03125D0*C&
  &A1*CA22*CA3*EL*m12squared*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SB2*SW) - (0.28125D0*CA1*CA3*EL*m12squared*SA12*DBLE(MW**INT(-&
  &3.D0))*dMW2Alter())/(CB*SB2*SW) - (0.28125D0*CA1*CA22*CA3*EL*m12squared*SA12*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SB2*SW) + (&
  &0.03125D0*CA1*CA3*EL*m12squared*SA22*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SB2*SW) + (0.28125D0*CA1*CA3*EL*m12squared*SA12*SA2&
  &2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SB2*SW) + (0.09375D0*EL*m12squared*SA1*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*S&
  &B2*SW) - (0.140625D0*CA12*EL*m12squared*SA1*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SB2*SW) + (0.28125D0*CA22*EL*m12squa&
  &red*SA1*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SB2*SW) - (0.421875D0*CA12*CA22*EL*m12squared*SA1*SA2*SA3*DBLE(MW**INT(-&
  &3.D0))*dMW2Alter())/(CB*SB2*SW) - (0.0625D0*CA1*CA3*EL*m12squared*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW*TB) - (0.0625D0*CA1&
  &*CA22*CA3*EL*m12squared*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW*TB) + (0.0625D0*CA1*CA3*EL*m12squared*SA22*DBLE(MW**INT(-3.D0&
  &))*dMW2Alter())/(SB*SW*TB) + (0.046875D0*EL*m12squared*SA1*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW*TB) + (0.140625D0*&
  &CA22*EL*m12squared*SA1*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW*TB) + (0.015625D0*CA3*EL*m12squared*SA1*TB*DBLE(MW**IN&
  &T(-3.D0))*dMW2Alter())/(CB*SW) + (0.015625D0*CA22*CA3*EL*m12squared*SA1*TB*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.0156&
  &25D0*CA3*EL*m12squared*SA1*SA22*TB*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.046875D0*CA1*EL*m12squared*SA2*SA3*TB*DBLE(M&
  &W**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.140625D0*CA1*CA22*EL*m12squared*SA2*SA3*TB*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) &
  &- (0.03125D0*EL*MH12*SA2*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.09375D0*CA22*EL*MH12*SA2*SA3*&
  &DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.015625D0*EL*MH22*SA2*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT&
  &(-3.D0))*dMW2Alter())/(CB*SW) - (0.046875D0*CA22*EL*MH22*SA2*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW&
  &) - (0.0625D0*CA3*EL*MH12*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.0625D0*CA22*CA3*EL*MH12*DBLE(CA1&
  &**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.03125D0*CA3*EL*MH22*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2&
  &Alter())/(SB*SW) - (0.03125D0*CA22*CA3*EL*MH22*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.0625D0*CA3*&
  &EL*MH12*SA22*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.03125D0*CA3*EL*MH22*SA22*DBLE(CA1**INT(3.D0))&
  &*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.046875D0*EL*m12squared*SA2*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2A&
  &lter())/(CB2*SB*SW) + (0.140625D0*CA22*EL*m12squared*SA2*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB2*SB*SW&
  &) + (0.09375D0*CA3*EL*m12squared*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SB2*SW) + (0.09375D0*CA22*CA3*EL*m&
  &12squared*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SB2*SW) - (0.09375D0*CA3*EL*m12squared*SA22*DBLE(CA1**INT&
  &(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SB2*SW) + (0.0625D0*CA3*EL*MH12*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Al&
  &ter())/(CB*SW) + (0.0625D0*CA22*CA3*EL*MH12*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(CB*SW) + (0.03125D0*CA3*EL&
  &*MH22*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(CB*SW) + (0.03125D0*CA22*CA3*EL*MH22*DBLE(MW**INT(-3.D0))*DBLE(S&
  &A1**INT(3.D0))*dMW2Alter())/(CB*SW) - (0.0625D0*CA3*EL*MH12*SA22*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(CB*SW&
  &) - (0.03125D0*CA3*EL*MH22*SA22*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(CB*SW) - (0.09375D0*CA3*EL*m12squared*&
  &DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(CB2*SB*SW) - (0.09375D0*CA22*CA3*EL*m12squared*DBLE(MW**INT(-3.D0))*DB&
  &LE(SA1**INT(3.D0))*dMW2Alter())/(CB2*SB*SW) + (0.09375D0*CA3*EL*m12squared*SA22*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2&
  &Alter())/(CB2*SB*SW) - (0.03125D0*EL*MH12*SA2*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(SB*SW) - (0.09375D0*&
  &CA22*EL*MH12*SA2*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(SB*SW) - (0.015625D0*EL*MH22*SA2*SA3*DBLE(MW**INT&
  &(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(SB*SW) - (0.046875D0*CA22*EL*MH22*SA2*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0)&
  &)*dMW2Alter())/(SB*SW) + (0.046875D0*EL*m12squared*SA2*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(CB*SB2*SW) &
  &+ (0.140625D0*CA22*EL*m12squared*SA2*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(CB*SB2*SW) + (0.09375D0*CA1*E&
  &L*MH12*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(CB*SW) + (0.046875D0*CA1*EL*MH22*SA3*DBLE(MW**INT(-3.D0))*D&
  &BLE(SA2**INT(3.D0))*dMW2Alter())/(CB*SW) + (0.140625D0*EL*m12squared*SA1*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alt&
  &er())/(CB*SW) - (0.09375D0*CA1*EL*MH12*SA12*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(CB*SW) - (0.046875D0*C&
  &A1*EL*MH22*SA12*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(CB*SW) + (0.140625D0*CA1*EL*m12squared*SA3*DBLE(MW&
  &**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(SB*SW) - (0.09375D0*CA1*EL*m12squared*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(&
  &3.D0))*dMW2Alter())/(CB2*SB*SW) + (0.09375D0*EL*MH12*SA1*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(SB*SW) - &
  &(0.09375D0*CA12*EL*MH12*SA1*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(SB*SW) + (0.046875D0*EL*MH22*SA1*SA3*D&
  &BLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(SB*SW) - (0.046875D0*CA12*EL*MH22*SA1*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA2&
  &**INT(3.D0))*dMW2Alter())/(SB*SW) + (0.140625D0*CA1*EL*m12squared*SA12*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter&
  &())/(CB2*SB*SW) - (0.09375D0*EL*m12squared*SA1*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(CB*SB2*SW) + (0.140&
  &625D0*CA12*EL*m12squared*SA1*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(CB*SB2*SW) - (0.046875D0*EL*m12square&
  &d*SA1*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(SB*SW*TB) - (0.046875D0*CA1*EL*m12squared*SA3*TB*DBLE(MW**IN&
  &T(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(CB*SW) + (0.03125D0*EL*MH12*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*DBLE(SA&
  &2**INT(3.D0))*dMW2Alter())/(CB*SW) + (0.015625D0*EL*MH22*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dM&
  &W2Alter())/(CB*SW) - (0.046875D0*EL*m12squared*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())&
  &/(CB2*SB*SW) + (0.03125D0*EL*MH12*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(SB*SW) + (0&
  &.015625D0*EL*MH22*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(SB*SW) - (0.046875D0*EL*m12&
  &squared*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(CB*SB2*SW) + 0.5D0*CA2*MH12*SA3*DBLE(&
  &vS**INT(-2.D0))*dvSMSBarAlter() + 0.25D0*CA2*MH22*SA3*DBLE(vS**INT(-2.D0))*dvSMSBarAlter() + 1.5D0*CA2*MH12*SA22*SA3*DBLE(vS**&
  &INT(-2.D0))*dvSMSBarAlter() + 0.75D0*CA2*MH22*SA22*SA3*DBLE(vS**INT(-2.D0))*dvSMSBarAlter() - 0.5D0*MH12*SA3*DBLE(CA2**INT(3.D&
  &0))*DBLE(vS**INT(-2.D0))*dvSMSBarAlter() - 0.25D0*MH22*SA3*DBLE(CA2**INT(3.D0))*DBLE(vS**INT(-2.D0))*dvSMSBarAlter() &
		& + CS1S1S1f111*dZH1H2OSAlter()/2D0 + CS1S1S1f211*dZH2H2OS()/2D0 + CS1S1S1f311*dZH3H2OSAlter()/2D0 &
		& + CS1S1S1f121*dZH1H1OS()/2D0 + CS1S1S1f221*dZH2H1OSAlter()/2D0 + CS1S1S1f321*dZH3H1OSAlter()/2D0 &
		& + CS1S1S1f121*dZH1H1OS()/2D0 + CS1S1S1f221*dZH2H1OSAlter()/2D0 + CS1S1S1f321*dZH3H1OSAlter()/2D0 &
		& )
	case (9)
		dRR11 = -CA2*SA1*dAlpha1MSBarUsual() - CA1*SA2*dAlpha2MSBarUsual()
		dRR12 = CA1*CA2*dAlpha1MSBarUsual() - SA1*SA2*dAlpha2MSBarUsual()
		dRR13 = CA2*dAlpha2MSBarUsual()
		dRR21 = -CA1*CA3*dAlpha1MSBarUsual() - CA1*CA3*SA2*dAlpha3MSBarUsual() - CA1*CA2*SA3*dAlpha2MSBarUsual() + &
			& SA1*SA3*dAlpha3MSBarUsual() + SA1*SA2*SA3*dAlpha1MSBarUsual()
		dRR22 = -CA3*SA1*dAlpha1MSBarUsual() - CA3*SA1*SA2*dAlpha3MSBarUsual() - CA1*SA3*dAlpha3MSBarUsual() - &
			& CA2*SA1*SA3*dAlpha2MSBarUsual() - CA1*SA2*SA3*dAlpha1MSBarUsual()
		dRR23 = CA2*CA3*dAlpha3MSBarUsual() - SA2*SA3*dAlpha2MSBarUsual()
		dRR31 = -CA1*CA2*CA3*dAlpha2MSBarUsual() + CA3*SA1*dAlpha3MSBarUsual() + CA3*SA1*SA2*dAlpha1MSBarUsual() + &
			& CA1*SA3*dAlpha1MSBarUsual() + CA1*SA2*SA3*dAlpha3MSBarUsual()
		dRR32 = -CA1*CA3*dAlpha3MSBarUsual() - CA2*CA3*SA1*dAlpha2MSBarUsual() - CA1*CA3*SA2*dAlpha1MSBarUsual() + &
			& SA1*SA3*dAlpha1MSBarUsual() + SA1*SA2*SA3*dAlpha3MSBarUsual()
		dRR33 = -CA3*SA2*dAlpha2MSBarUsual() - CA2*SA3*dAlpha3MSBarUsual()

		totalAmplitude = CS1S1S1f211*( &
&(0.125D0*CA1*CA3*EL*MH12*dAlpha1MSBarUsual())/(CB*MW*SW) + (0.125D0*CA1*CA22*CA3*EL*MH12*dAlpha1MSBarUsual())/(CB*MW*SW) + (0.062&
  &5D0*CA1*CA3*EL*MH22*dAlpha1MSBarUsual())/(CB*MW*SW) + (0.0625D0*CA1*CA22*CA3*EL*MH22*dAlpha1MSBarUsual())/(CB*MW*SW) + (0.125D&
  &0*CA3*EL*m12squared*SA1*dAlpha1MSBarUsual())/(CB*MW*SW) + (0.125D0*CA22*CA3*EL*m12squared*SA1*dAlpha1MSBarUsual())/(CB*MW*SW) &
  &- (1.125D0*CA1*CA3*EL*MH12*SA12*dAlpha1MSBarUsual())/(CB*MW*SW) - (1.125D0*CA1*CA22*CA3*EL*MH12*SA12*dAlpha1MSBarUsual())/(CB*&
  &MW*SW) - (0.5625D0*CA1*CA3*EL*MH22*SA12*dAlpha1MSBarUsual())/(CB*MW*SW) - (0.5625D0*CA1*CA22*CA3*EL*MH22*SA12*dAlpha1MSBarUsua&
  &l())/(CB*MW*SW) - (0.125D0*CA1*CA3*EL*MH12*SA22*dAlpha1MSBarUsual())/(CB*MW*SW) - (0.0625D0*CA1*CA3*EL*MH22*SA22*dAlpha1MSBarU&
  &sual())/(CB*MW*SW) - (0.125D0*CA3*EL*m12squared*SA1*SA22*dAlpha1MSBarUsual())/(CB*MW*SW) + (1.125D0*CA1*CA3*EL*MH12*SA12*SA22*&
  &dAlpha1MSBarUsual())/(CB*MW*SW) + (0.5625D0*CA1*CA3*EL*MH22*SA12*SA22*dAlpha1MSBarUsual())/(CB*MW*SW) + (0.28125D0*CA1*EL*m12s&
  &quared*SA2*SA3*dAlpha1MSBarUsual())/(CB*MW*SW) + (0.84375D0*CA1*CA22*EL*m12squared*SA2*SA3*dAlpha1MSBarUsual())/(CB*MW*SW) - (&
  &0.1875D0*EL*MH12*SA1*SA2*SA3*dAlpha1MSBarUsual())/(CB*MW*SW) - (0.5625D0*CA12*EL*MH12*SA1*SA2*SA3*dAlpha1MSBarUsual())/(CB*MW*&
  &SW) - (0.5625D0*CA22*EL*MH12*SA1*SA2*SA3*dAlpha1MSBarUsual())/(CB*MW*SW) - (1.6875D0*CA12*CA22*EL*MH12*SA1*SA2*SA3*dAlpha1MSBa&
  &rUsual())/(CB*MW*SW) - (0.09375D0*EL*MH22*SA1*SA2*SA3*dAlpha1MSBarUsual())/(CB*MW*SW) - (0.28125D0*CA12*EL*MH22*SA1*SA2*SA3*dA&
  &lpha1MSBarUsual())/(CB*MW*SW) - (0.28125D0*CA22*EL*MH22*SA1*SA2*SA3*dAlpha1MSBarUsual())/(CB*MW*SW) - (0.84375D0*CA12*CA22*EL*&
  &MH22*SA1*SA2*SA3*dAlpha1MSBarUsual())/(CB*MW*SW) + (0.21875D0*CA1*CA3*EL*m12squared*dAlpha1MSBarUsual())/(MW*SB*SW) + (0.21875&
  &D0*CA1*CA22*CA3*EL*m12squared*dAlpha1MSBarUsual())/(MW*SB*SW) - (0.15625D0*CA1*CA3*EL*m12squared*dAlpha1MSBarUsual())/(CB2*MW*&
  &SB*SW) - (0.15625D0*CA1*CA22*CA3*EL*m12squared*dAlpha1MSBarUsual())/(CB2*MW*SB*SW) + (0.125D0*CA3*EL*MH12*SA1*dAlpha1MSBarUsua&
  &l())/(MW*SB*SW) - (1.125D0*CA12*CA3*EL*MH12*SA1*dAlpha1MSBarUsual())/(MW*SB*SW) + (0.125D0*CA22*CA3*EL*MH12*SA1*dAlpha1MSBarUs&
  &ual())/(MW*SB*SW) - (1.125D0*CA12*CA22*CA3*EL*MH12*SA1*dAlpha1MSBarUsual())/(MW*SB*SW) + (0.0625D0*CA3*EL*MH22*SA1*dAlpha1MSBa&
  &rUsual())/(MW*SB*SW) - (0.5625D0*CA12*CA3*EL*MH22*SA1*dAlpha1MSBarUsual())/(MW*SB*SW) + (0.0625D0*CA22*CA3*EL*MH22*SA1*dAlpha1&
  &MSBarUsual())/(MW*SB*SW) - (0.5625D0*CA12*CA22*CA3*EL*MH22*SA1*dAlpha1MSBarUsual())/(MW*SB*SW) + (1.6875D0*CA1*CA3*EL*m12squar&
  &ed*SA12*dAlpha1MSBarUsual())/(CB2*MW*SB*SW) + (1.6875D0*CA1*CA22*CA3*EL*m12squared*SA12*dAlpha1MSBarUsual())/(CB2*MW*SB*SW) - &
  &(0.21875D0*CA1*CA3*EL*m12squared*SA22*dAlpha1MSBarUsual())/(MW*SB*SW) + (0.15625D0*CA1*CA3*EL*m12squared*SA22*dAlpha1MSBarUsua&
  &l())/(CB2*MW*SB*SW) - (0.125D0*CA3*EL*MH12*SA1*SA22*dAlpha1MSBarUsual())/(MW*SB*SW) + (1.125D0*CA12*CA3*EL*MH12*SA1*SA22*dAlph&
  &a1MSBarUsual())/(MW*SB*SW) - (0.0625D0*CA3*EL*MH22*SA1*SA22*dAlpha1MSBarUsual())/(MW*SB*SW) + (0.5625D0*CA12*CA3*EL*MH22*SA1*S&
  &A22*dAlpha1MSBarUsual())/(MW*SB*SW) - (1.6875D0*CA1*CA3*EL*m12squared*SA12*SA22*dAlpha1MSBarUsual())/(CB2*MW*SB*SW) + (0.1875D&
  &0*CA1*EL*MH12*SA2*SA3*dAlpha1MSBarUsual())/(MW*SB*SW) + (0.5625D0*CA1*CA22*EL*MH12*SA2*SA3*dAlpha1MSBarUsual())/(MW*SB*SW) + (&
  &0.09375D0*CA1*EL*MH22*SA2*SA3*dAlpha1MSBarUsual())/(MW*SB*SW) + (0.28125D0*CA1*CA22*EL*MH22*SA2*SA3*dAlpha1MSBarUsual())/(MW*S&
  &B*SW) - (0.28125D0*EL*m12squared*SA1*SA2*SA3*dAlpha1MSBarUsual())/(MW*SB*SW) - (0.84375D0*CA22*EL*m12squared*SA1*SA2*SA3*dAlph&
  &a1MSBarUsual())/(MW*SB*SW) + (0.1875D0*EL*m12squared*SA1*SA2*SA3*dAlpha1MSBarUsual())/(CB2*MW*SB*SW) + (0.84375D0*CA12*EL*m12s&
  &quared*SA1*SA2*SA3*dAlpha1MSBarUsual())/(CB2*MW*SB*SW) + (0.5625D0*CA22*EL*m12squared*SA1*SA2*SA3*dAlpha1MSBarUsual())/(CB2*MW&
  &*SB*SW) + (2.53125D0*CA12*CA22*EL*m12squared*SA1*SA2*SA3*dAlpha1MSBarUsual())/(CB2*MW*SB*SW) + (0.5625D0*CA1*EL*MH12*SA12*SA2*&
  &SA3*dAlpha1MSBarUsual())/(MW*SB*SW) + (1.6875D0*CA1*CA22*EL*MH12*SA12*SA2*SA3*dAlpha1MSBarUsual())/(MW*SB*SW) + (0.28125D0*CA1&
  &*EL*MH22*SA12*SA2*SA3*dAlpha1MSBarUsual())/(MW*SB*SW) + (0.84375D0*CA1*CA22*EL*MH22*SA12*SA2*SA3*dAlpha1MSBarUsual())/(MW*SB*S&
  &W) - (0.0625D0*CA3*EL*m12squared*SA1*dAlpha1MSBarUsual())/(CB*MW*SB2*SW) + (1.6875D0*CA12*CA3*EL*m12squared*SA1*dAlpha1MSBarUs&
  &ual())/(CB*MW*SB2*SW) - (0.0625D0*CA22*CA3*EL*m12squared*SA1*dAlpha1MSBarUsual())/(CB*MW*SB2*SW) + (1.6875D0*CA12*CA22*CA3*EL*&
  &m12squared*SA1*dAlpha1MSBarUsual())/(CB*MW*SB2*SW) + (0.0625D0*CA3*EL*m12squared*SA1*SA22*dAlpha1MSBarUsual())/(CB*MW*SB2*SW) &
  &- (1.6875D0*CA12*CA3*EL*m12squared*SA1*SA22*dAlpha1MSBarUsual())/(CB*MW*SB2*SW) - (0.1875D0*CA1*EL*m12squared*SA2*SA3*dAlpha1M&
  &SBarUsual())/(CB*MW*SB2*SW) - (0.5625D0*CA1*CA22*EL*m12squared*SA2*SA3*dAlpha1MSBarUsual())/(CB*MW*SB2*SW) - (0.84375D0*CA1*EL&
  &*m12squared*SA12*SA2*SA3*dAlpha1MSBarUsual())/(CB*MW*SB2*SW) - (2.53125D0*CA1*CA22*EL*m12squared*SA12*SA2*SA3*dAlpha1MSBarUsua&
  &l())/(CB*MW*SB2*SW) - (0.125D0*CA3*EL*m12squared*SA1*dAlpha1MSBarUsual())/(MW*SB*SW*TB) - (0.125D0*CA22*CA3*EL*m12squared*SA1*&
  &dAlpha1MSBarUsual())/(MW*SB*SW*TB) + (0.125D0*CA3*EL*m12squared*SA1*SA22*dAlpha1MSBarUsual())/(MW*SB*SW*TB) - (0.09375D0*CA1*E&
  &L*m12squared*SA2*SA3*dAlpha1MSBarUsual())/(MW*SB*SW*TB) - (0.28125D0*CA1*CA22*EL*m12squared*SA2*SA3*dAlpha1MSBarUsual())/(MW*S&
  &B*SW*TB) - (0.03125D0*CA1*CA3*EL*m12squared*TB*dAlpha1MSBarUsual())/(CB*MW*SW) - (0.03125D0*CA1*CA22*CA3*EL*m12squared*TB*dAlp&
  &ha1MSBarUsual())/(CB*MW*SW) + (0.03125D0*CA1*CA3*EL*m12squared*SA22*TB*dAlpha1MSBarUsual())/(CB*MW*SW) + (0.09375D0*EL*m12squa&
  &red*SA1*SA2*SA3*TB*dAlpha1MSBarUsual())/(CB*MW*SW) + (0.28125D0*CA22*EL*m12squared*SA1*SA2*SA3*TB*dAlpha1MSBarUsual())/(CB*MW*&
  &SW) + (0.5D0*CA1*CA2*CA3*EL*m12squared*SA2*dAlpha2MSBarUsual())/(CB*MW*SW) - (0.5D0*CA2*CA3*EL*MH12*SA1*SA2*dAlpha2MSBarUsual(&
  &))/(CB*MW*SW) - (1.5D0*CA12*CA2*CA3*EL*MH12*SA1*SA2*dAlpha2MSBarUsual())/(CB*MW*SW) - (0.25D0*CA2*CA3*EL*MH22*SA1*SA2*dAlpha2M&
  &SBarUsual())/(CB*MW*SW) - (0.75D0*CA12*CA2*CA3*EL*MH22*SA1*SA2*dAlpha2MSBarUsual())/(CB*MW*SW) + (0.1875D0*CA1*CA2*EL*MH12*SA3&
  &*dAlpha2MSBarUsual())/(CB*MW*SW) + (0.09375D0*CA1*CA2*EL*MH22*SA3*dAlpha2MSBarUsual())/(CB*MW*SW) + (0.28125D0*CA2*EL*m12squar&
  &ed*SA1*SA3*dAlpha2MSBarUsual())/(CB*MW*SW) - (0.1875D0*CA1*CA2*EL*MH12*SA12*SA3*dAlpha2MSBarUsual())/(CB*MW*SW) - (0.09375D0*C&
  &A1*CA2*EL*MH22*SA12*SA3*dAlpha2MSBarUsual())/(CB*MW*SW) - (1.6875D0*CA1*CA2*EL*MH12*SA22*SA3*dAlpha2MSBarUsual())/(CB*MW*SW) -&
  & (0.84375D0*CA1*CA2*EL*MH22*SA22*SA3*dAlpha2MSBarUsual())/(CB*MW*SW) - (2.53125D0*CA2*EL*m12squared*SA1*SA22*SA3*dAlpha2MSBarU&
  &sual())/(CB*MW*SW) + (1.6875D0*CA1*CA2*EL*MH12*SA12*SA22*SA3*dAlpha2MSBarUsual())/(CB*MW*SW) + (0.84375D0*CA1*CA2*EL*MH22*SA12&
  &*SA22*SA3*dAlpha2MSBarUsual())/(CB*MW*SW) + (0.5D0*CA1*CA2*CA3*EL*MH12*SA2*dAlpha2MSBarUsual())/(MW*SB*SW) + (0.25D0*CA1*CA2*C&
  &A3*EL*MH22*SA2*dAlpha2MSBarUsual())/(MW*SB*SW) - (0.875D0*CA2*CA3*EL*m12squared*SA1*SA2*dAlpha2MSBarUsual())/(MW*SB*SW) + (0.6&
  &25D0*CA2*CA3*EL*m12squared*SA1*SA2*dAlpha2MSBarUsual())/(CB2*MW*SB*SW) + (2.25D0*CA12*CA2*CA3*EL*m12squared*SA1*SA2*dAlpha2MSB&
  &arUsual())/(CB2*MW*SB*SW) + (1.5D0*CA1*CA2*CA3*EL*MH12*SA12*SA2*dAlpha2MSBarUsual())/(MW*SB*SW) + (0.75D0*CA1*CA2*CA3*EL*MH22*&
  &SA12*SA2*dAlpha2MSBarUsual())/(MW*SB*SW) + (0.28125D0*CA1*CA2*EL*m12squared*SA3*dAlpha2MSBarUsual())/(MW*SB*SW) - (0.1875D0*CA&
  &1*CA2*EL*m12squared*SA3*dAlpha2MSBarUsual())/(CB2*MW*SB*SW) + (0.1875D0*CA2*EL*MH12*SA1*SA3*dAlpha2MSBarUsual())/(MW*SB*SW) - &
  &(0.1875D0*CA12*CA2*EL*MH12*SA1*SA3*dAlpha2MSBarUsual())/(MW*SB*SW) + (0.09375D0*CA2*EL*MH22*SA1*SA3*dAlpha2MSBarUsual())/(MW*S&
  &B*SW) - (0.09375D0*CA12*CA2*EL*MH22*SA1*SA3*dAlpha2MSBarUsual())/(MW*SB*SW) + (0.28125D0*CA1*CA2*EL*m12squared*SA12*SA3*dAlpha&
  &2MSBarUsual())/(CB2*MW*SB*SW) - (2.53125D0*CA1*CA2*EL*m12squared*SA22*SA3*dAlpha2MSBarUsual())/(MW*SB*SW) + (1.6875D0*CA1*CA2*&
  &EL*m12squared*SA22*SA3*dAlpha2MSBarUsual())/(CB2*MW*SB*SW) - (1.6875D0*CA2*EL*MH12*SA1*SA22*SA3*dAlpha2MSBarUsual())/(MW*SB*SW&
  &) + (1.6875D0*CA12*CA2*EL*MH12*SA1*SA22*SA3*dAlpha2MSBarUsual())/(MW*SB*SW) - (0.84375D0*CA2*EL*MH22*SA1*SA22*SA3*dAlpha2MSBar&
  &Usual())/(MW*SB*SW) + (0.84375D0*CA12*CA2*EL*MH22*SA1*SA22*SA3*dAlpha2MSBarUsual())/(MW*SB*SW) - (2.53125D0*CA1*CA2*EL*m12squa&
  &red*SA12*SA22*SA3*dAlpha2MSBarUsual())/(CB2*MW*SB*SW) - (0.25D0*CA1*CA2*CA3*EL*m12squared*SA2*dAlpha2MSBarUsual())/(CB*MW*SB2*&
  &SW) - (2.25D0*CA1*CA2*CA3*EL*m12squared*SA12*SA2*dAlpha2MSBarUsual())/(CB*MW*SB2*SW) - (0.1875D0*CA2*EL*m12squared*SA1*SA3*dAl&
  &pha2MSBarUsual())/(CB*MW*SB2*SW) + (0.28125D0*CA12*CA2*EL*m12squared*SA1*SA3*dAlpha2MSBarUsual())/(CB*MW*SB2*SW) + (1.6875D0*C&
  &A2*EL*m12squared*SA1*SA22*SA3*dAlpha2MSBarUsual())/(CB*MW*SB2*SW) - (2.53125D0*CA12*CA2*EL*m12squared*SA1*SA22*SA3*dAlpha2MSBa&
  &rUsual())/(CB*MW*SB2*SW) - (0.5D0*CA1*CA2*CA3*EL*m12squared*SA2*dAlpha2MSBarUsual())/(MW*SB*SW*TB) - (0.09375D0*CA2*EL*m12squa&
  &red*SA1*SA3*dAlpha2MSBarUsual())/(MW*SB*SW*TB) + (0.84375D0*CA2*EL*m12squared*SA1*SA22*SA3*dAlpha2MSBarUsual())/(MW*SB*SW*TB) &
  &+ (0.125D0*CA2*CA3*EL*m12squared*SA1*SA2*TB*dAlpha2MSBarUsual())/(CB*MW*SW) - (0.09375D0*CA1*CA2*EL*m12squared*SA3*TB*dAlpha2M&
  &SBarUsual())/(CB*MW*SW) + (0.84375D0*CA1*CA2*EL*m12squared*SA22*SA3*TB*dAlpha2MSBarUsual())/(CB*MW*SW) + (0.5D0*MH12*SA2*SA3*d&
  &Alpha2MSBarUsual())/vS - (4.5D0*CA22*MH12*SA2*SA3*dAlpha2MSBarUsual())/vS + (0.25D0*MH22*SA2*SA3*dAlpha2MSBarUsual())/vS - (2.&
  &25D0*CA22*MH22*SA2*SA3*dAlpha2MSBarUsual())/vS + (0.1875D0*CA1*CA3*EL*MH12*SA2*dAlpha3MSBarUsual())/(CB*MW*SW) + (0.5625D0*CA1&
  &*CA22*CA3*EL*MH12*SA2*dAlpha3MSBarUsual())/(CB*MW*SW) + (0.09375D0*CA1*CA3*EL*MH22*SA2*dAlpha3MSBarUsual())/(CB*MW*SW) + (0.28&
  &125D0*CA1*CA22*CA3*EL*MH22*SA2*dAlpha3MSBarUsual())/(CB*MW*SW) + (0.28125D0*CA3*EL*m12squared*SA1*SA2*dAlpha3MSBarUsual())/(CB&
  &*MW*SW) + (0.84375D0*CA22*CA3*EL*m12squared*SA1*SA2*dAlpha3MSBarUsual())/(CB*MW*SW) - (0.1875D0*CA1*CA3*EL*MH12*SA12*SA2*dAlph&
  &a3MSBarUsual())/(CB*MW*SW) - (0.5625D0*CA1*CA22*CA3*EL*MH12*SA12*SA2*dAlpha3MSBarUsual())/(CB*MW*SW) - (0.09375D0*CA1*CA3*EL*M&
  &H22*SA12*SA2*dAlpha3MSBarUsual())/(CB*MW*SW) - (0.28125D0*CA1*CA22*CA3*EL*MH22*SA12*SA2*dAlpha3MSBarUsual())/(CB*MW*SW) + (0.1&
  &25D0*CA1*EL*m12squared*SA3*dAlpha3MSBarUsual())/(CB*MW*SW) + (0.125D0*CA1*CA22*EL*m12squared*SA3*dAlpha3MSBarUsual())/(CB*MW*S&
  &W) - (0.125D0*EL*MH12*SA1*SA3*dAlpha3MSBarUsual())/(CB*MW*SW) - (0.375D0*CA12*EL*MH12*SA1*SA3*dAlpha3MSBarUsual())/(CB*MW*SW) &
  &- (0.125D0*CA22*EL*MH12*SA1*SA3*dAlpha3MSBarUsual())/(CB*MW*SW) - (0.375D0*CA12*CA22*EL*MH12*SA1*SA3*dAlpha3MSBarUsual())/(CB*&
  &MW*SW) - (0.0625D0*EL*MH22*SA1*SA3*dAlpha3MSBarUsual())/(CB*MW*SW) - (0.1875D0*CA12*EL*MH22*SA1*SA3*dAlpha3MSBarUsual())/(CB*M&
  &W*SW) - (0.0625D0*CA22*EL*MH22*SA1*SA3*dAlpha3MSBarUsual())/(CB*MW*SW) - (0.1875D0*CA12*CA22*EL*MH22*SA1*SA3*dAlpha3MSBarUsual&
  &())/(CB*MW*SW) - (0.125D0*CA1*EL*m12squared*SA22*SA3*dAlpha3MSBarUsual())/(CB*MW*SW) + (0.125D0*EL*MH12*SA1*SA22*SA3*dAlpha3MS&
  &BarUsual())/(CB*MW*SW) + (0.375D0*CA12*EL*MH12*SA1*SA22*SA3*dAlpha3MSBarUsual())/(CB*MW*SW) + (0.0625D0*EL*MH22*SA1*SA22*SA3*d&
  &Alpha3MSBarUsual())/(CB*MW*SW) + (0.1875D0*CA12*EL*MH22*SA1*SA22*SA3*dAlpha3MSBarUsual())/(CB*MW*SW) + (0.28125D0*CA1*CA3*EL*m&
  &12squared*SA2*dAlpha3MSBarUsual())/(MW*SB*SW) + (0.84375D0*CA1*CA22*CA3*EL*m12squared*SA2*dAlpha3MSBarUsual())/(MW*SB*SW) - (0&
  &.1875D0*CA1*CA3*EL*m12squared*SA2*dAlpha3MSBarUsual())/(CB2*MW*SB*SW) - (0.5625D0*CA1*CA22*CA3*EL*m12squared*SA2*dAlpha3MSBarU&
  &sual())/(CB2*MW*SB*SW) + (0.1875D0*CA3*EL*MH12*SA1*SA2*dAlpha3MSBarUsual())/(MW*SB*SW) - (0.1875D0*CA12*CA3*EL*MH12*SA1*SA2*dA&
  &lpha3MSBarUsual())/(MW*SB*SW) + (0.5625D0*CA22*CA3*EL*MH12*SA1*SA2*dAlpha3MSBarUsual())/(MW*SB*SW) - (0.5625D0*CA12*CA22*CA3*E&
  &L*MH12*SA1*SA2*dAlpha3MSBarUsual())/(MW*SB*SW) + (0.09375D0*CA3*EL*MH22*SA1*SA2*dAlpha3MSBarUsual())/(MW*SB*SW) - (0.09375D0*C&
  &A12*CA3*EL*MH22*SA1*SA2*dAlpha3MSBarUsual())/(MW*SB*SW) + (0.28125D0*CA22*CA3*EL*MH22*SA1*SA2*dAlpha3MSBarUsual())/(MW*SB*SW) &
  &- (0.28125D0*CA12*CA22*CA3*EL*MH22*SA1*SA2*dAlpha3MSBarUsual())/(MW*SB*SW) + (0.28125D0*CA1*CA3*EL*m12squared*SA12*SA2*dAlpha3&
  &MSBarUsual())/(CB2*MW*SB*SW) + (0.84375D0*CA1*CA22*CA3*EL*m12squared*SA12*SA2*dAlpha3MSBarUsual())/(CB2*MW*SB*SW) + (0.125D0*C&
  &A1*EL*MH12*SA3*dAlpha3MSBarUsual())/(MW*SB*SW) + (0.125D0*CA1*CA22*EL*MH12*SA3*dAlpha3MSBarUsual())/(MW*SB*SW) + (0.0625D0*CA1&
  &*EL*MH22*SA3*dAlpha3MSBarUsual())/(MW*SB*SW) + (0.0625D0*CA1*CA22*EL*MH22*SA3*dAlpha3MSBarUsual())/(MW*SB*SW) - (0.21875D0*EL*&
  &m12squared*SA1*SA3*dAlpha3MSBarUsual())/(MW*SB*SW) - (0.21875D0*CA22*EL*m12squared*SA1*SA3*dAlpha3MSBarUsual())/(MW*SB*SW) + (&
  &0.15625D0*EL*m12squared*SA1*SA3*dAlpha3MSBarUsual())/(CB2*MW*SB*SW) + (0.5625D0*CA12*EL*m12squared*SA1*SA3*dAlpha3MSBarUsual()&
  &)/(CB2*MW*SB*SW) + (0.15625D0*CA22*EL*m12squared*SA1*SA3*dAlpha3MSBarUsual())/(CB2*MW*SB*SW) + (0.5625D0*CA12*CA22*EL*m12squar&
  &ed*SA1*SA3*dAlpha3MSBarUsual())/(CB2*MW*SB*SW) + (0.375D0*CA1*EL*MH12*SA12*SA3*dAlpha3MSBarUsual())/(MW*SB*SW) + (0.375D0*CA1*&
  &CA22*EL*MH12*SA12*SA3*dAlpha3MSBarUsual())/(MW*SB*SW) + (0.1875D0*CA1*EL*MH22*SA12*SA3*dAlpha3MSBarUsual())/(MW*SB*SW) + (0.18&
  &75D0*CA1*CA22*EL*MH22*SA12*SA3*dAlpha3MSBarUsual())/(MW*SB*SW) - (0.125D0*CA1*EL*MH12*SA22*SA3*dAlpha3MSBarUsual())/(MW*SB*SW)&
  & - (0.0625D0*CA1*EL*MH22*SA22*SA3*dAlpha3MSBarUsual())/(MW*SB*SW) + (0.21875D0*EL*m12squared*SA1*SA22*SA3*dAlpha3MSBarUsual())&
  &/(MW*SB*SW) - (0.15625D0*EL*m12squared*SA1*SA22*SA3*dAlpha3MSBarUsual())/(CB2*MW*SB*SW) - (0.5625D0*CA12*EL*m12squared*SA1*SA2&
  &2*SA3*dAlpha3MSBarUsual())/(CB2*MW*SB*SW) - (0.375D0*CA1*EL*MH12*SA12*SA22*SA3*dAlpha3MSBarUsual())/(MW*SB*SW) - (0.1875D0*CA1&
  &*EL*MH22*SA12*SA22*SA3*dAlpha3MSBarUsual())/(MW*SB*SW) - (0.1875D0*CA3*EL*m12squared*SA1*SA2*dAlpha3MSBarUsual())/(CB*MW*SB2*S&
  &W) + (0.28125D0*CA12*CA3*EL*m12squared*SA1*SA2*dAlpha3MSBarUsual())/(CB*MW*SB2*SW) - (0.5625D0*CA22*CA3*EL*m12squared*SA1*SA2*&
  &dAlpha3MSBarUsual())/(CB*MW*SB2*SW) + (0.84375D0*CA12*CA22*CA3*EL*m12squared*SA1*SA2*dAlpha3MSBarUsual())/(CB*MW*SB2*SW) - (0.&
  &0625D0*CA1*EL*m12squared*SA3*dAlpha3MSBarUsual())/(CB*MW*SB2*SW) - (0.0625D0*CA1*CA22*EL*m12squared*SA3*dAlpha3MSBarUsual())/(&
  &CB*MW*SB2*SW) - (0.5625D0*CA1*EL*m12squared*SA12*SA3*dAlpha3MSBarUsual())/(CB*MW*SB2*SW) - (0.5625D0*CA1*CA22*EL*m12squared*SA&
  &12*SA3*dAlpha3MSBarUsual())/(CB*MW*SB2*SW) + (0.0625D0*CA1*EL*m12squared*SA22*SA3*dAlpha3MSBarUsual())/(CB*MW*SB2*SW) + (0.562&
  &5D0*CA1*EL*m12squared*SA12*SA22*SA3*dAlpha3MSBarUsual())/(CB*MW*SB2*SW) - (0.09375D0*CA3*EL*m12squared*SA1*SA2*dAlpha3MSBarUsu&
  &al())/(MW*SB*SW*TB) - (0.28125D0*CA22*CA3*EL*m12squared*SA1*SA2*dAlpha3MSBarUsual())/(MW*SB*SW*TB) - (0.125D0*CA1*EL*m12square&
  &d*SA3*dAlpha3MSBarUsual())/(MW*SB*SW*TB) - (0.125D0*CA1*CA22*EL*m12squared*SA3*dAlpha3MSBarUsual())/(MW*SB*SW*TB) + (0.125D0*C&
  &A1*EL*m12squared*SA22*SA3*dAlpha3MSBarUsual())/(MW*SB*SW*TB) - (0.09375D0*CA1*CA3*EL*m12squared*SA2*TB*dAlpha3MSBarUsual())/(C&
  &B*MW*SW) - (0.28125D0*CA1*CA22*CA3*EL*m12squared*SA2*TB*dAlpha3MSBarUsual())/(CB*MW*SW) + (0.03125D0*EL*m12squared*SA1*SA3*TB*&
  &dAlpha3MSBarUsual())/(CB*MW*SW) + (0.03125D0*CA22*EL*m12squared*SA1*SA3*TB*dAlpha3MSBarUsual())/(CB*MW*SW) - (0.03125D0*EL*m12&
  &squared*SA1*SA22*SA3*TB*dAlpha3MSBarUsual())/(CB*MW*SW) - (0.5D0*CA2*CA3*MH12*dAlpha3MSBarUsual())/vS - (0.25D0*CA2*CA3*MH22*d&
  &Alpha3MSBarUsual())/vS - (1.5D0*CA2*CA3*MH12*SA22*dAlpha3MSBarUsual())/vS - (0.75D0*CA2*CA3*MH22*SA22*dAlpha3MSBarUsual())/vS &
  &+ (0.015625D0*CA3*EL*m12squared*SA1*dBetaMSBarUsual())/(CB*MW*SW) + (0.015625D0*CA22*CA3*EL*m12squared*SA1*dBetaMSBarUsual())/&
  &(CB*MW*SW) - (0.015625D0*CA3*EL*m12squared*SA1*SA22*dBetaMSBarUsual())/(CB*MW*SW) + (0.09375D0*CA1*EL*m12squared*SA2*SA3*dBeta&
  &MSBarUsual())/(CB*MW*SW) + (0.28125D0*CA1*CA22*EL*m12squared*SA2*SA3*dBetaMSBarUsual())/(CB*MW*SW) - (0.0625D0*CA1*CA3*EL*m12s&
  &quared*dBetaMSBarUsual())/(MW*SB*SW) - (0.0625D0*CA1*CA22*CA3*EL*m12squared*dBetaMSBarUsual())/(MW*SB*SW) + (0.0625D0*CA1*CA3*&
  &EL*m12squared*dBetaMSBarUsual())/(CB2*MW*SB*SW) + (0.0625D0*CA1*CA22*CA3*EL*m12squared*dBetaMSBarUsual())/(CB2*MW*SB*SW) - (0.&
  &0625D0*CA3*EL*MH12*SA1*dBetaMSBarUsual())/(MW*SB*SW) - (0.1875D0*CA12*CA3*EL*MH12*SA1*dBetaMSBarUsual())/(MW*SB*SW) - (0.0625D&
  &0*CA22*CA3*EL*MH12*SA1*dBetaMSBarUsual())/(MW*SB*SW) - (0.1875D0*CA12*CA22*CA3*EL*MH12*SA1*dBetaMSBarUsual())/(MW*SB*SW) + (0.&
  &0625D0*CA3*EL*MH12*SA1*dBetaMSBarUsual())/(CB2*MW*SB*SW) + (0.1875D0*CA12*CA3*EL*MH12*SA1*dBetaMSBarUsual())/(CB2*MW*SB*SW) + &
  &(0.0625D0*CA22*CA3*EL*MH12*SA1*dBetaMSBarUsual())/(CB2*MW*SB*SW) + (0.1875D0*CA12*CA22*CA3*EL*MH12*SA1*dBetaMSBarUsual())/(CB2&
  &*MW*SB*SW) - (0.03125D0*CA3*EL*MH22*SA1*dBetaMSBarUsual())/(MW*SB*SW) - (0.09375D0*CA12*CA3*EL*MH22*SA1*dBetaMSBarUsual())/(MW&
  &*SB*SW) - (0.03125D0*CA22*CA3*EL*MH22*SA1*dBetaMSBarUsual())/(MW*SB*SW) - (0.09375D0*CA12*CA22*CA3*EL*MH22*SA1*dBetaMSBarUsual&
  &())/(MW*SB*SW) + (0.03125D0*CA3*EL*MH22*SA1*dBetaMSBarUsual())/(CB2*MW*SB*SW) + (0.09375D0*CA12*CA3*EL*MH22*SA1*dBetaMSBarUsua&
  &l())/(CB2*MW*SB*SW) + (0.03125D0*CA22*CA3*EL*MH22*SA1*dBetaMSBarUsual())/(CB2*MW*SB*SW) + (0.09375D0*CA12*CA22*CA3*EL*MH22*SA1&
  &*dBetaMSBarUsual())/(CB2*MW*SB*SW) + (0.5625D0*CA1*CA3*EL*m12squared*SA12*dBetaMSBarUsual())/(CB2*MW*SB*SW) + (0.5625D0*CA1*CA&
  &22*CA3*EL*m12squared*SA12*dBetaMSBarUsual())/(CB2*MW*SB*SW) + (0.0625D0*CA1*CA3*EL*m12squared*SA22*dBetaMSBarUsual())/(MW*SB*S&
  &W) - (0.0625D0*CA1*CA3*EL*m12squared*SA22*dBetaMSBarUsual())/(CB2*MW*SB*SW) + (0.0625D0*CA3*EL*MH12*SA1*SA22*dBetaMSBarUsual()&
  &)/(MW*SB*SW) + (0.1875D0*CA12*CA3*EL*MH12*SA1*SA22*dBetaMSBarUsual())/(MW*SB*SW) - (0.0625D0*CA3*EL*MH12*SA1*SA22*dBetaMSBarUs&
  &ual())/(CB2*MW*SB*SW) - (0.1875D0*CA12*CA3*EL*MH12*SA1*SA22*dBetaMSBarUsual())/(CB2*MW*SB*SW) + (0.03125D0*CA3*EL*MH22*SA1*SA2&
  &2*dBetaMSBarUsual())/(MW*SB*SW) + (0.09375D0*CA12*CA3*EL*MH22*SA1*SA22*dBetaMSBarUsual())/(MW*SB*SW) - (0.03125D0*CA3*EL*MH22*&
  &SA1*SA22*dBetaMSBarUsual())/(CB2*MW*SB*SW) - (0.09375D0*CA12*CA3*EL*MH22*SA1*SA22*dBetaMSBarUsual())/(CB2*MW*SB*SW) - (0.5625D&
  &0*CA1*CA3*EL*m12squared*SA12*SA22*dBetaMSBarUsual())/(CB2*MW*SB*SW) - (0.09375D0*CA1*EL*MH12*SA2*SA3*dBetaMSBarUsual())/(MW*SB&
  &*SW) - (0.28125D0*CA1*CA22*EL*MH12*SA2*SA3*dBetaMSBarUsual())/(MW*SB*SW) + (0.09375D0*CA1*EL*MH12*SA2*SA3*dBetaMSBarUsual())/(&
  &CB2*MW*SB*SW) + (0.28125D0*CA1*CA22*EL*MH12*SA2*SA3*dBetaMSBarUsual())/(CB2*MW*SB*SW) - (0.046875D0*CA1*EL*MH22*SA2*SA3*dBetaM&
  &SBarUsual())/(MW*SB*SW) - (0.140625D0*CA1*CA22*EL*MH22*SA2*SA3*dBetaMSBarUsual())/(MW*SB*SW) + (0.046875D0*CA1*EL*MH22*SA2*SA3&
  &*dBetaMSBarUsual())/(CB2*MW*SB*SW) + (0.140625D0*CA1*CA22*EL*MH22*SA2*SA3*dBetaMSBarUsual())/(CB2*MW*SB*SW) - (0.140625D0*EL*m&
  &12squared*SA1*SA2*SA3*dBetaMSBarUsual())/(MW*SB*SW) - (0.421875D0*CA22*EL*m12squared*SA1*SA2*SA3*dBetaMSBarUsual())/(MW*SB*SW)&
  & - (0.046875D0*EL*m12squared*SA1*SA2*SA3*dBetaMSBarUsual())/(CB2*MW*SB*SW) + (0.28125D0*CA12*EL*m12squared*SA1*SA2*SA3*dBetaMS&
  &BarUsual())/(CB2*MW*SB*SW) - (0.140625D0*CA22*EL*m12squared*SA1*SA2*SA3*dBetaMSBarUsual())/(CB2*MW*SB*SW) + (0.84375D0*CA12*CA&
  &22*EL*m12squared*SA1*SA2*SA3*dBetaMSBarUsual())/(CB2*MW*SB*SW) + (0.09375D0*CA1*EL*MH12*SA12*SA2*SA3*dBetaMSBarUsual())/(MW*SB&
  &*SW) + (0.28125D0*CA1*CA22*EL*MH12*SA12*SA2*SA3*dBetaMSBarUsual())/(MW*SB*SW) - (0.09375D0*CA1*EL*MH12*SA12*SA2*SA3*dBetaMSBar&
  &Usual())/(CB2*MW*SB*SW) - (0.28125D0*CA1*CA22*EL*MH12*SA12*SA2*SA3*dBetaMSBarUsual())/(CB2*MW*SB*SW) + (0.046875D0*CA1*EL*MH22&
  &*SA12*SA2*SA3*dBetaMSBarUsual())/(MW*SB*SW) + (0.140625D0*CA1*CA22*EL*MH22*SA12*SA2*SA3*dBetaMSBarUsual())/(MW*SB*SW) - (0.046&
  &875D0*CA1*EL*MH22*SA12*SA2*SA3*dBetaMSBarUsual())/(CB2*MW*SB*SW) - (0.140625D0*CA1*CA22*EL*MH22*SA12*SA2*SA3*dBetaMSBarUsual()&
  &)/(CB2*MW*SB*SW) + (0.203125D0*CA3*EL*m12squared*SA1*dBetaMSBarUsual())/(CB*MW*SB2*SW) + (0.84375D0*CA12*CA3*EL*m12squared*SA1&
  &*dBetaMSBarUsual())/(CB*MW*SB2*SW) + (0.203125D0*CA22*CA3*EL*m12squared*SA1*dBetaMSBarUsual())/(CB*MW*SB2*SW) + (0.84375D0*CA1&
  &2*CA22*CA3*EL*m12squared*SA1*dBetaMSBarUsual())/(CB*MW*SB2*SW) - (0.203125D0*CA3*EL*m12squared*SA1*SA22*dBetaMSBarUsual())/(CB&
  &*MW*SB2*SW) - (0.84375D0*CA12*CA3*EL*m12squared*SA1*SA22*dBetaMSBarUsual())/(CB*MW*SB2*SW) + (0.10546875D0*CA1*EL*m12squared*S&
  &A2*SA3*dBetaMSBarUsual())/(CB*MW*SB2*SW) + (0.31640625D0*CA1*CA22*EL*m12squared*SA2*SA3*dBetaMSBarUsual())/(CB*MW*SB2*SW) - (0&
  &.38671875D0*CA1*EL*m12squared*SA12*SA2*SA3*dBetaMSBarUsual())/(CB*MW*SB2*SW) - (1.16015625D0*CA1*CA22*EL*m12squared*SA12*SA2*S&
  &A3*dBetaMSBarUsual())/(CB*MW*SB2*SW) + (0.125D0*CA1*CA3*EL*MH12*dBetaMSBarUsual())/(MW*SB*SW*TB) + (0.125D0*CA1*CA22*CA3*EL*MH&
  &12*dBetaMSBarUsual())/(MW*SB*SW*TB) + (0.0625D0*CA1*CA3*EL*MH22*dBetaMSBarUsual())/(MW*SB*SW*TB) + (0.0625D0*CA1*CA22*CA3*EL*M&
  &H22*dBetaMSBarUsual())/(MW*SB*SW*TB) - (0.203125D0*CA3*EL*m12squared*SA1*dBetaMSBarUsual())/(MW*SB*SW*TB) - (0.203125D0*CA22*C&
  &A3*EL*m12squared*SA1*dBetaMSBarUsual())/(MW*SB*SW*TB) + (0.375D0*CA1*CA3*EL*MH12*SA12*dBetaMSBarUsual())/(MW*SB*SW*TB) + (0.37&
  &5D0*CA1*CA22*CA3*EL*MH12*SA12*dBetaMSBarUsual())/(MW*SB*SW*TB) + (0.1875D0*CA1*CA3*EL*MH22*SA12*dBetaMSBarUsual())/(MW*SB*SW*T&
  &B) + (0.1875D0*CA1*CA22*CA3*EL*MH22*SA12*dBetaMSBarUsual())/(MW*SB*SW*TB) - (0.125D0*CA1*CA3*EL*MH12*SA22*dBetaMSBarUsual())/(&
  &MW*SB*SW*TB) - (0.0625D0*CA1*CA3*EL*MH22*SA22*dBetaMSBarUsual())/(MW*SB*SW*TB) + (0.203125D0*CA3*EL*m12squared*SA1*SA22*dBetaM&
  &SBarUsual())/(MW*SB*SW*TB) - (0.375D0*CA1*CA3*EL*MH12*SA12*SA22*dBetaMSBarUsual())/(MW*SB*SW*TB) - (0.1875D0*CA1*CA3*EL*MH22*S&
  &A12*SA22*dBetaMSBarUsual())/(MW*SB*SW*TB) - (0.140625D0*CA1*EL*m12squared*SA2*SA3*dBetaMSBarUsual())/(MW*SB*SW*TB) - (0.421875&
  &D0*CA1*CA22*EL*m12squared*SA2*SA3*dBetaMSBarUsual())/(MW*SB*SW*TB) - (0.1875D0*EL*MH12*SA1*SA2*SA3*dBetaMSBarUsual())/(MW*SB*S&
  &W*TB) + (0.1875D0*CA12*EL*MH12*SA1*SA2*SA3*dBetaMSBarUsual())/(MW*SB*SW*TB) - (0.5625D0*CA22*EL*MH12*SA1*SA2*SA3*dBetaMSBarUsu&
  &al())/(MW*SB*SW*TB) + (0.5625D0*CA12*CA22*EL*MH12*SA1*SA2*SA3*dBetaMSBarUsual())/(MW*SB*SW*TB) - (0.09375D0*EL*MH22*SA1*SA2*SA&
  &3*dBetaMSBarUsual())/(MW*SB*SW*TB) + (0.09375D0*CA12*EL*MH22*SA1*SA2*SA3*dBetaMSBarUsual())/(MW*SB*SW*TB) - (0.28125D0*CA22*EL&
  &*MH22*SA1*SA2*SA3*dBetaMSBarUsual())/(MW*SB*SW*TB) + (0.28125D0*CA12*CA22*EL*MH22*SA1*SA2*SA3*dBetaMSBarUsual())/(MW*SB*SW*TB)&
  & - (0.125D0*CA1*CA3*EL*m12squared*TB*dBetaMSBarUsual())/(CB*MW*SW) - (0.125D0*CA1*CA22*CA3*EL*m12squared*TB*dBetaMSBarUsual())&
  &/(CB*MW*SW) + (0.0625D0*CA3*EL*MH12*SA1*TB*dBetaMSBarUsual())/(CB*MW*SW) + (0.1875D0*CA12*CA3*EL*MH12*SA1*TB*dBetaMSBarUsual()&
  &)/(CB*MW*SW) + (0.0625D0*CA22*CA3*EL*MH12*SA1*TB*dBetaMSBarUsual())/(CB*MW*SW) + (0.1875D0*CA12*CA22*CA3*EL*MH12*SA1*TB*dBetaM&
  &SBarUsual())/(CB*MW*SW) + (0.03125D0*CA3*EL*MH22*SA1*TB*dBetaMSBarUsual())/(CB*MW*SW) + (0.09375D0*CA12*CA3*EL*MH22*SA1*TB*dBe&
  &taMSBarUsual())/(CB*MW*SW) + (0.03125D0*CA22*CA3*EL*MH22*SA1*TB*dBetaMSBarUsual())/(CB*MW*SW) + (0.09375D0*CA12*CA22*CA3*EL*MH&
  &22*SA1*TB*dBetaMSBarUsual())/(CB*MW*SW) + (0.125D0*CA1*CA3*EL*m12squared*SA22*TB*dBetaMSBarUsual())/(CB*MW*SW) - (0.0625D0*CA3&
  &*EL*MH12*SA1*SA22*TB*dBetaMSBarUsual())/(CB*MW*SW) - (0.1875D0*CA12*CA3*EL*MH12*SA1*SA22*TB*dBetaMSBarUsual())/(CB*MW*SW) - (0&
  &.03125D0*CA3*EL*MH22*SA1*SA22*TB*dBetaMSBarUsual())/(CB*MW*SW) - (0.09375D0*CA12*CA3*EL*MH22*SA1*SA22*TB*dBetaMSBarUsual())/(C&
  &B*MW*SW) + (0.09375D0*CA1*EL*MH12*SA2*SA3*TB*dBetaMSBarUsual())/(CB*MW*SW) + (0.28125D0*CA1*CA22*EL*MH12*SA2*SA3*TB*dBetaMSBar&
  &Usual())/(CB*MW*SW) + (0.046875D0*CA1*EL*MH22*SA2*SA3*TB*dBetaMSBarUsual())/(CB*MW*SW) + (0.140625D0*CA1*CA22*EL*MH22*SA2*SA3*&
  &TB*dBetaMSBarUsual())/(CB*MW*SW) + (0.140625D0*EL*m12squared*SA1*SA2*SA3*TB*dBetaMSBarUsual())/(CB*MW*SW) + (0.421875D0*CA22*E&
  &L*m12squared*SA1*SA2*SA3*TB*dBetaMSBarUsual())/(CB*MW*SW) - (0.09375D0*CA1*EL*MH12*SA12*SA2*SA3*TB*dBetaMSBarUsual())/(CB*MW*S&
  &W) - (0.28125D0*CA1*CA22*EL*MH12*SA12*SA2*SA3*TB*dBetaMSBarUsual())/(CB*MW*SW) - (0.046875D0*CA1*EL*MH22*SA12*SA2*SA3*TB*dBeta&
  &MSBarUsual())/(CB*MW*SW) - (0.140625D0*CA1*CA22*EL*MH22*SA12*SA2*SA3*TB*dBetaMSBarUsual())/(CB*MW*SW) - (0.1875D0*CA1*CA3*EL*m&
  &12squared*dBetaMSBarUsual())/(MW*SB*SW*TB2) - (0.1875D0*CA1*CA22*CA3*EL*m12squared*dBetaMSBarUsual())/(MW*SB*SW*TB2) + (0.1875&
  &D0*CA1*CA3*EL*m12squared*SA22*dBetaMSBarUsual())/(MW*SB*SW*TB2) + (0.09375D0*EL*m12squared*SA1*SA2*SA3*dBetaMSBarUsual())/(MW*&
  &SB*SW*TB2) + (0.28125D0*CA22*EL*m12squared*SA1*SA2*SA3*dBetaMSBarUsual())/(MW*SB*SW*TB2) - (0.03125D0*CA3*EL*m12squared*SA1*TB&
  &2*dBetaMSBarUsual())/(CB*MW*SW) - (0.03125D0*CA22*CA3*EL*m12squared*SA1*TB2*dBetaMSBarUsual())/(CB*MW*SW) + (0.03125D0*CA3*EL*&
  &m12squared*SA1*SA22*TB2*dBetaMSBarUsual())/(CB*MW*SW) - (0.140625D0*CA1*EL*m12squared*SA2*SA3*TB2*dBetaMSBarUsual())/(CB*MW*SW&
  &) - (0.421875D0*CA1*CA22*EL*m12squared*SA2*SA3*TB2*dBetaMSBarUsual())/(CB*MW*SW) + (0.375D0*CA3*EL*MH12*dAlpha1MSBarUsual()*DB&
  &LE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.375D0*CA22*CA3*EL*MH12*dAlpha1MSBarUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.1875D0*CA&
  &3*EL*MH22*dAlpha1MSBarUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.1875D0*CA22*CA3*EL*MH22*dAlpha1MSBarUsual()*DBLE(CA1**INT(&
  &3.D0)))/(CB*MW*SW) - (0.375D0*CA3*EL*MH12*SA22*dAlpha1MSBarUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.1875D0*CA3*EL*MH22*SA&
  &22*dAlpha1MSBarUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.5625D0*CA3*EL*m12squared*dAlpha1MSBarUsual()*DBLE(CA1**INT(3.D0))&
  &)/(CB2*MW*SB*SW) - (0.5625D0*CA22*CA3*EL*m12squared*dAlpha1MSBarUsual()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) + (0.5625D0*CA3*E&
  &L*m12squared*SA22*dAlpha1MSBarUsual()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.1875D0*EL*MH12*SA2*SA3*dAlpha1MSBarUsual()*DBL&
  &E(CA1**INT(3.D0)))/(MW*SB*SW) - (0.5625D0*CA22*EL*MH12*SA2*SA3*dAlpha1MSBarUsual()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.09375&
  &D0*EL*MH22*SA2*SA3*dAlpha1MSBarUsual()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.28125D0*CA22*EL*MH22*SA2*SA3*dAlpha1MSBarUsual()*&
  &DBLE(CA1**INT(3.D0)))/(MW*SB*SW) + (0.28125D0*EL*m12squared*SA2*SA3*dAlpha1MSBarUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW) +&
  & (0.84375D0*CA22*EL*m12squared*SA2*SA3*dAlpha1MSBarUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.0625D0*CA2*EL*MH12*SA3*dA&
  &lpha2MSBarUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.03125D0*CA2*EL*MH22*SA3*dAlpha2MSBarUsual()*DBLE(CA1**INT(3.D0)))/(CB*&
  &MW*SW) - (0.5625D0*CA2*EL*MH12*SA22*SA3*dAlpha2MSBarUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.28125D0*CA2*EL*MH22*SA22*SA3&
  &*dAlpha2MSBarUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.5D0*CA2*CA3*EL*MH12*SA2*dAlpha2MSBarUsual()*DBLE(CA1**INT(3.D0)))/(&
  &MW*SB*SW) - (0.25D0*CA2*CA3*EL*MH22*SA2*dAlpha2MSBarUsual()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.09375D0*CA2*EL*m12squared*SA&
  &3*dAlpha2MSBarUsual()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) + (0.84375D0*CA2*EL*m12squared*SA22*SA3*dAlpha2MSBarUsual()*DBLE(CA&
  &1**INT(3.D0)))/(CB2*MW*SB*SW) + (0.75D0*CA2*CA3*EL*m12squared*SA2*dAlpha2MSBarUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW) + (&
  &0.0625D0*CA3*EL*MH12*SA2*dAlpha3MSBarUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.1875D0*CA22*CA3*EL*MH12*SA2*dAlpha3MSBarUsu&
  &al()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.03125D0*CA3*EL*MH22*SA2*dAlpha3MSBarUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.0&
  &9375D0*CA22*CA3*EL*MH22*SA2*dAlpha3MSBarUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.09375D0*CA3*EL*m12squared*SA2*dAlpha3MSB&
  &arUsual()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.28125D0*CA22*CA3*EL*m12squared*SA2*dAlpha3MSBarUsual()*DBLE(CA1**INT(3.D0)&
  &))/(CB2*MW*SB*SW) - (0.125D0*EL*MH12*SA3*dAlpha3MSBarUsual()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.125D0*CA22*EL*MH12*SA3*dAlp&
  &ha3MSBarUsual()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.0625D0*EL*MH22*SA3*dAlpha3MSBarUsual()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) &
  &- (0.0625D0*CA22*EL*MH22*SA3*dAlpha3MSBarUsual()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) + (0.125D0*EL*MH12*SA22*SA3*dAlpha3MSBarUsua&
  &l()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) + (0.0625D0*EL*MH22*SA22*SA3*dAlpha3MSBarUsual()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) + (0.18&
  &75D0*EL*m12squared*SA3*dAlpha3MSBarUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.1875D0*CA22*EL*m12squared*SA3*dAlpha3MSBa&
  &rUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.1875D0*EL*m12squared*SA22*SA3*dAlpha3MSBarUsual()*DBLE(CA1**INT(3.D0)))/(CB&
  &*MW*SB2*SW) - (0.1875D0*CA3*EL*m12squared*dBetaMSBarUsual()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.1875D0*CA22*CA3*EL*m12sq&
  &uared*dBetaMSBarUsual()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) + (0.1875D0*CA3*EL*m12squared*SA22*dBetaMSBarUsual()*DBLE(CA1**IN&
  &T(3.D0)))/(CB2*MW*SB*SW) - (0.03125D0*EL*MH12*SA2*SA3*dBetaMSBarUsual()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.09375D0*CA22*EL*&
  &MH12*SA2*SA3*dBetaMSBarUsual()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) + (0.03125D0*EL*MH12*SA2*SA3*dBetaMSBarUsual()*DBLE(CA1**INT(3&
  &.D0)))/(CB2*MW*SB*SW) + (0.09375D0*CA22*EL*MH12*SA2*SA3*dBetaMSBarUsual()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.015625D0*E&
  &L*MH22*SA2*SA3*dBetaMSBarUsual()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.046875D0*CA22*EL*MH22*SA2*SA3*dBetaMSBarUsual()*DBLE(CA&
  &1**INT(3.D0)))/(MW*SB*SW) + (0.015625D0*EL*MH22*SA2*SA3*dBetaMSBarUsual()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) + (0.046875D0*C&
  &A22*EL*MH22*SA2*SA3*dBetaMSBarUsual()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) + (0.12890625D0*EL*m12squared*SA2*SA3*dBetaMSBarUsu&
  &al()*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.38671875D0*CA22*EL*m12squared*SA2*SA3*dBetaMSBarUsual()*DBLE(CA1**INT(3.D0)))/(&
  &CB*MW*SB2*SW) - (0.125D0*CA3*EL*MH12*dBetaMSBarUsual()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW*TB) - (0.125D0*CA22*CA3*EL*MH12*dBetaMS&
  &BarUsual()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW*TB) - (0.0625D0*CA3*EL*MH22*dBetaMSBarUsual()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW*TB) -&
  & (0.0625D0*CA22*CA3*EL*MH22*dBetaMSBarUsual()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW*TB) + (0.125D0*CA3*EL*MH12*SA22*dBetaMSBarUsual(&
  &)*DBLE(CA1**INT(3.D0)))/(MW*SB*SW*TB) + (0.0625D0*CA3*EL*MH22*SA22*dBetaMSBarUsual()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW*TB) + (0.&
  &03125D0*EL*MH12*SA2*SA3*TB*dBetaMSBarUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.09375D0*CA22*EL*MH12*SA2*SA3*TB*dBetaMSBarU&
  &sual()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.015625D0*EL*MH22*SA2*SA3*TB*dBetaMSBarUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + &
  &(0.046875D0*CA22*EL*MH22*SA2*SA3*TB*dBetaMSBarUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.5625D0*CA1*EL*MH12*SA3*dAlpha2MSBa&
  &rUsual()*DBLE(CA2**INT(3.D0)))/(CB*MW*SW) + (0.28125D0*CA1*EL*MH22*SA3*dAlpha2MSBarUsual()*DBLE(CA2**INT(3.D0)))/(CB*MW*SW) + &
  &(0.84375D0*EL*m12squared*SA1*SA3*dAlpha2MSBarUsual()*DBLE(CA2**INT(3.D0)))/(CB*MW*SW) - (0.5625D0*CA1*EL*MH12*SA12*SA3*dAlpha2&
  &MSBarUsual()*DBLE(CA2**INT(3.D0)))/(CB*MW*SW) - (0.28125D0*CA1*EL*MH22*SA12*SA3*dAlpha2MSBarUsual()*DBLE(CA2**INT(3.D0)))/(CB*&
  &MW*SW) + (0.84375D0*CA1*EL*m12squared*SA3*dAlpha2MSBarUsual()*DBLE(CA2**INT(3.D0)))/(MW*SB*SW) - (0.5625D0*CA1*EL*m12squared*S&
  &A3*dAlpha2MSBarUsual()*DBLE(CA2**INT(3.D0)))/(CB2*MW*SB*SW) + (0.5625D0*EL*MH12*SA1*SA3*dAlpha2MSBarUsual()*DBLE(CA2**INT(3.D0&
  &)))/(MW*SB*SW) - (0.5625D0*CA12*EL*MH12*SA1*SA3*dAlpha2MSBarUsual()*DBLE(CA2**INT(3.D0)))/(MW*SB*SW) + (0.28125D0*EL*MH22*SA1*&
  &SA3*dAlpha2MSBarUsual()*DBLE(CA2**INT(3.D0)))/(MW*SB*SW) - (0.28125D0*CA12*EL*MH22*SA1*SA3*dAlpha2MSBarUsual()*DBLE(CA2**INT(3&
  &.D0)))/(MW*SB*SW) + (0.84375D0*CA1*EL*m12squared*SA12*SA3*dAlpha2MSBarUsual()*DBLE(CA2**INT(3.D0)))/(CB2*MW*SB*SW) - (0.5625D0&
  &*EL*m12squared*SA1*SA3*dAlpha2MSBarUsual()*DBLE(CA2**INT(3.D0)))/(CB*MW*SB2*SW) + (0.84375D0*CA12*EL*m12squared*SA1*SA3*dAlpha&
  &2MSBarUsual()*DBLE(CA2**INT(3.D0)))/(CB*MW*SB2*SW) - (0.28125D0*EL*m12squared*SA1*SA3*dAlpha2MSBarUsual()*DBLE(CA2**INT(3.D0))&
  &)/(MW*SB*SW*TB) - (0.28125D0*CA1*EL*m12squared*SA3*TB*dAlpha2MSBarUsual()*DBLE(CA2**INT(3.D0)))/(CB*MW*SW) + (0.5D0*CA3*MH12*d&
  &Alpha3MSBarUsual()*DBLE(CA2**INT(3.D0)))/vS + (0.25D0*CA3*MH22*dAlpha3MSBarUsual()*DBLE(CA2**INT(3.D0)))/vS + (0.1875D0*EL*MH1&
  &2*SA3*dAlpha2MSBarUsual()*DBLE(CA1**INT(3.D0))*DBLE(CA2**INT(3.D0)))/(CB*MW*SW) + (0.09375D0*EL*MH22*SA3*dAlpha2MSBarUsual()*D&
  &BLE(CA1**INT(3.D0))*DBLE(CA2**INT(3.D0)))/(CB*MW*SW) - (0.28125D0*EL*m12squared*SA3*dAlpha2MSBarUsual()*DBLE(CA1**INT(3.D0))*D&
  &BLE(CA2**INT(3.D0)))/(CB2*MW*SB*SW) - (0.28125D0*CA3*EL*m12squared*SA1*dBetaMSBarUsual()*DBLE(CB**INT(-3.D0)))/(MW*SW) - (0.84&
  &375D0*CA12*CA3*EL*m12squared*SA1*dBetaMSBarUsual()*DBLE(CB**INT(-3.D0)))/(MW*SW) - (0.28125D0*CA22*CA3*EL*m12squared*SA1*dBeta&
  &MSBarUsual()*DBLE(CB**INT(-3.D0)))/(MW*SW) - (0.84375D0*CA12*CA22*CA3*EL*m12squared*SA1*dBetaMSBarUsual()*DBLE(CB**INT(-3.D0))&
  &)/(MW*SW) + (0.28125D0*CA3*EL*m12squared*SA1*SA22*dBetaMSBarUsual()*DBLE(CB**INT(-3.D0)))/(MW*SW) + (0.84375D0*CA12*CA3*EL*m12&
  &squared*SA1*SA22*dBetaMSBarUsual()*DBLE(CB**INT(-3.D0)))/(MW*SW) - (0.36328125D0*CA1*EL*m12squared*SA2*SA3*dBetaMSBarUsual()*D&
  &BLE(CB**INT(-3.D0)))/(MW*SW) - (1.08984375D0*CA1*CA22*EL*m12squared*SA2*SA3*dBetaMSBarUsual()*DBLE(CB**INT(-3.D0)))/(MW*SW) + &
  &(0.45703125D0*CA1*EL*m12squared*SA12*SA2*SA3*dBetaMSBarUsual()*DBLE(CB**INT(-3.D0)))/(MW*SW) + (1.37109375D0*CA1*CA22*EL*m12sq&
  &uared*SA12*SA2*SA3*dBetaMSBarUsual()*DBLE(CB**INT(-3.D0)))/(MW*SW) - (0.0625D0*CA3*EL*m12squared*SA1*dBetaMSBarUsual()*DBLE(CB&
  &**INT(-3.D0)))/(MW*SB2*SW) - (0.28125D0*CA12*CA3*EL*m12squared*SA1*dBetaMSBarUsual()*DBLE(CB**INT(-3.D0)))/(MW*SB2*SW) - (0.06&
  &25D0*CA22*CA3*EL*m12squared*SA1*dBetaMSBarUsual()*DBLE(CB**INT(-3.D0)))/(MW*SB2*SW) - (0.28125D0*CA12*CA22*CA3*EL*m12squared*S&
  &A1*dBetaMSBarUsual()*DBLE(CB**INT(-3.D0)))/(MW*SB2*SW) + (0.0625D0*CA3*EL*m12squared*SA1*SA22*dBetaMSBarUsual()*DBLE(CB**INT(-&
  &3.D0)))/(MW*SB2*SW) + (0.28125D0*CA12*CA3*EL*m12squared*SA1*SA22*dBetaMSBarUsual()*DBLE(CB**INT(-3.D0)))/(MW*SB2*SW) - (0.0585&
  &9375D0*CA1*EL*m12squared*SA2*SA3*dBetaMSBarUsual()*DBLE(CB**INT(-3.D0)))/(MW*SB2*SW) - (0.17578125D0*CA1*CA22*EL*m12squared*SA&
  &2*SA3*dBetaMSBarUsual()*DBLE(CB**INT(-3.D0)))/(MW*SB2*SW) + (0.10546875D0*CA1*EL*m12squared*SA12*SA2*SA3*dBetaMSBarUsual()*DBL&
  &E(CB**INT(-3.D0)))/(MW*SB2*SW) + (0.31640625D0*CA1*CA22*EL*m12squared*SA12*SA2*SA3*dBetaMSBarUsual()*DBLE(CB**INT(-3.D0)))/(MW&
  &*SB2*SW) - (0.15234375D0*EL*m12squared*SA2*SA3*dBetaMSBarUsual()*DBLE(CA1**INT(3.D0))*DBLE(CB**INT(-3.D0)))/(MW*SW) - (0.45703&
  &125D0*CA22*EL*m12squared*SA2*SA3*dBetaMSBarUsual()*DBLE(CA1**INT(3.D0))*DBLE(CB**INT(-3.D0)))/(MW*SW) - (0.03515625D0*EL*m12sq&
  &uared*SA2*SA3*dBetaMSBarUsual()*DBLE(CA1**INT(3.D0))*DBLE(CB**INT(-3.D0)))/(MW*SB2*SW) - (0.10546875D0*CA22*EL*m12squared*SA2*&
  &SA3*dBetaMSBarUsual()*DBLE(CA1**INT(3.D0))*DBLE(CB**INT(-3.D0)))/(MW*SB2*SW) + (0.1875D0*EL*MH12*SA2*SA3*dAlpha1MSBarUsual()*D&
  &BLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.5625D0*CA22*EL*MH12*SA2*SA3*dAlpha1MSBarUsual()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.093&
  &75D0*EL*MH22*SA2*SA3*dAlpha1MSBarUsual()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.28125D0*CA22*EL*MH22*SA2*SA3*dAlpha1MSBarUsual(&
  &)*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.375D0*CA3*EL*MH12*dAlpha1MSBarUsual()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.375D0*CA22&
  &*CA3*EL*MH12*dAlpha1MSBarUsual()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.1875D0*CA3*EL*MH22*dAlpha1MSBarUsual()*DBLE(SA1**INT(3.&
  &D0)))/(MW*SB*SW) + (0.1875D0*CA22*CA3*EL*MH22*dAlpha1MSBarUsual()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0.375D0*CA3*EL*MH12*SA22&
  &*dAlpha1MSBarUsual()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0.1875D0*CA3*EL*MH22*SA22*dAlpha1MSBarUsual()*DBLE(SA1**INT(3.D0)))/(&
  &MW*SB*SW) - (0.28125D0*EL*m12squared*SA2*SA3*dAlpha1MSBarUsual()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.84375D0*CA22*EL*m12&
  &squared*SA2*SA3*dAlpha1MSBarUsual()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.5625D0*CA3*EL*m12squared*dAlpha1MSBarUsual()*DBL&
  &E(SA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.5625D0*CA22*CA3*EL*m12squared*dAlpha1MSBarUsual()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) &
  &+ (0.5625D0*CA3*EL*m12squared*SA22*dAlpha1MSBarUsual()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.5D0*CA2*CA3*EL*MH12*SA2*dAlph&
  &a2MSBarUsual()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.25D0*CA2*CA3*EL*MH22*SA2*dAlpha2MSBarUsual()*DBLE(SA1**INT(3.D0)))/(CB*MW&
  &*SW) - (0.75D0*CA2*CA3*EL*m12squared*SA2*dAlpha2MSBarUsual()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) + (0.0625D0*CA2*EL*MH12*SA3*&
  &dAlpha2MSBarUsual()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.03125D0*CA2*EL*MH22*SA3*dAlpha2MSBarUsual()*DBLE(SA1**INT(3.D0)))/(M&
  &W*SB*SW) - (0.5625D0*CA2*EL*MH12*SA22*SA3*dAlpha2MSBarUsual()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0.28125D0*CA2*EL*MH22*SA22*S&
  &A3*dAlpha2MSBarUsual()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0.09375D0*CA2*EL*m12squared*SA3*dAlpha2MSBarUsual()*DBLE(SA1**INT(3&
  &.D0)))/(CB*MW*SB2*SW) + (0.84375D0*CA2*EL*m12squared*SA22*SA3*dAlpha2MSBarUsual()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.12&
  &5D0*EL*MH12*SA3*dAlpha3MSBarUsual()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.125D0*CA22*EL*MH12*SA3*dAlpha3MSBarUsual()*DBLE(SA1*&
  &*INT(3.D0)))/(CB*MW*SW) + (0.0625D0*EL*MH22*SA3*dAlpha3MSBarUsual()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.0625D0*CA22*EL*MH22*&
  &SA3*dAlpha3MSBarUsual()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) - (0.125D0*EL*MH12*SA22*SA3*dAlpha3MSBarUsual()*DBLE(SA1**INT(3.D0)))&
  &/(CB*MW*SW) - (0.0625D0*EL*MH22*SA22*SA3*dAlpha3MSBarUsual()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.0625D0*CA3*EL*MH12*SA2*dAlp&
  &ha3MSBarUsual()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.1875D0*CA22*CA3*EL*MH12*SA2*dAlpha3MSBarUsual()*DBLE(SA1**INT(3.D0)))/(M&
  &W*SB*SW) + (0.03125D0*CA3*EL*MH22*SA2*dAlpha3MSBarUsual()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.09375D0*CA22*CA3*EL*MH22*SA2*d&
  &Alpha3MSBarUsual()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0.1875D0*EL*m12squared*SA3*dAlpha3MSBarUsual()*DBLE(SA1**INT(3.D0)))/(C&
  &B2*MW*SB*SW) - (0.1875D0*CA22*EL*m12squared*SA3*dAlpha3MSBarUsual()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) + (0.1875D0*EL*m12squ&
  &ared*SA22*SA3*dAlpha3MSBarUsual()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.09375D0*CA3*EL*m12squared*SA2*dAlpha3MSBarUsual()*&
  &DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.28125D0*CA22*CA3*EL*m12squared*SA2*dAlpha3MSBarUsual()*DBLE(SA1**INT(3.D0)))/(CB*MW*&
  &SB2*SW) + (0.0625D0*CA3*EL*MH12*dBetaMSBarUsual()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.0625D0*CA22*CA3*EL*MH12*dBetaMSBarUsua&
  &l()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0.0625D0*CA3*EL*MH12*dBetaMSBarUsual()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.0625D&
  &0*CA22*CA3*EL*MH12*dBetaMSBarUsual()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) + (0.03125D0*CA3*EL*MH22*dBetaMSBarUsual()*DBLE(SA1*&
  &*INT(3.D0)))/(MW*SB*SW) + (0.03125D0*CA22*CA3*EL*MH22*dBetaMSBarUsual()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0.03125D0*CA3*EL*M&
  &H22*dBetaMSBarUsual()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.03125D0*CA22*CA3*EL*MH22*dBetaMSBarUsual()*DBLE(SA1**INT(3.D0)&
  &))/(CB2*MW*SB*SW) - (0.0625D0*CA3*EL*MH12*SA22*dBetaMSBarUsual()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.0625D0*CA3*EL*MH12*SA22&
  &*dBetaMSBarUsual()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.03125D0*CA3*EL*MH22*SA22*dBetaMSBarUsual()*DBLE(SA1**INT(3.D0)))/&
  &(MW*SB*SW) + (0.03125D0*CA3*EL*MH22*SA22*dBetaMSBarUsual()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.09375D0*EL*m12squared*SA2&
  &*SA3*dBetaMSBarUsual()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.28125D0*CA22*EL*m12squared*SA2*SA3*dBetaMSBarUsual()*DBLE(SA1&
  &**INT(3.D0)))/(CB2*MW*SB*SW) - (0.28125D0*CA3*EL*m12squared*dBetaMSBarUsual()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.28125D&
  &0*CA22*CA3*EL*m12squared*dBetaMSBarUsual()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.28125D0*CA3*EL*m12squared*SA22*dBetaMSBar&
  &Usual()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.0625D0*EL*MH12*SA2*SA3*dBetaMSBarUsual()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW*TB)&
  & - (0.1875D0*CA22*EL*MH12*SA2*SA3*dBetaMSBarUsual()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW*TB) - (0.03125D0*EL*MH22*SA2*SA3*dBetaMSBa&
  &rUsual()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW*TB) - (0.09375D0*CA22*EL*MH22*SA2*SA3*dBetaMSBarUsual()*DBLE(SA1**INT(3.D0)))/(MW*SB*&
  &SW*TB) - (0.0625D0*CA3*EL*MH12*TB*dBetaMSBarUsual()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) - (0.0625D0*CA22*CA3*EL*MH12*TB*dBetaMSBa&
  &rUsual()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) - (0.03125D0*CA3*EL*MH22*TB*dBetaMSBarUsual()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) - (0.&
  &03125D0*CA22*CA3*EL*MH22*TB*dBetaMSBarUsual()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.0625D0*CA3*EL*MH12*SA22*TB*dBetaMSBarUsual&
  &()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.03125D0*CA3*EL*MH22*SA22*TB*dBetaMSBarUsual()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.1&
  &875D0*EL*MH12*SA3*dAlpha2MSBarUsual()*DBLE(CA2**INT(3.D0))*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.09375D0*EL*MH22*SA3*dAlpha2MS&
  &BarUsual()*DBLE(CA2**INT(3.D0))*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0.28125D0*EL*m12squared*SA3*dAlpha2MSBarUsual()*DBLE(CA2**&
  &INT(3.D0))*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.28125D0*CA3*EL*m12squared*dBetaMSBarUsual()*DBLE(CB**INT(-3.D0))*DBLE(SA1&
  &**INT(3.D0)))/(MW*SW) + (0.28125D0*CA22*CA3*EL*m12squared*dBetaMSBarUsual()*DBLE(CB**INT(-3.D0))*DBLE(SA1**INT(3.D0)))/(MW*SW)&
  & - (0.28125D0*CA3*EL*m12squared*SA22*dBetaMSBarUsual()*DBLE(CB**INT(-3.D0))*DBLE(SA1**INT(3.D0)))/(MW*SW) + (0.09375D0*CA3*EL*&
  &m12squared*dBetaMSBarUsual()*DBLE(CB**INT(-3.D0))*DBLE(SA1**INT(3.D0)))/(MW*SB2*SW) + (0.09375D0*CA22*CA3*EL*m12squared*dBetaM&
  &SBarUsual()*DBLE(CB**INT(-3.D0))*DBLE(SA1**INT(3.D0)))/(MW*SB2*SW) - (0.09375D0*CA3*EL*m12squared*SA22*dBetaMSBarUsual()*DBLE(&
  &CB**INT(-3.D0))*DBLE(SA1**INT(3.D0)))/(MW*SB2*SW) - (0.28125D0*CA1*EL*m12squared*SA3*dAlpha1MSBarUsual()*DBLE(SA2**INT(3.D0)))&
  &/(CB*MW*SW) + (0.1875D0*EL*MH12*SA1*SA3*dAlpha1MSBarUsual()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.5625D0*CA12*EL*MH12*SA1*SA3*&
  &dAlpha1MSBarUsual()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.09375D0*EL*MH22*SA1*SA3*dAlpha1MSBarUsual()*DBLE(SA2**INT(3.D0)))/(C&
  &B*MW*SW) + (0.28125D0*CA12*EL*MH22*SA1*SA3*dAlpha1MSBarUsual()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.1875D0*CA1*EL*MH12*SA3*dA&
  &lpha1MSBarUsual()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.09375D0*CA1*EL*MH22*SA3*dAlpha1MSBarUsual()*DBLE(SA2**INT(3.D0)))/(MW*&
  &SB*SW) + (0.28125D0*EL*m12squared*SA1*SA3*dAlpha1MSBarUsual()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.1875D0*EL*m12squared*SA1*S&
  &A3*dAlpha1MSBarUsual()*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) - (0.84375D0*CA12*EL*m12squared*SA1*SA3*dAlpha1MSBarUsual()*DBLE(S&
  &A2**INT(3.D0)))/(CB2*MW*SB*SW) - (0.5625D0*CA1*EL*MH12*SA12*SA3*dAlpha1MSBarUsual()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.2812&
  &5D0*CA1*EL*MH22*SA12*SA3*dAlpha1MSBarUsual()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) + (0.1875D0*CA1*EL*m12squared*SA3*dAlpha1MSBarUs&
  &ual()*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) + (0.84375D0*CA1*EL*m12squared*SA12*SA3*dAlpha1MSBarUsual()*DBLE(SA2**INT(3.D0)))/(&
  &CB*MW*SB2*SW) + (0.09375D0*CA1*EL*m12squared*SA3*dAlpha1MSBarUsual()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) - (0.09375D0*EL*m12sq&
  &uared*SA1*SA3*TB*dAlpha1MSBarUsual()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (1.5D0*MH12*SA3*dAlpha2MSBarUsual()*DBLE(SA2**INT(3.D0&
  &)))/vS + (0.75D0*MH22*SA3*dAlpha2MSBarUsual()*DBLE(SA2**INT(3.D0)))/vS - (0.1875D0*CA1*CA3*EL*MH12*dAlpha3MSBarUsual()*DBLE(SA&
  &2**INT(3.D0)))/(CB*MW*SW) - (0.09375D0*CA1*CA3*EL*MH22*dAlpha3MSBarUsual()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.28125D0*CA3*E&
  &L*m12squared*SA1*dAlpha3MSBarUsual()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.1875D0*CA1*CA3*EL*MH12*SA12*dAlpha3MSBarUsual()*DBL&
  &E(SA2**INT(3.D0)))/(CB*MW*SW) + (0.09375D0*CA1*CA3*EL*MH22*SA12*dAlpha3MSBarUsual()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.2812&
  &5D0*CA1*CA3*EL*m12squared*dAlpha3MSBarUsual()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) + (0.1875D0*CA1*CA3*EL*m12squared*dAlpha3MSBarU&
  &sual()*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) - (0.1875D0*CA3*EL*MH12*SA1*dAlpha3MSBarUsual()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) +&
  & (0.1875D0*CA12*CA3*EL*MH12*SA1*dAlpha3MSBarUsual()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.09375D0*CA3*EL*MH22*SA1*dAlpha3MSBar&
  &Usual()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) + (0.09375D0*CA12*CA3*EL*MH22*SA1*dAlpha3MSBarUsual()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW&
  &) - (0.28125D0*CA1*CA3*EL*m12squared*SA12*dAlpha3MSBarUsual()*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) + (0.1875D0*CA3*EL*m12squar&
  &ed*SA1*dAlpha3MSBarUsual()*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) - (0.28125D0*CA12*CA3*EL*m12squared*SA1*dAlpha3MSBarUsual()*DB&
  &LE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) + (0.09375D0*CA3*EL*m12squared*SA1*dAlpha3MSBarUsual()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) &
  &+ (0.09375D0*CA1*CA3*EL*m12squared*TB*dAlpha3MSBarUsual()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.09375D0*CA1*EL*m12squared*SA3*&
  &dBetaMSBarUsual()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.09375D0*CA1*EL*MH12*SA3*dBetaMSBarUsual()*DBLE(SA2**INT(3.D0)))/(MW*SB&
  &*SW) - (0.09375D0*CA1*EL*MH12*SA3*dBetaMSBarUsual()*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) + (0.046875D0*CA1*EL*MH22*SA3*dBetaMS&
  &BarUsual()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.046875D0*CA1*EL*MH22*SA3*dBetaMSBarUsual()*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*S&
  &W) + (0.140625D0*EL*m12squared*SA1*SA3*dBetaMSBarUsual()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) + (0.046875D0*EL*m12squared*SA1*SA3*&
  &dBetaMSBarUsual()*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) - (0.28125D0*CA12*EL*m12squared*SA1*SA3*dBetaMSBarUsual()*DBLE(SA2**INT&
  &(3.D0)))/(CB2*MW*SB*SW) - (0.09375D0*CA1*EL*MH12*SA12*SA3*dBetaMSBarUsual()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) + (0.09375D0*CA1*&
  &EL*MH12*SA12*SA3*dBetaMSBarUsual()*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) - (0.046875D0*CA1*EL*MH22*SA12*SA3*dBetaMSBarUsual()*D&
  &BLE(SA2**INT(3.D0)))/(MW*SB*SW) + (0.046875D0*CA1*EL*MH22*SA12*SA3*dBetaMSBarUsual()*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) - (0&
  &.10546875D0*CA1*EL*m12squared*SA3*dBetaMSBarUsual()*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) + (0.38671875D0*CA1*EL*m12squared*SA1&
  &2*SA3*dBetaMSBarUsual()*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) + (0.140625D0*CA1*EL*m12squared*SA3*dBetaMSBarUsual()*DBLE(SA2**I&
  &NT(3.D0)))/(MW*SB*SW*TB) + (0.1875D0*EL*MH12*SA1*SA3*dBetaMSBarUsual()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) - (0.1875D0*CA12*EL&
  &*MH12*SA1*SA3*dBetaMSBarUsual()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) + (0.09375D0*EL*MH22*SA1*SA3*dBetaMSBarUsual()*DBLE(SA2**I&
  &NT(3.D0)))/(MW*SB*SW*TB) - (0.09375D0*CA12*EL*MH22*SA1*SA3*dBetaMSBarUsual()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) - (0.09375D0*&
  &CA1*EL*MH12*SA3*TB*dBetaMSBarUsual()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.046875D0*CA1*EL*MH22*SA3*TB*dBetaMSBarUsual()*DBLE(&
  &SA2**INT(3.D0)))/(CB*MW*SW) - (0.140625D0*EL*m12squared*SA1*SA3*TB*dBetaMSBarUsual()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.093&
  &75D0*CA1*EL*MH12*SA12*SA3*TB*dBetaMSBarUsual()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.046875D0*CA1*EL*MH22*SA12*SA3*TB*dBetaMSB&
  &arUsual()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.09375D0*EL*m12squared*SA1*SA3*dBetaMSBarUsual()*DBLE(SA2**INT(3.D0)))/(MW*SB*S&
  &W*TB2) + (0.140625D0*CA1*EL*m12squared*SA3*TB2*dBetaMSBarUsual()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.1875D0*EL*MH12*SA3*dAlp&
  &ha1MSBarUsual()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) + (0.09375D0*EL*MH22*SA3*dAlpha1MSBarUsual()*DBLE(CA1**I&
  &NT(3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.28125D0*EL*m12squared*SA3*dAlpha1MSBarUsual()*DBLE(CA1**INT(3.D0))*DBLE(SA2**I&
  &NT(3.D0)))/(CB*MW*SB2*SW) - (0.0625D0*CA3*EL*MH12*dAlpha3MSBarUsual()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - &
  &(0.03125D0*CA3*EL*MH22*dAlpha3MSBarUsual()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.09375D0*CA3*EL*m12square&
  &d*dAlpha3MSBarUsual()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) + (0.03125D0*EL*MH12*SA3*dBetaMSBarUsual()*DBL&
  &E(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.03125D0*EL*MH12*SA3*dBetaMSBarUsual()*DBLE(CA1**INT(3.D0))*DBLE(SA2**I&
  &NT(3.D0)))/(CB2*MW*SB*SW) + (0.015625D0*EL*MH22*SA3*dBetaMSBarUsual()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - &
  &(0.015625D0*EL*MH22*SA3*dBetaMSBarUsual()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) - (0.12890625D0*EL*m12squa&
  &red*SA3*dBetaMSBarUsual()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) - (0.03125D0*EL*MH12*SA3*TB*dBetaMSBarUsua&
  &l()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.015625D0*EL*MH22*SA3*TB*dBetaMSBarUsual()*DBLE(CA1**INT(3.D0))*&
  &DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.36328125D0*CA1*EL*m12squared*SA3*dBetaMSBarUsual()*DBLE(CB**INT(-3.D0))*DBLE(SA2**INT(3.&
  &D0)))/(MW*SW) - (0.45703125D0*CA1*EL*m12squared*SA12*SA3*dBetaMSBarUsual()*DBLE(CB**INT(-3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SW) &
  &+ (0.05859375D0*CA1*EL*m12squared*SA3*dBetaMSBarUsual()*DBLE(CB**INT(-3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SB2*SW) - (0.10546875D0&
  &*CA1*EL*m12squared*SA12*SA3*dBetaMSBarUsual()*DBLE(CB**INT(-3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SB2*SW) + (0.15234375D0*EL*m12squ&
  &ared*SA3*dBetaMSBarUsual()*DBLE(CA1**INT(3.D0))*DBLE(CB**INT(-3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SW) + (0.03515625D0*EL*m12squar&
  &ed*SA3*dBetaMSBarUsual()*DBLE(CA1**INT(3.D0))*DBLE(CB**INT(-3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SB2*SW) - (0.1875D0*EL*MH12*SA3*d&
  &Alpha1MSBarUsual()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.09375D0*EL*MH22*SA3*dAlpha1MSBarUsual()*DBLE(SA1&
  &**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.28125D0*EL*m12squared*SA3*dAlpha1MSBarUsual()*DBLE(SA1**INT(3.D0))*DBLE(SA2&
  &**INT(3.D0)))/(CB2*MW*SB*SW) - (0.0625D0*CA3*EL*MH12*dAlpha3MSBarUsual()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SB*SW)&
  & - (0.03125D0*CA3*EL*MH22*dAlpha3MSBarUsual()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) + (0.09375D0*CA3*EL*m12squ&
  &ared*dAlpha3MSBarUsual()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) + (0.09375D0*EL*m12squared*SA3*dBetaMSBarUs&
  &ual()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) + (0.0625D0*EL*MH12*SA3*dBetaMSBarUsual()*DBLE(SA1**INT(3.D0))&
  &*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) + (0.03125D0*EL*MH22*SA3*dBetaMSBarUsual()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(MW&
  &*SB*SW*TB) - (0.1875D0*CA1*CA3*EL*m12squared*dBetaMSBarUsual()*DBLE(SB**INT(-3.D0)))/(MW*SW) - (0.1875D0*CA1*CA22*CA3*EL*m12sq&
  &uared*dBetaMSBarUsual()*DBLE(SB**INT(-3.D0)))/(MW*SW) - (1.125D0*CA1*CA3*EL*m12squared*SA12*dBetaMSBarUsual()*DBLE(SB**INT(-3.&
  &D0)))/(MW*SW) - (1.125D0*CA1*CA22*CA3*EL*m12squared*SA12*dBetaMSBarUsual()*DBLE(SB**INT(-3.D0)))/(MW*SW) + (0.1875D0*CA1*CA3*E&
  &L*m12squared*SA22*dBetaMSBarUsual()*DBLE(SB**INT(-3.D0)))/(MW*SW) + (1.125D0*CA1*CA3*EL*m12squared*SA12*SA22*dBetaMSBarUsual()&
  &*DBLE(SB**INT(-3.D0)))/(MW*SW) + (0.46875D0*EL*m12squared*SA1*SA2*SA3*dBetaMSBarUsual()*DBLE(SB**INT(-3.D0)))/(MW*SW) - (0.562&
  &5D0*CA12*EL*m12squared*SA1*SA2*SA3*dBetaMSBarUsual()*DBLE(SB**INT(-3.D0)))/(MW*SW) + (1.40625D0*CA22*EL*m12squared*SA1*SA2*SA3&
  &*dBetaMSBarUsual()*DBLE(SB**INT(-3.D0)))/(MW*SW) - (1.6875D0*CA12*CA22*EL*m12squared*SA1*SA2*SA3*dBetaMSBarUsual()*DBLE(SB**IN&
  &T(-3.D0)))/(MW*SW) + (0.375D0*CA3*EL*m12squared*dBetaMSBarUsual()*DBLE(CA1**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(MW*SW) + (0.375D&
  &0*CA22*CA3*EL*m12squared*dBetaMSBarUsual()*DBLE(CA1**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(MW*SW) - (0.375D0*CA3*EL*m12squared*SA2&
  &2*dBetaMSBarUsual()*DBLE(CA1**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(MW*SW) + (0.1875D0*EL*m12squared*SA2*SA3*dBetaMSBarUsual()*DBL&
  &E(SA1**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(MW*SW) + (0.5625D0*CA22*EL*m12squared*SA2*SA3*dBetaMSBarUsual()*DBLE(SA1**INT(3.D0))*&
  &DBLE(SB**INT(-3.D0)))/(MW*SW) - (0.46875D0*EL*m12squared*SA1*SA3*dBetaMSBarUsual()*DBLE(SA2**INT(3.D0))*DBLE(SB**INT(-3.D0)))/&
  &(MW*SW) + (0.5625D0*CA12*EL*m12squared*SA1*SA3*dBetaMSBarUsual()*DBLE(SA2**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(MW*SW) - (0.1875D&
  &0*EL*m12squared*SA3*dBetaMSBarUsual()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(MW*SW) - (0.125D0*CA1*C&
  &A3*m12squared*dgAtMZ())/(CB*MW) - (0.125D0*CA1*CA22*CA3*m12squared*dgAtMZ())/(CB*MW) + (0.125D0*CA3*MH12*SA1*dgAtMZ())/(CB*MW)&
  & + (0.375D0*CA12*CA3*MH12*SA1*dgAtMZ())/(CB*MW) + (0.125D0*CA22*CA3*MH12*SA1*dgAtMZ())/(CB*MW) + (0.375D0*CA12*CA22*CA3*MH12*S&
  &A1*dgAtMZ())/(CB*MW) + (0.0625D0*CA3*MH22*SA1*dgAtMZ())/(CB*MW) + (0.1875D0*CA12*CA3*MH22*SA1*dgAtMZ())/(CB*MW) + (0.0625D0*CA&
  &22*CA3*MH22*SA1*dgAtMZ())/(CB*MW) + (0.1875D0*CA12*CA22*CA3*MH22*SA1*dgAtMZ())/(CB*MW) + (0.125D0*CA1*CA3*m12squared*SA22*dgAt&
  &MZ())/(CB*MW) - (0.125D0*CA3*MH12*SA1*SA22*dgAtMZ())/(CB*MW) - (0.375D0*CA12*CA3*MH12*SA1*SA22*dgAtMZ())/(CB*MW) - (0.0625D0*C&
  &A3*MH22*SA1*SA22*dgAtMZ())/(CB*MW) - (0.1875D0*CA12*CA3*MH22*SA1*SA22*dgAtMZ())/(CB*MW) + (0.1875D0*CA1*MH12*SA2*SA3*dgAtMZ())&
  &/(CB*MW) + (0.5625D0*CA1*CA22*MH12*SA2*SA3*dgAtMZ())/(CB*MW) + (0.09375D0*CA1*MH22*SA2*SA3*dgAtMZ())/(CB*MW) + (0.28125D0*CA1*&
  &CA22*MH22*SA2*SA3*dgAtMZ())/(CB*MW) + (0.28125D0*m12squared*SA1*SA2*SA3*dgAtMZ())/(CB*MW) + (0.84375D0*CA22*m12squared*SA1*SA2&
  &*SA3*dgAtMZ())/(CB*MW) - (0.1875D0*CA1*MH12*SA12*SA2*SA3*dgAtMZ())/(CB*MW) - (0.5625D0*CA1*CA22*MH12*SA12*SA2*SA3*dgAtMZ())/(C&
  &B*MW) - (0.09375D0*CA1*MH22*SA12*SA2*SA3*dgAtMZ())/(CB*MW) - (0.28125D0*CA1*CA22*MH22*SA12*SA2*SA3*dgAtMZ())/(CB*MW) - (0.125D&
  &0*CA1*CA3*MH12*dgAtMZ())/(MW*SB) - (0.125D0*CA1*CA22*CA3*MH12*dgAtMZ())/(MW*SB) - (0.0625D0*CA1*CA3*MH22*dgAtMZ())/(MW*SB) - (&
  &0.0625D0*CA1*CA22*CA3*MH22*dgAtMZ())/(MW*SB) + (0.21875D0*CA3*m12squared*SA1*dgAtMZ())/(MW*SB) + (0.21875D0*CA22*CA3*m12square&
  &d*SA1*dgAtMZ())/(MW*SB) - (0.15625D0*CA3*m12squared*SA1*dgAtMZ())/(CB2*MW*SB) - (0.5625D0*CA12*CA3*m12squared*SA1*dgAtMZ())/(C&
  &B2*MW*SB) - (0.15625D0*CA22*CA3*m12squared*SA1*dgAtMZ())/(CB2*MW*SB) - (0.5625D0*CA12*CA22*CA3*m12squared*SA1*dgAtMZ())/(CB2*M&
  &W*SB) - (0.375D0*CA1*CA3*MH12*SA12*dgAtMZ())/(MW*SB) - (0.375D0*CA1*CA22*CA3*MH12*SA12*dgAtMZ())/(MW*SB) - (0.1875D0*CA1*CA3*M&
  &H22*SA12*dgAtMZ())/(MW*SB) - (0.1875D0*CA1*CA22*CA3*MH22*SA12*dgAtMZ())/(MW*SB) + (0.125D0*CA1*CA3*MH12*SA22*dgAtMZ())/(MW*SB)&
  & + (0.0625D0*CA1*CA3*MH22*SA22*dgAtMZ())/(MW*SB) - (0.21875D0*CA3*m12squared*SA1*SA22*dgAtMZ())/(MW*SB) + (0.15625D0*CA3*m12sq&
  &uared*SA1*SA22*dgAtMZ())/(CB2*MW*SB) + (0.5625D0*CA12*CA3*m12squared*SA1*SA22*dgAtMZ())/(CB2*MW*SB) + (0.375D0*CA1*CA3*MH12*SA&
  &12*SA22*dgAtMZ())/(MW*SB) + (0.1875D0*CA1*CA3*MH22*SA12*SA22*dgAtMZ())/(MW*SB) + (0.28125D0*CA1*m12squared*SA2*SA3*dgAtMZ())/(&
  &MW*SB) + (0.84375D0*CA1*CA22*m12squared*SA2*SA3*dgAtMZ())/(MW*SB) - (0.1875D0*CA1*m12squared*SA2*SA3*dgAtMZ())/(CB2*MW*SB) - (&
  &0.5625D0*CA1*CA22*m12squared*SA2*SA3*dgAtMZ())/(CB2*MW*SB) + (0.1875D0*MH12*SA1*SA2*SA3*dgAtMZ())/(MW*SB) - (0.1875D0*CA12*MH1&
  &2*SA1*SA2*SA3*dgAtMZ())/(MW*SB) + (0.5625D0*CA22*MH12*SA1*SA2*SA3*dgAtMZ())/(MW*SB) - (0.5625D0*CA12*CA22*MH12*SA1*SA2*SA3*dgA&
  &tMZ())/(MW*SB) + (0.09375D0*MH22*SA1*SA2*SA3*dgAtMZ())/(MW*SB) - (0.09375D0*CA12*MH22*SA1*SA2*SA3*dgAtMZ())/(MW*SB) + (0.28125&
  &D0*CA22*MH22*SA1*SA2*SA3*dgAtMZ())/(MW*SB) - (0.28125D0*CA12*CA22*MH22*SA1*SA2*SA3*dgAtMZ())/(MW*SB) + (0.28125D0*CA1*m12squar&
  &ed*SA12*SA2*SA3*dgAtMZ())/(CB2*MW*SB) + (0.84375D0*CA1*CA22*m12squared*SA12*SA2*SA3*dgAtMZ())/(CB2*MW*SB) + (0.0625D0*CA1*CA3*&
  &m12squared*dgAtMZ())/(CB*MW*SB2) + (0.0625D0*CA1*CA22*CA3*m12squared*dgAtMZ())/(CB*MW*SB2) + (0.5625D0*CA1*CA3*m12squared*SA12&
  &*dgAtMZ())/(CB*MW*SB2) + (0.5625D0*CA1*CA22*CA3*m12squared*SA12*dgAtMZ())/(CB*MW*SB2) - (0.0625D0*CA1*CA3*m12squared*SA22*dgAt&
  &MZ())/(CB*MW*SB2) - (0.5625D0*CA1*CA3*m12squared*SA12*SA22*dgAtMZ())/(CB*MW*SB2) - (0.1875D0*m12squared*SA1*SA2*SA3*dgAtMZ())/&
  &(CB*MW*SB2) + (0.28125D0*CA12*m12squared*SA1*SA2*SA3*dgAtMZ())/(CB*MW*SB2) - (0.5625D0*CA22*m12squared*SA1*SA2*SA3*dgAtMZ())/(&
  &CB*MW*SB2) + (0.84375D0*CA12*CA22*m12squared*SA1*SA2*SA3*dgAtMZ())/(CB*MW*SB2) + (0.125D0*CA1*CA3*m12squared*dgAtMZ())/(MW*SB*&
  &TB) + (0.125D0*CA1*CA22*CA3*m12squared*dgAtMZ())/(MW*SB*TB) - (0.125D0*CA1*CA3*m12squared*SA22*dgAtMZ())/(MW*SB*TB) - (0.09375&
  &D0*m12squared*SA1*SA2*SA3*dgAtMZ())/(MW*SB*TB) - (0.28125D0*CA22*m12squared*SA1*SA2*SA3*dgAtMZ())/(MW*SB*TB) - (0.03125D0*CA3*&
  &m12squared*SA1*TB*dgAtMZ())/(CB*MW) - (0.03125D0*CA22*CA3*m12squared*SA1*TB*dgAtMZ())/(CB*MW) + (0.03125D0*CA3*m12squared*SA1*&
  &SA22*TB*dgAtMZ())/(CB*MW) - (0.09375D0*CA1*m12squared*SA2*SA3*TB*dgAtMZ())/(CB*MW) - (0.28125D0*CA1*CA22*m12squared*SA2*SA3*TB&
  &*dgAtMZ())/(CB*MW) + (0.0625D0*MH12*SA2*SA3*DBLE(CA1**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.1875D0*CA22*MH12*SA2*SA3*DBLE(CA1**INT&
  &(3.D0))*dgAtMZ())/(CB*MW) + (0.03125D0*MH22*SA2*SA3*DBLE(CA1**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.09375D0*CA22*MH22*SA2*SA3*DBLE&
  &(CA1**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.125D0*CA3*MH12*DBLE(CA1**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.125D0*CA22*CA3*MH12*DBLE(CA&
  &1**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.0625D0*CA3*MH22*DBLE(CA1**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.0625D0*CA22*CA3*MH22*DBLE(CA1&
  &**INT(3.D0))*dgAtMZ())/(MW*SB) - (0.125D0*CA3*MH12*SA22*DBLE(CA1**INT(3.D0))*dgAtMZ())/(MW*SB) - (0.0625D0*CA3*MH22*SA22*DBLE(&
  &CA1**INT(3.D0))*dgAtMZ())/(MW*SB) - (0.09375D0*m12squared*SA2*SA3*DBLE(CA1**INT(3.D0))*dgAtMZ())/(CB2*MW*SB) - (0.28125D0*CA22&
  &*m12squared*SA2*SA3*DBLE(CA1**INT(3.D0))*dgAtMZ())/(CB2*MW*SB) - (0.1875D0*CA3*m12squared*DBLE(CA1**INT(3.D0))*dgAtMZ())/(CB*M&
  &W*SB2) - (0.1875D0*CA22*CA3*m12squared*DBLE(CA1**INT(3.D0))*dgAtMZ())/(CB*MW*SB2) + (0.1875D0*CA3*m12squared*SA22*DBLE(CA1**IN&
  &T(3.D0))*dgAtMZ())/(CB*MW*SB2) - (0.125D0*CA3*MH12*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB*MW) - (0.125D0*CA22*CA3*MH12*DBLE(SA1**I&
  &NT(3.D0))*dgAtMZ())/(CB*MW) - (0.0625D0*CA3*MH22*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB*MW) - (0.0625D0*CA22*CA3*MH22*DBLE(SA1**IN&
  &T(3.D0))*dgAtMZ())/(CB*MW) + (0.125D0*CA3*MH12*SA22*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.0625D0*CA3*MH22*SA22*DBLE(SA1*&
  &*INT(3.D0))*dgAtMZ())/(CB*MW) + (0.1875D0*CA3*m12squared*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB2*MW*SB) + (0.1875D0*CA22*CA3*m12sq&
  &uared*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB2*MW*SB) - (0.1875D0*CA3*m12squared*SA22*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB2*MW*SB) + &
  &(0.0625D0*MH12*SA2*SA3*DBLE(SA1**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.1875D0*CA22*MH12*SA2*SA3*DBLE(SA1**INT(3.D0))*dgAtMZ())/(MW&
  &*SB) + (0.03125D0*MH22*SA2*SA3*DBLE(SA1**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.09375D0*CA22*MH22*SA2*SA3*DBLE(SA1**INT(3.D0))*dgAt&
  &MZ())/(MW*SB) - (0.09375D0*m12squared*SA2*SA3*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB*MW*SB2) - (0.28125D0*CA22*m12squared*SA2*SA3*&
  &DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB*MW*SB2) - (0.1875D0*CA1*MH12*SA3*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW) - (0.09375D0*CA1*MH&
  &22*SA3*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW) - (0.28125D0*m12squared*SA1*SA3*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.1875&
  &D0*CA1*MH12*SA12*SA3*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.09375D0*CA1*MH22*SA12*SA3*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*&
  &MW) - (0.28125D0*CA1*m12squared*SA3*DBLE(SA2**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.1875D0*CA1*m12squared*SA3*DBLE(SA2**INT(3.D0))&
  &*dgAtMZ())/(CB2*MW*SB) - (0.1875D0*MH12*SA1*SA3*DBLE(SA2**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.1875D0*CA12*MH12*SA1*SA3*DBLE(SA2*&
  &*INT(3.D0))*dgAtMZ())/(MW*SB) - (0.09375D0*MH22*SA1*SA3*DBLE(SA2**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.09375D0*CA12*MH22*SA1*SA3*&
  &DBLE(SA2**INT(3.D0))*dgAtMZ())/(MW*SB) - (0.28125D0*CA1*m12squared*SA12*SA3*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB2*MW*SB) + (0.18&
  &75D0*m12squared*SA1*SA3*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW*SB2) - (0.28125D0*CA12*m12squared*SA1*SA3*DBLE(SA2**INT(3.D0))*d&
  &gAtMZ())/(CB*MW*SB2) + (0.09375D0*m12squared*SA1*SA3*DBLE(SA2**INT(3.D0))*dgAtMZ())/(MW*SB*TB) + (0.09375D0*CA1*m12squared*SA3&
  &*TB*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW) - (0.0625D0*MH12*SA3*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW) - &
  &(0.03125D0*MH22*SA3*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.09375D0*m12squared*SA3*DBLE(CA1**INT(3.D0&
  &))*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB2*MW*SB) - (0.0625D0*MH12*SA3*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dgAtMZ())/(MW*SB)&
  & - (0.03125D0*MH22*SA3*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.09375D0*m12squared*SA3*DBLE(SA1**INT(3&
  &.D0))*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW*SB2) - (0.125D0*CA1*CA3*EL*dm122MSBarUsual())/(CB*MW*SW) - (0.125D0*CA1*CA22*CA3*E&
  &L*dm122MSBarUsual())/(CB*MW*SW) + (0.125D0*CA1*CA3*EL*SA22*dm122MSBarUsual())/(CB*MW*SW) + (0.28125D0*EL*SA1*SA2*SA3*dm122MSBa&
  &rUsual())/(CB*MW*SW) + (0.84375D0*CA22*EL*SA1*SA2*SA3*dm122MSBarUsual())/(CB*MW*SW) + (0.21875D0*CA3*EL*SA1*dm122MSBarUsual())&
  &/(MW*SB*SW) + (0.21875D0*CA22*CA3*EL*SA1*dm122MSBarUsual())/(MW*SB*SW) - (0.15625D0*CA3*EL*SA1*dm122MSBarUsual())/(CB2*MW*SB*S&
  &W) - (0.5625D0*CA12*CA3*EL*SA1*dm122MSBarUsual())/(CB2*MW*SB*SW) - (0.15625D0*CA22*CA3*EL*SA1*dm122MSBarUsual())/(CB2*MW*SB*SW&
  &) - (0.5625D0*CA12*CA22*CA3*EL*SA1*dm122MSBarUsual())/(CB2*MW*SB*SW) - (0.21875D0*CA3*EL*SA1*SA22*dm122MSBarUsual())/(MW*SB*SW&
  &) + (0.15625D0*CA3*EL*SA1*SA22*dm122MSBarUsual())/(CB2*MW*SB*SW) + (0.5625D0*CA12*CA3*EL*SA1*SA22*dm122MSBarUsual())/(CB2*MW*S&
  &B*SW) + (0.28125D0*CA1*EL*SA2*SA3*dm122MSBarUsual())/(MW*SB*SW) + (0.84375D0*CA1*CA22*EL*SA2*SA3*dm122MSBarUsual())/(MW*SB*SW)&
  & - (0.1875D0*CA1*EL*SA2*SA3*dm122MSBarUsual())/(CB2*MW*SB*SW) - (0.5625D0*CA1*CA22*EL*SA2*SA3*dm122MSBarUsual())/(CB2*MW*SB*SW&
  &) + (0.28125D0*CA1*EL*SA12*SA2*SA3*dm122MSBarUsual())/(CB2*MW*SB*SW) + (0.84375D0*CA1*CA22*EL*SA12*SA2*SA3*dm122MSBarUsual())/&
  &(CB2*MW*SB*SW) + (0.0625D0*CA1*CA3*EL*dm122MSBarUsual())/(CB*MW*SB2*SW) + (0.0625D0*CA1*CA22*CA3*EL*dm122MSBarUsual())/(CB*MW*&
  &SB2*SW) + (0.5625D0*CA1*CA3*EL*SA12*dm122MSBarUsual())/(CB*MW*SB2*SW) + (0.5625D0*CA1*CA22*CA3*EL*SA12*dm122MSBarUsual())/(CB*&
  &MW*SB2*SW) - (0.0625D0*CA1*CA3*EL*SA22*dm122MSBarUsual())/(CB*MW*SB2*SW) - (0.5625D0*CA1*CA3*EL*SA12*SA22*dm122MSBarUsual())/(&
  &CB*MW*SB2*SW) - (0.1875D0*EL*SA1*SA2*SA3*dm122MSBarUsual())/(CB*MW*SB2*SW) + (0.28125D0*CA12*EL*SA1*SA2*SA3*dm122MSBarUsual())&
  &/(CB*MW*SB2*SW) - (0.5625D0*CA22*EL*SA1*SA2*SA3*dm122MSBarUsual())/(CB*MW*SB2*SW) + (0.84375D0*CA12*CA22*EL*SA1*SA2*SA3*dm122M&
  &SBarUsual())/(CB*MW*SB2*SW) + (0.125D0*CA1*CA3*EL*dm122MSBarUsual())/(MW*SB*SW*TB) + (0.125D0*CA1*CA22*CA3*EL*dm122MSBarUsual(&
  &))/(MW*SB*SW*TB) - (0.125D0*CA1*CA3*EL*SA22*dm122MSBarUsual())/(MW*SB*SW*TB) - (0.09375D0*EL*SA1*SA2*SA3*dm122MSBarUsual())/(M&
  &W*SB*SW*TB) - (0.28125D0*CA22*EL*SA1*SA2*SA3*dm122MSBarUsual())/(MW*SB*SW*TB) - (0.03125D0*CA3*EL*SA1*TB*dm122MSBarUsual())/(C&
  &B*MW*SW) - (0.03125D0*CA22*CA3*EL*SA1*TB*dm122MSBarUsual())/(CB*MW*SW) + (0.03125D0*CA3*EL*SA1*SA22*TB*dm122MSBarUsual())/(CB*&
  &MW*SW) - (0.09375D0*CA1*EL*SA2*SA3*TB*dm122MSBarUsual())/(CB*MW*SW) - (0.28125D0*CA1*CA22*EL*SA2*SA3*TB*dm122MSBarUsual())/(CB&
  &*MW*SW) - (0.09375D0*EL*SA2*SA3*DBLE(CA1**INT(3.D0))*dm122MSBarUsual())/(CB2*MW*SB*SW) - (0.28125D0*CA22*EL*SA2*SA3*DBLE(CA1**&
  &INT(3.D0))*dm122MSBarUsual())/(CB2*MW*SB*SW) - (0.1875D0*CA3*EL*DBLE(CA1**INT(3.D0))*dm122MSBarUsual())/(CB*MW*SB2*SW) - (0.18&
  &75D0*CA22*CA3*EL*DBLE(CA1**INT(3.D0))*dm122MSBarUsual())/(CB*MW*SB2*SW) + (0.1875D0*CA3*EL*SA22*DBLE(CA1**INT(3.D0))*dm122MSBa&
  &rUsual())/(CB*MW*SB2*SW) + (0.1875D0*CA3*EL*DBLE(SA1**INT(3.D0))*dm122MSBarUsual())/(CB2*MW*SB*SW) + (0.1875D0*CA22*CA3*EL*DBL&
  &E(SA1**INT(3.D0))*dm122MSBarUsual())/(CB2*MW*SB*SW) - (0.1875D0*CA3*EL*SA22*DBLE(SA1**INT(3.D0))*dm122MSBarUsual())/(CB2*MW*SB&
  &*SW) - (0.09375D0*EL*SA2*SA3*DBLE(SA1**INT(3.D0))*dm122MSBarUsual())/(CB*MW*SB2*SW) - (0.28125D0*CA22*EL*SA2*SA3*DBLE(SA1**INT&
  &(3.D0))*dm122MSBarUsual())/(CB*MW*SB2*SW) - (0.28125D0*EL*SA1*SA3*DBLE(SA2**INT(3.D0))*dm122MSBarUsual())/(CB*MW*SW) - (0.2812&
  &5D0*CA1*EL*SA3*DBLE(SA2**INT(3.D0))*dm122MSBarUsual())/(MW*SB*SW) + (0.1875D0*CA1*EL*SA3*DBLE(SA2**INT(3.D0))*dm122MSBarUsual(&
  &))/(CB2*MW*SB*SW) - (0.28125D0*CA1*EL*SA12*SA3*DBLE(SA2**INT(3.D0))*dm122MSBarUsual())/(CB2*MW*SB*SW) + (0.1875D0*EL*SA1*SA3*D&
  &BLE(SA2**INT(3.D0))*dm122MSBarUsual())/(CB*MW*SB2*SW) - (0.28125D0*CA12*EL*SA1*SA3*DBLE(SA2**INT(3.D0))*dm122MSBarUsual())/(CB&
  &*MW*SB2*SW) + (0.09375D0*EL*SA1*SA3*DBLE(SA2**INT(3.D0))*dm122MSBarUsual())/(MW*SB*SW*TB) + (0.09375D0*CA1*EL*SA3*TB*DBLE(SA2*&
  &*INT(3.D0))*dm122MSBarUsual())/(CB*MW*SW) + (0.09375D0*EL*SA3*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dm122MSBarUsual())/(CB&
  &2*MW*SB*SW) + (0.09375D0*EL*SA3*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dm122MSBarUsual())/(CB*MW*SB2*SW) + (0.125D0*CA3*EL*&
  &SA1*dMH12OSUsual())/(CB*MW*SW) + (0.375D0*CA12*CA3*EL*SA1*dMH12OSUsual())/(CB*MW*SW) + (0.125D0*CA22*CA3*EL*SA1*dMH12OSUsual()&
  &)/(CB*MW*SW) + (0.375D0*CA12*CA22*CA3*EL*SA1*dMH12OSUsual())/(CB*MW*SW) - (0.125D0*CA3*EL*SA1*SA22*dMH12OSUsual())/(CB*MW*SW) &
  &- (0.375D0*CA12*CA3*EL*SA1*SA22*dMH12OSUsual())/(CB*MW*SW) + (0.1875D0*CA1*EL*SA2*SA3*dMH12OSUsual())/(CB*MW*SW) + (0.5625D0*C&
  &A1*CA22*EL*SA2*SA3*dMH12OSUsual())/(CB*MW*SW) - (0.1875D0*CA1*EL*SA12*SA2*SA3*dMH12OSUsual())/(CB*MW*SW) - (0.5625D0*CA1*CA22*&
  &EL*SA12*SA2*SA3*dMH12OSUsual())/(CB*MW*SW) - (0.125D0*CA1*CA3*EL*dMH12OSUsual())/(MW*SB*SW) - (0.125D0*CA1*CA22*CA3*EL*dMH12OS&
  &Usual())/(MW*SB*SW) - (0.375D0*CA1*CA3*EL*SA12*dMH12OSUsual())/(MW*SB*SW) - (0.375D0*CA1*CA22*CA3*EL*SA12*dMH12OSUsual())/(MW*&
  &SB*SW) + (0.125D0*CA1*CA3*EL*SA22*dMH12OSUsual())/(MW*SB*SW) + (0.375D0*CA1*CA3*EL*SA12*SA22*dMH12OSUsual())/(MW*SB*SW) + (0.1&
  &875D0*EL*SA1*SA2*SA3*dMH12OSUsual())/(MW*SB*SW) - (0.1875D0*CA12*EL*SA1*SA2*SA3*dMH12OSUsual())/(MW*SB*SW) + (0.5625D0*CA22*EL&
  &*SA1*SA2*SA3*dMH12OSUsual())/(MW*SB*SW) - (0.5625D0*CA12*CA22*EL*SA1*SA2*SA3*dMH12OSUsual())/(MW*SB*SW) - (0.5D0*CA2*SA3*dMH12&
  &OSUsual())/vS - (1.5D0*CA2*SA22*SA3*dMH12OSUsual())/vS + (0.0625D0*EL*SA2*SA3*DBLE(CA1**INT(3.D0))*dMH12OSUsual())/(CB*MW*SW) &
  &+ (0.1875D0*CA22*EL*SA2*SA3*DBLE(CA1**INT(3.D0))*dMH12OSUsual())/(CB*MW*SW) + (0.125D0*CA3*EL*DBLE(CA1**INT(3.D0))*dMH12OSUsua&
  &l())/(MW*SB*SW) + (0.125D0*CA22*CA3*EL*DBLE(CA1**INT(3.D0))*dMH12OSUsual())/(MW*SB*SW) - (0.125D0*CA3*EL*SA22*DBLE(CA1**INT(3.&
  &D0))*dMH12OSUsual())/(MW*SB*SW) + (0.5D0*SA3*DBLE(CA2**INT(3.D0))*dMH12OSUsual())/vS - (0.125D0*CA3*EL*DBLE(SA1**INT(3.D0))*dM&
  &H12OSUsual())/(CB*MW*SW) - (0.125D0*CA22*CA3*EL*DBLE(SA1**INT(3.D0))*dMH12OSUsual())/(CB*MW*SW) + (0.125D0*CA3*EL*SA22*DBLE(SA&
  &1**INT(3.D0))*dMH12OSUsual())/(CB*MW*SW) + (0.0625D0*EL*SA2*SA3*DBLE(SA1**INT(3.D0))*dMH12OSUsual())/(MW*SB*SW) + (0.1875D0*CA&
  &22*EL*SA2*SA3*DBLE(SA1**INT(3.D0))*dMH12OSUsual())/(MW*SB*SW) - (0.1875D0*CA1*EL*SA3*DBLE(SA2**INT(3.D0))*dMH12OSUsual())/(CB*&
  &MW*SW) + (0.1875D0*CA1*EL*SA12*SA3*DBLE(SA2**INT(3.D0))*dMH12OSUsual())/(CB*MW*SW) - (0.1875D0*EL*SA1*SA3*DBLE(SA2**INT(3.D0))&
  &*dMH12OSUsual())/(MW*SB*SW) + (0.1875D0*CA12*EL*SA1*SA3*DBLE(SA2**INT(3.D0))*dMH12OSUsual())/(MW*SB*SW) - (0.0625D0*EL*SA3*DBL&
  &E(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dMH12OSUsual())/(CB*MW*SW) - (0.0625D0*EL*SA3*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))&
  &*dMH12OSUsual())/(MW*SB*SW) + (0.0625D0*CA3*EL*SA1*dMH22OSUsual())/(CB*MW*SW) + (0.1875D0*CA12*CA3*EL*SA1*dMH22OSUsual())/(CB*&
  &MW*SW) + (0.0625D0*CA22*CA3*EL*SA1*dMH22OSUsual())/(CB*MW*SW) + (0.1875D0*CA12*CA22*CA3*EL*SA1*dMH22OSUsual())/(CB*MW*SW) - (0&
  &.0625D0*CA3*EL*SA1*SA22*dMH22OSUsual())/(CB*MW*SW) - (0.1875D0*CA12*CA3*EL*SA1*SA22*dMH22OSUsual())/(CB*MW*SW) + (0.09375D0*CA&
  &1*EL*SA2*SA3*dMH22OSUsual())/(CB*MW*SW) + (0.28125D0*CA1*CA22*EL*SA2*SA3*dMH22OSUsual())/(CB*MW*SW) - (0.09375D0*CA1*EL*SA12*S&
  &A2*SA3*dMH22OSUsual())/(CB*MW*SW) - (0.28125D0*CA1*CA22*EL*SA12*SA2*SA3*dMH22OSUsual())/(CB*MW*SW) - (0.0625D0*CA1*CA3*EL*dMH2&
  &2OSUsual())/(MW*SB*SW) - (0.0625D0*CA1*CA22*CA3*EL*dMH22OSUsual())/(MW*SB*SW) - (0.1875D0*CA1*CA3*EL*SA12*dMH22OSUsual())/(MW*&
  &SB*SW) - (0.1875D0*CA1*CA22*CA3*EL*SA12*dMH22OSUsual())/(MW*SB*SW) + (0.0625D0*CA1*CA3*EL*SA22*dMH22OSUsual())/(MW*SB*SW) + (0&
  &.1875D0*CA1*CA3*EL*SA12*SA22*dMH22OSUsual())/(MW*SB*SW) + (0.09375D0*EL*SA1*SA2*SA3*dMH22OSUsual())/(MW*SB*SW) - (0.09375D0*CA&
  &12*EL*SA1*SA2*SA3*dMH22OSUsual())/(MW*SB*SW) + (0.28125D0*CA22*EL*SA1*SA2*SA3*dMH22OSUsual())/(MW*SB*SW) - (0.28125D0*CA12*CA2&
  &2*EL*SA1*SA2*SA3*dMH22OSUsual())/(MW*SB*SW) - (0.25D0*CA2*SA3*dMH22OSUsual())/vS - (0.75D0*CA2*SA22*SA3*dMH22OSUsual())/vS + (&
  &0.03125D0*EL*SA2*SA3*DBLE(CA1**INT(3.D0))*dMH22OSUsual())/(CB*MW*SW) + (0.09375D0*CA22*EL*SA2*SA3*DBLE(CA1**INT(3.D0))*dMH22OS&
  &Usual())/(CB*MW*SW) + (0.0625D0*CA3*EL*DBLE(CA1**INT(3.D0))*dMH22OSUsual())/(MW*SB*SW) + (0.0625D0*CA22*CA3*EL*DBLE(CA1**INT(3&
  &.D0))*dMH22OSUsual())/(MW*SB*SW) - (0.0625D0*CA3*EL*SA22*DBLE(CA1**INT(3.D0))*dMH22OSUsual())/(MW*SB*SW) + (0.25D0*SA3*DBLE(CA&
  &2**INT(3.D0))*dMH22OSUsual())/vS - (0.0625D0*CA3*EL*DBLE(SA1**INT(3.D0))*dMH22OSUsual())/(CB*MW*SW) - (0.0625D0*CA22*CA3*EL*DB&
  &LE(SA1**INT(3.D0))*dMH22OSUsual())/(CB*MW*SW) + (0.0625D0*CA3*EL*SA22*DBLE(SA1**INT(3.D0))*dMH22OSUsual())/(CB*MW*SW) + (0.031&
  &25D0*EL*SA2*SA3*DBLE(SA1**INT(3.D0))*dMH22OSUsual())/(MW*SB*SW) + (0.09375D0*CA22*EL*SA2*SA3*DBLE(SA1**INT(3.D0))*dMH22OSUsual&
  &())/(MW*SB*SW) - (0.09375D0*CA1*EL*SA3*DBLE(SA2**INT(3.D0))*dMH22OSUsual())/(CB*MW*SW) + (0.09375D0*CA1*EL*SA12*SA3*DBLE(SA2**&
  &INT(3.D0))*dMH22OSUsual())/(CB*MW*SW) - (0.09375D0*EL*SA1*SA3*DBLE(SA2**INT(3.D0))*dMH22OSUsual())/(MW*SB*SW) + (0.09375D0*CA1&
  &2*EL*SA1*SA3*DBLE(SA2**INT(3.D0))*dMH22OSUsual())/(MW*SB*SW) - (0.03125D0*EL*SA3*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dMH&
  &22OSUsual())/(CB*MW*SW) - (0.03125D0*EL*SA3*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dMH22OSUsual())/(MW*SB*SW) + (0.0625D0*C&
  &A1*CA3*EL*m12squared*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SW) + (0.0625D0*CA1*CA22*CA3*EL*m12squared*DBLE(MW**INT(-3.D0))*dMW&
  &2Usual())/(CB*SW) - (0.0625D0*CA3*EL*MH12*SA1*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SW) - (0.1875D0*CA12*CA3*EL*MH12*SA1*DBLE(&
  &MW**INT(-3.D0))*dMW2Usual())/(CB*SW) - (0.0625D0*CA22*CA3*EL*MH12*SA1*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SW) - (0.1875D0*CA&
  &12*CA22*CA3*EL*MH12*SA1*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SW) - (0.03125D0*CA3*EL*MH22*SA1*DBLE(MW**INT(-3.D0))*dMW2Usual(&
  &))/(CB*SW) - (0.09375D0*CA12*CA3*EL*MH22*SA1*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SW) - (0.03125D0*CA22*CA3*EL*MH22*SA1*DBLE(&
  &MW**INT(-3.D0))*dMW2Usual())/(CB*SW) - (0.09375D0*CA12*CA22*CA3*EL*MH22*SA1*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SW) - (0.062&
  &5D0*CA1*CA3*EL*m12squared*SA22*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SW) + (0.0625D0*CA3*EL*MH12*SA1*SA22*DBLE(MW**INT(-3.D0))&
  &*dMW2Usual())/(CB*SW) + (0.1875D0*CA12*CA3*EL*MH12*SA1*SA22*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SW) + (0.03125D0*CA3*EL*MH22&
  &*SA1*SA22*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SW) + (0.09375D0*CA12*CA3*EL*MH22*SA1*SA22*DBLE(MW**INT(-3.D0))*dMW2Usual())/(&
  &CB*SW) - (0.09375D0*CA1*EL*MH12*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SW) - (0.28125D0*CA1*CA22*EL*MH12*SA2*SA3*DBLE(M&
  &W**INT(-3.D0))*dMW2Usual())/(CB*SW) - (0.046875D0*CA1*EL*MH22*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SW) - (0.140625D0*&
  &CA1*CA22*EL*MH22*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SW) - (0.140625D0*EL*m12squared*SA1*SA2*SA3*DBLE(MW**INT(-3.D0)&
  &)*dMW2Usual())/(CB*SW) - (0.421875D0*CA22*EL*m12squared*SA1*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SW) + (0.09375D0*CA1&
  &*EL*MH12*SA12*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SW) + (0.28125D0*CA1*CA22*EL*MH12*SA12*SA2*SA3*DBLE(MW**INT(-3.D0)&
  &)*dMW2Usual())/(CB*SW) + (0.046875D0*CA1*EL*MH22*SA12*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SW) + (0.140625D0*CA1*CA22&
  &*EL*MH22*SA12*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SW) + (0.0625D0*CA1*CA3*EL*MH12*DBLE(MW**INT(-3.D0))*dMW2Usual())/&
  &(SB*SW) + (0.0625D0*CA1*CA22*CA3*EL*MH12*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW) + (0.03125D0*CA1*CA3*EL*MH22*DBLE(MW**INT(-&
  &3.D0))*dMW2Usual())/(SB*SW) + (0.03125D0*CA1*CA22*CA3*EL*MH22*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW) - (0.109375D0*CA3*EL*m&
  &12squared*SA1*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW) - (0.109375D0*CA22*CA3*EL*m12squared*SA1*DBLE(MW**INT(-3.D0))*dMW2Usua&
  &l())/(SB*SW) + (0.078125D0*CA3*EL*m12squared*SA1*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB2*SB*SW) + (0.28125D0*CA12*CA3*EL*m12squ&
  &ared*SA1*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB2*SB*SW) + (0.078125D0*CA22*CA3*EL*m12squared*SA1*DBLE(MW**INT(-3.D0))*dMW2Usual&
  &())/(CB2*SB*SW) + (0.28125D0*CA12*CA22*CA3*EL*m12squared*SA1*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB2*SB*SW) + (0.1875D0*CA1*CA3&
  &*EL*MH12*SA12*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW) + (0.1875D0*CA1*CA22*CA3*EL*MH12*SA12*DBLE(MW**INT(-3.D0))*dMW2Usual()&
  &)/(SB*SW) + (0.09375D0*CA1*CA3*EL*MH22*SA12*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW) + (0.09375D0*CA1*CA22*CA3*EL*MH22*SA12*D&
  &BLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW) - (0.0625D0*CA1*CA3*EL*MH12*SA22*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW) - (0.03125&
  &D0*CA1*CA3*EL*MH22*SA22*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW) + (0.109375D0*CA3*EL*m12squared*SA1*SA22*DBLE(MW**INT(-3.D0)&
  &)*dMW2Usual())/(SB*SW) - (0.078125D0*CA3*EL*m12squared*SA1*SA22*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB2*SB*SW) - (0.28125D0*CA1&
  &2*CA3*EL*m12squared*SA1*SA22*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB2*SB*SW) - (0.1875D0*CA1*CA3*EL*MH12*SA12*SA22*DBLE(MW**INT(&
  &-3.D0))*dMW2Usual())/(SB*SW) - (0.09375D0*CA1*CA3*EL*MH22*SA12*SA22*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW) - (0.140625D0*CA&
  &1*EL*m12squared*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW) - (0.421875D0*CA1*CA22*EL*m12squared*SA2*SA3*DBLE(MW**INT(-3&
  &.D0))*dMW2Usual())/(SB*SW) + (0.09375D0*CA1*EL*m12squared*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB2*SB*SW) + (0.28125D0*C&
  &A1*CA22*EL*m12squared*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB2*SB*SW) - (0.09375D0*EL*MH12*SA1*SA2*SA3*DBLE(MW**INT(-3.D&
  &0))*dMW2Usual())/(SB*SW) + (0.09375D0*CA12*EL*MH12*SA1*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW) - (0.28125D0*CA22*EL*&
  &MH12*SA1*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW) + (0.28125D0*CA12*CA22*EL*MH12*SA1*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW&
  &2Usual())/(SB*SW) - (0.046875D0*EL*MH22*SA1*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW) + (0.046875D0*CA12*EL*MH22*SA1*S&
  &A2*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW) - (0.140625D0*CA22*EL*MH22*SA1*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*&
  &SW) + (0.140625D0*CA12*CA22*EL*MH22*SA1*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW) - (0.140625D0*CA1*EL*m12squared*SA12&
  &*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB2*SB*SW) - (0.421875D0*CA1*CA22*EL*m12squared*SA12*SA2*SA3*DBLE(MW**INT(-3.D0))*&
  &dMW2Usual())/(CB2*SB*SW) - (0.03125D0*CA1*CA3*EL*m12squared*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SB2*SW) - (0.03125D0*CA1*CA2&
  &2*CA3*EL*m12squared*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SB2*SW) - (0.28125D0*CA1*CA3*EL*m12squared*SA12*DBLE(MW**INT(-3.D0))&
  &*dMW2Usual())/(CB*SB2*SW) - (0.28125D0*CA1*CA22*CA3*EL*m12squared*SA12*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SB2*SW) + (0.0312&
  &5D0*CA1*CA3*EL*m12squared*SA22*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SB2*SW) + (0.28125D0*CA1*CA3*EL*m12squared*SA12*SA22*DBLE&
  &(MW**INT(-3.D0))*dMW2Usual())/(CB*SB2*SW) + (0.09375D0*EL*m12squared*SA1*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SB2*SW)&
  & - (0.140625D0*CA12*EL*m12squared*SA1*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SB2*SW) + (0.28125D0*CA22*EL*m12squared*SA&
  &1*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SB2*SW) - (0.421875D0*CA12*CA22*EL*m12squared*SA1*SA2*SA3*DBLE(MW**INT(-3.D0))&
  &*dMW2Usual())/(CB*SB2*SW) - (0.0625D0*CA1*CA3*EL*m12squared*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW*TB) - (0.0625D0*CA1*CA22*&
  &CA3*EL*m12squared*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW*TB) + (0.0625D0*CA1*CA3*EL*m12squared*SA22*DBLE(MW**INT(-3.D0))*dMW&
  &2Usual())/(SB*SW*TB) + (0.046875D0*EL*m12squared*SA1*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW*TB) + (0.140625D0*CA22*E&
  &L*m12squared*SA1*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW*TB) + (0.015625D0*CA3*EL*m12squared*SA1*TB*DBLE(MW**INT(-3.D&
  &0))*dMW2Usual())/(CB*SW) + (0.015625D0*CA22*CA3*EL*m12squared*SA1*TB*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SW) - (0.015625D0*C&
  &A3*EL*m12squared*SA1*SA22*TB*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SW) + (0.046875D0*CA1*EL*m12squared*SA2*SA3*TB*DBLE(MW**INT&
  &(-3.D0))*dMW2Usual())/(CB*SW) + (0.140625D0*CA1*CA22*EL*m12squared*SA2*SA3*TB*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SW) - (0.0&
  &3125D0*EL*MH12*SA2*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SW) - (0.09375D0*CA22*EL*MH12*SA2*SA3*DBLE(C&
  &A1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SW) - (0.015625D0*EL*MH22*SA2*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0&
  &))*dMW2Usual())/(CB*SW) - (0.046875D0*CA22*EL*MH22*SA2*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SW) - (0&
  &.0625D0*CA3*EL*MH12*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW) - (0.0625D0*CA22*CA3*EL*MH12*DBLE(CA1**INT(&
  &3.D0))*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW) - (0.03125D0*CA3*EL*MH22*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Usual(&
  &))/(SB*SW) - (0.03125D0*CA22*CA3*EL*MH22*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW) + (0.0625D0*CA3*EL*MH1&
  &2*SA22*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW) + (0.03125D0*CA3*EL*MH22*SA22*DBLE(CA1**INT(3.D0))*DBLE(&
  &MW**INT(-3.D0))*dMW2Usual())/(SB*SW) + (0.046875D0*EL*m12squared*SA2*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Usual()&
  &)/(CB2*SB*SW) + (0.140625D0*CA22*EL*m12squared*SA2*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB2*SB*SW) + (0&
  &.09375D0*CA3*EL*m12squared*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SB2*SW) + (0.09375D0*CA22*CA3*EL*m12squa&
  &red*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SB2*SW) - (0.09375D0*CA3*EL*m12squared*SA22*DBLE(CA1**INT(3.D0)&
  &)*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SB2*SW) + (0.0625D0*CA3*EL*MH12*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Usual())&
  &/(CB*SW) + (0.0625D0*CA22*CA3*EL*MH12*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Usual())/(CB*SW) + (0.03125D0*CA3*EL*MH22*&
  &DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Usual())/(CB*SW) + (0.03125D0*CA22*CA3*EL*MH22*DBLE(MW**INT(-3.D0))*DBLE(SA1**IN&
  &T(3.D0))*dMW2Usual())/(CB*SW) - (0.0625D0*CA3*EL*MH12*SA22*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Usual())/(CB*SW) - (0&
  &.03125D0*CA3*EL*MH22*SA22*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Usual())/(CB*SW) - (0.09375D0*CA3*EL*m12squared*DBLE(M&
  &W**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Usual())/(CB2*SB*SW) - (0.09375D0*CA22*CA3*EL*m12squared*DBLE(MW**INT(-3.D0))*DBLE(SA1&
  &**INT(3.D0))*dMW2Usual())/(CB2*SB*SW) + (0.09375D0*CA3*EL*m12squared*SA22*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Usual(&
  &))/(CB2*SB*SW) - (0.03125D0*EL*MH12*SA2*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Usual())/(SB*SW) - (0.09375D0*CA22*E&
  &L*MH12*SA2*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Usual())/(SB*SW) - (0.015625D0*EL*MH22*SA2*SA3*DBLE(MW**INT(-3.D0&
  &))*DBLE(SA1**INT(3.D0))*dMW2Usual())/(SB*SW) - (0.046875D0*CA22*EL*MH22*SA2*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2&
  &Usual())/(SB*SW) + (0.046875D0*EL*m12squared*SA2*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Usual())/(CB*SB2*SW) + (0.1&
  &40625D0*CA22*EL*m12squared*SA2*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Usual())/(CB*SB2*SW) + (0.09375D0*CA1*EL*MH12&
  &*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Usual())/(CB*SW) + (0.046875D0*CA1*EL*MH22*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA&
  &2**INT(3.D0))*dMW2Usual())/(CB*SW) + (0.140625D0*EL*m12squared*SA1*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Usual())/&
  &(CB*SW) - (0.09375D0*CA1*EL*MH12*SA12*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Usual())/(CB*SW) - (0.046875D0*CA1*EL*&
  &MH22*SA12*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Usual())/(CB*SW) + (0.140625D0*CA1*EL*m12squared*SA3*DBLE(MW**INT(&
  &-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Usual())/(SB*SW) - (0.09375D0*CA1*EL*m12squared*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))&
  &*dMW2Usual())/(CB2*SB*SW) + (0.09375D0*EL*MH12*SA1*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Usual())/(SB*SW) - (0.093&
  &75D0*CA12*EL*MH12*SA1*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Usual())/(SB*SW) + (0.046875D0*EL*MH22*SA1*SA3*DBLE(MW&
  &**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Usual())/(SB*SW) - (0.046875D0*CA12*EL*MH22*SA1*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(&
  &3.D0))*dMW2Usual())/(SB*SW) + (0.140625D0*CA1*EL*m12squared*SA12*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Usual())/(C&
  &B2*SB*SW) - (0.09375D0*EL*m12squared*SA1*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Usual())/(CB*SB2*SW) + (0.140625D0*&
  &CA12*EL*m12squared*SA1*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Usual())/(CB*SB2*SW) - (0.046875D0*EL*m12squared*SA1*&
  &SA3*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Usual())/(SB*SW*TB) - (0.046875D0*CA1*EL*m12squared*SA3*TB*DBLE(MW**INT(-3.D&
  &0))*DBLE(SA2**INT(3.D0))*dMW2Usual())/(CB*SW) + (0.03125D0*EL*MH12*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT&
  &(3.D0))*dMW2Usual())/(CB*SW) + (0.015625D0*EL*MH22*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Usua&
  &l())/(CB*SW) - (0.046875D0*EL*m12squared*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Usual())/(CB2*&
  &SB*SW) + (0.03125D0*EL*MH12*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dMW2Usual())/(SB*SW) + (0.01562&
  &5D0*EL*MH22*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dMW2Usual())/(SB*SW) - (0.046875D0*EL*m12square&
  &d*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dMW2Usual())/(CB*SB2*SW) + 0.5D0*CA2*MH12*SA3*DBLE(vS**IN&
  &T(-2.D0))*dvSMSBarUsual() + 0.25D0*CA2*MH22*SA3*DBLE(vS**INT(-2.D0))*dvSMSBarUsual() + 1.5D0*CA2*MH12*SA22*SA3*DBLE(vS**INT(-2&
  &.D0))*dvSMSBarUsual() + 0.75D0*CA2*MH22*SA22*SA3*DBLE(vS**INT(-2.D0))*dvSMSBarUsual() - 0.5D0*MH12*SA3*DBLE(CA2**INT(3.D0))*DB&
  &LE(vS**INT(-2.D0))*dvSMSBarUsual() - 0.25D0*MH22*SA3*DBLE(CA2**INT(3.D0))*DBLE(vS**INT(-2.D0))*dvSMSBarUsual() &
		& + CS1S1S1f111*dZH1H2OSUsual()/2D0 + CS1S1S1f211*dZH2H2OS()/2D0 + CS1S1S1f311*dZH3H2OSUsual()/2D0 &
		& + CS1S1S1f121*dZH1H1OS()/2D0 + CS1S1S1f221*dZH2H1OSUsual()/2D0 + CS1S1S1f321*dZH3H1OSUsual()/2D0 &
		& + CS1S1S1f121*dZH1H1OS()/2D0 + CS1S1S1f221*dZH2H1OSUsual()/2D0 + CS1S1S1f321*dZH3H1OSUsual()/2D0 &
		& )
	case (10)
		dRR11 = -CA2*SA1*dAlpha1MSBarAlter() - CA1*SA2*dAlpha2MSBarAlter()
		dRR12 = CA1*CA2*dAlpha1MSBarAlter() - SA1*SA2*dAlpha2MSBarAlter()
		dRR13 = CA2*dAlpha2MSBarAlter()
		dRR21 = -CA1*CA3*dAlpha1MSBarAlter() - CA1*CA3*SA2*dAlpha3MSBarAlter() - CA1*CA2*SA3*dAlpha2MSBarAlter() + &
			& SA1*SA3*dAlpha3MSBarAlter() + SA1*SA2*SA3*dAlpha1MSBarAlter()
		dRR22 = -CA3*SA1*dAlpha1MSBarAlter() - CA3*SA1*SA2*dAlpha3MSBarAlter() - CA1*SA3*dAlpha3MSBarAlter() - &
			& CA2*SA1*SA3*dAlpha2MSBarAlter() - CA1*SA2*SA3*dAlpha1MSBarAlter()
		dRR23 = CA2*CA3*dAlpha3MSBarAlter() - SA2*SA3*dAlpha2MSBarAlter()
		dRR31 = -CA1*CA2*CA3*dAlpha2MSBarAlter() + CA3*SA1*dAlpha3MSBarAlter() + CA3*SA1*SA2*dAlpha1MSBarAlter() + &
			& CA1*SA3*dAlpha1MSBarAlter() + CA1*SA2*SA3*dAlpha3MSBarAlter()
		dRR32 = -CA1*CA3*dAlpha3MSBarAlter() - CA2*CA3*SA1*dAlpha2MSBarAlter() - CA1*CA3*SA2*dAlpha1MSBarAlter() + &
			& SA1*SA3*dAlpha1MSBarAlter() + SA1*SA2*SA3*dAlpha3MSBarAlter()
		dRR33 = -CA3*SA2*dAlpha2MSBarAlter() - CA2*SA3*dAlpha3MSBarAlter()

		totalAmplitude = CS1S1S1f211*( &
&(0.125D0*CA1*CA3*EL*MH12*dAlpha1MSBarAlter())/(CB*MW*SW) + (0.125D0*CA1*CA22*CA3*EL*MH12*dAlpha1MSBarAlter())/(CB*MW*SW) + (0.062&
  &5D0*CA1*CA3*EL*MH22*dAlpha1MSBarAlter())/(CB*MW*SW) + (0.0625D0*CA1*CA22*CA3*EL*MH22*dAlpha1MSBarAlter())/(CB*MW*SW) + (0.125D&
  &0*CA3*EL*m12squared*SA1*dAlpha1MSBarAlter())/(CB*MW*SW) + (0.125D0*CA22*CA3*EL*m12squared*SA1*dAlpha1MSBarAlter())/(CB*MW*SW) &
  &- (1.125D0*CA1*CA3*EL*MH12*SA12*dAlpha1MSBarAlter())/(CB*MW*SW) - (1.125D0*CA1*CA22*CA3*EL*MH12*SA12*dAlpha1MSBarAlter())/(CB*&
  &MW*SW) - (0.5625D0*CA1*CA3*EL*MH22*SA12*dAlpha1MSBarAlter())/(CB*MW*SW) - (0.5625D0*CA1*CA22*CA3*EL*MH22*SA12*dAlpha1MSBarAlte&
  &r())/(CB*MW*SW) - (0.125D0*CA1*CA3*EL*MH12*SA22*dAlpha1MSBarAlter())/(CB*MW*SW) - (0.0625D0*CA1*CA3*EL*MH22*SA22*dAlpha1MSBarA&
  &lter())/(CB*MW*SW) - (0.125D0*CA3*EL*m12squared*SA1*SA22*dAlpha1MSBarAlter())/(CB*MW*SW) + (1.125D0*CA1*CA3*EL*MH12*SA12*SA22*&
  &dAlpha1MSBarAlter())/(CB*MW*SW) + (0.5625D0*CA1*CA3*EL*MH22*SA12*SA22*dAlpha1MSBarAlter())/(CB*MW*SW) + (0.28125D0*CA1*EL*m12s&
  &quared*SA2*SA3*dAlpha1MSBarAlter())/(CB*MW*SW) + (0.84375D0*CA1*CA22*EL*m12squared*SA2*SA3*dAlpha1MSBarAlter())/(CB*MW*SW) - (&
  &0.1875D0*EL*MH12*SA1*SA2*SA3*dAlpha1MSBarAlter())/(CB*MW*SW) - (0.5625D0*CA12*EL*MH12*SA1*SA2*SA3*dAlpha1MSBarAlter())/(CB*MW*&
  &SW) - (0.5625D0*CA22*EL*MH12*SA1*SA2*SA3*dAlpha1MSBarAlter())/(CB*MW*SW) - (1.6875D0*CA12*CA22*EL*MH12*SA1*SA2*SA3*dAlpha1MSBa&
  &rAlter())/(CB*MW*SW) - (0.09375D0*EL*MH22*SA1*SA2*SA3*dAlpha1MSBarAlter())/(CB*MW*SW) - (0.28125D0*CA12*EL*MH22*SA1*SA2*SA3*dA&
  &lpha1MSBarAlter())/(CB*MW*SW) - (0.28125D0*CA22*EL*MH22*SA1*SA2*SA3*dAlpha1MSBarAlter())/(CB*MW*SW) - (0.84375D0*CA12*CA22*EL*&
  &MH22*SA1*SA2*SA3*dAlpha1MSBarAlter())/(CB*MW*SW) + (0.21875D0*CA1*CA3*EL*m12squared*dAlpha1MSBarAlter())/(MW*SB*SW) + (0.21875&
  &D0*CA1*CA22*CA3*EL*m12squared*dAlpha1MSBarAlter())/(MW*SB*SW) - (0.15625D0*CA1*CA3*EL*m12squared*dAlpha1MSBarAlter())/(CB2*MW*&
  &SB*SW) - (0.15625D0*CA1*CA22*CA3*EL*m12squared*dAlpha1MSBarAlter())/(CB2*MW*SB*SW) + (0.125D0*CA3*EL*MH12*SA1*dAlpha1MSBarAlte&
  &r())/(MW*SB*SW) - (1.125D0*CA12*CA3*EL*MH12*SA1*dAlpha1MSBarAlter())/(MW*SB*SW) + (0.125D0*CA22*CA3*EL*MH12*SA1*dAlpha1MSBarAl&
  &ter())/(MW*SB*SW) - (1.125D0*CA12*CA22*CA3*EL*MH12*SA1*dAlpha1MSBarAlter())/(MW*SB*SW) + (0.0625D0*CA3*EL*MH22*SA1*dAlpha1MSBa&
  &rAlter())/(MW*SB*SW) - (0.5625D0*CA12*CA3*EL*MH22*SA1*dAlpha1MSBarAlter())/(MW*SB*SW) + (0.0625D0*CA22*CA3*EL*MH22*SA1*dAlpha1&
  &MSBarAlter())/(MW*SB*SW) - (0.5625D0*CA12*CA22*CA3*EL*MH22*SA1*dAlpha1MSBarAlter())/(MW*SB*SW) + (1.6875D0*CA1*CA3*EL*m12squar&
  &ed*SA12*dAlpha1MSBarAlter())/(CB2*MW*SB*SW) + (1.6875D0*CA1*CA22*CA3*EL*m12squared*SA12*dAlpha1MSBarAlter())/(CB2*MW*SB*SW) - &
  &(0.21875D0*CA1*CA3*EL*m12squared*SA22*dAlpha1MSBarAlter())/(MW*SB*SW) + (0.15625D0*CA1*CA3*EL*m12squared*SA22*dAlpha1MSBarAlte&
  &r())/(CB2*MW*SB*SW) - (0.125D0*CA3*EL*MH12*SA1*SA22*dAlpha1MSBarAlter())/(MW*SB*SW) + (1.125D0*CA12*CA3*EL*MH12*SA1*SA22*dAlph&
  &a1MSBarAlter())/(MW*SB*SW) - (0.0625D0*CA3*EL*MH22*SA1*SA22*dAlpha1MSBarAlter())/(MW*SB*SW) + (0.5625D0*CA12*CA3*EL*MH22*SA1*S&
  &A22*dAlpha1MSBarAlter())/(MW*SB*SW) - (1.6875D0*CA1*CA3*EL*m12squared*SA12*SA22*dAlpha1MSBarAlter())/(CB2*MW*SB*SW) + (0.1875D&
  &0*CA1*EL*MH12*SA2*SA3*dAlpha1MSBarAlter())/(MW*SB*SW) + (0.5625D0*CA1*CA22*EL*MH12*SA2*SA3*dAlpha1MSBarAlter())/(MW*SB*SW) + (&
  &0.09375D0*CA1*EL*MH22*SA2*SA3*dAlpha1MSBarAlter())/(MW*SB*SW) + (0.28125D0*CA1*CA22*EL*MH22*SA2*SA3*dAlpha1MSBarAlter())/(MW*S&
  &B*SW) - (0.28125D0*EL*m12squared*SA1*SA2*SA3*dAlpha1MSBarAlter())/(MW*SB*SW) - (0.84375D0*CA22*EL*m12squared*SA1*SA2*SA3*dAlph&
  &a1MSBarAlter())/(MW*SB*SW) + (0.1875D0*EL*m12squared*SA1*SA2*SA3*dAlpha1MSBarAlter())/(CB2*MW*SB*SW) + (0.84375D0*CA12*EL*m12s&
  &quared*SA1*SA2*SA3*dAlpha1MSBarAlter())/(CB2*MW*SB*SW) + (0.5625D0*CA22*EL*m12squared*SA1*SA2*SA3*dAlpha1MSBarAlter())/(CB2*MW&
  &*SB*SW) + (2.53125D0*CA12*CA22*EL*m12squared*SA1*SA2*SA3*dAlpha1MSBarAlter())/(CB2*MW*SB*SW) + (0.5625D0*CA1*EL*MH12*SA12*SA2*&
  &SA3*dAlpha1MSBarAlter())/(MW*SB*SW) + (1.6875D0*CA1*CA22*EL*MH12*SA12*SA2*SA3*dAlpha1MSBarAlter())/(MW*SB*SW) + (0.28125D0*CA1&
  &*EL*MH22*SA12*SA2*SA3*dAlpha1MSBarAlter())/(MW*SB*SW) + (0.84375D0*CA1*CA22*EL*MH22*SA12*SA2*SA3*dAlpha1MSBarAlter())/(MW*SB*S&
  &W) - (0.0625D0*CA3*EL*m12squared*SA1*dAlpha1MSBarAlter())/(CB*MW*SB2*SW) + (1.6875D0*CA12*CA3*EL*m12squared*SA1*dAlpha1MSBarAl&
  &ter())/(CB*MW*SB2*SW) - (0.0625D0*CA22*CA3*EL*m12squared*SA1*dAlpha1MSBarAlter())/(CB*MW*SB2*SW) + (1.6875D0*CA12*CA22*CA3*EL*&
  &m12squared*SA1*dAlpha1MSBarAlter())/(CB*MW*SB2*SW) + (0.0625D0*CA3*EL*m12squared*SA1*SA22*dAlpha1MSBarAlter())/(CB*MW*SB2*SW) &
  &- (1.6875D0*CA12*CA3*EL*m12squared*SA1*SA22*dAlpha1MSBarAlter())/(CB*MW*SB2*SW) - (0.1875D0*CA1*EL*m12squared*SA2*SA3*dAlpha1M&
  &SBarAlter())/(CB*MW*SB2*SW) - (0.5625D0*CA1*CA22*EL*m12squared*SA2*SA3*dAlpha1MSBarAlter())/(CB*MW*SB2*SW) - (0.84375D0*CA1*EL&
  &*m12squared*SA12*SA2*SA3*dAlpha1MSBarAlter())/(CB*MW*SB2*SW) - (2.53125D0*CA1*CA22*EL*m12squared*SA12*SA2*SA3*dAlpha1MSBarAlte&
  &r())/(CB*MW*SB2*SW) - (0.125D0*CA3*EL*m12squared*SA1*dAlpha1MSBarAlter())/(MW*SB*SW*TB) - (0.125D0*CA22*CA3*EL*m12squared*SA1*&
  &dAlpha1MSBarAlter())/(MW*SB*SW*TB) + (0.125D0*CA3*EL*m12squared*SA1*SA22*dAlpha1MSBarAlter())/(MW*SB*SW*TB) - (0.09375D0*CA1*E&
  &L*m12squared*SA2*SA3*dAlpha1MSBarAlter())/(MW*SB*SW*TB) - (0.28125D0*CA1*CA22*EL*m12squared*SA2*SA3*dAlpha1MSBarAlter())/(MW*S&
  &B*SW*TB) - (0.03125D0*CA1*CA3*EL*m12squared*TB*dAlpha1MSBarAlter())/(CB*MW*SW) - (0.03125D0*CA1*CA22*CA3*EL*m12squared*TB*dAlp&
  &ha1MSBarAlter())/(CB*MW*SW) + (0.03125D0*CA1*CA3*EL*m12squared*SA22*TB*dAlpha1MSBarAlter())/(CB*MW*SW) + (0.09375D0*EL*m12squa&
  &red*SA1*SA2*SA3*TB*dAlpha1MSBarAlter())/(CB*MW*SW) + (0.28125D0*CA22*EL*m12squared*SA1*SA2*SA3*TB*dAlpha1MSBarAlter())/(CB*MW*&
  &SW) + (0.5D0*CA1*CA2*CA3*EL*m12squared*SA2*dAlpha2MSBarAlter())/(CB*MW*SW) - (0.5D0*CA2*CA3*EL*MH12*SA1*SA2*dAlpha2MSBarAlter(&
  &))/(CB*MW*SW) - (1.5D0*CA12*CA2*CA3*EL*MH12*SA1*SA2*dAlpha2MSBarAlter())/(CB*MW*SW) - (0.25D0*CA2*CA3*EL*MH22*SA1*SA2*dAlpha2M&
  &SBarAlter())/(CB*MW*SW) - (0.75D0*CA12*CA2*CA3*EL*MH22*SA1*SA2*dAlpha2MSBarAlter())/(CB*MW*SW) + (0.1875D0*CA1*CA2*EL*MH12*SA3&
  &*dAlpha2MSBarAlter())/(CB*MW*SW) + (0.09375D0*CA1*CA2*EL*MH22*SA3*dAlpha2MSBarAlter())/(CB*MW*SW) + (0.28125D0*CA2*EL*m12squar&
  &ed*SA1*SA3*dAlpha2MSBarAlter())/(CB*MW*SW) - (0.1875D0*CA1*CA2*EL*MH12*SA12*SA3*dAlpha2MSBarAlter())/(CB*MW*SW) - (0.09375D0*C&
  &A1*CA2*EL*MH22*SA12*SA3*dAlpha2MSBarAlter())/(CB*MW*SW) - (1.6875D0*CA1*CA2*EL*MH12*SA22*SA3*dAlpha2MSBarAlter())/(CB*MW*SW) -&
  & (0.84375D0*CA1*CA2*EL*MH22*SA22*SA3*dAlpha2MSBarAlter())/(CB*MW*SW) - (2.53125D0*CA2*EL*m12squared*SA1*SA22*SA3*dAlpha2MSBarA&
  &lter())/(CB*MW*SW) + (1.6875D0*CA1*CA2*EL*MH12*SA12*SA22*SA3*dAlpha2MSBarAlter())/(CB*MW*SW) + (0.84375D0*CA1*CA2*EL*MH22*SA12&
  &*SA22*SA3*dAlpha2MSBarAlter())/(CB*MW*SW) + (0.5D0*CA1*CA2*CA3*EL*MH12*SA2*dAlpha2MSBarAlter())/(MW*SB*SW) + (0.25D0*CA1*CA2*C&
  &A3*EL*MH22*SA2*dAlpha2MSBarAlter())/(MW*SB*SW) - (0.875D0*CA2*CA3*EL*m12squared*SA1*SA2*dAlpha2MSBarAlter())/(MW*SB*SW) + (0.6&
  &25D0*CA2*CA3*EL*m12squared*SA1*SA2*dAlpha2MSBarAlter())/(CB2*MW*SB*SW) + (2.25D0*CA12*CA2*CA3*EL*m12squared*SA1*SA2*dAlpha2MSB&
  &arAlter())/(CB2*MW*SB*SW) + (1.5D0*CA1*CA2*CA3*EL*MH12*SA12*SA2*dAlpha2MSBarAlter())/(MW*SB*SW) + (0.75D0*CA1*CA2*CA3*EL*MH22*&
  &SA12*SA2*dAlpha2MSBarAlter())/(MW*SB*SW) + (0.28125D0*CA1*CA2*EL*m12squared*SA3*dAlpha2MSBarAlter())/(MW*SB*SW) - (0.1875D0*CA&
  &1*CA2*EL*m12squared*SA3*dAlpha2MSBarAlter())/(CB2*MW*SB*SW) + (0.1875D0*CA2*EL*MH12*SA1*SA3*dAlpha2MSBarAlter())/(MW*SB*SW) - &
  &(0.1875D0*CA12*CA2*EL*MH12*SA1*SA3*dAlpha2MSBarAlter())/(MW*SB*SW) + (0.09375D0*CA2*EL*MH22*SA1*SA3*dAlpha2MSBarAlter())/(MW*S&
  &B*SW) - (0.09375D0*CA12*CA2*EL*MH22*SA1*SA3*dAlpha2MSBarAlter())/(MW*SB*SW) + (0.28125D0*CA1*CA2*EL*m12squared*SA12*SA3*dAlpha&
  &2MSBarAlter())/(CB2*MW*SB*SW) - (2.53125D0*CA1*CA2*EL*m12squared*SA22*SA3*dAlpha2MSBarAlter())/(MW*SB*SW) + (1.6875D0*CA1*CA2*&
  &EL*m12squared*SA22*SA3*dAlpha2MSBarAlter())/(CB2*MW*SB*SW) - (1.6875D0*CA2*EL*MH12*SA1*SA22*SA3*dAlpha2MSBarAlter())/(MW*SB*SW&
  &) + (1.6875D0*CA12*CA2*EL*MH12*SA1*SA22*SA3*dAlpha2MSBarAlter())/(MW*SB*SW) - (0.84375D0*CA2*EL*MH22*SA1*SA22*SA3*dAlpha2MSBar&
  &Alter())/(MW*SB*SW) + (0.84375D0*CA12*CA2*EL*MH22*SA1*SA22*SA3*dAlpha2MSBarAlter())/(MW*SB*SW) - (2.53125D0*CA1*CA2*EL*m12squa&
  &red*SA12*SA22*SA3*dAlpha2MSBarAlter())/(CB2*MW*SB*SW) - (0.25D0*CA1*CA2*CA3*EL*m12squared*SA2*dAlpha2MSBarAlter())/(CB*MW*SB2*&
  &SW) - (2.25D0*CA1*CA2*CA3*EL*m12squared*SA12*SA2*dAlpha2MSBarAlter())/(CB*MW*SB2*SW) - (0.1875D0*CA2*EL*m12squared*SA1*SA3*dAl&
  &pha2MSBarAlter())/(CB*MW*SB2*SW) + (0.28125D0*CA12*CA2*EL*m12squared*SA1*SA3*dAlpha2MSBarAlter())/(CB*MW*SB2*SW) + (1.6875D0*C&
  &A2*EL*m12squared*SA1*SA22*SA3*dAlpha2MSBarAlter())/(CB*MW*SB2*SW) - (2.53125D0*CA12*CA2*EL*m12squared*SA1*SA22*SA3*dAlpha2MSBa&
  &rAlter())/(CB*MW*SB2*SW) - (0.5D0*CA1*CA2*CA3*EL*m12squared*SA2*dAlpha2MSBarAlter())/(MW*SB*SW*TB) - (0.09375D0*CA2*EL*m12squa&
  &red*SA1*SA3*dAlpha2MSBarAlter())/(MW*SB*SW*TB) + (0.84375D0*CA2*EL*m12squared*SA1*SA22*SA3*dAlpha2MSBarAlter())/(MW*SB*SW*TB) &
  &+ (0.125D0*CA2*CA3*EL*m12squared*SA1*SA2*TB*dAlpha2MSBarAlter())/(CB*MW*SW) - (0.09375D0*CA1*CA2*EL*m12squared*SA3*TB*dAlpha2M&
  &SBarAlter())/(CB*MW*SW) + (0.84375D0*CA1*CA2*EL*m12squared*SA22*SA3*TB*dAlpha2MSBarAlter())/(CB*MW*SW) + (0.5D0*MH12*SA2*SA3*d&
  &Alpha2MSBarAlter())/vS - (4.5D0*CA22*MH12*SA2*SA3*dAlpha2MSBarAlter())/vS + (0.25D0*MH22*SA2*SA3*dAlpha2MSBarAlter())/vS - (2.&
  &25D0*CA22*MH22*SA2*SA3*dAlpha2MSBarAlter())/vS + (0.1875D0*CA1*CA3*EL*MH12*SA2*dAlpha3MSBarAlter())/(CB*MW*SW) + (0.5625D0*CA1&
  &*CA22*CA3*EL*MH12*SA2*dAlpha3MSBarAlter())/(CB*MW*SW) + (0.09375D0*CA1*CA3*EL*MH22*SA2*dAlpha3MSBarAlter())/(CB*MW*SW) + (0.28&
  &125D0*CA1*CA22*CA3*EL*MH22*SA2*dAlpha3MSBarAlter())/(CB*MW*SW) + (0.28125D0*CA3*EL*m12squared*SA1*SA2*dAlpha3MSBarAlter())/(CB&
  &*MW*SW) + (0.84375D0*CA22*CA3*EL*m12squared*SA1*SA2*dAlpha3MSBarAlter())/(CB*MW*SW) - (0.1875D0*CA1*CA3*EL*MH12*SA12*SA2*dAlph&
  &a3MSBarAlter())/(CB*MW*SW) - (0.5625D0*CA1*CA22*CA3*EL*MH12*SA12*SA2*dAlpha3MSBarAlter())/(CB*MW*SW) - (0.09375D0*CA1*CA3*EL*M&
  &H22*SA12*SA2*dAlpha3MSBarAlter())/(CB*MW*SW) - (0.28125D0*CA1*CA22*CA3*EL*MH22*SA12*SA2*dAlpha3MSBarAlter())/(CB*MW*SW) + (0.1&
  &25D0*CA1*EL*m12squared*SA3*dAlpha3MSBarAlter())/(CB*MW*SW) + (0.125D0*CA1*CA22*EL*m12squared*SA3*dAlpha3MSBarAlter())/(CB*MW*S&
  &W) - (0.125D0*EL*MH12*SA1*SA3*dAlpha3MSBarAlter())/(CB*MW*SW) - (0.375D0*CA12*EL*MH12*SA1*SA3*dAlpha3MSBarAlter())/(CB*MW*SW) &
  &- (0.125D0*CA22*EL*MH12*SA1*SA3*dAlpha3MSBarAlter())/(CB*MW*SW) - (0.375D0*CA12*CA22*EL*MH12*SA1*SA3*dAlpha3MSBarAlter())/(CB*&
  &MW*SW) - (0.0625D0*EL*MH22*SA1*SA3*dAlpha3MSBarAlter())/(CB*MW*SW) - (0.1875D0*CA12*EL*MH22*SA1*SA3*dAlpha3MSBarAlter())/(CB*M&
  &W*SW) - (0.0625D0*CA22*EL*MH22*SA1*SA3*dAlpha3MSBarAlter())/(CB*MW*SW) - (0.1875D0*CA12*CA22*EL*MH22*SA1*SA3*dAlpha3MSBarAlter&
  &())/(CB*MW*SW) - (0.125D0*CA1*EL*m12squared*SA22*SA3*dAlpha3MSBarAlter())/(CB*MW*SW) + (0.125D0*EL*MH12*SA1*SA22*SA3*dAlpha3MS&
  &BarAlter())/(CB*MW*SW) + (0.375D0*CA12*EL*MH12*SA1*SA22*SA3*dAlpha3MSBarAlter())/(CB*MW*SW) + (0.0625D0*EL*MH22*SA1*SA22*SA3*d&
  &Alpha3MSBarAlter())/(CB*MW*SW) + (0.1875D0*CA12*EL*MH22*SA1*SA22*SA3*dAlpha3MSBarAlter())/(CB*MW*SW) + (0.28125D0*CA1*CA3*EL*m&
  &12squared*SA2*dAlpha3MSBarAlter())/(MW*SB*SW) + (0.84375D0*CA1*CA22*CA3*EL*m12squared*SA2*dAlpha3MSBarAlter())/(MW*SB*SW) - (0&
  &.1875D0*CA1*CA3*EL*m12squared*SA2*dAlpha3MSBarAlter())/(CB2*MW*SB*SW) - (0.5625D0*CA1*CA22*CA3*EL*m12squared*SA2*dAlpha3MSBarA&
  &lter())/(CB2*MW*SB*SW) + (0.1875D0*CA3*EL*MH12*SA1*SA2*dAlpha3MSBarAlter())/(MW*SB*SW) - (0.1875D0*CA12*CA3*EL*MH12*SA1*SA2*dA&
  &lpha3MSBarAlter())/(MW*SB*SW) + (0.5625D0*CA22*CA3*EL*MH12*SA1*SA2*dAlpha3MSBarAlter())/(MW*SB*SW) - (0.5625D0*CA12*CA22*CA3*E&
  &L*MH12*SA1*SA2*dAlpha3MSBarAlter())/(MW*SB*SW) + (0.09375D0*CA3*EL*MH22*SA1*SA2*dAlpha3MSBarAlter())/(MW*SB*SW) - (0.09375D0*C&
  &A12*CA3*EL*MH22*SA1*SA2*dAlpha3MSBarAlter())/(MW*SB*SW) + (0.28125D0*CA22*CA3*EL*MH22*SA1*SA2*dAlpha3MSBarAlter())/(MW*SB*SW) &
  &- (0.28125D0*CA12*CA22*CA3*EL*MH22*SA1*SA2*dAlpha3MSBarAlter())/(MW*SB*SW) + (0.28125D0*CA1*CA3*EL*m12squared*SA12*SA2*dAlpha3&
  &MSBarAlter())/(CB2*MW*SB*SW) + (0.84375D0*CA1*CA22*CA3*EL*m12squared*SA12*SA2*dAlpha3MSBarAlter())/(CB2*MW*SB*SW) + (0.125D0*C&
  &A1*EL*MH12*SA3*dAlpha3MSBarAlter())/(MW*SB*SW) + (0.125D0*CA1*CA22*EL*MH12*SA3*dAlpha3MSBarAlter())/(MW*SB*SW) + (0.0625D0*CA1&
  &*EL*MH22*SA3*dAlpha3MSBarAlter())/(MW*SB*SW) + (0.0625D0*CA1*CA22*EL*MH22*SA3*dAlpha3MSBarAlter())/(MW*SB*SW) - (0.21875D0*EL*&
  &m12squared*SA1*SA3*dAlpha3MSBarAlter())/(MW*SB*SW) - (0.21875D0*CA22*EL*m12squared*SA1*SA3*dAlpha3MSBarAlter())/(MW*SB*SW) + (&
  &0.15625D0*EL*m12squared*SA1*SA3*dAlpha3MSBarAlter())/(CB2*MW*SB*SW) + (0.5625D0*CA12*EL*m12squared*SA1*SA3*dAlpha3MSBarAlter()&
  &)/(CB2*MW*SB*SW) + (0.15625D0*CA22*EL*m12squared*SA1*SA3*dAlpha3MSBarAlter())/(CB2*MW*SB*SW) + (0.5625D0*CA12*CA22*EL*m12squar&
  &ed*SA1*SA3*dAlpha3MSBarAlter())/(CB2*MW*SB*SW) + (0.375D0*CA1*EL*MH12*SA12*SA3*dAlpha3MSBarAlter())/(MW*SB*SW) + (0.375D0*CA1*&
  &CA22*EL*MH12*SA12*SA3*dAlpha3MSBarAlter())/(MW*SB*SW) + (0.1875D0*CA1*EL*MH22*SA12*SA3*dAlpha3MSBarAlter())/(MW*SB*SW) + (0.18&
  &75D0*CA1*CA22*EL*MH22*SA12*SA3*dAlpha3MSBarAlter())/(MW*SB*SW) - (0.125D0*CA1*EL*MH12*SA22*SA3*dAlpha3MSBarAlter())/(MW*SB*SW)&
  & - (0.0625D0*CA1*EL*MH22*SA22*SA3*dAlpha3MSBarAlter())/(MW*SB*SW) + (0.21875D0*EL*m12squared*SA1*SA22*SA3*dAlpha3MSBarAlter())&
  &/(MW*SB*SW) - (0.15625D0*EL*m12squared*SA1*SA22*SA3*dAlpha3MSBarAlter())/(CB2*MW*SB*SW) - (0.5625D0*CA12*EL*m12squared*SA1*SA2&
  &2*SA3*dAlpha3MSBarAlter())/(CB2*MW*SB*SW) - (0.375D0*CA1*EL*MH12*SA12*SA22*SA3*dAlpha3MSBarAlter())/(MW*SB*SW) - (0.1875D0*CA1&
  &*EL*MH22*SA12*SA22*SA3*dAlpha3MSBarAlter())/(MW*SB*SW) - (0.1875D0*CA3*EL*m12squared*SA1*SA2*dAlpha3MSBarAlter())/(CB*MW*SB2*S&
  &W) + (0.28125D0*CA12*CA3*EL*m12squared*SA1*SA2*dAlpha3MSBarAlter())/(CB*MW*SB2*SW) - (0.5625D0*CA22*CA3*EL*m12squared*SA1*SA2*&
  &dAlpha3MSBarAlter())/(CB*MW*SB2*SW) + (0.84375D0*CA12*CA22*CA3*EL*m12squared*SA1*SA2*dAlpha3MSBarAlter())/(CB*MW*SB2*SW) - (0.&
  &0625D0*CA1*EL*m12squared*SA3*dAlpha3MSBarAlter())/(CB*MW*SB2*SW) - (0.0625D0*CA1*CA22*EL*m12squared*SA3*dAlpha3MSBarAlter())/(&
  &CB*MW*SB2*SW) - (0.5625D0*CA1*EL*m12squared*SA12*SA3*dAlpha3MSBarAlter())/(CB*MW*SB2*SW) - (0.5625D0*CA1*CA22*EL*m12squared*SA&
  &12*SA3*dAlpha3MSBarAlter())/(CB*MW*SB2*SW) + (0.0625D0*CA1*EL*m12squared*SA22*SA3*dAlpha3MSBarAlter())/(CB*MW*SB2*SW) + (0.562&
  &5D0*CA1*EL*m12squared*SA12*SA22*SA3*dAlpha3MSBarAlter())/(CB*MW*SB2*SW) - (0.09375D0*CA3*EL*m12squared*SA1*SA2*dAlpha3MSBarAlt&
  &er())/(MW*SB*SW*TB) - (0.28125D0*CA22*CA3*EL*m12squared*SA1*SA2*dAlpha3MSBarAlter())/(MW*SB*SW*TB) - (0.125D0*CA1*EL*m12square&
  &d*SA3*dAlpha3MSBarAlter())/(MW*SB*SW*TB) - (0.125D0*CA1*CA22*EL*m12squared*SA3*dAlpha3MSBarAlter())/(MW*SB*SW*TB) + (0.125D0*C&
  &A1*EL*m12squared*SA22*SA3*dAlpha3MSBarAlter())/(MW*SB*SW*TB) - (0.09375D0*CA1*CA3*EL*m12squared*SA2*TB*dAlpha3MSBarAlter())/(C&
  &B*MW*SW) - (0.28125D0*CA1*CA22*CA3*EL*m12squared*SA2*TB*dAlpha3MSBarAlter())/(CB*MW*SW) + (0.03125D0*EL*m12squared*SA1*SA3*TB*&
  &dAlpha3MSBarAlter())/(CB*MW*SW) + (0.03125D0*CA22*EL*m12squared*SA1*SA3*TB*dAlpha3MSBarAlter())/(CB*MW*SW) - (0.03125D0*EL*m12&
  &squared*SA1*SA22*SA3*TB*dAlpha3MSBarAlter())/(CB*MW*SW) - (0.5D0*CA2*CA3*MH12*dAlpha3MSBarAlter())/vS - (0.25D0*CA2*CA3*MH22*d&
  &Alpha3MSBarAlter())/vS - (1.5D0*CA2*CA3*MH12*SA22*dAlpha3MSBarAlter())/vS - (0.75D0*CA2*CA3*MH22*SA22*dAlpha3MSBarAlter())/vS &
  &+ (0.015625D0*CA3*EL*m12squared*SA1*dBetaMSBarAlter())/(CB*MW*SW) + (0.015625D0*CA22*CA3*EL*m12squared*SA1*dBetaMSBarAlter())/&
  &(CB*MW*SW) - (0.015625D0*CA3*EL*m12squared*SA1*SA22*dBetaMSBarAlter())/(CB*MW*SW) + (0.09375D0*CA1*EL*m12squared*SA2*SA3*dBeta&
  &MSBarAlter())/(CB*MW*SW) + (0.28125D0*CA1*CA22*EL*m12squared*SA2*SA3*dBetaMSBarAlter())/(CB*MW*SW) - (0.0625D0*CA1*CA3*EL*m12s&
  &quared*dBetaMSBarAlter())/(MW*SB*SW) - (0.0625D0*CA1*CA22*CA3*EL*m12squared*dBetaMSBarAlter())/(MW*SB*SW) + (0.0625D0*CA1*CA3*&
  &EL*m12squared*dBetaMSBarAlter())/(CB2*MW*SB*SW) + (0.0625D0*CA1*CA22*CA3*EL*m12squared*dBetaMSBarAlter())/(CB2*MW*SB*SW) - (0.&
  &0625D0*CA3*EL*MH12*SA1*dBetaMSBarAlter())/(MW*SB*SW) - (0.1875D0*CA12*CA3*EL*MH12*SA1*dBetaMSBarAlter())/(MW*SB*SW) - (0.0625D&
  &0*CA22*CA3*EL*MH12*SA1*dBetaMSBarAlter())/(MW*SB*SW) - (0.1875D0*CA12*CA22*CA3*EL*MH12*SA1*dBetaMSBarAlter())/(MW*SB*SW) + (0.&
  &0625D0*CA3*EL*MH12*SA1*dBetaMSBarAlter())/(CB2*MW*SB*SW) + (0.1875D0*CA12*CA3*EL*MH12*SA1*dBetaMSBarAlter())/(CB2*MW*SB*SW) + &
  &(0.0625D0*CA22*CA3*EL*MH12*SA1*dBetaMSBarAlter())/(CB2*MW*SB*SW) + (0.1875D0*CA12*CA22*CA3*EL*MH12*SA1*dBetaMSBarAlter())/(CB2&
  &*MW*SB*SW) - (0.03125D0*CA3*EL*MH22*SA1*dBetaMSBarAlter())/(MW*SB*SW) - (0.09375D0*CA12*CA3*EL*MH22*SA1*dBetaMSBarAlter())/(MW&
  &*SB*SW) - (0.03125D0*CA22*CA3*EL*MH22*SA1*dBetaMSBarAlter())/(MW*SB*SW) - (0.09375D0*CA12*CA22*CA3*EL*MH22*SA1*dBetaMSBarAlter&
  &())/(MW*SB*SW) + (0.03125D0*CA3*EL*MH22*SA1*dBetaMSBarAlter())/(CB2*MW*SB*SW) + (0.09375D0*CA12*CA3*EL*MH22*SA1*dBetaMSBarAlte&
  &r())/(CB2*MW*SB*SW) + (0.03125D0*CA22*CA3*EL*MH22*SA1*dBetaMSBarAlter())/(CB2*MW*SB*SW) + (0.09375D0*CA12*CA22*CA3*EL*MH22*SA1&
  &*dBetaMSBarAlter())/(CB2*MW*SB*SW) + (0.5625D0*CA1*CA3*EL*m12squared*SA12*dBetaMSBarAlter())/(CB2*MW*SB*SW) + (0.5625D0*CA1*CA&
  &22*CA3*EL*m12squared*SA12*dBetaMSBarAlter())/(CB2*MW*SB*SW) + (0.0625D0*CA1*CA3*EL*m12squared*SA22*dBetaMSBarAlter())/(MW*SB*S&
  &W) - (0.0625D0*CA1*CA3*EL*m12squared*SA22*dBetaMSBarAlter())/(CB2*MW*SB*SW) + (0.0625D0*CA3*EL*MH12*SA1*SA22*dBetaMSBarAlter()&
  &)/(MW*SB*SW) + (0.1875D0*CA12*CA3*EL*MH12*SA1*SA22*dBetaMSBarAlter())/(MW*SB*SW) - (0.0625D0*CA3*EL*MH12*SA1*SA22*dBetaMSBarAl&
  &ter())/(CB2*MW*SB*SW) - (0.1875D0*CA12*CA3*EL*MH12*SA1*SA22*dBetaMSBarAlter())/(CB2*MW*SB*SW) + (0.03125D0*CA3*EL*MH22*SA1*SA2&
  &2*dBetaMSBarAlter())/(MW*SB*SW) + (0.09375D0*CA12*CA3*EL*MH22*SA1*SA22*dBetaMSBarAlter())/(MW*SB*SW) - (0.03125D0*CA3*EL*MH22*&
  &SA1*SA22*dBetaMSBarAlter())/(CB2*MW*SB*SW) - (0.09375D0*CA12*CA3*EL*MH22*SA1*SA22*dBetaMSBarAlter())/(CB2*MW*SB*SW) - (0.5625D&
  &0*CA1*CA3*EL*m12squared*SA12*SA22*dBetaMSBarAlter())/(CB2*MW*SB*SW) - (0.09375D0*CA1*EL*MH12*SA2*SA3*dBetaMSBarAlter())/(MW*SB&
  &*SW) - (0.28125D0*CA1*CA22*EL*MH12*SA2*SA3*dBetaMSBarAlter())/(MW*SB*SW) + (0.09375D0*CA1*EL*MH12*SA2*SA3*dBetaMSBarAlter())/(&
  &CB2*MW*SB*SW) + (0.28125D0*CA1*CA22*EL*MH12*SA2*SA3*dBetaMSBarAlter())/(CB2*MW*SB*SW) - (0.046875D0*CA1*EL*MH22*SA2*SA3*dBetaM&
  &SBarAlter())/(MW*SB*SW) - (0.140625D0*CA1*CA22*EL*MH22*SA2*SA3*dBetaMSBarAlter())/(MW*SB*SW) + (0.046875D0*CA1*EL*MH22*SA2*SA3&
  &*dBetaMSBarAlter())/(CB2*MW*SB*SW) + (0.140625D0*CA1*CA22*EL*MH22*SA2*SA3*dBetaMSBarAlter())/(CB2*MW*SB*SW) - (0.140625D0*EL*m&
  &12squared*SA1*SA2*SA3*dBetaMSBarAlter())/(MW*SB*SW) - (0.421875D0*CA22*EL*m12squared*SA1*SA2*SA3*dBetaMSBarAlter())/(MW*SB*SW)&
  & - (0.046875D0*EL*m12squared*SA1*SA2*SA3*dBetaMSBarAlter())/(CB2*MW*SB*SW) + (0.28125D0*CA12*EL*m12squared*SA1*SA2*SA3*dBetaMS&
  &BarAlter())/(CB2*MW*SB*SW) - (0.140625D0*CA22*EL*m12squared*SA1*SA2*SA3*dBetaMSBarAlter())/(CB2*MW*SB*SW) + (0.84375D0*CA12*CA&
  &22*EL*m12squared*SA1*SA2*SA3*dBetaMSBarAlter())/(CB2*MW*SB*SW) + (0.09375D0*CA1*EL*MH12*SA12*SA2*SA3*dBetaMSBarAlter())/(MW*SB&
  &*SW) + (0.28125D0*CA1*CA22*EL*MH12*SA12*SA2*SA3*dBetaMSBarAlter())/(MW*SB*SW) - (0.09375D0*CA1*EL*MH12*SA12*SA2*SA3*dBetaMSBar&
  &Alter())/(CB2*MW*SB*SW) - (0.28125D0*CA1*CA22*EL*MH12*SA12*SA2*SA3*dBetaMSBarAlter())/(CB2*MW*SB*SW) + (0.046875D0*CA1*EL*MH22&
  &*SA12*SA2*SA3*dBetaMSBarAlter())/(MW*SB*SW) + (0.140625D0*CA1*CA22*EL*MH22*SA12*SA2*SA3*dBetaMSBarAlter())/(MW*SB*SW) - (0.046&
  &875D0*CA1*EL*MH22*SA12*SA2*SA3*dBetaMSBarAlter())/(CB2*MW*SB*SW) - (0.140625D0*CA1*CA22*EL*MH22*SA12*SA2*SA3*dBetaMSBarAlter()&
  &)/(CB2*MW*SB*SW) + (0.203125D0*CA3*EL*m12squared*SA1*dBetaMSBarAlter())/(CB*MW*SB2*SW) + (0.84375D0*CA12*CA3*EL*m12squared*SA1&
  &*dBetaMSBarAlter())/(CB*MW*SB2*SW) + (0.203125D0*CA22*CA3*EL*m12squared*SA1*dBetaMSBarAlter())/(CB*MW*SB2*SW) + (0.84375D0*CA1&
  &2*CA22*CA3*EL*m12squared*SA1*dBetaMSBarAlter())/(CB*MW*SB2*SW) - (0.203125D0*CA3*EL*m12squared*SA1*SA22*dBetaMSBarAlter())/(CB&
  &*MW*SB2*SW) - (0.84375D0*CA12*CA3*EL*m12squared*SA1*SA22*dBetaMSBarAlter())/(CB*MW*SB2*SW) + (0.10546875D0*CA1*EL*m12squared*S&
  &A2*SA3*dBetaMSBarAlter())/(CB*MW*SB2*SW) + (0.31640625D0*CA1*CA22*EL*m12squared*SA2*SA3*dBetaMSBarAlter())/(CB*MW*SB2*SW) - (0&
  &.38671875D0*CA1*EL*m12squared*SA12*SA2*SA3*dBetaMSBarAlter())/(CB*MW*SB2*SW) - (1.16015625D0*CA1*CA22*EL*m12squared*SA12*SA2*S&
  &A3*dBetaMSBarAlter())/(CB*MW*SB2*SW) + (0.125D0*CA1*CA3*EL*MH12*dBetaMSBarAlter())/(MW*SB*SW*TB) + (0.125D0*CA1*CA22*CA3*EL*MH&
  &12*dBetaMSBarAlter())/(MW*SB*SW*TB) + (0.0625D0*CA1*CA3*EL*MH22*dBetaMSBarAlter())/(MW*SB*SW*TB) + (0.0625D0*CA1*CA22*CA3*EL*M&
  &H22*dBetaMSBarAlter())/(MW*SB*SW*TB) - (0.203125D0*CA3*EL*m12squared*SA1*dBetaMSBarAlter())/(MW*SB*SW*TB) - (0.203125D0*CA22*C&
  &A3*EL*m12squared*SA1*dBetaMSBarAlter())/(MW*SB*SW*TB) + (0.375D0*CA1*CA3*EL*MH12*SA12*dBetaMSBarAlter())/(MW*SB*SW*TB) + (0.37&
  &5D0*CA1*CA22*CA3*EL*MH12*SA12*dBetaMSBarAlter())/(MW*SB*SW*TB) + (0.1875D0*CA1*CA3*EL*MH22*SA12*dBetaMSBarAlter())/(MW*SB*SW*T&
  &B) + (0.1875D0*CA1*CA22*CA3*EL*MH22*SA12*dBetaMSBarAlter())/(MW*SB*SW*TB) - (0.125D0*CA1*CA3*EL*MH12*SA22*dBetaMSBarAlter())/(&
  &MW*SB*SW*TB) - (0.0625D0*CA1*CA3*EL*MH22*SA22*dBetaMSBarAlter())/(MW*SB*SW*TB) + (0.203125D0*CA3*EL*m12squared*SA1*SA22*dBetaM&
  &SBarAlter())/(MW*SB*SW*TB) - (0.375D0*CA1*CA3*EL*MH12*SA12*SA22*dBetaMSBarAlter())/(MW*SB*SW*TB) - (0.1875D0*CA1*CA3*EL*MH22*S&
  &A12*SA22*dBetaMSBarAlter())/(MW*SB*SW*TB) - (0.140625D0*CA1*EL*m12squared*SA2*SA3*dBetaMSBarAlter())/(MW*SB*SW*TB) - (0.421875&
  &D0*CA1*CA22*EL*m12squared*SA2*SA3*dBetaMSBarAlter())/(MW*SB*SW*TB) - (0.1875D0*EL*MH12*SA1*SA2*SA3*dBetaMSBarAlter())/(MW*SB*S&
  &W*TB) + (0.1875D0*CA12*EL*MH12*SA1*SA2*SA3*dBetaMSBarAlter())/(MW*SB*SW*TB) - (0.5625D0*CA22*EL*MH12*SA1*SA2*SA3*dBetaMSBarAlt&
  &er())/(MW*SB*SW*TB) + (0.5625D0*CA12*CA22*EL*MH12*SA1*SA2*SA3*dBetaMSBarAlter())/(MW*SB*SW*TB) - (0.09375D0*EL*MH22*SA1*SA2*SA&
  &3*dBetaMSBarAlter())/(MW*SB*SW*TB) + (0.09375D0*CA12*EL*MH22*SA1*SA2*SA3*dBetaMSBarAlter())/(MW*SB*SW*TB) - (0.28125D0*CA22*EL&
  &*MH22*SA1*SA2*SA3*dBetaMSBarAlter())/(MW*SB*SW*TB) + (0.28125D0*CA12*CA22*EL*MH22*SA1*SA2*SA3*dBetaMSBarAlter())/(MW*SB*SW*TB)&
  & - (0.125D0*CA1*CA3*EL*m12squared*TB*dBetaMSBarAlter())/(CB*MW*SW) - (0.125D0*CA1*CA22*CA3*EL*m12squared*TB*dBetaMSBarAlter())&
  &/(CB*MW*SW) + (0.0625D0*CA3*EL*MH12*SA1*TB*dBetaMSBarAlter())/(CB*MW*SW) + (0.1875D0*CA12*CA3*EL*MH12*SA1*TB*dBetaMSBarAlter()&
  &)/(CB*MW*SW) + (0.0625D0*CA22*CA3*EL*MH12*SA1*TB*dBetaMSBarAlter())/(CB*MW*SW) + (0.1875D0*CA12*CA22*CA3*EL*MH12*SA1*TB*dBetaM&
  &SBarAlter())/(CB*MW*SW) + (0.03125D0*CA3*EL*MH22*SA1*TB*dBetaMSBarAlter())/(CB*MW*SW) + (0.09375D0*CA12*CA3*EL*MH22*SA1*TB*dBe&
  &taMSBarAlter())/(CB*MW*SW) + (0.03125D0*CA22*CA3*EL*MH22*SA1*TB*dBetaMSBarAlter())/(CB*MW*SW) + (0.09375D0*CA12*CA22*CA3*EL*MH&
  &22*SA1*TB*dBetaMSBarAlter())/(CB*MW*SW) + (0.125D0*CA1*CA3*EL*m12squared*SA22*TB*dBetaMSBarAlter())/(CB*MW*SW) - (0.0625D0*CA3&
  &*EL*MH12*SA1*SA22*TB*dBetaMSBarAlter())/(CB*MW*SW) - (0.1875D0*CA12*CA3*EL*MH12*SA1*SA22*TB*dBetaMSBarAlter())/(CB*MW*SW) - (0&
  &.03125D0*CA3*EL*MH22*SA1*SA22*TB*dBetaMSBarAlter())/(CB*MW*SW) - (0.09375D0*CA12*CA3*EL*MH22*SA1*SA22*TB*dBetaMSBarAlter())/(C&
  &B*MW*SW) + (0.09375D0*CA1*EL*MH12*SA2*SA3*TB*dBetaMSBarAlter())/(CB*MW*SW) + (0.28125D0*CA1*CA22*EL*MH12*SA2*SA3*TB*dBetaMSBar&
  &Alter())/(CB*MW*SW) + (0.046875D0*CA1*EL*MH22*SA2*SA3*TB*dBetaMSBarAlter())/(CB*MW*SW) + (0.140625D0*CA1*CA22*EL*MH22*SA2*SA3*&
  &TB*dBetaMSBarAlter())/(CB*MW*SW) + (0.140625D0*EL*m12squared*SA1*SA2*SA3*TB*dBetaMSBarAlter())/(CB*MW*SW) + (0.421875D0*CA22*E&
  &L*m12squared*SA1*SA2*SA3*TB*dBetaMSBarAlter())/(CB*MW*SW) - (0.09375D0*CA1*EL*MH12*SA12*SA2*SA3*TB*dBetaMSBarAlter())/(CB*MW*S&
  &W) - (0.28125D0*CA1*CA22*EL*MH12*SA12*SA2*SA3*TB*dBetaMSBarAlter())/(CB*MW*SW) - (0.046875D0*CA1*EL*MH22*SA12*SA2*SA3*TB*dBeta&
  &MSBarAlter())/(CB*MW*SW) - (0.140625D0*CA1*CA22*EL*MH22*SA12*SA2*SA3*TB*dBetaMSBarAlter())/(CB*MW*SW) - (0.1875D0*CA1*CA3*EL*m&
  &12squared*dBetaMSBarAlter())/(MW*SB*SW*TB2) - (0.1875D0*CA1*CA22*CA3*EL*m12squared*dBetaMSBarAlter())/(MW*SB*SW*TB2) + (0.1875&
  &D0*CA1*CA3*EL*m12squared*SA22*dBetaMSBarAlter())/(MW*SB*SW*TB2) + (0.09375D0*EL*m12squared*SA1*SA2*SA3*dBetaMSBarAlter())/(MW*&
  &SB*SW*TB2) + (0.28125D0*CA22*EL*m12squared*SA1*SA2*SA3*dBetaMSBarAlter())/(MW*SB*SW*TB2) - (0.03125D0*CA3*EL*m12squared*SA1*TB&
  &2*dBetaMSBarAlter())/(CB*MW*SW) - (0.03125D0*CA22*CA3*EL*m12squared*SA1*TB2*dBetaMSBarAlter())/(CB*MW*SW) + (0.03125D0*CA3*EL*&
  &m12squared*SA1*SA22*TB2*dBetaMSBarAlter())/(CB*MW*SW) - (0.140625D0*CA1*EL*m12squared*SA2*SA3*TB2*dBetaMSBarAlter())/(CB*MW*SW&
  &) - (0.421875D0*CA1*CA22*EL*m12squared*SA2*SA3*TB2*dBetaMSBarAlter())/(CB*MW*SW) + (0.375D0*CA3*EL*MH12*dAlpha1MSBarAlter()*DB&
  &LE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.375D0*CA22*CA3*EL*MH12*dAlpha1MSBarAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.1875D0*CA&
  &3*EL*MH22*dAlpha1MSBarAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.1875D0*CA22*CA3*EL*MH22*dAlpha1MSBarAlter()*DBLE(CA1**INT(&
  &3.D0)))/(CB*MW*SW) - (0.375D0*CA3*EL*MH12*SA22*dAlpha1MSBarAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.1875D0*CA3*EL*MH22*SA&
  &22*dAlpha1MSBarAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.5625D0*CA3*EL*m12squared*dAlpha1MSBarAlter()*DBLE(CA1**INT(3.D0))&
  &)/(CB2*MW*SB*SW) - (0.5625D0*CA22*CA3*EL*m12squared*dAlpha1MSBarAlter()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) + (0.5625D0*CA3*E&
  &L*m12squared*SA22*dAlpha1MSBarAlter()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.1875D0*EL*MH12*SA2*SA3*dAlpha1MSBarAlter()*DBL&
  &E(CA1**INT(3.D0)))/(MW*SB*SW) - (0.5625D0*CA22*EL*MH12*SA2*SA3*dAlpha1MSBarAlter()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.09375&
  &D0*EL*MH22*SA2*SA3*dAlpha1MSBarAlter()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.28125D0*CA22*EL*MH22*SA2*SA3*dAlpha1MSBarAlter()*&
  &DBLE(CA1**INT(3.D0)))/(MW*SB*SW) + (0.28125D0*EL*m12squared*SA2*SA3*dAlpha1MSBarAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW) +&
  & (0.84375D0*CA22*EL*m12squared*SA2*SA3*dAlpha1MSBarAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.0625D0*CA2*EL*MH12*SA3*dA&
  &lpha2MSBarAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.03125D0*CA2*EL*MH22*SA3*dAlpha2MSBarAlter()*DBLE(CA1**INT(3.D0)))/(CB*&
  &MW*SW) - (0.5625D0*CA2*EL*MH12*SA22*SA3*dAlpha2MSBarAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.28125D0*CA2*EL*MH22*SA22*SA3&
  &*dAlpha2MSBarAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.5D0*CA2*CA3*EL*MH12*SA2*dAlpha2MSBarAlter()*DBLE(CA1**INT(3.D0)))/(&
  &MW*SB*SW) - (0.25D0*CA2*CA3*EL*MH22*SA2*dAlpha2MSBarAlter()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.09375D0*CA2*EL*m12squared*SA&
  &3*dAlpha2MSBarAlter()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) + (0.84375D0*CA2*EL*m12squared*SA22*SA3*dAlpha2MSBarAlter()*DBLE(CA&
  &1**INT(3.D0)))/(CB2*MW*SB*SW) + (0.75D0*CA2*CA3*EL*m12squared*SA2*dAlpha2MSBarAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW) + (&
  &0.0625D0*CA3*EL*MH12*SA2*dAlpha3MSBarAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.1875D0*CA22*CA3*EL*MH12*SA2*dAlpha3MSBarAlt&
  &er()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.03125D0*CA3*EL*MH22*SA2*dAlpha3MSBarAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.0&
  &9375D0*CA22*CA3*EL*MH22*SA2*dAlpha3MSBarAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.09375D0*CA3*EL*m12squared*SA2*dAlpha3MSB&
  &arAlter()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.28125D0*CA22*CA3*EL*m12squared*SA2*dAlpha3MSBarAlter()*DBLE(CA1**INT(3.D0)&
  &))/(CB2*MW*SB*SW) - (0.125D0*EL*MH12*SA3*dAlpha3MSBarAlter()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.125D0*CA22*EL*MH12*SA3*dAlp&
  &ha3MSBarAlter()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.0625D0*EL*MH22*SA3*dAlpha3MSBarAlter()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) &
  &- (0.0625D0*CA22*EL*MH22*SA3*dAlpha3MSBarAlter()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) + (0.125D0*EL*MH12*SA22*SA3*dAlpha3MSBarAlte&
  &r()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) + (0.0625D0*EL*MH22*SA22*SA3*dAlpha3MSBarAlter()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) + (0.18&
  &75D0*EL*m12squared*SA3*dAlpha3MSBarAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.1875D0*CA22*EL*m12squared*SA3*dAlpha3MSBa&
  &rAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.1875D0*EL*m12squared*SA22*SA3*dAlpha3MSBarAlter()*DBLE(CA1**INT(3.D0)))/(CB&
  &*MW*SB2*SW) - (0.1875D0*CA3*EL*m12squared*dBetaMSBarAlter()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.1875D0*CA22*CA3*EL*m12sq&
  &uared*dBetaMSBarAlter()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) + (0.1875D0*CA3*EL*m12squared*SA22*dBetaMSBarAlter()*DBLE(CA1**IN&
  &T(3.D0)))/(CB2*MW*SB*SW) - (0.03125D0*EL*MH12*SA2*SA3*dBetaMSBarAlter()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.09375D0*CA22*EL*&
  &MH12*SA2*SA3*dBetaMSBarAlter()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) + (0.03125D0*EL*MH12*SA2*SA3*dBetaMSBarAlter()*DBLE(CA1**INT(3&
  &.D0)))/(CB2*MW*SB*SW) + (0.09375D0*CA22*EL*MH12*SA2*SA3*dBetaMSBarAlter()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.015625D0*E&
  &L*MH22*SA2*SA3*dBetaMSBarAlter()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.046875D0*CA22*EL*MH22*SA2*SA3*dBetaMSBarAlter()*DBLE(CA&
  &1**INT(3.D0)))/(MW*SB*SW) + (0.015625D0*EL*MH22*SA2*SA3*dBetaMSBarAlter()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) + (0.046875D0*C&
  &A22*EL*MH22*SA2*SA3*dBetaMSBarAlter()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) + (0.12890625D0*EL*m12squared*SA2*SA3*dBetaMSBarAlt&
  &er()*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.38671875D0*CA22*EL*m12squared*SA2*SA3*dBetaMSBarAlter()*DBLE(CA1**INT(3.D0)))/(&
  &CB*MW*SB2*SW) - (0.125D0*CA3*EL*MH12*dBetaMSBarAlter()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW*TB) - (0.125D0*CA22*CA3*EL*MH12*dBetaMS&
  &BarAlter()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW*TB) - (0.0625D0*CA3*EL*MH22*dBetaMSBarAlter()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW*TB) -&
  & (0.0625D0*CA22*CA3*EL*MH22*dBetaMSBarAlter()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW*TB) + (0.125D0*CA3*EL*MH12*SA22*dBetaMSBarAlter(&
  &)*DBLE(CA1**INT(3.D0)))/(MW*SB*SW*TB) + (0.0625D0*CA3*EL*MH22*SA22*dBetaMSBarAlter()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW*TB) + (0.&
  &03125D0*EL*MH12*SA2*SA3*TB*dBetaMSBarAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.09375D0*CA22*EL*MH12*SA2*SA3*TB*dBetaMSBarA&
  &lter()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.015625D0*EL*MH22*SA2*SA3*TB*dBetaMSBarAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + &
  &(0.046875D0*CA22*EL*MH22*SA2*SA3*TB*dBetaMSBarAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.5625D0*CA1*EL*MH12*SA3*dAlpha2MSBa&
  &rAlter()*DBLE(CA2**INT(3.D0)))/(CB*MW*SW) + (0.28125D0*CA1*EL*MH22*SA3*dAlpha2MSBarAlter()*DBLE(CA2**INT(3.D0)))/(CB*MW*SW) + &
  &(0.84375D0*EL*m12squared*SA1*SA3*dAlpha2MSBarAlter()*DBLE(CA2**INT(3.D0)))/(CB*MW*SW) - (0.5625D0*CA1*EL*MH12*SA12*SA3*dAlpha2&
  &MSBarAlter()*DBLE(CA2**INT(3.D0)))/(CB*MW*SW) - (0.28125D0*CA1*EL*MH22*SA12*SA3*dAlpha2MSBarAlter()*DBLE(CA2**INT(3.D0)))/(CB*&
  &MW*SW) + (0.84375D0*CA1*EL*m12squared*SA3*dAlpha2MSBarAlter()*DBLE(CA2**INT(3.D0)))/(MW*SB*SW) - (0.5625D0*CA1*EL*m12squared*S&
  &A3*dAlpha2MSBarAlter()*DBLE(CA2**INT(3.D0)))/(CB2*MW*SB*SW) + (0.5625D0*EL*MH12*SA1*SA3*dAlpha2MSBarAlter()*DBLE(CA2**INT(3.D0&
  &)))/(MW*SB*SW) - (0.5625D0*CA12*EL*MH12*SA1*SA3*dAlpha2MSBarAlter()*DBLE(CA2**INT(3.D0)))/(MW*SB*SW) + (0.28125D0*EL*MH22*SA1*&
  &SA3*dAlpha2MSBarAlter()*DBLE(CA2**INT(3.D0)))/(MW*SB*SW) - (0.28125D0*CA12*EL*MH22*SA1*SA3*dAlpha2MSBarAlter()*DBLE(CA2**INT(3&
  &.D0)))/(MW*SB*SW) + (0.84375D0*CA1*EL*m12squared*SA12*SA3*dAlpha2MSBarAlter()*DBLE(CA2**INT(3.D0)))/(CB2*MW*SB*SW) - (0.5625D0&
  &*EL*m12squared*SA1*SA3*dAlpha2MSBarAlter()*DBLE(CA2**INT(3.D0)))/(CB*MW*SB2*SW) + (0.84375D0*CA12*EL*m12squared*SA1*SA3*dAlpha&
  &2MSBarAlter()*DBLE(CA2**INT(3.D0)))/(CB*MW*SB2*SW) - (0.28125D0*EL*m12squared*SA1*SA3*dAlpha2MSBarAlter()*DBLE(CA2**INT(3.D0))&
  &)/(MW*SB*SW*TB) - (0.28125D0*CA1*EL*m12squared*SA3*TB*dAlpha2MSBarAlter()*DBLE(CA2**INT(3.D0)))/(CB*MW*SW) + (0.5D0*CA3*MH12*d&
  &Alpha3MSBarAlter()*DBLE(CA2**INT(3.D0)))/vS + (0.25D0*CA3*MH22*dAlpha3MSBarAlter()*DBLE(CA2**INT(3.D0)))/vS + (0.1875D0*EL*MH1&
  &2*SA3*dAlpha2MSBarAlter()*DBLE(CA1**INT(3.D0))*DBLE(CA2**INT(3.D0)))/(CB*MW*SW) + (0.09375D0*EL*MH22*SA3*dAlpha2MSBarAlter()*D&
  &BLE(CA1**INT(3.D0))*DBLE(CA2**INT(3.D0)))/(CB*MW*SW) - (0.28125D0*EL*m12squared*SA3*dAlpha2MSBarAlter()*DBLE(CA1**INT(3.D0))*D&
  &BLE(CA2**INT(3.D0)))/(CB2*MW*SB*SW) - (0.28125D0*CA3*EL*m12squared*SA1*dBetaMSBarAlter()*DBLE(CB**INT(-3.D0)))/(MW*SW) - (0.84&
  &375D0*CA12*CA3*EL*m12squared*SA1*dBetaMSBarAlter()*DBLE(CB**INT(-3.D0)))/(MW*SW) - (0.28125D0*CA22*CA3*EL*m12squared*SA1*dBeta&
  &MSBarAlter()*DBLE(CB**INT(-3.D0)))/(MW*SW) - (0.84375D0*CA12*CA22*CA3*EL*m12squared*SA1*dBetaMSBarAlter()*DBLE(CB**INT(-3.D0))&
  &)/(MW*SW) + (0.28125D0*CA3*EL*m12squared*SA1*SA22*dBetaMSBarAlter()*DBLE(CB**INT(-3.D0)))/(MW*SW) + (0.84375D0*CA12*CA3*EL*m12&
  &squared*SA1*SA22*dBetaMSBarAlter()*DBLE(CB**INT(-3.D0)))/(MW*SW) - (0.36328125D0*CA1*EL*m12squared*SA2*SA3*dBetaMSBarAlter()*D&
  &BLE(CB**INT(-3.D0)))/(MW*SW) - (1.08984375D0*CA1*CA22*EL*m12squared*SA2*SA3*dBetaMSBarAlter()*DBLE(CB**INT(-3.D0)))/(MW*SW) + &
  &(0.45703125D0*CA1*EL*m12squared*SA12*SA2*SA3*dBetaMSBarAlter()*DBLE(CB**INT(-3.D0)))/(MW*SW) + (1.37109375D0*CA1*CA22*EL*m12sq&
  &uared*SA12*SA2*SA3*dBetaMSBarAlter()*DBLE(CB**INT(-3.D0)))/(MW*SW) - (0.0625D0*CA3*EL*m12squared*SA1*dBetaMSBarAlter()*DBLE(CB&
  &**INT(-3.D0)))/(MW*SB2*SW) - (0.28125D0*CA12*CA3*EL*m12squared*SA1*dBetaMSBarAlter()*DBLE(CB**INT(-3.D0)))/(MW*SB2*SW) - (0.06&
  &25D0*CA22*CA3*EL*m12squared*SA1*dBetaMSBarAlter()*DBLE(CB**INT(-3.D0)))/(MW*SB2*SW) - (0.28125D0*CA12*CA22*CA3*EL*m12squared*S&
  &A1*dBetaMSBarAlter()*DBLE(CB**INT(-3.D0)))/(MW*SB2*SW) + (0.0625D0*CA3*EL*m12squared*SA1*SA22*dBetaMSBarAlter()*DBLE(CB**INT(-&
  &3.D0)))/(MW*SB2*SW) + (0.28125D0*CA12*CA3*EL*m12squared*SA1*SA22*dBetaMSBarAlter()*DBLE(CB**INT(-3.D0)))/(MW*SB2*SW) - (0.0585&
  &9375D0*CA1*EL*m12squared*SA2*SA3*dBetaMSBarAlter()*DBLE(CB**INT(-3.D0)))/(MW*SB2*SW) - (0.17578125D0*CA1*CA22*EL*m12squared*SA&
  &2*SA3*dBetaMSBarAlter()*DBLE(CB**INT(-3.D0)))/(MW*SB2*SW) + (0.10546875D0*CA1*EL*m12squared*SA12*SA2*SA3*dBetaMSBarAlter()*DBL&
  &E(CB**INT(-3.D0)))/(MW*SB2*SW) + (0.31640625D0*CA1*CA22*EL*m12squared*SA12*SA2*SA3*dBetaMSBarAlter()*DBLE(CB**INT(-3.D0)))/(MW&
  &*SB2*SW) - (0.15234375D0*EL*m12squared*SA2*SA3*dBetaMSBarAlter()*DBLE(CA1**INT(3.D0))*DBLE(CB**INT(-3.D0)))/(MW*SW) - (0.45703&
  &125D0*CA22*EL*m12squared*SA2*SA3*dBetaMSBarAlter()*DBLE(CA1**INT(3.D0))*DBLE(CB**INT(-3.D0)))/(MW*SW) - (0.03515625D0*EL*m12sq&
  &uared*SA2*SA3*dBetaMSBarAlter()*DBLE(CA1**INT(3.D0))*DBLE(CB**INT(-3.D0)))/(MW*SB2*SW) - (0.10546875D0*CA22*EL*m12squared*SA2*&
  &SA3*dBetaMSBarAlter()*DBLE(CA1**INT(3.D0))*DBLE(CB**INT(-3.D0)))/(MW*SB2*SW) + (0.1875D0*EL*MH12*SA2*SA3*dAlpha1MSBarAlter()*D&
  &BLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.5625D0*CA22*EL*MH12*SA2*SA3*dAlpha1MSBarAlter()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.093&
  &75D0*EL*MH22*SA2*SA3*dAlpha1MSBarAlter()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.28125D0*CA22*EL*MH22*SA2*SA3*dAlpha1MSBarAlter(&
  &)*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.375D0*CA3*EL*MH12*dAlpha1MSBarAlter()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.375D0*CA22&
  &*CA3*EL*MH12*dAlpha1MSBarAlter()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.1875D0*CA3*EL*MH22*dAlpha1MSBarAlter()*DBLE(SA1**INT(3.&
  &D0)))/(MW*SB*SW) + (0.1875D0*CA22*CA3*EL*MH22*dAlpha1MSBarAlter()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0.375D0*CA3*EL*MH12*SA22&
  &*dAlpha1MSBarAlter()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0.1875D0*CA3*EL*MH22*SA22*dAlpha1MSBarAlter()*DBLE(SA1**INT(3.D0)))/(&
  &MW*SB*SW) - (0.28125D0*EL*m12squared*SA2*SA3*dAlpha1MSBarAlter()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.84375D0*CA22*EL*m12&
  &squared*SA2*SA3*dAlpha1MSBarAlter()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.5625D0*CA3*EL*m12squared*dAlpha1MSBarAlter()*DBL&
  &E(SA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.5625D0*CA22*CA3*EL*m12squared*dAlpha1MSBarAlter()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) &
  &+ (0.5625D0*CA3*EL*m12squared*SA22*dAlpha1MSBarAlter()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.5D0*CA2*CA3*EL*MH12*SA2*dAlph&
  &a2MSBarAlter()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.25D0*CA2*CA3*EL*MH22*SA2*dAlpha2MSBarAlter()*DBLE(SA1**INT(3.D0)))/(CB*MW&
  &*SW) - (0.75D0*CA2*CA3*EL*m12squared*SA2*dAlpha2MSBarAlter()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) + (0.0625D0*CA2*EL*MH12*SA3*&
  &dAlpha2MSBarAlter()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.03125D0*CA2*EL*MH22*SA3*dAlpha2MSBarAlter()*DBLE(SA1**INT(3.D0)))/(M&
  &W*SB*SW) - (0.5625D0*CA2*EL*MH12*SA22*SA3*dAlpha2MSBarAlter()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0.28125D0*CA2*EL*MH22*SA22*S&
  &A3*dAlpha2MSBarAlter()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0.09375D0*CA2*EL*m12squared*SA3*dAlpha2MSBarAlter()*DBLE(SA1**INT(3&
  &.D0)))/(CB*MW*SB2*SW) + (0.84375D0*CA2*EL*m12squared*SA22*SA3*dAlpha2MSBarAlter()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.12&
  &5D0*EL*MH12*SA3*dAlpha3MSBarAlter()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.125D0*CA22*EL*MH12*SA3*dAlpha3MSBarAlter()*DBLE(SA1*&
  &*INT(3.D0)))/(CB*MW*SW) + (0.0625D0*EL*MH22*SA3*dAlpha3MSBarAlter()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.0625D0*CA22*EL*MH22*&
  &SA3*dAlpha3MSBarAlter()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) - (0.125D0*EL*MH12*SA22*SA3*dAlpha3MSBarAlter()*DBLE(SA1**INT(3.D0)))&
  &/(CB*MW*SW) - (0.0625D0*EL*MH22*SA22*SA3*dAlpha3MSBarAlter()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.0625D0*CA3*EL*MH12*SA2*dAlp&
  &ha3MSBarAlter()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.1875D0*CA22*CA3*EL*MH12*SA2*dAlpha3MSBarAlter()*DBLE(SA1**INT(3.D0)))/(M&
  &W*SB*SW) + (0.03125D0*CA3*EL*MH22*SA2*dAlpha3MSBarAlter()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.09375D0*CA22*CA3*EL*MH22*SA2*d&
  &Alpha3MSBarAlter()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0.1875D0*EL*m12squared*SA3*dAlpha3MSBarAlter()*DBLE(SA1**INT(3.D0)))/(C&
  &B2*MW*SB*SW) - (0.1875D0*CA22*EL*m12squared*SA3*dAlpha3MSBarAlter()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) + (0.1875D0*EL*m12squ&
  &ared*SA22*SA3*dAlpha3MSBarAlter()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.09375D0*CA3*EL*m12squared*SA2*dAlpha3MSBarAlter()*&
  &DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.28125D0*CA22*CA3*EL*m12squared*SA2*dAlpha3MSBarAlter()*DBLE(SA1**INT(3.D0)))/(CB*MW*&
  &SB2*SW) + (0.0625D0*CA3*EL*MH12*dBetaMSBarAlter()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.0625D0*CA22*CA3*EL*MH12*dBetaMSBarAlte&
  &r()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0.0625D0*CA3*EL*MH12*dBetaMSBarAlter()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.0625D&
  &0*CA22*CA3*EL*MH12*dBetaMSBarAlter()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) + (0.03125D0*CA3*EL*MH22*dBetaMSBarAlter()*DBLE(SA1*&
  &*INT(3.D0)))/(MW*SB*SW) + (0.03125D0*CA22*CA3*EL*MH22*dBetaMSBarAlter()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0.03125D0*CA3*EL*M&
  &H22*dBetaMSBarAlter()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.03125D0*CA22*CA3*EL*MH22*dBetaMSBarAlter()*DBLE(SA1**INT(3.D0)&
  &))/(CB2*MW*SB*SW) - (0.0625D0*CA3*EL*MH12*SA22*dBetaMSBarAlter()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.0625D0*CA3*EL*MH12*SA22&
  &*dBetaMSBarAlter()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.03125D0*CA3*EL*MH22*SA22*dBetaMSBarAlter()*DBLE(SA1**INT(3.D0)))/&
  &(MW*SB*SW) + (0.03125D0*CA3*EL*MH22*SA22*dBetaMSBarAlter()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.09375D0*EL*m12squared*SA2&
  &*SA3*dBetaMSBarAlter()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.28125D0*CA22*EL*m12squared*SA2*SA3*dBetaMSBarAlter()*DBLE(SA1&
  &**INT(3.D0)))/(CB2*MW*SB*SW) - (0.28125D0*CA3*EL*m12squared*dBetaMSBarAlter()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.28125D&
  &0*CA22*CA3*EL*m12squared*dBetaMSBarAlter()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.28125D0*CA3*EL*m12squared*SA22*dBetaMSBar&
  &Alter()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.0625D0*EL*MH12*SA2*SA3*dBetaMSBarAlter()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW*TB)&
  & - (0.1875D0*CA22*EL*MH12*SA2*SA3*dBetaMSBarAlter()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW*TB) - (0.03125D0*EL*MH22*SA2*SA3*dBetaMSBa&
  &rAlter()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW*TB) - (0.09375D0*CA22*EL*MH22*SA2*SA3*dBetaMSBarAlter()*DBLE(SA1**INT(3.D0)))/(MW*SB*&
  &SW*TB) - (0.0625D0*CA3*EL*MH12*TB*dBetaMSBarAlter()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) - (0.0625D0*CA22*CA3*EL*MH12*TB*dBetaMSBa&
  &rAlter()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) - (0.03125D0*CA3*EL*MH22*TB*dBetaMSBarAlter()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) - (0.&
  &03125D0*CA22*CA3*EL*MH22*TB*dBetaMSBarAlter()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.0625D0*CA3*EL*MH12*SA22*TB*dBetaMSBarAlter&
  &()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.03125D0*CA3*EL*MH22*SA22*TB*dBetaMSBarAlter()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.1&
  &875D0*EL*MH12*SA3*dAlpha2MSBarAlter()*DBLE(CA2**INT(3.D0))*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.09375D0*EL*MH22*SA3*dAlpha2MS&
  &BarAlter()*DBLE(CA2**INT(3.D0))*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0.28125D0*EL*m12squared*SA3*dAlpha2MSBarAlter()*DBLE(CA2**&
  &INT(3.D0))*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.28125D0*CA3*EL*m12squared*dBetaMSBarAlter()*DBLE(CB**INT(-3.D0))*DBLE(SA1&
  &**INT(3.D0)))/(MW*SW) + (0.28125D0*CA22*CA3*EL*m12squared*dBetaMSBarAlter()*DBLE(CB**INT(-3.D0))*DBLE(SA1**INT(3.D0)))/(MW*SW)&
  & - (0.28125D0*CA3*EL*m12squared*SA22*dBetaMSBarAlter()*DBLE(CB**INT(-3.D0))*DBLE(SA1**INT(3.D0)))/(MW*SW) + (0.09375D0*CA3*EL*&
  &m12squared*dBetaMSBarAlter()*DBLE(CB**INT(-3.D0))*DBLE(SA1**INT(3.D0)))/(MW*SB2*SW) + (0.09375D0*CA22*CA3*EL*m12squared*dBetaM&
  &SBarAlter()*DBLE(CB**INT(-3.D0))*DBLE(SA1**INT(3.D0)))/(MW*SB2*SW) - (0.09375D0*CA3*EL*m12squared*SA22*dBetaMSBarAlter()*DBLE(&
  &CB**INT(-3.D0))*DBLE(SA1**INT(3.D0)))/(MW*SB2*SW) - (0.28125D0*CA1*EL*m12squared*SA3*dAlpha1MSBarAlter()*DBLE(SA2**INT(3.D0)))&
  &/(CB*MW*SW) + (0.1875D0*EL*MH12*SA1*SA3*dAlpha1MSBarAlter()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.5625D0*CA12*EL*MH12*SA1*SA3*&
  &dAlpha1MSBarAlter()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.09375D0*EL*MH22*SA1*SA3*dAlpha1MSBarAlter()*DBLE(SA2**INT(3.D0)))/(C&
  &B*MW*SW) + (0.28125D0*CA12*EL*MH22*SA1*SA3*dAlpha1MSBarAlter()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.1875D0*CA1*EL*MH12*SA3*dA&
  &lpha1MSBarAlter()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.09375D0*CA1*EL*MH22*SA3*dAlpha1MSBarAlter()*DBLE(SA2**INT(3.D0)))/(MW*&
  &SB*SW) + (0.28125D0*EL*m12squared*SA1*SA3*dAlpha1MSBarAlter()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.1875D0*EL*m12squared*SA1*S&
  &A3*dAlpha1MSBarAlter()*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) - (0.84375D0*CA12*EL*m12squared*SA1*SA3*dAlpha1MSBarAlter()*DBLE(S&
  &A2**INT(3.D0)))/(CB2*MW*SB*SW) - (0.5625D0*CA1*EL*MH12*SA12*SA3*dAlpha1MSBarAlter()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.2812&
  &5D0*CA1*EL*MH22*SA12*SA3*dAlpha1MSBarAlter()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) + (0.1875D0*CA1*EL*m12squared*SA3*dAlpha1MSBarAl&
  &ter()*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) + (0.84375D0*CA1*EL*m12squared*SA12*SA3*dAlpha1MSBarAlter()*DBLE(SA2**INT(3.D0)))/(&
  &CB*MW*SB2*SW) + (0.09375D0*CA1*EL*m12squared*SA3*dAlpha1MSBarAlter()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) - (0.09375D0*EL*m12sq&
  &uared*SA1*SA3*TB*dAlpha1MSBarAlter()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (1.5D0*MH12*SA3*dAlpha2MSBarAlter()*DBLE(SA2**INT(3.D0&
  &)))/vS + (0.75D0*MH22*SA3*dAlpha2MSBarAlter()*DBLE(SA2**INT(3.D0)))/vS - (0.1875D0*CA1*CA3*EL*MH12*dAlpha3MSBarAlter()*DBLE(SA&
  &2**INT(3.D0)))/(CB*MW*SW) - (0.09375D0*CA1*CA3*EL*MH22*dAlpha3MSBarAlter()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.28125D0*CA3*E&
  &L*m12squared*SA1*dAlpha3MSBarAlter()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.1875D0*CA1*CA3*EL*MH12*SA12*dAlpha3MSBarAlter()*DBL&
  &E(SA2**INT(3.D0)))/(CB*MW*SW) + (0.09375D0*CA1*CA3*EL*MH22*SA12*dAlpha3MSBarAlter()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.2812&
  &5D0*CA1*CA3*EL*m12squared*dAlpha3MSBarAlter()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) + (0.1875D0*CA1*CA3*EL*m12squared*dAlpha3MSBarA&
  &lter()*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) - (0.1875D0*CA3*EL*MH12*SA1*dAlpha3MSBarAlter()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) +&
  & (0.1875D0*CA12*CA3*EL*MH12*SA1*dAlpha3MSBarAlter()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.09375D0*CA3*EL*MH22*SA1*dAlpha3MSBar&
  &Alter()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) + (0.09375D0*CA12*CA3*EL*MH22*SA1*dAlpha3MSBarAlter()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW&
  &) - (0.28125D0*CA1*CA3*EL*m12squared*SA12*dAlpha3MSBarAlter()*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) + (0.1875D0*CA3*EL*m12squar&
  &ed*SA1*dAlpha3MSBarAlter()*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) - (0.28125D0*CA12*CA3*EL*m12squared*SA1*dAlpha3MSBarAlter()*DB&
  &LE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) + (0.09375D0*CA3*EL*m12squared*SA1*dAlpha3MSBarAlter()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) &
  &+ (0.09375D0*CA1*CA3*EL*m12squared*TB*dAlpha3MSBarAlter()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.09375D0*CA1*EL*m12squared*SA3*&
  &dBetaMSBarAlter()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.09375D0*CA1*EL*MH12*SA3*dBetaMSBarAlter()*DBLE(SA2**INT(3.D0)))/(MW*SB&
  &*SW) - (0.09375D0*CA1*EL*MH12*SA3*dBetaMSBarAlter()*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) + (0.046875D0*CA1*EL*MH22*SA3*dBetaMS&
  &BarAlter()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.046875D0*CA1*EL*MH22*SA3*dBetaMSBarAlter()*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*S&
  &W) + (0.140625D0*EL*m12squared*SA1*SA3*dBetaMSBarAlter()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) + (0.046875D0*EL*m12squared*SA1*SA3*&
  &dBetaMSBarAlter()*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) - (0.28125D0*CA12*EL*m12squared*SA1*SA3*dBetaMSBarAlter()*DBLE(SA2**INT&
  &(3.D0)))/(CB2*MW*SB*SW) - (0.09375D0*CA1*EL*MH12*SA12*SA3*dBetaMSBarAlter()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) + (0.09375D0*CA1*&
  &EL*MH12*SA12*SA3*dBetaMSBarAlter()*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) - (0.046875D0*CA1*EL*MH22*SA12*SA3*dBetaMSBarAlter()*D&
  &BLE(SA2**INT(3.D0)))/(MW*SB*SW) + (0.046875D0*CA1*EL*MH22*SA12*SA3*dBetaMSBarAlter()*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) - (0&
  &.10546875D0*CA1*EL*m12squared*SA3*dBetaMSBarAlter()*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) + (0.38671875D0*CA1*EL*m12squared*SA1&
  &2*SA3*dBetaMSBarAlter()*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) + (0.140625D0*CA1*EL*m12squared*SA3*dBetaMSBarAlter()*DBLE(SA2**I&
  &NT(3.D0)))/(MW*SB*SW*TB) + (0.1875D0*EL*MH12*SA1*SA3*dBetaMSBarAlter()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) - (0.1875D0*CA12*EL&
  &*MH12*SA1*SA3*dBetaMSBarAlter()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) + (0.09375D0*EL*MH22*SA1*SA3*dBetaMSBarAlter()*DBLE(SA2**I&
  &NT(3.D0)))/(MW*SB*SW*TB) - (0.09375D0*CA12*EL*MH22*SA1*SA3*dBetaMSBarAlter()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) - (0.09375D0*&
  &CA1*EL*MH12*SA3*TB*dBetaMSBarAlter()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.046875D0*CA1*EL*MH22*SA3*TB*dBetaMSBarAlter()*DBLE(&
  &SA2**INT(3.D0)))/(CB*MW*SW) - (0.140625D0*EL*m12squared*SA1*SA3*TB*dBetaMSBarAlter()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.093&
  &75D0*CA1*EL*MH12*SA12*SA3*TB*dBetaMSBarAlter()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.046875D0*CA1*EL*MH22*SA12*SA3*TB*dBetaMSB&
  &arAlter()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.09375D0*EL*m12squared*SA1*SA3*dBetaMSBarAlter()*DBLE(SA2**INT(3.D0)))/(MW*SB*S&
  &W*TB2) + (0.140625D0*CA1*EL*m12squared*SA3*TB2*dBetaMSBarAlter()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.1875D0*EL*MH12*SA3*dAlp&
  &ha1MSBarAlter()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) + (0.09375D0*EL*MH22*SA3*dAlpha1MSBarAlter()*DBLE(CA1**I&
  &NT(3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.28125D0*EL*m12squared*SA3*dAlpha1MSBarAlter()*DBLE(CA1**INT(3.D0))*DBLE(SA2**I&
  &NT(3.D0)))/(CB*MW*SB2*SW) - (0.0625D0*CA3*EL*MH12*dAlpha3MSBarAlter()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - &
  &(0.03125D0*CA3*EL*MH22*dAlpha3MSBarAlter()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.09375D0*CA3*EL*m12square&
  &d*dAlpha3MSBarAlter()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) + (0.03125D0*EL*MH12*SA3*dBetaMSBarAlter()*DBL&
  &E(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.03125D0*EL*MH12*SA3*dBetaMSBarAlter()*DBLE(CA1**INT(3.D0))*DBLE(SA2**I&
  &NT(3.D0)))/(CB2*MW*SB*SW) + (0.015625D0*EL*MH22*SA3*dBetaMSBarAlter()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - &
  &(0.015625D0*EL*MH22*SA3*dBetaMSBarAlter()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) - (0.12890625D0*EL*m12squa&
  &red*SA3*dBetaMSBarAlter()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) - (0.03125D0*EL*MH12*SA3*TB*dBetaMSBarAlte&
  &r()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.015625D0*EL*MH22*SA3*TB*dBetaMSBarAlter()*DBLE(CA1**INT(3.D0))*&
  &DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.36328125D0*CA1*EL*m12squared*SA3*dBetaMSBarAlter()*DBLE(CB**INT(-3.D0))*DBLE(SA2**INT(3.&
  &D0)))/(MW*SW) - (0.45703125D0*CA1*EL*m12squared*SA12*SA3*dBetaMSBarAlter()*DBLE(CB**INT(-3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SW) &
  &+ (0.05859375D0*CA1*EL*m12squared*SA3*dBetaMSBarAlter()*DBLE(CB**INT(-3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SB2*SW) - (0.10546875D0&
  &*CA1*EL*m12squared*SA12*SA3*dBetaMSBarAlter()*DBLE(CB**INT(-3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SB2*SW) + (0.15234375D0*EL*m12squ&
  &ared*SA3*dBetaMSBarAlter()*DBLE(CA1**INT(3.D0))*DBLE(CB**INT(-3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SW) + (0.03515625D0*EL*m12squar&
  &ed*SA3*dBetaMSBarAlter()*DBLE(CA1**INT(3.D0))*DBLE(CB**INT(-3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SB2*SW) - (0.1875D0*EL*MH12*SA3*d&
  &Alpha1MSBarAlter()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.09375D0*EL*MH22*SA3*dAlpha1MSBarAlter()*DBLE(SA1&
  &**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.28125D0*EL*m12squared*SA3*dAlpha1MSBarAlter()*DBLE(SA1**INT(3.D0))*DBLE(SA2&
  &**INT(3.D0)))/(CB2*MW*SB*SW) - (0.0625D0*CA3*EL*MH12*dAlpha3MSBarAlter()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SB*SW)&
  & - (0.03125D0*CA3*EL*MH22*dAlpha3MSBarAlter()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) + (0.09375D0*CA3*EL*m12squ&
  &ared*dAlpha3MSBarAlter()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) + (0.09375D0*EL*m12squared*SA3*dBetaMSBarAl&
  &ter()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) + (0.0625D0*EL*MH12*SA3*dBetaMSBarAlter()*DBLE(SA1**INT(3.D0))&
  &*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) + (0.03125D0*EL*MH22*SA3*dBetaMSBarAlter()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(MW&
  &*SB*SW*TB) - (0.1875D0*CA1*CA3*EL*m12squared*dBetaMSBarAlter()*DBLE(SB**INT(-3.D0)))/(MW*SW) - (0.1875D0*CA1*CA22*CA3*EL*m12sq&
  &uared*dBetaMSBarAlter()*DBLE(SB**INT(-3.D0)))/(MW*SW) - (1.125D0*CA1*CA3*EL*m12squared*SA12*dBetaMSBarAlter()*DBLE(SB**INT(-3.&
  &D0)))/(MW*SW) - (1.125D0*CA1*CA22*CA3*EL*m12squared*SA12*dBetaMSBarAlter()*DBLE(SB**INT(-3.D0)))/(MW*SW) + (0.1875D0*CA1*CA3*E&
  &L*m12squared*SA22*dBetaMSBarAlter()*DBLE(SB**INT(-3.D0)))/(MW*SW) + (1.125D0*CA1*CA3*EL*m12squared*SA12*SA22*dBetaMSBarAlter()&
  &*DBLE(SB**INT(-3.D0)))/(MW*SW) + (0.46875D0*EL*m12squared*SA1*SA2*SA3*dBetaMSBarAlter()*DBLE(SB**INT(-3.D0)))/(MW*SW) - (0.562&
  &5D0*CA12*EL*m12squared*SA1*SA2*SA3*dBetaMSBarAlter()*DBLE(SB**INT(-3.D0)))/(MW*SW) + (1.40625D0*CA22*EL*m12squared*SA1*SA2*SA3&
  &*dBetaMSBarAlter()*DBLE(SB**INT(-3.D0)))/(MW*SW) - (1.6875D0*CA12*CA22*EL*m12squared*SA1*SA2*SA3*dBetaMSBarAlter()*DBLE(SB**IN&
  &T(-3.D0)))/(MW*SW) + (0.375D0*CA3*EL*m12squared*dBetaMSBarAlter()*DBLE(CA1**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(MW*SW) + (0.375D&
  &0*CA22*CA3*EL*m12squared*dBetaMSBarAlter()*DBLE(CA1**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(MW*SW) - (0.375D0*CA3*EL*m12squared*SA2&
  &2*dBetaMSBarAlter()*DBLE(CA1**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(MW*SW) + (0.1875D0*EL*m12squared*SA2*SA3*dBetaMSBarAlter()*DBL&
  &E(SA1**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(MW*SW) + (0.5625D0*CA22*EL*m12squared*SA2*SA3*dBetaMSBarAlter()*DBLE(SA1**INT(3.D0))*&
  &DBLE(SB**INT(-3.D0)))/(MW*SW) - (0.46875D0*EL*m12squared*SA1*SA3*dBetaMSBarAlter()*DBLE(SA2**INT(3.D0))*DBLE(SB**INT(-3.D0)))/&
  &(MW*SW) + (0.5625D0*CA12*EL*m12squared*SA1*SA3*dBetaMSBarAlter()*DBLE(SA2**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(MW*SW) - (0.1875D&
  &0*EL*m12squared*SA3*dBetaMSBarAlter()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(MW*SW) - (0.125D0*CA1*C&
  &A3*m12squared*dgAtMZ())/(CB*MW) - (0.125D0*CA1*CA22*CA3*m12squared*dgAtMZ())/(CB*MW) + (0.125D0*CA3*MH12*SA1*dgAtMZ())/(CB*MW)&
  & + (0.375D0*CA12*CA3*MH12*SA1*dgAtMZ())/(CB*MW) + (0.125D0*CA22*CA3*MH12*SA1*dgAtMZ())/(CB*MW) + (0.375D0*CA12*CA22*CA3*MH12*S&
  &A1*dgAtMZ())/(CB*MW) + (0.0625D0*CA3*MH22*SA1*dgAtMZ())/(CB*MW) + (0.1875D0*CA12*CA3*MH22*SA1*dgAtMZ())/(CB*MW) + (0.0625D0*CA&
  &22*CA3*MH22*SA1*dgAtMZ())/(CB*MW) + (0.1875D0*CA12*CA22*CA3*MH22*SA1*dgAtMZ())/(CB*MW) + (0.125D0*CA1*CA3*m12squared*SA22*dgAt&
  &MZ())/(CB*MW) - (0.125D0*CA3*MH12*SA1*SA22*dgAtMZ())/(CB*MW) - (0.375D0*CA12*CA3*MH12*SA1*SA22*dgAtMZ())/(CB*MW) - (0.0625D0*C&
  &A3*MH22*SA1*SA22*dgAtMZ())/(CB*MW) - (0.1875D0*CA12*CA3*MH22*SA1*SA22*dgAtMZ())/(CB*MW) + (0.1875D0*CA1*MH12*SA2*SA3*dgAtMZ())&
  &/(CB*MW) + (0.5625D0*CA1*CA22*MH12*SA2*SA3*dgAtMZ())/(CB*MW) + (0.09375D0*CA1*MH22*SA2*SA3*dgAtMZ())/(CB*MW) + (0.28125D0*CA1*&
  &CA22*MH22*SA2*SA3*dgAtMZ())/(CB*MW) + (0.28125D0*m12squared*SA1*SA2*SA3*dgAtMZ())/(CB*MW) + (0.84375D0*CA22*m12squared*SA1*SA2&
  &*SA3*dgAtMZ())/(CB*MW) - (0.1875D0*CA1*MH12*SA12*SA2*SA3*dgAtMZ())/(CB*MW) - (0.5625D0*CA1*CA22*MH12*SA12*SA2*SA3*dgAtMZ())/(C&
  &B*MW) - (0.09375D0*CA1*MH22*SA12*SA2*SA3*dgAtMZ())/(CB*MW) - (0.28125D0*CA1*CA22*MH22*SA12*SA2*SA3*dgAtMZ())/(CB*MW) - (0.125D&
  &0*CA1*CA3*MH12*dgAtMZ())/(MW*SB) - (0.125D0*CA1*CA22*CA3*MH12*dgAtMZ())/(MW*SB) - (0.0625D0*CA1*CA3*MH22*dgAtMZ())/(MW*SB) - (&
  &0.0625D0*CA1*CA22*CA3*MH22*dgAtMZ())/(MW*SB) + (0.21875D0*CA3*m12squared*SA1*dgAtMZ())/(MW*SB) + (0.21875D0*CA22*CA3*m12square&
  &d*SA1*dgAtMZ())/(MW*SB) - (0.15625D0*CA3*m12squared*SA1*dgAtMZ())/(CB2*MW*SB) - (0.5625D0*CA12*CA3*m12squared*SA1*dgAtMZ())/(C&
  &B2*MW*SB) - (0.15625D0*CA22*CA3*m12squared*SA1*dgAtMZ())/(CB2*MW*SB) - (0.5625D0*CA12*CA22*CA3*m12squared*SA1*dgAtMZ())/(CB2*M&
  &W*SB) - (0.375D0*CA1*CA3*MH12*SA12*dgAtMZ())/(MW*SB) - (0.375D0*CA1*CA22*CA3*MH12*SA12*dgAtMZ())/(MW*SB) - (0.1875D0*CA1*CA3*M&
  &H22*SA12*dgAtMZ())/(MW*SB) - (0.1875D0*CA1*CA22*CA3*MH22*SA12*dgAtMZ())/(MW*SB) + (0.125D0*CA1*CA3*MH12*SA22*dgAtMZ())/(MW*SB)&
  & + (0.0625D0*CA1*CA3*MH22*SA22*dgAtMZ())/(MW*SB) - (0.21875D0*CA3*m12squared*SA1*SA22*dgAtMZ())/(MW*SB) + (0.15625D0*CA3*m12sq&
  &uared*SA1*SA22*dgAtMZ())/(CB2*MW*SB) + (0.5625D0*CA12*CA3*m12squared*SA1*SA22*dgAtMZ())/(CB2*MW*SB) + (0.375D0*CA1*CA3*MH12*SA&
  &12*SA22*dgAtMZ())/(MW*SB) + (0.1875D0*CA1*CA3*MH22*SA12*SA22*dgAtMZ())/(MW*SB) + (0.28125D0*CA1*m12squared*SA2*SA3*dgAtMZ())/(&
  &MW*SB) + (0.84375D0*CA1*CA22*m12squared*SA2*SA3*dgAtMZ())/(MW*SB) - (0.1875D0*CA1*m12squared*SA2*SA3*dgAtMZ())/(CB2*MW*SB) - (&
  &0.5625D0*CA1*CA22*m12squared*SA2*SA3*dgAtMZ())/(CB2*MW*SB) + (0.1875D0*MH12*SA1*SA2*SA3*dgAtMZ())/(MW*SB) - (0.1875D0*CA12*MH1&
  &2*SA1*SA2*SA3*dgAtMZ())/(MW*SB) + (0.5625D0*CA22*MH12*SA1*SA2*SA3*dgAtMZ())/(MW*SB) - (0.5625D0*CA12*CA22*MH12*SA1*SA2*SA3*dgA&
  &tMZ())/(MW*SB) + (0.09375D0*MH22*SA1*SA2*SA3*dgAtMZ())/(MW*SB) - (0.09375D0*CA12*MH22*SA1*SA2*SA3*dgAtMZ())/(MW*SB) + (0.28125&
  &D0*CA22*MH22*SA1*SA2*SA3*dgAtMZ())/(MW*SB) - (0.28125D0*CA12*CA22*MH22*SA1*SA2*SA3*dgAtMZ())/(MW*SB) + (0.28125D0*CA1*m12squar&
  &ed*SA12*SA2*SA3*dgAtMZ())/(CB2*MW*SB) + (0.84375D0*CA1*CA22*m12squared*SA12*SA2*SA3*dgAtMZ())/(CB2*MW*SB) + (0.0625D0*CA1*CA3*&
  &m12squared*dgAtMZ())/(CB*MW*SB2) + (0.0625D0*CA1*CA22*CA3*m12squared*dgAtMZ())/(CB*MW*SB2) + (0.5625D0*CA1*CA3*m12squared*SA12&
  &*dgAtMZ())/(CB*MW*SB2) + (0.5625D0*CA1*CA22*CA3*m12squared*SA12*dgAtMZ())/(CB*MW*SB2) - (0.0625D0*CA1*CA3*m12squared*SA22*dgAt&
  &MZ())/(CB*MW*SB2) - (0.5625D0*CA1*CA3*m12squared*SA12*SA22*dgAtMZ())/(CB*MW*SB2) - (0.1875D0*m12squared*SA1*SA2*SA3*dgAtMZ())/&
  &(CB*MW*SB2) + (0.28125D0*CA12*m12squared*SA1*SA2*SA3*dgAtMZ())/(CB*MW*SB2) - (0.5625D0*CA22*m12squared*SA1*SA2*SA3*dgAtMZ())/(&
  &CB*MW*SB2) + (0.84375D0*CA12*CA22*m12squared*SA1*SA2*SA3*dgAtMZ())/(CB*MW*SB2) + (0.125D0*CA1*CA3*m12squared*dgAtMZ())/(MW*SB*&
  &TB) + (0.125D0*CA1*CA22*CA3*m12squared*dgAtMZ())/(MW*SB*TB) - (0.125D0*CA1*CA3*m12squared*SA22*dgAtMZ())/(MW*SB*TB) - (0.09375&
  &D0*m12squared*SA1*SA2*SA3*dgAtMZ())/(MW*SB*TB) - (0.28125D0*CA22*m12squared*SA1*SA2*SA3*dgAtMZ())/(MW*SB*TB) - (0.03125D0*CA3*&
  &m12squared*SA1*TB*dgAtMZ())/(CB*MW) - (0.03125D0*CA22*CA3*m12squared*SA1*TB*dgAtMZ())/(CB*MW) + (0.03125D0*CA3*m12squared*SA1*&
  &SA22*TB*dgAtMZ())/(CB*MW) - (0.09375D0*CA1*m12squared*SA2*SA3*TB*dgAtMZ())/(CB*MW) - (0.28125D0*CA1*CA22*m12squared*SA2*SA3*TB&
  &*dgAtMZ())/(CB*MW) + (0.0625D0*MH12*SA2*SA3*DBLE(CA1**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.1875D0*CA22*MH12*SA2*SA3*DBLE(CA1**INT&
  &(3.D0))*dgAtMZ())/(CB*MW) + (0.03125D0*MH22*SA2*SA3*DBLE(CA1**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.09375D0*CA22*MH22*SA2*SA3*DBLE&
  &(CA1**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.125D0*CA3*MH12*DBLE(CA1**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.125D0*CA22*CA3*MH12*DBLE(CA&
  &1**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.0625D0*CA3*MH22*DBLE(CA1**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.0625D0*CA22*CA3*MH22*DBLE(CA1&
  &**INT(3.D0))*dgAtMZ())/(MW*SB) - (0.125D0*CA3*MH12*SA22*DBLE(CA1**INT(3.D0))*dgAtMZ())/(MW*SB) - (0.0625D0*CA3*MH22*SA22*DBLE(&
  &CA1**INT(3.D0))*dgAtMZ())/(MW*SB) - (0.09375D0*m12squared*SA2*SA3*DBLE(CA1**INT(3.D0))*dgAtMZ())/(CB2*MW*SB) - (0.28125D0*CA22&
  &*m12squared*SA2*SA3*DBLE(CA1**INT(3.D0))*dgAtMZ())/(CB2*MW*SB) - (0.1875D0*CA3*m12squared*DBLE(CA1**INT(3.D0))*dgAtMZ())/(CB*M&
  &W*SB2) - (0.1875D0*CA22*CA3*m12squared*DBLE(CA1**INT(3.D0))*dgAtMZ())/(CB*MW*SB2) + (0.1875D0*CA3*m12squared*SA22*DBLE(CA1**IN&
  &T(3.D0))*dgAtMZ())/(CB*MW*SB2) - (0.125D0*CA3*MH12*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB*MW) - (0.125D0*CA22*CA3*MH12*DBLE(SA1**I&
  &NT(3.D0))*dgAtMZ())/(CB*MW) - (0.0625D0*CA3*MH22*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB*MW) - (0.0625D0*CA22*CA3*MH22*DBLE(SA1**IN&
  &T(3.D0))*dgAtMZ())/(CB*MW) + (0.125D0*CA3*MH12*SA22*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.0625D0*CA3*MH22*SA22*DBLE(SA1*&
  &*INT(3.D0))*dgAtMZ())/(CB*MW) + (0.1875D0*CA3*m12squared*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB2*MW*SB) + (0.1875D0*CA22*CA3*m12sq&
  &uared*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB2*MW*SB) - (0.1875D0*CA3*m12squared*SA22*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB2*MW*SB) + &
  &(0.0625D0*MH12*SA2*SA3*DBLE(SA1**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.1875D0*CA22*MH12*SA2*SA3*DBLE(SA1**INT(3.D0))*dgAtMZ())/(MW&
  &*SB) + (0.03125D0*MH22*SA2*SA3*DBLE(SA1**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.09375D0*CA22*MH22*SA2*SA3*DBLE(SA1**INT(3.D0))*dgAt&
  &MZ())/(MW*SB) - (0.09375D0*m12squared*SA2*SA3*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB*MW*SB2) - (0.28125D0*CA22*m12squared*SA2*SA3*&
  &DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB*MW*SB2) - (0.1875D0*CA1*MH12*SA3*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW) - (0.09375D0*CA1*MH&
  &22*SA3*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW) - (0.28125D0*m12squared*SA1*SA3*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.1875&
  &D0*CA1*MH12*SA12*SA3*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.09375D0*CA1*MH22*SA12*SA3*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*&
  &MW) - (0.28125D0*CA1*m12squared*SA3*DBLE(SA2**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.1875D0*CA1*m12squared*SA3*DBLE(SA2**INT(3.D0))&
  &*dgAtMZ())/(CB2*MW*SB) - (0.1875D0*MH12*SA1*SA3*DBLE(SA2**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.1875D0*CA12*MH12*SA1*SA3*DBLE(SA2*&
  &*INT(3.D0))*dgAtMZ())/(MW*SB) - (0.09375D0*MH22*SA1*SA3*DBLE(SA2**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.09375D0*CA12*MH22*SA1*SA3*&
  &DBLE(SA2**INT(3.D0))*dgAtMZ())/(MW*SB) - (0.28125D0*CA1*m12squared*SA12*SA3*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB2*MW*SB) + (0.18&
  &75D0*m12squared*SA1*SA3*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW*SB2) - (0.28125D0*CA12*m12squared*SA1*SA3*DBLE(SA2**INT(3.D0))*d&
  &gAtMZ())/(CB*MW*SB2) + (0.09375D0*m12squared*SA1*SA3*DBLE(SA2**INT(3.D0))*dgAtMZ())/(MW*SB*TB) + (0.09375D0*CA1*m12squared*SA3&
  &*TB*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW) - (0.0625D0*MH12*SA3*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW) - &
  &(0.03125D0*MH22*SA3*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.09375D0*m12squared*SA3*DBLE(CA1**INT(3.D0&
  &))*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB2*MW*SB) - (0.0625D0*MH12*SA3*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dgAtMZ())/(MW*SB)&
  & - (0.03125D0*MH22*SA3*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.09375D0*m12squared*SA3*DBLE(SA1**INT(3&
  &.D0))*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW*SB2) - (0.125D0*CA1*CA3*EL*dm122MSBarAlter())/(CB*MW*SW) - (0.125D0*CA1*CA22*CA3*E&
  &L*dm122MSBarAlter())/(CB*MW*SW) + (0.125D0*CA1*CA3*EL*SA22*dm122MSBarAlter())/(CB*MW*SW) + (0.28125D0*EL*SA1*SA2*SA3*dm122MSBa&
  &rAlter())/(CB*MW*SW) + (0.84375D0*CA22*EL*SA1*SA2*SA3*dm122MSBarAlter())/(CB*MW*SW) + (0.21875D0*CA3*EL*SA1*dm122MSBarAlter())&
  &/(MW*SB*SW) + (0.21875D0*CA22*CA3*EL*SA1*dm122MSBarAlter())/(MW*SB*SW) - (0.15625D0*CA3*EL*SA1*dm122MSBarAlter())/(CB2*MW*SB*S&
  &W) - (0.5625D0*CA12*CA3*EL*SA1*dm122MSBarAlter())/(CB2*MW*SB*SW) - (0.15625D0*CA22*CA3*EL*SA1*dm122MSBarAlter())/(CB2*MW*SB*SW&
  &) - (0.5625D0*CA12*CA22*CA3*EL*SA1*dm122MSBarAlter())/(CB2*MW*SB*SW) - (0.21875D0*CA3*EL*SA1*SA22*dm122MSBarAlter())/(MW*SB*SW&
  &) + (0.15625D0*CA3*EL*SA1*SA22*dm122MSBarAlter())/(CB2*MW*SB*SW) + (0.5625D0*CA12*CA3*EL*SA1*SA22*dm122MSBarAlter())/(CB2*MW*S&
  &B*SW) + (0.28125D0*CA1*EL*SA2*SA3*dm122MSBarAlter())/(MW*SB*SW) + (0.84375D0*CA1*CA22*EL*SA2*SA3*dm122MSBarAlter())/(MW*SB*SW)&
  & - (0.1875D0*CA1*EL*SA2*SA3*dm122MSBarAlter())/(CB2*MW*SB*SW) - (0.5625D0*CA1*CA22*EL*SA2*SA3*dm122MSBarAlter())/(CB2*MW*SB*SW&
  &) + (0.28125D0*CA1*EL*SA12*SA2*SA3*dm122MSBarAlter())/(CB2*MW*SB*SW) + (0.84375D0*CA1*CA22*EL*SA12*SA2*SA3*dm122MSBarAlter())/&
  &(CB2*MW*SB*SW) + (0.0625D0*CA1*CA3*EL*dm122MSBarAlter())/(CB*MW*SB2*SW) + (0.0625D0*CA1*CA22*CA3*EL*dm122MSBarAlter())/(CB*MW*&
  &SB2*SW) + (0.5625D0*CA1*CA3*EL*SA12*dm122MSBarAlter())/(CB*MW*SB2*SW) + (0.5625D0*CA1*CA22*CA3*EL*SA12*dm122MSBarAlter())/(CB*&
  &MW*SB2*SW) - (0.0625D0*CA1*CA3*EL*SA22*dm122MSBarAlter())/(CB*MW*SB2*SW) - (0.5625D0*CA1*CA3*EL*SA12*SA22*dm122MSBarAlter())/(&
  &CB*MW*SB2*SW) - (0.1875D0*EL*SA1*SA2*SA3*dm122MSBarAlter())/(CB*MW*SB2*SW) + (0.28125D0*CA12*EL*SA1*SA2*SA3*dm122MSBarAlter())&
  &/(CB*MW*SB2*SW) - (0.5625D0*CA22*EL*SA1*SA2*SA3*dm122MSBarAlter())/(CB*MW*SB2*SW) + (0.84375D0*CA12*CA22*EL*SA1*SA2*SA3*dm122M&
  &SBarAlter())/(CB*MW*SB2*SW) + (0.125D0*CA1*CA3*EL*dm122MSBarAlter())/(MW*SB*SW*TB) + (0.125D0*CA1*CA22*CA3*EL*dm122MSBarAlter(&
  &))/(MW*SB*SW*TB) - (0.125D0*CA1*CA3*EL*SA22*dm122MSBarAlter())/(MW*SB*SW*TB) - (0.09375D0*EL*SA1*SA2*SA3*dm122MSBarAlter())/(M&
  &W*SB*SW*TB) - (0.28125D0*CA22*EL*SA1*SA2*SA3*dm122MSBarAlter())/(MW*SB*SW*TB) - (0.03125D0*CA3*EL*SA1*TB*dm122MSBarAlter())/(C&
  &B*MW*SW) - (0.03125D0*CA22*CA3*EL*SA1*TB*dm122MSBarAlter())/(CB*MW*SW) + (0.03125D0*CA3*EL*SA1*SA22*TB*dm122MSBarAlter())/(CB*&
  &MW*SW) - (0.09375D0*CA1*EL*SA2*SA3*TB*dm122MSBarAlter())/(CB*MW*SW) - (0.28125D0*CA1*CA22*EL*SA2*SA3*TB*dm122MSBarAlter())/(CB&
  &*MW*SW) - (0.09375D0*EL*SA2*SA3*DBLE(CA1**INT(3.D0))*dm122MSBarAlter())/(CB2*MW*SB*SW) - (0.28125D0*CA22*EL*SA2*SA3*DBLE(CA1**&
  &INT(3.D0))*dm122MSBarAlter())/(CB2*MW*SB*SW) - (0.1875D0*CA3*EL*DBLE(CA1**INT(3.D0))*dm122MSBarAlter())/(CB*MW*SB2*SW) - (0.18&
  &75D0*CA22*CA3*EL*DBLE(CA1**INT(3.D0))*dm122MSBarAlter())/(CB*MW*SB2*SW) + (0.1875D0*CA3*EL*SA22*DBLE(CA1**INT(3.D0))*dm122MSBa&
  &rAlter())/(CB*MW*SB2*SW) + (0.1875D0*CA3*EL*DBLE(SA1**INT(3.D0))*dm122MSBarAlter())/(CB2*MW*SB*SW) + (0.1875D0*CA22*CA3*EL*DBL&
  &E(SA1**INT(3.D0))*dm122MSBarAlter())/(CB2*MW*SB*SW) - (0.1875D0*CA3*EL*SA22*DBLE(SA1**INT(3.D0))*dm122MSBarAlter())/(CB2*MW*SB&
  &*SW) - (0.09375D0*EL*SA2*SA3*DBLE(SA1**INT(3.D0))*dm122MSBarAlter())/(CB*MW*SB2*SW) - (0.28125D0*CA22*EL*SA2*SA3*DBLE(SA1**INT&
  &(3.D0))*dm122MSBarAlter())/(CB*MW*SB2*SW) - (0.28125D0*EL*SA1*SA3*DBLE(SA2**INT(3.D0))*dm122MSBarAlter())/(CB*MW*SW) - (0.2812&
  &5D0*CA1*EL*SA3*DBLE(SA2**INT(3.D0))*dm122MSBarAlter())/(MW*SB*SW) + (0.1875D0*CA1*EL*SA3*DBLE(SA2**INT(3.D0))*dm122MSBarAlter(&
  &))/(CB2*MW*SB*SW) - (0.28125D0*CA1*EL*SA12*SA3*DBLE(SA2**INT(3.D0))*dm122MSBarAlter())/(CB2*MW*SB*SW) + (0.1875D0*EL*SA1*SA3*D&
  &BLE(SA2**INT(3.D0))*dm122MSBarAlter())/(CB*MW*SB2*SW) - (0.28125D0*CA12*EL*SA1*SA3*DBLE(SA2**INT(3.D0))*dm122MSBarAlter())/(CB&
  &*MW*SB2*SW) + (0.09375D0*EL*SA1*SA3*DBLE(SA2**INT(3.D0))*dm122MSBarAlter())/(MW*SB*SW*TB) + (0.09375D0*CA1*EL*SA3*TB*DBLE(SA2*&
  &*INT(3.D0))*dm122MSBarAlter())/(CB*MW*SW) + (0.09375D0*EL*SA3*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dm122MSBarAlter())/(CB&
  &2*MW*SB*SW) + (0.09375D0*EL*SA3*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dm122MSBarAlter())/(CB*MW*SB2*SW) + (0.125D0*CA3*EL*&
  &SA1*dMH12OSAlter())/(CB*MW*SW) + (0.375D0*CA12*CA3*EL*SA1*dMH12OSAlter())/(CB*MW*SW) + (0.125D0*CA22*CA3*EL*SA1*dMH12OSAlter()&
  &)/(CB*MW*SW) + (0.375D0*CA12*CA22*CA3*EL*SA1*dMH12OSAlter())/(CB*MW*SW) - (0.125D0*CA3*EL*SA1*SA22*dMH12OSAlter())/(CB*MW*SW) &
  &- (0.375D0*CA12*CA3*EL*SA1*SA22*dMH12OSAlter())/(CB*MW*SW) + (0.1875D0*CA1*EL*SA2*SA3*dMH12OSAlter())/(CB*MW*SW) + (0.5625D0*C&
  &A1*CA22*EL*SA2*SA3*dMH12OSAlter())/(CB*MW*SW) - (0.1875D0*CA1*EL*SA12*SA2*SA3*dMH12OSAlter())/(CB*MW*SW) - (0.5625D0*CA1*CA22*&
  &EL*SA12*SA2*SA3*dMH12OSAlter())/(CB*MW*SW) - (0.125D0*CA1*CA3*EL*dMH12OSAlter())/(MW*SB*SW) - (0.125D0*CA1*CA22*CA3*EL*dMH12OS&
  &Alter())/(MW*SB*SW) - (0.375D0*CA1*CA3*EL*SA12*dMH12OSAlter())/(MW*SB*SW) - (0.375D0*CA1*CA22*CA3*EL*SA12*dMH12OSAlter())/(MW*&
  &SB*SW) + (0.125D0*CA1*CA3*EL*SA22*dMH12OSAlter())/(MW*SB*SW) + (0.375D0*CA1*CA3*EL*SA12*SA22*dMH12OSAlter())/(MW*SB*SW) + (0.1&
  &875D0*EL*SA1*SA2*SA3*dMH12OSAlter())/(MW*SB*SW) - (0.1875D0*CA12*EL*SA1*SA2*SA3*dMH12OSAlter())/(MW*SB*SW) + (0.5625D0*CA22*EL&
  &*SA1*SA2*SA3*dMH12OSAlter())/(MW*SB*SW) - (0.5625D0*CA12*CA22*EL*SA1*SA2*SA3*dMH12OSAlter())/(MW*SB*SW) - (0.5D0*CA2*SA3*dMH12&
  &OSAlter())/vS - (1.5D0*CA2*SA22*SA3*dMH12OSAlter())/vS + (0.0625D0*EL*SA2*SA3*DBLE(CA1**INT(3.D0))*dMH12OSAlter())/(CB*MW*SW) &
  &+ (0.1875D0*CA22*EL*SA2*SA3*DBLE(CA1**INT(3.D0))*dMH12OSAlter())/(CB*MW*SW) + (0.125D0*CA3*EL*DBLE(CA1**INT(3.D0))*dMH12OSAlte&
  &r())/(MW*SB*SW) + (0.125D0*CA22*CA3*EL*DBLE(CA1**INT(3.D0))*dMH12OSAlter())/(MW*SB*SW) - (0.125D0*CA3*EL*SA22*DBLE(CA1**INT(3.&
  &D0))*dMH12OSAlter())/(MW*SB*SW) + (0.5D0*SA3*DBLE(CA2**INT(3.D0))*dMH12OSAlter())/vS - (0.125D0*CA3*EL*DBLE(SA1**INT(3.D0))*dM&
  &H12OSAlter())/(CB*MW*SW) - (0.125D0*CA22*CA3*EL*DBLE(SA1**INT(3.D0))*dMH12OSAlter())/(CB*MW*SW) + (0.125D0*CA3*EL*SA22*DBLE(SA&
  &1**INT(3.D0))*dMH12OSAlter())/(CB*MW*SW) + (0.0625D0*EL*SA2*SA3*DBLE(SA1**INT(3.D0))*dMH12OSAlter())/(MW*SB*SW) + (0.1875D0*CA&
  &22*EL*SA2*SA3*DBLE(SA1**INT(3.D0))*dMH12OSAlter())/(MW*SB*SW) - (0.1875D0*CA1*EL*SA3*DBLE(SA2**INT(3.D0))*dMH12OSAlter())/(CB*&
  &MW*SW) + (0.1875D0*CA1*EL*SA12*SA3*DBLE(SA2**INT(3.D0))*dMH12OSAlter())/(CB*MW*SW) - (0.1875D0*EL*SA1*SA3*DBLE(SA2**INT(3.D0))&
  &*dMH12OSAlter())/(MW*SB*SW) + (0.1875D0*CA12*EL*SA1*SA3*DBLE(SA2**INT(3.D0))*dMH12OSAlter())/(MW*SB*SW) - (0.0625D0*EL*SA3*DBL&
  &E(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dMH12OSAlter())/(CB*MW*SW) - (0.0625D0*EL*SA3*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))&
  &*dMH12OSAlter())/(MW*SB*SW) + (0.0625D0*CA3*EL*SA1*dMH22OSAlter())/(CB*MW*SW) + (0.1875D0*CA12*CA3*EL*SA1*dMH22OSAlter())/(CB*&
  &MW*SW) + (0.0625D0*CA22*CA3*EL*SA1*dMH22OSAlter())/(CB*MW*SW) + (0.1875D0*CA12*CA22*CA3*EL*SA1*dMH22OSAlter())/(CB*MW*SW) - (0&
  &.0625D0*CA3*EL*SA1*SA22*dMH22OSAlter())/(CB*MW*SW) - (0.1875D0*CA12*CA3*EL*SA1*SA22*dMH22OSAlter())/(CB*MW*SW) + (0.09375D0*CA&
  &1*EL*SA2*SA3*dMH22OSAlter())/(CB*MW*SW) + (0.28125D0*CA1*CA22*EL*SA2*SA3*dMH22OSAlter())/(CB*MW*SW) - (0.09375D0*CA1*EL*SA12*S&
  &A2*SA3*dMH22OSAlter())/(CB*MW*SW) - (0.28125D0*CA1*CA22*EL*SA12*SA2*SA3*dMH22OSAlter())/(CB*MW*SW) - (0.0625D0*CA1*CA3*EL*dMH2&
  &2OSAlter())/(MW*SB*SW) - (0.0625D0*CA1*CA22*CA3*EL*dMH22OSAlter())/(MW*SB*SW) - (0.1875D0*CA1*CA3*EL*SA12*dMH22OSAlter())/(MW*&
  &SB*SW) - (0.1875D0*CA1*CA22*CA3*EL*SA12*dMH22OSAlter())/(MW*SB*SW) + (0.0625D0*CA1*CA3*EL*SA22*dMH22OSAlter())/(MW*SB*SW) + (0&
  &.1875D0*CA1*CA3*EL*SA12*SA22*dMH22OSAlter())/(MW*SB*SW) + (0.09375D0*EL*SA1*SA2*SA3*dMH22OSAlter())/(MW*SB*SW) - (0.09375D0*CA&
  &12*EL*SA1*SA2*SA3*dMH22OSAlter())/(MW*SB*SW) + (0.28125D0*CA22*EL*SA1*SA2*SA3*dMH22OSAlter())/(MW*SB*SW) - (0.28125D0*CA12*CA2&
  &2*EL*SA1*SA2*SA3*dMH22OSAlter())/(MW*SB*SW) - (0.25D0*CA2*SA3*dMH22OSAlter())/vS - (0.75D0*CA2*SA22*SA3*dMH22OSAlter())/vS + (&
  &0.03125D0*EL*SA2*SA3*DBLE(CA1**INT(3.D0))*dMH22OSAlter())/(CB*MW*SW) + (0.09375D0*CA22*EL*SA2*SA3*DBLE(CA1**INT(3.D0))*dMH22OS&
  &Alter())/(CB*MW*SW) + (0.0625D0*CA3*EL*DBLE(CA1**INT(3.D0))*dMH22OSAlter())/(MW*SB*SW) + (0.0625D0*CA22*CA3*EL*DBLE(CA1**INT(3&
  &.D0))*dMH22OSAlter())/(MW*SB*SW) - (0.0625D0*CA3*EL*SA22*DBLE(CA1**INT(3.D0))*dMH22OSAlter())/(MW*SB*SW) + (0.25D0*SA3*DBLE(CA&
  &2**INT(3.D0))*dMH22OSAlter())/vS - (0.0625D0*CA3*EL*DBLE(SA1**INT(3.D0))*dMH22OSAlter())/(CB*MW*SW) - (0.0625D0*CA22*CA3*EL*DB&
  &LE(SA1**INT(3.D0))*dMH22OSAlter())/(CB*MW*SW) + (0.0625D0*CA3*EL*SA22*DBLE(SA1**INT(3.D0))*dMH22OSAlter())/(CB*MW*SW) + (0.031&
  &25D0*EL*SA2*SA3*DBLE(SA1**INT(3.D0))*dMH22OSAlter())/(MW*SB*SW) + (0.09375D0*CA22*EL*SA2*SA3*DBLE(SA1**INT(3.D0))*dMH22OSAlter&
  &())/(MW*SB*SW) - (0.09375D0*CA1*EL*SA3*DBLE(SA2**INT(3.D0))*dMH22OSAlter())/(CB*MW*SW) + (0.09375D0*CA1*EL*SA12*SA3*DBLE(SA2**&
  &INT(3.D0))*dMH22OSAlter())/(CB*MW*SW) - (0.09375D0*EL*SA1*SA3*DBLE(SA2**INT(3.D0))*dMH22OSAlter())/(MW*SB*SW) + (0.09375D0*CA1&
  &2*EL*SA1*SA3*DBLE(SA2**INT(3.D0))*dMH22OSAlter())/(MW*SB*SW) - (0.03125D0*EL*SA3*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dMH&
  &22OSAlter())/(CB*MW*SW) - (0.03125D0*EL*SA3*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dMH22OSAlter())/(MW*SB*SW) + (0.0625D0*C&
  &A1*CA3*EL*m12squared*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.0625D0*CA1*CA22*CA3*EL*m12squared*DBLE(MW**INT(-3.D0))*dMW&
  &2Alter())/(CB*SW) - (0.0625D0*CA3*EL*MH12*SA1*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.1875D0*CA12*CA3*EL*MH12*SA1*DBLE(&
  &MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.0625D0*CA22*CA3*EL*MH12*SA1*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.1875D0*CA&
  &12*CA22*CA3*EL*MH12*SA1*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.03125D0*CA3*EL*MH22*SA1*DBLE(MW**INT(-3.D0))*dMW2Alter(&
  &))/(CB*SW) - (0.09375D0*CA12*CA3*EL*MH22*SA1*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.03125D0*CA22*CA3*EL*MH22*SA1*DBLE(&
  &MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.09375D0*CA12*CA22*CA3*EL*MH22*SA1*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.062&
  &5D0*CA1*CA3*EL*m12squared*SA22*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.0625D0*CA3*EL*MH12*SA1*SA22*DBLE(MW**INT(-3.D0))&
  &*dMW2Alter())/(CB*SW) + (0.1875D0*CA12*CA3*EL*MH12*SA1*SA22*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.03125D0*CA3*EL*MH22&
  &*SA1*SA22*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.09375D0*CA12*CA3*EL*MH22*SA1*SA22*DBLE(MW**INT(-3.D0))*dMW2Alter())/(&
  &CB*SW) - (0.09375D0*CA1*EL*MH12*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.28125D0*CA1*CA22*EL*MH12*SA2*SA3*DBLE(M&
  &W**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.046875D0*CA1*EL*MH22*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.140625D0*&
  &CA1*CA22*EL*MH22*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.140625D0*EL*m12squared*SA1*SA2*SA3*DBLE(MW**INT(-3.D0)&
  &)*dMW2Alter())/(CB*SW) - (0.421875D0*CA22*EL*m12squared*SA1*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.09375D0*CA1&
  &*EL*MH12*SA12*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.28125D0*CA1*CA22*EL*MH12*SA12*SA2*SA3*DBLE(MW**INT(-3.D0)&
  &)*dMW2Alter())/(CB*SW) + (0.046875D0*CA1*EL*MH22*SA12*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.140625D0*CA1*CA22&
  &*EL*MH22*SA12*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.0625D0*CA1*CA3*EL*MH12*DBLE(MW**INT(-3.D0))*dMW2Alter())/&
  &(SB*SW) + (0.0625D0*CA1*CA22*CA3*EL*MH12*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.03125D0*CA1*CA3*EL*MH22*DBLE(MW**INT(-&
  &3.D0))*dMW2Alter())/(SB*SW) + (0.03125D0*CA1*CA22*CA3*EL*MH22*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.109375D0*CA3*EL*m&
  &12squared*SA1*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.109375D0*CA22*CA3*EL*m12squared*SA1*DBLE(MW**INT(-3.D0))*dMW2Alte&
  &r())/(SB*SW) + (0.078125D0*CA3*EL*m12squared*SA1*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB2*SB*SW) + (0.28125D0*CA12*CA3*EL*m12squ&
  &ared*SA1*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB2*SB*SW) + (0.078125D0*CA22*CA3*EL*m12squared*SA1*DBLE(MW**INT(-3.D0))*dMW2Alter&
  &())/(CB2*SB*SW) + (0.28125D0*CA12*CA22*CA3*EL*m12squared*SA1*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB2*SB*SW) + (0.1875D0*CA1*CA3&
  &*EL*MH12*SA12*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.1875D0*CA1*CA22*CA3*EL*MH12*SA12*DBLE(MW**INT(-3.D0))*dMW2Alter()&
  &)/(SB*SW) + (0.09375D0*CA1*CA3*EL*MH22*SA12*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.09375D0*CA1*CA22*CA3*EL*MH22*SA12*D&
  &BLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.0625D0*CA1*CA3*EL*MH12*SA22*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.03125&
  &D0*CA1*CA3*EL*MH22*SA22*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.109375D0*CA3*EL*m12squared*SA1*SA22*DBLE(MW**INT(-3.D0)&
  &)*dMW2Alter())/(SB*SW) - (0.078125D0*CA3*EL*m12squared*SA1*SA22*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB2*SB*SW) - (0.28125D0*CA1&
  &2*CA3*EL*m12squared*SA1*SA22*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB2*SB*SW) - (0.1875D0*CA1*CA3*EL*MH12*SA12*SA22*DBLE(MW**INT(&
  &-3.D0))*dMW2Alter())/(SB*SW) - (0.09375D0*CA1*CA3*EL*MH22*SA12*SA22*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.140625D0*CA&
  &1*EL*m12squared*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.421875D0*CA1*CA22*EL*m12squared*SA2*SA3*DBLE(MW**INT(-3&
  &.D0))*dMW2Alter())/(SB*SW) + (0.09375D0*CA1*EL*m12squared*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB2*SB*SW) + (0.28125D0*C&
  &A1*CA22*EL*m12squared*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB2*SB*SW) - (0.09375D0*EL*MH12*SA1*SA2*SA3*DBLE(MW**INT(-3.D&
  &0))*dMW2Alter())/(SB*SW) + (0.09375D0*CA12*EL*MH12*SA1*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.28125D0*CA22*EL*&
  &MH12*SA1*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.28125D0*CA12*CA22*EL*MH12*SA1*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW&
  &2Alter())/(SB*SW) - (0.046875D0*EL*MH22*SA1*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.046875D0*CA12*EL*MH22*SA1*S&
  &A2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.140625D0*CA22*EL*MH22*SA1*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*&
  &SW) + (0.140625D0*CA12*CA22*EL*MH22*SA1*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.140625D0*CA1*EL*m12squared*SA12&
  &*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB2*SB*SW) - (0.421875D0*CA1*CA22*EL*m12squared*SA12*SA2*SA3*DBLE(MW**INT(-3.D0))*&
  &dMW2Alter())/(CB2*SB*SW) - (0.03125D0*CA1*CA3*EL*m12squared*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SB2*SW) - (0.03125D0*CA1*CA2&
  &2*CA3*EL*m12squared*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SB2*SW) - (0.28125D0*CA1*CA3*EL*m12squared*SA12*DBLE(MW**INT(-3.D0))&
  &*dMW2Alter())/(CB*SB2*SW) - (0.28125D0*CA1*CA22*CA3*EL*m12squared*SA12*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SB2*SW) + (0.0312&
  &5D0*CA1*CA3*EL*m12squared*SA22*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SB2*SW) + (0.28125D0*CA1*CA3*EL*m12squared*SA12*SA22*DBLE&
  &(MW**INT(-3.D0))*dMW2Alter())/(CB*SB2*SW) + (0.09375D0*EL*m12squared*SA1*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SB2*SW)&
  & - (0.140625D0*CA12*EL*m12squared*SA1*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SB2*SW) + (0.28125D0*CA22*EL*m12squared*SA&
  &1*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SB2*SW) - (0.421875D0*CA12*CA22*EL*m12squared*SA1*SA2*SA3*DBLE(MW**INT(-3.D0))&
  &*dMW2Alter())/(CB*SB2*SW) - (0.0625D0*CA1*CA3*EL*m12squared*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW*TB) - (0.0625D0*CA1*CA22*&
  &CA3*EL*m12squared*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW*TB) + (0.0625D0*CA1*CA3*EL*m12squared*SA22*DBLE(MW**INT(-3.D0))*dMW&
  &2Alter())/(SB*SW*TB) + (0.046875D0*EL*m12squared*SA1*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW*TB) + (0.140625D0*CA22*E&
  &L*m12squared*SA1*SA2*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW*TB) + (0.015625D0*CA3*EL*m12squared*SA1*TB*DBLE(MW**INT(-3.D&
  &0))*dMW2Alter())/(CB*SW) + (0.015625D0*CA22*CA3*EL*m12squared*SA1*TB*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.015625D0*C&
  &A3*EL*m12squared*SA1*SA22*TB*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.046875D0*CA1*EL*m12squared*SA2*SA3*TB*DBLE(MW**INT&
  &(-3.D0))*dMW2Alter())/(CB*SW) + (0.140625D0*CA1*CA22*EL*m12squared*SA2*SA3*TB*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.0&
  &3125D0*EL*MH12*SA2*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.09375D0*CA22*EL*MH12*SA2*SA3*DBLE(C&
  &A1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.015625D0*EL*MH22*SA2*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0&
  &))*dMW2Alter())/(CB*SW) - (0.046875D0*CA22*EL*MH22*SA2*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0&
  &.0625D0*CA3*EL*MH12*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.0625D0*CA22*CA3*EL*MH12*DBLE(CA1**INT(&
  &3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.03125D0*CA3*EL*MH22*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter(&
  &))/(SB*SW) - (0.03125D0*CA22*CA3*EL*MH22*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.0625D0*CA3*EL*MH1&
  &2*SA22*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.03125D0*CA3*EL*MH22*SA22*DBLE(CA1**INT(3.D0))*DBLE(&
  &MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.046875D0*EL*m12squared*SA2*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter()&
  &)/(CB2*SB*SW) + (0.140625D0*CA22*EL*m12squared*SA2*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB2*SB*SW) + (0&
  &.09375D0*CA3*EL*m12squared*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SB2*SW) + (0.09375D0*CA22*CA3*EL*m12squa&
  &red*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SB2*SW) - (0.09375D0*CA3*EL*m12squared*SA22*DBLE(CA1**INT(3.D0)&
  &)*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SB2*SW) + (0.0625D0*CA3*EL*MH12*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())&
  &/(CB*SW) + (0.0625D0*CA22*CA3*EL*MH12*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(CB*SW) + (0.03125D0*CA3*EL*MH22*&
  &DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(CB*SW) + (0.03125D0*CA22*CA3*EL*MH22*DBLE(MW**INT(-3.D0))*DBLE(SA1**IN&
  &T(3.D0))*dMW2Alter())/(CB*SW) - (0.0625D0*CA3*EL*MH12*SA22*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(CB*SW) - (0&
  &.03125D0*CA3*EL*MH22*SA22*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(CB*SW) - (0.09375D0*CA3*EL*m12squared*DBLE(M&
  &W**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(CB2*SB*SW) - (0.09375D0*CA22*CA3*EL*m12squared*DBLE(MW**INT(-3.D0))*DBLE(SA1&
  &**INT(3.D0))*dMW2Alter())/(CB2*SB*SW) + (0.09375D0*CA3*EL*m12squared*SA22*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter(&
  &))/(CB2*SB*SW) - (0.03125D0*EL*MH12*SA2*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(SB*SW) - (0.09375D0*CA22*E&
  &L*MH12*SA2*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(SB*SW) - (0.015625D0*EL*MH22*SA2*SA3*DBLE(MW**INT(-3.D0&
  &))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(SB*SW) - (0.046875D0*CA22*EL*MH22*SA2*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2&
  &Alter())/(SB*SW) + (0.046875D0*EL*m12squared*SA2*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(CB*SB2*SW) + (0.1&
  &40625D0*CA22*EL*m12squared*SA2*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(CB*SB2*SW) + (0.09375D0*CA1*EL*MH12&
  &*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(CB*SW) + (0.046875D0*CA1*EL*MH22*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA&
  &2**INT(3.D0))*dMW2Alter())/(CB*SW) + (0.140625D0*EL*m12squared*SA1*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/&
  &(CB*SW) - (0.09375D0*CA1*EL*MH12*SA12*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(CB*SW) - (0.046875D0*CA1*EL*&
  &MH22*SA12*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(CB*SW) + (0.140625D0*CA1*EL*m12squared*SA3*DBLE(MW**INT(&
  &-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(SB*SW) - (0.09375D0*CA1*EL*m12squared*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))&
  &*dMW2Alter())/(CB2*SB*SW) + (0.09375D0*EL*MH12*SA1*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(SB*SW) - (0.093&
  &75D0*CA12*EL*MH12*SA1*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(SB*SW) + (0.046875D0*EL*MH22*SA1*SA3*DBLE(MW&
  &**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(SB*SW) - (0.046875D0*CA12*EL*MH22*SA1*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(&
  &3.D0))*dMW2Alter())/(SB*SW) + (0.140625D0*CA1*EL*m12squared*SA12*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(C&
  &B2*SB*SW) - (0.09375D0*EL*m12squared*SA1*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(CB*SB2*SW) + (0.140625D0*&
  &CA12*EL*m12squared*SA1*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(CB*SB2*SW) - (0.046875D0*EL*m12squared*SA1*&
  &SA3*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(SB*SW*TB) - (0.046875D0*CA1*EL*m12squared*SA3*TB*DBLE(MW**INT(-3.D&
  &0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(CB*SW) + (0.03125D0*EL*MH12*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT&
  &(3.D0))*dMW2Alter())/(CB*SW) + (0.015625D0*EL*MH22*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alte&
  &r())/(CB*SW) - (0.046875D0*EL*m12squared*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(CB2*&
  &SB*SW) + (0.03125D0*EL*MH12*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(SB*SW) + (0.01562&
  &5D0*EL*MH22*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(SB*SW) - (0.046875D0*EL*m12square&
  &d*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(CB*SB2*SW) + 0.5D0*CA2*MH12*SA3*DBLE(vS**IN&
  &T(-2.D0))*dvSMSBarAlter() + 0.25D0*CA2*MH22*SA3*DBLE(vS**INT(-2.D0))*dvSMSBarAlter() + 1.5D0*CA2*MH12*SA22*SA3*DBLE(vS**INT(-2&
  &.D0))*dvSMSBarAlter() + 0.75D0*CA2*MH22*SA22*SA3*DBLE(vS**INT(-2.D0))*dvSMSBarAlter() - 0.5D0*MH12*SA3*DBLE(CA2**INT(3.D0))*DB&
  &LE(vS**INT(-2.D0))*dvSMSBarAlter() - 0.25D0*MH22*SA3*DBLE(CA2**INT(3.D0))*DBLE(vS**INT(-2.D0))*dvSMSBarAlter() &
		& + CS1S1S1f111*dZH1H2OSAlter()/2D0 + CS1S1S1f211*dZH2H2OS()/2D0 + CS1S1S1f311*dZH3H2OSAlter()/2D0 &
		& + CS1S1S1f121*dZH1H1OS()/2D0 + CS1S1S1f221*dZH2H1OSAlter()/2D0 + CS1S1S1f321*dZH3H1OSAlter()/2D0 &
		& + CS1S1S1f121*dZH1H1OS()/2D0 + CS1S1S1f221*dZH2H1OSAlter()/2D0 + CS1S1S1f321*dZH3H1OSAlter()/2D0 &
		& )
	case default
		totalAmplitude = 0D0
 end select

 H2toH1H1CT = totalAmplitude
end function H2toH1H1CT
