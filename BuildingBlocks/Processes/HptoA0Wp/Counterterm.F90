double precision function HptoA0WpCT(x)
 use constants
 use counterterms
 implicit none
#include "looptools.h"
 integer, intent(in) :: x
 double precision :: totalAmplitude

 select case (x)
	case (1)
        totalAmplitude = ( dgAtMZ()/(EL/SW) + dZA0A0OS()/2D0 + dZHpHpOS()/2D0 + dZWpWpOS()/2D0 ) * (EL2/SW2) * &
            & ( (MHp2**2 + MW2**2 + MA02**2 - 2D0*MHp2*MW2 - 2D0*MHp2*MA02 - 2D0*MW2*MA02)/(4D0*MW2) )
    case (2)
        totalAmplitude = ( dgAtMZ()/(EL/SW) + dZA0A0OS()/2D0 + dZHpHpOS()/2D0 + dZWpWpOS()/2D0 ) * (EL2/SW2) * &
            & ( (MHp2**2 + MW2**2 + MA02**2 - 2D0*MHp2*MW2 - 2D0*MHp2*MA02 - 2D0*MW2*MA02)/(4D0*MW2) )
    case (3)
        totalAmplitude = ( dgAtMZ()/(EL/SW) + dZA0A0OS()/2D0 + dZHpHpOS()/2D0 + dZWpWpOS()/2D0 ) * (EL2/SW2) * &
            & ( (MHp2**2 + MW2**2 + MA02**2 - 2D0*MHp2*MW2 - 2D0*MHp2*MA02 - 2D0*MW2*MA02)/(4D0*MW2) )
    case (4)
        totalAmplitude = ( dgAtMZ()/(EL/SW) + dZA0A0OS()/2D0 + dZHpHpOS()/2D0 + dZWpWpOS()/2D0 ) * (EL2/SW2) * &
            & ( (MHp2**2 + MW2**2 + MA02**2 - 2D0*MHp2*MW2 - 2D0*MHp2*MA02 - 2D0*MW2*MA02)/(4D0*MW2) )
    case (5)
        totalAmplitude = ( dgAtMZ()/(EL/SW) + dZA0A0OS()/2D0 + dZHpHpOS()/2D0 + dZWpWpOS()/2D0 ) * (EL2/SW2) * &
            & ( (MHp2**2 + MW2**2 + MA02**2 - 2D0*MHp2*MW2 - 2D0*MHp2*MA02 - 2D0*MW2*MA02)/(4D0*MW2) )
    case (6)
        totalAmplitude = ( dgAtMZ()/(EL/SW) + dZA0A0OS()/2D0 + dZHpHpOS()/2D0 + dZWpWpOS()/2D0 ) * (EL2/SW2) * &
            & ( (MHp2**2 + MW2**2 + MA02**2 - 2D0*MHp2*MW2 - 2D0*MHp2*MA02 - 2D0*MW2*MA02)/(4D0*MW2) )
    case (7)
        totalAmplitude = ( dgAtMZ()/(EL/SW) + dZA0A0OS()/2D0 + dZHpHpOS()/2D0 + dZWpWpOS()/2D0 ) * (EL2/SW2) * &
            & ( (MHp2**2 + MW2**2 + MA02**2 - 2D0*MHp2*MW2 - 2D0*MHp2*MA02 - 2D0*MW2*MA02)/(4D0*MW2) )
    case (8)
        totalAmplitude = ( dgAtMZ()/(EL/SW) + dZA0A0OS()/2D0 + dZHpHpOS()/2D0 + dZWpWpOS()/2D0 ) * (EL2/SW2) * &
            & ( (MHp2**2 + MW2**2 + MA02**2 - 2D0*MHp2*MW2 - 2D0*MHp2*MA02 - 2D0*MW2*MA02)/(4D0*MW2) )
    case (9)
        totalAmplitude = ( dgAtMZ()/(EL/SW) + dZA0A0OS()/2D0 + dZHpHpOS()/2D0 + dZWpWpOS()/2D0 ) * (EL2/SW2) * &
            & ( (MHp2**2 + MW2**2 + MA02**2 - 2D0*MHp2*MW2 - 2D0*MHp2*MA02 - 2D0*MW2*MA02)/(4D0*MW2) )
    case (10)
        totalAmplitude = ( dgAtMZ()/(EL/SW) + dZA0A0OS()/2D0 + dZHpHpOS()/2D0 + dZWpWpOS()/2D0 ) * (EL2/SW2) * &
            & ( (MHp2**2 + MW2**2 + MA02**2 - 2D0*MHp2*MW2 - 2D0*MHp2*MA02 - 2D0*MW2*MA02)/(4D0*MW2) )
	case default
		totalAmplitude = 0D0
 end select

 HptoA0WpCT = totalAmplitude
end function HptoA0WpCT
