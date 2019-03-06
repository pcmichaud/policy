! ******************************************************************************
! TIAA-CREF grant simulations
! Lusardi, Michaud and Mitchell
! computes results for figures and tables of paper
! *****************************************************************************

module simulate
	use sorting

	implicit none
	character*80 scenario
	character*80 path

	! parameters to set 
	double precision beta, gamma, pi0, pi1, cmin, k0, k1, phi, delta0, delta1, psi,eta, theta, omega, rhoe
	double precision amin, amax, fmin, fmax, inflation, rmax, rbar, sigr, alpha0, alpha1, sigrmax		
	integer T, ny, no, nr, ne, nd, nf, na, nsim, retage
	! switches for scenarios
	double precision issgen, ifin, pi0prog, cprog
	integer idifmx, idifeq, idifss, inofin, inotech, icagetti, igparker, izin, idprog, ageprog, idelta


       
	contains
		subroutine initialize(scn)
			character*80 scn
			integer i, e, s
			scenario = scn
			write(*,*) '+ running scenario : ', scenario
						
			! define baseline parameters
			
			! dimensions
			T = 75
			ny = 7
			no = 7
			nr = 7
			ne = 3
			nd = 2
			nf =  25
			na = 35
			nsim = 2500

			! other parameters
			amin = 0.0d0
			amax = 2.0d6
			fmin = 0.0d0
			fmax = 100.0d0
			inflation = 0.0d0
			rmax = 0.04d0
			rbar = 0.02d0
			sigr = 0.16d0
			retage = 40
			sigrmax = sigr
			rhoe = 0.0d0
						
			! preferences and technology
			beta = 0.96d0		
			gamma = 1.6d0
			pi0 = 50.0d0
			pi1 = 1.75d0
			cmin = 10000.0d0
			k0 = 750.0d0
			k1 = 0.0d0
			phi = 0.0d0
			delta0 = -0.06d0
			delta1 = 0.0d0
			alpha1 = 1.0d0
			alpha0 = rmax/(fmax**alpha1)
			psi = 0.0d0
			eta = gamma
			theta = (sigr - sigrmax)/fmax
			
			! other scenario specific parameters
			
			issgen = 1.0d0
			ifin = 0.0d0	
			idifmx = 1
			idifeq = 1
			idifss = 1			 
			inofin = 0
			inotech = 0
            icagetti = 0
            igparker = 0
            izin = 0
            idprog = 0
            ageprog = 25
            pi0prog = 1.0d0
            cprog = 0.0d0
            idelta = 1
                        
			! policy simulations
			if (scenario .eq. 'prog1') then
				idprog = 1
				ageprog = 5
				pi0prog = 0.5d0
				cprog = 500.0d0
			end if

			if (scenario .eq. 'prog2') then
				idprog = 1
				ageprog = 10
				pi0prog = 0.5d0
				cprog = 500.0d0
			end if

			if (scenario .eq. 'prog3') then
				idprog = 1
				ageprog = 15
				pi0prog = 0.5d0
				cprog = 500.0d0
			end if

			if (scenario .eq. 'prog4') then
				idprog = 1
				ageprog = 20
				pi0prog = 0.5d0
				cprog = 500.0d0
			end if

			if (scenario .eq. 'prog5') then
				idprog = 1
				ageprog = 25
				pi0prog = 0.5d0
				cprog = 500.0d0
			end if

			if (scenario .eq. 'prog6') then
				idprog = 1
				ageprog = 15
				pi0prog = 0.25d0
				cprog = 500.0d0
			end if

			if (scenario .eq. 'prog7') then
				idprog = 1
				ageprog = 15
				pi0prog = 0.75d0
				cprog = 500.0d0
			end if

			if (scenario .eq. 'prog8') then
				idprog = 1
				ageprog = 15
				pi0prog = 0.5d0
				cprog = 250.0d0
			end if

			if (scenario .eq. 'prog9') then
				idprog = 1
				ageprog = 15
				pi0prog = 0.5d0
				cprog = 750.0d0
			end if										

			if (scenario .eq. 'prog10') then
				idprog = 1
				ageprog = 15
				pi0prog = 0.25d0
				cprog = 250.0d0
			end if

			if (scenario .eq. 'prog11') then
				idprog = 1
				ageprog = 15
				pi0prog = 0.05d0
				cprog = 100.0d0
				idelta = 0
			end if
									    		
			! writing scenario parameters to file
			open(unit=1, file='../params/scenario-parameters.asc')		
			write(1,*) scenario	
			write(1,*) issgen
			write(1,*) ifin	
			write(1,*) idifmx 
			write(1,*) idifeq 
			write(1,*) idifss 	
			write(1,*) inofin
			write(1,*) inotech		
			write(1,*) icagetti
			write(1,*) igparker 
			write(1,*) izin
            write(1,*) idprog
            write(1,*) ageprog
            write(1,*) pi0prog	
            write(1,*) cprog
            write(1,*) idelta		
			close(1)
				 	
			! writing to file preference and technology parameters	
			open(unit=1, file='../params/structural-parameters.asc')
			write(1,*) beta
			write(1,*) gamma
			write(1,*) pi0
			write(1,*) pi1
			write(1,*) cmin
			write(1,*) k0
			write(1,*) k1
			write(1,*) phi
			write(1,*) delta0
			write(1,*) delta1
			write(1,*) alpha0
			write(1,*) alpha1	
			write(1,*) psi	
			write(1,*) eta
			write(1,*) theta
			close(unit=1)
			! writing other parameters 
			open(unit=1, file='../params/other-parameters.asc')
			write(1,*) amin
			write(1,*) amax
			write(1,*) fmin
			write(1,*) fmax
			write(1,*) inflation
			write(1,*) rmax
			write(1,*) rbar
			write(1,*) sigr
			write(1,*) retage
			write(1,*) sigrmax
			write(1,*) rhoe
			close(unit=1)
			! writing  dimensions 
			open(unit=1, file='../params/dimensions-parameters.asc')
			write(1,*) T
			write(1,*) ny
			write(1,*) no
			write(1,*) nr
			write(1,*) ne
			write(1,*) nd
			write(1,*) nf		
			write(1,*) na
			write(1,*) nsim
			close(unit=1)	
			
			
			if (scenario .eq. 'baseline') then
				call draws
			end if

		end subroutine initialize

		subroutine draws
			double precision rdraws(nsim,T), mdraws(nsim,T), &
					odraws(nsim,T), ydraws(nsim,T), u
			integer i, s
			
			do i = 1, nsim, 1
				do s = 1, T, 1
					call random_number(u)
					rdraws(i,s) = u
					call random_number(u)
					mdraws(i,s) = u
					call random_number(u)
					odraws(i,s) = u
					call random_number(u)
					ydraws(i,s) = u
				end do
			end do
			
			open(1,file='../data/simulations/rdraws.dat')
			do i = 1, nsim, 1
				write(1,*) rdraws(i,:)
			end do
			close(1)
			open(1,file='../data/simulations/odraws.dat')
			do i = 1, nsim, 1
				write(1,*) odraws(i,:)
			end do
			close(1)
			open(1,file='../data/simulations/ydraws.dat')
			do i = 1, nsim, 1
				write(1,*) ydraws(i,:)
			end do
			close(1)
			open(1,file='../data/simulations/mdraws.dat')
			do i = 1, nsim, 1
				write(1,*) mdraws(i,:)
			end do
			close(1)
								
					
		end subroutine draws

		
		subroutine runsim(scn)
			character*80 scn
			call initialize(scn)
			write(*,*) '	* running simulation ...'
			call system('mpirun -n 20 --oversubscribe ../runtime/solve')
		end subroutine runsim

							
end module simulate


	
