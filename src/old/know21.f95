! optimal knowledge class for solving and simulating model
! version with MPI capabilities
! Lusardi, Michaud and Mitchell
! mpif90 knowX.f95 dcdflib.a -o know (with mpi support)
! controlled from simulate.f95

module know
!use mpi
implicit none
include 'mpif.h'

!* general parameters
integer T									! number of years in model
integer ny, ne, nd, nf, na, no, nr, nq, ns	! grid size
integer nsim								! number of simulated agents
character (len=80) :: path					! path to data
character(len=100) filename					! file name when saving value function
character(len=4) fileid						! file id when saving value function


!* parameters for calibration
double precision beta, gamma, umin								! preferences
double precision delta0,delta1 									! depreciation of knowledge
double precision rmin, rmax, rbar, sigr, rhor,alpha0,alpha1		! interest rate parameters
double precision k0, k1, phi, pi0,pi1							! cost of technology
double precision inflation										! rate of inflation
double precision psi											! learning by doing parameter 
!* auxiliary process parameters and arrays
double precision bw(6,3), bo(6,3),br(36,3),incsig(3)			! income and oop parameters
double precision, allocatable :: ptey(:,:), proby(:,:,:), cumproby(:,:,:)
double precision, allocatable :: pteo(:,:), probo(:,:,:), cumprobo(:,:,:)
double precision, allocatable :: pter(:), probr(:), cumprobr(:), vprobr(:)

!: mortality rates and equivalence scales
double precision, allocatable :: mort(:,:), eq(:,:)

! switches for scenarios
double precision issgen, ifin
integer idifmx, idifeq, idifss, inofin
character*80 scenario
	
!* other global variables
double precision cmin, eps_c, eps_i
integer retage(3)

!* grid of continuous state variables
double precision, allocatable :: gridf(:)			! grid for knowledge
double precision fmin, fmax, gapf
double precision, allocatable :: grida(:)			! grid for assets
double precision amin, amax, gapa
double precision, allocatable :: gridd(:)			! grid for risky asset share
double precision dmin, dmax, gapd
integer, allocatable :: lookup(:,:)					! look up array for grid point
!* array for results (all allocated later)
double precision, allocatable :: vopt(:,:,:,:,:)	! value function
double precision, allocatable :: dopt(:,:,:,:,:)	! share invested in technology
double precision, allocatable :: fopt(:,:,:,:,:)	! financial knowledge stock
double precision, allocatable :: aopt(:,:,:,:,:)	! assets
double precision, allocatable :: copt(:,:,:,:,:)	! consumption
double precision, allocatable :: iopt(:,:,:,:,:)	! investment

! arrays for simulation
integer, allocatable :: esim(:,:)
double precision, allocatable :: csim(:,:), isim(:,:), fsim(:,:), asim(:,:), &
	vsim(:,:), dsim(:,:)
integer, allocatable :: ysim(:,:), osim(:,:), rsim(:,:), dead(:,:)
double precision, allocatable :: incsim(:,:), oopsim(:,:)

! MPI globals
integer ier, rank, numprocs, numworkers, status(MPI_STATUS_SIZE),maxsize
logical ismaster
real*8 starttime, endtime, totaltime

contains

	subroutine initmpi
		integer ier
		call mpi_init(ier)
		call mpi_comm_size(MPI_COMM_WORLD,numprocs,ier)
		call mpi_comm_rank(MPI_COMM_WORLD,rank,ier)
		numworkers = numprocs - 1
		
		if (rank.eq.0) then
			ismaster = .true.
		else
			ismaster = .false.
		end if	
		if (ismaster) then
			write(*,*) 'mpi initialized ...'
			write(*,*) ' # of procs 	  :', numprocs
			write(*,*) ' # of workers 	  :', numworkers
			call flush()
		end if	
		starttime = MPI_WTIME()
	end subroutine initmpi
	
	subroutine stopmpi
		endtime = MPI_WTIME()
		totaltime = endtime - starttime	
		if (ismaster) then
			write(*,*) 'total time taken for code =',totaltime
			call flush()
		end if
		
		call flush()
		call mpi_finalize(ier)
	end subroutine stopmpi
	
	subroutine initpar
		! assignment of structural parameters from file 	
		open(unit=1, file='/share/michaud_pi/fin/params/structural-parameters.asc')
		read(1,*) beta
		read(1,*) gamma
		read(1,*) pi0
		read(1,*) pi1
		read(1,*) cmin
		read(1,*) k0
		read(1,*) k1
		read(1,*) phi
		read(1,*) delta0
		read(1,*) delta1
		read(1,*) alpha0
		read(1,*) alpha1	
		read(1,*) psi	
		close(unit=1)
		! assignment of other parameters from file
		open(unit=1, file='/share/michaud_pi/fin/params/other-parameters.asc')
		read(1,*) amin
		read(1,*) amax
		read(1,*) fmin
		read(1,*) fmax
		read(1,*) inflation
		read(1,*) rmax
		read(1,*) rbar
		read(1,*) sigr
		read(1,*) retage(1)
		close(unit=1)
		! assignment of dimensions (grid, nsim) from file
		open(unit=1, file='/share/michaud_pi/fin/params/dimensions-parameters.asc')
		read(1,*) T
		read(1,*) ny
		read(1,*) no
		read(1,*) nr
		read(1,*) ne
		read(1,*) nd
		read(1,*) nf		
		read(1,*) na
		read(1,*) nsim
		close(unit=1)		
		! assignment of scenario parameters from file
		open(unit=1, file='/share/michaud_pi/fin/params/scenario-parameters.asc')			
		read(1,*) scenario
		read(1,*) issgen
		read(1,*) ifin	
		read(1,*) idifmx 
		read(1,*) idifeq 
		read(1,*) idifss
		read(1,*) inofin 			 
		close(unit=1)

		! retirement periods (25 + x)
		retage(2) = retage(1) 
		retage(3) = retage(1)
		! set path 
		path = '/share/michaud_pi/fin'
		! minimum utility
		umin = -10.0d6		
		if (ismaster) then
			write(*,*) '! parameter values initialized ...'
			call flush()
		end if
	end subroutine initpar
	
	subroutine statespace
		integer i, state, y, o, e, f, a
		! number of income/oop shocks
		nq = no*ny
		! bounds for share invested in stocks
		dmax = 1.0d0
		dmin = 0.0d0
		! allocate space for grid of assets and fin knowledge
		allocate(grida(na))
		allocate(gridf(nf))
		allocate(gridd(nd))
		!define and initialize asset and knowledge grid: 
		gapa = (dsqrt(amax) - dsqrt(amin))/dble(na-1)
		gapf = (fmax - fmin)/dble(nf-1)
		do i=1, na, 1
			grida(i) = dsqrt(amin) + dble(i-1)*gapa
		end do
		do i=1, nf, 1
			gridf(i) = fmin + dble(i-1)*gapf
		end do
		gapd = (dmax - dmin)/dble(nd-1)
		if (nd.ge.2) then
		do i = 1, nd, 1
			gridd(i) = dmin + dble(i-1)*gapd
		end do
		else
			gridd(1) = 0.0d0
		end if
		
		! allocate space for solution
		allocate(vopt(T,nq,ne,nf,na))			! value function
		allocate(dopt(T,nq,ne,nf,na))			! utility diff technology
		allocate(fopt(T,nq,ne,nf,na))			! financial knowledge stock
		allocate(aopt(T,nq,ne,nf,na))			! assets
		allocate(copt(T,nq,ne,nf,na))			! consumption
		allocate(iopt(T,nq,ne,nf,na))			! investment
		
		! grid of income shocks
		if (ismaster) then
			write(*,*) '- will now initialize income shocks ...'
		end if
			call income_shocks

		! grid of oop shocks
		if (ismaster) then
			write(*,*) '- will now initialize oop shocks ...'
		end if	
			call oop_shocks

		! rate of return on sophisticated technology		
		if (ismaster) then
			write(*,*) '- will now initialize return risks ...'
		end if	
			call rate_shocks
			
		! mortality rates
		if (ismaster) then
			write(*,*) '- will now initialize mortality rates ...'
		end if
		! loading mortality profiles
		allocate(mort(T,ne))
		open(unit=2,file='/share/michaud_pi/fin/params/mortality-par.csv') 
		! reading header
		read(2,*)
		do i =  1 ,T, 1 
			mort(i,:) = 0.0d0
			read(2,*) mort(i,:)
			if (idifmx.eq.0) then	
				do e= 1, ne, 1
					mort(i,e) = mort(i,2)
				end do
			end if
		end do 
		close(unit=2) 

		! equivalence scales
		if (ismaster) then
			write(*,*) '- will now initialize equivalence scales ...'
		end if
		allocate(eq(T,ne))
		! loading coefficients
		open(unit=3,file='/share/michaud_pi/fin/params/equivalence-par.csv') 
		read(3,*) 
		do i =  1 ,T 
			read(3,*) eq(i,:)	
			if (idifeq.eq.0) then
				do e = 1, ne, 1
					eq(i,e) = eq(i,2)
				end do
			end if
		end do 
		close(unit=3) 
		
		ns = nq*ne*nf*na
		maxsize = 25000
		! allocate space for lookup array
		allocate(lookup(ns,5))
		! fill in look up array
		state = 0
		do y = 1, ny, 1
			do o = 1, no, 1
				do e = 1, ne, 1
						do f = 1, nf, 1
							do a = 1, na, 1
								state = state + 1			
								lookup(state,1) = y
								lookup(state,2) = o
								lookup(state,3) = e
								lookup(state,4) = f
								lookup(state,5) = a
							end do
						end do
				end do
			end do
		end do
		
		! set tolerance for consumption and investment choice
		eps_c = 10.0d0
		eps_i = 0.1d0
		
		if (ismaster) then
			write(*,*) '! state space initialized ...'
			write(*,*) '   - state space has total of ',T*ns, ' points'
			write(*,*) '                              ',ns, ' per year'
		end if
		call flush()
	end subroutine statespace

	subroutine income_shocks
		integer e, i, j
		double precision ny1, prob, sig, rho, sump, mu, m, gap, sige, buffer
		
		! allocate space for probabilities
		allocate(ptey(ne,ny))
		allocate(proby(ne,ny,ny))
		allocate(cumproby(ne,ny,ny))

		! read parameters		
		open(unit=1, file='/share/michaud_pi/fin/params/income-work-par.csv')
		read(1,*) 
		do i = 1, 6, 1
			read(1,*) bw(i,:)
		end do
		close(unit=1)

		! use method of Tauchen (1986)
		m = 2.5d0
		if (ny.eq.1) then
			do e = 1, 3, 1
				ptey(e,1) = 0.0d0
				proby(e,1,1) = 1.0d0
				cumproby(e,1,1) = 1.0d0
			end do
		else	
		ny1 = dble(ny)-1.0d0 
		do e = 1, 3, 1
			! compute unconditional standard deviation
			rho = bw(4,e)
			sige = dsqrt(bw(5,e) + bw(6,e))
			sig = dsqrt((sige**2)/(1.0d0-rho**2) + bw(6,e)) 
			incsig(e) = sig**2
			! grid  points
			ptey(e,1) = -m*sig
			ptey(e,ny) = m*sig
			gap = (2.0d0*m*sig)/ny1
			! Fill in between
			do i=2, ny-1, 1
				ptey(e,i) = ptey(e,1) + dble(i-1)*gap
			end do
			
			! compute probabilities
			do i = 1, ny, 1
				do j = 1, ny, 1
					if (j.eq.1) then
						proby(e,i,1) = probn((ptey(e,j)+gap/2.0d0 - rho*ptey(e,i))/sige)
						cumproby(e,i,1) = proby(e,i,1)
					
					else if (j.gt.1 .and. j.lt.ny) then
						proby(e,i,j) = probn((ptey(e,j)+gap/2.0d0 - rho*ptey(e,i))/sige) - probn((ptey(e,j)-gap/2.0d0 - rho*ptey(e,i))/sige)
						cumproby(e,i,j) = proby(e,i,j) + cumproby(e,i,j-1) 
					
					else
						proby(e,i,ny) = 1.0d0 - probn((ptey(e,j)-gap/2.0d0 - rho*ptey(e,i))/sige)
						cumproby(e,i,ny) = 1.0d0	
					end if
				end do
			end do
		end do
		end if

		! input retirement income
		! read parameters		
		open(unit=1, file='/share/michaud_pi/fin/params/income-ret-par.csv')
		read(1,*) ; ! reading header
		do i = 1, 36, 1
			read(1,*) br(i,:)
			! retirement policy
			do j = 1, 3, 1
				br(i,j) = br(i,j)*issgen	
			end do	
			! differences in replacement rates
			if (idifss.eq.0) then
				do j = 1, 3, 1
					br(i,j) = br(i,2)	
				end do	
			end if			
		end do
		close(unit=1)		
	end subroutine income_shocks

	subroutine oop_shocks
		integer i,j,e
		double precision sig,sump,rho,prob, no1 , mu , m, gap, sige
		! allocate space for probabilities
		allocate(pteo(ne,no))
		allocate(probo(ne,no,no))
		allocate(cumprobo(ne,no,no))

		! read parameters		
		open(unit=2, file='/share/michaud_pi/fin/params/oop-par.csv')
		read(2,*) 
		do i = 1, 6, 1
			read(2,*) bo(i,:)
		end do
		close(unit=2)
		
		! use method of Tauchen (1986)
		m = 2.5d0
		if (no.eq.1) then
			do e = 1, ne, 1
				pteo(e,1) = 0.0d0
				probo(e,1,1) = 1.0d0
				cumprobo(e,1,1) = 1.0d0
			end do
		else	
		no1 = dble(no)-1.0d0 
		do e = 1, ne, 1
			! compute unconditional standard deviation
			rho = bo(4,e)
			sige = dsqrt(bo(5,e) + bo(6,e))
			sig = dsqrt((sige**2)/(1.0d0-rho**2) + bo(6,e)) 
			! grid  points
			pteo(e,1) = -m*sig
			pteo(e,no) = m*sig
			gap = (2.0d0*m*sig)/no1
			! Fill in between
			do i=2, no-1, 1
				pteo(e,i) = pteo(e,1) + dble(i-1)*gap
			end do
			
			! compute probabilities
			do i = 1, no, 1
				do j = 1, no, 1
					if (j.eq.1) then
						probo(e,i,1) = probn((pteo(e,j)+gap/2.0d0 - rho*pteo(e,i))/sige)
						cumprobo(e,i,1) = probo(e,i,1)
					
					else if (j.gt.1 .and. j.lt.no) then
						probo(e,i,j) = probn((pteo(e,j)+gap/2.0d0 - rho*pteo(e,i))/sige) &
						- probn((pteo(e,j)-gap/2.0d0 - rho*pteo(e,i))/sige)
						cumprobo(e,i,j) = probo(e,i,j) + cumprobo(e,i,j-1) 
					
					else
						probo(e,i,no) = 1.0d0 - probn((pteo(e,j)-gap/2.0d0 &
							- rho*pteo(e,i))/sige)
						cumprobo(e,i,no) = 1.0d0	
					end if
				end do
			end do
		end do
		end if
	end subroutine oop_shocks

	subroutine rate_shocks
		integer i
		double precision nr1, gap, m
		
		! allocate space for probabilities
		allocate(cumprobr(nr))
		allocate(pter(nr))
		allocate(probr(nr))
		
		! compute probabilities and quantiles		
		if (nr.eq.1) then
			pter(1) = 0.0d0
			probr(1) = 1.0d0
			cumprobr(1) = 1.0d0
		else	
			m = 3.0d0
			! grid  points
			nr1 = dble(nr)- 1.0d0
			gap = (2.0d0*m*sigr)/nr1
			pter(1) = -m*sigr
			probr(1) = probn((pter(1)+gap/2.0d0)/sigr)
			cumprobr(1) = probr(1)
			! Fill in between
			do i=2, nr-1, 1
				pter(i) = pter(1) + dble(i-1)*gap
				probr(i) = probn((pter(i)+gap/2.0d0)/sigr) &
					- probn((pter(i)-gap/2.0d0)/sigr)
				cumprobr(i) = cumprobr(i-1) + probr(i)
			end do
			pter(nr) = m*sigr
			probr(nr) = 1.0d0-probn((pter(nr)-gap/2.0d0)/sigr)
			cumprobr(nr) = 1.0d0
		end if
	end subroutine rate_shocks

	subroutine solve
		! compute problem last period
		call termination
		! compute recursion
		call recursion
		! save decision rules
		call saverules
						
	end subroutine solve

	subroutine termination
		integer y, o, e, f, a, q
		double precision inc, med, eqcmin, x
		
		if (ismaster) then
			write(*,*) '- computing termination value ...' 
			20      format (3f8.1) 
		
			do y = 1, ny, 1
				do o = 1, no, 1
					do e = 1, ne, 1
						if (T.ge.retage(e)) then
							inc = retinc(T, e, ptey(e,y))
						else
							inc = earn(T, e, ptey(e,y))
						end if	
						med = oop(T,e, pteo(e,o))
						eqcmin = cmin*eq(T,e)/((2.7d0)**0.7)
							do f = 1, nf, 1
								do a = 1, na, 1
									x = grida(a)**2 + inc - med 
									if (x.lt.eqcmin) then
										x = eqcmin
									end if	
									call grid(y,o,q)
									vopt(T,q,e,f,a) = eq(T,e)*utility(x/eq(T,e))
									copt(T,q,e,f,a) = x
									dopt(T,q,e,f,a) = umin
									iopt(T,q,e,f,a) = 0.0d0
								end do
							end do
					end do
				end do
			end do

		end if	
		call flush()
	end subroutine termination

	subroutine recursion
		integer s, y, o, e, f, a, j, k, q, z, p
		integer nstate, state, sent, received, jobsize, nstates, index(4)
		integer sender, tag, i
		double precision result(maxsize,4), nextvalue(ns)
		double precision , allocatable :: value(:),cvalue(:),ivalue(:),dvalue(:)
		logical finished
		if (ismaster) then
			write(*,*) '- starting recursion ...'
			end if
		do s=T-1, 1, -1	
			! start year
			if (ismaster) then						! Master's code
				jobsize = floor(dble(ns)/dble(numworkers))	
				call flush()
				if (ismaster .and. (s.eq.70 .or. s.eq.50 .or. s.eq.25 .or. s.eq.1)) then
					write(*,*) 'at age', 25+s
					call flush()
				end if
				
				sent = 0
								
				! broadcast value function
				! save v(s) to file
				z = 1
				do y = 1, ny, 1
					do o = 1, no, 1
						call grid(y,o,q)
						do e = 1, ne, 1
								do f = 1, nf, 1
									do a = 1, na, 1				
										nextvalue(z) =  vopt(s+1,q,e,f,a)
										z = z+1
									end do
								end do
						end do
					end do
				end do 
				
				call mpi_bcast(nextvalue,ns,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ier)	
				
				call flush()
				! send jobs
				index(1) = 1
				do j = 1, numworkers, 1					
					! check that last index not ns
					if (j.eq.numworkers) then
						index(2) = ns
						index(3) = ns - index(1) + 1
					else
						index(2) = index(1) + jobsize - 1
						index(3) = jobsize
					end if
					index(4) = s
					call flush()
					call mpi_send(index, 4, MPI_INTEGER, j, 1, MPI_COMM_WORLD,ier)
					sent = sent + index(3)
					index(1) = index(2)+1
					if (sent.ge.ns) then
						exit
					end if	
				end do
				if (sent.lt.ns) then
					write(*,*) 'WARNING: Did not send all points'
				end if	
					call flush()
				! receive jobs and assign to arrays
				received = 0
				do while (received .lt. sent)
					! get first the indices from worker
					call mpi_recv(index, 4, MPI_INTEGER, MPI_ANY_SOURCE, &
						MPI_ANY_TAG,MPI_COMM_WORLD,status,ier)
					! get who sent it
					sender = status(MPI_SOURCE)
					! then get data from that worker
					call mpi_recv(result,4*maxsize,MPI_DOUBLE_PRECISION,sender, &
						MPI_ANY_TAG, MPI_COMM_WORLD, status, ier)
					received = received + index(3)
					! Assign his data to array of results
					i = 1
					do k = index(1), index(2), 1
						y = lookup(k,1)
						o = lookup(k,2)
						e = lookup(k,3)
						f = lookup(k,4)
						a = lookup(k,5)
						call grid(y,o,q)
						vopt(s,q,e,f,a) = result(i,1)
						dopt(s,q,e,f,a) = result(i,2) 
						copt(s,q,e,f,a) = result(i,3)
						iopt(s,q,e,f,a) = result(i,4)
						i = i + 1						
					end do
					if ((i-1).ne.index(3)) then
						write(*,*) 'received do not match by ', sender, i,index(1), index(2), index(3)
						call flush()
					end if	
				end do
					call flush()
													
			else									! Worker's code
					
				! retrieve information on job
				call mpi_bcast(nextvalue,ns,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ier)
					call flush()
				! now receive job to do
				call mpi_recv(index,4,MPI_INTEGER,0,MPI_ANY_TAG,MPI_COMM_WORLD,status,ier)
				call flush()						
				!perform job
				allocate(value(index(3)))
				allocate(dvalue(index(3)))
				allocate(cvalue(index(3)))
				allocate(ivalue(index(3)))
				
				call get_opt(index(4) ,index(1), index(2), index(3), value, &
					dvalue, cvalue, ivalue, nextvalue)
				! return info
				result(1:index(3),1) = value
				result(1:index(3),2) = dvalue
				result(1:index(3),3) = cvalue
				result(1:index(3),4) = ivalue
				deallocate(value)
				deallocate(dvalue)
				deallocate(cvalue)
				deallocate(ivalue)	
				call mpi_send(index,4, MPI_INTEGER,0,rank,MPI_COMM_WORLD,ier)
				call mpi_send(result,4*maxsize,MPI_DOUBLE_PRECISION,0,rank,&
					MPI_COMM_WORLD,ier)
						call flush()
			end if
				call flush()
			call mpi_barrier(MPI_COMM_WORLD, ier)
		
		end do
		
		call mpi_barrier(MPI_COMM_WORLD, ier)
		call flush()
	end subroutine recursion

											
	subroutine get_opt(s, sbegin, send, nstate, value, dvalue, cvalue, ivalue, nextvalue)
		! argument list
		integer s, nstate, sbegin, send, state, k 
		double precision value(nstate), dvalue(nstate), cvalue(nstate), ivalue(nstate)		
		! variables to solve problem
		integer y, o, e, f, a, i, c, q, n, oi, p
		integer solution, nc, ni, maxd
		double precision inc, med,  eqcmin, z, bound_c(2), bound_i(2)
		double precision x, cost, guess, xn, oldc, oldi,nextvalue(ns)
		double precision cn, an, fn, in, u, ev, gapi, gapc,dif_c,dif_i,gapi_max
		double precision maxv, maxvd(nd), enough, maxc(nd), maxi(nd), maxvf
		double precision vnext(nq,ne,nf,na),cnext(nq,ne,nf,na),inext(nq,ne,nf,na)
		double precision optc, opti, optv
		logical tech, tr
		nc = na
		! medical expenditure point when not yet retired
		if (no.gt.1) then
			oi = int(dble(no)/2.0d0)+1
		else
			oi = 1
		end if	
		! load next period's value function 
		p = 1
		do y = 1, ny, 1
			do o = 1, no, 1
				call grid(y,o,q)
				do e = 1, ne, 1
						do f = 1, nf, 1
							do a = 1, na, 1				
								vnext(q,e,f,a) = nextvalue(p)
								p = p + 1
							end do
						end do
				end do
			end do
		end do 
		do state = 1, nstate, 1
			! find out what is the state
			k = sbegin + state-1
			y =lookup(k,1)
			o =lookup(k,2)
			e =lookup(k,3)
			f =lookup(k,4)
			a =lookup(k,5)
			if (s.lt.retage(e) .and. o.ne.oi) then
				cycle
			end if
			! income shock
			if (s.ge.retage(e)) then
				inc = retinc(s, e, ptey(e,y))

			else
				inc = earn(s, e, ptey(e,y))
			end if
			! medical exp shock
			med = oop(s,e, pteo(e,o))
			! eqcmin
			eqcmin = cmin*eq(s,e)/((2.7d0)**0.7d0)
			! net financial knowledge
			z = fdepreciate(s)*gridf(f)
			! next year's solution for defining grid
			call grid(y,o,q)
			! cash-on-hand before contemplating decisions
			x = inc + grida(a)**2 - med			
			! provide consumption floor if necessary (pay for meds)
			if (x.lt.eqcmin) then
				x = eqcmin
				tr = .true.
			else
				tr = .false.
			end if		
												
			! check whether technology can be bought
			cost = k0*dexp(-k1*z)
			if ((x-cost) .lt. eqcmin) then
				tech = .false.
			else
				tech = .true.
			end if	
			
			! define solution type
			if (tr) then
				solution = 1		! minimum consumption, no choice
			else
				if (.not. tech) then
					solution = 2	! technology is not affordable

				else 
					solution = 3	! technology is affordable and can be bought
				end if	
			end if

			select case (solution)
				! 1st case: minimum consumption
				case (1)
					! compute consumption
					cn = x
					! asset next period
					an = amin
					! financial lit next period
					fn = z
					! compute utility
					u = eq(s,e)*utility(cn/eq(s,e))
					! get next period's value function (current fin lit, no assets)
					call get_ev(s , e, y, o, fn, an, 1, vnext, ev)	
					value(state) = u + (1.0d0-mort(s,e))*beta*ev 
					dvalue(state) = gridd(1)
					cvalue(state) = cn
					ivalue(state) = 0.0d0								
				
				! 2nd case: technology is not affordable									
				case (2)
				
					xn = x
					n = 1					
					call optimize(xn, z, s, e, y, o, n, vnext, optc, opti, optv)
					value(state) = optv
					dvalue(state) = gridd(1)
					cvalue(state) = optc
					ivalue(state) = opti
		
				! 3rd case: both are possible	
				case (3)
			
					maxv = umin
					maxvd(:) = umin									
					do n = 1, nd, 1	
						if (n.gt.1) then
							xn = x - cost 
						else
							xn = x
						end if	
						optc = 0.0d0
						opti = 0.0d0
						optv = umin
						call optimize(xn, z, s, e, y, o, n, vnext, optc, opti, optv)
						maxvd(n) = optv
						maxc(n) = optc
						maxi(n) = opti
						if (maxvd(n).gt.maxv) then
							maxd = n
							maxv = maxvd(n)
						end if	
					end do
					!if (s.eq.70) then
						!do n = 1, nd, 1
							!write(*,*) s, gridf(f), gridd(n), maxvd(n)
						!end do
					!end if
					value(state) = maxvd(maxd)
					dvalue(state) = gridd(maxd)
					cvalue(state) = maxc(maxd)
					ivalue(state) = maxi(maxd)					
			
			end select												

		end do ! end points had to do
		
	end subroutine get_opt

	subroutine optimize(xn, z, s, e, y, o, n, vnext, optc, opti, optv)
		double precision xn, z
		integer s, e, y, o, n
		double precision vnext(nq,ne,nf,na)
		double precision optc, opti, optv, eqcmin
		double precision reqmin, initpar(2), step(2), par(2), fnmin
		integer konvge, kcount, npar, icount, numres, ifault
	
		optc = 0.0d0
		opti = 0.0d0
		eqcmin = cmin*eq(s,e)/((2.7d0)**0.7d0)
		! execute search for minimum
		npar = 2			
		reqmin = 1.0d-6
		konvge = 10
		kcount = 10000
		initpar(1) =  0.25d0*xn + 0.75d0*eqcmin
		initpar(2) =  0.0d0
		step(1) = 10.0d3
		step(2) = 5.0d0
		!write(*,*) 's optimize = ', s				
		! here will put the nelder mead algorithm			
		call nelmin (funcvalue, npar, initpar, par, fnmin, reqmin, step, &
			 konvge, kcount, icount, numres, ifault, xn, z, s, e, y, o, n, vnext)

		optc = par(1)
		opti = par(2)
		optv = -fnmin
					
	end subroutine optimize

	double precision function funcvalue(par,xn,z,s,e,y,o,n,vnext)
		double precision xn, z
		integer s, e, y, o, n
		double precision vnext(nq,ne,nf,na)
		double precision par(2), cn, in, u, an, fn, ev, eqcmin
		!write(*,*) 's funcvalue = ', s
		cn = par(1)
		in = par(2)
		eqcmin = cmin*eq(s,e)/((2.7d0)**0.7d0)

		u = eq(s,e)*utility(cn/eq(s,e))
		an = xn - cn - fcost(in)
		fn = z + in
		call get_ev(s,e, y, o,  fn, an, n, vnext, ev)

		funcvalue = u + (1.0d0-mort(s,e))*beta*ev

		if (cn.lt.eqcmin .or. cn.gt.xn) then
			funcvalue = umin
		end if
		if (in .lt. 0.0d0 .or. in .gt. (fmax - z)) then
			funcvalue = umin
		end if		 
		if (an.lt.0.0d0) then
			funcvalue = umin
		end if	
		funcvalue = -funcvalue
	end function funcvalue

	subroutine get_ev(s , e, y, o, fn0, an, n, vnext, ev)
		integer s, e, y, o, n, q
		double precision fn, fn0, an, ev, v(2,2)
		double precision pv, R, ar, au, fu, fnp
		double precision vnext(nq,ne,nf,na)
		integer env
		integer j,p,k, a0, a1, f0, f1
		
		if (s.ge.retage(e)) then
			if (n.eq.1) then
				env = 1
			else
				env = 2
			end if	
		else
			if (n.eq.1) then
				env = 3
			else
				env = 4
			end if
		end if
		
		if (ar.lt.amin) then
			ar = amin
		end if
		
		fn = fn0
		
		! learning by doing
		if (n.gt.1) then
			fn = fn + psi
		end if
				
		! find closest point to fn
		f0 = floor((fn - fmin)/gapf)+1
		if (f0.lt.1) then
			fn = fmin
			f0 = 1
			f1 = 2
		else if (f0.ge.nf) then
			f1 = nf
			f0 = nf - 1
		else 
			f1 = f0 + 1
		end if
		fu = (fn - gridf(f0))/(gridf(f1)-gridf(f0))
		ev = 0.0d0
		select case(env)
			! oop risk, no interest risk
			case (1)
				R = factor(fn, gridd(1), 0.0d0)
				ar = R*an
				a0 = floor((dsqrt(ar) - dsqrt(amin))/gapa)+1
				if (a0.lt.1) then
					ar = amin
					a0 = 1
					a1 = 2	
				else if (a0.ge.na) then
					ar = amax
					a1 = na
					a0 = na -1
				else	
					a1 = a0 + 1
				end if	
				au = (dsqrt(ar) - grida(a0))/(grida(a1)-grida(a0))
						
				do p = 1, no, 1
						call grid(y,p,q)
						v(1,1) = vnext(q,e,f0,a0)
						v(1,2) = vnext(q,e,f0,a1)
						v(2,1) = vnext(q,e,f1,a0)
						v(2,2) = vnext(q,e,f1,a1)	
						call blend_102(fu, au, v(1,1), v(1,2), v(2,1), v(2,2), pv)
						ev = ev + probo(e,o,p)*pv						
				end do			

			! oop risk and interest risk
			case (2)			
				do k = 1, nr, 1
					R = factor(fn, gridd(n), pter(k))
					ar = R*an
					a0 = floor((dsqrt(ar) - dsqrt(amin))/gapa)+1
					if (a0.lt.1) then
						ar = amin
						a0 = 1
						a1 = 2	
					else if (a0.ge.na) then
						ar = amax
						a1 = na
						a0 = na -1
					else	
						a1 = a0 + 1
					end if	
					au = (dsqrt(ar) - grida(a0))/(grida(a1)-grida(a0))
					do p = 1, no, 1
							call grid(y,p,q)
							v(1,1) = vnext(q,e,f0,a0)
							v(1,2) = vnext(q,e,f0,a1)
							v(2,1) = vnext(q,e,f1,a0)
							v(2,2) = vnext(q,e,f1,a1)	
							call blend_102(fu, au, v(1,1), v(1,2), v(2,1), v(2,2), pv)
							ev = ev + probr(k)*probo(e,o,p)*pv						
					end do			
				end do
				
			! income risk, no rate risk
			case (3)
				R = factor(fn, gridd(n), 0.0d0)
				ar = R*an
				a0 = floor((dsqrt(ar) - dsqrt(amin))/gapa)+1
				if (a0.lt.1) then
					ar = amin
					a0 = 1
					a1 = 2	
				else if (a0.ge.na) then
					ar = amax
					a1 = na
					a0 = na -1
				else	
					a1 = a0 + 1
				end if	
				au = (dsqrt(ar) - grida(a0))/(grida(a1)-grida(a0))
						
				do j = 1, ny, 1
						call grid(j,o,q)
						v(1,1) = vnext(q,e,f0,a0)
						v(1,2) = vnext(q,e,f0,a1)
						v(2,1) = vnext(q,e,f1,a0)
						v(2,2) = vnext(q,e,f1,a1)	
						call blend_102(fu, au, v(1,1), v(1,2), v(2,1), v(2,2), pv)
						ev = ev + proby(e,y,j)*pv						
				end do						
			! income risk and rate risk
			case (4)
				do k = 1, nr, 1
					R = factor(fn, gridd(n), pter(k))
					ar = R*an
					a0 = floor((dsqrt(ar) - dsqrt(amin))/gapa)+1
					if (a0.lt.1) then
						ar = amin
						a0 = 1
						a1 = 2	
					else if (a0.ge.na) then
						ar = amax
						a1 = na
						a0 = na -1
					else	
						a1 = a0 + 1
					end if	
					au = (dsqrt(ar) - grida(a0))/(grida(a1)-grida(a0))
					do j = 1, ny, 1
							call grid(j,o,q)
							v(1,1) = vnext(q,e,f0,a0)
							v(1,2) = vnext(q,e,f0,a1)
							v(2,1) = vnext(q,e,f1,a0)
							v(2,2) = vnext(q,e,f1,a1)	
							call blend_102(fu, au, v(1,1), v(1,2), v(2,1), v(2,2), pv)
							ev = ev + probr(k)*proby(e,y,j)*pv						
					end do			
				end do
		end select				
	end subroutine get_ev

	subroutine saverules
		integer s, y, o, e, f, a, q
		double precision inc(ne), x(ne)
		if (ismaster) then		
			! consumption by wealth and education
			open(1, file='/share/michaud_pi/fin/data/tab-fig/cons_'//trim(scenario)//'.txt')				
			s = 10
			y = nint(dble(ny/2))
			o = 1
			f = nint(dble(nf/2))
			do e = 1, ne, 1
				inc(e) = earn(s, e, ptey(e,y))
			end do
			call grid(y,o,q)
			do a = 1, na, 1
				do e = 1, ne, 1
					inc(e) = earn(s, e, ptey(e,y))
					x(e) = grida(a)**2 + inc(e)
				end do				
				write(1,*) x, inc, copt(s,q,1,f,a), copt(s,q,2,f,a), copt(s,q,3,f,a)
				!write(*,*) x, inc, copt(s,q,1,f,a), copt(s,q,2,f,a), copt(s,q,3,f,a)
			end do
			close(1)

			! portfolio share by wealth and education
			open(1, file='/share/michaud_pi/fin/data/tab-fig/share_'//trim(scenario)//'.txt')				
			s = 10
			y = nint(dble(ny/2))
			o = 1
			f = nint(dble(nf/2))
			call grid(y,o,q)
			do a = 1, na, 1
				do e = 1, ne, 1
					inc(e) = earn(s, e, ptey(e,y))
					x(e) = grida(a)**2 + inc(e)
				end do				
				write(1,*) x, inc, dopt(s,q,1,f,a), dopt(s,q,2,f,a), dopt(s,q,3,f,a)
				!write(*,*) x, inc, copt(s,q,1,f,a), copt(s,q,2,f,a), copt(s,q,3,f,a)
			end do
			close(1)

			! portfolio share by finlit and education
!			open(1, file='/share/michaud_pi/fin/data/simulations/sharefuncfin_'//trim(scenario)//'.dat')				
!			s = 10
!			y = 5
!			o = 1
!			a = 10
!			call grid(y,o,q)
!			do f = 1, nf, 1
!				write(1,*) gridf(f), dopt(s,q,1,f,a), dopt(s,q,2,f,a), dopt(s,q,3,f,a)
				!write(*,*) x, inc, copt(s,q,1,f,a), copt(s,q,2,f,a), copt(s,q,3,f,a)
!			end do
!			close(1)

		end if
	end subroutine saverules
	
	
	subroutine simulate
		integer q, a0, a1, f0, f1, i, s, j, seed(1), k, nobs
		double precision eqcmin, au, fu, dtag(nf,na), ctag(nf,na), itag(nf,na), vtag(nf,na)
		character (len=80) :: header
		double precision earnsim, ds, cs, is, vs, inc, z, x, R, draw, vproby(ny), &
			vprobo(no), age 	
		double precision rdraws(nsim,T), odraws(nsim,T), ydraws(nsim,T), mdraws(nsim,T)
		! allocate space for simulation arrays
		
		if (ismaster) then
			call flush()
			nobs = 0
			
			! load draws
			open(1,file=trim(path)//'/data/simulations/rdraws.dat')
			do i = 1, nsim, 1
				read(1,*) rdraws(i,:)
			end do
			close(1)
			open(1,file=trim(path)//'/data/simulations/odraws.dat')
			do i = 1, nsim, 1
				read(1,*) odraws(i,:)
			end do
			close(1)
			open(1,file=trim(path)//'/data/simulations/ydraws.dat')
			do i = 1, nsim, 1
				read(1,*) ydraws(i,:)
			end do
			close(1)
			open(1,file=trim(path)//'/data/simulations/mdraws.dat')
			do i = 1, nsim, 1
				read(1,*) mdraws(i,:)
			end do
			close(1)
					
			allocate(esim(nsim,T))
			allocate(dsim(nsim,T))
			allocate(csim(nsim,T))
			allocate(isim(nsim,T))
			allocate(fsim(nsim,T))
			allocate(asim(nsim,T))
			allocate(ysim(nsim,T))
			allocate(osim(nsim,T))
			allocate(rsim(nsim,T))
			allocate(dead(nsim,T))
			allocate(incsim(nsim,T))
			allocate(oopsim(nsim,T))
			allocate(vsim(nsim,T))
			
	
			! simulate individuals
		
			write(*,*) '- will now simulate synthetic cohort ...'
						
			open(unit=3,file='/share/michaud_pi/fin/params/initsample.csv') 
			read(3,*) header
			
			do i = 1, nsim, 1
				
				! take initial conditions from data
				! education level
				read(3,*) incsim(i,1),esim(i,1),asim(i,1)
				earnsim = earn(1, esim(i,1), 0.0d0)
				call point(dlog(incsim(i,1)/earnsim),ptey(esim(i,1),:), ny, ysim(i,1))
				if (asim(i,1).lt.amin) then
					asim(i,1) = amin
				end if	
					
				! technology
				dsim(i,1) = gridd(1)
				
				! medical expenditures
				if (no.gt.1) then
					osim(i,1) = int(dble(no)/2.0d0)+1
				else
					osim(i,1) = 1
				end if	
				oopsim(i,1) = 0.0d0
				
				! real rate of return shock
				rsim(i,1) = 2
				
				! asset and financial knowledge start period (set in scenario ifin)
				fsim(i,1) = ifin
				if (fsim(i,1).gt.fmax) then
					fsim(i,1) = fmax
				end if
				if (fsim(i,1).lt.fmin) then
					fsim(i,1) = fmin
				end if

				dead(i,1) = 1	
				
				
				
				
				do s = 1, T-1, 1
					nobs = nobs + 1
					call flush()
					if (dead(i,s).eq.1) then
						oopsim(i,s) = oop(s, esim(i,s), pteo(esim(i,s),osim(i,s))) 
						eqcmin = cmin*eq(s,esim(i,s))/((2.7d0)**0.7d0)
						! get state on grid for intrapolation
						call closest(dsqrt(asim(i,s)),dsqrt(amin),gapa,na,a0,a1)
						call closest(fsim(i,s),fmin,gapf,nf,f0,f1)
						au = (dsqrt(asim(i,s)) - grida(a0))/(grida(a1)-grida(a0))
						fu = (fsim(i,s) - gridf(f0))/(gridf(f1)-gridf(f0))						
						! compute optimal technology
						call grid(ysim(i,s),osim(i,s),q)
						dtag =  dopt(s,q,esim(i,s),:,:)
						call blend_102(fu, au, dtag(f0,a0),dtag(f0,a1),dtag(f1,a0), &
							dtag(f1,a1),ds)				
						dsim(i,s+1) = ds
						! intrapolate optimal consumption
						ctag =  copt(s,q,esim(i,s),:,:)
						call blend_102(fu, au, ctag(f0,a0),ctag(f0,a1),ctag(f1,a0),&
							ctag(f1,a1),cs)
						csim(i,s) = cs							
						! intrapolate optimal investment
						itag =  iopt(s,q,esim(i,s),:,:)
						call blend_102(fu, au, itag(f0,a0),itag(f0,a1),itag(f1,a0),&
							itag(f1,a1),is)
						isim(i,s) = is
						
					   ! intrapolate value function (to get utility)
						vtag =  vopt(s,q,esim(i,s),:,:)
						call blend_102(fu, au, vtag(f0,a0),vtag(f0,a1),vtag(f1,a0), &
							vtag(f1,a1),vs)
						vsim(i,s) = vs
						
						! first transition to 2
						!write(*,*) ' coeff are ', s,bw(:,esim(i,s-1))
						if (s.ge.retage(esim(i,s))) then
							inc = retinc(s, esim(i,s),ptey(esim(i,s),ysim(i,s)))						
						else
							inc = earn(s, esim(i,s),ptey(esim(i,s),ysim(i,s)))
						end if
						incsim(i,s) = inc

						! net financial knowledge
						z = fdepreciate(s)*fsim(i,s)	
											
						! cash-on-hand
						x = inc + asim(i,s) - oopsim(i,s)
						! transfers
						if (x.lt.eqcmin) then
							x = eqcmin
						end if
							
						! end of period financial lit
						fsim(i,s+1) = z + isim(i,s)
						if (fsim(i,s+1).gt.fmax) then
							fsim(i,s+1) = fmax
						else if (fsim(i,s+1).lt.fmin) then
							fsim(i,s+1) = fmin
						end if
			
						! draw shock to interest rate						
						do j = 1, nr, 1
							if (rdraws(i,s).lt.cumprobr(j)) then
								rsim(i,s+1) = j
								exit
							end if
						end do	
						
						! end of period assets
						R = factor(fsim(i,s+1), dsim(i,s+1), pter(rsim(i,s+1)))
						
						asim(i,s+1) = x - csim(i,s) - fcost(isim(i,s)) 
						if (dsim(i,s+1).gt.dmin ) then
							asim(i,s+1) = asim(i,s+1) - k0*dexp(-k1*z)
						end if 
						asim(i,s+1) = R*asim(i,s+1)
						if (asim(i,s+1).gt.amax) then
							asim(i,s+1) = amax
						else if (asim(i,s+1).lt.amin) then
							asim(i,s+1) = amin
						end if	

						esim(i,s+1) = esim(i,s)
						
						! draw shock to income
						
						if (s.lt.retage(esim(i,s))) then
							vproby = cumproby(esim(i,s),ysim(i,s),:)
							do j = 1, ny, 1
								if (ydraws(i,s).lt.vproby(j)) then
									ysim(i,s+1) = j
									exit
								end if
							end do	
						else
							ysim(i,s+1) = ysim(i,s)
						end if
												
						! draw shock to out-of-pocket
						if (s.ge.retage(esim(i,s))) then
							vprobo = cumprobo(esim(i,s),osim(i,s),:)
							do j = 1, no, 1
								if (odraws(i,s).lt.vprobo(j)) then
									osim(i,s+1) = j
									exit
								end if
							end do	
						else
							osim(i,s+1) = osim(i,s)
						end if
						
						! decide whether survive
						if (mdraws(i,s).lt.mort(s,esim(i,s))) then
							dead(i,s+1) = 2
							exit
						else 
							dead(i,s+1) = 1
						end if
						if (s.eq.T-1) then
							dead(i,s+1) = 2
							exit
						end if	
					end if			
					
				end do
				
			end do
			close(3)
					
			write(*,*) '- cohort simulated, saving dataset ...'
							
			open (8,file='/share/michaud_pi/fin/data/simulations/simknow_'//trim(scenario)//'.dat')
			write(8,*) nobs
			do i=1,nsim,1
				do s = 1, T-1, 1
					age = real(24+s)
					if (dead(i,s).eq.1) then
						write(8,*) i,age,vsim(i,s),csim(i,s),asim(i,s),incsim(i,s), &
							oopsim(i,s),rsim(i,s),isim(i,s),fsim(i,s),esim(i,s),dsim(i,s)
					end if
				end do
			end do	
			close(8)
		end if
		call mpi_barrier(MPI_COMM_WORLD, ier)
	end subroutine simulate
	
	double precision function probn(value)
		double precision value,  q, bound
		integer ifail, status
		call cdfnor(1,probn,q, value, 0.0d0, 1.0d0, status, bound)
	end function

	double precision function quann(prob)
		double precision prob, bound
		integer ifail, status
		ifail = 1
		call cdfnor(2,prob,1.0d0-prob, quann, 0.0d0, 1.0d0, status, bound)
	end function

	!* state of shock
	subroutine grid(y, o, q)
		integer q, y, o
		q = (y-1)*no + o
	end subroutine
		
	!* utility function
	double precision function utility(x)
		double precision x
		if (x.gt. phi) then
				utility = ((x-phi)**(1.0d0-gamma))/(1.0d0-gamma) 
			else
				utility = umin
		end if
	end function
		
	! investment cost
	double precision function fcost(i)
		double precision i
		if (i.le.0.0d0) then
			fcost = 0.0d0
		else		
			fcost = pi0*(i**pi1)
		end if	
	end function
	double precision function finvcost(c)
		double precision c
		if (c.le.0.0d0) then
			finvcost = 0.0d0
		else
			finvcost = (c/pi0)**(1.0d0/pi1)
		end if	
	end function
	! depreciation rate
	double precision function fdepreciate(s)
		integer s
		fdepreciate = dexp(delta0 - delta1*dble(s))
	end function
	! compute rate of return
	double precision function factor(fpt, tech, shock)
		double precision fpt, rf, shock, tech
		if (inofin.eq.0) then
			rf = alpha0*(fpt**alpha1)
		else
			rf = rmax
		end if		
		rf = rf  + sigr*shock + rbar
		factor = 1.0d0 +tech*rf + (1.0d0-tech)*rbar - inflation
	end function
	
	! compute earnings
	double precision function earn(s, e, py)
		integer s, e
		double precision py, age, agesq
			age = dble(25+s)
			agesq = 0.01d0*(age**2)
			earn = dexp(bw(3,e) + bw(1,e)*age + bw(2,e)*agesq -0.5d0*incsig(e) +  py)
	end function
	
	! compute retirement income
	double precision function retinc(s, e, py)
		integer s, e, age
		double precision work, rep, py
			age = 25+s
			work = earn(retage(e)-1,e,0.0d0)
			rep = br(age-64,e)
			retinc = work*rep			
	end function

	! compute out-of-pocket expenditures
	double precision function oop(s, e, po)
		integer s, e
		double precision po, age, agesq
		if (s.ge.retage(e)) then
			age = dble(25+s)
			agesq = 0.01d0*(age**2)
			oop = dexp(bo(1,e)*age + bo(2,e)*agesq  + bo(3,e) + po)
		else 
			oop = 0.0d0
		end if	
	end function
	
	! routine to find the closest point index on a grid
	subroutine closest(val, minval, step, n, low, up)
		double precision val, step,minval
		integer n, low, up		
		low = int(floor((val - minval)/step))+1
		if (low.lt.1) then
			low = 1
			up = 2
		else if (low.eq.n) then
			up = low
			low = low - 1
		else
			up = low + 1
		end if		
	end subroutine
	
	! routine to find the closest point on a grid
	subroutine point(val, grid, n, p)
		integer n,i,imin,p
		double precision val, grid(n),diff,dmin
		dmin = 1.0d20; imin = 1
		do i = 1,n,1
			diff = dabs(val - grid(i))
			if (diff.lt.dmin) then
				dmin = diff
				imin = i
			end if	
		end do
		p = imin
	end	subroutine

	! routine to linearly intrapolate in two dimensions
	subroutine blend_102 ( r, s, x00, x01, x10, x11, x )
	  double precision r, s, x, x00,x01,x10,x11
	  x = x00 + r*(-x00+x10) + s*(-x00+x01) + r*s*(x00 - x10 - x01 + x11 )	
	end subroutine


			 
	! optimization (Nelder-Mead algorithm": version by John Burkardt)
	subroutine nelmin ( fn, n, start, xmin, ynewlo, reqmin, step, konvge, kcount, &
	icount, numres, ifault, xn,zn,s,e,yy,o,d, vnext)

		  implicit none
		
		  integer ( kind = 4 ) n
		
		  double precision, parameter :: ccoeff = 0.5d0
		  double precision del
		  double precision, parameter :: ecoeff = 2.0d0
		  double precision, parameter :: eps = 0.001d0
		  double precision, external :: fn
		  integer ( kind = 4 ) i
		  integer ( kind = 4 ) icount
		  integer ( kind = 4 ) ifault
		  integer ( kind = 4 ) ihi
		  integer ( kind = 4 ) ilo
		  integer ( kind = 4 ) j
		  integer ( kind = 4 ) jcount
		  integer ( kind = 4 ) kcount
		  integer ( kind = 4 ) konvge
		  integer ( kind = 4 ) l
		  integer ( kind = 4 ) numres
		  double precision p(n,n+1)
		  double precision p2star(n)
		  double precision pbar(n)
		  double precision pstar(n)
		  double precision, parameter :: rcoeff = 1.0d0
		  double precision reqmin
		  double precision rq
		  double precision start(n)
		  double precision step(n)
		  double precision x
		  double precision xmin(n)
		  double precision y(n+1)
		  double precision y2star
		  double precision ylo
		  double precision ynewlo
		  double precision ystar
		  double precision z
		  double precision xn, zn
		  integer s, e, yy, o, d
		  double precision vnext(nq,ne,nf,na)
					
		!
		!  Check the input parameters.
		!
		  if ( reqmin <= 0.0d0 ) then
			ifault = 1
			return
		  end if
		
		  if ( n < 1 ) then
			ifault = 1
			return
		  end if
		
		  if ( konvge < 1 ) then
			ifault = 1
			return
		  end if
		!
		!  Initialization.
		!
			
		  icount = 0
		  numres = 0
		  jcount = konvge
		  del = 1.0d0
		  rq = reqmin * dble(n)
		
			!write(*,*) 's nelmin = ', s			
			!write(*,*) 'get function value ', fn ( start ,k,h,e, cash, value), start
			
		!
		!  Initial or restarted loop.
		!

		  do
		
			p(1:n,n+1) = start(1:n)
			y(n+1) = fn ( start ,xn,zn,s,e,yy,o,d,vnext)

			icount = icount + 1
		!
		!  Define the initial simplex.
		!
			do j = 1, n
			  x = start(j)
			  start(j) = start(j) + step(j) * del
			  p(1:n,j) = start(1:n)
			  !write(*,*) 'getting function along dimension of simplex ', j
			  y(j) = fn ( start,xn,zn,s,e,yy,o,d,vnext)
			  icount = icount + 1
			  start(j) = x
			end do
		!	
		!  Find highest and lowest Y values.  YNEWLO = Y(IHI) indicates
		!  the vertex of the simplex to be replaced.
		!
			ilo = minloc ( y(1:n+1), 1 )
			ylo = y(ilo)
		!
		!  Inner loop.
		!
			do while ( icount < kcount )
		!
		!  YNEWLO is, of course, the HIGHEST value???
		!
			  ihi = maxloc ( y(1:n+1), 1 )
			  ynewlo = y(ihi)
		!
		!  Calculate PBAR, the centroid of the simplex vertices
		!  excepting the vertex with Y value YNEWLO.
		!
			  do i = 1, n
				pbar(i) = ( sum ( p(i,1:n+1) ) - p(i,ihi) ) / dble(n)
			  end do
		!
		!  Reflection through the centroid.
		!
			  pstar(1:n) = pbar(1:n) + rcoeff * ( pbar(1:n) - p(1:n,ihi) )
			  ystar = fn ( pstar,xn,zn,s,e,yy,o,d,vnext)
			  icount = icount + 1
		!
		!  Successful reflection, so extension.
		!
			  if ( ystar < ylo ) then
		
				p2star(1:n) = pbar(1:n) + ecoeff * ( pstar(1:n) - pbar(1:n) )
				y2star = fn ( p2star,xn,zn,s,e,yy,o,d,vnext)
				icount = icount + 1
		!
		!  Retain extension or contraction.
		!
				if ( ystar < y2star ) then
				  p(1:n,ihi) = pstar(1:n)
				  y(ihi) = ystar
				else
				  p(1:n,ihi) = p2star(1:n)
				  y(ihi) = y2star
				end if
		!
		!  No extension.
		!
			  else
		
				l = 0
				do i = 1, n + 1
				  if ( ystar < y(i) ) then
					l = l + 1
				  end if
				end do
		
				if ( 1 < l ) then
		
				  p(1:n,ihi) = pstar(1:n)
				  y(ihi) = ystar
		!
		!  Contraction on the Y(IHI) side of the centroid.
		!
				else if ( l == 0 ) then
		
				  p2star(1:n) = pbar(1:n) + ccoeff * ( p(1:n,ihi) - pbar(1:n) )
				  y2star = fn ( p2star ,xn,zn,s,e,yy,o,d,vnext)
				  icount = icount + 1
		!
		!  Contract the whole simplex.
		!
				  if ( y(ihi) < y2star ) then
		
					do j = 1, n + 1
					  p(1:n,j) = ( p(1:n,j) + p(1:n,ilo) ) * 0.5d0
					  xmin(1:n) = p(1:n,j)
					  y(j) = fn ( xmin ,xn,zn,s,e,yy,o,d,vnext)
					  icount = icount + 1
					end do
		
					ilo = minloc ( y(1:n+1), 1 )
					ylo = y(ilo)
		
					cycle
		!
		!  Retain contraction.
		!
				  else
					p(1:n,ihi) = p2star(1:n)
					y(ihi) = y2star
				  end if
		!
		!  Contraction on the reflection side of the centroid.
		!
				else if ( l == 1 ) then
		
				  p2star(1:n) = pbar(1:n) + ccoeff * ( pstar(1:n) - pbar(1:n) )
				  y2star = fn ( p2star ,xn,zn,s,e,yy,o,d,vnext)
				  icount = icount + 1
		!
		!  Retain reflection?
		!
				  if ( y2star <= ystar ) then
					p(1:n,ihi) = p2star(1:n)
					y(ihi) = y2star
				  else
					p(1:n,ihi) = pstar(1:n)
					y(ihi) = ystar
				  end if
		
				end if
		
			  end if
		!
		!  Check if YLO improved.
		!
			  if ( y(ihi) < ylo ) then
				ylo = y(ihi)
				ilo = ihi
			  end if
		
			  jcount = jcount - 1
		
			  if ( 0 < jcount ) then
				cycle
			  end if
		!
		!  Check to see if minimum reached.
		!
			  if ( icount <= kcount ) then
		
				jcount = konvge
		
				x = sum ( y(1:n+1) ) / dble(n + 1)
				z = sum ( ( y(1:n+1) - x )**2 )
		
				if ( z <= rq ) then
				  exit
				end if
		
			  end if
		
			end do
		!
		!  Factorial tests to check that YNEWLO is a local minimum.
		!
			xmin(1:n) = p(1:n,ilo)
			ynewlo = y(ilo)
		
			if ( kcount < icount ) then
			  ifault = 2
			  exit
			end if
		
			ifault = 0
		
			do i = 1, n
			  del = step(i) * eps
			  xmin(i) = xmin(i) + del
			  z = fn ( xmin ,xn,zn,s,e,yy,o,d,vnext)
			  icount = icount + 1
			  if ( z < ynewlo ) then
				ifault = 2
				exit
			  end if
			  xmin(i) = xmin(i) - del - del
			  z = fn ( xmin ,xn,zn,s,e,yy,o,d,vnext)
			  icount = icount + 1
			  if ( z < ynewlo ) then
				ifault = 2
				exit
			  end if
			  xmin(i) = xmin(i) + del
			end do
		
			if ( ifault == 0 ) then
			  exit
			end if
		!
		!  Restart the procedure.
		!
			start(1:n) = xmin(1:n)
			del = eps
			numres = numres + 1
		
		  end do
			
		  return
		end subroutine nelmin

		
end module know

program main
use know
	call initmpi
	call initpar
	call statespace
	call solve
	call simulate
	call stopmpi
end program main
