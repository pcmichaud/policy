program main
	use know
	integer narg
	narg = command_argument_count()
  	call get_command_argument(1,scenario)
	call initmpi
	call initialize
	call initpar
	call statespace
	call solve
	call simulate
	call stopmpi
end program main