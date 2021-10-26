    module mpg_io_m
        use mpg_m
        use crc_m
        use file_io_m
        use layer2_m
        implicit none
        private 
        public :: mpgfile_t
        
        type, extends(file_t) :: mpgfile_t
            private
            integer :: iunit 
            integer :: ipos
            character(:), allocatable :: fn
            character (len = 20000) :: bit_string
        contains
            procedure :: open_file  => open_mpg
            procedure :: close_file => close_mpg
            final     :: destroy_file
            procedure :: write_bits_1frame
            procedure :: clear_bit_buff
            procedure :: encode_header
            procedure :: encode_crc
            procedure :: encode_alloc_bits
            procedure :: encode_body
            procedure :: encode_scale_factor
            procedure :: encode_scfsi
            procedure :: put_bits
            procedure :: put_bits_c
        end type mpgfile_t    
    
    contains

        subroutine open_mpg(this, fn)
            class(mpgfile_t),   intent(in out) :: this
            character(len = *), intent(in) :: fn
            integer :: io
            open(newunit = this%iunit, file = fn, iostat = io, status = 'unknown', access = 'stream')
            if (io /= 0) then
                write(*, *) 'i/o error ', io, ' occuerred. file =', this%iunit, 'file name ', fn
                stop 'check output file!'
            end if
        end subroutine open_mpg

        
        subroutine close_mpg(this)
            class(mpgfile_t), intent(in) :: this
            close(this%iunit)
        end subroutine close_mpg
      
        
        subroutine destroy_file(this)
            type(mpgfile_t), intent(in) :: this
            call this%close_file()
        end subroutine destroy_file  

        
        subroutine write_bits_1frame(this, n)
            class(mpgfile_t), intent(in) :: this
            integer, intent(in) :: n
            integer :: i, j, ipos
            integer(int8) :: m
            ipos = 0
            do i = 1, n, 8
                m = 0
                do j = 7, 0, -1
                    ipos = ipos + 1
                    if (ipos > len(this%bit_string)) exit
                    if (this%bit_string(ipos:ipos) == '1')  m = ibset(m, j) 
                end do
                write(this%iunit) m 
            end do
        end subroutine write_bits_1frame

        
        subroutine clear_bit_buff(this, n)
            class(mpgfile_t), intent(in out) :: this
            integer, intent(in) :: n
            this%bit_string = repeat(' ', n)
        end subroutine clear_bit_buff

        
        subroutine encode_header(this, mpg)
            class(mpgfile_t), intent(in out) :: this
            type (mpg_t)    , intent(in    ) :: mpg
            this%ipos = 1 ! reset position to the first bit
            call this%put_bits_c('11111111111'      )  !sync word
            call this%put_bits(2, mpg%mtype         )  !mpeg1
            call this%put_bits(2, mpg%layer         )  !layer 1
            call this%put_bits(1, mpg%icrc          )  !crc check no
            call this%put_bits(4, mpg%ibit_rate     )  !bitrate 
            call this%put_bits(2, mpg%isample_rate  )  !sampling frequency 44.1
            call this%put_bits(1, mpg%ipadding      )  !ipadding
            call this%put_bits(1, mpg%iprivate_bit  )  !private bit : unused
            call this%put_bits(2, mpg%mode          )  !stereo
            call this%put_bits(2, mpg%mode_extension)  !mode
            call this%put_bits(1, mpg%icopyright    )
            call this%put_bits(1, mpg%ioriginal     )
            call this%put_bits(2, mpg%iemphasis     )
        end subroutine encode_header

        
        subroutine encode_crc(this, mpg, ialloc_bits, iscfsi)
            class(mpgfile_t), intent(in out) :: this
            type (mpg_t), intent(in) :: mpg
            integer, intent(in) :: ialloc_bits(:, :), iscfsi(:, :)
            integer :: iband, ichannel, icrc
            icrc = int(Z'0000ffff')
            call crc16(4, mpg%ibit_rate     , icrc)
            call crc16(2, mpg%isample_rate  , icrc)
            call crc16(1, mpg%ipadding      , icrc)
            call crc16(1, mpg%iprivate_bit  , icrc)
            call crc16(2, mpg%mode          , icrc)
            call crc16(2, mpg%mode_extension, icrc)
            call crc16(1, mpg%icopyright    , icrc)
            call crc16(1, mpg%ioriginal     , icrc)
            call crc16(2, mpg%iemphasis     , icrc)
            do iband = 1, table%nband
                do ichannel = 1, size(ialloc_bits, 2)
                    call crc16(table%nbal(iband), ialloc_bits(iband, ichannel), icrc)
                end do
            end do
            do iband = 1, table%nband
                do ichannel = 1, size(ialloc_bits, 2)
                    if (ialloc_bits(iband, ichannel) /= 0) call crc16(2, iscfsi(iband, ichannel), icrc)
                end do
            end do
            call this%put_bits(16, icrc)
        end subroutine encode_crc

        
        subroutine encode_alloc_bits(this, ialloc_bits)
            class(mpgfile_t), intent(in out) :: this
            integer, intent(in) :: ialloc_bits(:, :)
            integer :: iband, ichannel
            do iband = 1, table%nband
                do ichannel = 1, size(ialloc_bits, 2)
                    call this%put_bits(table%nbal(iband), ialloc_bits(iband, ichannel) )
                end do
            end do
        end subroutine encode_alloc_bits

        
        subroutine encode_scfsi(this, ialloc_bits, iscfsi)
            class(mpgfile_t), intent(in out) :: this
            integer, intent(in    ) :: ialloc_bits(:, :), iscfsi(:, :)
            integer :: iband, ichannel
            do iband = 1, table%nband
                do ichannel = 1, size(ialloc_bits, 2)
                    if (ialloc_bits(iband, ichannel) /= 0) call this%put_bits(2, iscfsi(iband, ichannel) )
                end do
            end do
        end subroutine encode_scfsi

        
        subroutine encode_scale_factor(this, ialloc_bits, iscfsi, iscale_factor)
            class(mpgfile_t), intent(in out) :: this
            integer, intent(in    ) :: ialloc_bits(:, :), iscfsi(:, :), iscale_factor(:, :, :)
            integer :: iband, ichannel
            do iband = 1, table%nband
                do ichannel = 1, size(ialloc_bits, 2)
                    if (ialloc_bits(iband, ichannel) /= 0) then
                        select case ( nscfsi( iscfsi(iband, ichannel) ) )
                        case (1)
                            call this%put_bits(6, iscale_factor(iband, 1, ichannel) )
                        case (2)
                            call this%put_bits(6, iscale_factor(iband, 1, ichannel) )
                            call this%put_bits(6, iscale_factor(iband, 3, ichannel) )
                        case (3)
                            call this%put_bits(6, iscale_factor(iband, 1, ichannel) )
                            call this%put_bits(6, iscale_factor(iband, 2, ichannel) )
                            call this%put_bits(6, iscale_factor(iband, 3, ichannel) )
                        case default
                            stop 'unexpected number: encode_body'
                        end select
                    end if
                end do
            end do
        end subroutine encode_scale_factor

        
        subroutine encode_body(this, ialloc_bits, isubband)
            class(mpgfile_t), intent(in out) :: this
            integer, intent(in) :: ialloc_bits(:, :), isubband(:, :, :)
            integer :: iband, ichannel, igranule, j, k, m, n
            do igranule = 1, 12
                do iband = 1, table%nband
                    do ichannel = 1, size(ialloc_bits, 2)
                        if (ialloc_bits(iband, ichannel) /= 0) then
                            k = table%nbit(iband, ialloc_bits(iband, ichannel))
                            if (table%nsample(k) == 1) then
                                do j = 3 * (igranule - 1) + 1, 3 * igranule
                                      call this%put_bits(table%nlength(k), isubband(iband, j, ichannel) )
                                end do
                            else
                                j = 3 * (igranule - 1)
                                n = table%nsteps(k)
                                m = isubband(iband, j + 1, ichannel)        &
                                  + isubband(iband, j + 2, ichannel) * n    &
                                  + isubband(iband, j + 3, ichannel) * n**2
                                call this%put_bits(table%nlength(k), m )
                            end if
                        end if
                    end do
                end do
            end do
        end subroutine encode_body

        
        subroutine put_bits(this, n, inp)
            class(mpgfile_t), intent(in out) :: this
            integer, intent(in) :: n, inp
            integer :: i, m
            associate (ipos => this%ipos, bit_string => this%bit_string)
                do i = 1, n
                    m = 2**(n - i)
                    if (mod(inp / m, 2) == 1) then
                        this%bit_string(ipos:ipos) = '1'
                    else
                        this%bit_string(ipos:ipos) = '0'
                    end if
                    ipos = ipos + 1
                end do
            end associate  
        end subroutine put_bits
 
    
        subroutine put_bits_c(this, str)
            class(mpgfile_t), intent(in out) :: this
            character(len = *), intent(in) :: str
            integer :: i
            associate (ipos => this%ipos, bit_string => this%bit_string)
                do i = 1, len_trim(str)
                    if (scan(str(i:i), '01') == 0) stop 'invalid string: subroutine put_bit_c'
                    this%bit_string(ipos:ipos) = str(i:i)
                    ipos = ipos + 1
                end do
            end associate      
        end subroutine put_bits_c
        
    end module mpg_io_m