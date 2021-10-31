    program uzura2
        use mpg_m
        use wav_io_m
        use polyphase_m
        use psycho_m
        use layer2_m
        use mpg_io_m

        implicit none
        type (mpg_t) :: mpg
        integer, allocatable :: iscale_factor(:, :, :), iscfsi(:, :), isubband(:, :, :), ialloc_bits(:, :)
        real(kd), allocatable :: pcm(:, :), smr(:, :)
        integer :: max_bits, nchannel, ntotal_frames, iframe = 0
        
        character(len = :), allocatable :: file_name, fn_in, fn_out
        type(wavfile_t), allocatable :: wav
        type(mpgfile_t), allocatable :: mp2
        type(subband_t), allocatable :: subb
        !
        allocate(wav, mp2, subb)
        call get_option(mpg, file_name)
        fn_in  = trim(file_name) // '.wav'
        fn_out = trim(file_name) // '.mp2'
        call pr_info(mpg)
    
        call wav%open_file(fn_in) !  read whole wav file
        call mp2%open_file(fn_out)

        call pr_play_time( wav%get_play_time() )
        nchannel = wav%get_channel()
    
        allocate( pcm(1632, nchannel), source = 0.0_kd )
        allocate( iscale_factor(32, 3, nchannel),  iscfsi(32, nchannel) )

        call subb%init(nchannel)
        call select_table(mpg%isample_rate, mpg%ibit_rate, nchannel)
        ntotal_frames = wav%get_data_size() / (mpeg_frame_size(mpg%layer) * nchannel * 2) 
        do while (iframe < ntotal_frames )
<<<<<<< HEAD
            call get_maxbits(max_bits, mpg%ipadding)
            call mp2%clear_bit_buff(max_bits)
            call mp2%encode_header(mpg)
            call wav%pcm1frame(pcm) 
            call subb%polyphase_filter36(pcm)
            smr = psychoacoustics(pcm, wav%get_sampling_rate())
            call subband_normalization(subb%subband, iscfsi, iscale_factor)
            ialloc_bits = bit_allocation(mpg, smr, iscfsi, max_bits)
            call mp2%encode_crc(mpg, ialloc_bits, iscfsi)
            isubband = quantization(ialloc_bits, subb%subband)
=======
            call wav%pcm1frame(pcm) 
            call subb%polyphase_filter36(pcm)
            call subband_normalization(subb%subband, iscfsi, iscale_factor)
            smr = psychoacoustics(pcm, wav%get_sampling_rate())

            call get_maxbits(max_bits, mpg%ipadding)
            ialloc_bits = bit_allocation(mpg, smr, iscfsi, max_bits)
            isubband = quantization(ialloc_bits, subb%subband)

            call mp2%clear_bit_buff(max_bits)
            call mp2%encode_header(mpg)
            call mp2%encode_crc(mpg, ialloc_bits, iscfsi)
>>>>>>> dev
            call mp2%encode_alloc_bits(ialloc_bits)
            call mp2%encode_scfsi(ialloc_bits, iscfsi)
            call mp2%encode_scale_factor(ialloc_bits, iscfsi, iscale_factor)
            call mp2%encode_body(ialloc_bits, isubband)
            call mp2%write_bits_1frame(max_bits)
            if ( mod(iframe, 100) == 0 ) call update_status(iframe, ntotal_frames) 
            iframe = iframe + 1
        end do
        write(*, *) 'toal frames', iframe, '/', ntotal_frames
        deallocate(wav, mp2, subb)
    contains
    
        subroutine calc_slot_size(islot_size, fslot_size)
            integer       , intent(out) :: islot_size
            real(kd), intent(out) :: fslot_size
            real(kd) :: aslot_size
            aslot_size = 144.0_kd * 1000.0_kd * real(mpeg_bit_rates(mpg%ibit_rate, mpg%layer), kd) &
                       / real(mpeg_sample_rates(mpg%isample_rate), kd)
            aslot_size = 1.0e-3_kd * anint(1.0e3_kd * aslot_size) 
            islot_size = int(aslot_size)
            fslot_size = aslot_size - islot_size
        end subroutine calc_slot_size

    
        subroutine get_maxbits(max_bits, ipadding)
            integer,  intent(out) :: max_bits, ipadding
            integer        , save :: islot_size
            real (kd), save :: padding, fslot_size
            logical        , save :: qfirst = .true.
            if (qfirst) then
                qfirst = .false.
                padding = 0.0_kd
                call calc_slot_size(islot_size, fslot_size)
            end if
            padding = padding + fslot_size
            if (padding > 1) then
                ipadding = 1
                padding = padding - 1.0_kd
            else
                ipadding = 0
            end if
            max_bits = ( islot_size + ipadding ) * 8
        end subroutine get_maxbits

    
        subroutine pr_play_time(itot_sec)
            integer, intent(in) :: itot_sec
            integer :: ihour, imin, isec
            ihour =          itot_sec / 3600
            imin  =      mod(itot_sec, 3600) / 60
            isec  = mod( mod(itot_sec, 3600) , 60 )
            write(*, '(a, i3, a, i2, a, i2, /)') ' playtime ', ihour, ':', imin, ':', isec
        end subroutine pr_play_time
    
    
        subroutine pr_info(mpg)
            type (mpg_t), intent(in) :: mpg
            write(*, *) 'uzura2 (MPEG-1 audio/layer-II encoder for dec fortran/win) ver.0.2 (c) C.H. '
            write(*, *) 'psychoacoustic model ', mpeg_psy_names(mpg%ipsychoacoustic), &
                        ' bit rate (kbps)', mpeg_bit_rates(mpg%ibit_rate, mpg%layer)
            if (mpg%icrc == 0) write(*, *) 'crc16 error protection enabled'
        end subroutine pr_info


        subroutine update_status(iframe, itot_frames)
            integer, intent(in) :: iframe, itot_frames
            integer :: it(8), ielapsed, iel_min, iel_sec
            integer, save :: istart
            logical :: qfirst = .true.
            real    :: percent
            character (len = 10) :: time, date, zone
            call date_and_time(date, time, zone, it)
            if (qfirst) then
                istart   = it(5) * 3600 + it(6) * 60 + it(7)
                qfirst   = .false.
            end if
            ielapsed = it(5) * 3600 + it(6) * 60 + it(7) - istart
            iel_min  =     ielapsed / 60
            iel_sec  = mod(ielapsed , 60)
            percent = real(100 * iframe) / real(itot_frames)
            write(*, '(a, f6.2, a, i4, 2(a, i2), 3(a, i2.2), a, i4.2, a, i2.2, a)')  &
                  '+processed...', percent, '%  ', &
                  it(1), '/', it(2), '/', it(3), ' ', it(5), ':', it(6), ':', it(7), &
                  ' time elapsed ', iel_min, 'min ', iel_sec, 'sec'
        end subroutine update_status


        subroutine get_option(mpg, fn_in)
            type (mpg_t), intent(in out) :: mpg
            character (len = :), allocatable, intent(out) :: fn_in
            character (len = 80) :: buffer
            character (len =  6) :: fmt
            integer :: narg, iarg, length
            iarg = 0
            narg = command_argument_count()
            do
                iarg = iarg + 1
                if (iarg > narg) call print_option()
                call get_command_argument(iarg, buffer)
                if (buffer(1:1) /= '-') exit  
                select case(trim(buffer))
                case ('-b') 
                    iarg = iarg + 1
                    if ( iarg > narg ) call print_option()
                    call get_command_argument(iarg, buffer, length)
                    write(fmt, '(a, i1, a)') '(i', length, ')' 
                    read(buffer, fmt) mpg%ibit_rate
                    if (mpg%ibit_rate < 1 .or. mpg%ibit_rate > 14) call print_option()
                case ('-crc') 
                    mpg%icrc = 0       ! crc16 on 
                case ('-c') 
                    mpg%icopyright = 1 ! copyrigt on
                case ('-o') 
                    mpg%ioriginal = 1  ! original on
                case default
                    call print_option()
                end select
            end do
            fn_in = trim(buffer)
        end subroutine get_option


        subroutine print_option()
            write(*, *) 'usage : uzura2 -option file_name '
            write(*, *) '      : file_name.wav -> file_name.mp2'
            write(*, *) 'option: -b 1..14  bitrate '
            write(*, *) '        -crc      crc16 error protection on'
            write(*, *) '        -c        copyright flag on'
            write(*, *) '        -o        original  flag on'
        end subroutine print_option

    end program uzura2